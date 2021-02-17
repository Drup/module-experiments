open Types
open Modules

type error =
  | Unbound_module of mod_path
  | Unbound_module_type of path
  | Unbound_type of path
  | Not_a_subtype of mod_type * mod_type
  | Path_not_included of mod_path * mod_type
  | Cannot_eliminate_let of Modules.mod_type
  | Invalid_enrichment of Modules.enrichment * Modules.mod_type

exception Ascription_fail
exception Enrichment_fail
exception Error of error
let error e = raise (Error e)

let empty_sig id = {
  sig_self = id ;
  sig_values = FieldMap.empty ;
  sig_types = FieldMap.empty ;
  sig_modules = FieldMap.empty ;
  sig_module_types = FieldMap.empty ;
}

(** Strengthening *)

let rec strengthen_modtype env path mty = match mty with
  | Strengthen _ -> mty
  | Let _ 
  | Enrich (_, _) ->
    let mtyc = force env mty in
    strengthen_modtype_core env path mtyc
  | Core mtyc -> strengthen_modtype_core env path mtyc

and strengthen_modtype_core env path mty = match mty with
  | TPath mtp ->
    let mty = resolve_modtype env mtp in
    strengthen_modtype env path mty
  | Alias (_p) -> (* TOCHECK *)
    Core (Alias (path))
  | Signature sigs ->
    Core (Signature (strengthen_signature env path sigs))
  | Functor_type (param, param_mty, body_mty) ->
    let path_for_body = Apply (path, Path (PathId param)) in
    let body_mty = Strengthen (body_mty, path_for_body) in
    Core (Functor_type (param, param_mty, body_mty))

and strengthen_signature env path
    { sig_self; sig_values; sig_types; sig_modules; sig_module_types } =
  { sig_self ;
    sig_values ;
    sig_module_types ;
    sig_types = FieldMap.mapi (strengthen_type_decl env path) sig_types ;
    sig_modules = FieldMap.mapi (strengthen_module_decl env path) sig_modules ;
  }

and strengthen_type_decl _env path field _decl = 
  let new_decl = {
    manifest = Some (PathProj {path; field}) ;
    definition = None ;
  }
  in
  new_decl

and strengthen_module_decl _env path field _mty =
  Core (Alias (Path (PathProj {path; field})))

(** Enrichment *)
(* TODO optimize traversal in case of multiple enrichment *)

and enrich_modtype ~env eq mty =
  try Core (enrich_modtype_raw ~env eq mty) with
  | Enrichment_fail -> error @@ Invalid_enrichment (eq, mty)

and enrich_modtype_raw ~env eq mty = match mty with
  | Strengthen _
  | Let _ 
    -> enrich_modtype_core ~env eq @@ force env mty
  | Enrich (mty, eq') ->
    let mtyc = enrich_modtype_raw ~env eq' mty in
    enrich_modtype_raw ~env eq (Core mtyc)
  | Core mtyc -> enrich_modtype_core ~env eq mtyc

and enrich_modtype_core ~env eq mtyc = match mtyc with
  | TPath p ->
    let mty = resolve_modtype env p in
    enrich_modtype_raw ~env eq mty
  | Alias p ->
    (* XXX memoize *)
    let _ = enrich_modtype_raw ~env eq @@ resolve env p in
    Alias p
  | Signature s ->
    Signature (enrich_signature ~env eq s)
  | Functor_type (_, _, _) -> raise Enrichment_fail

and enrich_signature ~env eq
    { sig_self; sig_values; sig_types; sig_modules; sig_module_types } =
  { sig_self ;
    sig_values ;
    sig_module_types ;
    sig_types = FieldMap.mapi (enrich_type_decl ~env eq) sig_types ;
    sig_modules = FieldMap.mapi (enrich_module_decl ~env eq) sig_modules ;
  }

and enrich_type_decl ~env eq field tydecl = match eq with
  | Type ([field'], ty) when String.equal field field' ->
    let tydecl' = { manifest = None ; definition = Some ty } in
    check_subtype_type_decl env tydecl tydecl';
    tydecl'
  | _ -> tydecl

and enrich_module_decl ~env eq field mty = match eq with
  | Module ([],_) | Type ([],_) -> assert false
  | Module ([field'], refined_mty) when String.equal field field' ->
    check_subtype_modtype env refined_mty mty ;
    refined_mty
  | Module (field'::rest, refined_mty) when String.equal field field' ->
    let eq = Module (rest, refined_mty) in
    enrich_modtype ~env eq mty
  | Type (field'::rest, refined_ty) when String.equal field field' ->
    let eq = Type (rest, refined_ty) in
    enrich_modtype ~env eq mty
  | _ -> mty

(** Subtyping *)

and subtype_modtype env mty1 mty2 =
  try subtype_modtype_op env mty1 mty2 with
  | Ascription_fail -> error (Not_a_subtype (mty1, mty2))

and check_subtype_modtype env mty1 mty2 =
  let (_ : Modules.mod_type_core) = subtype_modtype env mty1 mty2 in
  ()

and subtype_modtype_op env mty1 mty2 =
  match mty1, mty2 with
  | _, _ ->
    let mtyc1 = force env mty1 in
    let mtyc2 = force env mty2 in
    subtype_modtype_core env mtyc1 mtyc2

and subtype_modtype_core env mty1 mty2 =
  match mty1, mty2 with
  | Alias p, mty ->
    Alias (subtype_path env p mty)
  | TPath p1, _ ->
    let mty1 = resolve_modtype env p1 in
    subtype_modtype env mty1 (Core mty2)
  | _, TPath p2 ->
    let mty2 = resolve_modtype env p2 in
    subtype_modtype env (Core mty1) mty2
  | Functor_type (param1, param_mty1, body1),
    Functor_type (param2, param_mty2, body2) ->
    check_subtype_modtype env param_mty2 param_mty1 ;
    let body1 =
      let subst = Subst.add_module
          param1 ((Path (PathId param2))) Subst.identity
      in
      Subst.mod_type subst body1
    in
    let env = Env.add_module param2 param_mty2 env in
    let body = subtype_modtype env body1 body2 in
    Functor_type (param1, param_mty2, Core body)
  | Signature sig1, Signature sig2 ->
    Signature (subtype_signature env sig1 sig2)
  | _ -> raise Ascription_fail

and is_no_alias : mod_type_core -> bool = function
  | Alias _ -> false
  | TPath _ | Functor_type _ | Signature _ -> true

(* We build a *canonical* path with the ascription.
   
   Invariant: 
   The left hand side of the ascription is a shape: either a signature
   or a functor.
   It must not contain aliases (or paths that could lead to an alias).
   TODO: Split path subtyping into "canonical" and "resolving" to not 
   need that invariant
*)
and subtype_path env path0 mty0 =
  match path0, mty0 with
  | Ascription (path1, mty1),
    Alias (Ascription (path2, mty2)) ->
    assert (is_no_alias @@ force env mty2);
    check_equiv_mod_path env path1 path2;
    let mty = subtype_modtype env (resolve env path1) mty1 in
    check_subtype_modtype env (Core mty) mty2 ;
    Ascription (path2, mty2)
  | path1,
    Alias (Ascription (path2, mty2)) ->
    check_equiv_mod_path env path1 path2;
    subtype_path env path2 (force env mty2)
  | Ascription (path1, mty1),
    Alias path2 ->
    check_equiv_mod_path env path1 path2;
    let mty = subtype_modtype env (resolve env path1) mty1 in
    check_subtype_modtype env (Core mty) (resolve env path2) ;
    path2
  | path1, Alias path2 ->
    check_equiv_mod_path env path1 path2;
    path2
  | path, TPath mtyp ->
    let mty = force env @@ resolve_modtype env mtyp in
    subtype_path env path mty
  | path1, (Signature _ | Functor_type _ as mty2) ->
    let mty1 = resolve env path1 in
    check_subtype_modtype env mty1 (Core mty2) ;
    Ascription (path1, Core mty2)

and subtype_signature env sig1 sig2 =
  let id = Ident.create (Ident.name sig1.sig_self) in
  let path = Path (PathId id) in
  let sig2 =
    let subst = Subst.add_module sig2.sig_self path Subst.identity in
    Subst.signature subst sig2
  in
  let env = Env.add_module id (Core (Signature sig1)) env in
  { sig_self = id ;
    sig_values =
      FieldMap.merge_safe sig1.sig_values sig2.sig_values
        ~f:(subtype_value_decl env path) ;
    sig_types =
      FieldMap.merge_safe sig1.sig_types sig2.sig_types
        ~f:(subtype_type_decl env path) ;
    sig_modules =
      FieldMap.merge_safe sig1.sig_modules sig2.sig_modules
        ~f:(subtype_module_decl env path) ;
    sig_module_types =
      FieldMap.merge_safe sig1.sig_module_types sig2.sig_module_types
        ~f:(subtype_module_type_decl env path) ;
  }

and subtype_value_decl env _path _field = function
  | `Right _ -> raise Ascription_fail
  | `Left _ -> None
  | `Both (ty1, ty2) ->
    Core_check.Include.val_type env ty1 ty2;
    Some ty2

and subtype_type_decl env _path _field = function
  | `Right _ -> raise Ascription_fail
  | `Left tydecl -> Some tydecl
  | `Both (tydecl1, tydecl2) ->
    check_subtype_type_decl env tydecl1 tydecl2;
    Some tydecl1

and check_subtype_type_decl env tydecl1 tydecl2 = 
  begin match tydecl1.manifest, tydecl2.manifest with
    | None, None -> ()
    | Some _, None -> ()
    | None, Some _ -> raise Ascription_fail
    | Some p1, Some p2 -> check_equiv_type_path env p1 p2
  end;
  begin match tydecl1.definition, tydecl2.definition with
    | None, None -> ()
    | Some _, None -> ()
    | None, Some _ -> raise Ascription_fail
    | Some d1, Some d2  -> Core_check.Include.def_type env d1 d2
  end;
  ()

and subtype_module_decl env _path field = function
  | `Right _ -> raise Ascription_fail
  | `Left mty1 ->
    let empty_sig = Core (Signature (empty_sig (Ident.create @@ Some field))) in
    let mty = Core (subtype_modtype env mty1 empty_sig) in
    Some mty
  | `Both (mty1, mty2) ->
    let mty = Core (subtype_modtype env mty1 mty2) in
    Some mty

and subtype_module_type_decl env _path _field = function 
  | `Right _ -> raise Ascription_fail
  | `Left mty1 -> Some mty1
  | `Both (mty1, mty2) ->
    let mty = Core (subtype_modtype env mty1 mty2) in
    Some mty


and check_equiv_type_path env path1 path2 = 
  if normalize_type env path1 = normalize_type env path2 then
    ()
  else
    raise Ascription_fail

and check_equiv_mod_path env path1 path2 = 
  if normalize env path1 = normalize env path2 then
    ()
  else
    raise Ascription_fail

(** Forcing operations *)

and force : Env.t -> mod_type -> mod_type_core =
  fun env mty0 ->
  match mty0 with
  | Strengthen (mty, path) ->
    force env @@ strengthen_modtype env path mty
  | Let (id, m, mty) ->
    begin match force env m with
      | Alias path ->
        let subst = Subst.add_module id path Subst.identity in
        let mty = force env mty in
        Subst.mod_type_core subst mty
      | _ -> error @@ Cannot_eliminate_let mty0
    end
  | Enrich (mty, eq) ->
    enrich_modtype_raw ~env eq mty
  | Core mtyc -> mtyc

and resolve
  : Env.t -> mod_path -> mod_type
  = fun env p ->
    let mty = match Env.lookup_module env p with
      | Some m -> m
      | None -> error @@ Unbound_module p
    in
    match force env mty with
    | Alias p -> resolve env p
    | mtyc -> strengthen_modtype env p (Core mtyc)

and resolve_modtype
  : Env.t -> path -> mod_type
  = fun env p ->
    match Env.lookup_module_type env p with
    | Some m -> m
    | None -> error @@ Unbound_module_type p

and normalize
  : Env.t -> mod_path -> mod_path
  = fun env p ->
    let mty = match Env.lookup_module env p with
      | Some m -> m
      | None -> error @@ Unbound_module p
    in
    match force env mty with
    | Alias p -> normalize env p
    | _ -> p

and normalize_type
  : Env.t -> Modules.path -> Modules.path
  = fun env p ->
    let tydecl = match Env.lookup_type env p with
      | Some m -> m
      | None -> error @@ Unbound_type p
    in
    match tydecl.manifest with
    | Some p -> normalize_type env p
    | None -> p

and shape
  : Env.t -> mod_type -> _
  = fun env mty ->
    let mtyc = force env mty in
    match mtyc with
    | TPath p -> shape env @@ resolve_modtype env p
    | Alias p -> shape env @@ resolve env p
    | Signature s -> `Signature s
    | Functor_type (p, m, b) -> `Functor_type (p, m, b)

let subtype_path env path mty =
  let p =
    try subtype_path env path mty with
    | Ascription_fail -> error @@ Path_not_included (path, Core mty)
  in
  p


(** Errors *)
  
let prepare_error = function
  | Not_a_subtype (mty1, mty2) ->
    Report.errorf
      "@[<v>@[<v2>The module@ @[%a@]@]@,@[<v2>is not included in@ @[%a@]@]@]"
      Printer.module_type mty1
      Printer.module_type mty2
  | Path_not_included (path, mty) ->
    Report.errorf
      "@[<v>@[<hov>The path@ %a@]@,@[<v2>is not included in@ @[%a@]@]@]"
      Printer.module_path path
      Printer.module_type mty
  | Cannot_eliminate_let mty ->
    Report.errorf
      "@[<v 2>Cannot eliminate let in the module type@,@[%a@]@]"
      Printer.module_type mty
  | Invalid_enrichment (eq, mty) ->
    Report.errorf
      "@[<v 2>The enrichment @[%a@] cannot be applied to module@ @[%a@]@]"
      Printer.enrichment eq
      Printer.module_type mty
  | Unbound_module p ->
    Report.errorf "Unbound module %a" Printer.module_path p
  | Unbound_module_type p ->
    Report.errorf "Unbound module type %a" Printer.path p
  | Unbound_type p ->
    Report.errorf "Unbound type %a" Printer.path p

let () = Report.register_report_of_exn @@ function
  | Error e -> Some (prepare_error e)
  | _ -> None
