open Modules

type error =
  | Not_a_subtype of mod_type * mod_type
  | Cannot_eliminate_let of Modules.mod_type
  | Invalid_enrichment 

exception Ascription_fail
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
    let mty = Env.lookup_module_type env mtp in
    strengthen_modtype env path mty
  | Alias (_p) -> (* TOCHECK *)
    Core (Alias (path))
  | Signature sigs ->
    Core (Signature (strengthen_signature env path sigs))
  | Functor_type (param, param_mty, body_mty) ->
    let path_for_body = Apply (path, Id param) in
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
    manifest = Some {path; field} ;
    definition = None ;
  }
  in
  new_decl

and strengthen_module_decl _env path field _mty =
  Core (Alias (Proj {path; field}))

(** Enrichment *)
(* TODO optimize traversal in case of multiple enrichment *)

and enrich_modtype ~env eq mty = match mty with
  | Strengthen _
  | Let _ 
    -> enrich_modtype_core ~env eq @@ force env mty
  | Enrich (mty, eq') ->
    let mtyc = enrich_modtype ~env eq' mty in
    enrich_modtype ~env eq (Core mtyc)
  | Core mtyc -> enrich_modtype_core ~env eq mtyc

and enrich_modtype_core ~env eq mtyc = match mtyc with
  | TPath p ->
    enrich_modtype ~env eq @@ Env.lookup_module_type env p
  | Alias p ->
    (* XXX memoize *)
    let _ = enrich_modtype ~env eq @@ resolve env p in
    Alias p
  | Signature s ->
    Signature (enrich_signature ~env eq s)
  | Functor_type (_, _, _) -> error Invalid_enrichment

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
    let _ = subtype_modtype env refined_mty mty in
    refined_mty
  | Module (field'::rest, refined_mty) when String.equal field field' ->
    let eq = Module (rest, refined_mty) in
    Enrich (mty, eq)
  | Type (field'::rest, refined_ty) when String.equal field field' ->
    let eq = Type (rest, refined_ty) in
    Enrich (mty, eq)
  | _ -> mty

(** Subtyping *)

and subtype_modtype env mty1 mty2 =
  try subtype_modtype_op env mty1 mty2 with
  | Ascription_fail -> error (Not_a_subtype (mty1, mty2))

and subtype_modtype_op env mty1 mty2 =
  match mty1, mty2 with
  | _, _ ->
    let mtyc1 = force env mty1 in
    let mtyc2 = force env mty2 in
    subtype_modtype_core env mtyc1 mtyc2

and subtype_modtype_core env mty1 mty2 =
  match mty1, mty2 with
  | TPath p1, _ ->
    let mty1 = Env.lookup_module_type env p1 in
    subtype_modtype env mty1 (Core mty2)
  | _, TPath p2 ->
    let mty2 = Env.lookup_module_type env p2 in
    subtype_modtype env (Core mty1) mty2
  | Functor_type (param1, param_mty1, body1),
    Functor_type (param2, param_mty2, body2) ->
    let _ = subtype_modtype env param_mty2 param_mty1 in
    let body1 =
      let subst = Subst.add_module param1 (Id param2) Subst.identity in
      Subst.mod_type subst body1
    in
    let env = Env.add_module param2 param_mty2 env in
    let body = subtype_modtype env body1 body2 in
    Functor_type (param1, param_mty2, Core body)
  | Alias (Ascription (path1, mty1)),
    Alias (Ascription (path2, mty2)) ->
    check_equiv_mod_path env path1 path2;
    let mty = subtype_modtype env (Env.lookup_module env path1) mty1 in
    let _ = subtype_modtype env (Core mty) mty2 in
    Alias (Ascription (path2, mty2))
  | Alias (Ascription (path1, mty1)),
    Alias path2 ->
    check_equiv_mod_path env path1 path2;
    let mty = subtype_modtype env (Env.lookup_module env path1) mty1 in
    let _ = subtype_modtype env (Core mty) (Env.lookup_module env path2) in
    Alias path2
  | Alias path1, Alias path2 ->
    check_equiv_mod_path env path1 path2;
    Alias path2
  | Alias path1, _ ->
    Alias (Ascription (path1, Core mty2))
    (* let mty1 = Env.lookup_module env path1 in
     * subtype_modtype env mty1 (Core mty2) *)
  | Signature sig1, Signature sig2 ->
    Signature (subtype_signature env sig1 sig2)
  | _ -> raise Ascription_fail
  

and subtype_signature env sig1 sig2 =
  let id = Ident.create (Ident.name sig1.sig_self) in
  let path = Id id in
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
    Core.Include.val_type env ty1 ty2;
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
    | Some d1, Some d2  -> Core.Include.def_type env d1 d2
  end;
  ()

and subtype_module_decl env _path field = function
  | `Right _ -> raise Ascription_fail
  | `Left mty1 ->
    let empty_sig = Core (Signature (empty_sig (Ident.create field))) in
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
    enrich_modtype ~env eq mty
  | Core mtyc -> mtyc

and resolve : Env.t -> mod_path -> mod_type =
  fun env p0 ->
  let rec loop p = 
    let mty = Env.lookup_module env p in
    match force env mty with
    | Alias p -> loop p
    | mtyc -> strengthen_modtype_core env p0 mtyc
  in
  loop p0

and normalize  : Env.t -> mod_path -> mod_path =
  fun env p0 ->
  let rec loop p = 
    let mty = Env.lookup_module env p in
    match force env mty with
    | Alias p -> loop p
    | _ -> p
  in
  loop p0

and normalize_type
  : Env.t -> Modules.path -> Modules.path
  = fun env p0 ->
    let rec loop p = 
      let tydecl = Env.lookup_type env p in
      match tydecl.manifest with
      | Some p -> loop p
      | None -> p
    in
    loop p0

and shape : Env.t -> mod_type -> _ =
  fun env mty ->
  let rec loop mty =
    let mtyc = force env mty in
    match mtyc with
    | TPath p -> loop @@ Env.lookup_module_type env p
    | Alias p -> loop @@ resolve env p
    | Signature s -> `Signature s
    | Functor_type (p, m, b) -> `Functor_type (p, m, b)
  in
  loop mty