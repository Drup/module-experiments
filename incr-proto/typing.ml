open Modules

type error =
  | Unbounded_module of Ident.t
  | Not_a_subtype of mod_type * mod_type
  | Not_a_functor
  | Already_seen of Ident.t * string

exception Ascription_fail
exception Error of error
let error e = raise (Error e)

(** Strenghtening *)

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

and strengthen_signature env path { sig_self; sig_content } =
  { sig_self ;
    sig_content = List.map (strengthen_signature_item env path) sig_content ;
  }
  
and strengthen_signature_item _env path sigi = match sigi with
  | Value_sig _
  | Module_type_sig _
    -> sigi
  | Type_sig (name, _decl) ->
    let new_decl = {
      manifest = Some {path; field = name} ;
      definition = None ;
    }
    in
    Type_sig (name, new_decl)
  | Module_sig (name, _mty) ->
    Module_sig (name, Core (Alias (Proj {path; field = name})))

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
    check_equiv_path env path1 path2;
    let mty = subtype_modtype env (Env.lookup_module env path1) mty1 in
    let _ = subtype_modtype env (Core mty) mty2 in
    Alias (Ascription (path2, mty2))
  | Alias (Ascription (path1, mty1)),
    Alias path2 ->
    check_equiv_path env path1 path2;
    let mty = subtype_modtype env (Env.lookup_module env path1) mty1 in
    let _ = subtype_modtype env (Core mty) (Env.lookup_module env path2) in
    Alias path2
  | Alias path1, Alias path2 ->
    check_equiv_path env path1 path2;
    Alias path2
  | Alias path1, _ ->
    let mty1 = Env.lookup_module env path1 in
    subtype_modtype env mty1 (Core mty2)
  | Signature sig1, Signature sig2 ->
    Signature (subtype_signature env sig1 sig2)
  | _ -> raise Ascription_fail

and subtype_signature env sig1 sig2 =
  let id = Ident.create (Ident.name sig1.sig_self) in
  let path = Id id in
  let sig2_content =
    let subst = Subst.add_module sig2.sig_self path Subst.identity in
    List.map (Subst.signature_item subst) sig2.sig_content
  in
  let env = Env.add_module id (Core (Signature sig1)) env in
  let newsig = List.map (function
      | Module_sig (field, mty2) ->
        let mty1 = Env.lookup_module env (Proj {path; field}) in
        Module_sig (field, Core (subtype_modtype env mty1 mty2))
      | Value_sig (field, ty2) ->
        let ty1 = Env.lookup_value env { path ; field } in
        Core.Include.val_type env ty1 ty2;
        Value_sig (field, ty2)
      | Type_sig (field, ty2) ->
        let ty1 = Env.lookup_type env { path ; field } in
        begin match ty1, ty2 with
          | _, _ -> () (* TODO *)
        end;
        Type_sig (field, ty2)
      | Module_type_sig (field, mty2) ->
        let mty1 = Env.lookup_module env (Proj {path; field}) in
        Module_type_sig (field, Core (subtype_modtype env mty1 mty2))
    ) sig2_content
  in
  { sig_self = id; sig_content = newsig }
  
and check_equiv_path env path1 path2 = 
  if normalize env path1 = normalize env path2 then
    ()
  else
    raise Ascription_fail

(** Forcing operations *)

and force : Env.t -> mod_type -> mod_type_core =
  fun env mty ->
  match mty with
  | Strengthen (mty, path) ->
    force env @@ strengthen_modtype env path mty
  | Let (id, m, mty) ->
    begin match m with
      | Alias path ->
        let subst = Subst.add_module id path Subst.identity in
        let mty = Subst.mod_type subst mty in
        force env mty
      | _ -> (??)
    end
  | Enrich (_, _) -> (??)
  | Core mtyc -> mtyc

and resolve : Env.t -> mod_path -> mod_type =
  fun env p0 ->
  let rec loop p = 
    let mty = Env.lookup_module env p in
    match force env mty with
    | Alias p -> loop p
    | mtyc -> strengthen_modtype_core env p mtyc
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

(** Typing *)

let rec type_module :
  type p . Env.t -> p mod_term -> mod_type
  = fun env md -> match md with
    | Id _ 
    | Proj _ as p ->
      let _ = Env.lookup_module env p in
      Core (Alias p)
    | Ascription (m, mty) ->
      let mty' = type_module env m in
      let mty_r = subtype_modtype env mty' mty in
      Core mty_r
    | Structure str ->
      let signature = type_structure env str in
      Core (Signature signature)
    | Functor(param, param_mty, body) ->
      Validity.check_modtype env param_mty;
      let body_mty =
        let env = Env.add_module param param_mty env in
        type_module env body
      in
      Core (Functor_type(param, param_mty, body_mty))
    | Apply(funct, arg) ->
      let fun_mty = type_module env funct in
      let fun_mtyc = force env fun_mty in
      begin match fun_mtyc with
       | Functor_type(param, param_mty, res_mty) ->
         let arg_mty = type_module env arg in
         let arg_ascribed_mty = subtype_modtype env arg_mty param_mty in
         Let (param, arg_ascribed_mty, res_mty)
       | _ -> error Not_a_functor
      end
    | Constraint(m, mty) ->
      Validity.check_modtype env mty;
      let mty' = type_module env m in
      let _ = subtype_modtype env mty' mty in
      mty

and type_structure env { str_self; str_content } =
  Env.fold_with str_self type_definition env str_content

and type_definition env = function
  | Value_str(id, term) ->
    let ty = Core.Typing.term env term in
    Value_sig(id, ty)
  | Module_str(id, modl) ->
    let mty =type_module env modl in
    Module_sig(id, mty)
  | Module_type_str(id, mty) ->
    Validity.check_modtype env mty;
    Module_sig(id, mty)
  | Type_str(id, typedef) ->
    begin match typedef.definition with
      | None -> ()
      | Some ty -> Core.Typing.def_type env ty
    end ;
    Type_sig(id, typedef)
