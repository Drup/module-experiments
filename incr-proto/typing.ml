open Parsetree.Modules
open Types
module M = Modules
module Op = Modules_op

type error =
  | Unbound_module of Parsetree.name
  | Unbound_module_type of Parsetree.Modules.path
  | Unbound_type of Parsetree.Modules.path
  | Not_a_functor of M.mod_type
  | Not_a_signature of M.mod_type
  | Not_a_mod_path of Parsetree.Modules.mod_path
  | Not_a_path of Parsetree.Modules.path
  
exception Ascription_fail
exception Error of error
let error e = raise (Error e)

(** Typing *)

let rec type_module :
  Env.t -> mod_term -> M.mod_type
  = fun env md ->
    match transl_mod_path_opt env md with
    | Some p ->
      Core (Alias p)
    | None -> begin match md with
        | Path _ -> assert false
        | Ascription (m, mty) ->
          let mty = transl_modtype env mty in
          let mty' = type_module env m in
          let mty_r = Op.subtype_modtype env mty' mty in
          Core mty_r
        | Structure str ->
          let signature = type_structure env str in
          Core (Signature signature)
        | Functor(param, param_mty, body) ->
          let param_mty = transl_modtype env param_mty in
          let param, env = Env.intro_module param param_mty env in
          let body_mty = type_module env body in
          Core (Functor_type(param, param_mty, body_mty))
        | Apply(funct, arg) ->
          let fun_mty = type_module env funct in
          let arg_mty = type_module env arg in
          type_functor_app env fun_mty arg_mty
        | Constraint(m, mty) ->
          let mty = transl_modtype env mty in
          let mty' = type_module env m in
          Op.check_subtype_modtype env mty' mty ;
          mty
      end
        
and type_functor_app env fun_mty arg_mty = 
  match Op.shape env fun_mty with
  | `Functor_type(param, param_mty, res_mty) ->
    let param_fresh = Ident.create @@ Ident.name param in
    let arg_ascribed_mty = Op.subtype_modtype env arg_mty param_mty in
    let res_mty =
      let sub =
        Subst.add_module param (Path (PathId param_fresh)) Subst.identity
      in
      Subst.mod_type sub res_mty
    in
    Let (param_fresh, Core arg_ascribed_mty, res_mty)
  | _ -> error @@ Not_a_functor fun_mty

and type_structure env { str_self; str_content } =
  Env.fold_with str_self type_definition env str_content

and type_definition env def =
  let add id f : M.signature_item option = Option.map f id in
  match def with
  | Value_str(id, term) ->
    let ty = Core_check.Typing.term env term in
    add id @@ fun id -> Value_sig(id, ty)
  | Module_str(id, modl) ->
    let mty = type_module env modl in
    add id @@ fun id -> Module_sig(id, mty)
  | Module_type_str(id, mty) ->
    let mty = transl_modtype env mty in
    add id @@ fun id -> Module_type_sig(id, mty)
  | Type_str(id, def) ->
    add id @@ fun id -> Type_sig(id, type_type_declaration env def)

and type_type_declaration env def = match def with
  | Abstract -> Abstract
  | TypeAlias p -> TypeAlias (Core_check.Transl.val_type env p)
  | Definition { manifest; definition } -> 
    let manifest =
      let f p = transl_type_path env p in
      Option.map f manifest
    in
    let definition = Core_check.Transl.def_type env definition in
    Definition { manifest; definition }

and transl_mod_path_internal : Env.t -> mod_term -> M.mod_path =
  fun env m ->
  let rec as_path env (m : mod_term) : M.mod_path = match m with
    | Path p ->
      let p =
        match p with
        | PathId name -> 
          let id =
            match Env.find_module env name with
            | Some (id, _) -> id
            | None -> error @@ Unbound_module name
          in
          M.PathId id
        | PathProj { path; field } ->
          let path = as_path env path in
          M.PathProj { path ; field }
      in
      M.Path p
    | Ascription (m, mty) ->
      let m = as_path env m in
      let mty = transl_modtype env mty in
      Op.subtype_path env m (Op.force env mty)
    | Apply (f, m) -> 
      let m = as_path env m in
      let f = as_path env f in
      M.Apply (f, m)
    | Structure _ 
    | Functor (_, _, _) 
    | Constraint (_, _) -> raise Exit
  in
  let p = as_path env m in
  (* We try to resolve the module, to check it's a valid path *)
  match Env.lookup_module env p with
  | None -> raise Exit
  | Some _ -> p

and transl_mod_path_opt env m =
  try Some (transl_mod_path_internal env m) with
  | Exit -> None

and transl_mod_path env m =
  try transl_mod_path_internal env m with
  | Exit -> error @@ Not_a_mod_path m

and transl_type_path env p =
  match Env.find_type env p with
  | Some (p,_) -> p
  | None -> error @@ Unbound_type p

and transl_module_type_path env p =
  match Env.find_module_type env p with
  | Some (p,_) -> p
  | None -> error @@ Unbound_module_type p

and transl_modtype env = function
  | TPath p ->
    let p' = transl_module_type_path env p in
    M.Core (TPath p')
  | Alias p ->
    let p = transl_mod_path env p in
    M.Core (Alias p)
  | Signature sg ->
    M.Core (transl_signature env sg)
  | Functor_type(param, param_ty, res) ->
    let param_ty = transl_modtype env param_ty in
    let param, env = Env.intro_module param param_ty env in
    let res = transl_modtype env res in
    M.Core (Functor_type(param, param_ty, res))
  | Let (name, mty1, mty2) ->
    let mty1 = transl_modtype env mty1 in
    let id, env = Env.intro_module (Some name) mty1 env in
    let mty2 = transl_modtype env mty2 in
    M.Let (id, mty1, mty2)
  | Enrich (mty, enrich) ->
    let enrich = match enrich with
      | Module (ns, mty) ->
        let mty = transl_modtype env mty in
        M.Module (ns, mty)
      | Type (ns, ty) ->
        let ty = Core_check.Transl.val_type env ty in
        Type (ns, ty)
    in
    let mty = transl_modtype env mty in
    Op.enrich_modtype ~env enrich mty
  | Ascription_sig (mty1, mty2) ->
    let mty1 = transl_modtype env mty1 in
    let mty2 = transl_modtype env mty2 in
    Core (Op.subtype_modtype env mty1 mty2)

and transl_signature env { sig_self; sig_content } =
  let sig_final =
    Env.fold_with sig_self transl_signature_item env sig_content
  in
  Signature sig_final

and transl_signature_item env def = 
  let add id f : M.signature_item option = Option.map f id in
  match def with
  | Value_sig(id, ty) ->
    let ty = Core_check.Transl.val_type env ty in 
    add id @@ fun id -> Value_sig(id, ty)
  | Module_sig(id, mty) ->
    let mty = transl_modtype env mty in
    add id @@ fun id -> Module_sig(id, mty)
  | Module_type_sig(id, mty) ->
    let mty = transl_modtype env mty in
    add id @@ fun id -> Module_sig(id, mty)
  | Type_sig(id, def) ->
    add id @@ fun id -> Type_sig(id, type_type_declaration env def)

and transl_type_declaration env def : M.type_decl = match def with
  | Abstract -> Abstract
  | TypeAlias p -> TypeAlias (Core_check.Transl.val_type env p)
  | Definition { manifest; definition } -> 
    let manifest =
      let f p = transl_type_path env p in
      Option.map f manifest
    in
    let definition = Core_check.Transl.def_type env definition in
    Definition { manifest; definition }

(** Env mutual recursion *)

and compute_ascription env mty1 mty2 =
  M.Core (Op.subtype_modtype env mty1 mty2)

and compute_functor_app env ~f:fun_mty ~arg =
  let arg_mty = M.Core (Alias arg) in
  type_functor_app env fun_mty arg_mty

and compute_signature env mty =
  match Op.shape env mty with
  | `Signature s -> s
  | _ -> error @@ Not_a_signature mty

let () =
  Env.compute_ascription := compute_ascription;
  Env.compute_functor_app := compute_functor_app;
  Env.compute_signature := compute_signature;
  Env.transl_mod_path := transl_mod_path;
  ()


let type_item env item =
  let item = type_definition env item in
  match item with
  | Some i -> 
    let _, env = Env.intro_item i env in
    item, env
  | None ->
    item, env

(** Errors *)
  
let prepare_error = function
  | Unbound_module name ->
    Report.errorf "Unbound module %s" name
  | Unbound_module_type p ->
    Report.errorf "Unbound module type %a" Printer.Untyped.path p
  | Unbound_type p ->
    Report.errorf "Unbound type %a" Printer.Untyped.path p
  | Not_a_functor mty ->
    Report.errorf "This module cannot be applied. It as type:@,%a"
      Printer.module_type mty
  | Not_a_signature mty ->
    Report.errorf "This module is not a signature. It as type:@,%a"
      Printer.module_type mty
  | Not_a_mod_path m ->
    Report.errorf "This module is not a module path:@,%a"
      Printer.Untyped.module_term m
  | Not_a_path m ->
    Report.errorf "This module is not a path:@,%a"
      Printer.Untyped.path m

let () = Report.register_report_of_exn @@ function
  | Error e -> Some (prepare_error e)
  | _ -> None
