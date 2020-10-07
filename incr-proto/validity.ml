open Modules

type error =
  | Unbound_modtype of path
  | Already_seen of field * string

exception Error of error
let error e = raise (Error e)

let rec check_modtype_core env = function
  | TPath path ->
    begin
      try ignore @@ Env.lookup_module_type env path with
      | Env.Not_found -> error @@ Unbound_modtype path
    end
  | Alias _ -> ()
  | Signature sg -> check_signature env sg
  | Functor_type(param, param_ty, res) ->
    check_modtype env param_ty;
    let env = Env.add_module param param_ty env in
    check_modtype env res

and check_modtype = assert false

and check_signature env {Modules. sig_self ; sig_content } =
  let id = Ident.create (Ident.name sig_self) in
  let _sig =
    Env.fold_with id check_signature_items env sig_content
  in
  ()

and check_signature_items env item =
  let () = match item with
    | Value_sig(_, vty) ->
      Core.Typing.val_type env vty
    | Type_sig(_, decl) ->
      begin match decl.definition with
          None -> ()
        | Some typ ->
          Core.Typing.def_type env typ
      end
    | Module_sig(_, mty) ->
      check_modtype env mty
    | Module_type_sig(_, mty) ->
      check_modtype env mty
  in
  item
