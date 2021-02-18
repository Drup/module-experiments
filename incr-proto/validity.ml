open Types

type error =
  | Unbound_modtype of Modules.path
  | Already_seen of field * string

exception Error of error
let error e = raise (Error e)

(* let rec check_modtype_core env = function
 *   | Modules.TPath path ->
 *     begin
 *       try ignore @@ Env.lookup_module_type env path with
 *       | Not_found -> error @@ Unbound_modtype path
 *     end
 *   | Alias _ -> ()
 *   | Signature sg -> check_signature env sg
 *   | Functor_type(param, param_ty, res) ->
 *     check_modtype env param_ty;
 *     let env = Env.add_module param param_ty env in
 *     check_modtype env res
 * 
 * and check_modtype = assert false
 * 
 * and check_signature env s =
 *   let mty = Modules.Core (Signature s) in
 *   let env = Env.add_module s.sig_self mty env in
 *   FieldMap.iter (fun _ vty -> Core_check.Validity.val_type env vty) s.sig_values ;
 *   FieldMap.iter (fun _ decl -> match decl.Modules.definition with
 *           None -> ()
 *         | Some typ ->
 *           Core_check.Validity.def_type env typ)
 *     s.sig_types ;
 *   FieldMap.iter (fun _ mty -> check_modtype env mty) s.sig_modules ;
 *   FieldMap.iter (fun _ mty -> check_modtype env mty) s.sig_module_types ;
 *   () *)
