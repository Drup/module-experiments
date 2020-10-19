type t = {
  values : Types.val_type Ident.tbl ;
  types : Modules.type_decl Ident.tbl ;
  modules : Modules.mod_type Ident.tbl ;
  module_types : Modules.mod_type Ident.tbl ;
}

type _ key =
  | Value : Types.val_type key
  | Type : Modules.type_decl key
  | Module : Modules.mod_type key
  | Module_type : Modules.mod_type key

let empty = {
  values = Ident.Map.empty ;
  types = Ident.Map.empty ;
  modules = Ident.Map.empty ;
  module_types = Ident.Map.empty ;
}

let select (type a) (k : a key) env : a Ident.tbl = match k with
  | Value -> env.values
  | Type -> env.types
  | Module -> env.modules
  | Module_type -> env.module_types

let map (type a) (k : a key) (f : a Ident.tbl -> a Ident.tbl) env =
  match k with
  | Value -> { env with values = f env.values }
  | Type -> { env with types = f env.types }
  | Module -> { env with modules = f env.modules }
  | Module_type -> { env with module_types = env.module_types }

exception Not_found
exception Duplicated_entry

let add key id v env =
  let f tbl =
    if Ident.Map.mem id tbl then raise Duplicated_entry
    else Ident.Map.add id v tbl
  in
  map key f env

let add_value = add Value
let add_type = add Type
let add_module = add Module
let add_module_type = add Module_type

let fold_with id f env0 l0 =
  let update_sig sig_content env =
    let signature = {Modules. sig_self = id ; sig_content } in
    map Module (Ident.Map.add id (Modules.Core (Signature signature))) env
  in
  let add_item item sigitems = sigitems @ [item] in
  let rec aux_fold env sigitems = function
    | [] ->
      {Modules. sig_self = id ; sig_content = sigitems }
    | h :: t ->
      let item = f env h in
      let sigitems = add_item item sigitems in
      let env = update_sig sigitems env in
      aux_fold env sigitems t
  in
  let env0 = update_sig [] env0 in
  aux_fold env0 [] l0

let find_in_env
  : type a . key:a key -> _ -> _ -> a
  = fun ~key id env ->
    match Ident.Map.find_opt id (select key env) with
    | Some x -> x
    | None -> raise Not_found

let rec find_in_sig
  : type a . key:a key -> _ -> _ -> a
  = fun ~key field env ->
    match key, env with
    | _, [] -> raise Not_found
    | Value,
      Modules.Value_sig (field', ty) :: _ when field = field' ->
      ty
    | Type,
      Modules.Type_sig (field', ty) :: _ when field = field' ->
      ty
    | Module,
      Modules.Module_sig (field', mty) :: _ when field = field' ->
      mty
    | Module_type,
      Modules.Module_type_sig (field', mty) :: _ when field = field' ->
      mty
    | _,
      ( Value_sig _ | Type_sig _ | Module_sig _ | Module_type_sig _ ) ::
      env ->
      find_in_sig ~key field env

let compute_signature
  : (t -> Modules.mod_type -> Modules.signature) ref
  = ref (assert false)

let compute_ascription
  : (t -> Modules.mod_type -> Modules.mod_type -> Modules.mod_type) ref
  = ref (assert false)

let compute_functor_app
  : (t -> f:Modules.mod_type -> arg:Modules.mod_path -> Modules.mod_type) ref
  = ref (assert false)

let subst_self_in_sig
  : type a . self:Ident.t -> path:Modules.mod_path -> sort:a key -> a -> a
  = fun ~self ~path ~sort x ->
    let subst = Subst.add_module self path Subst.identity in
    let f : _ -> a -> a = match sort with
      | Value -> Core.Subst.val_type
      | Type -> Subst.type_decl
      | Module -> Subst.mod_type
      | Module_type -> Subst.mod_type
    in
    f subst x

let rec lookup_module : t -> Modules.mod_path -> _
  = fun env path0 ->
    match path0 with
    | Id id ->
      let mty = find_in_env ~key:Module id env in
      mty
    | Proj {path; field} ->
      let path_mty = lookup_module env path in
      let {Modules. sig_self; sig_content} = !compute_signature env path_mty in
      let mty = find_in_sig ~key:Module field sig_content in
      subst_self_in_sig ~self:sig_self ~path ~sort:Module mty
    | Ascription (path, ascr_mty) -> 
      let path_mty = lookup_module env path in
      let mty = !compute_ascription env path_mty ascr_mty in
      mty
    | Apply (path_f, path_arg) ->
      let mty_f = lookup_module env path_f in
      let mty = !compute_functor_app env ~f:mty_f ~arg:path_arg in
      mty

let lookup : type a . a key -> t -> Modules.path -> a
  = fun key env {Modules. path ; field } ->
    let path_mty = lookup_module env path in
    let {Modules. sig_self; sig_content} = !compute_signature env path_mty in
    let elt = find_in_sig ~key field sig_content in
    subst_self_in_sig ~self:sig_self ~path ~sort:key elt

let lookup_value = lookup Value
let lookup_type = lookup Type
let lookup_module_type = lookup Module_type
