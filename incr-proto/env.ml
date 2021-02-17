open Types

type t = {
  values : Core.val_type Ident.tbl ;
  types : Modules.type_decl Ident.tbl ;
  modules : Modules.mod_type Ident.tbl ;
  module_types : Modules.mod_type Ident.tbl ;
}

type _ key =
  | Value : Core.val_type key
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
  | Module_type -> { env with module_types = f env.module_types }

type error =
  | Already_defined of Modules.signature_item

exception Error of error

let add key id v env =
  let f tbl = Ident.Map.add id v tbl in
  map key f env

let add_value = add Value
let add_type = add Type
let add_module = add Module
let add_module_type = add Module_type


let empty_sig id = {Modules.
  sig_self = id ;
  sig_values = FieldMap.empty ;
  sig_types = FieldMap.empty ;
  sig_modules = FieldMap.empty ;
  sig_module_types = FieldMap.empty ;
}

let fold_with name f env0 l0 =
  let id = Ident.create name in
  let update_sig env signature =
    map Module (Ident.Map.add id (Modules.Core (Signature signature))) env
  in
  let add_item item (s : Modules.signature) = match item with
    | Modules.Value_sig (field, decl) ->
      if FieldMap.mem field s.sig_values then
        raise (Error (Already_defined item));
      let sig_values = FieldMap.add field decl s.sig_values in
      {s with sig_values }
    | Type_sig (field, decl) ->
      if FieldMap.mem field s.sig_types then
        raise (Error (Already_defined item));
      let sig_types = FieldMap.add field decl s.sig_types in
      {s with sig_types }
    | Module_sig (field, decl) ->
      if FieldMap.mem field s.sig_modules then
        raise (Error (Already_defined item));
      let sig_modules = FieldMap.add field decl s.sig_modules in
      {s with sig_modules }
    | Module_type_sig (field, decl) ->
      if FieldMap.mem field s.sig_module_types then
        raise (Error (Already_defined item));
      let sig_module_types = FieldMap.add field decl s.sig_module_types in
      {s with sig_module_types }
  in
  let rec aux_fold env signature = function
    | [] -> signature
    | h :: t ->
      let env = update_sig env signature in
      let item = f env h in
      match item with
      | Some item -> 
        let signature = add_item item signature in
        aux_fold env signature t
      | None ->
        aux_fold env signature t
  in
  let sig0 = empty_sig id in
  aux_fold env0 sig0 l0

let intro key name v env =
  let id = Ident.create name in
  let f tbl =
    Ident.Map.add id v tbl
  in
  id, map key f env

let intro_value = intro Value
let intro_type = intro Type
let intro_module = intro Module
let intro_module_type = intro Module_type

let intro_item item env = match item with
  | Modules.Value_sig (field, decl) ->
    intro Value (Some field) decl env
  | Type_sig (field, decl) ->
    intro Type (Some field) decl env
  | Module_sig (field, decl) ->
    intro Module (Some field) decl env
  | Module_type_sig (field, decl) ->
    intro Module_type (Some field) decl env

let lookup_in_env
  : type a . key:a key -> _ -> _ -> a
  = fun ~key id env ->
    Ident.Map.lookup id (select key env)

let compute_signature
  : (t -> Modules.mod_type -> Modules.signature) ref
  = ref (fun _ _ -> assert false)

let compute_ascription
  : (t -> Modules.mod_type -> Modules.mod_type -> Modules.mod_type) ref
  = ref (fun _ _ _ -> assert false)

let compute_functor_app
  : (t -> f:Modules.mod_type -> arg:Modules.mod_path -> Modules.mod_type) ref
  = ref (fun _ ~f:_ ~arg:_ -> assert false)

let transl_mod_path
  : (t -> Parsetree.Modules.mod_path -> Types.Modules.mod_path) ref
  = ref (fun _ _ -> assert false)

(** Lookup of resolved names *)

let lookup_raw_in_sig
  : type a . key:a key -> field -> Modules.signature -> a
  = fun ~key field s ->
    match key with
    | Value -> FieldMap.find field s.sig_values
    | Type -> FieldMap.find field s.sig_types
    | Module -> FieldMap.find field s.sig_modules
    | Module_type -> FieldMap.find field s.sig_module_types

let subst_self_in_sig
  : type a . self:Ident.t -> path:Modules.mod_path -> sort:a key -> a -> a
  = fun ~self ~path ~sort x ->
    let subst = Subst.add_module self path Subst.identity in
    let f : _ -> a -> a = match sort with
      | Value -> Subst.val_type
      | Type -> Subst.type_decl
      | Module -> Subst.mod_type
      | Module_type -> Subst.mod_type
    in
    f subst x

let lookup_in_sig ~key path field s =
  let elt = lookup_raw_in_sig ~key field s in
  subst_self_in_sig ~self:s.sig_self ~path ~sort:key elt

let rec lookup_module_internal : t -> Modules.mod_path -> _
  = fun env path0 ->
    match path0 with
    | Path p -> lookup Module env p
    | Ascription (path, ascr_mty) -> 
      let path_mty = lookup_module_internal env path in
      let mty = !compute_ascription env path_mty ascr_mty in
      mty
    | Apply (path_f, path_arg) ->
      let mty_f = lookup_module_internal env path_f in
      let mty = !compute_functor_app env ~f:mty_f ~arg:path_arg in
      mty

and lookup : type a . a key -> t -> Modules.path -> a
  = fun key env p ->
    match p with
    | PathId id -> lookup_in_env ~key id env
    | PathProj { path ; field } ->
      let path_mty = lookup_module_internal env path in
      let s = !compute_signature env path_mty in
      lookup_in_sig ~key path field s

let try_lookup f env x = try Some (f env x) with Not_found -> None

let lookup_module = try_lookup lookup_module_internal
let lookup_value = try_lookup @@ lookup Value
let lookup_type = try_lookup @@ lookup Type
let lookup_module_type = try_lookup @@ lookup Module_type

(** Finding unresolved names *)

let find_ident key env id =
  Ident.Map.find id (select key env)

let find : type a . a key -> t -> Parsetree.Modules.path -> Modules.path * a
  = fun key env path ->
    match path with 
    | PathId id ->
      let id, def = find_ident key env id in 
      PathId id, def
    | PathProj { path ; field } ->
      let path = !transl_mod_path env path in
      let path_mty = lookup_module_internal env path in
      let s = !compute_signature env path_mty in
      let def = lookup_in_sig ~key path field s in
      PathProj {path; field}, def

let try_find f env x = try Some (f env x) with _ -> None

let find_module = try_find @@ find_ident Module
let find_value = try_find @@ find Value
let find_type = try_find @@ find Type
let find_module_type = try_find @@ find Module_type

(** Errors *)
  
let prepare_error = function
  | Already_defined item ->
    let sort, name = match item with
      | Modules.Value_sig (s,_) -> "value", s
      | Modules.Type_sig (s,_) -> "type", s
      | Modules.Module_sig (s,_) -> "module", s
      | Modules.Module_type_sig (s,_) -> "module type", s
    in
    Report.errorf "The %s %s is already defined" sort name
  
let () = Report.register_report_of_exn @@ function
  | Error e -> Some (prepare_error e)
  | _ -> None

