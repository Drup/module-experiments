open Core_types

type t = {
  (* types : Modules.path Ident.Map.t ; *)
  modules : Modules.mod_path Ident.Map.t ;
  (* module_types : Modules.path Ident.Map.t ; *)
}

let val_type
  : t -> val_type -> val_type
  = fun _ Unit -> Unit
let def_type
  : t -> def_type -> def_type
  = fun env -> function
    | Alias ty -> Alias (val_type env ty)
