open Core_types

module Typing = struct
  let term
    : Env.t -> term -> val_type
    = fun _ Unit -> Unit
  let val_type
    : Env.t -> val_type -> unit
    = fun _ Unit -> ()
  let def_type
    : Env.t -> def_type -> unit
    = fun env -> function
      | Alias ty -> val_type env ty
end

module Include = struct
  let val_type
    : Env.t -> val_type -> val_type -> unit
    = fun _ Unit Unit -> ()

  let def_type
    : Env.t -> def_type -> def_type -> unit
    = fun env tydecl1 tydecl2 -> match tydecl1, tydecl2 with
      | Alias ty1, Alias ty2 -> val_type env ty1 ty2
end
