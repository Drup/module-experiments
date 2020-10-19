open Core_types

module Typing = struct
  let term
    : Env.t -> term -> val_type
    = fun _ Unit -> Unit
  let def_type
    : Env.t -> def_type -> unit
    = fun _ Unit -> ()
  let val_type
    : Env.t -> val_type -> unit
    = fun _ Unit -> ()
end

module Include = struct
  let val_type
    : Env.t -> val_type -> val_type -> unit
    = fun _ Unit Unit -> ()

  let def_type
    : Env.t -> def_type -> def_type -> unit
    = fun _ Unit Unit -> ()
end
