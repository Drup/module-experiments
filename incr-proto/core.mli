open Core_types

module Typing : sig
  val term: Env.t -> term -> val_type
  val def_type: Env.t -> def_type -> unit
  val val_type: Env.t -> val_type -> unit
end

module Include : sig
  val val_type: Env.t -> val_type -> val_type -> unit
  val def_type: Env.t -> def_type -> def_type -> unit
end
