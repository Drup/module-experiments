module P = Parsetree.Core
module T = Types.Core

module Typing : sig
  val term: Env.t -> P.term -> T.val_type
end

module Transl : sig
  val def_type: Env.t -> P.def_type -> T.def_type
  val val_type: Env.t -> P.val_type -> T.val_type
end

module Validity : sig
  val def_type: Env.t -> T.def_type -> unit
  val val_type: Env.t -> T.val_type -> unit
end

module Include : sig
  val val_type: Env.t -> T.val_type -> T.val_type -> unit
  val def_type: Env.t -> T.def_type -> T.def_type -> unit
end
