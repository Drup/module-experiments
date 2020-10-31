type t
val create: string -> t
val name: t -> string
val equal: t -> t -> bool
val compare: t -> t -> int

module Map : sig
  type ident
  type 'a t
  val empty : 'a t
  val add : ident -> 'a -> 'a t -> 'a t
  val remove_min_binding : 'a t -> 'a t
  val merge : 'a t -> 'a t -> 'a t
  val remove : ident -> 'a t -> 'a t
  val mem : ident -> 'a t -> bool
  val lookup : ident -> 'a t -> 'a
  val find : string -> 'a t -> ident * 'a
  val find_all : string -> 'a t -> (ident * 'a) list
  val fold : (ident -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val fold_all : (ident -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : (ident -> 'a -> unit) -> 'a t -> unit
end with type ident := t

type 'a tbl = 'a Map.t
