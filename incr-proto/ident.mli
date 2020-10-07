type t
val create: string -> t
val name: t -> string
val equal: t -> t -> bool
val compare: t -> t -> int

module Map : Map.S with type key = t
type 'a tbl = 'a Map.t
