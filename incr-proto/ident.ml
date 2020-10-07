
module M = struct
  type t = {name: string; stamp: int}
  let compare id1 id2 = compare id1.stamp id2.stamp
end

include M
let currstamp = ref 0

let create s =
  currstamp := !currstamp + 1;
  {name = s; stamp = !currstamp}

let name id = id.name
let equal id1 id2 = (id1.stamp = id2.stamp)

module Map = Map.Make(M)
type 'a tbl = 'a Map.t
