
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

module Map = struct

  type 'a t =
      Empty
    | Node of 'a t * 'a data * 'a t * int

  and 'a data =
    { ident: M.t;
      data: 'a;
      previous: 'a data option }

  let empty = Empty

  let mknode l d r =
    let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h
    and hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
    Node(l, d, r, (if hl >= hr then hl + 1 else hr + 1))

  let balance l d r =
    let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h
    and hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
    if hl > hr + 1 then
      match l with
      | Node (ll, ld, lr, _)
        when (match ll with Empty -> 0 | Node(_,_,_,h) -> h) >=
             (match lr with Empty -> 0 | Node(_,_,_,h) -> h) ->
        mknode ll ld (mknode lr d r)
      | Node (ll, ld, Node(lrl, lrd, lrr, _), _) ->
        mknode (mknode ll ld lrl) lrd (mknode lrr d r)
      | _ -> assert false
    else if hr > hl + 1 then
      match r with
      | Node (rl, rd, rr, _)
        when (match rr with Empty -> 0 | Node(_,_,_,h) -> h) >=
             (match rl with Empty -> 0 | Node(_,_,_,h) -> h) ->
        mknode (mknode l d rl) rd rr
      | Node (Node (rll, rld, rlr, _), rd, rr, _) ->
        mknode (mknode l d rll) rld (mknode rlr rd rr)
      | _ -> assert false
    else
      mknode l d r

  let rec add id data = function
      Empty ->
      Node(Empty, {ident = id; data = data; previous = None}, Empty, 1)
    | Node(l, k, r, h) ->
      let c = String.compare (name id) (name k.ident) in
      if c = 0 then
        Node(l, {ident = id; data = data; previous = Some k}, r, h)
      else if c < 0 then
        balance (add id data l) k r
      else
        balance l k (add id data r)

  let rec min_binding = function
      Empty -> raise Not_found
    | Node (Empty, d, _, _) -> d
    | Node (l, _, _, _) -> min_binding l

  let rec remove_min_binding = function
      Empty -> invalid_arg "Map.remove_min_elt"
    | Node (Empty, _, r, _) -> r
    | Node (l, d, r, _) -> balance (remove_min_binding l) d r

  let merge t1 t2 =
    match (t1, t2) with
      (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) ->
      let d = min_binding t2 in
      balance t1 d (remove_min_binding t2)

  let rec remove id = function
      Empty ->
      Empty
    | (Node (l, k, r, h) as m) ->
      let c = String.compare (name id) (name k.ident) in
      if c = 0 then
        match k.previous with
        | None -> merge l r
        | Some k -> Node (l, k, r, h)
      else if c < 0 then
        let ll = remove id l in if l == ll then m else balance ll k r
      else
        let rr = remove id r in if r == rr then m else balance l k rr

  let rec find_previous id = function
      None ->
      raise Not_found
    | Some k ->
      if equal id k.ident then k.data else find_previous id k.previous

  let rec lookup id = function
      Empty ->
      raise Not_found
    | Node(l, k, r, _) ->
      let c = String.compare (name id) (name k.ident) in
      if c = 0 then
        if equal id k.ident
        then k.data
        else find_previous id k.previous
      else
        lookup id (if c < 0 then l else r)

  let mem id tbl =
    try
      let _ = lookup id tbl in
      true
    with Not_found -> false
  
  let rec find n = function
      Empty ->
      raise Not_found
    | Node(l, k, r, _) ->
      let c = String.compare n (name k.ident) in
      if c = 0 then
        k.ident, k.data
      else
        find n (if c < 0 then l else r)

  let rec get_all = function
    | None -> []
    | Some k -> (k.ident, k.data) :: get_all k.previous

  let rec find_all n = function
      Empty ->
      []
    | Node(l, k, r, _) ->
      let c = String.compare n (name k.ident) in
      if c = 0 then
        (k.ident, k.data) :: get_all k.previous
      else
        find_all n (if c < 0 then l else r)

  let rec fold_aux f stack accu = function
      Empty ->
      begin match stack with
          [] -> accu
        | a :: l -> fold_aux f l accu a
      end
    | Node(l, k, r, _) ->
      fold_aux f (l :: stack) (f k accu) r

  let fold f tbl accu = fold_aux (fun k -> f k.ident k.data) [] accu tbl

  let rec fold_data f d accu =
    match d with
      None -> accu
    | Some k -> f k.ident k.data (fold_data f k.previous accu)

  let fold_all f tbl accu =
    fold_aux (fun k -> fold_data f (Some k)) [] accu tbl

  let rec iter f = function
      Empty -> ()
    | Node(l, k, r, _) ->
      iter f l; f k.ident k.data; iter f r

end
type 'a tbl = 'a Map.t
