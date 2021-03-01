(** Requires the following packages: gg, graphics, tyxml *)

(** How to definition positions? *)

module type POS = sig
  type t
  val v : float -> float -> t
  val ( + ) : t -> t -> t
  val x : t -> float
  val y : t -> float
end

module Pos = struct
  type t = float * float
  let (+) (x1,y1) (x2,y2) = (x1+.x2,y1+.y2)
  let v x y = (x,y)
  let x = fst
  let y = snd
end

(** On what are we rendering? *)

module type RENDERER = sig
  type output
  type point
  val point : point -> output
  val polyline : point list -> output
  val many : output list -> output
end

module Svg = struct
  open Tyxml.Svg
  type output = [`Circle | `G | `Polyline ] elt
  let point p = 
    circle ~a:[a_r (0.1, None); a_cx (Gg.V2.x p,None); a_cy (Gg.V2.y p,None)] []
  let polyline l = 
    polyline ~a:[a_points (List.map Gg.V2.to_tuple l)] []
  let many l = g l
end

module Graphics = struct
  type output = unit
  let iof = int_of_float
  let point (x,y) =
    Graphics.plot (iof x) (iof y)
  let polyline l = match l with
    | [] -> ()
    | (x,y) :: t ->
      Graphics.moveto (iof x) (iof y);
      List.iter (fun (x,y) -> Graphics.lineto (iof x) (iof y)) t
  let many _l = ()
end

(** The picture API! *)
module Pic (P : POS) = struct
  
  type t =
    | Point of P.t
    | Polyline of P.t list
    | Many of t list

  let point p = Point p
  let polyline l = Polyline l
  let many l = Many l
  let (++) a b = many [a;b]
  
  let square lb size =
    let r = P.v size 0. and rt = P.v size size and t = P.v 0. size in
    polyline P.[lb; lb+r; lb+rt; lb+t; lb]
  
  module type RS = RENDERER with type point := P.t

  let render (type o) (module R : RS with type output = o) t : o =
    let rec aux = function
      | Point p -> R.point p
      | Polyline l -> R.polyline l
      | Many l -> R.many (List.map aux l)
    in
    aux t
end     

module A = Pic(Pos)

(* Applications on the same modules are compatible! *)
module B = Pic(Gg.V2)
module B2 = Pic(Gg.V2)

let x = B.point (Gg.V2.v 0. 0.)
let x2 = B2.point  (Gg.V2.v 0. 0.)
let pic : Pic(Gg.V2).t = B.many [x;x2]

(* We can use a renderer compatible with our choice of points *)
let svg = B.render (module Svg) pic
