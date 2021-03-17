(** Requires the following packages: gg, graphics, tyxml *)
#require "gg,graphics,tyxml";; (* For loading in the toplevel *)

(** The signature for Points *)
module type TYPEPOINT = sig
  type t
  val v : float -> float -> t
  val ( + ) : t -> t -> t
  val x : t -> float
  val y : t -> float
end

(** An hand-made implementation with tuples *)
module PointTuple = struct
  type t = float * float
  let (+) (x1,y1) (x2,y2) = (x1+.x2,y1+.y2)
  let v x y = (x,y)
  let x = fst
  let y = snd
end

(** A implementation with 2D Points from a library.
    This module is an alias. Equalities are preserved.
    It happens to have the right type!
*)
module PointLib = Gg.V2

(** The type of Renderers *)
module type RENDERER = sig
  type output
  type point
  val point : point -> output
  val polyline : point list -> output
  val many : output list -> output
end

module Pic (Point : TYPEPOINT) = struct

  type t =
    | Point of Point.t
    | Polyline of Point.t list
    | Many of t list

  let square lb size =
    let open Point in
    let r = v size 0. and rt = v size size and t = v 0. size in
    Polyline [lb; lb+r; lb+rt; lb+t; lb]

  (** A "direct" renderer, which produces SVGs *)
  let render t =
    let open Tyxml.Svg in
    let rec aux = function
      | Point p ->
        circle ~a:[
          a_r (0.1, None);
          a_cx (Point.x p,None); a_cy (Point.y p,None)] []
      | Polyline l ->
        polyline ~a:[a_points @@
                     List.map (fun p -> Point.x p, Point.y p) l] []
      | Many l -> g (List.map aux l)
    in
    aux t

  (** A "parameterized" renderer, which takes a so-called
      "first class" module as argument.
      The typing is a bit complicated for now, we have to state all
      the equalities by hand. This will change in OCaml soon.
  *)      
  let render'
      (type o)
      (module R :
        RENDERER with type point = Point.t and type output = o)
      t
    : o =
    let rec aux = function
      | Point p -> R.point p
      | Polyline l -> R.polyline l
      | Many l -> R.many (List.map aux l)
    in
    aux t

end     

(** Let's build some pictures *)

(* F is applicative
   iff X == Y => F(X) = F(Y)
*)

module MyPic = Pic(PointTuple)

let origin = PointTuple.v 0. 0.
let x = MyPic.Point origin

(** Functors in OCaml are *applicative*, which mean we can do this: *)
module MyPic2 = Pic(PointTuple)
let sq = MyPic2.square origin 100.

(** Pics from both modules are compatible! *)
let image = MyPic.Many [x ; sq]

(** Finally, we can render *)
let svg = MyPic.render image
let () = Format.printf "%a@." (Tyxml.Svg.pp_elt ()) svg

(** We can also use the parameterized renderer. For this
    we need a renderer, for example, with the Graphics module:
*)
module GraphicsRender = struct
  type point = PointTuple.t
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

(** And then we can render! *)
let () =
  Graphics.open_graph " ";
  MyPic.render' (module GraphicsRender) image
