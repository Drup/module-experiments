module Pic = struct

  type pos = float * float
  let (+~) (x1,y1) (x2,y2) = (x1+.x2,y1+.y2)
  
  type t =
    | Point of pos
    | Polyline of pos list
    | Many of t list

  let square lb size =
    let r = (size,0.) and rt = (size,size) and t = (0.,size) in
    Polyline [lb; lb+~r; lb+~rt; lb+~t; lb]
  
  let render t =
    let open Tyxml.Svg in
    let rec aux = function
      | Point (x, y) ->
        circle ~a:[a_r (0.1, None); a_cx (x,None); a_cy (y,None)] []
      | Polyline l ->
        polyline ~a:[a_points l] []
      | Many l -> g (List.map aux l)
    in
    aux t
end     








module type RENDERER = sig
  type output
  val point : float * float -> output
  val polyline : (float * float) list -> output
  val many : output list -> output
end


module Svg = struct
  open Tyxml.Svg
  type output = [`Circle | `G | `Polyline ] elt
  let point (x,y) = 
    circle ~a:[a_r (0.1, None); a_cx (x,None); a_cy (y,None)] []
  let polyline l = 
    polyline ~a:[a_points l] []
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
  
module type POINT = sig
  type number
  type t
  val (+) : t -> t -> t
  val v : number -> number -> t
end
module Coord = struct
  type number = float
  type t = float * float
  let (+) (x1,y1) (x2,y2) = (x1+.x2,y1+.y2)
  let v x y = (x,y)
end


module type RENDERER2 = sig
  type output
  type point
  val point : point -> output
  val polyline : point list -> output
  val many : output list -> output
end
module PicFinal
    (P : POINT with type number = float)
    (R : RENDERER2 with type point := P.t) =
struct
  
  type t =
    | Point of P.t
    | Polyline of P.t list
    | Many of t list

  let square lb size =
    let r = P.v size 0. and rt = P.v size size and t = P.v 0. size in
    Polyline P.[lb; lb+r; lb+rt; lb+t; lb]
  
  let render t =
    let rec aux = function
      | Point p -> R.point p
      | Polyline l -> R.polyline l
      | Many l -> R.many (List.map aux l)
    in
    aux t
end

module Cairo () = struct
  type output = unit

  let cr =
    let window = GWindow.window ~title:"Hello" () in
    let area = GMisc.drawing_area ~packing:window#add () in
    Cairo_gtk.create area#misc#window
  
  let point (x,y) =
    Cairo.translate cr x y;
    Cairo.arc cr 0. 0. ~r:50. ~a1:0. ~a2:(2. *. Float.pi);
    Cairo.fill cr
  let polyline l = match l with
    | [] -> ()
    | (x,y) :: t ->
      Cairo.move_to cr x y;
      List.iter (fun (x,y) -> Cairo.line_to cr x y) t
  let many _l = ()
end








type 'a backend = 
  | Svg : Svg.output backend
  | Graphics : unit backend
  | Cairo : unit backend

let dynamic_render (type a) (backend : a backend) : a =
  let module R = (val (match backend with
    | Svg -> (module Svg : RENDERER with type output = a)
    | Graphics -> (module Graphics : RENDERER with type output = a)
    | Cairo -> (module Cairo () : RENDERER with type output = a)))
  in
  let module M = PicFinal(Coord)(R) in
  M.render @@ M.square (13.,37.) 42.







module PicX (P : POINT with type number := float) = struct
  
  type t =
    | Point of P.t
    | Polyline of P.t list
    | Many of t list

  let square lb size =
    let r = P.v size 0. and rt = P.v size size and t = P.v 0. size in
    Polyline P.[lb; lb+r; lb+rt; lb+t; lb]

  module type R = RENDERER2 with type point := P.t
  let render (type o) (module R : R with type output = o) t : o =
    let rec aux = function
      | Point p -> R.point p
      | Polyline l -> R.polyline l
      | Many l -> R.many (List.map aux l)
    in
    aux t
end

module P1 = PicX(Coord)
let p1 = P1.Point (Coord.v 0. 0.)
module P2 = PicX(Coord)
let p2 = P2.square (Coord.v 1. 42.) 3.14

let p = P1.Many [p1;p2]

open Gg
let picture : PicX(V2).t =
  let module P = PicX(V2) in
  P.Many [
    Point (V2.v 0. 0.);
    Point (V2.v 2. 0.);
    Point (V2.v 4. 0.);
  ]
