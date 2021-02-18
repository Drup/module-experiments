module type M = sig (X)
  type t
  val x : X.t
end
(*EXPECT
module M = sig (X) type t val x : X.t end
*)

module A = struct (A)
  type t = unit
  let x = ()
  let y = ()
end
(*EXPECT
module A : sig (A) type t = unit val x : unit val y : unit end
*)

module F (X : M) = struct (R)
  type t
  module Y = X
end
(*EXPECT
module F : (X : M) -> sig (R) type t module Y = X end
*)

module R = F(A).Y
(*EXPECT
module R = F(A).Y
*)

module TestApplicative = (R : (=F(A).Y))
(*EXPECT
module TestApplicative = F(A).Y
*)

module TestAscript = (R : sig val y : unit end)
(*EXPECT
Error: The module
         sig (A)
           type t = (A <: let R = F(A) in sig (X) type t val x : X.t end).t
           val x : A.t
         end
       is not included in
         sig val y : unit end
*)

let x = R.x
let y = R.y
(*EXPECT
val x : R.t
Error: Unbound variable R.y
*)
