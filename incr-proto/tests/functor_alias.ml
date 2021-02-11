module type M = sig (X)
  type t
  val x : ()
end
(*EXPECT
module M = sig (X) type t val x = () end
*)

module A = struct
  type t
  let x = ()
  let y = ()
end
(*EXPECT
module A : sig type t val x = () val y = () end
*)

module F (X : M) = struct
  type t
  module Y = X
end
(*EXPECT
module F : (X : M) -> sig type t module Y = X end
*)

module R = F(A).Y
(*EXPECT
module R = F(A).Y
*)

module TestApplicative = (R : (=F(A).Y))
(*EXPECT
module TestApplicative = F(A).Y
*)

module TestAscript = (R : sig val y : () end)
(*EXPECT
Error: The module
         sig
           type t = (A <: let _ = F(A) in sig (X) type t val x = () end).t
           val x = ()
         end
       is not included in
         sig val y = () end
*)
