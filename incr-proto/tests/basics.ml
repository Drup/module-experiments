module A = struct (A)
  type t = ()
  type t2 = ()
  let a = ()
  module M = struct (M) type t = A.t end
end
(*EXPECT
module A : sig (A)
  type t = ()
  type t2 = ()
  module M : sig (M) type t = A.t end
  val a = ()
end
*)

module A' = (A <: sig (X) type t end)
type t2' = A'.t2
(*EXPECT
module A' = (A <: sig (X) type t end)
type t2' = A'.t2
*)

module type Alias = (= A)
module type Alias' = (Alias <: sig (X) type t end)
module X = (A <: Alias').M
(*EXPECT
module Alias = (= A)
module Alias' = (= (A <: sig (X) type t end))
module X = (A <: sig (X) type t end).M
*)
module Y = (X <: sig (X) val a : () end)
(*EXPECT
Error: The module
         sig (M) type t = (A <: sig (X) type t end).M.t end
       is not included in
         sig (X) val a = () end
*)

module type C = (sig (X) type t = () end <: sig (X) type t end)
(*EXPECT
module C = sig (X) type t = () end
*)
