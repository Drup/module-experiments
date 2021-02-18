module A = struct (A)
  type t = unit
  type t2 = unit
  let a = ()
  module M = struct type t = A.t end
end
(*EXPECT
module A : sig (A)
  type t = unit
  type t2 = unit
  module M : sig type t = A.t end
  val a : unit
end
*)

module A' = (A <: sig type t end)
type t2' = A'.t2
(*EXPECT
module A' = (A <: sig type t end)
type t2' = A'.t2
*)

module type Alias = (= A)
module type Alias' = (Alias <: sig type t end)
module X = (A <: Alias').M
(*EXPECT
module Alias = (= A)
module Alias' = (= (A <: sig type t end))
module X = (A <: sig type t end).M
*)
module Y = (X <: sig val a : unit end)
(*EXPECT
Error: The module
         sig type t = (A <: sig type t end).M.t end
       is not included in
         sig val a : unit end
*)

module type C = (sig type t = unit end <: sig type t end)
(*EXPECT
module C = sig type t = unit end
*)
