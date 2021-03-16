module A = struct (A) type t = () let a = () end
module type S = sig type t val a : unit end
(*EXPECT
module A : sig (A) type t = () val a : unit end
module S = sig type t val a : unit end
*)

module Bnamed =  (functor (X :S) -> X)(A)
let a = Bnamed.a
(*EXPECT
module Bnamed :
  let X = (A <: sig type t val a : unit end) in let X = X in (= X)
val a : unit
*)

module Binlined =  (functor (X :S) -> X)(struct (A) type t = () let a = () end)
module B = (Binlined : sig val a : unit end)
(*EXPECT
module Binlined :
  let X = sig (A) type t = () val a : unit end in let X = X in (= X)
Error: Cannot eliminate let in the module type
         let X = sig (A) type t = () val a : unit end in let X = X in (= X)
*)
