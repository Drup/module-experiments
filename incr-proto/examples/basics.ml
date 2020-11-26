module A = struct (A)
  type t = ()
  type t2 = ()
  let a = ()
  module M = struct (M) type t = A.t end
end

module A' = (Basics.A <: sig (X) type t end)
type t2' = Basics.A'.t2

module type Alias = (= Basics.A)
module type Alias' = (Basics.Alias <: sig (X) type t end)
module X = (Basics.A <: Basics.Alias').M
module Y = (Basics.X <: sig (X) val a : () end)

module type C = (sig (X) type t = () end <: sig (X) type t end)


