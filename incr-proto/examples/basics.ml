module A = struct (A)
  type t = ()
  type t2 = ()
  let a = ()
end

module B = (Basics.A <: sig (X) type t end)

type t = Basics.B.t2

(* module type B = struct (B) end *)
