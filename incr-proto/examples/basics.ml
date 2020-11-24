module A = struct (A)
  type t = ()
  type t2 = ()
  let a = ()
end

module B = (Basics.A <: sig (X) type t end)

type tb = Basics.B.t2

module C = (Basics.B <: sig (X) val a : () end)

type tc = Basics.C.t
