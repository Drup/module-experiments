let x = ()
let y = x
(*EXPECT
val x = ()
val y = ()
*)

module A = struct let a = y end
(*EXPECT
module A : sig val a = () end
*)

let z = A.a
(*EXPECT
val z = ()
*)

let z' = (A <: sig val a : () end).a
(*EXPECT
val z' = ()
*)

let z'' = (A <: sig end).a
(*EXPECT
Error: Unbound variable (A <: sig
                                
                              end).a
*)
