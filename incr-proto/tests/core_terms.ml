let x = ()
let y = x
(*EXPECT
val x : unit
val y : unit
*)

module A = struct let a = y end
(*EXPECT
module A : sig val a : unit end
*)

let z = A.a
(*EXPECT
val z : unit
*)

let z' = (A <: sig val a : unit end).a
(*EXPECT
val z' : unit
*)

let z'' = (A <: sig end).a
(*EXPECT
Error: Unbound variable (A <: sig
                                
                              end).a
*)
