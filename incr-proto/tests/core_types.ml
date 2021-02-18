type x
type y
module A = struct type a = x end
(*EXPECT
type x
type y
module A : sig type a = x end
*)

type testLookup = x
type testLookupMod = A.a
type testLookupFail = A.t
(*EXPECT
type testLookup = x
type testLookupMod = A.a
Error: Unbound type constructor A.t
*)

module TestLookupIncl
  : sig type t = A.a end = struct type t = A.a end
module TestAscriptionIncl
  : sig type t = A.a end = struct type t = (A <: sig end).a end
module TestInclFail
  : sig type t = A.a end = struct type t = y end
(*EXPECT
module TestLookupIncl : sig type t = A.a end
module TestAscriptionIncl : sig type t = A.a end
Error: The type
         y
       is not a subtype of
         A.a
*)

type a
module X = struct type x = a end
module F (M : sig type x end) = struct type x = M.x end
(*EXPECT
type a
module X : sig type x = a end
module F : (M : sig type x end) -> sig type x = M.x end
*)

module A = F(X)
(*EXPECT
module A = F(X)
*)

type testLookupFunctor = A.x
type testLookupFunctorPath = F(X).x
(*EXPECT
type testLookupFunctor = A.x
type testLookupFunctorPath = F(X).x
*)
type testLookupFunctorFail = A.y
(*EXPECT
Error: Unbound type constructor A.y
*)
type testLookupFunctorPathFail = F(X).x
(*EXPECT
type testLookupFunctorPathFail = F(X).x
*)

module TestInclFunctor : sig type x = a end = A
module TestInclFunctor2 : sig type x = X.x end = A
module TestInclFunctor3 : sig type x = X.x end = struct type x = A.x end
module TestInclFunctorApp : sig type x = F(X).x end = A
(*EXPECT
module TestInclFunctor : sig type x = a end
module TestInclFunctor2 : sig type x = X.x end
module TestInclFunctor3 : sig type x = X.x end
module TestInclFunctorApp : sig type x = F(X).x end
*)

module TestInclFunctorAppFail : sig type x = y end = A
(*EXPECT
Error: The type
         F(X).x
       is not a subtype of
         y
*)

module X = struct type x end
type testLookupFunctorDef = F(X).x
module TestInclFunctorDef : sig type x = F(X).x end = F(X)
module TestInclFunctorDef2 : sig type x = X.x end = F(X)
module TestInclFunctorDef3 : sig type x = X.x end = struct type x = F(X).x end
(*EXPECT
module X : sig type x end
type testLookupFunctorDef = F(X).x
module TestInclFunctorDef : sig type x = F(X).x end
Error: The type
         F(X).x
       is not a subtype of
         X.x
*)
