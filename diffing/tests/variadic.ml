module type T = sig
  module type Ty
  module M : Ty
end

module F(X:T) = X.M
module Ft = struct
  module M = F
  module type Ty = functor(X:T) -> X.Ty
end

module M = F
    (Ft)(Ft)(List)(Ft)(Ft)(Ft)(List)(Ft)(Ft)(List)
    (Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)
    (Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)
    (Ft)(Ft)(Ft)(Ft)(Ft)(Arg)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)
    (Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)
    (Ft)(Ft)(Ft)(Float)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)
    (Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)
    (Ft)(Ft)(Ft)(Ft)(Ft)(Gc)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)
    (Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)
    (Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Map)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)
    (Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)
    (Ft)(Ft)(Seq)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)
    (Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)
    (Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Queue)(Ft)(Ft)(Ft)(Ft)(Ft)
    (Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)
    (Ft)(Ft)(Ft)(Bigarray)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)
    (Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)
    (Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)(Ft)
