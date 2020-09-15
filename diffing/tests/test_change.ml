module type XT = sig type x end
module type YT = sig type y end
module type ZT = sig type z end
module F(X:XT)(Y:YT)(Z:ZT)
= struct end
module X = struct type x = A end
module Y = struct type y = B end
module Y' = struct type y' = B end
module Z = struct type z = C end

module Result = F(X)(Y')(Z)
