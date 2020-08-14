

module A = Seq
module A' = Complex
module B = Option
module B' = Bigarray
module C = Result
module C' = Bool
module D = Char
module D' = Uchar
module E = Sys
module E' = List
module F = Bytes
module F' = Map.Make
module G = String
module G' = Unit
module H = Marshal
module H' = Obj
module I = Array
module I' = Float
module J = Int
module J' = Int32
module K = Int64
module K' = Nativeint
module L = Lexing
module L' = Parsing
module M = Set
module M' = Map
module N = Stack
module N' = Queue
module O =CamlinternalLazy
module O' = Lazy
module P = Stream
module P' = Buffer
module Q = CamlinternalFormat
module Q' = Printf
module R = Arg
module R' = ArrayLabels
module S = Printexc
module S' = Fun
module T = Gc
module T' = Digest
module U = Random
module U' = Hashtbl
module V = Weak
module V' = Format
module W = Scanf
module W' = Callback
module X = CamlinternalOO
module X' = Oo
module Y = CamlinternalMod
module Y' = Genlex
module Z = Ephemeron
module Z' = Filename

module Big
    (A: module type of A)
    (B: module type of B)
    (C: module type of C)
    (D: module type of D)
    (E: module type of E)
    (F: module type of F)
    (G: module type of G)
    (H: module type of H)
    (I: module type of I)
    (J: module type of J)
    (K: module type of K)
    (L: module type of L)
    (M: module type of M)
    (N: module type of N)
    (O: module type of O)
    (P: module type of P)
    (Q: module type of Q)
    (R: module type of R)
    (S: module type of S)
    (T: module type of T)
    (U: module type of U)
    (V: module type of V)
    (W: module type of W)
    (X: module type of X)
    (Y: module type of Y)
    (Z: module type of Z)


= struct end

module E = Big
    (A)(A)
    (B)
    (C')
    (E)
    (F')
    (G)
    (H)(H')
    (I)
    (J)
    (L)
    (M)
    (N')
    (O)
    (O')
    (P)
    (R)
    (S)
    (T')
    (U')
    (V)
    (W)(W)
    (X)
    (Y)
    (Z)(Z)
