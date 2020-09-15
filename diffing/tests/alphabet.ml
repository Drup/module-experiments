

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

module Little
    (A: module type of A)
    (B: module type of B)
    (C: module type of C)
    (D: module type of D)
    (E: module type of E)
    (F: module type of F)
    (G: module type of G)
    (H: module type of H) = struct end

module E = Little
    (A)(A)
    (B)
    (C')
    (E)
    (F')
    (G)
    (H)(H')
