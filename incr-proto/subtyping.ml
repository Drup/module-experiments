open Modules

type error =
  | Foo

let check_modtype
  : Env.t -> mod_type -> mod_type -> (mod_type, error) result
  = fun _env m1 m2 -> match m1, m2 with
    | _ -> Ok m2
