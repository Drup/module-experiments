open Core_types

let term fmt : term -> _  = function
  | Unit -> Fmt.pf fmt "()"

let val_type fmt : val_type -> _ = function
  | Unit -> Fmt.pf fmt "()"

let def_type fmt : def_type -> _ = function
  | Unit -> Fmt.pf fmt "()"
