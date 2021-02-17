module P = Parsetree.Core
module T = Types.Core

type error =
  | Unbound_variable of Parsetree.path
  | Unbound_type_constructor of Parsetree.path
exception Error of error
let error e = raise (Error e)

module Typing = struct
  let term
    : Env.t -> P.term -> T.val_type
    = fun env -> function
      | Unit -> Unit
      | Variable p ->
        match Env.find_value env p with
        | None -> error @@ Unbound_variable p
        | Some (_, ty) -> ty
end

module Transl = struct
  let val_type
    : Env.t -> P.val_type -> T.val_type
    = fun _ -> function
      | Unit -> T.Unit
  let def_type
    : Env.t -> P.def_type -> T.def_type
    = fun env -> function
      | Alias ty -> Alias (val_type env ty)
end

module Validity = struct
  let val_type
    : Env.t -> T.val_type -> unit
    = fun _ Unit -> ()
  let def_type
    : Env.t -> T.def_type -> unit
    = fun env -> function
      | Alias ty -> val_type env ty
end

module Include = struct
  let val_type
    : Env.t -> T.val_type -> T.val_type -> unit
    = fun _ vdecl1 vdecl2 -> match vdecl1, vdecl2 with
      | Unit, Unit -> ()

  let def_type
    : Env.t -> T.def_type -> T.def_type -> unit
    = fun env tydecl1 tydecl2 -> match tydecl1, tydecl2 with
      | Alias ty1, Alias ty2 -> val_type env ty1 ty2
end

(** Errors *)
  
let prepare_error = function
  | Unbound_variable p -> 
    Report.errorf "Unbound variable %a" Printer.Untyped.path p
  | Unbound_type_constructor p -> 
    Report.errorf "Unbound type constructor %a" Printer.Untyped.path p

let () = Report.register_report_of_exn @@ function
  | Error e -> Some (prepare_error e)
  | _ -> None
