module P = Parsetree.Core
module T = Types.Core

type error =
  | Unbound_variable of Parsetree.path
  | Unbound_type of Parsetree.path
  | Unknown_type of Types.path
  | Not_a_subtype of T.val_type * T.val_type
exception Error of error
let error e = raise (Error e)

module Typing = struct
  let term
    : Env.t -> P.term -> T.val_type
    = fun env -> function
      | Unit -> Env_initial.unit_ty
      | Variable p ->
        match Env.find_value env p with
        | None -> error @@ Unbound_variable p
        | Some (_, ty) -> ty
          
end

module Transl = struct
  let val_type
    : Env.t -> P.val_type -> T.val_type
    = fun env -> function
      | Type p -> 
        match Env.find_type env p with
        | None -> error @@ Unbound_type p
        | Some (p', _) -> Type p'
  let def_type
    : Env.t -> P.def_type -> T.def_type
    = fun env -> function
      | Unit -> T.Unit
end

module Validity = struct
  let val_type
    : Env.t -> T.val_type -> unit
    = fun _ -> function
      | Type _ -> ()
  let def_type
    : Env.t -> T.def_type -> unit
    = fun env -> function
      | Unit -> ()

  (* let normalize_path_type env (p : Types.path) = match p with
   *   | PathId _ -> p
   *   | PathProj { path; field } ->
   *     PathProj { path = normalize env path ; field } *)

  let rec normalize_type
    : Env.t -> Types.path -> Types.path
    = fun env p ->
      let tydecl = match Env.lookup_type env p with
        | Some m -> m
        | None -> error @@ Unknown_type p
      in
      match tydecl with
      | TypeAlias Type p
      | Definition { manifest = Some p; definition = _ } ->
        normalize_type env p
      | Definition { manifest = None; definition = _ }
      | Abstract ->
        Env.canonical_type env p

end

module Include = struct
  let type_path env path1 path2 =
    let path1' = Validity.normalize_type env path1 in
    let path2' = Validity.normalize_type env path2 in
    (* Fmt.epr
     *   "@[<v2>Checking that the types equivalence@ %a = %a@ and@ %a = %a@]@."
     *   Printer.path path1 Printer.path path1'
     *   Printer.path path2 Printer.path path2'; *)
    path1' = path2'

  let val_type
    : Env.t -> T.val_type -> T.val_type -> unit
    = fun env vdecl1 vdecl2 -> match vdecl1, vdecl2 with
      | Type t1, Type t2 ->
        if type_path env t1 t2 then
          ()
        else
          error @@ Not_a_subtype (vdecl1, vdecl2)

  let def_type
    : Env.t -> T.def_type -> T.def_type -> unit
    = fun env tydecl1 tydecl2 -> match tydecl1, tydecl2 with
      | Unit, Unit -> ()
end

(** Errors *)
  
let prepare_error = function
  | Unbound_variable p -> 
    Report.errorf "Unbound variable %a" Printer.Untyped.path p
  | Unbound_type p -> 
    Report.errorf "Unbound type constructor %a" Printer.Untyped.path p
  | Unknown_type p ->
    Report.errorf "Unbound type %a" Printer.path p
  | Not_a_subtype (ty1, ty2) ->
    Report.errorf
      "@[<v>@[<v2>The type@ @[%a@]@]@,@[<v2>is not a subtype of@ @[%a@]@]@]"
      Printer.val_type ty1
      Printer.val_type ty2

let () = Report.register_report_of_exn @@ function
  | Error e -> Some (prepare_error e)
  | _ -> None
