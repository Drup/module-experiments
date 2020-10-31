open Modules

let rec path fmt { path ; field } =
  Fmt.pf fmt "@[<h>%a.%s@]" module_path path field

and ident fmt id = Fmt.string fmt (Ident.name id)

and module_type fmt = function
  | Strengthen (mty, p) ->
    Fmt.pf fmt "@[%a/%a@]" module_type mty module_path p
  | Let (id, mty1, mty2) ->
    Fmt.pf fmt "@[@[<2>@[<2>let@ %a@ =@]@ %a@]@ in@ %a@]"
      ident id
      module_type mty1
      module_type mty2
  | Enrich (mty, Module (fields, mty')) ->
    Fmt.pf fmt "@[%a@ with@ %a@ :@ %a@]"
      module_type mty
      (Fmt.list ~sep:(Fmt.unit ".") Fmt.string) fields
      module_type mty'
  | Enrich (mty, Type (fields, ty)) ->
    Fmt.pf fmt "@[%a@ with@ %a@ =@ %a@]"
      module_type mty
      (Fmt.list ~sep:(Fmt.unit ".") Fmt.string) fields
      Core_printer.def_type ty
  | Core mtyc -> mod_type_core fmt mtyc

and mod_type_core fmt = function
  | TPath p -> path fmt p
  | Alias p -> Fmt.pf fmt "@[(= %a)@]" module_path p
  | Signature { sig_self; sig_content } ->
    Fmt.pf fmt "@[<hov>@[<hov 2>sig (%a)@ %a@]@ end@]"
      ident sig_self
      (Fmt.list ~sep:Fmt.sp signature_item) sig_content
  | Functor_type (id, param, body) ->
    Fmt.pf fmt "@[<hov2>@[(%a@ :@ %a)@ ->@]@ %a@]"
      ident id
      module_type param
      module_type body

and signature_item fmt = function
  | Value_sig (field, ty) ->
    Fmt.pf fmt "@[<2>val %s =@ %a@]"
      field
      Core_printer.val_type ty
  | Type_sig (field, tydecl) ->
    Fmt.pf fmt "@[<2>type %s%a@]"
      field
      type_decl tydecl
  | Module_sig (field, mty) ->
    module_declaration fmt field mty
  | Module_type_sig (field, mty) ->
    Fmt.pf fmt "@[<2>module %s =@ %a@]"
      field
      module_type mty

and module_declaration fmt name mty = match mty with
  | Core (Alias p) -> 
    Fmt.pf fmt "@[<2>module %s =@ %a@]"
      name
      module_path p
  | Core (Signature { sig_self; sig_content }) -> 
    Fmt.pf fmt "@[<v>@[<v 2>module %s : sig (%a)@ %a@]@ end@]"
      name
      ident sig_self
      (Fmt.list ~sep:Fmt.sp signature_item) sig_content
  | _ -> 
    Fmt.pf fmt "@[<2>module %s :@ %a@]"
      name
      module_type mty

and module_path : _ -> mod_path -> unit =
  fun fmt mdt -> match mdt with
    | Id id -> ident fmt id
    | Proj p -> path fmt p
    | Ascription (md, mty) ->
      Fmt.pf fmt "@[<2>(%a@ <:@ %a)@]" module_path md module_type mty
    | Apply (md1, md2) ->
      Fmt.pf fmt "@[<2>%a@ %a@]" module_path md1 module_path md2

and type_decl fmt { manifest ; definition } =
  match manifest, definition with
  | None, None -> ()
  | Some p, None ->
    Fmt.pf fmt " =@ %a" path p
  | None, Some def ->
    Fmt.pf fmt " =@ %a" Core_printer.def_type def
  | Some p, Some def ->
    Fmt.pf fmt " @[=@ %a@]@ @[=@ %a@]"
      path p
      Core_printer.def_type def

and unit fmt l = 
  Fmt.pf fmt "@[<v>%a@]@."
    (Fmt.list ~sep:Fmt.cut signature_item) l

module Untyped = struct
  open Parsetree
  
  let rec path fmt { path ; field } =
    Fmt.pf fmt "@[<h>%a.%s@]" module_term path field

  and module_term : _ -> mod_term -> unit =
    fun fmt mt -> match mt with
      | Id id -> Fmt.string fmt id
      | Proj p -> path fmt p
      | Ascription (mt, mty) ->
        Fmt.pf fmt "@[<2>(%a@ <:@ %a)@]" module_term mt module_type mty
      | Apply (md1, md2) ->
        Fmt.pf fmt "@[<2>%a@ %a@]" module_term md1 module_term md2
      | Structure { str_self; str_content } -> 
        Fmt.pf fmt "@[<v>@[<v 2>sig (%s)@ %a@]@ end@]"
          str_self
          (Fmt.list ~sep:Fmt.cut structure_item) str_content
      | Functor (id, param, body) ->
        Fmt.pf fmt "@[<hov2>@[(%s@ :@ %a)@ ->@]@ %a@]"
          id
          module_type param
          module_term body
      | Constraint (mt, mty) ->
        Fmt.pf fmt "@[<2>(%a@ :@ %a)@]" module_term mt module_type mty


  and module_type fmt = function
    | Let (id, mty1, mty2) ->
      Fmt.pf fmt "@[@[<2>@[<2>let@ %s@ =@]@ %a@]@ in@ %a@]"
        id
        module_type mty1
        module_type mty2
    | Enrich (mty, Module (fields, mty')) ->
      Fmt.pf fmt "@[%a@ with@ %a@ :@ %a@]"
        module_type mty
        (Fmt.list ~sep:(Fmt.unit ".") Fmt.string) fields
        module_type mty'
    | Enrich (mty, Type (fields, ty)) ->
      Fmt.pf fmt "@[%a@ with@ %a@ =@ %a@]"
        module_type mty
        (Fmt.list ~sep:(Fmt.unit ".") Fmt.string) fields
        Core_printer.def_type ty
    | TPath p -> path fmt p
    | Alias p -> Fmt.pf fmt "@[(= %a)@]" module_term p
    | Signature { sig_self; sig_content } ->
      Fmt.pf fmt "@[<v>@[<v 2>sig (%s)@ %a@]@ end@]"
        sig_self
        (Fmt.list ~sep:Fmt.cut signature_item) sig_content
    | Functor_type (id, param, body) ->
      Fmt.pf fmt "@[<hov2>@[(%s@ :@ %a)@ ->@]@ %a@]"
        id
        module_type param
        module_type body
  
  and signature_item fmt = function
    | Value_sig (field, ty) ->
      Fmt.pf fmt "@[<2>val %s =@ %a@]"
        field
        Core_printer.val_type ty
    | Type_sig (field, tydecl) ->
      Fmt.pf fmt "@[<2>type %s %a@]"
        field
        type_decl tydecl
    | Module_sig (field, mty) ->
      Fmt.pf fmt "@[<2>module %s :@ %a@]"
        field
        module_type mty
    | Module_type_sig (field, mty) ->
      Fmt.pf fmt "@[<2>module %s =@ %a@]"
        field
        module_type mty

  and type_decl fmt { manifest ; definition } =
    match manifest, definition with
    | None, None -> ()
    | Some p, None ->
      Fmt.pf fmt "=@ %a" path p
    | None, Some def ->
      Fmt.pf fmt "=@ %a" Core_printer.def_type def
    | Some p, Some def ->
      Fmt.pf fmt "@[=@ %a@]@ @[=@ %a@]"
        path p
        Core_printer.def_type def

  and structure_item fmt = function
    | Value_str (field, t) ->
      Fmt.pf fmt "@[<2>let %s =@ %a@]"
        field
        Core_printer.term t
    | Type_str (field, tydecl) ->
      Fmt.pf fmt "@[<2>type %s %a@]"
        field
        type_decl tydecl
    | Module_str (field, mty) ->
      Fmt.pf fmt "@[<2>module %s :@ %a@]"
        field
        module_term mty
    | Module_type_str (field, mty) ->
      Fmt.pf fmt "@[<2>module %s =@ %a@]"
        field
        module_type mty

end
