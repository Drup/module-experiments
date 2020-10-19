open Modules

let rec path fmt { path ; field } =
  Fmt.pf fmt "@[<h>%a.%s@]" mod_path path field

and ident fmt id = Fmt.string fmt (Ident.name id)

and mod_type fmt = function
  | Strengthen (mty, p) ->
    Fmt.pf fmt "@[%a/%a@]" mod_type mty mod_path p
  | Let (id, mty1, mty2) ->
    Fmt.pf fmt "@[@[<2>@[<2>let@ %a@ =@]@ %a@]@ in@ %a@]"
      ident id
      mod_type_core mty1
      mod_type mty2
  | Enrich (mty, Module (fields, mty')) ->
    Fmt.pf fmt "@[%a@ with@ %a@ :@ %a@]"
      mod_type mty
      (Fmt.list ~sep:(Fmt.unit ".") Fmt.string) fields
      mod_type mty'
  | Enrich (mty, Type (fields, ty)) ->
    Fmt.pf fmt "@[%a@ with@ %a@ =@ %a@]"
      mod_type mty
      (Fmt.list ~sep:(Fmt.unit ".") Fmt.string) fields
      Core_printer.def_type ty
  | Core mtyc -> mod_type_core fmt mtyc

and mod_type_core fmt = function
  | TPath p -> path fmt p
  | Alias p -> Fmt.pf fmt "@[(= %a)@]" mod_path p
  | Signature { sig_self; sig_content } ->
    Fmt.pf fmt "@[@[<hov 2>sig (%a)@ %a@]@ end@]"
      ident sig_self
      (Fmt.list ~sep:Fmt.cut signature_item) sig_content
  | Functor_type (id, param, body) ->
    Fmt.pf fmt "@[<hov2>@[(%a@ :@ %a)@ ->@]@ %a@]"
      ident id
      mod_type param
      mod_type body

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
      mod_type mty
  | Module_type_sig (field, mty) ->
    Fmt.pf fmt "@[<2>module %s =@ %a@]"
      field
      mod_type mty

and mod_path : _ -> mod_path -> unit =
  fun fmt mdt -> match mdt with
    | Id id -> ident fmt id
    | Proj p -> path fmt p
    | Ascription (md, mty) ->
      Fmt.pf fmt "@[<2>(%a@ :>@ %a)@]" mod_path md mod_type mty
    | Apply (md1, md2) ->
      Fmt.pf fmt "@[<2>%a@ %a@]" mod_path md1 mod_path md2

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

and unit fmt l = 
  Fmt.pf fmt "@[<v 2>%a@]@."
    (Fmt.list ~sep:Fmt.cut signature_item) l
