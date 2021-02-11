open Modules

let fields f m fmt =
  FieldMap.iter (fun k d -> Fmt.sp fmt () ; f fmt k d) m

let bound_name = Fmt.(option ~none:(unit "_") string)

let rec path fmt = function
  | PathId id -> ident fmt id
  | PathProj p -> proj fmt p

and proj fmt { path ; field } =
  Fmt.pf fmt "@[<h>%a.%s@]" module_path path field

and ident fmt id = Fmt.(option ~none:(unit "_") string) fmt (Ident.name id)

and module_path : _ -> mod_path -> unit =
  fun fmt mdt -> match mdt with
    | Path p -> path fmt p
    | Ascription (md, mty) ->
      Fmt.pf fmt "@[<2>(%a@ <:@ %a)@]" module_path md module_type mty
    | Apply (md1, md2) ->
      Fmt.pf fmt "@[<2>%a(%a)@]" module_path md1 module_path md2

and module_type fmt = function
  | Strengthen (mty, p) ->
    Fmt.pf fmt "@[%a/%a@]" module_type mty module_path p
  | Let (id, Core (Alias p), mty2) ->
    Fmt.pf fmt "@[@[<2>@[<2>let@ %a@ =@]@ %a@]@ in@ %a@]"
      ident id
      module_path p
      module_type mty2
  | Let (id, mty1, mty2) ->
    Fmt.pf fmt "@[@[<2>@[<2>let@ %a@ =@]@ %a@]@ in@ %a@]"
      ident id
      module_type mty1
      module_type mty2
  | Enrich (mty, eq) ->
    Fmt.pf fmt "@[%a@ with@ %a@]"
      module_type mty
      enrichment eq
  | Core mtyc -> mod_type_core fmt mtyc

and enrichment fmt = function
  | Module (fields, mty) ->
    Fmt.pf fmt "module@ %a@ :@ %a"
      (Fmt.list ~sep:(Fmt.unit ".") Fmt.string) fields
      module_type mty
  | Type (fields, ty) ->
    Fmt.pf fmt "type@ %a@ :@ %a"
      (Fmt.list ~sep:(Fmt.unit ".") Fmt.string) fields
      Core_printer.def_type ty

and module_self fmt id = match Ident.name id with
  | None -> ()
  | Some _ -> Fmt.pf fmt " (%a)" ident id

and mod_type_core fmt = function
  | TPath p -> path fmt p
  | Alias p -> Fmt.pf fmt "@[(= %a)@]" module_path p
  | Signature s ->
    Fmt.pf fmt "@[<hv 2>sig%a%a@;<1 -2>end@]"
      module_self s.sig_self
      signature_content s
  | Functor_type (id, param, body) ->
    Fmt.pf fmt "@[<hv2>@[(%a@ :@ %a)@ ->@]@ %a@]"
      ident id
      module_type param
      module_type body

and signature_content fmt
    { sig_self = _ ; sig_values; sig_types; sig_modules; sig_module_types } =
  Fmt.pf fmt "%t%t%t%t"
    (fields type_declaration sig_types)
    (fields module_type_declaration sig_module_types)
    (fields module_declaration sig_modules)
    (fields value_declaration sig_values)

and value_declaration fmt field ty =
  Fmt.pf fmt "@[<2>val %s =@ %a@]" field Core_printer.val_type ty
and type_declaration fmt field tydecl = 
  Fmt.pf fmt "@[<2>type %s%a@]" field type_decl tydecl
and module_type_declaration fmt field mty = 
  Fmt.pf fmt "@[<2>module %s =@ %a@]" field module_type mty

and module_declaration fmt name mty = match mty with
  | Core (Alias p) -> 
    Fmt.pf fmt "@[<2>module %s =@ %a@]"
      name
      module_path p
  | Core (Signature s) -> 
    Fmt.pf fmt "@[<hv 2>module %s : sig%a%a@;<1 -2>end@]"
      name
      module_self s.sig_self
      signature_content s
  | _ -> 
    Fmt.pf fmt "@[<2>module %s :@ %a@]"
      name
      module_type mty

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

and signature_item fmt (item : Modules.signature_item) = match item with
  | Value_sig (n, d) -> value_declaration fmt n d
  | Type_sig (n, d) -> type_declaration fmt n d
  | Module_sig (n, d) -> module_declaration fmt n d
  | Module_type_sig (n, d) -> module_type_declaration fmt n d

module Untyped = struct
  open Parsetree
  
  let rec path fmt = function
    | PathId id -> Fmt.string fmt id
    | PathProj p -> proj fmt p

  and proj fmt { path ; field } =
    Fmt.pf fmt "@[<h>%a.%s@]" module_term path field

  and module_self fmt = function
    | None -> ()
    | Some x -> Fmt.pf fmt " (%s)" x
  
  and module_term : _ -> mod_term -> unit =
    fun fmt mt -> match mt with
      | Path p -> path fmt p
      | Ascription (mt, mty) ->
        Fmt.pf fmt "@[<2>(%a@ <:@ %a)@]" module_term mt module_type mty
      | Apply (md1, md2) ->
        Fmt.pf fmt "@[<2>%a@ %a@]" module_term md1 module_term md2
      | Structure { str_self; str_content } -> 
        Fmt.pf fmt "@[<v>@[<v 2>sig%a@ %a@]@ end@]"
          module_self str_self
          (Fmt.list ~sep:Fmt.cut structure_item) str_content
      | Functor (id, param, body) ->
        Fmt.pf fmt "@[<hv2>@[(%a@ :@ %a)@ ->@]@ %a@]"
          bound_name id
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
      Fmt.pf fmt "@[<v>@[<v 2>sig%a@ %a@]@ end@]"
        module_self sig_self
        (Fmt.list ~sep:Fmt.cut signature_item) sig_content
    | Functor_type (id, param, body) ->
      Fmt.pf fmt "@[<hv2>@[(%a@ :@ %a)@ ->@]@ %a@]"
        bound_name id
        module_type param
        module_type body
    | Ascription_sig (mty1, mty2) ->
      Fmt.pf fmt "@[<hov2>(%a@ <:@ %a)@]"
        module_type mty1
        module_type mty2
  
  and signature_item fmt = function
    | Value_sig (name, ty) ->
      Fmt.pf fmt "@[<2>val %a =@ %a@]"
        bound_name name
        Core_printer.val_type ty
    | Type_sig (name, tydecl) ->
      Fmt.pf fmt "@[<2>type %a %a@]"
        bound_name name
        type_decl tydecl
    | Module_sig (name, mty) ->
      Fmt.pf fmt "@[<2>module %a :@ %a@]"
        bound_name name
        module_type mty
    | Module_type_sig (name, mty) ->
      Fmt.pf fmt "@[<2>module %a =@ %a@]"
        bound_name name
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
    | Value_str (name, t) ->
      Fmt.pf fmt "@[<2>let %a =@ %a@]"
        bound_name name
        Core_printer.term t
    | Type_str (name, tydecl) ->
      Fmt.pf fmt "@[<2>type %a %a@]"
        bound_name name
        type_decl tydecl
    | Module_str (name, mty) ->
      Fmt.pf fmt "@[<2>module %a :@ %a@]"
        bound_name name
        module_term mty
    | Module_type_str (name, mty) ->
      Fmt.pf fmt "@[<2>module %a =@ %a@]"
        bound_name name
        module_type mty

end
