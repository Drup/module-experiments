
let unit = Ident.create @@ Some "unit"
let unit_ty : Types.Core.val_type = Type (PathId unit)
let unit_def : Types.Modules.type_decl =
  Definition { manifest = None ; definition = Unit }

let v =
  Env.empty
  |> Env.add_type unit unit_def
    
    
