open Types
open Modules

open Types.Core

type t = {
  types : Types.path Ident.Map.t ;
  modules : Types.Modules.mod_path Ident.Map.t ;
  (* module_types : Modules.path Ident.Map.t ; *)
}

let identity : t = {
  types = Ident.Map.empty ;
  modules = Ident.Map.empty ;
  (* module_types = Ident.Map.empty ; *)
}


let add_type p v sub =
  match v with
  | PathId p' when Ident.equal p p' -> sub
  | _ -> { sub with types = Ident.Map.add p v sub.types }

let add_module p v sub =
  match v with
  | Path (PathId p') when Ident.equal p p' -> sub
  | _ -> { sub with modules = Ident.Map.add p v sub.modules }

let rec val_type
  : t -> val_type -> val_type
  = fun sub -> function
    | Type p -> Type (type_path sub p)

and def_type
  : t -> def_type -> def_type
  = fun env -> function
    | Unit -> Unit

and type_decl sub (decl : Modules.type_decl) : Modules.type_decl =
  match decl with
  | Abstract -> Abstract
  | TypeAlias p -> TypeAlias (val_type sub p)
  | Definition { manifest; definition } ->
    let definition = def_type sub definition in
    let manifest = Option.map (type_path sub) manifest in
    Definition { manifest; definition }

and proj sub {Modules. path ; field } =
  {path = mod_path sub path ; field }
and type_path sub p = match p with
  | PathId id ->
    begin
      try Ident.Map.lookup id sub.types with
      | Not_found -> PathId id
    end
  | PathProj p -> PathProj (proj sub p)

and mod_path sub (p : mod_path) =
  match p with
  | Path (PathId id) ->
    begin
      try Ident.Map.lookup id sub.modules with
      | Not_found -> Path (PathId id)
    end
  | Path (PathProj pr) ->
    Path (PathProj (proj sub pr))
  | Apply (p1, p2) ->
    Apply (mod_path sub p1, mod_path sub p2)
  | Ascription (p, mty) ->
    Ascription (mod_path sub p, mod_type sub mty)

and mod_type sub mty =
  Ident.Map.fold
    (fun id p expr -> Let (id, Core (Alias p), expr))
    sub.modules mty

and mod_type_core sub mty =
  match mty with
  | TPath p -> TPath p
  | Alias p ->
    Alias (mod_path sub p)
  | Signature s ->
    Signature (signature sub s)
  | Functor_type(id, mty1, mty2) ->
    Functor_type(id, mod_type sub mty1, mod_type sub mty2)

and signature sub
    { sig_self; sig_values; sig_types; sig_modules; sig_module_types }
  = {
    sig_self ;
    sig_values = FieldMap.map (val_type sub) sig_values ;
    sig_types = FieldMap.map (type_decl sub) sig_types ;
    sig_modules = FieldMap.map (mod_type sub) sig_modules ;
    sig_module_types = FieldMap.map (mod_type sub) sig_module_types ;
  }
