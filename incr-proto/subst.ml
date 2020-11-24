open Modules

type t = Core_subst.t = {
  (* types : Modules.path Ident.Map.t ; *)
  modules : Modules.mod_path Ident.Map.t ;
  (* module_types : Modules.path Ident.Map.t ; *)
}

let identity : t = {
  (* types = Ident.Map.empty ; *)
  modules = Ident.Map.empty ;
  (* module_types = Ident.Map.empty ; *)
}

(* let add_type p v sub =
 *   {sub with types = Ident.Map.add p v sub.types } *)
let add_module p v sub =
  (* {sub with modules = Ident.Map.add p v sub.modules } *)
  { modules = Ident.Map.add p v sub.modules }
(* let add_module_type p v sub =
 *   {sub with module_types = Ident.Map.add p v sub.module_types } *)

let val_type sub (decl : Core_types.val_type) = Core_subst.val_type sub decl

let rec type_decl sub (decl : Modules.type_decl) : Modules.type_decl =
  { definition = (match decl.definition with
        | None -> None
        | Some dty -> Some (Core_subst.def_type sub dty)) ;
    manifest = match decl.manifest with
        None -> None
      | Some p -> Some (path sub p)
  }

and path sub {Modules. path ; field } =
  {path = mod_path sub path ; field }

and mod_path sub (p : mod_path) =
  match p with
  | Id id ->
    begin
      try Ident.Map.lookup id sub.modules with
      | Not_found -> Id id
    end
  | Proj {path; field} ->
    Proj {path = mod_path sub path; field}
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
  (* | Alias (p, Some mty) ->
   *   Alias (mod_path sub p, Some (mod_type sub mty)) *)
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
