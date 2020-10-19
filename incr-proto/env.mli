
type t

exception Not_found

val empty: t

val add_value: Ident.t -> Types.val_type -> t -> t
val add_type: Ident.t -> Modules.type_decl -> t -> t
val add_module: Ident.t -> Modules.mod_type -> t -> t
val add_module_type: Ident.t -> Modules.mod_type -> t -> t

val fold_with: 
  Ident.t ->
  (t -> 'a -> Modules.signature_item) ->
  t -> 'a list -> Modules.signature

val lookup_value: t -> Modules.path -> Types.val_type
val lookup_type: t -> Modules.path -> Modules.type_decl
val lookup_module: t -> Modules.mod_path -> Modules.mod_type
val lookup_module_type: t -> Modules.path -> Modules.mod_type


val compute_signature :
  (t -> Modules.mod_type -> Modules.signature) ref
val compute_ascription :
  (t -> Modules.mod_type -> Modules.mod_type -> Modules.mod_type) ref
val compute_functor_app :
  (t -> f:Modules.mod_type -> arg:Modules.mod_path -> Modules.mod_type) ref
