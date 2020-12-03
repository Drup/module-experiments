
type t

val empty: t

val add_value: Ident.t -> Core_types.val_type -> t -> t
val add_type: Ident.t -> Modules.type_decl -> t -> t
val add_module: Ident.t -> Modules.mod_type -> t -> t
val add_module_type: Ident.t -> Modules.mod_type -> t -> t

val intro_value: Parsetree.name -> Core_types.val_type -> t -> Ident.t * t
val intro_type: Parsetree.name -> Modules.type_decl -> t -> Ident.t * t
val intro_module: Parsetree.name -> Modules.mod_type -> t -> Ident.t * t
val intro_module_type: Parsetree.name -> Modules.mod_type -> t -> Ident.t * t

val intro_item : Modules.signature_item -> t -> Ident.t * t

val fold_with: 
  Parsetree.name ->
  (t -> 'a -> Modules.signature_item) ->
  t -> 'a list ->
  Modules.signature

val lookup_value: t -> Modules.path -> Core_types.val_type option
val lookup_type: t -> Modules.path -> Modules.type_decl option
val lookup_module: t -> Modules.mod_path -> Modules.mod_type option
val lookup_module_type: t -> Modules.path -> Modules.mod_type option

val find_module : t -> Parsetree.name -> Ident.t option
val find_value : t -> Parsetree.name -> Ident.t option
val find_type : t -> Parsetree.name -> Ident.t option
val find_module_type : t -> Parsetree.name -> Ident.t option

val compute_signature :
  (t -> Modules.mod_type -> Modules.signature) ref
val compute_ascription :
  (t -> Modules.mod_type -> Modules.mod_type -> Modules.mod_type) ref
val compute_functor_app :
  (t -> f:Modules.mod_type -> arg:Modules.mod_path -> Modules.mod_type) ref
