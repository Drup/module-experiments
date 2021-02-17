open Types

type t

val empty: t

val add_value: Ident.t -> Types.Core.val_type -> t -> t
val add_type: Ident.t -> Modules.type_decl -> t -> t
val add_module: Ident.t -> Modules.mod_type -> t -> t
val add_module_type: Ident.t -> Modules.mod_type -> t -> t

val intro_value: Parsetree.bound_name -> Types.Core.val_type -> t -> Ident.t * t
val intro_type: Parsetree.bound_name -> Modules.type_decl -> t -> Ident.t * t
val intro_module: Parsetree.bound_name -> Modules.mod_type -> t -> Ident.t * t
val intro_module_type: Parsetree.bound_name -> Modules.mod_type -> t -> Ident.t * t

val intro_item : Modules.signature_item -> t -> Ident.t * t

val fold_with: 
  Parsetree.bound_name ->
  (t -> 'a -> Modules.signature_item option) ->
  t -> 'a list ->
  Modules.signature

val lookup_value: t -> path -> Types.Core.val_type option
val lookup_type: t -> path -> Modules.type_decl option
val lookup_module: t -> Modules.mod_path -> Modules.mod_type option
val lookup_module_type: t -> path -> Modules.mod_type option

val find_value : t -> Parsetree.path -> (path * Types.Core.val_type) option
val find_type : t -> Parsetree.path -> (path * Modules.type_decl) option
val find_module : t -> Parsetree.name -> (Ident.t * Modules.mod_type) option
val find_module_type : t -> Parsetree.path -> (path * Modules.mod_type) option

val compute_signature :
  (t -> Modules.mod_type -> Modules.signature) ref
val compute_ascription :
  (t -> Modules.mod_type -> Modules.mod_type -> Modules.mod_type) ref
val compute_functor_app :
  (t -> f:Modules.mod_type -> arg:Modules.mod_path -> Modules.mod_type) ref
val transl_mod_path :
  (t -> Parsetree.Modules.mod_path -> Types.Modules.mod_path) ref
