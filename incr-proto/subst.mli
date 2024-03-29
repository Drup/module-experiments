open Types

type t

val identity: t

(* val add_type: Ident.t -> qid -> t -> t
 * val add_module: Ident.t -> qid -> t -> t
 * val add_module_type: Ident.t -> qid -> t -> t *)

val add_type: Ident.t -> path -> t -> t
val add_module: Ident.t -> Modules.mod_path -> t -> t

val type_path: t -> Modules.path -> Modules.path
val val_type : t -> Core.val_type -> Core.val_type
val type_decl : t -> Modules.type_decl -> Modules.type_decl
val mod_type : t -> Modules.mod_type -> Modules.mod_type
val mod_type_core : t -> Modules.mod_type_core -> Modules.mod_type_core
val signature : t -> Modules.signature -> Modules.signature
