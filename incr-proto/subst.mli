
type t = Core.Subst.t

val identity: t

(* val add_type: Ident.t -> qid -> t -> t
 * val add_module: Ident.t -> qid -> t -> t
 * val add_module_type: Ident.t -> qid -> t -> t *)

val add_module: Ident.t -> Modules.mod_path -> t -> t

val path: t -> Modules.path -> Modules.path
val type_decl : t -> Modules.type_decl -> Modules.type_decl
val mod_type : t -> Modules.mod_type -> Modules.mod_type
val signature_item : t -> Modules.signature_item -> Modules.signature_item
val mod_type_core : t -> Modules.mod_type_core -> Modules.mod_type_core
