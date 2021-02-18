open Types

val path : Modules.path Fmt.t
val module_path : Modules.mod_path Fmt.t

val def_type : Core.def_type Fmt.t
val val_type : Core.val_type Fmt.t

val enrichment : Modules.enrichment Fmt.t
val module_type : Modules.mod_type Fmt.t

val signature_item : Modules.signature_item Fmt.t

module Untyped : sig
  val path : Parsetree.Modules.path Fmt.t
  val module_term : Parsetree.Modules.mod_term Fmt.t
end
