
val path : Modules.path Fmt.t
val module_path : Modules.mod_path Fmt.t

val module_type : Modules.mod_type Fmt.t

val signature_item : Modules.signature_item Fmt.t
val unit : Modules.signature_item list Fmt.t

module Untyped : sig
  val path : Parsetree.path Fmt.t
  val module_term : Parsetree.mod_term Fmt.t
end
