open Types

module Subst : sig
  type t = {
    (* types : Modules.path Ident.Map.t ; *)
    modules : Modules.mod_path Ident.Map.t ;
    (* module_types : Modules.path Ident.Map.t ; *)
  }

  val val_type: t -> val_type -> val_type
  val def_type: t -> def_type -> def_type
end

module Typing : sig
  val term: Env.t -> Types.term -> Types.val_type
  val def_type: Env.t -> Types.def_type -> unit
  val val_type: Env.t -> Types.val_type -> unit
end

module Include : sig
  val val_type: Env.t -> Types.val_type -> Types.val_type -> unit
  val def_type: Env.t -> Types.def_type -> Types.def_type -> unit
end
