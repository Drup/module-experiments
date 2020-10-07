type pure = Pure
type impure = Impure

type field = string

type type_decl = {
  manifest : path option ;
  definition : Types.def_type option ;
}
(** abstract or manifest *)

and mod_type_core =
  | TPath of path
  (** X | P.X *)
  | Alias of mod_path
  (** (= P)
      (= P <: mty *)
  | Signature of signature
  (** sig ... end *)
  | Functor_type of Ident.t * mod_type * mod_type
  (** functor(X: mty) mty *)
and mod_type_op =
  | Strengthen of mod_type * mod_path
  (** mty/P *)
  | Let of Ident.t * mod_type_core * mod_type
  (** let X : mty in mty *)
  | Enrich of mod_type * enrichment
  (** (mty with C) *)
  | Core of mod_type_core
and mod_type = mod_type_op
  (* | Unresolved of mod_type_op
   * | Resolved of mod_type_core * mod_type_op  *)

and enrichment =
  | Module of mod_path * mod_path (** P = P *)
  | Type of path * Types.def_type (** P.t = τ *)

and signature = {
  sig_self : Ident.t ;
  sig_content : signature_item list ;
}
and signature_item =
  | Value_sig of field * Types.val_type
  (** val x: ty *)
  | Type_sig of field * type_decl
  (** type t :: k [= ty] *)
  | Module_sig of field * mod_type
  (** module X: mty *)
  | Module_type_sig of field * mod_type
  (** module type X = mty *)

and mod_path = pure mod_term

and path = {
  path : mod_path ;
  field : field ;
}

and _ mod_term =
  | Id : Ident.t -> pure mod_term
  (** X *)
  | Proj : mod_path * field -> pure mod_term
  (** P.X *)
  | Ascription : 'a mod_term * mod_type -> 'a mod_term
  (** (mod <: mty) *)
  | Apply : 'a mod_term * 'a mod_term -> 'a mod_term
  (** mod1(mod2) *)
  | Structure : structure -> impure mod_term
  (** struct ... end *)
  | Functor : Ident.t * mod_type * _ mod_term -> impure mod_term
  (** functor (X: mty) mod *)
  | Constraint : _ mod_term * mod_type -> impure mod_term
  (** (mod : mty) *)

and structure = {
  str_self : Ident.t ;
  str_content : structure_item list ;
}
and structure_item =
    Value_str of field * Types.term
  (** let x = expr *)
  | Type_str of field * type_decl
  (** type t :: k = ty *)
  | Module_str of field * impure mod_term
  (** module X = mod *)
  | Module_type_str of field * mod_type
  (** module type X = mty *)
