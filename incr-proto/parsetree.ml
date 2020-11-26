type name = string
type field = string

type type_decl = {
  manifest : path option ;
  definition : Core_types.def_type option ;
}
(** abstract or manifest *)

and mod_type =
  | TPath of path
  (** X | P.X *)
  | Alias of mod_path
  (** (= P)
      (= P <: mty *)
  | Signature of signature
  (** sig ... end *)
  | Functor_type of name * mod_type * mod_type
  (** functor(X: mty) mty *)
  | Let of name * mod_type * mod_type
  (** let X : mty in mty *)
  | Enrich of mod_type * enrichment
  (** (mty with C) *)
  | Ascription_sig of mod_type * mod_type
  (** (mty <: mty) *)

and enrichment =
  | Module of field list * mod_type (** X.Y.Z : M *)
  | Type of field list * Core_types.def_type (** X.Y.Z.t = τ *)

and signature = {
  sig_self : name ;
  sig_content : signature_item list ;
}
and signature_item =
  | Value_sig of field * Core_types.val_type
  (** val x: ty *)
  | Type_sig of field * type_decl
  (** type t :: k [= ty] *)
  | Module_sig of field * mod_type
  (** module X: mty *)
  | Module_type_sig of field * mod_type
  (** module type X = mty *)

and mod_path = mod_term

and path = {
  path : mod_path ;
  field : field ;
}

and mod_term =
  | Id : string -> mod_term
  (** X *)
  | Proj : path -> mod_term
  (** P.X *)
  | Ascription : mod_term * mod_type -> mod_term
  (** (mod <: mty) *)
  | Apply : mod_term * mod_term -> mod_term
  (** mod1(mod2) *)
  | Structure : structure -> mod_term
  (** struct ... end *)
  | Functor : string * mod_type * mod_term -> mod_term
  (** functor (X: mty) mod *)
  | Constraint : mod_term * mod_type -> mod_term
  (** (mod : mty) *)

and structure = {
  str_self : name ;
  str_content : structure_item list ;
}
and structure_item =
    Value_str of field * Core_types.term
  (** let x = expr *)
  | Type_str of field * type_decl
  (** type t :: k = ty *)
  | Module_str of field * mod_term
  (** module X = mod *)
  | Module_type_str of field * mod_type
  (** module type X = mty *)
