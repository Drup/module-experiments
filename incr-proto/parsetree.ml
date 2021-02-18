type name = string
type field = string
type bound_name = string option

module rec Core : sig
  (* The Unit core language, with:
     - 1 value: `Unit`
     - 1 type: `Unit`
     - 1 rule: `Unit : Unit`
  *)

  type term =
    | Unit
    | Variable of Modules.path

  type val_type =
    | Type of Modules.path

  type def_type =
    | Unit
end = Core

and Modules : sig

  type type_declaration =
    | Abstract
    | TypeAlias of Core.val_type
    | Definition of {
        manifest : path option ;
        definition : Core.def_type ;
      }
  (** A type declaration : abstract, alias or definition *)

  and mod_type =
    | TPath of path
    (** X | P.X *)
    | Alias of mod_path
    (** (= P)
        (= P <: mty *)
    | Signature of signature
    (** sig ... end *)
    | Functor_type of bound_name * mod_type * mod_type
    (** functor(X: mty) mty *)
    | Let of name * mod_type * mod_type
    (** let X : mty in mty *)
    | Enrich of mod_type * enrichment
    (** (mty with C) *)
    | Ascription_sig of mod_type * mod_type
    (** (mty <: mty) *)

  and enrichment =
    | Module of field list * mod_type (** X.Y.Z : M *)
    | Type of field list * Core.val_type (** X.Y.Z.t = τ *)

  and signature = {
    sig_self : bound_name ;
    sig_content : signature_item list ;
  }
  and signature_item =
    | Value_sig of bound_name * Core.val_type
    (** val x: ty *)
    | Type_sig of bound_name * type_declaration
    (** type t :: k [= ty] *)
    | Module_sig of bound_name * mod_type
    (** module X: mty *)
    | Module_type_sig of bound_name * mod_type
    (** module type X = mty *)

  and mod_path = mod_term

  and path =
    | PathId of string
    | PathProj of proj

  and proj = {
    path : mod_path ;
    field : field ;
  }

  and mod_term =
    | Path : path -> mod_term
    (** X, P.X *)
    | Ascription : mod_term * mod_type -> mod_term
    (** (mod <: mty) *)
    | Apply : mod_term * mod_term -> mod_term
    (** mod1(mod2) *)
    | Structure : structure -> mod_term
    (** struct ... end *)
    | Functor : bound_name * mod_type * mod_term -> mod_term
    (** functor (X: mty) mod *)
    | Constraint : mod_term * mod_type -> mod_term
    (** (mod : mty) *)

  and structure = {
    str_self : bound_name ;
    str_content : structure_item list ;
  }
  and structure_item =
      Value_str of bound_name * Core.term
    (** let x = expr *)
    | Type_str of bound_name * type_declaration
    (** type t :: k = ty *)
    | Module_str of bound_name * mod_term
    (** module X = mod *)
    | Module_type_str of bound_name * mod_type
    (** module type X = mty *)
end = Modules

type path = Modules.path
