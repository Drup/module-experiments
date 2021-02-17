module FieldMap = CCMap.Make(String)

type pure = Pure
type impure = Impure

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

  type val_type = Unit

  type def_type =
    | Alias of val_type

end = Core

and Modules : sig

  type type_decl = {
    manifest : path option ;
    definition : Core.def_type option ;
  }
  (** abstract or manifest *)

  and mod_type_core =
    | TPath of path
    (** X | P.X *)
    | Alias of mod_path
    (** (= P) *)
    | Signature of signature
    (** sig ... end *)
    | Functor_type of Ident.t * mod_type * mod_type
    (** functor(X: mty) mty *)
  and mod_type_op =
    | Strengthen of mod_type * mod_path
    (** mty/P *)
    | Let of Ident.t * mod_type * mod_type
    (** let X : mty in mty *)
    | Enrich of mod_type * enrichment
    (** (mty with C) *)
    | Core of mod_type_core
  and mod_type = mod_type_op
  (* | Unresolved of mod_type_op
   * | Resolved of mod_type_core * mod_type_op  *)

  and enrichment =
    | Module of field list * mod_type (** X.Y.Z : M *)
    | Type of field list * Core.def_type (** X.Y.Z.t = τ *)

  and signature = {
    sig_self : Ident.t ;
    sig_values : Core.val_type FieldMap.t ;
    sig_types : type_decl FieldMap.t ;
    sig_modules : mod_type FieldMap.t ;
    sig_module_types : mod_type FieldMap.t ;
  }

  and signature_item =
    | Value_sig of field * Core.val_type
    (** val x: ty *)
    | Type_sig of field * type_decl
    (** type t :: k [= ty] *)
    | Module_sig of field * mod_type
    (** module X: mty *)
    | Module_type_sig of field * mod_type
    (** module type X = mty *)

  and mod_path = pure mod_term

  and path =
    | PathId of Ident.t
    | PathProj of proj

  and proj = { path : mod_path ; field : field }

  and _ mod_term =
    | Path : path -> pure mod_term
    (** X, P.X *)
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
    | Value_str of bound_name * Core.term
    (** let x = expr *)
    | Type_str of bound_name * type_decl
    (** type t :: k = ty *)
    | Module_str of bound_name * impure mod_term
    (** module X = mod *)
    | Module_type_str of bound_name * mod_type
    (** module type X = mty *)
end = Modules

type path = Modules.path
