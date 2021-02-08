(* The parser *)

%{

open Input.Parse
open Parsetree
   
let make_loc (startpos, endpos) = Location.Location (startpos, endpos)

type error +=
  | Unclosed of Location.loc * string * Location.loc * string
  | Expecting of Location.loc * string
              
let unclosed opening_name opening_loc closing_name closing_loc =
  raise(Error(Unclosed(make_loc opening_loc, opening_name,
                       make_loc closing_loc, closing_name)))

let expecting loc nonterm =
    raise(Error(Expecting(make_loc loc, nonterm)))

(* Error report *)

let prepare_error = function
  | Error (Unclosed (opening_loc, _opening, closing_loc, closing)) ->
     Some (Report.errorf
        ~loc:closing_loc
        ~sub:[opening_loc]
        "Syntax error: '%s' expected" closing)
  | Error (Expecting (loc, nonterm)) ->
      Some (Report.errorf ~loc "Syntax error: %s expected." nonterm)
  | _ -> None

let () =
  Report.register_report_of_exn prepare_error

  
%}

/* Tokens */
(* %token AND
 * %token QUOTE *)
%token COLON
%token LESSERCOLON
(* %token COMMA *)
%token DOT
%token END
%token EOF
%token EQUAL
%token FUNCTOR
(* %token IN
 * %token INCLUDE *)
%token LET
%token <string> LIDENT
%token LPAREN
%token MINUSGREATER
%token MODULE
(* %token OPEN *)
%token RPAREN
%token SEMISEMI
%token SIG
%token STRUCT
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token VAL
%token WITH
%token <string> EXPECT

%left DOT
%nonassoc below_WITH
%left WITH
(* %nonassoc AND *)
%right    MINUSGREATER
%nonassoc LPAREN


/* Entry points */

%start implementation                   /* for implementation files */
%type <Parsetree.structure_item list> implementation
%start interface                        /* for interface files */
%type <Parsetree.signature_item list> interface
%start toplevel_phrase                  /* for interactive use */
%type <Parsetree.structure_item list> toplevel_phrase
%start use_file                         /* for the #use directive */
%type <Parsetree.structure_item list> use_file
%start expect_file
%type <(Parsetree.structure_item list * int * int) list> expect_file

%type <Parsetree.mod_term> mod_ext_longident

%%

/* Generic definitions */

(* [reversed_nonempty_llist(X)] recognizes a nonempty list of [X]s, and produces
   an OCaml list in reverse order -- that is, the last element in the input text
   appears first in this list. Its definition is left-recursive. *)

reversed_nonempty_llist(X):
  x = X
    { [ x ] }
| xs = reversed_nonempty_llist(X) x = X
    { x :: xs }

(* [reversed_separated_nonempty_llist(separator, X)] recognizes a nonempty list
   of [X]s, separated with [separator]s, and produces an OCaml list in reverse
   order -- that is, the last element in the input text appears first in this
   list. Its definition is left-recursive. *)

(* [inline_reversed_separated_nonempty_llist(separator, X)] is semantically
   equivalent to [reversed_separated_nonempty_llist(separator, X)], but is
   marked %inline, which means that the case of a list of length one and
   the case of a list of length more than one will be distinguished at the
   use site, and will give rise there to two productions. This can be used
   to avoid certain conflicts. *)

%inline inline_reversed_separated_nonempty_llist(separator, X):
  x = X
    { [ x ] }
| xs = reversed_separated_nonempty_llist(separator, X)
  separator
  x = X
    { x :: xs }

reversed_separated_nonempty_llist(separator, X):
  xs = inline_reversed_separated_nonempty_llist(separator, X)
    { xs }

(* [separated_nonempty_llist(separator, X)] recognizes a nonempty list of [X]s,
   separated with [separator]s, and produces an OCaml list in direct order --
   that is, the first element in the input text appears first in this list. *)

%inline separated_nonempty_llist(separator, X):
  xs = rev(reversed_separated_nonempty_llist(separator, X))
    { xs }

%inline inline_separated_nonempty_llist(separator, X):
  xs = rev(inline_reversed_separated_nonempty_llist(separator, X))
    { xs }

(* [reversed_separated_nontrivial_llist(separator, X)] recognizes a list of at
   least two [X]s, separated with [separator]s, and produces an OCaml list in
   reverse order -- that is, the last element in the input text appears first
   in this list. Its definition is left-recursive. *)

reversed_separated_nontrivial_llist(separator, X):
  xs = reversed_separated_nontrivial_llist(separator, X)
  separator
  x = X
    { x :: xs }
| x1 = X
  separator
  x2 = X
    { [ x2; x1 ] }

(* [separated_nontrivial_llist(separator, X)] recognizes a list of at least
   two [X]s, separated with [separator]s, and produces an OCaml list in direct
   order -- that is, the first element in the input text appears first in this
   list. *)

%inline separated_nontrivial_llist(separator, X):
  xs = rev(reversed_separated_nontrivial_llist(separator, X))
    { xs }

(* [separated_or_terminated_nonempty_list(delimiter, X)] recognizes a nonempty
   list of [X]s, separated with [delimiter]s, and optionally terminated with a
   final [delimiter]. Its definition is right-recursive. *)

separated_or_terminated_nonempty_list(delimiter, X):
  x = X ioption(delimiter)
    { [x] }
| x = X
  delimiter
  xs = separated_or_terminated_nonempty_list(delimiter, X)
    { x :: xs }

(* [reversed_preceded_or_separated_nonempty_llist(delimiter, X)] recognizes a
   nonempty list of [X]s, separated with [delimiter]s, and optionally preceded
   with a leading [delimiter]. It produces an OCaml list in reverse order. Its
   definition is left-recursive. *)

reversed_preceded_or_separated_nonempty_llist(delimiter, X):
  ioption(delimiter) x = X
    { [x] }
| xs = reversed_preceded_or_separated_nonempty_llist(delimiter, X)
  delimiter
  x = X
    { x :: xs }

(* [preceded_or_separated_nonempty_llist(delimiter, X)] recognizes a nonempty
   list of [X]s, separated with [delimiter]s, and optionally preceded with a
   leading [delimiter]. It produces an OCaml list in direct order. *)

%inline preceded_or_separated_nonempty_llist(delimiter, X):
  xs = rev(reversed_preceded_or_separated_nonempty_llist(delimiter, X))
    { xs }

(* [xlist(A, B)] recognizes [AB*]. We assume that the semantic value for [A]
   is a pair [x, b], while the semantic value for [B*] is a list [bs].
   We return the pair [x, b :: bs]. *)

%inline xlist(A, B):
  a = A bs = B*
    { let (x, b) = a in x, b :: bs }

(* [listx(delimiter, X, Y)] recognizes a nonempty list of [X]s, optionally
   followed with a [Y], separated-or-terminated with [delimiter]s. The
   semantic value is a pair of a list of [X]s and an optional [Y]. *)

listx(delimiter, X, Y):
| x = X ioption(delimiter)
    { [x], None }
| x = X delimiter y = Y delimiter?
    { [x], Some y }
| x = X
  delimiter
  tail = listx(delimiter, X, Y)
    { let xs, y = tail in
      x :: xs, y }

(* -------------------------------------------------------------------------- *)

(* Entry points. *)

(* An .ml file. *)
implementation:
  structure_item* EOF
    { $1 }
;

(* An .mli file. *)
interface:
  signature_item* EOF
    { $1 }
;

(* A toplevel phrase. *)
toplevel_phrase:
  (* (\* An expression with attributes, ended by a double semicolon. *\)
   * expr
   * SEMISEMI
   *   { Ptop_def $1 } *)
| (* A list of structure items, ended by a double semicolon. *)
  structure_item* SEMISEMI
    { $1 }
| (* End of input. *)
  EOF
    { raise End_of_file }
;

(* An .ml file that is read by #use. *)
use_file:
  (* An optional standalone expression,
     followed with a series of elements,
     followed with EOF. *)
  flatten(use_file_element*)
  EOF
    { $1 }
;

(* An element in a #used file is one of the following:
   - a double semicolon followed with an optional standalone expression;
   - a structure item;
   - a toplevel directive.
 *)
%inline use_file_element:
 | structure_item
      { [$1] }
;

expect_file:
  expect_item* EOF
    { $1 }
;

%inline expect_item:
  | l = structure_item+ EXPECT
    { l, $endofs(l), $endofs($2) }
;

(* -------------------------------------------------------------------------- *)

(* Functor arguments appear in module expressions and module types. *)

%inline functor_args:
  reversed_nonempty_llist(functor_arg)
    { $1 }
    (* Produce a reversed list on purpose;
       later processed using [fold_left]. *)
;

functor_arg:
    LPAREN x = module_name COLON mty = module_type RPAREN
      { (x, mty) }
;

module_name:
  | x = opt_ident(UIDENT) { x }
;

(* -------------------------------------------------------------------------- *)

(* Module expressions. *)

module_expr:
  | STRUCT s = structure END
      { Structure s }
  | STRUCT structure error
      { unclosed "struct" $loc($1) "end" $loc($3) }
  | FUNCTOR args = functor_args MINUSGREATER me = module_expr
      { List.fold_left (fun acc (id, param) ->
          Functor (id, param, acc)
        ) me args
      }
  | UIDENT            { Path (PathId $1) }
  | module_expr DOT UIDENT { Path (PathProj{ path = $1 ; field = $3 }) }
  | me = paren_module_expr
      { me }
  | me1 = module_expr me2 = paren_module_expr
      { Apply(me1, me2) }
;

paren_module_expr:
    LPAREN me = module_expr COLON mty = module_type RPAREN
      { Constraint(me, mty) }
  | LPAREN me = module_expr LESSERCOLON mty = module_type RPAREN
      { Ascription(me, mty) }
  | LPAREN module_expr COLON module_type error
      { unclosed "(" $loc($1) ")" $loc($5) }
  | LPAREN me = module_expr RPAREN
      { me }
  | LPAREN module_expr error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | LPAREN VAL expr COLON error
      { unclosed "(" $loc($1) ")" $loc($5) }
  | LPAREN VAL expr error
      { unclosed "(" $loc($1) ")" $loc($4) }
;

structure:
  name = option(LPAREN name = opt_ident(UIDENT) RPAREN {name})
  l = structure_item*
  { { str_self = CCOpt.flatten name ; str_content = l } }
;

(* A structure item. *)
structure_item:
  | let_binding
      { let name, decl = $1 in Value_str (name, decl) }
  | type_declaration
      { let name, decl = $1 in Type_str (name, decl) }
  | module_binding
      { let name, decl = $1 in Module_str (name, decl) }
  | module_type_declaration
      { let name, decl = $1 in Module_type_str (name, decl) }
  (* | open_declaration
   *     { let (body, ext) = $1 in (Pstr_open body, ext) }
   * | include_statement(module_expr)
   *     { pstr_include $1 } *)
;

(* A single module binding. *)
%inline module_binding:
  MODULE
  name = module_name
  body = module_binding_body
    { name, body }
;

(* The body (right-hand side) of a module binding. *)
module_binding_body:
    EQUAL me = module_expr
      { me }
  | COLON mty = module_type EQUAL me = module_expr
      { Constraint(me, mty) }
  | arg = functor_arg body = module_binding_body
      { let name, mty = arg in Functor(name, mty, body) }
;

(* -------------------------------------------------------------------------- *)

(* Shared material between structures and signatures. *)

(* (\* An [include] statement can appear in a structure or in a signature,
 *    which is why this definition is parameterized. *\)
 * %inline include_statement(thing):
 *   INCLUDE
 *   ext = ext
 *   thing = thing
 *   {
 *     let loc = make_loc $sloc in
 *     let docs = symbol_docs $sloc in
 *     Incl.mk thing ~loc ~docs, ext
 *   }
 * ; *)

(* A module type declaration. *)
module_type_declaration:
  MODULE TYPE
  id = opt_ident(ident)
  EQUAL
  mtyp = module_type
  { id, mtyp }
;

(* -------------------------------------------------------------------------- *)

(* (\* Opens. *\)
 * 
 * open_declaration:
 *   OPEN
 *   override = override_flag
 *   ext = ext
 *   attrs1 = attributes
 *   me = module_expr
 *   attrs2 = post_item_attributes
 *   {
 *     let attrs = attrs1 @ attrs2 in
 *     let loc = make_loc $sloc in
 *     let docs = symbol_docs $sloc in
 *     Opn.mk me ~override ~attrs ~loc ~docs, ext
 *   }
 * ;
 * 
 * open_description:
 *   OPEN
 *   override = override_flag
 *   ext = ext
 *   attrs1 = attributes
 *   id = mkrhs(mod_ext_longident)
 *   attrs2 = post_item_attributes
 *   {
 *     let attrs = attrs1 @ attrs2 in
 *     let loc = make_loc $sloc in
 *     let docs = symbol_docs $sloc in
 *     Opn.mk id ~override ~attrs ~loc ~docs, ext
 *   }
 * ;
 * 
 * %inline open_dot_declaration: mkrhs(mod_longident)
 *   { let loc = make_loc $loc($1) in
 *     let me = Mod.ident ~loc $1 in
 *     Opn.mk ~loc me }
 * ; *)

(* -------------------------------------------------------------------------- *)

/* Module types */

module_type:
  | SIG s = signature END
      { Signature s }
  | SIG signature error
      { unclosed "sig" $loc($1) "end" $loc($3) }
  | FUNCTOR args = functor_args
    MINUSGREATER mty = module_type
      { List.fold_left
          (fun acc (id,mty) -> Functor_type (id, mty, acc))
          mty args
      }
  | LPAREN module_type RPAREN
    { $2 }
  | mty_longident
    { TPath $1 }
  | m = module_type WITH cstrs = with_constraint
    { Enrich(m, cstrs) }
  | LPAREN EQUAL m = mod_ext_longident RPAREN
    { Alias m }
  | LPAREN mty1 = module_type LESSERCOLON mty2 = module_type RPAREN
    { Ascription_sig (mty1, mty2) }
  | LPAREN module_type error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | LPAREN EQUAL mod_ext_longident error
      { unclosed "(" $loc($1) ")" $loc($4) }
  | LPAREN module_type LESSERCOLON module_type error
      { unclosed "(" $loc($1) ")" $loc($5) }
;

(* A signature, which appears between SIG and END (among other places),
   is a list of signature elements. *)
signature:
  name = option(LPAREN name = opt_ident(UIDENT) RPAREN {name})
  l = flatten(signature_element*)
    { { sig_self = CCOpt.flatten name ; sig_content = l } }
;

(* A signature element is one of the following:
   - a double semicolon;
   - a signature item. *)
%inline signature_element:
    SEMISEMI { [] }
  | signature_item { [$1] }
;

(* A signature item. *)
signature_item:
  | value_description
      { let name, body = $1 in Value_sig (name, body) }
  | type_declaration
      { let name, body = $1 in Type_sig (name, body) }
  | module_declaration
      { let name, body = $1 in Module_sig (name, body) }
  | module_alias
      { let name, body = $1 in Module_sig (name, body) }
  | module_type_declaration
      { let name, body = $1 in Module_type_sig (name, body) }
  (* | open_description
   *     { let (body, ext) = $1 in (Psig_open body, ext) } *)
  (* | include_statement(module_type)
   *     { psig_include $1 } *)
;  

(* A module declaration. *)
%inline module_declaration:
  MODULE
  name = module_name
  body = module_declaration_body
  { name, body }
;

(* The body (right-hand side) of a module declaration. *)
module_declaration_body:
    COLON mty = module_type
      { mty }
  | arg = functor_arg body = module_declaration_body
      { let param, ty = arg in Functor_type(param, ty, body) }
;

(* A module alias declaration (in a signature). *)
%inline module_alias:
  MODULE
  name = module_name
  EQUAL
  body = module_expr_alias
  { name, body }
;
%inline module_expr_alias:
  id = mod_longident
    { Alias id }
;

(* Core expressions *)

expr:
    LPAREN RPAREN { Core_types.Unit }
;

/* Value descriptions */

let_binding_body:
    id = let_ident EQUAL e = expr
      { (id, e) }
;
let_binding:
  LET body = let_binding_body
    { body }
;
%inline let_ident:
    opt_ident(val_ident) { $1 }
;

value_description:
  VAL
  id = opt_ident(val_ident)
  COLON
  ty = core_type
    { id, ty }
;

(* Type declarations *)

%inline type_declaration:
  TYPE
  id = opt_ident(LIDENT)
  kind = type_kind
    { let manifest, definition = kind in
      id, { manifest ; definition }
    }
;

type_kind:
  | /*empty*/
      { (None, None) }
  | EQUAL
    ty = core_type
      { (None, Some (Alias ty : Core_types.def_type)) }
  | EQUAL
    ty = type_longident
      { (Some ty, None) }
  (* | EQUAL
   *   oty = type_synonym
   *   cs = constructor_declarations
   *     { (Ptype_variant cs, oty) }
   * | EQUAL
   *   oty = type_synonym
   *   LBRACE ls = label_declarations RBRACE
   *     { (Ptype_record ls, oty) } *)
;

with_constraint:
    TYPE name = fields(LIDENT) EQUAL ty = core_type
      { (Type (name, Alias ty) : enrichment) }
  | MODULE name = fields(UIDENT) EQUAL m = mod_ext_longident
      { (Module (name, Alias m) : enrichment) }
  | MODULE name = fields(UIDENT) COLON m = module_type %prec below_WITH
      { (Module (name, m) : enrichment) }
;

(* -------------------------------------------------------------------------- *)

(* Core language types. *)

core_type:
  | atomic_type { $1 }
;

atomic_type:
  | LPAREN RPAREN
      { Unit }
  | LPAREN core_type RPAREN
      { $2 }
  (* | QUOTE ident
   *     { Ptyp_var $2 }
   * | UNDERSCORE
   *     { Ptyp_any }
   * | tys = actual_type_parameters
   *   tid = type_longident
   *     { Ptyp_constr(tid, tys) } *)
;

(* %inline actual_type_parameters:
 *   | /* empty */
 *       { [] }
 *   | ty = atomic_type
 *       { [ty] }
 *   | LPAREN tys = separated_nontrivial_llist(COMMA, core_type) RPAREN
 *       { tys }
 * ; *)

(* Identifiers and long identifiers *)

ident:
    UIDENT                    { $1 }
  | LIDENT                    { $1 }
;
val_ident:
    LIDENT                    { $1 }
;
%inline opt_ident(id):
  | UNDERSCORE { None }
  | i = id { Some i }
;

fields(final):
  | s = final { [s] }
  | p = UIDENT DOT l=fields(final) { p::l }
;

%inline mk_longident(prefix,final):
  | final            { PathId $1 }
  | prefix DOT final { PathProj {path = $1 ; field = $3} }
;
longident_in_values:
  | UIDENT            { Path (PathId $1) }
  | longident_in_values DOT UIDENT { Path (PathProj{ path = $1 ; field = $3 }) }
  | LPAREN m = longident_in_values LESSERCOLON ty = module_type RPAREN
      { Ascription (m, ty) }
;
longident_in_types:
  | UIDENT            { Path (PathId $1) }
  | longident_in_types DOT UIDENT { Path (PathProj{ path = $1 ; field = $3 }) }
  | LPAREN m = longident_in_types LESSERCOLON ty = module_type RPAREN
      { Ascription (m, ty) }
  | longident_in_types LPAREN longident_in_types RPAREN
      { Apply ($1, $3) }
  | longident_in_types LPAREN error
      { expecting $loc($3) "module path" }
;
longident_in_module_types:
  | UIDENT            { Path (PathId $1) }
  | longident_in_module_types DOT UIDENT
    { Path (PathProj{ path = $1 ; field = $3 }) }
  | longident_in_module_types LPAREN longident_in_types RPAREN
    { Apply ($1, $3) }
  | longident_in_module_types LPAREN error
    { expecting $loc($3) "module path" }
;
mod_longident:
    longident_in_values { $1 }
;
mod_ext_longident:
    longident_in_types { $1 }
;
mty_longident:
    mk_longident(longident_in_module_types,ident) { $1 }
;
type_longident:
    mk_longident(longident_in_types,LIDENT) { $1 }
;

