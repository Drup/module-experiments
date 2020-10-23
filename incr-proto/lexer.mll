(* The lexer definition *)

{
open Lexing
open Misc
open Parser

type error =
  | Illegal_character of char
  | Unterminated_comment of Location.t

exception Error of error * Location.t;;

(* The table of keywords *)

let keyword_table =
  create_hashtable 7 [
    "end", END;
    "functor", FUNCTOR;
    (* "include", INCLUDE; *)
    "module", MODULE;
    (* "open", OPEN; *)
    "sig", SIG;
    "struct", STRUCT;
    "type", TYPE;
    "val", VAL;
    "with", WITH;
]

(* To buffer string literals *)

let string_buffer = Buffer.create 256
let reset_string_buffer () = Buffer.reset string_buffer
let get_stored_string () = Buffer.contents string_buffer
let store_string s = Buffer.add_string string_buffer s
let store_lexeme lexbuf = store_string (Lexing.lexeme lexbuf)

(* To store the position of the beginning of a string and comment *)
let comment_start_loc = ref [];;

let wrap_comment_lexer comment lexbuf =
  let start_loc = Location.curr lexbuf  in
  comment_start_loc := [start_loc];
  reset_string_buffer ();
  let end_loc = comment lexbuf in
  let s = get_stored_string () in
  reset_string_buffer ();
  s,
  { start_loc with Location.loc_end = end_loc.Location.loc_end }

let error lexbuf e = raise (Error(e, Location.curr lexbuf))
let error_loc loc e = raise (Error(e, loc))

(* Update the current location with file name and line number. *)

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }

(* Error report *)

(* open Format *)

(* let prepare_error loc = function
 *   | Illegal_character c ->
 *       Location.errorf ~loc "Illegal character (%s)" (Char.escaped c)
 *   | Illegal_escape (s, explanation) ->
 *       Location.errorf ~loc
 *         "Illegal backslash escape in string or character (%s)%t" s
 *         (fun ppf -> match explanation with
 *            | None -> ()
 *            | Some expl -> fprintf ppf ": %s" expl)
 *   | Reserved_sequence (s, explanation) ->
 *       Location.errorf ~loc
 *         "Reserved character sequence: %s%t" s
 *         (fun ppf -> match explanation with
 *            | None -> ()
 *            | Some expl -> fprintf ppf " %s" expl)
 *   | Unterminated_comment _ ->
 *       Location.errorf ~loc "Comment not terminated"
 *   | Unterminated_string ->
 *       Location.errorf ~loc "String literal not terminated"
 *   | Unterminated_string_in_comment (_, literal_loc) ->
 *       Location.errorf ~loc
 *         "This comment contains an unterminated string literal"
 *         ~sub:[Location.msg ~loc:literal_loc "String literal begins here"]
 *   | Keyword_as_label kwd ->
 *       Location.errorf ~loc
 *         "`%s' is a keyword, it cannot be used as label name" kwd
 *   | Invalid_literal s ->
 *       Location.errorf ~loc "Invalid literal %s" s
 *   | Invalid_directive (dir, explanation) ->
 *       Location.errorf ~loc "Invalid lexer directive %S%t" dir
 *         (fun ppf -> match explanation with
 *            | None -> ()
 *            | Some expl -> fprintf ppf ": %s" expl)
 * 
 * let () =
 *   Location.register_error_of_exn
 *     (function
 *       | Error (err, loc) ->
 *           Some (prepare_error loc err)
 *       | _ ->
 *           None
 *     ) *)

}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']

let ident = (lowercase | uppercase) identchar*
let extattrident = ident ('.' ident)*

rule token = parse
  | newline
      { update_loc lexbuf None 1 false 0;
        token lexbuf }
  | blank +
      { token lexbuf }
  (* | "_"
   *     { UNDERSCORE } *)
  | lowercase identchar * as name
      { try Hashtbl.find keyword_table name
        with Not_found -> LIDENT name }
  | uppercase identchar * as name
      { UIDENT name }
  | "(*" | "(**"
      { let _ = wrap_comment_lexer comment lexbuf in
        token lexbuf }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "->" { MINUSGREATER }
  | "."  { DOT }
  | ":"  { COLON }
  | "="  { EQUAL }
  | eof { EOF }
  | (_ as illegal_char)
      { error lexbuf (Illegal_character illegal_char) }

and comment = parse
    "(*"
      { comment_start_loc := (Location.curr lexbuf) :: !comment_start_loc;
        store_lexeme lexbuf;
        comment lexbuf
      }
  | "*)"
      { match !comment_start_loc with
        | [] -> assert false
        | [_] -> comment_start_loc := []; Location.curr lexbuf
        | _ :: l -> comment_start_loc := l;
                  store_lexeme lexbuf;
                  comment lexbuf
       }
  | "\'\'"
      { store_lexeme lexbuf; comment lexbuf }
  | "\'" newline "\'"
      { update_loc lexbuf None 1 false 1;
        store_lexeme lexbuf;
        comment lexbuf
      }
  | "\'" [^ '\\' '\'' '\010' '\013' ] "\'"
      { store_lexeme lexbuf; comment lexbuf }
  | "\'\\" ['\\' '\"' '\'' 'n' 't' 'b' 'r' ' '] "\'"
      { store_lexeme lexbuf; comment lexbuf }
  | "\'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "\'"
      { store_lexeme lexbuf; comment lexbuf }
  | "\'\\" 'o' ['0'-'3'] ['0'-'7'] ['0'-'7'] "\'"
      { store_lexeme lexbuf; comment lexbuf }
  | "\'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "\'"
      { store_lexeme lexbuf; comment lexbuf }
  | eof
      { match !comment_start_loc with
        | [] -> assert false
        | loc :: _ ->
          let start = List.hd (List.rev !comment_start_loc) in
          comment_start_loc := [];
          error_loc loc (Unterminated_comment start)
      }
  | newline
      { update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        comment lexbuf
      }
  | ident
      { store_lexeme lexbuf; comment lexbuf }
  | _
      { store_lexeme lexbuf; comment lexbuf }
