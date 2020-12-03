(* The lexer definition *)

{
open Peyhel
open Parser

type Input.Lex.error +=
  | Unterminated_comment of Location.loc

(* The table of keywords *)

let keyword_table =
  CCHashtbl.of_list [
    "end", END;
    "functor", FUNCTOR;
    (* "include", INCLUDE; *)
    "module", MODULE;
    (* "open", OPEN; *)
    "sig", SIG;
    "struct", STRUCT;
    "type", TYPE;
    "let", LET;
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
  let start_loc = Location.of_lex lexbuf  in
  comment_start_loc := [start_loc];
  reset_string_buffer ();
  let _end_loc = comment lexbuf in
  let s = get_stored_string () in
  reset_string_buffer ();
  s(* ,
    * { start_loc with Location.loc_end = end_loc.Location.loc_end } *)

let error lexbuf e = raise (Input.Lex.Error(e, Location.of_lex lexbuf))
let error_loc loc e = raise (Input.Lex.Error(e, loc))

(* Update the current location with file name and line number. *)

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.Lexing.lex_curr_p in
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

let prepare_error = function
  | Input.Lex.Error (Unterminated_comment _, loc) ->
    Some (Report.errorf ~loc "Comment not terminated")
  | _ -> None

let () =
  Report.register_report_of_exn prepare_error
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
  | "_"
      { UNDERSCORE }
  | lowercase identchar * as name
      { try Hashtbl.find keyword_table name
        with Not_found -> LIDENT name }
  | uppercase identchar * as name
      { UIDENT name }
  | "(*EXPECT"
    { let s = wrap_comment_lexer comment lexbuf in
     EXPECT s }
  | "(*" | "(**"
      { let _ = wrap_comment_lexer comment lexbuf in
        token lexbuf }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "->" { MINUSGREATER }
  | "."  { DOT }
  | ":"  { COLON }
  | "<:"  { LESSERCOLON }
  | "="  { EQUAL }
  | eof { EOF }
  | (_ as illegal_char)
      { error lexbuf (Peyhel.Input.Lex.Illegal_character illegal_char) }

and comment = parse
    "(*"
      { comment_start_loc := (Location.of_lex lexbuf) :: !comment_start_loc;
        store_lexeme lexbuf;
        comment lexbuf
      }
  | "*)"
      { match !comment_start_loc with
        | [] -> assert false
        | [_] -> comment_start_loc := []; Location.of_lex lexbuf
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
