(** The lexer *)

val token: Lexing.lexbuf -> Parser.token


type error =
  | Illegal_character of char
  | Unterminated_comment of Location.t

exception Error of error * Location.t
