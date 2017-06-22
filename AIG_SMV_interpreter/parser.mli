type token =
  | MODULE
  | MAIN
  | VAR
  | ASSIGN
  | DEFINE
  | INIT
  | NEXT
  | SPEC
  | COLON
  | SEMICOLON
  | ASSIGNO
  | EXCLAM
  | LB1
  | RB1
  | TRUE
  | FALSE
  | AND
  | OR
  | AG
  | File_end
  | ID of (string)

val input :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
