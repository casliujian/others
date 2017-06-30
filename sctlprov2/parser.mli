
(* The type of tokens. *)

type token = 
  | With
  | While
  | Vertical
  | Var
  | Val
  | Underline
  | UIden of (string)
  | True
  | Transition
  | Top
  | Then
  | Semicolon
  | RB3
  | RB2
  | RB1
  | Property
  | Oro
  | Or
  | Non_Equal
  | Negb
  | Neg
  | MultDot
  | Mult
  | Model
  | MinusDot
  | Minus
  | Match
  | LT
  | LE
  | LB3
  | LB2
  | LB1
  | LArrow
  | Int of (int)
  | Init
  | In
  | Import
  | If
  | Iden of (string)
  | GT
  | GE
  | Function
  | For
  | Float of (float)
  | False
  | Equal
  | Else
  | EX
  | EU
  | EOF
  | EG
  | DotDot
  | Dot
  | Done
  | Do
  | Datatype
  | Comma
  | ColonColon
  | Bottom
  | Arrow
  | Ando
  | And
  | AddDot
  | Add
  | AX
  | AR
  | AF

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((string list) * psymbol_tbl * (pkripke_model option))
