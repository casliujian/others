type expr = 
      Var of string
    | Int of int
    | Float of float
    | Unt
    | Aray of (expr array)
    | Lst of (expr list)
    | Bool of bool
    | Tuple of (expr list)
    | Record of ((string * expr) list)
    | Negb of expr
    | And of expr * expr
    | Or of expr * expr
    | Negi of expr
    | Add of expr * expr
    | Minus of expr * expr
    | Mult of expr * expr
    | Equal of expr * expr
    | Non_Equal of expr * expr
    | LT of expr * expr
    | GT of expr * expr
    | LE of expr * expr
    | GE of expr * expr
    | IF of expr * expr * (expr option)
    | While of expr * expr
    | For of expr * expr * expr * expr
    | Seq of expr list
    | Assign of expr * expr
    | Match of (pattern * expr) list
    | With of expr * ((string * expr) list)
and pattern =
      Pat_Var of string
    | Pat_Int of int
    | Pat_Float of float
    | Pat_Unt
    | Pat_Aray of (pattern array)
    | Pat_Lst of (pattern list)
    | Pat_Lst_Cons of pattern * pattern
    | Pat_Underline
    | Pat_Tuple of (pattern list)
    | Pat_Record of ((string, pattern) list)