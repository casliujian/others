type expr = 
      Symbol of string list
    | Val_binding of string * expr
    | Var_binding of string * expr 
    | Int of int
    | Float of float
    | Unt
    | Aray of (expr array)
    | Lst of (expr array)
    | Aray_field of expr * expr
    | Bool of bool
    | Tuple of (expr array)
    | Record of ((string * expr) array)
    | Negb of expr
    | Ando of expr * expr
    | Oro of expr * expr
    | Negi of expr
    | Negf of expr
    | Add of expr * expr
    | AddDot of expr * expr
    | Minus of expr * expr
    | MinusDot of expr * expr
    | Mult of expr * expr
    | MultDot of expr * expr
    | Equal of expr * expr
    | Non_Equal of expr * expr
    | LT of expr * expr
    | GT of expr * expr
    | LE of expr * expr
    | GE of expr * expr
    | IF of expr * expr * (expr option)
    | While of expr * expr
    | For of string * expr * expr * expr
    | Seq of expr list
    | Assign of expr * expr
    | Match of expr * ((pattern * expr) list)
    | With of expr * ((string * expr) list)
    | Constr of string * (expr option)
    | Apply of string * (expr list)
and pattern =
      Pat_Symbol of string
    | Pat_Int of int
    | Pat_Float of float
    | Pat_Unt
    | Pat_Aray of (pattern array)
    | Pat_Lst of (pattern array)
    | Pat_Lst_Cons of pattern * pattern
    | Pat_Underline
    | Pat_Tuple of (pattern array)
    | Pat_Record of ((string, pattern) array)
    | Pat_Constr of string * (pattern option)
and value = 
  | VInt of int
  | VFloat of float
  | VUnt
  | VBool of bool
  | VAray of (value array)
  | VLst of (value array)
  | VTuple of (value array)
  | VRecord of (string * value) list
  | VConstr of string * (value option)

let rec str_value v = 
  match v with
  | VInt i -> string_of_int i
  | VFloat f -> string_of_float f
  | VUnt -> "()"
  | VBool b -> string_of_bool b
  | VAray va -> "[|"^ (Array.fold_left (fun str v1 -> str^(str_value v1)^";") "" va) ^"|]"
  | VLst va -> "["^ (Array.fold_left (fun str v1 -> str^(str_value v1)^";") "" va) ^"]"
  | VTuple va -> "("^ (Array.fold_left (fun str v1 -> str^(str_value v1)^",") "" va) ^")"
  | VRecord str_value_array -> "{"^ (Array.fold_left (fun str (s1,v1) -> str^s1^"="^(str_value v1)^";") "" va) ^"}"
  | VConstr (str, None) -> str
  | VConstr (str, Some v1) -> str^" "^(str_value v1)
