type expr = 
      Symbol of string
    | Val_binding of string * expr
    | Var_binding of string * expr 
    | Int of int
    | Float of float
    | Unt
    | Aray of (expr array)
    | Lst of (expr list)
    | Aray_field of expr * expr
    | Bool of bool
    | Tuple of (expr list)
    | Record of ((string * expr) list)
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
    | Pat_Lst of (pattern list)
    | Pat_Lst_Cons of pattern * pattern
    | Pat_Underline
    | Pat_Tuple of (pattern list)
    | Pat_Record of ((string, pattern) list)
    | Pat_Constr of string * (pattern option)

exception runtime_error of string

type context = (string * expr) list

let modify_ctx ctx str expr = Pairs.replace_first ctx str expr 
let add_to_ctx ctx str expr = (str, expr)::ctx

let rec evaluate_seq exprs ctx = 
  match exprs with
  | [] -> raise (runtime_error "error evaluating seq expr")
  | [e] -> evaluate e ctx
  | e :: es -> begin
      match e with
      | Val_binding (str, e1) -> evaluate_seq es (add_to_ctx ctx str (evaluate e1 ctx))
      | Var_binding (str, e1) -> evaluate_seq es (add_to_ctx ctx str (evaluate e1 ctx))
      | _ -> let _ = evaluate e ctx in evaluate_seq es ctx
    end  

and evaluate expr ctx = 
  match expr with
  | Int _ | Float _ | Unt | Bool _ -> expr
  | 