type expr = 
      Symbol of string list
    | Val_binding of string * expr
    | Var_binding of string * expr 
    | Int of int
    | Float of float
    | Unt
    | Aray of (expr list)
    | Lst of (expr list)
    | Aray_field of expr * expr
    | Bool of bool
    | Tuple of (expr array)
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
    | Pat_Aray of (pattern list)
    | Pat_Lst of (pattern list)
    | Pat_Lst_Cons of pattern * pattern
    | Pat_Underline
    | Pat_Tuple of (pattern list)
    (* | Pat_Record of ((string, pattern) array) *)
    | Pat_Constr of string * (pattern option)
and value = 
  | VInt of int
  | VFloat of float
  | VUnt
  | VBool of bool
  | VAray of (value list)
  | VLst of (value list)
  | VTuple of (value list)
  | VRecord of (string * value) list
  | VConstr of string * (value option)

let rec pexprl_to_expr pel = 
  match pel.pexpr with
  | PSymbol strs -> Symbol strs
  | PLocal_Val (str, pel1) -> Val_binding (str, pexprl_to_expr pel1)
  | PLocal_Var (str, pel1) -> Var_binding (str, pexprl_to_expr pel1)
  | PInt i -> Int i
  | PFloat f -> Float f
  | PUnt -> Unt
  | PAray pel -> Aray (List.map (fun pe -> pexprl_to_expr pe) pel)
  | PLst pel -> Lst (List.map (fun pe -> pexprl_to_expr pe) pel)
  | PAray_Field (pel1, pel2) -> Aray_field (pexprl_to_expr pel1, pexprl_to_expr pel2)
  | PBool b -> Bool b
  | PTuple pels -> Tuple (List.map (fun pel->pexprl_to_expr pel) pels)
  | PRecord str_pel_list -> Record (List.map (fun (str, pel) -> str, pexprl_to_expr pel) str_pel_list)
  | PNegb pel1 -> Negb (pexprl_to_expr pel1)
  | PAndo (pel1, pel2) -> Ando (pexprl_to_expr pel1, pexprl_to_expr pel2)
  | POro (pel1, pel2) -> Oro (pexprl_to_expr pel1, pexprl_to_expr pel2)
  | PNegi pel1 -> pexprl_to_expr pel1
  | PNegf pel1 -> pexprl_to_expr pel1
  | PAdd (pel1, pel2) -> Add (pexprl_to_expr pel1, pexprl_to_expr pel2)
  | PAddDot (pel1, pel2) -> AddDot (pexprl_to_expr pel1, pexprl_to_expr pel2)
  | PMinus (pel1, pel2) -> Minus (pexprl_to_expr pel1, pexprl_to_expr pel2)
  | PMinusDot (pel1, pel2) -> MinusDot (pexprl_to_expr pel1, pexprl_to_expr pel2)
  | PMult (pel1, pel2) -> Mult (pexprl_to_expr pel1, pexprl_to_expr pel2)
  | PMultDot (pel1, pel2) -> MultDot (pexprl_to_expr pel1, pexprl_to_expr pel2)
  | PEqual (pel1, pel2) -> Equal (pexprl_to_expr pel1, pexprl_to_expr pel2)
  | PNon_Equal (pel1, pel2) -> Non_Equal (pexprl_to_expr pel1, pexprl_to_expr pel2)
  | PLT (pel1, pel2) -> LT (pexprl_to_expr pel1, pexprl_to_expr pel2)
  | PLE (pel1, pel2) -> LE (pexprl_to_expr pel1, pexprl_to_expr pel2)
  | PGT (pel1, pel2) -> GT (pexprl_to_expr pel1, pexprl_to_expr pel2)
  | PGE (pel1, pel2) -> GE (pexprl_to_expr pel1, pexprl_to_expr pel2)
  | PIF (pel1, pel2, opel3) -> PIF (pexprl_to_expr pel1, pexprl_to_expr pel2, Option.bind pexprl_to_expr opel3)
  | PWhile (pel1, pel2) -> While (pexprl_to_expr pel1, pexprl_to_expr pel2)
  | PFor (str, pel1, pel2, pel3) -> For ()

and ppatl_to_pattern ppatl = 
  match ppatl.ppat with
  | PPat_Symbol str -> Pat_Symbol str
  | PPat_Int i -> Pat_Int i
  | PPat_Unt -> Pat_Unt
  | PPat_Aray ppatls -> Pat_Aray (List.map (fun ppatl->ppatl_to_pattern ppatl) ppatls)
  | PPat_Lst ppatls -> Pat_Lst (List.map (fun ppatl->ppatl_to_pattern ppatl) ppatls)
  | PPat_Lst_Cons (ppatl1, ppatl2) -> Pat_Lst_Cons (ppatl_to_pattern ppatl1, ppatl_to_pattern ppatl2)
  | PPat_Underline -> Pat_Underline
  | PPat_Tuple ppatls -> Pat_Tuple (List.map (fun ppatl->ppatl_to_pattern ppatl) ppatls)
  | PPat_Constr (str, opatl) -> PPat_Constr (str, Options.bind ppatl_to_pattern opatl)

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
