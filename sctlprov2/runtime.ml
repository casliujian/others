(* open Expr *)

type modul = {
    imported: string list;
    ctx: Expr.context;
    vars: (string, Expr.value) Hashtbl.t;
    functions: (string, (Expr.pattern * Expr.expr)) Hashtbl.t;
}

type model = {
    transition: (Expr.pattern * Expr.expr);
    properties: (string * Expr.formula) list;
}

type runtime = {
    moduls: (string, modul) Hashtbl.t;
    model: model;
}

exception Evaluation_error of string

type context = (string * value) list

let modify_ctx ctx str value = Pairs.replace_first ctx str value 
let add_to_ctx ctx str value = (str, value)::ctx

let rec evaluate_seq exprs ctx runtime modul = 
  match exprs with
  | [] -> raise (Evaluation_error "error evaluating seq expr")
  | [e] -> evaluate e ctx runtime modul 
  | e :: es -> begin
      match e with
      | Val_binding (str, e1) -> evaluate_seq es (add_to_ctx ctx str (evaluate e1 ctx)) runtime modul
      | Var_binding (str, e1) -> evaluate_seq es (add_to_ctx ctx str (evaluate e1 ctx)) runtime modul
      | _ -> let _ = evaluate e ctx runtime modul in evaluate_seq es ctx runtime modul
    end  

and evaluate expr ctx runtime modul = 
  match expr with
  | Int i -> VInt i
  | Float f -> VFloat f
  | Unt -> VUnt
  | Bool b -> VBool b
  | Symbol str_list -> begin
        match str_list with
        | [] -> raise (Evaluation_error "can not evaluate an empty symbol")
        | [str] -> 
            if str = (String.capitalize_ascii str) then
                raise (Evaluation_error ("can not evaluate a modul name as a symbol: "^str))
            else begin
                match Pairs.find ctx str with
                | None -> 
                    let m = Hashtbl.find runtime.moduls modul in
                    
                | Some v -> v
            end 
    end