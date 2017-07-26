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
exception No_matched_pattern of value

type context = (string * value) list

let modify_ctx ctx str value = Pairs.replace_first ctx str value 
let add_to_ctx ctx str value = (str, value)::ctx

let rec value_of_str str runtime modul = 
    let m = Hashtbl.find runtime.moduls modul in
    if Pairs.key_exists m.ctx str then
        Pairs.get_value m.ctx str
    else begin
        let found_in_imported = ref false 
        and value = ref (VInt 0) in
        List.iter (fun mname -> 
            if not !found_in_imported then begin
                let m = Hashtbl.find runtime.moduls mname in
                if Pairs.key_exists m.ctx str then begin
                    value := Pairs.get_value m.ctx str;
                    found_in_imported := true
                end
            end
        ) m.imported;
        if !found_in_imported then
            !value
        else
            raise (Evaluation_error ("can not find binding of "^str))
    end

let rec value_of_str_path value strs =
    match strs with
    | [] -> value
    | str::strs' -> begin
            match value with
            | VRecord str_value_list -> 
                if Pairs.key_exists str_value_list str then
                    value_of_str_path (Pairs.get_value str_value_list str) strs'
                else
                    raise (Evaluation_error ((str_value value)^" does not have field "^str))
            | _ -> raise (Evaluation_error ((str_value value)^" should be a record with field "^str))
        end

let rec modified_record_value rv str_list value = 
    match str_list with
    | [] -> value
    | str::strs -> begin
            match rv with
            | VRecord str_value_list -> VRecord (Pairs.replace_first str_value_list str (modified_record_value (Pairs.get_value str_value_list str) strs value))
            | _ -> raise (Evaluation_error ((str_value rv)^" should be a record value."))
        end

let rec get_matched_pattern value pat_expr_list = 
    match value, pat_expr_list with
    | _, [] -> raise (No_matched_pattern value)
    | _, (Pat_Underline, expr)::pel -> [], expr
    | _, (Pat_Symbol str, expr)::pel -> [(str, value)], expr
    | VInt i, (Pat_Int j, expr)::pel -> if i = j then [], expr else get_matched_pattern value pel
    (* | VInt i, (Pat_Symbol str, expr)::pel -> [(str, VInt i)], expr *)
    | VFloat f, (Pat_Float g, expr)::pel -> if f = g then [], expr else get_matched_pattern value pel
    (* | VFloat f, (Pat_Symbol str, expr)::pel -> [(str, VFloat f)], expr *)
    | VUnt, (Pat_Unt, expr)::pel -> [], expr
    | VBool b, (VBool c, expr)::pel -> if b=c then [], expr else get_matched_pattern value pel
    | VAray vl, (Pat_Aray pl, expr)::pel ->
        

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
                | None -> value_of_str str runtime modul 
                | Some v -> v
            end 
        | str::strs ->
            if str = (String.capitalize_ascii str) then
                value_from_str_path (value_of_str (List.hd strs) runtime str) (List.tl strs))
            else
                if Pairs.key_exists ctx str then
                    value_from_str_path (Pairs.get_value ctx str) strs
                else
                    value_from_str_path (value_of_str str runtime modul) strs
        end
    | Val_binding _ | Var_binding _ -> raise (Evaluation_error "should not bind variables in the last expression")
    | Aray ea -> VAray (Array.map (fun e -> evaluate e ctx runtime modul) ea)
    | Lst ea -> VLst (Array.map (fun e -> evaluate e ctx runtime modul) ea)
    | Aray_field (e1, e2) -> 
        let v1 = evaluate e1 ctx runtime modul 
        and v2 = evaluate e2 ctx runtime modul in begin
            match v1,v2 with
            | VAray va, VInt i -> va.(i)
            | _ -> raise (Evaluation_error ((str_value v1)^" should be an array value, and "^(str_value v2)^" should be an integer value."))
        end
    | Tuple ea -> VTuple (Array.map (fun e -> evaluate e ctx runtime modul) ea)
    | Record str_expr_array -> VRecord (Array.to_list (Array.map () str_expr_array))
    | Negb e1 -> 
        let v1 = evaluate e1 ctx runtime modul in begin
            match v1 with
            | VBool b -> VBool (not b)
            | _ -> raise (Evaluation_error ((str_value v1)^" is not a bool value."))    
        end
    | Ando (e1, e2) -> 
        let v1 = evaluate e1 ctx runtime modul 
        and v2 = evaluate e2 ctx runtime modul in begin
            match v1, v2 with
            | VBool b1, VBool b2 -> VBool (b1 && b2)
            | _ -> raise (Evaluation_error "can not evaluate to bool value.")    
        end
    | Oro (e1, e2) -> 
        let v1 = evaluate e1 ctx runtime modul 
        and v2 = evaluate e2 ctx runtime modul in begin
            match v1, v2 with
            | VBool b1, VBool b2 -> VBool (b1 || b2)
            | _ -> raise (Evaluation_error "can not evaluate to bool value.")    
        end
    | Negi e1 -> 
        let v1 = evaluate e1 ctx runtime modul in begin
            match v1 with
            | VInt i -> VInt (-i)
            | _ -> raise (Evaluation_error ((str_value v1)^" is not a integer value."))    
        end
    | Negf e1 ->
        let v1 = evaluate e1 ctx runtime modul in begin
            match v1 with
            | VFloat f -> VFloat (0 -. f)
            | _ -> raise (Evaluation_error ((str_value v1)^" is not a float value."))    
        end
    | Add (e1, e2) -> 
        let v1 = evaluate e1 ctx runtime modul 
        and v2 = evaluate e2 ctx runtime modul in begin
            match v1, v2 with
            | VInt i1, VInt i2 -> VInt (i1+i2)
            | _ -> raise (Evaluation_error "can not evaluate to integer value.")    
        end
    | AddDot (e1, e2) -> 
        let v1 = evaluate e1 ctx runtime modul 
        and v2 = evaluate e2 ctx runtime modul in begin
            match v1, v2 with
            | VFloat f1, VFloat f2 -> VFloat (f1+.f2)
            | _ -> raise (Evaluation_error "can not evaluate to integer value.")    
        end
    | Minus (e1, e2) -> 
        let v1 = evaluate e1 ctx runtime modul 
        and v2 = evaluate e2 ctx runtime modul in begin
            match v1, v2 with
            | VInt i1, VInt i2 -> VInt (i1-i2)
            | _ -> raise (Evaluation_error "can not evaluate to integer value.")    
        end
    | MinusDot (e1, e2) -> 
        let v1 = evaluate e1 ctx runtime modul 
        and v2 = evaluate e2 ctx runtime modul in begin
            match v1, v2 with
            | VFloat f1, VFloat f2 -> VFloat (f1-.f2)
            | _ -> raise (Evaluation_error "can not evaluate to float value.")    
        end
    | Mult (e1, e2) -> 
        let v1 = evaluate e1 ctx runtime modul 
        and v2 = evaluate e2 ctx runtime modul in begin
            match v1, v2 with
            | VInt i1, VInt i2 -> VInt (i1*i2)
            | _ -> raise (Evaluation_error "can not evaluate to integer value.")    
        end
    | MultDot (e1, e2) -> 
        let v1 = evaluate e1 ctx runtime modul 
        and v2 = evaluate e2 ctx runtime modul in begin
            match v1, v2 with
            | VFloat i1, VFloat i2 -> VFloat (i1*.i2)
            | _ -> raise (Evaluation_error "can not evaluate to integer value.")    
        end
    | Equal (e1, e2) ->
        let v1 = evaluate e1 ctx runtime modul 
        and v2 = evaluate e2 ctx runtime modul in
        v1 = v2
    | Non_Equal (e1, e2) ->
        let v1 = evaluate e1 ctx runtime modul 
        and v2 = evaluate e2 ctx runtime modul in
        v1 <> v2
    | LT (e1, e2) ->
        let v1 = evaluate e1 ctx runtime modul 
        and v2 = evaluate e2 ctx runtime modul in
        v1 < v2
    | LE (e1, e2) ->
        let v1 = evaluate e1 ctx runtime modul 
        and v2 = evaluate e2 ctx runtime modul in
        v1 <= v2
    | GT (e1, e2) ->
        let v1 = evaluate e1 ctx runtime modul 
        and v2 = evaluate e2 ctx runtime modul in
        v1 > v2
    | GE (e1, e2) ->
        let v1 = evaluate e1 ctx runtime modul 
        and v2 = evaluate e2 ctx runtime modul in
        v1 >= v2
    | If (e1, e2, None) ->
        let v1 = evaluate e1 ctx runtime modul in
        if v1 = VBool true then
            evaluate e2 ctx runtime modul
        else if v1 = VBool false then
            VUnt
        else
            raise (Evaluation_error ((str_value v1)^" should be a bool value."))
    | Seq es -> evaluate_seqs es ctx runtime modul
    | Assign (e1, e2) -> begin
            match e1 with
            | Symbol str_list -> begin
                    match str_list with
                    | [] -> raise (Evaluation_error "can not assign to an empty symbol.")
                    | [str] -> modify_ctx ctx str (evaluate e2 ctx runtime modul); VUnt
                    | str::strs -> modify_ctx ctx str (modified_record_value (Pairs.get_value ctx str) strs (evaluate e2 ctx runtime modul)); VUnt
                end
            | _ -> raise (Evaluation_error ("error evaluating assign expr."))
        end        
    | Match (e1, pat_expr_list) -> ***
    | With (e1, str_expr_list) -> 
        let v1 = evaluate e1 ctx runtime modul in begin
            match v1 with
            | VRecord str_value_list -> VRecord (List.fold_left (fun svl (str, expr) -> Pairs.replace_first svl str (evaluate expr ctx runtime modul)) str_value_list str_expr_list)
            | _ -> raise (Evaluation_error ((str_value v1)^" should be a record value."))    
        end
    | Constr (str, None) -> VConstr (str, None)
    | Constr (str, Some e1) -> VConstr (str, Some (evaluate e1 ctx runtime modul))