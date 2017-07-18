open Ast

let rec str_ptyp pt = 
    match pt with
    | PTInt -> "int"
    | PTFloat -> "float"
    | PTBool -> "bool"
    | PTUnt -> "unit"
    | PTAray pt1 -> "(array "^(str_ptyp pt1)^")"
    | PTLst pt1 -> "(list "^(str_ptyp pt1)^")"
    | PTTuple pt_list -> 
        let tmp_str = ref "(" in
        tmp_str := !tmp_str ^ (str_ptyp (List.hd pt_list));
        for i = 1 to List.length pt_list - 1 do
            tmp_str := !tmp_str ^ (str_ptyp (List.nth pt_list i))
        done;
        tmp_str := !tmp_str ^ ")";
        !tmp_str
    | PTRecord str_pt_list ->
        let tmp_str = ref "{" in
        List.iter (fun (str, pt1) -> tmp_str := !tmp_str^str^":"^(str_ptyp pt1)^";") str_pt_list;
        tmp_str := !tmp_str ^ "}";
        !tmp_str
    | PTArrow (pt1, pt2) -> "("^(str_ptyp pt1)^")->("^(str_ptyp pt2)^")"
    | PTConstrs str_opt_list ->
        let str_constr (str, opt) = 
            match opt with
            | None -> ""
            | Some pt1 -> str_ptyp pt1 in
        let tmp_str = ref "" in
        tmp_str := !tmp_str ^ (str_constr (List.hd str_opt_list));
        List.iter (fun str_opt -> tmp_str := !tmp_str^" | "^(str_constr str_opt)) (List.tl str_opt_list);
        !tmp_str
    | PTUdt (str, pt_list) -> str^" "^(List.fold_left (fun s pt -> s^" "^(str_ptyp pt)) "" pt_list)
    | PTVar i -> "(Type "^(string_of_int i)^")"

let rec str_ppatl ppatl = 
    let rec str_ppat ppat = 
        match ppat with
        | 
    in
    (str_ppat ppatl.ppat)^":"^(str_ptyp ppatl.ptyp)

let rec str_pexprl pel =
    let rec str_pexpr pe =
        match pe with
        | PSymbol str -> str
        | PLocal_Val (str, pel1) -> "val "^str^"="^(str_pexprl pel1)
        | PLocal_Var (str, pel1) -> "var "^str^"="^(str_pexprl pel1)
        | PDot (pel1, pel2) -> (str_pexprl pel1)^"."^(str_pexprl pel2)
        | PInt i -> (string_of_int i)
        | PFloat f -> (string_of_float f)
        | PUnt -> "()"
        | PAray pel_list -> "[|" ^ (List.fold_left (fun s pel -> s^";"^(str_pexprl pel)) "" pel_list) ^ "|]"
        | PLst pel_list -> "[" ^ (List.fold_left (fun s pel -> s^";"^(str_pexprl pel)) "" pel_list) ^ "]"
        | PAray_Field (pel1, pel2) -> (str_pexprl pel1)^"["^(str_pexprl pel2)^"]"
        | PBool b -> string_of_bool b
        | PTuple pel_list ->
            let tmp_str = ref "(" in
            tmp_str := !tmp_str ^ (str_pexprl (List.hd pel_list));
            List.iter (fun pel -> tmp_str := !tmp_str^","^(str_pexprl pel)) (List.tl pel_list);
            tmp_str := !tmp_str ^ ")";
            !tmp_str
        | PRecord str_pel_list -> 
            let tmp_str = ref "{" in
            List.iter (fun (str, pel) -> tmp_str := !tmp_str^str^"="^(str_pexprl pel)^";") (str_pel_list);
            tmp_str := !tmp_str ^ "}";
            !tmp_str
        | PNegb pel1 -> "(! "^(str_pexprl pel1)^")"
        | PAndo (pel1, pel2) -> "("^(str_pexprl pel1)^"/\\"^(str_pexprl pel2)^")"
        | POro (pel1, pel2) -> "("^(str_pexprl pel1)^"\\/"^(str_pexprl pel2)^")"
        | PNegi pel1 -> "(- "^(str_pexprl pel1)^")"
        | PNegi pel1 -> "(-. "^(str_pexprl pel1)^")"
        | PAdd (pel1, pel2) -> "("^(str_pexprl pel1)^"+"^(str_pexprl pel2)^")"
        | PAddDot (pel1, pel2) -> "("^(str_pexprl pel1)^"+."^(str_pexprl pel2)^")"
        | PMinus (pel1, pel2) -> "("^(str_pexprl pel1)^"-"^(str_pexprl pel2)^")"
        | PMinusDot (pel1, pel2) -> "("^(str_pexprl pel1)^"-."^(str_pexprl pel2)^")"
        | PMult (pel1, pel2) -> "("^(str_pexprl pel1)^"*"^(str_pexprl pel2)^")"
        | PMultDot (pel1, pel2) -> "("^(str_pexprl pel1)^"*."^(str_pexprl pel2)^")"
        | PEqual (pel1, pel2) -> "("^(str_pexprl pel1)^"="^(str_pexprl pel2)^")"
        | PNon_Equal (pel1, pel2) -> "("^(str_pexprl pel1)^"!="^(str_pexprl pel2)^")"
        | PLT (pel1, pel2) -> "("^(str_pexprl pel1)^"<"^(str_pexprl pel2)^")"
        | PLE (pel1, pel2) -> "("^(str_pexprl pel1)^"<="^(str_pexprl pel2)^")"
        | PGT (pel1, pel2) -> "("^(str_pexprl pel1)^">"^(str_pexprl pel2)^")"
        | PGE (pel1, pel2) -> "("^(str_pexprl pel1)^">="^(str_pexprl pel2)^")"
        | PIF (pel1, pel2, opel3) -> begin
                match opel3 with
                | None -> "if "^(str_pexprl pel1)^" then ("^(str_pexprl pel2)^")"
                | Some pel3 -> "if "^(str_pexprl pel1)^" then ("^(str_pexprl pel2)^") else ("^(str_pexprl pel3)^")"
            end
        | PWhile (pel1, pel2) -> "while "^(str_pexprl pel1)^" do \n("^(str_pexprl pel2)^")\ndone"
        | PFor (str, pel1, pel2, pel3) -> "for "^str^" from "^(str_pexprl pel1)^" to "^(str_pexprl pel2)^" do\n("^(str_pexprl pel3)^")\ndone"
        | PSeq pel_list -> List.fold_left (fun s pel -> s^(str_pexprl pel)^";\n") pel_list
        | PAssign (pel1, pel2) -> (str_pexprl pel1)^" <- "^(str_pexprl pel2)
        | PMatch (pel1, ppatl_pel_list) ->
    in
    (str_pexpr (pel.pexpr))^":"^(str_ptyp (pel.ptyp))
