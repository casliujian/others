open Ast
open Printf

exception Unify_error of ptyp * ptyp
exception Invalid_typepath of string
exception Invalid_pexpr_loc of pexpr_loc * string
exception Undefined_modul of string
exception Undefined_idenfier of string

(*type dep = string * ((dep list) option)

let rec dep_of_pmodul pmname pmoduls = 
    try
        let pm = Hashtbl.find pmname in
        match pm.imported with
        | [] -> pmname, None
        | impts -> pmname, Some (List.map (fun a -> dep_of_pmodul a pmoduls) impts)
    with Not_found -> eprintf "Error: module %s is not defined.\n" pmname; exit 1*)

type env = (ptyp * ptyp) list

let ptyp_from_typepath strs modul moduls = 
    let find_ptyp_in_modul ptname m = 
        try
            let modul_exists = ref false in
            let modul = Hashtbl.find moduls m in
            modul_exists := true;
            let kind_ast = Hashtbl.find modul.psymbol_tbl ptname in
            match kind_ast with
            | UDT, PTyp pt -> pt
            | _ -> raise (Invalid_typepath str)
        with Not_found -> 
            if !modul_exists then 
                raise (Invalid_typepath str)
            else 
                raise (Undefined_modul m) in
    let strs = String.split_on_char '.' (String.trim str) in
    if List.length strs > 2 || List.length strs = 0 then 
        raise Invalid_typepath str
    else
        match strs with
        | [ptname] -> find_ptyp_in_modul ptname modul
        | [mname; ptname] -> find_ptyp_in_modul ptname mname

let rec apply_env_to_ptyp env ptyp = 
    let rec find_key_binding bindings (PTVar key) = 
        match bindings with
        | [] -> PTVar key
        | (PTVar ki, PTVar vi) :: bindings' -> if (key=ki)&&(ki>=vi) then PTVar vi else find_key_binding bindings' (PTVar key)
        | (PTVar ki, pt) :: bindings' -> if key=ki then pt else find_key_binding bindings' (PTVar key) 
        | _ :: bindings' -> find_key_binding bindings' (PTVar key)
    in
    match ptyp with
    | PTInt | PTFloat | PTBool | PTUnt | PTUdt _ -> ptyp
    | PTAray pt -> PTAray (apply_env_to_ptyp env pt)
    | PTLst pt -> PTLst (apply_env_to_ptyp env pt)
    | PTTuple pts -> PTTuple (List.map (fun a -> apply_env_to_ptyp env a) pts)
    | PRecord str_ptyps -> PRecord (List.map (fun (a,b) -> a, apply_env_to_ptyp b) str_ptyps)
    | PTConstrs str_optyps -> 
        PTConstrs (List.map (fun (a, ob) -> 
            match ob with
            | None -> a, None
            | Some b -> a, apply_env_to_ptyp b
            ) str_optyps)
    | PTVar vi -> find_key_binding env (PTVar vi)

let rec unify ptyp_list modul moduls = 
    match ptyp_list with
    | [] | [ptyp] -> []
    | ptyp1 :: ptyp2 :: ptyps -> begin
            match ptyp1, ptyp2 with
            | PTInt, PTInt | PTFloat, PTFloat | PTBool, PTBool | PTUnt, PTUnt -> unify (ptyp2:: ptyps)
            | PTVar vi1, PTVar vi2 -> 
                if vi1 = vi2 then 
                    unify (ptyp2::ptyps) modul moduls
                else if vi1 < vi2 then
                    let env = [(PTVar vi2, PTVar vi1)] in
                    env @ (unify (List.map (fun a -> apply_env_to_ptyp env a) (ptyp2::ptyps)) modul moduls)
                else 
                    let env = [(PTVar vi1, PTVar vi2)] in
                    env @ (unify (List.map (fun a -> apply_env_to_ptyp env a) (ptyp2::ptyps)) modul moduls)
            | pt, PTVar vi | PTVar vi, pt -> 
                let env = [(PTVar vi, pt)] in
                env @ (unify (List.map (fun a -> apply_env_to_ptyp env a) (ptyp2::ptyps)) modul moduls)
            | PTAray pt1, PTAray pt2 | PTLst pt1, PTLst pt2 -> 
                let env = unify [pt1;pt2] modul moduls in
                env @ (unify (List.map (fun a -> apply_env_to_ptyp env a) (ptyp2::ptyps)) modul moduls)
            | PTTuple pts1, PTTuple pts2 -> 
                if List.length pts1 <> List.length pts2 then
                    raise (Unify_error (ptyp1, ptyp2))
                else
                    let env = List.fold_left (fun e (a1,a2) -> (unify [a1;a2] modul moduls) @ e) [] (List.combine pts1 pts2) in
                    env @ (unify (List.map (fun a -> apply_env_to_ptyp env a) (ptyp2::ptyps)) modul moduls)
            | PTRecord str_pt_list1, PTRecord str_pt_list2 ->
                if List.length str_pt_list1 <> List.length str_pt_list2 then
                    raise (Unify_error (ptyp1, ptyp2))
                else 
                    let rec find_ptyp str_pt_list str =
                        match str_ptyp with
                        | [] -> PTVar 0
                        | (s,pt)::str_pt_list' -> if s = str then pt else find_ptyp str_pt_list' str in
                    let env = List.fold_left (fun e (str, pt) ->
                            let pt2 = find_ptyp str_pt_list2 str in
                            if pt2 = PTVar 0 then 
                                raise (Unify_error (ptyp1, ptyp2))
                            else
                                e @ (unify [pt;pt2] modul moduls)
                        ) [] str_pt_list1 in
                    env @ (unify (List.map (fun a -> apply_env_to_ptyp env a) (ptyp2::ptyps)) modul moduls)
            | PTConstrs str_opt_list1, PTConstrs str_opt_list2 -> 
                if List.length str_opt_list1 <> List.length str_opt_list2 then
                    raise (Unify_error (ptyp1, ptyp2))
                else 
                    let rec find_ptyp str_pt_list str =
                        match str_ptyp with
                        | [] -> Some (PTVar 0)
                        | (s,pt)::str_pt_list' -> if s = str then pt else find_ptyp str_pt_list' str in
                    let env = List.fold_left (fun e (str, opt) ->
                            let opt2 = find_ptyp str_opt_list2 str in
                            match opt, opt2 with
                            | None, None -> e
                            | None, Some _ | Some _, None -> raise (Unify_error (ptyp1, ptyp2))
                            | Some PTVar 0, _ | _, Some PTVar 0 -> raise (Unify_error (ptyp1, ptyp2))
                            | Some pt1, Some pt2 -> e @ (unify [pt1; pt2] modul moduls)
                        ) [] str_opt_list1 in
                    env @ (unify (List.map (fun a -> apply_env_to_ptyp env a) (ptyp2::ptyps)) modul moduls)
            | PTUdt (str, ptyp_list), _ -> 
                (*let strs = String.split_on_char '.' (String.trim str) in*)
                let ptyp = ptyp_from_typepath str modul moduls in
                let index = ref (0) in
                let ptyp_concrete = List.fold_left (fun pt pt1 -> descr index; replace_ptvar pt index pt1) ptyp ptyp_list in
                unify (ptyp_concrete::ptyp2::ptyps) modul moduls
            | _, PTUdt (str, ptyp_list) ->
                (*let strs = String.split_on_char '.' (String.trim str) in*)
                let ptyp = ptyp_from_typepath str modul moduls in
                let index = ref (0) in
                let ptyp_concrete = List.fold_left (fun pt pt1 -> descr index; replace_ptvar pt index pt1) ptyp ptyp_list in
                unify (ptyp1::ptyp_concrete::ptyps) modul moduls
            | _ -> raise (Unify_error (ptyp1,ptyp2))
        end


let rec check_dep pmname pmoduls = 
    try
        let pm = Hashtbl.find pmoduls pmname in
        List.iter (fun a -> check_dep a pmoduls) pm.imported
    with Not_found -> 
        eprintf "Error: module %s is not defined.\n" pmname; 
        exit 1

let expand_udt_path moduls = 
    let rec add_type_prefix mname ptyp =
        match ptyp with
        | PTUdt (str, ptyps) -> 
            let strs = String.split_on_char '.' str in begin
                match strs with
                | [] -> raise (Invalid_typepath str)
                | [pname] -> PTUdt (mname^"."^str, List.map (fun pt -> add_type_prefix mname pt) ptyps)
                | _ -> PTUdt (str, List.map (fun pt -> add_type_prefix mname pt) ptyps)     
            end
        | PTAray pt -> PTAray (add_type_prefix mname pt)
        | PTLst pt -> PTAray (add_type_prefix mname pt)
        | PTTuple pts -> PTTuple (List.map (fun pt -> add_type_prefix mname pt) pts)
        | PTRecord str_pts -> PTRecord (List.map (fun (str1, pt) -> (str1, add_type_prefix mname pt)) str_pts)
        | PTConstrs str_opts -> 
            PTConstrs (List.map (fun (str1, opt) ->
                match opt with
                | None -> (str1, None)
                | Some pt -> (str1, Some (add_type_prefix mname pt))
            ) str_opts)
        | _ -> ptyp in
    Hashtbl.iter (fun mname modul ->
        Hashtbl.iter (fun (str, (pkind, ast)) -> 
            match ast with
            | PExpr_loc pe -> pe.ptyp <- add_type_prefix mname pe.ptyp; PExpr_loc pe
            | PTyp pt -> add_type_prefix mname pt
            | PFunction (ppls, pe) -> pe.ptyp <- add_type_prefix mname pe; PFunction (ppls, pe)
        ) modul.psymbol_tbl
    ) moduls

type type_context = ((string * ptyp) list) list

let rec find_ptyp str_ptyps str1 = 
    match str_ptyps with
    | [] -> PTVar 0
    | (str, ptyp) :: str_ptyps' -> if str1 = str then ptyp else find_ptyp str_ptyps' str1

let rec type_of_var str tctx = 
    match tctx with
    | [] -> PTVar 0
    | str_ptyps :: tctx' -> 
        let pt = find_ptyp str_ptyps str in
        if pt = PTVar 0 then
            type_of_var str tctx'
        else 
            pt

let rec merge_env env1 env2 =
    match env1 with
    | [] -> env2
    | (pt1, pt2) :: env1' -> 
        let pts = Pairs.find_all env2 pt1 in 
        let rec find_ptyp pts pt = 
            match pts with
            | [] -> pt 
            | pt3::pts' -> begin
                    match pt,pt3 with
                    | PTVar i, PTVar j -> find_ptyp pts' (PTVar (min i j))
                    | _, PTVar j -> find_ptyp pts' pt
                    | PTVar i, _ -> find_ptyp pts' pt3
                    | _ ->  find_ptyp pts' pt
                end in
        (pt1, find_ptyp pts pt2)::(merge_env env1' (Pairs.remove_all env2 pt1))
        


let rec calculate_type pel env tctx modul moduls = 
    match pel.pexpr with
    | PSymbol str -> 
        try
            let pt = type_of_var str tctx in
            if pt = PTVar 0 then begin
                let m = Hashtbl.find moduls modul in
                try
                    match (Hashtbl.find m str) with
                    | (Val, PExpr_loc pel1) -> pel.ptyp <- pel1.ptyp; (env, tctx)
                    | (Var, PExpr_loc pel1) -> pel.ptyp <- pel1.ptyp; (env, tctx)
                    | _ -> raise (Undefined_idenfier (modul^"."^str))
                with Not_found -> raise (Undefined_idenfier (modul^"."^str))
            end else begin
                pel.ptyp <- pt;
                (env, tctx)
            end
        with Not_found -> raise (Undefined_modul modul)
    | PLocal_Val (str, pel1) | PLocal_Var (str, pel1) -> 
        let env1, tctx1 = calculate_type pel1 env tctx modul moduls in begin
            match tctx1 with
            | [] -> (env1, [[(str, pel1.ptyp)]])
            | c :: cs -> (env1, ((str, pel1.ptyp)::c) :: cs)
        end
    | PDot (pel1, pel2) -> 
        (*let rec calculate_dot pel3 pel4 = *)
        begin
            match pel1, pel2 with
            | PSymbol str1, PSymbol str2 -> 
                let fstchar = String.sub str1 0 1 in
                if fstchar = String.uppercase_ascii fstchar then begin (*str1 is module name*)
                    let env1, tctx1 = calculate_type pel2 env tctx str1 moduls in
                    pel.ptyp <- pel2.ptyp;
                    (env1, tctx1)
                end else begin
                    let _ = calculate_type pel1 env tctx modul moduls in
                    let pt = pel1.ptyp in begin
                        match pt with
                        | PTRecord str_pts -> 
                            let pt1 = find_ptyp str_pts str2 in
                            if pt1 = PTVar 0 then
                                raise (Invalid_pexpr_loc (pel1, "no binding of "^str2^" in the record."))
                            else begin
                                pel.ptyp <- pt1;
                                (env, tctx)
                            end
                        | _ -> raise (Invalid_pexpr_loc (pel1, "not a record."))
                    end
                end
            | _, PSymbol str2 ->
                let _ = calculate_type pel1 env tctx modul moduls in
                let pt = pel1.ptyp in begin
                    match pt with
                    | PTRecord str_pts -> 
                        let pt1 = find_ptyp str_pts str2 in
                        if pt1 = PTVar 0 then
                            raise (Invalid_pexpr_loc (pel1, "no binding of "^str2^" in the record."))
                        else begin
                            pel.ptyp <- pt1;
                            (env, tctx)
                        end
                    | _ -> raise (Invalid_pexpr_loc (pel1, "not a record."))
                end
            | _ -> raise (Invalid_pexpr_loc (pel,""))
        end
    | PInt i -> pel.ptyp <- PTInt; (env, tctx)
    | PFloat f -> pel.ptyp <- PTFloat; (env, tctx)
    | PUnt -> pel.ptyp <- PTUnt; (env, tctx)
    | PAray pel_aray -> 
        let env1 = unify (List.map (fun pel -> pel.ptyp) (Array.to_list pel_aray)) modul moduls in
        let new_env = merge_env env1 env in
        Array.iter (fun pel -> pel.ptyp <- apply_env_to_ptyp new_env pel.ptyp) pel_aray;
        pel.ptyp <- PTAray (pel_aray.(1).ptyp);
        (new_env, tctx)
    | PLst pel_list ->
        let env1 = unify (List.map (fun pel -> pel.ptyp) pel_list) modul moduls in
        let new_env = merge_env env1 env in
        List.iter (fun pel -> pel.ptyp <- apply_env_to_ptyp new_env pel.ptyp) pel_list;
        pel.ptyp <- PTAray ((List.hd pel_list).ptyp);
        (new_env, tctx)
    | PBool b -> pel.ptyp <- PTBool; (env, tctx)
    | PTuple pel_list -> 
        List.

    