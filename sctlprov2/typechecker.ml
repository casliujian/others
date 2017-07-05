open Ast
open Printf

exception Unify_error of ptyp * ptyp

(*type dep = string * ((dep list) option)

let rec dep_of_pmodul pmname pmoduls = 
    try
        let pm = Hashtbl.find pmname in
        match pm.imported with
        | [] -> pmname, None
        | impts -> pmname, Some (List.map (fun a -> dep_of_pmodul a pmoduls) impts)
    with Not_found -> eprintf "Error: module %s is not defined.\n" pmname; exit 1*)

type env = (ptyp * ptyp) list

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

let rec unify ptyp_list = 
    match ptyp_list with
    | [] | [ptyp] -> []
    | ptyp1 :: ptyp2 :: ptyps -> begin
            match ptyp1, ptyp2 with
            | PTInt, PTInt | PTFloat, PTFloat | PTBool, PTBool | PTUnt, PTUnt -> unify (ptyp2:: ptyps)
            | PTVar vi1, PTVar vi2 -> 
                if vi1 = vi2 then 
                    unify (ptyp2::ptyps)
                else if vi1 > vi2 then
                    let env = [(PTVar vi2, PTVar vi1)] in
                    env @ (unify (List.map (fun a -> apply_env_to_ptyp env a) (ptyp2::ptyps)))
                else 
                    let env = [(PTVar vi1, PTVar vi2)] in
                    env @ (unify (List.map (fun a -> apply_env_to_ptyp env a) (ptyp2::ptyps)))
            | pt, PTVar vi | PTVar vi, pt -> 
                let env = [(PTVar vi, pt)] in
                env @ (unify (List.map (fun a -> apply_env_to_ptyp env a) (ptyp2::ptyps)))
            | PTAray pt1, PTAray pt2 | PTLst pt1, PTLst pt2 -> 
                let env = unify [pt1;pt2] in
                env @ (unify (List.map (fun a -> apply_env_to_ptyp env a) (ptyp2::ptyps)))
            | PTTuple pts1, PTTuple pts2 -> 
                if List.length pts1 <> List.length pts2 then
                    raise (Unify_error (ptyp1, ptyp2))
                else
                    let env = List.fold_left (fun e (a1,a2) -> (unify [a1;a2]) @ e) [] (List.combine pts1 pts2) in
                    env @ (unify (List.map (fun a -> apply_env_to_ptyp env a) (ptyp2::ptyps)))
            | PTRecord str_pt_list1, PTRecord str_pt_list2 ->
                
        end


let rec check_dep pmname pmoduls = 
    try
        let pm = Hashtbl.find pmoduls pmname in
        List.iter (fun a -> check_dep a pmoduls) pm.imported
    with Not_found -> 
            eprintf "Error: module %s is not defined.\n" pmname; 
            exit 1

let rec check_type 