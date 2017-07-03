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

let rec unify ptyp_list = 
    match ptyp_list with
    | [] | [ptyp] -> []
    | ptyp1 :: ptyp2 :: ptyps -> begin
            match ptyp1, ptyp2 with
            | PTInt, PTInt | 
        end
let rec apply_env_to_ptyp env ptyp = 
    match ptyp with
    | PTInt | PTFloat | PTBool | PTUnt | PTUdt _ -> ptyp
    | PTAray pt -> PTAray (apply_env_to_ptyp env pt)
    | PTLst pt -> PTLst (apply_env_to_ptyp env pt)
    | PTTuple pts -> PTTuple (List.map (fun a -> apply_env_to_ptyp env a) pts)
    | PRecord 

let rec check_dep pmname pmoduls = 
    try
        let pm = Hashtbl.find pmoduls pmname in
        List.iter (fun a -> check_dep a pmoduls) pm.imported
    with Not_found -> 
            eprintf "Error: module %s is not defined.\n" pmname; 
            exit 1

let rec check_type 