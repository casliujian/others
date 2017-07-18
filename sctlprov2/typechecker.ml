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


let rec expand_udt pt modul moduls =
  match pt with
  | PTAray pt1 -> PTAray (expand_udt pt1 modul moduls)
  | PTLst pt1 -> PTLst (expand_udt pt1 modul moduls)
  | PTTuple pt_list -> PTTuple (List.map (fun pt->expand_udt pt modul moduls) pt_list)
  | PTRecord str_pt_list -> PTRecord (List.map (fun (str, pt) -> (str, expand_udt pt modul moduls)) str_pt_list)
  | PTArrow (pt1, pt2) -> PTArrow (expand_udt pt1 modul moduls, expand_udt pt2 modul moduls)
  | PTConstrs str_opt_list -> 
    PTConstrs (List.map (fun (str, opt) ->
        match opt with
        | None -> (str, None)
        | Some pt -> (str, Some (expand_udt pt modul moduls))
      ) str_opt_list) 
  | PTUdt (str, pt_list) -> 
    let pt1 = List.map (fun pt -> expand_udt pt modul moduls) pt_list in
    let strs = String.split_on_char '.' (String.trim str) in begin
      match strs with
      | [s] -> 
        if Hashtbl.mem moduls modul then begin
          let m = Hashtbl.find moduls modul in
          let tmp_pt = ref (PTVar 0) in
          if Hashtbl.mem m.psymbol_tbl s then begin
            match (Hashtbl.find m.psymbol_tbl s) with
            | (UDT, PTyp pt) -> 
              let index = ref 0 in
              List.iter (fun pt2 -> decr index; tmp_pt := replace_ptvar pt !index pt2) pt1
            | _ -> ()
          end;
          let index = ref 0 in
          while !tmp_pt = PTVar 0 && !index < List.length m.imported do
            (try 
               tmp_pt := expand_udt pt (List.nth m.imported !index) moduls
             with Invalid_typepath _ -> ());
            incr index
          done;
          if !tmp_pt = PTVar 0 then
            raise (Invalid_typepath s)
          else
            !tmp_pt
        end else begin
          raise (Undefined_modul modul)
        end
      | [s1;s2] -> expand_udt (PTUdt (s2, pt1)) s1 moduls
      | _ -> raise (Invalid_typepath str)
    end
  | _ -> pt


(* let rec ptyp_from_typepath strs modul moduls = 
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
        | [mname; ptname] -> find_ptyp_in_modul ptname mname *)

let rec apply_env_to_ptyp env ptyp = 
  let rec find_key_binding bindings key = 
    match bindings with
    | [] -> PTVar key
    | (PTVar ki, PTVar vi) :: bindings' -> if (key=ki)&&(ki>=vi) then PTVar vi else find_key_binding bindings' (key)
    | (PTVar ki, pt) :: bindings' -> if key=ki then pt else find_key_binding bindings' (key) 
    | _ :: bindings' -> find_key_binding bindings' (key)
  in
  match ptyp with
  | PTInt | PTFloat | PTBool | PTUnt | PTUdt _ -> ptyp
  | PTAray pt -> PTAray (apply_env_to_ptyp env pt)
  | PTLst pt -> PTLst (apply_env_to_ptyp env pt)
  | PTTuple pts -> PTTuple (List.map (fun a -> apply_env_to_ptyp env a) pts)
  | PTRecord str_ptyps -> PTRecord (List.map (fun (a,b) -> a, apply_env_to_ptyp env b) str_ptyps)
  | PTConstrs str_optyps -> 
    PTConstrs (List.map (fun (a, ob) -> 
        match ob with
        | None -> a, None
        | Some b -> a, Some (apply_env_to_ptyp env b)
      ) str_optyps)
  | PTVar vi -> find_key_binding env (vi)
  | PTArrow (pt1, pt2) -> PTArrow (pt1, pt2)

let rec unify ptyp_list modul moduls = 
  match ptyp_list with
  | [] -> []
  | [ptyp] -> []
  | ptyp1 :: ptyp2 :: ptyps -> begin
      match ptyp1, ptyp2 with
      | PTInt, PTInt | PTFloat, PTFloat | PTBool, PTBool | PTUnt, PTUnt -> unify (ptyp2:: ptyps) modul moduls
      | PTVar vi1, PTVar vi2 -> 
        if vi1 = vi2 then 
          unify (ptyp2::ptyps) modul moduls
        else if vi1 < vi2 then
          let env = [(PTVar vi2, PTVar vi1)] in
          merge_env env (unify (List.map (fun a -> apply_env_to_ptyp env a) (ptyp2::ptyps)) modul moduls)
        else 
          let env = [(PTVar vi1, PTVar vi2)] in
          merge_env env (unify (List.map (fun a -> apply_env_to_ptyp env a) (ptyp2::ptyps)) modul moduls)
      | pt, PTVar vi | PTVar vi, pt -> 
        let env = [(PTVar vi, pt)] in
        merge_env env (unify (List.map (fun a -> apply_env_to_ptyp env a) (ptyp2::ptyps)) modul moduls)
      | PTAray pt1, PTAray pt2 | PTLst pt1, PTLst pt2 -> 
        let env = unify [pt1;pt2] modul moduls in
        merge_env env (unify (List.map (fun a -> apply_env_to_ptyp env a) (ptyp2::ptyps)) modul moduls)
      | PTTuple pts1, PTTuple pts2 -> 
        if List.length pts1 <> List.length pts2 then
          raise (Unify_error (ptyp1, ptyp2))
        else
          let env = List.fold_left (fun e (a1,a2) -> merge_env (unify [a1;a2] modul moduls) e) [] (List.combine pts1 pts2) in
          merge_env env (unify (List.map (fun a -> apply_env_to_ptyp env a) (ptyp2::ptyps)) modul moduls)
      | PTRecord str_pt_list1, PTRecord str_pt_list2 ->
        if List.length str_pt_list1 <> List.length str_pt_list2 then
          raise (Unify_error (ptyp1, ptyp2))
        else 
          let rec find_ptyp str_pt_list str =
            match str_pt_list with
            | [] -> PTVar 0
            | (s,pt)::str_pt_list' -> if s = str then pt else find_ptyp str_pt_list' str in
          let env = List.fold_left (fun e (str, pt) ->
              let pt2 = find_ptyp str_pt_list2 str in
              if pt2 = PTVar 0 then 
                raise (Unify_error (ptyp1, ptyp2))
              else
                merge_env e (unify [pt;pt2] modul moduls)
            ) [] str_pt_list1 in
          merge_env env (unify (List.map (fun a -> apply_env_to_ptyp env a) (ptyp2::ptyps)) modul moduls)
      | PTConstrs str_opt_list1, PTConstrs str_opt_list2 -> 
        if List.length str_opt_list1 <> List.length str_opt_list2 then
          raise (Unify_error (ptyp1, ptyp2))
        else 
          let rec find_ptyp str_pt_list str =
            match str_pt_list with
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
          merge_env env (unify (List.map (fun a -> apply_env_to_ptyp env a) (ptyp2::ptyps)) modul moduls)
      | PTUdt (str, ptyp_list), _ -> 
        (*let strs = String.split_on_char '.' (String.trim str) in*)
        (* let ptyp = ptyp_from_typepath str modul moduls in
           let index = ref (0) in
           let ptyp_concrete = List.fold_left (fun pt pt1 -> descr index; replace_ptvar pt index pt1) ptyp ptyp_list in *)
        let ptyp_concrete = expand_udt ptyp1 modul moduls in
        unify (ptyp_concrete::ptyp2::ptyps) modul moduls
      | _, PTUdt (str, ptyp_list) ->
        (*let strs = String.split_on_char '.' (String.trim str) in*)
        (* let ptyp = ptyp_from_typepath str modul moduls in
           let index = ref (0) in
           let ptyp_concrete = List.fold_left (fun pt pt1 -> descr index; replace_ptvar pt index pt1) ptyp ptyp_list in *)
        let ptyp_concrete = expand_udt ptyp2 modul moduls in
        unify (ptyp1::ptyp_concrete::ptyps) modul moduls
      | PTArrow (pt1, pt2), PTArrow (pt3, pt4) ->
        let env1 = unify [pt1; pt3] modul moduls in
        let env2 = unify [pt2; pt4] modul moduls in
        merge_env (merge_env env1 env2) (unify (ptyp2::ptyps) modul moduls)
      | _ -> raise (Unify_error (ptyp1,ptyp2))
    end


let rec check_dep pmname pmoduls = 
  try
    let pm = Hashtbl.find pmoduls pmname in
    List.iter (fun a -> check_dep a pmoduls) pm.imported
  with Not_found -> 
    eprintf "Error: module %s is not defined.\n" pmname; 
    exit 1

(* let expand_udt_path moduls = 
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
    ) moduls *)

type type_context = (string * ptyp) list

let rec find_ptyp str_ptyps str1 = 
  match str_ptyps with
  | [] -> PTVar 0
  | (str, ptyp) :: str_ptyps' -> if str1 = str then ptyp else find_ptyp str_ptyps' str1

let rec type_of_var str tctx = 
  match tctx with
  | [] -> PTVar 0
  | (s, pt) :: tctx' -> 
    if s=str then pt else type_of_var str tctx'
(* let pt = find_ptyp str_ptyps str in
   if pt = PTVar 0 then
    type_of_var str tctx'
   else 
    pt *)
let add_to_tctx str pt tctx = (str, pt) :: tctx

let rec type_of_str str modul moduls =
  try
    let pt = ref (PTVar 0) in
    let m = Hashtbl.find moduls modul in
    if Hashtbl.mem m.psymbol_tbl str then begin
      match (Hashtbl.find m.psymbol_tbl str) with
      | (Val, PExpr_loc (ptyp, pel)) -> pt := ptyp
      | (Var, PExpr_loc (ptyp, pel)) -> pt := ptyp
      | (Function, PFunction (ptyp, _, _)) -> pt := ptyp
      | _ -> ()
    end else begin
      List.iter (fun mname ->
          if Hashtbl.mem moduls mname then
            let m1 = Hashtbl.find moduls mname in
            if Hashtbl.mem m1.psymbol_tbl str then begin
              match (Hashtbl.find m1.psymbol_tbl str) with
              | (Val, PExpr_loc (ptyp, pel)) -> pt := ptyp
              | (Var, PExpr_loc (ptyp, pel)) -> pt := ptyp
              | (Function, PFunction (ptyp, _, _)) -> pt := ptyp
              | _ -> ()
            end
            else
              raise (Undefined_modul mname)
        ) m.imported
    end;
    if !pt = PTVar 0 then
      raise (Undefined_idenfier str)
    else 
      !pt
  with Not_found -> raise (Undefined_modul modul)



(* let rec check_type pel tctx modul moduls = 
    match pel.pexpr with
    | PSymbol str ->
        try
            let pt = type_of_var str tctx in
            if pt = PTVar 0 then begin
                let m = Hashtbl.find moduls modul in
                try
                    match (Hashtbl.find m str) with
                    | (Val, PExpr_loc pel1) -> pel.ptyp <- pel1.ptyp; tctx
                    | (Var, PExpr_loc pel1) -> pel.ptyp <- pel1.ptyp; tctx
                    | _ -> raise (Undefined_idenfier (modul^"."^str))
                with Not_found -> raise (Undefined_idenfier (modul^"."^str))
            end else begin
                pel.ptyp <- pt;
                tctx
            end
        with Not_found -> raise (Undefined_modul modul) *)

let rec check_ppat_type ppatl modul moduls =
  match ppatl.ppat with
  | PPat_Symbol str -> ([], add_to_tctx str ppatl.ptyp [])
  | PPat_Int _ -> let env1 = unify [PTInt; ppatl.ptyp] modul moduls in (env1, [])
  | PPat_Float _ -> let env1 = unify [PTFloat; ppatl.ptyp] modul moduls in (env1, [])
  | PPat_Unt -> let env1 = unify [PTUnt; ppatl.ptyp] modul moduls in (env1, [])
  | PPat_Aray ppatls -> 
    let env0 = ref [] 
    and tctx0 = ref [] in
    List.iter (fun ppatl -> 
        let env,tctx = check_ppat_type ppatl modul moduls in
        env0 := merge_env env !env0;
        tctx0 := tctx @ !tctx0
      ) ppatls;
    begin
      match ppatls with
      | [] -> (!env0, !tctx0)
      | p::pl -> let env1 = unify [ppatl.ptyp; PTAray (p.ptyp)] modul moduls in (merge_env env1 !env0, !tctx0)
    end
  | PPat_Lst ppatls -> 
    let env0 = ref [] 
    and tctx0 = ref [] in
    List.iter (fun ppatl -> 
        let env,tctx = check_ppat_type ppatl modul moduls in
        env0 := merge_env env !env0;
        tctx0 := tctx @ !tctx0
      ) ppatls;
    begin
      match ppatls with
      | [] -> (!env0, !tctx0)
      | p::pl -> let env1 = unify [ppatl.ptyp; PTLst (p.ptyp)] modul moduls in (merge_env env1 !env0, !tctx0)
    end
  | PPat_Lst_Cons (ppatl1, ppatl2) ->
    let env1, tctx1 = check_ppat_type ppatl1 modul moduls in
    let env2, tctx2 = check_ppat_type ppatl2 modul moduls in
    let env3 = unify [ppatl.ptyp; PTLst (ppatl1.ptyp); ppatl2.ptyp] modul moduls in
    (merge_env (merge_env env3 env2) env1, tctx1 @ tctx2)
  | PPat_Underline -> ([], [])
  | PPat_Tuple ppatls ->
    let env0 = ref [] 
    and tctx0 = ref [] in
    List.iter (fun ppatl -> 
        let env,tctx = check_ppat_type ppatl modul moduls in
        env0 := merge_env env !env0;
        tctx0 := tctx @ !tctx0
      ) ppatls;
    let env1 = unify [ppatl.ptyp; PTTuple (List.map (fun p->p.ptyp) ppatls)] modul moduls in 
    (merge_env env1 !env0, !tctx0)
  | PPat_Constr (str, oppatl) -> begin
      match oppatl with
      | None -> ([], [])
      | Some ppatl1 ->
        let env1, tctx1 = check_ppat_type ppatl1 modul moduls in
        (env1, tctx1)
    end




let rec check_pel_type pel env tctx modul moduls = 
  match pel.pexpr with
  | PSymbol str -> 
    (try
       let pt = type_of_var str tctx in
       if pt = PTVar 0 then begin
         let m = Hashtbl.find moduls modul in
         try
           match (Hashtbl.find m.psymbol_tbl str) with
           | (Val, PExpr_loc (pt, pel1)) -> let env1 = unify [pt;pel.ptyp; pel1.ptyp] modul moduls in (merge_env env1 env, tctx)
           | (Var, PExpr_loc (pt, pel1)) -> let env1 = unify [pt;pel.ptyp; pel1.ptyp] modul moduls in (merge_env env1 env, tctx)
           | _ -> raise (Undefined_idenfier (modul^"."^str))
         with Not_found -> raise (Undefined_idenfier (modul^"."^str))
       end else begin
         let env1 = unify [pel.ptyp; pt] modul moduls in
         (merge_env env1 env, tctx)
       end
     with Not_found -> raise (Undefined_modul modul))
  | PLocal_Val (str, pel1) | PLocal_Var (str, pel1) -> 
    let env1, tctx1 = check_pel_type pel1 env tctx modul moduls in 
    (env1, add_to_tctx str pel1.ptyp tctx)
  (* begin
      match tctx1 with
      | [] -> (env1, [[(str, pel1.ptyp)]])
      | c :: cs -> (env1, ((str, pel1.ptyp)::c) :: cs)
     end *)
  | PDot (pel1, pel2) -> 
    (*let rec calculate_dot pel3 pel4 = *)
    begin
      match pel1.pexpr, pel2.pexpr with
      | PSymbol str1, PSymbol str2 -> 
        let fstchar = String.sub str1 0 1 in
        if fstchar = String.uppercase_ascii fstchar then begin (*str1 is module name*)
          let env1, tctx1 = check_pel_type pel2 env tctx str1 moduls in
          let env2 = unify [pel.ptyp; pel2.ptyp] str1 moduls in
          (merge_env env2 env1, tctx1)
        end else begin
          let env1, _ = check_pel_type pel1 env tctx modul moduls in
          let pt = pel1.ptyp in begin
            match pt with
            | PTRecord str_pts -> 
              let pt1 = find_ptyp str_pts str2 in
              if pt1 = PTVar 0 then
                raise (Invalid_pexpr_loc (pel1, "no binding of "^str2^" in the record."))
              else begin
                let env2 = unify [pel.ptyp; pt1; pel2.ptyp] modul moduls in
                (merge_env env2 (merge_env env1 env), tctx)
              end
            | _ -> raise (Invalid_pexpr_loc (pel1, "not a record."))
          end
        end
      | _, PSymbol str2 ->
        let _ = check_pel_type pel1 env tctx modul moduls in
        let pt = pel1.ptyp in begin
          match pt with
          | PTRecord str_pts -> 
            let pt1 = find_ptyp str_pts str2 in
            if pt1 = PTVar 0 then
              raise (Invalid_pexpr_loc (pel1, "no binding of "^str2^" in the record."))
            else begin
              let env1 = unify [pel.ptyp; pt1; pel2.ptyp] modul moduls in
              (merge_env env1 env, tctx)
            end
          | _ -> raise (Invalid_pexpr_loc (pel1, "not a record."))
        end
      | _ -> raise (Invalid_pexpr_loc (pel,""))
    end
  | PInt i -> let env1 = unify [pel.ptyp; PTInt] modul moduls in (merge_env env1 env, tctx)
  | PFloat f -> let env1 = unify [pel.ptyp; PTFloat] modul moduls in (merge_env env1 env, tctx)
  | PUnt -> let env1 = unify [pel.ptyp; PTUnt] modul moduls in (merge_env env1 env, tctx)
  | PAray pel_aray -> 
    let env1 = unify (List.map (fun (pel:pexpr_loc) -> pel.ptyp) (pel_aray)) modul moduls in
    let new_env = merge_env env1 env in
    List.iter (fun (pel:pexpr_loc) -> pel.ptyp <- apply_env_to_ptyp new_env pel.ptyp) pel_aray;
    let env2 = unify [pel.ptyp; PTAray ((List.hd pel_aray).ptyp)] modul moduls in 
    (merge_env env2 new_env, tctx)
  | PLst pel_list ->
    let env1 = unify (List.map (fun (pel:pexpr_loc) -> pel.ptyp) pel_list) modul moduls in
    let new_env = merge_env env1 env in
    List.iter (fun (pel:pexpr_loc) -> pel.ptyp <- apply_env_to_ptyp new_env pel.ptyp) pel_list;
    let env2 = unify [pel.ptyp; PTLst ((List.hd pel_list).ptyp)] modul moduls in 
    (merge_env env2 new_env, tctx)
  | PAray_Field (pel1, pel2) -> 
    let env1, _ = check_pel_type pel1 env tctx modul moduls  in
    let env2 = merge_env env1 env in
    let env3, _ = check_pel_type pel2 env2 tctx modul moduls in
    let env4 = unify [pel.ptyp; PTAray (pel2.ptyp)] modul moduls in
    (merge_env env4 env3, tctx)
  | PBool b -> let env1 = unify [pel.ptyp; PTBool] modul moduls in (merge_env env1 env, tctx)
  | PTuple pel_list -> 
    let env0 = ref env in
    List.iter (fun pel ->
        let env, _ = check_pel_type pel !env0 tctx modul moduls in
        env0 := env
      ) pel_list;
    let env1 = unify [pel.ptyp; PTTuple (List.map (fun (pel:pexpr_loc) -> pel.ptyp) pel_list)] modul moduls in
    (merge_env env1 !env0, tctx)
  | PRecord str_pels ->
    let env0 = ref env in
    List.iter (fun (str, pel) ->
        let env, _ = check_pel_type pel !env0 tctx modul moduls in
        env0 := env
      ) str_pels;
    let env1 = unify [pel.ptyp; PTRecord (List.map (fun (str, (pel:pexpr_loc)) ->
        (str, pel.ptyp)
      ) str_pels)] modul moduls in
    (merge_env env1 !env0, tctx)
  | PNegb pel1 -> 
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env2 = unify [PTBool; pel1.ptyp; pel.ptyp] modul moduls in
    (merge_env env2 env1, tctx)
  | PAndo (pel1, pel2) -> 
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env2, _ = check_pel_type pel2 env1 tctx modul moduls in
    let env3 = unify [PTBool; pel1.ptyp; pel2.ptyp; pel.ptyp] modul moduls in
    (merge_env env3 env2, tctx)
  | POro (pel1, pel2) -> 
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env2, _ = check_pel_type pel2 env1 tctx modul moduls in
    let env3 = unify [PTBool; pel1.ptyp; pel2.ptyp; pel.ptyp] modul moduls in
    (merge_env env3 env2, tctx)
  | PNegi pel1 ->
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env2 = unify [PTInt; pel1.ptyp; pel.ptyp] modul moduls in
    (merge_env env2 env1, tctx)
  | PNegf pel1 ->
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env2 = unify [PTFloat; pel1.ptyp; pel.ptyp] modul moduls in
    (merge_env env2 env1, tctx)
  | PAdd (pel1, pel2) ->
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env2, _ = check_pel_type pel2 env1 tctx modul moduls in
    let env3 = unify [PTInt; pel1.ptyp; pel2.ptyp; pel.ptyp] modul moduls in
    (merge_env env3 env2, tctx)
  | PAddDot(pel1, pel2) ->
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env2, _ = check_pel_type pel2 env1 tctx modul moduls in
    let env3 = unify [PTFloat; pel1.ptyp; pel2.ptyp; pel.ptyp] modul moduls in
    (merge_env env3 env2, tctx)
  | PMinus (pel1, pel2) ->
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env2, _ = check_pel_type pel2 env1 tctx modul moduls in
    let env3 = unify [PTInt; pel1.ptyp; pel2.ptyp; pel.ptyp] modul moduls in
    (merge_env env3 env2, tctx)
  | PMinusDot (pel1, pel2) ->
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env2, _ = check_pel_type pel2 env1 tctx modul moduls in
    let env3 = unify [PTFloat; pel1.ptyp; pel2.ptyp; pel.ptyp] modul moduls in
    (merge_env env3 env2, tctx)
  | PMult (pel1, pel2) ->
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env2, _ = check_pel_type pel2 env1 tctx modul moduls in
    let env3 = unify [PTInt; pel1.ptyp; pel2.ptyp; pel.ptyp] modul moduls in
    (merge_env env3 env2, tctx)
  | PMultDot (pel1, pel2) ->
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env2, _ = check_pel_type pel2 env1 tctx modul moduls in
    let env3 = unify [PTFloat; pel1.ptyp; pel2.ptyp; pel.ptyp] modul moduls in
    (merge_env env3 env2, tctx)
  | PEqual (pel1, pel2) ->
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env2, _ = check_pel_type pel2 env1 tctx modul moduls in
    let env3 = unify [pel1.ptyp; pel2.ptyp] modul moduls in
    let env4 = unify [PTBool; pel.ptyp] modul moduls in
    (merge_env (merge_env env3 env4) env2, tctx)
  | PNon_Equal (pel1, pel2) ->
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env2, _ = check_pel_type pel2 env1 tctx modul moduls in
    let env3 = unify [pel1.ptyp; pel2.ptyp] modul moduls in
    let env4 = unify [PTBool; pel.ptyp] modul moduls in
    (merge_env (merge_env env3 env4) env2, tctx)
  | PLT (pel1, pel2) ->
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env2, _ = check_pel_type pel2 env1 tctx modul moduls in
    let env3 = unify [pel1.ptyp; pel2.ptyp] modul moduls in
    let env4 = unify [PTBool; pel.ptyp] modul moduls in
    (merge_env (merge_env env3 env4) env2, tctx)
  | PGT (pel1, pel2) ->
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env2, _ = check_pel_type pel2 env1 tctx modul moduls in
    let env3 = unify [pel1.ptyp; pel2.ptyp] modul moduls in
    let env4 = unify [PTBool; pel.ptyp] modul moduls in
    (merge_env (merge_env env3 env4) env2, tctx)
  | PLE (pel1, pel2) ->
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env2, _ = check_pel_type pel2 env1 tctx modul moduls in
    let env3 = unify [pel1.ptyp; pel2.ptyp] modul moduls in
    let env4 = unify [PTBool; pel.ptyp] modul moduls in
    (merge_env (merge_env env3 env4) env2, tctx)
  | PGE (pel1, pel2) ->
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env2, _ = check_pel_type pel2 env1 tctx modul moduls in
    let env3 = unify [pel1.ptyp; pel2.ptyp] modul moduls in
    let env4 = unify [PTBool; pel.ptyp] modul moduls in
    (merge_env (merge_env env3 env4) env2, tctx)
  | PIF (pel1, pel2, opel3) -> begin
      match opel3 with
      | None ->
        let env1,_ = check_pel_type pel1 env tctx modul moduls in
        let env2,_ = check_pel_type pel2 env1 tctx modul moduls in
        let env3 = unify [PTBool; pel1.ptyp] modul moduls in
        let env4 = unify [PTUnt; pel.ptyp; pel2.ptyp] modul moduls in
        (merge_env (merge_env env3 env4) env2, tctx)
      | Some pel3 ->
        let env1, _ = check_pel_type pel1 env tctx modul moduls in
        let env2, _ = check_pel_type pel2 env1 tctx modul moduls in
        let env3, _ = check_pel_type pel3 env2 tctx modul moduls in
        let env4 = unify [PTBool; pel1.ptyp] modul moduls in
        let env5 = unify [pel.ptyp; pel2.ptyp; pel3.ptyp] modul moduls in
        (merge_env (merge_env env4 env5) env3, tctx)
    end
  | PWhile (pel1, pel2) ->
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env2, _ = check_pel_type pel2 env1 tctx modul moduls in
    let env3 = unify [PTBool; pel1.ptyp] modul moduls in
    let env4 = unify [PTUnt; pel.ptyp; pel2.ptyp] modul moduls in
    (merge_env (merge_env env3 env4) env2, tctx)
  | PFor (str, pel1, pel2, pel3) ->
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env2, _ = check_pel_type pel2 env1 tctx modul moduls in
    let env3, _ = check_pel_type pel3 env2 (add_to_tctx str pel1.ptyp tctx) modul moduls in
    let env4 = unify [PTInt; pel1.ptyp; pel2.ptyp] modul moduls in
    let env5 = unify [pel3.ptyp; pel.ptyp] modul moduls in
    (merge_env (merge_env env5 env4) env3, tctx)
  | PSeq pels ->
    let env0 = ref env
    and tctx0 = ref tctx in
    List.iter (fun pel -> 
        let env, tctx = check_pel_type pel !env0 !tctx0 modul moduls in
        env0 := env;
        tctx0 := tctx
      ) pels;
    let env1 = unify [pel.ptyp; (List.hd (List.rev pels)).ptyp] modul moduls in
    (merge_env env1 !env0, tctx)
  | PAssign (pel1, pel2) ->
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env2, _ = check_pel_type pel2 env1 tctx modul moduls in
    let env3 = unify [pel1.ptyp; pel2.ptyp] modul moduls in
    let env4 = unify [PTUnt; pel.ptyp] modul moduls in
    (merge_env (merge_env env3 env4) env2, tctx)
  | PMatch (pel1, ppatl_pel_list) ->
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env0 = ref env1 in
    List.iter (fun (ppatl1, pel2) ->
        let env2, tctx2 = check_ppat_type ppatl1 modul moduls in
        env0 := merge_env env2 !env0;
        let env3, _ = check_pel_type pel2 !env0 (tctx2@tctx) modul moduls in
        let env4 = unify [pel1.ptyp; ppatl1.ptyp] modul moduls in
        let env5 = unify [pel.ptyp; pel2.ptyp] modul moduls in
        env0 := merge_env (merge_env env4 env5) env3
      ) ppatl_pel_list;
    (!env0, tctx)
  | PWith (pel1, str_pel_list) ->
    let env1, _ = check_pel_type pel1 env tctx modul moduls in
    let env0 = ref env1 in
    List.iter (fun (str, pel2) ->
        let env,_ = check_pel_type pel2 !env0 tctx modul moduls in
        env0 := env
      ) str_pel_list;
    (!env0, tctx)
  | PConstr _ -> (env, tctx)
  | PApply (str, pel_list) -> 
    let ptf = type_of_str str modul moduls in
    let env0 = ref env in
    List.iter (fun pel ->
        let env, _ = check_pel_type pel !env0 tctx modul moduls in
        env0 := env
      ) pel_list;
    let pt1 = ref pel.ptyp in
    let rec construct_apply ptyps = 
      match ptyps with
      | [] -> raise (Invalid_pexpr_loc (pel, "not enough argument(s)."))
      | [pt] -> pt1 := pt; pt
      | pt::pts -> PTArrow (pt, construct_apply pts) in
    let env1 = unify [pel.ptyp; !pt1] modul moduls in
    let env2 = unify [ptf; construct_apply (List.map (fun (pel:pexpr_loc)->pel.ptyp) pel_list)] modul moduls in
    (merge_env (merge_env env1 env2) !env0, tctx)


let rec apply_env_to_ppatl env ppatl = 
  ppatl.ptyp <- apply_env_to_ptyp env ppatl.ptyp;
  match ppatl.ppat with
  | PPat_Aray ppatl1 -> List.iter (fun ppatl -> apply_env_to_ppatl env ppatl) ppatl1
  | PPat_Lst ppatl1 -> List.iter (fun ppatl -> apply_env_to_ppatl env ppatl) ppatl1
  | PPat_Lst_Cons (ppatl1, ppatl2) -> apply_env_to_ppatl env ppatl1; apply_env_to_ppatl env ppatl2
  | PPat_Tuple ppatl_list -> List.iter (fun ppatl -> apply_env_to_ppatl env ppatl) ppatl_list
  | PPat_Constr (str, oppatl) -> begin
      match oppatl with
      | None -> ()
      | Some ppatl1 -> apply_env_to_ppatl env ppatl1
    end
  | _ -> ()

let rec apply_env_to_pel env (pel:pexpr_loc) = 
  pel.ptyp <- apply_env_to_ptyp env pel.ptyp;
  match pel.pexpr with
  | PLocal_Val (_, pel1) -> apply_env_to_pel env pel1
  | PLocal_Var (_, pel1) -> apply_env_to_pel env pel1
  | PDot (pel1, pel2) -> apply_env_to_pel env pel1; apply_env_to_pel env pel2
  | PAray (pel_list) -> List.iter (fun pel->apply_env_to_pel env pel) pel_list
  | PLst (pel_list) -> List.iter (fun pel->apply_env_to_pel env pel) pel_list
  | PAray_Field (pel1, pel2) -> apply_env_to_pel env pel1; apply_env_to_pel env pel2
  | PTuple pel_list -> List.iter (fun pel->apply_env_to_pel env pel) pel_list
  | PRecord str_pel_list -> List.iter (fun (str,pel) -> apply_env_to_pel env pel) str_pel_list
  | PNegb pel1 -> apply_env_to_pel env pel1
  | PNegi pel1 -> apply_env_to_pel env pel1
  | PNegf pel1 -> apply_env_to_pel env pel1
  | PAndo (pel1, pel2) -> apply_env_to_pel env pel1; apply_env_to_pel env pel2
  | POro (pel1, pel2) -> apply_env_to_pel env pel1; apply_env_to_pel env pel2
  | PAdd (pel1, pel2) -> apply_env_to_pel env pel1; apply_env_to_pel env pel2
  | PAddDot (pel1, pel2) -> apply_env_to_pel env pel1; apply_env_to_pel env pel2
  | PMinus (pel1, pel2) -> apply_env_to_pel env pel1; apply_env_to_pel env pel2
  | PMinusDot (pel1, pel2) -> apply_env_to_pel env pel1; apply_env_to_pel env pel2
  | PMult (pel1, pel2) -> apply_env_to_pel env pel1; apply_env_to_pel env pel2
  | PMultDot (pel1, pel2) -> apply_env_to_pel env pel1; apply_env_to_pel env pel2
  | PEqual (pel1, pel2) -> apply_env_to_pel env pel1; apply_env_to_pel env pel2
  | PNon_Equal (pel1, pel2) -> apply_env_to_pel env pel1; apply_env_to_pel env pel2
  | PLT (pel1, pel2) -> apply_env_to_pel env pel1; apply_env_to_pel env pel2
  | PGT (pel1, pel2) -> apply_env_to_pel env pel1; apply_env_to_pel env pel2
  | PLE (pel1, pel2) -> apply_env_to_pel env pel1; apply_env_to_pel env pel2
  | PGE (pel1, pel2) -> apply_env_to_pel env pel1; apply_env_to_pel env pel2
  | PIF (pel1, pel2, opel3) -> begin
      match opel3 with
      | None -> apply_env_to_pel env pel1; apply_env_to_pel env pel2
      | Some pel3 -> apply_env_to_pel env pel1; apply_env_to_pel env pel2; apply_env_to_pel env pel3
    end
  | PWhile (pel1, pel2) -> apply_env_to_pel env pel1; apply_env_to_pel env pel2
  | PFor (str, pel1, pel2, pel3) -> apply_env_to_pel env pel1; apply_env_to_pel env pel2; apply_env_to_pel env pel3
  | PSeq pel_list -> List.iter (fun pel->apply_env_to_pel env pel) pel_list
  | PAssign (pel1, pel2) -> apply_env_to_pel env pel1; apply_env_to_pel env pel2
  | PMatch (pel1, ppatl_pel_list) -> 
    apply_env_to_pel env pel1; 
    List.iter (fun (ppatl, pel) -> apply_env_to_pel env pel; apply_env_to_ppatl env ppatl) ppatl_pel_list
  | PWith (pel1, str_pel_list) ->
    apply_env_to_pel env pel1; 
    List.iter (fun (str, pel) -> apply_env_to_pel env pel) str_pel_list
  | PConstr (PConstr_compound (str, pel1)) -> apply_env_to_pel env pel1
  | PApply (str, pel_list) -> List.iter (fun pel -> apply_env_to_pel env pel) pel_list
  | _ -> ()
