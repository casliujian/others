open Expr
open Ast

type formula = 
	  Top
	| Bottom
	| Atomic of string * (expr list)
	| Neg of formula
	| And of formula * formula
	| Or of formula * formula
	| AX of string * formula * expr
	| EX of string * formula * expr
	| AF of string * formula * expr
	| EG of string * formula * expr
	| AR of string * string * formula * formula * expr
	| EU of string * string * formula * formula * expr

let rec pfmll_to_fml pfmll = 
  match pfmll.pfml with
  | PTop -> Top 
  | PBottom -> Bottom
  | PAtomic (str, pels) -> Atomic (str, List.map (fun pel -> pexprl_to_expr pel) pels)
  | PNeg pfml1 -> Neg (pfmll_to_fml pfml1)
  | PAnd (pfml1, pfml2) -> And (pfmll_to_fml pfml1, pfmll_to_fml pfml2)
  | POr (pfml1, pfml2) -> Or (pfmll_to_fml pfml1, pfmll_to_fml pfml2)
  | PAX (str, pfml1, pel1) -> AX (str, pfmll_to_fml pfml1, pexprl_to_expr pel1)
  | PEX (str, pfml1, pel1) -> EX (str, pfmll_to_fml pfml1, pexprl_to_expr pel1)
  | PAF (str, pfml1, pel1) -> AF (str, pfmll_to_fml pfml1, pexprl_to_expr pel1)
  | PEG (str, pfml1, pel1) -> EG (str, pfmll_to_fml pfml1, pexprl_to_expr pel1)
  | PAR (str1, str2, pfml1, pfml2, pel1) -> AR (str1, str2, pfmll_to_fml pfml1, pfmll_to_fml pfml2, pexprl_to_expr pel1)
  | PEU (str1, str2, pfml1, pfml2, pel1) -> EU (str1, str2, pfmll_to_fml pfml1, pfmll_to_fml pfml2, pexprl_to_expr pel1)

let rec subst_s fml str expr = 
	match fml with
	| Top | Bottom -> fml
	| Atomic (s, es) -> 
		let rec replace_symbol es str expr =
			match es with
			| [] -> []
			| (Symbol [s]) :: es' -> if str = s then expr :: (replace_symbol es' str expr) else (Symbol [s]) :: (replace_symbol es' str expr) 
			| e :: es' ->  e :: (replace_symbol es' str expr) in
		Atomic (str, replace_symbol es str expr)
	| Neg fml1 -> Neg (subst_s fml1 str expr)
	| And (fml1, fml2) -> And (subst_s fml1 str expr, subst_s fml2 str expr)
	| Or (fml1, fml2) -> Or (subst_s fml1 str expr, subst_s fml2 str expr)
	| AX (s1, fml1, e1) -> AX (s1, (if s1 = str then fml1 else subst_s fml1 str expr), (if e1 = Symbol [str] then expr else e1))
	| EX (s1, fml1, e1) -> EX (s1, (if s1 = str then fml1 else subst_s fml1 str expr), (if e1 = Symbol [str] then expr else e1))
	| AF (s1, fml1, e1) -> AF (s1, (if s1 = str then fml1 else subst_s fml1 str expr), (if e1 = Symbol [str] then expr else e1))
	| EG (s1, fml1, e1) -> EG (s1, (if s1 = str then fml1 else subst_s fml1 str expr), (if e1 = Symbol [str] then expr else e1))
	| AR (s1, s2, fml1, fml2, e1) -> AR (s1, s2, (if s1 = str then fml1 else subst_s fml1 str expr), (if s2 = str then fml2 else subst_s fml2 str expr), (if e1 = Symbol [str] then expr else e1))
	| EU (s1, s2, fml1, fml2, e1) -> EU (s1, s2, (if s1 = str then fml1 else subst_s fml1 str expr), (if s2 = str then fml2 else subst_s fml2 str expr), (if e1 = Symbol [str] then expr else e1))

(* negative normal form *)
let rec nnf fml = 
	match fml with
	| Top | Bottom | Atomic _ -> fml
	| Neg fml1 -> neg (nnf fml1)
	| And (fml1, fml2) -> And (nnf fml1, nnf fml2)
	| Or (fml1, fml2) -> Or (nnf fml1, nnf fml2)
	| AX (s, fml1, s') -> AX (s, nnf fml1, s')
	| EX (s, fml1, s') -> EX (s, nnf fml1, s')
	| AF (s, fml1, s') -> AF (s, nnf fml1, s')
	| EG (s, fml1, s') -> EG (s, nnf fml1, s')
	| AR (s, s', fml1, fml2, s'') -> AR (s, s', nnf fml1, nnf fml2, s'')
	| EU (s, s', fml1, fml2, s'') -> EU (s, s', nnf fml1, nnf fml2, s'')
and neg fml = 
	match fml with
	| Top -> Bottom
	| Bottom -> Top
	| Atomic _ -> Neg fml
	| Neg (Atomic (e, sl)) -> Atomic (e, sl)
	| Neg fml1 -> fml1
	| And (fml1, fml2) -> Or (neg fml1, neg fml2)
	| Or (fml1, fml2) -> And (neg fml1, neg fml2)
	| AX (s, fml1, s') -> EX (s, neg fml1, s')
	| EX (s, fml1, s') -> AX (s, neg fml1, s')
	| AF (s, fml1, s') -> EG (s, neg fml1, s')
	| EG (s, fml1, s') -> AF (s, neg fml1, s')
	| AR (s, s', fml1, fml2, s'') -> EU (s, s', neg fml1, neg fml2, s'')
	| EU (s, s', fml1, fml2, s'') -> AR (s, s', neg fml1, neg fml2, s'')

(* formula to string *)
let rec str_fml fml = 
	match fml with
	| Top -> "TRUE"
	| Bottom -> "FALSE"
	| Atomic (e, sl) -> (e) ^ (str_expr_list sl)
	| Neg fml1 -> "(not " ^ (str_fml fml1) ^ ")"
	| And (fml1, fml2) -> (str_fml fml1) ^ "/\\" ^ (str_fml fml2)
	| Or (fml1, fml2) -> (str_fml fml1) ^ "\\/" ^ (str_fml fml2)
	| AX (s, fml1, s') -> "AX(" ^ (s) ^ ", (" ^ (str_fml fml1) ^ "), " ^ (str_expr s') ^ ")"
	| EX (s, fml1, s') -> "EX(" ^ (s) ^ ", (" ^ (str_fml fml1) ^ "), " ^ (str_expr s') ^ ")"
	| AF (s, fml1, s') -> "AF(" ^ (s) ^ ", (" ^ (str_fml fml1) ^ "), " ^ (str_expr s') ^ ")"
	| EG (s, fml1, s') -> "EG(" ^ (s) ^ ", (" ^ (str_fml fml1) ^ "), " ^ (str_expr s') ^ ")"
	| AR (s, s', fml1, fml2, s'') -> "AR(" ^ (s) ^ ", " ^ (s') ^ ", (" ^ (str_fml fml1) ^ "), (" ^ (str_fml fml2) ^ "), " ^ (str_expr s'') ^ ")"
	| EU (s, s', fml1, fml2, s'') -> "EU(" ^ (s) ^ ", " ^ (s') ^ ", (" ^ (str_fml fml1) ^ "), (" ^ (str_fml fml2) ^ "), " ^ (str_expr s'') ^ ")"


let rec sub_fmls fml levl =
	let fml_levl_tbl = Hashtbl.create 10 
	and add_tbl tbl1 tbl2 = 
	Hashtbl.iter (fun a b -> Hashtbl.add tbl1 a b) tbl2 in
	Hashtbl.add fml_levl_tbl levl fml;
	match fml with
	(*| Atomic _ -> Hashtbl.add fml_levl_tbl levl *)
	(*| Neg fml1 -> add_tbl fml_levl_tbl (sub_fmls fml1 (levl^"1")); fml_levl_tbl*)
	| And (fml1, fml2) -> add_tbl fml_levl_tbl (sub_fmls fml1 (levl^"1")); add_tbl fml_levl_tbl (sub_fmls fml2 (levl^"2")); fml_levl_tbl
	| Or (fml1, fml2) -> add_tbl fml_levl_tbl (sub_fmls fml1 (levl^"1")); add_tbl fml_levl_tbl (sub_fmls fml2 (levl^"2")); fml_levl_tbl
	| AX (s, fml1, s') -> add_tbl fml_levl_tbl (sub_fmls fml1 (levl^"1")); fml_levl_tbl
	| EX (s, fml1, s') -> add_tbl fml_levl_tbl (sub_fmls fml1 (levl^"1")); fml_levl_tbl
	| AF (s, fml1, s') -> add_tbl fml_levl_tbl (sub_fmls fml1 (levl^"1")); fml_levl_tbl
	| EG (s, fml1, s') -> add_tbl fml_levl_tbl (sub_fmls fml1 (levl^"1")); fml_levl_tbl
	| AR (s, s', fml1, fml2, s'') -> add_tbl fml_levl_tbl (sub_fmls fml1 (levl^"1")); add_tbl fml_levl_tbl (sub_fmls fml2 (levl^"2")); fml_levl_tbl
	| EU (s, s', fml1, fml2, s'') -> add_tbl fml_levl_tbl (sub_fmls fml1 (levl^"1")); add_tbl fml_levl_tbl (sub_fmls fml2 (levl^"2")); fml_levl_tbl
	| _ -> fml_levl_tbl

let select_sub_fmls fml_levl_tbl = 
	let filter fml  = 
	(match fml with
	| AF _ -> true
	| EG _ -> true
	| AR _ -> true
	| EU _ -> true
	| AX _ -> true
	| EX _ -> true
	| Atomic _ -> true
	| Neg (Atomic _) -> true
	| _ -> false) in
	Hashtbl.iter (fun a b -> if (filter b = false) then Hashtbl.remove fml_levl_tbl a else ()) fml_levl_tbl; fml_levl_tbl