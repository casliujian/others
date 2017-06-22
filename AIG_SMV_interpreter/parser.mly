%{
open Lexing

type expr = 
	  Value of bool
	| Var of string
	| Not of expr
	| And of expr * expr
	| Or of expr * expr
	| IVar of int


let state_vars = ref []
let var_tbl = Hashtbl.create 100
let inits = ref []
let nexts = ref []
let defines = Hashtbl.create 100
let spec = ref (Var "")

let create_var_tbl () = 
	let tmp_index = ref 0 in
	List.iter (fun a -> Hashtbl.add var_tbl a !tmp_index; incr tmp_index) !state_vars

let rec str_expr expr = 
	match expr with
	| Value b -> string_of_bool b
	| Var s -> s
	| Not e -> "(not "^(str_expr e) ^")"
	| And (e1, e2) -> "("^ (str_expr e1) ^ " && " ^ (str_expr e2) ^ ")"
	| Or (e1, e2) -> "(" ^ (str_expr e1) ^ " || " ^ (str_expr e2) ^ ")"
	| IVar i -> "var "^(string_of_int i)
(*
let rec find_define s defines = 
	match defines with
	| [] -> Var ""
	| (a, b) :: ds -> if s = a then b else find_define s ds 
*)

let rec expand_symbol s = 
	match s with
	| Value b -> s
	| Var v -> (try expand_symbol (Hashtbl.find defines v) with Not_found -> s)
	| Not e -> Not (expand_symbol e)
	| And (e1, e2) -> And (expand_symbol e1, expand_symbol e2)
	| Or (e1, e2) -> Or (expand_symbol e1, expand_symbol e2)
	| _ -> s

let rec symbol_to_i s = 
	match s with
	| Value b -> s
	| Var v -> (try IVar (Hashtbl.find var_tbl v) with Not_found -> s)
	| Not e -> Not (symbol_to_i e)
	| And (e1, e2) -> And (symbol_to_i e1, symbol_to_i e2)
	| Or (e1, e2) -> Or (symbol_to_i e1, symbol_to_i e2)
	| _ -> s

let rec expand_defines () = 
	Hashtbl.iter (fun a b -> Hashtbl.replace defines a (symbol_to_i (expand_symbol b))) defines

let rec expand_nexts () = 
	let tmp_nexts = ref [] in
	tmp_nexts := List.map (fun (a, b) -> (a, (symbol_to_i (expand_symbol b)))) !nexts;
	nexts := !tmp_nexts

let rec nnf e =
	match e with
	| Not (Not e1) -> nnf e1
	| Not (And (e1, e2)) -> Or (nnf (Not e1), nnf (Not e2))
	| Not (Or (e1, e2)) ->And (nnf (Not e1), nnf (Not e2))
	| Not (Value b) -> Value (not b)
	| And (e1, e2) -> And (nnf e1, nnf e2)
	| Or (e1, e2) -> Or (nnf e1, nnf e2)
	| _ -> e

let rec print_cnf e = 
	let cnf_tbl = Hashtbl.create 100 in
	let rec get_cnf oe = 
	match oe with
	| And (e1, e2) -> get_cnf e1; get_cnf e2
	| Not (Not (And (e1, e2))) -> get_cnf e1; get_cnf e2
	| _ -> Hashtbl.replace cnf_tbl oe () in
	get_cnf e;
	Hashtbl.iter (fun a b -> print_endline ("\t"^(str_expr a))) cnf_tbl

let rec print_dnf e = 
	let cnf_tbl = Hashtbl.create 100 in
	let rec get_cnf oe = 
	match oe with
	| Or (e1, e2) -> get_cnf e1; get_cnf e2
(*	| Not (Not (And (e1, e2))) -> get_cnf e1; get_cnf e2 *)
	| _ -> Hashtbl.replace cnf_tbl oe () in
	get_cnf e;
	Hashtbl.iter (fun a b -> print_endline ("\t"^(str_expr a))) cnf_tbl


let rec construct_bdd e man =
	match e with
	| Value true -> MLBDD.dtrue man
	| Value false -> MLBDD.dfalse man
	| IVar i -> MLBDD.ithvar man i
	| And (e1, e2) -> MLBDD.dand (construct_bdd e1 man) (construct_bdd e2 man)
	| Or (e1, e2) -> MLBDD.dor (construct_bdd e1 man) (construct_bdd e2 man)
	| Not e1 -> MLBDD.dnot (construct_bdd e1 man)
	| _ -> MLBDD.dfalse man

%}

%token MODULE MAIN VAR ASSIGN DEFINE INIT NEXT SPEC
%token COLON SEMICOLON ASSIGNO EXCLAM LB1 RB1 TRUE FALSE AND OR AG
%token File_end
%token <string>ID

%left OR
%left AND
%left EXCLAM


%start input
%type <unit>input

%%
input: inputs File_end	{$1}
;

inputs: MODULE MAIN VAR vars ASSIGN assigns DEFINE defines SPEC spec	
				{
					print_endline "state variables:";
					List.iter (fun a -> print_endline a) !state_vars;
					create_var_tbl ();
					print_endline "init values:";
					List.iter (fun (a,b) -> print_string a; print_string " := "; print_endline (str_expr b)) !inits;
					print_endline "nexts:";
					expand_nexts ();
					List.iter (fun (a,b) -> print_string a; print_string " := "; print_endline (str_expr b)) !nexts;
					(*print_endline "defines:";
					expand_defines ();
					Hashtbl.iter (fun a b -> print_string a; print_string " := "; print_endline (str_expr b)) defines;*)
					print_endline "spec:";
					spec := nnf (symbol_to_i (expand_symbol !spec));
					print_endline ("AG "^ (str_expr !spec));
					print_endline ("There are "^(string_of_int (List.length !state_vars))^" state variables.");
					print_dnf !spec
					(*let man = MLBDD.init () in
					print_endline (MLBDD.to_string (construct_bdd !spec man))*)

				}
;

vars:
	| ID COLON ID SEMICOLON	vars {state_vars := $1 :: !state_vars}
	| ID COLON ID SEMICOLON	{state_vars := $1 :: !state_vars}
;

expr: TRUE	{Value true}
	| FALSE	{Value false}
	| ID		{Var $1}
	| EXCLAM expr	{Not $2}
	| expr AND expr	{And ($1, $3)}
	| expr OR expr	{Or ($1, $3)}
;

assigns:
	| INIT LB1 ID RB1 ASSIGNO expr SEMICOLON assigns	{inits := ($3, $6)::!inits}
	| NEXT LB1 ID RB1 ASSIGNO expr SEMICOLON assigns	{nexts := ($3, $6)::!nexts}
	| INIT LB1 ID RB1 ASSIGNO expr SEMICOLON	{inits := ($3, $6)::!inits}
	| NEXT LB1 ID RB1 ASSIGNO expr SEMICOLON	{nexts := ($3, $6)::!nexts}
;

defines:
	| ID ASSIGNO expr SEMICOLON defines	{Hashtbl.add defines $1 $3}
	| ID ASSIGNO expr SEMICOLON	{Hashtbl.add defines $1 $3}
;

spec: AG expr	{spec := $2}
;

%%

