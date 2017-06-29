%{
    open Ast

    let imported = ref []
    let symbol_tbl:psymbol_tbl = Hashtbl.create 1
    let kripke_model = ref None
%}
%token <int>Int 
%token <float>Float
%token <string>Iden UIden
%token Import Datatype Vertical Val Var Match With Underline Model Transition Property If Then Else For In While Do Done
%token LB1 RB1 LB2 RB2 LB3 RB3 Equal Non_Equal LT GT LE GE Comma Colon Semicolon Dot DotDot Arrow EOF Add AddDot Minus MinusDot Mult MultDot
%token Negb Ando Oro And Or Neg LArrow ColonColon Init Transition Top Bottom

%start <(string list) * psymbol_tbl * (pkripke_model option)>program
%%

program: imported declars option(kripke)  {!imported, symbol_tbl, !kripke_model}
;

imported:   {}
    | imported Import UIden {imported := $3 :: !imported}
;

declars:    {}
    | Datatype id = Iden Equal cl = constr_locs   {Hashtbl.add psymbol_tbl id (UDT, PConstrs cl)}
    | Var id = Iden Equal e = expr  {Hashtbl.add psymbol_tbl id (Var, PExpr_loc e)}
    | Val id = Iden Equal e = expr  {Hashtbl.add psymbol_tbl id (Val, PExpr_loc e)}
    | id = iden ags = list(pattern) Equal e = expr {Hashtbl.add psymbol_tbl id (Function, PFunction(ags, e))}
;

kripke: Model LB3 Init Equal e1 = expr Semicolon Transition p = pattern Equal e2 = expr Semicolon pl = separated_list(Semicolon, property) RB3    {
        kripke_model := ref {
            init = e1;
            transition = (p, e2);
            properties = pl;
        }
    } 
;

property: Property id = Iden Equal fml = formula  {(id, fml)}
;

formula: Top {mk_pformula_loc PTop $startpos($1) $endpos($1)}
    | Bottom {mk_pformula_loc PBottom $startpos($1) $endpos($1)}
    | id = Iden el = list(expr) {mk_pformula_loc (PAtomic (id, el)) $startpos(id) $endpos(el)}
    | Neg formula   {mk_pformula_loc (PNeg $2) $startpos($1) $endpos($2)}
    | formula And formula   {mk_pformula_loc (PAnd ($1, $3)) $startpos($1) $endpos($3)}
    | formula Or formula    {mk_pformula_loc (POr ($1, $3)) $startpos($1) $endpos($3)}
    | AX LB1 id = Iden Comma f = formula Comma e = expr RB1 {mk_pformula_loc (PAX (id, f, e)) $startpos($1) $endpos($8)}
    | EX LB1 id = Iden Comma f = formula Comma e = expr RB1 {mk_pformula_loc (PEX (id, f, e)) $startpos($1) $endpos($8)}
    | AF LB1 id = Iden Comma f = formula Comma e = expr RB1 {mk_pformula_loc (PAF (id, f, e)) $startpos($1) $endpos($8)}
    | EG LB1 id = Iden Comma f = formula Comma e = expr RB1 {mk_pformula_loc (PEG (id, f, e)) $startpos($1) $endpos($8)}
    | AR LB1 id1 = Iden Comma id2 = Iden Comma f1 = formula Comma f2 = formula Comma e = expr RB1 {mk_pformula_loc (PAR (id1, id2, f1, f2, e)) $startpos($1) $endpos($12)}
    | EU LB1 id1 = Iden Comma id2 = Iden Comma f1 = formula Comma f2 = formula Comma e = expr RB1 {mk_pformula_loc (PEU (id1, id2, f1, f2, e)) $startpos($1) $endpos($12)}
;
/*
args: pattern   {[$1]}
    | args pattern  {$2 :: $1}
;*/

constr_locs: option(Vertical) c = constr   {[mk_pconstr_loc c $startpos(c) $endpos(c)]}
        | cl = constr_locs Vertical c = constr   {cl @ [mk_pconstr_loc c $startpos(c) $endpos(c)]}
;
constr: uid = UIden {PConstr_basic uid}
    | uid = UIden e = expr {PConstr_compound (uid, e)}
;

expr: id = Iden {mk_pexpr_loc (PSymbol id) None $startpos(id) $endpos(id)}
    | Iden Dot expr     {mk_pexpr_loc (PDot (PSymbol $1, $3)) None $startpos($1) $endpos($3)}
    | i = Int   {mk_pexpr_loc (PInt i) (Some TInt) $startpos(i) $endpos(i)}
    | f = Float {mk_pexpr_loc (PFloat f) (Some TFloat) $startpos(f) $endpos(f)}
    | LB1 RB1   {mk_pexpr_loc PUnt TUnt $startpos($1) $endpos($2)}
    | LB2 Vertical el = separated_list(Semicolon, expr) Vertical RB2   {
            let ea = Array.of_list el in
            if Array.length = 0 then
                mk_pexpr_loc (PAray ea) None $startpos($1) $endpos($5)
            else begin
                let e0 = ea.(0) in
                match e0.ptyp with
                | None -> mk_pexpr_loc (PAray ea) None $startpos($1) $endpos($5)
                | Some t -> mk_pexpr_loc (PAray ea) (Some (TAray t)) $startpos($1) $endpos($5)
            end 
        }
    | LB2 el = separated_list(Semicolon, expr) RB2    {
            if List.length el = 0 then
                mk_pexpr_loc (PLst el) None $startpos($1) $endpos($3)
            else begin
                let e0 = List.hd el in
                match e0.ptyp with
                | None -> mk_pexpr_loc (PLst el) None $startpos($1) $endpos($3)
                | Some t -> mk_pexpr_loc (PLst el) (Some (TLst t)) $startpos($1) $endpos($3)
            end
        }
    | True  {mk_pexpr_loc (PBool true) (Some TBool) $startpos($1) $endpos($1)}
    | False {mk_pexpr_loc (PBool false) (Some TBool) $startpos($1) $endpos($1)}
    | LB1 el = separated_list(Comma, expr) RB1 {
            let elt = List.map (fun e -> e.ptyp) el in
            mk_pexpr_loc (PLst el) (Some (TLst elt)) $startpos($1) $endpos($3)
        }
    | LB3 str_el = separated_list(Semicolon, str_expr) RB3 {
            let str_elt = List.map (fun se -> (fst se, (snd se).ptyp)) str_el in
            mk_pexpr_loc (PRecord str_el) (Some (TRecord str_elt)) $startpos($1) $endpos($3)
        }
    | Negb e = expr     {
            match e.ptyp with
            | None | Some TBool -> 
                e.ptyp <- Some TBool; 
                mk_pexpr_loc (Negb e) (Some TBool) $startpos($1) $endpos($2)
            | Some t -> raise type_mismatch (e, t, TBool) 
        }
    | e1 = expr Ando e2 = expr  {
            match e1.ptyp, e2.ptyp with
            | None, None | None, Some TBool | Some TBool, None | Some TBool, Some TBool | ->
                e1.ptyp <- Some TBool;
                e2.ptyp <- Some TBool;
                mk_pexpr_loc (Ando (e1, e2) (Some TBool) $startpos($1) $endpos($3))
            | Some t, None -> raise type_mismatch (e1, t, TBool) 
            | Some t, Some TBool -> raise type_mismatch (e1, t, TBool)
            | None, Some t -> raise type_mismatch (e2, t, TBool)
            | Some TBool, Some t -> raise type_mismatch (e2, t, TBool)
            | Some t1, Some t2 -> raise type_mismatch (e1, t1, TBool)
        }
    | e1 = expr Oro e2 = expr  {
            match e1.ptyp, e2.ptyp with
            | None, None | None, Some TBool | Some TBool, None | Some TBool, Some TBool | ->
                e1.ptyp <- Some TBool;
                e2.ptyp <- Some TBool;
                mk_pexpr_loc (Oro (e1, e2) (Some TBool) $startpos($1) $endpos($3))
            | Some t, None -> raise type_mismatch (e1, t, TBool) 
            | Some t, Some TBool -> raise type_mismatch (e1, t, TBool)
            | None, Some t -> raise type_mismatch (e2, t, TBool)
            | Some TBool, Some t -> raise type_mismatch (e2, t, TBool)
            | Some t1, Some t2 -> raise type_mismatch (e1, t1, TBool)
        }
    | Minus e = expr {
            match e.ptyp with
            | None | Some TInt ->
                e.ptyp <- Some TInt;
                mk_pexpr_loc (Negi e) (Some TInt) $startpos($1) $endpos($2)
            | Some t -> raise type_mismatch (e, t, TInt)
        }
    | e1 = expr Add e2 = expr {
            match e1.ptyp, e2.ptyp with
            | None, None | None, Some TInt | Some TInt, None | Some TInt, Some TInt | ->
                e1.ptyp <- Some TInt;
                e2.ptyp <- Some TInt;
                mk_pexpr_loc (Add (e1, e2) (Some TInt) $startpos($1) $endpos($3))
            | Some t, None -> raise type_mismatch (e1, t, TInt) 
            | Some t, Some TInt -> raise type_mismatch (e1, t, TInt)
            | None, Some t -> raise type_mismatch (e2, t, TInt)
            | Some Int, Some t -> raise type_mismatch (e2, t, TInt)
            | Some t1, Some t2 -> raise type_mismatch (e1, t1, TInt)
        }
    | e1 = expr Minus e2 = expr {
            match e1.ptyp, e2.ptyp with
            | None, None | None, Some TInt | Some TInt, None | Some TInt, Some TInt | ->
                e1.ptyp <- Some TInt;
                e2.ptyp <- Some TInt;
                mk_pexpr_loc (Minus (e1, e2) (Some TInt) $startpos($1) $endpos($3))
            | Some t, None -> raise type_mismatch (e1, t, TInt) 
            | Some t, Some TInt -> raise type_mismatch (e1, t, TInt)
            | None, Some t -> raise type_mismatch (e2, t, TInt)
            | Some Int, Some t -> raise type_mismatch (e2, t, TInt)
            | Some t1, Some t2 -> raise type_mismatch (e1, t1, TInt)
        }
    | e1 = expr Mult e2 = expr {
            match e1.ptyp, e2.ptyp with
            | None, None | None, Some TInt | Some TInt, None | Some TInt, Some TInt | ->
                e1.ptyp <- Some TInt;
                e2.ptyp <- Some TInt;
                mk_pexpr_loc (Mult (e1, e2) (Some TInt) $startpos($1) $endpos($3))
            | Some t, None -> raise type_mismatch (e1, t, TInt) 
            | Some t, Some TInt -> raise type_mismatch (e1, t, TInt)
            | None, Some t -> raise type_mismatch (e2, t, TInt)
            | Some Int, Some t -> raise type_mismatch (e2, t, TInt)
            | Some t1, Some t2 -> raise type_mismatch (e1, t1, TInt)
        }
    | e1 = expr AddDot e2 = expr {
            match e1.ptyp, e2.ptyp with
            | None, None | None, Some TFloat | Some TFloat, None | Some TFloat, Some TFloat | ->
                e1.ptyp <- Some TFloat;
                e2.ptyp <- Some TFloat;
                mk_pexpr_loc (AddDot (e1, e2) (Some TFloat) $startpos($1) $endpos($3))
            | Some t, None -> raise type_mismatch (e1, t, TFloat) 
            | Some t, Some TFloat -> raise type_mismatch (e1, t, TFloat)
            | None, Some t -> raise type_mismatch (e2, t, TFloat)
            | Some TFloat, Some t -> raise type_mismatch (e2, t, TFloat)
            | Some t1, Some t2 -> raise type_mismatch (e1, t1, TFloat)
        }
    | e1 = expr MinusDot e2 = expr {
            match e1.ptyp, e2.ptyp with
            | None, None | None, Some TFloat | Some TFloat, None | Some TFloat, Some TFloat | ->
                e1.ptyp <- Some TFloat;
                e2.ptyp <- Some TFloat;
                mk_pexpr_loc (MinusDot (e1, e2) (Some TFloat) $startpos($1) $endpos($3))
            | Some t, None -> raise type_mismatch (e1, t, TFloat) 
            | Some t, Some TFloat -> raise type_mismatch (e1, t, TFloat)
            | None, Some t -> raise type_mismatch (e2, t, TFloat)
            | Some TFloat, Some t -> raise type_mismatch (e2, t, TFloat)
            | Some t1, Some t2 -> raise type_mismatch (e1, t1, TFloat)
        }
    | e1 = expr MultDot e2 = expr {
            match e1.ptyp, e2.ptyp with
            | None, None | None, Some TFloat | Some TFloat, None | Some TFloat, Some TFloat | ->
                e1.ptyp <- Some TFloat;
                e2.ptyp <- Some TFloat;
                mk_pexpr_loc (MultDot (e1, e2) (Some TFloat) $startpos($1) $endpos($3))
            | Some t, None -> raise type_mismatch (e1, t, TFloat) 
            | Some t, Some TFloat -> raise type_mismatch (e1, t, TFloat)
            | None, Some t -> raise type_mismatch (e2, t, TFloat)
            | Some TFloat, Some t -> raise type_mismatch (e2, t, TFloat)
            | Some t1, Some t2 -> raise type_mismatch (e1, t1, TFloat)
        }
    | e1 = expr Equal e2 = expr {mk_pexpr_loc (PEqual (e1, e2)) (Some TBool) $startpos($1) $endpos($3)}
    | e1 = expr Non_Equal e2 = expr {mk_pexpr_loc (PNon_Equal (e1, e2)) (Some TBool) $startpos($1) $endpos($3)}
    | e1 = expr LT e2 = expr    {mk_pexpr_loc (PLT (e1, e2)) (Some TBool) $startpos($1) $endpos($3)}
    | e1 = expr GT e2 = expr    {mk_pexpr_loc (PGT (e1, e2)) (Some TBool) $startpos($1) $endpos($3)}
    | e1 = expr LE e2 = expr    {mk_pexpr_loc (PLE (e1, e2)) (Some TBool) $startpos($1) $endpos($3)}
    | e1 = expr GE e2 = expr    {mk_pexpr_loc (PGE (e1, e2)) (Some TBool) $startpos($1) $endpos($3)}
    | If e1 = expr Then e2 = expr oe = option(else_expr)   {
            match oe with
            | None -> begin
                    match e1.ptyp, e2.ptyp with
                    | None, None | None, Some TUnt | Some TBool, Some TUnt | Some TBool, None -> 
                        e1.ptyp <- Some TUnt;
                        e2.ptyp <- Some TBool;
                        mk_pexpr_loc (PIF (e1, e2, None)) e2.ptyp $startpos($1) $endpos(oe)
                    | Some t, None -> raise type_mismatch (e1, t, TBool)
                    | Some t, Some TUnt -> raise type_mismatch (e1, t, TBool)
                    | None, Some t -> raise type_mismatch (e2, t, TUnt)
                    | Some TBool, Some t -> raise type_mismatch (e2, t, TUnt)
                    | Some t1, Some t2 -> raise type_mismatch (e1, t1, TBool)
                end
            | Some e3 -> begin
                    match e1.ptyp with
                    | None | Some TBool -> 
                        e1.ptyp <- Some TBool;
                        mk_pexpr_loc (PIF (e1, e2, oe)) e2.ptyp $startpos($1) $endpos(oe)
                    | Some t -> raise type_mismatch (e1, t, TBool)
                end
        }
    | While e1 = expr Do e2 = expr Done {
             match e1.ptyp, e2.ptyp with
            | None, None | None, Some TUnt | Some TBool, Some TUnt | Some TBool, None -> 
                e1.ptyp <- Some TUnt;
                e2.ptyp <- Some TBool;
                mk_pexpr_loc (PWhile (e1, e2)) (Some TUnt) $startpos($1) $endpos($5)
            | Some t, None -> raise type_mismatch (e1, t, TBool)
            | Some t, Some TUnt -> raise type_mismatch (e1, t, TBool)
            | None, Some t -> raise type_mismatch (e2, t, TUnt)
            | Some TBool, Some t -> raise type_mismatch (e2, t, TUnt)
            | Some t1, Some t2 -> raise type_mismatch (e1, t1, TBool)
        }
    | For e1 = expr In LB2 e2 = expr DotDot e3 = expr RB2 Do e4 = expr Done {
            begin
                match e1.ptyp with
                | None | Some TBool -> e1.ptyp <- Some TBool
                | Some t -> raise type_mismatch (e1, t, TBool) 
            end;
            begin
                match e2.ptyp, e3.ptyp with
                | None, None | Some TInt, None | None, Some TInt | Some TInt, Some TInt ->
                    e2.ptyp <- Some TInt;
                    e3.ptyp <- Some TInt
                | Some t, None -> raise type_mismatch (e2, t, TInt)
                | Some t, Some TInt -> raise type_mismatch (e2, t, TInt)
                | None, Some t -> raise type_mismatch (e3, t, TInt)
                | Some TInt, Some t -> raise type_mismatch (e3, t, TInt)
                | Some t1, Some t2 -> raise type_mismatch (e2, t1, TInt)
            end;
            begin
                match e4.ptyp with
                | None | Some TUnt -> 
                    e4.ptyp <- Some TUnt;
                    mk_pexpr_loc (PFor (e1, e2, e3, e4)) (Some TUnt) $startpos($1) $endpos($10)
                | Some t -> raise type_mismatch (e4, t, TUnt)
            end
        }
    | e1 = expr Semicolon e2 = expr   {mk_pexpr_loc (PSeq (e1, e2)) (e2.ptyp) $startpos(e1) $endpos(e2)}
    | e1 = expr LArrow e2 = expr    {mk_pexpr_loc (PAssign (e1, e2)) TUnt $startpos(e1) $endpos(e2)}
    | Match e1 = expr With pel = separated_list(Vertical, pattern_expr) {mk_pexpr_loc (PMatch (e1, pel)) None $startpos($1) $endpos(pel)}
    | e1 = expr With LB3 str_el = separated_list(Semicolon, str_expr) RB3    {mk_pexpr_loc (PWith (e1, str_el)) e1.ptyp $startpos(e1) $endpos($5)}
    | uid = UIden eo = option(expr) {
            match eo with
            | None -> mk_pexpr_loc (PConstr (PConstr_basic uid)) None $startpos(uid) $endpos(eo)
            | Some e -> mk_pexpr_loc (Pconstr (PConstr_compound (uid, e))) None $startpos(uid) $endpos(eo)
        }
    | LB1 expr RB1  {$2}
;
str_expr: Iden Equal expr   {($1, $3)}
;

else_expr: Else expr    {$2}
;

pattern_expr: pattern Arrow expr    {($1, $3)}
;

pattern: id = Iden   {mk_ppat_loc (PPat_Symbol id) $startpos(id) $endpos(id)}
    | i = Int   {mk_ppat_loc (PPat_Int i) $startpos(i) $endpos(i)}
    | f = Float {mk_ppat_loc (PPat_Float f) $startpos(f) $endpos(f)}
    | LB1 RB1   {mk_ppat_loc (PPat_Unt) $startpos($1) $endpos($2)}
    | LB2 Vertical pl = separated_list(Semicolon, pattern)  Vertical RB2 {mk_ppat_loc (PPat_Aray (Array.of_list pl)) $startpos($1) $endpos($5)}
    | LB2 pl = separated_list(Semicolon, pattern) RB2   {mk_ppat_loc (PPat_Lst pl) $startpos($1) $endpos($3)}
    | p1 = pattern ColonColon p2 = pattern    {mk_ppat_loc (PPat_Lst_Cons (p1, p2)) $startpos(p1) $endpos(p2)}
    | Underline     {mk_ppat_loc PPat_Underline $startpos($1) $endpos($1)}
    | pl = separated_list(Comma, pattern)   {mk_ppat_loc (PPat_Tuple pl) $startpos(pl) $endpos(pl)}
    | LB3 str_pl = separated_list(Semicolon, str_pattern) RB3   {mk_ppat_loc (PPat_Record str_pl) $startpos($1) $endpos($3)}
    | uid = UIden po = option(pattern)  {mk_ppat_loc (PPat_Constr (uid, po)) $startpos(uid) $endpos(po)}
    | LB1 pattern RB1   {$2}
;

str_pattern: Iden Equal pattern     {($1, $3)}
;
/*expr_aray:  {[||]}
    | expr  {[|$1|]}
    | expr Semicolon expr_aray  {Array.append [|$1|] $3}
;
expr_list:  {[]}
    | expr  {[$1]}
    | expr Semicolon expr_list  {$1 :: $3}
;*/
%%