open Lexing
open Printf

type location = {
    loc_start: position;
    loc_end: position;
}
type attribute = Mutable | Unmutable
type ptyp = PTInt | PTFloat | PTBool | PTUnt 
          | PTAray of ptyp option
          | PTLst of ptyp option
          | PTTuple of (ptyp option) list
          | PTRecord of (string * (ptyp option)) list
          | PTUdt of string
          | PTVar of int

type ptyp = PTInt | PTFloat | PTBool | PTUnt 
          | PTAray of ptyp
          | PTLst of ptyp
          | PTTuple of (ptyp) list
          | PTRecord of (string * (ptyp)) list
          | PTConstrs of (string * (ptyp option)) list
          | PTUdt of string
          | PTVar of int
and ptyp_constr = PTyp of ptyp | 
and ptyp_loc = {
    ptyp: ptyp;
    loc: location;
}
 
type pexpr_loc = {
    pexpr: pexpr;
    mutable ptyp: ptyp option;
    loc: location;
    (*attri: attribute;*)
}
and pexpr = 
      PSymbol of string
    | PLocal_Val of string * pexpr_loc
    | PLocal_Var of string * pexpr_loc
    | PDot of pexpr_loc * pexpr_loc
    | PInt of int
    | PFloat of float
    | PUnt
    | PAray of (pexpr_loc array)
    | PLst of (pexpr_loc list)
    | PBool of bool
    | PTuple of (pexpr_loc list)
    | PRecord of ((string * pexpr_loc) list)
    | PNegb of pexpr_loc
    | PAndo of pexpr_loc * pexpr_loc
    | POro of pexpr_loc * pexpr_loc
    | PNegi of pexpr_loc
    | PAdd of pexpr_loc * pexpr_loc
    | PAddDot of pexpr_loc * pexpr_loc
    | PMinus of pexpr_loc * pexpr_loc
    | PMinusDot of pexpr_loc * pexpr_loc
    | PMult of pexpr_loc * pexpr_loc
    | PMultDot of pexpr_loc * pexpr_loc
    | PEqual of pexpr_loc * pexpr_loc
    | PNon_Equal of pexpr_loc * pexpr_loc
    | PLT of pexpr_loc * pexpr_loc
    | PGT of pexpr_loc * pexpr_loc
    | PLE of pexpr_loc * pexpr_loc
    | PGE of pexpr_loc * pexpr_loc
    | PIF of pexpr_loc * pexpr_loc * (pexpr_loc option)
    | PWhile of pexpr_loc * pexpr_loc
    | PFor of pexpr_loc * pexpr_loc * pexpr_loc * pexpr_loc
    | PSeq of pexpr_loc list
    | PAssign of pexpr_loc * pexpr_loc
    | PMatch of pexpr_loc * ((ppattern_loc * pexpr_loc) list)
    | PWith of pexpr_loc * ((string * pexpr_loc) list)
    | PConstr of pconstr_loc
and ppattern_loc = {
    ppat: ppattern;
    loc: location;
    (*constrnt: pexpr_loc option; *)
}
and ppattern =
      PPat_Symbol of string
    | PPat_Int of int
    | PPat_Float of float
    | PPat_Unt
    | PPat_Aray of (ppattern_loc array)
    | PPat_Lst of (ppattern_loc list)
    | PPat_Lst_Cons of ppattern_loc * ppattern_loc
    | PPat_Underline
    | PPat_Tuple of (ppattern_loc list)
    | PPat_Record of ((string * ppattern_loc) list)
    | PPat_Constr of (string * (ppattern_loc option))
and pconstr_loc = {
    pconstr: pconstr;
    loc: location;
}
and pconstr = 
    | PConstr_basic of string
    | PConstr_compound of string * pexpr_loc
and pformula = 
    | PTop
    | PBottom
    | PAtomic of string * (pexpr_loc list)
    | PNeg of pformula_loc
    | PAnd of pformula_loc * pformula_loc
    | POr of pformula_loc * pformula_loc
    | PAX of string * pformula_loc * pexpr_loc 
    | PEX of string * pformula_loc * pexpr_loc
    | PAF of string * pformula_loc * pexpr_loc
    | PEG of string * pformula_loc * pexpr_loc
    | PAR of string * string * pformula_loc * pformula_loc * pexpr_loc
    | PEU of string * string * pformula_loc * pformula_loc * pexpr_loc
and pformula_loc = {
    pfml: pformula;
    loc: location;
}
exception Type_mismatch of pexpr_loc * ptyp * ptyp (*type_mismatch (type_has, type_expected)*)
type ast = 
    | PExpr_loc of pexpr_loc
    | PConstrs of (string, ptyp_loc option) Hashtbl.t
    | PFunction of (ppattern_loc list) * pexpr_loc
    (*| PTyp of ptyp
    | PPattern of ppattern_loc*)

type psymbol_kind = UDT | Alias | Val | Var | Function
type psymbol_tbl = (string, (psymbol_kind * ast)) Hashtbl.t
type pkripke_model = {
    init: pexpr_loc;
    transition: (ppattern_loc * pexpr_loc);
    properties: (string * pformula_loc) list;
}
type pmodul = {
    fname: string;
    imported: string list;
    psymbol_tbl: psymbol_tbl;
    pkripke_model: pkripke_model option;
}

let mk_pexpr_loc pexpr ptyp loc_start loc_end = {
    pexpr = pexpr;
    ptyp = ptyp;
    loc = {
        loc_start = loc_start;
        loc_end = loc_end;
    };
    (*attri = attri;*)
}
let mk_ppat_loc ppat loc_start loc_end = {
    ppat = ppat;
    loc = {
        loc_start = loc_start;
        loc_end = loc_end;
    };
    (*constrnt = constrnt;*)
}
let mk_pconstr_loc pconstr loc_start loc_end = {
    pconstr = pconstr;
    loc = {
        loc_start = loc_start;
        loc_end = loc_end;
    };
}
let mk_pformula_loc pfml loc_start loc_end = {
    pfml = pfml;
    loc = {
        loc_start = loc_start;
        loc_end = loc_end;
    };
}


