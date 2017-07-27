
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | With
    | While
    | Vertical
    | Var
    | Val
    | Underline
    | UIden of (
# 35 "parser.mly"
       (string)
# 17 "parser.ml"
  )
    | True
    | Transition
    | Top
    | Then
    | TUnt
    | TLst
    | TInt
    | TFloat
    | TBool
    | TAray
    | State
    | Semicolon
    | RB3
    | RB2
    | RB1
    | Property
    | Oro
    | Or
    | Of
    | Non_Equal
    | Negb
    | Neg
    | MultDot
    | Mult
    | Model
    | MinusDot
    | Minus
    | Match
    | LT
    | LE
    | LB3
    | LB2
    | LB1
    | LArrow
    | Int of (
# 33 "parser.mly"
       (int)
# 56 "parser.ml"
  )
    | Init
    | In
    | Import
    | If
    | Iden of (
# 35 "parser.mly"
       (string)
# 65 "parser.ml"
  )
    | GT
    | GE
    | Function
    | For
    | Float of (
# 34 "parser.mly"
       (float)
# 74 "parser.ml"
  )
    | False
    | Equal
    | Else
    | EX
    | EU
    | EOF
    | EG
    | DotDot
    | Dot
    | Done
    | Do
    | Datatype
    | Comma
    | ColonColon
    | Colon
    | Bottom
    | Arrow
    | Ando
    | And
    | AddDot
    | Add
    | AX
    | AR
    | AF
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState345
  | MenhirState338
  | MenhirState334
  | MenhirState333
  | MenhirState330
  | MenhirState329
  | MenhirState327
  | MenhirState324
  | MenhirState323
  | MenhirState320
  | MenhirState319
  | MenhirState316
  | MenhirState315
  | MenhirState313
  | MenhirState310
  | MenhirState309
  | MenhirState307
  | MenhirState305
  | MenhirState303
  | MenhirState299
  | MenhirState293
  | MenhirState288
  | MenhirState284
  | MenhirState278
  | MenhirState273
  | MenhirState271
  | MenhirState270
  | MenhirState268
  | MenhirState265
  | MenhirState264
  | MenhirState263
  | MenhirState262
  | MenhirState259
  | MenhirState258
  | MenhirState255
  | MenhirState250
  | MenhirState243
  | MenhirState240
  | MenhirState238
  | MenhirState237
  | MenhirState236
  | MenhirState233
  | MenhirState232
  | MenhirState229
  | MenhirState227
  | MenhirState225
  | MenhirState224
  | MenhirState220
  | MenhirState219
  | MenhirState217
  | MenhirState215
  | MenhirState212
  | MenhirState211
  | MenhirState210
  | MenhirState209
  | MenhirState208
  | MenhirState206
  | MenhirState205
  | MenhirState204
  | MenhirState203
  | MenhirState201
  | MenhirState199
  | MenhirState198
  | MenhirState196
  | MenhirState192
  | MenhirState191
  | MenhirState187
  | MenhirState186
  | MenhirState184
  | MenhirState183
  | MenhirState180
  | MenhirState178
  | MenhirState173
  | MenhirState172
  | MenhirState171
  | MenhirState170
  | MenhirState168
  | MenhirState164
  | MenhirState155
  | MenhirState151
  | MenhirState148
  | MenhirState144
  | MenhirState142
  | MenhirState141
  | MenhirState139
  | MenhirState137
  | MenhirState136
  | MenhirState133
  | MenhirState132
  | MenhirState131
  | MenhirState130
  | MenhirState129
  | MenhirState128
  | MenhirState125
  | MenhirState124
  | MenhirState121
  | MenhirState119
  | MenhirState118
  | MenhirState115
  | MenhirState114
  | MenhirState113
  | MenhirState112
  | MenhirState111
  | MenhirState110
  | MenhirState109
  | MenhirState108
  | MenhirState107
  | MenhirState106
  | MenhirState105
  | MenhirState104
  | MenhirState103
  | MenhirState102
  | MenhirState101
  | MenhirState100
  | MenhirState99
  | MenhirState98
  | MenhirState97
  | MenhirState96
  | MenhirState95
  | MenhirState94
  | MenhirState93
  | MenhirState92
  | MenhirState90
  | MenhirState89
  | MenhirState88
  | MenhirState87
  | MenhirState86
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState80
  | MenhirState79
  | MenhirState77
  | MenhirState76
  | MenhirState75
  | MenhirState71
  | MenhirState69
  | MenhirState66
  | MenhirState62
  | MenhirState61
  | MenhirState58
  | MenhirState57
  | MenhirState56
  | MenhirState55
  | MenhirState53
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState49
  | MenhirState47
  | MenhirState46
  | MenhirState43
  | MenhirState40
  | MenhirState39
  | MenhirState36
  | MenhirState35
  | MenhirState34
  | MenhirState30
  | MenhirState28
  | MenhirState24
  | MenhirState23
  | MenhirState21
  | MenhirState17
  | MenhirState16
  | MenhirState15
  | MenhirState14
  | MenhirState13
  | MenhirState12
  | MenhirState10
  | MenhirState9
  | MenhirState5
  | MenhirState3
  | MenhirState2
  | MenhirState0

# 1 "parser.mly"
  
    open Ast

    let imported = ref []
    let symbol_tbl:(Ast.psymbol_tbl) = Hashtbl.create 1
    let kripke_model = ref None
    let type_var = ref 0
    let new_type_var () = 
        incr type_var;
        !type_var

    let erase_type_args t args = 
        let tmp_t = ref t in
        let rec erase_type_args_i pt i = 
            match pt with
            | PTUdt (str, pts) -> 
                let a = List.nth args i in
                if a=str then
                    PTVar (-i-1)
                else 
                    PTUdt (str, List.map (fun pt -> erase_type_args_i pt i) pts) 
            | PTAray pt1 -> PTAray (erase_type_args_i pt1 i) 
            | PTLst pt1 -> PTLst (erase_type_args_i pt1 i)
            | PTTuple pts -> PTTuple (List.map (fun pt -> erase_type_args_i pt i) pts)
            | PTRecord str_pts -> PTRecord (List.map (fun (str, pt) -> (str, erase_type_args_i pt i)) str_pts)
            | _ -> pt
        in
        for i = 0 to (List.length args) do
            tmp_t := erase_type_args_i !tmp_t i
        done;
        !tmp_t

# 324 "parser.ml"

let rec _menhir_goto_list_property_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.pformula_loc) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState338 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Ast.pformula_loc))), _, (xs : ((string * Ast.pformula_loc) list))) = _menhir_stack in
        let _v : ((string * Ast.pformula_loc) list) = 
# 187 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 337 "parser.ml"
         in
        _menhir_goto_list_property_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState265 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RB3 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__9_ = _endpos in
            let ((((((_menhir_stack, _startpos__2_), _, (_3 : ((string * Ast.pexpr_loc) list))), _endpos_p_, _, (p : (Ast.ppattern_loc)), _startpos_p_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc))), _, (pl : ((string * Ast.pformula_loc) list))) = _menhir_stack in
            let _9 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (unit) = 
# 118 "parser.mly"
                                                                                           (
        kripke_model := Some {
            transition = (p, e2);
            properties = pl;
        }
    )
# 365 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EOF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, (_1 : (unit))), _, (_2 : (unit))), (_3 : (unit))) = _menhir_stack in
                let _4 = () in
                let _v : (
# 41 "parser.mly"
       ((string list) * (Ast.psymbol_tbl) * ((Ast.pkripke_model) option))
# 380 "parser.ml"
                ) = 
# 71 "parser.mly"
                                   (!imported, symbol_tbl, !kripke_model)
# 384 "parser.ml"
                 in
                _menhir_goto_program _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_pattern_expr_list : _menhir_env -> 'ttv_tail -> Lexing.position -> ((Ast.ppattern_loc * Ast.pexpr_loc) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Vertical ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Float _v ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState168 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Iden _v ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState168 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Int _v ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState168 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB1 ->
            _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB2 ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIden _v ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState168 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Underline ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168)
    | Add | AddDot | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | Equal | False | Float _ | For | Function | GE | GT | Iden _ | If | Int _ | LArrow | LB1 | LB2 | LB3 | LE | LT | Match | Minus | MinusDot | Model | Mult | MultDot | Negb | Non_Equal | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | While | With ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e1_, _, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_pel_, (pel : ((Ast.ppattern_loc * Ast.pexpr_loc) list))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_pel_ in
        let _v : (Ast.pexpr_loc) = 
# 444 "parser.mly"
                                                          (mk_pexpr_loc (PMatch (e1, pel)) (PTVar (new_type_var ())) _startpos__1_ _endpos_pel_)
# 442 "parser.ml"
         in
        _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_separated_nonempty_list_Comma_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.pexpr_loc list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__5_ = _endpos in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : (Ast.pexpr_loc))), _, (el : (Ast.pexpr_loc list))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__5_ in
            let _v : (Ast.pexpr_loc) = 
# 241 "parser.mly"
                                                                       (
            let elt = List.map (fun (e:pexpr_loc) -> e.ptyp) (e::el) in
            mk_pexpr_loc (PTuple el) ((PTTuple elt)) _startpos__1_ _endpos__5_
        )
# 479 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _endpos_x_, _menhir_s, (x : (Ast.pexpr_loc))), _, (xs : (Ast.pexpr_loc list))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.pexpr_loc list) = 
# 217 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 496 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_Comma_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_else_expr_ : _menhir_env -> 'ttv_tail -> Lexing.position -> (Ast.pexpr_loc option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_oe_ = _endpos in
    let (oe : (Ast.pexpr_loc option)) = _v in
    let ((((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e1_, _, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc))) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos_oe_ in
    let _v : (Ast.pexpr_loc) = 
# 378 "parser.mly"
                                                                  (
            match oe with
            | None -> begin
                    mk_pexpr_loc (PIF (e1, e2, None)) PTUnt _startpos__1_ _endpos_oe_
                    (*match e1.ptyp, e2.ptyp with
                    | None, None | None, Some PTUnt | Some PTBool, Some PTUnt | Some PTBool, None -> 
                        e1.ptyp <- Some PTUnt;
                        e2.ptyp <- Some PTBool;
                        mk_pexpr_loc (PIF (e1, e2, None)) e2.ptyp $startpos($1) $endpos(oe)
                    | Some t, None -> raise (Type_mismatch (e1, t, PTBool))
                    | Some t, Some PTUnt -> raise (Type_mismatch (e1, t, PTBool))
                    | None, Some t -> raise (Type_mismatch (e2, t, PTUnt))
                    | Some PTBool, Some t -> raise (Type_mismatch (e2, t, PTUnt))
                    | Some t1, Some t2 -> raise (Type_mismatch (e1, t1, PTBool))*)
                end
            | Some e3 -> begin
                    mk_pexpr_loc (PIF (e1, e2, oe)) e2.ptyp _startpos__1_ _endpos_oe_
                    (*match e1.ptyp with
                    | None | Some PTBool -> 
                        e1.ptyp <- Some PTBool;
                        mk_pexpr_loc (PIF (e1, e2, oe)) e2.ptyp $startpos($1) $endpos(oe)
                    | Some t -> raise (Type_mismatch (e1, t, PTBool))*)
                end
        )
# 539 "parser.ml"
     in
    _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * ((string * Ast.pexpr_loc) list) -> Lexing.position -> (
# 35 "parser.mly"
       (string)
# 546 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Equal ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | False ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Float _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | For ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Iden _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | If ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Int _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB1 ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB2 ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB3 ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Match ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Negb ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | True ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIden _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Val ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Var ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | While ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce84 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Ast.pformula_loc) list) = 
# 185 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 610 "parser.ml"
     in
    _menhir_goto_list_property_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run266 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Equal ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AF ->
                _menhir_run300 _menhir_env (Obj.magic _menhir_stack) MenhirState268 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | AR ->
                _menhir_run294 _menhir_env (Obj.magic _menhir_stack) MenhirState268 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | AX ->
                _menhir_run290 _menhir_env (Obj.magic _menhir_stack) MenhirState268 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Bottom ->
                _menhir_run289 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState268 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EG ->
                _menhir_run285 _menhir_env (Obj.magic _menhir_stack) MenhirState268 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EU ->
                _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState268 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EX ->
                _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState268 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run271 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState268 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Neg ->
                _menhir_run270 _menhir_env (Obj.magic _menhir_stack) MenhirState268 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Top ->
                _menhir_run269 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState268 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState268)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run269 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.pformula_loc) = 
# 133 "parser.mly"
             (mk_pformula_loc PTop _startpos__1_ _endpos__1_)
# 682 "parser.ml"
     in
    _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run270 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AF ->
        _menhir_run300 _menhir_env (Obj.magic _menhir_stack) MenhirState270 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AR ->
        _menhir_run294 _menhir_env (Obj.magic _menhir_stack) MenhirState270 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AX ->
        _menhir_run290 _menhir_env (Obj.magic _menhir_stack) MenhirState270 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Bottom ->
        _menhir_run289 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState270 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EG ->
        _menhir_run285 _menhir_env (Obj.magic _menhir_stack) MenhirState270 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EU ->
        _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState270 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EX ->
        _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState270 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run271 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState270 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Neg ->
        _menhir_run270 _menhir_env (Obj.magic _menhir_stack) MenhirState270 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Top ->
        _menhir_run269 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState270 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState270

and _menhir_run271 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "parser.mly"
       (string)
# 720 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState271 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState271 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState271 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState271 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState271 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState271 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | And | Comma | Or | Property | RB3 ->
        _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack) MenhirState271
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState271

and _menhir_run275 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LB1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Iden _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Comma ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | AF ->
                    _menhir_run300 _menhir_env (Obj.magic _menhir_stack) MenhirState278 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AR ->
                    _menhir_run294 _menhir_env (Obj.magic _menhir_stack) MenhirState278 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AX ->
                    _menhir_run290 _menhir_env (Obj.magic _menhir_stack) MenhirState278 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Bottom ->
                    _menhir_run289 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState278 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EG ->
                    _menhir_run285 _menhir_env (Obj.magic _menhir_stack) MenhirState278 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EU ->
                    _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState278 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EX ->
                    _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState278 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Iden _v ->
                    _menhir_run271 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState278 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Neg ->
                    _menhir_run270 _menhir_env (Obj.magic _menhir_stack) MenhirState278 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Top ->
                    _menhir_run269 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState278 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState278)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, _), _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run279 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LB1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Iden _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Comma ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Iden _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | Comma ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | AF ->
                            _menhir_run300 _menhir_env (Obj.magic _menhir_stack) MenhirState284 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | AR ->
                            _menhir_run294 _menhir_env (Obj.magic _menhir_stack) MenhirState284 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | AX ->
                            _menhir_run290 _menhir_env (Obj.magic _menhir_stack) MenhirState284 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | Bottom ->
                            _menhir_run289 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState284 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | EG ->
                            _menhir_run285 _menhir_env (Obj.magic _menhir_stack) MenhirState284 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | EU ->
                            _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState284 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | EX ->
                            _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState284 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | Iden _v ->
                            _menhir_run271 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState284 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | Neg ->
                            _menhir_run270 _menhir_env (Obj.magic _menhir_stack) MenhirState284 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | Top ->
                            _menhir_run269 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState284 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState284)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((((_menhir_stack, _menhir_s, _), _), _, _, _), _, _, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s, _), _), _, _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, _), _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run285 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LB1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Iden _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Comma ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | AF ->
                    _menhir_run300 _menhir_env (Obj.magic _menhir_stack) MenhirState288 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AR ->
                    _menhir_run294 _menhir_env (Obj.magic _menhir_stack) MenhirState288 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AX ->
                    _menhir_run290 _menhir_env (Obj.magic _menhir_stack) MenhirState288 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Bottom ->
                    _menhir_run289 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState288 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EG ->
                    _menhir_run285 _menhir_env (Obj.magic _menhir_stack) MenhirState288 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EU ->
                    _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState288 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EX ->
                    _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState288 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Iden _v ->
                    _menhir_run271 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState288 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Neg ->
                    _menhir_run270 _menhir_env (Obj.magic _menhir_stack) MenhirState288 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Top ->
                    _menhir_run269 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState288 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState288)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, _), _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run289 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.pformula_loc) = 
# 134 "parser.mly"
             (mk_pformula_loc PBottom _startpos__1_ _endpos__1_)
# 1014 "parser.ml"
     in
    _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run290 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LB1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Iden _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Comma ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | AF ->
                    _menhir_run300 _menhir_env (Obj.magic _menhir_stack) MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AR ->
                    _menhir_run294 _menhir_env (Obj.magic _menhir_stack) MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AX ->
                    _menhir_run290 _menhir_env (Obj.magic _menhir_stack) MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Bottom ->
                    _menhir_run289 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EG ->
                    _menhir_run285 _menhir_env (Obj.magic _menhir_stack) MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EU ->
                    _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EX ->
                    _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Iden _v ->
                    _menhir_run271 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState293 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Neg ->
                    _menhir_run270 _menhir_env (Obj.magic _menhir_stack) MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Top ->
                    _menhir_run269 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState293)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, _), _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run294 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LB1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Iden _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Comma ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Iden _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | Comma ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | AF ->
                            _menhir_run300 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | AR ->
                            _menhir_run294 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | AX ->
                            _menhir_run290 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | Bottom ->
                            _menhir_run289 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState299 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | EG ->
                            _menhir_run285 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | EU ->
                            _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | EX ->
                            _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | Iden _v ->
                            _menhir_run271 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState299 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | Neg ->
                            _menhir_run270 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | Top ->
                            _menhir_run269 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState299 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState299)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((((_menhir_stack, _menhir_s, _), _), _, _, _), _, _, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s, _), _), _, _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, _), _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run300 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LB1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Iden _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Comma ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | AF ->
                    _menhir_run300 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AR ->
                    _menhir_run294 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AX ->
                    _menhir_run290 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Bottom ->
                    _menhir_run289 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EG ->
                    _menhir_run285 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EU ->
                    _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EX ->
                    _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Iden _v ->
                    _menhir_run271 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState303 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Neg ->
                    _menhir_run270 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Top ->
                    _menhir_run269 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState303)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, _), _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run305 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pformula_loc) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AF ->
        _menhir_run300 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AR ->
        _menhir_run294 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AX ->
        _menhir_run290 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Bottom ->
        _menhir_run289 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState305 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EG ->
        _menhir_run285 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EU ->
        _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EX ->
        _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run271 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState305 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Neg ->
        _menhir_run270 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Top ->
        _menhir_run269 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState305 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState305

and _menhir_run307 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pformula_loc) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AF ->
        _menhir_run300 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AR ->
        _menhir_run294 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AX ->
        _menhir_run290 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Bottom ->
        _menhir_run289 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EG ->
        _menhir_run285 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EU ->
        _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EX ->
        _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run271 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState307 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Neg ->
        _menhir_run270 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Top ->
        _menhir_run269 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState307

and _menhir_goto_list_expr_single_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (Ast.pexpr_loc list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    match _menhir_s with
    | MenhirState271 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_el_ = _endpos in
        let (el : (Ast.pexpr_loc list)) = _v in
        let (_menhir_stack, _endpos_id_, _menhir_s, (id : (
# 35 "parser.mly"
       (string)
# 1321 "parser.ml"
        )), _startpos_id_) = _menhir_stack in
        let _startpos = _startpos_id_ in
        let _endpos = _endpos_el_ in
        let _v : (Ast.pformula_loc) = 
# 135 "parser.mly"
                                       (mk_pformula_loc (PAtomic (id, el)) _startpos_id_ _endpos_el_)
# 1328 "parser.ml"
         in
        _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | MenhirState273 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_xs_ = _endpos in
        let (xs : (Ast.pexpr_loc list)) = _v in
        let (_menhir_stack, _endpos_x_, _menhir_s, (x : (Ast.pexpr_loc)), _startpos_x_) = _menhir_stack in
        let _endpos = _endpos_xs_ in
        let _v : (Ast.pexpr_loc list) = 
# 187 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 1341 "parser.ml"
         in
        _menhir_goto_list_expr_single_ _menhir_env _menhir_stack _endpos _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (Ast.pexpr_loc) -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : ((string * Ast.pexpr_loc) list))), _endpos__2_, (_2 : (
# 35 "parser.mly"
       (string)
# 1363 "parser.ml"
            )), _startpos__2_), _endpos__4_, _, (_4 : (Ast.pexpr_loc))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _v : ((string * Ast.pexpr_loc) list) = 
# 476 "parser.mly"
                                                ((_2, _4) :: _1)
# 1370 "parser.ml"
             in
            _menhir_goto_str_expr_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Done ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__11_ = _endpos in
            let ((((((((_menhir_stack, _menhir_s, _startpos__1_), _endpos_str_, (str : (
# 35 "parser.mly"
       (string)
# 1393 "parser.ml"
            )), _startpos_str_), _startpos__4_), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_), _), _endpos_e3_, _, (e3 : (Ast.pexpr_loc)), _startpos_e3_), _endpos__8_, _), _endpos_e4_, _, (e4 : (Ast.pexpr_loc))) = _menhir_stack in
            let _11 = () in
            let _9 = () in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__11_ in
            let _v : (Ast.pexpr_loc) = 
# 415 "parser.mly"
                                                                                           (
            mk_pexpr_loc (PFor (str, e2, e3, e4)) (PTUnt) _startpos__1_ _endpos__11_
            (*begin
                match e1.ptyp with
                | None | Some PTBool -> e1.ptyp <- Some PTBool
                | Some t -> raise (Type_mismatch (e1, t, PTBool))
            end;
            begin
                match e2.ptyp, e3.ptyp with
                | None, None | Some PTInt, None | None, Some PTInt | Some PTInt, Some PTInt ->
                    e2.ptyp <- Some PTInt;
                    e3.ptyp <- Some PTInt
                | Some t, None -> raise (Type_mismatch (e2, t, PTInt))
                | Some t, Some PTInt -> raise (Type_mismatch (e2, t, PTInt))
                | None, Some t -> raise (Type_mismatch (e3, t, PTInt))
                | Some PTInt, Some t -> raise (Type_mismatch (e3, t, PTInt))
                | Some t1, Some t2 -> raise (Type_mismatch (e2, t1, PTInt))
            end;
            begin
                match e4.ptyp with
                | None | Some PTUnt -> 
                    e4.ptyp <- Some PTUnt;
                    mk_pexpr_loc (PFor (e1, e2, e3, e4)) (Some PTUnt) $startpos($1) $endpos($11)
                | Some t -> raise (Type_mismatch (e4, t, PTUnt))
            end*)
        )
# 1432 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Else ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144)
        | Add | AddDot | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Equal | False | Float _ | For | Function | GE | GT | Iden _ | If | Int _ | LArrow | LB1 | LB2 | LB3 | LE | LT | Match | Minus | MinusDot | Model | Mult | MultDot | Negb | Non_Equal | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While | With ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_, _endpos) = Obj.magic _menhir_stack in
            let _v : (Ast.pexpr_loc option) = 
# 100 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( None )
# 1497 "parser.ml"
             in
            _menhir_goto_option_else_expr_ _menhir_env _menhir_stack _endpos _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _endpos__2_, _, (_2 : (Ast.pexpr_loc))) = _menhir_stack in
        let _1 = () in
        let _endpos = _endpos__2_ in
        let _v : (Ast.pexpr_loc) = 
# 484 "parser.mly"
                        (_2)
# 1515 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_x_ = _endpos in
        let (x : (Ast.pexpr_loc)) = _v in
        let _endpos = _endpos_x_ in
        let _v : (Ast.pexpr_loc option) = 
# 102 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 1525 "parser.ml"
         in
        _menhir_goto_option_else_expr_ _menhir_env _menhir_stack _endpos _v
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState155 | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155)
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos_x_, _menhir_s, (x : (Ast.pexpr_loc))) = _menhir_stack in
            let _v : (Ast.pexpr_loc list) = 
# 215 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 1640 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_Comma_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 35 "parser.mly"
       (string)
# 1661 "parser.ml"
            )), _startpos__1_), _endpos__3_, _, (_3 : (Ast.pexpr_loc))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : ((string * Ast.pexpr_loc) list) = 
# 475 "parser.mly"
                                ([(_1, _3)])
# 1668 "parser.ml"
             in
            _menhir_goto_str_expr_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState199 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (Ast.ppattern_loc)), _startpos__1_), _), _endpos__3_, _, (_3 : (Ast.pexpr_loc))) = _menhir_stack in
        let _2 = () in
        let _endpos = _endpos__3_ in
        let _v : (Ast.ppattern_loc * Ast.pexpr_loc) = 
# 490 "parser.mly"
                                    ((_1, _3))
# 1686 "parser.ml"
         in
        (match _menhir_s with
        | MenhirState168 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let (_3 : (Ast.ppattern_loc * Ast.pexpr_loc)) = _v in
            let (_menhir_stack, _endpos__1_, (_1 : ((Ast.ppattern_loc * Ast.pexpr_loc) list))) = _menhir_stack in
            let _2 = () in
            let _endpos = _endpos__3_ in
            let _v : ((Ast.ppattern_loc * Ast.pexpr_loc) list) = 
# 488 "parser.mly"
                                                (_1 @ [_3])
# 1700 "parser.ml"
             in
            _menhir_goto_pattern_expr_list _menhir_env _menhir_stack _endpos _v
        | MenhirState201 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos_pe_ = _endpos in
            let (pe : (Ast.ppattern_loc * Ast.pexpr_loc)) = _v in
            let (_menhir_stack, (_1 : (unit option))) = _menhir_stack in
            let _endpos = _endpos_pe_ in
            let _v : ((Ast.ppattern_loc * Ast.pexpr_loc) list) = 
# 487 "parser.mly"
                                                      ([pe])
# 1713 "parser.ml"
             in
            _menhir_goto_pattern_expr_list _menhir_env _menhir_stack _endpos _v
        | _ ->
            _menhir_fail ())
    | MenhirState212 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Done ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__5_ = _endpos in
            let ((((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e1_, _, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__5_ in
            let _v : (Ast.pexpr_loc) = 
# 402 "parser.mly"
                                               (
            mk_pexpr_loc (PWhile (e1, e2)) (PTUnt) _startpos__1_ _endpos__5_
             (*match e1.ptyp, e2.ptyp with
            | None, None | None, Some PTUnt | Some PTBool, Some PTUnt | Some PTBool, None -> 
                e1.ptyp <- Some PTUnt;
                e2.ptyp <- Some PTBool;
                mk_pexpr_loc (PWhile (e1, e2)) (Some PTUnt) $startpos($1) $endpos($5)
            | Some t, None -> raise (Type_mismatch (e1, t, PTBool))
            | Some t, Some PTUnt -> raise (Type_mismatch (e1, t, PTBool))
            | None, Some t -> raise (Type_mismatch (e2, t, PTUnt))
            | Some PTBool, Some t -> raise (Type_mismatch (e2, t, PTUnt))
            | Some t1, Some t2 -> raise (Type_mismatch (e1, t1, PTBool))*)
        )
# 1750 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState229 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 1765 "parser.ml"
        )), _startpos_id_), _, (ags : (Ast.ppattern_loc list))), _, (otf : (Ast.ptyp option))), _endpos_e_, _, (e : (Ast.pexpr_loc))) = _menhir_stack in
        let _5 = () in
        let _1 = () in
        let _v : (unit) = 
# 99 "parser.mly"
                                                                               (
        print_endline ("declaring function "^id);
        match otf with
        | None -> Hashtbl.add symbol_tbl id (Function, PFunction(PTVar (new_type_var ()), ags, e))
        | Some pt -> Hashtbl.add symbol_tbl id (Function, PFunction(pt, ags, e)))
# 1776 "parser.ml"
         in
        _menhir_goto_declare _menhir_env _menhir_stack _menhir_s _v
    | MenhirState264 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Property ->
            _menhir_run266 _menhir_env (Obj.magic _menhir_stack) MenhirState265
        | RB3 ->
            _menhir_reduce84 _menhir_env (Obj.magic _menhir_stack) MenhirState265
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState265)
    | _ ->
        _menhir_fail ()

and _menhir_goto_str_expr_list : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.pexpr_loc) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Iden _v ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RB3 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__5_ = _endpos in
            let ((((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _startpos__3_), _, (str_el : ((string * Ast.pexpr_loc) list))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos__5_ in
            let _v : (Ast.pexpr_loc) = 
# 445 "parser.mly"
                                                              (mk_pexpr_loc (PWith (e1, str_el)) e1.ptyp _startpos_e1_ _endpos__5_)
# 1821 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Iden _v ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RB3 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (str_el : ((string * Ast.pexpr_loc) list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.pexpr_loc) = 
# 245 "parser.mly"
                                     (
            let str_elt = List.map (fun (str, (pel:pexpr_loc)) -> (str, pel.ptyp)) str_el in
            mk_pexpr_loc (PRecord str_el) (PTRecord str_elt) _startpos__1_ _endpos__3_
        )
# 1854 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr_path : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (string list) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState333 | MenhirState329 | MenhirState323 | MenhirState319 | MenhirState315 | MenhirState309 | MenhirState271 | MenhirState273 | MenhirState264 | MenhirState258 | MenhirState229 | MenhirState219 | MenhirState39 | MenhirState40 | MenhirState212 | MenhirState43 | MenhirState46 | MenhirState47 | MenhirState49 | MenhirState50 | MenhirState51 | MenhirState52 | MenhirState199 | MenhirState55 | MenhirState56 | MenhirState57 | MenhirState155 | MenhirState151 | MenhirState58 | MenhirState61 | MenhirState144 | MenhirState142 | MenhirState62 | MenhirState128 | MenhirState133 | MenhirState137 | MenhirState131 | MenhirState129 | MenhirState66 | MenhirState118 | MenhirState121 | MenhirState75 | MenhirState77 | MenhirState83 | MenhirState85 | MenhirState87 | MenhirState89 | MenhirState92 | MenhirState94 | MenhirState114 | MenhirState112 | MenhirState110 | MenhirState108 | MenhirState100 | MenhirState102 | MenhirState106 | MenhirState104 | MenhirState98 | MenhirState96 | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__1_ = _endpos in
        let (_1 : (string list)) = _v in
        let _startpos__1_ = _startpos in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (Ast.pexpr_loc) = 
# 203 "parser.mly"
                       (mk_pexpr_loc (PSymbol _1) (PTVar (new_type_var ())) _startpos__1_ _endpos__1_)
# 1880 "parser.ml"
         in
        _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__3_ = _endpos in
        let (_3 : (string list)) = _v in
        let _startpos__3_ = _startpos in
        let ((_menhir_stack, _endpos_id_, _menhir_s, (id : (
# 35 "parser.mly"
       (string)
# 1892 "parser.ml"
        )), _startpos_id_), _) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos_id_ in
        let _endpos = _endpos__3_ in
        let _v : (string list) = 
# 200 "parser.mly"
                              (id::_3)
# 1900 "parser.ml"
         in
        _menhir_goto_expr_path _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | MenhirState206 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__3_ = _endpos in
        let (_3 : (string list)) = _v in
        let _startpos__3_ = _startpos in
        let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 35 "parser.mly"
       (string)
# 1912 "parser.ml"
        )), _startpos__1_), _) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (Ast.pexpr_loc) = 
# 212 "parser.mly"
                          (mk_pexpr_loc (PSymbol (_1::_3)) (PTVar (new_type_var ())) _startpos__1_ _endpos__3_)
# 1920 "parser.ml"
         in
        _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | _ ->
        _menhir_fail ()

and _menhir_goto_formula : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (Ast.pformula_loc) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState303 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | And ->
            _menhir_run307 _menhir_env (Obj.magic _menhir_stack)
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState309 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState309 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState309 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState309 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState309 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState309 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState309)
        | Or ->
            _menhir_run305 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState305 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | And ->
            _menhir_run307 _menhir_env (Obj.magic _menhir_stack)
        | Comma | Or | Property | RB3 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (Ast.pformula_loc)), _startpos__1_), _endpos__3_, _, (_3 : (Ast.pformula_loc)), _startpos__3_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.pformula_loc) = 
# 138 "parser.mly"
                            (mk_pformula_loc (POr (_1, _3)) _startpos__1_ _endpos__3_)
# 2006 "parser.ml"
             in
            _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState307 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (Ast.pformula_loc)), _startpos__1_), _endpos__3_, _, (_3 : (Ast.pformula_loc)), _startpos__3_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (Ast.pformula_loc) = 
# 137 "parser.mly"
                            (mk_pformula_loc (PAnd (_1, _3)) _startpos__1_ _endpos__3_)
# 2025 "parser.ml"
         in
        _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | MenhirState299 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | And ->
            _menhir_run307 _menhir_env (Obj.magic _menhir_stack)
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AF ->
                _menhir_run300 _menhir_env (Obj.magic _menhir_stack) MenhirState313 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | AR ->
                _menhir_run294 _menhir_env (Obj.magic _menhir_stack) MenhirState313 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | AX ->
                _menhir_run290 _menhir_env (Obj.magic _menhir_stack) MenhirState313 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Bottom ->
                _menhir_run289 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState313 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EG ->
                _menhir_run285 _menhir_env (Obj.magic _menhir_stack) MenhirState313 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EU ->
                _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState313 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EX ->
                _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState313 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run271 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState313 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Neg ->
                _menhir_run270 _menhir_env (Obj.magic _menhir_stack) MenhirState313 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Top ->
                _menhir_run269 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState313 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState313)
        | Or ->
            _menhir_run305 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState313 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | And ->
            _menhir_run307 _menhir_env (Obj.magic _menhir_stack)
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState315 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState315 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState315 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState315 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState315)
        | Or ->
            _menhir_run305 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState293 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | And ->
            _menhir_run307 _menhir_env (Obj.magic _menhir_stack)
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState319 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState319 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState319 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState319 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState319 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState319 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState319)
        | Or ->
            _menhir_run305 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState288 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | And ->
            _menhir_run307 _menhir_env (Obj.magic _menhir_stack)
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState323 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState323 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState323 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState323 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState323 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState323 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState323)
        | Or ->
            _menhir_run305 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState284 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | And ->
            _menhir_run307 _menhir_env (Obj.magic _menhir_stack)
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AF ->
                _menhir_run300 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | AR ->
                _menhir_run294 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | AX ->
                _menhir_run290 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Bottom ->
                _menhir_run289 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState327 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EG ->
                _menhir_run285 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EU ->
                _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EX ->
                _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run271 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState327 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Neg ->
                _menhir_run270 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Top ->
                _menhir_run269 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState327 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState327)
        | Or ->
            _menhir_run305 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState327 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | And ->
            _menhir_run307 _menhir_env (Obj.magic _menhir_stack)
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState329 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState329 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState329 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState329 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState329 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState329 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState329)
        | Or ->
            _menhir_run305 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState278 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | And ->
            _menhir_run307 _menhir_env (Obj.magic _menhir_stack)
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState333 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState333 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState333 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState333 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState333 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState333 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState333)
        | Or ->
            _menhir_run305 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState270 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : (Ast.pformula_loc)), _startpos__2_) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (Ast.pformula_loc) = 
# 136 "parser.mly"
                    (mk_pformula_loc (PNeg _2) _startpos__1_ _endpos__2_)
# 2426 "parser.ml"
         in
        _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | MenhirState268 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | And ->
            _menhir_run307 _menhir_env (Obj.magic _menhir_stack)
        | Or ->
            _menhir_run305 _menhir_env (Obj.magic _menhir_stack)
        | Property | RB3 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 2443 "parser.ml"
            )), _startpos_id_), _endpos_fml_, _, (fml : (Ast.pformula_loc)), _startpos_fml_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (string * Ast.pformula_loc) = 
# 130 "parser.mly"
                                                  ((id, fml))
# 2450 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Property ->
                _menhir_run266 _menhir_env (Obj.magic _menhir_stack) MenhirState338
            | RB3 ->
                _menhir_reduce84 _menhir_env (Obj.magic _menhir_stack) MenhirState338
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState338)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce82 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : (Ast.pexpr_loc list) = 
# 185 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 2480 "parser.ml"
     in
    _menhir_goto_list_expr_single_ _menhir_env _menhir_stack _endpos _menhir_s _v

and _menhir_goto_declare : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (unit))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 42 "parser.mly"
        (unit)
# 2501 "parser.ml"
            ) = 
# 65 "parser.mly"
                   ()
# 2505 "parser.ml"
             in
            _menhir_goto_debug _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState345 | MenhirState250 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Datatype ->
            _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState345
        | Function ->
            _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState345
        | Val ->
            _menhir_run216 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Var ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EOF | Model ->
            _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack) MenhirState345
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState345)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_Vertical_ : _menhir_env -> 'ttv_tail -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Float _v ->
        _menhir_run177 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState201 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run176 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState201 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run175 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState201 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run170 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState201 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Underline ->
        _menhir_run169 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState201

and _menhir_run71 : _menhir_env -> ('ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position) * _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RB3 ->
        _menhir_reduce128 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_goto_expr_single_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.pexpr_loc list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState56 | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RB2 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (el : (Ast.pexpr_loc list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.pexpr_loc) = 
# 228 "parser.mly"
                                       (
            if List.length el = 0 then
                mk_pexpr_loc (PLst el) (PTLst (PTVar (new_type_var ()))) _startpos__1_ _endpos__3_
            else begin
                let e0 = List.hd el in
                mk_pexpr_loc (PLst el) (PTLst e0.ptyp) _startpos__1_ _endpos__3_
                (*match e0.ptyp with
                | None -> mk_pexpr_loc (PLst el) None $startpos($1) $endpos($3)
                | Some t -> mk_pexpr_loc (PLst el) (Some (PTLst (Some t))) $startpos($1) $endpos($3)*)
            end
        )
# 2610 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (Ast.pexpr_loc)), _startpos__1_), _), _, (_3 : (Ast.pexpr_loc list))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.pexpr_loc list) = 
# 471 "parser.mly"
                                             (_1::_3)
# 2627 "parser.ml"
         in
        _menhir_goto_expr_single_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Vertical ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RB2 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos__5_ = _endpos in
                let (((_menhir_stack, _menhir_s, _startpos__1_), _), _, (el : (Ast.pexpr_loc list))) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _startpos = _startpos__1_ in
                let _endpos = _endpos__5_ in
                let _v : (Ast.pexpr_loc) = 
# 216 "parser.mly"
                                                        (
            let ea = el in
            if List.length ea = 0 then
                mk_pexpr_loc (PAray ea) (PTAray (PTVar (new_type_var ()))) _startpos__1_ _endpos__5_
            else begin
                let e0 = List.hd ea in
                mk_pexpr_loc (PAray ea) (PTAray e0.ptyp) _startpos__1_ _endpos__5_
                (*match e0.ptyp with
                | None -> mk_pexpr_loc (PAray ea) None $startpos($1) $endpos($5)
                | Some t -> mk_pexpr_loc (PAray ea) (Some (PTAray (Some t))) $startpos($1) $endpos($5) *)
            end 
        )
# 2667 "parser.ml"
                 in
                _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run137 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState137 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState137 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState137 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState137 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RB2 | Vertical ->
        _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137

and _menhir_reduce35 : _menhir_env -> ('ttv_tail * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos_e_ in
    let _v : (Ast.pexpr_loc) = 
# 283 "parser.mly"
                                       (
            mk_pexpr_loc (PNegi e) (PTInt) _startpos__1_ _endpos_e_
            (*match e.ptyp with
            | None | Some PTInt ->
                e.ptyp <- Some PTInt;
                mk_pexpr_loc (PNegi e) (Some PTInt) $startpos($1) $endpos(e)
            | Some t -> raise (Type_mismatch (e, t, PTInt))*)
        )
# 2750 "parser.ml"
     in
    _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_reduce36 : _menhir_env -> ('ttv_tail * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos_e_ in
    let _v : (Ast.pexpr_loc) = 
# 291 "parser.mly"
                                          (
            mk_pexpr_loc (PNegf e) PTFloat _startpos__1_ _endpos_e_
        )
# 2765 "parser.ml"
     in
    _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_goto_nonempty_list_expr_single_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (Ast.pexpr_loc list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    match _menhir_s with
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_el_ = _endpos in
        let (el : (Ast.pexpr_loc list)) = _v in
        let (_menhir_stack, _endpos_id_, _menhir_s, (id : (
# 35 "parser.mly"
       (string)
# 2780 "parser.ml"
        )), _startpos_id_) = _menhir_stack in
        let _startpos = _startpos_id_ in
        let _endpos = _endpos_el_ in
        let _v : (Ast.pexpr_loc) = 
# 453 "parser.mly"
                                                (
            mk_pexpr_loc (PApply (id, el)) (PTVar (new_type_var ())) _startpos_id_ _endpos_el_
        )
# 2789 "parser.ml"
         in
        _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_xs_ = _endpos in
        let (xs : (Ast.pexpr_loc list)) = _v in
        let (_menhir_stack, _endpos_x_, _menhir_s, (x : (Ast.pexpr_loc)), _startpos_x_) = _menhir_stack in
        let _endpos = _endpos_xs_ in
        let _v : (Ast.pexpr_loc list) = 
# 197 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 2802 "parser.ml"
         in
        _menhir_goto_nonempty_list_expr_single_ _menhir_env _menhir_stack _endpos _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run129 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129

and _menhir_run131 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState131 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState131 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState131 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState131 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131

and _menhir_run133 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Vertical ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RB2 ->
        _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133

and _menhir_run91 : _menhir_env -> (('ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__4_ = _endpos in
    let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _, _startpos__2_), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _startpos = _startpos_e1_ in
    let _endpos = _endpos__4_ in
    let _v : (Ast.pexpr_loc) = 
# 458 "parser.mly"
                                                (
        let e:Ast.pexpr_loc = e1 in
        let et1 = e.ptyp in
        match et1 with
        | PTAray pt -> mk_pexpr_loc (PAray_Field (e1, e2)) pt _startpos_e1_ _endpos__4_
        | PTVar _ -> mk_pexpr_loc (PAray_Field (e1, e2)) (PTVar (new_type_var ())) _startpos_e1_ _endpos__4_
        | _ -> raise (Type_mismatch (e1, et1, (PTAray (PTVar (new_type_var())))))        
        )
# 2973 "parser.ml"
     in
    _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_goto_separated_nonempty_list_Semicolon_expr_single_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (Ast.pexpr_loc list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    match _menhir_s with
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_el_ = _endpos in
        let (el : (Ast.pexpr_loc list)) = _v in
        let ((_menhir_stack, _endpos_e_, _menhir_s, (e : (Ast.pexpr_loc)), _startpos_e_), _) = _menhir_stack in
        let _2 = () in
        let _endpos = _endpos_el_ in
        let _v : (Ast.pexpr_loc) = 
# 194 "parser.mly"
                                                                                        (
            mk_pexpr_loc (PSeq (e::el)) (PTVar (new_type_var())) _startpos_e_ _endpos_el_
        )
# 2993 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_xs_ = _endpos in
        let (xs : (Ast.pexpr_loc list)) = _v in
        let ((_menhir_stack, _endpos_x_, _menhir_s, (x : (Ast.pexpr_loc)), _startpos_x_), _) = _menhir_stack in
        let _2 = () in
        let _endpos = _endpos_xs_ in
        let _v : (Ast.pexpr_loc list) = 
# 217 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 3007 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_Semicolon_expr_single_ _menhir_env _menhir_stack _endpos _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce17 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : (Ast.pexpr_loc)), _startpos__1_) = _menhir_stack in
    let _endpos = _endpos__1_ in
    let _v : (Ast.pexpr_loc) = 
# 193 "parser.mly"
                  (_1)
# 3020 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v

and _menhir_run77 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run70 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LB3 ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run83 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run85 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run87 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_run92 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92

and _menhir_run96 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96

and _menhir_run98 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState98 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState98 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState98 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState98 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98

and _menhir_run100 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100

and _menhir_run108 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState108 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState108 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState108 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState108 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108

and _menhir_run89 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_run94 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94

and _menhir_run110 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState110 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState110 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState110 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState110 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110

and _menhir_run112 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState112 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState112 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState112 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState112 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112

and _menhir_run102 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102

and _menhir_run114 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114

and _menhir_run104 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState104 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState104 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState104 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState104 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104

and _menhir_run106 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106

and _menhir_goto_constrs : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.ptyp option) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState236 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : ((string * Ast.ptyp option) list)) = _v in
        let _v : (Ast.ptyp) = 
# 160 "parser.mly"
              (PTConstrs _1)
# 3848 "parser.ml"
         in
        _menhir_goto_type_def _menhir_env _menhir_stack _menhir_s _v
    | MenhirState243 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : ((string * Ast.ptyp option) list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (string * Ast.ptyp option))) = _menhir_stack in
        let _2 = () in
        let _v : ((string * Ast.ptyp option) list) = 
# 156 "parser.mly"
                               (_1 :: _3)
# 3860 "parser.ml"
         in
        _menhir_goto_constrs _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_type_def : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ptyp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (t : (Ast.ptyp)) = _v in
    let (((_menhir_stack, _menhir_s), _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 3874 "parser.ml"
    )), _startpos_id_), _, (args : (string list))) = _menhir_stack in
    let _4 = () in
    let _1 = () in
    let _v : (unit) = 
# 84 "parser.mly"
                                                                  (
        Hashtbl.add symbol_tbl id (UDT, PTyp (erase_type_args t args)); 
        print_endline ("declared udt "^id))
# 3883 "parser.ml"
     in
    _menhir_goto_declare _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_nonempty_list_str_typ_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.ptyp) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Ast.ptyp))), _, (xs : ((string * Ast.ptyp) list))) = _menhir_stack in
        let _v : ((string * Ast.ptyp) list) = 
# 197 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 3898 "parser.ml"
         in
        _menhir_goto_nonempty_list_str_typ_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RB3 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (str_pts : ((string * Ast.ptyp) list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : ((string * Ast.ptyp) list) = 
# 186 "parser.mly"
                                                     (str_pts)
# 3918 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : ((string * Ast.ptyp) list)) = _v in
            let _v : (Ast.ptyp) = 
# 177 "parser.mly"
                 (PTRecord _1)
# 3926 "parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_tuple_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ptyp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (Ast.ptyp))), _), _, (_3 : (Ast.ptyp list))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ptyp list) = 
# 183 "parser.mly"
                          (_1 :: _3)
# 3950 "parser.ml"
         in
        _menhir_goto_tuple_typ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (_2 : (Ast.ptyp list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ptyp) = 
# 176 "parser.mly"
                        (PTTuple _2)
# 3970 "parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ptyp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState23 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TAray ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | TBool ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | TFloat ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | TInt ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | TLst ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | TUnt ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run16 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ptyp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TAray ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | TBool ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | TFloat ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | TInt ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | TLst ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | TUnt ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_goto_list_typ_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ptyp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.ptyp list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.ptyp))) = _menhir_stack in
        let _v : (Ast.ptyp list) = 
# 187 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 4051 "parser.ml"
         in
        _menhir_goto_list_typ_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (tl : (Ast.ptyp list)) = _v in
        let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 35 "parser.mly"
       (string)
# 4061 "parser.ml"
        )), _startpos__1_) = _menhir_stack in
        let _v : (Ast.ptyp) = 
# 175 "parser.mly"
                          (PTUdt (_1, tl))
# 4066 "parser.ml"
         in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run125 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "parser.mly"
       (string)
# 4075 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Dot ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | Add | AddDot | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | Equal | False | Float _ | For | Function | GE | GT | Iden _ | If | Int _ | LArrow | LB1 | LB2 | LB3 | LE | LT | Match | Minus | MinusDot | Model | Mult | MultDot | Negb | Non_Equal | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While | With ->
        _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125

and _menhir_reduce128 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Ast.pexpr_loc) list) = 
# 474 "parser.mly"
                ([])
# 4096 "parser.ml"
     in
    _menhir_goto_str_expr_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run54 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "parser.mly"
       (string)
# 4103 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Equal ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | False ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Float _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | For ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Iden _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | If ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Int _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB1 ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB2 ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB3 ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Match ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Negb ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | True ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIden _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Val ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Var ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | While ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.pexpr_loc list) = 
# 469 "parser.mly"
                    ([])
# 4167 "parser.ml"
     in
    _menhir_goto_expr_single_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Vertical ->
        _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_reduce19 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 35 "parser.mly"
       (string)
# 4223 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _endpos_id_, _menhir_s, (id : (
# 35 "parser.mly"
       (string)
# 4229 "parser.ml"
    )), _startpos_id_) = _menhir_stack in
    let _startpos = _startpos_id_ in
    let _endpos = _endpos_id_ in
    let _v : (string list) = 
# 199 "parser.mly"
                     ([id])
# 4236 "parser.ml"
     in
    _menhir_goto_expr_path _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run124 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 35 "parser.mly"
       (string)
# 4243 "parser.ml"
) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState124 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124

and _menhir_goto_expr_single : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (Ast.pexpr_loc) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | DotDot ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState69 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | MenhirState264 | MenhirState229 | MenhirState212 | MenhirState199 | MenhirState55 | MenhirState155 | MenhirState151 | MenhirState144 | MenhirState142 | MenhirState121 | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | Semicolon ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | And | Comma | Datatype | Do | Done | DotDot | EOF | Else | False | Float _ | For | Function | Iden _ | If | Int _ | LB1 | LB3 | Match | Model | Negb | Or | Property | RB1 | RB2 | RB3 | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | MenhirState80 | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState79 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | And | Comma | Datatype | Do | Done | DotDot | EOF | Else | False | Float _ | For | Function | Iden _ | If | Int _ | LB1 | LB3 | Match | Model | Negb | Or | Property | RB1 | RB2 | RB3 | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos_x_, _menhir_s, (x : (Ast.pexpr_loc)), _startpos_x_) = _menhir_stack in
            let _endpos = _endpos_x_ in
            let _v : (Ast.pexpr_loc list) = 
# 215 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 4491 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_Semicolon_expr_single_ _menhir_env _menhir_stack _endpos _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | And | Comma | Datatype | Do | Done | DotDot | EOF | Else | False | Float _ | For | Function | Iden _ | If | Int _ | LB1 | LB3 | Match | Model | Negb | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 270 "parser.mly"
                                             (
            mk_pexpr_loc (POro (e1, e2)) (PTBool) _startpos_e1_ _endpos_e2_
            (*match e1.ptyp, e2.ptyp with
            | None, None | None, Some PTBool | Some PTBool, None | Some PTBool, Some PTBool ->
                e1.ptyp <- Some PTBool;
                e2.ptyp <- Some PTBool;
                mk_pexpr_loc (POro (e1, e2)) (Some PTBool) $startpos(e1) $endpos(e2)
            | Some t, None -> raise (Type_mismatch (e1, t, PTBool)) 
            | Some t, Some PTBool -> raise ((Type_mismatch (e1, t, PTBool)))
            | None, Some t -> raise (Type_mismatch (e2, t, PTBool))
            | Some PTBool, Some t -> raise (Type_mismatch (e2, t, PTBool))
            | Some t1, Some t2 -> raise (Type_mismatch (e1, t1, PTBool))*)
        )
# 4556 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | False | Float _ | For | Function | GE | GT | Iden _ | If | Int _ | LB1 | LB3 | LE | LT | Match | Model | Negb | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 373 "parser.mly"
                                                  (mk_pexpr_loc (PNon_Equal (e1, e2)) (PTBool) _startpos_e1_ _endpos_e2_)
# 4595 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | Add | AddDot | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | Equal | False | Float _ | For | Function | GE | GT | Iden _ | If | Int _ | LB1 | LB3 | LE | LT | Match | Minus | MinusDot | Model | Mult | MultDot | Negb | Non_Equal | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 359 "parser.mly"
                                                (
            mk_pexpr_loc (PMultDot (e1, e2)) (PTFloat) _startpos_e1_ _endpos_e2_
            (*match e1.ptyp, e2.ptyp with
            | None, None | None, Some PTFloat | Some PTFloat, None | Some PTFloat, Some PTFloat ->
                e1.ptyp <- Some PTFloat;
                e2.ptyp <- Some PTFloat;
                mk_pexpr_loc (PMultDot (e1, e2)) (Some PTFloat) $startpos(e1) $endpos(e2)
            | Some t, None -> raise (Type_mismatch (e1, t, PTFloat))
            | Some t, Some PTFloat -> raise (Type_mismatch (e1, t, PTFloat))
            | None, Some t -> raise (Type_mismatch (e2, t, PTFloat))
            | Some PTFloat, Some t -> raise (Type_mismatch (e2, t, PTFloat))
            | Some t1, Some t2 -> raise (Type_mismatch (e1, t1, PTFloat))*)
        )
# 4634 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | RB2 ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | Add | AddDot | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | Equal | False | Float _ | For | Function | GE | GT | Iden _ | If | Int _ | LB1 | LB3 | LE | LT | Match | Minus | MinusDot | Model | Mult | MultDot | Negb | Non_Equal | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 320 "parser.mly"
                                             (
            mk_pexpr_loc (PMult (e1, e2)) (PTInt) _startpos_e1_ _endpos_e2_
            (*match e1.ptyp, e2.ptyp with
            | None, None | None, Some PTInt | Some PTInt, None | Some PTInt, Some PTInt ->
                e1.ptyp <- Some PTInt;
                e2.ptyp <- Some PTInt;
                mk_pexpr_loc (PMult (e1, e2)) (Some PTInt) $startpos(e1) $endpos(e2)
            | Some t, None -> raise (Type_mismatch (e1, t, PTInt)) 
            | Some t, Some PTInt -> raise (Type_mismatch (e1, t, PTInt))
            | None, Some t -> raise (Type_mismatch (e2, t, PTInt))
            | Some PTInt, Some t -> raise (Type_mismatch (e2, t, PTInt))
            | Some t1, Some t2 -> raise (Type_mismatch (e1, t1, PTInt))*)
        )
# 4718 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | And | Comma | Datatype | Do | Done | DotDot | EOF | Else | False | Float _ | For | Function | Iden _ | If | Int _ | LB1 | LB3 | Match | Model | Negb | Or | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 443 "parser.mly"
                                                  (mk_pexpr_loc (PAssign (e1, e2)) (PTUnt) _startpos_e1_ _endpos_e2_)
# 4773 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | Add | AddDot | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | Equal | False | Float _ | For | Function | GE | GT | Iden _ | If | Int _ | LB1 | LB3 | LE | LT | Match | Minus | MinusDot | Model | Negb | Non_Equal | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _, _startpos__2_), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 346 "parser.mly"
                                                 (
            mk_pexpr_loc (PMinusDot (e1, e2)) (PTFloat) _startpos_e1_ _endpos_e2_
            (*match e1.ptyp, e2.ptyp with
            | None, None | None, Some PTFloat | Some PTFloat, None | Some PTFloat, Some PTFloat ->
                e1.ptyp <- Some PTFloat;
                e2.ptyp <- Some PTFloat;
                mk_pexpr_loc (PMinusDot (e1, e2)) (Some PTFloat) $startpos(e1) $endpos(e2)
            | Some t, None -> raise (Type_mismatch (e1, t, PTFloat)) 
            | Some t, Some PTFloat -> raise (Type_mismatch (e1, t, PTFloat))
            | None, Some t -> raise (Type_mismatch (e2, t, PTFloat))
            | Some PTFloat, Some t -> raise (Type_mismatch (e2, t, PTFloat))
            | Some t1, Some t2 -> raise (Type_mismatch (e1, t1, PTFloat))*)
        )
# 4816 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | Add | AddDot | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | Equal | False | Float _ | For | Function | GE | GT | Iden _ | If | Int _ | LB1 | LB3 | LE | LT | Match | Minus | MinusDot | Model | Negb | Non_Equal | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _, _startpos__2_), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 307 "parser.mly"
                                              (
            mk_pexpr_loc (PMinus (e1, e2)) (PTInt) _startpos_e1_ _endpos_e2_
            (*match e1.ptyp, e2.ptyp with
            | None, None | None, Some PTInt | Some PTInt, None | Some PTInt, Some PTInt ->
                e1.ptyp <- Some PTInt;
                e2.ptyp <- Some PTInt;
                mk_pexpr_loc (PMinus (e1, e2)) (Some PTInt) $startpos(e1) $endpos(e2)
            | Some t, None -> raise (Type_mismatch (e1, t, PTInt)) 
            | Some t, Some PTInt -> raise (Type_mismatch (e1, t, PTInt))
            | None, Some t -> raise (Type_mismatch (e2, t, PTInt))
            | Some PTInt, Some t -> raise (Type_mismatch (e2, t, PTInt))
            | Some t1, Some t2 -> raise (Type_mismatch (e1, t1, PTInt))*)
        )
# 4859 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | False | Float _ | For | Function | Iden _ | If | Int _ | LB1 | LB3 | Match | Model | Negb | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 374 "parser.mly"
                                              (mk_pexpr_loc (PLT (e1, e2)) (PTBool) _startpos_e1_ _endpos_e2_)
# 4902 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | False | Float _ | For | Function | GE | GT | Iden _ | If | Int _ | LB1 | LB3 | LE | LT | Match | Model | Negb | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 372 "parser.mly"
                                              (mk_pexpr_loc (PEqual (e1, e2)) (PTBool) _startpos_e1_ _endpos_e2_)
# 4941 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | Add | AddDot | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | Equal | False | Float _ | For | Function | GE | GT | Iden _ | If | Int _ | LB1 | LB3 | LE | LT | Match | Model | Negb | Non_Equal | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 333 "parser.mly"
                                               (
            mk_pexpr_loc (PAddDot (e1, e2)) (PTFloat) _startpos_e1_ _endpos_e2_
            (*match e1.ptyp, e2.ptyp with
            | None, None | None, Some PTFloat | Some PTFloat, None | Some PTFloat, Some PTFloat ->
                e1.ptyp <- Some PTFloat;
                e2.ptyp <- Some PTFloat;
                mk_pexpr_loc (PAddDot (e1, e2)) (Some PTFloat) $startpos(e1) $endpos(e2)
            | Some t, None -> raise (Type_mismatch (e1, t, PTFloat))
            | Some t, Some PTFloat -> raise (Type_mismatch (e1, t, PTFloat))
            | None, Some t -> raise (Type_mismatch (e2, t, PTFloat))
            | Some PTFloat, Some t -> raise (Type_mismatch (e2, t, PTFloat))
            | Some t1, Some t2 -> raise (Type_mismatch (e1, t1, PTFloat))*)
        )
# 4988 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105)
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | Add | AddDot | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | Equal | False | Float _ | For | Function | GE | GT | Iden _ | If | Int _ | LB1 | LB3 | LE | LT | Match | Model | Negb | Non_Equal | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 294 "parser.mly"
                                            (
            mk_pexpr_loc (PAdd (e1, e2)) (PTInt) _startpos_e1_ _endpos_e2_
            (*match e1.ptyp, e2.ptyp with
            | None, None | None, Some PTInt | Some PTInt, None | Some PTInt, Some PTInt ->
                e1.ptyp <- Some PTInt;
                e2.ptyp <- Some PTInt;
                mk_pexpr_loc (PAdd (e1, e2)) (Some PTInt) $startpos(e1) $endpos(e2)
            | Some t, None -> raise (Type_mismatch (e1, t, PTInt)) 
            | Some t, Some PTInt -> raise (Type_mismatch (e1, t, PTInt))
            | None, Some t -> raise (Type_mismatch (e2, t, PTInt))
            | Some PTInt, Some t -> raise (Type_mismatch (e2, t, PTInt))
            | Some t1, Some t2 -> raise (Type_mismatch (e1, t1, PTInt))*)
        )
# 5035 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | False | Float _ | For | Function | Iden _ | If | Int _ | LB1 | LB3 | Match | Model | Negb | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 376 "parser.mly"
                                              (mk_pexpr_loc (PLE (e1, e2)) (PTBool) _startpos_e1_ _endpos_e2_)
# 5078 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | False | Float _ | For | Function | Iden _ | If | Int _ | LB1 | LB3 | Match | Model | Negb | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 375 "parser.mly"
                                              (mk_pexpr_loc (PGT (e1, e2)) (PTBool) _startpos_e1_ _endpos_e2_)
# 5121 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | False | Float _ | For | Function | Iden _ | If | Int _ | LB1 | LB3 | Match | Model | Negb | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 377 "parser.mly"
                                              (mk_pexpr_loc (PGE (e1, e2)) (PTBool) _startpos_e1_ _endpos_e2_)
# 5164 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | False | Float _ | For | Function | Iden _ | If | Int _ | LB1 | LB3 | Match | Model | Negb | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 257 "parser.mly"
                                              (
            mk_pexpr_loc (PAndo (e1, e2)) (PTBool) _startpos_e1_ _endpos_e2_
            (*match e1.ptyp, e2.ptyp with
            | None, None | None, Some PTBool | Some PTBool, None | Some PTBool, Some PTBool ->
                e1.ptyp <- Some PTBool;
                e2.ptyp <- Some PTBool;
                mk_pexpr_loc (PAndo (e1, e2)) (Some PTBool) $startpos(e1) $endpos(e2)
            | Some t, None -> raise (Type_mismatch (e1, t, PTBool)) 
            | Some t, Some PTBool -> raise (Type_mismatch (e1, t, PTBool))
            | None, Some t -> raise (Type_mismatch (e2, t, PTBool))
            | Some PTBool, Some t -> raise (Type_mismatch (e2, t, PTBool))
            | Some t1, Some t2 -> raise (Type_mismatch (e1, t1, PTBool))*)
        )
# 5227 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | RB2 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_s = MenhirState119 in
            let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Do ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | False ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Float _v ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | For ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Iden _v ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | If ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Int _v ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LB1 ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LB2 ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LB3 ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Match ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Minus ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MinusDot ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Negb ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | True ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UIden _v ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Val ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Var ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | While ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
    | MenhirState128 | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | False ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Float _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState128 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | For ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | Iden _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState128 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | If ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Int _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState128 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | LB1 ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB2 ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB3 ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | Match ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | Negb ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | True ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIden _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState128 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Val ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Var ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | While ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | And | Comma | Datatype | Do | Done | DotDot | EOF | Else | Function | Model | Or | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | Vertical ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos_x_, _menhir_s, (x : (Ast.pexpr_loc)), _startpos_x_) = _menhir_stack in
            let _endpos = _endpos_x_ in
            let _v : (Ast.pexpr_loc list) = 
# 195 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 5412 "parser.ml"
             in
            _menhir_goto_nonempty_list_expr_single_ _menhir_env _menhir_stack _endpos _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128)
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | And | Comma | Datatype | Do | Done | DotDot | EOF | Else | False | Float _ | For | Function | Iden _ | If | Int _ | LB1 | LB3 | Match | Model | Negb | Or | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | And | Comma | Datatype | Do | Done | DotDot | EOF | Else | False | Float _ | For | Function | Iden _ | If | Int _ | LB1 | LB3 | Match | Model | Negb | Or | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132)
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | RB2 ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState136
        | Semicolon ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
    | MenhirState56 | MenhirState57 | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | Semicolon ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | RB2 | Vertical ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : (Ast.pexpr_loc)), _startpos__1_) = _menhir_stack in
            let _v : (Ast.pexpr_loc list) = 
# 470 "parser.mly"
                    ([_1])
# 5603 "parser.ml"
             in
            _menhir_goto_expr_single_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | Then ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState141 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142)
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_s = MenhirState148 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : (Ast.pexpr_loc)), _startpos__2_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.pexpr_loc) = 
# 466 "parser.mly"
                           (_2)
# 5752 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | Semicolon ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | Comma ->
            _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | With ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState164 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LB3 ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Vertical ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let x = () in
                let _v : (unit option) = 
# 102 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 5819 "parser.ml"
                 in
                _menhir_goto_option_Vertical_ _menhir_env _menhir_stack _v
            | Float _ | Iden _ | Int _ | LB1 | LB2 | UIden _ | Underline ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _v : (unit option) = 
# 100 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( None )
# 5827 "parser.ml"
                 in
                _menhir_goto_option_Vertical_ _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState164)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState203
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState203
        | Add | AddDot | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | Equal | False | Float _ | For | Function | GE | GT | Iden _ | If | Int _ | LB1 | LB3 | LE | LT | Match | Minus | MinusDot | Model | Mult | MultDot | Negb | Non_Equal | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState203)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState204
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState204 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState204
        | Add | AddDot | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | Equal | False | Float _ | For | Function | GE | GT | Iden _ | If | Int _ | LB1 | LB3 | LE | LT | Match | Minus | MinusDot | Model | Mult | MultDot | Negb | Non_Equal | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState204)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | False | Float _ | For | Function | Iden _ | If | Int _ | LB1 | LB3 | Match | Model | Negb | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _v : (Ast.pexpr_loc) = 
# 249 "parser.mly"
                               (
            mk_pexpr_loc (PNegb e) (PTBool) _startpos__1_ _endpos_e_
            (*match e.ptyp with
            | None | Some PTBool -> 
                e.ptyp <- Some PTBool; 
                mk_pexpr_loc (PNegb e) (Some PTBool) $startpos($1) $endpos(e)
            | Some t -> raise (Type_mismatch (e, t, PTBool))*)
        )
# 5925 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState205)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | And | Comma | Datatype | Do | Done | DotDot | EOF | Else | False | Float _ | For | Function | Iden _ | If | Int _ | LB1 | LB3 | Match | Model | Negb | Or | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_uid_, _menhir_s, (uid : (
# 35 "parser.mly"
       (string)
# 5976 "parser.ml"
            )), _startpos_uid_), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
            let _startpos = _startpos_uid_ in
            let _endpos = _endpos_e_ in
            let _v : (Ast.pexpr_loc) = 
# 447 "parser.mly"
                                  (
            mk_pexpr_loc (PConstr ((PConstr_compound (uid, e)))) (PTVar (new_type_var ())) _startpos_uid_ _endpos_e_
            (*match eo with
            | None -> mk_pexpr_loc (PConstr (mk_pconstr_loc (PConstr_basic uid) $startpos(uid) $endpos(eo))) None $startpos(uid) $endpos(eo)
            | Some e -> *)
        )
# 5988 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState208)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | False | Float _ | For | Function | GE | GT | Iden _ | If | Int _ | LB1 | LB3 | LE | LT | Match | Model | Negb | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 6023 "parser.ml"
            )), _startpos_id_), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _v : (Ast.pexpr_loc) = 
# 456 "parser.mly"
                                            (mk_pexpr_loc (PLocal_Val (id, e)) (PTUnt) _startpos__1_ _endpos_e_)
# 6032 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState209)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState210
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState210
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState210
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState210
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState210
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState210
        | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | False | Float _ | For | Function | GE | GT | Iden _ | If | Int _ | LB1 | LB3 | LE | LT | Match | Model | Negb | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 6067 "parser.ml"
            )), _startpos_id_), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _v : (Ast.pexpr_loc) = 
# 457 "parser.mly"
                                            (mk_pexpr_loc (PLocal_Var (id, e)) (PTUnt) _startpos__1_ _endpos_e_)
# 6076 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState210)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | Do ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState211 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState212 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState212 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState212 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState212 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState212 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState212 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState212)
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState211 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState211 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState211 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState211)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState215 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState215 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState215 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | Datatype | EOF | Function | Model | Val | Var ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, _startpos__1_), _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 6217 "parser.ml"
            )), _startpos_id_), _, (ote : (Ast.ptyp option))), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : (unit) = 
# 87 "parser.mly"
                                                                       (
            print_endline ("declaring variable "^id);
            match ote with
            | None -> Hashtbl.add symbol_tbl id (Var, PExpr_loc (PTVar (new_type_var ()), e))
            | Some pt -> Hashtbl.add symbol_tbl id (Var, PExpr_loc (pt, e))
        )
# 6229 "parser.ml"
             in
            _menhir_goto_declare _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState215)
    | MenhirState219 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState220
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState220
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState220
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState220
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState220
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState220
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState220
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState220
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState220
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState220
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState220
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState220
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState220
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState220
        | Datatype | EOF | Function | Model | Val | Var ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, _startpos__1_), _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 6280 "parser.ml"
            )), _startpos_id_), _, (ote : (Ast.ptyp option))), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : (unit) = 
# 93 "parser.mly"
                                                                       (
            print_endline ("declaring value "^id);
            match ote with
            | None -> Hashtbl.add symbol_tbl id (Val, PExpr_loc (PTVar (new_type_var ()), e))
            | Some pt -> Hashtbl.add symbol_tbl id (Val, PExpr_loc (pt, e))
        )
# 6292 "parser.ml"
             in
            _menhir_goto_declare _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState220)
    | MenhirState258 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState259
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState259
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState259
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState259
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState259
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState259
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState259
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState259 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState259
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState259
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState259 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState259 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState259
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState259
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState259
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState259
        | State ->
            _menhir_run256 _menhir_env (Obj.magic _menhir_stack) MenhirState259
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState259
        | Transition ->
            _menhir_reduce126 _menhir_env (Obj.magic _menhir_stack) MenhirState259
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState259)
    | MenhirState273 | MenhirState271 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | False ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState273 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Float _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState273 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | For ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | Iden _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState273 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | If ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Int _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState273 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | LB1 ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB2 ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB3 ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | Match ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | Negb ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | True ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState273 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIden _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState273 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Val ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Var ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | While ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | And | Comma | Or | Property | RB3 ->
            _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState273)
    | MenhirState309 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState310
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState310
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState310
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState310
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState310
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState310
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState310
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState310 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState310
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState310
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState310 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState310 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState310
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState310
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState310
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState310
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_s = MenhirState310 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__8_ = _endpos in
            let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 6468 "parser.ml"
            )), _startpos_id_), _endpos_f_, _, (f : (Ast.pformula_loc)), _startpos_f_), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__8_ in
            let _v : (Ast.pformula_loc) = 
# 141 "parser.mly"
                                                                   (mk_pformula_loc (PAF (id, f, e)) _startpos__1_ _endpos__8_)
# 6480 "parser.ml"
             in
            _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState310
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState310)
    | MenhirState315 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState316
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState316
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState316
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState316
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState316
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState316
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState316
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState316 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState316
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState316
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState316 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState316 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState316
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState316
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState316
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState316
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_s = MenhirState316 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__12_ = _endpos in
            let (((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_id1_, (id1 : (
# 35 "parser.mly"
       (string)
# 6536 "parser.ml"
            )), _startpos_id1_), _endpos_id2_, (id2 : (
# 35 "parser.mly"
       (string)
# 6540 "parser.ml"
            )), _startpos_id2_), _endpos_f1_, _, (f1 : (Ast.pformula_loc)), _startpos_f1_), _endpos_f2_, _, (f2 : (Ast.pformula_loc)), _startpos_f2_), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
            let _12 = () in
            let _10 = () in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__12_ in
            let _v : (Ast.pformula_loc) = 
# 143 "parser.mly"
                                                                                                         (mk_pformula_loc (PAR (id1, id2, f1, f2, e)) _startpos__1_ _endpos__12_)
# 6554 "parser.ml"
             in
            _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState316
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState316)
    | MenhirState319 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState320
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState320
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState320
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState320
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState320
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState320
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState320
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState320 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState320
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState320
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState320 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState320 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState320
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState320
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState320
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState320
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_s = MenhirState320 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__8_ = _endpos in
            let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 6610 "parser.ml"
            )), _startpos_id_), _endpos_f_, _, (f : (Ast.pformula_loc)), _startpos_f_), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__8_ in
            let _v : (Ast.pformula_loc) = 
# 139 "parser.mly"
                                                                   (mk_pformula_loc (PAX (id, f, e)) _startpos__1_ _endpos__8_)
# 6622 "parser.ml"
             in
            _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState320
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState320)
    | MenhirState323 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState324 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState324 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState324 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_s = MenhirState324 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__8_ = _endpos in
            let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 6678 "parser.ml"
            )), _startpos_id_), _endpos_f_, _, (f : (Ast.pformula_loc)), _startpos_f_), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__8_ in
            let _v : (Ast.pformula_loc) = 
# 142 "parser.mly"
                                                                   (mk_pformula_loc (PEG (id, f, e)) _startpos__1_ _endpos__8_)
# 6690 "parser.ml"
             in
            _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState324)
    | MenhirState329 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState330 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState330 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState330 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_s = MenhirState330 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__12_ = _endpos in
            let (((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_id1_, (id1 : (
# 35 "parser.mly"
       (string)
# 6746 "parser.ml"
            )), _startpos_id1_), _endpos_id2_, (id2 : (
# 35 "parser.mly"
       (string)
# 6750 "parser.ml"
            )), _startpos_id2_), _endpos_f1_, _, (f1 : (Ast.pformula_loc)), _startpos_f1_), _endpos_f2_, _, (f2 : (Ast.pformula_loc)), _startpos_f2_), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
            let _12 = () in
            let _10 = () in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__12_ in
            let _v : (Ast.pformula_loc) = 
# 144 "parser.mly"
                                                                                                         (mk_pformula_loc (PEU (id1, id2, f1, f2, e)) _startpos__1_ _endpos__12_)
# 6764 "parser.ml"
             in
            _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState330)
    | MenhirState333 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState334
        | AddDot ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState334
        | Ando ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState334
        | Equal ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState334
        | GE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState334
        | GT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState334
        | LArrow ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState334
        | LB2 ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState334 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState334
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState334
        | Minus ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState334 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState334 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState334
        | MultDot ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState334
        | Non_Equal ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState334
        | Oro ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState334
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_s = MenhirState334 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__8_ = _endpos in
            let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 6820 "parser.ml"
            )), _startpos_id_), _endpos_f_, _, (f : (Ast.pformula_loc)), _startpos_f_), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__8_ in
            let _v : (Ast.pformula_loc) = 
# 140 "parser.mly"
                                                                   (mk_pformula_loc (PEX (id, f, e)) _startpos__1_ _endpos__8_)
# 6832 "parser.ml"
             in
            _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | With ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState334
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState334)
    | _ ->
        _menhir_fail ()

and _menhir_goto_constr : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * Ast.ptyp option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Vertical ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | UIden _v ->
            _menhir_run237 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState243 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState243)
    | Datatype | EOF | Function | Model | Val | Var ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (c : (string * Ast.ptyp option))) = _menhir_stack in
        let _v : ((string * Ast.ptyp option) list) = 
# 155 "parser.mly"
                    ([c])
# 6868 "parser.ml"
         in
        _menhir_goto_constrs _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_states : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.pexpr_loc) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState259 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _endpos__2_, (_2 : (
# 35 "parser.mly"
       (string)
# 6888 "parser.ml"
        )), _startpos__2_), _endpos__4_, _, (_4 : (Ast.pexpr_loc)), _startpos__4_), _, (_5 : ((string * Ast.pexpr_loc) list))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : ((string * Ast.pexpr_loc) list) = 
# 127 "parser.mly"
                                          ((_2, _4)::_5)
# 6895 "parser.ml"
         in
        _menhir_goto_states _menhir_env _menhir_stack _menhir_s _v
    | MenhirState255 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Transition ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Float _v ->
                _menhir_run177 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState262 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run176 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState262 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run175 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState262 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run170 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState262 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Underline ->
                _menhir_run169 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState262 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState262)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ptyp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState15 | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Arrow ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | Iden _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState15 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB1 ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB3 ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TAray ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | TBool ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | TFloat ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | TInt ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | TLst ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | TUnt ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | Comma | Datatype | EOF | Equal | Function | Model | RB1 | Semicolon | Val | Var | Vertical ->
            _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Arrow ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | Comma | Datatype | EOF | Equal | Function | Iden _ | LB1 | LB3 | Model | RB1 | Semicolon | TAray | TBool | TFloat | TInt | TLst | TUnt | Val | Var | Vertical ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ptyp))), _), _, (_3 : (Ast.ptyp))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ptyp) = 
# 178 "parser.mly"
                    (PTArrow (_1, _3))
# 6984 "parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Arrow ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | Comma ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_s = MenhirState21 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (t : (Ast.ptyp))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ptyp) = 
# 179 "parser.mly"
                        (t)
# 7013 "parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Arrow ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | Comma ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState24
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ptyp))), _), _, (_3 : (Ast.ptyp))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ptyp list) = 
# 182 "parser.mly"
                         ([_1; _3])
# 7036 "parser.ml"
             in
            _menhir_goto_tuple_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Arrow ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState28 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 35 "parser.mly"
       (string)
# 7058 "parser.ml"
            )), _startpos__1_), _, (_3 : (Ast.ptyp))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (string * Ast.ptyp) = 
# 189 "parser.mly"
                                  ((_1, _3))
# 7065 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Iden _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB3 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (string * Ast.ptyp))) = _menhir_stack in
                let _v : ((string * Ast.ptyp) list) = 
# 195 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 7080 "parser.ml"
                 in
                _menhir_goto_nonempty_list_str_typ_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Arrow ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | Comma | Datatype | EOF | Equal | Function | Iden _ | LB1 | LB3 | Model | RB1 | Semicolon | TAray | TBool | TFloat | TInt | TLst | TUnt | Val | Var | Vertical ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ptyp))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ptyp) = 
# 173 "parser.mly"
                (PTAray (_2))
# 7105 "parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Arrow ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | Comma | Datatype | EOF | Equal | Function | Iden _ | LB1 | LB3 | Model | RB1 | Semicolon | TAray | TBool | TFloat | TInt | TLst | TUnt | Val | Var | Vertical ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ptyp))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ptyp) = 
# 174 "parser.mly"
               (PTLst (_2))
# 7126 "parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Arrow ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | Equal ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ptyp))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ptyp) = 
# 111 "parser.mly"
                        (_2)
# 7147 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (x : (Ast.ptyp)) = _v in
            let _v : (Ast.ptyp option) = 
# 102 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 7155 "parser.ml"
             in
            _menhir_goto_option_type_of_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | MenhirState237 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Arrow ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | Datatype | EOF | Function | Model | Val | Var | Vertical ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_uid_, _menhir_s, (uid : (
# 35 "parser.mly"
       (string)
# 7174 "parser.ml"
            )), _startpos_uid_), _, (t : (Ast.ptyp))) = _menhir_stack in
            let _v : (string * Ast.ptyp option) = 
# 166 "parser.mly"
                           ((uid, Some t))
# 7179 "parser.ml"
             in
            _menhir_goto_constr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState238)
    | MenhirState236 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Arrow ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | Datatype | EOF | Function | Model | Val | Var ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.ptyp))) = _menhir_stack in
            let _v : (Ast.ptyp) = 
# 159 "parser.mly"
              (_1)
# 7199 "parser.ml"
             in
            _menhir_goto_type_def _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState240)
    | _ ->
        _menhir_fail ()

and _menhir_run11 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "parser.mly"
       (string)
# 7212 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Colon ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Iden _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState12 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB1 ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB3 ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TAray ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | TBool ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | TFloat ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | TInt ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | TLst ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | TUnt ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce86 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ptyp list) = 
# 185 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 7258 "parser.ml"
     in
    _menhir_goto_list_typ_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_args : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ppattern_loc list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState225 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (Ast.ppattern_loc)), _startpos__1_), _, (_2 : (Ast.ppattern_loc list))) = _menhir_stack in
        let _v : (Ast.ppattern_loc list) = 
# 115 "parser.mly"
                    (_1 :: _2)
# 7273 "parser.ml"
         in
        _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v
    | MenhirState224 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Colon ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState227
        | Equal ->
            _menhir_reduce96 _menhir_env (Obj.magic _menhir_stack) MenhirState227
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState227)
    | _ ->
        _menhir_fail ()

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Equal ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Equal ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run47 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "parser.mly"
       (string)
# 7486 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Dot ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState47 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Iden _v ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState206 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState206)
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState47 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState47 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState47 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState47 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Add | AddDot | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | Equal | Function | GE | GT | LArrow | LE | LT | Model | Mult | MultDot | Non_Equal | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | Vertical | With ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _endpos_uid_, _menhir_s, (uid : (
# 35 "parser.mly"
       (string)
# 7547 "parser.ml"
        )), _startpos_uid_) = _menhir_stack in
        let _startpos = _startpos_uid_ in
        let _endpos = _endpos_uid_ in
        let _v : (Ast.pexpr_loc) = 
# 446 "parser.mly"
                  (mk_pexpr_loc (PConstr ((PConstr_basic uid))) (PTVar (new_type_var ())) _startpos_uid_ _endpos_uid_)
# 7554 "parser.ml"
         in
        _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run48 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.pexpr_loc) = 
# 239 "parser.mly"
            (mk_pexpr_loc (PBool true) (PTBool) _startpos__1_ _endpos__1_)
# 7574 "parser.ml"
     in
    _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RB3 ->
        _menhir_reduce128 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Vertical ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RB2 ->
        _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RB1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_s = MenhirState58 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__2_ = _endpos in
        let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (Ast.pexpr_loc) = 
# 215 "parser.mly"
                (mk_pexpr_loc PUnt (PTUnt) _startpos__1_ _endpos__2_)
# 7879 "parser.ml"
         in
        _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run60 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 33 "parser.mly"
       (int)
# 7900 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_i_ = _endpos in
    let (i : (
# 33 "parser.mly"
       (int)
# 7909 "parser.ml"
    )) = _v in
    let _startpos_i_ = _startpos in
    let _startpos = _startpos_i_ in
    let _endpos = _endpos_i_ in
    let _v : (Ast.pexpr_loc) = 
# 213 "parser.mly"
                (mk_pexpr_loc (PInt i) (PTInt) _startpos_i_ _endpos_i_)
# 7917 "parser.ml"
     in
    _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run62 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "parser.mly"
       (string)
# 7971 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Dot ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | False ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Add | AddDot | And | Ando | Comma | Datatype | Do | Done | DotDot | EOF | Else | Equal | Function | GE | GT | LArrow | LE | LT | Model | Mult | MultDot | Non_Equal | Or | Oro | Property | RB1 | RB2 | RB3 | Semicolon | State | Then | Transition | Vertical | With ->
        _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | In ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LB2 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                let _menhir_stack = (_menhir_stack, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | False ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Float _v ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | For ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Iden _v ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | If ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Int _v ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LB1 ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LB2 ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LB3 ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Match ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Minus ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MinusDot ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Negb ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | True ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UIden _v ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Val ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Var ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | While ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run67 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 34 "parser.mly"
       (float)
# 8111 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_f_ = _endpos in
    let (f : (
# 34 "parser.mly"
       (float)
# 8120 "parser.ml"
    )) = _v in
    let _startpos_f_ = _startpos in
    let _startpos = _startpos_f_ in
    let _endpos = _endpos_f_ in
    let _v : (Ast.pexpr_loc) = 
# 214 "parser.mly"
                (mk_pexpr_loc (PFloat f) (PTFloat) _startpos_f_ _endpos_f_)
# 8128 "parser.ml"
     in
    _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run68 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.pexpr_loc) = 
# 240 "parser.mly"
            (mk_pexpr_loc (PBool false) (PTBool) _startpos__1_ _endpos__1_)
# 8144 "parser.ml"
     in
    _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_goto_pattern_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ppattern_loc list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Vertical ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RB2 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos__5_ = _endpos in
                let (((_menhir_stack, _menhir_s, _startpos__1_), _), _, (pl : (Ast.ppattern_loc list))) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _startpos = _startpos__1_ in
                let _endpos = _endpos__5_ in
                let _v : (Ast.ppattern_loc) = 
# 497 "parser.mly"
                                                   (
            match pl with
            | [] -> mk_ppat_loc (PPat_Aray []) (PTAray (PTVar (new_type_var()))) _startpos__1_ _endpos__5_
            | p::pl' -> mk_ppat_loc (PPat_Aray (pl)) (PTAray p.ptyp) _startpos__1_ _endpos__5_
        )
# 8182 "parser.ml"
                 in
                _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState192 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (Ast.ppattern_loc)), _startpos__1_), _), _, (_3 : (Ast.ppattern_loc list))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ppattern_loc list) = 
# 518 "parser.mly"
                                     (_1 :: _3)
# 8205 "parser.ml"
         in
        _menhir_goto_pattern_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState171 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RB2 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (pl : (Ast.ppattern_loc list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.ppattern_loc) = 
# 502 "parser.mly"
                                  (
            match pl with
            | [] -> mk_ppat_loc (PPat_Lst []) (PTLst (PTVar (new_type_var()))) _startpos__1_ _endpos__3_
            | p::pl' -> mk_ppat_loc (PPat_Lst pl) (PTLst p.ptyp) _startpos__1_ _endpos__3_
        )
# 8231 "parser.ml"
             in
            _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_Comma_pattern_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ppattern_loc list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState180 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__5_ = _endpos in
            let ((((_menhir_stack, _menhir_s, _startpos__1_), _endpos_p_, _, (p : (Ast.ppattern_loc)), _startpos_p_), _), _, (pl : (Ast.ppattern_loc list))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__5_ in
            let _v : (Ast.ppattern_loc) = 
# 509 "parser.mly"
                                                                               (mk_ppat_loc (PPat_Tuple (p::pl)) (PTTuple (List.map (fun pat -> pat.ptyp) (p::pl))) _startpos__1_ _endpos__5_)
# 8267 "parser.ml"
             in
            _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _endpos_x_, _menhir_s, (x : (Ast.ppattern_loc)), _startpos_x_), _), _, (xs : (Ast.ppattern_loc list))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ppattern_loc list) = 
# 217 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 8284 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_Comma_pattern_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run186 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.ppattern_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Float _v ->
        _menhir_run177 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState186 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run176 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState186 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run175 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState186 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run170 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState186 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Underline ->
        _menhir_run169 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState186 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState186

and _menhir_run237 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "parser.mly"
       (string)
# 8318 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState237 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState237 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState237 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TAray ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState237
    | TBool ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState237
    | TFloat ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState237
    | TInt ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState237
    | TLst ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState237
    | TUnt ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState237
    | Datatype | EOF | Function | Model | Val | Var | Vertical ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _endpos_uid_, _menhir_s, (uid : (
# 35 "parser.mly"
       (string)
# 8348 "parser.ml"
        )), _startpos_uid_) = _menhir_stack in
        let _v : (string * Ast.ptyp option) = 
# 165 "parser.mly"
                    (print_endline ("found constr "^uid); (uid, None))
# 8353 "parser.ml"
         in
        _menhir_goto_constr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState237

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce126 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Ast.pexpr_loc) list) = 
# 126 "parser.mly"
        ([])
# 8371 "parser.ml"
     in
    _menhir_goto_states _menhir_env _menhir_stack _menhir_s _v

and _menhir_run256 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Equal ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState258 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState258 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState258 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState258 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState258)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> (
# 41 "parser.mly"
       ((string list) * (Ast.psymbol_tbl) * ((Ast.pkripke_model) option))
# 8450 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 41 "parser.mly"
       ((string list) * (Ast.psymbol_tbl) * ((Ast.pkripke_model) option))
# 8458 "parser.ml"
    )) = _v in
    Obj.magic _1

and _menhir_goto_option_type_of_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ptyp option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Equal ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState217 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Equal ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState219 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState219 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState219 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState219 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState219 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState219 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState219)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState227 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Equal ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState229 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState229 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState229 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState229 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState229 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState229 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState229 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState229 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState229 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState229 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState229 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState229 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState229 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState229 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState229 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState229 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState229 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState229 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState229)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ptyp) = 
# 172 "parser.mly"
            (PTUnt)
# 8645 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState5 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TAray ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | TBool ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | TFloat ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | TInt ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | TLst ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | TUnt ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ptyp) = 
# 169 "parser.mly"
          (PTInt)
# 8686 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ptyp) = 
# 171 "parser.mly"
             (PTFloat)
# 8698 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ptyp) = 
# 170 "parser.mly"
            (PTBool)
# 8710 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TAray ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | TBool ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | TFloat ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | TInt ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | TLst ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | TUnt ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState10 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState13 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TAray ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | TBool ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | TFloat ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | TInt ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | TLst ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | TUnt ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run14 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "parser.mly"
       (string)
# 8788 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TAray ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TBool ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TFloat ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TInt ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TLst ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TUnt ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | Arrow | Comma | Datatype | EOF | Equal | Function | Model | RB1 | Semicolon | Val | Var | Vertical ->
        _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_reduce113 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ppattern_loc list) = 
# 516 "parser.mly"
                ([])
# 8825 "parser.ml"
     in
    _menhir_goto_pattern_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_pattern : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (Ast.ppattern_loc) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ColonColon ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState178 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Float _v ->
                _menhir_run177 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run176 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run175 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run170 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Underline ->
                _menhir_run169 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState180)
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_s = MenhirState178 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : (Ast.ppattern_loc)), _startpos__2_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.ppattern_loc) = 
# 513 "parser.mly"
                        (_2)
# 8880 "parser.ml"
             in
            _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState178)
    | MenhirState184 | MenhirState180 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ColonColon ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState183 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Float _v ->
                _menhir_run177 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState184 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run176 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState184 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run175 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState184 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run170 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState184 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Underline ->
                _menhir_run169 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState184)
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos_x_, _menhir_s, (x : (Ast.ppattern_loc)), _startpos_x_) = _menhir_stack in
            let _v : (Ast.ppattern_loc list) = 
# 215 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 8925 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_Comma_pattern_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183)
    | MenhirState186 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ColonColon ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | Arrow | Colon | Comma | Equal | Float _ | Iden _ | Int _ | LB1 | LB2 | RB1 | RB2 | Semicolon | UIden _ | Underline | Vertical ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_p1_, _menhir_s, (p1 : (Ast.ppattern_loc)), _startpos_p1_), _), _endpos_p2_, _, (p2 : (Ast.ppattern_loc)), _startpos_p2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_p1_ in
            let _endpos = _endpos_p2_ in
            let _v : (Ast.ppattern_loc) = 
# 507 "parser.mly"
                                              (mk_ppat_loc (PPat_Lst_Cons (p1, p2)) (p2.ptyp) _startpos_p1_ _endpos_p2_)
# 8948 "parser.ml"
             in
            _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState187)
    | MenhirState171 | MenhirState192 | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ColonColon ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState191 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Float _v ->
                _menhir_run177 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run176 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run175 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run170 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Underline ->
                _menhir_run169 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB2 | Vertical ->
                _menhir_reduce113 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState192)
        | RB2 | Vertical ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : (Ast.ppattern_loc)), _startpos__1_) = _menhir_stack in
            let _v : (Ast.ppattern_loc list) = 
# 517 "parser.mly"
                ([_1])
# 8995 "parser.ml"
             in
            _menhir_goto_pattern_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191)
    | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ColonColon ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState196
        | Arrow | Colon | Comma | Equal | Float _ | Iden _ | Int _ | LB1 | LB2 | RB1 | RB2 | Semicolon | UIden _ | Underline | Vertical ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_uid_, _menhir_s, (uid : (
# 35 "parser.mly"
       (string)
# 9014 "parser.ml"
            )), _startpos_uid_), _endpos_p_, _, (p : (Ast.ppattern_loc)), _startpos_p_) = _menhir_stack in
            let _startpos = _startpos_uid_ in
            let _endpos = _endpos_p_ in
            let _v : (Ast.ppattern_loc) = 
# 512 "parser.mly"
                               (mk_ppat_loc (PPat_Constr (uid, Some p)) (PTVar (new_type_var())) _startpos_uid_ _endpos_p_)
# 9021 "parser.ml"
             in
            _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState196)
    | MenhirState201 | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Arrow ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState198 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState199 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState199 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState199 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState199 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState199)
        | ColonColon ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState198
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState198)
    | MenhirState225 | MenhirState224 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ColonColon ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState225
        | Float _v ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState225 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Iden _v ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState225 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Int _v ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState225 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB1 ->
            _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB2 ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIden _v ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState225 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Underline ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState225 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Colon | Equal ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : (Ast.ppattern_loc)), _startpos__1_) = _menhir_stack in
            let _v : (Ast.ppattern_loc list) = 
# 114 "parser.mly"
              ([_1])
# 9113 "parser.ml"
             in
            _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState225)
    | MenhirState262 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ColonColon ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState263
        | Equal ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState263 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState264 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState264 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState264 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState264 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState264)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState263)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_Iden_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState233 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _endpos_x_, _menhir_s, (x : (
# 35 "parser.mly"
       (string)
# 9191 "parser.ml"
        )), _startpos_x_), _, (xs : (string list))) = _menhir_stack in
        let _v : (string list) = 
# 187 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 9196 "parser.ml"
         in
        _menhir_goto_list_Iden_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState232 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Equal ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Iden _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState236 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TAray ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState236
            | TBool ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState236
            | TFloat ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState236
            | TInt ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState236
            | TLst ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState236
            | TUnt ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState236
            | UIden _v ->
                _menhir_run237 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState236 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState236)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_declars : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState250 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, (_1 : (unit))), _, (_2 : (unit))) = _menhir_stack in
            let _3 = () in
            let _v : (
# 41 "parser.mly"
       ((string list) * (Ast.psymbol_tbl) * ((Ast.pkripke_model) option))
# 9259 "parser.ml"
            ) = 
# 70 "parser.mly"
                               (!imported, symbol_tbl, None)
# 9263 "parser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _v
        | Model ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LB3 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                let _menhir_stack = (_menhir_stack, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | State ->
                    _menhir_run256 _menhir_env (Obj.magic _menhir_stack) MenhirState255
                | Transition ->
                    _menhir_reduce126 _menhir_env (Obj.magic _menhir_stack) MenhirState255
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState255)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState345 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (unit))), _, (_2 : (unit))) = _menhir_stack in
        let _v : (unit) = 
# 81 "parser.mly"
                      ()
# 9304 "parser.ml"
         in
        _menhir_goto_declars _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce96 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ptyp option) = 
# 100 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( None )
# 9315 "parser.ml"
     in
    _menhir_goto_option_type_of_expr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState3 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TAray ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | TBool ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | TFloat ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | TInt ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | TLst ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | TUnt ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run169 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.ppattern_loc) = 
# 508 "parser.mly"
                    (mk_ppat_loc PPat_Underline (PTVar (new_type_var())) _startpos__1_ _endpos__1_)
# 9360 "parser.ml"
     in
    _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run170 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "parser.mly"
       (string)
# 9367 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Float _v ->
        _menhir_run177 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run176 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run175 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run170 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Underline ->
        _menhir_run169 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Arrow | Colon | ColonColon | Comma | Equal | RB1 | RB2 | Semicolon | Vertical ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _endpos_uid_, _menhir_s, (uid : (
# 35 "parser.mly"
       (string)
# 9393 "parser.ml"
        )), _startpos_uid_) = _menhir_stack in
        let _startpos = _startpos_uid_ in
        let _endpos = _endpos_uid_ in
        let _v : (Ast.ppattern_loc) = 
# 511 "parser.mly"
                  (mk_ppat_loc (PPat_Constr (uid, None)) (PTVar (new_type_var())) _startpos_uid_ _endpos_uid_)
# 9400 "parser.ml"
         in
        _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170

and _menhir_run171 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Float _v ->
        _menhir_run177 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState171 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run176 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState171 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run175 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState171 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run170 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState171 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Underline ->
        _menhir_run169 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Vertical ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState171 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Float _v ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Iden _v ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Int _v ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB1 ->
            _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB2 ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIden _v ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Underline ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Vertical ->
            _menhir_reduce113 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172)
    | RB2 ->
        _menhir_reduce113 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState171

and _menhir_run173 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Float _v ->
        _menhir_run177 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState173 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run176 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState173 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run175 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState173 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RB1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_s = MenhirState173 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__2_ = _endpos in
        let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (Ast.ppattern_loc) = 
# 496 "parser.mly"
                (mk_ppat_loc (PPat_Unt) PTUnt _startpos__1_ _endpos__2_)
# 9493 "parser.ml"
         in
        _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | UIden _v ->
        _menhir_run170 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState173 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Underline ->
        _menhir_run169 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState173 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState173

and _menhir_run175 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 33 "parser.mly"
       (int)
# 9508 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_i_ = _endpos in
    let (i : (
# 33 "parser.mly"
       (int)
# 9517 "parser.ml"
    )) = _v in
    let _startpos_i_ = _startpos in
    let _startpos = _startpos_i_ in
    let _endpos = _endpos_i_ in
    let _v : (Ast.ppattern_loc) = 
# 494 "parser.mly"
                (mk_ppat_loc (PPat_Int i) PTInt _startpos_i_ _endpos_i_)
# 9525 "parser.ml"
     in
    _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run176 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "parser.mly"
       (string)
# 9532 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_id_ = _endpos in
    let (id : (
# 35 "parser.mly"
       (string)
# 9541 "parser.ml"
    )) = _v in
    let _startpos_id_ = _startpos in
    let _startpos = _startpos_id_ in
    let _endpos = _endpos_id_ in
    let _v : (Ast.ppattern_loc) = 
# 493 "parser.mly"
                     (mk_ppat_loc (PPat_Symbol id) (PTVar (new_type_var())) _startpos_id_ _endpos_id_)
# 9549 "parser.ml"
     in
    _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run177 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 34 "parser.mly"
       (float)
# 9556 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_f_ = _endpos in
    let (f : (
# 34 "parser.mly"
       (float)
# 9565 "parser.ml"
    )) = _v in
    let _startpos_f_ = _startpos in
    let _startpos = _startpos_f_ in
    let _endpos = _endpos_f_ in
    let _v : (Ast.ppattern_loc) = 
# 495 "parser.mly"
                (mk_ppat_loc (PPat_Float f) PTFloat _startpos_f_ _endpos_f_)
# 9573 "parser.ml"
     in
    _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_reduce80 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (string list) = 
# 185 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 9582 "parser.ml"
     in
    _menhir_goto_list_Iden_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run233 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "parser.mly"
       (string)
# 9589 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run233 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState233 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Equal ->
        _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack) MenhirState233
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState233

and _menhir_reduce14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit) = 
# 80 "parser.mly"
         ()
# 9610 "parser.ml"
     in
    _menhir_goto_declars _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Colon ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | Equal ->
            _menhir_reduce96 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run216 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Colon ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState217
        | Equal ->
            _menhir_reduce96 _menhir_env (Obj.magic _menhir_stack) MenhirState217
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState217)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState345 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState338 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState334 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState333 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState330 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState329 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState327 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState324 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState323 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState320 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState319 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState316 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState315 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState313 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState310 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState309 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState307 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState305 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState303 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState299 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s, _), _), _, _, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState293 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState288 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState284 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s, _), _), _, _, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState278 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState273 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState271 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState270 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState268 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState265 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState264 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState263 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState262 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState259 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState258 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState255 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState250 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState243 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState240 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState238 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState237 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState236 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState233 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState232 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState229 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState227 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState225 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState224 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState220 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState219 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState217 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState215 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState212 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState211 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState210 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState209 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState208 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState206 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState204 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState203 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState201 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState199 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState198 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState196 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState192 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState191 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState187 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState186 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState180 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState178 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState171 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState164 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_goto_debug : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 42 "parser.mly"
        (unit)
# 10369 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 42 "parser.mly"
        (unit)
# 10377 "parser.ml"
    )) = _v in
    Obj.magic _1

and _menhir_run223 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Float _v ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState224 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Iden _v ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState224 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Int _v ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState224 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB1 ->
            _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB2 ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIden _v ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState224 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Underline ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState224 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState224)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run231 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Iden _v ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState232 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Equal ->
            _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState232)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_imported : _menhir_env -> 'ttv_tail -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Datatype ->
        _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState250
    | Function ->
        _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState250
    | Import ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState250 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | UIden _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let (_3 : (
# 35 "parser.mly"
       (string)
# 10477 "parser.ml"
            )) = _v in
            let _startpos__3_ = _startpos in
            let ((_menhir_stack, (_1 : (unit))), _) = _menhir_stack in
            let _2 = () in
            let _v : (unit) = 
# 75 "parser.mly"
                            (print_endline ("imported "^_3); imported := _3 :: !imported)
# 10485 "parser.ml"
             in
            _menhir_goto_imported _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | Val ->
        _menhir_run216 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EOF | Model ->
        _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack) MenhirState250
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState250

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and _menhir_init : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> _menhir_env =
  fun lexer lexbuf ->
    let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and debug : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 42 "parser.mly"
        (unit)
# 10530 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Datatype ->
        _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | Function ->
        _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | Import ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState0 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | UIden _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__2_ = _endpos in
            let (_2 : (
# 35 "parser.mly"
       (string)
# 10558 "parser.ml"
            )) = _v in
            let _startpos__2_ = _startpos in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _1 = () in
            let _v : (
# 42 "parser.mly"
        (unit)
# 10566 "parser.ml"
            ) = 
# 66 "parser.mly"
                   (print_endline ("imported "^_2))
# 10570 "parser.ml"
             in
            _menhir_goto_debug _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | Val ->
        _menhir_run216 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 41 "parser.mly"
       ((string list) * (Ast.psymbol_tbl) * ((Ast.pkripke_model) option))
# 10591 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) = 
# 74 "parser.mly"
            ()
# 10601 "parser.ml"
     in
    _menhir_goto_imported _menhir_env _menhir_stack _v)

# 538 "parser.mly"
  
# 10607 "parser.ml"

# 219 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
  


# 10613 "parser.ml"
