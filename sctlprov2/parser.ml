
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
    | Semicolon
    | RB3
    | RB2
    | RB1
    | Property
    | Oro
    | Or
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
# 54 "parser.ml"
  )
    | Init
    | In
    | Import
    | If
    | Iden of (
# 35 "parser.mly"
       (string)
# 63 "parser.ml"
  )
    | GT
    | GE
    | Function
    | For
    | Float of (
# 34 "parser.mly"
       (float)
# 72 "parser.ml"
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
  | MenhirState328
  | MenhirState321
  | MenhirState317
  | MenhirState315
  | MenhirState311
  | MenhirState307
  | MenhirState303
  | MenhirState301
  | MenhirState297
  | MenhirState295
  | MenhirState293
  | MenhirState291
  | MenhirState287
  | MenhirState281
  | MenhirState276
  | MenhirState272
  | MenhirState266
  | MenhirState261
  | MenhirState259
  | MenhirState258
  | MenhirState256
  | MenhirState253
  | MenhirState251
  | MenhirState250
  | MenhirState249
  | MenhirState246
  | MenhirState241
  | MenhirState240
  | MenhirState237
  | MenhirState236
  | MenhirState233
  | MenhirState231
  | MenhirState229
  | MenhirState228
  | MenhirState224
  | MenhirState223
  | MenhirState221
  | MenhirState219
  | MenhirState216
  | MenhirState215
  | MenhirState214
  | MenhirState213
  | MenhirState212
  | MenhirState211
  | MenhirState210
  | MenhirState209
  | MenhirState208
  | MenhirState207
  | MenhirState205
  | MenhirState203
  | MenhirState202
  | MenhirState200
  | MenhirState196
  | MenhirState195
  | MenhirState191
  | MenhirState190
  | MenhirState188
  | MenhirState187
  | MenhirState184
  | MenhirState182
  | MenhirState177
  | MenhirState176
  | MenhirState175
  | MenhirState174
  | MenhirState172
  | MenhirState168
  | MenhirState159
  | MenhirState155
  | MenhirState152
  | MenhirState148
  | MenhirState146
  | MenhirState145
  | MenhirState143
  | MenhirState141
  | MenhirState140
  | MenhirState137
  | MenhirState136
  | MenhirState135
  | MenhirState134
  | MenhirState133
  | MenhirState132
  | MenhirState130
  | MenhirState129
  | MenhirState126
  | MenhirState124
  | MenhirState123
  | MenhirState120
  | MenhirState119
  | MenhirState118
  | MenhirState117
  | MenhirState116
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
  | MenhirState95
  | MenhirState94
  | MenhirState93
  | MenhirState92
  | MenhirState91
  | MenhirState90
  | MenhirState89
  | MenhirState88
  | MenhirState86
  | MenhirState85
  | MenhirState83
  | MenhirState82
  | MenhirState81
  | MenhirState77
  | MenhirState75
  | MenhirState72
  | MenhirState68
  | MenhirState67
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState61
  | MenhirState59
  | MenhirState58
  | MenhirState57
  | MenhirState56
  | MenhirState55
  | MenhirState53
  | MenhirState52
  | MenhirState49
  | MenhirState46
  | MenhirState45
  | MenhirState42
  | MenhirState41
  | MenhirState37
  | MenhirState35
  | MenhirState31
  | MenhirState30
  | MenhirState28
  | MenhirState24
  | MenhirState20
  | MenhirState19
  | MenhirState16
  | MenhirState15
  | MenhirState14
  | MenhirState13
  | MenhirState11
  | MenhirState6
  | MenhirState5
  | MenhirState4

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

# 308 "parser.ml"

let rec _menhir_goto_separated_nonempty_list_Semicolon_property_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.pformula_loc) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState253 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((string * Ast.pformula_loc) list)) = _v in
        let _v : ((string * Ast.pformula_loc) list) = 
# 130 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( x )
# 320 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_Semicolon_property__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState328 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((string * Ast.pformula_loc) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string * Ast.pformula_loc))) = _menhir_stack in
        let _2 = () in
        let _v : ((string * Ast.pformula_loc) list) = 
# 217 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 332 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_Semicolon_property_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run293 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pformula_loc) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AF ->
        _menhir_run288 _menhir_env (Obj.magic _menhir_stack) MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AR ->
        _menhir_run282 _menhir_env (Obj.magic _menhir_stack) MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AX ->
        _menhir_run278 _menhir_env (Obj.magic _menhir_stack) MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Bottom ->
        _menhir_run277 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EG ->
        _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EU ->
        _menhir_run267 _menhir_env (Obj.magic _menhir_stack) MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EX ->
        _menhir_run263 _menhir_env (Obj.magic _menhir_stack) MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run259 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState293 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Neg ->
        _menhir_run258 _menhir_env (Obj.magic _menhir_stack) MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Top ->
        _menhir_run257 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState293

and _menhir_run295 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pformula_loc) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AF ->
        _menhir_run288 _menhir_env (Obj.magic _menhir_stack) MenhirState295 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AR ->
        _menhir_run282 _menhir_env (Obj.magic _menhir_stack) MenhirState295 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AX ->
        _menhir_run278 _menhir_env (Obj.magic _menhir_stack) MenhirState295 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Bottom ->
        _menhir_run277 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState295 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EG ->
        _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState295 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EU ->
        _menhir_run267 _menhir_env (Obj.magic _menhir_stack) MenhirState295 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EX ->
        _menhir_run263 _menhir_env (Obj.magic _menhir_stack) MenhirState295 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run259 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState295 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Neg ->
        _menhir_run258 _menhir_env (Obj.magic _menhir_stack) MenhirState295 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Top ->
        _menhir_run257 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState295 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState295

and _menhir_goto_list_expr_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (Ast.pexpr_loc list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    match _menhir_s with
    | MenhirState259 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_el_ = _endpos in
        let (el : (Ast.pexpr_loc list)) = _v in
        let (_menhir_stack, _endpos_id_, _menhir_s, (id : (
# 35 "parser.mly"
       (string)
# 409 "parser.ml"
        )), _startpos_id_) = _menhir_stack in
        let _startpos = _startpos_id_ in
        let _endpos = _endpos_el_ in
        let _v : (Ast.pformula_loc) = 
# 111 "parser.mly"
                                (mk_pformula_loc (PAtomic (id, el)) _startpos_id_ _endpos_el_)
# 416 "parser.ml"
         in
        _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | MenhirState261 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_xs_ = _endpos in
        let (xs : (Ast.pexpr_loc list)) = _v in
        let (_menhir_stack, _endpos_x_, _menhir_s, (x : (Ast.pexpr_loc))) = _menhir_stack in
        let _endpos = _endpos_xs_ in
        let _v : (Ast.pexpr_loc list) = 
# 187 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 429 "parser.ml"
         in
        _menhir_goto_list_expr_ _menhir_env _menhir_stack _endpos _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run257 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.pformula_loc) = 
# 109 "parser.mly"
             (mk_pformula_loc PTop _startpos__1_ _endpos__1_)
# 447 "parser.ml"
     in
    _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run258 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AF ->
        _menhir_run288 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AR ->
        _menhir_run282 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AX ->
        _menhir_run278 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Bottom ->
        _menhir_run277 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EG ->
        _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EU ->
        _menhir_run267 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EX ->
        _menhir_run263 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run259 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState258 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Neg ->
        _menhir_run258 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Top ->
        _menhir_run257 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState258 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState258

and _menhir_run259 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "parser.mly"
       (string)
# 485 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState259 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState259 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState259 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState259 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState259 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState259 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState259 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState259 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState259 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState259 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState259 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState259 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState259 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState259 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState259 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState259 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState259 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState259 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | And | Comma | Or | RB3 | Semicolon ->
        _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack) MenhirState259
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState259

and _menhir_run263 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
                    _menhir_run288 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AR ->
                    _menhir_run282 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AX ->
                    _menhir_run278 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Bottom ->
                    _menhir_run277 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState266 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EG ->
                    _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EU ->
                    _menhir_run267 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EX ->
                    _menhir_run263 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Iden _v ->
                    _menhir_run259 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState266 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Neg ->
                    _menhir_run258 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Top ->
                    _menhir_run257 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState266 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState266)
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

and _menhir_run267 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
                            _menhir_run288 _menhir_env (Obj.magic _menhir_stack) MenhirState272 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | AR ->
                            _menhir_run282 _menhir_env (Obj.magic _menhir_stack) MenhirState272 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | AX ->
                            _menhir_run278 _menhir_env (Obj.magic _menhir_stack) MenhirState272 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | Bottom ->
                            _menhir_run277 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState272 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | EG ->
                            _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState272 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | EU ->
                            _menhir_run267 _menhir_env (Obj.magic _menhir_stack) MenhirState272 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | EX ->
                            _menhir_run263 _menhir_env (Obj.magic _menhir_stack) MenhirState272 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | Iden _v ->
                            _menhir_run259 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState272 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | Neg ->
                            _menhir_run258 _menhir_env (Obj.magic _menhir_stack) MenhirState272 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | Top ->
                            _menhir_run257 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState272 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState272)
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

and _menhir_run273 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
                    _menhir_run288 _menhir_env (Obj.magic _menhir_stack) MenhirState276 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AR ->
                    _menhir_run282 _menhir_env (Obj.magic _menhir_stack) MenhirState276 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AX ->
                    _menhir_run278 _menhir_env (Obj.magic _menhir_stack) MenhirState276 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Bottom ->
                    _menhir_run277 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState276 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EG ->
                    _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState276 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EU ->
                    _menhir_run267 _menhir_env (Obj.magic _menhir_stack) MenhirState276 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EX ->
                    _menhir_run263 _menhir_env (Obj.magic _menhir_stack) MenhirState276 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Iden _v ->
                    _menhir_run259 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState276 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Neg ->
                    _menhir_run258 _menhir_env (Obj.magic _menhir_stack) MenhirState276 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Top ->
                    _menhir_run257 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState276 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState276)
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

and _menhir_run277 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.pformula_loc) = 
# 110 "parser.mly"
             (mk_pformula_loc PBottom _startpos__1_ _endpos__1_)
# 779 "parser.ml"
     in
    _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run278 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
                    _menhir_run288 _menhir_env (Obj.magic _menhir_stack) MenhirState281 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AR ->
                    _menhir_run282 _menhir_env (Obj.magic _menhir_stack) MenhirState281 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AX ->
                    _menhir_run278 _menhir_env (Obj.magic _menhir_stack) MenhirState281 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Bottom ->
                    _menhir_run277 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState281 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EG ->
                    _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState281 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EU ->
                    _menhir_run267 _menhir_env (Obj.magic _menhir_stack) MenhirState281 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EX ->
                    _menhir_run263 _menhir_env (Obj.magic _menhir_stack) MenhirState281 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Iden _v ->
                    _menhir_run259 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState281 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Neg ->
                    _menhir_run258 _menhir_env (Obj.magic _menhir_stack) MenhirState281 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Top ->
                    _menhir_run257 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState281 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState281)
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

and _menhir_run282 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
                            _menhir_run288 _menhir_env (Obj.magic _menhir_stack) MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | AR ->
                            _menhir_run282 _menhir_env (Obj.magic _menhir_stack) MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | AX ->
                            _menhir_run278 _menhir_env (Obj.magic _menhir_stack) MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | Bottom ->
                            _menhir_run277 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | EG ->
                            _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | EU ->
                            _menhir_run267 _menhir_env (Obj.magic _menhir_stack) MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | EX ->
                            _menhir_run263 _menhir_env (Obj.magic _menhir_stack) MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | Iden _v ->
                            _menhir_run259 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState287 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | Neg ->
                            _menhir_run258 _menhir_env (Obj.magic _menhir_stack) MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | Top ->
                            _menhir_run257 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState287)
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

and _menhir_run288 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
                    _menhir_run288 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AR ->
                    _menhir_run282 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AX ->
                    _menhir_run278 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Bottom ->
                    _menhir_run277 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState291 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EG ->
                    _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EU ->
                    _menhir_run267 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | EX ->
                    _menhir_run263 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Iden _v ->
                    _menhir_run259 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState291 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Neg ->
                    _menhir_run258 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Top ->
                    _menhir_run257 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState291 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState291)
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

and _menhir_goto_formula : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (Ast.pformula_loc) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState291 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | And ->
            _menhir_run295 _menhir_env (Obj.magic _menhir_stack)
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState297 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState297 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState297 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState297 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState297 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState297 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState297)
        | Or ->
            _menhir_run293 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run295 _menhir_env (Obj.magic _menhir_stack)
        | Comma | Or | RB3 | Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (Ast.pformula_loc)), _startpos__1_), _endpos__3_, _, (_3 : (Ast.pformula_loc)), _startpos__3_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.pformula_loc) = 
# 114 "parser.mly"
                            (mk_pformula_loc (POr (_1, _3)) _startpos__1_ _endpos__3_)
# 1095 "parser.ml"
             in
            _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState295 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (Ast.pformula_loc)), _startpos__1_), _endpos__3_, _, (_3 : (Ast.pformula_loc)), _startpos__3_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (Ast.pformula_loc) = 
# 113 "parser.mly"
                            (mk_pformula_loc (PAnd (_1, _3)) _startpos__1_ _endpos__3_)
# 1114 "parser.ml"
         in
        _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | MenhirState287 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | And ->
            _menhir_run295 _menhir_env (Obj.magic _menhir_stack)
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AF ->
                _menhir_run288 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | AR ->
                _menhir_run282 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | AX ->
                _menhir_run278 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Bottom ->
                _menhir_run277 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState301 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EG ->
                _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EU ->
                _menhir_run267 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EX ->
                _menhir_run263 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run259 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState301 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Neg ->
                _menhir_run258 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Top ->
                _menhir_run257 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState301 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState301)
        | Or ->
            _menhir_run293 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState301 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | And ->
            _menhir_run295 _menhir_env (Obj.magic _menhir_stack)
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState303 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState303 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState303 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState303 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState303)
        | Or ->
            _menhir_run293 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState281 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | And ->
            _menhir_run295 _menhir_env (Obj.magic _menhir_stack)
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState307 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState307 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState307 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState307 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState307)
        | Or ->
            _menhir_run293 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState276 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | And ->
            _menhir_run295 _menhir_env (Obj.magic _menhir_stack)
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState311 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState311 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState311 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState311 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState311 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState311 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState311)
        | Or ->
            _menhir_run293 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState272 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | And ->
            _menhir_run295 _menhir_env (Obj.magic _menhir_stack)
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AF ->
                _menhir_run288 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | AR ->
                _menhir_run282 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | AX ->
                _menhir_run278 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Bottom ->
                _menhir_run277 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EG ->
                _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EU ->
                _menhir_run267 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EX ->
                _menhir_run263 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run259 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState315 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Neg ->
                _menhir_run258 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Top ->
                _menhir_run257 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState315 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState315)
        | Or ->
            _menhir_run293 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState315 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | And ->
            _menhir_run295 _menhir_env (Obj.magic _menhir_stack)
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState317 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState317 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState317 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState317 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState317 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState317 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState317)
        | Or ->
            _menhir_run293 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState266 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | And ->
            _menhir_run295 _menhir_env (Obj.magic _menhir_stack)
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState321 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState321 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState321 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState321 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState321 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState321 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState321)
        | Or ->
            _menhir_run293 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState258 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : (Ast.pformula_loc)), _startpos__2_) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (Ast.pformula_loc) = 
# 112 "parser.mly"
                    (mk_pformula_loc (PNeg _2) _startpos__1_ _endpos__2_)
# 1515 "parser.ml"
         in
        _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | MenhirState256 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | And ->
            _menhir_run295 _menhir_env (Obj.magic _menhir_stack)
        | Or ->
            _menhir_run293 _menhir_env (Obj.magic _menhir_stack)
        | RB3 | Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 1532 "parser.ml"
            )), _startpos_id_), _endpos_fml_, _, (fml : (Ast.pformula_loc)), _startpos_fml_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (string * Ast.pformula_loc) = 
# 106 "parser.mly"
                                                  ((id, fml))
# 1539 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Semicolon ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Property ->
                    _menhir_run254 _menhir_env (Obj.magic _menhir_stack) MenhirState328
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState328)
            | RB3 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (string * Ast.pformula_loc))) = _menhir_stack in
                let _v : ((string * Ast.pformula_loc) list) = 
# 215 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 1563 "parser.ml"
                 in
                _menhir_goto_separated_nonempty_list_Semicolon_property_ _menhir_env _menhir_stack _menhir_s _v
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
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce76 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : (Ast.pexpr_loc list) = 
# 185 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 1587 "parser.ml"
     in
    _menhir_goto_list_expr_ _menhir_env _menhir_stack _endpos _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_Semicolon_property__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.pformula_loc) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RB3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__13_ = _endpos in
        let ((((((_menhir_stack, _startpos__2_), _endpos_e1_, _, (e1 : (Ast.pexpr_loc))), _endpos_p_, _, (p : (Ast.ppattern_loc)), _startpos_p_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc))), _, (xs0 : ((string * Ast.pformula_loc) list))) = _menhir_stack in
        let _13 = () in
        let _11 = () in
        let _9 = () in
        let _7 = () in
        let _6 = () in
        let _4 = () in
        let _3 = () in
        let _2 = () in
        let _1 = () in
        let _v : (unit) = let pl =
          let xs = xs0 in
          
# 206 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 1619 "parser.ml"
          
        in
        
# 97 "parser.mly"
                                                                                                                                                  (
        kripke_model := Some {
            init = e1;
            transition = (p, e2);
            properties = pl;
        }
    )
# 1631 "parser.ml"
         in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, (_1 : (unit))), (_2 : (unit))), (_3 : (unit))) = _menhir_stack in
            let _4 = () in
            let _v : (
# 41 "parser.mly"
       ((string list) * (Ast.psymbol_tbl) * ((Ast.pkripke_model) option))
# 1646 "parser.ml"
            ) = 
# 64 "parser.mly"
                                   (!imported, symbol_tbl, !kripke_model)
# 1650 "parser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _v
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run254 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run288 _menhir_env (Obj.magic _menhir_stack) MenhirState256 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | AR ->
                _menhir_run282 _menhir_env (Obj.magic _menhir_stack) MenhirState256 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | AX ->
                _menhir_run278 _menhir_env (Obj.magic _menhir_stack) MenhirState256 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Bottom ->
                _menhir_run277 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState256 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EG ->
                _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState256 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EU ->
                _menhir_run267 _menhir_env (Obj.magic _menhir_stack) MenhirState256 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EX ->
                _menhir_run263 _menhir_env (Obj.magic _menhir_stack) MenhirState256 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run259 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState256 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Neg ->
                _menhir_run258 _menhir_env (Obj.magic _menhir_stack) MenhirState256 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Top ->
                _menhir_run257 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState256 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState256)
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
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Iden _v ->
            _menhir_run180 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Int _v ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB1 ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB2 ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIden _v ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Underline ->
            _menhir_run173 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172)
    | Add | AddDot | And | Ando | Comma | Do | Done | DotDot | EOF | Else | Equal | False | Float _ | For | GE | GT | Iden _ | If | Int _ | LArrow | LB1 | LB2 | LB3 | LE | LT | Match | Minus | MinusDot | Model | Mult | MultDot | Negb | Non_Equal | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | While | With ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e1_, _, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_pel_, (pel : ((Ast.ppattern_loc * Ast.pexpr_loc) list))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_pel_ in
        let _v : (Ast.pexpr_loc) = 
# 406 "parser.mly"
                                                          (mk_pexpr_loc (PMatch (e1, pel)) (PTVar (new_type_var ())) _startpos__1_ _endpos_pel_)
# 1761 "parser.ml"
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
    | MenhirState155 ->
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
# 203 "parser.mly"
                                                                       (
            let elt = List.map (fun (e:pexpr_loc) -> e.ptyp) (e::el) in
            mk_pexpr_loc (PTuple el) ((PTTuple elt)) _startpos__1_ _endpos__5_
        )
# 1798 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState159 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _endpos_x_, _menhir_s, (x : (Ast.pexpr_loc))), _, (xs : (Ast.pexpr_loc list))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.pexpr_loc list) = 
# 217 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 1815 "parser.ml"
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
# 340 "parser.mly"
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
# 1858 "parser.ml"
     in
    _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * ((string * Ast.pexpr_loc) list) -> Lexing.position -> (
# 35 "parser.mly"
       (string)
# 1865 "parser.ml"
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Float _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | For ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Iden _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | If ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Int _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB1 ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB2 ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB3 ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Match ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Negb ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | True ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIden _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Val ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Var ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | While ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (Ast.pexpr_loc) -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState81 ->
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
# 1940 "parser.ml"
            )), _startpos__2_), _endpos__4_, _, (_4 : (Ast.pexpr_loc))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _v : ((string * Ast.pexpr_loc) list) = 
# 438 "parser.mly"
                                                ((_2, _4) :: _1)
# 1947 "parser.ml"
             in
            _menhir_goto_str_expr_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState126 ->
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
# 1970 "parser.ml"
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
# 377 "parser.mly"
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
# 2009 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState146 ->
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
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState148 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState148 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState148 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState148 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148)
        | Add | AddDot | And | Ando | Comma | Do | Done | DotDot | EOF | Equal | False | Float _ | For | GE | GT | Iden _ | If | Int _ | LArrow | LB1 | LB2 | LB3 | LE | LT | Match | Minus | MinusDot | Model | Mult | MultDot | Negb | Non_Equal | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While | With ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_, _endpos) = Obj.magic _menhir_stack in
            let _v : (Ast.pexpr_loc option) = 
# 100 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( None )
# 2074 "parser.ml"
             in
            _menhir_goto_option_else_expr_ _menhir_env _menhir_stack _endpos _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _endpos__2_, _, (_2 : (Ast.pexpr_loc))) = _menhir_stack in
        let _1 = () in
        let _endpos = _endpos__2_ in
        let _v : (Ast.pexpr_loc) = 
# 446 "parser.mly"
                        (_2)
# 2092 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_x_ = _endpos in
        let (x : (Ast.pexpr_loc)) = _v in
        let _endpos = _endpos_x_ in
        let _v : (Ast.pexpr_loc option) = 
# 102 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 2102 "parser.ml"
         in
        _menhir_goto_option_else_expr_ _menhir_env _menhir_stack _endpos _v
    | MenhirState64 ->
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
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState159 | MenhirState155 ->
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
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState159)
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos_x_, _menhir_s, (x : (Ast.pexpr_loc))) = _menhir_stack in
            let _v : (Ast.pexpr_loc list) = 
# 215 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 2217 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_Comma_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState61 ->
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
# 2238 "parser.ml"
            )), _startpos__1_), _endpos__3_, _, (_3 : (Ast.pexpr_loc))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : ((string * Ast.pexpr_loc) list) = 
# 437 "parser.mly"
                                ([(_1, _3)])
# 2245 "parser.ml"
             in
            _menhir_goto_str_expr_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState203 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (Ast.ppattern_loc)), _startpos__1_), _), _endpos__3_, _, (_3 : (Ast.pexpr_loc))) = _menhir_stack in
        let _2 = () in
        let _endpos = _endpos__3_ in
        let _v : (Ast.ppattern_loc * Ast.pexpr_loc) = 
# 452 "parser.mly"
                                    ((_1, _3))
# 2263 "parser.ml"
         in
        (match _menhir_s with
        | MenhirState172 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let (_3 : (Ast.ppattern_loc * Ast.pexpr_loc)) = _v in
            let (_menhir_stack, _endpos__1_, (_1 : ((Ast.ppattern_loc * Ast.pexpr_loc) list))) = _menhir_stack in
            let _2 = () in
            let _endpos = _endpos__3_ in
            let _v : ((Ast.ppattern_loc * Ast.pexpr_loc) list) = 
# 450 "parser.mly"
                                                (_1 @ [_3])
# 2277 "parser.ml"
             in
            _menhir_goto_pattern_expr_list _menhir_env _menhir_stack _endpos _v
        | MenhirState205 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos_pe_ = _endpos in
            let (pe : (Ast.ppattern_loc * Ast.pexpr_loc)) = _v in
            let (_menhir_stack, (_1 : (unit option))) = _menhir_stack in
            let _endpos = _endpos_pe_ in
            let _v : ((Ast.ppattern_loc * Ast.pexpr_loc) list) = 
# 449 "parser.mly"
                                                      ([pe])
# 2290 "parser.ml"
             in
            _menhir_goto_pattern_expr_list _menhir_env _menhir_stack _endpos _v
        | _ ->
            _menhir_fail ())
    | MenhirState216 ->
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
# 364 "parser.mly"
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
# 2327 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState233 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 2342 "parser.ml"
        )), _startpos_id_), _, (ags : (Ast.ppattern_loc list))), _, (otf : (Ast.ptyp option))), _endpos_e_, _, (e : (Ast.pexpr_loc))) = _menhir_stack in
        let _5 = () in
        let _1 = () in
        let _v : (unit) = 
# 84 "parser.mly"
                                                                              (
        match otf with
        | None -> Hashtbl.add symbol_tbl id (Function, PFunction(PTVar (new_type_var ()), ags, e))
        | Some pt -> Hashtbl.add symbol_tbl id (Function, PFunction(pt, ags, e)))
# 2352 "parser.ml"
         in
        _menhir_goto_declars _menhir_env _menhir_stack _v
    | MenhirState246 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Transition ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Float _v ->
                    _menhir_run181 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState249 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Iden _v ->
                    _menhir_run180 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState249 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Int _v ->
                    _menhir_run179 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState249 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LB1 ->
                    _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState249 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LB2 ->
                    _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState249 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UIden _v ->
                    _menhir_run174 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState249 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Underline ->
                    _menhir_run173 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState249 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState249)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState251 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Property ->
                _menhir_run254 _menhir_env (Obj.magic _menhir_stack) MenhirState253
            | RB3 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState253 in
                let _v : ((string * Ast.pformula_loc) list) = 
# 128 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 2418 "parser.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_Semicolon_property__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState253)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState261 | MenhirState259 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | False ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState261 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Float _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState261 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | For ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Iden _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState261 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | If ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Int _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState261 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB1 ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB2 ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB3 ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Match ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Negb ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | True ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState261 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIden _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState261 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Val ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Var ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | While ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | And | Comma | Or | RB3 | Semicolon ->
            _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack) MenhirState261
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState261)
    | MenhirState297 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__8_ = _endpos in
            let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 2492 "parser.ml"
            )), _startpos_id_), _endpos_f_, _, (f : (Ast.pformula_loc)), _startpos_f_), _endpos_e_, _, (e : (Ast.pexpr_loc))) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__8_ in
            let _v : (Ast.pformula_loc) = 
# 117 "parser.mly"
                                                            (mk_pformula_loc (PAF (id, f, e)) _startpos__1_ _endpos__8_)
# 2504 "parser.ml"
             in
            _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState303 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__12_ = _endpos in
            let (((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_id1_, (id1 : (
# 35 "parser.mly"
       (string)
# 2527 "parser.ml"
            )), _startpos_id1_), _endpos_id2_, (id2 : (
# 35 "parser.mly"
       (string)
# 2531 "parser.ml"
            )), _startpos_id2_), _endpos_f1_, _, (f1 : (Ast.pformula_loc)), _startpos_f1_), _endpos_f2_, _, (f2 : (Ast.pformula_loc)), _startpos_f2_), _endpos_e_, _, (e : (Ast.pexpr_loc))) = _menhir_stack in
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
# 119 "parser.mly"
                                                                                                  (mk_pformula_loc (PAR (id1, id2, f1, f2, e)) _startpos__1_ _endpos__12_)
# 2545 "parser.ml"
             in
            _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState307 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__8_ = _endpos in
            let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 2568 "parser.ml"
            )), _startpos_id_), _endpos_f_, _, (f : (Ast.pformula_loc)), _startpos_f_), _endpos_e_, _, (e : (Ast.pexpr_loc))) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__8_ in
            let _v : (Ast.pformula_loc) = 
# 115 "parser.mly"
                                                            (mk_pformula_loc (PAX (id, f, e)) _startpos__1_ _endpos__8_)
# 2580 "parser.ml"
             in
            _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState311 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__8_ = _endpos in
            let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 2603 "parser.ml"
            )), _startpos_id_), _endpos_f_, _, (f : (Ast.pformula_loc)), _startpos_f_), _endpos_e_, _, (e : (Ast.pexpr_loc))) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__8_ in
            let _v : (Ast.pformula_loc) = 
# 118 "parser.mly"
                                                            (mk_pformula_loc (PEG (id, f, e)) _startpos__1_ _endpos__8_)
# 2615 "parser.ml"
             in
            _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState317 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__12_ = _endpos in
            let (((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_id1_, (id1 : (
# 35 "parser.mly"
       (string)
# 2638 "parser.ml"
            )), _startpos_id1_), _endpos_id2_, (id2 : (
# 35 "parser.mly"
       (string)
# 2642 "parser.ml"
            )), _startpos_id2_), _endpos_f1_, _, (f1 : (Ast.pformula_loc)), _startpos_f1_), _endpos_f2_, _, (f2 : (Ast.pformula_loc)), _startpos_f2_), _endpos_e_, _, (e : (Ast.pexpr_loc))) = _menhir_stack in
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
# 120 "parser.mly"
                                                                                                  (mk_pformula_loc (PEU (id1, id2, f1, f2, e)) _startpos__1_ _endpos__12_)
# 2656 "parser.ml"
             in
            _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState321 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__8_ = _endpos in
            let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 2679 "parser.ml"
            )), _startpos_id_), _endpos_f_, _, (f : (Ast.pformula_loc)), _startpos_f_), _endpos_e_, _, (e : (Ast.pexpr_loc))) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__8_ in
            let _v : (Ast.pformula_loc) = 
# 116 "parser.mly"
                                                            (mk_pformula_loc (PEX (id, f, e)) _startpos__1_ _endpos__8_)
# 2691 "parser.ml"
             in
            _menhir_goto_formula _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_str_expr_list : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.pexpr_loc) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Iden _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
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
# 407 "parser.mly"
                                                              (mk_pexpr_loc (PWith (e1, str_el)) e1.ptyp _startpos_e1_ _endpos__5_)
# 2729 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Iden _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
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
# 207 "parser.mly"
                                     (
            let str_elt = List.map (fun (str, (pel:pexpr_loc)) -> (str, pel.ptyp)) str_el in
            mk_pexpr_loc (PRecord str_el) (PTRecord str_elt) _startpos__1_ _endpos__3_
        )
# 2762 "parser.ml"
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

and _menhir_goto_option_Vertical_ : _menhir_env -> 'ttv_tail -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Float _v ->
        _menhir_run181 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState205 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run180 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState205 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run179 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState205 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run174 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState205 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Underline ->
        _menhir_run173 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState205 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState205

and _menhir_run77 : _menhir_env -> ('ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position) * _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RB3 ->
        _menhir_reduce124 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_goto_expr_single_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.pexpr_loc list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState62 | MenhirState137 ->
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
# 190 "parser.mly"
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
# 2848 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (Ast.pexpr_loc)), _startpos__1_), _), _, (_3 : (Ast.pexpr_loc list))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.pexpr_loc list) = 
# 433 "parser.mly"
                                             (_1::_3)
# 2865 "parser.ml"
         in
        _menhir_goto_expr_single_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState63 ->
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
# 178 "parser.mly"
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
# 2905 "parser.ml"
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

and _menhir_run141 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RB2 | Vertical ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141

and _menhir_reduce29 : _menhir_env -> ('ttv_tail * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos_e_ in
    let _v : (Ast.pexpr_loc) = 
# 245 "parser.mly"
                            (
            mk_pexpr_loc (PNegi e) (PTInt) _startpos__1_ _endpos_e_
            (*match e.ptyp with
            | None | Some PTInt ->
                e.ptyp <- Some PTInt;
                mk_pexpr_loc (PNegi e) (Some PTInt) $startpos($1) $endpos(e)
            | Some t -> raise (Type_mismatch (e, t, PTInt))*)
        )
# 2988 "parser.ml"
     in
    _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_reduce30 : _menhir_env -> ('ttv_tail * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos_e_ in
    let _v : (Ast.pexpr_loc) = 
# 253 "parser.mly"
                               (
            mk_pexpr_loc (PNegf e) PTFloat _startpos__1_ _endpos_e_
        )
# 3003 "parser.ml"
     in
    _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_goto_nonempty_list_expr_single_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (Ast.pexpr_loc list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    match _menhir_s with
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_el_ = _endpos in
        let (el : (Ast.pexpr_loc list)) = _v in
        let (_menhir_stack, _endpos_id_, _menhir_s, (id : (
# 35 "parser.mly"
       (string)
# 3018 "parser.ml"
        )), _startpos_id_) = _menhir_stack in
        let _startpos = _startpos_id_ in
        let _endpos = _endpos_el_ in
        let _v : (Ast.pexpr_loc) = 
# 415 "parser.mly"
                                                (
            mk_pexpr_loc (PApply (id, el)) (PTVar (new_type_var ())) _startpos_id_ _endpos_el_
        )
# 3027 "parser.ml"
         in
        _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_xs_ = _endpos in
        let (xs : (Ast.pexpr_loc list)) = _v in
        let (_menhir_stack, _endpos_x_, _menhir_s, (x : (Ast.pexpr_loc)), _startpos_x_) = _menhir_stack in
        let _endpos = _endpos_xs_ in
        let _v : (Ast.pexpr_loc list) = 
# 197 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 3040 "parser.ml"
         in
        _menhir_goto_nonempty_list_expr_single_ _menhir_env _menhir_stack _endpos _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run96 : _menhir_env -> (('ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> Lexing.position -> _menhir_state -> 'ttv_return =
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
# 420 "parser.mly"
                                                (
        let e:Ast.pexpr_loc = e1 in
        let et1 = e.ptyp in
        match et1 with
        | PTAray pt -> mk_pexpr_loc (PAray_Field (e1, e2)) pt _startpos_e1_ _endpos__4_
        | PTVar _ -> mk_pexpr_loc (PAray_Field (e1, e2)) (PTVar (new_type_var ())) _startpos_e1_ _endpos__4_
        | _ -> raise (Type_mismatch (e1, et1, (PTAray (PTVar (new_type_var())))))        
        )
# 3066 "parser.ml"
     in
    _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_goto_separated_nonempty_list_Semicolon_expr_single_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (Ast.pexpr_loc list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    match _menhir_s with
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_el_ = _endpos in
        let (el : (Ast.pexpr_loc list)) = _v in
        let ((_menhir_stack, _endpos_e_, _menhir_s, (e : (Ast.pexpr_loc)), _startpos_e_), _) = _menhir_stack in
        let _2 = () in
        let _endpos = _endpos_el_ in
        let _v : (Ast.pexpr_loc) = 
# 162 "parser.mly"
                                                                                        (
            mk_pexpr_loc (PSeq (e::el)) (PTVar (new_type_var())) _startpos_e_ _endpos_el_
        )
# 3086 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v
    | MenhirState86 ->
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
# 3100 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_Semicolon_expr_single_ _menhir_env _menhir_stack _endpos _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce12 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : (Ast.pexpr_loc)), _startpos__1_) = _menhir_stack in
    let _endpos = _endpos__1_ in
    let _v : (Ast.pexpr_loc) = 
# 161 "parser.mly"
                  (_1)
# 3113 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _endpos _menhir_s _v

and _menhir_run83 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run76 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LB3 ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run88 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState88 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState88 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState88 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState88 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run90 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_run92 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92

and _menhir_run97 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState97 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState97 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState97 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState97 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97

and _menhir_run99 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99

and _menhir_run101 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101

and _menhir_run103 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState103 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState103 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState103 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState103 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103

and _menhir_run111 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111

and _menhir_run94 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94

and _menhir_run113 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState113 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState113 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState113 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState113 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113

and _menhir_run115 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115

and _menhir_run117 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117

and _menhir_run105 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_run119 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119

and _menhir_run107 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107

and _menhir_run109 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.pexpr_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109

and _menhir_goto_nonempty_list_constr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.ptyp option) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState240 | MenhirState5 | MenhirState6 | MenhirState13 | MenhirState14 | MenhirState30 | MenhirState15 | MenhirState16 | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (cl : ((string * Ast.ptyp option) list)) = _v in
        let _v : ((string * Ast.ptyp option) list) = 
# 130 "parser.mly"
                                    (cl)
# 3941 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : ((string * Ast.ptyp option) list)) = _v in
        let _v : (Ast.ptyp) = 
# 143 "parser.mly"
              (PTConstrs _1)
# 3949 "parser.ml"
         in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((string * Ast.ptyp option) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string * Ast.ptyp option))) = _menhir_stack in
        let _v : ((string * Ast.ptyp option) list) = 
# 197 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 3960 "parser.ml"
         in
        _menhir_goto_nonempty_list_constr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_Semicolon_str_typ_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.ptyp) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Ast.ptyp))), _, (xs : ((string * Ast.ptyp) list))) = _menhir_stack in
        let _2 = () in
        let _v : ((string * Ast.ptyp) list) = 
# 217 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 3978 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_Semicolon_str_typ_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState11 ->
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
# 154 "parser.mly"
                                                                          (str_pts)
# 3998 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : ((string * Ast.ptyp) list)) = _v in
            let _v : (Ast.ptyp) = 
# 145 "parser.mly"
                 (PTRecord _1)
# 4006 "parser.ml"
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
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (Ast.ptyp))), _), _, (_3 : (Ast.ptyp list))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ptyp list) = 
# 151 "parser.mly"
                          (_1 :: _3)
# 4030 "parser.ml"
         in
        _menhir_goto_tuple_typ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState14 ->
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
# 144 "parser.mly"
                        (PTTuple _2)
# 4050 "parser.ml"
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

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ptyp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBool ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | TFloat ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | TInt ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | TUnt ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | UIden _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run17 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ptyp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ptyp))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ptyp) = 
# 141 "parser.mly"
               (PTLst (_1))
# 4098 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ptyp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ptyp))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.ptyp) = 
# 140 "parser.mly"
                (PTAray (_1))
# 4111 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run19 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ptyp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState19 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBool ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | TFloat ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | TInt ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | TUnt ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | UIden _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState19 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_goto_list_typ_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ptyp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.ptyp list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.ptyp))) = _menhir_stack in
        let _v : (Ast.ptyp list) = 
# 187 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 4153 "parser.ml"
         in
        _menhir_goto_list_typ_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (tl : (Ast.ptyp list)) = _v in
        let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 35 "parser.mly"
       (string)
# 4163 "parser.ml"
        )), _startpos__1_) = _menhir_stack in
        let _v : (Ast.ptyp) = 
# 142 "parser.mly"
                          (PTUdt (_1, tl))
# 4168 "parser.ml"
         in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce124 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Ast.pexpr_loc) list) = 
# 436 "parser.mly"
                ([])
# 4179 "parser.ml"
     in
    _menhir_goto_str_expr_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run60 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "parser.mly"
       (string)
# 4186 "parser.ml"
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Float _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | For ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Iden _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | If ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Int _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB1 ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB2 ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB3 ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Match ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Negb ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | True ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIden _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Val ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Var ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | While ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.pexpr_loc list) = 
# 431 "parser.mly"
                    ([])
# 4250 "parser.ml"
     in
    _menhir_goto_expr_single_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Vertical ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_goto_expr_single : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (Ast.pexpr_loc) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | Ando ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | DotDot ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState75 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LArrow ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | Oro ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | With ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
    | MenhirState321 | MenhirState317 | MenhirState311 | MenhirState307 | MenhirState303 | MenhirState297 | MenhirState261 | MenhirState259 | MenhirState251 | MenhirState246 | MenhirState233 | MenhirState216 | MenhirState203 | MenhirState61 | MenhirState159 | MenhirState155 | MenhirState148 | MenhirState146 | MenhirState126 | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | Ando ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LArrow ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | Oro ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | Semicolon ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | With ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | And | Comma | Do | Done | DotDot | EOF | Else | False | Float _ | For | Iden _ | If | Int _ | LB1 | LB3 | Match | Model | Negb | Or | RB1 | RB2 | RB3 | Then | True | UIden _ | Val | Var | Vertical | While ->
            _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
    | MenhirState86 | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | Ando ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LArrow ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | Oro ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState85 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
        | With ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | And | Comma | Do | Done | DotDot | EOF | Else | False | Float _ | For | Iden _ | If | Int _ | LB1 | LB3 | Match | Model | Negb | Or | RB1 | RB2 | RB3 | Then | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos_x_, _menhir_s, (x : (Ast.pexpr_loc)), _startpos_x_) = _menhir_stack in
            let _endpos = _endpos_x_ in
            let _v : (Ast.pexpr_loc list) = 
# 215 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 4537 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_Semicolon_expr_single_ _menhir_env _menhir_stack _endpos _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | Ando ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | And | Comma | Do | Done | DotDot | EOF | Else | False | Float _ | For | Iden _ | If | Int _ | LArrow | LB1 | LB3 | Match | Model | Negb | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While | With ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 232 "parser.mly"
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
# 4598 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | And | Ando | Comma | Do | Done | DotDot | EOF | Else | False | Float _ | For | GE | GT | Iden _ | If | Int _ | LArrow | LB1 | LB3 | LE | LT | Match | Model | Negb | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While | With ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 335 "parser.mly"
                                                  (mk_pexpr_loc (PNon_Equal (e1, e2)) (PTBool) _startpos_e1_ _endpos_e2_)
# 4633 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Add | AddDot | And | Ando | Comma | Do | Done | DotDot | EOF | Else | Equal | False | Float _ | For | GE | GT | Iden _ | If | Int _ | LArrow | LB1 | LB3 | LE | LT | Match | Minus | MinusDot | Model | Mult | MultDot | Negb | Non_Equal | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While | With ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 321 "parser.mly"
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
# 4668 "parser.ml"
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
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | Ando ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | LArrow ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | Oro ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | RB2 ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95
        | With ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Add | AddDot | And | Ando | Comma | Do | Done | DotDot | EOF | Else | Equal | False | Float _ | For | GE | GT | Iden _ | If | Int _ | LArrow | LB1 | LB3 | LE | LT | Match | Minus | MinusDot | Model | Mult | MultDot | Negb | Non_Equal | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While | With ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 282 "parser.mly"
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
# 4748 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | Add | AddDot | And | Ando | Comma | Do | Done | DotDot | EOF | Else | Equal | False | Float _ | For | GE | GT | Iden _ | If | Int _ | LArrow | LB1 | LB3 | LE | LT | Match | Model | Negb | Non_Equal | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While | With ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _, _startpos__2_), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 308 "parser.mly"
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
# 4791 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | Add | AddDot | And | Ando | Comma | Do | Done | DotDot | EOF | Else | Equal | False | Float _ | For | GE | GT | Iden _ | If | Int _ | LArrow | LB1 | LB3 | LE | LT | Match | Model | Negb | Non_Equal | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While | With ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _, _startpos__2_), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 269 "parser.mly"
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
# 4834 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | And | Ando | Comma | Do | Done | DotDot | EOF | Else | False | Float _ | For | Iden _ | If | Int _ | LArrow | LB1 | LB3 | Match | Model | Negb | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While | With ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 336 "parser.mly"
                                              (mk_pexpr_loc (PLT (e1, e2)) (PTBool) _startpos_e1_ _endpos_e2_)
# 4873 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | And | Ando | Comma | Do | Done | DotDot | EOF | Else | False | Float _ | For | GE | GT | Iden _ | If | Int _ | LArrow | LB1 | LB3 | LE | LT | Match | Model | Negb | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While | With ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 334 "parser.mly"
                                              (mk_pexpr_loc (PEqual (e1, e2)) (PTBool) _startpos_e1_ _endpos_e2_)
# 4908 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | Add | AddDot | And | Ando | Comma | Do | Done | DotDot | EOF | Else | Equal | False | Float _ | For | GE | GT | Iden _ | If | Int _ | LArrow | LB1 | LB3 | LE | LT | Match | Model | Negb | Non_Equal | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While | With ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 295 "parser.mly"
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
# 4951 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | Add | AddDot | And | Ando | Comma | Do | Done | DotDot | EOF | Else | Equal | False | Float _ | For | GE | GT | Iden _ | If | Int _ | LArrow | LB1 | LB3 | LE | LT | Match | Model | Negb | Non_Equal | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While | With ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 256 "parser.mly"
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
# 4994 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | And | Ando | Comma | Do | Done | DotDot | EOF | Else | False | Float _ | For | Iden _ | If | Int _ | LArrow | LB1 | LB3 | Match | Model | Negb | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While | With ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 338 "parser.mly"
                                              (mk_pexpr_loc (PLE (e1, e2)) (PTBool) _startpos_e1_ _endpos_e2_)
# 5033 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | Ando ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | Oro ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | And | Comma | Do | Done | DotDot | EOF | Else | False | Float _ | For | Iden _ | If | Int _ | LB1 | LB3 | Match | Model | Negb | Or | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 405 "parser.mly"
                                                  (mk_pexpr_loc (PAssign (e1, e2)) (PTUnt) _startpos_e1_ _endpos_e2_)
# 5084 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | And | Ando | Comma | Do | Done | DotDot | EOF | Else | False | Float _ | For | Iden _ | If | Int _ | LArrow | LB1 | LB3 | Match | Model | Negb | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While | With ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 337 "parser.mly"
                                              (mk_pexpr_loc (PGT (e1, e2)) (PTBool) _startpos_e1_ _endpos_e2_)
# 5123 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | And | Ando | Comma | Do | Done | DotDot | EOF | Else | False | Float _ | For | Iden _ | If | Int _ | LArrow | LB1 | LB3 | Match | Model | Negb | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While | With ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 339 "parser.mly"
                                              (mk_pexpr_loc (PGE (e1, e2)) (PTBool) _startpos_e1_ _endpos_e2_)
# 5162 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | And | Ando | Comma | Do | Done | DotDot | EOF | Else | False | Float _ | For | Iden _ | If | Int _ | LArrow | LB1 | LB3 | Match | Model | Negb | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While | With ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (Ast.pexpr_loc)), _startpos_e1_), _), _endpos_e2_, _, (e2 : (Ast.pexpr_loc)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.pexpr_loc) = 
# 219 "parser.mly"
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
# 5221 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | Ando ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | LArrow ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | Oro ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | RB2 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_s = MenhirState124 in
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
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Float _v ->
                    _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | For ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Iden _v ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | If ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Int _v ->
                    _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LB1 ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LB2 ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LB3 ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Match ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Minus ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MinusDot ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Negb ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | True ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UIden _v ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Val ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Var ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | While ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | With ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | Ando ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | LArrow ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | Oro ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | With ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | And | Comma | Do | Done | DotDot | EOF | Else | False | Float _ | For | Iden _ | If | Int _ | LB1 | LB3 | Match | Model | Negb | Or | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 35 "parser.mly"
       (string)
# 5374 "parser.ml"
            )), _startpos__1_), _), _endpos__3_, _, (_3 : (Ast.pexpr_loc)), _startpos__3_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.pexpr_loc) = 
# 168 "parser.mly"
                               (
            let nt = PTVar (new_type_var ()) in
            mk_pexpr_loc (PDot (mk_pexpr_loc (PSymbol _1) nt _startpos__1_ _endpos__1_, _3)) nt _startpos__1_ _endpos__3_
        )
# 5385 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
    | MenhirState132 | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | Ando ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | False ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Float _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | For ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | Iden _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | If ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Int _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LArrow ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | LB1 ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB2 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState132 in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState137 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState137 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState137 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState137 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Vertical ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB2 ->
                _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137)
        | LB3 ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | Match ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState132 in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState135 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState135 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState135 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState135 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState135 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState135 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135)
        | MinusDot ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState132 in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133)
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | Negb ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | Oro ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | True ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIden _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Val ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Var ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | While ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | With ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | And | Comma | Do | Done | DotDot | EOF | Else | Model | Or | RB1 | RB2 | RB3 | Semicolon | Then | Vertical ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos_x_, _menhir_s, (x : (Ast.pexpr_loc)), _startpos_x_) = _menhir_stack in
            let _endpos = _endpos_x_ in
            let _v : (Ast.pexpr_loc list) = 
# 195 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 5610 "parser.ml"
             in
            _menhir_goto_nonempty_list_expr_single_ _menhir_env _menhir_stack _endpos _menhir_s _v
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
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | Ando ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | LArrow ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | Oro ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | With ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | And | Comma | Do | Done | DotDot | EOF | Else | False | Float _ | For | Iden _ | If | Int _ | LB1 | LB3 | Match | Model | Negb | Or | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While ->
            _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134)
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | Ando ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | LArrow ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | Oro ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | With ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | And | Comma | Do | Done | DotDot | EOF | Else | False | Float _ | For | Iden _ | If | Int _ | LB1 | LB3 | Match | Model | Negb | Or | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While ->
            _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | Ando ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | LArrow ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | Oro ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | RB2 ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState140
        | Semicolon ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | With ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140)
    | MenhirState62 | MenhirState63 | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | Ando ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | LArrow ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | Oro ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | Semicolon ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | With ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | RB2 | Vertical ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : (Ast.pexpr_loc)), _startpos__1_) = _menhir_stack in
            let _v : (Ast.pexpr_loc list) = 
# 432 "parser.mly"
                    ([_1])
# 5801 "parser.ml"
             in
            _menhir_goto_expr_single_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | Ando ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | LArrow ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | Oro ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | Then ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState145 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState146 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState146 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState146 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState146 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146)
        | With ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | Ando ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | LArrow ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | Oro ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_s = MenhirState152 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : (Ast.pexpr_loc)), _startpos__2_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.pexpr_loc) = 
# 428 "parser.mly"
                           (_2)
# 5950 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | Semicolon ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | With ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | Comma ->
            _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | Ando ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | LArrow ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | Oro ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | With ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState168 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LB3 ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Vertical ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let x = () in
                let _v : (unit option) = 
# 102 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 6017 "parser.ml"
                 in
                _menhir_goto_option_Vertical_ _menhir_env _menhir_stack _v
            | Float _ | Iden _ | Int _ | LB1 | LB2 | UIden _ | Underline ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _v : (unit option) = 
# 100 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( None )
# 6025 "parser.ml"
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | Add | AddDot | And | Ando | Comma | Do | Done | DotDot | EOF | Else | Equal | False | Float _ | For | GE | GT | Iden _ | If | Int _ | LArrow | LB1 | LB3 | LE | LT | Match | Model | Negb | Non_Equal | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While | With ->
            _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState207)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | Add | AddDot | And | Ando | Comma | Do | Done | DotDot | EOF | Else | Equal | False | Float _ | For | GE | GT | Iden _ | If | Int _ | LArrow | LB1 | LB3 | LE | LT | Match | Model | Negb | Non_Equal | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While | With ->
            _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState208)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | And | Ando | Comma | Do | Done | DotDot | EOF | Else | False | Float _ | For | Iden _ | If | Int _ | LArrow | LB1 | LB3 | Match | Model | Negb | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While | With ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _v : (Ast.pexpr_loc) = 
# 211 "parser.mly"
                               (
            mk_pexpr_loc (PNegb e) (PTBool) _startpos__1_ _endpos_e_
            (*match e.ptyp with
            | None | Some PTBool -> 
                e.ptyp <- Some PTBool; 
                mk_pexpr_loc (PNegb e) (Some PTBool) $startpos($1) $endpos(e)
            | Some t -> raise (Type_mismatch (e, t, PTBool))*)
        )
# 6127 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState209)
    | MenhirState210 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | Ando ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | LArrow ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState211 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState211 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState211 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | Oro ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | With ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | And | Comma | Do | Done | DotDot | EOF | Else | False | Float _ | For | Iden _ | If | Int _ | LB1 | LB3 | Match | Model | Negb | Or | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 35 "parser.mly"
       (string)
# 6178 "parser.ml"
            )), _startpos__1_), _), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _v : (Ast.pexpr_loc) = 
# 172 "parser.mly"
                                (
            mk_pexpr_loc (PDot (mk_pexpr_loc (PSymbol _1) e.ptyp _startpos__1_ _endpos__1_, e)) e.ptyp _startpos__1_ _endpos_e_
        )
# 6188 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState211)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | Ando ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | LArrow ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | Oro ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | With ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | And | Comma | Do | Done | DotDot | EOF | Else | False | Float _ | For | Iden _ | If | Int _ | LB1 | LB3 | Match | Model | Negb | Or | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_uid_, _menhir_s, (uid : (
# 35 "parser.mly"
       (string)
# 6239 "parser.ml"
            )), _startpos_uid_), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
            let _startpos = _startpos_uid_ in
            let _endpos = _endpos_e_ in
            let _v : (Ast.pexpr_loc) = 
# 409 "parser.mly"
                                  (
            mk_pexpr_loc (PConstr ((PConstr_compound (uid, e)))) (PTVar (new_type_var ())) _startpos_uid_ _endpos_e_
            (*match eo with
            | None -> mk_pexpr_loc (PConstr (mk_pconstr_loc (PConstr_basic uid) $startpos(uid) $endpos(eo))) None $startpos(uid) $endpos(eo)
            | Some e -> *)
        )
# 6251 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState212)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState213
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState213
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState213
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState213
        | And | Ando | Comma | Do | Done | DotDot | EOF | Else | False | Float _ | For | GE | GT | Iden _ | If | Int _ | LArrow | LB1 | LB3 | LE | LT | Match | Model | Negb | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While | With ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 6282 "parser.ml"
            )), _startpos_id_), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _v : (Ast.pexpr_loc) = 
# 418 "parser.mly"
                                            (mk_pexpr_loc (PLocal_Val (id, e)) (PTUnt) _startpos__1_ _endpos_e_)
# 6291 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState213)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState214 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState214 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState214 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | And | Ando | Comma | Do | Done | DotDot | EOF | Else | False | Float _ | For | GE | GT | Iden _ | If | Int _ | LArrow | LB1 | LB3 | LE | LT | Match | Model | Negb | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | True | UIden _ | Val | Var | Vertical | While | With ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 6322 "parser.ml"
            )), _startpos_id_), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _v : (Ast.pexpr_loc) = 
# 419 "parser.mly"
                                            (mk_pexpr_loc (PLocal_Var (id, e)) (PTUnt) _startpos__1_ _endpos_e_)
# 6331 "parser.ml"
             in
            _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState214)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | Ando ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | Do ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState215 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState216 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState216 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState216 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState216 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState216 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState216 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState216)
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | LArrow ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState215 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState215 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState215 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | Oro ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | With ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState215
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState215)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState219
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState219
        | Ando ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState219
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState219
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState219
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState219
        | LArrow ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState219
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState219
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState219
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState219
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState219
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState219
        | Oro ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState219
        | With ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState219
        | EOF | Model ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _startpos__1_), _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 6472 "parser.ml"
            )), _startpos_id_), _, (ote : (Ast.ptyp option))), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : (unit) = 
# 74 "parser.mly"
                                                                       (
            match ote with
            | None -> Hashtbl.add symbol_tbl id (Var, PExpr_loc (PTVar (new_type_var ()), e))
            | Some pt -> Hashtbl.add symbol_tbl id (Var, PExpr_loc (pt, e))
        )
# 6483 "parser.ml"
             in
            _menhir_goto_declars _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState219)
    | MenhirState223 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Add ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | AddDot ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | Ando ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | Equal ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | GE ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | GT ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | LArrow ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | LB2 ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LE ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | Minus ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Mult ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | MultDot ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | Non_Equal ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | Oro ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | With ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | EOF | Model ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _startpos__1_), _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 6534 "parser.ml"
            )), _startpos_id_), _, (ote : (Ast.ptyp option))), _endpos_e_, _, (e : (Ast.pexpr_loc)), _startpos_e_) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : (unit) = 
# 79 "parser.mly"
                                                                       (
            match ote with
            | None -> Hashtbl.add symbol_tbl id (Val, PExpr_loc (PTVar (new_type_var ()), e))
            | Some pt -> Hashtbl.add symbol_tbl id (Val, PExpr_loc (pt, e))
        )
# 6545 "parser.ml"
             in
            _menhir_goto_declars _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState224)
    | _ ->
        _menhir_fail ()

and _menhir_goto_constr : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * Ast.ptyp option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | UIden _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState24 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Arrow | Comma | EOF | Equal | Iden _ | LB1 | LB3 | Model | RB1 | RB3 | Semicolon | TAray | TBool | TFloat | TInt | TLst | TUnt ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (string * Ast.ptyp option))) = _menhir_stack in
        let _v : ((string * Ast.ptyp option) list) = 
# 195 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 6570 "parser.ml"
         in
        _menhir_goto_nonempty_list_constr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ptyp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState16 | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Arrow ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | Iden _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB1 ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB3 ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TAray ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | TBool ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | TFloat ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | TInt ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | TLst ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | TUnt ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | UIden _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Comma | EOF | Equal | Model | RB1 | RB3 | Semicolon ->
            _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Arrow ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | TAray ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | TLst ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | Comma | EOF | Equal | Iden _ | LB1 | LB3 | Model | RB1 | RB3 | Semicolon | TBool | TFloat | TInt | TUnt | UIden _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ptyp))), _), _, (_3 : (Ast.ptyp))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ptyp) = 
# 146 "parser.mly"
                    (PTArrow (_1, _3))
# 6633 "parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Arrow ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | Comma ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_s = MenhirState28 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (t : (Ast.ptyp))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ptyp) = 
# 147 "parser.mly"
                        (t)
# 6662 "parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | TAray ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | TLst ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Arrow ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | Comma ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | TAray ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | TLst ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Ast.ptyp))), _), _, (_3 : (Ast.ptyp))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ptyp list) = 
# 150 "parser.mly"
                         ([_1; _3])
# 6693 "parser.ml"
             in
            _menhir_goto_tuple_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Arrow ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | TAray ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | TLst ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | RB3 | Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 35 "parser.mly"
       (string)
# 6716 "parser.ml"
            )), _startpos__1_), _, (_3 : (Ast.ptyp))) = _menhir_stack in
            let _2 = () in
            let _v : (string * Ast.ptyp) = 
# 157 "parser.mly"
                        ((_1, _3))
# 6722 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Semicolon ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Iden _v ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState37 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37)
            | RB3 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (string * Ast.ptyp))) = _menhir_stack in
                let _v : ((string * Ast.ptyp) list) = 
# 215 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 6746 "parser.ml"
                 in
                _menhir_goto_separated_nonempty_list_Semicolon_str_typ_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Arrow ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | TAray ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | TLst ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | Comma | EOF | Equal | Iden _ | LB1 | LB3 | Model | RB1 | RB3 | Semicolon | TBool | TFloat | TInt | TUnt | UIden _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_uid_, _menhir_s, (uid : (
# 35 "parser.mly"
       (string)
# 6775 "parser.ml"
            )), _startpos_uid_), _, (t : (Ast.ptyp))) = _menhir_stack in
            let _v : (string * Ast.ptyp option) = 
# 133 "parser.mly"
                          ((uid, Some t))
# 6780 "parser.ml"
             in
            _menhir_goto_constr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Arrow ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | TAray ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | TLst ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | Equal ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.ptyp))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ptyp) = 
# 90 "parser.mly"
                        (_2)
# 6805 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (x : (Ast.ptyp)) = _v in
            let _v : (Ast.ptyp option) = 
# 102 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 6813 "parser.ml"
             in
            _menhir_goto_option_type_of_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
    | MenhirState240 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Arrow ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | TAray ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | TLst ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | EOF | Model ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_id_, (id : (
# 35 "parser.mly"
       (string)
# 6836 "parser.ml"
            )), _startpos_id_), _, (args : (string list))), _, (t : (Ast.ptyp))) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : (unit) = 
# 73 "parser.mly"
                                                         (Hashtbl.add symbol_tbl id (UDT, PTyp (erase_type_args t args)))
# 6843 "parser.ml"
             in
            _menhir_goto_declars _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState241)
    | _ ->
        _menhir_fail ()

and _menhir_run12 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "parser.mly"
       (string)
# 6856 "parser.ml"
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
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState13 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB1 ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB3 ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TBool ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | TFloat ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | TInt ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | TUnt ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | UIden _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState13 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce78 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ptyp list) = 
# 185 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 6900 "parser.ml"
     in
    _menhir_goto_list_typ_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_args : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ppattern_loc list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState229 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (Ast.ppattern_loc)), _startpos__1_), _, (_2 : (Ast.ppattern_loc list))) = _menhir_stack in
        let _v : (Ast.ppattern_loc list) = 
# 94 "parser.mly"
                    (_1 :: _2)
# 6915 "parser.ml"
         in
        _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v
    | MenhirState228 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Colon ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | Equal ->
            _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState231)
    | _ ->
        _menhir_fail ()

and _menhir_goto_pattern_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ppattern_loc list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState176 ->
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
# 459 "parser.mly"
                                                   (
            match pl with
            | [] -> mk_ppat_loc (PPat_Aray []) (PTAray (PTVar (new_type_var()))) _startpos__1_ _endpos__5_
            | p::pl' -> mk_ppat_loc (PPat_Aray (pl)) (PTAray p.ptyp) _startpos__1_ _endpos__5_
        )
# 6968 "parser.ml"
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
    | MenhirState196 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (Ast.ppattern_loc)), _startpos__1_), _), _, (_3 : (Ast.ppattern_loc list))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ppattern_loc list) = 
# 480 "parser.mly"
                                     (_1 :: _3)
# 6991 "parser.ml"
         in
        _menhir_goto_pattern_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState175 ->
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
# 464 "parser.mly"
                                  (
            match pl with
            | [] -> mk_ppat_loc (PPat_Lst []) (PTLst (PTVar (new_type_var()))) _startpos__1_ _endpos__3_
            | p::pl' -> mk_ppat_loc (PPat_Lst pl) (PTLst p.ptyp) _startpos__1_ _endpos__3_
        )
# 7017 "parser.ml"
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
    | MenhirState184 ->
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
# 471 "parser.mly"
                                                                               (mk_ppat_loc (PPat_Tuple (p::pl)) (PTTuple (List.map (fun pat -> pat.ptyp) (p::pl))) _startpos__1_ _endpos__5_)
# 7053 "parser.ml"
             in
            _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _endpos_x_, _menhir_s, (x : (Ast.ppattern_loc)), _startpos_x_), _), _, (xs : (Ast.ppattern_loc list))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ppattern_loc list) = 
# 217 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 7070 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_Comma_pattern_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run190 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (Ast.ppattern_loc) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Float _v ->
        _menhir_run181 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState190 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run180 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState190 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run179 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState190 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run174 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState190 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Underline ->
        _menhir_run173 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState190 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState190

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
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

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
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

and _menhir_run53 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "parser.mly"
       (string)
# 7300 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Dot ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState53 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | False ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState210 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Float _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState210 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | For ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Iden _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState210 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | If ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Int _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState210 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB1 ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB2 ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB3 ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Match ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Negb ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | True ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState210 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIden _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState210 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Val ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Var ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | While ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState210)
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Add | AddDot | And | Ando | Comma | Do | Done | DotDot | EOF | Else | Equal | GE | GT | LArrow | LE | LT | Model | Mult | MultDot | Non_Equal | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | Vertical | With ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _endpos_uid_, _menhir_s, (uid : (
# 35 "parser.mly"
       (string)
# 7395 "parser.ml"
        )), _startpos_uid_) = _menhir_stack in
        let _startpos = _startpos_uid_ in
        let _endpos = _endpos_uid_ in
        let _v : (Ast.pexpr_loc) = 
# 408 "parser.mly"
                  (mk_pexpr_loc (PConstr ((PConstr_basic uid))) (PTVar (new_type_var ())) _startpos_uid_ _endpos_uid_)
# 7402 "parser.ml"
         in
        _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run54 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.pexpr_loc) = 
# 201 "parser.mly"
            (mk_pexpr_loc (PBool true) (PTBool) _startpos__1_ _endpos__1_)
# 7422 "parser.ml"
     in
    _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState57 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState59 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RB3 ->
        _menhir_reduce124 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Vertical ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RB2 ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RB1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_s = MenhirState64 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__2_ = _endpos in
        let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (Ast.pexpr_loc) = 
# 177 "parser.mly"
                (mk_pexpr_loc PUnt (PTUnt) _startpos__1_ _endpos__2_)
# 7727 "parser.ml"
         in
        _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run66 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 33 "parser.mly"
       (int)
# 7748 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_i_ = _endpos in
    let (i : (
# 33 "parser.mly"
       (int)
# 7757 "parser.ml"
    )) = _v in
    let _startpos_i_ = _startpos in
    let _startpos = _startpos_i_ in
    let _endpos = _endpos_i_ in
    let _v : (Ast.pexpr_loc) = 
# 175 "parser.mly"
                (mk_pexpr_loc (PInt i) (PTInt) _startpos_i_ _endpos_i_)
# 7765 "parser.ml"
     in
    _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run68 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "parser.mly"
       (string)
# 7819 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Dot ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState68 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | False ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Float _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | For ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Iden _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | If ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Int _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB1 ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB2 ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB3 ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Match ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Minus ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MinusDot ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Negb ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | True ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIden _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState129 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Val ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Var ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | While ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
    | False ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Float _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | For ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | If ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Match ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Minus ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MinusDot ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Negb ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | True ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Val ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Var ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | While ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Add | AddDot | And | Ando | Comma | Do | Done | DotDot | EOF | Else | Equal | GE | GT | LArrow | LE | LT | Model | Mult | MultDot | Non_Equal | Or | Oro | RB1 | RB2 | RB3 | Semicolon | Then | Vertical | With ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _endpos_id_, _menhir_s, (id : (
# 35 "parser.mly"
       (string)
# 7914 "parser.ml"
        )), _startpos_id_) = _menhir_stack in
        let _startpos = _startpos_id_ in
        let _endpos = _endpos_id_ in
        let _v : (Ast.pexpr_loc) = 
# 167 "parser.mly"
                       (mk_pexpr_loc (PSymbol id) (PTVar (new_type_var ())) _startpos_id_ _endpos_id_)
# 7921 "parser.ml"
         in
        _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Float _v ->
                    _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | For ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Iden _v ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | If ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Int _v ->
                    _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LB1 ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LB2 ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LB3 ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Match ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Minus ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MinusDot ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Negb ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | True ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UIden _v ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Val ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Var ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | While ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
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

and _menhir_run73 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 34 "parser.mly"
       (float)
# 8017 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_f_ = _endpos in
    let (f : (
# 34 "parser.mly"
       (float)
# 8026 "parser.ml"
    )) = _v in
    let _startpos_f_ = _startpos in
    let _startpos = _startpos_f_ in
    let _endpos = _endpos_f_ in
    let _v : (Ast.pexpr_loc) = 
# 176 "parser.mly"
                (mk_pexpr_loc (PFloat f) (PTFloat) _startpos_f_ _endpos_f_)
# 8034 "parser.ml"
     in
    _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run74 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.pexpr_loc) = 
# 202 "parser.mly"
            (mk_pexpr_loc (PBool false) (PTBool) _startpos__1_ _endpos__1_)
# 8050 "parser.ml"
     in
    _menhir_goto_expr_single _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> (
# 41 "parser.mly"
       ((string list) * (Ast.psymbol_tbl) * ((Ast.pkripke_model) option))
# 8057 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 41 "parser.mly"
       ((string list) * (Ast.psymbol_tbl) * ((Ast.pkripke_model) option))
# 8065 "parser.ml"
    )) = _v in
    Obj.magic _1

and _menhir_goto_option_type_of_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ptyp option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState4 ->
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
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState45 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState45 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState45 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState45 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState221 ->
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
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState223 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState223 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState223 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState223 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState223 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState223 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState223 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState223 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState223 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState223 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState223 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState223 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState223 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState223 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState223 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState223 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState223 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState223 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState223)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState231 ->
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
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState233 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState233 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState233 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState233 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState233 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState233 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState233)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run6 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "parser.mly"
       (string)
# 8247 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState6 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBool ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | TFloat ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | TInt ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | TUnt ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | UIden _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState6 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Arrow | Comma | EOF | Equal | Model | RB1 | RB3 | Semicolon | TAray | TLst ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _endpos_uid_, _menhir_s, (uid : (
# 35 "parser.mly"
       (string)
# 8275 "parser.ml"
        )), _startpos_uid_) = _menhir_stack in
        let _v : (string * Ast.ptyp option) = 
# 132 "parser.mly"
                    ((uid, None))
# 8280 "parser.ml"
         in
        _menhir_goto_constr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ptyp) = 
# 139 "parser.mly"
            (PTUnt)
# 8296 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ptyp) = 
# 136 "parser.mly"
          (PTInt)
# 8308 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ptyp) = 
# 138 "parser.mly"
             (PTFloat)
# 8320 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ptyp) = 
# 137 "parser.mly"
            (PTBool)
# 8332 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBool ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TFloat ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TInt ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TUnt ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | UIden _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run15 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "parser.mly"
       (string)
# 8379 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState15 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBool ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | TFloat ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | TInt ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | TUnt ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | UIden _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState15 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Arrow | Comma | EOF | Equal | Model | RB1 | RB3 | Semicolon | TAray | TLst ->
        _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_reduce107 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ppattern_loc list) = 
# 478 "parser.mly"
                ([])
# 8414 "parser.ml"
     in
    _menhir_goto_pattern_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_pattern : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (Ast.ppattern_loc) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ColonColon ->
            _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState182 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Float _v ->
                _menhir_run181 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState184 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run180 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState184 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run179 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState184 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run174 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState184 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Underline ->
                _menhir_run173 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState184)
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_s = MenhirState182 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : (Ast.ppattern_loc)), _startpos__2_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.ppattern_loc) = 
# 475 "parser.mly"
                        (_2)
# 8469 "parser.ml"
             in
            _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState182)
    | MenhirState188 | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ColonColon ->
            _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | Comma ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState187 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Float _v ->
                _menhir_run181 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState188 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run180 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState188 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run179 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState188 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run174 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState188 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Underline ->
                _menhir_run173 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState188)
        | RB1 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos_x_, _menhir_s, (x : (Ast.ppattern_loc)), _startpos_x_) = _menhir_stack in
            let _v : (Ast.ppattern_loc list) = 
# 215 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 8514 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_Comma_pattern_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState187)
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ColonColon ->
            _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | Arrow | Colon | Comma | Equal | Float _ | Iden _ | Int _ | LB1 | LB2 | RB1 | RB2 | Semicolon | UIden _ | Underline | Vertical ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_p1_, _menhir_s, (p1 : (Ast.ppattern_loc)), _startpos_p1_), _), _endpos_p2_, _, (p2 : (Ast.ppattern_loc)), _startpos_p2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_p1_ in
            let _endpos = _endpos_p2_ in
            let _v : (Ast.ppattern_loc) = 
# 469 "parser.mly"
                                              (mk_ppat_loc (PPat_Lst_Cons (p1, p2)) (p2.ptyp) _startpos_p1_ _endpos_p2_)
# 8537 "parser.ml"
             in
            _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191)
    | MenhirState175 | MenhirState196 | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ColonColon ->
            _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | Semicolon ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState195 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Float _v ->
                _menhir_run181 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState196 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run180 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState196 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run179 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState196 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run174 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState196 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Underline ->
                _menhir_run173 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState196 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RB2 | Vertical ->
                _menhir_reduce107 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState196)
        | RB2 | Vertical ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : (Ast.ppattern_loc)), _startpos__1_) = _menhir_stack in
            let _v : (Ast.ppattern_loc list) = 
# 479 "parser.mly"
                ([_1])
# 8584 "parser.ml"
             in
            _menhir_goto_pattern_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState195)
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ColonColon ->
            _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | Arrow | Colon | Comma | Equal | Float _ | Iden _ | Int _ | LB1 | LB2 | RB1 | RB2 | Semicolon | UIden _ | Underline | Vertical ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_uid_, _menhir_s, (uid : (
# 35 "parser.mly"
       (string)
# 8603 "parser.ml"
            )), _startpos_uid_), _endpos_p_, _, (p : (Ast.ppattern_loc)), _startpos_p_) = _menhir_stack in
            let _startpos = _startpos_uid_ in
            let _endpos = _endpos_p_ in
            let _v : (Ast.ppattern_loc) = 
# 474 "parser.mly"
                               (mk_ppat_loc (PPat_Constr (uid, Some p)) (PTVar (new_type_var())) _startpos_uid_ _endpos_p_)
# 8610 "parser.ml"
             in
            _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState200)
    | MenhirState205 | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Arrow ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState202 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState203)
        | ColonColon ->
            _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState202
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState202)
    | MenhirState229 | MenhirState228 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ColonColon ->
            _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState229
        | Float _v ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState229 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Iden _v ->
            _menhir_run180 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState229 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Int _v ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState229 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB1 ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState229 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB2 ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState229 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIden _v ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState229 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Underline ->
            _menhir_run173 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState229 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Colon | Equal ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : (Ast.ppattern_loc)), _startpos__1_) = _menhir_stack in
            let _v : (Ast.ppattern_loc list) = 
# 93 "parser.mly"
              ([_1])
# 8702 "parser.ml"
             in
            _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState229)
    | MenhirState249 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ColonColon ->
            _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState250
        | Equal ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState250 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | False ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState251 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Float _v ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState251 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | For ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState251 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState251 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | If ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState251 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState251 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState251 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState251 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState251 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Match ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState251 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Minus ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState251 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MinusDot ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState251 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Negb ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState251 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | True ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState251 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState251 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Val ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState251 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Var ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState251 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | While ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState251 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState251)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState250)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_Iden_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState237 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _endpos_x_, _menhir_s, (x : (
# 35 "parser.mly"
       (string)
# 8780 "parser.ml"
        )), _startpos_x_), _, (xs : (string list))) = _menhir_stack in
        let _v : (string list) = 
# 187 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 8785 "parser.ml"
         in
        _menhir_goto_list_Iden_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState236 ->
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
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState240 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB3 ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TBool ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState240
            | TFloat ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState240
            | TInt ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState240
            | TUnt ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState240
            | UIden _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState240 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState240)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_declars : _menhir_env -> 'ttv_tail -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, (_1 : (unit))), (_2 : (unit))) = _menhir_stack in
        let _3 = () in
        let _v : (
# 41 "parser.mly"
       ((string list) * (Ast.psymbol_tbl) * ((Ast.pkripke_model) option))
# 8842 "parser.ml"
        ) = 
# 63 "parser.mly"
                               (!imported, symbol_tbl, None)
# 8846 "parser.ml"
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
            | Init ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Equal ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | False ->
                        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState246 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | Float _v ->
                        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState246 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | For ->
                        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | Iden _v ->
                        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState246 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | If ->
                        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | Int _v ->
                        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState246 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | LB1 ->
                        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | LB2 ->
                        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | LB3 ->
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | Match ->
                        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | Minus ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | MinusDot ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | Negb ->
                        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | True ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState246 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | UIden _v ->
                        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState246 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | Val ->
                        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | Var ->
                        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | While ->
                        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState246)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce90 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ptyp option) = 
# 100 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( None )
# 8937 "parser.ml"
     in
    _menhir_goto_option_type_of_expr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState5 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB3 ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBool ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | TFloat ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | TInt ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | TUnt ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | UIden _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState5 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run173 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.ppattern_loc) = 
# 470 "parser.mly"
                    (mk_ppat_loc PPat_Underline (PTVar (new_type_var())) _startpos__1_ _endpos__1_)
# 8980 "parser.ml"
     in
    _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run174 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "parser.mly"
       (string)
# 8987 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Float _v ->
        _menhir_run181 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState174 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run180 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState174 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run179 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState174 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run174 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState174 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Underline ->
        _menhir_run173 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Arrow | Colon | ColonColon | Comma | Equal | RB1 | RB2 | Semicolon | Vertical ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _endpos_uid_, _menhir_s, (uid : (
# 35 "parser.mly"
       (string)
# 9013 "parser.ml"
        )), _startpos_uid_) = _menhir_stack in
        let _startpos = _startpos_uid_ in
        let _endpos = _endpos_uid_ in
        let _v : (Ast.ppattern_loc) = 
# 473 "parser.mly"
                  (mk_ppat_loc (PPat_Constr (uid, None)) (PTVar (new_type_var())) _startpos_uid_ _endpos_uid_)
# 9020 "parser.ml"
         in
        _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174

and _menhir_run175 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Float _v ->
        _menhir_run181 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState175 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run180 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState175 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run179 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState175 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIden _v ->
        _menhir_run174 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState175 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Underline ->
        _menhir_run173 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState175 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Vertical ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState175 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Float _v ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Iden _v ->
            _menhir_run180 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Int _v ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB1 ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LB2 ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIden _v ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Underline ->
            _menhir_run173 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Vertical ->
            _menhir_reduce107 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState176)
    | RB2 ->
        _menhir_reduce107 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState175

and _menhir_run177 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Float _v ->
        _menhir_run181 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState177 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Iden _v ->
        _menhir_run180 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState177 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Int _v ->
        _menhir_run179 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState177 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB1 ->
        _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LB2 ->
        _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RB1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_s = MenhirState177 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__2_ = _endpos in
        let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (Ast.ppattern_loc) = 
# 458 "parser.mly"
                (mk_ppat_loc (PPat_Unt) PTUnt _startpos__1_ _endpos__2_)
# 9113 "parser.ml"
         in
        _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | UIden _v ->
        _menhir_run174 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState177 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Underline ->
        _menhir_run173 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState177 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState177

and _menhir_run179 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 33 "parser.mly"
       (int)
# 9128 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_i_ = _endpos in
    let (i : (
# 33 "parser.mly"
       (int)
# 9137 "parser.ml"
    )) = _v in
    let _startpos_i_ = _startpos in
    let _startpos = _startpos_i_ in
    let _endpos = _endpos_i_ in
    let _v : (Ast.ppattern_loc) = 
# 456 "parser.mly"
                (mk_ppat_loc (PPat_Int i) PTInt _startpos_i_ _endpos_i_)
# 9145 "parser.ml"
     in
    _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run180 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "parser.mly"
       (string)
# 9152 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_id_ = _endpos in
    let (id : (
# 35 "parser.mly"
       (string)
# 9161 "parser.ml"
    )) = _v in
    let _startpos_id_ = _startpos in
    let _startpos = _startpos_id_ in
    let _endpos = _endpos_id_ in
    let _v : (Ast.ppattern_loc) = 
# 455 "parser.mly"
                     (mk_ppat_loc (PPat_Symbol id) (PTVar (new_type_var())) _startpos_id_ _endpos_id_)
# 9169 "parser.ml"
     in
    _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run181 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 34 "parser.mly"
       (float)
# 9176 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_f_ = _endpos in
    let (f : (
# 34 "parser.mly"
       (float)
# 9185 "parser.ml"
    )) = _v in
    let _startpos_f_ = _startpos in
    let _startpos = _startpos_f_ in
    let _endpos = _endpos_f_ in
    let _v : (Ast.ppattern_loc) = 
# 457 "parser.mly"
                (mk_ppat_loc (PPat_Float f) PTFloat _startpos_f_ _endpos_f_)
# 9193 "parser.ml"
     in
    _menhir_goto_pattern _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState328 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState321 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState317 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState315 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState311 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState307 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState303 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState301 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState297 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState295 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState293 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState291 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState287 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s, _), _), _, _, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState281 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState276 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState272 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s, _), _), _, _, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState266 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState261 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState259 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState258 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState256 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState253 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState251 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState250 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState249 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState246 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState241 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState240 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState237 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState236 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState233 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState231 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState229 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState228 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState224 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState223 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState221 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState219 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState216 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState215 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState214 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState213 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState212 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState211 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState210 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState209 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState208 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState207 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState203 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState202 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState200 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState196 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState195 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState191 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState187 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState182 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState175 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState159 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState134 ->
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
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce74 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (string list) = 
# 185 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 9838 "parser.ml"
     in
    _menhir_goto_list_Iden_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run237 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "parser.mly"
       (string)
# 9845 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Iden _v ->
        _menhir_run237 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState237 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Equal ->
        _menhir_reduce74 _menhir_env (Obj.magic _menhir_stack) MenhirState237
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState237

and _menhir_goto_imported : _menhir_env -> 'ttv_tail -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Datatype ->
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
            | Iden _v ->
                _menhir_run237 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState236 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Equal ->
                _menhir_reduce74 _menhir_env (Obj.magic _menhir_stack) MenhirState236
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState236)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | Function ->
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
            | Float _v ->
                _menhir_run181 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState228 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Iden _v ->
                _menhir_run180 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState228 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Int _v ->
                _menhir_run179 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState228 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB1 ->
                _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState228 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LB2 ->
                _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState228 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIden _v ->
                _menhir_run174 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState228 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Underline ->
                _menhir_run173 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState228 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState228)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | Import ->
        let _menhir_stack = Obj.magic _menhir_stack in
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
# 9945 "parser.ml"
            )) = _v in
            let _startpos__3_ = _startpos in
            let (_menhir_stack, (_1 : (unit))) = _menhir_stack in
            let _2 = () in
            let _v : (unit) = 
# 68 "parser.mly"
                            (imported := _3 :: !imported)
# 9953 "parser.ml"
             in
            _menhir_goto_imported _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | Val ->
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
            | Colon ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState221
            | Equal ->
                _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack) MenhirState221
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState221)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | Var ->
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
            | Colon ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | Equal ->
                _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | EOF | Model ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (unit) = 
# 71 "parser.mly"
            ()
# 10022 "parser.ml"
         in
        _menhir_goto_declars _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

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

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 41 "parser.mly"
       ((string list) * (Ast.psymbol_tbl) * ((Ast.pkripke_model) option))
# 10046 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (unit) = 
# 67 "parser.mly"
            ()
# 10062 "parser.ml"
     in
    _menhir_goto_imported _menhir_env _menhir_stack _v)

# 500 "parser.mly"
  
# 10068 "parser.ml"

# 219 "/Users/liujian/.opam/system/lib/menhir/standard.mly"
  


# 10074 "parser.ml"
