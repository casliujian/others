open Ast
open Typechecker
open Printf
open Lexing

let moduls = Hashtbl.create 0

let _ = 
    let cha = open_in "test.model" in
    let lbuf = Lexing.from_channel cha in
    try 
        let imported, psymbol_tbl, pkripke_model = Parser.program Lexer.token (lbuf) in
        let modul = {
            fname = "test.model";
            imported = imported;
            psymbol_tbl = psymbol_tbl;
            pkripke_model = pkripke_model;
        } in
        Hashtbl.add moduls "Test" modul;
        print_endline "main.ml"
    with _ -> 
        let ep = lbuf.lex_curr_p in
        printf "syntax error at line %d, column %d\n" ep.pos_lnum ep.pos_bol

    