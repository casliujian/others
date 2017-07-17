open Ast
open Typechecker
open Printf
open Lexing

let moduls = Hashtbl.create 0

let debug () = 
    let exiting = ref true in
    let rec loop () = 
        while !exiting do
            let str = read_line () in
            printf "parsing: %s\n" str;
            if str <> "exit" then 
                let lbuf = Lexing.from_string str in
                try 
                    Parser.debug Lexer.token lbuf    
                with _ -> 
                    let ep = lbuf.lex_curr_p in
                    printf "syntax error at line %d, column %d\n" ep.pos_lnum (ep.pos_cnum - ep.pos_bol)
            else 
                exiting := false
        done in
    loop ()

let test () = 
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
        printf "syntax error at line %d, column %d\n" ep.pos_lnum (ep.pos_cnum - ep.pos_bol)


let _ = 
    let flag = ref 0 in
    Arg.parse [
        "-test", Arg.Unit (fun () -> flag := 0), "\tparse test.model";
        "-debug", Arg.Unit (fun () -> flag := 1), "\tdebug the parser interactively";
    ] (fun s -> print_endline ("unknown option: "^s)) "";
    match !flag with
    | 0 -> test ()
    | 1 -> debug ()
    | _ -> print_endline "don't know what to do."

    