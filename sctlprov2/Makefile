all:
	make -C utils all
	ocamlc -c ast.ml
	menhir --infer --explain parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -I utils -c utils.cma typechecker.ml
	ocamlc -c main.ml
	ocamlc -I utils -o sctlprov2 utils.cma ast.cmo parser.cmo lexer.cmo typechecker.cmo main.cmo

all_yacc:
	make -C utils all
	ocamlc -c ast.ml
	ocamlyacc parser_yacc.mly
	ocamlc -c parser_yacc.mli
	ocamlc -c parser_yacc.ml
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -I utils -c utils.cma typechecker.ml
	ocamlc -c main.ml
	ocamlc -I utils -o sctlprov2 utils.cma ast.cmo parser_yacc.cmo lexer.cmo typechecker.cmo main.cmo

lib:
	make -C utils all

clean:
	make -C utils clean
	rm -f lexer.ml
	rm -f parser.mli
	rm -f parser.ml
	rm -f *.cm[ioxa]