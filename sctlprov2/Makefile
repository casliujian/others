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

lib:
	make -C utils all

clean:
	make -C utils clean
	rm -f lexer.ml
	rm -f parser.mli
	rm -f parser.ml
	rm -f *.cm[ioxa]