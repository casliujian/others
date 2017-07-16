all:
	ocamlc -c options.ml
	ocamlc -c pairs.ml
	ocamlc -a -o utils.cma options.cmo pairs.cmo

clean:
	rm -f *.cm[ioxa]
	rm -f utils.cma