all: message.mli collector.ml worker.ml master.ml
	ocamlfind ocamlopt -o main -package netmulticore,netcamlbox -linkpkg -g message.mli worker.ml collector.ml master.ml

clean:
	rm -f *.cmi
	rm -f *.cmo
	rm -f main
	rm -f *.o
	rm -f *.cmx
	rm -r -f tmp
