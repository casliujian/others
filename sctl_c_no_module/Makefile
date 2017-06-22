all: term.h term.c formula.h formula.c modul.h modul.c parser_helper.h sctllexer.l sctlparser.y main.c
	bison -v -d -o sctlparser.c sctlparser.y
	flex sctllexer.l
	gcc term.h term.c formula.h formula.c modul.h modul.c parser_helper.h sctlparser.h sctlparser.c sctllexer.c state_set.h state_set.c merge.h merge.c prover.h prover.c main.c -o sctl -g
	
	
clean:
	rm -f *.h.gch
	rm -f sctllexer.c
	rm -f sctlparser.h
	rm -f sctllexer.h
	rm -f sctlparser.c
	rm -f *.o
	rm -f sctl test *.exe
	rm -f *.model[01234]
	rm -f *.bak *.stackdump
