bison -v -d -o sctlparser.c sctlparser.y
flex sctllexer.l
gcc -std=c11 term.h term.c formula.h formula.c modul.h modul.c parser_helper.h sctlparser.h sctlparser.c sctllexer.c main.c -o sctl -g