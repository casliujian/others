#include <stdio.h>
#include <string.h>
#include "parser_helper.h"
#include "modul.h"
#include "sctlparser.h"
#include "prover.h"

int main()
{
    printf("%s\n", "helloworld");
//    struct modul0* main_modul = (struct modul0*)malloc(sizeof(struct modul0));
//    struct modul0_list* sub_moduls = (struct modul0_list*)malloc(sizeof(sizeof(struct modul0_list)));
    extern FILE* yyin;
    yyin = fopen("river.model","r");
//    struct modul0 main_modul;
//    struct modul0_list sub_moduls;
	struct modul_defs moduls;
    int i = yyparse(&moduls);
    printf("model name: %s, model size: %d\n", moduls.main_modul->name, moduls.main_modul->size);
    FILE* output0 = fopen("river.model0", "w");
    FILE* output1 = fopen("river.model1", "w");
    FILE* output2 = fopen("river.model2", "w");
    FILE* output3 = fopen("river.model3", "w");
    FILE* output4 = fopen("river.model4", "w");
    if (output0 == NULL)
    {
    	printf("cannot open file river.model0\n");
    	return 1;
    }
    Modul0_list* sub_moduls = moduls.sub_moduls;
    while(NULL != sub_moduls)
    {
    	print_modul0(output0, &(sub_moduls->head), 0);
    	sub_moduls = sub_moduls->tail;
    }
    print_modul0(output0, (moduls.main_modul), 1);
    fclose(output0);
    Modul1* modul1 = modul021(moduls.main_modul);
    print_modul1(output1, modul1, 1);
    fclose(output1);
    Modul2* modul2 = modul122(modul1, NULL, NULL);
    print_modul2(output2, modul2, 1);
    fclose(output2);
    Modul3* modul3 = modul223(modul2);
    print_modul3(output3, modul3, 1);
    fclose(output3);
    Modul4* modul4 = modul324(modul3);
    print_modul4(output4, modul4, 1);
    fclose(output4);
    prove_model(modul4);
    return i;
}
