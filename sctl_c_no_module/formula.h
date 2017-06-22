#ifndef FORMULA_H_
#define FORMULA_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "term.h"

typedef enum formula_type
{
    top, bottom, atom, neg, and_fml, or_fml, ax, ex, af, eg, ar, eu
} Formula_type;


typedef struct formula 
{
    Formula_type type;
    char * str1;
    char * str2;
    struct formula * sub_fml1;
    struct formula * sub_fml2;
    State_list* states; 
} Formula;

Formula* mk_fml_top();
Formula* mk_fml_bottom();
Formula* mk_fml_atom(char* iden, State_list* sl);
Formula* mk_fml_neg(Formula* fml1);
Formula* mk_fml_and(Formula* fml1, Formula* fml2);
Formula* mk_fml_or(Formula* fml1, Formula* fml2);
Formula* mk_fml_ax(char* quan, Formula* fml1, State* s);
Formula* mk_fml_ex(char* quan, Formula* fml1, State* s);
Formula* mk_fml_af(char* quan, Formula* fml1, State* s);
Formula* mk_fml_eg(char* quan, Formula* fml1, State* s);
Formula* mk_fml_ar(char* quan1, char* quan2, Formula* fml1, Formula* fml2, State* s);
Formula* mk_fml_eu(char* quan1, char* quan2, Formula* fml1, Formula* fml2, State* s);

char* str_fml(Formula* fml);
Formula* replicate_fml(Formula* fml);
Formula* fml_replace_state_var(Formula* fml, char* name, int* state);
void clean_fml(Formula* fml);

#endif

