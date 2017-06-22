#ifndef MODUL_H_
#define MODUL_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "term.h"
#include "formula.h"
/***********************************************************************************/
typedef struct string_type_list
{
    char* name;
    Type type;
    struct string_type_list * next;
} String_type_list;

int len_string_type_list(String_type_list* stl);
/***********************************************************************************/
typedef struct string_expr_list
{
    char* name;
    Expr expr;
    struct string_expr_list * next;
} String_expr_list;

typedef struct string_vars_expr_list
{
    char* name;
    String_list* var_list;
    Expr* expr;
    struct string_vars_expr_list* next;

} String_vars_expr_list;

typedef struct expr_pair
{
    Expr first;
    Expr second;
} Expr_pair;

typedef struct expr_pair_list
{
    Expr_pair head;
    struct expr_pair_list * tail;
} Expr_pair_list;

typedef struct transition
{
    Expr guard;
    Expr_pair_list * action;
} Transition;

typedef struct transition_list
{
    Transition head;
    struct transition_list * tail;
} Transition_list;

typedef struct string_fml_list
{
    char* name;
    Formula fml;
    struct string_fml_list * next;
} String_fml_list;

/*
    initial definitions of modules.
*/


typedef struct modul0
{
    char* name;
    int size;
    String_type_list* parameters;
    String_type_list* vars;
    String_expr_list* symbols;
    String_emi_list* init_assigns;
    Transition_list* transitions;
    String_vars_expr_list* atomics;
    String_fml_list* specs;
} Modul0;

typedef struct modul0_list
{
    struct modul0 head;
    struct modul0_list * tail;
} Modul0_list;

Modul0* mk_modul0(char* name, 
                  int size, 
                  String_type_list* parameters, 
                  String_type_list* vars, 
                  String_expr_list* symbols, 
                  String_emi_list* init_assigns, 
                  Transition_list* transitions,
                  String_vars_expr_list* atomics,
                  String_fml_list* specs);

void print_modul0(FILE* out, Modul0* modul, int is_model);

/*
    expand all definitions of user defined symbols.
*/

typedef struct modul1_list
{
    struct modul1 * modul;
    struct modul1_list * next;
} Modul1_list;

typedef struct modul1
{
    char* name;
    int size;
    String_type_list* parameters;
    String_type_list* vars;
    String_emi_list* init_assigns;
    Transition_list* transitions;
    String_vars_expr_list* atomics;
    String_fml_list* specs;
} Modul1;

int is_svar_defined(char* svar, String_type_list* vars);
Expr* expand_symbol(Expr* expr, String_type_list* vars, String_expr_list* symbols);
void print_modul1(FILE* out, Modul1* modul, int is_model);

/*
may cause memory leak, as the symbols definition in modul0 is lost in modul1.
*/
Modul1* modul021(Modul0* modul0);

/*
    apply arguments to parameteric modules.
*/

typedef struct string_modul2_list 
{
    char* name;
    struct modul2 * modul;
    struct string_modul2_list * next;
} String_modul2_list;

typedef struct modul2
{
    char* name;
    int size;
    String_type_list* vars;
    String_emi_list* init_assigns;
    Transition_list* transitions;
    String_vars_expr_list* atomics;
    String_fml_list* specs;
    // String_modul2_list* moduls;
} Modul2;

Expr* find_expr(char* name, String_expr_list* sel);
Expr* replace_expr(Expr* expr, String_expr_list* sel);
Type* replace_expr_in_type(Type *t, String_expr_list* sel);
Modul2* modul122(Modul1* modul1, String_expr_list* args, Modul1_list* moduls);
void print_modul2(FILE* out, Modul2* modul, int is_model);

/*
    expand array definitions.
*/

typedef struct string_modul3_list 
{
    char* name;
    struct modul3 * modul;
    struct string_modul3_list * next;
} String_modul3_list;

typedef struct modul3
{
    char* name;
    int size;
    String_type_list* vars;
    String_expr_list* init_assigns;
    Transition_list* transitions;
    String_vars_expr_list* atomics;
    String_fml_list* specs;
    // String_modul3_list* moduls;
} Modul3;

Expr* expand_array(Expr* expr);
void expand_array_type(Type* t);
Modul3* modul223(Modul2* modul2);
void print_modul3(FILE* out, Modul3* modul, int is_model);

/*
    convert separate definitions of modules into one single definition.
    ---not needed anymore in this version---

typedef struct string_int_list
{
    char* name;
    int index;
    struct string_int_list * next;
} String_int_list;

typedef struct modul4
{
    char* name;
    int size;
    String_type_list* vars;
    String_int_list* var_index;
    Expr_list* init_assigns;
    Transition_list* transitions;
    String_vars_expr_list* atomics;
    String_fml_list* specs;
} Modul4;
*/

/*
    convert variable names into integer representations.
*/



typedef struct string_int_list
{
    char* name;
    int index;
    struct string_int_list * next;
} String_int_list;

typedef struct modul4
{
    char* name;
    int size;
    String_type_list* vars;
    String_int_list* var_index;
    int* init_assigns;
    Transition_list* transitions;
    String_vars_expr_list* atomics;
    String_fml_list* specs;
} Modul4;

typedef Modul4 Modul;

int find_index(char* name, String_int_list* sil);
Expr* recode_var(Expr* expr, String_int_list* var_index);
Expr* recode_state_var(Expr* expr, String_list* var_index);
Modul4* modul324(Modul3* modul3);
void print_modul4(FILE* out, Modul4* modul, int is_model);

#endif
