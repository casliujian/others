#ifndef TERM_H_
#define TERM_H_

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
/*************************************************************************************************************************************/
typedef enum expr_type
{
	symbol, nested_var, var_index, svar, scalar, state_expr, cnst, aray, negi, add, minus, mult, ando, oro, negb, equl, mod, lt, gt, le, ge
} Expr_type;

typedef union value {
	int 	int_value;
	char *	string_value; 
} Value;

typedef struct expr
{
	Expr_type 	type;
	Value 		value;
	struct expr_list *	sub_exprs;	
} Expr;

typedef struct expr_list
{
	struct expr* head;
	struct expr_list * 	tail;
} Expr_list;

typedef enum type_type
{
	int_type, bool_type, scalar_type, array_type, module_type
} Type_type;

typedef struct string_list
{
	char* head;
	struct string_list * tail;
} String_list;

typedef struct type
{
	Type_type type;
	union {
		struct {Expr* low; Expr* high;} int_range;
		String_list scalar_list;
		struct {struct type * element_type; Expr* index;} array_rep;
		char* modul_name;
	};
} Type;

typedef enum state_type
{
	ident, real
} State_type; 

typedef union state_content
{
	char* ident;
	int* real;
} State_content;

typedef struct state
{
	State_type type;
	State_content content;
} State;

typedef struct state_list 
{
	State* head;
	struct state_list * tail;
} State_list;

typedef struct expr_modul_instance
{
	enum {expr, modul_instance} type;
	union {
		Expr* expr;
		struct {char* modul_name; Expr_list* args;} modul;
	};
} Expr_modul_instance;

typedef struct expr_modul_instance_list
{
	Expr_modul_instance head;
	struct expr_modul_instance_list * tail;
} Expr_modul_instance_list;

typedef struct string_emi_list
{
	char* name;
	Expr_modul_instance* emi;
	struct string_emi_list* next;
} String_emi_list;


int len_expr_list(Expr_list* el);
/*
	make new expressions here.
*/
Expr* mk_expr_symbol(char* sym);
Expr* mk_expr_nested_var(Expr* e1, Expr* e2);
Expr* mk_expr_var_index(char* v, Expr* e);
// Expr* mk_expr_var(int v);
Expr* mk_expr_svar(char* sv);
Expr* mk_expr_scalar(char* sc);
Expr* mk_expr_state_expr(char* state, Expr* e);
Expr* mk_expr_cnst(int value);
Expr* mk_expr_aray(Expr_list* el);
Expr* mk_expr_negi(Expr* e);
Expr* mk_expr_add(Expr* e1, Expr* e2);
Expr* mk_expr_minus(Expr* e1, Expr* e2);
Expr* mk_expr_mult(Expr* e1, Expr* e2);
Expr* mk_expr_ando(Expr* e1, Expr* e2);
Expr* mk_expr_oro(Expr* e1, Expr* e2);
Expr* mk_expr_negb(Expr* e);
Expr* mk_expr_equl(Expr* e1, Expr* e2);
Expr* mk_expr_mod(Expr* e1, Expr* e2);
Expr* mk_expr_lt(Expr* e1, Expr* e2);
Expr* mk_expr_gt(Expr* e1, Expr* e2);
Expr* mk_expr_le(Expr* e1, Expr* e2);
Expr* mk_expr_ge(Expr* e1, Expr* e2);
Expr* replicate_expr(Expr* e);

char* str_expr(Expr* e);
char* str_expr_list(Expr_list* el);
Expr_list* eval_expr_list(Expr_list* el);
Expr* eval_expr(Expr* e);
Expr* eval_with_state(Expr* e, State* state);
Expr* eval_with_state_list(Expr* e, State_list* sl);

void clean_expr(Expr* e);

/***********************************************************************************************************************************/


int len_string_list(String_list* sl);
char* str_string_list(String_list* sl);
int position_in_string_list(char* str, String_list* sl);



Type* mk_type_int(Expr* low, Expr* high);
Type* mk_type_bool();
Type* mk_type_scalar(String_list scalars);
Type* mk_type_array(Type* element_type, Expr* index);
Type* mk_type_modul(char* name);

char* str_type(Type* t);


/*********************************************************************************************************/


int len_state_list(State_list* sl);
State* find_ith_state(State_list* sl, int i);

State* mk_state_iden(char* id);
State* mk_state_real(int* r);
State* replicate_state(State* s);
void clean_state(State* s);

char* str_state(State* s);
char* str_state_list(State_list* sl);

/***********************************************************************************************************/


Expr_modul_instance* mk_emi_expr(Expr* e);
Expr_modul_instance* mk_emi_modul_instance(char* name, Expr_list* args);
char* str_emi(Expr_modul_instance* emi);
/***********************************************************************************************************/

#endif