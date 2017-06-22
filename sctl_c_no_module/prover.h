#ifndef PROVER_H_
#define PROVER_H_
#include "term.h"
#include "formula.h"
#include "modul.h"
#include "state_set.h"
#include "merge.h"

#define TRUE 	1
#define FALSE 	0

typedef enum conttype
{
	basic_true, basic_false, regular
} Contype;

typedef struct continuation
{
	Contype type;
	int ref_count;
	State_set* states;
	Formula* formula;
	char* levl;
	struct continuation* contl;
	struct continuation* contr;
} Continuation;

typedef struct int_array_list 
{
	int* head;
	struct int_array_list* tail;
} Int_array_list;

Int_array_list* next(Modul* modul, int* state, int length);

void clean_continuation(Continuation* cont);
void incr_ref_count(Continuation* cont);
void decr_ref_count(Continuation* cont);

Continuation* mk_cont_ax(State_set* gamma, char* x, Formula* fml, char* levl, Int_array_list* ial, Continuation* contl, Continuation* contr);
Continuation* mk_cont_ex(State_set* gamma, char* x, Formula* fml, char* levl, Int_array_list* ial, Continuation* contl, Continuation* contr);
Continuation* mk_cont_af(State_set* gamma, char* x, Formula* fml, char* levl, int* ori_state, int length, Int_array_list* ial, Continuation* contl, Continuation* contr);
Continuation* mk_cont_eg(State_set* gamma, char* x, Formula* fml, char* levl, int* ori_state, int length, Int_array_list* ial, Continuation* contl, Continuation* contr);

int prove(Continuation* cont, Modul* modul);
void prove_model(Modul* modul);

#endif