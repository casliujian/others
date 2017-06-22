#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "prover.h"
#include "state_set.h"

Int_array_list* next(Modul* modul, int* state, int length)
{
	Int_array_list* ial = NULL;
	Transition_list* trans = modul->transitions;
	State* real_state = mk_state_real(state);
	while(NULL != trans) {
		Transition trans_head = trans->head;
		Expr* guard = eval_with_state(&(trans_head.guard), real_state);
		if(cnst == guard->type) {
			if(1 == guard->value.int_value) {
				int* new_state = (int*)malloc(sizeof(int)*length);
				for (int i = 0; i < length; ++i) {
					new_state[i] = state[i];
				}
				Expr_pair_list* actions = trans_head.action;
				while(NULL != actions) {
					Expr* var = eval_expr(&(actions->head.first));
					Expr* value = eval_with_state(&(actions->head.second), real_state);
					if(svar == var->type && cnst == value->type) {
						int position = atoi(var->value.string_value);
						new_state[position] = value->value.int_value;
						clean_expr(value);
					} else {
						printf("error evaluating action: %s := %s \n", str_expr(var), str_expr(value));
						exit(1);
					}
					actions = actions->tail;
				}
				Int_array_list* tmp_ial = (Int_array_list*)malloc(sizeof(Int_array_list));
				tmp_ial->head = new_state;
				tmp_ial->tail = ial;
				ial = tmp_ial;
			}
		} else {
			printf("error evaluating guard: %s, not a cnst expression\n", str_expr(guard));
			clean_expr(guard);
			exit(1);
		}
		clean_expr(guard);
		trans = trans->tail;
	}
	free(real_state);
	if (NULL == ial) {
		int* new_state = (int*)malloc(sizeof(int)*length);
		for (int i = 0; i < length; ++i) {
			new_state[i] = state[i];
		}
		Int_array_list* tmp_ial = (Int_array_list*)malloc(sizeof(Int_array_list));
		tmp_ial->head = new_state;
		tmp_ial->tail = ial;
		ial = tmp_ial;
	}
	return ial;
}


void clean_continuation(Continuation* cont) 
{
	if(NULL != cont) {
		if(cont->ref_count > 1) {
			cont->ref_count = cont->ref_count - 1;
		} else {
			if(regular == cont->type) {
				printf("Cleaning regular Continuation\n");
			} else if(basic_true == cont->type) {
				printf("Cleaning basic_true Continuation\n");
			} else {
				printf("Cleaning basic_false Continuation\n");
			}
			clear_state_set(cont->states);
			clean_fml(cont->formula);
			if(NULL != cont->levl) {
				free(cont->levl);
			}
			// free(cont->levl);
			clean_continuation(cont->contl);
			clean_continuation(cont->contr);
			
			if(regular == cont->type) {
				printf("Cleaned regular Continuation %d\n", cont->type);
			} else if(basic_true == cont->type) {
				printf("Cleaned basic_true Continuation %d\n", cont->type);
			} else {
				printf("Cleaned basic_false Continuation %d\n", cont->type);
			}
			free(cont);
		}
	}
}

void incr_ref_count(Continuation* cont) 
{
	if(NULL != cont) {
		cont->ref_count = cont->ref_count + 1;
	}
}

void decr_ref_count(Continuation* cont) 
{
	if(NULL != cont) {
		cont->ref_count = cont->ref_count - 1;
	}
}

Continuation* mk_cont_ax(State_set* gamma, char* x, Formula* fml, char* levl, Int_array_list* ial, Continuation* contl, Continuation* contr) 
{
	Continuation* ax_cont = contl;
	incr_ref_count(contl);
	while(NULL != ial) {
		int* state = ial->head;
		Formula* new_fml = replicate_fml(fml);
		new_fml = fml_replace_state_var(new_fml, x, state);
		Continuation* new_continuation = (Continuation*)malloc(sizeof(Continuation));
		new_continuation->type = regular;
		new_continuation->ref_count = 1;
		new_continuation->states = NULL;
		new_continuation->formula = new_fml;
		char* buffer = (char*)malloc(sizeof(char)*(strlen(levl)+1));
		sprintf(buffer, "%s%d", levl, 1);
		new_continuation->levl = buffer;
		new_continuation->contl = ax_cont;
		incr_ref_count(contr);
		new_continuation->contr = contr;
		ax_cont = new_continuation;
		ial = ial->tail;
	}
	return ax_cont;
}

Continuation* mk_cont_ex(State_set* gamma, char* x, Formula* fml, char* levl, Int_array_list* ial, Continuation* contl, Continuation* contr) 
{
	Continuation* ex_cont = contr;
	incr_ref_count(contr);
	while(NULL != ial) {
		int* state = ial->head;
		Formula* new_fml = replicate_fml(fml);
		new_fml = fml_replace_state_var(new_fml, x, state);
		Continuation* new_continuation = (Continuation*)malloc(sizeof(Continuation));
		new_continuation->type = regular;
		new_continuation->ref_count = 1;
		new_continuation->states = NULL;
		new_continuation->formula = new_fml;
		char* buffer = (char*)malloc(sizeof(char)*(strlen(levl)+1));
		sprintf(buffer, "%s%d", levl, 1);
		new_continuation->levl = buffer;
		incr_ref_count(contl);
		new_continuation->contl = contl;
		new_continuation->contr = ex_cont;
		ex_cont = new_continuation;
		ial = ial->tail;
	}
	return ex_cont;
}

Continuation* mk_cont_af(State_set* gamma, char* x, Formula* fml, char* levl, int* ori_state, int length, Int_array_list* ial, Continuation* contl, Continuation* contr)
{
	int* mem_state = NULL;

	Continuation* af_cont = contl;
	incr_ref_count(contl);
	while(NULL != ial) {
		int* state = ial->head;
		Formula* new_fml = replicate_fml(fml);
		new_fml->states->head->type = real;
		new_fml->states->head->content.real = state;
		// new_fml = fml_replace_state_var(new_fml, x, state);
		Continuation* new_continuation = (Continuation*)malloc(sizeof(Continuation));
		new_continuation->type = regular;
		new_continuation->ref_count = 1;
		State_set* ss = replicate_state_set(gamma, length);
		int* new_state = (int*)malloc(sizeof(int)*length);
		memcpy(new_state, ori_state, sizeof(int)*length);
		mem_state = new_state;
		ss = insert(ss, new_state, length);
		new_continuation->states = ss;
		new_continuation->formula = new_fml;
		// char* buffer = (char*)malloc(sizeof(char)*(strlen(levl)+1));
		// sprintf(buffer, "%s%d", levl, 1);
		new_continuation->levl = levl;
		new_continuation->contl = af_cont;
		incr_ref_count(contr);
		new_continuation->contr = contr;
		af_cont = new_continuation;
		ial = ial->tail;
	}
	Continuation* new_continuation = (Continuation*)malloc(sizeof(Continuation));
	new_continuation->type = regular;
	new_continuation->ref_count = 1;
	new_continuation->formula = fml_replace_state_var(replicate_fml(fml->sub_fml1), x, mem_state == NULL? ori_state : mem_state); 
	if(mem_state != NULL) {
		free(ori_state);
	}
	// new_continuation->formula = fml_replace_state_var(replicate_fml(fml->sub_fml1), x, ori_state); 
	char* buffer = (char*)malloc(sizeof(char)*(strlen(levl)+1));
	sprintf(buffer, "%s%d", levl, 1);
	new_continuation->levl = buffer;
	new_continuation->contl = contl;
	incr_ref_count(contl);
	new_continuation->contr = af_cont;
	return new_continuation;
}

Continuation* mk_cont_eg(State_set* gamma, char* x, Formula* fml, char* levl, int* ori_state, int length, Int_array_list* ial, Continuation* contl, Continuation* contr)
{
	int* mem_state = NULL;

	Continuation* eg_cont = contr;
	incr_ref_count(contr);
	while(NULL != ial) {
		int* state = ial->head;
		Formula* new_fml = replicate_fml(fml);
		new_fml->states->head->type = real;
		new_fml->states->head->content.real = state;
		// new_fml = fml_replace_state_var(new_fml, x, state);
		Continuation* new_continuation = (Continuation*)malloc(sizeof(Continuation));
		new_continuation->type = regular;
		new_continuation->ref_count = 1;
		State_set* ss = replicate_state_set(gamma, length);
		int* new_state = (int*)malloc(sizeof(int)*length);
		memcpy(new_state, ori_state, sizeof(int)*length);
		mem_state = new_state;
		ss = insert(ss, new_state, length);
		new_continuation->states = ss;
		new_continuation->formula = new_fml;
		// char* buffer = (char*)malloc(sizeof(char)*(strlen(levl)+1));
		// sprintf(buffer, "%s%d", levl, 1);
		new_continuation->levl = levl;
		new_continuation->contl = contl;
		incr_ref_count(contl);
		new_continuation->contr = eg_cont;
		eg_cont = new_continuation;
		ial = ial->tail;
	}
	Continuation* new_continuation = (Continuation*)malloc(sizeof(Continuation));
	new_continuation->type = regular;
	new_continuation->ref_count = 1;
	new_continuation->formula = fml_replace_state_var(replicate_fml(fml->sub_fml1), x, mem_state == NULL? ori_state : mem_state); 
	if(mem_state != NULL) {
		free(ori_state);
	}
	// new_continuation->formula = fml_replace_state_var(replicate_fml(fml->sub_fml1), x, ori_state); 
	char* buffer = (char*)malloc(sizeof(char)*(strlen(levl)+1));
	sprintf(buffer, "%s%d", levl, 1);
	new_continuation->levl = buffer;
	new_continuation->contl = eg_cont;
	new_continuation->contr = contr;
	incr_ref_count(contr);
	return new_continuation;
}



int prove(Continuation* cont, Modul* modul)
{
	Merge* global_merge = NULL;
	Continuation* current_cont = cont;
	while(1) {
		switch(current_cont->type) {
			case basic_true:
				return TRUE;
			case basic_false:
				return FALSE;
			default: {
				Formula* fml = current_cont->formula;
				// printf("Proving formula: %s\n", str_fml(fml));
				switch(fml->type) {
					case top: {
						Continuation* ccontl = current_cont->contl;
						incr_ref_count(ccontl);
						clean_continuation(current_cont);
						current_cont = ccontl;
						break;
					}
					case bottom: {
						// Continuation* freed = current_cont;
						Continuation* ccontr = current_cont->contr;
						incr_ref_count(ccontr);
						clean_continuation(current_cont);
						current_cont = ccontr;
						// free(freed);
						break;
					}
					case atom: {
						printf("Proving atomic formula: %s in levl: %s\n", str_fml(fml), current_cont->levl);
						String_vars_expr_list* atomics = modul->atomics;
						while(NULL != atomics) {
							if(0 == strcmp(fml->str1, atomics->name)) {
								Expr* e = eval_with_state_list(atomics->expr, fml->states);
								if(cnst == e->type && 1 == e->value.int_value) {
									// Continuation* freed = current_cont;
									Continuation* ccontl = current_cont->contl;
									incr_ref_count(ccontl);
									State_list* sl = fml->states;
									// while(NULL != sl) {
									// 	free(sl->head->content.real);
									// 	sl = sl->tail;
									// }
									clean_continuation(current_cont);
									current_cont = ccontl;
									// free(freed);
								} else if(cnst == e->type && 0 == e->value.int_value) {
									Continuation* ccontr = current_cont->contr;
									incr_ref_count(ccontr);
									State_list* sl = fml->states;
									// while(NULL != sl) {
									// 	free(sl->head->content.real);
									// 	sl = sl->tail;
									// }
									clean_continuation(current_cont);
									current_cont = ccontr;
								} else {
									printf("error proving atomic formula: %s\n", fml->str1);
									clean_continuation(current_cont);
									exit(1);
								}
								break;
							}
							atomics = atomics->next;
						}
						break;
					}
					case neg: {
						Continuation* tmp_cont = current_cont->contl;
						current_cont->formula = current_cont->formula->sub_fml1;
						current_cont->contl = current_cont->contr;
						current_cont->contr = tmp_cont;
						break;
					}
					case and_fml: {
						// printf("Proving and formula's sub_fml1 and sub_fml2: %s, %s\n", str_fml(fml->sub_fml1), str_fml(fml->sub_fml2));
						Continuation* cont1 = (Continuation*)malloc(sizeof(Continuation));
						cont1->type = regular;
						cont1->ref_count = 1;
						cont1->states = NULL;
						cont1->formula = fml->sub_fml1;
						char* buffer = (char*)malloc(sizeof(char)*(strlen(current_cont->levl)+1));
						sprintf(buffer, "%s%d", current_cont->levl, 1);
						cont1->levl = buffer;
						// cont1->contl = current_cont;
						cont1->contr = current_cont->contr;
						incr_ref_count(current_cont->contr);

						Continuation* cont2 = (Continuation*)malloc(sizeof(Continuation));
						cont2->type = regular;
						cont2->ref_count = 1;
						cont2->states = NULL;
						cont2->formula = fml->sub_fml2;
						char* buffer2 = (char*)malloc(sizeof(char)*(strlen(current_cont->levl)+1));
						sprintf(buffer2, "%s%d", current_cont->levl, 2);
						cont2->levl = buffer2;
						cont2->contl = current_cont->contl;
						incr_ref_count(current_cont->contl);
						cont2->contr = current_cont->contr;
						incr_ref_count(current_cont->contr);
						cont1->contl = cont2;
						clean_continuation(current_cont);
						current_cont = cont1;
						break;
					}
					case or_fml: {
						Continuation* cont1 = (Continuation*)malloc(sizeof(Continuation));
						cont1->type = regular;
						cont1->ref_count = 1;
						cont1->states = current_cont->states;
						cont1->formula = fml->sub_fml1;
						char* buffer = (char*)malloc(sizeof(char)*(strlen(current_cont->levl)+1));
						sprintf(buffer, "%s%d", current_cont->levl, 1);
						cont1->levl = buffer;
						cont1->contl = current_cont->contl;
						cont1->contr = current_cont;
						current_cont->formula = fml->sub_fml2;
						char* freed = current_cont->levl;
						char* buffer2 = (char*)malloc(sizeof(char)*(strlen(current_cont->levl)+1));
						sprintf(buffer2, "%s%d", current_cont->levl, 2);
						current_cont->levl = buffer2;
						free(freed);
						current_cont = cont1;
						break;
					}
					case ax: {
						Int_array_list* ial = next(modul, fml->states->head->content.real, modul->size);
						Continuation* new_continuation = mk_cont_ax(NULL, fml->str1, fml->sub_fml1, current_cont->levl, ial, current_cont->contl, current_cont->contr);
						while(NULL != ial) {
							Int_array_list* tail = ial->tail;
							free(ial);
							ial = tail;
						}
						// decr_ref_count(current_cont->contl);
						// decr_ref_count(current_cont->contr);
						// clean_fml(fml);
						// clean_state_set(current_cont->states);
						// free(current_cont->levl);
						// free(current_cont);
						clean_continuation(current_cont);
						current_cont = new_continuation;
						break;
					}
					case ex: {
						Int_array_list* ial = next(modul, fml->states->head->content.real, modul->size);
						Continuation* new_continuation = mk_cont_ex(NULL, fml->str1, fml->sub_fml1, current_cont->levl, ial, current_cont->contl, current_cont->contr);
						while(NULL != ial) {
							Int_array_list* tail = ial->tail;
							free(ial);
							ial = tail;
						}
						// clean_fml(fml);
						// clean_state_set(current_cont->states);
						// free(current_cont->levl);
						// free(current_cont);
						clean_continuation(current_cont);
						current_cont = new_continuation;
						break;
					}
					case af: {
						if(is_in_state_set(current_cont->states, fml->states->head->content.real, modul->size)) {
							// free(fml->states->head->content.real);
							Continuation* ccontr = current_cont->contr;
							incr_ref_count(ccontr);
							clean_continuation(current_cont);
							current_cont = ccontr;
						} else {
							Int_array_list* ial = next(modul, fml->states->head->content.real, modul->size);
							Continuation* new_continuation = mk_cont_af(current_cont->states, fml->str1, fml, current_cont->levl, fml->states->head->content.real, modul->size, ial, current_cont->contl, current_cont->contr);
							while(NULL != ial) {
								Int_array_list* tail = ial->tail;
								free(ial);
								ial = tail;
							}
							// clean_fml(fml);
							// clean_state_set(current_cont->states);
							// free(current_cont->levl);
							// free(current_cont);
							clean_continuation(current_cont);
							current_cont = new_continuation;
						}
						break;
					}
					case eg: {
						if(is_in_state_set(current_cont->states, fml->states->head->content.real, modul->size)) {
							// free(fml->states->head->content.real);
							Continuation* ccontl = current_cont->contl;
							incr_ref_count(ccontl);
							clean_continuation(current_cont);
							current_cont = ccontl;
						} else {
							Int_array_list* ial = next(modul, fml->states->head->content.real, modul->size);
							Continuation* new_continuation = mk_cont_eg(current_cont->states, fml->str1, fml, current_cont->levl, fml->states->head->content.real, modul->size, ial, current_cont->contl, current_cont->contr);
							while(NULL != ial) {
								Int_array_list* tail = ial->tail;
								free(ial);
								ial = tail;
							}
							// clean_fml(fml);
							// clean_state_set(current_cont->states);
							// free(current_cont->levl);
							// free(current_cont);
							clean_continuation(current_cont);
							current_cont = new_continuation;
						}
						break;
					}
					case eu: {
						printf("Proving EU formula's sub_fml2: %s\n", str_fml(fml->sub_fml2));
						State_set* merge = NULL;
						int finished = 0;
						Int_array_list* to_do_work = (Int_array_list*)malloc(sizeof(Int_array_list));
						to_do_work->head = fml->states->head->content.real;
						to_do_work->tail = NULL;

						

						while(NULL != to_do_work) {
							int* ia = to_do_work->head;
							if(is_in_state_set(merge, ia, modul->size)) {
								clear_state_set(merge);
								while(NULL != to_do_work) {
									Int_array_list* todotail = to_do_work->tail;
									free(to_do_work);
									to_do_work = todotail;
								}
								break;
							}

							Continuation* tcont = (Continuation*)malloc(sizeof(Continuation));
							tcont->type = basic_true;
							tcont->ref_count = 1;

							Continuation* fcont = (Continuation*)malloc(sizeof(Continuation));
							fcont->type = basic_false;
							fcont->ref_count = 1;

							Formula* fml2 = replicate_fml(fml->sub_fml2);
							Continuation* cont2 = (Continuation*)malloc(sizeof(Continuation));
							cont2->type = regular;
							cont2->ref_count = 1;
							cont2->states = NULL;
							// printf("%s\n", str_fml(fml2));
							cont2->formula = fml_replace_state_var(fml2, fml->str2, ia);
							char* buffer2 = (char*)malloc(sizeof(char)*(strlen(current_cont->levl)+1));
							buffer2[0] = '\0';
							something bad happens in the next.
							sprintf(buffer2, "%s%d", current_cont->levl, 2);
							cont2->levl = buffer2;
							cont2->contl = tcont;
							cont2->contr = fcont;
							// incr_ref_count(tcont);
							// incr_ref_count(fcont);

							int result2 = prove(cont2, modul);
							printf("EU doing fine for now.\n");
							if(1 == result2) {
								while(NULL != to_do_work) {
									Int_array_list* todotail = to_do_work->tail;
									free(to_do_work);
									to_do_work = todotail;
								}
								finished = 1;
								//clean_continuation(cont2);
								break;
							}

							//bad things happened before this position...
							

							tcont = (Continuation*)malloc(sizeof(Continuation));
							tcont->type = basic_true;
							tcont->ref_count = 1;

							fcont = (Continuation*)malloc(sizeof(Continuation));
							fcont->type = basic_false;
							fcont->ref_count = 1;
							// State* s = mk_state_real(ia);
							Formula* fml1 = replicate_fml(fml->sub_fml1);
							Continuation* cont1 = (Continuation*)malloc(sizeof(Continuation));
							cont1->type = regular;
							cont1->ref_count = 1;
							cont1->states = NULL;
							cont1->formula = fml_replace_state_var(fml1, fml->str1, ia);
							char* buffer = (char*)malloc(sizeof(char)*(strlen(current_cont->levl)+1));
							sprintf(buffer, "%s%d", current_cont->levl, 1);
							cont1->levl = buffer;
							cont1->contl = tcont;
							cont1->contr = fcont;
							// incr_ref_count(tcont);
							// incr_ref_count(fcont);

							int result1 = prove(cont1, modul);
							if(0 == result1) {
								while(NULL != to_do_work) {
									Int_array_list* todotail = to_do_work->tail;
									free(to_do_work);
									to_do_work = todotail;
								}
								//clean_continuation(cont1);
								break;
							} else {
								merge = insert(merge, ia, modul->size);
								Int_array_list* tmp_ial = next(modul, ia, modul->size);
								Int_array_list* tmptmp_ial = tmp_ial;
								while(NULL != tmptmp_ial->tail) {
									tmptmp_ial = tmptmp_ial->tail;
								}
								tmptmp_ial->tail = to_do_work;
								to_do_work = tmp_ial;
							}

							to_do_work = to_do_work->tail;
						}

						// free(tcont);
						// free(fcont);

						clear_state_set(merge);

						if(1 == finished) {
							Continuation* ccontl = current_cont->contl;
							incr_ref_count(ccontl);
							clean_continuation(current_cont);
							current_cont = ccontl;
							break;
						} else {
							Continuation* ccontr = current_cont->contr;
							incr_ref_count(ccontr);
							clean_continuation(current_cont);
							current_cont = ccontr;
							break;
						}
						
					}
					case ar: {
						State_set* merge = NULL;
						int finished = 1;
						Int_array_list* to_do_work = (Int_array_list*)malloc(sizeof(Int_array_list));
						to_do_work->head = fml->states->head->content.real;
						to_do_work->tail = NULL;

						Continuation* tcont = (Continuation*)malloc(sizeof(Continuation));
						tcont->type = basic_true;
						tcont->ref_count = 1;

						Continuation* fcont = (Continuation*)malloc(sizeof(Continuation));
						fcont->type = basic_false;
						fcont->ref_count = 1;

						while(NULL != to_do_work) {
							int* ia = to_do_work->head;
							if(is_in_state_set(merge, ia, modul->size)) {
								clear_state_set(merge);
								while(NULL != to_do_work) {
									Int_array_list* todotail = to_do_work->tail;
									free(to_do_work);
									to_do_work = todotail;
								}
								break;
							}

							Formula* fml2 = replicate_fml(fml->sub_fml2);
							Continuation* cont2 = (Continuation*)malloc(sizeof(Continuation));
							cont2->type = regular;
							cont2->ref_count = 1;
							cont2->states = NULL;
							cont2->formula = fml_replace_state_var(fml2, fml->str2, ia);
							char* buffer2 = (char*)malloc(sizeof(char)*(strlen(current_cont->levl)+1));
							sprintf(buffer2, "%s%d", current_cont->levl, 2);
							cont2->levl = buffer2;
							cont2->contl = tcont;
							cont2->contr = fcont;
							incr_ref_count(tcont);
							incr_ref_count(fcont);

							int result2 = prove(cont2, modul);
							if(0 == result2) {
								while(NULL != to_do_work) {
									Int_array_list* todotail = to_do_work->tail;
									free(to_do_work);
									to_do_work = todotail;
								}
								finished = 0;
								clean_continuation(cont2);
								break;
							}
							// State* s = mk_state_real(ia);
							Formula* fml1 = replicate_fml(fml->sub_fml1);
							Continuation* cont1 = (Continuation*)malloc(sizeof(Continuation));
							cont1->type = regular;
							cont1->ref_count = 1;
							cont1->states = NULL;
							cont1->formula = fml_replace_state_var(fml1, fml->str1, ia);
							char* buffer = (char*)malloc(sizeof(char)*(strlen(current_cont->levl)+1));
							sprintf(buffer, "%s%d", current_cont->levl, 1);
							cont1->levl = buffer;
							cont1->contl = tcont;
							cont1->contr = fcont;
							incr_ref_count(tcont);
							incr_ref_count(fcont);

							int result1 = prove(cont1, modul);
							if(1 == result1) {
								while(NULL != to_do_work) {
									Int_array_list* todotail = to_do_work->tail;
									free(to_do_work);
									to_do_work = todotail;
								}
								clean_continuation(cont1);
								break;
							} else {
								merge = insert(merge, ia, modul->size);
								Int_array_list* tmp_ial = next(modul, ia, modul->size);
								Int_array_list* tmptmp_ial = tmp_ial;
								while(NULL != tmptmp_ial->tail) {
									tmptmp_ial = tmptmp_ial->tail;
								}
								tmptmp_ial->tail = to_do_work;
								to_do_work = tmp_ial;
							}

							to_do_work = to_do_work->tail;
						}

						free(tcont);
						free(fcont);
						clear_state_set(merge);

						if(1 == finished) {
							Continuation* ccontl = current_cont->contl;
							incr_ref_count(ccontl);
							clean_continuation(current_cont);
							current_cont = ccontl;
							break;
						} else {
							Continuation* ccontr = current_cont->contr;
							incr_ref_count(ccontr);
							clean_continuation(current_cont);
							current_cont = ccontr;
							break;
						}
					}
				}
			}
		}
	}
	
}

void prove_model(Modul* modul) 
{
	String_fml_list* specs = modul->specs;
	while(NULL != specs) {
		char* name = specs->name;
		Formula* fml = &(specs->fml);
		fml = fml_replace_state_var(fml, "ini", modul->init_assigns);

		Continuation* tcont = (Continuation*)malloc(sizeof(Continuation));
		tcont->type = basic_true;
		tcont->ref_count = 1;
		Continuation* fcont = (Continuation*)malloc(sizeof(Continuation));
		fcont->type = basic_false;
		fcont->ref_count = 1;
		Continuation* cont = (Continuation*)malloc(sizeof(Continuation));
		cont->type = regular;
		cont->ref_count = 1;
		cont->states = NULL;
		cont->formula = fml;
		cont->levl = strdup("1");
		cont->contl = tcont;
		cont->contr = fcont;
		int result = prove(cont, modul);
		if(TRUE == result) {
			printf("Result of %s: True\n", name);
		} else {
			printf("Result of %s: False\n", name);
		}
		// clean_continuation(cont);
		specs = specs->next;
	}
}
