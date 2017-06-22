#ifndef STATE_SET_H_
#define STATE_SET_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
	a set of states is implemented as an AVL tree
*/
typedef struct state_set
{
	int height;
	int* state;
	struct state_set* setl;
	struct state_set* setr;
} State_set;

typedef struct state_set_list
{
	State_set* state;
	enum {left, right} left_right;
	struct state_set_list* next;
} State_set_list;

int height_left(State_set* ss);
int height_right(State_set* ss);
int update_height(State_set* ss);
int height_diff(State_set* ss);
int is_balanced(State_set* ss);
State_set* balance(State_set* ss);
State_set* insert(State_set* ss, int* state, int length);
int is_in_state_set(State_set* ss, int* state, int length);
void print_state_set(State_set* ss, int length);
void print_state_set_clr(State_set* ss, int length);
int actual_height(State_set* ss);
void clear_state_set(State_set* ss);
State_set* replicate_state_set(State_set* ss, int length);


#endif