#include "state_set.h"
#include <assert.h>
/*
step 1: find the minimal unbalanced tree.
step 2: balance the minimal unbalanced tree.
step 3: update height information.
*/

int height_left(State_set* ss)
{
	
	if (NULL == ss){
		// printf("getting left height of ss: %d\n", -1);
		return -1;
	} else if (NULL == ss->setl) {
		// printf("getting left height of ss: %d\n", 0);
		return 0;
	} else {
		// printf("getting left height of %d: %d\n", ss->state[0], ss->setl->height);
		return ss->setl->height;
	}
}

int height_right(State_set* ss)
{
	if (NULL == ss ) {
		return -1;
	} else if (NULL == ss->setr) {
		return 0;
	} else {
		return ss->setr->height;
	}
}

int update_height(State_set* ss) 
{
	if(NULL == ss) {
		return 0;
	}
	int hl = update_height(ss->setl);
	int hr = update_height(ss->setr);
	ss->height = hl > hr? hl+1 : hr+1;
	return ss->height;
}

int height_diff(State_set* ss)
{
	if ((NULL == ss) || (NULL == ss->setl && NULL == ss->setr)) {
		return 0;
	} else if (NULL == ss->setl && NULL != ss->setr) {
		return -(ss->setr->height);
	} else if (NULL != ss->setl && NULL == ss->setr) {
		return ss->setl->height;
	} else {
		return ss->setl->height - ss->setr->height;
	}
}


int is_balanced(State_set* ss)
{
	int hd = height_diff(ss);
	if (hd >= 2 || hd <= -2) {
		return 0;
	}
	return 1;
}

State_set* balance(State_set* ss)
{
	// return ss;
	int hd = height_diff(ss);
	switch(hd) {
		case -2: {
			int hdr = height_diff(ss->setr);
			if (hdr > 0) { //RL
				// printf("balancing RL\n");
				State_set* ss2 = ss->setr;
				State_set* ss3 = ss2->setl;
				ss->setr = ss3->setl;
				ss2->setl = ss3->setr;
				ss3->setl = ss;
				ss3->setr = ss2;
				update_height(ss3);
				return ss3;
			} else if (hd < 0) { //RR
				// printf("balancing RR\n");
				State_set* ss2 = ss->setr;
				State_set* ss3 = ss2->setl;
				ss->setr = ss3;
				ss2->setl = ss;
				update_height(ss2);
				return ss2;
			} else {
				printf("error balancing tree: the tree is already balanced.\n");
				exit(1);
			}
			break;
		}
		case 2: {
			int hdl = height_diff(ss->setl);
			if(hdl > 0) { //LL
				// printf("balancing LL\n");
				// print_state_set_clr(ss, 1);
				State_set* ss2 = ss->setl;
				State_set* ss4 = ss2->setr;
				ss->setl = ss4;
				ss2->setr = ss;
				update_height(ss2);
				// print_state_set_clr(ss2, 1);
				return ss2;
			} else if(hdl < 0) { //LR
				// printf("balancing LR\n");
				State_set* ss2 = ss->setl;
				State_set* ss4 = ss2->setr;
				ss2->setr = ss4->setl;
				ss->setl = ss4->setr;
				ss4->setl = ss2;
				ss4->setr = ss;
				update_height(ss4);
				return ss4;
			} else {
				printf("error balancing tree: the tree is already balanced.\n");
				exit(1);
			}
			break;
		}
		default:
			return ss;
	}
	
}

State_set* insert(State_set* ss, int* state, int size)
{
	// printf("inserting state\n");
	if (NULL == ss)
	{
		ss = (State_set*)malloc(sizeof(State_set));
		ss->height = 1;
		ss->state = state;
		ss->setl = NULL;
		ss->setr = NULL;
		return ss;
	} else {
		int need_balance = 0;
		// int left_right = 0;
		// State_set_stack* sss;
		State_set_list* ssl = NULL;
		// sss = push(sss, ss);
		State_set* tmp_ss = ss;
		while(NULL != tmp_ss) 
		{
			int compare = memcmp(tmp_ss->state, state, sizeof(int)*size);
			State_set_list* tmp_ssl = (State_set_list*)malloc(sizeof(State_set_list));
			tmp_ssl->state = tmp_ss;
			tmp_ssl->next = ssl;
			ssl = tmp_ssl;

			// sss = push(sss, tmp_ss);
			if (compare < 0) { 
				tmp_ss = tmp_ss->setl;
				need_balance = 1; //need to balance left subtree
				ssl->left_right = left;
			} else if (compare > 0) {
				tmp_ss = tmp_ss->setr;
				need_balance = 2; //need to balance right subtree
				ssl->left_right = right;
			} else {
				need_balance = 0;
				free(tmp_ss->state);
				tmp_ss->state = state;
				while(NULL != ssl) {
					State_set_list* tail = ssl->next;
					free(ssl);
					ssl = tail;
				}
				// update_height(ss);
				// clean_state_set_stack(sss);
				tmp_ss = NULL;
			}
		}
		//update height information
		if (1 == need_balance)
		{
			tmp_ss = (State_set*)malloc(sizeof(State_set));
			tmp_ss->height = 1;
			tmp_ss->state = state;
			tmp_ss->setl = NULL;
			tmp_ss->setr = NULL;
			// sss->head->setl = tmp_ss; //insert the new node as the left subnode of the head of sss.
			ssl->state->setl = tmp_ss;
			State_set_list* tmp_ssl = ssl;
			State_set* need_to_balance = NULL;
			while(NULL != tmp_ssl)
			{
				int hl = height_left(tmp_ssl->state);
				int hr = height_right(tmp_ssl->state);
				tmp_ssl->state->height = hl > hr? hl+1 : hr+1;
				if (NULL == need_to_balance && 0 == is_balanced(tmp_ssl->state)) {
					need_to_balance = tmp_ssl->state;
					break;
				}
				tmp_ssl = tmp_ssl->next;
			}
			if(NULL != need_to_balance) {
				State_set* new = balance(need_to_balance);
				assert(NULL != new);
				assert(NULL != need_to_balance);
				if(NULL == tmp_ssl->next) {
					ss = new;
				} else if(left == tmp_ssl->next->left_right) {
					tmp_ssl->next->state->setl = new;
				} else {
					tmp_ssl->next->state->setr = new;
				}
			}
			while(NULL != ssl) {
				State_set_list* tail = ssl->next;
				free(ssl);
				ssl = tail;
			}
			// clean_state_set_stack(sss);
		} 
		else if (2 == need_balance)
		{
			tmp_ss = (State_set*)malloc(sizeof(State_set));
			tmp_ss->height = 1;
			tmp_ss->state = state;
			tmp_ss->setl = NULL;
			tmp_ss->setr = NULL;
			// sss->head->setr = tmp_ss; //insert the new node as the right subnode of the head of sss.
			ssl->state->setr = tmp_ss;
			State_set_list* tmp_ssl = ssl;
			State_set* need_to_balance = NULL;
			while(NULL != tmp_ssl)
			{
				int hl = height_left(tmp_ssl->state);
				int hr = height_right(tmp_ssl->state);
				tmp_ssl->state->height = hl > hr? hl+1 : hr+1;
				if (NULL == need_to_balance && 0 == is_balanced(tmp_ssl->state)) {
					need_to_balance = tmp_ssl->state;
					break;
				}
				tmp_ssl = tmp_ssl->next;
			}
			if(NULL != need_to_balance) {
				State_set* new = balance(need_to_balance);
				assert(NULL != new);
				assert(NULL != need_to_balance);
				// *need_to_balance = *new;
				if(NULL == tmp_ssl->next) {
					ss = new;
				} else if(left == tmp_ssl->next->left_right) {
					tmp_ssl->next->state->setl = new;
				} else {
					tmp_ssl->next->state->setr = new;
				}
			}
			while(NULL != ssl) {
				State_set_list* tail = ssl->next;
				free(ssl);
				ssl = tail;
			}
			// clean_state_set_stack(sss);
		}
		return ss;
	}
}

int is_in_state_set(State_set* ss, int* state, int length)
{
	if(NULL != ss) {
		int compare = memcmp(ss->state, state, sizeof(int)*length);
		if(0 == compare) {
			return 1;
		} else if(compare < 0) {
			return is_in_state_set(ss->setl, state, length);
		} else {
			return is_in_state_set(ss->setr, state, length);
		}
	} else {
		return 0;
	}
}

void print_state_set(State_set* ss, int size) 
{
	if(NULL == ss){
		// printf("printing a null state set\n");
		return;
	} else {
		print_state_set(ss->setl, size);
		int* state = ss->state;
		for (int i = 0; i < size; ++i)
		{
			printf("%d ", state[i]);
		}
		printf("--> %d\n", ss->height);
		print_state_set(ss->setr, size);
	}
}

void print_state_set_clr(State_set* ss, int size)
{
	if(NULL == ss){
		// printf("printing a null state set\n");
		printf("*\n");
		return;
	} else {
		int* state = ss->state;
		for (int i = 0; i < size; ++i)
		{
			printf("%d ", state[i]);
		}
		printf("--> %d\n", ss->height);
		print_state_set_clr(ss->setl, size);
		print_state_set_clr(ss->setr, size);
	}
}

int actual_height(State_set* ss)
{
	if(NULL == ss) {
		return 0;
	} else {
		int ahl = actual_height(ss->setl);
		int ahr = actual_height(ss->setr);
		return (ahl > ahr? ahl+1 : ahr+1);
	}
}

void clear_state_set(State_set* ss)
{
	if(NULL == ss) {
		return;
	} else {
		State_set* setl = ss->setl;
		State_set* setr = ss->setr;
		free(ss->state);
		free(ss);
		clear_state_set(setl);
		clear_state_set(setr);
	}
}

State_set* replicate_state_set(State_set* ss, int length)
{
	if(NULL == ss) {
		return NULL;
	} else {
		State_set* tmp_ss = (State_set*)malloc(sizeof(State_set));
		tmp_ss->height = ss->height;
		int* new_state = (int*)malloc(sizeof(int)*length);
		memcpy(new_state, tmp_ss->state, sizeof(int)*length);
		tmp_ss->state = new_state;
		tmp_ss->setl = replicate_state_set(ss->setl, length);
		tmp_ss->setr = replicate_state_set(ss->setr, length);
		return tmp_ss;
	}
}