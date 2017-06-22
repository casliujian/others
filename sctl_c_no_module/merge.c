#include "merge.h"
#include "state_set.h"
void clear_merge(Merge* mrg, char* levl)
{
	while(NULL != mrg) {
		if(0 == strcmp(levl, mrg->levl)) {
			clear_state_set(mrg->states);
			mrg->states = NULL;
			return;
		}
		mrg = mrg -> next;
	}
}

Merge* add_to_merge(Merge* mrg, char* levl, int* state, int length)
{
	Merge* tmp_mrg = mrg;
	while(NULL != tmp_mrg) {
		if(0 == strcmp(levl, tmp_mrg->levl)) {
			tmp_mrg->states = insert(tmp_mrg->states, state, length);
			return mrg;
		}
		tmp_mrg = tmp_mrg->next;
	}
	Merge* new_mrg = (Merge*)malloc(sizeof(Merge));
	new_mrg->levl = levl;
	new_mrg->states = insert(NULL, state, length);
	new_mrg->next = mrg;
	return new_mrg;
}

int is_in_merge(Merge* mrg, char* levl, int* state, int length)
{
	while(NULL != mrg) {
		if(0 == strcmp(levl, mrg->levl)) {
			return is_in_state_set(mrg->states, state, length);
		}
		mrg = mrg -> next;
	}
	return 0;
}
