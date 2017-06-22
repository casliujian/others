#ifndef MERGE_H_
#define MERGE_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "state_set.h"

typedef struct merge
{
	char* levl;
	State_set* states;
	struct merge* next;
} Merge;

void clear_merge(Merge* mrg, char* levl);
Merge* add_to_merge(Merge* mrg, char* levl, int* state, int length);
int is_in_merge(Merge* mrg, char* levl, int* state, int length);


#endif