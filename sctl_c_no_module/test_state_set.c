#include "state_set.h"

#define size1 20
#define size2 1

int main(int argc, char const *argv[])
{
	// const int size1 = 3;
	// const int size2 = 1;
	static int a[size1][size2] = 
		{
			{3},	{4},	{1},	{2},	{5},	{7},	{0},	{11},	{20},	{8}, 
			{9}, 	{21},	{30}, 	{45},	{42},	{41},	{10},	{15},	{14},	{13}
		};
	// static int a[size1][size2] = {{1}, {2}, {3}};
	State_set* ss = NULL;
	for (int i = 0; i < size1; ++i)
	{
		ss = insert(ss, a[i], size2);
	}
	printf("height of ss is %d\n", actual_height(ss));
	printf("height diff of ss is: %d\n", height_diff(ss));
	printf("top node of ss is ");
	int* state = ss->state;
	for (int i = 0; i < size2; ++i)
	{
		printf("%d ", state[i]);
	}
	printf("\n");
	print_state_set(ss, size2);
	printf("clr:\n");
	print_state_set_clr(ss, size2);
	return 0;
}