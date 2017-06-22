#include <stdio.h>
#include <stdlib.h>

int main(int argc, char const *argv[])
{
	int* ia = (int*)malloc(sizeof(int)*10);
	for (int i = 0; i < 10; ++i)
	{
		printf("%d\t", ia[i]);
	}
	printf("\n");
	printf("size of ia is: %d\n", (sizeof(ia))/(sizeof(ia[0])));
	return 0;
}