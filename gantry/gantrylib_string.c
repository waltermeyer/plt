#include <string.h>
#include <stdio.h>
#include <stdlib.h>

int stringcmp(const char *a, const char *b){
	return strcmp(a, b);
}


int test_stringcmp(){
	char a [] = "foo";
	char b [] = "bar";
	char c [] = "foo";
	int x = stringcmp(a, b);
	int y = stringcmp(a, c);
	printf("These are different %d \n", x);
	printf("These are the same %d \n", y);
	return 0;
}

#ifdef BUILD_TEST
int main(){
	printf("=== Testing String Comparison ===\n");
	test_stringcmp();
	return 0;
}
#endif
