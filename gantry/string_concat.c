#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Get passed two strings, malloc space for concatenation */
char *string_concat(char *a, char *b){
	int len_a = strlen(a);
	int len_b = strlen(b);
	char *c = malloc(len_a + len_b + 1);
	memcpy(c, a, len_a);
	memcpy(c + len_a, b, len_b + 1);
	return c;
}

#ifdef BUILD_TEST
int main(){
	char a[] = "foo";
	char b[] = "bar";
	char *c = string_concat(a, b);
	printf("%s", c);
	free(c);
	return 0;
}
#endif
