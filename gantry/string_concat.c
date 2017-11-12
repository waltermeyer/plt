#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/* Get passed two strings, malloc space for concatenation */
char *string_concat(char *a, char *b){
	char *c = malloc(strlen(a) + strlen(b) + 1);
	strncat(strncat(c, a, strlen(a) + strlen(c)), b, strlen(b) + strlen(c));
	return c;
}

#ifdef BUILD_TEST
int main(){
	char a[] = "foo";
	char b[] = "bar";
	char *str_a = malloc(sizeof(a));
	char *str_b = malloc(sizeof(b));
	char *c = string_concat(a, b);
	printf("%s", c);
	return 0;
}
#endif
