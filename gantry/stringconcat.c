#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/* Get passed two strings, malloc space for concatenation */
char *string_concat(char *a, char *b){
	char *c = malloc(sizeof(a) + sizeof(b));
	c = strcat(a, b);
	printf("%s", c);
	return c;
}

#ifdef BUILD_TEST
int main(){
	char a[] = "foo";
	char b[] = "bar";
	char *str_a = malloc(sizeof(a));
	char *str_b = malloc(sizeof(b));
	string_concat(a, b);
	return 0;
}
#endif
