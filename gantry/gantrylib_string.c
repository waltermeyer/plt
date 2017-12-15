/*
 * Author: Audrey Copeland
 * Contributor: Taimur Samee
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

char *get_string(char* req) {
	char s[100];
	char *ret;
	printf("%s", req);
	scanf("%s", s);
	ret = malloc(strlen(s))+1;
	memcpy(ret, s, strlen(s)+1);
	return ret;
}

int stoint(char *s){
	int ret = atoi(s);
	return ret;
}

char *slice(char *src, int begin, int end){
	int len = end - begin;
	char *dest = malloc(len + 1);
	int i;
	if (begin>end){
    		printf("Runtime Error: Slice begin integer %d is greater than end integer %d,", begin, end);
	}
	int dest_i = 0;
	for (i=begin;i<end&&src[i]!='\0' ; i++){
		dest[dest_i] = src[i];
		dest_i++;
	}
	for (; i <= end; i++){
		dest[dest_i] = '\0';
	}
	return dest;
}

/* Returns 0 if equal, -1 if a<b, 1 if a>b */
int stringcmp(const char *a, const char *b){
	int x = strcmp(a, b);
	int res = 0;	
	if (x == 0){
		res = 0;;
	}
	else if (x < 0){
		res = -1;
	}
	else {
		res = 1;
	}
	return res;
}

/* Returns 1 if equal , returns 0 if not equal */
bool stringeq(const char *a , const char *b){
	int x = strcmp(a, b);
	bool res = 0;
	if (x == 0){
		res = 1;
	}
	else {
		res = 0;
	}
	return res;
}

/* Get passed two strings, malloc space for concatenation */
char *string_concat(char *a, char *b){
	int len_a = strlen(a);
	int len_b = strlen(b);
	char *c = malloc(len_a + len_b + 1);
	memcpy(c, a, len_a);
	memcpy(c + len_a, b, len_b + 1);
	return c;
}

int string_length(char *a){
	return strlen(a);
}

int test_slice(){
	char *src = "this is a string";
	char *dest = slice(src, 1, 3);
	printf("%s\n", dest);
	dest = slice(src, 0, 10);
	printf("%s\n", dest);
	dest = slice(src, 4, 20);
	printf("%s\n", dest);
	return 0;
}

int test_string_concat(){
	char a[] = "foo";
	char b[] = "bar";
	char *c = string_concat(a, b);
	printf("%s", c);
	free(c);
	return 0;
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


int test_string_length(){
	char a [] = "foo";
	char b [] = "foobar";
	int x = string_length(a);
	int y = string_length(b);
	printf("This should be 3 : %d \n", x);
	printf("This should be 6 : %d \n", y);
	return 0;
}

#ifdef BUILD_TEST
int main(){
	printf("=== Testing String Comparison ===\n");
	test_stringcmp();
	printf("=== Testing String Concat ===\n");
	test_string_concat();
	printf("=== Testing String Length ===\n");
	test_string_length();
	printf("=== Testing String Slice ===\n");
	test_slice();
	char *s = get_string("Please enter input : ");
	printf("%s\n", s);
	char *si = "4";
	int i = stoint(si);
	printf("%d\n", i);
	return 0;
}
#endif
