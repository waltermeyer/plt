#include <string.h>
#include <stdio.h>
#include <stdlib.h>


char *string_slice(int begin, int end, char *src){
	int len = end - begin;
	char *dest = malloc(len + 1);
	int i;
	int dest_i = 0;
	for (i=begin;i<end && src[i]!='\0'; i++)
		dest[dest_i] = src[i];
		dest_i++;
	for (;i<end;i++)
		dest[dest_i] = '\0';
		dest_i++;
	return dest;
}


int main(){
	char *src = "this is a string";
	char *dest = string_slice(1, 3, src);
	printf("%s\n", dest);
	src = "foobar";
	dest = string_slice(0, 10, src);
	printf("%s\n", dest);
	//dest = string_slice(4, 20, src);
	//printf("%s\n", dest);
	return 0;
}
