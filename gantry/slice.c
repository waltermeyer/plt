#include <string.h>
#include <stdio.h>
#include <stdlib.h>


char *slice(char *src, int begin, int end){
	int len = end - begin;
	char *dest = malloc(len + 1);
	int i;
	int dest_i = 0;
	for (i=begin;i<end&&src[i]!='\0' ; i++){
		dest[dest_i] = src[i];
		dest_i++;
	}
	for (; i< end; i++){
		dest[dest_i] = '\0';
	}
	return dest;
}


#ifdef BUILD_TEST
int main(){
	char *src = "this is a string";
	char *dest = slice(src, 1, 3);
	printf("%s\n", dest);
	dest = slice(src, 0, 10);
	printf("%s\n", dest);
	dest = slice(src, 4, 20);
	printf("%s\n", dest);
	return 0;
}
#endif
