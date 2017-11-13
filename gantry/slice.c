#include <string.h>
#include <stdio.h>
#include <stdlib.h>


char *slice(int begin, int end, char *src){
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
	char *dest = slice(1, 3, src);
	printf("%s\n", dest);
	dest = slice(0, 10, src);
	printf("%s\n", dest);
	dest = slice(4, 20, src);
	printf("%s\n", dest);
	return 0;
}
#endif
