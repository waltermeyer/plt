#include <stdio.h>
#include <string.h>
#include <alloca.h>

struct JSON{
  struct JSON *prev;
  struct JSON *next;
  struct JSON *child;
  char *val;

  int i;
  float f;
  int b;
  
  char *key;
};

// { "Name": "Joe",
//   "Age" : 20,
//   "Classes" : ["PLT", "Class 2", "Class 3"]
//   "Contact Info": 
//  	{ "Address" : "22 Jumpstreet",
// 	  "Phone" : 9999999999 }
// }
struct JSON * new_token () {
 	struct JSON *token = alloca(sizeof(struct JSON));
	token->key= NULL;
	token->val=NULL;
	token->prev = NULL;
	token->next=NULL;
	token->child=NULL;
	return token;
}


int main(){
	struct JSON *token= new_token();
	token->key = "Joe";
	printf("%s\n", token->key);
	return 0;
}
