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

// This successfully updates the key of token 1 but not the sibling pointer
int add_sibling(struct JSON *parent_token, struct JSON *sibling_token){
	parent_token->next = sibling_token;
	sibling_token->prev = parent_token;
	parent_token->key = "Bob";
	//printf("%s\n", sibling_token->key);
	//printf("%s\n", parent_token->key);
	return 0;
}


int main(){
	struct JSON *token1= new_token();
	token1->key = "Joe";
	printf("%s\n", token1->key);
	
	struct JSON *token2 = new_token();
	token2->key = "Age";
	token2->i = 22;
	
	//token1->next = token2;
	add_sibling(token1, token2);
	printf("%s\n", token1->key);
	struct JSON *token1_again = token2->prev;
	printf("%s\n", token1_again->key);
	return 0;
}
