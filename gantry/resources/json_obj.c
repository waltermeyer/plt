#include <stdio.h>
#include <string.h>
#include <alloca.h>

/*
 * Sample JSON Object:
 *
 * { "Name": "Joe",
 *   "Age" : 20,
 *
 *   "Classes" : ["PLT", "Class 2", "Class 3"]
 *   "Contact Info": 
 *  	{ "Address" : "22 Jumpstreet",
 * 	  "Phone" : 9999999999 }
 * }
 *
 */



/* Keep track of where objects are located in memory.
 * Stores pointers to the memory location of the first token in an object.
 */
struct json_list{
  struct token *HEAD;
};

/* JSON token struct*/
struct token{
  struct token *prev;
  struct token *next;
  struct token *child;

  char *val;
  int i;
  float f;
  int b;
  
  char *key;
};

struct token * new_token () {
 	struct token *t = alloca(sizeof(struct token));
	return t;
}

// This successfully updates the key of token 1 but not the sibling pointer
int add_sibling(struct token *parent_token, struct token *sibling_token){
	parent_token->next = sibling_token;
	sibling_token->prev = parent_token;
	parent_token->key = "Bob";
	//printf("%s\n", sibling_token->key);
	//printf("%s\n", parent_token->key);
	return 0;
}


// Based on the output I'm getting, I think there's a problem with how
// I'm using alloca, because key values are being flushed fast, but
// maybe there's really a problem in this code.
int main(){
	struct token *token1= new_token();
	token1->key = "Joe";
	printf("%s\n", token1->key);


	struct token *token2 = new_token();
	token2->key = "Age";
	token2->i = 22;
	
	struct token *token3 = new_token();
	token3->key = "Favorite Color";
	token3->val = "Blue";
	
	token1->next = token3;
	token3->prev = token1;

	struct token *token1_from_token3 = token3->prev;
	struct token *token3_from_token1 = token1->next;
	printf("This is token 1 key from 3 in main: %s\n", token1_from_token3->key);
	printf("This is token 1 key: %s\n", token1->key);
	printf("This is token 3 key from 1 in main: %s\n", token3_from_token1->key);
	printf("This is token 3 key: %s\n", token3->key);
	//token1->next = token2;
	add_sibling(token1, token2);
	printf("%s\n", token1->key);
	struct token *token1_again = token2->prev;
	printf("%s\n", token1_again->key);
	return 0;
}
