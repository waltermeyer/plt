#include <stdio.h>
#include <string.h>
#include <alloca.h>



/*
 * Sample JSON Object:
 *
 * { "Name": "Joe",
 *   "Age" : 20,
 *
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


// This successfully updates the key of token 1 but not the sibling pointer
int add_sibling(struct token *parent_token, struct token *sibling_token){
	parent_token->next = sibling_token;
	sibling_token->prev = parent_token;
	parent_token->key = "Bob";
	//printf("%s\n", sibling_token->key);
	//printf("%s\n", parent_token->key);
	return 0;
}

int add_child(struct token *parent_token, struct token *child_token){
	parent_token->child = child_token;
	return 0;
}

/* 
 * This should pretty print the object to check
 * but maybe not necessary
 */

int print_obj(struct token *obj_start){
	struct token *curr = obj_start;

	while(curr->next || curr->child){
	  if (curr->key) {
	    printf("%s\n", curr -> key);
	  }
	}
	return 0;
}




// * Generate struct based on JSON above */
// * NOTE : alloca frees memory on return so can't write specialized functions
// * dealing with these structs that involve returning alloca
// * TODO: Finish checking inner object.


int main(){
	struct token *token0 = alloca(sizeof(struct token));
	struct token *token1 = alloca(sizeof(struct token));
	struct token *token2 = alloca(sizeof(struct token));
	struct token *token3 = alloca(sizeof(struct token));
	struct token *token4 = alloca(sizeof(struct token));
	struct token *token5 = alloca(sizeof(struct token));
	struct token *token6 = alloca(sizeof(struct token));
	struct token *token7 = alloca(sizeof(struct token));
	struct token *token8 = alloca(sizeof(struct token));

	add_child(token0, token1);

	token1->key = "Name";
	printf("%s\n", token1->key);

	token2->key = "Age";
	token2->i = 22;
	
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
	add_sibling(token1, token2);
	add_sibling(token2, token3);
	printf("%s\n", token1->key);
	struct token *token1_again = token2->prev;
	printf("%s\n", token1_again->key);
	
	add_sibling(token3, token4);
	token4-> key = "Contact Info";
	add_child(token4, token5); 
	
	token5->key = "Address";
	token5->val = "22 Jump Street";
	token6->key = "Phone";
	token6->i = 999;
	return 0;
}
