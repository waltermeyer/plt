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

/* 
 * JSON token struct
 * Circularly linked list.  
 * 
 * HEAD will be a boolean in llvm 1 indicates that its the head, 0 otherwise
 *  
 * The 'next' pointer of the last sibling node will point to the HEAD of the outer list
 *
 * For inner objects, the HEAD flag will get set and the internal objects last token
 * will point to the HEAD of the inner list
 * 
 * So, if a token will always have a pointer to next, all except the head will have 
 * a pointer to prev, and tokens where the json value is an object will have next and child pointer.
 */

struct token{
  int HEAD;

  struct token *prev;
  struct token *next;
  
  char *key;

  /* 
   * What is the type of the value? 
   * 1 nested obj or array
   * 2 string
   * 3 int
   * 4 float
   * 5 boolean
   */
  int val_typ;


  /* These are the 'values' in the JSON object*/ 
  struct token *child;
  char *val;
  int i;
  float f;
  int b; // really a bool in LLVM
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
 * This doesn't work correctly and also is incorrect based on the new 
 * struct where next and child are not mutually exclusive
 *
 */

int print_obj(struct token *obj_start){
	struct token *curr = obj_start;

	while(curr->next || curr-> child){
	  if (curr->next){
	    curr = curr->next;
	  }
	  
	  else if (curr->child){
	    curr = curr->child;
	    printf("        {");
	  }
	  if (curr->key) {
	    printf("%s : ", curr->key);
	  }
	  if (curr->val){
	    printf("%s,/n", curr->val);
	  }
	  if (curr->i){
	    printf("%d,\n",curr->i);
	  }
	  if (curr->f){
	    printf("%f,\n", curr->f);
	  }
	  if (curr->b){
	    printf("%d,\n", curr->b);
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

	add_sibling(token5, token6);
	token6->key = "Phone";
	token6->i = 999;

	print_obj(token0);

	return 0;
}
