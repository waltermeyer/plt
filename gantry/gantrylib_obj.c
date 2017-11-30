#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// The Object Struct
typedef struct obj {
  // metadata
  struct obj *next; // next
  char *k;    // key name
  int  v_typ; // value type

  // values
  int i;     // int
  double f;   // float
  struct obj *o;   // object
  char *s;   // string
  bool b; // bool
} obj;

int print_k(obj *o) {
  if (o == NULL)
    return 1;
  switch(o->v_typ) {
    case 3: printf("%d\n", o->i); break;
    case 4: printf("%f\n", o->f); break;
    case 5: printf("object key [%p]\n", &o); break;
    case 6: printf("%s\n", o->s); break;
    case 7: printf("%s", o->b ? "true\n" : "false\n"); break;
    default: printf("object [%p]\n", &o); break;
  };
  return 0;
}


char *xrealloc(char *ptr, size_t sz){
	char *temp = (char *)realloc(ptr, sz);
	if (temp == NULL){
		printf("Failed to realloc");
	}
	else {
		ptr = temp;
		memset(ptr, 0 , sz);
	}
	return ptr;
}

char *fill_buff(char *buff, char *cpy_buff){
	size_t cpy_len;
	size_t buff_sz;
	char *old_buff;

	//printf("_____FILL BUFF_____\n");
	

	cpy_len = sizeof(char) *(strlen(cpy_buff) + 1);
	//printf("Contents of cpy_buff : %s \n" , cpy_buff);
	//printf("Length of cpy_buff : %zu \n", cpy_len);
	
	buff_sz = sizeof(char) *(strlen(buff)+1);
	old_buff = (char *)malloc(buff_sz);
	//printf("size of buff before memcpy %zu \n", buff_sz);
	memcpy(old_buff, buff, buff_sz);
	buff = (char *)xrealloc(buff, buff_sz + cpy_len);
	//printf("buffsz + cpy_len = %zu \n", buff_sz + cpy_len);
	
	memcpy(buff,old_buff, buff_sz);
	//printf("After inserting buff: %s \n", new_buff);
	free(old_buff);
	
	memcpy(buff + buff_sz -1, cpy_buff, cpy_len);
	//printf("After inserting cpy_buff : %s \n", new_buff);
		

	//printf("-- END OF FILL BUFF --\n");
	return buff;
}

char *string_key(obj *o, char *buff){
	char *cpy_buff;
	char *k;
	int len;
	
	len = 3;
	cpy_buff = malloc(sizeof(char)*len);
	cpy_buff = memcpy(cpy_buff, "\"", len);
	buff = fill_buff(buff, cpy_buff);
	free(cpy_buff);

	k = o->k;
	len = strlen(k);
	cpy_buff = malloc(sizeof(char)*(len+1));
	memcpy(cpy_buff, k, len+1);
	buff = fill_buff(buff, cpy_buff);
	free(cpy_buff);
	
	len = 3;
	cpy_buff = malloc(sizeof(char)*len);
	cpy_buff = memcpy(cpy_buff, "\"", len);
	buff = fill_buff(buff, cpy_buff);
	free(cpy_buff);
	
	len = 4;
	cpy_buff = malloc(sizeof(char)*len);
	cpy_buff = memcpy(cpy_buff, " : ", len);
	buff = fill_buff(buff, cpy_buff);
	free(cpy_buff);
	
	return buff;
}

// TODO : these should output to char buff not print
char *string_val(obj *o, char *buff){
	char *cpy_buff;
	int len;
	
	switch(o->v_typ) {
    	  case 3: 
		len = snprintf(NULL, 0 , "%d" , o->i);  
		cpy_buff = malloc(sizeof(char)*(len+1));
		snprintf(cpy_buff,len+1, "%d\n", o->i); 
		break;
	  case 4: 
		len = snprintf(NULL, 0 , "%f" , o->f); 
		cpy_buff = malloc(sizeof(char)*(len+1));
		snprintf(cpy_buff,len+1, "%f", o->f); 
		break;
	  //case 5: snprintf(NULL, 0 ,  , o->);  snprintf(temp, "object key [%p]\n", &o); break;
	  case 6: 
		len = snprintf(NULL, 0 , "%s" , o->s);  
		cpy_buff = malloc(sizeof(char)*(len+1));
		snprintf(cpy_buff,len+5, "\\\"%s\\\"", o->s); 
		break;
	  case 7:
		len= snprintf(NULL, 0 , "%s" , o->b);  
		cpy_buff = malloc(sizeof(char)*(len+1));
		snprintf(cpy_buff, len+1,"%s", o->b ? "true" : "false"); 
		break;
	  //default:snprintf("object [%p]\n", &o); break;
	};
	buff = fill_buff(buff, cpy_buff);
	free(cpy_buff);		

	len = 3;
	cpy_buff = malloc(sizeof(char)*len);
	memcpy(cpy_buff, " ,", len);
	buff = fill_buff(buff, cpy_buff);
	//printf(" copy buff after , %s \n" , cpy_buff);
	//printf(" buff after kv , %s \n", buff);
	free(cpy_buff);
	
	return buff;
}

char *rec_stringify(obj *o, char *buff){

	size_t cpy_len;
	//char *cpy_buff;
	const char *cpy_buff;
	size_t buff_len;
	char *old_buff;

	o = o->next;
	printf("line 164\n");	
	buff_len = (strlen(buff)+1)*sizeof(char);
	old_buff = malloc(buff_len);	
	old_buff = memcpy(old_buff, buff, buff_len);
		
	cpy_buff = "{ ";
	cpy_len = (strlen(cpy_buff) + 1)*sizeof(char);
	buff = xrealloc(buff, (buff_len + cpy_len)*sizeof(char));
	memcpy(buff, old_buff, buff_len);
	memcpy(buff + buff_len -1, cpy_buff, cpy_len);

	free(old_buff);	
	printf("line 176\n");	

	while (o != NULL) {
		if (o->v_typ == 5){
			if(o->k){	
				buff = string_key(o, buff);
			}
			buff = rec_stringify(o->o, buff);
		}
		else{
			buff = string_key(o, buff);
			printf("line 187\n");	
			buff = string_val(o, buff);	
			printf("line 189\n");	
		}
		printf("line 191\n");	
		o = o->next;
	}
	
	/* Remove comma after last value in object */
	buff_len = (strlen(buff)+1)*sizeof(char);

	old_buff = malloc(buff_len-1);	
	old_buff = memcpy(old_buff, buff, buff_len-2);	
	
	/* Add closing parenthesis of object */
	buff_len = (strlen(old_buff)+1)*sizeof(char);

	cpy_buff = " },";
	cpy_len = (strlen(cpy_buff) + 1)*sizeof(char);
	
	buff = xrealloc(buff, (buff_len + cpy_len));
	memcpy(buff, old_buff, buff_len);
	printf("Here is buff before inserting %s \n ", buff);
	memcpy(buff + buff_len - sizeof(char), cpy_buff, cpy_len);
	printf("Here is buff after inserting %s \n ", buff);

	free(old_buff);		
	
	return buff;
}

/* 
 * Called in Gantry to stringify an object.
 * Calls rec_stringify to obtain string, cleans up string
 */
char *obj_stringify(obj *o){
	size_t buff_len;	
	char *buff;
	char *pre_buff;

	char *temp_buff = (char *)malloc(sizeof(char));
	memcpy(temp_buff, "", 1);
	pre_buff = rec_stringify(o, temp_buff);
	
	//free(temp_buff);

	/* Remove comma after Object */
	buff_len = (strlen(pre_buff)+1)*sizeof(char);

	buff = malloc(buff_len-1);
	buff = memcpy(buff, pre_buff, buff_len-2);	
	free(pre_buff);		

	return buff;
}
 
/*
 * Recursively search for a key in an object
 */
obj *obj_findkey(obj *o, char *keys) {
  char *k, *keys_dup;
  keys_dup = strdup(keys);

  // parse a key to search for
  k = strsep(&keys_dup, ".");

//  printf("Searching: %s\n", keys);
//  printf("Current Key: %s\n", k);

  // search for keys within an object
  o = o->next; // get first key from head struct
  while (o != NULL) {
//    printf("on: %s\n", o->k);
    if (strcmp(o->k, k) == 0) {
      // found our key
      if (strlen(k) == strlen(keys)) {
//      printf("Found the key!\n");
	free(k);
	return o;
      }
      // continue nested key search
      else if (o->v_typ == 5) {
//      printf("Found object that nests the key\n");
//	printf("Now searching for: %s\n", keys_dup);
        o = obj_findkey(o->o, keys_dup);
        free(k);
	return o;
      }
    }
    o = o->next;
  }

  // not found
  free(k);
  return NULL;
}

/*
 * Assign a value to an object
 */
int obj_assign(obj *o, int t, void *v) {
  int l;
  char *s;
  if (o != NULL) {
    // If this key was a string, garbage collect it
//    if (o->v_typ == 6)
//      free(o->s);
    // Set new key type
    o->v_typ = t;
    // Set key value
    switch(o->v_typ) {
      case 3: o->i = *(int *)v; break;
      case 4: o->f = *(double *)v; break;
      case 5: o->o = (obj *)v; break;
      case 6:
	// malloc space for the string and store
	l = strlen((const char *)(*(void **)v)) + 1;
	s = (char *)malloc(sizeof(l));
	strcpy(s, (const char *)(*(void **)v));
	o->s = s;
	break;
      case 7: o->b = *(bool *)v; break;
    };
  }
  return 0;
}


/*
 * Get a key value from an Object
 */
void *obj_getkey(obj *o, int t) {
  /*
   * Throw a runtime error if the type
   * requested does not match actual type
   */
  if (o->v_typ != t) {
    printf("Runtime Error: Invalid type requested from Object ID [%s] [%p]\n",
            o->k, &o);
    exit(2);
  }
  switch(o->v_typ) {
    case 3: return (void *)&o->i;
    case 4: return (void *)&o->f;
    case 5: return (void *)o->o;
    case 6: return (void *)&o->s;
    case 7: return (void *)&o->b;
  };
  return NULL;
}

/*
 * Get a key value's type from an Object
 */
int obj_gettyp(obj *o) {
  return o->v_typ;
}

#ifdef BUILD_TEST
int main () {
  obj *o = (obj *) malloc(sizeof(obj)); // head
  obj *o2 = (obj *) malloc(sizeof(obj));
  obj *o3 = (obj *) malloc(sizeof(obj));
  obj *o4_head = (obj *) malloc(sizeof(obj));
  obj *o4 = (obj *) malloc(sizeof(obj));
  obj *o5_head = (obj *) malloc(sizeof(obj));
  obj *o5 = (obj *) malloc(sizeof(obj));
  obj *o6 = (obj *) malloc(sizeof(obj));

  /*
   *  Test data structure represented below:
   *  object o = {| int key2 : 3,
   *		    object key3 :
   *		      {| object nestedkey4 :
   *			 {| string nestedkey5 : "test",
   *			    float nestedkey6 : 30.65
   *			 |}
   *		      |}
   *		 |}
   */

  o->k = NULL;
  o->next = o2;

  o2->k = "key2";
  o2->i = 2;
  o2->v_typ = 3;
  o2->next = o3;

  o3->k = "key3";
  o3->o = o4_head;
  o3->v_typ = 5;
  o3->next = NULL;

  o4_head->next = o4;
  o4->k = "nestedkey4";
  o4->o = o5_head;
  o4->v_typ = 5;

  o5_head->next = o5;
  o5->k = "nestedkey5";
  o5->s = "test";
  o5->v_typ = 6;
  o5->next = o6;

  o6->k = "nestedkey6";
  o6->f = 30.65;
  o6->v_typ = 4;
  o6->next = NULL;

  obj *tmp;
  void *v;
  tmp = obj_findkey(o, "key2");
  v   = obj_getkey(tmp, tmp->v_typ);
  printf("%d\n", *(int *)v);
  double t = 42.24;
  obj_assign(tmp, 4, (void *)&t);
  tmp = obj_findkey(o, "key2");
  v   = obj_getkey(tmp, tmp->v_typ);
  printf("%lf\n", *(double *)v);

  obj_findkey(o, "key3.nestedkey4.nestedkey5");

  obj_findkey(o, "key3.nestedkey4.nestedkey6");
  obj_findkey(o, "key3.nestedkey4");
  obj_findkey(o, "keyfail.nestedkey4");

  char *buff;
  buff = obj_stringify(o);
  printf("====THE OBJECT=====\n\n %s \n\n ===============\n" , buff);
  free(buff);


  free(o);
  free(o2);
  free(o3);
  free(o4);
  free(o4_head);
  free(o5);
  free(o5_head);
  free(o6);
}
#endif

