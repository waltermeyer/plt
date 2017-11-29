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


char *stringify(obj *o){
	// if a value is an object, you want to call this recursively
	// but how does this impact the order of printing
	char *buff = (char*)malloc(sizeof(char));
	char *stringified = rec_stringify(o, buff);
	return stringified
} 

char *rec_stringify(obj *o, char *buff){

	int cpy_len;
	char *cpy_buff;
	
	o = o->next;

	cpy_buff = " { ";
	cpy_len = strlen(cpy_buff) + 1;
	buff = realloc(buff,cpy_len);
	memcpy(buff + cpy_len, cpy_buff, cpy_len);
	
		
	while (o != NULL) {
		if (o->v_type == 5){
			
			//o = obj_stringify(o->o, buff);
		}
		else{
			cpy_buff = o->k;
			cpy_len = strlen(cpy_buff) + 1;
			buff = realloc(buff,cpy_len);
			memcpy(buff + cpy_len, cpy_buff, cpy_len);
			cpy_buff = " : ";
			cpy_len = strlen(cpy_buff) + 1;
			buff = realloc(buff,cpy_len);
			memcpy(buff + cpy_len, cpy_buff, cpy_len);
					
			switch(o->v_typ) {
			  case 3: cpybreak;
			  case 4: printf("%f\n", o->f); break;
			  case 5: printf("object key [%p]\n", &o); break;
			  case 6: printf("%s\n", o->s); break;
			  case 7: printf("%s", o->b ? "true\n" : "false\n"); break;
			  default: printf("object [%p]\n", &o); break;
  };
		}

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

