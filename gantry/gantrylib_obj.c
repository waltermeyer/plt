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

/*
 * Get a key value from an Object
 */
void *obj_getkey(obj *o, int t, void *v) {
    switch(o->v_typ) {
      case 3: return (void *)&o->i;
      case 4: return (void *)&o->f;
      case 5: return (void *)&o->o;
      case 6: return (void *)&o->s;
      case 7: return (void *)&o->b;
    };
}

int print_k(obj *o) {
  if (o == NULL)
    return 1;
  switch(o->v_typ) {
    case 3: printf("%d\n", o->i); break;
    case 4: printf("%f\n", o->f); break;
    //case 5: printf("%d\n", o->o); break;
    case 6: printf("%s\n", o->s); break;
    case 7: printf("%s", o->b ? "true\n" : "false\n"); break;
  };
  return 0;
}

/*
 * Recursively search for a key in an object
 */
obj *obj_findkey(obj *o, char *keys) {
  char *k, *keys_dup;
  keys_dup = strdup(keys);

  // parse a key to search for
  k = strsep(&keys_dup, ".");

  printf("Searching: %s\n", keys);
  printf("Current Key: %s\n", k);

  // search for keys within an object
  o = o->next; // get first key from head struct
  while (o != NULL) {
    if (strcmp(o->k, k) == 0) {
      // found our key
      if (strlen(k) == strlen(keys)) {
        printf("Found the key!\n");
	free(k);
	return o;
      }
      // continue nested key search
      else {
        printf("Found object that nests the key\n");
	printf("Now searching for: %s\n", keys_dup);
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
    if (o->v_typ == 6)
      free(o->s);
    // Set key type
    o->v_typ = t;
    // Set key value
    switch(o->v_typ) {
      case 3: o->i = *(int *)v; break;
      case 4: o->f = *(double *)v; break;
      case 5: o->o =  (obj *)v; break;
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

//#ifdef BUILD_TEST
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

  obj *tmp;
  // Find Key
  tmp = obj_findkey(o, "key2");
  tmp = obj_findkey(o, "key3.nestedkey4.nestedkey5");
  tmp = obj_findkey(o, "key3.nestedkey4.nestedkey6");
  obj_findkey(o, "key3.nestedkey4");

  // Get its Value
  // Change its Value
  // Find it again...
  // Get its Value again...

// printf("%s\n", (char *)*(void **)tmp);

//  printf("%lf\n", *(double *)tmp);

  // change the key's type to int
//  obj_assign(o, 3, (void *)&(int){1});
//  printf("%d\n", *(int *)tmp);

//  tmp = obj_findkey(o, "key3.nestedkey4.nestedkey5");

  free(o);
  free(o2);
  free(o3);
  free(o4);
  free(o4_head);
  free(o5);
  free(o5_head);
  free(o6);
}
//#endif

