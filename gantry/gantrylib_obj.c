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
 * Recursively search for a key in an object
 */
obj *obj_findkey(obj *o, char *keys) {
  char *k, *keys_dup;
  keys_dup = strdup(keys);

  // parse a key to search for
  k = strsep(&keys_dup, ".");

  // search for keys within an object
  o = o->next; // get first key from head struct
  while (o != NULL) {
    if (strcmp(o->k, k) == 0) {
      // found our key
      if (strlen(k) == strlen(keys)) {
        free(k);
	return o;
      }
      // continue nested key search
      else {
        o = obj_findkey(o->o, keys_dup);
        free(k);
	return o;
      }
    }
    o = o->next;
  }

  printf("Did not find: %s\n", k);
  free(k);
  return NULL;
}

int print_k(obj *o) {
  switch(o->v_typ) {
    case 3: printf("%d\n", o->i); break;
    case 4: printf("%f\n", o->f); break;
    //case 5: printf("%d\n", o->o); break;
    case 6: printf("%s\n", o->s); break;
    case 7: printf("%s", o->b ? "true\n" : "false\n"); break;
    case 8: printf("null\n"); break;
  };
  return 0;
}

#ifdef BUILD_TEST
int main () {
  obj *o = (obj *) malloc(sizeof(obj));
  obj *o2 = (obj *) malloc(sizeof(obj));
  obj *o3 = (obj *) malloc(sizeof(obj));
  obj *o4_head = (obj *) malloc(sizeof(obj));
  obj *o4 = (obj *) malloc(sizeof(obj));
  obj *o5_head = (obj *) malloc(sizeof(obj));
  obj *o5 = (obj *) malloc(sizeof(obj));
  o->next = o2;
  o2->next = o3;

  o->k = NULL;
  o2->k = "key2";
  o2->i = 2;
  o3->k = "key3";
  o3->o = o4_head;
  o4_head->next = o4;
  o4->k = "nestedkey4";
  o4->o = o5_head;
  o5_head->next = o5;
  o5->k = "nestedkey5";
  o5->i = 5;

  obj_findkey(o, "key2");
  obj_findkey(o, "key3.nestedkey4");
  obj_findkey(o, "key3.nestedkey4.nestedkey5");

  free(o);
  free(o2);
  free(o3);
  free(o4);
  free(o4_head);
  free(o5);
  free(o5_head);
}
#endif

