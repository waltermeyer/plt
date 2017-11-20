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

obj *obj_findkey(obj *op, char *key) {
  // get first key from head
  obj *o = op->next;
  // search for key
  while (o != NULL) {
    printf("KEY: %s\n", o->k);
    if (strcmp(o->k, key) == 0) {
       printf("FOUND: %s\n", o->k);
       return o;
    }
    o = o->next;
  }
  return NULL;
}

int print_k(obj *o) {
  printf("%d\n", o->v_typ);
  switch(o->v_typ) {
    case 3: printf("%d\n", o->i); break;
    case 4: printf("%f\n", o->f); break;
    //case 5: printf("%d\n", o->o); break;
    case 6: printf("%s\n", o->s); break;
    case 7: printf("%s", o->b ? "true" : "false"); break;
    case 8: printf("null\n"); break;
  };
  return 0;
}

#ifdef BUILD_TEST
int main () {
  obj *o = (obj *) malloc(sizeof(obj));
  obj *o2 = (obj *) malloc(sizeof(obj));
  o->next = o2;
  obj *tmp = o->next;
  tmp->k = "mykey";
  tmp->v_typ = 6;
  //o->v.s  = "hello";
  tmp->i = 33;
  obj_findkey(&o, "mykey");
  free(o);
}
#endif

