#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// The arr int
typedef struct arr_int {
  int len;
  int typ;
  int *i_a;
} arr_int;

// The arr float
typedef struct arr_flt {
  int len;
  int typ;
  double *f_a;
} arr_flt;

// The arr string
typedef struct arr_str {
  int len;
  int typ;
  char **s_a;
} arr_str;


// The arr bool
typedef struct arr_bool {
  int len;
  int typ;
  bool *b_a;
} arr_bool;


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
  struct arr_int *i_a;
  struct arr_flt *f_a;
  struct arr_str *s_a;
  struct arr_bool *b_a; 
} obj;


// Object Key Printing
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

/* Checks realloc and makes sure memory null */
char *xrealloc(char *ptr, size_t sz){
	char *temp = (char *)realloc(ptr, sz);
	if (temp == NULL){
		printf("Runtime Error: Failed to realloc");
	}
	else {
		ptr = temp;
		memset(ptr, 0 , sz);
	}
	return ptr;
}
 

/* Grows buff by appended cpy_buff */
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
	
	memcpy(buff,old_buff, buff_sz);
	//printf("After inserting buff: %s \n", new_buff);
	free(old_buff);
	
	memcpy(buff + buff_sz -1, cpy_buff, cpy_len);
	//printf("After inserting cpy_buff : %s \n", new_buff);
	//printf("-- END OF FILL BUFF --\n");
	return buff;
}


char *arr_stringify(void *arr){
	/* Cast to each arr type */  	
	struct arr_int *arr_i = *(arr_int **)(arr);
	struct arr_flt *arr_f = *(arr_flt **)(arr);
	struct arr_str *arr_s = *(arr_str **)(arr);
	struct arr_bool *arr_b = *(arr_bool **)(arr);

	//int typ = arr_i->typ;
	int len = arr_i->len;
	int typ = arr_i->typ;
  	
	printf("Type :  %d \n " , typ);
	printf("Length :  %d \n ", len);
	
	char *buff;
	//size_t buff_len;

	char *cpy_buff;
	size_t cpy_len;

	cpy_buff = (char *)malloc(sizeof(char));
	
	buff = (char *)malloc(sizeof(char));
	memcpy(buff, "", 1);
	
	cpy_len = 2;
	cpy_buff = xrealloc(cpy_buff, sizeof(char)*(cpy_len+1));
 	memcpy(cpy_buff, " [", cpy_len+1);	
	buff = fill_buff(buff, cpy_buff);
	

	switch(typ){
		case 3:
			printf("GOT HERE \n");
			for (int i=1; i< len+1; i++){
				printf("Value %d is %d \n", i, (arr_i->i_a[i]));
				cpy_len = snprintf(NULL, 0 , "%d" , (arr_i->i_a[i]));  
				cpy_buff = xrealloc(cpy_buff, sizeof(char)*(cpy_len+1));
				snprintf(cpy_buff,cpy_len+1, "%d",  (arr_i->i_a[i]));
				buff = fill_buff(buff, cpy_buff);
				if (i!=len){
					buff = fill_buff(buff, " , ");
				}
				printf("%s \n " , buff);
			}
			break;
		case 4: 
			for (int i=1; i< len+1; i++){
				printf("Value %d is %lf \n", i, (arr_f->f_a[i]));
				cpy_len = snprintf(NULL, 0 , "%lf" , (arr_f->f_a[i]));  
				cpy_buff = xrealloc(cpy_buff, sizeof(char)*(cpy_len+1));
				snprintf(cpy_buff,cpy_len+1, "%lf",  (arr_f->f_a[i]));
				buff = fill_buff(buff, cpy_buff);
				if (i!=len){
					buff = fill_buff(buff, " , ");
				}
				printf("%s \n " , buff);
			}
			break;
      		case 6: 
			for (int i=1; i< len+1; i++){
				printf("Value %d is %d \n", i, (arr_s->s_a[i]));
				cpy_len = snprintf(NULL, 0 , "\"%s\"" , (arr_s->s_a[i]));  
				cpy_buff = xrealloc(cpy_buff, sizeof(char)*(cpy_len+1));
				snprintf(cpy_buff,cpy_len+1, "\"%s\"", (arr_s->s_a[i])); 
				buff = fill_buff(buff,cpy_buff);
				if (i!=len){
					buff = fill_buff(buff, " , ");
				}
				printf("%s \n", buff);
			}
			break;
      		case 7:
			for (int i=1; i< len+1; i++){
				printf("Value %d is %s \n", i, ((arr_b->b_a[i]) ? "true" : "false"));
				cpy_len= snprintf(NULL, 0 , "%s" , (arr_b->b_a[i]) ? "true" : "false");  
				cpy_buff = xrealloc(cpy_buff, sizeof(char)*(cpy_len+1));
				snprintf(cpy_buff, cpy_len+1,"%s", (arr_b->b_a[i]) ? "true" : "false"); 
				buff = fill_buff(buff, cpy_buff);
				if (i!=len){
					buff = fill_buff(buff, " , ");
				}
				printf("%s \n " , buff);
			}
			break;
	}
	
	cpy_len = 2;
	cpy_buff = xrealloc(cpy_buff, sizeof(char)*(cpy_len+1));
 	memcpy(cpy_buff, " ]", cpy_len+1);
	buff = fill_buff(buff, cpy_buff);
	
	free(cpy_buff);
	//char *buff = "foo";
	return buff;
}


/* Populates buffer with properly formatted key from object */
char *string_key(obj *o, char *buff){
	char *cpy_buff;
	char *k;
	int len;
	
	len = 2;
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
	
	len = 2;
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

/* Populates buff with properly formatted value from object */
char *string_val(obj *o, char *buff){
	char *cpy_buff;
	int len;

	struct arr_int *i_a;
	void *arr;
	
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
		len = snprintf(NULL, 0 , "\"%s\"" , o->s);  
		cpy_buff = malloc(sizeof(char)*(len+1));
		snprintf(cpy_buff,len+1, "\"%s\"", o->s); 
		break;
	  case 7:
		len= snprintf(NULL, 0 , "%s" , o->b ? "true" : "false");  
		cpy_buff = malloc(sizeof(char)*(len+1));
		snprintf(cpy_buff, len+1,"%s", o->b ? "true" : "false"); 
		break;
	  case 8:
  		i_a = o->i_a;
		arr = *(void **) i_a;
		printf("%d \n" , i_a->len);
		cpy_buff = arr_stringify(arr);
		break;
	  default:
		cpy_buff = malloc(sizeof(char));
	};
	buff = fill_buff(buff, cpy_buff);
	//printf(" Print buff : %s \n" , cpy_buff);
	free(cpy_buff);		
	len = 4;
	cpy_buff = malloc(sizeof(char)*len);
	memcpy(cpy_buff, " , ", len);
	buff = fill_buff(buff, cpy_buff);
	//printf(" copy buff after , %s \n" , cpy_buff);
	//printf(" buff after kv , %s \n", buff);
	free(cpy_buff);
	
	return buff;
}


/* Appends properly formatted string object to buff */
char *rec_stringify(obj *o, char *buff){

	size_t cpy_len;
	//char *cpy_buff;
	const char *cpy_buff;
	size_t buff_len;
	char *old_buff;

	o = o->next;
	buff_len = (strlen(buff)+1)*sizeof(char);
	old_buff = malloc(buff_len);	
	memset(old_buff, 0, buff_len);
	old_buff = memcpy(old_buff, buff, buff_len);
		
	cpy_buff = "{ ";
	cpy_len = (strlen(cpy_buff) + 1)*sizeof(char);
	buff = xrealloc(buff, (buff_len + cpy_len)*sizeof(char));
	memcpy(buff, old_buff, buff_len);
	memcpy(buff + buff_len -1, cpy_buff, cpy_len);

	free(old_buff);	

	while (o != NULL) {
		if (o->v_typ == 5){
			if(o->k){	
				buff = string_key(o, buff);
			}
			buff = rec_stringify(o->o, buff);
		}
		else{
			buff = string_key(o, buff);
			buff = string_val(o, buff);	
		}
		o = o->next;
	}
	
	/* Remove comma after last value in object */
	buff_len = (strlen(buff)+1)*sizeof(char);

	old_buff = malloc(buff_len-2);	
	memset(old_buff, 0, buff_len-2);
	old_buff = memcpy(old_buff, buff, buff_len-3);	
	
	/* Add closing parenthesis of object */
	buff_len = (strlen(old_buff)+1)*sizeof(char);

	cpy_buff = "},";
	cpy_len = (strlen(cpy_buff) + 1)*sizeof(char);
	
	buff = xrealloc(buff, (buff_len + cpy_len));
	memcpy(buff, old_buff, buff_len);
	memcpy(buff + buff_len - sizeof(char), cpy_buff, cpy_len);

	free(old_buff);		
	//printf("==== Object before rec_stringify returns =======\n %s \n ==========", buff);
	
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

	//printf("==== Object before cleaning up =======\n %s \n ==========", pre_buff);
	
	/* Remove comma after Object */
	buff_len = (strlen(pre_buff)+1)*sizeof(char);

	buff = malloc(buff_len-1);
	memset(buff, 0 , buff_len-1);
	
	buff = memcpy(buff, pre_buff, buff_len-2);	
	//printf("==== Object after cleaning up =======\n %s \n ==========", buff);
	free(pre_buff);		

	return buff;
}


/* Add key and value to the beginning of an object
 * Accepts: key, typ, value
 * Returns : Object with added key
 */
obj *obj_addkey(obj *o, char *key, int typ, void *val) {
	/* Save what o currently sexts as next */
	obj *o_next = o->next;
	/* Malloc object */
  	obj *o_new = (obj *) malloc(sizeof(obj));
	o_new->k = key;
  	o_new->v_typ = typ;
  	o_new->next = o_next;

	o->next = o_new;
	
	int l;
	char *s;
	switch(o_new->v_typ) {
  	  case 3: o_new->i = *((int *) val); break;
  	  case 4: o_new->f = *((double *) val); break;
  	  case 5: o_new->o = (obj *) val; break;
  	  case 6: 
		  l = strlen((const char *)(*(void **)val)) + 1;
		  s = (char *)malloc(sizeof(l));
		  memcpy(s, (const char *)(*(void **)val), l);
		  o_new->s = s;
		  break;
  	  case 7: o_new->b = *((bool *) val); break;
  	};
	return o;
}

 
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
      else if (o->v_typ == 5) {
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
      case 8: o->i_a = (arr_int *)v; break;
      case 9: o->f_a = (arr_flt *)v; break;
      case 10: o->s_a = (arr_str *)v; break;
      case 11: o->b_a = (arr_bool *)v; break;
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
    case 8: return (void *)o->i_a;
    case 9: return (void *)o->f_a;
    case 10: return (void *)o->s_a;
    case 11: return (void *)o->b_a;
  };
  return NULL;
}

/*
 * Get a key value's type from an Object
 */
int obj_gettyp(obj *o) {
  return o->v_typ;
}

/*
 * Get the length of an array
 */
int arr_length(void *arr) {
  struct arr_int *arr_i = *(arr_int **)(arr);
  return arr_i->len;
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

  obj *o_new = (obj *) malloc(sizeof(obj)); // head
  obj *o2_new = (obj *) malloc(sizeof(obj));
  o_new->k = NULL;
  o_new->next = o2_new;

  o2_new->k = "key2";
  o2_new->i = 2;
  o2_new->v_typ = 3;
  o2_new->next = NULL;
  
  buff = obj_stringify(o_new);
  printf("====THE OBJECT NEW=====\n\n %s \n\n ===============\n" , buff);
  
  /* Array Stringify */
  printf("Before arr_int \n"); 
  struct arr_int *ia = (arr_int *) malloc(sizeof(arr_int));
  printf("After arr_int \n");
  //ia->len = 4;

  //ia->typ = 3;
  printf("After setting typ and len \n");
  //
  int *arr = (int *) malloc(4*sizeof(int));
  
  arr[0] = 1;
  arr[1] = 2;
  arr[2] = 3;
  arr[3] = 4;
 
  ia->len = 4;
  ia->typ = 3;
  ia->i_a = arr;

  printf("%d \n " , arr[1]);

  //printf("%d\n" , ia->len); 
  //printf("Strinfied array %s \n", s);
  //free(s);
  //free(ia->i_a);
  void *int_array = *(void **)(ia);
  char *s = arr_stringify(int_array);
  printf("%s\n", s);

  free(int_array);
  free(s);
  free(ia->i_a);
  free(ia);
  free(buff);

  free(o_new);
  free(o2_new);

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

