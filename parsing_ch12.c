// step 1 - add to grammar (produces ast)
// step 2 - add data format
// step 3 - add conversion from ast to new data format (produces lvals, lenvs, etc)
// step 4 - add evaluation (evaluating the feature in the context of this new data format)

#include <stdio.h>
#include <stdlib.h>
#include <editline/readline.h>
#include "mpc.h"

#define LASSERT(args, cond, fmt, ...)			\
  if (!(cond)) { \
  lval* err = lval_err(fmt,##__VA_ARGS__); \
    lval_del(args); \
    return err; \
  }

// takes a function, it's arguments,
// an index to check, and the type we expect
// tells us whether the argument at the index
// matches the type we expect
#define LASSERT_TYPE(func,args,index,expect) \
  LASSERT(args,args->cell[index]->type == expect, \
	  "Function '%s'  passed incorrect type for a argument %i."	\
	  "Got %s, Expect %s.",						\
	  func, index, ltype_name(args->cell[index]->type), ltype_name(expect));

#define LASSERT_NUM(func,args,num) \
  LASSERT(args,args->count == num, \
	  "Function '%s' passed incorrect number of arguments. "	\
	  "Got %s, Expected %i.",					\
	  func, args->count, num);

#define LASSERT_NOT_EMPTY(func,args,index) \
  LASSERT(args,args->cell[index]->count != 0, \
	  "Function '%s' passed {} for argument %i.", func, index);


struct lval;
struct lenv;
typedef struct lval lval;
typedef struct lenv lenv;

enum { LVAL_NUM, LVAL_ERR, LVAL_SYM,
       LVAL_FUN, LVAL_SEXPR, LVAL_QEXPR };

char* ltype_name(int t) {
  switch(t){
  case LVAL_FUN:
    return "Function";
  case LVAL_NUM:
    return "Number";
  case LVAL_ERR:
    return "Error";
  case LVAL_SYM:
    return "Symbol";
  case LVAL_SEXPR:
    return "S-Expression";
  case LVAL_QEXPR:
    return "Q-Expression";
  default:
    return "Unknown";
  }
}

typedef lval*(*lbuiltin)(lenv*,lval*);

struct lval {
  int type;

  // basic
  long num; 
  char* err; 
  char* sym;

  // function
  lbuiltin builtin;
  lenv* env;
  lval* formals;
  lval* body;

  // epxression
  int count; 
  lval** cell; 
};

struct lenv {
  lenv* par;
  int count;
  char** syms;
  lval** vals;
};

//* section lval functions *//

// returns an lval of a number type
lval* lval_num(long x){
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_NUM;
  v->num = x;
  return v;
}

// returns an lval of an error type
lval* lval_err(char* fmt, ...){
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_ERR;

  // create a va list and start
  // it with the right format str
  va_list va;
  va_start(va,fmt);
  // allocate 512 bytes
  v->err = malloc(512);
  // print the error str with 511 bytes
  // (i guess 1 byte for null terminator)
  vsnprintf(v->err,511,fmt,va);
  // reallocate space for the v error to
  // match this new string? i guess?
  // ah, it could be smaller, it's more
  // efficient this way
  v->err = realloc(v->err,strlen(v->err)+1);
  // cleanup and return new lval
  va_end(va);
  return v;
}

// returns an lval of a symbol type
lval* lval_sym(char* s){
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_SYM;
  v->sym = malloc(strlen(s)+1);
  strcpy(v->sym,s);
  return v;
}

// returns an lval for an sexpression type
// with the initial value for the contents
// set to NULL
lval* lval_sexpr(void){
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_SEXPR;
  v->count = 0;
  v->cell = NULL;
  return v;
}

// returns an lval for a quoted expression type
lval* lval_qexpr(void){
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_QEXPR;
  v->count = 0;
  v->cell = NULL;
  return v;
}

// returns an lval for the function type
// takes an lbuiltin type which is a func
// pointer
lval* lval_fun(lbuiltin func){
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_FUN;
  v->builtin = func;
  return v;
}

lenv* lenv_new();

lval* lval_lambda(lval* formals,lval* body){
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_FUN;
  v->builtin = NULL;
  v->env = lenv_new();
  v->formals = formals;
  v->body = body;
  return v;
}

void lenv_del(lenv* e);

// this function will take an lval, first
// delete any malloced data inside it, then
// recursively delete any lvals it points to
// in the same way, then free the original 
// lval itself
void lval_del(lval* v){
  // free any contents
  switch(v->type){
  case LVAL_NUM:
    break;
  case LVAL_ERR:
    free(v->err);
    break;
  case LVAL_SYM:
    free(v->sym);
    break;
  case LVAL_QEXPR:
  case LVAL_SEXPR:
    for (int i = 0; i < v->count; i++){
      lval_del(v->cell[i]);
    }
    free(v->cell);
    break;
  case LVAL_FUN:
    if (!v->builtin) {
      lenv_del(v->env);
      lval_del(v->formals);
      lval_del(v->body);
    }
    break;
  }
  // free the lval itself
  free(v);
}

lenv* lenv_copy(lenv* e);

lval* lval_copy(lval* v){
  lval* x = malloc(sizeof(lval));
  x->type = v->type;

  switch (v->type){
  case LVAL_FUN:
    if (v->builtin){
      x->builtin = v->builtin;
    } else {
      x->builtin = NULL;
      x->env = lenv_copy(v->env);
      x->formals = lval_copy(v->formals);
      x->body = lval_copy(v->body);
    }
    break;
  case LVAL_NUM:
    x->num = v->num;
    break;
  case LVAL_ERR:
    x->err = malloc(strlen(v->err)+1);
    strcpy(x->err,v->err);
    break;
  case LVAL_SYM:
    x->sym = malloc(strlen(v->sym)+1);
    strcpy(x->sym,v->sym);
    break;
  case LVAL_SEXPR:
  case LVAL_QEXPR:
    x->count = v->count;
    x->cell = malloc(sizeof(lval*) * x->count);
    for (int i = 0; i < x->count; i++){
      x->cell[i] = lval_copy(v->cell[i]);
    }
    break;
  }
  return x;
}

lenv* lenv_copy(lenv* e) {
  lenv* n = malloc(sizeof(lenv));
  n->par = e->par;
  n->count = e->count;
  n->syms = malloc(sizeof(char*) * n->count);
  n->vals = malloc(sizeof(lval*) * n->count);
  for (int i = 0; i < e->count; i++) {
    n->syms[i] = malloc(strlen(e->syms[i])+1);
    strcpy(n->syms[i],e->syms[i]);
    n->vals[i] = lval_copy(e->vals[i]);
  }
  return n;
}

//* section lenv functions *//

lenv* lenv_new(void){
  lenv* e = malloc(sizeof(lenv));
  e->par = NULL;
  e->count = 0;
  e->syms = NULL;
  e->vals = NULL;
  return e;
}

void lenv_del(lenv* e){
  for (int i = 0; i < e->count; i++){
    free(e->syms[i]);
    lval_del(e->vals[i]);
  }
  free(e->syms);
  free(e->vals);
  free(e);
}

lval* lenv_get(lenv* e,lval* k){
  for (int i = 0; i < e->count; i++){
    if (strcmp(e->syms[i],k->sym) == 0){
      return lval_copy(e->vals[i]);
    }
  }

  // if not symbol is found in this env, search the parent
  if (e->par) {
    return lenv_get(e->par,k);
  } else {
    return lval_err("Unbound Symbol '%s'", k->sym);
  }
  
  return lval_err("Unbound symbol! %s!", k->sym);
}

void lenv_put(lenv* e, lval* k, lval* v){
  // for each element in the env
  for (int i = 0; i < e->count; i++){
    // if we find the symbol we're looking for
    if (strcmp(e->syms[i],k->sym) == 0){
      // delete what stored in the env there
      lval_del(e->vals[i]);
      // create a copy of the new value and
      // store it there
      e->vals[i] = lval_copy(v);
      // return cutting short execution
      return;
    }
  }
  // if we didn't find the symbol
  // increase the count of items in
  // the environment
  e->count++;
  // reallocate memory for the vals and symbols arrays
  // using the new count
  e->vals = realloc(e->vals,sizeof(lval*) * e->count);
  e->syms = realloc(e->syms,sizeof(char*) * e->count);
  // at the end of the values array put a copy of lval for v
  e->vals[e->count-1] = lval_copy(v);
  // at the end of the symbols array allocate new space
  // for a string (which itself needs to be allocated)
  e->syms[e->count-1] = malloc(strlen(k->sym)+1);
  // copy the the string from k->sym into the allocated
  // space
  strcpy(e->syms[e->count-1],k->sym);
}

// global variable declaration 
void lenv_def(lenv* e, lval* k, lval* v){
  // keep getting the parent env
  // while there is one (not NULL)
  while (e->par) { e = e->par; }
  // then call lenv_put on the
  // that topmost parent with
  // the symbole lval (k)
  // and the value lval (v)
  lenv_put(e,k,v);
}

lval* lval_read_num(mpc_ast_t* t){
  errno = 0;
  long x = strtol(t->contents,NULL,10);
  return errno != ERANGE ?
    lval_num(x) : lval_err("invalid number");
}

// take in an lval v and a new lval x
// increase the count on v by 1
// allocate new space with the new, bigger, count.
// add the new lval to the end of the list
// return the original lval v with the new list
lval* lval_add(lval* v, lval* x){
  v->count++;
  v->cell = realloc(v->cell,sizeof(lval*) * v->count);
  v->cell[v->count-1] = x;
  return v;
}

lval* lval_read(mpc_ast_t* t){
  // if we find a number tag
  // send the ast to the read_num
  // and get a num lval
  if (strstr(t->tag,"number")) {
    return lval_read_num(t);
  }
  // if we find a symbol tag
  // create an lval_sym from the
  // contents.
  if (strstr(t->tag,"symbol")) {
    return lval_sym(t->contents);
  }

  // if were the root node
  // make an sexpr
  lval* x = NULL;
  if (strcmp(t->tag,">") == 0) {
    x = lval_sexpr();
  }
  
  // if we have an sexpr tag
  // make an sexpr
  if (strcmp(t->tag,"sexpr")) {
    x = lval_sexpr();
  }

  // if we have a qexpr tag
  // set x to be a new lval
  // representing a qexpr
  if (strstr(t->tag,"qexpr")) {
    x = lval_qexpr();
  }

  // assign children from mpc ast to the lval sexpr
  for (int i = 0; i < t->children_num; i++){
    if (strcmp(t->children[i]->contents,"(") == 0) { continue; }
    if (strcmp(t->children[i]->contents,")") == 0) { continue; }
    if (strcmp(t->children[i]->contents,"{") == 0) { continue; }
    if (strcmp(t->children[i]->contents,"}") == 0) { continue; }
    if (strcmp(t->children[i]->tag,"regex") == 0) { continue; }
    x = lval_add(x,lval_read(t->children[i]));
  }
  return x;
 }

void lval_print (lval* v);

// takes an lval (which contains a list of other lval pointers)
// prints open/close characters
// in between prints out the contents of each lval in the list
// of lval pointers
void lval_expr_print(lval* v, char open, char close){
  putchar(open);
  for (int i = 0; i < v->count; i++){
    lval_print(v->cell[i]);
    if (i != (v->count-1)){
      putchar(' ');
    }
  }
  putchar(close);
}

// the fucntion that takes an actual lval and prints it's
// contents
void lval_print(lval* v){
  switch(v->type){
  case LVAL_NUM:
    printf("%li",v->num);
    break;
  case LVAL_ERR:
    printf("Error: %s",v->err);
    break;
  case LVAL_SYM:
    printf("%s",v->sym);
    break;
  case LVAL_SEXPR:
    lval_expr_print(v,'(',')');
    break;
  case LVAL_QEXPR:
    lval_expr_print(v,'{','}');
    break;
  case LVAL_FUN:
    if (v->builtin){
      printf("<builtin>");
    } else {
      printf("(\\ ");
      lval_print(v->formals);
      putchar(' ');
      lval_print(v->body);
      putchar(')');
    }
    break;
  }  
}

// function for adding a newline after a call to lval_print
void lval_println(lval* v){ lval_print(v); putchar('\n'); }

lval* lval_pop(lval* v, int i) {
  lval* x = v->cell[i];
  memmove(&v->cell[i], &v->cell[i+1],
	  sizeof(lval*) * (v->count-i-1));
  v->count--;
  v->cell = realloc(v->cell,sizeof(lval*) * v->count);
  return x;
}

lval* lval_take(lval* v, int i){
  lval* x = lval_pop(v,i);
  lval_del(v);
  return x;
}

lval* lval_join(lval* x, lval* y){
  while(y->count){
    x = lval_add(x,lval_pop(y,0));
  }
  lval_del(y);
  return x;
}

lval* lval_eval(lenv* e,lval* v);

//* section builtins *//

lval* builtin_list(lenv* e,lval* a){
  a->type = LVAL_QEXPR;
  return a;
}

lval* builtin_head(lenv* e,lval* a){
  LASSERT(a, a->count == 1, "Function 'head' passed too many arguments! Got %i, Expected %i.", a->count, 1);
  LASSERT(a, a->cell[0]->type == LVAL_QEXPR, "Function 'head' passed incorrect type for argument 0. Got %s, Expected %s.", ltype_name(a->cell[0]->type), ltype_name(LVAL_QEXPR));
  LASSERT(a,a->cell[0]->count != 0,"Function 'head' passed {}!");

  // if we pass all error conditions
  // we take/pop the first elem
  // then we delete all elements
  // that are not that first elem
  // of our lval (a) and return
  // the value from lval (a)
  lval* v = lval_take(a,0);
  while (v->count > 1){
    lval_del(lval_pop(v,1));
  }
  return v;
}

lval* builtin_tail(lenv* e,lval* a){
  LASSERT(a,a->count == 1, "Function 'tail' passed too many arguments!");
  LASSERT(a,a->cell[0]->type == LVAL_QEXPR,"Function 'tail' passed incorrect types!");
  LASSERT(a,a->cell[0] != 0,"Function 'tail' passed {}!");
  
  // if we pass all error conditions
  // delete the first value, then
  // return the lval
  lval* v = lval_take(a,0);
  lval_del(lval_pop(v,0));
  return v;
}

lval* builtin_eval(lenv* e,lval* a){
  LASSERT(a,a->count == 1,"Function 'eval' passed too many arguments!");
  LASSERT(a,a->cell[0]->type == LVAL_QEXPR,"Function 'eval' passed incorrect type!");

  lval* x = lval_take(a,0);
  x->type = LVAL_SEXPR;
  return lval_eval(e,x);
}

// code when a function lval called
lval* lval_call(lenv* e, lval* f, lval* a) {
  // if it's a builtin function call builtin
  // with the arguments
  if (f->builtin) { return f->builtin(e,a); }

  // get the given number of arguments
  // and the total number we will need
  int given = a->count;
  int total = f->formals->count;
  
  // while we have arguments left in a
  while (a->count) {
    // if the number of arguments
    // the function wants is 0
    // throw an error (ok cool)
    if (f->formals->count == 0) {
      lval_del(a);
      return lval_err("Function passed too many arguments. "
		      "Got %i, Expected %i.", given, total);
    }

    // otherwise pop out the symbol from the function definition
    lval* sym = lval_pop(f->formals,0);

    //deal with specail case & multiple args

    if (strcmp(sym->sym, "&") == 0) {
      // make sure that there is only 1 symbol
      // left to pop after this.
      if (f->formals->count != 1){
	lval_del(a);
	return lval_err("Function format invalid. "
			"Symbol '&' not followed by single symbol.");
      }
      // bind that next symbol to the remaining args as a list
      lval* nsym = lval_pop(f->formals,0);
      lenv_put(f->env,nsym,builtin_list(e,a));
      lval_del(sym);
      lval_del(nsym);
      break;
    }
    
    // pop out the value from the argument
    lval* val = lval_pop(a,0);
    // put this binding sym = value into the env for the function
    lenv_put(f->env,sym,val);
    // cleanup both lvals
    lval_del(sym);
    lval_del(val);
  }

  // delete the original input of given arguments
  lval_del(a);

  // if & special case stays in formal list but no args were provided
  // to bind to it, bind it to the empty list
  if (f->formals->count > 0 && strcmp(f->formals->cell[0]->sym,"&") == 0) {
    if (f->formals->count != 2) {
      return lval_err("Function format invalid. " "Symbol '&' not followed by single symbol.");
    }
    // pop and delete & symbol
    lval_del(lval_pop(f->formals,0));
    // pop next symbol and create empty list
    lval* sym = lval_pop(f->formals,0);
    lval* val = lval_qexpr();
    // in the function's env bind the symbol to the empty list
    lenv_put(f->env,sym,val);
    // cleanup
    lval_del(sym);
    lval_del(val);
  }

  // if there are no arguments, set the parent env to e, and call the function
  // body
  if (f->formals->count == 0) {
    f->env->par = e;
    return builtin_eval(f->env,lval_add(lval_sexpr(),lval_copy(f->body)));
  // otherwise return a copy of the function. partially evaluated. (i.e. with n args bound)
  } else {
    return lval_copy(f);
  }
}

lval* builtin_join(lenv* e,lval* a){
  // for every lval this lval a
  // make sure that each is a qexpr
  for (int i = 0; i < a->count; i++){
    LASSERT(a,a->cell[i]->type == LVAL_QEXPR,"Function 'join' passed incorrect type.");
  }

  // if we made it past error conds
  lval* x = lval_pop(a,0);
  // keep removing and joining
  // sub lvals until none left
  while (a->count){
    x = lval_join(x,lval_pop(a,0));
  }
  // delete the original lval
  // return the joined value
  lval_del(a);
  return x;
}

/* lval* builtin_def(lenv* e, lval* a){ */
/*   // make sure the passed in lval is a qexpr (it's a qexpr of symbols). */
/*   LASSERT(a,a->cell[0]->type == LVAL_QEXPR,"Function 'def' passed incorrect type!"); */

/*   // get the first lval in the */
/*   // this should be a q expr with */
/*   // a list of symbol names */
/*   lval* syms = a->cell[0]; */

/*   // for each symbol  */
/*   for (int i = 0; i < syms->count; i++) { */
/*     // make sure the type is set to symbol */
/*     LASSERT(a, syms->cell[i]->type == LVAL_SYM,"Function 'def' cannot define non-symbol."); */
/*   } */

/*   LASSERT(a,syms->count == a->count-1,"Function 'def' must have same number of symbols and values."); */

/*   // if we pass the error conditions */
/*   // put each symbol and value into */
/*   // the environment */
/*   for (int i = 0; i <syms->count; i++) { */
/*     lenv_put(e,syms->cell[i],a->cell[i+1]); */
/*   } */

/*   lval_del(a); */
/*   return lval_sexpr(); */
/* } */

lval* builtin_lambda(lenv* e, lval* a) {
  LASSERT_NUM("\\",a,2);
  LASSERT_TYPE("\\",a,0,LVAL_QEXPR);
  LASSERT_TYPE("\\",a,1,LVAL_QEXPR);

  for (int i = 0; i < a->cell[0]->count; i++) {
    LASSERT(a,(a->cell[0]->cell[i]->type == LVAL_SYM),
	    "Cannot define non-symbol. Got %s, Expected %s.",
	    ltype_name(a->cell[0]->cell[i]->type),
	    ltype_name(LVAL_SYM));
  }
  lval* formals = lval_pop(a,0);
  lval* body = lval_pop(a,0);
  lval_del(a);
  return lval_lambda(formals,body);
}

// definition for defining variables in the env
lval* builtin_var(lenv* e, lval* a, char* func) {
  // assert that symbol associated with "func"
  // is getting an lval a assigned to it that
  // has a q expression as its first value
  LASSERT_TYPE(func,a,0,LVAL_QEXPR);

  // then get every part of the q expr
  // and iterate over it
  lval* syms = a->cell[0]; 
  for (int i = 0; i < syms->count; i++) {
    // the type of each part of the qexpr
    // needs to be a symbol
    LASSERT(a,(syms->cell[i]->type == LVAL_SYM),
	    "Function '%s' cannot define non-symbol."
	    "Got %s, Expect %s.",
	    func,
	    ltype_name(syms->cell[i]->type),
	    ltype_name(LVAL_SYM));
  }
  // the number of symbols inside the 0th cell
  // of a should equal to the count of a,
  LASSERT(a,(syms->count == a->count-1),
	  "Function '%s' passed too many arguments for symbols. "
	  "Got %i, Expect %i", func, syms->count, a->count-1);

  // for each symbol
  for (int i = 0; i < syms->count; i++) {
    // if any is a def call def with the env
    if (strcmp(func,"def") == 0) {
      lenv_def(e,syms->cell[i],a->cell[i+1]);
    }
    // if any is a put call put with the env
    if (strcmp(func,"=") == 0) {
      lenv_put(e,syms->cell[i],a->cell[i+1]);
    }
  }
  // delete the lval a
  lval_del(a);
  // return an empty s expr
  return lval_sexpr();
}

lval* builtin_def(lenv* e, lval* a) {
  return builtin_var(e,a,"def");
}

lval* builtin_put(lenv* e, lval* a) {
  return builtin_var(e,a,"=");
}

lval* builtin_op(lenv* e, lval* a, char* op){
  // make sure all arguments are numbers
  // if we find a non number delete the
  // lval and return an error
  for (int i = 0; i < a->count; i++){
    if (a->cell[i]->type != LVAL_NUM){
      lval_del(a);
      return lval_err("Cannot operate on non-number!");
    }
  }
  
  // if all elements are numbers,
  // get the first one.
  lval* x = lval_pop(a,0);
  
  // if it's a - and there's no arguments do unary negation
  if ((strcmp(op,"-") == 0) && a->count == 0){
    x->num = -x->num;
  }

  // while there are elems remaning
  while (a->count > 0){
    // pop next elem
    lval* y = lval_pop(a,0);
    if (strcmp(op,"+") == 0) { x->num += y->num; }
    if (strcmp(op,"-") == 0) { x->num -= y->num; }
    if (strcmp(op,"*") == 0) { x->num *= y->num; }
    if (strcmp(op,"/") == 0) {
      if (y->num == 0){
	lval_del(x);
	lval_del(y);
	x = lval_err("Division By Zero!");
	break;	
      }
      x->num /= y->num;
    }
    // delete the popped lval
    lval_del(y);
  }
  // delete the passed in lval
  lval_del(a);
  // return the accumulated number
  return x;
}

lval* builtin_add(lenv* e, lval* a) { return builtin_op(e,a,"+"); }
lval* builtin_sub(lenv* e, lval* a) { return builtin_op(e,a,"-"); }
lval* builtin_mul(lenv* e, lval* a) { return builtin_op(e,a,"*"); }
lval* builtin_div(lenv* e, lval* a) { return builtin_op(e,a,"/"); }

void lenv_add_builtin(lenv* e, char* name, lbuiltin func) {
  lval* k = lval_sym(name);
  lval* v = lval_fun(func);
  lenv_put(e,k,v);
  lval_del(k);
  lval_del(v);
}

void lenv_add_builtins(lenv* e) {
  lenv_add_builtin(e,"list",builtin_list);
  lenv_add_builtin(e,"head",builtin_head);
  lenv_add_builtin(e,"tail",builtin_tail);
  lenv_add_builtin(e,"eval",builtin_eval);
  lenv_add_builtin(e,"join",builtin_join);
  lenv_add_builtin(e,"def",builtin_def);
  lenv_add_builtin(e,"=",builtin_put);
  lenv_add_builtin(e,"\\",builtin_lambda);
  lenv_add_builtin(e,"+",builtin_add);
  lenv_add_builtin(e,"-",builtin_sub);
  lenv_add_builtin(e,"*",builtin_mul);
  lenv_add_builtin(e,"/",builtin_div);
}

//* lval eval section *//

lval* lval_eval_sexpr(lenv* e,lval* v){
  // eval children of the passed in lval
  for (int i = 0; i < v->count; i++){
    v->cell[i] = lval_eval(e,v->cell[i]);
  }
  
  // for all children if any is an error
  // then pop that one out and return it
  for (int i = 0; i < v->count; i++){
    if (v->cell[i]->type == LVAL_ERR) {
      return lval_take(v,i);
    }
  }
  
  // if the expression has nothing then
  // return the lval as it was given
  if (v->count == 0) {
    return v;
  }

  // if the expression has 1 thing in it
  // just return that one lval
  if (v->count == 1) {
    return lval_take(v,0);
  }

  // if we make it part all the above
  // and have a simplified, valid, lval
  // expr then get the first elem of the
  // sexpr
  lval* f = lval_pop(v,0);
  
  // if the type of the first value is
  // not a function delete it and
  // the original lval and return an error
  if (f->type != LVAL_FUN) {
    // 
    lval_del(f);
    lval_del(v);
    return lval_err("first elem is not a function!");
  }

  // if it is a function call it with the env,
  // and the original lval? then cleanup
  lval* result = lval_call(e,f,v);
  lval_del(f);
  return result;
}

// take an lval, if it's an sexpr
// eval it. if it's a symbol then
// get the lval from the env,
// otherwise return it
lval* lval_eval(lenv* e,lval* v){
  if (v->type == LVAL_SYM) {
    lval* x = lenv_get(e,v);
    lval_del(v);
    return x;
  }
  if (v->type == LVAL_SEXPR) {
    return lval_eval_sexpr(e,v);
  }
  return v;
}

int main(int argc, char** argv) {
  mpc_parser_t* Number = mpc_new("number");
  mpc_parser_t* Symbol = mpc_new("symbol");
  mpc_parser_t* Sexpr = mpc_new("sexpr");
  mpc_parser_t* Qexpr = mpc_new("qexpr");
  mpc_parser_t* Expr = mpc_new("expr");
  mpc_parser_t* Lispy = mpc_new("lispy");

  mpca_lang(MPCA_LANG_DEFAULT,
  "                                                                \
    number   : /-?[0-9]+/ ;		                           \
    symbol   : /[a-zA-Z0-9_+\\-*\\/\\\\=<>!&]+/ ;	   \
    sexpr    : '(' <expr>* ')' ;                                   \
    qexpr    : '{' <expr>* '}' ;                                   \
    expr     : <number> | <symbol> | <sexpr> | <qexpr> ;           \
    lispy     : /^/ <expr>* /$/ ;                                  \
  ", Number, Symbol, Sexpr, Qexpr, Expr, Lispy);
 
  puts("Lispy Version 0.0.1");
  puts("Press Ctrl+c to Exit\n");

  lenv* e = lenv_new();
  lenv_add_builtins(e);
  
  while (1) {
    char* input = readline("lispy> ");
    add_history(input);
    
    mpc_result_t r;
    if (mpc_parse("<stdin>", input, Lispy, &r)) {
      
      lval* x = lval_eval(e, lval_read(r.output));
      lval_println(x);
      lval_del(x);
      
      mpc_ast_delete(r.output);
    } else {
      mpc_err_print(r.error);
      mpc_err_delete(r.error);       
    }
    
    free(input);    
  }
  lenv_del(e);
  mpc_cleanup(6,Number, Symbol, Sexpr, Qexpr, Expr, Lispy);
  return 0;
}
