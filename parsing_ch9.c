#include <stdio.h>
#include <stdlib.h>
#include <editline/readline.h>
#include "mpc.h"

typedef struct lval {
  int type; // what kind of lval is this?
  long num; // filled if this is a number
  char* err; // filled if this is an error
  char* sym; // filled if this is a symbol
  int count; // number of sexprs
  struct lval** cell; // the other lvals in the sexpr
} lval;

enum { LVAL_NUM, LVAL_ERR, LVAL_SYM, LVAL_SEXPR };

// returns an lval of a number type
lval* lval_num(long x){
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_NUM;
  v->num = x;
  return v;
}

// returns an lval of an error type
lval* lval_err(char* m){
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_ERR;
  v->err = malloc(strlen(m)+1);
  strcpy(v->err,m);
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
  case LVAL_SEXPR:
    for (int i = 0; i < v->count; i++){
      lval_del(v->cell[i]);
    }
    free(v->cell);
    break;
  }
  // free the lval itself
  free(v);
}

// 
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

  // assign children from mpc ast to the lval sexpr
  for (int i = 0; i < t->children_num; i++){
    if (strcmp(t->children[i]->contents,"(") == 0) { continue; }
    if (strcmp(t->children[i]->contents,")") == 0) { continue; }
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
  }  
}

// function for adding a newline after a call to lval_print
void lval_println(lval* v){ lval_print(v); putchar('\n'); }

lval* lval_eval(lval* v);

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

lval* builtin_op(lval* a, char* op){
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

lval* lval_eval_sexpr(lval* v){
  // eval children of the passed in lval
  for (int i = 0; i < v->count; i++){
    v->cell[i] = lval_eval(v->cell[i]);
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
  // if it's not a symbol then
  // delete the 0th lval we popped
  // and delete the original lval
  // and return an error.
  if (f->type != LVAL_SYM) {
    // 
    lval_del(f);
    lval_del(v);
    return lval_err("S-expression Does not start with symbol!");
  }

  // if we have a symbol call that symbol
  // as a builtin operator
  lval* result = builtin_op(v,f->sym);
  // delete the popped value, the v
  // original lval value is deleted
  // in the call to builtin_op
  lval_del(f);
  return result;
}

// take an lval, if it's an sexpr
// eval it, otherwise return it
lval* lval_eval(lval* v){
  if (v->type == LVAL_SEXPR) {
    return lval_eval_sexpr(v);
  }
  return v;
}

int main(int argc, char** argv) {
  mpc_parser_t* Number = mpc_new("number");
  mpc_parser_t* Symbol = mpc_new("symbol");
  mpc_parser_t* Sexpr = mpc_new("sexpr");
  mpc_parser_t* Expr = mpc_new("expr");
  mpc_parser_t* Lispy = mpc_new("lispy");

  mpca_lang(MPCA_LANG_DEFAULT,
  "                                                                \
    number   : /-?[0-9]+/ ;		                           \
    symbol   : '+' | '-' | '*' | '/' ;                             \
    sexpr    : '(' <expr>* ')' ;                                   \
    expr     : <number> | <symbol> | <sexpr> ;                     \
    lispy     : /^/ <expr>* /$/ ;                                  \
  ", Number, Symbol, Sexpr, Expr, Lispy);
 
  puts("Lispy Version 0.0.1");
  puts("Press Ctrl+c to Exit\n");
  while (1) {
    char* input = readline("lispy> ");
    add_history(input);
    mpc_result_t r;
    if (mpc_parse("<stdin>", input, Lispy, &r)) {
      lval* x = lval_eval(lval_read(r.output));
      lval_println(x);
      lval_del(x);
      mpc_ast_delete(r.output);
    } else {
      mpc_err_print(r.error);
      mpc_err_delete(r.error);       
    }
    free(input);
  }
  mpc_cleanup(4,Number,Symbol, Sexpr, Expr,Lispy);
  return 0;
}
