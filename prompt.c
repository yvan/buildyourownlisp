#include <stdio.h>

static char input [2048];

int main (int argc, char** argv){
  
  puts("lispy version 0.0.1");
  puts("press ctrl+c to exit\n");

  while (1){
    fputs("lispy> ", stdout);
    fgets(input,2048, stdin);
    printf("no you're a %s", input);
  }
  
  return 0;
}
