#include <stdio.h>

void hw5x(){
  int a = 0;
  while(a<5){
    puts("hw");
    a=a+1;
  }
}

int main(int argc, char** argv) {
  for (int i=0;i<5;i++){
    puts("Hello world!");
  }
  puts("-----");
  int a = 0;
  while(a<5){
    puts("Hello world!");
    a=a+1;
  }
  puts("-----");
  hw5x();
}
