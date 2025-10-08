#include <stdio.h>

void print(const char *msg) {
  puts(msg);
}

void printInt(const int i) {
  printf("%i", i);
}

int _43(const int a, const int b) {
  return a + b;
}

int _45(const int a, const int b) {
  return a - b;
}

int _42(const int a, const int b) {
  return a * b;
}

int _47(const int a, const int b) {
  return a / b;
}
