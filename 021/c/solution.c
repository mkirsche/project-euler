#include <stdio.h>
#include "factor.h"

int solve(int p);

#ifndef TESTING
int main () {
  int p;
  scanf("%d", &p);
  printf("%d\n", solve(p));
  return 0;
}
#endif

int solve(int p) {
  int sum = 0;
  int divs[p];
  fill_sum_divisors(divs, p);
  int i;
  for (i = 1; i < p; i++) {
    if (divs[i] < p
        // Amicable pairs have to be pairs of _different_ numbers
        && i != divs[i]
        && divs[divs[i]] == i) {
      sum += i;
    }
  }
  return sum;
}
