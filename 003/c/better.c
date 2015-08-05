#include <stdio.h>
#include <stdbool.h>

#include "factors.h"
#include "primes.h"

long solve(long p);

#ifndef TESTING
int main () {
    printf("%ld\n", solve(600851475143));
}
#endif

long solve(long p) {
    long i;
    for (i = p/2; i >= 1; i++){
        if (is_prime(i) && is_factor_l(i, p)){
            return i;
        }
    }
    return 1;
}