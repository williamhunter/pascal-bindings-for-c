/*
 * Filename: main1.c
 *
 */

#include <stdio.h>
#include "sum.h"

int main(void)
{
    int a = 17;
    int b = 19;
    printf("From C: The sum of %d and %d is %d\n", a, b, sum(a,b));

    return 0;
}
