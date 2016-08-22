/*
 * Filename: main3.c
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include "arrfun.h"

int main(void)
{
    int i;

    // two vectors
    double a[] = {1,2,3,4,5};
    double b[] = {6,2,4,7,5};
    double c[] = {0,0,0,0,0};

    // determine length of vectors
    int veclen = sizeof(a) / sizeof(double);

    // multiply vectors element-wise
    multelem(a, b, c, veclen);

    // print result 1
    printf("From C: The multiplied elements are:\n");
    for (i = 0; i < veclen; i++)
    {
	     printf("Item %d: %f\n", i, c[i]);
    }

    // print result 2
    printf("From C: The sum of the elements is:\n");
    printf("Sum: %f\n", sumelem(c, veclen));

    return 0;
}
