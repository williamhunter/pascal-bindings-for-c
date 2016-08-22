/*
 * Filename: main.c
 *
 */

#include <stdio.h>
#include "fibonacci.h"

int main(void)
{
    int i, j;
    int val = 13;
    printf("The %dth Fibonacci number is = %d\n", val, fib(val));


    int vals[4] = {7, 9, 11, val};
    int copyvals[4] = {7, 9, 11, val};
    const int len = 4;

    int mats[2][2] = {{3, 5}, {7, 9}};

    fibseries(vals, len);
    // print each element's fib(val)
    for (i = 0; i < len; i++)
    {
        printf("Element[%d]: The %dth Fibonacci number is = %d\n", i, copyvals[i], vals[i] );
    }
}
