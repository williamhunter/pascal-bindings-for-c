/*
 * Filename: hello.c
 *
 */

#include <stdio.h>
#include "hello.h"

void printhello(void)
{
    printf("Hello, World!");
    printf("\n");
}

void printhelloperson(char *name)
{
    printf("Hello ");
    printf("%s\n", name);
}
