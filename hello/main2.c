/*
 * Filename: main2.c
 *  
 */
 
#include <stdio.h>
#include "hello.h"

int main(void)
{
    char *name = "John Smith"; // same as: char name[] = "John Smith";
    printf("From C:\n");
    printhello();
    printhelloperson(name);
    
    return 0;
}
