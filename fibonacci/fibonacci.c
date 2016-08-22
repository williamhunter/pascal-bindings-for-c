/*
 * Filename: fibonacci.c
 *
 */

int fib(int val)
// Recursive function to compute Fibonacci number, counting from 0
{
    if (val < 0) //  'Wrong' input will return -1
        return - 1;
    else if (val == 0)
        return 0;
    else if ((val == 1)||(val == 2))
        return 1;
    else
        return fib(val - 2) + fib(val - 1);
}

void fibseries(int series[], int len)
// Compute nth Fibonacci number for series of values
{
    int i;
    for (i = 0; i < len; i++)
    {
    series[i] = fib(series[i]);
    }
}