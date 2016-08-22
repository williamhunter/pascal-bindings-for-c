unit ulibfibDyn;

interface
// leave this empty

// The function prototypes in fibonacci.h are:
// int fib(int a);
// void fibseries(int series[], int len);

// The equivalent Pascal functions are (and link to external shared object):
function fib(a:longint):longint; cdecl; external 'libfibonacci.so';
procedure fibseries(series:plongint; len:longint); cdecl; external 'libfibonacci.so';

implementation
// leave this empty

end.
