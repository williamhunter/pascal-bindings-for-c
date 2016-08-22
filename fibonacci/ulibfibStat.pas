unit ulibfibStat;

{$linklib 'libfibonacci.a'} // link to our static library libfibonacci.a

interface
// leave this empty

// The function prototypes in fibonacci.h (the C header file) are:
// int fib(int a);
// void fibseries(int series[], int len);

// The equivalent Pascal functions are:
function fib(a:longint):longint; cdecl; external;
procedure fibseries(series:plongint; len:longint); cdecl; external;

implementation
// leave this empty

end.
