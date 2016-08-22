unit unit1;

{$link sum.o} // link to C object-code, same as {$L sum.o}

interface
// leave this empty

uses ctypes; // need to specify this else FPC won't compile

function sum(x, y : cint32) : cint32; cdecl; external;
// function args and types to match the ones as defined in sum.h

implementation
// leave this empty

end.
