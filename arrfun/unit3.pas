unit unit3;

{$link arrfun.o}

interface
// leave this empty

uses ctypes;

procedure multelem(u, v, w:pcdouble; len_w:cint32); cdecl; external;
// multiply arguments element-wise; vectors must be same length

function sumelem(v:pcdouble; len_v:cint32): cdouble; cdecl; external;
// sum of elements

implementation
// leave this empty

end.
