unit unit2;

{$link hello.o} // link to C object-code

{$ifdef WINDOWS}
  {$linklib libmsvcrt}
{$else} // Linux
  {$linklib libc} // or try {$linklib c}
{$endif}

{Note the difference in use of $link and $linklib above,
the first is for object files, the latter for libraries.}

interface
// leave this empty

uses ctypes;

procedure printhello; cdecl; external;
procedure printhelloperson(name : pchar); cdecl; external;
// function prototypes to match that of hello.h exactly, even the case!

implementation
// leave this empty

end.
