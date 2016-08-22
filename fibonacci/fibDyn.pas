program fibDyn;

{$mode objfpc}{$H+}

uses
  ulibfibDyn;

var
  i: integer;
  vec: array[0..3] of integer = (18, 15, 17, 23);
  copyvec: array[0..3] of integer = (18, 15, 17, 23);
  len: integer = length(vec);

begin
  WriteLn('From Pascal, referencing the Shared Object (Dynamic Library):');
  
  for i := 8 to 23 do
    WriteLn('Fib(', i, ') = ', fib(i));
  
  fibseries(vec, len);
  WriteLn();
  WriteLn('Calculate Fibonacci numbers for the following vector of values:');
  Write('[ ');
  for i := 0 to (len - 1) do  
    Write(copyvec[i], ' ');
  WriteLn(']');

  for i := 0 to (len - 1) do
    WriteLn('Fib(vec[', i, ']) = ', vec[i]);
end.
