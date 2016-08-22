program fibStat;

{$mode objfpc}{$H+}

uses
  ulibfibStat;

var
  i: integer;
  vec: array[0..3] of integer = (3, 5, 7, 17);
  copyvec: array[0..3] of integer = (3, 5, 7, 17);
  len: integer = length(vec);

begin
  WriteLn('From Pascal, referencing the C Static Library:');
  
  for i := 0 to 17 do
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
