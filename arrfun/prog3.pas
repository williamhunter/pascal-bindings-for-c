program prog3;

{$mode objfpc}{$H+}

uses
  unit3;

var
  a: array[0..4] of double = (1, 0.5, 1/3, 0.25, 0.2);
  b: array[0..4] of double = (5, 3.2, 8.5, 4.05, 1.4);
  c: array[0..4] of double = (0, 0, 0, 0, 0);
  veclen: integer = length(a);
  i: integer;
  ans : double;

begin
  writeln('From Pascal: The multiplied elements are:');
  multelem(a, b, c, veclen);
  for i := 0 to veclen - 1 do
    writeln('Item ', i:1, ': ', c[i]:1:2);

  writeln('');
  writeln('From Pascal: The sum of the elements is:');
  ans := sumelem(b, veclen);
  writeln('Sum :', ans:5:1);
end.
