program prog1;

{$mode objfpc}{$H+}

uses
  unit1;

var
  a, b: integer;

begin
  a := 12; // 17 in C program
  b := 51; // 19 in C program
  writeln('From Pascal: The sum of ', a, ' and ', b, ' is ', sum(a, b));
end.
