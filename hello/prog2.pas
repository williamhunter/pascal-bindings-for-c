program prog2;

{$mode objfpc}{$H+}

uses
  unit2;

var
  name: pchar; // ^char will also work

begin
  name := 'Joe Public';
  writeln('From Pascal, calling C functions:');
  PrintHello;
  PrintHelloPerson(name);
end.
