//
//Copyright Larry Tyree, N6TR, 2011,2012,2013,2014,2015.
//
//This file is part of TR log for linux.
//
//TR log for linux is free software: you can redistribute it and/or
//modify it under the terms of the GNU General Public License as
//published by the Free Software Foundation, either version 2 of the
//License, or (at your option) any later version.
//
//TR log for linux is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General
//    Public License along with TR log for linux.  If not, see
//<http://www.gnu.org/licenses/>.
//

//KS - These routines are defined in this weird way because they are
//replacements for the original TR DOS assembly language routines.
//They are put in a unit so that they can be compiled to an object file.
//The aliases force those names to appear in the object file for linking.
//In linux run nm dupepas.o to see the free pascal mangled names and the
//alias name. The alias names are the names of the original assembler routines.
//This should be cleaned up eventually.

unit dupepas;
interface
function bytdupe(c: pointer; n: integer; a: pointer):boolean;cdecl;
function numbytes(call1: pointer; call2: pointer):integer;cdecl;
function bytaddr(c: pointer; n: integer; a: pointer):integer;cdecl;
function paralleladdress(i: integer):integer;cdecl;//alias:'paralleladdress';
function serialaddress(i: integer):integer;cdecl;//alias:'serialaddress';

implementation
function bytdupe(c: pointer; n: integer; a: pointer):boolean;cdecl;
   alias: 'BYTDUPE';
var i:integer;
cl,al: ^cardinal;
begin
   cl := c;
   al := a;
   bytdupe := false;
   for i := 0 to n-1 do
   begin
      if (cl^ = al[i]) then
      begin
         bytdupe := boolean($ff);
         exit;
      end;
   end;
end;

function numbytes(call1: pointer; call2: pointer):integer;cdecl;
   alias: 'NUMBYTES';
var cmp: cardinal;
   c1,c2: ^cardinal;
   i:integer;
begin
   i := 0;
   c1 := call1;
   c2 := call2;
   cmp := c1^ xor c2^;
   if ((cmp and $ff) = 0) then inc(i);
   if ((cmp and $ff00) = 0) then inc(i);
   if ((cmp and $ff0000) = 0) then inc(i);
   if ((cmp and $ff000000) = 0) then inc(i);
   numbytes := i;
end;

function bytaddr(c: pointer; n: integer; a: pointer):integer;cdecl;
   alias: 'BYTADDR';
var i:integer;
cl,al: ^cardinal;
begin
   cl := c;
   al := a;
   for i:=0 to n-1 do
   begin
      if ((cl^) = al[i]) then
      begin
          bytaddr := i;
          exit;
      end;
   end;
   bytaddr := n;
end;

function paralleladdress(i: integer):integer;cdecl;alias:'paralleladdress';
begin
   paralleladdress := 0;
end;

function serialaddress(i: integer):integer;cdecl;alias:'serialaddress';
begin
   serialaddress := 0;
end;

end.
