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

unit portname;
interface

{$mode objfpc}
{$L findports}

type
   pchararray = array of pchar;

   function findserial(Buf: PCharArray; bufsize:integer; ndevmax: integer)
      :integer;cdecl;external;
   function findparallel(Buf: PCharArray; bufsize:integer; ndevmax: integer)
      :integer;cdecl;external;
implementation
end.
