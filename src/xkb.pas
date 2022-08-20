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

unit xkb;
{$mode objfpc}  

interface

    procedure setupkeyboard;cdecl;
    function ctrlshift:boolean;cdecl;
    function ctrl:boolean;cdecl;
    function ctrlenter:boolean;cdecl;
    function ritshift:integer;cdecl;
    procedure shiftchange(ishift: integer);cdecl;
    procedure shiftgrab(on: integer);cdecl;

implementation

    procedure setupkeyboard;cdecl;external;
    function ctrlshift:boolean;cdecl;external;
    function ctrl:boolean;cdecl;external;
    function ctrlenter:boolean;cdecl;external;
    function ritshift:integer;cdecl;external;
    procedure shiftchange(ishift: integer);cdecl;external;
    procedure shiftgrab(on: integer);cdecl;external;
    
end.
