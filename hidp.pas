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

unit hidp;
{$mode objfpc}
{$l hid.o}
{$linklib libusb-1.0}
{$linklib libudev}

interface

function hid_init:longint;cdecl;
function hid_exit:longint;cdecl;
function hid_open(vendor: integer; product: integer; p:pucs4char):Pointer;cdecl;
procedure hid_close(dev: pointer);cdecl;
function hid_get_manufacturer_string(dev: pointer;p: pucs4char;size: longint)
   :longint;cdecl;
function hid_get_product_string(dev: pointer;p: pucs4char;size: longint)
   :longint;cdecl;
function hid_get_serial_number_string(dev: pointer;p: pucs4char;size: longint)
   :longint;cdecl;
function hid_read(dev: pointer; d: pbyte; size: longint):longint;cdecl;
function hid_write(dev: pointer; d: pbyte; size: longint):longint;cdecl;
function hid_set_nonblocking(dev: pointer; on: longint):longint;cdecl;

implementation

function hid_init:longint;cdecl;external;
function hid_exit:longint;cdecl;external;
function hid_open(vendor: integer; product: integer; p:pucs4char):Pointer;cdecl;
   external;
procedure hid_close(dev: pointer);cdecl;external;
function hid_get_manufacturer_string(dev: pointer;p: pucs4char;size: longint)
   :longint;cdecl;external;
function hid_get_product_string(dev: pointer;p: pucs4char;size: longint)
   :longint;cdecl;external;
function hid_get_serial_number_string(dev: pointer;p: pucs4char;size: longint)
   :longint;cdecl;external;
function hid_read(dev: pointer; d: pbyte; size: longint):longint;cdecl;external;
function hid_write(dev: pointer; d: pbyte; size: longint):longint;cdecl
   ;external;
function hid_set_nonblocking(dev: pointer; on: longint):longint;cdecl;external;

end.
