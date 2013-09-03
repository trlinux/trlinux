unit hidp;
{$mode objfpc}
{$l hid.o}
{$linklib libusb-1.0}

interface

function hid_init:longint;cdecl;
function hid_exit:longint;cdecl;
function hid_open(vendor: integer; product: integer):Pointer;cdecl;
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
function hid_open(vendor: integer; product: integer):Pointer;cdecl;external;
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
