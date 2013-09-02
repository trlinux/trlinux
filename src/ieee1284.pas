unit ieee1284;
{$linklib ieee1284}
interface
Type
  Pchar  = ^char;
  Pdword  = ^dword;
  Plongint  = ^longint;
  Pparport  = ^parport;
  Pparport_list  = ^parport_list;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

  parport = record
     name : ^char;
//     base_addr : dword;
//     hibase_addr : dword;
     base_addr : ptruint;
     hibase_addr : ptruint;
     priv : pointer;
     filename : ^char;
  end;

  parport_list = record
     portc : longint;
     portv : ^Pparport;
  end;

  function ieee1284_find_ports(list:Pparport_list; flags:longint):longint;cdecl;
  procedure ieee1284_free_ports(list:Pparport_list);cdecl;
  function ieee1284_open(port:Pparport; flags:longint; capabilities:Plongint)
     :longint;cdecl;
  function ieee1284_close(port:Pparport):longint;cdecl;
  function ieee1284_claim(port:Pparport):longint;cdecl;
  procedure ieee1284_release(port:Pparport);cdecl;
  procedure ieee1284_write_data(port:Pparport; dt:byte);cdecl;
  function ieee1284_read_status(port:Pparport):longint;cdecl;
  function ieee1284_read_control(port:Pparport):longint;cdecl;
  procedure ieee1284_write_control(port:Pparport; ct:byte);cdecl;
  procedure ieee1284_frob_control(port:Pparport; mask:byte; val:byte);cdecl;

implementation
  function ieee1284_find_ports(list:Pparport_list; flags:longint):longint;cdecl;
     external;
  procedure ieee1284_free_ports(list:Pparport_list);cdecl;
     external;
  function ieee1284_open(port:Pparport; flags:longint; capabilities:Plongint)
     :longint;cdecl;
     external;
  function ieee1284_close(port:Pparport):longint;cdecl;
     external;
  function ieee1284_claim(port:Pparport):longint;cdecl;
     external;
  procedure ieee1284_release(port:Pparport);cdecl;
     external;
  procedure ieee1284_write_data(port:Pparport; dt:byte);cdecl;
     external;
  function ieee1284_read_status(port:Pparport):longint;cdecl;
     external;
  function ieee1284_read_control(port:Pparport):longint;cdecl;
     external;
  procedure ieee1284_write_control(port:Pparport; ct:byte);cdecl;
     external;
  procedure ieee1284_frob_control(port:Pparport; mask:byte; val:byte);cdecl;
     external;
end.
