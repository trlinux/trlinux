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

unit lowlatency;
interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

  type
    serial_struct = record
        _type : longint;
        line : longint;
        port : dword;
        irq : longint;
        flags : longint;
        xmit_fifo_size : longint;
        custom_divisor : longint;
        baud_base : longint;
        close_delay : word;
        io_type : char;
        reserved_char : array[0..0] of char;
        hub6 : longint;
        closing_wait : word;
        closing_wait2 : word;
        iomem_base : ^byte;
        iomem_reg_shift : word;
        port_high : dword;
        iomap_base : dword;
      end;

  const
  { Request low latency behaviour  }
    ASYNCB_LOW_LATENCY = 13;    
    ASYNC_LOW_LATENCY = 1 shl ASYNCB_LOW_LATENCY;    

implementation
end.
