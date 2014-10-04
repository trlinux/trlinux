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
