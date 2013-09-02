unit portname;
interface

{$mode objfpc}
{$L findports}
{$linklib ieee1284}
{$linklib libc}
type
   PCharArray = array of PChar;

   function findserial(Buf: PCharArray; bufsize:integer; ndevmax: integer)
      :integer;cdecl;external;
   function findparallel(Buf: PCharArray; bufsize:integer; ndevmax: integer)
      :integer;cdecl;external;
implementation
end.
