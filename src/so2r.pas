unit so2r;
{$mode objfpc}
interface
type
   so2rinterface = interface
      procedure setrcvfocus;
      procedure setxmtfocus;
      procedure setrig1band;
      procedure setrig2band;
      function getrcvfocus:integer;
      function getxmtfocus:integer;
   end;
implementation
end.
