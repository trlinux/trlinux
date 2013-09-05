unit so2r;
{$mode objfpc}
interface
type
   so2rinterface = interface
      procedure setrcvfocus;
      procedure setxmtfocus;
      procedure setrig1band(band: integer);
      procedure setrig2band(band: integer);
      function getrcvfocus:integer;
      function getxmtfocus:integer;
   end;
implementation
end.
