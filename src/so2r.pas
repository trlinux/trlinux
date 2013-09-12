unit so2r;
{$mode objfpc}
interface
type
   rcvfocus_t = (RX1, RX2, STEREO);
   hmode_t = (HNORMAL, HSYMMETRIC, HSPATIAL);

   so2rinterface = interface
      procedure setrig1band(band: integer);
      procedure setrig2band(band: integer);
      procedure setheadphonemode(hmode: hmode_t);
      procedure setblend(on: boolean);
      procedure blendvalue(val: integer);
      procedure setmicrelay(on: boolean);
      procedure setrig1map(val: integer);
      procedure setrig2map(val: integer);
      procedure setrcvfocus(rcvfocus: rcvfocus_t);
      function footswitchpressed:boolean;

      function getheadphonemode:hmode_t;
      function getblend:boolean;
      function getblendvalue:integer;
      function getmicrelay:boolean;
      function getrig1map:integer;
      function getrig2map:integer;
   end;

implementation
end.
