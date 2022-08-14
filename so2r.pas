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

unit so2r;
{$mode objfpc}
{$interfaces corba}
interface
type
   rcvfocus_t = (RX1, RX2, STEREO);
   hmode_t = (HNORMAL, HSYMMETRIC, HSPATIAL);

   so2rinterface = interface
      ['{85C18D3F-F198-4681-B9D2-03B38213EA94}']
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
      procedure setlatch(on: boolean);

      function getheadphonemode:hmode_t;
      function getblend:boolean;
      function getblendvalue:integer;
      function getmicrelay:boolean;
      function getrig1map:integer;
      function getrig2map:integer;
      function getlatch:boolean;
   end;

implementation
end.
