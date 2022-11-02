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
{$mode objfpc}
unit rig;

interface
uses communication,tree;

const
   rigbuffersize = 1024;

type
//   ModeType = (CW, Digital, Phone, Both, NoMode, FM);
//   BandType = (Band160, Band80, Band40, Band20, Band15, Band10,
//               Band30, Band17, Band12, Band6, Band2, Band222, Band432,
//               Band902, Band1296, Band2304, Band3456, Band5760, Band10G,
//               Band24G, BandLight, All, NoBand);

   RigBuffer = Array [0..rigbuffersize-1] of Char;

   radioctl = class
      public
      constructor create(debugin: boolean);virtual;
      procedure putradiointosplit;virtual;
      procedure putradiooutofsplit;virtual;
      procedure setradiofreq(f: longint; m: modetype; vfo: char);virtual;
      procedure clearrit;virtual;
      procedure bumpritup;virtual;
      procedure bumpritdown;virtual;
      procedure bumpvfoup;virtual;
      procedure bumpvfodown;virtual;
      procedure setpollradio(polling: boolean);virtual;

      function getradioparameters(var f: longint; var b: bandtype;
         var m: modetype): boolean;virtual;

      procedure responsetimeout(ms: integer);virtual;
      function getresponsetimeout():longint;virtual;
      procedure directcommand(s: string);virtual;
      procedure timer(caughtup: boolean);virtual;
      procedure setport(port: serialportx);virtual;
      procedure setpolltime(ms: integer);virtual;
      procedure setcwreverse(on: boolean);virtual;
      procedure closedebug();

      FUNCTION CalmDownEveryone: BOOLEAN;virtual;
      FUNCTION DirectCommandAndResponse (RadioCommand: STRING): STRING;virtual;
      FUNCTION K3IsStillTalking: BOOLEAN;virtual;
      PROCEDURE SetK3TXPollMode (enable: boolean);virtual;

      protected
      freq: longint;
      mode: modetype;
      txon: BOOLEAN;
      debug: boolean;
      debugopen: boolean;
      polltime: integer;
      pollcounter: integer;
      pollradio: boolean;
      radioport: serialportx;
      fromrig: rigbuffer;
      fromrigstart,fromrigend: integer;
      torig: rigbuffer;
      torigstart,torigend: integer;
      waiting: boolean;
      debugfile: text;
      lastcommand: string;
      cwreverse: boolean;
      k3txpollmode: boolean;
      procedure sendstring(s: string);
      function getband(f: longint):bandtype;

   end;

   yaesuctl = class(radioctl)
   end;

   orionctl = class(radioctl)
   end;

   hamlibctl = class(radioctl)
   end;


implementation

const
   nband = 15;
   bandedge: array[1..nband,1..2] of longint = ( (1790000, 2010000),
      (3490000, 4010000), (6990000, 7310000), (13990000, 14360000),
      (20990000, 21460000), (27990000, 29710000), (10090000, 10160000),
      (18060000, 18180000), (24890000, 25000000), (49990000, 54010000),
      (143990000, 148010000), (218990000, 225010000), (419990000, 450010000),
      (901990000,928010000), (1239990000, 1300010000));
   bandin: array[1..nband] of BandType = (Band160, Band80, Band40, Band20,
      Band15, Band10, Band30, Band17, Band12, Band6, Band2, Band222, Band432,
               Band902, Band1296);

constructor radioctl.create(debugin: boolean);
begin
   fromrigstart := 0;
   fromrigend := 0;
   torigstart := 0;
   torigend := 0;
   freq := 0;
   mode := nomode;
   pollradio := true;
   polltime := 2500 div 17;
   pollcounter := 0;
   radioport := nil;
   waiting := false;
   lastcommand := '';
   debug := debugin;
   debugopen := false;
   cwreverse := false;
   k3txpollmode := false;
end;


FUNCTION radioctl.CalmDownEveryone: BOOLEAN;

    BEGIN
    END;


FUNCTION radioctl.DirectCommandAndResponse (RadioCommand: STRING): STRING;

    BEGIN
    END;


function radioctl.k3isstilltalking: boolean;
    begin
    k3isstilltalking := false;
    end;



procedure radioctl.setk3txpollmode (enable: boolean);

begin
   k3txpollmode := enable;
end;

procedure radioctl.setpolltime(ms: integer);
begin
   polltime := (ms*10) div 17;
   if polltime <= 0 then polltime := 1;
end;

procedure radioctl.setpollradio(polling: boolean);
begin
   pollradio := polling;
end;

procedure radioctl.closedebug();
begin
   if debugopen then close(debugfile);
end;

procedure radioctl.setport(port: serialportx);
var tempstr: string;
    i: longint;
begin
   radioport := port;
   if (debug) then
   begin
      debugopen := true;
      tempstr := port.devname;
      for i := length(tempstr) downto 1 do
      begin
         if tempstr[i] = directoryseparator then
         begin
            delete(tempstr,1,i);
            break;
         end
      end;
      assign(debugfile,'rig_' + tempstr + '.dbg');
      rewrite(debugfile);
   end;
end;

procedure radioctl.timer(caughtup: boolean);
begin
   if radioport = nil then exit else radioport.charready;
   if not caughtup then exit;
end;

procedure radioctl.sendstring(s: string);
var i: integer;
begin
   for i := 1 to length(s) do
   begin
      torig[torigend] := s[i];
      torigend := (torigend+1) mod rigbuffersize;
   end;
end;

function radioctl.getradioparameters(var f: longint; var b: bandtype;
         var m: modetype):boolean;
begin
   f := 0;
   b := noband;
   m := cw;
   getradioparameters := false;
end;

function radioctl.getband(f: longint):bandtype;
var i: integer;
begin
   for i := 1 to nband do
   begin
      if (f > bandedge[i,1]) and (f < bandedge[i,2]) then
      begin
         getband := bandin[i];
         exit;
      end;
   end;
   getband := NoBand;
end;

procedure radioctl.setcwreverse(on: boolean);
begin
   cwreverse := on;
end;

procedure radioctl.putradiointosplit;
begin
end;

procedure radioctl.putradiooutofsplit;
begin
end;

procedure radioctl.setradiofreq(f: longint; m: modetype; vfo: char);
begin
end;

procedure radioctl.clearrit;
begin
end;

procedure radioctl.bumpritup;
begin
end;

procedure radioctl.bumpritdown;
begin
end;

procedure radioctl.bumpvfoup;
begin
end;

procedure radioctl.bumpvfodown;
begin
end;

procedure radioctl.responsetimeout(ms: integer);
begin
end;

function radioctl.getresponsetimeout:longint;
begin
   getresponsetimeout := 0;
end;

procedure radioctl.directcommand(s: string);
begin
end;

end.
