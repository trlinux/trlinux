{$mode objfpc}
unit rig;

interface
uses communication,tree;

const
   rigbuffersize = 256;

type
//   ModeType = (CW, Digital, Phone, Both, NoMode, FM);
//   BandType = (Band160, Band80, Band40, Band20, Band15, Band10,
//               Band30, Band17, Band12, Band6, Band2, Band222, Band432,
//               Band902, Band1296, Band2304, Band3456, Band5760, Band10G,
//               Band24G, BandLight, All, NoBand);

   RigBuffer = Array [0..rigbuffersize-1] of Char;

   radioctl = class
      public
      constructor create;virtual;
      procedure putradiointosplit;virtual;abstract;
      procedure putradiooutofsplit;virtual;abstract;
      procedure setradiofreq(f: longint; m: modetype; vfo: char);virtual;
         abstract;
      procedure clearrit;virtual;abstract;
      procedure bumpritup;virtual;abstract;
      procedure bumpritdown;virtual;abstract;
      procedure bumpvfoup;virtual;abstract;
      procedure bumpvfodown;virtual;abstract;
      procedure setpollradio(polling: boolean);virtual;
      function getradioparameters(var f: longint; var b: bandtype;
         var m: modetype): boolean;virtual;
      procedure responsetimeout(ms: integer);virtual;abstract;
      procedure directcommand(s: string);virtual;abstract;
      procedure timer(caughtup: boolean);virtual;
      procedure setport(port: serialportx);virtual;
      procedure setpolltime(ms: integer);virtual;

      protected
      freq: longint;
      mode: modetype;
      polltime: integer;
      pollcounter: integer;
      pollradio: boolean;
      radioport: serialportx;
      fromrig: rigbuffer;
      fromrigstart,fromrigend: integer;
      torig: rigbuffer;
      torigstart,torigend: integer;
      waiting: boolean;
      lastcommand: string;
      procedure sendstring(s: string);
      function getband(f: longint):bandtype;

   end;

//   kenwoodctl = class(radioctl)
//   end;

   yaesuctl = class(radioctl)
   end;

   icomctl = class(radioctl)
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

constructor radioctl.create;
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

procedure radioctl.setport(port: serialportx);
begin
   radioport := port;
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

end.
