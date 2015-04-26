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

unit timer;
{$mode objfpc}

interface
type
   timerprocedure = procedure(x: boolean) of object;

procedure checktimer;
procedure millisleep;
procedure timerinitialize;
procedure addtimer(tp: timerprocedure);

implementation
uses unix,baseunix;
var interrupttime: TTimeval;
    last: TTimeval;
    timerinitialized: boolean;
    inttime: longint;
    timers: array[1..20] of timerprocedure;
    ntimers: integer;

function interruptnow:boolean;
var timezone: PTimeZone;
    timeval: TTimeVal;
begin
    if timerinitialized then
    begin
       timezone := nil;
       fpgettimeofday(@timeval,timezone);
       interruptnow := ((timeval.tv_sec-interrupttime.tv_sec)*1000000
          +timeval.tv_usec-interrupttime.tv_usec) >= -100;
    end
    else
    begin
       interruptnow := false;
    end;
end;

procedure setnextinterrupt(micro: longint);
var timezone: PTimeZone;
    timeval: TTimeVal;
begin
    timezone := nil;
    fpgettimeofday(@timeval,timezone);
    interrupttime.tv_sec := timeval.tv_sec
       +((timeval.tv_usec+micro) div 1000000);
    interrupttime.tv_usec := (timeval.tv_usec+micro) mod 1000000;
end;

function microsincelast:longint;
var timezone: PTimeZone;
    timeval: TTimeVal;
begin
    timezone := nil;
    fpgettimeofday(@timeval,timezone);
    microsincelast := (timeval.tv_sec-last.tv_sec)*1000000
       +timeval.tv_usec-last.tv_usec;
end;

procedure setlast;
var timezone: PTimeZone;
begin
    timezone := nil;
    fpgettimeofday(@last,timezone);
end;



procedure timerinitialize;
begin
   if not timerinitialized then
   begin
      timerinitialized := true;
      setlast; //set initial interrupt time to now
      inttime := 1680;
      setnextinterrupt(inttime);
   end;
end;

procedure timerinterrupt;
VAR micro: longint;
    caughtup: boolean;
    i: integer;
begin
    caughtup := true;
    repeat 
    begin
       micro := microsincelast;
       setlast;
       micro := inttime+((7*(inttime-micro)) div 8);
       caughtup := micro > 0;
       if caughtup then setnextinterrupt(micro);
       for i := 1 to ntimers do timers[i](caughtup);
    end
    until caughtup;
end;

procedure checktimer;
begin
   if interruptnow then TimerInterrupt;
end;

procedure millisleep;
var
   req,rem: timespec;
   i: integer;
begin
   for i:=1 to 2 do
   begin
      if interruptnow then TimerInterrupt;
      req.tv_sec := 0;
      req.tv_nsec := 500000;
      fpNanoSleep(@req,@rem);
   end;
end;

//call with addtimer(@procedure_name)
procedure addtimer(tp: timerprocedure);
begin
   inc(ntimers);
   timers[ntimers] := tp;
end;

procedure removetimer(tp: timerprocedure);
var i,ir: integer;
 
begin
   ir := 0;
   for i := 1 to ntimers do
   begin
      if (tp = timers[i]) then
      begin
         ir := i;
         break;
      end;
   end;
   if (ir <> 0) then
   begin
      dec(ntimers);
      for i := ir to ntimers do
      begin
         timers[i] := timers[i+1];
      end;
   end;
end;

begin
   timerinitialized := false;
   ntimers := 0;
end.
