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
unit rigctld;

interface
uses rig,timer,tree;

type
   rigctldctl = class(radioctl)
      public
      constructor create(debugin: boolean);override;
      procedure putradiointosplit;override;
      procedure putradiooutofsplit;override;
      procedure setradiofreq(f: longint; m: modetype; vfo: char);override;
      procedure clearrit;override;
      procedure bumpritup;override;
      procedure bumpritdown;override;
      procedure bumpvfoup;override;
      procedure bumpvfodown;override;
      function getradioparameters(var f: longint; var b: bandtype;
         var m: modetype): boolean;override;
      procedure responsetimeout(ms: integer);override;
      function getresponsetimeout:longint;override;
      procedure timer(caughtup: boolean);override;
      procedure directcommand(s: string);override;
      private
         commandtime: longint;
         commandcount: longint;
         commandretrycount: longint;
         commandmaxretry: longint;
         ignorefreq: boolean;
         ignorefreqcount: longint;
         bumpcount: longint;
         bumptime: longint;
         bumpignore: boolean;
         cwfilter,phfilter,digfilter: longint;
   end;

implementation
uses sysutils,strutils,keycode;

constructor rigctldctl.create(debugin: boolean);
begin
   inherited create(debugin);
   commandtime := 500;
   commandcount := 0;
   commandmaxretry := 2;
   commandretrycount := 0;
   ignorefreq := false;
   ignorefreqcount := 0;
   bumpcount := 0;
   bumptime := 200;
   bumpignore := true;
   cwfilter := 0;
   phfilter := 0;
   digfilter := 0;
end;

procedure rigctldctl.putradiointosplit;
begin
   sendstring('|S 1 VFOB'+linefeed);
end;

procedure rigctldctl.putradiooutofsplit;
begin
   sendstring('|S 0 VFOA'+linefeed);
end;

procedure rigctldctl.setradiofreq(f: longint; m: modetype; vfo: char);
var freqstr: string;
begin
   str(f,freqstr);
   pollcounter := -3*polltime; // skip next few polls
   torigstart := 0;
   torigend := 0;
   if (vfo = 'A') then
   begin
      freq := f;
   end;
   if vfo = 'B' then begin
      sendstring('|V VFOB' + linefeed);
   end;
   sendstring('|F ' + freqstr + linefeed);
   case m of
      CW: sendstring('|M CW ' + inttostr(cwfilter) + linefeed);
      Digital: sendstring('|M RTTY ' + inttostr(digfilter) + linefeed);
      else
      if (freq < 10000000) then
         sendstring('|M LSB ' + inttostr(phfilter) + linefeed)
      else 
         sendstring('|M USB ' + inttostr(phfilter) + linefeed);
   end;
   mode := m;
   if (vfo = 'B') then sendstring('|V VFOA' + linefeed);
   ignorefreq := true;
   ignorefreqcount := 0;
end;

procedure rigctldctl.clearrit;
begin
   sendstring('|J 1' + linefeed); // 0 Hz turns it off, try 1 Hz for clear
end;

procedure rigctldctl.bumpritup;
begin
   if bumpignore then exit;
   bumpignore := true;
   bumpcount := 0;
//   sendstring();
end;

procedure rigctldctl.bumpritdown;
begin
   if bumpignore then exit;
   bumpignore := true;
   bumpcount := 0;
//   sendstring();
end;

procedure rigctldctl.bumpvfoup;
begin
   if bumpignore then exit;
   bumpignore := true;
   bumpcount := 0;
//   sendstring();
end;

procedure rigctldctl.bumpvfodown;
begin
   if bumpignore then exit;
   bumpignore := true;
   bumpcount := 0;
//   sendstring();
end;

function rigctldctl.getradioparameters(var f: longint; var b: bandtype;
         var m: modetype): boolean;
begin
   f := freq;
   m := mode;
   b := getband(freq);
// fixme
   getradioparameters := true;
end;
   
procedure rigctldctl.timer(caughtup: boolean);
var c: char;
    response: string;
    command: string;
    i,j,k: integer;
    freqnow,hznow,hz: longint;
    modestr: string;
    code: word = 0;
begin
   inherited timer(caughtup);
   if radioport = nil then exit;
   while radioport.charready do
   begin
      c := radioport.readchar();
      if debugopen then write(debugfile,c);
      if c <> linefeed then
      begin
         fromrig[fromrigend] := c;
         fromrigend := (fromrigend + 1) mod rigbuffersize;
      end
      else
      begin
         if debugopen then writeln(debugfile,'');
         waiting := false;
         i := 0;
         while fromrigstart <> fromrigend do
         begin
            inc(i);
            response[i] := fromrig[fromrigstart];
            fromrigstart := (fromrigstart + 1) mod rigbuffersize;
         end;
         inc(i);
         response[i] := c;
         setlength(response,i);
         if response[length(response)-1] <> '0' then
         begin
            inc(commandretrycount);
            if commandretrycount <= commandmaxretry then
            begin
               if debugopen then writeln(debugfile,'Retrying ' + lastcommand);
               for k := 1 to length(lastcommand) do
                  radioport.putchar(lastcommand[k]);
               waiting := true;
            end
            else
            begin
               if debugopen then writeln(debugfile,'Timeout 1');
               waiting := false;
               commandretrycount := 0;
               commandcount := 0;
            end;
         end;
         if ansistartsstr('get_freq:|Frequency: ',response) then
         begin
            delete(response,1,length('get_freq:|Frequency: '));
            code := 1;
            if pos('|',response) <> 0 then
               val(copy(response,1,pos('|',response)-1),freqnow,code);
            if ignorefreq then
            begin
               freqnow := freq;
               inc(ignorefreqcount);
               ignorefreq := (ignorefreqcount <= 2);
            end;
            if code = 0 then freq := freqnow;
            if debugopen then writeln(debugfile,'Frequency found ',freq);
         end else if ansistartsstr('get_mode:|Mode: ',response) then
         begin
            delete(response,1,length('get_mode:|Mode: '));
            if pos('|',response) <> 0 then
            begin
               modestr := copy(response,1,pos('|',response)-1);
               delete(response,1,pos('|',response));
               hz := 0;
               if ansistartsstr('Passband: ',response) then
               begin
                  delete(response,1,length('Passband: '));
                  val(copy(response,1,pos('|',response)-1),hznow,code);
               end;
               if code = 0 then hz := hznow;
               if (ansicomparestr(modestr,'USB') = 0) or
                  (ansicomparestr(modestr,'LSB') = 0) then
               begin
                  mode := Phone;
                  phfilter := hz;
               end
               else if (ansicomparestr(modestr,'RTTY') = 0) or
                 (ansicomparestr(modestr,'RTTYR') = 0) then
               begin
                  mode := Digital;
                  digfilter := hz;
               end
               else
               begin
                  mode := CW;
                  cwfilter := hz;
               end;
               if debugopen then
               begin
                  writeln(debugfile,'Mode Filter found ',mode,hz);
               end;
            end;
         end;
      end;
   end;
   if bumpignore then
   begin
      inc(bumpcount);
      bumpignore := (bumpcount <= bumptime);
   end;
   if waiting then begin
      inc(commandcount);
      if commandcount >= commandtime then begin
         while radioport.charready do radioport.readchar();
         fromrigstart := 0;
         fromrigend := 0;
         commandcount := 0;
         commandretrycount := 0;
         waiting := false;
         if debugopen then writeln(debugfile,'Timeout 2');
      end;
   end;
   if ((not waiting) and (pollcounter >= polltime)) and pollradio then
   begin
      sendstring('|v' + linefeed); // needed to force current vfo freq return
      sendstring('|f' + linefeed);
      sendstring('|m' + linefeed);
      pollcounter := 0;
   end;
   inc(pollcounter);
   if not waiting then begin
      i := 0;
      j := torigstart;
      while j <> torigend do begin
         inc(i);
         command[i] := torig[j];
         j := (j+1) mod rigbuffersize;
         if command[i] = linefeed then begin
            setlength(command,i);
            torigstart := j;
            for k := 1 to length(command) do radioport.putchar(command[k]);
            lastcommand := command;
            if debugopen then writeln(debugfile,'Sending ',command);
            waiting := true;
            commandcount := 0;
            break;
         end;
      end;
   end;
end;

procedure rigctldctl.directcommand(s: string);
begin
//   if (s[length(s)] <> linefeed) then exit;
   sendstring('|w' + s + linefeed);
end;

procedure rigctldctl.responsetimeout(ms: integer);
begin
   commandtime := (ms*10) div 17;
   if commandtime <= 0 then commandtime := 1;
end;

function rigctldctl.getresponsetimeout:longint;
begin
   getresponsetimeout := (commandtime*17) div 10;
end;

end.
