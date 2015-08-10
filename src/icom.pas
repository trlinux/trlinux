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
unit icom;

interface
uses rig,timer,tree;

type
   icomctl = class(radioctl)
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
      procedure setcivaddress(addressin: byte);
      procedure settrcivaddress(addressin: byte);
      private
         commandtime: longint;
         commandcount: longint;
         commandretrycount: longint;
         commandmaxretry: longint;
         rcvtime: longint;
         rcvcount: longint;
         address: byte;
         traddress: byte;
         filterbyte: byte;
         echofound: boolean;
         pollwait: longint;
         f10digit: boolean;
   end;

implementation
uses sysutils,math;

constructor icomctl.create(debugin: boolean);
begin
   inherited create(debugin);
   commandtime := 40;
   commandcount := 0;
   rcvtime := 40;
   rcvcount := 0;
   commandmaxretry := 2;
   commandretrycount := 0;
   address := $04; //ic-735
   echofound := false;
   traddress := $e0;
   f10digit := true;
   pollwait := 30000 div 17; //wait 3 seconds before polling for K9JM box
end;

procedure icomctl.setcivaddress(addressin: byte);
begin
   address := addressin;
end;

procedure icomctl.settrcivaddress(addressin: byte);
begin
   traddress := addressin;
end;

procedure icomctl.putradiointosplit;
begin
   sendstring(chr($fe)+chr($fe)+chr(address)+chr(traddress)+chr($0f)
      +chr($01)+chr($fd));
end;

procedure icomctl.putradiooutofsplit;
begin
   sendstring(chr($fe)+chr($fe)+chr(address)+chr(traddress)+chr($0f)
      +chr($00)+chr($fd));
end;

procedure icomctl.setradiofreq(f: longint; m: modetype; vfo: char);
var freqstr: string;
begin
   mode := m;
   if (vfo = 'A') then
   begin
      freq := f;
      fromrigstart := 0; //new frequency so clear radio buffer and state
      fromrigend := 0;
      torigstart := 0;
      torigend := 0;
      waiting := false;
   end;
   if vfo = 'B' then sendstring(chr($fe)+chr($fe)+chr(address)+chr(traddress)
      +chr($07)+chr($01)+chr($fd));
   case m of
      CW: sendstring(chr($fe)+chr($fe)+chr(address)+chr(traddress)+chr($06)
         +chr($03)+chr($fd));
      Digital: sendstring(chr($fe)+chr($fe)+chr(address)+chr(traddress)+chr($06)
         +chr($04)+chr($fd));
      else
      if (freq < 10000000) then
         sendstring(chr($fe)+chr($fe)+chr(address)+chr(traddress)+chr($06)
            +chr($00)+chr($fd))
      else 
         sendstring(chr($fe)+chr($fe)+chr(address)+chr(traddress)+chr($06)
            +chr($01)+chr($fd));
   end;
   str(f,freqstr);
   if f10digit then
   begin
      while length(freqstr) < 10 do freqstr := '0' + freqstr;
      sendstring(chr($fe)+chr($fe)+chr(address)+chr(traddress)+chr($05)
         +chr(ord(freqstr[10])-$30 or ((ord(freqstr[9])-$30) shl 4))
         +chr(ord(freqstr[8])-$30 or ((ord(freqstr[7])-$30) shl 4))
         +chr(ord(freqstr[6])-$30 or ((ord(freqstr[5])-$30) shl 4))
         +chr(ord(freqstr[4])-$30 or ((ord(freqstr[3])-$30) shl 4))
         +chr(ord(freqstr[2])-$30 or ((ord(freqstr[1])-$30) shl 4))
         +chr($fd));
   end
   else
   begin
      while length(freqstr) < 8 do freqstr := '0' + freqstr;
      sendstring(chr($fe)+chr($fe)+chr(address)+chr(traddress)+chr($05)
         +chr(ord(freqstr[8])-$30 or ((ord(freqstr[7])-$30) shl 4))
         +chr(ord(freqstr[6])-$30 or ((ord(freqstr[5])-$30) shl 4))
         +chr(ord(freqstr[4])-$30 or ((ord(freqstr[3])-$30) shl 4))
         +chr(ord(freqstr[2])-$30 or ((ord(freqstr[1])-$30) shl 4))
         +chr($fd));
   end;
   if vfo = 'B' then sendstring(chr($fe)+chr($fe)+chr(address)+chr(traddress)
      +chr($07)+chr($00)+chr($fd));
end;

procedure icomctl.clearrit;
begin
end;

procedure icomctl.bumpritup;
begin
end;

procedure icomctl.bumpritdown;
begin
end;

procedure icomctl.bumpvfoup;
begin
end;

procedure icomctl.bumpvfodown;
begin
end;

function icomctl.getradioparameters(var f: longint; var b: bandtype;
         var m: modetype): boolean;
begin
   f := freq;
   m := mode;
   b := getband(freq);
// fixme
   getradioparameters := true;
end;
   
procedure icomctl.timer(caughtup: boolean);
var c: char;
    response: string;
    command: string;
    i,j,k,m: integer;
    f1,f2,f3,f4,f5: byte;
begin
   inherited timer(caughtup);
   if radioport = nil then exit;
   while radioport.charready do
   begin
      c := radioport.readchar();
      if debugopen then write(debugfile,inttohex(ord(c),2));
      if c = chr($fc) then //collision
      begin
         fromrigstart := fromrigend;
         rcvcount := 0;
         waiting := false;
         commandretrycount := 0;
         if debugopen then writeln(debugfile,'');
         exit;
      end
      else if c = chr($fd) then
      begin
         if debugopen then writeln(debugfile,'');
         rcvcount := 0;
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
         if (response[1] = chr($fe)) and (response[2] = chr($fe)) and
            (response[3] = chr($00)) and (response[4] = chr(address)) then
         begin //transceive mode grab freq/mode
            case ord(response[5]) of
               0: if ((i = 10) or (i=11)) then
                  begin
                     if debugopen then writeln(debugfile,'Transceive mode freq');
                     m := 0;
                     for j:=6 to i-1 do
                     begin
                        m := max(m,ord(response[j]) and $0f);
                        m := max(m,ord(response[j]) shr 4);
                     end;
                     if (m < 10) then
                     begin
                        f1 := ord(response[6]);
                        f2 := ord(response[7]);
                        f3 := ord(response[8]);
                        f4 := ord(response[9]);
                        f5 := ord(response[10]);
                     
                        freq := (f1 and $0f)+10*(f1 shr 4)
                           +100*((f2 and $0f)+10*(f2 shr 4))
                           +10000*((f3 and $0f)+10*(f3 shr 4))
                           +1000000*((f4 and $0f)+10*(f4 shr 4));
                        if (i=11) then freq := freq
                          +100000000*((f5 and $0f)+10*(f5 shr 4));
                     end;
                  end;
               1: begin
                     if debugopen then writeln(debugfile,'Transceive mode');
                     case ord(response[6]) of
                        0,1: mode := phone;
                        3: mode := cw;
                        4: mode := digital;
                        else mode := cw;
                     end;
                     if (i = 8) then filterbyte := ord(response[7]);
                  end;
            end;
         end
         else if waiting and (not echofound) and (i = length(lastcommand)) then
         begin
            echofound := true;
            for j:=1 to i do
            begin
               echofound := echofound and (response[j] = lastcommand[j])
            end;
            if debugopen then writeln(debugfile,'echo found ',echofound);
         end
         else if waiting and echofound and (response[1] = chr($fe)) and
            (response[2] = chr($fe)) and (response[3] = chr(traddress)) and
            (response[4] = chr(address)) then
         begin
            if debugopen then writeln(debugfile,'sent to my address');
            if (i < 6) or  (response[i-1] = chr($fa)) then
            begin
               inc(commandretrycount);
               if commandretrycount <= commandmaxretry then
               begin
                  if debugopen then writeln(debugfile,'resending command ');
                  for k := 1 to length(lastcommand) do
                  begin
                     radioport.putchar(lastcommand[k]);
                     if debugopen then write(debugfile,lastcommand[k]);
                  end;
                  if debugopen then writeln(debugfile);
                  waiting := true;
               end
               else
               begin
                  waiting := false;
                  commandretrycount := 0;
                  if debugopen then writeln(debugfile,'giving up on retries');
               end;
            end
            else if (response[5] = chr($03)) then //frequency
            begin
               if debugopen then writeln(debugfile,'polled frequency message');
               waiting := false;
               if ((i = 10) or (i=11)) then
               begin
                  m := 0;
                  for j:=6 to i-1 do
                  begin
                     m := max(m,ord(response[j]) and $0f);
                     m := max(m,ord(response[j]) shr 4);
                  end;
                  if (m < 10) then
                  begin
                     f1 := ord(response[6]);
                     f2 := ord(response[7]);
                     f3 := ord(response[8]);
                     f4 := ord(response[9]);
                     f5 := ord(response[10]);
                     freq := (f1 and $0f)+10*(f1 shr 4)
                        +100*((f2 and $0f)+10*(f2 shr 4))
                        +10000*((f3 and $0f)+10*(f3 shr 4))
                        +1000000*((f4 and $0f)+10*(f4 shr 4));
                     if (i=11) then freq := freq
                       +100000000*((f5 and $0f)+10*(f5 shr 4));
                  end;
               end;
            end
            else if (response[5] = chr($04)) then //mode and possible filter
            begin
               if debugopen then writeln(debugfile,'polled mode message');
               waiting := false;
               if ((i = 7) or (i = 8)) then
               begin
                  case ord(response[6]) of
                     0,1: mode := phone;
                     3: mode := cw;
                     4: mode := digital;
                     else mode := cw;
                  end;
               end;
               if (i = 8) then
               begin
                  waiting := false;
                  filterbyte := ord(response[7]);
               end;
            end else if (response[5] = chr($fb)) then //good result
            begin
               waiting := false;
               if debugopen then writeln(debugfile,'success');
            end;
         end;
      end
      else
      begin
         fromrig[fromrigend] := c;
         fromrigend := (fromrigend + 1) mod rigbuffersize;
      end;
   end;
   if (fromrigstart <> fromrigend) then
   begin
      inc(rcvcount);
      if rcvcount >= rcvtime then //give up on current message
      begin
         rcvcount := 0;
         fromrigstart := fromrigend;
         if debugopen then writeln(debugfile,'receive time out');
      end;
   end;
   if waiting then begin
      inc(commandcount);
      if commandcount >= commandtime then begin
         waiting := false;
         commandcount := 0;
         if debugopen then writeln(debugfile,'command timed out');
      end;
   end;
   if (not waiting) and (pollcounter >= polltime) and pollradio and
      (pollcounter >= pollwait) then
   begin
// ask for mode ask for frequency
      sendstring(chr($fe)+chr($fe)+chr(address)+chr(traddress)+chr($04)
         +chr($fd));
      sendstring(chr($fe)+chr($fe)+chr(address)+chr(traddress)+chr($03)
         +chr($fd));
      pollcounter := 0;
      pollwait := 0;
   end;
   inc(pollcounter);
   if (not waiting) and (fromrigstart = fromrigend) then begin
      i := 0;
      j := torigstart;
      while j <> torigend do begin
         inc(i);
         command[i] := torig[j];
         j := (j+1) mod rigbuffersize;
         if command[i] = chr($fd) then begin
            setlength(command,i);
            torigstart := j;
            if debugopen then write(debugfile,'sending ');
            for k := 1 to length(command) do
            begin
               radioport.putchar(command[k]);
               if debugopen then write(debugfile,inttohex(ord(command[k]),2));
            end;
            if debugopen then writeln(debugfile);
            lastcommand := command;
            waiting := true;
            echofound := false;
            commandcount := 0;
            break;
         end;
      end;
   end;
end;

procedure icomctl.directcommand(s: string);
begin
   if length(s) < 5 then exit;
   if (s[1] <> chr($fe)) then exit;
   if (s[2] <> chr($fe)) then exit;
   if (s[3] <> chr(address)) then exit;
   if (s[4] <> chr(traddress)) then exit;
   if (s[length(s)] <> chr($fd)) then exit;
   sendstring(s);
end;

procedure icomctl.responsetimeout(ms: integer);
begin
   commandtime := (ms*10) div 17;
   if commandtime <= 0 then commandtime := 1;
end;

function icomctl.getresponsetimeout:longint;
begin
   getresponsetimeout := (commandtime*17) div 10;
end;

end.
