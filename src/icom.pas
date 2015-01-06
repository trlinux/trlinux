{$mode objfpc}
unit icom;

interface
uses rig,timer,tree;

type
   icomctl = class(radioctl)
      public
      constructor create;override;
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
      procedure timer(caughtup: boolean);override;
      procedure directcommand(s: string);override;
      procedure setcivaddress(addressin: byte);
      private
         commandtime: longint;
         commandcount: longint;
         commandretrycount: longint;
         commandmaxretry: longint;
         address: byte;
         filterbyte: byte;
   end;

implementation

constructor icomctl.create;
begin
   inherited create;
   commandtime := 10;
   commandcount := 0;
   commandmaxretry := 2;
   commandretrycount := 0;
   address := $04; //ic-735
end;

procedure icomctl.setcivaddress(addressin: byte);
begin
   address := addressin;
end;

procedure icomctl.putradiointosplit;
begin
   sendstring(chr($fe)+chr($fe)+chr(address)+chr($e0)+chr($0f)
      +chr($01)+chr($fd));
end;

procedure icomctl.putradiooutofsplit;
begin
   sendstring(chr($fe)+chr($fe)+chr(address)+chr($e0)+chr($0f)
      +chr($00)+chr($fd));
end;

procedure icomctl.setradiofreq(f: longint; m: modetype; vfo: char);
var freqstr: string;
begin
   if vfo = 'B' then sendstring(chr($fe)+chr($fe)+chr(address)+chr($e0)
      +chr($07)+chr($01)+chr($fd));
   case m of
      CW: sendstring(chr($fe)+chr($fe)+chr(address)+chr($e0)+chr($06)
         +chr($03)+chr($fd));
      Digital: sendstring(chr($fe)+chr($fe)+chr(address)+chr($e0)+chr($06)
         +chr($04)+chr($fd));
      else
      if (freq < 10000000) then
         sendstring(chr($fe)+chr($fe)+chr(address)+chr($e0)+chr($06)
            +chr($00)+chr($fd))
      else 
         sendstring(chr($fe)+chr($fe)+chr(address)+chr($e0)+chr($06)
            +chr($01)+chr($fd));
   end;
   str(f,freqstr);
   while length(freqstr) < 8 do freqstr := '0' + freqstr;
   sendstring(chr($fe)+chr($fe)+chr(address)+chr($e0)+chr($05)
      +chr(ord(freqstr[8])-$30 or ((ord(freqstr[7])-$30) shl 4))
      +chr(ord(freqstr[6])-$30 or ((ord(freqstr[5])-$30) shl 4))
      +chr(ord(freqstr[4])-$30 or ((ord(freqstr[3])-$30) shl 4))
      +chr(ord(freqstr[2])-$30 or ((ord(freqstr[1])-$30) shl 4))
      +chr($fd));
   if vfo = 'B' then sendstring(chr($fe)+chr($fe)+chr(address)+chr($e0)
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
    i,j,k: integer;
    f1,f2,f3,f4,f5: byte;
begin
   inherited timer(caughtup);
   if radioport = nil then exit;
   while radioport.charready do
   begin
      c := radioport.readchar();
      if c = chr($fd) then
      begin
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
         if (i < 6) or  (response[i-1] = chr($fa)) then
         begin
            inc(commandretrycount);
            if commandretrycount <= commandmaxretry then
            begin
               for k := 1 to length(lastcommand) do
                  radioport.putchar(lastcommand[k]);
               waiting := true;
            end
            else
            begin
               waiting := false;
               commandretrycount := 0;
            end;
         end
         else if (response[4] = chr($e0)) then waiting := true //echo
         else if (response[5] = chr($03)) then //frequency
         begin
            if ((i = 10) or (i=11)) then
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
         end
         else if (response[5] = chr($04)) then //mode and possible filter
         begin
            if ((i = 7) or (i = 8)) then
            begin
               case ord(response[6]) of
                  0,1: mode := phone;
                  3: mode := cw;
                  4: mode := digital;
                  else mode := cw;
               end;
            end;
            if (i = 8) then filterbyte := ord(response[7]);
         end;
      end
      else
      begin
         fromrig[fromrigend] := c;
         fromrigend := (fromrigend + 1) mod rigbuffersize;
      end;
   end;
   if waiting then begin
      inc(commandcount);
      if commandcount >= commandtime then begin
         waiting := false;
         commandcount := 0;
      end;
   end;
   if ((not waiting) and (pollcounter >= polltime)) and pollradio then
   begin
// ask for mode ask for frequency
      sendstring(chr($fe)+chr($fe)+chr(address)+chr($e0)+chr($04)+chr($fd));
      sendstring(chr($fe)+chr($fe)+chr(address)+chr($e0)+chr($03)+chr($fd));
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
         if command[i] = chr($fd) then begin
            setlength(command,i);
            torigstart := j;
            for k := 1 to length(command) do radioport.putchar(command[k]);
            lastcommand := command;
            waiting := true;
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
   if (s[4] <> chr($e0)) then exit;
   if (s[length(s)] <> chr($fd)) then exit;
   sendstring(s);
end;

procedure icomctl.responsetimeout(ms: integer);
begin
   commandtime := (ms*10) div 17;
   if commandtime <= 0 then commandtime := 1;
end;

end.
