{$mode objfpc}
unit kenwood;

interface
uses rig,timer,tree;

type
   kenwoodctl = class(radioctl)
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
      private
         commandtime: longint;
         commandcount: longint;
         commandretrycount: longint;
         commandmaxretry: longint;
         ignorefreq: boolean;
         ignorefreqcount: longint;
         intosplit: string;
         outofsplit: string;
         tovfob: string;
         tovfoa: string;
         ritup: string;
         ritdn: string;
         ritclr: string;
         vfoup: string;
         vfodn: string;
         cwmode: string;
         digitalmode: string;
         lsbmode: string;
         usbmode: string;
         freqpos: longint;
         freqdigits: longint;
         modepos: longint;
         responselength: longint;
         bumpcount: longint;
         bumptime: longint;
         bumpignore: boolean;
   end;

   ftdx5000ctl = class(kenwoodctl)
      public
      constructor create;override;
   end;
  


implementation

constructor kenwoodctl.create;
begin
   inherited create;
   commandtime := 500;
   commandcount := 0;
   commandmaxretry := 2;
   commandretrycount := 0;
   ignorefreq := false;
   ignorefreqcount := 0;
   intosplit := 'FR0;FA;FT1';
   outofsplit := 'FR0;FT0;';
   tovfob := 'FR1;FT1;';
   tovfoa := 'FA;FR0;FT0;';
   freqpos := 3;
   freqdigits := 11;
   modepos := 30;
   responselength := 38;
   ritup := 'RU;';
   ritdn := 'RD;';
   ritclr := 'RC;';
   vfoup := 'UP;';
   vfodn := 'DN;';
   cwmode := 'MD3;';
   digitalmode := 'MD6;';
   lsbmode := 'MD1;';
   usbmode := 'MD2;';
   bumpcount := 0;
   bumptime := 200;
   bumpignore := false
end;

constructor ftdx5000ctl.create;
begin
   inherited create;
   commandtime := 500;
   commandcount := 0;
   commandmaxretry := 2;
   commandretrycount := 0;
   ignorefreq := false;
   ignorefreqcount := 0;
   intosplit := 'FR0;FA;FT1';
   outofsplit := 'FR0;FT0;';
   tovfob := 'FR1;FT1;';
   tovfoa := 'FA;FR0;FT0;';
   freqpos := 6;
   freqdigits := 8;
   modepos := 21;
   responselength := 27;
   ritup := 'RU0010;';
   ritdn := 'RD0010;';
   ritclr := 'RC;';
   vfoup := 'EU001;';
   vfodn := 'ED001;';
   cwmode := 'MD03;MD13';
   digitalmode := 'MD06;MD16;';
   lsbmode := 'MD01;MD11;';
   usbmode := 'MD02;MD12;';
   bumpcount := 0;
   bumptime := 200;
   bumpignore := false
end;

procedure kenwoodctl.putradiointosplit;
begin
   sendstring(intosplit);
end;

procedure kenwoodctl.putradiooutofsplit;
begin
   sendstring(outofsplit);
end;

procedure kenwoodctl.setradiofreq(f: longint; m: modetype; vfo: char);
var freqstr: string;
begin
   str(f,freqstr);
   while length(freqstr) < freqdigits do freqstr := '0' + freqstr;
   pollcounter := -3*polltime; // skip next few polls
   torigstart := 0;
   torigend := 0;
   if (vfo = 'A') then
   begin
      freq := f;
   end;
   sendstring('F' + VFO + freqstr + ';');
   if vfo = 'B' then begin
      sendstring(tovfob);
   end;
   case m of
      CW: sendstring(cwmode);
      Digital: sendstring(digitalmode);
      else
      if (freq < 10000000) then
         sendstring(lsbmode)
      else 
         sendstring(usbmode);
   end;
   mode := m;
   if (vfo = 'B') then sendstring(tovfoa);
   ignorefreq := true;
   ignorefreqcount := 0;
end;

procedure kenwoodctl.clearrit;
begin
   sendstring(ritclr);
end;

procedure kenwoodctl.bumpritup;
begin
   if bumpignore then exit;
   bumpignore := true;
   bumpcount := 0;
   sendstring(ritup);
end;

procedure kenwoodctl.bumpritdown;
begin
   if bumpignore then exit;
   bumpignore := true;
   bumpcount := 0;
   sendstring(ritdn);
end;

procedure kenwoodctl.bumpvfoup;
begin
   if bumpignore then exit;
   bumpignore := true;
   bumpcount := 0;
   sendstring(vfoup);
end;

procedure kenwoodctl.bumpvfodown;
begin
   if bumpignore then exit;
   bumpignore := true;
   bumpcount := 0;
   sendstring(vfodn);
end;

function kenwoodctl.getradioparameters(var f: longint; var b: bandtype;
         var m: modetype): boolean;
begin
   f := freq;
   m := mode;
   b := getband(freq);
// fixme
   getradioparameters := true;
end;
   
procedure kenwoodctl.timer(caughtup: boolean);
var c: char;
    response: string;
    command: string;
    i,j,k: integer;
    freqnow: longint;
    code: word = 0;
begin
   inherited timer(caughtup);
   if radioport = nil then exit;
   while radioport.charready do
   begin
      c := radioport.readchar();
      if c <> ';' then
      begin
         fromrig[fromrigend] := c;
         fromrigend := (fromrigend + 1) mod rigbuffersize;
      end
      else
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
         if response[1] = '?' then
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
               commandcount := 0;
            end;
         end;
         if (response[1] = 'I') and (response[2] = 'F') and
            (length(response) = responselength) then
         begin
            val(copy(response,freqpos,freqdigits),freqnow,code);
            if ignorefreq then
            begin
               freqnow := freq;
               inc(ignorefreqcount);
               ignorefreq := (ignorefreqcount <= 2);
           end;
            if code = 0 then freq := freqnow;
            case response[modepos] of
               '1', '2', '4', '5': mode := Phone;
               '6', '9': mode := Digital;
               else mode := cw;
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
      end;
   end;
   if ((not waiting) and (pollcounter >= polltime)) and pollradio then
   begin
      sendstring('IF;');
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
         if command[i] = ';' then begin
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

procedure kenwoodctl.directcommand(s: string);
begin
   if (s[length(s)] <> ';') then exit;
   sendstring(s);
end;

procedure kenwoodctl.responsetimeout(ms: integer);
begin
   commandtime := (ms*10) div 17;
   if commandtime <= 0 then commandtime := 1;
end;

end.
