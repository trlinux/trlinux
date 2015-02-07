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
   end;

implementation

constructor kenwoodctl.create;
begin
   inherited create;
   commandtime := 10;
   commandcount := 0;
   commandmaxretry := 2;
   commandretrycount := 0;
end;

procedure kenwoodctl.putradiointosplit;
begin
   sendstring('FR0;FA;FT1;');
end;

procedure kenwoodctl.putradiooutofsplit;
begin
   sendstring('FR0;FT0;');
end;

procedure kenwoodctl.setradiofreq(f: longint; m: modetype; vfo: char);
var freqstr: string;
begin
   str(f,freqstr);
   while length(freqstr) < 11 do freqstr := '0' + freqstr;
   if (vfo = 'A') then
   begin
      freq := f;
      fromrigstart := 0; //new frequency so clear radio buffer and state
      fromrigend := 0;
      while radioport.charready do radioport.readchar();
      torigstart := 0;
      torigend := 0;
      commandcount := 0;
      commandretrycount := 0;
   end;
   sendstring('F' + VFO + freqstr + ';');
   if vfo = 'B' then begin
      sendstring('FR1;FT1;');
   end;
   case m of
      CW: sendstring('MD3;');
      Digital: sendstring('MD6;');
      else
      if (freq < 10000000) then
         sendstring('MD1;') //lsb
      else 
         sendstring('MD2;'); //usb
   end;
   mode := m;
   if (vfo = 'B') then sendstring('FA;FR0;FT0;');
end;

procedure kenwoodctl.clearrit;
begin
   sendstring('RC;');
end;

procedure kenwoodctl.bumpritup;
begin
   sendstring('RU;');
end;

procedure kenwoodctl.bumpritdown;
begin
   sendstring('RD;');
end;

procedure kenwoodctl.bumpvfoup;
begin
   sendstring('UP;');
end;

procedure kenwoodctl.bumpvfodown;
begin
   sendstring('DN;');
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
      if c = ';' then
      begin
         waiting := false;
         i := 0;
         while fromrigstart <> fromrigend do
         begin
            inc(i);
            response[i] := fromrig[fromrigstart];
            fromrigstart := (fromrigstart + 1) mod rigbuffersize;
         end;
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
            end;
         end;
         if (response[1] = 'I') and (response[2] = 'F') then
         begin
            delete(response,1,2);
            val(copy(response,1,11),freqnow,code);
            if code = 0 then freq := freqnow;
            delete(response,1,27);
            case response[1] of
                '1', '2', '4', '5': mode := Phone;
                '6', '9': mode := Digital;
                 else mode := cw;
            end;
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
