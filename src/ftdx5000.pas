{$mode objfpc}
unit ftdx5000;

interface
uses kenwood,timer,tree;

type
   ftdx5000ctl = class(kenwoodctl)
      public
      procedure setradiofreq(f: longint; m: modetype; vfo: char);override;
      procedure clearrit;override;
      procedure bumpritup;override;
      procedure bumpritdown;override;
      procedure bumpvfoup;override;
      procedure bumpvfodown;override;
      procedure timer(caughtup: boolean);override;
      private
         commandtime: longint;
         commandcount: longint;
         commandretrycount: longint;
         commandmaxretry: longint;
   end;

implementation

procedure ftdx5000ctl.setradiofreq(f: longint; m: modetype; vfo: char);
var freqstr: string;
begin
   str(f,freqstr);
   while length(freqstr) < 8 do freqstr := '0' + freqstr;
   sendstring('F' + VFO + freqstr + ';');
   if vfo = 'B' then begin
      sendstring('FR1;FT1;');
   end
   else
   begin
      freq := f;
      fromrigstart := 0; //new frequency so clear radio buffer and state
      fromrigend := 0;
      torigstart := 0;
      torigend := 0;
      waiting := false;
   end;
   case m of
      CW: begin
         sendstring('MD03;');
         sendstring('MD13;');
      end;
      Digital: begin
         sendstring('MD06;');
         sendstring('MD16;');
      end;
      else
      if (freq < 10000000) then
      begin
         sendstring('MD01;') //lsb
         sendstring('MD11;') //lsb
      end
      else 
      begin
         sendstring('MD02;'); //usb
         sendstring('MD12;'); //usb
      end;
   end;
   mode := m;
   if (vfo = 'B') then sendstring('FA;FR0;FT0;');
end;

procedure ftdx5000ctl.bumpritup;
begin
   sendstring('RU0010;');
end;

procedure ftdx5000ctl.bumpritdown;
begin
   sendstring('RD0010;');
end;

procedure kenwoodctl.bumpvfoup;
begin
   sendstring('EU001;');
end;

procedure kenwoodctl.bumpvfodown;
begin
   sendstring('ED001;');
end;

procedure ftdx5000ctl.timer(caughtup: boolean);
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
            delete(response,1,5);
            val(copy(response,1,8),freqnow,code);
            if code = 0 then freq := freqnow;
            delete(response,1,15);
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

end.
