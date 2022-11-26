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
unit kenwood;

interface

uses rig, timer, tree;

type
   kenwoodctl = class(radioctl)
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
      function  getresponsetimeout:longint;override;
      procedure timer(caughtup: boolean);override;
      procedure directcommand(s: string);override;

      FUNCTION CalmDownEveryone: BOOLEAN;override;
      FUNCTION DirectCommandAndResponse (RadioCommand: STRING): STRING;override;
      FUNCTION K3IsStillTalking: BOOLEAN;override;

      private
         commandtime: longint;
         commandcount: longint;
         commandretrycount: longint;
         commandmaxretry: longint;

         DirectCommandSent: BOOLEAN;
         ignorefreq: boolean;
         ignorefreqcount: longint;
         intosplit: string;
         outofsplit: string;
         tovfob: string;
         tovfoa: string;

         RadioResponse: STRING;

         ritup: string;
         ritdn: string;
         ritclr: string;
         vfoup: string;
         vfodn: string;
         cwmode: string;
         cwmoder: string;
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
      constructor create(debugin: boolean);override;
   end;

   ftdx3000ctl = class(kenwoodctl)
      public
      constructor create(debugin: boolean);override;
   end;


implementation

constructor kenwoodctl.create(debugin: boolean);
begin
   inherited create(debugin);
   commandtime := 200;
   commandcount := 0;
   commandmaxretry := 2;
   commandretrycount := 0;

   DirectCommandSent := FALSE;

   ignorefreq := false;
   ignorefreqcount := 0;
   intosplit := 'FR0;FA;FT1;';
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
   cwmoder := 'MD7;';
   cwmode := 'MD3;';
   digitalmode := 'MD6;';
   lsbmode := 'MD1;';
   usbmode := 'MD2;';
   bumpcount := 0;
   bumptime := 200;
   bumpignore := false
end;

constructor ftdx5000ctl.create(debugin: boolean);
begin
   inherited create(debugin);
   commandtime := 200;
   commandcount := 0;
   commandmaxretry := 2;
   commandretrycount := 0;
   ignorefreq := false;
   ignorefreqcount := 0;
   intosplit := 'FR0;FA;FT1;';
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
   cwmoder := 'MD07;MD17';
   digitalmode := 'MD06;MD16;';
   lsbmode := 'MD01;MD11;';
   usbmode := 'MD02;MD12;';
   bumpcount := 0;
   bumptime := 200;
   bumpignore := false
end;

constructor ftdx3000ctl.create(debugin: boolean);
begin
   inherited create(debugin);
   commandtime := 200;
   commandcount := 0;
   commandmaxretry := 2;
   commandretrycount := 0;
   ignorefreq := false;
   ignorefreqcount := 0;
   intosplit := 'FR0;FT3';
   outofsplit := 'FR0;FT2;';
   tovfob := 'FR4;FT3;';
   tovfoa := 'FR0;FT2;';
   freqpos := 6;
   freqdigits := 8;
   modepos := 21;
   responselength := 27;
   ritup := 'RU0010;';
   ritdn := 'RD0010;';
   ritclr := 'RC;';
   vfoup := 'EU001;';
   vfodn := 'ED001;';
   cwmode := 'MD03;';
   cwmoder := 'MD07;';
   digitalmode := 'MD06;';
   lsbmode := 'MD01;';
   usbmode := 'MD02;';
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
      CW: if cwreverse then
             sendstring(cwmoder)
          else
             sendstring(cwmode);
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



FUNCTION kenwoodctl.K3IsStillTalking: BOOLEAN;

{ Returns the current state of the txon parameter.  Note that this normally
  comes from the IF; command which has significant latency.  If you want a
  quicker response - you should set K3TXPollMode to TRUE using the
  SetK3RXPollMode procedure.  }

    BEGIN
    K3IsStillTalking := txon;
    END;



FUNCTION kenwoodctl.CalmDownEveryone: BOOLEAN;

{ This can be used to quiet the communication between the host and radio
  so that we can do a direct command without stomping on any communication.

  It will initially set a flag to stop any further initiation of polling
  commands.

  It will finally return TRUE after the "frequency is clear".  It is assumed
  that whatever procedure that does the direction communication will do
  what it takes to resume normal polling after completed. }

    BEGIN
    { First - disable polling if not already done }

    IF PollRadio THEN
        BEGIN
        SetPollRadio (False);
        CalmDownEveryone := False;
        Exit;
        END;

    { If we are waiting for a response to a previously sent request - we
      have not established a clear channel yet }

    CalmDownEveryone := NOT Waiting;
    END;



FUNCTION kenwoodctl.DirectCommandAndResponse (RadioCommand: STRING): STRING;

{ Will disable normal polling and send a command to the radio and return the
  response string.  Normal polling will resume once complete.  If no response
  is generated within a certain amount of time - the function will return with
  a null string.

  Make sure you call this after CalmDownEveryone went TRUE }

VAR i, j, k: INTEGER;
    Command: STRING;

    BEGIN
    DirectCommandAndResponse := '';   { Default upon exit }

    IF NOT DirectCommandSent THEN
        BEGIN

        { We are going to take over communication to send the command right away }

        SendString (RadioCommand);  { puts it into the send string buffer }

        i := 0;
        j := torigstart;

        WHILE j <> torigend do
            BEGIN
            inc(i);
            command [i] := torig[j];
            j := (j+1) mod rigbuffersize;

            if command [i] = ';' then
                BEGIN
                setlength (command,i);
                torigstart := j;
                if debugopen then writeln(debugfile,'sending ' + command);
                for k := 1 to length(command) do radioport.putchar(command[k]);
                lastcommand := command;
                waiting := true;
                commandcount := 0;
                break;
                END;
            END;

        DirectCommandSent := True;
        RadioResponse := '';
        Exit;
        END;

    { We will now look to see if a response has come in for this commmand.  We use
      the normal buffer for receiving the response - but it is looking to see if
      DirectCommandSet is true.  If so, it will wait until there is a complete response
      and put into the variable RadioRespone.  Once we see this - we can return that
      string - clear DirectCommandSet and re-enable polling }

    IF RadioResponse <> '' THEN
        BEGIN
        DirectCommandAndResponse := RadioResponse;
        DirectCommandSent := False;
        SetPollRadio (True);
        END;
    END;



procedure kenwoodctl.timer (caughtup: boolean);

var c: char;
    response: string;
    command: string;
    i,j,k: integer;
    freqnow: longint;
    code: word = 0;

    BEGIN
    inherited timer (caughtup);

    if radioport = nil then exit;

    { Read in any characters that have arrived }

    WHILE radioport.charready do
        BEGIN
        c := radioport.readchar();
        if debugopen then write(debugfile,c);

        IF c <> ';' THEN { Not ; = add to response buffer }
            BEGIN
            fromrig[fromrigend] := c;
            fromrigend := (fromrigend + 1) mod rigbuffersize;
            END
        ELSE

            { We found the semi-colon indicating we have the whole response }

            BEGIN
            if debugopen then writeln(debugfile,c);

            waiting := false;     { No longer waiting for a response }
            i := 0;

            { Generate response string from buffer }

            WHILE fromrigstart <> fromrigend do
                BEGIN
                inc(i);
                response[i] := fromrig[fromrigstart];
                fromrigstart := (fromrigstart + 1) mod rigbuffersize;
                END;

            inc(i);
            response[i] := c;
            setlength(response,i);

            IF response [1] = '?' then
                 BEGIN
                 if debugopen then writeln(debugfile,'received ? resending');

                 inc(commandretrycount);

                 if commandretrycount <= commandmaxretry then
                    BEGIN
                    for k := 1 to length(lastcommand) do
                       begin
                       if debugopen then write(debugfile,lastcommand[k]);
                       radioport.putchar(lastcommand[k]);
                       end;
                    waiting := true;
                    if debugopen then writeln(debugfile);
                    END
                 else
                    BEGIN
                    waiting := false;
                    commandretrycount := 0;
                    commandcount := 0;
                    if debugopen then writeln(debugfile,'giving up on retries');
                    END;
                 END

            ELSE        { Not a ? }
                BEGIN
                { If we have issued a direct command - we will put the response
                  into RadioResponse }

                IF DirectCommandSent THEN
                    RadioResponse := Response;

                { We expect to either see a TQ or IF response }
                { The TQ response simply tells us if the radio is in TX or not }

                IF (response[1] = 'T') and (response[2] = 'Q') then
                    txon := response [3] <> '0';

                { IF response gives us frequency, mode and TX info }

                IF (response[1] = 'I') and (response[2] = 'F') and (length(response) = responselength) then
                    BEGIN
                    if debugopen then writeln (debugfile, 'polled information message');

                    val(copy(response,freqpos,freqdigits),freqnow,code);

                    if ignorefreq then
                        BEGIN
                        freqnow := freq;
                        inc(ignorefreqcount);
                        ignorefreq := (ignorefreqcount <= 2);
                        if debugopen then writeln(debugfile,'ignoring frequency');
                        END;

                    if code = 0 then freq := freqnow;

                    CASE Response [modepos] OF
                           '1', '2', '4', '5': mode := Phone;
                           '6', '9': mode := Digital;
                           else mode := cw;
                           END;  { of case mode }

                    { Parse if the transmitter is on - one character before the modpos }

                    txon := response[modepos - 1] = '1';
                    END; { of IF; response }

                END;     { end of processing valid (non ?) responses }
            END;  { end of semi-colon terminated response }
        END;  { of loop reading in any available characters }

    { We have read any characters that were coming in }

    if bumpignore then
        BEGIN
        inc(bumpcount);
        bumpignore := (bumpcount <= bumptime);
        END;

   { Check waiting time to see if we are timed out }

    IF Waiting THEN
        BEGIN
        inc(commandcount);

        IF commandcount >= commandtime then
           BEGIN
           if debugopen then writeln(debugfile,'time out flushing read buffer');

           { Dump any characters coming from the radio }

           while radioport.charready do radioport.readchar();

           { Reinitialize everything }

           fromrigstart := 0;
           fromrigend := 0;
           commandcount := 0;
           commandretrycount := 0;
           Waiting := false;
           END;
        END;

    { If we are still waiting - then we aren't going to send any commands }

    IF Waiting THEN Exit;

    { We are adding another type of request instead of the IF; here.  In order to
      see when a transmission is completed more quickly, we have the ability to
      set the k3tightloop parameter to TRUE and send the TQ; command instead. }

    IF PollRadio AND ((PollCounter >= PollTime) OR K3TXPollMode) THEN
        BEGIN
        IF K3TXPollMode THEN      { Ask for TX state only }
            BEGIN
            SendString ('TQX;');
            END
        ELSE
            SendString ('IF;');   { Ask for everything }

        PollCounter := 0;
        END;

    Inc (pollcounter);

    { If we have any bytes in the torig buffer, then send it to the radio.
      Note that this gets done regardless who put the command in there.  This
      is necessary so other commands like FA ones get sent }

    i := 0;
    j := torigstart;

    WHILE j <> torigend do
        BEGIN
        inc(i);
        command[i] := torig[j];
        j := (j+1) mod rigbuffersize;

        if command[i] = ';' then
            BEGIN
            setlength(command,i);
            torigstart := j;
            if debugopen then writeln(debugfile,'sending ' + command);
            for k := 1 to length(command) do radioport.putchar(command[k]);
            lastcommand := command;
            waiting := true;
            commandcount := 0;
            break;
            END;
        END;
    END;



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

function kenwoodctl.getresponsetimeout:longint;
begin
   getresponsetimeout := (commandtime*17) div 10;
end;

end.
