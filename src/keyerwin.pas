unit keyerwin;
{$mode objfpc}

INTERFACE

USES trcrt,communication,beep,foot,radio,keyers;

CONST
    CWBufferSize           = 1024;
    WinkeyerHangTime: Array [0..3] of Integer = (8, 9, 11, 15);

TYPE
 
    WinkeyerState = record
       modereg: Char;
       speed: Char;
       stconst: Char;
       weight: Char;
       leadin: Char;
       tail: Char;
       minwpm: Char;
       wpmrange: Char;
       xtnd: Char;
       kcomp: Char;
       farns: Char;
       sampadj: Char;
       ditdahratio: Char;
       pincfg: Char;
       potrange: Char;
    end;

     WinkeyerBuffer = Array [0..CWBufferSize-1] of Char;

     WinKeyer = class(keyer)

     private
        CodeSpeed:        INTEGER;
        curtmode: CurtisMode;
        KeyerInitialized: BOOLEAN;
//        ElementLength:         INTEGER;
//        ElementLengthConstant: INTEGER;
//        PaddleActive:            BOOLEAN;
        PaddleBug:               BOOLEAN;
//        PaddlePTTOffTimer:      INTEGER;
        PaddleMonitorTone:      INTEGER;
        PaddleSpeed:            INTEGER;
        SwapPaddles:              BOOLEAN;
        HangTime: Integer;

        CountsSinceLastCW: LONGINT;
        FarnsworthEnable:  BOOLEAN;
        FarnsworthSpeed:   INTEGER;
        MonitorTone:            INTEGER;
        PTTEnable:              BOOLEAN;
        PTTTurnOnDelay:         INTEGER;
//        RememberCodeSpeed:        INTEGER;
        TuningWithDits:           BOOLEAN;
        Tuning:             BOOLEAN;
        TuneWithDits:       BOOLEAN;
        Weight:             Integer;
        CWBuffer: WinKeyerBuffer;
        CWBufferStart:         INTEGER;
        CWBufferEnd:           INTEGER;
        PTTAsserted:    BOOLEAN; { Indicates that the PTT signal is asserted.
                               Comes on PTTTurnOnDelay before CW starts.  }
        PTTFootSwitch:  BOOLEAN; { Indicates that the footswitch wants the PTT
                               to be on. }
//        DoingPaddle:  BOOLEAN;
        LastFootSwitchStatus: BOOLEAN;
//        SendStatus:           SendStatusType;
        FsCwGrant: Boolean;
        Footsw: FootSwitchx;
        WinkeyerPort: serialportx;
        pincfg: char;
        mirror: array [0..127] of char;
        mirrorstart: integer;
        mirrorend: integer;
        xoff: boolean;
        wait: boolean;
        breakin: boolean;
        busy: boolean;
        status: integer;
        Winkeyer1: boolean;
        WinkeyerFreqs: Array [1..10] of Integer;

        Function getMode:char;
        Function getSideTone:char;
        procedure flushLocal;
        function echoTest:boolean;
        
    public

        Constructor create;
        PROCEDURE AddCharacterToBuffer (Character: CHAR);override;
        PROCEDURE AddStringToBuffer (Msg: String; Tone: INTEGER);override;
        FUNCTION  BufferEmpty: BOOLEAN;override;
        FUNCTION  CWStillBeingSent: BOOLEAN;override;
        FUNCTION  DeleteLastCharacter: BOOLEAN;override;

        PROCEDURE FlushCWBuffer;override;
        PROCEDURE InitializeKeyer;override;
        PROCEDURE PTTForceOn;override;
        PROCEDURE PTTUnForce;override;
        PROCEDURE SetSpeed (Speed: INTEGER);override;
        Function GetSpeed:INTEGER;override;
        PROCEDURE UnInitializeKeyer;override;
        PROCEDURE SetPaddlePttHoldCount(Count: INTEGER);override;
        PROCEDURE SetPaddleBug(On: Boolean);override;
        PROCEDURE SetPaddleMonitorTone(tone: Integer);override;
        PROCEDURE SetPaddleSpeed(speed: Integer);override;
        PROCEDURE LetFootSwitchControlPtt;override;
        PROCEDURE Timer;override;
        Function GetPaddleBug:boolean;override;
        Function GetPaddleMonitorTone:integer;override;
        Function GetPaddleSpeed:integer;override;
        Function GetPaddlePTTHoldCount:integer;override;
        PROCEDURE SetWeight(w: real);override;
        Function GetWeight:real;override;
        PROCEDURE SetFarnsworthEnable(on: boolean);override;
        Function GetFarnsworthEnable:boolean;override;
        PROCEDURE SetFarnsworthSpeed(speed: integer);override;
        Function GetFarnsworthSpeed:integer;override;
        PROCEDURE SetPTTTurnOnDelay(delay: integer);override;
        Function GetPTTTurnOnDelay:integer;override;
        Procedure SetPTTEnable(on: boolean);override;
        Function GetPTTEnable:boolean;override;
        Procedure SetTuneWithDits(on: boolean);override;
        Function GetTuneWithDits:boolean;override;
        Procedure SetMonitorTone(tone: integer);override;
        Function GetMonitorTone:integer;override;
        Procedure SetCountsSinceLastCW(count: integer);override;
        Function GetCountsSinceLastCW:integer;override;
        Procedure SetCurtisMode(m: CurtisMode);override;
        Function GetCurtisMode:CurtisMode;override;
        Function GetKeyerInitialized:boolean;override;
        Procedure SetSwapPaddles(on: boolean);override;
        Function GetSwapPaddles:boolean;override;
        Function GetPTTAsserted:boolean;override;
        Procedure SetPTTFootSwitch(on :boolean);override;
        Procedure SetActiveRadio(r: RadioType);override;
        Procedure SetFootSwitch(f: FootSwitchx);override;
        Procedure SetCwGrant(on: boolean);override;

        Procedure SetPort(port: serialportx);

        END;

IMPLEMENTATION

//Uses trcrt,keycode,linuxsound,xkb,sysutils;
Uses keycode,linuxsound,xkb,sysutils;

Constructor WinKeyer.create;
begin
   LastFootSwitchStatus := false;
   CountsSinceLastCW := 0;
   CodeSpeed := 35;
   PaddleBug := false;
   CurtMode := ModeB;
   KeyerInitialized := False;
   MonitorTone := 700;
   PaddleMonitorTone := 700;
   Tuning := False;
   Weight := 50;
   PTTFootSwitch := False;
   TuningWithDits := False;
   TuneWithDits := False;
   FsCwGrant := False;
   WinkeyerPort := nil;
   PTTEnable := false;
   xoff := false;
   breakin := false;
   busy := false;
   wait := false;
   status := $c0;
   swappaddles := false;
end;

function Winkeyer.echoTest:boolean;
var i: integer;
begin
   WinKeyerPort.putchar(Char($00)); //echo test
   WinKeyerPort.putchar(Char($04)); //echo test
   WinKeyerPort.putchar(Char($55)); //test character
   for i := 0 to 500 do // wait up to 500ms
   begin
      delay(1);
      if WinKeyerPort.charready then break;
   end;
   i := 0; //check if returned test character
   if WinKeyerPort.charready then i := Integer(WinKeyerPort.readchar);
   echoTest := (i = $55);
end;

Procedure WinKeyer.InitializeKeyer;
var i,f: integer;
    passed: boolean;
begin
   CWBufferStart    := 0;
   CWBufferEnd      := 0;
   mirrorstart := 0;
   mirrorend := 0;
   PTTAsserted := False;
   WinKeyerPort.setparams(1200,8,NoParity,2);
   WinKeyerPort.ptt(false); //turn off rts if winkeyer powered from serial port
   WinKeyerPort.key(true); //turn on dtr if winkeyer powered from serial port
   delay(700); //wait for winkeyer to power up
   WinKeyerPort.putchar(Char($13)); //magic incantation
   WinKeyerPort.putchar(Char($13));
   WinKeyerPort.putchar(Char($13));
   WinKeyerPort.putchar(Char($13));
   while WinKeyerPort.charready do WinKeyerPort.readchar; //purge garbage
   passed := echoTest; //echo test
   if not passed then passed := echotest; //try again
   if not passed then
   begin
      clrscr;
      writeln('Winkeyer not responding -- check connection');
      halt;
   end;

   Winkeyer1 := true;
   WinKeyerPort.putchar(Char($00)); //open host mode
   WinKeyerPort.putchar(Char($02)); //open host mode
   for i := 0 to 5000 do // wait up to 5000ms
   begin
      delay(1);
      if WinKeyerPort.charready then break;
   end;
   i := 0;
   if WinKeyerPort.charready then i := Integer(WinKeyerPort.readchar);
   if i = 0 then
   begin
      writeln('Winkeyer version not returned -- check connection');
      halt;
   end
   else
   begin
      if i > 10 then
      begin
         Winkeyer1 := false;
         WinKeyerPort.putchar(Char($00)); //close host
         WinKeyerPort.putchar(Char($03));
         WinKeyerPort.putchar(Char($00)); //wk2 mode
         WinKeyerPort.putchar(Char($11)); //wk2 mode
         WinKeyerPort.putchar(Char($00)); //open host mode
         WinKeyerPort.putchar(Char($02)); //open host mode
         for i := 0 to 5000 do // wait up to 5000ms
         begin
            delay(1);
            if WinKeyerPort.charready then break;
         end;
         i := 0;
         if WinKeyerPort.charready then i := Integer(WinKeyerPort.readchar);
         if i = 0 then
         begin
            writeln('Winkeyer 2 version not returned -- check connection');
            halt;
         end
      end;
   end;
   if Winkeyer1 then f := 3759 else f := 4000;
   for i := 1 to 10 do
   begin
      WinkeyerFreqs[i] := f div i;
   end;
   
   WinKeyerPort.putchar(Char($0f)); // load current values
   WinKeyerPort.putchar(getMode);
   WinKeyerPort.putchar(Char(CodeSpeed));
   WinKeyerPort.putchar(getSideTone); //possibly change this to load a variable
                                      //that is updated with monitor tone 
   WinKeyerPort.putchar(Char(weight));
   WinKeyerPort.putchar(Char(PTTTurnOnDelay));
   WinKeyerPort.putchar(Char($02)); //tail
   WinKeyerPort.putchar(Char(5)); //5 wpm min for speed pot
   WinKeyerPort.putchar(Char(85)); //99 wpm max for speed pot
   WinKeyerPort.putchar(Char($00)); //time before keying starts
   WinKeyerPort.putchar(Char($00)); //key comp
   if FarnsworthEnable then
      WinKeyerPort.putchar(Char(FarnsworthSpeed))
   else
      WinKeyerPort.putchar(Char($00));
   WinKeyerPort.putchar(Char(50)); //paddle set point
   WinKeyerPort.putchar(Char(50)); //ratio
   if Winkeyer1 then
      pincfg := Char($04)  //radio1 no sidetone or ptt
   else
      pincfg := Char($07); //radio1 sidetone enable
   WinKeyerPort.putchar(pincfg);
   WinKeyerPort.putchar(Char($ff)); //don't care
   KeyerInitialized := True;
end;

Function WinKeyer.getMode:char;
var mode: integer;
begin
   mode := $84; //serial echo back -- no paddle watchdog
   if paddlebug then
      mode := mode or $30
   else
      if CurtMode = ModeA  then mode := mode or $10;
      if CurtMode = Ultimatic then mode := mode or $20;
   if SwapPaddles then mode := mode or $08;
   getMode := Char(mode);
end;

Function WinKeyer.getSideTone:char;
var st,tone,df,df0,i,j: integer;
begin
   st := $00;
   tone := monitortone;
   if monitortone = 0 then
   begin
      tone := paddlemonitortone;
      st := $80;
   end;
   if tone > WinKeyerFreqs[10] then tone := WinKeyerFreqs[10];
   if tone < WinKeyerFreqs[1] then tone := WinKeyerFreqs[1];
   df0 := 4000;
   j := 10;
   for i := 1 to 10 do
   begin
      df := Abs(tone-WinKeyerFreqs[i]);
      if (df < df0) then
      begin
        j := i;
        df := df0;
      end
      else
      begin
        j := i-1;
        break;
      end;
   end;
   st := st + j;
   getSideTone := char(st);
end;

PROCEDURE WinKeyer.SetWeight(w: real);
begin
   weight := round(50.0*w);
   if weight < 25 then weight := 25;
   if weight > 75 then weight := 75;
   if KeyerInitialized then 
   begin
      WinKeyerPort.putchar(Char($03));
      WinKeyerPort.putchar(Char(weight));
   end;
end;

Function WinKeyer.GetWeight:real;
begin
   GetWeight := weight*0.02;
end;

Procedure WinKeyer.SetPTTTurnOnDelay(delay: integer);
begin
   PTTTurnOnDelay := round(real(delay)*0.17);
   if PTTTurnOnDelay > 250 then PTTTurnOnDelay := 250;
   if KeyerInitialized then 
   begin
      WinKeyerPort.putchar(Char($04));
      WinKeyerPort.putchar(Char(PTTTurnOnDelay));
      WinKeyerPort.putchar(Char(0));
   end;
end;

Function WinKeyer.GetPTTTurnOnDelay:integer;
begin
   GetPTTTurnOnDelay := Round(10.0*real(PTTTurnOnDelay)/1.7);
end;

Procedure WinKeyer.SetActiveRadio(r: RadioType);
begin
   flushcwbuffer; //probably not necessary
   Pincfg := Char(Integer(Pincfg) and $f3); 
   case r of
      RadioOne:
      begin
         Pincfg := Char(Integer(Pincfg) or $04);
      end;

      RadioTwo:
      begin
         Pincfg := Char(Integer(Pincfg) or $08);
      end;
   end;
   if KeyerInitialized then 
   begin
      WinKeyerPort.putchar(Char($09));
      WinKeyerPort.putchar(Pincfg);
   end;
end;

Procedure WinKeyer.SetPaddlePttHoldCount(Count: INTEGER);
var i,diff0,diff,j: integer;
begin
   diff0 := abs(Count-WinkeyerHangTime[0]);
   j := 0;
   for i := 1 to 3 do
   begin
      diff := abs(count-WinkeyerHangTime[i]);
      if (diff < diff0) then
      begin
         j := i;
         diff0 := diff;
      end;
   end;
   HangTime := j;
   Pincfg := Char(Integer(Pincfg) and $8f);
   Pincfg := Char(Integer(Pincfg) or (HangTime shl 4));
   if KeyerInitialized then 
   begin
      WinKeyerPort.putchar(Char($09));
      WinKeyerPort.putchar(Pincfg);
   end;
end;

Function WinKeyer.GetPaddlePttHoldCount:integer;
begin
   GetPaddlePttHoldCount := WinkeyerHangTime[HangTime];
end;

Procedure WinKeyer.PTTForceOn;
begin
end;

Procedure WinKeyer.PTTUnForce;
begin
end;

procedure WinKeyer.SetSpeed (Speed: integer);
begin
   CodeSpeed := Speed;
   if CodeSpeed > 99 then CodeSpeed := 99;
   if CodeSpeed < 5 then CodeSpeed := 5;
   if KeyerInitialized then 
   begin
      WinKeyerPort.putchar(Char($02));
      WinKeyerPort.putchar(Char(CodeSpeed));
   end;
end;

function WinKeyer.GetSpeed:integer;
begin
   GetSpeed := CodeSpeed;
end;

procedure WinKeyer.SetSwapPaddles(on: boolean);
begin
   SwapPaddles := on;
   if KeyerInitialized then 
   begin
      flushcwbuffer;
      while WinKeyerPort.charready do WinKeyerPort.readchar; //purge garbage
      WinKeyerPort.putchar(Char($0e));
      WinKeyerPort.putchar(getMode);
   end;
end;

function WinKeyer.GetSwapPaddles:boolean;
begin
   GetSwapPaddles := SwapPaddles;
end;

procedure WinKeyer.SetCurtisMode(m: CurtisMode);
begin
   CurtMode := m;
   if KeyerInitialized then 
   begin
      flushcwbuffer;
      while WinKeyerPort.charready do WinKeyerPort.readchar; //purge garbage
      WinKeyerPort.putchar(Char($0e));
      WinKeyerPort.putchar(getMode);
   end;
end;

function WinKeyer.GetCurtisMode:CurtisMode;
begin
   GetCurtisMode := CurtMode;
end;

procedure WinKeyer.SetPaddleBug(on: boolean);
begin
   PaddleBug := on;
   if KeyerInitialized then 
   begin
      flushcwbuffer;
      while WinKeyerPort.charready do WinKeyerPort.readchar; //purge garbage
      WinKeyerPort.putchar(Char($0e));
      WinKeyerPort.putchar(getMode);
   end;
end;

function WinKeyer.GetPaddleBug:boolean;
begin
   GetPaddleBug := PaddleBug;
end;

function WinKeyer.GetKeyerInitialized:boolean;
begin
   GetKeyerInitialized := KeyerInitialized;
end;

procedure WinKeyer.AddCharacterToBuffer (Character: char);
var c: char;
begin
   c := upcase(Character);
   case c of
      'A'..'Z', '0'..'9', '.', ',', '?', '/', '+', '=', ' ':
      begin
         CWBuffer[CWBufferEnd] := c;
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
      end;

      '<':
      begin
         CWBuffer[CWBufferEnd] := '>';
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
      end;

//other winkeyer characters
      '"','#','$','%',Char($27),'(',')','*',':',';','>','@','-':
      begin
         CWBuffer[CWBufferEnd] := c;
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
      end;

      Char(134), Char(143):
      begin // A-ring
         CWBuffer[CWBufferEnd] := Char($1b);
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
         CWBuffer[CWBufferEnd] := 'W';
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
         CWBuffer[CWBufferEnd] := 'A';
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
      end;

      Char(132), Char(142):
      begin // A-umlaut
         CWBuffer[CWBufferEnd] := ';';
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
      end;

      
      Char(148), Char(153):
      begin
         CWBuffer[CWBufferEnd] := Char($1b);
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
         CWBuffer[CWBufferEnd] := 'O';
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
         CWBuffer[CWBufferEnd] := 'E';
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
      end;

      '!':
      begin
         CWBuffer[CWBufferEnd] := Char($1b);
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
         CWBuffer[CWBufferEnd] := 'S';
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
         CWBuffer[CWBufferEnd] := 'N';
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
      end;

      '&':
      begin
         CWBuffer[CWBufferEnd] := '['; // AS -- wait character
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
      end;

      ControlE:
      begin //73 percent dah not implemented
      end;

        ControlDash:
        begin
           //150 percent dah not implemented
        end;

        ControlK:
        begin
           //normal dah not implemented
        end;

        ControlN:
        begin
           //113 percent dah not implemented
        end;

        ControlO:
        begin
          //127 percent dah not implemented
        end;

        ControlP:
        begin
          //60 percent dit not implemented
        end;

        ControlQ:
        begin
          //80 percent dit not implemented
        end;

        ControlBackSlash:
        begin
          //normal dit not implemented
        end;

        ControlV:
        begin
          //120 percent dit not implemented
        end;

        ControlL:
        begin
          //140 percent dit not implemented
        end;

        ControlF:   { Speed up command }
        begin
          //increase speed by 6 percent
           CWBuffer[CWBufferEnd] := c;
           CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
        end;

        ControlS:   { Slow down command }
        begin
          //decrease speed by 6 percent
           CWBuffer[CWBufferEnd] := c;
           CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
        end;


        ControlX:   { Decrease weight command }
        begin
           //decrease weight by .03 not implemented
        end;
 
        ControlY:   { Increase weight command }
        begin
           //increase weight by .03 not implemented
        end;

        '^':
        begin
           CWBuffer[CWBufferEnd] := '|';
           CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
        end;
    end;
end;

Procedure WinKeyer.SetPTTEnable(on: boolean);
begin
   if Winkeyer1 then exit; //no ptt or sidetone for WK1
   PTTEnable := on;
   if PTTEnable then
       pincfg := Char(Integer(pincfg) or 1)
   else
       pincfg := Char(Integer(pincfg) and $fe);
   if KeyerInitialized then 
   begin
      WinKeyerPort.putchar(Char($09));
      WinKeyerPort.putchar(Pincfg);
   end;
end;

Function WinKeyer.GetPTTEnable:boolean;
begin
   GetPTTEnable := PTTEnable;
end;

procedure WinKeyer.UninitializeKeyer;
begin
   if KeyerInitialized then 
   begin
      WinKeyerPort.putchar(Char($00));
      WinKeyerPort.putchar(Char($03));
   end;
end;

procedure WinKeyer.SetTuneWithDits(on: boolean);
begin
   TuneWithDits := on;
end;

Function WinKeyer.GetTuneWithDits:boolean;
begin
   GetTuneWithDits := TuneWithDits;
end;

function WinKeyer.GetCountsSinceLastCW:integer;
begin
   GetCountsSinceLastCW := CountsSinceLastCw;
end;

procedure WinKeyer.SetCountsSinceLastCW(count: integer);
begin
   CountsSinceLastCW := count;
end;

function WinKeyer.GetFarnsworthEnable:boolean;
begin
   GetFarnsworthEnable := FarnsworthEnable;
end;

procedure WinKeyer.SetFarnsworthEnable(on: boolean);
begin
   FarnsworthEnable := on;
   if KeyerInitialized then
   begin
      WinKeyerPort.putchar(Char($0d));
      if FarnsworthEnable then
         WinKeyerPort.putchar(Char(FarnsworthSpeed))
      else
         WinKeyerPort.putchar(Char($00));
   end;
end;

procedure WinKeyer.SetFarnsworthSpeed(speed: integer);
begin
   FarnsworthSpeed := speed;
   SetFarnsworthEnable(FarnsworthEnable);
end;

function WinKeyer.GetFarnsworthSpeed:integer;
begin
    GetFarnsworthSpeed := FarnsworthSpeed;
end;

procedure WinKeyer.SetPTTFootSwitch(on :boolean);
begin
   PTTFootSwitch := on;
end;

function WinKeyer.GetMonitorTone:integer;
begin
   GetMonitorTone := MonitorTone;
end;

procedure WinKeyer.SetMonitorTone(tone: integer);
begin
   MonitorTone := tone;
   if KeyerInitialized then
   begin
      WinKeyerPort.putchar(Char($01));
      WinKeyerPort.putchar(getSideTone);
      if (MonitorTone = 0) and (PaddleMonitorTone = 0) then
         pincfg := Char(Integer(pincfg) and (not $02))
      else
         pincfg := Char(Integer(pincfg) or $02);
      WinKeyerPort.putchar(Char($09));
      WinKeyerPort.putchar(Pincfg);
   end;
end;

procedure WinKeyer.SetPaddleMonitorTone(tone: integer);
begin
   PaddleMonitorTone := tone;
   if KeyerInitialized then
   begin
      WinKeyerPort.putchar(Char($01));
      WinKeyerPort.putchar(getSideTone);
      if (MonitorTone = 0) and (PaddleMonitorTone = 0) then
         pincfg := Char(Integer(pincfg) and (not $02))
      else
         pincfg := Char(Integer(pincfg) or $02);
      WinKeyerPort.putchar(Char($09));
      WinKeyerPort.putchar(Pincfg);
   end;
end;

function WinKeyer.GetPaddleMonitorTone:integer;
begin
   GetPaddleMonitorTone := PaddleMonitorTone;
end;

procedure WinKeyer.setpaddlespeed(speed: integer);
begin
   paddlespeed := speed; //does nothing for now
end;

procedure WinKeyer.SetCwGrant(on: boolean);
begin
   FsCwGrant := on;
end;

procedure WinKeyer.SetFootSwitch(f: FootSwitchx);
begin
   Footsw := f;
end;

procedure WinKeyer.LetFootSwitchControlPTT;
begin
   //does nothing for now
end;

Function WinKeyer.GetPaddleSpeed:integer;
begin
   getpaddlespeed := paddlespeed;
 //does nothing for now
end;

Function WinKeyer.GetPTTAsserted:boolean;
begin
   getPTTAsserted := PTTAsserted;
end;

procedure WinKeyer.SetPort(port: serialportx);
begin
   WinKeyerPort := port;
end;

procedure WinKeyer.Timer;
var
    c,c1,c2,c3: char;
    i: integer;
begin
   if not KeyerInitialized then exit;
   if ctrlshift then
   begin
      if not tuning then
      begin
         tuning := true;
         if tunewithdits then
         begin
            flushcwbuffer;
            while WinKeyerPort.charready do WinKeyerPort.readchar;
            WinKeyerPort.putchar(Char($02));
            WinKeyerPort.putchar(Char(75));
            WinKeyerPort.putchar(Char($14));
            WinKeyerPort.putchar(Char($02)); //opposite of wk2 documentation
         end
         else
         begin
            while WinKeyerPort.charready do WinKeyerPort.readchar;
            WinKeyerPort.putchar(Char($0b));
            WinKeyerPort.putchar(Char($01));
         end;
      end;
   end
   else if tuning then
   begin
      tuning := false;
      if tunewithdits then
      begin
         WinKeyerPort.putchar(Char($14));
         WinKeyerPort.putchar(Char($00));
      end
      else
      begin
         WinKeyerPort.putchar(Char($0b));
         WinKeyerPort.putchar(Char($00));
      end;
      WinKeyerPort.putchar(Char($02));
      WinKeyerPort.putchar(Char(CodeSpeed));
   end;

   while WinKeyerPort.charready do
   begin
      c := WinKeyerPort.readchar;
      if (Integer(c) and $c0) = $c0 then
      begin
         if (Integer(c) and $c8) = $c0 then
         begin
            status := Integer(c);
            xoff := (Integer(c) and $01) = $01;
            breakin := (Integer(c) and $02) = $02;
            busy := (Integer(c) and $04) = $04;
            wait := (Integer(c) and $10) = $10;
            if breakin then flushlocal;
         end
         else
         begin
         end;
      end
      else
      begin
         if (Integer(c) and $c0) = $80 then
         begin
           // speed pot
         end
         else
         begin
            //echo back?
            if (mirrorstart <> mirrorend) then
            begin
               if (c = mirror[mirrorstart]) then
               begin
                  mirrorstart := (mirrorstart+1) mod 128;
               end
               else
               begin
                  flushcwbuffer;
               end;
            end;
         end;
      end;
   end;

   if (cwbufferend <> cwbufferstart) and (not xoff) then
   begin
      i := Integer(cwbuffer[cwbufferstart]);
      case i of

         $18..$1a,$1d:
         begin
            c1 := Char(i);
            cwbufferstart := (cwbufferstart+1) mod CWBufferSize;
            if (cwbufferend <> cwbufferstart) then
            begin
               c2 := cwbuffer[cwbufferstart];
               cwbufferstart := (cwbufferstart+1) mod CWBufferSize;
               WinkeyerPort.putchar(c1);
               WinkeyerPort.putchar(c2);
            end;
         end;

         $1b:
         begin
            c1 := Char(i);
            cwbufferstart := (cwbufferstart+1) mod CWBufferSize;
            if (cwbufferend <> cwbufferstart) then
            begin
               c2 := cwbuffer[cwbufferstart];
               cwbufferstart := (cwbufferstart+1) mod CWBufferSize;
               if (cwbufferend <> cwbufferstart) then
               begin
                  c3 := cwbuffer[cwbufferstart];
                  cwbufferstart := (cwbufferstart+1) mod CWBufferSize;
                  WinkeyerPort.putchar(c1);
                  WinkeyerPort.putchar(c2);
                  WinkeyerPort.putchar(c3);
               end
           end;
         end;

         $1e,$1f,Integer('|'):
         begin
            c1 := Char(i);
            cwbufferstart := (cwbufferstart+1) mod CWBufferSize;
            WinkeyerPort.putchar(c1);
         end;

         $06:
         begin
            inc(CodeSpeed);
            if CodeSpeed > 25 then inc(CodeSpeed);
            if CodeSpeed > 35 then inc(CodeSpeed);
            if CodeSpeed > 45 then inc(CodeSpeed);
            if CodeSpeed > 99 then CodeSpeed := 99;
            WinkeyerPort.putchar(Char($1c));
            WinkeyerPort.putchar(Char(CodeSpeed));
            cwbufferstart := (cwbufferstart+1) mod CWBufferSize;
         end;

         $13:
         begin
            if CodeSpeed > 49 then Dec (CodeSpeed);
            if CodeSpeed > 38 then Dec (CodeSpeed);
            if CodeSpeed > 27 then Dec (CodeSpeed);
            if CodeSpeed > 6 then Dec (CodeSpeed);
            WinkeyerPort.putchar(Char($1c));
            WinkeyerPort.putchar(Char(CodeSpeed));
            cwbufferstart := (cwbufferstart+1) mod CWBufferSize;
         end;

         else
         begin
            c1 := Char(i);
            cwbufferstart := (cwbufferstart+1) mod CWBufferSize;
            mirror[mirrorend] := c1;
            mirrorend := (mirrorend+1) mod 128;
            WinkeyerPort.putchar(c1);
         end;

      end;

   end;

   if status = $c0 then
   begin
      if countssincelastcw = 0 then countssincelastcw := 1;
   end
   else
      countssincelastcw := 0;

   pttasserted := pttenable and (not (status = $c0));
end;

function WinKeyer.CWStillBeingSent:boolean;
begin
   CWStillBeingSent := (status <> $c0) or not bufferempty;
end;

function WinKeyer.BufferEmpty:boolean;
begin
   BufferEmpty := (cwbufferstart = cwbufferend) and (mirrorstart = mirrorend);
end;

procedure WinKeyer.AddStringToBuffer (Msg: String; Tone: INTEGER);
var i: integer;
begin
// fix this to allow commands
   for i := 1 to length(msg) do
   begin
      AddCharacterToBuffer(Msg[i]);
   end;
end;

function WinKeyer.DeleteLastCharacter:boolean;
begin
   if cwbufferstart <> cwbufferend then
   begin
      cwbufferend := (cwbufferend -1 + CWBufferSize) mod CWBufferSize;
      DeleteLastCharacter := true;
      exit;
   end;
   DeleteLastCharacter := ((mirrorend-mirrorstart+128) mod 128) > 1;
   if DeleteLastCharacter then
   begin
      WinKeyerPort.putchar(Char($08));
      mirrorend := (mirrorend + 127) mod 128;
   end;
end;

procedure WinKeyer.flushCwBuffer;
begin
   mirrorstart := 0;
   mirrorend := 0;
   cwbufferstart := 0;
   cwbufferend := 0;
   WinKeyerPort.putchar(Char($0a));
   WinKeyerPort.putchar(Char($1e));
end;

procedure WinKeyer.flushLocal;
begin
   mirrorstart := 0;
   mirrorend := 0;
   cwbufferstart := 0;
   cwbufferend := 0;
end;

END.
