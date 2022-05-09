//Copyright Larry Tyree, N6TR, 2022
//New unit for support for SO2R Mini with TRCW firmware
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

unit keyerard;
{$mode objfpc}

INTERFACE

USES tree,trcrt,communication,beep,foot,radio,keyers;

TYPE
     ArduinoKeyer = class(keyer)

     private
        ComputerCodeSpeed:   INTEGER;
        curtmode:            CurtisMode;
        KeyerInitialized:    BOOLEAN;
        PaddleBug:           BOOLEAN;
        PaddleMonitorTone:   INTEGER;
        PaddleCWSpeed:       INTEGER;
        SwapPaddles:    BOOLEAN;
        FarnsworthEnable:   BOOLEAN;
        FarnsworthSpeed:    INTEGER;

        FootswitchControlPTT: BOOLEAN;

        MonitorTone:        INTEGER;
        PaddlePTTHoldCount: INTEGER;
        PTTAsserted:        BOOLEAN;
        PTTEnable:          BOOLEAN;
        PTTTurnOnDelay:     INTEGER;
        RelayImage:         INTEGER;  { Image of SO2R relay bits }
        TuningWithDits:     BOOLEAN;
        Tuning:             BOOLEAN;
        TuneWithDits:       BOOLEAN;
        Version:            STRING;
        Weight:             Integer;

        FsCwGrant: Boolean;
        Footsw: FootSwitchx;
        ArduinoKeyerPort: serialportx;
        xoff: boolean;
        wait: boolean;
        breakin: boolean;
        busy: boolean;
        status: integer;

        FUNCTION EchoTest: BOOLEAN;

    public

        Constructor create;

        PROCEDURE AddCharacterToBuffer (Character: CHAR);override;
        PROCEDURE AddStringToBuffer (Msg: String; Tone: INTEGER);override;
        FUNCTION  BufferEmpty: BOOLEAN;override;
        PROCEDURE ClearOutAnyIncomingCharacters;
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
        Procedure SetPTTFootSwitch(on: boolean);override;
        Procedure SetActiveRadio (radio: RadioType);override;
        Procedure SetFootSwitch(f: FootSwitchx);override;
        Procedure SetCwGrant(on: boolean);override;
        Procedure dvpptt(on: boolean);override;
        Procedure SetPort(port: serialportx);

        END;

IMPLEMENTATION

//Uses trcrt,keycode,linuxsound,xkb,sysutils;
Uses keycode,linuxsound,xkb,sysutils;

Constructor ArduinoKeyer.create;

   begin
   PaddleCWSpeed := 0;
   RelayImage    := 0;  { All relays cleared }
   PaddlePTTHoldCount := 10; { default }

   ComputerCodeSpeed := 35;
   PaddleBug := false;
   CurtMode := ModeB;
   KeyerInitialized := False;
   MonitorTone := 700;
   PaddleMonitorTone := 700;
   Tuning := False;
   Weight := 128;
   TuningWithDits := False;
   TuneWithDits := False;
   FsCwGrant := False;
   FootswitchControlPTT := False;
   ArduinoKeyerPort := nil;
   PTTEnable := false;
   xoff := false;
   breakin := false;
   busy := false;
   wait := false;
   status := $c0;
   end;

FUNCTION ArduinoKeyer.EchoTest: BOOLEAN;

VAR DelayLoops: INTEGER;
    RetryCount: INTEGER;

{ Returns true is the Arduino seems to be awake }

    BEGIN
    RetryCount := 0;
    ClearOutAnyIncomingCharacters;

    Version := '';
    ArduinoKeyerPort.PutChar (Char ($01));  { Ask for version }

    REPEAT
        DelayLoops := 0;

        REPEAT
            Delay (1);
            Inc (DelayLoops);
        UNTIL (DelayLoops > 200) OR ArduinoKeyerPort.CharReady;

        IF ArduinoKeyerPort.CharReady THEN
            BEGIN
            Version := Version + ArduinoKeyerPort.ReadChar;

            IF Length (Version) = 7 THEN  { We have enough characters }
                BEGIN
                EchoTest := Copy (Version, 1, 4) = 'TRCW';
                Exit;
                END;
            END
        ELSE
            BEGIN     { Here because we timed out }
            Inc (RetryCount);

            IF RetryCount > 20 THEN
                BEGIN
                ClrScr;
                WriteLn ('Unable to communicat with Arduino after ', RetryCount, ' retries');
                Halt;
                END;

            { Need to setup another attempt }

            ClearoutAnyIncomingCharacters;
            Version := '';
            ArduinoKeyerPort.PutChar (Char (1));  { Ask for version again }
            END;

        { We just keep going in this loop until we either give up or get an answer }
        UNTIL False;
    END;



Procedure ArduinoKeyer.InitializeKeyer;

{ So much of the initialization is done in the arduino code.  Any custom
  initialization should come from LOGCCFG commands, which we will assume
  are executed after the keyer is online.  }

   BEGIN
   IF KeyerInitialized THEN Exit;


   IF NOT EchoTest THEN
       BEGIN
       ClrScr;
       WriteLn ('Arduino Keyer not responding -- check connection');
       Halt;
       END;


   KeyerInitialized := True;
   END;



PROCEDURE ArduinoKeyer.SetWeight (W: REAL);

{ Integer is weight in 100th of a percent }

    BEGIN
    Weight := Round (100 * W);

    IF KeyerInitialized THEN
        BEGIN
        ArduinoKeyerPort.PutChar (Char ($06));
        ArduinoKeyerPort.PutChar (Char ( Lo (Weight)));
        END;
    END;



FUNCTION ArduinoKeyer.GetWeight: REAL;

{ We just report back what we sent last to the keyer }

    BEGIN
    GetWeight := Weight * (1 / $80);
    END;

PROCEDURE ArduinoKeyer.SetPTTTurnOnDelay (delay: integer);

{ The PTT Turn On Delay is the time between assertion of PTT until
  the first CW is sent.  The time is in milliseconds. }

   BEGIN
   PTTTurnOnDelay := delay;

   IF PTTTurnOnDelay > 250 then PTTTurnOnDelay := 250;

   IF KeyerInitialized THEN
       BEGIN
       ArduinoKeyerPort.PutChar (Char ($0E));
       ArduinoKeyerPort.PutChar (Char (PTTTurnOnDelay));
       END;
   END;

FUNCTION ArduinoKeyer.GetPTTTurnOnDelay: INTEGER;

    BEGIN
    GetPTTTurnOnDelay := PTTTurnOnDelay;
    END;


PROCEDURE ArduinoKeyer.SetActiveRadio (Radio: RadioType);

{ Determines which radio will have CW and PTT enabled.  It will
  also switch the microphone. Any CW in progress will be aborted. }

    BEGIN
    FlushCWBuffer;

    CASE Radio OF
        RadioOne:
            BEGIN
            RelayImage := RelayImage AND $FB;       { Set bit 2 to a zero }

            IF KeyerInitialized THEN
                BEGIN
                ArduinoKeyerPort.PutChar (Char ($13));  { Radio select command }
                ArduinoKeyerPort.PutChar (Char ($01));  { Radio One }
                ArduinoKeyerPort.PutChar (Char ($02));  { Relay control command }
                ArduinoKeyerPort.PutChar (Char (RelayImage));  { Relay control data }
                END;
            END;

        RadioTwo:
            BEGIN
            RelayImage := RelayImage OR $04;        { Set bit 2 to a one }

            IF KeyerInitialized THEN
                BEGIN
                ArduinoKeyerPort.PutChar (Char ($13));  { Radio select command }
                ArduinoKeyerPort.PutChar (Char ($02));  { Radio Two }
                ArduinoKeyerPort.PutChar (Char ($02));  { Relay control command }
                ArduinoKeyerPort.PutChar (Char (RelayImage));  { Relay control data }
                END;
            END;

        END;  { of case }
    END;


PROCEDURE ArduinoKeyer.SetPaddlePttHoldCount (Count: INTEGER);

{ Sets the # of ms that PTT will stay active after sending CW
  manually. }

    BEGIN
    PaddlePTTHoldCount := Count;

    IF KeyerInitialized THEN
        BEGIN
        ArduinoKeyerPort.PutChar (Char ($10));   { PTT Hold fimr for paddle sent CW command }
        ArduinoKeyerPort.PutChar (Char (Count)); { Hold time in ms }
        END;
    END;

FUNCTION ArduinoKeyer.GetPaddlePttHoldCount: INTEGER;

    BEGIN
    GetPaddlePTTHoldCount := PaddlePTTHoldCount;
    END;



PROCEDURE ArduinoKeyer.PTTForceOn;

{ Turns on the PTT signal - no if ands or buts...  well - if we implement it
  Arduino. }

    BEGIN
    IF KeyerInitialized THEN
        BEGIN
        ArduinoKeyerPort.PutChar (Char ($0A)); { PTT command }
        ArduinoKeyerPort.PutChar (Char ($01)); { PTT on }
        END;
    END;

Procedure ArduinoKeyer.PTTUnForce;

{ Turns off PTT signal - no if ands or buts }

    BEGIN
    IF KeyerInitialized THEN
        BEGIN
        ArduinoKeyerPort.PutChar (Char ($0A)); { PTT command }
        ArduinoKeyerPort.PutChar (Char ($00)); { PTT off }
        END;
    END;



PROCEDURE ArduinoKeyer.SetSpeed (Speed: INTEGER);

{ Sets the speed for computer sent CW.  If the Paddle CW Speed variable is
  zero - we also will set the Paddle Speed to the new speed. }

   BEGIN
   ComputerCodeSpeed := Speed;

   IF ComputerCodeSpeed > 99 THEN ComputerCodeSpeed := 99;
   IF ComputerCodeSpeed < 5  THEN ComputerCodeSpeed := 5;

   IF KeyerInitialized THEN
       BEGIN
       ArduinoKeyerPort.PutChar (Char ($08));  { Computer CW Speed command }
       ArduinoKeyerPort.PutChar (Char (ComputerCodeSpeed));

       { If we haven't set the Paddle CW Speed to anything - set it to this speed too }

       IF PaddleCWSpeed = 0 THEN
           BEGIN
           ArduinoKeyerPort.PutChar (Char ($09));  { Paddle CW Speed command }
           ArduinoKeyerPort.PutChar (Char (ComputerCodeSpeed));
           END;
       END;
   END;

FUNCTION ArduinoKeyer.GetSpeed: INTEGER;

{ Returns the ComputerCodeSpeed variable }

    BEGIN
    GetSpeed := ComputerCodeSpeed;
    END;



PROCEDURE ArduinoKeyer.SetSwapPaddles (On: BOOLEAN);

   BEGIN
   SwapPaddles := On;

   IF KeyerInitialized THEN
       BEGIN
       FlushCWBuffer;

       ArduinoKeyerPort.PutChar (Char($05));  { Paddle orientation command }

       IF SwapPaddles THEN
           ArduinoKeyerPort.putchar(Char($01))  { Swapped - tip = dah }
       ELSE
           ArduinoKeyerPort.putchar(Char($01)); { Not swapped - tip = dit}
       END;
   END;


FUNCTION ArduinoKeyer.GetSwapPaddles: BOOLEAN;

    BEGIN
    GetSwapPaddles := SwapPaddles;
    END;



PROCEDURE ArduinoKeyer.SetCountsSinceLastCW(count: integer);

    BEGIN
    END;

PROCEDURE ArduinoKeyer.SetPTTFootSwitch (on: BOOLEAN);

    BEGIN
    END;

PROCEDURE ArduinoKeyer.dvpptt(on: boolean);

    BEGIN
    END;

PROCEDURE ArduinoKeyer.SetCurtisMode(m: CurtisMode);

    BEGIN
    CurtMode := m;

    IF KeyerInitialized THEN
        BEGIN
        FlushCWBuffer;
        ArduinoKeyerPort.PutChar (Char ($14));

        CASE CurtMode OF
            ModeA:     ArduinoKeyerPort.PutChar (Char ($00));
            ModeB:     ArduinoKeyerPort.PutChar (Char ($01));
{           ModeNL:    ArduinoKeyerPort.PutChar (Char ($02));}
{           Ultimatic: Not supported at this time }
            END;       { of case }
        END;
    END;

FUNCTION ArduinoKeyer.GetCurtisMode: CurtisMode;

    BEGIN
    GetCurtisMode := CurtMode;
    END;



PROCEDURE ArduinoKeyer.SetPaddleBug (On: boolean);

    BEGIN
    PaddleBug := On;

    IF KeyerInitialized THEN
        BEGIN
        ArduinoKeyerPort.PutChar (Char ($15));  { Paddle bug command }

        IF PaddleBug THEN
            ArduinoKeyerPort.PutChar (Chr($01))
        ELSE
            ArduinoKeyerPort.PutChar (Chr ($00));
        END;
    END;

FUNCTION ArduinoKeyer.GetPaddleBug: BOOLEAN;

    BEGIN
    GetPaddleBug := PaddleBug;
    END;


FUNCTION ArduinoKeyer.GetKeyerInitialized: BOOLEAN;

    BEGIN
    GetKeyerInitialized := KeyerInitialized;
    END;



PROCEDURE ArduinoKeyer.AddCharacterToBuffer (Character: char);

{ This might need to get smarter for sending things that look like control
  characters down }

    BEGIN
    IF Character >= Chr ($20) THEN
        IF KeyerInitialized THEN
            ArduinoKeyerPort.PutChar (Character)  { Just send it along }
    END;


PROCEDURE ArduinoKeyer.SetPTTEnable (On: BOOLEAN);

    BEGIN
    PTTEnable := On;

    IF KeyerInitialized THEN
        BEGIN
        ArduinoKeyerPort.PutChar (Char ($16));  { PTT Enable command }

        IF PTTEnable THEN
            ArduinoKeyerPort.PutChar (Char (1))   { PTT enabled }
        ELSE
            ArduinoKeyerPort.PutChar (Char (0));  { PTT disabled }
        END;
    END;

FUNCTION ArduinoKeyer.GetPTTEnable: BOOLEAN;

    BEGIN
    GetPTTEnable := PTTEnable;
    END;



PROCEDURE ArduinoKeyer.UninitializeKeyer;

{ I have no idea why this exists.  I won't do anything to the keyer, but I
  will set the KeyerInitialized variable to FALSE, which wasn't done when I
  modified this code from the Winkey instance }

    BEGIN
    KeyerInitialized := False;
    END;



PROCEDURE ArduinoKeyer.SetTuneWithDits (On: boolean);

{ Sending with dits is currently not implemented in the Arudino - but this
  will do no harm }

    BEGIN
    TuneWithDits := On;

    IF KeyerInitialized THEN
        IF TuneWithDits THEN
            ArduinoKeyerPort.PutChar (Char ($17))   { Tune with dits }
        ELSE
            ArduinoKeyerPort.PutChar (Char ($12));   { Stop tuning with dits }
    END;

FUNCTION ArduinoKeyer.GetTuneWithDits: BOOLEAN;

    BEGIN
    GetTuneWithDits := TuneWithDits;
    END;



FUNCTION ArduinoKeyer.GetCountsSinceLastCW: INTEGER;

{ This is probably used for the repeat function key timer.  The arduino will
  return the # of 256 ms counts since last CW.  This will get multiplied by
  74 to get something close to what pascal is expecting }

VAR ArduinoCount: INTEGER;
    ArduinoCountsLoByte, ArduinoCountsHiByte: BYTE;

    BEGIN
    IF KeyerInitialized THEN
        BEGIN
        ClearOutAnyIncomingCharacters;

        ArduinoKeyerPort.putchar (Char ($1B));  { Ask for number of counts }

        { Warning - we could get trapped here }

        REPEAT UNTIL ArduinoKeyerPort.CharReady;

        ArduinoCountsLoByte := Integer (ArduinoKeyerPort.ReadChar);

        REPEAT UNTIL ArduinoKeyerPort.CharReady;

        ArduinoCountsHiByte := Integer (ArduinoKeyerPort.ReadChar);

        { COmbine the two bytes into one integer.  This is one count for every
          250 milliseconds }

        ArduinoCount := ((ArduinoCountsHiByte * 256) + ArduinoCountsLoByte);

        GetCountsSinceLastCW := ArduinoCount * 175;  { A guess }
        END

    ELSE
        GetCountsSinceLastCW := 0;  { what would be a better default? }
    END;



PROCEDURE ArduinoKeyer.SetFarnsworthEnable (On: boolean);

    BEGIN
    FarnsworthEnable := On;

    IF KeyerInitialized then
        BEGIN
        ArduinoKeyerPort.putchar(Char($18));  { Farnsworth enable command }

        IF FarnsworthEnable THEN
            ArduinoKeyerPort.PutChar (Char (FarnsworthSpeed)) { enabled }
        ELSE
            ArduinoKeyerPort.PutChar (Char ($00));  { disabled }
        END;
    END;

FUNCTION ArduinoKeyer.GetFarnsworthEnable: BOOLEAN;

    BEGIN
    GetFarnsworthEnable := FarnsworthEnable;
    END;

PROCEDURE ArduinoKeyer.SetFarnsworthSpeed (Speed: INTEGER);

    BEGIN
    FarnsworthSpeed := Speed;
    SetFarnsworthEnable (FarnsworthEnable);
    END;

FUNCTION ArduinoKeyer.GetFarnsworthSpeed: INTEGER;

    BEGIN
    GetFarnsworthSpeed := FarnsworthSpeed;
    END;



PROCEDURE ArduinoKeyer.SetMonitorTone (Tone: INTEGER);

{ This is for computer sent CW.  Zero value = disabled }

    BEGIN
    MonitorTone := Tone;

    Tone := Tone DIV 10;  { Convert to 10's of hertz }

    IF KeyerInitialized THEN
        BEGIN
        ArduinoKeyerPort.PutChar (Char ($03));   { Sidetone command }
        ArduinoKeyerPort.PutChar (Char (Tone));  { Sidetone value in 10's of hertz }
        END;
    END;

FUNCTION ArduinoKeyer.GetMonitorTone: INTEGER;

    BEGIN
    GetMonitorTone := MonitorTone;
    END;



PROCEDURE ArduinoKeyer.SetPaddleMonitorTone (Tone: INTEGER);

    BEGIN
    PaddleMonitorTone := tone;

    Tone := Tone DIV 10;  { Convert to 10's of Hertz }

    IF KeyerInitialized THEN
        BEGIN
        ArduinoKeyerPort.PutChar (Char($04));   { Paddle sidetone command }
        ArduinoKeyerPort.PutChar (Char (Tone)); { Paddle sidetone 10's of hertz }
        END;
    END;

FUNCTION ArduinoKeyer.GetPaddleMonitorTone: INTEGER;

    BEGIN
    GetPaddleMonitorTone := PaddleMonitorTone;
    END;



PROCEDURE ArduinoKeyer.SetPaddleSpeed (Speed: INTEGER);

    BEGIN
    IF Speed < 5 THEN Speed := 5;
    IF Speed > 99 THEN Speed := 99;

    PaddleCWSpeed := Speed;

    IF KeyerInitialized THEN
        BEGIN
        ArduinoKeyerPort.PutChar (Char($09));   { Paddle CW speed command }
        ArduinoKeyerPort.PutChar (Char (PaddleCWSpeed)); { Paddle speed in WPM }
        END;
    END;

PROCEDURE ArduinoKeyer.SetCwGrant (On: BOOLEAN);

{ Does nothing for now.  Probably W9CF used this in the timer routine. }

    BEGIN
    FsCwGrant := On;
    END;

PROCEDURE ArduinoKeyer.SetFootSwitch (F: FootSwitchX);

{ Arudinio does not know what to do about this }

    BEGIN
    Footsw := F;
    END;

PROCEDURE ArduinoKeyer.LetFootSwitchControlPTT;

{ This did nothing for W9CF - but I can imagine letting the Arduino control
  the PTT output directly from the footswitch. }

    BEGIN
    FootSwitchControlPTT := True;

    IF KeyerInitialized THEN
        BEGIN
        ArduinoKeyerPort.PutChar (Char($19));   { Footswitch mode command }

        IF FootSwitchControlPTT THEN
            ArduinoKeyerPort.PutChar (Char ($01))  { Footswitch turns on PTT }
        ELSE
            ArduinoKeyerPort.PutChar (Char ($00)); { Footswitch not turn on PTT }
        END;
    END;

FUNCTION ArduinoKeyer.GetPaddleSpeed: INTEGER;

    BEGIN
    GetPaddleSpeed := PaddleCWSpeed;
    END;


FUNCTION ArduinoKeyer.GetPTTAsserted: BOOLEAN;

{ I decided go ask the Arduiino }

    BEGIN
    IF KeyerInitialized THEN
        BEGIN
        ClearOutAnyIncomingCharacters;
        ArduinoKeyerPort.PutChar (Char ($11)); { Ask for PTT state }

        { WARNING!!  We could get trapped here }

        REPEAT UNTIL ArduinoKeyerPort.CharReady;

        CASE Integer (ArduinoKeyerPort.ReadChar) OF
            0: PTTAsserted := False;
            1: PTTAsserted := True;
            END;  { of case }
        END;

    GetPTTAsserted := PTTAsserted;   { Guess I will keep this here as a default }
    END;



PROCEDURE ArduinoKeyer.SetPort (Port: SerialPortx);

    BEGIN
    ArduinoKeyerPort := Port;
    END;



procedure ArduinoKeyer.Timer;

{ Not sure this does anything for us }

    BEGIN
    if not KeyerInitialized then exit;
    END;

PROCEDURE ArduinoKeyer.ClearOutAnyIncomingCharacters;

    BEGIN
    WHILE ArduinoKeyerPort.CharReady DO
        ArduinoKeyerPort.ReadChar;
    END;


FUNCTION ArduinoKeyer.CWStillBeingSent: BOOLEAN;

{ Well - this can get smarter someday as we can tell when we are doing
  the PTT TurnOff timer.  But for now - we will just use it a originally
  intended }

    BEGIN
    IF KeyerInitialized THEN
        BEGIN
        ClearOutAnyIncomingCharacters;
        ArduinoKeyerPort.PutChar (Char ($0C));  { Ask if CW still being sent }

        { WARNING!!  We could get trapped here }

        REPEAT UNTIL ArduinoKeyerPort.CharReady;

        CASE Integer (ArduinoKeyerPort.ReadChar) OF
            0: BEGIN
                 CWStillBeingSent := False;
                 END;

            1: BEGIN
                 CWStillBeingSent := True;  { we are doing the PTT wind down thing }
                 END;

            2: BEGIN
                 CWStillBeingSent := True;  { we are indeed sending CW }
                 END;
            END;
        END
    ELSE
        CWStillBeingSent := False;
    END;



FUNCTION ArduinoKeyer.BufferEmpty: BOOLEAN;

{ I am not really sure what this is being used for in the host.  For now - we
  will use the $0D function call to the Arduino which will return the number of
  chraacters in its character buffer - which should mimic what this did when W9CF
  had his own character buffer here.  If that number is zero - we will return TRUE }

    BEGIN
    IF KeyerInitialized THEN
        BEGIN
        ClearOutAnyIncomingCharacters;
        ArduinoKeyerPort.PutChar (Char ($0D));  { Ask if CW still being sent }

        { WARNING!!  We could get trapped here }

        REPEAT UNTIL ArduinoKeyerPort.CharReady;

        BufferEmpty := Integer (ArduinoKeyerPort.ReadChar) = 0;
        END
    ELSE
        BufferEmpty := True;
    END;



PROCEDURE ArduinoKeyer.AddStringToBuffer (Msg: String; Tone: INTEGER);

VAR Index: INTEGER;

    BEGIN
    FOR Index := 1 to Length (Msg) DO
        AddCharacterToBuffer (Msg [Index]);
    END;



FUNCTION ArduinoKeyer.DeleteLastCharacter: BOOLEAN;

{ Returns TRUE if successful.  Most of the heavy lifting for this is done in
  the Arduino.  }

    BEGIN
    DeleteLastCharacter := False;  { Default value }

    IF KeyerInitialized THEN
        BEGIN
        ClearOutAnyIncomingCharacters;
        ArduinoKeyerPort.PutChar (Char ($1A));  { Ask to delete last unsent character }

        { WARNING!!  We could get trapped here }

        REPEAT UNTIL ArduinoKeyerPort.CharReady;

        DeleteLastCharacter := Integer (ArduinoKeyerPort.ReadChar) = 1;
        END;
    END;



PROCEDURE ArduinoKeyer.FlushCWBuffer;

    BEGIN
    IF KeyerInitialized THEN
        ArduinoKeyerPort.PutChar (Char($13));  { Stop CW after current character - clear buffer }
    END;

END.
