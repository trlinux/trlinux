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

USES foot,tree,trcrt,communication,beep,radio,keyers,so2r,ycccprotocol;

TYPE
     ArduinoKeyer = class(keyer,so2rinterface)

     private
        ArduinoKeyerPort:     serialportx;
        ComputerCodeSpeed:    INTEGER;
        CurtMode:             CurtisMode;
        FarnsworthEnable:     BOOLEAN;
        FarnsworthSpeed:      INTEGER;
        FsCwGrant:            Boolean;
        Footsw:               FootSwitchx;
        FootswitchControlPTT: BOOLEAN;
        KeyerInitialized:     BOOLEAN;
        MonitorTone:          INTEGER;
        PaddleBug:            BOOLEAN;
        PaddleMonitorTone:    INTEGER;
        PaddleCWSpeed:        INTEGER;
        PaddlePTTHoldCount:   INTEGER;
        PTTAsserted:          BOOLEAN;
        PTTEnable:            BOOLEAN;
        PTTTurnOnDelay:       INTEGER;
        SwapPaddles:          BOOLEAN;
        TuningWithDits:       BOOLEAN;
        Tuning:               BOOLEAN;
        TuneWithDits:         BOOLEAN;
        Weight:               Integer;

        { so2r stuff }

        Blend:                INTEGER;
        FootSwitchDebounce:   INTEGER;
        HeadphoneMode:        hmode_t;
        Latch:                BOOLEAN;   { I really don't know what this does }
        map1:                 INTEGER;
        map2:                 INTEGER;
        SO2R_config:          so2r_config_t;   { not used except I remember if blend and relays are set  }
        SO2R_state:           so2r_state_t;    { this gets used for RX focus and mic switching }
        SO2R_switches:        so2r_switches_t; { not used }

        FUNCTION  EchoTest: BOOLEAN;
        PROCEDURE SendRelayStatusToSO2RMini;

    public

        Version: STRING;

        CONSTRUCTOR Create;

        PROCEDURE AddCharacterToBuffer (Character: CHAR);override;
        PROCEDURE AddStringToBuffer (Msg: String; Tone: INTEGER);override;

        FUNCTION  BufferEmpty: BOOLEAN;override;

        PROCEDURE ClearFootSwitchControlPTT;
        PROCEDURE ClearOutAnyIncomingCharacters;
        FUNCTION  CWStillBeingSent: BOOLEAN;override;

        FUNCTION  DeleteLastCharacter: BOOLEAN;override;
        PROCEDURE dvpptt(on: boolean);override;                 { Not yet implemented in Arduino }

        PROCEDURE FootSwitch2BSIQSSB; override;

        PROCEDURE FlushCWBuffer;override;

        Function  GetCountsSinceLastCW:integer;override;
        Function  GetCurtisMode:CurtisMode;override;
        FUNCTION  GetFarnsworthEnable:boolean;override;
        FUNCTION  GetFarnsworthSpeed:integer;override;
        Function  GetKeyerInitialized:boolean;override;
        FUNCTION  GetMonitorTone:integer;override;
        Function  GetPTTAsserted:boolean;override;
        FUNCTION  GetPTTTurnOnDelay:integer;override;
        FUNCTION  GetPTTEnable:boolean;override;
        FUNCTION  GetTuneWithDits:boolean;override;
        FUNCTION  GetPaddleBug:boolean;override;
        FUNCTION  GetPaddleMonitorTone:integer;override;
        FUNCTION  GetPaddlePTTHoldCount:integer;override;
        FUNCTION  GetPaddleSpeed:integer;override;
        FUNCTION  GetSpeed:INTEGER;override;
        Function  GetSwapPaddles:boolean;override;
        FUNCTION  GetWeight:real;override;

        PROCEDURE InitializeKeyer;override;

        PROCEDURE LetFootSwitchControlPTT; override;

        FUNCTION  PTTAssertedStill: BOOLEAN; override;
        PROCEDURE PTTForceOn;override;
        PROCEDURE PTTUnForce;override;

        Procedure SetActiveRadio (radio: RadioType);override;    { Works for SSB and CW }
        Procedure SetCountsSinceLastCW(count: integer);override; { Not sure you can set that }
        Procedure SetCurtisMode(m: CurtisMode);override;
        Procedure SetCWGrant(on: boolean);override;              { Not yet implemetned in Arduino }
        PROCEDURE SetFarnsworthEnable(on: boolean);override;     { Not yet implemented in Arduino }
        PROCEDURE SetFarnsworthSpeed(speed: integer);override;   { Not yet implemented in Arduino }
        Procedure SetFootSwitch (f: FootSwitchx);override;
        PROCEDURE SetMonitorTone(tone: integer);override;
        PROCEDURE SetPaddleBug(On: Boolean);override;            { Not yet implemented in Ardunio}
        PROCEDURE SetPaddleMonitorTone(tone: Integer);override;
        PROCEDURE SetPaddlePttHoldCount(Count: INTEGER);override;
        PROCEDURE SetPaddleSpeed(speed: Integer);override;
        Procedure SetPort(port: serialportx);
        PROCEDURE SetPTTEnable(on: boolean);override;
        Procedure SetPTTFootSwitch(on: boolean);override;
        PROCEDURE SetPTTTurnOnDelay(delay: integer);override;
        PROCEDURE SetSpeed (Speed: INTEGER);override;
        Procedure SetSwapPaddles(on: boolean);override;
        PROCEDURE SetTuneWithDits(on: boolean);override;         { Not yet implemented in Arduino }
        PROCEDURE SetWeight (w: real);override;                  { 1.00 is normal keying }

        PROCEDURE Timer;override;                                { Does nothing }

        PROCEDURE UnInitializeKeyer;override;

        { SO2R stuff for now }

        { The following procedurse will do nothing with the hardware.  They will
          do their best not to break anything }

        procedure setrig1band(band: integer);
        procedure setrig2band(band: integer);
        procedure setaux(aux: integer; value: integer);
        PROCEDURE SetHeadphoneMode (mode: hmode_t);
        procedure setblend(on: boolean);
        procedure blendvalue(val: integer);
        procedure SetRig1Map (Val: INTEGER);
        procedure SetRig2Map (Val: INTEGER);
        PROCEDURE SetLatch (On: BOOLEAN);

        function  getblend:boolean;
        function  getblendvalue:integer;
        FUNCTION  GetHeadphoneMode: hmode_t;
        FUNCTION  GetLatch: BOOLEAN;
        function  GetRig1Map: INTEGER;
        function  GetRig2Map: INTEGER;

        { Here are things we will support }

        PROCEDURE SetMicRelay (On: BOOLEAN);   { Enables the mic relay to follow radio }
        FUNCTION  GetMicRelay: BOOLEAN;

        { Appears we never read this }

        PROCEDURE SetRcvFocus (RcvFocus: rcvfocus_t);  { RX1, RX2 or STEREO }

        function  footswitchpressed:boolean;
        END;

VAR ArduinoDebug: BOOLEAN;

IMPLEMENTATION

Uses keycode,linuxsound,xkb,sysutils;

PROCEDURE AppendDebugFile (Text: STRING);

VAR DebugFileWrite: TEXT;

    BEGIN
    IF ArduinoDebug THEN
        BEGIN
        OpenFileForAppend (DebugFileWrite, 'arduino.txt');
        WriteLn (DebugFileWrite, GetFullTimeString, ': ', Text);
        Close (DebugFileWrite);
        END;
    END;



PROCEDURE ArduinoKeyer.SetRig1Band (Band: INTEGER);

{ SO2R mini might have a band output port thanks to W9CF }

    BEGIN
    ArduinoKeyerPort.PutChar (Char ($1D));
    ArduinoKeyerPort.PutChar (Char (Band and $0f));
    AppendDebugFile ('SetRig1Band');
    END;

PROCEDURE ArduinoKeyer.SetRig2Band (Band: INTEGER);

{ SO2R mini might have band output port thanks to W9CF  }

    BEGIN
    ArduinoKeyerPort.PutChar (Char ($1D));
    ArduinoKeyerPort.PutChar (Char ($10 OR (Band and $0f)));
    AppendDebugFile ('SetRig2Band');
    END;

PROCEDURE ArduinoKeyer.SetAux (Aux: INTEGER; Value: INTEGER);

{ I have no understanding of how to apply this to the SO2R mini }

    BEGIN
    ArduinoKeyerPort.PutChar (Char ($1D));
    ArduinoKeyerPort.PutChar (Char (((Aux AND $0F) SHL 4) OR (Value AND $0F)));
    AppendDebugFile ('SetAux');
    END;


PROCEDURE ArduinoKeyer.SetBlend (On: BOOLEAN);

{ SO2R mini has no blend feature - but I will support remembering it }

    BEGIN
    IF On THEN
        SO2R_Config.Blend := 1
    ELSE
        SO2R_Config.Blend := 0;
    END;


PROCEDURE ArduinoKeyer.BlendValue (Val: INTEGER);

{ SO2R mini has no blend feature - but I will support remembering it }

    BEGIN
    Blend := Val;
    END;


PROCEDURE ArduinoKeyer.SetHeadphoneMode (Mode: hmode_t);

{ SO2R mini does not have headphone modes - but I will support remembering it }

    BEGIN
    HeadphoneMode := Mode;
    END;


PROCEDURE ArduinoKeyer.SetMicRelay (On: boolean);

{ This appears to be an enable for SO2R microphone switching.  The YCCC box
  seems to use this parameter - but I am not going to pass it down to the
  Arduino.  I can decide if I want to support switching the microphone based
  upon headphone focus or not here in pascal land }

    BEGIN
    IF On THEN
        SO2R_Config.Relays := 1
    ELSE
        SO2R_Config.Relays := 0;
    END;


PROCEDURE ArduinoKeyer.SetRig1Map (Val: INTEGER);

{ SO2R does not support this - but I will remember the value }

    BEGIN
    IF (Val < -4) THEN Val := -4;
    IF (Val > 4) THEN Val := 4;
    Map1 := Val;
    END;


PROCEDURE ArduinoKeyer.SetRig2Map (Val: INTEGER);

{ SO2R does not support this - but I will remember the value }

    BEGIN
    IF (Val < -4) THEN Val := -4;
    IF (Val > 4) THEN Val := 4;
    Map2 := Val;
    END;



PROCEDURE ArduinoKeyer.SendRelayStatusToSO2RMini;

{ Uses the SO2R_State flags to set the relays to the proper state }

VAR Cmd: BYTE;

    BEGIN
    IF KeyerInitialized THEN
        BEGIN
        Cmd := 0;  { default value }

        { Bit zero of the command is relay 1 - OFF = rig 1  ON = rig 2 }

        IF (SO2R_State.RX2 = 1) THEN
            Cmd := $01;

        { Bit one is for relay 2 - which if relay 1 is also on will do stereo }

        IF (SO2R_State.Stereo = 1) THEN
            Cmd := $03;

        { Microphone relay }

        IF (SO2R_Config.Relays = 1) THEN
            IF (SO2R_State.TX2 = 1) THEN
                Cmd := Cmd OR $04;  { Set bit }

        ArduinoKeyerPort.PutChar (Char ($02));  { SO2R relay command }
        ArduinoKeyerPort.PutChar (Char (Cmd));  { SO2R relay data }
        END;

    AppendDebugFile ('SendRelayStatusToSO2RMini');
    END;


PROCEDURE ArduinoKeyer.SetRcvFocus (RcvFocus: rcvfocus_t);

{ Here we get to tell the SO2R mini which way to set the headphone relays
  K1 and K2 off  = Radio 1
  K1 on - K2 off = Radio 2
  K1 and K2 on   = STEREO    }

    BEGIN
    CASE RcvFocus OF
        RX1:
            BEGIN
            SO2R_State.Stereo := 0;
            SO2R_State.RX2    := 0;
            END;

        RX2:
            BEGIN
            SO2R_State.Stereo := 0;
            SO2R_State.RX2    := 1;
            END;

        Stereo:
            BEGIN
            SO2R_State.Stereo := 1;
            END;

        END;  { of case }

     AppendDebugFile ('SetRcvFocus');
     SendRelayStatusToSO2RMini;
     END;


PROCEDURE ArduinoKeyer.SetLatch (On: BOOLEAN);

{ SO2R does not support this - but I will remember the value }

    BEGIN
    Latch := On;
    END;


FUNCTION ArduinoKeyer.GetHeadphoneMode: hmode_t;

{ Just returning whatever value was pass down - not used by SO2R mini }

    BEGIN
    GetHeadphoneMode := HeadphoneMode;
    END;


FUNCTION ArduinoKeyer.GetLatch: BOOLEAN;

{ Just returning whatever value was pass down - not used by SO2R mini }

    BEGIN
    GetLatch := Latch;
    END;


FUNCTION ArduinoKeyer.footswitchpressed:boolean;

{ HERE IT IS - this is how we tell LOGSUBS2.PAS if the foot switch is
  pressed! I guess I will go with the debounced state }

    BEGIN
    IF KeyerInitialized THEN
        BEGIN
        ClearOutAnyIncomingCharacters;
        ArduinoKeyerPort.PutChar (Char ($11)); { Ask for footswitch state }

        { WARNING!!  We could get trapped here }

        REPEAT UNTIL ArduinoKeyerPort.CharReady;

        CASE Integer (ArduinoKeyerPort.ReadChar) OF
            0: FootSwitchPressed := False;
            1: FootSwitchPressed := True;
            END;  { of case }
        END
    ELSE
        FootSwitchPressed := False;
    END;


FUNCTION ArduinoKeyer.GetBlend: BOOLEAN;

{ Just returning whatever value was pass down - not used by SO2R mini }

    BEGIN
    GetBlend := (SO2R_Config.blend = 1);
    END;


FUNCTION ArduinoKeyer.GetBlendValue: INTEGER;

{ Just returning whatever value was pass down - not used by SO2R mini }

    BEGIN
    GetBlendValue := Blend;
    END;


FUNCTION ArduinoKeyer.GetMicRelay: BOOLEAN;

{ This is flag that enables SO2R microphone relay action (I think) }

    BEGIN
    GetMicRelay := (SO2r_Config.Relays = 1);
    END;


FUNCTION ArduinoKeyer.GetRig1Map: INTEGER;

{ Just returning whatever value was pass down - not used by SO2R mini }

    BEGIN
    GetRig1Map := Map1;
    END;


FUNCTION ArduinoKeyer.GetRig2Map: INTEGER;

{ Just returning whatever value was pass down - not used by SO2R mini }

    BEGIN
    GetRig2Map := Map2;
    END;



CONSTRUCTOR ArduinoKeyer.Create;

   BEGIN
   ArduinoKeyerPort := nil;

   { Default values }

   ComputerCodeSpeed := 35;
   CurtMode := ModeB;
   FsCwGrant := False;
   FootswitchControlPTT := False;
   KeyerInitialized := False;
   MonitorTone := 700;
   PaddleBug := false;
   PaddleCWSpeed := 0;
   PaddlePTTHoldCount := 10;
   PaddleMonitorTone := 700;
   PTTEnable := false;

   SO2R_State.TX2 := 0;
   SO2R_State.RX2 := 0;
   SO2R_State.Stereo := 0;
   SO2R_Config.Relays := 1;

   Tuning := False;
   TuningWithDits := False;
   Weight := 100;
   END;



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
                IF Copy (Version, 1, 7) <> 'TRCW V5' THEN
                    BEGIN
                    WriteLn ('Expected TRCW V5 response from SO2R Mini.  Received ', Version);
                    WaitForKeyPressed;
                    Halt;
                    END;

                GoToXY (1, WhereY);
                ClrEol;

                EchoTest := True;
                Exit;
                END;
            END
        ELSE
            BEGIN     { Here because we timed out }
            Inc (RetryCount);
            GoToXY (1, WhereY);
            Write ('SO2R Retry count = ', RetryCount);

            IF RetryCount > 20 THEN
                BEGIN
                ClrScr;
                WriteLn;
                WriteLn ('No valid response from Arduino after ', RetryCount, ' attempts');

                IF Version <> '' THEN
                    WriteLn ('Returned characters = ', Version);

                WaitForKeyPressed;
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
       WriteLn ('Arduino Keyer not responding -- check connection');
       WaitForKeyPressed;
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
    AppendDebugFile ('SetWeight');
    END;



FUNCTION ArduinoKeyer.GetWeight: REAL;

{ We just report back what we sent last to the keyer }

    BEGIN
    GetWeight := Weight * (1 / 100);
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
   AppendDebugFile ('SetPTTTurnOnDelay');
   END;

FUNCTION ArduinoKeyer.GetPTTTurnOnDelay: INTEGER;

    BEGIN
    GetPTTTurnOnDelay := PTTTurnOnDelay;
    END;


PROCEDURE ArduinoKeyer.SetActiveRadio (Radio: RadioType);

{ Determines which radio will have CW and PTT enabled.  It will
  also switch the microphone (if the mic relay is enabled with
  SetMicRelay) . Any CW in progress will be aborted. }

    BEGIN
    CASE Radio OF
        RadioOne:
            BEGIN
            AppendDebugFile ('SetActiveRadio = RadioOne');
            SO2R_State.TX2 := 0;          { Set microphone to radio 1 }

            IF KeyerInitialized THEN
                BEGIN
                ArduinoKeyerPort.PutChar (Char ($0B));  { Radio select command }
                ArduinoKeyerPort.PutChar (Char ($01));  { Radio One }
                SendRelayStatusToSO2RMini ;  { Update microphone relay  }
                END;
            END;

        RadioTwo:
            BEGIN
            SO2R_State.TX2 := 1;          { Set microphone to radio 2 }

            IF KeyerInitialized THEN
                BEGIN
                AppendDebugFile ('SetActiveRadio = RadioTwo');
                ArduinoKeyerPort.PutChar (Char ($0B));  { Radio select command }
                ArduinoKeyerPort.PutChar (Char ($02));  { Radio Two }
                SendRelayStatusToSO2RMini;   { Update microphone relay }
                END;
            END;

        END;  { of case }
    END;


PROCEDURE ArduinoKeyer.SetPaddlePttHoldCount (Count: INTEGER);

{ Number of dit lengths to hold the paddle after sending.  This used to
  be milliseconds but we changed. }

    BEGIN
    IF Count > 250 THEN Count := 250;

    PaddlePTTHoldCount := Count;

    IF KeyerInitialized THEN
        BEGIN
        ArduinoKeyerPort.PutChar (Char ($10));   { PTT Hold fimr for paddle sent CW command }
        ArduinoKeyerPort.PutChar (Char (Count)); { Hold time in dit lengths }
        END;

    AppendDebugFile ('SetPaddlePTTHoldCount');
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
    AppendDebugFile ('PTTForceOn');
    END;

Procedure ArduinoKeyer.PTTUnForce;

{ Turns off PTT signal - no if ands or buts.  Well - okay, if there are
  CW characters to be sent - the PTT will stay asserted }

    BEGIN
    IF KeyerInitialized THEN
        BEGIN
        ArduinoKeyerPort.PutChar (Char ($0A)); { PTT command }
        ArduinoKeyerPort.PutChar (Char ($00)); { PTT off }
        END;
    AppendDebugFile ('PTTUnForce');
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
       END;
   AppendDebugFile ('SetSpeed');
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
   AppendDebugFile ('Swap Paddles');
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
            ModeNL:    ArduinoKeyerPort.PutChar (Char ($02));
            Ultimatic: ArduinoKeyerPort.PutChar (Char ($03));
            END;       { of case }
        END;
    AppendDebugFile ('SetCurtisMode');
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
    AppendDebugFile ('SetPaddleBug');
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
            ArduinoKeyerPort.PutChar (Character);  { Just send it along }

    AppendDebugFile ('AddCharacterToBuffer = ' + Character);
    END;


PROCEDURE ArduinoKeyer.SetPTTEnable (On: BOOLEAN);

    BEGIN
    PTTEnable := On;

    IF KeyerInitialized THEN
        BEGIN
        ArduinoKeyerPort.PutChar (Char ($16));  { PTT Enable command }

        IF PTTEnable THEN
            BEGIN
            ArduinoKeyerPort.PutChar (Char (1));  { PTT enabled }
            AppendDebugFile ('PTTEnabled');
            END
        ELSE
            BEGIN
            ArduinoKeyerPort.PutChar (Char (0));  { PTT disabled }
            AppendDebugFile ('PTTDisabled');
            END;
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
    AppendDebugFile ('SetTuneWithDits');
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

    AppendDebugFile ('GetCountsSinceLastCW');
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
    { We allow zero here - which makes it track the ComputerCWSpeed }

    IF Speed > 99 THEN Speed := 99;

    PaddleCWSpeed := Speed;

    IF KeyerInitialized THEN
        BEGIN
        ArduinoKeyerPort.PutChar (Char($09));   { Paddle CW speed command }
        ArduinoKeyerPort.PutChar (Char (PaddleCWSpeed)); { Paddle speed in WPM }
        END;
    END;

PROCEDURE ArduinoKeyer.SetCwGrant (On: BOOLEAN);

{ When this is set to TRUE - the Arduino will set the footswitch mode to FS_Lockout
  which will prevent multiple Arduinos from sending CW at the same time if their
  footswtich connectors are tied together }

    BEGIN
    FsCwGrant := On;

    IF On THEN
        BEGIN
        IF KeyerInitialized THEN
            BEGIN
            ArduinoKeyerPort.PutChar (Char($19));   { Footswitch mode command }
            ArduinoKeyerPort.PutChar (Char($03));   { Lockout mode }
            END;

        AppendDebugFile ('Set footswitch lockout mode');
        END
    ELSE
        BEGIN
        IF KeyerInitialized THEN
            BEGIN
            ArduinoKeyerPort.PutChar (Char($19));   { Footswitch mode command }

            IF FootSwitchControlPTT THEN
                BEGIN
                ArduinoKeyerPort.PutChar (Char($01));  { Normal PTT mode }
                AppendDebugFile ('Normal PTT Mode');
                END
            ELSE
                BEGIN
                ArduinoKeyerPort.PutChar (Char($00));  { No PTT mode }
                AppendDebugFile ('No PTTmode');
                END;
            END;
        END;
    END;



PROCEDURE ArduinoKeyer.SetFootSwitch (F: FootSwitchX);

{ Arudinio does not know what to do about this }

    BEGIN
    Footsw := F;
    END;

PROCEDURE ArduinoKeyer.FootSwitch2BSIQSSB;

{ Puts the Arduino into the 2BSIQ SSB MODE }

    BEGIN
    Write ('.');
    IF KeyerInitialized THEN
        BEGIN
        ArduinoKeyerPort.PutChar (Char ($19));  { Footswitch mode command }
        ArduinoKeyerPort.PutChar (Char ($02))   { 2BSIQ SSB Mode }
        END;

    AppendDebugFile ('Set2BSIQSSBFootswitchMode');
    END;



PROCEDURE ArduinoKeyer.LetFootSwitchControlPTT;

{ This did nothing for W9CF - but I can imagine letting the Arduino control
  the PTT output directly from the footswitch. }

    BEGIN
    FootSwitchControlPTT := True;

    IF KeyerInitialized THEN
        BEGIN
        ArduinoKeyerPort.PutChar (Char($19));   { Footswitch mode command }
        ArduinoKeyerPort.PutChar (Char ($01))  { Footswitch turns on PTT }
        END;

    AppendDebugFile ('LetFootswitchControlPTT');
    END;



PROCEDURE ArduinoKeyer.ClearFootSwitchControlPTT;

{ Put the SO2R mini in the mode of only returning the footswitch state without affecting
  PTT. }

    BEGIN
    FootSwitchControlPTT := False;

    IF KeyerInitialized THEN
        BEGIN
        ArduinoKeyerPort.PutChar (Char($19));   { Footswitch mode command }
        ArduinoKeyerPort.PutChar (Char ($00)); { Footswitch not turn on PTT }
        END;
    AppendDebugFile ('Clear LetFootswitchControlPTT');
    END;

FUNCTION ArduinoKeyer.GetPaddleSpeed: INTEGER;

    BEGIN
    GetPaddleSpeed := PaddleCWSpeed;
    END;


FUNCTION ArduinoKeyer.GetPTTAsserted: BOOLEAN;

{ It isn't clear to me what this is actually used for.  I can't find
  PTTasserted in logcw or anywhere else above the keyer objects. }

    BEGIN
    GetPTTAsserted := PTTAsserted;   { Guess I will keep this here as a default }
    END;



PROCEDURE ArduinoKeyer.SetPort (Port: SerialPortx);

    BEGIN
    ArduinoKeyerPort := Port;
    //ArduinoKeyerPort.setparams (115200, 8, NoParity, 1);
    ArduinoKeyerPort.setparams (19200, 8, NoParity, 1);
    END;



procedure ArduinoKeyer.Timer;

{ Not sure this does anything for us }

    BEGIN
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
            0: CWStillBeingSent := False;  { PTT dropped }

             { A 1 result  used to return True -  but we had a funny
               case where if someone forced on the PTT and was waiting
               for CW to finish - we would lock up since PTT would never
               drop.  So - best to report back that CW is done even if
               the PTT is still asserted.  }

            1: CWStillBeingSent := False;  { CW buffer empty - PTT On }
            2: CWStillBeingSent := True    { we are indeed sending CW }

            END;  { of case }
        END
    ELSE
        CWStillBeingSent := False;
    END;



FUNCTION ArduinoKeyer.PTTAssertedStill: BOOLEAN;

{ Kind of like CWStillBeingSent, but looks for the PTT to drop before reporting
  back with a changed status }

    BEGIN
    IF KeyerInitialized THEN
        BEGIN
        ClearOutAnyIncomingCharacters;
        ArduinoKeyerPort.PutChar (Char ($0C));  { Ask if CW still being sent }

        { WARNING!!  We could get trapped here }

        REPEAT UNTIL ArduinoKeyerPort.CharReady;

        CASE Integer (ArduinoKeyerPort.ReadChar) OF
            0: PTTAssertedStill := False;     { PTT dropped }
            1: PTTAssertedStill := True; { CW buffer empty - PTT On }
            2: PTTAssertedStill := True  { we are indeed sending CW }
            END;  { of case }
        END
    ELSE
        PTTAssertedStill := False;

    AppendDebugFile ('Called PTTAssertedStill');
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

{ Was sending $13 to finish current character but that resulted in PTT errors
  so amd trying $12 }

    BEGIN
    IF KeyerInitialized THEN
        ArduinoKeyerPort.PutChar (Char($13));  { Stop CW after current character - clear buffer }
    END;

    BEGIN
    ArduinoDebug := True;  { Set to true to get some debug data }
    END.
