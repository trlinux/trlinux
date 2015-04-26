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

unit keyerk1ea;
{$mode objfpc}

INTERFACE

USES trcrt,communication,beep,foot,radio,keyers;

CONST
    CWBufferSize           = 1024;

TYPE
    CWElementRecord = RECORD
        Length: INTEGER;
        Key:    BOOLEAN;
        END;
 

     CWBufferType = ARRAY [0..CWBufferSize] OF CWElementRecord;
     CWBufferPtr  = ^CWBufferType;

     K1EAKeyer = class(keyer)

     private
        CodeSpeed:        INTEGER;
        curtmode: CurtisMode;
        KeyerInitialized: BOOLEAN;
        ElementLength:         INTEGER;
        ElementLengthConstant: INTEGER;
        PaddleActive:            BOOLEAN;
        PaddleBug:               BOOLEAN;
        PaddleBugDahBeingSent:   BOOLEAN;
        PaddlePTTOffTimerCount:  INTEGER;
        PaddlePTTHoldCount:     INTEGER;
        PaddlePTTOffTimer:      INTEGER;
        PaddleMonitorTone:      INTEGER;
        PaddleSpeed:            INTEGER;
    SwapPaddles:              BOOLEAN;

    CountsSinceLastCW: LONGINT;
    FarnsworthEnable:  BOOLEAN;
    FarnsworthSpeed:   INTEGER;
    MonitorTone:            INTEGER;
    PTTEnable:              BOOLEAN;
    PTTTurnOnDelay:         INTEGER;
    RememberCodeSpeed:        INTEGER;
    TuningWithDits:           BOOLEAN;
    Tuning:             BOOLEAN;
    TuneWithDits:       BOOLEAN;
    Weight:             REAL;
    ActivePaddlePort:  keyerportx;
    DahMemory:    BOOLEAN;
    DitMemory:    BOOLEAN;
    CWBuffer: CWBufferPtr;
    CWBufferAvailable: BOOLEAN;
    CWBufferStart:         INTEGER;
    CWBufferEnd:           INTEGER;
    CWElementLength:       INTEGER;
    PTTDelayCount:            INTEGER; { When non zero, indicates that
                                         PTT turn on delay is in progress }
    PTTAsserted:    BOOLEAN; { Indicates that the PTT signal is asserted.
                               Comes on PTTTurnOnDelay before CW starts.  }
    PTTFootSwitch:  BOOLEAN; { Indicates that the footswitch wants the PTT
                               to be on. }
    PTTForcedOn:    BOOLEAN; { Indicates that someone wants the PTT to stay
                               on until they turn it off. }
    DoingPaddle:  BOOLEAN;
    LastFootSwitchStatus: BOOLEAN;
    SendStatus:           SendStatusType;
    SendingADah:          BOOLEAN;
    ActivePortOn: BOOLEAN;
    ActiveKeyerPort:   keyerportx;
    PortBeingUsed:          keyerportx;
    RadioOneKeyerOutputPort: keyerportx;
    RadioTwoKeyerOutputPort: keyerportx;
    FsCwGrant: Boolean;
    dvppttstatus: Boolean;
    Footsw: FootSwitchx;
    BeepTone: Beeper;
    nullp: nullportx;
    contactstate: integer;

        PROCEDURE Dah;
        PROCEDURE Dat;
        PROCEDURE Dit;
        procedure contacts(var ditcontact: boolean; var dahcontact: boolean);
        Procedure TurnOffActivePort;
        Procedure TurnOnActivePort;
        PROCEDURE IncrementBufferEnd;
        PROCEDURE CheckPTT;
        Function GetPTTForcedOn:boolean;
        Procedure SetPTTForcedOn(on :boolean);

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
        Procedure dvpptt(on: boolean);override;

        Procedure SetBeeper(b: Beeper);
        Procedure SetRadioOnePort(k: keyerportx);
        Procedure SetRadioTwoPort(k: keyerportx);
        Procedure SetActivePaddlePort(port: keyerportx);
        END;

IMPLEMENTATION

//Uses trcrt,keycode,linuxsound,xkb,sysutils;
Uses keycode,linuxsound,xkb,sysutils;


Constructor K1EAKeyer.create;
begin
   LastFootSwitchStatus := false;
    CountsSinceLastCW := 0;
    CodeSpeed := 35;
    ElementLength := Round (ElementLengthConstant / CodeSpeed);
    PaddlePTTOffTimerCount := ElementLength * PaddlePTTHoldCount;
    CurtMode := ModeB;
    KeyerInitialized := False;
    CWBufferAvailable := False;
    DahMemory   := False;
    DitMemory   := False;
    MonitorTone := (700);
    Tuning := False;
    Weight := 1.0;
    ActivePaddlePort := nil;
    nullp := nullportx.create;
    RadioOneKeyerOutputPort := nullp;
    RadioTwoKeyerOutputPort := nullp;
    ActiveKeyerPort := nullp;
    PortBeingUsed := nil;
    PTTFootSwitch := False;
    TuningWithDits := False;
    TuneWithDits := False;
    FsCwGrant := False;
    dvppttstatus := False;
    contactstate := 0;
end;


Procedure K1EAKeyer.SetCwGrant(on: boolean);
begin
   FsCwGrant := on;
end;

Procedure K1EAKeyer.dvpptt(on: boolean);
begin
   dvppttstatus := on;
   pttforceon;
end;

procedure K1EAKeyer.contacts(var ditcontact: boolean; var dahcontact: boolean);
var con: integer;
begin
    if ActivePaddlePort <> nil then
    begin
       IF SwapPaddles THEN
       begin
          DitContact := ActivePaddlePort.readdah;
          DahContact := ActivePaddlePort.readdit;
       end
       else
       begin
          DitContact := ActivePaddlePort.readdit;
          DahContact := ActivePaddlePort.readdah;
       end;
    end
    else
    begin
       DitContact := false;
       DahContact := false;
    end;

    if curtmode = Ultimatic then
    begin
       con := 0;
       if DitContact then inc(con);
       if DahContact then con := con + 2;
       case contactstate of
          0: begin
             case con of
                1: contactstate := 1;
                3: DahContact := false;
             end;
          end;

          1: begin
             case con of
                0: contactstate := 0;
                3: DitContact := false;
             end;
          end;

       end;
    end;
end;

PROCEDURE K1EAKeyer.TurnOffActivePort;

    BEGIN
    ActivePortOn := (PTTAsserted OR PTTForcedOn OR PTTFootSwitch) AND PTTEnable;

    IF ActivePortOn THEN
        BEGIN
        ActiveKeyerPort.key(false);
        ActiveKeyerPort.ptt(true);
        END
    ELSE
        BEGIN
        activekeyerport.key(false);
        ActiveKeyerPort.ptt(false);
        END;
    END;

Function K1EAKeyer.GetKeyerInitialized:boolean;
begin
   GetKeyerInitialized := KeyerInitialized;
end;

Function K1EAKeyer.GetCurtisMode:CurtisMode;
begin
   GetCurtisMode := CurtMode;
end;

Procedure K1EAKeyer.SetCurtisMode(m: CurtisMode);
begin
   CurtMode := m;
end;

Procedure K1EAKeyer.SetActivePaddlePort(port: keyerportx);
begin
   activepaddleport := port;
end;

Function K1EAKeyer.GetSwapPaddles:boolean;
begin
   GetSwapPaddles := SwapPaddles;
end;

Function K1EAKeyer.GetPTTAsserted:boolean;
begin
   GetPTTAsserted := PTTAsserted;
end;

Function K1EAKeyer.GetPTTForcedOn:boolean;
begin
   GetPTTForcedOn := PTTForcedOn;
end;

Procedure K1EAKeyer.SetActiveRadio(r: RadioType);
begin
   case r of

      RadioOne:
      begin
         ActiveKeyerPort := RadioOneKeyerOutputPort;
      end;

      RadioTwo:
      begin
         ActiveKeyerPort := RadioTwoKeyerOutputPort;
      end;

      NoRadio:
      begin
         ActiveKeyerPort := nullp;
      end;
   end;
end;

Procedure K1EAKeyer.SetRadioOnePort(k: keyerportx);
begin
   RadioOneKeyerOutputPort := k;
end;

Procedure K1EAKeyer.SetRadioTwoPort(k: keyerportx);
begin
   RadioTwoKeyerOutputPort := k;
end;

Procedure K1EAKeyer.SetPTTForcedOn(on: boolean);
begin
   PTTForcedOn := on;
end;

Procedure K1EAKeyer.SetPTTFootSwitch(on :boolean);
begin
   PTTFootSwitch := on;
end;

Procedure K1EAKeyer.SetSwapPaddles(on: boolean);
begin
   SwapPaddles := on;
end;



PROCEDURE K1EAKeyer.TurnOnActivePort;

{ PTT will always be on if PTTEnable. }

    BEGIN
    ActivePortOn := True;

    CountsSinceLastCW := 0;

    IF PTTEnable THEN
        BEGIN
        ActiveKeyerPort.key(true);
        ActiveKeyerPort.ptt(true);
        END
    ELSE
        BEGIN
        ActiveKeyerPort.key(true);
        ActiveKeyerPort.ptt(false);
        END;
    END;


PROCEDURE K1EAKeyer.PTTForceOn;

    BEGIN
    PTTForcedOn := True;
    TurnOffActivePort;
    END;


PROCEDURE K1EAKeyer.PTTUnForce;

    BEGIN
    PTTForcedOn := False;
    END;


PROCEDURE K1EAKeyer.IncrementBufferEnd;

    BEGIN
    CWBufferEnd := (CWBufferEnd + 1) MOD CWBufferSize;
    END;


PROCEDURE K1EAKeyer.CheckPTT;

    BEGIN
    IF NOT PTTAsserted THEN
        BEGIN
        PTTAsserted := True;
        IF NOT PTTForcedOn THEN PTTDelayCount := PTTTurnOnDelay;
        TurnOffActivePort;
        END;
    END;


PROCEDURE K1EAKeyer.Dit;

    BEGIN
    IF NOT CWBufferAvailable THEN
        BEGIN
        New (CWBuffer);
        CWBufferAvailable := True;
        END;

    CheckPTT;

    CWBuffer^ [CWBufferEnd].Length := 10;
    CWBuffer^ [CWBufferEnd].Key    := True;
    IncrementBufferEnd;

    CWBuffer^ [CWBufferEnd].Length := 10;
    CWBuffer^ [CWBufferEnd].Key    := False;
    IncrementBufferEnd;

    CheckPTT;
    END;


PROCEDURE K1EAKeyer.Dat;

    BEGIN
    IF NOT CWBufferAvailable THEN
        BEGIN
        New (CWBuffer);
        CWBufferAvailable := True;
        END;

    IF NOT PTTAsserted THEN
        BEGIN
        PTTAsserted := True;
        PTTDelayCount := PTTTurnOnDelay;
        TurnOffActivePort;
        END;

    CWBuffer^ [CWBufferEnd].Length := 45;
    CWBuffer^ [CWBufferEnd].Key  := True;

    IncrementBufferEnd;

    CWBuffer^ [CWBufferEnd].Length := 10;
    CWBuffer^ [CWBufferEnd].Key    := False;
    IncrementBufferEnd;

    IF NOT PTTAsserted THEN
        BEGIN
        PTTAsserted := True;
        PTTDelayCount := PTTTurnOnDelay;
        TurnOffActivePort;
        END;
    END;



PROCEDURE K1EAKeyer.Dah;

    BEGIN
    IF NOT CWBufferAvailable THEN
        BEGIN
        New (CWBuffer);
        CWBufferAvailable := True;
        END;

    IF NOT PTTAsserted THEN
        BEGIN
        PTTAsserted := True;
        PTTDelayCount := PTTTurnOnDelay;
        TurnOffActivePort;
        END;

    CWBuffer^ [CWBufferEnd].Length := 30;
    CWBuffer^ [CWBufferEnd].Key  := True;

    IncrementBufferEnd;

    CWBuffer^ [CWBufferEnd].Length := 10;
    CWBuffer^ [CWBufferEnd].Key    := False;
    IncrementBufferEnd;

    IF NOT PTTAsserted THEN
        BEGIN
        PTTAsserted := True;
        PTTDelayCount := PTTTurnOnDelay;
        TurnOffActivePort;
        END;
    END;



FUNCTION K1EAKeyer.DeleteLastCharacter: BOOLEAN;

{ This routine will remove the last character off the keying buffer.  If
  it fouund a letter to remove, it will return TRUE. }

VAR BufferPointer: INTEGER;

    BEGIN
    IF CWBufferEnd <> CWBufferStart THEN
        BufferPointer := CWBufferEnd - 1
    ELSE
        BEGIN
        DeleteLastCharacter := False;
        Exit;
        END;

    WHILE BufferPointer <> CWBufferStart DO
        BEGIN
        Dec (BufferPointer);

        IF BufferPointer < 0 THEN
            BufferPointer := BufferPointer + CWBufferSize;

        IF (CWBuffer^ [BufferPointer].Length = 20) AND (CWBuffer^ [BufferPointer].Key = False) THEN
            BEGIN
            CWBufferEnd := (BufferPointer + 1) MOD CWBufferSize;
            DeleteLastCharacter := True;
            Exit;
            END;
        END;

    DeleteLastCharacter := False;
    END;

PROCEDURE K1EAKeyer.AddCharacterToBuffer (Character: CHAR);

    BEGIN
    CASE UpCase (Character) OF
        'A': BEGIN Dit;Dah; END;
        'B': BEGIN Dah;Dit;Dit;Dit; END;
        'C': BEGIN Dah;Dit;Dah;Dit; END;
        'D': BEGIN Dah;Dit;Dit; END;
        'E': BEGIN Dit; END;
        'F': BEGIN Dit;Dit;Dah;Dit; END;
        'G': BEGIN Dah;Dah;Dit; END;
        'H': BEGIN Dit;Dit;Dit;Dit; END;
        'I': BEGIN Dit;Dit; END;
        'J': BEGIN Dit;Dah;Dah;Dah; END;
        'K': BEGIN Dah;Dit;Dah; END;
        'L': BEGIN Dit;Dah;Dit;Dit; END;
        'M': BEGIN Dah;Dah; END;
        'N': BEGIN Dah;Dit; END;
        'O': BEGIN Dah;Dah;Dah; END;
        'P': BEGIN Dit;Dah;Dah;Dit; END;
        'Q': BEGIN Dah;Dah;Dit;Dah; END;
        'R': BEGIN Dit;Dah;Dit; END;
        'S': BEGIN Dit;Dit;Dit; END;
        'T': BEGIN Dah; END;
        'U': BEGIN Dit;Dit;Dah; END;
        'V': BEGIN Dit;Dit;Dit;Dah; END;
        'W': BEGIN Dit;Dah;Dah; END;
        'X': BEGIN Dah;Dit;Dit;Dah; END;
        'Y': BEGIN Dah;Dit;Dah;Dah; END;
        'Z': BEGIN Dah;Dah;Dit;Dit; END;
        '0' : BEGIN Dah;Dah;Dah;Dah;Dah; END;
        '1' : BEGIN Dit;Dah;Dah;Dah;Dah; END;
        '2' : BEGIN Dit;Dit;Dah;Dah;Dah; END;
        '3' : BEGIN Dit;Dit;Dit;Dah;Dah; END;
        '4' : BEGIN Dit;Dit;Dit;Dit;Dah; END;
        '5' : BEGIN Dit;Dit;Dit;Dit;Dit; END;
        '6' : BEGIN Dah;Dit;Dit;Dit;Dit; END;
        '7' : BEGIN Dah;Dah;Dit;Dit;Dit; END;
        '8' : BEGIN Dah;Dah;Dah;Dit;Dit; END;
        '9' : BEGIN Dah;Dah;Dah;Dah;Dit; END;
        '.' : BEGIN Dit;Dah;Dit;Dah;Dit;Dah; END;
        ',' : BEGIN Dah;Dah;Dit;Dit;Dah;Dah; END;
        '?' : BEGIN Dit;Dit;Dah;Dah;Dit;Dit; END;
        '/' : BEGIN Dah;Dit;Dit;Dah;Dit; END;
        '+' : BEGIN Dit;Dah;Dit;Dah;Dit; END;
        '<' : BEGIN Dit;Dit;Dit;Dah;Dit;Dah; END;
        '=' : BEGIN Dah;Dit;Dit;Dit;Dah; END;
        '!' : BEGIN Dit;Dit;Dit;Dah;Dit; END;
        '&' : BEGIN Dit;Dah;Dit;Dit;Dit; END;
        '-' : Dat;

        Chr (134), Chr (143): BEGIN Dit; Dah; Dah; Dit; Dah; END; {KK1L: NOTE This is A-ring!!}
        Chr (132), Chr (142): BEGIN Dit; Dah; Dit; Dah; END;      {KK1L: NOTE This is A-umlaut!}
        Chr (148), Chr (153): BEGIN Dah; Dah; Dah; Dit; END;      {KK1L: NOTE This is O-umlaut!}

        ControlE:
            BEGIN
            CheckPtt;
            CWBuffer^ [CWBufferEnd].Length := 22;
            CWBuffer^ [CWBufferEnd].Key  := True;
            IncrementBufferEnd;
            CWBuffer^ [CWBufferEnd].Length := 10;
            CWBuffer^ [CWBufferEnd].Key    := False;
            IncrementBufferEnd;
            END;

        ControlDash:
            BEGIN
            CheckPtt;
            CWBuffer^ [CWBufferEnd].Length := 26;
            CWBuffer^ [CWBufferEnd].Key  := True;
            IncrementBufferEnd;
            CWBuffer^ [CWBufferEnd].Length := 10;
            CWBuffer^ [CWBufferEnd].Key    := False;
            IncrementBufferEnd;
            END;

        ControlK:
            BEGIN
            CheckPtt;
            CWBuffer^ [CWBufferEnd].Length := 30;
            CWBuffer^ [CWBufferEnd].Key  := True;
            IncrementBufferEnd;
            CWBuffer^ [CWBufferEnd].Length := 10;
            CWBuffer^ [CWBufferEnd].Key    := False;
            IncrementBufferEnd;
            END;

        ControlN:
            BEGIN
            CheckPtt;
            CWBuffer^ [CWBufferEnd].Length := 34;
            CWBuffer^ [CWBufferEnd].Key  := True;
            IncrementBufferEnd;
            CWBuffer^ [CWBufferEnd].Length := 10;
            CWBuffer^ [CWBufferEnd].Key    := False;
            IncrementBufferEnd;
            END;

        ControlO:
            BEGIN
            CheckPtt;
            CWBuffer^ [CWBufferEnd].Length := 38;
            CWBuffer^ [CWBufferEnd].Key  := True;
            IncrementBufferEnd;
            CWBuffer^ [CWBufferEnd].Length := 10;
            CWBuffer^ [CWBufferEnd].Key    := False;
            IncrementBufferEnd;
            END;

        ControlP:
            BEGIN
            CheckPtt;
            CWBuffer^ [CWBufferEnd].Length := 6;
            CWBuffer^ [CWBufferEnd].Key  := True;
            IncrementBufferEnd;
            CWBuffer^ [CWBufferEnd].Length := 10;
            CWBuffer^ [CWBufferEnd].Key    := False;
            IncrementBufferEnd;
            END;

        ControlQ:
            BEGIN
            CheckPtt;
            CWBuffer^ [CWBufferEnd].Length := 8;
            CWBuffer^ [CWBufferEnd].Key  := True;
            IncrementBufferEnd;
            CWBuffer^ [CWBufferEnd].Length := 10;
            CWBuffer^ [CWBufferEnd].Key    := False;
            IncrementBufferEnd;
            END;

        ControlBackSlash:
            BEGIN
            CheckPtt;
            CWBuffer^ [CWBufferEnd].Length := 10;
            CWBuffer^ [CWBufferEnd].Key  := True;
            IncrementBufferEnd;
            CWBuffer^ [CWBufferEnd].Length := 10;
            CWBuffer^ [CWBufferEnd].Key    := False;
            IncrementBufferEnd;
            END;

        ControlV:
            BEGIN
            CWBuffer^ [CWBufferEnd].Length := 12;
            CWBuffer^ [CWBufferEnd].Key  := True;
            IncrementBufferEnd;
            CWBuffer^ [CWBufferEnd].Length := 10;
            CWBuffer^ [CWBufferEnd].Key    := False;
            IncrementBufferEnd;
            END;

        ControlL:
            BEGIN
            CheckPtt;
            CWBuffer^ [CWBufferEnd].Length := 14;
            CWBuffer^ [CWBufferEnd].Key  := True;
            IncrementBufferEnd;
            CWBuffer^ [CWBufferEnd].Length := 10;
            CWBuffer^ [CWBufferEnd].Key    := False;
            IncrementBufferEnd;
            END;

        ControlF:   { Speed up command }
            BEGIN
            CWBuffer^ [CWBufferEnd].Length := 0;
            CWBuffer^ [CWBufferEnd].Key  := True;
            IncrementBufferEnd;
            END;

        ControlS:   { Slow down command }
            BEGIN
            CWBuffer^ [CWBufferEnd].Length := 0;
            CWBuffer^ [CWBufferEnd].Key  := False;
            IncrementBufferEnd;
            END;

        ControlX:   { Decrease weight command }
            BEGIN
            CWBuffer^ [CWBufferEnd].Length := -1;
            CWBuffer^ [CWBufferEnd].Key  := False;
            IncrementBufferEnd;
            END;

        ControlY:   { Increase weight command }
            BEGIN
            CWBuffer^ [CWBufferEnd].Length := -1;
            CWBuffer^ [CWBufferEnd].Key  := True;
            IncrementBufferEnd;
            END;

        ' ': BEGIN
             CheckPtt;
             CWBuffer^ [CWBufferEnd].Length := 30;
             CWBuffer^ [CWBufferEnd].Key  := False;
             IncrementBufferEnd;
             END;

        '^': BEGIN
             CheckPtt;
             CWBuffer^ [CWBufferEnd].Length := 15;
             CWBuffer^ [CWBufferEnd].Key  := False;
             IncrementBufferEnd;
             END;

        END;

    IF (Character > ' ') AND
       (Character <> ' ') AND
       (Character <> '^') THEN
           BEGIN
           CWBuffer^ [CWBufferEnd].Length := 20;
           CWBuffer^ [CWBufferEnd].Key  := False;
           IncrementBufferEnd;
           END;
    END;


PROCEDURE K1EAKeyer.AddStringToBuffer (Msg: String; Tone: INTEGER);

VAR Character: INTEGER;
    CommandState: BOOLEAN;

    BEGIN
    { Next two lines to fix bug where somtimes paddle speed gets ignored }

    PaddlePTTOffTimer := 0;
    PaddleActive := False;

    IF NOT CWBufferAvailable THEN
        BEGIN
        New (CWBuffer);
        CWBufferAvailable := True;
        END;

    MonitorTone := Tone;

    IF ActiveKeyerPort <> PortBeingUsed THEN
        BEGIN
        FlushCWBuffer;
        PortBeingUsed := ActiveKeyerPort;
        END;

    CommandState := False;

    IF Length (Msg) > 0 THEN
        BEGIN
        FOR Character := 1 TO Length (Msg) DO
            BEGIN
            IF CommandState THEN
                BEGIN
                CWBuffer^ [CWBufferEnd].Length := Ord ('0') - Ord (Msg [Character]);
                IncrementBufferEnd;
                CommandState := False;
                Continue;
                END;

            CASE UpCase (Msg [Character]) OF
                ControlLeftBracket: CommandState := True;

                ELSE
                    AddCharacterToBuffer (UpCase (Msg [Character]));
                END;

           END;
        END;
    END;



FUNCTION K1EAKeyer.BufferEmpty: BOOLEAN;

    BEGIN
    BufferEmpty := CWBufferStart = CWBufferEnd;
    END;


FUNCTION K1EAKeyer.CWStillBeingSent: BOOLEAN;

    BEGIN
    CWStillBeingSent := PTTAsserted;
    END;


PROCEDURE K1EAKeyer.FlushCWBuffer;

    BEGIN
    CWBufferStart := CWBufferEnd;

    IF (MonitorTone <> 0) AND (Not BeepTone.IsBeeping) THEN LNoSound;

    PTTDelayCount := 0;
    PTTAsserted   := False;

    TurnOffActivePort;
    PortBeingUsed := nil;
    END;


PROCEDURE K1EAKeyer.SetSpeed (Speed: INTEGER);

    BEGIN
    IF Speed > 0 THEN
        BEGIN
        CodeSpeed := Speed;
        ElementLength := Round (ElementLengthConstant / Speed);
        PaddlePTTOffTimerCount := ElementLength * PaddlePTTHoldCount;
        END;
    END;

Function K1EAKeyer.GetSpeed:Integer;
begin
   GetSpeed := CodeSpeed;
end;

Procedure K1EAKeyer.SetPaddlePttHoldCount(Count: INTEGER);
BEGIN
   PaddlePTTHoldCount := Count;
   PaddlePTTOffTimerCount := ElementLength * PaddlePTTHoldCount;
END;

Function K1EAKeyer.GetPaddlePttHoldCount:integer;
begin
   GetPaddlePttHoldCount := PaddlePTTHoldCount;
end;

PROCEDURE K1EAKeyer.SetWeight(w: real);
begin
   weight := w;
end;

Function K1EAKeyer.GetWeight:real;
begin
   GetWeight := weight;
end;

PROCEDURE K1EAKeyer.SetFarnsworthEnable(on: boolean);
begin
   FarnsworthEnable := on;
end;

PROCEDURE K1EAKeyer.SetPTTTurnOnDelay(delay: integer);
begin
   PTTTurnOnDelay := delay;
end;

Procedure K1EAKeyer.SetTuneWithDits(on: boolean);
begin
   TuneWithDits := on;
end;

Function K1EAKeyer.GetTuneWithDits:boolean;
begin
   GetTuneWithDits := TuneWithDits;
end;

Function K1EAKeyer.GetFarnsworthEnable:boolean;
begin
    GetFarnsworthEnable := FarnsworthEnable;
end;

Function K1EAKeyer.GetPTTTurnOnDelay:integer;
begin
   GetPTTTurnOnDelay := PTTTurnOnDelay;
end;

Procedure K1EAKeyer.SetPTTEnable(on: boolean);
begin
   PTTEnable := on;
end;

Function K1EAKeyer.GetPTTEnable:boolean;
begin
   GetPTTEnable := PTTEnable;
end;
 
PROCEDURE K1EAKeyer.SetFarnsworthSpeed(speed: integer);
begin
    FarnsworthSpeed := speed;
end;

Function K1EAKeyer.GetFarnsworthSpeed:integer;
begin
   GetFarnsworthSpeed := FarnsworthSpeed;
end;

Procedure K1EAKeyer.SetPaddleBug(On: Boolean);
begin
   paddlebug := on;
end;

PROCEDURE K1EAKeyer.SetPaddleMonitorTone(tone: Integer);
begin
   paddlemonitortone := tone;
end;

procedure K1EAKeyer.setpaddlespeed(speed: integer);
begin
   paddlespeed := speed;
end;

Procedure K1EAKeyer.SetMonitorTone(tone: integer);
begin
   MonitorTone := tone;
end;

Function K1EAKeyer.GetMonitorTone:integer;
begin
   GetMonitorTone := MonitorTone;
end;

Procedure K1EAKeyer.SetCountsSinceLastCW(count: integer);
begin
   CountsSinceLastCW := count;
end;

Function K1EAKeyer.GetCountsSinceLastCW:integer;
begin
   GetCountsSinceLastCW := CountsSinceLastCW;
end;


PROCEDURE K1EAKeyer.LetFootSwitchControlPTT;

    BEGIN
    IF PTTForcedOn THEN Exit;

    PTTFootSwitch := FootSw.getState;

    IF PTTFootSwitch <> LastFootSwitchStatus THEN
        BEGIN
        TurnOffActivePort;
        LastFootSwitchStatus := PTTFootSwitch;
        IF PaddlePTTOffTimer > 0 THEN PaddlePTTOffTimer := 1;
        END;
    END;

Function K1EAKeyer.GetPaddleBug:boolean;
begin
   GetPaddleBug := PaddleBug;
end;

Function K1EAKeyer.GetPaddleMonitorTone:integer;
begin
   getpaddlemonitortone := paddlemonitortone;
end;

Function K1EAKeyer.GetPaddleSpeed:integer;
begin
   getpaddlespeed := paddlespeed;
end;

PROCEDURE K1EAKeyer.InitializeKeyer;

    BEGIN
    DoingPaddle  := ActivePaddlePort <> nil;
    PaddlePTTOffTimerCount := ElementLength * PaddlePTTHoldCount;
    TurnOffActivePort;
    CWElementLength  := 0;
    CWBufferStart    := 0;
    CWBufferEnd      := 0;
    PTTAsserted := False;
    PTTDelayCount := 0;
    TurnOffActivePort;

    IF NOT KeyerInitialized THEN
        BEGIN
        ElementLengthConstant := 714;
        KeyerInitialized := True;
        END;
    ElementLength := Round (ElementLengthConstant / CodeSpeed);

    END;

Procedure K1EAKeyer.SetBeeper(b: Beeper);
begin
   BeepTone := b;
end;

Procedure K1EAKeyer.SetFootSwitch(f: FootSwitchx);
begin
   Footsw := f;
end;

PROCEDURE K1EAKeyer.UninitializeKeyer;
begin
   keyerinitialized := false;
end;

procedure K1EAKeyer.Timer;
var ditcontact,dahcontact: boolean;
begin
    IF DoingPaddle OR TuneWithDits THEN
        BEGIN
        IF SendStatus = NothingBeingSent THEN
            IF DitMemory THEN
                BEGIN
                Dit;
                SendStatus        := DitBeingSent;
                DitMemory         := False;
                PaddlePTTOffTimer := PaddlePTTOffTimerCount;
                END
            ELSE
                IF DahMemory THEN
                    BEGIN
                    Dah;
                    SendStatus        := DahBeingSent;
                    DahMemory         := False;
                    PaddlePTTOffTimer := PaddlePTTOffTimerCount;
                    SendingADah       := True;
                    END;

        IF TuneWithDits THEN
           IF ctrlshift then
                BEGIN
                TuningWithDits := True;
                RememberCodeSpeed := CodeSpeed;
                ElementLength := Round (ElementLengthConstant / 75);
                PaddlePTTOffTimerCount := ElementLength * PaddlePTTHoldCount;
                END
            ELSE
                IF TuningWithDits THEN
                    BEGIN
                    TuningWithDits := False;
                    CodeSpeed := RememberCodeSpeed;
                    ElementLength := Round (ElementLengthConstant / CodeSpeed);
                    PaddlePTTOffTimerCount := ElementLength * PaddlePTTHoldCount;
                    END;

        contacts(ditcontact,dahcontact);
        IF DitContact OR TuningWithDits THEN
            BEGIN
            CASE SendStatus OF
                NothingBeingSent:
                    BEGIN
                    CWBufferStart := CWBufferEnd;
                    IF (MonitorTone <> 0) AND (Not BeepTone.IsBeeping) THEN LNoSound;
                    TurnOffActivePort;
                    MonitorTone := PaddleMonitorTone;
                    Dit;
                    SendStatus := DitBeingSent;
                    END;

                DahBeingSent: IF CurtMode = ModeA THEN
                                  DitMemory := NOT SendingADah
                              ELSE
                                  DitMemory := True;
                END;

            PaddlePTTOffTimer := PaddlePTTOffTimerCount;

            IF NOT PaddleActive THEN
                BEGIN
                PaddleActive := True;
                IF PaddleSpeed <> 0 THEN SetSpeed (PaddleSpeed);
                END;
            END;

        IF DahContact THEN
            BEGIN
            IF PaddleBug THEN
                BEGIN
                IF NOT PaddleBugDahBeingSent THEN
                    BEGIN
                    PaddleBugDahBeingSent := True;

                    CWBufferStart := CWBufferEnd;
                    IF (MonitorTone <> 0) AND (Not BeepTone.IsBeeping) THEN LNoSound;
                    MonitorTone := PaddleMonitorTone;
                    TurnOffActivePort;
                    CWElementLength := 0;

                    CWBuffer^ [CWBufferEnd].Length := 10;
                    CWBuffer^ [CWBufferEnd].Key  := True;
                    IncrementBufferEnd;

                    IF NOT PTTAsserted THEN
                        BEGIN
                        PTTAsserted := True;
                        PTTDelayCount := PTTTurnOnDelay;
                        TurnOffActivePort;
                        END;
                    END;
                END
            ELSE
                CASE SendStatus OF
                    NothingBeingSent:
                        BEGIN
                        CWBufferStart := CWBufferEnd;
                        IF (MonitorTone <> 0) AND (Not BeepTone.IsBeeping) THEN LNoSound;
                        TurnOffActivePort;
                        MonitorTone := PaddleMonitorTone;
                        Dah;
                        SendStatus := DahBeingSent;
                        SendingADah := True;
                        END;

                    DitBeingSent: DahMemory := True;
                    END;

            { Runtime error at next line 201 @ 20AD:59C8 }

            PaddlePTTOffTimer := PaddlePTTOffTimerCount;

            IF NOT PaddleActive THEN
                BEGIN
                PaddleActive := True;
                IF PaddleSpeed <> 0 THEN SetSpeed (PaddleSpeed);
                END;
            END
        ELSE
            IF PaddleBug AND PaddleBugDahBeingSent THEN
                BEGIN
                PaddleBugDahBeingSent := False;
                TurnOffActivePort;
                IF (MonitorTone <> 0) AND (Not BeepTone.IsBeeping) THEN LNoSound;
                CWBufferStart := CWBufferEnd;
                SendStatus := NothingBeingSent;
                END;

        END;

    { Next we check the Packet Port }

    IF NOT TuneWithDits THEN
        IF Tuning THEN
            BEGIN
            IF not ctrlshift THEN { Not left shift & control keys }
                BEGIN
                Tuning := False;
                TurnOffActivePort;
                IF (MonitorTone <> 0) AND (Not BeepTone.IsBeeping) THEN LNoSound;
                CWBufferStart := CWBufferEnd;
                SendStatus := NothingBeingSent;
//                RITEnable := RememberRIT;
                AddStringToBuffer ('', MonitorTone); {KK1L: 6.73 Restore tone for CW}
                END;
            END
        ELSE
            BEGIN
            IF ctrlshift THEN  { Left shift and control keys }
              BEGIN
                Tuning := True;
//                RememberRIT := RITEnable;
//                RITEnable := False;

               AddStringToBuffer ('', 0); {KK1L: 6.73 Disable tone when tuning without dits (using as PTT)}

                { Abort any CW message }

                CWBufferStart := CWBufferEnd;
                IF (MonitorTone <> 0) AND (Not BeepTone.IsBeeping) THEN LNoSound;
                TurnOffActivePort;
                CWElementLength := 0;

                IF NOT CWBufferAvailable THEN
                    BEGIN
                    New (CWBuffer);
                    CWBufferAvailable := True;
                    END;

                CWBuffer^ [CWBufferEnd].Length := 10;
                CWBuffer^ [CWBufferEnd].Key  := True;
                IncrementBufferEnd;

                IF NOT PTTAsserted THEN
                    BEGIN
                    PTTAsserted := True;
                    PTTDelayCount := PTTTurnOnDelay;
                    TurnOffActivePort;
                    END;
                END;
           END;

    { Now we check the radio communication and shift keys.  Used to be
      a bunch of IF THEN ELSEs here, but I put everything up so it
      gets executed each time }


    { Check PTTDelayCount timer.  If not active, then check CW status }

    IF PTTDelayCount > 0 THEN
        BEGIN
        IF PTTDelayCount = PTTTurnOnDelay THEN TurnOffActivePort;

        IF (FsCwGrant) and (Footsw.portDefined) THEN
        begin
            if Footsw.getState then Dec (PTTDelayCount);
        end
        ELSE
            Dec (PTTDelayCount);
        END
    ELSE
        BEGIN
        IF (CWElementLength > 0) AND NOT (Tuning OR PaddleBugDahBeingSent) THEN
            Dec (CWElementLength);

        IF CWElementLength = 0 THEN
            BEGIN
            IF CWBufferStart <> CWBufferEnd THEN
                BEGIN
                IF CWBuffer^ [CWBufferStart].Length = 0 THEN { Bump speed command }
                    BEGIN
                    IF CWBuffer^ [CWBufferStart].Key THEN
                        BEGIN
                        IF CodeSpeed < 98 THEN Inc (CodeSpeed);
                        IF (CodeSpeed > 25) AND (CodeSpeed < 98) THEN Inc (CodeSpeed);
                        IF (CodeSpeed > 35) AND (CodeSpeed < 98) THEN Inc (CodeSpeed);
                        IF (CodeSpeed > 45) AND (CodeSpeed < 98) THEN Inc (CodeSpeed);
                        END
                    ELSE
                        BEGIN
                        IF CodeSpeed > 49 THEN Dec (CodeSpeed);
                        IF CodeSpeed > 38 THEN Dec (CodeSpeed);
                        IF CodeSpeed > 27 THEN Dec (CodeSpeed);
                        IF CodeSpeed >  2 THEN Dec (CodeSpeed);
                        END;

                    ElementLength := Round (ElementLengthConstant / CodeSpeed);
                    END
                ELSE
                    IF CWBuffer^ [CWBufferStart].Length < 0 THEN { Command mode }
                        BEGIN
                        CASE CWBuffer^ [CWBufferStart].Length OF
                            -1: IF CWBuffer^ [CWBufferStart].Key THEN
                                    Weight := Weight + 0.03
                                ELSE
                                    Weight := Weight - 0.03;

                            -2: FarnsworthEnable := NOT FarnsworthEnable;
                            -3: FarnsworthSpeed := 25;
                            -4: FarnsworthSpeed := 35;
                            -5: FarnsworthSpeed := 45;
                            -6: FarnsworthSpeed := 55;
                            -7: FarnsworthSpeed := 75;
                            -8: FarnsworthSpeed := 95;
                            END;
                        END
                    ELSE
                        BEGIN
                        IF CWBuffer^ [CWBufferStart].Key THEN
                            BEGIN
                            IF (Not BeepTone.IsBeeping) AND (MonitorTone <> 0) THEN LSound (MonitorTone);
                            TurnOnActivePort;
                            CWElementLength := Round (Weight * (CWBuffer^ [CWBufferStart].Length * ElementLength) / 10.0);
                            END
                        ELSE
                            BEGIN
                            SendingADah := False;
                            TurnOffActivePort;
                            IF (MonitorTone <> 0) AND (Not BeepTone.IsBeeping) THEN LNoSound;

                            IF FarnsworthEnable AND
                               (CWBuffer^ [CWBufferStart].Length >= 15) AND
                               (CodeSpeed < FarnsworthSpeed) THEN
                                   BEGIN
                                   IF CWBuffer^ [CWBufferStart].Length = 15 THEN
                                       CWBuffer^ [CWBufferStart].Length := CWBuffer^ [CWBufferStart].Length
                                                                       + (FarnsworthSpeed - CodeSpeed)
                                   ELSE
                                       CWBuffer^ [CWBufferStart].Length := CWBuffer^ [CWBufferStart].Length
                                                                       + ((FarnsworthSpeed - CodeSpeed) * 2);
                                   END;

                            CWElementLength := Round (((CWBuffer^ [CWBufferStart].Length * ElementLength) / 10.0) -
                                               ((Weight - 1.0) * 2 * ElementLength));

                            END;
                        END;

                Inc (CWBufferStart);
                CWBufferStart := CWBufferStart MOD CWBufferSize;
                END
            ELSE
                BEGIN
                SendStatus := NothingBeingSent;

                IF PaddleActive THEN
                    BEGIN
                    IF PaddlePTTOffTimer > 0 THEN Dec (PaddlePTTOffTimer);
                    IF PaddlePTTOffTimer = 0 THEN PaddleActive := False;
                    END
                ELSE
                    BEGIN
                    PTTAsserted := False;
                    PaddlePTTOffTimer := 0;
                    END;

                IF CountsSinceLastCW = 0 THEN CountsSinceLastCW := 1;

                IF ActivePortOn THEN TurnOffActivePort;
                END;
            END;
        END;

        if pttenable and dvppttstatus and (not playingfile) then
        begin
           dvppttstatus := false;
           pttunforce;
        end;
end;

END.
