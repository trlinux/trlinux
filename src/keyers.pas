unit keyers;
{$mode objfpc}

INTERFACE

USES trcrt,communication,beep,foot,radio;

CONST
    CWBufferSize           = 1024;
    WinkeyerHangTime: Array [0..3] of Integer = (8, 9, 11, 15);

TYPE
    SendStatusType = (NothingBeingSent, DitBeingSent, DahBeingSent);
    CurtisMode = (ModeA, ModeB, Ultimatic);
    CWElementRecord = RECORD
        Length: INTEGER;
        Key:    BOOLEAN;
        END;
 
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

     CWBufferType = ARRAY [0..CWBufferSize] OF CWElementRecord;
     CWBufferPtr  = ^CWBufferType;
     WinkeyerBuffer = Array [0..CWBufferSize-1] of Char;

     Keyer = class
        PROCEDURE AddCharacterToBuffer (Character: CHAR);virtual;abstract;
        PROCEDURE AddStringToBuffer (Msg: String; Tone: INTEGER);virtual;abstract;
        FUNCTION  BufferEmpty: BOOLEAN;virtual;abstract;
        FUNCTION  CWStillBeingSent: BOOLEAN;virtual;abstract;
        FUNCTION  DeleteLastCharacter: BOOLEAN;virtual;abstract;

        PROCEDURE FlushCWBuffer;virtual;abstract;
        PROCEDURE InitializeKeyer;virtual;abstract;
        PROCEDURE PTTForceOn;virtual;abstract;
        PROCEDURE PTTUnForce;virtual;abstract;
        PROCEDURE SetSpeed (Speed: INTEGER);virtual;abstract;
        Function GetSpeed:INTEGER;virtual;abstract;
        PROCEDURE UnInitializeKeyer;virtual;abstract;
        PROCEDURE SetPaddlePttHoldCount(Count: INTEGER);virtual;abstract;
        PROCEDURE SetPaddleBug(On: Boolean);virtual;abstract;
        PROCEDURE SetPaddleMonitorTone(tone: Integer);virtual;abstract;
        PROCEDURE SetPaddleSpeed(speed: Integer);virtual;abstract;
        PROCEDURE LetFootSwitchControlPtt;virtual;abstract;
        PROCEDURE Timer;virtual;abstract;
        Function GetPaddleBug:boolean;virtual;abstract;
        Function GetPaddleMonitorTone:integer;virtual;abstract;
        Function GetPaddleSpeed:integer;virtual;abstract;
        Function GetPaddlePTTHoldCount:integer;virtual;abstract;
        PROCEDURE SetWeight(w: real);virtual;abstract;
        Function GetWeight:real;virtual;abstract;
        PROCEDURE SetFarnsworthEnable(on: boolean);virtual;abstract;
        Function GetFarnsworthEnable:boolean;virtual;abstract;
        PROCEDURE SetFarnsworthSpeed(speed: integer);virtual;abstract;
        Function GetFarnsworthSpeed:integer;virtual;abstract;
        PROCEDURE SetPTTTurnOnDelay(delay: integer);virtual;abstract;
        Function GetPTTTurnOnDelay:integer;virtual;abstract;
        Procedure SetPTTEnable(on: boolean);virtual;abstract;
        Function GetPTTEnable:boolean;virtual;abstract;
        Procedure SetTuneWithDits(on: boolean);virtual;abstract;
        Function GetTuneWithDits:boolean;virtual;abstract;
        Procedure SetMonitorTone(tone: integer);virtual;abstract;
        Function GetMonitorTone:integer;virtual;abstract;
        Procedure SetCountsSinceLastCW(count: integer);virtual;abstract;
        Function GetCountsSinceLastCW:integer;virtual;abstract;
        Procedure SetCurtisMode(m: CurtisMode);virtual;abstract;
        Function GetCurtisMode:CurtisMode;virtual;abstract;
        Function GetKeyerInitialized:boolean;virtual;abstract;
        Procedure SetSwapPaddles(on: boolean);virtual;abstract;
        Function GetSwapPaddles:boolean;virtual;abstract;
        Function GetPTTAsserted:boolean;virtual;abstract;
        Procedure SetPTTFootSwitch(on :boolean);virtual;abstract;
        Procedure SetActiveRadio(r: RadioType);virtual;abstract;

        Procedure SetFootSwitch(f: FootSwitchx);virtual;abstract;
        Procedure SetCwGrant(on: boolean);virtual;abstract;
        END;

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

        Procedure SetBeeper(b: Beeper);
        Procedure SetRadioOnePort(k: keyerportx);
        Procedure SetRadioTwoPort(k: keyerportx);
        Procedure SetActivePaddlePort(port: keyerportx);
        END;


     WinKeyer = class(keyer)

     private
        CodeSpeed:        INTEGER;
        curtmode: CurtisMode;
        KeyerInitialized: BOOLEAN;
        ElementLength:         INTEGER;
        ElementLengthConstant: INTEGER;
        PaddleActive:            BOOLEAN;
        PaddleBug:               BOOLEAN;
        PaddlePTTOffTimer:      INTEGER;
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
        RememberCodeSpeed:        INTEGER;
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
        DoingPaddle:  BOOLEAN;
        LastFootSwitchStatus: BOOLEAN;
        SendStatus:           SendStatusType;
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
    FsCwGrant := False;
    contactstate := 0;
end;


Procedure K1EAKeyer.SetCwGrant(on: boolean);
begin
   FsCwGrant := on;
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

    IF (PTTAsserted OR PTTForcedOn OR PTTFootSwitch) AND PTTEnable THEN
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
end;

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
      st := st + $80;
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
//   flushcwbuffer; //probably not necessary
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
         pincfg := Char(Integer(pincfg) and $02)
      else
         pincfg := Char(Integer(pincfg) or $02);
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
         pincfg := Char(Integer(pincfg) and $02)
      else
         pincfg := Char(Integer(pincfg) or $02);
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
