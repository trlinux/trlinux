UNIT LogK1EA;
{$mode objfpc}

{$V-}

INTERFACE

USES LogGrid, Dos, trCrt, SlowTree, Tree, communication, beep, foot, radio,
   keyerk1ea,keyerwin,keyers,so2r,keyeryccc,footyccc;

CONST
    RadioCommandBufferSize = 100;

{ Parallel port bit assignments:  (Bits are shown 0 to 7.  0 = LSB)

  Pin     Port    Bit    Description
  ---     ----    ---    -----------
C  1     Base+2    0     Input/Output - Output to emitters of transistors.
D  2     Base+0    0     Output - Band bit 0 or DVK Clear
D  3     Base+0    1     Output - DVK #1
D  4     Base+0    2     Output - DVK #2
D  5     Base+0    3     Output - DVK #3 or WX0B SO2R Stereo/Mono KK1L: 6.71
D  6     Base+0    4     Output - DVK #4
D  7     Base+0    5     Output - Band bit 1 or DVK #5 KK1L: 6.71
D  8     Base+0    6     Output - Band bit 2 or DVK #6 KK1L: 6.71
D  9     Base+0    7     Output - Band bit 3 or WX0B SO2R Stereo/Mono KK1L: 6.71
S 10
S 11
S 12     Base+1    5     Input - Dit paddle
S 13     Base+1    4     Input - Dah paddle
C 14     Base+2    1     Input/Output - Used for paddle pullup or relay.
S 15     Base+1    3     Input - Foot switch input.
C 16     Base+2    2     Input/Output - Output PTT
C 17     Base+2    3     Input/Output - Output CW

  18-25                  Grounds

  None   Base+2    5     W9XT card record bit (set to 1, pulse memory, then
                         clear this bit to stop recording).

  BIT Patterns for Band bits:

  BAND   9   8   7   2      Value written to I/O port
  ----  --- --- --- ---     -------------------------
  160   lo  lo  lo  hi      $01   $00 is written if not one of these bands.
   80   lo  lo  hi  lo      $20
   40   lo  lo  hi  hi      $21
   30   lo  hi  lo  lo      $40
   20   lo  hi  lo  hi      $41
   17   lo  hi  hi  lo      $60
   15   lo  hi  hi  hi      $61
   12   hi  lo  lo  lo      $80
   10   hi  lo  lo  hi      $81
    6   hi  lo  hi  lo      $A0
    2   hi  lo  hi  hi      $A1
  222   hi  hi  lo  lo      $C0
  432   hi  hi  lo  hi      $C1
  902   hi  hi  hi  lo      $E0
  1296  hi  hi  hi  hi      $E1  }


TYPE
    k1eatimer = class
    public   
       procedure timer(caughtup: boolean);
    end;

    SendStatusType = (NothingBeingSent, DitBeingSent, DahBeingSent);
    RotatorType = (NoRotator, DCU1Rotator, OrionRotator, YaesuRotator); {KK1L: 6.71 Added YaesuRotator}

    GobbleStatus = (GobbleDone, GobbleAborted, GobbledTooMuch);

    ShiftKeyType = (Shift, AltShift, None);

    InterfacedRadioType = (NoInterfacedRadio, K2,
                                              TS850,
                                              FT100,
                                              FT817, {KK1L: 6.73 Also used for FT897}
                                              FT847,
                                              FT890,  { Also used for FT900 }
                                              FT920,
                                              FT990,
                                              FT1000,
                                              FT1000MP,
                                              IC706,
                                              IC706II,
                                              IC706IIG,
                                              IC707,
                                              IC725,
                                              IC726,
                                              IC728,
                                              IC729,
                                              IC735,
                                              IC736,
                                              IC737,
                                              IC738,
                                              IC746,
                                              IC746PRO,
                                              IC756,
                                              IC756PRO,
                                              IC756PROII,
                                              IC761,
                                              IC765,
                                              IC775,
                                              IC781,
                                              JST245,
                                              OMNI6,  {KK1L: 6.73}
                                              ORION,
                                              ARGO    {KK1L: 6.73}
                                              );


    { Notes:

            FT980 - I decided this wasn't going to work out very well.
                    The radio has to be put into a mode to enable
                    computer control (sending five 00 bytes = External
                    On command) - then the radio sends 148 bytes of
                    status update bits.  During the time the radio is
                    in this mode, the EXT indicator is lit, and the
                    front panel controls are disabled.

    }


VAR ActiveDVKPort:     parallelportx;
    k1eatimerx:        k1eatimer;

    ActiveModemPort:   serialportx;
    ActiveMultiPort:   serialportx;
    ActiveRadio:       RadioType;
    InactiveRadio:     RadioType; {KK1L: 6.73}
    ActiveRotatorType: RotatorType;
    ActiveRotatorPort: serialportx;
    ActiveRTTYPort:    serialportx;
    ActiveStereoPort:  parallelportx; {KK1L: 6.71}


    BumpCount: INTEGER;
    BumpDelay: INTEGER;

    ReforkCount: INTEGER;
    ReforkDelay: INTEGER;

    CPUKeyer:          K1EAKeyer;
    WinKey:            WinKeyer;
    YcccKey:           YcccKeyer;
    ActiveKeyer:       Keyer;
    so2rbox:           so2rinterface;

    CQRITEnabled:      BOOLEAN;

    DVKDelay: INTEGER;
    DVKPortAddress: WORD;
    DVPOn:          BOOLEAN;
    DVKClearAllBits, DVKClearMessageBits: BYTE; {KK1L: 6.72}

    EightBitRTTYPort:      BOOLEAN; {KK1L: 6.71}
    EnableSixDVKMessages:  BOOLEAN; {KK1L: 6.72}

    FootSwitchDebug:   BOOLEAN;
    FootSwitchMode:    FootSwitchModeType;
    FT1000MPCWReverse: BOOLEAN;

    IcomCommandPause:    WORD;
    IcomRetries: INTEGER; {KK1L: 6.72}
    RadioOneIcomFilterByte:      BYTE;
    RadioTwoIcomFilterByte:      BYTE;
    IcomResponseTimeout: WORD;

    JSTResponseTimeout: WORD;

    K1EANetworkEnable: BOOLEAN;
    KenwoodResponseTimeout: WORD;


    MultiPortBaudRate:      LONGINT;
    MultiReceiveCharBuffer: CharacterBuffer;
    MultiSendCharBuffer:    CharacterBuffer;

    NetDebug: BOOLEAN;
    NoPollDuringPTT: BOOLEAN;

    NetDebugBinaryOutput: FILE;
    NetDebugBinaryInput:  FILE;

    OrionResponseTimeout: WORD;

    PacketReceiveCharBuffer: CharacterBuffer;
    PacketSendCharBuffer:    CharacterBuffer;

    PacketAddLF:             BOOLEAN;
    PollRadioOne:           BOOLEAN;
    PollRadioTwo:           BOOLEAN;

    Radio1BandOutputPort:     parallelportx;
    Radio1CommandBufferEnd:   INTEGER;
    Radio1CommandBufferStart: INTEGER;
    Radio1CommandDelay:       INTEGER;
    Radio1ControlDelay:       INTEGER;
    Radio1ControlPort:        serialportx;
    Radio1FrequencyAdder:     LONGINT;
    Radio1IcomFilterByte:     BYTE;
    Radio1PollDelay:          INTEGER;
    Radio1Type:               InterfacedRadioType;

    Radio2BandOutputPort:     parallelportx;
    Radio2CommandBufferStart: INTEGER;
    Radio2CommandBufferEnd:   INTEGER;
    Radio2CommandDelay:       INTEGER;
    Radio2ControlDelay:       INTEGER;
    Radio2ControlPort:        serialportx;
    Radio2FrequencyAdder:     LONGINT;
    Radio2IcomFilterByte:     BYTE;
    Radio2PollDelay:          INTEGER;
    Radio2Type:               InterfacedRadioType;

    RadioDebugWrite:          FILE;
    RadioDebugMode:           BOOLEAN;

    RelayControlPort:         keyerportx;
    RotatorSendCharBuffer:    CharacterBuffer;

    RTTYReceiveCharBuffer: CharacterBuffer;
    RTTYSendCharBuffer:    CharacterBuffer;

    StereoControlPin:         INTEGER; {KK1L: 6.71}
    SwapRadioRelaySense:      BOOLEAN;
    TalkDebugWrite:           FILE;
    TalkDebugMode:            BOOLEAN;
    TimerInitialized:         BOOLEAN;

    Tone: Beeper;
    Footsw: FootSwitchx;
    footparallel: FootSwitchx;
    footso2r: FootSwitchx;

    Radio1ReceiverAddress: BYTE;
    Radio2ReceiverAddress: BYTE;

    RITEnable:          BOOLEAN;

    RTTYPortAddress: WORD;
    RTTYReceiveString: Str20;
    RTTYSendString: Str20;

    ShiftKeyEnable:     ShiftKeyType;
    StableRadio1Freq:   LONGINT;
    StableRadio2Freq:   LONGINT;
    StereoPinState:     BOOLEAN; {KK1L: 6.71}

    Trace:              BOOLEAN;

    YaesuResponseTimeout: WORD;


PROCEDURE AddRadioCommandCharacter (Radio: RadioType; Character: CHAR);
PROCEDURE ClearRIT;

PROCEDURE DVKEnableWrite;
PROCEDURE DVKDisableWrite;
FUNCTION  DVKMessagePlaying: BOOLEAN;

{KK1L: 6.71 Added Polling to keep from waiting for current FreqPoll}

FUNCTION  GetRadioParameters (Radio: RadioType;
                              RadioInfoString: STRING;
                              Var Freq: LONGINT;
                              Var Band: BandType;
                              Var Mode: ModeType;
                              Polling: BOOLEAN;
                              Debug: BOOLEAN): BOOLEAN;

FUNCTION  GetRadioInformation (Radio: RadioType; Polling: BOOLEAN): STRING; {KK1L: 6.71 Added Polling}
PROCEDURE K1EAInit;
PROCEDURE OutputBandInfo (Radio: RadioType; Band: BandType);
PROCEDURE PutRadioIntoSplit (Radio: RadioType);
PROCEDURE PutRadioOutOfSplit (Radio: RadioType); {KK1L: 6.64 Added to recover from bandmap split}
FUNCTION  RadioSendBufferEmpty (Radio: RadioType): BOOLEAN;
PROCEDURE SendMultiMessage (Message: STRING);
PROCEDURE SendPacketMessage (Message: STRING);
PROCEDURE SendRotatorMessage (Message: STRING);
PROCEDURE SetDVKDelay (Delay: INTEGER);
PROCEDURE SetRadioFreq (Radio: RadioType; Freq: LONGINT; Mode: ModeType; VFO: Char);
PROCEDURE SetRelayForActiveRadio (Radio: RadioType);
PROCEDURE SetStereoPin (PinNumber: INTEGER; PinSet: BOOLEAN); {KK1L: 6.71}
PROCEDURE StartDVK (MemorySwitch: INTEGER);
PROCEDURE Wait (DelayTimeMs: LONGINT);
PROCEDURE TimerInit;
PROCEDURE CloseDebug;


IMPLEMENTATION

Uses LogWind {KK1L: 6.71 added for DEBUG (to use QuickDisplay)}
   ,unix,baseunix,sysutils,linuxsound,timer,keycode,xkb;

CONST ParallelOutputPullupOffset  = 5;
      ParallelOutputAddressOffset = 2;
      ParallelInputAddressOffset  = 1;



VAR


    DelayCount:   LONGINT;
    DoingDVK:     BOOLEAN;
    DoingModem:   BOOLEAN;
    DoingMulti:   BOOLEAN;
    DoingPacket:  BOOLEAN;
    DoingRotator: BOOLEAN;
    DoingRTTY:    BOOLEAN;
    DVKTimeOut:   INTEGER;

    LastRadioOneMessageRead: Str80;
    LastRadioOneMessageTime: TimeRecord; {KK1L: 6.71}
    LastRadioTwoMessageRead: Str80;
    LastRadioTwoMessageTime: TimeRecord; {KK1L: 6.71}
    LastRadioOneFreq: LONGINT;
    LastRadioTwoFreq: LONGINT;

    ModemCharacterSentDelayCount: INTEGER;
    ModemDelayCount:              INTEGER;

    MultiCharacterSentDelayCount: INTEGER;
    MultiDelayCount:              INTEGER;

    PacketOutputDelay:        INTEGER;

    Radio1Delay:         INTEGER;
    Radio2Delay:         INTEGER;
    Radio1CommandBuffer: ARRAY [0..RadioCommandBufferSize - 1] OF CHAR;
    Radio2CommandBuffer: ARRAY [0..RadioCommandBufferSize - 1] OF CHAR;

    RadioOneBandOutputStatus: BandType;
    RadioTwoBandOutputStatus: BandType;


    RTTYCharacterSentDelayCount: INTEGER;
    RTTYDelayCount:              INTEGER;


PROCEDURE AddK1EACheckSumToString (VAR Message: STRING);

VAR Index, Total: BYTE;

    BEGIN
    Total := 0;

    IF Length (Message) > 0 THEN
        FOR Index := 1 TO Length (Message) DO
            Total := Total + Ord (Message [Index]);

    Total := Total OR $80;

    Message := Message + Chr (Total);
    END;



FUNCTION GobbleCharactersFromSerialPort (ControlPort: serialportx;
                                         MaximumGobbleCount: INTEGER): GobbleStatus;

VAR GobbleCount, DelayCount: INTEGER;

    { If we are not waiting for more data from the radio, we need to
      gobble up any characters that the radio might be spitting out
      for whatever reason }

    BEGIN
    GobbleCount := 0;

    WHILE ControlPort.CharReady DO
        BEGIN
        ControlPort.ReadChar;
        Inc (GobbleCount);

        { A nice little loop to delay a bit, but keeping an eye on the
          keyboard and footswitch }

        FOR DelayCount := 1 TO 10 DO
            BEGIN
            IF NewKeyPressed OR Footsw.getState THEN
                BEGIN
                GobbleCharactersFromSerialPort := GobbleAborted;
                Exit;
                END;

//            IF CharReady (ControlPort) THEN Break;
//            Wait (1);
   millisleep;
            END;

        { Am I forver gettting characters?  If so, we have a problem }

        IF GobbleCount > MaximumGobbleCount THEN
            BEGIN
            GobbleCharactersFromSerialPort := GobbledTooMuch;
            Exit;
            END;
        END;

    { Okay now }

    GobbleCharactersFromSerialPort := GobbleDone;
    END;



PROCEDURE TurnOn847CAT (Radio: RadioType);

    BEGIN
    AddRadioCommandCharacter (Radio, NullKey);
    AddRadioCommandCharacter (Radio, NullKey);
    AddRadioCommandCharacter (Radio, NullKey);
    AddRadioCommandCharacter (Radio, NullKey);
    AddRadioCommandCharacter (Radio, Chr (0));
    END;

PROCEDURE TurnOff847Cat (Radio: RadioType);

    BEGIN
    AddRadioCommandCharacter (Radio, NullKey);
    AddRadioCommandCharacter (Radio, NullKey);
    AddRadioCommandCharacter (Radio, NullKey);
    AddRadioCommandCharacter (Radio, NullKey);
    AddRadioCommandCharacter (Radio, Chr ($80));
    END;



FUNCTION GetIcomResponse (Radio: RadioType; VAR IcomAckString: STRING): BOOLEAN;

{ This function reads in data from an Icom radio.  It seems to expect an
  echo of the command sent to the radio - which I am not sure always
  occurs }

VAR Timeout: LONGINT;
    ControlPort: serialportx;
    FoundTheEcho: BOOLEAN;
    Resultx, CharPointer: INTEGER;
    DebugString: STRING;

    BEGIN
    GetIcomResponse := False;
    FoundTheEcho := False;
    IcomAckString := '';

    CASE Radio OF
        RadioOne: BEGIN
                  ControlPort  := Radio1ControlPort;
                  END;

        RadioTwo: BEGIN
                  ControlPort  := Radio2ControlPort;
                  END;
        END;

    REPEAT
        TimeOut := 0;

        REPEAT
            Inc (TimeOut);

            IF TimeOut > IcomResponseTimeout THEN
                BEGIN
                { Clear last message so if fail next time, we get null string }

                CASE Radio OF
                    RadioOne:
                        LastRadioOneMessageRead := '';

                    RadioTwo:
                        LastRadioTwoMessageRead := '';
                    END;

                Exit;
                END;

            IF KeyPressed OR Footsw.getState THEN
                BEGIN
                IF RadioDebugMode THEN
                    BEGIN
                    FOR CharPointer := 1 TO SizeOf (DebugString) - 1 DO
                        DebugString [CharPointer] := Chr (0);

                    DebugString := 'KEY OR FOOTSWITCH PRESSED';
                    BlockWrite (RadioDebugWrite, DebugString, SizeOf (DebugString), Resultx);
                    END;

                Exit;
                END;

//            IF ControlPort.CharReady THEN Break;

//            Wait (1);
        millisleep;

        UNTIL ControlPort.CharReady;

        { Read in the data }

        IcomAckString := IcomAckString + ControlPort.ReadChar;

        { Don't start an Icom string without $FE being the first byte }

        IF (Length (IcomAckString) = 1) AND (IcomAckString [1] <> Chr ($FE)) THEN
            IcomAckString := '';

        { You know, the first two bytes should be $FE really. Let's get fancy
          and fix the string that way. }

        IF (Length (IcomAckString) = 2) AND (IcomAckString [2] <> Chr ($FE)) THEN
            Insert (Chr ($FE), IcomAckString, 1);

        { The first $FD is assumed to end the Icom echo }

        IF NOT FoundTheEcho THEN
            IF IcomAckString [Length (IcomAckString)] = Chr ($FD) THEN
                BEGIN
                IcomAckString := '';   { Start again!! }
                FoundTheEcho := True;
                END;

        { See if we have gotten all the way to the end of it }

        IF IcomAckString [Length (IcomAckString)] = Chr ($FD) THEN
            BEGIN
            GetIcomResponse := True;
            Exit;
            END;

    UNTIL Length (IcomAckString) > 160;

    { We didn't find what we were looking for }

    END;



FUNCTION GetOrionResponse (Radio: RadioType; VAR OrionData: STRING): BOOLEAN;

{ This function reads in data from an Orion radio.  It seems to expect an
  echo of the command sent to the radio - which I am not sure always
  occurs }

VAR TimeoutTrigger, Timeout: LONGINT;
    ControlPort: serialportx;
    Resultx, CharPointer: INTEGER;
    DebugString: STRING;

    BEGIN
    GetOrionResponse := False;
    OrionData := '';

    CASE Radio OF
        RadioOne: ControlPort  := Radio1ControlPort;
        RadioTwo: ControlPort  := Radio2ControlPort;
        END;

    TimeOutTrigger := OrionResponseTimeout * 1000;

    REPEAT
        TimeOut := 0;

        REPEAT
            Inc (TimeOut);

            IF TimeOut > TimeOutTrigger THEN
                BEGIN

                CASE Radio OF
                    RadioOne:
                        LastRadioOneMessageRead := '';

                    RadioTwo:
                        LastRadioTwoMessageRead := '';

                    END;

                IF RadioDebugMode THEN
                    BEGIN
                    FOR CharPointer := 1 TO SizeOf (DebugString) - 1 DO
                        DebugString [CharPointer] := Chr (0);

                    DebugString := 'TIMEOUT ' + OrionData;
                    BlockWrite (RadioDebugWrite, DebugString, SizeOf (DebugString), Resultx);
                    END;

                Exit;
                END;

            IF KeyPressed OR Footsw.getState THEN
                BEGIN
                IF RadioDebugMode THEN
                    BEGIN
                    FOR CharPointer := 1 TO SizeOf (DebugString) - 1 DO
                        DebugString [CharPointer] := Chr (0);

                    DebugString := 'KEY OR FOOTSWITCH PRESSED';
                    BlockWrite (RadioDebugWrite, DebugString, SizeOf (DebugString), Resultx);
                    END;

                Exit;
                END;

        UNTIL ControlPort.CharReady;

        { Read in the data }

        OrionData := OrionData + ControlPort.ReadChar;

        { See if we have gotten all the way to the end of it }

        IF OrionData [Length (OrionData)] = CarriageReturn THEN
            BEGIN
            GetOrionResponse := True;
            Exit;
            END;

    UNTIL Length (OrionData) > 160;

    { We didn't find what we were looking for }

    END;



FUNCTION GetKenwoodResponse (Radio: RadioType; VAR KenwoodData: STRING): BOOLEAN;

{ This function reads in data from an Kenwood radio.  It seems to expect an
  echo of the command sent to the radio - which I am not sure always
  occurs }

VAR Timeout: LONGINT;
    ControlPort: serialportx;
    Resultx, CharPointer: INTEGER;
    DebugString: STRING;

    BEGIN
    GetKenwoodResponse := False;
    KenwoodData := '';

    CASE Radio OF
        RadioOne: ControlPort  := Radio1ControlPort;
        RadioTwo: ControlPort  := Radio2ControlPort;
        END;

    REPEAT
        TimeOut := 0;

        REPEAT
            Inc (TimeOut);

            IF TimeOut > KenwoodResponseTimeout THEN
                BEGIN
                IF RadioDebugMode THEN
                    BEGIN
                    FOR CharPointer := 1 TO SizeOf (DebugString) - 1 DO
                        DebugString [CharPointer] := Chr (0);

                    DebugString := 'KENWOOD TIMEOUT';
                    BlockWrite (RadioDebugWrite, DebugString, SizeOf (DebugString), Resultx);
                    END;
                CASE Radio OF
                    RadioOne:
                        LastRadioOneMessageRead := '';

                    RadioTwo:
                        LastRadioTwoMessageRead := '';
                    END;

                Exit;
                END;

            IF KeyPressed OR Footsw.getState THEN
                BEGIN
                IF RadioDebugMode THEN
                    BEGIN
                    FOR CharPointer := 1 TO SizeOf (DebugString) - 1 DO
                        DebugString [CharPointer] := Chr (0);

                    DebugString := 'KEY OR FOOTSWITCH PRESSED';
                    BlockWrite (RadioDebugWrite, DebugString, SizeOf (DebugString), Resultx);
                    END;

                Exit;
                END;

//            IF ControlPort.CharReady THEN Break;

//            Wait (1);
        millisleep;

        UNTIL ControlPort.CharReady;

        { Read in the data }

        KenwoodData := KenwoodData + ControlPort.ReadChar;

        { See if we have gotten all the way to the end of it }

        IF KenwoodData [Length (KenwoodData)] = ';' THEN
            BEGIN
            GetKenwoodResponse := True;
            Exit;
            END;

    UNTIL Length (KenwoodData) > 160;

    { We didn't find what we were looking for }

    END;



PROCEDURE AddRadioCommandString (Radio: RadioType; Data: Str80);

VAR CharPointer: INTEGER;

    BEGIN
    IF Data <> '' THEN
        FOR CharPointer := 1 TO Length (Data) DO
            AddRadioCommandCharacter (Radio, Data [CharPointer]);
    END;




PROCEDURE PutRadioIntoSplit (Radio: RadioType);

VAR IntRadioType: InterfacedRadioType;

    BEGIN
    CASE Radio OF

        RadioOne: BEGIN
                  IntRadioType := Radio1Type;
                  END;

        RadioTwo: BEGIN
                  IntRadioType := Radio2Type;
                  END;
        END;

    CASE IntRadioType OF

        TS850, K2:
            BEGIN
            AddRadioCommandCharacter (Radio, 'F');
            AddRadioCommandCharacter (Radio, 'R');
            AddRadioCommandCharacter (Radio, '0');
            AddRadioCommandCharacter (Radio, ';');

            AddRadioCommandCharacter (Radio, 'F'); {KK1L: 6.71 For some reason needed this to get the next   }
            AddRadioCommandCharacter (Radio, 'A'); {           command to take. Started when I added setting }
            AddRadioCommandCharacter (Radio, ';'); {           mode of B VFO to set freq.                    }

            AddRadioCommandCharacter (Radio, 'F');
            AddRadioCommandCharacter (Radio, 'T');
            AddRadioCommandCharacter (Radio, '1');
            AddRadioCommandCharacter (Radio, ';');
            END;

        IC706, IC706II, IC706IIG, IC707, IC725, IC726, IC728, IC729, IC735,
        IC746, IC746PRO, IC756, IC756PRO, IC756PROII,  IC736, IC737, IC738,
        IC761, IC765, IC775, IC781, OMNI6: { KK1L: 6.73 Added OMNI6 }
            BEGIN
            AddRadioCommandCharacter (Radio, Chr ($FE));
            AddRadioCommandCharacter (Radio, Chr ($FE));

            CASE Radio OF
                RadioOne: AddRadioCommandCharacter (Radio, Chr (Radio1ReceiverAddress));
                RadioTwo: AddRadioCommandCharacter (Radio, Chr (Radio2ReceiverAddress));
                END;

            AddRadioCommandCharacter (Radio, Chr ($E0));
            AddRadioCommandCharacter (Radio, Chr ($0F)); { $0F cmd}
            AddRadioCommandCharacter (Radio, Chr ($01)); { $01 put in split}
            AddRadioCommandCharacter (Radio, Chr ($FD));
            END;

        FT100, FT890, FT920, FT990, FT1000, FT1000MP:
            AddRadioCommandString (Radio, Chr (0) + Chr (0) + Chr (0) + Chr (1) + Chr (1));

        JST245:
            BEGIN
            END;

        Orion:
            AddRadioCommandString (Radio, '*KVABB' + CarriageReturn);

        END;
    END;



PROCEDURE PutRadioOutOfSplit (Radio: RadioType);

VAR IntRadioType: InterfacedRadioType;

    BEGIN
    CASE Radio OF

        RadioOne: BEGIN
                  IntRadioType := Radio1Type;
                  END;

        RadioTwo: BEGIN
                  IntRadioType := Radio2Type;
                  END;
        END;

    CASE IntRadioType OF
        TS850, K2:
            BEGIN
            AddRadioCommandCharacter (Radio, 'F');
            AddRadioCommandCharacter (Radio, 'R');
            AddRadioCommandCharacter (Radio, '0');
            AddRadioCommandCharacter (Radio, ';');

            AddRadioCommandCharacter (Radio, 'F');
            AddRadioCommandCharacter (Radio, 'T');
            AddRadioCommandCharacter (Radio, '0'); {KK1L: 6.64 Xmit on A VFO}
            AddRadioCommandCharacter (Radio, ';');
            END;

        IC706, IC706II, IC706IIG, IC707, IC725, IC726, IC728, IC729, IC735,
        IC746, IC746PRO, IC756, IC756PRO, IC756PROII,  IC736, IC737, IC738,
        IC761, IC765, IC775, IC781, OMNI6: {KK1L: 6.73 Added OMNI6}
            BEGIN
            AddRadioCommandCharacter (Radio, Chr ($FE));
            AddRadioCommandCharacter (Radio, Chr ($FE));

            CASE Radio OF
                RadioOne: AddRadioCommandCharacter (Radio, Chr (Radio1ReceiverAddress));
                RadioTwo: AddRadioCommandCharacter (Radio, Chr (Radio2ReceiverAddress));
                END;

            AddRadioCommandCharacter (Radio, Chr ($E0));
            AddRadioCommandCharacter (Radio, Chr ($0F)); { $0F cmd}
            AddRadioCommandCharacter (Radio, Chr ($00)); {KK1L: 6.64 This is the cancel split code}
            AddRadioCommandCharacter (Radio, Chr ($FD));
            END;

        FT100, FT890, FT920, FT990, FT1000, FT1000MP:  { QSL }
            AddRadioCommandString (Radio, Chr (0) + Chr (0) + Chr (0) + Chr (0) + Chr (1));

        JST245:
            BEGIN
            END;

        Orion:
            AddRadioCommandString (Radio, '*KVABA' + CarriageReturn);

        END;
    END;



PROCEDURE OutputBandInfo (Radio: RadioType; Band: BandType);

VAR Image: BYTE;
    data: integer;
{KK1L: Note I can create personized freq range output by calling BandOutputFreqLimits proc (to be created)}
{           Using frequency should be okay, since if no radio connected then can use TR band data}

    BEGIN
    CASE Band OF
        Band160:
        begin
           Image := $01;
           data := 1;
        end;
        Band80:
        begin
           Image := $20;
           data := 2;
        end;
        Band40:
        begin
           Image := $21;
           data := 3;
        end;
        Band30:
        begin
           Image := $40;
           data := 4;
        end;
        Band20:
        begin
           Image := $41;
           data := 5;
        end;
        Band17:
        begin
           Image := $60;
           data := 6;
        end;
        Band15:
        begin
           Image := $61;
           data := 7;
        end;
        Band12:
        begin
           Image := $80;
           data := 8;
        end;
        Band10:
        begin
           Image := $81;
           data := 9;
        end;
        Band6:
        begin
           Image := $A0;
           data := 10;
        end;
        Band2:
        begin
           Image := $A1;
           data := 11;
        end;
        Band222:
        begin
           Image := $C0;
           data := 12;
        end;
        Band432:
        begin
           Image := $C1;
           data := 13;
        end;
        Band902:
        begin
           Image := $E0;
           data := 14;
        end;
        Band1296:
        begin
           Image := $E1;
           data := 15;
        end;
        ELSE      Image := $00;
        END;

    IF Radio = RadioOne THEN
        BEGIN
        IF Band = RadioOneBandOutputStatus THEN Exit;
        RadioOneBandOutputStatus := Band;

        so2rbox.setrig1band(data);
        IF Radio1BandOutputPort <> nil THEN
            Radio1BandOutputPort.writedata($e1,Image);
        END
       
    ELSE
        BEGIN
        IF Band = RadioTwoBandOutputStatus THEN Exit;
        RadioTwoBandOutputStatus := Band;

        so2rbox.setrig2band(data);
        IF Radio2BandOutputPort <> nil THEN
           Radio2BandOutputPort.writedata($e1,Image);
        END;
    END;



PROCEDURE StartDVK (MemorySwitch: INTEGER);

VAR Image: BYTE;

    BEGIN
    IF NOT DVPOn THEN Exit;

    CASE MemorySwitch OF
        0: BEGIN
           IF (Radio1BandOutputPort <> ActiveDVKPort) AND
              (Radio2BandOutputPort <> ActiveDVKPort) THEN
                  Image :=  $1      { Abort message }
              ELSE
                  Exit;
           END;

        1: Image :=  $2;      { Memory one   }
        2: Image :=  $4;      { Memory two   }
        3: Image :=  $8;      { Memory three }
        4: Image := $10;      { Memory four  }
        {5: IF EnableSixDVKMessages THEN Image := $20 ELSE Exit;} {KK1L: 6.71}{KK1L: 6.72 added IF}{KK1L: 6.73}
        {6: IF EnableSixDVKMessages THEN Image := $40 ELSE Exit;} {KK1L: 6.71}{KK1L: 6.72 added IF}{KK1L: 6.73}
        5: Image := $20;  { Memory five  } {KK1L: 6.73}
        6: Image := $40;  { Memory six   } {KK1L: 6.73}
        ELSE Exit;
        END;

    if ActiveDVKPort <> nil then ActiveDVKPORT.writedata($7f,Image);

    DVKTimeOut := 40;
    END;



{KK1L: 6.71 Started coding for the stereo pin stuff}

PROCEDURE SetStereoPin (PinNumber: INTEGER; PinSet: BOOLEAN);

VAR Image: BYTE;

    BEGIN
    if ActiveStereoPort = nil then exit;

    CASE PinNumber OF
        5: Image :=  $08;      { Pin five, bit 3 }
        9: Image :=  $80;      { Pin nine, bit 7 }
        ELSE Exit;
        END;

    if ActiveStereoPort = nil then exit;
    If PinSet then
       ActiveStereoPort.writedata(Image,Image)
    else
       ActiveStereoPort.writedata(Image,$00);

    END;

PROCEDURE DVKEnableWrite;

{ This procedure will turn on the fifth bit of the third LPT port which
  will set the DVK up to be written into. }

    BEGIN
    if ActiveDVKPort <> nil then ActiveDVKPort.writedata($20,$20);
    END;


PROCEDURE DVKDisableWrite;

{ This procedure will stop the recording process on the DVK. }

    BEGIN
    if ActiveDVKPort <> nil then ActiveDVKPort.writedata($20,$00);
    END;


PROCEDURE SetRelayForActiveRadio (Radio: RadioType);

    BEGIN
    if RelayControlPort = nil then exit;
    IF SwapRadioRelaySense THEN
        IF Radio = RadioOne THEN
            Radio := RadioTwo
        ELSE
            Radio := RadioOne;

    CASE Radio OF
        RadioOne:
           RelayControlPort.relayhigh(true);

        RadioTwo:
           RelayControlPort.relayhigh(false);
        END;

    Wait (2);
    END;


PROCEDURE SendMultiMessage (Message: STRING);

{ Works for both N6TR and K1EA Network modes }

VAR CharPointer: INTEGER;

    BEGIN
    IF Length (Message) > 0 THEN
        BEGIN

        IF K1EANetworkEnable THEN
            BEGIN

            { We add the checksum and new line unless we already find
              the new line there }

            IF Message [Length (Message)] <> LineFeed THEN
                BEGIN
                AddK1EACheckSumToString (Message);
                Message := Message + LineFeed;
                END;
            END
        ELSE
            Message := SlipMessage (Message);

        { If we don't have enough room for the message, we will have to
          wait until we do as we have no other choice.  }

        IF NetDebug THEN
            BEGIN
            IF MultiSendCharBuffer.FreeSpace < Length (Message) THEN
                SendMorse ('PBF PBF');

            SaveAndSetActiveWindow (BandMapWindow);
            GoToXY (1, 22);
            ClrEol;
            END;

        WHILE NOT MultiSendCharBuffer.FreeSpace >= Length (Message) DO;

        FOR CharPointer := 1 TO Length (Message) DO
            BEGIN
            MultiSendCharBuffer.AddEntry (Ord (Message [CharPointer]));

            IF NetDebug THEN
                BEGIN
                BlockWrite (NetDebugBinaryOutput, Message [CharPointer], 1);

                IF (Message [CharPointer] >= ' ') AND
                   (Message [CharPointer] <= 'z') THEN
                       Write (Message [CharPointer]);
                END;
            END;

        IF NetDebug THEN RestorePreviousWindow;
        END;
    END;


PROCEDURE SendPacketMessage (Message: STRING);

VAR CharPointer: INTEGER;

    BEGIN
    IF Length (Message) > 0 THEN
        BEGIN

        { This is ugly!! }

        WHILE NOT PacketSendCharBuffer.FreeSpace >= Length (Message) DO;

        FOR CharPointer := 1 TO Length (Message) DO
            PacketSendCharBuffer.AddEntry (Ord (Message [CharPointer]));
        END;
    END;



PROCEDURE SendRotatorMessage (Message: STRING);

VAR CharPointer: INTEGER;

    BEGIN
    IF Length (Message) > 0 THEN
        BEGIN

        { This is ugly!! }

        WHILE NOT RotatorSendCharBuffer.FreeSpace >= Length (Message) DO;

        FOR CharPointer := 1 TO Length (Message) DO
            RotatorSendCharBuffer.AddEntry (Ord (Message [CharPointer]));
        END;
    END;


PROCEDURE CheckRadio1ControlPort;

    BEGIN
    IF Radio1CommandDelay > 0 THEN
        BEGIN
        Dec (Radio1CommandDelay);
        Exit;
        END;

    IF Radio1Delay > 0 THEN
        BEGIN
        Dec (Radio1Delay);
        Exit;
        END;

    IF (Radio1CommandBufferStart = Radio1CommandBufferEnd) or
       (Radio1ControlPort = Nil) THEN Exit;
//if not radio1controlport.charready then exit;
//seems wrong -- transmit buffer empty makes more sense
    radio1controlport.putchar(Radio1CommandBuffer [Radio1CommandBufferStart]);

    Inc (Radio1CommandBufferStart);
    Radio1CommandBufferStart := Radio1CommandBufferStart MOD RadioCommandBufferSize;

    Radio1Delay := Radio1ControlDelay;
    END;



PROCEDURE CheckRadio2ControlPort;

    BEGIN
    IF Radio2CommandDelay > 0 THEN
        BEGIN
        Dec (Radio2CommandDelay);
        Exit;
        END;


    IF Radio2Delay > 0 THEN
        BEGIN
        Dec (Radio2Delay);
        Exit;
        END;

    IF (Radio2CommandBufferStart = Radio2CommandBufferEnd) or
       (Radio2ControlPort = Nil) THEN Exit;

    radio2controlport.putchar(Radio2CommandBuffer [Radio2CommandBufferStart]);

    Inc (Radio2CommandBufferStart);
    Radio2CommandBufferStart := Radio2CommandBufferStart MOD RadioCommandBufferSize;

    Radio2Delay := Radio2ControlDelay;
    END;



FUNCTION RadioSendBufferEmpty (Radio: RadioType): BOOLEAN;

    BEGIN
    CASE Radio OF
        RadioOne:
            RadioSendBufferEmpty := Radio1CommandBufferStart = Radio1CommandBufferEnd;

        RadioTwo:
            RadioSendBufferEmpty := Radio2CommandBufferStart = Radio2CommandBufferEnd;

        END;
    END;



PROCEDURE AddRadioCommandCharacter (Radio: RadioType; Character: CHAR);

    BEGIN
    IF TalkDebugMode THEN BlockWrite (TalkDebugWrite, Character, 1);

    CASE Radio OF
        RadioOne:
            BEGIN
            Radio1CommandBuffer [Radio1CommandBufferEnd] := Character;
            Inc (Radio1CommandBufferEnd);
            Radio1CommandBufferEnd := Radio1CommandBufferEnd MOD RadioCommandBufferSize;
            END;

        RadioTwo:
            BEGIN
            Radio2CommandBuffer [Radio2CommandBufferEnd] := Character;
            Inc (Radio2CommandBufferEnd);
            Radio2CommandBufferEnd := Radio2CommandBufferEnd MOD RadioCommandBufferSize;
            END;

        END;
    END;


PROCEDURE SetRadioFreq (Radio: RadioType; Freq: LONGINT; Mode: ModeType; VFO: Char);

 { Freq is in hertz - without frequency adder removed yet. }

VAR FreqStr: Str20;
    CharPointer: INTEGER;
    SendByte, TempByte: BYTE;
    IntRadioType: InterfacedRadioType;

    BEGIN
    CASE Radio OF

        RadioOne: BEGIN
                  Freq := Freq - Radio1FrequencyAdder;

                  IntRadioType     := Radio1Type;
                  StableRadio1Freq := Freq;
                  LastRadioOneFreq := Freq;
                  Radio1PollDelay  := 0;     { Skips the next poll? }
                  END;

        RadioTwo: BEGIN
                  Freq := Freq - Radio2FrequencyAdder;

                  IntRadioType     := Radio2Type;
                  StableRadio2Freq := Freq;
                  LastRadioTwoFreq := Freq;
                  Radio2PollDelay  := 0;     { Skips the next poll? }
                  END;
        END;

    CASE IntRadioType OF

        TS850, K2:
            BEGIN
            Str (Freq, FreqStr);
            WHILE Length (FreqStr) < 11 DO FreqStr := '0' + FreqStr;

            AddRadioCommandString (Radio, 'F' + VFO + FreqStr + ';');

            {KK1L: 6.71 To set mode of B VFO on TS850 need to make it active first}

            IF VFO = 'B' THEN
                AddRadioCommandString (Radio, 'FR1;FT1;');

            CASE Mode OF
                CW:      AddRadioCommandString (Radio, 'MD3;');
                Digital: AddRadioCommandString (Radio, 'MD6;');

                ELSE { SSB }
                    IF Freq < 10000000 THEN
                        AddRadioCommandString (Radio, 'MD1;')  { LSB }
                    ELSE
                        AddRadioCommandString (Radio, 'MD2;'); { USB }
                END;

            {KK1L: 6.71 To set mode of B VFO on TS850 made it active first and now reset to A}
            {           Must assume A is the primary VFO. To be otherwise must set after set freq call}

            IF (VFO = 'B') THEN
                AddRadioCommandString (Radio, 'FA;FR0;FT0;');

            {KK1L: 6.71 For some reason I need to poll the radio in order to
                   have the FR and FT commands work!?! }

            END;

        IC706, IC706II, IC706IIG, IC707, IC725, IC726, IC728, IC729, IC735,
        IC736, IC746, IC746PRO, IC756, IC756PRO, IC756PROII,  IC737, IC738,
        IC761, IC765, IC775, IC781, OMNI6: {KK1L: 6.73 Added OMNI6}
            BEGIN
            IF VFO = 'B' THEN
                BEGIN
                AddRadioCommandCharacter (Radio, Chr ($FE));
                AddRadioCommandCharacter (Radio, Chr ($FE));

                CASE Radio OF
                    RadioOne: AddRadioCommandCharacter (Radio, Chr (Radio1ReceiverAddress));
                    RadioTwo: AddRadioCommandCharacter (Radio, Chr (Radio2ReceiverAddress));
                    END;

                AddRadioCommandCharacter (Radio, Chr ($E0));
                AddRadioCommandCharacter (Radio, Chr ($07));
                AddRadioCommandCharacter (Radio, Chr ($01));  { Select B VFO }
                AddRadioCommandCharacter (Radio, Chr ($FD));

                Wait (IcomCommandPause);  { KK1L: 6.71 Look for response rather than just wait!}
                END;

            { Send the mode information }

            AddRadioCommandCharacter (Radio, Chr ($FE));
            AddRadioCommandCharacter (Radio, Chr ($FE));

            CASE Radio OF
                RadioOne: AddRadioCommandCharacter (Radio, Chr (Radio1ReceiverAddress));
                RadioTwo: AddRadioCommandCharacter (Radio, Chr (Radio2ReceiverAddress));
                END;

            AddRadioCommandCharacter (Radio, Chr ($E0));
            AddRadioCommandCharacter (Radio, Chr ($06));

            IF Mode = CW THEN
                AddRadioCommandCharacter (Radio, Chr (3))
            ELSE
                IF Mode = Digital THEN
                    AddRadioCommandCharacter (Radio, Chr (4))
                ELSE                                               {must be phone}
                    IF Freq < 10000000 THEN
                        AddRadioCommandCharacter (Radio, Chr (0))  {LSB for less than 10MHz}
                    ELSE
                        AddRadioCommandCharacter (Radio, Chr (1)); {USB otherwise}

                IF ((IntRadioType = IC765) OR (IntRadioType = IC736)) AND (Mode = CW) THEN
                    BEGIN
                    IF Radio = RadioOne THEN
                        AddRadioCommandCharacter (Radio, Chr (Radio1IcomFilterByte))
                    ELSE
                        AddRadioCommandCharacter (Radio, Chr (Radio2IcomFilterByte));
                    END;

                AddRadioCommandCharacter (Radio, Chr ($FD));

            Wait (IcomCommandPause);  { KK1L: 6.71 Look for response rather than just wait!}

            { Send the frequency information }

            AddRadioCommandCharacter (Radio, Chr ($FE));
            AddRadioCommandCharacter (Radio, Chr ($FE));

            CASE Radio OF
                RadioOne: AddRadioCommandCharacter (Radio, Chr (Radio1ReceiverAddress));
                RadioTwo: AddRadioCommandCharacter (Radio, Chr (Radio2ReceiverAddress));
                END;

            AddRadioCommandCharacter (Radio, Chr ($E0));
            AddRadioCommandCharacter (Radio, Chr ($05));

            Str (Freq, FreqStr);

            WHILE Length (FreqStr) < 8 DO
                FreqStr := '0' + FreqStr;

sendbyte := $00; //KS added -- Check
            FOR CharPointer := Length (FreqStr) DOWNTO 1 DO
                IF Odd (CharPointer) THEN
                    BEGIN
                    TempByte := Ord (FreqStr [CharPointer]) - $30;
                    SendByte := SendByte OR (TempByte Shl 4);
                    AddRadioCommandCharacter (Radio, Chr (SendByte));
                    END
                ELSE
                    SendByte := Ord (FreqStr [CharPointer]) - $30;

            AddRadioCommandCharacter (Radio, Chr ($FD));

            IF VFO = 'B' THEN
                BEGIN
                Wait (IcomCommandPause);  { KK1L: 6.71 Look for response rather than just wait!}

                AddRadioCommandCharacter (Radio, Chr ($FE));
                AddRadioCommandCharacter (Radio, Chr ($FE));

                CASE Radio OF
                    RadioOne: AddRadioCommandCharacter (Radio, Chr (Radio1ReceiverAddress));
                    RadioTwo: AddRadioCommandCharacter (Radio, Chr (Radio2ReceiverAddress));
                    END;

                AddRadioCommandCharacter (Radio, Chr ($E0));
                AddRadioCommandCharacter (Radio, Chr ($07));
                AddRadioCommandCharacter (Radio, Chr ($00));  { Select A VFO }
                AddRadioCommandCharacter (Radio, Chr ($FD));
                END;
            END;

        FT890, FT990:
            BEGIN
            IF VFO = 'B' THEN  { Select VFO B on the radio }
                BEGIN
                AddRadioCommandCharacter (Radio, Chr ($0));
                AddRadioCommandCharacter (Radio, Chr ($0));
                AddRadioCommandCharacter (Radio, Chr ($0));
                AddRadioCommandCharacter (Radio, Chr ($1));
                AddRadioCommandCharacter (Radio, Chr ($5));
                END;

            Str (Freq, FreqStr);
            Delete (FreqStr, Length (FreqStr), 1);

            WHILE Length (FreqStr) < 8 DO
                FreqStr := '0' + FreqStr;

            FOR CharPointer := Length (FreqStr) DOWNTO 1 DO
                IF Odd (CharPointer) THEN
                    BEGIN
                    TempByte := Ord (FreqStr [CharPointer]) - $30;
                    SendByte := SendByte OR (TempByte Shl 4);
                    AddRadioCommandCharacter (Radio, Chr (SendByte));
                    END
                ELSE
                    SendByte := Ord (FreqStr [CharPointer]) - $30;

            AddRadioCommandCharacter (Radio, Chr ($0A));

            { Now send 5 bytes for the mode }

            AddRadioCommandCharacter (Radio, Chr ($0));
            AddRadioCommandCharacter (Radio, Chr ($0));
            AddRadioCommandCharacter (Radio, Chr ($0));

            IF Mode = Digital THEN
                AddRadioCommandCharacter (Radio, Chr ($08))
            ELSE
                IF Mode = CW THEN
                    AddRadioCommandCharacter (Radio, Chr ($03))
                ELSE
                    IF Freq < 10000000 THEN
                        AddRadioCommandCharacter (Radio, Chr ($00))
                    ELSE
                        AddRadioCommandCharacter (Radio, Chr ($01));

            AddRadioCommandCharacter (Radio, Chr ($0C));

            IF VFO = 'B' THEN  { Put radio back on VFO A }
                BEGIN
                AddRadioCommandCharacter (Radio, Chr ($0));
                AddRadioCommandCharacter (Radio, Chr ($0));
                AddRadioCommandCharacter (Radio, Chr ($0));
                AddRadioCommandCharacter (Radio, Chr ($0));
                AddRadioCommandCharacter (Radio, Chr ($5));
                END;
            END;

        FT847, FT817:
            BEGIN
            IF IntRadioType = FT847 THEN TurnOn847CAT (Radio);

            { First set the mode }

            IF Mode = CW THEN
                AddRadioCommandCharacter (Radio, Chr (2))
            ELSE
                IF Mode = Phone THEN
                    AddRadioCommandCharacter (Radio, Chr (1));

            AddRadioCommandCharacter (Radio, Chr (0));
            AddRadioCommandCharacter (Radio, Chr (0));
            AddRadioCommandCharacter (Radio, Chr (0));
            AddRadioCommandCharacter (Radio, Chr (7));   { Main VFO }

            { Now do the frequency }

            Str (Freq, FreqStr);

            IF Freq < 1000000   THEN FreqStr := '0' + FreqStr;
            IF Freq < 10000000  THEN FreqStr := '0' + FreqStr;
            IF Freq < 100000000 THEN FreqStr := '0' + FreqStr;

            WHILE Length (FreqStr) < 8 DO FreqStr := '0' + FreqStr;

            FOR CharPointer := 1 TO Length (FreqStr) DO
                IF Odd (CharPointer) THEN                 { 1, 3, 5 and 7 }
                    BEGIN
                    TempByte := Ord (FreqStr [CharPointer]) - $30;
                    TempByte := TempByte Shl 4;
                    SendByte := TempByte;
                    SendByte := SendByte AND $F0;
                    END
                ELSE
                    BEGIN
                    TempByte := Ord (FreqStr [CharPointer]) - $30;
                    TempByte := TempByte AND $0F;
                    SendByte := SendByte OR TempByte;
                    AddRadioCommandCharacter (Radio, Chr (SendByte));
                    END;

            AddRadioCommandCharacter (Radio, Chr (1));  { Main VFO }
            IF IntRadioType = FT847 THEN TurnOff847CAT (Radio);
            END;

        FT100, FT920, FT1000, FT1000MP:
            BEGIN
            { First we send the mode to the proper VFO }

            IF (VFO = 'A') OR (IntRadioType = FT100) THEN
                BEGIN             { Now send 5 bytes for the mode to VFO A }

                IF (IntRadioType = FT100) AND (VFO = 'B') THEN
                    BEGIN
                    AddRadioCommandCharacter (Radio, Chr ($0));
                    AddRadioCommandCharacter (Radio, Chr ($0));
                    AddRadioCommandCharacter (Radio, Chr ($0));
                    AddRadioCommandCharacter (Radio, Chr ($1));
                    AddRadioCommandCharacter (Radio, Chr ($5));
                    END;

                AddRadioCommandCharacter (Radio, Chr ($0));
                AddRadioCommandCharacter (Radio, Chr ($0));
                AddRadioCommandCharacter (Radio, Chr ($0));

                IF Mode = Digital THEN
                    BEGIN
                    IF IntRadioType = FT100 THEN
                        AddRadioCommandCharacter (Radio, Chr ($05))
                    ELSE
                        AddRadioCommandCharacter (Radio, Chr ($08));
                    END
                ELSE
                    IF Mode = CW THEN
                        BEGIN
                        IF (IntRadioType = FT1000MP) OR (IntRadioType = FT920) OR (IntRadioType = FT100) THEN
                            BEGIN
                            IF FT1000MPCWReverse THEN
                                AddRadioCommandCharacter (Radio, Chr ($03))
                            ELSE
                                AddRadioCommandCharacter (Radio, Chr ($02));
                            END
                        ELSE
                            AddRadioCommandCharacter (Radio, Chr ($03));
                        END
                    ELSE
                        IF Freq < 10000000 THEN
                            AddRadioCommandCharacter (Radio, Chr ($00))
                        ELSE
                            AddRadioCommandCharacter (Radio, Chr ($01));

                AddRadioCommandCharacter (Radio, Chr ($0C));

                END

            ELSE
                BEGIN             { Now send 5 bytes for the mode to VFO B }
                AddRadioCommandCharacter (Radio, Chr ($0));
                AddRadioCommandCharacter (Radio, Chr ($0));
                AddRadioCommandCharacter (Radio, Chr ($0));

                IF Mode = Digital THEN
                    BEGIN
                    IF IntRadioType = FT100 THEN
                        AddRadioCommandCharacter (Radio, Chr ($85))
                    ELSE
                        AddRadioCommandCharacter (Radio, Chr ($88));
                    END
                ELSE
                    BEGIN
                    IF Mode = CW THEN
                        BEGIN
                        IF (IntRadioType = FT1000MP) OR (IntRadioType = FT920) OR (IntRadioType = FT100) THEN
                            AddRadioCommandCharacter (Radio, Chr ($82))
                        ELSE
                            AddRadioCommandCharacter (Radio, Chr ($83));
                        END
                    ELSE
                        IF Freq < 10000000 THEN
                            AddRadioCommandCharacter (Radio, Chr ($80))
                        ELSE
                            AddRadioCommandCharacter (Radio, Chr ($81));

                    AddRadioCommandCharacter (Radio, Chr ($0C));
                    END;
                END;

            { Send the frequency information }

            Str (Freq, FreqStr);
            Delete (FreqStr, Length (FreqStr), 1);

            WHILE Length (FreqStr) < 8 DO
                FreqStr := '0' + FreqStr;

            FOR CharPointer := Length (FreqStr) DOWNTO 1 DO
                IF Odd (CharPointer) THEN
                    BEGIN
                    TempByte := Ord (FreqStr [CharPointer]) - $30;
                    SendByte := SendByte OR (TempByte Shl 4);
                    AddRadioCommandCharacter (Radio, Chr (SendByte));
                    END
                ELSE
                    SendByte := Ord (FreqStr [CharPointer]) - $30;

            { Finish frequency to proper VFO }

            IF (VFO = 'A') OR (IntRadioType = FT100) THEN
                AddRadioCommandCharacter (Radio, Chr ($0A))  { Finish freq }
            ELSE
                AddRadioCommandCharacter (Radio, Chr ($8A));

            IF (VFO = 'A') OR (IntRadioType = FT100) THEN
                BEGIN
                AddRadioCommandCharacter (Radio, Chr ($0));
                AddRadioCommandCharacter (Radio, Chr ($0));
                AddRadioCommandCharacter (Radio, Chr ($0));
                AddRadioCommandCharacter (Radio, Chr ($0));
                AddRadioCommandCharacter (Radio, Chr ($5));
                END;

            END;


        JST245:
            BEGIN
            AddRadioCommandCharacter (Radio, 'H');  { Put radio in remote }
            AddRadioCommandCharacter (Radio, '1');
            AddRadioCommandCharacter (Radio, CarriageReturn);

            AddRadioCommandCharacter (Radio, 'F');  { Frequency command }

            Str (Freq, FreqStr);
            WHILE Length (FreqStr) < 8 DO FreqStr := '0' + FreqStr;

            FOR CharPointer := 1 TO Length (FreqStr) DO
                AddRadioCommandCharacter (Radio, FreqStr [CharPointer]);

            AddRadioCommandCharacter (Radio, VFO);
            AddRadioCommandCharacter (Radio, CarriageReturn);

            { Send mode }

            AddRadioCommandCharacter (Radio, 'D');

            IF Mode = CW THEN
                AddRadioCommandCharacter (Radio, '1')
            ELSE
                IF Freq < 10000000 THEN
                    AddRadioCommandCharacter (Radio, '3')
                ELSE
                    AddRadioCommandCharacter (Radio, '2');

            AddRadioCommandCharacter (Radio, CarriageReturn);

            AddRadioCommandCharacter (Radio, 'H');  { Put radio in remote }
            AddRadioCommandCharacter (Radio, '0');
            AddRadioCommandCharacter (Radio, CarriageReturn);
            END;

        Orion:
            BEGIN
            Str (Freq, FreqStr);

            AddRadioCommandString (Radio, '*' + VFO + 'F' + FreqStr + CarriageReturn);

            CASE Mode OF
                CW: IF FT1000MPCWReverse THEN
                        AddRadioCommandString (Radio, '*RMM3' + CarriageReturn)
                    ELSE
                        AddRadioCommandString (Radio, '*RMM2' + CarriageReturn);

                Phone:
                    IF Freq < 10000000 THEN
                        AddRadioCommandString (Radio, '*RMM1' + CarriageReturn)
                    ELSE
                        AddRadioCommandString (Radio, '*RMM0' + CarriageReturn);

                END;
            END;
        END;
    END;



PROCEDURE ClearRIT;

VAR IntRadioType: InterfacedRadioType;

    BEGIN
    IF ActiveRadio = RadioOne THEN
        IntRadioType := Radio1Type
    ELSE
        IntRadioType := Radio2Type;

    CASE IntRadioType OF
        TS850, K2:
            BEGIN
            AddRadioCommandCharacter (ActiveRadio, 'R');
            AddRadioCommandCharacter (ActiveRadio, 'C');
            AddRadioCommandCharacter (ActiveRadio, ';');
            END;

        FT890, FT920, FT990, FT1000, FT1000MP:
            BEGIN
            AddRadioCommandCharacter (ActiveRadio, Chr (0));
            AddRadioCommandCharacter (ActiveRadio, Chr (0));
            AddRadioCommandCharacter (ActiveRadio, Chr (0));
            AddRadioCommandCharacter (ActiveRadio, Chr ($FF));
            AddRadioCommandCharacter (ActiveRadio, Chr ($09));
            END;

        IC706, IC706II, IC706IIG, IC707, IC725, IC726, IC728, IC729, IC735,
        IC736, IC737, IC738, IC746, IC746PRO, IC756, IC756PRO, IC756PROII,
        IC761, IC765, IC775, IC781, OMNI6: {KK1L: 6.73 Added OMNI6}
            Exit;

        Orion:
            BEGIN
            AddRadioCommandString (ActiveRadio, '*RMR0' + CarriageReturn);
            Exit;
            END;

        ELSE Exit;
        END;

    { Anyone know why this stuff is here?  I am chicken to remove it }

    AddRadioCommandString (ActiveRadio, Chr (0) + Chr (0) + Chr (0) + Chr (0) + Chr (0) +
                                        Chr (0) + Chr (0) + Chr (0) + Chr (0) + Chr (0) +
                                        Chr (0) + Chr (0) + Chr (0) + Chr (0) + Chr (0) +
                                        Chr (0) + Chr (0) + Chr (0) + Chr (0) + Chr (0) +
                                        Chr (0) + Chr (0) + Chr (0) + Chr (0) + Chr (0) +
                                        Chr (0) + Chr (0) + Chr (0) + Chr (0) + Chr (0) +
                                        Chr (0) + Chr (0) + Chr (0) + Chr (0) + Chr (0));
    END;



PROCEDURE BumpRITUp;

VAR IntRadioType: InterfacedRadioType;

    BEGIN
    IF ActiveRadio = RadioOne THEN
        BEGIN
        IF Radio1CommandBufferStart <> Radio1CommandBufferEnd THEN Exit;
        IntRadioType := Radio1Type;
        END
    ELSE
        BEGIN
        IF Radio2CommandBufferStart <> Radio2CommandBufferEnd THEN Exit;
        IntRadioType := Radio2Type;
        END;

    IF (IntRadioType = TS850) OR (IntRadioType = K2) THEN
        AddRadioCommandString (ActiveRadio, 'RU;');

    IF IntRadioType = Orion THEN
        AddRadioCommandString (ActiveRadio, '*RMR200' + CarriageReturn);
    END;



PROCEDURE BumpRITDown;

VAR IntRadioType: InterfacedRadioType;

    BEGIN
    IF ActiveRadio = RadioOne THEN
        BEGIN
        IF Radio1CommandBufferStart <> Radio1CommandBufferEnd THEN Exit;
        IntRadioType := Radio1Type;
        END
    ELSE
        BEGIN
        IF Radio2CommandBufferStart <> Radio2CommandBufferEnd THEN Exit;
        IntRadioType := Radio2Type;
        END;

    IF (IntRadioType = TS850) OR (IntRadioType = K2) THEN
        AddRadioCommandString (ActiveRadio, 'RD;');

    IF IntRadioType = Orion THEN
        AddRadioCommandString (ActiveRadio, '*RMR-200' + CarriageReturn);
    END;


PROCEDURE BumpVFOUp;

VAR IntRadioType: InterfacedRadioType;

    BEGIN
    IF ActiveRadio = RadioOne THEN
        BEGIN
        IF Radio1CommandBufferStart <> Radio1CommandBufferEnd THEN Exit;
        IntRadioType := Radio1Type;
        END
    ELSE
        BEGIN
        IF Radio2CommandBufferStart <> Radio2CommandBufferEnd THEN Exit;
        IntRadioType := Radio2Type;
        END;

    CASE IntRadioType OF
        TS850, K2:
            BEGIN
            AddRadioCommandCharacter (ActiveRadio, 'U');
            AddRadioCommandCharacter (ActiveRadio, 'P');
            AddRadioCommandCharacter (ActiveRadio, ';');
            END;


        FT890, FT920, FT990, FT1000, FT1000MP:
            BEGIN
            AddRadioCommandCharacter (ActiveRadio, Chr (0));
            AddRadioCommandCharacter (ActiveRadio, Chr (0));
            AddRadioCommandCharacter (ActiveRadio, Chr (0));
            AddRadioCommandCharacter (ActiveRadio, Chr (0));
            AddRadioCommandCharacter (ActiveRadio, Chr ($8E));
            END;

        IC706, IC706II, IC706IIG, IC707, IC725, IC726, IC728, IC729, IC735,
        IC736, IC737, IC738, IC746, IC746PRO, IC756, IC756PRO, IC756PROII,
        IC761, IC765, IC775, IC781, OMNI6:
            Exit;

        Orion:
            AddRadioCommandString (ActiveRadio, '*AS+1' + CarriageReturn);

        END;
    END;



PROCEDURE BumpVFODown;

VAR IntRadioType: InterfacedRadioType;

    BEGIN
    IF ActiveRadio = RadioOne THEN
        BEGIN
        IF Radio1CommandBufferStart <> Radio1CommandBufferEnd THEN Exit;
        IntRadioType := Radio1Type;
        END
    ELSE
        BEGIN
        IF Radio2CommandBufferStart <> Radio2CommandBufferEnd THEN Exit;
        IntRadioType := Radio2Type;
        END;

    CASE IntRadioType OF
        TS850, K2:
            BEGIN
            AddRadioCommandCharacter (ActiveRadio, 'D');
            AddRadioCommandCharacter (ActiveRadio, 'N');
            AddRadioCommandCharacter (ActiveRadio, ';');
            END;

        FT890, FT920, FT990, FT1000, FT1000MP:
            BEGIN
            AddRadioCommandCharacter (ActiveRadio, Chr (0));
            AddRadioCommandCharacter (ActiveRadio, Chr (0));
            AddRadioCommandCharacter (ActiveRadio, Chr (0));
            AddRadioCommandCharacter (ActiveRadio, Chr (1));
            AddRadioCommandCharacter (ActiveRadio, Chr ($8E));
            END;

        IC706, IC706II, IC706IIG, IC707, IC725, IC726, IC728, IC729, IC735,
        IC736, IC737, IC738, IC746, IC746PRO, IC756, IC756PRO, IC756PROII,
        IC761, IC765, IC775, IC781, OMNI6: {KK1L: 6.73 Added OMNI6}
            Exit;

        Orion:
            AddRadioCommandString (ActiveRadio, '*AS-1' + CarriageReturn);

        END;
    END;



FUNCTION GetRadioParameters (Radio: RadioType;
                             RadioInfoString: STRING;
                             Var Freq: LONGINT;
                             Var Band: BandType;
                             Var Mode: ModeType;
                             Polling: BOOLEAN;
                             Debug: BOOLEAN): BOOLEAN;

VAR DebugString: STRING;
    FreqString: Str20;
    CharPointer, FDLocation, Resultx: INTEGER;
    F1, F2, F3: LongInt;
    IntRadioType: InterfacedRadioType;

    BEGIN
    GetRadioParameters := False;

    { Go get a string from the radio that has the frequency and mode
      information.  This string will be in a unique format for each
      different type of radio and we will have to parse the data based
      upon the radio type.  }

    IF RadioInfoString = '' THEN
        RadioInfoString := GetRadioInformation (Radio, Polling);

    { If we get nothing back - we can't parse anything - return FALSE }

    IF RadioInfoString = '' THEN Exit;

    { If the radio debug mode is enabled, save this data to the debug file }

    IF RadioDebugMode THEN
        BEGIN
        FOR CharPointer := 1 TO SizeOf (DebugString) - 1 DO
            DebugString [CharPointer] := Chr (0);

        DebugString := RadioInfoString;
        BlockWrite (RadioDebugWrite, DebugString, SizeOf (DebugString), Resultx);
        END;

    { Set up the IntRadioType variable to the type of radio that is active }

    IF Radio = RadioOne THEN
        IntRadioType := Radio1Type
    ELSE
        IntRadioType := Radio2Type;

    CASE IntRadioType OF
        TS850, K2:
            BEGIN
            Delete (RadioInfoString, 1, 2);
            FreqString := Copy (RadioInfoString, 1, 11);
            Val (FreqString, Freq, Resultx);

            IF Resultx <> 0 THEN Exit;

            IF Radio = RadioOne THEN
                BEGIN
                IF (Abs (LastRadioOneFreq - Freq) > 200000)
                       AND (StableRadio1Freq <> 0) THEN
                   BEGIN
                   LastRadioOneFreq := Freq;
                   Freq := StableRadio1Freq;
                   END
                ELSE
                   BEGIN
                   LastRadioOneFreq := Freq;
                   StableRadio1Freq := Freq;
                   END;

                Freq := Freq + Radio1FrequencyAdder;

                END
            ELSE
                BEGIN
                IF (Abs (LastRadioTwoFreq - Freq) > 200000
                    ) AND (StableRadio2Freq <> 0) THEN
                   BEGIN
                   LastRadioTwoFreq := Freq;
                   Freq := StableRadio2Freq;
                   END
                ELSE
                   BEGIN
                   LastRadioTwoFreq := Freq;
                   StableRadio2Freq := Freq;
                   END;

                Freq := Freq + Radio2FrequencyAdder;
                END;

            CalculateBandMode (Freq, Band, Mode);

            Delete (RadioInfoString, 1, 27);

            CASE RadioInfoString [1] OF
                '1', '2', '4', '5': Mode := Phone;
                ELSE
                    IF RadioInfoString [1] = '6' THEN
                        Mode := Digital
                    ELSE
                        Mode := CW;
                END;

            GetRadioParameters := True;
            END;

        FT847, FT817:
            BEGIN
            F1 := (Ord (RadioInfoString [1]) AND $F0) SHR 4;   { 100s of mhz }
            F1 := F1 * 10;
            F1 := F1 + (Ord (RadioInfoString [1]) AND $0F);    { 10s of mhz}
            F1 := F1 * 10;

            F1 := F1 + ((Ord (RadioInfoString [2]) AND $F0) SHR 4);  { MHz }
            F1 := F1 * 10;
            F1 := F1 + (Ord (RadioInfoString [2]) AND $0F);      { 100 kHz }
            F1 := F1 * 10;

            F1 := F1 + ((Ord (RadioInfoString [3]) AND $F0) SHR 4); { 10 kHz }
            F1 := F1 * 10;
            F1 := F1 + (Ord (RadioInfoString [3]) AND $0F);         { kHz }
            F1 := F1 * 10;

            F1 := F1 + ((Ord (RadioInfoString [4]) AND $F0) SHR 4); { 100 hz }
            F1 := F1 * 10;

            F1 := F1 + (Ord (RadioInfoString [4]) AND $0F);       { 10 hz }
            F1 := F1 * 10;
            Freq := F1;

            IF Radio = RadioOne THEN
                BEGIN
                IF Abs (LastRadioOneFreq - F1) > 200000 THEN
                   BEGIN
                   LastRadioOneFreq := Freq;
                   Freq := StableRadio1Freq;
                   END
                ELSE
                   BEGIN
                   LastRadioOneFreq := Freq;
                   StableRadio1Freq := Freq;
                   END;

                Freq := Freq + Radio1FrequencyAdder;
                END
            ELSE
                BEGIN
                IF Abs (LastRadioTwoFreq - Freq) > 200000 THEN
                    BEGIN
                    LastRadioTwoFreq := Freq;
                    Freq := StableRadio2Freq;
                    END
                ELSE
                    BEGIN
                    LastRadioTwoFreq := Freq;
                    StableRadio2Freq := Freq;
                    END;

                Freq := Freq + Radio2FrequencyAdder;
                END;

            CalculateBandMode (Freq, Band, Mode);

            CASE Ord (RadioInfoString [5]) OF
                0, 1, 04,08, $84, $88: Mode := Phone;
                2, 3, $82, $83: Mode := CW;
                END;

            GetRadioParameters := True;

            Exit;
            END;

        FT890:
            BEGIN
            F1 := Ord (RadioInfoString [2]);
            F1 := F1 * 65536;
            F2 := Ord (RadioInfoString [3]);
            F2 := F2 * 256;

            Freq := F1 + F2 + Ord (RadioInfoString [4]);
            Freq := Freq * 10;

            IF Radio = RadioOne THEN
                BEGIN
                IF Abs (LastRadioOneFreq - Freq) > 200000 THEN
                   BEGIN
                   LastRadioOneFreq := Freq;
                   Freq := StableRadio1Freq;
                   END
                ELSE
                   BEGIN
                   LastRadioOneFreq := Freq;
                   StableRadio1Freq := Freq;
                   END;

                Freq := Freq + Radio1FrequencyAdder;
                END
            ELSE
                BEGIN
                IF Abs (LastRadioTwoFreq - Freq) > 200000 THEN
                   BEGIN
                   LastRadioTwoFreq := Freq;
                   Freq := StableRadio2Freq;
                   END
                ELSE
                   BEGIN
                   LastRadioTwoFreq := Freq;
                   StableRadio2Freq := Freq;
                   END;

                Freq := Freq + Radio2FrequencyAdder;
                END;

            CalculateBandMode (Freq, Band, Mode);

            CASE Ord (RadioInfoString [7]) OF
                2, 5, 6: Mode := CW;
                ELSE     Mode := Phone;
                END;

            GetRadioParameters := True;
            END;

        FT990, FT1000:
            BEGIN
            F1 := Ord (RadioInfoString [2]);
            F1 := F1 * 65536;
            F2 := Ord (RadioInfoString [3]);
            F2 := F2 * 256;

            Freq := F1 + F2 + Ord (RadioInfoString [4]);
            Freq := Freq * 10;

            IF Radio = RadioOne THEN
                BEGIN
                IF Abs (LastRadioOneFreq - Freq) > 200000 THEN
                   BEGIN
                   LastRadioOneFreq := Freq;
                   Freq := StableRadio1Freq;
                   END
                ELSE
                   BEGIN
                   LastRadioOneFreq := Freq;
                   StableRadio1Freq := Freq;
                END;

                Freq := Freq + Radio1FrequencyAdder;
                END
            ELSE
                BEGIN
                IF Abs (LastRadioTwoFreq - Freq) > 200000 THEN
                   BEGIN
                   LastRadioTwoFreq := Freq;
                   Freq := StableRadio2Freq;
                   END
                ELSE
                   BEGIN
                   LastRadioTwoFreq := Freq;
                   StableRadio2Freq := Freq;
                   END;

                Freq := Freq + Radio2FrequencyAdder;
                END;

            CalculateBandMode (Freq, Band, Mode);

            CASE Ord (RadioInfoString [8]) OF
                2, 5, 6: Mode := CW;
                ELSE     Mode := Phone;
                END;

            GetRadioParameters := True;
            END;

        FT100, FT920, FT1000MP:
            BEGIN
            F1 := Ord (RadioInfoString [2]);
            F1 := F1 * 256 * 256 * 256;
            F2 := Ord (RadioInfoString [3]);
            F2 := F2 * 256 * 256;
            F3 := Ord (RadioInfoString [4]);
            F3 := F3 * 256;

            Freq := F1 + F2 + F3 + Ord (RadioInfoString [5]);

            { Frequency corrections }

            IF IntRadioType = FT1000MP THEN Freq := Round (Freq * 0.625);
            IF IntRadioType = FT100    THEN Freq := Round (Freq * 1.25);

            { See if it looks stable }

            IF Radio = RadioOne THEN
                BEGIN
                IF Abs (LastRadioOneFreq - Freq) > 200000 THEN
                   BEGIN
                   LastRadioOneFreq := Freq;
                   Freq := StableRadio1Freq;
                   END
                ELSE
                   BEGIN
                   LastRadioOneFreq := Freq;
                   StableRadio1Freq := Freq;
                   END;

                Freq := Freq + Radio1FrequencyAdder;
                END
            ELSE
                BEGIN
                IF Abs (LastRadioTwoFreq - Freq) > 200000 THEN
                   BEGIN
                   LastRadioTwoFreq := Freq;
                   Freq := StableRadio2Freq;
                   END
                ELSE
                   BEGIN
                   LastRadioTwoFreq := Freq;
                   StableRadio2Freq := Freq;
                   END;

                Freq := Freq + Radio2FrequencyAdder;
                END;

            { Calculate default band/mode }

            CalculateBandMode (Freq, Band, Mode);

            { Look at band/mode information from radio }

            IF IntRadioType = FT1000MP THEN
                CASE (Ord (RadioInfoString [8]) AND $07) OF
                    2, 5, 6: Mode := CW;
                    ELSE     Mode := Phone;
                    END;

            IF IntRadioType = FT100 THEN
                CASE (Ord (RadioInfoString [6]) AND $07) OF
                    2, 3, 5: Mode := CW;
                    ELSE  Mode := Phone;
                    END;

            IF IntRadioType = FT920 THEN
                CASE (Ord (RadioInfoString [8]) AND $07) OF
                    1, 4, 5: Mode := CW;
                    ELSE     Mode := Phone;
                    END;

            GetRadioParameters := True;
            END;

        IC706, IC706II, IC706IIG, IC707, IC725, IC726, IC728, IC729, IC735,
        IC736, IC737, IC738, IC746, IC746PRO, IC756, IC756PRO, IC756PROII,
        IC761, IC765, IC775, IC781, OMNI6: {KK1L: 6.73 Added OMNI6}
            BEGIN
            Delete (RadioInfoString, 1, 5);       { Get rid of address bytes }

            { Check to see if we have 5 or 4 bytes of frequency data.  If
              you find $FD at the 5th byte, you only got 4 }

            IF Ord (RadioInfoString [5]) <> $FD THEN  { Five bytes }
                BEGIN
                F1 := (Ord (RadioInfoString [5]) AND $F0) SHR 4;   { 1000s of mhz }
                F1 := F1 * 10;
                F1 := F1 + (Ord (RadioInfoString [5]) AND $0F);    { 100s of mhz}
                F1 := F1 * 10;
                END
            ELSE
                F1 := 0;

            { Now process the 4 bytes of remaining frequency data }

            F1 := F1 + ((Ord (RadioInfoString [4]) AND $F0) SHR 4); { 10s of mhz }
            F1 := F1 * 10;
            F1 := F1 + (Ord (RadioInfoString [4]) AND $0F);    { 1s of mhz}
            F1 := F1 * 10;

            F1 := F1 + ((Ord (RadioInfoString [3]) AND $F0) SHR 4);
            F1 := F1 * 10;
            F1 := F1 + (Ord (RadioInfoString [3]) AND $0F);
            F1 := F1 * 10;

            F1 := F1 + ((Ord (RadioInfoString [2]) AND $F0) SHR 4);
            F1 := F1 * 10;
            F1 := F1 + (Ord (RadioInfoString [2]) AND $0F);
            F1 := F1 * 10;

            F1 := F1 + ((Ord (RadioInfoString [1]) AND $F0) SHR 4);
            F1 := F1 * 10;

            Freq := F1 + (Ord (RadioInfoString [1]) AND $0F);

            { Add in any frequency adder }

            { Check to see if this frequency is way off from the last
              frequency we got from the radio.  If it is, don't use it,
              but remember it for next time }

            IF Radio = RadioOne THEN
                BEGIN
                IF Abs (LastRadioOneFreq - Freq) > 200000 THEN
                   BEGIN
                   LastRadioOneFreq := Freq;
                   Freq := StableRadio1Freq;
                   END
                ELSE
                   BEGIN
                   LastRadioOneFreq := Freq;
                   StableRadio1Freq := Freq;
                   END;

                Freq := Freq + Radio1FrequencyAdder;
                END
            ELSE
                BEGIN
                IF Abs (LastRadioTwoFreq - Freq) > 200000 THEN
                   BEGIN
                   LastRadioTwoFreq := Freq;
                   Freq := StableRadio2Freq;
                   END
                ELSE
                   BEGIN
                   LastRadioTwoFreq := Freq;
                   StableRadio2Freq := Freq;
                   END;

                Freq := Freq + Radio2FrequencyAdder;
                END;

            { Calculate the band for this and the default mode }

            CalculateBandMode (Freq, Band, Mode);

            { Delete the frequency response }

            FDLocation := Pos (Chr ($FD), RadioInfoString);

            IF FDLocation <> 0 THEN
                Delete (RadioInfoString, 1, FDLocation);

            WHILE (Ord (RadioinfoString [1]) <> $FE) AND (Length (RadioInfoString) > 0) DO
                Delete (RadioInfoString, 1, 1);

            IF RadioInfoString = '' THEN Exit;

            Delete (RadioInfoString, 1, 5);   { Get rid of address bytes }

            CASE Ord (RadioInfoString [1]) OF
                3,7: Mode := CW;      {KK1L: 6.69 added CW-R as CW}
                4,8: Mode := Digital; {KK1L: 6.69 added RTTY-R as Digital}
                ELSE Mode := Phone;
                END;

            IF Radio = RadioOne THEN
                RadioOneIcomFilterByte := Ord (RadioInfoString [2])
            ELSE
                RadioTwoIcomFilterByte := Ord (RadioInfoString [2]);

            GetRadioParameters := True;
            END;

        JST245:
            BEGIN
            FreqString := Copy (RadioInfoString, 5, 8);

            Val (FreqString, Freq, Resultx);

            IF Resultx <> 0 THEN Exit;

            IF Radio = RadioOne THEN
                BEGIN
                IF Abs (LastRadioOneFreq - Freq) > 200000 THEN
                   BEGIN
                   LastRadioOneFreq := Freq;
                   Freq := StableRadio1Freq;
                   END
                ELSE
                   BEGIN
                   LastRadioOneFreq := Freq;
                   StableRadio1Freq := Freq;
                   END;

                Freq := Freq + Radio1FrequencyAdder;
                END
            ELSE
                BEGIN
                IF Abs (LastRadioTwoFreq - Freq) > 200000 THEN
                    BEGIN
                    LastRadioTwoFreq := Freq;
                    Freq := StableRadio2Freq;
                    END
                ELSE
                    BEGIN
                    LastRadioTwoFreq := Freq;
                    StableRadio2Freq := Freq;
                    END;

                Freq := Freq + Radio2FrequencyAdder;
                END;

            CalculateBandMode (Freq, Band, Mode);

            IF RadioInfoString [4] = '1' THEN
                Mode := CW
            ELSE
                Mode := Phone;

            GetRadioParameters := True;
            END;

   { @A 4 bytes <cr> @B 4 bytes <cr> @KV 3 bytes <cr> @RMM byte <cr> }
   {                    1111     1   111 112      2   2222  2    2   }
   { 12 3456     7   89 0123     4   567 890      1   2345  6    7   }

        Orion:
            BEGIN
            IF RadioInfoString [1] <> '@' THEN
                Insert ('@A', RadioInfoString, 1);

            IF RadioInfoString [8] <> '@' THEN
                Insert ('@B', RadioInfoString, 8);

            IF (RadioInfoString [1] = '@') AND
               (RadioInfoString [8] = '@') AND
               (Length (RadioInfoString) = 27) THEN  { Looks good }
                   BEGIN
                   IF RadioInfoString [18] = 'A' THEN { Main RX A VFO }
                       BEGIN
                       F1 := Ord (RadioInfoString [3]);
                       F1 := F1 * 256 * 256 * 256;
                       F2 := Ord (RadioInfoString [4]);
                       F2 := F2 * 256 * 256;
                       F3 := Ord (RadioInfoString [5]);
                       F3 := F3 * 256;

                       Freq := F1 + F2 + F3 + Ord (RadioInfoString [6]);
                       END

                   ELSE  { Assume B VFO on main RX }
                       BEGIN
                       F1 := Ord (RadioInfoString [10]);
                       F1 := F1 * 256 * 256 * 256;
                       F2 := Ord (RadioInfoString [11]);
                       F2 := F2 * 256 * 256;
                       F3 := Ord (RadioInfoString [12]);
                       F3 := F3 * 256;

                       Freq := F1 + F2 + F3 + Ord (RadioInfoString [13]);
                       END;

                   IF Radio = RadioOne THEN
                      BEGIN
                      IF Abs (LastRadioOneFreq - Freq) > 200000 THEN
                         BEGIN
                         LastRadioOneFreq := Freq;
                         Freq := StableRadio1Freq;
                         END
                      ELSE
                         BEGIN
                         LastRadioOneFreq := Freq;
                         StableRadio1Freq := Freq;
                         END;

                      Freq := Freq + Radio1FrequencyAdder;
                      END
                    ELSE
                       BEGIN
                       IF Abs (LastRadioTwoFreq - Freq) > 200000 THEN
                          BEGIN
                          LastRadioTwoFreq := Freq;
                          Freq := StableRadio2Freq;
                          END
                       ELSE
                          BEGIN
                          LastRadioTwoFreq := Freq;
                          StableRadio2Freq := Freq;
                          END;

                    Freq := Freq + Radio2FrequencyAdder;
                    END;

                   CalculateBandMode (Freq, Band, Mode);

                   CASE RadioInfoString [26] OF
                       '2', '3': Mode := CW;
                       '6': Mode := Digital;
                       ELSE Mode := Phone;
                       END;

                   GetRadioParameters := True;
                   END;

            END;
        END;
    END;



FUNCTION GetIcomFreqString (InputString: STRING): Str20;

{ Looks at the string passed to it and sees if it looks like a valid
  frequency entry from the Transceive function exists }

VAR CharPos: INTEGER;

    BEGIN
    GetIcomFreqString := '';

    IF Length (InputString) < 11 THEN Exit;

    CharPos := Length (InputString) + 1;

    REPEAT
        Dec (CharPos);

        IF (InputString [CharPos] = Chr ($FD))      AND
           (InputString [CharPos -  9] = Chr ($FE)) AND
           (InputString [CharPos - 10] = Chr ($FE)) THEN
               BEGIN
               GetIcomFreqString := Copy (InputString, CharPos - 10, 11);
               Exit;
               END;

    UNTIL CharPos < 11;
    END;


FUNCTION GetRadioInformation (Radio: RadioType; Polling: BOOLEAN): STRING;

{ This goes off and gets the radio information string for the active radio.
  The string is crunched by the GetRadioParameters function.  }

VAR LastMessage, TempString: STRING;
    TimeOut: LONGINT;
    IntRadioType: InterfacedRadioType;
    ControlPort: serialportx;
    DebugString: STRING;
    Resultx, CharPointer: INTEGER;
    IcomDataString: STRING; {KK1L: 6.72}
    VFOAFrequencyString, VFOBFrequencyString, ActiveVFOString, ModeString: Str40;
    BEGIN
    GetRadioInformation := '';  { If we exit with this - nothing will happen }

    TempString := '';

    CASE Radio OF
        RadioOne:
            BEGIN
IF RadioDebugMode THEN
BEGIN
   FOR CharPointer := 1 TO SizeOf (DebugString) - 1 DO
      DebugString [CharPointer] := Chr (0);
   DebugString := 'Get Radio1 Information called';
   BlockWrite (RadioDebugWrite, DebugString, SizeOf (DebugString), Resultx);
END;
            IF NoPollDuringPTT AND ActiveKeyer.GetPTTAsserted THEN
                BEGIN
                GetRadioInformation := LastRadioOneMessageRead;
IF RadioDebugMode THEN
BEGIN
   FOR CharPointer := 1 TO SizeOf (DebugString) - 1 DO
      DebugString [CharPointer] := Chr (0);
   DebugString := 'Radio 1 NoPoll and PTT';
   BlockWrite (RadioDebugWrite, DebugString, SizeOf (DebugString), Resultx);
END;
                Exit;
                END;

            IF Radio1PollDelay > 0 THEN
                BEGIN
                Dec (Radio1PollDelay);
IF RadioDebugMode THEN
BEGIN
   FOR CharPointer := 1 TO SizeOf (DebugString) - 1 DO
      DebugString [CharPointer] := Chr (0);
   DebugString := 'Radio1polldelay > 0';
   BlockWrite (RadioDebugWrite, DebugString, SizeOf (DebugString), Resultx);
END;
                GetRadioInformation := LastRadioOneMessageRead;
                Exit;
                END;

            { Were we just here? }

            IF ElaspedSec100 (LastRadioOneMessageTime) < 50 THEN
                BEGIN
IF RadioDebugMode THEN
BEGIN
   FOR CharPointer := 1 TO SizeOf (DebugString) - 1 DO
      DebugString [CharPointer] := Chr (0);
   DebugString := 'Elaspedsec100 problem';
   BlockWrite (RadioDebugWrite, DebugString, SizeOf (DebugString), Resultx);
END;
                GetRadioInformation := LastRadioOneMessageRead;
                Exit;
                END;

            IntRadioType := Radio1Type;
            ControlPort  := Radio1ControlPort;
            LastMessage  := LastRadioOneMessageRead;
            END;

        RadioTwo:
            BEGIN
            IF NoPollDuringPTT AND ActiveKeyer.GetPTTAsserted THEN
                BEGIN
                GetRadioInformation := LastRadioTwoMessageRead;
                Exit;
                END;

            IF Radio2PollDelay > 0 THEN
                BEGIN
                Dec (Radio2PollDelay);
                GetRadioInformation := LastRadioTwoMessageRead;
                Exit;
                END;

            { Were we just here? }

            IF ElaspedSec100 (LastRadioTwoMessageTime) < 50 THEN
                BEGIN
                GetRadioInformation := LastRadioTwoMessageRead;
                Exit;
                END;

            IntRadioType := Radio2Type;
            ControlPort  := Radio2ControlPort;
            LastMessage  := LastRadioTwoMessageRead;
            END;

        END;

    { Make sure the radio isn't sending me any data }
    if controlport = nil then exit;

    CASE GobbleCharactersFromSerialPort (ControlPort, 2048) OF

        GobbleAborted:
            BEGIN
IF RadioDebugMode THEN
BEGIN
   FOR CharPointer := 1 TO SizeOf (DebugString) - 1 DO
      DebugString [CharPointer] := Chr (0);
   DebugString := 'Gobble Aborted';
   BlockWrite (RadioDebugWrite, DebugString, SizeOf (DebugString), Resultx);
END;
            CASE Radio OF
                RadioOne: GetRadioInformation := LastRadioOneMessageRead;
                RadioTwo: GetRadioInformation := LastRadioTwoMessageRead;
                END;

            Exit;
            END;

        GobbledTooMuch:   { What is going on here }
begin
IF RadioDebugMode THEN
BEGIN
   FOR CharPointer := 1 TO SizeOf (DebugString) - 1 DO
      DebugString [CharPointer] := Chr (0);
   DebugString := 'Gobble too much Polling off';
   BlockWrite (RadioDebugWrite, DebugString, SizeOf (DebugString), Resultx);
END;

            CASE Radio OF

                RadioOne: BEGIN
                          GetRadioInformation := '';
                          PollRadioOne := False;
                          QuickDisplay ('RADIO ONE polling disabled.');
                          Tone.DoABeep (Warning);
                          Exit;
                          END;

                RadioTwo: BEGIN
                          GetRadioInformation := '';
                          PollRadioTwo := False;
                          QuickDisplay ('RADIO TWO polling disabled.');
                          Tone.DoABeep (Warning);
                          Exit;
                          END;

                END;
        END;
end;

    { Now - ask the radio for data and collect it into TempString }

    CASE IntRadioType OF

        TS850, K2:
            BEGIN
IF RadioDebugMode THEN
BEGIN
   FOR CharPointer := 1 TO SizeOf (DebugString) - 1 DO
      DebugString [CharPointer] := Chr (0);
   DebugString := 'Sending IF calling getkenwoodresponse ';
   BlockWrite (RadioDebugWrite, DebugString, SizeOf (DebugString), Resultx);
END;
            AddRadioCommandString (Radio, 'IF;');

            IF NOT GetKenwoodResponse (Radio, TempString) THEN
                BEGIN
                GetRadioInformation := LastMessage;
                Exit;
                END;
            END;

        FT847, FT817:
            BEGIN
            IF IntRadioType = FT847 THEN TurnOn847CAT (Radio);

            { Ask the 847 for frequency / mode data }

            AddRadioCommandCharacter (Radio, NullKey);
            AddRadioCommandCharacter (Radio, NullKey);
            AddRadioCommandCharacter (Radio, NullKey);
            AddRadioCommandCharacter (Radio, NullKey);
            AddRadioCommandCharacter (Radio, Chr (3));

            REPEAT
                TimeOut := 0;

                REPEAT
                    Inc (TimeOut);

                    IF TimeOut > YaesuResponseTimeout THEN
                        BEGIN

                        { Return last message we have from this radio }

                        GetRadioInformation := LastMessage;

                        { Clear Last Message so if fail on next attempt, we
                          will get cleared data }

                        CASE Radio OF
                                RadioOne:
                                    LastRadioOneMessageRead := '';

                                RadioTwo:
                                    LastRadioTwoMessageRead := '';

                                END;

                        IF RadioDebugMode THEN
                            BEGIN
                            FOR CharPointer := 1 TO SizeOf (DebugString) - 1 DO
                                DebugString [CharPointer] := Chr (0);

                            DebugString := 'TIME OUT ' + LastMessage;
                            BlockWrite (RadioDebugWrite, DebugString, SizeOf (DebugString), Resultx);
                            END;

                        TurnOff847CAT (Radio);
                        Exit;
                        END;

                    IF NewKeyPressed OR Footsw.getState THEN
                        BEGIN
                        GetRadioInformation := LastMessage;

                        IF RadioDebugMode THEN
                            BEGIN
                            FOR CharPointer := 1 TO SizeOf (DebugString) - 1 DO
                                DebugString [CharPointer] := Chr (0);

                            DebugString := 'KEY PRESSED';
                            BlockWrite (RadioDebugWrite, DebugString, SizeOf (DebugString), Resultx);
                            END;

                        Exit;
                        END;

//                    Wait (1);
          millisleep;
                UNTIL ControlPort.CharReady;

                TempString := TempString + ControlPort.ReadChar;

            UNTIL Length (TempString) = 5;

            IF IntRadioType = FT847 THEN TurnOff847CAT (Radio);
            END;


        FT100, FT890, FT920, FT990, FT1000, FT1000MP:
            BEGIN
            AddRadioCommandString (Radio, Chr (0) + Chr (0) + Chr (0) + Chr (2) + Chr ($10));

            REPEAT
                TimeOut := 0;

                REPEAT
                    Inc (TimeOut);

                    IF TimeOut > YaesuResponseTimeout THEN
                        BEGIN
                        GetRadioInformation := LastMessage;

                        CASE Radio OF
                            RadioOne:
                                LastRadioOneMessageRead := '';

                            RadioTwo:
                                LastRadioTwoMessageRead := '';

                            END;

                        IF RadioDebugMode THEN
                            BEGIN
                            FOR CharPointer := 1 TO SizeOf (DebugString) - 1 DO
                                DebugString [CharPointer] := Chr (0);

                            DebugString := 'TIME OUT ' + LastMessage;
                            BlockWrite (RadioDebugWrite, DebugString, SizeOf (DebugString), Resultx);
                            END;

                        Exit;
                        END;

                    IF NewKeyPressed OR Footsw.getState THEN
                        BEGIN
                        GetRadioInformation := LastMessage;

                        IF RadioDebugMode THEN
                            BEGIN
                            FOR CharPointer := 1 TO SizeOf (DebugString) - 1 DO
                                DebugString [CharPointer] := Chr (0);

                            DebugString := 'KEY PRESSED';
                            BlockWrite (RadioDebugWrite, DebugString, SizeOf (DebugString), Resultx);
                            END;

                        Exit;
                        END;

//                    Wait (1);
                millisleep;
                UNTIL ControlPort.CharReady;

                TempString := TempString + ControlPort.ReadChar;

{ WARNING!!  The following bandaid was added to fix a specific problem.
             I have no explanation why it is necessary.  Remove at your
             own risk!! }

                IF Length (TempString) = 1 THEN
                    IF (Ord (TempString [1]) = $FF) OR (Ord (TempString [1]) = 0) THEN
                        TempString := '';

            UNTIL (Length (TempString) = 16) OR ((IntRadioType = FT920) AND (Length (TempString) = 28));
            END;

        IC706, IC706II, IC706IIG, IC707, IC725, IC726, IC728, IC729, IC735,
        IC736, IC737, IC738, IC746, IC746PRO, IC756, IC756PRO, IC756PROII,
        IC761, IC765, IC775, IC781, OMNI6:
            BEGIN

            { Ask for frequency information }

            AddRadioCommandCharacter (Radio, Chr ($FE));
            AddRadioCommandCharacter (Radio, Chr ($FE));

            CASE Radio OF
                RadioOne: AddRadioCommandCharacter (Radio, Chr (Radio1ReceiverAddress));
                RadioTwo: AddRadioCommandCharacter (Radio, Chr (Radio2ReceiverAddress));
                END;

            AddRadioCommandCharacter (Radio, Chr ($E0));
            AddRadioCommandCharacter (Radio, Chr ($03));
            AddRadioCommandCharacter (Radio, Chr ($FD));

            { The GetIcomResponse routine sucks in the data, ignores the echo
              and returns TRUE if it looks like we got a complete response.
              It can be interrupted if the operator presses a key and will
              return FALSE. }

            IF NOT GetIcomResponse (Radio, IcomDataString) THEN
                BEGIN
                GetRadioInformation := LastMessage;
                Exit;
                END;

            { Remember the frequency response }

            TempString := IcomDataString;

            { Now ask for the mode }

            AddRadioCommandCharacter (Radio, Chr ($FE));
            AddRadioCommandCharacter (Radio, Chr ($FE));

            CASE Radio OF
                RadioOne: AddRadioCommandCharacter (Radio, Chr (Radio1ReceiverAddress));
                RadioTwo: AddRadioCommandCharacter (Radio, Chr (Radio2ReceiverAddress));
                END;

            AddRadioCommandCharacter (Radio, Chr ($E0));
            AddRadioCommandCharacter (Radio, Chr ($04));
            AddRadioCommandCharacter (Radio, Chr ($FD));

            IF NOT GetIcomResponse (Radio, IcomDataString) THEN
                BEGIN
                GetRadioInformation := LastMessage;
                Exit;
                END;

            { Total response is the frequency and the mode }

            TempString := TempString + IcomDataString;
            END;

        Orion:
            BEGIN
            { Ask for VFO A Frequency }

            AddRadioCommandString (Radio, '?A' + CarriageReturn);

            IF NOT GetOrionResponse (Radio, VFOAFrequencyString) THEN
                BEGIN
                GetRadioInformation := LastMessage;
                Exit;
                END;

            { Ask for VFO B frequency }

            AddRadioCommandString (Radio, '?B' + CarriageReturn);

            IF NOT GetOrionResponse (Radio, VFOBFrequencyString) THEN
                BEGIN
                GetRadioInformation := LastMessage;
                Exit;
                END;

            { Ask for active VFO information }

            AddRadioCommandString (Radio, '?KV' + CarriageReturn);

            IF NOT GetOrionResponse (Radio, ActiveVFOString) THEN
                BEGIN
                GetRadioInformation := LastMessage;
                Exit;
                END;

            { Ask for mode }

            AddRadioCommandString (Radio, '?RMM' + CarriageReturn);

            IF NOT GetOrionResponse (Radio, ModeString) THEN
                BEGIN
                GetRadioInformation := LastMessage;
                Exit;
                END;

            { Let GetRadioParameters sort it all out }

            TempString := VFOAFrequencyString + VFOBFrequencyString +
                          ActiveVFOString + ModeString;
            END;

        JST245:
            BEGIN
            { Ask for data }

            AddRadioCommandString (Radio, 'I' + CarriageReturn);

            REPEAT
                TimeOut := 0;

                REPEAT
                    Inc (TimeOut);

                    IF TimeOut > JSTResponseTimeout THEN
                        BEGIN
                        GetRadioInformation := LastMessage;

                        CASE Radio OF
                            RadioOne:
                                LastRadioOneMessageRead := ''; {KK1L: 6.72 Will clear GetRadioInformation if fail next time}

                            RadioTwo:
                                LastRadioTwoMessageRead := ''; {KK1L: 6.72 Will clear GetRadioInformation if fail next time}

                            END;

                        IF RadioDebugMode THEN
                            BEGIN
                            FOR CharPointer := 1 TO SizeOf (DebugString) - 1 DO
                                DebugString [CharPointer] := Chr (0);

                            DebugString := 'TIME OUT ' + LastMessage;
                            BlockWrite (RadioDebugWrite, DebugString, SizeOf (DebugString), Resultx);
                            END;

                        Exit;
                        END;

                    IF NewKeyPressed OR Footsw.getState THEN
                        BEGIN
                        GetRadioInformation := LastMessage;

                        IF RadioDebugMode THEN
                            BEGIN
                            FOR CharPointer := 1 TO SizeOf (DebugString) - 1 DO
                                DebugString [CharPointer] := Chr (0);

                            DebugString := 'KEY PRESSED';
                            BlockWrite (RadioDebugWrite, DebugString, SizeOf (DebugString), Resultx);
                            END;

                        Exit;
                        END;

//                    Wait (1);
                millisleep;
                UNTIL ControlPort.CharReady;

                TempString := TempString + ControlPort.ReadChar;
            UNTIL (TempString [Length (TempString)] = CarriageReturn) OR (Length (TempString) > 160);

            IF Length (TempString) < 14 THEN
                BEGIN
                GetRadioInformation := LastMessage;
                Exit;
                END;

            IF Copy (TempString, 1, 1) <> 'I' THEN
                BEGIN
                GetRadioInformation := LastMessage;
                Exit;
                END;
            END;

        ELSE
            BEGIN
            GetRadioInformation := '';
            Exit;
            END;

        END;

  { Okay - we think we have a valid string to send back up to the routine
    that called us.  }

    GetRadioInformation := TempString;

    CASE Radio OF
        RadioOne:
            BEGIN
            LastRadioOneMessageRead := TempString;
            MarkTime (LastRadioOneMessageTime);
            Radio1CommandDelay := 100;
            END;

        RadioTwo:
            BEGIN
            LastRadioTwoMessageRead := TempString;
            MarkTime (LastRadioTwoMessageTime);
            Radio2CommandDelay := 100;
            END;
        END;
    END;


FUNCTION DVKMessagePlaying: BOOLEAN;

    BEGIN
    DVKMessagePlaying := DVKDelay > 0;
    END;


PROCEDURE SetDVKDelay (Delay: INTEGER);

    BEGIN
    DVKDelay := Round (Delay / 1.68);
    END;


procedure k1eatimer.timer(caughtup: boolean);

{ Welcome to the heart beat of the TR Logging Program.  This interrupt
  will occur every 1.68 milliseconds (every 3.36 milliseconds if running
  in TR SLOW mode).

  The following things happen during this interrupt:

      - Decrement delay count for Wait Routine (if > 0).
      - Check beeper count and state.
      - Check Paddle port inputs.
      - Check Packet port (input and output buffers).
      - Check CW Demod port (input only);
      - Multi port (input and output buffers).
      - DVK pulse time out.
      - Tuning keys (left control and left shift).
      - RIT keys (left and right shift keys).
      - PTT timeout and CW Status.

  This procedure also performs the jump to the old interrupt 08 handler
  when appropriate to keep the system clock ticking correctly.  (ie: every
  32 times (or 16 if TR SLOW)) }


//INTERRUPT;

VAR TempChar:   CHAR;
    TempByte:   BYTE;
    tempint: integer;

 BEGIN
 caughtup := true;

    { Check multi input port real quick - we check it again later. }
if caughtup then
//    IF DoingMulti AND ActiveMultiPort.CharReady THEN
    while DoingMulti AND ActiveMultiPort.CharReady do
        BEGIN
        TempByte := Ord(ActiveMultiPort.ReadChar);
        MultiReceiveCharBuffer.AddEntry (TempByte);
        END;

    { Decrement or increment various counts or timers }

    IF DelayCount > 0 THEN Dec (DelayCount);

    IF BumpCount > 0 THEN Dec (BumpCount);

    IF ReforkCount > 0 THEN Dec (ReforkCount);

    IF DVKDelay   > 0 THEN Dec (DVKDelay);


//    IF CountsSinceLastCW <> 0 THEN Inc (CountsSinceLastCW);
    tempint := ActiveKeyer.GetCountsSinceLastCW;
    IF tempint <> 0 THEN ActiveKeyer.SetCountsSinceLastCW(tempint+1);


    IF FootSwitchMode = Normal THEN ActiveKeyer.LetFootSwitchControlPTT;


    { Check the Beep count and status. }
if caughtup then Tone.Timer;

    { First we check the keyer status }
if caughtup then ActiveKeyer.Timer;

if caughtup then Footsw.timer;

    { Next we check the Packet Port }

if caughtup then
    IF DoingPacket THEN
        BEGIN
//        IF ActivePacketPort.CharReady THEN
          while ActivePacketPort.CharReady do
            BEGIN
            TempChar := ActivePacketPort.ReadChar;
            PacketReceiveCharBuffer.AddEntry (Ord (TempChar) AND $7F);

            IF (TempChar = CarriageReturn) AND PacketAddLF THEN
                PacketReceiveCharBuffer.AddEntry (Ord (LineFeed));
            END;

        IF PacketOutputDelay = 0 THEN
            BEGIN
            IF PacketSendCharBuffer.GetNextByte (TempByte) THEN
                BEGIN
                PacketSendChar (ActivePacketPort, Chr (TempByte));
                PacketOutputDelay := 5;
                END;
            END
        ELSE
            Dec (PacketOutputDelay);

        END;

    { Check the rotator control port }

if caughtup then
    IF DoingRotator THEN
        IF RotatorSendCharBuffer.GetNextByte (TempByte) THEN
            SendChar (ActiveRotatorPort, Chr (TempByte));

    { Now we check the RTTY port }

    IF DoingRTTY THEN
        BEGIN
        IF RTTYCharacterSentDelayCount = 0 THEN
            BEGIN
            IF RTTYSendCharBuffer.GetNextByte (TempByte) THEN
                SendChar (ActiveRTTYPort, Chr(TempByte));

            RTTYCharacterSentDelayCount := RTTYDelayCount;
            END
        ELSE
            Dec (RTTYCharacterSentDelayCount);

        IF ActiveRTTYPort.CharReady THEN
            BEGIN
            TempByte := Ord(ActiveRTTYPort.ReadChar);
            RTTYReceiveCharBuffer.AddEntry (TempByte);
            END;
        END;

    { Now we check the multi port and modem ports }

if caughtup then
    IF DoingMulti AND NOT DoingModem THEN  { Old routine - not touched }
        BEGIN
        IF MultiCharacterSentDelayCount = 0 THEN
            BEGIN
//            IF MultiSendCharBuffer.GetNextByte (TempByte) THEN
            while MultiSendCharBuffer.GetNextByte (TempByte) do
                ActiveMultiPort.putchar(Char(TempByte));

            MultiCharacterSentDelayCount := MultiDelayCount;
            END
        ELSE
            Dec (MultiCharacterSentDelayCount);

//        IF ActiveMultiPort.CharReady THEN
        while ActiveMultiPort.CharReady do
            BEGIN
            TempByte := Ord(ActiveMultiPort.ReadChar);
            MultiReceiveCharBuffer.AddEntry (TempByte);
            END;
        END

    ELSE
        IF DoingMulti AND DoingModem THEN  { Doing both }
            BEGIN

            { Characters coming in on multi port get put into buffer }

//            IF ActiveMultiPort.CharReady THEN
            while ActiveMultiPort.CharReady do
                BEGIN
                TempByte := Ord(ActiveMultiPort.readchar);
                MultiReceiveCharBuffer.AddEntry (TempByte);
                END;

            { Characters that are in the send character buffer get sent
              to the MODEM }

            IF MultiCharacterSentDelayCount = 0 THEN
                BEGIN
                IF MultiSendCharBuffer.GetNextByte (TempByte) THEN
                    ActiveModemPort.putchar(Char(TempByte));

                MultiCharacterSentDelayCount := MultiDelayCount;
                END
            ELSE
                Dec (MultiCharacterSentDelayCount);

            { Characters from MODEM get sent out to the network - no buffer }

            IF ActiveModemPort.CharReady THEN
                BEGIN
                TempByte := Ord(ActiveModemPort.ReadChar);
                ActiveModemPort.PutChar(Char(TempByte));
                END;

            END
        ELSE
            IF DoingModem THEN  { Not doing Multi }
                BEGIN

                { Characters from modem port get put into multi RX buffer }

                IF ActiveModemPort.CharReady THEN
                    BEGIN
                    TempByte := Ord(ActiveModemPort.ReadChar);
                    MultiReceiveCharBuffer.AddEntry (TempByte);
                    END;

                { Characters that are in the multi send character buffer get
                  sent to the MODEM }

                IF ModemCharacterSentDelayCount = 0 THEN
                    BEGIN
                    IF MultiSendCharBuffer.GetNextByte (TempByte) THEN
                        ActiveModemPort.PutChar(Char(TempByte));

                    ModemCharacterSentDelayCount := ModemDelayCount;
                    END
                ELSE
                    Dec (ModemCharacterSentDelayCount);

                END;

    { Now we check for DVK timeout }

if caughtup then
    IF DoingDVK AND (DVKTimeout > 0) THEN
        BEGIN
        Dec (DVKTimeOut);

        IF (DVKTimeOut = 0) and (ActiveDVKPort <> nil) THEN
           IF (Radio1BandOutputPort <> ActiveDVKPort) AND
               (Radio2BandOutputPort <> ActiveDVKPort) THEN
               ActiveDVKPort.writedata($7f,$00)
           else
               ActiveDVKPort.writedata($7e,$00);
        END;

if caughtup then
    CheckRadio1ControlPort;

if caughtup then
    CheckRadio2ControlPort;

if caughtup then
    begin
       if ReforkCount = 0 then
       begin
          ReforkCount := ReforkDelay;
          if (ActivePacketPort <> Nil) then ActivePacketPort.refork;
          if (ActiveMultiPort <> Nil) then ActiveMultiPort.refork;
       end;
    end;

    IF RitEnable AND (ShiftKeyEnable <> None) AND (BumpCount = 0) THEN
//    IF RitEnable AND ShiftKeyEnable THEN
        BEGIN
        IF CQRITEnabled THEN
            BEGIN
            CASE ritshift OF
                1: BEGIN
                   BumpRITUp;
                   BumpCount := BumpDelay;
                   END;
                2: BEGIN
                   BumpRITDown;
                   BumpCount := BumpDelay;
                   END;
                3: BEGIN
                   ClearRIT;
                   BumpCount := BumpDelay;
                   END;
                END;
            END
        ELSE
            CASE ritshift OF
                1: BEGIN
                   BumpVFOUp;
                   BumpCount := BumpDelay;
                   END;
                2: BEGIN
                   BumpVFODown;
                   BumpCount := BumpDelay;
                   END;
                END;
        END;
    END;

PROCEDURE TimerInit;
BEGIN
    DoingDVK := ActiveDVKPort <> nil;
    IF ActiveMultiPort <> nil THEN
        BEGIN
        DoingMulti := True;
        MultiReceiveCharBuffer.InitializeBuffer;
        MultiSendCharBuffer.InitializeBuffer;

        IF NetDebug THEN
            BEGIN
            Assign  (NetDebugBinaryOutput, 'NETOUT.BIN');
            Rewrite (NetDebugBinaryOutput, 1);

            Assign  (NetDebugBinaryInput,  'NETIN.BIN');
            Rewrite (NetDebugBinaryInput, 1);
            END;
        END;

    IF ActiveModemPort <> nil THEN
        BEGIN
        DoingModem := True;
        END;

    IF ActivePacketPort <> nil THEN
        BEGIN
        DoingPacket := True;
        PacketReceiveCharBuffer.InitializeBuffer;
        PacketSendCharBuffer.InitializeBuffer;
        END;

    IF ActiveRotatorPort <> nil THEN
        BEGIN
        DoingRotator := True;
        RotatorSendCharBuffer.InitializeBuffer;
        END;

    IF ActiveRTTYPort <> nil THEN
        BEGIN
        DoingRTTY := True;

        RTTYReceiveCharBuffer.InitializeBuffer;
        RTTYSendCharBuffer.InitializeBuffer;
        END;


    IF RadioDebugMode THEN
        BEGIN
        Assign  (RadioDebugWrite, 'RADIO.DBG');
        ReWrite (RadioDebugWrite, 1);
        END;

    IF TalkDebugMode THEN
        BEGIN
        Assign  (TalkDebugWrite, 'TALK.DBG');
        ReWrite (TalkDebugWrite, 1);
        END;

    IF (Not Tone.isBeeping) THEN LNoSound;
    IF NOT TimerInitialized THEN
        BEGIN
        timerinitialize;
        BumpCount := 0;
        ModemDelayCount := 0;
        MultiDelayCount := 0;
        RTTYDelayCount  := 0;
        BumpDelay := 50;
        TimerInitialized := true;

        END;
   END;

PROCEDURE K1EAInit;

    BEGIN
    CPUKeyer := K1EAKeyer.create;
    WinKey := WinKeyer.create;
    YcccKey := YcccKeyer.create;
    so2rbox := so2rinterface(YcccKey);
    Footparallel := FootSwitchx.create;
    Footso2r := FootSwitchYcccx.create(so2rbox);
    Footsw := Footparallel;
    Tone := Beeper.create;
    CPUKeyer.SetBeeper(Tone);
    CPUKeyer.SetFootSwitch(Footsw);
    WinKey.SetFootSwitch(Footsw);
    ActiveKeyer := CPUKeyer;

    RITEnable                := True;
    CQRITEnabled              := True;
    ActiveMultiPort   := nil;
    ActivePacketPort  := nil;
    ActiveRotatorPort := nil;
    ActiveRTTYPort    := nil;
    ActiveStereoPort  := nil; {KK1L: 6.72 Forgot this from 6.71}

    DelayCount  := 0;

    DoingModem  := False;
    DoingMulti  := False;
    DoingRTTY   := False;

    DVKTimeOut  := 0;


    LastRadioOneMessageRead := '';
    LastRadioTwoMessageRead := '';

    LastRadioOneMessageTime.Hour   := 0; {KK1L: 6.71}
    LastRadioOneMessageTime.Minute := 0; {KK1L: 6.71}
    LastRadioOneMessageTime.Second := 0; {KK1L: 6.71}
    LastRadioOneMessageTime.Sec100 := 0; {KK1L: 6.71}

    LastRadioTwoMessageTime.Hour   := 0; {KK1L: 6.71}
    LastRadioTwoMessageTime.Minute := 0; {KK1L: 6.71}
    LastRadioTwoMessageTime.Second := 0; {KK1L: 6.71}
    LastRadioTwoMessageTime.Sec100 := 0; {KK1L: 6.71}


    ModemCharacterSentDelayCount := 0;
    MultiCharacterSentDelayCount := 0;

    MultiReceiveCharBuffer.GoAway;
    MultiSendCharBuffer.GoAway;

    PacketReceiveCharBuffer.GoAway;
    PacketSendCharBuffer.GoAway;

    PacketOutputDelay       := 0;

    Radio1BandOutputPort := nil;
    Radio2BandOutputPort := nil;

    Radio1CommandBufferStart := 0;
    Radio1CommandBufferEnd   := 0;
    Radio2CommandBufferStart := 0;
    Radio2CommandBufferEnd   := 0;

    Radio1ControlDelay       := 0;
    Radio2ControlDelay       := 0;
    Radio1ControlPort        := nil;
    Radio2ControlPort        := nil;

    RadioDebugMode           := False;


    RotatorSendCharBuffer.GoAway;

    RTTYReceiveCharBuffer.GoAway;
    RTTYSendCharBuffer.GoAway;

    RTTYCharacterSentDelayCount := 0;


    TalkDebugMode            := False;


    END;

PROCEDURE CloseDebug;
    BEGIN
    IF RadioDebugMode THEN Close (RadioDebugWrite);
    IF TalkDebugMode  THEN Close (TalkDebugWrite);
    END;



PROCEDURE Wait (DelayTimeMs: LONGINT);

VAR Ticks: LONGINT;

    BEGIN
    IF NOT ActiveKeyer.GetKeyerInitialized THEN
        BEGIN
        Delay (DelayTimeMs);
        Exit;
        END;

    Ticks := Round (DelayTimeMs / 1.68);

    IF (Ticks = 0) THEN Ticks := 1;
    DelayCount := Ticks;

    REPEAT MILLISLEEP UNTIL DelayCount = 0;
    END;



    BEGIN
    K1EAInit;

    { Default values }
    DVKDelay := 0;


    RadioOneBandOutputStatus := NoBand;
    RadioTwoBandOutputStatus := NoBand;

    LastRadioOneFreq := 0; {KK1L: 6.71 Used LastRadioOneFreq here instead of LOGWIND.PAS}
    LastRadioTwoFreq := 0; {KK1L: 6.71 Used LastRadioTwoFreq here instead of LOGWIND.PAS}

    StableRadio1Freq := 0; {KK1L: 6.71 Was -1}
    StableRadio2Freq := 0; {KK1L: 6.71 Was -1}

    StereoPinState   := False; {KK1L: 6.71}{KK1L: 6.73 Set to False. STEREO PIN HIGH did not work with default TRUE}
    StereoControlPin := 9;    {KK1L: 6.72}

    Radio1FrequencyAdder := 0;
    Radio2FrequencyAdder := 0;

    CPUKeyer.SetPTTTurnOnDelay(15);
    WinKey.SetPTTTurnOnDelay(15);
    YcccKey.SetPTTTurnOnDelay(15);
    IcomCommandPause       := 300;
    IcomRetries            := 5;   {KK1L: 6.72 Default number of tries to get an Ack from Icoms}
    Radio1IcomFilterByte   := 2;
    Radio2IcomFilterByte   := 2;
    IcomResponseTimeout    := 100; {KK1L: 6.71 Testing shows 100 is the best setting (was 300). Now default.}{KK1L:6.72 100}
    JSTResponseTimeout     := 100;
    KenwoodResponseTimeout := 100; {KK1L: 6.71 Was 25ms. Made 1000ms because when tuning 25ms caused extra queries}
    YaesuResponseTimeout   := 100;
    OrionResponseTimeout   := 100;

    Radio1PollDelay := 0;
    Radio2PollDelay := 0;

    Radio1CommandDelay := 0;
    Radio2CommandDelay := 0;

    PollRadioOne                := True; {KK1L: 6.72 Allows polling to be turned off by radio while still allowing commands}
    PollRadioTwo                := True; {KK1L: 6.72 Allows polling to be turned off by radio while still allowing commands}
    BumpDelay := 10;
    BumpCount := 0;
    ReforkCount := 0;
    ReforkDelay := 600;

    DoingModem   := False;
    DoingMulti   := False;
    DoingPacket  := False;
    DoingRotator := False;
    DoingRTTY    := False;
    TimerInitialized := False;
    k1eatimerx := k1eatimer.create;
    addtimer(@k1eatimerx.timer);

    END.
