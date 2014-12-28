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

UNIT LogK1EA;
{$mode objfpc}

{$V-}

INTERFACE

USES LogGrid, Dos, trCrt, SlowTree, Tree, communication, beep, foot, radio,
   keyerk1ea,keyerwin,keyers,so2r,keyeryccc,footyccc,rig;

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
    DVKControlKeyRecord: boolean;
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
    Radio1CommandDelay:       INTEGER;
    Radio1ControlDelay:       INTEGER;
    Radio1ControlPort:        serialportx;
    Radio1FrequencyAdder:     LONGINT;
    Radio1IcomFilterByte:     BYTE;
    Radio1PollDelay:          INTEGER;
    Radio1Type:               InterfacedRadioType;

    Radio2BandOutputPort:     parallelportx;
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
    rig1,rig2 :radioctl;


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

PROCEDURE K1EAInit;
PROCEDURE OutputBandInfo (Radio: RadioType; Band: BandType);
PROCEDURE PutRadioIntoSplit (Radio: RadioType);
PROCEDURE PutRadioOutOfSplit (Radio: RadioType); {KK1L: 6.64 Added to recover from bandmap split}
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




PROCEDURE PutRadioIntoSplit (Radio: RadioType);
BEGIN
    CASE Radio OF

        RadioOne: rig1.putradiointosplit;
        RadioTwo: rig2.putradiointosplit;
    END;
END;


PROCEDURE PutRadioOutOfSplit (Radio: RadioType);
BEGIN
    CASE Radio OF

        RadioOne: rig1.putradiooutofsplit;
        RadioTwo: rig2.putradiooutofsplit;
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

    if ActiveDVKPort <> nil then
    begin
       if ActiveDVKPort.devname = 'yccc' then
       begin
          yccckey.setaux(3,image and $0f);
          yccckey.setaux(4,(image and $f0) shr 4);
       end
       else
          ActiveDVKPORT.writedata($7f,Image);
    end;

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
    if ActiveDVKPort <>  nil then
       if ActiveDVKPort.devname <> 'yccc' then
          ActiveDVKPort.writedata($20,$20);
    END;


PROCEDURE DVKDisableWrite;

{ This procedure will stop the recording process on the DVK. }

    BEGIN
    if ActiveDVKPort <> nil  then
       if ActiveDVKPort.devname <> 'yccc' then
          ActiveDVKPort.writedata($20,$00);
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


PROCEDURE SetRadioFreq (Radio: RadioType; Freq: LONGINT; Mode: ModeType; VFO: Char);

 { Freq is in hertz - without frequency adder removed yet. }

VAR FreqStr: Str20;
    CharPointer: INTEGER;
    SendByte, TempByte: BYTE;

    BEGIN
    CASE Radio OF

        RadioOne: BEGIN
                  Freq := Freq - Radio1FrequencyAdder;
                  StableRadio1Freq := Freq;
                  LastRadioOneFreq := Freq;
                  rig1.setradiofreq(freq,mode,vfo);
                  END;

        RadioTwo: BEGIN
                  Freq := Freq - Radio2FrequencyAdder;
                  StableRadio2Freq := Freq;
                  LastRadioTwoFreq := Freq;
                  rig2.setradiofreq(freq,mode,vfo);
                  END;
        END;
    END;

PROCEDURE ClearRIT;

    BEGIN
    IF ActiveRadio = RadioOne THEN
        rig1.clearrit
    ELSE
        rig2.clearrit;
    END;


PROCEDURE BumpRITUp;

    BEGIN
    IF ActiveRadio = RadioOne THEN
        rig1.bumpritup
    ELSE
        rig2.bumpritup;
    END;



PROCEDURE BumpRITDown;

    BEGIN
    IF ActiveRadio = RadioOne THEN
        rig1.bumpritdown
    ELSE
        rig2.bumpritdown;
    END;

PROCEDURE BumpVFOUp;

    BEGIN
    IF ActiveRadio = RadioOne THEN
        rig1.bumpvfoup
    ELSE
        rig2.bumpvfoup;
    END;



PROCEDURE BumpVFODown;

    BEGIN
    IF ActiveRadio = RadioOne THEN
        rig1.bumpvfodown
    ELSE
        rig2.bumpvfodown;
    END;


FUNCTION GetRadioParameters (Radio: RadioType;
                             RadioInfoString: STRING;
                             Var Freq: LONGINT;
                             Var Band: BandType;
                             Var Mode: ModeType;
                             Polling: BOOLEAN;
                             Debug: BOOLEAN): BOOLEAN;


    BEGIN

    IF Radio = RadioOne THEN
        rig1.getradioparameters(freq,band,mode)
    ELSE
        rig2.getradioparameters(freq,band,mode);

    GetRadioParameters := True;
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
          while ActivePacketPort.CharReady do
            BEGIN
            TempChar := ActivePacketPort.ReadChar;
//            PacketReceiveCharBuffer.AddEntry (Ord (TempChar) AND $7F);
// Try to get xterm packet port to work better
            PacketReceiveCharBuffer.AddEntry (Ord (TempChar));
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
           if (ActiveDVKPort.devname = 'yccc') then
           begin
              yccckey.setaux(3,0);
              yccckey.setaux(4,0);
           end else
              IF (Radio1BandOutputPort <> ActiveDVKPort) AND
                  (Radio2BandOutputPort <> ActiveDVKPort) THEN
                  ActiveDVKPort.writedata($7f,$00)
              else
                  ActiveDVKPort.writedata($7e,$00);
        END;

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
    DVKControlKeyRecord := true;

    END.
