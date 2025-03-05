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

UNIT LogCW;

{$O+}
{$V-}

INTERFACE

USES Radio, N4OGW, SlowTree, Tree, LogWind, Dos, LogK1EA, trCrt,communication,keycode;

TYPE
     SendBufferType = ARRAY [0..255] OF Char;

     MessagePointer = ^Str80;

     FunctionKeyMemoryArray = ARRAY [RadioType, CW..PHONE, F1..AltF12] OF MessagePointer;

     CWMessageCommandType = (NoCWCommand,
                             CWCommandControlEnter,
                             CWCommandCQMode,
                             CWCommandSAPMode,
                             CWCommandQSY);

VAR
    AutoCQDelayTime: INTEGER;
    AutoCQMemory:    CHAR;

    AutoSidetoneLevel: INTEGER;  { Zero defeats the function }

    CorrectedCallMessageR1:    Str80;
    CorrectedCallMessageR2:    Str80;

    CorrectedCallPhoneMessageR1: Str80;
    CorrectedCallPhoneMessageR2: Str80;

    CQExchangeR1:             Str160;
    CQExchangeR2:             Str160;

    CQExchangeNameKnownR1:    Str160;
    CQExchangeNameKnownR2:    Str160;

    CQPhoneExchangeR1:        Str80;
    CQPhoneExchangeR2:        Str80;

    CQPhoneExchangeNameKnownR1: Str80;
    CQPhoneExchangeNameKnownR2: Str80;

    CWEnable:                 BOOLEAN;
    CWMessageCommand:         CWMessageCommandType;
    CWMessageDone:            BOOLEAN;  { Indicates that if you were sending a message - it is done now }
    CWSpeedFromDataBase:      BOOLEAN;
    CWTone: INTEGER;

    CQMemory:  FunctionKeyMemoryArray;

    DetectedPaddleActivityR1: BOOLEAN;
    DetectedPaddleActivityR2: BOOLEAN;

    EXMemory:  FunctionKeyMemoryArray;

    KeyersSwapped:    BOOLEAN;
    KeyPressedMemory: CHAR;
    KYCWEnable:       BOOLEAN;

    LastRSTSent: STRING [3];
    LeadingZeros: INTEGER;
    LeadingZeroCharacter:  CHAR;

    NeedToSetCQMode:       BOOLEAN; {KK1L: 6.69 This variable is used to leap around some AutoS&PMode code.}

    QSLMessageR1:          Str160;
    QSLMessageR2:          Str160;

    QSLPhoneMessageR1:     Str80;
    QSLPhoneMessageR2:     Str80;

    QSOBeforeMessageR1:    Str160;
    QSOBeforeMessageR2:    Str160;

    QSOBeforePhoneMessageR1: Str80;
    QSOBeforePhoneMessageR2: Str80;

    QuickQSLPhoneMessageR1:  Str80;
    QuickQSLPhoneMessageR2:  Str80;

    QuickQSLMessage1R1:      Str80;
    QuickQSLMessage1R2:      Str80;

    QuickQSLMessage2R1:      Str80;
    QuickQSLMessage2R2:      Str80;

    RememberCWSpeed:         INTEGER;

    RepeatSearchAndPounceExchangeR1: Str80;
    RepeatSearchAndPounceExchangeR2: Str80;

    RepeatSearchAndPouncePhoneExchangeR1: Str80;
    RepeatSearchAndPouncePhoneExchangeR2: Str80;

    RTTYTransmissionStarted: BOOLEAN;

    SearchAndPounceExchangeR1: Str80;
    SearchAndPounceExchangeR2: Str80;

    SearchAndPouncePhoneExchangeR1: Str80;
    SearchAndPouncePhoneExchangeR2: Str80;

    SendingOnRadioOne: BOOLEAN; {KK1L: 6.72 Moved from local (IMPLIMENTATION section) for use in LOGSUBS}
    SendingOnRadioTwo: BOOLEAN; {KK1L: 6.72 Moved from local (IMPLIMENTATION section) for use in LOGSUBS}

    Short0: CHAR;
    Short1: CHAR;
    Short2: CHAR;
    Short9: CHAR;

    TailEndMessageR1: Str80;
    TailEndMessageR2: Str80;

    TailEndPhoneMessageR1: Str80;
    TailEndPhoneMessageR2: Str80;



    PROCEDURE AddStringToBuffer (MSG: Str160; Tone: INTEGER);
    PROCEDURE AppendConfigFile (AddedLine: Str160);

    PROCEDURE ClearPTTForceOn;
    PROCEDURE ContinueRTTYTransmission (MSG: Str160);
    PROCEDURE CWInit;
    FUNCTION  CWStillBeingSent: BOOLEAN;

    FUNCTION  DeleteLastCharacter: BOOLEAN;
    PROCEDURE DVKRecordMessage (MemoryString: Str20);

    PROCEDURE FinishRTTYTransmission (MSG: Str160);
    PROCEDURE FlushCWBufferAndClearPTT;

    PROCEDURE InitializeKeyer;

    FUNCTION  GetCQMemoryString (Radio: RadioType; Mode: ModeType; Key: CHAR): Str80;{KK1L: 6.73 Added mode}
    FUNCTION  GetEXMemoryString (Radio: RadioType; Mode: ModeType; Key: CHAR): Str80;{KK1L: 6.73 Added mode}

    PROCEDURE MemoryProgram (Radio: RadioType; Mode: ModeType);

    PROCEDURE PTTForceOn;

    FUNCTION  QSONumberString (QSONumber: INTEGER): Str80;

    PROCEDURE SendKeyboardInput;
    PROCEDURE SendKeysToRTTY;
    PROCEDURE SendStringAndStop (MSG: Str160);
    PROCEDURE SetCWMonitorLevel (Level: INTEGER);
    PROCEDURE SetSpeed (Speed: INTEGER);
    PROCEDURE SetCQMemoryString (Mode: ModeType; Key: CHAR; MemoryString: Str80);
    PROCEDURE SetEXMemoryString (Mode: ModeType; Key: CHAR; MemoryString: Str80);
    PROCEDURE SetCQMemoryStringRadio (Radio: RadioType; Mode: ModeType; Key: CHAR; MemoryString: Str80);
    PROCEDURE SetEXMemoryStringRadio (Radio: RadioType; Mode: ModeType; Key: CHAR; MemoryString: Str80);
    PROCEDURE SetNewCodeSpeed;
    PROCEDURE SetUpToSendOnActiveRadio;
    PROCEDURE SetUpToSendOnInactiveRadio;
    PROCEDURE StartRTTYTransmission (MSG: Str160);

    PROCEDURE ToggleCW (DisplayPrompt: BOOLEAN);

    PROCEDURE UnInitializeKeyer;

IMPLEMENTATION

USES CfgCmd,timer,so2r,sysutils;

TYPE
     SendData = RECORD
         SendTime:  INTEGER;      { Time in milliseconds }
         SendState: BOOLEAN;      { True for key on }
         END;
TYPE
    KeyStatusType = (NormalKeys, AltKeys, ControlKeys);

VAR KeyStatus: KeyStatusType;
    {SendingOnRadioOne: BOOLEAN;} {KK1L: 6.72 Moved to global (INTERFACE section) for use in LOGSUBS}
    {SendingOnRadioTwo: BOOLEAN;} {KK1L: 6.72 Moved to global (INTERFACE section) for use in LOGSUBS}



PROCEDURE SetCQMemoryString (Mode: ModeType; Key: CHAR; MemoryString: Str80);

    BEGIN
    SetCQMemoryStringRadio (NoRadio, Mode, Key, MemoryString);
    SetCQMemoryStringRadio (RadioOne, Mode, Key, MemoryString);
    SetCQMemoryStringRadio (RadioTwo, Mode, Key, MemoryString);
    END;



PROCEDURE SetEXMemoryString (Mode: ModeType; Key: CHAR; MemoryString: Str80);

    BEGIN
    SetEXMemoryStringRadio (NoRadio, Mode, Key, MemoryString);
    SetEXMemoryStringRadio (RadioOne, Mode, Key, MemoryString);
    SetEXMemoryStringRadio (RadioTwo, Mode, Key, MemoryString);
    END;



procedure WriteLnControl (s: string);

VAR i: integer;

{ Displays control characters with a carrot in front of them and a carriage return }

    BEGIN
    FOR i := 1 to length (s) DO
        IF (Ord (s[i]) < 32) THEN
            Write ('^', char(ord(s[i]) - 1 + ord('a')))
        ELSE
            Write (s [i]);

    WriteLn;
    END;

PROCEDURE ClearPTTForceOn;

    BEGIN
    IF CWEnable THEN ActiveKeyer.PTTUnForce;
    END;


PROCEDURE PTTForceOn;

    BEGIN
    IF CWEnable AND CWEnabled THEN ActiveKeyer.PTTForceOn;
    END;



PROCEDURE AddStringToBuffer (MSG: Str160; Tone: INTEGER);

{ We hope ActiveRadio is correct for N4OGW }

VAR Count: INTEGER;

    BEGIN
    CASE ActiveRadio OF
        RadioOne:
           IF N4OGW_RadioOne_BandMap_IP <> '' THEN
               N4OGW_RadioOne_BandMap.SetTXMode;

        RadioTwo:
           IF N4OGW_RadioTwo_BandMap_IP <> '' THEN
               N4OGW_RadioTwo_BandMap.SetTXMode;

        END;  { of CASE ActiveRadio }

    IF CWEnable AND CWEnabled THEN
        BEGIN
        CASE ActiveRadio OF
            RadioOne:
                IF (AutoSideToneLevel > 0) AND DetectedPaddleActivityR1 THEN
                    BEGIN
                    DetectedPaddleActivityR1 := False;
                    SetCWMonitorLevel (0);
                    FOR Count := 1 TO 50 DO milliSleep;
                    END;

            RadioTwo:
                IF (AutoSideToneLevel > 0) AND DetectedPaddleActivityR2 THEN
                    BEGIN
                    DetectedPaddleActivityR2 := False;
                    SetCWMonitorLevel (0);
                    FOR Count := 1 TO 50 DO milliSleep;
                    END;

            END; { of CASE ActiveRadio }

        ActiveKeyer.AddStringToBuffer (Msg, Tone);
        ActiveKeyer.SetCountsSinceLastCW(0);

        CWMessageDone := False;  { Shows that the message being sent was started here }

        END;
    END;


FUNCTION CWStillBeingSent: BOOLEAN;

    BEGIN
    IF ActiveKeyer.CWStillBeingSent THEN
        BEGIN
        CWStillBeingSent := True;

        CASE ActiveRadio OF

            RadioOne:
                IF (AutoSidetoneLevel > 0) AND CWMessageDone AND NOT DetectedPaddleActivityR1 THEN  { Is this CW being sent from the paddle? }
                    BEGIN
                    SetCWMonitorLevel (AutoSidetoneLevel);
                    DetectedPaddleActivityR1 := True;
                    END;

            RadioTwo:
                IF (AutoSideToneLevel > 0) AND CWMessageDone AND NOT DetectedPaddleActivityR2 THEN  { Is this CW being sent from the paddle? }
                    BEGIN
                    SetCWMonitorLevel (AutoSideToneLevel);
                    DetectedPaddleActivityR2 := True;
                    END;

            END; { of CASE ActiveRadio }

        END
    ELSE
        BEGIN
        CWStillBeingSent := False;
        CWMessageDone := True;

        CASE ActiveRadio OF
            RadioOne:
                IF N4OGW_RadioOne_BandMap_IP <> '' THEN
                    N4OGW_RadioOne_BandMap.SetRXMode;

            RadioTwo:
                IF N4OGW_RadioTwo_BandMap_IP <> '' THEN
                    N4OGW_RadioTwo_BandMap.SetRXMode;

            END;  { of CASE ActiveRadio }
        END;
    END;

FUNCTION DeleteLastCharacter: BOOLEAN;

    BEGIN
    DeleteLastCharacter := ActiveKeyer.DeleteLastCharacter;
    END;



PROCEDURE FlushCWBufferAndClearPTT;

    BEGIN
    ActiveKeyer.FlushCWBuffer;
    ActiveKeyer.PTTUnForce;      { Just in case it was forced on }

    { Legacy stuff }

    if (activerttyport <> nil) and (activemode = Digital) then
        ActiveRttyPort.putchar(chr(27));

    { New RTTY support with K3/K4 }

    IF ActiveMode = Digital THEN
        BEGIN
        IF ActiveRadio = RadioOne THEN
            IF (Radio1Type = K2) OR (Radio1Type = K3) OR (Radio1Type = K4) THEN
                rig1.directcommand ('KY ' + ControlD + ';');

        IF ActiveRadio = RadioTwo THEN
            IF (Radio1Type = K2) OR (Radio1Type = K3) OR (Radio1Type = K4) THEN
                rig2.directcommand ('KY ' + ControlD + ';');
        END;

    { Tell N4OGW that we are going to RX }

    CASE ActiveRadio OF
        RadioOne:
            IF N4OGW_RadioOne_BandMap_IP <> '' THEN
                N4OGW_RadioOne_BandMap.SetRXMode;

        RadioTwo:
            IF N4OGW_RadioTwo_BandMap_IP <> '' THEN
                N4OGW_RadioTwo_BandMap.SetRXMode;

        END;  { of CASE ActiveRadio }
    END;




PROCEDURE SetCWMonitorLevel (Level: INTEGER);

VAR LevelString: Str20;

    BEGIN
    Str (Level, LevelString);
    WHILE Length (LevelString) < 3 DO LevelString := '0' + LevelString;

    IF ActiveRadio = RadioOne THEN
        IF Radio1Type = K3 THEN
            rig1.directcommand ('ML' + LevelString + ';')
        ELSE
            IF Radio1Type = K4 THEN
                rig1.directcommand ('ML0' + LevelString + ';');


    IF ActiveRadio = RadioTwo THEN
        IF Radio2Type = K3 THEN
            rig2.directcommand ('ML' + LevelString + ';')
        ELSE
            IF Radio2Type = K4 THEN
                rig2.directcommand ('ML0' + LevelString + ';');

    END;



PROCEDURE StartRTTYTransmission (MSG: Str160);

VAR CharPointer: INTEGER;

    BEGIN
    { This is legacy stuff - I have no idea if anyone uses it }

    IF (ActiveMode = Digital) AND (ActiveRTTYPort <> nil) AND (NOT KYCWEnable) THEN
        BEGIN
        RTTYTransmissionStarted := True;
        RTTYReceiveCharBuffer.AddEntry (Ord (CarriageReturn));
        RTTYReceiveCharBuffer.AddEntry (Ord (LineFeed));

        WHILE NOT RTTYSendCharBuffer.FreeSpace >= Length (MSG) + 1 DO;

        IF Length (RTTYSendString) > 0 THEN
            FOR CharPointer := 1 TO Length (RTTYSendString) DO
               RTTYSendCharBuffer.AddEntry (Ord (RTTYSendString [CharPointer]));

        IF Length (MSG) > 0 THEN
            FOR CharPointer := 1 TO Length (MSG) DO
               RTTYSendCharBuffer.AddEntry (Ord (MSG [CharPointer]));

        Exit;   { Added so the K3/K4 case doesn't get executed }
        END;

    { New stuff for K3/K4 }

    IF (ActiveMode = Digital) OR KYCWEnable THEN
        BEGIN
        IF ActiveRadio = RadioOne THEN
            IF (Radio1Type = K2) OR (Radio1Type = K3) OR (Radio1Type = K4) THEN
                BEGIN
                rig1.directcommand ('TX;');

                IF MSG <> '' THEN
                    rig1.directcommand ('KY ' + MSG + ';');
                END;

        IF ActiveRadio = RadioTwo THEN
            IF (Radio1Type = K2) OR (Radio1Type = K3) OR (Radio1Type = K4) THEN
                BEGIN
                rig2.directcommand ('TX;');

                IF MSG <> '' THEN
                    rig2.directcommand ('KY ' + MSG + ';');
                END;
        END;

    END;


PROCEDURE ContinueRTTYTransmission (MSG: Str160);

VAR CharPointer: INTEGER;

    BEGIN
    IF (ActiveMode = Digital) AND (ActiveRTTYPort <> nil) AND (NOT KYCWenable) THEN
        BEGIN
        WHILE NOT RTTYSendCharBuffer.FreeSpace >= Length (MSG) DO;

        IF Length (MSG) > 0 THEN
            FOR CharPointer := 1 TO Length (MSG) DO
                RTTYSendCharBuffer.AddEntry (Ord (MSG [CharPointer]));

        Exit;  { Added so K3/K4 code does not get executed }
        END;

    IF (ActiveMode = Digital) OR KYCWEnable THEN
        BEGIN
        IF ActiveRadio = RadioOne THEN
            IF (Radio1Type = K2) OR (Radio1Type = K3) OR (Radio1Type = K4) THEN
                rig1.directcommand ('KYW' + MSG + ';');

        IF ActiveRadio = RadioTwo THEN
            IF (Radio1Type = K2) OR (Radio1Type = K3) OR (Radio1Type = K4) THEN
                rig2.directcommand ('KYW' + MSG + ';');
        END;
    END;


PROCEDURE FinishRTTYTransmission (MSG: Str160);

VAR CharPointer: INTEGER;

    BEGIN
    { Legacy stuff }

    IF (ActiveMode = Digital) AND (ActiveRTTYPort <> nil) AND (NOT KYCWEnable) THEN
        BEGIN
        WHILE NOT RTTYSendCharBuffer.FreeSpace >= Length (MSG) + 1 DO;

        IF Length (MSG) > 0 THEN
            FOR CharPointer := 1 TO Length (MSG) DO
                RTTYSendCharBuffer.AddEntry (Ord (MSG [CharPointer]));

        IF Length (RTTYReceiveString) > 0 THEN
            FOR CharPointer := 1 TO Length (RTTYReceiveString) DO
               RTTYSendCharBuffer.AddEntry (Ord (RTTYReceiveString [CharPointer]));

        Exit;
        END;

    { K3/K4 stuff }

    IF (ActiveMode = Digital) OR KYCWENable THEN
        BEGIN
        IF ActiveRadio = RadioOne THEN
            BEGIN
            IF (Radio1Type = K2) OR (Radio1Type = K3) OR (Radio1Type = K4) THEN
                BEGIN
                WHILE Pos ('_', MSG) > 0 DO
                    MSG [Pos ('_', MSG)] := ' ';

                rig1.directcommand ('KY ' + MSG + '|;');
                END;
            END;

        IF ActiveRadio = RadioTwo THEN
            IF (Radio2Type = K2) OR (Radio2Type = K3) OR (Radio2Type = K4) THEN
                BEGIN
                WHILE Pos ('_', MSG) > 0 DO
                    MSG [Pos ('_', MSG)] := ' ';

                rig2.directcommand ('KY ' + MSG + '|;');
                END;

        END;


    RTTYTransmissionStarted := False;
    END;




PROCEDURE SendStringAndStop (MSG: Str160);

VAR CharPointer: INTEGER;

    BEGIN
    IF ActiveMode = CW THEN
        BEGIN
        CASE ActiveRadio OF
            RadioOne:
                IF N4OGW_RadioOne_BandMap_IP <> '' THEN
                    N4OGW_RadioOne_BandMap.SetTXMode;

            RadioTwo:
                IF N4OGW_RadioTwo_BandMap_IP <> '' THEN
                    N4OGW_RadioTwo_BandMap.SetTXMode;

            END;  { of CASE ActiveRadio }


        IF CWEnable AND CWEnabled THEN
            AddStringToBuffer (MSG, CWTone);  { Use generic procedure please }

        Exit;
        END;

    IF (ActiveMode = Digital) AND (ActiveRTTYPort <> nil) THEN
        BEGIN
        RTTYReceiveCharBuffer.AddEntry (Ord (CarriageReturn));
        RTTYReceiveCharBuffer.AddEntry (Ord (LineFeed));

        WHILE NOT RTTYSendCharBuffer.FreeSpace >= Length (MSG) + 2 DO;

        IF Length (RTTYSendString) > 0 THEN
            FOR CharPointer := 1 TO Length (RTTYSendString) DO
               RTTYSendCharBuffer.AddEntry (Ord (RTTYSendString [CharPointer]));

        FOR CharPointer := 1 TO Length (MSG) DO
            RTTYSendCharBuffer.AddEntry (Ord (MSG [CharPointer]));

        IF Length (RTTYReceiveString) > 0 THEN
            FOR CharPointer := 1 TO Length (RTTYReceiveString) DO
               RTTYSendCharBuffer.AddEntry (Ord (RTTYReceiveString [CharPointer]));

        RTTYReceiveCharBuffer.AddEntry (Ord (CarriageReturn));
        RTTYReceiveCharBuffer.AddEntry (Ord (LineFeed));
        Exit;
        END;

    { K3/K4 stuff }

    IF ActiveMode = Digital THEN
        BEGIN
        IF ActiveRadio = RadioOne THEN
            IF (Radio1Type = K2) OR (Radio1Type = K3) OR (Radio1Type = K4) THEN
                IF MSG <> '' THEN
                    BEGIN
                    WHILE Pos ('_', MSG) > 0 DO
                        MSG [Pos ('_', MSG)] := ' ';

                    rig1.directcommand ('KY ' + MSG + '|;');
                    END;

        IF ActiveRadio = RadioTwo THEN
            IF (Radio1Type = K2) OR (Radio1Type = K3) OR (Radio1Type = K4) THEN
                IF MSG <> '' THEN
                    BEGIN
                    WHILE Pos ('_', MSG) > 0 DO
                        MSG [Pos ('_', MSG)] := ' ';

                    rig2.directcommand ('KY ' + MSG + '|;');
                    END;

        END;
    END;

PROCEDURE SetSpeed (Speed: INTEGER);

    BEGIN
    DisplayedCodeSpeed := Speed;

    IF Speed > 0 THEN
        BEGIN
        CodeSpeed := Speed;
        ActiveKeyer.SetSpeed (Speed);
        END;
    END;


PROCEDURE SetNewCodeSpeed;

{ This procedure will ask what code speed you want to use and set it }

VAR WPM: INTEGER;

    BEGIN
    CWEnabled := True;
    WPM := QuickEditInteger ('Enter WPM code speed : ', 2);
    SetSpeed (WPM);
    DisplayCodeSpeed (CodeSpeed, CWEnabled, False, ActiveMode);
    END;



PROCEDURE DisplayBuffer (Buffer: SendBufferType;
                         BufferStart: INTEGER;
                         BufferEnd: INTEGER);

VAR BufferAddress: INTEGER;

    BEGIN
    ClrScr;

    IF BufferStart = BufferEnd THEN
        BEGIN
        Write ('Buffer empty - type something to start sending or RETURN to stop');
        Exit;
        END;

    BufferAddress := BufferStart;

    WHILE BufferAddress <> BufferEnd DO
        BEGIN
        Write (Buffer [BufferAddress]);
        Inc (BufferAddress);
        IF BufferAddress = 256 THEN BufferAddress := 0;
        END;
    END;



PROCEDURE SendKeysToRTTY;

VAR Key: CHAR;

    BEGIN
    IF (ActiveRTTYPort <> nil) THEN   { Legacy stuff }
        BEGIN
        QuickDisplay ('Keyboard input being sent to RTTY.  Press ESCAPE to stop.');

        ContinueRTTYTransmission (CarriageReturn);
        ContinueRTTYTransmission (LineFeed);

        REPEAT
            REPEAT
                CheckRTTY;
                millisleep;
            UNTIL KeyPressed;

            Key := ReadKey;

            IF Key = EscapeKey THEN
                BEGIN
                QuickDIsplay ('Finished sending manual input to RTTY.');
                FinishRTTYTransmission ('');
                Exit;
                END;

            ContinueRTTYTransmission (Key);
        UNTIL False;
        END;

    { See if we are using an Elecraft radio }

    IF ActiveMode = Digital THEN
        BEGIN
        IF ActiveRadio = RadioOne THEN
            IF (Radio1Type = K2) OR (Radio1Type = K3) OR (Radio1Type = K4) THEN
                rig1.directcommand ('KY ' + ControlD + ';');

        IF ActiveRadio = RadioTwo THEN
            IF (Radio1Type = K2) OR (Radio1Type = K3) OR (Radio1Type = K4) THEN
                rig2.directcommand ('KY ' + ControlD + ';');
        END;

    END;



PROCEDURE SendKeyboardInput;

{ This procedure will take input from the keyboard and send it until a
  return is pressed.                                                    }

VAR Key: CHAR;
    TimeMark: TimeRecord;
    Buffer: SendBufferType;
    BufferStart, BufferEnd: INTEGER;
    so2r_i: so2rinterface;
    so2r_l,latchsave: boolean;

    BEGIN
    BufferStart := 0;
    BufferEnd := 0;
    Buffer[0] := ' '; //to kill buffer not initialized warning

    IF NOT CWEnable THEN Exit;

    SetUpToSendOnActiveRadio;

    CWEnabled := True;
    DisplayCodeSpeed (CodeSpeed, CWEnabled, False, ActiveMode);
    so2r_l := supports(activekeyer,'{85C18D3F-F198-4681-B9D2-03B38213EA94}');
    if so2r_l then
    begin
       so2r_i := activekeyer as so2rinterface;
       latchsave := so2r_i.getlatch;
       so2r_i := activekeyer as so2rinterface;
       so2r_i.setlatch(false);
    end;

    ActiveKeyer.PTTForceOn;

    SaveAndSetActiveWindow (QuickCommandWindow);
    ClrScr;
    Write ('Sending CW from the keyboard.  Use ENTER/Escape/F10 to exit.');

    REPEAT
        MarkTime (TimeMark);

        REPEAT
            IF (ActiveMultiPort <> nil) OR (MultiUDPPort > -1) THEN
                IF ElaspedSec100 (TimeMark) > 3000 THEN  { 30 second timeout }
                    BEGIN
                    FlushCWBufferAndClearPTT;
                    RemoveAndRestorePreviousWindow;
                    if so2r_l then so2r_i.setlatch(latchsave);
                    Exit;
                    END;

        UpdateTimeAndRateDisplays (True, False);

        { Send a character if the buffer is empty in the keyer }

        IF ActiveKeyer.BufferEmpty THEN
            IF BufferStart <> BufferEnd THEN
                BEGIN
                ActiveKeyer.AddCharacterToBuffer (Buffer [BufferStart]);
                Inc (BufferStart);
                IF BufferStart = 256 THEN BufferStart := 0;
                DisplayBuffer (Buffer, BufferStart, BufferEnd);
                END;
millisleep;
        UNTIL NewKeyPressed;
        Key := UpCase (NewReadKey);

        IF Key >= ' ' THEN
            BEGIN
            IF BufferStart = BufferEnd THEN ClrScr;
            Buffer [BufferEnd] := Key;
            Inc (BufferEnd);
            IF BufferEnd = 256 THEN BufferEnd := 0;
            Write (Key);
            END
        ELSE
            CASE Key OF
                CarriageReturn:
                    BEGIN
                    { Send the rest of the characters in the buffer to the keyer }

                    WHILE BufferStart <> BufferEnd DO
                        BEGIN
                        ActiveKeyer.AddCharacterToBuffer (Buffer [BufferStart]);
                        Inc (BufferStart);
                        IF BufferStart = 256 THEN BufferStart := 0;
                        END;

                    { Remove PTT forced on }

                    ActiveKeyer.PTTUnForce;
                    RemoveAndRestorePreviousWindow;

                    { For debug purposes only }

                    if so2r_l then so2r_i.setlatch(latchsave);
                    Exit;
                    END;

                BackSpace:
                    IF BufferEnd <> BufferStart THEN
                        BEGIN
                        Dec (BufferEnd);
                        IF BufferEnd < 0 THEN BufferEnd := 255;
                        DisplayBuffer (Buffer, BufferStart, BufferEnd);
                        END;

                EscapeKey:
                    BEGIN
                    FlushCWBufferAndClearPTT;
                    RemoveAndRestorePreviousWindow;
                    if so2r_l then so2r_i.setlatch(latchsave);
                    Exit;
                    END;

                NullKey:
                    CASE NewReadKey OF
                        F10: BEGIN
                             FlushCWBufferAndClearPTT;
                             RemoveAndRestorePreviousWindow;
                             if so2r_l then so2r_i.setlatch(latchsave);
                             Exit;
                             END;

                        PageUpKey:
                            IF CodeSpeed < 96 THEN
                                BEGIN
                                SetSpeed (CodeSpeed + 3);
                                DisplayCodeSpeed (CodeSpeed, CWEnabled, False, ActiveMode);
                                END;

                        PageDownKey:
                            IF CodeSpeed > 4 THEN
                                BEGIN
                                SetSpeed (CodeSpeed - 3);
                                DisplayCodeSpeed (CodeSpeed, CWEnabled, False, ActiveMode);
                                END;

                        DeleteKey:
                            IF BufferEnd <> BufferStart THEN
                                BEGIN
                                Dec (BufferEnd);
                                IF BufferEnd < 0 THEN BufferEnd := 255;
                                DisplayBuffer (Buffer, BufferStart, BufferEnd);
                                END;

                        END;

                END;

    UNTIL False;
    END;




FUNCTION QSONumberString (QSONumber: INTEGER): Str80;

VAR TempString: Str80;

    BEGIN
    Str (QSONumber, TempString);
    QSONumberString := TempString;
    END;



PROCEDURE DisplayCrypticCWMenu;

    BEGIN
    GoToXY (1, Hi (WindMax) - 5);
    WriteLn ('# QSO number   % database name    ~ GM/GA/GE         : Enable keyboard CW');
    WriteLn ('[ RST prompt   ^ half space       ] repeat RST sent  @ Call window contents');
    WriteLn ('$ GM + name    | received name    \ My callsign      } partial corrected call');
    WriteLn ('^F WPM+2  ^S WPM-2  + AR  < SK  = BT  ! SN  & AS     ) last QSO''s call');
    Write   ('To program control characters, press Control-P first then control character.');
    END;



PROCEDURE DisplayCrypticSSBMenu;

    BEGIN
        IF DVKEnable THEN
            BEGIN
            GoToXY (1, Hi (WindMax) - 4);
            WriteLn ('Alt-W = Write selected message to DVK (DVK1 to DVK4 only');
            WriteLn ('Alt-R = Play selected message from DVK (to transmitter)');
            Write   ('');
            END
        ELSE
            BEGIN
            GoToXY (1, Hi (WindMax) - 4);
            WriteLn ('You have not enabled your DVK.');
            WriteLn ('Set DVK ENABLE to TRUE so you can program messages.');
            Write   ('');
            END;
    END;



PROCEDURE ShowCQFunctionKeyStatus (Radio: RadioType; Mode: ModeType);

VAR Key: CHAR;
    TempString: Str160;

    BEGIN
    { We use CW CQ Function Keys for Digital }

    IF Mode = Digital THEN Mode := CW;

    GoToXY (1, 1);

    CASE KeyStatus OF
        NormalKeys:
            BEGIN
            CASE Radio OF
                NoRadio:  WriteLnCenter ('CQ ' + ModeString [Mode] + ' FUNCTION KEY MEMORY STATUS');
                RadioOne: WriteLnCenter ('RADIO ONE CQ ' + ModeString [Mode] + ' FUNCTION KEY MEMORY STATUS');
                RadioTwo: WriteLnCenter ('RADIO TWO CQ ' + ModeString [Mode] + ' FUNCTION KEY MEMORY STATUS');
                END;

            FOR Key := F1 TO F10 DO
                BEGIN
                Str (Ord (Key) - Ord (F1) + 1, TempString);
                TempString := 'F' + TempString +  ' - ';

                IF GetCQMemoryString (Radio, Mode, Key) <> '' THEN
                    TempString := TempString + GetCQMemoryString (Radio, Mode, Key);

                IF Length (TempString) > 79 THEN
                    TempString := Copy (TempString, 1, 78) + '+';
                ClrEol;

                WriteLnControl (TempString);
                END;
            END;

        AltKeys:
            BEGIN
            CASE Radio OF
                NoRadio:  WriteLnCenter ('ALT-CQ ' + ModeString [Mode] + ' FUNCTION KEY MEMORY STATUS');
                RadioOne: WriteLnCenter ('RADIO ONE ALT-CQ ' + ModeString [Mode] + ' FUNCTION KEY MEMORY STATUS');
                RadioTwo: WriteLnCenter ('RADIO TWO ALT-CQ ' + ModeString [Mode] + ' FUNCTION KEY MEMORY STATUS');
                END;

            FOR Key := AltF1 TO AltF10 DO
                BEGIN
                Str (Ord (Key) - Ord (AltF1) + 1, TempString);
                TempString := 'Alt-F' + TempString +  ' - ';

                IF GetCQMemoryString (Radio, Mode, Key) <> '' THEN {KK1L: 6.73 Added Mode}
                    TempString := TempString + GetCQMemoryString (Radio, Mode, Key); {KK1L: 6.73 Added Mode}

                IF Length (TempString) > 79 THEN
                    TempString := Copy (TempString, 1, 78) + '+';

                ClrEol;
                WriteLnControl(TempString);
                END;
            END;

        ControlKeys:
            BEGIN
            CASE Radio OF
                NoRadio:  WriteLnCenter ('CONTROL-CQ ' + ModeString [Mode] + ' FUNCTION KEY MEMORY STATUS');
                RadioOne: WriteLnCenter ('RADIO ONE CONTROL-CQ ' + ModeString [Mode] + ' FUNCTION KEY MEMORY STATUS');
                RadioTwo: WriteLnCenter ('RADIO TWO CONTROL-CQ ' + ModeString [Mode] + ' FUNCTION KEY MEMORY STATUS');
                END;

            FOR Key := ControlF1 TO ControlF10 DO
                BEGIN
                Str (Ord (Key) - Ord (ControlF1) + 1, TempString);
                TempString := 'Ctrl-F' + TempString +  ' - ';

                IF GetCQMemoryString (Radio, Mode, Key) <> '' THEN {KK1L: 6.73 Added mode}
                    TempString := TempString + GetCQMemoryString (Radio, Mode, Key); {KK1L: 6.73 Added mode}

                IF Length (TempString) > 79 THEN
                    TempString := Copy (TempString, 1, 78) + '+';

                ClrEol;
                WriteLnControl(TempString);
                END;
            END;
        END;
    END;



PROCEDURE ShowExFunctionKeyStatus (Radio: RadioType; Mode: ModeType);

VAR Key: CHAR;
    TempString: Str160;

    BEGIN
    GoToXY (1, 1);

    IF Mode = Digital THEN Mode := CW;

    CASE KeyStatus OF
        NormalKeys:
            BEGIN
            CASE Radio OF
                NoRadio:  WriteLnCenter ('EXCHANGE' + ModeString [Mode] + ' FUNCTION KEY MEMORY STATUS');
                RadioOne: WriteLnCenter ('RADIO ONE EXCHANGE' + ModeString [Mode] + ' FUNCTION KEY MEMORY STATUS');
                RadioTwo: WriteLnCenter ('RADIO TWO EXCHANGE' + ModeString [Mode] + ' FUNCTION KEY MEMORY STATUS');
                END;

            IF ActiveMode = CW THEN
                BEGIN
                WriteLn ('F1 - Set by the MY CALL statement in config file');
                WriteLn ('F2 - Set by S&P EXCHANGE and REPEAT S&P EXCHANGE');

                FOR Key := F3 TO F10 DO
                    BEGIN
                    Str (Ord (Key) - Ord (F1) + 1, TempString);
                    TempString := 'F' + TempString +  ' - ';

                    IF GetEXMemoryString (Radio, CW, Key) <> '' THEN
                        TempString := TempString + GetEXMemoryString (Radio, CW, Key);

                    IF Length (TempString) > 79 THEN
                        TempString := Copy (TempString, 1, 78) + '+';

                    ClrEol;
                    WriteLnControl(TempString);
                    END;
                END
            ELSE            { Phone or Digital }
                FOR Key := F1 TO F10 DO
                    BEGIN
                    Str (Ord (Key) - Ord (F1) + 1, TempString);
                    TempString := 'F' + TempString +  ' - ';

                    IF GetExMemoryString (Radio, Mode, Key) <> '' THEN
                        TempString := TempString + GetExMemoryString (Radio, Mode, Key);

                    IF Length (TempString) > 79 THEN
                        TempString := Copy (TempString, 1, 78) + '+';

                    ClrEol;
                    WriteLnControl (TempString);
                    END;
            END;

        AltKeys:
            BEGIN
            CASE Radio OF
                NoRadio:  WriteLnCenter ('ALT-EXCHANGE' + ModeString [Mode] + ' FUNCTION KEY MEMORY STATUS');
                RadioOne: WriteLnCenter ('RADIO ONE ALT-EXCHANGE' + ModeString [Mode] + ' FUNCTION KEY MEMORY STATUS');
                RadioTwo: WriteLnCenter ('RADIO TWO ALT-EXCHANGE' + ModeString [Mode] + ' FUNCTION KEY MEMORY STATUS');
                END;

            FOR Key := AltF1 TO AltF10 DO
                BEGIN
                Str (Ord (Key) - Ord (AltF1) + 1, TempString);
                TempString := 'Alt-F' + TempString +  ' - ';

                IF GetExMemoryString (Radio, ActiveMode, Key) <> '' THEN
                    TempString := TempString + GetExMemoryString (Radio, Mode, Key);

                IF Length (TempString) > 79 THEN
                    TempString := Copy (TempString, 1, 78) + '+';

                ClrEol;
                WriteLnControl (TempString);
                END;
            END;

        ControlKeys:
            BEGIN
            CASE Radio OF
                NoRadio:  WriteLnCenter ('CONTROL-EXCHANGE' + ModeString [Mode] + ' FUNCTION KEY MEMORY STATUS');
                RadioOne: WriteLnCenter ('RADIO ONE CONTROL-EXCHANGE' + ModeString [Mode] + ' FUNCTION KEY MEMORY STATUS');
                RadioTwo: WriteLnCenter ('RADIO TWO CONTROL-EXCHANGE' + ModeString [Mode] + ' FUNCTION KEY MEMORY STATUS');
                END;

            FOR Key := ControlF1 TO ControlF10 DO
                BEGIN
                Str (Ord (Key) - Ord (ControlF1) + 1, TempString);
                TempString := 'Ctrl-F' + TempString +  ' - ';

                IF GetExMemoryString (Radio, ActiveMode, Key) <> '' THEN
                    TempString := TempString + GetExMemoryString (Radio, Mode, Key);

                IF Length (TempString) > 79 THEN
                    TempString := Copy (TempString, 1, 78) + '+';

                ClrEol;
                WriteLnControl (TempString);
                END;
            END;
        END;  { of CASE KeyStatus }
    END;



PROCEDURE ShowOtherMemoryStatus (Radio: RadioType; Mode: ModeType);

VAR TempString: Str160;

    BEGIN
    CASE RADIO OF
        RadioOne:
            BEGIN
            IF (ActiveMode = CW) OR (ActiveMode = Digital) THEN
                BEGIN
                GoToXY (1, 1);
                WriteLnCenter ('OTHER CW/DIGITAL MESSAGE MEMORY STATUS RADIO ONE');

                ClrEol;
                TempString := ' 1. Call Okay Now - ' + CorrectedCallMessageR1;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl (TempString);

                ClrEol;

                IF ActiveMode = CW THEN
                    BEGIN
                    TempString := ' 2. CQ Exchange   - ' + CQExchangeR1;
                    IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                    WriteLnControl (TempString);
                    END
                ELSE
                    WriteLn ('2. Use Exchange Memory F2 for Digital CQ Exchange');

                ClrEol;
                TempString := ' 3. CQ Ex Name    - ' + CQExchangeNameKnownR1;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                TempString := ' 4. QSL Message   - ' + QSLMessageR1;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                TempString := ' 5. QSO Before    - ' + QSOBeforeMessageR1;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                TempString := ' 6. Quick QSL     - ' + QuickQSLMessage1R1;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                IF ActiveMode = CW THEN
                    BEGIN
                    ClrEol;
                    TempString := ' 7. Repeat S&P Ex - ' + RepeatSearchAndPounceExchangeR1;
                    IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                    WriteLnControl(TempString);
                    END
                ELSE
                    Write ('7. Use Exchange Memory F3 for REPEAT S&P Exchange');

                IF ActiveMode = CW THEN
                    BEGIN
                    ClrEol;
                    TempString := ' 8. S&P Exchange  - ' + SearchAndPounceExchangeR1;
                    IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                    WriteLnControl (TempString);
                    END
                ELSE
                    WriteLn ('8. Use Exchange Memory F2 for S&P Exchange');

                ClrEol;
                TempString := ' 9. Tail end msg  - ' + TailEndMessageR1;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                Write   ('A. Short 0 = ', Short0, '   ',
                         'B. Short 1 = ', Short1, '   ',
                         'C. Short 2 = ', Short2, '   ',
                         'D. Short 9 = ', Short9);
                END

            ELSE  { Phone mode }
                BEGIN
                GoToXY (1, 1);
                WriteLnCenter ('OTHER SSB MESSAGE MEMORY STATUS RADIO ONE');

                ClrEol;
                TempString := ' 1. Call Okay Now - ' + CorrectedCallPhoneMessageR1;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                TempString := ' 2. CQ Exchange   - ' + CQPhoneExchangeR1;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                TempString := ' 3. CQ Ex Name    - ' + CQPhoneExchangeNameKnownR1;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                TempString := ' 4. QSL Message   - ' + QSLPhoneMessageR1;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                TempString := ' 5. QSO Before    - ' + QSOBeforePhoneMessageR1;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                TempString := ' 6. Quick QSL     - ' + QuickQSLPhoneMessageR1;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                TempString := ' 7. Repeat S&P Ex - ' + RepeatSearchAndPouncePhoneExchangeR1;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                TempString := ' 8. S&P Exchange  - ' + SearchAndPouncePhoneExchangeR1;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl (TempString);

                ClrEol;
                TempString := ' 9. Tail end msg  - ' + TailEndPhoneMessageR1;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl (TempString);

                ClrEol;
                END;
            END;

        RadioTwo:
            BEGIN
            IF (ActiveMode = CW) OR (ActiveMode = Digital) THEN
                BEGIN
                GoToXY (1, 1);
                WriteLnCenter ('OTHER CW/DIGITAL MESSAGE MEMORY STATUS RADIO TWO');

                ClrEol;
                TempString := ' 1. Call Okay Now - ' + CorrectedCallMessageR2;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl (TempString);

                ClrEol;

                IF ActiveMode = CW THEN
                    BEGIN
                    TempString := ' 2. CQ Exchange   - ' + CQExchangeR2;
                    IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                    WriteLnControl (TempString);
                    END
                ELSE
                    WriteLn ('2. Use Exchange Memory F2 for Digital CQ Exchange');

                ClrEol;
                TempString := ' 3. CQ Ex Name    - ' + CQExchangeNameKnownR2;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                TempString := ' 4. QSL Message   - ' + QSLMessageR2;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                TempString := ' 5. QSO Before    - ' + QSOBeforeMessageR2;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                TempString := ' 6. Quick QSL     - ' + QuickQSLMessage1R2;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                IF ActiveMode = CW THEN
                    BEGIN
                    ClrEol;
                    TempString := ' 7. Repeat S&P Ex - ' + RepeatSearchAndPounceExchangeR2;
                    IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                    WriteLnControl(TempString);
                    END
                ELSE
                    Write ('7. Use Exchange Memory F3 for REPEAT S&P Exchange');

                IF ActiveMode = CW THEN
                    BEGIN
                    ClrEol;
                    TempString := ' 8. S&P Exchange  - ' + SearchAndPounceExchangeR2;
                    IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                    WriteLnControl (TempString);
                    END
                ELSE
                    WriteLn ('8. Use Exchange Memory F2 for S&P Exchange');

                ClrEol;
                TempString := ' 9. Tail end msg  - ' + TailEndMessageR2;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                Write   ('A. Short 0 = ', Short0, '   ',
                         'B. Short 1 = ', Short1, '   ',
                         'C. Short 2 = ', Short2, '   ',
                         'D. Short 9 = ', Short9);
                END

            ELSE  { Phone mode }
                BEGIN
                GoToXY (1, 1);
                WriteLnCenter ('OTHER SSB MESSAGE MEMORY STATUS RADIO TWO');

                ClrEol;
                TempString := ' 1. Call Okay Now - ' + CorrectedCallPhoneMessageR2;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                TempString := ' 2. CQ Exchange   - ' + CQPhoneExchangeR2;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                TempString := ' 3. CQ Ex Name    - ' + CQPhoneExchangeNameKnownR2;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                TempString := ' 4. QSL Message   - ' + QSLPhoneMessageR2;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                TempString := ' 5. QSO Before    - ' + QSOBeforePhoneMessageR2;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                TempString := ' 6. Quick QSL     - ' + QuickQSLPhoneMessageR2;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                TempString := ' 7. Repeat S&P Ex - ' + RepeatSearchAndPouncePhoneExchangeR2;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl(TempString);

                ClrEol;
                TempString := ' 8. S&P Exchange  - ' + SearchAndPouncePhoneExchangeR2;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl (TempString);

                ClrEol;
                TempString := ' 9. Tail end msg  - ' + TailEndPhoneMessageR2;
                IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
                WriteLnControl (TempString);

                ClrEol;
                END;
            END;

        END;  { of CASE Radio }
    END;



PROCEDURE AppendConfigFile (AddedLine: Str160);

VAR FileWrite: TEXT;

    BEGIN
    IF OpenFileForAppend (FileWrite, LogConfigFileName) THEN
        BEGIN
        WriteLn (FileWrite);
        WriteLn (FileWrite, AddedLine);
        Close (FileWrite);
        END;
    END;


PROCEDURE DVKLIstenMessage (MemoryString: Str20);

    BEGIN
    MemoryString := UpperCase (MemoryString);
    IF MemoryString = 'DVK1' THEN StartDVK (1);
    IF MemoryString = 'DVK2' THEN StartDVK (2);
    IF MemoryString = 'DVK3' THEN StartDVK (3);
    IF MemoryString = 'DVK4' THEN StartDVK (4);
    IF MemoryString = 'DVK5' THEN StartDVK (5); {KK1L: 6.71}
    IF MemoryString = 'DVK6' THEN StartDVK (6); {KK1L: 6.71}
    {IF MemoryString = 'DVK7' THEN StartDVK (7);} {KK1L: 6.71} {KK1L: 6.72 removed}
    END;

PROCEDURE DVKRecordMessage (MemoryString: Str20);

    BEGIN
    MemoryString := UpperCase (MemoryString);

    IF Copy (MemoryString, 1, 3) <> 'DVK' THEN Exit;

    DVKEnableWrite;

    IF MemoryString = 'DVK1' THEN StartDVK (1);
    IF MemoryString = 'DVK2' THEN StartDVK (2);
    IF MemoryString = 'DVK3' THEN StartDVK (3);
    IF MemoryString = 'DVK4' THEN StartDVK (4);
    IF MemoryString = 'DVK5' THEN StartDVK (5); {KK1L: 6.71}
    IF MemoryString = 'DVK6' THEN StartDVK (6); {KK1L: 6.71}
    {IF MemoryString = 'DVK7' THEN StartDVK (7); }{KK1L: 6.71} {KK1L: 6.72 removed}

    REPEAT millisleep UNTIL KeyPressed;

    DVKDisableWrite;

    IF ReadKey = NullKey THEN ReadKey;
    END;



PROCEDURE MemoryProgram (Radio: RadioType; Mode: ModeType);

VAR Key, FirstExchangeFunctionKey, FunctionKey: CHAR;
    ModeString, RadioString, TempString: Str160;
    TimeMark: TimeRecord;

    BEGIN
    CASE Radio OF
        NoRadio:  RadioString := '';
        RadioOne: RadioString := 'RADIO1';
        RadioTwo: RadioString := 'RADIO2';
        END;

    CASE Mode OF
        Phone:  BEGIN
                FirstExchangeFunctionKey := F1;
                ModeString := 'SSB';
                END;

        Digital:
                BEGIN
                FirstExchangeFunctionKey := F1;
                ModeString := 'RTTY';
                END;

        CW:     BEGIN
                FirstExchangeFunctionKey := F3;
                ModeString := 'CW';
                END;
        END;

    RemoveWindow (QuickCommandWindow);
    SaveSetAndClearActiveWindow (EditableLogWindow);

    WriteLnCenter ('MEMORY PROGRAM FUNCTION');
    WriteLn ('Press C to program a CQ function key.');
    WriteLn ('Press E to program an exchange/search and pounce function key.');
    WriteLn ('Press O to program the other non function key messages.');
    Write   ('Press ESCAPE to abort.');

    MarkTime (TimeMark);

    REPEAT
        REPEAT millisleep UNTIL NewKeyPressed;
        Key := Upcase (NewReadKey);

        IF (ActiveMultiPort <> nil) OR (MultiUDPPort > -1) THEN
            IF ElaspedSec100 (TimeMark) > 3000 THEN
                BEGIN
                RemoveAndRestorePreviousWindow;
                Exit;
                END;

    UNTIL (Key = 'C') OR (Key = 'E') OR (Key = 'O') OR (Key = EscapeKey);

    RemoveAndRestorePreviousWindow;

    IF Key = EscapeKey THEN Exit;

    RemoveWindow (TotalWindow);
    SaveSetAndClearActiveWindow (BigWindow);

    IF (Mode = CW) OR (Mode = Digital) THEN
        DisplayCrypticCWMenu
    ELSE
        DisplayCrypticSSBMenu;

    VisibleDupeSheetRemoved := True;

    KeyStatus := NormalKeys;

    CASE Key OF

        { C key is for CQ memories }

        'C': BEGIN
             REPEAT
                 ShowCQFunctionKeyStatus (Radio, Mode);
                 GoToXY (1, Hi (WindMax));
                 Write (' Press CQ function key to program (F1, AltF1, CtrlF1), or ESCAPE to exit) : '); {KK1L: 6.72 changed}

                 MarkTime (TimeMark);

                 REPEAT
                     REPEAT
                         IF (ActiveMultiPort <> nil) OR (MultiUDPPort > -1) THEN
                             IF ElaspedSec100 (TimeMark) > 3000 THEN
                                 BEGIN
                                 RemoveAndRestorePreviousWindow;
                                 Exit;
                                 END;
                         millisleep;
                     UNTIL NewKeyPressed;

                     FunctionKey := Upcase (NewReadKey);
                 UNTIL (FunctionKey = NullKey) OR (FunctionKey = EscapeKey);

                 IF FunctionKey = EscapeKey THEN
                     BEGIN
                     RemoveAndRestorePreviousWindow;
                     Exit;
                     END;

                 { We have a null key - so have to read again to get the function key }

                 FunctionKey := NewReadKey;

                 IF ((FunctionKey >= F1) AND (FunctionKey <= F10)) OR
                    ((FunctionKey >= ControlF1) AND (FunctionKey <= ControlF10)) OR
                    ((FunctionKey >= AltF1) AND (FunctionKey <= AltF10)) OR
                    ((FunctionKey >= F11) AND (FunctionKey <= AltF12)) THEN
                     BEGIN
                     IF FunctionKey >= AltF1 THEN
                         BEGIN
                         IF KeyStatus <> AltKeys THEN
                             BEGIN
                             KeyStatus := AltKeys;
                             ShowCQFunctionKeyStatus (Radio, Mode);
                             END;
                         END
                     ELSE
                         IF FunctionKey >= ControlF1 THEN
                             BEGIN
                             IF KeyStatus <> ControlKeys THEN
                                 BEGIN
                                 KeyStatus := ControlKeys;
                                 ShowCQFunctionKeyStatus (Radio, Mode);
                                 END;
                             END
                         ELSE
                             IF KeyStatus <> NormalKeys THEN
                                 BEGIN
                                 KeyStatus := NormalKeys;
                                 ShowCQFunctionKeyStatus (Radio, Mode);
                                 END;

                     SaveSetAndClearActiveWindow (QuickCommandWindow);

                     REPEAT
                         TempString := LineInput ('Msg = ',
                                              GetCQMemoryString (Radio, Mode, FunctionKey),  {KK1L: 6.73 Added mode}
                                              True,
                                              (Mode = Phone) AND (DVKEnable));

                         IF TempString [1] = NullKey THEN
                                 IF DVKEnable THEN
                                     CASE TempString [2] OF
                                         {KK1L: 6.73 Added mode}
                                         AltW: DVKRecordMessage (GetCQMemoryString (Radio, Mode, FunctionKey));
                                         {KK1L: 6.73 Added mode}
                                         AltR: DVKListenMessage (GetCQMemoryString (Radio, Mode, FunctionKey));
                                         END;
                         millisleep;
                     UNTIL (TempString [1] <> NullKey);

                     IF (TempString <> EscapeKey) AND
                        {KK1L: 6.73 Added mode}
                        (GetCQMemoryString (Radio, Mode, FunctionKey) <> TempString) THEN
                            BEGIN
                            SetCQMemoryStringRadio (Radio, Mode, FunctionKey, TempString);

                            IF Mode = Phone THEN
                                AppendConfigFile ('CQ SSB MEMORY ' + RadioString + ' ' + KeyId (FunctionKey) + ' = ' + TempString)
                            ELSE
                                AppendConfigFile ('CQ MEMORY ' + RadioString + ' ' + KeyId (FunctionKey) + ' = ' + TempString);
                            END;

                     RemoveAndRestorePreviousWindow;
                     END;  { End of legal funtion keys }
             UNTIL False;
             END;   { End of CQ memories }

        'E': REPEAT
                 ShowEXFunctionKeyStatus (Radio, Mode);
                 GoToXY (1, Hi (WindMax));
                 Write (' Press ex function key to program (F3-F12, Alt/Ctrl F1-F12) or ESCAPE to exit :');
                 {KK1L: 6.72 changed above line}

                 MarkTime (TimeMark);

                 REPEAT
                     REPEAT
                         IF (ActiveMultiPort <> nil) OR (MultiUDPPort > -1) THEN
                             IF ElaspedSec100 (TimeMark) > 3000 THEN
                                 BEGIN
                                 RemoveAndRestorePreviousWindow;
                                 Exit;
                                 END;

                         millisleep;
                     UNTIL NewKeyPressed;

                     FunctionKey := Upcase (NewReadKey);
                 UNTIL (FunctionKey = NullKey) OR (FunctionKey = EscapeKey);

                 IF FunctionKey = EscapeKey THEN
                     BEGIN
                     RemoveAndRestorePreviousWindow;
                     Exit;
                     END;

                 FunctionKey := NewReadKey;

                 IF ((FunctionKey >= FirstExchangeFunctionKey) AND (FunctionKey <= F10)) OR
                    ((FunctionKey >= ControlF1) AND (FunctionKey <= ControlF10)) OR
                    ((FunctionKey >= AltF1) AND (FunctionKey <= AltF10)) OR
                    ((FunctionKey >= F11) AND (FunctionKey <= AltF12)) THEN
                     BEGIN
                     IF FunctionKey >= AltF1 THEN
                         BEGIN
                         IF KeyStatus <> AltKeys THEN
                             BEGIN
                             KeyStatus := AltKeys;
                             ShowEXFunctionKeyStatus (Radio, Mode);
                             END;
                         END
                     ELSE
                         IF FunctionKey >= ControlF1 THEN
                             BEGIN
                             IF KeyStatus <> ControlKeys THEN
                                 BEGIN
                                 KeyStatus := ControlKeys;
                                 ShowEXFunctionKeyStatus (Radio, Mode);
                                 END;
                             END
                         ELSE
                             IF KeyStatus <> NormalKeys THEN
                                 BEGIN
                                 KeyStatus := NormalKeys;
                                 ShowEXFunctionKeyStatus (Radio, Mode);
                                 END;

                     SaveSetAndClearActiveWindow (QuickCommandWindow);

                     REPEAT
                         TempString := LineInput ('Msg = ',
                                                  {KK1L: 6.73 Added mode to GetExMemoryString}
                                                  GetEXMemoryString (Radio, Mode, FunctionKey),
                                                  True,
                                                  (Mode = Phone) AND (DVKEnable));

                         IF TempString [1] = NullKey THEN
                                 IF DVKEnable THEN
                                     CASE TempString [2] OF
                                         {KK1L: 6.73 Added mode to GetExMemoryString}
                                         AltW: DVKRecordMessage (GetEXMemoryString (Radio, Mode, FunctionKey));
                                         AltR: DVKListenMessage (GetEXMemoryString (Radio, Mode, FunctionKey));
                                         END;

                         millisleep;
                     UNTIL (TempString [1] <> NullKey);

                     IF TempString <> EscapeKey THEN
                         BEGIN
                         SetExMemoryStringRadio (Radio, Mode, FunctionKey, TempString);

                         CASE Mode OF
                             Phone:
                                 AppendConfigFile ('EX SSB MEMORY ' + RadioString + ' ' + KeyId (FunctionKey) + ' = ' + TempString);

                             CW:
                                 AppendConfigFile ('EX MEMORY ' + RadioString + ' ' + KeyId (FunctionKey) + ' = ' + TempString);

                             Digital:
                                 AppendConfigFile ('EX DIGITAL MEMORY ' + RadioString + ' ' + KeyId (FunctionKey) + ' = ' + TempString);

                             END;  { of CASE Mode }
                         END;

                     RemoveAndRestorePreviousWindow;
                     END;
             UNTIL False;


        'O': REPEAT
                 ShowOtherMemoryStatus (Radio, Mode);
                 GoToXY (1, Hi (WindMax));

                 Write ('Number or letter of message to be programmed (1-9, A-D, or ESCAPE to exit) : ');

                 MarkTime (TimeMark);

                 REPEAT
                     REPEAT
                         IF (ActiveMultiPort <> nil) OR (MultiUDPPort > -1) THEN
                             IF ElaspedSec100 (TimeMark) > 3000 THEN
                                 BEGIN
                                 RemoveAndRestorePreviousWindow;
                                 Exit;
                                 END;

                         millisleep;
                     UNTIL NewKeyPressed;

                     FunctionKey := Upcase (ReadKey);
                 UNTIL ((FunctionKey >= '1') AND (FunctionKey <= '9')) OR
                       ((FunctionKey >= 'A') AND (FunctionKey <= 'D')) OR
                        (FunctionKey = EscapeKey);

                 IF FunctionKey = EscapeKey THEN
                     BEGIN
                     RemoveAndRestorePreviousWindow;
                     Exit;
                     END;

                 SaveSetAndClearActiveWindow (QuickCommandWindow);

                 CASE FunctionKey OF
                       '1': BEGIN
                            IF Mode <> Phone THEN
                                BEGIN
                                IF Radio = RadioOne THEN
                                    BEGIN
                                    TempString := LineInput ('Msg = ',
                                                             CorrectedCallMessageR1,
                                                             True,
                                                             False);

                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        CorrectedCallMessageR1 := TempString;
                                        AppendConfigFile ('CALL OK NOW MESSAGE RADIO1 = ' + TempString);
                                        END;
                                    END
                                ELSE   { Radio two }
                                    BEGIN
                                    TempString := LineInput ('Msg = ',
                                                             CorrectedCallMessageR2,
                                                             True,
                                                             False);

                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        CorrectedCallMessageR2 := TempString;
                                        AppendConfigFile ('CALL OK NOW MESSAGE RADIO2 = ' + TempString);
                                        END;
                                    END;
                                END
                            ELSE    { Phone }
                                IF Radio = RadioOne THEN
                                    BEGIN
                                    REPEAT
                                        TempString := LineInput ('Msg = ',
                                                                 CorrectedCallPhoneMessageR1,
                                                                 True,
                                                                 True);


                                        IF TempString [1] = NullKey THEN
                                                IF DVKEnable THEN
                                                    CASE TempString [2] OF
                                                        AltW: DVKRecordMessage (CorrectedCallPhoneMessageR1);
                                                        AltR: DVKListenMessage (CorrectedCallPhoneMessageR1);
                                                        END;


                                        millisleep;
                                    UNTIL (TempString [1] <> NullKey);

                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        CorrectedCallPhoneMessageR1 := TempString;
                                        AppendConfigFile ('CALL OK NOW SSB MESSAGE RADIO1 = ' + TempString);
                                        END;
                                    END
                                ELSE      { Radio Two }
                                    BEGIN
                                    REPEAT
                                        TempString := LineInput ('Msg = ',
                                                                 CorrectedCallPhoneMessageR2,
                                                                 True,
                                                                 True);


                                        IF TempString [1] = NullKey THEN
                                                IF DVKEnable THEN
                                                    CASE TempString [2] OF
                                                        AltW: DVKRecordMessage (CorrectedCallPhoneMessageR2);
                                                        AltR: DVKListenMessage (CorrectedCallPhoneMessageR2);
                                                        END;


                                        millisleep;
                                    UNTIL (TempString [1] <> NullKey);

                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        CorrectedCallPhoneMessageR2 := TempString;
                                        AppendConfigFile ('CALL OK NOW SSB MESSAGE RADIO2 = ' + TempString);
                                        END;
                                    END;

                            END;  { of 1 }

                       '2': BEGIN
                            IF Mode <> Phone THEN
                                BEGIN
                                IF Radio = RadioOne THEN
                                    BEGIN
                                    TempString := LineInput ('Msg = ', CQExchangeR1, True, False);
                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        CQExchangeR1 := TempString;
                                        AppendConfigFile ('CQ EXCHANGE RADIO1 = ' + TempString);
                                        END;
                                    END
                                ELSE    { Radio Two }
                                    BEGIN
                                    TempString := LineInput ('Msg = ', CQExchangeR2, True, False);
                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        CQExchangeR2 := TempString;
                                        AppendConfigFile ('CQ EXCHANGE RADIO2 = ' + TempString);
                                        END;
                                    END;
                                END
                            ELSE  { Phone }
                                IF Radio = RadioOne THEN
                                    BEGIN
                                    REPEAT
                                        TempString := LineInput ('Msg = ',
                                                                 CQPhoneExchangeR1,
                                                                 True,
                                                                 True);

                                        IF TempString [1] = NullKey THEN
                                                IF DVKEnable THEN
                                                    CASE TempString [2] OF
                                                        AltW: DVKRecordMessage (CQPhoneExchangeR1);
                                                        AltR: DVKListenMessage (CQPhoneExchangeR1);
                                                        END;
                                        millisleep;
                                    UNTIL (TempString [1] <> NullKey);

                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        CQPhoneExchangeR1 := TempString;
                                        AppendConfigFile ('CQ SSB EXCHANGE RADIO1 = ' + TempString);
                                        END;
                                    END
                                ELSE { Radio 2 Phone }
                                    BEGIN
                                    REPEAT
                                        TempString := LineInput ('Msg = ',
                                                                 CQPhoneExchangeR2,
                                                                 True,
                                                                 True);

                                        IF TempString [1] = NullKey THEN
                                                IF DVKEnable THEN
                                                    CASE TempString [2] OF
                                                        AltW: DVKRecordMessage (CQPhoneExchangeR2);
                                                        AltR: DVKListenMessage (CQPhoneExchangeR2);
                                                        END;
                                        millisleep;
                                    UNTIL (TempString [1] <> NullKey);

                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        CQPhoneExchangeR2 := TempString;
                                        AppendConfigFile ('CQ SSB EXCHANGE RADIO2 = ' + TempString);
                                        END;
                                    END;

                            END;  { of 2 }

                       '3': BEGIN
                            IF Mode <> Phone THEN
                                BEGIN
                                IF Radio = RadioOne THEN
                                    BEGIN
                                    TempString := LineInput ('Msg = ', CQExchangeNameKnownR1, True, False);
                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        CQExchangeNameKnownR1 := TempString;
                                        AppendConfigFile ('CQ EXCHANGE NAME KNOWN RADIO1 = ' + TempString);
                                        END;
                                    END
                                ELSE
                                    BEGIN  { Radio Two }
                                    TempString := LineInput ('Msg = ', CQExchangeNameKnownR2, True, False);
                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        CQExchangeNameKnownR2 := TempString;
                                        AppendConfigFile ('CQ EXCHANGE NAME KNOWN RADIO2 = ' + TempString);
                                        END;
                                    END;
                                END
                            ELSE  { Phone }
                                IF Radio = RadioOne THEN
                                    BEGIN
                                    REPEAT
                                        TempString := LineInput ('Msg = ',
                                                                 CQPhoneExchangeNameKnownR1,
                                                                 True,
                                                                 True);

                                        IF TempString [1] = NullKey THEN
                                                IF DVKEnable THEN
                                                    CASE TempString [2] OF
                                                        AltW: DVKRecordMessage (CQPhoneExchangeNameKnownR1);
                                                        AltR: DVKListenMessage (CQPhoneExchangeNameKnownR1);
                                                        END;
                                        millisleep;
                                    UNTIL (TempString [1] <> NullKey);

                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        CQPhoneExchangeNameKnownR1 := TempString;
                                        AppendConfigFile ('CQ SSB EXCHANGE NAME KNOWN RADIO1 = ' + TempString);
                                        END;
                                    END
                                ELSE
                                    BEGIN { Radio 2 Phone }
                                    REPEAT
                                        TempString := LineInput ('Msg = ',
                                                                 CQPhoneExchangeNameKnownR2,
                                                                 True,
                                                                 True);

                                        IF TempString [1] = NullKey THEN
                                                IF DVKEnable THEN
                                                    CASE TempString [2] OF
                                                        AltW: DVKRecordMessage (CQPhoneExchangeNameKnownR2);
                                                        AltR: DVKListenMessage (CQPhoneExchangeNameKnownR2);
                                                        END;
                                        millisleep;
                                    UNTIL (TempString [1] <> NullKey);

                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        CQPhoneExchangeNameKnownR2 := TempString;
                                        AppendConfigFile ('CQ SSB EXCHANGE NAME KNOWN RADIO2 = ' + TempString);
                                        END;
                                    END;

                            END;  { of 3 }

                       '4': BEGIN
                            IF Mode <> Phone THEN
                                BEGIN
                                IF Radio = RadioOne THEN
                                    BEGIN
                                    TempString := LineInput ('Msg = ', QSLMessageR1, True, False);
                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        QSLMessageR1 := TempString;
                                        AppendConfigFile ('QSL MESSAGE RADIO1 = ' + TempString);
                                        END;
                                    END
                                ELSE
                                    BEGIN { radio 2 }
                                    TempString := LineInput ('Msg = ', QSLMessageR2, True, False);
                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        QSLMessageR2 := TempString;
                                        AppendConfigFile ('QSL MESSAGE RADIO2 = ' + TempString);
                                        END;
                                    END;
                                END
                            ELSE  { Phone }
                                IF Radio = RadioOne THEN
                                    BEGIN
                                    REPEAT
                                        TempString := LineInput ('Msg = ',
                                                                 QSLPhoneMessageR1,
                                                                 True, True);

                                        IF TempString [1] = NullKey THEN
                                                IF DVKEnable THEN
                                                    CASE TempString [2] OF
                                                        AltW: DVKRecordMessage (QSLPhoneMessageR1);
                                                        AltR: DVKListenMessage (QSLPhoneMessageR1);
                                                        END;
                                        millisleep;
                                    UNTIL (TempString [1] <> NullKey);

                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        QSLPhoneMessageR1 := TempString;
                                        AppendConfigFile ('QSL SSB MESSAGE RADIO1 = ' + TempString);
                                        END;
                                    END
                                ELSE
                                    BEGIN  { Phone radio 2 }
                                    REPEAT
                                        TempString := LineInput ('Msg = ',
                                                                 QSLPhoneMessageR2,
                                                                 True, True);

                                        IF TempString [1] = NullKey THEN
                                                IF DVKEnable THEN
                                                    CASE TempString [2] OF
                                                        AltW: DVKRecordMessage (QSLPhoneMessageR2);
                                                        AltR: DVKListenMessage (QSLPhoneMessageR2);
                                                        END;
                                        millisleep;
                                    UNTIL (TempString [1] <> NullKey);

                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        QSLPhoneMessageR2 := TempString;
                                        AppendConfigFile ('QSL SSB MESSAGE RADIO2 = ' + TempString);
                                        END;
                                    END;

                            END; { of 4 }

                       '5': BEGIN
                            IF Mode <> Phone THEN
                                BEGIN
                                IF Radio = RadioOne THEN
                                    BEGIN
                                    TempString := LineInput ('Msg = ', QSOBeforeMessageR1, True, False);
                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        QSOBeforeMessageR1 := TempString;
                                        AppendConfigFile ('QSO BEFORE MESSAGE RADIO1 = ' + TempString);
                                        END;
                                    END
                                ELSE
                                    BEGIN  { radio Two }
                                    TempString := LineInput ('Msg = ', QSOBeforeMessageR2, True, False);
                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        QSOBeforeMessageR2 := TempString;
                                        AppendConfigFile ('QSO BEFORE MESSAGE RADIO2 = ' + TempString);
                                        END;
                                    END;
                                END
                            ELSE  { Phone }
                                IF Radio = RadioOne THEN
                                    BEGIN
                                    REPEAT
                                        TempString := LineInput ('Msg = ',
                                                                 QSOBeforePhoneMessageR1,
                                                                 True, True);

                                        IF TempString [1] = NullKey THEN
                                                IF DVKEnable THEN
                                                    CASE TempString [2] OF
                                                        AltW: DVKRecordMessage (QSOBeforePhoneMessageR1);
                                                        AltR: DVKListenMessage (QSOBeforePhoneMessageR1);
                                                        END;
                                        millisleep;
                                    UNTIL (TempString [1] <> NullKey);

                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        QSOBeforePhoneMessageR1 := TempString;
                                        AppendConfigFile ('QSO BEFORE SSB MESSAGE RADIO1 = ' + TempString);
                                        END;
                                    END
                                ELSE
                                    BEGIN  { Phone Radio 2 }
                                    REPEAT
                                        TempString := LineInput ('Msg = ',
                                                                 QSOBeforePhoneMessageR2,
                                                                 True, True);

                                        IF TempString [1] = NullKey THEN
                                                IF DVKEnable THEN
                                                    CASE TempString [2] OF
                                                        AltW: DVKRecordMessage (QSOBeforePhoneMessageR2);
                                                        AltR: DVKListenMessage (QSOBeforePhoneMessageR2);
                                                        END;
                                        millisleep;
                                    UNTIL (TempString [1] <> NullKey);

                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        QSOBeforePhoneMessageR2 := TempString;
                                        AppendConfigFile ('QSO BEFORE SSB MESSAGE RADIO2 = ' + TempString);
                                        END;
                                    END

                            END; { of 5 }

                       '6': BEGIN
                            IF Mode <> Phone THEN
                                BEGIN
                                IF Radio = RadioOne THEN
                                    BEGIN
                                    TempString := LineInput ('Msg = ', QuickQSLMessage1R1, True, False);
                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        QuickQSLMessage1R1 := TempString;
                                        AppendConfigFile ('QUICK QSL MESSAGE RADIO1 = ' + TempString);
                                        END;
                                    END
                                ELSE  { Radio two }
                                    BEGIN
                                    TempString := LineInput ('Msg = ', QuickQSLMessage1R2, True, False);
                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        QuickQSLMessage1R2 := TempString;
                                        AppendConfigFile ('QUICK QSL MESSAGE RADIO2 = ' + TempString);
                                        END;
                                    END;
                                END
                            ELSE  { Phone }
                                IF Radio = RadioOne THEN
                                    BEGIN
                                    REPEAT
                                        TempString := LineInput ('Msg = ',
                                                                 QuickQSLPhoneMessageR1,
                                                                 True, True);

                                        IF TempString [1] = NullKey THEN
                                                IF DVKEnable THEN
                                                    CASE TempString [2] OF
                                                        AltW: DVKRecordMessage (QuickQSLPhoneMessageR1);
                                                        AltR: DVKListenMessage (QuickQSLPhoneMessageR1);
                                                        END;
                                        millisleep;
                                    UNTIL (TempString [1] <> NullKey);

                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        QuickQSLPhoneMessageR1 := TempString;
                                        AppendConfigFile ('QUICK QSL SSB MESSAGE RADIO1 = ' + TempString);
                                        END;
                                    END
                                ELSE
                                    BEGIN { Phone radio 2 }
                                    REPEAT
                                        TempString := LineInput ('Msg = ',
                                                                 QuickQSLPhoneMessageR2,
                                                                 True, True);

                                        IF TempString [1] = NullKey THEN
                                                IF DVKEnable THEN
                                                    CASE TempString [2] OF
                                                        AltW: DVKRecordMessage (QuickQSLPhoneMessageR2);
                                                        AltR: DVKListenMessage (QuickQSLPhoneMessageR2);
                                                        END;
                                        millisleep;
                                    UNTIL (TempString [1] <> NullKey);

                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        QuickQSLPhoneMessageR2 := TempString;
                                        AppendConfigFile ('QUICK QSL SSB MESSAGE RADIO2 = ' + TempString);
                                        END;
                                    END;

                            END;  { of 6 }

                       '7': BEGIN
                            IF Mode <> Phone THEN
                                BEGIN
                                IF Radio = RadioOne THEN
                                    BEGIN
                                    TempString := LineInput ('Msg = ', RepeatSearchAndPounceExchangeR1, True, False);
                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        RepeatSearchAndPounceExchangeR1 := TempString;
                                        AppendConfigFile ('REPEAT S&P EXCHANGE RADIO1 = ' + TempString);
                                        END;
                                    END
                                ELSE
                                    BEGIN
                                    TempString := LineInput ('Msg = ', RepeatSearchAndPounceExchangeR2, True, False);
                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        RepeatSearchAndPounceExchangeR2 := TempString;
                                        AppendConfigFile ('REPEAT S&P EXCHANGE RADIO2 = ' + TempString);
                                        END;
                                    END;
                                END
                            ELSE  { Phone }
                                IF Radio = RadioOne THEN
                                    BEGIN
                                    REPEAT
                                        TempString := LineInput ('Msg = ',
                                                                 RepeatSearchAndPouncePhoneExchangeR1,
                                                                 True, True);

                                        IF TempString [1] = NullKey THEN
                                                IF DVKEnable THEN
                                                    CASE TempString [2] OF
                                                        AltW: DVKRecordMessage (RepeatSearchAndPouncePhoneExchangeR1);
                                                        AltR: DVKListenMessage (RepeatSearchAndPouncePhoneExchangeR1);
                                                        END;
                                        millisleep;
                                    UNTIL (TempString [1] <> NullKey);

                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        RepeatSearchAndPouncePhoneExchangeR1 := TempString;
                                        AppendConfigFile ('REPEAT S&P SSB EXCHANGE RADIO1 = ' + TempString);
                                        END;
                                    END
                                ELSE
                                    BEGIN
                                    REPEAT
                                        TempString := LineInput ('Msg = ',
                                                                 RepeatSearchAndPouncePhoneExchangeR2,
                                                                 True, True);

                                        IF TempString [1] = NullKey THEN
                                                IF DVKEnable THEN
                                                    CASE TempString [2] OF
                                                        AltW: DVKRecordMessage (RepeatSearchAndPouncePhoneExchangeR2);
                                                        AltR: DVKListenMessage (RepeatSearchAndPouncePhoneExchangeR2);
                                                        END;
                                        millisleep;
                                    UNTIL (TempString [1] <> NullKey);

                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        RepeatSearchAndPouncePhoneExchangeR2 := TempString;
                                        AppendConfigFile ('REPEAT S&P SSB EXCHANGE RADIO2 = ' + TempString);
                                        END;
                                    END;

                            END; { of 7 }

                       '8': BEGIN
                            IF Mode <> Phone THEN
                                BEGIN
                                IF Radio = RadioOne THEN
                                    BEGIN
                                    TempString := LineInput ('Msg = ', SearchAndPounceExchangeR1, True, False);
                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        SearchAndPounceExchangeR1 := TempString;
                                        AppendConfigFile ('S&P EXCHANGE RADIO1 = ' + TempString);
                                        END;
                                    END
                                ELSE
                                    BEGIN
                                    TempString := LineInput ('Msg = ', SearchAndPounceExchangeR2, True, False);
                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        SearchAndPounceExchangeR2 := TempString;
                                        AppendConfigFile ('S&P EXCHANGE RADIO2 = ' + TempString);
                                        END;
                                    END;
                                END
                            ELSE
                                IF Radio = RadioOne THEN
                                    BEGIN
                                    REPEAT
                                        TempString := LineInput ('Msg = ',
                                                                 SearchAndPouncePhoneExchangeR1,
                                                                 True, True);

                                        IF TempString [1] = NullKey THEN
                                                IF DVKEnable THEN
                                                    CASE TempString [2] OF
                                                        AltW: DVKRecordMessage (SearchAndPouncePhoneExchangeR1);
                                                        AltR: DVKListenMessage (SearchAndPouncePhoneExchangeR1);
                                                        END;
                                        millisleep;
                                    UNTIL (TempString [1] <> NullKey);

                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        SearchAndPouncePhoneExchangeR1 := TempString;
                                        AppendConfigFile ('S&P SSB EXCHANGE RADIO1 = ' + TempString);
                                        END;
                                    END
                                ELSE
                                    BEGIN
                                    REPEAT
                                        TempString := LineInput ('Msg = ',
                                                                 SearchAndPouncePhoneExchangeR2,
                                                                 True, True);

                                        IF TempString [1] = NullKey THEN
                                                IF DVKEnable THEN
                                                    CASE TempString [2] OF
                                                        AltW: DVKRecordMessage (SearchAndPouncePhoneExchangeR2);
                                                        AltR: DVKListenMessage (SearchAndPouncePhoneExchangeR2);
                                                        END;
                                        millisleep;
                                    UNTIL (TempString [1] <> NullKey);

                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        SearchAndPouncePhoneExchangeR2 := TempString;
                                        AppendConfigFile ('S&P SSB EXCHANGE RADIO2 = ' + TempString);
                                        END;
                                    END;

                            END; { of 8 }

                       '9': BEGIN
                            IF Mode <> Phone THEN
                                BEGIN
                                IF Radio = RadioOne THEN
                                    BEGIN
                                    TempString := LineInput ('Msg = ', TailEndMessageR1, True, False);
                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        TailEndMessageR1 := TempString;
                                        AppendConfigFile ('TAIL END MESSAGE RADIO1 = ' + TempString);
                                        END;
                                    END
                                ELSE
                                    BEGIN
                                    TempString := LineInput ('Msg = ', TailEndMessageR2, True, False);
                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        TailEndMessageR2 := TempString;
                                        AppendConfigFile ('TAIL END MESSAGE RADIO2 = ' + TempString);
                                        END;
                                    END;
                                END
                            ELSE   { Phone }
                                IF Radio = RadioOne THEN
                                    BEGIN
                                    REPEAT
                                        TempString := LineInput ('Msg = ',
                                                                 TailEndPhoneMessageR1,
                                                                 True, True);

                                        IF TempString [1] = NullKey THEN
                                                IF DVKEnable THEN
                                                    CASE TempString [2] OF
                                                        AltW: DVKRecordMessage (TailEndPhoneMessageR1);
                                                        AltR: DVKListenMessage (TailEndPhoneMessageR1);
                                                        END;
                                        millisleep;
                                    UNTIL (TempString [1] <> NullKey);

                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        TailEndPhoneMessageR1 := TempString;
                                        AppendConfigFile ('TAIL END SSB MESSAGE RADIO1 = ' + TempString);
                                        END;
                                    END
                                ELSE
                                    BEGIN
                                    REPEAT
                                        TempString := LineInput ('Msg = ',
                                                                 TailEndPhoneMessageR2,
                                                                 True, True);

                                        IF TempString [1] = NullKey THEN
                                                IF DVKEnable THEN
                                                    CASE TempString [2] OF
                                                        AltW: DVKRecordMessage (TailEndPhoneMessageR2);
                                                        AltR: DVKListenMessage (TailEndPhoneMessageR2);
                                                        END;
                                        millisleep;
                                    UNTIL (TempString [1] <> NullKey);

                                    IF TempString <> EscapeKey THEN
                                        BEGIN
                                        TailEndPhoneMessageR2 := TempString;
                                        AppendConfigFile ('TAIL END SSB MESSAGE RADIO2 = ' + TempString);
                                        END;
                                    END;


                            END; { of 9 }

                       'A': IF Mode <> Phone THEN
                                BEGIN
                                TempString := LineInput ('Enter character for short zeros : ', '', True, False);

                                IF (TempString <> EscapeKey) AND (TempString <> '') THEN
                                    BEGIN
                                    Short0 := TempString [1];
                                    AppendConfigFile ('SHORT 0 = ' + Short0);
                                    END;
                                END;

                       'B': IF Mode <> Phone THEN
                                BEGIN
                                TempString := LineInput ('Enter character for short ones : ', '', True, False);

                                IF (TempString <> EscapeKey) AND (TempString <> '') THEN
                                    BEGIN
                                    Short1 := TempString [1];
                                    AppendConfigFile ('SHORT 1 = ' + Short1);
                                    END;
                                END;

                       'C': IF Mode <> Phone THEN
                                BEGIN
                                TempString := LineInput ('Enter character for short twos : ', '', True, False);

                                IF (TempString <> EscapeKey) AND (TempString <> '') THEN
                                    BEGIN
                                    Short2 := TempString [1];
                                    AppendConfigFile ('SHORT 2 = ' + Short2);
                                    END;
                                END;

                       'D': IF Mode <> Phone THEN
                                BEGIN
                                TempString := LineInput ('Enter character for short nines : ', '', True, False);

                                IF (TempString <> EscapeKey) AND (TempString <> '') THEN
                                    BEGIN
                                    Short9 := TempString [1];
                                    AppendConfigFile ('SHORT 9 = ' + Short9);
                                    END;
                                END;

                       END;  { of case }

                 RemoveAndRestorePreviousWindow;
             UNTIL False;
        END;
    END;



FUNCTION GetCQMemoryString (Radio: RadioType; Mode: ModeType; Key: CHAR): Str80; {KK1L: 6.73 Added Mode to do split mode}

    BEGIN
    IF Mode = Digital THEN Mode := CW;

    IF CQMemory [Radio, Mode, Key] <> nil THEN
        GetCQMemoryString := CQMemory [Radio, Mode, Key]^
    ELSE
        GetCQMemoryString := '';
    END;


FUNCTION GetEXMemoryString (Radio: RadioType; Mode: ModeType; Key: CHAR): Str80; {KK1L: 6.73 Added Mode to do split mode}

    BEGIN
    GetEXMemoryString := '';

    CASE MODE OF
        CW, Phone:
            IF EXMemory [Radio, Mode, Key] <> Nil THEN
                GetEXMemoryString := EXMemory [Radio, Mode, Key]^;

        { For Digital, use Digital EX memory if there - otherwise, use CW }

        Digital:
            IF ExMemory [Radio, Mode, Key] <> Nil then
                GetEXMemoryString := EXMemory [Radio, Mode, Key]^
            ELSE
                IF ExMemory [Radio, CW, Key] <> Nil THEN
                    GetExMemoryString := ExMemory [Radio, CW, Key]^;

        END;  { of case }
    END;


PROCEDURE SetCQMemoryStringRadio (Radio: RadioType; Mode: ModeType; Key: CHAR; MemoryString: Str80);

    BEGIN
    { All digital CQ strings go to the CW strings }

    IF Mode = Digital THEN Mode := CW;

    IF CQMemory [Radio, Mode, Key] = Nil THEN New (CQMemory [Radio, Mode, Key]);
    SniffOutControlCharacters (MemoryString); {KK1L: 6.72}
    CQMemory [Radio, Mode, Key]^ := MemoryString;
    END;


PROCEDURE SetEXMemoryStringRadio (Radio: RadioType; Mode: ModeType; Key: CHAR; MemoryString: Str80);

{ Must use digital mode - not CW instead }

    BEGIN
    IF EXMemory [Radio, Mode, Key] = Nil THEN
        New (EXMemory [Radio, Mode, Key]);

    SniffOutControlCharacters (MemoryString);
    EXMemory [Radio, Mode, Key]^ := MemoryString;
    END;


PROCEDURE InitializeKeyer;

    BEGIN
    ActiveKeyer.SetActiveRadio (RadioOne);
    TimerInit;
    ActiveKeyer.InitializeKeyer;
    ActiveKeyer.SetActiveRadio (RadioOne); // for yccc box
    END;


PROCEDURE UnInitializeKeyer;

    BEGIN
    IF ActiveKeyer.GetKeyerInitialized THEN
       BEGIN
          ActiveKeyer.UnInitializeKeyer;
          CloseDebug;
       END;
    END;



PROCEDURE SetUpToSendOnActiveRadio;

    BEGIN
    IF ActiveRadio = RadioOne THEN
        BEGIN
        IF NOT SendingOnRadioOne THEN
            BEGIN
            FlushCWBufferAndClearPTT;      { Clear CW sent on Inactive Radio}
            ActiveKeyer.SetActiveRadio (RadioOne);
            CodeSpeed := SpeedMemory[RadioOne]; {KK1L: 6.73}
            SetSpeed (CodeSpeed);
            SendingOnRadioOne := True;
            SendingOnRadioTwo := False;
            SetRelayForActiveRadio (ActiveRadio);
            END;
        END

    ELSE           { Radio Two }

        IF NOT SendingOnRadioTwo THEN
            BEGIN
            FlushCWBufferAndClearPTT;      { Clear CW sent on Inactive Radio}

            ActiveKeyer.SetActiveRadio(RadioTwo);
            CodeSpeed := SpeedMemory[RadioTwo]; {KK1L: 6.73}
            SetSpeed (CodeSpeed);
            SendingOnRadioOne := False;
            SendingOnRadioTwo := True;
            SetRelayForActiveRadio (ActiveRadio);
            END;

    KeyersSwapped := False;
    END;


PROCEDURE SetUpToSendOnInactiveRadio;

{ This used to swap ActiveRadio as well, but I decided not to do that
  anymore.  }

    BEGIN
    IF KeyersSwapped THEN Exit;        { Already swapped to inactive rig }

    IF ActiveRadio = RadioOne THEN
        BEGIN
        IF NOT SendingOnRadioTwo THEN
            BEGIN
            FlushCWBufferAndClearPTT;          { Clear CW being sent on Active Radio}
            ActiveKeyer.SetActiveRadio(RadioTwo);
            CodeSpeed := SpeedMemory[RadioTwo]; {KK1L: 6.73}
            SetSpeed (CodeSpeed);
            SetRelayForActiveRadio (RadioTwo);
            SendingOnRadioOne := False;
            SendingOnRadioTwo := True;
            END;
        END

    ELSE   { Active radio = radio two }

        IF NOT SendingOnRadioOne THEN
            BEGIN
            FlushCWBufferAndClearPTT;          { Clear CW being sent on Active Radio}
            ActiveKeyer.SetActiveRadio(RadioOne);
            CodeSpeed := SpeedMemory[RadioOne]; {KK1L: 6.73}
            SetSpeed (CodeSpeed);
            SetRelayForActiveRadio (RadioOne);
            SendingOnRadioOne := True;
            SendingOnRadioTwo := False;
            END;

    KeyersSwapped := True;
    END;



PROCEDURE ToggleCW (DisplayPrompt: BOOLEAN);

    BEGIN
    IF ActiveMode = CW THEN
        BEGIN
        IF CWEnabled THEN
            BEGIN
            IF DisplayPrompt THEN
                QuickDisplay ('CW disabled with Alt-K!!  Use Alt-K again to enable.');

            FlushCWBufferAndClearPTT;
            CWEnabled := False;
            END
        ELSE
            CWEnabled := True;
        END;

    DisplayCodeSpeed (CodeSpeed, CWEnabled, False, ActiveMode);
    SetSpeed (CodeSpeed);
    END;



PROCEDURE CWInit;

VAR TempKey: CHAR;
    Radio: RadioType;

    BEGIN
    FOR Radio := NoRadio TO RadioTwo DO
        FOR TempKey := F1 TO AltF12 DO
            BEGIN
            CQMemory [Radio, CW, TempKey]      := Nil;
            EXMemory [Radio, CW, TempKey]      := Nil;
            CQMemory [Radio, Phone, TempKey]   := Nil;
            EXMemory [Radio, Phone, TempKey]   := Nil;
            EXMemory [Radio, Digital, TempKey] := Nil;
            END;

    AutoCQMemory := NullCharacter;
    DetectedPaddleActivityR1 := False;
    DetectedPaddleActivityR2 := False;
    KeyPressedMemory := Chr (0);
    KeyersSwapped := False;
    LastRSTSent := '';
    CWMessageDone := True;
    CWMessageCommand := NoCWCommand;
    NeedToSetCQMode := False; {KK1L: 6.69 This variable is used to leap around some AutoS&PMode code.}
    RTTYTransmissionStarted := False;
    SendingOnRadioOne := False;
    SendingOnRadioTwo := False;
    END;


    BEGIN
    CWInit;
    END.
