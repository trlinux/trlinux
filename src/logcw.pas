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

USES LogDVP, SlowTree, Tree, LogWind, Dos, LogK1EA, trCrt,communication,keycode;

TYPE
     SendBufferType = ARRAY [0..255] OF Char;

     MessagePointer = ^Str80;
     FunctionKeyMemoryArray = ARRAY [CW..PHONE, F1..AltF12] OF MessagePointer;

     CWMessageCommandType = (NoCWCommand,
                             CWCommandControlEnter,
                             CWCommandCQMode,
                             CWCommandSAPMode,
                             CWCommandQSY);

VAR
    AutoCQDelayTime: INTEGER;
    AutoCQMemory:    CHAR;

    CorrectedCallMessage:      Str80;
    CorrectedCallPhoneMessage: Str80;

    CQExchange:               Str160;
    CQExchangeNameKnown:      Str160;
    CQPhoneExchange:          Str80;
    CQPhoneExchangeNameKnown: Str80;
    CWEnable:                 BOOLEAN;
    CWMessageCommand:         CWMessageCommandType;
    CWSpeedFromDataBase:      BOOLEAN;
    CWTone: INTEGER;

    CQMemory:  FunctionKeyMemoryArray;

    EXMemory:  FunctionKeyMemoryArray;

    KeyersSwapped:        BOOLEAN;
    KeyPressedMemory:     CHAR;

    LastRSTSent: STRING [3];
    LeadingZeros: INTEGER;
    LeadingZeroCharacter:  CHAR;

    NeedToSetCQMode:       BOOLEAN; {KK1L: 6.69 This variable is used to leap around some AutoS&PMode code.}

    QSLMessage:            Str160;
    QSLPhoneMessage:       Str80;
    QSOBeforeMessage:      Str160;
    QSOBeforePhoneMessage: Str80;
    QuickQSLPhoneMessage:  Str80;
    QuickQSLMessage1:      Str80;
    QuickQSLMessage2:      Str80;

    RememberCWSpeed:         INTEGER;
    RepeatSearchAndPounceExchange: Str80;
    RepeatSearchAndPouncePhoneExchange: Str80;

    RTTYTransmissionStarted: BOOLEAN;

    SearchAndPounceExchange: Str80;
    SearchAndPouncePhoneExchange: Str80;
    SendingOnRadioOne: BOOLEAN; {KK1L: 6.72 Moved from local (IMPLIMENTATION section) for use in LOGSUBS}
    SendingOnRadioTwo: BOOLEAN; {KK1L: 6.72 Moved from local (IMPLIMENTATION section) for use in LOGSUBS}

    Short0: CHAR;
    Short1: CHAR;
    Short2: CHAR;
    Short9: CHAR;
    TailEndMessage: Str80;
    TailEndPhoneMessage: Str80;



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

    FUNCTION  GetCQMemoryString (Mode: ModeType; Key: CHAR): Str80;{KK1L: 6.73 Added mode}
    FUNCTION  GetEXMemoryString (Mode: ModeType; Key: CHAR): Str80;{KK1L: 6.73 Added mode}

    PROCEDURE MemoryProgram;

    PROCEDURE PTTForceOn;

    FUNCTION  QSONumberString (QSONumber: INTEGER): Str80;

    PROCEDURE SendKeyboardInput;
    PROCEDURE SendKeysToRTTY;
    PROCEDURE SendStringAndStop (MSG: Str160);
    PROCEDURE SetSpeed (Speed: INTEGER);
    PROCEDURE SetCQMemoryString (Mode: ModeType; Key: CHAR; MemoryString: Str80);
    PROCEDURE SetEXMemoryString (Mode: ModeType; Key: CHAR; MemoryString: Str80);
    PROCEDURE SetNewCodeSpeed;
    PROCEDURE SetUpToSendOnActiveRadio;
    PROCEDURE SetUpToSendOnInactiveRadio;
    PROCEDURE StartRTTYTransmission (MSG: Str160);

    PROCEDURE ToggleCW (DisplayPrompt: BOOLEAN);

    PROCEDURE UnInitializeKeyer;

IMPLEMENTATION

USES CfgCmd,radio,timer,so2r,sysutils;

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

    BEGIN
    IF CWEnable AND CWEnabled THEN
        BEGIN
        ActiveKeyer.AddStringToBuffer (Msg, Tone);
        ActiveKeyer.SetCountsSinceLastCW(0);
        END;
    END;


FUNCTION CWStillBeingSent: BOOLEAN;

    BEGIN
    CWStillBeingSent := ActiveKeyer.CWStillBeingSent;
    END;

FUNCTION DeleteLastCharacter: BOOLEAN;

    BEGIN
    DeleteLastCharacter := ActiveKeyer.DeleteLastCharacter;
    END;



PROCEDURE FlushCWBufferAndClearPTT;

    BEGIN
    ActiveKeyer.PTTUnForce;
    ActiveKeyer.FlushCWBuffer;

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
    END;


PROCEDURE StartRTTYTransmission (MSG: Str160);

VAR CharPointer: INTEGER;

    BEGIN
    { This is legacy stuff - I have no idea if anyone uses it }

    IF (ActiveMode = Digital) AND (ActiveRTTYPort <> nil) THEN
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

    IF ActiveMode = Digital THEN
        BEGIN
        IF ActiveRadio = RadioOne THEN
            IF (Radio1Type = K2) OR (Radio1Type = K3) OR (Radio1Type = K4) THEN
                BEGIN
                rig1.directcommand ('TX;');

                IF MSG <> '' THEN
                    rig1.directcommand ('KYW' + MSG + ';');
                END;

        IF ActiveRadio = RadioTwo THEN
            IF (Radio1Type = K2) OR (Radio1Type = K3) OR (Radio1Type = K4) THEN
                BEGIN
                rig2.directcommand ('TX;');

                IF MSG <> '' THEN
                    rig2.directcommand ('KYW' + MSG + ';');
                END;
        END;

    END;


PROCEDURE ContinueRTTYTransmission (MSG: Str160);

VAR CharPointer: INTEGER;

    BEGIN
    IF (ActiveMode = Digital) AND (ActiveRTTYPort <> nil) THEN
        BEGIN
        WHILE NOT RTTYSendCharBuffer.FreeSpace >= Length (MSG) DO;

        IF Length (MSG) > 0 THEN
            FOR CharPointer := 1 TO Length (MSG) DO
                RTTYSendCharBuffer.AddEntry (Ord (MSG [CharPointer]));

        Exit;  { Added so K3/K4 code does not get executed }
        END;

    IF ActiveMode = Digital THEN
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

    IF (ActiveMode = Digital) AND (ActiveRTTYPort <> nil) THEN
        BEGIN
        WHILE NOT RTTYSendCharBuffer.FreeSpace >= Length (MSG) + 1 DO;

        IF Length (MSG) > 0 THEN
            FOR CharPointer := 1 TO Length (MSG) DO
                RTTYSendCharBuffer.AddEntry (Ord (MSG [CharPointer]));

        IF Length (RTTYReceiveString) > 0 THEN
            FOR CharPointer := 1 TO Length (RTTYReceiveString) DO
               RTTYSendCharBuffer.AddEntry (Ord (RTTYReceiveString [CharPointer]));

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


    RTTYTransmissionStarted := False;
    END;




PROCEDURE SendStringAndStop (MSG: Str160);

VAR CharPointer: INTEGER;

    BEGIN
    IF ActiveMode = CW THEN
        BEGIN
        IF CWEnable AND CWEnabled THEN
            BEGIN
            ActiveKeyer.AddStringToBuffer (MSG, CWTone);
            END;
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
    DisplayCodeSpeed (CodeSpeed, CWEnabled, DVPOn, ActiveMode);
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
    IF ActiveRTTYPort = nil THEN Exit;

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
    DisplayCodeSpeed (CodeSpeed, CWEnabled, DVPOn, ActiveMode);
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
            IF ActiveMultiPort <> nil THEN
                IF ElaspedSec100 (TimeMark) > 3000 THEN  { 30 second timeout }
                    BEGIN
                    FlushCWBufferAndClearPTT;
                    RemoveAndRestorePreviousWindow;
                    if so2r_l then so2r_i.setlatch(latchsave);
                    Exit;
                    END;

        UpdateTimeAndRateDisplays (True, False);

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
                    WHILE BufferStart <> BufferEnd DO
                        BEGIN
                        ActiveKeyer.AddCharacterToBuffer (Buffer [BufferStart]);
                        Inc (BufferStart);
                        IF BufferStart = 256 THEN BufferStart := 0;
                        END;

                    ActiveKeyer.PTTUnForce;
                    RemoveAndRestorePreviousWindow;
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
                                DisplayCodeSpeed (CodeSpeed, CWEnabled, DVPOn, ActiveMode);
                                END;

                        PageDownKey:
                            IF CodeSpeed > 4 THEN
                                BEGIN
                                SetSpeed (CodeSpeed - 3);
                                DisplayCodeSpeed (CodeSpeed, CWEnabled, DVPOn, ActiveMode);
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
    IF DVPEnable THEN
        BEGIN
        GoToXY (1, Hi (WindMax) - 4);
        WriteLn ('Alt-W = Write selected message to DVP');
        WriteLn ('Alt-R = Read selected message from DVP (headphones only)');
        Write   ('');
        END
    ELSE
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
            WriteLn ('You have not enabled your DVP or DVK.');
            WriteLn ('Set DVP ENABLE or DVK ENABLE to TRUE so you can program messages.');
            Write   ('');
            END;
    END;



PROCEDURE ShowCQFunctionKeyStatus;

VAR Key: CHAR;
    TempString: Str160;
    modetmp: modetype;

    BEGIN
    { We use CW CQ Function Keys for Digital }

    if ActiveMode = Digital THEN
        modetmp := CW
    else
        modetmp := ActiveMode;

    GoToXY (1, 1);

    CASE KeyStatus OF
        NormalKeys:
            BEGIN
            WriteLnCenter ('CQ ' + ModeString [modetmp] + ' FUNCTION KEY MEMORY STATUS');

            FOR Key := F1 TO F10 DO
                BEGIN
                Str (Ord (Key) - Ord (F1) + 1, TempString);
                TempString := 'F' + TempString +  ' - ';

                IF GetCQMemoryString (modetmp, Key) <> '' THEN
                    TempString := TempString + GetCQMemoryString (modetmp, Key);

                IF Length (TempString) > 79 THEN
                    TempString := Copy (TempString, 1, 78) + '+';
                ClrEol;

                WriteLnControl (TempString);
                END;
            END;

        AltKeys:
            BEGIN
            WriteLnCenter ('ALT-CQ ' + ModeString [modetmp] + ' FUNCTION KEY MEMORY STATUS');

            FOR Key := AltF1 TO AltF10 DO
                BEGIN
                Str (Ord (Key) - Ord (AltF1) + 1, TempString);
                TempString := 'Alt-F' + TempString +  ' - ';

                IF GetCQMemoryString (modetmp, Key) <> '' THEN {KK1L: 6.73 Added Mode}
                    TempString := TempString + GetCQMemoryString (modetmp, Key); {KK1L: 6.73 Added Mode}

                IF Length (TempString) > 79 THEN
                    TempString := Copy (TempString, 1, 78) + '+';

                ClrEol;
                WriteLnControl(TempString);
                END;
            END;

        ControlKeys:
            BEGIN
            WriteLnCenter ('CONTROL-CQ ' + ModeString [modetmp] + ' FUNCTION KEY MEMORY STATUS');

            FOR Key := ControlF1 TO ControlF10 DO
                BEGIN
                Str (Ord (Key) - Ord (ControlF1) + 1, TempString);
                TempString := 'Ctrl-F' + TempString +  ' - ';

                IF GetCQMemoryString (modetmp, Key) <> '' THEN {KK1L: 6.73 Added mode}
                    TempString := TempString + GetCQMemoryString (modetmp, Key); {KK1L: 6.73 Added mode}

                IF Length (TempString) > 79 THEN
                    TempString := Copy (TempString, 1, 78) + '+';

                ClrEol;
                WriteLnControl(TempString);
                END;
            END;
        END;
    END;



PROCEDURE ShowExFunctionKeyStatus;

VAR Key: CHAR;
    TempString: Str160;

    BEGIN
    GoToXY (1, 1);

    CASE KeyStatus OF
        NormalKeys:
            BEGIN
            WriteLnCenter ('EXCHANGE ' + ModeString [ActiveMode] + ' FUNCTION KEY MEMORY STATUS');

            IF ActiveMode = CW THEN
                BEGIN
                WriteLn ('F1 - Set by the MY CALL statement in config file');
                WriteLn ('F2 - Set by S&P EXCHANGE and REPEAT S&P EXCHANGE');

                FOR Key := F3 TO F10 DO
                    BEGIN
                    Str (Ord (Key) - Ord (F1) + 1, TempString);
                    TempString := 'F' + TempString +  ' - ';

                    IF GetEXMemoryString (CW, Key) <> '' THEN
                        TempString := TempString + GetEXMemoryString (CW, Key);

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

                    IF GetExMemoryString (ActiveMode, Key) <> '' THEN
                        TempString := TempString + GetExMemoryString (ActiveMode, Key);

                    IF Length (TempString) > 79 THEN
                        TempString := Copy (TempString, 1, 78) + '+';

                    ClrEol;
                    WriteLnControl (TempString);
                    END;
            END;

        AltKeys:
            BEGIN
            WriteLnCenter ('ALT-EXCHANGE ' + ModeString [ActiveMode] + ' FUNCTION KEY MEMORY STATUS');

            FOR Key := AltF1 TO AltF10 DO
                BEGIN
                Str (Ord (Key) - Ord (AltF1) + 1, TempString);
                TempString := 'Alt-F' + TempString +  ' - ';

                IF GetExMemoryString (ActiveMode, Key) <> '' THEN
                    TempString := TempString + GetExMemoryString (ActiveMode, Key);

                IF Length (TempString) > 79 THEN
                    TempString := Copy (TempString, 1, 78) + '+';

                ClrEol;
                WriteLnControl (TempString);
                END;
            END;

        ControlKeys:
            BEGIN
            WriteLnCenter ('CONTROL-EXCHANGE FUNCTION KEY MEMORY STATUS');

            FOR Key := ControlF1 TO ControlF10 DO
                BEGIN
                Str (Ord (Key) - Ord (ControlF1) + 1, TempString);
                TempString := 'Ctrl-F' + TempString +  ' - ';

                IF GetExMemoryString (ActiveMode, Key) <> '' THEN
                    TempString := TempString + GetExMemoryString (ActiveMode, Key);

                IF Length (TempString) > 79 THEN
                    TempString := Copy (TempString, 1, 78) + '+';

                ClrEol;
                WriteLnControl (TempString);
                END;
            END;
        END;  { of CASE KeyStatus }
    END;



PROCEDURE ShowOtherMemoryStatus;

VAR TempString: Str160;

    BEGIN
    IF (ActiveMode = CW) OR (ActiveMode = Digital) THEN
        BEGIN
        GoToXY (1, 1);
        WriteLnCenter ('OTHER CW/DIGITAL MESSAGE MEMORY STATUS');

        ClrEol;
        TempString := ' 1. Call Okay Now - ' + CorrectedCallMessage;
        IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
        WriteLnControl (TempString);

        ClrEol;

        IF ActiveMode = CW THEN
            BEGIN
            TempString := ' 2. CQ Exchange   - ' + CQExchange;
            IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
            WriteLnControl (TempString);
            END
        ELSE
            WriteLn ('2. Use Exchange Memory F2 for Digital CQ Exchange');

        ClrEol;
        TempString := ' 3. CQ Ex Name    - ' + CQExchangeNameKnown;
        IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
        WriteLnControl(TempString);

        ClrEol;
        TempString := ' 4. QSL Message   - ' + QSLMessage;
        IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
        WriteLnControl(TempString);

        ClrEol;
        TempString := ' 5. QSO Before    - ' + QSOBeforeMessage;
        IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
        WriteLnControl(TempString);

        ClrEol;
        TempString := ' 6. Quick QSL     - ' + QuickQSLMessage1;
        IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
        WriteLnControl(TempString);

        IF ActiveMode = CW THEN
            BEGIN
            ClrEol;
            TempString := ' 7. Repeat S&P Ex - ' + RepeatSearchAndPounceExchange;
            IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
            WriteLnControl(TempString);
            END
        ELSE
            Write ('7. Use Exchange Memory F3 for REPEAT S&P Exchange');

        IF ActiveMode = CW THEN
            BEGIN
            ClrEol;
            TempString := ' 8. S&P Exchange  - ' + SearchAndPounceExchange;
            IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
            WriteLnControl (TempString);
            END
        ELSE
            WriteLn ('8. Use Exchange Memory F2 for S&P Exchange');

        ClrEol;
        TempString := ' 9. Tail end msg  - ' + TailEndMessage;
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
        WriteLnCenter ('OTHER SSB MESSAGE MEMORY STATUS');

        ClrEol;
        TempString := ' 1. Call Okay Now - ' + CorrectedCallPhoneMessage;
        IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
        WriteLnControl(TempString);

        ClrEol;
        TempString := ' 2. CQ Exchange   - ' + CQPhoneExchange;
        IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
        WriteLnControl(TempString);

        ClrEol;
        TempString := ' 3. CQ Ex Name    - ' + CQPhoneExchangeNameKnown;
        IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
        WriteLnControl(TempString);

        ClrEol;
        TempString := ' 4. QSL Message   - ' + QSLPhoneMessage;
        IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
        WriteLnControl(TempString);

        ClrEol;
        TempString := ' 5. QSO Before    - ' + QSOBeforePhoneMessage;
        IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
        WriteLnControl(TempString);

        ClrEol;
        TempString := ' 6. Quick QSL     - ' + QuickQSLPhoneMessage;
        IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
        WriteLnControl(TempString);

        ClrEol;
        TempString := ' 7. Repeat S&P Ex - ' + RepeatSearchAndPouncePhoneExchange;
        IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
        WriteLnControl(TempString);

        ClrEol;
        TempString := ' 8. S&P Exchange  - ' + SearchAndPouncePhoneExchange;
        IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
        WriteLnControl (TempString);

        ClrEol;
        TempString := ' 9. Tail end msg  - ' + TailEndPhoneMessage;
        IF Length (TempString) > 79 THEN TempString := Copy (TempString, 1, 78) + '+';
        WriteLnControl (TempString);

        ClrEol;
        END;
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
    DVPOn := True;

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
    DVPOn := True;

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



PROCEDURE MemoryProgram;

VAR Key, FirstExchangeFunctionKey, FunctionKey: CHAR;
    TempString: Str160;
    TimeMark: TimeRecord;

    BEGIN
    CASE ActiveMode OF
        Phone, Digital: FirstExchangeFunctionKey := F1;
        CW:             FirstExchangeFunctionKey := F3;
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

        IF ActiveMultiPort <> nil THEN
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

    IF (ActiveMode = CW) OR (ActiveMode = Digital) THEN
        DisplayCrypticCWMenu
    ELSE
        DisplayCrypticSSBMenu;

    VisibleDupeSheetRemoved := True;

    KeyStatus := NormalKeys;

    CASE Key OF

        { C key is for CQ memories }

        'C': BEGIN
             REPEAT
                 ShowCQFunctionKeyStatus;
                 GoToXY (1, Hi (WindMax));
                 Write (' Press CQ function key to program (F1, AltF1, CtrlF1), or ESCAPE to exit) : '); {KK1L: 6.72 changed}

                 MarkTime (TimeMark);

                 REPEAT
                     REPEAT
                         IF ActiveMultiPort <> nil THEN
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
                             ShowCQFunctionKeyStatus;
                             END;
                         END
                     ELSE
                         IF FunctionKey >= ControlF1 THEN
                             BEGIN
                             IF KeyStatus <> ControlKeys THEN
                                 BEGIN
                                 KeyStatus := ControlKeys;
                                 ShowCQFunctionKeyStatus;
                                 END;
                             END
                         ELSE
                             IF KeyStatus <> NormalKeys THEN
                                 BEGIN
                                 KeyStatus := NormalKeys;
                                 ShowCQFunctionKeyStatus;
                                 END;

                     SaveSetAndClearActiveWindow (QuickCommandWindow);

                     REPEAT
                         TempString := LineInput ('Msg = ',
                                              GetCQMemoryString (ActiveMode, FunctionKey),  {KK1L: 6.73 Added mode}
                                              True,
                                              (ActiveMode = Phone) AND (DVPEnable OR DVKEnable));

                         IF TempString [1] = NullKey THEN
                             IF DVPEnable THEN
                                 BEGIN
                                 CASE TempString [2] OF
                                     {KK1L: 6.73 Added mode}
                                     AltW: DVPRecordMessage (GetCQMemoryString (ActiveMode, FunctionKey), False);
                                     {KK1L: 6.73 Added mode}
                                     AltR: DVPListenMessage (GetCQMemoryString (ActiveMode, FunctionKey), True);
                                     END;
                                 END
                             ELSE
                                 BEGIN
                                 IF DVKEnable THEN
                                     CASE TempString [2] OF
                                         {KK1L: 6.73 Added mode}
                                         AltW: DVKRecordMessage (GetCQMemoryString (ActiveMode, FunctionKey));
                                         {KK1L: 6.73 Added mode}
                                         AltR: DVKListenMessage (GetCQMemoryString (ActiveMode, FunctionKey));
                                         END;
                                 END;

                         millisleep;
                     UNTIL (TempString [1] <> NullKey);

                     IF (TempString <> EscapeKey) AND
                        {KK1L: 6.73 Added mode}
                        (GetCQMemoryString (ActiveMode, FunctionKey) <> TempString) THEN
                            BEGIN
                            SetCQMemoryString (ActiveMode, FunctionKey, TempString);

                            IF ActiveMode = Phone THEN
                                AppendConfigFile ('CQ SSB MEMORY ' + KeyId (FunctionKey) + ' = ' + TempString)
                            ELSE
                                AppendConfigFile ('CQ MEMORY ' + KeyId (FunctionKey) + ' = ' + TempString);
                            END;

                     RemoveAndRestorePreviousWindow;
                     END;  { End of legal funtion keys }
             UNTIL False;
             END;   { End of CQ memories }

        'E': REPEAT
                 ShowEXFunctionKeyStatus;
                 GoToXY (1, Hi (WindMax));
                 Write (' Press ex function key to program (F3-F12, Alt/Ctrl F1-F12) or ESCAPE to exit :');
                 {KK1L: 6.72 changed above line}

                 MarkTime (TimeMark);

                 REPEAT
                     REPEAT
                         IF ActiveMultiPort <> nil THEN
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
                             ShowEXFunctionKeyStatus;
                             END;
                         END
                     ELSE
                         IF FunctionKey >= ControlF1 THEN
                             BEGIN
                             IF KeyStatus <> ControlKeys THEN
                                 BEGIN
                                 KeyStatus := ControlKeys;
                                 ShowEXFunctionKeyStatus;
                                 END;
                             END
                         ELSE
                             IF KeyStatus <> NormalKeys THEN
                                 BEGIN
                                 KeyStatus := NormalKeys;
                                 ShowEXFunctionKeyStatus;
                                 END;

                     SaveSetAndClearActiveWindow (QuickCommandWindow);

                     REPEAT
                         TempString := LineInput ('Msg = ',
                                                  {KK1L: 6.73 Added mode to GetExMemoryString}
                                                  GetEXMemoryString (ActiveMode, FunctionKey),
                                                  True,
                                                  (ActiveMode = Phone) AND (DVPEnable OR DVKEnable));

                         IF TempString [1] = NullKey THEN
                             IF DVPEnable THEN
                                 BEGIN
                                 CASE TempString [2] OF
                                     {KK1L: 6.73 Added mode to GetExMemoryString}
                                     AltW: DVPRecordMessage (GetEXMemoryString (ActiveMode, FunctionKey), False);
                                     AltR: DVPListenMessage (GetEXMemoryString (ActiveMode, FunctionKey), True);
                                     END;
                                 END
                             ELSE
                                 IF DVKEnable THEN
                                     CASE TempString [2] OF
                                         {KK1L: 6.73 Added mode to GetExMemoryString}
                                         AltW: DVKRecordMessage (GetEXMemoryString (ActiveMode, FunctionKey));
                                         AltR: DVKListenMessage (GetEXMemoryString (ActiveMode, FunctionKey));
                                         END;

                         millisleep;
                     UNTIL (TempString [1] <> NullKey);

                     IF TempString <> EscapeKey THEN
                         BEGIN
                         SetExMemoryString (ActiveMode, FunctionKey, TempString);

                         CASE ActiveMode OF
                             Phone:
                                 AppendConfigFile ('EX SSB MEMORY ' + KeyId (FunctionKey) + ' = ' + TempString);

                             CW:
                                 AppendConfigFile ('EX MEMORY ' + KeyId (FunctionKey) + ' = ' + TempString);

                             Digital:
                                 AppendConfigFile ('EX DIGIGAL MEMORY' + KeyId (FunctionKey) + ' = ' + TempString);

                             END;  { of CASE ActiveMode }
                         END;

                     RemoveAndRestorePreviousWindow;
                     END;
             UNTIL False;


        'O': REPEAT
                 ShowOtherMemoryStatus;
                 GoToXY (1, Hi (WindMax));

                 Write ('Number or letter of message to be programmed (1-9, A-D, or ESCAPE to exit) : ');

                 MarkTime (TimeMark);

                 REPEAT
                     REPEAT
                         IF ActiveMultiPort <> nil THEN
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
                            IF ActiveMode <> Phone THEN
                                BEGIN
                                TempString := LineInput ('Msg = ',
                                                          CorrectedCallMessage,
                                                          True,
                                                          False);

                                IF TempString <> EscapeKey THEN
                                    BEGIN
                                    CorrectedCallMessage := TempString;
                                    AppendConfigFile ('CALL OK NOW MESSAGE = ' + TempString);
                                    END;
                                END
                            ELSE
                                BEGIN
                                REPEAT
                                    TempString := LineInput ('Msg = ',
                                                             CorrectedCallPhoneMessage,
                                                             True,
                                                             True);


                                    IF TempString [1] = NullKey THEN
                                        IF DVPEnable THEN
                                            BEGIN
                                            CASE TempString [2] OF
                                                AltW: DVPRecordMessage (CorrectedCallPhoneMessage, False);
                                                AltR: DVPListenMessage (CorrectedCallPhoneMessage, True);
                                                END;
                                            END
                                        ELSE
                                            IF DVKEnable THEN
                                                CASE TempString [2] OF
                                                    AltW: DVKRecordMessage (CorrectedCallPhoneMessage);
                                                    AltR: DVKListenMessage (CorrectedCallPhoneMessage);
                                                    END;


                                    millisleep;
                                UNTIL (TempString [1] <> NullKey);

                                IF TempString <> EscapeKey THEN
                                    BEGIN
                                    CorrectedCallPhoneMessage := TempString;
                                    AppendConfigFile ('CALL OK NOW SSB MESSAGE = ' + TempString);
                                    END;
                                END;
                            END;

                       '2': BEGIN
                            IF ActiveMode <> Phone THEN
                                BEGIN
                                TempString := LineInput ('Msg = ', CQExchange, True, False);
                                IF TempString <> EscapeKey THEN
                                    BEGIN
                                    CQExchange := TempString;
                                    AppendConfigFile ('CQ EXCHANGE = ' + TempString);
                                    END;
                                END
                            ELSE
                                BEGIN
                                REPEAT
                                    TempString := LineInput ('Msg = ',
                                                             CQPhoneExchange,
                                                             True,
                                                             True);

                                    IF TempString [1] = NullKey THEN
                                        IF DVPEnable THEN
                                            BEGIN
                                            CASE TempString [2] OF
                                                AltW: DVPRecordMessage (CQPhoneExchange, False);
                                                AltR: DVPListenMessage (CQPhoneExchange, True);
                                                END;
                                            END
                                        ELSE
                                            IF DVKEnable THEN
                                                CASE TempString [2] OF
                                                    AltW: DVKRecordMessage (CQPhoneExchange);
                                                    AltR: DVKListenMessage (CQPhoneExchange);
                                                    END;
                                    millisleep;
                                UNTIL (TempString [1] <> NullKey);

                                IF TempString <> EscapeKey THEN
                                    BEGIN
                                    CQPhoneExchange := TempString;
                                    AppendConfigFile ('CQ SSB EXCHANGE = ' + TempString);
                                    END;
                                END;
                            END;

                       '3': BEGIN
                            IF ActiveMode <> Phone THEN
                                BEGIN
                                TempString := LineInput ('Msg = ', CQExchangeNameKnown, True, False);
                                IF TempString <> EscapeKey THEN
                                    BEGIN
                                    CQExchangeNameKnown := TempString;
                                    AppendConfigFile ('CQ EXCHANGE NAME KNOWN = ' + TempString);
                                    END;
                                END
                            ELSE
                                BEGIN
                                REPEAT
                                    TempString := LineInput ('Msg = ',
                                                             CQPhoneExchangeNameKnown,
                                                             True,
                                                             True);

                                    IF TempString [1] = NullKey THEN
                                        IF DVPEnable THEN
                                            BEGIN
                                            CASE TempString [2] OF
                                                AltW: DVPRecordMessage (CQPhoneExchangeNameKnown, False);
                                                AltR: DVPListenMessage (CQPhoneExchangeNameKnown, True);
                                                END;
                                            END
                                        ELSE
                                            IF DVKEnable THEN
                                                CASE TempString [2] OF
                                                    AltW: DVKRecordMessage (CQPhoneExchangeNameKnown);
                                                    AltR: DVKListenMessage (CQPhoneExchangeNameKnown);
                                                    END;
                                    millisleep;
                                UNTIL (TempString [1] <> NullKey);

                                IF TempString <> EscapeKey THEN
                                    BEGIN
                                    CQPhoneExchangeNameKnown := TempString;
                                    AppendConfigFile ('CQ SSB EXCHANGE NAME KNOWN = ' + TempString);
                                    END;
                                END;
                            END;

                       '4': BEGIN
                            IF ActiveMode <> Phone THEN
                                BEGIN
                                TempString := LineInput ('Msg = ', QSLMessage, True, False);
                                IF TempString <> EscapeKey THEN
                                    BEGIN
                                    QSLMessage := TempString;
                                    AppendConfigFile ('QSL MESSAGE = ' + TempString);
                                    END;
                                END
                            ELSE
                                BEGIN
                                REPEAT
                                    TempString := LineInput ('Msg = ',
                                                             QSLPhoneMessage,
                                                             True, True);

                                    IF TempString [1] = NullKey THEN
                                        IF DVPEnable THEN
                                            BEGIN
                                            CASE TempString [2] OF
                                                AltW: DVPRecordMessage (QSLPhoneMessage, False);
                                                AltR: DVPListenMessage (QSLPhoneMessage, True);
                                                END;
                                            END
                                        ELSE
                                            IF DVKEnable THEN
                                                CASE TempString [2] OF
                                                    AltW: DVKRecordMessage (QSLPhoneMessage);
                                                    AltR: DVKListenMessage (QSLPhoneMessage);
                                                    END;
                                    millisleep;
                                UNTIL (TempString [1] <> NullKey);

                                IF TempString <> EscapeKey THEN
                                    BEGIN
                                    QSLPhoneMessage := TempString;
                                    AppendConfigFile ('QSL SSB MESSAGE = ' + TempString);
                                    END;
                                END;
                            END;

                       '5': BEGIN
                            IF ActiveMode <> Phone THEN
                                BEGIN
                                TempString := LineInput ('Msg = ', QSOBeforeMessage, True, False);
                                IF TempString <> EscapeKey THEN
                                    BEGIN
                                    QSOBeforeMessage := TempString;
                                    AppendConfigFile ('QSO BEFORE MESSAGE = ' + TempString);
                                    END;
                                END
                            ELSE
                                BEGIN
                                REPEAT
                                    TempString := LineInput ('Msg = ',
                                                             QSOBeforePhoneMessage,
                                                             True, True);

                                    IF TempString [1] = NullKey THEN
                                        IF DVPEnable THEN
                                            BEGIN
                                            CASE TempString [2] OF
                                                AltW: DVPRecordMessage (QSOBeforePhoneMessage, False);
                                                AltR: DVPListenMessage (QSOBeforePhoneMessage, True);
                                                END;
                                            END
                                        ELSE
                                            IF DVKEnable THEN
                                                CASE TempString [2] OF
                                                    AltW: DVKRecordMessage (QSOBeforePhoneMessage);
                                                    AltR: DVKListenMessage (QSOBeforePhoneMessage);
                                                    END;
                                    millisleep;
                                UNTIL (TempString [1] <> NullKey);

                                IF TempString <> EscapeKey THEN
                                    BEGIN
                                    QSOBeforePhoneMessage := TempString;
                                    AppendConfigFile ('QSO BEFORE SSB MESSAGE = ' + TempString);
                                    END;
                                END;
                            END;

                       '6': BEGIN
                            IF ActiveMode <> Phone THEN
                                BEGIN
                                TempString := LineInput ('Msg = ', QuickQSLMessage1, True, False);
                                IF TempString <> EscapeKey THEN
                                    BEGIN
                                    QuickQSLMessage1 := TempString;
                                    AppendConfigFile ('QUICK QSL MESSAGE= ' + TempString);
                                    END;
                                END
                            ELSE
                                BEGIN
                                REPEAT
                                    TempString := LineInput ('Msg = ',
                                                             QuickQSLPhoneMessage,
                                                             True, True);

                                    IF TempString [1] = NullKey THEN
                                        IF DVPEnable THEN
                                            BEGIN
                                            CASE TempString [2] OF
                                                AltW: DVPRecordMessage (QuickQSLPhoneMessage, False);
                                                AltR: DVPListenMessage (QuickQSLPhoneMessage, True);
                                                END;
                                            END
                                        ELSE
                                            IF DVKEnable THEN
                                                CASE TempString [2] OF
                                                    AltW: DVKRecordMessage (QuickQSLPhoneMessage);
                                                    AltR: DVKListenMessage (QuickQSLPhoneMessage);
                                                    END;
                                    millisleep;
                                UNTIL (TempString [1] <> NullKey);

                                IF TempString <> EscapeKey THEN
                                    BEGIN
                                    QuickQSLPhoneMessage := TempString;
                                    AppendConfigFile ('QUICK QSL SSB MESSAGE = ' + TempString);
                                    END;
                                END;
                            END;

                       '7': BEGIN
                            IF ActiveMode <> Phone THEN
                                BEGIN
                                TempString := LineInput ('Msg = ', RepeatSearchAndPounceExchange, True, False);
                                IF TempString <> EscapeKey THEN
                                    BEGIN
                                    RepeatSearchAndPounceExchange := TempString;
                                    AppendConfigFile ('REPEAT S&P EXCHANGE = ' + TempString);
                                    END;
                                END
                            ELSE
                                BEGIN
                                REPEAT
                                    TempString := LineInput ('Msg = ',
                                                             RepeatSearchAndPouncePhoneExchange,
                                                             True, True);

                                    IF TempString [1] = NullKey THEN
                                        IF DVPEnable THEN
                                            BEGIN
                                            CASE TempString [2] OF
                                                AltW: DVPRecordMessage (RepeatSearchAndPouncePhoneExchange, False);
                                                AltR: DVPListenMessage (RepeatSearchAndPouncePhoneExchange, True);
                                                END;
                                            END
                                        ELSE
                                            IF DVKEnable THEN
                                                CASE TempString [2] OF
                                                    AltW: DVKRecordMessage (RepeatSearchAndPouncePhoneExchange);
                                                    AltR: DVKListenMessage (RepeatSearchAndPouncePhoneExchange);
                                                    END;
                                    millisleep;
                                UNTIL (TempString [1] <> NullKey);

                                IF TempString <> EscapeKey THEN
                                    BEGIN
                                    RepeatSearchAndPouncePhoneExchange := TempString;
                                    AppendConfigFile ('REPEAT S&P SSB EXCHANGE = ' + TempString);
                                    END;
                                END;
                            END;

                       '8': BEGIN
                            IF ActiveMode <> Phone THEN
                                BEGIN
                                TempString := LineInput ('Msg = ', SearchAndPounceExchange, True, False);
                                IF TempString <> EscapeKey THEN
                                    BEGIN
                                    SearchAndPounceExchange := TempString;
                                    AppendConfigFile ('S&P EXCHANGE = ' + TempString);
                                    END;
                                END
                            ELSE
                                BEGIN
                                REPEAT
                                    TempString := LineInput ('Msg = ',
                                                             SearchAndPouncePhoneExchange,
                                                             True, True);

                                    IF TempString [1] = NullKey THEN
                                        IF DVPEnable THEN
                                            BEGIN
                                            CASE TempString [2] OF
                                                AltW: DVPRecordMessage (SearchAndPouncePhoneExchange, False);
                                                AltR: DVPListenMessage (SearchAndPouncePhoneExchange, True);
                                                END;
                                            END
                                        ELSE
                                            IF DVKEnable THEN
                                                CASE TempString [2] OF
                                                    AltW: DVKRecordMessage (SearchAndPouncePhoneExchange);
                                                    AltR: DVKListenMessage (SearchAndPouncePhoneExchange);
                                                    END;
                                    millisleep;
                                UNTIL (TempString [1] <> NullKey);

                                IF TempString <> EscapeKey THEN
                                    BEGIN
                                    SearchAndPouncePhoneExchange := TempString;
                                    AppendConfigFile ('S&P SSB EXCHANGE = ' + TempString);
                                    END;
                                END;
                            END;

                       '9': BEGIN
                            IF ActiveMode <> Phone THEN
                                BEGIN
                                TempString := LineInput ('Msg = ', TailEndMessage, True, False);
                                IF TempString <> EscapeKey THEN
                                    BEGIN
                                    TailEndMessage := TempString;
                                    AppendConfigFile ('TAIL END MESSAGE = ' + TempString);
                                    END;
                                END
                            ELSE
                                BEGIN
                                REPEAT
                                    TempString := LineInput ('Msg = ',
                                                             TailEndPhoneMessage,
                                                             True, True);

                                    IF TempString [1] = NullKey THEN
                                        IF DVPEnable THEN
                                            BEGIN
                                            CASE TempString [2] OF
                                                AltW: DVPRecordMessage (TailEndPhoneMessage, False);
                                                AltR: DVPListenMessage (TailEndPhoneMessage, True);
                                                END;
                                            END
                                        ELSE
                                            IF DVKEnable THEN
                                                CASE TempString [2] OF
                                                    AltW: DVKRecordMessage (TailEndPhoneMessage);
                                                    AltR: DVKListenMessage (TailEndPhoneMessage);
                                                    END;
                                    millisleep;
                                UNTIL (TempString [1] <> NullKey);

                                IF TempString <> EscapeKey THEN
                                    BEGIN
                                    TailEndPhoneMessage := TempString;
                                    AppendConfigFile ('TAIL END SSB MESSAGE = ' + TempString);
                                    END;
                                END;
                            END;

                       'A': IF ActiveMode <> Phone THEN
                                BEGIN
                                TempString := LineInput ('Enter character for short zeros : ', '', True, False);

                                IF (TempString <> EscapeKey) AND (TempString <> '') THEN
                                    BEGIN
                                    Short0 := TempString [1];
                                    AppendConfigFile ('SHORT 0 = ' + Short0);
                                    END;
                                END;

                       'B': IF ActiveMode <> Phone THEN
                                BEGIN
                                TempString := LineInput ('Enter character for short ones : ', '', True, False);

                                IF (TempString <> EscapeKey) AND (TempString <> '') THEN
                                    BEGIN
                                    Short1 := TempString [1];
                                    AppendConfigFile ('SHORT 1 = ' + Short1);
                                    END;
                                END;

                       'C': IF ActiveMode <> Phone THEN
                                BEGIN
                                TempString := LineInput ('Enter character for short twos : ', '', True, False);

                                IF (TempString <> EscapeKey) AND (TempString <> '') THEN
                                    BEGIN
                                    Short2 := TempString [1];
                                    AppendConfigFile ('SHORT 2 = ' + Short2);
                                    END;
                                END;

                       'D': IF ActiveMode <> Phone THEN
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



FUNCTION GetCQMemoryString (Mode: ModeType; Key: CHAR): Str80; {KK1L: 6.73 Added Mode to do split mode}

    BEGIN
    IF Mode = Digital THEN Mode := CW;

    IF CQMemory [Mode, Key] <> nil THEN
        GetCQMemoryString := CQMemory [Mode, Key]^
    ELSE
        GetCQMemoryString := '';
    END;


FUNCTION GetEXMemoryString (Mode: ModeType; Key: CHAR): Str80; {KK1L: 6.73 Added Mode to do split mode}

    BEGIN
    GetEXMemoryString := '';

    CASE MODE OF
        CW, Phone:
            IF EXMemory [Mode, Key] <> Nil THEN
                GetEXMemoryString := EXMemory [Mode, Key]^;

        { For Digital, use Digital EX memory if there - otherwise, use CW }

        Digital:
            IF ExMemory [Mode, Key] <> Nil then
                GetEXMemoryString := EXMemory [Mode, Key]^
            ELSE
                IF ExMemory [CW, Key] <> Nil THEN
                    GetExMemoryString := ExMemory [CW, Key]^;

        END;  { of case }
    END;


PROCEDURE SetCQMemoryString (Mode: ModeType; Key: CHAR; MemoryString: Str80);

    BEGIN
    { All digital CQ strings go to the CW strings }

    IF Mode = Digital THEN Mode := CW;

    IF CQMemory [Mode, Key] = Nil THEN New (CQMemory [Mode, Key]);
    SniffOutControlCharacters (MemoryString); {KK1L: 6.72}
    CQMemory [Mode, Key]^ := MemoryString;
    END;


PROCEDURE SetEXMemoryString (Mode: ModeType; Key: CHAR; MemoryString: Str80);

{ Must use digital mode - not CW instead }

    BEGIN
    IF EXMemory [Mode, Key] = Nil THEN New (EXMemory [Mode, Key]);
    SniffOutControlCharacters (MemoryString);
    EXMemory [Mode, Key]^ := MemoryString;
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

VAR TimeOut: BYTE;

    BEGIN
    IF (ActiveMode = Phone) AND DVPEnable AND DVPActive AND DVPMessagePlaying THEN
        BEGIN
        TimeOut := 0;

        DVPStopPlayback;

        REPEAT
            Wait (5);
            Inc (TimeOut);
        UNTIL (NOT DVPMessagePlaying) OR (TimeOut > 50);
        END;

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

VAR TimeOut: BYTE;

    BEGIN
    IF KeyersSwapped THEN Exit;        { Already swapped to inactive rig }

    IF (ActiveMode = Phone) AND DVPEnable AND DVPActive AND DVPMessagePlaying THEN
        BEGIN
        TimeOut := 0;

        DVPStopPlayback;

        REPEAT
            Wait (5);
            Inc (TimeOut);
        UNTIL (NOT DVPMessagePlaying) OR (TimeOut > 50);
        END;

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
        END
    ELSE
        IF DVPEnable OR DVKEnable THEN
            BEGIN
            DVPOn := NOT DVPOn;

            IF DisplayPrompt THEN
               if DVPOn then
                QuickDisplay ('Voice keyer enabled with Alt-K!!  Use Alt-K again to disable.')
              else
                QuickDisplay ('Voice keyer disabled with Alt-K!!  Use Alt-K again to enable.');
            END;

    DisplayCodeSpeed (CodeSpeed, CWEnabled, DVPOn, ActiveMode);
    SetSpeed (CodeSpeed);
    END;



PROCEDURE CWInit;

VAR TempKey: CHAR;

    BEGIN
    FOR TempKey := F1 TO AltF12 DO
        BEGIN
        CQMemory [CW, TempKey]      := Nil;
        EXMemory [CW, TempKey]      := Nil;
        CQMemory [Phone, TempKey]   := Nil;
        EXMemory [Phone, TempKey]   := Nil;
        EXMemory [Digital, TempKey] := Nil;
        END;

    AutoCQMemory := NullCharacter;
    KeyPressedMemory := Chr (0);
    KeyersSwapped := False;
    LastRSTSent := '';
    CWMessageCommand := NoCWCommand;
    NeedToSetCQMode := False; {KK1L: 6.69 This variable is used to leap around some AutoS&PMode code.}
    RTTYTransmissionStarted := False;
    SendingOnRadioOne := False;
    SendingOnRadioTwo := False;
    END;


    BEGIN
    CWInit;
    END.
