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

{ Some stuff called from here but contained in LOGSUBS2 }

PROCEDURE CheckMultiState; FORWARD;


PROCEDURE PutUpCQMenu;

    BEGIN
    SaveSetAndClearActiveWindow (FunctionKeyWindow);
    Write (CQMenu);
    RestorePreviousWindow;
    END;



PROCEDURE PossibleCallCursorRight;

    BEGIN
    WITH ClassicPossibleCallList DO
        IF (NumberCalls > 0) AND (CursorPosition < NumberCalls - 1) THEN
            BEGIN
            Inc (CursorPosition);
            DisplayPossibleCalls (ClassicPossibleCallList);
            END;
    END;


PROCEDURE PossibleCallCursorLeft;

    BEGIN
    WITH ClassicPossibleCallList DO
        IF (NumberCalls > 0) AND (CursorPosition > 0) THEN
            BEGIN
            Dec (CursorPosition);
            DisplayPossibleCalls (ClassicPossibleCallList);
            END;
    END;


PROCEDURE SendCrypticDigitalStringToK3 (SendString: Str160);

{ Doesn't send the characters one by one.  }

VAR StringPointer, QSONumber: INTEGER;
    Result, Offset: INTEGER;
    SendChar, TempChar: CHAR;
    TempCall: CallString;
    TempString: Str80;
    OutputString: STRING;

    BEGIN
    OutputString := '';

    StringPointer := 1;   { Point to first character }

    WHILE StringPointer <= Length (SendString) DO
        BEGIN
        SendChar := SendString [StringPointer];

        CASE SendChar OF
            '#': BEGIN
                 IF QSONumberForThisQSO <= 0 THEN  { We don't have a QSO number yet }
                     BEGIN
                     QSONumberForThisQSO := ReserveNextQSONumber (ActiveBand);

                     { If we are getting the QSO from the multi network, we will need to wait
                       for it to show up before we can send it. }

                     IF MultiRequestQSONumber THEN
                         BEGIN
                         REPEAT
                             CheckMultiState;
                             UpdateTimeAndRateDisplays (True, True);
                         UNTIL QSONumberFromNetwork > 0;

                         QSONumberForThisQSO := QSONumberFromNetwork;
                         END;

                     DisplayQSONumber (QSONumberForThisQSO, ActiveBand);
                     END;

                 QSONumber := QSONumberForThisQSO;  { So the code below is unchanged }

                 IF TailEnding THEN Inc (QSONumber);

                 { We automatically decrement the QSO number if repeating w/o new call }

                 IF AutoQSONumberDecrement THEN
                     IF (ActiveWindow = CallWindow) AND
                        (CallWindowString = '') AND (ExchangeWindowString = '') THEN
                            Dec (QSONumber);

                 { Peek ahead and see if we have a + or - character }

                 IF Length (SendString) >= StringPointer + 1THEN
                     BEGIN
                     TempChar := SendString [StringPointer + 1];

                     IF TempChar = '+' THEN
                         BEGIN
                         TempChar := SendString [StringPointer + 2];
                         Val (TempChar, Offset, Result);

                         IF Result = 0 THEN
                             BEGIN
                             QSONumber := QSONumber + Offset;
                             StringPointer := StringPointer + 2;   { Skip over two additonal chars }
                             END;
                         END;

                     IF TempChar = '-' THEN
                         BEGIN
                         TempChar := SendString [StringPointer + 2];
                         Val (TempChar, Offset, Result);

                         IF Result = 0 THEN
                             BEGIN
                             QSONumber := QSONumber - Offset;
                             StringPointer := StringPointer + 2;   { Skip over two additional chars }
                             END;
                         END;
                     END;

                 TempString := QSONumberString (QSONumber);

                 OutputString := OutputString + TempString;

                 WHILE LeadingZeros > Length (TempString) DO
                     TempString := LeadingZeroCharacter + TempString;

                 OutputString := OutputString + TempString;
                 END;

            '@': BEGIN
                 IF CallsignUpdateEnable THEN
                     BEGIN
                     TempString := GetCorrectedCallFromExchangeString (ExchangeWindowString);

                     IF TempString <> '' THEN
                         BEGIN
                         CallWindowString := TempString;
                         CallsignICameBackTo := TempString;
                         END;
                     END;

                IF CallWindowString <> '' THEN
                     OutputString := OutputString + CallWindowString;
                END;

            ':': SendKeysToRTTY;

            '\': OutputString := OutputString + MyCall;

            '|': IF ReceivedData.Name <> '' THEN
                     ContinueRTTYTransmission (ReceivedData.Name + ' ');

            '{': OutputString := OutputString + ReceivedData.Callsign;

            '>': ClearRIT;

            ')': OutputString := OutputString + VisibleLog.LastCallsign;

            ControlW: OutputString := OutputString + VisibleLog.LastName (4);

            ControlR: BEGIN
                      ReceivedData.RandomCharsSent := '';

                      REPEAT
                          ReceivedData.RandomCharsSent :=
                            ReceivedData.RandomCharsSent +
                            Chr (Random (25) + Ord ('A'));
                      UNTIL Length (ReceivedData.RandomCharsSent) = 5;

                      OutputString := OutputString + ReceivedData.RandomCharsSent;

                      SaveSetAndClearActiveWindow (DupeInfoWindow);
                      Write ('Sent = ', ReceivedData.RandomCharsSent);
                      RestorePreviousWindow;
                      END;

            ControlT: OutputString := OutputString + ReceivedData.RandomCharsSent;

            ControlU: BEGIN
                      TempCall := GetCorrectedCallFromExchangeString (ExchangeWindowString);

                      IF TempCall <> '' THEN
                          CallSignICameBackTo := TempString
                      ELSE
                          CallsignICameBackTo := CallWindowString;

                      ShowStationInformation (CallsignICameBackTo);
                      END;

            ELSE OutputString := OutputString + SendChar;
            END;  { of case SendChar }

        Inc (StringPointer);
        END;

    FinishRTTYTransmission (OutputString);
    END;



PROCEDURE SendCrypticDigitalString (SendString: Str160);

{ Control-A will put the message out on the InactiveRadio and set the flag
  InactiveRadioSendingCW.  It does not change the ActiveRadio any more.

  If you decide to answer someone who responds to the inactive radio,
  you will want to call SwapRadios.  This will now make Control-A messages
  be sent on the new inactive radio (which is probably what you want).   }


VAR CharacterCount, QSONumber: INTEGER;
    cc: integer;
    Result, Offset: INTEGER;
    Key, SendChar, TempChar: CHAR;
    TempCall: CallString;
    WarningSounded: BOOLEAN;
    TempString: Str80;

    BEGIN
    IF Length (SendString) = 0 THEN Exit;

    CASE ActiveRadio OF
        RadioOne:
            IF (Radio1Type = K2) OR (Radio1Type = K3) OR (Radio1Type = K4) THEN
                BEGIN
                SendCrypticDigitalStringToK3 (SendString);
                Exit;
                END;

        RadioTwo:
            IF (Radio2Type = K2) OR (Radio2Type = K3) OR (Radio2Type = K4) THEN
                BEGIN
                SendCrypticDigitalStringToK3 (SendString);
                Exit;
                END;

        END;  { of case }

    { Legacy stuff }

    IF NOT RTTYTransmissionStarted THEN
        StartRTTYTransmission ('');

    cc := 0;

    FOR CharacterCount := 1 TO Length (SendString) DO
        BEGIN
        cc := cc + 1;
        if CharacterCount < cc then continue;
        SendChar := SendString [CharacterCount];

        CASE SendChar OF
            '#': BEGIN
                 IF QSONumberForThisQSO <= 0 THEN  { We don't have a QSO number yet }
                     BEGIN
                     QSONumberForThisQSO := ReserveNextQSONumber (ActiveBand);

                     { If we are getting the QSO from the multi network, we will need to wait
                       for it to show up before we can send it. }

                     IF MultiRequestQSONumber THEN
                         BEGIN
                         REPEAT
                             CheckMultiState;
                             UpdateTimeAndRateDisplays (True, True);
                         UNTIL QSONumberFromNetwork > 0;

                         QSONumberForThisQSO := QSONumberFromNetwork;
                         END;

                     DisplayQSONumber (QSONumberForThisQSO, ActiveBand);
                     END;

                 QSONumber := QSONumberForThisQSO;  { So the code below is unchanged }

                 IF TailEnding THEN Inc (QSONumber);

                 IF AutoQSONumberDecrement THEN
                     IF (ActiveWindow = CallWindow) AND
                        (CallWindowString = '') AND (ExchangeWindowString = '') THEN
                            Dec (QSONumber);

                 IF Length (SendString) >= CharacterCount + 2 THEN
                     BEGIN
                     TempChar := SendString [CharacterCount + 1];

                     IF TempChar = '+' THEN
                         BEGIN
                         TempChar := SendString [CharacterCount + 2];
                         Val (TempChar, Offset, Result);
                         IF Result = 0 THEN
                             BEGIN
                             QSONumber := QSONumber + Offset;
//                             CharacterCount := CharacterCount + 2;
                             cc := cc + 2;
                             END;
                         END;

                     IF TempChar = '-' THEN
                         BEGIN
                         TempChar := SendString [CharacterCount + 2];
                         Val (TempChar, Offset, Result);
                         IF Result = 0 THEN
                             BEGIN
                             QSONumber := QSONumber - Offset;
//                             CharacterCount := CharacterCount + 2;
                             cc := cc + 2;
                             END;
                         END;
                     END;

                 TempString := QSONumberString (QSONumber);

                 WHILE LeadingZeros > Length (TempString) DO
                     TempString := LeadingZeroCharacter + TempString;

                 ContinueRTTYTransmission (TempString);
                 END;

            '*': BEGIN {KK1L: 6.72 New character to send Alt-D dupe checked call or call in call window}
                 IF (DupeInfoCall <> '') AND (DupeInfoCall <> EscapeKey) THEN
                     AddStringToBuffer (DupeInfoCall, CWTone)
                 ELSE
                     BEGIN
                     IF (CallsignUpdateEnable) AND (TempString <> '') THEN
                         BEGIN
                         TempString := GetCorrectedCallFromExchangeString (ExchangeWindowString);

                         IF TempString <> '' THEN
                             BEGIN
                             CallWindowString := TempString;
                             CallsignICameBackTo := TempString;
                             END;
                         END;

                     IF CallWindowString <> '' THEN
                         ContinueRTTYTransmission (CallWindowString);
                     END;
                 END;

            '@': BEGIN
                 IF CallsignUpdateEnable THEN
                     BEGIN
                     TempString := GetCorrectedCallFromExchangeString (ExchangeWindowString);

                     IF TempString <> '' THEN
                         BEGIN
                         CallWindowString := TempString;
                         CallsignICameBackTo := TempString;
                         END;
                     END;

                IF CallWindowString <> '' THEN
                     ContinueRTTYTransmission (CallWindowString);
                END;

            ':': BEGIN
                 SendKeysToRTTY;
                 END;

            '\': ContinueRTTYTransmission (MyCall);

            '|': IF ReceivedData.Name <> '' THEN
                     ContinueRTTYTransmission (ReceivedData.Name + ' ');

            '[': BEGIN
                 WarningSounded := False;

                 QuickDisplay ('WAITING FOR YOU ENTER STRENGTH OF RST (Single digit)!!');

                 ContinueRTTYTransmission ('5');

                 Key := '0';

                 REPEAT
                     REPEAT
                         IF NOT CWStillBeingSent THEN
                             BEGIN
                             IF NOT WaitForStrength THEN
                                 BEGIN
                                 Key := '9';
                                 Break;
                                 END
                             ELSE
                                 IF NOT WarningSounded THEN
                                     BEGIN
                                     WarningSounded := True;
                                     Tone.DoABeep (ThreeHarmonics);
                                     END;
                             END;

                     UNTIL KeyPressed;

                     IF Key <> '9' THEN Key := ReadKey;

                 UNTIL ((Key >= '1') AND (Key <= '9')) OR (Key = EscapeKey);

                 IF Key = EscapeKey THEN
                     BEGIN
                     FinishRTTYTransmission ('');
                     Activerttyport.putchar(chr(27));
                     Exit;
                     END;

                 IF Key = '9' THEN
                     ContinueRTTYTransmission ('NN')
                 ELSE
                     ContinueRTTYTransmission (Key + 'N');

                 ReceivedData.RSTSent := '5' + Key + '9';
                 LastRSTSent := ReceivedData.RSTSent;
                 END;

            ']': ContinueRTTYTransmission (LastRSTSent);

            '{': ContinueRTTYTransmission (ReceivedData.Callsign);

            '>': ClearRIT;

            ')': ContinueRTTYTransmission (VisibleLog.LastCallsign);

            ControlW: ContinueRTTYTransmission (VisibleLog.LastName (4));

            ControlR: BEGIN
                      ReceivedData.RandomCharsSent := '';

                      REPEAT
                          ReceivedData.RandomCharsSent :=
                            ReceivedData.RandomCharsSent +
                            Chr (Random (25) + Ord ('A'));
                      UNTIL Length (ReceivedData.RandomCharsSent) = 5;

                      ContinueRTTYTransmission (ReceivedData.RandomCharsSent);

                      SaveSetAndClearActiveWindow (DupeInfoWindow);
                      Write ('Sent = ', ReceivedData.RandomCharsSent);
                      RestorePreviousWindow;
                      END;

            ControlT: ContinueRTTYTransmission (ReceivedData.RandomCharsSent);

            ControlU: BEGIN
                      TempCall := GetCorrectedCallFromExchangeString (ExchangeWindowString);

                      IF TempCall <> '' THEN
                          CallSignICameBackTo := TempString
                      ELSE
                          CallsignICameBackTo := CallWindowString;

                      ShowStationInformation (CallsignICameBackTo);
                      END;

            ELSE ContinueRTTYTransmission (SendChar);
            END;
        END;

    FinishRTTYTransmission ('');
    END;



PROCEDURE SendDVKMessage (Message: Str20);

    BEGIN
    Message := UpperCase (Message);

    IF (Message = 'DVK0') OR DVKMessagePlaying THEN {KK1L: 6.71 If already playing then stop it first}
        BEGIN
        StartDVK (0);
        DVKPlaying := False;
        END;

    IF Message = 'DVK1' THEN
        BEGIN
        StartDVK (1);
        DVKStamp;
        END;

    IF Message = 'DVK2' THEN
        BEGIN
        StartDVK (2);
        DVKStamp;
        END;

    IF Message = 'DVK3' THEN
        BEGIN
        StartDVK (3);
        DVKStamp;
        END;

    IF Message = 'DVK4' THEN
        BEGIN
        StartDVK (4);
        DVKStamp;
        END;

    IF Message = 'DVK5' THEN {KK1L: 6.71}
        BEGIN
        StartDVK (5);
        DVKStamp;
        END;

    IF Message = 'DVK6' THEN {KK1L: 6.71}
        BEGIN
        StartDVK (6);
        DVKStamp;
        END;
    END;



PROCEDURE SendCrypticCWString (SendString: Str160);

{ Control-A will put the message out on the InactiveRadio and set the flag
  InactiveRadioSendingCW.  It does not change the ActiveRadio any more.

  If you decide to answer someone who responds to CW on the inactive radio,
  you will want to call SwapRadios.  This will now make Control-A messages
  be sent on the new inactive radio (which is probably what you want).   }


VAR CharPointer, CharacterCount, QSONumber: INTEGER;
    cc: integer;
    Result, Offset: INTEGER;
    Key, SendChar, TempChar: CHAR;
    CommandMode, WarningSounded: BOOLEAN;
    TempCall: CallString;
    TempString: Str80;
    TempFreq: LONGINT;

    BEGIN
    { If you are trying to find where the PTT is dropping out after finishing
      an auto start send call and before the exchange is being sent - it is
      before here. }

    SetSpeed (DisplayedCodeSpeed);

    IF Length (SendString) = 0 THEN Exit;

    CommandMode := False;

//ugly patch to fix original code incrementing the for loop variable

    cc := 0;
    FOR CharacterCount := 1 TO Length (SendString) DO
        BEGIN
        cc := cc + 1;
        if CharacterCount < cc then continue;
        SendChar := SendString [CharacterCount];

        IF CommandMode THEN
            BEGIN
            CASE SendChar OF

                '@': IF StringHas (CallWindowString, '?') THEN
                         AddStringToBuffer (' ' + CallWindowString, CWTone);

                ELSE AddStringToBuffer (ControlLeftBracket + SendChar, CWTone);
                END;

            CommandMode := False;
            Continue;
            END;

        CASE SendChar OF
            '#': BEGIN
                 IF QSONumberForThisQSO <= 0 THEN  { We don't have a QSO number yet }
                     BEGIN
                     QSONumberForThisQSO := ReserveNextQSONumber (ActiveBand);

                     { If we are getting the QSO from the multi network, we will need to wait
                       for it to show up before we can send it. }

                     IF MultiRequestQSONumber THEN
                         BEGIN
                         REPEAT
                             CheckMultiState;
                             UpdateTimeAndRateDisplays (True, True);
                         UNTIL QSONumberFromNetwork > 0;

                         QSONumberForThisQSO := QSONumberFromNetwork;
                         END;

                     DisplayQSONumber (QSONumberForThisQSO, ActiveBand);
                     END;

                 QSONumber := QSONumberForThisQSO;  { So the code below is unchanged }

                 IF TailEnding THEN Inc (QSONumber);

                 IF AutoQSONumberDecrement THEN
                     IF (ActiveWindow = CallWindow) AND
                        (CallWindowString = '') AND (ExchangeWindowString = '') THEN
                            Dec (QSONumber);

                 IF Length (SendString) >= CharacterCount + 2 THEN
                     BEGIN
                     TempChar := SendString [CharacterCount + 1];

                     IF TempChar = '+' THEN
                         BEGIN
                         TempChar := SendString [CharacterCount + 2];
                         Val (TempChar, Offset, Result);
                         IF Result = 0 THEN
                             BEGIN
                             QSONumber := QSONumber + Offset;
//                             CharacterCount := CharacterCount + 2;
                             cc := cc + 2;
                             END;
                         END;

                     IF TempChar = '-' THEN
                         BEGIN
                         TempChar := SendString [CharacterCount + 2];
                         Val (TempChar, Offset, Result);
                         IF Result = 0 THEN
                             BEGIN
                             QSONumber := QSONumber - Offset;
//                             CharacterCount := CharacterCount + 2;
                             cc := cc + 2;
                             END;
                         END;
                     END;

                 TempString := QSONumberString (QSONumber);

                 WHILE LeadingZeros > Length (TempString) DO
                     TempString := LeadingZeroCharacter + TempString;

                 IF ShortIntegers THEN
                     FOR CharPointer := 1 TO Length (TempString) DO
                         BEGIN
                         IF TempString [CharPointer] = '0' THEN TempString [CharPointer] := Short0;
                         IF TempString [CharPointer] = '1' THEN TempString [CharPointer] := Short1;
                         IF TempString [CharPointer] = '2' THEN TempString [CharPointer] := Short2;
                         IF TempString [CharPointer] = '9' THEN TempString [CharPointer] := Short9;
                         END;

                 AddStringToBuffer (TempString, CWTone);
                 END;

            '_': AddStringToBuffer (' ', CWTone);

            ControlD: IF CWStillBeingSent THEN AddStringToBuffer (' ', CWTone);

            '*': BEGIN {KK1L: 6.72 New character to send Alt-D dupe checked call or call in call window}
                 IF (DupeInfoCall <> '') AND (DupeInfoCall <> EscapeKey) THEN
                     AddStringToBuffer (DupeInfoCall, CWTone)
                 ELSE
                     BEGIN
                     IF CallsignUpdateEnable THEN
                         BEGIN
                         TempString := GetCorrectedCallFromExchangeString (ExchangeWindowString);

                         IF TempString <> '' THEN
                             BEGIN
                             CallWindowString := TempString;
                             CallsignICameBackTo := TempString;
                             END;
                         END;

                     IF CallWindowString <> '' THEN
                         AddStringToBuffer (CallWindowString, CWTone);
                     END;
                 END;

            '@': BEGIN
                 IF CallsignUpdateEnable THEN
                     BEGIN
                     TempString := GetCorrectedCallFromExchangeString (ExchangeWindowString);

                     IF TempString <> '' THEN
                         BEGIN
                         CallWindowString := TempString;
                         CallsignICameBackTo := TempString;
                         END;
                     END;

                IF CallWindowString <> '' THEN
                         AddStringToBuffer (CallWindowString, CWTone);
                END;

            '$': IF SayHiEnable AND (Rate < SayHiRateCutoff) THEN SayHello (CallWindowString);
            '%': IF SayHiEnable AND (Rate < SayHiRateCutoff) THEN SayName  (CallWindowString);

            ':': BEGIN
                 RITEnable := False;
                 SendKeyboardInput;
                 RITEnable := True;
                 END;

            '~': SendSalutation (CallWindowString);
            '\': AddStringToBuffer (MyCall, CWTone);

            '|': IF ReceivedData.Name <> '' THEN
                     AddStringToBuffer (ReceivedData.Name + ' ', CWTone);

            '[': BEGIN
                 WarningSounded := False;

                 QuickDisplay ('WAITING FOR YOU ENTER STRENGTH OF RST (Single digit)!!');

                 AddStringToBuffer ('5', CWTone);

                 Key := '0';

                 REPEAT
                     REPEAT
                         IF NOT CWStillBeingSent THEN
                             BEGIN
                             IF NOT WaitForStrength THEN
                                 BEGIN
                                 Key := '9';
                                 Break;
                                 END
                             ELSE
                                 IF NOT WarningSounded THEN
                                     BEGIN
                                     WarningSounded := True;
                                     Tone.DoABeep (ThreeHarmonics);
                                     END;
                             END;

                     UNTIL KeyPressed;

                     IF Key <> '9' THEN Key := ReadKey;

                 UNTIL ((Key >= '1') AND (Key <= '9')) OR (Key = EscapeKey);

                 IF Key = EscapeKey THEN
                     BEGIN
                     FlushCWBufferAndClearPTT;
                     Exit;
                     END;

                 IF Key = '9' THEN
                     AddStringToBuffer ('NN', CWTone)
                 ELSE
                     AddStringToBuffer (Key + 'N', CWTone);
                 ReceivedData.RSTSent := '5' + Key + '9';

                 LastRSTSent := ReceivedData.RSTSent;
                 END;

            ']': AddStringToBuffer (LastRSTSent, CWTone);

            '{': AddStringToBuffer (ReceivedData.Callsign, CWTone);

            '}': IF StringHas (ReceivedData.Callsign, '/') OR
                    ((Length (ReceivedData.Callsign) = 4) AND SendCompleteFourLetterCall) OR
                    StringHas (CallsignICameBackTo, '/') THEN
                        AddStringToBuffer (ReceivedData.Callsign, CWTone)
                    ELSE
                        IF GetPrefix (ReceivedData.Callsign) =
                           GetPrefix (CallsignICameBackTo) THEN
                               BEGIN
                               TempString := GetSuffix (ReceivedData.Callsign);
                               IF Length (TempString) = 1 THEN
                                   TempString := Copy (ReceivedData.Callsign, Length (ReceivedData.Callsign) - 1, 2);
                               AddStringToBuffer (TempString, CWTone);
                               END
                        ELSE
                           IF GetSuffix (ReceivedData.Callsign) =
                              GetSuffix (CallsignICameBackTo) THEN
                                  AddStringToBuffer (GetPrefix (ReceivedData.Callsign), CWTone)
                           ELSE
                               AddStringToBuffer (ReceivedData.Callsign, CWTone);

            '>': ClearRIT;

            ')': AddStringToBuffer (VisibleLog.LastCallsign, CWTone);

            ControlQ:   { Send frequency of other radio }
                BEGIN
                IF ActiveRadio = RadioOne THEN
                    TempFreq := LastDisplayedFreq [RadioTwo]
                ELSE
                    TempFreq := LastDisplayedFreq [RadioOne];

                TempFreq := Round (TempFreq / 1000);  { Compute kHz }
                Str (TempFreq, TempString);
                AddStringToBuffer (TempString, CWTone);
                END;

            ControlR:   { Generate random characters }
                BEGIN
                ReceivedData.RandomCharsSent := '';

                REPEAT
                    ReceivedData.RandomCharsSent := ReceivedData.RandomCharsSent + Chr (Random (25) + Ord ('A'));
                UNTIL Length (ReceivedData.RandomCharsSent) = 5;

                AddStringToBuffer (ReceivedData.RandomCharsSent, CWTone);

                SaveSetAndClearActiveWindow (DupeInfoWindow);
                Write ('Sent = ', ReceivedData.RandomCharsSent);
                RestorePreviousWindow;
                END;

            ControlT:   { Repeat random characters previously sent }
                AddStringToBuffer (ReceivedData.RandomCharsSent, CWTone);

            ControlU:   { Show station information }
                BEGIN
                TempCall := GetCorrectedCallFromExchangeString (ExchangeWindowString);

                IF TempCall <> '' THEN
                    CallSignICameBackTo := TempString
                ELSE
                    CallsignICameBackTo := CallWindowString;

                ShowStationInformation (CallsignICameBackTo);
                END;

            ControlW:   { Send previous name sent }
                AddStringToBuffer (VisibleLog.LastName (4), CWTone);

            ControlLeftBracket:
                CommandMode := True;

            ELSE
                AddStringToBuffer (SendChar, CWTone);
            END;
        END;

    { ClearPTTForceOn;  Removed Nov-2022  }

    { Hmmm - not sure how I feel about this in Dec-2023 as I am stuck with
      the PTT stuck on after sending the CQ exchange. For now - I put that
      somewhere else. }

    END;



PROCEDURE SendCrypticMessage (Message: Str160);

    BEGIN
    IF FoundCommand (Message) AND (Message = '') THEN Exit;

    InactiveRigCallingCQ := False;

    IF NOT (Copy (Message, 1, 1) = ControlA) THEN SetUpToSendOnActiveRadio;

    WHILE (Copy (Message, 1, 1) = ControlA) OR (Copy (Message, 1, 1) = ControlB) DO
        BEGIN
        IF Copy (Message, 1, 1) = ControlA THEN
            IF NOT SingleRadioMode THEN
                BEGIN

                {KK1L: 6.72 Need to set mode to that of ModeMemory [RadioOne] for split mode SO2R}
                {           Copied here from SetUpToSendOnInactiveRadio. It affected normal SO2R there.}
                IF ActiveRadio = RadioOne THEN
                    BEGIN
                    IF NOT SendingOnRadioTwo THEN {KK1L: 6.73 Was SendingOnRadioOne...mistake}
                        BEGIN
                        ActiveMode := ModeMemory [RadioTwo];
                        END;
                    END
                ELSE           { Radio Two }
                    IF NOT SendingOnRadioOne THEN {KK1L: 6.73 Was SendingOnRadioTwo...mistake}
                        BEGIN
                        ActiveMode := ModeMemory [RadioOne];
                        END;

                SetUpToSendOnInactiveRadio;

                Delay (5);  { Added to get rid of relay flicking on old radio }
                END;

        IF Copy (Message, 1, 1) = ControlB THEN
            IF NOT SingleRadioMode THEN InactiveRigCallingCQ := True;

        Delete (Message, 1, 1);
        END;

    CASE ActiveMode OF
        Phone:
            BEGIN
            IF DVKEnable THEN
                SendDVKMessage (Message);
            END;

        CW: SendCrypticCWString (Message);
        Digital: SendCrypticDigitalString (Message);
        END;
    END;


PROCEDURE SendFunctionKeyMessage (Key: CHAR; OpMode: OpModeType);

VAR QSONumberString: Str20;
    MessageKey: CHAR;
    Message: Str160;

    BEGIN
    IF ((Key >= F1)         AND (Key <= F10)) OR
       ((Key >= F11)        AND (Key <= F12)) OR
       ((Key >= ShiftF1)    AND (Key <= ShiftF10)) OR
       ((Key >= ShiftF11)   AND (Key <= ShiftF12)) OR
       ((Key >= ControlF1)  AND (Key <= ControlF10)) OR
       ((Key >= ControlF11) AND (Key <= ControlF12)) OR
       ((Key >= AltF1)      AND (Key <= AltF10)) OR
       ((Key >= AltF11)     AND (Key <= AltF12)) THEN
           BEGIN
           MessageKey := Key;

           IF (ActiveMode = Phone) AND DVKEnable THEN
               IF (Key >= ControlF1) AND (Key <= ControlF10)
                   and dvkcontrolkeyrecord then
                   MessageKey := Chr (Ord (Key) - 35);

           IF OpMode = CQOpMode THEN
               BEGIN
               Message := GetCQMemoryString (ActiveMode, MessageKey); {KK1L: 6.73 Added mode}

               IF (Key = F1) OR (Key = F2) THEN   { Likely CQ called - new in Jan 2024 }
                   CallAlreadySent := False;  { Re-arm AutoStartSend }
               END
           ELSE
               Message := GetEXMemoryString (ActiveMode, MessageKey); {KK1L: 6.73 Added mode}

           FoundCommand (Message);

           IF Message = '' THEN Exit;    { Nothing left }

           InactiveRigCallingCQ := False;

           SetUpToSendOnActiveRadio;

           IF ActiveMode = CW THEN
               BEGIN
               CWEnabled := True;
               DisplayCodeSpeed (CodeSpeed, CWEnabled, False, ActiveMode);

               IF AllCWMessagesChainable THEN
                   BEGIN
                   IF CWStillBeingSent THEN
                       AddStringToBuffer (' ', CWTone);
                   END
               ELSE

                   { I am not sure exactly why this is here.  I notice when I press
                     a function key memory, I end up flushing the buffer and
                     unasserting PTT.  I am not really sure this is necessary, but
                     I will leave it here as it doesn't seem to do any harm.  }

                   IF Pos (ControlD, Message) = 0 THEN FlushCWBufferAndClearPTT;
               END;


           {QuickDisplay2('SendFunctionKeyMessage..1..2..3..4');}

           IF (ActiveMode = Phone) AND DVKEnable AND (Key >= ControlF1) AND (Key <= ControlF10) AND DVKControlKeyRecord THEN
               BEGIN
               {KK1L: 6.73 Added mode}
               IF StringHas (UpperCase (GetCQMemoryString (ActiveMode, Chr (Ord (Key) - 35))), 'DVK') THEN
                   BEGIN
                   IF OpMode = CQOpMode THEN
                       BEGIN
                       QuickDisplay ('Recording DVK.  Press ESCAPE or RETURN to stop.');
                       {KK1L: 6.73 Added mode}
                       DVKRecordMessage (GetCQMemoryString (ActiveMode, Chr (Ord (Key) - 35)));
                       END
                   ELSE
                       BEGIN
                       QuickDisplay ('Recording DVK.  Press ESCAPE or RETURN to stop.');
                       {KK1L: 6.73 Added mode to GetEXMemoryString}
                       DVKRecordMessage (GetEXMemoryString (ActiveMode, Chr (Ord (Key) - 35)));
                       END;
                   END;
               Exit;
               END;

           { Finally we are ready to send the message }

           IF ((Key >= F1) AND (Key <= AltF10)) OR ((Key >= F11) AND (Key <= AltF11)) THEN
                BEGIN
                SendCrypticMessage (Message);

                IF (OpMode = CQOpMode) AND (Key >= F1) AND (Key <= F4) THEN
                    BEGIN
                    IF BandMapEnable AND BandMapDisplayCQ THEN
                        BEGIN
                        IF ActiveRadio = RadioOne THEN
                            BEGIN
                            IF LastDisplayedFreq [RadioOne] <> 0 THEN
                                BEGIN
                                Str (QSONumberForThisQSO, QSONumberString);

                                {BandMapCursorFrequency := LastDisplayedFreq [RadioOne];}

                                NewBandMapEntry ('CQ/' + QSONumberString,
                                                 LastDisplayedFreq [RadioOne], 0, ActiveMode,
                                                 False, False, BandMapDecayTime, True);

                                LastCQFrequency := LastDisplayedFreq [RadioOne]; {KK1L: 6.68 Saves LastCQFreq}
                                LastCQMode      := ActiveMode;         {KK1L: 6.68 and mode}
                                END;
                            END

                        ELSE { Radio two }

                            IF LastDisplayedFreq [RadioTwo] <> 0 THEN
                                BEGIN
                                Str (QSONumberForThisQSO, QSONumberString);
                                { BandMapCursorFrequency := Frequency;}

                                NewBandMapEntry ('CQ/' + QSONumberString,
                                                 LastDisplayedFreq [RadioTwo], 0, ActiveMode,
                                                 False, False, BandMapDecayTime, True);
                                LastCQFrequency := LastDisplayedFreq [RadioTwo]; {KK1L: 6.68 Saves LastCQFreq}
                                LastCQMode      := ActiveMode;         {KK1L: 6.68 and mode}
                                END;
                           END;

                    IF ((ActiveMultiPort <> nil) OR (MultiUDPPort > -1)) AND (MultiInfoMessage <> '') THEN
                        CreateAndSendCQMultiInfoMessage;

                    END;
                END;
           END;
    END;



PROCEDURE AddOnCQExchange;

{ In Jan 2023, came here trying to make the RTTY exchange get sent and it
  was not happening with the ELSE into the CW case, so I put in the case
  statement of the ActiveMode }

VAR Name: Str20;
    Count, StationSpeed: INTEGER;

    BEGIN
    CASE ActiveMode OF
        Phone:
            BEGIN
            IF (CQPhoneExchangeNameKnown <> '') AND SayHiEnable THEN
                BEGIN
                Name := UpperCase (CD.GetName (RootCall (CallsignICameBackTo)));

                IF (Name = '') OR (Name = 'CLUB') THEN
                    SendCrypticMessage (CQPhoneExchangeNameKnown)
                ELSE
                    SendCrypticMessage (CQPhoneExchange);
                END
            ELSE
                SendCrypticMessage (CQPhoneExchange);
            END;

        CW:
            BEGIN
            IF CWSpeedFromDataBase THEN
                BEGIN
                StationSpeed := CD.GetCodeSpeed (RootCall (CallsignICameBackTo));

                IF StationSpeed > 0 THEN
                    BEGIN
                    RememberCWSpeed := CodeSpeed;
                    SetSpeed (StationSpeed);
                    DisplayCodeSpeed (CodeSpeed, CWEnabled, False, ActiveMode);
                    END;
                END;

            IF (CQExchangeNameKnown <> '') AND SayHiEnable THEN
                BEGIN
                Name := UpperCase (CD.GetName (RootCall (CallsignICameBackTo)));

                IF (Name = '') OR (Name = 'CLUB') THEN
                    SendCrypticMessage (CQExchange)
                ELSE
                    SendCrypticMessage (CQExchangeNameKnown);
                END
            ELSE
                BEGIN
                SendCrypticMessage (CQExchange);
                END;

            { ClearPTTForceOn;   Taking this out in Nov 22 }

            { I feel I still need this here in Dec-2023.  Going to try and
              put it back in and see what happens.  So - this seems to fix
              the issue of being stuck in TX after sending the CQ exhange
              after I put the PTTForceOn back in when starting to send the
              callsign in logsubs2.pas.

              However, I am still seeing a PTT dropout occur after sending
              the callsign.  Maybe this is because I am clearing this before
              the message is getting setup properly in the Arduino?

              Well - no - I put a very long delay here and I still got the
              dropout - and ended up with along delay after the callsign
              was sent before going to RX and dropping into the exchange
              window.

              However, I think it is prudent to add a small delay here
              anyway. }

            IF CWenabled THEN
                BEGIN
                Count := 0;

                REPEAT
                    millisleep;
                    Inc (Count);
                UNTIL Count = 100;

                ClearPTTForceOn;
                END;

            END; { of CW case }

        Digital:
            { Digital does not use CQ EXCHANGE.  It also does not send the
              callsign automaticall.  It is up to the user to put the callsign
              in the EX DIGITAL MEMORY F2 }

            SendCrypticMessage (GetEXMemoryString (Digital, F2));

        END; { of CASE }

    ExchangeHasBeenSent := True;
    END;



PROCEDURE Send73Message;

    BEGIN
    IF SeventyThreeMessageSent OR NOT MessageEnable THEN Exit;
    IF BeSilent THEN Exit;

    IF ActiveMode = Phone THEN
        SendCrypticMessage (QSLPhoneMessage)
    ELSE
        BEGIN
        IF AutoQSLCount > 0 THEN
            BEGIN
            Dec (AutoQSLCount);
            IF AutoQSLCount = 0 THEN
                BEGIN
                SendCrypticMessage (QSLMessage);
                AutoQSLCount := AutoQSLInterval;
                END
            ELSE
                SendCrypticMessage (QuickQSLMessage1);

            END
        ELSE
            SendCrypticMessage (QSLMessage);
        END;

    SeventyThreeMessageSent := True;
    END;



PROCEDURE SendCorrectCallIfNeeded;

    BEGIN
    IF (ReceivedData.Callsign <> CallsignICameBackTo) AND NOT BeSilent THEN
        BEGIN
        IF MessageEnable THEN
            SendCrypticMessage (CorrectedCallMessage);

        CallsignICameBackTo := ReceivedData.Callsign;
        END;

    { Does this really ever work? }

    IF StringHas (CallWindowString, ',') THEN
        BEGIN
        TailEndCallString := PostcedingString (CallWindowString, ',');
        TailEnding := True;
        AddStringToBuffer (TailEndMessage + ' ' + TailEndCallString, CWTone);
        END;
    END;



PROCEDURE PutUpExchangeMenu;

{ This routine will put up the exchange menu.  This menu is displayed
  during the time the exchange is being entered and edited.  The exchange
  window is normally underneath this menu.                               }

    BEGIN
    IF ExchangeFunctionKeyMenu = '' THEN
        RemoveWindow (FunctionKeyWindow)
    ELSE
        BEGIN
        SaveSetAndClearActiveWindow (FunctionKeyWindow);
        Write (ExchangeFunctionKeyMenu);
        RestorePreviousWindow;
        END;
    END;



PROCEDURE SwapRadios;

VAR TimeOut: INTEGER;

    BEGIN
    IF SingleRadioMode THEN
        BEGIN
        QuickDisplay ('Alt-R command disabled by SINGLE RADIO MODE = TRUE');
        Tone.DoABeep (Single);
        Wait (3000); {KK1L: 6.71}
        Exit;
        END;

        IF (ActiveMode = Phone) AND DVKMessagePlaying THEN
            BEGIN
            SendDVKMessage('DVK0');

            TimeOut := 0;

            REPEAT
                Wait (5);
                Inc (TimeOut)
            UNTIL (NOT DVKMessagePlaying) OR (TimeOut > 30);
            END;

    IF ActiveMode = CW THEN FlushCWBufferAndClearPTT;

    LastDisplayedFreq[RadioOne] := 0; {KK1L: 6.73 Forces new display for highlight}
    LastDisplayedFreq[RadioTwo] := 0; {KK1L: 6.73 Forces new display for highlight}

    { RX audio affected after here }

    IF ActiveRadio = RadioOne THEN
        BEGIN
        ActiveBand      := BandMemory [RadioTwo];
        ActiveMode      := ModeMemory [RadioTwo];
        ActiveKeyer.SetActiveRadio (RadioTwo);
        ActiveRadio     := RadioTwo;
        InactiveRadio   := RadioOne;
        CodeSpeed       := SpeedMemory[RadioTwo];
        SetSpeed (CodeSpeed);
        DisplayCodeSpeed (CodeSpeed, CWEnabled, False, ActiveMode);

        { If we have reserved a QSO number - try to give it back }

        IF QSONumberByBand AND (BandMemory [RadioOne] <> BandMemory [RadioTwo]) THEN
            BEGIN
            IF QSONumberForThisQSO > 0 THEN
                ReturnQSONumber (BandMemory [RadioOne], QSONumberForThisQSO);

            QSONumberForThisQSO := ReserveNextQSONumber (ActiveBand);
            END;



        END
    ELSE
        BEGIN
        ActiveBand      := BandMemory [RadioOne];
        ActiveMode      := ModeMemory [RadioOne];
        ActiveKeyer.SetActiveRadio (RadioOne);
        ActiveRadio     := RadioOne;
        InactiveRadio   := RadioTwo;
        CodeSpeed       := SpeedMemory[RadioOne];
        SetSpeed (CodeSpeed);
        DisplayCodeSpeed (CodeSpeed, CWEnabled, False, ActiveMode);

        { If we have reserved a QSO number - try to give it back }

        IF QSONumberByBand AND (BandMemory [RadioOne] <> BandMemory [RadioTwo]) THEN
            BEGIN
            IF QSONumberForThisQSO > 0 THEN
                ReturnQSONumber (BandMemory [RadioTwo], QSONumberForThisQSO);

            QSONumberForThisQSO := ReserveNextQSONumber (ActiveBand);
            END;
        END;

    { RX Audio affected before here }

    SetUpToSendOnActiveRadio;
    DisplayRadio (ActiveRadio);
    DisplayBandMode (ActiveBand, ActiveMode, False);
    UpdateTotals;

    DisplayQSONumber (QSONumberForThisQSO, ActiveBand);

    IF MultByBand THEN
        BEGIN
        VisibleLog.ShowRemainingMultipliers;
        VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);
        END;

    BandMapBand := ActiveBand;
    BandMapMode := ActiveMode; {KK1L: 6.69}
    VisibleDupeSheetChanged := True;
    END;



PROCEDURE ExchangeRadios; {KK1L: 6.71}
{KK1L: 6.71 This proc exchanges the band/mode/freq of the radios keeping the same one active}
{           The original purpose was to make it easy to find a new run freq on the second radio}
{           (presumably not on the best antenna system) and swap it to the primary antenna setup.}

VAR TimeOut: INTEGER;
    ModeOne, ModeTwo: ModeType;
    BandOne, BandTwo: BandType;
    FreqOne, FreqTwo: LONGINT;
    RadioOneReadOkay, RadioTwoReadOkay: BOOLEAN;

    BEGIN
    IF SingleRadioMode THEN
        BEGIN
        QuickDisplay2 ('ExchangeRadios command disabled by SINGLE RADIO MODE = TRUE');
        Tone.DoABeep (Single);
        Wait (3000); {KK1L: 6.71}
        Exit;
        END;

    IF (ActiveMode = Phone) AND DVKMessagePlaying THEN
        BEGIN
        SendDVKMessage('DVK0');

        TimeOut := 0;

        REPEAT
            Wait (5);
            Inc (TimeOut)
        UNTIL (NOT DVKMessagePlaying) OR (TimeOut > 30);
        END;

    IF ActiveMode = CW THEN FlushCWBufferAndClearPTT;

    RadioOneReadOkay := True;{KK1L: 6.71 False if CASE is used!}
    RadioTwoReadOkay := True;{KK1L: 6.71 False if CASE is used!}
    BandOne := BandMemory [RadioTwo]; {KK1L: 6.71 Save the values to become RadioOne values}
    ModeOne := ModeMemory [RadioTwo]; {KK1L: 6.71 Save the values to become RadioOne values}
    FreqOne := StableRadio2Freq;      {KK1L: 6.71 Save the values to become RadioOne values}
    BandTwo := BandMemory [RadioOne]; {KK1L: 6.71 Save the values to become RadioTwo values}
    ModeTwo := ModeMemory [RadioOne]; {KK1L: 6.71 Save the values to become RadioTwo values}
    FreqTwo := StableRadio1Freq;      {KK1L: 6.71 Save the values to become RadioTwo values}

    IF (RadioOneReadOkay) AND (RadioTwoReadOkay) THEN
        BEGIN
        SetRadioFreq (RadioTwo, FreqTwo, ModeTwo, 'A'); {KK1L: 6.71 Need yet to handle split mode and VFO B}
        {SetRadioFreq (RadioTwo, FreqTwo, ModeTwo, 'A'); }{KK1L: 6.71 Need yet to handle split mode and VFO B}
        BandMemory [RadioTwo] := BandTwo; {KK1L: 6.71 Set RadioTwo stuff from RadioOne stuff}
        ModeMemory [RadioTwo] := ModeTwo; {KK1L: 6.71 Set RadioTwo stuff from RadioOne stuff}
        IF FrequencyMemoryEnable THEN FreqMemory [BandTwo, ModeTwo] := FreqTwo;

        Delay(200); {KK1L: 6.73}

        SetRadioFreq (RadioOne, FreqOne, ModeOne, 'A'); {KK1L: 6.71 Need yet to handle split mode and VFO B}
        {SetRadioFreq (RadioOne, FreqOne, ModeOne, 'A');} {KK1L: 6.71 Need yet to handle split mode and VFO B}
        BandMemory [RadioOne] := BandOne; {KK1L: 6.71 Set RadioOne stuff from what was RadioTwo stuff}
        ModeMemory [RadioOne] := ModeOne; {KK1L: 6.71 Set RadioOne stuff from what was RadioTwo stuff}
        IF FrequencyMemoryEnable THEN FreqMemory [BandOne, ModeOne] := FreqOne;

        Delay(200); {KK1L: 6.73}

        CodeSpeed := SpeedMemory[RadioTwo];  {KK1L: 6.73}
        SpeedMemory[RadioTwo] := SpeedMemory[RadioOne]; {KK1L: 6.73 array for speed}
        SpeedMemory[RadioOne] := CodeSpeed; {KK1L: 6.73}
        END
    ELSE
        BEGIN
        QuickDisplayError ('Trouble reading inactive radio!');
        Tone.DoABeep (Single);
        Exit;
        END;

    {KK1L 6.71 Sets the program to operate where the active radio is now at.}
    {IF ActiveRadio = RadioOne THEN
        BEGIN
        ActiveBand      := BandMemory [RadioOne];
        ActiveMode      := ModeMemory [RadioOne];
        END
    ELSE
        BEGIN
        ActiveBand      := BandMemory [RadioTwo];
        ActiveMode      := ModeMemory [RadioTwo];
        END;
    } {KK1L: 6.71 Alread done for us in SetUpToSendOnActiveRadio}

    SetSpeed (CodeSpeed);
    DisplayCodeSpeed (CodeSpeed, CWEnabled, False, ActiveMode);
    SetUpToSendOnActiveRadio;
    DisplayRadio (ActiveRadio);
    DisplayBandMode (ActiveBand, ActiveMode, False);
    UpdateTotals;

    IF QSONumberByBand THEN
        DisplayQSONumber (QSONumberForThisQSO, ActiveBand);

    IF MultByBand THEN
        BEGIN
        VisibleLog.ShowRemainingMultipliers;
        VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);
        END;

    BandMapBand := ActiveBand;
    BandMapMode := ActiveMode;
    {DisplayBandMap;} {KK1L: 6.71 Removed because UpdateTimeAndRateDisplays in LOGWIND covers it.}
    VisibleDupeSheetChanged := True;
    END;



PROCEDURE CheckTwoRadioState (Action: TwoRadioAction);

VAR Key: CHAR;

    BEGIN
    IF SingleRadioMode THEN
        BEGIN
        TwoRadioState := TwoRadiosDisabled;
        Exit;
        END;

    CASE TwoRadioState OF

        Idle:
            IF Action = CallPutUp THEN
                BEGIN
                TwoRadioState := CallReady;
                ExchangeHasBeenSent := False;
                END;

        { There is a callsign displayed and if someone presses the space
          bar, we want to call that station NOW! }

        CallReady:
            CASE Action OF
                SpaceBarPressed:
                    BEGIN
{                   IF (ActiveMode = CW) AND CWStillBeingSent THEN
                        FlushCWBufferAndClearPTT;
}
                    IF (OpMode = CQOpMode) THEN {KK1L: 6.73}
                        CalledFromCQMode := TRUE
                    ELSE
                        CalledFromCQMode := FALSE; {KK1L: 6.73 NOTE TRUE disables special called from SAPMode}

                    SwapRadios;    { Changes active band/mode and display }

                    InactiveRigCallingCQ := False;

                    IF ActiveMode = CW THEN
                        BEGIN
                        IF DEEnable THEN
                            SendStringAndStop ('DE ' + MyCall)
                        ELSE
                            SendStringAndStop (MyCall);
                        END
                    ELSE
                        IF ActiveMode = Digital THEN
                            SendStringAndStop (CallWindowString + ' DE ' + MyCall + ' KK ')
                        ELSE
                            IF DVKEnable THEN
                                {KK1L: 6.73 Added mode to GetEXMemoryString}
                                SendCrypticMessage (GetExMemoryString (ActiveMode, F1));
//                              SendDVKMessage (GetExMemoryString (ActiveMode, F1));

                    TwoRadioState := StationCalled;

                    { If during the time we are calling an ESCAPE key
                      is pressed, we will abort sending MyCall on the
                      inactive radio and go back to CQing on the Active
                      Radio }

                    {KK1L: 6.71 Added DoingDVK so DVKDelay is used too!}
                    IF (ActiveMode = CW) OR DVKEnable THEN
                        BEGIN
                        REPEAT
                            IF KeyPressed THEN
                                BEGIN
                                Key := ReadKey;

                                IF Key = EscapeKey THEN
                                    BEGIN
                                    FlushCWBufferAndClearPTT;

                                    SwapRadios;  { Goes back to original display }

                                    {SendCrypticMessage (GetCQMemoryString (F1));}
                                    {KK1L: 6.73}
                                    IF CalledFromCQMode THEN SendCrypticMessage (GetCQMemoryString (ActiveMode, F1));

                                    { We have a problem... the callsign is
                                      still up and we are in S&P mode }

                                    TwoRadioState := CallReady;  { Should fix it }
                                    Exit;
                                    END
                                ELSE
                                    IF Key = NullKey THEN
                                        CASE ReadKey OF
                                            AltB: BEGIN
                                                  RememberFrequency;
                                                  BandUp;
                                                  END;

                                            AltV: BEGIN
                                                  RememberFrequency;
                                                  BandDown;
                                                  END;

                                            PageUpKey:   SpeedUp;
                                            PageDownKey: SlowDown;
                                            END;
                                END;


                            IF ActiveMode = Phone THEN Wait (10);
                            millisleep;

                        UNTIL (((ActiveMode = CW)    AND NOT CWStillBeingSent) OR
                               ((ActiveMode = Phone) AND NOT (DVKMessagePlaying)));

                        { Now launch a CQ on the "inactive" rig (which was
                          the one we were CQing on.  }

                        {KK1L: 6.73 Added CalledFromCQMode}
                        {KK1L: 6.73 Added mode to GetCQMemoryString}
                        IF (GetCQMemoryString (ModeMemory[InactiveRadio], AltF3) <> '') AND (CalledFromCQMode) THEN
                            BEGIN
                            {KK1L: 6.73 Added mode to GetCQMemoryString}
                            SendCrypticMessage (ControlA + GetCQMemoryString (ModeMemory[InactiveRadio], AltF3));
                            END;

                        END;

                    Exit;
                    END;
                END;


        { We have called a station on the second rig.  We were sending a CQ
          on the inactive radio using the ControlA feature which means that
          InactiveRigSendingCW is TRUE. }

        StationCalled:
            CASE Action OF
                SpaceBarPressed,        { Need to call the station again }
                F1Pressed:
                    BEGIN
                    InActiveRigCallingCQ := False;

                    IF ActiveMode = CW THEN
                        BEGIN

                        IF (ActiveMode = CW) AND DEEnable THEN
                            SendStringAndStop ('DE ' + MyCall)
                        ELSE
                            SendStringAndStop (MyCall);
                        END
                    ELSE
                        BEGIN
                        IF DVKEnable THEN
                            {SendDVKMessage (GetEXMemoryString (F2));} {KK1L: 6.71 removed}
                            {KK1L: 6.73 Added mode to GetEXMemoryString}
                            SendDVKMessage (GetEXMemoryString (ActiveMode, F1)); {KK1L: 6.71 added to be consistent!}

                        END;

                    {KK1L: 6.71 Added DoingDVK so DVKDelay is used too!}
//                    IF (ActiveMode = CW) OR DVPEnable OR DVKEnable THEN
                    IF (true) THEN
                        BEGIN
                        REPEAT
                            IF KeyPressed THEN
                                BEGIN
                                Key := ReadKey;
                                IF Key = EscapeKey THEN
                                    BEGIN
                                    SwapRadios;  { Goes back to original display }
                                    {SendCrypticMessage (GetCQMemoryString (F1));}
                                    {KK1L: 6.73 Added mode to GetCQMemoryString}
                                    {KK1L: 6.73}
                                    IF CalledFromCQMode THEN SendCrypticMessage (GetCQMemoryString (ActiveMode, F1));
                                    TwoRadioState := CallReady;
                                    Exit;
                                    END
                                ELSE
                                    IF Key = NullKey THEN
                                        CASE ReadKey OF
                                            AltB: BEGIN
                                                  RememberFrequency;
                                                  BandUp;
                                                  END;

                                            AltV: BEGIN
                                                  RememberFrequency;
                                                  BandDown;
                                                  END;

                                            PageUpKey:   SpeedUp;
                                            PageDownKey: SlowDown;
                                            END;
                                END;


                            IF ActiveMode = Phone THEN Wait (20);

                            millisleep;
                        UNTIL (((ActiveMode = CW)    AND NOT CWStillBeingSent) OR
                               ((ActiveMode = Phone) AND NOT (DVKMessagePlaying)));

                        {KK1L: 6.73 Added CalledFromCQMode}
                        {KK1L: 6.73 Added mode to GetCQMemoryString}
                        IF (GetCQMemoryString (ModeMemory[InactiveRadio], AltF3) <> '') AND (CalledFromCQMode) THEN
                            BEGIN
                            {KK1L: 6.73 Added mode to GetCQMemoryString}
                            SendCrypticMessage (ControlA + GetCQMemoryString (ModeMemory[InactiveRadio], AltF3));
                            END;
                        END;

                    Exit;
                    END;

                F2Pressed:
                    BEGIN
                    IF ActiveMode = CW THEN
                        IF ExchangeHasBeenSent AND (RepeatSearchAndPounceExchange <> '') THEN
                            SendCrypticMessage (RepeatSearchAndPounceExchange)
                        ELSE
                            SendCrypticMessage (SearchAndPounceExchange)
                    ELSE
                        IF ExchangeHasBeenSent AND (RepeatSearchAndPouncePhoneExchange <> '') THEN
                            SendCrypticMessage (RepeatSearchAndPouncePhoneExchange)
                        ELSE
                            SendCrypticMessage (SearchAndPouncePhoneExchange);

                    ExchangeHasBeenSent := True;
                    TwoRadioState := SendingExchange;
                    END;

                ReturnPressed:
                    BEGIN
                    IF NOT (ExchangeHasBeenSent OR BeSilent) THEN  { Added BeSilent in 6.59 }
                        BEGIN
                        SetUpToSendOnInactiveRadio;

                        IF ActiveMode = CW THEN
                            SendCrypticMessage (SearchAndPounceExchange)
                        ELSE
                            SendCrypticMessage (SearchAndPouncePhoneExchange);
                        END;

                    ExchangeHasBeenSent := True;
                    TwoRadioState := SendingExchange;
                    END;

                { The station didn't come back to me, so we need to continue
                  the CQ and be ready for a response. }

                EscapePressed:
                    BEGIN
                    IF ActiveRadio = RadioTwo THEN  { We are sending CQ on rig 1}
                        BEGIN
                        ActiveBand  := BandMemory [RadioOne];
                        ActiveMode  := ModeMemory [RadioOne];
                        ActiveRadio := RadioOne;
                        InactiveRadio := RadioTwo; {KK1L: 6.73}
                        END
                    ELSE                            { We are sending CQ on rig 2}
                        BEGIN
                        ActiveBand  := BandMemory [RadioTwo];
                        ActiveMode  := ModeMemory [RadioTwo];
                        ActiveRadio := RadioTwo;
                        InactiveRadio := RadioOne; {KK1L: 6.73}
                        END;

                    CallWindowString := '';
                    ResetSavedWindowListAndPutUpCallWindow;
                    RemoveWindow (ExchangeWindow);

                    LastDisplayedFreq[RadioOne]  := 0; {KK1L: 6.73 forces update of freq display so highlight is shown}
                    LastDisplayedFreq[RadioTwo] := 0; {KK1L: 6.73 forces update of freq display so highlight is shown}
                    DisplayRadio    (ActiveRadio);
                    DisplayBandMode (ActiveBand, ActiveMode, False);
                    UpdateTotals;

                    TwoRadioState := CallReady;    { Leave CW alone }
                    END;


                { Someone pressed the footswitch.  We are probably on SSB
                  and the operator wants to send his exchange to the guy
                  we are working.  We need to make sure the proper radio
                  is getting keyed. }

                FootSwitchWasPressed:
                    SetUpToSendOnActiveRadio;
                END;

        SendingExchange:
            IF Action = ContactDone THEN
                BEGIN
                IF ActiveMode = Phone THEN Wait (50);

                {KK1L: 6.71 Added DoingDVK so DVKDelay is used too!}
//                IF (ActiveMode = CW) OR DVPEnable OR DVKEnable THEN
                IF (true) THEN
                    BEGIN
                    REPEAT
                        IF KeyPressed THEN
                            BEGIN
                            Key := UpCase (ReadKey);

                            CASE Key OF

                                EscapeKey:
                                    BEGIN
                                    SwapRadios;  { Goes back to original display }
                                    {SendCrypticMessage (GetCQMemoryString (F1));}
                                    {KK1L: 6.73 Added mode to GetCQMemoryString}
                                    IF CalledFromCQMode THEN SendCrypticMessage (GetCQMemoryString (ActiveMode, F1));
                                    TwoRadioState := Idle;
                                    Exit;
                                    END;

                                NullKey:
                                    CASE ReadKey OF
                                        AltB: BEGIN
                                              RememberFrequency;
                                              BandUp;
                                              END;

                                        AltV: BEGIN
                                              RememberFrequency; {KK1L 6.72 Was in AltB but not here!}
                                              BandDown;
                                              END;

                                        PageUpKey:   SpeedUp;
                                        PageDownKey: SlowDown;
                                        END;

                                ELSE
                                    IF ActiveWindow = CallWindow THEN
                                        IF ValidCallCharacter (Key) THEN
                                            BEGIN
                                            ControlBMemory := ControlBMemory + Key;
                                            Write (Key);
                                            END;
                                END;

                            END;


                        IF ActiveMode = Phone THEN Wait (20);
                        millisleep;

                    UNTIL (((ActiveMode = CW)    AND NOT CWStillBeingSent) OR
                           ((ActiveMode = Phone) AND NOT (DVKMessagePlaying)));
                    END;

                SwapRadios;  { Back to main radio band and mode }

                {KK1L: 6.73 Added CalledFromCQMode}
                IF (ControlBMemory = '') AND (OnDeckCall = '') AND (CalledFromCQMode) THEN
                    {KK1L: 6.73 Added mode to GetCQMemoryString}
                    SendCrypticMessage (GetCQMemoryString (ActiveMode, F1));  { Launch a real CQ }

                TwoRadioState := Idle;
                END;
        END;
    END;



PROCEDURE SetupPacketSpot (Address: INTEGER; Radio: RadioType);

{ This procedure will look in the packet memory at the indicated address
  and set the CallWindowString to the proper callsign, set the active
  radio to the proper radio with the proper band and mode, and show
  the QSO and multiplier status for the station, and display the beam
  heading.  It is up to whoever calls this to make sure the state
  of the program is ready to accept this data.    }

VAR QSXFrequency, PacketFrequency: LONGINT;

    BEGIN
    PacketFrequency := Packet.PacketMemory [Address].Frequency;
    QSXFrequency    := Packet.PacketMemory [Address].QSXFrequency;

    CalculateBandMode (PacketFrequency, ActiveBand, ActiveMode);

    BandMemory [Radio] := ActiveBand;
    ModeMemory [Radio] := ActiveMode;

    IF Radio <> ActiveRadio THEN
        SwapRadios
    ELSE
        BEGIN
        DisplayBandMode (ActiveBand, ActiveMode, False);
        UpdateTotals;
        END;

    IF QSXFrequency <> 0 THEN
        BEGIN
        {KK1L: 6.71 moved ahead of PutRadioIntoSplit because for TS850 need to make B active to change mode}
        SetRadioFreq (ActiveRadio, QSXFrequency, ActiveMode, 'B');
        PutRadioIntoSplit (ActiveRadio);
        END
    ELSE {KK1L: 6.64 Takes radio out of split if not needed for spot}
        BEGIN
        PutRadioOutOfSplit (ActiveRadio);
        END;

    SetRadioFreq (Radio, PacketFrequency, ActiveMode, 'A'); {KK1L: 6.71 Moved here from before IF for TS850 change}

    CallWindowString := Packet.PacketMemory [Address].Call;

    VisibleLog.ShowQSOStatus        (CallWindowString);
    VisibleLog.ShowMultiplierStatus (CallWindowString);
    DisplayBeamHeading              (CallWindowString);
    END;



PROCEDURE DisplayPacketSpots (HighLightedSpot: INTEGER);

VAR PacketAddress, Spot: INTEGER;

    BEGIN
    TextColor (ActiveColor);
    TextBackground (ActiveBackground);

    ClrScr;

    FOR Spot := 1 TO 10 DO
        BEGIN

        PacketAddress := Packet.PacketMemoryStart + Spot - 1;

        IF PacketAddress > 10 THEN
            PacketAddress := PacketAddress - 11;

        IF PacketAddress = Packet.PacketMemoryEnd THEN Exit;

        GoToXY (((Spot - 1) DIV 5) * 40 + 1, (Spot - 1) MOD 5 + 1);

        IF Spot = HighLightedSpot THEN
            BEGIN
            TextColor (ActiveBackground);
            TextBackground (ActiveColor);
            END
        ELSE
            BEGIN
            TextColor (ActiveColor);
            TextBackground (ActiveBackground);
            END;

        Write (Packet.PacketMemory [PacketAddress].Time, '  ',
               Packet.PacketMemory [PacketAddress].Frequency / 1000:7:1, '  ',
               Packet.PacketMemory [PacketAddress].Call);

        IF Packet.PacketMemory [PacketAddress].QSXFrequency <> 0 THEN
            Write (' QSX ', Packet.PacketMemory [PacketAddress].QSXFrequency / 1000:7:1);

        NoCursor;
        END;

    QuickDisplay ('Select spot.  RETURN, left or right arrow to select.  ESCAPE to exit.');
    END;



PROCEDURE SetUpRadioFromPacketSpot (Radio: RadioType;
                                     PacketAddress: INTEGER;
                                     Spot: INTEGER);

    BEGIN
    RestorePreviousWindow;
    VisibleLog.SetUpEditableLog;
    RemoveWindow (QuickCommandWindow);
    BigCursor;

    PacketAddress := Packet.PacketMemoryStart + Spot - 1;

    IF PacketAddress > 10 THEN
        PacketAddress := PacketAddress - 11;

    IF Packet.PacketMemoryEnd > Packet.PacketMemoryStart THEN
        BEGIN
        IF (PacketAddress >= Packet.PacketMemoryStart) AND
           (PacketAddress < Packet.PacketMemoryEnd) THEN
               SetupPacketSpot (PacketAddress, Radio)
        END
    ELSE
        IF (PacketAddress >= Packet.PacketMemoryStart) OR
           (PacketAddress < Packet.PacketMemoryEnd) THEN
               SetupPacketSpot (PacketAddress, Radio);
    END;




FUNCTION PacketMemoryRequest: BOOLEAN;

{ Gets called when a Control-U is done.  Returns TRUE if spot selected.
  Spot address number is put into Global variable PacketAddress.     }

VAR Key: CHAR;
    PacketAddress, NumberSpots: INTEGER;
    Spot: INTEGER;

    BEGIN
    PacketMemoryRequest := False;

    IF (ActivePacketPort = nil) AND (ActiveMultiPort = nil) AND (MultiUDPPort = -1) THEN
        Exit;

    IF Packet.PacketMemoryStart = Packet.PacketMemoryEnd THEN Exit;

    SaveSetAndClearActiveWindow (EditableLogWindow);
    ClrScr;

    Spot := 1;

    IF Packet.PacketMemoryEnd < Packet.PacketMemoryStart THEN
        NumberSpots := (Packet.PacketMemoryEnd + 11) - Packet.PacketMemoryStart
    ELSE
        NumberSpots := Packet.PacketMemoryEnd - Packet.PacketMemoryStart;

    REPEAT
        DisplayPacketSpots (Spot);

        REPEAT millisleep UNTIL KeyPressed;

        Key := ReadKey;

        CASE Key OF
            TabKey:
                IF (Spot > 5) THEN Spot := Spot - 5 ELSE Spot := Spot + 5;

            EscapeKey:
                BEGIN
                RestorePreviousWindow;
                VisibleLog.SetUpEditableLog;
                RemoveWindow (QuickCommandWindow);
                BigCursor;
                Exit;
                END;

            CarriageReturn:
                BEGIN
                PacketMemoryRequest := True;
                RestorePreviousWindow;
                VisibleLog.SetUpEditableLog;
                RemoveWindow (QuickCommandWindow);
                BigCursor;

                PacketAddress := Packet.PacketMemoryStart + Spot - 1;

                IF PacketAddress > 10 THEN
                    PacketAddress := PacketAddress - 11;

                IF Packet.PacketMemoryEnd > Packet.PacketMemoryStart THEN
                    BEGIN
                    IF (PacketAddress >= Packet.PacketMemoryStart) AND
                       (PacketAddress < Packet.PacketMemoryEnd) THEN
                           SetupPacketSpot (PacketAddress, ActiveRadio)
                    END
                ELSE
                    IF (PacketAddress >= Packet.PacketMemoryStart) OR
                       (PacketAddress < Packet.PacketMemoryEnd) THEN
                           SetupPacketSpot (PacketAddress, ActiveRadio);

                Exit;
                END;

            NullKey:
                BEGIN
                Key := ReadKey;

                CASE Key OF
                    UpArrow:
                        IF (Spot <> 1) AND (Spot <> 6) THEN
                            Dec (Spot);

                    DownArrow:
                        IF (Spot <> 5) AND (Spot <> 10) THEN
                            IF Spot < NumberSpots THEN
                                Inc (Spot);

                    LeftArrow:
                        BEGIN
                        IF SwapPacketSpotRadios THEN
                            BEGIN
                            IF Radio1ControlPort <> nil THEN
                                BEGIN
                                SetUpRadioFromPacketSpot (RadioOne, PacketAddress, Spot);
                                PacketMemoryRequest := True;
                                Exit;
                                END;
                            END
                        ELSE
                            IF Radio2ControlPort <> nil THEN
                                BEGIN
                                PacketMemoryRequest := True;
                                SetUpRadioFromPacketSpot (RadioTwo, PacketAddress, Spot);
                                Exit;
                                END;

                        IF Spot > 5 THEN Spot := Spot - 5;
                        END;


                    RightArrow:
                        BEGIN
                        IF SwapPacketSpotRadios THEN
                            BEGIN
                            IF Radio2ControlPort <> nil THEN
                                BEGIN
                                SetUpRadioFromPacketSpot (RadioTwo, PacketAddress, Spot);
                                PacketMemoryRequest := True;
                                Exit;
                                END;
                            END
                        ELSE
                            IF Radio1ControlPort <> nil THEN
                                BEGIN
                                PacketMemoryRequest := True;
                                SetUpRadioFromPacketSpot (RadioOne, PacketAddress, Spot);
                                Exit;
                                END;

                         IF Spot <= NumberSpots - 5 THEN
                             IF (Spot < 6) THEN Spot := Spot + 5;
                         END;
                    END;
                END;
            END;
    UNTIL False;
    END;



PROCEDURE DualingCQs;

    BEGIN
    IF SingleRadioMode THEN Exit;

    {KK1L: 6.73 Added mode to GetCQMemoryString}
    IF GetCQMemoryString (ActiveMode, AltF1) = '' THEN
        BEGIN
        QuickDisplay ('No CQ message programmed into CQ MEMORY AltF1.');
        Exit;
        END;

    SwapRadios;

    REPEAT
        SendFunctionKeyMessage (AltF1, CQOpMode);

        REPEAT
            UpdateTimeAndRateDisplays (True, True);
            Packet.CheckPacket;
            millisleep;
        UNTIL (KeyPressed) OR NOT CWStillBeingSent;

        IF KeyPressed THEN
            BEGIN
            FlushCWBufferAndClearPTT;
            SwapRadios;
            Exit;
            END;

        SwapRadios;
    UNTIL False;
    END;


