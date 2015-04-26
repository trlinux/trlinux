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

UNIT LogWAE;

{$O+}
{$V-}

INTERFACE

USES Dos, Tree, Country9, ZoneCont, LogSCP, LogWind, LogCW,
     LogDDX, LogDupe, LogStuff, LogGrid, LogHelp, LogK1EA, LogDVP,
     LogDom, LogEdit, trCrt, Ports, datetimec;


TYPE QTCActionType = (NoQTCAction, AbortThisQTC, SaveThisQTC);

VAR QTCExtraSpace: BOOLEAN;
    QTCNote: Str80;
    QTCQRS: BOOLEAN;

PROCEDURE WAEQTC (VAR QTCCallsign: CallString);

IMPLEMENTATION
uses keycode,beep,timer;

PROCEDURE FixUpQTCString (VAR QTCString: Str80);

VAR TempString: Str80;

    BEGIN
    IF QTCExtraSpace THEN
        BEGIN
        TempString := RemoveFirstString (QTCString);

        WHILE QTCString <> '' DO
            TempString := TempString + '  ' + RemoveFirstString (QTCString);

        QTCString := TempString;
        END;
    END;



FUNCTION QTCQuickEditResponse (Prompt: Str80;
                               VAR QTCAction: QTCActionType;
                               VAR ControlEnterUsed: BOOLEAN): Str80;

VAR TempString, InputString: Str80;
    Key, PreviousCursorChar: Char;
    CursorPosition, StartOfInputCursorPosition, RememberX, CharPointer: INTEGER;

    BEGIN
    QTCAction := NoQTCAction;

    ControlEnterUsed := False;

    SaveSetAndClearActiveWindow (QuickCommandWindow);

    Write (Prompt);

    StartOfInputCursorPosition := WhereX;

    InputString := '';

    REPEAT
        REPEAT millisleep UNTIL NewKeyPressed;

        Key := UpCase (NewReadKey);

        CASE Key OF
            EscapeKey:
                IF CWStillBeingSent THEN
                    FlushCWBufferAndClearPTT
                ELSE
                    IF InputString <> '' THEN
                        BEGIN
                        GoToXY (WhereX - Length (InputString), WhereY);
                        ClrEol;
                        InputString := '';
                        END
                    ELSE
                        BEGIN
                        GoToXY (1, WhereY);

                        Write ('Do you want to (A)bort, (C)ontinue or (S)ave this QTC? : ');

                        REPEAT
                            REPEAT millisleep UNTIL KeyPressed;
                            Key := UpCase (ReadKey);
                        UNTIL (Key = 'A') OR (Key = 'C') OR (Key = 'S');

                        CASE Key OF
                            'A': BEGIN
                                 QTCAction := AbortThisQTC;
                                 ClrScr;
                                 Exit;
                                 END;

                            'C': BEGIN
                                 ClrScr;
                                 Write (Prompt);
                                 END;

                            'S': BEGIN
                                 QTCAction := SaveThisQTC;
                                 ClrScr;
                                 Exit;
                                 END;
                            END;
                        END;


            BackSpace:
                IF Length (InputString) > 0 THEN
                    IF WhereX > StartOfInputCursorPosition THEN
                        BEGIN
                        Delete (InputString, WhereX - StartOfInputCursorPosition, 1);
                        RememberX := WhereX;
                        GoToXY (RememberX - 1, WhereY);
                        ClrEol;
                        Write (Copy (InputString, WhereX - StartOfInputCursorPosition + 1, 50));
                        GoToXY (RememberX - 1, WhereY);
                        END;


            ControlA:
                BEGIN
                CursorPosition := WhereX - StartOfInputCursorPosition + 1;

                IF CursorPosition > 1 THEN
                    BEGIN
                    REPEAT
                        Dec (CursorPosition);
                        PreviousCursorChar := InputString [CursorPosition - 1];
                    UNTIL ((InputString [CursorPosition] <> ' ') AND
                           (PreviousCursorChar = ' ')) OR (CursorPosition = 1);
                    GoToXY (CursorPosition + StartOfInputCursorPosition - 1, WhereY);
                    END;
                END;

            ControlD:
                IF WhereX - StartOfInputCursorPosition < Length (InputString) + 1 THEN
                    GoToXY (WhereX + 1, WhereY);

            ControlF:
                BEGIN
                CursorPosition := WhereX - StartOfInputCursorPosition + 1;

                IF CursorPosition <= Length (InputString) THEN
                    BEGIN
                    REPEAT
                        PreviousCursorChar := InputString [CursorPosition];
                        Inc (CursorPosition);
                    UNTIL ((InputString [CursorPosition] <> ' ') AND
                           (PreviousCursorChar = ' ')) OR (CursorPosition = Length (InputString) + 1);
                    GoToXY (CursorPosition + StartOfInputCursorPosition - 1, WhereY);
                    END;
                END;

            ControlG:
                IF WhereX - StartOfInputCursorPosition + 1 <= Length (InputString) THEN
                    BEGIN
                    Delete (InputString, WhereX - StartOfInputCursorPosition + 1, 1);
                    RememberX := WhereX;
                    ClrEol;
                    Write (Copy (InputString, WhereX - StartOfInputCursorPosition + 1, 50));
                    GoToXY (RememberX, WhereY);
                    END;

            ControlJ:  { Might be ControlCarriageReturn }
                IF Port [$60] <> $24 THEN
                    BEGIN
                    ControlEnterUsed := True;

                    TempString := InputString;

                    RemoveFirstString (TempString);
                    RemoveFirstString (TempString);

                    IF TempString <> '' THEN
                        BEGIN
                        QTCQuickEditResponse := InputString;
                        RemoveAndRestorePreviousWindow;
                        Exit;
                        END;

                    Exit;
                    END;

            ControlS:
                IF WhereX - StartOfInputCursorPosition >= 1 THEN
                    GoToXY (WhereX - 1, WhereY);

            ControlT:
                BEGIN
                CursorPosition := WhereX - StartOfInputCursorPosition + 1;

                IF CursorPosition <= Length (InputString) THEN
                    BEGIN
                    TempString := InputString;
                    TempString [0] := Chr (CursorPosition - 1);
                    CharPointer := CursorPosition;

                    REPEAT
                        Inc (CharPointer);
                    UNTIL (CharPointer = Length (InputString)) OR
                          (InputString [CharPointer] = ' ');

                    IF CharPointer = Length (InputString) THEN
                        BEGIN
                        ClrEOL;
                        InputString [0] := Chr (CursorPosition - 1);
                        END
                    ELSE
                        BEGIN
                        Inc (CharPointer);
                        FOR CharPointer := CharPointer TO Length (InputString) DO
                            TempString := TempString + InputString [CharPointer];

                        GoToXY (StartOfInputCursorPosition, WhereY);
                        ClrEOL;

                        Write (TempString);
                        InputString := TempString;
                        GoToXY (CursorPosition + StartOfInputCursorPosition - 1, WhereY);
                        END;
                    END
                ELSE
                    IF CursorPosition = Length (InputString) THEN
                        BEGIN
                        ClrEol;
                        InputString [0] := Chr (Length (InputString) - 1);
                        END;
                END;

            CarriageReturn:
                BEGIN
                TempString := InputString;

                RemoveFirstString (TempString);
                RemoveFirstString (TempString);

                IF TempString <> '' THEN
                    BEGIN
                    QTCQuickEditResponse := InputString;
                    RemoveAndRestorePreviousWindow;
                    Exit;
                    END;
                END;

            NullKey:
                BEGIN
                Key := ReadKey;

                CASE Key OF
                    AltA: SendStringAndStop ('AGN');
                    AltC: SendStringAndStop ('CALL?');
                    AltK: ToggleCW (False);
                    AltN: SendStringAndStop ('NR?');
                    AltR: SendStringAndStop ('RPT?');
                    AltT: SendStringAndStop ('TIME?');

                    HomeKey: GoToXY (StartOfInputCursorPosition, 1);

                    LeftArrow:
                        IF WhereX - StartOfInputCursorPosition >= 1 THEN
                            GoToXY (WhereX - 1, WhereY);

                    RightArrow:
                        IF WhereX - StartOfInputCursorPosition < Length (InputString) + 1 THEN
                            GoToXY (WhereX + 1, WhereY);

                    EndKey:
                        GoToXY (Length (InputString) + StartOfInputCursorPosition , WhereY);

                    InsertKey:
                        BEGIN
                        InsertMode := NOT InsertMode;
                        DisplayInsertMode (InsertMode);
                        END;

                    DeleteKey:
                        IF WhereX - StartOfInputCursorPosition + 1 <= Length (InputString) THEN
                            BEGIN
                            Delete (InputString, WhereX - StartOfInputCursorPosition + 1, 1);
                            RememberX := WhereX;
                            ClrEol;
                            Write (Copy (InputString, WhereX - StartOfInputCursorPosition + 1, 50));
                            GoToXY (RememberX, WhereY);
                            END;


                    END;
                END;

            ELSE
                BEGIN
                IF InsertMode THEN
                    BEGIN
                    Insert (Key, InputString, WhereX - StartOfInputCursorPosition + 1);
                    ClrEol;
                    RememberX := WhereX;
                    Write (Copy (InputString, WhereX - StartOfInputCursorPosition + 1, 50));
                    GotoXY (RememberX + 1, WhereY);
                    END
                ELSE
                    BEGIN
                    IF WhereX - StartOfInputCursorPosition + 1 <= Length (InputString) THEN
                        InputString [WhereX - StartOfInputCursorPosition + 1] := Key
                    ELSE
                        InputString := InputString + Key;

                    Write (Key);
                    END;
                END;

            END;  { of case }

    UNTIL FALSE;
    END;



PROCEDURE WAEQTC (VAR QTCCallsign: CallString);

{ This procedure gets called whenever we are asking for a QTC.  It will
  handle everything needed to do the QTC.  It will return after everything
  has been done.  }

LABEL SaveQTC, StopQTC, KeepGoing;

VAR QTCNumberString, QTCString, MessageString: Str80;
    NumberMessagesToBeSentString, TempString, MaxQTCString, CommandString: Str80;
    NumberQTCsAlreadySent, QTCNumber, NumberMessagesToBeSent, MaxQTCsThisStation: INTEGER;
    FullTimeString: Str20;
    Message, Line, Index: INTEGER;
    QTCBuffer: LogEntryArray;
    ControlEnterUsed, DataChanged, ValidKey: BOOLEAN;
    FileWrite: TEXT;
    ExtendedKey, Key: CHAR;
    QTCAction: QTCActionType;

    BEGIN
    QTCNote := '';

    LastQTCTimeSent := 'AA';

    IF (DDXState <> Off) THEN
        BEGIN
        IF DDXState <> SAndPExchangeSentAndAskedForQTC THEN Exit;
        DDXState := QTCInProgress;
        END;

    IF QTCCallsign = '' THEN
        IF WindowDupeCheckCall <> '' THEN
            BEGIN
            QTCCallsign := WindowDupeCheckCall;
            IF ActiveWindow <> CallWindow THEN
                BEGIN
                SaveSetAndClearActiveWindow (CallWindow);
                Write (QTCCallsign);
                RestorePreviousWindow;
                END
            ELSE
                BEGIN
                ClrScr;
                Write (QTCCallsign);
                END;
            END
        ELSE
            Exit;

    IF NOT GoodCallSyntax (QTCCallsign) THEN
        BEGIN
        Tone.DoABeep (Warning);
        QuickDisplay ('Invalid callsign in call window!!');
        Exit;
        END;

    QTCCallsign := StandardCallFormat (QTCCallsign, False);

    FOR Line := 1 TO 5 DO QTCBuffer [Line] := '';

    NumberQTCsAlreadySent := NumberQTCsThisStation (QTCCallsign);

    IF NumberQTCsAlreadySent >= 10 THEN
        BEGIN
        Tone.DoABeep (Warning);
        QuickDisplay ('Sorry, you already have 10 QTCs with ' + QTCCallsign);
        Exit;
        END;

    MaxQTCsThisStation := 10 - NumberQTCsAlreadySent;
    Str (MaxQTCsThisStation, MaxQTCString);

    IF MyContinent = Europe THEN    { We are receiving QTCs }
        BEGIN
        IF NumberQTCsAlreadySent = 0 THEN
            SendStringAndStop ('QTC?')
        ELSE
            SendStringAndStop ('PSE ' + MaxQTCString + ' QTC?');

        ClearPTTForceOn;

        REPEAT
            TempString := QuickEditResponse ('Enter QTC #/# (max of ' + MaxQTCString + ') or RETURN if QRU : ', 8);

            IF TempString = EscapeKey THEN
                IF CWStillBeingSent THEN
                    FlushCWBufferAndClearPTT
                ELSE
                    Exit;

            IF TempString = '' THEN Exit;
        UNTIL TempString <> EscapeKey;

        IF NOT DetermineQTCNumberAndQuanity (TempString, QTCNumber, NumberMessagesToBeSent) THEN Exit;

        IF NumberMessagesToBeSent > MaxQTCsThisStation THEN
            BEGIN
            Str (MaxQTCsThisStation, TempString);
            AddStringToBuffer ('PSE ONLY ' + TempString + ' QTCS ', CWTone);
            QuickDisplay ('That is too many messages.  We will only take ' + TempString);
            NumberMessagesToBeSent := MaxQTCsThisStation;
            END;

        Str (NumberMessagesToBeSent, NumberMessagesToBeSentString);

        SendStringAndStop ('QRV');

        Message := 1;
        Str (QTCNumber, QTCNumberString);
        ClearWindow (EditableLogwindow);

        WHILE Message <= NumberMessagesToBeSent DO
            BEGIN
            Str (Message, MessageString);


    KeepGoing:

            QTCString := QTCQuickEditResponse ('Enter QTC #' + MessageString +
                                               ' of ' + NumberMessagesToBeSentString +
                                               ' : ', QTCAction, ControlEnterUsed);


            CASE QTCAction OF

                AbortThisQTC:
                    BEGIN
                    NextQTCToBeSent := NextQTCToBeSent - NumberMessagesToBeSent;
                    DecrementQTCCount (QTCCallsign, Message - 1);
                    VisibleLog.SetUpEditableLog;
                    Tone.DoABeep (Warning);
                    QuickDisplay ('QTC cancelled since it was not QSLed');
                    Exit;
                    END;

                SaveThisQTC:
                    BEGIN
                    Dec (Message);
                    Str (Message, MessageString);
                    NumberMessagesToBeSent := -1;
                    NumberMessagesToBeSentString := MessageString;
                    RestorePreviousWindow;
                    GoTo SaveQTC;
                    END;
                END;

            IF NOT ControlEnterUsed THEN SendStringAndStop ('R');

            AddQTCToQTCBuffer (QTCBuffer, QTCString, Message);

            IncrementQTCCount (QTCCallsign);
            Inc (Message);
            END;

        REPEAT
            TempString := UpperCase (QuickEditResponse ('Press RETURN to log or E to Edit : ', 1));
        UNTIL (TempString = '') OR (TempString = 'E');

        IF TempString = 'E' THEN
            BEGIN
            QuickDisplay ('You are editing the QTC data.  Press ESCAPE to exit.');
            EditWindowEditor (QTCBuffer, 5, 1, DataChanged);
            END;

        SendStringAndStop ('QSL ' + QTCNumberString + '/' + MessageString);
        END



    ELSE
        BEGIN       { We are sending QTCs }
        NumberMessagesToBeSent := NumberAvailableQTCsForThisCall (QTCCallsign);

        IF NumberMessagesToBeSent = 0 THEN
            BEGIN
            SendStringAndStop ('QRU');
            Tone.DoABeep (Warning);
            QuickDisplay ('No QTCs pending, QRU.');
            Exit;
            END;

        IF NumberMessagesToBeSent > MaxQTCsThisStation THEN
            NumberMessagesToBeSent := MaxQTCsThisStation;

        Str (NumberMessagesToBeSent, NumberMessagesToBeSentString);
        Str (NumberQTCBooksSent + 1, QTCNumberString);

        SendStringAndStop ('QTC ' + QTCNumberString + '/' + NumberMessagesToBeSentString + ' QRV?');

        ClearPTTForceOn;

        IF DDXState = QTCInProgress THEN
            BEGIN
            REPEAT millisleep UNTIL NOT CWStillBeingSent;
            Wait (100);
            Wait (Random (300));
            AddStringToBuffer ('QRV', DDXTone1);
            END;

        ClearWindow (EditableLogwindow);

        REPEAT
            CommandString := UpperCase (QuickEditResponse ('Is ' + QTCCallsign + ' QRV for QTC '
                                        + QTCNumberString + '/' + NumberMessagesToBeSentString
                                        + '? (RETURN to start or ESCAPE to abort) : ', 1));

            IF CommandString = EscapeKey THEN
                BEGIN
                IF CommandString = EscapeKey THEN
                    IF CWStillBeingSent THEN
                        FlushCWBufferAndClearPTT
                    ELSE
                        BEGIN
                        TempString := UpperCase (QuickEditResponse
                            ('Do you really want to abort this QTC? (Y/N) : ', 1));

                        IF TempString = 'Y' THEN
                            BEGIN
                            QuickDisplay ('QTC aborted by operator.');
                            VisibleLog.SetUpEditableLog;
                            Exit;
                            END;
                        END;
                END;
        UNTIL CommandString = '';

//        FOR Message := 1 TO NumberMessagesToBeSent DO
    Message := 0;
    while ((Message + 1) <= NumberMessagesToBeSent) Do
            BEGIN
            inc(Message);

            QTCString := PopNextQTCToBeSent (FullTimeString);

            FixUpQTCString (QTCString);

            IF QTCQRS THEN
                SendStringAndStop (ControlS + QTCString + ControlF)
            ELSE
                SendStringAndStop (QTCString);

            AddQTCToQTCBuffer (QTCBuffer, FullTimeString, Message);

            REPEAT
                IF DDXState = QTCInProgress THEN
                    BEGIN
                    QuickDisplay (QTCNumberString + '/' + NumberMessagesToBeSentString +
                                  '  RETURN-next  ESCAPE-abort  S-Stop  T-time  C-call  N-qso#  A-all');

                    REPEAT millisleep
                    UNTIL NOT CWStillBeingSent;
                    Wait (100);
                    Wait (Random (300));

                    Index := Random (50);

                    CASE Index OF
                        0, 1: AddStringToBuffer ('AGN',   DDXTone1);
                        2, 3: AddStringToBuffer ('CL?',   DDXTone1);
                      {KS  3,} 4: AddStringToBuffer ('NR?',   DDXTone1);
                           5: AddStringToBuffer ('QTR?',  DDXTone1);
                           6: AddStringToBuffer ('Time?', DDXTone1);

                        ELSE BEGIN
                             AddStringToBuffer ('R', DDXTone1);
                             IF Message = NumberMessagesToBeSent THEN
                                 IF Index > 20 THEN
                                     AddStringToBuffer (' QSL TU', DDXTone1)
                                 ELSE
                                     AddStringToBuffer (' QSL 73 DE ' + QTCCallsign, DDXTone1);
                             END;
                        END;
                    END;


                REPEAT
                    QuickDisplay (QTCNumberString + '/' + NumberMessagesToBeSentString +
                                  '  RETURN-next  ESCAPE-abort  S-Stop  T-time  C-call  N-qso#  A-all');

                    ValidKey := True;
                    REPEAT millisleep UNTIL KeyPressed;
                    Key := UpCase (ReadKey);

                    CASE Key OF
                        'A': SendStringAndStop (QTCString);

                        'C': BEGIN
                             TempString := QTCString;
                             RemoveFirstString (TempString);
                             TempString := RemoveFirstString (TempString);
                             SendStringAndStop (TempString);
                             END;

                        'N': BEGIN
                             TempString := GetLastString (QTCString);
                             SendStringAndStop (TempString);
                             END;

                        'S': BEGIN
                             Str (Message, MessageString);
                             TempString := UpperCase (QuickEditResponse
                                ('Do you really want stop now? (Y/N) : ', 1));

                             IF TempString = 'Y' THEN
                                 BEGIN
                                 TempString := UpperCase (QuickEditResponse
                                     ('Was message number ' + MessageString + ' confirmed (Y/N) : ', 1));

                                 IF TempString <> 'Y' THEN
                                     BEGIN
                                     Dec (Message);
                                     Dec (NextQTCToBeSent);

                                     IF Message = 0 THEN
                                         BEGIN
                                         VisibleLog.SetUpEditableLog;
                                         QuickDisplay ('QTC aborted.  No messages were QSLed.');
                                         Exit;
                                         END;
                                     END;

                                 NumberMessagesToBeSent := Message;
                                 Str (NumberMessagesToBeSent, NumberMessagesToBeSentString);
                                 GoTo StopQTC;
                                 END;

                             QuickDisplay (QTCNumberString + '/' + NumberMessagesToBeSentString +
                               '  RETURN-next  ESCAPE-abort  S-Stop  T-time  C-call  N-qso#  A-all');
                             END;

                        'T': SendStringAndStop (PrecedingString (QTCString, ' '));

                        CarriageReturn: ;
                        ControlJ: ;

                        NullKey:
                            BEGIN
                            ExtendedKey := ReadKey;

                            CASE ExtendedKey OF

                                AltK: ToggleCW (False);

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

                                END;

                            ValidKey := False;
                            END;

                        EscapeKey:
                            BEGIN
                            IF CWStillBeingSent THEN
                                FlushCWBufferAndClearPTT
                            ELSE
                                BEGIN
                                TempString := UpperCase (QuickEditResponse
                                    ('Do you really want to abort the whole QTC? (Y/N) : ', 1));

                                IF TempString = 'Y' THEN
                                    BEGIN
                                    NextQTCToBeSent := NextQTCToBeSent - Message;
                                    VisibleLog.SetUpEditableLog;
                                    QuickDisplay ('QTC aborted by operator.');
                                    Exit;
                                    END;
                                END;

                            ValidKey := False;
                            END;

                        ELSE ValidKey := False;
                        END;
                UNTIL ValidKey;

            UNTIL (Key = CarriageReturn) or (Key = ControlJ);
            END;

StopQTC:

        REPEAT
            TempString := UpperCase (QuickEditResponse ('QSL ' +
                                                     QTCNumberString +
                                                     '/' +
                                                     NumberMessagesToBeSentString +
                                                    '? (Y/N) : ', 1));

            IF TempString [1] = 'N' THEN
                BEGIN
                NextQTCToBeSent := NextQTCToBeSent - NumberMessagesToBeSent;
                VisibleLog.SetUpEditableLog;
                QuickDisplay ('QTC cancelled since it was not QSLed');
                Exit;
                END;
        UNTIL TempString [1] = 'Y';

        FOR Message := 1 TO NumberMessagesToBeSent DO
            IncrementQTCCount (QTCCallsign);

        Inc (NumberQTCBooksSent);
        QTCNumber := NumberQTCBooksSent;
        DisplayQTCNumber (NumberQTCsThisStation (StandardCallFormat (QTCCallsign, False)));
        END;

SaveQTC:

    Str (QTCNumber, QTCNumberString);

    TempString := 'QTC ' + QTCNumberString + '/' + NumberMessagesToBeSentString +
                  ' with ' + QTCCallsign + ' on ' + GetDateString + ' at ' +
                  GetTimeString + ' on ' + BandString [ActiveBand] + ModeString [ActiveMode];

    IF OpenFileForAppend (FileWrite, QTCFileName) THEN
        BEGIN
        WriteLn (FileWrite);

        WriteLn (FileWrite, TempString);

        FOR Line := 1 TO 5 DO
            IF QTCBuffer [Line] <> '' THEN
                WriteLn (FileWrite, QTCBuffer [Line]);

        Close (FileWrite);
        END;

    SaveQTCDataFile;
    DisplayQTCNumber (NumberQTCsThisStation (StandardCallFormat (CallWindowString, False)));
    VisibleLog.SetUpEditableLog;

    QTCNote := TempString;

    QuickDisplay (TempString);

    UpdateTotals;

    IF DDXState = QTCInProgress THEN DDXState := SAndPExchangeSent;
    END;



    BEGIN
    END.
