UNIT PostLog;

{$O+}
{$V-}

INTERFACE

Uses Tree,
     SlowTree,
     PostSubs,
     Country9,
     LogDupe,
     LogWind,
     trCrt,
     ZoneCont;

FUNCTION  DupeLog: BOOLEAN;
FUNCTION  LogProcedureMenu: BOOLEAN;
PROCEDURE MultCheck;

IMPLEMENTATION
uses keycode;


TYPE WorkedArrayType = ARRAY [BandType, CW..Both, 0..400] OF BOOLEAN;
     WorkedArrayPtr  = ^WorkedArrayType;

     PrefixArrayType = ARRAY [0..2000] OF FourBytes;

     PrefixArrayPtr = ^PrefixArrayType;

     PrefixArrayRecordType = RECORD
         List:           PrefixArrayPtr;
         NumberPrefixes: INTEGER;
         END;

VAR Worked: WorkedArrayPtr;
    PrefixLists: ARRAY [BandType, CW..Both] OF PrefixArrayRecordType;



PROCEDURE TotalStamp (Band: BandType; Mode: ModeType);

VAR TempString: Str80;
    PageQSOPointsString, LogQSOPointsString, ValidContactsString: Str20;
    PageValidContactsString, PageMultString, TotalMultString: Str20;

    BEGIN
    TempString := '';
    WriteToLogFile ('', Band, Mode);

    Str (PageValidContacts^ [Band, Mode], PageValidContactsString);
    Str (ValidContacts^ [Band, Mode], ValidContactsString);
    TempString := TempString + 'Valid QSOs = ' + PageValidContactsString;
    TempString := TempString + '/' + ValidContactsString + '  ';
    PageValidContacts^ [Band, Mode] := 0;

    IF AccumulateQSOPoints THEN
        BEGIN
        Str (PageQSOPoints^ [Band, Mode], PageQSOPointsString);
        Str (LogFileQSOPoints^ [Band, Mode], LogQSOPointsString);
        TempString := TempString + 'Pts = ' + PageQSOPointsString;
        TempString := TempString + '/' + LogQSOPointsString + '  ';
        PageQSOPoints^ [Band, Mode] := 0;
        END;

    IF AccumulateMultipliers THEN
        BEGIN
        IF DoingDomesticMults THEN
            BEGIN
            TotalDomesticMults^ [Band, Mode]:= TotalDomesticMults^ [Band, Mode] + NumberPageDomesticMults^ [Band, Mode];
            Str (NumberPageDomesticMults^ [Band, Mode], PageMultString);
            Str (TotalDomesticMults^ [Band, Mode], TotalMultString);
            TempString := TempString + 'Dom Mult = ' + PageMultString;
            TempString := TempString + '/' + TotalMultString + '  ';
            NumberPageDomesticMults^ [Band, Mode]:= 0;
            END;

        IF DoingZoneMults THEN
            BEGIN
            TotalZoneMults^ [Band, Mode] := TotalZoneMults^ [Band, Mode] + NumberPageZoneMults^ [Band, Mode];
            Str (NumberPageZoneMults^ [Band, Mode], PageMultString);
            Str (TotalZoneMults^ [Band, Mode], TotalMultString);
            TempString := TempString + 'Zone = ' + PageMultString;
            TempString := TempString + '/' + TotalMultString + '  ';
            NumberPageZoneMults^ [Band, Mode] := 0;
            END;

        IF DoingDXMults THEN
            BEGIN
            TotalDXMults^ [Band, Mode] := TotalDXMults^ [Band, Mode] + NumberPageDXMults^ [Band, Mode];
            Str (NumberPageDXMults^ [Band, Mode], PageMultString);
            Str (TotalDXMults^ [Band, Mode], TotalMultString);
            TempString := TempString + 'DX Mult = ' + PageMultString;
            TempString := TempString + '/' + TotalMultString + '  ';
            NumberPageDXMults^ [Band, Mode] := 0;
            END;

        IF DoingPrefixMults THEN
            BEGIN
            TotalPrefixMults^ [Band, Mode] := TotalPrefixMults^ [Band, Mode] + NumberPagePrefixMults^ [Band, Mode];
            Str (NumberPagePrefixMults^ [Band, Mode], PageMultString);
            Str (TotalPrefixMults^ [Band, Mode], TotalMultString);
            TempString := TempString + 'Prefix = ' + PageMultString;
            TempString := TempString + '/' + TotalMultString + '  ';
            NumberPagePrefixMults^ [Band, Mode] := 0;
            END;

        END;

    WriteToLogFile (TempString, Band, Mode);
    END;



PROCEDURE GetCopyOfHeader;

VAR TempString: Str80;
    Band: BandType;
    FileRead: TEXT;

    BEGIN
    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError ('Unable to open ' + LogFileName + '!!!!');
        WaitForKeyPressed;
        Halt;
        END;

    NumberHeaderLines := 0;

    REPEAT
        ReadLn (FileRead, TempString);
        Band := GetLogEntryBand (TempString);
        IF Band = NoBand THEN
            BEGIN
            ExpandTabs (TempString);
            Header [NumberHeaderLines] := TempString;
            Inc (NumberHeaderLines);
            END;
    UNTIL Band <> NoBand;

    Close (FileRead);
    END;



PROCEDURE WriteHeader (Band: BandType; Mode: ModeType);

VAR Line, PageNumber, NumberPages: INTEGER;
    PageNumberString, NumberPagesString: Str80;

    BEGIN
    FOR Line := 0 TO NumberHeaderLines - 1 DO
        BEGIN
        IF Line = 1 THEN
            BEGIN
            WHILE Length (Header [Line]) < 65 DO Header [Line] := Header [Line] + ' ';
            Header [Line] := Copy (Header [Line], 1, 65);
            PageNumber := (LogFileQSOsPrinted^ [Band, Mode] DIV QSOsPerPage) + 1;
            Str (PageNumber, PageNumberString);
            NumberPages := ((LogTotals [Band, Mode] - 1) DIV QSOsPerPage) + 1;
            Str (NumberPages, NumberPagesString);
            Header [Line] := Header [Line] + 'PAGE ' + PageNumberString + ' OF ' + NumberPagesString;
            END;

        WriteToLogFile (Header [Line], Band, Mode);
        END;
    END;



PROCEDURE WriteLogEntry (LogEntry: Str80; Band: BandType; Mode: ModeType);

VAR TempString: Str80;
    MultString, DomesticString, UnknownString, NumberString: Str20;
    QSOPoints, Mult, NumberMults: WORD;
    MultArray: ARRAY [1..2] OF Str20;
    MultIdentified: BOOLEAN;

    BEGIN
    IF QSOsPerPage > 0 THEN
        BEGIN
        IF LogFileQSOsPrinted^ [Band, Mode] MOD QSOsPerPage = 0 THEN
            BEGIN
            IF LogfileQSOsPrinted^ [Band, Mode] > 0 THEN
                BEGIN
                TotalStamp (Band, Mode);
                WriteToLogFile (ControlL, Band, Mode);
                END
            ELSE
                WriteToLogFile (' ', Band, Mode);

            WriteHeader (Band, Mode);
            END
        ELSE
            IF LogFileQSOsPrinted^ [Band, Mode] MOD 10 = 0 THEN
                WriteToLogfile ('', Band, Mode);
        END;

    Inc (LogFileQSOsPrinted^ [Band, Mode]);

    IF ReNumberQSONumbers THEN
        BEGIN
        Delete (LogEntry, LogEntryQSONumberAddress, LogEntryQSONumberWidth);
        Str (LogFileQSOsPrinted^ [Band, Mode], TempString);
        WHILE Length (TempString) < 4 DO TempString := ' ' + TempString;
            Insert (TempString, LogEntry, LogEntryQSONumberAddress);
        END;

    WriteToLogFile (LogEntry, Band, Mode);

    IF NOT ((StringHas (LogEntry, '*DUPE*')) OR (StringHas (LogEntry, '*ZERO*'))) THEN
        BEGIN
        Inc (ValidContacts^ [Band, Mode]);
        Inc (PageValidContacts^ [Band, Mode]);

        IF AccumulateQSOPoints THEN
            BEGIN
            QSOPoints := GetLogEntryQSOPoints (LogEntry);
            PageQSOPoints^ [Band, Mode] := PageQSOPoints^ [Band, Mode] + QSOPoints;
            LogFileQSOPoints^ [Band, Mode] := LogFileQSOPoints^ [Band, Mode] + QSOPoints;
            END;

        IF AccumulateMultipliers THEN
            BEGIN
            MultString := GetLogEntryMultString (LogEntry);

            IF MultString <> '' THEN
                BEGIN
                IF StringHas (MultString, ' ') THEN
                    BEGIN
                    MultArray [1] := PrecedingString  (MultString, ' ');
                    MultArray [2] := PostcedingString (MultString, ' ');
                    NumberMults := 2;
                    END
                ELSE
                    BEGIN
                    MultArray [1] := MultString;
                    NumberMults := 1;
                    END;

                FOR Mult := 1 TO NumberMults DO
                    BEGIN
                    MultIdentified := False;

                    IF DoingDomesticMults AND StringHasLowerCase (MultArray [Mult]) THEN
                        BEGIN
                        Inc (NumberPageDomesticMults^ [Band, Mode]);
                        MultIdentified := True;
                        END;

                    IF DoingZoneMults AND StringIsAllNumbers (MultArray [Mult]) AND NOT MultIdentified THEN
                        BEGIN
                        Inc (NumberPageZoneMults^ [Band, Mode]);
                        MultIdentified := True;
                        END;

                    IF DoingPrefixMults AND NOT MultIdentified THEN
                        BEGIN
                        Inc (NumberPagePrefixMults^ [Band, Mode]);
                        MultIdentified := True;
                        END;

                    IF DoingDXMults AND NOT MultIdentified THEN
                        IF MultArray [Mult] <> '' THEN
                            Inc (NumberPageDXMults^ [Band, Mode]);
                    END;
                END;
            END;
        END;
    END;



FUNCTION EditLine (InitialString: Str160): Str160;

VAR Key: CHAR;
    InputString, TempString: Str80;
    SavedCursorPosition: INTEGER;

    BEGIN
    ClrEol;
    Write (InitialString);
    InputString := InitialString;

    REPEAT
        REPEAT UNTIL KeyPressed;
        Key := ReadKey;

        CASE Key OF
            EscapeKey:
                BEGIN
                GoToXY (1, WhereY);
                TextColor (Cyan);
                Write (InitialString);
                EditLine := InitialString;
                Exit;
                END;

            ControlA:
                BEGIN
                IF WhereX > 1 THEN
                    REPEAT
                        GoToXY (WhereX - 1, WhereY);
                    UNTIL (WhereX = 1) OR
                          ((InputString [WhereX - 1] = ' ') AND (InputString [WhereX] <> ' '));
                END;

            BackSpace:
                BEGIN
                GoToXY (WhereX - 1, WhereY);
                InputString [WhereX] := ' ';
                Write (' ');
                GoToXY (WhereX - 1, WhereY);
                END;

            ControlS:
                BEGIN
                IF WhereX > 1 THEN
                    GoToXY (WhereX - 1, WhereY);
                END;

            ControlD:
                BEGIN
                IF WhereX < 1 + Length (InputString) THEN
                    GoToXY (WhereX + 1, WhereY);
                END;

            ControlF:
                BEGIN
                IF WhereX < 1 + Length (InputString) THEN
                    REPEAT
                        GoToXY (WhereX + 1, WhereY);
                    UNTIL ((InputString [WhereX - 1] = ' ') AND (InputString [WhereX] <> ' ')) OR
                          (WhereX >= 1 + Length (InputString));
                END;

            NullKey:
                BEGIN
                Key := ReadKey;

                CASE Key OF
                    HomeKey: GoToXY (1, WhereY);

                    LeftArrow:
                        IF WhereX > 1 THEN
                            GoToXY (WhereX - 1, WhereY);

                    RightArrow:
                        IF WhereX < 1 + Length (InputString) THEN
                            GoToXY (WhereX + 1, WhereY);

                    EndKey: GoToXY (1 + Length (InputString), WhereY);
                    END;
                END;

            ELSE
                IF Key >= ' ' THEN
                    BEGIN
                    SavedCursorPosition := WhereX;
                    TempString := Copy (InputString, WhereX, 100);
                    Delete (InputString, WhereX, 100);
                    Delete (TempString, 1, 1);
                    TextColor (Yellow);
                    Write (Key);
                    TempString := Key + TempString;
                    InputString := InputString + TempString;
                    GoToXY (SavedCursorPosition + 1, WhereY);
                    END
                ELSE
                    IF Key = CarriageReturn THEN
                        BEGIN
                        EditLine := InputString;
                        Exit;
                        END;

            END;  { of case }
    UNTIL False;
    END;



PROCEDURE ARRLCompatibleLog;

VAR ARRLString, LogString, DateString, TimeString, TempString, OutputFileName: STRING;
    NumberString: Str20;
    FileRead, FileWrite: TEXT;
    Band: BandType;
    Mode: ModeType;
    QSONumber: INTEGER;
    ExchangeString, SentInformation: Str40;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('PRODUCE ARRL DISK LOG');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This procedure will produce an ARRL compatible disk log from the active log ');
    WriteLn ('file.  This file can be submitted along with a summary sheet to the ARRL for');
    WriteLn ('any of their contests.  It also can be used for the CQ WPX contests.');

    WriteLn;

    OutputFileName := GetResponse ('Enter file name for output : ');
    IF OutputFileName = '' THEN Exit;

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError (LogFileName + 'not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    IF NOT OpenFileForWrite (FileWrite, OutputFileName) THEN
        BEGIN
        ReportError ('Unable to open ' + OutputFileName);
        WaitForKeyPressed;
        Exit;
        END;

    TextColor (Cyan);

    WriteLn ('The ARRL log checkers like to have all of your exchange sent on each log');
    WriteLn ('entry.  Your log already has your QSO number and RS(T) sent.');
    WriteLn;

    SentInformation := GetResponse ('Enter additional exchange information to print : ');

    QSONumber := 0;
    Write (QSONumber);

    REPEAT
        REPEAT
            ReadLn (FileRead, LogString);
            Band := GetLogEntryBand (LogString);
        UNTIL (Band <> NoBand) OR EOF (FileRead);

        IF Band <> NoBand THEN
            BEGIN
            IF (LogString [1] = ';') THEN Continue;

            ExpandTabs (LogString);

            IF LogString [42] = '*' THEN LogString [42] := ' ';

            { Save the exchange part for later }

            ExchangeString := Copy (LogString, 44, Length (LogString) - 43);

            { Get rid of any S&P indicator }

            IF Pos ('$', LogString) > 0 THEN LogString [Pos ('$', LogString)] := ' ';

            Mode := GetLogEntryMode (LogString);

            TimeString := Copy (LogString, LogEntryHourAddress, 2) +
                          Copy (LogString, LogEntryMinuteAddress, 2);

            DateString := UpperCase (Copy (LogString, LogEntryMonthAddress, 3));

            IF DateString = 'JAN' THEN DateString := '01';
            IF DateString = 'FEB' THEN DateString := '02';
            IF DateString = 'MAR' THEN DateString := '03';
            IF DateString = 'APR' THEN DateString := '04';
            IF DateString = 'MAY' THEN DateString := '05';
            IF DateString = 'JUN' THEN DateString := '06';
            IF DateString = 'JUL' THEN DateString := '07';
            IF DateString = 'AUG' THEN DateString := '08';
            IF DateString = 'SEP' THEN DateString := '09';
            IF DateString = 'OCT' THEN DateString := '10';
            IF DateString = 'NOV' THEN DateString := '11';
            IF DateString = 'DEC' THEN DateString := '12';

            DateString := Copy (LogString, LogEntryDayAddress, 2) + '/' + DateString;
            DateString := DateString + '/' + Copy (LogString, LogEntryYearAddress, 2);

            ARRLString := Copy (LogString, LogEntryBandAddress, LogEntryBandWidth);

            IF ARRLString = '  6' THEN
                ARRLString := '50 '
            ELSE
                IF ARRLString = '  2' THEN
                    ARRLString := '144'
                ELSE
                    BEGIN
                    GetRidOfPrecedingSpaces (ARRLString);
                    WHILE Length (ARRLString) < 3 DO
                        ARRLString := ARRLString + ' ';
                    END;

            IF Mode = CW THEN
                ARRLString := ARRLString + ' CW'
            ELSE
                ARRLString := ARRLString + ' PH';

            ARRLString := ARRLString + ' ' + DateString + ' ' + TimeString + ' ';

            Delete (LogString, 1, LogEntryQSONumberAddress - 1);

            NumberString := RemoveFirstString (LogString);

{ Took this out so two xmtr multi-mutil works

            IF Copy (NumberString, Length (NumberString), 1) > '9' THEN
                Delete (NumberString, Length (NumberString), 1);
}

            WHILE Length (NumberString) < 6 DO
                NumberString := NumberString + ' ';

            ARRLString := ARRLString + ' ' + NumberString;

            Call := RemoveFirstString (LogString);

            WHILE Length (Call) < 12 DO Call := Call + ' ';

            ARRLString := ARRLString + ' ' + Call;
            ARRLString := ARRLString + ' ' + SentInformation +  ' ' + ExchangeString;

            GetRidOfPrecedingSpaces (ARRLString);
            WriteLn (FileWrite, ARRLString);
            Inc (QSONumber);
            GoToXY (1, WhereY);
            Write (QSONumber);
            END;
    UNTIL Eof (FileRead);
    Close (FileRead);
    Close (FileWrite);
    END;



PROCEDURE CreateFinalLog;

VAR TempString: Str80;
    Band: BandType;
    Mode: ModeType;
    QSONumber, NumberZeroContacts: INTEGER;
    Command, Key: CHAR;
    FileRead: TEXT;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('CREATE FINAL LOG');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This procedure will allow you to create final logs with running totals of  ');
    WriteLn ('QSOs, QSO points and multipliers.  You can have you logs printed separatly ');
    WriteLn ('by band or mode and have the QSO numbers renumbered.  This procedure will ');
    WriteLn ('only output the logs to the disk.  The files can be found with .DAT as');
    WriteLn ('their extension.');
    WriteLn;

    REPEAT
        Command := Upcase (GetKey ('Separate logs by band? (Y/N) : '));
    UNTIL (Command = 'Y') OR (Command = 'N') OR (Command = EscapeKey);
    WriteLn;

    IF Command = EscapeKey THEN Exit;
    SeparateBandLogs := Command = 'Y';

    REPEAT
        Command := Upcase (GetKey ('Separate logs by mode? (Y/N) : '));
    UNTIL (Command = 'Y') OR (Command = 'N') OR (Command = EscapeKey);
    WriteLn;

    IF Command = EscapeKey THEN Exit;
    SeparateModeLogs := Command = 'Y';

    IF SeparateBandLogs THEN
        BEGIN
        StartBand := Band160;
        StopBand  := Band2304;
        END
    ELSE
        BEGIN
        StartBand := All;
        StopBand  := All;
        END;

    IF SeparateModeLogs THEN
        BEGIN
        StartMode := CW;
        StopMode  := Phone;
        END
    ELSE
        BEGIN
        StartMode := Both;
        StopMode  := Both;
        END;

    REPEAT
        Command := UpCase (GetKey ('Renumber QSO numbers? (Y/N) : '));
    UNTIL (Command = 'Y') OR (Command = 'N') OR (Command = EscapeKey);
    WriteLn;

    IF Command = EscapeKey THEN Exit;
    ReNumberQSONumbers := Command = 'Y';

    REPEAT
        Command := UpCase (GetKey ('Accumulate QSO points? (Y/N) : '));
    UNTIL (Command = 'Y') OR (Command = 'N') OR (Command = EscapeKey);
    WriteLn;

    IF Command = EscapeKey THEN Exit;
    AccumulateQSOPoints := Command = 'Y';

    REPEAT
        Command := UpCase (GetKey ('Accumulate multipliers? (Y/N) : '));
    UNTIL (Command = 'Y') OR (Command = 'N') OR (Command = EscapeKey);
    WriteLn;

    IF Command = EscapeKey THEN Exit;
    AccumulateMultipliers := Command = 'Y';

    IF AccumulateMultipliers THEN
        IF NOT DetermineMultiplierTypes THEN Exit;

    NumberZeroContacts := 0;

    IF NOT FileExists (LogFileName) THEN
        BEGIN
        ReportError ('No logfile file found!!');
        WaitForKeyPressed;
        Exit;
        END;

    QSOsPerPage := GetValue ('Enter number of contacts per page for output : ');

    DetermineLogTotals;
    OpenFilesForBandsBeingProcessed;

    GetCopyOfHeader;

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN Exit;

    QSONumber := 0;
    Write (QSONumber);

    REPEAT
        REPEAT
            ReadLn (FileRead, TempString);
            Band := GetLogEntryBand (TempString);
        UNTIL (Band <> NoBand) OR EOF (FileRead);

        IF (Band <> NoBand) THEN
            BEGIN
            ExpandTabs (TempString);
            Mode := GetLogEntryMode (TempString);

            IF NOT SeparateBandLogs THEN Band := All;
            IF NOT SeparateModeLogs THEN Mode := Both;

            WriteLogEntry (TempString, Band, Mode);

            Inc (QSONumber);
            GoToXY (1, WhereY);
            TextColor (Yellow);
            Write (QSONumber);
            END;
    UNTIL EOF (FileRead);

    FOR Band := StartBand TO StopBand DO
        FOR Mode := StartMode TO StopMode DO
            IF LogTotals [Band, Mode] > 0 THEN
                BEGIN
                TotalStamp (Band, Mode);
                WriteToLogFile (ControlL, Band, Mode);
                END;

    Close (FileRead);
    CloseAllOpenFiles;
    GoToXY (1, WhereY);
    ClrEol;
    END;



FUNCTION DupeLog: BOOLEAN;

VAR Band, DupeBand: BandType;
    Mode, DupeMode: ModeType;
    FileRead, FileWrite: TEXT;
    DupeFileName, TempString, FileString: Str80;
    QSONumber, DupePos, NumberDupesUnmarked, NumberZeros: INTEGER;
    RXData: ContestExchange;
    Key, Command: CHAR;
    PossibleDupeList: ARRAY [0..100] OF CallString;
    PossibleDupe, NumberPossibleDupes: INTEGER;
    FileDupe: TEXT;
    Grid: Str20;

    BEGIN
    DupeLog := False;
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('CHECK ACTIVE LOG FOR DUPES');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This procedure will look through the active log file and flag any duplicate');
    WriteLn ('contacts.  It actually performs a dupe check for each callsign.  If a dupe is');
    WriteLn ('found, it will be marked with the word DUPE and the QSO points will be set to');
    WriteLn ('zero.  It is always a good idea to execute this procedure after a contest as ');
    WriteLn ('it is possible for you to use the edit log feature to create duplicate contacts');
    WriteLn ('without your knowledge.  The original unduped log will be saved as PLOG###.BAK.');
    WriteLn ('The duped log will have the active log file name.  This option also generates a');
    WriteLn ('list of the QSOs that are dupes, and saves the list to a separate file.');
    WriteLn;

    DetermineQSOByBandAndQSOByMode;
    IF NOT QSOByBandAndModeDetermined THEN Exit;

    DoingDomesticMults := False;
    DoingDXMults :=       False;
    DoingZoneMults :=     False;
    DoingPrefixMults :=   False;

    RXData.DomesticMult := False;
    RXData.DXMult := False;
    RXData.ZoneMult := False;
    RXData.PrefixMult := False;

    Sheet.DisposeOfMemoryAndZeroTotals;

    NumberZeros := 0;
    NumberPossibleDupes := 0;
    NumberDupesUnmarked := 0;

    REPEAT
        Key := UpCase (GetKey ('Do you want to save a copy of the duplicate QSOs? (Y/N) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'Y') OR (Key = 'N');
    WriteLn;

    IF Key = 'Y' THEN
        DupeFileName := GetResponse ('Enter filename to save dupes to (use PRN for printer) : ')
    ELSE
        DupeFileName := '';

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError ('Unable to open ' + LogFileName + '!!');
        WaitForKeyPressed;
        Exit;
        END;

    IF NOT OpenFileForWrite (FileWrite, TempFileName) THEN
        BEGIN
        Close (FileRead);
        ReportError ('Unable to open ' + TempFileName + ' for output!!');
        WaitForKeyPressed;
        Exit;
        END;

    IF DupeFileName <> '' THEN
        IF NOT OpenFileForWrite (FileDupe,  DupeFileName) THEN Exit;

    QSONumber := 0;
    Write (QSONumber);

    WHILE NOT Eof (FileRead) DO
        BEGIN
        ReadLn (FileRead, FileString);
        Band := GetLogEntryBand (FileString);

        IF (Band <> NoBand) THEN
            BEGIN
            ExpandTabs (FileString);
            Mode := GetLogEntryMode (FileString);

            IF QSOByBand THEN RXData.Band := Band ELSE RXData.Band := All;
            IF QSOByMode THEN RXData.Mode := Mode ELSE RXData.Mode := Both;

            RXData.Callsign := UpperCase (StandardCallFormat (GetLogEntryCall (FileString), True));

            IF RoverCall (RXData.Callsign) THEN
                BEGIN
                TempString := GetLogEntryExchangeString (FileString);

                RemoveFirstString (TempString);
                RemoveFirstString (TempString);
                Grid := UpperCase (RemoveFirstString (TempString));

                RXData.Callsign := PrecedingString (RXData.Callsign, '/R') + '/' + Grid;
                END;

            IF Sheet.CallIsADupe (RXData.Callsign, RXData.Band, RXData.Mode) THEN
                BlankOutMultsAndZeroQSOPoints (FileString, '*DUPE*')
            ELSE
                IF RXData.Callsign = '' THEN
                    BlankOutMultsAndZeroQSOPoints (FileString, '*ZERO*')
                ELSE
                    BEGIN
                    Sheet.AddQSOToSheets (RXData);

                    DupePos := Pos ('*DUPE*', FileString);

                    IF DupePos > 0 THEN
                        BEGIN
                        TextColor (Yellow);
                        GoToXY (1, WhereY);
                        WriteLn (FileString);

                        REPEAT
                            Key := UpCase (GetKey ('This QSO should not be marked as a dupe.  Okay to fix? (Y/N) : '));
                        UNTIL (Key = 'Y') OR (Key = 'N');

                        GoToXY (1, WhereY);
                        ClrEol;

                        IF Key = 'Y' THEN
                            BEGIN
                            Inc (NumberDupesUnMarked);
                            Delete (FileString, DupePos, 6);
                            Insert ('      ', FileString, DupePos);
                            GoToXY (1, WhereY - 1);
                            TextColor (Yellow);
                            WriteLn (FileString);
                            END
                        ELSE
                            BEGIN
                            GoToXY (1, WhereY - 1);
                            ClrEol;
                            END;
                        END;
                    END;

            IF (StringHas (FileString, '*DUPE*')) OR (StringHas (FileString, '*ZERO*')) THEN
                BEGIN
                GoToXY (1, WhereY);
                TextColor (Yellow);
                WriteLn (FileString);
                IF DupeFileName <> '' THEN WriteLn (FileDupe, FileString);
                Inc (NumberZeros);
                END;

            Inc (QSONumber);
            GoToXY (1, WhereY);
            ClrEol;
            TextColor (Cyan);
            Write (QSONumber);
            END;

        WriteLn (FileWrite, FileString);
        END;

    IF DupeFileName <> '' THEN Close (FileDupe);

    Close (FileRead);
    Close (FileWrite);

    PushLogFiles;
    RenameFile (TempFileName, LogFileName);

    TextColor (Cyan);
    GoToXY (1, WhereY);
    ClrEol;
    WriteLn;
    WriteLn ('Original log saved as ', LastPushedLogName);

    WriteLn ('Now doing second pass to look for possible portable dupes...');

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN Exit;

    QSONumber := 0;
    Write (QSONumber);

    Sheet.DisposeOfMemoryAndZeroTotals;

    WHILE NOT Eof (FileRead) DO
        BEGIN
        ReadLn (FileRead, FileString);
        Band := GetLogEntryBand (FileString);

        IF (Band <> NoBand) THEN
            BEGIN
            ExpandTabs (FileString);
            Mode := GetLogEntryMode (FileString);

            IF QSOByBand THEN RXData.Band := Band ELSE RXData.Band := All;
            IF QSOByMode THEN RXData.Mode := Mode ELSE RXData.Mode := Both;

            RXData.Callsign := RootCall (StandardCallFormat (GetLogEntryCall (FileString), True));

            IF NOT RoverCall (GetLogEntryCall (FileString)) THEN
                BEGIN
                IF Sheet.CallIsADupe (RXData.Callsign, RXData.Band, RXData.Mode) THEN
                    BEGIN
                    IF NOT (StringHas (FileString, '*DUPE*') OR StringHas (FileString, '*ZERO*')) THEN
                        BEGIN
                        PossibleDupeList [NumberPossibleDupes] := RootCall (RXData.Callsign);
                        Inc (NumberPossibleDupes);
                        END;
                    END
                ELSE
                    Sheet.AddQSOToSheets (RXData);
                END;

            Inc (QSONumber);
            GoToXY (1, WhereY);
            ClrEol;
            Write (QSONumber);
            END;
        END;

    Close (FileRead);

    Sheet.DisposeOfMemoryAndZeroTotals;

    GoToXY (1, WhereY);
    ClrEol;
    WriteLn;
    WriteLn ('There were ', QSONumber, ' contacts found in the log.');
    WriteLn ('There were ', NumberZeros, ' exact match dupes marked.');

    IF NumberPossibleDupes > 0 THEN
        BEGIN
        WriteLn;

        IF NumberPossibleDupes = 1 THEN
            WriteLn ('There was one possible dupe found in your log.  It is suggested that you   ')
        ELSE
            WriteLn ('There were ', NumberPossibleDupes, ' possible dupes found.  It is suggested that you');

        WriteLn ('use the Edit command and look carefully at the QSOs with the stations listed.');
        WriteLn ('You will probably decide that there is a duplicate QSO somewhere.  Edit the');
        WriteLn ('duplicate QSO so the callsigns are the same and rerun this procedure.');
        WriteLn;
        TempString := '';
        TextColor (Yellow);

        FOR PossibleDupe := 0 TO NumberPossibleDupes - 1 DO
            BEGIN
            IF Length (TempString) > 70 THEN
                BEGIN
                WriteLn (TempString);
                TempString := '';
                END;
            TempString := TempString + PossibleDupeList [PossibleDupe] + ' ';
            END;

        IF TempString <> '' THEN WriteLn (TempString);
        WaitForKeyPressed;
        Exit;
        END
    ELSE
        WriteLn ('There were no possible portable dupes found.');

    IF NumberDupesUnmarked > 0 THEN
        BEGIN
        WriteLn;
        WriteLn ('There were QSOs in your log that were marked as dupes but no longer are.');
        WriteLn ('You will need to fix the QSO points for those contacts one of two ways.');
        WriteLn ('Either edit the log file and insert the correct points manuall, or use the');
        WriteLn ('TR READ feature of TR to "rework" the log and assure the proper QSO points');
        WriteLn ('appear.  To use the TR READ feature, rename the log file to LOGREAD.DAT, and ');
        WriteLn ('type TR READ LOGREAD.DAT.');
        WriteLn;
        END;

    WaitForKeyPressed;
    DupeLog := True;
    END;



PROCEDURE EditLog;

VAR Call, TempString, FileString: Str80;
    FileWrite: TEXT;
    Key: CHAR;
    VAR FileRead: TEXT;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('LINE EDIT PROCEDURE');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('This procedure allows you to edit specific lines of the log.  You will be');
    WriteLn ('asked for a search string (case sensitive).  Then each log entry found');
    WriteLn ('with the search string will be shown to you one at a time.  You can edit each');
    WriteLn ('entry using the cursor keys, backspace and overwrite.  The non insert mode is');
    WriteLn ('the only mode available to you.  Be careful to keep columns aligned.');
    WriteLn;

    IF NOT FileExists (LogFileName) THEN
        BEGIN
        ReportError ('No logfile file found.');
        WaitForKeyPressed;
        Exit;
        END;

    Call := GetResponse ('Enter search string to find (none to quit) : ');
    IF Call = '' THEN Exit;

    WriteLn;
    TextColor (Cyan);
    WriteLn ('For each entry found, edit the line and press return when done.');
    WriteLn ('Escape will abort any changes made for the line being edited.');
    GoToXY (LogEntryMultAddress - 8, WhereY);
    Write ('Mults start here');
    WriteLn;
    GoToXY (LogEntryMultAddress, WhereY);
    WriteLn ('');

    RenameFile (LogFileName, 'POST.TMP');

    IF NOT OpenFileForRead (FileRead, 'POST.TMP') THEN
        BEGIN
        ReportError ('Unable to open POST.TMP');
        WaitForKeyPressed;
        Exit;
        END;

    IF NOT OpenFileForWrite (FileWrite, LogFileName) THEN
        BEGIN
        ReportError ('Unable to open ' + LogFileName + ' for output.');
        WaitForKeyPressed;
        Exit;
        END;

    WHILE NOT Eof (FileRead) DO
        BEGIN
        ReadLn (FileRead, FileString);

        IF StringHas (FileString, Call) THEN
            BEGIN
            ExpandTabs (FileString);
            TextColor (Cyan);
            WriteLn (FileWrite, EditLine (FileString));
            WriteLn;
            END
        ELSE
            WriteLn (FileWrite, FileString);
        END;

    Close (FileRead);
    Close (FileWrite);

    WriteLn;
    REPEAT
        Key := UpCase (GetKey ('Do you want to save the changes you have just made? (Y/N) : '));
    UNTIL (Key = 'Y') OR (Key = 'N');
    WriteLn;

    IF Key = 'N' THEN
        BEGIN
        DeleteFile (LogFileName);
        RenameFile ('POST.TMP', LogFileName);
        WriteLn ('Original ', LogFileName, ' restored.');
        END
    ELSE
        WriteLn ('Changes saved to ', LogFileName, '.  Old file saved as POST.TMP.');

    WaitForKeyPressed;
    END;



PROCEDURE FilterLog;

VAR QSONumber, Country, NumberEntriesSaved, NumberCountries: INTEGER;
    LogEntryCountry: INTEGER;
    CountryList: ARRAY [0..19] OF INTEGER;
    CludeString, Call: CallString;
    FileName, FileString, TempString: Str80;
    Destination, CludeKey, Key: CHAR;
    FileRead, FileWrite: TEXT;
    FoundCountry: BOOLEAN;
    Band: BandType;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('FILTER LOG PROCEDURE');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This procedure will allow you to filter a log file by including or excluding');
    WriteLn ('up to 20 ARRL countries.  The output is a new log file with a different name.');
    WriteLn ('To enter the countries, simply enter any callsign from that country.');
    WriteLn;

    REPEAT
        CludeKey := UpCase (GetKey ('(I)nclude or (E)xclude countries? (I/E) : '));
        IF CludeKey = EscapeKey THEN Exit;
    UNTIL (CludeKey = 'I') OR (CludeKey = 'E');
    WriteLn;
    IF CludeKey = 'I' THEN CludeString := 'include' ELSE CludeString := 'exclude';

    TextColor (Cyan);
    WriteLn;
    WriteLn ('Enter callsigns of countries you want to ', CludeString, '.  Press RETURN with no');
    WriteLn ('entry to stop entering callsigns.');

    NumberCountries := 0;
    NumberEntriesSaved := 0;

    REPEAT
        Call := UpperCase (GetResponse ('Enter callsign of country to ' + CludeString + ' : '));

        IF Call <> '' THEN
            IF NumberCountries < 20 THEN
                BEGIN
                CountryList [NumberCountries] := CountryTable.GetCountry (Call, False);
                Inc (NumberCountries);
                END
            ELSE
                ReportError ('Too many coutries!!  Input ignored.  Press RETURN with no entry to stop.');
    UNTIL Call = '';

    IF NumberCountries = 0 THEN Exit;

    ClrScr;
    TextColor (Cyan);

    WriteLn ('You have choosen to ', CludeString, ' the following countries: ');

    FOR Country := 0 TO NumberCountries - 1 DO
        WriteLn (Country + 1, '.  ', CountryTable.GetCountryName (CountryList [Country]));

    WriteLn;

    REPEAT
        Key := UpCase (GetKey ('Is this list correct? (Y/N) : '));
        IF (Key = EscapeKey) OR (Key = 'N') THEN Exit;
    UNTIL (Key = 'Y');

    ClrScr;

    REPEAT
    Destination := UpCase (GetKey ('Output to (F)ile, (P)rinter or (S)creen? : '));
        CASE Destination OF
            'F': BEGIN
                 WriteLn;
                 FileName := UpperCase (GetResponse ('Enter filename to filtered log file to : '));

                 IF FileName = UpperCase (LogFileName) THEN
                     BEGIN
                     ReportError ('Output file must be different than active log file!!');
                     WaitForKeyPressed;
                     Exit;
                     END;

                 IF FileName = '' THEN Exit;
                 END;
            'S': FileName := '';
            'P': FileName := 'PRN';
            EscapeKey: Exit;
            END;
    UNTIL (Destination = 'F') OR (Destination = 'S') OR (Destination = 'P');
    WriteLn;

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError (LogFileName + ' not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    IF NOT OpenFileForWrite (FileWrite, FileName) THEN
        BEGIN
        ReportError ('Unable to open output file or device.');
        WaitForKeyPressed;
        Exit;
        END;

    TextColor (Cyan);
    QSONumber := 0;

    REPEAT
        REPEAT
            ReadLn (FileRead, FileString);
            Band := GetLogEntryBand (FileString);
        UNTIL (Band <> NoBand) OR EOF (FileRead);

        IF Band <> NoBand THEN
            BEGIN
            Inc (QSONumber);
            GoToXY (1, WhereY);
            Write (QSONumber);
            ExpandTabs (FileString);
            Call := UpperCase (StandardCallFormat (GetLogEntryCall (FileString), False));
            LogEntryCountry := CountryTable.GetCountry (Call, True);

            IF CludeKey = 'I' THEN
                BEGIN
                FOR Country := 0 TO NumberCountries - 1 DO
                    IF LogEntryCountry = CountryList [Country] THEN
                        BEGIN
                        WriteLn (FileWrite, FileString);
                        Inc (NumberEntriesSaved);
                        END;
                END
            ELSE
                BEGIN
                FoundCountry := False;
                FOR Country := 0 TO NumberCountries - 1 DO
                    IF LogEntryCountry = CountryList [Country] THEN
                        FoundCountry := True;
                IF NOT FoundCountry THEN
                    BEGIN
                    WriteLn (FileWrite, FileString);
                    Inc (NumberEntriesSaved);
                    END;
                END;
            END;

    UNTIL Eof (FileRead);
    Close (FileRead);
    Close (FileWrite);
    GoToXY (1, WhereY);
    ClrEol;
    WriteLn ('There were ', NumberEntriesSaved, ' entries saved.');
    WaitForKeyPressed;
    END;



FUNCTION QuasiDayOfYear (DateString: Str20): INTEGER;

VAR TempDay, Result, Day: INTEGER;
    DayString: Str20;

    BEGIN
    DateString := UpperCase (DateString);
    Day := 0;

    IF StringHas (DateString, 'FEB') THEN Day :=  31;
    IF StringHas (DateString, 'MAR') THEN Day :=  62;
    IF StringHas (DateString, 'APR') THEN Day :=  93;
    IF StringHas (DateString, 'MAY') THEN Day := 124;
    IF StringHas (DateString, 'JUN') THEN Day := 155;
    IF StringHas (DateString, 'JUL') THEN Day := 186;
    IF StringHas (DateString, 'AUG') THEN Day := 217;
    IF StringHas (DateString, 'SEP') THEN Day := 248;
    IF StringHas (DateString, 'OCT') THEN Day := 279;
    IF StringHas (DateString, 'NOV') THEN Day := 310;
    IF StringHas (DateString, 'DEC') THEN Day := 341;

    DayString := PrecedingString (DateString, '-');

    Val (DayString, TempDay, Result);
    IF Result = 0 THEN Day := Day + TempDay;

    QuasiDayOfYear := Day;
    END;



FUNCTION GetNextLogEntry (VAR FileHandle: TEXT): STRING;

VAR FileString: STRING;

    BEGIN
    GetNextLogEntry := '';

    WHILE NOT Eof (FileHandle) DO
        BEGIN
        ReadLn (FileHandle, FileString);

        IF (GetLogEntryBand (FileString) <> NoBand) AND
           (GetLogEntryMode (FileString) <> NoMode) THEN
               BEGIN
               GetNextLogEntry := FileString;
               Exit;
               END;
        END;
    END;



PROCEDURE Merge;

VAR FileOneName, FileTwoName, OutputFileName: Str80;
    FileOne, FileTwo, OutputFile: TEXT;
    FileStringOne, FileStringTwo: Str80;
    Address, DayOfYearOne, DayOfYearTwo: INTEGER;
    IntegerTimeOne, IntegerTimeTwo: INTEGER;
    NeedNewQSOForFileOne, NeedNewQSOForFileTwo: BOOLEAN;
    Command: CHAR;
    TotalFileOneContacts, TotalFileTwoContacts, TotalContacts: INTEGER;
    LTQW: ARRAY [0..29] OF Str80;
    LTQWHead, LTQWTail: INTEGER;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('MERGE UTILITY');
    TextColor (Cyan);

    WriteLn;
    WriteLn ('This procedure will merge two log files together into a single log file');
    WriteLn ('They will be shuffled so the result is in order by time and date.  Obvious');
    WriteLn (' duplicate contacts will be removed when they appear in both logs.');
    WriteLn;
    WriteLn ('If you have more than two logs to combine, you will need to run this procedure');
    WriteLn ('multiple times and use a unique intermediate file name for the output file each');
    WriteLn ('time.  Use the desired finished log name when merging the last log.');
    WriteLn;

    FileOneName := GetResponse ('Enter the filename of the first file to merge : ');
    IF FileOneName = '' THEN Exit;

    FileTwoName := GetResponse ('Enter the filename of the second file to merge : ');
    IF FileTwoName = '' THEN Exit;

    OutputFileName := GetResponse ('Enter filename for the combined log : ');
    IF OutputFileName = '' THEN Exit;

    IF (OutputFileName = FileOneName) OR (OutputFileName = FileTwoName) THEN
        BEGIN
        ReportError ('ERROR!! You have specified on of the input files as the output file!!');
        Exit;
        END;

    IF NOT OpenFileForRead (FileOne, FileOneName) THEN
        BEGIN
        ReportError (FileOneName + ' not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    IF NOT OpenFileForRead (FileTwo, FileTwoName) THEN
        BEGIN
        ReportError (FileTwoName + ' not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    OpenFileForWrite (OutputFile, OutputFileName);

    WriteLn ('Loading in logs...');

    TotalContacts := 0;
    TotalFileOneContacts := 0;
    TotalFileTwoContacts := 0;

    NeedNewQSOForFileOne := True;
    NeedNewQSOForFileTwo := True;

    LTQWHead := 0;
    LTQWTail := 0;

    REPEAT
        IF NeedNewQSOForFileOne THEN
            REPEAT
                FileStringOne := GetNextLogEntry (FileOne);

                DayOfYearOne := QuasiDayOfYear (GetLogEntryDateString (FileStringOne));
                IntegerTimeOne := GetLogEntryIntegerTime (FileStringOne);

                IF FileStringOne <> '' THEN Inc (TotalFileOneContacts);
                NeedNewQSOForFileOne := False;

                IF LTQWHead <> LTQWTail THEN
                    BEGIN
                    Address := LTQWTail;

                    REPEAT
                        IF (GetLogEntryCall (LTQW [Address]) = GetLogEntryCall (FileStringOne)) AND
                           (GetLogEntryBand (LTQW [Address]) = GetLogEntryBand (FileStringOne)) AND
                           (GetLogEntryMode (LTQW [Address]) = GetLogEntryMode (FileStringOne)) AND
                           (GetLogEntryIntegerTime (LTQW [Address]) = GetLogEntryIntegerTime (FileStringOne)) THEN
                               NeedNewQSOForFileOne := True;

                        Address := (Address + 1) MOD 30;
                    UNTIL (Address = LTQWHead) OR NeedNewQSOForFileOne;
                    END;
            UNTIL NOT NeedNewQSOForFileOne;

        IF NeedNewQSOForFileTwo THEN
            REPEAT
                FileStringTwo := GetNextLogEntry (FileTwo);

                DayOfYearTwo := QuasiDayOfYear (GetLogEntryDateString (FileStringTwo));
                IntegerTimeTwo := GetLogEntryIntegerTime (FileStringTwo);

                IF FileStringTwo <> '' THEN Inc (TotalFileTwoContacts);
                NeedNewQSOForFileTwo := False;

                IF LTQWHead <> LTQWTail THEN
                    BEGIN
                    Address := LTQWTail;

                    REPEAT
                        IF (GetLogEntryCall (LTQW [Address]) = GetLogEntryCall (FileStringTwo)) AND
                           (GetLogEntryBand (LTQW [Address]) = GetLogEntryBand (FileStringTwo)) AND
                           (GetLogEntryMode (LTQW [Address]) = GetLogEntryMode (FileStringTwo)) AND
                           (GetLogEntryIntegerTime (LTQW [Address]) = GetLogEntryIntegerTime (FileStringTwo)) THEN
                               NeedNewQSOForFileTwo := True;

                        Address := (Address + 1) MOD 30;
                    UNTIL (Address = LTQWHead) OR NeedNewQSOForFileTwo;
                    END;

            UNTIL NOT NeedNewQSOForFileTwo;

        GoToXY (1, WhereY);
        Write ('File 1 = ', TotalFileOneContacts:4, '   File 2 = ', TotalFileTwoContacts:4);

        IF (FileStringOne = '') AND (FileStringTwo = '') THEN
            BEGIN
            Close (OutputFile);
            Close (FileOne);
            Close (FileTwo);
            GoToXY (1, WhereY);
            WriteLn;
            WriteLn ('There were ', TotalFileOneContacts, ' contacts found in ', FileOneName);
            WriteLn ('There were ', TotalFileTwoContacts, ' contacts found in ', FileTwoName);
            WriteLn ('There are ', TotalContacts, ' total contacts saved in ', OutputFileName);

            IF TotalContacts <> (TotalFileOneContacts + TotalFileTwoContacts) THEN
                WriteLn ('There were ', (TotalFileOneContacts + TotalFileTwoContacts) - TotalContacts,
                         ' matched QSOs found which were only saved once.');

            WaitForKeyPressed;
            Exit;
            END;

        IF ((FileStringOne <> '') AND (DayOfYearOne < DayOfYearTwo)) OR (FileStringTwo = '') THEN
            BEGIN
            Inc (TotalContacts);
            WriteLn (OutputFile, FileStringOne);

            LTQW [LTQWHead] := FileStringOne;
            LTQWHead := (LTQWHead + 1) MOD 30;
            IF LTQWHead = LTQWTail THEN LTQWTail := (LTQWTail + 1) MOD 30;

            NeedNewQSOForFileOne := True;
            END
        ELSE
            IF ((FileStringTwo <> '') AND (DayOfYearOne > DayOfYearTwo)) OR (FileStringOne = '') THEN
                BEGIN
                Inc (TotalContacts);
                WriteLn (OutputFile, FileStringTwo);

                LTQW [LTQWHead] := FileStringTwo;
                LTQWHead := (LTQWHead + 1) MOD 30;
                IF LTQWHead = LTQWTail THEN LTQWTail := (LTQWTail + 1) MOD 30;

                NeedNewQSOForFileTwo := True;
                END
            ELSE
                IF IntegerTimeOne < IntegerTimeTwo THEN
                    BEGIN
                    Inc (TotalContacts);
                    WriteLn (OutputFile, FileStringOne);

                    LTQW [LTQWHead] := FileStringOne;
                    LTQWHead := (LTQWHead + 1) MOD 30;
                    IF LTQWHead = LTQWTail THEN LTQWTail := (LTQWTail + 1) MOD 30;

                    NeedNewQSOForFileOne := True;
                    END
                ELSE
                    IF IntegerTimeOne > IntegerTimeTwo THEN
                        BEGIN
                        Inc (TotalContacts);
                        WriteLn (OutputFile, FileStringTwo);

                        LTQW [LTQWHead] := FileStringTwo;
                        LTQWHead := (LTQWHead + 1) MOD 30;
                        IF LTQWHead = LTQWTail THEN LTQWTail := (LTQWTail + 1) MOD 30;

                        NeedNewQSOForFileTwo := True;
                        END
                    ELSE
                        BEGIN
                        IF (GetLogEntryCall (FileStringOne) = GetLogEntryCall (FileStringTwo)) AND
                           (GetLogEntryBand (FileStringOne) = GetLogEntryBand (FileStringTwo)) AND
                           (GetLogEntryMode (FileStringOne) = GetLogEntryMode (FileStringTwo)) THEN
                               BEGIN
                               Inc (TotalContacts);
                               WriteLn (OutputFile, FileStringOne); { A dupe }
                               NeedNewQSOForFileOne := True;
                               NeedNewQSOForFileTwo := True;
                               END
                           ELSE
                               BEGIN
                               Inc (TotalContacts);
                               WriteLn (OutputFile, FileStringOne);
                               NeedNewQSOForFIleOne := True;
                               END;
                        END;

    UNTIL False;
    END;



PROCEDURE MakeKCJLog;

VAR KCJString, LogString, TimeString, TempString, OutputFileName: Str80;
    FileRead, FileWrite: TEXT;
    Band: BandType;
    Mode: ModeType;
    DayNumber, QSONumber: INTEGER;
    BandChar: CHAR;
    Call: CallString;
    LastTime, RSTSent, RSTReceived, QTHReceived: Str20;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('PRODUCE KCJ DISK FILE');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This procedure will produce a disk file in the format used by the KCJ contest');
    WriteLn ('log checkers.  Submission of this file does not excuse you from sending in a');
    WriteLn ('printed log.  It is just a nice thing to do, so they don''t have to manually');
    WriteLn ('enter your log.');
    WriteLn;

    OutputFileName := GetResponse ('Enter file name for output : ');
    IF OutputFileName = '' THEN Exit;

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError (LogFileName + 'not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    IF NOT OpenFileForWrite (FileWrite, OutputFileName) THEN
        BEGIN
        ReportError ('Unable to open ' + OutputFileName);
        WaitForKeyPressed;
        Exit;
        END;

    QSONumber := 0;
    Write (QSONumber);
    DayNumber := 0;
    LastTime := '0000';

    REPEAT
        REPEAT
            ReadLn (FileRead, LogString);
            Band := GetLogEntryBand (LogString);
        UNTIL (Band <> NoBand) OR EOF (FileRead);

        IF Band <> NoBand THEN
            BEGIN
            CASE Band OF
                Band160: BandChar := 'A';
                Band80:  BandChar := 'B';
                Band40:  BandChar := 'C';
                Band20:  BandChar := 'D';
                Band15:  BandChar := 'E';
                Band10:  BandChar := 'F';
                Band6:   BandChar := 'G';
                ELSE     BandChar := '?';
                END;

            ExpandTabs (LogString);

            TimeString := Copy (LogString, LogEntryHourAddress, 2) +
                          Copy (LogString, LogEntryMinuteAddress, 2);

            IF LastTime > TimeString THEN
                Inc (DayNumber);

            IF DayNumber > 0 THEN
                BEGIN
                TimeString [1] := Chr (Ord (TimeString [1]) + 2);
                TimeString [2] := Chr (Ord (TimeString [2]) + 4);

                IF TimeString [2] > '9' THEN
                    BEGIN
                    TimeString [2] := Chr (Ord (TimeString [2]) - 10);
                    TimeString [1] := Chr (Ord (TimeString [1]) + 1);
                    END;
                END;

            LastTime := TimeString;

            Call := GetLogEntryCall (LogString);

            WHILE Length (Call) < 11 DO
                Call := Call + ' ';

            TempString := GetLogEntryExchangeString (LogString);
            RSTSent := RemoveFirstString (TempString);
            RSTReceived := RemoveFirstString (TempString);
            QTHReceived := UpperCase (RemoveFirstString (TempString));

            KCJString := TimeString + BandChar + Call + RSTSent +
                         RSTReceived + QTHReceived;

            IF GetLogEntryQSOPoints (LogString) = 0 THEN
                KCJString := KCJString + ' '
            ELSE
                IF GetLogEntryMultString (LogString) <> '' THEN
                    KCJString := KCJString + ':'
                ELSE
                    KCJString := KCJString + '.';

            KCJString := KCJString + '     ';
            WriteLn (FileWrite, KCJString);

            Inc (QSONumber);
            GoToXY (1, WhereY);
            Write (QSONumber);
            END;

    UNTIL Eof (FileRead);
    Close (FileRead);
    Close (FileWrite);
    END;



FUNCTION CheckGetPrefix (Call: CallString): Str20;

VAR Prefix, StandardCall: CallString;

    BEGIN
    StandardCall := StandardCallFormat (Call, True);

    IF StringHas (StandardCall, '/') THEN
        BEGIN
        Prefix := PrecedingString (StandardCallFormat (Call, True), '/');

        {KK1L: 6.68 Added AM to allow for aeronautical mobile stations}
        IF (Copy (Prefix, 1, 2) = 'MM') OR (Copy (Prefix, 1, 2) = 'AM')  THEN
                    BEGIN
                    Prefix := GetPrefix (PostcedingString (Prefix, '/'));
                    Exit;
                    END;

        IF NOT StringHasNumber (Prefix) THEN
                    Prefix := Prefix + '0';

        CheckGetPrefix := GetPrefix (Prefix);
        END
    ELSE
        CheckGetPrefix := GetPrefix (StandardCall);
    END;



PROCEDURE AddPrefix (Prefix: Str20; Band: BandType; Mode: ModeType);

VAR TempBytes: FourBytes;

    BEGIN
    IF PrefixLists [Band, Mode].NumberPrefixes = 0 THEN
        New (PrefixLists [Band, Mode].List);

    WITH PrefixLists [Band, Mode] DO
         BEGIN
         CompressFormat (Prefix, TempBytes);
         List^ [NumberPrefixes] := TempBytes;
         Inc (NumberPrefixes);
         END;
    END;



FUNCTION WorkedPrefix (Prefix: Str20; Band: BandType; Mode: ModeType): BOOLEAN;

VAR Address: INTEGER;
    TempBytes: FourBytes;

    BEGIN
    IF PrefixLists [Band, Mode].NumberPrefixes = 0 THEN
        BEGIN
        WorkedPrefix := False;
        Exit;
        END;

    CompressFormat (Prefix, TempBytes);

    WITH PrefixLists [Band, Mode] DO
        FOR Address := 0 TO NumberPrefixes - 1 DO
            IF (List^ [Address] [1] = TempBytes [1]) AND
               (List^ [Address] [2] = TempBytes [2]) AND
               (List^ [Address] [3] = TempBytes [3]) AND
               (List^ [Address] [4] = TempBytes [4]) THEN
                   BEGIN
                   WorkedPrefix := True;
                   Exit;
                   END;

    WorkedPrefix := False;
    END;



FUNCTION DomesticMultsOkay (FileString: STRING;
                            VAR EData, MData: ContestExchange;
                            VAR MultString: Str20): BOOLEAN;

{ This is the routine that works out if there is a multiplier problem for
  domestic multipliers. }


VAR CharPosition: INTEGER;

    BEGIN
    DomesticMultsOkay := True;

    IF EData.DomMultQTH <> MData.DomMultQTH THEN { Different }
        BEGIN

        { First case is if this should be marked as a domestic mult,
          but it isn't.  }

        IF EData.DomesticMult AND NOT MData.DomesticMult THEN
            BEGIN
            DomesticMultsOkay := False;
            TextColor (Yellow);
            WriteLn (FileString);

            REPEAT
                Key := UpCase (GetKey (EData.DomMultQTH + ' multiplier is missing!  Okay to add it? (Y/N) : '));
            UNTIL (Key = 'Y') OR (Key = 'N');
            GoToXY (1, WhereY);
            ClrEol;

            IF Key = 'Y' THEN
                IF MultString = '' THEN
                    MultString := EData.DomMultQTH
                ELSE
                    BEGIN
                    CharPosition := Pos (MData.QTHString, MultString);

                    IF CharPosition > 0 THEN
                        Delete (MultString, CharPosition, Length (MData.QTHString) + 1);

                    Insert (EData.DomMultQTH + ' ', MultString, CharPosition);
                    END;
            END;

            { Here, they are both domestic mults, but are different }

        IF EData.DomesticMult AND MData.DomesticMult THEN
            BEGIN
            DomesticMultsOkay := False;
            TextColor (Yellow);
            WriteLn (FileString);

            REPEAT
                Key := UpCase (GetKey ('Domestic multiplier should be ' +
                                       EData.DomMultQTH + '!  Okay to change it? (Y/N) : '));
            UNTIL (Key = 'Y') OR (Key = 'N');
            GoToXY (1, WhereY);
            ClrEol;

            IF Key = 'Y' THEN
                BEGIN
                CharPosition := Pos (Mdata.DomMultQTH, MultString);

                IF CharPosition > 0 THEN
                    BEGIN
                    Delete (MultString, CharPosition, Length (MData.DomMultQTH));
                    Insert (EData.DomMultQTH, MultString, CharPosition);
                    END
                ELSE
                    BEGIN
                    CharPosition := Pos (Mdata.QTHString, MultString);

                    IF CharPosition > 0 THEN
                        BEGIN
                        Delete (MultString, CharPosition, Length (MData.QTHString));
                        Insert (EData.DomMultQTH, MultString, CharPosition);
                        END;
                    END;
                END;
            END;
        END;

    { Now we may or may not have the same domestic qth as the mult. }

    IF (NOT EData.DomesticMult) AND Mdata.DomesticMult THEN
        BEGIN
        DomesticMultsOkay := False;
        TextColor (Yellow);
        WriteLn (FileString);

        IF EData.DomMultQTH = MData.DomMultQTH THEN
            BEGIN
            REPEAT
                Key := UpCase (GetKey ('You have already worked ' + EData.DomMultQTH + '!  Okay to remove it? (Y/N) : '));
            UNTIL (Key = 'Y') OR (Key = 'N');
            GoToXY (1, WhereY);
            ClrEol;
            END
        ELSE
            BEGIN
            REPEAT
                Key := UpCase (GetKey ('This station is not in ' + MData.DomMultQTH + '!  Okay to remove mult? (Y/N) : '));
            UNTIL (Key = 'Y') OR (Key = 'N');
            GoToXY (1, WhereY);
            ClrEol;
            END;

        IF Key = 'Y' THEN
            BEGIN
            CharPosition := Pos (MData.DomMultQTH, MultString);

            IF CharPosition > 0 THEN
                Delete (MultString, CharPosition, Length (MData.DomMultQTH) + 1)
            ELSE
                BEGIN
                CharPosition := Pos (MData.QTHString, MultString);

                IF CharPosition > 0 THEN
                    Delete (MultString, CharPosition, Length (MData.QTHString) + 1)
                END;
            END;
        END;
   END;



FUNCTION DXMultsOkay (FileString: STRING;
                      VAR EData, MData: ContestExchange;
                      VAR MultString: Str20): BOOLEAN;

{ This is the routine that works out if there is a multiplier problem for
  DX multipliers. }


VAR CharPosition: INTEGER;

    BEGIN
    DXMultsOkay := True;

    IF EData.DXQTH <> MData.DXQTH THEN { Different }
        BEGIN

    { First case is if this should be marked as a DX mult, but it isn't. }

        IF EData.DXMult AND NOT MData.DXMult THEN
            BEGIN
            DXMultsOkay := False;
            TextColor (Yellow);
            WriteLn (FileString);

            REPEAT
                Key := UpCase (GetKey (EData.DXQTH + ' multiplier is missing!  Okay to add it? (Y/N) : '));
            UNTIL (Key = 'Y') OR (Key = 'N');
            GoToXY (1, WhereY);
            ClrEol;

            IF Key = 'Y' THEN
                IF MultString = '' THEN
                    MultString := EData.DXQTH
                ELSE
                    BEGIN
                    CharPosition := Pos (MData.QTHString, MultString);

                    IF CharPosition > 0 THEN
                        Delete (MultString, CharPosition, Length (MData.QTHString) + 1);

                    Insert (EData.DXQTH + ' ', MultString, CharPosition);
                    END;
            END;

        { Here, they are both DX mults, but are different }

        IF EData.DXMult AND MData.DXMult THEN
            BEGIN
            DXMultsOkay := False;
            TextColor (Yellow);
            WriteLn (FileString);

            REPEAT
                Key := UpCase (GetKey ('DX multiplier should be ' + EData.DXQTH + '!  Okay to change it? (Y/N) : '));
            UNTIL (Key = 'Y') OR (Key = 'N');
            GoToXY (1, WhereY);
            ClrEol;

            IF Key = 'Y' THEN
                BEGIN
                CharPosition := Pos (Mdata.DXQTH, MultString);

                IF CharPosition > 0 THEN
                    BEGIN
                    Delete (MultString, CharPosition, Length (MData.DXQTH));
                    IF NOT StringHas (MultString, EData.DXQTH) THEN
                        Insert (EData.DXQTH, MultString, CharPosition);
                    END
                ELSE
                    BEGIN
                    CharPosition := Pos (Mdata.QTHString, MultString);

                    IF CharPosition > 0 THEN
                        BEGIN
                        Delete (MultString, CharPosition, Length (MData.QTHString));
                        IF NOT StringHas (MultString, EData.DXQTH) THEN
                            Insert (EData.DXQTH, MultString, CharPosition);
                        END;
                    END;
                END;
            END;
        END;

    { Now we may or may not have the same DX qth as the mult. }

    IF (NOT EData.DXMult) AND Mdata.DXMult THEN
        BEGIN
        DXMultsOkay := False;
        TextColor (Yellow);
        WriteLn (FileString);

        IF EData.DXQTH = MData.DXQTH THEN
            BEGIN
            REPEAT
                Key := UpCase (GetKey ('You have already worked ' + EData.DXQTH + '!  Okay to remove mult? (Y/N) : '));
            UNTIL (Key = 'Y') OR (Key = 'N');
            GoToXY (1, WhereY);
            ClrEol;
            END
        ELSE
            BEGIN
            REPEAT
                Key := UpCase (GetKey ('This station is not in ' + MData.DXQTH + '!  Okay to remove mult? (Y/N) : '));
            UNTIL (Key = 'Y') OR (Key = 'N');
            GoToXY (1, WhereY);
            ClrEol;
            END;

        IF Key = 'Y' THEN
            BEGIN
            CharPosition := Pos (MData.DXQTH, MultString);

            IF CharPosition > 0 THEN
                Delete (MultString, CharPosition, Length (MData.DXQTH) + 1);
            END;
        END;
   END;



FUNCTION PrefixMultsOkay (FileString: STRING;
                          VAR EData, MData: ContestExchange;
                          VAR MultString: Str20): BOOLEAN;

{ This is the routine that works out if there is a multiplier problem for
  Prefix multipliers. }


VAR CharPosition: INTEGER;

    BEGIN
    PrefixMultsOkay := True;

    IF EData.Prefix <> MData.Prefix THEN { Different }
        BEGIN

    { First case is if this should be marked as a Prefix mult, but it isn't. }

        IF EData.PrefixMult AND NOT MData.PrefixMult THEN
            BEGIN
            PrefixMultsOkay := False;
            TextColor (Yellow);
            WriteLn (FileString);

            REPEAT
                Key := UpCase (GetKey ('Prefix multiplier is missing!  Okay to add it? (Y/N) : '));
            UNTIL (Key = 'Y') OR (Key = 'N');
            GoToXY (1, WhereY);
            ClrEol;

            IF Key = 'Y' THEN
                IF MultString = '' THEN
                    MultString := EData.Prefix
                ELSE
                    BEGIN
                    CharPosition := Pos (MData.QTHString, MultString);

                    IF CharPosition > 0 THEN
                        Delete (MultString, CharPosition, Length (MData.QTHString) + 1);

                    Insert (EData.Prefix + ' ', MultString, CharPosition);
                    END;
            END;

            { Here, they are both Prefix mults, but are different }

        IF EData.PrefixMult AND MData.PrefixMult THEN
            BEGIN
            PrefixMultsOkay := False;
            TextColor (Yellow);
            WriteLn (FileString);

            REPEAT
                Key := UpCase (GetKey ('Prefix multiplier should be ' + Edata.Prefix + '!  Okay to change it? (Y/N) : '));
            UNTIL (Key = 'Y') OR (Key = 'N');
            GoToXY (1, WhereY);
            ClrEol;

            IF Key = 'Y' THEN
                BEGIN
                CharPosition := Pos (Mdata.Prefix, MultString);

                IF CharPosition > 0 THEN
                    BEGIN
                    Delete (MultString, CharPosition, Length (MData.Prefix));
                    Insert (EData.Prefix, MultString, CharPosition);
                    END
                ELSE
                    BEGIN
                    CharPosition := Pos (Mdata.QTHString, MultString);

                    IF CharPosition > 0 THEN
                        BEGIN
                        Delete (MultString, CharPosition, Length (MData.QTHString));
                        Insert (EData.Prefix, MultString, CharPosition);
                        END;
                    END;
                END;
            END;
        END;

    { Now we may or may not have the same Prefix qth as the mult. }

    IF (NOT EData.PrefixMult) AND Mdata.PrefixMult THEN
        BEGIN
        PrefixMultsOkay := False;
        TextColor (Yellow);
        WriteLn (FileString);

        IF EData.Prefix = MData.Prefix THEN
            BEGIN
            REPEAT
                Key := UpCase (GetKey ('You have already worked the ' + EData.Prefix +
                               ' prefix!  Okay to remove it? (Y/N) : '));
            UNTIL (Key = 'Y') OR (Key = 'N');
            GoToXY (1, WhereY);
            ClrEol;
            END
        ELSE
            BEGIN
            REPEAT
                Key := UpCase (GetKey (MData.Prefix + ' is not the correct prefix!  Okay to remove mult? (Y/N) : '));
            UNTIL (Key = 'Y') OR (Key = 'N');
            GoToXY (1, WhereY);
            ClrEol;
            END;

        IF Key = 'Y' THEN
            BEGIN
            CharPosition := Pos (MData.Prefix, MultString);

            IF CharPosition > 0 THEN
                Delete (MultString, CharPosition, Length (MData.Prefix) + 1);
            END;
        END;
   END;



FUNCTION ZoneMultsOkay (VAR FileString: STRING;
                        VAR EData, MData: ContestExchange;
                        VAR MultString: Str20;
                        CheckZoneAgainstCTYDotDat: BOOLEAN): BOOLEAN;

{ This is the routine that works out if there is a multiplier problem for
  Zone multipliers.  It will also look at the zone suggested by the callsign
  and make sure that it looks correct. }


VAR CharPosition: INTEGER;
    ExchangeZone, Result, CallsignZone: INTEGER;
    ExchangeString, CallsignZoneString: Str40;

    BEGIN
    ZoneMultsOkay := True;
    TextColor (Yellow);

    IF CheckZoneAgainstCTYDotDat THEN
        BEGIN
        CallsignZone := CountryTable.GetZone (EData.Callsign);

        IF CallsignZone > 0 THEN
            BEGIN
            Val (EData.Zone, ExchangeZone, Result);

            IF (Result <> 0) OR
               ((CountryTable.ZoneMode = ITUZoneMode) AND (ExchangeZone <> CallsignZone)) OR
               ((CountryTable.ZoneMode = CQZoneMode)  AND (ExchangeZone <> CallsignZone) AND
               (ExchangeZone <> 3) AND (ExchangeZone <> 4) AND (ExchangeZone <> 5)) THEN
                   BEGIN
                   Str (CallsignZone, CallsignZoneString);

                   WHILE Length (CallsignZoneString) < 2 DO
                       CallsignZoneString := '0' + CallsignZoneString;

                   TextColor (Yellow);
                   WriteLn (FileString);

                   REPEAT
                       Key := UpCase (GetKey ('Zone incorrect?  Change to zone ' + CallsignZoneString + '? (Y/N) : '));
                   UNTIL (Key = 'Y') OR (Key = 'N');
                   GoToXY (1, WhereY);
                   ClrEol;

                   IF Key = 'Y' THEN
                       BEGIN
                       ZoneMultsOkay := False;

                       EData.Zone := CallsignZoneString;
                       Sheet.SetMultFlags (EData);

                       ExchangeString := Copy (FileString, LogEntryExchangeAddress, LogEntryExchangeWidth);

                       RemoveLastString (ExchangeString);
                       ExchangeString := ExchangeString + ' ' + CallsignZoneString;

                       WHILE Length (ExchangeString) < LogEntryExchangeWidth DO
                           ExchangeString := ExchangeString + ' ';

                       Delete (FileString, LogEntryExchangeAddress, LogEntryExchangeWidth);

                       Insert (ExchangeString, FileString, LogEntryExchangeAddress);
                       END;
                   END;
            END;
        END;

{    MakeSureMults are not DUPE or ZERO.}

    IF EData.Zone <> MData.Zone THEN { Different }
        BEGIN

    { First case is if this should be marked as a Zone mult, but it isn't. }

        IF EData.ZoneMult AND NOT MData.ZoneMult THEN
            BEGIN
            ZoneMultsOkay := False;
            TextColor (Yellow);
            WriteLn (FileString);

            REPEAT
                Key := UpCase (GetKey ('Zone ' + Edata.Zone + ' multiplier is missing!  Okay to add it? (Y/N) : '));
            UNTIL (Key = 'Y') OR (Key = 'N');
            GoToXY (1, WhereY);
            ClrEol;

            IF Key = 'Y' THEN
                IF MultString = '' THEN
                    MultString := EData.Zone
                ELSE
                    MultString := MultString + ' ' + EData.Zone;
            END;

            { Here, they are both Zone mults, but are different }

        IF EData.ZoneMult AND MData.ZoneMult THEN
            BEGIN
            ZoneMultsOkay := False;
            TextColor (Yellow);
            WriteLn (FileString);

            REPEAT
                Key := UpCase (GetKey ('Zone multiplier should be ' + EData.Zone + '!  Okay to change it? (Y/N) : '));
            UNTIL (Key = 'Y') OR (Key = 'N');
            GoToXY (1, WhereY);
            ClrEol;

            IF Key = 'Y' THEN
                BEGIN
                CharPosition := Pos (Mdata.Zone, MultString);

                IF CharPosition > 0 THEN
                    Delete (MultString, CharPosition, Length (MData.Zone));

                MultString := MultString + EData.Zone;
                END;
            END;
        END;

    { Now we may or may not have the same Zone qth as the mult. }

    IF (NOT EData.ZoneMult) AND Mdata.ZoneMult THEN
        BEGIN
        ZoneMultsOkay := False;
        TextColor (Yellow);
        WriteLn (FileString);

        IF EData.Zone = MData.Zone THEN
            BEGIN
            REPEAT
                Key := UpCase (GetKey ('You have already worked zone ' + EData.Zone + '!  Okay to remove mult? (Y/N) : '));
            UNTIL (Key = 'Y') OR (Key = 'N');
            GoToXY (1, WhereY);
            ClrEol;
            END
        ELSE
            BEGIN
            REPEAT
                Key := UpCase (GetKey ('Incorrect Zone multiplier!!  Okay to remove mult? (Y/N) : '));
            UNTIL (Key = 'Y') OR (Key = 'N');
            GoToXY (1, WhereY);
            ClrEol;
            END;

        IF Key = 'Y' THEN
            BEGIN
            CharPosition := Pos (MData.Zone, MultString);

            IF CharPosition > 0 THEN
                Delete (MultString, CharPosition, Length (MData.Zone) + 1);
            END;
        END;
   END;



PROCEDURE MultCheck;

{ This routine will do a complete log check as follows:
    - Checks for LOG.TMP and appends it to LOG.DAT file.
    - Looks at the LOGCFG.DAT file to determine exchange and mult types.
    - Goes through the log and verifies all multipliers.
}

VAR EData, MData: ContestExchange;
    MultString: Str20;
    CharPosition, QSONumber: INTEGER;
    FileString: STRING;
    CheckZones, ProblemFound: BOOLEAN;
    Key: CHAR;
    FileRead, FileWrite: TEXT;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('MULTIPLIER CHECK PROCEDURE');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This procedure will look through your log and check all of your multipliers.');
    WriteLn ('If it finds a duplicate or missing multiplier, it will show you the contact.');
    WriteLn;
    WriteLn ('It is necessary to be logged into the same directory you used during the');
    WriteLn ('contest so the LOGCFG.DAT file can be used to determine the proper format for');
    WriteLn ('the multiplier information.');
    WriteLn;

    IF NOT OkayToProceed THEN Exit;

    { This needs do be done before the SetUpPostParameters command.  Fixed
      crashing occurring in 6.24 that didn't happen in 6.20. }

    DupeInit;
    Sheet.DisposeOfMemoryAndZeroTotals;

    WriteLn;

    IF NOT SetUpPostParametersFromLOGCFGFile THEN
        BEGIN
        WaitForKeyPressed;
        Exit;
        END;

    IF DoingZoneMults THEN
        BEGIN
        TextColor (Cyan);
        WriteLn ('This contest uses zone multipliers.  If you want, this procedure can check   ');
        WriteLn ('the zones entered into the log against the CTY.DAT file and alert you to any');
        WriteLn ('differences and give you the option of changing it.  ');
        WriteLn;

        REPEAT
            Key := UpCase (GetKey ('Would you like to perform this zone check? (Y/N): '));
            IF Key = EscapeKey THEN Exit;
        UNTIL (Key = 'Y') OR (Key = 'N');
        WriteLn;

        CheckZones := Key = 'Y';
        END;

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError ('Unable to open ' + LogFileName + '!!');
        WaitForKeyPressed;
        Exit;
        END;

    IF NOT OpenFileForWrite (FileWrite, TempFileName) THEN
        BEGIN
        Close (FileRead);
        ReportError ('Unable to open ' + TempFileName + ' for output!!');
        WaitForKeyPressed;
        Exit;
        END;

    QSONumber := 0;
    Write (QSONumber);
    ProblemFound := False;

    WHILE NOT Eof (FileRead) DO
        BEGIN
        ReadLn (FileRead, FileString);

        TempString := UpperCase (FileString);

        IF StringHas (FileString, '*DUPE*') OR StringHas (FileString, '*ZERO*') THEN
            BEGIN
            WriteLn (FileWrite, FileString);
            TextColor (Cyan);
            Inc (QSONumber);
            GoToXY (1, WhereY);
            Write (QSONumber);
            GoToXY (1, WhereY);
            TextColor (Yellow);
            Continue;
            END;

        IF GetContestExchangeFromLogEntryExchange (FileString, EData) THEN
            BEGIN
            TextColor (Cyan);
            Inc (QSONumber);
            GoToXY (1, WhereY);
            Write (QSONumber);
            GoToXY (1, WhereY);
            TextColor (Yellow);

            GetContestExchangeFromMultiplierString (FileString, MData);

            MultString := GetLogEntryMultString (FileString);

            { First we look at the Domestic multipliers }

            IF DoingDomesticMults AND
               (DomesticCountryCall (EData.Callsign) OR StringHasLowerCase (EData.QTHString)) THEN
                IF NOT DomesticMultsOKay (FileString, EData, MData, MultString) THEN
                    ProblemFound := True;

            IF ProblemFound THEN
                BEGIN
                Delete (FileString, LogEntryMultAddress, LogEntryMultWidth);
                WHILE Length (MultString) < 8 DO MultString := MultString + ' ';
                Insert (MultString, FileString, LogEntryMultAddress);

                GoToXY (1, WhereY - 1);
                ClrEol;
                TextColor (Cyan);
                WriteLn (FileString);
                ProblemFound := False;
                MultString := GetLogEntryMultString (FileString);
                END;

            IF DoingDXMults AND NOT DomesticCountryCall (EData.Callsign) THEN
                IF NOT DXMultsOKay (FileString, EData, MData, MultString) THEN
                    ProblemFound := True;

            IF ProblemFound THEN
                BEGIN
                Delete (FileString, LogEntryMultAddress, LogEntryMultWidth);
                WHILE Length (MultString) < 8 DO MultString := MultString + ' ';
                Insert (MultString, FileString, LogEntryMultAddress);

                GoToXY (1, WhereY - 1);
                ClrEol;
                TextColor (Cyan);
                WriteLn (FileString);
                ProblemFound := False;
                MultString := GetLogEntryMultString (FileString);
                END;

            IF DoingPrefixMults THEN
                IF NOT PrefixMultsOKay (FileString, EData, MData, MultString) THEN
                    ProblemFound := True;

            IF ProblemFound THEN
                BEGIN
                Delete (FileString, LogEntryMultAddress, LogEntryMultWidth);
                WHILE Length (MultString) < 8 DO MultString := MultString + ' ';
                Insert (MultString, FileString, LogEntryMultAddress);

                GoToXY (1, WhereY - 1);
                ClrEol;
                TextColor (Cyan);
                WriteLn (FileString);
                ProblemFound := False;
                MultString := GetLogEntryMultString (FileString);
                END;

            IF DoingZoneMults THEN
                IF NOT ZoneMultsOKay (FileString, EData, MData, MultString, CheckZones) THEN
                    ProblemFound := True;

            IF ProblemFound THEN
                BEGIN
                TextColor (Yellow);
                Delete (FileString, LogEntryMultAddress, LogEntryMultWidth);
                WHILE Length (MultString) < 8 DO MultString := MultString + ' ';
                Insert (MultString, FileString, LogEntryMultAddress);

                GoToXY (1, WhereY - 1);
                ClrEol;
                TextColor (Cyan);
                WriteLn (FileString);
                ProblemFound := False;
                END;

            Sheet.AddQSOToSheets (EData);
            END;

        WriteLn (FileWrite, FileString);
        END;

    Close (FileRead);
    Close (FileWrite);
    Sheet.DisposeOfMemoryAndZeroTotals;

    PushLogFiles;
    RenameFile (TempFileName, LogFileName);
    TextColor (Yellow);
    WriteLn;
    WriteLn ('Multiplier check completed.  Your original log can be found as ', LastPushedLogName);
    WaitForKeyPressed;
    END;



PROCEDURE PrefixMultCheck;

VAR FileString, NameString, FileName: Str80;
    Prefix, MultString: Str20;
    Band: BandType;
    Mode: ModeType;
    CharPointer, StringLength, QSONumber: INTEGER;
    Command, Key: CHAR;
    MultByBand, MultByMode: BOOLEAN;
    MultiplierString, Call: CallString;
    FileRead, FileWrite, FileAppend: TEXT;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('PREFIX MULTIPLIER CHECK PROCEDURE');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This procedure will examine your log to verify the prefix multipliers.  It');
    WriteLn ('looks at each callsign, determines its prefix, and then checks to see if');
    WriteLn ('it is a new multiplier.  It will check to make sure the proper entry is in');
    WriteLn ('the multiplier column.  If a multiplier is missing that should be marked, or');
    WriteLn ('if a multiplier is flagged that should not be, the contact will be displayed');
    WriteLn ('and you will be asked if a correction should be made.  Your original log will');
    WriteLn ('be saved as PLOG###.BAK and the new log file will be saved to the active log');
    WriteLn ('name.  If this is not what you want to do, press ESCAPE now.');
    WriteLn;

    FOR Band := Band160 TO NoBand DO
         FOR Mode := CW TO Both DO
             PrefixLists [Band, Mode].NumberPrefixes := 0;

    REPEAT
        Key := UpCase (GetKey ('Do prefix multipliers count again on each mode? (Y/N or ESCAPE to abort) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'Y') OR (Key = 'N');
    WriteLn;

    MultByMode := Key = 'Y';

    REPEAT
        Key := UpCase (GetKey ('Do prefix multipliers count again on each band? (Y/N or ESCAPE to abort) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'Y') OR (Key = 'N');
    WriteLn;

    MultByBand := Key = 'Y';

    PushLogFiles;
    TextColor (Cyan);
    WriteLn ('Original copy of ', LogFileName, ' saved as ', LastPushedLogName);

    IF NOT OpenFileForRead (FileRead, LastPushedLogName) THEN
        BEGIN
        ReportError (LastPushedLogName + ' not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    IF NOT OpenFileForWrite (FileWrite, LogFileName) THEN
        BEGIN
        Close (FileRead);
        ReportError ('Unable to open ' + LogFileName + ' for output!!');
        WaitForKeyPressed;
        Exit;
        END;

    QSONumber := 0;
    Write (QSONumber);

    REPEAT
        REPEAT
            ReadLn (FileRead, FileString);
            Band := GetLogEntryBand (FileString);
            IF ((Band = NoBand) AND NOT EOF (FileRead)) THEN
                WriteLn (FileWrite, FileString);
        UNTIL (Band <> NoBand) OR EOF (FileRead);

        IF (Band <> NoBand) AND NOT StringHas (FileString, 'DUPE') THEN
            BEGIN
            Inc (QSONumber);
            GoToXY (1, WhereY);
            Write (QSONumber);

            ExpandTabs (FileString);
            Mode := GetLogEntryMode (FileString);
            Call := StandardCallFormat (GetLogEntryCall (FileString), False);

            IF NOT MultByBand THEN Band := All;
            IF NOT MultByMode THEN Mode := Both;

            Prefix := UpperCase (CheckGetPrefix (Call));

            MultString := UpperCase (GetLogEntryMultString (FileString));

            IF (MultString = '') THEN
                BEGIN
                IF NOT WorkedPrefix (Prefix, Band, Mode) THEN
                    BEGIN
                    WHILE Length (Prefix) < Length (MultString) DO
                        Prefix := Prefix + ' ';

                    Delete (FileString, LogEntryMultAddress, Length (Prefix));
                    Insert (Prefix, FileString, LogEntryMultAddress);
                    AddPrefix (Prefix, Band, Mode);
                    END;

                WriteLn (FileWrite, FileString);
                Continue;
                END;

            IF MultString <> Prefix THEN
                BEGIN
                GoToXY (1, WhereY);
                WriteLn ('I am about to change the prefix for ', Call, ' from ', MultString, ' to ', Prefix, '.');

                REPEAT
                    Key := UpCase (GetKey ('Is this okay? (Y/N) : '));
                UNTIL (Key = 'Y') OR (Key = 'N');
                WriteLn;

                IF Key = 'Y' THEN
                    IF NOT WorkedPrefix (Prefix, Band, Mode) THEN
                        BEGIN
                        WHILE Length (Prefix) < Length (MultString) DO
                            Prefix := Prefix + ' ';

                        Delete (FileString, LogEntryMultAddress, Length (Prefix));
                        Insert (Prefix, FileString, LogEntryMultAddress);
                        AddPrefix (Prefix, Band, Mode);
                        END;

                WriteLn (FileWrite, FileString);
                Continue;
                END;

            IF MultString = Prefix THEN
                BEGIN
                IF WorkedPrefix (Prefix, Band, Mode) THEN
                    BEGIN
                    Delete (FileString, LogEntryMultAddress, Length (Prefix));

                    FOR CharPointer := 1 TO Length (Prefix) DO
                        Prefix [CharPointer] := ' ';

                    Insert (Prefix, FileString, LogEntryMultAddress);
                    END
                ELSE
                    AddPrefix (Prefix, Band, Mode);

                WriteLn (FileWrite, FileString);
                Continue;
                END;
            END;

    UNTIL Eof (FileRead);

    Close (FileRead);
    Close (FileWrite);
    WriteLn;
    WriteLn ('Completed');
    END;



PROCEDURE PullSpecificComputerLog;

VAR Key, ComputerID: CHAR;
    KeepID: BOOLEAN;
    FileRead, FileWrite: TEXT;
    OutputFileName: Str40;
    FileString: Str160;
    NumberQSOs, NumberNotes, Line: INTEGER;
    Band: BandType;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('PULL OUT LOG FOR ONE SPECIFIC COMPUTER');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This procedure will create a new log file with only the QSOs and notes from');
    WriteLn ('a specific computer.  It is assume you used the COMPUTER ID feature of the');
    WriteLn ('TR program.  This puts a single character (A-Z) just after the QSO number in');
    WriteLn ('the log.');
    WriteLn;

    REPEAT
        ComputerID := UpCase (GetKey ('Computer ID to pull out (SPACE for no ID QSOs, ESCAPE to exit) : '));
        IF ComputerID = EscapeKey THEN Exit;
    UNTIL ((ComputerID >= 'A') AND (ComputerID <= 'Z')) OR (ComputerID = ' ');
    WriteLn;

    IF ComputerID <> ' ' THEN
        BEGIN
        REPEAT
            Key := UpCase (GetKey ('Do you want to remove the computer ID in the new log? (Y/N) : '));
            IF Key = EscapeKey THEN Exit;
        UNTIL (Key = 'Y') OR (Key = 'N');
        WriteLn;
        KeepID := Key = 'N';
        END
    ELSE
        KeepID := True;

    WHILE True DO
        BEGIN
        OutputFileName := GetResponse ('Enter filename to save specific log to : ');
        IF OutputFileName = '' THEN Exit;

        IF OutputFileName = LogFileName THEN Exit;

        IF FileExists (OutputFileName) THEN
            BEGIN
            REPEAT
                Key := UpCase (GetKey (OutputFileName + ' already exists!!  Okay to overwrite? (Y/N) : '));
                IF Key = EscapeKey THEN Exit;
            UNTIL (Key = 'Y') OR (Key = 'N');
            WriteLn;

            IF Key = 'Y' THEN Break;
            END
        ELSE
            Break;
        END;

    GetCopyOfHeader;

    NumberQSOs  := 0;
    NumberNotes := 0;

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError (LogFileName + 'not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    IF NOT OpenFileForWrite (FileWrite, OutputFileName) THEN
        BEGIN
        ReportError ('Unable to open ' + OutputFileName);
        WaitForKeyPressed;
        Exit;
        END;

    REPEAT
        REPEAT
            ReadLn (FileRead, FileString);
            Band := GetLogEntryBand (FileString);
        UNTIL (Band <> NoBand) OR EOF (FileRead) OR (Copy (FileString, 1, 1) = ';');

        IF Copy (FileString, 1, 1) = ';' THEN   { This is a note }
            BEGIN
            IF StringHas (FileString, ComputerID + ':') THEN  { A note for us }
                BEGIN
                WriteLn (FileWrite, FileString);
                Inc (NumberNotes);
                END;
            END

        ELSE
            IF Band <> NoBand THEN
                IF GetLogEntryComputerID (FileString) = ComputerID THEN
                    BEGIN
                    IF (NumberQSOs MOD 50) = 0 THEN
                        BEGIN
                        IF NumberQSOs <> 0 THEN Write (FileWrite, ControlL);

                        FOR Line := 0 TO NumberHeaderLines - 1 DO
                            WriteLn (FileWrite, Header [Line]);
                        END
                    ELSE
                        IF (NumberQSOs MOD 10) = 0 THEN WriteLn (FileWrite);

                    IF NOT KeepID THEN FileString [LogEntryComputerIDAddress] := ' ';
                    WriteLn (FileWrite, FileString);
                    Inc (NumberQSOs);
                    END;

    UNTIL Eof (FileRead);

    Close (FileRead);
    Close (FileWrite);

    TextColor (Cyan);
    WriteLn ('There were, ', NumberQSOs, ' QSOs written to ', OutputFileName);
    WriteLn ('There were, ', NumberNotes, ' notes saved.');
    WaitForKeyPressed;
    END;


FUNCTION LogProcedureMenu: BOOLEAN;

VAR Key: CHAR;

    BEGIN
    LogProcedureMenu := True;
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('LOG PROCEDURE MENU');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('  A - Create copy of log in the ARRL suggested format.');
    WriteLn ('  C - Create final logs (By band or mode and with running totals).');
    WriteLn ('  D - Dupe log (marks any dupes that may be left in the log).');
    WriteLn ('  E - Edit log (simple line editor to change specific lines).');
    WriteLn ('  F - Filter log (create new log including or excluding desired countries).');
    WriteLn ('  G - Merge two log files together, checking for dupes/mults.');
    WriteLn ('  J - Create log check file for KCJ contest.');
    WriteLn ('  M - Multiplier check.  Complete check of all multiplier types.');
    WriteLn ('  P - Pull out log for one specific computer ID in a multi-multi.');
    WriteLn ('  V - View log segments.');
    WriteLn ('  X - Exit log procedure menu.');
    WriteLn;

    REPEAT
        WriteLn;
        TextColor (Cyan);
        Write   ('  Enter command : ');

        REPEAT UNTIL KeyPressed;
        Key := UpCase (ReadKey);

        CASE Key OF
            'A': BEGIN ARRLCompatibleLog;       Exit; END;
            'C': BEGIN CreateFinalLog;          Exit; END;
            'D': BEGIN DupeLog;                 Exit; END;
            'E': BEGIN EditLog;                 Exit; END;
            'F': BEGIN FilterLog;               Exit; END;
            'G': BEGIN Merge;                   Exit; END;
            'J': BEGIN MakeKCJLog;              Exit; END;
            'M': BEGIN MultCheck;               Exit; END;
            'P': BEGIN PullSpecificComputerLog; Exit; END;
            'V': BEGIN ViewLog;                 Exit; END;

            'X', EscapeKey:
                     BEGIN
                     LogProcedureMenu := False;
                     Exit;
                     END;
            END;
    UNTIL False;
    END;



    BEGIN
    END.
