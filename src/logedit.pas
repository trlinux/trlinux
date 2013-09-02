UNIT LogEdit;

{ This unit contains the object EditableLog.  }

{$O+}
{$F+}
{$V-}

INTERFACE

USES Dos, Tree, LogWind, LogDupe, LogStuff, ZoneCont, Country9,
     LogCW, LogDVP, LogDom, Printer, LogK1EA, LogHelp, LogGrid, trCrt,
     LogSCP,datetimec,radio;

TYPE
    EditableLog = OBJECT
        LogEntries: LogEntryArray;

        FUNCTION  CallIsADupe (Call: CallString; Band: BandType; Mode: ModeType): BOOLEAN;
        PROCEDURE CancelOutMultsThatAreInEditableWindow (VAR Exchange: ContestExchange);
        FUNCTION  CallIsInEditableLog (Call: CallString; Band: BandType; Mode: ModeType): BOOLEAN;

        PROCEDURE CreateModeSpecificDomesticMultiplierInfo
                      (DomQth: Str20; Mode: ModeType; VAR OutputString: Str160);

        PROCEDURE CreateModeSpecificMultiplierInfo (Call: CallString;
                                                    Mode: ModeType;
                                                    VAR OutputString: Str160);

        PROCEDURE CreateModeSpecificQSOInfo (Call: CallString;
                                             Mode: ModeType;
                                             VAR OutputString: Str160);

        PROCEDURE DeleteLastLogEntry;

        PROCEDURE DetermineIfNewDomesticMult (DomesticMult: Str20;
                                              Band: BandType;
                                              Mode: ModeType;
                                              VAR MultString: Str80);

        PROCEDURE DetermineIfNewMult (Call: CallString;
                                      Band: BandType;
                                      Mode:ModeType;
                                      VAR MultString: Str80);

        PROCEDURE DisplayGridMap (Band: BandType; Mode: ModeType);

        PROCEDURE DisplayVisibleDupeSheet (Band: BandType; Mode: ModeType);

        PROCEDURE DoPossibleCalls (Call: CallString);

        PROCEDURE EditLog;
        FUNCTION  EditableLogIsEmpty: BOOLEAN;

        FUNCTION  GetInitialExchangeFromEditableLog (Call: CallString): Str40;

        FUNCTION  GetMultStatus (Call: CallString): Str160;

        PROCEDURE IncrementQSOPointsWithContentsOfEditableWindow (VAR QPoints: LongInt);
        PROCEDURE IncrementQSOTotalsWithContentsOfEditableWindow (VAR QSOTotals: QSOTotalArray);
        PROCEDURE IncrementMultTotalsWithContentsOfEditableWindow (VAR MTotals: MultTotalArrayType);
        FUNCTION  IsNotFoundInEditableWindow (ActiveGrid: Str10; Band: BandType; Mode: ModeType): BOOLEAN;

        FUNCTION  LastCallsign: CallString;
        FUNCTION  LastName (Entry: INTEGER): Str20;

        PROCEDURE GeneratePartialCallList (InputCall: CallString;
                                           Band: BandType;
                                           Mode: ModeType;
                                           VAR PossCallList: PossibleCallRecord);

        FUNCTION  NumberNamesSentInEditableLog: INTEGER;

        FUNCTION  PushLogEntry (NewEntry: Str80): Str80;
        PROCEDURE ProcessMultipliers (VAR RXData: ContestExchange);
        PROCEDURE PutLogEntryIntoSheet (VAR LogEntry: Str80);

        PROCEDURE SearchLog (InitialString: Str20);
        PROCEDURE SetUpEditableLog;
        PROCEDURE ShowDomesticMultiplierStatus (DomesticQTH: Str20);
        PROCEDURE ShowMissingMultiplierReport;
        PROCEDURE ShowMultiplierStatus (Call: CallString);
        PROCEDURE ShowRemainingMultipliers;
        PROCEDURE ShowQSOStatus (Call: CallString);
        PROCEDURE SuperCheckPartial (Call: CallString; Automatic: BOOLEAN; Radio: RadioType); {KK1L: 6.73 Added Radio for SO2R}

        PROCEDURE UpdateTempLogFile;
        END;

VAR VisibleLog: EditableLog;
    LastSCPCall: CallString;

    QTotals: QSOTotalArray;
    MTotals: MultTotalArrayType;

    OriginalTextMode: INTEGER;
    RememberHeap:     POINTER;

PROCEDURE AddQTCToQTCBuffer (VAR QTCBuffer: LogEntryArray; QTCString: Str80; Message: INTEGER);

PROCEDURE BandDown;
PROCEDURE BandUp;
PROCEDURE CleanUpDisplay;

FUNCTION  DetermineQTCNumberAndQuanity (InputString: Str80;
                                        VAR QTCNumber: INTEGER;
                                        VAR Quantity: INTEGER): BOOLEAN;

PROCEDURE EditInit;
PROCEDURE TimeAndDateSet;

PROCEDURE DisplayBandTotals (Band: BandType);

FUNCTION  InitialExchangeEntry (Call: CallString): Str80;

PROCEDURE MoveGridMap (Key: Char);

FUNCTION  QuickEditResponseWithPartials (Prompt: Str80;
                                        MaxInputLength: INTEGER): Str80;
PROCEDURE Send88Message;
PROCEDURE ShowStationInformation (Call: CallString);
FUNCTION  TotalContacts: INTEGER;
FUNCTION  TotalCWContacts: INTEGER;
FUNCTION  TotalPhoneContacts: INTEGER;

{KK1L: 6.64 Added to keep BM up to date with changes made while logging}
PROCEDURE UpdateBandMapDupeStatus(RXCall: CallString; RXBand: BandType; RXMode: ModeType; MakeDupe: BOOLEAN);
PROCEDURE UpdateBandMapMultiplierStatus;
PROCEDURE UpdateTotals;

IMPLEMENTATION

Uses memlinux,LogMenu,keycode,beep,timer;
{KK1L: 6.71 attempt to get POST to compile. Moved here from INTERFACE section. Was not there in original}

VAR SCPScreenFull: BOOLEAN;



FUNCTION TotalCWContacts: INTEGER;

VAR Band: BandType;
    Total: INTEGER;
    TempQSOTotals: QSOTotalArray;

    BEGIN
    TempQSOTotals := QSOTotals;

    VisibleLog.IncrementQSOTotalsWithContentsOfEditableWindow (TempQSOTotals);

    Total := 0;

    FOR Band := Band160 TO Band10 DO
        Total := Total + TempQSOTotals [Band, CW];

    TotalCWContacts := Total;
    END;




FUNCTION TotalPhoneContacts: INTEGER;

VAR Band: BandType;
    Total: INTEGER;
    TempQSOTotals: QSOTotalArray;

    BEGIN
    TempQSOTotals := QSOTotals;

    VisibleLog.IncrementQSOTotalsWithContentsOfEditableWindow (TempQSOTotals);

    Total := 0;

    FOR Band := Band160 TO Band10 DO
        Total := Total + TempQSOTotals [Band, Phone];

    TotalPhoneContacts := Total;
    END;



PROCEDURE Send88Message;

    BEGIN
    IF SeventyThreeMessageSent THEN Exit;

    SendStringAndStop ('88 ' + MyCall + ' TEST');
    END;



PROCEDURE CleanUpDisplay;

    BEGIN
    IF QTCsEnabled THEN RemoveWindow (QTCNumberWindow);
    RemoveWindow (NameSentWindow);
    RemoveWindow (PossibleCallWindow);

    DisplayNextQSONumber (TotalContacts + 1);
    DisplayBandMode      (ActiveBand, ActiveMode, False);
    DisplayFreeMemory;

    IF VisibleDupeSheetEnable THEN
        VisibleLog.DisplayVisibleDupeSheet (ActiveBand, ActiveMode);

    RemoveWindow (CountryNameWindow);
    DisplayNamePercentage (TotalNamesSent + VisibleLog.NumberNamesSentInEditableLog, TotalContacts);
    DisplayInsertMode (InsertMode);

    IF GridSquareListShown THEN VisibleLog.SetUpEditableLog;
    IF BigRemainingList THEN VisibleLog.ShowRemainingMultipliers;
    DisplayUserInfo ('');
    END;



FUNCTION TotalContacts: INTEGER;

VAR TempQSOTotals: QSOTotalArray;

    BEGIN
    TempQSOTotals := QSOTotals;
    VisibleLog.IncrementQSOTotalsWithContentsOfEditableWindow (TempQSOTotals);

    IF QSONumberByBand THEN
        TotalContacts := TempQSOTotals [ActiveBand, Both]
    ELSE
        TotalContacts := TempQSOTotals [All, Both];
    END;



PROCEDURE DisplayBandTotals (Band: BandType);

VAR InitialCursorX: INTEGER;
    MultDisplayEnable: BOOLEAN;

    BEGIN
    MultDisplayEnable := True;

    InitialCursorX := WhereX;

    IF ActiveBand = Band THEN
        BEGIN
        Write ('  ');
        IF (Band = Band160) OR (Band = All) THEN
            WriteColor (' ', ActiveBackground, ActiveColor);
        WriteColor (BandString [Band] + ' ', ActiveBackground, ActiveColor);
        GoToXY (InitialCursorX, WhereY + 1);
        TextColor (ActiveColor);
        TextBackground (ActiveBackground);
        Write ('  ');
{KS vga font displays this correctly, but hyphens below
    allow more fonts and is only a little uglier
        WriteColor ('컴컴', ActiveBackground, ActiveColor);
}
        WriteColor ('----', ActiveBackground, ActiveColor);
        TextColor (ActiveColor);
        TextBackground (ActiveBackground);
        END
    ELSE
        BEGIN
        IF (Band <> Band160) AND (Band <> All) THEN Write ('  ')
        ELSE Write ('   ');
        Write (BandString [Band]);
        GoToXY (InitialCursorX, WhereY + 1);
{KS
        Write ('  컴컴');
}
        Write ('  ----');
        END;

    GoToXY (InitialCursorX, WhereY + 1);

    IF QSOByMode THEN
        BEGIN
        IF (ActiveMode = CW) OR ((QTotals [All, CW] > 0) AND (NumberDifferentMults < 3)) THEN
            BEGIN
            IF QTotals [Band, CW] < 100 THEN
                Write (QTotals [Band, CW]:5)
            ELSE
                Write (QTotals [Band, CW]:6);
            GoToXY (InitialCursorX, WhereY + 1);
            END;

        IF (ActiveMode = Phone) OR ((QTotals [All, Phone] > 0) AND (NumberDifferentMults < 3)) THEN
            BEGIN
            IF QTotals [Band, Phone] < 100 THEN
                Write (QTotals [Band, Phone]:5)
            ELSE
                Write (QTotals [Band, Phone]:6);
            GoToXY (InitialCursorX, WhereY + 1);
            END;

        IF (ActiveMode = Digital) OR ((QTotals [All, Digital] > 0) AND (NumberDifferentMults < 3)) THEN
            BEGIN
            IF QTotals [Band, Digital] < 100 THEN
                Write (QTotals [Band, Digital]:5)
            ELSE
                Write (QTotals [Band, Digital]:6);
            GoToXY (InitialCursorX, WhereY + 1);
            END;
        END
    ELSE
        BEGIN
        IF QTotals [Band, Both] < 100 THEN
            Write (QTotals [Band, Both]:5)
        ELSE
            Write (QTotals [Band, Both]:6);
        GoToXY (InitialCursorX, WhereY + 1);
        END;

    IF (DoingDomesticMults) AND (MultByBand OR (Band = All)) AND MultDisplayEnable THEN
        BEGIN
        IF MultByMode THEN
            BEGIN
            IF MTotals [Band, ActiveMode].NumberDomesticMults < 100 THEN
                Write (MTotals [Band, ActiveMode].NumberDomesticMults:5)
            ELSE
                Write (MTotals [Band, ActiveMode].NumberDomesticMults:6)
            END
        ELSE
            IF MTotals [Band, Both].NumberDomesticMults < 100 THEN
                Write (MTotals [Band, Both].NumberDomesticMults:5)
            ELSE
                Write (MTotals [Band, Both].NumberDomesticMults:6);

        IF WhereY <= 5 THEN
            GoToXY (InitialCursorX, WhereY + 1)
        ELSE
            MultDisplayEnable := False;
        END;

    IF (DoingDXMults) AND (MultByBand OR (Band = All)) AND MultDisplayEnable AND
       (ActiveDXMult <> NoCountDXMults) THEN
        BEGIN
        IF MultByMode THEN
            BEGIN
            IF MTotals [Band, ActiveMode].NumberDXMults < 100 THEN
                Write (MTotals [Band, ActiveMode].NumberDXMults:5)
            ELSE
                Write (MTotals [Band, ActiveMode].NumberDXMults:6)
            END
        ELSE
            IF MTotals [Band, Both].NumberDXMults < 100 THEN
                Write (MTotals [Band, Both].NumberDXMults:5)
            ELSE
                Write (MTotals [Band, Both].NumberDXMults:6);

        IF WhereY <= 5 THEN
            GoToXY (InitialCursorX, WhereY + 1)
        ELSE
            MultDisplayEnable := False;
        END;

    IF (DoingPrefixMults) AND (MultByBand OR (Band = All)) AND MultDisplayEnable THEN
        BEGIN
        IF MultByMode THEN
            BEGIN
            IF MTotals [Band, ActiveMode].NumberPrefixMults < 100 THEN
                Write (MTotals [Band, ActiveMode].NumberPrefixMults:5)
            ELSE
                Write (MTotals [Band, ActiveMode].NumberPrefixMults:6)
            END
        ELSE
            IF MTotals [Band, Both].NumberPrefixMults < 100 THEN
                Write (MTotals [Band, Both].NumberPrefixMults:5)
            ELSE
                Write (MTotals [Band, Both].NumberPrefixMults:6);

        IF WhereY <= 5 THEN
            GoToXY (InitialCursorX, WhereY + 1)
        ELSE
            MultDisplayEnable := False;
        END;

    IF (DoingZoneMults) AND (MultByBand OR (Band = All)) AND MultDisplayEnable THEN
        BEGIN
        IF MultByMode THEN
            BEGIN
            IF MTotals [Band, ActiveMode].NumberZoneMults < 100 THEN
                Write (MTotals [Band, ActiveMode].NumberZoneMults:5)
            ELSE
                Write (MTotals [Band, ActiveMode].NumberZoneMults:6)
            END
        ELSE
            IF MTotals [Band, Both].NumberZoneMults < 100 THEN
                Write (MTotals [Band, Both].NumberZoneMults:5)
            ELSE
                Write (MTotals [Band, Both].NumberZoneMults:6);

        IF WhereY <= 5 THEN
            GoToXY (InitialCursorX, WhereY + 1)
        ELSE
            MultDisplayEnable := False;
        END;

    GoToXY (InitialCursorX + 6, 1);
    END;



PROCEDURE UpdateTotals;

{ This procedure will update the QSO and score information.  This is a
  generic six band total summary with both modes shown.  Someone should
  put a case statement in here someday and make it more appropriate to
  different contest.                                                    }

VAR Band: BandType;
    Mode: ModeType;

    BEGIN
    QTotals := QSOTotals;
    Sheet.MultSheetTotals (MTotals);
    VisibleLog.IncrementQSOTotalsWithContentsOfEditableWindow  (QTotals);
    VisibleLog.IncrementMultTotalsWithContentsOfEditableWindow (MTotals);

    SaveSetAndClearActiveWindow (TotalWindow);
    GoToXY (1, 2);

    IF QSOByMode THEN
        BEGIN
        IF (ActiveMode = CW) OR ((QTotals [All, CW] > 0) AND (NumberDifferentMults < 3)) THEN
            BEGIN
            GoToXY (1, WhereY + 1);
            Write   ('CW QSOs');
            END;

        IF (ActiveMode = Phone) OR ((QTotals [All, Phone] > 0) AND (NumberDifferentMults < 3)) THEN
            BEGIN
            GoToXY (1, WhereY + 1);
            Write   ('SSB QSOs');
            END;

        IF (ActiveMode = Digital) OR ((QTotals [All, Digital] > 0) AND (NumberDifferentMults < 3)) THEN
            BEGIN
            GoToXY (1, WhereY + 1);
            Write   ('DIG QSOs');
            END;
        END
    ELSE
        BEGIN
        GoToXY (1, WhereY + 1);
        Write ('QSOs');
        END;

    IF DoingDomesticMults AND (WhereY <= 5) THEN
        BEGIN
        IF MultByMode THEN
            BEGIN
            GoToXY (1, WhereY + 1);
            IF DomesticQTHDataFileName = 'IARUHQ.DOM' THEN {KK1L: 6.68 Added for IARU. Pretty cheesy check!}
                BEGIN
                IF ActiveMode = CW THEN Write    ('CW HQmlt');
                IF ActiveMode = Phone THEN Write ('Ph HQmlt');
                END
            ELSE
                BEGIN
                IF ActiveMode = CW THEN Write    ('CW Domlt');
                IF ActiveMode = Phone THEN Write ('Ph Domlt');
                END;
            END
        ELSE
            BEGIN
            GoToXY (1, WhereY + 1);
            IF DomesticQTHDataFileName = 'IARUHQ.DOM' THEN {KK1L: 6.68 Added for IARU. Pretty cheesy check!}
                Write ('HQ Mults')
            ELSE
                Write ('DomMults');
            END;
        END;


    IF DoingDXMults AND (ActiveDXMult <> NoCountDXMults) AND (WhereY <= 5) THEN
        BEGIN
        IF MultByMode THEN
            BEGIN
            GoToXY (1, WhereY + 1);
            IF ActiveMode = CW THEN Write    ('CW DXMt');
            IF ActiveMode = Phone THEN Write ('Ph DXMt');
            END
        ELSE
            BEGIN
            GoToXY (1, WhereY + 1);
            Write ('DX Mults');
            END;
        END;

    IF DoingPrefixMults AND (WhereY <= 5) THEN
        BEGIN
        IF MultByMode THEN
            BEGIN
            GoToXY (1, WhereY + 1);
            IF ActiveMode = CW THEN Write    ('CW Pfxs');
            IF ActiveMode = Phone THEN Write ('Ph Pfxs');
            END
        ELSE
            BEGIN
            GoToXY (1, WhereY + 1);
            Write ('Prefix');
            END;

        END;

    IF DoingZoneMults AND (WhereY <= 5) THEN
        BEGIN
        IF MultByMode THEN
            BEGIN
            GoToXY (1, WhereY + 1);
            IF ActiveMode = CW THEN Write    ('CW Zone');
            IF ActiveMode = Phone THEN Write ('Ph Zone');
            END
        ELSE
            BEGIN
            GoToXY (1, WhereY + 1);
            Write ('Zone');
            END;
        END;

    GoToXY (9, 1);   { Upper left corner of first band info block }

    IF (ActiveBand = Band160) OR (ActiveBand = Band80) OR
       (ActiveBand = Band40)  OR (ActiveBand = Band20) OR
       (ActiveBand = Band15)  OR (ActiveBand = Band10) THEN
           BEGIN
           DisplayBandTotals (Band160);
           DisplayBandTotals (Band80);
           DisplayBandTotals (Band40);
           DisplayBandTotals (Band20);
           DisplayBandTotals (Band15);
           DisplayBandTotals (Band10);
           END
       ELSE
           IF (ActiveBand = Band30) OR (ActiveBand = Band17) OR
              (ActiveBand = Band12) THEN
                  BEGIN
                  DisplayBandTotals (Band40);
                  DisplayBandTotals (Band30);
                  DisplayBandTotals (Band20);
                  DisplayBandTotals (Band17);
                  DisplayBandTotals (Band15);
                  DisplayBandTotals (Band12);
                  END
              ELSE
                  IF (ActiveBand = Band6) OR
                     (ActiveBand = Band2) OR
                     (ActiveBand = Band222) OR
                     (ActiveBand = Band432) OR
                     (ActiveBand = Band902) OR
                     (ActiveBand = Band1296) THEN
                         BEGIN
                         DisplayBandTotals (Band6);
                         DisplayBandTotals (Band2);
                         DisplayBandTotals (Band222);
                         DisplayBandTotals (Band432);
                         DisplayBandTotals (Band902);
                         DisplayBandTotals (Band1296);
                         END
                     ELSE
                         IF (ActiveBand = Band2304) OR
                            (ActiveBand = Band3456) OR
                            (ActiveBand = Band5760) OR
                            (ActiveBand = Band10G) OR
                            (ActiveBand = Band24G) OR
                            (ActiveBand = BandLight) THEN
                                BEGIN
                                DisplayBandTotals (Band2304);
                                DisplayBandTotals (Band3456);
                                DisplayBandTotals (Band5760);
                                DisplayBandTotals (Band10G);
                                DisplayBandTotals (Band24G);
                                DisplayBandTotals (BandLight);
                                END;

    DisplayBandTotals (All);

    IF QTCsEnabled THEN
        BEGIN
        GoToXY (1, Hi (WindMax));

        IF MyContinent <> Europe THEN
            BEGIN
            Write ('QTCs Pending = ', TotalPendingQTCs);
            Write ('  Number QTCs sent = ', TotalNumberQTCsProcessed);
            END
        ELSE
            BEGIN
            Write ('Number QTCs received = ', TotalNumberQTCsProcessed);
            END;
        END;

    RestorePreviousWindow;
    END;



PROCEDURE BandUp;

    BEGIN
    IF (MultipleBandsEnabled) OR (TotalContacts = 0) THEN
        BEGIN
        IF CommandUseInactiveRadio THEN {KK1L: 6.73 Band change on inactive radio via command}
            BEGIN
            BandChange (BandMemory[InactiveRadio], DirectionUp);

            IF InactiveRadio = RadioOne THEN
                StableRadio1Freq := 0
            ELSE
                StableRadio2Freq := 0;
            END
        ELSE
            BEGIN
            BandChange (ActiveBand, DirectionUp);

            IF ActiveRadio = RadioOne THEN
                BEGIN
                {KK1L: 6.73 This gets done in UpdateTimeAndRateDisplay. Only do if no radio connected.}
                IF (Radio1ControlPort = nil) OR (NOT PollRadioOne) THEN BandMemory [RadioOne] := ActiveBand;
                StableRadio1Freq := 0;
                END
            ELSE
                BEGIN
                {KK1L: 6.73 This gets done in UpdateTimeAndRateDisplay. Only do if no radio connected.}
                IF (Radio2ControlPort = nil) OR (NOT PollRadioTwo) THEN BandMemory [RadioTwo] := ActiveBand;
                StableRadio2Freq := 0;
                END;

            UpdateTotals;

            IF QSOByBand THEN VisibleDupeSheetChanged := True;

            IF MultByBand THEN
                VisibleLog.ShowRemainingMultipliers;

            BandMapBand := ActiveBand;
            DisplayBandMap;

            IF QSONumberByBand THEN
                DisplayNextQSONumber (TotalContacts + 1);
            END;
        END;
    END;


PROCEDURE BandDown;

    BEGIN
    IF (MultipleBandsEnabled) OR (TotalContacts = 0) THEN
        BEGIN
        IF CommandUseInactiveRadio THEN {KK1L: 6.73 Band change on inactive radio via command}
            BEGIN
            BandChange (BandMemory[InactiveRadio], DirectionDown);

            IF InactiveRadio = RadioOne THEN
                StableRadio1Freq := 0
            ELSE
                StableRadio2Freq := 0;
            END
        ELSE
            BEGIN
            BandChange (ActiveBand, DirectionDown);

            IF ActiveRadio = RadioOne THEN
                BEGIN
                {KK1L: 6.73 This gets done in UpdateTimeAndRateDisplay. Only do if no radio connected.}
                IF (Radio1ControlPort = nil) OR (NOT PollRadioOne) THEN BandMemory [RadioOne] := ActiveBand;
                StableRadio1Freq := 0; {KK1L: 6.71 Was in BandUp and not here.}
                END
            ELSE
                BEGIN
                {KK1L: 6.73 This gets done in UpdateTimeAndRateDisplay. Only do if no radio connected.}
                IF (Radio2ControlPort = nil) OR (NOT PollRadioTwo) THEN BandMemory [RadioTwo] := ActiveBand;
                StableRadio2Freq := 0; {KK1L: 6.71 Was in BandUp and not here.}
                END;

            UpdateTotals;

            IF QSOByBand THEN VisibleDupeSheetChanged := True;

            IF MultByBand THEN
                VisibleLog.ShowRemainingMultipliers;

            BandMapBand := ActiveBand;
            DisplayBandMap;

            IF QSONumberByBand THEN
                DisplayNextQSONumber (TotalContacts + 1);

            END;
        END;
    END;



PROCEDURE FlagDupesInPossibleCallList (Band: Bandtype;
                                       Mode: ModeType;
                                       VAR PossCallList: PossibleCallRecord);

VAR Address, Entry: INTEGER;
    PossibleCall, Call: CallString;


    BEGIN
    IF PossibleCallList.NumberPossibleCalls > 0 THEN
        FOR Address := 0 TO PossibleCallList.NumberPossibleCalls - 1 DO
            BEGIN
            PossibleCall := PossibleCallList.List [Address].Call;


            IF Sheet.CallIsADupe (PossibleCall, Band, Mode) THEN
                PossCallList.List [Address].Dupe := True
            ELSE
                FOR Entry := 1 TO 5 DO
                    BEGIN
                    Call := UpperCase (GetLogEntryCall (VisibleLog.LogEntries [Entry]));

                    IF PossibleCall = Call THEN    { This is a dupe? }
                        IF ((ActiveMode = GetLogEntryMode (VisibleLog.LogEntries [Entry])) OR
                           (NOT QSOByMode)) AND
                           ((ActiveBand = GetLogEntryBand (VisibleLog.LogEntries [Entry])) OR
                           (NOT QSOByBand)) THEN
                               PossibleCallList.List [Address].Dupe := True;

                    END;
            END;
    END;

PROCEDURE EditableLog.UpdateTempLogFile;

VAR Entry: INTEGER;
    FileWrite: TEXT;

    BEGIN
    IF OpenFileForWrite (FileWrite, LogTempFileName) THEN
        BEGIN
        FOR Entry := 1 TO NumberEditableLines DO
            WriteLn (FileWrite, LogEntries [Entry]);
        Close (FileWrite);
        END;
    END;




PROCEDURE EditableLog.SetUpEditableLog;

VAR Entry: INTEGER;
    FileRead: TEXT;
    TempBand: BandType;

    BEGIN
    IF OpenFileForRead (FileRead, LogTempFileName) THEN
        BEGIN
        FOR Entry := 1 TO NumberEditableLines DO
            ReadLn (FileRead, LogEntries [Entry]);
        Close (FileRead);
        END
    ELSE
        FOR Entry := 1 TO NumberEditableLines DO
            LogEntries [Entry] := '';

    DisplayEditableLog (LogEntries);
    END;


FUNCTION EditableLog.PushLogEntry (NewEntry: Str80): Str80;

VAR Entry: INTEGER;

    BEGIN
    PushLogEntry := LogEntries [1];

    FOR Entry := 1 TO NumberEditableLines - 1 DO
        LogEntries [Entry] := LogEntries [Entry + 1];

    LogEntries [NumberEditableLines] := NewEntry;

    IF ActiveWindow <> DupeSheetWindow THEN { not doing packet window }
        DisplayEditableLog (LogEntries);

    UpdateTempLogFile;
    END;


FUNCTION EditableLog.CallIsInEditableLog (Call: CallString; Band: BandType; Mode: ModeType): BOOLEAN;

VAR Entry: INTEGER;

    BEGIN
    FOR Entry := 1 TO NumberEditableLines DO
        IF (Band = GetLogEntryBand (LogEntries [Entry])) OR NOT QSOByBand THEN
            IF (Mode = GetLogEntryMode (LogEntries [Entry])) OR NOT QSOByMode THEN
                IF GetLogEntryCall (LogEntries [Entry]) = Call THEN
                    BEGIN
                    CallIsInEditableLog := True;
                    Exit;
                    END;

    CallIsInEditableLog := False;
    END;



FUNCTION EditableLog.CallIsADupe (Call: CallString; Band: BandType; Mode: ModeType): BOOLEAN;

VAR Entry: INTEGER;

    BEGIN
    CallIsADupe := False;

    IF (ActiveDomesticMult = GridSquares) AND RoverCall (Call) THEN
        Exit;

    IF Sheet.CallIsADupe (Call, Band, Mode) OR CallIsInEditableLog (Call, Band, Mode)  THEN
        CallIsADupe := True;
    END;




PROCEDURE WriteVisibleDupeSheetEntry (Entry: Str20;
                                      NumberCalls: INTEGER);

VAR NumberPossibleRows: INTEGER;

    BEGIN
    NumberPossibleRows := Hi (WindMax) - Hi (WindMin) + 1;

    IF NumberCalls <= (NumberPossibleRows * 11 + 1) THEN
        BEGIN
        WHILE (Entry [3] > '9') AND (Length (Entry) < 6) DO
            Entry := ' ' + Entry;

        IF WhereX = 1 THEN
            BEGIN
            Write (Entry);
            WHILE WhereX MOD 7 <> 0 DO Write (' ');
            END
        ELSE
            BEGIN
            IF WhereY < NumberPossibleRows THEN
                GoToXY (WhereX - 6, WhereY + 1)
            ELSE
                GoToXY (WhereX + 1, 1);

            Write (Entry);

            WHILE WhereX MOD 7 <> 0 DO Write (' ');
            END;
        END
    ELSE
        BEGIN
        IF Entry = '------' THEN Entry := '  ';

        IF WhereX + Length (Entry) < 79 THEN
            Write (Entry + ' ')
        ELSE
            BEGIN
            GoToXY (1, WhereY + 1);
            Write (Entry, ' ');
            END;
        END;
    END;



PROCEDURE SelectProperColor (CallDistrict: INTEGER);

    BEGIN
    IF DoingColors THEN
        BEGIN
        CASE CallDistrict OF
               1: BEGIN
                  SetBackground (Red);
                  SetColor      (White);
                  END;

               6: BEGIN
                  WriteVisibleDupeSheetEntry ('------', NumberVDCalls);
                  SetBackground (Red);
                  SetColor      (White);
                  END;

            2, 7: BEGIN
                  WriteVisibleDupeSheetEntry ('------', NumberVDCalls);
                  SetBackground (Green);
                  SetColor      (White);
                  END;

            3, 8: BEGIN
                  WriteVisibleDupeSheetEntry ('------', NumberVDCalls);
                  SetBackground (Magenta);
                  SetColor      (White);
                  END;

            4, 9: BEGIN
                  WriteVisibleDupeSheetEntry ('------', NumberVDCalls);
                  SetBackground (Brown);
                  SetColor      (White);
                  END;

            5,10: BEGIN
                  WriteVisibleDupeSheetEntry ('------', NumberVDCalls);
                  SetBackground (Blue);
                  SetColor      (White);
                  END;

            END; { of CASE }

        END
    ELSE
        IF CallDistrict MOD 2 = 0 THEN
             BEGIN
             SetBackground (SelectedColors.DupeSheetWindowBackground);
             SetColor      (SelectedColors.DupeSheetWindowColor);
             END
         ELSE
             BEGIN
             SetColor      (SelectedColors.DupeSheetWindowBackground);
             SetBackground (SelectedColors.DupeSheetWindowColor);
             END;
    END;



FUNCTION ComputeGrid (Row: INTEGER; Col: INTEGER; NumberRows: INTEGER): Str10;

VAR HomeX, HomeY, CharValue, Address: INTEGER;
    Grid: Str10;

    BEGIN
    HomeX := 8;
    HomeY := NumberRows DIV 2;

    IF (Row = HomeY) AND (Col = HomeX) THEN
        BEGIN
        ComputeGrid := GridMapCenter;
        Exit;
        END;

    Grid := GridMapCenter;

    IF Col < HomeX THEN
        BEGIN
        FOR Address := HomeX - 1 DOWNTO Col DO
            BEGIN
            CharValue := Ord (Grid [3]);
            Dec (CharValue);

            IF CharValue >= Ord ('0') THEN
                Grid [3] := Chr (CharValue)
            ELSE
                BEGIN
                Grid [3] := '9';

                CharValue := Ord (Grid [1]);
                Dec (CharValue);

                IF CharValue >= Ord ('A') THEN
                    Grid [1] := Chr (CharValue)
                ELSE
                    Grid [1] := 'R';
                END;

            END;
        END

    ELSE
        FOR Address := HomeX + 1 TO Col DO     { Row > HomeX }
            BEGIN
            CharValue := Ord (Grid [3]);
            Inc (CharValue);

            IF CharValue <= Ord ('9') THEN
                Grid [3] := Chr (CharValue)
            ELSE
                BEGIN
                Grid [3] := '0';

                CharValue := Ord (Grid [1]);
                Inc (CharValue);

                IF CharValue <= Ord ('R') THEN
                    Grid [1] := Chr (CharValue)
                ELSE
                    Grid [1] := 'A';
                END;
            END;


    IF Row > HomeY THEN
        BEGIN
        FOR Address := HomeY + 1 TO Row DO
            BEGIN
            CharValue := Ord (Grid [4]);
            Dec (CharValue);

            IF CharValue >= Ord ('0') THEN
                Grid [4] := Chr (CharValue)
            ELSE
                BEGIN
                Grid [4] := '9';

                CharValue := Ord (Grid [2]);
                Dec (CharValue);

                IF CharValue >= Ord ('A') THEN
                    Grid [2] := Chr (CharValue)
                ELSE
                    Grid [2] := 'R';
                END;

            END;
        END

    ELSE
        FOR Address := HomeY - 1 DOWNTO Row DO
            BEGIN
            CharValue := Ord (Grid [4]);
            Inc (CharValue);

            IF CharValue <= Ord ('9') THEN
                Grid [4] := Chr (CharValue)
            ELSE
                BEGIN
                Grid [4] := '0';

                CharValue := Ord (Grid [2]);
                Inc (CharValue);

                IF CharValue <= Ord ('R') THEN
                    Grid [2] := Chr (CharValue)
                ELSE
                    Grid [2] := 'A';
                END;
            END;


    ComputeGrid := Grid;
    END;



FUNCTION EditableLog.IsNotFoundInEditableWindow (ActiveGrid: Str10; Band: BandType; Mode: ModeType): BOOLEAN;

VAR Entry: INTEGER;
    MultString: Str20;

    BEGIN
    FOR Entry := 1 TO NumberEditableLines DO
        IF (Band = GetLogEntryBand (LogEntries [Entry])) OR NOT MultByBand THEN
            IF (Mode = GetLogEntryMode (LogEntries [Entry])) OR NOT MultByMode THEN
                BEGIN
                MultString := UpperCase (Copy (LogEntries [Entry], LogEntryMultAddress, LogentryMultWidth));
                GetRidOfPostcedingSpaces (MultString);

                IF ActiveGrid = MultString THEN
                    BEGIN
                    IsNotFoundInEditableWindow := False;
                    Exit;
                    END;
                END;

    IsNotFoundInEditableWindow := True;
    END;




PROCEDURE MoveGridMap (Key: Char);

    BEGIN
    CASE Key OF
        DownArrow:
            BEGIN
            IF GridMapCenter [4] <> '0' THEN
                GridMapCenter := Copy (GridMapCenter, 1, 3) + Chr (Ord (GridMapCenter [4]) - 1)
            ELSE
                GridMapCenter := GridMapCenter [1] + Chr (Ord (GridMapCenter [2]) - 1) +
                                 GridMapcenter [3] + '9';
            END;


        UpArrow: ;

        ControlLeftArrow:
            BEGIN
            IF GridMapCenter [3] <> '0' THEN
                GridMapCenter := Copy (GridMapCenter, 1, 2) + Chr (Ord (GridMapCenter [3]) - 1) +
                                 GridMapCenter [4]
            ELSE
                GridMapCenter := Chr (Ord (GridMapCenter [1]) - 1) + GridMapCenter [2] +
                                 '9' + GridMapCenter [4];
            END;


        ControlRightArrow:
            BEGIN
            IF GridMapCenter [3] <> '9' THEN
                GridMapCenter := Copy (GridMapCenter, 1, 2) + Chr (Ord (GridMapCenter [3]) + 1) +
                                 GridMapCenter [4]
            ELSE
                GridMapCenter := Chr (Ord (GridMapCenter [1]) + 1) + GridMapCenter [2] +
                                 '0' + GridMapCenter [4];
            END;
        END;
    END;



PROCEDURE EditableLog.DisplayGridMap (Band: BandType; Mode: ModeType);

VAR NumberRows, Row, Col: INTEGER;
    ActiveGrid: Str10;

    BEGIN
    IF (GridMapCenter = '') OR (ActiveDomesticMult <> GridSquares) THEN Exit;

    IF NOT MultByBand THEN Band := All;
    IF NOT MultByMode THEN Mode := Both;

    SaveAndSetActiveWindow (DupeSheetWindow);
    ClrScr;

    WriteLn  ('                           GRID STATUS FOR ' + BandString [Band]);

    NumberRows := Hi (WindMax) - Hi (WindMin);

    FOR Row := 1 TO NumberRows DO
        FOR Col := 1 TO 16 DO
            BEGIN
            ActiveGrid := ComputeGrid (Row, Col, NumberRows);

            IF Sheet.IsADomesticMult (ActiveGrid, Band, Mode) AND
                IsNotFoundInEditableWindow (ActiveGrid, Band, Mode) THEN
                    TextColor (Black)
                ELSE
                    TextColor (LightBlue);

            IF WhereX > 76 THEN WriteLn;
            IF WhereX > 1 THEN Write (' ');
            Write (ActiveGrid);
            END;

    RestorePreviousWindow;
    END;



PROCEDURE EditableLog.DisplayVisibleDupeSheet (Band: BandType; Mode: ModeType);

VAR CallsThisDistrict, CallDistrict, NumberDistrictsOver25: INTEGER;
    ActiveVDEntry: VDEntryPointer;
    CallDistrictOver25, Entry: INTEGER;
    CallDistrictTotals: CallDistrictTotalArray;
    DoingColumnDupeSheet: BOOLEAN;


    BEGIN
    IF NOT VisibleDupeSheetChanged THEN Exit;

    VisibleDupeSheetChanged := False;

    CallDistrictOver25 := 100;

    IF NOT QSOByBand THEN Band := All;
    IF NOT QSOByMode THEN Mode := Both;

    FOR CallDistrict := 1 TO 10 DO CallDistrictTotals [CallDistrict] := 0;

    Sheet.CreateVisibleDupesheetArrays (Band, Mode);

    FOR Entry := 1 TO NumberEditableLines DO
        IF (Band = GetLogEntryBand (LogEntries [Entry])) OR NOT QSOByBand THEN
            IF (Mode = GetLogEntryMode (LogEntries [Entry])) OR NOT QSOByMode THEN
                Sheet.AddCallToVisibleDupeSheet (RootCall (GetLogEntryCall (LogEntries [Entry])));

    IF NumberVDCalls = 0 THEN
        BEGIN
        SaveAndSetActiveWindow (DupeSheetWindow);
        ClrScr;
        Write ('No QSOs on this band/mode!');
        RestorePreviousWindow;
        Exit;
        END;

    IF ColumnDupesheetEnable AND SuperDupeSheet THEN
        BEGIN
        ActiveVDEntry := FirstVDEntry;
        NumberDistrictsOver25 := 0;

        CallDistrict := 0;
        CallsThisDistrict := 0;

        REPEAT
            IF Length (ActiveVDEntry^.Callsign) > 1 THEN
                Inc (CallsThisDistrict)
            ELSE
                BEGIN
                Inc (CallDistrict);

                IF CallsThisDistrict > 25 THEN
                    BEGIN
                    Inc (NumberDistrictsOver25);
                    CallsThisDistrict := 0;
                    CallDistrictOver25 := CallDistrict - 1;
                    END;

                CallsThisDistrict := 0;
                END;

            ActiveVDEntry := ActiveVDEntry^.NextEntry;

        UNTIL (ActiveVDEntry = Nil) OR (CallDistrict > 10);

        DoingColumnDupeSheet := NumberDistrictsOver25 <= 1;
        END
    ELSE
        DoingColumnDupeSheet := False;

    SaveAndSetActiveWindow (DupeSheetWindow);
    ClrScr;

    CallDistrict := 0;

    ActiveVDEntry := FirstVDEntry;

    REPEAT
        IF Length (ActiveVDEntry^.Callsign) > 1 THEN
            WriteVisibleDupeSheetEntry (ActiveVDEntry^.Callsign, NumberVDCalls)
        ELSE
            BEGIN
            Inc (CallDistrict);
            SelectProperColor (CallDistrict);

            IF DoingColumnDupeSheet THEN
                IF (CallDistrict <> 1) THEN
                    BEGIN
                    IF CallDistrict > CallDistrictOver25 THEN
                        GoToXY (CallDistrict * 7, 25)
                    ELSE
                        GoToXY ((CallDistrict - 1) * 7, 25);

                    END;

            END;

        IF (ActiveVDEntry^.NextEntry = Nil) OR (CallDistrict > 10) THEN
            BEGIN
            RestorePreviousWindow;
            Exit;
            END;

        ActiveVDEntry := ActiveVDEntry^.NextEntry;

    UNTIL ActiveVDEntry = Nil;
    END;



FUNCTION  EditableLog.EditableLogIsEmpty: BOOLEAN;

VAR Entry: INTEGER;

    BEGIN
    EditableLogIsEmpty := False;

    FOR Entry := 1 TO 5 DO
        IF GetLogEntryCall (VisibleLog.LogEntries [Entry]) <> '' THEN Exit;

    EditableLogIsEmpty := True;
    END;



PROCEDURE EditableLog.EditLog;

{ This procedure gets called when the operator wants to edit the log.
  Wordstar type cursor commands are supported.  The contents of the
  LogEntries array will be updated along with the display.  There is
  no checking for syntax or new mults.  When the entry goes from the
  editable log window to the LogFileName file, it will be looked at for
  proper syntax and possible new multipliers.  Also, dupes are checked
  at that time.                                                         }

VAR ChangesMade: BOOLEAN;

    BEGIN
    IF VisibleDupesheetEnable THEN SetUpEditableLog;
    QuickDisplay ('You are editing the logsheet.  Use escape to quit.');
    EditWindowEditor (LogEntries, LogEntryCallAddress, NumberEditableLines, ChangesMade);
    IF ChangesMade THEN UpdateTempLogFile;
    IF ActiveWindow <> QuickCommandWindow THEN RemoveWindow (QuickCommandWindow);
    END;



PROCEDURE EditableLog.IncrementQSOPointsWithContentsOfEditableWindow (VAR QPoints: LongInt);

VAR Points, LogEntry: INTEGER;
    LogEntryMode: ModeType;

    BEGIN
    FOR LogEntry := 1 TO NumberEditableLines DO
        BEGIN
        LogEntryMode := GetLogEntryMode (LogEntries [LogEntry]);

        IF LogEntryMode <> NoMode THEN
            BEGIN
            Points := GetLogEntryQSOPoints (LogEntries [LogEntry]);

            IF SingleBand <> All THEN
                BEGIN
                IF GetLogEntryBand (LogEntries [LogEntry]) = SingleBand THEN
                    QPoints := QPoints + Points;
                END
            ELSE
                QPoints := QPoints + Points;
            END;
        END;
    END;


PROCEDURE EditableLog.IncrementQSOTotalsWithContentsOfEditableWindow (VAR QSOTotals: QSOTotalArray);

VAR LogEntry: INTEGER;
    LogEntryMode: ModeType;
    LogEntryBand: BandType;

    BEGIN
    FOR LogEntry := 1 TO NumberEditableLines DO
        BEGIN
        LogEntryMode := GetLogEntryMode (LogEntries [LogEntry]);

        IF LogEntryMode <> NoMode THEN
            BEGIN
            LogEntryBand := GetLogEntryBand (LogEntries [LogEntry]);
            Inc (QSOTotals [LogEntryBand, LogEntryMode]);
            Inc (QSOTotals [LogEntryBand, Both]);
            Inc (QSOTotals [All, LogEntryMode]);
            Inc (QSOTotals [All, Both]);
            END;
        END;
    END;



PROCEDURE EditableLog.IncrementMultTotalsWithContentsOfEditableWindow (VAR MTotals: MultTotalArrayType);

VAR LogEntry: INTEGER;
    TempRXData: ContestExchange;

    BEGIN
    FOR LogEntry := 1 TO NumberEditableLines DO
        BEGIN
        ClearContestExchange (TempRXData);
        TempRXData.Band := GetLogEntryBand (LogEntries [LogEntry]);

        IF TempRXData.Band <> NoBand THEN
            BEGIN
            TempRXData.Mode := GetLogEntryMode (LogEntries [LogEntry]);
            GetMultsFromLogEntry (LogEntries [LogEntry], TempRXData);

            IF TempRXData.DomesticQTH <> '' THEN
                BEGIN
                Inc (MTotals [TempRXData.Band, TempRXData.Mode].NumberDomesticMults);
                Inc (MTotals [TempRXData.Band, Both].           NumberDomesticMults);
                Inc (MTotals [All,             TempRXData.Mode].NumberDomesticMults);
                Inc (MTotals [All,  Both].                      NumberDomesticMults);
                END;

            IF TempRXData.DXQTH <> '' THEN
                BEGIN
                {KK1L: 6.65 TEST DX mult limit for NEQP. Works but need to fix read log.dat}
                IF (MTotals [TempRXData.Band, TempRXData.Mode].NumberDXMults < DXMultLimit) THEN
                  Inc (MTotals [TempRXData.Band, TempRXData.Mode].NumberDXMults);
                IF (MTotals [TempRXData.Band, Both].           NumberDXMults < DXMultLimit) THEN
                  Inc (MTotals [TempRXData.Band, Both].           NumberDXMults);
                IF (MTotals [All,  TempRXData.Mode].           NumberDXMults < DXMultLimit) THEN
                  Inc (MTotals [All,  TempRXData.Mode].           NumberDXMults);
                IF (MTotals [All,  Both].                      NumberDXMults < DXMultLimit) THEN
                  Inc (MTotals [All,  Both].                      NumberDXMults);
                END;

            IF TempRXData.Prefix <> '' THEN
                BEGIN
                Inc (MTotals [TempRXData.Band, TempRXData.Mode].NumberPrefixMults);
                Inc (MTotals [TempRXData.Band, Both].           NumberPrefixMults);
                Inc (MTotals [All,  TempRXData.Mode].           NumberPrefixMults);
                Inc (MTotals [All,  Both].                      NumberPrefixMults);
                END;

            IF TempRXData.Zone <> '' THEN
                BEGIN
                Inc (MTotals [TempRXData.Band, TempRXData.Mode].NumberZoneMults);
                Inc (MTotals [TempRXData.Band, Both].           NumberZoneMults);
                Inc (MTotals [All,  TempRXData.Mode].           NumberZoneMults);
                Inc (MTotals [All,  Both].                      NumberZoneMults);
                END;

            END;
        END;
    END;



PROCEDURE EditableLog.CancelOutMultsThatAreInEditableWindow (VAR Exchange: ContestExchange);

{ This procedure will look though the editable log and see if any of the
  new multipliers in the contest record alredy exist in the editable window.
  If so, their mult flags will be cleared.                            }

VAR Entry: INTEGER;
    MultString: Str80;
    DomQTH: Str20;

    BEGIN
    FOR Entry := 1 TO NumberEditableLines DO
        BEGIN
        IF Exchange.DomesticMult OR Exchange.DXMult OR Exchange.PrefixMult OR Exchange.ZoneMult THEN
            BEGIN
            IF (Exchange.Band = GetLogEntryBand (LogEntries [Entry])) OR NOT MultByBand THEN
                BEGIN
                IF (Exchange.Mode = GetLogEntryMode (LogEntries [Entry])) OR NOT MultByMode THEN
                    BEGIN
                    MultString := Copy (LogEntries [Entry], LogEntryMultAddress, LogentryMultWidth);

                    GetRidOfPrecedingSpaces (MultString);
                    MultString := ' ' + MultString + ' ';

                    IF DoingDomesticMults THEN
                        BEGIN
                        IF StringHas (Exchange.DomMultQTH, '/') THEN
                            DomQTH := PrecedingString (Exchange.DomMultQTH, '/')
                        ELSE
                            DomQTH := Exchange.DomMultQTH;

                        IF (StringHas (MultString, ' ' + DomQTH + ' ')) OR
                           (StringHas (MultString, ' ' + DomQTH + '/')) THEN
                            Exchange.DomesticMult := False;
                        END;

                    IF DoingDXMults AND (ActiveDXMult <> NoCountDXMults) THEN
                        IF StringHas (MultString, ' ' + Exchange.DXQTH + ' ') THEN
                            Exchange.DXMult := False;

                    IF DoingPrefixMults THEN
                        IF StringHas (MultString, ' ' + Exchange.Prefix + ' ') THEN
                            Exchange.PrefixMult := False;

                    IF DoingZoneMults THEN
                        IF StringHas (MultString, ' ' + Exchange.Zone + ' ') THEN
                            Exchange.ZoneMult := False;

                    END;
                END;
            END;
        END;
    END;



PROCEDURE EditableLog.ProcessMultipliers (VAR RXData: ContestExchange);

{ This procedure will set the appropriate multiplier flags for a QSO
  that is ready to be put into the editable log.  It will check both the
  multiplier arrays in LogDupe and the contents of the editable log for
  previous occurances of multipliers.                                   }

    BEGIN
    IF RXData.InhibitMults THEN Exit;

    Sheet.SetMultFlags (RXData);

    CancelOutMultsThatAreInEditableWindow (RXData);

    IF MultiplierAlarm THEN
        IF RXData.DomesticMult OR RXData.DXMult OR RXData.PrefixMult OR RXData.ZoneMult THEN
            Tone.DoABeep (BeepCongrats);
    END;


PROCEDURE FixMultiplierString (TempRXData: ContestExchange; VAR LogEntry: Str80);

VAR NewMultString: Str80;

    BEGIN
    NewMultString := '';
    MultiplierStamp (TempRXData, NewMultString);
    GetRidOfPrecedingSpaces (NewMultString);

    WHILE Length (NewMultString) < LogEntryPointsAddress - LogEntryMultAddress DO
        NewMultString := NewMultString + ' ';

    Delete (LogEntry, LogEntryMultAddress, LogEntryPointsAddress - LogentryMultAddress);
    Insert (NewMultString, LogEntry, LogEntryMultAddress);
    END;


PROCEDURE EditableLog.PutLogEntryIntoSheet (VAR LogEntry: Str80);

{ This procedure will look at the log entry string passed to it and
  see if any multiplier flags need to be set.  It will not erase any
  that are there.  After adding any that need to be added, it will
  add it to the multiplier arrays and increment the totals.  The
  correct string is returned.  This procedure should only be used for
  entries that have been popped off the editable log stack as it does
  not look for any mults in the editable log.  Use the ProcessMultipliers
  procedure for new entries into the editable log.                        }

VAR TempRXData: ContestExchange;
    QSOPoints: INTEGER;

    BEGIN
    TransferLogEntryInfoToContestExchange (LogEntry, TempRXData);

    IF (TempRXData.Mode = NoMode) OR (TempRXData.Band = NoBand) THEN Exit; {KK1L: 6.71 Fix "non" contacts from crashing}

    Inc (QSOTotals [TempRXData.Band, TempRXData.Mode]);
    Inc (QSOTotals [TempRXData.Band, Both]);
    Inc (QSOTotals [All,  TempRXData.Mode]);
    Inc (QSOTotals [All,  Both]);

    IF NumberDifferentMults = 0 THEN
        Sheet.AddQSOToSheets (TempRXData)
    ELSE
        BEGIN
        Sheet.SetMultFlags   (TempRXData);
        Sheet.AddQSOToSheets (TempRXData);
        {KK1L: 6.70 Sometimes there is just not a pretty way to do it!!}
        IF ActiveExchange <> RSTQTHNameAndFistsNumberOrPowerExchange THEN
            FixMultiplierString  (TempRXData, LogEntry);
        END;
    END;



PROCEDURE EditableLog.DetermineIfNewDomesticMult (DomesticMult: Str20;
                                                  Band: BandType;
                                                  Mode:ModeType;
                                                  VAR MultString: Str80);

VAR TempRXData: ContestExchange;

    BEGIN
    MultString := '';
    TempRXData.Band := Band;
    TempRXData.Mode := Mode;
    TempRXData.QTHString := DomesticMult;

    IF FoundDomesticQTH (TempRXData) THEN
        BEGIN
        Sheet.SetMultFlags (TempRXData);
        CancelOutMultsThatAreInEditableWindow (TempRXData);

        IF TempRXData.DomesticMult THEN
            MultString := TempRXData.DomMultQTH
        ELSE
            MultString := '';
        END
    ELSE
        MultString := '';
    END;



PROCEDURE EditableLog.DetermineIfNewMult (Call: CallString; Band: BandType;
                                          Mode:ModeType; VAR MultString: Str80);

{ This procedure will try and determine the multiplier for the call passed
  to it.  If something can be figured out, and it appears to be a new
  multiplier for the band/mode specified, it will be returned in the
  mult string.  Otherwise, a null string will be returned.  }

VAR TempRXData: ContestExchange;

    BEGIN
    ClearContestExchange (TempRXData);
    LocateCall (Call, TempRXData.QTH, True);

    TempRXData.Band := Band;
    TempRXData.Mode := Mode;

    MultString := '';

    IF MarineOrAirMobileStation (Call) AND (NoMultMarineMobile) THEN Exit; {KK1L: 6.68 Brute force no count /MM or /AM}

    IF DoingDXMults AND (ActiveDXMult <> NoCountDXMults) THEN GetDXQTH (TempRXData);

    IF DoingPrefixMults THEN
        CASE ActivePrefixMult OF
            SACDistricts: TempRXData.Prefix := SACDistrict (TempRXData.QTH)
            ELSE
                TempRXData.Prefix := TempRXData.QTH.Prefix;
            END;

    IF DoingZoneMults THEN
        BEGIN
        Str (TempRXData.QTH.Zone,  TempRXData.Zone);

        IF Length (TempRXData.Zone) < 2 THEN
            TempRXData.Zone := '0' + TempRXData.Zone;
        END;

    Sheet.SetMultFlags (TempRXData);

    CancelOutMultsThatAreInEditableWindow (TempRXData);

    IF TempRXData.DXMult THEN
        MultString := MultString + TempRXData.DXQTH + ' ';

    IF TempRXData.PrefixMult THEN
        BEGIN
        IF ActivePrefixMult = SACDistricts THEN
            MultString := MultString + SACDistrict (TempRXData.QTH) + ' '
        ELSE
            MultString := MultString + TempRXData.Prefix + ' ';
        END;

    IF TempRXData.ZoneMult THEN
        MultString := MultString + TempRXData.Zone;
    END;



PROCEDURE EditableLog.CreateModeSpecificDomesticMultiplierInfo
             (DomQth: Str20; Mode: ModeType; VAR OutputString: Str160);

VAR Band: BandType;
    MultString: Str20;

    BEGIN
    OutputString := '';

    IF Mode <> Both THEN
        OutputString := OutputString + ModeString [Mode] + ': ';

    IF MultByBand THEN
        BEGIN
        IF ActiveBand <= Band10 THEN
            Band := Band160
        ELSE
            Band := Band6;
        END
    ELSE
        Band := All;

    WHILE True DO
        BEGIN
        DetermineIfNewDomesticMult (DomQTH, Band, Mode, MultString);

        IF MultString <> '' THEN
            BEGIN
            IF Band <= Band10 THEN
                OutputString := OutputString + BandString [Band]
            ELSE
                IF Band = Band6 THEN
                    OutputString := OutputString + '6 '
                ELSE
                    IF Band = Band2 THEN
                        OutputString := OutputString + '2 '
                    ELSE
                        OutputString := OutputString + BandString [Band] + ' ';

            END
        ELSE
            IF Band <= Band10 THEN
                OutputString := OutputString + '   '
            ELSE
                IF (Band = Band6) OR (Band = Band2) THEN
                    OutputString := OutputString + '  '
                ELSE
                    OutputString := OutputString + '    ';


       CASE Band OF
           All:     Exit;
           Band160: Band := Band80;
           Band80:  Band := Band40;
           Band40:  Band := Band20;
           Band20:  Band := Band15;
           Band15:  Band := Band10;
           Band10:  Exit;

           Band6:    Band := Band2;
           Band2:    Band := Band222;
           Band222:  Band := Band432;
           Band432:  Band := Band902;
           Band902:  Band := Band1296;
           Band1296: Band := Band2304;
           Band2304: Band := Band3456;
           Band3456: Band := Band5760;
           Band5760: Band := Band10G;
           Band10G:  Band := Band24G;
           Band24G:  Band := BandLight;
           BandLight: Exit;
           END;
        END;
    END;



PROCEDURE EditableLog.ShowDomesticMultiplierStatus (DomesticQTH: Str20);

VAR OutputString: Str80;
    Band, FirstBand, LastBand: BandType;
    Mode, FirstMode, LastMode: ModeType;
    MultString, TempString: Str80;
    Cursor: INTEGER;
    QTotals: QSOTotalArray;

    BEGIN
    IF VisibleDupeSheetEnable AND NOT SuperDupesheet THEN Exit;
    IF MultByMode AND (NOT MultByBand) AND (NOT MultipleModesEnabled) THEN Exit;
    IF MultByBand AND (NOT MultByMode) AND (NOT MultipleBandsEnabled) THEN Exit;
    IF MultByBand AND MultByMode AND (NOT MultipleModesEnabled) AND (NOT MultipleBandsEnabled) THEN Exit;

    QTotals := QSOTotals;
    IncrementQSOTotalsWithContentsOfEditableWindow (QTotals);

    SaveSetAndClearActiveWindow (MultiplierInformationWindow);

    WriteLn ('Mult needs for ', DomesticQTH, ':');

    OutputString := '';

    IF MultByMode THEN
        BEGIN
        CreateModeSpecificDomesticMultiplierInfo (DomesticQTH, CW, OutputString);
        OutputQSOorMultStatusString (OutputString);
        OutputString := '';
        CreateModeSpecificDomesticMultiplierInfo (DomesticQTH, Phone, OutputString);
        OutputQSOorMultStatusString (OutputString);
        END
    ELSE
        BEGIN
        CreateModeSpecificDomesticMultiplierInfo (DomesticQTH, Both, OutputString);
        OutputQSOorMultStatusString (OutputString);
        END;

    RestorePreviousWindow;
    END;



PROCEDURE EditableLog.ShowMultiplierStatus (Call: CallString);

    BEGIN
    IF VisibleDupeSheetEnable AND NOT SuperDupeSheet THEN Exit;

    SaveSetAndClearActiveWindow (MultiplierInformationWindow);
    ClrScr;
    WriteLn ('Mult needs for ', Call);
    OutputQSOorMultStatusString (GetMultStatus (Call));
    RestorePreviousWindow;
    END;



PROCEDURE EditableLog.CreateModeSpecificMultiplierInfo
     (Call: CallString; Mode: ModeType; VAR OutputString: Str160);

VAR Band: BandType;
    MultString: Str20;

    BEGIN
    IF Mode <> Both THEN
        OutputString := OutputString + ModeString [Mode] + ': ';

    IF MultByBand THEN
         BEGIN
         IF ActiveBand <= Band10 THEN
             Band := Band160
         ELSE
             Band := Band6;
         END
     ELSE
         Band := All;

    WHILE True DO
        BEGIN
        DetermineIfNewMult (Call, Band, Mode, MultString);

        IF MultString <> '' THEN
            BEGIN
            IF Band <= Band10 THEN
                OutputString := OutputString + BandString [Band]
            ELSE
                IF Band = Band6 THEN
                    OutputString := OutputString + '6 '
                ELSE
                    IF Band = Band2 THEN
                        OutputString := OutputString + '2 '
                    ELSE
                        OutputString := OutputString + BandString [Band] + ' ';

            END
        ELSE
            IF Band <= Band10 THEN
                OutputString := OutputString + '   '
            ELSE
                IF (Band = Band6) OR (Band = Band2) THEN
                    OutputString := OutputString + '  '
                ELSE
                    OutputString := OutputString + '    ';

        CASE Band OF
           All:     Exit;
           Band160: Band := Band80;
           Band80:  Band := Band40;
           Band40:  Band := Band20;
           Band20:  Band := Band15;
           Band15:  Band := Band10;
           Band10:  Exit;

           Band6:    Band := Band2;
           Band2:    Band := Band222;
           Band222:  Band := Band432;
           Band432:  Band := Band902;
           Band902:  Band := Band1296;
           Band1296: Band := Band2304;
           Band2304: Band := Band3456;
           Band3456: Band := Band5760;
           Band5760: Band := Band10G;
           Band10G:  Band := Band24G;
           Band24G:  Band := BandLight;
           BandLight: Exit;
           END;
        END;
    END;



FUNCTION EditableLog.GetInitialExchangeFromEditableLog (Call: CallString): Str40;

VAR Entry: INTEGER;
    RData: ContestExchange;

    BEGIN
    GetInitialExchangeFromEditableLog := '';

    IF ExchangeMemoryEnable THEN
        FOR Entry := 5 DOWNTO 1 DO
            IF Call = UpperCase (GetLogEntryCall (VisibleLog.LogEntries [Entry])) THEN
                BEGIN
                IF ParseExchangeIntoContestExchange (VisibleLog.LogEntries [Entry], RData) THEN
                    GetInitialExchangeFromEditableLog := GetInitialExchangeStringFromContestExchange (RData);
                Exit;
                END;
    END;



FUNCTION EditableLog.GetMultStatus (Call: CallString): Str160;

VAR OutputString: Str160;

    BEGIN
    GetMultStatus := '';

    IF NOT (DoingDXMults OR DoingPrefixMults OR DoingZoneMults) THEN Exit;

    IF NOT (DoingDXMults OR DoingPrefixMults) AND
           (ActiveZoneMult = JAPrefectures) THEN
               Exit;

    IF NOT (DoingPrefixMults OR DoingZoneMults) THEN
        IF ActiveDXMult = NoCountDXMults THEN Exit;

    IF (DoingDXMults AND DoingDomesticMults) AND DomesticCountryCall (Call) THEN
        Exit;

    OutputString := '';

    IF MultByMode THEN
        BEGIN
        CreateModeSpecificMultiplierInfo (Call, CW, OutputString);
        OutputString := OutputString + CarriageReturn + LineFeed;
        CreateModeSpecificMultiplierInfo (Call, Phone, OutputString);
        END
    ELSE
        CreateModeSpecificMultiplierInfo (Call, Both, OutputString);

    GetMultStatus := OutputString;
    END;



PROCEDURE EditableLog.CreateModeSpecificQSOInfo
     (Call: CallString; Mode: ModeType; VAR OutputString: Str160);

VAR Band: BandType;
    MultString: Str20;

    BEGIN
    IF Mode <> Both THEN
        OutputString := OutputString + ModeString [Mode] + ': ';

    IF QSOByBand THEN
         BEGIN
         IF ActiveBand <= Band10 THEN
             Band := Band160
         ELSE
             Band := Band6;
         END
     ELSE
         Band := All;

    WHILE True DO
        BEGIN
        IF Sheet.CallIsADupe (Call, Band, Mode) OR CallIsInEditableLog (Call, Band, Mode) THEN
            BEGIN
            IF Band <= Band10 THEN
                OutputString := OutputString + '   '
            ELSE
                IF (Band = Band6) OR (Band = Band2) THEN
                    OutputString := OutputString + '  '
                ELSE
                    OutputString := OutputString + '    ';
            END
        ELSE
            IF Band <= Band10 THEN
                OutputString := OutputString + BandString [Band]
            ELSE
                IF Band = Band6 THEN
                    OutputString := OutputString + '6 '
                ELSE
                    IF Band = Band2 THEN
                        OutputString := OutputString + '2 '
                    ELSE
                        OutputString := OutputString + BandString [Band] + ' ';


        CASE Band OF
           All:     Exit;
           Band160: Band := Band80;
           Band80:  Band := Band40;
           Band40:  Band := Band20;
           Band20:  Band := Band15;
           Band15:  Band := Band10;
           Band10:  Exit;

           Band6:    Band := Band2;
           Band2:    Band := Band222;
           Band222:  Band := Band432;
           Band432:  Band := Band902;
           Band902:  Band := Band1296;
           Band1296: Band := Band2304;
           Band2304: Band := Band3456;
           Band3456: Band := Band5760;
           Band5760: Band := Band10G;
           Band10G:  Band := Band24G;
           Band24G:  Band := BandLight;
           BandLight: Exit;
           END;
        END;
    END;



PROCEDURE EditableLog.ShowQSOStatus (Call: CallString);

{ This procedure will display the QSO status for the call specified.
  It will look at any dupe sheets it can find in memory.              }

VAR OutputString: Str160;

    BEGIN
    IF VisibleDupeSheetEnable AND NOT SuperDupeSheet THEN Exit;
    SaveSetAndClearActiveWindow (QSOInformationWindow);
    ClrScr;
    WriteLn ('QSO needs for ', Call);

    IF NOT (QSOByBand OR QSOByMode) THEN
        BEGIN
        RestorePreviousWindow;
        Exit;
        END;

    IF QSOByMode AND (NOT QSOByBand) AND (NOT MultipleModesEnabled) THEN
        BEGIN
        RestorePreviousWindow;
        Exit;
        END;

    IF QSOByBand AND (NOT QSOByMode) AND (NOT MultipleBandsEnabled) AND
                     (ActiveMultiPort = nil) THEN
        BEGIN
        RestorePreviousWindow;
        Exit;
        END;

    IF QSOByBand AND QSOByMode AND (NOT MultipleModesEnabled) AND (NOT MultipleBandsEnabled)
       AND (ActiveMultiPort = nil) THEN
        BEGIN
        RestorePreviousWindow;
        Exit;
        END;

    OutputString := '';

    IF QSOByMode THEN
        BEGIN
        CreateModeSpecificQSOInfo (Call, CW, OutputString);
        OutputQSOOrMultStatusString (OutputString);
        IF WhereX > 1 THEN WriteLn;

        SetBackground (SelectedColors.QSOInformationWindowColor);
        SetColor      (SelectedColors.QSOInformationWindowBackground);

        OutputString := '';
        CreateModeSpecificQSOInfo (Call, Phone, OutputString);
        END
    ELSE
        CreateModeSpecificQSOInfo (Call, Both, OutputString);

    OutputQSOOrMultStatusString (OutputString);
    RestorePreviousWindow;
    END;



PROCEDURE EditableLog.DoPossibleCalls (Call: CallString);

    BEGIN
    PossibleCallList.NumberPossibleCalls := 0;
    PossibleCallList.CursorPosition      := 0;
    IF NOT PossibleCallEnable THEN Exit;
    CD.GeneratePossibleCallList (Call);
    Sheet.MakePossibleCallList (Call, PossibleCallList);
    FlagDupesInPossibleCallList (ActiveBand, ActiveMode, PossibleCallList);
    DisplayPossibleCalls (PossibleCallList);
    END;



PROCEDURE EditableLog.ShowRemainingMultipliers;

VAR RemainingMults: RemainingMultListPointer;

    MultBand, Band: BandType;
    MultMode, Mode: ModeType;
    Result, Entry, Index: INTEGER;
    MultString, TempString, TestString: Str80;
    Key: CHAR;

    BEGIN
    IF VisibleDupeSheetEnable AND NOT SuperDupeSheet THEN Exit;

    IF RemainingMultDisplay = NoRemMultDisplay THEN Exit;

    IF MaxAvail < SizeOf (RemainingMultList) THEN
        BEGIN
        SaveSetAndClearActiveWindow (RemainingMultsWindow);
        WriteLn ('Insufficient memory to generate remaining');
        WriteLn ('multiplier display.');
        RestorePreviousWindow;
        Exit;
        END;

    IF MultByBand THEN MultBand := ActiveBand ELSE MultBand := All;
    IF MultByMode THEN MultMode := ActiveMode ELSE MultMode := Both;

    New (RemainingMults);

    CASE RemainingMultDisplay OF

        Domestic:
            BEGIN
            IF DomQTHTable.NumberRemainingMults = 0 THEN Exit;

if remmultmatrix[MultBand,MultMode,Domestic] = nil then exit;

            RemainingMults^ := RemMultMatrix [MultBand, MultMode, Domestic]^;

            FOR Entry := 1 TO NumberEditableLines DO
                BEGIN
                Band := GetLogEntryBand (LogEntries [Entry]);
                Mode := GetLogEntryMode (LogEntries [Entry]);

                IF (Band <> NoBand) AND (Mode <> NoMode) THEN
                    BEGIN
                    IF NOT MultByBand THEN Band := All;
                    IF NOT MultByMode THEN Mode := Both;

                    IF (Band = MultBand) AND (Mode = MultMode) THEN
                        BEGIN
                        MultString := Copy (LogEntries [Entry], LogEntryMultAddress, LogentryMultWidth);
                        GetRidOfPrecedingSpaces  (MultString);
                        GetRidOfPostcedingSpaces (MultString);

                        IF MultString <> '' THEN
                            BEGIN
                            IF StringHas (MultString, '/') THEN
                                MultString := ' ' + PrecedingString (MultString, '/') + ' '
                            ELSE
                                MultString := ' ' + MultString + ' ';

                            WHILE MultString <> '' DO
                                BEGIN
                                TempString := RemoveFirstString (MultString);

                                IF StringHasLowerCase (TempString) OR NOT DoingDXMults THEN
                                    BEGIN
                                    Index := DomQTHTable.GetDomMultInteger (TempString);

                                    IF Index >= 0 THEN
                                        RemainingMults^ [Index] := False;
                                    END;
                                END;
                            END;
                        END;
                    END;
                END;
            END;


        DX:
            BEGIN
            IF ActiveDXMult = NoDXMults THEN Exit;
            IF CountryTable.NumberRemainingCountries = 0 THEN Exit;
if remmultmatrix[multband,multmode,DX] = nil then exit;
            RemainingMults^ := RemMultMatrix [MultBand, MultMode, DX]^;

            FOR Entry := 1 TO NumberEditableLines DO
                BEGIN
                Band := GetLogEntryBand (LogEntries [Entry]);
                Mode := GetLogEntryMode (LogEntries [Entry]);

                IF (Band <> NoBand) AND (Mode <> NoMode) THEN
                    BEGIN
                    IF NOT MultByBand THEN Band := All;
                    IF NOT MultByMode THEN Mode := Both;

                    IF (Band = MultBand) AND (Mode = MultMode) THEN
                        BEGIN
                        MultString := Copy (LogEntries [Entry], LogEntryMultAddress, LogentryMultWidth);
                        GetRidOfPostcedingSpaces (MultString);
                        GetRidOfPrecedingSpaces (MultString);

                        IF MultString <> '' THEN
                            WHILE MultString <> '' DO
                                BEGIN
                                TempString := RemoveFirstString (MultString);

                                Index := CountryTable.GetDXMultInteger (TempString);

                                IF Index >= 0 THEN
                                    RemainingMults^ [Index] := False;
                                END;
                        END;
                    END;
                END;
            END;

        Zone:
            BEGIN
            IF MaxNumberOfZones = 0 THEN Exit;

            RemainingMults^ := RemMultMatrix [MultBand, MultMode, Zone]^;

            FOR Entry := 1 TO NumberEditableLines DO
                BEGIN
                Band := GetLogEntryBand (LogEntries [Entry]);
                Mode := GetLogEntryMode (LogEntries [Entry]);

                IF (Band <> NoBand) AND (Mode <> NoMode) THEN
                    BEGIN
                    IF NOT MultByBand THEN Band := All;
                    IF NOT MultByMode THEN Mode := Both;

                    IF (Band = MultBand) AND (Mode = MultMode) THEN
                        BEGIN
                        MultString := Copy (LogEntries [Entry], LogEntryMultAddress, LogentryMultWidth);
                        GetRidOfPostcedingSpaces (MultString);
                        GetRidOfPrecedingSpaces (MultString);

                        IF MultString <> '' THEN
                            WHILE MultString <> '' DO
                                BEGIN
                                TempString := RemoveFirstString (MultString);

                                IF StringIsAllNumbers (TempString) THEN
                                    BEGIN
                                    Val (TempString, Index, Result);

                                    IF ActiveZoneMult <> EuHFCYear THEN
                                        Dec (Index);

                                    IF Index >= 0 THEN
                                        RemainingMults^ [Index] := False;
                                    END;
                                END;
                        END;
                    END;
                END;
            END;

        END;

    DisplayRemainingMults (RemainingMults, RemainingMultDisplay);
    Dispose (RemainingMults);
    END;



PROCEDURE EditableLog.SearchLog (InitialString: Str20);

VAR SearchString, TempString, FileString: Str80;
    FileRead: TEXT;
    DisplayArray: LogEntryArray;
    Line, NumberLinesWritten: INTEGER;
    Key: CHAR;
    TotalEntriesFound: INTEGER;

    BEGIN
    SaveSetAndClearActiveWindow (QuickCommandWindow);
    SearchString := LineInput ('Enter string to search log for : ', InitialString, True, False);
    RemoveAndRestorePreviousWindow;

    IF (SearchString = '') OR (SearchString = EscapeKey) THEN Exit;

    IF OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        GridSquareListShown := False;
        SaveSetAndClearActiveWindow (EditableLogWindow);
        NumberLinesWritten := 0;
        TotalEntriesFound := 0;
        SaveSetAndClearActiveWindow (QuickCommandWindow);
        Write ('Searching for ', SearchString);
        RestorePreviousWindow;
        SearchString := UpperCase (SearchString);

        WHILE (NOT Eof (FileRead)) AND (NOT NewKeyPressed) DO
            BEGIN
            ReadLn (FileRead, FileString);
            FileString := UpperCase (FileString);

            IF StringHas (FileString, SearchString) THEN
                BEGIN
                Inc (TotalEntriesFound);
                IF NumberLinesWritten = 5 THEN
                    BEGIN
                    SaveSetAndClearActiveWindow (QuickCommandWindow);
                    Write ('Press any key to view more...');
                    REPEAT millisleep UNTIL NewKeyPressed;
                    RemoveAndRestorePreviousWindow;
                    ClrScr;
                    SaveSetAndClearActiveWindow (QuickCommandWindow);
                    Write ('Searching for ', SearchString);
                    RestorePreviousWindow;
                    Key := NewReadKey;

                    IF Key = EscapeKey THEN
                        BEGIN
                        Close (FileRead);
                        RestorePreviousWindow;
                        DisplayEditableLog (LogEntries);
                        RemoveWindow (QuickCommandWindow);
                        Exit;
                        END;

                    NumberLinesWritten := 0;
                    END;

                IF WhereX <> 1 THEN WriteLn;
                Write (FileString);
                Inc (NumberLinesWritten);
                END;
            END;
        END;

    Close (FileRead);

    IF NewKeyPressed THEN
        BEGIN
        Key := NewReadKey;
        IF Key = EscapeKey THEN
            BEGIN
            RestorePreviousWindow;
            DisplayEditableLog (LogEntries);
            RemoveWindow (QuickCommandWindow);
            Exit;
            END;
        END;

    IF TotalEntriesFound = 0 THEN
        BEGIN
        Write ('No entries with ', SearchString, ' found.');
        Wait (1500);
        RestorePreviousWindow;
        DisplayEditableLog (LogEntries);
        RemoveWindow (QuickCommandWindow);
        Exit;
        END;

    TempString := QuickEditResponse ('Press any key to clear...', 1);
    RestorePreviousWindow;
    DisplayEditableLog (LogEntries);
    END;




PROCEDURE EditableLog.DeleteLastLogEntry;

VAR Call: CallString;
    Band: BandType;
    Mode: ModeType;

    BEGIN
    IF LastDeletedLogEntry = '' THEN
        BEGIN
        LastDeletedLogEntry := LogEntries [NumberEditableLines];

        IF LastDeletedLogEntry <> '' THEN
            BEGIN
            SaveSetAndClearActiveWindow (QuickCommandWindow);
            Write ('You have deleted the last log entry!!  Use Alt-Y to restore it.');
            RestorePreviousWindow;
            Call := UpperCase (GetLogEntryCall (LogEntries [NumberEditableLines]));
            Band := GetLogEntryBand (LogEntries [NumberEditableLines]);
            Mode := GetLogEntryMode (LogEntries [NumberEditableLines]);
            LogEntries [NumberEditableLines] := '';
            DisplayEditableLog (LogEntries);
            UpdateTempLogFile;
            {KK1L: 6.64 Deleting a contact should reverse the dupe and mult process}
            UpdateBandMapMultiplierStatus;
            UpdateBandMapDupeStatus(Call, Band, Mode, False);
            END;
        END
    ELSE
        BEGIN
        RemoveWindow (QuickCommandWindow);
        LogEntries [NumberEditableLines] := LastDeletedLogEntry;
        DisplayEditableLog (LogEntries);
        LastDeletedLogEntry := '';
        UpdateTempLogFile;
        {KK1L: 6.64 Restoring contact should update the bandmap again}
        Call := UpperCase (GetLogEntryCall (LogEntries [NumberEditableLines]));
        Band := GetLogEntryBand (LogEntries [NumberEditableLines]);
        Mode := GetLogEntryMode (LogEntries [NumberEditableLines]);
        UpdateBandMapMultiplierStatus;
        UpdateBandMapDupeStatus(Call, Band, Mode, True);
        END;
    END;



FUNCTION EditableLog.LastName (Entry: INTEGER): Str20;

{ Revised for use with UK postal groups too in 6.27 }

VAR TempString: Str40;
    EntryString: Str20;

    BEGIN
    TempString := GetLogEntryExchangeString (LogEntries [Entry]);
    RemoveFirstString (TempString);

    CASE ActiveExchange OF
        QSONumberNameDomesticOrDXQTHExchange:
            BEGIN
            LastName := RemoveFirstString (TempString);
            Exit;
            END;

        RSTAndPostalCodeExchange:
            BEGIN
            RemoveFirstString (TempString);
            LastName := RemoveFirstSTring (TempString) + ' ' + RemoveFirstString (TempString);
            Exit;
            END;
        END;

    WHILE TempString <> '' DO
        BEGIN
        EntryString := RemoveFirstString (TempString);

        IF (NOT StringIsAllNumbers (EntryString)) OR (Length (EntryString) >= 4) THEN
            BEGIN
            LastName := EntryString;
            Exit;
            END;
        END;

    TempString := GetLogEntryExchangeString (LogEntries [Entry]);
    RemoveFirstString (TempString);
    LastName := RemoveFirstString (TempString);
    END;


FUNCTION EditableLog.LastCallsign: CallString;

VAR TempString: CallString;

    BEGIN
    TempString := GetLogEntryCall (LogEntries [5]);
    GetRidOfPostcedingSpaces (TempString);
    LastCallsign := TempString;
    END;


PROCEDURE EditableLog.GeneratePartialCallList (InputCall: CallString;
                                               Band: BandType;
                                               Mode: ModeType;
                                               VAR PossCallList: PossibleCallRecord);

VAR Call: CallString;
    Entry: INTEGER;

    BEGIN
    Sheet.MakePartialCallList (InputCall, Band, Mode, PossibleCallList);

    FOR Entry := 1 TO 5 DO
        BEGIN
        Call := GetLogEntryCall (LogEntries [Entry]);

        IF Pos (InputCall, Call) <> 0 THEN
            IF CallNotInPossibleCallList (Call, PossibleCallList) THEN
                BEGIN
                PossCallList.List [PossCallList.NumberPossibleCalls].Call := Call;
                PossCallList.List [PossCallList.NumberPossibleCalls].Dupe := False;
                Inc (PossCallList.NumberPossibleCalls);
                IF PossCallList.NumberPossibleCalls > 12 THEN Break;
                END;
        END;

    FlagDupesInPossibleCallList (Band, Mode, PossCallList);
    END;




FUNCTION EditableLog.NumberNamesSentInEditableLog: INTEGER;

VAR NumberNamesSent, Entry: INTEGER;

    BEGIN
    NumberNamesSent := 0;
    FOR Entry := 1 TO NumberEditableLines DO
        IF Copy (LogEntries [Entry], LogEntryNameSentAddress, 1) = '*' THEN
            Inc (NumberNamesSent);
    NumberNamesSentInEditableLog := NumberNamesSent;
    END;



PROCEDURE DisplaySCPCall (Call: CallString; Radio: RadioType); {KK1L: 6.73 Added Radio to allow SO2R SCP}

    BEGIN
    IF WhereX + Length (Call) >= 78 THEN
        BEGIN
        IF WhereY = 5 THEN
            BEGIN
            SCPScreenFull := True;
            Exit;
            END;

        WriteLn;
        END;

    IF WhereX > 1 THEN Write (' ');

    {IF VisibleLog.CallIsADupe (Call, ActiveBand, ActiveMode) THEN}
    IF VisibleLog.CallIsADupe (Call, BandMemory[Radio], ModeMemory[Radio]) THEN  {KK1L: 6.73 supports SO2R SCP}
        BEGIN
        TextBackground (SCPDupeBackground);
        TextColor (SCPDupeColor);
        Write (Call);
        TextBackground (SelectedColors.EditableLogWindowBackground);
        TextColor (SelectedColors.EditableLogWindowColor);
        END
    ELSE
        Write (Call);

    END;





PROCEDURE EditableLog.SuperCheckPartial (Call: CallString; Automatic: BOOLEAN; Radio: RadioType);
{KK1L: 6.73 Added Radio to allow SO2R SCP}

LABEL NotAPartialCall, RememberNotAPartialCall;

VAR NumberBytes: ARRAY [1..11] OF LONGINT;
    FileOffset, Bytes, MinimumBytes: LONGINT;
    X, Y, CharPos, QuestionPos1, QuestionPos2, CallAddress, Result: INTEGER;
    NumberAnticipatedCalls: INTEGER;

    FileChar: CHAR;
    FileCall: CallString;
    FileRead: FILE;

    BEGIN
    IF CD.SCPDisabledByApplication THEN Exit;
    IF Call = LastSCPCall THEN Exit;

    SCPScreenFull := False;

    IF Call = '' THEN
        BEGIN
        IF VisibleDupeSheetEnable AND NOT SuperDupesheet THEN
            {DisplayVisibleDupesheet (ActiveBand, ActiveMode)}
            DisplayVisibleDupesheet (BandMemory[Radio], ModeMemory[Radio]) {KK1L: 6.73 suppory SO2R SCP}
        ELSE
            IF NOT EditableLogDisplayed THEN
                DisplayEditableLog (LogEntries);

        LastSCPCall := '';
        Exit;
        END;

    IF Automatic THEN
        BEGIN
        IF Length (Call) < SCPMinimumLetters THEN
            BEGIN
            LastSCPCall := '';
            Exit;
            END
        END
    ELSE
        IF Length (Call) < 2 THEN
            BEGIN
            LastSCPCall := '';
            Exit;
            END;

    LastSCPCall := Call;

    IF NOT CD.PartialCallSetup (Call) THEN Exit;

    EditableLogDisplayed := False;
    SaveSetAndClearActiveWindow (EditableLogWindow);

    GridSquareListShown := False;

    REPEAT
        Call := CD.GetNextPartialCall;
        IF Call <> '' THEN
            DisplaySCPCall (Call, Radio); {KK1L: 6.73 Fixed proc to for SO2R}

        IF SCPScreenFull THEN Break;

        IF NewKeyPressed THEN
            BEGIN
            LastSCPCall := '';
            Break;
            END;

    UNTIL (Call = '') OR NewKeyPressed;

    RestorePreviousWindow;
    END;



PROCEDURE EditableLog.ShowMissingMultiplierReport;

VAR Country: INTEGER;
    Entry, Row, Column, NumberCountriesDisplayed, Hits, NumberMults: INTEGER;
    StartBand, EndBand, Band: BandType;
    Mode: ModeType;
    MultString, TempString, ID: Str20;
    CompressedMult: FourBytes;
    BandHits: ARRAY [BandType] OF BOOLEAN;
    Key: CHAR;

    BEGIN
    IF ActiveDXMult = NoDXMults THEN Exit;
    IF NOT MultByBand THEN Exit;

    VisibleDupeSheetRemoved := True;

    SaveSetAndClearActiveWindow (BigWindow);
    ClrScr;

    TempString := '';
    NumberCountriesDisplayed := 0;

    FOR Country := 0 TO CountryTable.NumberCountries - 1 DO
        BEGIN
        Hits := 0;
        ID := CountryTable.GetCountryID (Country);
        CompressFormat (ID, CompressedMult);


        IF ActiveBand <= Band10 THEN
            BEGIN
            StartBand := Band160;
            EndBand   := Band10;
            END
        ELSE
            IF ActiveBand >= Band6 THEN
                BEGIN
                StartBand := Band6;
                EndBand   := BandLight;
                END;

        FOR Band := StartBand TO EndBand DO
            BEGIN
            BandHits [Band] := False;

            IF MultByMode THEN Mode := ActiveMode ELSE Mode := Both;

            NumberMults := Sheet.MultSheet.Totals [Band, Mode].NumberDXMults;

            IF (NumberMults > 0) AND BytDupe (Addr (CompressedMult), NumberMults, Sheet.MultSheet.DXList [Band, Mode]) THEN
                BEGIN
                Inc (Hits);
                BandHits [Band] := True;
                END
            ELSE
                BEGIN
                FOR Entry := 1 TO NumberEditableLines DO
                    IF GetLogEntryBand (LogEntries [Entry]) = Band THEN
                        IF (Mode = Both) OR (Mode = GetLogEntryMode (LogEntries [Entry])) THEN
                            BEGIN
                            MultString := Copy (LogEntries [Entry], LogEntryMultAddress, LogentryMultWidth);
                            GetRidOfPrecedingSpaces (MultString);
                            MultString := ' ' + MultString + ' ';

                            IF StringHas (MultString, ' ' + ID + ' ') THEN
                                BEGIN
                                Inc (Hits);
                                BandHits [Band] := True;
                                END;
                            END;
                END;
            END;

        IF (Hits >= MultReportMinimumBands) AND (Hits < 6) AND (NumberCountriesDisplayed < 80) THEN
            BEGIN
            Row := (NumberCountriesDisplayed MOD 16) + 1;
            Column := (((NumberCountriesDisplayed DIV 16)) * 15) + 1;
            GoToXY (Column, Row);

            TempString := ID;

            WHILE Length (TempString) < 6 DO
                TempString := ' '+ TempString;

            Write (TempString, ' ');

            FOR Band := Band160 TO Band10 DO
                IF BandHits [Band] THEN
                    Write ('*')
                ELSE
                    Write ('.');

            Inc (NumberCountriesDisplayed);
            END;
        END;

    WriteLn;

    GoToXY (8, 17);
    Write ('Countries on at least ', MultReportMinimumBands, ' but not all bands.  Any key to exit.');
    RestorePreviousWindow;

    REPEAT millisleep UNTIL NewKeyPressed;
    Key := NewReadKey;
    IF Key = NullKey THEN Key := NewReadKey;
    END;



FUNCTION QuickEditResponseWithPartials (Prompt: Str80;
                                        MaxInputLength: INTEGER): Str80;

VAR InputString: Str80;
    Key: Char;

    BEGIN
    SaveSetAndClearActiveWindow (QuickCommandWindow);

    Tone.DoABeep (PromptBeep);

    Write (Prompt);

    {InputString := '';}
    {KK1L: 6.73 Set to DupeInfoCall for ALT-D use. As of 6.73 ALT-D is the only time this is used.}

    IF AltDBufferEnable THEN
        BEGIN
        IF DupeInfoCall <> '' THEN
            InputString := DupeInfoCall
        ELSE
            InputString := DupeInfoCallPrompt;

        Write (InputString); {KK1L: 6.73 Display existing call (from band map) if there.}
        END
    ELSE
        InputString := '';

    REPEAT
        REPEAT millisleep UNTIL NewKeyPressed;

        Key := UpCase (NewReadKey);

        CASE Key OF
            EscapeKey:
                IF InputString = '' THEN
                    BEGIN
                    QuickEditResponseWithPartials := EscapeKey;
                    RemoveAndRestorePreviousWindow;
                    Exit;
                    END
                ELSE
                    BEGIN
                    GoToXY (WhereX - Length (InputString), WhereY);
                    ClrEol;
                    InputString := '';
                    END;

            BackSpace:
                BEGIN
                IF Length (InputString) > 0 THEN
                    BEGIN
                    InputString [0] := Chr (Length (InputString) - 1);
                    GoToXY (WhereX - 1, WhereY);
                    ClrEol;

                    IF PartialCallEnable THEN
                        IF Sheet.TwoLetterCrunchProcess (InputString) THEN
                            BEGIN
                            VisibleLog.GeneratePartialCallList (InputString,
                                                                ActiveBand,
                                                                ActiveMode,
                                                                PossibleCallList);
                            DisplayPossibleCalls (PossibleCallList);
                            END;

                    IF SCPMinimumLetters > 0 THEN  {KK1L: 6.73 Adds SCP to ALT-D entry}
                        VisibleLog.SuperCheckPartial (InputString, True, InactiveRadio); {KK1L: 6.73 Added InactiveRadio}

                    END;
                END;

            CarriageReturn:
                BEGIN
                QuickEditResponseWithPartials := InputString;
                RemoveAndRestorePreviousWindow;
                Exit;
                END;

            NullKey:
                BEGIN
                END;

            ELSE
                IF ((Key >= '0') AND (Key <= '9')) OR
                   ((Key >= 'A') AND (Key <= 'Z')) OR
                   (Key = '/') THEN
                       BEGIN
                       {KK1L: 6.73 Overwrites band map callsign.}
                       IF (InputString = DupeInfoCallPrompt) OR (InputString = DupeInfoCallPrompt) THEN
                           BEGIN
                           GoToXY (WhereX - Length (InputString), WhereY);
                           ClrEol;
                           InputString := '';
                           END;
                       Write (Key);
                       InputString := InputString + Key;

                       IF PartialCallEnable THEN
                           IF Sheet.TwoLetterCrunchProcess (InputString) THEN
                               BEGIN
                               VisibleLog.GeneratePartialCallList (InputString,
                                                                   ActiveBand,
                                                                   ActiveMode,
                                                                   PossibleCallList);
                               DisplayPossibleCalls (PossibleCallList);
                               END;

                       IF SCPMinimumLetters > 0 THEN  {KK1L: 6.73 Adds SCP to ALT-D entry}
                           VisibleLog.SuperCheckPartial (InputString, True, InactiveRadio); {KK1L: 6.73 Added InactiveRadio}

                       END;


            IF Length (InputString) = MaxInputLength THEN
                BEGIN
                QuickEditResponseWithPartials := InputString;
                RemoveAndRestorePreviousWindow;
                Exit;
                END;

            END;  { of case }

    UNTIL FALSE;
    END;



PROCEDURE TimeAndDateSet;

VAR Hour, Minute, Second, Sec100, Year, Month, Day, DayOfWeek: Word;
    TempString, SecondTempString, HourTempString, MinuteTempString: Str80;
    CharPointer, Result: INTEGER;
    Hours, Mins, SecS, YearS, MonS, DayS: Str20;

    BEGIN
    LastDisplayedTime := '';
    LastDisplayedDate := '';

    SaveSetAndClearActiveWindow (QuickCommandWindow);

    REPEAT
        ClrScr;

        Tone.DoABeep (PromptBeep);
        IF HourOffset = 0 THEN
            TempString := LineInput ('Enter time (HH:MM:SS) : ', '', False, False)
        ELSE
            TempString := LineInput ('Enter time (use local time, not UTC) HH:MM:SS : ', '', False, False);

        {KK1L: 6.64 Changed to IF to accomodate option of updating time to the network even if no time entered.}
        {      Saves having to enter time, day, etc if you only want to update a new computer on the network.}

        IF TempString = '' THEN
            BEGIN
            IF ActiveMultiPort <> nil THEN
                BEGIN
                IF UpperCase (QuickEditResponse ('Do you want to send time to computers on the network? (Y/N) : ', 1))='Y' THEN
                    BEGIN
                    GetTime (Hour, Minute, Second, Sec100);
                    GetDate (Year, Month, Day, DayOfWeek);

                    Str (Hour,   HourS);
                    Str (Minute, MinS);
                    Str (Second, SecS);
                    Str (Year,   YearS);
                    Str (Month,  MonS);
                    Str (Day,    DayS);

                    SendMultiCommand (MultiBandAddressArray [ActiveBand], $FF,
                                      MultiTimeMessage, YearS + ' ' + MonS  + ' ' +
                                                        DayS  + ' ' + HourS + ' ' +
                                                        MinS  + ' ' + SecS);
                    END;
                END;
            RemoveAndRestorePreviousWindow;
            Exit;
            END
        ELSE IF TempString = EscapeKey THEN
            BEGIN
            RemoveAndRestorePreviousWindow;
            Exit;
            END;

    UNTIL TempString [3] = ':';

    HourTempString := '';

    CharPointer := 1;

    HourTempString := PrecedingString (TempString, ':');

    Delete (TempString, 1, Pos (':', TempString));

    IF StringHas (TempString, ':') THEN
        BEGIN
        MinuteTempString := PrecedingString (TempString, ':');
        SecondTempString := PostcedingString (TempString, ':');
        END
    ELSE
        BEGIN
        MinuteTempString := TempString;
        SecondTempString := '00';
        END;

    IF (NOT StringIsAllNumbers (HourTempString)) OR
       (NOT StringIsAllNumbers (MinuteTempString)) OR
       (NOT StringIsAllNumbers (SecondTempString)) THEN
           BEGIN
           RemoveAndRestorePreviousWindow;
           Exit;
           END;

    Val (HourTempString,     Hour, Result);
    Val (MinuteTempString, Minute, Result);
    Val (SecondTempString, Second, Result);

    SetTime (Hour, Minute, Second, 0);

    REPEAT
        ClrScr;

        GetDate (Year, Month, Day, DayOfWeek);
        Str (Year, TempString);

        Tone.DoABeep (PromptBeep);
        TempString := LineInput ('Enter year (ESCAPE to skip) : ', TempString, True, False);

        IF (TempString = '') OR (TempString = EscapeKey) THEN
            BEGIN
            RemoveAndRestorePreviousWindow;
            Exit;
            END;

        Val (TempString, Year, Result);

    UNTIL (Year > 1900) AND (Year < 2100);

    REPEAT
        ClrScr;

        Str (Month, TempString);
        Tone.DoABeep (PromptBeep);

        TempString := LineInput ('Enter month (1-12) : ', TempString, True, False);

        IF (TempString = '') OR (TempString = EscapeKey) THEN
            BEGIN
            RemoveAndRestorePreviousWindow;
            Exit;
            END;

        Val (TempString, Month, Result);

    UNTIL (Month >= 1) AND (Month <= 12);

    REPEAT
        ClrScr;

        Str (Day, TempString);

        TempString := LineInput ('Enter day (1-31) : ', TempString, True, False);
        Tone.DoABeep (PromptBeep);

        IF (TempString = '') OR (TempString = EscapeKey) THEN
            BEGIN
            RemoveAndRestorePreviousWindow;
            Exit;
            END;

        Val (TempString, Day, Result);

    UNTIL (Day >= 1) AND (Day <= 31);

    SetDate (Year, Month, Day);

    IF ActiveMultiPort <> nil THEN
        BEGIN
        IF UpperCase (QuickEditResponse ('Do you want to send time to computers on the network? (Y/N) : ', 1)) = 'Y' THEN
            BEGIN
            GetTime (Hour, Minute, Second, Sec100);
            GetDate (Year, Month, Day, DayOfWeek);

            Str (Hour,   HourS);
            Str (Minute, MinS);
            Str (Second, SecS);
            Str (Year,   YearS);
            Str (Month,  MonS);
            Str (Day,    DayS);

            SendMultiCommand (MultiBandAddressArray [ActiveBand], $FF,
                              MultiTimeMessage, YearS + ' ' + MonS  + ' ' +
                                                DayS  + ' ' + HourS + ' ' +
                                                MinS  + ' ' + SecS);
            END;
        END;

    RemoveAndRestorePreviousWindow;
    UpdateTimeAndRateDisplays (True, False);
    END;



PROCEDURE EditInit;

    BEGIN
    PossibleCallList.NumberPossibleCalls := 0;
    LastDeletedLogEntry := '';
    END;



PROCEDURE ShowStationInformation (Call: CallString);

VAR Name: Str80;

    BEGIN
    { Took next line out in 6.27.  What if you are working the same guy
      on a different band? }

    { IF Call = StationInformationCall THEN Exit; }

    StationInformationCall := Call;

    IF Copy (Call, 1, 3) = 'CQ-' THEN Exit;

    ShowName (Call);

    IF QTCsEnabled THEN
        DisplayQTCNumber (NumberQTCsThisStation (StandardCallFormat (Call, False)));

    IF ContestName <> 'General QSOs' THEN
        BEGIN
        VisibleLog.ShowMultiplierStatus (Call);
        VisibleLog.ShowQSOStatus        (Call);
        END;

    IF ActiveDomesticMult <> GridSquares THEN
        BEGIN
        DisplayBeamHeading (Call);
        DisplayCountryName (Call);
        END;

    DisplayUserInfo (Call);

    IF CountryInformationFile <> '' THEN
        DisplayCountryInformation (CountryInformationFile, Call);
    END;



FUNCTION DetermineQTCNumberAndQuanity (InputString: Str80;
                                       VAR QTCNumber: INTEGER;
                                       VAR Quantity: INTEGER): BOOLEAN;

VAR Result: INTEGER;
    QTCNumberString, NumberMessagesString: Str80;

    BEGIN
    DetermineQTCNumberAndQuanity := False;
    IF InputString = '' THEN Exit;
    QTCNumberString      := PrecedingString  (InputString, '/');
    NumberMessagesString := PostcedingString (InputString, '/');

    IF (QTCNumberString = '') OR (NumberMessagesString = '') THEN
        BEGIN
        Tone.DoABeep (Single);
        QuickDisplay ('QTC process terminated.');
        Exit;
        END;

    Val (QTCNumberString, QTCNumber, Result);

    IF Result <> 0 THEN
        BEGIN
        Tone.DoABeep (Warning);
        QuickDisplay ('QTC Number has illegal characters.');
        Exit;
        END;

    Val (NumberMessagesString, Quantity, Result);

    IF Result <> 0 THEN
        BEGIN
        Tone.DoABeep (Warning);
        QuickDisplay ('Number of messages has illegal characters.');
        Exit;
        END;

    IF (Quantity < 1) OR (Quantity > 10) THEN
        BEGIN
        Tone.DoABeep (Warning);
        QuickDisplay ('QTC process terminated.  Invalid number of messages.');
        Exit;
        END;

    DetermineQTCNumberAndQuanity := True;
    END;



PROCEDURE AddQTCToQTCBuffer (VAR QTCBuffer: LogEntryArray; QTCString: Str80; Message: INTEGER);

{ This procedure will add the indicated string to the proper message
  number of the QTCBuffer specified.  It will also display it in the
  proper place of the EditableLogWindow.                     }

    BEGIN
    GridSquareListShown := False;

    SaveAndSetActiveWindow (EditableLogWindow);

    CASE Message OF
        1: BEGIN
           QTCBuffer [1] := ' 1: ' + QTCString;
           GoToXY (1, 1);
           ClrEol;
           Write (QTCBuffer [1]);
           END;

        2: BEGIN
           QTCBuffer [2] := ' 2: ' + QTCString;
           GoToXY (1, 2);
           ClrEol;
           Write (QTCBuffer [2]);
           END;

        3: BEGIN
           QTCBuffer [3] := ' 3: ' + QTCString;
           GoToXY (1, 3);
           ClrEol;
           Write (QTCBuffer [3]);
           END;

        4: BEGIN
           QTCBuffer [4] := ' 4: ' + QTCString;
           GoToXY (1, 4);
           ClrEol;
           Write (QTCBuffer [4]);
           END;

        5: BEGIN
           QTCBuffer [5] := ' 5: ' + QTCString;
           GoToXY (1, 5);
           ClrEol;
           Write (QTCBuffer [5]);
           END;

        6: BEGIN
           WHILE Length (QTCBuffer [1]) < 40 DO
           QTCBuffer [1] := QTCBuffer [1] + ' ';
           QTCBuffer [1] := QTCBuffer [1] + ' 6: ' + QTCString;
           GoToXY (1, 1);
           ClrEol;
           Write (QTCBuffer [1]);
           END;

        7: BEGIN
           WHILE Length (QTCBuffer [2]) < 40 DO
           QTCBuffer [2] := QTCBuffer [2] + ' ';
           QTCBuffer [2] := QTCBuffer [2] + ' 7: ' + QTCString;
           GoToXY (1, 2);
           ClrEol;
           Write (QTCBuffer [2]);
           END;

        8: BEGIN
           WHILE Length (QTCBuffer [3]) < 40 DO
           QTCBuffer [3] := QTCBuffer [3] + ' ';
           QTCBuffer [3] := QTCBuffer [3] + ' 8: ' + QTCString;
           GoToXY (1, 3);
           ClrEol;
           Write (QTCBuffer [3]);
           END;

        9: BEGIN
           WHILE Length (QTCBuffer [4]) < 40 DO
           QTCBuffer [4] := QTCBuffer [4] + ' ';
           QTCBuffer [4] := QTCBuffer [4] + ' 9: ' + QTCString;
           GoToXY (1, 4);
           ClrEol;
           Write (QTCBuffer [4]);
           END;

       10: BEGIN
           WHILE Length (QTCBuffer [5]) < 40 DO
           QTCBuffer [5] := QTCBuffer [5] + ' ';
           QTCBuffer [5] := QTCBuffer [5] + '10: ' + QTCString;
           GoToXY (1, 5);
           ClrEol;
           Write (QTCBuffer [5]);
           END;

        END;
    RestorePreviousWindow;
    END;



FUNCTION InitialExchangeEntry (Call: CallString): Str80;

{ This function will give you an initial exchange window entry if it
  thinks it knows what the guy will send.                            }

VAR Heading, CharPosition, Distance, Zone, Result: INTEGER;
    TempQTH: QTHRecord;
    TestSTring, StandardCall, Command: Str20;
    Exchange, CustomString, TempString: Str80;
    TempExchange: ContestExchange;
    Data: DatabaseEntryRecord;

    BEGIN
    {TR6.74 - Removed any manipulation of IntialExchangePutUp from this
     routine.  It is up to whomever calls it to decide if they want to
     change the state of that variable. }

    { InitialExchangePutUp := False; }

    IF NOT GoodCallSyntax (Call) THEN
        BEGIN
        InitialExchangeEntry := '';
        Exit;
        END;

    TempString := VisibleLog.GetInitialExchangeFromEditableLog (Call);

    IF TempString = '' THEN TempString := GetInitialExchange (Call);

    IF TempString = '' THEN
        BEGIN
        StandardCall := StandardCallFormat (Call, True);

        CASE ActiveInitialExchange OF

            CustomInitialExchange:
                BEGIN
                Exchange := '';
                CustomString := CustomInitialExchangeString;

                WHILE CustomString <> '' DO
                    BEGIN
                    Command := RemoveFirstString (CustomString);

                    IF Command = 'CQZONE' THEN
                        IF CD.GetEntry (Call, Data) AND (Data.CQZone <> '') THEN
                            Exchange := Exchange + Data.CQZone + ' '
                        ELSE
                            BEGIN
                            Zone := CountryTable.GetCQZone (Call);

                            IF Zone > 0 THEN
                                BEGIN
                                Str (Zone, TempString);
                                Exchange := Exchange + TempString + ' ';
                                END;
                            END;

                    IF Command = 'ITUZONE' THEN
                        IF CD.GetEntry (Call, Data) AND (Data.ITUZone <> '') THEN
                            Exchange := Exchange + Data.ITUZone + ' '
                        ELSE
                            BEGIN
                            Zone := CountryTable.GetITUZone (Call);

                            IF Zone > 0 THEN
                                BEGIN
                                Str (Zone, TempString);
                                Exchange := Exchange + TempString + ' ';
                                END;
                            END;

                    IF Command = 'NAME' THEN
                        IF CD.GetEntry (StandardCall, Data) AND (Data.Name <> '') THEN
                            Exchange := Exchange + Data.Name + ' ';

                    IF Command = 'QTH' THEN
                        IF CD.GetEntry (Call, Data) AND (Data.QTH <> '') THEN
                            Exchange := Exchange + Data.QTH + ' ';

                    IF Command = 'SECTION' THEN
                        IF CD.GetEntry (Call, Data) AND (Data.Section <> '') THEN
                            Exchange := Exchange + Data.Section + ' ';

                    IF Command = 'USER1' THEN
                        IF CD.GetEntry (Call, Data) AND (Data.User1 <> '') THEN
                            Exchange := Exchange + Data.User1 + ' ';

                    IF Command = 'USER2' THEN
                        IF CD.GetEntry (Call, Data) AND (Data.User2 <> '') THEN
                            Exchange := Exchange + Data.User2 + ' ';

                    IF Command = 'USER3' THEN
                        IF CD.GetEntry (Call, Data) AND (Data.User3 <> '') THEN
                            Exchange := Exchange + Data.User3 + ' ';

                    IF Command = 'USER4' THEN
                        IF CD.GetEntry (Call, Data) AND (Data.User4 <> '') THEN
                            Exchange := Exchange + Data.User4 + ' ';

                    IF Command = 'USER5' THEN
                        IF CD.GetEntry (Call, Data) AND (Data.User5 <> '') THEN
                            Exchange := Exchange + Data.User5 + ' ';

                    IF Command = 'GRID' THEN
                        IF CD.GetEntry (Call, Data) AND (Data.Grid <> '') THEN
                            BEGIN
                            Exchange := Exchange + Data.Grid + ' ';

                            IF (MyGrid <> '') AND LooksLikeAGrid (Data.Grid) THEN
                                BEGIN
                                SaveSetAndClearActiveWindow (BeamHeadingWindow);
                                Heading := Round (GetBeamHeading (MyGrid, Data.Grid));
                                Write (Data.Grid, ' at ', Heading, DegreeSymbol);
                                RestorePreviousWindow;
                                END;
                            END;

                    IF Command = 'FOC' THEN
                        IF CD.GetEntry (StandardCall, Data) AND (Data.FOC <> '') THEN
                            Exchange := Exchange + Data.FOC + ' ';

                    IF Command = 'CHECK' THEN
                        IF CD.GetEntry (Call, Data) AND (Data.Check <> '') THEN
                            Exchange := Exchange + Data.Check + ' ';

                    IF Command = 'OLDCALL' THEN
                        IF CD.GetEntry (Call, Data) AND (Data.OldCall <> '') THEN
                            Exchange := Exchange + Data.OldCall + ' ';

                    IF Command = 'TENTEN' THEN
                        IF CD.GetEntry (Call, Data) AND (Data.TenTen <> '') THEN
                            Exchange := Exchange + Data.TenTen + ' ';

                    END;

                InitialExchangeEntry := ' ' + Exchange; {KK1L: 6.73 Added ' '. K9PG forgets to add it when cursor at start.}

{               IF InitialExchangeOverwrite THEN
                    InitialExchangePutUp := True; {KK1L: 6.70 For custom typing any character overwrites the whole exchange.}

                Exit;
                END;


            ZoneInitialExchange:
                BEGIN
                IF CD.GetEntry (Call, Data) THEN
                    BEGIN
                    IF ActiveZoneMult = ITUZones THEN
                        BEGIN
                        IF Data.ITUZone <> '' THEN
                            BEGIN
                            InitialExchangeEntry := ' ' + Data.ITUZone; {KK1L: 6.73 Added ' '. K9PG forgets to add it.}

                            { InitialExchangePutUp := True; }

                            Exit;
                            END;
                        END
                    ELSE
                        IF Data.CQZone <> '' THEN
                            BEGIN
                            InitialExchangeEntry := ' ' + Data.CQZone; {KK1L: 6.73 Added ' '. K9PG forgets to add it.}

                            { InitialExchangePutUp := True; }
                            Exit;
                            END;
                    END;

                LocateCall (Call, TempQTH, True);
                IF TempQTH.Zone > 0 THEN Str (TempQTH.Zone, TempString);
                GetRidOfPrecedingSpaces (TempString);
                IF Debug AND (TempString = '') THEN TempString := '40';

                { InitialExchangePutUp := TempString <> ''; }
                END;

            NameInitialExchange:
                BEGIN
                TempString := CD.GetName (StandardCall);
                IF TempString <> '' THEN TempString := TempString + ' ';
                END;

            NameQTHInitialExchange:
                IF CD.GetEntry (Call, Data) THEN
                    BEGIN
                    IF Data.Name <> '' THEN TempString := Data.Name + ' ';
                    IF Data.QTH  <> '' THEN TempString := TempString + Data.QTH + ' ';
                    END;

            CheckSectionInitialExchange:
                IF CD.GetEntry (Call, Data) THEN
                    IF (Data.Check   <> '') AND (Data.Section <> '') THEN
                        TempString := Data.Check + Data.Section + ' ';

            SectionInitialExchange:
                IF CD.GetEntry (Call, Data) THEN
                    BEGIN
                    IF Data.Section <> '' THEN
                        TempString := Data.Section + ' '
                    ELSE
                        TempString := GetVEInitialExchange (Call);
                    END
                ELSE
                    TempString := GetVEInitialExchange (Call);


            QTHInitialExchange:
                IF CD.GetEntry (Call, Data) THEN
                    BEGIN
                    IF Data.QTH <> '' THEN
                        TempString := Data.QTH + ' '
                    ELSE
                        TempString := GetVEInitialExchange (Call);
                    END
                ELSE
                    TempString := GetVEInitialExchange (Call);

            GridInitialExchange:
                IF CD.GetEntry (Call, Data) THEN
                    BEGIN
                    TempString := Data.Grid;

                    IF (MyGrid <> '') AND LooksLikeAGrid (Data.Grid) THEN
                        BEGIN
                        SaveSetAndClearActiveWindow (BeamHeadingWindow);
                        Heading := Round (GetBeamHeading (MyGrid, Data.Grid));
                        Write (Data.Grid, ' at ', Heading, DegreeSymbol);
                        RestorePreviousWindow;
                        END;
                    END;

            FOCInitialExchange:
                IF CD.GetEntry (StandardCall, Data) THEN
                    TempString := Data.Foc;

            User1InitialExchange:
                IF CD.GetEntry (Call, Data) THEN
                    TempString := Data.User1;

            User2InitialExchange:
                IF CD.GetEntry (Call, Data) THEN
                    TempString := Data.User2;

            User3InitialExchange:
                IF CD.GetEntry (Call, Data) THEN
                    TempString := Data.User3;

            User4InitialExchange:
                IF CD.GetEntry (Call, Data) THEN
                    TempString := Data.User4;

            User5InitialExchange:
                IF CD.GetEntry (Call, Data) THEN
                    TempString := Data.User5;

            END; { if case }

        InitialExchangeEntry := ' ' + TempString; {KK1L: 6.73 Added ' '. K9PG forgets to add it when cursor at start.}

{        IF InitialExchangeOverwrite THEN
            InitialExchangePutUp := True; {KK1L: 6.73 Typing any character overwrites the whole exchange.}

        END
    ELSE
        BEGIN
        GetRidOfPostcedingSpaces (TempString);

        IF (ActiveExchange = QSONumberDomesticOrDXQTHExchange)     OR
           (ActiveExchange = QSONumberDomesticQTHExchange)         OR
           (ActiveExchange = RSTAndQSONumberOrDomesticQTHExchange) OR
           (ActiveExchange = RSTDomesticOrDXQTHExchange)           OR
           (ActiveExchange = RSTDomesticQTHExchange)               OR
           (ActiveExchange = RSTDomesticQTHOrQSONumberExchange)    OR
           (ActiveExchange = RSTQSONumberAndDomesticQTHExchange)   OR
           (ActiveExchange = QSONumberAndPossibleDomesticQTHExchange) OR {KK1L: 6.73}
           (ActiveExchange = RSTQSONumberAndPossibleDomesticQTHExchange) THEN
               FOR CharPosition := 1 TO Length (TempString) DO
                   IF Copy (TempString, CharPosition, 1) = ' ' THEN
                       TempString [CharPosition] := '/';

        {KK1L: 6.68 For IARU there was a problem with intial exchanges read from the log.}
        {           Both the zone and country are log entries, but only the zone is in the}
        {           exchange. This confused the parser. Only the first string (zone) is needed.}

        IF (ActiveInitialExchange = ZoneInitialExchange) OR
           ((DomesticQTHDataFileName = 'IARUHQ.DOM')) THEN
            {KK1L: 6.71 The init exchange bug came back when I changed the Initital exchange default}
            {           for IARU and WRTC!! Added the second line above.}
            TempString := RemoveFirstString (TempString);

        {KK1L: 6.73 Typing any character overwrites the whole exchange. Tree
         decided this was a good thing even for rover calls and moved it here }

{       IF InitialExchangeOverwrite THEN InitialExchangePutUp := True;  }

        IF NOT RoverCall (Call) THEN
            BEGIN
            InitialExchangeEntry := ' ' + TempString + ' '; {KK1L: 6.73 Added ' '. K9PG forgets to add it.}
            END
        ELSE
            BEGIN
            InitialExchangeEntry := TempString; {KK1L: 6.73 Per Tree to fix VHF rover problem.}

{           InitialExchangePutUp := True; }
            END;

        END;

    { In 6.40 - made this work all of the time instead of only with
      above }

    WHILE TempString <> '' DO
        BEGIN
        { was FirstString before 6.43 }

        TestString := RemoveLastString (TempString);

        IF (MyGrid <> '') AND LooksLikeAGrid (TestString) THEN
            BEGIN
            SaveSetAndClearActiveWindow (BeamHeadingWindow);
            Heading := Round (GetBeamHeading (MyGrid, TestString));
            Write (TestString, ' ', Heading, DegreeSymbol);

            IF DistanceMode <> NoDistanceDisplay THEN
                BEGIN
                Distance := GetDistanceBetweenGrids (MyGrid, TestString);

                IF DistanceMode = DistanceMiles THEN
                    BEGIN
                    Distance := Round (Distance / 1.6);
                    Write (' ', Distance, 'm');
                    END
                ELSE
                    Write (' ', Distance, 'km');
                END;

            RestorePreviousWindow;
            END;

        IF DoingDomesticMults THEN
            IF NOT StringIsAllNumbersOrSpaces (TestString) THEN
                BEGIN
                TempExchange.QTHString := TestString;

                IF FoundDomesticQTH (TempExchange) THEN
                    BEGIN
                    VisibleLog.ShowDomesticMultiplierStatus (TempExchange.DomesticQTH);
                    Exit;
                    END;
                END
            ELSE
                IF ActiveExchange = RSTPrefectureExchange THEN
                    BEGIN
                    TempExchange.QTHString := 'p' + RemoveFirstString (TempString);

                    IF FoundDomesticQTH (TempExchange) THEN
                        VisibleLog.ShowDomesticMultiplierStatus (TempExchange.DomesticQTH);
                    Exit;
                    END;
        END;
    END;



PROCEDURE UpdateBandMapMultiplierStatus;
{KK1L: 6.64 Made the procedure create a mult as well if the need be. When called from}
{      Alt-Y it is possible mults are recreated. The original routine checked if}
{      the entry was a mult before checking to see if it was no longer a mult.}
{      Logic could be added to only fully process the whole band map on a call}
{      from "EditableLog.DeleteLastLogEntry"}

VAR BandMapEntryRecord: BandMapEntryPointer;
    Mode, CheckMode: ModeType;
    Band, CheckBand: BandType;
    MultString: Str20;

    BEGIN

    FOR Band := Band160 TO Band2 DO
        FOR Mode := CW TO Phone DO
            BEGIN
            IF MultByBand THEN CheckBand := Band ELSE CheckBand := All;
            IF MultByMode THEN CheckMode := Mode ELSE CheckMode := Both;

            BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

            WHIlE BandMapEntryRecord <> nil DO
                BEGIN
                WITH BandMapEntryRecord^ DO
                        BEGIN
                        VisibleLog.DetermineIfNewMult (BigExpandedString (Call), Band, Mode, MultString);

                        StatusByte := StatusByte AND $7F;

                        IF MultString <> '' THEN
                            StatusByte := StatusByte OR $80
                        END;

                BandMapEntryRecord  := BandMapEntryRecord^.NextEntry;
                END;
            END;

    DisplayBandMap;
    END;


PROCEDURE UpdateBandMapDupeStatus(RXCall: CallString; RXBand: BandType; RXMode: ModeType; MakeDupe: BOOLEAN);


VAR BandMapEntryRecord: BandMapEntryPointer;
    ChangeMade: BOOLEAN;
    CallString: Str20;
    CharPointer: INTEGER;
    TempStr: Str80;

    BEGIN
    ChangeMade := False;

    BandMapEntryRecord := BandMapFirstEntryList [RXBand, RXMode];
    WHILE BandMapEntryRecord <> nil DO
        BEGIN
        WITH BandMapEntryRecord^ DO
            BEGIN

            {IF (RXCall = BandMapExpandedString(Call)) AND           } {KK1L: 6.73 Removed}
            {   (RXBand = ActiveBand) AND (RXMode = ActiveMode) THEN }
            {KK1L: 6.73 Don't limit compare to active band/mode. This keeps a contact made on the}
            {           second radio from getting reset correctly. The BandMapFirstEntryList limits}
            {           the check appropriately to the band/mode of the deleted contact.}
            IF (RXCall = BandMapExpandedString(Call)) THEN
              IF MakeDupe THEN
                BEGIN
                StatusByte := StatusByte OR $40;   {KK1L: 6.64 Turn on dupe bit}
                StatusByte := StatusByte AND $7F; {KK1L: 6.69 if it is a dupe it CAN'T be a mult}
                ChangeMade := True;
                END
              ELSE
                BEGIN
                StatusByte := StatusByte AND $BF; {KK1L: 6.64 Turn off dupe bit}
                ChangeMade := True;
                END;
            END;
        BandMapEntryRecord  := BandMapEntryRecord^.NextEntry;
        END;

    IF ChangeMade THEN DisplayBandMap;
    END;



    BEGIN
    EditInit;
    END.
