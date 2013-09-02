UNIT PostSubs;

{$F+}
{$O+}
{$V-}

INTERFACE

USES trCrt,
     Tree,             {  444 bytes }
     SlowTree,
     LogDupe,          { 3516 bytes }
     LogWind,          { 4842 bytes }
     Country9,         { 1454 bytes }
     ZoneCont,         {    4 bytes }
     LogDom,           {  298 bytes }
     FContest;         {    0 bytes }

{$I versions.inc} // version constant moved here for easy update

CONST

//  PostVersion = '6.76 native linux port';
//    PostVersion = 'Linux 0.02';

    MaxLogFilesOpen = 24;

    TempFileName = 'TR$TR$TR.TMP';

    CallWidth = 12;
    MaximumCalls = 65536 DIV (CallWidth + 1) - 1;

    Columns = 6;
    PrinterColumns = 80;

    QSLLabelBlockSize = 500;
    MaxNumberBlocks = 10;
    MaxQSLLabelAddress = QSLLabelBlockSize * MaxNumberBlocks;

    QSLTempFileName = 'POSTQSL.TMP';

    DoubleStrike = Chr (0);
    NormalStrike = Chr (0);
    Expand = Chr (0);
    Norm = Chr (0);

    ArrayIndex = CallWidth + 1;
    MaximumListCalls = 65536 DIV ArrayIndex - 1;

TYPE
    CallArray = ARRAY [0..MaximumCalls] OF STRING [CallWidth];
    CallArrayPtr = ^CallArray;

    CallPointerArray = ARRAY [0..MaximumCalls] OF INTEGER;
    CallPointerArrayPtr = ^CallPointerArray;

    DataArrayEntry = RECORD
        LogEntry: Str80;
        Mult:     STRING [6];
        END;

    WordArrayType = ARRAY [BandType, CW..Both] OF WORD;
    WordArrayPtr = ^WordArrayType;

    ByteArrayType = ARRAY [BandType, CW..Both] OF BYTE;
    ByteArrayPtr = ^ByteArrayType;

CONST
    MaximumLogEntries = 65536 DIV SizeOf (DataArrayEntry) - 1;

TYPE
    DataArray = ARRAY [0..MaximumLogEntries] OF DataArrayEntry;
    DataArrayPtr = ^DataArray;

    SuffixString = Str80;



    ContactRecord = RECORD
        Band: BandType;
        Mode: ModeType;
        Date: String [9];
        Time: String [5];
        RST:  String [3];
        END;

    QSLLabelRecord = RECORD
        CallSign: CallString;
        NumberContacts: INTEGER;
        ContactArray: ARRAY [1..3] OF ContactRecord;
        END;

    QSLLabelBlock = ARRAY [0..QSLLabelBlockSize - 1] OF QSLLabelRecord;
    QSLLabelBlockPointer = ^QSLLabelBlock;

    QSLDataRecord = RECORD
        NumberLabels: INTEGER;
        Blocks: ARRAY [1..MaxNumberBlocks] OF QSLLabelBlockPointer;
        END;

    CountryTotalArrayType = ARRAY [0..400] OF INTEGER;

    PointerArrayType = ARRAY [0..MaxQSLLabelAddress] OF INTEGER;
    PointerArrayPointer = ^PointerArrayType;

    RateSheetType = ARRAY [1..3, 0..23, BandType, CW..Both] OF INTEGER;
    RateSheetPointer = ^RateSheetType;

    ReportEntryArray = ARRAY [0..500] OF Str20;
    ReportEntryArrayPointer = ^ReportEntryArray;

    OpenLogRecord = RECORD
        Band: BandType;
        Mode: ModeType;
        END;

VAR AccumulateQSOPoints:   BOOLEAN;
    AccumulateMultipliers: BOOLEAN;

    Block:      INTEGER;
    PrintBreak: INTEGER;

    Call:          CallString;
    CallLists:     ARRAY [1..10] OF CallArrayPtr;
    CheckForDupes: BOOLEAN;
    Contest:       Str80;
    Continued:     BOOLEAN;
    CWEnable:      BOOLEAN;

    DupeLogs:       BOOLEAN;
    DupeByBand:     BOOLEAN;
    DupeByMode:     BOOLEAN;
    DupeSheetTitle: Str80;

    EndOfOtherCallList: INTEGER;
    FirstCallAddress:    INTEGER;

    FileWrite0, FileWrite1, FileWrite2, FileWrite3, FileWrite4: TEXT;
    FileWrite5, FileWrite6, FileWrite7, FileWrite8, FileWrite9: TEXT;

    FileWrite10, FileWrite11, FileWrite12, FileWrite13, FileWrite14: TEXT;
    FileWrite15, FileWrite16, FileWrite17, FileWrite18, FileWrite19: TEXT;
    FileWrite20, FileWrite21, FileWrite22, FileWrite23: TEXT;

    Header: ARRAY [0..9] OF Str80;

    Key: CHAR;

    LastCallAddress:    INTEGER;
    LastPushedLogName:  Str20;
    LastNewList:        INTEGER;
    Lines:              INTEGER;
    LogFileOpen:        ARRAY [0..MaxLogFilesOpen] OF OpenLogRecord;
    LogFileQSOPoints:   WordArrayPtr;
    LogFileQSOsPrinted: WordArrayPtr;
    LogTotals:          QSOTotalArray;
    LogTotalsAccurate:  BOOLEAN;

    MultByBandAndModeDetermined: BOOLEAN;
    MultiplierArray:    DataArrayPtr;


    NCalls:                 INTEGER;
    NLines:                 INTEGER;
    NumberBufferCalls:      INTEGER;
    NumberContactsPerLabel: BYTE;
    NumberCountries :       INTEGER;
    NumberDifferentMults:   INTEGER;
    NumberDupes:            INTEGER;
    NumberHeaderLines:      INTEGER;
    NumberLines:            INTEGER;
    NumberLogFilesOpen:     WORD;
    NumberOtherCalls :      INTEGER;

    NumberPageDomesticMults: ByteArrayPtr;
    NumberPageDXMults:       ByteArrayPtr;
    NumberPagePrefixMults:   ByteArrayPtr;
    NumberPageZoneMults:     ByteArrayPtr;
    NumberUSACalls:          INTEGER;

    Offset:               INTEGER;
    OtherCallList:        CallArrayPtr;
    OtherCallPointerList: CallPointerArrayPtr;
    Over:                 BOOLEAN;

    PageQSOPoints:       WordArrayPtr;
    PageValidContacts:   ByteArrayPtr;
    PointerArray:        PointerArrayPointer;
    PrintingRestOfCalls: BOOLEAN;

    QSLData:                    QSLDataRecord;
    QSOByBandAndModeDetermined: BOOLEAN;
    QSOsPerPage:                INTEGER;

    RateSheet:          RateSheetPointer;

    ReNumberQSONumbers: BOOLEAN;
    ReportEntries:      ReportEntryArrayPointer;
    Result:             INTEGER;

    SeparateBandLogs:    BOOLEAN;
    SeparateModeLogs:    BOOLEAN;
    Sheet:               DupeAndMultSheet;            { 13,638 bytes }
    Spaces:              Str40;
    StartBand, StopBand: BandType;
    StartMode, StopMode: ModeType;

    Temp:       INTEGER;
    TempKey:    CHAR;
    TempString: Str80;
    Title:      Str80;
    TotalCalls: INTEGER;

    TotalDomesticMults: ByteArrayPtr;
    TotalDXMults:       ByteArrayPtr;
    TotalPrefixMults:   WordArrayPtr;
    TotalZoneMults:     ByteArrayPtr;

    USACallList:        CallArrayPtr;
    USACallPointerList: CallPointerArrayPtr;

    ValidContacts: WordArrayPtr;


PROCEDURE BlankOutMultsAndZeroQSOPoints (VAR LogString: Str80; InsertString: Str40);

PROCEDURE ChangeLogStringDateTime (VAR LogString: STRING; Offset: INTEGER);
FUNCTION  CheckForTempFile: BOOLEAN;
PROCEDURE CloseAllOpenFiles;

PROCEDURE DetermineLogTotals;
FUNCTION  DetermineMultiplierTypes: BOOLEAN;
PROCEDURE DetermineMultByBandAndMultByMode;
PROCEDURE DetermineQSOByBandAndQSOByMode;
PROCEDURE DisposeCallBuffer;
FUNCTION  DupingFileName (Band: BandType; Mode: ModeType): Str80;
FUNCTION  DupingFileTitle (Band: BandType; Mode: ModeType): Str80;

FUNCTION  GetCall (CallAddress: INTEGER): CallString;

FUNCTION  GetContestExchangeFromLogEntryExchange (LogEntry: STRING;
                                                  VAR RXData: ContestExchange): BOOLEAN;

FUNCTION  GetContestExchangeFromMultiplierString (LogEntry: Str80;
                                                  VAR RXData: ContestExchange): BOOLEAN;
PROCEDURE OpenFilesForBandsBeingProcessed;
FUNCTION  OpenLogFile (Band: BandType; Mode: ModeType): BOOLEAN;

PROCEDURE PushLogFiles;

PROCEDURE PutCall (CallAddress: INTEGER; Call: CallString);
FUNCTION  PutLogFileIntoCallBuffer (Mode: ModeType; Band: BandType): BOOLEAN;

FUNCTION  SetUpPostParametersFromLOGCFGFile: BOOLEAN;

PROCEDURE ViewLog;
PROCEDURE WriteToLogFile (LogEntry: Str80; Band: BandType; Mode: ModeType);

IMPLEMENTATION

uses memlinux,keycode;

{$I PostCfg}  { Has the function ProcessPostConfigInstruction }



{ ***************** Callsign Array Support Routines **************** }

FUNCTION GetCall (CallAddress: INTEGER): CallString;

{ The function will retrieve a callsign from the address specified.  }
{ It computes which array to find the call in along with the proper  }
{ address within the array.  An error message is printed if the      }
{ address is out of range.                                           }

VAR List: INTEGER;

    BEGIN
    List :=        CallAddress DIV MaximumListCalls + 1;
    CallAddress := CallAddress MOD MaximumListCalls;
    GetCall :=     CallLists [List]^ [CallAddress];
    END;


PROCEDURE PutCall (CallAddress: INTEGER; Call: CallString);

{ This procedure is used to put a call into the calllist.  The address }
{ of the call is used to compute which array and array address to use. }
{ An error message is printed if the address to out of range. }

VAR List: INTEGER;

    BEGIN
    List := (CallAddress DIV MaximumListCalls) + 1;
    CallAddress := CallAddress MOD MaximumListCalls;

    IF List > LastNewList THEN
        IF MaxAvail > SizeOf (CallArray) THEN
            BEGIN
            New (CallLists [List]);
            LastNewList := List;
            END
        ELSE
            BEGIN
            ReportError ('Not enough memory to add Call List!!');
            WaitForKeyPressed;
            Halt;
            END;

    CallLists [List]^ [CallAddress] := Call;
    END;

PROCEDURE DisposeCallBuffer;

VAR List: INTEGER;

    BEGIN
    IF LastNewList > 0 THEN
        BEGIN
        FOR List := 1 TO LastNewList DO
            Dispose (CallLists [List]);
        LastNewList := 0;
        END;

    LastNewList := 0;
    NumberBufferCalls := 0;
    END;



FUNCTION DupingFileName (Band: BandType; Mode: ModeType): Str80;

VAR TempString: Str20;

    BEGIN
    TempString := ModeString [Mode] + BandString [Band];

    WHILE Pos (' ', TempString) > 0 DO
        Delete (TempString, Pos (' ', TempString), 1);

    DupingFileName := 'L' + TempString;
    END;



FUNCTION DupingFileTitle (Band: BandType; Mode: ModeType): Str80;

VAR TempString: Str80;

    BEGIN
    TempString := BandString [Band] + ModeString [Mode];

    IF StringHas (TempString, 'BTH') OR StringHas (TempString, 'ALL') THEN
        BEGIN
        Delete (TempString, Length (TempString) - 2, 3);
        TempString := TempString + ' Meters';
        END;

    DupingFileTitle := Title + '  ' + TempString;
    END;



FUNCTION GetLogFileName (Band: BandType; Mode: ModeType): Str20;

    BEGIN
    IF SeparateBandLogs THEN
        IF SeparateModeLogs THEN
            BEGIN
            IF Mode = CW THEN
                CASE Band OF
                    Band160:   GetLogFileName := 'L160CW.DAT';
                    Band80:    GetLogFileName := 'L80CW.DAT';
                    Band40:    GetLogFileName := 'L40CW.DAT';
                    Band20:    GetLogFileName := 'L20CW.DAT';
                    Band15:    GetLogFileName := 'L15CW.DAT';
                    Band10:    GetLogFileName := 'L10CW.DAT';
                    Band30:    GetLogFileName := 'L30CW.DAT';
                    Band17:    GetLogFileName := 'L17CW.DAT';
                    Band12:    GetLogFileName := 'L12CW.DAT';
                    Band6:     GetLogFileName := 'L6CW.DAT';
                    Band2:     GetLogFileName := 'L2CW.DAT';
                    Band222:   GetLogFileName := 'L222CW.DAT';
                    Band432:   GetLogFileName := 'L432CW.DAT';
                    Band902:   GetLogFileName := 'L902CW.DAT';
                    Band1296:  GetLogFileName := 'L1GHCW.DAT';
                    Band2304:  GetLogFileName := 'L2GHCW.DAT';
                    Band3456:  GetLogFileName := 'L3GHCW.DAT';
                    Band5760:  GetLogFileName := 'L5GHCW.DAT';
                    Band10G:   GetLogFileName := 'L10GW.DAT';
                    Band24G:   GetLogFileName := 'L24GW.DAT';
                    BandLight: GetLogFileName := 'LLHTCW.DAT';
                    END
            ELSE
                CASE Band OF
                    Band160:   GetLogFileName := 'L160SSB.DAT';
                    Band80:    GetLogFileName := 'L80SSB.DAT';
                    Band40:    GetLogFileName := 'L40SSB.DAT';
                    Band20:    GetLogFileName := 'L20SSB.DAT';
                    Band15:    GetLogFileName := 'L15SSB.DAT';
                    Band10:    GetLogFileName := 'L10SSB.DAT';
                    Band30:    GetLogFileName := 'L30SSB.DAT';
                    Band17:    GetLogFileName := 'L17SSB.DAT';
                    Band12:    GetLogFileName := 'L12SSB.DAT';
                    Band6:     GetLogFileName := 'L6SSB.DAT';
                    Band2:     GetLogFileName := 'L2SSB.DAT';
                    Band222:   GetLogFileName := 'L222SSB.DAT';
                    Band432:   GetLogFileName := 'L432SSB.DAT';
                    Band902:   GetLogFileName := 'L902SSB.DAT';
                    Band1296:  GetLogFileName := 'L1GHSSB.DAT';
                    Band2304:  GetLogFileName := 'L2GHSSB.DAT';
                    Band3456:  GetLogFileName := 'L3GHSSB.DAT';
                    Band5760:  GetLogFileName := 'L5GHSSB.DAT';
                    Band10G:   GetLogFileName := 'L10GW.DAT';
                    Band24G:   GetLogFileName := 'L24GW.DAT';
                    BandLight: GetLogFileName := 'LLHTSSB.DAT';
                    END;
            END
        ELSE
            BEGIN
            CASE Band OF
                Band160:   GetLogFileName := 'LOG160.DAT';
                Band80:    GetLogFileName := 'LOG80.DAT';
                Band40:    GetLogFileName := 'LOG40.DAT';
                Band20:    GetLogFileName := 'LOG20.DAT';
                Band15:    GetLogFileName := 'LOG15.DAT';
                Band10:    GetLogFileName := 'LOG10.DAT';
                Band30:    GetLogFileName := 'LOG30.DAT';
                Band17:    GetLogFileName := 'LOG17.DAT';
                Band12:    GetLogFileName := 'LOG12.DAT';
                Band6:     GetLogFileName := 'LOG6.DAT';
                Band2:     GetLogFileName := 'LOG2.DAT';
                Band222:   GetLogFileName := 'LOG222.DAT';
                Band432:   GetLogFileName := 'LOG432.DAT';
                Band902:   GetLogFileName := 'LOG902.DAT';
                Band1296:  GetLogFileName := 'LOG1GH.DAT';
                Band2304:  GetLogFileName := 'LOG2GH.DAT';
                Band3456:  GetLogFileName := 'LOG3GH.DAT';
                Band5760:  GetLogFileName := 'LOG5GH.DAT';
                Band10G:   GetLogFileName := 'LOG10GW.DAT';
                Band24G:   GetLogFileName := 'LOG24GW.DAT';
                BandLight: GetLogFileName := 'LOGLHT.DAT';
                END;
            END
    ELSE
        IF SeparateModeLogs THEN
            CASE Mode OF
                CW:    GetLogFileName := 'LOGCW.DAT';
                Phone: GetLogFileName := 'LOGSSB.DAT';
                END
        ELSE
            GetLogFileName := 'LALLBOTH.DAT';
    END;



FUNCTION FindOpenFile (Band: BandType; Mode: ModeType): INTEGER;

{ Returns -1 if open file for that band/mode is not found }

VAR LogFile: INTEGER;

    BEGIN
    FindOpenFile := -1;

    IF NumberLogFilesOpen > 0 THEN
        FOR LogFile := 0 TO NumberLogFilesOpen - 1 DO
            IF (LogFileOpen [LogFile].Band = Band) AND
               (LogFileOpen [LogFile].Mode = Mode) THEN
                   BEGIN
                   FindOpenFile := LogFile;
                   Exit;
                   END;

    END;



FUNCTION OpenLogFile (Band: BandType; Mode: ModeType): BOOLEAN;

VAR FileName: Str20;

    BEGIN
    IF NumberLogFilesOpen >= MaxLogFilesOpen THEN
        BEGIN
        OpenLogFile := False;
        Exit;
        END;

    LogFileOpen [NumberLogFilesOpen].Band := Band;
    LogFileOpen [NumberLogFilesOpen].Mode := Mode;

    FileName := GetLogFileName (Band, Mode);

    CASE NumberLogFilesOpen OF
        0: OpenFileForWrite (FileWrite0,  FileName);
        1: OpenFileForWrite (FileWrite1,  FileName);
        2: OpenFileForWrite (FileWrite2,  FileName);
        3: OpenFileForWrite (FileWrite3,  FileName);
        4: OpenFileForWrite (FileWrite4,  FileName);
        5: OpenFileForWrite (FileWrite5,  FileName);
        6: OpenFileForWrite (FileWrite6,  FileName);
        7: OpenFileForWrite (FileWrite7,  FileName);
        8: OpenFileForWrite (FileWrite8,  FileName);
        9: OpenFileForWrite (FileWrite9,  FileName);
       10: OpenFileForWrite (FileWrite10, FileName);
       11: OpenFileForWrite (FileWrite11, FileName);
       12: OpenFileForWrite (FileWrite12, FileName);
       13: OpenFileForWrite (FileWrite13, FileName);
       14: OpenFileForWrite (FileWrite14, FileName);
       15: OpenFileForWrite (FileWrite15, FileName);
       16: OpenFileForWrite (FileWrite16, FileName);
       17: OpenFileForWrite (FileWrite17, FileName);
       18: OpenFileForWrite (FileWrite18, FileName);
       19: OpenFileForWrite (FileWrite19, FileName);
       20: OpenFileForWrite (FileWrite20, FileName);
       21: OpenFileForWrite (FileWrite21, FileName);
       22: OpenFileForWrite (FileWrite22, FileName);
       23: OpenFileForWrite (FileWrite23, FileName);

       ELSE
           BEGIN
           ReportError ('K7RAT error!! - report to N6TR.');
           Halt;
           END;
       END;

    Inc (NumberLogFilesOpen);
    END;



PROCEDURE WriteToLogFile (LogEntry: Str80; Band: BandType; Mode: ModeType);

VAR LogFile: INTEGER;

    BEGIN
    CASE FindOpenFile (Band, Mode) OF

       -1: Exit; { File not open }

        0: WriteLn (FileWrite0,  LogEntry);
        1: WriteLn (FileWrite1,  LogEntry);
        2: WriteLn (FileWrite2,  LogEntry);
        3: WriteLn (FileWrite3,  LogEntry);
        4: WriteLn (FileWrite4,  LogEntry);
        5: WriteLn (FileWrite5,  LogEntry);
        6: WriteLn (FileWrite6,  LogEntry);
        7: WriteLn (FileWrite7,  LogEntry);
        8: WriteLn (FileWrite8,  LogEntry);
        9: WriteLn (FileWrite9,  LogEntry);
       10: WriteLn (FileWrite10, LogEntry);
       11: WriteLn (FileWrite11, LogEntry);
       12: WriteLn (FileWrite12, LogEntry);
       13: WriteLn (FileWrite13, LogEntry);
       14: WriteLn (FileWrite14, LogEntry);
       15: WriteLn (FileWrite15, LogEntry);
       16: WriteLn (FileWrite16, LogEntry);
       17: WriteLn (FileWrite17, LogEntry);
       18: WriteLn (FileWrite18, LogEntry);
       19: WriteLn (FileWrite19, LogEntry);
       20: WriteLn (FileWrite20, LogEntry);
       21: WriteLn (FileWrite21, LogEntry);
       22: WriteLn (FileWrite22, LogEntry);
       23: WriteLn (FileWrite23, LogEntry);

       ELSE
           BEGIN
           ReportError ('WA6TUT error!! - report to N6TR');
           Halt;
           END;

       END;
   END;



PROCEDURE CloseAllOpenFiles;

VAR LogFile: INTEGER;

    BEGIN
    IF NumberLogFilesOpen > 0 THEN
        FOR LogFile := 0 TO NumberLogFilesOpen - 1 DO
            CASE LogFile OF
                0: Close (FileWrite0);
                1: Close (FileWrite1);
                2: Close (FileWrite2);
                3: Close (FileWrite3);
                4: Close (FileWrite4);
                5: Close (FileWrite5);
                6: Close (FileWrite6);
                7: Close (FileWrite7);
                8: Close (FileWrite8);
                9: Close (FileWrite9);
               10: Close (FileWrite10);
               11: Close (FileWrite11);
               12: Close (FileWrite12);
               13: Close (FileWrite13);
               14: Close (FileWrite14);
               15: Close (FileWrite15);
               16: Close (FileWrite16);
               17: Close (FileWrite17);
               18: Close (FileWrite18);
               19: Close (FileWrite19);
               20: Close (FileWrite20);
               21: Close (FileWrite21);
               22: Close (FileWrite22);
               23: Close (FileWrite23);
               END;

    END;



PROCEDURE DetermineLogTotals;

VAR Band: BandType;

    BEGIN
    IF LogTotalsAccurate THEN Exit;
    TextColor (Cyan);
    Write ('Examining log for QSO totals...');
    Sheet.ExamineLogForQSOTotals (LogTotals);
    GoToXY (1, WhereY);
    ClrEol;
    LogTotalsAccurate := True;
    END;



PROCEDURE DetermineQSOByBandAndQSOByMode;

VAR Key: CHAR;

    BEGIN
    QSOByBandAndModeDetermined := False;
    REPEAT
        Key := UpCase (GetKey ('Can you work stations again on each mode? (Y/N or ESCAPE to abort) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'Y') OR (Key = 'N');
    WriteLn;
    QSOByMode := Key = 'Y';

    REPEAT
        Key := UpCase (GetKey ('Can you work stations again on each band? (Y/N or ESCAPE to abort) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'Y') OR (Key = 'N');
    WriteLn;
    QSOByBand := Key = 'Y';

    IF QSOByBand THEN BEGIN StartBand := Band160; StopBand := Band10; END
                 ELSE BEGIN StartBand := All;     StopBand := All;    END;

    IF QSOByMode THEN BEGIN StartMode := CW;   StopMode  := Phone; END
                 ELSE BEGIN StartMode := Both; StopMode  := Both;  END;

    QSOByBandAndModeDetermined := True;
    END;



PROCEDURE DetermineMultByBandAndMultByMode;

VAR Key: CHAR;

    BEGIN
    QSOByBandAndModeDetermined := False;
    REPEAT
        Key := UpCase (GetKey ('Do multipliers count again on each mode? (Y/N or ESCAPE to abort) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'Y') OR (Key = 'N');
    WriteLn;
    MultByMode := Key = 'Y';

    REPEAT
        Key := UpCase (GetKey ('Do multipliers count again on each band? (Y/N or ESCAPE to abort) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'Y') OR (Key = 'N');
    WriteLn;
    MultByBand := Key = 'Y';

    IF MultByBand THEN BEGIN StartBand := Band160; StopBand := Band10; END
                  ELSE BEGIN StartBand := All;     StopBand := All;    END;

    IF MultByMode THEN BEGIN StartMode := CW;   StopMode  := Phone; END
                  ELSE BEGIN StartMode := Both; StopMode  := Both;  END;

    MultByBandAndModeDetermined := True;
    END;



PROCEDURE OpenFilesForBandsBeingProcessed;

VAR Band: BandType;
    Mode: ModeType;

    BEGIN
    NumberLogFilesOpen := 0;

    FOR Band := StartBand TO StopBand DO
        FOR Mode := StartMode TO StopMode DO
            IF LogTotals [Band, Mode] > 0 THEN
                OpenLogFile (Band, Mode);
    END;


PROCEDURE BlankOutMultsAndZeroQSOPoints (VAR LogString: Str80; InsertString: Str40);

    BEGIN
    Delete (LogString, LogEntryMultAddress, 8);

    WHILE Length (InsertString) < 8 DO InsertString := InsertString + ' ';
    Insert (InsertString, LogString, LogEntryMultAddress);

    Delete (LogString, LogEntryPointsAddress, LogEntryPointsWidth);
    Insert (' 0', LogString, LogEntryPointsAddress);
    END;



FUNCTION PutLogFileIntoCallBuffer (Mode: ModeType; Band: BandType): BOOLEAN;

VAR FileRead: TEXT;
    LogBand:  BandType;
    LogMode:  ModeType;
    Call:     CallString;
    List:     INTEGER;

    BEGIN
    TextColor (Cyan);
    GoToXY (1, WhereY);
    ClrEol;
    Write ('Loading calls from log for ', BandString [Band], ModeString [Mode], '...');

    DisposeCallBuffer;

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        PutLogFileIntoCallBuffer := False;
        Exit;
        END;

    WHILE NOT Eof (FileRead) DO
        BEGIN
        ReadLn (FileRead, TempString);
        LogMode := GetLogEntryMode (TempString);

        IF LogMode <> NoMode THEN
            IF ((LogMode = Mode) OR (Mode = Both)) THEN
                IF NOT (StringHas (TempString, 'DUPE') OR StringHas (TempString, 'ZERO')) THEN
                    IF (Band = All) OR (GetLogEntryBand (TempString) = Band) THEN
                        BEGIN
                        ExpandTabs (TempString);
                        Call := Copy (TempString, LogEntryCallAddress, 12);
                        GetRidOfPostcedingSpaces (Call);
                        Call := StandardCallFormat (Call, False);
                        PutCall (NumberBufferCalls, Call);
                        Inc (NumberBufferCalls);
                        END;
        END;

    Close (FileRead);
    PutLogFileIntoCallBuffer := True;
    GoToXY (1, WhereY);
    ClrEol;
    END;



FUNCTION DetermineMultiplierTypes: BOOLEAN;

VAR Key: CHAR;

    BEGIN
    DetermineMultiplierTypes := False;
    REPEAT
        Key := UpCase (GetKey ('Domestic mults? (Y/N) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'Y') OR (Key = 'N');
    WriteLn;
    DoingDomesticMults := Key = 'Y';

    REPEAT
        Key := UpCase (GetKey ('DX Country mults? (Y/N) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'Y') OR (Key = 'N');
    WriteLn;
    DoingDXMults := Key = 'Y';

    REPEAT
        Key := UpCase (GetKey ('Prefix mults? (Y/N) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'Y') OR (Key = 'N');
    WriteLn;
    DoingPrefixMults := Key = 'Y';

    REPEAT
        Key := UpCase (GetKey ('Zone mults? (Y/N) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'Y') OR (Key = 'N');
    WriteLn;
    DoingZoneMults := Key = 'Y';
    DetermineMultiplierTypes := True;
    END;



PROCEDURE ViewLog;

VAR Call, FileString: Str80;
    SearchStringFound: BOOLEAN;
    Line, Count, NumberLinesPrinted, NumberLinesInBuffer: INTEGER;
    AddressOfEntryFoundInBuffer: INTEGER;
    Buffer: ARRAY [0..25] OF Str80;
    FileRead: TEXT;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('VIEW LOG SEGMENT PROCEDURE');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('This procedure allows you to search for something in the log and look at the');
    WriteLn ('first log entry that contains the search string.  It also shows you the next');
    WriteLn ('screen full of log entries occuring afterwards.  You can then input a new');
    WriteLn ('search string and the program will continue forward looking for the new search');
    WriteLn ('string.  This allows you to find a QSO that occured at 06:05 on the second day.');
    WriteLn ('You would first search for the proper date, then search for 06:.  The search');
    WriteLn ('is case insensitive.  If you want to continue after seeing 20 QSOs with the');
    WriteLn ('next 20 QSOs, use a space as your search string.');
    WriteLn;

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError ('Unable to open ' + LogFileName);
        WaitForKeyPressed;
        Exit;
        END;

    Call := UpperCase (GetResponse ('Enter string to search for (none to stop) : '));
    IF Call = '' THEN Exit;

    NumberLinesInBuffer := 0;

    WHILE (NOT Eof (FileRead)) AND (NOT OperatorEscape) DO
        BEGIN
        NumberLinesPrinted := 0;
        SearchStringFound := False;

        IF (NumberLinesInBuffer > 0) AND (Call <> ' ') THEN
            FOR Line := 0 TO NumberLinesInBuffer - 1 DO
                IF StringHas (Buffer [Line], Call) THEN
                    BEGIN
                    SearchStringFound := True;
                    ClrScr;
                    WriteLn (Buffer [Line]);
                    Inc (NumberLinesPrinted);
                    AddressOfEntryFoundInBuffer := Line;

                    IF Line = (NumberLinesInBuffer - 1) THEN
                        NumberLinesInBuffer := 0
                    ELSE
                        Line := NumberLinesInBuffer - 1;
                    END;

        IF NOT SearchStringFound THEN
            BEGIN
            NumberLinesInBuffer := 0;
            ReadLn (FileRead, FileString);

            IF StringHas (UpperCase (FileString), Call) THEN
                BEGIN
                ClrScr;
                ExpandTabs (FileString);
                WriteLn (FileString);
                Inc (NumberLinesPrinted);
                SearchStringFound := True;
                END;
            END;

        IF SearchStringFound THEN
            BEGIN
            IF NumberLinesInBuffer > 0 THEN
                BEGIN
                FOR Count := AddressOfEntryFoundInBuffer + 1 TO NumberLinesInBuffer - 1 DO
                    BEGIN
                    WriteLn (Buffer [Count]);
                    Buffer [Count - AddressOfEntryFoundInBuffer + 1] := Buffer [Count];
                    Inc (NumberLinesPrinted);
                    END;

                NumberLinesInBuffer := NumberLinesInBuffer - AddressOfEntryFoundInBuffer - 1;
                END;

            WHILE (NumberLinesPrinted < 21) AND NOT Eof (FileRead) DO
                BEGIN
                ReadLn (FileRead, FileString);
                ExpandTabs (FileString);
                WriteLn (FileString);
                Buffer [NumberLinesInBuffer] := FileString;
                Inc (NumberLinesInBuffer);
                Inc (NumberLinesPrinted);
                END;

            Call := UpperCase (GetResponse ('Enter string to search for (none to stop) : '));

            IF Call = '' THEN
                BEGIN
                Close (FileRead);
                Exit;
                END;
            END;
        END;

    TextColor (Cyan);
    WriteLn ('End of file...');
    Close (FileRead);
    WaitForKeyPressed;
    END;



FUNCTION CheckForTempFile: BOOLEAN;

{ Returns FALSE if the procedure should exit. }

VAR Key: CHAR;
    FileRead, FileWrite: TEXT;
    FileString: Str160;
    TempFileName: Str20;

    BEGIN
    CheckForTempFile := False;

    IF LogFileName = 'LOG.DAT' THEN
        TempFileName := 'LOG.TMP'
    ELSE
        TempFileName := PrecedingString (LogFileName, '.') + '.TMP';

    If FileExists (TempFileName) AND (GetFileSize (TempFileName) > 20) THEN
        BEGIN
        WriteLn ('The temp file has been found.  This means the Alt-U command has not been ');
        WriteLn ('executed in the logging program.  You can exit this program and execute the');
        WriteLn ('Alt-U command or I can append it to the log file for you.  If I do it for');
        WriteLn ('you, there is some chance that the page format will be wrong.  This is not a');
        WriteLn ('problem if you are planning on creating a new log sheet with running totals.');
        WriteLn;

        REPEAT
            Key := UpCase (GetKey ('Type A to Append temp file to ' + LogFileName + ' or ESCAPE to exit : '));
            IF Key = EscapeKey THEN Exit;
        UNTIL Key = 'A';

        IF NOT OpenFileForRead   (FileRead,  TempFileName)   THEN Exit;
        IF NOT OpenFileForAppend (FileWrite, LogFileName) THEN Exit;

        WHILE NOT Eof (FileRead) DO
            BEGIN
            ReadLn  (FileRead,  FileString);
            WriteLn (FileWrite, FileString);
            END;

        Close (FileRead);
        Close (FileWrite);
        DeleteFile (TempFileName);
        GoToXY (1, WhereY);
        TextColor (Cyan);
        WriteLn (TempFileName, ' has been appended to ', LogFileName, '.');
        WriteLn ('Also, ', TempFileName, ' has been erased so you won''t be tempted to add another 5 QSOs');
        WriteLn ('to your log later on.');
        END;

    CheckForTempFile := True;
    END;



FUNCTION SetUpPostParametersFromLOGCFGFile: BOOLEAN;


{ This routine will read in the LOGCFG.DAT file found and set up all the
  global variables that might be needed to accurately check multipliers,
  generate summary sheets, or any of the other functions likely to be
  executed during the POST process.  }

VAR FileRead: TEXT;
    FileString: STRING;
    LogConfigFileName, ID, CMD: Str40;

    BEGIN
    ActiveExchange := UnknownExchange;

    SetUpPostParametersFromLOGCFGFile := False;

    ClearDomesticCountryList;

    IF LogFileName = 'LOG.DAT' THEN
        LogConfigFileName := 'LOGCFG.DAT'
    ELSE
        LogConfigFileName := PrecedingString (LogFileName, '.') + '.CFG';

    IF NOT OpenFileForRead (FileRead, LogConfigFileName) THEN
        BEGIN
        ReportError ('Unable to open ' + LogConfigFileName + ' file!!  Make sure you are in the proper directory.');
        Exit;
        END;

    WHILE NOT Eof (FileRead) DO
        BEGIN
        ReadLn (FileRead, FileString);
        FileString := UpperCase (FileString);

        ID  := PrecedingString  (FileString, '=');
        CMD := PostcedingString (FileString, '=');

        GetRidOfPrecedingSpaces  (ID);
        GetRidOfPrecedingSpaces  (CMD);
        GetRidOfPostcedingSpaces (ID);
        GetRidOfPostcedingSpaces (CMD);

        IF ID <> '' THEN ProcessPostConfigInstruction (ID, CMD);
        END;

    Close (FileRead);

    SetUpExchangeInformation (ActiveExchange, ExchangeInformation);

    DoingDomesticMults := ActiveDomesticMult <> NoDomesticMults;
    DoingDXMults       := ActiveDXMult       <> NoDXMults;
    DoingPrefixMults   := ActivePrefixMult   <> NoPrefixMults;
    DoingZoneMults     := ActiveZoneMult     <> NoZoneMults;

    IF DomesticQTHDataFileName <> '' THEN
        IF NOT DomQTHTable.LoadInDomQTHFile (DomesticQTHDataFileName) THEN
            BEGIN
            ReportError ('Unable to find ' + DomesticQTHDataFileName + '!!');
            Exit;
            END;

    Sheet.SetUpRemainingMultiplierArrays;

    NumberDifferentMults := 0;

    IF DoingZoneMults THEN
        BEGIN
        Inc (NumberDifferentMults);
        RemainingMultDisplay := Zone;
        END;

    IF (DoingDomesticMults) AND (ActiveDomesticMult <> WYSIWYGDomestic) THEN
        RemainingMultDisplay := Domestic;

    IF DoingDomesticMults THEN
        Inc (NumberDifferentMults);

    IF DoingDXMults THEN
        BEGIN
        Inc (NumberDifferentMults);
        RemainingMultDisplay := DX;
        END;

    IF DoingPrefixMults THEN Inc (NumberDifferentMults);

    IF NumberDifferentMults > 2 THEN
        BEGIN
        ReportError ('Too many multipliers enabled!!');
        Halt;
        END;

    SetUpPostParametersFromLOGCFGFile := ActiveExchange <> UnknownExchange;
    END;



FUNCTION GetContestExchangeFromLogEntryExchange (LogEntry: STRING;
                                                 VAR RXData: ContestExchange): BOOLEAN;

{ This procedure will take a log entry from a LOG and parse it back into
  the ContestExchange format.  It ignores the multiplier string and
  generates the multiplier flags based upon the exchange string. }

VAR ExchangeString, MultString: Str40;
    Mult, NumberMults: INTEGER;
    MultArray: ARRAY [1..2] OF Str20;
    MultIdentified: BOOLEAN;

    BEGIN
    GetContestExchangeFromLogEntryExchange := False;

    IF (ActiveExchange = UnknownExchange) OR (LogEntry = '') THEN Exit;

    ClearContestExchange (RXData);

    ExpandTabs (LogEntry);

    RXData.Band       := GetLogEntryBand (LogEntry);
    RXData.Mode       := GetLogEntryMode (LogEntry);
    RXData.Callsign   := GetLogEntryCall (LogEntry);

    IF (RXData.Band = NoBand) OR (RXData.Mode = NoMode) OR
       (RXData.Callsign = '') THEN Exit;

    RXData.Date       := GetLogEntryDateString (LogEntry);
    RXData.Time       := GetLogEntryIntegerTime (LogEntry);
    RXData.NumberSent := GetLogEntryQSONumber (LogEntry);
    RXData.QSOPoints  := GetLogEntryQSOPoints (LogEntry);

    ParseExchangeIntoContestExchange (LogEntry, RXData);

    { We now have all of the log entry elements in RXData.  We need to
      set the DX QTH and prefix variables up, and if we are doing
      domestic QTHs, see if we can figure out what it is. }

    LocateCall (RXData.Callsign, RXData.QTH, True);

    IF DoingDXMults THEN GetDXQTH (RXData);

    IF DoingPrefixMults THEN
        CASE ActivePrefixMult OF

            BelgiumPrefixes:
                IF RXData.QTH.CountryID = 'ON' THEN
                    RXData.Prefix := RXData.QTH.Prefix;

            SACDistricts: RXData.Prefix := SACDistrict (RXData.QTH);
            Prefix: RXData.Prefix := RXData.QTH.Prefix;

            SouthAmericanPrefixes:
                IF RXData.QTH.Continent = SouthAmerica THEN
                    RXData.Prefix := RXData.QTH.Prefix;
            END;

    IF DoingDomesticMults AND (RXData.DXQTH = '') THEN
        FoundDomesticQTH (RXData);

    { Now set the multiplier flags }

    Sheet.SetMultFlags (RXData);
    GetContestExchangeFromLogEntryExchange := True;
    END;



FUNCTION GetContestExchangeFromMultiplierString (LogEntry: Str80;
                                                 VAR RXData: ContestExchange): BOOLEAN;

{ This procedure will recreate the QTHs from the multiplier string and set
  the multiplier flags. }

VAR MultString: Str20;
    NumberMults, Mult: INTEGER;
    MultArray: ARRAY [1..2] OF Str20;
    MultIdentified: BOOLEAN;

    BEGIN
    GetContestExchangeFromMultiplierString := False;
    ClearContestExchange (RXData);

    IF NumberDifferentMults = 0 THEN Exit;

    ExpandTabs (LogEntry);

    MultString := GetLogEntryMultString (LogEntry);

    IF MultString = '' THEN Exit;

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

        IF DoingDomesticMults AND
           (StringHasLowerCase (MultArray [Mult]) OR (NumberDifferentMults = 1)) THEN
            BEGIN
            RXData.QTHString := MultArray [Mult];
            RXData.Callsign  := GetLogEntryCall (LogEntry);


{ This next step is necessary to fake the FoundDomesticQTH routine into
  thinking the Domestic QTH is valid if you are using GridFields. }

            IF (ActiveDomesticMult = GridFields) AND (Length (RXData.QTHString) = 2) THEN
                RXData.QTHString := RXData.QTHString + '12';

            FoundDomesticQTH (RXData);
            RXData.DomesticMult := True;
            MultIdentified := True;
            END;

        IF DoingZoneMults AND StringIsAllNumbers (MultArray [Mult]) AND NOT MultIdentified THEN
            BEGIN
            RXData.ZoneMult := True;
            RXData.Zone := MultArray [Mult];
            MultIdentified := True;
            END;

        IF DoingPrefixMults AND NOT MultIdentified THEN
            BEGIN
            RXData.PrefixMult := True;
            RXData.Prefix := MultArray [Mult];
            MultIdentified := True;
            END;

        IF DoingDXMults AND NOT MultIdentified THEN
            BEGIN
            RXData.DXQTH := MultArray [Mult];
            RXData.DXMult := True;
            END;
        END;

    GetContestExchangeFromMultiplierString := True;
    END;



PROCEDURE PushLogFiles;

{ This procedure will take the current active log file and create a
  backup file with the filename PLOG###.BAK.  ## is intially 01, and
  then increments each time.  The active log file is removed. }

VAR FileNumber: INTEGER;
    TempString: Str20;

    BEGIN
    FileNumber := 0;

    REPEAT
        Str (FileNumber, TempString);
        WHILE Length (TempString) < 3 DO TempString := '0' + TempString;

        TempString := 'PLOG' + Tempstring + '.BAK';

        IF NOT FileExists (TempString) THEN
            BEGIN
            RenameFile (LogFileName, TempString);

            LastPushedLogName := TempString;
            Exit;
            END;

        Inc (FileNumber);

    UNTIL FileNumber > 1000;

    ReportError ('Unable to create backup file!!');
    Halt;
    END;



FUNCTION GetDayOfYear (DateString: Str20; VAR Year: INTEGER): INTEGER;

VAR Day, Result, Month, MonthAdder: INTEGER;
    DayString, MonthString, YearString: Str20;

    BEGIN
    DayString   := Copy (DateString, 1, 2);
    MonthString := Copy (DateString, 4, 3);
    YearString  := Copy (DateString, 8, 2);

    Val (DayString, Day, Result);

    Val (YearString, Year, Result);

    MonthString := UpperCase (MonthString);

    IF MonthString = 'JAN' THEN Month := 1;
    IF MonthString = 'FEB' THEN Month := 2;
    IF MonthString = 'MAR' THEN Month := 3;
    IF MonthString = 'APR' THEN Month := 4;
    IF MonthString = 'MAY' THEN Month := 5;
    IF MonthString = 'JUN' THEN Month := 6;
    IF MonthString = 'JUL' THEN Month := 7;
    IF MonthString = 'AUG' THEN Month := 8;
    IF MonthString = 'SEP' THEN Month := 9;
    IF MonthString = 'OCT' THEN Month := 10;
    IF MonthString = 'NOV' THEN Month := 11;
    IF MonthString = 'DEC' THEN Month := 12;

    IF Year MOD 4 = 0 THEN { Leap year for my lifetime }
        BEGIN
        CASE Month OF
             1: MonthAdder := 0;
             2: MonthAdder := 31;
             3: MonthAdder := 31 + 29;
             4: MonthAdder := 31 + 29 + 31;
             5: MonthAdder := 31 + 29 + 31 + 30;
             6: MonthAdder := 31 + 29 + 31 + 30 + 31;
             7: MonthAdder := 31 + 29 + 31 + 30 + 31 + 30;
             8: MonthAdder := 31 + 29 + 31 + 30 + 31 + 30 + 31;
             9: MonthAdder := 31 + 29 + 31 + 30 + 31 + 30 + 31 + 31;
            10: MonthAdder := 31 + 29 + 31 + 30 + 31 + 30 + 31 + 31 + 30;
            11: MonthAdder := 31 + 29 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31;
            12: MonthAdder := 31 + 29 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30;
            END;
        END

    ELSE
        BEGIN
        CASE MONTH OF
             1: MonthAdder := 0;
             2: MonthAdder := 31;
             3: MonthAdder := 31 + 28;
             4: MonthAdder := 31 + 28 + 31;
             5: MonthAdder := 31 + 28 + 31 + 30;
             6: MonthAdder := 31 + 28 + 31 + 30 + 31;
             7: MonthAdder := 31 + 28 + 31 + 30 + 31 + 30;
             8: MonthAdder := 31 + 28 + 31 + 30 + 31 + 30 + 31;
             9: MonthAdder := 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31;
            10: MonthAdder := 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30;
            11: MonthAdder := 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31;
            12: MonthAdder := 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30;
            END;
        END;

    GetDayOfYear := Day + MonthAdder;
    END;



FUNCTION ComputeDateString (DayOfYear: INTEGER; Year: INTEGER): Str20;

VAR FebDays: INTEGER;
    DayString, YearString: Str20;

    BEGIN
    Str (Year, YearString);

    IF Length (YearString) = 1 THEN YearString := '0' + YearString;

    IF Year MOD 4 = 0 THEN FebDays := 29 ELSE FebDays := 28;

    IF DayOfYear <= 31 THEN   { January }
        BEGIN
        Str (DayOfYear, DayString);
        IF Length (DayString) = 1 THEN DayString := '0' + DayString;
        ComputeDateString := DayString + '-' + 'Jan' + '-' + YearString;
        Exit;
        END;

    DayOfYear := DayOfYear - 31;

    IF DayOfYear <= FebDays THEN  { February }
        BEGIN
        Str (DayOfYear, DayString);
        IF Length (DayString) = 1 THEN DayString := '0' + DayString;
        ComputeDateString := DayString + '-' + 'Feb' + '-' + YearString;
        Exit;
        END;

    DayOfYear := DayOfYear - FebDays;

    IF DayOfYear <= 31 THEN  { March }
        BEGIN
        Str (DayOfYear, DayString);
        IF Length (DayString) = 1 THEN DayString := '0' + DayString;
        ComputeDateString := DayString + '-' + 'Mar' + '-' + YearString;
        Exit;
        END;

    DayOfYear := DayOfYear - 31;

    IF DayOfYear <= 30 THEN { April }
        BEGIN
        Str (DayOfYear, DayString);
        IF Length (DayString) = 1 THEN DayString := '0' + DayString;
        ComputeDateString := DayString + '-' + 'Apr' + '-' + YearString;
        Exit;
        END;

    DayOfYear := DayOfYear - 30;

    IF DayOfYear <= 31 THEN { May }
        BEGIN
        Str (DayOfYear, DayString);
        IF Length (DayString) = 1 THEN DayString := '0' + DayString;
        ComputeDateString := DayString + '-' + 'May' + '-' + YearString;
        Exit;
        END;

    DayOfYear := DayOfYear - 31;

    IF DayOfYear <= 30 THEN { June }
        BEGIN
        Str (DayOfYear, DayString);
        IF Length (DayString) = 1 THEN DayString := '0' + DayString;
        ComputeDateString := DayString + '-' + 'Jun' + '-' + YearString;
        Exit;
        END;

    DayOfYear := DayOfYear - 30;

    IF DayOfYear <= 31 THEN { July }
        BEGIN
        Str (DayOfYear, DayString);
        IF Length (DayString) = 1 THEN DayString := '0' + DayString;
        ComputeDateString := DayString + '-' + 'Jul' + '-' + YearString;
        Exit;
        END;

    DayOfYear := DayOfYear - 31;

    IF DayOfYear <= 31 THEN { August }
        BEGIN
        Str (DayOfYear, DayString);
        IF Length (DayString) = 1 THEN DayString := '0' + DayString;
        ComputeDateString := DayString + '-' + 'Aug' + '-' + YearString;
        Exit;
        END;

    DayOfYear := DayOfYear - 31;


    IF DayOfYear <= 30 THEN { September }
        BEGIN
        Str (DayOfYear, DayString);
        IF Length (DayString) = 1 THEN DayString := '0' + DayString;
        ComputeDateString := DayString + '-' + 'Sep' + '-' + YearString;
        Exit;
        END;

    DayOfYear := DayOfYear - 30;

    IF DayOfYear <= 31 THEN { October }
        BEGIN
        Str (DayOfYear, DayString);
        IF Length (DayString) = 1 THEN DayString := '0' + DayString;
        ComputeDateString := DayString + '-' + 'Oct' + '-' + YearString;
        Exit;
        END;

    DayOfYear := DayOfYear - 31;

    IF DayOfYear <= 30 THEN { November }
        BEGIN
        Str (DayOfYear, DayString);
        IF Length (DayString) = 1 THEN DayString := '0' + DayString;
        ComputeDateString := DayString + '-' + 'Nov' + '-' + YearString;
        Exit;
        END;

    DayOfYear := DayOfYear - 30;

    Str (DayOfYear, DayString);
    IF Length (DayString) = 1 THEN DayString := '0' + DayString;
    ComputeDateString := DayString + '-' + 'Dec' + '-' + YearString;
    END;



PROCEDURE ChangeTime (VAR DateString: Str20; VAR TimeString: Str20; Offset: INTEGER);

VAR Minute, Hour, TimeMinutes, Result: INTEGER;
    DayOffset: INTEGER;
    DayofYear, Month, Year: INTEGER;
    MinuteString, HourString: Str20;

    BEGIN
    Val (PostcedingString (TimeString, ':'), Minute, Result);
    Val (PrecedingString  (TimeString, ':'), Hour,   Result);

    TimeMinutes := (Hour * 60) + Minute;

    { Compute new minute }

    TimeMinutes := TimeMinutes + Offset;

    DayOffset := 0;

    WHILE TimeMinutes < 0 DO
        BEGIN
        Dec (DayOffset);
        TimeMinutes := TimeMinutes + (24 * 60);
        END;

    WHILE TimeMinutes >= (24 * 60) DO
        BEGIN
        Inc (DayOffset);
        TimeMinutes := TimeMinutes - (24 * 60);
        END;

    IF DayOffset <> 0 THEN
        BEGIN
        DayOfYear  := GetDayOfYear (DateString, Year);
        DayOfYear  := DayOfYear + DayOffset;
        DateString := ComputeDateString (DayOfYear, Year);
        END;

    { Reconstruct time string }

    Minute := TimeMinutes MOD 60;
    Hour   := TimeMinutes DIV 60;

    Str (Minute, MinuteString);
    Str (Hour,   HourString);

    IF Length (MinuteString) = 1 THEN MinuteString := '0' + MinuteString;
    IF Length (HourString)   = 1 THEN HourString   := '0' + HourString;

    TimeString := HourString + ':' + MinuteString;
    END;



PROCEDURE ChangeLogStringDateTime (VAR LogString: STRING; Offset: INTEGER);

VAR TestString, DateString, TimeString: Str20;

    BEGIN
    DateString := GetLogEntryDateString (LogString);
    TimeString := GetLogEntryTimeString (LogString);

    TestString := UpperCase (Copy (DateString, 3, 5));

    IF (TestString = '-JAN-') OR (TestString = '-FEB-') OR
       (TestString = '-MAR-') OR (TestString = '-APR-') OR
       (TestString = '-MAY-') OR (TestString = '-JUN-') OR
       (TestString = '-JUL-') OR (TestString = '-AUG-') OR
       (TestString = '-SEP-') OR (TestString = '-OCT-') OR
       (TestString = '-NOV-') OR (TestString = '-DEC-') THEN
           IF (TimeString [3] = ':') AND
              (StringIsAllNumbers (Copy (TimeString, 1, 2))) AND
              (StringIsAllNumbers (Copy (TimeString, 4, 2))) AND
              (StringIsAllNumbers (Copy (DateString, 1, 2))) AND
              (StringIsAllNumbers (Copy (DateString, 8, 2))) THEN
                  BEGIN
                  ChangeTime (DateString, TimeString, Offset);

                  Delete (LogString, LogEntryHourAddress, 5);
                  Insert (TimeString, LogString, LogEntryHourAddress);

                  Delete (LogString, LogEntryDayAddress, 9);
                  Insert (DateString, LogString, LogEntryDayAddress);
                  END;
    END;




    BEGIN
    ActiveExchange := UnknownExchange;
    LastPushedLogName := '';
    LogTotalsAccurate := False;
    NumberLogFilesOpen := 0;
    Sheet.DupeSheetEnable := True;
    END.
