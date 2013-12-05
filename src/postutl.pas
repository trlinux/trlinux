UNIT PostUtl;

{$O+}
{$V-}

INTERFACE

Uses trCrt,
     Tree,
     SlowTree,
     PostSubs,
     Country9,
     LogDupe,
     ZoneCont,
     LogName,
     LogGrid,
     LogWind,
     LogSCP,
     PostSCP;

FUNCTION UtilityMenu: BOOLEAN;

IMPLEMENTATION
uses keycode;


CONST BufferSize = 300;
      CallBufferSize = 1000;

TYPE FileBuffer = ARRAY [0..BufferSize] OF Str160;
     FileBufferPointer = ^FileBuffer;

     CallBuffer = ARRAY [0..CallbufferSize] OF CallString;
     CallBufferPointer = ^CallBuffer;

     SCPArray = ARRAY [0..36, 0..36] OF LONGINT;
     SCPArrayPtr = ^SCPArray;

VAR Buffer: FileBufferPointer;
    CheckCallBuffer: CallBufferPointer;
    SCPIndexMatrix: SCPArrayPtr;

{ UTILITY Programs }

FUNCTION BadName (Name: Str80): BOOLEAN;

VAR CharPointer: INTEGER;

    BEGIN
    BadName := True;
    IF Name = '' THEN Exit;

    IF Length (Name) < 2 THEN
        BEGIN
        WriteLn ('That name is really short!!');
        Exit;
        END;

    IF (Length (Name) > 6) THEN
        BEGIN
        WriteLn ('That name is too long!!');
        Exit;
        END;

    FOR CharPointer := 1 TO Length (Name) DO
        IF Name [CharPointer] < 'A' THEN
            BEGIN
            WriteLn ('That name has a funny character!!');
            Exit;
            END;

    BadName := False;
    END;



FUNCTION BadCall (Call: Str80): BOOLEAN;

VAR CharPointer: INTEGER;

    BEGIN
    BadCall := True;
    IF Call = '' THEN Exit;
    IF Length (Call) < 2 THEN
        BEGIN
        WriteLn ('That call is too short!!');
        Exit;
        END;

    IF Length (Call) > 6 THEN
        BEGIN
        WriteLn ('That call is too long!!');
        Exit;
        END;

    BadCall := False;

    FOR CharPointer := 1 TO Length (Call) DO
        IF (Call [CharPointer] >= '0') AND (Call [CharPointer] <= '9') THEN
            Exit;

    WriteLn ('That call does not have a number!!');
    BadCall := True;
    END;



PROCEDURE ShowNameEditCommands;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('NAME EDITOR PROGRAM');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This editor allows you to access your NAMES.CMQ database.  This name database');
    WriteLn ('is no longer used by the TR program.  Instead, the names are stored in the ');
    WriteLn ('TRMASTER.DTA file.  You can move the names in your NAMES.CMQ database to the');
    WriteLn ('new database.  To do this, save the data in an ASCII file with the F command.');
    WriteLn ('Then import the data into the TRMASTER.DTA file using the DTA database editor''s');
    WriteLn ('file import menu (accessed with the commands POST U E F N).');
    WriteLn;
    WriteLn ('Single letter commands: C - Toggle CW sending of names.');
    WriteLn ('                        D - Delete a call from the database.');
    WriteLn ('                        F - Make ASCII file with calls and names.');
    WriteLn ('                        G - Delete calls from a file.');
    WriteLn ('                        I - Input ASCII file with calls and names.');
    WriteLn ('                        P - Make possible call list for a call.');
    WriteLn ('                        X - Quit editing and save changes.');
    WriteLn;

    END;



PROCEDURE AddHeadingToOutputFile (Call: Str20;
                                  Heading: Str20;
                                  VAR FileWrite: TEXT);

    BEGIN
    IF Call = '' THEN Exit;
    IF Heading = '' THEN Exit;

    IF Call = '3Y/B'   THEN Exit;
    IF Call = '3Y/P'   THEN Exit;
    IF Call = 'CE0/JF' THEN Exit;
    IF Call = 'CE0/SF' THEN Exit;
    IF Call = 'HK0/M'  THEN Exit;
    IF Call = 'JD/O'   THEN Exit;
    IF Call = 'KC6/W'  THEN Exit;
    IF Call = 'LU4FM'  THEN Exit;
    IF Call = 'SV/A'   THEN Exit;
    IF Call = 'VE1/SA' THEN Exit;
    IF Call = 'VE1/SP' THEN Exit;
    IF Call = 'VE1'    THEN Exit;
    IF Call = 'VE2'    THEN Exit;
    IF Call = 'VE3'    THEN Exit;
    IF Call = 'VE4'    THEN Exit;
    IF Call = 'VE5'    THEN Exit;
    IF Call = 'VE6'    THEN Exit;
    IF Call = 'VE7'    THEN Exit;
    IF Call = 'VE8'    THEN Exit;
    IF Call = 'VP8/G'  THEN Exit;
    IF Call = 'VP8/O'  THEN Exit;
    IF Call = 'VP8/SA' THEN Exit;
    IF Call = 'VP8/SH' THEN Exit;
    IF Call = 'VK9/W'  THEN Exit;
    IF Call = 'VU/A'   THEN Exit;
    IF Call = 'VU/L'   THEN Exit;
    IF Call = 'VU7/L'  THEN Exit;
    IF Call = 'VU7/A'  THEN Exit;

    IF Call = 'W0' THEN Exit;
    IF Call = 'W1' THEN Exit;
    IF Call = 'W2' THEN Exit;
    IF Call = 'W3' THEN Exit;
    IF Call = 'W4' THEN Exit;
    IF Call = 'W5' THEN Exit;
    IF Call = 'W6' THEN Exit;
    IF Call = 'W7' THEN Exit;
    IF Call = 'W8' THEN Exit;
    IF Call = 'W9' THEN Exit;
    IF Call = 'ZK1/S' THEN Exit;
    IF Call = '' THEN Exit;
    IF Call = '' THEN Exit;
    IF Call = '' THEN Exit;
    IF Call = '' THEN Exit;
    IF Call = '' THEN Exit;
    IF Call = '' THEN Exit;
    IF Call = '' THEN Exit;
    IF Call = '' THEN Exit;

    IF Call = '3Y/B'   THEN Call := '3Y';
    IF Call = '4J'     THEN Call := '4J1FS';
    IF Call = 'CT'     THEN Call := 'CT1';
    IF Call = 'EA'     THEN Call := 'EA1';
    IF Call = 'F'      THEN Call := 'F1';
    IF Call = 'G'      THEN Call := 'G3';
    IF Call = 'KC6/E'  THEN Call := 'KC6XX';
    IF Call = 'HK0/S'  THEN Call := 'HK0';
    IF Call = 'JD/MT'  THEN Call := 'JD1';
    IF Call = 'PJ'     THEN Call := 'PJ7';
    IF Call = 'PJ/SM'  THEN Call := 'PJ4';
    IF Call = 'UA1/FJ' THEN Call := '4K2';
    IF Call = 'UP'     THEN Call := 'LY';
    IF Call = 'UR'     THEN Call := 'ES';
    IF Call = 'UQ'     THEN Call := 'YL';
    IF Call = 'VK9/C'  THEN Call := 'VK9Y';
    IF Call = 'VK9/M'  THEN Call := 'VK9Z';
    IF Call = 'VK0/M'  THEN Call := 'VK0';
    IF Call = 'VP8/F'  THEN Call := 'VP8';
    IF Call = 'VU'     THEN Call := 'VU2';
    IF Call = 'W'      THEN Call := 'W';
    IF Call = 'XE'     THEN Call := 'XE1';
    IF Call = 'ZK1/N'  THEN Call := 'ZK1';
    IF Call = 'ZL'     THEN Call := 'ZL2';
    IF Call = 'ZS3'    THEN Call := 'V5';

    IF CountryTable.GetCountry (Call, True) = -1 THEN
        ReportError ('Unknown country found for ' + Call)
    ELSE
        WriteLn (FileWrite, Call, ' = ', Heading, DegreeSymbol);
    END;



FUNCTION NumberFileQSOs (FileName: Str80): LONGINT;

VAR FileRead: TEXT;

    BEGIN
    NumberFileQSOs := 0;

    IF FileExists (FileName) THEN
       NumberFileQSOs := GetFileSize (FileName) DIV 84;
    END;



PROCEDURE AppendProcedure;

VAR SourceFileName, DestFileName: Str80;
    NumberLinesRead, Line: INTEGER;
    FileRead, FileWrite: TEXT;
    Key: CHAR;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('APPEND PROGRAM');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This program will append a file to another existing file.  It is intended to');
    WriteLn ('be used to append a log file to a history file.');
    WriteLn;

    SourceFileName := GetResponse ('Enter source file name : ');
    IF SourceFileName = '' THEN Exit;

    WriteLn ('Source file has about ', NumberFileQSOs (SourceFileName), ' QSOs in it.');

    DestFileName   := GetResponse ('Enter file to add ' + SourceFileName + ' to : ');
    IF DestFileName = '' THEN Exit;

    WriteLn ('Destination file has about ', NumberFileQSOs (DestFileName), ' QSOs in it.');

    IF SourceFileName = DestFileName THEN
        BEGIN
        ReportError ('You cannot copy a file onto itself!!');
        WaitForKeyPressed;
        Exit;
        END;

    IF NOT OpenFileForRead (FileRead, SourceFileName) THEN
        BEGIN
        ReportError (SourceFileName + ' does not exist!!');
        WaitForKeyPressed;
        Exit;
        END;

    IF NOT OpenFileForAppend (FileWrite, DestFileName) THEN
        BEGIN
        ReportError ('Error trying to open ' + DestFileName);
        WaitForKeyPressed;
        Exit;
        END;

    WriteLn;

    REPEAT
        Key := UpCase (GetKey ('Okay to proceed? (Y/N) : '));
        IF (Key = EscapeKey) OR (Key = 'N') THEN Exit;
    UNTIL Key = 'Y';
    WriteLn;
    WriteLn;

    New (Buffer);

    REPEAT
        NumberLinesRead := 0;

        WHILE (NOT Eof (FileRead)) AND (NumberLinesRead < BufferSize) DO
            BEGIN
            ReadLn (FileRead, Buffer^ [NumberLinesRead]);
            Inc (NumberLinesRead);
            END;

        IF NumberLinesRead > 0 THEN
            FOR Line := 0 TO NumberLinesRead - 1 DO
                WriteLn (FileWrite, Buffer^ [Line]);

        IF Eof (FileRead) THEN
            BEGIN
            Close (FileRead);
            Close (FileWrite);
            Dispose (Buffer);
            WriteLn (SourceFileName, ' has been successfully added to ', DestFileName, '.');
            WriteLn (DestFileName, ' now has about ', NumberFileQSOs (DestFileName), ' QSOs in it.');
            WriteLn;

            REPEAT
                Key := UpCase (GetKey ('Do you wish to delete ' + SourceFileName + '? (Y/N) : '));
                IF Key = EscapeKey THEN Exit;
            UNTIL (Key = 'Y') OR (Key = 'N');
            WriteLn;

            IF Key = 'Y' THEN
                BEGIN
                DeleteFile (SourceFileName);
                WriteLn (SourceFileName, ' has been deleted.');
                END;

            WaitForKeyPressed;
            Exit;
            END;

    UNTIL False;
    END;



PROCEDURE CountryCheck;

VAR QTH: QTHRecord;
    ID, Call: CallString;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('COUNTRY AND ZONE CHECK UTILITY');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('This utility will allow you to enter a callsign and view the pre-programmed ');
    WriteLn ('information.  This will show you the ARRL country, CQ county, continent, CQ');
    WriteLn ('zone and ITU zone.');
    WriteLn;

    REPEAT
        Call:= UpperCase (GetResponse ('Enter call (none to exit) : '));
        IF Call = '' THEN Exit;

        ClrScr;

        TextColor (Cyan);

        Write ('Callsign = ', Call);

        CountryTable.ZoneMode    := ITUZoneMode;

        LocateCall (Call, QTH, True);

        IF QTH.StandardCall <> Call THEN
            Write ('    Standard format callsign = ', QTH.StandardCall);

        WriteLn;

        CountryTable.CountryMode := ARRLCountryMode;
        WriteLn ('ARRL Country = ',  CountryTable.GetCountryName (CountryTable.GetCountry (Call, True)));

        CountryTable.CountryMode := CQCountryMode;
        WriteLn ('CQ Country = ',  CountryTable.GetCountryName (CountryTable.GetCountry (Call, True)));

        WriteLn ('Continent = ', CountryTable.GetContinentName (QTH.Continent));
        WriteLn ('CQ Zone = ', CountryTable.GetCQZone (Call), '    ITU Zone = ', CountryTable.GetITUZone (Call));
        WriteLn;

        WriteLn ('Grid square for beam heading purposes = ', CountryTable.GetGrid (Call, ID));
        WriteLn ('The country ID for this country is = ', ID);
        WriteLn;

    UNTIL False;
    END;



FUNCTION CallHasNoNumber (Call: CallString): BOOLEAN;

VAR CharPointer: INTEGER;

    BEGIN
    IF Call = '' THEN
        BEGIN
        CallHasNoNumber := True;
        Exit;
        END;

    CallHasNoNumber := False;

    FOR CharPointer := 1 TO Length (Call) DO
        IF ((Call [CharPointer] >= '0') AND (Call [CharPointer] <= '9')) THEN
            Exit;

    CallHasNoNumber := True;
    END;



PROCEDURE GlobalLogSearch;

VAR FileString, Callsign, CallListFileName, LogFileName, OutputFileName: Str80;
    CallListRead, OutputFileWrite, LogFileRead: TEXT;
    CallAddress, NumberCallsLeft, NumberCallsMatched, NumberCallsInBuffer: INTEGER;

    BEGIN
    NumberCallsLeft := 0;

    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('GLOBAL LOG SEARCH');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('This procedure take a list of callsigns and a list of filenames and see if');
    WriteLn ('each callsign appears in at least one of the files.  If it does, it will');
    WriteLn ('be removed from the list.');
    WriteLn;
    WriteLn ('This procedure was initially intended to be used as a way to remove names');
    WriteLn ('from the database that are not in your recent logs.  To use it this was,');
    WriteLn ('use the F command in the name editor to make an ASCII list of calls and');
    WriteLn ('names.  Execute this procedure on all the files you want to look through,');
    WriteLn ('then delete the remaining calls using the G command in the name editor.');
    WriteLn;

    CallListFileName := GetResponse ('Enter file name containing call list to be checked : ');
    IF (CallListFileName = '') OR (NOT FileExists (CallListFileName)) THEN Exit;

    LogFileName := GetResponse ('Enter a log file name to check against : ');
    IF (LogFileName = '') OR (NOT FileExists (CallListFileName)) THEN Exit;

    OutputFileName := GetResponse ('Enter output file name for calls not found in the log : ');
    IF OutputFileName = '' THEN Exit;

    IF CallListFileName = OutputFileName THEN Exit;

    IF NOT OpenFileForRead (CallListRead, CallListfileName) THEN Exit;
    IF NOT OpenFileForWrite (OutputFileWrite, OutputFileName) THEN Exit;

    New (CheckCallBuffer);
    NumberCallsInBuffer := 0;
    NumberCallsMatched := 0;

    WHILE NOT Eof (CallListRead) DO
        BEGIN

        WHILE NOT Eof (CallListRead) AND (NumberCallsInBuffer < CallBufferSize) DO
            BEGIN
            ReadLn (CallLIstRead, FileString);

            Callsign := UpperCase (GetFirstString (FileString));

            IF Callsign <> '' THEN
                BEGIN
                CheckCallBuffer^ [NumberCallsInBuffer] := Callsign;
                Inc (NumberCallsInBuffer);
                END;
            END;

        IF NumberCallsInBuffer > 0 THEN
            BEGIN
            IF OpenFileForRead (LogFileRead, LogFileName) THEN
                BEGIN

                WHILE NOT Eof (LogFileRead) DO
                    BEGIN
                    IF KeyPressed AND (ReadKey = EscapeKey) THEN
                        BEGIN
                        Close (LogFileRead);
                        Close (CallListRead);
                        Close (OutputFileWrite);
                        WriteLn;
                        Writeln ('Procedure aborted.  However, ', OutputFileName, ' is still valid.');
                        WriteLn;
                        Exit;
                        Dispose (CheckCallBuffer);
                        END;

                    ReadLn (LogFileRead, FileString);

                    FileString := UpperCase (FileString);

                    FOR CallAddress := 0 TO NumberCallsInBuffer - 1 DO
                        IF CheckCallBuffer^ [CallAddress] <> '' THEN
                            IF Pos (CheckCallBuffer^ [CallAddress], FileString) > 0 THEN
                                BEGIN
                                Inc (NumberCallsMatched);
                                WriteLn (NumberCallsMatched, ' ', CheckCallBuffer^ [CallAddress], ' found in log!!');
                                CheckCallBuffer^ [CallAddress] := '';
                                Break;
                                END;

                    END;

                Close (LogFileRead);
                END;

            FOR CallAddress := 0 TO NumberCallsInBuffer - 1 DO
                IF CheckCallBuffer^ [CallAddress] <> '' THEN
                    BEGIN
                    Inc (NumberCallsLeft);
                    WriteLn (OutputFileWrite, CheckCallBuffer^ [CallAddress]);
                    WriteLn (NumberCallsLeft, ' ', CheckCallBuffer^ [CallAddress], ' not found in log.  Saved to ',
                             OutputFileName);
                    END;

            END;

        NumberCallsInBuffer := 0;
        END;

    Close (CallListRead);
    Close (OutputFileWrite);
    Dispose (CheckCallBuffer);
    WriteLn;
    WriteLn ('There were ', NumberCallsMatched, ' calls found in the log.');
    WriteLn ('There were ', NumberCallsLeft, ' calls written to ', OutputFileName);
    Congrats;
    WaitForKeyPressed;
    END;


PROCEDURE NameEditor;

VAR FileName, Name, TotalString, Call: Str80;
    NumberTimes, NameCode, NamePointer, Offset, CallPointer: INTEGER;
    CWSend: BOOLEAN;
    StartOfCallEntered: INTEGER;
    FileRead: TEXT;

    LastPrefix: Str20;
    TempBytes: Fourbytes;
    FileString: Str20;

    BEGIN
    GoToXY (1, WhereY);
    ClrEol;
    TextColor (Cyan);
    Write ('Loading name file.  Please wait...');

    IF NOT Names.Load THEN
        BEGIN
        GoToXY (1, WhereY);
        ReportError ('WARNING!! No NAMES.CMQ file found.  You are starting at zero.');
        Delay (3000);
        END;

    LastPrefix := '';

    ShowNameEditCommands;
    CWSend := True;

    REPEAT
        Str (Names.TotalNumberOfCalls, TotalString);
        GoToXY (1, WhereY);
        ClrEol;

        TextColor (Cyan);

        Write ('Enter call to edit (' + TotalString + ') : ');

        StartOfCallEntered := WhereX;

        TextColor (Yellow);

        ReadLn (Call);
        Call := UpperCase (Call);

        IF Length (Call) = 1 THEN
            BEGIN
            CASE Call [1] OF
                'C': CWSend := NOT CWSend;
                'D': BEGIN
                     GoToXY (40, WhereY - 1);
                     Call := UpperCase (GetResponse ('Callsign to delete : '));
                     IF Call <> '' THEN Names.DeleteName (Call);
                     END;

                'F': BEGIN
                     GoToXY (40, WhereY -1);
                     FileName := UpperCase (GetResponse ('Filename for list : '));
                     IF FileName <> '' THEN Names.MakeASCIIList (FileName);
                     END;

                'G': BEGIN
                     GoToXY (40, WhereY - 1);
                     FileName := Uppercase (GetResponse ('File to delete calls of : '));

                     IF FileName <> '' THEN
                          IF OpenFileForRead (FileRead, FileName) THEN
                              BEGIN
                              WHILE NOT Eof (FileRead) DO
                                  BEGIN
                                  ReadLn (FileRead, FileString);
                                  Names.DeleteName (FileString);
                                  WriteLn ('Deleting ' , FileString);
                                  END;

                              Close (FileRead);
                              END;
                     END;

                'I': Names.InputASCIIFile;

                'X': BEGIN
                     Names.Save;
                     WaitForKeyPressed;
                     Exit;
                     END;

                ELSE
                    ShowNameEditCommands;

                END;  { of single letter command case }
            END

        ELSE
            BEGIN
            IF ((Length (Call) = 2) OR (Length (Call) = 3)) AND CallHasNoNumber (Call) THEN
                BEGIN
                Call := LastPrefix + Call;
                GoToXY (StartOfCallEntered, WhereY - 1);
                Write (Call);
                GoToXY (40, WhereY);
                END
            ELSE
                GoToXY (40, WhereY - 1);

            LastPrefix := GetPrefix (Call);

            IF NOT BadCall (Call) THEN
                BEGIN
                IF Names.GetName (Call) = '' THEN
                    BEGIN
                    Name := UpperCase (GetResponse ('Name = '));
                    IF Name = '' THEN
                        BEGIN
                        GoToXY (1, WhereY - 1);
                        ClrEol;
                        END
                    ELSE
                        IF NOT BadName (Name) THEN
                            IF Name <> '' THEN
                                Names.AddName (Call, Name);
                    END
                ELSE
                    BEGIN
                    TextColor (Cyan);
                    Write ('Name = ');
                    TextColor (Red);
                    Name := Names.GetName (Call);
                    WriteLn (Name);
                    IF CWSend THEN SendMorse (Name);
                    END;
                END;
            END;
    UNTIL False;
    END;



PROCEDURE ShowRestartDotBin;


VAR FileWrite: TEXT;
    Block, NumberCalls, NumberEntriesInLastBlock, NumberBlocks: INTEGER;
    BlockAddress, CharPointer, EndAddress, Address: INTEGER;

    Band: BandType;
    Mode: ModeType;
    CallSign, TempString: Str20;
    CompressedCall: FourBytes;
    StartMode, EndMode: ModeType;
    StartBand, EndBand: BandType;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('GENERATE REPORT OF RESTART.BIN FILE');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('This procedure will generate a file with all of the information contained in');
    WriteLn ('the restart bin file.  This includes the QSO totals, dupesheets, exchange');
    WriteLn ('memory (if any) and partial call lists.  The output is saved in a file called');
    WriteLn ('RESTART.DAT');
    WriteLn;

    IF LogFileName = 'LOG.DAT' THEN
        LogRestartFileName := 'RESTART.BIN'
    ELSE
        LogRestartFileName := PrecedingString (LogFileName, '.') + '.RST';

    IF NOT FileExists (LogRestartFileName) THEN
        BEGIN
        ReportError (LogRestartFileName + ' not found in this directory!!');
        WaitForKeyPressed;
        Exit;
        END;

    ContestName := '';

    WriteLn ('Loading in ', LogRestartFileName);

    IF NOT Sheet.ReadInBinFiles (True) THEN
        BEGIN
        WaitForKeyPressed;
        Exit;
        END;

    GoToXY (1, WhereY);
    ClrEol;
    Write ('Saving output to RESTART.DAT...');

    OpenFileForWrite (FileWrite, 'RESTART.DAT');

    WriteLn (FileWrite, 'RESTART.BIN file contents.');
    WriteLn (FileWrite);
    WriteLn (FileWrite, 'Format version = ', RestartVersionNumber);
    WriteLn (FileWrite);
    WriteLn (FileWrite, 'Contest name = ', ContestName);
    WriteLn (FileWrite);

    WriteLn (FileWrite, 'DupeSheet totals : ');

    FOR Band := Band160 TO All DO
        FOR Mode := CW TO Both DO
            WriteLn (FileWrite, BandString [Band], ModeString [Mode], Sheet.DupeSheet.Totals [Band, Mode] :8);

    WriteLn (FileWrite);

    WriteLn (FileWrite, 'Number of big calls (> 6 characters) = ', Sheet.DupeSheet.NumberBigCalls);
    WriteLn (FileWrite);


    WriteLn (FileWrite, 'QSO totals (includes dupes) : ');

    FOR Band := Band160 TO All DO
        FOR Mode := CW TO Both DO
            WriteLn (FileWrite, BandString [Band], ModeString [Mode], QSOTotals [Band, Mode] :8);

    WriteLn (FileWrite);

    WriteLn (FileWrite, 'Total names sent = ', TotalNamesSent);
    WriteLn (FileWrite);

    WriteLn (FileWrite, 'Total QSO points = ', TotalQSOPoints);
    WriteLn (FileWrite);

    WriteLn (FileWrite, 'Code Speed = ', CodeSpeed);
    WriteLn (FileWrite);

    FOR Band := Band160 TO All DO
        FOR Mode := CW TO Both DO
            IF Sheet.DupeSheet.Totals [Band, Mode] > 0 THEN
                BEGIN
                NumberCalls  := Sheet.DupeSheet.Totals [Band, Mode];
                NumberBlocks := (NumberCalls DIV FourByteBlockSize) + 1;
                NumberEntriesInLastBlock := NumberCalls MOD FourByteBlockSize;

                WriteLn (FileWrite, 'Dupesheet for ', BandString [Band], ModeString [Mode], ' : ');

                Block := 1;

                REPEAT
                    IF Block = NumberBlocks THEN
                        EndAddress := NumberEntriesInLastBlock
                    ELSE
                        EndAddress := FourByteBlockSize;

                    FOR Address := 0 TO EndAddress - 1 DO
                        BEGIN
                        Write (FileWrite, (Address) + (Block - 1) * FourByteBlockSize + 1:5, ' ');

                        CompressedCall := Sheet.DupeSheet.DupeList [Band, Mode, Block]^ [Address];

                        IF PointsToBigCall (CompressedCall) THEN
                            TempString := BigExpandedString (Sheet.DupeSheet.BigCallList [1]^
                                              [BigEntryAddress (CompressedCall)])

                        ELSE
                            TempString := ExpandedString (CompressedCall);

                        GetRidOfPrecedingSpaces (TempString);
                        GetRidOfPostcedingSpaces (TempString);

                        IF Length (TempString) > 0 THEN
                            FOR CharPointer := 1 TO Length (TempString) DO
                                IF TempString [CharPointer] = ' ' THEN
                                    TempString [CharPointer] := '/';

                        WriteLn (FileWrite, TempString);
                        END;

                    Inc (Block);
                UNTIL Block > NumberBlocks;

                WriteLn (FileWrite);
                END;


    FOR Band := Band160 TO All DO
        FOR Mode := CW TO Both DO
          IF (((MultByBand) AND (Band <> All)) OR
             ((NOT MultByBand) AND (Band = All))) AND
             (((MultByMode) AND (Mode <> Both)) OR
             ((NOT MultByMode) AND (Mode = Both))) THEN WITH Sheet.Multsheet DO
                BEGIN
                IF Totals [Band, Mode].NumberDomesticMults > 0 THEN
                    BEGIN
                    WriteLn (FileWrite, 'Domestic mult list for ', BandString [Band], ModeString [Mode], ' : ');

                    FOR Address := 0 TO Totals [Band, Mode].NumberDomesticMults - 1 DO
                        WriteLn (FileWrite, Address + 1:5, ' ',
                                            ExpandedString (DomesticList [Band, Mode]^ [Address]));
                    WriteLn (FileWrite);
                    END;

                IF Totals [Band, Mode].NumberDXMults > 0 THEN
                    BEGIN

                    WriteLn (FileWrite, 'DX mult list for ', BandString [Band], ModeString [Mode], ' : ');

                    FOR Address := 0 TO Totals [Band, Mode].NumberDXMults - 1 DO
                        WriteLn (FileWrite, Address + 1:5, ' ',
                                            ExpandedString (DXList [Band, Mode]^ [Address]));
                    WriteLn (FileWrite);
                    END;


                IF Totals [Band, Mode].NumberPrefixMults > 0 THEN
                    BEGIN

                    WriteLn (FileWrite, 'Prefix mult list for ', BandString [Band], ModeString [Mode], ' : ');

                    FOR Address := 0 TO Totals [Band, Mode].NumberPrefixMults - 1 DO
                        WriteLn (FileWrite, Address + 1:5, ' ',
                                            ExpandedString (PrefixList [Band, Mode]^ [Address]));
                    WriteLn (FileWrite);
                    END;


                IF Totals [Band, Mode].NumberZoneMults > 0 THEN
                    BEGIN

                    WriteLn (FileWrite, 'Zone mult list for ', BandString [Band], ModeString [Mode], ' : ');

                    FOR Address := 0 TO Totals [Band, Mode].NumberZoneMults - 1 DO
                        WriteLn (FileWrite, Address + 1:5, ' ',
                                            ExpandedString (ZoneList [Band, Mode]^ [Address]));
                    WriteLn (FileWrite);
                    END;
                END;

    WriteLn (FileWrite, 'Number of calls in partial call list = ', NumberPartialCalls);
    WriteLn (FileWrite, 'Number different initial exchange = ', NumberInitialExchanges);

    ExchangeMemoryEnable := NumberInitialExchanges <> 0;

    WriteLn (FileWrite);

    IF NumberPartialCalls > 0 THEN
        BEGIN
        Block := 1;
        BlockAddress := 0;

        WriteLn (FileWrite, 'Partial call list with initial exchanges (if any) : ');

        FOR Address := 0 TO NumberPartialCalls - 1 DO
            BEGIN
            CallSign := ExpandedString (PartialCallList [Block]^ [BlockAddress].Call);

            WriteLn (FileWrite, Address + 1:5, Callsign:12, GetInitialExchange (CallSign):12);

            Inc (BlockAddress);

            IF BlockAddress = FourByteBlockSize THEN
                BEGIN
                BlockAddress := 0;
                Inc (Block);
                END;

            END;
        END;

    WriteLn (FileWrite);

    IF NumberLongPartialCalls > 0 THEN
        BEGIN
        WriteLn (FileWrite, 'Long partial call list with initial exchanges (if any) : ');

        FOR Address := 0 TO NumberLongPartialCalls - 1 DO
            BEGIN
            CallSign := BigExpandedString (LongPartialCallList^ [Address]);
            WriteLn (FileWrite, Address + 1:5, Callsign:14, GetInitialExchange (CallSign):12);
            Inc (BlockAddress);
            END;
        END;

    Close (FileWrite);
    Sheet.DisposeOfMemoryAndZeroTotals;
    WaitForKeyPressed;
    END;



PROCEDURE IncrementCall (VAR Call: CallString);

    BEGIN
    IF Call [Length (Call)] = '9' THEN
        Call [Length (Call)] := 'A'
    ELSE
        IF Call [Length (Call)] = 'Z' THEN
            BEGIN
            Delete (Call, Length (Call), 1);

            IF Call <> '' THEN IncrementCall (Call);

            Call := Call + '0';
            END
        ELSE
            Call [Length (Call)] := Chr ( Ord (Call [Length (Call)]) + 1);

        END;



PROCEDURE TestEveryCall;

VAR Key: CHAR;
    Call, Partial: Str40;

    BEGIN
    REPEAT
        Key := UpCase (GetKey ('Do you want to test every possible prefix (Y/N) : '));
        IF (Key = EscapeKey) OR (Key = 'N') THEN Exit;
    UNTIL (Key = 'Y');
    WriteLn;

    { Save current databse to ASCII file }

    Call := '000';

    REPEAT
        TextColor (Yellow);

        Write (Call, ' = ');

        TextColor (Cyan);

        CD.PartialCallSetup (Call);

        REPEAT
            Partial := CD.GetNextPartialCall;

            IF Partial <> '' THEN
                BEGIN
                IF WhereX + Length (Partial) + 2 >= 80 THEN WriteLn;
                IF WhereX > 1 THEN Write (' ');
                Write (Partial);
                END;

            IF KeyPressed THEN
                BEGIN
                Key := ReadKey;

                IF Key = EscapeKey THEN Exit;
                END;

        UNTIL Partial = '';

        WriteLn;

        IncrementCall (Call);

   UNTIL Call = '000';
   END;



PROCEDURE DTAEditor;


VAR Key: CHAR;
    Data: DataBaseEntryRecord;
    PCall, Call: CallString;
    FileName: Str40;
    Address: INTEGER;

    BEGIN
    REPEAT
        ClrScr;
        TextColor (Yellow);
        WriteLnCenter ('CALL DATABASE UTILITY MENU');
        WriteLn;
        TextColor (Cyan);

        WriteLn ('This menu contains all the commands that are needed to maintain your callsign');
        WriteLn ('database file (TRMASTER.DTA).  This database file is used for storing known');
        WriteLn ('good calls for partial call lookups and various pieces of data that are ');
        WriteLn ('associated to the calls (ie: name, QTH, grid, etc).');
        WriteLn;
        WriteLn ('  A - Convert the TRMASTER.DTA file to an ASCII File (TRMASTER.ASC).');
        WriteLn ('  B - Build TRMASTER.DTA file from TRMASTER.ASC');
        WriteLn ('  C - Clear out one of the TRMASTER data fields.');
        WriteLn ('  D - Delete entries in the database below hit threshold.');
        WriteLn ('  E - Edit data for specific callsigns in database.');
        WriteLn ('  F - Add data to TRMASTER database from various type of files.');
        WriteLn ('  G - Save calls and names to an ASCII file.');
        WriteLn ('  N - Name editor (similar to the old NAMES.CMQ name editor).');
        WriteLn ('  P - Partial calls test - pull partial calls out of databse.');
        WriteLn ('  R - Get random call (used by simulator).');
        WriteLn ('  S - Sort .DTA file so calls are in alphabetical order.');
        WriteLn ('  T - Continually fetch random callsigns until a key is pressed.');
        WriteLn ('  U - Get Unique+1 (possible) calls for a specific call.');
        WriteLn ('  X - Exit menu.');
        WriteLn ('  Y - Test every call in the database.');
        WriteLn ('  Z - Show database statistics.');
        WriteLn;

        Key := UpCase (GetKey ('Enter command : '));

        ClrScr;

        CASE Key OF
            'A': CD.SaveToASCIIFile;
            'B': CD.BuildNewDatabasefromASCIIfile (Chr (0));

            'C': CD.ClearField;

            'D': CD.DeleteLowHitCalls;

            'E': CD.ASCIIFileEditor;

            'F': AddFileToDatabase;

            'G': BEGIN
                 FileName := GetResponse ('Enter filename to save calls and names to : ');
                 IF FileName <> '' THEN
                     CD.SaveCallsAndNamesToFile (FileName);
                 END;

            'N': TRMasterNameEditor;

            'P': WHILE TRUE DO
                     BEGIN
                     Call := UpperCase (GetREsponse ('Enter partial call to test : '));

                     IF Call = '' THEN Break;

                     CD.PartialCallSetup (Call);

                     REPEAT
                         Call := CD.GetNextPartialCall;

                         IF Call <> '' THEN
                             BEGIN
                             IF WhereX + Length (Call) < 79 THEN
                                 BEGIN
                                 IF WhereX > 1 THEN Write (' ');
                                 Write (Call);
                                 END
                             ELSE
                                 BEGIN
                                 WriteLn;
                                 Write (Call);
                                 END;
                             END;
                     UNTIL Call = '';
                     WriteLn;
                     END;

            'R': BEGIN
                 WriteLn (CD.GetRandomCall);
                 WaitForKeyPressed;
                 END;

            'S': CD.SortDTAFile;

            'T': BEGIN
                 REPEAT
                     WriteLn (CD.GetRandomCall)
                 UNTIL KeyPressed;
                 WHILE KeyPressed DO ReadKey;
                 END;

            'U': BEGIN
                 CD.PossibleCallAction          := AnyCall;

                 REPEAT
                     Call := UpperCase (GetResponse ('Enter call to get possible calls for : '));

                     IF Call <> '' THEN
                         BEGIN
                         CD.GeneratePossibleCallList (Call);

                         IF PossibleCallList.NumberPossibleCalls > 0 THEN
                             FOR Address := 0 TO PossibleCallList.NumberPossibleCalls - 1 DO
                                 BEGIN
                                 IF WhereX > 72 THEN WriteLn;
                                 IF WhereX > 1 THEN Write (' ');
                                 Write (PossibleCallList.List [Address].Call);
                                 END;
                         END
                     ELSE
                         Break;

                     WriteLn;
                 UNTIL False;
                 END;

            'V': REPEAT
                     Call := UpperCase (GetResponse ('Enter call to get value of : '));

                     IF Call = '' THEN Break;

                     WriteLn (CallSortValue (Call));
                 UNTIL FAlse;

            'X', EscapeKey:
                    BEGIN
                    ClrScr;
                    Exit;
                    END;

            'Y': TestEveryCall;

            'Z': CD.ShowStatistics;
            END;
    UNTIL False;
    END;



PROCEDURE GetBeamHeadings;

VAR Heading, Country: INTEGER;
    Grid, MyGrid: Str20;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('GET BEAM HEADINGS');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This procedure will let you enter your grid and then ask for beam headings to');
    WriteLn ('other grids.  It will also tell you the distance between them.');
    WriteLn;

    MyGrid := GetResponse ('Enter your home grid : ');

    IF NOT LooksLikeAGrid (MyGrid) THEN
        BEGIN
        ReportError ('That does not look like a grid to me!!');
        Exit;
        END;

    REPEAT
        Grid := GetResponse ('Enter grid to compute beam heading to : ');

        IF Grid = '' THEN Exit;

        IF LooksLikeAGrid (Grid) THEN
            BEGIN
            Write   ('Heading = ', Round (GetBeamHeading (MyGrid, Grid)), '   ');
            WriteLn ('Distance = ', GetDistanceBetweenGrids (MyGrid, Grid), ' km');
            END
        ELSE
            ReportError ('That does not look like a grid to me!!');

    UNTIL False;
    END;



PROCEDURE DateTimeChange;

VAR OutputFileName, InputFileName, DateString, TimeString: Str40;
    LogString: STRING;
    OutputFile, InputFile: TEXT;
    Offset: INTEGER;

    BEGIN
    ClearScreenAndTitle ('DATE / TIME CHANGE FOR A LOG');

    WriteLn ('This procedure will allow you to change the date and time entries in a log');
    WriteLn ('by a fixed number of minutes - either adding or subtracting.  Use a minus');
    WriteLn ('minute offset to subtract time.');
    WriteLn;

    InputFileName := GetResponse ('Enter Input log filename (none to exit) : ');
    IF InputFileName = '' THEN Exit;

    OutputFileName := GetResponse ('Enter Output log filename (none to exit) : ');
    IF OutputFileName = '' THEN Exit;

    Offset := GetValue ('Enter time offset in minutes (zero to abort) : ');

    IF Offset = 0 THEN Exit;

    IF NOT OpenFileForRead (InputFile, InputFileName) THEN
        BEGIN
        ReportError (InputFileName + ' not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    OpenFileForWrite (OutputFile, OutputFileName);

    WHILE NOT Eof (InputFile) DO
        BEGIN
        ReadLn (InputFile, LogString);
        ChangeLogStringDateTime (LogString, Offset);
        WriteLn (OutputFile, LogString);
        END;

    Close (OutputFile);
    Close (InputFile);

    WriteLn ('All done.');
    WaitForKeyPressed;
    END;



FUNCTION UtilityMenu: BOOLEAN;

VAR Key: CHAR;

    BEGIN
    UtilityMenu := True;
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('UTILITY PROGRAM MENU');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('  A - Append program (append LOG.DAT to history files).');
    WriteLn ('  C - Check country and zone for specified callsign.');
    WriteLn ('  D - Date/time change for a log.');
    WriteLn ('  E - Edit TRMASTER.ASC file (menu).');
    WriteLn ('  G - Global log search (list of calls not in a log.');
    WriteLn ('  H - Get beam headings and distance between grids.');
    WriteLn ('  N - NameEdit (old NAMES.CMQ database editor).');
    WriteLn ('  S - Show contents of RESTART.BIN file.');
    WriteLn ('  X - Exit utility program menu.');
    WriteLn;
    TextColor (Cyan);
    Write   ('  Enter command : ');

    REPEAT
        REPEAT UNTIL KeyPressed;
        Key := UpCase (ReadKey);

        CASE Key OF
            'A': BEGIN AppendProcedure;   Exit; END;
            'C': BEGIN CountryCheck;      Exit; END;
            'D': BEGIN DateTimeChange;    Exit; END;
            'E': BEGIN DTAEditor;         Exit; END;
            'G': BEGIN GlobalLogSearch;   Exit; END;
            'H': BEGIN GetBeamHeadings;   Exit; END;
            'N': BEGIN NameEditor;        Exit; END;
            'S': BEGIN ShowRestartDotBin; Exit; END;
            'X', EscapeKey:
                BEGIN
                UtilityMenu := False;
                Exit;
                END;

            END;
    UNTIL False;
    END;



    BEGIN
    END.
