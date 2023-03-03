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

UNIT PostUtl;

{$O+}
{$L curlroutines}
{$linklib curl}
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

FUNCTION urldownload(url, ctyfilename :Pchar):longint;cdecl;external;

FUNCTION UtilityMenu: BOOLEAN;

IMPLEMENTATION
uses keycode,sysutils;

CONST BufferSize = 300;
      CallBufferSize = 1000;

      DaysPerMonth : ARRAY [1..12] of SHORTINT =
                            (031,028,031,030,031,030,031,031,030,031,030,031);

      SecsPerYear:     LONGINT = 31536000;
      SecsPerLeapYear: LONGINT = 31622400;
      SecsPerDay:      LONGINT = 86400;
      SecsPerHour:     INTEGER = 3600;
      SecsPerMinute:   INTEGER = 60;

TYPE
    FileBuffer = ARRAY [0..BufferSize] OF Str160;
    FileBufferPointer = ^FileBuffer;

    CallBuffer = ARRAY [0..CallbufferSize] OF CallString;
    CallBufferPointer = ^CallBuffer;

    SCPArray = ARRAY [0..36, 0..36] OF LONGINT;
    SCPArrayPtr = ^SCPArray;

    CabrilloRecPtr = ^CabrilloRec;

    CabrilloRec = RECORD
        CabrilloString: STRING [100];
        Time:           LONGINT;
        LogsFound:      WORD;           { Bits 0 - 11 true/false }
        PreviousRecord: CabrilloRecPtr;
        NextRecord:     CabrilloRecPtr;
        END;

    NAQPExchangeRecord = RECORD
        Callsign: CallString;
        NumberExchanges: INTEGER;
        ExchangesFound: ARRAY [0..10] OF Str20;
        END;

    NAQPStationDatabaseType = RECORD
        Stations: ARRAY [0..750] OF NAQPExchangeRecord;
        END;

    NAQPStationDatabasePointer = ^NAQPStationDatabaseType;


VAR Buffer: FileBufferPointer;
    CheckCallBuffer: CallBufferPointer;
    CabrilloEntryHead: CabrilloRecPtr;
    NAQPStationDatabase: NAQPStationDatabasePointer;

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
    CWSend: BOOLEAN;
    StartOfCallEntered: INTEGER;
    FileRead: TEXT;

    LastPrefix: Str20;
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

PROCEDURE DownloadCtyFile;
VAR CtyFile, CtyFileOld, Url: string;
    res: longint;
BEGIN
    Url := 'https://www.country-files.com/cty/cty.dat';
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('Download new CTY.DAT file');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('This procedure will download the latest version of CTY.DAT');
    WriteLn ('From ' + Url);
    WriteLn;
    url := url + chr(0);
    setlength(CtyFile,200);
    setlength(CtyFileOld,200);
    CtyFile := FindDirectory ('CTY.DAT') + DirectorySeparator + 'CTY.DAT'
       + chr(0);
    WriteLn ('Current CTY.DAT file is:');
    WriteLn (CtyFile);
    CtyFileOld := Copy(CtyFile,1,length(CtyFile)-4) + 'BCK' + chr(0);
    IF CopyFile(CtyFile,CtyFileOld) then
    Begin
       WriteLn('CTY.DAT successfully backed up to:');
       WriteLn (CtyFileOld);
    End
    Else
    Begin
       WriteLn('Unable to back up CTY.DAT, exiting');
       WaitForKeyPressed;
       Exit;
    End;
    REPEAT
        Key := UpCase (GetKey ('Okay to proceed? (Y/N) : '));
        IF (Key = EscapeKey) OR (Key = 'N') THEN Exit;
    UNTIL Key = 'Y';
    WriteLn;
    WriteLn('Attempting download');
    res := urldownload(@url[1],@CtyFile[1]);
    if (res = 0) then
    Begin
       WriteLn('Download successful');
    End;
    if (res = -1) then
    Begin
       WriteLn('Error opening CTY.DAT file -- Exiting');
    End;
    if (res = -2) then
    Begin
       WriteLn('Error in download -- Copying CTY.BCK to CTY.DAT');
       IF CopyFile(CtyFileOld,CtyFile) then
       Begin
          WriteLn('CTY.DAT successfully copied from CTY.BCK');
       End
       Else
          WriteLn('CTY.DAT not copied from CTY.BCK -- exiting');
    End;
    WaitForKeyPressed;
    Exit;
END;


PROCEDURE ShowRestartDotBin;

VAR FileWrite: TEXT;
    Block, NumberCalls, NumberEntriesInLastBlock, NumberBlocks: INTEGER;
    BlockAddress, CharPointer, EndAddress, Address: INTEGER;

    Band: BandType;
    Mode: ModeType;
    CallSign, TempString: Str20;
    CompressedCall: FourBytes;

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
    Call: CallString;
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

VAR Grid, MyGrid: Str20;

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

VAR OutputFileName, InputFileName: Str40;
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

    IF OutputFileName = InputFileName THEN
        BEGIN
        ReportError ('ERROR - can not use the same filename for both!!');
        Exit;
        END;

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



FUNCTION IsLeapYear (Year: Word): BOOLEAN;

    BEGIN
    IF Year MOD 100 = 0 THEN
        BEGIN
        IF (Year MOD 400 = 0) THEN
            IsLeapYear := True
        ELSE
            IsLeapYear := False;

        Exit;
        END;

    IsLeapYear := Year MOD 4 = 0;
    END;



FUNCTION GetUnixTimeFromCabrilloEntry (CabrilloString: STRING): LONGINT;

{ QSO:  1800 CW 2003-12-06 2353 K7RAT         599 OR     W7SE          599 Wy}

VAR DateString, TimeString, YearString, MonthString, DayString: Str20;
    HourString, MinuteString: Str20;
    Index, Year, Month, Day, Hour, Minute: WORD;
    UnixDate: LONGINT;

    BEGIN
    RemoveFirstString (CabrilloString);  { QSO: }
    RemoveFirstString (CabrilloString);  { 1800 }
    RemoveFirstString (CabrilloString);  { CW }

    DateString := RemoveFirstString (CabrilloString);
    TimeString := RemoveFirstString (CabrilloString);

    YearString  := Copy (DateString, 1, 4);
    MonthString := Copy (DateString, 6, 2);
    DayString   := Copy (DateString, 9, 2);

    HourString   := Copy (TimeString, 1, 2);
    MinuteString := Copy (TimeString, 3, 2);

    Val (YearString,   Year);
    Val (MonthString,  Month);
    Val (DayString,    Day);
    Val (HourString,   Hour);
    Val (MinuteString, Minute);

    UnixDate := 0; {initialize}

    Inc (UnixDate, SecsPerMinute * Minute); { add minutes}
    Inc (UnixDate, SecsPerHour   * Hour);   { add hours}

    { add days }

    Inc (UnixDate, (SecsPerDay * (Day - 1)));

    { We now have how many seconds have passed so far in the month }

    { Figure out how many hours have passed in this year up to the end
      of the previous day }

    IF IsLeapYear (Year) THEN
        DaysPerMonth [02] := 29
    ELSE
        DaysPerMonth [02] := 28; {Check for Feb. 29th}

    { Add in seconds for completed months so far }

    Index := 1;

    IF Month > 1 THEN
        FOR Index := 1 TO Month - 1 DO  {has one month already passed?}
            Inc (UnixDate, (DaysPerMonth [Index] * SecsPerDay));

    { Now do the complete years }

    WHILE Year > 1970 DO
        BEGIN
        IF IsLeapYear (Year - 1) THEN
            Inc (UnixDate, SecsPerLeapYear)
        ELSE
            Inc (UnixDate, SecsPerYear);

       Dec (Year, 1);
       END;

    GetUnixTimeFromCabrilloEntry := UnixDate;
    END;



FUNCTION FileStringsMatch (FStr1, FStr2: STRING): BOOLEAN;

{ Matches all except the last 3 digits of frequency

QSO:  1800 CW 2003-12-06 2348 K7RAT         599 OR     N6RK          599 Sv }

    BEGIN
    Delete (FStr1, 8, 3);
    Delete (FStr2, 8, 3);

    FileStringsMatch := FStr1 = FStr2;
    END;



PROCEDURE AddQSOToLinkedList (FileString: STRING; QSOTime: LONGINT; LogWorkedValue: WORD);

VAR ActiveCabrilloEntry, PreviousCabrilloRecordPtr: CabrilloRecPtr;

    BEGIN
    { See if we have room for another entry }

//    IF MaxAvail < SizeOf (CabrilloRec) THEN
//        BEGIN
//        ReportError ('Out of memory - sorry.');
//        Halt;
//        END;

    { See if this is the first entry in the list }

    IF CabrilloEntryHead = nil THEN { Very first entry }
        BEGIN
        CabrilloEntryHead := New (CabrilloRecPtr);

        CabrilloEntryHead^.CabrilloString := FileString;
        CabrilloEntryHead^.Time           := QSOTime;
        CabrilloEntryHead^.LogsFound      := LogWorkedValue;
        CabrilloEntryHead^.PreviousRecord := nil;
        CabrilloEntryHead^.NextRecord     := nil;

        Exit;
        END;

    ActiveCabrilloEntry := CabrilloEntryHead;
    PreviousCabrilloRecordPtr := ActiveCabrilloEntry;

    { Increment the ActiveCabrilloEntry until we either find a QSO at the
      same time (or later) - or we reach the end of the linked list }

    WHILE (ActiveCabrilloEntry <> nil) AND (ActiveCabrilloEntry^.Time < QSOTime) DO
        BEGIN
        PreviousCabrilloRecordPtr := ActiveCabrilloEntry;  { Remember }
        ActiveCabrilloEntry       := ActiveCabrilloEntry^.NextRecord;
        END;

    { There is one special case here to deal with.  If the entry trying to
      be added should be put at the start of the list.  This can happen if
      the QSO time of the QSO trying to be added is earlier than the first
      entry on the list }

    IF ActiveCabrilloEntry = CabrilloEntryHead THEN
        BEGIN
        CabrilloEntryHead := New (CabrilloRecPtr);

        CabrilloEntryHead^.CabrilloString := FileString;
        CabrilloEntryHead^.Time           := QSOTime;
        CabrilloEntryHead^.LogsFound      := LogWorkedValue;
        CabrilloEntryHead^.PreviousRecord := nil;
        CabrilloEntryHead^.NextRecord     := ActiveCabrilloEntry;

        ActiveCabrilloEntry^.PreviousRecord := CabrilloEntryHead;

        Exit;
        END;

    { We are either pointing to the first entry in the list where the
      times match - or the very end of the list.  Let's take care of
      the case where the QSO simply needs to be added to the end of
      the linked list first }

    IF ActiveCabrilloEntry = nil THEN   { We are at last entry }
        BEGIN
        PreviousCabrilloRecordPtr^.NextRecord := New (CabrilloRecPtr);

        WITH PreviousCabrilloRecordPtr^.NextRecord^ DO
            BEGIN
            Time           := QSOTime;
            LogsFound      := LogWorkedValue;
            CabrilloString := FileString;
            PreviousRecord := PreviousCabrilloRecordPtr;
            NextRecord     := nil;
            END;

        Exit;
        END;

    { We are in the middle of the list - we should see if this QSO is
      already in the list - if so - we just OR in the LogWorkedVale and
      go away }

    WHILE (ActiveCabrilloEntry <> nil) AND (ActiveCabrilloEntry^.Time = QSOTime) DO
        BEGIN
        WITH ActiveCabrilloEntry^ DO
            IF (Time = QSOTime) AND FileStringsMatch (CabrilloString, FileString) THEN
                BEGIN
                LogsFound := LogsFound OR LogWorkedValue;
                Exit;
                END;

        PreviousCabrilloRecordPtr := ActiveCabrilloEntry;  { Remember }
        ActiveCabrilloEntry       := ActiveCabrilloEntry^.NextRecord;
        END;

    { It does not match any of the QSOs with the same time.  We get to add
      the QSO at the PreviousCabrilloRecordPtr since we went past the time
      of the QSO being added }

    PreviousCabrilloRecordPtr^.NextRecord := New (CabrilloRecPtr);

    WITH PreviousCabrilloRecordPtr^.NextRecord^ DO
        BEGIN
        CabrilloString := FileString;
        Time := QSOTime;
        LogsFound := LogWorkedValue;
        PreviousRecord := PreviousCabrilloRecordPtr;
        NextRecord := ActiveCabrilloEntry;
        END;

    IF ActiveCabrilloEntry <> nil THEN
        ActiveCabrilloEntry^.PreviousRecord := PreviousCabrilloRecordPtr^.NextRecord;
    END;



PROCEDURE MergeCabrilloLogs;

VAR Filenames: ARRAY [0..11] OF Str20;
    FileNumber, NumberFiles: INTEGER;
    OutputFileName: Str20;
    Fileread, FileWrite: TEXT;
    LogWorkedValue: WORD;
    FileString: Str160;
    Time: LONGINT;
    ActiveCabrilloEntry: CabrilloRecPtr;
    nofile: boolean;

    BEGIN
    ClearScreenAndTitle ('MERGE CABRILLO LOGS');
    WriteLn ('This routine will merge up to twelve cabrillo files at one time into a single');
    WriteLn ('Cabrillo file.  The contacts will all be sorted into chronological order and');
    WriteLn ('duplicate QSOs found in more than one log will not be saved.');
    WriteLn;

    NumberFiles := 0;

    CabrilloEntryHead := nil;  { Make sure these are nil }

    REPEAT
        Str (NumberFiles, TempString);

        FileNames [NumberFiles] := GetResponse (TempString + '. Enter filename (none to continue) : ');

        nofile := FileNames [NumberFiles] = '';
        IF nofile THEN
            BEGIN
            IF NumberFiles = 0 THEN Exit;
            END
        ELSE
            IF FileExists (FileNames [NumberFiles]) THEN
                Inc (NumberFiles)
            ELSE
                ReportError (FileNames [NumberFiles] + ' not found.');

    UNTIL (NumberFiles = 12) OR nofile;

    OutputFileName := GetResponse ('Enter file to save results to (none to abort) : ');

    IF OutputFileName = '' THEN Exit;

    { Read in each file into the linked list }

    FOR FileNumber := 0 TO NumberFiles - 1 DO
        BEGIN
        GoToXY (1, WhereY);
        ClrEol;

        WriteLn ('Processing file ', FileNames [FileNumber]);

        { Determine binary value to use to set the proper log worked bit in
          the LogsFound word in the Cabrillo records }

        CASE FileNumber OF
            0: LogWorkedValue := $0001;
            1: LogWorkedValue := $0002;
            2: LogWorkedValue := $0004;
            3: LogWorkedValue := $0008;
            4: LogWorkedValue := $0010;
            5: LogWorkedValue := $0020;
            6: LogWorkedValue := $0040;
            7: LogWorkedValue := $0080;
            8: LogWorkedValue := $0100;
            9: LogWorkedValue := $0200;
           10: LogWorkedValue := $0400;
           11: LogWorkedValue := $0800;
           END;

        IF NOT OpenFileForRead (FileRead, FileNames [FileNumber]) THEN
            BEGIN
            ReportError ('Unable to open ' + FileNames [FileNumber]);
            Halt;
            END;

        { Skip over the header }

        REPEAT
            ReadLn (FileRead, FileString);

            IF Copy (Filestring, 1, 4) = 'QSO:' THEN
                BEGIN
                Time := GetUnixTimeFromCabrilloEntry (FileString);
                AddQSOToLinkedList (FileString, Time, LogWorkedValue);

                GoToXY (1, WhereY);
                ClrEol;
                END;

            IF KeyPressed THEN IF ReadKey = EscapeKey THEN
                BEGIN
                GoToXY (1, WhereY);
                ClrEol;
                WriteLn ('Aborted by operator ESCAPE KEY');
                Break;
                END;

        UNTIL Eof (FileRead);

        Close (FileRead);
        END;

    GoToXY (1, WhereY);
    ClrEol;
    WriteLn ('All files read in - now writing output file ', OutputFileName);

    { Okay - now we need to spit out the new log }

    OpenFileForWrite (FileWrite, OutputFileName);

    ActiveCabrilloEntry := CabrilloEntryHead;  { Start at the start }

    WHILE ActiveCabrilloEntry <> nil DO
        BEGIN
        WriteLn (FileWrite, ActiveCabrilloEntry^.CabrilloString);
        ActiveCabrilloEntry := ActiveCabrilloEntry^.NextRecord;
        END;

    Close (FileWrite);
    END;

Procedure AddAdifFreq(Var AdifString:String; FreqMHz: Real);
Var
   FreqStr,LenStr: string;
Begin
   FreqStr := FormatFloat('######.000###',FreqMhz);
   Str(length(FreqStr),LenStr);
   AdifString := AdifString + '<Freq:' + LenStr +'>' + FreqStr;
End;

PROCEDURE ConvertLogStringToADIF (LogString: STRING; VAR ADIFString: STRING);

VAR TempString: STRING;
    Band: BandType;
    Mode: ModeType;
    DateString, TimeString, Call, RSTSent, RSTReceived, LengthString: Str20;
    MonthString, YearString, DayString: STRING [5];
    FreqMhz: Real;
    PossibleFreqString : String;
    xresult: INTEGER;

    BEGIN
    TempString := '';

    GetRidOfPostcedingSpaces (LogString);

    IF LogString <> '' THEN
        BEGIN
        Band       := GetLogEntryBand       (LogString);
        Mode       := GetLogEntryMode       (LogString);
        DateString := GetLogEntryDateString (LogString);
        TimeString := GetLogEntryTimeString (LogString);
        Call       := GetLogEntryCall       (LogString);
        PossibleFreqString := Copy(LogString,23,5);
        FreqMhz := 0.0;
        IF StringHas (PossibleFreqString, '.')  THEN
        Begin
           Val(PossibleFreqString,FreqMHz,xResult);
           if xResult <> 0 then FreqMHz := 0.0;
        end;

        RSTSent     := Copy (LogString, 45, 3);
        RSTReceived := Copy (LogString, 50, 3);

        GetRidOfPostcedingSpaces (RSTSent);
        GetRidOfPostcedingSpaces (RSTReceived);

        IF (Band = NoBand) OR (Mode = NoMode) THEN
            BEGIN
            ADIFString := '';
            Exit;
            END;

        Str (Length (Call), LengthString);

        TempString := '<Call:' + LengthString + '>' + Call;

        CASE Band OF
            BAND160:
               Begin
                  TempString := TempString + '<Band:4>160M';
                  if FreqMHz > 0.0 then AddAdifFreq(TempString,FreqMHz+1.0);
               End;
             BAND80:
               Begin
                  TempString := TempString + '<Band:3>80M';
                  if FreqMHz > 0.0 then AddAdifFreq(TempString,FreqMHz+3.0);
               End;
             BAND40:
                Begin
                  TempString := TempString + '<Band:3>40M';
                  if FreqMHz > 0.0 then AddAdifFreq(TempString,FreqMHz+7.0);
                End;
             BAND30:
                Begin
                  TempString := TempString + '<Band:3>30M';
                  if FreqMHz > 0.0 then AddAdifFreq(TempString,FreqMHz+10.0);
                End;
             BAND20:
                Begin
                  TempString := TempString + '<Band:3>20M';
                  if FreqMHz > 0.0 then AddAdifFreq(TempString,FreqMHz+14.0);
                End;
             BAND17:
                Begin
                  TempString := TempString + '<Band:3>17M';
                  if FreqMHz > 0.0 then AddAdifFreq(TempString,FreqMHz+18.0);
                End;
             BAND15:
                Begin
                  TempString := TempString + '<Band:3>15M';
                  if FreqMHz > 0.0 then AddAdifFreq(TempString,FreqMHz+21.0);
                End;
             BAND12:
                Begin
                  TempString := TempString + '<Band:3>12M';
                  if FreqMHz > 0.0 then AddAdifFreq(TempString,FreqMHz+24.0);
                End;
             BAND10:
                Begin
                  TempString := TempString + '<Band:3>10M';
                  if FreqMHz > 0.0 then AddAdifFreq(TempString,FreqMHz+28.0);
                End;
              BAND6:
                 Begin
                   TempString := TempString + '<Band:2>6M';
                   if FreqMHz > 0.0 then AddAdifFreq(TempString,FreqMHz+50.0);
                 End;
              BAND2:
                 Begin
                   TempString := TempString + '<Band:2>2M';
                   if FreqMHz > 0.0 then AddAdifFreq(TempString,FreqMHz+144.0);
                 End;
            BAND222:
                 Begin
                    TempString := TempString + '<Band:5>1.25M';
                   if FreqMHz > 0.0 then AddAdifFreq(TempString,FreqMHz+222.0);
                 End;
            BAND432:
                 Begin
                   TempString := TempString + '<Band:4>70CM';
                   if FreqMHz > 0.0 then AddAdifFreq(TempString,FreqMHz+432.0);
                 End;
            BAND902:
                 Begin
                   TempString := TempString + '<Band:4>35CM';
                   if FreqMHz > 0.0 then AddAdifFreq(TempString,FreqMHz+902.0);
                 End;
           BAND1296:
                 Begin
                   TempString := TempString + '<Band:4>23CM';
                   if FreqMHz > 0.0 then AddAdifFreq(TempString,FreqMHz+1296.0);
                 End;
           BAND2304:
                 Begin
                   TempString := TempString + '<Band:4>13CM';
                   if FreqMHz > 0.0 then AddAdifFreq(TempString,FreqMHz+2304.0);
                 End;
           BAND3456:
                 Begin
                   TempString := TempString + '<Band:3>9CM';
                   if FreqMHz > 0.0 then AddAdifFreq(TempString,FreqMHz+3456.0);
                 End;
           BAND5760:
              Begin
                 TempString := TempString + '<Band:3>6CM';
                 if FreqMHz > 0.0 then AddAdifFreq(TempString,FreqMHz+5760.0);
              End;
            BAND10G:
               Begin
                 TempString := TempString + '<Band:3>3CM';
                 if FreqMHz > 0.0 then AddAdifFreq(TempString,FreqMHz+10000.0);
               End;
            BAND24G:
               Begin
                 TempString := TempString + '<Band:6>1.25CM';
                 if FreqMHz > 0.0 then AddAdifFreq(TempString,FreqMHz+24000.0);
               End;
            END;



        CASE Mode OF
            CW:      TempString := TempString + '<Mode:2>CW';
            Digital: TempString := TempString + '<Mode:4>RTTY';
            Phone:   TempString := TempString + '<Mode:3>SSB';
            FM:      TempString := TempString + '<Mode:2>FM';
            END;


        DateString := UpperCase (DateString);

        IF StringHas (DateString, 'JAN') THEN MonthString := '01' ELSE
        IF StringHas (DateString, 'FEB') THEN MonthString := '02' ELSE
        IF StringHas (DateString, 'MAR') THEN MonthString := '03' ELSE
        IF StringHas (DateString, 'APR') THEN MonthString := '04' ELSE
        IF StringHas (DateString, 'MAY') THEN MonthString := '05' ELSE
        IF StringHas (DateString, 'JUN') THEN MonthString := '06' ELSE
        IF StringHas (DateString, 'JUL') THEN MonthString := '07' ELSE
        IF StringHas (DateString, 'AUG') THEN MonthString := '08' ELSE
        IF StringHas (DateString, 'SEP') THEN MonthString := '09' ELSE
        IF StringHas (DateString, 'OCT') THEN MonthString := '10' ELSE
        IF StringHas (DateString, 'NOV') THEN MonthString := '11' ELSE
        IF StringHas (DateString, 'DEC') THEN MonthString := '12';

        IF Copy (DateString, 8, 1) >= '8' THEN
            YearString := '19' + Copy (DateString, 8, 2)
        ELSE
            YearString := '20' + Copy (DateString, 8, 2);

        DayString := Copy (DateString, 1, 2);

        TempString := TempString + '<QSO_Date:8>' + YearString + MonthString + DayString;

        Delete (TimeString, 3, 1);

        TempString := TempString + '<Time_on:4>' + TimeString;

        IF StringIsAllNumbers (RSTSent) AND StringIsAllNumbers (RSTReceived) THEN
            BEGIN
            IF (Length (RSTSent) = 2) AND (Length (RSTReceived) = 2) THEN
               BEGIN
               TempString := TempString + '<RST_SENT:2>' + RSTSent;
               TempString := TempString + '<RST_RCVD:2>' + RSTReceived;
               END;

            IF (Length (RSTSent) = 3) AND (Length (RSTReceived) = 3) THEN
               BEGIN
               TempString := TempString + '<RST_SENT:3>' + RSTSent;
               TempString := TempString + '<RST_RCVD:3>' + RSTReceived;
               END;
            END;

        TempString := TempString + '<eor>';
        END;

    ADIFString := TempString;
    END;



PROCEDURE ADIFConvert;

VAR OutputFileName, InputFileName: Str40;
    LogString, ADIFString: STRING;
    OutputFile, InputFile: TEXT;
    NumberQSOs: LONGINT;

    BEGIN
    ClearScreenAndTitle ('ADIF CONVERSION TOOL');

    WriteLn ('This procedure will convert a TR Log file to an ADIF file.  The band, mode,');
    WriteLn ('date, time and callsign will be converted.  If RSTs are found in the log,');
    WriteLn ('they will be converted as well.');
    WriteLn;

    InputFileName := GetResponse ('Enter Input log filename (none to exit) : ');
    IF InputFileName = '' THEN Exit;

    OutputFileName := GetResponse ('Enter ADIF output filename (none to exit) : ');
    IF OutputFileName = '' THEN Exit;

    IF OutputFileName = InputFileName THEN
        BEGIN
        ReportError ('ERROR - can not use the same filename for both!!');
        Exit;
        END;

    IF NOT OpenFileForRead (InputFile, InputFileName) THEN
        BEGIN
        ReportError (InputFileName + ' not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    OpenFileForWrite (OutputFile, OutputFileName);
    WriteLn (OutputFile,'<ADIF_VER:5>2.2.1');
    WriteLn (OutputFile,'<EOH>');

    NumberQSOs := 0;

    WHILE NOT Eof (InputFile) DO
        BEGIN
        ReadLn (InputFile, LogString);
        ConvertLogStringToADIF (LogString, ADIFString);

        IF ADIFString <> '' THEN
            BEGIN
            WriteLn (OutputFile, ADIFString);
            Inc (NumberQSOs);
            END;
        END;

    Close (OutputFile);
    Close (InputFile);

    WriteLn ('All done.  There were ', NumberQSOs, ' QSOs converted.');
    WaitForKeyPressed;
    END;



PROCEDURE ConvertCabrilloToTR;

VAR InputFileName, OutputFileName: Str40;
    FileRead, FileWrite: TEXT;
    NumberQSOs: INTEGER;
    FileString: STRING;
    Frequency: LONGINT;
    NumberSent, NumberReceived: Str20;
    Band: BandType;
    Mode: ModeType;
    MonthNumber, DateString, TimeString, RSTSent, RSTReceived: Str20;
    CallWorked, TempString: Str40;
    TRLogString: STRING;

    BEGIN
    NumberQSOs := 0;

    ClearScreenAndTitle ('CONVERT CABRILLO LOG TO TR FORMAT');

    WriteLn ('This procedure will convert a Cabrillo log to something close to a TR .DAT');
    WriteLn ('file.  It was designed for the WPX contest.');
    WriteLn;

    InputFileName := GetResponse ('Enter input filename (none to abort) : ');

    IF InputFileName = '' THEN Exit;

    IF NOT OpenFileForRead (FileRead, InputFileName) THEN
        BEGIN
        ReportError (InputFileName + ' does not exist.');
        WaitForKeyPressed;
        Exit;
        END;

    OutputFileName := GetResponse ('Enter output filename (none to abort) :');

    IF OutputFileName = '' THEN Exit;

    OpenFileForWrite (FileWrite, OutputFileName);

    WHILE NOT Eof (FileRead) DO
        BEGIN
        ReadLn (FileRead, FileString);

        TempString := RemoveFirstString (FileString);

        WriteLn (TempString);

        IF TempString <> 'QSO:' THEN Continue;

        { Read frequency }

        Frequency := RemoveFirstLongInteger (FileString);

        IF Frequency < 3000 THEN Band := Band160 ELSE
          IF Frequency < 6000 THEN Band := Band80 ELSE
            IF Frequency < 8000 THEN Band := Band40 ELSE
              IF Frequency < 11000 THEN Band := Band30 ELSE
                IF Frequency < 15000 THEN Band := Band20 ELSE
                  IF Frequency < 20000 THEN Band := Band17 ELSE
                    IF Frequency < 22000 THEN Band := Band15 ELSE
                      IF Frequency < 25000 THEN Band := Band12 ELSE
                          Band := Band10;

        TempString := RemoveFirstString (FileString);

        IF TempString = 'CW' THEN Mode := CW ELSE
          IF TempString = 'PH' THEN Mode := Phone ELSE
            Mode := Digital;

        TempString := RemoveFirstString (FileString);

        DateString := Copy (TempString, Length (TempString) - 1, 2);

        MonthNumber := Copy (TempString, 6, 2);

        IF MonthNumber = '01' THEN DateString := DateString + '-Jan-' ELSE
        IF MonthNumber = '02' THEN DateString := DateString + '-Feb-' ELSE
        IF MonthNumber = '03' THEN DateString := DateString + '-Mar-' ELSE
        IF MonthNumber = '04' THEN DateString := DateString + '-Apr-' ELSE
        IF MonthNumber = '05' THEN DateString := DateString + '-May-' ELSE
        IF MonthNumber = '06' THEN DateString := DateString + '-Jun-' ELSE
        IF MonthNumber = '07' THEN DateString := DateString + '-Jul-' ELSE
        IF MonthNumber = '08' THEN DateString := DateString + '-Aug-' ELSE
        IF MonthNumber = '09' THEN DateString := DateString + '-Sep-' ELSE
        IF MonthNumber = '10' THEN DateString := DateString + '-Oct-' ELSE
        IF MonthNumber = '11' THEN DateString := DateString + '-Nov-' ELSE
        IF MonthNumber = '12' THEN DateString := DateString + '-Dec-';

        DateString := DateString + Copy (TempString, 3, 2);

        TimeString := RemoveFirstString (FileString);
        Insert (':', TimeString, 3);

        RemoveFirstString (FileString); { Dump my call }

        RSTSent := RemoveFirstString (FileString);
        NumberSent := RemoveFirstString (FileString);

        CallWorked := RemoveFirstString (FileString);

        RSTReceived := RemoveFirstString (FileString);
        NumberReceived := RemoveFirstString (FileString);

        WHILE Length (NumberSent) < 5 DO NumberSent := ' ' + NumberSent;

        WHILE Length (CallWorked) < 14 DO CallWorked := CallWorked + ' ';

        WHILE Length (RSTSent) < 3 DO RSTSent := RSTSent + ' ';

        WHILE Length (RSTReceived) < 3 DO RSTReceived := RSTReceived + ' ';

        WHILE Length (NumberReceived) < 5 DO NumberReceived := ' ' + NumberReceived;

        TRLogString := BandString [Band] + ModeString [Mode] + ' ' +
                       DateString + ' ' + TimeString + NumberSent + '  ' +
                       CallWorked + RSTSent + '  ' + RSTReceived + NumberReceived;

        WHILE Length (TRLogString) < 77 DO TRLogString := TRLogString + ' ';

        TRLogString := TRLogString + '1';

        WriteLn (FileWrite, TRLogString);

        Inc (NumberQSOs);
        END;

    Close (FileWrite);
    Close (FileRead);

    WriteLn ('There were ', NumberQSOs, ' QSOs written to ', OutputFileName);
    WaitForKeyPressed;
    END;



PROCEDURE ADIFToTRLog;

VAR InputFileName, FieldCommand, OutputFileName: Str40;
    FileRead, FileWrite: TEXT;
    StringLength, NumberQSOs: INTEGER;
    FileString: STRING;
    QSONumberString: Str20;
    DateString, TimeString, RSTSent, RSTReceived, Callsign: Str20;
    TRLogString: STRING;
    MonthString, YearString, FieldID, LengthString: Str40;
    BandString, ModeString: Str20;

    BEGIN
    NumberQSOs := 0;

    ClearScreenAndTitle ('CONVERT ADIF FILE TO TR FORMAT');

    WriteLn ('This procedure will convert a ADIF file to something close to a TR .DAT');
    WriteLn ('file.  It only supports RST exchange information.');
    WriteLn;

    InputFileName := GetResponse ('Enter input filename (none to abort) : ');

    IF InputFileName = '' THEN Exit;

    IF NOT OpenFileForRead (FileRead, InputFileName) THEN
        BEGIN
        ReportError (InputFileName + ' does not exist.');
        WaitForKeyPressed;
        Exit;
        END;

    OutputFileName := GetResponse ('Enter output filename (none to abort) :');

    IF OutputFileName = '' THEN Exit;

    OpenFileForWrite (FileWrite, OutputFileName);

    NumberQSOs := 0;

    WHILE NOT Eof (FileRead) DO
        BEGIN
        ReadLn (FileRead, FileString);

        Callsign := '';

        WHILE FileString <> '' DO
            BEGIN
            FieldCommand := BracketedString (FileString, '<', '>');

            IF FieldCommand = 'EOR' THEN Break;

            Delete (FileString, 1, Length (FieldCommand) + 2);

            FieldID := UpperCase (PrecedingString (FieldCommand, ':'));
            LengthString := PostcedingString (FieldCommand, ':');

            Val (LengthString, StringLength);

            IF FieldID = 'CALL' THEN
                BEGIN
                Callsign := Copy (FileString, 1, StringLength);
                Delete (FileString, 1, StringLength);
                END;

            IF FieldID = 'BAND' THEN
                BEGIN
                BandString := Copy (FileString, 1, StringLength);
                Delete (FileString, 1, StringLength);
                Delete (BandString, Length (BandString), 1);  { get rid of M }
                END;

            IF FieldID = 'MODE' THEN
                BEGIN
                ModeString := Copy (FileString, 1, StringLength);
                Delete (FileString, 1, StringLength);
                END;

            IF FieldID = 'QSO_DATE' THEN
                BEGIN
                DateString := Copy (FileString, 1, StringLength);
                Delete (FileString, 1, StringLength);

                YearString := Copy (DateString, 1, 4);
                Delete (DateString, 1, 4);
                YearString := Copy (YearString, 3, 2);

                MonthString := Copy (DateString, 1, 2);
                Delete (DateString, 1, 2);

                IF MonthString = '01' THEN DateString := DateString + '-Jan-' ELSE
                IF MonthString = '02' THEN DateString := DateString + '-Feb-' ELSE
                IF MonthString = '03' THEN DateString := DateString + '-Mar-' ELSE
                IF MonthString = '04' THEN DateString := DateString + '-Apr-' ELSE
                IF MonthString = '05' THEN DateString := DateString + '-May-' ELSE
                IF MonthString = '06' THEN DateString := DateString + '-Jun-' ELSE
                IF MonthString = '07' THEN DateString := DateString + '-Jul-' ELSE
                IF MonthString = '08' THEN DateString := DateString + '-Aug-' ELSE
                IF MonthString = '09' THEN DateString := DateString + '-Sep-' ELSE
                IF MonthString = '10' THEN DateString := DateString + '-Oct-' ELSE
                IF MonthString = '11' THEN DateString := DateString + '-Nov-' ELSE
                IF MonthString = '12' THEN DateString := DateString + '-Dec-';

                DateString := DateString + YearString;
                END;


            IF FIeldID = 'TIME_ON' THEN
                BEGIN
                TimeString := Copy (FileString, 1, StringLength);

                IF StringLength = 6 THEN Delete (TimeString, 5, 2);

                WHILE Length (TimeString) < 4 DO TimeString := '0' + TimeString;
                Insert (':', TimeString, 3);
                Delete (FileString, 1, StringLength);
                END;


            IF FieldID = 'RST_SENT' THEN
                BEGIN
                RSTSent := Copy (FileString, 1, StringLength);
                Delete (FileString, 1, StringLength);
                END;

            IF FieldID = 'RST_RCVD' THEN
                BEGIN
                RSTReceived := Copy (FileString, 1, StringLength);
                Delete (FileString, 1, StringLength);
                END;


            IF FieldID = 'EOR' THEN Break;
            END;

        IF Callsign <> '' THEN
            BEGIN
            Inc (NumberQSOs);

            WHILE Length (BandString) < 3  DO BandString := ' ' + BandString;
            WHILE Length (ModeString) < 4  DO ModeString := ModeString + ' ';
            WHILE Length (DateString) < 10 DO DateString := DateString + ' ';
            WHILE Length (Callsign)   < 15 DO Callsign   := Callsign + ' ';

            Str (NumberQSOs:5, QSONumberString);

            TRLogString := BandString + ModeString + DateString + TimeString + QSONumberString;
            TRLogString := TRLogString + '  ' + Callsign;

            WHILE Length (RSTSent)     < 3 DO RSTSent := RSTSent + ' ';
            WHILE Length (RSTReceived) < 3 DO RSTReceived := RSTReceived + ' ';

            TRLogString := TRLogString + RSTSent + '  ' + RSTReceived;

            WHILE Length (TRLogString) < 77 DO TRLogString := TRLogString + ' ';

            TRLogString := TRLogString + '1';

            WriteLn (FileWrite, TRLogString);
            END;
        END;

    Close (FileWrite);
    Close (FileRead);

    WriteLn ('There were ', NumberQSOs, ' QSOs written to ', OutputFileName);
    WaitForKeyPressed;
    END;



PROCEDURE NAQPExchangeChecker;

{ Checks the log file from the NAQP contest to make sure the exchanges
  for all of the QSOs with a specific station are consistent }

LABEL StationFound;

VAR FileName: Str40;
    FileString: STRING;
    Call, Name, QTH: Str20;
    Exchange, NumberStations, Entry: INTEGER;
    FileRead: TEXT;

    BEGIN
    ClearScreenAndTitle ('NAQP EXCHANGE CHECKER');
    WriteLn;
    WriteLn ('This procedure will look at your NAQP log and see if the exchanges');
    WriteLn ('are all consistent');
    WriteLn;

    FileName := GetResponse ('Enter file to process (none to abort) : ');

    IF FileName = '' THEN Exit;

    IF NOT OpenFileForRead (FileRead, FileName) THEN
        BEGIN
        WriteLn ('Unable to open that file');
        WaitForKeyPressed;
        Exit;
        END;

    New (NAQPStationDatabase);

    WITH NAQPStationDatabase^ DO
        BEGIN
        NumberStations:= 0;

        { Suck up the data into the database }

        WHILE NOT Eof (FileRead) DO
            BEGIN
            ReadLn (FileRead, FileString);
            GetRidOfPrecedingSpaces (FileString);

            IF NOT StringHas (FileString, '-') THEN Continue;
            IF NOT StringHas (FileSTring, ':') THEN Continue;

            RemoveFirstString (FileString);  { 10CW }
            RemoveFirstString (FileString);  { 10CW }
            RemoveFirstString (FileString);  { 10CW }
            RemoveFirstString (FileString);  { 10CW }

            Call := RemoveFirstString (FileString);
            Name := RemoveFirstString (FileString);
            QTH  := RemoveFirstString (FileString);

            { See if we have this entry already in the database }

            IF NumberStations > 0 THEN
                FOR Entry := 0 TO NumberStations - 1 DO
                    WITH Stations [Entry] DO
                        IF Callsign = Call THEN
                            BEGIN
                            ExchangesFound [NumberExchanges] := Name + ' ' + QTH;
                            Inc (NumberExchanges);
                            Goto StationFound;
                            END;

                { Callsign not worked before - add new entry }

            WITH Stations [NumberStations] DO
                BEGIN
                Callsign := Call;
                ExchangesFound [0] := Name + ' ' + QTH;
                NumberExchanges := 1;
                END;

            Inc (NumberStations);
  StationFound:
            END;

        WriteLn ('There were ', NumberStations, ' stations found');

        { Now look at the data and highlight any exchanges that are inconsistent }

        IF NumberStations = 0 THEN
            BEGIN
            WriteLn ('There were no QSOs found');
            WaitForKeyPressed;
            Exit;
            END;

        FOR Entry := 0 TO NumberStations - 1 DO
            WITH Stations [Entry] DO
                IF NumberExchanges > 1 THEN
                    FOR Exchange := 1 TO NumberExchanges - 1 DO
                        IF ExchangesFound [0] <> ExchangesFound [Exchange] THEN
                            BEGIN
                            WriteLn ('Issue found with ', Callsign);
                            WriteLn (ExchangesFound [0], ' ', ExchangesFound [Exchange]);
                            END;

         END;

    Dispose (NAQPStationDatabase);
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
    WriteLn ('  B - ADIF file to TR Log file convert.');
    WriteLn ('  C - Check country and zone for specified callsign.');
    WriteLn ('  D - Date/time change for a log.');
    WriteLn ('  E - Edit TRMASTER.ASC file (menu).');
    WriteLn ('  F - ADIF convert tool.');
    WriteLn ('  G - Global log search (list of calls not in a log.');
    WriteLn ('  H - Get beam headings and distance between grids.');
    WriteLn ('  L - Convert Cabrillo Log to TR Log.');
    WriteLn ('  M - Merge Cabrillo files into single file.');
    WriteLn ('  N - NameEdit (old NAMES.CMQ database editor).');
    WriteLn ('  Q - NAQP exchange checker');
    WriteLn ('  S - Show contents of RESTART.BIN file.');
    WriteLn ('  Y - Download new country file.');
    WriteLn ('  X - Exit utility program menu.');
    WriteLn;
    TextColor (Cyan);
    Write   ('  Enter command : ');

    REPEAT
        REPEAT UNTIL KeyPressed;
        Key := UpCase (ReadKey);

        CASE Key OF
            'A': BEGIN AppendProcedure;      Exit; END;
            'B': BEGIN ADIFToTRLog;          Exit; END;
            'C': BEGIN CountryCheck;         Exit; END;
            'D': BEGIN DateTimeChange;       Exit; END;
            'E': BEGIN DTAEditor;            Exit; END;
            'F': BEGIN ADIFConvert;          Exit; END;
            'G': BEGIN GlobalLogSearch;      Exit; END;
            'H': BEGIN GetBeamHeadings;      Exit; END;
            'L': BEGIN ConvertCabrilloToTR;  Exit; END;
            'M': BEGIN MergeCabrilloLogs;    Exit; END;
            'N': BEGIN NameEditor;           Exit; END;
            'Q': BEGIN NAQPExchangeChecker;  Exit; END;
            'Y': BEGIN DownloadCtyFile;      Exit; END;
            'S': BEGIN ShowRestartDotBin;    Exit; END;

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

