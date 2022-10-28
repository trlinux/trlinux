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

UNIT PostSCP;

{$O+}
{$V-}

INTERFACE

USES Country9,
     LogSort,
     LogSCP,
     Tree,
     SlowTree,
     trCrt,
     DOS;

    PROCEDURE AddFileToDatabase;
    PROCEDURE ExportOldCallsFromASCIIFile;
    PROCEDURE ReadDataFromAnotherDTAFile;
    PROCEDURE TRMasterNameEditor;



IMPLEMENTATION
uses memlinux,keycode;

FUNCTION CheckForOverwriteUpdates: BOOLEAN;

VAR Key: CHAR;

    BEGIN
    REPEAT
        Key := UpCase (GetKey ('Do you want to check the overwrite flag status? (Y/N) : '));

        IF Key = EscapeKey THEN
            BEGIN
            CheckForOverwriteUpdates := False;
            WriteLn;
            Exit;
            END;

    UNTIL (Key = 'Y') OR (Key = 'N');
    WriteLn;

    IF Key = 'Y' THEN
        BEGIN
        CheckForOverwriteUpdates := CD.OverwriteFlagStatus;
        WriteLn;
        END;
    END;



PROCEDURE ExportOldCallsFromASCIIFile;

VAR FileString, FileName: Str80;
    FileRead: TEXT;
    CallCount: INTEGER;
    Data: DatabaseEntryRecord;
    SaveNewCalls: BOOLEAN;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('UPDATE DATABASE WITH NEW CALLSIGNS');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This procedure will update callsigns in the database with new callsigns.');
    WriteLn ('The previous call will be stored in the OldCall field.  If the old call');
    WriteLn ('was not found in a database, a new record will be created for the new');
    WriteLn ('call if you want.');
    WriteLn;
    WriteLn ('The input file format is an ASCII list with old call followed by a space,');
    WriteLn ('then the new call.');
    WriteLn;

    FileName := GetResponse ('Enter callsign change file to process (none to quit) : ');
    IF FileName = '' THEN Exit;

    IF NOT OpenFileForRead (FileRead, FileName) THEN
        BEGIN
        ReportError (FileName + ' was not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    SaveNewCalls := 'Y' = GetKeyResponse ('Save new calls which were not found in the database? (Y, N) : ');

    CD.FirstMergeDataListEntry := nil;

    IF NOT CD.ASCIIFileIsCurrent THEN CD.SaveToASCIIFile;

    REPEAT
        TextColor (Yellow);
        WriteLn ('Reading data from ', FileName, ' into memory...');
        CallCount := 0;

        WHILE (NOT Eof (FileRead)) AND (MemAvail - 100000 > SizeOf (DataListEntryRecord)) DO
            BEGIN
            CD.ClearDataEntry (Data);
            ReadLn (FileRead, FileString);

            Data.OldCall := UpperCase (RemoveFirstString (FileString));
            Data.Call    := UpperCase (RemoveFirstString (FileString));

            IF Data.Call <> '' THEN
                BEGIN
                CD.AddRecordToMergeList (Data);
                Inc (CallCount);
                IF CallCount MOD 10 = 0 THEN PinWheel;
                END;
            END;

        TextColor (Cyan);
        GoToXY (1, WhereY);
        ClrEol;
        IF Eof (FileRead) THEN Write ('All ');

        WriteLn (CallCount, ' entries read from ', FileName, ' into memory.');

        CD.TransferMergeDataToASCIIFile (True, SaveNewCalls);
    UNTIL Eof (FileRead);

    Close (FileRead);
    CD.BuildNewDatabaseFromASCIIFile (Chr (0));
    END;



PROCEDURE ShowTypicalLogEntry (FileName: Str80);

VAR FileRead: TEXT;
    FileString: Str80;

    BEGIN
    IF OpenFileForRead (FileRead, FileName) THEN
        BEGIN
        WHILE NOT Eof (FileRead) DO
            BEGIN
            ReadLn (FileRead, FileString);
            ExpandTabs (FileString);

            IF (GetLogEntryBand (FileString) <> NoBand) AND
               (GetLogEntryMode (FileString) <> NoMode) AND
               (GoodCallSyntax (GetLogEntryCall (FileString))) THEN
                   BEGIN
                   WriteLn ('Here is a typical log entry to help you compute cursor positions:');
                   WriteLn;
                   WriteLn (FileString);
                   WriteLn ('123456789112345678921234567893123456789412345678951234567896123456789712345678');
                   WriteLn ('         0         0         0         0         0         0         0 ');
                   WriteLn;
                   Close (FileRead);
                   Exit;
                   END

            END;
        Close (FileRead);
        END;
    END;



PROCEDURE MoveDataIntoField (DataString: Str20; FieldKey: CHAR; VAR Data: DataBaseEntryRecord);

    BEGIN
    CASE FieldKey OF
        '1': BEGIN
             IF Length (DataString) > 5 THEN DataString := Copy (DataString, 1, 5);
             Data.Section := DataString;
             END;

        '2': BEGIN
             IF Length (DataString) > 6 THEN DataString := Copy (DataString, 1, 6);
             IF LooksLikeAGrid (DataString) THEN Data.Grid    := DataString;
             END;

        '3': BEGIN
             IF Length (DataString) > 2 THEN DataString := Copy (DataString, 1, 2);
             Data.Check   := DataString;
             END;

        '4': BEGIN
             IF Length (DataString) > 10 THEN DataString := Copy (DataString, 1, 10);
             Data.QTH     := DataString;
             END;

        '5': BEGIN
             IF Length (DataString) > 2 THEN DataString := Copy (DataString, 1, 2);
             Data.CQZone  := DataString;
             END;

        '6': BEGIN
             IF Length (DataString) > 5 THEN DataString := Copy (DataString, 1, 5);
             Data.ITUZone := DataString;
             END;

        '7': BEGIN
             Data.Name := DataString;
             IF Length (DataString) > 12 THEN DataString := Copy (DataString, 1, 12);
             END;

        '8': BEGIN
             IF StringIsAllNumbers (DataString) THEN
             Val (DataString, Data.Speed);
             END;

        '9': BEGIN
             IF Length (DataString) > 5 THEN DataString := Copy (DataString, 1, 5);
             Data.FOC  := DataString;
             END;

        'A': BEGIN
             IF Length (DataString) > 6 THEN DataString := Copy (DataString, 1, 6);
             Data.TenTen  := DataString;
             END;

        'B': BEGIN
             IF Length (DataString) > 12 THEN DataString := Copy (DataString, 1, 12);
             Data.User1 := DataString;
             END;

        'C': BEGIN
             IF Length (DataString) > 12 THEN DataString := Copy (DataString, 1, 12);
             Data.User2 := DataString;
             END;

        'D': BEGIN
             IF Length (DataString) > 12 THEN DataString := Copy (DataString, 1, 12);
             Data.User3   := DataString;
             END;

        'E': BEGIN
             IF Length (DataString) > 12 THEN DataString := Copy (DataString, 1, 12);
             Data.User4   := DataString;
             END;

        'F': BEGIN
             IF Length (DataString) > 12 THEN DataString := Copy (DataString, 1, 12);
             Data.User5   := DataString;
             END;
        END;
    END;



PROCEDURE GeneralPurposeLogFileExtract;

{ This procedure is intended to allow any exchange data field to be moved
  from a log file into the TRMASTER database, along with adding new calls. }

TYPE SearchInfoType = RECORD
         CursorPosition: SHORTINT;
         IgnoreCount:    SHORTINT;
         FieldKey:       CHAR;
         END;

VAR NumberSearchItems: INTEGER;
    SearchItemData: ARRAY [0..10] OF SearchInfoType;
    TempKey, Key: CHAR;
    DoingLogFile: BOOLEAN;
    DataString, FileName: Str40;
    FileRead: TEXT;
    FileString: Str160;
    Data: DatabaseEntryRecord;
    CallCursorPosition, CallCount, Count: INTEGER;
    Ignore: INTEGER;
    TotalCallCount: LONGINT;

    BEGIN
    TotalCallCount := 0;
    NumberSearchItems := 0;

    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('GENERAL PURPOSE FILE TO TRMASTER PROCEDURE');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This procedure allows you to pull data from an ASCII file and put it into your');
    WriteLn ('TRMASTER database.  The file can be either be a LOG.DAT format file, or an');
    WriteLn ('ASCII file.');
    WriteLn;
    WriteLn ('This procedure will add any new callsigns found into the database, along with');
    WriteLn ('one data parameter.  If there is more than one type of data that you wish to');
    WriteLn ('to pull out of the log, you will have to run this procedure again for each');
    WriteLn ('type of data. (ie: once for names and again for QTHs).');
    WriteLn;
    WriteLn ('Old data will be over-written if the overwrite flag for the data field is set');
    WriteLn ('to true.  Execute the O command on the menu to view/edit these flags.');
    WriteLn;

    REPEAT
        FileName := GetResponse ('Enter the name of the file to process (none to quit) : ');
        IF FileName = '' THEN Exit;

        IF NOT FileExists (FileName) THEN
            ReportError (FileName + ' not found!!');

    UNTIL FileExists (FileName);

    REPEAT
        Key := UpCase (GetKey ('Is this a TR log file? (Y/N) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'Y') OR (Key = 'N');
    WriteLn;

    DoingLogFile := Key = 'Y';

    WriteLn;

    REPEAT
        TextColor (Cyan);

        IF DoingLogFile THEN
            BEGIN
            ShowTypicalLogEntry (FileName);

            WriteLn ('You have selected TR log file format.  I will find the callsigns of the ');
            WriteLn ('stations (calls always start character position 30), but I need you to ');
            WriteLn ('specify the character position where the data you want to pull from the');
            WriteLn ('log starts.  Only entries with a valid looking call, band and mode will');
            WriteLn ('be processed.  Data will be copied starting at the specified position ');
            WriteLn ('until a space is found.  If there is a space found at the position you ');
            WriteLn ('select, the data is assumed to be blank.');
            WriteLn;
            WriteLn ('Also, I can get fancy and ignore a certain number of entries starting at');
            WriteLn ('this position.  You would need to do this if you wanted to access the');
            WriteLn ('third entry in the exchange, and the first two entries were not always the');
            WriteLn ('same length.');
            WriteLn;

            SearchItemData [NumberSearchItems].CursorPosition :=
                            GetValue ('Enter character position to start looking at data from (zero for none): ');

            IF SearchItemData [NumberSearchItems].CursorPosition >= 80 THEN Exit;

            IF SearchItemData [NumberSearchItems].CursorPosition > 0 THEN
                SearchItemData [NumberSearchItems].IgnoreCount :=
                    GetValue ('Enter number of entries to ignore (zero for none) : ');
            END
        ELSE
            BEGIN
            WriteLn ('You can use most any ASCII format with this routine.  The only requirement');
            WriteLn ('is that the callsign and data must appear on the same line and there must');
            WriteLn ('be one entry per line.  Lines with no data - or calls that does not look');
            WriteLn ('like a callsign will be ignored.  You will need to specify the character');
            WriteLn ('positions to use for the callsign and any data you want to save.  If there');
            WriteLn ('is a space found at the data position, the data is assumed to be blank.');
            WriteLn;
            WriteLn ('Also, I can get fancy and ignore a certain number of entries starting at');
            WriteLn ('the data position.  You would need to do this if you wanted to access the');
            WriteLn ('third entry in the exchange, and the first two entries were not always the');
            WriteLn ('same length.');
            WriteLn;

            IF OpenFileForRead (FileRead, FileName) THEN
                BEGIN
                REPEAT
                    ReadLn (FileRead, FileString);
                    ExpandTabs (FileString);
                    GetRidOfPrecedingSpaces (FileString);
                UNTIL Eof (FileRead) OR (FileString <> '');

                IF FileString <> '' THEN
                    BEGIN
                    WriteLn (FileString);
                    WriteLn ('123456789112345678921234567893123456789412345678951234567896123456789712345678');
                    WriteLn ('         0         0         0         0         0         0         0 ');
                    WriteLn;
                    END;

                Close (FileRead);
                END;

            CallCursorPosition := GetValue ('Enter cursor position for callsign : ');
            SearchItemData [NumberSearchItems].CursorPosition :=
                    GetValue ('Enter cursor position to start looking for data (zero for none) : ');

            IF SearchItemData [NumberSearchItems].CursorPosition > 0 THEN
                SearchItemData [NumberSearchItems].IgnoreCount :=
                    GetValue ('Enter number of entries to ignore (zero for none) : ');
            END;

        TextColor (Cyan);
        WriteLn;

        IF SearchItemData [NumberSearchItems].CursorPosition > 0 THEN
            BEGIN
            WriteLn ('Here are the available places to save your data.  Choose none if you only');
            WriteLn ('want to process the callsigns : ');
            WriteLn;
            WriteLn ('0 - None            5 - CQ Zone         A - TenTen Number   F - User 5');
            WriteLn ('1 - ARRL Section    6 - ITU Zone        B - User 1');
            WriteLn ('2 - Grid            7 - Name            C - User 2');
            WriteLn ('3 - Check           8 - Speed           D - User 3');
            WriteLn ('4 - QTH             9 - FOC Number      E - User 4');
            WriteLn;

            REPEAT
                TempKey := UpCase (GetKey ('Select field (0-12 or escape to quit) : '));
                IF TempKey = EscapeKey THEN Exit;

            UNTIL (TempKey = '1') OR (TempKey = '2') OR (TempKey = '3') OR
                  (TempKey = '4') OR (TempKey = '5') OR (TempKey = '6') OR
                  (TempKey = '7') OR (TempKey = '8') OR (TempKey = '9') OR
                  (TempKey = '0') OR (TempKey = 'A') OR (TempKey = 'B') OR
                  (TempKey = 'C') OR (TempKey = 'D') OR (TempKey = 'E') OR
                  (TempKey = 'F');

            SearchItemData [NumberSearchItems].FieldKey := TempKey;
            Inc (NumberSearchItems);

            IF NumberSearchItems > 10 THEN Break;

            REPEAT
                TempKey := UpCase (GetKey ('Do you want to process another data field at the same time? (Y/N) : '));
                IF TempKey = EscapeKey THEN Exit;
            UNTIL (TempKey = 'Y') OR (TempKey = 'N');

            WriteLn;
            END
        ELSE
            TempKey := 'N';

    UNTIL TempKey = 'N';

    IF NOT CheckForOverwriteUpdates THEN Exit;

    IF NOT CD.ASCIIFileIsCurrent THEN CD.SaveToASCIIFile;

    CD.FirstMergeDataListEntry := nil;

    IF NOT OpenFileForRead (FileRead, Filename) THEN Exit;

    REPEAT
        TextColor (Yellow);
        WriteLn ('Reading data from ', FileName, ' into memory...');
        CallCount := 0;

        WHILE (NOT Eof (FileRead)) AND (MemAvail - 100000 > SizeOf (DataListEntryRecord)) DO
            BEGIN
            CD.ClearDataEntry (Data);

            ReadLn (FileRead, FileString);
            ExpandTabs (FileString);

            IF DoingLogFile THEN
                BEGIN
                IF (GetLogEntryBand (FileString) <> NoBand) THEN
                    IF (GetLogEntryMode (FileString) <> NoMode) THEN
                        IF (GoodCallSyntax (GetLogEntryCall (FileString))) THEN
                            BEGIN
                            Inc (CallCount);
                            IF CallCount MOD 10 = 0 THEN PinWheel;

                            Data.Call := GetLogEntryCall (FileString);

                            IF NumberSearchItems > 0 THEN
                                FOR Count := 0 TO NumberSearchItems - 1 DO
                                    WITH SearchItemData [Count] DO
                                        BEGIN
                                        IF (CursorPosition > 0) AND (FieldKey <> '0') THEN
                                            BEGIN
                                            DataString := Copy (FileString, CursorPosition, 40);

                                            IF DataString [1] <> ' ' THEN
                                                BEGIN
                                                IF IgnoreCount > 0 THEN
                                                    FOR Ignore := 1 TO IgnoreCount DO
                                                        RemoveFirstString (DataString);

                                                DataString := RemoveFirstString (DataString);
                                                MoveDataIntoField (DataString, FieldKey, Data);
                                                END;
                                            END;
                                        END;

                            CD.AddRecordToMergeList (Data);
                            END;
                END
            ELSE
                BEGIN
                GetRidOfPrecedingSpaces (FileString);

                IF FileString <> '' THEN
                    BEGIN
                    DataString := Copy (FileString, CallCursorPosition, 12);
                    Data.Call  := UpperCase (RemoveFirstString (DataString));

                    IF GoodCallSyntax (Data.Call) THEN
                        BEGIN
                        Inc (CallCount);
                        IF CallCount MOD 10 = 0 THEN PinWheel;

                        IF NumberSearchItems > 0 THEN
                            FOR Count := 0 TO NumberSearchItems - 1 DO
                                WITH SearchItemData [Count] DO
                                    BEGIN
                                    IF (CursorPosition > 0) AND (FieldKey <> '0') THEN
                                        BEGIN
                                        DataString := Copy (FileString, CursorPosition, 40);

                                        IF DataString [1] <> ' ' THEN
                                            BEGIN
                                            IF IgnoreCount > 0 THEN
                                                FOR Ignore := 1 TO IgnoreCount DO
                                                    RemoveFirstString (DataString);

                                            DataString := RemoveFirstString (DataString);
                                            MoveDataIntoField (DataString, FieldKey, Data);
                                            END;
                                        END;
                                    END;

                        CD.AddRecordToMergeList (Data);
                        END;
                    END;
                END;

            GoToXY (1, WhereY);
            END;

        TextColor (Cyan);
        GoToXY (1, WhereY);
        ClrEol;
        IF Eof (FileRead) THEN Write ('All ');

        WriteLn (CallCount, ' entries read from ', FileName, ' into memory.');

        IF CallCount > 0 THEN
            CD.TransferMergeDataToASCIIFile (False, True);

        TotalCallCount := TotalCallCount + CallCount;
    UNTIL Eof (FileRead);

    Close (FileRead);

    IF TotalCallCount > 0 THEN
        CD.BuildNewDatabaseFromASCIIFile (Chr (0));
    END;



PROCEDURE ReadDataFromAnotherDTAFile;

VAR RememberActiveFileName, RememberASCIIFileName: Str80;
    FileString, FileName: Str80;
    FileRead: TEXT;
    CallCount: INTEGER;
    Data: DatabaseEntryRecord;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('TRANSFER DATA FROM ANOTHER DTA FILE TO TRMASTER DATABASE');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This procedure will take data from another .DTA file (MASTER OR TRMASTER) and');
    WriteLn ('put it into the TRMASTER.DTA file.  Data for existing callsigns will be over-');
    WriteLn ('written depending on the state of the over-write flags.');
    WriteLn;

    FileName := GetResponse ('Enter name of database file to read (none to quit): ');
    IF FileName = '' THEN Exit;

    IF NOT FileExists (FileName) THEN
        BEGIN
        ReportError (FileName + ' was not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    IF NOT CheckForOverwriteUpdates THEN Exit;

    { Save the current TRMASTER.DTA file to ascii.  We use this file to
      get the data from to compare to the merge list. }

    IF NOT CD.ASCIIFileIsCurrent THEN
        BEGIN
        CD.SaveToASCIIFile;

        { Get rid of current .DTA file's index array }

        Close (CD.TRMasterFileRead);
        Dispose (CD.SCPIndexArray);
        CD.IndexArrayAllocated := False;
        CD.TRMasterFileOpen := False;
        END;

    { Save the input .DTA file to DTATEMP.ASC in ASCII format. }

    { Remember the filenames that the current .DTA file uses }

    RememberActiveFileName := CD.ActiveFileName;
    RememberASCIIFileName  := CD.ActiveASCIIFileName;

    { Point to .DTA file to be merged and temporary ASCII file }

    CD.ActiveFileName      := FileName;
    CD.ActiveASCIIFileName := 'DTATEMP.ASC';

    { Go get input .DTA file's index array }

    IF NOT CD.LoadInIndexArray THEN
        BEGIN
        CD.ActiveFileName      := RememberActiveFileName;
        CD.ActiveASCIIFileName := RememberASCIIFileName;
        Exit;
        END;

    { Save data to DTATEMP.ASC file in ASCII }

    CD.SaveToASCIIFile;

    { Close the index array }

    Close (CD.TRMasterFileRead);
    Dispose (CD.SCPIndexArray);
    CD.IndexArrayAllocated := False;
    CD.TRMasterFileOpen := False;

    { Restore normal filenames }

    CD.ActiveFileName      := RememberActiveFileName;
    CD.ActiveASCIIFileName := RememberASCIIFileName;

    { Open up the input ASCII file }

    IF NOT OpenFileForRead (FileRead, CD.WorkingDirectory + 'DTATEMP.ASC')
        THEN Exit;

    { Initialize first merge data pointer }

    CD.FirstMergeDataListEntry := nil;

    { Suck data from the input ASCII file into the merge array }

    WHILE NOT Eof (FileRead) DO
        BEGIN
        TextColor (Yellow);
        WriteLn ('Reading ASCII data for ', FileName, ' into memory...');
        CallCount := 0;

        WHILE (NOT Eof (FileRead)) AND (MemAvail - 100000 > SizeOf (DataListEntryRecord)) DO
            BEGIN
            CD.ClearDataEntry (Data);
            ReadLn (FileRead, FileString);

            IF FileString <> '' THEN
                BEGIN
                CD.GetDataFromASCIIEntry (FileString, Data, Chr (0));
                CD.AddRecordToMergeList (Data);
                Inc (CallCount);
                IF CallCount MOD 10 = 0 THEN PinWheel;
                END;

            END;

        TextColor (Cyan);
        GoToXY (1, WhereY);
        ClrEol;
        IF Eof (FileRead) THEN Write ('All ');

        WriteLn (CallCount, ' entries read from ', FileName, ' into memory.');

        { Go create a new ASCII file with merged data included }

        CD.TransferMergeDataToASCIIFile (False, True);
        END;

    Close (FileRead);

    CD.BuildNewDatabaseFromASCIIFile (Chr (0));
    DeleteFile (CD.WorkingDirectory + 'DTATEMP.ASC');
    END;



PROCEDURE ExportNamesFromASCIIFile;

VAR FileString, FileName: Str80;
    FileRead: TEXT;
    CallCount: INTEGER;
    Data: DatabaseEntryRecord;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('ASCII NAME FILE TO TRMASTER DATABASE');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This procedure will take an ASCII file with calls and names and move the data');
    WriteLn ('into the TRMASTER.DTA file.  This file can be generated from the old name');
    WriteLn ('database files using the name editor.  The format is simply a callsign,');
    WriteLn ('followed by a space, then the name, then a new line.');
    WriteLn;

    FileName := GetResponse ('ASCII file with names and calls to process (none to quit) : ');
    IF FileName = '' THEN Exit;

    IF NOT OpenFileForRead (FileRead, FileName) THEN
        BEGIN
        ReportError (FileName + ' was not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    IF NOT CheckForOverwriteUpdates THEN Exit;

    IF NOT CD.ASCIIFileIsCurrent THEN CD.SaveToASCIIFile;

    CD.FirstMergeDataListEntry := nil;

    REPEAT
        TextColor (Yellow);
        WriteLn ('Reading data from ', FileName, ' into memory...');
        CallCount := 0;

        WHILE (NOT Eof (FileRead)) AND (MemAvail - 100000 > SizeOf (DataListEntryRecord)) DO
            BEGIN
            CD.ClearDataEntry (Data);
            ReadLn (FileRead, FileString);
            Data.Call := RemoveFirstString (FileString);
            Data.Name := RemoveFirstString (FileString);
            CD.AddRecordToMergeList (Data);
            Inc (CallCount);
            IF CallCount MOD 10 = 0 THEN PinWheel;
            END;

        TextColor (Cyan);
        GoToXY (1, WhereY);
        ClrEol;
        IF Eof (FileRead) THEN Write ('All ');

        WriteLn (CallCount, ' entries read from ', FileName, ' into memory.');

        CD.TransferMergeDataToASCIIFile (False, True);
    UNTIL Eof (FileRead);

    Close (FileRead);
    CD.BuildNewDatabaseFromASCIIFile (Chr (0));
    END;



PROCEDURE AddFileToDatabase;

VAR Key: CHAR;

    BEGIN
    IF MemAvail <= 150000 THEN
        BEGIN
        ReportError ('Insufficient memory to function.  Please free up some memory and retry.');
        WaitForKeyPressed;
        Exit;
        END;

    REPEAT
        ClrScr;
        TextColor (Yellow);
        WriteLnCenter ('ADD DATA FROM FILE TO TRMASTER.DTA');
        TextColor (Cyan);

        WriteLn;
        WriteLn ('  D - Load calls and data from another .DTA file.');
        WriteLn ('  F - Load calls and data from a LOG.DAT or other ASCII file.');
        WriteLn ('  N - Load ASCII file generated from NAMES file by name editor.');
        WriteLn ('  O - Overwrite data flag status - view and set.');
        WriteLn ('  V - Vanity callsign update procedure.');
        WriteLn ('  X - Exit menu.');
        WriteLn;

        Key := UpCase (GetKey ('Enter desired operation (escape to exit) : '));
        IF Key = EscapeKey THEN Exit;

        CASE Key OF
            'D': ReadDataFromAnotherDTAFile;
            'F': GeneralPurposeLogFileExtract;
            'N': ExportNamesFromASCIIFile;
            'O': CD.OverwriteFlagStatus;
            'V': ExportOldCallsFromASCIIFile;
            'X': Exit;
            END;

    UNTIL False;
    END;



PROCEDURE ShowNameEditorCommands;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('TRMASTER NAME EDITOR PROGRAM');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This editor allows you to quickly edit the names in your TRMASTER database.');
    WriteLn;
    WriteLn ('Since the TRMASTER.DTA file can''t be edited quickly, this procedure will');
    WriteLn ('stop occasionally to update the files.  Because of this, the changes you');
    WriteLn ('had just typed in will not take effect until this process occurs.');
    WriteLn;
    WriteLn ('Changes are first made to the TRMASTER.ASC file.  They are not saved in');
    WriteLn ('the TRMASTER.DTA file until the end of this procedure.');
    WriteLn;
    WriteLn ('Single letter commands: C - Toggle CW sending of names.');
    WriteLn ('                        D - Delete a call from the database.');
    WriteLn ('                        F - Make ASCII file with calls and names.');
    WriteLn ('                        X - Quit editing and save changes.');
    WriteLn;
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



PROCEDURE TRMasterNameEditor;

VAR FileName: Str80;
    CWSend: BOOLEAN;
    StartOfCallEntered: INTEGER;
    Name, Call, LastPrefix: Str20;
    TempData: DataBaseEntryRecord;

    BEGIN
    CD.NameOverwrite := True;

    ShowNameEditorCommands;

    CD.FirstMergeDataListEntry := nil;

    IF NOT CD.ASCIIFileIsCurrent THEN CD.SaveToASCIIFile;

    LastPrefix := '';

    CWSend := True;
    CD.NumberDeleteCalls := 0;

    REPEAT
        GoToXY (1, WhereY);
        ClrEol;

        TextColor (Cyan);

        Write ('Enter call to edit : ');

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

                     IF Call <> '' THEN
                         BEGIN
                         CD.DeleteCallList [CD.NumberDeleteCalls] := Call;
                         Inc (CD.NumberDeleteCalls);

                         IF CD.NumberDeleteCalls >= 20 THEN
                             BEGIN
                             WriteLn ('Please wait - I need to update files.');
                             CD.TransferMergeDataToASCIIFile (False, True);
                             END;
                         END;

                     END;

                'E': BEGIN
                     GoToXY (40, WhereY - 1);
                     Call := UpperCase (GetResponse ('Callsign to change name of : '));

                     IF GoodCallSyntax (Call) THEN
                         BEGIN
                         Name := GetResponse ('Old name = ' + CD.GetName (Call) + '.  Enter new name for ' + Call + ' : ');

                         IF Name <> '' THEN
                             BEGIN
                             CD.ClearDataEntry (TempData);
                             TempData.Call := Call;
                             TempData.Name := Name;

                             CD.AddRecordToMergeList (TempData);

                             IF MemAvail < 100000 THEN
                                 BEGIN
                                 WriteLn;
                                 WriteLn ('Please wait, I need to update the files now...');
                                 CD.TransferMergeDataToASCIIFile (False, True);
                                 END;
                             END;
                         END
                     ELSE
                         WriteLn ('Sorry - that call looks funny to me.');
                     END;

                'F': BEGIN
                     GoToXY (40, WhereY -1);
                     FileName := UpperCase (GetResponse ('Filename for list : '));
                     IF FileName <> '' THEN CD.SaveCallsAndNamestoFile (FileName);
                     END;

                'X': BEGIN
                     WriteLn ('Okay - give me a minute to update the files.');
                     CD.TransferMergeDataToASCIIFile (False, True);
                     CD.BuildNewDatabaseFromASCIIFile (Chr (0));
                     WriteLn ('All done!!');
                     WaitForKeyPressed;
                     CD.NameOverwrite := False;
                     Exit;
                     END;

                ELSE
                    ShowNameEditorCommands;

                END;  { of single letter command case }
            END

        ELSE

            { More than one character }

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

            IF GoodCallSyntax (Call) THEN
                BEGIN
                IF CD.GetName (Call) = '' THEN
                    BEGIN
                    Name := UpperCase (GetResponse ('Name = '));

                    IF Name = '' THEN
                        BEGIN
                        GoToXY (1, WhereY - 1);
                        ClrEol;
                        END
                    ELSE
                        IF Name <> '' THEN
                            BEGIN
                            CD.ClearDataEntry (TempData);
                            TempData.Call := Call;
                            TempData.Name := Name;

                            CD.AddRecordToMergeList (TempData);

                            IF MemAvail < 100000 THEN
                                BEGIN
                                WriteLn;
                                WriteLn ('Please wait, I need to update the files now...');
                                CD.TransferMergeDataToASCIIFile (False, True);
                                END;
                            END;
                    END
                ELSE
                    BEGIN
                    TextColor (Cyan);
                    Write ('Name = ');
                    TextColor (Red);
                    Name := CD.GetName (Call);
                    WriteLn (Name);
                    IF CWSend THEN SendMorse (Name);
                    END;
                END
            ELSE
                BEGIN
                TextColor (Red);
                WriteLn ('That call does not look right!');
                END;

            END;
    UNTIL False;
    END;



    BEGIN
    END.
