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

UNIT PostQSL;

{$O+}
{$V-}

INTERFACE

Uses Tree,
     SlowTree,
     PostSubs,
     Country9,
     LogDupe,
     trCrt,
     LogWind,
     LogSCP;

FUNCTION QSLProcedureMenu: BOOLEAN;

IMPLEMENTATION
uses keycode;

 { QSL Procedues }

TYPE QSOListType = ARRAY [0..200] OF Str80;
     QSOListPointer = ^QSOListType;

VAR QSOList: QSOListPointer;


PROCEDURE SetUpSpaces;

VAR Count, NumberSpaces: INTEGER;

    BEGIN
    TextColor (Cyan);
    WriteLn ('You can select how many blank spaces show up in front of each line printed');
    WriteLn ('on your QSL labels.  The program used to always put 19 spaces in front of');
    WriteLn ('each line, but now you can select the number.');
    WriteLn;

    NumberSpaces := GetValue ('Enter number of spaces to start each line with : ');

    Spaces := '';

    IF NumberSpaces > 0 THEN
        FOR Count := 1 TO NumberSpaces DO
            Spaces := Spaces + ' ';

    END;



PROCEDURE ComputeCountryTotals (VAR CountryTotalArray: CountryTotalArrayType; VAR TotalQSOs: INTEGER);

VAR Call, FileString: Str80;
    FileRead: TEXT;
    Country: INTEGER;
    Band: BandType;

    BEGIN
    TotalQSOs := 0;
    FOR Country := 0 TO CountryTable.NumberCountries -1 DO
        CountryTotalArray [Country] := 0;

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError (LogFileName + ' has disappeared!!');
        WaitForKeyPressed;
        Exit;
        END;

    WHILE NOT Eof (FileRead) DO
        BEGIN
        IF OperatorEscape THEN
            BEGIN
            Close (FileRead);
            FOR Country := 0 TO CountryTable.NumberCountries - 1 DO
                CountryTotalArray [Country] := 0;
            TotalQSOs := 0;
            Exit;
            END;

        ReadLn (FileRead, FileString);

        Band := GetLogEntryBand (FileString);

        IF Band <> NoBand THEN
            BEGIN
            ExpandTabs (FileString);
            Call := GetLogEntryCall (FileString);
            Inc (CountryTotalArray [CountryTable.GetCountry (Call, True)]);
            Inc (TotalQSOs);
            END;
        END;

    Close (FileRead);
    END;



PROCEDURE AddQSLDataToLabels (Call: CallString; Data: ContactRecord);

VAR NumberBlocks, Block, Address, FinishAddress: INTEGER;

    BEGIN
    NumberBlocks := QSLData.NumberLabels DIV QSLLabelBlockSize + 1;

    IF QSLData.NumberLabels > 0 THEN
        FOR Block := 1 TO NumberBlocks DO
            BEGIN
            IF Block = NumberBlocks THEN
                FinishAddress := (QSLData.NumberLabels - 1) MOD QSLLabelBlockSize
            ELSE
                FinishAddress := QSLLabelBlockSize - 1;

            FOR Address := 0 TO FinishAddress DO
                IF QSLData.Blocks [Block]^ [Address].CallSign = Call THEN
                    WITH QSLData.Blocks [Block]^ [Address] DO
                        IF NumberContacts < NumberContactsPerLabel THEN
                            BEGIN
                            Inc (NumberContacts);
                            ContactArray [NumberContacts] := Data;
                            Exit;
                            END;
            END;

    Address := QSLData.NumberLabels MOD QSLLabelBlockSize;

    IF Address = 0 THEN
        BEGIN
        IF QSLData.NumberLabels >= (QSLLabelBlockSize * MaxNumberBlocks) THEN
            BEGIN
            ReportError ('Out of memory!!  ' +  Call + ' not added.');
            WaitForKeyPressed;
            Exit;
            END;

        New (QSLData.Blocks [QSLData.NumberLabels DIV QSLLabelBlockSize + 1]);
        END;

    WITH QSLData.Blocks [NumberBlocks]^ [Address] DO
        BEGIN
        CallSign := Call;
        NumberContacts := 1;
        ContactArray [NumberContacts] := Data;
        END;

    Inc (QSLData.NumberLabels);
    END;



PROCEDURE ClearQSLDataMemory;

VAR Block, NumberBlocks: INTEGER;

    BEGIN
    NumberBlocks := QSLData.NumberLabels DIV QSLLabelBlockSize + 1;
    FOR Block := 1 TO NumberBlocks DO
        Dispose (QSLData.Blocks [Block]);
    END;



PROCEDURE SetUpPointerArray;

VAR Address: INTEGER;

    BEGIN
    New (PointerArray);

    FOR Address := 0 TO QSLData.NumberLabels DO
        PointerArray^ [Address] := Address;
    END;



PROCEDURE SortQSLLabelData;

VAR Address, Temp, NumberCallsToSort, AddressA, AddressB: INTEGER;
    BlockA, BlockB, Index, BubbleCount: INTEGER;

    BEGIN
    NumberCallsToSort := QSLData.NumberLabels;

    IF NumberCallsToSort <= 1 THEN Exit;

    Index := NumberCallsToSort - 2;

    FOR BubbleCount := 1 TO NumberCallsToSort - 1 DO
        BEGIN
        FOR Address := 0 TO Index DO
            BEGIN
            AddressA := PointerArray^ [Address];
            AddressB := PointerArray^ [Address + 1];
            BlockA := AddressA DIV QSLLabelBlockSize + 1;
            BlockB := AddressB DIV QSLLabelBlockSize + 1;
            AddressA := AddressA MOD QSLLabelBlockSize;
            AddressB := AddressB MOD QSLLabelBlockSize;

            IF QSLData.Blocks [BlockA]^ [AddressA].Callsign > QSLData.Blocks [BlockB]^ [AddressB].Callsign THEN
                BEGIN
                Temp := PointerArray^ [Address + 1];
                PointerArray^ [Address + 1] := PointerArray^ [Address];
                PointerArray^ [Address] := Temp;
                END;

            END;
        Dec (Index);
        END;
    END;



PROCEDURE CreateSmallCountryTempFile (CountryTotalArray: CountryTotalArrayType);

VAR FileRead, FileWrite: TEXT;
    Call, FileString: Str80;
    Band: BandType;

    BEGIN
    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError (LogFileName + ' has disappeared!!');
        WaitForKeyPressed;
        Exit;
        END;

    IF NOT OpenFileForWrite (FileWrite, QSLTempFileName) THEN
        BEGIN
        ReportError ('Unable to open ' + QSLTempFileName);
        Close (FileRead);
        WaitForKeyPressed;
        Exit;
        END;

    WHILE NOT Eof (FileRead) DO
        BEGIN
        IF OperatorEscape THEN
            BEGIN
            Close (FileRead);
            Close (FileWrite);
            DeleteFile (QSLTempFileName);
            Exit;
            END;

        ReadLn (FileRead, FileString);

        Band := GetLogEntryBand (FileString);

        IF Band <> NoBand THEN
            BEGIN
            ExpandTabs (FileString);
            Call := StandardCallFormat (GetLogEntryCall (FileString), True);
            IF CountryTotalArray [CountryTable.GetCountry (Call, True)] < 20 THEN
                WriteLn (FileWrite, FileString);
            END;
        END;

    Close (FileWrite);
    Close (FileRead);
    END;



PROCEDURE ConfirmContacts;

TYPE QSOEntryRecord = RECORD
         Call: CallString;
         NumberQSOs: INTEGER;
         EntryIndexList: ARRAY [0..20] OF INTEGER;
         END;

VAR Call, StationsName, NewName, FileString: Str80;
    DoingNames: BOOLEAN;
    CallAddress, Entry, NumberQSOEntries, NumberCalls: INTEGER;
    CallList: ARRAY [0..20] OF QSOEntryRecord;
    FileCallString: CallString;
    NameFileWrite: TEXT;
    FileRead: TEXT;

    BEGIN
    Clrscr;
    TextColor (Yellow);
    WriteLnCenter ('CONFIRM CONTACT PROCEDURE');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This procedure will allow you to search for individual callsigns in the');
    WriteLn ('active log and see all the contacts you have made with that station.  Up');
    WriteLn ('to 20 different stations may be processed at once.');
    WriteLn;
    WriteLn ('You can also be shown the stations name if it exists in the name database,');
    WriteLn ('and be given a chance to enter or correct his name.');
    WriteLn;

    IF NOT FileExists (LogFileName) THEN
        BEGIN
        ReportError ('No logfile file found.');
        WaitForKeyPressed;
        Exit;
        END;

    DoingNames := 'Y' = GetKeyResponse ('Do you want to enter names? (Y, N) : ');

    IF DoingNames THEN
        BEGIN
        WriteLn ('New names will be saved in the file NAMES.NEW.  You can add this to your');
        WriteLn ('TRMASTER database with the POST U E F N command.');
        OpenFileForAppend (NameFileWrite, 'NAMES.NEW');
        END;

    New (QSOList);

    REPEAT
        NumberCalls := 0;
        ClrScr;

        REPEAT
            IF NumberCalls = 0 THEN
                Call := UpperCase (GetResponse ('Enter a call to confirm QSO of (none to stop) : '))
            ELSE
                Call := UpperCase (GetResponse ('Enter more calls to check at the same time or none to end list : '));

            IF (Call = '') AND (NumberCalls = 0) THEN
                BEGIN
                IF DoingNames THEN
                    BEGIN
                    Write ('Saving new names to NAMES.NEW.');
                    Close (NameFileWrite);
                    END;

                Dispose (QSOList);
                Exit;
                END;

            IF Call <> '' THEN
                BEGIN
                GoToXY (1, WhereY - 1);
                ClrEol;
                TextColor (Cyan);
                Write (Call);

                CallList [NumberCalls].Call := RootCall (Call);
                CallList [NumberCalls].NumberQSOs := 0;
                Inc (NumberCalls);

                IF (DoingNames AND GoodCallSyntax (Call)) THEN
                    BEGIN
                    GoToXY (20, WhereY);
                    StationsName := CD.GetName (Call);

                    IF StationsName = '' THEN
                        BEGIN
                        TextColor (Cyan);
                        NewName := GetResponse ('Enter name : ');

                        IF NewName <> '' THEN
                            WriteLn (NameFileWrite, Call, ' ', NewName);
                        END
                    ELSE
                        BEGIN
                        Write ('Name = ', StationsName);
                        IF CWEnable THEN SendMorse (StationsName);

                        GoToXY (45, WhereY);

                        NewName := GetResponse ('Enter new name if any : ');

                        IF NewName <> '' THEN
                            WriteLn (NameFileWrite, Call, ' ', NewName);
                        END;
                    END
                ELSE
                    WriteLn;
                END;
        UNTIL (Call = '') OR (NumberCalls = 20);

        GoToXY (1, WhereY - 1);
        ClrEol;
        WriteLn;

        TextColor (Cyan);

        IF NumberCalls > 1 THEN
            WriteLn ('Searching your log for QSOs with ', NumberCalls, ' stations...')
        ELSE
            WriteLn ('Searching your log for QSOs with 1 station...');

        NumberQSOEntries := 0;

        IF NOT OpenFileForRead (FileRead, LogFileName) THEN
            BEGIN
            ReportError ('Unable to open ' + LogFileName + '!!  It was there just a minute ago.');
            IF DoingNames THEN Close (NameFileWrite);
            WaitForKeyPressed;
            Exit;
            END;

        WHILE (NOT Eof (FileRead)) AND (NOT OperatorEscape) DO
            BEGIN
            ReadLn (FileRead, FileString);
            FileCallString := RootCall (GetLogEntryCall (FileString));

            FOR CallAddress := 0 TO NumberCalls - 1 DO
                IF FileCallString = CallList [CallAddress].Call THEN
                    IF CallList [CallAddress].NumberQSOs < 20 THEN
                        BEGIN
                        WITH CallList [CallAddress] DO
                            BEGIN
                            EntryIndexList [NumberQSOs] := NumberQSOEntries;
                            Inc (NumberQSOs);
                            END;

                        ExpandTabs (FileString);
                        QSOList^ [NumberQSOEntries] := FileString;
                        Inc (NumberQSOEntries);
                        TextColor (Cyan);
                        Write ('+');
                        END
                    ELSE
                        BEGIN
                        TextColor (Red);
                        Write ('!');
                        END;
            END;

        Close (FileRead);

        IF NumberQSOEntries = 0 THEN
            BEGIN
            WriteLn ('Sorry...  There were no QSOs found.');
            WriteLn;
            WaitForKeyPressed;
            END
        ELSE
            FOR CallAddress := 0 TO NumberCalls - 1 DO
                BEGIN
                ClrScr;
                TextColor (Yellow);
                WriteLnCenter ('QSOs with ' + CallList [CallAddress].Call);
                TextColor (Cyan);
                WriteLn;

                IF CallList [CallAddress].NumberQSOs = 0 THEN
                    WriteLn ('No QSOs were found with ', CallList [CallAddress].Call)
                ELSE
                    FOR Entry := 0 TO CallList [CallAddress].NumberQSOs - 1 DO
                        WriteLn (QSOList^ [CallList [CallAddress].EntryIndexList [Entry]]);
                WriteLn;
                WaitForKeyPressed;
                END;

    UNTIL False;
    END;



PROCEDURE EstimateNumberOfLabels;

VAR Key: CHAR;
    QSOsPerLabel, NumberQSOs, SearchAddress, CallAddress: INTEGER;
    DifferentCallsigns, TotalLabels: INTEGER;
    QSODistribution: ARRAY [1..100] OF INTEGER;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('ESTIMATE NUMBER OF QSL LABELS');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This procedure will estimate the number of QSL labels that will be printed   ');
    WriteLn ('when using the three QSO per label procedure.  You can specify if you want a');
    WriteLn ('maximum of 2 or 3 QSOs per label.  This procedure does not generate the labels.');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('You can print up to three QSOs per label.  However, if you want to always   ');
    WriteLn ('print at least the first signature, you can limit the number of lines to two.');
    WriteLn ('If you want to always print both signatures, you can limit the number of');
    WriteLn ('lines to one.');
    WriteLn;
    WriteLn ('This procedure will also show you how many times you have worked each station');
    WriteLn ('in your log.  This is useful for scoring contests where bonus points are');
    WriteLn ('given for working a station on 5 or 6 bands.  Make sure you dupe your log');
    WriteLn ('before doing this.');
    WriteLn;

    IF NOT FileExists (LogFileName) THEN
        BEGIN
        ReportError ('No logfile file found.');
        WaitForKeyPressed;
        Exit;
        END;

    REPEAT
        Key := UpCase (GetKey ('Enter the maximum number of QSOs per label (1-6) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key >= '1') AND (Key <= '6');
    WriteLn;

    CASE Key OF
        '1': QSOsPerLabel := 1;
        '2': QSOsPerLabel := 2;
        '3': QSOsPerLabel := 3;
        '4': QSOsPerLabel := 4;
        '5': QSOsPerLabel := 5;
        '6': QSOsPerLabel := 6;
        END;

    ClrScr;
    TextColor (Cyan);
    WriteLn;
    TotalLabels := 0;
    DifferentCallsigns := 0;
    FOR NumberQSOs := 1 TO 100 DO QSODistribution [NumberQSOs] := 0;

    IF PutLogFileIntoCallBuffer (Both, All) THEN
        BEGIN
        IF NumberBufferCalls > 0 THEN
            BEGIN
            WriteLn ('Looking through buffer and counting labels.');

            FOR CallAddress := 0 TO NumberBufferCalls - 1 DO
                BEGIN
                Call := GetCall (CallAddress);
                NumberQSOs := 0;

                IF Call <> 'DUPE' THEN
                    BEGIN
                    Inc (NumberQSOs);
                    Inc (DifferentCallsigns);

                    IF CallAddress < NumberBufferCalls - 2 THEN
                        FOR SearchAddress := CallAddress + 1 TO NumberBufferCalls - 1 DO
                            IF Call = GetCall (SearchAddress) THEN
                                BEGIN
                                Inc (NumberQSOs);
                                PutCall (SearchAddress, 'DUPE');
                                END;

                    TotalLabels := TotalLabels + (NumberQSOs - 1) DIV QSOsPerLabel + 1;

                    Inc (QSODistribution [NumberQSOs]);
                    GoToXY (1, WhereY);
                    ClrEol;
                    Write (TotalLabels);
                    END;
                END;

            DisposeCallBuffer;
            END;
        END
    ELSE
        BEGIN
        ReportError ('Unable to find log file!!');
        WaitForKeyPressed;
        Exit;
        END;

    ClrScr;

    WriteLn;
    TextColor (Cyan);

    WriteLn ('There are ', TotalLabels, ' labels to be printed with ',
             QSOsPerLabel, ' max QSOs per label.');

    WriteLn;
    WriteLn ('There are ', DifferentCallsigns, ' different callsigns.  This is how many QSL cards you will');
    WriteLn ('need if you put multiple labels for the same station on one QSL card.');
    WriteLn;

    FOR NumberQSOs := 1 TO 100 DO
        IF QSODistribution [NumberQSOs] > 0 THEN
            IF NumberQSOs = 1 THEN
                WriteLn ('There are ', QSODistribution [1], ' calls that appear only once in your log.')
            ELSE
                WriteLn ('There are ', QSODistribution [NumberQSOs], ' calls that appear ', NumberQSOs, ' times in your log.');

    WaitForKeyPressed;
    END;



PROCEDURE PrintLabelsOne;

VAR FileName, FirstCall, TempString: Str80;
    Mode: ModeType;
    MyCall, BandString, ModeString, DateString, CallString,
    TimeString, RSTString, Signature: Str80;
    FirstMode, FirstBand: Str20;
    UseLogRST, PrintEnable: BOOLEAN;
    Destination, Key: CHAR;
    FileRead, FileWrite: TEXT;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('PRINT ONE LABEL FOR EVERY QSO PROCEDURE');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This procedure will print a single 15/16 Inch high label for each QSO in the');
    WriteLn ('active log file.  The label will look like this:');
    WriteLn;
    WriteLn (Spaces, 'To: WB6ZVC  From: 4U1ITU');
    WriteLn (Spaces, '------------------------------');
    WriteLn (Spaces, 'Confirming 40SSB QSO at 0000Z ');
    WriteLn (Spaces, 'on 25-Dec-90.  Your RS was 59');
    WriteLn (Spaces, 'Signature                    ');
    WriteLn;
    WriteLn ('You can enter up to thirty characters as your signature.  This procedure can');
    WriteLn ('only print labels in the same order as they appear in your log.  The three');
    WriteLn ('QSO per label procedure can sort labels into alphabetical order.');
    WriteLn;

    IF NOT FileExists (LogFileName) THEN
        BEGIN
        ReportError ('No logfile file found.');
        WaitForKeyPressed;
        Exit;
        END;

    MyCall := UpperCase (GetResponse ('Enter From: callsign : '));
    IF MyCall = '' THEN Exit;

    REPEAT
        Signature := GetResponse ('Enter signature (max 30 chars) : ');
        IF Length (Signature) > 30 THEN
            ReportError ('That signature is too long!!');
    UNTIL Length (Signature) <= 30;

    SetUpSpaces;

    FirstCall := UpperCase (GetResponse ('Enter first call or none for first call in log : '));

    IF FirstCall <> '' THEN
        BEGIN
        FirstBand := UpperCase (GetResponse ('Enter band for this call (160 - 2) : '));
        FirstMode := UpperCase (GetResponse ('Enter mode for this call (CW or SSB) : '));
        PrintEnable := False;
        END
    ELSE
        PrintEnable := True;

    REPEAT
        Key := UpCase (GetKey ('Use RSTs from log? (Y/N) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'Y') OR (Key = 'N');
    WriteLn;
    UseLogRST := Key = 'Y';

    REPEAT
    Destination := UpCase (GetKey ('Output to (F)ile, (P)rinter or (S)creen? : '));
        CASE Destination OF
            'F': BEGIN
                 WriteLn;
                 FileName := UpperCase (GetResponse ('Enter filename to save QSL data to : '));

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

    IF Destination = 'P' THEN
        BEGIN
        WriteLn ('Press A to adjust number of spaces.');
        WriteLn ('Press S to print sample label.');
        WriteLn ('Press P to start printing labels.');

        REPEAT
            REPEAT UNTIL KeyPressed;
            Key := UpCase (ReadKey);

            IF Key = 'A' THEN SetUpSpaces;

            IF Key = 'S' THEN
                BEGIN
                WriteLn (FileWrite, Spaces, 'To: WB6ZVC  From: ', MyCall);
                WriteLn (FileWrite, Spaces, '------------------------------');
                WriteLn (FileWrite, Spaces, 'Confirming 40CW QSO at 0000Z ');
                WriteLn (FileWrite, Spaces, 'on 25-Dec-90.  Ur RST was 599');
                WriteLn (FileWrite, Spaces, Signature);
                WriteLn (FileWrite);
                END;

        UNTIL Key = 'P';
        END;

    ClrScr;
    WriteLn ('Printing labels...  Press P to stop.');

    REPEAT
        IF KeyPressed THEN
            BEGIN
            Key := Upcase (ReadKey);
            IF Key = 'P' THEN
                BEGIN
                Close (FileRead);
                Exit;
                END;
            END;

        REPEAT
            ReadLn (FileRead, TempString);
            GetLogEntryBand (TempString);
            Mode := GetLogEntryMode (TempString);
        UNTIL (Mode <> NoMode) OR EOF (FileRead);

        IF (Mode <> NoMode) AND NOT StringHas (TempString, 'DUPE') THEN
            BEGIN
            ExpandTabs (TempString);
            BandString := Copy (TempString, LogEntryBandAddress,  3);
            ModeString := Copy (TempString, LogEntryModeAddress,  3);
            DateString := Copy (TempString, LogEntryDayAddress,   9);
            TimeString := Copy (TempString, LogEntryHourAddress,  2) +
                          Copy (TempString, LogEntryMinuteAddress, 2);
            CallString := Copy (TempString, LogEntryCallAddress, 12);

            GetRidOfPostcedingSpaces (ModeString);
            GetRidOfPrecedingSpaces  (BandString);
            GetRidOfPostcedingSpaces (CallString);

            IF UseLogRST THEN
                RSTString := GetLogEntryRSTString  (TempString)
            ELSE
                IF ModeString = 'SSB' THEN
                    RSTString := '59'
                ELSE
                    RSTString := '599';

            IF NOT PrintEnable THEN
                IF (CallString = FirstCall) AND
                   (BandString = FirstBand) AND
                   (ModeString = FirstMode) THEN
                       PrintEnable := True;

            IF PrintEnable AND NOT StringHas (TempString, 'DUPE') THEN
                BEGIN
                WriteLn (FileWrite, Spaces, 'To: ', CallString, '  From: ', MyCall);
                WriteLn (FileWrite, Spaces, '------------------------------');
                WriteLn (FileWrite, Spaces, 'Confirming ', BandString, ModeString, ' QSO at ', TimeString, 'Z');
                IF ModeString = 'SSB' THEN
                    WriteLn (FileWrite, Spaces, 'on ', DateString, '.  Your RS was ', RSTString)
                ELSE
                    WriteLn (FileWrite, Spaces, 'on ', DateString, '. Your RST was ', RSTString);
                WriteLn (FileWrite, Spaces, Signature);
                WriteLn (FileWrite);
                END;
            END;

         IF Eof (FileRead) THEN
            BEGIN
            Close (FileRead);
            Close (FileWrite);
            IF FileName = '' THEN WaitForKeyPressed;
            Exit;
            END;

    UNTIL False;
    END;



FUNCTION SignatureWithName (Signature: Str80;
                            Call: CallString;
                            MaximumNameLength: INTEGER): Str80;

VAR NameString: CallString;

    BEGIN
    IF NOT StringHas (Signature, '%') THEN
        BEGIN
        SignatureWithName := Signature;
        Exit;
        END;

    NameString := CD.GetName (RootCall (Call));

    IF (NameString = '') OR (NameString = 'CLUB') THEN
        NameString := 'OM';

    IF (MaximumNameLength <> 0) AND (Length (NameString) > MaximumNameLength) THEN
        NameString := 'OM';

    SignatureWithName := PrecedingString (Signature, '%') +
                         NameString + ' ' +
                         PostcedingString (Signature, '%');
    END;



PROCEDURE PrintSpecificLabels;

VAR Call, FileName, FileString: Str80;
    Mode: ModeType;
    MyCall, BandString, ModeString, DateString, CallString,
    TimeString, StationsName, NewName, RSTString, Signature: Str80;
    Destination, Key: CHAR;
    DoingNames: BOOLEAN;
    FileWrite: TEXT;
    MaximumNameLength: INTEGER;
    NameFileWrite: TEXT;
    FileRead: TEXT;


    BEGIN
MaximumNameLength := 80; //KS -- this was not initialized
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('PRINT SINGLE LABELS FOR SPECIFIC STATIONS');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This procedure will search your log file for contacts with a specific      ');
    WriteLn ('station and allow you to print a label for any of the contacts found.');
    WriteLn ('The labels are in the same format as the one label per QSO procedure:');
    WriteLn;
    WriteLn (Spaces, 'To: WB6ZVC  From: 4U1ITU');
    WriteLn (Spaces, '------------------------------');
    WriteLn (Spaces, 'Confirming 40SSB QSO at 0000Z ');
    WriteLn (Spaces, 'on 25-Dec-90.  Your RS was 59');
    WriteLn (Spaces, 'Signature                    ');
    WriteLn;
    WriteLn ('You can enter up to thirty characters as your signature.  You can also ');
    WriteLn ('enter names into the database when checking QSOs.  You can also include');
    WriteLn ('the stations name from the name database in the signature with %.');
    WriteLn;

    IF NOT FileExists (LogFileName) THEN
        BEGIN
        ReportError ('No logfile file found.');
        WaitForKeyPressed;
        Exit;
        END;

    MyCall := UpperCase (GetResponse ('Enter From: callsign : '));
    IF MyCall = '' THEN Exit;

    REPEAT
        Signature := GetResponse ('Enter signature (max 30 chars) : ');
        IF Length (Signature) > 30 THEN
            ReportError ('That signature is too long!!');
    UNTIL Length (Signature) <= 30;

    REPEAT
    Destination := UpCase (GetKey ('Output to (F)ile, (P)rinter or (S)creen? : '));
        CASE Destination OF
            'F': BEGIN
                 WriteLn;
                 FileName := UpperCase (GetResponse ('Enter filename to save QSL data to : '));

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

    IF NOT OpenFileForWrite (FileWrite, FileName) THEN
        BEGIN
        ReportError ('Unable to open output file or device.');
        WaitForKeyPressed;
        Exit;
        END;

    SetUpSpaces;

    IF Destination = 'P' THEN
        BEGIN
        REPEAT
            Key := UpCase (GetKey ('Do you need to align the printer? (Y/N) : '));
            IF Key = EscapeKey THEN Exit;
        UNTIL (Key = 'Y') OR (Key = 'N');
        WriteLn;

        IF Key = 'Y' THEN
            BEGIN
            TextColor (Cyan);
            WriteLn ('Press S for sample, A to adjust spaces, or any other to go on.');
            REPEAT
                REPEAT UNTIL KeyPressed;
                Key := UpCase (ReadKey);

                IF Key = 'A' THEN SetUpSpaces;

                IF Key = 'S' THEN
                    BEGIN
                    WriteLn (FileWrite, Spaces, 'To: WB6ZVC  From: ', MyCall);
                    WriteLn (FileWrite, Spaces, '------------------------------');
                    WriteLn (FileWrite, Spaces, 'Confirming 40CW QSO at 0000Z ');
                    WriteLn (FileWrite, Spaces, 'on 25-Dec-90.  Ur RST was 599');
                    WriteLn (FileWrite, Spaces, Signature);
                    WriteLn (FileWrite);
                    END;

            UNTIL Key <> 'S';
        END;
        GoToXY (1, WhereY);
        ClrEol;
        END;

    DoingNames := 'Y' = GetKeyResponse ('Do you want to enter names? (Y, N) : ');

    IF DoingNames THEN
        BEGIN
        WriteLn ('New names will be saved in the file NAMES.NEW.  You can add this to your');
        WriteLn ('TRMASTER database with the POST U E F N command.');
        OpenFileForAppend (NameFileWrite, 'NAMES.NEW');
        END;

    REPEAT
        Call := UpperCase (GetResponse ('Enter call to print labels for (none to stop) : '));

        IF Call = '' THEN
            BEGIN
            Close (FileWrite);

            IF DoingNames THEN
                BEGIN
                Write ('Name file saved as NAMES.NEW.');
                Close (NameFileWrite);
                WaitForKeyPressed;
                END;

            Exit;
            END;

        IF DoingNames THEN
            BEGIN
            StationsName := CD.GetName (Call);

            IF StationsName = '' THEN
                BEGIN
                NewName := GetResponse ('Enter name : ');
                IF NewName <> '' THEN
                    WriteLn (NameFileWrite, Call, ' ', NewName);
                END
            ELSE
                BEGIN
                Write ('Name = ', StationsName);
                IF CWEnable THEN SendMorse (StationsName);

                NewName := GetResponse ('  Enter new name if any : ');

                IF NewName <> '' THEN
                    WriteLn (NameFileWrite, Call, ' ', NewName);
                END;
            END;

        IF NOT OpenFileForRead (FileRead, LogFileName) THEN
            BEGIN
            ReportError ('Unable to open ' + LogFileName);

            IF DoingNames THEN
                BEGIN
                Write ('Name file saved as NAMES.NEW.');
                Close (NameFileWrite);
                WaitForKeyPressed;
                END;

            WaitForKeyPressed;
            Exit;
            END;

        WHILE NOT Eof (FileRead) DO
            BEGIN
            IF OperatorEscape THEN
                BEGIN
                Close (FileRead);

                IF DoingNames THEN
                    BEGIN
                    Write ('Name file saved as NAMES.NEW.');
                    Close (NameFileWrite);
                    WaitForKeyPressed;
                    END;

                Exit;
                END;

            ReadLn (FileRead, FileString);

            IF StringHas (FileString, Call) THEN
                BEGIN
                ExpandTabs (FileString);
                WriteLn (FileString);
                RSTString := GetResponse ('Enter RST to be used on label for this contact (none for skip) : ');
                IF RSTString <> '' THEN
                    BEGIN
                    GetLogEntryBand (FileString);
                    Mode := GetLogEntryMode (FileString);

                    IF Mode <> NoMode THEN
                        BEGIN
                        BandString := Copy (FileString, LogEntryBandAddress,  3);
                        ModeString := Copy (FileString, LogEntryModeAddress,  3);
                        DateString := Copy (FileString, LogEntryDayAddress,   9);
                        TimeString := Copy (FileString, LogEntryHourAddress,  2) +
                                      Copy (FileString, LogEntryMinuteAddress, 2);
                        CallString := Copy (FileString, LogEntryCallAddress, 12);

                        GetRidOfPostcedingSpaces (ModeString);
                        GetRidOfPrecedingSpaces  (BandString);
                        GetRidOfPostcedingSpaces (CallString);

                        IF NOT GoodCallSyntax (CallString) THEN
                            CallString := GetResponse ('Enter callsign of this station : ');


                        WriteLn (FileWrite, Spaces, 'To: ', CallString, '  From: ', MyCall);
                        WriteLn (FileWrite, Spaces, '------------------------------');
                        WriteLn (FileWrite, Spaces, 'Confirming ', BandString, ModeString, ' QSO at ', TimeString, 'Z');
                        IF ModeString = 'SSB' THEN
                            WriteLn (FileWrite, Spaces, 'on ', DateString, '.  Your RS was ', RSTString)
                        ELSE
                            WriteLn (FileWrite, Spaces, 'on ', DateString, '. Your RST was ', RSTString);

                        WriteLn (FileWrite, Spaces, SignatureWithName (Signature, CallString, MaximumNameLength));
                        WriteLn (FileWrite);
                        END;
                    END;
                END;
            END;
        Close (FileRead);
    UNTIL False;
    END;



PROCEDURE PrintLabelsThree;

VAR FirstCall, LastCall: CallString;
    FileRead: TEXT;
    Destination, Key: CHAR;
    DoingNames, UseLogRST, PrintEnable, AlphabeticalOrder: BOOLEAN;
    Country: INTEGER;
    TempContactData: ContactRecord;
    Call, DefaultRST: Str20;
    Signature1, Signature2, FileName, FileString: Str80;
    TotalNumberQSLs, QSLLabel, Block, Address, QSLLabelAddress, QSO: INTEGER;
    MaximumName1Length, MaximumName2Length, TotalQSOs: INTEGER;
    CountryTotalArray: CountryTotalArrayType;
    FileWrite: TEXT;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('PRINT UP TO THREE QSOs PER LABEL PROCEDURE');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This procedure will combine up to three QSOs with the same station onto a ');
    WriteLn ('single 15/16th inch label.  If you have more than 3 QSOs with the station,');
    WriteLn ('a second label will be printed.  You can sort the labels into alphabetical');
    WriteLn ('order if you want.  You can enter two different signatures that will appear');
    WriteLn ('on any blank lines.');
    WriteLn;

    IF NOT FileExists (LogFileName) THEN
        BEGIN
        ReportError ('No logfile file found.');
        WaitForKeyPressed;
        Exit;
        END;

    REPEAT
        Key := UpCase (GetKey ('Use RSTs from the (L)og or (D)efault RST? (L/D) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'L') OR (Key = 'D');
    WriteLn;

    IF Key = 'D' THEN
        BEGIN
        DefaultRST := GetResponse ('Enter default RST : ');
        UseLogRST := False;
        END
    ELSE
        UseLogRST := True;

    FirstCall := UpperCase (GetResponse ('Enter first call to print label for (return for start of log) : '));
    LastCall  := UpperCase (GetResponse ('Enter last call to print label for (return for end of log) :  '));

    PrintEnable := FirstCall = '';

    REPEAT
        Key := UpCase (GetKey ('Sort labels in alphabetical order within each country? (Y/N) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'Y') OR (Key = 'N');
    WriteLn;
    AlphabeticalOrder := Key = 'Y';

    TextColor (Cyan);
    WriteLn;
    WriteLn ('The first signature you will enter will appear if there is at least one     ');
    WriteLn ('blank line on the label.  The second will appear after the first if there');
    WriteLn ('are two blank lines on the label.  Each must be 30 characters or less.');
    WriteLn ('You can use the special character % to fetch a name from the name database.');
    WriteLn ('If no name exists, the name OM will appear instead.  Do not put a space after');
    WriteLn ('the % character.');
    WriteLn;

    MaximumName1Length := 0;
    MaximumName2Length := 0;

    REPEAT
        Signature1 := GetResponse ('Enter first signature : ');

        IF StringHas (Signature1, '%') THEN
            MaximumName1Length := GetValue ('Enter maximum length of name to use (6-12) : ');

        IF Length (Signature1) + MaximumName1Length > 30 THEN
            ReportError ('Too many characters!!');

    UNTIL Length (Signature1) <= 30;

    REPEAT
        Signature2 := GetResponse ('Enter second signature : ');

        IF StringHas (Signature2, '%') THEN
            MaximumName2Length := GetValue ('Enter maximum length of name to use (6-12) : ');

        IF Length (Signature2) + MaximumName2Length > 30 THEN
            ReportError ('Too many characters!!');

    UNTIL Length (Signature2) <= 30;

    WriteLn;
    TextColor (Cyan);
    WriteLn ('You can print up to three QSOs per label.  However, if you want to always   ');
    WriteLn ('print at least the first signature, you can limit the number of lines to two.');
    WriteLn ('If you want to always print both signatures, you can limit the number of');
    WriteLn ('lines to one.');
    WriteLn;

    REPEAT
        Key := UpCase (GetKey ('Enter the maximum number of QSOs per label (1-3) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = '1') OR (Key = '2') OR (Key = '3');
    WriteLn;

    CASE Key OF
        '1': NumberContactsPerLabel := 1;
        '2': NumberContactsPerLabel := 2;
        '3': NumberContactsPerLabel := 3;
        END;

    DoingNames := False;

    REPEAT
    Destination := UpCase (GetKey ('Output to (F)ile, (P)rinter or (S)creen? : '));
        CASE Destination OF
            'F': BEGIN
                 WriteLn;
                 FileName := UpperCase (GetResponse ('Enter filename to save QSL data to : '));

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

    IF NOT OpenFileForWrite (FileWrite, FileName) THEN
        BEGIN
        ReportError ('Unable to open output file or device.');
        WaitForKeyPressed;
        Exit;
        END;

    SetUpSpaces;

    IF Destination = 'P' THEN
        BEGIN
        WriteLn ('Press S for sample label, A to adjust spaces, or P to start printing.');

        REPEAT
            REPEAT UNTIL KeyPressed;
            Key := UpCase (ReadKey);

            IF Key = 'A' THEN SetUpSpaces;

            IF Key = 'S' THEN
                BEGIN
                WriteLn (FileWrite, Spaces, '      TO RADIO: WB6ZVC');
                WriteLn (FileWrite, Spaces, 'BAND    DATE     UTC   RST  2X');
                WriteLn (FileWrite, Spaces, ' 10  10-Dec-90  13:03  599  CW');
                WriteLn (FileWrite, Spaces, '160  23-Dec-90  01:47  579  CW');
                WriteLn (FileWrite, Spaces, ' 20   7-Jan-91  03:24  59  SSB');
                WriteLn (FileWrite);
                END;
        UNTIL Key = 'P';
        END;

    ClrScr;
    TextColor (Cyan);
    WriteLn;
    Write ('Computing QSO totals for each country...');
    ComputeCountryTotals (CountryTotalArray, TotalQSOs);
    GoToXY (1, WhereY);
    ClrEol;

    WriteLn ('Total QSOs found in log = ', TotalQSOs);
    IF TotalQSOs = 0 THEN Exit;

    WriteLn;
    Write ('Sorting out countries with fewer than 20 QSOs into temp file...');
    CreateSmallCountryTempFile (CountryTotalArray);

    GoToXY (1, WhereY);
    ClrEol;

    IF FirstCall = '' THEN
//        StartCountry := 0
    ELSE
        CountryTable.GetCountry (FirstCall, True);

    IF LastCall  = '' THEN
//        FinishCountry := CountryTable.NumberCountries - 1
    ELSE
        CountryTable.GetCountry (LastCall, True);

    WriteLn ('Press ESCAPE to stop print.');

    IF StringHas (Signature1, '%') OR StringHas (Signature2, '%') THEN
        IF (CD.SCPIndexArray <> nil) OR CD.LoadInIndexArray THEN
            DoingNames := True;

    TotalNumberQSLs := 0;

    FOR Country := 0 TO CountryTable.NumberCountries - 1 DO
      IF CountryTotalArray [Country] > 0 THEN
        BEGIN
        GoToXY (1, WhereY);
        ClrEol;
        WriteLn ('Processing ', CountryTable.GetCountryName (Country), ' with ', CountryTotalArray [Country], ' QSOs.');
        QSLData.NumberLabels := 0;

        IF CountryTotalArray [Country] < 20 THEN
            FileName := QSLTempFileName
        ELSE
            FileName := LogFileName;

        IF NOT OpenFileForRead (FileRead, FileName) THEN
            BEGIN
            ReportError (FileName + ' has disappeared!!');
            WaitForKeyPressed;
            Exit;
            END;

        WHILE NOT Eof (FileRead) DO
            BEGIN
            IF OperatorEscape THEN
                BEGIN
                Close (FileRead);
                Close (FileWrite);
                DeleteFile (QSLTempFileName);
                Exit;
                END;

            ReadLn (FileRead, FileString);

            TempContactData.Band := GetLogEntryBand (FileString);

            IF (TempContactData.Band <> NoBand) AND NOT StringHas (FileString, 'DUPE') THEN
                BEGIN
                ExpandTabs (FileString);
                Call := GetLogEntryCall (FileString);

                IF Country = CountryTable.GetCountry (Call, True) THEN
                    BEGIN
                    TempContactData.Mode := GetLogEntryMode       (FileString);
                    TempContactData.Date := GetLogEntryDateString (FileString);
                    TempContactData.Time := GetLogEntryTimeString (FileString);
                    IF UseLogRST THEN
                        TempContactData.RST  := GetLogEntryRSTString  (FileString)
                    ELSE
                        TempContactData.RST := DefaultRST;
                    AddQSLDataToLabels (Call, TempContactData);
                    END;
                END;
            END;

        Close (FileRead);

        SetUpPointerArray;

        IF AlphabeticalOrder THEN SortQSLLabelData;

        IF QSLData.NumberLabels > 0 THEN
            BEGIN
            TotalNumberQSLs := TotalNumberQSLs + QSLData.NumberLabels;
            FOR QSLLabel := 0 TO QSLData.NumberLabels - 1 DO
                BEGIN
                QSLLabelAddress := PointerArray^ [QSLLabel];
                Block := QSLLabelAddress DIV QSLLabelBlockSize + 1;
                Address := QSLLabelAddress MOD QSLLabelBlockSize;

                WITH QSLData.Blocks [Block]^ [Address] DO
                    BEGIN
                    IF NOT PrintEnable THEN
                        IF Callsign = FirstCall THEN PrintEnable := True;

                    IF OperatorEscape THEN
                        BEGIN
                        Close (FileRead);
                        Close (FileWrite);
                        Dispose (PointerArray);
                        DeleteFile (QSLTempFileName);
                        Exit;
                        END;

                    IF PrintEnable THEN
                        BEGIN
                        WriteLn (FileWrite, Spaces, '      TO RADIO: ', Callsign);
                        WriteLn (FileWrite, Spaces, 'BAND    DATE     UTC   RST  2X');

                        FOR QSO := 1 TO 3 DO
                            WITH ContactArray [QSO] DO
                                BEGIN
                                IF NumberContacts >= QSO THEN
                                    BEGIN
                                    Write (FileWrite, Spaces, BandString [Band], '  ');
                                    Write (FileWrite, Date, '  ', Time, '  ', RST, '  ', ModeString [Mode]);
                                    END
                                ELSE
                                    CASE QSO OF
                                        2: IF DoingNames THEN
                                               Write (FileWrite, Spaces, SignatureWithName (Signature1,
                                                                                            Callsign,
                                                                                            MaximumName1Length))
                                           ELSE
                                               Write (FileWrite, Spaces, Signature1);

                                        3: IF NumberContacts = 1 THEN
                                               BEGIN
                                               IF DoingNames THEN
                                                   Write (FileWrite, Spaces, SignatureWithName (Signature2,
                                                                                                Callsign,
                                                                                                MaximumName2Length))
                                               ELSE
                                                   Write (FileWrite, Spaces, Signature2);
                                               END
                                           ELSE
                                               IF DoingNames THEN
                                                   Write (FileWrite, Spaces, SignatureWithName (Signature1,
                                                                                                Callsign,
                                                                                                MaximumName1Length))
                                               ELSE
                                                   Write (FileWrite, Spaces, Signature1);
                                        END;
                                WriteLn (FileWrite);
                                END;

                        WriteLn (FileWrite);
                        IF Callsign = LastCall THEN
                            BEGIN
                            Close (FileWrite);
                            DeleteFile (QSLTempFileName);
                            TextColor (Cyan);
                            WriteLn (LastCall, ' has been found.  Printing stopped.');
                            Dispose (PointerArray);
                            WaitForKeyPressed;
                            Exit;
                            END;
                        END;
                    END;
                END;

            ClearQSLDataMemory;
            END;

        Dispose (PointerArray);
        END;

    Close (FileWrite);
    DeleteFile (QSLTempFileName);
    WriteLn;
    TextColor (Yellow);
    WriteLn ('All ', TotalNumberQSLs, ' labels printed.');
    WaitForKeyPressed;
    END;



FUNCTION QSLProcedureMenu: BOOLEAN;

VAR Key: CHAR;

    BEGIN
    QSLProcedureMenu := True;
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('QSL PROCEDURE MENU');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('  C - Confirm contacts with no labels printed.');
    WriteLn ('  E - Estimate number of QSL labels.');
    WriteLn ('  O - Print labels, one QSO per label.');
    WriteLn ('  S - Print labels for QSOs with a selected station.');
    WriteLn ('  T - Print labels, up to three QSOs per label.');
    WriteLn ('  V - View log segments.');
    WriteLn ('  X - Exit QSL Procedure Menu');
    WriteLn;
    TextColor (Cyan);
    Write   ('  Enter command : ');

    REPEAT
        REPEAT UNTIL KeyPressed;
        Key := UpCase (ReadKey);

        CASE Key OF
            'C': BEGIN ConfirmContacts;         Exit; END;
            'E': BEGIN EstimateNumberOfLabels;  Exit; END;
            'O': BEGIN PrintLabelsOne;          Exit; END;
            'S': BEGIN PrintSpecificLabels;     Exit; END;
            'T': BEGIN PrintLabelsThree;        Exit; END;
            'V': BEGIN ViewLog;                 Exit; END;

            'X', EscapeKey:
                BEGIN
                QSLProcedureMenu := False;
                Exit;
                END;
            END;
    UNTIL False;
    END;



    BEGIN
    END.
