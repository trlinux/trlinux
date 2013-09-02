UNIT LogHelp;

{$O+}
{$F+}
{$V-}

INTERFACE

USES Country9, LogSCP, LogK1EA, DOS, trCrt, SlowTree, Tree, LogGrid, LogWind
   ,Ports;

CONST
    QTCFileName = 'QTC.DAT';
    QTCListFileName = 'QTCLIST.DAT';

TYPE
    HelpSubject = (NoSubject);

    TabModeType = (NormalTabMode, ControlFTabMode);


VAR
    ConfirmEditChanges: BOOLEAN;
    TabMode:            TabModeType;
    UserNameString:     Str40;
    VideoGameLength:    INTEGER;

    PROCEDURE AddReminder;
    PROCEDURE Bin64Decode;
    PROCEDURE CheckForName;
    PROCEDURE CoaxLength;
    PROCEDURE ComputeGridDistance;
    PROCEDURE ContextHelp (Subject: HelpSubject);
    PROCEDURE DisplayCountryInformation (FileName: Str80; Call: CallString);

    PROCEDURE EditWindowEditor (VAR EditLines: LogEntryArray;
                                CursorX: INTEGER; CursorY: INTEGER;
                                VAR DataChanged: BOOLEAN);

    PROCEDURE FreeStartUpScreen;
    PROCEDURE HexDump;
    PROCEDURE HexConvert;
    PROCEDURE Inductance;
    PROCEDURE IOPort;

    PROCEDURE LoadQTCDataFile;
    PROCEDURE LoopBackTest;
    PROCEDURE PacketMess;
    PROCEDURE PacketSimulate;
    PROCEDURE PassThrough;
    PROCEDURE PortToFile;
    PROCEDURE PutUpHelpMenu;
    PROCEDURE SaveQTCDataFile;
    PROCEDURE SetAlarm;
    PROCEDURE ShowIOPorts;
    PROCEDURE StartUpHelp;
    PROCEDURE SunriseSunset;
    PROCEDURE TellMeMyGrid;
    PROCEDURE UUDecode;
    PROCEDURE ViewLogFile;
    PROCEDURE ViewRadioDebug;

IMPLEMENTATION

Uses LogCfg,memlinux,communication,keycode,beep,radio,portname,timer;

CONST
    PageBufferSize = BigWIndowRX * (BigWindowRY - BigWindowLY + 2);
    MaxBuffers = 100;

TYPE
    PageBuffer = RECORD
        List: ARRAY [0..PageBufferSize - 1] OF CHAR;
        NumberChars: INTEGER;
        END;

    PageBufferPointer = ^PageBuffer;

VAR
    DisplayedBuffer: INTEGER;
    FirstBuffer:     INTEGER;
    LogFileRead:     TEXT;
    NumberBuffers:   INTEGER;
    TextBuffer:      ARRAY [0..MaxBuffers - 1] OF PageBufferPointer;

function serialaddress(i: integer):integer;cdecl;external;
function paralleladdress(i: integer):integer;cdecl;external;



PROCEDURE TellMeMyGrid;

VAR LatString, LonString: Str40;
    Result: INTEGER;
    Lat, Lon: REAL;

    BEGIN
    WriteLn ('Okay, I will tell you the grid of a given lat/lon.');

    REPEAT
        WriteLn;
        LatString := GetResponse ('Enter lat (minus is south - none to quit) : ');
        IF LatString = '' THEN Exit;
        Val (LatString, Lat, Result);
        IF Result <> 0 THEN Exit;

        LonString := GetResponse ('Enter lon (minus is east) : ');
        IF LonString = '' THEN Exit;
        Val (LonString, Lon, Result);
        IF Result <> 0 THEN Exit;

        WriteLn ('Grid is ', ConvertLatLonToGrid (Lat, Lon));
    UNTIL False;
    END;

PROCEDURE UUDecode;

VAR Mode: Str20;
    OutFileName, InputfileName: Str80;
    CharacterPointer: INTEGER;
    FileRead: TEXT;
    FileWrite: FILE;
    FileString: Str160;
    Key: CHAR;
    OutputByte1, OutputByte2, OutputByte3: BYTE;

    BEGIN
    IF ParamStr (2) = '' THEN
        InputFileName := GetResponse ('Enter file to uudecode : ')
    ELSE
        InputFileName := Paramstr (2);

    IF InputFileName = '' THEN Exit;

    IF NOT OpenFileForRead (FileRead, InputFileName) THEN Exit;

    IF NOT Eof (FileRead) THEN ReadLn (FileRead, FileString);

    WHILE UpperCase (Copy (FileString, 1, 6)) <> 'BEGIN ' DO
        BEGIN
        ReadLn (FileRead, FileString);

        IF Eof (FileRead) THEN
           BEGIN
           Close (FileRead);
           Exit;
           END;
        END;

   { We have found the begin statement }

   FileString := UpperCase (FileString);
   FileString := PostcedingString (FileString, 'BEGIN ');
   Mode := RemoveFirstString (FileString);
   OutFileName := RemoveFirstString (FileString);

   IF OutFileName = '' THEN
       BEGIN
       Close (FileRead);
       Exit;
       END;

       { See if output file exists, and if so, make sure overwrite is okay }

   WriteLn ('Output file = ', OutFileName);

   IF FileExists (OutFileName) THEN
       BEGIN
       REPEAT
       Key := UpCase (GetKey (OutFileName + ' already exists.  Overwrite? (Y/N/) : '));

       IF (Key = EscapeKey) OR (Key = 'N') THEN
           BEGIN
           Close (FileRead);
           Exit;
           END;

       UNTIL Key = 'Y';
       WriteLn;
       END
   ELSE
       WriteLn ('Output file = ', OutFileName);

   { Open output file }

   Assign (FileWrite, OutFileName);
   ReWrite (FileWrite, 1);

   WHILE NOT Eof (FileRead) DO
       BEGIN
       ReadLn (FileRead, FileString);

       IF (FileString = '') OR (UpperCase (FileString) = 'END') THEN Continue;

       IF NOT (FileString [1] IN [' ', '`']) THEN
           BEGIN
           Delete (FileString, 1, 1);  { Get rid of length character }

           FOR CharacterPointer := 1 TO Length (FileString) DO
               BEGIN
               IF NOT (FileString [CharacterPointer] IN [' '..'`']) THEN
                   BEGIN
                   Close (FileRead);
                   Close (FileWrite);
                   ReportError ('Illegal character found!!');
                   WriteLn (FileString);
                   Exit;
                   END;

               IF FileString [CharacterPointer] = '`' THEN
                   FileString [CharacterPointer] := ' ';
               END;

           { We have legal characters - process 4 bytes at a time }

           WHILE FileString <> '' DO
               BEGIN

               { Suppressed space at end of line protetion }

               WHILE Length (FileString) < 4 DO
                   FileString := FileString + ' ';

               OutputByte1 := ((Ord (FileString [1]) - 32) SHL 2) + ((Ord (FileString [2]) - 32) SHR 4);
               OutputByte2 := ((Ord (FileString [2]) - 32) SHL 4) + ((Ord (FileString [3]) - 32) SHR 2);
               OutputByte3 := ((Ord (FileString [3]) - 32) SHL 6) + ((Ord (FileString [4]) - 32));

               BlockWrite (FileWrite, OutputByte1, 1);
               BlockWrite (FileWrite, OutputByte2, 1);
               BlockWrite (FileWrite, OutputByte3, 1);

               Delete (FileString, 1, 4);
               END;
           END;
       END;

    Close (FileWrite);
    Close (FileRead);
    END;



PROCEDURE Bin64Decode;

VAR N: INTEGER;
    T: ARRAY [0..255] OF BYTE;
    InputFile, OutputFile: Str40;
    FileRead:  TEXT;
    FileWrite: FILE;
    FileString: Str160;
    NumberBytes: LONGINT;
    ByteValue: INTEGER;
    CharSet: Str80;
    SaveByte: BYTE;
    Started: BOOLEAN;
    LeftOverChars: Str20;

    BEGIN
    IF ParamStr (2) = '' THEN
        InputFile := GetResponse ('Enter file to decode : ')
    ELSE
        InputFile := Paramstr (2);

    IF InputFile = '' THEN Exit;

    CharSet := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

    FOR N := 0 TO 255 DO T [N] := 0;
    FOR N := 0 TO 63 DO T [Ord (CharSet [N+1])] := N;

    NumberBytes := 0;
    OutputFile := '';

    IF OpenFileForRead (FileRead, InputFile) THEN
        BEGIN
        WHILE NOT Eof (FileRead) DO
            BEGIN
            ReadLn (FileRead, FileString);
            FileString := UpperCase (FileString);
            WriteLn (FileString);

            IF StringHas (FileString, 'NAME=') THEN
                BEGIN
                OutputFile := PostcedingString (FileString, 'NAME=');

                IF OutputFile [1] = '"' THEN
                    Delete (OutputFile, 1, 1);

                IF StringHas (OutputFile, '"') THEN
                    OutputFile := PrecedingString (OutputFile, '"')
                ELSE
                    OutputFile := PrecedingString (OutputFile, ';');

                WriteLn ('Output file command found = ', OutputFile);
                END;

            IF (StringHas (FileString, 'CONTENT-TRANSFER-ENCOD') AND
                StringHas (FileString, 'BASE64')) THEN
                BEGIN
                ReadLn (FileRead, FileString);

                FileString := UpperCase (FileString);

                IF Stringhas (FileString, 'NAME=') THEN
                    BEGIN
                    OutputFile := PostcedingString (FileString, 'NAME=');

                    IF OutputFile [1] = '"' THEN
                        Delete (OutputFile, 1, 1);

                    IF StringHas (OutputFile, '"') THEN
                        OutputFile := PrecedingString (OutputFile, '"')
                    ELSE
                        OutputFile := PrecedingString (OutputFile, ';');

                    WriteLn ('Output file command found = ', OutputFile);
                    END;

                IF OutputFile = '' THEN
                    BEGIN
                    OutputFile := GetResponse ('Output file name = ');

                    IF OutputFile = '' THEN
                        BEGIN
                        Close (FileRead);
                        Exit;
                        END;
                    END;

                Assign  (FileWrite, OutputFile);
                ReWrite (FileWrite, 1);

                Started := False;
                LeftOverChars := '';

                WHILE NOT Eof (FileRead) DO
                    BEGIN
                    ReadLn (FileRead, FileString);

                    GetRidOfPrecedingSpaces (FileString);
                    GetRidOfPostcedingSpaces (FileString);

                    IF Started AND (FileString = '') THEN
                        BEGIN
                        Close (FileWrite);
                        Close (FileRead);
                        WriteLn (NumberBytes, ' bytes saved to ', OutputFile);
                        Exit;
                        END;

                    IF (Length (FileString) > 40) AND (NOT StringHas (FileString, '=')) THEN
                        Started := True;

                    IF Started THEN
                        BEGIN
                        FileString := LeftOverChars + FileString;

                        WHILE Length (FileString) >= 4 DO
                            BEGIN
                            ByteValue := T [Ord (FileString [1])];

                            { Push six bits to the left }

                            ByteValue := ByteValue * 64;

                            { Fill in next six bits }

                            ByteValue := ByteValue + T [Ord (FileString [2])];

                            { Four more shifts to get first byte in hi byte }

                            ByteValue := ByteValue * 16;

                            { Save first byte }

                            SaveByte := Hi (ByteValue);
                            BlockWrite (FileWrite, SaveByte, 1);

                            { Push two more places to make room for six bits }

                            ByteValue := ByteValue *  4;

                            { Get next 6 bits }

                            ByteValue := ByteValue + T [Ord (FileSTring [3])];

                            { Push 6 times to make second byte }

                            ByteValue := ByteValue * 64;

                            { Save second byte }

                            SaveByte := Hi (ByteValue);
                            BlockWrite (FileWrite, SaveByte, 1);

                            { Get last 6 bits and you have the byte in Lo }

                            ByteValue := ByteValue + T [Ord (FileString [4])];

                            { Save third byte }

                            SaveByte := Lo (ByteValue);
                            BlockWrite (FileWrite, SaveByte, 1);

                            { Delete the bits we have processed }

                            Delete (FileString, 1, 4);
                            NumberBytes := NumberBytes + 3;
                            END;

                        LeftOverChars := FileString;
                        END;
                    END;
                Close (FileWrite);
                END;
            END;
        Close (FileRead);
        WriteLn (NumberBytes, ' bytes saved to ', OutputFile);
        END;
    END;



PROCEDURE LastQTCSent (VAR Call: Callstring; VAR Time: Str20; VAR QSONumber: INTEGER);

VAR TempString, LastFirstString, LastSecondString, FileString: Str80;
    FileRead: TEXT;

    BEGIN
    NumberQTCBooksSent := 0;

    IF OpenFileForRead (FileRead, QTCFileName) THEN
        BEGIN
        WHILE NOT Eof (FileRead) DO
            BEGIN
            ReadLn (FileRead, FileString);

            GetRidOfPostcedingSpaces (FileString);

            IF FileString = '' THEN Continue;

            IF Copy (FileString, 1, 3) = 'QTC' THEN
                BEGIN
                Inc (NumberQTCBooksSent);
                LastFirstString := '';
                LastSecondString := '';
                Continue;
                END;

            TempString := Copy (FileString, 1, 40);

            IF (Copy (TempString, 1, 3) = ' 1:') OR
               (Copy (TempString, 1, 3) = ' 2:') OR
               (Copy (TempString, 1, 3) = ' 3:') OR
               (Copy (TempString, 1, 3) = ' 4:') OR
               (Copy (TempString, 1, 3) = ' 5:') THEN
                   BEGIN
                   RemoveFirstString (TempString);
                   LastFirstString := TempString;
                   END;

            TempString := Copy (FileString, 41, 30);

            IF (Copy (TempString, 1, 3) = ' 6:') OR
               (Copy (TempString, 1, 3) = ' 7:') OR
               (Copy (TempString, 1, 3) = ' 8:') OR
               (Copy (TempString, 1, 3) = ' 9:') OR
               (Copy (TempString, 1, 3) = '10:') THEN
                   BEGIN
                   RemoveFirstString (TempString);
                   LastSecondString := TempString;
                   END;
            END;

        Close (FileRead);

        IF LastSecondString <> '' THEN
            BEGIN
            Time      := RemoveFirstString (LastSecondString);
            Call      := RemoveFirstString (LastSecondString);
            QSONumber := RemoveFirstLongInteger (LastSecondString);
            END
        ELSE
            BEGIN
            Time      := RemoveFirstString (LastFirstString);
            Call      := RemoveFirstString (LastFirstString);
            QSONumber := RemoveFirstLongInteger (LastFirstString);
            END;

        Insert (':', Time, 3);

        Exit;
        END;

    Call := '';
    Time := '';
    QSONumber := 0;
    END;



FUNCTION GetLogEntryQSONumberReceived (FileString: Str80): INTEGER;

VAR Exchange: Str40;
    QSONumber, Result: INTEGER;

    BEGIN
    Exchange := GetLogEntryExchangeString (FileString);
    GetRidOfPrecedingSpaces (Exchange);
    Exchange := PostcedingString (Exchange, ' ');
    GetRidOfPrecedingSpaces (Exchange);
    Exchange := PostcedingString (Exchange, ' ');
    GetRidOfPrecedingSpaces  (Exchange);
    GetRidOfPostcedingSpaces (Exchange);
    Val (Exchange, QSONumber, Result);

    IF Result = 0 THEN
        GetLogEntryQSONumberReceived := QSONumber
    ELSE
        GetLogEntryQSONumberReceived := 0;
    END;



PROCEDURE LoadQTCDataFile;

VAR Band: BandType;
    Mode: ModeType;
    FileRead: TEXT;
    LastQTCTime: Str20;
    FileString, Exchange, TempString: Str80;
    QSOPoints, TotalQTCs, QSONumber, Line, LastQTCQSONumber, Result: INTEGER;
    LogQSONumber, LogTime: INTEGER;
    Call, LogCall, LastQTCCall: CallString;
    Started: BOOLEAN;

    BEGIN
    IF MyContinent <> Europe THEN New (PendingQTCArray);
    New (QTCDataArray);

    NumberQTCStations := 0;

    IF OpenFileForRead (FileRead, QTCListFileName) THEN
        BEGIN
        WHILE NOT Eof (FileRead) DO
            BEGIN
            ReadLn (FileRead, Call);
            BigCompressFormat (Call, QTCDataArray ^[NumberQTCStations].Call);
            ReadLn (FileRead, QTCDataArray ^[NumberQTCStations].NumberQTCs);
            Inc (NumberQTCStations);
            END;

        Close (FileRead);
        END;

    IF MyContinent <> Europe THEN  { We are sending QTCs }
        BEGIN
        New (PendingQTCArray);

        TotalQTCs := TotalNumberQTCsProcessed;  { TotalNumberQTCs is a FUNCTION }
        NextQTCToBeSent  := 0;
        NextQTCToBeAdded := 0;
        NumberQTCBooksSent := 0;

        { Find out callsign, time and QSO Number of last QTC sent }

        LastQTCSent (LastQTCCall, LastQTCTime, LastQTCQSONumber);

        Started := LastQTCCall = '';    { None sent yet }

        { Okay - read in QTC data from LOG file }

        IF OpenFileForRead (FileRead, LogFileName) THEN
            BEGIN
            WHILE NOT Eof (FileRead) DO
                BEGIN

                ReadLn (FileRead, FileString);

                Band := GetLogEntryBand (FileString);
                Mode := GetLogEntryMode (FileString);
                QSOPoints := GetLogEntryQSOPoints (FileString);

                IF (Band <> NoBand) AND (Mode <> NoMode) AND (QSOPoints > 0) THEN
                    BEGIN
                    LogCall      := GetLogEntryCall (FileString);
                    LogQSONumber := GetLogEntryQSONumberReceived (FileString);
                    LogTime      := GetLogEntryIntegerTime (FileString);

                    IF Started THEN
                        AddQSOToPendingQTCList (LogTime, LogCall, LogQSONumber)
                    ELSE
                        IF (LogCall = LastQTCCall) AND (LogQSONumber = LastQTCQSONumber) THEN
                            Started := True;
                    END;

                END;

            Close (FileRead);
            END;
        END;
    END;



PROCEDURE SaveQTCDataFile;

VAR FileWrite: TEXT;
    TempString: Str80;
    Station: INTEGER;

    BEGIN
    IF NumberQTCStations > 0 THEN
        IF OpenFileForWrite (FileWrite, QTCListFileName) THEN
            BEGIN
            FOR Station := 0 TO NumberQTCStations - 1 DO
                BEGIN
                WriteLn (FileWrite, BigExpandedString (QTCDataArray ^[Station].Call));
                WriteLn (FileWrite, QTCDataArray ^[Station].NumberQTCs);
                END;
            Close (FileWrite);
            END;
    END;


PROCEDURE AddReminder;

VAR TimeString, DayString, DateString, ReminderString: Str80;
    Result: INTEGER;
    FileWrite: TEXT;

    BEGIN
    IF NumberReminderRecords >= MaximumReminderRecords THEN
        BEGIN
        SaveSetAndClearActiveWindow (QuickCommandWindow);
        Write ('Sorry, you have used up all your reminders!!');
        Delay (2000);
        RemoveAndRestorePreviousWindow;
        Exit;
        END;

    TimeString := QuickEditResponse ('Enter time for reminder (XX:XX) : ', 12);
    IF (TimeString = EscapeKey) OR (Copy (TimeString, 3, 1) <> ':') THEN Exit;
    Delete (TimeString, 3, 1);

    IF NOT StringIsAllNumbers (TimeString) THEN Exit;
    IF Length (TimeString) <> 4 THEN Exit;

    DateString := UpperCase (QuickEditResponse ('Enter day or date : ', 12));
    IF (DateString = '') OR (DateString = EscapeKey) THEN Exit;

    IF NumberReminderRecords = 0 THEN New (Reminders);

    Reminders^ [NumberReminderRecords].DateString := '';
    Reminders^ [NumberReminderRecords].DayString  := '';
    Reminders^ [NumberReminderRecords].Alarm := False;

    Val (TimeString, Reminders^ [NumberReminderRecords].Time, Result);

    IF StringHas (UpperCase (DateString), 'ALARM') THEN
        BEGIN
        Reminders^ [NumberReminderRecords].Alarm := True;
        DateString := BracketedString (DateString, '', 'ALARM');
        END;

    GetRidOfPostcedingSpaces (DateString);
    DateString := UpperCase (DateString);



    IF StringHas (DateString, '-') THEN
        BEGIN
        CASE Length (DateString) OF
            8: IF (DateString [2] <> '-') OR (DateString [6] <> '-') THEN
                   BEGIN
                   SaveSetAndClearActiveWindow (QuickCommandWindow);
                   Write ('Invalid date!!');
                   Delay (1000);
                   RestorePreviousWindow;
                   Exit;
                   END
               ELSE
                   DateString := '0' + DateString;

            9: IF (DateString [3] <> '-') OR (DateString [7] <> '-') THEN
                   BEGIN
                   SaveSetAndClearActiveWindow (QuickCommandWindow);
                   Write ('Invalid date!!');
                   Delay (1000);
                   RestorePreviousWindow;
                   Exit;
                   END;

            ELSE
                BEGIN
                SaveSetAndClearActiveWindow (QuickCommandWindow);
                Write ('Invalid date!!');
                Delay (1000);
                RestorePreviousWindow;
                Exit;
                END;
            END;    { of case }

        Reminders^ [NumberReminderRecords].DateString := DateString;
        END
    ELSE
        BEGIN
        IF Copy (DateString, 1, 2) = 'MO' THEN DateString := 'MONDAY';
        IF Copy (DateString, 1, 2) = 'TU' THEN DateString := 'TUESDAY';
        IF Copy (DateString, 1, 2) = 'WE' THEN DateString := 'WEDNESDAY';
        IF Copy (DateString, 1, 2) = 'TH' THEN DateString := 'THURSDAY';
        IF Copy (DateString, 1, 2) = 'FR' THEN DateString := 'FRIDAY';
        IF Copy (DateString, 1, 2) = 'SA' THEN DateString := 'SATURDAY';
        IF Copy (DateString, 1, 2) = 'SU' THEN DateString := 'SUNDAY';

        DayString := Copy (DateString, Length (DateString) - 2, 3);

        IF (DayString <> 'DAY') AND (DateString <> 'ALL') AND (DateString <> 'TMW') THEN
            BEGIN
            SaveSetAndClearActiveWindow (QuickCommandWindow);
            Write ('Invalid date!!');
            Delay (1000);
            RestorePreviousWindow;
            Exit;
            END;

        IF DateString = 'TODAY' THEN DateString := UpperCase (GetDayString);
        IF DateString = 'TMW'   THEN DateString := UpperCase (GetTomorrowString);

        Reminders^ [NumberReminderRecords].DayString := DateString;
        END;

    ReminderString := QuickEditResponse ('Msg = ', 75);
    IF (ReminderString = '') OR (ReminderString = EscapeKey) THEN Exit;

    Reminders^ [NumberReminderRecords].Message := ReminderString;
    Inc (NumberReminderRecords);



    SaveSetAndClearActiveWindow (QuickCommandWindow);
    Write ('Reminder number ', NumberReminderRecords, ' added on ');
    Write (Reminders^ [NumberReminderRecords - 1].DateString);
    Write (Reminders^ [NumberReminderRecords - 1].DayString);
    Write (' at ');
    IF Reminders^ [NumberReminderRecords - 1].Time < 1000 THEN
        Write ('0', Reminders^ [NumberReminderRecords - 1].Time)
    ELSE
        Write (Reminders^ [NumberReminderRecords - 1].Time);

    Delay (2000);

    IF OpenFileForAppend (FileWrite, LogConfigFileName) THEN
        BEGIN
        WriteLn (FileWrite, ';   Reminder added by operator on line');
        Write (FileWrite, 'REMINDER = ');
        Write (FileWrite, TimeString, ' ON ');
        Write (FileWrite, Reminders^ [NumberReminderRecords - 1].DateString);
        Write (FileWrite, Reminders^ [NumberReminderRecords - 1].DayString);
        Write (FileWrite, ' ');
        IF Reminders^ [NumberReminderRecords - 1].Alarm THEN
            Write (FileWrite, 'ALARM');
        WriteLn (FileWrite);
        WriteLn (FileWrite, Reminders^ [NumberReminderRecords - 1].Message);
        Close (FileWrite);
        END;

    RemoveAndRestorePreviousWindow;
    END;



PROCEDURE CheckForName;

VAR FileRead, FileWrite: TEXT;
    Directory, CityString, AddressString: Str80;

    BEGIN
    Directory := FindDirectory ('NAME.DAT');

    IF Directory = '' THEN Directory := FindDirectory ('trlog');

    IF OpenFileForRead (FileRead, Directory  + DirectorySeparator + 'NAME.DAT') THEN
        BEGIN
        ReadLn (FileRead, UserNameString);
        WriteLn ('TR Program registered to ', UserNameString);
        Close (FileRead);
        END
    ELSE
        BEGIN
        Directory := FindDirectory ('trlog');

        UserNameString := GetResponse ('Please enter your name and call : ');

        IF UserNameString = '' THEN Halt;

        OpenFileForWrite (FileWrite, Directory + 'NAME.DAT');
        WriteLn (FileWrite, UserNameString);
        WriteLn (FileWrite, AddressString);
        WriteLn (FileWrite, CityString);
        Close (FileWrite);
        END;
    END;

PROCEDURE LoopBackTest;

VAR Key, RXChar: CHAR;
    BaudString: Str20;
    BaudRate, Result: LONGINT;
    SevenBitMode: BOOLEAN;
    Parity: ParityType;
    FileWrite: FILE;
    dev: str80;
    p : serialportx;
    c : char;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('SERIAL PORT LOOP BACK TEST');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('This procedure will test the serial port to see if it appears to be working.');
    WriteLn ('It can also be used as a primative dumb terminal for testing TNCs and the');
    WriteLn ('like.  The data you get back will be saved in the binary file LOOPBACK.BIN');
    WriteLn;
    WriteLn ('Please connect pin 2 to pin 3 if you are doing a loopback test.');
    WriteLn;

    dev := getresponse('Enter device name for serial port: ');

    REPEAT
        BaudString := UpperCase (GetResponse ('Baud rate (300, 1200, 2400, 4800, 9600, 19200 or 57600) : '));
        IF BaudString = '' THEN Exit;
    UNTIL (BaudString =  '300') OR (BaudString = '1200') OR
          (BaudString = '2400') OR (BaudString = '4800') OR
          (BaudString = '9600') OR (BaudString = '19200') OR
          (BaudString = '57600');

    Val (BaudString, BaudRate, Result);

    REPEAT
        Key := UpCase (GetKey ('7 or 8 bits? : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = '7') OR (Key = '8');
    WriteLn;

    SevenBitMode := Key = '7';

    REPEAT
        Key := UpCase (GetKey ('(E)ven, (O)dd or (N)o parity? (E/O/N) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'E') OR (Key = 'O') OR (Key = 'N');
    WriteLn;

    CASE Key OF
        'E': Parity := EvenParity;
        'O': Parity := OddParity;
        'N': Parity := NoParity;
        END;

    p := serialportx.create(dev);
    IF SevenBitMode THEN
        p.setparams(BaudRate,7,Parity,1)
    ELSE
        p.setparams(BaudRate,8,Parity,2);

    WriteLn;
    WriteLn ('Okay, you are all set.  Press ESCAPE to stop.');
    WriteLn;

    Assign (FileWrite, 'LOOPBACK.BIN');
    Rewrite (FileWrite, 1);

    REPEAT
        IF KeyPressed THEN
            BEGIN
            Key := ReadKey;

            IF Key = EscapeKey THEN
                BEGIN
                Close (FileWrite);
                Exit;
                END;

            p.putchar(Key);
            END;

        IF p.CharReady THEN
            BEGIN
            RXChar := p.ReadChar;
            BlockWrite (FileWrite, RXChar, 1);

            IF SevenBitMode THEN
                RXChar := Chr (Ord (RXChar) AND $7F);

            Write (RXChar);
            END;

    UNTIL False;
    END;



PROCEDURE PortToFile;

VAR Key: CHAR;
    FileName, BaudString: Str20;
    FileWrite: FILE;
    Result, BaudRate: LONGINT;
    RChar: CHAR;
    dev: str80;
    p : serialportx;
    c : char;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('PORT TO FILE');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('This procedure will take whatever data is coming in on the serial port ');
    WriteLn ('specified and dump it into a file.  Any keys pressed will be sent to the');
    WriteLn ('the port.');
    WriteLn;

    dev := getresponse('Enter device name for serial port to test: ');

    WriteLn;

    REPEAT
        BaudString := UpperCase (GetResponse ('Enter baud rate : '));
        IF BaudString = '' THEN Exit;
    UNTIL (BaudString =  '300') OR (BaudString = '1200') OR
          (BaudString = '2400') OR (BaudString = '4800') OR
          (BaudString = '9600');

    WriteLn;

    FileName := GetResponse ('Enter filename to save data to : ');

    IF FileName = '' THEN Exit;

    Assign  (FileWrite, Filename);
    ReWrite (FileWrite, 1);


    Val (BaudString, BaudRate, Result);

    p := serialportx.create(dev);
    p.setparams(BaudRate,8,NoParity,1);

    WriteLn ('Saving data to file.  Use the ESCAPE key to exit.');

    REPEAT
        IF KeyPressed THEN
            BEGIN
            Key := ReadKey;

            IF Key = EscapeKey THEN
                BEGIN
                Close (FileWrite);
                Exit;
                END;

            p.putchar(Key);
            END;

        IF p.CharReady THEN
            BEGIN
            RChar := p.ReadChar;
            BlockWrite (FileWrite, RChar, SizeOf (RChar), Result);
            Write (RChar);
            END;

    UNTIL False;
    END;



PROCEDURE DisplayEditableLogString (LineData: Str80; Cursor: INTEGER);

VAR Inverse: BOOLEAN;
    Block:   INTEGER;

    BEGIN
    GoToXY (1, WhereY);

    WHILE Length (LineData) < 79 DO
        LineData := LineData + ' ';

    IF Length (LineData) > 79 THEN
        BEGIN
        LineData := Copy (LineData, 1, 78);
        LineData := LineData + '+';
        END;

    FOR Block := 1 TO 10 DO
        BEGIN
        Inverse := False;

        IF ActiveExchange = RSTNameAndQTHExchange THEN
            BEGIN
            CASE Block OF
                1: IF ((Cursor >= 1)  AND (Cursor <= 7))  THEN Inverse := True;
                2: IF ((Cursor >= 8)  AND (Cursor <= 17)) THEN Inverse := True;
                3: IF ((Cursor >= 18) AND (Cursor <= 23)) THEN Inverse := True;
                4: IF ((Cursor >= 24) AND (Cursor <= 29)) THEN Inverse := True;
                5: IF ((Cursor >= 30) AND (Cursor <= 41)) THEN Inverse := True;
                6: IF ((Cursor >= 45) AND (Cursor <= 47)) THEN Inverse := True;
                7: IF ((Cursor >= 50) AND (Cursor <= 52)) THEN Inverse := True;
                8: IF ((Cursor >= 54) AND (Cursor <= 77)) THEN Inverse := True;
                9: IF ((Cursor >= 77) AND (Cursor <= 78)) THEN Inverse := True;
                END;
            END
        ELSE
            CASE Block OF
                 1: IF ((Cursor >= 1)  AND (Cursor <= 7))  THEN Inverse := True;
                 2: IF ((Cursor >= 8)  AND (Cursor <= 17)) THEN Inverse := True;
                 3: IF ((Cursor >= 18) AND (Cursor <= 23)) THEN Inverse := True;
                 4: IF ((Cursor >= 24) AND (Cursor <= 29)) THEN Inverse := True;
                 5: IF ((Cursor >= 30) AND (Cursor <= 41)) THEN Inverse := True;
                 6: IF ((Cursor >= 42) AND (Cursor <= 43)) THEN Inverse := True;
                 7: IF ((Cursor >= 44) AND (Cursor <= 68)) THEN Inverse := True;
                 8: IF ((Cursor >= 69) AND (Cursor <= 76)) THEN Inverse := True;
                 9: IF ((Cursor >= 77) AND (Cursor <= 78)) THEN Inverse := True;
                END;

        IF Inverse THEN
            BEGIN
            TextColor (ActiveBackground);
            TextBackground (ActiveColor);
            END
        ELSE
            BEGIN
            TextColor (ActiveColor);
            TextBackground (ActiveBackground);
            END;

        IF ActiveExchange = RSTNameAndQTHExchange THEN
            BEGIN
            CASE Block OF
                1: Write (Copy (LineData,  1,  7));
                2: Write (Copy (LineData,  8, 10));
                3: Write (Copy (LineData, 18,  6));
                4: Write (Copy (LineData, 24,  6));
                5: Write (Copy (LineData, 30, 15));
                6: Write (Copy (LineData, 45,  5));
                7: Write (Copy (LineData, 50,  4));
                8: Write (Copy (LineData, 54, 23));
                9: Write (Copy (LineData, 77,  2));
                END;
            END
        ELSE
            BEGIN
            CASE Block OF
                1: Write (Copy (LineData,  1,  7));
                2: Write (Copy (LineData,  8, 10));
                3: Write (Copy (LineData, 18,  6));
                4: Write (Copy (LineData, 24,  6));
                5: Write (Copy (LineData, 30, 12));
                6: Write (Copy (LineData, 42,  2));
                7: Write (Copy (LineData, 44, 25));
                8: Write (Copy (LineData, 69,  8));
                9: Write (Copy (LineData, 77,  2));
               10: Write (Copy (LineData, 79,  1));
                END;
            END;
        END;

    IF Cursor < 80 THEN
        BEGIN
        GoToXY (Cursor, WhereY);
        TextColor (ActiveBackground);
        TextBackground (ActiveColor);
        END;
    END;



PROCEDURE EditWindowEditor (VAR EditLines: LogEntryArray;
                            CursorX: INTEGER; CursorY: INTEGER;
                            VAR DataChanged: BOOLEAN);

VAR CharPointer, CursorPosition, InsertCursorPosition, NewCursor, Line: INTEGER;
    PreviousCursorChar: CHAR;
    WindowString, TempString: Str160;
    KeyChar: CHAR;
    InsertMode: BOOLEAN;
    OriginalEditLines: LogEntryArray;
    TimeMark: TimeRecord;

    BEGIN
    DataChanged := False;
    OriginalEditLines := EditLines;
    InsertMode := False;
    DisplayInsertMode (InsertMode);
    SaveAndSetActiveWindow (EditableLogWindow);
    GoToXY (CursorX, CursorY);

    WindowString := EditLines [CursorY];
    DisplayEditableLogString (WindowString, CursorX);

    REPEAT
        MarkTime (TimeMark);

        REPEAT
            UpdateTimeAndRateDisplays (True, False);

            IF ElaspedSec100 (TimeMark) > 2000 THEN
                BEGIN
                EditLines [WhereY] := WindowString;
                DisplayEditableLogString (WindowString, 80);
                EditLines := OriginalEditLines;
                ClrScr;

                FOR Line := 1 TO 5 DO
                    BEGIN
                    GoToXY (1, Line);
                    DisplayEditableLogString (EditLines [Line], 80);
                    END;
                DataChanged := False;
                RestorePreviousWindow;
                Exit;
                END;

        UNTIL (KeyPressed);

        KeyChar := ReadKey;
        CursorPosition := WhereX;

        CASE KeyChar OF

          EscapeKey:
             BEGIN
             EditLines [WhereY] := WindowString;
             DisplayEditableLogString (WindowString, 80);

             IF DataChanged AND ConfirmEditChanges THEN
                 BEGIN
                 TempString := UpperCase (QuickEditResponse ('Save changes? (Y/N) : ', 1));

                 IF TempString [1] <> 'Y' THEN
                     BEGIN
                     EditLines := OriginalEditLines;
                     ClrScr;
                     FOR Line := 1 TO 5 DO
                         BEGIN
                         GoToXY (1, Line);
                         DisplayEditableLogString (EditLines [Line], 80);
                         END;
                     DataChanged := False;
                     END;
                 END;

             RestorePreviousWindow;
             Exit;
             END;



         TabKey:
             IF TabMode = NormalTabMode THEN
                 BEGIN
                 NewCursor := 8;
                 IF ActiveExchange = RSTNameAndQTHExchange THEN
                     BEGIN
                     IF CursorPosition >  7 THEN NewCursor := 18;
                     IF CursorPosition > 17 THEN NewCursor := 24;
                     IF CursorPosition > 23 THEN NewCursor := 30;
                     IF CursorPosition > 29 THEN NewCursor := 45;
                     IF CursorPosition > 44 THEN NewCursor := 50;
                     IF CursorPosition > 49 THEN NewCursor := 54;
                     IF CursorPosition > 53 THEN NewCursor := 77;
                     IF CursorPosition > 77 THEN
                         IF WhereY < NumberEditableLines THEN
                             BEGIN
                             EditLines [WhereY] := WindowString;
                             DisplayEditableLogString (WindowString, 80);
                             GoToXY (1, WhereY + 1);
                             WindowString := EditLines [WhereY];
                             NewCursor := 1;
                             END;
                     END
                 ELSE
                     BEGIN
                     IF CursorPosition >  7 THEN NewCursor := 18;
                     IF CursorPosition > 17 THEN NewCursor := 24;
                     IF CursorPosition > 23 THEN NewCursor := 30;
                     IF CursorPosition > 29 THEN NewCursor := 42;
                     IF CursorPosition > 41 THEN NewCursor := 44;
                     IF CursorPosition > 43 THEN NewCursor := 69;
                     IF CursorPosition > 68 THEN NewCursor := 77;
                     IF CursorPosition > 77 THEN
                         IF WhereY < NumberEditableLines THEN
                             BEGIN
                             EditLines [WhereY] := WindowString;
                             DisplayEditableLogString (WindowString, 80);
                             GoToXY (1, WhereY + 1);
                             WindowString := EditLines [WhereY];
                             NewCursor := 1;
                             END;
                     END;

                 DisplayEditableLogString (WindowString, NewCursor);
                 END
             ELSE
                 IF CursorPosition <= Length (WindowString) THEN
                     BEGIN
                     REPEAT
                         PreviousCursorChar := WindowString [CursorPosition];
                         Inc (CursorPosition);
                     UNTIL ((WindowString [CursorPosition] <> ' ') AND
                           (PreviousCursorChar = ' ')) OR (CursorPosition = Length (WindowString) + 1);
                     DisplayEditableLogString (WindowString, CursorPosition);
                     END
                 ELSE
                     IF WhereY < NumberEditableLines THEN
                         BEGIN
                         EditLines [WhereY] := WindowString;
                         DisplayEditableLogString (WindowString, 80);
                         GoToXY (1, WhereY + 1);
                         WindowString := EditLines [WhereY];
                         DisplayEditableLogString (WindowString, 1);
                         END;


         ControlA:
             IF CursorPosition > 1 THEN
                 BEGIN
                 REPEAT
                     Dec (CursorPosition);
                     PreviousCursorChar := WindowString [CursorPosition - 1];
                 UNTIL ((WindowString [CursorPosition] <> ' ') AND
                        (PreviousCursorChar = ' ')) OR (CursorPosition = 1);

                 DisplayEditableLogString (WindowString, CursorPosition);
                 END
             ELSE
                 IF WhereY > 1 THEN
                     BEGIN
                     EditLines [WhereY] := WindowString;
                     DisplayEditableLogString (WindowString, 80);
                     WindowString := EditLines [WhereY];
                     DisplayEditableLogString (WindowString, 79);
                     END;


            ControlC:
                BEGIN
                EditLines [WhereY] := WindowString;
                DisplayEditableLogString (WindowString, 80);
                WindowString := EditLines [NumberEditableLines];
                DisplayEditableLogString (WindowString, CursorPosition);
                END;

            ControlD:
                BEGIN
                IF CursorPosition <= Length (WindowString) THEN
                    Inc (CursorPosition);
                DisplayEditableLogString (WindowString, CursorPosition);
                END;

            ControlE, ControlW:
                IF WhereY > 1 THEN
                    BEGIN
                    EditLines [WhereY] := WindowString;
                    DisplayEditableLogString (WindowString, 80);
                    GoToXY (1, WhereY - 1);
                    WindowString := EditLines [WhereY];
                    DisplayEditableLogString (WindowString, CursorPosition);
                    END;

            ControlF:
                IF CursorPosition <= Length (WindowString) THEN
                    BEGIN
                    REPEAT
                        PreviousCursorChar := WindowString [CursorPosition];
                        Inc (CursorPosition);
                    UNTIL ((WindowString [CursorPosition] <> ' ') AND
                          (PreviousCursorChar = ' ')) OR (CursorPosition = Length (WindowString) + 1);
                    DisplayEditableLogString (WindowString, CursorPosition);
                    END
                ELSE
                    IF WhereY < NumberEditableLines THEN
                        BEGIN
                        EditLines [WhereY] := WindowString;
                        DisplayEditableLogString (WindowString, 80);
                        GoToXY (1, WhereY + 1);
                        WindowString := EditLines [WhereY];
                        DisplayEditableLogString (WindowString, 1);
                        END;

            ControlS:
                BEGIN
                IF CursorPosition > 1 THEN
                    Dec (CursorPosition);
                DisplayEditableLogString (WindowString, CursorPosition);
                END;

            ControlY:
                BEGIN
                WindowString := '                                                                            ';
                CursorPosition := 1;
                DisplayEditableLogString (WindowString, CursorPosition);
                DataChanged := True;
                END;

            ControlX, ControlZ:
                IF WhereY < NumberEditableLines THEN
                    BEGIN
                    EditLines [WhereY] := WindowString;
                    DisplayEditableLogString (WindowString, 80);
                    GoToXY (WhereX, WhereY + 1);
                    WindowString := EditLines [WhereY];
                    DisplayEditableLogString (WindowString, CursorPosition);
                    END;

            CarriageReturn:
                IF WhereY < NumberEditableLines THEN
                    BEGIN
                    EditLines [WhereY] := WindowString;
                    DisplayEditableLogString (WindowString, 80);
                    GoToXY (1, WhereY + 1);
                    WindowString := EditLines [WhereY];
                    DisplayEditableLogString (WindowString, 1);
                    END;

            BackSpace:
                IF CursorPosition > 1 THEN
                    BEGIN
                    Dec (CursorPosition);
                    WindowString [CursorPosition] := ' ';
                    DisplayEditableLogString (WindowString, CursorPosition);
                    END;

            NullCharacter:
                BEGIN
                KeyChar := ReadKey;

                CASE KeyChar OF
                    UpArrow:
                      IF WhereY > 1 THEN
                        BEGIN
                        EditLines [WhereY] := WindowString;
                        DisplayEditableLogString (WindowString, 80);
                        GoToXY (CursorPosition, WhereY - 1);
                        WindowString := EditLines [WhereY];
                        DisplayEditableLogString (WindowString, CursorPosition);
                        END;



                    DownArrow:
                      IF WhereY < NumberEditableLines THEN
                          BEGIN
                          EditLines [WhereY] := WindowString;
                          DisplayEditableLogString (WindowString, 80);
                          GoToXY (CursorPosition, WhereY + 1);
                          WindowString := EditLines [WhereY];
                          DisplayEditableLogString (WindowString, CursorPosition);
                          END
                      ELSE
                          BEGIN
                          EditLines [WhereY] := WindowString;
                          DisplayEditableLogString (WindowString, 80);

                          IF DataChanged AND ConfirmEditChanges THEN
                              BEGIN
                              TempString := UpperCase (QuickEditResponse ('Save changes? (Y/N) : ', 1));

                              IF TempString [1] <> 'Y' THEN
                                  BEGIN
                                  EditLines := OriginalEditLines;
                                  ClrScr;
                                  FOR Line := 1 TO 5 DO
                                      BEGIN
                                      GoToXY (1, Line);
                                      DisplayEditableLogString (EditLines [Line], 80);
                                      END;
                                  DataChanged := False;
                                  END;
                              END;

                          RestorePreviousWindow;
                          Exit;
                          END;


                    LeftArrow:
                      BEGIN
                      IF CursorPosition > 1 THEN
                          Dec (CursorPosition);
                      DisplayEditableLogString (WindowString, CursorPosition);
                      END;

                    RightArrow:
                      BEGIN
                      IF CursorPosition <= Length (WindowString) THEN
                          Inc (CursorPosition);
                      DisplayEditableLogString (WindowString, CursorPosition);
                      END;

                    HomeKey:
                      BEGIN
                      EditLines [WhereY] := WindowString;
                      DisplayEditableLogString (WindowString, 80);
                      GoToXY (1, 1);
                      WindowString := EditLines [1];
                      DisplayEditableLogString (WindowString, 1);
                      END;

                    EndKey:
                      BEGIN
                      EditLines [WhereY] := WindowString;
                      DisplayEditableLogString (WindowString, 80);
                      GoToXY (1, NumberEditableLines);
                      WindowString := EditLines [NumberEditableLines];
                      DisplayEditableLogString (WindowString, 1);
                      END;

                    ShiftTab:
                        IF TabMode = NormalTabMode THEN
                            BEGIN
                            NewCursor := 69;
                            IF CursorPosition < 77 THEN NewCursor := 44;
                            IF CursorPosition < 69 THEN NewCursor := 42;
                            IF CursorPosition < 44 THEN NewCursor := 30;
                            IF CursorPosition < 42 THEN NewCursor := 24;
                            IF CursorPosition < 30 THEN NewCursor := 18;
                            IF CursorPosition < 24 THEN NewCursor :=  8;
                            IF CursorPosition < 18 THEN NewCursor :=  1;
                            IF CursorPosition < 8  THEN
                                IF WhereY > 1 THEN
                                    BEGIN
                                    EditLines [WhereY] := WindowString;
                                    DisplayEditableLogString (WindowString, 80);
                                    GoToXY (1, WhereY - 1);
                                    WindowString := EditLines [WhereY];
                                    NewCursor := 77;
                                    END;
                            DisplayEditableLogString (WindowString, NewCursor);
                            END
                        ELSE
                            IF CursorPosition > 1 THEN
                                BEGIN
                                REPEAT
                                    Dec (CursorPosition);
                                    PreviousCursorChar := WindowString [CursorPosition - 1];
                                UNTIL ((WindowString [CursorPosition] <> ' ') AND
                                       (PreviousCursorChar = ' ')) OR (CursorPosition = 1);

                                DisplayEditableLogString (WindowString, CursorPosition);
                                END
                            ELSE
                                IF WhereY > 1 THEN
                                    BEGIN
                                    EditLines [WhereY] := WindowString;
                                    DisplayEditableLogString (WindowString, 80);
                                    WindowString := EditLines [WhereY];
                                    DisplayEditableLogString (WindowString, 79);
                                    END;


                    END;  { of case }
                END;   { of null string case }



            ELSE
              BEGIN
              IF KeyChar >= ' ' THEN
                  IF CursorPosition <= Length (WindowString) THEN
                      BEGIN
                      WindowString [CursorPosition] := KeyChar;
                      DisplayEditableLogString (WindowString, CursorPosition + 1);
                      DataChanged := True;
                      END
                  ELSE
                      IF CursorPosition < 79 THEN
                          BEGIN
                          WindowString := WindowString + KeyChar;
                          DisplayEditableLogString (WindowString, CursorPosition + 1);
                          DataChanged := True;
                          END;

              END;
            END;  { of case }

    UNTIL False;
    END;



PROCEDURE LoadPageBuffer (BufferNumber: INTEGER);

{ This procedure will load up the PageBuffer with one screen full of text
  from the file indicated by FileRead.  If the end of the file is found,
  the rest of the PageBuffer will be blank. }

VAR Result, NumberLines: INTEGER;
    FileChar: CHAR;
    TempString: Str80;

    BEGIN
    TextBuffer [BufferNumber]^.NumberChars := 0;
    NumberLines := 0;

    WHILE NOT Eof (LogFileRead) DO
        BEGIN
        Read (LogFileRead, FileChar);

        IF FileChar = LineFeed THEN
            BEGIN
            Inc (NumberLines);
            IF NumberLines >= BigWindowRY - BigWindowLY + 1 THEN
                Exit;
            END;

        TextBuffer [BufferNumber]^.List [TextBuffer [BufferNumber]^.NumberChars] := FileChar;
        Inc (TextBuffer [BufferNumber]^.NumberChars);
        END;
    END;



PROCEDURE DisplayBuffer (BufferNumber: INTEGER);

VAR Address: INTEGER;

    BEGIN
    ClrScr;

    IF TextBuffer [BufferNumber]^.NumberChars > 0 THEN
        FOR Address := 0 TO TextBuffer [BufferNumber]^.NumberChars - 1 DO
           BEGIN
            IF TextBuffer [BufferNumber]^.List [Address] = ControlL THEN
               Write (' ')
            ELSE
               Write (TextBuffer [BufferNumber]^.List [Address]);
           END;

    DisplayedBuffer := BufferNumber;
    END;



PROCEDURE StartLogFileView (FileName: Str80);

    BEGIN
    IF MaxAvail < SizeOf (PageBuffer) THEN Exit;

    FirstBuffer     := 0;
    NumberBuffers   := 1;

    New (TextBuffer [FirstBuffer]);

    Assign (LogFileRead, FileName);
    Reset  (LogFileRead);

    LoadPageBuffer (FirstBuffer);
    DisplayBuffer  (FirstBuffer);

    IF (NOT Eof (LogFileRead)) AND (MaxAvail > SizeOf (PageBuffer)) THEN
        BEGIN
        New (TextBuffer [NumberBuffers]);
        LoadPageBuffer (NumberBuffers);
        Inc (NumberBuffers);
        END;
    END;



PROCEDURE LoadNextPage;

    BEGIN
    IF NOT Eof (LogFileRead) THEN               { there is more to load }
        BEGIN
        IF (MaxAvail > SizeOf (PageBuffer)) AND (NumberBuffers < MaxBuffers - 1) THEN  { there is more memory }
            BEGIN
            New (TextBuffer [NumberBuffers]);
            LoadPageBuffer (NumberBuffers);
            Inc (NumberBuffers);
            END
        ELSE                                    { no new buffers, wrap }
            BEGIN
            LoadPageBuffer (FirstBuffer);
            Inc (FirstBuffer);
            IF FirstBuffer = NumberBuffers THEN FirstBuffer := 0;
            END;
        END;
    END;



PROCEDURE ShowNextPage;

{ This procedure will display the next page and load the next page after
  that into the appropriate buffer.   }

    BEGIN
    IF NumberBuffers > 1 THEN                            { multiple buffers }
        BEGIN
        Inc (DisplayedBuffer);                           { is already loaded }

        IF DisplayedBuffer = NumberBuffers THEN          { need to wrap to 0 }
            DisplayedBuffer := 0;

        IF DisplayedBuffer <> FirstBuffer THEN
            DisplayBuffer (DisplayedBuffer)
        ELSE
            BEGIN
            Dec (DisplayedBuffer);
            IF DisplayedBuffer < 0 THEN DisplayedBuffer := NumberBuffers - 1;
            END;

        LoadNextPage;
        END
    ELSE                                    { we are using just one buffer }
        IF NOT Eof (LogFileRead) THEN
            BEGIN
            LoadPageBuffer (0);
            DisplayBuffer (0);
            END;
    END;



PROCEDURE ShowPreviousPage;

    BEGIN
    IF DisplayedBuffer <> FirstBuffer THEN
        BEGIN
        Dec (DisplayedBuffer);

        IF DisplayedBuffer < 0 THEN                  { need to wrap }
            DisplayedBuffer := NumberBuffers - 1;

        DisplayBuffer (DisplayedBuffer);
        END;
    END;



PROCEDURE ViewRadioDebug;

VAR Key: CHAR;
    FileString, FileName, TempString: STRING;
    FileRead: FILE;
    Lines, RecordNumber, Result: INTEGER;
    Band: BandType;
    Mode: ModeType;
    Freq: LONGINT;
    RealFreq: REAL;

    BEGIN
    ClrScr;
    TextColor (Yellow);

    WriteLnCenter ('Look at Radio1Type.DBG file');
    WriteLn;

    TextColor (Cyan);

    WriteLn ('This procedure will allow you to look at a Radio1Type.DddBG file and have the');
    WriteLn ('program calculate the frequency and mode from that data using the same');
    WriteLn ('algorithm that the program would us if it was interfaced to that Radio1Type.');
    WriteLn;

    FileName := GetResponse ('Enter filename of Radio1Type debug file to process : ');
    IF FIleName = '' THEN Exit;

    IF NOT FileExists (FileName) THEN
        BEGIN
        ReportError ('Unable to find ' + FileName);
        Exit;
        END;

    REPEAT
        Key := UpCase (GetKey ('Enter Radio1Type type: (K)enwood (Y)aesu (J)ST (I)com or (T)enTec : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'K') OR (Key = 'Y') OR (Key = 'J') OR (Key = 'I') OR (Key = 'T');
    WriteLn;

    CASE Key OF
        'K': Radio1Type := TS850; {KK1L: 6.73 NOTE This covers the K2 case as well!}

        'J': Radio1Type := JST245;

        'I': Radio1Type := IC781; {KK1L: 6.73 Was IC735. This rig uses the wrong freq data length}

        'T': BEGIN
             REPEAT
                 Key := UpCase (GetKey ('Enter 1 for Omni-VI or 2 for Orion : '));
                 IF Key = EscapeKey THEN Exit;
             UNTIL (Key = '1') OR (Key = '2');
             WriteLn;

             IF Key = '1' THEN Radio1Type := OMNI6 ELSE Radio1Type := Orion;
             END;

        'Y': BEGIN
             REPEAT
                 TempString := UpperCase (GetResponse ('Enter full model number (i.e., FT1000MP) : '));

                 IF TempString = '' THEN Exit;

                 Radio1Type := NoInterfacedRadio;

                 IF TempString = 'FT1000MP' THEN Radio1Type := FT1000MP;
                 IF TempString = 'FT1000'   THEN Radio1Type := FT1000;
                 IF TempString = 'FT100'    THEN Radio1Type := FT100;
                 IF TempString = 'FT847'    THEN Radio1Type := FT847;
                 IF TempString = 'FT890'    THEN Radio1Type := FT890;
                 IF TempString = 'FT920'    THEN Radio1Type := FT920;
                 IF TempString = 'FT990'    THEN Radio1Type := FT990;
             UNTIL Radio1Type <> NoInterfacedRadio;
             END;
        END;

    Assign  (FileRead, FileName);
    Reset   (FileRead, 1);

    RecordNumber := 1;
    Lines := 7;

    TextColor (Cyan);

    WHILE NOT Eof (FileRead) DO
        BEGIN
        BlockRead (FileRead, FileString, SizeOf (FileString), Result);

        IF GetRadioParameters (RadioOne, FileString, Freq, Band, Mode, TRUE, False) THEN
            BEGIN
            RealFreq := Freq / 1000;
            WriteLn ('Record #', RecordNumber, ' ', RealFreq:8:3, ' kHz  ', BandString [Band], ModeString [Mode]);
            END
        ELSE
            WriteLn ('Error found at record ', RecordNumber);

        Inc (RecordNumber);
        Inc (Lines);

        IF (Lines >= 22) AND NOT Eof (FileRead) THEN
            BEGIN
            WaitForKeyPressed;
            Lines := 1;
            TextColor (Cyan);
            ClrScr;
            END;

        END;

    Close (FileRead);
    END;



PROCEDURE ViewLogFile;

VAR Buffer: INTEGER;
    Key: CHAR;
    TempString: Str20;
    TimeMark: TimeRecord;
    FileName: Str80;

    BEGIN
    IF QTCsEnabled THEN
        BEGIN

        REPEAT
            TempString := UpperCase (QuickEditResponse ('(Q)TC file or (L)og file view? (Q/L) : ', 1));
            IF TempString = EscapeKey THEN Exit;
            Key := TempString [1];
        UNTIL (Key = 'Q') OR (Key = 'L');

        IF Key = 'Q' THEN
            FileName := QTCFileName
        ELSE
            FileName := LogFileName;
        END
    ELSE
        FileName := LogFileName;

    QuickDisplay ('File view.  Use PageUp/Down, HOME and END.  ESCAPE to quit');

    SaveAndSetActiveWindow (BigWindow);
    StartLogFileView (FileName);
    DisplayFreeMemory;

    IF Key = 'Q' THEN { Go to end of file as starting point }
        BEGIN
        ClrScr;
        Write ('Reading file...  Use ESCAPE to stop.');

        WHILE (NOT Eof (LogFileRead)) AND (NOT KeyPressed) DO
            LoadNextPage;

        IF KeyPressed THEN
            WHILE KeyPressed DO Key := ReadKey;

        DisplayedBuffer := FirstBuffer - 1;
        IF DisplayedBuffer < 0 THEN
            DisplayedBuffer := NumberBuffers - 1;
        DisplayBuffer (DisplayedBuffer);
        END;

    MarkTime (TimeMark);

    REPEAT
        WHILE KeyPressed DO ReadKey;

        REPEAT
            UpdateTimeAndRateDisplays (False, False);

        IF ActiveMultiPort <> nil THEN
            IF ElaspedSec100 (TimeMark) > 2000 THEN
                BEGIN
                RestorePreviousWindow;
                RemoveWindow (QuickCommandWindow);
                Close (LogFileRead);

                FOR Buffer := 0 TO NumberBuffers - 1 DO
                    Dispose (TextBuffer [Buffer]);
                DisplayFreeMemory;
                Exit;
                END;

        millisleep;
        UNTIL KeyPressed;

        Key := ReadKey;

        MarkTime (TimeMark);

        CASE Key OF
            NullKey:
                BEGIN
                Key := ReadKey;

                CASE Key OF
                    PageUpKey:   ShowPreviousPage;

                    PageDownKey: ShowNextPage;

                    HomeKey:
                        BEGIN
                        IF MaxAvail > SizeOf (PageBuffer) THEN
                            DisplayBuffer (0)
                        ELSE
                            BEGIN
                            Close (LogFileRead);
                            FOR Buffer := 0 TO NumberBuffers - 1 DO
                                Dispose (TextBuffer [Buffer]);
                            StartLogFileView (FileName);
                            END;
                        END;

                    EndKey:
                        BEGIN
                        ClrScr;
                        Write ('Reading file...  Use ESCAPE to stop.');

                        WHILE (NOT Eof (LogFileRead)) AND (NOT KeyPressed) DO
                            LoadNextPage;

                        IF KeyPressed THEN
                            WHILE KeyPressed DO Key := ReadKey;

                        DisplayedBuffer := FirstBuffer - 1;
                        IF DisplayedBuffer < 0 THEN
                            DisplayedBuffer := NumberBuffers - 1;
                        DisplayBuffer (DisplayedBuffer);
                        END;

                    END;
                END;

            EscapeKey:
                BEGIN
                RestorePreviousWindow;
                RemoveWindow (QuickCommandWindow);
                Close (LogFileRead);

                FOR Buffer := 0 TO NumberBuffers - 1 DO
                    Dispose (TextBuffer [Buffer]);
                DisplayFreeMemory;
                Exit;
                END;

            END;

    DisplayFreeMemory;
    UNTIL False;
    END;



PROCEDURE SetAlarm;

VAR Hour, Minute, Second, Sec100: Word;
    TempString, HourTempString, MinuteTempString: Str80;
    CharPointer, Result, Year, Month, Day: INTEGER;

    BEGIN
    IF AlarmSet THEN
        BEGIN
        RemoveWindow (AlarmWindow);
        AlarmSet := False;
        Exit;
        END;

    SaveSetAndClearActiveWindow (QuickCommandWindow);

    REPEAT
        ClrScr;

        TempString := LineInput ('Enter alarm time (HH:MM) : ', '', False, False);

        IF (TempString = '') OR (TempString = EscapeKey) THEN
            BEGIN
            RemoveAndRestorePreviousWindow;
            Exit;
            END;

    UNTIL TempString [3] = ':';

    HourTempString := '';

    CharPointer := 1;

    WHILE TempString [CharPointer] <> ':' DO
        BEGIN
        HourTempString [CharPointer] := TempString [CharPointer];
        Inc (CharPointer);
        END;

    HourTempString [0] := Chr (CharPointer - 1);

    Inc (CharPointer);

    MinuteTempString [1] := TempString [CharPointer];
    Inc (CharPointer);
    MinuteTempString [2] := TempString [CharPointer];
    MinuteTempString [0] := Chr (2);

    Val (HourTempString, AlarmHour, Result);
    Val (MinuteTempString, AlarmMinute, Result);

    RemoveAndRestorePreviousWindow;
    SaveSetAndClearActiveWindow (AlarmWindow);

    Write (' Alarm = ', HourTempString, ':', MinuteTempString);
    AlarmSet := True;
    RestorePreviousWindow;
    END;



PROCEDURE ContextHelp (Subject: HelpSubject);

VAR Key: CHAR;

    BEGIN
    RemoveWindow (EditableLogWindow);
    RemoveWindow (TotalWindow);

    SaveSetAndClearActiveWindow (BigWindow);
    ClrScr;
    WriteLnCenter ('HELP MENU');
    WriteLn;

    CASE Subject OF
        NoSubject:
            BEGIN
            END;
        END;

    WriteLn;
    Write ('Press any key to continue...');
    RestorePreviousWindow;

    REPEAT
        UpdateTimeAndRateDisplays (False, False)
    UNTIL KeyPressed;

    Key := ReadKey;
    IF Key = NullKey THEN Key := ReadKey;
    END;



PROCEDURE ProcessAltHelp;

    BEGIN
    CASE ReadKey OF
        AltDash:
             BEGIN
             WriteLn ('The Alt-- (Alt Dash) command will toggle the Auto Send feature if the AUTO');
             WriteLn ('SEND CHARACTER COUNT has been set to something greater than zero.  When ');
             WriteLn ('enabled, an arrow will appear above the call window showing you the cursor');
             WriteLn ('position where CW will be automatically started when entering a callsign.');
             WriteLn ('When the Auto Send feature is disabled, this arrow will disappear.');
             END;

        AltEqual:
             BEGIN
             WriteLn ('The Alt-= does two different things depending on the active mode.');
             WriteLn;
             WriteLn ('If you are in the CW mode, this command will toggle the sidetone on or off.');
             WriteLn ('If the sidetone was set to zero in LOGCFG.DAT, it will be turned on at 700');
             WriteLn ('hertz.');
             WriteLn;
             WriteLn ('If you are in the SSB mode and the DVP is enabled, you will be shown a list');
             WriteLn ('of the backcopy files you have made and can review or delete them.');
             END;

        AltA:
             BEGIN
             WriteLn ('The Alt-A has two functions.  If you have entered some characters in the call');
             WriteLn ('window and have specified a SCP FILENAME (e.g. MASTER.DTA) then a manual super');
             WriteLn ('check partial will be performmed.  Otherwise, this command will allow you to');
             WriteLn ('control a built in alarm clock.');
             WriteLn;
             WriteLn ('When setting the alarm, you will be asked to enter the time you want the alarm');
             WriteLn ('sound.  This time will be displayed in the lower right corner of the screen.');
             WriteLn ('To stop or clear the alarm, press Alt-A again.  It will signal you every few');
             WriteLn ('minutes until you turn it off.');
             WriteLn;
             WriteLn ('When performming a super check partial, the results are shown in the editable');
             WriteLn ('log window.  Press Alt-A again to clear the results.  A minimum of 2 characters');
             WriteLn ('need to be entered and you can use question marks to match any character.');
             END;

        AltB:
             BEGIN
             WriteLn ('The Alt-B command will move you up one band.  If the MULTIPLE BANDS flag');
             WriteLn ('has been set to FALSE (as it would automatically in a single band');
             WriteLn ('contest), and you have made at least one QSO, nothing will happen.');
             END;

        AltC:
             BEGIN
             WriteLn ('The Alt-C command will continue the last auto CQ that was set up using the');
             WriteLn ('Alt-Q command.  Remember the PageUp and PageDown keys are redefined while ');
             WriteLn ('you are auto CQing.  Pressing them will change the delay time between CQs.');
             END;

        AltD:
             BEGIN
             WriteLn ('The Alt-D command will perform a dupecheck on the inactive radio''s band and');
             WriteLn ('mode.  You will be asked for the callsign of the station you want to have');
             WriteLn ('checked.  The QSO and multiplier status windows will be updated with the');
             WriteLn ('information for that station.  If you decide to work the station, pressing');
             WriteLn ('the space bar will put you in the Search and Pounce mode with the proper');
             WriteLn ('band and mode set.  After the QSO is complete, you will be returned to the');
             WriteLn ('CQ mode with your original band and mode set.');
             WriteLn;
             WriteLn ('This command is even more powerful if you are using the TWO RADIO MODE on CW.');
             WriteLn ('In this case, pressing the space bar will also call the station as if you');
             WriteLn ('pressed F1, wait for the CW to stop, and then call CQ on the original radio.');
             WriteLn ('Pressing F2 or RETURN will interrupt this CQ and send the exchange on the');
             WriteLn ('second radio.  Finally a CQ will start back on the first radio automatically.');
             END;

        AltE:
             BEGIN
             WriteLn ('The Alt-E command will allow you to edit the editable log which contains the');
             WriteLn ('last five QSOs you have made.  The TAB and Shift-TAB keys can be used to move');
             WriteLn ('the highlighted cursor field forward or backward.  This is a low level edit,');
             WriteLn ('so if you make changes to the QTH, you need to make the changes to the');
             WriteLn ('multiplier field.  This editor will only allow you to overwrite data so the');
             WriteLn ('other columns will not be misaligned by adding characters.');
             WriteLn;
             WriteLn ('When you are done editing the log, press ESCAPE.  You will then be asked if you');
             WriteLn ('want to save the changes you have made, or go back to what you had before');
             WriteLn ('executing the Alt-E command.  CONFIRM EDIT CHANGES can be set FALSE to disable');
             WriteLn ('being asked if you want to save the changes.');
             WriteLn;
             WriteLn ('You can also activate the editor if your cursor is in a blank call window by');
             WriteLn ('pressing the up arrow.');
             END;

        AltF:
             BEGIN
             WriteLn ('The Alt-F command will force a save of the LOG.DAT file to the floppy that');
             WriteLn ('was set up with a FLOPPY FILE SAVE NAME command in LOGCFG.DAT.  If the name');
             WriteLn ('was not set up, no save will be done.  You can make these saves happen auto-');
             WriteLn ('matically by using the FLOPPY FILE SAVE FREQUENCY command in LOGCFG.DAT');
             END;

        AltG:
             BEGIN
             WriteLn ('If there is more than one type of remaining multiplier display, the Alt-G');
             WriteLn ('command will switch between them.  For example, in the CQ WW contest, the');
             WriteLn ('Alt-G command will switch between the remaining country display and the');
             WriteLn ('remaining zone list.  Please note that there is not a remaining prefix list.');
             END;

        AltH:
             BEGIN
             WriteLn ('The Alt-H command enters the online help function that you are now using.');
             END;

        AltI:
             BEGIN
             WriteLn ('The Alt-I command will increment a (received) numeric value in the exchange');
             WriteLn ('window by one.  This is handy when you are searching and pouncing and didn''t');
             WriteLn ('bust through the pileup.');
             END;

        AltJ:
             BEGIN
             WriteLn ('The Alt-J command will toggle the enable for a multiplier bell.  You can tell');
             WriteLn ('it is enabled when the bell rings when pressing Alt-J.');
             END;

        AltK:
             BEGIN
             WriteLn ('The Alt-K command will disable the sending of CW immediately. Sending will be');
             WriteLn ('restored when either another Alt-K command is issued, or when a CW function');
             WriteLn ('key memory is pressed.  While sending is disabled, the code speed window will');
             WriteLn ('show NO CW!!.  This mode is very handy when having to take over a QSO by');
             WriteLn ('sending manually.');
             WriteLn;
             WriteLn ('If you are using the DVP, the Alt-K command will enable or disable DVP sending.');
             END;

        AltL:
             BEGIN
             WriteLn ('The Alt-L command will allow you to look through your LOG.DAT file for all');
             WriteLn ('occurances of a search string.  This can be used to find a previous QSO and');
             WriteLn ('prove to someone that they really are a dupe.  If you are looking for N6AR,');
             WriteLn ('it will help to put a space after the N6AR so you won''t also get entries ');
             WriteLn ('that start with N6AR (ie: N6ARA).  The question mark will match any character.');
             WriteLn ('Searches are case insensitive.  It can be used anywhere except as the first');
             WriteLn ('character of the search string.  If there is something in the call window,');
             WriteLn ('it will be used as a default which you can simply type over if you don''t want');
             WriteLn ('to use it.');
             END;

        AltM:
             BEGIN
             WriteLn ('The Alt-M command will swap modes between CW and SSB.  If the MULTIPLE MODES');
             WriteLn ('ENABLED flag has been set to FALSE (as it would in a single mode contest),');
             WriteLn ('and you have made at least one QSO, nothing will happen.');
             END;

        AltN:
             BEGIN
             WriteLn ('The Alt-N command will put your Kenwood or Yaesu radio into split mode and');
             WriteLn ('set the transmit VFO to the frequency entered.  You can enter just the last');
             WriteLn ('three digits of the frequency in most cases.  You can also activate this');
             WriteLn ('function with the single dash key, allowing you to enter the frequency by');
             WriteLn ('using only the numeric keypad (in NUM LOCK mode).');
             END;

        AltO:
             BEGIN
             WriteLn ('The Alt-O command allows you to add reminders on line without having to stop');
             WriteLn ('the program and edit the LOGCFG.DAT file.  Added reminders will automatically');
             WriteLn ('be written to the LOGCFG.DAT file so they will be remembered in the case of');
             WriteLn ('a power failure.  Control characters can be entered by pressing control-P and');
             WriteLn ('then the control character.');
             END;

        AltP:
             BEGIN
             WriteLn ('The Alt-P command will allow you to change the contents of a function key');
             WriteLn ('memory or other QSO message.  Keep in mind that there are two sets of function');
             WriteLn ('key memories, those for CQ mode and those for the exchange/search and pounce');
             WriteLn ('mode.  Follow the prompts to edit other messages such as CQ Exchange.  In all');
             WriteLn ('cases, you will be shown the existing message which you can either edit or');
             WriteLn ('overwrite.  ESCAPE will exit you from the AltP function with no changes made');
             WriteLn ('to the message.  Your new message will be appended to your LOGCFG.DAT file so');
             WriteLn ('it will be there if you restart the program.');
             END;

        AltQ:
             BEGIN
             WriteLn ('The Alt-Q command will allow you to set up a given memory to be repeated at');
             WriteLn ('selected intervals.  This is handy for making a CQ repeat automatically.');
             WriteLn ('When any key is pressed, the CQ will stop immediately.  If the key is a valid');
             WriteLn ('callsign key, it will be entered into the call window.  To resume the last');
             WriteLn ('auto CQ setup, use the Alt-C command.  While you are auto CQing, you can ');
             WriteLn ('use the PageUp and PageDown keys to change the delay time between messages');
             WriteLn ('in half second steps.');
             END;

        AltR:
             BEGIN
             WriteLn ('The Alt-R command will toggle between the two radios you have set up.');
             WriteLn ('The mode and band will be updated to the proper values and the radio name');
             WriteLn ('display in the lower left corner of the screen will be updated.');
             WriteLn;
             WriteLn ('If you are on CW mode, the code speed will be remembered for each radio');
             WriteLn ('separately.');
             END;

        AltS:
             BEGIN
             WriteLn ('The Alt-S command will allow you to enter the desired code speed.  If you');
             WriteLn ('want to use the speed knob on the MM3, enter 0 WPM.  You can also change');
             WriteLn ('the code speed by using the PageUp and PageDown keys.');
             WriteLn;
             WriteLn ('If you are using two radios, the code speed is set separately for each');
             WriteLn ('radio.');
             END;

        AltT:
             BEGIN
             WriteLn ('The Alt-T command will allow you to set the time and date.  The time will');
             WriteLn ('be set with the seconds at zero.');
             WriteLn;
             WriteLn ('If you are using the multi network, you will be asked if you would like to');
             WriteLn ('send the date and time to all of the computers on the network.');
             END;

        AltU:
             BEGIN
             WriteLn ('The Alt-U command will move the contents of the editable log into the LOG.DAT');
             WriteLn ('file.  This is normally done after the contest is over so all your contacts are');
             WriteLn ('in the LOG.DAT file.  The LOG.TMP file will be erased.  This command is also');
             WriteLn ('useful when QTCs are enabled as the contacts in the editable log window must');
             WriteLn ('be moved into the LOG.DAT file before they can be sent as QTCs.');
             END;

        AltV:
             BEGIN
             WriteLn ('The Alt-V command will move you down one band.  If the MULTIPLE BANDS ENABLED');
             WriteLn ('has been set to FALSE (as it would for a single band contest), nothing will');
             WriteLn ('happen.');
             END;

        AltW:
             BEGIN
             WriteLn ('The Alt-W command will allow you to reset the wake up counter to zero.');
             WriteLn ('You would do this when the wake up timeout alarm has sounded and you want');
             WriteLn ('it to stop as if you had made another QSO.');
             END;

        AltX:
             BEGIN
             WriteLn ('The Alt-X command will allow you to exit the program.  You will be asked if');
             WriteLn ('you really want to exit in case you hit Alt-X by mistake.');
             END;

        AltY:
             BEGIN
             WriteLn ('The Alt-Y command will delete the last QSO you have made that is displayed');
             WriteLn ('on the bottom of the editable log window.  This contact can be restored');
             WriteLn ('by pressing Alt-Y again before another contact is made.');
             END;

        AltZ:
             BEGIN
             WriteLn ('The Alt-Z command will recalculate the initial exchange entry based upon');
             WriteLn ('the callsign entered in the call window.  This is handy if you came back');
             WriteLn ('to a station, then change his prefix, and want to have the program to tell');
             WriteLn ('you his zone.');
             END;

        Alt1:
             BEGIN
             WriteLn ('Pressing Alt and a number key (ie: Alt-1) will increment the time to the');
             WriteLn ('next even minute.  This function is quite handy if you are entering QSOs');
             WriteLn ('manually from another log.  You would set the time and date to the first');
             WriteLn ('QSO of the log and then use this function to increment the time as needed');
             WriteLn ('for each QSO.  This feature must be enabled by putting the following ');
             WriteLn ('command in your LOGCFG.DAT file: INCREMENT TIME ENABLE = TRUE.');
             WriteLn;
             WriteLn ('This function should not be used if you have set the HOUR OFFSET to a non');
             WriteLn ('zero value as it will affect your system clock.');
             END;

        END;

    END;




PROCEDURE PutUpHelpMenu;

VAR Key: CHAR;

  BEGIN
  RemoveWindow (EditableLogWindow);
  RemoveWindow (TotalWindow);

  REPEAT
    SaveSetAndClearActiveWindow (BigWindow);
    ClrScr;
    WriteLn ('Alt-A - Alarm set      Alt-P - Pgrm CW mem       Ctrl-B - Talk to packet TNC');
    WriteLn ('Alt-B - Band up        Alt-Q - Auto CQ setup     Ctrl-J - LOGCFG value edit');
    WriteLn ('Alt-C - Auto CQ resume Alt-R - Radio toggle      Ctrl-K - Clear dupesheet');
    WriteLn ('Alt-D - Dupecheck      Alt-S - Set CW speed      Ctrl-L - LOG.DAT file view');
    WriteLn ('Alt-E - Edit QSOs      Alt-T - Time/date set     Ctrl-N - Note entry into log');
    WriteLn ('Alt-F - Floppy save    Alt-U - Flush edit log    Ctrl-O - Missing mult report');
    WriteLn ('Alt-G - Swap mults     Alt-V - Band down         Ctrl-P - Possible call redo');
    WriteLn ('Alt-H - Help menu      Alt-W - Wake up reset     Ctrl-Q - QTC functions');
    WriteLn ('Alt-I - Inc rcvd #     Alt-X - Exit program      Ctrl-R - Last erased call');
    WriteLn ('Alt-J - Mult bell      Alt-Y - Delete last QSO   Ctrl-U - Last 10 packet spots');
    WriteLn ('Alt-K - Kill CW        Alt-Z - Redo initial ex   Ctrl-V - Execute config file');
    WriteLn ('Alt-L - Log search     Alt-= - CW tone/Backcopy  Ctrl-Y - Retime band map call');
    WriteLn ('Alt-M - SSB/CW Mode    Alt-- - AutoSend toggle   Ctrl-- - 2 radio dualing CQs');
    WriteLn ('Alt-N - XMIT freq      Alt-1 - Increment time    Ctrl-Ent Same as RET w/o msg');
    WriteLn ('Alt-O - Add reminder   " - Send multi message    ` - Send spot to packet.');
    WriteLn ('SPACE BAR - Check if dupe or if no call, go into S&P and send call');
    Write ('      Press the key you want more information for or ESCAPE to exit help:');
    RestorePreviousWindow;

    REPEAT
        begin
        UpdateTimeAndRateDisplays (False, False);
        millisleep;
        end;
    UNTIL KeyPressed;

    SaveSetAndClearActiveWindow (BigWindow);
    ClrScr;

    Key := ReadKey;

    CASE UpCase (Key) OF

        EscapeKey: Exit;
        NullKey: ProcessAltHelp;

        ' ':
            BEGIN
            WriteLn ('The most common use for the SPACE BAR is to check if a callsign entered');
            WriteLn ('in the call window is a dupe or not.  This is not a necessary step in ');
            WriteLn ('normal operation because the program does check the call itself when you');
            WriteLn ('press RETURN, but if you are interested in finding out it is a dupe before');
            WriteLn ('pressing RETURN (and possibly sending CW), then the SPACE BAR is an easy');
            WriteLn ('way to do this.  You will also be shown the station''s name, if known.');
            WriteLn;
            WriteLn ('If the call window is empty and there is not any dupe information on the');
            WriteLn ('screen in the upper right corner, then pressing SPACE BAR is a short cut way');
            WriteLn ('to enter the search and pounce mode AND send your callsign.');
            WriteLn;
            WriteLn ('When you use the Alt-D command to check for a dupe on the inactive radio');
            WriteLn ('or a packet spot comes up, there will be station information shown in the');
            WriteLn ('upper right hand corner of the screen.  If this information is present, ');
            WriteLn ('pressing the SPACE BAR will allow you to work that station.');
            END;

        '"':
            BEGIN
            WriteLn ('If you are using the program in the multi mode, you can send messages to ');
            WriteLn ('the other computers connected.  Use the " key to open a window and enter');
            WriteLn ('who you want the message to go to, a spaceq, and then the message.  You');
            WriteLn ('can address a message to the band you want the message to go to (160, 80');
            WriteLn ('75, 40, 20, 15 or 10) or to ALL of the computers.  The message will be');
            WriteLn ('instantly displayed on the proper computers with an audiable alert.');
            END;

        '`':
            BEGIN
            WriteLn ('The ` key will send a spot to packet when a valid call is in the call');
            WriteLn ('window, you have the packet port set up and the program can read the');
            WriteLn ('the frequency from your interfaced radio.');
            END;

        ControlB:
            BEGIN
            WriteLn ('The Control-B command allows you to access a 13 line window for talking to ');
            WriteLn ('packet TNC.  Typically you would access this window to log onto a packet');
            WriteLn ('cluster and then exit from it to operate a contest.  DX Spots that are new');
            WriteLn ('multipliers will be shown to you.  You can also type sh/dx while the packet');
            WriteLn ('window is up and then exit before the data shows up, and the spot information');
            WriteLn ('will be loaded into the spot memory that you can access with the Control-U');
            WriteLn ('command.');
            WriteLn;
            WriteLn ('While the packet window is active, you can press F1 to call CQ.  This is');
            WriteLn ('handy if you are talking to a friend on packet and want to continue to  call');
            WriteLn ('CQ in the contest.  If someone answers you, press ESCAPE to exit the window');
            WriteLn ('and then work the station.  When you press Control-B after the QSO, the window');
            WriteLn ('will show any data that you missed while you were busy.');
            WriteLn;
            WriteLn ('You need the PACKET PORT command in LOGCFG.DAT to enable packet operations.');
            END;

        ControlJ:
            BEGIN
            WriteLn ('The Control-J command can be used to change many of the LOGCFG.DAT commands');
            WriteLn ('while the program is running.  To see which ones can be accessed, simply try');
            WriteLn ('running the command.');
            WriteLn;
            WriteLn ('You can select which parameter you want to modify using the cursor keys.  For');
            WriteLn ('each entry, you are shown the LOGCFG.DAT name for the parameter, the current');
            WriteLn ('value of the parameter, and a short description explaining what the result of');
            WriteLn ('the current value is.  You can change the value by pressing RETURN.  For most');
            WriteLn ('of the parameters, this will toggle the value to the next value.  For others,');
            WriteLn ('you will have to enter a new value for the parameter.');
            WriteLn;
            WriteLn ('The Alt-W command may be used to write the selected entry to the LOGCFG.DAT ');
            WriteLn ('file.  This will make the change permanent even if the program is restarted.');
            END;

        ControlK:
            BEGIN
            WriteLn ('The Control-K command will clear the dupesheet.  This is used for those');
            WriteLn ('contests where you can work people again on the second day of the contest');
            WriteLn ('Use this command after the first day so you can work people again without');
            WriteLn ('the program flagging them as dupes.');
            END;

        ControlL:
            BEGIN
            WriteLn ('The Control-L command will allow you to browse through the LOG.DAT file.');
            WriteLn ('The PageUp, PageDown, Home and End keys can be used to move around. ');
            WriteLn ('No editing of the LOG.DAT is supported.  To exit, simply press ESCAPE.');
            END;

        ControlN:
            BEGIN
            WriteLn ('The Control-N command will allow you to input a single line note to the log.');
            WriteLn ('The note will appear with a semi-colon in front of it which makes sure the ');
            WriteLn ('line is not counted as a QSO.  After the contest, POST can create a file with');
            WriteLn ('all the notes you have made.  Any notes in your log will not appear in your');
            WriteLn ('final band/mode log as generated by POST.');
            WriteLn;
            END;

        ControlO:
            BEGIN
            WriteLn ('The Control-O command can be used when you have any kind of country DX     ');
            WriteLn ('multiplier.  This command will display a list of countries that you have   ');
            WriteLn ('worked on at least 4 bands, but not all 6 bands.  It will show you the bands');
            WriteLn ('with QSOs indicated with asterisks.  This display is quite useful to make ');
            WriteLn ('sure you are not missing any of the active countries on easy bands.');
            END;

        ControlP:
            BEGIN
            WriteLn ('The Control-P command will show you all the information known for the call');
            WriteLn ('presently in the call window, including possible calls (if enabled).');
            END;

        ControlQ:
            BEGIN
            WriteLn ('When using QTCs, the Control-Q command is used to either send or receive QTCs');
            WriteLn ('with the station you are currently working.');
            END;

        ControlR:
            BEGIN
            WriteLn ('If you have erased a callsign using the ESCAPE key, you can recall the ');
            WriteLn ('callsign with the Control-R command.  This can also be used for erased');
            WriteLn ('exchanges.');
            END;

        ControlU:
            BEGIN
            WriteLn ('The Control-U command allows you to review the last 10 packet spots.  The');
            WriteLn ('spots were either shown to you as new multipliers or they can come from');
            WriteLn ('a sh/dx command that was executed using the packet window (Control-B).  To');
            WriteLn ('load the spot memory using a sh/dx command, you must exit the packet window');
            WriteLn ('before the data comes back from the cluster.');
            WriteLn;
            WriteLn ('You can move a cursor to any of the listed stations and press RETURN to set');
            WriteLn ('up the program and your radio to work that station.');
            END;

        ControlY:
            BEGIN
            WriteLn ('When using the band map, the Control-Y command will refresh the time for');
            WriteLn ('whatever entry is currently blinking.');
            END;

        ControlDash:
            BEGIN
            WriteLn ('If you are using two radios, the Control-Dash function will send the      ');
            WriteLn ('message programmed in CQ memory Alt-F1 alternately on each radio.  This   ');
            WriteLn ('is typically a short CQ message so you can CQ on two bands at once.  If   ');
            WriteLn ('you get a response, simply type in the call as you normally would.  The CQ');
            WriteLn ('on the other band will finish and then the station will be called on the  ');
            WriteLn ('correct band.  While the station is sending you the exchange, another CQ  ');
            WriteLn ('will be sent on the other band.  This technique allows you to be          ');
            WriteLn ('transmitting 100 percent of the time!  It is best used when the rate is ');
            WriteLn ('fairly slow on both bands (ie: around 40 per hour or less) and with short ');
            WriteLn ('CQ messages (e.g. TEST N6TR TEST).');
            END;

        ControlBackSlash:
            BEGIN
            WriteLn ('The Control-\ or Control-Enter key will allow you to take a CQ mode QSO to ');
            WriteLn ('the next step without sending CW.  This applies to either the CQ EXCHANGE or');
            WriteLn ('the QSL MESSAGE.');
            END;

        ELSE
            BEGIN
            ClrScr;
            WriteLn;
            WriteLn ('No help available for that command');
            Tone.DoABeep (Single);
            END;
        END;

    WriteLn;
    Write ('Press any key to continue...');
    RestorePreviousWindow;

    REPEAT
        UpdateTimeAndRateDisplays (False, False)
    UNTIL KeyPressed;

    IF ReadKey = NullKey THEN ReadKey;
  UNTIL False;
  END;




PROCEDURE DisplayCountryInformation (FileName: Str80; Call: CallString);

{ This procedure will go search through the file specified and if it
  finds an entry with the country ID for the callsign indicated, it
  will display the data contained after that line, until the next blank
  line if found (up to five lines).  The information will be displayed
  in the editable log window. }

VAR FileRead: TEXT;
    CountryID: Str20;
    FileString: Str80;

    BEGIN
    IF NOT OpenFileForRead (FileRead, FileName) THEN Exit;

    CountryID := CountryTable.GetCountryID (CountryTable.GetCountry (Call, True));
    CountryID := UpperCase (CountryID);

    WHILE NOT Eof (FileRead) DO
        BEGIN
        ReadLn (FileRead, FileString);

        IF UpperCase (FileString) = CountryID THEN
            BEGIN
            SaveSetAndClearActiveWindow (EditableLogWindow);

            REPEAT
                ReadLn (FileRead, FileString);

                IF FileString <> '' THEN
                    BEGIN
                    IF WhereX > 1 THEN WriteLn;
                    Write (FileString);
                    END;

            UNTIL (WhereY = 5) OR (FileString = '');

            Close (FileRead);
            RestorePreviousWindow;
            Exit;
            END;
        END;

    Close (FileRead);
    END;



PROCEDURE ComputeGridDistance;

VAR Grid1, Grid2: Str20;
    Distance: REAL;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('COMPUTE DISTANCE BETWEEN TWO GRID SQUARES');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This routine will compute the distance between two grid squares in kilometers.');
    WriteLn;

    REPEAT
        Grid1 := UpperCase (GetResponse ('Enter first grid (none to exit) : '));
        IF Grid1 = '' THEN Exit;
        Grid2 := UpperCase (GetResponse ('Enter second grid (none to exit) : '));
        IF Grid2 = '' THEN Exit;

        Distance := GetDistanceBetweenGrids (Grid1, Grid2);
        WriteLn ('Distance = ', Distance:6:1, ' kilometers');

        Distance := GetEuropeanDistanceBetweenGrids (Grid1, Grid2);
        WriteLn ('European VHF Distance = ', Distance:6:1, ' kilometers');

    UNTIL False;
    END;



PROCEDURE HexConvert;

VAR TempString: Str20;
    TempInt: LONGINT;
    Result: INTEGER;

    BEGIN
    ClearScreenAndTitle ('CONVERT NUMBERS TO/FROM HEX');

    WriteLn ('This procedure will let you convert a base 10 number to / from HEX.  If you');
    WriteLn ('are entering a HEX (base 16) number, put an H after it.  Otherwise, the');
    WriteLn ('routine will assume it is a base 10 number to be converted to hex.');
    WriteLn;
    WriteLn ('Enter a null string to exit.');
    WriteLn;

    REPEAT
        TempString := UpperCase (GetResponse ('Enter number to convert (put an H after a hex number) : '));

        IF TempString = '' THEN Exit;

        IF StringHas (TempString, 'H') THEN
            BEGIN
            Delete (TempString, Pos ('H', TempString), 1);
            HexToLongInteger (TempString, TempInt, Result);
            WriteLn (TempInt);
            END
        ELSE
            BEGIN
            Val (TempString, TempInt, Result);
            WriteHexLongInt (TempInt);
            WriteLn;
            END;

    UNTIL False;
    END;



PROCEDURE HexDump;

{ Procedure to dump a HEX file so you can see what is in it. }

LABEL ContinueSearch;

VAR FileName, ASCIIString: Str20;
    SearchString: Str80;
    FileRead: FILE;
    FileByte: BYTE;
    Key: CHAR;
    SizeOFFIle, NewAddress: LONGINT;
    CharPos: INTEGER;
    FileChar: CHAR;

    BEGIN
    IF ParamStr (2) <> '' THEN
        FileName := ParamStr (2)
    ELSE
        FileName := GetResponse ('Enter file to do hex dump of : ');

    IF (FileName = '') OR NOT FileExists (FileName) THEN Exit;

    Assign (FileRead, FileName);
    Reset  (FileRead, 1);

    ASCIIString := '';

    SizeOfFile := FileSize (FileRead);

    REPEAT
        IF ((FilePos (FileRead) MOD 256) = 0) OR Eof (FileRead) THEN
            BEGIN
            IF (FilePos (FileRead) > 0) OR Eof (FileRead) THEN
                BEGIN
                WriteLn;
                WriteLn;

                REPEAT
                    TextColor (Green);

                    Key := UpCase (GetKey ('ESCAPE, (S)earch, HOME, END, PAGE UP or PAGE DOWN'));

                    IF Key = EscapeKey THEN
                        BEGIN
                        Close (FileRead);
                        GoToXY (1, WhereY);
                        ClrEol;
                        Exit;
                        END;

                    IF Key = 'S' THEN
                        BEGIN
                        GoToXY (1, WhereY);
                        ClrEol;
                        SearchString := GetResponse ('Enter string to search for : ');

                        IF SearchString <> '' THEN
                            BEGIN

                        ContinueSearch:

                            REPEAT
                                BlockRead (FileRead, FileChar, 1);
                                IF FilePos (FileRead) MOD 1000 = 0 THEN PinWheel;
                            UNTIL (FilePos (FileRead) = SizeOfFile) OR (FileChar = SearchString [1]);

                            Seek (FileRead, FilePos (FileRead) - 1);

                            IF FilePos (FileRead) < (SizeOfFile - Length (SearchString)) THEN
                                BEGIN
                                FOR CharPos := 1 TO Length (SearchString) DO
                                    BEGIN
                                    BlockRead (FileRead, FileChar, 1);

                                    IF NOT (SearchString [CharPos] = FileChar) THEN
                                        GoTo ContinueSearch;
                                    END;
                                END
                            ELSE
                                BEGIN
                                GoToXY (1, WhereY);
                                ReportError (SearchString + ' not found!!');
                                WaitForKeyPressed;
                                END;

                            { We found the search string! }

                            NewAddress := FilePos (FileRead) - 16 - Length (SearchString);
                            WHILE (NewAddress MOD 256) <> 0 DO Dec (NewAddress);
                            Seek (FileRead, NewAddress);
                            END;
                        END;

                    IF Key = NullKey THEN Key := ReadKey;

                UNTIL (Key = PageUpKey) OR
                      ((Key = PageDownKey) AND NOT Eof (FileRead)) OR
                      (Key = HomeKey) OR
                      (Key = AltA) OR
                      (Key = 'S') OR
                      (Key = EndKey);

                CASE Key OF
                    PageUpKey:
                        BEGIN
                        NewAddress := FilePos (FileRead);

                        IF NewAddress > 0 THEN
                            NewAddress := NewAddress - 257;

                        WHILE (NewAddress MOD 256) <> 0 DO Dec (NewAddress);

                        IF NewAddress < 0 THEN NewAddress := 0;

                        Seek (FileRead, NewAddress);
                        END;

                    HomeKey:
                        BEGIN
                        NewAddress := 0;
                        Seek (FileRead, NewAddress);
                        END;


                    EndKey:
                        BEGIN
                        NewAddress := FileSize (FileRead) - 1;
                        WHILE ((NewAddress MOD 256) <> 0) DO Dec (NewAddress);
                        Seek (FileRead, NewAddress);
                        END;

                    END;
                END;

            ClrScr;
            TextColor (Yellow);
            WriteLnCenter ('HEX DUMP FOR ' + FileName);
            WriteLn;

            ASCIIString := '';
            END;

        TextColor (Cyan);

        IF (FilePos (FileRead) MOD 16) = 0 THEN
            BEGIN
            WriteHexWord (FilePos (FileRead) DIV 65536);
            WriteHexWord (FilePos (FileRead) MOD 65536);
            Write ('  ');
            END;

        BlockRead (FileRead, FileByte, 1);

        IF (FileByte >= 32) AND (FileByte <= 126) THEN
            ASCIIString := ASCIIString + Chr (FileByte)
        ELSE
            ASCIIString := ASCIIString + '.';

        WriteHexByte (FileByte);
        Write (' ');

        IF (WhereX >= 35) AND (WhereX <= 37) THEN Write (' ');

        IF (FilePos (FileRead) MOD 16) = 0 THEN
            BEGIN
            WriteLn ('   ', ASCIIString);
            ASCIIString := '';
            END;

    UNTIL False;
    END;



PROCEDURE ShowIOPorts;
var buf: PChararray;
   nch,nd,i,nf: integer;
begin
   ClrScr;
   TextColor (Yellow);
   WriteLnCenter ('Serial and Parallel ports');
   TextColor (Cyan);
   WriteLn;
   WriteLn('Here are the Serial and Parallel ports found. The device name');
   WriteLn('is given followed by a description if known. If you have ports');
   WriteLn('that were not found, they can still be used if you know their');
   Writeln('correct device name');
   WriteLn;
   nch := 80;
   nd := 20;
   setlength(buf,nd);
   for i := 0 to nd-1 do getmem(buf[i],nch);
   nf := findserial(buf,nch,nd);
   TextColor (Yellow);
   if (nf = 1) then
      WriteLn(nf,' Serial port found:')
   else
      WriteLn(nf,' Serial ports found:');
   TextColor (Cyan);
   if (nf > nd) then
   begin
      writeln('more ports found than can be printed ',nf,' ',nd);
      nf := nd;
   end;
   for i := 0 to nf-1 do
   begin
      writeln(buf[i]);
   end;
   WriteLn;
   nf := findparallel(buf,nch,nd);
   TextColor (Yellow);
   if (nf = 1) then
      WriteLn(nf,' Parallel port found:')
   else
      WriteLn(nf,' Parallel ports found:');
   TextColor (Cyan);
   if (nf > nd) then
   begin
      writeln('more ports found than can be printed ',nf,' ',nd);
      nf := nd;
   end;
   for i := 0 to nf-1 do
   begin
      writeln(buf[i]);
   end;
   for i := 0 to nd-1 do freemem(buf[i],nch);
end;


PROCEDURE IoPort;

VAR TempString, PortString, OutputString: Str80;
    OutputValue, InputValue, PortAddress, Result: INTEGER;
    LoopMode: BOOLEAN;
    Key: CHAR;

    BEGIN
    LoopMode := False;
    TextColor (Cyan);
    WriteLn;
    WriteLn ('Use L to change loop mode.');

    REPEAT
        WriteLn;

        IF LoopMode THEN
            TempString := UpperCase (GetResponse ('Loop Input or Output : '))
        ELSE
            TempString := UpperCase (GetResponse ('Input or Output : '));

        IF TempString = '' THEN Exit;

        IF (TempString [1] = 'I') OR (TempString [1] = 'O') THEN
            BEGIN
            PortString := GetResponse ('Hex address : ');
            HexToInteger (PortString, PortAddress, Result);

            IF Result = 0 THEN
                BEGIN
                CASE TempString [1] OF

                    'I': BEGIN
                         IF LoopMode THEN
                             BEGIN
                             Key := UpCase (GetKey ('Constant display of value (slows down looping) (Y/N) : ' ));
                             WriteLn;
                             Write ('Looping until key pressed.');
                             END;

                         GoToXY (1, WhereY);
                         ClrEol;

                         REPEAT
                             InputValue := Port [PortAddress];

                             IF ((Key = 'Y') AND LoopMode) OR NOT LoopMode THEN
                                 BEGIN
                                 GoToXY (1, WhereY);
                                 Write ('Value from port ', PortString, ' = ');
                                 WriteHexByte (InputValue);
                                 END;

                             IF LoopMode THEN
                                 BEGIN
                                 IF KeyPressed THEN
                                     BEGIN
                                     LoopMode := False;
                                     ReadKey;
                                     WriteLn;
                                     END;
                                 END
                             ELSE
                                 WriteLn;

                         UNTIL NOT LoopMode;
                         END;

                    'O': BEGIN
                         OutputString := GetResponse ('Enter hex value to output : ');
                         HexToInteger (OutputString, OutputValue, Result);

                         IF Result = 0 THEN
                             BEGIN
                             WriteLn ('Output ', OutputString, ' to ', PortString);
                             REPEAT
                                 Port [PortAddress] := OutputValue;
                                 IF LoopMode THEN
                                     IF KeyPressed THEN
                                         BEGIN
                                         GoToXY (1, WhereY);
                                         ClrEol;
                                         LoopMode := False;
                                         ReadKey;
                                         END;

                             UNTIL NOT LoopMode;
                             END;
                         END;

                     END;
                END;
            END
        ELSE
            IF TempString = 'L' THEN LoopMode := NOT LoopMode;

    UNTIL False;
    END;



PROCEDURE Inductance;

VAR L, F, C: REAL;

    BEGIN
    F := GetReal ('Enter Frequency of resonance (MHz) : ');

    F := F * F;

    F := F * 0.00003948;

    C := GetReal ('Enter Capacitance (pf) or Inductance (uH) : ');

    L := F * C;

    L := 1.0 / L;

    WriteLn (L:3:2, ' uH or pf')
    END;



PROCEDURE PassThrough;

VAR Key: CHAR;
    BaudString: Str20;
    Result, BaudRate: LONGINT;
    dev: str80;
    p1,p2 : serialportx;
    c : char;

    BEGIN
    ClrScr;
    WriteLn ('Serial port pass through.  Connects two serial ports together so that data');
    WriteLn ('flows from one port to the other (and visa versa).');
    WriteLn;

    dev := getresponse('Enter device name for first serial port: ');

    REPEAT
        BaudString := UpperCase (GetResponse ('Enter baud rate : '));
        IF BaudString = '' THEN Exit;
    UNTIL (BaudString =  '300') OR (BaudString = '1200') OR
          (BaudString = '2400') OR (BaudString = '4800') OR
          (BaudString = '9600');

    WriteLn;
    Val (BaudString, BaudRate, Result);

    REPEAT
        Key := UpCase (GetKey ('Enter number of bits (7 or 8) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = '7') OR (Key = '8');
    WriteLn;

    p1 := serialportx.create(dev);
    CASE Key OF
        '7': p1.setparams(BaudRate,7,EvenParity,1);
        '8': p1.setparams(BaudRate,8,NoParity,1);
        END;

    dev := getresponse('Enter device name for second serial port: ');

    REPEAT
        BaudString := UpperCase (GetResponse ('Enter baud rate : '));
        IF BaudString = '' THEN Exit;
    UNTIL (BaudString =  '300') OR (BaudString = '1200') OR
          (BaudString = '2400') OR (BaudString = '4800') OR
          (BaudString = '9600');

    WriteLn;
    Val (BaudString, BaudRate, Result);

    REPEAT
        Key := UpCase (GetKey ('Enter number of bits (7 or 8) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = '7') OR (Key = '8');
    WriteLn;

    p2 := serialportx.create(dev);
    CASE Key OF
        '7': p2.setparams(BaudRate,7,EvenParity,1);
        '8': p2.setparams(BaudRate,8,NoParity,1);
        END;

    WriteLn;
    WriteLn ('Press ESCAPE to stop');

    REPEAT
        IF KeyPressed THEN
            IF ReadKey = EscapeKey THEN
                Halt;

        IF p1.CharReady THEN
            p2.putChar(p1.ReadChar);

        IF p2.CharReady THEN
            p1.putChar(p2.ReadChar);

    UNTIL False;
    END;



PROCEDURE PacketSimulate;


VAR SerialPort: serialportx;
    Key: CHAR;
    FreqString, RandomCall, TempString: Str80;
    Frequency: REAL;
    Time: TimeRecord;

    BEGIN
    ClrScr;
    WriteLn ('Simulate large amounts of packet spots.');
    WriteLn;
    WriteLn ('This procedure will generate random packet spots and send them out to the');
    WriteLn ('serial port indicated at 4800 baud with 7 bits, even parity, one stop bit.');
    WriteLn;

    REPEAT
        Key := UpCase (GetKey ('Enter  serial port (1-4): '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = '1') OR (Key = '2') OR (Key = '3') OR (Key = '4');
    WriteLn;

//    CASE Key OF
//        '1': SerialPort := Serial1;
//        '2': SerialPort := Serial2;
//        '3': SerialPort := Serial3;
//        '4': SerialPort := Serial4;
//        ELSE Exit;
//        END;

//    InitializeSerialPort (SerialPort, 4800, 7, EvenParity, 1);

    WriteLn ('Press ESCAPE key to stop packet spot generation.');

    REPEAT
        IF KeyPressed THEN
            IF ReadKey = EscapeKey THEN
                Exit;


        CASE Random (4) OF
            0: RandomCall := GetRandomLetter +
                             GetRandomNumber +
                             GetRandomLetter +
                             GetRandomLetter;

            1: RandomCall := GetRandomLetter +
                             GetRandomNumber +
                             GetRandomLetter +
                             GetRandomLetter +
                             GetRandomLetter;

            2: RandomCall := GetRandomLetter +
                             GetRandomLetter +
                             GetRandomNumber +
                             GetRandomLetter +
                             GetRandomLetter +
                             GetRandomLetter;

            3: RandomCall := GetRandomLetter +
                             GetRandomLetter +
                             GetRandomNumber +
                             GetRandomLetter;
            END;


        Frequency := (7000000 + Random (50000) + Random (50000)) / 1000.0;

        CASE Random (5) OF
            0: TempString := 'DX de N6TR: ';
            1: TempString := 'DX de K7RAT: ';
            2: TempString := 'DX de WB6ZVC: ';
            3: TempString := 'DX de WA6TUT: ';
            4: TempString := 'DX de KC7KMC: ';
            END;

        WHILE Length (TempString) < 18 DO TempString := TempString + ' ';

        Str (Frequency:5:1, FreqString);

        TempString := TempString + FreqString + '  ' + RandomCall;

        CASE Random (20) OF
            0: TempString := TempString + '  QSX 7255.1';
            1: TempString := TempString + '  7247.3';
            2: TempString := TempString + '  203.5';
            3: TempString := TempString + '  Gus in Helsinki';
            END;

        SendString (SerialPort, TempString + CarriageReturn + LineFeed);

        MarkTime (Time);

        REPEAT millisleep UNTIL ElaspedSec100 (Time) >= 50;

    UNTIL False;
    END;




PROCEDURE SunriseSunset;

VAR Lat, Lon: REAL;

    BEGIN
    REPEAT
        Lat := GetReal ('Enter Lat (0 to exit) ');
        IF Lat = 0 THEN Exit;
        Lon := GetReal ('Enter Lon ');

        WriteLn ('Sunrise/Sunset = ', GetSunriseSunsetString (Lat, Lon));
    UNTIL False;
    END;



PROCEDURE StartUpHelp;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('TR LOG STARTUP HELP SCREEN');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('Legal commands include the following (see manual for details) : ');
    WriteLn;
    WriteLn ('B64Decode           BandMap             Coax                Debug');
    WriteLn ('Distance            FindFile            FootSwitchDebug     Grid');
    WriteLn ('Help                HexConvert          HexDump             IOPort');
    WriteLn ('HP                  LC                  LoopBack            NetDebug');
    WriteLn ('New                 Packet              PacketFile          PacketInputFile');
    WriteLn ('Port                PacketSimulate      PassThrough         PortToFile');
    WriteLn ('RadioDebug          Read                Sun                 TalkDebug');
    WriteLn ('Trace               UUDecode            UnixTime            ViewRadioDebug');
    WriteLn;
    END;




PROCEDURE CoaxLength;

VAR C1, C2, Avg, L1, L2, F1, F2: REAL;
    N: INTEGER;
    Base: INTEGER;

    BEGIN
    ClrScr;

    TextColor (Yellow);
    WriteLnCenter ('COAX Length Calculator');
    WriteLn;

    F1 := GetReal ('Enter a low impedance frequency (0 to exit) : ');

    IF F1 = 0 THEN Exit;

    F2 := GetReal ('Enter the next highest impedance null (0 to exit) : ');

    IF F2 = 0 THEN Exit;

    L1 := 300 / F1;
    L2 := 300 / F2;

    N := Round ((2 * L2) / (L1 - L2));

    WriteLn ('F1 = ', N, ' quarter wavelenths.');

    C1 := (L1 * N) / 4;
    C2 := (L2 * (N + 2)) / 4;

    Avg := (C1 + C2) / 2;

    WriteLn ('Length from F1 = ', C1:4:2, ' meters.');
    WriteLn ('Length from F2 = ', C2:4:2, ' meters.');
    WriteLn ('Average = ', Avg:4:2, ' meters.');

    WriteLn ('Length from new equation = ', 150 / (F2 - F1):4:2);
    END;



PROCEDURE FreeStartUpScreen;

    BEGIN
    ClrScr;

    TextColor (Yellow);
    WriteLnCenter ('1.06 - Freeware version based on TR Log 6.69');
    WriteLn;
    TextColor (Cyan);

    WriteLn ('This free version only supports the following contests: ARRL Field Day, Region');
    WriteLn ('One Field Day, Japanese DX (JIDX), New Zealand Field Day, South American WW,');
    WriteLn ('the Stew Perry 160, and WRTC 2002.  Other than the contest selection parameters');
    WriteLn ('and associated parameters (such as exchange received and multiplier types),');
    WriteLn ('this is the same as TR Log version 6.68 - released in June 2002.');
    WriteLn;
    WriteLn ('The manual for the program can be downloaded from http://www.qth.com/tr.');
    WriteLn;
    WriteLn ('We hope that after using this program in one of these contests, you will want');
    WriteLn ('to purchase the commercial version of the program, which supports over 50');
    WriteLn ('different contests.  Also, since TR Log is constantly being enhanced, there');
    WriteLn ('will be many new features as well.  You can order the program on the web at');
    WriteLn ('http:\\www.qth.com\tr or from one of the following distributors:');
    WriteLn;
    WriteLn ('NA/SA: George Fremin III; 624 Lost Oak Trail, Johnson City, TX  78636');
    WriteLn ('       Tel: (830) 868-2510; FAX: (512) 732-7099 e-mail: geoiii@kkn.net');
    WriteLn ('   EU: Jon Silvergran; Box 178; SE-83122 OSTERSUND Sweden infor@jonit.com');
    WriteLn ('   JA: Tack Kumagai; PO Box 22 Mitaka; Tokyo 181, Japan je1cka@nal.go.jp');
    WriteLn ('VK/ZL: John McRae; 13 Francis St; Kapunda; So. Australia 5373');
    WriteLn ('       e-mail: vkspojfm@dove.net.au');

    WHILE KeyPressed DO ReadKey;
    WaitForKeyPressed;
    END;




PROCEDURE PacketMess;

VAR Password: Str40;

    BEGIN
    ClearScreenAndTitle ('Welcome to Packet Mess');

    WriteLn ('This is a demonstration only.');

    Password := GetResponse ('Enter password : ');

    IF Length (Password) <> 4 THEN Exit;

    IF Password [1] <> 'K' THEN Exit;
    IF Password [2] <> '1' THEN Exit;
    IF Password [3] <> 'Z' THEN Exit;
    IF Password [4] <> 'M' THEN Exit;

    WriteLn ('Packet Mess Enabled.  May God have mercy on your soul.');

    WaitForKeyPressed;

    PacketMessMode := True;
    END;



    BEGIN
    END.
