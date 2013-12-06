UNIT SlowTree;

{ This unit contains things that used to be in Tree - but were moved
  here because I wanted to have them in a different unit to maximize
  TR's performance.  This unit is intended to be an overlay whre Tree
  isn't. }

{$O+}
{$V-}

INTERFACE

Uses Tree,keycode;

    FUNCTION  AppendFile (SourceFile: Str40; DestFile: Str40): BOOLEAN;
    PROCEDURE BigCursor;
    FUNCTION  HexString (HexByte: Byte): Str20;
    FUNCTION  CallFoundInFile (Callsign: CallString; FileName: Str40): BOOLEAN;

    FUNCTION  CheckMouse (VAR XPos: INTEGER;
                          VAR YPos: INTEGER;
                          VAR Button1: BOOLEAN;
                          VAR Button2: BOOLEAN): BOOLEAN;

    FUNCTION  CompareFileTimes (FirstFileName: Str80; SecondFileName: Str80): FileComparisonType;

    PROCEDURE SetUpComPortAddresses;

    PROCEDURE Congrats;
    FUNCTION  CopyFile (SourceFile: Str80; DestFile: Str80): BOOLEAN;
    FUNCTION  CopyFiles (SourceDirectory: Str80; FileMask: Str80; DestDirectory: Str80): BOOLEAN;
    PROCEDURE ClearScreenAndTitle (Title: Str80);
    PROCEDURE DeleteFile (FileName: Str80);
    PROCEDURE DirectoryClearCursor (Entry: INTEGER;  StartY: INTEGER);
    FUNCTION  DirectoryExists (DirName: Str80): BOOLEAN;
    PROCEDURE DirectoryShowCursor (Entry: INTEGER;  StartY: INTEGER);
    FUNCTION  DupingFileExists (Name: Str80): BOOLEAN;

    FUNCTION  ElaspedMinutes (StartTime: TimeRecord): INTEGER;

    FUNCTION  FileExists (Filename: Str80): BOOLEAN;
    FUNCTION  FindDirectory (FileName: Str80): Str80;
    PROCEDURE FormFeed;

    FUNCTION  GetColorInteger (ColorString: Str80): INTEGER;
    PROCEDURE GetFileNames (Path: Str80; Mask: Str80; VAR FileNames: FileNameRecord);
    FUNCTION  GetKey (Prompt: Str80): CHAR;
    FUNCTION  GetKeyResponse (Prompt: STRING): CHAR;
    FUNCTION  GetReal (Prompt: Str80): REAL;
    FUNCTION  GetResponse (Prompt: Str80): Str80;
    FUNCTION  GetStateFromSection (Section: Str20): Str20;
    FUNCTION  GetValue (Prompt: Str80): LONGINT;

    PROCEDURE HexToInteger (InputString: Str80; VAR OutputInteger: INTEGER; VAR xResult: INTEGER);
    PROCEDURE HexToLongInteger (InputString: Str80; VAR OutputInteger: LONGINT; VAR xResult: INTEGER);
    PROCEDURE HexToWord    (InputString: Str80; VAR OutputWord:    WORD;    VAR xResult: INTEGER);

    PROCEDURE IncrementMinute (VAR DateString: Str20; VAR TimeString: Str80);

    FUNCTION  KeyId (Key: CHAR): Str80;

    FUNCTION  LineInput (Prompt: Str160;
                         InitialString: Str160;
                         OverwriteEnable: BOOLEAN;
                         ExitOnAltKey: BOOLEAN): Str160;

    FUNCTION  Lpt1BaseAddress: WORD;
    FUNCTION  Lpt2BaseAddress: WORD;
    FUNCTION  Lpt3BaseAddress: WORD;

    FUNCTION  MakeDupeFilename (Band: BandType; Mode: ModeType): Str80;
    FUNCTION  MinutesToTimeString (Minutes: INTEGER): Str20;

    FUNCTION  NextMinute (PreviousDateString: Str20; DateString: Str20;
                          PreviousTimeString: Str20; TimeString: Str20): BOOLEAN;

    PROCEDURE NoCursor;

    FUNCTION  OkayToDeleteExistingFile (FileName: Str80): BOOLEAN;
    FUNCTION  OkayToProceed: BOOLEAN;
    FUNCTION  OpenDupeFileForRead (VAR FileHandle: TEXT; Filename: Str80): BOOLEAN;

    PROCEDURE PinWheel;

    PROCEDURE QuickBeep;

    PROCEDURE Raspberry;
    PROCEDURE RenameFile (OldName: Str80; NewName: Str80);
    PROCEDURE ReportError (Prompt: Str80);

    FUNCTION  SameMinute (PreviousDateString: Str20; DateString: Str20;
                          PreviousTimeString: Str20; TimeString: Str20): BOOLEAN;

    PROCEDURE SendIntegerInMorse (Value: INTEGER);
    PROCEDURE SendMorse (Message: Str255);
    PROCEDURE SetMorseSpeed (Speed: INTEGER; Pitch: INTEGER);

    FUNCTION  ShowDirectoryAndSelectFile (PathAndMask: Str80;
                                          InitialFile: Str80;
                                          ClearOnCarriageReturn: BOOLEAN): Str80;

    PROCEDURE SmallCursor;

    FUNCTION  TimeElasped (StartHour, StartMinute, StartSecond, NumberOfMinutes: INTEGER): BOOLEAN;
    PROCEDURE TimeStamp (VAR FileWrite: TEXT);

    PROCEDURE WaitForKeyPressed;
    PROCEDURE WakeUp;
    PROCEDURE WriteHexByte (HexByte: Byte);
    PROCEDURE WriteHexWord (HexWord: Word);
    PROCEDURE WriteHexLongInt (HexLogInt: LONGINT);

    PROCEDURE WriteLnCenter (Prompt: Str80);
    PROCEDURE WriteLnVarCenter (VAR FileWrite: TEXT; Prompt: Str80);
    PROCEDURE WriteLnLstCenter (Prompt: Str80);
    PROCEDURE WritePortType (Port: PortType);

{ New disk functions }





IMPLEMENTATION

USES DOS, trCrt, Printer,memlinux,linuxsound, datetimec,timer;

CONST
    NoteVeryLoA = 220;
    NoteVeryLoASharp = 235;
    NoteVeryLoBFlat = 235;
    NoteVeryLoB = 250;
    NoteVeryLoBSharp = 265;
    NoteLoC = 265;
    NoteLoCSharp = 280;
    NoteLoDFlat  = 280;
    NoteLoD = 295;
    NoteLoDSharp = 312;
    NoteLoEFlat  = 312;
    NoteLoE = 330;
    NoteLoESharp = 350;
    NoteLoFFlat = 330;
    NoteLoF = 350;
    NoteLoFSharp = 372;
    NoteLoGFlat = 372;
    NoteLoG = 395;
    NoteLoGSharp = 417;
    NoteLoAFlat = 417;
    NoteLoA = 440;
    NoteLoASharp = 470;
    NoteLoBFlat = 470;
    NoteLoB = 500;
    NoteLoBSharp = 530;
    NoteCFlat = 500;
    NoteC = 530;
    NoteCSharp = 560;
    NoteDFlat = 560;
    NoteD = 590;
    NoteDSharp = 625;
    NoteEFlat = 625;
    NoteE = 660;
    NoteESharp = 700;
    NoteFFlat = 660;
    NoteF = 700;
    NoteFSharp = 745;
    NoteGFlat = 745;
    NoteG = 790;
    NoteGSharp = 835;
    NoteAFlat = 835;
    NoteA = 880;
    NoteASharp = 940;
    NoteBFlat = 940;
    NoteB = 1000;
    NoteBSharp = 1060;
    NoteHiCFlat = 1000;
    NoteHiC = 1060;
    NoteHiCSharp = 1120;
    NoteHiDFlat = 1120;
    NoteHiD = 1180;
    NoteHiDSharp = 1250;
    NoteHiEFlat = 1250;
    NoteHiE = 1320;
    NoteHiESharp = 1400;
    NoteHiFFlat = 1320;
    NoteHiF = 1400;
    NoteHiFSharp = 1490;
    NoteHiGFlat = 1490;
    NoteHiG = 1580;
    NoteHiGSharp = 1670;
    NoteHiAFlat = 1670;
    NoteHiA = 1760;
    NoteHiASharp = 1880;
    NoteHiBFlat = 1880;
    NoteHiB = 2000;
    NoteVeryHiE = 2640;
    NoNote = 0;

TYPE FileControlBlockType = RECORD
         DriveIdentification: BYTE;
         Name:                ARRAY [1..8] OF CHAR;
         Extension:           ARRAY [1..3] OF CHAR;
         CurrentBlockNumber:  INTEGER;
         RecordSize:          INTEGER;
         FileSize:            LONGINT;
         Date:                INTEGER;
         Time:                INTEGER;
         Reserved:            ARRAY [1..8] OF CHAR;
         CurrentRecord:       BYTE;
         RelativeRecord:      LONGINT;
         END;

VAR ActivityCounter: INTEGER;
    Beat:            INTEGER;
    CBuffer:         BufferTypePtr;
    CharacterSpace:  INTEGER;
    CWPitch:         INTEGER;
    DahLength:       INTEGER;
    DitLength:       INTEGER;
    WordSpace:       INTEGER;

//{$L dupe}


function serialaddress(i: integer):integer;cdecl;external;
function paralleladdress(i: integer):integer;cdecl;external;

PROCEDURE SixteenthNote (Pitch: INTEGER);

    BEGIN
    IF Pitch > 0 THEN
        BEGIN
        LSound (Pitch);
        END;
    Delay (Beat DIV 4);
    LNoSound;
    END;

PROCEDURE EigthNote (Pitch: INTEGER);

    BEGIN
    IF Pitch > 0 THEN
        BEGIN
        LSound (Pitch);
        END;
    Delay (Beat DIV 2);
    LNoSound;
    END;

PROCEDURE QuarterNote (Pitch: INTEGER);

    BEGIN
    IF Pitch > 0 THEN
        BEGIN
        LSound (Pitch);
        END;
    Delay (Beat);
    LNoSound;
    END;


PROCEDURE Dit;

    BEGIN
    LSound (CWPitch);
    Delay (DitLength);
    LNoSound;
    Delay (DitLength);
    END;


PROCEDURE Dah;

    BEGIN
    LSound (CWPitch);
    Delay (DahLength);
    LNoSound;
    Delay (DitLength);
    END;



{ Now for the external routines in alphabetical order. }

FUNCTION AppendFile (SourceFile: Str40; DestFile: Str40): BOOLEAN;

VAR FileWrite, FileRead: TEXT;
    FileString: STRING;

    BEGIN
    AppendFile := False;

    IF OpenFileForAppend (FileWrite, DestFile) THEN
        BEGIN
        IF OpenFileForRead (FileRead, SourceFile) THEN
            BEGIN
            WHILE NOT Eof (FileRead) DO
                BEGIN
                ReadLn (FileRead, FileString);
                WriteLn (FileWrite, FileString);
                END;

            Close (FileRead);
            AppendFile := True;
            END;

        Close (FileWrite);
        END;

    END;



FUNCTION BracketedString (LongString: Str160; StartString: Str80; StopString: Str80): Str80;

{ This function will return any string sits between the StartString and the
  StopString.  The shortest possible string to meet this criteria is
  returned.  If the start string is null, then the returned string will
  be the preceding string to the StopString.  If the StopString is null, the
  returned string will be the postceding string to the StartString.         }

VAR StartLocation, StopLocation: INTEGER;

    BEGIN
    BracketedString := '';

    IF StartString <> '' THEN
        BEGIN
        StartLocation := Pos (StartString, LongString);
        IF StartLocation = 0 THEN Exit;
        END
    ELSE
        StartLocation := 0;

    IF StartLocation > 0 THEN
        Delete (LongString, 1, StartLocation + Length (StartString) - 1);

    IF StopString = '' THEN
        BEGIN
        BracketedString := LongString;
        Exit;
        END
    ELSE
        BEGIN
        StopLocation := Pos (StopString, LongString);
        IF StopLocation = 0 THEN Exit;
        END;

    BracketedString := Copy (LongString, 1, StopLocation - 1);
    END;



FUNCTION CaliforniaCall (Call: CallString): BOOLEAN;

    BEGIN
    CaliforniaCall := False;

    IF NOT StringHas (Call, '6') THEN Exit;

    Call := StandardCallFormat (Call, True);

    IF StringHas (Call, '/') THEN
        IF NOT StringHas (Call, '6/') THEN Exit;

    IF (Call [1] <> 'A') AND (Call [1] <> 'K') AND
       (Call [1] <> 'N') AND (Call [1] <> 'W') THEN Exit;

    IF Call [2] = 'H' THEN Exit;
    CaliforniaCall := True;
    END;



FUNCTION CallFoundInFile (Callsign: CallString; FileName: Str40): BOOLEAN;


VAR FileRead: FILE;
    MatchStartAddress, BytesInBuffer, Address: INTEGER;
    Match: BOOLEAN;
    aa: integer;

    BEGIN
    MatchStartAddress := 0;//Silence uninitialized compiler note
    CallFoundInFile := False;

    Assign  (FileRead, FileName);
    Reset   (FileRead, 1);

    Match := False;

    New (CBuffer);

    WHILE NOT Eof (FileRead) DO
        BEGIN
        BlockRead (FileRead, CBuffer, SizeOf (CBuffer), BytesInBuffer);

        IF BytesInBuffer > 0 THEN
            aa := -1;
            FOR Address := 0 TO BytesInBuffer - 1 DO
                BEGIN
                aa := aa + 1;
                if address < aa then continue;
                IF Match THEN
                    BEGIN
                    IF (CBuffer^ [Address] = CallSign [(Address - MatchStartAddress) + 1]) THEN
                        BEGIN
                        IF ((Address - MatchStartAddress) + 1) = (Length (CallSign)) THEN
                            BEGIN
                            CallFoundInFile := True;
                            Close (FileRead);
                            Dispose (CBuffer);
                            Exit;
                            END;
                        END
                    ELSE
                        BEGIN
//                        Address := MatchStartAddress + 1;
                        aa := MatchStartAddress + 1;
                        Match := CBuffer^ [aa] = CallSign [1];
                        IF Match THEN MatchStartAddress := aa;
                        END;
                    END
                ELSE
                    IF CBuffer^ [Address] = CallSign [1] THEN
                        BEGIN
                        Match := True;
                        MatchStartAddress := Address;
                        END;
                END;

        MatchStartAddress := MatchStartAddress - BytesInBuffer;
        END;

    Close (FileRead);
    Dispose (CBuffer);
    END;





FUNCTION CheckMouse (VAR XPos: INTEGER;
                     VAR YPos: INTEGER;
                     VAR Button1: BOOLEAN;
                     VAR Button2: BOOLEAN): BOOLEAN;


    BEGIN
       CheckMouse := false;
    END;



FUNCTION CompareFileTimes (FirstFileName: Str80; SecondFileName: Str80): FileComparisonType;

VAR FirstFile, SecondFile: TEXT;
    FirstTime, SecondTime: LongInt;

    BEGIN
    Assign (FirstFile, FirstFileName);
    Reset (FirstFile);
    GetFTime (FirstFile, FirstTime);
    Close (FirstFile);

    Assign (SecondFile, SecondFileName);
    Reset (SecondFile);
    GetFTime (SecondFile, SecondTime);
    Close (SecondFile);

    IF FirstTime > SecondTime THEN
        CompareFileTimes := After
    ELSE
        IF FirstTime < SecondTime THEN
            CompareFileTimes := Before
        ELSE
            CompareFileTimes := Same;
    END;



PROCEDURE SetUpComPortAddresses;

VAR Address: WORD;

    BEGIN
    Address := serialaddress(1);
//    Address := MemW [ $0040:0 ];
    IF Address = 0 THEN
        Com1PortBaseAddress := $3F8
    ELSE
        Com1PortBaseAddress := Address;

    Address := serialaddress(2);
//    Address := MemW [ $0040:2 ];

    IF Address = 0 THEN
        Com2PortBaseAddress := $2F8
    ELSE
        Com2PortBaseAddress := Address;

    Address := serialaddress(3);
//    Address := MemW [ $0040:4 ];
    IF Address <> 0 THEN
        Com3PortBaseAddress := Address
    ELSE
        Com3PortBaseAddress := $3E8;

    Address := serialaddress(4);
//    Address := MemW [ $0040:6 ];
    IF Address <> 0 THEN
        Com4PortBaseAddress := Address
    ELSE
        Com4PortBaseAddress := $2E8;
    END;


PROCEDURE Congrats;

VAR OldBeat: INTEGER;

    BEGIN
    OldBeat := Beat;
    Beat := 200;
    SixteenthNote (NoteC);
    SixteenthNote (NoteE);
    SixteenthNote (NoteG);
    EigthNote (NoteHiC);
    SixteenthNote (NoteG);
    EigthNote (NoteHiC);
    Beat := OldBeat;
    END;



PROCEDURE Raspberry;

VAR OldBeat: INTEGER;

    BEGIN
    OldBeat := Beat;
    Beat := 100;
    SixteenthNote (NoteLoC);
    SixteenthNote (NoteLoD);
    SixteenthNote (NoteLoC);
    SixteenthNote (NoteLoD);
    SixteenthNote (NoteLoC);
    SixteenthNote (NoteLoD);
    SixteenthNote (NoteLoC);
    SixteenthNote (NoteLoD);
    SixteenthNote (NoteLoC);
    SixteenthNote (NoteLoD);
    Beat := OldBeat;
    END;



FUNCTION CopyFiles (SourceDirectory: Str80;
                    FileMask: Str80;
                    DestDirectory: Str80): BOOLEAN;

VAR FileNames: FileNameRecord;
    FileNumber: INTEGER;

    BEGIN
    IF NOT (SourceDirectory [Length (SourceDirectory)] = DirectorySeparator) THEN
        IF SourceDirectory <> '' THEN
            SourceDirectory := SourceDirectory + DirectorySeparator;

    IF NOT (DestDirectory [Length (DestDirectory)] = DirectorySeparator) THEN
        IF DestDirectory <> '' THEN
            DestDirectory := DestDirectory + DirectorySeparator;

    GetFileNames (SourceDirectory, FileMask, FileNames);

    IF FileNames.NumberFiles > 0 THEN
        FOR FileNumber := 0 TO FileNames.NumberFiles - 1 DO
            CopyFile (SourceDirectory + FileNames.List [FileNumber],
                      DestDirectory + FileNames.List [FileNumber]);

    CopyFiles := (IoResult = 0);
    END;


FUNCTION CopyFile (SourceFile: Str80; DestFile: Str80): BOOLEAN;

VAR FileBuffer: Pointer;
    FileRead, FileWrite: FILE;
    BytesRead, BytesWritten, BufferSize: WORD;
    FileSize: LONGINT;

    BEGIN
    {$I-}
    CopyFile := False;
    IF NOT FileExists (SourceFile) THEN Exit;
    IF MaxAvail < 1000 THEN Exit;

    FileSize := GetFileSize (SourceFile);

    IF FileSize > (MaxAvail - 256) THEN
        BufferSize := MaxAvail - 256
    ELSE
        BufferSize := FileSize;

    GetMem (FileBuffer, BufferSize);

    Assign  (FileRead, SourceFile);
    Reset   (FileRead,        1);

    IF IoResult = 0 THEN
        BEGIN
        Assign  (FileWrite, DestFile);
        Rewrite (FileWrite, 1);

        IF IoResult = 0 THEN
            BEGIN
            REPEAT
                BlockRead  (FileRead, FileBuffer^, BufferSize, BytesRead);
                BlockWrite (FileWrite, FileBuffer^, BytesRead, BytesWritten);

                IF BytesWritten <> BytesRead THEN  { Disk full? }
                    BEGIN
                    Close (FileRead);
                    Close (FileWrite);
                    FreeMem (FileBuffer, BufferSize);
                    Exit;
                    END;

            UNTIL Eof (FileRead);
            Close (FileRead);
            END
        ELSE
            BEGIN
            Close (FileRead);
            FreeMem (FileBuffer, BufferSize);
            Exit;
            END;

        Close (FileWrite);
        END
    ELSE
        BEGIN
        FreeMem (FileBuffer, BufferSize);
        Exit;
        END;

    FreeMem (FileBuffer, BufferSize);
    CopyFile := True;
    {$I+}
    END;



PROCEDURE ClearScreenAndTitle (Title: Str80);

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter (Title);
    TextColor (Cyan);
    WriteLn;
    END;



PROCEDURE DeleteFile (FileName: Str80);

{ This procedure will unceramonisly delete the filename specified.  If the
    file doesn't exist, it is not deleted.           }

VAR F: Text;

    BEGIN
    IF FileExists (FileName) THEN
        BEGIN
        Assign (F, FileName);
        Erase (F);
        END;
    END;



FUNCTION DupingFileExists (Name: Str80): BOOLEAN;

VAR TempString: Str80;
    FileTest: TEXT;

    BEGIN
    DupingFileExists := False;
    IF NOT OpenFileForRead (FileTest, Name) THEN Exit;
        ReadLn (FileTest, TempString);
    DupingFileExists := TempString = 'DUPE';
    Close (FileTest);
    END;



FUNCTION ElaspedMinutes (StartTime: TimeRecord): INTEGER;

VAR Hour, Minute, Second, Sec100: WORD;
    th,tm,ts,ts100,t0h,t0m,t0s,t0s100: longint;
    
    BEGIN
    t0h := starttime.hour;
    t0m := starttime.minute;
    t0s := starttime.second;
    t0s100 := starttime.sec100;
    GetTime (Hour, Minute, Second, Sec100);
    th := hour;
    tm := minute;
    ts := second;
    ts100 := sec100;
    ts100 := ts100-t0s100+100*(ts-t0s+60*(tm-t0m+60*(th-t0h)));
    if ts100 < 0 then ts100 := ts100+360000;
    elaspedminutes := ts100 div 6000;
    if elaspedminutes < 0 then elaspedminutes := 0;
    END;



FUNCTION FileExists (Filename: Str80): BOOLEAN;

{ This function will return TRUE if the filename specified exists. }

VAR DirInfo: SearchRec;

    BEGIN
    FindFirst (FileName, Archive, DirInfo);
    FileExists := DosError = 0;
    END;



FUNCTION FoundDirectory (FileName: Str80; Path: Str80; VAR Directory: Str80): BOOLEAN;

VAR TempString: Str80;
    S: PathStr;

    BEGIN
    FoundDirectory := False;

    S := FSearch (FileName, Path);

    IOResult; {KK1L: 6.68 Added per Tree e-mail 02JUN02}
    { This will clear the IOResult flag in case there was an
      error during FSearch - like if you have an illegal directory
      in your PATH statement.  If it isn't cleared, it will prevent
      the next IO statement from working.}
    IF S = '' THEN
        Exit
    ELSE
        BEGIN
        TempString := FExpand (S);

        WHILE TempString [Length (TempString)] <> DirectorySeparator DO
            Delete (TempString, Length (TempString), 1);

        Delete (TempString, Length (TempString), 1);

        Directory := TempString;
        FoundDirectory := True;
        END;
    END;



FUNCTION FindDirectory (FileName: Str80): Str80;

{ This procedure will attempt to find the directory for the filename
  passed to it.  It will first check the current working directory,
  $HOME/.trlog
  then check the environment variable TRLOG.

  If still no luck, it will check to see it can be found in the command
  string used to execute the program that is running.

  Then it will check the PATH environment string directories.

  If it isn't found,
  it will return a null string.

  If a directory was found, it will end without \. }

VAR TempString, Directory: Str80;

    BEGIN
    FindDirectory := '';
    IF FoundDirectory (FileName, '.', Directory) THEN
        BEGIN
        FindDirectory := Directory;
        Exit;
        END;

//    IF FoundDirectory (FileName, '..', Directory) THEN
//        BEGIN
//        FindDirectory := Directory;
//        Exit;
//        END;

    IF FoundDirectory (FileName, GetEnv ('HOME') + DirectorySeparator
        + '.trlog', Directory) THEN
        BEGIN
        FindDirectory := Directory;
        Exit;
        END;

    IF FoundDirectory (FileName, GetEnv ('TRLOG'), Directory) THEN
        BEGIN
        FindDirectory := Directory;
        Exit;
        END;

    { All this will check to see what command was typed in to run the
      active program and see if a path was specified. }

    TempString := ParamStr (0);

    IF StringHas (TempString, DirectorySeparator) THEN
        BEGIN
        WHILE TempString [Length (TempString)] <> DirectorySeparator DO
            Delete (TempString, Length (TempString), 1);

        Delete (TempString, Length (TempString), 1);

        IF FoundDirectory (FileName, TempString, Directory) THEN
            BEGIN
            FindDirectory := Directory;
            Exit;
            END;
        END;

    IF FoundDirectory (FileName, GetEnv ('PATH'), Directory) THEN
        BEGIN
        FindDirectory := Directory;
        Exit;
        END;

//    IF FoundDirectory (FileName, '\log\name', Directory) THEN
//        BEGIN
//        FindDirectory := Directory;
//        Exit;
//        END;
    END;



PROCEDURE FormFeed;

    BEGIN
    Write (Lst, Chr (12));
    END;



FUNCTION GetColorInteger (ColorString: Str80): INTEGER;

    BEGIN
    ColorString := UpperCase (ColorString);
    IF ColorString = 'BLACK'   THEN GetColorInteger := 0;
    IF ColorString = 'BLUE'    THEN GetColorInteger := 1;
    IF ColorString = 'GREEN'   THEN GetColorInteger := 2;
    IF ColorString = 'CYAN'    THEN GetColorInteger := 3;
    IF ColorString = 'RED'     THEN GetColorInteger := 4;
    IF ColorString = 'MAGENTA'   THEN GetColorInteger := 5;
    IF ColorString = 'BROWN'   THEN GetColorInteger := 6;
    IF ColorString = 'LIGHT GRAY'  THEN GetColorInteger := 7;
    IF ColorString = 'DARK GRAY'   THEN GetColorInteger := 8;
    IF ColorString = 'LIGHT BLUE'  THEN GetColorInteger := 9;
    IF ColorString = 'LIGHT GREEN'   THEN GetColorInteger := 10;
    IF ColorString = 'LIGHT CYAN'  THEN GetColorInteger := 11;
    IF ColorString = 'LIGHT RED'   THEN GetColorInteger := 12;
    IF ColorString = 'LIGHT MAGENTA' THEN GetColorInteger := 13;
    IF ColorString = 'YELLOW'  THEN GetColorInteger := 14;
    IF ColorString = 'WHITE'   THEN GetColorInteger := 15;
    END;



PROCEDURE GetFileNames (Path: Str80;
                        Mask: Str80;
                        VAR FileNames: FileNameRecord);

{ This function will get files names for you until all of them have been
  returned.  When this happens, you will get a null string as a result. }

VAR DirInfo: SearchRec;

    BEGIN
    FileNames.NumberFiles := 0;

    IF (Path <> '') AND (Path [Length (Path)] <> DirectorySeparator) THEN
        Path := Path + DirectorySeparator;

    FindFirst (Path + Mask, Archive, DirInfo);

    WHILE (DosError = 0) AND (FileNames.NumberFiles < MaximumFileNames) DO
        BEGIN
        FileNames.List [FileNames.NumberFiles] := DirInfo.Name;
        Inc (FileNames.NumberFiles);
        FindNext (DirInfo);
        END;
    END;



FUNCTION GetKey (Prompt: Str80): CHAR;

VAR Key: CHAR;

    BEGIN
    GoToXY (1, WhereY);
    ClrEol;
    TextColor (Cyan);
    TextBackground (Black);
    Write (Prompt);
    REPEAT UNTIL KeyPressed;
    Key := ReadKey;
    TextColor (Yellow);
    IF Key >= ' ' THEN Write (Key);
    GetKey := Key;
    END;

FUNCTION GetKeyResponse (Prompt: STRING): CHAR;

{ Looks for prompt to be in the form: prompt (A, B, Q, U, S, M) : and will
  accept only those keys listed. }

VAR Key: CHAR;
    ListString: Str40;

    BEGIN
    ListString := UpperCase (BracketedString (Prompt, '(', ')')) + ',';

    REPEAT
        Key := UpCase (GetKey (Prompt));

        IF (Key = EscapeKey) OR StringHas (ListString, Key + ',') THEN
            BEGIN
            GetKeyResponse := Key;
            WriteLn;
            Exit;
            END;
    UNTIL False;
    END;


FUNCTION GetResponse (Prompt: Str80): Str80;

VAR InputString: STRING;
    Key: CHAR;

    BEGIN
    TextColor (Cyan);
    TextBackground (Black);

    Write (Prompt);
    TextColor (Yellow);

    InputString := '';

    REPEAT
        REPEAT UNTIL KeyPressed;

        Key := ReadKey;

        IF Key = CarriageReturn THEN
            BEGIN
            GetResponse := InputString;
            WriteLn;
            Exit;
            END;

        IF Key = BackSpace THEN
            BEGIN
            IF InputString <> '' THEN
                BEGIN
                GoToXY (WhereX - 1, WhereY);
                Write (' ');
                GoToXY (WhereX - 1, WhereY);
                Delete (InputString, Length (InputString), 1);
                END;
            END
        ELSE
            BEGIN
            Write (Key);
            InputString := InputString + Key;
            END;

    UNTIL Length (InputString) > 250;
    END;



FUNCTION GetReal (Prompt: Str80): REAL;

VAR TempValue: REAL;
    xResult: INTEGER;
    TempString: Str80;

    BEGIN
    TempString := GetResponse (Prompt);
    Val (TempString, TempValue, xResult);

    IF xResult = 0 THEN
        GetReal := TempValue
    ELSE
        GetReal := 0;
    END;



FUNCTION GetValue (Prompt: Str80): LONGINT;

{ This function will display the prompt passed to it and return a
  integer value input by the operator.  If the input is illegal, the
  prompt will be reprinted and a new value read.  }

VAR TempValue, xResult: INTEGER;
    TempString: Str80;

    BEGIN
    REPEAT
        TextColor (Cyan);
        Write (Prompt);
        TextColor (Yellow);
        ReadLn (TempString);
        Val (TempString, TempValue, xResult);
    UNTIL xResult = 0;
    GetValue := TempValue;
    END;



FUNCTION HexString (HexByte: Byte): Str20;

    BEGIN
    HexString := HexChars [HexByte SHR 4] + HexChars [HexByte AND $F];
    END;



PROCEDURE HexToInteger (InputString: Str80; VAR OutputInteger: INTEGER; VAR xResult: INTEGER);

VAR Multiplier: INTEGER;


    BEGIN
    xResult := 1;
    Multiplier := 1;
    OutputInteger := 0;
    IF InputString = '' THEN Exit;

    xResult := 0;

    WHILE Length (InputString) > 0 DO
        BEGIN
        CASE UpCase (InputString [Length (InputString)]) OF
            '0': OutputInteger := OutputInteger + Multiplier * 0;
            '1': OutputInteger := OutputInteger + Multiplier * 1;
            '2': OutputInteger := OutputInteger + Multiplier * 2;
            '3': OutputInteger := OutputInteger + Multiplier * 3;
            '4': OutputInteger := OutputInteger + Multiplier * 4;
            '5': OutputInteger := OutputInteger + Multiplier * 5;
            '6': OutputInteger := OutputInteger + Multiplier * 6;
            '7': OutputInteger := OutputInteger + Multiplier * 7;
            '8': OutputInteger := OutputInteger + Multiplier * 8;
            '9': OutputInteger := OutputInteger + Multiplier * 9;
            'A': OutputInteger := OutputInteger + Multiplier * 10;
            'B': OutputInteger := OutputInteger + Multiplier * 11;
            'C': OutputInteger := OutputInteger + Multiplier * 12;
            'D': OutputInteger := OutputInteger + Multiplier * 13;
            'E': OutputInteger := OutputInteger + Multiplier * 14;
            'F': OutputInteger := OutputInteger + Multiplier * 15;

            ELSE BEGIN
                 xResult := 1;
                 Exit;
                 END;
            END;

        Delete (InputString, Length (InputString), 1);
        Multiplier := Multiplier * 16;
        END;

    xResult := 0;
    END;



PROCEDURE HexToWord (InputString: Str80; VAR OutputWord: Word; VAR xResult: INTEGER);

VAR Multiplier: Word;

    BEGIN
    xResult := 1;
    Multiplier := 1;
    OutputWord := 0;
    IF InputString = '' THEN Exit;

    xResult := 0;

    WHILE Length (InputString) > 0 DO
        BEGIN
        CASE UpCase (InputString [Length (InputString)]) OF
            '0': OutputWord := OutputWord + Multiplier * 0;
            '1': OutputWord := OutputWord + Multiplier * 1;
            '2': OutputWord := OutputWord + Multiplier * 2;
            '3': OutputWord := OutputWord + Multiplier * 3;
            '4': OutputWord := OutputWord + Multiplier * 4;
            '5': OutputWord := OutputWord + Multiplier * 5;
            '6': OutputWord := OutputWord + Multiplier * 6;
            '7': OutputWord := OutputWord + Multiplier * 7;
            '8': OutputWord := OutputWord + Multiplier * 8;
            '9': OutputWord := OutputWord + Multiplier * 9;
            'A': OutputWord := OutputWord + Multiplier * 10;
            'B': OutputWord := OutputWord + Multiplier * 11;
            'C': OutputWord := OutputWord + Multiplier * 12;
            'D': OutputWord := OutputWord + Multiplier * 13;
            'E': OutputWord := OutputWord + Multiplier * 14;
            'F': OutputWord := OutputWord + Multiplier * 15;

            ELSE BEGIN
                 xResult := 1;
                 Exit;
                 END;
            END;

        Delete (InputString, Length (InputString), 1);
        Multiplier := Multiplier * 16;
        END;

    xResult := 0;
    END;



PROCEDURE HexToLongInteger (InputString: Str80; VAR OutputInteger: LONGINT; VAR xResult: INTEGER);

VAR Multiplier: LONGINT;

    BEGIN
    xResult := 1;
    Multiplier := 1;
    OutputInteger := 0;
    IF InputString = '' THEN Exit;

    xResult := 0;

    WHILE Length (InputString) > 0 DO
        BEGIN
        CASE UpCase (InputString [Length (InputString)]) OF
            '0': OutputInteger := OutputInteger + Multiplier * 0;
            '1': OutputInteger := OutputInteger + Multiplier * 1;
            '2': OutputInteger := OutputInteger + Multiplier * 2;
            '3': OutputInteger := OutputInteger + Multiplier * 3;
            '4': OutputInteger := OutputInteger + Multiplier * 4;
            '5': OutputInteger := OutputInteger + Multiplier * 5;
            '6': OutputInteger := OutputInteger + Multiplier * 6;
            '7': OutputInteger := OutputInteger + Multiplier * 7;
            '8': OutputInteger := OutputInteger + Multiplier * 8;
            '9': OutputInteger := OutputInteger + Multiplier * 9;
            'A': OutputInteger := OutputInteger + Multiplier * 10;
            'B': OutputInteger := OutputInteger + Multiplier * 11;
            'C': OutputInteger := OutputInteger + Multiplier * 12;
            'D': OutputInteger := OutputInteger + Multiplier * 13;
            'E': OutputInteger := OutputInteger + Multiplier * 14;
            'F': OutputInteger := OutputInteger + Multiplier * 15;

            ELSE BEGIN
                 xResult := 1;
                 Exit;
                 END;
            END;

        Delete (InputString, Length (InputString), 1);
        Multiplier := Multiplier * 16;
        END;

    xResult := 0;
    END;



PROCEDURE IncrementMonth (VAR DateString: Str20);

{ This procedure will increment the month of the date string to the next
  month and set the day to 1.  If it is in December, the year will also
  be incremented. }

VAR MonthString, YearString: Str20;
    Year, xResult: INTEGER;

    BEGIN
    MonthString := UpperCase (BracketedString (DateString, '-', '-'));

    YearString := Copy (DateString, Length (DateString) - 1, 2);
    Val (YearString, Year, xResult);

    IF MonthString = 'JAN' THEN
        DateString := '1-FEB-' + YearString;

    IF MonthString = 'FEB' THEN
        DateString := '1-MAR-' + YearString;

    IF MonthString = 'MAR' THEN
        DateString := '1-APR-' + YearString;

    IF MonthString = 'APR' THEN
        DateString := '1-MAY-' + YearString;

    IF MonthString = 'MAY' THEN
        DateString := '1-JUN-' + YearString;

    IF MonthString = 'JUN' THEN
        DateString := '1-JUL-' + YearString;

    IF MonthString = 'JUL' THEN
        DateString := '1-AUG-' + YearString;

    IF MonthString = 'AUG' THEN
        DateString := '1-SEP-' + YearString;

    IF MonthString = 'SEP' THEN
        DateString := '1-OCT-' + YearString;

    IF MonthString = 'OCT' THEN
        DateString := '1-NOV-' + YearString;

    IF MonthString = 'NOV' THEN
        DateString := '1-DEC-' + YearString;

    IF MonthString = 'DEC' THEN
        BEGIN
        Inc (Year);
        IF Year > 99 THEN Year := 0;

        Str (Year, YearString);

        WHILE Length (YearString) < 2 DO
            YearString := '0' + YearString;

        DateString := '1-JAN-' + YearString;
        END;
    END;



PROCEDURE IncrementMinute (VAR DateString: Str20; VAR TimeString: Str80);

{ This procedure will add a day to the DateString passed to it.  The
  string is in the format dd-mon-yr.  It will handle month ends and
  increment the year correctly (including leap years). }

VAR Day, Hour, Minute, Year, xResult: INTEGER;
    MinuteString, HourString, DayString, MonthString, YearString: Str20;


    BEGIN
    Val (PostcedingString (TimeString, ':'), Minute, xResult);
    Val (PrecedingString  (TimeString, ':'), Hour,   xResult);

    Inc (Minute);

    IF Minute > 59 THEN
        BEGIN
        Minute := 0;
        Inc (Hour);

        IF Hour > 23 THEN
            BEGIN
            Hour := 0;

            Val (PrecedingString (DateString, '-'), Day, xResult);
            Inc (Day);
            Str (Day, DayString);

            WHILE Length (DayString) < 2 DO
                DayString := '0' + DayString;

            Delete (DateString, 1, Pos ('-', DateString) - 1);
            Insert (DayString, DateString, 1);

            { Now check for new month }

            IF Day > 28 THEN { All months have at least 28 days }
                BEGIN
                MonthString := UpperCase (BracketedString (DateString, '-', '-'));

                IF (MonthString = 'JAN') OR (MonthString = 'MAR') OR
                   (MonthString = 'MAY') OR (MonthString = 'JUL') OR
                   (MonthString = 'AUG') OR (MonthString = 'OCT') OR
                   (MonthString = 'DEC') THEN
                       IF Day > 31 THEN IncrementMonth (DateString);

                IF MonthString = 'FEB' THEN
                    BEGIN
                    YearString := Copy (DateString, Length (DateString) - 1, 2);
                    Val (YearString, Year, xResult);

                    IF (Year MOD 4 = 0) AND (Year <> 0) THEN { Leap year }
                        BEGIN
                        IF Day > 29 THEN IncrementMonth (DateString);
                        END
                    ELSE
                        IF Day > 28 THEN IncrementMonth (DateString);

                    END;

                IF (MonthString = 'APR') OR (MonthString = 'JUN') OR
                   (MonthString = 'SEP') OR (MonthString = 'NOV') THEN
                       IF Day > 30 THEN IncrementMonth (DateString);

                END;
            END;


        END;

    Str (Minute, MinuteString);

    WHILE Length (MinuteString) < 2 DO
        MinuteString := '0' + MinuteString;

    Str (Hour, HourString);

    WHILE Length (HourString) < 2 DO
        HourString := '0' + HourString;

    TimeString := HourString + ':' + MinuteString;
    END;



FUNCTION KeyId (Key: CHAR): Str80;

    BEGIN
    CASE Key OF
        F1: KeyId := 'F1';
        F2: KeyId := 'F2';
        F3: KeyId := 'F3';
        F4: KeyId := 'F4';
        F5: KeyId := 'F5';
        F6: KeyId := 'F6';
        F7: KeyId := 'F7';
        F8: KeyId := 'F8';
        F9: KeyId := 'F9';
       F10: KeyId := 'F10';
       F11: KeyId := 'F11';
       F12: KeyId := 'F12';

       AltF1: KeyId := 'AltF1';
       AltF2: KeyId := 'AltF2';
       AltF3: KeyId := 'AltF3';
       AltF4: KeyId := 'AltF4';
       AltF5: KeyId := 'AltF5';
       AltF6: KeyId := 'AltF6';
       AltF7: KeyId := 'AltF7';
       AltF8: KeyId := 'AltF8';
       AltF9: KeyId := 'AltF9';
      AltF10: KeyId := 'AltF10';
      AltF11: KeyId := 'AltF11';
      AltF12: KeyId := 'AltF12';

       ControlF1: KeyId := 'ControlF1';
       ControlF2: KeyId := 'ControlF2';
       ControlF3: KeyId := 'ControlF3';
       ControlF4: KeyId := 'ControlF4';
       ControlF5: KeyId := 'ControlF5';
       ControlF6: KeyId := 'ControlF6';
       ControlF7: KeyId := 'ControlF7';
       ControlF8: KeyId := 'ControlF8';
       ControlF9: KeyId := 'ControlF9';
      ControlF10: KeyId := 'ControlF10';
      ControlF11: KeyId := 'ControlF11';
      ControlF12: KeyId := 'ControlF12';

      ELSE KeyId := '';
      END;

    END;



PROCEDURE DisplayLineInputString (Str: Str160; Cursor: INTEGER; EndOfPrompt: INTEGER);

VAR DisplayArea, Offset: INTEGER;

    BEGIN
    GoToXY (EndOfPrompt, WhereY);
    ClrEol;

    DisplayArea := Lo (WindMax) - EndOfPrompt - 2;

    IF Length (Str) < DisplayArea THEN
        BEGIN
        Write (Str);
        GoToXY (Cursor + EndOfPrompt - 1, WhereY);
        END
    ELSE
        BEGIN
        Offset := 0;

        IF Cursor >= DisplayArea - 1 THEN
            BEGIN
            REPEAT
                Offset := Offset + 8
            UNTIL Cursor - Offset < DisplayArea - 3;

            IF Length (Str) - Offset > DisplayArea THEN
                Write ('+', Copy (Str, Offset, DisplayArea - 3), '+')
            ELSE
                Write ('+', Copy (Str, Offset, DisplayArea - 2));
            GoToXY (Cursor - Offset + EndOfPrompt + 1, WhereY);
            END
        ELSE
            BEGIN
            Write (Copy (Str, 1, DisplayArea - 2), '+');
            GoToXY (Cursor - Offset + EndOfPrompt - 1, WhereY);
            END;

        END;
    END;



FUNCTION LineInput (Prompt: Str160;
                    InitialString: Str160;
                    OverWriteEnable: BOOLEAN;
                    ExitOnAltKey: BOOLEAN): Str160;

{ This function will display the prompt and allow the operator to input
  a response to the prompt.  If an InitialString is passed along, it will
  be displayed as the initial entry value.  It can be edited, or written
  over as desired.  An escape with no entry will give a result of escape
  key.  If the input or initial entry goes beyond the right of the window,
  it will be handled nicely I hope. }

VAR Key: CHAR;
    InputString, TempString: Str160;
    CursorPosition, EndOfPrompt: INTEGER;
    VirginEntry, InsertMode: BOOLEAN;

    BEGIN
    ClrScr;
    Write (Prompt);
    EndOfPrompt := WhereX;
    CursorPosition := Length (InitialString) + 1;

    DisplayLineInputString (InitialString, CursorPosition, EndOfPrompt);

    InputString := InitialString;
    InsertMode := True;
    VirginEntry := OverWriteEnable;

    REPEAT
        REPEAT millisleep UNTIL KeyPressed;
        Key := ReadKey;

        CASE Key OF
            EscapeKey:
                BEGIN
                IF InputString = '' THEN
                    BEGIN
                    LineInput := EscapeKey;
                    Exit;
                    END;

                InputString := '';
                CursorPosition := 1;
                VirginEntry := False;
                END;

            BackSpace:
                BEGIN
                IF CursorPosition > 1 THEN
                    BEGIN
                    Delete (InputString, CursorPosition - 1, 1);
                    Dec (CursorPosition);
                    END;
                VirginEntry := False;
                END;

            ControlA:
                IF CursorPosition > 1 THEN
                    BEGIN
                    REPEAT
                        Dec (CursorPosition);
                    UNTIL (CursorPosition = 1) OR
                          ((InputString [CursorPosition - 1] = ' ') AND
                           (InputString [CursorPosition] <> ' '));
                    VirginEntry := False;
                    END;

            ControlS:
                IF CursorPosition > 1 THEN
                    BEGIN
                    Dec (CursorPosition);
                    VirginEntry := False;
                    END;


            ControlD:
                IF CursorPosition < (Length (InputString) + 1) THEN
                    BEGIN
                    Inc (CursorPosition);
                    VirginEntry := False;
                    END;

            ControlF:
                IF CursorPosition < (Length (InputString) + 1) THEN
                    BEGIN
                    REPEAT
                        Inc (CursorPosition);
                    UNTIL (CursorPosition = Length (InputString) + 1) OR
                          ((InputString [CursorPosition - 1] = ' ') AND
                           (InputString [CursorPosition] <> ' '));
                    VirginEntry := False;
                    END;

            ControlG:
                IF CursorPosition < Length (InputString) + 1 THEN
                    BEGIN
                    Delete (InputString, CursorPosition, 1);
                    VirginEntry := False;
                    END;




            ControlP:
                BEGIN
                REPEAT UNTIL KeyPressed;
                Key := ReadKey;

                IF VirginEntry THEN
                    BEGIN
                    InputString := Key;
                    VirginEntry := False;
                    CursorPosition := 2;
                    END
                ELSE
                    BEGIN
                    IF InsertMode THEN
                        Insert (Key, InputString, CursorPosition)
                    ELSE
                        InputString [CursorPosition] := Key;
                    Inc (CursorPosition);
                    END;
                END;

            ControlT:
                BEGIN
                TempString := Copy (InputString, CursorPosition, 200);
                Delete (InputString, CursorPosition, 200);
                TempString := StringWithFirstWordDeleted (TempString);
                InputString := InputString + TempString;
                VirginEntry := False;
                END;

            ControlY:
                BEGIN
                InputString := '';
                VirginEntry := False;
                END;

            NullKey:
                BEGIN
                VirginEntry := False;
                Key := ReadKey;

                CASE Key OF
                    HomeKey: CursorPosition := 1;

                    LeftArrow:
                        IF CursorPosition > 1 THEN
                            Dec (CursorPosition);

                    RightArrow:
                        IF CursorPosition < Length (InputString) + 1 THEN
                            Inc (CursorPosition);

                    EndKey:
                        CursorPosition := Length (InputString) + 1;

                    InsertKey: InsertMode := NOT InsertMode;

                    DeleteKey:
                        IF CursorPosition <= Length (InputString) THEN
                            Delete (InputString, CursorPosition, 1);

                    ELSE
                        IF ExitOnAltKey THEN
                            BEGIN
                            LineInput := NullKey + Key;
                            Exit;
                            END;
                    END;

                END



            ELSE
                IF Key >= ' ' THEN
                    BEGIN
                    IF VirginEntry THEN
                        BEGIN
                        InputString := Key;
                        VirginEntry := False;
                        CursorPosition := 2;
                        END
                    ELSE
                        BEGIN
                        IF InsertMode THEN
                            Insert (Key, InputString, CursorPosition)
                        ELSE
                            InputString [CursorPosition] := Key;
                        Inc (CursorPosition);
                        END;
                    END
                ELSE
                    IF Key = CarriageReturn THEN
                        BEGIN
                        LineInput := InputString;
                        Exit;
                        END;

            END;

    DisplayLineInputString (InputString, CursorPosition, EndOfPrompt);
    UNTIL False;
    END;



FUNCTION Lpt1BaseAddress: WORD;

VAR Address: WORD;

    BEGIN
    Address := paralleladdress(1);
    IF Address = 0 THEN
        Lpt1BaseAddress := $3BC
    ELSE
        Lpt1BaseAddress := Address;
    END;

FUNCTION Lpt2BaseAddress: WORD;

VAR Address: WORD;

    BEGIN
    Address := paralleladdress(2);
    IF Address = 0 THEN
        Lpt2BaseAddress := $278
    ELSE
        Lpt2BaseAddress := Address;
    END;

FUNCTION Lpt3BaseAddress: WORD;

VAR Address: WORD;

    BEGIN
    Address := paralleladdress(3);
    IF Address = 0 THEN
        Lpt3BaseAddress := $378
    ELSE
        Lpt3BaseAddress := Address;
    END;


FUNCTION MakeDupeFilename (Band: BandType; Mode: ModeType): Str80;

{ This procedure will generate the duping filename for the band and mode
    specified.                                                                }

    BEGIN
    MakeDupeFilename := 'L' + ModeString [Mode] + BandString [Band];
    END;



FUNCTION MinutesToTimeString (Minutes: INTEGER): Str20;

VAR Hours: INTEGER;
    HourString, MinuteString: Str20;

    BEGIN
    Hours := Minutes DIV 60;
    Minutes := Minutes MOD 60;

    Str (Hours, HourString);
    Str (Minutes, MinuteString);
    WHILE Length (MinuteString) < 2 DO MinuteString := '0' + MinuteString;
    MinutesToTimeString := HourString + ':' + MinuteString;
    END;



FUNCTION NextMinute (PreviousDateString: Str20; DateString: Str20;
                     PreviousTimeString: Str20; TimeString: Str20): BOOLEAN;

    BEGIN
    IncrementMinute (PreviousDateString, PreviousTimeString);
    NextMinute := SameMinute (PreviousDateString, DateString, PreviousTimeString, TimeString);
    END;


PROCEDURE NoCursor;

    BEGIN
    END;



FUNCTION OkayToDeleteExistingFile (FileName: Str80): BOOLEAN;

VAR Key: CHAR;

    BEGIN
    OkayToDeleteExistingFile := False;

    REPEAT
        Key := UpCase (GetKey (FileName + ' already exists.  Okay to delete? (Y/N): '));
    UNTIL (Key = 'N') OR (Key = EscapeKey) OR (Key = 'Y');

    GoToXY (1, WhereY);
    ClrEol;

    OkayToDeleteExistingFile := Key = 'Y';
    TextColor (Cyan);
    END;



FUNCTION OkayToProceed: BOOLEAN;

VAR Key: CHAR;

    BEGIN
    OkayToProceed := False;

    REPEAT
        Key := UpCase (GetKey ('Okay to proceed? (Y/N): '));
    UNTIL (Key = 'N') OR (Key = EscapeKey) OR (Key = 'Y');

    GoToXY (1, WhereY);
    ClrEol;

    OkayToProceed := Key = 'Y';
    TextColor (Cyan);
    END;



FUNCTION OpenDupeFileForRead (VAR FileHandle: TEXT; Filename: Str80): BOOLEAN;

{ This function will open a duping file and make it ready to read in.  If
    the file does not exist or does not appear to be a duping file, it will
    return FALSE.  If it does exist, the next line to be read will be the
    title.                                                                  }

VAR TempString: Str80;

    BEGIN
    OpenDupeFileForRead := False;
    IF NOT OpenFileForRead (FileHandle, FileName) THEN Exit;
    ReadLn (FileHandle, TempString);
    OpenDupeFileForRead := TempString = 'DUPE';
    END;



PROCEDURE PinWheel;

    BEGIN
    GoToXY (1, WhereY);
    Inc (ActivityCounter);

    CASE ActivityCounter OF
        1: Write ('-');
        2: Write ('\');
        3: Write ('|');
        ELSE BEGIN
             Write ('/');
             ActivityCounter := 0;
             END;
        END;
    END;



PROCEDURE QuickBeep;

    BEGIN
    LSound (CWPitch);
    Delay (DitLength);
    LNoSound;
    Delay (DitLength);
    END;



PROCEDURE RenameFile (OldName: Str80; NewName: Str80);

{ This procedure will rename the filename specified to the new name
    indicated.  If a file existed with the newname, it is deleted first. }

VAR F: Text;

    BEGIN
    DeleteFile (NewName);
    Assign (F, OldName);
    Rename (F, NewName);
    END;


PROCEDURE ReportError (Prompt: Str80);

{ This procedure will print the string passed to it in red}
//KS removed beep

    BEGIN
    TextColor (Red);
    WriteLn (Prompt);
    TextColor (Yellow);
    END;


FUNCTION SameMinute (PreviousDateString: Str20; DateString: Str20;
                     PreviousTimeString: Str20; TimeString: Str20): BOOLEAN;

    BEGIN
    SameMinute := (PreviousDateString = DateString) AND (PreviousTimeString = TimeString);
    END;


PROCEDURE SendIntegerInMorse (Value: INTEGER);

VAR TempString: Str20;

    BEGIN
    Str (Value, TempString);
    SendMorse (TempString);
    END;



PROCEDURE SendMorse (Message: Str255);

VAR Pointer: INTEGER;

    BEGIN
    Message := UpperCase (Message);
    FOR Pointer := 1 TO Length (Message) DO
    BEGIN
    CASE Message [Pointer] OF
        'A': BEGIN Dit;Dah; END;
        'B': BEGIN Dah;Dit;Dit;Dit; END;
        'C': BEGIN Dah;Dit;Dah;Dit; END;
        'D': BEGIN Dah;Dit;Dit; END;
        'E': BEGIN Dit; END;
        'F': BEGIN Dit;Dit;Dah;Dit; END;
        'G': BEGIN Dah;Dah;Dit; END;
        'H': BEGIN Dit;Dit;Dit;Dit; END;
        'I': BEGIN Dit;Dit; END;
        'J': BEGIN DIt;Dah;Dah;Dah; END;
        'K': BEGIN Dah;Dit;Dah; END;
        'L': BEGIN Dit;Dah;Dit;Dit; END;
        'M': BEGIN Dah;Dah; END;
        'N': BEGIN Dah;Dit; END;
        'O': BEGIN Dah;Dah;Dah; END;
        'P': BEGIN Dit;Dah;Dah;Dit; END;
        'Q': BEGIN Dah;Dah;Dit;Dah; END;
        'R': BEGIN Dit;Dah;Dit; END;
        'S': BEGIN Dit;Dit;Dit; END;
        'T': BEGIN Dah; END;
        'U': BEGIN Dit;Dit;Dah; END;
        'V': BEGIN Dit;Dit;Dit;Dah; END;
        'W': BEGIN Dit;Dah;Dah; END;
        'X': BEGIN Dah;Dit;Dit;Dah; END;
        'Y': BEGIN Dah;Dit;Dah;Dah; END;
        'Z': BEGIN Dah;Dah;Dit;Dit; END;
        '0' : BEGIN Dah;Dah;Dah;Dah;Dah; END;
        '1' : BEGIN Dit;Dah;Dah;Dah;Dah; END;
        '2' : BEGIN Dit;Dit;Dah;Dah;Dah; END;
        '3' : BEGIN Dit;Dit;Dit;Dah;Dah; END;
        '4' : BEGIN Dit;Dit;Dit;Dit;Dah; END;
        '5' : BEGIN Dit;Dit;Dit;Dit;Dit; END;
        '6' : BEGIN Dah;Dit;Dit;Dit;Dit; END;
        '7' : BEGIN Dah;Dah;Dit;Dit;Dit; END;
        '8' : BEGIN Dah;Dah;Dah;Dit;Dit; END;
        '9' : BEGIN Dah;Dah;Dah;Dah;Dit; END;
        '.' : BEGIN Dit;Dah;Dit;Dah;Dit;Dah; END;
        ',' : BEGIN Dah;Dah;Dit;Dit;Dah;Dah; END;
        '?' : BEGIN Dit;Dit;Dah;Dah;Dit;Dit; END;
        '/' : BEGIN Dah;Dit;Dit;Dah;Dit; END;
        '<' : BEGIN Dit;Dit;Dit;Dah;Dit;Dah; END;
        ' ' : Delay (WordSpace);
        END;
    Delay (CharacterSpace);
    END;
    END;



PROCEDURE SetMorseSpeed (Speed: INTEGER; Pitch: INTEGER);

    BEGIN
    IF Speed > 0 THEN
        BEGIN
        DitLength := 1200 DIV Speed;
        DahLength := DitLength * 3;
        CharacterSpace := DitLength;
        WordSpace := 2 * DitLength + (DitLength DIV 2);
        END;
    CWPitch := Pitch;
    END;



FUNCTION DirectoryExists (DirName: Str80): BOOLEAN;

VAR DirInfo: SearchRec;

    BEGIN
    FindFirst (DirName, Directory, DirInfo);
    DirectoryExists := DosError = 0;
    END;



PROCEDURE DirectoryShowCursor (Entry: INTEGER;  StartY: INTEGER);

VAR Row, Col: INTEGER;

    BEGIN
    NoCursor;
    Row := Entry DIV 5 + 1;
    Col := (Entry MOD 5) * 15 + 1;
    GoToXY (Col, Row + StartY);
    Write ('<');
    Col := (Entry MOD 5) * 15 + 14;
    GoToXY (Col, Row + StartY);
    Write ('>');
    END;

PROCEDURE DirectoryClearCursor (Entry: INTEGER;  StartY: INTEGER);

VAR Row, Col: INTEGER;

    BEGIN
    Row := Entry DIV 5 + 1;
    Col := (Entry MOD 5) * 15 + 1;
    GoToXY (Col, Row + StartY);
    Write (' ');
    Col := (Entry MOD 5) * 15 + 14;
    GoToXY (Col, Row + StartY);
    Write (' ');
    END;



FUNCTION ShowDirectoryAndSelectFile (PathAndMask: Str80;
                                     InitialFile: Str80;
                                     ClearOnCarriageReturn: BOOLEAN): Str80;

{ This procedure will show the directory of files found in the specified
    path.  The files are shown 5 across starting on the line the cursor is
    on.  The available window will be filled with entries and if there are
    more, moving the cursor below the window will scroll the entries up.
    The cursor can select a file by pressing return. The complete filename
    with path will be returned.  If an escape is hit, the string will be
    the escape char.  The complete window will be cleared before returning. }

VAR DirInfo: SearchRec;
    NumberFiles, FileNumber, SelectedFile, StartY, Row, Col: INTEGER;
    StartIndex, Line, NumberLines, Address, BubbleCount, Index: INTEGER;
    FileNames: ARRAY [0..255] OF STRING [12];
    InputString, TempString, Dir, Name, Ext: Str20;
    Key: Char;

    BEGIN
    ShowDirectoryAndSelectFile := '';
    NumberFiles := 0;
    StartIndex := 0;

    FSplit (PathAndMask, Dir, Name, Ext);
    IF Name = '' THEN PathAndMask := Dir + '*.*';

    FindFirst (PathAndMask, Archive, DirInfo);

    WHILE DosError = 0 DO
        BEGIN
        FileNames [NumberFiles] := DirInfo.Name;
        Inc (NumberFiles);
        FindNext (DirInfo);
        END;

    WhereX;
    StartY := WhereY;
    SelectedFile := 0;

    IF InitialFile = '' THEN
        WriteLn ('DIRECTORY for ', PathAndMask, ' : ')
    ELSE
        GoToXY (1, WhereY + 1);

    NumberLines := 1;

    IF NumberFiles > 0 THEN
        BEGIN
        IF NumberFiles > 1 THEN
            BEGIN
            Index := NumberFiles - 2;

            FOR BubbleCount := 1 TO NumberFiles - 1 DO
                BEGIN
                FOR Address := 0 TO Index DO
                    IF FileNames [Address] > FileNames [Address + 1] THEN
                        BEGIN
                        TempString := FileNames [Address + 1];
                        FileNames [Address + 1] := FileNames [Address];
                        FileNames [Address] := TempString;
                        END;
                Dec (Index);
                END;
            END;

        FOR FileNumber := 0 TO NumberFiles - 1 DO
            BEGIN
            Row := FileNumber DIV 5;
            Col := (FileNumber MOD 5) * 15 + 2;
            GoToXY (Col, Row + StartY + 1);

            IF InitialFile = '' THEN
                Write (Copy (FileNames [FileNumber], 1, 12));

            IF FileNames [FileNumber] = InitialFile THEN
                StartIndex := FileNumber;
            IF Col = 2 THEN Inc (NumberLines);
            END;

        DirectoryShowCursor (StartIndex, StartY);

        SelectedFile := StartIndex;

        InputString := '';

        REPEAT
            Key := UpCase (ReadKey);

            CASE Key OF
                EscapeKey, ControlC:
                    BEGIN
                    ShowDirectoryAndSelectFile := Key;
                    GoToXY (1, StartY);
                    FOR Line := 1 TO NumberLines DO
                        BEGIN
                        ClrEol;
                        GoToXY (1, WhereY + 1);
                        END;
                    GoToXY (1, StartY);
                    SmallCursor;
                    Exit;
                    END;


                CarriageReturn:
                    BEGIN
                    ShowDirectoryAndSelectFile := Dir + FileNames [SelectedFile];
                    IF ClearOnCarriageReturn THEN
                        BEGIN
                        GoToXY (1, StartY);
                        FOR Line := 1 TO NumberLines DO
                            BEGIN
                            ClrEol;
                            GoToXY (1, WhereY + 1);
                            END;
                        GoToXY (1, StartY);
                        SmallCursor;
                        END;
                    Exit;
                    END;


                BackSpace:
                    IF InputString <> '' THEN
                        BEGIN
                        Delete (InputString, Length (InputString), 1);

                        FOR FileNumber := 0 TO NumberFiles - 1 DO
                            BEGIN
                            IF Pos (InputString, FileNames [FileNumber]) = 1 THEN
                                BEGIN
                                DirectoryClearCursor (SelectedFile, StartY);
                                SelectedFile := FileNumber;
                                DirectoryShowCursor (SelectedFile, StartY);
                                Break;
                                END;
                            END;
                        END;


                NullKey:
                    BEGIN
                    Key := ReadKey;

                    CASE Key OF
                        RightArrow:
                            IF SelectedFile < NumberFiles - 1 THEN
                                BEGIN
                                DirectoryClearCursor (SelectedFile, StartY);
                                Inc (SelectedFile);
                                DirectoryShowCursor (SelectedFile, StartY);
                                END;

                        LeftArrow:
                            IF SelectedFile > 0 THEN
                                BEGIN
                                DirectoryClearCursor (SelectedFile, StartY);
                                Dec (SelectedFile);
                                DirectoryShowCursor (SelectedFile, StartY);
                                END;

                        UpArrow:
                            IF SelectedFile - 5 >= 0 THEN
                                BEGIN
                                DirectoryClearCursor (SelectedFile, StartY);
                                SelectedFile := SelectedFile - 5;
                                DirectoryShowCursor (SelectedFile, StartY);
                                END;

                        DownArrow:
                            IF SelectedFile + 5 <= NumberFiles - 1 THEN
                                BEGIN
                                DirectoryClearCursor (SelectedFile, StartY);
                                SelectedFile := SelectedFile + 5;
                                DirectoryShowCursor (SelectedFile, StartY);
                                END;

                        HomeKey:
                            BEGIN
                            DirectoryClearCursor (SelectedFile, StartY);
                            SelectedFile := 0;
                            DirectoryShowCursor (SelectedFile, StartY);
                            END;

                        EndKey:
                            BEGIN
                            DirectoryClearCursor (SelectedFile, StartY);
                            SelectedFile := NumberFiles - 1;
                            DirectoryShowCursor (SelectedFile, StartY);
                            END;

                        END;
                    END;

                ELSE
                    IF ((Key >= 'A') AND (Key <= 'Z')) OR
                       ((Key >= '0') AND (Key <= '9')) THEN
                        BEGIN
                        InputString := InputString + Key;

                        FOR FileNumber := 0 TO NumberFiles - 1 DO
                            BEGIN
                            IF Pos (InputString, FileNames [FileNumber]) = 1 THEN
                                BEGIN
                                DirectoryClearCursor (SelectedFile, StartY);
                                SelectedFile := FileNumber;
                                DirectoryShowCursor (SelectedFile, StartY);
                                Break;
                                END;
                            END;
                        END;

                END;
        UNTIL False;
        END
    ELSE
        BEGIN
        Write ('No files found!!  Press any key to continue.');
        REPEAT UNTIL KeyPressed;
        WHILE KeyPressed DO Key := ReadKey;
        GoToXY (1, WhereY);
        ClrEol;
        ShowDirectoryAndSelectFile := EscapeKey;
        SmallCursor;
        END;
    END;



PROCEDURE BigCursor;

    BEGIN
    END;

PROCEDURE SmallCursor;


    BEGIN
    END;



FUNCTION TimeElasped (StartHour, StartMinute, StartSecond, NumberOfMinutes: INTEGER): BOOLEAN;

{ This function will compare the present time to the start time given to
  it and decide if the NumberOfMinutes have elasped since the start time.
  It will return TRUE at least the NumberOfMinutes have elasped since
  the start time.                                                          }

VAR Hour, Minute, Second, Sec100: WORD;
    th,tm,ts,ts100: longint;

    BEGIN
    GetTime (Hour, Minute, Second, Sec100);
    th := hour;
    tm := minute;
    ts := second;
    ts100 := sec100;
    ts100 := 100*(ts-startsecond+60*(tm-startminute
       +60*(th-starthour)));
    if ts100 < 0 then ts100 := ts100+360000;
    if ts100 < 0 then ts100 := 0;
    timeelasped := (ts100 div 6000) >=  numberofminutes;
    END;




PROCEDURE TimeStamp (VAR FileWrite: TEXT);

    BEGIN
    WriteLn (FileWrite, '      This report generated on ', GetDayString,
                        ', ', GetDateString, ' at ', GetTimeString, '.');
    END;



PROCEDURE WaitForKeyPressed;

VAR OrigMode: INTEGER;
// Key: CHAR;

    BEGIN
    OrigMode := LastMode;
    WriteLn;
    TextColor (Cyan);
    Write ('Press any key to continue...');
    REPEAT UNTIL KeyPressed;
    ReadKey;
    GoToXY (1, WhereY);
    ClrEol;
    TextMode(OrigMode);
    END;



PROCEDURE WakeUp;

    BEGIN
    SixteenthNote (NoteC);
    SixteenthNote (NoteC);
    SixteenthNote (NoteF);
    SixteenthNote (NoteC);
    SixteenthNote (NoteF);
    SixteenthNote (NoteA);
    SixteenthNote (NoteF);
    SixteenthNote (NoNote);
    SixteenthNote (NoteF);
    SixteenthNote (NoteF);
    SixteenthNote (NoteA);
    SixteenthNote (NoteF);
    SixteenthNote (NoteA);
    SixteenthNote (NoteHiC);
    SixteenthNote (NoteA);
    SixteenthNote (NoNote);
    SixteenthNote (NoteF);
    SixteenthNote (NoteA);
    EigthNote     (NoteHiC);
    SixteenthNote (NoteA);
    SixteenthNote (NoteF);
    EigthNote     (NoteC);
    SixteenthNote (NoteC);
    SixteenthNote (NoteC);
    EigthNote     (NoteF);
    SixteenthNote (NoteF);
    SixteenthNote (NoteF);
    Delay (10);
    EigthNote     (NoteF);
    END;


PROCEDURE WriteLnCenter (Prompt: Str80);

VAR ScreenWidth, CenterSpaces: INTEGER;

    BEGIN
    ScreenWidth := Lo (WindMax);

    CenterSpaces := (ScreenWidth DIV 2) - (Length (Prompt) DIV 2);
    IF CenterSpaces > 0 THEN GoToXY (CenterSpaces, WhereY);
    WriteLn (Prompt);
    END;


PROCEDURE WriteLnVarCenter (VAR FileWrite: TEXT; Prompt: Str80);

VAR Space, CenterSpaces: INTEGER;

    BEGIN
    CenterSpaces := 40 - (Length (Prompt) DIV 2);

    IF CenterSpaces > 0 THEN FOR Space := 1 TO CenterSpaces DO
        Prompt := ' ' + Prompt;

    WriteLn (FileWrite, Prompt);
    END;


PROCEDURE WriteLnLstCenter (Prompt: Str80);

VAR Space, CenterSpaces: INTEGER;

    BEGIN
    CenterSpaces := 40 - (Length (Prompt) DIV 2);
    IF CenterSpaces > 0 THEN FOR Space := 1 TO CenterSpaces DO Write (Lst, ' ');
    WriteLn (Lst, Prompt);
    END;


PROCEDURE WriteHexByte (HexByte: Byte);

    BEGIN
    Write (HexChars [Lo (HexByte) shr 4], HexChars [Lo (HexByte) and $F]);
    END;


PROCEDURE WriteHexWord (HexWord: Word);

    BEGIN
    Write (HexChars [Hi (HexWord) shr 4], HexChars [Hi (HexWord) and $F],
           HexChars [Lo (HexWord) shr 4], HexChars [Lo (HexWord) and $F]);
    END;


PROCEDURE WriteHexLongInt (HexLogInt: LONGINT);

VAR Divisor: LONGINT;
    FirstCharacterPrinted: BOOLEAN;
    Count: BYTE;

    BEGIN
    FirstCharacterPrinted := False;

    Divisor := 268435456;

    REPEAT
        IF HexLogInt >= Divisor THEN
            BEGIN
            Count := HexLogInt DIV Divisor;
            HexLogInt := HexLogInt MOD Divisor;
            Write (HexChars [Count]);
            FirstCharacterPrinted := True;
            END
        ELSE
            IF FirstCharacterPrinted THEN Write ('0');

        IF Divisor <= 1 THEN Exit;
        Divisor := Divisor DIV 16;

    UNTIL False;
    END;



PROCEDURE WritePortType (Port: PortType);

    BEGIN
    CASE Port OF
        NoPort:  Write ('None');
        Serial1: Write ('COM1');
        Serial2: Write ('COM2');
        Serial3: Write ('COM3');
        Serial4: Write ('COM4');
        Serial5: Write ('COM5');
        Serial6: Write ('COM6');

        Parallel1: Write ('LPT1');
        Parallel2: Write ('LPT2');
        Parallel3: Write ('LPT3');
        END;
    END;



PROCEDURE MouseInit;

    BEGIN
    END;



FUNCTION CSV (Letter: CallString): INTEGER;

{ Computers character sort value for CallSortValue routine }

    BEGIN
    IF Letter = '' THEN
        BEGIN
        CSV := 0;
        Exit;
        END;

    IF Letter = '/' THEN
        BEGIN
        CSV := 1;
        Exit;
        END;

    IF (Letter >= '0') AND (Letter <= '9') THEN
        BEGIN
        CSV := Ord (Letter [1]) - Ord ('0') + 1;
        Exit;
        END;

    IF (Letter >= 'A') AND (Letter <= 'Z') THEN
        BEGIN
        CSV := Ord (Letter [1]) - Ord ('A') + 11;
        Exit;
        END;
    END;



FUNCTION CallSortValue (Call: CallString): LONGINT;

{ Processes /, 0-9 and A-Z for the first 6 letters }

VAR CharPos: INTEGER;
    Total: REAL;

    BEGIN
    Total := 0;

    IF Call = '' THEN
        BEGIN
        CallSortValue := 0;
        Exit;
        END;

    FOR CharPos:= 1 TO 6 DO
        Total := CSV (Copy (Call, CharPos, 1)) + (Total * 37);

    CallSortValue := Round (Total - 2147483648.0);
    END;



FUNCTION GetStateFromSection (Section: Str20): Str20;

    BEGIN
    Section := UpperCase (Section);

    IF (Section = 'AK') OR (Section = 'AL') OR (Section = 'AR') OR
       (Section = 'AZ') OR (Section = 'CO') OR (Section = 'CT') OR
       (Section = 'DE') OR (Section = 'GA') OR (Section = 'IA') OR
       (Section = 'ID') OR (Section = 'IN') OR (Section = 'IL') OR
       (Section = 'KS') OR (Section = 'KY') OR (Section = 'LA') OR
       (Section = 'ME') OR (Section = 'MI') OR (Section = 'MN') OR
       (Section = 'MO') OR (Section = 'MS') OR (Section = 'MT') OR
       (Section = 'NC') OR (Section = 'ND') OR (Section = 'NE') OR
       (Section = 'NH') OR (Section = 'NM') OR (Section = 'NV') OR
       (Section = 'OH') OR (Section = 'OK') OR (Section = 'OR') OR

       (Section = 'RI') OR (Section = 'SD') OR (Section = 'TN') OR
       (Section = 'UT') OR (Section = 'VA') OR (Section = 'VT') OR
       (Section = 'WI') OR (Section = 'WV') OR (Section = 'WY') OR
       (Section = 'SC') THEN
           BEGIN
           GetStateFromSection := Section;
           Exit;
           END;

    IF (Section = 'EB') OR (Section = 'LAX') OR (Section = 'ORG') OR
       (Section = 'SB') OR (Section = 'SCV') OR (Section = 'SDG') OR
       (Section = 'SF') OR (Section = 'SJV') OR (Section = 'SV')  THEN
           BEGIN
           GetStateFromSection := 'CA';
           Exit;
           END;

    IF (Section = 'EM') OR (Section = 'WM') THEN
        BEGIN
        GetStateFromSection := 'MA';
        Exit;
        END;

    IF (Section = 'EN')  OR (Section = 'WNY') OR (Section = 'NNY') OR
       (Section = 'ENY') OR (Section = 'NLI') THEN
           BEGIN
           GetStateFromSection := 'NY';
           Exit;
           END;

    IF (Section = 'EP') OR (Section = 'WP') THEN
        BEGIN
        GetStateFromSection := 'PA';
        Exit;
        END;

    IF (Section = 'EW') OR (Section = 'WWA') OR (Section = 'EWA') THEN
        BEGIN
        GetStateFromSection := 'WA';
        Exit;
        END;

    IF (Section = 'NF') OR (Section = 'SF') THEN
        BEGIN
        GetStateFromSection := 'FL';
        Exit;
        END;

    IF (Section = 'NNJ') OR (Section = 'SNJ') THEN
        BEGIN
        GetStateFromSection := 'NJ';
        Exit;
        END;

    IF (Section = 'NTX') OR (Section = 'STX') OR (Section = 'WTX') THEN
        BEGIN
        GetStateFromSection := 'TX';
        Exit;
        END;

    GetStateFromSection := '';
    END;




    BEGIN
    Beat := 500;
    SetMorseSpeed (36, 700);
    MouseInit;
    SetUpComPortAddresses;
    END.


