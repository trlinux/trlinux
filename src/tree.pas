UNIT Tree;

{ This unit contains all the neat little library routines that are used
    in most of the N6TR programs.  They are not specfic to any program
    which is a requirement for adding new routines. }

{$O+}
{$F+}
{$V-}

INTERFACE

uses communication,baseunix;

TYPE
    Str10 = STRING [10];
    Str20 = STRING [20];
    Str30 = STRING [30];
    Str40 = STRING [40];
    Str80 = STRING [80];
    Str160 = STRING [160];
    Str255 = STRING [255];

    CallString   = STRING [12];
    GridString   = STRING [4];
    PrefixString = STRING [5];
    NameString   = STRING [6];
    RSTString    = STRING [3];

    MouseTypeType = (NoMouse, OneButton, TwoButton, ThreeButton);

//    ParityType = (NoParity, EvenParity, OddParity);

    TwoBytes   = ARRAY [1..2] OF BYTE;
    FourBytes  = ARRAY [1..4] OF BYTE;
    EightBytes = ARRAY [1..8] OF BYTE;

    PortType = (NoPort, Serial1, Serial2, Serial3, Serial4, Serial5, Serial6,
                Parallel1, Parallel2, Parallel3, DRSI);

    BandType = (Band160, Band80, Band40, Band20, Band15, Band10,
                Band30, Band17, Band12, Band6, Band2, Band222, Band432,
                Band902, Band1296, Band2304, Band3456, Band5760, Band10G,
                Band24G, BandLight, All, NoBand);


     ModeType = (CW, Digital, Phone, Both, NoMode, FM);  { Use for TR }
{    ModeType = (CW, Phone, Both, NoMode, FM, Digital);   }{ Use for calltest }

    MultiBandAddressArrayType = ARRAY [BandType] OF BYTE;


CONST

    HexChars: ARRAY [0..$F] of Char = '0123456789ABCDEF';

    BandString: ARRAY [BandType] OF STRING [3] = ('160',
                                                  ' 80',
                                                  ' 40',
                                                  ' 20',
                                                  ' 15',
                                                  ' 10',
                                                  ' 30',
                                                  ' 17',
                                                  ' 12',
                                                  '  6',
                                                  '  2',
                                                  '222',
                                                  '432',
                                                  '902',
                                                  '1GH',
                                                  '2GH',
                                                  '3GH',
                                                  '5GH',
                                                  '10G',
                                                  '24G',
                                                  'LGT',
                                                  'All',
                                                  'NON');


    ModeString: ARRAY [ModeType] OF STRING [3] = ('CW ',
                                                  'DIG',
                                                  'SSB',
                                                  'BTH',
                                                  'NON',
                                                  'FM ');

    MonthTags: ARRAY [1..12] OF STRING [3] = ('Jan', 'Feb', 'Mar', 'Apr',
                'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

    BufferLength = 32768;

    MultiBandAddressArray: MultiBandAddressArrayType =
        ($E0, $E1, $E2, $E3, $E4, $E5,                 {160/80/40/20/15/10}
         $E6, $E7, $E8, $E9, $EA, $EB, $EC,            {30/17/12/6/2/222/432}
         $ED, $EE, $EF, $F0, $F1, $F2, $F3, $F4,       {902/1GH/2GH/3GH/5GH/10G/24G/LGT}
         $FF, $FF);                                    {All/Non}


TYPE

    FileComparisonType = (Before, Same, After);

    TimeRecord = RECORD
      Hour: Word;
      Minute: Word;
      Second: Word;
      Sec100: Word;
      END;

CONST
    FrameEnd              = $C0;
    FrameEscape           = $DB;
    TransposedFrameEnd    = $DC;
    TransposedFrameEscape = $DD;

    FrameEndChr = Chr ($C0);
    FrameEscapeChr = Chr ($DB);
    TransposedFrameEndChr = Chr ($DC);
    TransposedFrameEscapeChr = Chr ($DD);

    MaximumFileNames = 300;


    ModemStatusAddressOffset      = 6;
    ModemControlAddressOffset     = 4;
    ModemLineControlAddressOffset = 3;
    PortStatusAddressOffset       = 5;
    ReceiveDataAddressOffset      = 0;
    TransmitDataAddressOffset     = 0;

{ The following addresses are used as the bible for finding things in log
    entries from the logging program.         }

    LogEntryBandAddress       = 1;    LogEntryBandWidth       =  3;
    LogEntryModeAddress       = 4;    LogEntryModeWidth       =  3;
    LogEntryDayAddress        = 8;    LogEntryDayWidth        =  2;
    LogEntryMonthAddress      = 11;   LogEntryMonthWidth      =  3;
    LogEntryYearAddress       = 15;   LogEntryYearWidth       =  4;
    LogEntryHourAddress       = 18;   LogEntryHourWidth       =  2;
    LogEntryMinuteAddress     = 21;   LogEntryMinuteWidth     =  2;
    LogEntryQSONumberAddress  = 24;   LogEntryQSONumberWidth  =  4;
    LogEntryComputerIDAddress = 28;   LogEntryComputerIDWidth =  1;
    LogEntryCallAddress       = 30;   LogEntryCallWidth       = 12;
    LogEntryNameSentAddress   = 42;   LogEntryNameSentWidth   =  1;
    LogEntryExchangeAddress   = 44;   LogEntryExchangeWidth   = 24;
    LogEntryMultAddress       = 69;   LogEntryMultWidth       =  8;
    LogEntryPointsAddress     = 77;   LogEntryPointsWidth     =  2;



TYPE

    BufferArrayType = ARRAY [0..BufferLength] OF BYTE;

    CharacterBuffer = OBJECT
        Tail: INTEGER;                    { Oldest entry address }
        Head: INTEGER;                    { Place where new entry goes }
        List: ^BufferArrayType;

        PROCEDURE InitializeBuffer;
        PROCEDURE AddEntry (Entry: BYTE);
        FUNCTION  FreeSpace: INTEGER;
        FUNCTION  GetNextByte (VAR Entry: BYTE): BOOLEAN;
        FUNCTION  GetSlippedString (VAR Entry: STRING): BOOLEAN;
        FUNCTION  GetNextLine      (VAR Entry: STRING): BOOLEAN;
        PROCEDURE GoAway;
        FUNCTION  IsEmpty: BOOLEAN;
        END;


    ContinentType = (NorthAmerica, SouthAmerica, Europe, Africa, Asia,
                     Oceania, UnknownContinent);

    FileNameRecord = RECORD
        NumberFiles: INTEGER;
        List: ARRAY [0..MaximumFileNames - 1] OF String [12];
        END;

    QTHRecord = RECORD
        Country:      INTEGER;
        Continent:    ContinentType;
        CountryID:    STRING [6];
        Zone:         INTEGER;
        Prefix:       PrefixString;
        StandardCall: CallString;
        END;

    BufferType = ARRAY [0..$FF] OF CHAR;
    BufferTypePtr = ^BufferType;

VAR CodeSpeed:  BYTE;
    FMMode: BOOLEAN;
    HourOffset: INTEGER;
    QuestionMarkChar: CHAR;
    SlashMarkChar:    CHAR;
    UseBIOSKeyCalls:  BOOLEAN;

    Com1PortBaseAddress: WORD;
    Com2PortBaseAddress: WORD;
    Com3PortBaseAddress: WORD;
    Com4PortBaseAddress: WORD;
    Com5PortBaseAddress: WORD;
    Com6PortBaseAddress: WORD;

    DegreeSymbol: String;

    FUNCTION  AddBand (Band: BandType): CHAR;
    FUNCTION  AddMode (Mode: ModeType): CHAR;

    FUNCTION  AlphaPartOfString (InputString: Str160): Str80;

    FUNCTION  ArcCos (X: REAL): REAL;
    FUNCTION  ATan2 (Y, X : REAL) : REAL;

    FUNCTION  BigCompressedCallsAreEqual (Call1, Call2: EightBytes): BOOLEAN;
    PROCEDURE BigCompressFormat (Call: CallString; VAR CompressedBigCall: EightBytes);
    PROCEDURE BigCursor;
    FUNCTION  BigExpandedString (Input: EightBytes): Str80;
    FUNCTION  BracketedString (LongString: Str160; StartString: Str80; StopString: Str80): Str80;
    FUNCTION  BYTADDR  (Call: Pointer; NumberCalls: INTEGER; A: Pointer): INTEGER;CDECL;
    FUNCTION  BYTDUPE  (Call: Pointer; NumberCalls: INTEGER; A: Pointer): BOOLEAN;CDECL;

    PROCEDURE CalculateBandMode (Freq: LONGINT; VAR Band: BandType; VAR Mode: ModeType);
    FUNCTION  CaliforniaCall (Call: CallString): BOOLEAN;
    FUNCTION  CallFitsFormat (Format: Str20; Call: Str20): BOOLEAN;
    FUNCTION  CallSortValue (Call: CallString): LONGINT;

    FUNCTION  CheckSum (InputString: STRING): BYTE;
    FUNCTION  CheckSumWord (InputString: STRING): WORD;

    PROCEDURE CompressFormat (Call: CallString; VAR Output: FourBytes);

    FUNCTION  ControlKeyPressed: BOOLEAN;
    FUNCTION  CopyWord (LongString: STRING; Index: INTEGER): Str80;

    PROCEDURE DecrementASCIIInteger (VAR ASCIIString: Str80);
    PROCEDURE DelayOrKeyPressed (DelayTime: INTEGER);
    FUNCTION  DeleteMult (VAR LogString: Str80; MultString: Str20): BOOLEAN;
    FUNCTION  DupeTest (Call: Str80): BOOLEAN;

    FUNCTION  ElaspedTimeString (StartTime: TimeRecord): Str20;
    FUNCTION  ElaspedSec100 (StartTime: TimeRecord): LongInt;

    FUNCTION  ExpandedString (Input: FourBytes): Str80;
    PROCEDURE ExpandTabs (VAR InputString: STRING);
    FUNCTION  ExpandTwoBytes (Input: TwoBytes): Str80;

    FUNCTION  FirstLetter (InputString: Str80): CHAR;
    PROCEDURE FormFeed;

    FUNCTION  GetChecksum8 (Call: FourBytes): INTEGER;
    FUNCTION  GetColorInteger (ColorString: Str80): INTEGER;
    FUNCTION  GetDateString: Str80;
    FUNCTION  GetDayString:  Str80;
    PROCEDURE GetFileNames (Path: Str80; Mask: Str80; VAR FileNames: FileNameRecord);
    FUNCTION  GetFileSize (FileName: Str80): LONGINT;
    FUNCTION  GetFirstString (LongString: STRING): Str80;
    FUNCTION  GetFullTimeString: Str80;
    FUNCTION  GetIntegerTime: INTEGER;

    FUNCTION  GetLastString (LongString: STRING): Str80;

    FUNCTION  GetLogEntryBand           (LogEntry: Str160): BandType;
    FUNCTION  GetLogEntryCall           (LogEntry: Str160): CallString;
    FUNCTION  GetLogEntryComputerID     (LogEntry: Str160): CHAR;
    FUNCTION  GetLogEntryDateString     (LogEntry: Str160): Str160;
    FUNCTION  GetLogEntryExchangeString (LogEntry: Str160): Str160;
    FUNCTION  GetLogEntryHour           (LogEntry: Str160): INTEGER;
    FUNCTION  GetLogEntryIntegerTime    (LogEntry: Str160): INTEGER;
    FUNCTION  GetLogEntryMode           (LogEntry: Str160): ModeType;
    FUNCTION  GetLogEntryMultString     (LogEntry: Str160): Str160;
    FUNCTION  GetLogEntryQSONumber      (LogEntry: Str160): INTEGER;
    FUNCTION  GetLogEntryQSOPoints      (LogEntry: Str160): INTEGER;
    FUNCTION  GetLogEntryRSTString      (LogEntry: Str160): Str160;
    FUNCTION  GetLogEntryTimeString     (LogEntry: Str160): Str160;

    FUNCTION  GetKey (Prompt: Str80): CHAR;
    FUNCTION  GetKeyResponse (Prompt: STRING): CHAR;
    FUNCTION  GetPrefix (Call: CallString): PrefixString;
    FUNCTION  GetOblast (Call: CallString): Str20;
    FUNCTION  GetReal (Prompt: Str80): REAL;
    FUNCTION  GetResponse (Prompt: Str80): Str80;
    PROCEDURE GetRidOfCarriageReturnLineFeeds (VAR S: STRING);
    PROCEDURE GetRidOfPostcedingSpaces (VAR S: STRING);
    PROCEDURE GetRidOfPrecedingSpaces (VAR S: STRING);
    FUNCTION  GetSCPCharFromInteger (Index: INTEGER): CHAR;
    FUNCTION  GetSCPIntegerFromChar (InputChar: CHAR): INTEGER;
    FUNCTION  GetStateFromSection (Section: Str20): Str20;
    FUNCTION  GetSuffix (Call: CallString): CallString;
    FUNCTION  GetTimeString: Str80;
    FUNCTION  GetTomorrowString: Str80;
    FUNCTION  GetValue (Prompt: Str80): LONGINT;
    FUNCTION  GetYearString: Str20;
    FUNCTION  GoodCallSyntax (Call: CallString): BOOLEAN;
    FUNCTION  GoodLookingGrid (Grid: Str20): BOOLEAN;

    PROCEDURE HexToInteger (InputString: Str80; VAR OutputInteger: INTEGER; VAR Result: INTEGER);
    PROCEDURE HexToLongInteger (InputString: Str80; VAR OutputInteger: LONGINT; VAR Result: INTEGER);
    PROCEDURE HexToWord    (InputString: Str80; VAR OutputWord:    WORD;    VAR Result: INTEGER);

    PROCEDURE IncrementASCIIInteger (VAR ASCIIString: Str80);
    PROCEDURE IncrementMinute (VAR DateString: Str20; VAR TimeString: Str80);
    FUNCTION  WordValueFromCharacter (Character: CHAR): WORD;


    FUNCTION  KeyId (Key: CHAR): Str80;

    FUNCTION  LastLetter (InputString: Str160): CHAR;
    FUNCTION  LastString (InputString: Str160): Str160;

    FUNCTION  LineInput (Prompt: Str160;
                         InitialString: Str160;
                         OverwriteEnable: BOOLEAN;
                         ExitOnAltKey: BOOLEAN): Str160;

    FUNCTION  LooksLikeAGrid (VAR GridString: Str20): BOOLEAN;
    FUNCTION  Lpt1BaseAddress: WORD;
    FUNCTION  Lpt2BaseAddress: WORD;
    FUNCTION  Lpt3BaseAddress: WORD;



    FUNCTION  MakeDupeFilename (Band: BandType; Mode: ModeType): Str80;
    FUNCTION  MakeTitle (Band: BandType; Mode: ModeType; Contest: Str80; CallUsed: Str80): Str80;
    PROCEDURE MarkTime (VAR StartTime: TimeRecord);
    FUNCTION  MicroTimeElapsed (StartTime: TimeRecord): LONGINT;
    FUNCTION  MinutesToTimeString (Minutes: INTEGER): Str20;
    FUNCTION  MultiMessageSourceBand (Source: BYTE): BandType;

    FUNCTION  NewKeyPressed: BOOLEAN;
    FUNCTION  NewReadKey: CHAR;

    PROCEDURE NoCursor;
    FUNCTION  NUMBYTES (Call1: Pointer; Call2: Pointer): INTEGER; CDECL;
    FUNCTION  NumberPartOfString (InputString: Str160): Str80;

    FUNCTION  OkayToDeleteExistingFile (FileName: Str80): BOOLEAN;
    FUNCTION  OkayToProceed: BOOLEAN;
    FUNCTION  OpenDupeFileForRead (VAR FileHandle: TEXT; Filename: Str80): BOOLEAN;
    FUNCTION  OpenFileForAppend   (VAR FileHandle: TEXT; FIlename: Str80): BOOLEAN;
    FUNCTION  OpenFileForRead     (VAR FileHandle: TEXT; Filename: Str80): BOOLEAN;
    FUNCTION  OpenFileForWrite    (VAR FileHandle: TEXT; Filename: Str80): BOOLEAN;
    FUNCTION  OperatorEscape: BOOLEAN;

    PROCEDURE PacketSendChar (SerialPort: serialportx; CharToSend: CHAR);
    FUNCTION  PartialCall (Pattern: Callstring; Call: CallString): BOOLEAN;
    FUNCTION  PortableStation (Call: CallString): BOOLEAN;
    FUNCTION  PostcedingString (LongString: STRING; Deliminator: STRING): STRING;
    FUNCTION  PrecedingString  (LongString: STRING; Deliminator: STRING): STRING;

    FUNCTION  RemoveBand             (VAR LongString: STRING): BandType;
    FUNCTION  RemoveFirstChar        (VAR LongString: STRING): CHAR;
    FUNCTION  RemoveFirstLongInteger (VAR LongString: STRING): LongInt;
    FUNCTION  RemoveFirstReal        (VAR LongString: STRING): REAL;
    FUNCTION  RemoveFirstString      (VAR LongString: STRING): Str80;
    FUNCTION  RemoveLastString       (VAR LongString: STRING): Str80;
    FUNCTION  RemoveMode             (VAR LongString: STRING): ModeType;

    FUNCTION  RootCall (Call: CallString): CallString;
    FUNCTION  RoverCall (Call: CallString): BOOLEAN;

    PROCEDURE SendChar (SerialPort: serialportx; CharToSend: CHAR);
    PROCEDURE SendString (SerialPort: serialportx; StringToSend: Str160);

    FUNCTION  SimilarCall (Call1: CallString; Call2: CallString): BOOLEAN;
    FUNCTION  SlipMessage (Message: STRING): STRING;
    FUNCTION  StandardCallFormat (Call: CallString; Complete: BOOLEAN): CallString;
    FUNCTION  StringHas (LongString: Str255; SearchString: Str80): BOOLEAN;
    FUNCTION  StringHasNumber (Prompt: Str80): BOOLEAN;
    FUNCTION  StringHasLowerCase (InputString: Str160): BOOLEAN;
    FUNCTION  StringIsAllNumbers (InputString: Str160): BOOLEAN;
    FUNCTION  StringIsAllNumbersOrSpaces (InputString: Str160): BOOLEAN;
    FUNCTION  StringIsAllNumbersOrDecimal (InputString: Str160): BOOLEAN;
    FUNCTION  StringHasLetters (InputString: Str160): BOOLEAN;

    FUNCTION  StringWithFirstWordDeleted (InputString:Str160): Str160;
    FUNCTION WordAfter(LongString: Str160; SearchString: Str80): Str160;

    FUNCTION  Tan(X: REAL): REAL;

    FUNCTION  UpperCase (Input: STRING): STRING;

    FUNCTION  ValidCallCharacter (CallChar: CHAR): BOOLEAN;
    FUNCTION  ValidRST (VAR Ex: Str80; VAR RST: RSTString; Mode: ModeType): BOOLEAN;

    FUNCTION  WhiteSpaceCharacter (InputChar: CHAR): BOOLEAN;

    PROCEDURE WriteColor (Prompt: Str80; FColor: INTEGER; BColor: INTEGER);

//    PROCEDURE setupkeyboard;cdecl;
//    FUNCTION ctrlshift:boolean;cdecl;
//    FUNCTION ctrl:boolean;cdecl;
//    FUNCTION ctrlenter:boolean;cdecl;
//    FUNCTION ritshift:integer;cdecl;



IMPLEMENTATION

USES DOS, trCrt, Printer, Ports,linuxsound,timer,unix,datetimec,
     keycode,xkb;

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


VAR Beat:            INTEGER;
    CWPitch:         INTEGER;
    DahLength:       INTEGER;
    DitLength:       INTEGER;
    ExtendedKey:     BYTE;
    ReadKeyAltState: BOOLEAN;

function paralleladdress(i: integer):integer;cdecl;external;

{$L dupe}



{ The first set of routines are local to TREE }

FUNCTION CharacterFromIntegerValue (IntegerValue: INTEGER): CHAR;

{ This table is used for compressing data. }

    BEGIN
    CASE IntegerValue OF
        0: CharacterFromIntegerValue := ' ';
        1: CharacterFromIntegerValue := '0';
        2: CharacterFromIntegerValue := '1';
        3: CharacterFromIntegerValue := '2';
        4: CharacterFromIntegerValue := '3';
        5: CharacterFromIntegerValue := '4';
        6: CharacterFromIntegerValue := '5';
        7: CharacterFromIntegerValue := '6';
        8: CharacterFromIntegerValue := '7';
        9: CharacterFromIntegerValue := '8';

        10: CharacterFromIntegerValue := '9';
        11: CharacterFromIntegerValue := 'A';
        12: CharacterFromIntegerValue := 'B';
        13: CharacterFromIntegerValue := 'C';
        14: CharacterFromIntegerValue := 'D';
        15: CharacterFromIntegerValue := 'E';
        16: CharacterFromIntegerValue := 'F';
        17: CharacterFromIntegerValue := 'G';
        18: CharacterFromIntegerValue := 'H';
        19: CharacterFromIntegerValue := 'I';
        20: CharacterFromIntegerValue := 'J';
        21: CharacterFromIntegerValue := 'K';
        22: CharacterFromIntegerValue := 'L';
        23: CharacterFromIntegerValue := 'M';
        24: CharacterFromIntegerValue := 'N';
        25: CharacterFromIntegerValue := 'O';
        26: CharacterFromIntegerValue := 'P';
        27: CharacterFromIntegerValue := 'Q';
        28: CharacterFromIntegerValue := 'R';
        29: CharacterFromIntegerValue := 'S';
        30: CharacterFromIntegerValue := 'T';
        31: CharacterFromIntegerValue := 'U';
        32: CharacterFromIntegerValue := 'V';
        33: CharacterFromIntegerValue := 'W';
        34: CharacterFromIntegerValue := 'X';
        35: CharacterFromIntegerValue := 'Y';
        36: CharacterFromIntegerValue := 'Z';
        ELSE CharacterFromIntegerValue := '?';
        END;
    END;



FUNCTION WordValueFromCharacter (Character: CHAR): WORD;

VAR TempInteger: WORD;

{ This table is used for compressing data. }

    BEGIN
    IF (Character = Chr (0)) OR (Character = ' ') OR
       (Character = '/')     OR (Character = '?') THEN
           BEGIN
           WordValueFromCharacter := 0;
           Exit;
           END;

    IF (Character >= 'A') AND (Character <= 'Z') THEN
        BEGIN
        WordValueFromCharacter := Ord (Character) - Ord ('A') + 11;
        Exit;
        END;

    IF (Character >= 'a') AND (Character <= 'z') THEN
        BEGIN
        WordValueFromCharacter := Ord (Character) - Ord ('a') + 11;
        Exit;
        END;

    IF (Character >= '0') AND (Character <= '9') THEN
        BEGIN
        WordValueFromCharacter := Ord (Character) - Ord ('0') + 1;
        Exit;
        END;

    IF Character = 'í' THEN
        BEGIN
        WordValueFromCharacter := 1;
        Exit;
        END;

    WordValueFromCharacter := 0;
    END;



PROCEDURE CompressThreeCharacters (Input: Str80; VAR Output: TwoBytes);

{ This procedure will compress a string of up to 3 characters to 2 bytes. }

VAR Multiplier, Value, Sum: WORD;
    LoopCount, CharPosition: INTEGER;

    BEGIN
    IF Input = '' THEN
        BEGIN
        Output [1] := 0;
        Output [2] := 0;
        Exit;
        END;

    Multiplier := 1;
    Sum := 0;
    LoopCount := 0;

    IF Length (Input) > 3 THEN
        BEGIN
        Output [1] := 0;
        Output [2] := 0;
        Exit;
        END;

    FOR CharPosition := Length (Input) DOWNTO 1 DO
        BEGIN
        Value := WordValueFromCharacter (Input [CharPosition]);
        Sum := Sum + Value * Multiplier;
        Inc (LoopCount);
        IF LoopCount >= 3 THEN Break;
        Multiplier := Multiplier * 37;
        END;

    Output [2] := Lo (Sum);
    Output [1] := Hi (Sum);
    END;


FUNCTION ExpandTwoBytes (Input: TwoBytes): Str80;

VAR Sum: LongInt;
    TempString: Str80;
    TempInt1, TempInt2: LongInt;

    BEGIN
    TempInt1 := Input [1];
    IF TempInt1 < 0 THEN TempInt1 := TempInt1 + 256;
    TempInt2 := Input [2];
    IF TempInt2 < 0 THEN TempInt2 := TempInt2 + 256;
    Sum := TempInt1 * 256 + TempInt2;

    IF Sum = 0 THEN
        BEGIN
        ExpandTwoBytes := '';
        Exit;
        END;

    TempString := CharacterFromIntegerValue (Sum DIV 1369);
    IF TempString [1] = Chr (0) THEN TempString := '';
    Sum := Sum MOD 1369;
    TempString := TempString + CharacterFromIntegerValue (Sum DIV 37);
    IF TempString [1] = Chr (0) THEN TempString := '';
    Sum := Sum MOD 37;
    TempString := TempString + CharacterFromIntegerValue (Sum);
    IF TempString [1] = Chr (0) THEN TempString := '';
    ExpandTwoBytes := TempString;
    END;



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

FUNCTION  BYTADDR  (Call: Pointer; NumberCalls: INTEGER; A: Pointer): INTEGER;
   CDECL;EXTERNAL;

FUNCTION  BYTDUPE  (Call: Pointer; NumberCalls: INTEGER; A: Pointer): BOOLEAN;
   CDECL;EXTERNAL;

FUNCTION  NUMBYTES (Call1: Pointer; Call2: Pointer): INTEGER;CDECL;EXTERNAL;

//    procedure setupkeyboard;cdecl;external;
//    function ctrlshift:boolean;cdecl;external;
//    function ctrl:boolean;cdecl;external;
//    function ctrlenter:boolean;cdecl;external;
//    function ritshift:integer;cdecl;external;

    procedure lsound(Hz: Integer);CDECL;external;
    procedure lnosound;CDECL;external;


FUNCTION AddBand (Band: BandType): CHAR;

VAR TempChar: CHAR;

    BEGIN
    Move (Band, TempChar, 1);
    AddBand := TempChar;
    END;

FUNCTION AddMode (Mode: ModeType): CHAR;

VAR TempChar: CHAR;

    BEGIN
    Move (Mode, TempChar, 1);
    AddMode := TempChar;
    END;


FUNCTION AlphaPartOfString (InputString: Str160): Str80;

VAR TempString: Str80;
    CharPointer: INTEGER;

    BEGIN
    IF InputString = '' THEN
        BEGIN
        AlphaPartOfString := '';
        Exit;
        END;

    TempString := '';

    FOR CharPointer := 1 TO Length (InputString) DO
        IF (InputString [CharPointer] >= 'A') AND (InputString [CharPointer] <= 'Z') THEN
            TempString := TempString + InputString [CharPointer];

    AlphaPartOfString := TempString;
    END;



FUNCTION BigCompressedCallsAreEqual (Call1, Call2: EightBytes): BOOLEAN;

    BEGIN
    BigCompressedCallsAreEqual := (Call1 [1] = Call2 [1]) AND
        (Call1 [2] = Call2 [2]) AND (Call1 [3] = Call2 [3]) AND
        (Call1 [4] = Call2 [4]) AND (Call1 [5] = Call2 [5]) AND
        (Call1 [6] = Call2 [6]) AND (Call1 [7] = Call2 [7]) AND
        (Call1 [8] = Call2 [8]);
    END;


PROCEDURE BigCompressFormat (Call: CallString; VAR CompressedBigCall: EightBytes);

VAR CompressedCall: FourBytes;
    Byte: INTEGER;
    ShortCall: Str20;

    BEGIN
    WHILE Length (Call) < 12 DO
        Call := ' ' + Call;

    ShortCall := Copy (Call, 1, 6);
    CompressFormat (ShortCall, CompressedCall);

    FOR Byte := 1 TO 4 DO
        CompressedBigCall [Byte] := CompressedCall [Byte];
    Delete (Call, 1, 6);

    CompressFormat (Call, CompressedCall);
    FOR Byte := 1 TO 4 DO
        CompressedBigCall [Byte + 4] := CompressedCall [Byte];
    END;


FUNCTION BigExpandedString (Input: EightBytes): Str80;

VAR TempBytes: TwoBytes;
    TempString: Str80;

    BEGIN
    TempBytes [1] := Input [1];
    TempBytes [2] := Input [2];
    TempString := ExpandTwoBytes (TempBytes);
    TempBytes [1] := Input [3];
    TempBytes [2] := Input [4];
    TempString := TempString + ExpandTwoBytes (TempBytes);
    TempBytes [1] := Input [5];
    TempBytes [2] := Input [6];
    TempString := TempString + ExpandTwoBytes (TempBytes);
    TempBytes [1] := Input [7];
    TempBytes [2] := Input [8];
    TempString := TempString + ExpandTwoBytes (TempBytes);
    WHILE (TempString [1] = ' ') AND (Length (TempString) > 1) DO
        Delete (TempString, 1, 1);
    BigExpandedString := TempString;
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



PROCEDURE CalculateBandMode (Freq: LONGINT; VAR Band: BandType; VAR Mode: ModeType);

    BEGIN
    IF (Freq >= 1790000) AND (Freq < 2000000) THEN
        BEGIN
        Band := Band160;    { Leave mode alone }
        Exit;
        END;

    IF (Freq >= 3490000) AND (Freq < 3530000) THEN
        BEGIN
        Band := Band80;
        Mode := CW;
        Exit;
        END;

    IF (Freq >= 3530000) AND (Freq < 3600000) THEN
        BEGIN
        Band := Band80;     { Leave mode alone }
        Exit;
        END;

    IF (Freq >= 3600000) AND (Freq < 4000000) THEN
        BEGIN
        Band := Band80;
        Mode := Phone;
        Exit;
        END;

    IF (Freq >= 6990000) AND (Freq < 7030000) THEN
        BEGIN
        Band := Band40;
        Mode := CW;
        Exit;
        END;

    IF (Freq >= 7030000) AND (Freq < 7100000) THEN
        BEGIN
        Band := Band40;     { Leave the mode alone }
        Exit;
        END;

    IF (Freq >= 7100000) AND (Freq < 7300000) THEN
        BEGIN
        Band := Band40;
        Mode := Phone;
        Exit;
        END;

    IF (Freq >= 10099000) AND (Freq < 10150000) THEN
        BEGIN
        Band := Band30;
        Mode := CW;
        Exit;
        END;

    IF (Freq >= 13990000) AND (Freq < 14100000) THEN
        BEGIN
        Band := Band20;
        Mode := CW;
        Exit;
        END;

    IF (Freq >= 14100000) AND (Freq < 14350000) THEN
        BEGIN
        Band := Band20;
        Mode := Phone;
        Exit;
        END;

    IF (Freq >= 18068000) AND (Freq < 18110000) THEN
        BEGIN
        Band := Band17;
        Mode := CW;
        Exit;
        END;

    IF (Freq >= 18110000) AND (Freq < 18168000) THEN
        BEGIN
        Band := Band17;
        Mode := Phone;
        Exit;
        END;

    IF (Freq >= 20990000) AND (Freq < 21000000) THEN
        BEGIN
        Band := Band15;
        Mode := CW;
        Exit;
        END;

    IF (Freq >= 21000000) AND (Freq < 21200000) THEN
        BEGIN
        Band := Band15;     { Leave mode alone }
        Exit;
        END;

    IF (Freq >= 21100000) AND (Freq < 21450000) THEN
        BEGIN
        Band := Band15;
        Mode := Phone;
        Exit;
        END;

    IF (Freq >= 24890000) AND (Freq < 24930000) THEN
        BEGIN
        Band := Band12;
        Mode := CW;
        Exit;
        END;

    IF (Freq >= 24930000) AND (Freq < 24990000) THEN
        BEGIN
        Band := Band12;
        Mode := Phone;
        Exit;
        END;

    IF (Freq >= 27990000) AND (Freq < 28300000) THEN
        BEGIN
        Band := Band10;
        Mode := CW;
        Exit;
        END;

    IF (Freq >= 28300000) AND (Freq < 29700000) THEN
        BEGIN
        Band := Band10;
        Mode := Phone;
        Exit;
        END;

    IF (Freq >= 50000000) AND (Freq < 50100000) THEN
        BEGIN
        Band := Band6;
        Mode := CW;
        Exit;
        END;

    IF (Freq >= 50100000) AND (Freq < 54000000) THEN
        BEGIN
        Band := Band6;
        Mode := Phone;
        Exit;
        END;

    IF (Freq >= 144000000) AND (Freq < 144100000) THEN
        BEGIN
        Band := Band2;
        Mode := CW;
        Exit;
        END;

    IF (Freq >= 144100000) AND (Freq < 148000000) THEN
        BEGIN
        Band := Band2;
        Mode := Phone;
        Exit;
        END;

    IF (Freq >= 218000000) AND (Freq < 250000000) THEN
        BEGIN
        Band := Band222;
        Mode := Phone;
        Exit;
        END;

    IF (Freq >= 400000000) AND (Freq <= 500000000) THEN
        BEGIN
        Band := Band432;
        Mode := Phone;
        Exit;
        END;

    IF (Freq >= 900000000) AND (Freq <= 1000000000) THEN
        BEGIN
        Band := Band902;
        Mode := Phone;
        Exit;
        END;

    IF (Freq > 1000000000) AND (Freq <= 1500000000) THEN
        BEGIN
        Band := Band1296;
        Mode := Phone;
        Exit;
        END;

    Band := NoBand;
    Mode := NoMode;
    END;



FUNCTION CallFitsFormat (Format: Str20; Call: Str20): BOOLEAN;

{ Format includes ? and * characters }

VAR LengthFormat, LengthCall, CallAddress, FormatAddress: INTEGER;

    BEGIN
    LengthFormat := Length (Format);
    LengthCall   := Length (Call);

    FormatAddress := 1;
    CallAddress   := 1;

    REPEAT

        { See if the charaters are the same }

        IF Format [FormatAddress] = Call [CallAddress] THEN
            BEGIN
            IF FormatAddress <= LengthFormat THEN Inc (FormatAddress);
            IF CallAddress   <= LengthCall   THEN Inc (CallAddress);
            Continue;
            END;

        { We have a mismatch.  If ? in format - count it like a match }

        IF Format [FormatAddress] = '?' THEN
            BEGIN
            IF FormatAddress <= LengthFormat THEN Inc (FormatAddress);

            IF CallAddress   <= LengthCall   THEN
                Inc (CallAddress)
            ELSE
                BEGIN  { We didn't have a character to match up with ? }
                CallFitsFormat := False;
                Exit;
                END;

            Continue;
            END;

        { Now for the hard one - we are going to assume that only one
          * can be contained in the format statement }

        IF Format [FormatAddress] = '*' THEN
            BEGIN
            IF FormatAddress = LengthFormat THEN  { We are done }
                BEGIN
                CallFitsFormat := True;
                Exit;
                END;

            { * found - not at end.  See if stuff after * matches }

            Format := Copy (Format,
                            FormatAddress + 1,
                            LengthFormat - FormatAddress);

            Call := Copy (Call, Length (Call) - Length (Format) + 1, Length (Format));

            IF Call = Format THEN
                BEGIN
                CallFitsFormat := True;
                Exit;
                END;

            CallFitsFormat := CallFitsFormat (Format, Call);
            Exit;
            END;

        { We have a real mismatch - so return a FALSE value }

        CallFitsFormat := False;
        Exit;

    UNTIL (FormatAddress = LengthFormat + 1) AND (CallAddress = LengthCall + 1);

    CallFitsFormat := True;
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

FUNCTION CheckSum (InputString: STRING): BYTE;

VAR Index, Sum: WORD;

    BEGIN
    Sum := 0;

    IF Length (InputString) > 0 THEN
        FOR Index := 1 TO Length (InputString) DO
            Sum := Sum + Ord (InputString [Index]);

    CheckSum := Lo (Sum);
    END;

FUNCTION CheckSumWord (InputString: STRING): WORD;

VAR Index, Sum: WORD;

    BEGIN
    Sum := 0;

    IF Length (InputString) > 0 THEN
        FOR Index := 1 TO Length (InputString) DO
            Sum := Sum + Ord (InputString [Index]);

    CheckSumWord := Sum;
    END;



PROCEDURE CompressFormat (Call: CallString; VAR Output: FourBytes);

{ This function will give the compressed representation for the string
    passed to it.  The string must be no longer than 6 characters.  }

VAR TempBytes: TwoBytes;

    BEGIN
    IF Call = '' THEN
        BEGIN
        Output [1] := 0;
        Output [2] := 0;
        Output [3] := 0;
        Output [4] := 0;
        Exit;
        END;

    Call := UpperCase (Call);

    WHILE Length (Call) < 6 DO
        Call := ' ' + Call;

    CompressThreeCharacters (Copy (Call, 1, 3), TempBytes);
    Output [1] := TempBytes [1];
    Output [2] := TempBytes [2];
    CompressThreeCharacters (Copy (Call, 4, 3), TempBytes);
    Output [3] := TempBytes [1];
    Output [4] := TempBytes [2];
    END;



FUNCTION CopyWord (LongString: STRING; Index: INTEGER): Str80;

    BEGIN
    CopyWord := '';

    IF Index = 0 THEN Exit;

    Delete (LongString, 1, Index - 1);

    CopyWord := GetFirstString (LongString);
    END;



PROCEDURE DecrementASCIIInteger (VAR ASCIIString: Str80);

VAR TempValue, Result: INTEGER;

    BEGIN
    Val (ASCIIString, TempValue, Result);
    IF Result <> 0 THEN
        BEGIN
        ASCIIString := '';
        Exit;
        END;
    Dec (TempValue);
    Str (TempValue, ASCIIString);
    END;


PROCEDURE DelayOrKeyPressed (DelayTime: INTEGER);

    BEGIN
    WHILE DelayTime > 0 DO
        BEGIN
        IF KeyPressed THEN Exit;
        Delay (1);
        Dec (DelayTime);
        END;
    END;



FUNCTION DeleteMult (VAR LogString: Str80; MultString: Str20): BOOLEAN;

VAR CharPointer, Position: INTEGER;
    TempString: Str20;

    BEGIN
    DeleteMult := False;
    TempString := Copy (LogString, LogEntryMultAddress, LogentryMultWidth);

    Position := Pos (MultString, TempString);

    IF (Position >= 1) AND
       (Position < LogEntryMultWidth) THEN
           BEGIN
           Position := Position + LogEntryMultAddress - 1;

           FOR CharPointer := Position TO Position + Length (MultString) - 1 DO
               LogString [CharPointer] := ' ';

           DeleteMult := True;
           Exit;
           END;
    END;



FUNCTION DupeTest (Call: Str80): BOOLEAN;

{ This function will check the string passed to it and return TRUE if
    the call starts with the letters DUPE.                                 }

    BEGIN
    Call := Copy (Call, 1, 4);
    DupeTest := Call = 'DUPE';
    END;



PROCEDURE MarkTime (VAR StartTime: TimeRecord);

    BEGIN
    WITH StartTime DO
        GetTime (Hour, Minute, Second, Sec100);
    END;




FUNCTION ElaspedTimeString (StartTime: TimeRecord): Str20;

{ Returns a string in the format HH:MM:SS with how long it has been }

VAR Hours, Mins, Secs, TotalSeconds: LONGINT;
    HourString, MinsString, SecsString: Str20;

    BEGIN
    TotalSeconds := ElaspedSec100 (StartTime) DIV 100;

    Hours := TotalSeconds DIV 3600;

    TotalSeconds := TotalSeconds - (Hours * 60);

    Mins := TotalSeconds DIV 60;

    TotalSeconds := TotalSeconds - (Mins * 60);

    Str (Hours,  HourString);
    Str (Mins,   MinsString);
    Str (TotalSeconds, SecsString);

    IF Length (SecsString) < 2 THEN SecsString := '0' + SecsString;
    IF Length (MinsString) < 2 THEN MinsString := '0' + MinsString;

    ElaspedTimeString := HourString + ':' + MinsString + ':' + SecsString;
    END;


FUNCTION ElaspedSec100 (StartTime: TimeRecord): LONGINT;

VAR Hour, Minute, Second, Sec100: WORD;
    TempMinute, TempSecond, TempSec100: LONGINT;

    BEGIN
    GetTime (Hour, Minute, Second, Sec100);

    IF StartTime.Hour > Hour THEN Hour := Hour + 24;

    IF StartTime.Hour > Hour THEN
        BEGIN
        ElaspedSec100 := 0;
        Exit;
        END;

    IF StartTime.Minute > Minute THEN
        BEGIN
        Minute := Minute + 60;
        Dec (Hour);

        IF StartTime.Hour > Hour THEN
            Hour := Hour + 24;
        END;

    IF StartTime.Second > Second THEN
        BEGIN
        Second := Second + 60;
        Dec (Minute);
        IF Minute < 0 THEN
            BEGIN
            Minute := Minute + 60;
            Dec (Hour);
            IF StartTime.Hour > Hour THEN
                Hour := Hour + 24;
            END;
        END;



    IF StartTime.Sec100 > Sec100 THEN
        BEGIN
        Sec100 := Sec100 + 100;
        Dec (Second);
        IF Second < 0 THEN
            BEGIN
            Second := Second + 60;
            Dec (Minute);
            IF Minute < 0 THEN
                BEGIN
                Minute := Minute + 60;
                Dec (Hour);
                IF Hour < 0 THEN
                    Hour := Hour + 24;
                END;
            END;
        END;

    IF Sec100 > StartTime.Sec100 THEN
        TempSec100 := Sec100 - StartTime.Sec100
    ELSE
        TempSec100 := 0;

    IF Minute > StartTime.Minute THEN
        TempMinute := Minute - StartTime.Minute
    ELSE
        TempMinute := 0;

    TempMinute := TempMinute * 6000;

    IF Second > StartTime.Second THEN
        TempSecond := Second - StartTime.Second
    ELSE
        TempSecond := 0;

    TempSecond := TempSecond * 100;

    ElaspedSec100 := TempMinute + TempSecond + TempSec100;
    END;



FUNCTION ExpandedString (Input: FourBytes): Str80;

{ Returns the expanded string for the compressed integer passed to it. }

VAR TempBytes: TwoBytes;
    TempString: Str80;

    BEGIN
    TempBytes [1] := Input [1];
    TempBytes [2] := Input [2];
    TempString := ExpandTwoBytes (TempBytes);
    TempBytes [1] := Input [3];
    TempBytes [2] := Input [4];
    TempString := TempString + ExpandTwoBytes (TempBytes);
    WHILE (TempString [1] = ' ') AND (Length (TempString) > 1) DO
        Delete (TempString, 1, 1);
    ExpandedString := TempString;
    END;


PROCEDURE ExpandTabs (VAR InputString: STRING);

VAR TabPos: INTEGER;

    BEGIN
    TabPos := Pos (TabKey, InputString);

    WHILE TabPos > 0 DO
        BEGIN
        Delete (InputString, TabPos, 1);
        Insert (' ', InputString, TabPos);
        Inc (TabPos);

        WHILE TabPos MOD 8 <> 0 DO
            BEGIN
            Insert (' ', InputString, TabPos);
            Inc (TabPos);
            END;

        TabPos := Pos (TabKey, InputString);
        END;
    END;



PROCEDURE BigCursor;

    BEGIN
    END;




FUNCTION SlipMessage (Message: STRING): STRING;

VAR CharPointer: INTEGER;
    TempString: STRING;

    BEGIN
    TempString := '';

    IF Message = '' THEN
        BEGIN
        SlipMessage := '';
        Exit;
        END;

    FOR CharPointer := 1 TO Length (Message) DO
        BEGIN
        IF Message [CharPointer] = Chr (FrameEnd) THEN
            TempString := Concat (TempString, Chr (FrameEscape), Chr (TransposedFrameEnd))
        ELSE
            IF Message [CharPointer] = Chr (FrameEscape) THEN
                TempString := Concat (TempString, Chr (FrameEscape), Chr (TransposedFrameEscape))
            ELSE
                TempString := TempString + Message [CharPointer];
        END;

    SlipMessage := Chr (FrameEnd) + TempString + Chr (FrameEnd);
    END;




FUNCTION FirstLetter (InputString: Str80): CHAR;

VAR TempString: Str20;

    BEGIN
    TempString := Copy (InputString, 1, 1);
    IF Length (TempString) > 0 THEN
        FirstLetter := TempString [1]
    ELSE
        FirstLetter := Chr (0);
    END;


FUNCTION LastLetter (InputString: Str160): CHAR;

VAR TempString: Str20;

    BEGIN
    TempString := Copy (InputString, Length (InputString), 1);
    IF Length (TempString) > 0 THEN
        LastLetter := TempString [1]
    ELSE
        LastLetter := Chr (0);
    END;


PROCEDURE FormFeed;

    BEGIN
    Write (Lst, Chr (12));
    END;


FUNCTION GetCheckSum8 (Call: FourBytes): INTEGER;

VAR Sum: INTEGER;

    BEGIN
    Sum := Call[1] + Call[2] + Call[3] + Call[4];
    Sum := Sum AND 7;
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



FUNCTION GetDateString: Str80;

{ This function goes off and reads the DOS clock and generates a nice
  looking ASCII string using the format 25-DEC-90.  It takes the Time
  Offset variable into account.

  If the HOUR OFFSET variable is non zero, the hours will get 24 added
  to it and then the HOUR OFFSET added.  This is done to keep the
  HOURS word variable from wrapping around to 65536 since there are no
  negative numbers for a WORD variable.  Thus a value greater than 47
  after adding the HOUR OFFSET indicates that a day must be added.  If
  the value is less than 24, then a day must be subtracted.
   }

VAR TempString, DString: Str80;

CONST
    DayTags: ARRAY [0..6] OF STRING [9] = ('Sunday', 'Monday', 'Tuesday',
          'Wednesday', 'Thursday', 'Friday', 'Saturday');

VAR Year, Month, Day, DayOfWeek: WORD;
    Hours, Minutes, Seconds, Hundredths: WORD;
    I: INTEGER;

    BEGIN
    GetDate (Year, Month, Day, DayOfWeek);

    IF HourOffset <> 0 THEN
        BEGIN
        GetTime (Hours, Minutes, Seconds, Hundredths);
        I := Hours;
        I := I + HourOffset;

        IF I > 23 THEN                     { Add a day }
            BEGIN
            Inc (Day);
            Inc (DayOfWeek);
            IF DayOfWeek = 8 THEN DayOfWeek := 1;

            CASE Month OF
                4, 6, 9, 11:                   { 30 day month }
                    IF Day > 30 THEN
                        BEGIN
                        Day := 1;
                        Inc (Month);
                        END;

                2: IF ((Year MOD 4) = 0) AND (Year <> 2000) THEN  { tricky }
                       BEGIN
                       IF Day > 29 THEN
                           BEGIN
                           Day := 1;
                           Inc (Month);
                           END;
                       END
                   ELSE
                       BEGIN
                       IF Day > 28 THEN
                           BEGIN
                           Day := 1;
                           Inc (Month);
                           END;
                       END;

                ELSE                           { 31 day month }
                    IF Day > 31 THEN
                        BEGIN
                        Day := 1;
                        Inc (Month);
                        IF Month = 13 THEN
                            BEGIN
                            Month := 1;
                            Inc (Year);
                            END;
                        END;
                END;

            END;

        IF I < 0 THEN                      { Subtract a day }
            BEGIN
            Dec (Day);
            Dec (DayOfWeek);
            IF DayOfWeek = 0 THEN DayOfWeek := 7;

            IF Day = 0 THEN
                BEGIN
                Dec (Month);

                CASE Month OF
                    4, 6, 9, 11: Day := 30;
                    2: IF ((Year MOD 4) = 0) AND (Year <> 2000) THEN  { tricky }
                           Day := 29
                       ELSE
                           Day := 28;
                    ELSE
                        BEGIN
                        IF Month = 12 THEN Dec (Year);
                        Day := 31;
                        END;
                    END;
                END;

            END;
        END;

    Str (Day, TempString);
    IF Day < 10 THEN TempString := '0' + TempString;
    DString := TempString + '-' + MonthTags [Month] + '-';
    Year := Year MOD 100;
    STR (Year, TempString);
    IF Year < 10 THEN TempString := '0' + TempString;
    GetDateString := DString + TempString;
    END;


FUNCTION GetDayString: Str80;

{ This function will look at the DOS clock and generate a nice looking
    ASCII string showing the name of the day of the week (ie: Monday).  }

CONST
    DayTags: ARRAY [0..6] OF STRING [9] = ('Sunday', 'Monday', 'Tuesday',
          'Wednesday', 'Thursday', 'Friday', 'Saturday');

VAR TempString: Str80;
    Year, Month, Day, DayOfWeek: Word;
    Hours, Minutes, Seconds, Hundredths: Word;
    I: Integer;

    BEGIN
    GetDate (Year, Month, Day, DayOfWeek);

    IF HourOffset <> 0 THEN
        BEGIN
        GetTime (Hours, Minutes, Seconds, Hundredths);

        I := Hours;
        I := I + HourOffset;

        IF I > 23 THEN                { Add a day }
            BEGIN
            Inc (DayOfWeek);
            IF DayOfWeek = 8 THEN DayOfWeek := 1;
            END
        ELSE
            IF I < 0 THEN
                BEGIN
                Dec (DayOfWeek);
                IF DayOfWeek = 0 THEN DayOfWeek := 7;
                END;

       END;

    GetDayString := DayTags [DayOfWeek];
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



FUNCTION GetFileSize (FileName: Str80): LONGINT;

VAR FileRead: FILE OF BYTE;

    BEGIN
    Assign(FileRead, FileName);
    Reset (FileRead);
    GetFileSize := FileSize (FileRead);
    Close (FileRead);
    END;



FUNCTION GetIntegerTime: INTEGER;

{ This function will return the present time in N6TR integer format. }

VAR Hours, Minutes, Seconds, Hundredths: Word;
    I: INTEGER;

    BEGIN
    GetTime (Hours, Minutes, Seconds, Hundredths);
    I := Hours;

    IF HourOffset <> 0 THEN
        BEGIN
        I := I + HourOffset;
        IF I > 23 THEN I := I - 24;
        IF I < 0  THEN I := I + 24;
        END;

    GetIntegerTime := I * 100 + Minutes;
    END;



FUNCTION GetLogEntryBand (LogEntry: Str160): BandType;

VAR TempString: Str80;

    BEGIN
    IF Length (LogEntry) < 4 THEN
        BEGIN
        GetLogEntryBand := NoBand;
        Exit;
        END;

    TempString := Copy (LogEntry, 1, 3);

    IF TempString = 'LGT' THEN BEGIN GetLogEntryBand := BandLight; Exit; END;

    IF TempString = '24G' THEN BEGIN GetLogEntryBand := Band24G;  Exit; END;
    IF TempString = '10G' THEN BEGIN GetLogEntryBand := Band10G;  Exit; END;
    IF TempString = '5GH' THEN BEGIN GetLogEntryBand := Band5760; Exit; END;
    IF TempString = '3GH' THEN BEGIN GetLogEntryBand := Band3456; Exit; END;
    IF TempString = '2GH' THEN BEGIN GetLogEntryBand := Band2304; Exit; END;
    IF TempString = '1GH' THEN BEGIN GetLogEntryBand := Band1296; Exit; END;
    IF TempString = '902' THEN BEGIN GetLogEntryBand := Band902;  Exit; END;
    IF TempString = '432' THEN BEGIN GetLogEntryBand := Band432;  Exit; END;
    IF TempString = '222' THEN BEGIN GetLogEntryBand := Band222;  Exit; END;

    IF TempString = '  2' THEN BEGIN GetLogEntryBand := Band2;   Exit; END;
    IF TempString = '  6' THEN BEGIN GetLogEntryBand := Band6;   Exit; END;
    IF TempString = ' 10' THEN BEGIN GetLogEntryBand := Band10;  Exit; END;
    IF TempString = ' 12' THEN BEGIN GetLogEntryBand := Band12;  Exit; END;
    IF TempString = ' 15' THEN BEGIN GetLogEntryBand := Band15;  Exit; END;
    IF TempString = ' 17' THEN BEGIN GetLogEntryBand := Band17;  Exit; END;
    IF TempString = ' 20' THEN BEGIN GetLogEntryBand := Band20;  Exit; END;
    IF TempString = ' 30' THEN BEGIN GetLogEntryBand := Band30;  Exit; END;
    IF TempString = ' 40' THEN BEGIN GetLogEntryBand := Band40;  Exit; END;
    IF TempString = ' 75' THEN BEGIN GetLogEntryBand := Band80;  Exit; END;
    IF TempString = ' 80' THEN BEGIN GetLogEntryBand := Band80;  Exit; END;
    IF TempString = '160' THEN BEGIN GetLogEntryBand := Band160; Exit; END;
    GetLogEntryBand := NoBand;
    END;


FUNCTION GetLogEntryCall (LogEntry: Str160): CallString;

VAR TempString: Str80;

    BEGIN
    TempString := Copy (LogEntry, LogEntryCallAddress, LogEntryCallWidth);
    GetRidOfPostcedingSpaces (TempString);
    GetLogEntryCall := TempString;
    END;


FUNCTION GetLogEntryComputerID (LogEntry: Str160): CHAR;

VAR TempString: Str20;

    BEGIN
    TempString := Copy (LogEntry, LogEntryComputerIDAddress + 1, LogEntryComputerIDWidth);

    GetRidOfPrecedingSpaces (TempString);

    IF TempString = '' THEN
        TempString := Copy (LogEntry, LogEntryComputerIDAddress, LogEntryComputerIDWidth);

    IF TempString = '' THEN
        GetLogEntryComputerID := Chr (0)
    ELSE
        GetLogEntryComputerID := TempString [1];
    END;


FUNCTION GetLogEntryDateString (LogEntry: Str160): Str160;

    BEGIN
    GetLogEntryDateString := Copy (LogEntry, LogEntryDayAddress, 9);
    END;


FUNCTION GetLogEntryExchangeString (LogEntry: Str160): Str160;

VAR TempString: Str80;

    BEGIN
    TempString := Copy (LogEntry, LogEntryExchangeAddress, LogEntryExchangeWidth);
    GetRidOfPostcedingSpaces (TempString);
    GetLogEntryExchangeString := TempString;
    END;



FUNCTION GetLogEntryHour (LogEntry: Str160): INTEGER;

VAR HourString: Str80;
    Hour, Result: INTEGER;

    BEGIN
    HourString := Copy (LogEntry, LogEntryHourAddress, LogEntryHourWidth);
    Val (HourString, Hour, Result);
    IF Result = 0 THEN GetLogEntryHour := Hour ELSE GetLogEntryHour := -1;
    END;



FUNCTION GetLogEntryMode (LogEntry: Str160): ModeType;

VAR TempString: Str80;

    BEGIN
    TempString := Copy (LogEntry, LogEntryModeAddress, LogEntryModeWidth);

    IF TempString = 'CW ' THEN
        GetLogEntryMode := CW
    ELSE
        IF TempString = 'DIG' THEN
            GetLogEntryMode := Digital
        ELSE
            IF (TempString = 'SSB') OR (TempString = 'FM ') THEN
                GetLogEntryMode := Phone
            ELSE
                GetLogEntryMode := NoMode;
    END;


FUNCTION GetLogEntryMultString (LogEntry: Str160): Str160;

VAR TempString: Str80;

    BEGIN
    TempString := Copy (LogEntry, LogEntryMultAddress, LogentryMultWidth);
    GetRidOfPostcedingSpaces (TempString);
    GetRidOfPrecedingSpaces (TempString);
    GetLogEntryMultString := TempString;
    END;


FUNCTION GetLogEntryQSONumber (LogEntry: Str160): INTEGER;

VAR TempString: Str20;
    QSONumber, Result: INTEGER;

    BEGIN
    TempString := Copy (LogEntry, LogEntryQSONumberAddress, LogEntryQSONumberWidth + 1);
    GetRidOfPrecedingSpaces (TempString);
    GetRidOfPrecedingSpaces (TempString);

    TempString := NumberPartOfString (TempString);

    Val (TempString, QSONumber, Result);
    IF Result = 0 THEN
        GetLogEntryQSONumber := QSONumber
    ELSE
        GetLogEntryQSONumber := -1;
    END;



FUNCTION GetLogEntryQSOPoints (LogEntry: Str160): INTEGER;

VAR TempString: Str80;
    Address, QSOPoints, Result: INTEGER;

    BEGIN
    TempString := Copy (LogEntry, LogEntryPointsAddress, LogEntryPointsWidth);

    Address := LogEntryPointsAddress + LogEntryPointsWidth;

    WHILE (Copy (LogEntry, Address, 1) >= '0') AND (Copy (LogEntry, Address, 1) <= '9') DO
        BEGIN
        TempString := TempString + LogEntry [Address];
        Inc (Address);
        END;

    GetRidOfPrecedingSpaces (TempString);

    IF NOT StringIsAllNumbers (TempString) THEN
        GetLogEntryQSOPoints := 0
    ELSE
        BEGIN
        VAL (TempString, QSOPoints, Result);
        GetLogEntryQSOPoints := QSOPoints;
        END;
    END;



FUNCTION GetLogEntryRSTString (LogEntry: Str160): Str160;

VAR TempString: Str80;

    BEGIN
    TempString := Copy (LogEntry, LogEntryExchangeAddress, 4);
    GetLogEntryRSTString := NumberPartOfString (TempString);
    END;


FUNCTION GetLogEntryIntegerTime (LogEntry: Str160): INTEGER;

VAR TempString: Str20;
    Time, Result: INTEGER;

    BEGIN
    TempString := Copy (LogEntry, LogEntryHourAddress, 5);
    Delete (TempString, 3, 1);

    Val (TempString, Time, Result);

    IF Result = 0 THEN
        GetLogEntryIntegerTime := Time
    ELSE
        GetLogEntryIntegerTime := -1;
    END;



FUNCTION GetLogEntryTimeString (LogEntry: Str160): Str160;

    BEGIN
    GetLogEntryTimeString := Copy (LogEntry, LogEntryHourAddress, 5);
    END;


FUNCTION GetKey (Prompt: Str80): CHAR;

VAR Key: CHAR;

    BEGIN
    GoToXY (1, WhereY);
    ClrEol;
    TextColor (Cyan);
    TextBackground (Black);
    Write (Prompt);
    REPEAT millisleep UNTIL KeyPressed;
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


FUNCTION GetPrefix (Call: CallString): PrefixString;

{ This function will return the prefix for the call passed to it. This is
    a new and improved version that will handle calls as they are usaully
    sent on the air.                                                          }

VAR CallPointer, PrefixPointer, Count: INTEGER;
    CallHasPortableSign: BOOLEAN;
    FirstPart, SecondPart, TempString: Str80;

    BEGIN
    FOR CallPointer := 1 TO Length (Call) DO
        IF Call [CallPointer] = '/' THEN
            BEGIN
            FirstPart := Call;
            FirstPart [0] := Chr (CallPointer - 1);
            SecondPart := '';

            FOR Count := CallPointer + 1 TO Length (Call) DO
                SecondPart := SecondPart + Call [Count];

            IF Length (SecondPart) = 1 THEN
                IF (SecondPart >= '0') AND (SecondPart <= '9') THEN
                    BEGIN
                    TempString := GetPrefix (FirstPart);
                    TempString [0] := Chr (Length (TempString) - 1);
                    GetPrefix := TempString + SecondPart;
                    Exit;
                    END
                ELSE
                    BEGIN
                    GetPrefix := GetPrefix (FirstPart);
                    Exit;
                    END;

            {KK1L: 6.68 Added AM check to allow /AM as aeronautical mobile rather than Spain}
            IF (Copy (SecondPart, 1, 2) = 'MM') OR (Copy (SecondPart, 1, 2) = 'AM') THEN
                BEGIN
                GetPrefix := GetPrefix (FirstPart);
                Exit;
                END;

            IF Length (FirstPart) > Length (SecondPart) THEN
                BEGIN
                GetPrefix := GetPrefix (SecondPart);
                Exit;
                END;

            IF Length (FirstPart) <= Length (SecondPart) THEN
                BEGIN
                GetPrefix := GetPrefix (FirstPart);
                Exit;
                END;
            END;

    { Call does not have portable sign.  Find natural prefix. }

    IF NOT StringHasNumber (Call) THEN
        BEGIN
        GetPrefix := Call + '0';
        Exit;
        END;

    FOR CallPointer := Length (Call) DOWNTO 2 DO
        IF Call [CallPointer] <= '9' THEN
            BEGIN
            GetPrefix := Call;
            GetPrefix [0] := CHR (CallPointer);
            Exit;
            END;

    IF (Call [1] <= '9') AND (Length (Call) = 2) THEN
        BEGIN
        GetPrefix := Call + '0';
        Exit;
        END;

    GetPrefix := '';  { We have no idea what the prefix is }
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
        REPEAT millisleep UNTIL KeyPressed;

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


PROCEDURE GetRidOfCarriageReturnLineFeeds (VAR S: STRING);

    BEGIN
    WHILE Pos (CarriageReturn, S) > 0 DO
        S [Pos (CarriageReturn, S)] := ' ';

    WHILE Pos (LineFeed, S) > 0 DO
        Delete (S, Pos (LineFeed, S), 1);
    END;

PROCEDURE GetRidOfPostcedingSpaces (VAR S: STRING);

    BEGIN
    WHILE Length (S) > 0 DO
        IF (S [Length (S)] = ' ') OR (S [Length (S)] = TabKey) THEN
            Delete (S, Length (S), 1)
        ELSE
            Exit;
    END;


PROCEDURE GetRidOfPrecedingSpaces (VAR S: STRING);

    BEGIN
    IF S = '' THEN Exit;
    WHILE ((S [1] = ' ') OR (S [1] = TabKey)) AND (Length (S) > 0) DO
        Delete (S, 1, 1);
    END;


FUNCTION GetSuffix (Call: CallString): CallString;

VAR TempString: Str20;
    CharPointer: INTEGER;

    BEGIN
    CharPointer := Length (Call);
    TempString := '';

    WHILE (Call [CharPointer] >= 'A') AND (CharPointer > 0) DO
        BEGIN
        TempString := Call [CharPointer] + TempString;
        Dec (CharPointer);
        END;

    GetSuffix := TempString;
    END;



FUNCTION GetFullTimeString: Str80;

{ This function will look at the DOS clock and generate a nice looking
  ASCII string showing the time using the format 23:42:32.  It will take
  the HourOffset variable into account. }

VAR Temp1, Temp2, Temp3: String[5];
    Hours, Minutes, Seconds, Hundredths: WORD;
    I: INTEGER;

    BEGIN
    GetTime (Hours, Minutes, Seconds, Hundredths);
    I := Hours;

    IF HourOffset <> 0 THEN
        BEGIN
        I := I + HourOffset;
        IF I > 23 THEN I := I - 24;
        IF I < 0  THEN I := I + 24;
        END;

    Str (I,       Temp1);
    Str (Minutes, Temp2);
    Str (Seconds, Temp3);

    IF Length (Temp1) < 2 THEN Temp1 := '0' + Temp1;
    IF Length (Temp2) < 2 THEN Temp2 := '0' + Temp2;
    IF Length (Temp3) < 2 THEN Temp3 := '0' + Temp3;

    GetFullTimeString := Temp1 + ':' + Temp2 + ':' + Temp3;
    END;



FUNCTION GetTimeString: Str80;

{ This function will look at the DOS clock and generate a nice looking
  ASCII string showing the time using the format 23:42.  It will take
  the HourOffset variable into account. }

VAR Temp1, Temp2: String[5];
    Hours, Minutes, Seconds, Hundredths: Word;
    I: INTEGER;

    BEGIN
    GetTime (Hours, Minutes, Seconds, Hundredths);
    I := Hours;

    IF HourOffset <> 0 THEN
        BEGIN
        I := I + HourOffset;
        IF I > 23 THEN I := I - 24;
        IF I < 0  THEN I := I + 24;
        END;

    Str (I,       Temp1);
    Str (Minutes, Temp2);

    IF Length (Temp1) < 2 THEN Temp1 := '0' + Temp1;
    IF Length (Temp2) < 2 THEN Temp2 := '0' + Temp2;
    GetTimeString := Temp1 + ':' + Temp2;
    END;



FUNCTION GetTomorrowString: Str80;

{ This function will look at the DOS clock and generate a nice looking
  ASCII string showing the name of tomorrow (ie: Monday).  }

CONST
    DayTags: ARRAY [0..6] OF STRING [9] = ('Sunday', 'Monday', 'Tuesday',
          'Wednesday', 'Thursday', 'Friday', 'Saturday');

VAR Year, Month, Day, DayOfWeek: Word;
    Hours, Minutes, Seconds, Hundredths: Word;
    I: INTEGER;

    BEGIN
    GetDate (Year, Month, Day, DayOfWeek);

    IF HourOffset <> 0 THEN
        BEGIN
        GetTime (Hours, Minutes, Seconds, Hundredths);
        I := Hours;

        I := I + HourOffset;

        IF Hours > 23 THEN                { Add a day }
            BEGIN
            Inc (DayOfWeek);
            IF DayOfWeek = 8 THEN DayOfWeek := 1;
            END
        ELSE
            IF Hours < 0 THEN             { Subtract a day }
                BEGIN
                Dec (DayOfWeek);
                IF DayOfWeek = 0 THEN DayOfWeek := 7;
                END;

       END;

    Inc (DayOfWeek);
    IF DayOfWeek = 8 THEN DayOfWeek := 1;

    GetTomorrowString := DayTags [DayOfWeek];
    END;



FUNCTION GetValue (Prompt: Str80): LONGINT;

{ This function will display the prompt passed to it and return a
  integer value input by the operator.  If the input is illegal, the
  prompt will be reprinted and a new value read.  }

VAR TempValue, Result: INTEGER;
    TempString: Str80;

    BEGIN
    REPEAT
        TextColor (Cyan);
        Write (Prompt);
        TextColor (Yellow);
        ReadLn (TempString);
        Val (TempString, TempValue, Result);
    UNTIL Result = 0;
    GetValue := TempValue;
    END;


FUNCTION GetReal (Prompt: Str80): REAL;

VAR TempValue: REAL;
    Result: INTEGER;
    TempString: Str80;

    BEGIN
    TempString := GetResponse (Prompt);
    Val (TempString, TempValue, Result);

    IF Result = 0 THEN
        GetReal := TempValue
    ELSE
        GetReal := 0;
    END;


FUNCTION GetYearString: Str20;

VAR TempString: Str80;
    Year, Month, Day, DayOfWeek: Word;

    BEGIN
    GetDate (Year, Month, Day, DayOfWeek);
    Str (Year, TempString);
    GetYearString := TempString;
    END;


FUNCTION GoodCallSyntax (Call: CallString): BOOLEAN;

{ This function will look at the callsign passed to it and see if it
    looks like a real callsign.                                           }

VAR CharacterPointer: INTEGER;

    BEGIN
    Call := UpperCase (Call);
    GoodCallSyntax := False;
    IF Length (Call) < 3 THEN Exit;

    IF NOT StringHasLetters (Call) THEN Exit;

    CASE Length (Call) OF
        8: IF ((Call [2] = '/') OR (Call [2] = '-')) AND
              ((Call [6] = '/') OR (Call [6] = '-')) THEN
                  Exit;

        9: IF ((Call [3] = '/') OR (Call [3] = '-')) AND
              ((Call [7] = '/') OR (Call [7] = '-')) THEN
                  Exit;
        END;

    FOR CharacterPointer := 1 TO Length (Call) DO
        IF NOT ValidCallCharacter (Call [CharacterPointer]) THEN Exit;

    FOR CharacterPointer := 1 TO Length (Call) DO
        IF Call [CharacterPointer] = '/' THEN
            BEGIN
            IF CharacterPointer = 1 THEN Exit;
            IF CharacterPointer = Length (Call) THEN Exit;
            GoodCallSyntax := True;
            Exit;
            END;

    IF (Call [1] <= '9') AND (Call [2] <= '9') THEN Exit;

    IF Length (Call) = 3 THEN
        BEGIN
        IF ((Call [2] < '0') OR (Call [2] > '9')) AND
           ((Call [3] < '0') OR (Call [3] > '9')) THEN
               Exit;
        END
    ELSE
        IF ((Call [2] < '0') OR (Call [2] > '9')) AND
           ((Call [3] < '0') OR (Call [3] > '9')) AND
           ((Call [4] < '0') OR (Call [4] > '9')) THEN
               Exit;

    GoodCallSyntax := True;
    END;



PROCEDURE HexToInteger (InputString: Str80; VAR OutputInteger: INTEGER; VAR Result: INTEGER);

VAR Multiplier: INTEGER;


    BEGIN
    Result := 1;
    Multiplier := 1;
    OutputInteger := 0;
    IF InputString = '' THEN Exit;

    Result := 0;

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
                 Result := 1;
                 Exit;
                 END;
            END;

        Delete (InputString, Length (InputString), 1);
        Multiplier := Multiplier * 16;
        END;

    Result := 0;
    END;



PROCEDURE HexToWord (InputString: Str80; VAR OutputWord: Word; VAR Result: INTEGER);

VAR Multiplier: Word;

    BEGIN
    Result := 1;
    Multiplier := 1;
    OutputWord := 0;
    IF InputString = '' THEN Exit;

    Result := 0;

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
                 Result := 1;
                 Exit;
                 END;
            END;

        Delete (InputString, Length (InputString), 1);
        Multiplier := Multiplier * 16;
        END;

    Result := 0;
    END;



PROCEDURE HexToLongInteger (InputString: Str80; VAR OutputInteger: LONGINT; VAR Result: INTEGER);

VAR Multiplier: LONGINT;

    BEGIN
    Result := 1;
    Multiplier := 1;
    OutputInteger := 0;
    IF InputString = '' THEN Exit;

    Result := 0;

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
                 Result := 1;
                 Exit;
                 END;
            END;

        Delete (InputString, Length (InputString), 1);
        Multiplier := Multiplier * 16;
        END;

    Result := 0;
    END;



PROCEDURE IncrementASCIIInteger (VAR ASCIIString: Str80);

VAR TempValue, Result: INTEGER;

    BEGIN
    Val (ASCIIString, TempValue, Result);
    IF Result <> 0 THEN
        BEGIN
        ASCIIString := '';
        Exit;
        END;
    Inc (TempValue);
    Str (TempValue, ASCIIString);
    END;

FUNCTION  KeyId (Key: CHAR): Str80;

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



FUNCTION ControlKeyPressed: BOOLEAN;

    BEGIN
    ControlKeyPressed := ctrl();
    END;



FUNCTION LastString (InputString: Str160): Str160;

VAR CharPointer: INTEGER;

    BEGIN
    LastString := '';
    IF InputString = '' THEN Exit;

    GetRidOfPostcedingSpaces (InputString);

    FOR CharPointer := Length (InputString) DOWNTO 1 DO
        IF (InputString [CharPointer] = ' ') OR
           (InputString [CharPointer] = ControlI) THEN
               BEGIN
               LastString := Copy (InputString, CharPointer + 1, Length (InputString) - CharPointer);
               Exit;
               END;

    LastString := InputString;
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
                REPEAT millisleep UNTIL KeyPressed;
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



FUNCTION LooksLikeAGrid (VAR GridString: Str20): BOOLEAN;

{ If it does look like a grid, it will make the first two letters lower
  case so it looks like a domestic QTH. }

VAR TestString: Str20;

    BEGIN
    TestString := UpperCase (GridString);

    LooksLikeAGrid := False;

    IF (Length (TestString) <> 4) AND (Length (TestString) <> 6) THEN Exit;

    IF (TestString [1] < 'A') OR (TestString [1] > 'R') THEN Exit;
    IF (TestString [2] < 'A') OR (TestString [2] > 'R') THEN Exit;
    IF (TestString [3] < '0') OR (TestString [3] > '9') THEN Exit;
    IF (TestString [4] < '0') OR (TestString [4] > '9') THEN Exit;

    IF GridString [1] > 'Z' THEN
        GridString [1] := Chr (Ord (GridString [1]) - Ord ('a') + Ord ('A'));

    IF GridString [2] > 'Z' THEN
        GridString [2] := Chr (Ord (GridString [2]) - Ord ('a') + Ord ('A'));

    LooksLikeAGrid := True;
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


FUNCTION MakeTitle (Band: BandType; Mode: ModeType; Contest: Str80; CallUsed: Str80): Str80;

    BEGIN
    MakeTitle := Contest + '   ' + CallUsed + '   ' + BandString [Band] + ' ' + ModeString [Mode];
    END;



FUNCTION MicroTimeElapsed (StartTime: TimeRecord): LONGINT;

{ Gives answer in Sec100s }

VAR ElaspedTime: LONGINT;
    Hour, Min, Sec, Sec100: WORD;

    BEGIN
    GetTime (Hour, Min, Sec, Sec100);

    ElaspedTime := 0;

    IF Sec <> StartTime.Second THEN
        BEGIN
        IF Sec < StartTime.Second THEN
            ElaspedTime := (Sec + 60) - StartTime.Second
        ELSE
            ElaspedTime := Sec - StartTime.Second;
        ElaspedTime := ElaspedTime * 100;
        END;

    IF Sec100 <> StartTime.Sec100 THEN
        BEGIN
        IF Sec100 < StartTime.Sec100 THEN
            BEGIN
            ElaspedTime := ElaspedTime + Sec100;
            ElaspedTime := ElaspedTime - StartTime.Sec100;
            END
        ELSE
            ElaspedTime := ElaspedTime + (Sec100 - StartTime.Sec100);
        END;

    MicroTimeElapsed := ElaspedTime;
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



FUNCTION MultiMessageSourceBand (Source: BYTE): BandType;


    BEGIN
    CASE Source OF
        $E0: MultiMessageSourceBand := Band160;
        $E1: MultiMessageSourceBand := Band80;
        $E2: MultiMessageSourceBand := Band40;
        $E3: MultiMessageSourceBand := Band20;
        $E4: MultiMessageSourceBand := Band15;
        $E5: MultiMessageSourceBand := Band10;

        $E6: MultiMessageSourceBand := Band30;
        $E7: MultiMessageSourceBand := Band17;
        $E8: MultiMessageSourceBand := Band12;
        $E9: MultiMessageSourceBand := Band6;
        $EA: MultiMessageSourceBand := Band2;
        $EB: MultiMessageSourceBand := Band222;
        $EC: MultiMessageSourceBand := Band432;

        $ED: MultiMessageSourceBand := Band902;
        $EE: MultiMessageSourceBand := Band1296;
        $EF: MultiMessageSourceBand := Band2304;
        $F0: MultiMessageSourceBand := Band3456;
        $F1: MultiMessageSourceBand := Band5760;
        $F2: MultiMessageSourceBand := Band10G;
        $F3: MultiMessageSourceBand := Band24G;
        $F4: MultiMessageSourceBand := BandLight;

        ELSE
            MultiMessageSourceBand := NoBand;
        END;
    END;



PROCEDURE NoCursor;

    BEGIN
    END;


FUNCTION NumberPartOfString (InputString: Str160): Str80;

VAR TempString: Str80;
    CharPointer: INTEGER;

    BEGIN
    IF InputString = '' THEN
        BEGIN
        NumberPartOfString := '';
        Exit;
        END;
    TempString := '';
    FOR CharPointer := 1 TO Length (InputString) DO
        IF (InputString [CharPointer] >= '0') AND (InputString [CharPointer] <= '9') THEN
          TempString := TempString + InputString [CharPointer];
    NumberPartOfString := TempString;
    END;


FUNCTION GetOblast (Call: CallString): Str20;

    BEGIN
    Call := StandardCallFormat (Call, False);

    IF StringHas (Call, '/') THEN Call := PrecedingString (Call, '/');

    WHILE Copy (Call, 1, 1) >= 'A' DO
        BEGIN
        Delete (Call, 1, 1);

        IF Call = '' THEN
            BEGIN
            GetOblast := '';
            Exit;
            END;
        END;

    GetOblast := Copy (Call, 1, 2);
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



FUNCTION OpenFileForAppend (VAR FileHandle: TEXT; Filename: Str80): BOOLEAN;

{ This function will open the filename indicated for append.  If the file
    does not exist, it is opened for write.  At this time, the function
    always returns TRUE (must of been lazy the day I wrote it).                 }

VAR DirInfo: SearchRec;

    BEGIN
    OpenFileForAppend := True;

    FindFirst (FileName, Archive, DirInfo);

    IF DosError = 0 THEN { FileExists }
        BEGIN
        Assign (FileHandle, Filename);
        Append (FileHandle);
        END
    ELSE
        BEGIN
        Assign (FileHandle, Filename);
        ReWrite (FileHandle);
        END;
    END;


FUNCTION OpenFileForRead (VAR FileHandle: TEXT; Filename: Str80): BOOLEAN;

    BEGIN
    Assign (FileHandle, Filename);
    {$I-}
    Reset (FileHandle);
    {$I+}
    OpenFileForRead := IoResult = 0;
    END;


FUNCTION OpenFileForWrite (VAR FileHandle: TEXT; Filename: Str80): BOOLEAN;

    BEGIN
    Assign (FileHandle, Filename);
    {$I-}
    ReWrite (FileHandle);
    {$I+}
    OpenFileForWrite := IoResult = 0;
    END;


FUNCTION OperatorEscape: BOOLEAN;

VAR Key: CHAR;

    BEGIN
    OperatorEscape := False;

    IF KeyPressed THEN
        BEGIN
        Key := ReadKey;
        IF Key = EscapeKey THEN
            BEGIN
            OperatorEscape := True;
            Exit;
            END
        ELSE
            IF Key = NullKey THEN
                Key := ReadKey;
        END;
    END;


FUNCTION PortableStation (Call: CallString): Boolean;

{ This function will return TRUE if the callsign passed to it is a portable
  station. }

VAR TempString: Str20;
    TempChar: Char;

    BEGIN
    PortableStation := False;
    TempString := PostcedingString (Call, '/');

    IF StringHas (TempString, '/') THEN
        TempString := PostcedingString (TempString, '/');

    IF Length (TempString) = 1 THEN
        BEGIN
        TempChar := TempString [1];
        IF ((TempChar >= '0') AND (TempChar <= '9')) OR (TempChar = 'P') OR (TempChar = 'M') THEN
            PortableStation := True;
        END;
    END;



FUNCTION PostcedingString (LongString: STRING; Deliminator: STRING): STRING;

VAR Position: INTEGER;

    BEGIN
    Position := Pos (Deliminator, LongString);

    IF Position > 0 THEN
        PostcedingString := Copy (LongString,
                                  Position + Length (Deliminator),
                                  Length (LongString) - Position - (Length (Deliminator) - 1))
    ELSE
        PostcedingString := '';
    END;


FUNCTION PrecedingString (LongString: STRING; Deliminator: STRING): STRING;

VAR Position: INTEGER;

    BEGIN
    Position := Pos (Deliminator, LongString);

    IF Position >= 2 THEN
        PrecedingString := Copy (LongString, 1, Position - 1)
    ELSE
        PrecedingString := '';
    END;



FUNCTION RemoveBand (VAR LongString: STRING): BandType;

VAR TempByte: BYTE;
    Band: BandType;

    BEGIN
    TempByte := Ord (LongString [1]);
    Move (TempByte, Band, 1);
    RemoveBand := Band;
    Delete (LongString, 1, 1);
    END;



FUNCTION RemoveMode (VAR LongString: STRING): ModeType;

VAR TempByte: BYTE;
    Mode: ModeType;

    BEGIN
    TempByte := Ord (LongString [1]);
    Move (TempByte, Mode, 1);
    RemoveMode:= Mode;
    Delete (LongString, 1, 1);
    END;



FUNCTION RemoveFirstChar (VAR LongString: STRING): CHAR;

    BEGIN
    WHILE (LongString <> '') AND ((Copy (LongString, 1, 1) = ' ') OR (Copy (LongString, 1, 1) = TabKey)) DO
        Delete (LongString, 1, 1);

    IF LongString = '' THEN
        BEGIN
        RemoveFirstChar := NullCharacter;
        Exit;
        END;

    RemoveFirstChar := LongString [1];
    Delete (LongString, 1, 1);

    WHILE (LongString <> '') AND ((Copy (LongString, 1, 1) = ' ') OR (Copy (LongString, 1, 1) = TabKey)) DO
        Delete (LongString, 1, 1);
    END;






FUNCTION RemoveFirstLongInteger (VAR LongString: STRING): LongInt;

VAR IntegerString: Str80;
    Number: LongInt;
    Result: INTEGER;

    BEGIN
    IntegerString := RemoveFirstString (LongString);
    Val (IntegerString, Number, Result);
    IF Result = 0 THEN
        RemoveFirstLongInteger := Number
    ELSE
        RemoveFirstLongInteger := 0;
    END;



FUNCTION RemoveFirstReal (VAR LongString: STRING): REAL;

VAR Result: INTEGER;
    TempString: Str80;
    TempReal: REAL;

    BEGIN
    TempString := RemoveFirstString (LongString);

    Val (TempString, TempReal, Result);

    IF Result = 0 THEN
        RemoveFirstReal := TempReal
    ELSE
        RemoveFirstReal := 0;
    END;



FUNCTION GetFirstString (LongString: STRING): Str80;

VAR CharCount: INTEGER;
    FirstWordFound: BOOLEAN;
    FirstWordCursor: INTEGER;

    BEGIN
    IF LongString = '' THEN
        BEGIN
        GetFirstString := '';
        Exit;
        END;

    FirstWordFound := False;

    FOR CharCount := 1 TO Length (LongString) DO
        IF FirstWordFound THEN
            BEGIN
            IF (LongString [CharCount] = ' ') OR (LongString [CharCount] = TabKey) THEN
                BEGIN
                GetFirstString := Copy (LongString, FirstWordCursor, CharCount - FirstWordCursor);
                Exit;
                END;
            END
        ELSE
            IF (LongString [CharCount] <> ' ') AND (LongString [CharCount] <> TabKey) THEN
                BEGIN
                FirstWordFound := True;
                FirstWordCursor := CharCount;
                END;

    IF FirstWordFound THEN
        GetFirstString := Copy (LongString, FirstWordCursor, Length (LongString) - FirstWordCursor + 1)
    ELSE
        GetFirstString := '';
    END;



FUNCTION RemoveFirstString (VAR LongString: STRING): Str80;

VAR CharCount: INTEGER;
    FirstWordFound: BOOLEAN;
    FirstWordCursor: INTEGER;

    BEGIN
    IF LongString = '' THEN
        BEGIN
        RemoveFirstString := '';
        Exit;
        END;

    FOR CharCount := 1 TO Length (LongString) DO
        IF (LongString [CharCount] = CarriageReturn) OR
           (LongString [CharCount] = LineFeed) THEN
               LongString [CharCount] := ' ';

    FirstWordFound := False;

    FOR CharCount := 1 TO Length (LongString) DO
        IF FirstWordFound THEN
            BEGIN
            IF (LongString [CharCount] = ' ') OR (LongString [CharCount] = TabKey) THEN
                BEGIN
                RemoveFirstString := Copy (LongString, FirstWordCursor, CharCount - FirstWordCursor);
                Delete (LongString, 1, CharCount);
                Exit;
                END;
            END
        ELSE
            IF (LongString [CharCount] <> ' ') AND (LongString [CharCount] <> TabKey) THEN
                BEGIN
                FirstWordFound := True;
                FirstWordCursor := CharCount;
                END;

    IF FirstWordFound THEN
        RemoveFirstString := Copy (LongString, FirstWordCursor, Length (LongString) - FirstWordCursor + 1)
    ELSE
        RemoveFirstString := '';

    LongString := '';
    END;



FUNCTION RemoveLastString (VAR LongString: STRING): Str80;

VAR CharPos: INTEGER;

    BEGIN
    IF Length (LongString) > 0 THEN
        BEGIN
        GetRidOfPostcedingSpaces (LongString);
        FOR CharPos := Length (LongString) DOWNTO 1 DO
            IF (LongString [CharPos] = ' ') OR (LongString [CharPos] = TabKey) THEN
                BEGIN
                RemoveLastString := Copy (LongString, CharPos + 1, Length (LongString) - CharPos);
                Delete (LongString, CharPos, Length (LongString) - CharPos + 1);
                Exit;
                END;

        RemoveLastString := LongString;
        LongString := '';
        END
    ELSE
        RemoveLastString := '';
    END;



FUNCTION GetLastString (LongString: STRING): Str80;

VAR CharPos: INTEGER;

    BEGIN
    IF Length (LongString) > 0 THEN
        BEGIN
        GetRidOfPostcedingSpaces (LongString);
        FOR CharPos := Length (LongString) DOWNTO 1 DO
            IF (LongString [CharPos] = ' ') OR (LongString [CharPos] = TabKey) THEN
                BEGIN
                GetLastString := Copy (LongString, CharPos + 1, Length (LongString) - CharPos);
                Exit;
                END;

        GetLastString := LongString;
        END
    ELSE
        GetLastString := '';
    END;



FUNCTION RootCall (Call: CallString): CallString;

VAR TempCall: CallString;

    BEGIN
    TempCall := StandardCallFormat (Call, True);

    IF StringHas (TempCall, '/') THEN
        TempCall := PostcedingString (TempCall, '/');

    IF Length (TempCall) <= 2 THEN
        BEGIN
        TempCall := PrecedingString (StandardCallFormat (Call, True), '/');

        IF Length (TempCall) >= 3 THEN
            BEGIN
            RootCall := TempCall;
            Exit;
            END;
        END;

    IF StringHas (TempCall, '/') THEN
        TempCall := PrecedingString (TempCall, '/');

{   IF Length (TempCall) > 6 THEN TempCall [0] := Chr (6); }
    RootCall := TempCall;
    END;



FUNCTION RoverCall (Call: CallString): BOOLEAN;

    BEGIN
    RoverCall := UpperCase (Copy (Call, Length (Call) - 1, 2)) = '/R';
    END;




PROCEDURE PacketSendChar (SerialPort: serialportx; CharToSend: CHAR);

VAR PortAddress: WORD;

    BEGIN
    PortAddress := 0;

    SerialPort.putchar(CharToSend);
    END;


PROCEDURE SendChar (SerialPort: serialportx; CharToSend: CHAR);


VAR PortAddress: WORD;

    BEGIN
    SerialPort.putchar(CharToSend);
    END;




PROCEDURE SendString (SerialPort: serialportx; StringToSend: Str160);

{ This procedure will sent a string to the serial port }

VAR StringPointer: INTEGER;

    BEGIN
    IF Length (StringToSend) > 0 THEN
        FOR StringPointer := 1 TO Length (StringToSend) DO
          SendChar (SerialPort, StringToSend [StringPointer]);
    END;




FUNCTION SimilarCall (Call1: CallString; Call2: CallString): BOOLEAN;

{ This function will return true if the two calls only differ in one
    character position.         }

VAR NumberDifferentChars, NumberTestChars, TestChar: INTEGER;
    C1, C2: STRING [1];

    BEGIN
    IF Pos ('/', Call1) > 0 THEN Call1 := RootCall (Call1);
    IF Pos ('/', Call2) > 0 THEN Call2 := RootCall (Call2);

    SimilarCall := False;

    IF Abs (Length (Call1) - Length (Call2)) > 1 THEN Exit;

    NumberTestChars := Length (Call1);
    IF (Length (Call2) > NumberTestChars) THEN Inc (NumberTestChars);

    { NumberTestChars is equal to length of longest call. }

    NumberDifferentChars := 0;

    FOR TestChar := NumberTestChars DOWNTO 1 DO
        BEGIN
        C1 := Copy (Call1, TestChar, 1);
        C2 := Copy (Call2, TestChar, 1);

        IF (C1 <> C2) AND (C1 <> '?') AND (C2 <> '?') THEN
            BEGIN
            Inc (NumberDifferentChars);
            IF (NumberDifferentChars) = 2 THEN Break;
            END;
        END;

    IF NumberDifferentChars <= 1 THEN
        BEGIN
        SimilarCall := True;
        Exit;
        END;

    { Let's see if either call shows up in the other - finds I4COM PI4COM }

    IF (Pos (Call1, Call2) = 0) AND (Pos (Call2, Call1) = 0) THEN Exit;
    SimilarCall := True;
    END;



FUNCTION StandardCallFormat (Call: CallString; Complete: BOOLEAN): CallString;

{ This fucntion will take the call passed to it and put it into a
  standard format with the country indicator as the first part of
  the call.  It is intended to convert calls as they would be sent
  on the air to N6TR duping service perferred format.  This means
  that a callsign as normally sent on the air would be converted to
  a callsign that can be passed to GetCountry, GetContinent, GetZone
  and so on with probable success.

  A change made on 4 November adds the complete flag.  If the flag is
  TRUE, the routine works the way it always has.  If the flag is false,
  the call is unchanged if the call has a single integer after the / sign.
  This is intended to eliminate problems with KC8UNP/6 showing up as
  KC6/KC8UNP which gets reported as the Carolines. }

VAR FirstPart, SecondPart, TempString: Str80;
    LastTwoLetters, LastThreeLetters, LastFourLetters: STRING [5];

    BEGIN
    IF NOT StringHas (Call, '/') THEN
        BEGIN
        StandardCallFormat := Call;
        Exit;
        END;

    LastTwoLetters   := Copy (Call, Length (Call) - 1, 2);
    LastThreeLetters := Copy (Call, Length (Call) - 2, 3);
    LastFourLetters  := Copy (Call, Length (Call) - 3, 4);

    IF (LastTwoLetters = '/P') OR (LastTwoLetters = '/M') OR
       (LastTwoLetters = '/N') OR (LastTwoLetters = '/T') THEN
           Delete (Call, Length (Call) - 1, 2);

    IF (LastThreeLetters = '/AG') OR (LastThreeLetters = '/AA') OR
       (LastThreeLetters = '/AE') THEN
           Delete (Call, Length (Call) - 2, 3);

    IF (LastFourLetters = '/QRP') THEN
           Delete (Call, Length (Call) - 3, 4);

    IF NOT StringHas (Call, '/') THEN
        BEGIN
        StandardCallFormat := Call;
        Exit;
        END;

    FirstPart  := PrecedingString  (Call, '/');
    SecondPart := PostcedingString (Call, '/');

    IF SecondPart = 'MOBILE' THEN  {KK1L: 6.71 Added per Tree request}
        BEGIN
        StandardCallFormat := FirstPart;
        Exit;
        END;

    IF SecondPart = 'MM' THEN
        BEGIN
        StandardCallFormat := 'MM/' + FirstPart;
        Exit;
        END;

    IF SecondPart = 'R' THEN
        BEGIN
        StandardCallFormat := FirstPart + '/' + SecondPart;
        Exit;
        END;

    IF SecondPart [1] = 'M' THEN
        IF Length (SecondPart) = 1 THEN
            BEGIN
            StandardCallFormat := FirstPart;
            Exit;
            END
        ELSE
            BEGIN
            Delete (SecondPart, 1, 1);
            StandardCallFormat := StandardCallFormat (FirstPart + '/' + SecondPart, Complete);
            Exit;
            END;



    IF Length (Call) = 11 THEN
        IF (Copy (Call, 1, 2) = 'VU') AND (Copy (Call, 7, 1) = '/') THEN
            IF (Copy (Call, 8, 1) >= '0') AND (Copy (Call, 8, 1) <= '9') THEN
                BEGIN
                StandardCallFormat := Call;
                Exit;
                END;

    IF Length (SecondPart) = 1 THEN
        IF (SecondPart >= '0') AND (SecondPart <= '9') THEN
          BEGIN
          IF Complete THEN
              BEGIN
              TempString := GetPrefix (FirstPart);
              Delete (TempString, Length (TempString), 1);
              SecondPart := TempString + SecondPart;
              StandardCallFormat := SecondPart + '/' + FirstPart;
              END
          ELSE
              StandardCallFormat := Call;
          Exit;
          END
        ELSE
          BEGIN
          CASE SecondPart [1] OF
                'F': StandardCallFormat := 'F/' + FirstPart;
                'G': StandardCallFormat := 'G/' + FirstPart;
                'I': StandardCallFormat := 'I/' + FirstPart;
                'K': StandardCallFormat := 'K/' + FirstPart;
                'N': StandardCallFormat := 'N/' + FirstPart;
                'W': StandardCallFormat := 'W/' + FirstPart;
                ELSE StandardCallFormat := FirstPart;
                END;
          Exit;
          END;

    IF Length (FirstPart) > Length (SecondPart) THEN
        BEGIN
        StandardCallFormat := SecondPart + '/' + FirstPart;
        Exit;
        END;

    IF Length (FirstPart) <  Length (SecondPart) THEN
        BEGIN
        StandardCallFormat := Call;
        Exit;
        END;

    IF Length (FirstPart) = Length (SecondPart) THEN
        BEGIN
        StandardCallFormat := Call;
        Exit;
        END;

    END;



FUNCTION WordAfter(LongString: Str160; SearchString: Str80): Str160;
VAR i: INTEGER;
    s: Str160;
BEGIN
   i := Pos(UpperCase(SearchString), UpperCase(LongString));
   if i = 0 then 
   begin
      wordafter := '';
      exit;
   end;
   s := copy(LongString,i+Length(SearchString),
      Length(LongString)-(i+Length(SearchString))+1);
   while (Length(s) > 0) and (s[1] = ' ') do delete(s,1,1);
   i := Pos(' ',s);
   if i = 0 then
      begin
         wordafter := s;
         exit;
      end
   else
      begin
         wordafter := copy(s,1,i);
      end;
END;

FUNCTION StringHas (LongString: Str255; SearchString: Str80): BOOLEAN;

{ This function will return TRUE if the SearchString is contained in the
    LongString.                                                                }

    BEGIN
    StringHas := Pos (SearchString, LongString) <> 0;
    END;



FUNCTION StringHasLetters (InputString: Str160): BOOLEAN;

VAR CharPos: INTEGER;

    BEGIN
    FOR CharPos := 1 TO Length (InputString) DO

    IF (UpCase (InputString [CharPos]) <= 'Z') AND (UpCase (InputString [CharPos]) >= 'A') THEN
        BEGIN
        StringHasLetters := True;
        Exit;
        END;

    StringHasLetters := False;
    END;


FUNCTION StringHasLowerCase (InputString: Str160): BOOLEAN;

VAR CharPos: INTEGER;

    BEGIN
    FOR CharPos := 1 TO Length (InputString) DO
        IF (InputString [CharPos] <= 'z') AND (InputString [CharPos] >= 'a') THEN
            BEGIN
            StringHasLowerCase := True;
            Exit;
            END;

    StringHasLowerCase := False;
    END;


FUNCTION StringHasNumber (Prompt: Str80): BOOLEAN;

VAR ChrPtr: INTEGER;

    BEGIN
    StringHasNumber := False;
    IF Length (Prompt) = 0 THEN Exit;

    FOR ChrPtr := 1 TO Length (Prompt) DO
        IF (Prompt [ChrPtr] >= '0') AND (Prompt [ChrPtr] <= '9') THEN
          BEGIN
          StringHasNumber := True;
          Exit;
          END;
    END;



FUNCTION StringIsAllNumbers (InputString: Str160): BOOLEAN;

VAR CharPos: INTEGER;

    BEGIN
    StringIsAllNumbers := False;
    IF InputString = '' THEN Exit;

    FOR CharPos := 1 TO Length (InputString) DO
        IF (InputString [CharPos] < '0') OR (InputString [CharPos] > '9') THEN
            Exit;

    StringIsAllNumbers := True;
    END;


FUNCTION StringIsAllNumbersOrSpaces (InputString: Str160): BOOLEAN;

VAR CharPos: INTEGER;

    BEGIN
    StringIsAllNumbersOrSpaces := False;
    IF InputString = '' THEN Exit;

    FOR CharPos := 1 TO Length (InputString) DO
        IF (InputString [CharPos] < '0') OR (InputString [CharPos] > '9') THEN
            IF InputString [CharPos] <> ' ' THEN Exit;

    StringIsAllNumbersOrSpaces := True;
    END;


FUNCTION StringIsAllNumbersOrDecimal (InputString: Str160): BOOLEAN;

VAR CharPos: INTEGER;

    BEGIN
    StringIsAllNumbersOrDecimal := False;
    IF InputString = '' THEN Exit;

    FOR CharPos := 1 TO Length (InputString) DO
        IF (InputString [CharPos] < '0') OR (InputString [CharPos] > '9') THEN
            IF InputString [CharPos] <> '.' THEN Exit;

    StringIsAllNumbersOrDecimal := True;
    END;




FUNCTION StringWithFirstWordDeleted (InputString:Str160): Str160;

{ This function performs a wordstar like control-T operation on the
    string passed to it.                                                   }

VAR DeletedChar: CHAR;

    BEGIN
    IF (InputString = '') OR (NOT StringHas (InputString, ' ')) THEN
        BEGIN
        StringWithFirstWordDeleted := '';
        Exit;
        END;

    REPEAT
        DeletedChar := InputString [1];
        Delete (InputString, 1, 1);

        IF Length (InputString) = 0 THEN
          BEGIN
          StringWithFirstWordDeleted := '';
          Exit;
          END;

    UNTIL (DeletedChar = ' ') AND (InputString [1] <> ' ');
    StringWithFirstWordDeleted := InputString;
    END;



FUNCTION UpperCase (Input: STRING): STRING;

    BEGIN
       UpperCase := UPCASE(Input);
    END;



FUNCTION ValidCallCharacter (CallChar: CHAR): BOOLEAN;

    BEGIN
    IF (CallChar='/') OR ((CallChar>='0') AND (CallChar<='9')) OR
       ((CallChar>='A') AND (CallChar<='Z')) THEN
           ValidCallCharacter := True
    ELSE
        ValidCallCharacter := False;
    END;


FUNCTION ValidRST (VAR Ex: Str80; VAR RST: RSTString; Mode: ModeType): BOOLEAN;

VAR  RSTString, DefaultRST: Str20;

    BEGIN
    ValidRST := False;

    IF (Mode = CW) OR (Mode = Digital) THEN
        DefaultRST := '599'
    ELSE
        DefaultRST := '59';

    IF Length (Ex) = 0 THEN
        BEGIN
        RST := DefaultRST;
        ValidRST := True;
        Exit;
        END;

    GetRidOfPrecedingSpaces (Ex);
    RSTString := '';

    WHILE (Copy (Ex, 1, 1) >= '0') AND (Copy (Ex, 1, 1) <='9') DO
        BEGIN
        RSTString := RSTString + Copy (Ex, 1, 1);
        Delete (Ex, 1, 1);
        END;

    CASE Length (RSTString) OF
        0: BEGIN
           RST := DefaultRST;
           ValidRST := True;
           END;

        1: BEGIN
           Delete (DefaultRST, 2, 1);
           Insert (RSTString, DefaultRST, 2);
           RST := DefaultRST;
           ValidRST := True;
           END;

        2: IF Mode = Phone THEN
               BEGIN
               RST := RSTString;
               ValidRST := (RST [1] >= '1') AND (RST [1] <= '5');
               END;

        3: IF (Mode = CW) OR (Mode = Digital) THEN
               BEGIN
               RST := RSTString;
               ValidRST := (RST [1] >= '1') AND (RST [1] <= '5');
               END;
        END;
    END;



PROCEDURE WriteColor (Prompt: Str80; FColor: INTEGER; BColor: INTEGER);

    BEGIN
    TextColor (FColor);
    TextBackground (BColor);
    Write (Prompt);
    END;


FUNCTION WhiteSpaceCharacter (InputChar: CHAR): BOOLEAN;

    BEGIN
    WhiteSpaceCharacter := (InputChar = ' ') OR (InputChar = TabKey);
    END;



PROCEDURE CharacterBuffer.InitializeBuffer;

   BEGIN
   Tail := 0;
   Head := 0;

   IF List = nil THEN New (List);
   END;


FUNCTION CharacterBuffer.FreeSpace: INTEGER;

   BEGIN
   IF Head < Tail THEN
       FreeSpace := Tail - Head - 1
   ELSE
       FreeSpace := BufferLength - (Head - Tail) - 1;
   END;



PROCEDURE CharacterBuffer.GoAway;

    BEGIN
    IF List <> nil then
        Dispose (List);

    List := nil;
    END;


FUNCTION CharacterBuffer.IsEmpty;

    BEGIN
    IsEmpty := (Head = Tail) OR (List = nil);
    END;


PROCEDURE CharacterBuffer.AddEntry (Entry: BYTE);

    BEGIN
    List^ [Head] := Entry;
    Head := (Head + 1) MOD BufferLength;

    IF Tail = Head THEN
        Tail := (Tail + 1) MOD BufferLength;
    END;


FUNCTION CharacterBuffer.GetNextByte (VAR Entry: BYTE): BOOLEAN;

{ Returns TRUE if a byte was there to get }

    BEGIN
    IF Tail <> Head THEN
        BEGIN
        Entry := List^ [Tail];
        Tail := (Tail + 1) MOD BufferLength;
        GetNextByte := True;
        END
    ELSE
        GetNextByte := False;
    END;



FUNCTION CharacterBuffer.GetNextLine (VAR Entry: STRING): BOOLEAN;

{ The Head is where new data would go (there isn't any there yet), and
  the Tail is the oldest byte in the buffer. }

VAR TestTail: INTEGER;

    BEGIN
    GetNextLine := False;

    IF Tail = Head THEN Exit;     { Nothing to look at }

    TestTail := Tail;

    { We move through the data, looking for a CarriageReturn }

    WHILE (List^ [TestTail] <> Ord (CarriageReturn)) AND
          (List^ [TestTail] <> Ord (LineFeed)) DO
              BEGIN
              TestTail := (TestTail + 1) MOD BufferLength;

              IF TestTail = Head THEN Exit;  { No CarriageReturn was found }
              END;

    { We have found a carriage return or line feed at TestTail.  We aren't
      going to copy the carriage return, so back up one count. }

    Dec (TestTail);

    { See if we went below zero }

    IF TestTail < 0 THEN TestTail := BufferLength - 1;

    { Copy the string }

    Entry := '';

    IF TestTail > Tail THEN   { Can use monotonic addressing }
        BEGIN
        IF (TestTail - Tail) + 1 <= 255 THEN
            BEGIN
            Entry [0] := Chr ((TestTail - Tail) + 1);  { String Length }
            Move (List^ [Tail], Entry [1], (TestTail - Tail) + 1);
            END;
        END
    ELSE
        IF (BufferLength - Tail) + TestTail + 1 < 255 THEN
            BEGIN
            Entry [0] := Chr ((BufferLength - Tail) + TestTail + 1);
            Move (List^ [Tail], Entry [1], BufferLength - Tail);
            Move (List^ [0],    Entry [BufferLength - Tail + 1], Length (Entry) - BufferLength + Tail);
            END;

    { Tell the guy calling me that we have something for him to look at }

    GetNextLine := True;

    Tail := TestTail;  { Point to the last piece of the data }

    { Added 12-Oct-2003 because this wasn't working a second time }

    Tail := (Tail + 1) MOD BufferLength;  { Point to the CR }

    { This will take care of jumping over CarriageReturn and any line feeds
      that might follow it. }

    WHILE ((List^ [Tail] = Ord (CarriageReturn)) OR (List^ [Tail] = Ord (LineFeed))) DO
        BEGIN
        Tail := (Tail + 1) MOD BufferLength;
        IF Tail = Head THEN Exit;
        END;
    END;



FUNCTION CharacterBuffer.GetSlippedString (VAR Entry: STRING): BOOLEAN;

{ Returns TRUE if a complete slipped string is found with it as parameter.
  It will always leave a FrameEnd at the tail. }

VAR LengthOfString, CharPointer, RemainingBytes, HeadAtStart, TestTail: INTEGER;

    BEGIN
    HeadAtStart := Head;

    GetSlippedString := False;

    { If the tail isn't pointing to a FrameEnd, delete characters until
      one it found, or there aren't any more characters }

    WHILE (List^ [Tail] <> FrameEnd) AND (Tail <> HeadAtStart) DO
        Tail := (Tail + 1) MOD BufferLength;

    IF Tail = HeadAtStart THEN Exit;

    { We have a FrameEnd at the tail.  Now look for consecutive ones to
      ignore. }

    TestTail := (Tail + 1) MOD BufferLength;

    IF TestTail = HeadAtStart THEN Exit;  { Not any characters after 1st FrameEnd }

    REPEAT
        IF List^ [TestTail] = FrameEnd THEN
            Tail := (Tail + 1) MOD BufferLength;  { Move real tail up one }

        TestTail := (Tail + 1) MOD BufferLength;

        IF TestTail = HeadAtStart THEN Exit;  { No chars after 1st FrameEnd }
    UNTIL List^ [TestTail] <> FrameEnd;

    { We have Tail = FrameEnd and Tail + 1 <> FrameEnd.  Now see if there
      is a frame end somewhere else - like at the end of a message? }

    REPEAT
        TestTail := (TestTail + 1) MOD BufferLength;
    UNTIL (List^ [TestTail] = FrameEnd) OR (TestTail = HeadAtStart);

    IF TestTail = HeadAtStart THEN Exit;  { Nothing yet }

    { There is another FrameEnd with data before it.  It is at TestTail. }
    { Increment the tail over the FrameEnd (which we want to ignore.}

    Tail := (Tail + 1) MOD BufferLength; { Skip over first FrameEnd }

    IF Tail = HeadAtStart THEN Exit;            { This would be weird }

    { Now copy the data into Entry }

    Entry := '';

    IF TestTail > Tail THEN   { Can use monotonic addressing  }
        BEGIN
        IF (TestTail - Tail) < 255 THEN   { Check for too long }
            BEGIN
            Entry [0] := Chr (TestTail - Tail);
            Move (List^ [Tail], Entry [1], TestTail - Tail);
            END;
        END
    ELSE
        BEGIN
        LengthOfString := BufferLength - Tail + TestTail;

        IF LengthOfString < 255 THEN
            BEGIN
            Entry [0] := Chr (LengthOfString);

            Move (List^ [Tail], Entry [1], BufferLength - Tail);

            RemainingBytes := LengthOfString - (BufferLength - Tail);

            { Runtime 201 error on next instruction  273b:94ca or 2736:949b
              or 2742:950a  }

            Move (List^ [0], Entry [BufferLength - Tail + 1], RemainingBytes);
            END;
        END;


{   Entry := '';

    WHILE Tail <> TestTail DO
        BEGIN
        IF Length (Entry) < SizeOf (Entry) - 2 THEN
            Entry := Entry + Chr (List^ [Tail]);
        Tail := (Tail + 1) MOD BufferLength;
        END;                                                   }

    Tail := TestTail;          { Missing from < 6.25 }

    { Now look for any Frame Escapes }

    FOR CharPointer := 1 TO Length (Entry) DO
        IF Entry [CharPointer] = FrameEscapeChr THEN
            BEGIN
            Delete (Entry, CharPointer, 1);

            IF Entry [CharPointer] = TransposedFrameEndChr THEN
                Entry [CharPointer] := FrameEndChr
            ELSE
                IF Entry [CharPointer] = TransposedFrameEscapeChr THEN
                    Entry [CharPointer] := FrameEscapeChr;
            END;

    GetSlippedString := True;
    END;



PROCEDURE IncrementMonth (VAR DateString: Str20);

{ This procedure will increment the month of the date string to the next
  month and set the day to 1.  If it is in December, the year will also
  be incremented. }

VAR MonthString, YearString: Str20;
    Year, Result: INTEGER;

    BEGIN
    MonthString := UpperCase (BracketedString (DateString, '-', '-'));

    YearString := Copy (DateString, Length (DateString) - 1, 2);
    Val (YearString, Year, Result);

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

VAR Day, Hour, Minute, Year, Result: INTEGER;
    MinuteString, HourString, DayString, MonthString, YearString: Str20;


    BEGIN
    Val (PostcedingString (TimeString, ':'), Minute, Result);
    Val (PrecedingString  (TimeString, ':'), Hour,   Result);

    Inc (Minute);

    IF Minute > 59 THEN
        BEGIN
        Minute := 0;
        Inc (Hour);

        IF Hour > 23 THEN
            BEGIN
            Hour := 0;

            Val (PrecedingString (DateString, '-'), Day, Result);
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
                    Val (YearString, Year, Result);

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



FUNCTION NewKeyPressed: BOOLEAN;

    BEGIN
    IF UseBIOSKeyCalls THEN
        BEGIN
        NewKeyPressed := KeyPressed;
        Exit;
        END;
{ KS
    NewKeyPressed := Mem [$40:$1A] <> Mem [$40:$1C];
}
    NewKeyPressed := False;
    END;


FUNCTION NewReadKey: CHAR;

VAR MemByte: BYTE;
    Address: BYTE;
    Key: CHAR;

    BEGIN
    IF UseBIOSKeyCalls THEN
        BEGIN
        Key := ReadKey;

        IF ReadKeyAltState THEN
            BEGIN
            NewReadKey := Key;
            ReadKeyAltState := False;
            Exit;
            END;

        IF Key = NullKey THEN
            BEGIN
            NewReadKey := Key;
            ReadKeyAltState := True;
            Exit;
            END;

        IF Key = QuestionMarkChar THEN
            NewReadKey := '?'
        ELSE
            IF Key = SlashMarkChar THEN
                NewReadKey := '/'
            ELSE
                NewReadKey := Key;

        Exit;
        END;

{ KS
    Address := Mem [$40:$1A];
}
    Address := 0;

    IF ExtendedKey <> 0 THEN
        BEGIN
        NewReadKey := Chr (ExtendedKey);
        Address := Address + 2;
        IF Address > $3C THEN Address := $1E;
{ KS
        Mem [$40:$1A] := Address;
}
        ExtendedKey := 0;
        Exit;
        END;

    REPEAT UNTIL NewKeyPressed;

{ KS
    MemByte := Mem [$40:Address];
}
    MemByte := 0;

    IF (MemByte = 0) OR (MemByte = $E0) THEN
        BEGIN
        NewReadKey := Chr (0);
{ KS
        ExtendedKey := Mem [$40:Address + 1];
}
        ExtendedKey := 0;
        Exit;
        END
    ELSE
        BEGIN
        IF Chr (MemByte) = QuestionMarkChar THEN
            NewReadKey := '?'
        ELSE
            IF Chr (MemByte) = SlashMarkChar THEN
                NewReadKey := '/'
            ELSE
                NewReadKey := Chr (MemByte);

        Address := Address + 2;
        IF Address > $3C THEN Address := $1E;
{ KS
        Mem [$40:$1A] := Address;
}
        END;
    END;




FUNCTION Tan (X: REAL): REAL;

    BEGIN
    IF Cos (x) = 0 THEN
        BEGIN
        Tan := 1000000000;
        Exit;
        END;

    Tan := Sin (x) / Cos(x);
    END;



FUNCTION ArcCos (X: REAL): REAL;

    BEGIN
    IF X = 1 THEN
        BEGIN
        ArcCos := 0;
        Exit;
        END
    ELSE
        IF X = -1 THEN
            BEGIN
            ArcCos := Pi / 2;
            Exit;
            END;

    ArcCos := (Pi / 2) - ArcTan (X / Sqrt (1- X*X));
    END;



FUNCTION ATan2 (Y, X : REAL) : REAL;

{ Returns ArcTan in the rnge 0.. 2Pi.  Input is X and Y points. }

    BEGIN
    If X < 0 THEN
        Atan2 := Pi + ArcTan (Y / X)    { Left two quadrants - sign works }
    ELSE
        IF X = 0 THEN                   { Either up or down the Y axis }
            BEGIN
            IF Y < 0 THEN               { Pointing down the Y axis }
                Atan2 := 1.5 * Pi
            ELSE
                IF Y = 0 THEN
                    Atan2 := 0.0        { Nicer than blowing up }
                ELSE
                    Atan2 := 0.5 * Pi;  { Pointing up the Y axis }
            END
        ELSE
            IF Y < 0 THEN
                Atan2 := 2.0 * Pi + ArcTan (Y / X) { Lower right Quadrant }
            ELSE
                Atan2 := ArcTan (Y / X);           { Upper right Quadrant }

    END; {Atan2}



FUNCTION GoodLookingGrid (Grid: Str20): BOOLEAN;

{ Verifies that the grid square is legitimate }

VAR CharPosition: INTEGER;

    BEGIN
    GoodLookingGrid := False;

    IF NOT ((Length (Grid) = 4) OR (Length (Grid) = 6)) THEN Exit;

    Grid := UpperCase (Grid);

    FOR CharPosition := 1 TO Length (Grid) DO
        CASE CharPosition OF
            1, 2:
                IF (Grid [CharPosition] < 'A') OR (Grid [CharPosition] > 'R') THEN
                    Exit;

            3, 4:
                IF (Grid [CharPosition] < '0') OR (Grid [CharPosition] > '9') THEN
                    Exit;

            5, 6:
                IF (Grid [CharPosition] < 'A') OR (Grid [CharPosition] > 'Z') THEN
                    Exit;

            END;

    GoodLookingGrid := True;
    END;



FUNCTION GetSCPIntegerFromChar (InputChar: CHAR): INTEGER;

    BEGIN
    GetSCPIntegerFromChar := -1;

    IF (InputChar >= 'A') AND (InputChar <= 'Z') THEN
        BEGIN
        GetSCPIntegerFromChar := Ord (InputChar) - Ord ('A');
        Exit;
        END;

    IF (InputChar >= '0') AND (InputChar <= '9') THEN
        BEGIN
        GetSCPIntegerFromChar := Ord (InputChar) - Ord ('0') + 26;
        Exit;
        END;

    IF InputChar = '/' THEN GetSCPIntegerFromChar := 36;
    END;



FUNCTION GetSCPCharFromInteger (Index: INTEGER): CHAR;

    BEGIN
    IF Index <= 25 THEN
        GetSCPCharFromInteger := Chr (Ord ('A') + Index)
    ELSE
        IF Index <= 35 THEN
            GetSCPCharFromInteger := Chr (Ord ('0') + (Index - 26))
        ELSE
            IF Index = 36 THEN
                GetSCPCharFromInteger := '/'
            ELSE
                GetSCPCharFromInteger := Chr (0);
    END;



FUNCTION PartialCall (Pattern: Callstring; Call: CallString): BOOLEAN;

LABEL NotAPartialCallHere;

VAR CharPos, ComparePos: INTEGER;

    BEGIN
    IF Pos (Pattern, Call) > 0 THEN
        BEGIN
        PartialCall := True;
        Exit;
        END;

    IF Pos ('?', Pattern) = 0 THEN
        BEGIN
        PartialCall := False;
        Exit;
        END;

    FOR CharPos := 1 TO Length (Call) - Length (Pattern) + 1 DO
        BEGIN
        IF (Call [CharPos] = Pattern [1]) OR (Pattern [1] = '?') THEN
            BEGIN
            FOR ComparePos := 2 TO Length (Pattern) DO
                IF (Call [CharPos + ComparePos - 1] <> Pattern [ComparePos]) AND (Pattern [ComparePos] <> '?') THEN
                    GoTo NotAPartialCallHere;

            { We have a match! }

            PartialCall := True;
            Exit;
            END;

    NotAPartialCallHere:

        END;

    PartialCall := False;
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


procedure getdegree(d: pchar);cdecl;external;

var ts:string;
    BEGIN
    setlength(DegreeSymbol,8);
    getdegree(@DegreeSymbol[1]);
    setlength(DegreeSymbol,strlen(pchar(@DegreeSymbol[0])));
    ExtendedKey := 0;
    Com5PortBaseAddress := 0;
    Com6PortBaseAddress := 0;

    HourOffset := 0;
    FMMode := False;

    QuestionMarkChar := '?';
    SlashMarkChar    := '/';
    UseBIOSKeyCalls  := True;

    ReadKeyAltState := False;
    END.


