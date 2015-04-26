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

UNIT LogSCP;

{$O+}
{$R+}
{$V-}

INTERFACE

USES Country9, LogSort, SlowTree, Tree, trCrt, DOS;

CONST
    ASCIIFileName        = 'TRMASTER.ASC';    { ASCII file used for making changes }
    BufferArraySize      = 65000;             { Largest cell that can be read in }
    TempFileName         = 'TEMPDTA.TMP';
    AVeryBigNumber       = 1000000000;
    MaxBlocks            = 10;
    MaximumPossibleCalls = 50;
    MemoryBlockSize      = 65000;
    MaximumCallsAlreadSaved = 3000;

TYPE EntryArrayType = ARRAY [0..300] OF CHAR;
     EntryArrayPtr  = ^EntryArrayType;

    CallsAlreadySavedArray = ARRAY [0..MaximumCallsAlreadSaved - 1] OF CallString;
    CallsAlreadySavedArrayPtr = ^CallsAlreadySavedArray;

    PossibleCallActionType = (AnyCall, OnlyCallsWithNames, LogOnly); {KK1L: 6.69 added LogOnly}

    PossibleCallEntry = RECORD
        Call: CallString;
        Dupe: BOOLEAN;
        END;

    PossibleCallRecord = RECORD
        NumberPossibleCalls: INTEGER;
        List: ARRAY [0..MaximumPossibleCalls - 1] OF PossibleCallEntry;
        CursorPosition: INTEGER;
        END;

    DataBaseEntryRecord = RECORD
        Call:    CallString;      { Always first }
        Section: STRING [5];      { Control-A }
        CQZone:  STRING [2];      { Control-C }
        FOC:     STRING [5];      { Control-F }
        Grid:    STRING [6];      { Control-G }
        Hits:    BYTE;            { Control-H }
        ITUZone: STRING [5];      { Control-I }
        Check:   STRING [2];      { Control-K }
        Name:    CallString;      { Control-N }
        OldCall: CallString;      { Control-O }
        QTH:     STRING [10];     { Control-Q }
        Speed:   BYTE;            { Control-S }
        TenTen:  STRING [6];      { Control-T }
        User1:   CallString;      { Control-U }
        User2:   CallString;      { Control-V }
        User3:   CallString;      { Control-W }
        User4:   CallString;      { Control-X }
        User5:   CallString;      { Control-Y }
        END;

    DataListEntryPointer = ^DataListEntryRecord;

    DataListEntryRecord = RECORD
        Data: DataBaseEntryRecord;
        NextEntry: DataListEntryPointer;
        END;

    MemoryBlock = ARRAY [0..MemoryBlockSize - 1] OF BYTE;
    MemoryBlockPtr = ^MemoryBlock;

    BufferArray = ARRAY [0..BufferArraySize - 1] OF CHAR;
    BufferPtr   = ^BufferArray;

    CellBufferObject = OBJECT
        MaximumMemoryToUse: LONGINT;   { TR should set this to a low # }
        ReadAddress: LONGINT;
        MemoryAllocated: LONGINT;
        Key: Str20;

        NumberBufferEntries: LONGINT;

        Buffer1Bytes: LONGINT;
        Buffer2Bytes: LONGINT;
        Buffer3Bytes: LONGINT;

        Buffer1: BufferPtr;
        Buffer2: BufferPtr;
        Buffer3: BufferPtr;

        Buffer1Used: BOOLEAN;
        Buffer2Used: BOOLEAN;
        Buffer3Used: BOOLEAN;

        PROCEDURE FigureOutBufferSizes (NumberBytes: LONGINT);
        FUNCTION  GetNextEntry (VAR EntryString: STRING): BOOLEAN;
        FUNCTION  GetNextEntryAddress (VAR EntryAddress: POINTER): BOOLEAN;
        PROCEDURE Initialize (VAR NumberBytes: LONGINT);
        PROCEDURE GoAway;
        PROCEDURE LoadCellIntoBuffer (KeyString: CallString; VAR FileRead: FILE; NumberBytes: LONGINT);
        END;

    PartialCallListEntryPtr = ^PartialCallListEntryType;

    PartialCallListEntryType = RECORD
        Call:      CallString;
        NextEntry: PartialCallListEntryPtr;
        END;

    SCPIndexArrayType    = ARRAY [0..36, 0..36] OF LONGINT;
    SCPIndexArrayPtr     = ^SCPIndexArrayType;
    BytesWrittenArrayPtr = ^SCPIndexArrayType;

    CallDatabase = OBJECT
        ActiveASCIIFileName: Str80;      { For ASCII file }
        ActiveFilename:      Str80;      { Database filename }

        ASCIIFileCrunchCallToRemove: CallString;
        ASCIIFileCrunchRead:  TEXT;
        ASCIIFileCrunchWrite: TEXT;
        ASCIIFileIsCurrent:   BOOLEAN;   { Handy if successive AddEntries }

        CellBuffer: CellBufferObject;    { The big and great File Buffer }
        CountryString: Str80;

        DTAFileSize: LONGINT;

        DeleteCallList: ARRAY [0..19] OF CallString;
        NumberDeleteCalls: INTEGER;

        { Used by TransferMergeDataToASCIIFile to delete some calls }

        EntryPointerList: EntryPointerListPtr;

        FirstMergeDataListEntry: DataListEntryPointer;

        IndexArrayAllocated: BOOLEAN;

        InitialPartialCall: CallString;
        InitialPartialList: PartialCallListEntryPtr;

        LastCallRecord: DataBaseEntryRecord;
        LastPartialCall: CallString;
        LastPartialList: PartialCallListEntryPtr;

        MemoryBlocks: ARRAY [0..MaxBlocks - 1] OF MemoryBlockPtr;
        NumberEntries: LONGINT;

        SectionOverwrite:  BOOLEAN;
        CQZoneOverwrite:   BOOLEAN;
        FOCOverwrite:      BOOLEAN;
        GridOverwrite:     BOOLEAN;
        HitsOverwrite:     BOOLEAN;
        ITUZoneOverwrite:  BOOLEAN;
        CheckOverwrite:    BOOLEAN;
        NameOverwrite:     BOOLEAN;
        OldCallOverwrite:  BOOLEAN;
        QTHOverwrite:      BOOLEAN;
        SpeedOverwrite:    BOOLEAN;
        TenTenOverwrite:   BOOLEAN;
        User1Overwrite:    BOOLEAN;
        User2Overwrite:    BOOLEAN;
        User3Overwrite:    BOOLEAN;
        User4Overwrite:    BOOLEAN;
        User5Overwrite:    BOOLEAN;

        PossibleCallAction: PossibleCallActionType;

        SCPDisabledByApplication: BOOLEAN;  { If TRUE - memory deallocated }

        SCPIndexArray:   SCPIndexArrayPtr;  { Table of cell addresses }
        SCPEndOfFile:    LONGINT;           { Address for end of last entry }
        WorkingDirectory: Str40;

        TRMasterFileRead: FILE;
        TRMasterFileOpen: BOOLEAN;

        PROCEDURE AddEntry (NewData: DataBaseEntryRecord;
                            BuildNewFile: BOOLEAN);

        PROCEDURE AddInBytes (NumberBytes, X, Y: INTEGER);
        PROCEDURE AddRecordToMergeList (Data: DatabaseEntryRecord);
        FUNCTION  ASCIIFileCrunch: BOOLEAN;
        PROCEDURE ASCIIFileEditor;
        FUNCTION  BestTwoLetters (Partial: CallString): Str20;
        PROCEDURE BlowAwayFirstLettersList;
        FUNCTION  BuildNewDatabaseFromASCIIFile (Ignore: CHAR): BOOLEAN;
        PROCEDURE CheckDTAFile;
        PROCEDURE ClearDataEntry (VAR Data: DatabaseEntryRecord);
        PROCEDURE ClearField;
        PROCEDURE DeleteLowHitCalls;

        FUNCTION  FirstCellForThisCall (Call: CallString; X, Y: INTEGER): BOOLEAN;

        PROCEDURE GeneratePossibleCallList (Call: CallString);

        PROCEDURE GetBestOffsets (Call: CallString;
                                  VAR StartingOffset, EndingOffset: LONGINT;
                                  VAR X, Y: INTEGER);

        FUNCTION  GetEntry (Call: CallString; VAR Data: DataBaseEntryRecord): BOOLEAN;
        FUNCTION  GetCodeSpeed (Call: CallString): INTEGER;
        PROCEDURE GetDataFromASCIIEntry (FileString: STRING; VAR Data: DataBaseEntryRecord; Ignore: CHAR);
        FUNCTION  GetFOCNumber (Call: CallString): CallString;
        FUNCTION  GetName (Call: CallString): CallString;
        FUNCTION  GetNextPartialCall: CallString;
        FUNCTION  GetRandomCall: CallString;  { Use this one }

        FUNCTION  GoodCountry (Call: CallString): BOOLEAN;
        FUNCTION  RandomCall: CallString;     { Might get FOC number calls }


        FUNCTION  LoadInIndexArray: BOOLEAN;

        PROCEDURE MergeArrays (VAR FirstArray: EntryArrayPtr;
                               VAR SecondArray: EntryArrayPtr;
                               VAR OutputArray: EntryArrayPtr);

        PROCEDURE MovePossibleCallsFromBufferIntoCallList (Call: CallString);
        PROCEDURE NewTwoLetters (PartialCall: Str20);
        FUNCTION  NumberOfBytesAtThisAddress (TwoLetters: Str20): LONGINT;
        FUNCTION  OverwriteFlagStatus: BOOLEAN;
        PROCEDURE ParseEntryToDataRecord (EntryString: STRING;
                                          VAR DataRecord: DatabaseEntryRecord);

        PROCEDURE PushChar (InChar: CHAR; VAR BlockNumber: WORD; VAR BlockAddress: WORD);

        FUNCTION  PartialCallSetup (PartialCall: CallString): BOOLEAN;

        PROCEDURE SaveCallsAndNamesToFile (FileName: Str40);
        PROCEDURE SaveDataToASCIIFile (VAR FileWrite: TEXT; NewData: DataBaseEntryRecord);
        PROCEDURE SaveToASCIIFile;

        PROCEDURE SCPDisableAndDeAllocateFileBuffer;

        PROCEDURE SortDTAFile;
        PROCEDURE ShowStatistics;
        PROCEDURE TransferMergeDataToASCIIFile (CallUpdate: BOOLEAN; SaveNewCalls: BOOLEAN);
        PROCEDURE TransferNewData (OldData: DataBaseEntryRecord; VAR NewData: DataBaseEntryRecord);
        END;

VAR CD: CallDatabase;

    CallsAlreadySaved: CallsAlreadySavedArrayPtr;

    NumberCallsAlreadySaved: INTEGER;

    PossibleCallList: PossibleCallRecord;

    FUNCTION  GetRandomLetter: CHAR;
    FUNCTION  GetRandomNumber: CHAR;



IMPLEMENTATION

uses memlinux,keycode,sysutils,timer;

FUNCTION DoubleIndexCall (Call: CallString): BOOLEAN;

    BEGIN
    WHILE Length (Call) >= 3 DO
        BEGIN
        IF Pos (Copy (Call, 1, 2), Copy (Call, 2, Length (Call) - 1)) > 0 THEN
            BEGIN
            DoubleIndexCall := True;
            Exit;
            END;

        Delete (Call, 1, 1);
        END;

    DoubleIndexCall := False;
    END;



PROCEDURE CellBufferObject.Initialize (VAR NumberBytes: LONGINT);

    BEGIN
    IF MaximumMemoryToUse = 3 * BufferArraySize THEN  { POST mode }
        BEGIN
        IF MemoryAllocated = 0 THEN
            BEGIN
            IF MaxAvail > BufferArraySize THEN
                BEGIN
                Buffer1Used := True;
                New (Buffer1);
                END;

            IF MaxAvail > BufferArraySize THEN
                BEGIN
                Buffer2Used := True;
                New (Buffer2);
                END;

            IF MaxAvail > BufferArraySize THEN
                BEGIN
                Buffer3Used := True;
                New (Buffer3);
                END;
            END;

        MemoryAllocated := 3 * BufferArraySize;
        END

    ELSE               { TR has set the max memory to something else }
        BEGIN
        IF MemoryAllocated > 0 THEN
            FreeMem (Buffer1, MemoryAllocated);

        IF (NumberBytes > MaximumMemoryToUse) OR
           (NumberBytes < 0) THEN
                NumberBytes := MaximumMemoryToUse;

        { Runtime error 201 here with K5KG - 0D73:0139 }

        IF NumberBytes < MaxAvail THEN
            BEGIN
            GetMem (Buffer1, NumberBytes);
            MemoryAllocated := NumberBytes;
            END
        ELSE
            MemoryAllocated := 0;
        END;

    Key := '';
    NumberBufferEntries := 0;
    Buffer1Bytes := 0;
    Buffer2Bytes := 0;
    Buffer3Bytes := 0;
    END;


PROCEDURE CellBufferObject.GoAway;

    BEGIN
    IF MaximumMemoryToUse = 3 * BufferArraySize THEN { Post mode }
        BEGIN
        IF MemoryAllocated > 0 THEN
            BEGIN
            IF Buffer1Used THEN Dispose (Buffer1);
            IF Buffer2Used THEN Dispose (Buffer2);
            IF Buffer3Used THEN Dispose (Buffer3);

            Buffer1Used := False;
            Buffer2Used := False;
            Buffer3Used := False;
            END;
        END
    ELSE
        IF MemoryAllocated > 0 THEN
            BEGIN
            FreeMem (Buffer1, MemoryAllocated);
            END;

    MemoryAllocated := 0;
    Key := '';
    NumberBufferEntries := 0;
    Buffer1Bytes := 0;
    Buffer2Bytes := 0;
    Buffer3Bytes := 0;
    END;

PROCEDURE CellBufferObject.FigureOutBufferSizes (NumberBytes: LONGINT);

    BEGIN
    Buffer2Bytes := 0;
    Buffer3Bytes := 0;

    IF NumberBytes > BufferArraySize THEN
        BEGIN
        Buffer1Bytes := BufferArraySize;

        NumberBytes := NumberBytes - BufferArraySize;

        IF NumberBytes > BufferArraySize THEN
            BEGIN
            Buffer2Bytes := BufferArraySize;
            NumberBytes := NumberBytes - BufferArraySize;

            IF NumberBytes > BufferArraySize THEN
                BEGIN
                ReportError ('This cell is too big!');
                Halt;
                END
            ELSE
                Buffer3Bytes := NumberBytes;

            END
        ELSE
            Buffer2Bytes := NumberBytes;
        END
    ELSE
        Buffer1Bytes := NumberBytes;

    END;



PROCEDURE CellBufferObject.LoadCellIntoBuffer (KeyString: CallString;
                                               VAR FileRead: FILE;
                                               NumberBytes: LONGINT);

VAR BytesRead: WORD;

    BEGIN
    Initialize (NumberBytes);

    IF MaximumMemoryToUse <= BufferArraySize THEN  { TR mode }
        BEGIN
        BlockRead (FileRead, Buffer1^, NumberBytes, BytesRead);
        Key := KeyString;
        NumberBufferEntries := NumberBytes;
        ReadAddress := 0;
        Exit;
        END;

    FigureOutBufferSizes (NumberBytes);

    IF (Buffer1Bytes > 0) AND Buffer1Used THEN
        BEGIN
        BlockRead (FileRead, Buffer1^, Buffer1Bytes, BytesRead);

        IF BytesRead < Buffer1Bytes THEN
            BEGIN
            ReportError ('File read error!!  Line #721 in LOGSCP.PAS.');
            Halt;
            END;
        END;

    IF (Buffer2Bytes > 0) AND Buffer2Used THEN
        BEGIN
        BlockRead (FileRead, Buffer2^, Buffer2Bytes, BytesRead);

        IF BytesRead < Buffer2Bytes THEN
            BEGIN
            ReportError ('File read error!!  Line #721 in LOGSCP.PAS.');
            Halt;
            END;
        END;

    IF (Buffer3Bytes > 0) AND Buffer3Used THEN
        BEGIN
        BlockRead (FileRead, Buffer3^, Buffer3Bytes, BytesRead);

        IF BytesRead < Buffer3Bytes THEN
            BEGIN
            ReportError ('File read error!!  Line #721 in LOGSCP.PAS.');
            Halt;
            END;
        END;

    Key := KeyString;
    NumberBufferEntries := NumberBytes;
    ReadAddress := 0;
    END;



FUNCTION CellBufferObject.GetNextEntry (VAR EntryString: STRING): BOOLEAN;

{ Returns FALSE when no more entries left in the cell buffer }

VAR CellChar: CHAR;

    BEGIN
    EntryString := '';

    WHILE ReadAddress <= NumberBufferEntries - 1 DO
        BEGIN
        IF ReadAddress < BufferArraySize THEN
            CellChar := Buffer1^ [ReadAddress]
        ELSE
            IF ReadAddress < BufferArraySize * 2 THEN
                CellChar := Buffer2^ [ReadAddress - BufferArraySize]
            ELSE
                CellChar := Buffer3^ [ReadAddress - (2 * BufferArraySize)];

        Inc (ReadAddress);

        IF CellChar = Chr (0) THEN
            BEGIN
            GetNextEntry := True;
            Exit;
            END;

        EntryString := EntryString + CellChar;
        END;

    { We didn't find a null character at the end - return a null string }

    EntryString := '';
    GetNextEntry := False;
    END;



FUNCTION CellBufferObject.GetNextEntryAddress (VAR EntryAddress: POINTER): BOOLEAN;

{ Returns FALSE when no more entries left in the cell buffer }

VAR CellChar: CHAR;

    BEGIN
    { Find address of first byte in the record to be read }

    IF ReadAddress < BufferArraySize THEN
        EntryAddress := Addr (Buffer1^ [ReadAddress])
    ELSE
        IF ReadAddress < BufferArraySize * 2 THEN
            BEGIN
            IF NOT Buffer2Used THEN
                BEGIN
                GetNextEntryAddress := False;
                Exit;
                END;


            EntryAddress := Addr (Buffer2^ [ReadAddress - BufferArraySize])
            END
        ELSE
            BEGIN
            IF NOT Buffer3Used THEN
                BEGIN
                GetNextEntryAddress := False;
                Exit;
                END;

            EntryAddress := Addr (Buffer3^ [ReadAddress - (2 * BufferArraySize)]);
            END;

    WHILE ReadAddress <= NumberBufferEntries - 1 DO
        BEGIN
        IF ReadAddress < BufferArraySize THEN
            CellChar := Buffer1^ [ReadAddress]
        ELSE
            IF ReadAddress < BufferArraySize * 2 THEN
                CellChar := Buffer2^ [ReadAddress - BufferArraySize]
            ELSE
                CellChar := Buffer3^ [ReadAddress - (2 * BufferArraySize)];

        Inc (ReadAddress);

        IF CellChar = Chr (0) THEN
            BEGIN
            GetNextEntryAddress := True;
            Exit;
            END;
        END;

    { We didn't find a null character at the end - return a null string }

    GetNextEntryAddress := False;
    END;



FUNCTION GetRandomLetter: CHAR;

    BEGIN
    GetRandomLetter := Chr (Ord ('A') + Random (26));
    END;

FUNCTION GetRandomNumber: CHAR;

    BEGIN
    GetRandomNumber := Chr (Ord ('0') + Random (10));
    END;

PROCEDURE CallDatabase.PushChar (InChar: CHAR; VAR BlockNumber: WORD; VAR BlockAddress: WORD);

    BEGIN
    MemoryBlocks [BlockNumber]^ [BlockAddress] := Ord (InChar);

    Inc (BlockAddress);

    IF BlockAddress = SizeOf (MemoryBlock) THEN
        BEGIN
        BlockAddress := 0;
        Inc (BlockNumber);
        END;
    END;



FUNCTION KeyInRange (X, Y, FirstKeyX, FirstKeyY, LastKeyX, LastKeyY: INTEGER): BOOLEAN;

{ Returns TRUE if the key is >= FirstKey and < LastKey }

    BEGIN
    KeyInRange := False;

    IF (X < 0) OR (X > 36) OR (Y < 0) OR (Y > 36) THEN Exit;

    IF (X < FirstKeyX) OR (X > LastKeyX) THEN Exit;

    IF (X = FirstKeyX) AND (Y < FirstKeyY) THEN Exit;

    IF (X = LastKeyX) AND (Y >= LastKeyY) THEN Exit;

    KeyInRange := True;
    END;



FUNCTION NumberOfDataBytes (Data: DatabaseEntryRecord): INTEGER;

VAR NumberBytes: INTEGER;

    BEGIN
    WITH DATA DO
        BEGIN
        NumberBytes := Length (Call);

        IF Section <> '' THEN NumberBytes := NumberBytes + Length (Section) + 1;
        IF CQZone  <> '' THEN NumberBytes := NumberBytes + Length (CQZone)  + 1;
        IF FOC     <> '' THEN NumberBytes := NumberBytes + Length (FOC)     + 1;
        IF Hits     >  0 THEN NumberBytes := NumberBytes + 2;
        IF Grid    <> '' THEN NumberBytes := NumberBytes + Length (Grid)    + 1;
        IF ITUZone <> '' THEN NumberBytes := NumberBytes + Length (ITUZone) + 1;
        IF Check   <> '' THEN NumberBytes := NumberBytes + Length (Check)   + 1;
        IF Name    <> '' THEN NumberBytes := NumberBytes + Length (Name)    + 1;
        IF OldCall <> '' THEN NumberBytes := NumberBytes + Length (OldCall) + 1;
        IF QTH     <> '' THEN NumberBytes := NumberBytes + Length (QTH)     + 1;
        IF Speed    >  0 THEN NumberBytes := NumberBytes + 2;
        IF TenTen  <> '' THEN NumberBytes := NumberBytes + Length (TenTen)  + 1;
        IF User1   <> '' THEN NumberBytes := NumberBytes + Length (User1)   + 1;
        IF User2   <> '' THEN NumberBytes := NumberBytes + Length (User2)   + 1;
        IF User3   <> '' THEN NumberBytes := NumberBytes + Length (User3)   + 1;
        IF User4   <> '' THEN NumberBytes := NumberBytes + Length (User4)   + 1;
        IF User5   <> '' THEN NumberBytes := NumberBytes + Length (User5)   + 1;
        END;

    NumberBytes := NumberBytes + 1;     { for the null character at the end }
    NumberOfDataBytes := NumberBytes;
    END;



FUNCTION GetSize (EntryArray: EntryArrayPtr): INTEGER;

VAR EntrySize: INTEGER;

    BEGIN
    EntrySize := 0;

    WHILE (EntryArray^ [EntrySize] <> Chr (0)) DO
        Inc (EntrySize);

    Inc (EntrySize);  { Give me the null character too }

    GetSize := EntrySize;
    END;



FUNCTION GetCall (EntryArray: EntryArrayPtr): CallString;

VAR TempCall: Str20;
    CharPointer: INTEGER;

    BEGIN
    TempCall := '';

    CharPointer := 0;

    WHILE EntryArray^ [CharPointer] > ControlZ DO
        BEGIN
        TempCall := TempCall + EntryArray^ [CharPointer];
        Inc (CharPointer);
        END;

    GetCall := TempCall;
    END;



FUNCTION GetArrayString (VAR InputArray: EntryArrayPtr): STRING;

VAR TempString: STRING;
    Address: INTEGER;

    BEGIN
    TempString := '';
    Address := 0;

    WHILE InputArray^ [Address] <> Chr (0) DO
        BEGIN
        TempString := TempString + InputArray^ [Address];
        Inc (Address);
        END;

    GetArrayString := TempString;
    END;



PROCEDURE ConvertDatbaseEntryRecordToEntryArray (InputRecord: DatabaseEntryRecord;
                                                 VAR OutputArray: EntryArrayPtr);

VAR InputAddress, OutputAddress: INTEGER;

    BEGIN
    OutputAddress := 0;

    WITH InputRecord DO
        BEGIN
        FOR InputAddress := 1 TO Length (Call) DO
            BEGIN
            OutputArray^ [OutputAddress] := Call [InputAddress];
            Inc (OutputAddress);
            END;

        IF Section <> '' THEN
            BEGIN
            OutputArray^ [OutputAddress] := ControlA;
            Inc (OutputAddress);

            FOR InputAddress := 1 TO Length (Section) DO
                BEGIN
                OutputArray^ [OutputAddress] := Section [InputAddress];
                Inc (OutputAddress);
                END;
            END;

        IF CQZone <> '' THEN
            BEGIN
            OutputArray^ [OutputAddress] := ControlC;
            Inc (OutputAddress);

            FOR InputAddress := 1 TO Length (CQZone) DO
                BEGIN
                OutputArray^ [OutputAddress] := CQZone [InputAddress];
                Inc (OutputAddress);
                END;
            END;

        IF FOC <> '' THEN
            BEGIN
            OutputArray^ [OutputAddress] := ControlF;
            Inc (OutputAddress);

            FOR InputAddress := 1 TO Length (FOC) DO
                BEGIN
                OutputArray^ [OutputAddress] := FOC [InputAddress];
                Inc (OutputAddress);
                END;
            END;

        IF Hits > 0 THEN
            BEGIN
            OutputArray^ [OutputAddress] := ControlH;
            Inc (OutputAddress);
            OutputArray^ [OutputAddress] := Chr (Hits);
            Inc (OutputAddress);
            END;

        IF Grid <> '' THEN
            BEGIN
            OutputArray^ [OutputAddress] := ControlG;
            Inc (OutputAddress);

            FOR InputAddress := 1 TO Length (Grid) DO
                BEGIN
                OutputArray^ [OutputAddress] := Grid [InputAddress];
                Inc (OutputAddress);
                END;
            END;


        IF ITUZone <> '' THEN
            BEGIN
            OutputArray^ [OutputAddress] := ControlI;
            Inc (OutputAddress);

            FOR InputAddress := 1 TO Length (ITUZone) DO
                BEGIN
                OutputArray^ [OutputAddress] := ITUZone [InputAddress];
                Inc (OutputAddress);
                END;
            END;

        IF Check <> '' THEN
            BEGIN
            OutputArray^ [OutputAddress] := ControlK;
            Inc (OutputAddress);

            FOR InputAddress := 1 TO Length (Check) DO
                BEGIN
                OutputArray^ [OutputAddress] := Check [InputAddress];
                Inc (OutputAddress);
                END;
            END;

        IF Name <> '' THEN
            BEGIN
            OutputArray^ [OutputAddress] := ControlN;
            Inc (OutputAddress);

            FOR InputAddress := 1 TO Length (Name) DO
                BEGIN
                OutputArray^ [OutputAddress] := Name [InputAddress];
                Inc (OutputAddress);
                END;
            END;

        IF OldCall <> '' THEN
            BEGIN
            OutputArray^ [OutputAddress] := ControlO;
            Inc (OutputAddress);

            FOR InputAddress := 1 TO Length (OldCall) DO
                BEGIN
                OutputArray^ [OutputAddress] := OldCall [InputAddress];
                Inc (OutputAddress);
                END;
            END;

        IF QTH <> '' THEN
            BEGIN
            OutputArray^ [OutputAddress] := ControlQ;
            Inc (OutputAddress);

            FOR InputAddress := 1 TO Length (QTH) DO
                BEGIN
                OutputArray^ [OutputAddress] := QTH [InputAddress];
                Inc (OutputAddress);
                END;
            END;

        IF Speed <> 0 THEN
            BEGIN
            OutputArray^ [OutputAddress] := ControlS;
            Inc (OutputAddress);
            OutputArray^ [OutputAddress] := Chr (Speed);
            Inc (OutputAddress);
            END;

        IF TenTen <> '' THEN
            BEGIN
            OutputArray^ [OutputAddress] := ControlT;
            Inc (OutputAddress);

            FOR InputAddress := 1 TO Length (TenTen) DO
                BEGIN
                OutputArray^ [OutputAddress] := TenTen [InputAddress];
                Inc (OutputAddress);
                END;
            END;

        IF User1 <> '' THEN
            BEGIN
            OutputArray^ [OutputAddress] := ControlU;
            Inc (OutputAddress);

            FOR InputAddress := 1 TO Length (User1) DO
                BEGIN
                OutputArray^ [OutputAddress] := User1 [InputAddress];
                Inc (OutputAddress);
                END;
            END;

        IF User2 <> '' THEN
            BEGIN
            OutputArray^ [OutputAddress] := ControlV;
            Inc (OutputAddress);

            FOR InputAddress := 1 TO Length (User2) DO
                BEGIN
                OutputArray^ [OutputAddress] := User2 [InputAddress];
                Inc (OutputAddress);
                END;
            END;

        IF User3 <> '' THEN
            BEGIN
            OutputArray^ [OutputAddress] := ControlW;
            Inc (OutputAddress);

            FOR InputAddress := 1 TO Length (User3) DO
                BEGIN
                OutputArray^ [OutputAddress] := User3 [InputAddress];
                Inc (OutputAddress);
                END;
            END;

        IF User4 <> '' THEN
            BEGIN
            OutputArray^ [OutputAddress] := ControlX;
            Inc (OutputAddress);

            FOR InputAddress := 1 TO Length (User4) DO
                BEGIN
                OutputArray^ [OutputAddress] := User4 [InputAddress];
                Inc (OutputAddress);
                END;
            END;

        IF User5 <> '' THEN
            BEGIN
            OutputArray^ [OutputAddress] := ControlY;
            Inc (OutputAddress);

            FOR InputAddress := 1 TO Length (User5) DO
                BEGIN
                OutputArray^ [OutputAddress] := User5 [InputAddress];
                Inc (OutputAddress);
                END;
            END;
        END;

    OutputArray^ [OutputAddress] := NullCharacter;
    Inc (OutputAddress);
    END;



PROCEDURE CallDatabase.MergeArrays (VAR FirstArray: EntryArrayPtr;
                                    VAR SecondArray: EntryArrayPtr;
                                    VAR OutputArray: EntryArrayPtr);

{ This will move any data from the first array into the second one. }

VAR FirstDataRecord, SecondDataRecord, OutputDataRecord: DatabaseEntryRecord;

    BEGIN
    ParseEntryToDataRecord (GetArrayString (FirstArray),  FirstDataRecord);
    ParseEntryToDataRecord (GetArrayString (SecondArray), SecondDataRecord);

    ClearDataEntry (OutputDataRecord);

    { Start with the contents of the first record }

    OutputDataRecord := FirstDataRecord;

    { We add the hits together }

    IF FirstDataRecord.Hits + SecondDataRecord.Hits > 255 THEN
        OutputDataRecord.Hits := 255
    ELSE
        OutputDataRecord.Hits := FirstDataRecord.Hits + SecondDataRecord.Hits;

    { If the second record has any data - put it into the output record }

    IF SecondDataRecord.Section <> '' THEN OutputDataRecord.Section := SecondDataRecord.Section;
    IF SecondDataRecord.CQZone  <> '' THEN OutputDataRecord.CQZone  := SecondDataRecord.CQZone;
    IF SecondDataRecord.FOC     <> '' THEN OutputDataRecord.FOC     := SecondDataRecord.FOC;
    IF SecondDataRecord.Grid    <> '' THEN OutputDataRecord.Grid    := SecondDataRecord.Grid;
    IF SecondDataRecord.ITUZone <> '' THEN OutputDataRecord.ITUZone := SecondDataRecord.ITUZone;
    IF SecondDataRecord.Check   <> '' THEN OutputDataRecord.Check   := SecondDataRecord.Check;
    IF SecondDataRecord.Name    <> '' THEN OutputDataRecord.Name    := SecondDataRecord.Name;
    IF SecondDataRecord.OldCall <> '' THEN OutputDataRecord.OldCall := SecondDataRecord.OldCall;
    IF SecondDataRecord.QTH     <> '' THEN OutputDataRecord.QTH     := SecondDataRecord.QTH;
    IF SecondDataRecord.Speed   <>  0 THEN OutputDataRecord.Speed   := SecondDataRecord.Speed;
    IF SecondDataRecord.TenTen  <> '' THEN OutputDataRecord.TenTen  := SecondDataRecord.TenTen;
    IF SecondDataRecord.User1   <> '' THEN OutputDataRecord.User1   := SecondDataRecord.User1;
    IF SecondDataRecord.User2   <> '' THEN OutputDataRecord.User2   := SecondDataRecord.User2;
    IF SecondDataRecord.User3   <> '' THEN OutputDataRecord.User3   := SecondDataRecord.User3;
    IF SecondDataRecord.User4   <> '' THEN OutputDataRecord.User4   := SecondDataRecord.User4;
    IF SecondDataRecord.User5   <> '' THEN OutputDataRecord.User5   := SecondDataRecord.User5;

    { Convert the output record back into a packed array in the format used
      in the TRMASTER.DTA file }

    ConvertDatbaseEntryRecordToEntryArray (OutputDataRecord, OutputArray);
    END;



FUNCTION AlreadySaved (Call: CallString): BOOLEAN;

VAR Address: INTEGER;

    BEGIN
    IF NumberCallsAlreadySaved > 0 THEN
        FOR Address := 0 TO NumberCallsAlreadySaved - 1 DO
            IF Call = CallsAlreadySaved^ [Address] THEN
                BEGIN
                AlreadySaved := True;
                Exit;
                END;

    AlreadySaved := False;

    IF NumberCallsAlreadySaved < MaximumCallsAlreadSaved THEN
        BEGIN
        CallsAlreadySaved^ [NumberCallsAlreadySaved] := Call;
        Inc (NumberCallsAlreadySaved);
        END;
    END;



PROCEDURE CallDatabase.SaveCallsAndNamesToFile (FileName: Str40);

VAR CellSize: LONGINT;
    FileRead: FILE;
    FileWrite: TEXT;
    X, Y, NextX, NextY, StartingOffset, EndingOffset: LONGINT;
    KeyString, TempString: Str40;
    EntryString: STRING;
    DataRecord: DataBaseEntryRecord;

    BEGIN
    IF NOT LoadInIndexArray THEN Exit;
    IF FileName = '' THEN Exit;

    OpenFileForWrite (FileWrite, FileName);

    GoToXY (1, WhereY);
    Write ('Saving .DTA file to ASCII file.  Processing cell   ');

    CellBuffer.Initialize (CellSize);  { Only works in POST mode }

    New (CallsAlreadySaved);

    FOR X := 0 TO 36 DO
        FOR Y := 0 TO 36 DO
            BEGIN
            NumberCallsAlreadySaved := 0;   { Set to zero for each cell }

            GoToXY (WhereX - 2, WhereY);

            KeyString := GetSCPCharFromInteger (X) + GetSCPCharFromInteger (Y);

            Write (KeyString);

            StartingOffset := SCPIndexArray^ [X, Y];

            NextY := Y + 1;
            NextX := X;

            IF NextY > 36 THEN
                BEGIN
                IF X < 36 THEN
                    BEGIN
                    NextX := X + 1;
                    NextY := 0;
                    EndingOffset := SCPIndexArray^ [NextX, NextY];
                    END
                ELSE
                    EndingOffset := SCPEndOfFile;
                END
            ELSE
                EndingOffset := SCPIndexArray^ [NextX, NextY];

            IF (X <> 0) OR (Y <> 0) THEN
                BEGIN
                IF FilePos (FileRead) <> StartingOffset THEN
                    BEGIN
                    Close  (FileRead);
                    Assign (FileRead, ActiveFileName);
                    Reset  (FileRead, 1);
                    Seek   (FileRead, StartingOffset);
                    END;
                END
            ELSE
                BEGIN
                Assign (FileRead, ActiveFileName);
                Reset  (FileRead, 1);
                Seek   (FileRead, StartingOffset);
                END;

            IF StartingOffset < EndingOffset THEN
                BEGIN
                CellSize := EndingOffset - StartingOffset;

                IF CellSize > 3 * BufferArraySize THEN
                    BEGIN
                    WriteLn;
                    ReportError (GetSCPCharFromInteger (X) + GetSCPCharFromInteger (Y) + ' cell is too large!!!');
                    Halt;
                    END;

                CellBuffer.LoadCellIntoBuffer (KeyString, FileRead, CellSize);

                WHILE CellBuffer.GetNextEntry (EntryString) DO
                    BEGIN
                    ParseEntryToDataRecord (EntryString, DataRecord);

                    IF DataRecord.Name <> '' THEN
                        IF FirstCellForThisCall (DataRecord.Call, X, Y) THEN
                            IF DoubleIndexCall (DataRecord.Call) THEN
                                BEGIN
                                IF NOT AlreadySaved (DataRecord.Call) THEN
                                    BEGIN
                                    TempString := DataRecord.Call;
                                    WHILE Length (TempString) < 15 DO TempString := TempString + ' ';
                                    TempString := TempString + DataRecord.Name;
                                    WriteLn (FileWrite, TempString);
                                    END;
                                END
                            ELSE
                                BEGIN
                                TempString := DataRecord.Call;
                                WHILE Length (TempString) < 15 DO TempString := TempString + ' ';
                                TempString := TempString + DataRecord.Name;
                                WriteLn (FileWrite, TempString);
                                END;
                    END;

                END;
            END;

    CellBuffer.GoAway;
    Dispose (CallsAlreadySaved);

    Close (FileWrite);
    ASCIIFileIsCurrent := True;
    GoToXY (1, WhereY);
    ClrEol;
    TextColor (Cyan);
    WriteLn ('Data successfully saved to ASCII file.');
    END;



PROCEDURE CallDatabase.AddInBytes (NumberBytes, X, Y: INTEGER);

{ Will increment the totals in the SCPIndexArray starting for the cell
  indicated }

VAR XAddress, YAddress: INTEGER;

    BEGIN
    { We start at the end of the specified cell }

    Inc (Y);

    IF Y > 36 THEN
        BEGIN
        Inc (X);
        Y := 0;
        END;

    IF X <= 36 THEN
        FOR XAddress := X TO 36 DO
            FOR YAddress := Y TO 36 DO
                SCPIndexArray^ [XAddress, YAddress] := SCPIndexArray^ [XAddress, YAddress] + NumberBytes;

    SCPEndOfFile := SCPEndOfFile + NumberBytes;
    END;



PROCEDURE CallDatabase.GetBestOffsets (Call: CallString;
                                       VAR StartingOffset, EndingOffset: LONGINT;
                                       VAR X, Y: INTEGER);

VAR BestTwoLetterString: Str20;
    NextX, NextY: INTEGER;

    BEGIN
    BestTwoLetterString := BestTwoLetters (Call);

    IF BestTwoLetterString = '' THEN  { No calls found in array }
        BEGIN
        StartingOffset := 0;
        EndingOffset   := 0;
        Exit;
        END;

    X := GetSCPIntegerFromChar (BestTwoLetterString [1]);
    Y := GetSCPIntegerFromChar (BestTwoLetterString [2]);

    IF (X <> -1) AND (Y <> -1) THEN
        BEGIN
        StartingOffset := SCPIndexArray^ [X, Y];

        NextY := Y + 1;
        NextX := X;

        IF NextY > 36 THEN
            BEGIN
            IF X < 36 THEN
                BEGIN
                NextX := X + 1;
                NextY := 0;
                EndingOffset := SCPIndexArray^ [NextX, NextY];
                END
            ELSE
                EndingOffset := SCPEndOfFile;
            END
        ELSE
            EndingOffset := SCPIndexArray^ [NextX, NextY];
        END;

    END;



PROCEDURE CallDatabase.AddEntry (NewData: DataBaseEntryRecord;
                                 BuildNewFile: BOOLEAN);

{ This procedure will add the data in NewData for the callsign specified.
  We will check to see if the current ASCII list is up to date, and if
  so, make the changes to that file, then build a new SCP file from
  the ASCII information.  If the ASCII file isn't up to date, then
  we will first create it, add the new info, then rebuild the file.

  However, if you know you are going to be making a lot of changes,
  you can tell the routine to not build the new file.  Call with
  a null callsign to just build the current ASCII file. }

VAR FileRead, FileWrite: TEXT;
    FileString: STRING;
    OldData: DataBaseEntryRecord;

    BEGIN
    IF NOT ASCIIFileIsCurrent THEN SaveToASCIIFile;

    { See if we have a call to add data for }

    IF NewData.Call <> '' THEN
        BEGIN
        RenameFile (WorkingDirectory + ActiveASCIIFileName, WorkingDirectory + 'ADDENTRY.$$$');

        IF OpenFileForRead (FileRead, WorkingDirectory + 'ADDENTRY.$$$') THEN
            BEGIN
            OpenFileForWrite (FileWrite, WorkingDirectory + ActiveASCIIFileName);

            WHILE NOT Eof (FileRead) DO
                BEGIN
                ReadLn (FileRead, FileString);

                IF GetFirstString (FileString) = NewData.Call THEN
                    BEGIN
                    GetDataFromASCIIEntry (FileString, OldData, Chr (0));
                    TransferNewData (OldData, NewData);
                    SaveDataToASCIIFile (FileWrite, NewData);

                    { Go into a tight loop to finish writing the rest of the data }

                    WHILE NOT Eof (FileRead) DO
                        BEGIN
                        ReadLn (FileRead, FileString);
                        WriteLn (FileWrite, FileString);
                        END;

                    { Close the files }

                    Close (FileRead);
                    Close (FileWrite);

                    IF BuildNewFile THEN
                        BuildNewDataBaseFromASCIIFile (Chr (0));

                    { Add done }

                    Exit;
                    END
                ELSE
                    WriteLn (FileWrite, FileString);
                END;

            { We never found the call in the ASCII list - add it to the end }

            SaveDataToASCIIFile (FileWrite, NewData);

            { Close the files }

            Close (FileRead);
            Close (FileWrite);
            END
        ELSE
            BEGIN
            ReportError ('ERROR K1AR!!  Temporary file that I just created can not be opened!!');
            Halt;
            END;
        END;

    { See if we need to build a new file }

    IF BuildNewFile THEN BuildNewDataBaseFromASCIIFile (Chr (0));
    END;



FUNCTION CallDatabase.BestTwoLetters (Partial: CallString): Str20;

{ This routine will return the best two letters to be used when looking
  for the specified callsign/partial. }


VAR CharPos: INTEGER;
    MinimumBytes: LONGINT;
    NumberBytes: ARRAY [1..11] OF LONGINT;

    BEGIN
    BestTwoLetters := '';               { Default if we fall out }

    IF NOT LoadInIndexArray THEN Exit;

    IF Length (Partial) < 2 THEN Exit;  { Can't deal with this }

    MinimumBytes := AVeryBigNumber;

    FOR CharPos := 1 TO Length (Partial) - 1 DO
        BEGIN
        IF Copy (Partial, CharPos, 2) = 'JA' THEN Continue;

        NumberBytes [CharPos] := NumberOfBytesAtThisAddress (Copy (Partial, CharPos, 2));

        { We get zero if we had an illegal character - like a question mark }

        IF NumberBytes [CharPos] > 0 THEN
            IF NumberBytes [CharPos] < MinimumBytes THEN
                MinimumBytes := NumberBytes [CharPos];
        END;

    { Now, which pair had the minimum cell size? }

    FOR CharPos := 1 TO Length (Partial) - 1 DO
        IF NumberBytes [CharPos] = MinimumBytes THEN
            BEGIN
            BestTwoLetters := Copy (Partial, CharPos, 2);
            Exit;
            END;

    { We fall through leaving a null string if we couldn't process the
      data.  This might occure if someone called us with a string that
      didn't have two non wildcard entries in a row (like N?6). }

    END;



PROCEDURE CallDatabase.BlowAwayFirstLettersList;

{ This procedure will clear out the InitialPartialList }

VAR NextRecord, ActiveRecord: PartialCallListEntryPtr;

    BEGIN
    ActiveRecord := InitialPartialList;

    WHILE ActiveRecord <> nil DO
        BEGIN
        NextRecord := ActiveRecord^.NextEntry;
        Dispose (ActiveRecord);
        ActiveRecord := NextRecord;
        END;

    InitialPartialCall := '';
    InitialPartialList := nil;
    END;



FUNCTION CallDatabase.BuildNewDatabaseFromASCIIFile (Ignore: CHAR): BOOLEAN;

{ This procedure will create a new binary .DTA file from the active
  ASCII file.  }

TYPE Point = ARRAY [0..1] OF INTEGER;

VAR FileString: STRING;
//    Done: BOOLEAN;
    FileRead: TEXT;
    FileWrite: FILE;
    Data: DatabaseEntryRecord;
    NumberPoints, Block, X, Y, PointAddress, NumberBlocks, NumberBytes: INTEGER;
    Address, LastTotal, MemoryAddress, MemoryAvailable, MemoryOffset: LONGINT;
    FirstKeyX, FirstKeyY, LastKeyX, LastKeyY: INTEGER;
    PointList: ARRAY [0..10] OF Point;
    BlockNumber, LargeX, LargeY, BlockAddress, CharPointer: WORD;
    LargestCell, TotalBytesWritten, NumberBytesToWrite: LONGINT;
    BytesWrittenArray: BytesWrittenArrayPtr;

    BEGIN
    PointList[0,0] := 0; //kill uninitialized warning message
    BuildNewDatabaseFromASCIIFile := False;

    { See if we have an ASCII file to work with }

    IF NOT FileExists (WorkingDirectory + ActiveASCIIFileName) THEN Exit;

    { Generate index array - the first step is to examine the ASCII
      file and generate the proper index array.  We do this first
      so we can write it to the file and then forget about it }

    TextColor (Cyan);
    WriteLn ('Building new ', ActiveFileName, ' file from ', WorkingDirectory + ActiveASCIIFileName,
             '.  This may take a minute.');
    WriteLn;

    TextColor (Yellow);
    Write ('Calculating size of each two letter cell...');

    IF NOT IndexArrayAllocated THEN
        BEGIN
        New (SCPIndexArray);
        IndexArrayAllocated := True;
        END;

    FOR X := 0 TO 36 DO
        FOR Y := 0 TO 36 DO
            SCPIndexArray^ [X, Y] := 0;

    LargestCell := 0;

    IF NOT OpenFileForRead (FileRead, WorkingDirectory + ActiveASCIIFileName) THEN Exit;

    WHILE NOT Eof (FileRead) DO
        BEGIN
        ReadLn (FileRead, FileString);
        GetRidOfPrecedingSpaces (FileString);

        NumberPoints := 0;

        IF FileString <> '' THEN
            BEGIN
            GetDataFromASCIIEntry (FileString, Data, Ignore);
            NumberBytes := NumberOfDataBytes (Data);

            FOR Address := 1 TO Length (Data.Call) - 1 DO
                BEGIN
                X := GetSCPIntegerFromChar (Data.Call [Address]);
                Y := GetSCPIntegerFromChar (Data.Call [Address + 1]);

                { Skip the JA cell }

                IF Copy (Data.Call, Address, 2) = 'JA' THEN Continue;

                IF (X <> -1) AND (Y <> -1) THEN  { Valid pair }
                    BEGIN
                    IF NumberPoints > 0 THEN
                        FOR PointAddress := 0 TO NumberPoints - 1 DO
                            IF ((X = PointList [PointAddress, 0]) AND
                                (Y = PointList [PointAddress, 1])) THEN
                                    Continue;

                    SCPIndexArray^ [X, Y] := SCPIndexArray^ [X, Y] + NumberBytes;
                    PointList [NumberPoints, 0] := X;
                    PointList [NumberPoints, 1] := Y;
                    Inc (NumberPoints);
                    END;

                END;
            END;
        END;

    { Convert the totals for each cell into real addresses }

    Address := SizeOf (SCPIndexArray^) + SizeOf (SCPEndOfFile);

    FOR X := 0 TO 36 DO
        FOR Y := 0 TO 36 DO
            BEGIN
            IF SCPIndexArray^ [X, Y] > LargestCell THEN
                BEGIN
                LargestCell := SCPIndexArray^ [X, Y];
                LargeX := X;
                LargeY := Y;
                END;

            LastTotal := SCPIndexArray^ [X, Y];
            SCPIndexArray^ [X, Y] := Address;
            Address := LastTotal + Address;
            END;

    SCPEndOfFile := Address;

    { Open up the output file and write the index array }

    GoToXY (1, WhereY);
    TextColor (Cyan);
    ClrEol;
    WriteLn ('Cell sizes computed.  Largest cell = ', LargestCell, ' bytes at cell ',
            GetSCPCharFromInteger (LargeX), GetSCPCharFromInteger (LargeY));

    TextColor (Yellow);
    Write ('Opening output file and saving index array...');

    Assign  (FileWrite, WorkingDirectory + TempFileName);
    ReWrite (FileWrite, 1);

    BlockWrite (FileWrite, SCPIndexArray^, SizeOf (SCPIndexArray^));
    BlockWrite (FileWrite, SCPEndOfFile, SizeOf (SCPEndOfFile));

    TotalBytesWritten := SizeOf (SCPIndexArray^) + SizeOf (SCPEndOfFile);

    { Now we need to use all the memory we can get our hands onto so
      we can minimize the number of passes.  }

    NumberBlocks := 0;

    New (BytesWrittenArray);

    WHILE (SizeOF (MemoryBlock) < MaxAvail) AND (NumberBlocks < MaxBlocks) DO
        BEGIN
        New (MemoryBlocks [NumberBlocks]);
        Inc (NumberBlocks);
        END;

    MemoryAvailable := NumberBlocks * SizeOf (MemoryBlock);
    MemoryOffset := 0;

    FirstKeyX := 0;
    FirstKeyY := 0;

    FOR X := 0 TO 36 DO
        FOR Y := 0 TO 36 DO
            BEGIN
            BytesWrittenArray^ [X, Y] := 0;

            IF SCPIndexArray^ [X, Y] < MemoryAvailable THEN
                BEGIN
                LastKeyX := X;
                LastKeyY := Y;
                END;
            END;


//    Done := False;

    GoToXY (1, WhereY);
    TextColor (Cyan);
    ClrEol;
    WriteLn ('Output file opened and index array saved.');

    WHILE True DO
        BEGIN
        GoToXY (1, WhereY);
        ClrEol;

        TextColor (Cyan);

        Write ('Reading data from ASCII file for cells ',
               GetSCPCharFromInteger (FirstKeyX), GetSCPCharFromInteger (FirstKeyY),
               ' up to ',
               GetSCPCharFromInteger (LastKeyX), GetSCPCharFromInteger (LastKeyY), '...');

        OpenFileForRead (FileRead, WorkingDirectory + ActiveASCIIFileName);

        WHILE NOT EOf (FileRead) DO
            BEGIN
            ReadLn (FileRead, FileString);
            GetDataFromASCIIEntry (FileString, Data, Ignore);

            NumberPoints := 0;

            FOR Address := 1 TO Length (Data.Call) - 1 DO
                BEGIN
                X := GetSCPIntegerFromChar (Data.Call [Address]);
                Y := GetSCPIntegerFromChar (Data.Call [Address + 1]);

                { We ignore JA cell entries }

                IF Copy (Data.Call, Address, 2) = 'JA' THEN Continue;

                IF KeyInRange (X, Y, FirstKeyX, FirstKeyY, LastKeyX, LastKeyY) THEN
                    BEGIN
                    IF NumberPoints > 0 THEN
                        FOR PointAddress := 0 TO NumberPoints - 1 DO
                            IF ((X = PointList [PointAddress, 0]) AND
                                (Y = PointList [PointAddress, 1])) THEN
                                    Continue;

                    { Save this key so we don't dupe call in this cell }

                    PointList [NumberPoints, 0] := X;
                    PointList [NumberPoints, 1] := Y;
                    Inc (NumberPoints);

                    { Figure out where this is going to go }

                    { Got a runtime 201 error at the next instruction
                      Address = 1226:146D while reading from 4H to 8G }

                    MemoryAddress := BytesWrittenArray^ [X, Y] +
                                     SCPIndexArray^ [X, Y] -
                                     SizeOf (SCPIndexArray^) -
                                     SizeOf (SCPEndOfFile);

                    BlockNumber  := (MemoryAddress - MemoryOffset) DIV SizeOf (MemoryBlock);

                    { Runtime 201 error at next instruction.
                      Address = 01AE:30BC.  Because MemoryAddress = -5480
                      and MemoryOffset := 0.  SizeOf (SCPEndofFile) = 4
                      and SizeOf (SCPIndexArray^) = 5476.
                      SCPIndexArray^ [29,1] = 0 }

                    BlockAddress := (MemoryAddress - MemoryOffset) MOD SizeOf (MemoryBlock);

                    WITH Data DO
                        BEGIN
                        FOR CharPointer := 1 TO Length (Data.Call) DO
                            PushChar (Call [CharPointer], BlockNumber, BlockAddress);

                        IF Section <> '' THEN
                            BEGIN
                            PushChar (ControlA, BlockNumber, BlockAddress);

                            FOR CharPointer := 1 TO Length (Section) DO
                                PushChar (Section [CharPointer], BlockNumber, BlockAddress);
                            END;

                        IF CQZone <> '' THEN
                            BEGIN
                            PushChar (ControlC, BlockNumber, BlockAddress);

                            FOR CharPointer := 1 TO Length (CQZone) DO
                                PushChar (CQZone [CharPointer], BlockNumber, BlockAddress);
                            END;

                        IF FOC <> '' THEN
                            BEGIN
                            PushChar (ControlF, BlockNumber, BlockAddress);

                            FOR CharPointer := 1 TO Length (FOC) DO
                                PushChar (FOC [CharPointer], BlockNumber, BlockAddress);
                            END;

                        IF Hits > 0 THEN
                            BEGIN
                            PushChar (ControlH, BlockNumber, BlockAddress);
                            PushChar (Chr (Hits), BlockNumber, BlockAddress);
                            END;

                        IF Grid <> '' THEN
                            BEGIN
                            PushChar (ControlG, BlockNumber, BlockAddress);

                            FOR CharPointer := 1 TO Length (Grid) DO
                                PushChar (Grid [CharPointer], BlockNumber, BlockAddress);
                            END;

                        IF ITUZone <> '' THEN
                            BEGIN
                            PushChar (ControlI, BlockNumber, BlockAddress);

                            FOR CharPointer := 1 TO Length (ITUZone) DO
                                PushChar (ITUZone [CharPointer], BlockNumber, BlockAddress);
                            END;

                        IF Check <> '' THEN
                            BEGIN
                            PushChar (ControlK, BlockNumber, BlockAddress);

                            FOR CharPointer := 1 TO Length (Check) DO
                                PushChar (Check [CharPointer], BlockNumber, BlockAddress);
                            END;

                        IF Name <> '' THEN
                            BEGIN
                            PushChar (ControlN, BlockNumber, BlockAddress);

                            FOR CharPointer := 1 TO Length (Name) DO
                                PushChar (Name [CharPointer], BlockNumber, BlockAddress);
                            END;

                        IF OldCall <> '' THEN
                            BEGIN
                            PushChar (ControlO, BlockNumber, BlockAddress);

                            FOR CharPointer := 1 TO Length (OldCall) DO
                                PushChar (OldCall [CharPointer], BlockNumber, BlockAddress);
                            END;

                        IF QTH <> '' THEN
                            BEGIN
                            PushChar (ControlQ, BlockNumber, BlockAddress);

                            FOR CharPointer := 1 TO Length (QTH) DO
                                PushChar (QTH [CharPointer], BlockNumber, BlockAddress);
                            END;

                        IF Speed <> 0 THEN
                            BEGIN
                            PushChar (ControlS, BlockNumber, BlockAddress);
                            PushChar (Chr (Speed), BlockNumber, BlockAddress);
                            END;

                        IF TenTen <> '' THEN
                            BEGIN
                            PushChar (ControlT, BlockNumber, BlockAddress);

                            FOR CharPointer := 1 TO Length (TenTen) DO
                                PushChar (TenTen [CharPointer], BlockNumber, BlockAddress);
                            END;

                        IF User1 <> '' THEN
                            BEGIN
                            PushChar (ControlU, BlockNumber, BlockAddress);

                            FOR CharPointer := 1 TO Length (User1) DO
                                PushChar (User1 [CharPointer], BlockNumber, BlockAddress);
                            END;

                        IF User2 <> '' THEN
                            BEGIN
                            PushChar (ControlV, BlockNumber, BlockAddress);

                            FOR CharPointer := 1 TO Length (User2) DO
                                PushChar (User2 [CharPointer], BlockNumber, BlockAddress);
                            END;

                        IF User3 <> '' THEN
                            BEGIN
                            PushChar (ControlW, BlockNumber, BlockAddress);

                            FOR CharPointer := 1 TO Length (User3) DO
                                PushChar (User3 [CharPointer], BlockNumber, BlockAddress);
                            END;

                        IF User4 <> '' THEN
                            BEGIN
                            PushChar (ControlX, BlockNumber, BlockAddress);

                            FOR CharPointer := 1 TO Length (User4) DO
                                PushChar (User4 [CharPointer], BlockNumber, BlockAddress);
                            END;

                        IF User5 <> '' THEN
                            BEGIN
                            PushChar (ControlY, BlockNumber, BlockAddress);

                            FOR CharPointer := 1 TO Length (User5) DO
                                PushChar (User5 [CharPointer], BlockNumber, BlockAddress);
                            END;

                        END;

                    PushChar (NullCharacter, BlockNumber, BlockAddress);

                    BytesWrittenArray^ [X, Y] := BytesWrittenArray^ [X, Y] + NumberOfDataBytes (Data);
                    END;
                END;
            END;

        Close (FileRead);

        GoToXY (1, WhereY);
        ClrEol;

        TextColor (Cyan);

        WriteLn ('Data read from ASCII file for cells ',
                 GetSCPCharFromInteger (FirstKeyX), GetSCPCharFromInteger (FirstKeyY),
                 ' up to ',
                 GetSCPCharFromInteger (LastKeyX), GetSCPCharFromInteger (LastKeyY), '...');


        TextColor (Yellow);

        Write ('Saving data to .DTA file for cells ',
               GetSCPCharFromInteger (FirstKeyX), GetSCPCharFromInteger (FirstKeyY),
               ' to ',
               GetSCPCharFromInteger (LastKeyX), GetSCPCharFromInteger (LastKeyY), '...');

        NumberBytesToWrite := SCPIndexArray^ [LastKeyX, LastKeyY] -
                              MemoryOffset -
                              SizeOf (SCPIndexArray^) -
                              SizeOf (SCPEndOfFile);

        BlockNumber := 0;

        WHILE NumberBytesToWrite > SizeOf (MemoryBlock) DO
            BEGIN
            BlockWrite (FileWrite, MemoryBlocks [BlockNumber]^, SizeOf (MemoryBlock));
            Inc (BlockNumber);
            NumberBytesToWrite := NumberBytesToWrite - SizeOf (MemoryBlock);
            MemoryOffset := MemoryOffset + SizeOf (MemoryBlock);
            TotalBytesWritten := TotalBytesWritten + SizeOf (MemoryBlock);
            END;

        IF NumberBytesToWrite > 0 THEN
            BEGIN
            BlockWrite (FileWrite, MemoryBlocks [BlockNumber]^, NumberBytesToWrite);
            MemoryOffset := MemoryOffset + NumberBytesToWrite;
            TotalBytesWritten := TotalBytesWritten + NumberBytesToWrite;
            END;

        GoToXY (1, WhereY);
        TextColor (Cyan);
        ClrEol;
        WriteLn ('Data saved to .DTA file for cells ',
               GetSCPCharFromInteger (FirstKeyX), GetSCPCharFromInteger (FirstKeyY),
               ' to ',
               GetSCPCharFromInteger (LastKeyX), GetSCPCharFromInteger (LastKeyY), '.');

        IF (LastKeyX = 36) AND (LastKeyY = 36) THEN Break;

        { Setup to do some more }

        FirstKeyX := LastKeyX;
        FirstKeyY := LastKeyY;

        FOR X := 0 TO 36 DO
            FOR Y := 0 TO 36 DO
                BEGIN
                BytesWrittenArray^ [X, Y] := 0;

                IF (SCPIndexArray^ [X, Y] - MemoryOffset) < MemoryAvailable THEN
                    BEGIN
                    LastKeyX := X;
                    LastKeyY := Y;
                    END;
                END;

        END;

    Dispose (BytesWrittenArray);

    FOR Block := 0 TO NumberBlocks - 1 DO
        BEGIN
        Dispose (MemoryBlocks [Block]);
        MemoryBlocks [Block] := nil;
        END;

    Close (FileWrite);

    TextColor (Yellow);
    Write ('Pushing back up files...');

    RenameFile (WorkingDirectory + TempFileName, ActiveFileName);

    TextColor (Cyan);
    GoToXY (1, WhereY);
    ClrEol;
    WriteLn ('There were ', TotalBytesWritten, ' bytes written to ', ActiveFileName);
    WaitForKeyPressed;
    END;



PROCEDURE CallDatabase.ClearField;

VAR Key: CHAR;

    BEGIN
    ClearScreenAndTitle ('CLEAR TRMASTER DATA FIELD');

    WriteLn ('This procedure will allow you to clear out one of the data fields in the ');
    WriteLn ('database.  For string values, they will be set to null string.  Numberic');
    WriteLn ('values will be set to zero.');
    WriteLn;

    WriteLn ('A - Section    K - Check        U - User 1');
    WriteLn ('C - CQ Zone    N - Name         V - User 2');
    WriteLn ('F - FOC        O - Old Call     W - User 3');
    WriteLn ('G - Grid       Q - QTH          X - User 4');
    WriteLn ('H - Hits       S - Speed        Y - User 5');
    WriteLn ('I - ITU Zone   T - Ten ten #');
    WriteLn;

    REPEAT
        Key := UpCase (GetKey ('Enter field to clear (escape to abort) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'A') OR (Key = 'C') OR (Key = 'F') OR (Key = 'G') OR
          (Key = 'H') OR (Key = 'I') OR (Key = 'K') OR (Key = 'N') OR
          (Key = 'O') OR (Key = 'Q') OR (Key = 'S') OR (Key = 'T') OR
          (Key = 'U') OR (Key = 'V') OR (Key = 'W') OR (Key = 'X') OR
          (Key = 'Y');

    IF NOT ASCIIFileIsCurrent THEN SaveToASCIIFile;

    BuildNewDatabaseFromASCIIFile (Key);

    WriteLn ('COMPLETE.  If for some reason, you want to undo this operation, simply run');
    WriteLn ('the Build command from the main menu.');
    WaitForKeyPressed;
    END;



PROCEDURE CallDatabase.ClearDataEntry (VAR Data: DatabaseEntryRecord);

    BEGIN
    WITH Data DO
        BEGIN
        Call    := '';
        Section := '';
        CQZone  := '';
        FOC     := '';
        Grid    := '';
        Hits    :=  0;
        ITUZone := '';
        Check   := '';
        Name    := '';
        OldCall := '';
        QTH     := '';
        Speed   :=  0;
        TenTen  := '';
        User1   := '';
        User2   := '';
        User3   := '';
        User4   := '';
        User5   := '';
        END;
    END;



FUNCTION  CallDatabase.FirstCellForThisCall (Call: CallString; X, Y: INTEGER): BOOLEAN;

{ This function will return TRUE if X Y values passed to it point to the
  first cell this call would be found in when using a typical FOR X FOR Y
  loop to go through the database. }

VAR LeastX, LeastY, TestX, TestY, CharPointer: INTEGER;

    BEGIN
    IF Length (Call) < 2 THEN
        BEGIN
        FirstCellForThisCall := False;
        Exit;
        END;

    LeastX := 37;
    LeastY := 37;

    FOR CharPointer := 1 TO Length (Call) - 1 DO
        BEGIN
        TestX := GetSCPIntegerFromChar (Call [CharPointer]);
        TestY := GetSCPIntegerFromChar (Call [CharPointer + 1]);

        IF (TestX < LeastX) OR ((TestX = LeastX) AND (TestY < LeastY)) THEN
            BEGIN
            LeastX := TestX;
            LeastY := TestY;
            END;
        END;

    FirstCellForThisCall := (X = LeastX) AND (Y = LeastY);
    END;



FUNCTION CallDatabase.GetCodeSpeed (Call: CallString): INTEGER;

VAR Data: DatabaseEntryRecord;

    BEGIN
    IF GetEntry (Call, Data) THEN
        GetCodeSpeed := Data.Speed
    ELSE
        GetCodeSpeed := 0;
    END;



FUNCTION CallDatabase.GetName (Call: CallString): CallString;

VAR Data: DatabaseEntryRecord;
    Name: Str20;

    BEGIN
    Name := '';

    IF Name <> '' THEN
        BEGIN
        GetName := Name;
        Exit;
        END;

    IF GetEntry (Call, Data) THEN
        GetName := Data.Name
    ELSE
        GetName := '';
    END;



FUNCTION CallDatabase.GetFOCNumber (Call: CallString): CallString;

VAR Data: DatabaseEntryRecord;

    BEGIN
    IF GetEntry (RootCall (Call), Data) THEN
        GetFOCNumber := Data.FOC
    ELSE
        GetFOCNumber := '';
    END;




FUNCTION CallDatabase.GetEntry (Call: CallString;
                                VAR Data: DataBaseEntryRecord): BOOLEAN;

{ This function will fetch all the data for the callsign indicated.  It
  returns TRUE if the callsign was found. }

VAR EntryString: STRING;

    BEGIN
    IF Call = LastCallRecord.Call THEN
        BEGIN
        Data := LastCallRecord;
        GetEntry := True;
        Exit;
        END;

    GetEntry := False;

    ClearDataEntry (Data);

    IF NOT LoadInIndexArray THEN Exit;

    IF NOT PartialCallSetup (Call) THEN Exit;

    WHILE CellBuffer.GetNextEntry (EntryString) DO
        BEGIN
        ParseEntryToDataRecord (EntryString, Data);

        IF Data.Call = Call THEN
            BEGIN
            GetEntry := True;
            LastCallRecord := Data;
            Exit;
            END;
        END;

    { Didn't find anything }

    ClearDataEntry (Data);
    END;



PROCEDURE CallDatabase.GetDataFromASCIIEntry (FileString: STRING; VAR Data: DataBaseEntryRecord; Ignore: CHAR);

{ This function will decode the ASCII string and put the proper data into
  the Data variable }

VAR TestName, Command: Str40;
    ID: CHAR;
    xResult: INTEGER;

    BEGIN
    ClearDataEntry (Data);

    Data.Call := UpperCase (RemoveFirstString (FileString));

    WHILE (FileString <> '') AND (FileString [1]= '=') DO
        BEGIN
        ID := FileString [2];

        Delete (FileString, 1, 2);

        IF StringHas (FileString, '=') THEN  { Another command in waiting }
            BEGIN
            Command := PrecedingString (FileString, ' =');

            FileString := Copy (FileString,
                                Pos ('=', FileString),
                                (Length (FileString) - Pos ('=', FileString)) + 1);

            END
        ELSE
            BEGIN
            Command := FileString;
            FileString := '';
            END;

        WITH Data DO
            CASE ID OF
                'A': IF Ignore <> 'A' THEN Section := Command;
                'C': IF Ignore <> 'C' THEN CQZone  := Command;
                'F': IF Ignore <> 'F' THEN FOC     := Command;
                'G': IF Ignore <> 'G' THEN Grid    := Command;

                'H': IF Ignore <> 'H' THEN Val (Command, Hits, xResult);

                'I': IF Ignore <> 'I' THEN ITUZone := Command;
                'K': IF Ignore <> 'K' THEN Check   := Command;

                'N': IF Ignore <> 'N' THEN
                         BEGIN
                         IF Length (Command) = 1 THEN
                             Continue;

                         IF Length (Command) = 2 THEN
                             BEGIN
                             TestName := UpperCase (Command);

                             IF (TestName <> 'AL') AND (TestName <> 'ED') AND
                                (TestName <> 'OZ') AND (TestName <> 'BO') AND
                                (TestName <> 'TY') AND (TestName <> 'JO') AND
                                (TestName <> 'GO') AND (TestName <> 'OX') AND
                                (TestName <> 'CY') AND (TestName <> 'JP') AND
                                (TestName <> 'MO') AND (TestName <> 'HI') AND
                                (TestName <> 'OJ') THEN
                                    Continue;
                             END;

                         Name    := Command;
                         END;

                'O': IF Ignore <> 'O' THEN OldCall := Command;
                'Q': IF Ignore <> 'Q' THEN QTH     := Command;

                'S': IF Ignore <> 'S' THEN Val (Command, Speed, xResult);

                'T': IF Ignore <> 'T' THEN TenTen  := Command;
                'U': IF Ignore <> 'U' THEN User1   := Command;
                'V': IF Ignore <> 'V' THEN User2   := Command;
                'W': IF Ignore <> 'W' THEN User3   := Command;
                'X': IF Ignore <> 'X' THEN User4   := Command;
                'Y': IF Ignore <> 'Y' THEN User5   := Command;
                END;
        END;

    END;



FUNCTION CountryStringPresent (CountryID: Str20; CountryString: Str80): BOOLEAN;

VAR CString: Str20;
    CommaPosition: INTEGER;

    BEGIN
    WHILE Pos (',', CountryString) > 0 DO
        BEGIN
        CommaPosition := Pos (',', CountryString);

        IF CommaPosition > 1 THEN
            BEGIN
            CString := Copy (CountryString, 1, CommaPosition - 1);

            IF CString = CountryID THEN
                BEGIN
                CountryStringPresent := True;
                Exit;
                END;

            Delete (CountryString, 1, CommaPosition);
            GetRidOfPrecedingSpaces (CountryString);
            END;
        END;

    IF CountryString <> '' THEN
        CountryStringPresent := CountryID = CountryString
    ELSE
        CountryStringPresent := False;
    END;



FUNCTION CallDatabase.GoodCountry (Call: CallString): BOOLEAN;

VAR CountryID: Str20;
    TempString: Str80;

    BEGIN
    GoodCountry := True;
    IF CountryString = '' THEN Exit;

    TempString := CountryString;

    CountryID := CountryTable.GetCountryID (CountryTable.GetCountry (Call, True));

    IF (Copy (CountryString, 1, 1) = '!') OR
       (Copy (CountryString, 1, 1) = '-') THEN { Exclude countries }
        BEGIN
        Delete (TempString, 1, 1);   { Get rid of ! or - }

        IF CountryStringPresent (CountryID, TempString) THEN
            GoodCountry := False;

        Exit;
        END
    ELSE

        { Include countries}

        GoodCountry := CountryStringPresent (CountryID, TempString);
    END;



FUNCTION CallDatabase.GetNextPartialCall: CallString;

{ Returns the next possible call and puts it in the InitialPartialCallList }

VAR EntryString: STRING;
    Data: DatabaseEntryRecord;

    BEGIN
    GetNextPartialCall := '';

    IF SCPDisabledByApplication THEN Exit;
    IF CellBuffer.Key = '' THEN Exit;

    WHILE CellBuffer.GetNextEntry (EntryString) DO
        BEGIN
        ParseEntryToDataRecord (EntryString, Data);

        IF (Length (InitialPartialCall) = 2) OR (PartialCall (InitialPartialCall, Data.Call)) THEN
            IF CountryString <> '' THEN
                BEGIN
                IF GoodCountry (Data.Call) THEN
                    BEGIN
                    GetNextPartialCall := Data.Call;
                    Exit;
                    END;
                END
            ELSE
                BEGIN
                GetNextPartialCall := Data.Call;
                Exit;
                END;
        END;
    END;



FUNCTION CallDatabase.LoadInIndexArray: BOOLEAN;

{ This procedure will go read in the index array for the active file.
  It is saved in SCPIndexArray. Returns TRUE if it thinks it worked. }

VAR Directory: Str80;
    xResult: WORD;
    infile,outfile: file;
    infilename,outfilename: string;
    buf: array[1..1024] of char;
    numread,numwritten: longint;

    BEGIN
    WorkingDirectory := '';

    LoadInIndexArray := False;

    IF (SCPDisabledByApplication) OR (PossibleCallAction = LogOnly) THEN Exit; {KK1L 6.69 added LogOnly check}

    IF TRMasterFileOpen THEN
        BEGIN
        LoadInIndexArray := True;
        Exit;
        END;

    IF SizeOf (SCPIndexArray^) + 5000 >= MaxAvail THEN Exit;

    IF ActiveFileName <> '' THEN
        BEGIN

        { If the file does not appear in the active directory, we need
          to search around for it and come up with a complete path for
          it. }

        IF NOT FileExists (ActiveFileName) THEN
            BEGIN
            Directory := FindDirectory (ActiveFileName);
            IF Directory = FindDirectory('trlog') then
             BEGIN
               infilename := Directory + DirectorySeparator + ActiveFileName;
               Directory := GetEnv('HOME')+DirectorySeparator + '.trlog';
               IF NOT DirectoryExists(Directory) then
               BEGIN
                  IF NOT CreateDir(Directory) then
                     WriteLn('Failed to create $HOME/.trlog');
               END;
               outfilename := Directory + DirectorySeparator + ActiveFileName;
               assign(infile,infilename);
               reset(infile,1);
               assign(outfile,outfilename);
               rewrite(outfile,1);
               repeat
                   blockread(infile,buf,sizeof(buf),numread);
                   blockwrite(outfile,buf,numread,numwritten);
               until (numread = 0) or (numwritten <> numread);
               close(infile);
               close(outfile);
             END;

            IF Directory = '' THEN
                BEGIN
                LoadInIndexArray := False;
                Exit;
                END
            ELSE
                BEGIN
                ActiveFileName := Directory + DirectorySeparator + ActiveFileName;
                WorkingDirectory := Directory + DirectorySeparator;
                END;
            END;

        DosError := 0;  { Seems to be necessary in some cases as the
                          value will be 18 - indicating "no more files"
                          which might be because of the FindFirst that
                          was executed.  Doesn't seem to get reset by
                          the next two statements...  weird. }

        Assign (TRMasterFileRead, ActiveFileName);
        Reset (TRMasterFileRead, 1);

        IF DosError = 0 THEN
            BEGIN
            DTAFileSize := FileSize (TRMasterFileRead);

            New (SCPIndexArray);
            BlockRead (TRMasterFileRead, SCPIndexArray^, SizeOf (SCPIndexArray^), xResult);
            BlockRead (TRMasterFileRead, SCPEndOfFile,   SizeOf (SCPEndOfFile),   xResult);
            LoadInIndexArray := True;
            TRMasterFileOpen := True;
            IndexArrayAllocated := True;
            END;
        END;

    END;



PROCEDURE CallDatabase.NewTwoLetters (PartialCall: Str20);

{ This procedure will setup the GetNextPartialCall function for a
  new two letter key.  }

VAR StartingOffset, EndingOffset, NumberBytes: LONGINT;
    X, Y: INTEGER;
    KeyString: CallString;

    BEGIN
    BlowAwayFirstLettersList;

    { Get rid of old file buffer }

    GetBestOffsets (PartialCall, StartingOffset, EndingOffset, X, Y);

    KeyString := GetSCPCharFromInteger (X) + GetSCPCharFromInteger (Y);

    IF StartingOffset = EndingOffset THEN  { No calls found }
       BEGIN
       CellBuffer.GoAway;
       Exit;
       END;

    CellBuffer.Key := GetSCPCharFromInteger (X) + GetSCPCharFromInteger (Y);

    IF EndingOffset - StartingOffset > BufferArraySize THEN
        EndingOffset := StartingOffset + BufferArraySize;

    IF EndingOffset - StartingOffset >= MaxAvail THEN
        EndingOffset := StartingOffset + (MaxAvail - 5000);

    NumberBytes := EndingOffset - StartingOffset;

    Seek (TRMasterFileRead, StartingOffset);

    CellBuffer.LoadCellIntoBuffer (KeyString, TRMasterFileRead, NumberBytes);
    InitialPartialCall := PartialCall;
    END;



FUNCTION CallDatabase.NumberOfBytesAtThisAddress (TwoLetters: Str20): LONGINT;

VAR X, Y: INTEGER;
    Difference, StartingOffset, EndingOffset: LONGINT;

    BEGIN
    IF TwoLetters = '//' THEN
        BEGIN
        NumberOfBytesAtThisAddress := 0;
        Exit;
        END;

    IF NOT LoadInIndexArray THEN Exit;

    X := GetSCPIntegerFromChar (TwoLetters [1]);
    Y := GetSCPIntegerFromChar (TwoLetters [2]);

    IF (X = -1) OR (Y = -1) THEN
        BEGIN
        NumberOfBytesAtThisAddress := 0;
        Exit;
        END;

    StartingOffset := SCPIndexArray^ [X, Y];

    Inc (Y);

    IF Y > 36 THEN
        BEGIN
        Y := 0;
        Inc (X);
        END;

    EndingOffset := SCPIndexArray^ [X, Y];
    Difference := EndingOffset - StartingOffset;
    NumberOfBytesAtThisAddress := Difference;
    END;



PROCEDURE CallDatabase.ParseEntryToDataRecord (EntryString: STRING;
                                               VAR DataRecord: DatabaseEntryRecord);

{ This procedure will take a null terminated entry from the .DTA file
  and parse the data into the data record. }

VAR CharAddress: INTEGER;

    BEGIN
    ClearDataEntry (DataRecord);

    WITH DataRecord DO
        BEGIN
        CharAddress := 1;

        REPEAT
            WHILE EntryString [CharAddress] > ControlZ DO
                BEGIN
                Call := Call + EntryString [CharAddress];
                Inc (CharAddress);
                IF CharAddress > Length (EntryString) THEN Exit;
                END;

            CASE EntryString [CharAddress] OF

                ControlA: BEGIN
                          Inc (CharAddress);

                          WHILE EntryString [CharAddress] > ControlZ DO
                              BEGIN
                              Section := Section + EntryString [CharAddress];
                              Inc (CharAddress);
                              IF CharAddress > Length (EntryString) THEN Exit;
                              END;
                          END;

                ControlC: BEGIN
                          Inc (CharAddress);

                          WHILE EntryString [CharAddress] > ControlZ DO
                              BEGIN
                              CQZone := CQZone + EntryString [CharAddress];
                              Inc (CharAddress);
                              IF CharAddress > Length (EntryString) THEN Exit;
                              END;
                          END;

                ControlF: BEGIN
                          Inc (CharAddress);

                          WHILE EntryString [CharAddress] > ControlZ DO
                              BEGIN
                              FOC := FOC + EntryString [CharAddress];
                              Inc (CharAddress);
                              IF CharAddress > Length (EntryString) THEN Exit;
                              END;
                          END;

                ControlG: BEGIN
                          Inc (CharAddress);

                          WHILE EntryString [CharAddress] > ControlZ DO
                              BEGIN
                              Grid := Grid + EntryString [CharAddress];
                              Inc (CharAddress);
                              IF CharAddress > Length (EntryString) THEN Exit;
                              END;
                          END;

                ControlH: BEGIN
                          Inc (CharAddress);
                          Hits := Ord (EntryString [CharAddress]);
                          Inc (CharAddress);
                          END;

                ControlI: BEGIN
                          Inc (CharAddress);

                          WHILE EntryString [CharAddress] > ControlZ DO
                              BEGIN
                              ITUZone := ITUZone + EntryString [CharAddress];
                              Inc (CharAddress);
                              IF CharAddress > Length (EntryString) THEN Exit;
                              END;
                          END;

                ControlK: BEGIN
                          Inc (CharAddress);

                          WHILE EntryString [CharAddress] > ControlZ DO
                              BEGIN
                              Check := Check + EntryString [CharAddress];
                              Inc (CharAddress);
                              IF CharAddress > Length (EntryString) THEN Exit;
                              END;
                          END;

                ControlN: BEGIN
                          Inc (CharAddress);

                          WHILE EntryString [CharAddress] > ControlZ DO
                              BEGIN
                              Name := Name + EntryString [CharAddress];
                              Inc (CharAddress);
                              IF CharAddress > Length (EntryString) THEN Exit;
                              END;
                          END;

                ControlO: BEGIN
                          Inc (CharAddress);

                          WHILE EntryString [CharAddress] > ControlZ DO
                              BEGIN
                              OldCall := OldCall + EntryString [CharAddress];
                              Inc (CharAddress);
                              IF CharAddress > Length (EntryString) THEN Exit;
                              END;
                          END;

                ControlQ: BEGIN
                          Inc (CharAddress);

                          WHILE EntryString [CharAddress] > ControlZ DO
                              BEGIN
                              QTH := QTH + EntryString [CharAddress];
                              Inc (CharAddress);
                              IF CharAddress > Length (EntryString) THEN Exit;
                              END;
                          END;

                ControlS: BEGIN
                          Inc (CharAddress);
                          Speed := Ord (EntryString [CharAddress]);
                          Inc (CharAddress);
                          END;

                ControlT: BEGIN
                          Inc (CharAddress);

                          WHILE EntryString [CharAddress] > ControlZ DO
                              BEGIN
                              TenTen := TenTen + EntryString [CharAddress];
                              Inc (CharAddress);
                              IF CharAddress > Length (EntryString) THEN Exit;
                              END;
                          END;

                ControlU: BEGIN
                          Inc (CharAddress);

                          WHILE EntryString [CharAddress] > ControlZ DO
                              BEGIN
                              User1 := User1 + EntryString [CharAddress];
                              Inc (CharAddress);
                              IF CharAddress > Length (EntryString) THEN Exit;
                              END;
                          END;

                ControlV: BEGIN
                          Inc (CharAddress);

                          WHILE EntryString [CharAddress] > ControlZ DO
                              BEGIN
                              User2 := User2 + EntryString [CharAddress];
                              Inc (CharAddress);
                              IF CharAddress > Length (EntryString) THEN Exit;
                              END;
                          END;

                ControlW: BEGIN
                          Inc (CharAddress);

                          WHILE EntryString [CharAddress] > ControlZ DO
                              BEGIN
                              User3 := User3 + EntryString [CharAddress];
                              Inc (CharAddress);
                              IF CharAddress > Length (EntryString) THEN Exit;
                              END;
                          END;

                ControlX: BEGIN
                          Inc (CharAddress);

                          WHILE EntryString [CharAddress] > ControlZ DO
                              BEGIN
                              User4 := User4 + EntryString [CharAddress];
                              Inc (CharAddress);
                              IF CharAddress > Length (EntryString) THEN Exit;
                              END;
                          END;

                ControlY: BEGIN
                          Inc (CharAddress);

                          WHILE EntryString [CharAddress] > ControlZ DO
                              BEGIN
                              User5 := User5 + EntryString [CharAddress];
                              Inc (CharAddress);
                              IF CharAddress > Length (EntryString) THEN Exit;
                              END;
                          END;

                ELSE Exit; { This should not happen! }
                END;

        UNTIL CharAddress >= Length (EntryString);
        END;
    END;



PROCEDURE CallDatabase.SaveDataToASCIIFile (VAR FileWrite: TEXT;
                                            NewData: DataBaseEntryRecord);

{ Saves the data in NewData to the file opened in FileWrite.  }

    BEGIN
    Write (FileWrite, NewData.Call);

    WITH NewData DO
        BEGIN
        IF Section <> '' THEN Write (FileWrite, ' =A', Section);
        IF CQZone  <> '' THEN Write (FileWrite, ' =C', CQZone);
        IF FOC     <> '' THEN Write (FileWrite, ' =F', FOC);
        IF Grid    <> '' THEN Write (FileWrite, ' =G', Grid);
        IF Hits     >  0 THEN Write (FileWrite, ' =H', Hits);
        IF ITUZone <> '' THEN Write (FileWrite, ' =I', ITUZone);
        IF Check   <> '' THEN Write (FileWrite, ' =K', Check);
        IF Name    <> '' THEN Write (FileWrite, ' =N', Name);
        IF OldCall <> '' THEN Write (FileWrite, ' =O', OldCall);
        IF QTH     <> '' THEN Write (FileWrite, ' =Q', QTH);
        IF Speed    >  0 THEN Write (FileWrite, ' =S', Speed);
        IF TenTen  <> '' THEN Write (FileWrite, ' =T', TenTen);
        IF User1   <> '' THEN Write (FileWrite, ' =U', User1);
        IF User2   <> '' THEN Write (FileWrite, ' =V', User2);
        IF User3   <> '' THEN Write (FileWrite, ' =W', User3);
        IF User4   <> '' THEN Write (FileWrite, ' =X', User4);
        IF User5   <> '' THEN Write (FileWrite, ' =Y', User5);
        END;

    WriteLn (FileWrite);
    END;



PROCEDURE CallDatabase.SaveToASCIIFile;

{ This procedure will save the active file to the file ASCIIFileName.
  It will set the variable ASCIIFileIsCurrent to TRUE }


VAR CellSize: LONGINT;
    FileRead: FILE;
    FileWrite: TEXT;
    X, Y, NextX, NextY, StartingOffset, EndingOffset: LONGINT;
    EntryString: STRING;
    KeyString: CallString;
    DataRecord: DataBaseEntryRecord;

    BEGIN
    IF NOT LoadInIndexArray THEN
        BEGIN
        ReportError ('Unable to save .DTA to ASCII file.');
        Exit;
        END;

    OpenFileForWrite (FileWrite, WorkingDirectory + ActiveASCIIFileName);

    GoToXY (1, WhereY);
    Write ('Saving .DTA file to ASCII file.  Processing cell   ');

    New (CallsAlreadySaved);

    FOR X := 0 TO 36 DO
        FOR Y := 0 TO 36 DO
            BEGIN
            NumberCallsAlreadySaved := 0;

            GoToXY (WhereX - 2, WhereY);
            KeyString := GetSCPCharFromInteger (X) + GetSCPCharFromInteger (Y);
            Write (KeyString);

            StartingOffset := SCPIndexArray^ [X, Y];

            NextY := Y + 1;
            NextX := X;

            IF NextY > 36 THEN
                BEGIN
                IF X < 36 THEN
                    BEGIN
                    NextX := X + 1;
                    NextY := 0;
                    EndingOffset := SCPIndexArray^ [NextX, NextY];
                    END
                ELSE
                    EndingOffset := SCPEndOfFile;
                END
            ELSE
                EndingOffset := SCPIndexArray^ [NextX, NextY];

            IF (X <> 0) OR (Y <> 0) THEN
                BEGIN
                IF FilePos (FileRead) <> StartingOffset THEN
                    BEGIN
                    Close  (FileRead);
                    Assign (FileRead, ActiveFileName);
                    Reset  (FileRead, 1);
                    Seek   (FileRead, StartingOffset);
                    END;
                END
            ELSE
                BEGIN
                Assign (FileRead, ActiveFileName);
                Reset  (FileRead, 1);
                Seek   (FileRead, StartingOffset);
                END;

            IF StartingOffset < EndingOffset THEN
                BEGIN
                CellSize := EndingOffset - StartingOffset;

                IF CellSize > BufferArraySize * 3 THEN
                    BEGIN
                    WriteLn;
                    ReportError (GetSCPCharFromInteger (X) + GetSCPCharFromInteger (Y) + ' cell is too large - 3!!');
                    Halt;
                    END;

                CellBuffer.LoadCellIntoBuffer (KeyString, FileRead, CellSize);

                WHILE CellBuffer.GetNextEntry (EntryString) DO
                    BEGIN
                    ParseEntryToDataRecord (EntryString, DataRecord);

                    IF FirstCellForThisCall (DataRecord.Call, X, Y) THEN
                        IF DoubleIndexCall (DataRecord.Call) THEN
                            BEGIN
                            IF NOT AlreadySaved (DataRecord.Call) THEN
                                SaveDataTOASCIIFile (FileWrite, DataRecord);
                            END
                       ELSE
                           SaveDataTOASCIIFile (FileWrite, DataRecord);
                    END;
                END;
            END;

    Dispose (CallsAlreadySaved);
    CellBuffer.GoAway;

    Close (FileWrite);
    ASCIIFileIsCurrent := True;
    GoToXY (1, WhereY);
    ClrEol;
    TextColor (Cyan);
    WriteLn ('Data successfully saved to ASCII file.');
    END;



PROCEDURE CallDatabase.TransferNewData (OldData: DataBaseEntryRecord; VAR NewData: DataBaseEntryRecord);

{ Moves any data found in OldData into the NewData variable if not found
  in the NewData entry }

    BEGIN
    IF HitsOverwrite AND (OldData.Hits < 255) THEN
        NewData.Hits := OldData.Hits + 1
    ELSE
        NewData.Hits := OldData.Hits;

    WITH NewData DO
        BEGIN
        IF (NOT SectionOverwrite) AND (OldData.Section <> '') OR (NewData.Section = '') THEN Section := OldData.Section;
        IF (NOT CQZoneOverwrite)  AND (OldData.CQZone  <> '') OR (NewData.CQZone  = '') THEN CQZone  := OldData.CQZone;
        IF (NOT FOCOverwrite)     AND (OldData.FOC     <> '') OR (NewData.FOC     = '') THEN FOC     := OldData.FOC;
        IF (NOT GridOverwrite)    AND (OldData.Grid    <> '') OR (NewData.Grid    = '') THEN Grid    := OldData.Grid;
        IF (NOT ITUZoneOverwrite) AND (OldData.ITUZone <> '') OR (NewData.ITUZone = '') THEN ITUZone := OldData.ITUZone;
        IF (NOT CheckOverwrite)   AND (OldData.Check   <> '') OR (NewData.Check   = '') THEN Check   := OldData.Check;
        IF (NOT NameOverwrite)    AND (OldData.Name    <> '') OR (NewData.Name    = '') THEN Name    := OldData.Name;
        IF (NOT OldCallOverwrite) AND (OldData.OldCall <> '') OR (NewData.OldCall = '') THEN OldCall := OldData.OldCall;
        IF (NOT QTHOverwrite)     AND (OldData.QTH     <> '') OR (NewData.QTH     = '') THEN QTH     := OldData.QTH;
        IF (NOT SpeedOverwrite)   AND (OldData.Speed    >  0) OR (NewData.Speed   =  0) THEN Speed   := OldData.Speed;
        IF (NOT TenTenOverwrite)  AND (OldData.TenTen  <> '') OR (NewData.TenTen  = '') THEN TenTen  := OldData.TenTen;
        IF (NOT User1Overwrite)   AND (OldData.User1   <> '') OR (NewData.User1   = '') THEN User1   := OldData.User1;
        IF (NOT User2Overwrite)   AND (OldData.User2   <> '') OR (NewData.User2   = '') THEN User2   := OldData.User2;
        IF (NOT User3Overwrite)   AND (OldData.User3   <> '') OR (NewData.User3   = '') THEN User3   := OldData.User3;
        IF (NOT User4Overwrite)   AND (OldData.User4   <> '') OR (NewData.User4   = '') THEN User4   := OldData.User4;
        IF (NOT User5Overwrite)   AND (OldData.User5   <> '') OR (NewData.User5   = '') THEN User5   := OldData.User5;
        END;
    END;



FUNCTION CallDatabase.PartialCallSetup (PartialCall: CallString): BOOLEAN;

{ Just call this routine, then call GetNextPartialCall until it returns
  nothing }

VAR CharPos: INTEGER;

    BEGIN
    IF NOT LoadInIndexArray THEN
        BEGIN
        PartialCallSetup := False;
        Exit;
        END;

    PartialCallSetup := True;

    FOR CharPos := 1 TO Length (PartialCall) DO
        IF Copy (PartialCall, CharPos, 2) = CellBuffer.Key THEN
            BEGIN   { We have already read in a cell with this call }
            CellBuffer.ReadAddress := 0; { Rewind list }
            InitialPartialCall := PartialCall;
            Exit;
            END;

    NewTwoLetters (PartialCall);
    END;



PROCEDURE CallDatabase.ASCIIFileEditor;

VAR ChangesMade, NeedToSaveLastRecord: BOOLEAN;
    Data: DatabaseEntryRecord;
    Key: CHAR;
    Call: CallString;
    FileWrite: TEXT;

    BEGIN
    ClearDataEntry(Data);//To suppress seems uninitialized note
    ChangesMade := False;
    NeedToSaveLastRecord := False;

    ClearScreenAndTitle ('TRMASTER EDITOR');

    WriteLn ('This editor will allow you to look at the data for any call in the TRMASTER');
    WriteLn ('database.  This editor will convert your .DTA file to an ASCII .ASC file');
    WriteLn ('which it will perform edits to.  When you are done, you will be asked if you');
    WriteLn ('want to save your changes by converting the edited .ASC file to the .DTA file.');
    WriteLn;
    WriteLn ('New calls may be added as well.');
    WriteLn;
    WriteLn ('Note that you will need to save the data back to the .DTA file before you can');
    WriteLn ('see the changes that you have made.  You will be prompted to do this when you');
    WriteLn ('have finished.');
    WriteLn;

    REPEAT
        Key := UpCase (GetKey ('Okay to proceed? (Y/N) : '));
        IF (Key = EscapeKey) OR (Key = 'N') THEN Exit;
    UNTIL Key = 'Y';
    WriteLn;

    IF NOT ASCIIFileIsCurrent THEN SaveToASCIIFile;

    WriteLn;

    REPEAT

        { This screen of code is basically doing the GetResponse function,
          with the added benefit of doing the ASCIIFileCrunch from the
          last entry if neceseary. }

        TextColor (Cyan);
        Write ('Enter call to edit (none to quit): ');
        TextColor (Yellow);

        Call := '';

        REPEAT
            REPEAT
                IF NOT ASCIIFileCrunch THEN
                    IF NeedToSaveLastRecord THEN
                        BEGIN
                        OpenFileForAppend (FileWrite, WorkingDirectory + ActiveASCIIFileName);
                        SaveDataToASCIIFile (FileWrite, Data);
                        Close (FileWrite);
                        CellBuffer.Key := '';
                        NeedToSaveLastRecord := False;
                        END;

            UNTIL KeyPressed;

            Key := UpCase (ReadKey);

            IF Key = CarriageReturn THEN
                BEGIN
                WriteLn;

                IF ASCIIFileCrunch THEN
                    BEGIN
                    TextColor (Yellow);
                    Write ('Please give me a second to finish the last update...');

                    REPEAT millisleep UNTIL NOT ASCIIFileCrunch;
                    TextColor (Cyan);
                    GoToXY (1, WhereY);
                    ClrEol;
                    END;

                IF NeedToSaveLastRecord THEN
                    BEGIN
                    OpenFileForAppend (FileWrite, WorkingDirectory + ActiveASCIIFileName);
                    SaveDataToASCIIFile (FileWrite, Data);
                    Close (FileWrite);
                    CellBuffer.Key := '';
                    NeedToSaveLastRecord := False;
                    END;

                Break;
                END;

             IF Key = BackSpace THEN
                BEGIN
                IF Call <> '' THEN
                    BEGIN
                    GoToXY (WhereX - 1, WhereY);
                    Write (' ');
                    GoToXY (WhereX - 1, WhereY);
                    Delete (Call, Length (Call), 1);
                    END;
                END
            ELSE
                BEGIN
                Write (Key);
                Call := Call + Key;
                END;

        UNTIL Length (Call) >= 12;

        IF NeedToSaveLastRecord THEN
            BEGIN
            OpenFileForAppend (FileWrite, WorkingDirectory + ActiveASCIIFileName);
            SaveDataToASCIIFile (FileWrite, Data);
            Close (FileWrite);
            CellBuffer.Key := '';
            END;

        IF Call = '' THEN
            BEGIN
            IF ChangesMade THEN
                BEGIN
                GoTOXY (1, WhereY);
                ClrEol;

                REPEAT
                    Key := UpCase (GetKey ('Do you want to save your changes to TRMASTER.DTA? (Y/N) : '));
                UNTIL (Key = 'Y') OR (Key = 'N');
                WriteLn;
                IF Key = 'Y' THEN BuildNewDatabaseFromASCIIFile (Chr (0));
                END;

            Exit;
            END;

        GetEntry (Call, Data);

        IF Data.Call = '' THEN
            Data.Call := Call
        ELSE
            BEGIN   { Set up ASCII crunch process }
            ASCIIFileCrunchCallToRemove := Call;
            OpenFileForRead  (ASCIIFileCrunchRead,  WorkingDirectory + ActiveASCIIFileName);
            OpenFileForWrite (ASCIIFileCrunchWrite, WorkingDirectory + TempFileName);
            END;

        REPEAT
            WITH Data DO
                BEGIN
                ClrScr;

                TextColor (Yellow);
                Write ('(C)');
                TextColor (Cyan);
                WriteLn ('all = ', Call);

                TextColor (Yellow);
                Write ('(A)');
                TextColor (Cyan);
                WriteLn ('RRL Section = ', Section);

                Write ('CQ');
                TextColor (Yellow);
                Write ('(Z)');
                TextColor (Cyan);
                WriteLn ('one = ',  CQZone);

                TextColor (Yellow);
                Write ('(F)');
                TextColor (Cyan);
                WriteLn ('OC = ', FOC);

                TextColor (Yellow);
                Write ('(G)');
                TextColor (Cyan);
                WriteLn ('rid = ', Grid);

                TextColor (Yellow);
                Write ('(H)');
                TextColor (Cyan);
                WriteLn ('its = ', Hits);

                TextColor (Yellow);
                Write ('(I)');
                TextColor (Cyan);
                WriteLn ('TUZone = ', ITUZone);

                Write ('Chec');
                TextColor (Yellow);
                Write ('(k)');
                TextColor (Cyan);
                WriteLn (' = ', Check);

                TextColor (Yellow);
                Write ('(N)');
                TextColor (Cyan);
                WriteLn ('ame = ', Name);

                TextColor (Yellow);
                Write ('(O)');
                TextColor (Cyan);
                WriteLn ('ldCall = ', OldCall);

                TextColor (Yellow);
                Write ('(Q)');
                TextColor (Cyan);
                WriteLn ('TH = ', QTH);

                TextColor (Yellow);
                Write ('(S)');
                TextColor (Cyan);
                WriteLn ('peed = ', Speed, '   Zero disables the speed function.');

                TextColor (Yellow);
                Write ('(T)');
                TextColor (Cyan);
                WriteLn ('enTen = ', TenTen);

                TextColor (Cyan);
                Write ('User');
                TextColor (Yellow);
                Write ('(1)');
                TextColor (Cyan);
                WriteLn (' = ', User1);

                TextColor (Cyan);
                Write ('User');
                TextColor (Yellow);
                Write ('(2)');
                TextColor (Cyan);
                WriteLn (' = ', User2);

                TextColor (Cyan);
                Write ('User');
                TextColor (Yellow);
                Write ('(3)');
                TextColor (Cyan);
                WriteLn (' = ', User3);

                TextColor (Cyan);
                Write ('User');
                TextColor (Yellow);
                Write ('(4)');
                TextColor (Cyan);
                WriteLn (' = ', User4);

                TextColor (Cyan);
                Write ('User');
                TextColor (Yellow);
                Write ('(5)');
                TextColor (Cyan);
                WriteLn (' = ', User5);

                WriteLn;

                REPEAT
                    GoToXY (1, WhereY);
                    ClrEol;
                    TextColor (Cyan);
                    Write ('Enter field ID to edit or ESCAPE if done with this call : ');

                    REPEAT
                        ASCIIFileCrunch
                    UNTIL KeyPressed;

                    Key := UpCase (ReadKey);
                    TextColor (Yellow);
                    IF Key >= ' ' THEN Write (Key);
                UNTIL (Key = 'C') OR (Key = 'A') OR (Key = 'Z') OR (Key = 'F') OR
                      (Key = 'G') OR (Key = 'H') OR (Key = 'I') OR (Key = 'K') OR
                      (Key = 'N') OR (Key = 'O') OR (Key = 'Q') OR (Key = 'S') OR
                      (Key = 'T') OR (Key = '1') OR (Key = '2') OR (Key = '3') OR
                      (Key = '4') OR (Key = '5') OR (Key = EscapeKey);

                IF Key <> EscapeKey THEN ChangesMade := True;

                GoToXY (1, WhereY);
                ClrEol;

                CASE Key OF
                    'C': Call    := UpperCase (GetResponse ('Enter new call : '));
                    'A': Section := GetResponse ('Enter new ARRL section : ');
                    'Z': CQZone  := GetResponse ('Enter new CQ zone : ');
                    'F': FOC     := GetResponse ('Enter new FOC number : ');
                    'G': Grid    := GetResponse ('Enter new Grid : ');
                    'H': Hits    := GetValue    ('Enter new hit value (0-255) : ');
                    'I': ITUZone := GetResponse ('Enter new ITU zone : ');
                    'K': Check   := GetResponse ('Enter new check : ');
                    'N': Name    := GetResponse ('Enter new name : ');
                    'O': OldCall := GetResponse ('Enter new old call : ');
                    'Q': QTH     := GetResponse ('Enter new QTH : ');
                    'S': Speed   := GetValue    ('Enter code speed for this station : ');
                    'T': TenTen  := GetResponse ('Enter new TenTen number : ');
                    '1': User1   := GetResponse ('Enter new user 1 value : ');
                    '2': User2   := GetResponse ('Enter new user 2 value : ');
                    '3': User3   := GetResponse ('Enter new user 3 value : ');
                    '4': User4   := GetResponse ('Enter new user 4 value : ');
                    '5': User5   := GetResponse ('Enter new user 5 value : ');

                    EscapeKey:
                        BEGIN
                        IF ASCIIFileCrunch THEN
                            NeedToSaveLastRecord := True
                        ELSE
                            BEGIN
                            OpenFileForAppend (FileWrite, WorkingDirectory + ActiveASCIIFileName);
                            SaveDataToASCIIFile (FileWrite, Data);
                            Close (FileWrite);
                            CellBuffer.Key := '';
                            END;

                        Break;
                        END;
                    END;
                END;
            UNTIL False;
    UNTIL False;
    END;



FUNCTION CallDatabase.ASCIIFileCrunch: BOOLEAN;

{ Reads a line from the ASCII file and if not the call we are working on,
  spits it to the output file.  Returns TRUE if there was something left
  to do.  Returns FALSE when the file is completed transferred. }

VAR FileString: Str80;
    Data: DatabaseEntryRecord;

   BEGIN
   IF ASCIIFileCrunchCallToRemove = '' THEN
       BEGIN
       ASCIIFileCrunch := False;
       Exit;
       END;

   IF Eof (ASCIIFileCrunchRead) THEN
       BEGIN
       Close (ASCIIFileCrunchRead);
       Close (ASCIIFileCrunchWrite);
       DeleteFile (WorkingDirectory + ActiveASCIIFileName);
       RenameFile (WorkingDirectory + TempFileName, WorkingDirectory + ActiveASCIIFileName);
       ASCIIFileCrunchCallToRemove := '';
       ASCIIFileCrunch := False;
       Exit;
       END;

   ReadLn (ASCIIFileCrunchRead, FileString);
   GetRidOfPrecedingSpaces (FileString);

   IF FileString <> '' THEN
       BEGIN
       GetDataFromASCIIEntry (FileString, Data, Chr (0));

       IF Data.Call <> ASCIIFileCrunchCallToRemove THEN
           WriteLn (ASCIIFileCrunchWrite, FileString);
       END;

   ASCIIFileCrunch := True;
   END;



PROCEDURE CallDatabase.TransferMergeDataToASCIIFile (CallUpdate: BOOLEAN;
                                                     SaveNewCalls: BOOLEAN);

{ This procedure will transfer the contents of the linked list of data
  entries found starting at FirstMergeDataListEntry and move the data
  to the ASCII file.  The list will disappear.  Any data found will
  overwrite any old data if the Overwrite flag is set.

  If any calls are found in the DeleteCallList, they will be left off.

  This procedure works a little differently if the CallUpdate flag is
  true.  This is normally set to true when processing something like
  a vanity call file.  The records in the merge list have the new call
  in the call field and the previous call in the old call field.

  First, the procedure will look for any records with the new call.
  If it finds one, it will update the old call field if blank, or if
  the overwrite flag is true.

  The additional step comes now.  The procedure will again look through
  the list for any records for the old call.  If it finds one, it will
  change the call field to the new call and save the data.

  Finally, new records will be added for those entries not matched up
  to existing records in the database.

  Calls not found in the database will be added if the SaveNewCalls flag
  is true. }

LABEL IgnoreThisCall;

VAR ActiveRecord, OldRecord, PreviousRecord, NextRecord: DataListEntryPointer;
    FileString: STRING;
    Data: DatabaseEntryRecord;
    FoundASCIIFile: BOOLEAN;
    Entry: INTEGER;

    BEGIN
    IF (FirstMergeDataListEntry = nil) AND (NumberDeleteCalls = 0) THEN Exit; { Nothing to add }

    { First, we need to suck out the data for the callsigns in the linked
      list - and at the same time, don't put them in the output file.  }

    { Open up the files we will need to process }

    TextColor (Cyan);

    GoToXY (1, WhereY);
    WriteLn ('Merging data from memory with ASCII file into temporary ASCII file...');

    { See if the TRMASTER.ASC file exists - if so, open it up.  If it does
      not exist, then someone must be merging into an empty database.   }

    IF FileExists (WorkingDirectory + ActiveASCIIFileName) THEN
        BEGIN
        OpenFileForRead  (ASCIIFileCrunchRead, WorkingDirectory + ActiveASCIIFileName);
        FoundASCIIFile := True;
        END
    ELSE
        FoundASCIIFile := False;

    { Open up the temporary ASCII file that we will write to }

    OpenFileForWrite (ASCIIFileCrunchWrite, WorkingDirectory + TempFileName);

    { If we have an ASCII file - look through it entry by entry and see
      if we have some data in the merge list for the same station }

    IF FoundASCIIFile THEN
        BEGIN
        WHILE NOT Eof (ASCIIFileCrunchRead) DO
            BEGIN
            { Get entry from .ASC file }

            ReadLn (ASCIIFileCrunchRead, FileString);
            GetRidOfPrecedingSpaces (FileString);

            IF FileString <> '' THEN
                BEGIN
                GetDataFromASCIIEntry (FileString, Data, Chr (0));

                { Check and see if this call is on the delete list }

                IF NumberDeleteCalls > 0 THEN
                    FOR Entry := 0 TO NumberDeleteCalls - 1 DO
                        IF Data.Call = DeleteCallList [Entry] THEN
                           GoTo IgnoreThisCall;

                { Not on the delete list.  Now, we are going to look
                  through the merge list to see if the same call is
                  there.  If so, we will update the entry in Data with
                  any data found in the merge list. }

                ActiveRecord := FirstMergeDataListEntry;
                PreviousRecord := nil;

                WHILE ActiveRecord <> nil DO
                    BEGIN
                    IF ActiveRecord^.Data.Call = Data.Call THEN
                        BEGIN
                        GoToXY (1, WhereY);
                        ClrEol;
                        Write ('Updating data for ', Data.Call);

                        TransferNewData (Data, ActiveRecord^.Data);

                        Data := ActiveRecord^.Data;

                        { Reassign pointers so this record will now be ignored }

                        OldRecord := ActiveRecord;

                        IF PreviousRecord = nil THEN { This is the 1st entry }
                            BEGIN
                            FirstMergeDataListEntry := ActiveRecord^.NextEntry;
                            ActiveRecord := ActiveRecord^.NextEntry;
                            END
                        ELSE
                            BEGIN
                            PreviousRecord^.NextEntry := ActiveRecord^.NextEntry;
                            ActiveRecord := ActiveRecord^.NextEntry;
                            END;

                        Dispose (OldRecord);
                        OldRecord := nil;
                        END
                    ELSE
                        { Skip to next record }

                        BEGIN
                        PreviousRecord := ActiveRecord;
                        ActiveRecord   := ActiveRecord^.NextEntry;
                        END;
                    END;

                { We will now save the entry (either updated or not) to
                  the temporary ASCII file.  }

                SaveDataToASCIIFile (ASCIIFileCrunchWrite, Data);
                END;

        IgnoreThisCall:

            END;

        Close (ASCIIFileCrunchRead);
        END;

    NumberDeleteCalls := 0;

    GoToXY (1, WhereY);
    ClrEol;
    WriteLn ('Existing calls all updated and saved to temporary ASCII file.');



    IF CallUpdate THEN   { Special step for the case of updating callsigns }
        BEGIN
        Close (ASCIIFileCrunchWrite);   { Close the temporary file }

        { Delete old ASCII TRMASTER file and put this new file in its place }

        DeleteFile (WorkingDirectory + ActiveASCIIFileName);
        RenameFile (WorkingDirectory + TempFileName, WorkingDirectory + ActiveASCIIFileName);

        WriteLn ('Second pass - looking for records with old calls...');

        { Open up the files }

        OpenFileForRead  (ASCIIFileCrunchRead, WorkingDirectory + ActiveASCIIFileName);
        OpenFileForWrite (ASCIIFileCrunchWrite, WorkingDirectory + TempFileName);

        WHILE NOT Eof (ASCIIFileCrunchRead) DO
            BEGIN
            { Get entry from .ASC file }

            ReadLn (ASCIIFileCrunchRead, FileString);
            GetRidOfPrecedingSpaces (FileString);

            IF FileString <> '' THEN
                BEGIN
                GetDataFromASCIIEntry (FileString, Data, Chr (0));

                { Update with any data found in the merge list }

                ActiveRecord := FirstMergeDataListEntry;
                PreviousRecord := nil;

                WHILE ActiveRecord <> nil DO
                    BEGIN
                    IF ActiveRecord^.Data.OldCall = Data.Call THEN
                        BEGIN
                        GoToXY (1, WhereY);
                        ClrEol;
                        Write ('Changing ', Data.Call, ' to ', ActiveRecord^.Data.Call);

                        Data.Call := ActiveRecord^.Data.Call;

                        TransferNewData (Data, ActiveRecord^.Data);

                        Data := ActiveRecord^.Data;

                        { Reassign pointers so this record will now be ignored }

                        OldRecord := ActiveRecord;

                        IF PreviousRecord = nil THEN { This is the 1st entry }
                            BEGIN
                            FirstMergeDataListEntry := ActiveRecord^.NextEntry;
                            ActiveRecord := ActiveRecord^.NextEntry;
                            END
                        ELSE
                            BEGIN
                            PreviousRecord^.NextEntry := ActiveRecord^.NextEntry;
                            ActiveRecord := ActiveRecord^.NextEntry;
                            END;

                        Dispose (OldRecord);
                        OldRecord := nil;
                        END;

                    PreviousRecord := ActiveRecord;
                    ActiveRecord   := ActiveRecord^.NextEntry;
                    END;

                SaveDataToASCIIFile (ASCIIFileCrunchWrite, Data);
                END;
            END;

        Close (ASCIIFileCrunchRead);
        END;

    { End of special case when updating callsigns }


    IF SaveNewCalls THEN
        BEGIN
        Write ('Saving new calls to ASCII file...');

        { We still may have entries in the merge list which were for
          callsigns not found in the database.  We need to spit them
          out now. }

        ActiveRecord := FirstMergeDataListEntry;

        WHILE ActiveRecord <> nil DO
            BEGIN
            SaveDataToASCIIFile (ASCIIFileCrunchWrite, ActiveRecord^.Data);
            NextRecord := ActiveRecord^.NextEntry;
            Dispose (ActiveRecord);
            ActiveRecord := NextRecord;
            END;

        { All done spitting out the new callsigns.  Close output file }

        GoToXY (1, WhereY);
        ClrEol;
        WriteLn ('New calls saved to temporary ASCII file.');
        END
    ELSE

        { We need to dispose of the remaining calls in the linked list }

        BEGIN
        ActiveRecord := FirstMergeDataListEntry;

        WHILE ActiveRecord <> nil DO
            BEGIN
            NextRecord := ActiveRecord^.NextEntry;
            Dispose (ActiveRecord);
            ActiveRecord := NextRecord;
            END;
        END;

    Close (ASCIIFileCrunchWrite);

    { Rename temp ASCII file to be new ASCII file }

    Write ('Deleting old ASCII file & renaming temp file to ', WorkingDirectory + ActiveASCIIFileName, '...');

    DeleteFile (WorkingDirectory + ActiveASCIIFileName);
    RenameFile (WorkingDirectory + TempFileName, WorkingDirectory + ActiveASCIIFileName);

    GoToXY (1, WhereY);
    ClrEol;

    WriteLn ('Old ASCII file deleted & temp file renamed to ', WorkingDirectory + ActiveASCIIFileName, '.');

    { Make sure initial entry is nil }

    FirstMergeDataListEntry := nil;
    END;



PROCEDURE CallDatabase.AddRecordToMergeList (Data: DatabaseEntryRecord);

VAR ActiveRecord: DataListEntryPointer;

    BEGIN
    IF FirstMergeDataListEntry = nil THEN
        BEGIN
        FirstMergeDataListEntry := New (DataListEntryPointer);
        FirstMergeDataListEntry^.NextEntry := nil;
        FirstMergeDataListEntry^.Data      := Data;
        Exit;
        END;

    { We have some entries already - make sure it isn't a dupe }

    ActiveRecord := FirstMergeDataListEntry;

    REPEAT
        IF ActiveRecord^.Data.Call = Data.Call THEN Exit; { dupe - ignore it }
        IF ActiveRecord^.NextEntry = nil THEN Break;
        ActiveRecord := ActiveRecord^.NextEntry;
    UNTIL False;

    { Okay, this call does not appear in the merge list, so add it }

    ActiveRecord^.NextEntry := New (DataListEntryPointer);
    ActiveRecord := ActiveRecord^.NextEntry;
    ActiveRecord^.NextEntry := nil;
    ActiveRecord^.Data := Data;
    END;



FUNCTION CallDatabase.OverwriteFlagStatus: BOOLEAN;

VAR Key: CHAR;

    BEGIN
    REPEAT
        ClrScr;
        TextColor (Yellow);
        WriteLnCenter ('OVERWRITE FLAG STATUS');
        WriteLn;

        TextColor (Yellow);
        Write ('(A)');
        TextColor (Cyan);
        Write ('RRL Section = ', SectionOverwrite);

        GoToXY (30, WhereY);
        WriteLn ('The flags listed determine what will');

        Write ('      CQ');
        TextColor (Yellow);
        Write ('(Z)');
        TextColor (Cyan);
        Write ('one = ', CQZoneOverwrite);

        GoToXY (30, WhereY);
        WriteLn ('happen if there is data found in the');

        TextColor (Yellow);
        Write ('         (F)');
        TextColor (Cyan);
        Write ('OC = ', FOCOverwrite);

        GoToXY (30, WhereY);
        WriteLn ('file being processed and there is');

        TextColor (Yellow);
        Write ('        (G)');
        TextColor (Cyan);
        Write ('rid = ', GridOverwrite);

        GoToXY (30, WhereY);
        WriteLn ('existing data for the same field');

        TextColor (Yellow);
        Write ('        (H)');
        TextColor (Cyan);
        Write ('its = ', HitsOverwrite);

        GoToXY (30, WhereY);
        WriteLn ('already in the database.  If the flag');

        TextColor (Yellow);
        Write ('     (I)');
        TextColor (Cyan);
        Write ('TUZone = ', ITUZoneOverwrite);

        GoToXY (30, WhereY);
        WriteLn ('is TRUE, the data will be updated to');

        Write ('       Chec');
        TextColor (Yellow);
        Write ('(k)');
        TextColor (Cyan);
        Write (' = ', CheckOverwrite);

        GoToXY (30, WhereY);
        WriteLn ('value found in the file.  ');

        TextColor (Yellow);
        Write ('        (N)');
        TextColor (Cyan);
        Write ('ame = ', NameOverwrite);

        GoToXY (30, WhereY);
        WriteLn;

        TextColor (Yellow);
        Write ('     (O)');
        TextColor (Cyan);
        Write ('ldCall = ', OldCallOverwrite);

        GoToXY (30, WhereY);
        WriteLn ('The H flag determines if the hits count');

        TextColor (Yellow);
        Write ('         (Q)');
        TextColor (Cyan);
        Write ('TH = ', QTHOverwrite);

        GoToXY (30, WhereY);
        WriteLn ('is incremented for each log a call is');

        TextColor (Yellow);
        Write ('       (S)');
        TextColor (Cyan);
        Write ('peed = ', SpeedOverwrite);

        GoToXY (30, WhereY);
        WriteLn ('found in.  You can later filter out the');

        TextColor (Yellow);
        Write ('      (T)');
        TextColor (Cyan);
        Write ('enTen = ', TenTenOverwrite);

        GoToXY (30, WhereY);
        WriteLn ('calls that only appear in 1 or 2 logs.');

        TextColor (Cyan);
        Write ('       User');
        TextColor (Yellow);
        Write ('(1)');
        TextColor (Cyan);
        Write (' = ', User1Overwrite);

        GoToXY (30, WhereY);
        WriteLn;

        TextColor (Cyan);
        Write ('       User');
        TextColor (Yellow);
        Write ('(2)');
        TextColor (Cyan);
        Write (' = ', User2Overwrite);

        GoToXY (30, WhereY);
        WriteLn ('To change the value of one of the flags,');

        TextColor (Cyan);
        Write ('       User');
        TextColor (Yellow);
        Write ('(3)');
        TextColor (Cyan);
        Write (' = ', User3Overwrite);

        GoToXY (30, WhereY);
        WriteLn ('just press the key of the highlighted');

        TextColor (Cyan);
        Write ('       User');
        TextColor (Yellow);
        Write ('(4)');
        TextColor (Cyan);
        Write (' = ', User4Overwrite);

        GoToXY (30, WhereY);
        WriteLn ('letter.');

        TextColor (Cyan);
        Write ('       User');
        TextColor (Yellow);
        Write ('(5)');
        TextColor (Cyan);
        WriteLn (' = ', User5Overwrite);

        WriteLn;

        REPEAT
            Key := UpCase (GetKey ('Enter field ID to change, RETURN to start or ESCAPE to exit : '));

            IF Key = EscapeKey THEN
                BEGIN
                OverwriteFlagStatus := False;
                Exit;
                END;

            IF Key = CarriageReturn THEN
                BEGIN
                OverwriteFlagStatus := True;
                Exit;
                END;

        UNTIL (Key = 'S') OR (Key = 'Z') OR (Key = 'F') OR (Key = 'G') OR
              (Key = 'I') OR (Key = 'K') OR (Key = 'N') OR (Key = 'Q') OR
              (Key = '1') OR (Key = '2') OR (Key = '3') OR (Key = '4') OR
              (Key = '5') OR (Key = 'T') OR (Key = 'H') OR (Key = 'A');

        CASE Key OF
            'A': SectionOverwrite := NOT SectionOverwrite;
            'Z': CQZoneOverwrite  := NOT CQZoneOverwrite;
            'F': FOCOverwrite     := NOT FOCOverwrite;
            'G': GridOverwrite    := NOT GridOverwrite;
            'H': HitsOverwrite    := NOT HitsOverwrite;
            'I': ITUZoneOverwrite := NOT ITUZoneOverwrite;
            'K': CheckOverwrite   := NOT CheckOverwrite;
            'N': NameOverwrite    := NOT NameOverwrite;
            'O': OldCallOverwrite := NOT OldCallOverwrite;
            'Q': QTHOverwrite     := NOT QTHOverwrite;
            'S': SpeedOverwrite   := NOT SpeedOverwrite;
            'T': TenTenOverwrite  := NOT TenTenOverwrite;
            '1': User1Overwrite   := NOT User1Overwrite;
            '2': User2Overwrite   := NOT User2Overwrite;
            '3': User3Overwrite   := NOT User3Overwrite;
            '4': User4Overwrite   := NOT User4Overwrite;
            '5': User5Overwrite   := NOT User5Overwrite;
            END;

    UNTIL False;
    END;



PROCEDURE CallDatabase.DeleteLowHitCalls;

VAR Hits, Threshold, xResult: INTEGER;
    SaveNames: BOOLEAN;
    Key: CHAR;
    CallsFound, CallsSaved: LONGINT;
    TempString, FileString: STRING;
    FileRead, FileWrite: TEXT;

    BEGIN
    ClearScreenAndTitle ('DELETE CALLSIGNS WITH HIT COUNTS BELOW THRESHOLD');

    WriteLn ('This procedure will delete callsigns in the database which have hit values');
    WriteLn ('less then a certain threshold.  The hit value is normally incremented ');
    WriteLn ('for each call already in the database when a file that is being loaded into');
    WriteLn ('the database contains the call.  This purpose of this hit count is to allow');
    WriteLn ('you to remove calls that may have only appeared in one or two files, so you');
    WriteLn ('can have a high degree of confidence that the remaining calls are error free.');
    WriteLn;
    WriteLn ('You have the option to retain calls with names associated with them,');
    WriteLn ('regardless of their hit count.');
    WriteLn;

    REPEAT
        Key := UpCase (GetKey ('Keep entries with names regardless of hits? (Y/N or escape) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'Y') OR (Key = 'N');
    WriteLn;

    SaveNames := Key = 'Y';

    Threshold := GetValue ('Enter minimum hits for calls to keep (zero to exit) : ');
    IF Threshold = 0 THEN Exit;

    IF NOT ASCIIFileIsCurrent THEN SaveToASCIIFile;

    CallsFound := 0;
    CallsSaved := 0;

    IF OpenFileForRead (FileRead, WorkingDirectory + ActiveASCIIFileName) THEN
        BEGIN
        OpenFileForWrite (FileWrite, WorkingDirectory + TempFileName);

        WHILE NOT Eof (FileRead) DO
            BEGIN
            ReadLn (FileRead, FileString);

            Inc (CallsFound);

            IF StringHas (FileString, '=H') THEN
                BEGIN
                TempString := PostcedingString (FileString, '=H');
                TempString := RemoveFirstString (TempString);
                Val (TempString, Hits, xResult);

                IF (Hits >= Threshold) THEN
                    BEGIN
                    WriteLn (FileWrite, FileString);
                    Inc (CallsSaved);
                    Continue;
                    END;
                END;

            IF SaveNames AND StringHas (FileString, '=N') THEN
                BEGIN
                WriteLn (FileWrite, FileString);
                Inc (CallsSaved);
                END;
            END;

        Close (FileRead);
        Close (FileWrite);

        WriteLn ('Out of ', CallsFound, ' database calls, ', CallsSaved, ' calls would be left.');

        REPEAT
            Key := UpCase (GetKey ('Do you want to build a new database with these changes? (Y/N) : '));
            IF Key = EscapeKey THEN Exit;
        UNTIL (Key = 'Y') OR (Key = 'N');
        WriteLn;

        IF Key = 'Y' THEN
            BEGIN
            DeleteFile (WorkingDirectory + ActiveASCIIFileName);
            RenameFile (WorkingDirectory + TempFileName, WorkingDirectory + ActiveASCIIFileName);
            BuildNewDatabaseFromASCIIFile (Chr (0));
            END;
        END;
    END;



PROCEDURE CallDatabase.MovePossibleCallsFromBufferIntoCallList (Call: CallString);

LABEL CallFound;

VAR EntryString: STRING;
    Address: INTEGER;
    Data: DataBaseEntryRecord;

    BEGIN
    IF CellBuffer.NumberBufferEntries = 0 THEN Exit;  { No calls to move }

    CellBuffer.ReadAddress := 0; { Rewind list }

    WHILE CellBuffer.GetNextEntry (EntryString) DO
        BEGIN
        ParseEntryToDataRecord (EntryString, Data);

        IF SimilarCall (Call, Data.Call) THEN
            IF (PossibleCallAction <> OnlyCallsWithNames) OR (Data.Name <> '') THEN
                IF PossibleCallList.NumberPossibleCalls < MaximumPossibleCalls THEN
                    BEGIN
                    IF PossibleCallList.NumberPossibleCalls > 0 THEN
                        FOR Address := 0 TO PossibleCallList.NumberPossibleCalls - 1 DO
                            IF PossibleCallList.List [Address].Call = Data.Call THEN
                                GoTo CallFound;

                    { Call not in list and there is room for it }

                    WITH PossibleCallList DO
                        BEGIN
                        List [NumberPossibleCalls].Call := Data.Call;
                        List [NumberPossibleCalls].Dupe := False;
                        Inc (NumberPossibleCalls);
                        END;
                    END;

        CallFound:
        END;
   END;



PROCEDURE CallDatabase.GeneratePossibleCallList (Call: CallString);

{ This procedure will generate a list of possible callsigns for the one
  entered.  The list will be stored in the PossibleCalls list with
  NumberPossibleCalls indicating the number of callsigns found. }

VAR FirstKeyPos, SecondKeyPos, CharPos: INTEGER;
    LowestCell, SecondLowestCell, ThirdLowestCell, BytesAtThisPos: LONGINT;
    LowestKeyPos, SecondLowestKeyPos, ThirdLowestKeyPos: INTEGER;
    TempDupe, Change: BOOLEAN;
    Range, CallAddress: INTEGER;
    TempString: CallString;

    BEGIN
    SecondLowestKeyPos := 0; //to suppress does not seem to be initialized
    LowestKeyPos := 0; //to suppress does not seem to be initialized
    PossibleCallList.NumberPossibleCalls := 0;

    IF NOT LoadInIndexArray THEN Exit;

    FirstKeyPos := 0;

    LowestCell       := AVeryBigNumber;
    SecondLowestCell := AVeryBigNumber;
    ThirdLowestCell  := AVeryBigNumber;

    FOR CharPos := 1 TO Length (Call) - 1 DO
        BEGIN
        BytesAtThisPos := NumberOfBytesAtThisAddress (Copy (Call, CharPos, 2));

        IF BytesAtThisPos < LowestCell THEN
            BEGIN
            ThirdLowestCell   := SecondLowestCell;
            ThirdLowestKeyPos := SecondLowestKeyPos;

            SecondLowestCell   := LowestCell;
            SecondLowestKeyPos := LowestKeyPos;

            LowestCell   := BytesAtThisPos;
            LowestKeyPos := CharPos;
            END
        ELSE
            IF BytesAtThisPos < SecondLowestCell THEN
                BEGIN
                ThirdLowestCell   := SecondLowestCell;
                ThirdLowestKeyPos := SecondLowestKeyPos;

                SecondLowestCell   := BytesAtThisPos;
                SecondLowestKeyPos := CharPos;
                END
            ELSE
                IF BytesAtThisPos < ThirdLowestCell THEN
                    BEGIN
                    ThirdLowestCell   := BytesAtThisPos;
                    ThirdLowestKeyPos := CharPos;
                    END;
        END;

    FOR CharPos := 1 TO Length (Call) - 1 DO
        IF Copy (Call, CharPos, 2) = CellBuffer.Key THEN
            BEGIN   { We have already read in a cell for this call }
            MovePossibleCallsFromBufferIntoCallList (Call);
            FirstKeyPos := CharPos;
            Break;
            END;

    IF FirstKeyPos = 0 THEN          { Current buffer was no help }
        BEGIN
        IF Abs (LowestKeyPos - SecondLowestKeyPos) >= 2 THEN
            BEGIN
            FirstKeyPos  := LowestKeyPos;
            SecondKeyPos := SecondLowestKeyPos;
            END
        ELSE
            IF Abs (LowestKeyPos - ThirdLowestKeyPos) >= 2 THEN
                BEGIN
                FirstKeyPos  := LowestKeyPos;
                SecondKeyPos := ThirdLowestKeyPos;
                END
            ELSE
                BEGIN
                FirstKeyPos  := SecondLowestKeyPos;
                SecondKeyPos := ThirdLowestKeyPos;
                END;

        NewTwoLetters (Copy (Call, FirstKeyPos, 2));
        MovePossibleCallsFromBufferIntoCallList (Call);
        END
    ELSE
        IF Abs (FirstKeyPos - LowestKeyPos) >= 2 THEN
            SecondKeyPos := LowestKeyPos
        ELSE
            IF Abs (FirstKeyPos - SecondLowestKeyPos) >= 2 THEN
                SecondKeyPos := SecondLowestKeyPos
            ELSE
                SecondKeyPos := ThirdLowestKeyPos;

    NewTwoLetters (Copy (Call, SecondKeyPos, 2));
    MovePossibleCallsFromBufferIntoCallList (Call);

    { Sort the calls into alphabetical order }

    IF PossibleCallList.NumberPossibleCalls < 2 THEN Exit;  { No sorting needed }

    Range := PossibleCallList.NumberPossibleCalls - 2;

    REPEAT
        WITH PossibleCallList DO
            BEGIN
            Change := False;

            FOR CallAddress := 0 TO Range DO
                IF List [CallAddress].Call > List [CallAddress + 1].Call THEN
                   BEGIN
                   TempString := List [CallAddress].Call;
                   List [CallAddress].Call := List [CallAddress + 1].Call;
                   List [CallAddress + 1].Call := TempString;

                   TempDupe := List [CallAddress].Dupe;
                   List [CallAddress].Dupe := List [CallAddress + 1].Dupe;
                   List [CallAddress].Dupe := TempDupe;

                   Change := True;
                   END;
            END;

        Dec (Range);
    UNTIL (NOT Change) OR (Range < 0);
    END;



PROCEDURE CallDatabase.SortDTAFile;

{ This routine will sort the .DTA file cell by cell, so that the callsigns
  in each cell are in alphabetical order. }

VAR FileRead, FileWrite: FILE;
    Count, EntrySize, NumberPaddingBytes, ByteDifference, Entry: INTEGER;
    EntryArray, TempArray, NextArray: EntryArrayPtr;
    TempChar: CHAR;

    X, Y, NextX, NextY: INTEGER;
    NumberBytes, StartingOffset, EndingOffset: LONGINT;

    WriteTempArray, WriteEnable: BOOLEAN;
    KeyString, EntryCall, NextCall: CallString;
    NextEntryAddress: POINTER;

    BEGIN
    temparray := nil; //kill uninitialized warning
    ClearScreenAndTitle ('SORT TRMASTER FILE');

    WriteLn ('This routine will sort the callsigns in the TRMASTER file so they will appear');
    WriteLn ('in alphabetical order in the partial call display.');
    WriteLn;
    WriteLn ('Please note - that this does not sort your TRMASTER.ASC file into alphabetical');
    WriteLn ('order.  Instead, it sorts calls into order within the 1369 different cells.');
    WriteLn ;
    WriteLn ('This procedure will also merge any duplicate call entries into one withing a');
    WriteLn ('cell.  This will show up as a reduction in your TRMASTER.DTA file size only ');
    WriteLn ('after you convert it to an ASCII file and then build it back to a .DTA file.');
    WriteLn;

    IF NOT FileExists (ActiveFileName) THEN
        BEGIN
        ReportError ('No ' + ActiveFileName + ' file found to sort!!');
        WaitForKeyPressed;
        Exit;
        END;

    IF NOT IndexArrayAllocated THEN
        BEGIN
        IF SizeOf (SCPIndexArray^) >= (MaxAvail - 60000) THEN
            BEGIN
            ReportError ('Insufficient memory to run this procedure!!');
            WaitForKeyPressed;
            Exit;
            END;

        New (SCPIndexArray);
        IndexArrayAllocated := True;
        END;

    { Open file for read }

    Assign (FileRead, ActiveFileName);
    Reset  (FileRead, 1);

    { Read in index array }

    BlockRead (FileRead, SCPIndexArray^, SizeOf (SCPIndexArray^));
    BlockRead (FileRead, SCPEndOfFile,   SizeOf (SCPEndOfFile));

    { Open output file }

    Assign  (FileWrite, WorkingDirectory + TempFileName);
    ReWrite (FileWrite, 1);

    BlockWrite (FileWrite, SCPIndexArray^, SizeOf (SCPIndexArray^));
    BlockWrite (FileWrite, SCPEndOfFile, SizeOf (SCPEndOfFile));

    IF SizeOf (EntryPointerList^) >= MaxAvail THEN Exit;

    New (EntryPointerList);
    NumberPaddingBytes := 0;

    FOR X := 0 TO 36 DO
        FOR Y := 0 TO 36 DO
            BEGIN
            GoToXY (1, WhereY);
            ClrEol;
            Write ('Sorting cell ', GetSCPCharFromInteger (X), GetSCPCharFromInteger (Y));

            KeyString := GetSCPCharFromInteger (X) + GetSCPCharFromInteger (Y);

            StartingOffset := SCPIndexArray^ [X, Y];

            { Figure out starting address of the next cell }

            NextY := Y + 1;
            NextX := X;

            IF NextY > 36 THEN
                BEGIN
                IF X < 36 THEN
                    BEGIN
                    NextX := X + 1;
                    NextY := 0;
                    EndingOffset := SCPIndexArray^ [NextX, NextY];
                    END
                ELSE
                    EndingOffset := SCPEndOfFile;
                END
            ELSE
                EndingOffset := SCPIndexArray^ [NextX, NextY];

            { See if there is something to work with - and if so - check
              to make sure enough memory is available to read it in }

            IF StartingOffset < EndingOffset THEN
                BEGIN
                NumberBytes := EndingOffset - StartingOffset;

                CellBuffer.LoadCellIntoBuffer (KeyString, FileRead, NumberBytes);

                { Initialize the address for the pointer array }

                NumberEntries := 0;

                WHILE CellBuffer.GetNextEntryAddress (NextEntryAddress) DO
                    BEGIN
                    EntryPointerList^ [NumberEntries] := NextEntryAddress;
                    Inc (NumberEntries);
                    END;

                { If we have more than one entry - do the head sort }

                IF NumberEntries > 1 THEN
                    HeapSort (EntryPointerList, NumberEntries);

                { Write each record to the .DTA file }

                { We use WriteTempArray if we are combining two or more
                  records that have the same callsign.  We need to make
                  sure it is disabled to start with. }

                WriteTempArray := False;

                { Go through the sorted list of pointers and pull the data
                  out of the FileBuffer and spit it out to the output file.
                  Some tricky stuff happens if we find two or more records
                  with the same call to produce only one output record. }

                FOR Entry := 0 TO NumberEntries - 1 DO
                    BEGIN
                    EntryArray := EntryPointerList^ [Entry];
                    EntrySize  := GetSize (EntryArray);
                    EntryCall  := GetCall (EntryArray);

                    { If this isn't the last entry - then peek ahead at
                      the next call. }

                    IF Entry < NumberEntries - 1 THEN
                        BEGIN
                        NextArray := EntryPointerList^ [Entry + 1];
                        NextCall  := GetCall (NextArray);

                        { If the next call and the current entry call are the
                          same, then we need to merge the two records into one. }

                        IF NextCall = EntryCall THEN
                            BEGIN

                        { If we have WriteTempArray = TRUE already, then we have
                          already combined two or more records into TempArray.
                          We need to reference TempArray instead of EntryArray }

                            IF WriteTempArray THEN
                                BEGIN
                                EntrySize := GetSize (TempArray);

                                { Merge TempArray with NextArray >= TempArray }

                                MergeArrays (TempArray, NextArray, TempArray);
                                END
                            ELSE

                                { This is the first duplicate found - create
                                  TempArray and merge the two records into it }

                                BEGIN
                                New (TempArray);
                                MergeArrays (EntryArray, NextArray, TempArray);
                                WriteTempArray := True;
                                WriteEnable := False;
                                END;

                            { Compute the number of bytes this merge saved  so we
                              can add a bunch of nulls to the end of the cell.
                              Otherwise, or index table will not match the
                              resulting data }

                            ByteDifference := EntrySize + GetSize (NextArray) - GetSize (TempArray);

                            { Keep running total of padding bytes }

                            NumberPaddingBytes := NumberPaddingBytes + ByteDifference;
                            END
                        ELSE
                            WriteEnable := True;   { Next call is different }

                        END

                    ELSE
                        WriteEnable := True;  { There are no next calls }

                    IF WriteEnable THEN
                        BEGIN
                        IF WriteTempArray THEN
                            BEGIN
                            BlockWrite (FileWrite, TempArray^, GetSize (TempArray));
                            Dispose (TempArray);
                            WriteTempArray := False;
                            END
                        ELSE
                            BlockWrite (FileWrite, EntryArray^, EntrySize);
                        END;
                    END;

                { Write any padding bytes }

                TempChar := Chr (0);

                IF NumberPaddingBytes > 0 THEN
                    BEGIN
                    FOR Count := 1 TO NumberPaddingBytes DO
                        BlockWrite (FileWrite, TempChar, 1);

                    NumberPaddingBytes := 0;
                    END;
                END;
            END;

    Dispose (EntryPointerList);
    EntryPointerList := nil;
    Close (FileWrite);

    RenameFile (WorkingDirectory + TempFileName, ActiveFileName);
    GoToXY (1, WhereY);
    ClrEol;
    END;



FUNCTION CallDatabase.GetRandomCall: CallString;

VAR Call: Callstring;

    BEGIN
    REPEAT
        Call := RandomCall;
    UNTIL NOT StringIsAllNumbers (Call);

    GetRandomCall := Call;
    END;



FUNCTION CallDatabase.RandomCall: CallString;



VAR NextX, NextY, X, Y: WORD;
    StartingOffset, Address, EndingOffset, Range: LONGINT;
    MiniBuffer: ARRAY [0..63] OF CHAR;
    Call: CallString;

    BEGIN
    IF NOT LoadInIndexArray THEN
        BEGIN
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

        Exit;
        END;

    X := Random (37);
    Y := Random (36);

    StartingOffset := SCPIndexArray^ [X, Y];

    NextY := Y + 1;
    NextX := X;

    IF NextY > 36 THEN
        BEGIN
        IF X < 36 THEN
            BEGIN
            NextX := X + 1;
            NextY := 0;
            EndingOffset := SCPIndexArray^ [NextX, NextY];
            END
        ELSE
            EndingOffset := SCPEndOfFile;
        END
    ELSE
        EndingOffset := SCPIndexArray^ [NextX, NextY];

    Range := EndingOffset - StartingOffset;

    WHILE Range > 65000 DO Range := Range - 32000;

    Address := Random (Range) + StartingOffset;

    IF Address + SizeOf (MiniBuffer) >= FileSize (TRMasterFileRead) THEN
        Address := (FileSize (TRMasterFileRead) - SizeOf (MiniBuffer)) - 5;

    Seek (TRMasterFileRead, Address);

    BlockRead (TRMasterFileRead, MiniBuffer, SizeOf (MiniBuffer));

    Address := 0;

    WHILE MiniBuffer [Address] <> NullCharacter DO
        BEGIN
        Inc (Address);

        IF Address = 64 THEN
            BEGIN
            RandomCall := 'N6TR';
            Exit;
            END;

        END;

    Inc (Address);

    Call := '';

    WHILE MiniBuffer [Address] > ControlZ DO
        BEGIN
        Call := Call + MiniBuffer [Address];
        Inc (Address);

        IF Address = 64 THEN
            BEGIN
            WHILE Length (Call) < 4 DO
                Call := Call + 'A';

            RandomCall := Call;
            Exit;
            END;
        END;

    RandomCall := Call;
    END;



PROCEDURE CallDatabase.SCPDisableAndDeAllocateFileBuffer;

    BEGIN
{   IF FileBuffer    <> nil THEN Dispose (FileBuffer);    6.43 }

    IF TRMasterFileOpen THEN
        BEGIN
        Close (TRMasterFileRead);
        Dispose (SCPIndexArray);
        TRMasterFileOpen := False;
        IndexArrayAllocated := False;
        END;

    SCPDisabledByApplication := True;
    END;



PROCEDURE CallDatabase.ShowStatistics;


VAR FileRead: FILE;
    X, Y, Entry, NextX, NextY, StartingOffset, EndingOffset: LONGINT;
    EntryString: STRING;
    DataRecord: DataBaseEntryRecord;

    TotalNames, TotalGrids, TotalSections, TotalCQZones, TotalFocs: LONGINT;
    TotalITUZones, TotalCalls, TotalChecks, TotalQTHs, TotalTenTens: LONGINT;
    NumberBytes, TotalOldCalls, TotalSpeeds: LONGINT;
    TotalUser1s, TotalUser2s, TotalUser3s, TotalUser4s, TotalUser5s: LONGINT;
    HitList: ARRAY [0..51] OF LONGINT;
    KeyString: Str20;

    BEGIN
    ClearScreenAndTitle ('TRMASTER DATABASE STATISTICS');

    Write ('Computing totals for cell AA');

    IF SCPIndexArray = nil THEN         { Make sure index array is loaded }
        IF NOT LoadInIndexArray THEN
            BEGIN
            GoToXY (1, WhereY);
            ClrEol;
            WriteLn ('No TRMASTER.DTA file found.');
            WaitForKeyPressed;
            Exit;
            END;

    { Initialize counts }

    TotalCalls    := 0;
    TotalNames    := 0;
    TotalGrids    := 0;
    TotalSections := 0;
    TotalCQZones  := 0;
    TotalFocs     := 0;
    TotalITUZones := 0;
    TotalChecks   := 0;
    TotalOldCalls := 0;
    TotalQTHs     := 0;
    TotalTenTens  := 0;
    TotalSpeeds   := 0;
    TotalUser1s   := 0;
    TotalUser2s   := 0;
    TotalUser3s   := 0;
    TotalUser4s   := 0;
    TotalUser5s   := 0;

    FOR Entry := 0 TO 51 DO HitList [Entry] := 0;

    FOR X := 0 TO 36 DO
        FOR Y := 0 TO 36 DO
            BEGIN
            GoToXY (WhereX - 2, WhereY);
            KeyString := GetSCPCharFromInteger (X) + GetSCPCharFromInteger (Y);
            Write (KeyString);

            StartingOffset := SCPIndexArray^ [X, Y];

            NextY := Y + 1;
            NextX := X;

            IF NextY > 36 THEN
                BEGIN
                IF X < 36 THEN
                    BEGIN
                    NextX := X + 1;
                    NextY := 0;
                    EndingOffset := SCPIndexArray^ [NextX, NextY];
                    END
                ELSE
                    EndingOffset := SCPEndOfFile;
                END
            ELSE
                EndingOffset := SCPIndexArray^ [NextX, NextY];

            IF (X <> 0) OR (Y <> 0) THEN
                BEGIN
                IF FilePos (FileRead) <> StartingOffset THEN
                    BEGIN
                    Close  (FileRead);
                    Assign (FileRead, ActiveFileName);
                    Reset  (FileRead, 1);
                    Seek   (FileRead, StartingOffset);
                    END;
                END
            ELSE
                BEGIN
                Assign (FileRead, ActiveFileName);
                Reset  (FileRead, 1);
                Seek   (FileRead, StartingOffset);
                END;

            IF StartingOffset < EndingOffset THEN
                BEGIN
                NumberBytes := EndingOffset - StartingOffset;

                IF NumberBytes > BufferArraySize * 3 THEN
                    BEGIN
                    WriteLn;
                    ReportError (GetSCPCharFromInteger (X) + GetSCPCharFromInteger (Y) + ' cell is too large - 5!!');
                    Halt;
                    END;

                CellBuffer.LoadCellIntoBuffer (KeyString, FileRead, NumberBytes);

                WHILE CellBuffer.GetNextEntry (EntryString) DO
                    BEGIN
                    ParseEntryToDataRecord (EntryString, DataRecord);

                    IF FirstCellForThisCall (DataRecord.Call, X, Y) THEN
                        BEGIN
                        Inc (TotalCalls);

                        WITH DataRecord DO
                            BEGIN
                            IF Name    <> '' THEN Inc (TotalNames);
                            IF Grid    <> '' THEN Inc (TotalGrids);
                            IF Section <> '' THEN Inc (TotalSections);
                            IF CQZone  <> '' THEN Inc (TotalCQZones);
                            IF FOC     <> '' THEN Inc (TotalFocs);
                            IF ITUZone <> '' THEN Inc (TotalITUZones);
                            IF Check   <> '' THEN Inc (TotalChecks);
                            IF QTH     <> '' THEN Inc (TotalQTHs);
                            IF OldCall <> '' THEN Inc (TotalOldCalls);
                            IF Speed    >  0 THEN Inc (TotalSpeeds);
                            IF TenTen  <> '' THEN Inc (TotalTenTens);
                            IF User1   <> '' THEN Inc (TotalUser1s);
                            IF User2   <> '' THEN Inc (TotalUser2s);
                            IF User3   <> '' THEN Inc (TotalUser3s);
                            IF User4   <> '' THEN Inc (TotalUser4s);
                            IF User5   <> '' THEN Inc (TotalUser5s);

                            IF Hits <= 50 THEN
                                Inc (HitList [Hits])
                            ELSE
                                Inc (HitList [51]);
                            END;
                        END;
                    END;
                END;
            END;

    GoToXY (1, WhereY);
    ClrEol;

    Write   ('Total calls = ', TotalCalls);
    GoToXY (40, WhereY);
    WriteLn ('Total QTHs = ', TotalQTHs);

    Write   ('Total Names = ', TotalNames);
    GoToXY (40, WhereY);
    WriteLn ('Total Grids = ', TotalGrids);

    Write   ('Total CQ Zones = ', TotalCQZones);
    GoToXY (40, WhereY);
    WriteLn ('Total ITU Zones = ', TotalITUZones);

    Write   ('Total Checks = ', TotalChecks);
    GoToXY (40, WhereY);
    WriteLn ('Total Sections = ', TotalSections);

    Write   ('Total TenTens = ', TotalTenTens);
    GoToXY (40, WhereY);
    WriteLn ('Total FOCs = ', TotalFOCs);

    Write   ('Total User1s = ', TotalUser1s);
    GoToXY (40, WhereY);
    WriteLn ('Total User2s = ', TotalUser2s);

    Write   ('Total User3s = ', TotalUser3s);
    GoToXY (40, WhereY);
    WriteLn ('Total User4s = ', TotalUser4s);

    Write   ('Total User5s = ', TotalUser5s);
    GoToXY (40, WhereY);
    WriteLn ('Total CW Speeds = ', TotalSpeeds);

    WriteLn ('Total OldCalls = ', TotalOldCalls);

    WriteLn;

    WriteLn ('Hit count histogram for 0 to 50+ : ');

    FOR Entry := 0 TO 51 DO
        BEGIN
        IF WhereX > 68 THEN WriteLn;

        IF Entry < 51 THEN
            Write (Entry:3, ': ', HitList [Entry])
        ELSE
            Write ('More than 50 = ', HitList [51]);

        WHILE ((WhereX MOD 12) <> 0) AND (WhereX < 77) DO Write (' ');
        END;

    WriteLn;
    WaitForKeyPressed;
    END;



PROCEDURE ShowNameEditorCommands;

    BEGIN
    ClearScreenAndTitle ('TRMASTER NAME EDITOR PROGRAM');

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



PROCEDURE CallDatabase.CheckDTAFile;

{ This procedure will do the following:

    1. Load in the index file of the TRMASTER.DTA file.

    2. Check to see if the JA cell has stuff in it - if so, the .DTA
       file will be rewritten with no calls in the JA cell.

    3. Check to make sure no remaining cell is over BufferArraySize
       bytes and if so - truncate it.

  The result is a new .DTA file.  The .ASC file is not changed. }


    BEGIN


    END;



    BEGIN
    CD.CellBuffer.Buffer1Used := False;
    CD.CellBuffer.Buffer2Used := False;
    CD.CellBuffer.Buffer3Used := False;

    CD.CellBuffer.MaximumMemoryToUse := 3 * BufferArraySize;

    CD.CellBuffer.MemoryAllocated := 0;

    CD.SCPDisabledByApplication := False;

    CD.ASCIIFileCrunchCallToRemove := '';
    CD.ASCIIFileIsCurrent          := False;
    CD.ActiveASCIIFileName         := ASCIIFileName;
    CD.ActiveFileName              := 'TRMASTER.DTA';
    CD.CountryString               := '';

    CD.CellBuffer.Key               := '';
    CD.PossibleCallAction          := OnlyCallsWithNames;
    CD.FirstMergeDataListEntry     := nil;
    CD.InitialPartialList          := nil;
    CD.SCPIndexArray               := nil;

    Randomize;

    CD.SectionOverwrite  := False;
    CD.CQZoneOverwrite   := False;
    CD.FOCOverwrite      := False;
    CD.GridOverwrite     := False;
    CD.HitsOverwrite     := True;
    CD.TRMasterFileOpen  := False;
    CD.IndexArrayAllocated := False;
    CD.ITUZoneOverwrite  := False;
    CD.CheckOverwrite    := False;
    CD.NameOverwrite     := False;
    CD.NumberDeleteCalls := 0;
    CD.OldCallOverwrite  := False;
    CD.QTHOverwrite      := False;
    CD.SpeedOverwrite    := False;
    CD.TenTenOverwrite   := False;
    CD.User1Overwrite    := False;
    CD.User2Overwrite    := False;
    CD.User3Overwrite    := False;
    CD.User4Overwrite    := False;
    CD.User5Overwrite    := False;
    END.
