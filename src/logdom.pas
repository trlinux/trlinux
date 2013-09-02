Unit LogDom;

{$O+}
{$F+}
{$V-}

INTERFACE

USES SlowTree, Tree;

CONST MaxNumberRemainingDomQTHs = 325;


TYPE
    DomesticMultType = (NoDomesticMults,
                        WYSIWYGDomestic,
                        IOTADomestic,
                        GridSquares,
                        GridFields,
                        DOKCodes,
                        DomesticFile);


    RemainingMultDisplayModeType = (NoRemainingMults, Erase, HiLight);

    PrefixRecPtr = ^PrefixRec;

    PrefixRec = RECORD
        Prefix:     STRING [6];
        DomQTH:     STRING [6];
        Multiplier: STRING [6];
        NextRecord: PrefixRecPtr;
        END;

    RemainingDomMultArrayType = ARRAY [0..MaxNumberRemainingDomQTHs] OF STRING [6];
    RemainingDomMultArrayPtr  = ^RemainingDomMultArrayType;

    DomQTHTableObject = OBJECT

        ActiveDomQTHFile:     Str80;
        FirstPrefixRecord:    ARRAY [1..36] OF PrefixRecPtr; {KK1L: 6.72 36 from 26. Allows for numbers in DOM}
        LastPrefixRecord:     ARRAY [1..36] OF PrefixRecPtr; {KK1L: 6.72 36 from 26. Allows for numbers in DOM}
        NumberRemainingMults: INTEGER;
        RemainingDomMults:    RemainingDomMultArrayPtr;

        PROCEDURE AddNewPrefixRecord (Prefix: Str20; DomQTH: Str20; Multiplier: Str20);

        FUNCTION  GetDomMultInteger (InputString: Str20): INTEGER;
        FUNCTION  GetDomMultName    (DomMultInteger: INTEGER): Str20;

        FUNCTION  GetDomQTH (InputString: Str20; VAR Multiplier: Str20; VAR QTH: Str20): BOOLEAN;
        FUNCTION  GetStandardDomesticQTH (InputString: Str20): Str20;
        FUNCTION  LoadInDomQTHFile (FileName: Str80): BOOLEAN;
        FUNCTION  ReadDomQTHFile (FileName: Str80): BOOLEAN;
        FUNCTION  RecordPointerIndex (FirstLetter: CHAR): INTEGER;
        END;

VAR
    ActiveDomesticMult: DomesticMultType;
    DomQTHTable:        DomQTHTableObject;
    LiteralDomesticQTH:        BOOLEAN;

    RemainingMultDisplayMode: RemainingMultDisplayModeType;

IMPLEMENTATION

USES LogWind; {KK1L: DEBUG}



PROCEDURE DomQTHTableObject.AddNewPrefixRecord (Prefix: Str20; DomQTH: Str20; Multiplier: Str20);

VAR Index: INTEGER;

    BEGIN
    Index := RecordPointerIndex (Prefix [1]);

    IF Index <= 0 THEN Exit;

    IF FirstPrefixRecord [Index] = Nil THEN
        BEGIN
        FirstPrefixRecord [Index] := New (PrefixRecPtr);
        FirstPrefixRecord [Index] ^.DomQTH     := DomQTH;
        FirstPrefixRecord [Index] ^.Multiplier := Multiplier;
        FirstPrefixRecord [Index] ^.Prefix     := Prefix;
        FirstPrefixRecord [Index] ^.NextRecord := Nil;
        LastPrefixRecord  [Index] := FirstPrefixRecord [Index] ;
        END
    ELSE
        BEGIN
        LastPrefixRecord [Index] ^.NextRecord := New (PrefixRecPtr);
        LastPrefixRecord [Index] := LastPrefixRecord [Index] ^.NextRecord;
        LastPrefixRecord [Index] ^.Multiplier := Multiplier;
        LastPrefixRecord [Index] ^.DomQTH     := DomQTH;
        LastPrefixRecord [Index] ^.Prefix     := Prefix;
        LastPrefixRecord [Index] ^.NextRecord := Nil;
        END;


    END;



FUNCTION DomQTHTableObject.LoadInDomQTHFile (FileName: Str80): BOOLEAN;

VAR Directory: Str80;

    BEGIN
    LoadInDomQTHFile := False;

    IF ActiveDOMQTHFile = FileName THEN
        BEGIN
        LoadInDomQTHFile := True;
        Exit;
        END;

    Directory := FindDirectory (FileName);

    IF NOT DomQTHTable.ReadDomQTHFile (Directory + DirectorySeparator + FileName) THEN
        BEGIN
        ActiveDomQTHFile := '';
        Exit;
        END;

    LoadInDomQTHFile := True;
    ActiveDomQTHFile := FileName;
    END;




FUNCTION DomQTHTableObject.GetDomMultInteger (InputString: Str20): INTEGER;

VAR Entry: INTEGER;

    BEGIN
    InputString := UpperCase (InputString);

    IF NumberRemainingMults > 0 THEN
        FOR Entry := 0 TO NumberRemainingMults - 1 DO
            IF InputString = UpperCase (RemainingDomMults^ [Entry]) THEN
                BEGIN
                GetDomMultInteger := Entry;
                Exit;
                END;

    GetDomMultInteger := -1;
    END;



FUNCTION DomQTHTableObject.GetDomMultName    (DomMultInteger: INTEGER): Str20;

    BEGIN
    IF (NumberRemainingMults > 0) AND (DomMultInteger >= 0) AND (DomMultInteger <= NumberRemainingMults - 1) THEN
        GetDomMultName := RemainingDomMults^ [DomMultInteger]
    ELSE
        GetDomMultName := '';
    END;



FUNCTION DomQTHTableObject.GetStandardDomesticQTH (InputString: Str20): Str20;

VAR CurrentPrefixRecord: PrefixRecPtr;
    Index: INTEGER;

    BEGIN
    GetStandardDomesticQTH := '';

    InputString := UpperCase (InputString);

    Index := RecordPointerIndex (InputString [1]);

    {IF (Index <= 0) OR (Index > 26) THEN Exit;}
    {KK1L: 6.72 NOTE RecordPointerIndex returns 0-36 on its own. 0=0..9=9, A=10..Z=35.}
    {                Why limit this to the numbers 0-9 and letters A-P??              }
    {                BECAUSE the procedure used is in the object NOT the one in COUNTRY9.PAS!!!}
    {                I modified local procedure to match the one in COUNTRY9.PAS so numbers can be used.}
    IF (Index <= 0) OR (Index > 36) THEN Exit; {KK1L: 6.72}

    IF FirstPrefixRecord [Index] = nil THEN Exit;

    WHILE InputString <> '' DO
        BEGIN
        CurrentPrefixRecord := FirstPrefixRecord [Index];

        REPEAT
            IF CurrentPrefixRecord^.Prefix = InputString THEN
                BEGIN
                GetStandardDomesticQTH := CurrentPrefixRecord^.DomQTH;
                Exit;
                END;

            IF CurrentPrefixRecord <> nil THEN
                CurrentPrefixRecord := CurrentPrefixRecord^.NextRecord;
        UNTIL CurrentPrefixRecord = nil;

        Delete (InputString, Length (InputString), 1);
        END;

    END;



FUNCTION DomQTHTableObject.GetDomQTH (InputString: Str20;
                                      VAR Multiplier: Str20;
                                      VAR QTH: Str20): BOOLEAN;

 { Returns TRUE if found }


VAR CurrentPrefixRecord: PrefixRecPtr;
    Index: INTEGER;
    PossibleUSACall: BOOLEAN;
    ContinentString, NumberString: Str20;

    BEGIN
    Multiplier := '';
    QTH        := '';
    GetDomQTH  := False;

    InputString := UpperCase (InputString);

    IF ActiveDomesticMult = NoDomesticMults THEN
        BEGIN
        QTH := LowerCase (InputString);
        GetDomQTH := True;
        Exit;
        END;

    IF ActiveDomesticMult = WYSIWYGDomestic THEN
        BEGIN
        Multiplier := LowerCase (InputString);
        QTH        := LowerCase (InputString);
        GetDomQTH  := True;
        Exit;
        END;

    IF ActiveDomesticMult = DOKCodes THEN  { for WAG contest }
        BEGIN
        Multiplier := LowerCase (Copy (InputString, 1, 1));
        QTH        := InputString;
        GetDomQTH  := Multiplier <> '';
        Exit;
        END;

    IF ActiveDomesticMult = IOTADomestic THEN
        BEGIN
        IF (Length (InputString) < 3) OR (Length (InputString) > 5) THEN Exit;

        ContinentString := UpperCase (Copy (InputString, 1, 2));

        IF (ContinentString = 'AF') OR (ContinentString = 'AS') OR
           (ContinentString = 'EU') OR (ContinentString = 'NA') OR
           (ContinentString = 'OC') OR (ContinentString = 'SA') OR
           (ContinentString = 'AN') THEN
               BEGIN
               NumberString := Copy (InputString, 3, Length (InputString) - 2);

               IF NOT StringIsAllNumbers (NumberString) THEN Exit;

               WHILE Length (NumberString) < 3 DO
                   NumberString := '0' + NumberString;

               Multiplier := LowerCase (ContinentString + NumberString);
               QTH        := Multiplier;
               GetDomQTH  := True;
               END;
        END;

    IF ActiveDomesticMult = GridSquares THEN
        BEGIN
        IF (Length (InputString) <> 4) AND (Length (InputString) <> 6) THEN
            Exit;

        IF (InputString [1] < 'A') OR (InputString [1] > 'R') OR
           (InputString [2] < 'A') OR (InputString [2] > 'R') OR
           (InputString [3] > '9') OR (InputString [3] < '0') OR
           (InputString [4] > '9') OR (InputString [4] < '0') THEN
               Exit;

        IF Length (InputString) = 6 THEN
            IF (InputString [5] < 'A') OR (InputString [5] > 'Z') OR
               (InputString [6] < 'A') OR (InputString [6] > 'Z') THEN
                   Exit;

        Multiplier := Copy (LowerCase (InputString), 1, 4);
        QTH        := LowerCase (InputString);
        GetDomQTH  := True;
        Exit;
        END;


    IF ActiveDomesticMult = GridFields THEN
        BEGIN
        IF (Length (InputString) <> 4) OR (InputString [1] < 'A') OR (InputString [1] > 'R') OR
                                  (InputString [2] < 'A') OR (InputString [2] > 'R') OR
                                  (InputString [3] > '9') OR (InputString [3] < '0') OR
                                  (InputString [4] > '9') OR (InputString [4] < '0') THEN
                                      Exit;

        Multiplier := Copy (LowerCase (InputString), 1, 2);
        QTH        := LowerCase (InputString);
        GetDomQTH  := True;
        Exit;
        END;

    Index := RecordPointerIndex (InputString [1]);

    {IF (Index <= 0) OR (Index > 26) THEN Exit;}
    {KK1L: 6.72 NOTE RecordPointerIndex returns 0-36 on its own. 0=0..9=9, A=10..Z=35.}
    {                Why limit this to the numbers 0-9 and letters A-P??              }
    {                BECAUSE the procedure used is in the object NOT the one in COUNTRY9.PAS!!!}
    {                I modified local procedure to match the one in COUNTRY9.PAS so numbers can be used.}
    IF (Index <= 0) OR (Index > 36) THEN Exit; {KK1L: 6.72}

    IF FirstPrefixRecord [Index] = nil THEN Exit;

    WHILE InputString <> '' DO
        BEGIN
        CurrentPrefixRecord := FirstPrefixRecord [Index];

        REPEAT
            IF CurrentPrefixRecord^.Prefix = InputString THEN
                BEGIN
                QTH        := CurrentPrefixRecord^.DomQTH;
                Multiplier := CurrentPrefixRecord^.Multiplier;
                GetDOMQTh  := True;
                Exit;
                END;

            IF CurrentPrefixRecord <> nil THEN
                CurrentPrefixRecord := CurrentPrefixRecord^.NextRecord;
        UNTIL CurrentPrefixRecord = nil;

        Delete (InputString, Length (InputString), 1);
        END;
    END;



FUNCTION DomQTHTableObject.RecordPointerIndex (FirstLetter: CHAR): INTEGER;

VAR TempInt, Result: INTEGER;

    {BEGIN
    {IF (FirstLetter >= 'A') AND (FirstLetter <= 'Z') THEN  }
    {    BEGIN                                              }
    {    TempInt := Ord (FirstLetter) - Ord ('A') + 1;      }
    {    RecordPointerIndex := TempInt;                     }
    {    END                                                }
    {ELSE                                                   }
    {    RecordPointerIndex := -1;                          }
    {END;                                                   }
{KK1L: 6.72 Replaced above to allow use of numbers in the DOM file}
{           Returns a value of 1 through 36 for 0-9, A-Z.         }
    BEGIN
    Val (FirstLetter, TempInt, Result);

    IF Result = 0 THEN
        BEGIN
        RecordPointerIndex := TempInt + 1;
        Exit;
        END
    ELSE
        IF (FirstLetter >= 'A') AND (FirstLetter <= 'Z') THEN
            BEGIN
            TempInt := Ord (FirstLetter) - Ord ('A') + 11;
            RecordPointerIndex := TempInt;
            END
        ELSE
            RecordPointerIndex := -1;
    END;



FUNCTION RemoveDomQTHName (VAR FileString: Str160): Str80;

    BEGIN
    RemoveDomQTHName := PrecedingString  (FileString, ':');
    FileString  := PostcedingString (FileString, ':');
    GetRidOfPrecedingSpaces (FileString);
    END;


FUNCTION RemoveDomQTHID (VAR FileString: Str160): Str80;

    BEGIN
    RemoveDomQTHID := PrecedingString  (FileString, ':');
    FileString := PostcedingString (FileString, ':');
    GetRidOfPrecedingSpaces (FileString);
    GetRidOfPostcedingSpaces (FileString);
    END;



FUNCTION GetNextPrefix (VAR FileString: Str160;
                        VAR PrefixString: Str20): BOOLEAN;

VAR Result: INTEGER;

    BEGIN
    GetNextPrefix := False;

    IF FileString = '' THEN
        BEGIN
        PrefixString := '';
        Exit;
        END;

    IF StringHas (FileString, ',') THEN
        BEGIN
        PrefixString := PrecedingString (FileString, ',');
        FileString := PostcedingString (FileString, ',');
        GetRidOfPrecedingSpaces (FileString);
        END
    ELSE
        BEGIN
        PrefixString := FileString;
        FileString := '';
        END;

    IF StringHas (PrefixString, '(') THEN
        BEGIN
        PrefixString := PrecedingString (PrefixString, '(');
        GetRidOfPostcedingSpaces (PrefixString);
        END;

    GetNextPrefix := PrefixString <> '';
    END;



FUNCTION DomQTHTableObject.ReadDomQTHFile (FileName: Str80): BOOLEAN;

LABEL AlreadyInList;

VAR FileRead: TEXT;
    FileString: STRING;
    TempString, Multiplier, DomQTHString, DomQTHName: Str40;
    RemainingMult, RecordPointer: INTEGER;
    CurrentRecord, NextRecord: PrefixRecPtr;


    BEGIN
    ReadDomQTHFile := False;

    NumberRemainingMults := 0;

    IF RemainingDomMults = nil THEN New (RemainingDomMults);

    FOR RecordPointer := 1 TO 36 DO {KK1L: 6.72 1-36 from 1-26 to allow for numbers in DOM file}
        BEGIN
        IF FirstPrefixRecord [RecordPointer] <> nil THEN
            BEGIN
            CurrentRecord := FirstPrefixRecord [RecordPointer];
            NextRecord    := FirstPrefixRecord [RecordPointer]^.NextRecord;

            WHILE CurrentRecord <> nil DO
                BEGIN
                Dispose (CurrentRecord);
                CurrentRecord := NextRecord;
                NextRecord := CurrentRecord^.NextRecord;
                END;
            END;

        FirstPrefixRecord [RecordPointer] := nil;
        LastPrefixRecord  [RecordPointer] := nil;
        END;

    IF NOT OpenFileForRead (FileRead, FileName) THEN Exit;

    WHILE NOT Eof (FileRead) DO
        BEGIN
        ReadLn (FileRead, FileString);

        GetRidOfPrecedingSpaces (FileString);

        IF StringHas (FileString, '=') THEN
            BEGIN
            TempString := PrecedingString (FileString, '=');

            GetRidOfPrecedingSpaces  (TempString);
            GetRidOfPostcedingSpaces (TempString);

            IF StringHas (TempString, '>') THEN
                BEGIN
                DomQTHName := PrecedingString  (TempString, '>');
                Multiplier := PostcedingString (TempString, '>');

                GetRidOfPrecedingSpaces  (Multiplier);
                GetRidOfPostcedingSpaces (DomQTHName);

                { Added in 6.30 }

                IF NumberRemainingMults > 0 THEN
                    FOR RemainingMult := 0 TO NumberRemainingMults - 1 DO
                        IF RemainingDomMults^ [RemainingMult] = Multiplier THEN
                            GoTo AlreadyInList;

                IF NumberRemainingMults < MaxNumberRemainingDomQTHs THEN
                    BEGIN
                    RemainingDomMults^ [NumberRemainingMults] := Multiplier;
                    Inc (NumberRemainingMults);
                    END;

    AlreadyInList:

                END
            ELSE
                BEGIN
                DomQTHName := TempString;
                Multiplier := TempString;

                IF NumberRemainingMults < MaxNumberRemainingDomQTHs THEN
                    BEGIN
                    RemainingDomMults^ [NumberRemainingMults] := DomQTHName;
                    Inc (NumberRemainingMults);
                    END;
                END;

            FileString := PostcedingString (FileString, '=');

            WHILE FileString <> '' DO
                BEGIN
                IF StringHas (FileString, ',') THEN
                    DomQTHString := UpperCase (PrecedingString (FileString, ','))
                ELSE
                    DomQTHString := UpperCase (FileString);

                GetRidOfPrecedingSpaces  (DomQTHString);
                GetRidOfPostcedingSpaces (DomQTHString);

                AddNewPrefixRecord (DomQTHString, DomQTHName, Multiplier);

                IF StringHas (FileString, ',') THEN
                    FileString := PostcedingString (FileString, ',')
                ELSE
                    FileString := '';
                END;
            END;
        END;

    Close (FileRead);
    ReadDomQTHFile := True;
    END;



PROCEDURE DomQTHInit;

VAR RecordPointer: INTEGER;
    CurrentRecord, NextRecord: PrefixRecPtr;

    BEGIN
    ActiveDomesticMult := NoDomesticMults;
    DomQTHTable.ActiveDomQTHFile := '';

    FOR RecordPointer := 1 TO 36 DO {KK1L: 6.72 36 from 26. Allows for numbers in DOM file.}
        BEGIN
        IF DomQTHTable.FirstPrefixRecord [RecordPointer] <> Nil THEN
            BEGIN
            CurrentRecord := DomQTHTable.FirstPrefixRecord [RecordPointer];
            NextRecord    := DomQTHTable.FirstPrefixRecord [RecordPointer]^.NextRecord;

            WHILE CurrentRecord <> Nil DO
                BEGIN
                CurrentRecord := Nil;
                CurrentRecord := NextRecord;
                NextRecord := CurrentRecord^.NextRecord;
                END;
            END;

        DomQTHTable.FirstPrefixRecord [RecordPointer] := Nil;
        DomQTHTable.LastPrefixRecord  [RecordPointer] := Nil;
        END;

    DomQTHTable.RemainingDomMults := Nil;
    END;


    BEGIN
    DomQTHInit;
    END.
