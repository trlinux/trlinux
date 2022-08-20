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

UNIT Country9;

{$O+}
{$V-}

INTERFACE

USES LogGrid, SlowTree, Tree;

CONST MaxCountries = 510;

TYPE
    CountryModeType = (ARRLCountryMode, CQCountryMode);
    ZoneModeType = (CQZoneMode, ITUZoneMode);

    PrefixRecPtr = ^PrefixRec;

    PrefixRec = RECORD
        Prefix:     STRING [10];
        Country:    INTEGER;
        Continent:  ContinentType;
        CQZone:     BYTE;
        ITUZone:    BYTE;
        Grid:       STRING [4];
        exactcall:  boolean;
        NextRecord: PrefixRecPtr;
        END;

    CountryInfoRecord = RECORD
        DefaultContinent: ContinentType;
        DefaultCQZone:    BYTE;
        DefaultITUZone:   BYTE;
        DefaultGrid:      STRING [4];
        UTCOffset:        INTEGER;
        ID:               STRING [6];
        Name:             STRING [25];
        END;

    RemainingDXMultArrayType = ARRAY [0..200] OF BOOLEAN;
    RemainingDXMultArrayPtr  = ^RemainingDXMultArrayType;

    RemainingDXMultTemplateType = ARRAY [0..200] OF FourBytes;
    RemainingDXMultTemplatePointer = ^RemainingDXMultTemplateType;

    CountryInfoArrayType = ARRAY [0..MaxCountries - 1] OF CountryInfoRecord;
    CountryInfoArrayPtr  = ^CountryInfoArrayType;

    CountryTableObject = OBJECT

        CustomRemainingCountryListFound: BOOLEAN;
        CountryMode: CountryModeType;
        ZoneMode:    ZoneModeType;

        CountryInfoTable:  CountryInfoArrayPtr;
        FirstPrefixRecord: ARRAY [1..36] OF PrefixRecPtr;

        LastCountry: INTEGER;
        LastCountryCall: CallString;

        LastPrefixRecord:  ARRAY [1..36] OF PrefixRecPtr;
        NumberCountries:   INTEGER;

        RemainingDXMultTemplate:  RemainingDXMultTemplatePointer;
        NumberRemainingCountries: INTEGER;

        PROCEDURE AddNewPrefixRecord (Prefix:    STRING;
                                      Country:   INTEGER;
                                      Continent: ContinentType;
                                      CQZone:    INTEGER;
                                      ITUZone:   INTEGER;
                                      Grid:      Str20;
                                      exactcall: boolean);


        FUNCTION  GetCountry (Call: CallString; UseStandardCall: BOOLEAN): INTEGER;

        FUNCTION  GetARRLCountry   (Call: CallString;
                                    UseStandardCall: BOOLEAN): INTEGER;

        FUNCTION  GetCQCountry     (Call: CallString;
                                    UseStandardCall: BOOLEAN): INTEGER;

        FUNCTION  GetCountryID     (Index: INTEGER):   CallString;
        FUNCTION  GetCountryName   (Index: INTEGER):   Str80;

        FUNCTION  GetDXMultInteger (Mult: Str20): INTEGER;
        FUNCTION  GetDXMultName    (Index: INTEGER): Str20;

        FUNCTION  GetCQZone        (Call: CallString): INTEGER;
        FUNCTION  GetITUZone       (Call: CallString): INTEGER;

        FUNCTION  GetContinent     (Call: CallString): ContinentType;
        FUNCTION  GetContinentName (Continent: ContinentType): Str40;

        FUNCTION  GetGrid (Call: CallString; VAR ID: CallString): Str20;
        FUNCTION  GetZone (Call: CallString): INTEGER;

        FUNCTION  LoadInCountryFile: BOOLEAN;

        PROCEDURE MakeDefaultRemainingCountryList;
        END;

VAR BigRemainingList:    BOOLEAN;
    CountryTable: CountryTableObject;


    FUNCTION  ARRLSectionCountry (CountryID: Str20): BOOLEAN;
    PROCEDURE CheckForNewCountryForTreeOn160 (Call: CallString);
    PROCEDURE CountryInit;
    FUNCTION  DXCall (Call: CallString): BOOLEAN;
    FUNCTION  GetFirstSuffixLetter (Call: CallString): CHAR;
    FUNCTION  GetNumber (Call: CallString): CHAR;
    FUNCTION  ScandinavianCountry (CountryID: Str20): BOOLEAN;


IMPLEMENTATION
uses memlinux;

VAR NaturalPrefixFound: BOOLEAN;



FUNCTION ARRLSectionCountry (CountryID: Str20): BOOLEAN;

    BEGIN
    ARRLSectionCountry := (CountryID = 'K')   OR (CountryID = 'VE') OR
                          (CountryID = 'KC6') OR (CountryID = 'KG4') OR
                          (CountryID = 'KL')  OR (CountryID = 'KH0') OR
                          (CountryID = 'KH1') OR (CountryID = 'KH2') OR
                          (CountryID = 'KH3') OR (CountryID = 'KH4') OR
                          (CountryID = 'KH5') OR (CountryID = 'KH6') OR
                          (CountryID = 'KH7') OR (CountryID = 'KH8') OR
                          (CountryID = 'KH9') OR (CountryID = 'KP1') OR
                          (CountryID = 'KP2') OR (CountryID = 'KP3') OR
                          (CountryID = 'KP4') OR (CountryID = 'KP5');
    END;



PROCEDURE CheckForNewCountryForTreeOn160 (Call: CallString);

    BEGIN
    END;



FUNCTION RecordPointerIndex (FirstLetter: CHAR): INTEGER;

VAR xResult, TempInt: INTEGER;

    BEGIN
    Val (FirstLetter, TempInt, xResult);

    IF xResult = 0 THEN
        BEGIN
        RecordPointerIndex := TempInt;
        Exit;
        END
    ELSE
        IF (FirstLetter >= 'A') AND (FirstLetter <= 'Z') THEN
            BEGIN
            TempInt := Ord (FirstLetter) - Ord ('A') + 10;
            RecordPointerIndex := TempInt;
            END
        ELSE
            RecordPointerIndex := -1;
    END;



FUNCTION CountryTableObject.GetCountry (Call: CallString; UseStandardCall: BOOLEAN): INTEGER;

VAR Country: INTEGER;

    BEGIN
    IF Call = LastCountryCall THEN
        BEGIN
        GetCountry := LastCountry;
        Exit;
        END;

    CASE CountryMode OF
        ARRLCountryMode: Country := GetARRLCountry (Call, UseStandardCall);
        CQCountryMode:   Country := GetCQCountry   (Call, UseStandardCall);
        END;

    GetCountry := Country;
    LastCountry := Country;
    LastCountryCall := Call;
    END;


FUNCTION CountryTableObject.GetZone (Call: CallString): INTEGER;

    BEGIN
    CASE ZoneMode OF
        ITUZoneMode: GetZone := GetITUZone (Call);
        CQZoneMode:  GetZone := GetCQZone (Call);
        END;
    END;



FUNCTION CountryTableObject.GetDXMultInteger (Mult: Str20): INTEGER;

VAR Address: INTEGER;

    BEGIN
    IF NumberRemainingCountries > 0 THEN
        FOR Address := 0 TO NumberRemainingCountries - 1 DO
            IF Mult = ExpandedString (RemainingDXMultTemplate^ [Address]) THEN
                BEGIN
                GetDXMultInteger := Address;
                Exit;
                END;

    GetDXMultInteger := -1;
    END;



FUNCTION CountryTableObject.GetDXMultName (Index: INTEGER): Str20;

    BEGIN
    IF (Index >= 0) AND (Index < NumberRemainingCountries) THEN
        GetDXMultName := ExpandedString (RemainingDXMultTemplate^ [Index])
    ELSE
        GetDXMultName := '';
    END;



FUNCTION CountryTableObject.GetCountryID (Index: INTEGER): CallString;

VAR TempString: Str20;

    BEGIN
    IF (Index >= 0) AND (Index < NumberCountries) THEN
        BEGIN
        TempString := CountryInfoTable^ [Index].ID;
        IF StringHas (TempString, '*') THEN
            Delete (TempString, 1, 1);
        GetCountryID := TempString;
        END
    ELSE
        GetCountryID := '';
    END;



FUNCTION CountryTableObject.GetCountryName (Index: INTEGER): Str80;

    BEGIN
    IF (Index >=0) AND (Index < NumberCountries) THEN
        GetCountryName := CountryInfoTable^ [Index].Name
    ELSE
        GetCountryName := '';
    END;



PROCEDURE CountryTableObject.AddNewPrefixRecord (Prefix:    STRING;
                                                 Country:   INTEGER;
                                                 Continent: ContinentType;
                                                 CQZone:    INTEGER;
                                                 ITUZone:   INTEGER;
                                                 Grid:      Str20;
                                                 exactcall: boolean);


VAR Index: INTEGER;

    BEGIN
    IF SizeOf (PrefixRec) >= MaxAvail THEN
        BEGIN
        ReportError ('Not enough memory for PrefixRec!!');
        Halt;
        END;

    Index := RecordPointerIndex (Prefix [1]);

    IF Index <= 0 THEN Exit;

    IF FirstPrefixRecord [Index] = Nil THEN
        BEGIN
        FirstPrefixRecord [Index] := New (PrefixRecPtr);

        FirstPrefixRecord [Index] ^.Country := Country;
        FirstPrefixRecord [Index] ^.Prefix  := Prefix;

        FirstPrefixRecord [Index] ^.Continent := Continent;
        FirstPrefixRecord [Index] ^.CQZone    := CQZone;
        FirstPrefixRecord [Index] ^.ITUZone   := ITUZone;
        FirstPrefixRecord [Index] ^.Grid      := Grid;
        FirstPrefixRecord [Index] ^.exactcall := exactcall;

        FirstPrefixRecord [Index] ^.NextRecord := Nil;
        LastPrefixRecord  [Index] := FirstPrefixRecord [Index] ;
        END
    ELSE
        BEGIN
        LastPrefixRecord [Index] ^.NextRecord := New (PrefixRecPtr);
        LastPrefixRecord [Index] := LastPrefixRecord [Index] ^.NextRecord;

        LastPrefixRecord [Index] ^.Country := Country;
        LastPrefixRecord [Index] ^.Prefix  := Prefix;

        LastPrefixRecord [Index] ^.Continent := Continent;
        LastPrefixRecord [Index] ^.CQZone    := CQZone;
        LastPrefixRecord [Index] ^.ITUZone   := ITUZone;
        LastPrefixRecord [Index] ^.Grid      := Grid;
        LastPrefixRecord [Index] ^.exactcall := exactcall;

        LastPrefixRecord [Index] ^.NextRecord := Nil;
        END;
    END;



FUNCTION CountryTableObject.GetARRLCountry (Call: CallString;
                                            UseStandardCall: BOOLEAN): INTEGER;

VAR CurrentPrefixRecord: PrefixRecPtr;
    Index: INTEGER;
    PossibleUSACall: BOOLEAN;
    ID: Str20;
    fullcall: boolean;
    CallTotal: CallString;

    BEGIN
    GetARRLCountry := -1;

    CallTotal := UpCase(Call);
    IF UseStandardCall THEN
        Call := StandardCallFormat (CallTotal, True);

    IF Copy (Call, 1, 3) = 'MM/' THEN Exit;

    PossibleUSACall := ((Copy (Call, 1, 3) = 'KC6') OR
                        (Copy (Call, 1, 3) = 'KC4') OR
                        (Copy (Call, 1, 3) = 'KG4')) AND (Length (Call) > 3);

    IF PossibleUSACall THEN
        BEGIN
        IF Copy (Call, 1, 3) = 'KC6' THEN PossibleUSACall := Length (Call) <> 5;
        IF Copy (Call, 1, 3) = 'KG4' THEN PossibleUSACall := Length (Call) <> 5;
        END;

    IF (Length (Call) = 6) THEN
        BEGIN
        IF Copy (Call, 1, 3) = 'KA2' THEN Call := 'K2OVJ';
        IF Copy (Call, 1, 3) = 'KC6' THEN Call := 'K6OVJ';
        END;

    Index := RecordPointerIndex (Call [1]);

    IF (Index <= 0) OR (FirstPrefixRecord [Index] = Nil) THEN Exit;
    fullcall := true;
    WHILE Call <> '' DO
        BEGIN
        CurrentPrefixRecord := FirstPrefixRecord [Index] ;

        REPEAT
            IF (
                  ((not CurrentPrefixRecord^.exactcall) and
                     (CurrentPrefixRecord^.Prefix = Call))
               OR
                  ((fullcall and CurrentPrefixRecord^.exactcall) and
                     (CurrentPrefixRecord^.Prefix = CallTotal))
                ) THEN
                BEGIN
                ID := CountryInfoTable^ [CurrentPrefixRecord^.Country].ID;

                IF StringHas (ID, '*') THEN  { CQ only country }
                    BEGIN

                    { If this is 4U1VIC, fake a Austrian callsign }

                    IF StringHas (ID, '4U1V') THEN
                        BEGIN
                        GetARRLCountry := GetARRLCountry ('OE', True); { :-) }
                        Exit;
                        END;

                    { All others will get worked out with one less letter }
                    END
                ELSE
                    BEGIN
                    GetARRLCountry := CurrentPrefixRecord^.Country;
                    Exit;
                    END;
                END;

            IF CurrentPrefixRecord <> Nil THEN
                CurrentPrefixRecord := CurrentPrefixRecord^.NextRecord;
        UNTIL CurrentPrefixRecord = Nil;

        fullcall := false;
        Delete (Call, Length (Call), 1);

        IF PossibleUSACall THEN
            IF (Length (Call) = 3) OR (Length (Call) = 4) THEN
                BEGIN
                IF Copy (Call, 1, 3) = 'KC4' THEN Call := 'K4BAI';
                IF Copy (Call, 1, 3) = 'KC6' THEN Call := 'K6OVJ';
                IF Copy (Call, 1, 3) = 'KG4' THEN Call := 'K4BAI';
                END;
        END;

    END;



FUNCTION CountryTableObject.GetCQCountry (Call: CallString;
                                          UseStandardCall: BOOLEAN): INTEGER;

VAR CurrentPrefixRecord: PrefixRecPtr;
    Index: INTEGER;
    PossibleUSACall: BOOLEAN;
    fullcall: boolean;
    CallTotal: CallString;

    BEGIN
    GetCQCountry := -1;

    CallTotal := UpCase(Call);
    IF UseStandardCall THEN
        Call := StandardCallFormat (CallTotal, True);

    IF Copy (Call, 1, 3) = 'MM/' THEN Exit;

    PossibleUSACall := ((Copy (Call, 1, 3) = 'KC6') OR
                        (Copy (Call, 1, 3) = 'KC4') OR
                        (Copy (Call, 1, 3) = 'KG4')) AND (Length (Call) > 3);

    IF PossibleUSACall THEN
        BEGIN
        IF Copy (Call, 1, 3) = 'KC6' THEN PossibleUSACall := Length (Call) <> 5;
        IF Copy (Call, 1, 3) = 'KG4' THEN PossibleUSACall := Length (Call) <> 5;
        END;

    IF (Length (Call) = 6) THEN
        BEGIN
        IF Copy (Call, 1, 3) = 'KA2' THEN Call := 'K2OVJ';
        IF Copy (Call, 1, 3) = 'KC6' THEN Call := 'K6OVJ';
        END;

    Index := RecordPointerIndex (Call [1]);

    IF (Index <= 0) OR (FirstPrefixRecord [Index] = Nil) THEN Exit;
    fullcall := true;
    WHILE Call <> '' DO
        BEGIN
        CurrentPrefixRecord := FirstPrefixRecord [Index] ;

        REPEAT
            IF (
                  ((not CurrentPrefixRecord^.exactcall) and
                     (CurrentPrefixRecord^.Prefix = Call))
               OR
                  ((fullcall and CurrentPrefixRecord^.exactcall) and
                     (CurrentPrefixRecord^.Prefix = CallTotal))
                ) THEN
                BEGIN
                GetCQCountry := CurrentPrefixRecord^.Country;
                Exit;
                END;

            IF CurrentPrefixRecord <> Nil THEN
                CurrentPrefixRecord := CurrentPrefixRecord^.NextRecord;
        UNTIL CurrentPrefixRecord = Nil;

        fullcall := false;
        Delete (Call, Length (Call), 1);

        IF PossibleUSACall THEN
            IF (Length (Call) = 3) OR (Length (Call) = 4) THEN
                BEGIN
                IF Copy (Call, 1, 3) = 'KC4' THEN
                    Call := 'K4BAI';

                IF Copy (Call, 1, 3) = 'KC6' THEN
                    Call := 'K6OVJ';

                IF Copy (Call, 1, 3) = 'KG4' THEN
                    Call := 'K4BAI';
                END;
        END;

    END;



FUNCTION CountryTableObject.GetCQZone (Call: CallString): INTEGER;

VAR CurrentPrefixRecord: PrefixRecPtr;
    Index: INTEGER;
    ShortCall, ID: CallString;
    PossibleUSACall: BOOLEAN;
    fullcall: boolean;
    CallTotal: CallString;

    BEGIN
    GetCQZone := -1;

    CallTotal := UpCase(Call);
    Call := StandardCallFormat (CallTotal, True);

    ShortCall := Call;

    PossibleUSACall := ((Copy (Call, 1, 3) = 'KC6') OR
                        (Copy (Call, 1, 3) = 'KC4') OR
                        (Copy (Call, 1, 3) = 'KG4')) AND (Length (Call) > 3);

    IF PossibleUSACall THEN
        BEGIN
        IF Copy (Call, 1, 3) = 'KC6' THEN PossibleUSACall := Length (Call) <> 5;
        IF Copy (Call, 1, 3) = 'KG4' THEN PossibleUSACall := Length (Call) <> 5;
        END;

    IF (Length (Call) = 6) THEN
        BEGIN
        IF Copy (Call, 1, 3) = 'KA2' THEN ShortCall := 'K2OVJ';
        IF Copy (Call, 1, 3) = 'KC6' THEN ShortCall := 'K6OVJ';
        END;

    Index := RecordPointerIndex (ShortCall [1]);

    IF (Index <= 0) OR (FirstPrefixRecord [Index] = Nil) THEN Exit;

    fullcall := true;
    WHILE ShortCall <> '' DO
        BEGIN
        CurrentPrefixRecord := FirstPrefixRecord [Index];

        REPEAT
            IF (
                  ((not CurrentPrefixRecord^.exactcall) and
                     (CurrentPrefixRecord^.Prefix = ShortCall))
               OR
                  ((fullcall and CurrentPrefixRecord^.exactcall) and
                     (CurrentPrefixRecord^.Prefix = CallTotal))
                ) THEN
                BEGIN
                IF CurrentPrefixRecord^.CQZone <> 0 THEN  { Override }
                    BEGIN
                    GetCQZone := CurrentPrefixRecord^.CQZone;
                    Exit;
                    END;

                ID        := GetCountryID (CurrentPrefixRecord^.Country);
                GetCQZone := CountryInfoTable^ [CurrentPrefixRecord^.Country].DefaultCQZone;

                { We have to check for call areas in USA }

                IF ID = 'K' THEN
                    CASE GetNumber (Call) OF
                        '1', '2', '3', '4': GetCQZone := 5;
                        '5', '8', '9', '0': GetCQZone := 4;
                        '6', '7':           GetCQZone := 3;
                        END;

                { We have to check for call areas for UA9 }

                IF ID = 'VE' THEN
                    CASE GetNumber (Call) OF
                        '1', '2', '9', '0': GetCQZone := 5;
                        '3', '4', '5', '6': GetCQZone := 4;
                        '7': GetCQZone := 3;
                        '8': GetCQZone := 2;
                        END;

                IF ID = 'UA9' THEN
                    CASE GetNumber (Call) OF
                        '8': GetCQZone := 18;

                        '9': CASE GetFirstSuffixLetter (Call) OF
                                 'A','C','D','F','G','J','K','L','M','N','Q','R','S','T','W','X': GetCQZone := 17;
                                 'H','O','U','Y','Z':                                             GetCQZone := 18;
                                 END;

                        '0': CASE GetFirstSuffixLetter (Call) OF
                                 'A', 'B', 'H', 'O', 'S', 'U', 'W':       GetCQZone := 18;
                                 'C', 'D', 'E', 'F', 'I', 'J', 'K', 'L':  GetCQZone := 19;
                                 'Q', 'X', 'Z':                           GetCQZone := 19;
                                 'Y':                                     GetCQZone := 23;
                                 ELSE                                     GetCQZone := 19;
                                 END;
                        END;

                { And China }

                IF ID = 'BY' THEN
                    CASE GetNumber (Call) OF
                        '1', '2': GetCQZone := 24;

                        '3': CASE GetFirstSuffixLetter (Call) OF
                                 'G', 'H', 'I', 'J', 'K', 'L': GetCQZone := 23;
                                 ELSE                          GetCQZone := 24;
                                 END;

                        '4', '5', '6', '7', '8': GetCQZone := 24;

                        '9': CASE GetFirstSuffixLetter (Call) OF
                                 'M', 'N', 'P', 'Q', 'R', 'S': GetCQZone := 24;
                                 ELSE                          GetCQZone := 23;
                                 END;

                        '0': GetCQZone := 23;
                        END;

                { And Austrailia }

                IF ID = 'VK' THEN
                    CASE GetNumber (Call) OF
                        '6','8': GetCQZone := 29;
                        ELSE     GetCQZone := 30;
                        END;

                Exit;
                END;

            IF CurrentPrefixRecord <> Nil THEN
                CurrentPrefixRecord := CurrentPrefixRecord^.NextRecord;
        UNTIL CurrentPrefixRecord = Nil;

        fullcall := false;
        Delete (ShortCall, Length (ShortCall), 1);

        IF PossibleUSACall THEN
            IF (Length (ShortCall) = 3) OR (Length (ShortCall) = 4) THEN
                BEGIN
                IF Copy (ShortCall, 1, 3) = 'KC4' THEN
                    ShortCall := 'K4BAI';

                IF Copy (ShortCall, 1, 3) = 'KC6' THEN
                    ShortCall := 'K6OVJ';

                IF Copy (ShortCall, 1, 3) = 'KG4' THEN
                     ShortCall := 'K4BAI';
                END;
        END;

    END;




FUNCTION CountryTableObject.GetITUZone (Call: CallString): INTEGER;

VAR CurrentPrefixRecord: PrefixRecPtr;
    Index: INTEGER;
    ShortCall, ID: CallString;
    PossibleUSACall: BOOLEAN;
    fullcall: boolean;
    CallTotal: CallString;

    BEGIN
    GetITUZone := -1;

    CallTotal := UpCase(Call);
    Call := StandardCallFormat (CallTotal, True);

    ShortCall := Call;

    PossibleUSACall := ((Copy (Call, 1, 3) = 'KC6') OR
                        (Copy (Call, 1, 3) = 'KC4') OR
                        (Copy (Call, 1, 3) = 'KG4')) AND (Length (Call) > 3);

    IF PossibleUSACall THEN
        BEGIN
        IF Copy (Call, 1, 3) = 'KC6' THEN PossibleUSACall := Length (Call) <> 5;
        IF Copy (Call, 1, 3) = 'KG4' THEN PossibleUSACall := Length (Call) <> 5;
        END;

    IF (Length (Call) = 6) THEN
        BEGIN
        IF Copy (Call, 1, 3) = 'KA2' THEN ShortCall := 'K2OVJ';
        IF Copy (Call, 1, 3) = 'KC6' THEN ShortCall := 'K6OVJ';
        END;

    Index := RecordPointerIndex (ShortCall [1]);

    IF (Index <= 0) OR (FirstPrefixRecord [Index] = Nil) THEN Exit;

    fullcall := true;
    WHILE ShortCall <> '' DO
        BEGIN
        CurrentPrefixRecord := FirstPrefixRecord [Index];

        REPEAT
            IF (
                  ((not CurrentPrefixRecord^.exactcall) and
                     (CurrentPrefixRecord^.Prefix = ShortCall))
               OR
                  ((fullcall and CurrentPrefixRecord^.exactcall) and
                     (CurrentPrefixRecord^.Prefix = CallTotal))
                ) THEN
                BEGIN
                IF CurrentPrefixRecord^.ITUZone <> 0 THEN
                    BEGIN
                    GetITUZone := CurrentPrefixRecord^.ITUZone;
                    Exit;
                    END;

                ID         := GetCountryID (CurrentPrefixRecord^.Country);
                GetITUZone := CountryInfoTable^ [CurrentPrefixRecord^.Country].DefaultITUZone;

                { We have to check for call areas in USA }

                IF ID = 'K' THEN
                    CASE GetNumber (Call) OF
                        '5', '0': GetITUZone := 7;
                        '6', '7': GetITUZone := 6;
                        ELSE      GetITUZone := 8;
                        END;

                { In China }

                IF ID = 'BY' THEN
                    CASE GetNumber (Call) OF
                        '8', '9': GetITUZone := 43;
                        '0':      GetITUZone := 42;
                        ELSE      GetITUZone := 44;
                        END;

                IF ID = 'CE' THEN
                    CASE GetNumber (Call) OF
                        '1', '2', '3', '4', '5': GetITUZone := 14;
                        '6', '7', '8':           GetITUZone := 16;
                        END;

                IF ID = 'CP' THEN
                    CASE GetNumber (Call) OF
                        '1', '8', '9': GetITUZone := 12;
                        ELSE           GetITUZone := 14;
                        END;

                IF ID = 'LU' THEN
                    CASE GetFirstSuffixLetter (Call) OF
                        'V', 'W', 'X': GetITUZone := 16;
                        ELSE           GetITUZone := 14;
                        END;

                IF ID = 'PY' THEN
                    CASE GetNumber (Call) OF
                        '6', '7', '8': GetITUZone := 13;
                        ELSE           GetITUZone := 15;
                        END;

                IF ID = 'UA' THEN
                    CASE GetNumber (Call) OF
                        '1': CASE GetFirstSuffixLetter (Call) OF
                                 'N', 'Z': GetITUZone := 19;
                                 ELSE      GetITUZone := 29;
                                 END;

                        '3','6': GetITUZone := 29;

                        '4': CASE GetFirstSuffixLetter (Call) OF
                                 'A', 'C', 'F', 'L', 'Q', 'S', 'U', 'Y': GetITUZone := 29;
                                 'H', 'N', 'P', 'W':                     GetITUZone := 30;
                                 END;
                        END;

                IF ID = 'UA9' THEN
                    CASE GetNumber (Call) OF
                        '8': CASE GetFirstSuffixLetter (Call) OF
                                 'T': GetITUZone := 32;
                                 'V': GetITUZone := 33;
                                 END;

                        '9': CASE GetFirstSuffixLetter (Call) OF
                                 'A', 'C', 'F', 'G', 'L',
                                 'M', 'Q', 'S', 'T', 'W': GetITUZone := 30;
                                 'H', 'O', 'U', 'Y', 'Z': GetITUZone := 31;
                                 'J', 'K':                GetITUZone := 21;
                                 'X':                     GetITUZone := 20;
                                 END;

                        '0': CASE GetFirstSuffixLetter (Call) OF
                                 'B', 'H':                GetITUZone := 22;
                                 'Q':                     GetITUZone := 23;
                                 'I':                     GetITUZone := 24;
                                 'X':                     GetITUZone := 25;
                                 'K':                     GetITUZone := 26;
                                 'A', 'O', 'S', 'W', 'Y': GetITUZone := 32;
                                 'D', 'J', 'U':           GetITUZone := 33;
                                 'C', 'F', 'L':           GetITUZone := 34;
                                 'Z':                     GetITUZone := 35;
                                 END;

                        END;

                IF ID = 'VE' THEN
                    CASE GetNumber (Call) OF
                        '1', '9': GetITUZone := 9;
                        '2', '3': GetITUZone := 4;
                        '4','5':  GetITUZone := 3;
                        '6','7':  GetITUZone := 2;
                        ELSE      GetITUZone := 0;
                        END;

                IF ID = 'VK' THEN
                    CASE GetNumber (Call) OF
                        '4','8': GetITUZone := 55;
                        '6':     GetITUZone := 58;
                        ELSE     GetITUZone := 59;
                        END;

                Exit;
                END;

            IF CurrentPrefixRecord <> Nil THEN
                CurrentPrefixRecord := CurrentPrefixRecord^.NextRecord;
        UNTIL CurrentPrefixRecord = Nil;

        fullcall := false;
        Delete (ShortCall, Length (ShortCall), 1);

        IF PossibleUSACall THEN
            IF (Length (ShortCall) = 3) OR (Length (ShortCall) = 4) THEN
                BEGIN
                IF Copy (ShortCall, 1, 3) = 'KC4' THEN
                    ShortCall := 'K4BAI';

                IF Copy (ShortCall, 1, 3) = 'KC6' THEN
                    ShortCall := 'K6OVJ';

                IF Copy (ShortCall, 1, 3) = 'KG4' THEN
                     ShortCall := 'K4BAI';
                END;
        END;
    END;



FUNCTION CountryTableObject.GetGrid (Call: CallString; VAR ID: CallString): Str20;

VAR CurrentPrefixRecord: PrefixRecPtr;
    Index: INTEGER;
    Oblast, ShortCall: CallString;
    PossibleUSACall: BOOLEAN;
    fullcall: boolean;
    CallTotal: CallString;

    BEGIN
    GetGrid := '';

    CallTotal := UpCase(Call);
    Call := StandardCallFormat (CallTotal, True);

    ShortCall := Call;

    PossibleUSACall := ((Copy (Call, 1, 3) = 'KC6') OR
                        (Copy (Call, 1, 3) = 'KC4') OR
                        (Copy (Call, 1, 3) = 'KG4')) AND (Length (Call) > 3);

    IF PossibleUSACall THEN
        BEGIN
        IF Copy (Call, 1, 3) = 'KC6' THEN PossibleUSACall := Length (Call) <> 5;
        IF Copy (Call, 1, 3) = 'KG4' THEN PossibleUSACall := Length (Call) <> 5;
        END;

    IF (Length (Call) = 6) THEN
        BEGIN
        IF Copy (Call, 1, 3) = 'KA2' THEN ShortCall := 'K2OVJ';
        IF Copy (Call, 1, 3) = 'KC6' THEN ShortCall := 'K6OVJ';
        END;

    Index := RecordPointerIndex (ShortCall [1]);

    IF (Index <= 0) OR (FirstPrefixRecord [Index] = Nil) THEN Exit;

    fullcall := true;
    WHILE ShortCall <> '' DO
        BEGIN
        CurrentPrefixRecord := FirstPrefixRecord [Index];

        REPEAT
            IF (
                  ((not CurrentPrefixRecord^.exactcall) and
                     (CurrentPrefixRecord^.Prefix = ShortCall))
               OR
                  ((fullcall and CurrentPrefixRecord^.exactcall) and
                     (CurrentPrefixRecord^.Prefix = CallTotal))
                ) THEN
                BEGIN
                IF CurrentPrefixRecord^.Grid <> '' THEN { Override }
                    BEGIN
                    GetGrid := CurrentPrefixRecord^.Grid;
                    ID := GetCountryID (CurrentPrefixRecord^.Country);
                    Exit;
                    END;

                GetGrid := CountryInfoTable^ [CurrentPrefixRecord^.Country].DefaultGrid;

                ID := GetCountryID (CurrentPrefixRecord^.Country);

                { Might want to add defaults for various call areas }

                IF ID = 'BY' THEN
                    BEGIN
                    Oblast := GetOblast (Call);

                    CASE Oblast [1] OF
                       '1': GetGrid := 'OM89';

                       '2': IF Oblast [2] <= 'H' THEN
                                GetGrid := 'PN37'
                            ELSE
                                IF Oblast [2] <= 'P' THEN
                                    GetGrid := 'PN23'
                                ELSE
                                    GetGrid := 'PN11';

                       '3': IF Oblast [2] <= 'F' THEN
                                GetGrid := 'OM89'
                            ELSE
                                IF Oblast [2] <= 'L' THEN
                                    GetGrid := 'ON50'
                                ELSE
                                    IF Oblast [2] <= 'R' THEN
                                        GetGrid := 'OM78'
                                    ELSE
                                        GetGrid := 'OM68';

                       '4': IF Oblast [2] <= 'H' THEN
                                GetGrid := 'PM01'
                            ELSE
                                IF Oblast [2] <= 'P' THEN
                                    GetGrid := 'OM86'
                                ELSE
                                    GetGrid := 'OM92';

                       '5':IF Oblast [2] <= 'H' THEN
                                GetGrid := 'PM00'
                            ELSE
                                IF Oblast [2] <= 'P' THEN
                                    GetGrid := 'OL78'
                                ELSE
                                    GetGrid := 'OL96';

                       '6': IF Oblast [2] <= 'H' THEN
                                GetGrid := 'OM81'
                            ELSE
                                IF Oblast [2] <= 'P' THEN
                                    GetGrid := 'OM64'
                                ELSE
                                    GetGrid := 'OM70';

                       '7': IF Oblast [2] <= 'H' THEN
                                GetGrid := 'OL68'
                            ELSE
                                IF Oblast [2] <= 'P' THEN
                                    GetGrid := 'OL63'
                                ELSE
                                    IF Oblast [2] <= 'X' THEN
                                        GetGrid := 'OL42'
                                    ELSE
                                        GetGrid := 'OL50';

                       '8': IF Oblast [2] <= 'F' THEN
                                GetGrid := 'OM20'
                            ELSE
                                IF Oblast [2] <= 'R' THEN
                                    GetGrid := 'OL36'
                                ELSE
                                    GetGrid := 'OL15';

                       '9': IF Oblast [2] <= 'F' THEN
                                GetGrid := 'OM44'
                            ELSE
                                IF Oblast [2] <= 'L' THEN
                                    GetGrid := 'OM16'
                                ELSE
                                    IF Oblast [2] <= 'R' THEN
                                        GetGrid := 'OM38'
                                    ELSE
                                        GetGrid := 'OM06';

                       '0': IF Oblast [2] <= 'F' THEN
                                GetGrid := 'NN33'
                            ELSE
                                GetGrid := 'NL59';
                       END;

                    END;


                IF ID = 'CE' THEN
                    CASE GetNumber (Call) OF
                        '1': GetGrid := 'FG58';
                        '2': GetGrid := 'FG41';
                        '3': GetGrid := 'FF47';
                        '4': GetGrid := 'FF45';
                        '5': GetGrid := 'FF32';
                        '6': GetGrid := 'FF30';
                        '7': GetGrid := 'FE22';
                        END;

                IF ID = 'CP' THEN
                    CASE GetNumber (Call) OF
                        '1': GetGrid := 'FH63';
                        '2': GetGrid := 'FH70';
                        '3': GetGrid := 'FH61';
                        '4': GetGrid := 'FG69';
                        '5': GetGrid := 'FH73';
                        '6': GetGrid := 'FH93';
                        '7': GetGrid := 'FG88';
                        '8': GetGrid := 'FH77';
                        '9': GetGrid := 'FH68';
                        END;

                IF ID = 'EA' THEN
                    CASE GetNumber (Call) OF
                        '1': GetGrid := 'IN62';
                        '2': GetGrid := 'IN82';
                        '3': GetGrid := 'IN92';
                        '4': GetGrid := 'IM79';
                        '5': GetGrid := 'IM98';
                        '7': GetGrid := 'IM77';
                        END;

                IF ID = 'EU' THEN
                    CASE GetNumber (Call) OF
                        '1': GetGrid := 'KO33';
                        '2': GetGrid := 'KO33';
                        '3': GetGrid := 'KO12';
                        '4': GetGrid := 'KO13';
                        '6': GetGrid := 'KO55';
                        '7': GetGrid := 'KO53';
                        '8': GetGrid := 'KO52';
                        END;

                IF ID = 'HK' THEN
                    CASE GetNumber (Call) OF
                        '1': GetGrid := 'FK20';
                        '2': GetGrid := 'GK80';
                        '3': GetGrid := 'FJ34';
                        '4': GetGrid := 'FJ16';
                        '5': GetGrid := 'FJ16';
                        '6': GetGrid := 'FJ23';
                        '7': GetGrid := 'FJ35';
                        '8': GetGrid := 'FJ21';
                        '9': GetGrid := 'FJ40';
                        END;

                IF ID = 'JA' THEN
                    CASE GetNumber (Call) OF
                        '1': GetGrid := 'PM95';
                        '2': GetGrid := 'PM84';
                        '3': GetGrid := 'PM74';
                        '4': GetGrid := 'PM65';
                        '5': GetGrid := 'PM63';
                        '6': GetGrid := 'PM52';
                        '7': GetGrid := 'QM09';
                        '8': GetGrid := 'QN13';
                        '9': GetGrid := 'PM86';
                        END;

                IF ID = 'K' THEN
                    CASE GetNumber (Call) OF
                        '1': GetGrid := 'FN48';
                        '2': GetGrid := 'FN22';
                        '3': GetGrid := 'FN10';
                        '4': GetGrid := 'EM73';
                        '5': GetGrid := 'EM01';
                        '6': GetGrid := 'DM06';
                        '7': GetGrid := 'DN21';
                        '8': GetGrid := 'EN81';
                        '9': GetGrid := 'EN52';
                        '0': GetGrid := 'EN12';
                        END;

                IF ID = 'LU' THEN
                    BEGIN
//                    LUCall := RootCall (Call);
                    RootCall (Call);

                    WHILE StringHasNumber (Call) DO
                    begin
                        Delete (Call, 1, 1);
                        fullcall := false;
                    end;

                    CASE Call [1] OF
                        'A', 'B', 'C': GetGrid := 'GF15';
                        'D', 'E': GetGrid := 'GF03';
                        'F': GetGrid := 'FF98';
                        'G': GetGrid := 'FG95';
                        'H': GetGrid := 'FF87';
                        'I': GetGrid := 'GG23';
                        'J': GetGrid := 'GF07';
                        'K': GetGrid := 'FG73';
                        'L': GetGrid := 'GG12';
                        'M': GetGrid := 'FF55';
                        'N': GetGrid := 'FG82';
                        'O': GetGrid := 'FG76';
                        'P': GetGrid := 'FF59';
                        'Q': GetGrid := 'FF76';
                        'R': GetGrid := 'FG63';
                        'S': GetGrid := 'FG60';
                        'T': GetGrid := 'FG76';
                        'U': GetGrid := 'FF72';
                        'V': GetGrid := 'FF60';
                        'W': GetGrid := 'FE55';
                        'X': GetGrid := 'FE50';
                        'Y': GetGrid := 'FF51';
                        END;
                    END;

                IF ID = 'OA' THEN
                    CASE GetNumber (Call) OF
                        '1': GetGrid := 'EI95';
                        '2': GetGrid := 'FI03';
                        '3': GetGrid := 'FI20';
                        '4': GetGrid := 'FH18';
                        '5': GetGrid := 'FH26';
                        '6': GetGrid := 'FH32';
                        '7': GetGrid := 'FH46';
                        '8': GetGrid := 'FI27';
                        '9': GetGrid := 'FI14';
                        END;

                IF ID = 'OH' THEN
                    CASE GetNumber (Call) OF
                        '1': GetGrid := 'KP10';
                        '2': GetGrid := 'KP20';
                        '3': GetGrid := 'KP11';
                        '4': GetGrid := 'KP32';
                        '5': GetGrid := 'KP20';
                        '6': GetGrid := 'KP03';
                        '7': GetGrid := 'KP42';
                        '8': GetGrid := 'KP44';
                        '9': GetGrid := 'KP25';
                        END;

                IF ID = 'PY' THEN
                    CASE GetNumber (Call) OF
                        '1': GetGrid := 'GG88';
                        '2': GetGrid := 'GH65';
                        '3': GetGrid := 'GG30';
                        '4': GetGrid := 'GH71';
                        '5': GetGrid := 'GG44';
                        '6': GetGrid := 'GH97';
                        '7': GetGrid := 'HI13';
                        '8': GetGrid := 'GI05';
                        '9': GetGrid := 'GH25';
                        END;

                IF ID = 'SM' THEN
                    CASE GetNumber (Call) OF
                        '1': GetGrid := 'JO97';
                        '2': GetGrid := 'KP05';
                        '3': GetGrid := 'JP83';
                        '4': GetGrid := 'JO79';
                        '5': GetGrid := 'JO89';
                        '6': GetGrid := 'JO67';
                        '7': GetGrid := 'JO76';
                        END;

                IF ID = 'UA' THEN
                    BEGIN
                    Oblast := GetOblast (Call);

                    CASE Oblast [1] OF
                        '1': CASE Oblast [2] OF
                                 'N': GetGrid := 'KP64';
                                 'O': GetGrid := 'LP04';
                                 'P': GetGrid := 'LP77';
                                 'Q', 'R', 'S': GetGrid := 'KO99';
                                 'T', 'U': GetGrid := 'LO26';
                                 'W', 'X': GetGrid := 'KO47';
                                 'Y', 'Z': GetGrid := 'KP68';
                                 ELSE
                                     GetGrid := 'KO59';
                                 END;

                        '3': CASE Oblast [2] OF
                                 'A', 'B', 'C', 'D', 'F', 'H': GetGrid := 'LO05';
                                 'E': GetGrid := 'KO92';
                                 'G': GetGrid := 'LO02';
                                 'I', 'J': GetGrid := 'KO76';
                                 'L': GetGrid := 'KO65';
                                 'M': GetGrid := 'KO97';
                                 'N', 'O': GetGrid := 'LO07';
                                 'P': GetGrid := 'KO84';
                                 'Q': GetGrid := 'KO91';
                                 'R': GetGrid := 'LO02';
                                 'S': GetGrid := 'KO94';
                                 'T': GetGrid := 'LO26';
                                 'U': GetGrid := 'LO07';
                                 'V': GetGrid := 'LO06';
                                 'W': GetGrid := 'KO81';
                                 'X': GetGrid := 'KO84';
                                 'Y': GetGrid := 'KO73';
                                 'Z': GetGrid := 'KO80';
                                 END;

                        '4': CASE Oblast [2] OF
                                 'A', 'B': GetGrid := 'LN28';
                                 'C', 'D': GetGrid := 'LO31';
                                 'F': GetGrid := 'LO23';
                                 'H', 'I': GetGrid := 'LO53';
                                 'L', 'M': GetGrid := 'LO44';
                                 'N', 'O': GetGrid := 'KO74';
                                 'P', 'Q', 'R': GetGrid := 'LO55';
                                 'S', 'T': GetGrid := 'LO46';
                                 'U': GetGrid := 'LO24';
                                 'W': GetGrid := 'LO67';
                                 'Y', 'Z': GetGrid := 'LO35';
                                 END;

                        '6': CASE Oblast [2] OF
                                 'A', 'B', 'C', 'D': GetGrid := 'KN95';
                                 'E': GetGrid := 'LN14';
                                 'F', 'G', 'H': GetGrid := 'LN05';
                                 'I': GetGrid := 'LN26';
                                 'J': GetGrid := 'LN23';
                                 'L', 'M', 'N', 'O': GetGrid := 'KO97';
                                 'P': GetGrid := 'LN23';
                                 'Q': GetGrid := 'LN23';
                                 'U', 'V': GetGrid := 'LN46';
                                 'W': GetGrid := 'LN33';
                                 'X': GetGrid := 'LN13';
                                 'Y': GetGrid := 'LN05';
                                 END;
                        END;
                    END;

                IF ID = 'UA9' THEN
                    BEGIN
                    Oblast := GetOblast (Call);

                    CASE Oblast [1] OF
                        '8': CASE Oblast [2] OF
                                 'T': GetGrid := 'LO26';
                                 'V': GetGrid := 'KO47';
                                 END;

                        '9': CASE Oblast [2] OF
                                 'A', 'B': GetGrid := 'MO05';
                                 'C', 'D', 'E': GetGrid := 'MO06';
                                 'F': GetGrid := 'LO88';
                                 'G': GetGrid := 'LP70';
                                 'H', 'I': GetGrid := 'NO26';
                                 'J': GetGrid := 'MP50';
                                 'K': GetGrid := 'MP26';
                                 'L': GetGrid := 'MO27';
                                 'M', 'N': GetGrid := 'MO65';
                                 'O', 'P': GetGrid := 'NO15';
                                 'Q', 'R': GetGrid := 'MO25';
                                 'S', 'T': GetGrid := 'LO71';
                                 'U', 'V': GetGrid := 'NO35';
                                 'W': GetGrid := 'LO84';
                                 'X': GetGrid := 'LP63';
                                 'Y': GetGrid := 'NO12';
                                 'Z': GetGrid := 'NO31';
                                 END;

                        '0': CASE Oblast [2] OF
                                 'A': GetGrid := 'NO66';
                                 'B': GetGrid := 'OQ26';
                                 'C': GetGrid := 'PN78';
                                 'D': GetGrid := 'PN68';
                                 'E', 'F', 'G': GetGrid := 'QO11';
                                 'H': GetGrid := 'OO29';
                                 'I': GetGrid := 'QO59';
                                 'J': GetGrid := 'PO80';
                                 'K': GetGrid := 'RP96';
                                 'L', 'M', 'N': GetGrid := 'PN53';
                                 'O', 'P': GetGrid := 'OO43';
                                 'Q', 'R': GetGrid := 'PP00';
                                 'S', 'T': GetGrid := '0022';
                                 'U', 'V': GetGrid := 'OO62';
                                 'W': GetGrid := 'N053';
                                 'X': GetGrid := 'RO06';
                                 'Y': GetGrid := 'NO83';
                                 'Z': GetGrid := 'RO06';
                                 END;
                        END;
                    END;

                IF ID = 'UK' THEN   { Uzbekistan }
                     BEGIN
                     Oblast := GetOblast (Call);

                     CASE Oblast [2] OF
                         'A', 'B': GetGrid := 'MN41';
                         'C': GetGrid := 'MM28';
                         'D': GetGrid := 'MN40';
                         'F': GetGrid := 'MN60';
                         'G': GetGrid := 'MN50';
                         'I': GetGrid := 'MM39';
                         'L': GetGrid := 'MM29';
                         'O': GetGrid := 'MN51';
                         'Q': GetGrid := 'MN20';
                         'T': GetGrid := 'MM37';
                         'U': GetGrid := 'MN01';
                         'V': GetGrid := 'MN30';
                         'Z': GetGrid := 'LN92';
                         END;
                     END;

                IF ID = 'UN' THEN   { Kazakhstan }
                     BEGIN
                     Oblast := GetOblast (Call);

                     CASE Oblast [2] OF
                         'A': GetGrid := 'LN74';
                         'B': GetGrid := 'MO51';
                         'C': GetGrid := 'MO44';
                         'D': GetGrid := 'NN09';
                         'E': GetGrid := 'MO53';
                         'F': GetGrid := 'MO82';
                         'G': GetGrid := 'MN84';
                         'I': GetGrid := 'LN98';
                         'J': GetGrid := 'NO10';
                         'K': GetGrid := 'MN25';
                         'L': GetGrid := 'MO12';
                         'M': GetGrid := 'LO50';
                         'N': GetGrid := 'MN42';
                         'O': GetGrid := 'LN77';
                         'P': GetGrid := 'MN69';
                         'Q': GetGrid := 'MN84';
                         'R': GetGrid := 'MN37';
                         'S': GetGrid := 'MN15';
                         'T': GetGrid := 'MN64';
                         'V': GetGrid := 'MN94';
                         'Y': GetGrid := 'MO30';
                         END;
                     END;

                IF ID = 'UR' THEN
                     BEGIN
                     Oblast := GetOblast (Call);

                     CASE Oblast [2] OF
                         'A': GetGrid := 'KO62';
                         'B': GetGrid := 'KN29';
                         'C': GetGrid := 'KN69';
                         'D': GetGrid := 'KN18';
                         'E': GetGrid := 'KN78';
                         'F': GetGrid := 'KN57';
                         'G': GetGrid := 'KN66';
                         'H': GetGrid := 'KN79';
                         'I': GetGrid := 'KN88';

                         'J': BEGIN
                              IF Oblast [1] = '9' THEN
                                  GetGrid := 'KN64'   { Sevastopol }
                              ELSE
                                  GetGrid := 'KN75';  { Crimea }
                              END;

                         'K': GetGrid := 'KO30';
                         'L': GetGrid := 'KN89';
                         'M': GetGrid := 'KN98';
                         'N': GetGrid := 'KN48';
                         'P': GetGrid := 'KO20';
                         'Q': GetGrid := 'KN77';
                         'R': GetGrid := 'KO62';
                         'S': GetGrid := 'KN28';
                         'T': GetGrid := 'KN39';
                         'U': GetGrid := 'KO70';
                         'V': GetGrid := 'KN68';
                         'W': GetGrid := 'KN29';
                         'X': GetGrid := 'KO40';
                         'Y': GetGrid := 'KN28';
                         'Z': GetGrid := 'KN66';
                         END;
                     END;

                IF ID = 'VE' THEN
                    BEGIN
                    CASE GetNumber (Call) OF
                        '1': GetGrid := 'FN75';
                        '2': GetGrid := 'FN25';
                        '3': GetGrid := 'FN04';
                        '4': GetGrid := 'EN19';
                        '5': GetGrid := 'DO72';
                        '6': GetGrid := 'DO33';
                        '7': GetGrid := 'CN99';
                        '8': GetGrid := 'DP20';
                        '9': GetGrid := 'FN65';
                        END;

                    IF StringHas (Call, 'VY1') THEN
                        GetGrid := 'CP20';

                    IF StringHas (Call, 'VY2') THEN
                        GetGrid := 'FN76';

                    END;

                IF ID = 'VK' THEN
                    CASE GetNumber (Call) OF
                        '1': GetGrid := 'QF56';
                        '2': GetGrid := 'QF46';
                        '3': GetGrid := 'QF22';
                        '4': GetGrid := 'QG53';
                        '5': GetGrid := 'PF85';
                        '6': GetGrid := 'OF88';
                        '7': GetGrid := 'QE37';
                        '8': GetGrid := 'PH65';
                        END;

                IF ID = 'XE' THEN
                    CASE GetNumber (Call) OF
                        '1': GetGrid := 'EK09';
                        '2': GetGrid := 'DM21';
                        '3': GetGrid := 'EK48';
                        END;


                IF ID = 'YB' THEN
                    CASE GetNumber (Call) OF
                        '0', '1': GetGrid := 'OI33';
                        '2': GetGrid := 'OI52';
                        '3': GetGrid := 'OI62';
                        '4': GetGrid := 'OI27';
                        '5': GetGrid := 'OJ00';
                        '6': GetGrid := 'NJ93';
                        '7': GetGrid := 'OI68';
                        '8': GetGrid := 'PI08';
                        '9': GetGrid := 'OI07';
                        END;


                IF ID = 'YV' THEN
                    CASE GetNumber (Call) OF
                        '1': GetGrid := 'FK30';
                        '2': GetGrid := 'FJ48';
                        '3': GetGrid := 'FK40';
                        '4': GetGrid := 'FJ57';
                        '5': GetGrid := 'FK60';
                        '6': GetGrid := 'FJ86';
                        '7': GetGrid := 'FK80';
                        '8': GetGrid := 'FJ99';
                        '9': GetGrid := 'FJ63';
                        END;

                IF ID = 'ZL' THEN
                    CASE GetNumber (Call) OF
                        '1': GetGrid := 'RF73';
                        '2': GetGrid := 'RE68';
                        '3': GetGrid := 'RE55';
                        '4': GetGrid := 'RE44';
                        END;

                IF ID = 'ZP' THEN
                    CASE GetNumber (Call) OF
                        '1': GetGrid := 'GG08';
                        '2': GetGrid := 'GG06';
                        '3': GetGrid := 'GG17';
                        '4': GetGrid := 'GG16';
                        '5': GetGrid := 'GG14';
                        '6': GetGrid := 'GG14';
                        '7': GetGrid := 'GG25';
                        '8': GetGrid := 'GG13';
                        '9': GetGrid := 'GG24';
                        END;

                IF ID = 'ZS' THEN
                    CASE GetNumber (Call) OF
                        '1': GetGrid := 'KF07';
                        '2': GetGrid := 'KF26';
                        '3': GetGrid := 'KG12';
                        '4': GetGrid := 'KG31';
                        '5': GetGrid := 'KG50';
                        '6': GetGrid := 'KG44';
                        END;

                Exit;
                END;

            IF CurrentPrefixRecord <> Nil THEN
                CurrentPrefixRecord := CurrentPrefixRecord^.NextRecord;
        UNTIL CurrentPrefixRecord = Nil;

        fullcall := false;
        Delete (ShortCall, Length (ShortCall), 1);

        IF PossibleUSACall THEN
            IF (Length (ShortCall) = 3) OR (Length (ShortCall) = 4) THEN
                BEGIN
                IF Copy (ShortCall, 1, 3) = 'KC4' THEN
                    ShortCall := 'K4BAI';

                IF Copy (ShortCall, 1, 3) = 'KC6' THEN
                    ShortCall := 'K6OVJ';

                IF Copy (ShortCall, 1, 3) = 'KG4' THEN
                     ShortCall := 'K4BAI';
                END;
        END;
    END;



FUNCTION CountryTableObject.GetContinentName (Continent: ContinentType): Str40;

    BEGIN
    CASE Continent OF
        NorthAmerica: GetContinentName := 'North America';
        SouthAmerica: GetContinentName := 'South America';
        Europe:       GetContinentName := 'Europe';
        Africa:       GetContinentName := 'Africa';
        Oceania:      GetContinentName := 'Oceania';
        Asia:         GetContinentName := 'Asia';
        ELSE GetContinentName := 'Unknown';
        END;
    END;



FUNCTION CountryTableObject.GetContinent (Call: CallString): ContinentType;

VAR CurrentPrefixRecord: PrefixRecPtr;
    Index: INTEGER;
    ShortCall: CallString;
    PossibleUSACall: BOOLEAN;
    fullcall: boolean;
    CallTotal: CallString;

    BEGIN
    GetContinent := UnknownContinent;

    CallTotal := UpCase(Call);
    Call := StandardCallFormat (CallTotal, True);

    ShortCall := Call;

    PossibleUSACall := ((Copy (Call, 1, 3) = 'KC6') OR
                        (Copy (Call, 1, 3) = 'KC4') OR
                        (Copy (Call, 1, 3) = 'KG4')) AND (Length (Call) > 3);

    IF PossibleUSACall THEN
        BEGIN
        IF Copy (Call, 1, 3) = 'KC6' THEN PossibleUSACall := Length (Call) <> 5;
        IF Copy (Call, 1, 3) = 'KG4' THEN PossibleUSACall := Length (Call) <> 5;
        END;

    Index := RecordPointerIndex (ShortCall [1]);

    IF (Index <= 0) OR (FirstPrefixRecord [Index] = Nil) THEN Exit;

    fullcall := true;
    WHILE ShortCall <> '' DO
        BEGIN
        CurrentPrefixRecord := FirstPrefixRecord [Index];

        REPEAT
            IF (
                  ((not CurrentPrefixRecord^.exactcall) and
                     (CurrentPrefixRecord^.Prefix = ShortCall))
               OR
                  ((fullcall and CurrentPrefixRecord^.exactcall) and
                     (CurrentPrefixRecord^.Prefix = CallTotal))
                ) THEN
                BEGIN
                IF CurrentPrefixRecord^.Continent <> UnknownContinent THEN
                    BEGIN
                    GetContinent := CurrentPrefixRecord^.Continent;
                    Exit;
                    END;

                GetContinent := CountryInfoTable^ [CurrentPrefixRecord^.Country].DefaultContinent;
                Exit;
                END;

            IF CurrentPrefixRecord <> Nil THEN
                CurrentPrefixRecord := CurrentPrefixRecord^.NextRecord;
        UNTIL CurrentPrefixRecord = Nil;
        fullcall := false;
        Delete (ShortCall, Length (ShortCall), 1);

        IF PossibleUSACall THEN
            IF (Length (ShortCall) = 3) OR (Length (ShortCall) = 4) THEN
                BEGIN
                IF Copy (ShortCall, 1, 3) = 'KC4' THEN
                    ShortCall := 'K4BAI';

                IF Copy (ShortCall, 1, 3) = 'KC6' THEN
                    ShortCall := 'K6OVJ';

                IF Copy (ShortCall, 1, 3) = 'KG4' THEN
                     ShortCall := 'K4BAI';
                END;
        END;

    END;



FUNCTION RemoveContinentStringAndDetermineContinent (VAR FileString: Str160): ContinentType;

VAR ContinentString: Str80;
    Continent: ContinentType;

    BEGIN
    Continent := UnknownContinent;

    ContinentString := LastString (FileString);

    IF ContinentString = 'AS' THEN Continent := Asia;
    IF ContinentString = 'AF' THEN Continent := Africa;
    IF ContinentString = 'EU' THEN Continent := Europe;
    IF ContinentString = 'OC' THEN Continent := Oceania;
    IF ContinentString = 'NA' THEN Continent := NorthAmerica;
    IF ContinentString = 'SA' THEN Continent := SouthAmerica;

    RemoveContinentStringAndDetermineContinent := Continent;

    IF Continent <> UnknownContinent THEN
        BEGIN
        RemoveContinentStringAndDetermineContinent := Continent;
        RemoveLastString (FileString);
        Exit;
        END;
    END;



FUNCTION DetermineContinentFromZone (Zone: INTEGER; ITUZones: BOOLEAN): ContinentType;

VAR Continent: ContinentType;

    BEGIN
    Continent := UnknownContinent;

    IF ITUZones THEN
        BEGIN
        CASE Zone OF
            1,2,3,4,5,6,7,8,9,10,11:    Continent := NorthAmerica;
            12,13,14,15,16:             Continent := SouthAmerica;
            17,18,19,20:                Continent := Europe;
            21,22,23,24,25,26:          Continent := Asia;
            27,28,29:                   Continent := Europe;
            30,31,32,33,34,35:          Continent := Asia;
            36:                         Continent := Europe;
            37,38:                      Continent := Africa;
            39,40,41,42,43,44,45:       Continent := Asia;
            46,47,48:                   Continent := Africa;
            49:                         Continent := Asia;
            50,51:                      Continent := Oceania;
            52,53:                      Continent := Africa;
            54:                         Continent := Asia;
            55,56:                      Continent := Oceania;
            57:                         Continent := Africa;
            58,59,60,61,62:             Continent := Oceania;
            63,64,65:                   Continent := Oceania;
            66,67:                      Continent := Africa;
            68:                         Continent := Oceania;
            69:                         Continent := Africa;
            70,71:                      Continent := Oceania;
            72:                         Continent := SouthAmerica;
            73:                         Continent := SouthAmerica;
            74:                         Continent := SouthAmerica;
            75:                         Continent := NorthAmerica;
            END;
        END
    ELSE
        BEGIN
        CASE Zone OF
            1,2,3,4,5,6,7,8:            Continent := NorthAmerica;
            9,10,11,12,13:              Continent := SouthAmerica;
            14,15,16,20:                Continent := Europe;
            17,18,19,21,22,23,24,25,26: Continent := Asia;
            27,28,29,30,31,32:          Continent := Oceania;
            33,34,35,36,37,38,39:       Continent := Africa;
            40:                         Continent := Europe;
            END;
        END;

    DetermineContinentFromZone := Continent;
    END;



PROCEDURE CountryTableObject.MakeDefaultRemainingCountryList;

CONST
    NumberDefaultCountries = 99;
    BigNumberDefaultCountries = 147;

    DefaultRemMults: ARRAY [1..NumberDefaultCountries] OF STRING [3] =

('4J',  '4L',  '4X',  '6Y',  '8P',  '9A', '9M2', '9M6',  '9Y',  'BV',
 'BY',  'C6',  'CE',  'CM',  'CT', 'CT3',  'CU',  'DL',  'DU',  'EA',
 'EA6','EA8', 'EA9',  'EI',  'EK',  'ER',  'ES',  'EU',  'EX',  'EY',  'EZ',
  'F',  'FM',   'G',  'GD',  'GI',  'GJ',  'GM',  'GW',  'HA',  'HB',  'HC',
'HC8',  'HI',  'HK',  'HL',   'I',  'IS',  'J6',  'JA',   'K', 'KH2', 'KH6',
 'KL', 'KP2', 'KP4',  'LA',  'LU',  'LX',  'LY',  'LZ',  'OA',  'OE',  'OH',
 'OK',  'OM',  'ON',  'OZ',  'P4',  'PA', 'PJ2', 'PJ7',  'PY',  'S5',  'SM',
 'SP',  'SV',  'TG',  'TI',  'UA', 'UA2', 'UA9',  'UK',  'UN',  'UR',  'VE',
 'VK',  'VU',  'XE',  'YB',  'YL',  'YO',  'YU',  'YV',  'Z3',  'ZA',  'ZF',
 'ZL',  'ZS');

  BigDefaultRemMults: ARRAY [1..BigNumberDefaultCountries] OF STRING [4] =

('3B8',  '3V',  '4J',  '4L', '4U1I', '4U1U',  '4S',  '4X',  '5B',  '6Y',
  '7X',  '8P',  '8R',  '9A',   '9H',  '9K',  '9M2', '9M6',  '9V',  '9Y',
  'A7',  'BV',  'BY',  'C3',   'C6',  'CE',   'CM',  'CN',  'CP',  'CT',
 'CT3',  'CU',  'CX',  'D4',   'DL',  'DU',   'EA', 'EA6', 'EA8', 'EA9',
  'EI',  'EK',  'ER',  'ES',   'EU',  'EX',   'EY',  'EZ',   'F',  'FG',
  'FK',  'FM',  'FO',  'FR',   'FY',   'G',   'GD',  'GI',  'GJ',  'GM',
  'GW',  'HA',  'HB', 'HB0',   'HC', 'HC8',   'HH',  'HI',  'HK',  'HL',
  'HP',  'HS',  'HZ',   'I',  'IG9',  'IS',  'IT9',  'J6',  'JA',   'K',
 'KG4', 'KH2', 'KH6',  'KL',  'KP2', 'KP4',   'LA',  'LU',  'LX',  'LY',
  'LZ',  'OA',  'OD',  'OE',  'OH',  'OH0',   'OK',  'OM',  'ON',  'OY',
  'OZ',  'P4',  'PA', 'PJ2', 'PJ7',   'PY', 'PY0F',  'S5',  'SM',  'SP',
  'SU',  'SV', 'SV5', 'SV9',  'T9',   'TA',  'TA1',  'TF',  'TG',  'TI',
  'TK',  'TR',  'TU',  'UA', 'UA2',  'UA9',  'UK',   'UN',  'UR',  'VE',
  'VK','VK9N', 'VS6',  'VU',  'XE',  'XX9',  'YB',   'YL',  'YO',  'YU',
  'YV',  'Z3',  'ZA', 'ZD8',  'ZF',   'ZL',  'ZS');

VAR Index: INTEGER;

    BEGIN
    NumberRemainingCountries := 0;

    IF RemainingDXMultTemplate = Nil THEN
        BEGIN
        IF SizeOf (RemainingDXMultTemplate^) >= MaxAvail THEN
            BEGIN
            ReportError ('Insufficient memory for RemainingDXMultTemplate!!');
            Halt;
            END;

        New (RemainingDXMultTemplate);
        END;

    IF BigRemainingList THEN
        BEGIN
        FOR Index := 1 TO BigNumberDefaultCountries DO
            BEGIN
            CompressFormat (UpperCase (BigDefaultRemMults [Index]), RemainingDXMultTemplate^ [NumberRemainingCountries]);
            Inc (NumberRemainingCountries);
            END;
        END

    ELSE
        FOR Index := 1 TO NumberDefaultCountries DO
            BEGIN
            CompressFormat (UpperCase (DefaultRemMults [Index]), RemainingDXMultTemplate^ [NumberRemainingCountries]);
            Inc (NumberRemainingCountries);
            END;
    END;



FUNCTION GetNextPrefixEntry (VAR TempString: Str80): Str80;

    BEGIN
    IF Pos (',', TempString) > 0 THEN
        BEGIN
        GetNextPrefixEntry := PrecedingString (TempString, ',');
        TempString := PostcedingString (TempString, ',');
        END
    ELSE
        IF Pos (';', TempString) > 0 THEN
            BEGIN
            GetNextPrefixEntry := PrecedingString (TempString, ';');
            TempString := '';
            END;
    END;



FUNCTION CountryTableObject.LoadInCountryFile: BOOLEAN;

VAR FileRead: TEXT;
    FileString: STRING;
    PrefixString, Grid, CountryID, TempString1, CountryName: Str40;
    RecordPointer: INTEGER;
    CurrentRecord, NextRecord: PrefixRecPtr;
    TempString, FileName: Str160;
    UTCOffset: INTEGER;

    CQZOne, ITUZone: INTEGER;
    Continent: ContinentType;
    Lat, Lon: REAL;

    BEGIN
    LoadInCountryFile := False;
    CustomRemainingCountryListFound := False;

    NumberCountries := 0;
    NumberRemainingCountries := 0;


    FileName := FindDirectory ('CTY.DAT') + DirectorySeparator + 'CTY.DAT';

    IF CountryInfoTable = Nil THEN
        BEGIN
        IF MaxAvail > SizeOf (CountryInfoTable^) THEN
            New (CountryInfoTable)
        ELSE
            BEGIN
            ReportError ('Not enough room for country table!!');
            Halt;
            END;
        END;

    FOR RecordPointer := 1 TO 36 DO
        BEGIN
        IF FirstPrefixRecord [RecordPointer] <> Nil THEN
            BEGIN
            CurrentRecord := FirstPrefixRecord [RecordPointer];
            NextRecord    := FirstPrefixRecord [RecordPointer]^.NextRecord;

            WHILE CurrentRecord <> Nil DO
                BEGIN
                Dispose (CurrentRecord);
                CurrentRecord := NextRecord;
                IF CurrentRecord <> Nil THEN
                    NextRecord := CurrentRecord^.NextRecord;
                END;
            END;

        FirstPrefixRecord [RecordPointer] := Nil;
        LastPrefixRecord  [RecordPointer] := Nil;
        END;

    IF NOT OpenFileForRead (FileRead, FileName) THEN Exit;

    WHILE NOT Eof (FileRead) DO
        BEGIN
        ReadLn (FileRead, FileString);

        GetRidOfPrecedingSpaces (FileString);

        IF StringHas (UpperCase (FileString), 'REMAINING MULTS') THEN
            BEGIN
            LoadInCountryFile := True;
            WriteLn ('There were ', NumberCountries, ' countries loaded in from ', FileName);

            { Now to do the remaining mult stuff }

            CustomRemainingCountryListFound := True;

            IF RemainingDXMultTemplate = Nil THEN
                BEGIN
                IF SizeOf (RemainingDXMultTemplate^) >= MaxAvail THEN
                    BEGIN
                    ReportError ('Insufficient memory for RemainingDXMultTemplate!!');
                    Halt;
                    END;

                New (RemainingDXMultTemplate);
                END;

            WHILE NOT Eof (FileRead) DO
                BEGIN
                ReadLn (FileRead, FileString);
                GetRidOfPrecedingSpaces (FileString);

                WHILE FileString <> '' DO
                    BEGIN
                    PrefixString := UpperCase (RemoveFirstString (FileString));
                    CompressFormat (PrefixString, RemainingDXMultTemplate^ [NumberRemainingCountries]);
                    Inc (NumberRemainingCountries);
                    END;
                END;

            Close (FileRead);
            Exit;
            END;

        { Not remaining mults entry }

        IF StringHas (FileString, ':') THEN   { Country info line }
            BEGIN
            TempString := PrecedingString (FileString, ':');
            FileString := UpperCase (PostcedingString (FileString, ':'));
            GetRidOfPrecedingSpaces (FileString);
            GetRidOfPostcedingSpaces (TempString);

            CountryName := TempString;

            TempString := PrecedingString (FileString, ':');
            FileString := PostcedingString (FileString, ':');
            GetRidOfPrecedingSpaces (FileString);
            GetRidOfPostcedingSpaces (TempString);

            Val (TempString, CountryInfoTable^ [NumberCountries].DefaultCQZone);

            TempString := PrecedingString (FileString, ':');
            FileString := PostcedingString (FileString, ':');
            GetRidOfPrecedingSpaces (FileString);
            GetRidOfPostcedingSpaces (TempString);

            Val (TempString, CountryInfoTable^ [NumberCountries].DefaultITUZone);

            TempString := PrecedingString (FileString, ':');
            FileString := PostcedingString (FileString, ':');
            GetRidOfPrecedingSpaces (FileString);
            GetRidOfPostcedingSpaces (TempString);

            CountryInfoTable^ [NumberCountries].DefaultContinent := RemoveContinentStringAndDetermineContinent (TempString);

            TempString := PrecedingString (FileString, ':');
            FileString := PostcedingString (FileString, ':');
            GetRidOfPrecedingSpaces (FileString);
            GetRidOfPostcedingSpaces (TempString);

            Val (TempString, Lat);

            TempString := PrecedingString (FileString, ':');
            FileString := PostcedingString (FileString, ':');
            GetRidOfPrecedingSpaces (FileString);
            GetRidOfPostcedingSpaces (TempString);

            Val (TempString, Lon);

            CountryInfoTable^ [NumberCountries].DefaultGrid := ConvertLatLonToGrid (Lat, Lon);

            TempString := PrecedingString (FileString, ':');
            FileString := PostcedingString (FileString, ':');
            GetRidOfPrecedingSpaces (FileString);
            GetRidOfPostcedingSpaces (TempString);

            Val (TempString, UTCOffset);

            TempString := PrecedingString (FileString, ':');
            FileString := PostcedingString (FileString, ':');
            GetRidOfPrecedingSpaces (FileString);
            GetRidOfPostcedingSpaces (TempString);

            CountryID := TempString;

            CountryInfoTable^ [NumberCountries].UTCOffset := UTCOffset;
            CountryInfoTable^ [NumberCountries].ID        := CountryID;
            CountryInfoTable^ [NumberCountries].Name      := CountryName;
            Inc (NumberCountries);

            IF NumberCountries = MaxCountries THEN
                BEGIN
                ReportError ('Too many entries found in country file!');
                WriteLn ('Maximum countries = ', MaxCountries);
                Halt;
                END;

            NaturalPrefixFound := False;

            REPEAT
                ReadLn (FileRead, FileString);

                TempString := UpperCase (FileString);

                GetRidOfPrecedingSpaces (TempString);

                WHILE TempString <> '' DO
                    BEGIN
                    PrefixString := GetNextPrefixEntry (TempString);

                    IF StringHas (PrefixString, '(') THEN { CQ Zone override }
                        BEGIN
                        TempString1 := BracketedString (PrefixString, '(', ')');

                        Val (TempString1, CQZone);

                        PrefixString := PrecedingString  (PrefixString, '(') +
                                        PostcedingString (PrefixString, ')');
                        END
                    ELSE
                        CQZone := 0;

                    IF StringHas (PrefixString, '[') THEN { ITU override }
                        BEGIN
                        TempString1 := BracketedString (PrefixString, '[', ']');

                        Val (TempString1, ITUZone);

                        PrefixString := PrecedingString  (PrefixString, '[') +
                                        PostcedingString (PrefixString, ']');
                        END
                    ELSE
                        ITUZone := 0;

                    IF StringHas (PrefixString, '<') THEN { Lat/lon override }
                        BEGIN
                        TempString1 := BracketedString (PrefixString, '<', '>');

                        Val (PrecedingString  (TempString1, '/'), Lat);
                        Val (PostcedingString (TempString1, '/'), Lon);

                        Grid := ConvertLatLonToGrid (Lat, Lon);

                        PrefixString := PrecedingString  (PrefixString, '<') +
                                        PostcedingString (PrefixString, '>');
                        END
                    ELSE
                        Grid := '';

                    IF StringHas (PrefixString, '{') THEN { Cont override }
                        BEGIN
                        TempString1 := BracketedString (PrefixString, '[', ']');

                        Continent := RemoveContinentStringAndDetermineContinent (TempString1);

                        PrefixString := PrecedingString (PrefixString, '{');
                        END
                    ELSE
                        Continent := UnknownContinent;

                    { All overrides taken care of }

                    IF UpperCase (PrefixString) = UpperCase (CountryID) THEN
                        NaturalPrefixFound := True;
//ks here
                    if pos('=',prefixstring) = 1 then
                    begin
                       delete(prefixstring,1,1);
                       AddNewPrefixRecord (PrefixString,
                                        NumberCountries - 1,
                                        Continent,
                                        CQZone,
                                        ITUZone,
                                        Grid,true);
                    end else begin
                       AddNewPrefixRecord (PrefixString,
                                        NumberCountries - 1,
                                        Continent,
                                        CQZone,
                                        ITUZone,
                                        Grid,false);
                    end;
                    END;

            UNTIL Copy (FileString, Length (FileString), 1) = ';';

            IF NumberCountries > 0 THEN
                IF NOT NaturalPrefixFound THEN
                    AddNewPrefixRecord (CountryInfoTable^ [NumberCountries - 1].ID,
                                        NumberCountries - 1,
                                        CountryInfoTable^ [NumberCountries - 1].DefaultContinent,
                                        CountryInfoTable^ [NumberCountries - 1].DefaultCQZone,
                                        CountryInfoTable^ [NumberCountries - 1].DefaultITUZone, '',false);
            END;
        END;

    LoadInCountryFile := True;
    WriteLn ('There were ', NumberCountries, ' countries loaded in from ', FileName);
    Close (FileRead);

    MakeDefaultRemainingCountryList;
    END;




FUNCTION ScandinavianCountry (CountryID: Str20): BOOLEAN;

    BEGIN
    ScandinavianCountry := (CountryID = 'LA')   OR (CountryID = 'JW')   OR
                           (CountryID = 'JX')   OR (CountryID = 'OH')   OR
                           (CountryID = 'OH0')  OR (CountryID = 'OJ0')  OR
                           (CountryID = 'OX')   OR (CountryID = 'OY')   OR
                           (CountryID = 'OZ')   OR (CountryID = 'SM')   OR
                           (CountryID = 'TF');
    END;



FUNCTION GetFirstSuffixLetter (Call: CallString): CHAR;

{ This function will get the first letter after the last number in the
  callsign or portable designator.  If the call does not have a letter
  after the last number, or if the portable designator does not have
  it, a null character will be returned.                             }

VAR CharPtr: INTEGER;
    TempString: Str80;

    BEGIN
    IF StringHas (Call, '/') THEN
        BEGIN
        TempString := PostcedingString (Call, '/');
        GetFirstSuffixLetter := GetFirstSuffixLetter (TempString);
        END
    ELSE
        BEGIN
        FOR CharPtr := Length (Call) - 1 DOWNTO 1 DO
            IF (Call [CharPtr] >= '0') AND (Call [CharPtr] <= '9') THEN
                BEGIN
                GetFirstSuffixLetter := Call [CharPtr + 1];
                Exit;
                END;
        GetFirstSuffixLetter := Chr (0);
        END;
    END;



FUNCTION  GetNumber (Call: CallString): CHAR;

{ This function will look at the callsign passed to it and return the
  single number that is in it.  If the call is portable, the number from
  the portable designator will be given if there is one.  If the call
  or prefix has two numbers in it, the last one will be given.         }

VAR CharPtr: INTEGER;

    BEGIN
    IF StringHas (Call, '/') THEN
        Call := PrecedingString (Call, '/');

    FOR CharPtr := Length (Call) DOWNTO 1 DO
        IF (Call [CharPtr] <= '9') AND (Call [CharPtr] >= '0') THEN
            BEGIN
            GetNumber := Call [CharPtr];
            Exit;
            END;
    GetNumber := Chr (0);
    END;



FUNCTION DXCall (Call: CallString): BOOLEAN;

VAR CountryID: Str20;

    BEGIN
    CountryID := CountryTable.GetCountryID (CountryTable.GetCQCountry (Call, True));
    DXCall := (CountryID <> 'K') AND (CountryID <> 'VE');
    END;



PROCEDURE CountryInit;

VAR RecordPointer: INTEGER;
    CurrentRecord, NextRecord: PrefixRecPtr;

    BEGIN
    CountryTable.CountryMode := CQCountryMode;
    CountryTable.ZoneMode    := CQZoneMode;

    FOR RecordPointer := 1 TO 36 DO
        BEGIN
        IF CountryTable.FirstPrefixRecord [RecordPointer] <> Nil THEN
            BEGIN
            CurrentRecord := CountryTable.FirstPrefixRecord [RecordPointer];
            NextRecord    := CountryTable.FirstPrefixRecord [RecordPointer]^.NextRecord;

            WHILE CurrentRecord <> Nil DO
                BEGIN
                CurrentRecord := Nil;
                CurrentRecord := NextRecord;
                NextRecord := CurrentRecord^.NextRecord;
                END;
            END;

        CountryTable.FirstPrefixRecord [RecordPointer] := Nil;
        CountryTable.LastPrefixRecord  [RecordPointer] := Nil;
        END;

    CountryTable.CountryInfoTable := Nil;
    CountryTable.RemainingDXMultTemplate := Nil;
    END;


    BEGIN
    CountryInit;
    END.
