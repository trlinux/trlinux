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

UNIT ZoneCont;

{$O+}

INTERFACE

USES Tree, Country9;

CONST
  UpdateListDate = '3 Apr 93';

TYPE
    DomesticCountryRecordPointer = ^DomesticCountryRecord;

    DomesticCountryRecord = RECORD
        CountryID: STRING [6];
        NextRecord: DomesticCountryRecordPointer;
        END;

VAR FirstDomesticCountryRecord: DomesticCountryRecordPointer;
    LastLocateCall: CallString;
    LastLocateQTH:  QTHRecord;

    PROCEDURE AddDomesticCountry (ID: CallString);
    PROCEDURE ClearDomesticCountryList;
    FUNCTION  DomesticCountryCall (Call: CallString): BOOLEAN;

    FUNCTION  ClubCall (Call: CallString): BOOLEAN;

    FUNCTION  GetVEInitialExchange (Call: CallString): Str20;

    PROCEDURE LocateCall (Call: CallString;
                          VAR QTH: QTHRecord;
                          UseStandardCallFormat: BOOLEAN);

    FUNCTION  GetContinentName (Cont: ContinentType): Str20;

    FUNCTION  SACDistrict (QTH: QTHRecord): Str20;



IMPLEMENTATION


FUNCTION GetVEInitialExchange (Call: CallString): Str20;

    BEGIN
    GetVEInitialExchange := '';

    Call := StandardCallFormat (Call, True);

    IF CountryTable.GetCountryID (CountryTable.GetCountry (Call, False)) <> 'VE' THEN
        Exit;

    Call := Copy (Call, 1, 3);

    IF StringHas (Call, '3') THEN GetVEInitialExchange := 'Ont ';
    IF StringHas (Call, '4') THEN GetVEInitialExchange := 'Man';
    IF StringHas (Call, '5') THEN GetVEInitialExchange := 'Sask';
    IF StringHas (Call, '6') THEN GetVEInitialExchange := 'Ab';
    IF StringHas (Call, '7') THEN GetVEInitialExchange := 'Bc';
    IF StringHas (Call, '8') THEN GetVEInitialExchange := 'NWT';

    IF Call = 'VE2' THEN GetVEInitialExchange := 'Que';
    IF Call = 'VY2' THEN GetVEInitialExchange := 'vy2';
    IF Call = 'VY1' THEN GetVEInitialExchange := 'Yuk';
    IF Call = 'VO1' THEN GetVEInitialExchange := 'vo1';
    IF Call = 'VO2' THEN GetVEInitialExchange := 'vo2';
    IF Call = 'VE1' THEN GetVEInitialExchange := 've1';
    END;



PROCEDURE AddDomesticCountry (ID: CallString);

VAR ActiveRecord: DomesticCountryRecordPointer;

    BEGIN
    ID := UpperCase (ID);

    IF FirstDomesticCountryRecord = nil THEN
        BEGIN
        FirstDomesticCountryRecord := New (DomesticCountryRecordPointer);
        FirstDomesticCountryRecord^.CountryID  := ID;
        FirstDomesticCountryRecord^.NextRecord := nil;
        Exit;
        END;

    ActiveRecord := FirstDomesticCountryRecord;

    WHILE ActiveRecord^.NextRecord <> nil DO
        ActiveRecord := ActiveRecord^.NextRecord;

    ActiveRecord^.NextRecord := New (DomesticCountryRecordPointer);

    ActiveRecord := ActiveRecord^.NextRecord;

    ActiveRecord^.CountryID  := ID;
    ActiveRecord^.NextRecord := nil;
    END;



FUNCTION DomesticCountryCall (Call: CallString): BOOLEAN;

{ Returns TRUE if the callsign is in one of the countries identified as
  domestic countries. }

VAR ActiveRecord: DomesticCountryRecordPointer;
    Id: CallString;

    BEGIN
    ActiveRecord := FirstDomesticCountryRecord;

    IF ActiveRecord = nil THEN
        BEGIN
        DomesticCountryCall := False;
        Exit;
        END;

    ID := CountryTable.GetCountryID (CountryTable.GetCountry (Call, True));
    ID := UpperCase (ID);

    REPEAT
        IF ActiveRecord^.CountryID = ID THEN
            BEGIN
            DomesticCountryCall := True;
            Exit;
            END;

        ActiveRecord := ActiveRecord^.NextRecord;

    UNTIL ActiveRecord = nil;

    DomesticCountryCall := False;
    END;



PROCEDURE ClearDomesticCountryList;

VAR NextRecord, ActiveRecord: DomesticCountryRecordPointer;

    BEGIN
    ActiveRecord := FirstDomesticCountryRecord;

    WHILE ActiveRecord <> nil DO
        BEGIN
        NextRecord := ActiveRecord^.NextRecord;
        Dispose (ActiveRecord);
        ActiveRecord := NextRecord;
        END;
    END;



FUNCTION CallDistrict (Call: CallString): Str20;

VAR TempString: Str20;
    FoundLetter, FoundNumber: BOOLEAN;
    CharacterCount: INTEGER;

    BEGIN
    Call := StandardCallFormat (Call, True);

    IF StringHas (Call, '/') THEN Call := PrecedingString (Call, '/');

    CallDistrict := '';
    TempString := '';

    IF Call = '' THEN Exit;

    FoundLetter := False;
    FoundNumber := False;

    FOR CharacterCount := 1 TO Length (Call) DO
        IF NOT FoundLetter THEN
            BEGIN
            IF (Call [CharacterCount] >= 'A') AND (Call [CharacterCount] <= 'Z') THEN
                FoundLetter := True;
            END
        ELSE
            IF NOT FoundNumber THEN
                BEGIN
                IF (Call [CharacterCount] >= '0') AND (Call [CharacterCount] <= '9') THEN
                    BEGIN
                    TempString := Call [CharacterCount];
                    FoundNumber := True;
                    END;
                END
            ELSE
                IF Call [CharacterCount] > '9' THEN
                    BEGIN
                    CallDistrict := TempString;
                    Exit;
                    END
                ELSE
                    TempString := TempString + Call [CharacterCount];

    CallDistrict := TempString;
    END;


FUNCTION SACDistrict (QTH: QTHRecord): Str20;

VAR ID: STRING [6];

    BEGIN
    SACDistrict := '';

    ID := CountryTable.GetCountryID (QTH.Country);

    IF ID = 'LA' THEN SACDistrict := 'LA' + CallDistrict (QTH.Prefix);
    IF ID = 'SM' THEN SACDistrict := 'SM' + CallDistrict (QTH.Prefix);
    IF ID = 'OZ' THEN SACDistrict := 'OZ' + CallDistrict (QTH.Prefix);
    IF (ID = 'OH') OR (ID = 'OH0') OR (ID = 'OJ0') THEN
        SACDistrict := 'OH' + CallDistrict (QTH.Prefix);
    IF ID = 'OY' THEN SACDistrict := 'OY' + CallDistrict (QTH.Prefix);
    IF ID = 'OX' THEN SACDistrict := 'OX' + CallDistrict (QTH.Prefix);
    IF ID = 'TF' THEN SACDistrict := 'TF' + CallDistrict (QTH.Prefix);
    IF ID = 'JX' THEN SACDistrict := 'JX' + CallDistrict (QTH.Prefix);
    IF ID = 'JW' THEN SACDistrict := 'JW' + CallDistrict (QTH.Prefix);
    END;



FUNCTION ClubCall (Call: CallString): BOOLEAN;

VAR ID, StandardCall: Str20;

    BEGIN
    ClubCall := False;
    StandardCall := UpperCase (StandardCallFormat (Call, True));
    ID := CountryTable.GetCountryID (CountryTable.GetCountry (StandardCall, True));

    IF (ID = 'CT') OR (ID = 'CT3') THEN
        ClubCall := (Copy (Call, 1, 2) = 'CS') AND (Length (Call) = 6);

    IF ID = 'DL' THEN
        IF Call [1] = 'D' THEN
            ClubCall := (Copy (Call, 1, 3) = 'DF0') OR
                        (Copy (Call, 1, 3) = 'DK0') OR
                        (Copy (Call, 1, 3) = 'DL0');

    IF (ID = 'EA') OR (ID = 'EA6') OR (ID = 'EA8') OR (ID = 'EA9') THEN
            CASE Length (Call) OF
                4: ClubCall := True;
                6: ClubCall := Call[4]='R';
                END;

    IF (ID = 'ES') THEN
        CASE Length (Call) OF
            4: ClubCall := True;
            5: ClubCall := Call[4]>='W';
            END;

    IF (ID = 'F') THEN
        IF Call [2] = 'F' THEN ClubCall := True;

    IF ID = 'G'  THEN IF Call [2] = 'X' THEN ClubCall := True;
    IF ID = 'GD' THEN IF Call [2] = 'T' THEN ClubCall := True;
    IF ID = 'GI' THEN IF Call [2] = 'N' THEN ClubCall := True;
    IF ID = 'GJ' THEN IF Call [2] = 'H' THEN ClubCall := True;
    IF ID = 'GM' THEN IF Call [2] = 'S' THEN ClubCall := True;
    IF ID = 'GU' THEN IF Call [2] = 'P' THEN ClubCall := True;
    IF ID = 'GW' THEN IF Call [2] = 'C' THEN ClubCall := True;

    IF ID = 'JA' THEN
        IF Length (Call) = 6 THEN
            ClubCall := (Call[4]='Y') OR (Call[4]='Z');

    IF (ID = 'HA') OR (ID = 'LZ') OR (ID = 'OK') OR (ID = 'OM') OR
       (ID = 'SP') OR (ID = 'YO') THEN
           CASE Length (Call) OF
               4: ClubCall := True;
               6: ClubCall := Call [4] = 'W';
               END;

    IF ID = 'LY' THEN
        CASE Length (Call) OF
            4: ClubCall := True;
            5: ClubCall := Call[4]>='W';
            END;

    IF ID = 'PA' THEN
        ClubCall := (Copy (Call, 1, 3) = 'PI4') OR
                    (Copy (Call, 1, 3) = 'PI5') OR
                    (Copy (Call, 1, 3) = 'PI9');

    IF ID = 'OE' THEN IF Call [4] = 'X' THEN ClubCall := True;

    IF (ID = 'OH') OR (ID = 'OH0') OR (ID = 'OH0M') THEN
            IF Length (Call) = 5 THEN
                ClubCall := Call [4]='A';

    IF ID = 'S5' THEN
        ClubCall := (Length (Call) = 6) AND
                    ((Copy (Call, 1, 3) = 'S51') OR
                    (Copy (Call, 1, 3) = 'S52') OR
                    (Copy (Call, 1, 3) = 'S53') OR
                    (Copy (Call, 1, 3) = 'S59'));

    IF ID = 'SM' THEN
        ClubCall := (Copy (Call, 1, 2) = 'SI') OR
                    (Copy (Call, 1, 2) = 'SJ') OR
                    (Copy (Call, 1, 2) = 'SK');

    IF (ID = 'UA') OR (ID = 'UA2') OR (ID = 'UA0') OR (ID = 'UJ') OR
       (ID = 'UN') OR (ID = 'UR')  OR (ID = '4K2') THEN
           IF Length (Call) = 6 THEN
               ClubCall := (Call[5]>='W') AND (Call[5]<='Z');

    IF ID = 'K' THEN
        ClubCall := (Call = 'W1AW') OR (Call = 'W1MX') OR
                    (Call = 'W6LS') OR (Call = 'W6YRA') OR
                    (Call = 'W9YB') OR (Call = 'W9YH') OR
                    (Call = 'W9YT');

    IF ID = 'YL' THEN
        CASE Length (Call) OF
            4: ClubCall := True;
            5: ClubCall := Call[4]>='W';
            END;

    IF ID = 'YU' THEN
        CASE Length (Call) OF
            4: ClubCall := True;
            6: BEGIN
               IF (Copy (Call, 1, 3) = 'YU1') OR
                  (Copy (Call, 1, 3) = 'YU5') OR
                  (Copy (Call, 1, 3) = 'YU6') OR
                  (Copy (Call, 1, 3) = 'YU7') OR
                  (Copy (Call, 1, 3) = 'YU8') THEN
                      ClubCall := Call [4] <= 'W';
                END;
            END;

    IF ID = 'ZP' THEN
        ClubCall := (Length (Call) = 5) AND (Copy (Call, 4, 2) = 'AA');

    IF (ID = 'Z2') OR (ID = '3A') THEN
        ClubCall := Length (Call) = 6;

    IF (ID = '4X') THEN
        IF Length (Call) = 5 THEN
            ClubCall := (Call[4]='E') OR (Call[4]='H') OR
                        (Copy (Call, 1, 4) = '4Z4S') OR
                        (Copy (Call, 1, 4) = '4Z4Y');


    IF ID = '9A' THEN
        ClubCall := Length (Call) = 4;
    END;


FUNCTION GetNumber (Call: CallString): CHAR;

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



PROCEDURE LocateCall (Call: CallString;
                      VAR QTH:QTHRecord;
                      UseStandardCallFormat: BOOLEAN);

{ This is the procedure that gets everything done  }

VAR TempString: Str80;

    BEGIN
    IF Call = LastLocateCall THEN
        BEGIN
        QTH := LastLocateQTH;
        Exit;
        END;

    LastLocateCall := Call;

    QTH.StandardCall := StandardCallFormat (Call, True);

    IF StringHas (QTH.StandardCall, '/') THEN
        BEGIN
        TempString := PrecedingString (StandardCallFormat (Call, True), '/');

        {KK1L: 6.68 Added AM check to allow /AM as aeronautical mobile rather than Spain}
        IF (Copy (TempString, 1, 2) = 'MM') OR (Copy (TempString, 1, 2) = 'AM') THEN
            BEGIN
            Call := PostcedingString (QTH.StandardCall, '/');

            QTH.Prefix    := GetPrefix (Call);

{            QTH.Country   := CountryTable.GetCountry   (Call, UseStandardCallFormat);
             QTH.CountryID := CountryTable.GetCountryID (QTH.Country);}

            QTH.Country   := -1;
            QTH.CountryID := '';
            QTH.Continent := CountryTable.GetContinent (Call);
            QTH.Zone      := CountryTable.GetZone      (Call);
            QTH.CountryID := CountryTable.GetCountryID (QTH.Country);
            LastLocateQTH := QTH;
            Exit;
            END;

        IF NOT StringHasNumber (TempString) THEN
            TempString := TempString + '0';

        TempString := GetPrefix (TempString);
        QTH.Prefix := TempString;
        END
    ELSE
        QTH.Prefix := GetPrefix (QTH.StandardCall);

    QTH.Country   := CountryTable.GetCountry   (Call, UseStandardCallFormat);
    QTH.Continent := CountryTable.GetContinent (Call);
    QTH.Zone      := CountryTable.GetZone      (Call);
    QTH.CountryID := CountryTable.GetCountryID (QTH.Country);
    LastLocateQTH := QTH;
    END;



FUNCTION GetContinentName (Cont: ContinentType): Str20;

    BEGIN
    CASE Cont OF
        NorthAmerica: GetContinentName := 'NA';
        SouthAmerica: GetContinentName := 'SA';
        Europe:       GetContinentName := 'EU';
        Africa:       GetContinentName := 'AF';
        Asia:         GetContinentName := 'AS';
        Oceania:      GetContinentName := 'OC';
        ELSE          GetContinentName := '';
        END;

    END;



    BEGIN
    LastLocateCall := '';
    END.

