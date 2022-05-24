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

UNIT PostCab;
{$mode objfpc}
{$O+}

INTERFACE

Uses trCrt,
     Dos,
     Tree,
     SlowTree,
     Country9,
     LogDupe,
     ZoneCont,
     LogName,
     LogGrid,
     LogWind,
     PostSubs,
     PostMult,
     PostRpt;   { Contains ContestType definition }

PROCEDURE CreateCabrilloFile;

IMPLEMENTATION
uses keycode,sysutils;

CONST

    CabrilloVersion = '3.0';

    CabrilloNameFileName = 'CABNAME.DAT';

    NumberCategoryAssistedTypes    =  2;
    NumberCategoryBandTypes        = 13;
    NumberCategoryModeTypes        =  4;
    NumberCategoryOperatorTypes    =  3;
    NumberCategoryPowerTypes       =  3;
    NumberCategoryStationTypes     =  7;
    NumberCategoryTimeTypes        =  4;
    NumberCategoryTransmitterTypes =  5;
    NumberCategoryOverlayTypes     =  6;

    CategoryAssistedType :
        ARRAY [0..NumberCategoryAssistedTypes - 1] OF STRING [15] = (
            'ASSISTED',
            'NON-ASSISTED');

    CategoryBandType:
        ARRAY [0..NumberCategoryBandTypes - 1] OF STRING [4] = (
            'ALL',
            '160M',
            '80M',
            '40M',
            '20M',
            '15M',
            '10M',
            '6M',
            '2M',
            '222',
            '432',
            '903',
            '1.2G');

    CategoryModeType:
        ARRAY [0..NumberCategoryModeTypes - 1] OF STRING [5] = (
            'SSB',
            'CW',
            'RTTY',
            'MIXED');

    CategoryOperatorType:
        ARRAY [0..NumberCategoryOperatorTypes - 1] OF STRING [9] = (
            'SINGLE-OP',
            'MULTI-OP',
            'CHECKLOG');

    CategoryPowerType:
        ARRAY [0..NumberCategoryPowerTypes - 1] OF STRING [4] = (
            'HIGH',
            'LOW',
            'QRP');

    CategoryStationType:
        ARRAY [0..NumberCategoryStationTypes - 1] OF STRING [10] = (
            'FIXED',
            'MOBILE',
            'PORTABLE',
            'ROVER',
            'EXPEDITION',
            'HQ',
            'SCHOOL');

    CategoryTimeType:
        ARRAY [0..NumberCategoryTimeTypes - 1] OF STRING [8] = (
            'NONE',
            '6-HOURS',
            '12-HOURS',
            '24-HOURS');


    CategoryTransmitterType:
        ARRAY [0..NumberCategoryTransmitterTypes - 1] OF STRING [9] = (
            'ONE',
            'TWO',
            'LIMITED',
            'UNLIMITED',
            'SWL');

    CategoryOverlayType:
        ARRAY [0..NumberCategoryOverlayTypes - 1] OF STRING [11] = (
            'NONE',
            'ROOKIE',
            'CLASSIC',
            'TB-WIRES',
            'NOVICE-TECH',
            'OVER-50');


TYPE QTCEntryRecord = RECORD
         Time: CallString;
         Call: CallString;
         Number: CallString;
         END;

    QTCEntryArrayType = ARRAY [0..9] OF QTCEntryRecord;
    QTCEntryArrayPtr = ^QTCEntryArrayType;


VAR CallSignLength:  INTEGER;
    Contest:         ContestType;
    ContestName:     Str80;
    CountryID:       CallString;
    MyQTH:           QTHRecord;

    NumberExchangeElementsToRecord: INTEGER;

    PreviousName: Str20;  { Used for Interent Sprint }

    QTCEnable: BOOLEAN;
    QTCList: QTCEntryArrayPtr;

    ReceivedCallCursorPosition: INTEGER;
    ReceivedQSONumberLength:    INTEGER;
    RSTIsPartOfTheExchange: BOOLEAN;

    SentDataLength:      INTEGER;
    SniffForTransmitter: BOOLEAN;
    SentInformation:     Str40;
    SentQSONumberLength: INTEGER;

    SuppressRST: BOOLEAN;

    TempString:      Str40;


FUNCTION GetFrequencyStringFromBand (Band: BandType; PossibleFreqString: Str20): Str20;

VAR TempString: Str20;

    BEGIN
    TempString := '';

    CASE Band OF
        Band160: TempString := '1800';
        Band80:  TempString := '3500';
        Band40:  TempString := '7000';
        Band30:  TempString := '10100';
        Band20:  TempString := '14000';
        Band17:  TempString := '18100';
        Band15:  TempString := '21000';
        Band12:  TempString := '24900';
        Band10:  TempString := '28000';
        Band6:   TempString := '50';    {KK1L: 6.71 Changes from Trey from old lettering style}
        Band2:   TempString := '144';   {KK1L: 6.71 Changes from Trey from old lettering style}
        Band222: TempString := '222';   {KK1L: 6.71 Changes from Trey from old lettering style}
        Band432: TempString := '432';   {KK1L: 6.71 Changes from Trey from old lettering style}
        Band902: TempString := '903';   {KK1L: 6.71 Changes from Trey from old lettering style}
        Band1296: TempString := '1.2';  {KK1L: 6.71 Changes from Trey from old lettering style}
        Band2304: TempString := '2.3';  {KK1L: 6.71 Changes from Trey from old lettering style}
        Band3456: TempString := '3.4';  {KK1L: 6.71 Changes from Trey from old lettering style}
        Band5760: TempString := '5.7';  {KK1L: 6.71 Changes from Trey from old lettering style}
        Band10G:  TempString := '10';   {KK1L: 6.71 Changes from Trey from old lettering style}
        Band24G:  TempString := '24';   {KK1L: 6.71 Changes from Trey from old lettering style}
        BandLight: TempString := 'LIGHT'; {KK1L: 6.71 Changes from Trey from old lettering style}
        END;

    IF StringHas (PossibleFreqString, '.') THEN
        BEGIN
        Delete (PossibleFreqString, 1, 1);
        Delete (TempString, Length (TempString) - 2, 3);
        TempString := TempString + PossibleFreqString;
        END;

    WHILE Length (TempString) < 5 DO
        TempString := ' ' + TempString;

    GetFrequencyStringFromBand := TempString;
    END;


FUNCTION GetModeStringFromMode (Mode: ModeType): Str20;

    BEGIN
    CASE Mode OF
        CW:       GetModeStringFromMode := 'CW';
        FM:       GetModeStringFromMode := 'FM';
        Digital:  GetModeStringFromMode := 'RY';
        Phone:    GetModeStringFromMode := 'PH';
        END;
    END;

FUNCTION GetCabrilloDateStringFromDateString (FileString: STRING): Str20;

 { Do the date up so it looks like 1997-11-03 }

VAR DateString, MonthString, YearString: Str20;

    BEGIN
    DateString := Copy (FileString, 1, 2);

    MonthString := UpperCase (Copy (FileString, 4, 3));

    IF MonthString = 'JAN' THEN MonthString := '01';
    IF MonthString = 'FEB' THEN MonthString := '02';
    IF MonthString = 'MAR' THEN MonthString := '03';
    IF MonthString = 'APR' THEN MonthString := '04';
    IF MonthString = 'MAY' THEN MonthString := '05';
    IF MonthString = 'JUN' THEN MonthString := '06';
    IF MonthString = 'JUL' THEN MonthString := '07';
    IF MonthString = 'AUG' THEN MonthString := '08';
    IF MonthString = 'SEP' THEN MonthString := '09';
    IF MonthString = 'OCT' THEN MonthString := '10';
    IF MonthString = 'NOV' THEN MonthString := '11';
    IF MonthString = 'DEC' THEN MonthString := '12';

    YearString := Copy (FileString, 8, 2);

    { This is not year 2090 compliant - but easy to fix }

    IF Copy (YearString, 1, 1) = '9' THEN
        YearString := '19' + YearString
    ELSE
        YearString := '20' + YearString;

    GetCabrilloDateStringFromDateString := YearString + '-' + MonthString + '-' + DateString;
    END;


FUNCTION GetCabrilloDateStringFromLogEntry (LogEntryString: STRING): Str20;

    BEGIN
    LogEntryString := Copy (LogEntryString, LogEntryDayAddress, 9);
    GetCabrilloDateStringFromLogEntry := GetCabrilloDateStringFromDateString (LogEntryString);
    END;


FUNCTION ClaimedScore: LONGINT;

VAR TotalMultipliers: LONGINT;
    TotalQSOPoints:   LONGINT;

    BEGIN
    TotalQSOPoints   := QSOPointTotals [All, Both];

    TotalMultipliers := DomesticTotals [All, Both] +
                        DXTotals       [All, Both] +
                        PrefixTotals   [All, Both] +
                        ZoneTotals     [All, Both];

    IF TotalMultipliers > 0 THEN
        ClaimedScore := TotalQSOPoints * TotalMultipliers
    ELSE
        ClaimedScore := TotalQSOPoints;
    END;


PROCEDURE ShowCategoryData (StartY, AssistedCursor, BandCursor, ModeCursor,
                            OperatorCursor, PowerCursor, StationCursor,
                            TimeCursor, TransmitterCursor, OverlayCursor: INTEGER);

{ Add 100 to the cursor who's column you want to be active }

    BEGIN
    GoToXY (1, StartY);

    TextColor (Cyan);
    TextBackground (Black);

    ClrEol;

    IF AssistedCursor >= 100 THEN
        BEGIN
        AssistedCursor := AssistedCursor - 100;
        TextBackground (Cyan);
        TextColor (Black);
        WriteLn ('CATEGORY-ASSISTED: ',    CategoryAssistedType [AssistedCursor],Chr(13));
        TextColor (Cyan);
        TextBackground (Black);
        END
    ELSE
        WriteLn ('CATEGORY-ASSISTED: ',    CategoryAssistedType [AssistedCursor],Chr(13));

    ClrEol;

    IF BandCursor >= 100 THEN
        BEGIN
        BandCursor := BandCursor - 100;
        TextBackground (Cyan);
        TextColor (Black);
        WriteLn ('CATEGORY-BAND: ',        CategoryBandType [BandCursor],Chr(13));
        TextColor (Cyan);
        TextBackground (Black);
        END
    ELSE
        WriteLn ('CATEGORY-BAND: ',        CategoryBandType [BandCursor],Chr(13));

    ClrEol;

    IF ModeCursor >= 100 THEN
        BEGIN
        ModeCursor := ModeCursor - 100;
        TextBackground (Cyan);
        TextColor (Black);
        WriteLn ('CATEGORY-MODE: ',        CategoryModeType [ModeCursor],Chr(13));
        TextColor (Cyan);
        TextBackground (Black);
        END
    ELSE
        WriteLn ('CATEGORY-MODE: ',        CategoryModeType [ModeCursor],Chr(13));

    ClrEol;

    IF OperatorCursor >= 100 THEN
        BEGIN
        OperatorCursor := OperatorCursor - 100;
        TextBackground (Cyan);
        TextColor (Black);
        WriteLn ('CATEGORY-OPERATOR: ',    CategoryOperatorType [OperatorCursor],Chr(13));
        TextColor (Cyan);
        TextBackground (Black);
        END
    ELSE
        WriteLn ('CATEGORY-OPERATOR: ',    CategoryOperatorType [OperatorCursor],Chr(13));

    ClrEol;

    IF PowerCursor >= 100 THEN
        BEGIN
        PowerCursor := PowerCursor - 100;
        TextBackground (Cyan);
        TextColor (Black);
        WriteLn ('CATEGORY-POWER: ',       CategoryPowerType [PowerCursor],Chr(13));
        TextColor (Cyan);
        TextBackground (Black);
        END
    ELSE
        WriteLn ('CATEGORY-POWER: ',       CategoryPowerType [PowerCursor],Chr(13));

    ClrEol;

    IF StationCursor >= 100 THEN
        BEGIN
        StationCursor := StationCursor - 100;
        TextBackground (Cyan);
        TextColor (Black);
        WriteLn ('CATEGORY-STATION: ',     CategoryStationType [StationCursor],Chr(13));
        TextColor (Cyan);
        TextBackground (Black);
        END
    ELSE
        WriteLn ('CATEGORY-STATION: ',     CategoryStationType [StationCursor],Chr(13));

    ClrEol;

    IF TimeCursor >= 100 THEN
        BEGIN
        TimeCursor := TimeCursor - 100;
        TextBackground (Cyan);
        TextColor (Black);
        WriteLn ('CATEGORY-TIME: ',        CategoryTimeType [TimeCursor],Chr(13));
        TextColor (Cyan);
        TextBackground (Black);
        END
    ELSE
        WriteLn ('CATEGORY-TIME: ',        CategoryTimeType [TimeCursor],Chr(13));

    ClrEol;

    IF TransmitterCursor >= 100 THEN
        BEGIN
        TransmitterCursor := TransmitterCursor - 100;
        TextBackground (Cyan);
        TextColor (Black);
        WriteLn ('CATEGORY-TRANSMITTER: ', CategoryTransmitterType [TransmitterCursor],Chr(13));
        TextColor (Cyan);
        TextBackground (Black);
        END
    ELSE
        WriteLn ('CATEGORY-TRANSMITTER: ', CategoryTransmitterType [TransmitterCursor],Chr(13));

    ClrEol;

    IF OverlayCursor >= 100 THEN
        BEGIN
        OverLayCursor := OverLayCursor - 100;
        TextBackground (Cyan);
        TextColor (Black);
        WriteLn ('CATEGORY-OVERLAY: ',     CategoryOverlayType [OverlayCursor],Chr(13));
        TextColor (Cyan);
        TextBackground (Black);
        END
    ELSE
        WriteLn ('CATEGORY-OVERLAY: ',     CategoryOverlayType [OverlayCursor],Chr(13));

    END;


PROCEDURE ActivateNextEntry (VAR AssistedCursor,
                                 BandCursor,
                                 ModeCursor,
                                 OperatorCursor,
                                 PowerCursor,
                                 StationCursor,
                                 TimeCursor,
                                 TransmitterCursor,
                                 OverlayCursor: INTEGER);

    BEGIN
    IF AssistedCursor >= 100 THEN
        BEGIN
        AssistedCursor := AssistedCursor - 100;
        BandCursor     := BandCursor + 100;
        Exit;
        END;

    IF BandCursor >= 100 THEN
        BEGIN
        BandCursor := BandCursor - 100;
        ModeCursor := ModeCursor + 100;
        Exit;
        END;

    IF ModeCursor >= 100 THEN
        BEGIN
        ModeCursor := ModeCursor - 100;
        OperatorCursor := OperatorCursor + 100;
        Exit;
        END;

    IF OperatorCursor >= 100 THEN
        BEGIN
        OperatorCursor := OperatorCursor - 100;
        PowerCursor := PowerCursor + 100;
        Exit;
        END;

    IF PowerCursor >= 100 THEN
        BEGIN
        PowerCursor := PowerCursor - 100;
        StationCursor := StationCursor + 100;
        Exit;
        END;

    IF StationCursor >= 100 THEN
        BEGIN
        StationCursor := StationCursor - 100;
        TimeCursor := TimeCursor + 100;
        Exit;
        END;

    IF TimeCursor >= 100 THEN
        BEGIN
        TimeCursor := TimeCursor - 100;
        TransmitterCursor := TransmitterCursor + 100;
        Exit;
        END;

    IF TransmitterCursor >= 100 THEN
        BEGIN
        TransmitterCursor := TransmitterCursor - 100;
        OverLayCursor := OverlayCursor + 100;
        Exit;
        END;

    IF OverlayCursor >= 100 THEN
        BEGIN
        OverLayCursor := OverLayCursor - 100;
        AssistedCursor := AssistedCursor + 100;
        Exit;
        END;

    ReportError ('ERROR!! - Unexpected Cursor Value in ActivateNextEntry');
    Halt;
    END;


PROCEDURE ActivatePreviousEntry (VAR AssistedCursor,
                                     BandCursor,
                                     ModeCursor,
                                     OperatorCursor,
                                     PowerCursor,
                                     StationCursor,
                                     TimeCursor,
                                     TransmitterCursor,
                                     OverlayCursor: INTEGER);

    BEGIN
    IF AssistedCursor >= 100 THEN
        BEGIN
        AssistedCursor := AssistedCursor - 100;
        OverlayCursor := OverlayCursor + 100;
        Exit;
        END;

    IF BandCursor >= 100 THEN
        BEGIN
        BandCursor := BandCursor - 100;
        AssistedCursor := AssistedCursor + 100;
        Exit;
        END;

    IF ModeCursor >= 100 THEN
        BEGIN
        ModeCursor := ModeCursor - 100;
        BandCursor := BandCursor + 100;
        Exit;
        END;

    IF OperatorCursor >= 100 THEN
        BEGIN
        OperatorCursor := OperatorCursor - 100;
        ModeCursor := ModeCursor + 100;
        Exit;
        END;

    IF PowerCursor >= 100 THEN
        BEGIN
        PowerCursor := PowerCursor - 100;
        OperatorCursor := OperatorCursor + 100;
        Exit;
        END;

    IF StationCursor >= 100 THEN
        BEGIN
        StationCursor := StationCursor - 100;
        PowerCursor := PowerCursor + 100;
        Exit;
        END;

    IF TimeCursor >= 100 THEN
        BEGIN
        TimeCursor := TimeCursor - 100;
        StationCursor := StationCursor + 100;
        Exit;
        END;

    IF TransmitterCursor >= 100 THEN
        BEGIN
        TransmitterCursor := TransmitterCursor - 100;
        TimeCursor := TimeCursor + 100;
        Exit;
        END;

    IF OverlayCursor >= 100 THEN
        BEGIN
        OverLayCursor := OverLayCursor - 100;
        TransmitterCursor := TransmitterCursor + 100;
        Exit;
        END;

    ReportError ('ERROR!! - Unexpected Cursor Value in ActivatePreviousEntry');
    Halt;
    END;


FUNCTION GetCategory (VAR AssistedCursor, BandCursor, ModeCursor,
                          OperatorCursor, PowerCursor, StationCursor,
                          TimeCursor, TransmitterCursor, OverlayCursor: INTEGER): BOOLEAN;

VAR StartY: INTEGER;

    BEGIN
    ClearScreenAndTitle ('CHOOSE CATEGORY');

    WriteLn ('Move the cursor to the various fields show and use SPACE BAR to');
    WriteLn ('select the correct values for your Cabrillo entry.  When you');
    WriteLn ('have the correct values - press the RETURN key to continue.');
    WriteLn;
    WriteLn ('Use the ESCAPE key to abort.');
    WriteLn;

    TextColor (Cyan);

    NoCursor;

    StartY := WhereY;

    AssistedCursor    := 100;
    BandCursor        := 0;
    ModeCursor        := 0;
    OperatorCursor    := 0;
    PowerCursor       := 0;
    StationCursor     := 0;
    TimeCursor        := 0;
    TransmitterCursor := 0;
    OverlayCursor     := 0;

    REPEAT
        ShowCategoryData (StartY, AssistedCursor,
                                  BandCursor,
                                  ModeCursor,
                                  OperatorCursor,
                                  PowerCursor,
                                  StationCursor,
                                  TimeCursor,
                                  TransmitterCursor,
                                  OverlayCursor);

        CASE ReadKey OF
            EscapeKey:
                BEGIN
                GetCategory := False;
                SmallCursor;
                TextColor (Cyan);
                TextBackground (Black);
                ClrScr;
                Exit;
                END;

            SpaceBar:
                BEGIN
                IF AssistedCursor >= 100 THEN
                    BEGIN
                    Inc (AssistedCursor);
                    IF AssistedCursor >= NumberCategoryAssistedTypes + 100 THEN
                        AssistedCursor := 100;
                    END;

                IF BandCursor >= 100 THEN
                    BEGIN
                    Inc (BandCursor);
                    IF BandCursor >= NumberCategoryBandTypes + 100 THEN
                        BandCursor := 100;
                    END;

                IF ModeCursor >= 100 THEN
                    BEGIN
                    Inc (ModeCursor);
                    IF ModeCursor >= NumberCategoryModeTypes + 100 THEN
                        ModeCursor := 100;
                    END;

                IF OperatorCursor >= 100 THEN
                    BEGIN
                    Inc (OperatorCursor);
                    IF OperatorCursor >= NumberCategoryOperatorTypes + 100 THEN
                        OperatorCursor := 100;
                    END;

                IF PowerCursor >= 100 THEN
                    BEGIN
                    Inc (PowerCursor);
                    IF PowerCursor >= NumberCategoryPowerTypes + 100 THEN
                        PowerCursor := 100;
                    END;

                IF StationCursor >= 100 THEN
                    BEGIN
                    Inc (StationCursor);
                    IF StationCursor >= NumberCategoryStationTypes + 100 THEN
                        StationCursor := 100;
                    END;

                IF TimeCursor >= 100 THEN
                    BEGIN
                    Inc (TimeCursor);
                    IF TimeCursor >= NumberCategoryTimeTypes + 100 THEN
                        TimeCursor := 100;
                    END;

                IF TransmitterCursor >= 100 THEN
                    BEGIN
                    Inc (TransmitterCursor);
                    IF TransmitterCursor >= NumberCategoryTransmitterTypes + 100 THEN
                        TransmitterCursor := 100;
                    END;

                IF OverlayCursor >= 100 THEN
                    BEGIN
                    Inc (OverlayCursor);
                    IF OverlayCursor >= NumberCategoryOverlayTypes + 100 THEN
                        OverlayCursor := 100;
                    END;

                END;

            TabKey:  ActivateNextEntry (AssistedCursor,
                                        BandCursor,
                                        ModeCursor,
                                        OperatorCursor,
                                        PowerCursor,
                                        StationCursor,
                                        TimeCursor,
                                        TransmitterCursor,
                                        OverlayCursor);

            CarriageReturn:
                BEGIN
                IF AssistedCursor    >= 100 THEN AssistedCursor := AssistedCursor - 100;
                IF BandCursor        >= 100 THEN BandCursor := BandCursor - 100;
                IF ModeCursor        >= 100 THEN ModeCursor := ModeCursor - 100;
                IF OperatorCursor    >= 100 THEN OperatorCursor := OperatorCursor - 100;
                IF PowerCursor       >= 100 THEN PowerCursor := PowerCursor - 100;
                IF StationCursor     >= 100 THEN StationCursor := StationCursor - 100;
                IF TimeCursor        >= 100 THEN TimeCursor := TimeCursor - 100;
                IF TransmitterCursor >= 100 THEN TransmitterCursor := TransmitterCursor - 100;
                IF OverlayCursor     >= 100 THEN OverlayCursor := OverlayCursor - 100;

                GetCategory := True;

                TextColor (Cyan);
                TextBackground (Black);
                ClrScr;
                SmallCursor;
                Exit;
                END;

            NullKey:
                CASE ReadKey OF
                    DownArrow:
                        ActivateNextEntry (AssistedCursor,
                                           BandCursor,
                                           ModeCursor,
                                           OperatorCursor,
                                           PowerCursor,
                                           StationCursor,
                                           TimeCursor,
                                           TransmitterCursor,
                                           OverlayCursor);


                    ShiftTab, UpArrow:
                        ActivatePreviousEntry (AssistedCursor,
                                               BandCursor,
                                               ModeCursor,
                                               OperatorCursor,
                                               PowerCursor,
                                               StationCursor,
                                               TimeCursor,
                                               TransmitterCursor,
                                               OverlayCursor);

                    END; { of null key case }

            END; { of case}

    UNTIL False;
    END;



FUNCTION GenerateSummaryPortionOfCabrilloFile (CabrilloFileName: Str80;
                                               VAR FileWrite: TEXT): BOOLEAN;

VAR Key:             CHAR;
    Address1, Address2, Address3, Address4, TempString:      Str80;
    Operators:       STRING;
    Precedence, Check, Section, Power, Name, QTH: Str40;
    NameFileWrite, NameFileRead: TEXT;
    ModeString : Str40;
    NameFileDirectory: Str80;
    Year, Month, Day, DayOfWeek: WORD;

    AssistedCursor, BandCursor, ModeCursor, OperatorCursor: INTEGER;
    PowerCursor, StationCursor, TimeCursor, TransmitterCursor: INTEGER;
    OverlayCursor: INTEGER;

    BEGIN
    GenerateSummaryPortionOfCabrilloFile := False;

    WriteLn ('The first step in generating the Cabrillo file is to put the necessary ');
    WriteLn ('summary information at the front of the file.');

    IF NOT GetCallAndContestFromLOGCFGFile (MyCall, ContestName) THEN Exit;

    GetRidOfPostcedingSpaces (ContestName);

    Contest := DetermineContest (ContestName);

    IF Contest = UnknownContest THEN
        BEGIN
        ReportError (ContestName + ' is not recognized.');
        WaitForKeyPressed;
        Exit;
        END;

    IF NOT CheckForTempFile THEN Exit;

    CountryTable.CountryMode := ARRLCountryMode;
    CountryTable.ZoneMode    := CQZoneMode;

    { Make sure we are using the right zones and countries }

    CASE Contest OF
        CQ160:     CountryTable.CountryMode := CQCountryMode;
        CQVHF:     CountryTable.CountryMode := CQCountryMode;
        CQWPX:     CountryTable.CountryMode := CQCountryMode;
        CQWPXRTTY: CountryTable.CountryMode := CQCountryMode;
        Region1FD: CountryTable.CountryMode := CQCountryMode;
        CQWW:      CountryTable.CountryMode := CQCountryMode;
        CQWWRTTY:  CountryTable.CountryMode := CQCountryMode;
        IARU:      CountryTable.ZoneMode    := ITUZoneMode;
        WRTC2018:  CountryTable.ZoneMode    := ITUZoneMode;
        Russian:   CountryTable.CountryMode := CQCountryMode;
        WAE:       CountryTable.CountryMode := CQCountryMode;
        END;

    LocateCall (MyCall, MyQTH, True);

    CountryID := CountryTable.GetCountryID (MyQTH.Country);

    TextColor (Cyan);

    CalculateAppropriateTotalsForThisContest (Contest, MyQTH);

    IF RawQSOTotals [All, Both] = 0 THEN
        BEGIN
        GoToXY (1, WhereY);
        ClrEol;
        WriteLn ('There were no contacts found in the log file.');
        WaitForKeyPressed;
        Exit;
        END;

    Write (' QSOs were found in your log.');
    WriteLn;

    IF NOT GetCategory (AssistedCursor,BandCursor, ModeCursor,
                        OperatorCursor, PowerCursor, StationCursor,
                        TimeCursor, TransmitterCursor, OverlayCursor) THEN
                            BEGIN
                            GenerateSummaryPortionOfCabrilloFile := False;
                            Exit;
                            END;


    Section := '';
    Power   := '';
    PreviousName := '';

    SniffForTransmitter := False;

    CallSignLength := 13;              { Default maximum callsign length }
    ReceivedCallCursorPosition := 56;  { Default location of received call }

    RSTIsPartOfTheExchange := False;
    NumberExchangeElementsToRecord := 0;  { 0 is disable counting }

    ReceivedQSONumberLength := 0;
    SentQSONumberLength     := 0;

    SuppressRST := False;

    { Get the sent information string for the specific contest we are doing }

    { Currently not doing the AP-SPRINT, ARRL-UHF-AUG, SAC, TARA-RTTY }

    CASE Contest OF
        ARRL10:      { Okay for domestic and DX }
            BEGIN
            RSTIsPartOfTheExchange := True;

            IF (MyQTH.CountryId = 'K')   OR (MyQTH.CountryID = 'VE') OR
               (MyQTH.CountryId = 'KH6') OR (MyQTH.CountryID = 'KL') THEN
                BEGIN
                SentInformation := GetResponse ('Enter the state/province you sent durin the contest : ');
                SentInformation := '$ ' + SentInformation;

                Section := GetResponse ('Enter your ARRL Section abbreviation : ');
                IF Section = '' THEN Exit;
                END
            ELSE
                BEGIN
                SentInformation := '$ #';
                SentQSONumberLength := 6;
                Section := 'DX'; {KK1L: 6.73 2.2}
                END;

            NumberExchangeElementsToRecord := 1;
            END;

        ARRL160:     { Okay for domestic and DX }
            BEGIN
            RSTIsPartOfTheExchange := True;

            IF ARRLSectionCountry (MYQTH.CountryID) THEN
                BEGIN
                Section := GetResponse ('Enter your ARRL Section abbreviation : ');
                IF Section = '' THEN Exit;
                SentInformation := '$ ' + Section;
                END
            ELSE
                BEGIN
                SentInformation := '$';
                Section := 'DX'; {KK1L: 6.73 2.2}
                END;
            END;

        ARRLDX:      { Okay for domestic and DX }
            BEGIN
            RSTIsPartOfTheExchange := True;
            SniffForTransmitter := True;

            IF (MyQTH.CountryId = 'K') OR (MyQTH.CountryID = 'VE') THEN
                BEGIN
                SentInformation := GetResponse ('Enter the QTH you sent during the contest : ');
                IF SentInformation = '' THEN Exit;
                SentInformation := '$ ' + SentInformation;
                Section := GetResponse ('Enter your ARRL Section abbreviation : ');
                IF Section = '' THEN Exit;
                END
            ELSE
                BEGIN
                Power := GetResponse ('Enter power sent during contest : ');
                SentInformation := '$ ' + Power;
                Section := 'DX'; {KK1L: 6.73 2.2}
                END;
            END;


        ARRLVHFQSO, ARRLVHFSS, CQVHF:
            BEGIN
            SentInformation := GetResponse ('Enter the grid you sent : ');
            IF SentInformation = '' THEN Exit;
            Section := GetResponse ('Enter your ARRL Section : ');
            IF Section = '' THEN Exit;
            RSTIsPartOfTheExchange := True;
            SuppressRST := True;
            END;

        ARRLRTTY:
            BEGIN
            Section := UpperCase (GetResponse ('Enter your ARRL Section Abbreviation (or DX) : '));

            IF Section <> 'DX' THEN
                BEGIN
                SentInformation := GetResponse ('Enter the state or province you sent during the contest : ');
                SentInformation := '$ ' + SentInformation;
                END
            ELSE
                SentInformation := '$ #';

            RSTIsPartOfTheExchange := True;
            NumberExchangeElementsToRecord := 1;
            END;

        CalQSO, FQP, MIQP, MQP, OHIOQP, SALMONRUN, WQP:  {KK1L: 6.71 added other state QPs}
            BEGIN
            Section := GetResponse ('Enter your state or county sent : '); {KK1L: 6.71 Removed 'Cal' before 'county'}
            IF Section = '' THEN Exit;
            SentInformation := '# ' + Section;
            END;

        CQ160:       { Okay }
            BEGIN
            RSTIsPartOfTheExchange := True;

            Section := GetResponse ('Enter the QTH you sent : ');
            IF Section = '' THEN Exit;
            SentInformation := '$ ' + Section;

            NumberExchangeElementsToRecord := 1;
            END;


        CQWPX, CQWPXRTTY, OceaniaVKZL, SAC, APSprint, Region1FD, WAE, MMC:
            BEGIN
            CallsignLength := 13;
            RSTIsPartOfTheExchange := True;
            SentDataLength := 10;
            SentInformation := '$ #';
            SentQSONumberLength := 6;
            END;


        CQWW:        { Okay }
            BEGIN
            CallsignLength := 13;
            RSTIsPartOfTheExchange := True;
            SentDataLength := 10;
            SniffForTransmitter := True; {KK1L: 6.71 Add Multi-Two check for CQWW}

            QTH := GetResponse ('Enter the zone you were sending : ');
            IF QTH = '' THEN Exit;

            SentInformation := '$ ' + QTH;

            IF ARRLSectionCountry (MYQTH.CountryID) THEN
                Section := GetResponse ('Enter your ARRL Section abbreviation : ');
            END;

        CQWWRTTY:
            BEGIN
            CallsignLength := 13;
            RSTIsPartOfTheExchange := True;
            SentDataLength := 10;
            SniffForTransmitter := True; {KK1L: 6.71 Add Multi-Two check for CQWW}

            QTH := GetResponse ('Enter the zone you were sending : ');
            IF QTH = '' THEN Exit;
            IF (MyQTH.CountryId = 'K')   OR (MyQTH.CountryID = 'VE') then
               Section := GetResponse ('Enter your state: ')
            else
               Section := 'DX';

            SentInformation := '$ ' + QTH + ' ' + Section;
            end;

        IARU:        { Okay }
            BEGIN
            RSTIsPartOfTheExchange := True;

            QTH := GetResponse ('Enter the zone you were sending : ');
            IF QTH = '' THEN Exit;

            SentInformation := '$ ' + QTH;

            IF ARRLSectionCountry (MYQTH.CountryID) THEN
                Section := GetResponse ('Enter your ARRL Section abbreviation : ');

            END;


        IntSprint:      { Okay - somewhat modified format - name length }
            BEGIN
            CallsignLength := 10;
            SentQSONumberLength := 4;
            ReceivedCallCursorPosition := 62;

            PreviousName := GetResponse ('Enter the name you sent for the first QSO : ');
            IF PreviousName = '' THEN Exit;

            QTH := GetResponse ('Enter the QTH you sent (DX if outside W/VE) : ');
            IF QTH = '' THEN Exit;

            SentInformation := '# ' + ControlN + ' ' + QTH;
            ReceivedQSONumberLength := 4;
            END;

        NAQSO:       { Doesn't comply with field length for name }
            BEGIN
            CallsignLength := 10;
            ReceivedCallCursorPosition := 62;
            SniffForTransmitter := True; {KK1L: 6.71 Add Multi-Two check for NAQP}

            Name := GetResponse ('Enter the name you sent : ');
            IF Name = '' THEN Exit;

            QTH := GetResponse ('Enter the QTH you sent (DX if outside W/VE) : ');
            IF QTH = '' THEN Exit;

            SentInformation := Name + ' ' + QTH;
            END;

        NEQSO:
            BEGIN
            REPEAT
                Key := UpCase (GetKey ('Did you operate from New England? (Y/N or ESCAPE to abort) : '));
                IF Key = EscapeKey THEN Exit;
            UNTIL (Key = 'Y') OR (Key = 'N');
            WriteLn;

            Section := GetResponse ('Enter your state: ');

            IF Section = '' THEN Exit;

            IF Key = 'Y' THEN
                BEGIN
                QTH := GetResponse ('Enter your THREE leter county abreviation: ');
                SentInformation := '$ ' + QTH + Section;
                END
            ELSE
                SentInformation := '$ ' + Section;

            RSTIsPartOfTheExchange := True;
            END;


        Russian:
            BEGIN
            NumberExchangeElementsToRecord := 1;

            SentQSONumberLength := 6;
            ReceivedCallCursorPosition := 56;
            RSTIsPartOfTheExchange := True;

            REPEAT
                Key := UpCase (GetKey ('Were you operating from Russia? (Y/N) : '));
                IF Key = EscapeKey THEN Exit;
            UNTIL (Key = 'Y') OR (Key = 'N');
            WriteLn;

            IF Key = 'Y' THEN
                BEGIN
                QTH := GetResponse ('Enter your two letter region : ');
                SentInformation := '$ ' + QTH;
                END
            ELSE
                SentInformation := '$ #';

            END;

        Sprint:      { Okay - somewhat modified format - name length }
            BEGIN
            CallsignLength := 10;
            SentQSONumberLength := 4;
            ReceivedCallCursorPosition := 62;

            Name := GetResponse ('Enter the name you sent : ');
            IF Name = '' THEN Exit;

            QTH := GetResponse ('Enter the QTH you sent (DX if outside W/VE) : ');
            IF QTH = '' THEN Exit;

            SentInformation := '# ' + Name + ' ' + QTH;
            ReceivedQSONumberLength := 4;
            END;

        SevenQP:
            BEGIN
            RSTIsPartOfTheExchange := TRUE;
            Section := GetResponse ('Enter your state or county sent : '); {KK1L: 6.71 Removed 'Cal' before 'county'}
            IF Section = '' THEN Exit;
            SentInformation := '$ ' + Section;
            END;


        SS: BEGIN    { Okay }
            CallsignLength := 10;
            ReceivedQSONumberLength := 4;
            SentDataLength := 13;
            SentQSONumberLength := 4;

            Precedence := GetKeyResponse ('Enter your precedence sent (A, B, Q, M, S, U) : ');
            IF Precedence = EscapeKey THEN Exit;

            Check := GetResponse ('Enter your check (last two digits of year licensed : ');
            IF (Length (Check) <> 2) OR NOT StringIsAllNumbers (Check) THEN
                Exit;

            Section := GetResponse ('Enter your ARRL Section abbreviation : ');
            IF Section = '' THEN Exit;

            SentInformation := '# ' + Precedence + ' ' + Check + ' ' + Section;
            END;

        StewPerry:
            BEGIN
            SentInformation := GetResponse ('Enter the grid you sent : ');
            IF SentInformation = '' THEN Exit;

            SentInformation := '$ ' + SentInformation;

            RSTIsPartOfTheExchange := True;
            END;

        WRTC2018:
            BEGIN
            RSTIsPartOfTheExchange := True;
            SniffForTransmitter := True;
            QTH := GetResponse ('Enter the zone you were sending : ');
            IF QTH = '' THEN Exit;
            SentInformation := '$ ' + qth;
            numberexchangeelementstorecord := 1;
            END;
        END;

    IF NOT OpenFileForWrite (FileWrite, CabrilloFileName) THEN Exit;

    WriteLn (FileWrite, 'START-OF-LOG: ', CabrilloVersion,Chr(13));
    WriteLn (FileWrite, 'CREATED-BY: TR Log POST Version ', PostVersion,Chr(13));
    WriteLn (FileWrite, 'CALLSIGN: ', MyCall,Chr(13));

    CASE ModeCursor OF
        0: ModeString := 'SSB';
        1: ModeString := 'CW';
        2: ModeString := 'RTTY';
        END;

    QTCEnable := False;

    CASE Contest OF
            APSprint:    WriteLn (FileWrite, 'CONTEST: AP SPRINT',Chr(13));
            ARRL10:      WriteLn (FileWrite, 'CONTEST: ARRL-10',Chr(13));
            ARRL160:     WriteLn (FileWrite, 'CONTEST: ARRL-160',Chr(13));
            ARRLDX:      WriteLn (FileWrite, 'CONTEST: ARRL-DX-', ModeString,Chr(13));
            ARRLRTTY:    WriteLn (FileWrite, 'CONTEST: ARRL-RTTY',Chr(13));

            ARRLVHFQSO:
                BEGIN
                GetDate (Year, Month, Day, DayOfWeek);

                IF (Month >= 6) AND (Month < 9) THEN
                    WriteLn (FileWrite, 'CONTEST: ARRL-VHF-JUN',Chr(13))
                ELSE
                    IF (Month >= 9) OR (Month < 6) THEN
                        WriteLn (FileWrite, 'CONTEST: ARRL-VHF-SEP',Chr(13));
                END;

            ARRLVHFSS:   WriteLn (FileWrite, 'CONTEST: ARRL-VHF-JAN',Chr(13));
            CalQSO:      WriteLn (FileWrite, 'CONTEST: CA-QSO-PARTY',Chr(13));
            CQ160:       WriteLn (FileWrite, 'CONTEST: CQ-160-', ModeString,Chr(13));
            CQVHF:       WriteLn (FileWrite, 'CONTEST: CQ-VHF',Chr(13));
            CQWPX:       WriteLn (FileWrite, 'CONTEST: CQ-WPX-', ModeString,Chr(13));
            CQWPXRTTY:       WriteLn (FileWrite, 'CONTEST: CQ-WPX-RTTY',Chr(13));
            CQWW:        WriteLn (FileWrite, 'CONTEST: CQ-WW-', ModeString,Chr(13));
            CQWWRTTY:        WriteLn (FileWrite, 'CONTEST: CQ-WW-', ModeString,Chr(13));
            IARU:        WriteLn (FileWrite, 'CONTEST: IARU-HF',Chr(13));
            IntSprint:   WriteLn (FileWrite, 'CONTEST: INTERNET-SPRINT',Chr(13));
            NAQSO:       WriteLn (FileWrite, 'CONTEST: NAQP-', ModeString,Chr(13));
            NEQSO:       WriteLn (FileWrite, 'CONTEST: New England QSO Party',Chr(13));
            OceaniaVKZL: WriteLn (FileWrite, 'CONTEST: OCEANIA',Chr(13));
            Region1FD:   WriteLn (FileWrite, 'CONTEST: REGION ONE FIELD DAY',Chr(13));
            Russian:     WriteLn (FileWrite, 'CONTEST: RDXC',Chr(13));
            SAC:         WriteLn (FileWrite, 'CONTEST: SAC',Chr(13));
            SevenQP:     WriteLn (FileWrite, 'CONTEST: 7QP', Chr (13));
            Sprint:      WriteLn (FileWrite, 'CONTEST: NA-SPRINT-', ModeString,Chr(13));
            SS:          WriteLn (FileWrite, 'CONTEST: ARRL-SS-', ModeString,Chr(13));
            StewPerry:   WriteLn (FileWrite, 'CONTEST: STEW-PERRY',Chr(13));

            WAE:         BEGIN
                         WriteLn (FileWrite, 'CONTEST: DARC-WAEDC-', ModeString,Chr(13));
                         QTCEnable := True;
                         END;
            WRTC2018:
                         WriteLn (FileWrite, 'CONTEST: WRTC 2018',Chr(13));
            END;

    IF Section <> '' THEN
        WriteLn (FileWrite, 'ARRL-SECTION: ', Section,Chr(13));


    WriteLn (FileWrite, 'CATEGORY-ASSISTED: ',    CategoryAssistedType [AssistedCursor],Chr(13));
    WriteLn (FileWrite, 'CATEGORY-BAND: ',        CategoryBandType [BandCursor],Chr(13));
    WriteLn (FileWrite, 'CATEGORY-MODE: ',        CategoryModeType [ModeCursor],Chr(13));
    WriteLn (FileWrite, 'CATEGORY-OPERATOR: ',    CategoryOperatorType [OperatorCursor],Chr(13));
    WriteLn (FileWrite, 'CATEGORY-POWER: ',       CategoryPowerType [PowerCursor],Chr(13));
    WriteLn (FileWrite, 'CATEGORY-STATION: ',     CategoryStationType [StationCursor],Chr(13));

    IF TimeCursor > 0 THEN
        WriteLn (FileWrite, 'CATEGORY-TIME: ',        CategoryTimeType [TimeCursor],Chr(13));

    WriteLn (FileWrite, 'CATEGORY-TRANSMITTER: ', CategoryTransmitterType [TransmitterCursor],Chr(13));

    IF OverLayCursor > 0 THEN
        WriteLn (FileWrite, 'CATEGORY-OVERLAY: ',     CategoryOverlayType [OverlayCursor],Chr(13));

    WriteLn (FileWrite, 'CLAIMED-SCORE: ', ClaimedScore,Chr(13));

    TempString := GetResponse ('Enter team or club (if any) : ');

    IF TempString <> '' THEN
        WriteLn (FileWrite, 'CLUB: ' + TempString,Chr(13));

    { Do the name and address thing }

    NameFileDirectory := FindDirectory ('CABNAME.DAT');

    Address2 := '';
    Address3 := '';
    Address4 := '';

    IF NameFileDirectory <> '' THEN
        BEGIN
        IF 'Y' = GetKeyResponse ('Okay to use previously stored name and address? (Y, N): ') THEN
            BEGIN
            IF OpenFileForRead (NameFileRead, NameFileDirectory + DirectorySeparator + CabrilloNameFileName) THEN
                BEGIN
                ReadLn (NameFileRead);
                ReadLn (NameFileRead, Name);
                ReadLn (NameFileRead, Address1);
                IF NOT Eof (NameFileRead) THEN ReadLn (NameFileRead, Address2);
                IF NOT Eof (NameFileREad) THEN ReadLn (NameFileRead, Address3);
                IF NOT Eof (NameFileREad) THEN ReadLn (NameFileRead, Address4);
                Close  (NameFileRead);
                END
            ELSE
                BEGIN
                ReportError ('ERROR!!  CABNAME file was there - but now it is not.');
                Halt;
                END;
            END
        ELSE
            NameFileDirectory := '';
        END;

    IF NameFileDirectory = '' THEN
        BEGIN
        Name := GetResponse ('Enter your full name : ');
        Address1 := GetResponse ('Address (first line) : ');
        Address2 := GetResponse ('Address (second line) : ');
        Address3 := GetResponse ('Address (third line) : ');
        Address4 := GetResponse ('Address (last line) : ');

        IF 'Y' = GetKeyResponse ('Would you like me to save this information for next time? (Y, N) : ') THEN
            BEGIN
            NameFileDirectory := GetEnv('HOME')+DirectorySeparator + '.trlog';
            IF NOT DirectoryExists(NameFileDirectory) then
            BEGIN
               IF NOT CreateDir(NameFileDirectory) then
                  WriteLn('Failed to create $HOME/.trlog');
            END;

            OpenFileForWrite (NameFileWrite, NameFileDirectory + DirectorySeparator + CabrilloNameFileName);
            WriteLn (NameFileWrite, 'This is your default name and address that is used by the Cabrillo procedure.');
            WriteLn (NameFileWrite, Name);
            WriteLn (NameFileWrite, Address1);
            WriteLn (NameFileWrite, Address2);
            WriteLn (NameFileWrite, Address3);
            WriteLn (NameFileWrite, Address4);
            Close (NameFileWrite);
            END;
        END;

    WriteLn (FileWrite, 'NAME: ', Name,Chr(13));
    WriteLn (FileWrite, 'ADDRESS: ', Address1,Chr(13));

    IF Address2 <> '' THEN WriteLn (FileWrite, 'ADDRESS: ', Address2,Chr(13));
    IF Address3 <> '' THEN WriteLn (FileWrite, 'ADDRESS: ', Address3,Chr(13));
    IF Address4 <> '' THEN WriteLn (FileWrite, 'ADDRESS: ', Address4,Chr(13));

    { Do operator list}

    Operators := GetResponse ('Enter Operator(s) : ');

    IF Operators <> '' THEN
        WriteLn (FileWrite, 'OPERATORS: ', Operators,Chr(13));

    { Do soapbox }

    WriteLn ('Enter as many soapbox comment lines as you want.  Enter blank line to stop.');

    TextColor (Yellow);

    REPEAT
        TextColor (Cyan);
        Write ('SOAPBOX: ');
        TextColor (Yellow);
        ReadLn (TempString);
        IF TempString <> '' THEN
            WriteLn (FileWrite, 'SOAPBOX: ', TempString,Chr(13));
    UNTIL TempString = '';

    GenerateSummaryPortionOfCabrilloFile := True;
    END;


{START-OF-LOG: 2.0
CONTEST: WAEDC
CALLSIGN: DL8WPX
CATEGORY: SINGLE-OP ALL LOW
CATEGORY-ASSISTED: NON-ASSISTED
CLAIMED-SCORE: 1456
OPERATORS: DL8WPX
CLUB: Bavarian Contest Club
NAME: Joerg Puchstein
ADDRESS: Jung-Jochen-Weg 16B
ADDRESS: 18069 Rostock
ADDRESS: Germany
SOAPBOX: Just a couple of hours left for this one, sri.
SOAPBOX: At least I catched HC8N for an all time new one at this weekend.
OFFTIME: 2001-09-11 0000 2001-09-11 2026
OFFTIME: 2001-09-11 2210 2001-09-12 2400
QSO: 14234 PH 2001-09-11 2026 DL8WPX        59     623 K3NM          59     798
QSO: 14234 PH 2001-09-11 2027 DL8WPX        59     624 9K9O          59     975
QSO: 14234 PH 2001-09-11 2029 DL8WPX        59     625 LT5F          59     589
QSO: 14234 PH 2001-09-11 2044 DL8WPX        59     626 WF3M          59     180
QSO: 14234 PH 2001-09-11 2052 DL8WPX        59     627 LU1DZ         59     621
QSO: 14234 PH 2001-09-11 2057 DL8WPX        59     628 JR3KQJ        59      14
QSO: 14234 PH 2001-09-11 2058 DL8WPX        59     629 JA0QWO        59     419
QSO: 14234 PH 2001-09-11 2112 DL8WPX        59     630 JG3VEI        59     503
QSO: 14234 PH 2001-09-11 2115 DL8WPX        59     631 KC1F          59     147
QSO: 14234 PH 2001-09-11 2119 DL8WPX        59     632 TU2XZ         59     573
QSO: 14234 PH 2001-09-11 2132 DL8WPX        59     633 JA3YBK        59     940
QSO: 14234 PH 2001-09-11 2137 DL8WPX        59     634 C4A           59    1187
QTC: 14234 PH 2001-09-11 2138 DL8WPX        123/10     C4A           2109 OH1VR          376
QTC: 14234 PH 2001-09-11 2138 DL8WPX        123/10     C4A           2109 OM4WW          109
QTC: 14234 PH 2001-09-11 2138 DL8WPX        123/10     C4A           2110 LZ1QZ          170
QTC: 14234 PH 2001-09-11 2138 DL8WPX        123/10     C4A           2110 ES4MM          170
QTC: 14234 PH 2001-09-11 2138 DL8WPX        123/10     C4A           2111 RW3QO          204
QTC: 14234 PH 2001-09-11 2138 DL8WPX        123/10     C4A           2112 G3LKZ           86
QTC: 14234 PH 2001-09-11 2138 DL8WPX        123/10     C4A           2113 OK2KR          196
QTC: 14234 PH 2001-09-11 2138 DL8WPX        123/10     C4A           2114 HA6KNX         232
QTC: 14234 PH 2001-09-11 2138 DL8WPX        123/10     C4A           2116 SP9XCN         410
QTC: 14234 PH 2001-09-11 2138 DL8WPX        123/10     C4A           2119 9A2EY           49
QSO: 14234 PH 2001-09-11 2151 DL8WPX        59     635 UA9AYA        59     904
QSO: 14234 PH 2001-09-11 2201 DL8WPX        59     636 JA1YPA        59     532
QSO: 14234 PH 2001-09-11 2205 DL8WPX        59     637 N2NI          59       3
QSO: 14234 PH 2001-09-11 2207 DL8WPX        59     638 K1BV          59     185
QSO: 14234 PH 2001-09-11 2209 DL8WPX        59     639 N1XS          59     236
QSO: 21289 PH 2001-09-12 1412 DL8WPX        59     000 HC8N          59    1620
END-OF-LOG:
}

FUNCTION GenerateLogPortionOfCabrilloFile (CabrilloFileName: Str80;
                                           VAR FileWrite: TEXT): BOOLEAN;

{ This function relies on some global variables defined by the previous
  routines.  It does not know specifically which contest it is processing

  MyCall: STRING;
  NumberExchangeElementsToRecord: INTEGER;
  QTCEnable: BOOLEAN;
  ReceivedCallCursorPosition: INTEGER;
  ReceivedQSONumberLength: INTEGER;
  RSTIsPartOfTheExchange: BOOLEAN;
  SentDataLength:      INTEGER;
  SentInformation: STRING;
  SentQSONumberLength: INTEGER;
  SniffForTransmitter: BOOLEAN;
  SuppressRST: BOOLEAN;

}


LABEL DoItAgain;

VAR QTCFileRead, FileRead: TEXT;
    QTCFileString, FileString, CabrilloString: STRING;
    QTCWriteString: STRING;
    ExchangeString, TimeString, DateString: Str40;
    SentData, ModeString, FrequencyString, MyCallString: Str40;
    NumberQSOs: WORD;
    Band: BandType;
    Mode: ModeType;
    Count, QSONumberSent, NumberReceived, CursorPosition: INTEGER;
    RSTReceived, RSTSentString, CallReceivedString, QSONumberSentString: Str20;
    Transmitter0Char, ComputerID: CHAR;

    QTCIndexNumberString, QTCNumber, QTCCall, QTCDate, QTCTime: CallString;
    QTCBandModeString: CallString;

    QTCBand: BandType;
    QTCMode: ModeType;

    QTCEntry, QTCIndexNumber, xResult: INTEGER;
    QNumber: INTEGER;

    BEGIN
    QNumber := 0;
    Transmitter0Char := Chr (0);

    GenerateLogPortionOfCabrilloFile := False;
    NumberQSOs := 0;

    IF QTCEnable THEN
        IF NOT OpenFileForRead (QTCFileRead, 'QTC.DAT') THEN
            BEGIN
            ReportError ('Can not open QTC.DAT file!!');

            REPEAT
                Key := UpCase (GetKey ('Do you want to continue? (Y/N) : '));
                IF (Key = EscapeKey) OR (Key = 'N') THEN Exit;
            UNTIL Key = 'Y';

            QTCEnable := False;
            END;

    IF QTCEnable THEN New (QTCList);

    IF OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        Write (NumberQSOs);

        { Some things only need to be done once - do them here }

        { Format my callsign }

        MyCallString := MyCall;

        IF Length (MyCallString) >  CallsignLength THEN
            MyCallString := Copy (MyCallString, 1, CallsignLength);

        WHILE Length (MyCallString) < CallsignLength DO
            MyCallString := MyCallString + ' ';

        TextColor (Cyan);

        { Now - read in the log and process the entries }

        WHILE NOT Eof (FileRead) DO
            BEGIN
            REPEAT
                ReadLn (FileRead, FileString);

                IF QTCEnable THEN
                    IF StringHas (FileString, '; QTC') THEN
                        BEGIN
                        Delete (FileString, 1, 2);  { ; and space }
                        GetRidOfPostcedingSpaces (FileString);
                        WriteLn;
                        WriteLn (FileString, '.');

                        REPEAT
                            ReadLn (QTCFileRead, QTCFileString);
                            GetRidOfPostcedingSpaces (QTCFileString);

                            IF Eof (QTCFileRead) THEN
                                BEGIN
                                ReportError ('QTC file problem.  Could not find log file entry in QTC.DAT');
                                WriteLn (FileString);
                                Halt;
                                END;

                        UNTIL (QTCFileString = FileString);

                        { QTC.DAT entry has matched up with log entry }

                        RemoveFirstString (FileString);  { QTC }
                        QTCNumber := RemoveFirstString (FileString); { 1/10 }

                        WHILE Length (QTCNumber) < 7 DO
                            QTCNumber := ' ' + QTCNumber;

                        RemoveFirstString (FileString);  { with }
                        QTCCall := RemoveFirstString (FileString);

                        WHILE Length (QTCCall) < 12 DO
                            QTCCall := QTCCall + ' ';

                        RemoveFirstString (FileString);  { on }
                        QTCDate := RemoveFirstString (FileString); { 16-Aug-03 }

                        { Convert the date format to Cabrillo }

                        QTCDate := GetCabrilloDateStringFromDateString (QTCDate);

                        RemoveFirstString (FileString);  { at }

                        QTCTime := RemoveFirstString (FileString); { 14:27 }
                        QTCTime := NumberPartOfString (QTCTime);

                        RemoveFirstString (FileString);  { on }
                        QTCBandModeString := RemoveFirstString (FileString); { 80CW }

                        QTCBand := GetLogEntryBand (' ' + QTCBandModeString + ' ');
                        QTCMode := GetLogEntryMode (' ' + QTCBandModeString + ' ');

                        FOR QTCEntry := 0 TO 9 DO         { Clear the array }
                            WITH QTCList^ [QTCEntry] DO
                                BEGIN
                                Time := '';
                                Call := '';
                                Number:= '';
                                END;

                        REPEAT
                            ReadLn (QTCFileRead, QTCFileString);

                            GetRidOfPrecedingSpaces (QTCFileString);

                            IF QTCFileString <> '' THEN
                                BEGIN
                                WriteLn (QTCFileString);

    DoItAgain: { I know - this is a cheap trick  - used to read the second column }

                                QTCIndexNumberString := RemoveFirstString (QTCFileString);
                                QTCIndexNumberString := PrecedingString (QTCIndexNumberString, ':');

                                Val (QTCIndexNumberString, QTCIndexNumber, xResult);

                                IF xResult <> 0 THEN
                                    BEGIN
                                    ReportError ('Error reading QTC.DAT file.');
                                    WriteLn (QTCFileString);
                                    END;

                                IF (QTCIndexNumber < 1) OR (QTCIndexNumber > 10) THEN
                                    BEGIN
                                    ReportError ('Number error reading QTC.DAT file.');
                                    WriteLn (QTCFileString);
                                    END;

                                Dec (QTCIndexNumber);

                                WITH QTCList^ [QTCIndexNumber] DO
                                    BEGIN
                                    Time := RemoveFirstString (QTCFileString);
                                    Time := NumberPartOfString (Time);

                                    Call := RemoveFirstString (QTCFileString);
                                    Number:= RemoveFirstString (QTCFileString);
                                    END;

                                GetRidOfPrecedingSpaces (QTCFileString);

                                IF QTCFileString <> '' THEN GoTo DoItAgain;

                                QTCFileString := ' ';
                                END;

                        UNTIL (QTCFileString = '');

                        { Okay, we have read in all of the data we need to spit
                          out the QTC entries - now what??? Oh yeah, make it look
                          like this:

QTC: 14234 PH 2001-09-11 2138 DL8WPX        123/10     C4A           2109 OH1VR          376
Call fields end here for length=12                                

}

                        FOR QTCEntry := 0 TO 9 DO
                            WITH QTCList^ [QTCEntry] DO
                                BEGIN
                                IF Call <> '' THEN
                                    BEGIN
                                    WHILE Length (Call) < 12 DO
                                        Call := Call + ' ';

                                    WHILE Length (Number) < 6 DO
                                        Number := ' ' + Number;

                                    QTCWriteString := 'QTC: ' +
                                                      GetFrequencyStringFromBand (QTCBand, '') +
                                                      ' ' +
                                                      GetModeStringFromMode (QTCMode) +
                                                      ' ' +
                                                      QTCDate + ' ' + QTCTime + ' ' +
                                                      MyCallString +
                                                      QTCNumber + '     ' +
                                                      QTCCall + '  ' +
                                                      Time + ' ' +
                                                      Call + Number;



                                    WriteLn (FileWrite, QTCWriteString,Chr(13));
                                    END;
                                END;


                        Continue; { Skip this entry and get on with the next }
                        END;

                Band := GetLogEntryBand (FileString);
            UNTIL (Band <> NoBand) OR EOF (FileRead);

            Mode := GetLogEntryMode (FileString);

            IF Band <> NoBand THEN
                BEGIN
                IF (FileString [1] = ';') THEN Continue;

                IF Length (FileString) < 60 THEN Continue;
                IF Copy (FileString, 1, 1) = LineFeed THEN Continue;

                ExpandTabs (FileString);

                IF FileString [42] = '*' THEN FileString [42] := ' ';

                { Save the exchange part for later }

                ExchangeString := GetLogEntryExchangeString (FileString);

                { The first RST is always there }

                IF RSTIsPartOfTheExchange THEN
                    RemoveFirstString (ExchangeString);

                GetRidOfPrecedingSpaces (ExchangeString);

                IF RSTIsPartOfTheExchange THEN
                    IF StringIsAllNumbers (GetFirstString (ExchangeString)) THEN
                        BEGIN
                        RSTReceived := RemoveFirstString (ExchangeString);

                        WHILE Length (RSTReceived) < 3 DO
                            RSTReceived := RSTReceived + ' ';

                        GetRidOfPrecedingSpaces (ExchangeString);

                        IF SuppressRST THEN RSTReceived := '   ';
                        END
                    ELSE
                        IF (Mode = CW) OR (Mode = Digital) THEN
                            RSTReceived := '599'
                        ELSE
                            RSTReceived := '59';


                IF ReceivedQSONumberLength > 0 THEN
                    BEGIN
                    NumberReceived := RemoveFirstLongInteger (ExchangeString);
                    GetRidOfPrecedingSpaces (ExchangeString);

                    Str (NumberReceived:ReceivedQSONumberLength, TempString);

                    ExchangeString := TempString + ' ' + ExchangeString;
                    END;

                IF NumberExchangeElementsToRecord > 0 THEN
                    BEGIN
                    TempString := '';

                    FOR Count := 1 TO NumberExchangeElementsToRecord DO
                        TempString := TempString + RemoveFirstString (ExchangeString) + ' ';

                    GetRidOfPostcedingSpaces (TempString);

                    ExchangeString := TempString;
                    END;

                WHILE Pos ('  ', ExchangeString) > 0 DO
                    Delete (ExchangeString, Pos ('  ', ExchangeString), 1);

                { Get rid of any S&P indicator }

                WHILE Pos ('$', FileString) > 0 DO
                    FileString [Pos ('$', FileString)] := ' ';

                Mode := GetLogEntryMode (FileString);

                ModeString := GetModeStringFromMode (Mode);

                { Generate a fake frequency for the band/mode }

                FrequencyString := GetFrequencyStringFromBand (Band, Copy (FileString, 24, 4));

                CabrilloString := 'QSO: ' + FrequencyString + ' ' + ModeString + ' ';

                DateString := GetCabrilloDateStringFromLogEntry (FileString);

                CabrilloString := CabrilloString + DateString + ' ';

                { Now do the time }

                TimeString := Copy (FileString, LogEntryHourAddress, 2) +
                              Copy (FileString, LogEntryMinuteAddress, 2);

                CabrilloString := CabrilloString + TimeString + ' ';

                { Add my callsign which was previously formatted }

                CabrilloString := CabrilloString + MyCallString + ' ';

                { Do the exchange sent }

                IF SentInformation <> '' THEN
                    BEGIN
                    SentData := SentInformation;

                    IF StringHas (SentData, '$') THEN  { RST sent }
                        BEGIN
                        CursorPosition := Pos ('$', SentData);

                        Delete (SentData, CursorPosition, 1);

                        RSTSentString := GetFirstString (GetLogEntryExchangeString (FileString));

                        WHILE Length (RSTSentString) < 3 DO
                            RSTSentString := RSTSentString + ' ';

                        Insert (RSTSentString, SentData, CursorPosition);
                        END
                    ELSE
                        IF SuppressRST THEN
                            SentData := '   ' + SentData;

                    IF StringHas (SentData, '#') THEN
                        BEGIN
                        CursorPosition := Pos ('#', SentData);

                        Delete (SentData, CursorPosition, 1);

                        QSONumberSent := GetLogEntryQSONumber (FileString);
                        IF QSONumberSent = -2 then
                        Begin
                           Inc(Qnumber);
                           QSONumberSent := Qnumber;
                        End;

                        Str (QSONumberSent:SentQSONumberLength, QSONumberSentString);

                        Insert (QSONumberSentString, SentData, CursorPosition);
                        END;

                    IF StringHas (SentData, ControlN) THEN
                        BEGIN
                        CursorPosition := Pos (ControlN, SentData);
                        Delete (SentData, CursorPosition, 1);
                        Insert (PreviousName, SentData, CursorPosition);
                        END;

                    WHILE Length (SentData) < SentDataLength DO
                        SentData := SentData + ' ';

                    CabrilloString := CabrilloString + SentData + ' ';
                    END;

                WHILE Length (CabrilloString) < ReceivedCallCursorPosition - 1 DO
                    CabrilloString := CabrilloString + ' ';

                CallReceivedString := GetLogEntryCall (FileString);

                IF Length (CallReceivedString) > CallsignLength THEN
                    CallReceivedString := Copy (CallReceivedString, 1, CallSignLength)
                ELSE
                    WHILE Length (CallReceivedString) < CallsignLength DO
                        CallReceivedString := CallReceivedString + ' ';

                CabrilloString := CabrilloString + CallReceivedString + ' ';

                IF RSTIsPartOfTheExchange THEN
                    CabrilloString := CabrilloString + RSTReceived + ' ' + ExchangeString
                ELSE
                    CabrilloString := CabrilloString + ExchangeString;

                IF PreviousName <> '' THEN
                    BEGIN
                    RemoveFirstString (ExchangeString);
                    PreviousName := RemoveFirstString (ExchangeString);
                    END;

                IF SniffForTransmitter THEN  { ARRL DX Multi2 } {KK1L: 6.71 Or other contests as needed}
                    BEGIN
                    WHILE Length (CabrilloString) < 79 DO  {KK1L: 6.73 Need to add space for contests >80 columns. Was 80.}
                        CabrilloString := CabrilloString + ' ';

                    CabrilloString := CabrilloString + ' ';{KK1L: 6.73 Will always add a space.}

                    ComputerId := GetLogEntryComputerID (FileString);

                    IF Transmitter0Char = Chr (0) THEN
                        Transmitter0Char := ComputerID;

                    IF Transmitter0Char = ComputerID THEN
                        CabrilloString := CabrilloString + '0'
                    ELSE
                        CabrilloString := CabrilloString + '1';
                    END;

                WriteLn (FileWrite, CabrilloString,Chr(13));


                Inc (NumberQSOs);
                GoToXY (1, WhereY);
                Write (NumberQSOs);
                END;
            END;
        Close (FileRead);

        GenerateLogPortionOfCabrilloFile := True;
        END

    ELSE
        BEGIN
        ReportError ('Unable to open log file ' + LogFileName);
        WaitForKeyPressed;
        END;

    IF QTCEnable THEN Dispose (QTCList);
    END;



PROCEDURE CreateCabrilloFile;

VAR CabrilloFileName: Str40;
    CabrilloFileWrite: TEXT;

    BEGIN
    ClearScreenAndTitle ('CREATE CABRILLO FILE');

    WriteLn ('The Cabrillo format has been adopted by the ARRL and CQ and the preferred');
    WriteLn ('format for entry submission.  An entry consists of all of the necessary ');
    WriteLn ('data to submit your log - including summary sheet.');
    WriteLn;
    WriteLn ('This procedure will generate a Cabrillo file for the active log file.  It');
    WriteLn ('will be saved with the active file name prefix and .CBR as the suffix.');
    WriteLn;

    IF NOT OkayToProceed THEN Exit;

    CabrilloFileName := PrecedingString (LogFileName, '.') + '.CBR';

    IF FileExists (CabrilloFileName) THEN
        IF NOT OkayToDeleteExistingFile (CabrilloFileName) THEN Exit;

    IF NOT GenerateSummaryPortionOfCabrilloFile (CabrilloFileName, CabrilloFileWrite) THEN
        Exit;

    IF NOT GenerateLogPortionOfCabrilloFile (CabrilloFileName, CabrilloFileWrite) THEN
        BEGIN
        Close (CabrilloFileWrite);
        Exit;
        END;

    WriteLn (CabrilloFileWrite, 'END-OF-LOG: ',Chr(13));
    Close (CabrilloFileWrite);

    WriteLn;
    WriteLn ('Cabrillo sucessfully generated and saved as ' + CabrilloFileName);
    WaitForKeyPressed;
    END;


    BEGIN
    END.
