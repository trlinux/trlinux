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

UNIT PostMult;

{$O+}
{$V-}

INTERFACE

Uses trCrt,
     Tree,
     SlowTree,
     PostSubs,
     Country9,
     LogDupe,
     LogWind,
     ZoneCont;

PROCEDURE MultiplierReportMenu;

TYPE
     MultCountryTotalRecordType = RECORD
         FirstCall: CallString;
         TotalQSOs: INTEGER;
         END;

     CountryMultiplierArray = ARRAY [Band160..Band10, 0..MaxCountries - 1] OF MultCountryTotalRecordType;
     CountryMultiplierArrayPointer = ^CountryMultiplierArray;

     HourTotalsRecord = RECORD
         HourName:  STRING [2];
         QSOs:      ARRAY [BandType, ModeType] OF INTEGER;
         QSOPoints: ARRAY [BandType, ModeType] OF INTEGER;
         Mults:     ARRAY [BandType, ModeType] OF INTEGER;
         Minutes:   ARRAY [BandType, ModeType] OF INTEGER;
         END;

    LogHourTotalArray = ARRAY [0..49] OF HourTotalsRecord;
    LogHourTotalArrayPointer = ^LogHourTotalArray;

    PLEFTONBIStateType = RECORD
        LastBand:        BandType;
        LastMode:        ModeType;
        LastDateString:  Str20;
        LastTimeString:  Str20;
        LastHourIndex:   INTEGER;
        DoingBreak:      BOOLEAN;
        BandsThisMinute: ARRAY [BandType] OF BOOLEAN;
        END;

VAR BandModeUsed:      ARRAY [BandType, ModeType] OF BOOLEAN;

    ContestDates:      ARRAY [0..10] OF STRING [10];
    CountryMultTotals: CountryMultiplierArrayPointer;
    CWUsed:            BOOLEAN;

    DigitalUsed:       BOOLEAN;
    DomesticTotals:    ARRAY [BandType, CW..Both] OF WORD;
    DXTotals:          ARRAY [BandType, CW..Both] OF WORD;

    Header:            Str80;

    LogHourTotals:     LogHourTotalArrayPointer;

    NumberDates:       INTEGER;
    NumberLogHours:    INTEGER;

    PhoneUsed:         BOOLEAN;
    PLeftOnBiState:    PLEFTONBIStateType;

    PrefixTotals:      ARRAY [BandType, CW..Both] OF WORD;
    QSOPointTotals:    ARRAY [BandType, CW..Both] OF WORD;
    RawQSOTotals:      ARRAY [BandType, CW..Both] OF WORD;

    SummaryWrite:      TEXT;

    TotalLogQSOs:      LONGINT;

    ZoneTotals:        ARRAY [BandType, CW..Both] OF WORD;

{ FUNCTION Number (Call: CallString): Char; EXTERNAL;}



IMPLEMENTATION
uses keycode,classes;


FUNCTION GenerateCountryMultiplierTotals: BOOLEAN;

{ This procedure will go through the log file and determine how many QSOs
  were made with each country - and record the callsign of the first QSO
  for each country.  The results are saved in the structure
  CountryMultTotals. }

VAR FileRead: TEXT;
    MultString, FileString, TempString: Str80;
    Band: BandType;
    Mode: ModeType;
    CountryIndex: INTEGER;

    BEGIN
    GenerateCountryMultiplierTotals := False;

    FOR Band := Band160 TO Band10 DO
        FOR CountryIndex := 0 TO MaxCountries - 1 DO
            BEGIN
            CountryMultTotals^ [Band, CountryIndex].FirstCall := '';
            CountryMultTotals^ [Band, CountryIndex].TotalQSOs := 0;
            END;

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError (LogFileName + ' file not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    Write ('Searching log file for country multipliers...');

    TotalLogQSOs := 0;

    REPEAT
        REPEAT
            ReadLn (FileRead, FileString);
            Band := GetLogEntryBand (FileString);
            Mode := GetLogEntryMode (FileString);
        UNTIL ((Band <> NoBand) AND (Mode <> NoMode)) OR EOF (FileRead);

       IF NOT (StringHas (FileString, '*DUPE*') OR StringHas (FileString, '*ZERO*')) THEN
           BEGIN
           ExpandTabs (FileString);
           MultString := GetLogEntryMultString (FileString);

           Inc (TotalLogQSOs);

           IF MultString <> '' THEN
               WHILE MultString <> '' DO
                   BEGIN
                   TempString := RemoveFirstString (MultString);

                   CountryIndex := CountryTable.GetCountry (TempString, True);


                   if CountryIndex <> -1 then
                   IF NOT StringHasLowerCase (TempString) THEN
                       IF NOT StringIsAllNumbers (TempString) THEN
                           IF CountryMultTotals^ [Band, CountryIndex].TotalQSOs = 0 THEN
                               CountryMultTotals^ [Band, CountryIndex].FirstCall := GetLogEntryCall (FileString);
                   END;

           CountryIndex := CountryTable.GetCountry (GetLogEntryCall (FileString), True);
           if CountryIndex <> -1 then
              Inc (CountryMultTotals^ [Band, CountryIndex].TotalQSOs);
           END;

    UNTIL Eof (FileRead);
    Close (FileRead);

    GoToXY (1, WhereY);
    ClrEol;

    GenerateCountryMultiplierTotals := True;
    END;



FUNCTION GenerateZoneMultiplierTotals: BOOLEAN;

{ This procedure will go through the log file and determine how many QSOs
  were made with each zone - and record the callsign of the first QSO
  for each zone.  The results are saved in the structure
  CountryMultTotals. }

VAR FileRead: TEXT;
    FileString, MultString, ZoneString, TempString: Str80;
    Band: BandType;
    Mode: ModeType;
    ZoneIndex: INTEGER;

    BEGIN
    GenerateZoneMultiplierTotals := False;

    FOR Band := Band160 TO Band10 DO
        FOR ZoneIndex := 0 TO MaxCountries - 1 DO
            BEGIN
            CountryMultTotals^ [Band, ZoneIndex].FirstCall := '';
            CountryMultTotals^ [Band, ZoneIndex].TotalQSOs := 0;
            END;

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError (LogFileName + ' file not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    Write ('Searching log file for zone multipliers...');

    TotalLogQSOs := 0;

    REPEAT
        REPEAT
            ReadLn (FileRead, FileString);
            Band := GetLogEntryBand (FileString);
            Mode := GetLogEntryMode (FileString);
        UNTIL ((Band <> NoBand) AND (Mode <> NoMode)) OR EOF (FileRead);

       IF NOT (StringHas (FileString, '*DUPE*') OR StringHas (FileString, '*ZERO*')) THEN
           BEGIN
           ExpandTabs (FileString);
           MultString := GetLogEntryMultString (FileString);

           Inc (TotalLogQSOs);

           IF MultString <> '' THEN
               WHILE MultString <> '' DO
                   BEGIN
                   TempString := RemoveFirstString (MultString);

                   IF StringIsAllNumbers (TempString) THEN
                       BEGIN
                       Val (TempString, ZoneIndex);

                       IF (ZoneIndex > 0) AND (ZoneIndex <= 100) THEN
                           IF CountryMultTotals^ [Band, ZoneIndex].TotalQSOs = 0 THEN
                              CountryMultTotals^ [Band, ZoneIndex].FirstCall := GetLogEntryCall (FileString);
                       END;
                   END;

           TempString := GetLogEntryExchangeString (FileString);
           RemoveFirstString (TempString);
           RemoveFirstString (TempString);
           ZoneString := RemoveFirstString (TempString);

           IF StringIsAllNumbers (ZoneString) THEN
               BEGIN
               Val (ZoneString, ZoneIndex);

               IF (ZoneIndex > 0) AND (ZoneIndex <= 100) THEN
                   Inc (CountryMultTotals^ [Band, ZoneIndex].TotalQSOs);
               END;
           END;

    UNTIL Eof (FileRead);
    Close (FileRead);

    GoToXY (1, WhereY);
    ClrEol;

    GenerateZoneMultiplierTotals := True;
    END;



PROCEDURE PrintFirstCountryMultiplierCallsigns;

VAR Lines, CountryIndex: INTEGER;
    TempString, FileName: Str80;
    FileWrite: TEXT;
    Destination: CHAR;

    BEGIN
    REPEAT
    Destination := UpCase (GetKey ('Output to (F)ile, (P)rinter or (S)creen? : '));
        CASE Destination OF
            'F': BEGIN
                 WriteLn;
                 FileName := UpperCase (GetResponse ('Enter filename to save multiplier report to : '));

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

    OpenFileForWrite (FileWrite, FileName);

    IF Header = '' THEN
        Header := GetResponse ('Enter contest name and callsign : ');

    Lines := 0;

    WriteLnVarCenter (FileWrite, 'COUNTRIES WORKED - WITH CALL OF FIRST STATION WORKED');
    WriteLn (FileWrite);
    WriteLnVarCenter (FileWrite, Header);
    WriteLn (FileWrite);

    WriteLn (FileWrite, 'Prefix   160        80          40          20          15          10');
    WriteLn (FileWrite, '------ --------   --------    --------    --------    --------    --------');

    Lines := 5;

    CountryIndex := 0;

    WHILE CountryTable.GetCountryID (CountryIndex) <> '' DO
      BEGIN

      IF (CountryMultTotals^ [Band160, CountryIndex].TotalQSOs > 0) OR
         (CountryMultTotals^ [Band80,  CountryIndex].TotalQSOs > 0) OR
         (CountryMultTotals^ [Band40,  CountryIndex].TotalQSOs > 0) OR
         (CountryMultTotals^ [Band20,  CountryIndex].TotalQSOs > 0) OR
         (CountryMultTotals^ [Band15,  CountryIndex].TotalQSOs > 0) OR
         (CountryMultTotals^ [Band10,  CountryIndex].TotalQSOs > 0) THEN

             BEGIN
             IF Lines >= 55 THEN
                 BEGIN
                 Write (FileWrite, '');

                 WriteLnVarCenter (FileWrite, 'COUNTRIES WORKED - WITH CALL OF FIRST STATION WORKED');
                 WriteLn (FileWrite);
                 WriteLnVarCenter (FileWrite, Header);
                 WriteLn (FileWrite);
                 WriteLn (FileWrite, 'Prefix   160        80          40          20          15          10');
                 WriteLn (FileWrite, '------ --------   --------    --------    --------    --------    --------');
                 Lines := 5;
                 END;

             TempString := CountryTable.GetCountryID (CountryIndex);

             WHILE Length (TempString) < 6 DO TempString := ' ' + TempString;

             TempString := TempString + ' ';

             IF CountryMultTotals^ [Band160, CountryIndex].TotalQSOs > 0 THEN
                TempString := TempString + CountryMultTotals^ [Band160, CountryIndex].FirstCall;

             WHILE Length (TempString) < 18 DO TempString := TempString + ' ';

             IF CountryMultTotals^ [Band80, CountryIndex].TotalQSOs > 0 THEN
                 TempString := TempString + CountryMultTotals^ [Band80, CountryIndex].FirstCall;

             WHILE Length (TempString) < 30 DO TempString := TempString + ' ';

             IF CountryMultTotals^ [Band40, CountryIndex].TotalQSOs > 0 THEN
                 TempString := TempString + CountryMultTotals^ [Band40, CountryIndex].FirstCall;

             WHILE Length (TempString) < 42 DO TempString := TempString + ' ';

             IF CountryMultTotals^ [Band20, CountryIndex].TotalQSOs > 0 THEN
                 TempString := TempString + CountryMultTotals^ [Band20, CountryIndex].FirstCall;

             WHILE Length (TempString) < 54 DO TempString := TempString + ' ';

             IF CountryMultTotals^ [Band15, CountryIndex].TotalQSOs > 0 THEN
                 TempString := TempString + CountryMultTotals^ [Band15, CountryIndex].FirstCall;

             WHILE Length (TempString) < 66 DO TempString := TempString + ' ';

             IF CountryMultTotals^ [Band10, CountryIndex].TotalQSOs > 0 THEN
                 TempString := TempString + CountryMultTotals^ [Band10, CountryIndex].FirstCall;

             WriteLn (FileWrite, TempString);
             Inc (Lines);
             END;

      Inc (CountryIndex);
      END;

    Write (FileWrite, '');
    Close (FileWrite);
    END;



PROCEDURE PrintFirstZoneMultiplierCallsigns;

VAR Lines, MaxNumberOfZones, ZoneIndex: INTEGER;
    TempString, FileName: Str80;
    FileWrite: TEXT;
    Destination: CHAR;
    Band: BandType;

    BEGIN
    REPEAT
    Destination := UpCase (GetKey ('Output to (F)ile, (P)rinter or (S)creen? : '));
        CASE Destination OF
            'F': BEGIN
                 WriteLn;
                 FileName := UpperCase (GetResponse ('Enter filename to save multiplier report to : '));

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

    OpenFileForWrite (FileWrite, FileName);

    IF Header = '' THEN
        Header := GetResponse ('Enter contest name and callsign : ');

    Lines := 0;

    WriteLnVarCenter (FileWrite, 'ZONES WORKED - WITH CALL OF FIRST STATION WORKED');
    WriteLn (FileWrite);
    WriteLnVarCenter (FileWrite, Header);
    WriteLn (FileWrite);
    WriteLn (FileWrite, 'Zone   160        80          40          20          15          10');
    WriteLn (FileWrite, '---- --------   --------    --------    --------    --------    --------');

    Lines := 5;

    MaxNumberOfZones := 40;

    FOR Band := Band160 TO Band10 DO
        FOR ZoneIndex := 41 TO 100 DO
            IF CountryMultTotals^ [Band, ZoneIndex].TotalQSOs > 0 THEN
                MaxNumberOfZones := ZoneIndex;

    IF (MaxNumberOfZones > 40) THEN
        IF MaxNumberOfZones <= 75 THEN
            MaxNumberOfZones := 75
        ELSE
            MaxNumberOfZones := 100;

    FOR ZoneIndex := 1 TO MaxNumberOfZones DO
      BEGIN
      IF Lines >= 55 THEN
          BEGIN
          Write (FileWrite, '');

          WriteLnVarCenter (FileWrite, 'ZONES WORKED - WITH CALL OF FIRST STATION WORKED');
          WriteLn (FileWrite);
          WriteLnVarCenter (FileWrite, Header);
          WriteLn (FileWrite);
          WriteLn (FileWrite, 'Zone   160        80          40          20          15          10');
          WriteLn (FileWrite, '---- --------   --------    --------    --------    --------    --------');
          Lines := 5;
          END;

      Str (ZoneIndex:3, TempString);

      TempString := TempString + '  ';

      IF CountryMultTotals^ [Band160, ZoneIndex].TotalQSOs > 0 THEN
          TempString := TempString + CountryMultTotals^ [Band160, ZoneIndex].FirstCall;

      WHILE Length (TempString) < 16 DO TempString := TempString + ' ';

      IF CountryMultTotals^ [Band80, ZoneIndex].TotalQSOs > 0 THEN
          TempString := TempString + CountryMultTotals^ [Band80, ZoneIndex].FirstCall;

      WHILE Length (TempString) < 28 DO TempString := TempString + ' ';

      IF CountryMultTotals^ [Band40, ZoneIndex].TotalQSOs > 0 THEN
          TempString := TempString + CountryMultTotals^ [Band40, ZoneIndex].FirstCall;

      WHILE Length (TempString) < 40 DO TempString := TempString + ' ';

      IF CountryMultTotals^ [Band20, ZoneIndex].TotalQSOs > 0 THEN
          TempString := TempString + CountryMultTotals^ [Band20, ZoneIndex].FirstCall;

      WHILE Length (TempString) < 52 DO TempString := TempString + ' ';

      IF CountryMultTotals^ [Band15, ZoneIndex].TotalQSOs > 0 THEN
          TempString := TempString + CountryMultTotals^ [Band15, ZoneIndex].FirstCall;

      WHILE Length (TempString) < 64 DO TempString := TempString + ' ';

      IF CountryMultTotals^ [Band10, ZoneIndex].TotalQSOs > 0 THEN
          TempString := TempString + CountryMultTotals^ [Band10, ZoneIndex].FirstCall;

      WriteLn (FileWrite, TempString);
      Inc (Lines);
      END;

    Write (FileWrite, '');
    Close (FileWrite);
    END;



PROCEDURE PrintQSOsByCountry;

VAR Lines, CountryIndex: INTEGER;
    TempString, FileName: Str80;
    FileWrite: TEXT;
    Destination: CHAR;
    TotalCountryQSOs: LONGINT;

    BEGIN
    REPEAT
    Destination := UpCase (GetKey ('Output to (F)ile, (P)rinter or (S)creen? : '));
        CASE Destination OF
            'F': BEGIN
                 WriteLn;
                 FileName := UpperCase (GetResponse ('Enter filename to save multiplier report to : '));

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

    OpenFileForWrite (FileWrite, FileName);

    IF Header = '' THEN
        Header := GetResponse ('Enter contest name and callsign : ');

    WriteLnVarCenter (FileWrite, 'QSOs MADE IN EACH COUNTRY');
    WriteLn (FileWrite);
    WriteLnVarCenter (FileWrite, Header);
    WriteLn (FileWrite);
    WriteLn (FileWrite, '          Prefix  160   80    40    20    15    10   Total  Percent');
    WriteLn (FileWrite, '          ------  ---  ----  ----  ----  ----  ----  -----  -------');

    Lines := 5;

    CountryIndex := 0;

    WHILE CountryTable.GetCountryID (CountryIndex) <> '' DO
      BEGIN

      IF (CountryMultTotals^ [Band160, CountryIndex].TotalQSOs > 0) OR
         (CountryMultTotals^ [Band80,  CountryIndex].TotalQSOs > 0) OR
         (CountryMultTotals^ [Band40,  CountryIndex].TotalQSOs > 0) OR
         (CountryMultTotals^ [Band20,  CountryIndex].TotalQSOs > 0) OR
         (CountryMultTotals^ [Band15,  CountryIndex].TotalQSOs > 0) OR
         (CountryMultTotals^ [Band10,  CountryIndex].TotalQSOs > 0) THEN

             BEGIN
             IF Lines >= 55 THEN
                 BEGIN
                 Write (FileWrite, '');

                 WriteLn (FileWrite, '                         QSOs MADE IN EACH COUNTRY');
                 WriteLnVarCenter (FileWrite, Header);
                 WriteLn (FileWrite);
                 WriteLn (FileWrite, '          Prefix  160   80    40    20    15    10   Total  Percent');
                 WriteLn (FileWrite, '          ------  ---  ----  ----  ----  ----  ----  -----  -------');

                 Lines := 5;
                 END;

             TempString := CountryTable.GetCountryID (CountryIndex);

             WHILE Length (TempString) < 6 DO TempString := ' ' + TempString;

             Write (FileWrite, '          ', TempString + ' ');

             IF CountryMultTotals^ [Band160, CountryIndex].TotalQSOs > 0 THEN
                 Write (FileWrite, CountryMultTotals^ [Band160, CountryIndex].TotalQSOs:4)
             ELSE
                 Write (FileWrite, '   -');

             IF CountryMultTotals^ [Band80, CountryIndex].TotalQSOs > 0 THEN
                 Write (FileWrite, CountryMultTotals^ [Band80, CountryIndex].TotalQSOs:6)
             ELSE
                 Write (FileWrite, '     -');


             IF CountryMultTotals^ [Band40, CountryIndex].TotalQSOs > 0 THEN
                 Write (FileWrite, CountryMultTotals^ [Band40, CountryIndex].TotalQSOs:6)
             ELSE
                 Write (FileWrite, '     -');


             IF CountryMultTotals^ [Band20, CountryIndex].TotalQSOs > 0 THEN
                 Write (FileWrite, CountryMultTotals^ [Band20, CountryIndex].TotalQSOs:6)
             ELSE
                 Write (FileWrite, '     -');


             IF CountryMultTotals^ [Band15, CountryIndex].TotalQSOs > 0 THEN
                 Write (FileWrite, CountryMultTotals^ [Band15, CountryIndex].TotalQSOs:6)
             ELSE
                 Write (FileWrite, '     -');

             IF CountryMultTotals^ [Band10, CountryIndex].TotalQSOs > 0 THEN
                 Write (FileWrite, CountryMultTotals^ [Band10, CountryIndex].TotalQSOs:6)
             ELSE
                 Write (FileWrite, '     -');

             TotalCountryQSOs := CountryMultTotals^ [Band160, CountryIndex].TotalQSOs +
                                 CountryMultTotals^ [Band80,  CountryIndex].TotalQSOs +
                                 CountryMultTotals^ [Band40,  CountryIndex].TotalQSOs +
                                 CountryMultTotals^ [Band20,  CountryIndex].TotalQSOs +
                                 CountryMultTotals^ [Band15,  CountryIndex].TotalQSOs +
                                 CountryMultTotals^ [Band10,  CountryIndex].TotalQSOs;

             Write (FileWrite, TotalCountryQSOs:7);

             WriteLn (FileWrite, ((TotalCountryQSOs * 100) / TotalLogQSOs):8:2);
             Inc (Lines);
             END;

      Inc (CountryIndex);
      END;

    Write (FileWrite, '');
    Close (FileWrite);
    END;



PROCEDURE PrintQSOsByZone;

VAR Lines, ZoneIndex, MaxNumberOfZones: INTEGER;
    TempString, FileName: Str80;
    FileWrite: TEXT;
    Destination: CHAR;
    TotalZoneQSOs: LONGINT;
    Band: BandType;

    BEGIN
    REPEAT
    Destination := UpCase (GetKey ('Output to (F)ile, (P)rinter or (S)creen? : '));
        CASE Destination OF
            'F': BEGIN
                 WriteLn;
                 FileName := UpperCase (GetResponse ('Enter filename to save multiplier report to : '));

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

    OpenFileForWrite (FileWrite, FileName);

    IF Header = '' THEN
        Header := GetResponse ('Enter contest name and callsign : ');

    WriteLnVarCenter (FileWrite, 'QSOs MADE IN EACH ZONE');
    WriteLn (FileWrite);
    WriteLnVarCenter (FileWrite, Header);
    WriteLn (FileWrite);
    WriteLn (FileWrite, '          Zone  160   80    40    20    15    10   Total  Percent');
    WriteLn (FileWrite, '          ----  ---  ----  ----  ----  ----  ----  -----  -------');

    Lines := 5;

    MaxNumberOfZones := 40;

    FOR Band := Band160 TO Band10 DO
        FOR ZoneIndex := 41 TO 100 DO
            IF CountryMultTotals^ [Band, ZoneIndex].TotalQSOs > 0 THEN
                MaxNumberOfZones := ZoneIndex;

    IF (MaxNumberOfZones > 40) THEN
        IF MaxNumberOfZones <= 75 THEN
            MaxNumberOfZones := 75
        ELSE
            MaxNumberOfZones := 100;

    FOR ZoneIndex := 1 TO MaxNumberOfZones DO
        BEGIN
        IF Lines >= 55 THEN
            BEGIN
            Write (FileWrite, '');

            WriteLnVarCenter (FileWrite, '                        QSOs MADE IN EACH ZONE');
            WriteLn (FileWrite);
            WriteLnVarCenter (FileWrite, Header);
            WriteLn (FileWrite);
            WriteLn (FileWrite, '          Zone  160   80    40    20    15   10   Total  Percent');
            WriteLn (FileWrite, '          ----  ---  ----  ----  ----  ---- ----  -----  -------');

            Lines := 5;
            END;

        Str (ZoneIndex:3, TempString);

        TempString := TempString + '  ';

        Write (FileWrite, '          ', TempString);

        IF CountryMultTotals^ [Band160, ZoneIndex].TotalQSOs > 0 THEN
            Write (FileWrite, CountryMultTotals^ [Band160, ZoneIndex].TotalQSOs:4)
        ELSE
            Write (FileWrite, '   -');

        IF CountryMultTotals^ [Band80, ZoneIndex].TotalQSOs > 0 THEN
            Write (FileWrite, CountryMultTotals^ [Band80, ZoneIndex].TotalQSOs:6)
        ELSE
            Write (FileWrite, '     -');

        IF CountryMultTotals^ [Band40, ZoneIndex].TotalQSOs > 0 THEN
            Write (FileWrite, CountryMultTotals^ [Band40, ZoneIndex].TotalQSOs:6)
        ELSE
            Write (FileWrite, '     -');

        IF CountryMultTotals^ [Band20, ZoneIndex].TotalQSOs > 0 THEN
            Write (FileWrite, CountryMultTotals^ [Band20, ZoneIndex].TotalQSOs:6)
        ELSE
            Write (FileWrite, '     -');

        IF CountryMultTotals^ [Band15, ZoneIndex].TotalQSOs > 0 THEN
            Write (FileWrite, CountryMultTotals^ [Band15, ZoneIndex].TotalQSOs:6)
        ELSE
            Write (FileWrite, '     -');

        IF CountryMultTotals^ [Band10, ZoneIndex].TotalQSOs > 0 THEN
            Write (FileWrite, CountryMultTotals^ [Band10, ZoneIndex].TotalQSOs:6)
        ELSE
            Write (FileWrite, '     -');

        TotalZoneQSOs := CountryMultTotals^ [Band160, ZoneIndex].TotalQSOs +
                            CountryMultTotals^ [Band80,  ZoneIndex].TotalQSOs +
                            CountryMultTotals^ [Band40,  ZoneIndex].TotalQSOs +
                            CountryMultTotals^ [Band20,  ZoneIndex].TotalQSOs +
                            CountryMultTotals^ [Band15,  ZoneIndex].TotalQSOs +
                            CountryMultTotals^ [Band10,  ZoneIndex].TotalQSOs;

        Write (FileWrite, TotalZoneQSOs:7);

        WriteLn (FileWrite, ((TotalZoneQSOs * 100) / TotalLogQSOs):8:2);
        Inc (Lines);
        END;

    Write (FileWrite, '');
    Close (FileWrite);
    END;


PROCEDURE WRTC2018MultiplierCheckOffSheet;
VAR
   Destination: CHAR;
   FileName: Str20;
   CountryIndex, NumberReportEntries: INTEGER;
   RealAddress: INTEGER;
   FileWrite: TEXT;
   FileRead: TEXT;
   MultString, FileString, TempString: Str80;
   Band: BandType;
   Mode: ModeType;
   i,index: INTEGER;
   hqlist, hqbandlist: TStringList;

   BEGIN
   writeln;
   IF Header = '' THEN
       Header := GetResponse ('Enter contest name and callsign : ');
   CountryTable.CountryMode := ARRLCountryMode;
   REPEAT
      Destination := UpCase (GetKey ('Output to (F)ile, or (S)creen? : '));
       CASE Destination OF
           'F': BEGIN
                WriteLn;
                FileName := UpperCase (GetResponse ('Enter filename to save multiplier report to : '));

                IF FileName = UpperCase (LogFileName) THEN
                    BEGIN
                    ReportError ('Output file must be different than active log file!!');
                    WaitForKeyPressed;
                    Exit;
                    END;

                IF FileName = '' THEN Exit;
                END;
           'S': FileName := '';
            EscapeKey: Exit;
            END;
   UNTIL (Destination = 'F') OR (Destination = 'S');

   WriteLn;
   New (CountryMultTotals);
    FOR Band := Band160 TO Band10 DO
        FOR CountryIndex := 0 TO MaxCountries - 1 DO
            BEGIN
            CountryMultTotals^ [Band, CountryIndex].FirstCall := '';
            CountryMultTotals^ [Band, CountryIndex].TotalQSOs := 0;
            END;

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError (LogFileName + ' file not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    Write ('Searching log file for country multipliers...');

    TotalLogQSOs := 0;

    REPEAT
        REPEAT
            ReadLn (FileRead, FileString);
            Band := GetLogEntryBand (FileString);
            Mode := GetLogEntryMode (FileString);
        UNTIL ((Band <> NoBand) AND (Mode <> NoMode)) OR EOF (FileRead);

       IF NOT (StringHas (FileString, '*DUPE*') OR StringHas (FileString, '*ZERO*')) THEN
           BEGIN
           ExpandTabs (FileString);
           MultString := GetLogEntryMultString (FileString);

           Inc (TotalLogQSOs);

           IF MultString <> '' THEN
            IF NOT StringHasLowerCase(MultString) THEN
               WHILE MultString <> '' DO
                   BEGIN
                   TempString := RemoveFirstString (MultString);

                   CountryIndex := CountryTable.GetCountry (TempString, True);


                   if CountryIndex <> -1 then
                   IF NOT StringHasLowerCase (TempString) THEN
                       IF NOT StringIsAllNumbers (TempString) THEN
                           IF CountryMultTotals^ [Band, CountryIndex].TotalQSOs = 0 THEN
                               CountryMultTotals^ [Band, CountryIndex].FirstCall := GetLogEntryCall (FileString);
                   END;

           CountryIndex := CountryTable.GetCountry (GetLogEntryCall (FileString), True);
           MultString := GetLogEntryMultString (FileString);
           IF NOT StringHasLowerCase (MultString) THEN
           if CountryIndex <> -1 then
              Inc (CountryMultTotals^ [Band, CountryIndex].TotalQSOs);
           END;

    UNTIL Eof (FileRead);
    Close (FileRead);
//   IF NOT GenerateCountryMultiplierTotals THEN Exit;
   OpenFileForWrite (FileWrite, FileName);

   New (ReportEntries);
   NumberReportEntries := 0;

   CountryIndex := 0;

   WHILE CountryTable.GetCountryID (CountryIndex) <> '' DO
      BEGIN
      IF (CountryMultTotals^ [Band160, CountryIndex].TotalQSOs > 0) OR
         (CountryMultTotals^ [Band80,  CountryIndex].TotalQSOs > 0) OR
         (CountryMultTotals^ [Band40,  CountryIndex].TotalQSOs > 0) OR
         (CountryMultTotals^ [Band20,  CountryIndex].TotalQSOs > 0) OR
         (CountryMultTotals^ [Band15,  CountryIndex].TotalQSOs > 0) OR
         (CountryMultTotals^ [Band10,  CountryIndex].TotalQSOs > 0) THEN
               BEGIN
               TempString := CountryTable.GetCountryID (CountryIndex);

               WHILE Length (TempString) < 5 DO TempString := ' ' + TempString;
               TempString := TempString + ' ';

               IF CountryMultTotals^ [Band160, CountryIndex].TotalQSOs > 0 THEN
                   TempString := TempString + 'X'
               ELSE
                   TempString := TempString + '_';

               IF CountryMultTotals^ [Band80, CountryIndex].TotalQSOs > 0 THEN
                   TempString := TempString + 'X'
               ELSE
                   TempString := TempString + '_';

               IF CountryMultTotals^ [Band40, CountryIndex].TotalQSOs > 0 THEN
                   TempString := TempString + 'X'
               ELSE
                   TempString := TempString + '_';

               IF CountryMultTotals^ [Band20, CountryIndex].TotalQSOs > 0 THEN
                   TempString := TempString + 'X'
               ELSE
                   TempString := TempString + '_';

               IF CountryMultTotals^ [Band15, CountryIndex].TotalQSOs > 0 THEN
                   TempString := TempString + 'X'
               ELSE
                   TempString := TempString + '_';

               IF CountryMultTotals^ [Band10, CountryIndex].TotalQSOs > 0 THEN
                   TempString := TempString + 'X'
               ELSE
                   TempString := TempString + '_';

               ReportEntries^ [NumberReportEntries] := TempString;
               Inc (NumberReportEntries);
               END;

        Inc (CountryIndex);
        END;

    WHILE (NumberReportEntries MOD 50) <> 0 DO
        BEGIN
        ReportEntries^ [NumberReportEntries] := '            ';
        Inc (NumberReportEntries);
        END;

// hqs here

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError (LogFileName + ' file not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    Write ('Searching log file for hq multipliers...');

    TotalLogQSOs := 0;

    hqlist := TStringList.create;
    hqbandlist := TStringList.create;
    try
       hqlist.sorted := true;
       hqlist.duplicates := dupignore;
       hqbandlist.sorted := true;
       hqbandlist.duplicates := dupignore;

       REPEAT
          REPEAT
             ReadLn (FileRead, FileString);
             Band := GetLogEntryBand (FileString);
             Mode := GetLogEntryMode (FileString);
          UNTIL ((Band <> NoBand) AND (Mode <> NoMode)) OR EOF (FileRead);

          IF NOT (StringHas (FileString, '*DUPE*') OR
             StringHas (FileString, '*ZERO*')) THEN
          BEGIN
             ExpandTabs (FileString);
             MultString := GetLogEntryMultString (FileString);
             Inc (TotalLogQSOs);
             IF MultString <> '' THEN
                WHILE MultString <> '' DO
                BEGIN
                   TempString := RemoveFirstString (MultString);
                   IF StringHasLowerCase (TempString) THEN
                   begin
                      hqlist.add(tempstring);
                      case band of
                         band160: tempstring := tempstring + '160';
                         band80: tempstring := tempstring + '80';
                         band40: tempstring := tempstring + '40';
                         band20: tempstring := tempstring + '20';
                         band15: tempstring := tempstring + '15';
                         band10: tempstring := tempstring + '10';
                      else
                         tempstring := '';
                      end;
                      if tempstring <> '' then hqbandlist.add(tempstring);
                   end;
                END;
           END;

    UNTIL Eof (FileRead);
    Close (FileRead);

    for i := 0 to hqlist.count-1 do
    begin
       TempString := hqlist[i];
       WHILE Length (TempString) < 8 DO TempString := ' ' + TempString;
       TempString := TempString + ' ';
       if hqbandlist.find(hqlist[i]+'160',index) then
          TempString := TempString + 'X'
       else
          TempString := TempString + '_';
       if hqbandlist.find(hqlist[i]+'80',index) then
          TempString := TempString + 'X'
       else
          TempString := TempString + '_';
       if hqbandlist.find(hqlist[i]+'40',index) then
          TempString := TempString + 'X'
       else
          TempString := TempString + '_';
       if hqbandlist.find(hqlist[i]+'20',index) then
          TempString := TempString + 'X'
       else
          TempString := TempString + '_';
       if hqbandlist.find(hqlist[i]+'15',index) then
          TempString := TempString + 'X'
       else
          TempString := TempString + '_';
       if hqbandlist.find(hqlist[i]+'10',index) then
          TempString := TempString + 'X'
       else
          TempString := TempString + '_';

       ReportEntries^ [NumberReportEntries] := TempString;
       Inc (NumberReportEntries);
    END;

    GoToXY (1, WhereY);
    ClrEol;

    finally
       hqlist.free;
       hqbandlist.free;
    end;

    IF NumberReportEntries <= 300 THEN
        BEGIN
        WriteLnVarCenter (FileWrite, 'MULTIPLIER CHECK OFF SHEET');
        WriteLn (FileWrite);
        WriteLnVarCenter (FileWrite, Header);
        WriteLn (FileWrite);
        END
    ELSE
        BEGIN
        WriteLnVarCenter (FileWrite, 'MULTIPLIER CHECK OFF SHEET - Page 1');
        WriteLn (FileWrite);
        WriteLnVarCenter (FileWrite, Header);
        WriteLn (FileWrite);
        END;

    FOR CountryIndex := 0 TO 299 DO
        BEGIN
        RealAddress := ((CountryIndex MOD 6) * 50) + (CountryIndex DIV 6);

        { Only print if there is really a call there }

        IF RealAddress < NumberReportEntries THEN
            Write (FileWrite, ReportEntries^ [RealAddress])
        ELSE
            Write (FileWrite, '           ');

        IF (CountryIndex + 1) MOD 6 = 0 THEN WriteLn (FileWrite);

        ReportEntries^ [RealAddress] := '';
        END;

//    WriteLn (FileWrite, '');

    IF NumberReportEntries > 300 THEN
        BEGIN
        WriteLnVarCenter (FileWrite, 'MULTIPLIER CHECK OFF SHEET - Page 2');
        WriteLn (FileWrite);
        WriteLnVarCenter (FileWrite, Header);
        WriteLn (FileWrite);

        FOR CountryIndex := 0 TO (NumberReportEntries - 301) DO
             ReportEntries^ [CountryIndex] := ReportEntries^ [CountryIndex + 300];

        NumberReportEntries := NumberReportEntries - 300;

        FOR CountryIndex := 0 TO 299 DO
            BEGIN
            RealAddress := ((CountryIndex MOD 6) * 50) + (CountryIndex DIV 6);

        { Only print if there is really a call there }

            IF RealAddress < NumberReportEntries THEN
                Write (FileWrite, ReportEntries^ [RealAddress])
            ELSE
                Write (FileWrite, '            ');

            IF (CountryIndex + 1) MOD 6 = 0 THEN WriteLn (FileWrite);

            ReportEntries^ [RealAddress] := '';
            END;

        WriteLn (FileWrite, '');
        END;

    Close (FileWrite);
    Dispose (ReportEntries);
    WaitForKeyPressed;
   END;

PROCEDURE MultiplierCheckOffSheet;

VAR Destination, Key: CHAR;
    TempString: Str80;
    FileName: Str20;
    DoAllMults: BOOLEAN;
    MaxNumberOfZones, CountryIndex, ZoneIndex, NumberReportEntries: INTEGER;
    RealAddress: INTEGER;
    Band: BandType;
    FileWrite: TEXT;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('MULTIPLIER CHECK OFF SHEET');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This report will generate a check off sheet showing the country and zone ');
    WriteLn ('multipliers that have been worked.');
    WriteLn;

    REPEAT
        Key := UpCase (GetKey ('Use (C)Q or (A)RRL countries? (C/A) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'C') OR (Key = 'A');
    WriteLn;

    IF Key = 'C' THEN
        CountryTable.CountryMode := CQCountryMode
    ELSE
        BEGIN
        CountryTable.CountryMode := ARRLCountryMode;

        REPEAT
            Key := UpCase (GetKey ('Use IARU zones? (Y/N): '));
            IF Key = EscapeKey THEN Exit;
        UNTIL (Key = 'Y') OR (Key = 'N');
        WriteLn;

        IF Key = 'Y' THEN
            CountryTable.ZoneMode := ITUZoneMode
        ELSE
            CountryTable.ZoneMode := CQZoneMode;
        END;

    REPEAT
        Key := UpCase (GetKey ('Do (A)ll multiplier or only ones (W)orked? (A/W) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'A') OR (Key = 'W');
    WriteLn;

    DoAllMults := Key = 'A';

    IF Header = '' THEN
        Header := GetResponse ('Enter contest name and callsign : ');

    REPEAT
       Destination := UpCase (GetKey ('Output to (F)ile, (P)rinter or (S)creen? : '));
        CASE Destination OF
            'F': BEGIN
                 WriteLn;
                 FileName := UpperCase (GetResponse ('Enter filename to save multiplier report to : '));

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

    New (CountryMultTotals);

    IF NOT GenerateCountryMultiplierTotals THEN Exit;

    OpenFileForWrite (FileWrite, FileName);

    New (ReportEntries);
    NumberReportEntries := 0;

    CountryIndex := 0;

    WHILE CountryTable.GetCountryID (CountryIndex) <> '' DO
        BEGIN
        IF (CountryMultTotals^ [Band160, CountryIndex].TotalQSOs > 0) OR
           (CountryMultTotals^ [Band80,  CountryIndex].TotalQSOs > 0) OR
           (CountryMultTotals^ [Band40,  CountryIndex].TotalQSOs > 0) OR
           (CountryMultTotals^ [Band20,  CountryIndex].TotalQSOs > 0) OR
           (CountryMultTotals^ [Band15,  CountryIndex].TotalQSOs > 0) OR
           (CountryMultTotals^ [Band10,  CountryIndex].TotalQSOs > 0) OR DoAllMults THEN
               BEGIN
               TempString := CountryTable.GetCountryID (CountryIndex);

               WHILE Length (TempString) < 5 DO TempString := ' ' + TempString;
               TempString := TempString + ' ';

               IF CountryMultTotals^ [Band160, CountryIndex].TotalQSOs > 0 THEN
                   TempString := TempString + 'X'
               ELSE
                   TempString := TempString + '_';

               IF CountryMultTotals^ [Band80, CountryIndex].TotalQSOs > 0 THEN
                   TempString := TempString + 'X'
               ELSE
                   TempString := TempString + '_';

               IF CountryMultTotals^ [Band40, CountryIndex].TotalQSOs > 0 THEN
                   TempString := TempString + 'X'
               ELSE
                   TempString := TempString + '_';

               IF CountryMultTotals^ [Band20, CountryIndex].TotalQSOs > 0 THEN
                   TempString := TempString + 'X'
               ELSE
                   TempString := TempString + '_';

               IF CountryMultTotals^ [Band15, CountryIndex].TotalQSOs > 0 THEN
                   TempString := TempString + 'X'
               ELSE
                   TempString := TempString + '_';

               IF CountryMultTotals^ [Band10, CountryIndex].TotalQSOs > 0 THEN
                   TempString := TempString + 'X'
               ELSE
                   TempString := TempString + '_';

               ReportEntries^ [NumberReportEntries] := TempString;
               Inc (NumberReportEntries);
               END;

        Inc (CountryIndex);
        END;

    WHILE (NumberReportEntries MOD 50) <> 0 DO
        BEGIN
        ReportEntries^ [NumberReportEntries] := '            ';
        Inc (NumberReportEntries);
        END;

    IF NOT GenerateZoneMultiplierTotals THEN Exit;

    MaxNumberOfZones := 40;

    FOR Band := Band160 TO Band10 DO
        FOR ZoneIndex := 41 TO 100 DO
            IF CountryMultTotals^ [Band, ZoneIndex].TotalQSOs > 0 THEN
                MaxNumberOfZones := ZoneIndex;

    IF (MaxNumberOfZones > 40) THEN
        IF MaxNumberOfZones <= 75 THEN
            MaxNumberOfZones := 75
        ELSE
            MaxNumberOfZones := 100;

    FOR ZoneIndex := 1 TO MaxNumberOfZones DO
        IF (CountryMultTotals^ [Band160, ZoneIndex].TotalQSOs > 0) OR
           (CountryMultTotals^ [Band80,  ZoneIndex].TotalQSOs > 0) OR
           (CountryMultTotals^ [Band40,  ZoneIndex].TotalQSOs > 0) OR
           (CountryMultTotals^ [Band20,  ZoneIndex].TotalQSOs > 0) OR
           (CountryMultTotals^ [Band15,  ZoneIndex].TotalQSOs > 0) OR
           (CountryMultTotals^ [Band10,  ZoneIndex].TotalQSOs > 0) OR DoAllMults THEN
               BEGIN
               Str (ZoneIndex, TempString);

               WHILE Length (TempString) < 5 DO TempString := ' ' + TempString;
               TempString := TempString + ' ';

               IF CountryMultTotals^ [Band160, ZoneIndex].TotalQSOs > 0 THEN
                   TempString := TempString + 'X'
               ELSE
                   TempString := TempString + '_';

               IF CountryMultTotals^ [Band80, ZoneIndex].TotalQSOs > 0 THEN
                   TempString := TempString + 'X'
               ELSE
                   TempString := TempString + '_';

               IF CountryMultTotals^ [Band40, ZoneIndex].TotalQSOs > 0 THEN
                   TempString := TempString + 'X'
               ELSE
                   TempString := TempString + '_';

               IF CountryMultTotals^ [Band20, ZoneIndex].TotalQSOs > 0 THEN
                   TempString := TempString + 'X'
               ELSE
                   TempString := TempString + '_';

               IF CountryMultTotals^ [Band15, ZoneIndex].TotalQSOs > 0 THEN
                   TempString := TempString + 'X'
               ELSE
                   TempString := TempString + '_';

               IF CountryMultTotals^ [Band10, ZoneIndex].TotalQSOs > 0 THEN
                   TempString := TempString + 'X'
               ELSE
                   TempString := TempString + '_';

               ReportEntries^ [NumberReportEntries] := TempString;
               Inc (NumberReportEntries);
               END;

    IF NumberReportEntries <= 300 THEN
        BEGIN
        WriteLnVarCenter (FileWrite, 'MULTIPLIER CHECK OFF SHEET');
        WriteLn (FileWrite);
        WriteLnVarCenter (FileWrite, Header);
        WriteLn (FileWrite);
        END
    ELSE
        BEGIN
        WriteLnVarCenter (FileWrite, 'MULTIPLIER CHECK OFF SHEET - Page 1');
        WriteLn (FileWrite);
        WriteLnVarCenter (FileWrite, Header);
        WriteLn (FileWrite);
        END;

    FOR CountryIndex := 0 TO 299 DO
        BEGIN
        RealAddress := ((CountryIndex MOD 6) * 50) + (CountryIndex DIV 6);

        { Only print if there is really a call there }

        IF RealAddress < NumberReportEntries THEN
            Write (FileWrite, ReportEntries^ [RealAddress])
        ELSE
            Write (FileWrite, '           ');

        IF (CountryIndex + 1) MOD 6 = 0 THEN WriteLn (FileWrite);

        ReportEntries^ [RealAddress] := '';
        END;

    WriteLn (FileWrite, '');

    IF NumberReportEntries > 300 THEN
        BEGIN
        WriteLnVarCenter (FileWrite, 'MULTIPLIER CHECK OFF SHEET - Page 2');
        WriteLn (FileWrite);
        WriteLnVarCenter (FileWrite, Header);
        WriteLn (FileWrite);

        FOR CountryIndex := 0 TO (NumberReportEntries - 301) DO
             ReportEntries^ [CountryIndex] := ReportEntries^ [CountryIndex + 300];

        NumberReportEntries := NumberReportEntries - 300;

        FOR CountryIndex := 0 TO 299 DO
            BEGIN
            RealAddress := ((CountryIndex MOD 6) * 50) + (CountryIndex DIV 6);

        { Only print if there is really a call there }

            IF RealAddress < NumberReportEntries THEN
                Write (FileWrite, ReportEntries^ [RealAddress])
            ELSE
                Write (FileWrite, '            ');

            IF (CountryIndex + 1) MOD 6 = 0 THEN WriteLn (FileWrite);

            ReportEntries^ [RealAddress] := '';
            END;

        WriteLn (FileWrite, '');
        END;

    Close (FileWrite);
    Dispose (ReportEntries);
    WaitForKeyPressed;
    END;



FUNCTION ProcessLogEntryForTimeOnBandInformation (Band: BandType;
                                                  Mode: ModeType;
                                                  TimeString: Str20;
                                                  DateString: Str20;
                                                  HourIndex: INTEGER): BOOLEAN;

{ This procedure will look at the band/mode/time of each QSO and determine
  how to accumulate the on time for each band (in the global LogHourTotals.

  Returns FALSE if an error was found (illegal time). }

VAR TempTimeString, TempDateString: Str20;
    NumberMinutesSinceLastEntry: INTEGER;
    TempBand: BandType;

    BEGIN
    ProcessLogEntryForTimeOnBandInformation := True;

    WITH PLeftOnBiState DO
        BEGIN

        { First, see if it is the same minute as previously processed }

        IF ((TimeString = LastTimeString) AND (DateString = LastDateString)) OR
           (LastTimeString = '') THEN
               BEGIN
               IF NOT BandsThisMinute [Band] THEN
                   BEGIN
                   BandsThisMinute [Band] := True;
                   Inc (LogHourTotals^ [HourIndex].Minutes [Band, Both]);
                   END;

               IF LastTimeString = '' THEN
                   Inc (LogHourTotals^ [HourIndex].Minutes [All, Both]);

               LastMode := Mode;
               LastBand := Band;
               LastTimeString := TimeString;
               LastDateString := DateString;
               LastHourIndex  := HourIndex;
               Exit;
               END;

        { We are processing a new minute, so make sure BandsThisMinute
          is all cleared out. }

        FOR TempBand := Band160 TO NoBand DO
            BandsThisMinute [TempBand] := False;

        { See how long it has been since the last log entry }

        TempTimeString := LastTimeString;
        TempDateString := LastDateString;
        NumberMinutesSinceLastEntry := 0;

        WHILE (TempTimeString <> TimeString) OR (TempDateString <> DateString) DO
            BEGIN
            IncrementMinute (TempDateString, TempTimeString);
            Inc (NumberMinutesSinceLastEntry);

            { See if this count has overflowed }

            IF NumberMinutesSinceLastEntry = 0 THEN
                BEGIN
                ProcessLogEntryForTimeOnBandInformation := False;
                Exit;
                END;
            END;

        { Check number of minutes to see if this should be counted as a
          break or not (15 or more minutes) }

        IF NumberMinutesSinceLastEntry < 15 THEN
            BEGIN

            { Fill in the previous minutes with the last band }

            IF LastHourIndex <> HourIndex THEN
                WHILE NOT (PostcedingString (LastTimeString, ':') = '59') DO
                    BEGIN
                    IncrementMinute (LastDateString, LastTimeString);

                { If not at the current minute, increment minute totals }

                    Inc (LogHourTotals^ [LastHourIndex].Minutes [LastBand, Both]);
                    Inc (LogHourTotals^ [LastHourIndex].Minutes [All, Both]);
                    END;

            { We are now pointing to the last minute processed }

            { Now work on this hour - fill up any previously unassigned minutes
              with the LastBand and LastMode }

            WHILE (LastDateString <> DateString) OR (LastTimeString <> TimeString) DO
                BEGIN
                IncrementMinute (LastDateString, LastTimeString);

                { If not at the current minute - increment totals }

                IF (LastDateString <> DateString) OR (LastTimeString <> TimeString) THEN
                    BEGIN
                    Inc (LogHourTotals^ [HourIndex].Minutes [LastBand, Both]);
                    Inc (LogHourTotals^ [HourIndex].Minutes [All, Both]);
                    END;
                END;

            { We should now be pointing to the current minute }
            END;

        IF NOT BandsThisMinute [Band] THEN
            BEGIN
            Inc (LogHourTotals^ [HourIndex].Minutes [Band, Both]);
            Inc (LogHourTotals^ [HourIndex].Minutes [All, Both]);
            BandsThisMinute [Band] := True;
            END;

        LastBand := Band;
        LastMode := Mode;
        LastTimeString := TimeString;
        LastDateString := DateString;
        LastHourIndex := HourIndex;
        END;
    END;



PROCEDURE FinishOutLastHour;

{ This routine will look at the last QSO processed by the PLeftOnBiState
  machine to see if we should fill in the last few minutes of the contest
  with the band of the last QSO.  This is done if the interval is less
  than 10 minutes. }

VAR LastMinuteString: Str20;
    Minute, LastMinute: INTEGER;

    BEGIN
    WITH PLeftOnBiState DO
        BEGIN

        LastMinuteString := PostcedingString (LastTimeString, ':');

        Val (LastMinuteString, LastMinute);

        IF (LastMinute >= 50) AND (LastMinute < 59) THEN
            FOR Minute := LastMinute + 1 TO 59 DO
                BEGIN
                Inc (LogHourTotals^ [LastHourIndex].Minutes [LastBand, Both]);
                Inc (LogHourTotals^ [LastHourIndex].Minutes [All, Both]);
                END;

        END;
    END;




PROCEDURE CheckTimeOnByBand (LastHour: INTEGER);

{ This procedure will look through the LogHourTotals to make sure that the
  individual band on times add up to the hourly on time (as indicated in
  the All, Both cell).  If the total of the band times exceeds that of
  the All, Both number, the band will the longest on time during the
  hour will have its time adjusted to make the total come out correctly. }

VAR Correction, HourTotal, BiggestBandTotal, HourIndex: INTEGER;
    Band, BiggestBand: BandType;


    BEGIN
    FOR HourIndex := 0 TO LastHour DO
        BEGIN
        HourTotal := 0;
        BiggestBand := NoBand;
        BiggestBandTotal := 0;

        FOR Band := Band160 TO BandLight DO
            BEGIN
            HourTotal := HourTotal + LogHourTotals^ [HourIndex].Minutes [Band, Both];

            IF LogHourTotals^ [HourIndex].Minutes [Band, Both] > BiggestBandTotal THEN
                BEGIN
                BiggestBandTotal := LogHourTotals^ [HourIndex].Minutes [Band, Both];
                BiggestBand := Band;
                END;
            END;

        Correction := LogHourTotals^ [HourIndex].Minutes [All, Both] - HourTotal;

        IF Correction <> 0 THEN
            LogHourTotals^ [HourIndex].Minutes [BiggestBand, Both] :=
            LogHourTotals^ [HourIndex].Minutes [BiggestBand, Both] + Correction;

        END;
    END;



FUNCTION GenerateAllTotalsPerHour: BOOLEAN;

{ This routine will fill up the global variable LogHourTotals with data. }

VAR FileRead: TEXT;
    TempString, FileString: Str80;
    Band: BandType;
    Mode: ModeType;
    LineNumber, CurrentHourIndex, HourIndex, QSOPoints: INTEGER;
    TotalLogQSOs: LONGINT;
    HourString, MultString, LastHourProcessed: Str20;

    BEGIN
    FOR HourIndex := 0 TO 49 DO
        BEGIN

        LogHourTotals^ [HourIndex].HourName := '';

        FOR Band := Band160 TO NoBand DO
            FOR Mode := CW TO NoMode DO
                WITH LogHourTotals^ [HourIndex] DO
                    BEGIN
                    QSOs      [Band, Mode] := 0;
                    QSOPoints [Band, Mode] := 0;
                    Mults     [Band, Mode] := 0;
                    Minutes   [Band, Mode] := 0;
                    END;
        END;

    { Initialize the time on band status }

    WITH PLeftOnBiState DO
        BEGIN
        LastBand       := NoBand;
        LastMode       := NoMode;
        LastDateString := '';
        LastTimeString := '';  ;
        LastHourIndex  := -1;
        DoingBreak     := False;

        FOR Band := Band160 TO NoBand DO
            PLeftOnBiState.BandsThisMinute [Band] := False;
        END;

    GenerateAllTotalsPerHour := False;   { Default return value }

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError (LogFileName + ' file not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    { Initialize variables }

    CurrentHourIndex  := 0;
    LastHourProcessed := '';
    NumberLogHours    := 0;
    TotalLogQSOs      := 0;
    LineNumber        := 0;

    WriteLn ('Searching log file for hourly totals...');

    { Look through the log file and process each QSO }

    REPEAT
        REPEAT
            ReadLn (FileRead, FileString);
            Band := GetLogEntryBand (FileString);
            Mode := GetLogEntryMode (FileString);
            Inc (LineNumber);

        UNTIL ((Band <> NoBand) AND (Mode <> NoMode)) OR EOF (FileRead);

        IF (Band <> NoBand) AND (Mode <> NoMode) THEN
            BEGIN
            PinWheel;
            GetLogEntryCall (FileString);
            HourString   := PrecedingString  (GetLogEntryTimeString (FileString), ':');
//            MinuteString := PostcedingString (GetLogEntryTimeString (FileString), ':');

//            Val (MinuteString, Minute, xResult);

            IF LastHourProcessed <> HourString THEN
                BEGIN
                IF CurrentHourIndex = 49 THEN
                    BEGIN
                    ReportError ('More than 50 different hours in this log!!');
                    Halt;
                    END;

                IF LastHourProcessed <> '' THEN Inc (CurrentHourIndex);

                LogHourTotals^ [CurrentHourIndex].HourName := HourString;
                LastHourProcessed := HourString
                END;

            IF NOT ProcessLogEntryForTimeOnBandInformation (Band,
                                                            Mode,
                                                            GetLogEntryTimeString (FileString),
                                                            GetLogEntryDateString (FileSTring),
                                                            CurrentHourIndex) THEN

                BEGIN
                GoToXY (1, WhereY);
                ClrEol;
                Str (LineNumber, TempString);
                ReportError ('Error found at line ' + TempString + '.  Check time and date format.');
                WriteLn (FileString);
                Close (FileRead);
                Exit;
                END;

            IF NOT (StringHas (FileString, '*DUPE*') OR StringHas (FileString, '*ZERO*')) THEN
                BEGIN
                ExpandTabs (FileString);
                Inc (TotalLogQSOs);

                Inc (LogHourTotals^ [CurrentHourIndex].QSOs [Band, Mode]);
                Inc (LogHourTotals^ [CurrentHourIndex].QSOs [All,  Mode]);
                Inc (LogHourTotals^ [CurrentHourIndex].QSOs [Band, Both]);
                Inc (LogHourTotals^ [CurrentHourIndex].QSOs [All,  Both]);

                QSOPoints := GetLogEntryQSOPoints (FileString);

                LogHourTotals^ [CurrentHourIndex].QSOPoints [Band, Mode] :=
                LogHourTotals^ [CurrentHourIndex].QSOPoints [Band, Mode] + QSOPoints;

                LogHourTotals^ [CurrentHourIndex].QSOPoints [All, Mode] :=
                LogHourTotals^ [CurrentHourIndex].QSOPoints [All, Mode] + QSOPoints;

                LogHourTotals^ [CurrentHourIndex].QSOPoints [Band, Both] :=
                LogHourTotals^ [CurrentHourIndex].QSOPoints [Band, Both] + QSOPoints;

                LogHourTotals^ [CurrentHourIndex].QSOPoints [All, Both] :=
                LogHourTotals^ [CurrentHourIndex].QSOPoints [All, Both] + QSOPoints;

                MultString := GetLogEntryMultString (FileString);

                WHILE MultString <> '' DO
                    BEGIN
                    Inc (LogHourTotals^ [CurrentHourIndex].Mults [Band, Mode]);
                    Inc (LogHourTotals^ [CurrentHourIndex].Mults [All,  Mode]);
                    Inc (LogHourTotals^ [CurrentHourIndex].Mults [Band, Both]);
                    Inc (LogHourTotals^ [CurrentHourIndex].Mults [All,  Both]);
                    RemoveFirstString (MultString);
                    END;
                END;
            END;

    UNTIL Eof (FileRead);

    GoToXY (1, WhereY);
    ClrEol;

    FinishOutLastHour;
    CheckTimeOnByBand (CurrentHourIndex);

    Close (FileRead);
    GenerateAllTotalsPerHour := True;
    GoToXY (1, WhereY);
    ClrEol;
    END;



PROCEDURE PrintHourTotals;

VAR Destination: CHAR;
    FileWrite: TEXT;
    Band: BandType;
    FileName: Str80;
    TotalMults, LastHourPrinted, HourIndex, ThisHour: INTEGER;
    TotalQSOPoints, TotalContacts: LONGINT;
    Score: REAL;
    Day1QSOTotals, Day1MultTotals, Day2QSOTotals, Day2MultTotals: ARRAY [Band160..Band10] OF LONGINT;
    DoingDay1: BOOLEAN;
    TotalDay1QSOs, TotalDay1Mults, TotalDay2QSOs, TotalDay2Mults: LONGINT;

    BEGIN
    LastHourPrinted := 0;//KS initialized ??
    REPEAT
    Destination := UpCase (GetKey ('Output to (F)ile, (P)rinter or (S)creen? : '));
        CASE Destination OF
            'F': BEGIN
                 WriteLn;
                 FileName := UpperCase (GetResponse ('Enter filename to save multiplier report to : '));

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

    OpenFileForWrite (FileWrite, FileName);

    IF Header = '' THEN
        Header := GetResponse ('Enter contest name and callsign : ');

    WriteLnVARCenter (FileWrite, Header);
    WriteLn (FileWrite);
    WriteLn (FileWrite, 'HR    160     80       40       20       15       10    HR TOT CUM TOTAL  SCORE');
    WriteLn (FileWrite, '--   -----  ------- -------- -------- -------- -------- ------ ---------  -----');

    HourIndex := 0;

    TotalMults     := 0;
    TotalContacts  := 0;
    TotalQSOPoints := 0;

    DoingDay1 := True;

    FOR Band := Band160 TO Band10 DO
        BEGIN
        Day1QSOTotals  [Band] := 0;
        Day1MultTotals [Band] := 0;
        Day2QSOTotals  [Band] := 0;
        Day2MultTotals [Band] := 0;
        END;

    WHILE LogHourTotals^ [HourIndex].HourName <> '' DO
        BEGIN
        Inc (LastHourPrinted);

        IF (HourIndex > 0) AND (LastHourPrinted > 23) THEN
            BEGIN
            DoingDay1 := False;
            LastHourPrinted := 0;
            END;

        Val (LogHourTotals^ [HourIndex].HourName, ThisHour);

        IF HourIndex = 0 THEN LastHourPrinted := ThisHour;

        WHILE ThisHour <> LastHourPrinted DO
            BEGIN
            Write (FileWrite, LastHourPrinted:2);

            FOR Band := Band160 TO Band80 DO
                Write (FileWrite, '    --- ');

            FOR Band := Band40 TO Band10 DO
                Write (FileWrite, '     --- ');

            Write (FileWrite, '    --- ');

            Write (FileWrite, TotalContacts: 6, '/', TotalMults);

            IF TotalMults < 10  THEN Write (FileWrite, ' ');
            IF TotalMults < 100 THEN Write (FileWrite, ' ');

            Score := TotalQSOPoints * TotalMults / 1000000;

            WriteLn (FileWrite, Score:6:2, 'M');

            Inc (LastHourPrinted);
            IF LastHourPrinted > 23 THEN LastHourPrinted := 0;
            END;

        TotalContacts  := TotalContacts  + LogHourTotals^ [HourIndex].QSOs      [All, Both];
        TotalMults     := TotalMults     + LogHourTotals^ [HourIndex].Mults     [All, Both];
        TotalQSOPoints := TotalQSOPoints + LogHourTotals^ [HourIndex].QSOPoints [All, Both];

        IF DoingDay1 THEN
            BEGIN
            Day1QSOTotals [Band160] := Day1QSOTotals [Band160] + LogHourTotals^ [HourIndex].QSOs [Band160, Both];
            Day1QSOTotals [Band80]  := Day1QSOTotals [Band80]  + LogHourTotals^ [HourIndex].QSOs [Band80,  Both];
            Day1QSOTotals [Band40]  := Day1QSOTotals [Band40]  + LogHourTotals^ [HourIndex].QSOs [Band40,  Both];
            Day1QSOTotals [Band20]  := Day1QSOTotals [Band20]  + LogHourTotals^ [HourIndex].QSOs [Band20,  Both];
            Day1QSOTotals [Band15]  := Day1QSOTotals [Band15]  + LogHourTotals^ [HourIndex].QSOs [Band15,  Both];
            Day1QSOTotals [Band10]  := Day1QSOTotals [Band10]  + LogHourTotals^ [HourIndex].QSOs [Band10,  Both];

            Day1MultTotals [Band160] := Day1MultTotals [Band160] + LogHourTotals^ [HourIndex].Mults [Band160, Both];
            Day1MultTotals [Band80]  := Day1MultTotals [Band80]  + LogHourTotals^ [HourIndex].Mults [Band80,  Both];
            Day1MultTotals [Band40]  := Day1MultTotals [Band40]  + LogHourTotals^ [HourIndex].Mults [Band40,  Both];
            Day1MultTotals [Band20]  := Day1MultTotals [Band20]  + LogHourTotals^ [HourIndex].Mults [Band20,  Both];
            Day1MultTotals [Band15]  := Day1MultTotals [Band15]  + LogHourTotals^ [HourIndex].Mults [Band15,  Both];
            Day1MultTotals [Band10]  := Day1MultTotals [Band10]  + LogHourTotals^ [HourIndex].Mults [Band10,  Both];
            END
        ELSE
            BEGIN
            Day2QSOTotals [Band160] := Day2QSOTotals [Band160] + LogHourTotals^ [HourIndex].QSOs [Band160, Both];
            Day2QSOTotals [Band80]  := Day2QSOTotals [Band80]  + LogHourTotals^ [HourIndex].QSOs [Band80,  Both];
            Day2QSOTotals [Band40]  := Day2QSOTotals [Band40]  + LogHourTotals^ [HourIndex].QSOs [Band40,  Both];
            Day2QSOTotals [Band20]  := Day2QSOTotals [Band20]  + LogHourTotals^ [HourIndex].QSOs [Band20,  Both];
            Day2QSOTotals [Band15]  := Day2QSOTotals [Band15]  + LogHourTotals^ [HourIndex].QSOs [Band15,  Both];
            Day2QSOTotals [Band10]  := Day2QSOTotals [Band10]  + LogHourTotals^ [HourIndex].QSOs [Band10,  Both];

            Day2MultTotals [Band160] := Day2MultTotals [Band160] + LogHourTotals^ [HourIndex].Mults [Band160, Both];
            Day2MultTotals [Band80]  := Day2MultTotals [Band80]  + LogHourTotals^ [HourIndex].Mults [Band80,  Both];
            Day2MultTotals [Band40]  := Day2MultTotals [Band40]  + LogHourTotals^ [HourIndex].Mults [Band40,  Both];
            Day2MultTotals [Band20]  := Day2MultTotals [Band20]  + LogHourTotals^ [HourIndex].Mults [Band20,  Both];
            Day2MultTotals [Band15]  := Day2MultTotals [Band15]  + LogHourTotals^ [HourIndex].Mults [Band15,  Both];
            Day2MultTotals [Band10]  := Day2MultTotals [Band10]  + LogHourTotals^ [HourIndex].Mults [Band10,  Both];
            END;

        WITH LogHourTotals^ [HourIndex] DO
            BEGIN
            Write (FileWrite, ThisHour:2);

            FOR Band := Band160 TO Band10 DO
                BEGIN
                IF Band >= Band40 THEN
                    BEGIN
                    IF QSOs [Band, Both] > 0 THEN
                        BEGIN
                        Write (FileWrite, QSOs [Band, Both]:6, '/', Mults [Band, Both]);
                        IF Mults [Band, Both] < 10 THEN Write (FileWrite, ' ');
                        END
                    ELSE
                        Write (FileWrite, '     --- ');
                    END
                ELSE
                    BEGIN
                    IF QSOs [Band, Both] > 0 THEN
                        BEGIN
                        Write (FileWrite, QSOs [Band, Both]:5, '/', Mults [Band, Both]);
                        IF Mults [Band, Both] < 10 THEN Write (FileWrite, ' ');
                        END
                    ELSE
                        Write (FileWrite, '    --- ');
                    END;
                END;

            Write (FileWrite, QSOs [All, Both]:5, '/', Mults [All, Both]);

            IF Mults [All, Both] < 10 THEN Write (FileWrite, ' ');

            Write (FileWrite, TotalContacts: 6, '/', TotalMults);

            IF TotalMults < 10  THEN Write (FileWrite, ' ');
            IF TotalMults < 100 THEN Write (FileWrite, ' ');

            Score := TotalQSOPoints * TotalMults / 1000000;

            WriteLn (FileWrite, Score:6:2, 'M');
            END;

        Val (LogHourTotals^ [HourIndex].HourName, LastHourPrinted);
        Inc (HourIndex);
        END;


    TotalDay1QSOs  := Day1QSOTotals [Band160] + Day1QSOTotals [Band80] +
                      Day1QSOTotals [Band40]  + Day1QSOTotals [Band20] +
                      Day1QSOTotals [Band15]  + Day1QSOTotals [Band10];


    TotalDay1Mults := Day1MultTotals [Band160] + Day1MultTotals [Band80] +
                      Day1MultTotals [Band40]  + Day1MultTotals [Band20] +
                      Day1MultTotals [Band15]  + Day1MultTotals [Band10];

    TotalDay2QSOs  := Day2QSOTotals [Band160] + Day2QSOTotals [Band80] +
                      Day2QSOTotals [Band40]  + Day2QSOTotals [Band20] +
                      Day2QSOTotals [Band15]  + Day2QSOTotals [Band10];


    TotalDay2Mults := Day2MultTotals [Band160] + Day2MultTotals [Band80] +
                      Day2MultTotals [Band40]  + Day2MultTotals [Band20] +
                      Day2MultTotals [Band15]  + Day2MultTotals [Band10];

    Write (FileWrite, 'D1');

    FOR Band := Band160 TO Band10 DO
        IF Band >= Band40 THEN
            BEGIN
            Write (FileWrite, Day1QSOTotals [Band]:5, '/', Day1MultTotals [Band]);
            IF Day1MultTotals [Band] < 10  THEN Write (FileWrite, ' ');
            IF Day1MultTotals [Band] < 100 THEN Write (FileWrite, ' ');
            END
        ELSE
            BEGIN
            Write (FileWrite, Day1QSOTotals [Band]:4, '/', Day1MultTotals [Band]);
            IF Day1MultTotals [Band] < 10  THEN Write (FileWrite, ' ');
            IF Day1MultTotals [Band] < 100 THEN Write (FileWrite, ' ');
            END;

    WriteLn (FileWrite, '        ', TotalDay1QSOs:6, '/', TotalDay1Mults);

    IF NOT DoingDay1 THEN
        BEGIN
        Write (FileWrite, 'D2');

        FOR Band := Band160 TO Band10 DO
        IF Band >= Band40 THEN
            BEGIN
            Write (FileWrite, Day2QSOTotals [Band]:5, '/', Day2MultTotals [Band]);
            IF Day2MultTotals [Band] < 10  THEN Write (FileWrite, ' ');
            IF Day2MultTotals [Band] < 100 THEN Write (FileWrite, ' ');
            END
        ELSE
            BEGIN
            Write (FileWrite, Day2QSOTotals [Band]:4, '/', Day2MultTotals [Band]);
            IF Day2MultTotals [Band] < 10  THEN Write (FileWrite, ' ');
            IF Day2MultTotals [Band] < 100 THEN Write (FileWrite, ' ');
            END;

        WriteLn (FileWrite, '        ', TotalDay2QSOs:6, '/', TotalDay2Mults);
        END;

    Write (FileWrite, 'TO');

    FOR Band := Band160 TO Band10 DO
        IF Band >= Band40 THEN
            BEGIN
            Write (FileWrite, (Day1QSOTotals [Band] + Day2QSOTotals [Band]):5, '/',
                               Day1MultTotals [Band] + Day2MultTotals [Band]);

            IF Day1MultTotals [Band] + Day2MultTotals [Band] < 10  THEN Write (FileWrite, ' ');
            IF Day1MultTotals [Band] + Day2MultTotals [Band] < 100 THEN Write (FileWrite, ' ');
            END
        ELSE
            BEGIN
            Write (FileWrite, (Day1QSOTotals [Band] + Day2QSOTotals [Band]):4, '/',
                               Day1MultTotals [Band] + Day2MultTotals [Band]);
            IF Day1MultTotals [Band] + Day2MultTotals [Band] < 10  THEN Write (FileWrite, ' ');
            IF Day1MultTotals [Band] + Day2MultTotals [Band] < 100 THEN Write (FileWrite, ' ');
            END;

    WriteLn (FileWrite, '        ', TotalDay1QSOs + TotalDay2QSOs:6, '/', TotalDay1Mults + TotalDay2Mults);

    Write (FileWrite, '');
    Close (FileWrite);
    END;



PROCEDURE PrintRateByHourTotals;

VAR Destination: CHAR;
    FileWrite: TEXT;
    Band: BandType;
    TempString, RateString, FileName: Str80;
    Rate, TotalMinutes, LastHourPrinted, HourIndex, ThisHour: INTEGER;
    TotalQSOPoints, TotalContacts: LONGINT;
    Day1QSOTotals, Day1MinTotals, Day2QSOTotals, Day2MinTotals: ARRAY [Band160..Band10] OF LONGINT;
    DoingDay1: BOOLEAN;
    TotalDay1QSOs, TotalDay1Mins, TotalDay2QSOs, TotalDay2Mins: LONGINT;

    BEGIN
    LastHourPrinted := 0; //KS initialized?
    REPEAT
    Destination := UpCase (GetKey ('Output to (F)ile, (P)rinter or (S)creen? : '));
        CASE Destination OF
            'F': BEGIN
                 WriteLn;
                 FileName := UpperCase (GetResponse ('Enter filename to save multiplier report to : '));

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

    OpenFileForWrite (FileWrite, FileName);

    IF Header = '' THEN
        Header := GetResponse ('Enter contest name and callsign : ');

    WriteLnVARCenter (FileWrite, 'Minutes / Rate Summary  ' + Header);
    WriteLn (FileWrite);

    WriteLn (FileWrite, 'HR     160      80       40       20       15       10    HR TOT   CUM TOTAL');
    WriteLn (FileWrite, '--   ------   ------   ------   ------   ------   ------  ------   ---------');

    HourIndex := 0;

    TotalMinutes := 0;
    TotalContacts  := 0;
    TotalQSOPoints := 0;

    DoingDay1 := True;

    FOR Band := Band160 TO Band10 DO
        BEGIN
        Day1QSOTotals  [Band] := 0;
        Day1MinTotals [Band] := 0;
        Day2QSOTotals  [Band] := 0;
        Day2MinTotals [Band] := 0;
        END;

    WHILE LogHourTotals^ [HourIndex].HourName <> '' DO
        BEGIN
        Inc (LastHourPrinted);

        IF (HourIndex > 0) AND (LastHourPrinted > 23) THEN
            BEGIN
            DoingDay1 := False;
            LastHourPrinted := 0;
            END;

        Val (LogHourTotals^ [HourIndex].HourName, ThisHour);

        IF HourIndex = 0 THEN LastHourPrinted := ThisHour;

        WHILE ThisHour <> LastHourPrinted DO
            BEGIN
            Write (FileWrite, LastHourPrinted:2, '  ');

            FOR Band := Band160 TO Band10 DO
                Write (FileWrite, ' ----   ');

            Write (FileWrite, '  ----   ');

            WriteLn (FileWrite, TotalMinutes:4, '/', Round (TotalContacts * (60 / TotalMinutes)));

            Inc (LastHourPrinted);
            IF LastHourPrinted > 23 THEN LastHourPrinted := 0;
            END;

        TotalContacts  := TotalContacts  + LogHourTotals^ [HourIndex].QSOs      [All, Both];
        TotalMinutes   := TotalMinutes   + LogHourTotals^ [HourIndex].Minutes   [All, Both];
        TotalQSOPoints := TotalQSOPoints + LogHourTotals^ [HourIndex].QSOPoints [All, Both];

        IF DoingDay1 THEN
            BEGIN
            Day1QSOTotals [Band160] := Day1QSOTotals [Band160] + LogHourTotals^ [HourIndex].QSOs [Band160, Both];
            Day1QSOTotals [Band80]  := Day1QSOTotals [Band80]  + LogHourTotals^ [HourIndex].QSOs [Band80,  Both];
            Day1QSOTotals [Band40]  := Day1QSOTotals [Band40]  + LogHourTotals^ [HourIndex].QSOs [Band40,  Both];
            Day1QSOTotals [Band20]  := Day1QSOTotals [Band20]  + LogHourTotals^ [HourIndex].QSOs [Band20,  Both];
            Day1QSOTotals [Band15]  := Day1QSOTotals [Band15]  + LogHourTotals^ [HourIndex].QSOs [Band15,  Both];
            Day1QSOTotals [Band10]  := Day1QSOTotals [Band10]  + LogHourTotals^ [HourIndex].QSOs [Band10,  Both];

            Day1MinTotals [Band160] := Day1MinTotals [Band160] + LogHourTotals^ [HourIndex].Minutes [Band160, Both];
            Day1MinTotals [Band80]  := Day1MinTotals [Band80]  + LogHourTotals^ [HourIndex].Minutes [Band80,  Both];
            Day1MinTotals [Band40]  := Day1MinTotals [Band40]  + LogHourTotals^ [HourIndex].Minutes [Band40,  Both];
            Day1MinTotals [Band20]  := Day1MinTotals [Band20]  + LogHourTotals^ [HourIndex].Minutes [Band20,  Both];
            Day1MinTotals [Band15]  := Day1MinTotals [Band15]  + LogHourTotals^ [HourIndex].Minutes [Band15,  Both];
            Day1MinTotals [Band10]  := Day1MinTotals [Band10]  + LogHourTotals^ [HourIndex].Minutes [Band10,  Both];
            END
        ELSE
            BEGIN
            Day2QSOTotals [Band160] := Day2QSOTotals [Band160] + LogHourTotals^ [HourIndex].QSOs [Band160, Both];
            Day2QSOTotals [Band80]  := Day2QSOTotals [Band80]  + LogHourTotals^ [HourIndex].QSOs [Band80,  Both];
            Day2QSOTotals [Band40]  := Day2QSOTotals [Band40]  + LogHourTotals^ [HourIndex].QSOs [Band40,  Both];
            Day2QSOTotals [Band20]  := Day2QSOTotals [Band20]  + LogHourTotals^ [HourIndex].QSOs [Band20,  Both];
            Day2QSOTotals [Band15]  := Day2QSOTotals [Band15]  + LogHourTotals^ [HourIndex].QSOs [Band15,  Both];
            Day2QSOTotals [Band10]  := Day2QSOTotals [Band10]  + LogHourTotals^ [HourIndex].QSOs [Band10,  Both];

            Day2MinTotals [Band160] := Day2MinTotals [Band160] + LogHourTotals^ [HourIndex].Minutes [Band160, Both];
            Day2MinTotals [Band80]  := Day2MinTotals [Band80]  + LogHourTotals^ [HourIndex].Minutes [Band80,  Both];
            Day2MinTotals [Band40]  := Day2MinTotals [Band40]  + LogHourTotals^ [HourIndex].Minutes [Band40,  Both];
            Day2MinTotals [Band20]  := Day2MinTotals [Band20]  + LogHourTotals^ [HourIndex].Minutes [Band20,  Both];
            Day2MinTotals [Band15]  := Day2MinTotals [Band15]  + LogHourTotals^ [HourIndex].Minutes [Band15,  Both];
            Day2MinTotals [Band10]  := Day2MinTotals [Band10]  + LogHourTotals^ [HourIndex].Minutes [Band10,  Both];
            END;

        WITH LogHourTotals^ [HourIndex] DO
            BEGIN
            Write (FileWrite, ThisHour:2, '  ');

            FOR Band := Band160 TO Band10 DO
                BEGIN
                IF QSOs [Band, Both] > 0 THEN
                    BEGIN
                    Str (Minutes [Band, Both]:3, TempString);
                    TempString := TempString + '/';

                    Rate := Round (QSOs [Band, Both] * (60 / Minutes [Band, Both]));
                    Str (Rate, RateString);

                    TempString := TempString + RateString;
                    WHILE Length (TempString) < 9 DO TempString := TempString + ' ';
                    Write (FileWrite, TempString);
                    END
                ELSE
                    Write (FileWrite, '  ----   ');
                END;

            Str (Minutes [All, Both]:2, TempString);
            TempString := TempString + '/';

            Rate := Round (QSOs [All, Both] * (60 / Minutes [All, Both]));
            Str (Rate, RateString);

            TempString := TempString + RateString;
            WHILE Length (TempString) < 9 DO TempString := TempString + ' ';
            Write (FileWrite, TempString);

            WriteLn (FileWrite, TotalMinutes:4, '/', Round (TotalContacts * (60 / TotalMinutes)));
            END;

        Val (LogHourTotals^ [HourIndex].HourName, LastHourPrinted);
        Inc (HourIndex);
        END;


    TotalDay1QSOs  := Day1QSOTotals [Band160] + Day1QSOTotals [Band80] +
                      Day1QSOTotals [Band40]  + Day1QSOTotals [Band20] +
                      Day1QSOTotals [Band15]  + Day1QSOTotals [Band10];


    TotalDay1Mins := Day1MinTotals [Band160] + Day1MinTotals [Band80] +
                     Day1MinTotals [Band40]  + Day1MinTotals [Band20] +
                     Day1MinTotals [Band15]  + Day1MinTotals [Band10];

    TotalDay2QSOs  := Day2QSOTotals [Band160] + Day2QSOTotals [Band80] +
                      Day2QSOTotals [Band40]  + Day2QSOTotals [Band20] +
                      Day2QSOTotals [Band15]  + Day2QSOTotals [Band10];


    TotalDay2Mins := Day2MinTotals [Band160] + Day2MinTotals [Band80] +
                     Day2MinTotals [Band40]  + Day2MinTotals [Band20] +
                     Day2MinTotals [Band15]  + Day2MinTotals [Band10];

    Write (FileWrite, 'D1 ');

    FOR Band := Band160 TO Band10 DO
        BEGIN
        Write (FileWrite, (Day1MinTotals [Band] / 60):4:1, '/');

        Rate := Round (Day1QSOTotals [Band] * (60 / Day1MinTotals [Band]));

        Str (Rate, RateString);
        WHILE Length (RateString) < 4 DO RateString := RateString + ' ';
        Write (FileWrite, RateString);
        END;

    Write (FileWrite, '         ', (TotalDay1Mins / 60):5:1, '/');

    Rate := Round (TotalDay1QSOs * (60 / TotalDay1Mins));
    WriteLn (FileWrite, Rate);

    IF NOT DoingDay1 THEN
        BEGIN
        Write (FileWrite, 'D2 ');

        FOR Band := Band160 TO Band10 DO
            BEGIN
            Write (FileWrite, (Day2MinTotals [Band] / 60):4:1, '/');

            Rate := Round (Day2QSOTotals [Band] * (60 / Day2MinTotals [Band]));

            Str (Rate, RateString);
            WHILE Length (RateString) < 4 DO RateString := RateString + ' ';
            Write (FileWrite, RateString);
            END;

        Write (FileWrite, '         ', (TotalDay2Mins / 60):5:1, '/');

        Rate := Round (TotalDay2QSOs * (60 / TotalDay2Mins));
        WriteLn (FileWrite, Rate);
        END;

    Write (FileWrite, 'TO ');

    FOR Band := Band160 TO Band10 DO
        BEGIN
        Write (FileWrite, ((Day1MinTotals [Band] + Day2MinTotals [Band]) / 60):4:1, '/');

        Rate := Round ((Day1QSOTotals [Band] + Day2QSOTotals [Band]) *
                (60 / (Day1MinTotals [Band] + Day2MinTotals [Band])));

        Str (Rate, RateString);
        WHILE Length (RateString) < 4 DO RateString := RateString + ' ';
        Write (FileWrite, RateString);
        END;

    Write (FileWrite, '         ', ((TotalDay1Mins + TotalDay2Mins) / 60):5:1, '/');
    Rate := Round ((TotalDay1QSOs + TotalDay2QSOs) * (60 / (TotalDay1Mins + TotalDay2Mins)));
    WriteLn (FileWrite, Rate);
    Write (FileWrite, '');
    Close (FileWrite);
    END;



PROCEDURE MultiplierReportMenu;

VAR Key: CHAR;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('MULTIPLIER REPORT MENU');
    TextColor (Cyan);
    WriteLn;

    Header := '';

    REPEAT
        ClrScr;
        TextColor (Yellow);
        WriteLnCenter ('OH2MM CQ WW REPORT MENU');
        TextColor (Cyan);

        WriteLn;
        WriteLn ('These reports were originally inspired by OH2MM for the CQ WW contest, ');
        WriteLn ('however many of them are also useful for other contests.');
        WriteLn;
        WriteLn (' C - Multiplier check off sheet');
        WriteLn (' F - First callsigns worked by country by band.');
        WriteLn (' G - First callsigns worked by zone by band.');
        WriteLn (' H - Rate sheet with multipliers and running score.');
        WriteLn (' R - Rate by band by hour report.');
        WriteLn (' T - Total QSOs worked by country by band.');
        WriteLn (' W - WRTC 2018 Multiplier check off sheet');
        WriteLn (' Z - Total Zones worked by zone by band.');
        WriteLn;
        Write   (' Enter desired report (ESCAPE to exit this menu) : ');

        REPEAT UNTIL KeyPressed;
        Key := UpCase (ReadKey);

        CASE Key OF

            'C': MultiplierCheckOffSheet;

            'F': BEGIN
                 ClearScreenAndTitle ('SHOW FIRST COUNTRY CALLSIGN WORKED BY BAND');
                 WriteLn ('This report will show the first callsign worked in each country by band');
                 WriteLn;

                 New (CountryMultTotals);

                 REPEAT
                     Key := UpCase (GetKey ('Use (C)Q or (A)RRL countries? (C/A) : '));
                     IF Key = EscapeKey THEN Exit;
                 UNTIL (Key = 'C') OR (Key = 'A');
                 WriteLn;

                 IF Key = 'C' THEN
                     CountryTable.CountryMode := CQCountryMode
                 ELSE
                     CountryTable.CountryMode := ARRLCountryMode;

                 IF GenerateCountryMultiplierTotals THEN
                     PrintFirstCountryMultiplierCallsigns;

                 Dispose (CountryMultTotals);
                 WaitForKeyPressed;
                 END;

            'G': BEGIN
                 ClrScr;
                 TextColor (Yellow);
                 WriteLnCenter ('SHOW FIRST ZONE CALLSIGN WORKED BY BAND');
                 TextColor (Cyan);
                 WriteLn;
                 WriteLn ('This report will show the first callsign worked in each zone by band');
                 WriteLn;

                 New (CountryMultTotals);

                 IF GenerateZoneMultiplierTotals THEN
                     PrintFirstZoneMultiplierCallsigns;

                 Dispose (CountryMultTotals);
                 WaitForKeyPressed;
                 END;

            'H': BEGIN
                 ClrScr;
                 TextColor (Yellow);
                 WriteLnCenter ('GENERATE REPORT OF SCORE BY HOUR');
                 TextColor (Cyan);
                 WriteLn;
                 WriteLn ('This report will show the score at the end of each hour.');
                 WriteLn;

                 New (LogHourTotals);

                 IF GenerateAllTotalsPerHour THEN PrintHourTotals;

                 Dispose (LogHourTotals);
                 WaitForKeyPressed;
                 END;

            'R': BEGIN
                 ClrScr;
                 TextColor (Yellow);
                 WriteLnCenter ('GENERATE REPORT OF RATE BY BAND BY HOUR');
                 TextColor (Cyan);
                 WriteLn;
                 WriteLn ('This report will show the score at the end of each hour.');
                 WriteLn;

                 New (LogHourTotals);

                 IF GenerateAllTotalsPerHour THEN
                      PrintRateByHourTotals;

                 Dispose (LogHourTotals);
                 WaitForKeyPressed;
                 END;

            'T': BEGIN
                 ClrScr;
                 TextColor (Yellow);
                 WriteLnCenter ('SHOW TOTAL QSOs BY COUNTRY BY BAND');
                 TextColor (Cyan);
                 WriteLn;
                 WriteLn ('This report will show the number of QSOs made in each country by band');
                 WriteLn;

                 New (CountryMultTotals);

                 IF GenerateCountryMultiplierTotals THEN
                     PrintQSOsByCountry;

                 Dispose (CountryMultTotals);
                 WaitForKeyPressed;
                 END;

            'W': WRTC2018MultiplierCheckOffSheet;

            'Z': BEGIN
                 ClrScr;
                 TextColor (Yellow);
                 WriteLnCenter ('SHOW TOTAL QSOs BY ZONE BY BAND');
                 TextColor (Cyan);
                 WriteLn;
                 WriteLn ('This report will show the number of QSOs made in each zone by band');
                 WriteLn;

                 New (CountryMultTotals);

                 IF GenerateZoneMultiplierTotals THEN
                     PrintQSOsByZone;

                 Dispose (CountryMultTotals);
                 WaitForKeyPressed;
                 END;

            'X', EscapeKey: Exit;
            END;
    UNTIL False;
    END;



    BEGIN
    END.
