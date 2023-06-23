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

UNIT FContest;

{$O+}

{ PLEASE NOTE!!  This file is included with the TR Logging Program as a
  reference document.  It is intended to show you the default values for
  the different contests supported by the program.

  Changing parameters in this file will have no effect on the operation
  of TR or POST.

  Most variable names are the same as their LOGCFG commands, but in
  some cases, they are slightly different. }

INTERFACE

USES trCrt, LogDom, SlowTree, Tree, LogCW, LogWind, LogDupe, ZoneCont,
     Country9;

    FUNCTION  FoundContest (CMD: Str80): BOOLEAN;
    PROCEDURE SetUpFileNames (FileRoot: Str20);


IMPLEMENTATION

Uses LogGrid, LogStuff, LogSCP, keycode; {KK1L: 6.71 attempt to get POST to compile. Moved here from INTERFACE section.}

PROCEDURE SetUpFileNames (FileRoot: Str20);


    BEGIN
    FileRoot := UpperCase (FileRoot);

    IF StringHas (FileRoot, '.') THEN
        FileRoot := PrecedingString (FileRoot, '.');

    IF FileRoot = 'LOGCFG' THEN
        BEGIN
        LogConfigFileName  := 'LOGCFG.DAT';
        LogTempFileName    := 'LOG.TMP';
        LogFileName        := 'LOG.DAT';
        LogRestartFileName := 'RESTART.BIN';
        END
    ELSE
        BEGIN
        LogConfigFileName  := FileRoot + '.CFG';
        LogTempFileName    := FileRoot + '.TMP';
        LogFileName        := FileRoot + '.DAT';
        LogRestartFileName := FileRoot + '.RST';
        END;
    END;


PROCEDURE SetUpRSTQSONumberExchange;

    BEGIN
    CQExchange := '_~ %5NN #';
    ExchangeFunctionKeyMenu := 'F1-DE+Cl  F2-Ex  F3-RST  F4-NR  F5-Cl+Ex  F8-EE  F9-?  Alt-AskforHis';
    RepeatSearchAndPounceExchange := '5NN #';
    SearchAndPounceExchange := '~ %5NN #';
    SetCQMemoryString (CW, F3, '5NN #');
    SetExMemoryString (CW, F3, '5NN');
    SetExMemoryString (CW, F4, 'NR #');
    SetExMemoryString (CW, F5, '@ DE \ 5NN #');
    SetExMemoryString (CW, AltF3, 'RST?');
    SetExMemoryString (CW, AltF4, 'NR?');
    END;


PROCEDURE AddARRLSectionDomesticCountries;

    BEGIN
    AddDomesticCountry ('K');
    AddDomesticCountry ('VE');
    AddDomesticCountry ('KC6');
    AddDomesticCountry ('KG4');
    AddDomesticCountry ('KL');
    AddDomesticCountry ('KH0');
    AddDomesticCountry ('KH1');
    AddDomesticCountry ('KH2');
    AddDomesticCountry ('KH3');
    AddDomesticCountry ('KH4');
    AddDomesticCountry ('KH5');
    AddDomesticCountry ('KH6');
    AddDomesticCountry ('KH7');
    AddDomesticCountry ('KH8');
    AddDomesticCountry ('KH9');
    AddDomesticCountry ('KP1');
    AddDomesticCountry ('KP2');
    AddDomesticCountry ('KP3');
    AddDomesticCountry ('KP4');
    AddDomesticCountry ('KP5');
    END;



FUNCTION FoundContest (CMD: Str80): BOOLEAN;

VAR TempQTH: QTHRecord;

    BEGIN
    CountryTable.CountryMode := ARRLCountryMode;
    CountryTable.ZoneMode    := ITUZoneMode;
    LastLocateCall := '';

    FoundContest := False;
    NoMultMarineMobile := False; {KK1L: 6.68 Added for WRTC 2002 as flag to not count /MM or /AM as mults or countries}
    NoMultDxIfDomestic := False;
    WRTC2018 := False;

    CMD := UpperCase (CMD);

    { You get these for free }

        IF CMD = 'WRTC 2018' THEN
            BEGIN
            LastLocateCall := '';
            LocateCall (MyCall, TempQTH, True);
            Str (CountryTable.GetZone (MyCall),  MyZone);

            ActiveDomesticMult := WYSIWYGDomestic;
            DomesticQTHDataFileName := 'IARUHQ.DOM';
            ActiveExchange := RSTZoneOrSocietyExchange;
            ActiveInitialExchange := ZoneInitialExchange; {KK1L: 6.71 NOTE changed in code}
            {ActiveInitialExchange := None;} {May need this to satisfy rules}
            ActiveDXMult := ARRLDXCC;
            ActiveQSOPointMethod := TwoEuropeFiveOther;
            ActiveZoneMult := NoZoneMults;
            ContestName := 'WRTC 2018';
            SCPMinimumLetters := 0; {KK1L: 6.68 closes SCP to WRTC}
            CQExchange := ' $5NN ' + MyZone;
            ExchangeFunctionKeyMenu := 'F1-DE  F2-Ex  F3-RST  F4-Zone F5-Cl+Ex F8-EE  F9-?  Alt = Ask for his';
            NoMultMarineMobile := True;
            NoMultDxIfDomestic := True;
            WRTC2018 := True;
            MultByBand := True;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := True;
            RepeatSearchAndPounceExchange := '5NN ' + MyZone;
            SearchAndPounceExchange := '~ %5NN ' + MyZone;

            SetCQMemoryString (CW, F3, '5NN ' + MyZone);

            SetExMemoryString (CW, F3, '5NN');
            SetExMemoryString (CW, F4, MyZone);
            SetExMemoryString (CW, F5, '@ DE \ 5NN ' + MyZone);

            SetExMemoryString (CW, AltF3, 'RST?');
            SetExMemoryString (CW, AltF4, 'ZONE?');

            FoundContest := True;

            LocateCall (MyCall, TempQTH, True);
            MyCountry     := CountryTable.GetCountryID (TempQTH.Country);
            MyContinent   := TempQTH.Continent;
            Str (TempQTH.Zone,  MyZone);
            END;

    IF (CMD = 'REGION ONE FIELD DAY') OR (CMD = 'REGION 1 FIELD DAY') THEN
        BEGIN
        ActiveDXMult := CQDXCC;
        ActiveExchange := RSTQSONumberExchange;
        ActiveQSOPointMethod := EuropeanFieldDayQSOPointMethod;
        ContestName := 'Region One Field Day';
        MultByBand := True;
        MultByMode := False;
        QSOByBand := True;
        QSOByMode := True;
        FoundContest := True;
        END;

    IF CMD = 'FIELD DAY' THEN
        BEGIN
        ActiveDomesticMult := DomesticFile;
        ActiveDXMult       := ARRLDXCCWithNoARRLSections;
        ActiveExchange := ClassDomesticOrDXQTHExchange;
        ActiveQSOPointMethod := ARRLFieldDayQSOPointMethod;
        ContestName := 'Field Day';
        DigitalModeEnable := True;
        QSOByBand := True;
        QSOByMode := True;
        DomesticQTHDataFileName := 'ARRLSECT.DOM';
        VHFBandsEnabled := True;
        WARCBandsEnabled := True;

        SetCQMemoryString (CW, F1, 'CQ^FD \ \ FD');
        SetCQMemoryString (CW, F2, 'CQ^FD CQ^FD \ \ FD');

        CQExchange  := ' ' + MyFDClass + ' ' + MySection;
        SearchAndPounceExchange := MyFDClass + ' ' + MySection;

        QSLMessage  := '73 \ FD';

        FoundContest := True;

        AddARRLSectionDomesticCountries;
        END;

    IF (CMD = 'JA INTERNATIONAL DX') OR (CMD = 'JIDX') THEN
        BEGIN
        IF MyCountry = 'JA' THEN
            BEGIN
            ActiveDXMult := ARRLDXCC;
            ActiveInitialExchange := ZoneInitialExchange;
            ActiveExchange := RSTZoneExchange;
            ActiveZoneMult := CQZones;
            END
        ELSE
            BEGIN
            ActiveDomesticMult := DomesticFile;
            ActiveExchange := RSTPrefectureExchange;
            DomesticQTHDataFileName := 'JIDX.DOM';
            END;

        ActiveQSOPointMethod := JapanInternationalDXQSOPointMethod;
        ContestName := 'Japan International DX Test';
        MultByBand := True;
        MultByMode := False;
        QSOByBand := True;
        QSOByMode := False;
        CountryTable.ZoneMode := CQZoneMode;
        FoundContest := True;
        END;

    IF CMD = 'NZ FIELD DAY' THEN
        BEGIN
        ActiveExchange := NZFieldDayExchange;
        ActiveQSOPointMethod := NZFieldDayQSOPointMethod;
        ActiveZoneMult := BranchZones;
        ContestName := 'New Zealand Field Day Contest';
        MultByBand := True;
        MultByMode := True;
        QSOByBand := True;
        QSOByMode := True;
        FoundContest := True;
        END;

    IF CMD = 'SOUTH AMERICAN WW' THEN
        BEGIN
        ActiveExchange := RSTAndContinentExchange;

        IF MyContinent = SouthAmerica THEN
            ActivePrefixMult := NonSouthAmericanPrefixes
        ELSE
            ActivePrefixMult := SouthAmericanPrefixes;

        ActiveQSOPointMethod := SouthAmericanWWQSOPointMethod;
        ContestName := 'South American WW';
        MultByBand := True;
        MultByMode := False;
        QSOByBand := True;
        QSOByMode := True;
        FoundContest := True;
        END;

    IF CMD = 'STEW PERRY' THEN
        BEGIN
        ActiveExchange := RSTAndOrGridExchange;
        ActiveQSOPointMethod := StewPerryQSOPointMethod;
        ContestName := 'STEW PERRY TOPBAND CHALLENGE';
        MultByBand := False;
        MultByMode := False;
        QSOByBand  := False;
        QSOByMode  := False;
        FoundContest := True;

        CQExchange  := ' ' + MyGrid;
        SearchAndPounceExchange := MyGrid;
        END;

    { And these you have to pay for }

    IF NOT TRFree THEN
        BEGIN
        IF CMD = '7QP' THEN
            BEGIN
            ActiveExchange := RSTQTHExchange;
            ActiveQSOPointMethod := TwoPhoneThreeCW;
            ActiveDomesticMult := WYSIWYGDomestic;
            MultByBand := False;
            MultByMode := False;
            QSOByBand := True;
            QSOByMode := True;
            FoundContest := True;
            END;

        IF CMD = 'ALL ASIAN' THEN
            BEGIN
            IF MyContinent = Asia THEN
                ActiveDXMult := ARRLDXCC
            ELSE
                ActivePrefixMult := Prefix;

            ActiveExchange := RSTAgeExchange;
            ActiveQSOPointMethod := AllAsianQSOPointMethod;
            ContestName := 'All Asian';
            MultByBand := True;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := False;
            FoundContest := True;
            END;

        IF CMD = 'ALL JA' THEN
            BEGIN
            ActiveDomesticMult := DomesticFile;
            ActiveExchange := RSTAllJAPrefectureAndPrecedenceExchange;
            ActiveQSOPointMethod := OnePointPerQSO;
            ActiveBand := Band80;
            ContestName := 'All JA';
            DomesticQTHDataFileName := 'ALLJA.DOM';
            MultByBand := True;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := False;
            VHFBandsEnabled := True;
            FoundContest := True;
            END;

        IF CMD = 'JA LONG PREFECT' THEN  {KK1L: 6.72}
            BEGIN
            ActiveDomesticMult := DomesticFile;
            ActiveExchange := RSTLongJAPrefectureExchange;
            ActiveQSOPointMethod := OnePointPerQSO;
            ActiveBand := Band80;
            ContestName := 'JA PREFECTURE';
            DomesticQTHDataFileName := 'JACG3.DOM';
            MultByBand := False;
            MultByMode := False;
            QSOByBand  := False;
            QSOByMode  := True;
            VHFBandsEnabled := False;
            FoundContest := True;
            END;

        IF CMD = 'ARCI' THEN
            BEGIN
            ActiveExchange := RSTPossibleDomesticQTHAndPower;
            ActiveDomesticMult := DomesticFile;
            ActiveDXMult := ARRLDXCCWithNoUSACanadaKH6OrKL7;
            ActiveQSOPointMethod := ARCIQSOPointMethod;
            ContestName := 'ARCI QSO PARTY';
            DomesticQTHDataFileName := 'S50P12.DOM';
            MultByBand := True;
            MultByMode := False;
            QSOByBand := True;
            QSOByMode := False;
            FoundContest := True;

            AddDomesticCountry ('K');
            AddDomesticCountry ('VE');
            AddDomesticCountry ('KL');
            AddDomesticCountry ('KH6');
            END;

        IF CMD = 'ARI' THEN
            BEGIN
            ActiveExchange := RSTAndQSONumberOrDomesticQTHExchange;
            ActiveDomesticMult := WYSIWYGDomestic;
            ActiveDXMult := ARRLDXCCWithNoIOrIS0;
            ActiveQSOPointMethod := ARIQSOPointMethod;
            ContestName := 'ARI International DX Contest';
            MultByBand := True;
            MultByMode := False;
            QSOByBand := True;
            QSOByMode := True;
            SetUpRSTQSONumberExchange;
            FoundContest := True;
            AddDomesticCountry ('I');
            AddDomesticCountry ('IS');
            END;

        IF CMD = 'ARRL 10' THEN
            BEGIN
            ActiveBand := Band10;
            ActiveDomesticMult := DomesticFile;
            ActiveDXMult := ARRLDXCCWithNoUSACanadaKH6KL7OrXE;
            ActiveQSOPointMethod := ARRL10QSOPointMethod;
            ActiveExchange := RSTDomesticQTHOrQSONumberExchange;
            ContestName := 'ARRL Ten Meter Contest';
            DomesticQTHDataFileName := 'ARRL10.DOM';
            ExchangeMemoryEnable := False;
            MultByBand := False;
            MultByMode := True;
            MultipleBandsEnabled := False;
            QSOByBand := False;
            QSOByMode := True;
            FoundContest := True;

            AddDomesticCountry ('K');     { Doesn't really get used }
            AddDomesticCountry ('XE');
            AddDomesticCountry ('VE');
            AddDomesticCountry ('KL');
            AddDomesticCountry ('KH6');
            AddDomesticCountry ('XE');

            IF MyState <> '' THEN
                BEGIN
                CQExchange := ' 5NN ' + MyState;
                SetCQMemoryString (CW, F3, '5NN ' + MyState);
                SetEXMemoryString (CW, F3, '5NN');
                SetExMemoryString (CW, F4, MyState);
                SetExMemoryString (CW, F5, '@ DE \ 5NN ' + MyState);
                SetExMemoryString (CW, AltF3, 'RST?');
                SetExMemoryString (CW, AltF4, 'QTH?');
                RepeatSearchAndPounceExchange := '5NN ' + MyState;
                SearchAndPounceExchange := '~ %5NN ' + MyState;
                END
            ELSE
                SetUpRSTQSONumberExchange;
            END;

    
        IF CMD = 'ARRL 160' THEN
            BEGIN
            IF (MyCountry = 'K') OR
               (MyCountry = 'VE') OR
               (MyCountry = 'KC6') OR
               (MyCountry = 'KG4') OR
               (MyCountry = 'KL') OR
               (MyCountry = 'KH0') OR
               (MyCountry = 'KH1') OR
               (MyCountry = 'KH2') OR
               (MyCountry = 'KH3') OR
               (MyCountry = 'KH4') OR
               (MyCountry = 'KH5') OR
               (MyCountry = 'KH6') OR
               (MyCountry = 'KH7') OR
               (MyCountry = 'KH8') OR
               (MyCountry = 'KH9') OR
               (MyCountry = 'KP1') OR
               (MyCountry = 'KP2') OR
               (MyCountry = 'KP3') OR
               (MyCountry = 'KP4') OR
               (MyCountry = 'KP5') THEN
                BEGIN
                ActiveExchange := RSTDomesticOrDXQTHExchange;
                ActiveDXMult   := ARRLDXCCWithNoARRLSections;
                END
            ELSE
                ActiveExchange := RSTDomesticQTHExchange;

            ActiveDomesticMult := DomesticFile;
            ActiveQSOPointMethod := ARRL160QSOPointMethod;
            ContestName := 'ARRL 160 Contest';
            DomesticQTHDataFileName := 'ARRLSECT.DOM';
            MultByBand := False;
            MultByMode := False;
            QSOByBand  := False;
            QSOByMode  := False;
            FoundContest := True;

            AddARRLSectionDomesticCountries;
            END;


        IF CMD = 'ARRL DX' THEN
            BEGIN
            IF MyCall = '' THEN
                BEGIN
                Write ('Please put valid MY CALL statement before the CONTEST statement.  Thanks!');
                Exit;
                END;

            IF (MyCountry = 'K') OR
               (MyCountry = 'VE') THEN
                BEGIN
                ActiveExchange := RSTPowerExchange;
                ActiveDXMult   := ARRLDXCCWithNoUSAOrCanada;
                END
            ELSE
                BEGIN
                ActiveDomesticMult := DomesticFile;
                DomesticQTHDataFileName := 'S48P14DC.DOM';
                ActiveExchange     := RSTDomesticQTHExchange;
                END;

            ActiveQSOPointMethod := ARRLDXQSOPointMethod;
            ContestName := 'ARRL DX Test';
            MultByBand := True;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := False;
            FoundContest := True;

            AddDomesticCountry ('K');
            AddDomesticCountry ('VE');
            END;

        IF CMD = 'ARRL RTTY ROUNDUP' THEN
            BEGIN
            CountryTable.CountryMode := CQCountryMode;
            CountryTable.ZoneMode    := CQZoneMode;

            ActiveDomesticMult := DomesticFile;
            ActiveDXMult := ARRLDXCC;
            ActiveExchange := RSTDomesticQTHOrQSONumberExchange;;
            ActiveQSOPointMethod := OnePointPerQSO;
            ContestName := 'ARRL RTTY ROUNDUP';
            DigitalModeEnable := True;
            DomesticQTHDataFileName := 'S48P14DC.DOM'; {KK1L: 6.72 Used DC file instead per rules}

            MultByBand := False;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := False;
            FoundContest := True;

            AddDomesticCountry ('K');
            AddDomesticCountry ('VE');
            END;



        IF CMD = 'ARRL VHF QSO' THEN
            BEGIN
            ActiveBand := Band6;
            ActiveDomesticMult := GridSquares;
            ActiveExchange := RSTDomesticQTHExchange;
            ActiveQSOPointMethod := ARRLVHFQSOPointMethod;
            ContestName := 'VHF QSO Party';
            MultByBand := True;
            MultByMode := False;
            QSOByBand := True;
            QSOByMode := False;
            HFBandEnable := False;
            VHFBandsEnabled := True;
            FoundContest := True;
            END;

        IF CMD = 'ARRL VHF SS' THEN
            BEGIN
            ActiveBand := Band6;
            ActiveDomesticMult := GridSquares;
            ActiveExchange := RSTDomesticQTHExchange;
            ActiveQSOPointMethod := ARRLVHFSSPointMethod;
            ContestName := 'ARRL VHF SWEEPSTAKES';
            MultByBand := True;
            MultByMode := False;
            QSOByBand := True;
            QSOByMode := False;
            HFBandEnable := False;
            VHFBandsEnabled := True;
            FoundContest := True;
            END;

        IF CMD = 'AP SPRINT' THEN
            BEGIN
            ActiveBand := Band20;
            ActiveExchange := RSTQSONumberExchange;
            ActivePrefixMult := Prefix;
            ActiveQSOPointMethod := OnePointPerQSO;
            ContestName := 'ASIA PACIFIC SPRINT';
            MultByBand := False;
            MultByMode := False;
            QSOByBand := True;
            QSOByMode := False;
            FoundContest := True;
            END;

        IF CMD = 'BALTIC' THEN
            BEGIN
            ActiveBand := Band80;
            ActiveExchange := RSTQSONumberExchange;
            ActiveQSOPointMethod := BalticQSOPointMethod;
            ContestName := 'Baltic Contest';
            QSOByMode := True;
            FoundContest := True;
            END;

        IF (CMD = 'CAL QSO PARTY') OR (CMD = 'CQP') OR (CMD = 'CALIFORNIA QSO PARTY') THEN
            BEGIN
            IF UpperCase (Copy (MyState, 1, 2)) = 'CA' THEN
                BEGIN
                ActiveDomesticMult := DomesticFile;
                DomesticQTHDataFileName := 'CALQSOW6.DOM';
                ActiveDXMult := NoCountDXMults;
                ActiveExchange := QSONumberDomesticOrDXQTHExchange;
                END
            ELSE
                BEGIN
                ActiveDomesticMult := DomesticFile;
                DomesticQTHDataFileName := 'CALCTY.DOM';
                ActiveExchange := QSONumberDomesticQTHExchange;
                END;

            ActiveQSOPointMethod := TwoPhoneThreeCW;
            ContestName := 'California QSO Party';
            MultByBand := False;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := True;
            FoundContest := True;

            AddDomesticCountry ('K');
            AddDomesticCountry ('VE');
            AddDomesticCountry ('KH6');
            AddDomesticCountry ('KL');
            END;

        IF CMD = 'COUNTY HUNTER' THEN
            BEGIN
            ActiveExchange := RSTQTHExchange;
            ActiveQSOPointMethod := OnePointPerQSO;
            ContestName := 'County Hunting';
            FoundContest := True;
            END;

        IF CMD = 'CQ 160' THEN
            BEGIN
            CountryTable.CountryMode := CQCountryMode;
            CountryTable.ZoneMode    := CQZoneMode;

            ActiveBand             := Band160;
            ActiveDomesticMult     := DomesticFile;
            ActiveDXMult           := CQDXCCWithNoUSAOrCanada;
            ActiveExchange         := RSTAndDomesticQTHOrZoneExchange;
            ActiveQSOPointMethod   := CQ160QSOPointMethod;
            ContestName := 'CQ 160 Contest';
            MultByBand := False;
            MultByMode := False;
            MultipleBandsEnabled := False;
            QSOByBand  := False;
            QSOByMode  := False;
            DomesticQTHDataFileName := 'S48P13DC.DOM';
            FoundContest := True;

            AddDomesticCountry ('K');
            AddDomesticCountry ('VE');
            END;

        IF CMD = 'CQ M' THEN
            BEGIN
            ActiveDXMult := ARRLDXCC;
            ActiveExchange := RSTQSONumberExchange;
            ActiveQSOPointMethod := CQMQSOPointMethod;
            ContestName := 'CQ M Contest';
            MultByBand := True;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := False;
            SetUpRSTQSONumberExchange;
            FoundContest := True;
            END;

        IF CMD = 'CQ VHF' THEN
            BEGIN
            ActiveBand := Band6;
            ActiveDomesticMult := GridSquares;
            ActiveExchange := RSTDomesticQTHExchange;
            ActiveQSOPointMethod := CQVHFQSOPointMethod;
            ContestName := 'CQ WORLD WIDE VHF Contest';
            MultByBand := True;
            MultByMode := False;
            QSOByBand := True;
            QSOByMode := False;
            HFBandEnable := False;
            VHFBandsEnabled := True;
            FoundContest := True;
            END;

        IF CMD = 'CQ WPX' THEN
            BEGIN
            CountryTable.CountryMode := CQCountryMode;
            CountryTable.ZoneMode    := CQZoneMode;

            ActiveExchange := RSTQSONumberExchange;
            ActivePrefixMult := Prefix;
            ActiveQSOPointMethod := CQWPXQSOPointMethod;
            ContestName := 'CQ WPX Contest';
            MultByBand := False;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := False;
            SetUpRSTQSONumberExchange;
            FoundContest := True;
            END;

    

        IF CMD = 'CQ WPX RTTY' THEN
            BEGIN
            CountryTable.CountryMode := CQCountryMode;
            CountryTable.ZoneMode    := CQZoneMode;

            ActiveExchange := RSTQSONumberExchange;
            ActivePrefixMult := Prefix;
            ActiveQSOPointMethod := CQWPXRTTYQSOPointMethod;
            ContestName := 'CQ WPX Contest';
            MultByBand := False;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := False;
            SetUpRSTQSONumberExchange;
            FoundContest := True;
            END;

    

        IF CMD = 'CQ WW' THEN
            BEGIN
            CountryTable.CountryMode := CQCountryMode;
            CountryTable.ZoneMode    := CQZoneMode;

            ActiveDXMult := CQDXCC;
            ActiveExchange := RSTZoneExchange;
            ActiveInitialExchange := ZoneInitialExchange;
            ActiveQSOPointMethod := CQWWQSOPointMethod;
            ActiveZoneMult := CQZones;
            ContestName := 'CQ WW Contest';
            CQExchange := '5NN ' + MyZone;
            ExchangeFunctionKeyMenu := 'F1-DE Call  F2-RST#  F3-RST  F4-Zone  Alt-AskForHis  F5-Cl+Ex  AltF10-CfmCl';
            SetCQMemoryString (CW, F3, '5NN ' + MyZone);
            SetEXMemoryString (CW, F3, '5NN');
            SetExMemoryString (CW, F4, MyZone);
            SetExMemoryString (CW, F5, '@ DE \ 5NN ' + MyZone);
            SetExMemoryString (CW, AltF3, 'RST?');
            SetExMemoryString (CW, AltF4, 'NR?');
            RepeatSearchAndPounceExchange := '5NN ' + MyZone;
            SearchAndPounceExchange := '~ %5NN ' + MyZone;
            MultByBand := True;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := False;
            LogFrequencyEnable := True;
            FoundContest := True;
            END;

        IF CMD = 'CQ WW RTTY' THEN
            BEGIN
            CountryTable.CountryMode := CQCountryMode;
            CountryTable.ZoneMode    := CQZoneMode;

            ActiveDomesticMult := DomesticFile;
            ActiveDXMult := CQDXCC;
            ActiveExchange := RSTZoneAndPossibleDomesticQTHExchange;
            ActiveInitialExchange := ZoneInitialExchange;
            ActiveQSOPointMethod := CQWWRTTYQSOPointMethod;
            ActiveZoneMult := CQZones;
            ContestName := 'CQ WW RTTY CONTEST';
            DigitalModeEnable := True;
            DomesticQTHDataFileName := 'S48P13.DOM';

            MultByBand := True;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := False;
            FoundContest := True;

            AddDomesticCountry ('K');
            AddDomesticCountry ('VE');
            END;

        IF CMD = 'CROATIAN' THEN
            BEGIN
            ActiveQSOPointMethod := CroatianQSOPointMethod;
            ActiveDomesticMult := NoDomesticMults;
            ActiveDXMult := CQDXCC;
            ActiveExchange := RSTQSONumberExchange;
            ActiveZoneMult := NoZoneMults;
            ContestName := 'Croatian';
            QSOByBand := True;
            MultByBand := True;
            SetUpRSTQSONumberExchange;
            FoundContest := True;
            END;

         IF CMD = 'CWT' THEN
            BEGIN
            ActiveExchange := CWTExchange;
            ActiveDomesticMult := WYSIWYGDomestic;
            ActiveQSOPointMethod := OnePointPerQSO;
            ContestName := 'CWT';
            LiteralDomesticQTH := True;
            QSOByBand := True;
            FoundContest := True;
            END;

         IF CMD = 'CWO' THEN
            BEGIN
            ActiveExchange := QSONumberAndNameExchange;
            ActiveDomesticMult := WYSIWYGDomestic;
            ActiveQSOPointMethod := OnePointPerQSO;
            ContestName := 'CWO';
            LiteralDomesticQTH := True;
            QSOByBand := True;
            FoundContest := True;
            END;

         IF CMD = 'EUROPEAN HFC' THEN
            BEGIN
            ActiveExchange := RSTZoneExchange;
            ActiveQSOPointMethod := OnePointPerQSO;
            ActiveZoneMult := EUHFCYear;
            ContestName := 'European HF Championship';
            CQExchange := '5NN ' + MyZone;
            ExchangeFunctionKeyMenu := 'F1-DE Call  F2-RST#  F3-RST  F4-Year Alt-AskForHis  F5-Cl+Ex  AltF10-CfmCl';
            SetCQMemoryString (CW, F1, 'CQ EU DE \ \ ');
            SetCQMemoryString (CW, F2, 'CQ EU CQ EU \ \ TEST');
            SetCQMemoryString (CW, F3, '5NN ' + MyZone);
            SetEXMemoryString (CW, F3, '5NN');
            SetExMemoryString (CW, F4, MyZone);
            SetExMemoryString (CW, F5, '@ DE \ 5NN ' + MyZone);
            SetExMemoryString (CW, AltF3, 'RST?');
            SetExMemoryString (CW, AltF4, 'NR?');
            RepeatSearchAndPounceExchange := '@ 5NN ' + MyZone;
            SearchAndPounceExchange := 'TU 5NN ' + MyZone;
            MultByBand := True;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := True;
            FoundContest := True;
            END;

        IF (CMD = 'EUROPEAN SPRINT') OR (CMD = 'EU SPRINT') THEN
            BEGIN
            ActiveBand := Band20;
            ActiveExchange := QSONumberAndNameExchange;
            ActiveInitialExchange := NameInitialExchange;
            ActiveQSOPointMethod := EuropeanSprintQSOPointMethod;
            ContestName := 'European Sprint';
            QSOByBand := True;
            QSOByMode := False;
            FoundContest := True;
            END;

        IF CMD = 'EUROPEAN VHF' THEN
            BEGIN
            ActiveBand := Band6;
            ActiveExchange := RSTQSONumberAndGridSquareExchange;
            ActiveQSOPointMethod := EuropeanVHFQSOPointMethod;
            ContestName := 'EUROPEAN VHF CONTEST';
            MultByBand := True;
            MultByMode := False;
            QSOByBand := True;
            QSOByMode := False;
            HFBandEnable := False;
            VHFBandsEnabled := True;
            FoundContest := True;
            END;

        IF CMD = 'FISTS' THEN
            BEGIN
            ActiveExchange := RSTQTHNameAndFistsNumberOrPowerExchange;
            ActiveQSOPointMethod := FistsQSOPointMethod;
            ContestName := 'FISTS SPRINT';
            ActiveDomesticMult := DomesticFile;
            DomesticQTHDataFileName := 'S49P8.DOM';
            MultByBand := False;
            MultByMode := False;
            QSOByBand := True;
            QSOByMode := False;
            FoundContest := True;
            END;

        IF (CMD = 'FQP') OR (CMD = 'FLORIDA QSO PARTY') THEN
            BEGIN
            IF UpperCase (Copy (MyState, 1, 2)) = 'FL' THEN
                BEGIN
                ActiveDomesticMult := DomesticFile;
                DomesticQTHDataFileName := 'FQP_FLA.DOM';
                ActiveExchange := RSTDomesticOrDXQTHExchange;
                END
            ELSE
                BEGIN
                ActiveDomesticMult := DomesticFile;
                DomesticQTHDataFileName := 'FQP.DOM';
                ActiveExchange := RSTDomesticQTHExchange;
                END;

            ActiveQSOPointMethod := OnePhoneTwoCW;
            ContestName := 'Florida QSO Party';
            MultByBand := False;
            MultByMode := True;
            QSOByBand  := True;
            QSOByMode  := True;
            FoundContest := True;

            AddDomesticCountry ('K');
            AddDomesticCountry ('VE');
            AddDomesticCountry ('KH6');
            AddDomesticCountry ('KL');
            END;


        IF CMD = 'GENERAL QSO' THEN
            BEGIN
            ActiveExchange := RSTNameAndQTHExchange;
            ActiveInitialExchange := NameQTHInitialExchange;
            ActiveQSOPointMethod := OnePointPerQSO;
            AutoDupeEnableCQ := False;
            AutoDupeEnableSAndP := False;
            ContestName := 'General QSOs';
            QSOByBand := True;
            QSOByMode := True;
            WARCBandsEnabled := True;
            FoundContest := True;
            END;

        IF CMD = 'GRID LOC' THEN
            BEGIN
            ActiveDomesticMult := WYSIWYGDomestic;
            ActiveInitialExchange := NameInitialExchange;
            ActiveExchange := NameAndPossibleGridSquareExchange;
            ActiveQSOPointMethod := OnePointPerQSO;
            ContestName := 'Grid Loc Contest';
            MultByBand := True;
            MultByMode := False;
            QSOByBand := True;
            QSOByMode := True;
            FoundContest := True;
            END;

        IF CMD = 'HA DX' THEN
            BEGIN
            ActiveDomesticMult := DomesticFile;
            ActiveExchange := RSTQSONumberAndPossibleDomesticQTHExchange;
            ActiveQSOPointMethod := HADXQSOPointMethod;
            ContestName := 'HA-DX Contest';
            DomesticQTHDataFileName := 'HUNGARY.DOM';
            MultByBand := True;
            MultByMode := False;
            QSOByBand := True;
            QSOByMode := False;
            SetUpRSTQSONumberExchange;
            FoundContest := True;
            AddDomesticCountry ('HA');
            END;

        IF CMD = 'HELVETIA' THEN
            BEGIN
            ActiveDomesticMult := DomesticFile;

            IF MyCountry = 'HB' THEN
                ActiveDXMult := ARRLDXCC
            ELSE
                AddDomesticCountry ('HB');

            ActiveExchange := RSTQSONumberAndPossibleDomesticQTHExchange;
            ActiveQSOPointMethod := HelvetiaQSOPointMethod;
            ContestName := 'Helvetia Contest';
            DomesticQTHDataFileName := 'SWISS.DOM';
            ExchangeFunctionKeyMenu := 'F1-DE Call  F2-RST #  F3-RST  F4-#  Alt-AskForHis  AltF5-QTH?';
            MultByBand := True;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := False;
            SetExMemoryString (CW, AltF5, 'QTH?');
            SetUpRSTQSONumberExchange;
            FoundContest := True;
            END;

        IF (CMD = 'IARU') OR (CMD = 'WRTC') THEN
            BEGIN
            LastLocateCall := '';
            LocateCall (MyCall, TempQTH, True);
            Str (CountryTable.GetZone (MyCall),  MyZone);

            ActiveDomesticMult := WYSIWYGDomestic;
            DomesticQTHDataFileName := 'IARUHQ.DOM'; {KK1L: 6.68}
            ActiveExchange := RSTZoneOrSocietyExchange;
            ActiveInitialExchange := ZoneInitialExchange; {KK1L: 6.71 NOTE changed in code}
            ActiveQSOPointMethod := IARUQSOPointMethod;
            ActiveZoneMult := ITUZones;
            ContestName := 'IARU Radiosport';
            CQExchange := ' $5NN ' + MyZone;
            ExchangeFunctionKeyMenu := 'F1-DE  F2-Ex  F3-RST  F4-Zone F5-Cl+Ex F8-EE  F9-?  Alt = Ask for his';
            MultByBand := True;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := True;
            RepeatSearchAndPounceExchange := '5NN ' + MyZone;
            SearchAndPounceExchange := '~ %5NN ' + MyZone;

            SetCQMemoryString (CW, F3, '5NN ' + MyZone);

            SetExMemoryString (CW, F3, '5NN');
            SetExMemoryString (CW, F4, MyZone);
            SetExMemoryString (CW, F5, '@ DE \ 5NN ' + MyZone);

            SetExMemoryString (CW, AltF3, 'RST?');
            SetExMemoryString (CW, AltF4, 'ZONE?');

            FoundContest := True;

            LocateCall (MyCall, TempQTH, True);
            MyCountry     := CountryTable.GetCountryID (TempQTH.Country);
            MyContinent   := TempQTH.Continent;
            Str (TempQTH.Zone,  MyZone);

            IF CMD = 'WRTC' THEN
                BEGIN
                ActiveDXMult := ARRLDXCC;
                ActiveQSOPointMethod := OnePhoneTwoCW;
                ContestName := 'IARU RADIOSPORT FOR WRTC';
                ActiveBand := Band40;
                END;

            END;

        IF CMD = 'INTERNET SIX' THEN
            BEGIN
            ActiveBand := Band6;
            ActiveDomesticMult := GridSquares;
            ActiveDXMult       := CQDXCC;
            ActiveExchange := RSTDomesticQTHExchange;
            ActiveQSOPointMethod := InternetSixQSOPointMethod;
            ContestName := 'Internet 6m Contest';
            MultByBand := False;
            MultByMode := False;
            QSOByBand := False;
            QSOByMode := False;
            HFBandEnable := False;
            VHFBandsEnabled := True;
            FoundContest := True;
            END;

        IF CMD = 'INTERNET SPRINT' THEN
            BEGIN
            ActiveBand := Band20;
            AutoDupeEnableCQ    := False;
            AutoDupeEnableSAndP := False;
            ActiveExchange := QSONumberNameDomesticOrDXQTHExchange;
            ActiveDomesticMult := DomesticFile;
            ActiveDXMult := NorthAmericanARRLDXCCWithNoUSACanadaOrkL7;
            ActiveInitialExchange := NoInitialExchange;
            ActiveQSOPointMethod := AlwaysOnePointPerQSO;
            ContestName := 'Internet SprINT';
            DomesticQTHDataFileName := 'S49P13.DOM';
            ExchangeMemoryEnable := False;
            MultByBand := False;
            MultByMode := False;
            QSOByBand := True;
            QSOByMode := False;
            SprintQSYRule := True;
            VisibleDupeSheetEnable := False;
            FoundContest := True;

            SearchAndPounceExchange := '@ # ( ' + MyState + ' \';
            RepeatSearchAndPounceExchange := '@ # ( ' + MyState;
            CQExchange := ' \ # ( ' + MyState;
            QSLMessage := 'EE';

            SetCQMemoryString (CW, F1,  'INT \');
            SetCQMemoryString (CW, F2, 'CQ^INT \ \ INT');

            SetCQMemoryString (CW, F5, '?');
            SetCQMemoryString (CW, F6, 'INT \');
            SetCQMemoryString (CW, F7, 'CQ^INT \ \ INT');
            SetCQMemoryString (CW, F8, 'CQ^INT CQ^INT \ \ INT');

            SetEXMemoryString (CW, F7, 'CQ^INT \ \ INT');
            SetEXMemoryString (CW, F8, 'CQ^INT CQ^INT \ \ INT');
            SetEXMemoryString (CW, F3, '#');
            SetEXMemoryString (CW, F4, '(');
            SetEXMemoryString (CW, F5, MyState);
            SetEXMemoryString (CW, F6, '@ \ # ( ' + MyState);
            SetEXMemorySTring (CW, AltF3, 'NR?');
            SetEXMemorySTring (CW, AltF4, 'NAME?');
            SetEXMemorySTring (CW, AltF5, 'QTH?');

            AddDomesticCountry ('K');
            AddDomesticCountry ('VE');
            AddDomesticCountry ('KL');
            END;

        IF CMD = 'IOTA' THEN
            BEGIN
            ActiveDomesticMult := IOTADomestic;
            ActiveExchange := RSTQSONumberAndPossibleDomesticQTHExchange;
            ActiveQSOPointMethod := IOTAQSOPointMethod;
            ContestName := 'RSGB IOTA Contest';
            MultByBand := True;
            MultByMode := True;
            QSOByBand  := True;
            QSOByMode  := True;
            FoundContest := True;
            END;

        IF CMD = 'JARTS' THEN
            BEGIN
            ActiveExchange := RSTAgeExchange;
            ActiveQSOPointMethod := AllAsianQSOPointMethod;
            ContestName := 'Japanese Amateur Radio Teleprinter Society';
            MultByBand := True;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := False;
            FoundContest := True;
            END;

        IF CMD = 'KCJ' THEN
            BEGIN
            ActiveDomesticMult := DomesticFile;

            IF (MyCountry = 'JA') OR
               (MyCountry = 'JD1') THEN
                DomesticQTHDataFileName := 'JAPREFCT.DOM'
            ELSE
                DomesticQTHDataFileName := 'JAPREF.DOM';

            ActiveExchange := RSTDomesticQTHExchange;
            ActiveMode := CW;
            ActiveQSOPointMethod := KCJQSOPointMethod;
            ContestName := 'KCJ Single Op Contest';
            MultByBand := True;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := False;

            ExchangeFunctionKeyMenu := 'F1-DE  F2-Ex  F3-RST  F4-QTH  F5-Cl+Ex  F8-EE  F9-?  Alt = Ask for his';

            CQExchange := '_~ %5NN ' + CountryTable.GetContinentName (MyContinent);
            QSLMessage := '73 \ KCJ';
            QSOBeforeMessage := '_SRI QSO B4 73 \ KCJ';
            QuickQSLMessage1 := 'QSL TU EE';
            QuickQSLMessage2 := 'TU EE';
            RepeatSearchAndPounceExchange := '5NN ' + CountryTable.GetContinentName (MyContinent);
            SearchAndPounceExchange := '~ %5NN ' + CountryTable.GetContinentName (MyContinent);
            SetCQMemoryString (CW, F1, 'CQTEST CQKCJ  \ \ KCJ');
            SetCQMemoryString (CW, F2, 'CQKCJ CQTEST CQKCJ  \  \  \ KCJ');

            SetCQMemoryString (CW, F3, '5NN ' + MyZone);
            SetExMemoryString (CW, F3, '5NN');
            SetExMemoryString (CW, F4, CountryTable.GetContinentName (MyContinent));
            SetExMemoryString (CW, F5, '@ DE \ 5NN ' + CountryTable.GetContinentName (MyContinent));
            SetExMemoryString (CW, AltF3, 'RST?');
            SetExMemoryString (CW, AltF4, 'QTH?');
            FoundContest := True;
            END;

        IF CMD = 'KIDS DAY' THEN
            BEGIN
            ActiveExchange := KidsDayExchange;
            ActiveQSOPointMethod := OnePointPerQSO;
            AutoDupeEnableCQ := False;
            ContestName := 'KIDS DAY';
            FoundContest := True;
            END;

        IF CMD = 'KVP' THEN
            BEGIN
            ClrScr;
            TextColor (Yellow);
            WriteLn ('V LOGCFG.DAT vpisi MY ZONE = letnica, da ti bo program');
            WriteLn ('pravilno nastavil CW sporocila!');
            WriteLn;
            WaitForKeyPressed;

            ActiveBand := Band80;
            ActiveExchange := RSTZoneExchange;
            ActiveInitialExchange := ZoneInitialExchange;
            ActiveQSOPointMethod := OnePhoneTwoCW;
            ActiveZoneMult := BranchZones;
            ContestName := 'KV Prvenstvo ZRS';
            CQExchange := '5NN ' + MyZone;
            ExchangeFunctionKeyMenu := 'F1-DE Call  F2-RST#  F3-RST  F4-Leto  Alt-AskForHis  F5-Cl+Ex  AltF10-CfmCl';
            SetCQMemoryString (CW, F1, 'CQ ZRS DE \ \ ');
            SetCQMemoryString (CW, F2, 'CQ ZRS CQ ZRS \ \ TEST');
            SetCQMemoryString (CW, F3, '5NN ' + MyZone);
            SetEXMemoryString (CW, F3, '5NN');
            SetExMemoryString (CW, F4, MyZone);
            SetExMemoryString (CW, F5, '@ DE \ 5NN ' + MyZone);
            SetExMemoryString (CW, AltF3, 'RST?');
            SetExMemoryString (CW, AltF4, 'NR?');
            RepeatSearchAndPounceExchange := '@ 5NN ' + MyZone;
            SearchAndPounceExchange := 'TU 5NN ' + MyZone;
            MultByBand := True;
            MultByMode := True;
            MultipleBandsEnabled := False;
            QSOByBand := False;
            QSOByMode := True;
            FoundContest := True;
            END;

        IF (CMD = 'MICHIGAN QSO PARTY') OR (CMD = 'MICH QSO PARTY') OR (CMD = 'MI QSO PARTY') THEN
            BEGIN
            ActiveDomesticMult := DomesticFile;
            ActiveQSOPointMethod := OnePhoneTwoCW;
            QSOByBand := True;
            QSOByMode := True;
            MultByBand := False;
            MultByMode := True;
            ContestName := 'MICHIGAN QSO PARTY';

            IF UpperCase (Copy (MyState, 1, 2)) = 'MI' THEN
                BEGIN
                DomesticQTHDataFileName := 'MIQP.DOM';
                {ActiveExchange := RSTQSONumberAndPossibleDomesticQTHExchange;} {KK1L: 6.73}
                ActiveExchange := QSONumberDomesticQTHExchange; {KK1L: 6.73}
                END
            ELSE
                BEGIN
                DomesticQTHDataFileName := 'MICHCTY.DOM';
                {ActiveExchange := RSTQSONumberAndDomesticQTHExchange;} {KK1L: 6.73}
                ActiveExchange := QSONumberDomesticQTHExchange; {KK1L: 6.73}
                END;

            AddDomesticCountry ('K');
            AddDomesticCountry ('VE');
            AddDomesticCountry ('KH6');
            AddDomesticCountry ('KL');
            FoundContest := True;
            END;

        IF (CMD = 'MARCONI MEMORIAL') OR (CMD = 'MMC') THEN
            BEGIN
            CountryTable.CountryMode := CQCountryMode;
            CountryTable.ZoneMode    := CQZoneMode;
            ActiveExchange := RSTQSONumberExchange;
            ActiveDXMult := CQDXCC;
            ActiveQSOPointMethod := MMCQSOPointMethod;
            ContestName := 'MARCONI MEMORIAL CONTEST';
            MultByBand := True;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := False;
            SetUpRSTQSONumberExchange;
            FoundContest := True;
            END;

        IF (CMD = 'MINNESOTA QSO PARTY') OR (CMD = 'MINN QSO PARTY') OR (CMD = 'MN QSO PARTY') THEN
            BEGIN
            IF UpperCase (Copy (MyState, 1, 2)) = 'MN' THEN
                BEGIN
                ActiveDomesticMult := DomesticFile;
                DomesticQTHDataFileName := 'MQP.DOM';
                ActiveExchange := NameAndDomesticOrDXQTHExchange;
                END
            ELSE
                BEGIN
                ActiveDomesticMult := DomesticFile;
                DomesticQTHDataFileName := 'MINNCTY.DOM';
                ActiveExchange := NameAndDomesticOrDXQTHExchange;
                END;

            ActiveQSOPointMethod := MQPQSOPointMethod;
            ContestName := 'Minnesota QSO Party';
            MultByBand := False;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := True;
            FoundContest := True;

            AddDomesticCountry ('K');
            AddDomesticCountry ('VE');
            AddDomesticCountry ('KH6');
            AddDomesticCountry ('KL');
            VHFBandsEnabled := True;
            END;

        IF CMD = 'NA QSO' THEN
            BEGIN
            ActiveDomesticMult := DomesticFile;
            ActiveDXMult := NorthAmericanARRLDXCCWithNoUSACanadaOrkL7;
            ActiveExchange := NameAndDomesticOrDXQTHExchange;
            ActiveInitialExchange := NameInitialExchange;
            ActiveQSOPointMethod := OnePointPerQSO;
            ContestName := 'North American QSO Party';
            DomesticQTHDataFileName := 'NAQP.DOM';

            MultByBand := True;
            MultByMode := False;
            QSOByBand := True;
            QSOByMode := False;
            FoundContest := True;

            CQExchange := ' ' + MyName + ' ' + MyState;
            QSLMessage := '73 \ NA>';
            QuickQSLMessage1 := 'TU';
            QSOBeforeMessage := ' QSO B4 \ NA';
            SearchAndPounceExchange := MyName + ' ' + MyState;
            CorrectedCallMessage := '} R';

            SetCQMemoryString (CW, F1, 'CQ^NA \ \ NA>');
            SetCQMemoryString (CW, F2, 'CQ^NA CQ^NA \ \ NA>');

            SetCQMemoryString (CW, F5,  '?');
            SetCQMemoryString (CW, F6,  'NA \ NA');
            SetCQMemoryString (CW, F7,  'CQ^NA \ \ NA');
            SetCQMemoryString (CW, F8,  'CQ^NA CQ^NA \ \ NA');

            SetCQMemoryString (CW, AltF1,  'NA \ \ NA');
            SetCQMemoryString (CW, AltF1,  'NA \ \ NA');

            SetExMemoryString (CW, F3, MyName);
            SetExMemoryString (CW, F4, MyState);
            SetExMemoryString (CW, F5, '@ DE \ ' + MyName + ' ' + MyState);
            SetExMemoryString (CW, AltF3, 'NAME?');
            SetExMemoryString (CW, AltF4, 'QTH?');

            SetExMemoryString (CW, F7,  'CQ^NA \ \ NA');
            SetExMemoryString (CW, F8,  'CQ^NA CQ^NA \ \ NA');

            AddDomesticCountry ('K');
            AddDomesticCountry ('VE');
            AddDomesticCountry ('KL');
            AddDomesticCountry ('KH6');
            END;

        {KK1L: 6.65 Added NEQP}

        IF (CMD = 'NEW ENGLAND QSO') OR (CMD = 'NEQSO') OR (CMD = 'NEW ENGLAND QSO PARTY') THEN
            BEGIN
            IF (UpperCase (Copy (MyState, 1, 2)) = 'ME') OR
               (UpperCase (Copy (MyState, 1, 2)) = 'NH') OR
               (UpperCase (Copy (MyState, 1, 2)) = 'VT') OR
               (UpperCase (Copy (MyState, 1, 2)) = 'MA') OR
               (UpperCase (Copy (MyState, 1, 2)) = 'CT') OR
               (UpperCase (Copy (MyState, 1, 2)) = 'RI') THEN

                BEGIN
                ActiveDomesticMult := DomesticFile;
                DomesticQTHDataFileName := 'NEQSOW1.DOM';
                ActiveDXMult := ARRLDXCCWithNoUSACanadaKH6OrKL7;
                ActiveExchange := RSTDomesticOrDXQTHExchange;
                ContestName := 'New England QSO Party (within NE)';
                END
            ELSE
                BEGIN
                ActiveDomesticMult := DomesticFile;
                DomesticQTHDataFileName := 'NEQSO.DOM';
                ActiveDXMult := NoCountDXMults;
                ActiveExchange := RSTDomesticQTHExchange;
                ContestName := 'New England QSO Party (outside NE)';
                BigRemainingList := True;
                END;

            DXMultLimit := 20;
            ActiveQSOPointMethod := OnePhoneTwoCW;
            MultByBand := False;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := True;
            FoundContest := True;

            AddDomesticCountry ('K');
            AddDomesticCountry ('VE');
            AddDomesticCountry ('KH6');
            AddDomesticCountry ('KL');
            END;

        IF (CMD = 'NEW YORK QSO PARTY') OR (CMD = 'NY QSO') OR (CMD = 'NYQP') THEN
            BEGIN
            IF UpperCase (Copy (MyState, 1, 2)) = 'NY' THEN
                BEGIN
                ActiveDomesticMult := DomesticFile;
                DomesticQTHDataFileName := 'NYQPNY.DOM';
                ActiveDXMult := NoCountDXMults;
                ActiveExchange := RSTDomesticOrDXQTHExchange;
                END
            ELSE
                BEGIN
                ActiveDomesticMult := DomesticFile;
                DomesticQTHDataFileName := 'NYQP.DOM';
                ActiveExchange := RSTDomesticQTHExchange;
                END;

            ActiveQSOPointMethod := OnePhoneTwoCWThreeDigital;
            ContestName := 'New York QSO Party';
            MultByBand := False;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := True;
            FoundContest := True;

            AddDomesticCountry ('K');
            AddDomesticCountry ('VE');
            AddDomesticCountry ('KH6');
            AddDomesticCountry ('KL');
            END;



        IF CMD = 'NRAU BALTIC' THEN
            BEGIN
            ActiveBand := Band80;
            ActiveDomesticMult := DomesticFile;
            DomesticQTHDataFileName := 'NRAU.DOM';
            ActiveExchange := RSTQSONumberAndDomesticQTHExchange;
            ActiveQSOPointMethod := TwoPointsPerQSO;
            ContestName := 'NRAU Baltic Contest';
            MultByBand := True;
            QSOByBand  := True;
            FoundContest := True;
            END;


        IF CMD = 'OHIO QSO PARTY' THEN
            BEGIN
            ActiveDomesticMult := DomesticFile;
            ActiveQSOPointMethod := OnePhoneTwoCW;
            QSOByBand := True;
            QSOByMode := True;
            MultByBand := False;
            MultByMode := True;
            ContestName := 'OHIO QSO PARTY';

            IF UpperCase (Copy (MyState, 1, 2)) = 'OH' THEN
                BEGIN
                DomesticQTHDataFileName := 'OHIOOHIO.DOM';
                ActiveExchange := RSTQSONumberAndPossibleDomesticQTHExchange;
                END
            ELSE
                BEGIN
                DomesticQTHDataFileName := 'OHIOCTY.DOM';
                ActiveExchange := RSTQSONumberAndDomesticQTHExchange;
                END;

            AddDomesticCountry ('K');
            AddDomesticCountry ('VE');
            AddDomesticCountry ('KH6');
            AddDomesticCountry ('KL');
            FoundContest := True;
            END;

        IF CMD = 'OK DX' THEN
            BEGIN
            CountryTable.CountryMode := CQCountryMode;
            CountryTable.ZoneMode    := CQZoneMode;

            ActiveExchange := RSTAndQSONumberOrDomesticQTHExchange;
            ActiveQSOPointMethod := OKDXQSOPointMethod;
            AddDomesticCountry ('OK');
            AddDomesticCountry ('OM');
            ContestName := 'OK/OM DX Contest';

            IF (MyCountry <> 'OK') AND (MyCountry <> 'OM') THEN
                BEGIN
                ActiveDomesticMult := DomesticFile;
                DomesticQTHDataFileName := 'OKOM.DOM';
                MultByBand := True;
                MultByMode := True;
                SetUpRSTQSONumberExchange;
                END
            ELSE
                BEGIN
                ActivePrefixMult := Prefix;
                MultByBand := False;
                MultByMode := False;
                END;

            QSOByBand := True;
            QSOByMode := True;
            FoundContest := True;
            END;

        IF CMD = 'PACC' THEN
            BEGIN
            ActiveDomesticMult := DomesticFile;

            IF MyCountry = 'PA' THEN
                BEGIN
                ActiveDXMult := PACCCountriesAndPrefixes;
                ActiveExchange := RSTAndQSONumberOrDomesticQTHExchange;
                AddDomesticCountry ('PA');
                DomesticQTHDataFileName := 'PACCPA.DOM';
                LiteralDomesticQTH := True;
                END
            ELSE
                BEGIN
                ActiveExchange          := RSTDomesticQTHExchange;
                DomesticQTHDataFileName := 'PACC.DOM';
                END;

            ActiveQSOPointMethod := OnePointPerQSO;
            ContestName := 'PACC Contest';

            MultByBand := True;
            MultByMode := False;
            QSOByBand := True;
            QSOByMode := False;
            FoundContest := True;
            END;

        IF CMD = 'QCWA' THEN
            BEGIN
            ActiveExchange := QSONumberNameChapterAndQTHExchange;
            ActiveQSOPointMethod := OnePhoneTwoCW;
            AddDomesticCountry ('K');
            AddDomesticCountry ('KH6');
            AddDomesticCountry ('KL');

            ContestName := 'QCWA QSO Party';
            MultByBand := False;
            MultByMode := False;
            QSOByBand := True;
            QSOByMode := True;
            FoundContest := True;
            END;

        IF CMD = 'QCWA GOLDEN' THEN
            BEGIN
            ActiveExchange := CheckAndChapterOrQTHExchange;
            ActiveQSOPointMethod := OnePointPerQSO;

            ContestName := 'QCWA Golden Anniversary';
            MultByBand := False;
            MultByMode := False;
            QSOByBand := True;
            QSOByMode := True;
            FoundContest := True;
            END;

        IF CMD = 'RAC' THEN
            BEGIN
            ActiveExchange := RSTAndQSONumberOrDomesticQTHExchange;
            ActiveDomesticMult := DomesticFile;
            DomesticQTHDataFileName := 'P13.DOM';
            ActiveQSOPointMethod := RACQSOPointMethod;
            AddDomesticCountry ('VE');
            AddDomesticCountry ('CY0');
            AddDomesticCountry ('CY9');
            ContestName := 'RAC Winter Contest';
            MultByBand := True;
            MultByMode := True;
            QSOByBand := True;
            QSOByMode := True;
            VHFBandsEnabled := True;
            FoundContest := True;
            END;

        IF CMD = 'ROPOCO' THEN
            BEGIN
            ActiveBand := Band80;
            ActiveExchange := RSTAndPostalCodeExchange;
            ActiveQSOPointMethod := TenPointsPerQSO;
            ContestName := 'UK Rotating Postal Code';
            MultipleBandsEnabled := False;
            QSOByBand := True;
            FoundContest := True;

            CQExchange := '_~ %5NN ' + MyPostalCode;
            ExchangeFunctionKeyMenu := 'F1-DE+Cl  F2-Ex  F3-RST  F4-Postal F5-Cl+Ex  F8-EE  F9-?  Alt-AskforHis';
            RepeatSearchAndPounceExchange := '5NN (';
            SearchAndPounceExchange := '~ %5NN (';
            SetCQMemoryString (CW, F3, '5NN (');
            SetExMemoryString (CW, F3, '5NN');
            SetExMemoryString (CW, F4, '(');
            SetExMemoryString (CW, F5, '@ DE \ 5NN (');
            SetExMemoryString (CW, AltF3, 'RST?');
            SetExMemoryString (CW, AltF4, 'PC?');
            END;

        IF CMD = 'RUSSIAN DX' THEN
            BEGIN
            ActiveExchange := RSTDomesticQTHOrQSONumberExchange;
            ActiveDomesticMult := DomesticFile;
            ActiveDXMult := ARRLDXCC;
            AddDomesticCountry ('UA');
            AddDomesticCountry ('UA2');
            AddDomesticCountry ('UA9');
            AddDomesticCountry ('R1FJ');
            AddDomesticCountry ('R1MV');
            AddDomesticCountry ('CE9');

            CountDomesticCountries := True;

            DomesticQTHDataFileName := 'RUSSIAN.DOM';

            ActiveQSOPointMethod := RussianDXQSOPointMethod;
            ContestName := 'Russian DX Contest';
            MultByBand := True;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := True;
            LogFrequencyEnable := True;
            FoundContest := True;
            END;

        IF CMD = 'SALMON RUN' THEN
            BEGIN
            ActiveDomesticMult := DomesticFile;

            IF UpperCase (Copy (MyState, 1, 2)) = 'WA' THEN
                BEGIN
                DomesticQTHDataFileName := 'SALMONWA.DOM';
                ActiveExchange := RSTDomesticOrDXQTHExchange;
                ActiveDXMult := ARRLDXCCWithNoUSACanadaKH6OrKL7;
                END
            ELSE
                BEGIN
                DomesticQTHDataFileName := 'SALMON.DOM';
                ActiveExchange := RSTDomesticQTHExchange;
                END;

            ActiveQSOPointMethod := SalmonRunQSOPointMethod;

            ContestName := 'Washington State Salmon Run';

            MultByMode := True;
            MultByBand := False;
            QSOByMode := True;
            QSOByBand := True;

            BigRemainingList := True;

            AddDomesticCountry ('K');
            AddDomesticCountry ('VE');
            AddDomesticCountry ('KL');
            AddDomesticCountry ('KH6');

            FoundContest := True;
            END;

        IF (CMD = 'SCANDINAVIAN') OR (CMD = 'SAC') THEN
            BEGIN
            ActiveExchange := RSTQSONumberExchange;

            IF ScandinavianCountry (MyCountry) THEN
                ActiveDXMult := ARRLDXCC
            ELSE
                ActivePrefixMult := SACDistricts;

            ActiveQSOPointMethod := ScandinavianQSOPointMethod;
            ContestName := 'Scandinavian Contest';
            MultByBand := True;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := False;
            SetUpRSTQSONumberExchange;
            FoundContest := True;
            END;

        IF CMD = 'SP DX' THEN
            BEGIN
            ActiveDomesticMult := DomesticFile;
            ActiveExchange := RSTDomesticQTHExchange;
            ActiveQSOPointMethod := ThreePointsPerQSO;
            ContestName := 'SP-DX Contest';
            DomesticQTHDataFileName := 'SPDX.DOM';
            MultByBand := True;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := True;
            FoundContest := True;
            AddDomesticCountry ('SP');
            END;

        IF CMD = 'SPRINT' THEN
            BEGIN
            ActiveBand := Band20;
            ActiveExchange := QSONumberNameDomesticOrDXQTHExchange;
            ActiveDomesticMult := DomesticFile;
            ActiveDXMult := NorthAmericanARRLDXCCWithNoUSACanadaOrkL7;
            ActiveInitialExchange := NameQTHInitialExchange;
            ActiveQSOPointMethod := OnePointPerQSO;
            ContestName := 'North American Sprint';
            DomesticQTHDataFileName := 'S49P8.DOM';

            SetCQMemoryString (CW, AltF1,  'NA \ NA');

            CQExchange := '^\ # ' + MyName + ' ' + MyState;
            QSLMessage := 'TU';
            QuickQSLMessage1 := 'EE';
            QSOBeforeMessage := 'B4 \ NA';
            SearchAndPounceExchange := '@ # ' + MyName + ' ' + MyState + ' \';
            RepeatSearchAndPounceExchange := '# ' + MyName + ' ' + MyState;
            CorrectedCallMessage := '} R';

            SetCQMemoryString (CW, F1,  'NA \');
            SetCQMemoryString (CW, F2,  'CQ^NA CQ^NA \ \ NA');
            SetCQMemoryString (CW, F5,  '?');
            SetCQMemoryString (CW, F6,  'NA \ NA');
            SetCQMemoryString (CW, F7,  'CQ^NA \ \ NA');
            SetCQMemoryString (CW, F8,  'CQ^NA CQ^NA \ \ NA');
            SetCQMemoryString (CW, AltF1,  'NA \ \ NA');

            SetExMemoryString (CW, F3, 'NR #');
            SetExMemoryString (CW, F4, MyName);
            SetExMemoryString (CW, F5, MyState);
            SetExMemoryString (CW, F6, '@ \ NR^# ' + MyName + ' ' + MyState);
            SetExMemoryString (CW, F7,  'CQ^NA \ \ NA');
            SetExMemoryString (CW, F8,  'CQ^NA CQ^NA \ \ NA');
            SetExMemoryString (CW, AltF3, 'NR?');
            SetExMemoryString (CW, AltF4, 'NAME?');
            SetExMemoryString (CW, AltF5, 'QTH?');

            MultByBand := False;
            MultByMode := False;
            QSOByBand := True;
            QSOByMode := False;
            SprintQSYRule := True;
            VisibleDupeSheetEnable := True;
            FoundContest := True;

            AddDomesticCountry ('K');
            AddDomesticCountry ('VE');
            AddDomesticCountry ('KL');
            END;

        IF CMD = 'SWEEPSTAKES' THEN
            BEGIN
            ActiveDomesticMult := DomesticFile;
            ActiveExchange := QSONumberPrecedenceCheckDomesticQTHExchange;
            ActiveInitialExchange := CheckSectionInitialExchange;
            ActiveQSOPointMethod := TwoPointsPerQSO;
            CallsignUpdateEnable := True;
            ContestName := 'ARRL Sweepstakes';
            DomesticQTHDataFileName := 'ARRLSECT.DOM';
            MultByBand := False;
            MultByMode := False;
            QSOByBand  := False;
            QSOByMode  := True;
            FoundContest := True;

            AddARRLSectionDomesticCountries;

            SetCQMemoryString (CW, AltF1,  'CQ^SS \ SS');

            CQExchange := '_# ' + MyPrec + ' ' + MyCall + ' ' + MyCheck + ' ' + MySection;

            SearchAndPounceExchange := 'NR # ' + MyPrec + ' ' + MyCall + ' ' + MyCheck + ' ' + MySection;
            RepeatSearchAndPounceExchange := 'NR # ' + MyPrec + ' ' + MyCall + ' ' + MyCheck + ' ' + MySection;

            QSLMessage := '73 ' + MyCall + ' SS>';
            QSOBeforeMessage := 'SRI QSO ' + MyCall + ' SS';

            QuickQSLMessage1 := 'TU>';

            SearchAndPounceExchange := 'NR # ' + MyPrec + ' ' + MyCall + ' ' + MyCheck + ' ' + MySection;
            RepeatSearchAndPounceExchange := 'NR # ' + MyPrec + ' ' + MyCall + ' ' + MyCheck + ' ' + MySection;
            CorrectedCallMessage := '} R';

            SetCQMemoryString (CW, F1, 'SS ' + MyCall + ' SS>');
            SetCQMemoryString (CW, F2, 'CQ^SS ' + MyCall + ' ' + MyCall + ' SS>');
            SetCQMemoryString (CW, F3, 'CQ^SS CQ^SS ' + MyCall + ' ' + MyCall + ' SS>');
            SetCQMemoryString (CW, F7, 'CQ^SS ' + MyCall + ' SS');
            SetCQMemoryString (CW, F8, 'CQ^SS CQ^SS ' + MyCall + ' ' + MyCall + ' SS');

            SetCQMemoryString (CW, AltF1, 'SS ' + MyCall + ' SS');
            SetCQMemoryString (CW, AltF2, 'CQ^SS cq^ss ' + MyCall + ' ' + MyCall + ' SS');
            SetCQMemoryString (CW, AltF3, 'CQ^SS cq^ss ' + MyCall + ' ' + MyCall + ' SS');

            SetEXMemoryString (CW, AltF7, 'CQ^SS CQ^SS ' + MyCall + ' ' + MyCall + ' SS');

            SetEXMemoryString (CW, F3, 'NR #');
            SetEXMemoryString (CW, F4, MyPrec);
            SetEXMemoryString (CW, F5, MyCheck);
            SetEXMemoryString (CW, F6, MySection);
            SetEXMemoryString (CW, F7, 'CQ^SS ' + MyCall + ' SS');
            SetEXMemoryString (CW, F8, 'CQ^SS CQ^SS ' + MyCall + ' SS');

            SetEXMemoryString (CW, AltF3, 'NR?');
            SetEXMemoryString (CW, AltF4, 'PREC?');
            SetEXMemoryString (CW, AltF5, 'CK?');
            SetEXMemoryString (CW, AltF6, 'SEC?');
            SetEXMemoryString (CW, AltF7, 'CQ^SS CQ^SS ' + MyCall + ' ' + MyCall + ' SS');
            END;


        IF CMD = 'TEN TEN' THEN
            BEGIN
            ActiveExchange := NameQTHAndPossibleTenTenNumber;
            ActiveQSOPointMethod := TenTenQSOPointMethod;
            ContestName := 'Ten Ten QSO Party';
            MultByBand := False;
            MultByMode := False;
            QSOByBand := True;
            QSOByMode := True;
            FoundContest := True;
            AddDomesticCountry ('K');
            AddDomesticCountry ('VE');
            AddDomesticCountry ('KH6');
            AddDomesticCountry ('KL');
            END;

        IF CMD = 'TEXAS QSO PARTY' THEN
            BEGIN
            IF (UpperCase (Copy (MyState, 1, 3)) = 'TEX') OR (UpperCase (Copy (MyState, 1, 2)) = 'TX') THEN
                BEGIN
                ActiveDomesticMult := DomesticFile;
                ActiveDXMult       := ARRLDXCCWithNoUSACanadaKH6OrKL7;
                DomesticQTHDataFileName := 'TEXASTX.DOM';
                ActiveExchange := RSTDomesticOrDXQTHExchange;
                END
            ELSE
                BEGIN
                ActiveDomesticMult := DomesticFile;
                DomesticQTHDataFileName := 'TEXAS.DOM';
                ActiveExchange := RSTDomesticQTHExchange;
                END;

            ActiveQSOPointMethod := TwoPhoneThreeCW;
            ContestName := 'Texas QSO Party';
            MultByBand := False;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := True;
            FoundContest := True;

            AddDomesticCountry ('K');
            AddDomesticCountry ('VE');
            AddDomesticCountry ('KH6');
            AddDomesticCountry ('KL');
            END;

        IF CMD = 'TOEC' THEN
            BEGIN
            ActiveDomesticMult := GridFields;
            ActiveExchange := RSTDomesticQTHExchange;
            ActiveQSOPointMethod := TOECQSOPointMethod;
            ContestName := 'TOEC WW GRID CONTEST';
            MultByBand := True;
            MultByMode := False;
            QSOByBand := True;
            QSOByMode := False;
            FoundContest := True;
            END;

        IF CMD = 'UBA' THEN
            BEGIN
            ActiveDomesticMult := DomesticFile;
            AddDomesticCountry ('ON');
            ActiveDXMult := CQEuropeanCountries;
            ActiveExchange := RSTQSONumberAndPossibleDomesticQTHExchange;
            ActivePrefixMult := BelgiumPrefixes;
            ActiveQSOPointMethod := UBAQSOPointMethod;
            ContestName := 'UBA CONTEST';
            DomesticQTHDataFileName := 'UBA.DOM';

            MultByBand := True;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := False;

            FoundContest := True;
            END;


        IF CMD = 'UKRAINIAN' THEN
            BEGIN
            ActiveDXMult := CQDXCC;
            ActiveExchange := RSTAndQSONumberOrDomesticQTHExchange;
            ActiveQSOPointMethod := UkrainianQSOPointMethod;
            ContestName := 'Ukrainian Contest';
            DomesticQTHDataFileName := 'UKRAINE.DOM';
            MultByBand := True;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := True;
            AddDomesticCountry ('UR');
            FoundContest := True;
            END;

        IF (CMD = 'VK ZL') OR (CMD = 'OCEANIA') THEN
            BEGIN
            ActivePrefixMult := Prefix;
            ActiveExchange := RSTQSONumberExchange;
            ActiveQSOPointMethod := VKZLQSOPointMethod;
            ContestName := 'Oceania DX Contest';
            MultByBand := True;
            MultByMode := True;
            QSOByBand  := True;
            QSOByMode  := True;
            SetUpRSTQSONumberExchange;
            FoundContest := True;
            END;

        IF CMD = 'WAG' THEN
            BEGIN
            CountryTable.CountryMode := CQCountryMode;
            CountryTable.ZoneMode    := CQZoneMode;

            IF MyCountry = 'DL' THEN
                BEGIN
                ActiveDXMult := CQDXCC;
                ClrScr;
                TextColor (Yellow);
                WriteLn ('For the WAG contest - German stations will need to count Germany as a country');
                WriteLn ('multiplier manually after the contest.');
                WriteLn;
                WaitForKeyPressed;
                END
            ELSE
                ActiveDomesticMult := DOKCodes;

            LiteralDomesticQTH := True;

            ActiveExchange := RSTAndQSONumberOrDomesticQTHExchange;
            ActiveQSOPointMethod := WAGQSOPointMethod;

            AddDomesticCountry ('DL');

            MultByBand := True;
            MultByMode := False;
            QSOByBand := True;
            QSOByMode := True;

            ContestName := 'Worked All Germany Contest';
            FoundContest := True;
            END;

        IF CMD = 'WAE' THEN
            BEGIN
            CountryTable.CountryMode := CQCountryMode;
            CountryTable.ZoneMode    := CQZoneMode;

            IF MyContinent = Europe THEN
                ActiveDXMult := CQNonEuropeanCountriesAndWAECallRegions {KK1L: 6.70 Support for new (old) WAE rules}
            ELSE
                ActiveDXMult := CQEuropeanCountries;

            ActiveBand := Band80;
            ActiveExchange := RSTQSONumberExchange;
            ActiveQSOPointMethod := WAEQSOPointMethod;
            ContestName := 'WAE Contest';
            ContactsPerPage := 40;
            MultByMode := False;
            MultByBand := True;
            QSOByBand  := True;
            QSOByMode  := False;
            QTCsEnabled := True;
            SetUpRSTQSONumberExchange;
            FoundContest := True;
            END;

        IF (CMD = 'WQP') OR (CMD = 'WISCONSIN QSO PARTY') THEN
            BEGIN
            ActiveDomesticMult := DomesticFile;

            IF UpperCase (Copy (MyState, 1, 2)) = 'WI' THEN
                DomesticQTHDataFileName := 'WIQSOWI.DOM'
            ELSE
                DomesticQTHDataFileName := 'WIQSO.DOM';

            ActiveExchange := RSTDomesticQTHExchange;
            ActiveQSOPointMethod := OnePhoneTwoCW;
            ContestName := 'Wisconsin QSO Party';

            MultByBand := False;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := True;
            FoundContest := True;

            AddDomesticCountry ('K');
            AddDomesticCountry ('VE');
            AddDomesticCountry ('KH6');
            AddDomesticCountry ('KL');
            END;

        IF (CMD = 'WWL') OR (CMD = 'WORLD WIDE LOCATOR') THEN
            BEGIN
            ActiveDomesticMult := GridFields;
            ActiveExchange := RSTAndGridExchange;
            ActiveQSOPointMethod := WWLQSOPointMethod;
            ContestName := 'World Wide Locator Contest';
            MultByMode := False;
            MultByBand := True;
            QSOByMode := True;
            QSOByBand := True;
            FoundContest := True;
            END;

        IF CMD = 'XMAS' THEN
            BEGIN
            ActiveExchange := RSTQSONumberAndRandomCharactersExchange;
            ActiveQSOPointMethod := TwoPointsPerQSO;
            ContestName := 'SSA X-MAS Contest';
            LeadingZeros := 3;
            MultByMode := False;
            MultByBand := True;
            QSOByBand  := True;
            QSOByMode  := False;
            FoundContest := True;
            END;

        IF CMD = 'YO DX' THEN {KK1L: 6.71 Changed per new rules. Changed point method too.}
            BEGIN
            ActiveDomesticMult := DomesticFile;
            ActiveExchange := RSTAndQSONumberOrDomesticQTHExchange; {KK1L: 6.71}
            ActiveInitialExchange := NoInitialExchange; {KK1L: 6.71}
            ActiveQSOPointMethod := YODXQSOPointMethod;
            ActiveDXMult := ARRLDXCC; {KK1L: 6.71}
            ActiveZoneMult := NoZoneMults; {KK1L: 6.71}
            ContestName := 'YO DX Contest';
            DomesticQTHDataFileName := 'ROMANIA.DOM';
            MultByBand := True;
            MultByMode := False;
            QSOByBand  := True;
            QSOByMode  := False; {KK1L: 6.71}
            FoundContest := True;
            AddDomesticCountry ('YO');
            END;
        END;

    ContestTitle := GetYearString + ' ' + ContestName + ' ' + MyCall;
    Exit;
    END;



    BEGIN
    END.
