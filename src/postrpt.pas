UNIT PostRpt;

{$O+}
{$F+}
{$V-}

INTERFACE

Uses Tree,
     SlowTree,
     PostSubs,
     Country9,
     LogDupe,
     trCrt,
     LogWind,
     ZoneCont,
     PostMult;

FUNCTION ReportProcedureMenu: BOOLEAN;

CONST CallBufferSize = 1000;

TYPE ContestType = (UnknownContest,
                    AllAsian,
                    AllJA,
                    APSprint,
                    ARCI,
                    ARI,
                    ARRL10,
                    ARRL160,
                    ARRLDX,
                    ARRLRTTY,
                    ARRLVHFQSO,
                    ARRLVHFSS,
                    Baltic,
                    CalQSO,
                    County,
                    CQ160,
                    CQM,
                    CQVHF,
                    CQWPX,
                    CQWW,
                    CQWWRTTY,
                    Croatian,
                    EuropeanVHF,
                    EuropeanSprint,
                    EuropeanHFC,
                    FieldDay,
                    FQP,
                    General,
                    GridLoc,
                    HADX,
                    Helvetia,
                    IARU,
                    IntSprint,
                    IOTA,
                    JADX,
                    JALP, {KK1L: 6.72}
                    KCJ,
                    KidsDay,
                    KVP,
                    MIQP,
                    MQP,
                    NAQSO,
                    NEQSO,
                    NRAUBaltic,
                    NZFieldDay,
                    OceaniaVKZL,
                    OhioQP,
                    OKDX,
                    PACC,
                    QCWA,
                    RAC,
                    Region1FD,
                    Ropoco,
                    SAC,
                    SA,
                    SAWW,
                    Russian,
                    SalmonRun, {KK1L: 6.71}
                    SPDX,
                    Sprint,
                    SS,
                    StewPerry,
                    TenTen,
                    TexQSO,
                    Toec,
                    UBA,
                    Ukrainian,
                    WAE,
                    WAG,
                    WQP,
                    WWL,
                    WRTC,
                    WRTC2002, {KK1L: 6.68}
                    XMAS,
                    YODX);


    DupeSheetEntryRecordPointer = ^DupeSheetEntryRecord;

    DupeSheetEntryRecord = RECORD
        Call: CallString;
        Next: DupeSheetEntryRecordPointer;
        END;

    DupeSheetStartArrayType = ARRAY [-1..MaxCountries - 1] OF DupeSheetEntryRecordPointer;

    CallBufferPointer = ^CallBufferType;

    CallBufferType = ARRAY [0..CallBufferSize - 1] OF CallString;

VAR CallBuffer: CallBufferPointer;
    DupeSheetStartArray: DupeSheetStartArrayType;
    NumberCallsInCallBuffer: INTEGER;
    SummaryFileName: Str40; {KK1L: 6.72}


FUNCTION  GetCallAndContestFromLOGCFGFile (VAR MyCall: CallString;
                                           VAR Contest: Str40): BOOLEAN;

FUNCTION  DetermineContest (Name: Str80): ContestType;

PROCEDURE CalculateAppropriateTotalsForThisContest (Contest: ContestType;
                                                    MyQTH: QTHRecord);

PROCEDURE DetermineModesAndBandsUsed;
FUNCTION  DetermineNumberOfQTCs: INTEGER;


IMPLEMENTATION
uses dos,keycode,sysutils;

FUNCTION Number (Call: CallString): Char;

VAR CharPointer: INTEGER;

    BEGIN
    FOR CharPointer := 2 TO 4 DO
        IF Call [CharPointer] <= '9' THEN
            BEGIN
            Number := Call [CharPointer];
            Exit;
            END;

    Number := '/';
    END;



FUNCTION Suffix (Call: CallString): CallString;

VAR CharPointer: INTEGER;

    BEGIN
    FOR CharPointer := 2 TO 4 DO
        IF Call [CharPointer] <= '9' THEN
            BEGIN
            Delete (Call, 1, CharPointer);
            Suffix := Call;
            Exit;
            END;

    Suffix := Call;
    END;




PROCEDURE ProcessMultiplierTotals (Band: BandType;
                                   Mode: ModeType;
                                   DomesticMults: BOOLEAN;
                                   DXMults: BOOLEAN;
                                   PrefixMults: BOOLEAN;
                                   ZoneMults: BOOLEAN;
                                   FileString: Str80);

VAR NumDifMults, NumberMults, Mult: INTEGER;
    MultArray: ARRAY [1..2] OF Str20;
    MultString: Str80;
    MultIdentified: BOOLEAN;

    BEGIN
    NumDifMults := 0;

    IF DomesticMults THEN Inc (NumDifMults);
    IF DXMults       THEN Inc (NumDifMults);
    IF PrefixMults   THEN Inc (NumDifMults);
    IF ZoneMults     THEN Inc (NumDifMults);

    MultString := GetLogEntryMultString (FileString);

    IF MultString = '' THEN Exit;

    IF StringHas (MultString, ' ') THEN
        BEGIN
        MultArray [1] := PrecedingString  (MultString, ' ');
        MultArray [2] := PostcedingString (MultString, ' ');
        NumberMults := 2;
        END
    ELSE
        BEGIN
        MultArray [1] := MultString;
        NumberMults := 1;
        END;

    FOR Mult := 1 TO NumberMults DO
        BEGIN
        MultIdentified := False;

        IF DomesticMults THEN
            IF StringHasLowerCase (MultArray [Mult]) OR (NumDifMults = 1) THEN
                BEGIN
                MultIdentified := True;

                Inc (DomesticTotals [Band, Mode]);
                Inc (DomesticTotals [Band, Both]);
                Inc (DomesticTotals [All,  Mode]);
                Inc (DomesticTotals [All,  Both]);
                END;

        IF NOT MultIdentified THEN
            IF ZoneMults AND StringIsAllNumbers (MultArray [Mult]) THEN
                BEGIN
                Inc (ZoneTotals [Band, Mode]);
                Inc (ZoneTotals [Band, Both]);
                Inc (ZoneTotals [All,  Mode]);
                Inc (ZoneTotals [All,  Both]);
                MultIdentified := True;
                END;

        IF NOT MultIdentified THEN
            IF PrefixMults THEN
                BEGIN
                Inc (PrefixTotals [Band, Mode]);
                Inc (PrefixTotals [Band, Both]);
                Inc (PrefixTotals [All,  Mode]);
                Inc (PrefixTotals [All,  Both]);
                MultIdentified := True;
                END;

        IF NOT MultIdentified THEN
            IF DXMults THEN
                BEGIN
                Inc (DXTotals [Band, Mode]);
                Inc (DXTotals [Band, Both]);
                Inc (DXTotals [All,  Mode]);
                Inc (DXTotals [All,  Both]);
                END;
        END;
    END;



PROCEDURE CheckForNewContestDate (DateString: Str20);

VAR Index: INTEGER;

    BEGIN
    IF NumberDates = 0 THEN
        BEGIN
        ContestDates [0] := DateString;
        Inc (NumberDates);
        Exit;
        END;

    FOR Index := 0 TO NumberDates - 1 DO
        IF ContestDates [Index] = DateString THEN Exit;

    ContestDates [NumberDates] := DateString;
    Inc (NumberDates);

    IF NumberDates > 10 THEN
        BEGIN
        ReportError ('Too many contest dates!!');
        Halt;
        END;

    END;



FUNCTION CalculateTotals (DomesticMults: BOOLEAN;
                          DXMults:       BOOLEAN;
                          PrefixMults:   BOOLEAN;
                          ZoneMults:     BOOLEAN): BOOLEAN;

{ Returns TRUE if successful }

VAR FileRead: TEXT;
    FileString: Str80;
    Band: BandType;
    Mode: ModeType;
    QSOPoints: INTEGER;

    BEGIN
    NumberDates := 0;

    CalculateTotals := False;

    FOR Band := Band160 TO All DO
        FOR Mode := CW TO Both DO
            BEGIN
            QSOTotals      [Band, Mode] := 0;
            RawQSOTotals   [Band, Mode] := 0;
            QSOPointTotals [Band, Mode] := 0;
            DomesticTotals [Band, Mode] := 0;
            DXTotals       [Band, Mode] := 0;
            PrefixTotals   [Band, Mode] := 0;
            ZoneTotals     [Band, Mode] := 0;
            END;

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError ('Unable to open log file ' + LogFileName + '.');
        WaitForKeyPressed;
        Exit;
        END;

    WHILE NOT Eof (FileRead) DO
        BEGIN
        ReadLn (FileRead, FileString);

        Band := GetLogEntryBand (FileString);

        IF Band <> NoBand THEN
            BEGIN
            CheckForNewContestDate (GetLogEntryDateString (FileString));

            Mode := GetLogEntryMode (FileString);

            Inc (RawQSOTotals [Band, Mode]);
            Inc (RawQSOTotals [Band, Both]);
            Inc (RawQSOTotals [All,  Mode]);
            Inc (RawQSOTotals [All,  Both]);

            IF NOT (StringHas (UpperCase (FileString), '*DUPE*') OR
                   (StringHas (UpperCase (FileString), '*ZERO*'))) THEN
                       BEGIN
                       Inc (QSOTotals [Band, Mode]);
                       Inc (QSOTotals [Band, Both]);
                       Inc (QSOTotals [All,  Mode]);
                       Inc (QSOTotals [All,  Both]);

                       QSOPoints := GetLogEntryQSOPoints (FileString);

                       QSOPointTotals [Band, Mode] := QSOPointTotals [Band, Mode] + QSOPoints;
                       QSOPointTotals [Band, Both] := QSOPointTotals [Band, Both] + QSOPoints;
                       QSOPointTotals [All,  Mode] := QSOPointTotals [All,  Mode] + QSOPoints;
                       QSOPointTotals [All,  Both] := QSOPointTotals [All,  Both] + QSOPoints;

                       ProcessMultiplierTotals (Band, Mode,
                                                DomesticMults,
                                                DXMults,
                                                PrefixMults,
                                                ZoneMults,
                                                FileString);
                       END;

            GoToXY (1, WhereY);
            Write (QSOTotals [All, Both]);
            END;
        END;

    Close (FileRead);
    CalculateTotals := True;
    END;



FUNCTION GetCallAndContestFromLOGCFGFile (VAR MyCall: CallString;
                                          VAR Contest: Str40): BOOLEAN;

VAR FileRead: TEXT;
    FileName: Str20;
    FileString: Str160;

    BEGIN
    GetCallAndContestFromLOGCFGFile := False;

    IF LogFileName = 'LOG.DAT' THEN
        FileName := 'LOGCFG.DAT'
    ELSE
        FileName := PrecedingString (LogFileName, '.') + '.CFG';

    IF NOT OpenFileForRead (FileRead, FileName) THEN
        BEGIN
        ReportError (FileName + ' not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    WHILE NOT Eof (FileRead) DO
        BEGIN
        ReadLn (FileRead, FileString);
        ExpandTabs (FileString);

        WHILE Pos ('  ', FileString) <> 0 DO
            Delete (FileString, Pos ('  ', FileString), 1);

        FileString := UpperCase (FileString);

        IF Pos (';', Filestring) > 0 THEN
            FileString := PrecedingString (FileString, ';');

        IF StringHas (FileString, 'MY CALL') THEN
            BEGIN
            MyCall := PostcedingString (FileString, '=');
            GetRidOfPrecedingSpaces (MyCall);

            END;

        IF StringHas (FileString, 'CONTEST=') OR StringHas (FileString, 'CONTEST =') THEN
            BEGIN
            Contest := PostcedingString (FileString, '=');
            IF StringHas (Contest, ';') THEN Contest := PrecedingString (Contest, ';');
            GetRidOfPrecedingSpaces (Contest);

            GetCallAndContestFromLOGCFGFile := True;
            Close (FileRead);
            Exit;
            END;
        END;

    Close (FileRead);
    END;



FUNCTION DetermineContest (Name: Str80): ContestType;

    BEGIN
    DetermineContest := UnknownContest;
    Name := UpperCase (Name);

    IF Name = 'ALL ASIAN'     THEN DetermineContest := AllAsian;
    IF Name = 'ALL JA'        THEN DetermineContest := AllJA;
    IF Name = 'AP SPRINT'     THEN DetermineContest := APSprint;
    IF Name = 'ARCI'          THEN DetermineContest := ARCI;
    IF Name = 'ARI'           THEN DetermineContest := ARI;
    IF Name = 'ARRL 10'       THEN DetermineContest := ARRL10;
    IF Name = 'ARRL 160'      THEN DetermineContest := ARRL160;
    IF Name = 'ARRL DX'       THEN DetermineContest := ARRLDX;
    IF Name = 'ARRL RTTY ROUNDUP' THEN DetermineContest := ARRLRTTY;
    IF Name = 'ARRL VHF QSO'  THEN DetermineContest := ARRLVHFQSO;
    IF Name = 'ARRL VHF SS'   THEN DetermineContest := ARRLVHFSS;

    IF StringHas (Name, 'BALTIC') THEN DetermineContest := Baltic;

    IF (Name = 'CAL QSO PARTY') OR (Name = 'CALIFORNIA QSO PARTY') THEN
        DetermineContest := CalQSO;

    IF Name = 'COUNTY HUNTER' THEN DetermineContest := County;
    IF Name = 'CQ 160'        THEN DetermineContest := CQ160;
    IF Name = 'CQ M'          THEN DetermineContest := CQM;
    IF Name = 'CQ VHF'        THEN DetermineContest := CQVHF;
    IF Name = 'CQ WPX'        THEN DetermineContest := CQWPX;
    IF Name = 'CQ WW'         THEN DetermineContest := CQWW;
    IF Name = 'CQ WW RTTY'    THEN DetermineContest := CQWWRTTY;
    IF Name = 'CROATIAN'      THEN DetermineContest := Croatian;
    IF Name = 'EUROPEAN VHF'  THEN DetermineContest := EuropeanVHF;
    IF Name = 'EUROPEAN HFC'  THEN DetermineContest := EuropeanHFC;
    IF Name = 'FIELD DAY'     THEN DetermineContest := FieldDay;

    IF (Name = 'FQP') OR (Name = 'FLORIDA QSO PARTY') THEN
        DetermineContest := FQP;

    IF Name = 'GENERAL QSO'   THEN DetermineContest := General;
    IF Name = 'GRID LOC'      THEN DetermineContest := GridLoc;
    IF Name = 'HA DX'         THEN DetermineContest := HADX;
    IF Name = 'HELVETIA'      THEN DetermineContest := Helvetia;
    IF Name = 'IARU'          THEN DetermineContest := IARU;
    IF Name = 'IOTA'          THEN DetermineContest := IOTA;
    IF Name = 'KCJ'           THEN DetermineContest := KCJ;
    IF Name = 'KIDS DAY'      THEN DetermineContest := KidsDay;
    IF Name = 'KVP'           THEN DetermineContest := KVP;

    IF (Name = 'MICHIGAN QSO PARTY') OR (Name = 'MICH QSO PARTY') OR (Name = 'MI QSO PARTY') THEN
        DetermineContest := MIQP;

    IF (Name = 'MINNESOTA QSO PARTY') OR (Name = 'MINN QSO PARTY') OR (Name = 'MQP') THEN
        DetermineContest := MQP;

    IF Name = 'NA QSO'        THEN DetermineContest := NAQSO;

    IF (Name = 'NEW ENGLAND QSO') OR (Name = 'NEQSO') OR (Name = 'NEW ENGLAND QSO PARTY') THEN
        DetermineContest := NEQSO;

    IF StringHas (Name, 'NRAU') THEN DetermineContest := NRAUBaltic;

    IF Name = 'NZ FIELD DAY'  THEN DetermineContest := NZFieldDay;
    IF Name = 'OCEANIA'       THEN DetermineContest := OceaniaVKZL;
    IF Name = 'OHIO QSO PARTY' THEN DetermineContest := OhioQP;
    IF Name = 'OK DX'         THEN DetermineContest := OKDX;
    IF Name = 'PACC'          THEN DetermineContest := PACC;
    IF Name = 'QCWA'          THEN DetermineContest := QCWA;
    IF Name = 'RAC'           THEN DetermineContest := Rac;
    IF Name = 'ROPOCO'        THEN DetermineContest := Ropoco;
    IF Name = 'RUSSIAN DX'    THEN DetermineContest := Russian;
    IF Name = 'SALMON RUN'    THEN DetermineContest := SalmonRun; {KK1L: 6.71}
    IF Name = 'SP DX'         THEN DetermineContest := SPDX;
    IF Name = 'SPRINT'        THEN DetermineContest := Sprint;
    IF Name = 'STEW PERRY'    THEN DetermineContest := StewPerry;
    IF Name = 'SWEEPSTAKES'   THEN DetermineContest := SS;
    IF Name = 'TEN TEN'       THEN DetermineContest := TenTen;
    IF Name = 'TOEC'          THEN DetermineContest := Toec;
    IF Name = 'UBA'           THEN DetermineContest := UBA;
    IF Name = 'UKRAINIAN'     THEN DetermineContest := Ukrainian;
    IF Name = 'WAE'           THEN DetermineContest := WAE;
    IF Name = 'WAG'           THEN DetermineContest := WAG;
    IF Name = 'WQP'           THEN DetermineContest := WQP;
    IF Name = 'WWL'           THEN DetermineContest := WWL;
    IF Name = 'WRTC'          THEN DetermineContest := WRTC;
    IF Name = 'WRTC 2002'     THEN DetermineContest := WRTC2002; {KK1L: 6.68}
    IF Name = 'XMAS'          THEN DetermineContest := XMAS;
    IF Name = 'YO DX'         THEN DetermineContest := YODX;

    IF (Name = 'REGION ONE FIELD DAY') OR (Name = 'REGION 1 FIELD DAY') THEN
        DetermineContest := Region1FD;

    IF (Name = 'EU SPRINT') OR (Name = 'EUROPEAN SPRINT') THEN
        DetermineContest := EuropeanSprint;

    IF Name = 'INTERNET SPRINT'                  THEN DetermineContest := IntSprint;
    IF Name = 'JA INTERNATIONAL DX'              THEN DetermineContest := JADX;
    IF Name = 'JIDX'                             THEN DetermineContest := JADX;
    IF Name = 'JA LONG PREFECT'                  THEN DetermineContest := JALP; {KK1L: 6.72}
    IF (Name = 'SCANDINAVIAN') OR (Name = 'SAC') THEN DetermineContest := SAC;
    IF Name = 'SOUTH AMERICA WW'                 THEN DetermineContest := SAWW;
    IF Name = 'SOUTH AMERICAN'                   THEN DetermineContest := SA;
    IF Name = 'TEXAS QSO PARTY'                  THEN DetermineContest := TexQSO;
    IF Name = 'WISCONSIN QSO PARTY'              THEN DetermineContest := WQP;
    END;



PROCEDURE CalculateAppropriateTotalsForThisContest (Contest: ContestType;
                                                    MyQTH: QTHRecord);

    BEGIN
    CASE Contest OF

        AllAsian:
               IF MyQTH.Continent = Asia THEN
                   BEGIN
                   IF NOT CalculateTotals (False,  True, False, False) THEN Exit;
                   END
               ELSE
                   IF NOT CalculateTotals (False, False,  True, False) THEN Exit;


        AllJA:     IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        ARCI:      IF NOT CalculateTotals ( True,  True, False, False) THEN Exit;
        APSprint:  IF NOT CalculateTotals (False, False,  True, False) THEN Exit;
        ARI:       IF NOT CalculateTotals ( True,  True, False, False) THEN Exit;
        ARRL10:    IF NOT CalculateTotals ( True,  True, False, False) THEN Exit;
        ARRL160:   IF NOT CalculateTotals ( True,  True, False, False) THEN Exit;

        ARRLDX:
               IF (MyQTH.CountryID = 'K') OR (MyQTH.CountryID = 'VE') THEN
                   BEGIN
                   IF NOT CalculateTotals (False,  True, False, False) THEN Exit;
                   END
               ELSE
                   IF NOT CalculateTotals ( True, False, False, False) THEN Exit;

        ARRLRTTY:   IF NOT CalculateTotals ( True,  True, False, False) THEN Exit;
        ARRLVHFQSO: IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        ARRLVHFSS:  IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        Baltic:     IF NOT CalculateTotals (False, False, False, False) THEN Exit;

        CalQSO:    IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        County:    IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        CQ160:     IF NOT CalculateTotals ( True,  True, False, False) THEN Exit;
        CQM:       IF NOT CalculateTotals (False,  True, False, False) THEN Exit;
        CQVHF:     IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        CQWPX:     IF NOT CalculateTotals (False, False,  True, False) THEN Exit;
        CQWW:      IF NOT CalculateTotals (False,  True, False,  True) THEN Exit;
        CQWWRTTY:  IF NOT CalculateTotals ( True,  True, False,  True) THEN Exit;
        Croatian:  IF NOT CalculateTotals ( True,  True, False,  True) THEN Exit;

      EuropeanSprint:
                   IF NOT CalculateTotals (False, False, False, False) THEN Exit;

      EuropeanHFC: IF NOT CalculateTotals (False, False, False,  True) THEN Exit;
      EuropeanVHF: IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        FieldDay:  IF NOT CalculateTotals (False, False, False, False) THEN Exit;
        FQP:       IF NOT CalculateTotals ( True,  True, False, False) THEN Exit;
        General:   IF NOT CalculateTotals (False, False, False, False) THEN Exit;
        GridLoc:   IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        HADX:      IF NOT CalculateTotals ( True, False, False, False) THEN Exit;

        Helvetia:
               IF MyQTH.CountryID = 'HB' THEN
                   BEGIN
                   IF NOT CalculateTotals ( True,  True, False, False) THEN Exit;
                   END
               ELSE
                   IF NOT CalculateTotals ( True, False, False, False) THEN Exit;

        IARU:      IF NOT CalculateTotals ( True, False, False,  True) THEN Exit;
        IntSprint: IF NOT CalculateTotals (False, False, False, False) THEN Exit;
        IOTA:      IF NOT CalculateTotals ( True, False, False, False) THEN Exit;

        JADX:
               IF (MyQTH.CountryID = 'JA') OR (MyQTH.CountryID = 'JD/m') OR (MyQTH.CountryID = 'JD/o') THEN
                   BEGIN
                   IF NOT CalculateTotals (False,  True, False,  True) THEN Exit;
                   END
               ELSE
                   IF NOT CalculateTotals (True, False, False,  False) THEN Exit;

        JALP:      IF NOT CalculateTotals (True, False, False,  False) THEN Exit; {KK1L: 6.72}

        KCJ:       IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        KidsDay:   IF NOT CalculateTotals (False, False, False, False) THEN Exit;
        KVP:       IF NOT CalculateTotals (False, False, False,  True) THEN Exit;
        MIQP:      IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        MQP:       IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        NAQSO:     IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        NEQSO:     IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        NRAUBaltic: IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        NZFieldDay:
                   IF NOT CalculateTotals (False, False, False,  True) THEN Exit;

        OceaniaVKZL: IF NOT CalculateTotals (False, False,  True, False) THEN Exit;

        OhioQP:    IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        OKDX:      IF NOT CalculateTotals ( True,  True, False, False) THEN Exit;
        PACC:      IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        QCWA:      IF NOT CalculateTotals (False, False, False, False) THEN Exit;
        RAC:       IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        Region1FD: IF NOT CalculateTotals (False, False, False, False) THEN Exit;
        Ropoco:    IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        Russian:   IF NOT CalculateTotals ( True,  True, False, False) THEN Exit;

        SAC:
               IF ScandinavianCountry (MyQTH.CountryID) THEN
                   BEGIN
                   IF NOT CalculateTotals (False,  True, False, False) THEN Exit;
                   END
               ELSE
                   IF NOT CalculateTotals ( True, False, False, False) THEN Exit;

        SA:        IF NOT CalculateTotals (False, False,  True, False) THEN Exit;
        SAWW:      IF NOT CalculateTotals (False, False,  True, False) THEN Exit;
        SalmonRun: IF NOT CalculateTotals ( True, False, False, False) THEN Exit; {KK1L: 6.71}
        SPDX:      IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        Sprint:    IF NOT CalculateTotals ( True,  True, False, False) THEN Exit;
        SS:        IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        StewPerry: IF NOT CalculateTotals (False, False, False, False) THEN Exit;
        TenTen:    IF NOT CalculateTotals ( True,  True, False, False) THEN Exit;
        TexQSO:    IF NOT CalculateTotals ( True,  True, False, False) THEN Exit;
        TOEC:      IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        UBA:       IF NOT CalculateTotals ( True,  True,  True, False) THEN EXit;
        Ukrainian: IF NOT CalculateTotals ( True,  True, False, False) THEN Exit;
        WAE:       IF NOT CalculateTotals (False,  True, False, False) THEN Exit;
        WAG:       IF NOT CalculateTotals ( True,  True, False, False) THEN Exit;
        WQP:       IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        WWL:       IF NOT CalculateTotals ( True, False, False, False) THEN Exit;
        WRTC:      IF NOT CalculateTotals ( True,  True, False,  True) THEN Exit;
        WRTC2002:  IF NOT CalculateTotals ( True,  True, False, False) THEN Exit; {KK1L: 6.68}
        XMAS:      IF NOT CalculateTotals (False, False, False, False) THEN Exit;
        YODX:      IF NOT CalculateTotals ( True,  True, False,  True) THEN Exit;
        END;
    END;



PROCEDURE DetermineModesAndBandsUsed;

VAR Band: BandType;
    Mode: ModeType;

    BEGIN
    FOR Band := Band160 TO All DO
        FOR Mode := CW TO Phone DO
            BandModeUsed [Band, Mode] := RAWQSOTotals [Band, Mode] > 0;

    CWUsed      := RAWQSOTotals [All, CW] > 0;
    PhoneUsed   := RAWQSOTotals [All, Phone] > 0;
{   DigitalUsed := RAWQSOTotals [All, Digital] > 0;}
    END;




PROCEDURE WriteTitleBlockToSummarySheet (ContestName: Str80;
                                         Contest: ContestType;
                                         MyCall: CallString;
                                         MyQTH: QTHRecord);

VAR Directory, CityString, AddressString, NameString, TempString: Str80;
    FileRead, FileWrite: TEXT;
    Key: CHAR;
    Date: INTEGER;
    ReadAddress: BOOLEAN;

    BEGIN
    WriteLn (SummaryWrite);

    ContestName := ContestName + ' SUMMARY SHEET';

    WHILE Length (ContestName) < 80 DO
        ContestName := ' ' + ContestName + ' ';

    GetRidOfPostcedingSpaces (ContestName);

    WriteLn (SummaryWrite, ContestName);
    WriteLn (SummaryWrite);
    WriteLn (SummaryWrite);

    IF NumberDates = 1 THEN
        WriteLn (SummaryWrite, '     Contest Date : ' + ContestDates [0])
    ELSE
        BEGIN
        Write (SummaryWrite, '    Contest Dates : ');

        FOR Date := 0 TO NumberDates - 2 DO
            Write (SummaryWrite, ContestDates [Date] + ', ');

        WriteLn (SummaryWrite, ContestDates [NumberDates - 1]);
        END;

    WriteLn (SummaryWrite);
    WriteLn (SummaryWrite);

    WriteLn (SummaryWrite, '    Callsign Used : ', MyCall);

    TempString := GetResponse ('Operators : ');

    IF TempString <> '' THEN
        IF StringHas (TempString, ' ') THEN
            WriteLn (SummaryWrite, '        Operators : ', UpperCase (TempString))
        ELSE
            WriteLn (SummaryWrite, '         Operator : ', UpperCase (TempString));

    WriteLn (SummaryWrite);

    TempString := GetResponse ('Enter the category you are entering : ');

    IF TempString <> '' THEN
        BEGIN
        WriteLn (SummaryWrite, '         Category : ', TempString);
        WriteLn (SummaryWrite);
        END;

    IF Contest = IOTA THEN
        BEGIN
        REPEAT
            Key := UpCase (GetKey ('Are you a (H)ome station or (D)X-pedition? (H/D) : '));
        UNTIL (Key = 'H') OR (Key = 'D') OR (Key = EscapeKey);
        WriteLn;

        IF Key = 'H' THEN
            WriteLn (SummaryWrite, 'This effort was from my home station.');

        IF Key = 'D' THEN
            WriteLn (SummaryWrite, 'This was a DX Pedition operation.');

        END;

    TextColor (Cyan);
    WriteLn;
    WriteLn ('If you sent some information such as RST, section, age, zone or power for');
    WriteLn ('each QSO, it will be noted on your summary sheet as your default exchange.');
    WriteLn;

    TempString := GetResponse ('Enter the default exchange you sent if any : ');

    IF TempString <> '' THEN
        BEGIN
        WriteLn (SummaryWrite, ' Default Exchange : ', TempString);
        WriteLn (SummaryWrite);
        END;

    
    Directory := FindDirectory ('ADDRESS.DAT');

    IF Directory = '' THEN
       BEGIN
       Directory := GetEnv('HOME')+DirectorySeparator + '.trlog';
            IF NOT DirectoryExists(Directory) then
               BEGIN
               IF NOT CreateDir(Directory) then
                  WriteLn('Failed to create $HOME/.trlog');
               END;
       END;

    ReadAddress := OpenFileForRead (FileRead, Directory + DirectorySeparator
        + 'ADDRESS.DAT');
    If ReadAddress then
        BEGIN
        IF 'Y' = GetKeyResponse
         ('Okay to use previously stored name and address? (Y, N): ') THEN
        BEGIN
            WHILE NOT Eof (FileRead) DO
               BEGIN
               ReadLn (FileRead, TempString);
               WriteLn (SummaryWrite, TempString);
               END;
        END
        ELSE ReadAddress := False;

           Close (FileRead);
        END;
       
    IF NOT ReadAddress THEN
        BEGIN
        NameString := GetResponse ('Enter your full name : ');
        NameString := '             Name : ' + NameString;
        WriteLn (SummaryWrite, NameString);

        AddressString := GetResponse ('Enter your Address (not city/state) : ');
        AddressString := '          Address : ' + AddressString;
        WriteLn (SummaryWrite, AddressString);

        CityString := GetResponse ('Enter your City/State/Zip : ');
        CityString := '   City/State/Zip : ' + CityString;
        WriteLn (SummaryWrite, CityString);

        WriteLn;

        REPEAT
            Key := UpCase (GetKey ('Do you want to save this information for next time? (Y/N) : '));
        UNTIL (Key = 'Y') OR (Key = 'N');
        WriteLn;

        IF Key = 'Y' THEN
            BEGIN
            OpenFileForWrite (FileWrite, Directory + DirectorySeparator +
               'ADDRESS.DAT');
            WriteLn (FileWrite, NameString);
            WriteLn (FileWrite, AddressString);
            WriteLn (FileWrite, CityString);
            Close (FileWrite);
            END;
        END;

    WriteLn (SummaryWrite, '          Country : ', CountryTable.GetCountryName (MyQTH.Country));
    WriteLn (SummaryWrite);
    WriteLn (SummaryWrite);

    TempString := GetResponse ('Enter team or club if any : ');

    IF TempString <> '' THEN
        BEGIN
        WriteLn (SummaryWrite, '        Team/Club : ', TempString);
        WriteLn (SummaryWrite);
        END;
    END;



FUNCTION DetermineNumberOfQTCs: INTEGER;

{ This procedure will look at the QTC.DAT file and determine how many QTCs
  were sent or received. }

VAR FileRead: TEXT;
    FileString: Str80;
    QTCsThisStation, NumberQTCs: INTEGER;

    BEGIN
    NumberQTCs := 0;

    IF OpenFileForRead (FileRead, 'QTCLIST.DAT') THEN
        BEGIN
        WHILE NOT Eof (FileRead) DO
            BEGIN
            ReadLn (FileRead);

            IF NOT Eof (FileRead) THEN
                BEGIN
                ReadLn (FileRead, QTCsThisStation);
                NumberQTCs := NumberQTCs + QTCsThisStation;
                END;
            END;

        Close (FileRead);
        END;

    DetermineNumberOfQTCs := NumberQTCs;
    END;



PROCEDURE WriteScoreInformationToSummarySheet (Contest: ContestType;
                                               MyCall: CallString;
                                               MyQTH: QTHRecord);

VAR UnderlineString:  Str80;
    Index:            INTEGER;
    Band:             BandType;
    Mode:             ModeType;
    TotalMultipliers: LONGINT;
    TotalQSOPoints:   LONGINT;
    TotalQTCs:        LONGINT;
    TotalScore:       LONGINT;
    TempString:       Str80;

    BEGIN
    TempString := '   BAND   Raw QSOs   Valid QSOs   Points   ';

    IF DomesticTotals [All, Both] > 0 THEN TempString := TempString + 'Mults   ';
    IF       DXTotals [All, Both] > 0 THEN TempString := TempString + 'Countries   ';
    IF   PrefixTotals [All, Both] > 0 THEN TempString := TempString + 'Prefixes   ';
    IF     ZoneTotals [All, Both] > 0 THEN TempString := TempString + 'Zones';

    WriteLn (SummaryWrite);
    WriteLn (SummaryWrite, TempString);

    UnderlineString := '';

    FOR Index := 1 TO Length (TempString) DO
        UnderlineString := UnderlineString + '_';

    UnderlineString [1] := ' ';

    WriteLn (SummaryWrite, UnderlineString);
    WriteLn (SummaryWrite);

    FOR Band := Band160 TO BandLight DO
        FOR Mode := CW TO Phone DO
           IF RawQSOTotals [Band, Mode] > 0 THEN
               BEGIN
               Write (SummaryWrite, '  ', BandString [Band], ModeString [Mode]);

               Write (SummaryWrite, RawQSOTotals   [Band, Mode]:8);
               Write (SummaryWrite, QSOTotals      [Band, Mode]:12);
               Write (SummaryWrite, QSOPointTotals [Band, Mode]:11);
               Write (SummaryWrite, ' ');

               IF DomesticTotals [All, Both] > 0 THEN
                   Write (SummaryWrite, DomesticTotals [Band, Mode]:7, ' ');

               IF DXTotals [All, Both] > 0 THEN
                   Write (SummaryWrite, DXTotals [Band, Mode]:9, ' ');

               IF PrefixTotals [All, Both] > 0 THEN
                   Write (SummaryWrite, PrefixTotals [Band, Mode]:8, ' ');

               IF ZoneTotals [All, Both] > 0 THEN
                   Write (SummaryWrite, ZoneTotals [Band, Mode]:9);

               WriteLn (SummaryWrite);
               END;

    WriteLn (SummaryWrite, UnderlineString);
    WriteLn (SummaryWrite);

    Write   (SummaryWrite, ' Totals ');
    Write   (SummaryWrite, RawQSOTotals   [All, Both]:8);
    Write   (SummaryWrite, QSOTotals      [All, Both]:12);
    Write   (SummaryWrite, QSOPointTotals [All, Both]:11);
    Write   (SummaryWrite, ' ');

    IF DomesticTotals [All, Both] > 0 THEN
        Write (SummaryWrite, DomesticTotals [All, Both]:7, ' ');

    IF DXTotals [All, Both] > 0 THEN
        Write (SummaryWrite, DXTotals [All, Both]:9, ' ');

    IF PrefixTotals [All, Both] > 0 THEN
        Write (SummaryWrite, PrefixTotals [All, Both]:8, ' ');

    IF ZoneTotals [All, Both] > 0 THEN
        Write (SummaryWrite, ZoneTotals [All, Both]:9);

    WriteLn (SummaryWrite);
    WriteLn (SummaryWrite);
    WriteLn (SummaryWrite);

{ Now we compute the final score }

    TotalQSOPoints   := QSOPointTotals [All, Both];

    IF Contest = WAE THEN
        BEGIN
        TotalQTCs := DetermineNumberOfQTCs;    { Add into QSO points }

        IF TotalQTCs = 0 THEN
            BEGIN
            WriteLn (SummaryWrite, '    There were no QTC points.');
            WriteLn (SummaryWrite);
            END
        ELSE
            BEGIN
            WriteLn (SummaryWrite, '    There were ', TotalQTCs, ' QTC points.');
            WriteLn (SummaryWrite);
            END;

        TotalMultipliers := DXTotals [Band80, Both] * 4 +
                            DXTotals [Band40, Both] * 3 +
                            DXTotals [Band20, Both] * 2 +
                            DXTotals [Band15, Both] * 2 +
                            DXTotals [Band10, Both] * 2;
        END
    ELSE
        BEGIN
        TotalQTCs := 0;
        TotalMultipliers := DomesticTotals [All, Both] +
                            DXTotals       [All, Both] +
                            PrefixTotals   [All, Both] +
                            ZoneTotals     [All, Both];
        END;

    IF TotalMultipliers > 0 THEN
        TotalScore := (TotalQSOPoints + TotalQTCs) * TotalMultipliers
    ELSE
        TotalScore := TotalQSOPoints;

    WriteLn (SummaryWrite, '    Final Score = ', TotalScore, ' points.');
    WriteLn (SummaryWrite);
    WriteLn (SummaryWrite);
    END;



PROCEDURE WriteSoapBoxToSummarySheet;

VAR TempString: Str80;
    FirstLine: BOOLEAN;

    BEGIN
    FirstLine := True;
    TextColor (Cyan);
    WriteLn;
    WriteLn ('Enter as many soapbox comment lines as you want.  Press extra RETURN to stop.');

    TextColor (Yellow);

    REPEAT
        ReadLn (TempString);

        IF TempString <> '' THEN
            BEGIN
            IF FirstLine THEN
                BEGIN
                WriteLn (SummaryWrite, ' Soapbox Comments');
                WriteLn (SummaryWrite, ' ________________');
                WriteLn (SummaryWrite);
                FirstLine := False;
                END;

            WriteLn (SummaryWrite, ' ' + TempString);
            END;

    UNTIL TempString = '';

    IF NOT FirstLine THEN
        BEGIN
        WriteLn (SummaryWrite);
        WriteLn (SummaryWrite);
        END;
    END;



PROCEDURE WriteSignatureBlockToSummarySheet (Contest: ContestType);

    BEGIN
    WriteLn (SummaryWrite);

    CASE Contest OF

        CQWW, CQWPX, CQ160, CQVHF:
            BEGIN
            WriteLn (SummaryWrite, ' This is to certify that in the contest I have operated my transmitter within');
            WriteLn (SummaryWrite, ' the limitations of my license and have observed fully the rules and ');
            WriteLn (SummaryWrite, ' regulations of the contest.');
            END;

        Sprint:
            BEGIN
            WriteLn (SummaryWrite, 'I, by virtue of my statement below, have taken part in the NCJ North American');
            WriteLn (SummaryWrite, 'Sprint, conscientiously applying my most ethical interpretation of the rules');
            WriteLn (SummaryWrite, 'for this contest as set forth in the National Contest Journal.');
            END;

        ELSE
            BEGIN
            WriteLn (SummaryWrite, ' I have observed all competition rules as well as all regulations established');
            WriteLn (SummaryWrite, ' for amateur radio in my country.  My report is correct and true to the best');
            WriteLn (SummaryWrite, ' of my knowledge.  I agree to be bound by the decisions of the Awards Committee.');
            END;

        END;

    WriteLn (SummaryWrite);
    WriteLn (SummaryWrite);
    WriteLn (SummaryWrite);

    WriteLn (SummaryWrite, ' Date ______________   Signed _____________________________  Call ___________');
    END;



PROCEDURE SummarySheet;

VAR Contest:         ContestType;
    ContestName:     Str80;
    Key:             CHAR;
    MyCall:          CallString;
    MyQTH:           QTHRecord;
    TotalScore:      LONGINT;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('SUMMARY SHEET');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('This procedure will use the information in your LOG.DAT file to generate a ');
    WriteLn ('generic summary sheet.  The summary sheet will only be output to a file so');
    WriteLn ('you can make changes to it before printing it out.');
    WriteLn;
    WriteLn ('You need to run this procedure from the same directory you used during the ');
    WriteLn ('contest so that the log and config files are available.');
    WriteLn;
    WriteLn ('You should mark any dupes in your log before running this procedure.  Use the');
    WriteLn ('Dupe Log command under the LOG MENU to do this.');
    WriteLn;
    WriteLn ('If you have the file ADDRESS.DAT, the information in that file will be written');
    WriteLn ('on the summary sheet so your address shows up automatically.  You can also put');
    WriteLn ('your station description in this file.  If you don''t have this file, the');
    WriteLn ('program will prompt you for your address and create on.');
    WriteLn;

    IF NOT OkayToProceed THEN Exit;

    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('SUMMARY SHEET');
    WriteLn;
    TextColor (Cyan);

    IF NOT GetCallAndContestFromLOGCFGFile (MyCall, ContestName) THEN Exit;

    GetRidOfPostcedingSpaces (ContestName);

    Contest := DetermineContest (ContestName);

    IF Contest = UnknownContest THEN
        BEGIN
        ReportError (ContestName + ' is not yet supported.  Check for new updates.');
        WaitForKeyPressed;
        Exit;
        END;

    CountryTable.CountryMode := ARRLCountryMode;
    CountryTable.ZoneMode    := CQZoneMode;

    CASE Contest OF
        CQ160: CountryTable.CountryMode := CQCountryMode;
        CQVHF: CountryTable.CountryMode := CQCountryMode;
        CQWPX: CountryTable.CountryMode := CQCountryMode;
        CQWW:  CountryTable.CountryMode := CQCountryMode;
        IARU:  CountryTable.ZoneMode    := ITUZoneMode;
        OKDX:  CountryTable.CountryMode := CQCountryMode;
        WAE:   CountryTable.CountryMode := CQCountryMode;
        WAG:   CountryTable.CountryMode := CQCountryMode;
        YODX:  CountryTable.ZoneMode    := ITUZoneMode;
        END;

    LocateCall (MyCall, MyQTH, True);

    IF NOT CheckForTempFile THEN Exit;

    WriteLn;
    WriteLn ('Your call is ', MyCall, ' and the contest is ', ContestName, '.');
    WriteLn ('Your continent is ', GetContinentName (MyQTH.Continent));
    WriteLn ('Your country is ', CountryTable.GetCountryName (MyQTH.Country));
    WriteLn ('Your country ID prefix is ', MyQTH.CountryID);
    WriteLn;

    REPEAT
        Key := UpCase (GetKey ('Is this information correct? (Y/N): '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'Y') OR (Key = 'N');

    IF Key = 'N' THEN
        BEGIN
        GoToXY (1, WhereY);
        ClrEol;
        TextColor (Cyan);
        WriteLn ('Please correct your config file or your country files and then restart.');
        WaitForKeyPressed;
        Exit;
        END;

    WriteLn;

    TextColor (Cyan);
    CalculateAppropriateTotalsForThisContest (Contest, MyQTH);

    IF RawQSOTotals [All, Both] = 0 THEN
        BEGIN
        GoToXY (1, WhereY);
        ClrEol;
        WriteLn ('There were no contacts found in the active log file.');
        WaitForKeyPressed;
        Exit;
        END;

    Write (' QSOs were found in your log.');
    WriteLn;

    DetermineModesAndBandsUsed;


    SummaryFileName := PrecedingString (LogFileName, '.'); {KK1L: 6.72}
    IF SummaryFileName = 'LOG' THEN                        {KK1L: 6.72}
        SummaryFileName := 'SUMMARY.DAT'                   {KK1L: 6.72}
    ELSE                                                   {KK1L: 6.72}
        SummaryFileName := SummaryFileName + '.SUM';       {KK1L: 6.72}

    IF FileExists (SummaryFileName) THEN {KK1L: 6.72 changed from SUMMARY.DAT'}
        BEGIN
        REPEAT
            Key := UpCase (GetKey ('File ' + SummaryFileName + ' already exists.  Okay to overwrite? (Y/N): ')); {KK1L: 6.72}
        UNTIL (Key = EscapeKey) OR (Key = 'N') OR (Key = 'Y');
        WriteLn;
        IF Key <> 'Y' THEN Exit;
        END;

    OpenFileForWrite (SummaryWrite, SummaryFileName); {KK1L: 6.72 changed from SUMMARY.DAT'}

    WriteTitleBlockToSummarySheet (ContestName, Contest, MyCall, MyQTH);
    WriteScoreInformationToSummarySheet (Contest, MyCall, MyQTH);
    WriteSoapBoxToSummarySheet;
    WriteSignatureBlockToSummarySheet (Contest);

    Close (SummaryWrite);

    TextColor (Cyan);

    WriteLn;
    WriteLn ('Your summary sheet has been saved in the file SUMMARY.DAT');
    WriteLn ('If you need to make changes to it, this can be done with a text editor.');
    WriteLn;
    WaitForKeyPressed;
    END;



PROCEDURE TopOfForm (VAR FileWrite: TEXT);

    BEGIN
    Lines := 5;
    WriteLn (FileWrite);
    WriteLn (FileWrite, '           ', DupeSheetTitle);
    WriteLn (FileWrite);
    END;


PROCEDURE FormFeed (VAR FileWrite: TEXT);

    BEGIN
    Write (FileWrite, Chr (12));
    TopOfForm (FileWrite);
    END;



PROCEDURE PrintSomeCalls (PrintList: CallArrayPtr;
                          CallPointerList: CallPointerArrayPtr;
                          Finish: INTEGER;
                          VAR FileWrite: TEXT);

  VAR Index, Row, Col: INTEGER;

    BEGIN
    FOR Row := 0 TO NLines - 1 DO
        BEGIN
        FOR Col := 0 TO (Columns - 1) DO
            BEGIN
            Index := PrintBreak + Offset + (NLines * Col) + Row;

            IF Index > Finish THEN
                TempString := ' '
            ELSE
                TempString := PrintList^ [CallPointerList^ [Index]];

            TempString := TempString + '            ';
            TempString [0] := Chr (PrinterColumns DIV Columns);
            Write (FileWrite, TempString);
            END;

        WriteLn (FileWrite, '');
        END;
    END;



PROCEDURE PrintCalls (VAR CountryName: Str80;
                      VAR FileWrite: TEXT);

{ This procedure will print as many of the callsigns as it can on the
  current page. }

VAR Index, Row, Col, BytesToMove, ActualCallsPrinted: INTEGER;

    BEGIN
    { Determine how many lines we are going to print -> NLines.  If there
      needs to be a line feed before starting, make it so. }

    NCalls := NumberCallsInCallBuffer;
    NLines := (NCalls - 1) DIV Columns + 1;

    IF (NLines + Lines) > 55 THEN
        IF Lines > 45 THEN
            BEGIN
            FormFeed (FileWrite);
            IF NLines > 55 THEN   { Can't do more than 55 lines }
                NLines := 55;
            END
        ELSE
            NLines := 56 - Lines;

    { If this is the first time we are printing calls for this country,
      CountryName will not be blank and we will need to print the name
      for the country. }

    IF CountryName <> '' THEN
        BEGIN
        WriteLn (FileWrite);
        Inc (Lines);
        WriteLn (FileWrite, CountryName);
        Inc (Lines);
        END;

    FOR Row := 0 TO NLines - 1 DO
        BEGIN
        FOR Col := 0 TO (Columns - 1) DO
            BEGIN
            Index := (NLines * Col) + Row;

            IF Index >= NumberCallsInCallBuffer THEN
                TempString := ' '
            ELSE
                TempString := CallBuffer^ [Index];

            IF Col <> (Columns - 1) THEN
                BEGIN
                TempString := TempString + '            ';
                TempString := Copy (TempString, 1, PrinterColumns DIV Columns);
                END;

            Write (FileWrite, TempString);
            END;

        WriteLn (FileWrite, '');
        Inc (Lines);
        END;

    ActualCallsPrinted := NLines * Columns;

    IF ActualCallsPrinted >= NumberCallsInCallBuffer THEN
        BEGIN
        NumberCallsInCallBuffer := 0;
        Exit;
        END;

    { There are calls left in the buffer - we need to remove the ones
      we printed and leave things so more can be added and I get called
      again.

      First, clear out the name so I don't print the country name again }

    CountryName := '';

    BytesToMove := (NumberCallsInCallBuffer - ActualCallsPrinted) * SizeOf (CallBuffer^ [0]);

    Move (CallBuffer^ [ActualCallsPrinted], CallBuffer^ [0], BytesToMove);
    NumberCallsInCallBuffer := NumberCallsInCallBuffer - ActualCallsPrinted;
    END;



PROCEDURE LoadUpCallBufferWithSomeCalls (Country: INTEGER);

{ This routine will move as many callsigns as it can from the linked
  list for the country indicated into the CallBuffer array.  If it
  can't fit them all in, it will leave the DupeSheetStartArray [Country]
  pointint to the next record to be processed.

  It disposes of memory used by entries moved into the array. }


VAR NextRecord: DupeSheetEntryRecordPointer;

    BEGIN
    WHILE (DupeSheetStartArray [Country] <> nil) AND (NumberCallsInCallBuffer < CallBufferSize) DO
        BEGIN
        CallBuffer^ [NumberCallsInCallBuffer] := DupeSheetStartArray [Country]^.Call;
        Inc (NumberCallsInCallBuffer);

        NextRecord := DupeSheetStartArray [Country]^.Next;
        Dispose (DupeSheetStartArray [Country]);

        DupeSheetStartArray [Country] := NextRecord;
        END;
    END;



PROCEDURE PrintCountryCalls (Country: INTEGER;
                             VAR FileWrite: TEXT);

{ This procedure will print the calls found for the country indicated.  The
  records for the callsigns are disposed of as they are printed. }

VAR CountryName: Str20;

    BEGIN
    NumberCallsInCallBuffer := 0;

    CountryName := CountryTable.GetCountryName (Country);
    IF CountryName = '' THEN CountryName := 'Unknown countries';

    LoadUpCallBufferWithSomeCalls (Country);

    WHILE NumberCallsInCallBuffer > 0 DO
        BEGIN
        PrintCalls (CountryName, FileWrite);
        LoadUpCallBufferWithSomeCalls (Country);
        END;
    END;



PROCEDURE MakeDupeSheet (OutputFileName: Str80; Title: Str80);

{ This procedure will take the list of callsigns loaded into the call
  buffer and produce a dupe sheet with the filename indicated.  If the
  filename is LST:, then the list will be printed.  If it is null string,
  then the list is displayed. }

VAR Call, CountryID: CallString;
    Address, Country, NumberEntries: INTEGER;
    ActiveRecord, PreviousRecord, NewRecord: DupeSheetEntryRecordPointer;
    FileWrite: TEXT;

    BEGIN
    DupeSheetTitle := Title;
    NumberEntries := 0;

    IF NumberBufferCalls <= 0 THEN Exit;

    IF NOT OpenFileForWrite (FileWrite, OutputFileName) THEN
        BEGIN
        ReportError ('Unable to open output file.');
        WaitForKeyPressed;
        Exit;
        END;

    New (CallBuffer);

    FOR Address := -1 TO MaxCountries - 1 DO
        DupeSheetStartArray [Address] := nil;

    TextColor (Cyan);

    FOR Address := 0 TO NumberBufferCalls - 1 DO
        BEGIN
        IF Address MOD 10 = 0 THEN PinWheel;

        Call    := GetCall (Address);
        Country := CountryTable.GetCountry (Call, True);   { -1 = unknown }

        { First see if this is the first entry for this country }

        IF DupeSheetStartArray [Country] = nil THEN
            BEGIN
            DupeSheetStartArray [Country] := New (DupeSheetEntryRecordPointer);
            DupeSheetStartArray [Country]^.Call := Call;
            DupeSheetStartArray [Country]^.Next := nil;
            Inc (NumberEntries);
            Continue;
            END;

        { Look at the first entry to see if we should squeeze in at front }

        IF DupeSheetStartArray [Country]^.Call > Call THEN
            BEGIN
            ActiveRecord := DupeSheetStartArray [Country];
            DupeSheetStartArray [Country] := New (DupeSheetEntryRecordPointer);
            DupeSheetStartArray [Country]^.Call := Call;
            DupeSheetStartArray [Country]^.Next := ActiveRecord;
            Inc (NumberEntries);
            Continue;
            END;

        { If there is only one entry, put it at the end of the list }

        IF DupeSheetStartArray [Country]^.Next = nil THEN
            BEGIN
            ActiveRecord := New (DupeSheetEntryRecordPointer);
            ActiveRecord^.Call := Call;
            ActiveRecord^.Next := nil;
            DupeSheetStartArray [Country]^.Next := ActiveRecord;
            Inc (NumberEntries);
            Continue;
            END;

        { Look through the list and see if we can find a place for this call }

        PreviousRecord := DupeSheetStartArray [Country];
        ActiveRecord   := DupeSheetStartArray [Country]^.Next;

        REPEAT
            IF ActiveRecord^.Call > Call THEN
                BEGIN
                NewRecord := New (DupeSheetEntryRecordPointer);

                NewRecord^.Next := ActiveRecord;
                NewRecord^.Call := Call;

                PreviousRecord^.Next := NewRecord;
                Inc (NumberEntries);
                Break;
                END;

            IF ActiveRecord^.Call = Call THEN Break;  { Ignore dupes }

            IF ActiveRecord^.Next = nil THEN  { Reached end of list }
                BEGIN
                ActiveRecord^.Next := New (DupeSheetEntryRecordPointer);

                ActiveRecord := ActiveRecord^.Next;

                ActiveRecord^.Call := Call;
                ActiveRecord^.Next := nil;
                Inc (NumberEntries);
                Break;
                END;

            PreviousRecord := ActiveRecord;
            ActiveRecord   := ActiveRecord^.Next;
        UNTIL ActiveRecord = nil;
        END;

    GoToXY (1, WhereY);
    ClrEol;
    GoTOXY (1, WhereY - 1);
    ClrEol;
    Write ('Writing output...');

    NumberCountries := 0;

    TopOfForm (FileWrite);

    TextColor (Cyan);

    FOR Country := 0 TO CountryTable.NumberCountries - 1 DO
        IF DupeSheetStartArray [Country] <> nil THEN
            BEGIN
            GoToXY (1, WhereY);
            ClrEol;
            Write ('Printing calls for ', CountryTable.GetCountryId (Country));
            PrintCountryCalls (Country, FileWrite);
            Inc (NumberCountries);
            END;

    WriteLn (FileWrite, '');
    WriteLn (FileWrite, 'Number of different countries listed = ', NumberCountries, '.');

    Lines := Lines + 2;

    PrintCountryCalls (-1, FileWrite);  { Unknown countries }

    IF NumberEntries <> NumberBufferCalls THEN
        BEGIN
        Write (FileWrite, 'Number of callsigns = ', NumberEntries, '.  ');

        IF NumberBufferCalls - NumberEntries = 1 THEN
            Write (FileWrite, 'There was one dupe ignored.')
        ELSE
            Write (FileWrite, 'There were ', NumberBufferCalls - NumberEntries, ' dupes ignored.');
        END
    ELSE
        Write (FileWrite, 'Number of callsigns = ', NumberEntries, '.');

    Write   (FileWrite, Chr (12));
    Close   (FileWrite);
    Dispose (CallBuffer);
    END;



PROCEDURE BandChangeReport;

VAR LastBand, Band: BandType;
    LastMode, Mode: ModeType;
    FirstQSO: BOOLEAN;
    QSONumber, NumberBandChanges, NumberTwoXmtrQSOs, LastBandChangeQSO: INTEGER;
    Destination: CHAR;
    Title, FileName, TempString: Str80;
    FileRead, FileWrite: TEXT;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('BAND CHANGE REPORT PROCEDURE');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('This procedure will generate a report showing the when band changes were    ');
    WriteLn ('made.');
    WriteLn;

    REPEAT
    Destination := UpCase (GetKey ('Output to (F)ile, (P)rinter or (S)creen? : '));
        CASE Destination OF
            'F': BEGIN
                 WriteLn;
                 FileName := UpperCase (GetResponse ('Enter filename to save band change report to : '));

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

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError (LogFileName + ' file not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    ReadLn (FileRead, TempString);

    IF NOT OpenFileForWrite (FileWrite, FileName) THEN
        BEGIN
        ReportError ('Unable to open output file or device.');
        WaitForKeyPressed;
        Exit;
        END;

    IF FileName = '' THEN ClrScr;

    QSONumber := 0;
    NumberBandChanges := 0;
    LastBandChangeQSO := -100;
    NumberTwoXmtrQSOs := 0;

    Write (QSONumber);
    FirstQSO := True;

    REPEAT
        REPEAT
            ReadLn (FileRead, TempString);
            Band := GetLogEntryBand (TempString);
            Mode := GetLogEntryMode (TempString);
        UNTIL (Band <> NoBand) OR EOF (FileRead);

        IF FirstQSO THEN
            BEGIN
            LastBand := Band;
            LastMode := Mode;
            FirstQSO := False;
            END;

        Inc (QSONumber);
        GoToXY (1, WhereY);
        ClrEol;
        Write (QSONumber);

        IF (Band <> NoBand) THEN
            BEGIN
            IF (LastBand <> Band) OR (LastMode <> Mode) THEN
                BEGIN
                Inc (NumberBandChanges);
                Write (FileWrite, NumberBandChanges:4, '. ',
                       GetLogEntryQSONumber (TempString):5,
                       '  Band change from ',
                       BandString [LastBand], ModeString [LastMode], ' to ',
                       BandString [Band], ModeString [Mode], ' at ',
                       GetLogEntryTimeString (TempString), '.');

                IF LastBandChangeQSO + 1 = QSONumber THEN
                    BEGIN
                    Write (FileWrite, '  *');
                    Inc (NumberTwoXmtrQSOs);
                    END;

                WriteLn (FileWrite);
                LastBandChangeQSO := QSONumber;
                END;

            LastBand := Band;
            LastMode := Mode;
            END;

    UNTIL Eof (FileRead);

    IF NumberTwoXmtrQSOs > 0 THEN
        BEGIN
        WriteLn (FileWrite);
        WriteLn (FileWrite, 'There were ', NumberTwoXmtrQSOs, ' second radio QSOs made.');
        END;

    Close (FileRead);
    Close (FileWrite);
    GoToXY (1, WhereY);
    ClrEol;

    WriteLn;
    IF FileName = '' THEN WaitForKeyPressed;
    END;



PROCEDURE ContinentReport;

TYPE ContListType = (USA, Canada, NA, SA, EU, AF, AS, Japan, OC, UK);

CONST MaxUnknownCalls = 10;

VAR Band: BandType;
    ID, Call: CallString;
    Continent: ContinentType;
    Country:   INTEGER;
    CallAddress, QSONumber, AllBandTotal: INTEGER;
    Destination: CHAR;
    ContTotals:   ARRAY [BandType, ContListType] OF INTEGER;
    Cont, ListContinent: ContListType;
    QSOTotals:    ARRAY [BandType] OF INTEGER;
    UnknownCalls: ARRAY [BandType, 0..MaxUnknownCalls - 1] OF CallString;
    Title, FileName, TempString: Str80;
    DoingDupingFile: BOOLEAN;
    FileRead, FileWrite: TEXT;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('CONTINENT LIST PROCEDURE');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('This procedure will generate a report showing the distribution of your');
    WriteLn ('contacts by continent.');
    WriteLn;

    FOR Band := Band160 TO All DO
        BEGIN
        QSOTotals [Band] := 0;
        FOR Cont := USA TO UK DO ContTotals [Band, Cont] := 0;
        END;

    REPEAT
    Destination := UpCase (GetKey ('Output to (F)ile, (P)rinter or (S)creen? : '));
        CASE Destination OF
            'F': BEGIN
                 WriteLn;
                 FileName := UpperCase (GetResponse ('Enter filename to save continent report to : '));

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

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError (LogFileName + ' file not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    ReadLn (FileRead, TempString);

    IF TempString = 'DUPE' THEN
        BEGIN
        DoingDupingFile := True;
        ReadLn (FileRead, Title);
        END
    ELSE
        BEGIN
        DoingDupingFile := False;
        Close (FileRead);
        OpenFileForRead (FileRead, LogFileName);
        Title := GetResponse ('Enter continent report title : ');
        END;

    IF NOT OpenFileForWrite (FileWrite, FileName) THEN
        BEGIN
        ReportError ('Unable to open output file or device.');
        WaitForKeyPressed;
        Exit;
        END;

    IF FileName = '' THEN ClrScr;

    QSONumber := 0;
    Write (QSONumber);

    REPEAT
        REPEAT
            ReadLn (FileRead, TempString);
            Band := GetLogEntryBand (TempString);
        UNTIL (Band <> NoBand) OR EOF (FileRead) OR DoingDupingFile;

        Inc (QSONumber);
        GoToXY (1, WhereY);
        ClrEol;
        Write (QSONumber);

        IF (Band <> NoBand) OR DoingDupingFile THEN
          IF NOT ((StringHas (TempString, '*DUPE*')) OR (StringHas (TempString, '*ZERO*'))) THEN
            BEGIN
            IF DoingDupingFile THEN
                BEGIN
                Call := TempString;
                Band := Band160;
                END
            ELSE
                BEGIN
                Inc (QSOTotals [Band]);
                ExpandTabs (TempString);
                Call := StandardCallFormat (UpperCase (GetLogEntryCall (TempString)), True);
                END;

            Continent := CountryTable.GetContinent (Call);
            Country   := CountryTable.GetCountry   (Call, True);

            ID := CountryTable.GetCountryID (Country);

            IF ID = 'K' THEN
                ListContinent := USA
            ELSE
                IF ID = 'VE' THEN
                    ListContinent := Canada
                ELSE
                   IF ID = 'JA' THEN
                       ListContinent := Japan
                   ELSE
                       CASE Continent OF
                           NorthAmerica: ListContinent := NA;
                           SouthAmerica: ListContinent := SA;
                           Europe:       ListContinent := EU;
                           Africa:       ListContinent := AF;
                           Asia:         ListContinent := AS;
                           Oceania:      ListContinent := OC;
                           ELSE          ListContinent := UK;
                           END;

            Inc (ContTotals [Band, ListContinent]);

            IF ListContinent = UK THEN
                IF ContTotals [Band, UK] <= MaxUnknownCalls THEN
                    UnknownCalls [Band, ContTotals [Band, UK] - 1] := Call;
            END;

    UNTIL Eof (FileRead);
    Close (FileRead);
    GoToXY (1, WhereY);
    ClrEol;

    WriteLn (FileWrite);
    WriteLn (FileWrite, '      Continent List  ', Title);
    WriteLn (FileWrite);

    IF NOT DoingDupingFile THEN
        BEGIN
        WriteLn (FileWrite, '                   160    80    40    20    15    10    30    17    12    ALL');
        WriteLn (FileWrite, '                   ---    --    --    --    --    --    --    --    --    ---');
        END;

    Write (FileWrite, '  USA calls   = ');

    IF NOT DoingDupingFile THEN
        BEGIN
        AllBandTotal := 0;

        FOR Band := Band160 TO Band12 DO
            BEGIN
            Write (FileWrite, ContTotals [Band, USA]:6);
            AllBandTotal := ContTotals [Band, USA] + AllBandTotal;
            END;

        WriteLn (FileWrite, AllBandTotal:7);
        END
    ELSE
        WriteLn (FileWrite, ContTotals [Band160, USA]:6);

    Write (FileWrite, '  VE calls    = ');
    IF NOT DoingDupingFile THEN
        BEGIN
        AllBandTotal := 0;
        FOR Band := Band160 TO Band12 DO
            BEGIN
            Write (FileWrite, ContTotals [Band, Canada]:6);
            AllBandTotal := ContTotals [Band, Canada] + AllBandTotal;
            END;

        WriteLn (FileWrite, AllBandTotal:7);
        END
    ELSE
        WriteLn (FileWrite, ContTotals [Band160, Canada]:6);

    Write (FileWrite, '  N.A. calls  = ');
    IF NOT DoingDupingFile THEN
        BEGIN
        AllBandTotal := 0;
        FOR Band := Band160 TO Band12 DO
            BEGIN
            Write (FileWrite, ContTotals [Band, NA]:6);
            AllBandTotal := ContTotals [Band, NA] + AllBandTotal;
            END;
        WriteLn (FileWrite, AllBandTotal:7);
        END
    ELSE
        WriteLn (FileWrite, ContTotals [Band160, NA]:6);

    Write (FileWrite, '  S.A. calls  = ');
    IF NOT DoingDupingFile THEN
        BEGIN
        AllBandTotal := 0;
        FOR Band := Band160 TO Band12 DO
            BEGIN
            Write (FileWrite, ContTotals [Band, SA]:6);
            AllBandTotal := ContTotals [Band, SA] + AllBandTotal;
            END;
        WriteLn (FileWrite, AllBandTotal:7);
        END
    ELSE
        WriteLn (FileWrite, ContTotals [Band160, SA]:6);

    Write (FileWrite, '  Euro calls  = ');
    IF NOT DoingDupingFile THEN
        BEGIN
        AllBandTotal := 0;
        FOR Band := Band160 TO Band12 DO
            BEGIN
            Write (FileWrite, ContTotals [Band, EU]:6);
            AllBandTotal := ContTotals [Band, EU] + AllBandTotal;
            END;
        WriteLn (FileWrite, AllBandTotal:7);
        END
    ELSE
        WriteLn (FileWrite, ContTotals [Band160, EU]:6);

    Write (FileWrite, '  Afrc calls  = ');
    IF NOT DoingDupingFile THEN
        BEGIN
        AllBandTotal := 0;
        FOR Band := Band160 TO Band12 DO
            BEGIN
            Write (FileWrite, ContTotals [Band, AF]:6);
            AllBandTotal := ContTotals [Band, AF] + AllBandTotal;
            END;
        WriteLn (FileWrite, AllBandTotal:7);
        END
    ELSE
        WriteLn (FileWrite, ContTotals [Band160, AF]:6);

    Write (FileWrite, '  Asia calls  = ');
    IF NOT DoingDupingFile THEN
        BEGIN
        AllBandTotal := 0;
        FOR Band := Band160 TO Band12 DO
            BEGIN
            Write (FileWrite, ContTotals [Band, AS]:6);
            AllBandTotal := ContTotals [Band, AS] + AllBandTotal;
            END;
        WriteLn (FileWrite, AllBandTotal:7);
        END
    ELSE
        WriteLn (FileWrite, ContTotals [Band, AS]:6);

    Write (FileWrite, '  JA calls    = ');
    IF NOT DoingDupingFile THEN
        BEGIN
        AllBandTotal := 0;
        FOR Band := Band160 TO Band12 DO
            BEGIN
            Write (FileWrite, ContTotals [Band, Japan]:6);
            AllBandTotal := ContTotals [Band, Japan] + AllBandTotal;
            END;
        WriteLn (FileWrite, AllBandTotal:7);
        END
    ELSE
        WriteLn (FileWrite, ContTotals [Band160, Japan]:6);

    Write (FileWrite, '  Ocen calls  = ');
    IF NOT DoingDupingFile THEN
        BEGIN
        AllBandTotal := 0;
        FOR Band := Band160 TO Band12 DO
            BEGIN
            Write (FileWrite, ContTotals [Band, OC]:6);
            AllBandTotal := ContTotals [Band, OC] + AllBandTotal;
            END;
        WriteLn (FileWrite, AllBandTotal:7);
        END
    ELSE
        WriteLn (FileWrite, ContTotals [Band, OC]:6);

    WriteLn (FileWrite, '');

    Write (FileWrite, '  Unknowns    = ');
    IF NOT DoingDupingFile THEN
        BEGIN
        AllBandTotal := 0;
        FOR Band := Band160 TO Band12 DO
            BEGIN
            Write (FileWrite, ContTotals [Band, UK]:6);
            AllBandTotal := ContTotals [Band, UK] + AllBandTotal;
            END;
        WriteLn (FileWrite, AllBandTotal:7);
        END
    ELSE
        WriteLn (FileWrite, ContTotals [Band160, UK]:6);

    WriteLn (FileWrite, '');



    Write (FileWrite, '  Total calls = ');

    IF NOT DoingDupingFile THEN
        BEGIN
        AllBandTotal := 0;
        FOR Band := Band160 TO Band12 DO
            BEGIN
            Write (FileWrite, QSOTotals [Band]:6);
            AllBandTotal := QSOTotals [Band] + AllBandTotal;
            END;
        WriteLn (FileWrite, AllBandTotal:7);
        END
    ELSE
        WriteLn (FileWrite, QSOTotals [Band160]:6);

    WriteLn (FileWrite);

    FOR Band := Band160 TO Band12 DO
        IF ContTotals [Band, UK] > 0 THEN
            BEGIN
            TempString := BandString [Band];
            GetRidOfPrecedingSpaces (TempString);
            IF NOT DoingDupingFile THEN
                Write (FileWrite, 'Unknowns on ', TempString, ' = ')
            ELSE
                Write (FileWrite, 'Unknowns = ');

            FOR CallAddress := 0 TO ContTotals [Band, UK] - 1 DO
                IF CallAddress < MaxUnknownCalls THEN
                    Write (FileWrite, UnknownCalls [Band, CallAddress], ' ');

            WriteLn (FileWrite);
            END;

    Close (FileWrite);
    WriteLn;
    IF FileName = '' THEN WaitForKeyPressed;
    END;



PROCEDURE ViewNotes;

VAR Destination: CHAR;
    FileName, FileString: Str80;
    FileRead, FileWrite: TEXT;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('VIEW NOTES PROCEDURE');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('This procedure will generate a report showing all the note entries in your ');
    WriteLn ('log.  Notes start with a semicolon and are entered with the Control-N key in');
    WriteLn ('the logging program.  These notes will only appear in your raw log.  When you');
    WriteLn ('create your final log using the Create band and/or mode logs command from the');
    WriteLn ('Log menu, the notes will not be copied.');
    WriteLn;

    REPEAT
    Destination := UpCase (GetKey ('Output notes to (F)ile, (P)rinter or (S)creen? : '));
        CASE Destination OF
            'F': BEGIN
                 WriteLn;
                 FileName := UpperCase (GetResponse ('Enter filename to save notes to : '));

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

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError (LogFileName + ' file not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    IF NOT OpenFileForWrite (FileWrite, FileName) THEN
        BEGIN
        ReportError ('Unable to open output file or device.');
        WaitForKeyPressed;
        Exit;
        END;

    IF FileName = '' THEN ClrScr;

    WHILE NOT Eof (FileRead) DO
        BEGIN
        ReadLn (FileRead, FileString);
        IF Copy (FileString, 1, 1) = ';' THEN
            WriteLn (FileWrite, FileString);
        END;

    Close (FileRead);
    Close (FileWrite);
    WriteLn;

    IF FileName = '' THEN WaitForKeyPressed;
    END;



PROCEDURE DupeSheet;

{ Need to enhance this so it will do it by band and/or mode }

VAR Command, Destination: CHAR;
    TempString, TitleMaster, FileName: Str80;
    Mode: ModeType;
    Band: BandType;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('DUPE SHEET PROCEDURE');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('This procedure will generate dupesheets from the active log file.  You can');
    WriteLn ('create just one dupesheet that has all your contacts in it, or make multiple');
    WriteLn ('sheets for different bands and/or modes.  The sheets can be output to a file,');
    WriteLn ('the printer or to the screen.  You will be asked if you want the calls sorted');
    WriteLn ('by CQ countries.  If you answer no, they will be sorted into ARRL countries.');
    WriteLn ('Callsigns which the program isn''t sure what country they are in are listed at');
    WriteLn ('the end of the dupesheet.  If you are printing separate sheets for each band ');
    WriteLn ('or mode, you will be given the chance to print all the dupesheets or just one');
    WriteLn ('for a specific band or mode.');

    WriteLn;

    DetermineQSOByBandAndQSOByMode;

    IF NOT QSOByBandAndModeDetermined THEN Exit;

    WriteLn;
    TextColor (Cyan);
    WriteLn;
    WriteLn ('The dupesheet title will be printed at the top of each page.  It will have');
    WriteLn ('the band and mode appended to it.  Typically you would enter the name of the');
    WriteLn ('contest with a couple of spaces, followed by the callsign.  If you are saving');
    WriteLn ('dupesheets to files, you will find the files with .DS extensions');
    WriteLn;

    TitleMaster := GetResponse ('Enter dupsheet title : ');

    REPEAT
    Destination := UpCase (GetKey ('Output to (F)ile, (P)rinter or (S)creen? : '));
        CASE Destination OF
            'F': FileName := '';
            'S': FileName := '';
            'P': FileName := 'PRN';
            EscapeKey: Exit;
            END;
    UNTIL (Destination = 'F') OR (Destination = 'S') OR (Destination = 'P');
    WriteLn;

    REPEAT
        Key := UpCase (GetKey ('Use (A)RRL or (C)QWW countries? (A,C) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'A') OR (Key = 'C');
    WriteLn;

    CASE Key OF
        'A': CountryTable.CountryMode := ARRLCountryMode;
        'C': CountryTable.CountryMode := CQCountryMode;
        END;

    DetermineLogTotals;

    IF QSOByBand THEN
        BEGIN
        REPEAT
            Command := UpCase (GetKey ('Print sheets for (A)ll the bands or (S)ingle band? (A/S) : '));
            IF Command = EscapeKey THEN Exit;
        UNTIL (Command = 'A') OR (Command = 'S');
        WriteLn;

        IF Command = 'A' THEN
            BEGIN
            StartBand := Band160;
            StopBand  := BandLight;
            END
        ELSE
            BEGIN
            StartBand := NoBand;
            TempString := GetResponse ('Band (160 - 2, 222, 432, 902, 1296, 2304, 3GH, 5GH, 10G, 24G or LGT) : ');

            IF TempString = '' THEN Exit;

            IF TempString = '160'  THEN StartBand := Band160;
            IF TempString = '80'   THEN StartBand := Band80;
            IF TempString = '40'   THEN StartBand := Band40;
            IF TempString = '20'   THEN StartBand := Band20;
            IF TempString = '15'   THEN StartBand := Band15;
            IF TempString = '10'   THEN StartBand := Band10;
            IF TempString = '6'    THEN StartBand := Band6;
            IF TempString = '2'    THEN StartBand := Band2;
            IF TempString = '222'  THEN StartBand := Band222;
            IF TempString = '432'  THEN StartBand := Band432;
            IF TempString = '902'  THEN StartBand := Band902;
            IF TempString = '1296' THEN StartBand := Band1296;
            IF TempString = '2304' THEN StartBand := Band2304;
            IF TempString = '3GH'  THEN StartBand := Band3456;
            IF TempString = '5GH'  THEN StartBand := Band5760;
            IF TempString = '10G'  THEN StartBand := Band10G;
            IF TempString = '24G'  THEN StartBand := Band24G;
            IF TempString = 'LGT'  THEN StartBand := BandLight;

            IF StartBand = NoBand THEN Exit;
            StopBand := StartBand;
            END;
        END
    ELSE
        BEGIN
        StartBand := All;
        StopBand := All;
        END;

    IF QSOByMode THEN
        BEGIN
        REPEAT
            Command := UpCase (GetKey ('Print sheets for (B)oth modes (S)ingle mode? (B/S) : '));
            IF Command = EscapeKey THEN Exit;
        UNTIL (Command = 'B') OR (Command = 'S');
        WriteLn;

        IF Command = 'S' THEN
            BEGIN
            StartMode := NoMode;
            TempString := UpperCase (GetResponse ('Enter mode to print sheet for (CW or SSB) : '));
            IF TempString = '' THEN Exit;

            IF TempString = 'CW' THEN
                BEGIN
                StartMode := CW;
                StopMode := CW;
                END;

            IF TempString = 'DIG' THEN
                BEGIN
                StartMode := Digital;
                StopMode := Digital;
                END;

            IF (TempString = 'SSB') OR (TempString = 'PHONE') THEN
                BEGIN
                StartMode := Phone;
                StopMode := Phone;
                END;

            IF StartMode = NoMode THEN Exit;
            END;
        END
    ELSE
        BEGIN
        StartMode := Both;
        StopMode  := Both;
        END;

    FOR Mode := StartMode TO StopMode DO
        FOR Band := StartBand TO StopBand DO
            IF LogTotals [Band, Mode] > 0 THEN
                BEGIN
                IF PutLogFileIntoCallBuffer (Mode, Band) THEN
                    BEGIN
                    IF Band <> All THEN
                        DupeSheetTitle := TitleMaster + '  ' + BandString [Band]
                    ELSE
                        DupeSheetTitle := TitleMaster + '  All Bands';

                    CASE Mode OF
                        CW:    DupeSheetTitle := DupeSheetTitle + ' CW';
                        Phone: DupeSheetTitle := DupeSheetTitle + ' Phone';
                        Both:  DupeSheetTitle := DupeSheetTitle + ' Both modes';
                        END;

                    IF StringHas (DupeSheetTitle, 'BTH') OR StringHas (DupeSheetTitle, 'ALL') THEN
                        BEGIN
                        Delete (TempString, Length (TempString) - 2, 3);
                        TempString := TempString + ' Meters';
                        END;

                    IF Destination = 'F' THEN
                        BEGIN
                        FileName := DupingFileName (Band, Mode) + '.DS';

                        GoToXY (1, WhereY);
                        ClrEol;
                        WriteLn ('Writing to dupesheet file ', FileName, '...');
                        END;

                    MakeDupeSheet (FileName, DupeSheetTitle);
                    DisposeCallBuffer;

                    IF Destination = 'F' THEN
                        BEGIN
                        GoToXY (1, WhereY);
                        ClrEol;
                        END;
                    END
                ELSE
                    BEGIN
                    ReportError ('PutLogInFileBuffer did not work!!');
                    WaitForKeyPressed;
                    Exit;
                    END;
                END;

    WriteLn;
    IF FileName = '' THEN WaitForKeyPressed;
    END;



PROCEDURE MultLogEntries;

VAR FileRead, FileWrite: TEXT;
    MultString, FileString, FileName, TempString: Str80;
    Destination, BandChar, ModeChar, AlphabetizeChar: CHAR;
    MultByBand, MultByMode: BOOLEAN;
    SelectedBand, Band: BandType;
    SelectedMode, Mode: ModeType;
    NumberMults, Index, NumberCallsToSort, BubbleCount: INTEGER;
    Temp, MultAddress, NumberLinesPrinted: INTEGER;
    SortPointer: ARRAY [0..MaximumLogEntries] OF INTEGER;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('MULTIPLIER LOG ENTRY REPORT');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('This procedure will show the QSOs which are multipliers (as LOG.DAT entries');
    WriteLn ('You can have all the bands reported together or generate a report for a single');
    WriteLn ('band or mode.  You can leave them in log order or have them alphabetized if ');
    Writeln ('you wish.  This procedure will only show multipliers that are marked in your');
    WriteLn ('log with an entry in the multiplier column.  You can use the Multiplier Check');
    WriteLn ('procedure under the Log menu to verify the correct country multipliers are');
    WriteLn ('marked in your log based upon the callsign of each contact.');
    WriteLn;

    REPEAT
        BandChar := UpCase (GetKey ('(A)ll the bands together or select a (S)ingle band (A/S) : '));
    UNTIL (BandChar = EscapeKey) OR (BandChar = 'A') OR (BandChar = 'S');
    WriteLn;
    IF BandChar = EscapeKey THEN Exit;
    WriteLn;

    NumberMults  := 0;

    IF BandChar = 'S' THEN
        BEGIN
        SelectedBand := NoBand;
        TempString := GetResponse ('Band (160 - 2, 222, 432, 902, 1296, 2304, 3GH, 5GH, 10G, 24G or LGT) : ');
        IF TempString = '160' THEN SelectedBand := Band160;
        IF TempString = '80'  THEN SelectedBand := Band80;
        IF TempString = '40'  THEN SelectedBand := Band40;
        IF TempString = '20'  THEN SelectedBand := Band20;
        IF TempString = '15'  THEN SelectedBand := Band15;
        IF TempString = '10'  THEN SelectedBand := Band10;
        IF TempString =  '6'  THEN SelectedBand := Band6;
        IF TempString =  '2'  THEN SelectedBand := Band2;

        IF TempString =  '222'  THEN SelectedBand := Band222;
        IF TempString =  '432'  THEN SelectedBand := Band432;
        IF TempString =  '902'  THEN SelectedBand := Band902;
        IF TempString =  '1296' THEN SelectedBand := Band1296;
        IF TempString =  '2304' THEN SelectedBand := Band2304;

        IF TempString = '3GH'  THEN StartBand := Band3456;
        IF TempString = '5GH'  THEN StartBand := Band5760;
        IF TempString = '10G'  THEN StartBand := Band10G;
        IF TempString = '24G'  THEN StartBand := Band24G;
        IF TempString = 'LGT'  THEN StartBand := BandLight;

        IF SelectedBand = NoBand THEN Exit;
        END
    ELSE
        SelectedBand := All;

    REPEAT
        ModeChar := UpCase (GetKey ('(B)oth modes together or select a (S)ingle mode (B/S) : '));
    UNTIL (ModeChar = EscapeKey) OR (ModeChar = 'B') OR (ModeChar = 'S');
    WriteLn;

    IF ModeChar = EscapeKey THEN Exit;

    IF ModeChar = 'S' THEN
        BEGIN
        SelectedMode := NoMode;
        REPEAT
            TempString := UpperCase (GetResponse ('Enter desired mode (CW or SSB) : '));
            IF TempString = '' THEN Exit;
        UNTIL (TempString = 'CW') OR (TempString = 'SSB');

        IF TempString = 'CW'  THEN SelectedMode := CW;
        IF TempString = 'SSB' THEN SelectedMode := Phone;
        END
    ELSE
        SelectedMode := Both;

    NumberMults  := 0;

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

    REPEAT
        AlphabetizeChar := UpCase (GetKey ('Alphabetical or Log order? (A/L) : '));
    UNTIL (AlphabetizeChar = 'A') OR (AlphabetizeChar = 'L') OR (AlphabetizeChar = EscapeKey);
    WriteLn;

    IF AlphabetizeChar = EscapeKey THEN Exit;
    WriteLn;

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError (LogFileName + ' file not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    IF NOT OpenFileForWrite (FileWrite, FileName) THEN
        BEGIN
        ReportError ('Unable to open output file.');
        WaitForKeyPressed;
        Exit;
        END;

    IF FileName = '' THEN ClrScr;
    NumberLinesPrinted := 0;

    New (MultiplierArray);
    GoToXY (1, WhereY);
    TextColor (Cyan);
    ClrEol;
    Write ('Searching log file for multipliers...');

    REPEAT
        REPEAT
            ReadLn (FileRead, FileString);
            Band := GetLogEntryBand (FileString);
        UNTIL (Band <> NoBand) OR EOF (FileRead);

        IF (Band <> NoBand) AND
           ((Band = SelectedBand) OR (SelectedBand = All)) AND
           NOT (StringHas (FileString, '*DUPE*') OR StringHas (FileString, '*ZERO*')) THEN
               BEGIN
               Mode := GetLogEntryMode (FileString);

               IF (Mode <> NoMode) AND ((Mode = SelectedMode) OR (SelectedMode = Both)) THEN
                   BEGIN
                   ExpandTabs (FileString);
                   MultString := GetLogEntryMultString (FileString);
                   IF MultString <> '' THEN
                       BEGIN
                       WHILE StringHas (MultString, ' ') DO
                           BEGIN
                           TempString := PrecedingString (MultString, ' ');
                           MultiplierArray ^[NumberMults].LogEntry := FileString;
                           MultiplierArray ^[NumberMults].Mult     := TempString;
                           Inc (NumberMults);
                           MultString := PostcedingString (MultString, ' ');
                           END;

                       MultiplierArray ^[NumberMults].LogEntry := FileString;
                       MultiplierArray ^[NumberMults].Mult     := MultString;
                       Inc (NumberMults);
                       END;
                   END;
               END;

    UNTIL Eof (FileRead);
    Close (FileRead);

    GoToXY (1, WhereY);
    ClrEol;

    IF AlphabetizeChar = 'A' THEN
        BEGIN
        Write ('Sorting multipliers...');

        IF NumberMults > 1 THEN
            BEGIN
            NumberCallsToSort := NumberMults;

            FOR MultAddress := 0 TO NumberCallsToSort - 1 DO
                SortPointer [MultAddress] := MultAddress;

            Index := NumberCallsToSort - 2;

            FOR BubbleCount := 1 TO NumberCallsToSort - 1 DO
                BEGIN
                FOR MultAddress := 0 TO Index DO
                    IF MultiplierArray ^[SortPointer [MultAddress]].Mult >
                    MultiplierArray ^[SortPointer [MultAddress + 1]].Mult THEN
                        BEGIN
                        Temp := SortPointer [MultAddress + 1];
                        SortPointer [MultAddress + 1] := SortPointer [MultAddress];
                        SortPointer [MultAddress] := Temp;
                        END;

                Index := Index - 1;
                END;
            END
        ELSE
            SortPointer [0] := 0;

        GoToXY (1, WhereY);
        ClrEol;

        FOR MultAddress := 0 TO NumberMults - 1 DO
            BEGIN
            WriteLn (FileWrite, MultiplierArray ^[SortPointer [MultAddress]].LogEntry);
            Inc (NumberLinesPrinted);
            IF NumberLinesPrinted >= 50 THEN
                BEGIN
                NumberLinesPrinted := 0;
                WriteLn (FileWrite, '');
                END;
            END;
        END
    ELSE
        BEGIN
        FOR MultAddress := 0 TO NumberMults - 1 DO
            BEGIN
            WriteLn (FileWrite, MultiplierArray ^[MultAddress].LogEntry);
            Inc (NumberLinesPrinted);
            IF NumberLinesPrinted >= 50 THEN
                BEGIN
                NumberLinesPrinted := 0;
                WriteLn (FileWrite, '');
                END;
            END;
        END;

    Close (FileWrite);
    Dispose (MultiplierArray);
    IF FileName = '' THEN WaitForKeyPressed;
    END;




PROCEDURE QSODistribution;

LABEL QTHFound;

TYPE ListRec = RECORD
         QTH: CallString;
         Num: INTEGER;
         END;

VAR NumberQSOs, Count, CenterSpaces, NumberQTHs, Space, Address: INTEGER;
    HiTotal, HiAddress, QSONumber: INTEGER;
    Title, FileName, FileString, ExchangeString, QTHString: Str80;
    Band: BandType;
    Mode: ModeType;
    Destination: CHAR;
    FileRead, FileWrite: TEXT;
    List: ARRAY [0..300] OF ListRec;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('QSO DISTRIBUTION REPORT');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This procedure will generate a report showing the number of contacts made in');
    WriteLn ('each domestic QTH or zone (whichever is the last thing in the exchange field.');
    WriteLn;

    REPEAT
    Destination := UpCase (GetKey ('Output to (F)ile, (P)rinter or (S)creen? : '));
        CASE Destination OF
            'F': BEGIN
                 WriteLn;
                 FileName := UpperCase (GetResponse ('Enter filename to save report to : '));

                 IF FileName = UpperCase (LogFileName) THEN
                     BEGIN
                     ReportError ('Output file must be different than active log file!!');
                     WaitForKeyPressed;
                     Dispose (RateSheet);
                     Exit;
                     END;

                 IF FileName = '' THEN Exit;
                 END;
            'S': FileName := '';
            'P': FileName := 'PRN';

            EscapeKey:
                Exit;
            END;
    UNTIL (Destination = 'F') OR (Destination = 'S') OR (Destination = 'P');
    WriteLn;

    IF (Destination <> 'S') THEN
        BEGIN
        Title := GetResponse ('Enter title : ');
        CenterSpaces := 40 - (Length (Title) DIV 2);
        IF CenterSpaces > 0 THEN
            FOR Space := 1 TO CenterSpaces DO
                Title := ' ' + Title;
        END;

    NumberQTHs := 0;
    NumberQSOs := 0;

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN Exit;

    REPEAT
        REPEAT
            ReadLn (FileRead, FileString);
            Band := GetLogEntryBand (FileString);
        UNTIL (Band <> NoBand) OR EOF (FileRead);

        Inc (NumberQSOs);
        GoToXY (1, WhereY);
        Write (NumberQSOs);

        IF Band <> NoBand THEN
            IF NOT ((StringHas (FileString, 'DUPE')) OR (StringHas (FileString, 'ZERO'))) THEN
                BEGIN
                ExpandTabs (FileString);
                ExchangeString := GetLogEntryExchangeString (FileString);
                QTHString      := RemoveLastString (ExchangeString);

                IF NumberQTHs > 0 THEN
                    FOR Address := 0 TO NumberQTHs - 1 DO
                        IF List [Address].QTH = QTHString THEN
                            BEGIN
                            Inc (List [Address].Num);
                            GoTo QTHFound;
                            END;

                List [NumberQTHs].QTH := QTHString;
                List [NumberQTHs].Num := 1;
                Inc (NumberQTHs);

          QTHFound:
                END;

    UNTIL Eof (FileRead);

    Close (FileRead);

    GoToXY (1, WhereY);
    ClrEol;
    WriteLn ('There were ', NumberQSOs, ' QSOs found.');

    IF NumberQTHs = 0 THEN
        BEGIN
        ReportError ('No QTHs found.');
        WaitForKeyPressed;
        Exit;
        END;

    IF OpenFileForWrite (FileWrite, FileName) THEN

    IF (Destination <> 'S') THEN
        BEGIN
        WriteLn (FileWrite, Title);
        WriteLn (FileWrite);
        END;

    FOR Count := 1 TO NumberQTHs DO
        BEGIN
        HiTotal := 0;

        FOR Address := 0 TO NumberQTHs - 1 DO
            IF List [Address].Num > HiTotal THEN
                BEGIN
                HiTotal := List [Address].Num;
                HiAddress := Address;
                END;

        WriteLn (FileWrite, Count:4, '. ', List [HiAddress].QTH:12, List [HiAddress].Num : 5);
        List [HiAddress].Num := 0;
        END;

    Close (FileWrite);

    IF Destination = 'S' THEN WaitForKeyPressed;
    END;



PROCEDURE RateReport;

VAR QSONumber, FirstHourOfTheContest, LastHourOfTheContest, Hour: INTEGER;
    NumberDifferentBands, NumberHoursWithContacts: INTEGER;
    Day, LastDay: INTEGER;
    FileName, DayString, Title, TempString: Str80;
    Band: BandType;
    Mode: ModeType;
    CenterSpaces, Space, TotalContacts: INTEGER;
    UnderLineString: STRING [150];
    QSOTotals: ARRAY [BandType, ModeType] OF INTEGER;
    Destination: CHAR;
    FileRead, FileWrite: TEXT;

    BEGIN
    New (RateSheet);

    FOR Day := 1 TO 3 DO
        FOR Hour := 0 TO 23 DO
            FOR Band := Band160 TO All DO
                FOR Mode := CW TO Both DO
                    RateSheet^ [Day, Hour, Band, Mode] := 0;

    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('RATE SHEET REPORT');
    TextColor (Cyan);
    WriteLn;
    WriteLn ('This procedure will generate a rate sheet for the active log.  This sheet   ');
    WriteLn ('will show the band by band, hour by hour statistics.');
    WriteLn;
    REPEAT
    Destination := UpCase (GetKey ('Output to (F)ile, (P)rinter or (S)creen? : '));
        CASE Destination OF
            'F': BEGIN
                 WriteLn;
                 FileName := UpperCase (GetResponse ('Enter filename to save rate report to : '));

                 IF FileName = UpperCase (LogFileName) THEN
                     BEGIN
                     ReportError ('Output file must be different than active log file!!');
                     WaitForKeyPressed;
                     Dispose (RateSheet);
                     Exit;
                     END;

                 IF FileName = '' THEN
                     BEGIN
                     Dispose (RateSheet);
                     Exit;
                     END;
                 END;
            'S': FileName := '';
            'P': FileName := 'PRN';
            EscapeKey:
                BEGIN
                Dispose (RateSheet);
                Exit;
                END;
            END;
    UNTIL (Destination = 'F') OR (Destination = 'S') OR (Destination = 'P');
    WriteLn;

    IF (Destination <> 'S') THEN
        BEGIN
        Title := GetResponse ('Enter title : ');
        CenterSpaces := 40 - (Length (Title) DIV 2);
        IF CenterSpaces > 0 THEN
            FOR Space := 1 TO CenterSpaces DO
                Title := ' ' + Title;
        END;

    DetermineLogTotals;

    NumberDifferentBands := 0;

    FOR Band := Band160 TO BandLight DO
        FOR Mode := CW TO Phone DO
            BEGIN
            IF LogTotals [Band, Mode] > 0 THEN
                Inc (NumberDifferentBands);
            QSOTotals [Band, Mode] := 0;
            END;

    IF NumberDifferentBands = 0 THEN Exit;

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError (LogFileName + ' file not found!!');
        WaitForKeyPressed;
        Dispose (RateSheet);
        Exit;
        END;

    IF NOT OpenFileForWrite (FileWrite, FileName) THEN
        BEGIN
        ReportError ('Unable to open output file or device.');
        WaitForKeyPressed;
        Dispose (RateSheet);
        Exit;
        END;

    IF FileName = '' THEN
    begin
       ClrScr;
       gotoxy(1,3);
    end;

    IF Destination <> 'S' THEN
        BEGIN
        WriteLn (FileWrite);
        WriteLn (FileWrite, Title);
        WriteLn (FileWrite);
        END;

    Day := 0;

    QSONumber := 0;
    Write (QSONumber);

    REPEAT
        REPEAT
            ReadLn (FileRead, TempString);
            Band := GetLogEntryBand (TempString);
        UNTIL (Band <> NoBand) OR EOF (FileRead);

        IF Band <> NoBand THEN
          IF NOT ((StringHas (TempString, '*DUPE*')) OR (StringHas (TempString, '*ZERO*'))) THEN
            BEGIN
            ExpandTabs (TempString);
            Mode := GetLogEntryMode (TempString);
            LastHourOfTheContest := GetLogEntryHour (TempString);
            Inc (QSOTotals [Band, Mode]);

            CASE Day OF
                0: BEGIN
                   DayString := GetLogEntryDateString (TempString);
                   FirstHourOfTheContest := GetLogEntryHour (TempString);
                   Day := 1;
                   END;

                1: IF DayString <> GetLogEntryDateString (TempString) THEN
                       BEGIN
                       Day := 2;
                       DayString := GetLogEntryDateString (TempString);
                       END;

                2: IF DayString <> GetLogEntryDateString (TempString) THEN
                       Day := 3;
                END;

            Hour := GetLogEntryHour (TempString);

            Inc (RateSheet^ [Day, Hour, Band, Mode]);
            Inc (RateSheet^ [Day, Hour, All,  Both]);

            Inc (QSONumber);
            GoToXY (1, WhereY);
            Write (QSONumber);
            END;

    UNTIL Eof (FileRead);
    Close (FileRead);

    GoToXY (1, WhereY);
    ClrEol;

    IF QSONumber = 0 THEN Exit;
    TotalContacts := 0;

    LastDay := Day;

    UnderLineString := '';

    WriteLn (FileWrite);
    Write   (FileWrite, '  HOUR  ');
    UnderLineString :=  '  ----  ';

    FOR Band := Band160 TO BandLight DO
        FOR Mode := CW TO Phone DO
            IF LogTotals [Band, Mode] > 0 THEN
                BEGIN
                TempString := BandString [Band] + ModeString [Mode];
                CASE Length (TempString) OF
                    4: TempString := ' ' + TempString + ' ';
                    5: TempString := ' ' + TempString;
                    END;
                Write (FileWrite, TempString, '  ');
                UnderLineString := UnderLineString + '------  ';
                END;

    WriteLn (FileWrite, ' TOTAL   ACCUM');
//Write (FileWrite, ' TOTAL   ACCUM');
GoToXY (1, WhereY+1);
//WriteLn(FileWrite,'');
    UnderLineString := UnderLineString + ' -----   -----';
    WriteLn (FileWrite, UnderLineString);
GoToXY (1, WhereY+1);

    FOR Day := 1 TO LastDay DO
        FOR Hour := 0 TO 23 DO
            IF (((Day = 1) AND (Hour >= FirstHourOfTheContest)) OR (Day > 1)) AND
               (((Day = LastDay) AND (Hour <= LastHourOfTheContest)) OR (Day < LastDay)) THEN
                 BEGIN
                 Write (FileWrite, Hour:5);
                 FOR Band := Band160 TO BandLight DO
                     FOR Mode := CW TO Phone DO
                         IF LogTotals [Band, Mode] > 0 THEN
                             Write (FileWrite, RateSheet^ [Day, Hour, Band, Mode]:8);
                 Write (FileWrite, RateSheet^ [Day, Hour, All, Both]:8);
                 TotalContacts := TotalContacts + RateSheet^ [Day, Hour, All, Both];
                 WriteLn (Filewrite, TotalContacts:8);
//Write (Filewrite, TotalContacts:8);
GoToXY (1, WhereY+1);
//Writeln(Filewrite,'');
                 IF (Hour = 23) AND (Day <> LastDay) THEN
                 begin
                     WriteLn (FileWrite);
GoToXY (1, WhereY+1);
                 end;
                 END;

    WriteLn (FileWrite);
GoToXY (1, WhereY+1);
    Write   (FileWrite, '  TOTAL');

    FOR Band := Band160 TO BandLight DO
        FOR Mode := CW TO Phone DO
            IF LogTotals [Band, Mode] > 0 THEN
                Write (FileWrite, QSOTotals [Band, Mode]:6, '  ');

    Write (FileWrite, ControlL);
    Dispose (RateSheet);
    Close (FileWrite);
    WriteLn;
GoToXY (1, WhereY+1);
    IF FileName = '' THEN WaitForKeyPressed;
    END;



PROCEDURE ShowDupeReport;

VAR FileRead, FileWrite: TEXT;
    FileString, FileName: Str80;
    Destination: CHAR;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('SHOW DUPLICATE QSO PROCEDURE');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('This procedure will list the duplicate contacts that appear in your log.');
    WriteLn ('To make sure all your dupes are marked in your log, you need to execute the');
    WriteLn ('DUPE LOG procedure under LOG PROCEDURE menu before executing this procedure.');
    WriteLn;

    REPEAT
    Destination := UpCase (GetKey ('Output to (F)ile, (P)rinter or (S)creen? : '));
        CASE Destination OF
            'F': BEGIN
                 WriteLn;
                 FileName := UpperCase (GetResponse ('Enter filename to save dupe report to : '));

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

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError (LogFileName + ' file not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    IF FileName = '' THEN ClrScr;
    IF OpenFileForWrite (FileWrite, FileName) THEN
        BEGIN
        WHILE NOT Eof (FileRead) DO
            BEGIN
            ReadLn (FileRead, FileString);
            IF StringHas (FileString, 'DUPE') THEN
                WriteLn (FileWrite, FileString);
            END;
        Close (FileWrite);
        END
    ELSE
        BEGIN
        ReportError ('Unable to open output file or device.');
        Close (FileRead);
        WaitForKeyPressed;
        Exit;
        END;

    Close (FileRead);
    IF FileName = '' THEN WaitForKeyPressed;
    END;



PROCEDURE WPXPrefixReport;

VAR FileRead: TEXT;
    FileString, FileName: Str80;
    PrefixMult: Str20;
    Band: BandType;
    QSONumber: INTEGER;
    Destination: CHAR;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('WPX PREFIX DUPESHEET PROCEDURE');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('This procedure will generate a dupe sheet showing the prefix multipliers you');
    WriteLn ('have worked.');
    WriteLn;

    REPEAT
    Destination := UpCase (GetKey ('Output to (F)ile, (P)rinter or (S)creen? : '));
        CASE Destination OF
            'F': BEGIN
                 WriteLn;
                 FileName := UpperCase (GetResponse ('Enter filename to save WPX dupesheet to : '));

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
    Title := GetResponse ('Enter report title : ');

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError (LogFileName + ' file not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    DisposeCallBuffer;

    QSONumber := 0;
    Write (QSONumber);

    WHILE NOT Eof (FileRead) DO
        BEGIN
        ReadLn (FileRead, FileString);
        Band := GetLogEntryBand (FileString);

        IF Band <> NoBand THEN
            BEGIN
            PrefixMult := GetLogEntryMultString (FileString);
            IF PrefixMult <> '' THEN
                BEGIN
                PutCall (NumberBufferCalls, PrefixMult);
                Inc (NumberBufferCalls);
                END;

            Inc (QSONumber);
            GoToXY (1, WhereY);
            Write (QSONumber);
            END;
        END;

    Close (FileRead);
    GoToXY (1, WhereY);
    ClrEol;

    MakeDupeSheet (FileName, Title);
    IF FileName = '' THEN WaitForKeyPressed;
    END;



PROCEDURE ZoneReport;

VAR FileRead, FileWrite: TEXT;
    FileString, FileName: Str80;
    CallString, ZoneString: Str20;
    Band: BandType;
    Mode: ModeType;
    Location: QTHRecord;
    CQZones: BOOLEAN;
    QSONumber, NumberBadZones, Zone, Result: INTEGER;
    Destination, TempKey: CHAR;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('POSSIBLE BAD ZONE REPORT PROCEDURE');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('This procedure will list any contacts where the received zone differs from');
    WriteLn ('the default zone for the indicated callsign.  You should be aware that some');
    WriteLn ('of the more active stations have "special" default zones.  For example, the');
    WriteLn ('program knows that N4AR is in zone 4, not zone 5.');
    WriteLn;

    REPEAT
    Destination := UpCase (GetKey ('Output to (F)ile, (P)rinter or (S)creen? : '));
        CASE Destination OF
            'F': BEGIN
                 WriteLn;
                 FileName := UpperCase (GetResponse ('Enter filename to save bad zone report to : '));

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

    NumberBadZones := 0;

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError (LogFileName + ' file not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    IF FileName = '' THEN ClrScr;

    IF NOT OpenFileForWrite (FileWrite, FileName) THEN
        BEGIN
        ReportError ('Unable to open output file!!');
        WaitForKeyPressed;
        Exit;
        END;

    CQZones := Copy (UpperCase (GetResponse ('Use CQ zones (no for ITU)? : ')), 1, 1) = 'Y';

    IF CQZones THEN
        CountryTable.ZoneMode := CQZoneMode
    ELSE
        CountryTable.ZoneMode := ITUZoneMode;

    WriteLn;
    QSONumber := 0;

    WHILE NOT Eof (FileRead) DO
        BEGIN
        IF OperatorEscape THEN
            BEGIN
            Close (FileRead);
            WriteLn (FileWrite, 'Aborted by operator before end of file...');
            Close (FileWrite);
            Exit;
            END;

        ReadLn (FileRead, FileString);

        Band := GetLogEntryBand (FileString);

        IF Band <> NoBand THEN
            BEGIN
            Inc (QSONumber);
            GoToXY (1, WhereY);
            Write (QSONumber);
            CallString := GetLogEntryCall (FileString);
            LocateCall (CallString, Location, True);
            ZoneString := GetLogEntryExchangeString (FileString);
            Delete (ZoneString, 1, 11);
            GetRidOfPostcedingSpaces (ZoneString);
            Val (ZoneString, Zone, Result);

            IF Zone <> Location.Zone THEN
                BEGIN
                WriteLn (FileWrite, FileString);
                Inc (NumberBadZones);
                END;
            END;
        END;

    Close (FileRead);

    WriteLn;
    WriteLn (FileWrite, 'There were ', NumberBadZones, ' questionable zones found.');
    WriteLn (FileWrite);
    Close (FileWrite);
    IF FileName = '' THEN WaitForKeyPressed;
    END;



PROCEDURE ListMultipliersWorked;

CONST MaxEntries = 2000;

TYPE MultiplierTotalArrayType = ARRAY [BandType, ModeType] OF INTEGER;
     MultiplierTotalArrayPtr  = ^MultiplierTotalArrayType;

     MultiplierListType       = ARRAY [0..MaxEntries - 1] OF STRING [6];
     MultiplierListPtr        = ^MultiplierListType;

     MultiplierListMatrixType = ARRAY [BandType, ModeType] OF MultiplierListPtr;

VAR Key, Destination: CHAR;
    Band: BandType;
    Mode: ModeType;

    FileWrite, FileRead: TEXT;
    TempString, FileName, FileString: STRING;
    MultString: Str20;

    MultiplierTotalArray: MultiplierTotalArrayPtr;
    MultiplierListArray:  MultiplierListMatrixType;

    Range, Line, Entry, RequiredLines, Address, TotalLogQSOs: INTEGER;
    Change, Sort: BOOLEAN;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('LIST MULTIPLIERS WORKED');
    WriteLn;
    TextColor (Cyan);

    WriteLn ('This procedure will create a report showing which multipliers were worked');
    WriteLn ('in the contest.  All multiplier types will be shown.');

    WriteLn;

    DetermineMultByBandAndMultByMode;
    IF NOT MultByBandAndModeDetermined THEN Exit;

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

    REPEAT
        Key := UpCase (GetKey ('Do you want to sort the mults in alphabetical order? (Y/N) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'Y') OR (Key = 'N');
    WriteLn;

    Sort := Key = 'Y';

    IF NOT OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        ReportError (LogFileName + ' file not found!!');
        WaitForKeyPressed;
        Exit;
        END;

    Write ('Searching log file for multipliers...');

    New (MultiplierTotalArray);

    { Make sure all totals are zero }

    FOR Band := Band160 TO All DO     { Was StartBand to StopBand }
        FOR Mode := CW TO Both DO     { Was StartMode to StopMode }
            MultiplierTotalArray^ [Band, Mode] := 0;

    TotalLogQSOs := 0;

    REPEAT
        REPEAT
            ReadLn (FileRead, FileString);
            Band := GetLogEntryBand (FileString);
            Mode := GetLogEntryMode (FileString);
        UNTIL ((Band <> NoBand) AND (Mode <> NoMode)) OR EOF (FileRead);

       Inc (TotalLogQSOs);

       IF NOT MultByBand THEN Band := All;
       IF NOT MultByMode THEN Mode := Both;

       IF NOT (StringHas (FileString, '*DUPE*') OR StringHas (FileString, '*ZERO*')) THEN
           BEGIN
           ExpandTabs (FileString);
           MultString := GetLogEntryMultString (FileString);

           IF MultString <> '' THEN
               WHILE MultString <> '' DO
                   BEGIN
                   TempString := RemoveFirstString (MultString);

                   IF MultiplierTotalArray^ [Band, Mode] = 0 THEN
                       New (MultiplierListArray [Band, Mode]);

                   MultiplierListArray [Band, Mode]^ [MultiplierTotalArray^ [Band, Mode]] := TempString;

                   Inc (MultiplierTotalArray^ [Band, Mode]);
                   END;
           END;
    UNTIL Eof (FileRead);
    Close (FileRead);

    IF TotalLogQSOS > 0 THEN
        BEGIN
        OpenFileForWrite (FileWrite, FileName);

        WriteLn (FileWrite, Header);
        WriteLn (FileWrite);
        Lines := 2;

        FOR Band := Band160 TO All DO
            FOR Mode := CW TO Both DO
                IF MultiplierTotalArray^ [Band, Mode] > 0 THEN
                    BEGIN

                    { Do sort if needed }

                    IF Sort AND (MultiplierTotalArray^ [Band, Mode] > 1) THEN
                        BEGIN
                        Range := MultiplierTotalArray^ [Band, Mode] - 2;

                        REPEAT
                            Change := False;

                            FOR Address := 0 TO Range DO
                                IF MultiplierListArray [Band, Mode]^ [Address] >
                                   MultiplierListArray [Band, Mode]^ [Address + 1] THEN
                                       BEGIN
                                       TempString := MultiplierListArray [Band, Mode]^ [Address] ;
                                       MultiplierListArray [Band, Mode]^ [Address] :=
                                           MultiplierListArray [Band, Mode]^ [Address + 1];
                                       MultiplierListArray [Band, Mode]^ [Address + 1] := TempString;
                                       Change := True;
                                       END;

                            Dec (Range);
                        UNTIL (NOT Change) OR (Range < 0);
                        END;


                    { Compute number of lines needed }

                    RequiredLines := ( (MultiplierTotalArray^ [Band, Mode] - 1) DIV 10) + 1;

                    IF RequiredLines + Lines > 55 THEN
                        IF Destination <> 'S' THEN
                            BEGIN
                            WriteLn (FileWrite, '');
                            WriteLn (FileWrite, Header);
                            WriteLn (FileWrite);
                            Lines := 2;
                            END;

                    { Write band/mode header }

                    WriteLn (FileWrite, BandString [Band], ModeString [Mode]);

                    FOR Line := 0 TO RequiredLines - 1 DO
                        BEGIN
                        TempString := '';

                        FOR Entry := 0 TO 9 DO
                            BEGIN
                            Address := (Entry * RequiredLines) + Line;

                            IF Address < MultiplierTotalArray^ [Band, Mode] THEN
                                TempString := TempString + MultiplierListArray [Band, Mode]^ [Address];

                            IF Entry < 9 THEN
                                WHILE (Length (TempString) MOD 8) <> 0 DO
                                    TempString := TempString + ' ';
                            END;

                        WriteLn (FileWrite, TempString);
                        Inc (Lines);
                        END;

                    WriteLn (FileWrite);
                    Inc (Lines);

                    { Runtime 204 here - 05AD:B66A }

                    Dispose (MultiplierListArray [Band, Mode]);
                    END;

        END
    ELSE
        WriteLn (FileWrite, 'No QSOs found in log file.');

    Dispose (MultiplierTotalArray);

    Close (FileWrite);
    END;



FUNCTION ReportProcedureMenu: BOOLEAN;

VAR Key: CHAR;

    BEGIN
    ReportProcedureMenu := True;
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('REPORT PROCEDURE MENU');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('  B - Band change report.');
    WriteLn ('  C - Continent distribution report.');
    WriteLn ('  D - Dupe sheet.');
    WriteLn ('  E - Show QSOs that were dupes.');
    WriteLn ('  L - List Multipliers worked.');
    WriteLn ('  M - OH2MM CQ WW report menu.');
    WriteLn ('  R - Rate report (hour by hour, band by band summary).');
    WriteLn ('  Q - QSO distribution by domestic QTH or zone.');
    WriteLn ('  S - Summary sheet.');
    WriteLn ('  V - View any notes that are in your log (starting with ;).');
    WriteLn ('  W - WPX prefix report.');
    WriteLn ('  X - Exit report procedure menu.');
    WriteLn ('  Z - Zone report (shows zones that may be wrong in the log).');
    WriteLn;
    Write   ('  Enter command : ');

    REPEAT
        REPEAT UNTIL KeyPressed;
        Key := UpCase (ReadKey);

        CASE Key OF
            'B': BEGIN BandChangeReport;      Exit; END;
            'C': BEGIN ContinentReport;       Exit; END;
            'D': BEGIN DupeSheet;             Exit; END;
            'E': BEGIN ShowDupeReport;        Exit; END;
            'L': BEGIN ListMultipliersWorked; Exit; END;
            'M': BEGIN MultiplierReportMenu;  Exit; END;
            'Q': BEGIN QSODistribution;       Exit; END;
            'R': BEGIN RateReport;            Exit; END;
            'S': BEGIN SummarySheet;          Exit; END;
            'V': BEGIN ViewNotes;             Exit; END;
            'W': BEGIN WPXPrefixReport;       Exit; END;
            'Z': BEGIN ZoneReport;            Exit; END;

            'X', EscapeKey:
                 BEGIN
                 ReportProcedureMenu := False;
                 Exit;
                 END;
            END;
    UNTIL False;
    END;



    BEGIN
    END.
