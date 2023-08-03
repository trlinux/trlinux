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

UNIT LogDupe;

{ This unit contains the objects DupeSheet and MultSheet.  It has all
  of the methods required to use them. }

{$O+}
{$V-}

INTERFACE

USES LogDom, trCrt, Dos, SlowTree, Tree, Country9, ZoneCont, LogWind,
     LogHelp, radio, LogSCP, LogK1EA;



CONST
    DomesticMultArraySize          =  400;
    DXMultArraySize                =  500;
    MaxAllCalls                    = 7000;
    MaxGridSquaresInList           =   40;
    MaxLongPartialCalls            =  150;
    MaxVisDupeCallTotal            =   40;
    PrefixMultArraySize            = 1500;
    ZoneMultArraySize              =  100;

    { Note that the four byte and eight byte blocks must be the same
      size due to how the initial exchange stuff works.        }

    FourByteBlockSize    = 200;  { Used for both calls and prefixes }
    EightByteBlockSize   = 100;  { Used for calls > 6 characters }
    PartialCallBlockSize = 200;

    MaxCallBlocks        = 200;     { Per band mode }
    MaxBigCallBlocks     = 200;     { Total in dupesheet }
    MaxPartialCallBlocks = 100;

TYPE
    ExchangeInformationRecord = RECORD
        Age:           BOOLEAN;
        Chapter:       BOOLEAN;
        Check:         BOOLEAN;
        Classs:         BOOLEAN;
        Kids:          BOOLEAN;
        Name:          BOOLEAN;
        PostalCode:    BOOLEAN;
        Power:         BOOLEAN;
        Precedence:    BOOLEAN;
        QSONumber:     BOOLEAN;
        QTH:           BOOLEAN;
        RandomChars:   BOOLEAN;
        RST:           BOOLEAN;
        TenTenNum:     BOOLEAN;
        Zone:          BOOLEAN;
        ZoneOrSociety: BOOLEAN;
        END;

    CallDistrictRecord = RECORD
        List:  ARRAY [0..MaxVisDupeCallTotal] OF STRING [6];
        Total: INTEGER;
        END;

    VisibleDupesheet = ARRAY [1..10] OF CallDistrictRecord;

    ContestExchangeSourceType = (LocalQSO, ImportedQSO);

    ContestExchange = RECORD
          Age:            Str20;
          Band:           BandType;
          Callsign:       Str20;
          Chapter:        Str20;                      { QCWA Chapter }
          Check:          Str20;
          Classs:         Str20;                      { Field day class - extra s on purpose }
          Date:           Str20;                      { dd-mmm-yy }
          DomesticMult:   BOOLEAN;
          DomMultQTH:     DomesticMultiplierString;
          DomesticQTH:    Str20;
          DXMult:         BOOLEAN;
          DXQTH:          DXMultiplierString;         { Has the country prefix }
          Frequency:      LONGINT;
          InhibitMults:   BOOLEAN;
          Kids:           Str40;                      { Used for whole ex string }
          Mode:           ModeType;
          Name:           Str20;
          NameSent:       BOOLEAN;
          NumberReceived: INTEGER;
          NumberSent:     INTEGER;
          PostalCode:     Str20;
          Power:          Str20;
          Precedence:     Str20;
          Prefecture:     INTEGER;                    { Up to four characters }
          PrefixMult:     BOOLEAN;
          Prefix:         PrefixMultiplierString;
          QSOPoints:      INTEGER;
          QTH:            QTHRecord;                  { See ZoneCont }
          QTHString:      STRING [30];

          RandomCharsSent:     STRING [10];
          RandomCharsReceived: STRING [10];

          Radio:           RadioType;                 { Which radio made the QSO }
          RSTSent:         RSTString;
          RSTReceived:     RSTString;
          SearchAndPounce: BOOLEAN;
          Source:          ContestExchangeSourceType;
          TenTenNum:       LONGINT;
          Time:            INTEGER;                   { INTEGER time }
          TimeSeconds:     INTEGER;                   { Use with Time to get more resolution }
          Zone:            ZoneMultiplierString;
          ZoneMult:        BOOLEAN;
          END;

    StateType    = (NoStates, State48, State49, State50);
    ProvinceType = (NoProvinces, Province8, Province11, Province12, Province13);

    DXMultType  = (NoCountDXMults, NoDXMults, ARRLDXCCWithNoUSAOrCanada,
                   ARRLDXCCWithNoARRLSections,
                   ARRLDXCCWithNoUSACanadakH6OrkL7,
                   ARRLDXCCWithNoUSACanadakH6kL7OrXe,
                   ARRLDXCCWithNoIOrIS0, ARRLDXCC,
                   CQDXCC, CQDXCCWithNoUSAOrCanada,
                   CQDXCCWithNoHB9, CQDXCCWithNoOK,
                   CQEuropeanCountries, CQNonEuropeanCountries,
                   NorthAmericanARRLDXCCWithNoUSACanadaOrkL7,
                   NonSouthAmericanCountries, PACCCountriesAndPrefixes,
                   CQNonEuropeanCountriesAndWAECallRegions);

    PrefixMultType = (NoPrefixMults,
                      BelgiumPrefixes,
                      SACDistricts,
                      Prefix,
                      NonSouthAmericanPrefixes,
                      SouthAmericanPrefixes);

    DomesticMultArray = ARRAY [0..DomesticMultArraySize - 1] OF FourBytes;
    DXMultArray       = ARRAY [0..DXMultArraySize - 1] OF       FourBytes;
    PrefixMultArray   = ARRAY [0..PrefixMultArraySize - 1] OF   FourBytes;
    ZoneMultArray     = ARRAY [0..ZoneMultArraySize - 1] OF     FourBytes;

    DomesticMultArrayPtr = ^DomesticMultArray;
    DXMultArrayPtr       = ^DXMultArray;
    PrefixMultArrayPtr   = ^PrefixMultArray;
    ZoneMultArrayPtr     = ^ZoneMultArray;

    FourByteBlockArray  = ARRAY [0..FourByteBlockSize - 1]  OF FourBytes;
    EightByteBlockArray = ARRAY [0..EightByteBlockSize - 1] OF EightBytes;



    CallBlockPtr    = ^FourByteBlockArray;
    BigCallBlockPtr = ^EightByteBlockArray;
    FourBytePtr     = ^FourBytes;

    MultTotals = RECORD
        NumberDomesticMults: INTEGER;
        NumberDXMults:       INTEGER;
        NumberPrefixMults:   INTEGER;
        NumberZoneMults:     INTEGER;
        END;

    QSOTotalArray      = ARRAY [BandType, CW..Both] OF INTEGER;
    MultTotalArrayType = ARRAY [BandType, CW..Both] OF MultTotals;

    MultList = RECORD
        Totals:         MultTotalArrayType;
        DomesticList:   ARRAY [BandType, CW..Both] OF DomesticMultArrayPtr;
        DXList:         ARRAY [BandType, CW..Both] OF DXMultArrayPtr;
        PrefixList:     ARRAY [BandType, CW..Both] OF PrefixMultArrayPtr;
        ZoneList:       ARRAY [BandType, CW..Both] OF ZoneMultArrayPtr;
        END;

    DupeList = RECORD
        Totals: QSOTotalArray;
        NumberBigCalls: INTEGER;
        DupeList:    ARRAY [BandType, CW..Both, 1..MaxCallBlocks] OF CallBlockPtr;
        BigCallList: ARRAY [1..MaxBigCallBlocks] OF BigCallBlockPtr;
        END;

    VDEntry = RECORD
         Callsign:  STRING [6];
         NextEntry: POINTER;
         END;

    VDEntryPointer = ^VDEntry;


    CallDistrictTotalArray = ARRAY [1..11] OF INTEGER;



    DupeAndMultSheet = OBJECT
        DupeSheetEnable: BOOLEAN;

        DupeSheet:    DupeList;
        MultSheet:    MultList;

        FUNCTION  AddBigCallAddress (BigCall: EightBytes): INTEGER;
        PROCEDURE AddCallToVisibleDupeSheet (Callsign: CallString);
        PROCEDURE AddCompressedCallToDupeSheet (Call:FourBytes; Band: BandType; Mode: ModeType);

        PROCEDURE AddPossibleCallsFromDupesheet (Call: CallString; VAR List: CallListRecord);

        PROCEDURE AddQSOToSheets (RXData: ContestExchange);

        FUNCTION  CallIsADupe (Call: CallString; Band: BandType; Mode: ModeType): BOOLEAN;
        PROCEDURE CancelOutNewDomesticMultWeHaveWorked (MultString: Str20; Band: BandType; Mode: ModeType);
        PROCEDURE CancelOutNewDXMultWeHaveWorked (MultString: Str20; Band: BandType; Mode: ModeType);
        PROCEDURE CancelOutNewZoneMultWeHaveWorked (MultString: Str20; Band: BandType; Mode: ModeType);
        PROCEDURE CancelOutRemainingMultsWeHaveWorked (Band: BandType; Mode: ModeType; MultType: RemainingMultiplierType);
        PROCEDURE ClearDupeSheet;
        PROCEDURE CreateVisibleDupeSheetArrays (VAR Band: BandType; Mode: ModeType);

        PROCEDURE DisposeOfMemoryAndZeroTotals;
        PROCEDURE DupeSheetTotals (VAR Totals: QSOTotalArray);

        FUNCTION  EntryExists (Entry: FourBytes; Band: BandType; Mode: ModeType): BOOLEAN;
        PROCEDURE ExamineLogForQSOTotals (VAR QTotals: QSOTotalArray);

        PROCEDURE GeneratePartialCallListFromAllCallList (PartialCall: CallString;
                                                          Band: BandType;
                                                          Mode: ModeType;
                                                          VAR List: CallListRecord);

        FUNCTION  IsADomesticMult (Mult: Str10; Band: BandType; Mode: ModeType): BOOLEAN;

        PROCEDURE MultSheetTotals (VAR Totals: MultTotalArrayType);

        FUNCTION  ReadInBinFiles (JustDoIt: BOOLEAN): BOOLEAN;

        PROCEDURE SetMultFlags (VAR RXData: ContestExchange);
        PROCEDURE SetUpRemainingMultiplierArrays;
        PROCEDURE SaveRestartFile;
        PROCEDURE SheetInitAndLoad;
        END;



VAR

    { An alphabetical list of all of the calls in the dupesheets }

    AllCallList: ARRAY [0..MaxAllCalls - 1] OF CallString;

    { The following array has the exchange memory and the indexes match those
      for the AllCallList.  So, if yoy find K7RAT at address 34, you will
      find the exchange memory entry for hi in the ExchangeMemoryList at
      address 34. }

    ExchangeMemoryList: ARRAY [0..MaxAllCalls - 1] OF Str20;

    ActiveDXMult:           DXMultType;
    ActivePrefixMult:       PrefixMultType;
    AutoDupeEnableCQ:       BOOLEAN;
    AutoDupeEnableSAndP:    BOOLEAN;

    CallsignUpdateEnable:   BOOLEAN;
    CountDomesticCountries: BOOLEAN;

    DoingDomesticMults:      BOOLEAN;
    DoingDXMults:            BOOLEAN;
    DoingPrefixMults:        BOOLEAN;
    DoingZoneMults:          BOOLEAN;
    DomesticQTHDataFileName: Str40;

    ExchangeInformation:  ExchangeInformationRecord;
    ExchangeMemoryEnable: BOOLEAN;

    FirstVDEntry: VDEntryPointer;

    GridSquareList: ARRAY [0..MaxGridSquaresInList - 1] OF STRING [4];

    HFBandEnable: BOOLEAN;

    LoadingInLogFile:             BOOLEAN;

    MultByBand:      BOOLEAN;
    MultByMode:      BOOLEAN;
    MultiplierAlarm: BOOLEAN;

    NumberDifferentMults:    BYTE;
    NumberGridSquaresInList: BYTE;
    NumberAllCalls:          INTEGER;
    NumberPartialCalls:      INTEGER;
    NumberVDCalls:           INTEGER;

    OffTimeStart: TimeRecord;

    PartialCallEnable: BOOLEAN;  { This is a global variable that affects any/all partical call engines }

    { The PartialCallList is an alphabetical list of callsigns which each have an index to an initial exchange }

    PartialCallLoadLogEnable: BOOLEAN;

    QSOByBand: BOOLEAN;
    QSOByMode: BOOLEAN;
    QSOTotals: QSOTotalArray; { This may not also be exactly the same as
                                DupeList.Totals because of dupes read in }

    RemainingMultDisplay: RemainingMultiplierType;

    RemMultMatrix: ARRAY [Band160..NoBand,
                          CW..FM,
                          RemainingMultiplierType] OF RemainingMultListPointer;

    RestartVersionNumber: Str20;
    SingleBand: BandType;
    StartHour, StartMinute, StartSecond, StartSec100: WORD;

    TakingABreak: BOOLEAN;
    TotalOffTime: INTEGER;

    TimeElasped: ARRAY [1..20] OF LONGINT;

    TotalNamesSent: INTEGER;
    TotalQSOPoints: LongInt;

    WildCardPartials: BOOLEAN;

    PROCEDURE AddCallToAllCallList (Call: CallString; Exchange: Str20);

    FUNCTION  BigEntryAddress (Entry: FourBytes): INTEGER;

    PROCEDURE ClearContestExchange (VAR Exchange: ContestExchange);
    PROCEDURE ConvertBigEntryAddressToFourBytes (EntryPointer: INTEGER; VAR BigEntry: FourBytes);
    PROCEDURE CreateGridSquareList (Call: CallString; Band: BandType);

    PROCEDURE DupeInit;
    FUNCTION  FindProperAllCallListAddress (Call: CallString): INTEGER;
    FUNCTION  FoundDomesticQTH (VAR RXData: ContestExchange): BOOLEAN;

    PROCEDURE GetDXQTH (VAR RXData: ContestExchange);

    FUNCTION  GetInitialExchangeFromExchangeMemory (Call: CallString): STRING;

    FUNCTION  GetInitialExchangeStringFromContestExchange (RData: ContestExchange): Str40;
    PROCEDURE GetMultsFromLogEntry   (LogEntry: Str80; VAR RXData: ContestExchange);

    FUNCTION  ParseExchangeIntoContestExchange (LogEntry: STRING;
                                                VAR RXData: ContestExchange): BOOLEAN;

    FUNCTION  PointsToBigCall (Entry: FourBytes): BOOLEAN;

    PROCEDURE SetUpExchangeInformation (ActiveExchange: ExchangeType;
                                        VAR ExchangeInformation: ExchangeInformationRecord
                                        );

    PROCEDURE TransferLogEntryInfoToContestExchange (LogEntry: Str80; VAR RXData: ContestExchange);


IMPLEMENTATION

uses memlinux,keycode,beep;

{$I RemMults}



PROCEDURE CreateGridSquareList (Call: CallString; Band: BandType);

VAR FileRead: TEXT;
    TempString, FileString: Str80;

    BEGIN
    NumberGridSquaresInList := 0;

    IF OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        WHILE NOT Eof (FileRead) DO
            BEGIN
            ReadLn (FileRead, FileString);

            IF Band = GetLogEntryBand (FileString) THEN
                IF UpperCase (GetLogEntryCall (FileString)) = Call THEN
                    BEGIN
                    TempString := GetLogEntryExchangeString (FileString);

                    RemoveFirstString (TempString);
                    RemoveFirstString (TempString);

                    GridSquareList [NumberGridSquaresInList] := RemoveFirstString (TempString);
                    Inc (NumberGridSquaresInList);

                    IF NumberGridSquaresInList = MaxGridSquaresInList THEN
                        BEGIN
                        Close (FileRead);
                        Exit;
                        END;
                    END;
            END;

        Close (FileRead);
        END;

    IF OpenFileForRead (FileRead, 'LOG.TMP') THEN
        BEGIN
        WHILE NOT Eof (FileRead) DO
            BEGIN
            ReadLn (FileRead, FileString);

            IF Band = GetLogEntryBand (FileString) THEN
                IF UpperCase (GetLogEntryCall (FileString)) = Call THEN
                    BEGIN
                    TempString := GetLogEntryExchangeString (FileString);

                    RemoveFirstString (TempString);
                    RemoveFirstString (TempString);

                    GridSquareList [NumberGridSquaresInList] := RemoveFirstString (TempString);
                    Inc (NumberGridSquaresInList);
                    END;

            IF NumberGridSquaresInList = MaxGridSquaresInList THEN
                BEGIN
                Close (FileRead);
                Exit;
                END;
            END;

        Close (FileRead);
        END;
    END;



FUNCTION FoundDomesticQTH (VAR RXData: ContestExchange): BOOLEAN;

{ This function will look at the domestic QTH in the contest exchange and
  see if it can figure out what it is.  If so, it will be converted to
  the standard name for the QTH and a TRUE response generated.  Otherwise
  it will be cleared out and a FALSE response generated.  It looks at
  ActiveDomesticMultiplier to see what type of domestic mult it is.  }

VAR QTHString: Str40;

    BEGIN
    FoundDomesticQTH := False;

    IF RXData.QTHString = '' THEN Exit;

    QTHString := UpperCase (RXData.QTHString);

    IF StringHas (QTHString, '/') THEN QTHString := PrecedingString (QTHString, '/');

    GetRidOfPrecedingSpaces (QTHString);
    GetRidOfPostcedingSpaces (QTHString);

    FoundDomesticQTH := DomQTHTable.GetDomQTH (QTHString,
                                               RXData.DomMultQTH,
                                               RXData.DomesticQTH)
    END;



FUNCTION FindProperAllCallListAddress (Call: CallString): LONGINT;

{ Just needs to find the address where the entry before that address is
  < the Call, and the address above it is > the call.  It might end up
  pointing to the Call - or it might be one more than the Number of
  Calls.  It could also be zero which means it is less than the first
  call in the list. }

VAR StartAddress, StopAddress, JumpSize, TestAddress, Address: INTEGER;

    BEGIN
    { See if it should go at the first entry }

    IF (NumberAllCalls = 0) OR (Call < AllCallList [0]) THEN
        BEGIN
        FindProperAllCallListAddress := 0;
        Exit;
        END;

    { See if it should go to the last entry }

    IF Call > AllCallList [NumberAllCalls - 1] THEN
        BEGIN
        FindProperAllCallListAddress := NumberAllCalls;
        Exit;
        END;

    StartAddress := 0;
    StopAddress := NumberAllCalls - 1;

    IF NumberAllCalls > 20 THEN   { Let's play with the addresses to make this quicker }
        BEGIN
        TestAddress := NumberAllCalls - 1;
        JumpSize := NumberAllCalls DIV 2;

        TestAddress := TestAddress - JumpSize;

        { We start somewhere near the middle of the list }

        WHILE True DO
            BEGIN
            IF AllCallList [TestAddress] = Call THEN   { BINGO!! }
                BEGIN
                FindProperAllCallListAddress := TestAddress;
                Exit;
                END;

            JumpSize := JumpSize DIV 2;

            IF AllCallList [TestAddress] < Call THEN   { We need to move up the list }
                BEGIN
                IF (TestAddress + JumpSize >= NumberAllCalls - 1) OR (JumpSize < 10) THEN
                    BEGIN
                    StartAddress := TestAddress;
                    StopAddress := NumberAllCalls - 1;
                    Break;
                    END;

                TestAddress := TestAddress + JumpSize;
                END

            ELSE                                       { We need to move down the list }
                BEGIN
                IF (TestAddress - JumpSize <= 0) OR (JumpSize < 10) THEN
                    BEGIN
                    StartAddress := 0;
                    StopAddress := NumberAllCalls - 1;
                    Break;
                    END;

                TestAddress := TestAddress - JumpSize;
                END;
            END;  { of WHILE True }
        END;


    FOR Address := StartAddress TO StopAddress DO
        IF Call <= AllCallList [Address] THEN
            BEGIN
            FindProperAllCallListAddress := Address;
            Exit;
            END;

    { I really don't think we should ever get here }

    WriteLn ('Error in FindProperAllCallListAddress');
    Halt;
    END;



PROCEDURE AddCallToAllCallList (Call: CallString; Exchange: Str20);

VAR Address, ProperAddress: LONGINT;

    BEGIN
    ProperAddress := FindProperAllCallListAddress (Call);

    IF AllCallList [ProperAddress] = Call THEN    { Call already exists }
        BEGIN
        { Do we want to update the initial exchange? }
        Exit;
        END;

    IF NumberAllCalls = MaxAllCalls THEN Exit;  { No room }

    IF ProperAddress = NumberAllCalls THEN   { Add it to the end }
        BEGIN
        AllCallList [NumberAllCalls] := Call;
        ExchangeMemoryList [NumberAllCalls] := Exchange;
        Inc (NumberAllCalls);
        Exit;
        END;

    { Make room for the new entry }

    FOR Address := NumberAllCalls DOWNTO ProperAddress + 1 DO
        BEGIN
        AllCallList [Address] := AllCallList [Address - 1];
        ExchangeMemoryList [Address] := ExchangeMemoryList [Address - 1];
        END;

    AllCallList [ProperAddress] := Call;
    ExchangeMemoryList [ProperAddress] := Exchange;

    Inc (NumberAllCalls);
    END;



FUNCTION GetInitialExchangeFromExchangeMemory (Call: CallString): STRING;

{ This procedure will return the initial exchange for the callsign passed
  to it.  If there is no initial exchange, a null string will be returned. }

VAR Address: INTEGER;
    AddressString: Str20;

    BEGIN
    GetInitialExchangeFromExchangeMemory := '';

    IF NOT ExchangeMemoryEnable THEN Exit;
    IF NumberAllCalls = 0 THEN Exit;

    { We can speed this up later when it all works if we need to }

    FOR Address := 0 TO NumberAllCalls - 1 DO
        IF AllCallList [Address] = Call THEN
            BEGIN
            GetInitialExchangeFromExchangeMemory := ExchangeMemoryList [Address];
            Exit;
            END;
    END;



PROCEDURE DupeAndMultSheet.GeneratePartialCallListFromAllCallList (PartialCall: CallString;
                                                                   Band: BandType;
                                                                   Mode: ModeType;
                                                                   VAR List: CallListRecord);

{ New routine that just uses the AllCalls list }

VAR Address: INTEGER;

    BEGIN
    List.NumberCalls := 0;
    List.CursorPosition := 0;

    IF Length (PartialCall) < 2 THEN Exit;

    IF NumberAllCalls = 0 THEN Exit;

    FOR Address := 0 TO NumberAllCalls - 1 DO
        IF Pos (PartialCall, AllCallList [Address]) > 0 THEN
            BEGIN
            List.CallList [List.NumberCalls].Call := AllCallList [Address];
            List.CallList [List.NumberCalls].Dupe := CallIsADupe (AllCallList [Address], Band, Mode);
            Inc (List.NumberCalls);
            END;
    END;



FUNCTION PointsToBigCall (Entry: FourBytes): BOOLEAN;

    BEGIN
    PointsToBigCall := (Entry [1] = 255) AND (Entry [4] = 255);
    END;


PROCEDURE ConvertBigEntryAddressToFourBytes (EntryPointer: INTEGER; VAR BigEntry: FourBytes);

    BEGIN
    BigEntry [1] := 255;
    BigEntry [2] := EntryPointer MOD 256;
    BigEntry [3] := EntryPointer DIV 256;
    BigEntry [4] := 255;
    END;


FUNCTION BigEntryAddress (Entry: FourBytes): INTEGER;

{ Converts big entry string found in the entry list to an address where
  the entry can be found in the BigEntryList.  Starts at zero.  If there
  is an error, it will return with -1.                                   }

    BEGIN
    BigEntryAddress := (Entry [3] * 256) + Entry [2];
    END;



PROCEDURE ClearContestExchange (VAR Exchange: ContestExchange);

    BEGIN
    Exchange.Age              := '';
    Exchange.Band             := NoBand;
    Exchange.Callsign         := '';
    Exchange.Check            := '';
    Exchange.Classs            := '';
    Exchange.Date             := '';
    Exchange.DomesticMult     := False;
    Exchange.DomMultQTH       := '';
    Exchange.DomesticQTH      := '';
    Exchange.DXMult           := False;
    Exchange.DXQTH            := '';
    Exchange.Frequency        := 0;
    Exchange.InhibitMults     := False;
    Exchange.Mode             := NoMode;
    Exchange.Name             := '';
    Exchange.NameSent         := False;
    Exchange.NumberReceived   := -1;
    Exchange.NumberSent       := -1;
    Exchange.PostalCode       := '';
    Exchange.Power            := '';
    Exchange.Precedence       := NullCharacter;
    Exchange.Prefecture       := -1;
    Exchange.Prefix           := '';
    Exchange.PrefixMult       := False;
    Exchange.QSOPoints        := -1;

    Exchange.RandomCharsSent     := '';
    Exchange.RandomCharsReceived := '';
    Exchange.Source           := LocalQSO;  { Change to imported if needed }

    Exchange.QTH.Country      := -1;
    Exchange.QTH.Continent    := UnknownContinent;
    Exchange.QTH.Zone         := -1;
    Exchange.QTH.Prefix       := '';
    Exchange.QTH.StandardCall := '';

    Exchange.QTHString        := '';

    Exchange.Radio           := NoRadio;
    Exchange.RSTReceived     := '';
    Exchange.RSTSent         := '';
    Exchange.SearchAndPounce := FALSE;
    Exchange.TenTenNum       := -1;
    Exchange.Time            := -1;
    Exchange.TimeSeconds     := -1;
    Exchange.Zone            := '';
    Exchange.ZoneMult        := False;
    END;



FUNCTION GetNumber (Call: CallString): CHAR;

VAR TempString, NumberString: CallString;

    BEGIN
    GetNumber := '0';

    IF (Call = '') OR NOT StringHasNumber (Call) THEN Exit;

    TempString := GetPrefix (Call);

    NumberString := '';

    WHILE (TempString <> '') AND
          (Copy (TempString, Length (TempString), 1) >= '0') AND
          (Copy (TempString, Length (TempString), 1) <= '9') DO
              BEGIN
              NumberString := Copy (TempString, Length (TempString), 1) + NumberString;
              Delete (TempString, Length (TempString), 1);
              END;

    GetNumber := NumberString [1];
    END;


PROCEDURE GetDXQTH (VAR RXData: ContestExchange);

VAR NumberChar: Char; {KK1L: 6.70}

    BEGIN
    IF DomesticCountryCall (RXData.Callsign) THEN
        IF NOT CountDomesticCountries THEN
            Exit;

    CASE ActiveDXMult OF

        NoCountDXMults:
            RXData.DXQTH := RXData.QTH.CountryID;     { 6.60 }

        NoDXMults:
            {RXData.DXQTH := RXData.QTH.CountryID};   { 6.30 }

        ARRLDXCCWithNoUSAOrCanada, CQDXCCWithNoUSAOrCanada:
            IF (RXData.QTH.CountryID <> 'K')   AND (RXData.QTH.CountryID <> 'VE') THEN
                    RXData.DXQTH := RXData.QTH.CountryID;

        ARRLDXCCWithNoARRLSections:
            IF (RXData.QTH.CountryID <> 'K')    AND (RXData.QTH.CountryID <> 'VE')  AND
               (RXData.QTH.CountryID <> 'KC6')  AND (RXData.QTH.CountryID <> 'KG4') AND
               (RXData.QTH.CountryID <> 'KP1')  AND (RXData.QTH.CountryID <> 'KP2') AND
               (RXData.QTH.CountryID <> 'KP4')  AND (RXData.QTH.CountryID <> 'KP5') AND
               (RXData.QTH.CountryID <> 'KH0')  AND (RXData.QTH.CountryID <> 'KH1') AND
               (RXData.QTH.CountryID <> 'KH2')  AND (RXData.QTH.CountryID <> 'KH3') AND
               (RXData.QTH.CountryID <> 'KH4')  AND (RXData.QTH.CountryID <> 'KH5') AND
               (RXData.QTH.CountryID <> 'KH5K') AND (RXData.QTH.CountryID <> 'KH6') AND
               (RXData.QTH.CountryID <> 'KH7K') AND (RXData.QTH.CountryID <> 'KH8') AND
               (RXData.QTH.CountryID <> 'KH9')  AND (RXData.QTH.CountryID <> 'KL')  THEN
                   RXData.DXQTH := RXData.QTH.CountryID;

        ARRLDXCCWithNoUSACanadakH6OrkL7:
            IF (RXData.QTH.CountryID <> 'K')   AND (RXData.QTH.CountryID <> 'VE') AND
               (RXData.QTH.CountryID <> 'KH6') AND (RXData.QTH.CountryID <> 'KL') THEN
                    RXData.DXQTH := RXData.QTH.CountryID;

        ARRLDXCCWithNoUSACanadakH6kL7OrXE:
            IF (RXData.QTH.CountryID <> 'K')   AND (RXData.QTH.CountryID <> 'VE') AND
               (RXData.QTH.CountryID <> 'KH6') AND (RXData.QTH.CountryID <> 'KL') and (RXData.QTH.CountryID <> 'XE') THEN
                    RXData.DXQTH := RXData.QTH.CountryID;

        ARRLDXCCWithNoIOrIS0:
            IF (RXData.QTH.CountryID <> 'I')   AND (RXData.QTH.CountryID <> 'IS') THEN
                RXData.DXQTH := RXData.QTH.CountryID;


        CQDXCCWithNoHB9:
            IF RXData.QTH.CountryID <> 'HB' THEN
                RXData.DXQTH := RXData.QTH.CountryID;

        CQDXCCWithNoOK:
            IF RXData.QTH.CountryID <> 'OK' THEN
                RXData.DXQTH := RXData.QTH.CountryID;

        NorthAmericanARRLDXCCWithNoUSACanadaOrkL7:
            IF (RXData.QTH.Continent = NorthAmerica) THEN
                IF (RXData.QTH.CountryID <> 'K')  AND
                   (RXData.QTH.CountryID <> 'VE') AND
                   (RXData.QTH.CountryID <> 'KL') THEN
                       RXData.DXQTH := RXData.QTH.CountryID;

        CQEuropeanCountries:
            IF (RXData.QTH.Continent = Europe) THEN
                RXData.DXQTH := RXData.QTH.CountryID;

        CQNonEuropeanCountries:
            IF (RXdata.QTH.Continent <> Europe) THEN
                RXData.DXQTH := RXData.QTH.CountryID;

        NonSouthAmericanCountries:
            IF RXData.QTH.Continent <> SouthAmerica THEN
                RXData.DXQTH := RXData.QTH.CountryID;

        PACCCountriesAndPrefixes:
            BEGIN
            IF RXData.QTH.CountryID = 'UA9' THEN
                BEGIN
                IF StringHas (RXData.Callsign, '9') THEN
                    RXData.DXQTH := 'UA9'
                ELSE
                    RXData.DXQTH := 'UA0';

                Exit;
                END;

            IF (RXData.QTH.CountryID = 'CE')  OR
               (RXData.QTH.CountryID = 'JA')  OR (RXData.QTH.CountryID = 'LU') OR
               (RXData.QTH.CountryID = 'PY')  OR (RXData.QTH.CountryID = 'VE') OR
               (RXData.QTH.CountryID = 'K')   OR (RXData.QTH.CountryID = 'VK') OR
               (RXData.QTH.CountryID = 'ZS')  OR (RXData.QTH.CountryID = 'ZL') THEN
                   BEGIN
                   RXData.DXQTH := RXData.QTH.CountryID + GetNumber (RXData.Callsign);
                   Exit;
                   END;

            RXData.DXQTH := RXData.QTH.CountryID;
            END;

        CQNonEuropeanCountriesAndWAECallRegions:  {KK1L: 6.70 Added for new (old) mult method}
            IF (RXdata.QTH.Continent <> Europe) THEN
                BEGIN
                NumberChar := GetNumber (RXData.Callsign);

                IF (RXData.QTH.CountryID = 'JA')  OR (RXData.QTH.CountryID = 'K')  OR
                   (RXData.QTH.CountryID = 'PY')  OR (RXData.QTH.CountryID = 'VE') OR
                   (RXData.QTH.CountryID = 'VK')  OR (RXData.QTH.CountryID = 'ZS') OR
                   (RXData.QTH.CountryID = 'ZL') THEN
                BEGIN
                RXData.DXQTH := RXData.QTH.CountryID + NumberChar;
                Exit;
                END;

                IF (RXData.QTH.CountryID = 'UA9') THEN
                    CASE NumberChar OF
                        '8', '9', '0': BEGIN
                                       RXData.DXQTH := 'UA' + NumberChar;
                                       Exit;
                                       END;
                    END;

                RXData.DXQTH := RXData.QTH.CountryID;
                END;
        ELSE
            RXData.DXQTH := RXData.QTH.CountryID;
        END;

    END;



PROCEDURE GetMultsFromLogEntry (LogEntry: Str80; VAR RXData: ContestExchange);

{ This procedure will look at the log entry string passed to it and
  based upon the active mutliplier flags determine what, if any multipliers
  are contained in the mult string.  Any multipliers found will be returned
  in it's proper variable.  All other variables will be set to the null
  string.                                                                }

VAR MultString: Str20;
    Mult, NumberMults: INTEGER;
    MultArray: ARRAY [1..2] OF Str20;

    BEGIN
    RXData.DomesticQTH := '';
    RXData.DomMultQTH  := '';
    RXData.DXQTH       := '';
    RXData.Prefix      := '';
    RXData.Zone        := '';

    IF NumberDifferentMults = 0 THEN Exit;

    MultString := GetLogEntryMultString (LogEntry);

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
        IF DoingDomesticMults AND
           (StringHasLowerCase (MultArray [Mult]) OR (NumberDifferentMults = 1)) THEN
            BEGIN
            RXData.QTHString := MultArray [Mult];
            RXData.Callsign  := GetLogEntryCall (LogEntry);

{ This next step is necessary to fake the FoundDomesticQTH routine into
  thinking the Domestic QTH is valid if you are using GridFields. }

            IF ActiveDomesticMult = GridFields THEN
                RXData.QTHString := RXData.QTHString + '12';

            IF NOT FoundDomesticQTH (RXData) AND LoadingInLogFile THEN
                BEGIN
                ReportError ('Unknown domestic QTH found and ignored = ' + MultArray [Mult]);
                Wait (1000);
                END;

            Continue;
            END;

        IF DoingZoneMults AND StringIsAllNumbers (MultArray [Mult]) THEN
            BEGIN
            RXData.Zone := MultArray [Mult];
            Continue;
            END;

        IF DoingPrefixMults THEN
            BEGIN
            RXData.Prefix := MultArray [Mult];
            Continue;
            END;

        IF DoingDXMults THEN RXData.DXQTH := MultArray [Mult];
        END;
    END;



PROCEDURE TransferLogEntryInfoToContestExchange (LogEntry: Str80; VAR RXData: ContestExchange);

{ So - in 2023 we have a problem.  I am expecting RXData to have the necessary information
  to determine the initial exchange.  This wasn't a requirement before I changed things
  and went to the AllCallList that gets entries added to it when the QSO is put into
  the dupesheet.  Thus - a QSO that has scrolled off the top of the editable log window
  has messed up initial exchange information.

  To fix this - this routine has to get a lot smarter and properly populate the exchange
  fields with the proper information.  Some of this is already done in the routine that
  fetches initial exchanges from the EditableLogWindow!  }

    BEGIN
    ClearContestExchange (RXData);
    RXData.Band     := GetLogEntryBand (LogEntry);
    RXData.Mode     := GetLogEntryMode (LogEntry);
    RXData.Callsign := GetLogEntryCall (LogEntry);

    { The above stuff is what used to be here - it gets destroyed when the
      ParseExchangeIntoContestExchange procedure is called }

    ParseExchangeIntoContestExchange (LogEntry, RXData);

    GetMultsFromLogEntry (LogEntry, RXData);
    RXData.QTH.Prefix := RXData.Prefix;
    END;



FUNCTION DupeAndMultSheet.AddBigCallAddress (BigCall: EightBytes): INTEGER;

{ This function will return the proper big call array address for the
  big call entered.  If the call already exists in the big call array,
  then its address is returned.  If it does not exist, it is added to
  the end of the array and that address is returned.  This is done to
  eliminate double entries of big calls in this list, saving memory and
  making it easier to determine if a big call is a dupe on any given
  band or mode.                                                      }

VAR NumberCalls, NumberDupeBlocks, NumberEntriesInLastBlock, Block, EndAddress, Address: INTEGER;

    BEGIN
    NumberCalls  := DupeSheet.NumberBigCalls;
    NumberDupeBlocks := (NumberCalls DIV EightByteBlockSize) + 1;
    NumberEntriesInLastBlock := NumberCalls MOD EightByteBlockSize;
    If NumberDupeBlocks > MaxBigCallBlocks then
    BEGIN
       Tone.DoABeep (Single);
       QuickDisplay ('Too many big calls ask maintainer to increase!!');
       ReminderPostedCount := 30;
       Exit;
    END;

    IF NumberCalls = 0 THEN
        BEGIN
        AddBigCallAddress := 0;

        IF MaxAvail > SizeOf (EightByteBlockArray) THEN
            BEGIN
            IF DupeSheet.BigCallList [NumberDupeBlocks] = Nil THEN
                New (DupeSheet.BigCallList [NumberDupeBlocks]);
            DupeSheet.BigCallList [NumberDupeBlocks]^ [0] := BigCall;
            END
         ELSE
            BEGIN
            Tone.DoABeep (Single);
            QuickDisplay ('Not enough memory for EightByteBlockArray!!');
            ReminderPostedCount := 30;
            Exit;
            END;

        Inc (DupeSheet.NumberBigCalls);
        Exit;
        END;

    { First, look and see if the big call is in the big call arrays }

    FOR Block := 1 TO NumberDupeBlocks DO
        BEGIN
        IF Block = NumberDupeBlocks THEN
            EndAddress := NumberEntriesInLastBlock
        ELSE
            EndAddress := EightByteBlockSize;

        IF EndAddress > 0 THEN
          FOR Address := 0 TO EndAddress - 1 DO
            IF (DupeSheet.BigCallList [Block]^ [Address] [1] = BigCall [1]) AND
               (DupeSheet.BigCallList [Block]^ [Address] [2] = BigCall [2]) AND
               (DupeSheet.BigCallList [Block]^ [Address] [3] = BigCall [3]) AND
               (DupeSheet.BigCallList [Block]^ [Address] [4] = BigCall [4]) AND
               (DupeSheet.BigCallList [Block]^ [Address] [5] = BigCall [5]) AND
               (DupeSheet.BigCallList [Block]^ [Address] [6] = BigCall [6]) AND
               (DupeSheet.BigCallList [Block]^ [Address] [7] = BigCall [7]) AND
               (DupeSheet.BigCallList [Block]^ [Address] [8] = BigCall [8]) THEN
                   BEGIN
                   AddBigCallAddress := (Block - 1) * EightByteBlockSize + Address;
                   Exit;
                   END;
        END;

    { Call not found in the list.  Add it to the end and increment total. }

    IF EndAddress = 0 THEN
        BEGIN
        IF MaxAvail < SizeOf (EightByteBlockArray) THEN
            BEGIN
            Tone.DoABeep (Single);
            QuickDisplay ('Not enough memory for EightByteBlockArray!!');
            ReminderPostedCount := 30;
            AddBigCallAddress := 0;
            Exit;
            END;

        AddBigCallAddress := DupeSheet.NumberBigCalls;
        Inc (DupeSheet.NumberBigCalls);
        IF DupeSheet.BigCallList [NumberDupeBlocks] = Nil THEN
            New (DupeSheet.BigCallList [NumberDupeBlocks]);
        DupeSheet.BigCallList [NumberDupeBlocks]^ [0] := BigCall;
        Exit;
        END;

    DupeSheet.BigCallList [NumberDupeBlocks]^ [NumberEntriesInLastBlock] := BigCall;
    AddBigCallAddress := DupeSheet.NumberBigCalls;
    Inc (DupeSheet.NumberBigCalls);
    END;



PROCEDURE DupeAndMultSheet.AddCompressedCallToDupeSheet (Call:FourBytes; Band: BandType; Mode: ModeType);

VAR NumberCalls, DupeBlock, BlockAddress: INTEGER;
    DupeBand: BandType;
    DupeMode: ModeType;

    BEGIN
    IF QSOByBand THEN DupeBand := Band ELSE DupeBand := All;
    IF QSOByMode THEN DupeMode := Mode ELSE DupeMode := Both;

    NumberCalls  := DupeSheet.Totals [DupeBand, DupeMode];
    DupeBlock    := NumberCalls DIV FourByteBlockSize + 1;
    BlockAddress := NumberCalls MOD FourByteBlockSize;

    IF BlockAddress = 0 THEN    { we need to set up a new block }
        BEGIN
        IF MaxAvail < SizeOf (FourByteBlockArray) THEN
            BEGIN
            Tone.DoABeep (Single);
            QuickDisplay ('Not enough memory for FourByteBlockArray!!');
            ReminderPostedCount := 30;
            Exit;
            END;

        IF DupeSheet.DupeList [DupeBand, DupeMode, DupeBlock] = Nil THEN
            New (DupeSheet.DupeList [DupeBand, DupeMode, DupeBlock]);
        END;

    DupeSheet.DupeList [DupeBand, DupeMode, DupeBlock]^ [BlockAddress] := Call;
    Inc (DupeSheet.Totals [DupeBand, DupeMode]);
    END;



PROCEDURE DupeAndMultSheet.AddQSOToSheets (RXData: ContestExchange);

{ This procedure will take the information from the contest exchange passed
  to it and add the contact to the DupeAndMultSheet.  This is normally done
  with information after it has gone through the EditableWindow or when
  reading in the LOG.DAT file at the start.                               }

VAR CompressedCall, CompressedMult: FourBytes;
    BigCompressedCall: EightBytes;
    MultBand: BandType;
    MultMode: ModeType;
    BigCallAddress, NumberMults: INTEGER;

    BEGIN
    AddCallToAllCallList (RXData.Callsign, GetInitialExchangeStringFromContestExchange (RXData));

    RXData.Callsign := StandardCallFormat (RXData.Callsign, True);

    IF DupeSheetEnable THEN
        BEGIN
        IF Length (RXData.Callsign) <= 6 THEN
            BEGIN
            CompressFormat (RXData.Callsign, CompressedCall);
            AddCompressedCallToDupeSheet (CompressedCall, RXData.Band, RXData.Mode);
            END
        ELSE
            BEGIN
            BigCompressFormat (RXData.Callsign, BigCompressedCall);
            BigCallAddress := AddBigCallAddress (BigCompressedCall);
            ConvertBigEntryAddressToFourBytes (BigCallAddress, CompressedCall);
            AddCompressedCallToDupeSheet (CompressedCall, RXData.Band, RXData.Mode);
            END;
        END;

    IF RXData.DomesticMult OR RXData.DXMult OR RXData.PrefixMult OR RXData.ZoneMult THEN
        BEGIN
        IF MultByBand THEN MultBand := RXData.Band ELSE MultBand := All;
        IF MultByMode THEN MultMode := RXData.Mode ELSE MultMode := Both;

        IF RXData.DomesticMult THEN
            BEGIN
            NumberMults := MultSheet.Totals [MultBand, MultMode].NumberDomesticMults;

            IF NumberMults = 0 THEN
                BEGIN
                IF MaxAvail < SizeOf (DomesticMultArray) THEN
                    BEGIN
                    Tone.DoABeep (Single);
                    QuickDisplay ('Not enough memory for DomesticMultArray!!');
                    ReminderPostedCount := 30;
                    Exit;
                    END;

                IF MultSheet.DomesticList [MultBand, MultMode] = Nil THEN
                    New (MultSheet.DomesticList [MultBand, MultMode]);
                END;

            IF StringHas (RXData.DomMultQTH, '/') THEN
                RXData.DomMultQTH := PrecedingString (RXData.DomMultQTH, '/');

            CompressFormat (UpperCase (RXData.DomMultQTH), CompressedMult);

            MultSheet.DomesticList [MultBand, MultMode]^ [NumberMults] := CompressedMult;
            Inc (MultSheet.Totals [MultBand, MultMode].NumberDomesticMults);

            IF MultBand <> All THEN
                Inc (MultSheet.Totals [All, MultMode].NumberDomesticMults);

            IF MultMode <> Both THEN
                Inc (MultSheet.Totals [MultBand, Both].NumberDomesticMults);

            IF (MultBand <> All) AND (MultMode <> Both) THEN
                Inc (MultSheet.Totals [All, Both].NumberDomesticMults);

            {KK1L: 6.68 Changed following to allow for domestic mult display when a file is  }
            {           used with WYSIWYGDomestic.                                           }
            {IF NOT (LoadingInLogFile OR (ActiveDomesticMult = WYSIWYGDomestic))  THEN       }
            IF NOT (LoadingInLogFile OR ((ActiveDomesticMult = WYSIWYGDomestic) AND (DomesticQTHDataFileName = '')))  THEN
                CancelOutNewDomesticMultWeHaveWorked (RXData.DomMultQTH, MultBand, MultMode);
            END;



        IF RXData.DXMult THEN
            BEGIN
            NumberMults := MultSheet.Totals [MultBand, MultMode].NumberDXMults;

            IF NumberMults = 0 THEN
                BEGIN
                IF MaxAvail < SizeOf (DXMultArray) THEN
                    BEGIN
                    Tone.DoABeep (Single);
                    QuickDisplay ('Not enough memory for DXMultArray!!');
                    ReminderPostedCount := 30;
                    Exit;
                    END;

                IF MultSheet.DXList [MultBand, MultMode] = Nil THEN
                    New (MultSheet.DXList [MultBand, MultMode]);
                END;

            CompressFormat (RXData.DXQTH, CompressedMult);

            MultSheet.DXList [MultBand, MultMode]^ [NumberMults] := CompressedMult;

            {KK1L: 6.65 Added DXMultLimit checks to support NEQP. Allows support in other contest too}
            IF (MultSheet.Totals [MultBand, MultMode].NumberDXMults < DXMultLimit) THEN
                Inc (MultSheet.Totals [MultBand, MultMode].NumberDXMults);

            IF (MultBand <> All) AND (MultSheet.Totals [All, MultMode].NumberDXMults < DXMultLimit) THEN
                Inc (MultSheet.Totals [All, MultMode].NumberDXMults);

            IF (MultMode <> Both) AND (MultSheet.Totals [MultBand, Both].NumberDXMults < DXMultLimit) THEN
                Inc (MultSheet.Totals [MultBand, Both].NumberDXMults);

            IF (MultBand <> All) AND (MultMode <> Both) AND
               (MultSheet.Totals [All, Both].NumberDXMults < DXMultLimit) THEN
                Inc (MultSheet.Totals [All, Both].NumberDXMults);

            IF NOT LoadingInLogFile THEN
                CancelOutNewDXMultWeHaveWorked (RXData.DXQTH, MultBand, MultMode);

            END;

        IF RXData.PrefixMult THEN
            BEGIN
            NumberMults := MultSheet.Totals [MultBand, MultMode].NumberPrefixMults;
            IF NumberMults = 0 THEN
                BEGIN
                IF MaxAvail < SizeOf (PrefixMultArray) THEN
                    BEGIN
                    Tone.DoABeep (Single);
                    QuickDisplay ('Not enough memory for PrefixMultArray!!');
                    ReminderPostedCount := 30;
                    Exit;
                    END;

                IF MultSheet.PrefixList [MultBand, MultMode] = Nil THEN
                    New (MultSheet.PrefixList [MultBand, MultMode]);
                END;

            CompressFormat (RXData.Prefix, CompressedMult);

            MultSheet.PrefixList [MultBand, MultMode]^ [NumberMults] := CompressedMult;
            Inc (MultSheet.Totals [MultBand, MultMode].NumberPrefixMults);
            IF MultBand <> All THEN
                Inc (MultSheet.Totals [All, MultMode].NumberPrefixMults);
            IF MultMode <> Both THEN
                Inc (MultSheet.Totals [MultBand, Both].NumberPrefixMults);
            IF (MultBand <> All) AND (MultMode <> Both) THEN
                Inc (MultSheet.Totals [All, Both].NumberPrefixMults);
            END;

        IF RXData.ZoneMult THEN
            BEGIN
            NumberMults := MultSheet.Totals [MultBand, MultMode].NumberZoneMults;

            IF NumberMults = 0 THEN
                BEGIN
                IF MaxAvail < SizeOf (ZoneMultArray) THEN
                    BEGIN
                    Tone.DoABeep (Single);
                    QuickDisplay ('Not enough memory for ZoneMultArray!!');
                    ReminderPostedCount := 30;
                    Exit;
                    END;

                IF MultSheet.ZoneList [MultBand, MultMode] = nil THEN
                    New (MultSheet.ZoneList [MultBand, MultMode]);
                END;

            CompressFormat (RXData.Zone, CompressedMult);

            IF NumberMults < ZoneMultArraySize THEN
                BEGIN
                MultSheet.ZoneList [MultBand, MultMode]^ [NumberMults] := CompressedMult;

                Inc (MultSheet.Totals [MultBand, MultMode].NumberZoneMults);

                IF MultBand <> All THEN
                    Inc (MultSheet.Totals [All, MultMode].NumberZoneMults);

                IF MultMode <> Both THEN
                    Inc (MultSheet.Totals [MultBand, Both].NumberZoneMults);

                IF (MultBand <> All) AND (MultMode <> Both) THEN
                    Inc (MultSheet.Totals [All, Both].NumberZoneMults);

                END;

            IF NOT LoadingInLogFile THEN
                CancelOutNewZoneMultWeHaveWorked (RXData.Zone, MultBand, MultMode);
            END;
        END;
    END;



FUNCTION DupeAndMultSheet.CallIsADupe (Call: CallString; Band: BandType; Mode: ModeType): BOOLEAN;

{ This function will return true if the callsign passed to it exists in
  the dupesheet for the band and mode specified.                         }

VAR CompressedCall: FourBytes;
    BigCompressedCall : EightBytes;
    BigCallAddress: INTEGER;
    DupeBand: BandType;
    DupeMode: ModeType;

    BEGIN
    Call := StandardCallFormat (Call, True);

    IF QSOByBand THEN DupeBand := Band ELSE DupeBand := All;
    IF QSOByMode THEN DupeMode := Mode ELSE DupeMode := Both;

    IF Length (Call) <= 6 THEN
        CompressFormat (Call, CompressedCall)
    ELSE
        BEGIN
        BigCompressFormat (Call, BigCompressedCall);
        BigCallAddress := AddBigCallAddress (BigCompressedCall);
        ConvertBigEntryAddressToFourBytes (BigCallAddress, CompressedCall);
        END;

    CallIsADupe := EntryExists (CompressedCall, DupeBand, DupeMode);
    END;



PROCEDURE DupeAndMultSheet.ClearDupeSheet;

VAR Band: BandType;
    Mode: ModeType;
    NumberBlocks, Block: INTEGER;

    BEGIN
    FOR Band := Band160 TO All DO
        FOR Mode := CW TO Both DO
            IF DupeSheet.Totals [Band, Mode] > 0 THEN
                BEGIN
                NumberBlocks := (DupeSheet.Totals [Band, Mode] - 1) DIV FourByteBlockSize + 1;

                FOR Block := 1 TO NumberBlocks DO
                    IF DupeSheet.DupeList [Band, Mode, Block] <> Nil THEN
                        BEGIN
                        Dispose (DupeSheet.DupeList [Band, Mode, Block]);
                        DupeSheet.DupeList [Band, Mode, Block] := nil;
                        END;

                DupeSheet.Totals [Band, Mode] := 0;
                END;

    IF DupeSheet.NumberBigCalls > 0 THEN
        BEGIN
        NumberBlocks := (DupeSheet.NumberBigCalls - 1) DIV EightByteBlockSize + 1;
        FOR Block := 1 TO NumberBlocks DO
            IF DupeSheet.BigCallList [Block] <> Nil THEN
                BEGIN
                Dispose (DupeSheet.BigCallList [Block]);
                DupeSheet.BigCallList [Block] := nil;
                END;

        DupeSheet.NumberBigCalls := 0;
        END;
    END;



PROCEDURE DupeAndMultSheet.AddCallToVisibleDupeSheet (Callsign: CallString);

VAR SuffixString, NumberString: CallString;
    NumberChar, Character: CHAR;
    NextRecord, ActiveVDEntry: VDEntryPointer;
    Count: INTEGER;

    BEGIN
    IF MaxAvail <= SizeOf (VDEntry) THEN Exit;

{    StandardCall := StandardCallFormat (Callsign, True);

    IF StringHas (StandardCall, '/') THEN
        StandardCall := PostcedingString (StandardCall, '/');
}
    Callsign := RootCall (Callsign);

    NumberString := NumberPartOfString (Callsign);
    NumberChar := NumberString [1];

    IF FirstVDEntry = nil THEN  { Set up entries with 1 to 10 }
        BEGIN
        FirstVDEntry := New (VDEntryPointer);
        ActiveVDEntry := FirstVDEntry;

        ActiveVDEntry^.Callsign  := '1';
        ActiveVDEntry^.NextEntry := New (VDEntryPointer);

        FOR Count := 2 TO 10 DO
            BEGIN
            IF Count < 10 THEN
                Character := Chr (Ord ('0') + Count)
            ELSE
                Character := '0';

            ActiveVDEntry := ActiveVDEntry^.NextEntry;

            ActiveVDEntry^.Callsign := Character;

            IF Character <> '0' THEN
                ActiveVDEntry^.NextEntry := New (VDEntryPointer)
            ELSE
                ActiveVDEntry^.NextEntry := nil;
            END;

        NumberVDCalls := 10;
        END;

    ActiveVDEntry := FirstVDEntry;

    WHILE ActiveVDEntry^.Callsign <> NumberChar DO
        IF ActiveVDEntry^.NextEntry = Nil THEN
            BEGIN
            ActiveVDEntry^.NextEntry := New (VDEntryPointer);
            ActiveVDEntry := ActiveVDEntry^.NextEntry;

            ActiveVDEntry^.Callsign := Callsign;
            ActiveVDEntry^.NextEntry := nil;
            Inc (NumberVDCalls);
            Exit;
            END
        ELSE
            ActiveVDEntry := ActiveVDEntry^.NextEntry;

  { We have found the Number Entry that matches the call we are adding. }

    IF ActiveVDEntry^.NextEntry = nil THEN         { Adding the 1st 0 call? }
        BEGIN
        IF ActiveVDEntry^.Callsign <> '0' THEN
            BEGIN
            SetWindow (WholeScreenWindow);
            ClrScr;
            ReportError ('Not finding zero at end visible dupesheet list!!');
            Halt;
            END;

        ActiveVDEntry^.NextEntry := New (VDEntryPointer);
        ActiveVDEntry := ActiveVDEntry^.NextEntry;

        ActiveVDEntry^.Callsign := Callsign;
        ActiveVDEntry^.NextEntry := nil;
        Inc (NumberVDCalls);
        Exit;                               { All done - 1st 0 call added }
        END;

   { See if it is the first for this number }

   NextRecord := ActiveVDEntry^.NextEntry;

   IF (Length (NextRecord^.Callsign) = 1) AND StringIsAllNumbers (ActiveVDEntry^.Callsign) THEN
       BEGIN
       ActiveVDEntry^.NextEntry := New (VDEntryPointer);

       ActiveVDEntry := ActiveVDEntry^.NextEntry;
       ActiveVDEntry^.Callsign := Callsign;
       ActiveVDentry^.NextEntry := NextRecord;
       Inc (NumberVDCalls);
       Exit;
       END;

   { We have to find the right place to squeeze it }

   SuffixString := GetSuffix (Callsign);

//   WHILE (SuffixString > GetSuffix (NextRecord^.Callsign)) AND
//         (Length (NextRecord^.Callsign) > 1) AND
//         (ActiveVDEntry^.NextEntry <> nil) DO
while true do
             BEGIN
if (ActiveVDEntry^.NextEntry = nil) then break;
if (SuffixString <= GetSuffix(NextRecord^.Callsign)) then break;
if (Length(NextRecord^.Callsign) < 2) then break;
             ActiveVDEntry := NextRecord;
             NextRecord := ActiveVDEntry^.NextEntry;
             END;

//   IF (ActiveVDEntry^.NextEntry = nil) AND (SuffixString > GetSuffix (NextRecord^.Callsign)) THEN
//       BEGIN
//       ActiveVDEntry := NextRecord;
//
//       ActiveVDEntry^.NextEntry := New (VDEntryPointer);
//       ActiveVDEntry := ActiveVDEntry^.NextEntry;
//       ActiveVDEntry^.Callsign := Callsign;
//       ActiveVDEntry^.NextEntry := nil;
//       Exit;
//       END;

   ActiveVDEntry^.NextEntry := New (VDEntryPointer);

   ActiveVDEntry := ActiveVDEntry^.NextEntry;
   ActiveVDEntry^.Callsign := Callsign;
   ActiveVDentry^.NextEntry := NextRecord;
   Inc (NumberVDCalls);
   END;



PROCEDURE DupeAndMultSheet.CreateVisibleDupeSheetArrays (VAR Band: BandType;
                                                             Mode: ModeType);

VAR Block, NumberBlocks, NumberEntriesInLastBlock, TargetAddress: INTEGER;
    CallAddress: INTEGER;
    Callsign: Str80;
    CompressedCall: FourBytes;
    BigCompressedCall: EightBytes;
    NextEntry, ActiveVDEntry: VDEntryPointer;

    BEGIN
    NumberVDCalls := 0;

    ActiveVDEntry := FirstVDEntry;

    WHILE ActiveVDEntry <> Nil DO
        BEGIN
        NextEntry := ActiveVDEntry^.NextEntry;
        Dispose (ActiveVDEntry);
        ActiveVDEntry := NextEntry;
        END;

    FirstVDEntry := nil;

    IF (Mode = NoMode) OR (DupeSheet.Totals [Band, Mode] = 0) THEN Exit;

    NumberBlocks := (DupeSheet.Totals [Band, Mode] - 1) DIV FourByteBlockSize + 1;
    NumberEntriesInLastBlock := DupeSheet.Totals [Band,Mode] MOD FourByteBlockSize;

    FOR Block := 1 TO NumberBlocks DO
        BEGIN
        IF Block = NumberBlocks THEN
            TargetAddress := NumberEntriesInLastBlock
        ELSE
            TargetAddress := FourByteBlockSize;

        FOR CallAddress := 0 TO TargetAddress - 1 DO
            BEGIN
            CompressedCall := Dupesheet.DupeList [Band, Mode, Block]^ [CallAddress];

            IF NOT PointsToBigCall (CompressedCall) THEN
                Callsign := ExpandedString (CompressedCall)
            ELSE
                BEGIN
                BigCompressedCall := DupeSheet.BigCallList [1]^ [BigEntryAddress (CompressedCall)];
                Callsign := BigExpandedString (BigCompressedCall);
                Callsign := PostcedingString (Callsign, ' ');
                GetRidOfPostcedingSpaces (Callsign);
                END;

            AddCallToVisibleDupeSheet (Callsign);
            END;
        END;
    END;



PROCEDURE DupeAndMultSheet.DisposeOfMemoryAndZeroTotals;

VAR Band: BandType;
    Mode: ModeType;
    NumberBlocks, Block: INTEGER;

    BEGIN
    FOR Band := Band160 TO All DO
        FOR Mode := CW TO Both DO
            BEGIN
            IF DupeSheet.Totals [Band, Mode] > 0 THEN
                BEGIN
                NumberBlocks := (DupeSheet.Totals [Band, Mode] - 1) DIV FourByteBlockSize + 1;
                FOR Block := 1 TO NumberBlocks DO
                    IF DupeSheet.DupeList [Band, Mode, Block] <> Nil THEN
                        BEGIN
                        Dispose (DupeSheet.DupeList [Band, Mode, Block]);
                        DupeSheet.DupeList [Band, Mode, Block] := nil;
                        END;
                END;

            IF MultSheet.DomesticList [Band, Mode] <> nil THEN
                BEGIN
                Dispose (MultSheet.DomesticList [Band, Mode]);
                MultSheet.DomesticList [Band, Mode] := nil;
                END;

            IF MultSheet.DXList [Band, Mode] <> nil THEN
                BEGIN
                Dispose (MultSheet.DXList [Band, Mode]);
                MultSheet.DXList [Band, Mode] := nil;
                END;

            IF MultSheet.PrefixList [Band, Mode] <> nil THEN
                BEGIN
                Dispose (MultSheet.PrefixList [Band, Mode]);
                MultSheet.PrefixList [Band, Mode] := nil;
                END;

            IF MultSheet.ZoneList [Band, Mode] <> nil THEN
                BEGIN
                Dispose (MultSheet.ZoneList [Band, Mode]);
                MultSheet.ZoneList [Band, Mode] := nil;
                END;

            IF RemMultMatrix [Band, Mode, Domestic] <> nil THEN
                BEGIN
                Dispose (RemMultMatrix [Band, Mode, Domestic]);
                RemMultMatrix [Band, Mode, Domestic] := nil;
                END;

            IF RemMultMatrix [Band, Mode, DX] <> nil THEN
                BEGIN
                Dispose (RemMultMatrix [Band, Mode, DX]);
                RemMultMatrix [Band, Mode, DX] := nil;
                END;

            IF RemMultMatrix [Band, Mode, Zone] <> nil THEN
                BEGIN
                Dispose (RemMultMatrix [Band, Mode, Zone]);
                RemMultMatrix [Band, Mode, Zone] := nil;
                END;

            END;

    IF DupeSheet.NumberBigCalls > 0 THEN
        BEGIN
        NumberBlocks := (DupeSheet.NumberBigCalls - 1) DIV EightByteBlockSize + 1;

        FOR Block := 1 TO NumberBlocks DO
            IF DupeSheet.BigCallList [Block] <> nil THEN
                BEGIN
                Dispose (DupeSheet.BigCallList [Block]);
                DupeSheet.BigCallList [Block] := nil;
                END;
        END;

    FOR Band := Band160 TO All DO
        FOR Mode := CW TO Both DO
            BEGIN
            DupeSheet.Totals [Band, Mode] := 0;
            QSOTotals        [Band, Mode] := 0;

            FOR Block := 1 TO MaxCallBlocks DO
                DupeSheet.DupeList [Band, Mode, Block] := nil;

            FOR Block := 1 TO MaxBigCallBlocks DO
                DupeSheet.BigCallList [Block] := nil;

            MultSheet.Totals [Band, Mode].NumberDomesticMults := 0;
            MultSheet.Totals [Band, Mode].NumberDXMults       := 0;
            MultSheet.Totals [Band, Mode].NumberPrefixMults   := 0;
            MultSheet.Totals [Band, Mode].NumberZoneMults     := 0;

            MultSheet.DomesticList [Band, Mode] := nil;
            MultSheet.DXList [Band, Mode]       := nil;
            MultSheet.PrefixList [Band, Mode]   := nil;
            MultSheet.ZoneList [Band, Mode]     := nil;
            END;

    DupeSheet.NumberBigCalls := 0;
    TotalQSOPoints := 0;
    TotalNamesSent := 0;
    NumberPartialCalls := 0;
    NumberAllCalls := 0;
    END;

PROCEDURE DupeAndMultSheet.DupeSheetTotals (VAR Totals: QSOTotalArray);

    BEGIN
    Totals := DupeSheet.Totals;
    END;



FUNCTION DupeAndMultSheet.EntryExists (Entry: FourBytes; Band: BandType; Mode: ModeType): BOOLEAN;

VAR DupeBand: BandType;
    DupeMode: ModeType;
    NumberCalls, NumberDupeBlocks, NumberEntriesInLastBlock, Block, EndAddress: INTEGER;

    BEGIN
    IF QSOByBand  THEN DupeBand := Band ELSE DupeBand := All;
    IF QSOByMode  THEN DupeMode := Mode ELSE DupeMode := Both;

    IF DupeMode = NoMode THEN
        BEGIN
        EntryExists := False;
        Exit;
        END;

    NumberCalls := DupeSheet.Totals [DupeBand, DupeMode];

    NumberDupeBlocks         := (NumberCalls - 1) DIV FourByteBlockSize + 1;
    NumberEntriesInLastBlock := (NumberCalls - 1) MOD FourByteBlockSize + 1;

    Block := 1;

    REPEAT
        IF Block = NumberDupeBlocks THEN
            EndAddress := NumberEntriesInLastBlock
        ELSE
            EndAddress := FourByteBlockSize;

        IF BYTDUPE (Addr (Entry), EndAddress, DupeSheet.DupeList [DupeBand, DupeMode, Block]) THEN
            BEGIN
            EntryExists := True;
            Exit;
            END;

        Inc (Block);
    UNTIL Block > NumberDupeBlocks;

    EntryExists := False;
    END;




PROCEDURE DupeAndMultSheet.ExamineLogForQSOTotals (VAR QTotals: QSOTotalArray);

VAR FileRead: TEXT;
    TempString: Str80;
    Band: BandType;
    Mode: ModeType;

    BEGIN
    FOR Band := Band160 TO All DO
        FOR Mode := CW TO Both DO
            QTotals [Band, Mode] := 0;

    IF OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        WHILE NOT EOF (FileRead) DO
            BEGIN
            REPEAT
                ReadLn (FileRead, TempString);
                Band := GetLogEntryBand (TempString);
            UNTIL (Band <> NoBand) OR Eof (FileRead);

            Mode := GetLogEntryMode (TempString);

            IF (Band <> NoBand) AND (Mode <> NoMode) THEN
                BEGIN
                ExpandTabs (TempString);
                Inc (QTotals [Band, Mode]);
                Inc (QTotals [Band, Both]);
                Inc (QTotals [All,  Mode]);
                Inc (QTotals [All,  Both]);
                END;
            END;
        Close (FileRead);
        END;
    END;



PROCEDURE DupeAndMultSheet.AddPossibleCallsFromDupesheet (Call: CallString; VAR List: CallListRecord);

{ This will not set the NumberCalls to zero - so any calls already added will be there }

VAR Address: INTEGER;

    BEGIN
    IF Call = '' THEN Exit;
    IF NumberAllCalls = 0 THEN Exit;

    { Not sure about this - but will keep it for now }

    Call := StandardCallFormat (Call, True);

    { We can speed this up later }

    FOR Address := 0 TO NumberAllCalls - 1 DO
        IF SimilarCall (Call, AllCallList [Address]) THEN
            BEGIN
            List.CallList [List.NumberCalls].Call := AllCallList [Address];
            List.CallList [List.NumberCalls].Dupe := False;  { Unless you want to give me band/mode }
            Inc (List.NumberCalls);
            END;
    END;




PROCEDURE DupeAndMultSheet.MultSheetTotals (VAR Totals: MultTotalArrayType);

    BEGIN
    Totals := MultSheet.Totals;
    END;



FUNCTION DupeAndMultSheet.IsADomesticMult (Mult: Str10; Band: BandType; Mode: ModeType): BOOLEAN;

VAR NumberMults: INTEGER;
    CompressedMult: FourBytes;

    BEGIN
    IsADomesticMult := False;

    IF Mult = '' THEN Exit;

    IF NOT MultByBand THEN Band := All;
    IF NOT MultByMode THEN Mode := Both;

    IF DoingDomesticMults THEN
        BEGIN
        NumberMults := MultSheet.Totals [Band, Mode].NumberDomesticMults;

        IF NumberMults = 0 THEN
            IsADomesticMult := True
        ELSE
            BEGIN
            CompressFormat (Mult, CompressedMult);

            IF NOT BytDupe (Addr (CompressedMult), NumberMults, MultSheet.DomesticList [Band, Mode]) THEN
                IsADomesticMult := True;
            END;
        END;
    END;



PROCEDURE DupeAndMultSheet.SetMultFlags (VAR RXData: ContestExchange);

{ This procedure will look at the contest exchange passed to it and see
  if any multiplier flags should be set.  No updating of multiplier arrays
  of totals is done.        }

VAR NumberMults: INTEGER;
    MultBand: BandType;
    MultMode: ModeType;
    CompressedMult: FourBytes;
    DomQTH: Str20;
    FoundDomesticQTH: boolean;

    BEGIN
    RXData.DomesticMult := False;
    RXData.DXMult       := False;
    RXData.PrefixMult   := False;
    RXData.ZoneMult     := False;

    IF MultByBand THEN MultBand := RXData.Band ELSE MultBand := All;
    IF MultByMode THEN MultMode := RXData.Mode ELSE MultMode := Both;

    IF (ContestName = 'CWT') OR (ContestName = 'CWO') THEN { CWT and CWO uses calls for mults }
        BEGIN
        RXData.DomMultQTH := RXData.Callsign;
        IF Length (RXData.DomMultQTH) > 6 THEN
            RXData.DomMultQTH := Copy (RXData.DomMultQTH, 1, 6);
        END
    ELSE
        IF (RXData.DomMultQTH = '') AND (RXData.DomesticQTH <> '') THEN
            RXData.DomMultQTH := RXData.DomesticQTH;

    FoundDomesticQTH := RXData.DomMultQTH <> ''; //for WRTC 2018

    IF (RXData.DomMultQTH <> '') AND DoingDomesticMults THEN
        BEGIN
        NumberMults := MultSheet.Totals [MultBand, MultMode].NumberDomesticMults;
        IF NumberMults = 0 THEN
            RXData.DomesticMult := True
        ELSE
            BEGIN
            IF StringHas (RXData.DomMultQTH, '/') THEN
                DomQTH := PrecedingString (RXData.DomMultQTH, '/')
            ELSE
                DomQTH := RXData.DomMultQTH;

            CompressFormat (UpperCase (DomQTH), CompressedMult);

            IF NOT BytDupe (Addr (CompressedMult), NumberMults, MultSheet.DomesticList [MultBand, MultMode]) THEN
                RXData.DomesticMult := True;
            END;
        END;

//Last test WRTC 2018 Remove DX mult if HQ station

    IF (RXData.DXQTH <> '') AND DoingDXMults AND
       (ActiveDXMult <> NoCountDXMults) AND
       (NOT (FoundDomesticQTH and NoMultDXIfDomestic)) THEN
        BEGIN
        NumberMults := MultSheet.Totals [MultBand, MultMode].NumberDXMults;

        IF NumberMults = 0 THEN
            RXData.DXMult := True
        ELSE
            BEGIN
            CompressFormat (RXData.DXQTH, CompressedMult);

            IF NOT BytDupe (Addr (CompressedMult), NumberMults, MultSheet.DXList [MultBand, MultMode]) THEN
                RXData.DXMult := True;
            END;

        END;

    IF (RXData.Prefix <> '') AND DoingPrefixMults THEN
        BEGIN
        NumberMults := MultSheet.Totals [MultBand, MultMode].NumberPrefixMults;

        IF NumberMults = 0 THEN
            RXData.PrefixMult := True
        ELSE
            BEGIN
            CompressFormat (RXData.Prefix, CompressedMult);

            IF NOT BytDupe (Addr (CompressedMult), NumberMults, MultSheet.PrefixList [MultBand, MultMode]) THEN
                RXData.PrefixMult := True;
            END;
        END;

    IF (RXData.Zone <> '') AND DoingZoneMults THEN
        BEGIN
        NumberMults := MultSheet.Totals [MultBand, MultMode].NumberZoneMults;

        IF NumberMults = 0 THEN
            RXData.ZoneMult := True
        ELSE
            BEGIN
            CompressFormat (RXData.Zone, CompressedMult);
            IF NOT BytDupe (Addr (CompressedMult), NumberMults, MultSheet.ZoneList [MultBand, MultMode]) THEN
                RXData.ZoneMult := True;
            END;
        END;
    END;



PROCEDURE DupeAndMultSheet.SaveRestartFile;

VAR Band: BandType;
    Mode: ModeType;
    FileWrite: File;
    Address, Block, xResult, NumberBlocks: INTEGER;

    BEGIN
    Assign  (FileWrite, LogRestartFileName);
    ReWrite (FileWrite, 1);

    BlockWrite (FileWrite, RestartVersionNumber,   SizeOf (RestartVersionNumber),   xResult);
    BlockWrite (FileWrite, ContestName,            SizeOf (ContestName),            xResult);

    BlockWrite (FileWrite, BandMemory,       SizeOf (BandMemory),       xResult);
    BlockWrite (FileWrite, ModeMemory,       SizeOf (ModeMemory),       xResult);
    BlockWrite (FileWrite, DupeSheet.Totals, SizeOf (DupeSheet.Totals), xResult);
    BlockWrite (FileWrite, DupeSheet.NumberBigCalls, SizeOf (DupeSheet.NumberBigCalls), xResult);
    BlockWrite (FileWrite, QSOTotals,        SizeOf (QSOTotals),        xResult);
    BlockWrite (FileWrite, TotalNamesSent,   SizeOf (TotalNamesSent),   xResult);
    BlockWrite (FileWrite, TotalQSOPoints,   SizeOf (TotalQSOPoints),   xResult);
    {BlockWrite (FileWrite, RadioOneSpeed,    SizeOf (RadioOneSpeed),    xResult);}
    {BlockWrite (FileWrite, RadioTwoSpeed,    SizeOf (RadioTwoSpeed),    xResult);}
    BlockWrite (FileWrite, SpeedMemory[RadioOne], SizeOf (SpeedMemory[RadioOne]),  xResult); {KK1L: 6.73}
    BlockWrite (FileWrite, SpeedMemory[RadioTwo], SizeOf (SpeedMemory[RadioTwo]),  xResult); {KK1L: 6.73}
    BlockWrite (FileWrite, MultByBand,       SizeOf (MultByBand),       xResult);
    BlockWrite (FileWrite, MultByMode,       SizeOf (MultByMode),       xResult);
    BlockWrite (FileWrite, TakingABreak,     SizeOf (TakingABreak),     xResult);
    BlockWrite (FileWrite, TotalOffTime,     SizeOf (TotalOffTime),     xResult);
    BlockWrite (FileWrite, OffTimeStart,     SizeOf (OffTimeStart),     xResult);

    BlockWrite (FileWrite, ContinentQSOCount, SizeOf (ContinentQSOCount), xResult);
    BlockWrite (FileWrite, TimeSpentByBand,   SizeOf (TimeSpentByBand),   xResult);

    BlockWrite (FileWrite, BandChangesThisHour, SizeOf (BandChangesThisHour), xResult);
    BlockWrite (FileWrite, LastBand,            SizeOf (LastBand),            xResult);
    BlockWrite (FileWrite, LastCQFrequency,     SizeOf (LastCQFrequency),     xResult); {KK1L: 6.68}
    BlockWrite (FileWrite, LastCQMode,          SizeOf (LastCQMode),          xResult); {KK1L: 6.68}


    FOR Band := Band160 TO All DO
        FOR Mode := CW TO Both DO
            BEGIN
            IF DupeSheet.Totals [Band, Mode] > 0 THEN
                BEGIN
                NumberBlocks := (DupeSheet.Totals [Band, Mode] - 1) DIV FourByteBlockSize + 1;
                FOR Block := 1 TO NumberBlocks DO
                    BlockWrite (FileWrite,
                                DupeSheet.DupeList [Band, Mode, Block]^,
                                SizeOf (DupeSheet.DupeList [Band, Mode, Block]^),
                                xResult);
                END;
            END;

    IF DupeSheet.NumberBigCalls > 0 THEN
        BEGIN
        NumberBlocks := (DupeSheet.NumberBigCalls - 1) DIV EightByteBlockSize + 1;

        FOR Block := 1 TO NumberBlocks DO
            BlockWrite (FileWrite,
                        DupeSheet.BigCallList [Block]^,
                        SizeOf (DupeSheet.BigCallList [Block]^),
                        xResult);
        END;

    BlockWrite (FileWrite, RemainingMultDisplay, SizeOf (RemainingMultDisplay), xResult);

    WITH Multsheet DO
        BEGIN
        BlockWrite (FileWrite, Totals, SizeOf (Totals), xResult);

        FOR Band := Band160 TO All DO
            FOR Mode := CW TO Both DO
                IF (((MultByBand) AND (Band <> All)) OR
                   ((NOT MultByBand) AND (Band = All))) AND
                   (((MultByMode) AND (Mode <> Both)) OR
                   ((NOT MultByMode) AND (Mode = Both))) THEN
                       BEGIN
                       IF Totals [Band, Mode].NumberDomesticMults > 0 THEN
                           BlockWrite (FileWrite,
                                       DomesticList [Band, Mode]^,
                                       SizeOf (DomesticList [Band, Mode]^),
                                       xResult);

                       IF Totals [Band, Mode].NumberDXMults > 0 THEN
                           BlockWrite (FileWrite,
                                       DXList [Band, Mode]^,
                                       SizeOf (DXList [Band, Mode]^),
                                       xResult);

                       IF Totals [Band, Mode].NumberPrefixMults > 0 THEN
                           BlockWrite (FileWrite,
                                       PrefixList [Band, Mode]^,
                                       SizeOf (PrefixList [Band, Mode]^),
                                       xResult);

                       IF Totals [Band, Mode].NumberZoneMults > 0 THEN
                           BlockWrite (FileWrite,
                                       ZoneList [Band, Mode]^,
                                       SizeOf (ZoneList [Band, Mode]^),
                                       xResult);
                       END;
        END;

    BlockWrite (FileWrite, NumberAllCalls , SizeOf (NumberAllCalls), xResult);

    IF NumberAllCalls > 0 THEN
        FOR Address := 0 TO NumberAllCalls - 1 DO
            BEGIN
            BlockWrite (FileWrite, AllCallList [Address], SizeOf (AllCallList [Address]), xResult);
            BlockWrite (FileWrite, ExchangeMemoryList [Address], SizeOf (ExchangeMemoryList [Address]), xResult);
            END;

    FOR Band := Band160 TO Band2 DO
        FOR Mode := CW TO PHONE DO
            BlockWrite (FileWrite, FreqMemory [Band, Mode], SizeOf (FreqMemory [Band, Mode]), xResult);

    Close (FileWrite);
    END;



FUNCTION DupeAndMultSheet.ReadInBinFiles (JustDoIt: BOOLEAN): BOOLEAN;

VAR FileRead: FILE;
    Address, xResult, Block, NumberBlocks: INTEGER;
    Band: BandType;
    Mode: ModeType;
    RestartVersion: Str20;
    NameOfContest: Str80;

    BEGIN
    DisposeOfMemoryAndZeroTotals;

    ReadInBinFiles := False;

    IF NOT JustDoIt THEN
        BEGIN
        IF NOT FileExists (LogFileName) THEN
            BEGIN
            Write ('No ', LogFileName, ' file found.  ');

            IF NOT FileExists (LogRestartFileName) THEN
                WriteLn ('Ignoring ', LogRestartFileName, '.')
            ELSE
                WriteLn;

            Exit;
            END;

        IF NOT FileExists (LogRestartFileName) THEN
            BEGIN
            WriteLn ('No ', LogRestartFileName, ' file found.');
            Exit;
            END;

        IF CompareFileTimes (LogRestartFileName, LogFileName) = Before THEN
            BEGIN
            WriteLn (LogRestartFileName, ' is older than LOG.DAT and will be ignored.');
            Exit;
            END;
        END;

    WriteLn ('Reading in restart file...');

    Assign (FileRead, LogRestartFileName);
    Reset  (FileRead, 1);

    BlockRead (FileRead, RestartVersion,   SizeOf (RestartVersion),   xResult);

    IF RestartVersion <> RestartVersionNumber THEN
        BEGIN
        Close (FileRead);
        SetUpRemainingMultiplierArrays;
        ReportError ('RESTART.BIN is from a different program version.');
        ReportError ('POST is expecting version ' + RestartVersionNumber + '.');  {KK1L: 6.73}
        ReportError ('The file you are trying to read version ' + RestartVersion + '.'); {KK1L: 6.73}
        Exit;
        END;

    BlockRead (FileRead, NameOfContest, SizeOf (ContestName), xResult);

    IF ContestName = '' THEN ContestName := NameOfContest;

    IF NameOfContest <> ContestName THEN
        BEGIN
        Close (FileRead);
        SetUpRemainingMultiplierArrays;
        WriteLn ('RESTART.BIN is for a different contest.');
        Exit;
        END;

    BlockRead (FileRead, BandMemory,       SizeOf (BandMemory),       xResult);
    BlockRead (FileRead, ModeMemory,       SizeOf (ModeMemory),       xResult);
    BlockRead (FileRead, DupeSheet.Totals, SizeOf (DupeSheet.Totals), xResult);
    BlockRead (FileRead, DupeSheet.NumberBigCalls, SizeOf (DupeSheet.NumberBigCalls), xResult);
    BlockRead (FileRead, QSOTotals,        SizeOf (QSOTotals),        xResult);
    BlockRead (FileRead, TotalNamesSent,   SizeOf (TotalNamesSent),   xResult);
    BlockRead (FileRead, TotalQSOPoints,   SizeOf (TotalQSOPoints),   xResult);
    BlockRead (FileRead, SpeedMemory[RadioOne], SizeOf (SpeedMemory[RadioOne]),  xResult); {KK1L: 6.73}
    BlockRead (FileRead, SpeedMemory[RadioTwo], SizeOf (SpeedMemory[RadioTwo]),  xResult); {KK1L: 6.73}
    BlockRead (FileRead, MultByBand,       SizeOf (MultByBand),       xResult);
    BlockRead (FileRead, MultByMode,       SizeOf (MultByMode),       xResult);
    BlockRead (FileRead, TakingABreak,     SizeOf (TakingABreak),     xResult);
    BlockRead (FileRead, TotalOffTime,     SizeOf (TotalOffTime),     xResult);
    BlockRead (FileRead, OffTimeStart,     SizeOf (OffTimeStart),     xResult);

    BlockRead (FileRead, ContinentQSOCount, SizeOf (ContinentQSOCount), xResult);
    BlockRead (FileRead, TimeSpentByBand,   SizeOf (TimeSpentByBand),   xResult);

    BlockRead (FileRead, BandChangesThisHour, SizeOf (BandChangesThisHour), xResult);
    BlockRead (FileRead, LastBand,            SizeOf (LastBand),            xResult);
    BlockRead (FileRead, LastCQFrequency,     SizeOf (LastCQFrequency),     xResult); {KK1L: 6.68}
    BlockRead (FileRead, LastCQMode,          SizeOf (LastCQMode),          xResult); {KK1L: 6.68}

    IF BandMemory [RadioOne] >= All THEN BandMemory [RadioOne] := Band160;
    IF BandMemory [RadioTwo] >= All THEN BandMemory [RadioTwo] := Band160;

    IF ModeMemory [RadioOne] >= Both THEN ModeMemory [RadioOne] := CW;
    IF ModeMemory [RadioTwo] >= Both THEN ModeMemory [RadioTwo] := CW;

    ActiveBand := BandMemory [RadioOne];
    ActiveMode := ModeMemory [RadioOne];

    BandMapBand := ActiveBand; {KK1L: 6.68 gets BM in sync when no radio connected}
    BandMapMode := ActiveMode; {KK1L: 6.68 gets BM in sync when no radio connected}

    FOR Band := Band160 TO All DO
        FOR Mode := CW TO Both DO
            IF DupeSheet.Totals [Band, Mode] > 0 THEN
                BEGIN
                NumberBlocks := (DupeSheet.Totals [Band, Mode] - 1) DIV FourByteBlockSize + 1;
                FOR Block := 1 TO NumberBlocks DO
                    BEGIN
                    IF MaxAvail < SizeOf (FourByteBlockArray) THEN
                        BEGIN
                        ReportError ('Not enough memory for FourByteBlockArray!!');
                        Halt;
                        END;

                    IF DupeSheet.DupeList [Band, Mode, Block] = Nil THEN
                        New (DupeSheet.DupeList [Band, Mode, Block]);

                    BlockRead (FileRead,
                               DupeSheet.DupeList [Band, Mode, Block]^,
                               SizeOf (DupeSheet.DupeList [Band, Mode, Block]^),
                               xResult);
                    END;
                END;

    IF DupeSheet.NumberBigCalls > 0 THEN
        BEGIN
        NumberBlocks := (DupeSheet.NumberBigCalls - 1) DIV EightByteBlockSize + 1;

        FOR Block := 1 TO NumberBlocks DO
            BEGIN
            IF MaxAvail < SizeOf (EightByteBlockArray) THEN
                BEGIN
                ReportError ('Not enough memory for EightByteBlockArray!!');
                Halt;
                END;

            IF DupeSheet.BigCallList [Block] = Nil THEN
                New (DupeSheet.BigCallList [Block]);

            BlockRead (FileRead,
                       DupeSheet.BigCallList [Block]^,
                       SizeOf (DupeSheet.BigCallList [Block]^),
                       xResult);
            END;
        END;

    BlockRead (FileRead, RemainingMultDisplay, SizeOf (RemainingMultDisplay), xResult);

    WITH Multsheet DO
        BEGIN
        BlockRead (FileRead, Totals, SizeOf (Totals), xResult);

        FOR Band := Band160 TO All DO
            FOR Mode := CW TO Both DO
                IF (((MultByBand) AND (Band <> All)) OR
                   ((NOT MultByBand) AND (Band = All))) AND
                   (((MultByMode) AND (Mode <> Both)) OR
                   ((NOT MultByMode) AND (Mode = Both))) THEN
                       BEGIN
                       IF Totals [Band, Mode].NumberDomesticMults > 0 THEN
                           BEGIN
                           IF DomesticList [Band, Mode] = Nil THEN
                               New (DomesticList [Band, Mode]);
                           BlockRead (FileRead,
                                      DomesticList [Band, Mode]^,
                                      SizeOf (DomesticList [Band, Mode]^),
                                      xResult);
                           END;

                       IF Totals [Band, Mode].NumberDXMults > 0 THEN
                           BEGIN
                           IF DXList [Band, Mode] = Nil THEN
                               New (DXList [Band, Mode]);
                           BlockRead (FileRead,
                                      DXList [Band, Mode]^,
                                      SizeOf (DXList [Band, Mode]^),
                                      xResult);
                           END;

                       IF Totals [Band, Mode].NumberPrefixMults > 0 THEN
                           BEGIN
                           IF PrefixList [Band, Mode] = Nil THEN
                               New (PrefixList [Band, Mode]);
                           BlockRead (FileRead,
                                      PrefixList [Band, Mode]^,
                                      SizeOf (PrefixList [Band, Mode]^),
                                      xResult);
                           END;

                       IF Totals [Band, Mode].NumberZoneMults > 0 THEN
                           BEGIN
                           IF ZoneList [Band, Mode] = Nil THEN
                               New (ZoneList [Band, Mode]);
                           BlockRead (FileRead,
                                      ZoneList [Band, Mode]^,
                                      SizeOf (ZoneList [Band, Mode]^),
                                      xResult);
                           END;
                END;
        END;

    BlockRead (FileRead, NumberAllCalls , SizeOf (NumberAllCalls), xResult);

    IF NumberAllCalls > 0 THEN
        FOR Address := 0 TO NumberAllCalls - 1 DO
            BEGIN
            BlockRead (FileRead, AllCallList [Address], SizeOf (AllCallList [Address]), xResult);
            BlockRead (FileRead, ExchangeMemoryList [Address], SizeOf (ExchangeMemoryList [Address]), xResult);
            END;

    FOR Band := Band160 TO Band2 DO
        FOR Mode := CW TO Phone DO
            BlockRead (FileRead, FreqMemory [Band, Mode], SizeOf (FreqMemory [Band, Mode]), xResult);

    Close (FileRead);
    SetUpRemainingMultiplierArrays;
    ReadInBinFiles := True;
    {CodeSpeed := RadioOneSpeed;}
    CodeSpeed := SpeedMemory[RadioOne]; {KK1L: 6.73}
    END;



PROCEDURE DupeAndMultSheet.SheetInitAndLoad;

{ This procedure will load in the LOG.DAT file and fill up all the sheets
  with the right stuff.  Make sure that QSOByBand, QSOByMode and the
  active multiplier globals are setup before executing this.         }

VAR FileRead: TEXT;
    QSOPoints: INTEGER;
    FileString: STRING;
    TempRXData: ContestExchange;

    BEGIN
    LoadingInLogFile := False;

    IF ReadInBINFiles (False) THEN Exit;

    DisposeOfMemoryAndZeroTotals;

    IF OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        WriteLn ('Reading in ', LogFileName, '...');

        LoadingInLogFile := True;

        WriteLn;

        WHILE NOT Eof (FileRead) DO
            BEGIN
            ReadLn (FileRead, FileString);

            TempRXData.Band     := GetLogEntryBand (FileString);
            TempRXData.Mode     := GetLogEntryMode (FileString);
            TempRXData.Callsign := GetLogEntryCall (FileString);

            GetRidOfPostcedingSpaces (TempRXData.Callsign);

            IF (TempRXData.Band <> NoBand) AND (TempRXData.Mode <> NoMode) AND (TempRXData.Callsign <> '') THEN
                BEGIN
                Inc (QSOTotals [TempRXData.Band, TempRXData.Mode]);
                Inc (QSOTotals [TempRXData.Band, Both]);
                Inc (QSOTotals [All,             TempRXData.Mode]);
                Inc (QSOTotals [All,             Both]);

                GoToXY (1, WhereY);
                Write (QSOTotals [All, Both]);

                IF PartialCallLoadLogEnable THEN
                    BEGIN
                    ParseExchangeIntoContestExchange (FileString, TempRXData);
                    QSOPoints := TempRXData.QSOPoints;
                    END
                ELSE
                    BEGIN
                    QSOPoints := GetLogEntryQSOPoints (FileString);
                    TempRXData.Callsign := GetLogEntryCall (FileString);
                    END;

                IF SingleBand <> All THEN
                    IF TempRXData.Band <> SingleBand THEN
                        QSOPoints := 0;

                IF NOT (StringHas (FileString, '*DUPE*') OR StringHas (FileString, '*ZERO*')) THEN
                    BEGIN
                    TempRXData.Callsign := GetLogEntryCall (FileString);

                    IF FileString [LogEntryNameSentAddress] = '*' THEN
                        Inc (TotalNamesSent);

                    TotalQSOPoints := TotalQSOPoints + QSOPoints;

                    GetMultsFromLogEntry (FileString, TempRXData);
                    SetMultFlags (TempRXData);

                    IF DoingDomesticMults AND (TempRXData.DomesticQTH <> '') THEN
                        IF NOT TempRXData.DomesticMult THEN
                            BEGIN
                            ReportError ('Duplicate domestic multiplier found = ' + TempRXData.DomesticQTH);
                            Wait (1000);
                            END;

                    IF DoingDXMults AND (TempRXData.DXQTH <> '') THEN
                        IF NOT TempRXData.DXMult THEN
                            BEGIN
                            ReportError ('Duplicate DX multiplier found = ' + TempRXData.DXQTH);
                            END;

                    IF DoingPrefixMults AND (TempRXData.Prefix <> '') THEN
                        IF NOT TempRXData.PrefixMult THEN
                            BEGIN
                            ReportError ('Duplicate prefix multiplier found = ' + TempRXData.Prefix);
                            END;

                    IF DoingZoneMults AND (TempRXData.Zone <> '') THEN
                        IF NOT TempRXData.ZoneMult THEN
                            BEGIN
                            ReportError ('Duplicate zone multiplier found = ' + TempRXData.Zone);
                            END;

                    AddQSOToSheets (TempRXData);
                    END;
                END
            ELSE
                IF StringHas (GetLogEntryDateString (FileString), '-Jan-') OR
                   StringHas (GetLogEntryDateString (FileString), '-Feb-') OR
                   StringHas (GetLogEntryDateString (FileString), '-Mar-') OR
                   StringHas (GetLogEntryDateString (FileString), '-Apr-') OR
                   StringHas (GetLogEntryDateString (FileString), '-May-') OR
                   StringHas (GetLogEntryDateString (FileString), '-Jun-') OR
                   StringHas (GetLogEntryDateString (FileString), '-Jul-') OR
                   StringHas (GetLogEntryDateString (FileString), '-Aug-') OR
                   StringHas (GetLogEntryDateString (FileString), '-Sep-') OR
                   StringHas (GetLogEntryDateString (FileString), '-Oct-') OR
                   StringHas (GetLogEntryDateString (FileString), '-Nov-') OR
                   StringHas (GetLogEntryDateString (FileString), '-Dec-') THEN
                       BEGIN
                       IF (TempRXData.Mode <> NoMode) AND (TempRXData.Band = NoBand) THEN
                           BEGIN
                           WriteLn;
                           ReportError ('The following QSO has an invalid Band:');
                           WriteLn (FileString);
                           WaitForKeyPressed;
                           END;

                       IF (TempRXData.Band <> NoBand) AND (TempRXData.Mode = NoMode) THEN
                           BEGIN
                           WriteLn;
                           ReportError ('The following QSO has an invalid Mode:');
                           WriteLn (FileString);
                           WaitForKeyPressed;
                           END;
                       END;

            END;

        Close (FileRead);
        END;

    WriteLn ('There were ', QSOTotals [All, Both], ' QSOs loaded in.');

    SetUpRemainingMultiplierArrays;

    LoadingInLogFile := False;
    END;



PROCEDURE SetUpExchangeInformation (ActiveExchange: ExchangeType;
                                    VAR ExchangeInformation: ExchangeInformationRecord);

    BEGIN
    WITH ExchangeInformation DO
        BEGIN
        Age            := False;
        Chapter        := False;
        Check          := False;
        Classs         := False;
        Name           := False;
        PostalCode     := False;
        Power          := False;
        Precedence     := False;
        QSONumber      := False;
        QTH            := False;
        RandomChars    := False;
        RST            := False;
        Zone           := False;
        ZoneOrSociety  := False;
        END;

    CASE ActiveExchange OF
        CheckAndChapterOrQTHExchange:
            BEGIN
            ExchangeInformation.Chapter := True;
            ExchangeInformation.Check := True;
            ExchangeInformation.QTH   := True;
            END;

        ClassDomesticOrDXQTHExchange:
            BEGIN
            ExchangeInformation.Classs := True;
            ExchangeInformation.QTH   := True;
            END;

        CWTExchange:
            BEGIN
            ExchangeInformation.QTH := True;
            ExchangeInformation.QSONumber := True;
            ExchangeInformation.Name := True;
            END;

        KidsDayExchange:
            BEGIN
            ExchangeInformation.Kids := True;
            END;

        RSTAndContinentExchange:
            BEGIN
            ExchangeInformation.RST := True;
            ExchangeInformation.QTH := True;
            END;

        NameQTHAndPossibleTenTenNumber:
            BEGIN
            ExchangeInformation.Name      := True;
            ExchangeInformation.QTH       := True;
            ExchangeInformation.TenTenNum := True;
            END;

        NameAndDomesticOrDXQTHExchange:
            BEGIN
            ExchangeInformation.Name := True;
            ExchangeInformation.QTH  := True;
            END;

        NameAndPossibleGridSquareExchange:
            BEGIN
            ExchangeInformation.Name := True;
            ExchangeInformation.QTH  := True;
            END;

        NZFieldDayExchange:
            BEGIN
            ExchangeInformation.RST := True;
            ExchangeInformation.QSONumber := True;
            ExchangeInformation.Zone := True;
            END;

        QSONumberAndNameExchange:
            BEGIN
            ExchangeInformation.Name := True;
            ExchangeInformation.QSONumber := True;
            END;

        QSONumberDomesticQTHExchange,
        QSONumberDomesticOrDXQTHExchange:
            BEGIN
            ExchangeInformation.QSONumber := True;
            ExchangeInformation.QTH       := True;
            END;

        QSONumberNameChapterAndQTHExchange:
            BEGIN
            ExchangeInformation.QSONumber := True;
            ExchangeInformation.Name      := True;
            ExchangeInformation.Chapter   := True;
            ExchangeInformation.QTH       := True;
            END;

        QSONumberNameDomesticOrDXQTHExchange:
            BEGIN
            ExchangeInformation.QSONumber := True;
            ExchangeInformation.Name := True;
            ExchangeInformation.QTH := True;
            END;

        QSONumberPrecedenceCheckDomesticQTHExchange:
            BEGIN
            ExchangeInformation.QSONumber := True;
            ExchangeInformation.Precedence := True;
            ExchangeInformation.Check := True;
            ExchangeInformation.QTH := True;
            END;

        RSTAgeExchange:
            BEGIN
            ExchangeInformation.RST := True;
            ExchangeInformation.Age := True;
            END;

        RSTAllJAPrefectureAndPrecedenceExchange:
            BEGIN
            ExchangeInformation.RST        := True;
            ExchangeInformation.Precedence := True;
            ExchangeInformation.QTH        := True;
            END;

        RSTPossibleDomesticQTHAndPower:
            BEGIN
            ExchangeInformation.RST   := True;
            ExchangeInformation.QTH   := True;
            ExchangeInformation.Power := True;
            END;

        RSTAndOrGridExchange, RSTAndGridExchange:
            BEGIN
            ExchangeInformation.RST   := True;
            ExchangeInformation.QTH   := True;
            END;

        RSTAndPostalCodeExchange:
            BEGIN
            ExchangeInformation.RST         := True;
            ExchangeInformation.PostalCode  := True;
            END;

        RSTAndDomesticQTHOrZoneExchange:
            BEGIN
            ExchangeInformation.RST  := True;
            ExchangeInformation.QTH  := True;
            ExchangeInformation.Zone := True;
            END;

        RSTQTHExchange,
        RSTDomesticOrDXQTHExchange:
            BEGIN
            ExchangeInformation.RST := True;
            ExchangeInformation.QTH := True;
            END;

        RSTDomesticQTHExchange,
        RSTPrefectureExchange:
            BEGIN
            ExchangeInformation.RST := True;
            ExchangeInformation.QTH := True;
            END;

        RSTDomesticQTHOrQSONumberExchange:
            BEGIN
            ExchangeInformation.RST := True;
            ExchangeInformation.QSONumber := True;
            ExchangeInformation.QTH := True;
            END;

        RSTNameAndQTHExchange:
            BEGIN
            ExchangeInformation.RST := True;
            ExchangeInformation.Name := True;
            ExchangeInformation.QTH := True;
            END;

        RSTPowerExchange:
            BEGIN
            ExchangeInformation.RST := True;
            ExchangeInformation.Power := True;
            END;

        RSTQSONumberExchange:
            BEGIN
            ExchangeInformation.RST := True;
            ExchangeInformation.QSONumber := True;
            END;

        RSTQSONumberAndPossibleDomesticQTHExchange,
        RSTQSONumberAndDomesticQTHExchange,
        RSTQSONumberAndGridSquareExchange,
        RSTAndQSONumberOrDomesticQTHExchange:
            BEGIN
            ExchangeInformation.RST := True;
            ExchangeInformation.QSONumber := True;
            ExchangeInformation.QTH := True;
            END;

        RSTQTHNameAndFistsNumberOrPowerExchange:
            BEGIN
            ExchangeInformation.RST := True;
            ExchangeInformation.QTH := True;
            ExchangeInformation.Name := True;
            ExchangeInformation.Power := True;
            ExchangeInformation.QSONumber := True;
            END;

        RSTQSONumberAndRandomCharactersExchange:
            BEGIN
            ExchangeInformation.RST := True;
            ExchangeInformation.RandomChars := True;
            ExchangeInformation.QSONumber := True;
            END;

        RSTZoneAndPossibleDomesticQTHExchange:
            BEGIN
            ExchangeInformation.RST  := True;
            ExchangeInformation.Zone := True;
            ExchangeInformation.QTH  := True;
            END;

        RSTZoneExchange:
            BEGIN
            ExchangeInformation.RST := True;
            ExchangeInformation.Zone := True;
            END;

        RSTZoneOrSocietyExchange:
            BEGIN
            ExchangeInformation.RST  := True;
            ExchangeInformation.QTH  := True;
            ExchangeInformation.Zone := True;
            END;

        RSTLongJAPrefectureExchange: {KK1L: 6.72}
            BEGIN
            ExchangeInformation.RST  := True;
            ExchangeInformation.QTH  := True;
            END;

        END;
    END;



FUNCTION ParseExchangeIntoContestExchange (LogEntry: STRING;
                                           VAR RXData: ContestExchange): BOOLEAN;

{ Returns TRUE if it looks like a good QSO }

VAR ExchangeString: Str80;
    FirstString, SecondString: Str40;

    BEGIN
    ParseExchangeIntoContestExchange := False;

    ClearContestExchange (RXData);

    { See if it is a note }

    IF Copy (LogEntry, 1, 1) = ';' THEN Exit;

    RXData.Callsign  := GetLogEntryCall      (LogEntry);
    RXData.Band      := GetLogEntryBand      (LogEntry);
    RXData.Mode      := GetLogEntryMode      (LogEntry);
    RXData.QSOPoints := GetLogEntryQSOPoints (LogEntry);
    RXData.Date      := GetLogEntryDateString  (LogEntry);
    RXData.Time      := GetLogEntryIntegerTime (LogEntry);

    IF (RXData.Band = NoBand) OR (RXData.Mode = NoMode) THEN Exit;

    ExchangeString := GetLogEntryExchangeString (LogEntry);

    IF ExchangeInformation.RST THEN
        BEGIN
        RXData.RSTSent := RemoveFirstString (ExchangeString);

        { Sometimes the received RST is optional, so I only pull it
          off if it appears to be all there (numbers only). }

        IF StringIsAllNumbers (GetFirstString (ExchangeString)) THEN
            RXData.RSTReceived := RemoveFirstString (ExchangeString);
        END;

    IF ActiveExchange = RSTQTHNameAndFistsNumberOrPowerExchange THEN {KK1L: 6.70 for FISTS funny exchange}
        BEGIN
        RXData.QTHString := RemoveFirstString(ExchangeString);
        RXData.Name := RemoveFirstString (ExchangeString);
        {KK1L: 6.70 The power/number is right where the mults usually go}
        RXData.Power := GetLogEntryMultString (LogEntry); {KK1L: 6.70 I use power string for either number or power}
        Exit;
        END;

    IF ActiveExchange = CWTExchange THEN
        BEGIN
        { We either have a member number and name, or name and QTH }

        FirstString := RemoveFirstString (ExchangeString);
        SecondString := RemoveFirstString (ExchangeString);

        IF StringIsAllNumbers (FirstString) THEN
            BEGIN
            Val (FirstString, RXData.NumberReceived);
            RXData.Name := SecondString;
            END
        ELSE
            BEGIN
            RXData.Name := FirstString;
            RXData.QTHString := SecondString;
            END;
        Exit;
        END;

    {KK1L: 6.70 What follows is what was always here!}

    IF ExchangeInformation.Classs THEN
        RXData.Classs := RemoveFirstString (ExchangeString);

    { Sometimes the QSO number is optional - so only pull it off the
      exchange if it looks like it is there. }

    IF ExchangeInformation.QSONumber AND StringIsAllNumbers (GetFirstString (ExchangeString)) THEN
        RXData.NumberReceived := RemoveFirstLongInteger (ExchangeString);

    IF ExchangeInformation.RandomChars THEN
        BEGIN
        RXData.RandomCharsSent     := RemoveFirstString (ExchangeString);   { added in 6.27 }
        RXData.RandomCharsReceived := RemoveFirstString (ExchangeString);
        END;

    IF ExchangeInformation.PostalCode THEN
        BEGIN
        RXData.PostalCode := RemoveFirstString (ExchangeString) + ' ' + RemoveFirstString (ExchangeString);
        END;

    IF ExchangeInformation.Power THEN
        RXData.Power := RemoveFirstString (ExchangeString);

    IF ExchangeInformation.Age THEN
        RXData.Age := RemoveFirstString (ExchangeString);

    IF ExchangeInformation.Name THEN
        RXData.Name := RemoveFirstString (ExchangeString);

    IF ExchangeInformation.Chapter THEN
        RXData.Chapter := RemoveFirstString (ExchangeString);

    IF ExchangeInformation.Precedence THEN
        RXData.Precedence := RemoveFirstString (ExchangeString);

    IF ExchangeInformation.Check THEN
        RXData.Check := RemoveFirstString (ExchangeString);

    IF ExchangeInformation.Zone AND StringIsAllNumbersOrSpaces (ExchangeString) THEN
        RXData.Zone := RemoveFirstString (ExchangeString);

    IF ExchangeInformation.QTH THEN
        BEGIN
        GetRidOfPrecedingSpaces (ExchangeString);
        GetRidOfPostcedingSpaces (ExchangeString);
        RXData.QTHString := ExchangeString;
        END;

    ParseExchangeIntoContestExchange := True;
    END;



PROCEDURE DupeInit;

VAR NextEntry, ActiveVDEntry: VDEntryPointer;
    Band: BandType;
    Mode: ModeType;

    BEGIN
    ActiveVDEntry := FirstVDEntry;

    WHILE ActiveVDEntry <> nil DO
        BEGIN
        NextEntry := ActiveVDEntry^.NextEntry;
        Dispose (ActiveVDEntry);
        ActiveVDEntry := NextEntry;
        END;

    FirstVDEntry        := nil;

    ExchangeMemoryEnable       := False;
    FirstDomesticCountryRecord := nil;
    LoadingInLogFile           := False;
    MultiplierAlarm            := False;
    NumberVDCalls              := 0;
    RemainingMultDisplay       := NoRemMultDisplay;
    RestartVersionNumber       := '3,0'; { From 2.7 with new partial call / initial ex }
    TakingABreak               := False;
    TotalOffTime               := 0;

    FOR Band := Band160 TO NoBand DO
        FOR Mode := CW TO FM DO
            BEGIN
            RemMultMatrix [Band, Mode, Domestic] := nil;
            RemMultMatrix [Band, Mode, DX]       := nil;
            RemMultMatrix [Band, Mode, Zone]     := nil;
            END;
    END;



FUNCTION HeapFunc (Size: Word): INTEGER;

    BEGIN
    HeapFunc := 1;
    END;



FUNCTION GetInitialExchangeStringFromContestExchange (RData: ContestExchange): Str40;

VAR QString, TString, TempString: Str40;

    BEGIN
    GetInitialExchangeStringFromContestExchange := '';
    TempString := '';

    WITH RData DO
        BEGIN
        IF ActiveExchange = CWTExchange THEN
            BEGIN
            IF NumberReceived > 0 THEN
                BEGIN
                Str (NumberReceived, TempString);
                TempString := TempString + ' ' + Name;
                END
            ELSE
                TempString := Name + ' ' + QTHString;

            GetInitialExchangeStringFromContestExchange := TempString;
            Exit;
            END;

        IF ActiveExchange = RSTAllJAPrefectureAndPrecedenceExchange THEN
            BEGIN
            GetInitialExchangeStringFromContestExchange := Precedence + ' ' + QTHString;
            Exit;
            END;

        IF ActiveExchange = RSTDomesticQTHOrQSONumberExchange THEN
            BEGIN
            QString := QTHString;

            WHILE QString <> '' DO
                BEGIN
                TString := RemoveFirstString (QString);

                IF StringHasLowerCase (TString) THEN
                    BEGIN
                    GetInitialExchangeStringFromContestExchange := TString;
                    Exit;
                    END;
                END;
            Exit;
            END;

        IF ActiveExchange = RSTQTHNameAndFistsNumberOrPowerExchange THEN  {KK1L: 6.70}
            BEGIN
            GetInitialExchangeStringFromContestExchange := QTHString + ' ' + Name + ' ' + Power;
            Exit;
            END;

        IF ExchangeInformation.Zone  THEN TempString := Zone;
        IF ExchangeInformation.Name  THEN TempString := Name;
        IF ExchangeInformation.Classs THEN TempString := Classs;
        IF ExchangeInformation.Age   THEN TempString := Age;
        IF ExchangeInformation.Check THEN TempString := Check;

        IF ExchangeInformation.Chapter THEN
            TempString := TempString + ' ' + Chapter;

        IF ExchangeInformation.QTH THEN
            BEGIN
            QString := QTHString;

            IF (ActiveExchange = QSONumberNameChapterAndQTHExchange) OR
               (ActiveDomesticMult <> NoDomesticMults) OR
               (ActiveExchange = RSTAndOrGridExchange) THEN
                    IF TempString = '' THEN
                        TempString := QString
                    ELSE
                        TempString := TempString + ' ' + QString;
            END;

        IF ExchangeInformation.Power THEN
            IF TempString = '' THEN
                TempString := Power
            ELSE
                TempString := TempString + ' ' + Power;
        END;

    GetInitialExchangeStringFromContestExchange := TempString;
    END;



    BEGIN
{KS
    HeapError := @HeapFunc;
}
    DupeInit;
    END.
