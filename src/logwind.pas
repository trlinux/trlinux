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

UNIT LogWind;

{$O+}
{$V-}

INTERFACE

Uses LogGrid, LogDom, LogK1EA, SlowTree, Tree, trCrt, Dos, LogSCP,
     ZoneCont, Country9, Ports, datetimec, radio;

CONST
    PendingQTCArraySize = 600;

    Blink = 128;
    BandMapFileName = 'BANDMAP.BIN';

    NumberEditableLines = 5;
    MaximumReminderRecords = 100;
    MaxExchangeTemplateEntries = 20;

    QSOInformationWindowLX = 55;
    QSOInformationWindowLY =  4;
    QSOInformationWindowRX = 80;
    QSOInformationWindowRY =  7;

    RemainingMultsWindowLX =  1;
    RemainingMultsWindowLY =  8;
    RemainingMultsWindowRX = 54;
    RemainingMultsWindowRY = 13;

    BigRemainingMultsWindowLX =  1;
    BigRemainingMultsWindowLY =  8;
    BigRemainingMultsWindowRX = 80;
    BigRemainingMultsWindowRY = 13;

    MultiplierInformationWindowLX = 55;
    MultiplierInformationWindowLY =  8;
    MultiplierInformationWindowRX = 80;
    MultiplierInformationWindowRY = 12;

    EditableLogWindowLX =  1;
    EditableLogWindowLY = 14;
    EditableLogWindowRX = 80;
    EditableLogWindowRY = 18;

    BigWindowLX = 1;
    BigWindowLY = 2;
    BigWindowRX = 80;
    BigWindowRY = 18;


TYPE

    SpecialCommandType = (NoSpecialCommand, SendF1Message);

    OpModeType = (CQOpMode, SearchAndPounceOpMode);

    MultiMessage = ^Str80;

    TenMinuteRuleType = (NoTenMinuteRule, TimeOfFirstQSO);

    TenMinuteTimeRecord = RECORD
        Band: BandType;
        Mode: ModeType;
        Time: TimeRecord;
        END;

    UserInfoType = (NoUserInfo,
                   NameInfo,
                   QTHInfo,
                   CheckSectionInfo,
                   SectionInfo,
                   OldCallInfo,
                   FOCInfo,
                   GridInfo,
                   CQZoneInfo,
                   ITUZoneInfo,
                   User1Info,
                   User2Info,
                   User3Info,
                   User4Info,
                   User5Info,
                   CustomInfo);

    RemainingMultiplierType = (NoRemMultDisplay, Domestic, DX, Zone);

    TwoRadioStates = (TwoRadiosDisabled,
                      Idle,
                      CallReady,
                      StationCalled,
                      SendingExchange);


    BandMapEntry = RECORD
        Call:       EightBytes;
        Frequency:  LONGINT;
        QSXOffset:  LONGINT;    { + or - offset from Frequency }
        StatusByte: BYTE;       { Bits 0-5 = # minutes remaining (0-63)
                                   Bit 6   = Dupe   Bit 7 = Mult   }
        NextEntry:    POINTER;
        END;

    BandMapEntryPointer = ^BandMapEntry;

    BandMapSplitModeType = (ByCutoffFrequency, AlwaysPhone); {KK1L: 6.64}

    RateDisplayType = (QSOs, Points);
    HourDisplayType  = (ThisHour, LastSixtyMins, BandChanges);

    DomesticMultiplierString = STRING [6];
    DXMultiplierString       = STRING [6];
    PrefixMultiplierString   = STRING [6];
    ZoneMultiplierString     = STRING [6];

    K1EAStationInfoFieldType = (Pass, Run);

    K1EAMultiInfoRecord = RECORD
        RunFreq, PassFreq: LONGINT;
        END;

    TotalScoreMessageRecord = RECORD
        Score: LONGINT;
        MessageString: Str40;
        END;

    KeyerType = (NoKeyer, CPU, MM3);

    PendingQTCDataType = RECORD
        Time:      INTEGER;
        Call:      EightBytes;
        QSONumber: INTEGER;
        END;

    PendingQTCArrayType = ARRAY [0..PendingQTCArraySize - 1] OF PendingQTCDataType;
    PendingQTCArrayPtr  = ^PendingQTCArrayType;

    QTCDataType = RECORD
        Call:       EightBytes;
        NumberQTCs: INTEGER;
        END;

    QTCDataArrayType = ARRAY [0..500] OF QTCDataType;
    QTCDataArrayPtr  = ^QTCDataArrayType;

    WindowType = (NoWindow,
                  AlarmWindow,
                  AltCallWindow, {KK1L: 6.73}
                  BandMapWindow,
                  BandModeWindow,
                  BeamHeadingWindow,
                  BigWindow,
                  CallWindow,
                  ClockWindow,
                  CodeSpeedWindow,
                  ContestTitleWindow,
                  DateWindow,
                  DupeInfoWindow,
                  DupeSheetWindow,
                  EditableLogWindow,
                  ExchangeWindow,
                  ExtraTimeWindow,
                  FreeMemoryWindow,
                  FrequencyOneWindow,
                  FrequencyTwoWindow, {KK1L: 6.73}
                  FunctionKeyWindow,
                  HourRateWindow,
                  InsertWindow,
                  MultiplierInformationWindow,
                  NamePercentageWindow,
                  NameSentWindow,
                  PossibleCallWindow,
                  QSOInformationWindow,
                  QSONumberWindow,
                  QTCNumberWindow,
                  UserInfoWindow,
                  CountryNameWindow,
                  QuickCommandWindow,
                  QuickCommand2Window,
                  RadioWindow,
                  RemainingMultsWindow,
                  RateWindow,
                  RTTYWindow,
                  RTTYInverseWindow,
                  TotalScoreWindow,
                  TotalWindow,
                  WholeScreenWindow);

    LogEntryArray = ARRAY [1..NumberEditableLines] OF STRING [100];

    RemainingMultList = ARRAY [0..MaxNumberRemainingDomQTHs] OF BOOLEAN;
    RemainingMultListPointer = ^RemainingMultList;

    ColorRecord = RECORD
        AlarmWindowBackground: INTEGER;
        AlarmWindowColor:      INTEGER;
        AltCallWindowBackground: INTEGER; {KK1L: 6.73}
        AltCallWindowColor:      INTEGER; {KK1L: 6.73}

        BandMapWindowBackground:     INTEGER;
        BandMapWindowColor:          INTEGER;

        BandModeWindowBackground:    INTEGER;
        BandModeWindowColor:         INTEGER;
        BeamHeadingWindowBackground: INTEGER;
        BeamHeadingWindowColor:      INTEGER;
        BigWindowBackground:         INTEGER;
        BigWindowColor:              INTEGER;

        CallWindowBackground:        INTEGER;
        CallWindowColor:             INTEGER;
        ClockWindowBackground:       INTEGER;
        ClockWindowColor:            INTEGER;
        CodeSpeedWindowBackground:   INTEGER;
        CodeSpeedWindowColor:        INTEGER;
        ContestTitleWindowBackground:INTEGER;
        ContestTitleWindowColor:     INTEGER;
        CountryNameWindowBackground: INTEGER;
        CountryNameWindowColor:      INTEGER;

        DateWindowBackground:      INTEGER;
        DateWindowColor:           INTEGER;
        DupeInfoWindowBackground:  INTEGER;
        DupeInfoWindowColor:       INTEGER;
        DupeSheetWindowBackground: INTEGER;
        DupeSheetWindowColor:      INTEGER;

        EditableLogWindowBackground:   INTEGER;
        EditableLogWindowColor:        INTEGER;
        ExchangeWindowBackground:      INTEGER;
        ExchangeWindowColor:           INTEGER;
        ExchangeSAndPWindowBackground: INTEGER;

        FreeMemoryWindowBackground: INTEGER;
        FreeMemoryWindowColor:      INTEGER;
        FrequencyOneWindowBackground: INTEGER;
        FrequencyTwoWindowBackground: INTEGER; {KK1L: 6.73}
        FrequencyOneWindowHighlight:  INTEGER; {KK1L: 6.73}
        FrequencyTwoWindowHighlight:  INTEGER; {KK1L: 6.73}
        FrequencyOneWindowColor:      INTEGER;
        FrequencyTwoWindowColor:      INTEGER; {KK1L: 6.73}
        FunctionKeyWindowBackground:      INTEGER;
        FunctionKeyWindowColor:           INTEGER;

        InsertWindowBackground: INTEGER;
        InsertWindowColor:      INTEGER;

        MultiplierInformationWindowBackground: INTEGER;
        MultiplierInformationWindowColor:      INTEGER;

        NamePercentageWindowBackground: INTEGER;
        NamePercentageWindowColor:      INTEGER;
        NameSentWindowBackground:       INTEGER;
        NameSentWindowColor:            INTEGER;

        PossibleCallWindowBackground:     INTEGER;
        PossibleCallWindowColor:          INTEGER;
        PossibleCallWindowDupeBackground: INTEGER;
        PossibleCallWindowDupeColor:      INTEGER;

        QSOInformationWindowBackground: INTEGER;
        QSOInformationWindowColor:      INTEGER;
        QSONumberWindowBackground:      INTEGER;
        QSONumberWindowColor:           INTEGER;
        QTCNumberWindowBackground:      INTEGER;
        QTCNumberWindowColor:           INTEGER;
        QuickCommandWindowBackground:   INTEGER;
        QuickCommandWindowColor:        INTEGER;

        RadioOneWindowBackground: INTEGER; {KK1L: 6.73 Changed from RadioWindowBackground}
        RadioOneWindowColor:      INTEGER; {KK1L: 6.73 Changed from RadioWindowColor}
        RadioTwoWindowBackground: INTEGER; {KK1L: 6.73}
        RadioTwoWindowColor:      INTEGER; {KK1L: 6.73}

        RateWindowBackground:  INTEGER;
        RateWindowColor:       INTEGER;

        RemainingMultsWindowSubdue:     INTEGER;
        RemainingMultsWindowBackground: INTEGER;
        RemainingMultsWindowColor:      INTEGER;

        RTTYWindowBackground:     INTEGER;
        RTTYWindowColor:          INTEGER;

        RTTYInverseWindowBackground:     INTEGER;
        RTTYInverseWindowColor:          INTEGER;

        TotalScoreWindowBackground: INTEGER;
        TotalScoreWindowColor:      INTEGER;
        TotalWindowBackground:      INTEGER;
        TotalWindowColor:           INTEGER;

        UserInfoWindowColor: INTEGER;
        UserInfoWindowBackground: INTEGER;

        WholeScreenColor: INTEGER;
        WholeScreenBackground: INTEGER;
        END;

    ReminderRecord = RECORD
        Time: INTEGER;
        DateString: STRING [10];
        DayString:  STRING [10];
        Message: Str40;
        Alarm: BOOLEAN;
        END;

    ReminderArray = ARRAY [0..MaximumReminderRecords - 1] OF ReminderRecord;
    ReminderArrayPointer = ^ReminderArray;

    RateRecord = RECORD
        QSOs:   BYTE;
        Points: BYTE;
        END;

    StringPointer = ^STRING;

    InitialExchangeCursorPosType = (AtStart, AtEnd);

    InitialExchangeType = (NoInitialExchange,
                           NameInitialExchange,
                           NameQTHInitialExchange,
                           CheckSectionInitialExchange,
                           SectionInitialExchange,
                           QTHInitialExchange,
                           FOCInitialExchange,
                           GridInitialExchange,
                           ZoneInitialExchange,
                           User1InitialExchange,
                           User2InitialExchange,
                           User3InitialExchange,
                           User4InitialExchange,
                           User5InitialExchange,
                           CustomInitialExchange);

    CallWindowPositionType = (NormalCallWindowPosition, UpOneCallWindowPosition);

    ExchangeType = (UnknownExchange,
                    NoExchangeReceived,
                    CheckAndChapterOrQTHExchange,
                    ClassDomesticOrDXQTHExchange,
                    KidsDayExchange,
                    NameAndDomesticOrDXQTHExchange,
                    NameQTHAndPossibleTenTenNumber,
                    NameAndPossibleGridSquareExchange,
                    NZFieldDayExchange,
                    QSONumberAndNameExchange,
                    QSONumberDomesticOrDXQTHExchange,
                    QSONumberDomesticQTHExchange,
                    QSONumberNameChapterAndQTHExchange,
                    QSONumberNameDomesticOrDXQTHExchange,
                    QSONumberPrecedenceCheckDomesticQTHExchange,
                    RSTAgeExchange,
                    RSTALLJAPrefectureAndPrecedenceExchange,
                    RSTAndContinentExchange,
                    RSTAndDomesticQTHOrZoneExchange,
                    RSTAndGridExchange,
                    RSTAndOrGridExchange,
                    RSTAndQSONumberOrDomesticQTHExchange,
                    RSTAndPostalCodeExchange,
                    RSTDomesticOrDXQTHExchange,
                    RSTDomesticQTHExchange,
                    RSTDomesticQTHOrQSONumberExchange,
                    RSTNameAndQTHExchange,
                    RSTPossibleDomesticQTHAndPower,
                    RSTPowerExchange,
                    RSTPrefectureExchange,
                    RSTQSONumberAndDomesticQTHExchange,
                    RSTQSONumberAndGridSquareExchange,
                    RSTQSONumberAndPossibleDomesticQTHExchange,
                    QSONumberAndPossibleDomesticQTHExchange, {KK1L: 6.73 For MIQP originally}
                    RSTQSONumberAndRandomCharactersExchange,
                    RSTQTHNameAndFistsNumberOrPowerExchange,
                    RSTQSONumberExchange,
                    RSTQTHExchange,
                    RSTZoneAndPossibleDomesticQTHExchange,
                    RSTZoneExchange,
                    RSTZoneOrSocietyExchange,
                    RSTLongJAPrefectureExchange); {KK1L: 6.72 JA}

    QSOPointMethodType = (NoQSOPointMethod,            { Score = 0 }
                          AllAsianQSOPointMethod,
                          ARCIQSOPointMethod,
                          ARIQSOPointMethod,
                          ARRLFieldDayQSOPointMethod,
                          ARRL160QSOPointMethod,
                          ARRL10QSOPointMethod,
                          ARRLDXQSOPointMethod,
                          ARRLVHFQSOPointMethod,
                          ARRLVHFSSPointMethod,
                          BalticQSOPointMethod,
                          CQ160QSOPointMethod,
                          CQMQSOPointMethod,
                          CQVHFQSOPointMethod,
                          CQWWQSOPointMethod,
                          CQWWRTTYQSOPointMethod,
                          CQWPXQSOPointMethod,
                          CQWPXRTTYQSOPointMethod,
                          CroatianQSOPointMethod,
                          EuropeanFieldDayQSOPointMethod,
                          EuropeanSprintQSOPointMethod,
                          EuropeanVHFQSOPointMethod,
                          FistsQSOPointMethod,
                          HADXQSOPointMethod,
                          HelvetiaQSOPointMethod,
                          IARUQSOPointMethod,
                          InternetSixQSOPointMethod,
                          IOTAQSOPointMethod,
                          JapanInternationalDXQSOPointMethod,
                          KCJQSOPointMethod,
                          MMCQSOPointMethod,
                          MQPQSOPointMethod,
                          NZFieldDayQSOPointMethod,
                          OKDXQSOPointMethod,
                          RACQSOPointMethod,
                          RSGBQSOPointMethod,
                          RussianDXQSOPointMethod,
                          SalmonRunQSOPointMethod,
                          ScandinavianQSOPointMethod,
                          SouthAmericanQSOPointMethod,
                          SouthAmericanWWQSOPointMethod,
                          SLFivePointQSOMethod,
                          StewPerryQSOPointMethod,
                          TenTenQSOPointMethod,
                          TOECQSOPointMethod,
                          UBAQSOPointMethod,
                          UkrainianQSOPointMethod,
                          VKZLQSOPointMethod,
                          WAEQSOPointMethod,
                          WAGQSOPointMethod,
                          WWLQSOPointMethod,
                          YODXQSOPointMethod,
                          AlwaysOnePointPerQSO,    { Ignores dupes }
                          OnePointPerQSO,
                          TwoPointsPerQSO,
                          ThreePointsPerQSO,
                          TwoPhoneFourCW,
                          OnePhoneTwoCW,
                          ThreePhoneFiveCW,
                          TwoPhoneThreeCW,
                          TenPointsPerQSO,
                          OneEuropeTwoOther); {KK1L: 6.67 For WRTC 2002}

    ZoneMultType   = (NoZoneMults, CQZones, ITUZones,
                      JAPrefectures, BranchZones, EUHFCYear);




VAR
    ActiveBackground:       BYTE;
    ActiveBand:             BandType;
    ActiveColor:            BYTE;
    ActiveExchange:         ExchangeType;
    ActiveInitialExchange:  InitialExchangeType;
    ActiveMode:             ModeType;
    ActiveQSOPointMethod:   QSOPointMethodType;
    ActiveWindow:           WindowType;
    ActiveZoneMult:         ZoneMultType;
    AlarmSet:               BOOLEAN;
    AlarmMinute:            INTEGER;
    AlarmHour:              INTEGER;
    AltDBufferEnable:       BOOLEAN;
    AskForFrequencies:      BOOLEAN;
    AutoReturnToCQMode:     BOOLEAN;
    AutoSAPEnableRate:      LONGINT; {KK1L 6.72}
    AutoSendCharacterCount: INTEGER;
    AutoSendEnable:         BOOLEAN;
    AutoTimeIncrementQSOs:  BYTE;
    AutoTimeQSOCount:       BYTE;

    BandMapAllBands:            BOOLEAN;
    BandMapAllModes:            BOOLEAN;
    BandMapBand:                BandType;
    BandMapMode:                ModeType;
    BandMapBlinkingCall:        CallString;
    BandMapBlinkingCallRecord:  BandMapEntryPointer;
    BandMapCursorData:          BandMapEntryPointer;
    BandMapCursorFrequency:     LONGINT;
    BandMapDisplayCQ:           BOOLEAN;
    BandMapModeCutoffFrequency: ARRAY [Band160..Band2] OF LONGINT;
    BandMapDecayTime:           INTEGER;
    BandMapDecayValue:          INTEGER; {KK1L: 6.65}
    BandMapDecayMultiplier:     INTEGER; {KK1L: 6.65}
    BandMapDupeDisplay:         BOOLEAN;
    BandMapEnable:              BOOLEAN;
    BandMapEntryInCallWindow:   BOOLEAN;
    BandMapFileVersion:         CHAR;
    {KK1L: 6.65 Expanded array to cover all cases to keep BM from going whacko when tuning out of band}
    BandMapFirstEntryList:      ARRAY [Band160..NoBand, CW..FM] OF BandMapEntryPointer;
    BandMapGuardBand:           INTEGER;
    BandMapMultsOnly:           BOOLEAN; {KK1L: 6.68}
    BandMapSplitMode:           BandMapSplitModeType; {KK1L: 6.64}
    BandMapTotalCalls:          INTEGER;
    BandMapWindowRY:            INTEGER;

    BandChangesThisHour: INTEGER;
    BandMemory: ARRAY [RadioType] OF BandType;

    CalledFromCQMode:            BOOLEAN; {KK1L: 6.73}
    CallLastTimeIWasHere:        CallString; {KK1L: 6.73 Used in LOGSUBS2 to trigger band map info update}
    CallWindowPosition:          CallWindowPositionType;
    CallWindowShowAllSpots:      BOOLEAN; {KK1L: 6.65}
    ColorColors:                 ColorRecord;
    CommandUseInactiveRadio:     BOOLEAN; {KK1L: 6.73}
    ContactsPerPage:             INTEGER;
    ContinentString:             Str20;
    ContestName:                 Str40; {KK1L: 6.68 shortened from Str80}
    ContestTitle:                Str80;
    ContinentQSOCount:           ARRAY [Band160..Band10, ContinentType] OF INTEGER;
    ControlBMemory:              CallString;
    CodeSpeedIncrement:          INTEGER; {KK1L: 6.72}
    CountryString:               Str20;
    CurrentFreq:                 ARRAY [RadioType] OF LONGINT; {KK1L: 6.73}
    CustomInitialExchangeString: Str40; {KK1L: 6.67 was Str80;}
    CustomUserString:            Str40; {KK1L: 6.68 shortened from Str80}
    CWEnabled:                   BOOLEAN;

    DigitalModeEnable:       BOOLEAN;
    DisplayBandMapEnable:    BOOLEAN; {KK1L: 6.73}
    DisplayedCodeSpeed: BYTE;
    DisplayedFrequency: LONGINT;
    DVPEnable:      BOOLEAN;
    DXMultLimit:                  INTEGER; {KK1L: 6.65}

    EditableLogDisplayed:         BOOLEAN;
    EscapeFromEditBandMap:        BOOLEAN;
    NeedToSynchBandMap:           BOOLEAN; {KK1L: 6.69}
    ExchangeFunctionKeyMenu:      Str80;
    ExchangeTemplateList:         ARRAY [0..MaxExchangeTemplateEntries - 1] OF StringPointer;
    ExchangeWindowCursorPosition: BYTE;

    FirstDisplayableBandMapFrequency: LONGINT; {KK1L: 6.64}
    FirstDisplayedBandMapColumn: INTEGER; {KK1L: 6.64 allows tracking of bandmap display overflow}
    FirstDisplayedBandMapFrequency: LONGINT; {KK1L: 6.64}
    FoundCursor: BOOLEAN; {KK1L: 6.65 Made FoundCursor global. DisplayBandMap will set false if not within guardband}
    FrequencyDisplayed:    BOOLEAN;
    FreqMemory: ARRAY [BandType, ModeType] OF LONGINT;
    FrequencyMemoryEnable: BOOLEAN;
    FreqPollRate: WORD; {KK1L: 6.71a Frequency Poll Rate in milliseconds}

    GridSquareListShown: BOOLEAN;

    HourDisplay: HourDisplayType;

    IncrementTimeEnable: BOOLEAN;
    InEditBandMap: BOOLEAN; {KK1L 6.65}
    InitialExchangeCursorPos: InitialExchangeCursorPosType;

    JustLoadingBandMapWithNoRadio: BOOLEAN; {KK1L: 6.68}

    K1EAMultiInfo: ARRAY [Band160 .. Band10] OF K1EAMultiInfoRecord;

    LastBand:                      BandType;
    LastCQFrequency:               LONGINT;
    LastCQMode:                    ModeType;
    LastDisplayableBandMapFrequency:          LONGINT; {KK1L: 6.64}
    LastDisplayedBand:             BandType;
    LastDisplayedBandMapFrequency: LONGINT; {KK1L: 6.64}
    LastDisplayedDate:             Str20;
    LastDisplayedFMMode:           BOOLEAN;
    LastDisplayedFreq:             ARRAY [RadioType] of LONGINT; {KK1L: 6.73}
    LastDisplayedMode:             ModeType;
    LastDisplayedTime:             Str20;
    LastDisplayedHour:             Str20;
    LastEditedBandMapEntry: INTEGER;
    LastFullTimeString:  Str20;
    LastSecond100:       WORD;    {KK1L: 6.71a Will store the hundredths of seconds since UpdateTimeAndRateDisplays }
                                  {           was called. Used to support wicked fast radio checking               }
    LastHeadingShown:    INTEGER;
    LastQTCTimeSent:     Str20;
    LastRadioOneBand:    BandType;
    LastRadioOneMode:    ModeType;
    LastRadioTwoBand:    BandType;
    LastRadioTwoMode:    ModeType;
    LogConfigFileName:   Str40;
    LogFileName:         Str40;
    LogRestartFileName:  Str40;
    LogSubTitle:         Str40; {KK1L: 6.68 shortened from Str80}
    LogTempFileName:     Str40;

    MaximumDisplayableRemainingMults: BYTE;
    MaxNumberOfZones: INTEGER;

    MinutesSinceLastBMUpdate: INTEGER; {KK1L: 6.65}

    ModeMemory: ARRAY [RadioType] OF ModeType;

    MonoColors:           ColorRecord;
    MouseEnable:          BOOLEAN;
    MultiInfoMessage:     Str20;
    MultiStatus:          ARRAY [BandType, ModeType] OF MultiMessage;
    MultipleBandsEnabled: BOOLEAN;
    MultipleModesEnabled: BOOLEAN;

    MyCall:               Str20;
    MyCheck:              Str10;  {KK1L: 6.67 Was Str20}
    MyContinent:          ContinentType;
    MyCountry:            Str20;
    MyCounty:             Str20;
    MyFDClass:            Str10;
    MyName:               Str20;
    MyPostalCode:         Str20;
    MyPrec:               Str10;  {KK1L: 6.67 Was Str20}
    MySection:            Str10;  {KK1L: 6.67 Was Str20}
    MyState:              Str10;  {KK1L: 6.67 Was Str20}
    MyZone:               ZoneMultiplierString;

    NoMultMarineMobile: BOOLEAN; {KK1L: 6.68 Added for WRTC 2002 as flag to not count /MM or /AM as mults or countries}
    NumberBandMapEntries: INTEGER; {KK1L: 6.64 needed a global version of NumberVisibleBMEntries}
    NumberContactsThisMinute:  BYTE;
    NumberExchangeTemplates:   SHORTINT;
    NumberQSOPointsThisMinute: INTEGER;

    NextQTCToBeAdded:   INTEGER;
    NextQTCToBeSent:    INTEGER;
    NumberQTCBooksSent: INTEGER;
    NumberQTCStations:  INTEGER;

    NumberReminderRecords:    BYTE;
    NumberTotalScoreMessages: BYTE;

    OkayToPutUpBandMapCall: BOOLEAN;
    OnDeckCall:             CallString;
    OpMode:                 OpModeType;

    PacketMessMode:         BOOLEAN;
    PacketReturnCount:      INTEGER;
    PacketReturnPerMinute:  INTEGER;
    PacketSpotComment:      Str20;   {KK1L: 6.68 Need to change when added to LOGCFG.DAT read.}
    PendingQTCArray:        PendingQTCArrayPtr;
    PrefixInfoFileName:     Str40;
    PreviousRadioOneFreq:   LONGINT; {KK1L: 6.71b To fix AutoSAPModeEnable}
    PreviousRadioTwoFreq:   LONGINT; {KK1L: 6.71b To fix AutoSAPModeEnable}

    QSOPointsDomesticCW:    INTEGER;
    QSOPointsDomesticPhone: INTEGER;
    QSOPointsDXCW:          INTEGER;
    QSOPointsDXPhone:       INTEGER;
    QTCDataArray:           QTCDataArrayPtr;
    QTCsEnabled:            BOOLEAN;
    QTCMinutes:             BOOLEAN;

    RadioOnTheMove:         ARRAY [RadioType] of BOOLEAN; {KK1L: 6.73 slowly changing over to arrayed radio varibles}
    RadioMovingInBandMode:  ARRAY [RadioType] of BOOLEAN; {KK1L: 6.73}

    RadioOneName: STRING [10];
    RadioTwoName: STRING [10];

    Radio1TrackingEnable: BOOLEAN;
    Radio2TrackingEnable: BOOLEAN;

    Radio1UpdateSeconds: INTEGER;
    Radio2UpdateSeconds: INTEGER;

    Rate:            INTEGER;
    RateDisplay:     RateDisplayType;
    RateMinuteArray: ARRAY [1..60] OF RateRecord;
    ReminderPostedCount:  INTEGER;
    Reminders:       ReminderArrayPointer;

    RTTYX, RTTYY: SHORTINT;
    RTTYInverseX, RTTYInverseY: SHORTINT;

    SayHiEnable:            BOOLEAN;
    SCPDupeBackground:      INTEGER;
    SCPDupeColor:           INTEGER;
    ScreenHeight:           INTEGER;
    SearchAndPounceMode:    BOOLEAN;
    SelectedColors:         ColorRecord;
    SendAltDSpotsToPacket:  BOOLEAN;
    SpecialCommand:         SpecialCommandType;
    SplitFreq:              LONGINT;

    SpeedMemory: ARRAY [RadioType] OF INTEGER; {KK1L: 6.73}
    SpeedString: Str10; {KK1L: 6.73}

    SprintQSYRule:          BOOLEAN;
    StartingFrequencies:    ARRAY [BandType] OF LONGINT;
    StoppingFrequencies:    ARRAY [BandType] OF LONGINT;
    SuperDupeSheet:         BOOLEAN;
    SuperDupeSheetWindowRY: INTEGER;
    SwapPacketSpotRadios:   BOOLEAN;

    TenMinuteRule:  TenMinuteRuleType;
    TenMinuteTime:  TenMinuteTimeRecord;
    TimeSpentByBand: ARRAY [Band160..Band10] OF INTEGER;

    TotalThisHour:  INTEGER;
    TotalLastSixty: INTEGER;
    TotalScoreMessages:     ARRAY [0..10] OF TotalScoreMessageRecord;
    TRFree:                 BOOLEAN;
    TwoRadioState:          TwoRadioStates;
    TuneDupeCheckEnable:    BOOLEAN; {KK1L: 6.73}

    UnknownCountryFileEnable: BOOLEAN;
    UnknownCountryFileName:   Str80;
    UserInfoShown:            UserInfoType;

    VGADisplayEnable:        BOOLEAN;
    VHFBandsEnabled:         BOOLEAN;
    VisibleDupeSheetChanged: BOOLEAN;
    VisibleDupeSheetEnable:  BOOLEAN;
    VisibleDupeSheetRemoved: BOOLEAN;

    WakeUpTimeOut: BYTE;
    WakeUpCount:   BYTE;
    WARCBandsEnabled: BOOLEAN;
    WideFreqDisplay:  BOOLEAN; {KK1L: 6.73}
    WRTC2002: BOOLEAN; {KK1l: 6.68}



  PROCEDURE ActivateExchangeWindow;

  PROCEDURE AddBandMapModeCutoffFrequency (Freq: LONGINT);

  PROCEDURE AddBandMapEntry (Call: CallString;
                             Frequency: LONGINT;
                             QSXFrequency: LONGINT;
                             Mode: ModeType;
                             Dupe: BOOLEAN;
                             Mult: BOOLEAN;
                             MinutesLeft: BYTE);

  PROCEDURE AddBandMapPlaceHolder; {KK1L: 6.65}

  PROCEDURE AddQSOToPendingQTCList (Time: INTEGER; Call: CallString; QSONumber: INTEGER);

  FUNCTION  BandMapExpandedString (Call: EightBytes): CallString;

  PROCEDURE CalculateNumberVisibleBandMapEntries (VAR NumberVisibleBandMapEntries: INTEGER;
                                                  VAR CursorEntryNumber: INTEGER;
                                                  StartAtTop: BOOLEAN);
  PROCEDURE CheckBand (Band: BandType);
  PROCEDURE CheckRTTY;
  PROCEDURE ClearAutoSendDisplay;
  PROCEDURE ClearWindow (WindowName: WindowType);

  PROCEDURE DecrementQTCCount (Call: CallString; Count: INTEGER);

  PROCEDURE DeleteBandMapEntry (VAR Entry: BandMapEntryPointer);

  PROCEDURE DisplayAutoSendCharacterCount;

  PROCEDURE DisplayBandMap;

  PROCEDURE DisplayBandMode (Band: BandType; Mode: ModeType; UpdateRadio: BOOLEAN);
  PROCEDURE DisplayBeamHeading (Call: CallString);

  PROCEDURE DisplayCodeSpeed (WPM: INTEGER;
                              SendingEnabled: BOOLEAN;
                              DVPEnabled: BOOLEAN;
                              Mode: ModeType);

  PROCEDURE DisplayContestTitle (Title: Str80);
  PROCEDURE DisplayCountryName (Call: CallString);
  PROCEDURE DisplayCT1BOHData;
  PROCEDURE DisplayEditableLog (EditableLog: LogEntryArray);
  PROCEDURE DisplayFreeMemory;
  PROCEDURE DisplayFrequency (Freq: LONGINT; Radio: RadioType); {KK1L: 6.73 Added Radio}
  PROCEDURE DisplayInsertMode (InsertMode: BOOLEAN);
  PROCEDURE DisplayMultiMessages;
  PROCEDURE DisplayNameSent (Name: Str80);
  PROCEDURE DisplayNamePercentage (TotalNamesSent: INTEGER; QSONumber: INTEGER);
  PROCEDURE DisplayNextQSONumber (QSONumber: INTEGER);
  PROCEDURE DisplayPossibleCalls (VAR PossibleCalls: PossibleCallRecord);
  PROCEDURE DisplayPrefixInfo (Prefix: Str20);
  PROCEDURE DisplayQTCNumber (QTCNumber: INTEGER);
  PROCEDURE DisplayRadio (Radio: RadioType);
  PROCEDURE DisplayRate (Rate: INTEGER);

  PROCEDURE DisplayRemainingMults (RemainingMults: RemainingMultListPointer;
                                   RemMultDisplay: RemainingMultiplierType);

  PROCEDURE DisplayTotalScore (Score: LONGINT);
  PROCEDURE DisplayUserInfo (Call: CallString);

  PROCEDURE EditBandMap;

  {KK1L: 6.64 Created procedure because I needed to get this info more than once}
  PROCEDURE GetBandMapDisplayInfo (VAR MaxEntriesPerPage: INTEGER; VAR NumberBandMapRows: INTEGER);
  PROCEDURE GetBandMapBandModeFromFrequency (Frequency: LONGINT;
                                             VAR Band: BandType;
                                             VAR Mode: ModeType);

  {KK1L: 6.64 created a function for this step used in EditBandMap}
  FUNCTION GetRecordForBandMapCursor (VAR Entry: BandMapEntryPointer;
                                          CursorEntryNumber: INTEGER) : BOOLEAN;

  PROCEDURE IncrementTime (Count: INTEGER);

  PROCEDURE LoadBandMap;
  PROCEDURE LookForOnDeckCall (VAR ExchangeString: Str80);

  FUNCTION  NextNonDupeEntryInBandMap (Band: BandType; Mode: ModeType): BOOLEAN;
  FUNCTION  NextMultiplierEntryInBandMap (Band: BandType; Mode: ModeType): BOOLEAN; {KK1L: 6.68}
  {KK1L: 6.64 created to allow traversal of entrire displayed bandmap}
  FUNCTION  NextNonDupeEntryInDisplayedBandMap (Band: BandType; Mode: ModeType): BOOLEAN;
  FUNCTION  NextMultiplierEntryInDisplayedBandMap (Band: BandType; Mode: ModeType): BOOLEAN; {KK1L: 6.68}
  FUNCTION  NumberAvailableQTCsForThisCall (Call: CallString): INTEGER;

  PROCEDURE OutputQSOorMultStatusString (OutputString: Str160);

  FUNCTION  PopNextQTCToBeSent (VAR FullString: Str80): Str80;
  PROCEDURE PutUpExchangeWindow;

  PROCEDURE QuickDisplay (Prompt: Str80);
  PROCEDURE QuickDisplay2 (Prompt: Str80);   { Next line down }
  PROCEDURE QuickDisplayError (Prompt: Str80);

  FUNCTION  QuickEditInteger (Prompt: Str80; MaxInputLength: INTEGER): LONGINT;
  FUNCTION  QuickEditFreq (Prompt: Str80; MaxInputLength: INTEGER): LONGINT;
  FUNCTION  QuickEditReal (Prompt: Str80; MaxInputLength: INTEGER): REAL;
  FUNCTION  QuickEditResponse (Prompt: Str80;
                               MaxInputLength: INTEGER): Str80;

  PROCEDURE RemoveAndRestorePreviousWindow;
  PROCEDURE RemoveWindow (WindowName: WindowType);
  PROCEDURE ResetBandMapTimes; {KK1L: 6.70}
  PROCEDURE ResetSavedWindowListAndPutUpCallWindow;
  PROCEDURE RestorePreviousWindow;

  PROCEDURE SaveActiveWindow;
  PROCEDURE SaveBandMap;
  PROCEDURE SetBackground (Color: INTEGER);
  PROCEDURE SetColor      (Color: INTEGER);
  PROCEDURE SaveAndSetActiveWindow (WindowName: WindowType);
  PROCEDURE SaveSetAndClearActiveWindow (WindowName: WindowType);
  PROCEDURE SetUpBandMapEntry (BandMapData: BandMapEntryPointer; Radio: RadioType); {KK1L: 6.73 Added Radio}
  PROCEDURE SetWindow (WindowName: WindowType);
  PROCEDURE SetActiveWindow   (WindowName: WindowType);   { Does not clear window }

  FUNCTION  TotalNumberQTCsProcessed: INTEGER;
  FUNCTION  TotalPendingQTCs: INTEGER;

  PROCEDURE UpdateBlinkingBandMapCall;

  PROCEDURE UpdateK1EAStationInfo (Field: K1EAStationInfoFieldType;
                                   StationIDChar: CHAR;
                                   MessageString: Str20);

  PROCEDURE UpdateTenMinuteDate (Band: BandType; Mode: ModeType);

  PROCEDURE UpdateTimeAndRateDisplays (ShowTime: BOOLEAN; DoRadios: BOOLEAN);
  PROCEDURE WindInit;




IMPLEMENTATION

Uses LogStuff,keycode,beep,xkb,timer;

CONST
    TotalScoreWindowLX =  1;
    TotalScoreWindowLY =  1;
    TotalScoreWindowRX = 14;
    TotalScoreWindowRY =  1;

    ContestTitleWindowLX = 15;
    ContestTitleWindowLY =  1;
    ContestTitleWindowRX = 65;
    ContestTitleWindowRY =  1;

    FreeMemoryWindowLX = 66;
    FreeMemoryWindowLY =  1;
    FreeMemoryWindowRX = 80;
    FreeMemoryWindowRY =  1;

    TotalWindowLX =  1;
    TotalWindowLY =  2;
    TotalWindowRX = 53;
    TotalWindowRY =  7;

    DupeInfoWindowLX = 55;
    DupeInfoWindowLY =  2;
    DupeInfoWindowRX = 80;
    DupeInfoWindowRY =  3;

    NormalDupeSheetWindowLX = 1;
    NormalDupeSheetWindowLY = 5;
    NormalDupeSheetWindowRX = 80;
    NormalDupeSheetWindowRY = 17;

    SuperDupeSheetWindowLX =  1;
    SuperDupeSheetWindowLY = 26;
    SuperDupeSheetWindowRX = 80;

    BeamHeadingWindowLX = 50;
    BeamHeadingWindowLY = 13;
    BeamHeadingWindowRX = 80;
    BeamHeadingWindowRY = 13;

    BandMapWindowLX =  1;
    BandMapWindowLY = 26;
    BandMapWindowRX = 80;

    BandModeWindowLX =  1;
    BandModeWindowLY = 20;
    BandModeWindowRX =  7;
    BandModeWindowRY = 20;

    BandModeWindowUpLX =  1;
    BandModeWindowUpLY = 19;
    BandModeWindowUpRX =  7;
    BandModeWindowUpRY = 19;

    DateWindowLX =  8;
    DateWindowLY = 20;
    DateWindowRX = 17;
    DateWindowRY = 20;

    DateWindowUpLX =  8;
    DateWindowUpLY = 19;
    DateWindowUpRX = 17;
    DateWindowUpRY = 19;

    ClockWindowLX = 18;
    ClockWindowLY = 20;
    ClockWindowRX = 23;
    ClockWindowRY = 20;

    ClockWindowUpLX = 18;
    ClockWindowUpLY = 19;
    ClockWindowUpRX = 23;
    ClockWindowUpRY = 19;

    QSONumberWindowLX = 24;
    QSONumberWindowLY = 20;
    QSONumberWindowRX = 29;
    QSONumberWindowRY = 20;

    QSONumberWindowUpLX = 24;
    QSONumberWindowUpLY = 19;
    QSONumberWindowUpRX = 29;
    QSONumberWindowUpRY = 19;

    CallWindowLX = 30;
    CallWindowLY = 20;
    CallWindowRX = 46;
    CallWindowRY = 20;

    CallWindowUpLX = 30;
    CallWindowUpLY = 19;
    CallWindowUpRX = 41;
    CallWindowUpRY = 19;

    NameSentWindowLX = 48;
    NameSentWindowLY = 20;
    NameSentWindowRX = 58;
    NameSentWindowRY = 20;

    InsertWindowLX = 60;
    InsertWindowLY = 20;
    InsertWindowRX = 67;
    InsertWindowRY = 20;

    NamePercentageWindowLX = 69;
    NamePercentageWindowLY = 20;
    NamePercentageWindowRX = 79;
    NamePercentageWindowRY = 20;

    FrequencyOneWindowLX =  2;
    FrequencyOneWindowLY = 21;
    FrequencyOneWindowRX = 14;
    FrequencyOneWindowRY = 21;
    FrequencyTwoWindowLY = 21; {KK1L: 6.73}
    FrequencyTwoWindowRY = 21; {KK1L: 6.73}

    {IF WideFreqDisplay THEN                   }
    {    BEGIN                                 }
    {    FrequencyTwoWindowLX = 67;} {KK1L: 6.73}
    {    FrequencyTwoWindowRX = 79;} {KK1L: 6.73}
    {    END                                   }
    {ELSE                                      }
    {    BEGIN                                 }
        FrequencyTwoWindowLX = 15; {KK1L: 6.73}
        FrequencyTwoWindowRX = 27; {KK1L: 6.73}
    {    END;                                  }

    AltCallWindowLX = 58; {KK1L: 6.73}
    AltCallWindowLY = 21; {KK1L: 6.73}
    AltCallWindowRX = 80; {KK1L: 6.73}
    AltCallWindowRY = 21; {KK1L: 6.73}

    QTCNumberWindowLX = 31;
    QTCNumberWindowLY = 21;
    QTCNumberWindowRX = 45;
    QTCNumberWindowRY = 21;

    UserInfoWindowLX = 23;
    UserInfoWindowLY = 21;
    UserInfoWindowRX = 57;
    UserInfoWindowRY = 21;

    CountryNameWindowLX = 57;
    CountryNameWindowLY = 20;
    CountryNameWindowRX = 80;
    CountryNameWindowRY = 20;

    RadioWindowLX =  2;
    RadioWindowLY = 22;
    RadioWindowRX = 11;
    RadioWindowRY = 22;

    CodeSpeedWindowLX = 13;
    CodeSpeedWindowLY = 22;
    CodeSpeedWindowRX = 21;
    CodeSpeedWindowRY = 22;

    ExchangeWindowLX = 25;
    ExchangeWindowLY = 22;
    ExchangeWindowRX = 51;
    ExchangeWindowRY = 22;

    AlarmWindowLX = 53;
    AlarmWindowLY = 22;
    AlarmWindowRX = 67;
    AlarmWindowRY = 22;

    RateWindowLX = 69;
    RateWindowLY = 22;
    RateWindowRX = 79;
    RateWindowRY = 22;

    RTTYWindowLX = 1;
    RTTYWindowLY = 26;
    RTTYWindowRX = 80;

    RTTYInverseWindowLX = 1;
    RTTYInverseWindowRX = 80;

    QuickCommandWindowLX =  4;
    QuickCommandWindowLY = 23;
    QuickCommandWindowRX = 77;
    QuickCommandWindowRY = 23;

    QuickCommand2WindowLX =  4;
    QuickCommand2WindowLY = 24;
    QuickCommand2WindowRX = 77;
    QuickCommand2WindowRY = 24;

    PossibleCallWindowLX =  2;
    PossibleCallWindowLY = 24;
    PossibleCallWindowRX = 78;
    PossibleCallWindowRY = 24;

    FunctionKeyWindowLX =  1;
    FunctionKeyWindowLY = 25;
    FunctionKeyWindowRX = 80;
    FunctionKeyWindowRY = 25;

TYPE
    SavedWindow = RECORD
        CursorX:           INTEGER;
        CursorY:           INTEGER;
        UpperLeftCorner:   WORD;
        LowerRightCorner:  WORD;
        Color:             INTEGER;
        Background:        INTEGER;
        SavedActiveWindow: WindowType;
        END;



VAR
    Band: BandType;
    Continent: ContinentType;

    NumberMinutesProgramRunning: INTEGER;
    NumberSavedWindows:          INTEGER;

    Radio1InquireCount:           INTEGER;
    Radio2InquireCount:           INTEGER;

    SavedWindowList:             ARRAY [0..12] OF SavedWindow;




PROCEDURE LookForOnDeckCall (VAR ExchangeString: Str80);

VAR TempString, WordString: Str40;

    BEGIN
    OnDeckCall := '';

    IF Pos (' /', ExchangeString) > 0 THEN
        BEGIN
        TempString := '';

        WHILE Length (ExchangeString) > 0 DO
            BEGIN
            WordString := RemoveFirstString (ExchangeString);

            IF Copy (WordString, 1, 1) <> '/' THEN
                TempString := TempString + WordString + ' '
            ELSE
                BEGIN
                OnDeckCall := WordString;
                Delete (OnDeckCall, 1, 1);
                END;
            END;

        ExchangeString := TempString;
        Exit;
        END;

    { Added in 6.61 }

    IF Copy (ExchangeString, 1, 1) = '/' THEN
        BEGIN
        OnDeckCall := RemoveFirstString (ExchangeString);
        Delete (OnDeckCall, 1, 1);
        END;
    END;




FUNCTION BandMapExpandedString (Call: EightBytes): CallString;

VAR TempString: CallString;

    BEGIN
    TempString := BigExpandedString (Call);

    WHILE Pos (' ', TempString) > 0 DO
        TempString [Pos (' ', TempString)] := '/';

    BandMapExpandedString := TempString;
    END;



PROCEDURE DisplayFrequency (Freq: LONGINT; Radio: RadioType); {KK1L: 6.73 Added Radio switch}

VAR TempFreq: REAL;

    BEGIN
    IF (Freq = LastDisplayedFreq[RadioOne]) AND (Radio = RadioOne) THEN Exit; {KK1L: 6.73 Added Radio}
    IF (Freq = LastDisplayedFreq[RadioTwo]) AND (Radio = RadioTwo) THEN Exit; {KK1L: 6.73}

    {KK1L: 6.73 Added Radio=ActiveRadio. Keeps freq correct when logging with LogFrequencyEnable}
    IF (Freq <> 0) AND (Radio = ActiveRadio) THEN DisplayedFrequency := Freq;

    CASE Radio OF  {KK1L: 6.73}
        RadioOne:
            BEGIN
            LastDisplayedFreq[RadioOne] := Freq;
            SaveSetAndClearActiveWindow (FrequencyOneWindow);

            IF ActiveRadio = RadioOne THEN
                SetColor (SelectedColors.FrequencyOneWindowHighlight)
            ELSE
                SetColor (SelectedColors.FrequencyOneWindowColor);

            END;

        RadioTwo:
            BEGIN
            LastDisplayedFreq[RadioTwo] := Freq;
            SaveSetAndClearActiveWindow (FrequencyTwoWindow);

            IF ActiveRadio = RadioTwo THEN
                SetColor (SelectedColors.FrequencyTwoWindowHighlight)
            ELSE
                SetColor (SelectedColors.FrequencyTwoWindowColor);

            END;
        END;

    IF Freq <> 0 THEN
        BEGIN
        TempFreq := Freq DIV 100;
        Freq := Trunc (TempFreq);

        Write (Freq / 10:6:1, ' kHz');
        FrequencyDisplayed := True;
        END
    ELSE
        IF FrequencyDisplayed THEN
            BEGIN
            FrequencyDisplayed := False;
            ClrScr;
            END;

    RestorePreviousWindow;
    END;



PROCEDURE DisplayBeamHeading (Call: CallString);

VAR Heading, Distance: INTEGER;
    CountryID, HeadingString, DistanceString, HisGrid : Str20;
    Lat, Lon: REAL;
    TempString: Str40;

    BEGIN
    IF MyGrid <> '' THEN
        BEGIN
        HisGrid := CountryTable.GetGrid (Call, CountryID);

        IF HisGrid = '' THEN Exit;

        IF Call = MyCall THEN
            GetLatLon (MyGrid, Lat, Lon)
        ELSE
            GetLatLon (CountryTable.GetGrid (Call, CountryID), Lat, Lon);

        SaveSetAndClearActiveWindow (BeamHeadingWindow);

        IF HisGrid <> '' THEN
            BEGIN
            Heading := Round (GetBeamHeading (MyGrid, HisGrid));

            LastHeadingShown := Heading;

            Str (Heading, HeadingString);

            TempString := CountryID + ' ' + HeadingString + DegreeSymbol;

            IF DistanceMode <> NoDistanceDisplay THEN
                BEGIN
                Distance := GetDistanceBetweenGrids (MyGrid, HisGrid);

                IF DistanceMode = DistanceMiles THEN
                    BEGIN
                    Distance := Round (Distance / 1.6);
                    Str (Distance, DistanceString);
                    DistanceString := DistanceString + 'm';
                    END
                ELSE
                    BEGIN
                    Str (Distance, DistanceString);
                    DistanceString := DistanceString + 'km';
                    END;

                TempString := TempString + ' ' + DistanceString;
                END;


            TempString := TempString + ' ' + GetSunriseSunsetString (Lat, Lon);
            END
        ELSE
            TempString := 'Unknown country';


        WHILE Length (TempString) < (BeamHeadingWindowRX - BeamHeadingWindowLX) DO
            TempString := ' ' + TempString;

        Write (TempString);
        RestorePreviousWindow;
        END;
    END;

PROCEDURE SetColor (Color: INTEGER);

    BEGIN
    ActiveColor := Color;
    TextColor (Color);
    END;


PROCEDURE SetBackground (Color: INTEGER);

    BEGIN
    ActiveBackground := Color;
    TextBackground (Color);
    END;




PROCEDURE SaveActiveWindow;

{ This procedure will store information about the current active window into
  the SavedWindowList.  This is so the current window paramters can be
  restored by calling RestoreActiveWindow.  The information stored includes
  the present cursor position, the window coordinates and the colors.

  The SavedWindowList has room for 11 entries.  Normally only a few are
  ever used.  If a unbalance occurs resulting in too many entries, a
  SendMorse routine will start sending the number of windows being saved.  }

VAR MorseString: Str80;

    BEGIN
    IF NumberSavedWindows > 9 THEN NumberSavedWindows := 8;

    WITH SavedWindowList [NumberSavedWindows] DO
        BEGIN
        CursorX           := WhereX;
        CursorY           := WhereY;
        UpperLeftCorner   := WindMin;
        LowerRightCorner  := WindMax;
        Color             := ActiveColor;
        Background        := ActiveBackground;
        SavedActiveWindow := ActiveWindow;
        END;

    Inc (NumberSavedWindows);

    IF NumberSavedWindows > 7 THEN
        BEGIN
        Str (NumberSavedWindows, MorseString);
        SendMorse (MorseString + ' ');
        END;
    END;




PROCEDURE RestorePreviousWindow;

{ This is used to pop window information for the most recently saved window
  off the SavedWindowList.  If NumberOfSaved equals zero, then nothing
  happens.                                                        }

    BEGIN
    IF ActiveWindow = ExchangeWindow THEN
        ExchangeWindowCursorPosition := WhereX;

    IF NumberSavedWindows > 0 THEN
        BEGIN
        Dec (NumberSavedWindows);

        WITH SavedWindowList [NumberSavedWindows] DO
            BEGIN
            Window (Lo (UpperLeftCorner) + 1, Hi (UpperLeftCorner) + 1, Lo (LowerRightCorner) + 1, Hi (LowerRightCorner) + 1);
            SetColor (Color);
            SetBackground (Background);
            GoToXY (CursorX, CursorY);
            ActiveWindow := SavedActiveWindow;
            END;
        END;
    END;


PROCEDURE RemoveAndRestorePreviousWindow;

{ This procedure is the same as the RestorePreviousWindow procedure except
  it will remove the current active window from the display.              }

    BEGIN
    TextBackground (SelectedColors.WholeScreenBackground);
    ClrScr;
    RestorePreviousWindow;
    END;




PROCEDURE SetWindow (WindowName: WindowType);

{ This procedure will set the window paramters and colors for the specified
   window.                                                              }

    BEGIN
    CASE WindowName OF

      AlarmWindow:
          BEGIN
          Window (AlarmWindowLX, AlarmWindowLY, AlarmWindowRX, AlarmWindowRY);
          SetBackground (SelectedColors.AlarmWindowBackground);
          SetColor      (SelectedColors.AlarmWindowColor);
          END;

      AltCallWindow: {KK1L: 6.73}
          BEGIN
          Window (AltCallWindowLX, AltCallWindowLY, AltCallWindowRX, AltCallWindowRY);
          SetBackground (SelectedColors.AltCallWindowBackground);
          SetColor      (SelectedColors.AltCallWindowColor);
          END;

      BandMapWindow:
          BEGIN
          Window (BandMapWindowLX, BandMapWindowLY, BandMapWindowRX, BandMapWindowRY);
          SetBackground (SelectedColors.BandMapWindowBackground);
          SetColor      (SelectedColors.BandMapWindowColor);
          END;

      BandModeWindow:
          IF CallWindowPosition = NormalCallWindowPosition THEN
              BEGIN
              Window (BandModeWindowLX, BandModeWindowLY, BandModeWindowRX, BandModeWindowRY);
              SetBackground (SelectedColors.BandModeWindowBackground);
              SetColor      (SelectedColors.BandModeWindowColor);
              END
          ELSE
              BEGIN
              Window (BandModeWindowUpLX, BandModeWindowUpLY, BandModeWindowUpRX, BandModeWindowUpRY);
              SetBackground (SelectedColors.BandModeWindowBackground);
              SetColor      (SelectedColors.BandModeWindowColor);
              END;

      BeamHeadingWindow:
          BEGIN
          Window (BeamHeadingWindowLX, BeamHeadingWindowLY, BeamHeadingWindowRX, BeamHeadingWindowRY);
          SetBackground (SelectedColors.BeamHeadingWindowBackground);
          SetColor      (SelectedColors.BeamHeadingWindowColor);
          END;

      BigWindow:
          BEGIN
          Window (BigWindowLX, BigWindowLY, BigWindowRX, BigWindowRY);
          SetBackground (SelectedColors.BigWindowBackground);
          SetColor      (SelectedColors.BigWindowColor);
          END;

      CallWindow:
          IF CallWindowPosition = NormalCallWindowPosition THEN
              BEGIN
              Window (CallWindowLX, CallWindowLY, CallWindowRX, CallWindowRY);
              SetBackground (SelectedColors.CallWindowBackground);
              SetColor      (SelectedColors.CallWindowColor);
              END
          ELSE
              BEGIN
              Window (CallWindowUpLX, CallWindowUpLY, CallWindowUpRX, CallWindowUpRY);
              SetBackground (SelectedColors.BandMapWindowBackground);
              SetColor      (SelectedColors.BandMapWindowColor);
              END;

      ClockWindow:
          IF CallWindowPosition = NormalCallWindowPosition THEN
              BEGIN
              Window (ClockWindowLX, ClockWindowLY, ClockWindowRX, ClockWindowRY);
              SetBackground (SelectedColors.ClockWindowBackground);
              SetColor      (SelectedColors.ClockWindowColor);
              END
          ELSE
              BEGIN
              Window (ClockWindowUpLX, ClockWindowUpLY, ClockWindowUpRX, ClockWindowUpRY);
              SetBackground (SelectedColors.ClockWindowBackground);
              SetColor      (SelectedColors.ClockWindowColor);
              END;

      CodeSpeedWindow:
          BEGIN
          Window (CodeSpeedWindowLX, CodeSpeedWindowLY, CodeSpeedWindowRX, CodeSpeedWindowRY);
          SetBackground (SelectedColors.CodeSpeedWindowBackground);
          SetColor      (SelectedColors.CodeSpeedWindowColor);
          END;

      ContestTitleWindow:
          BEGIN
          Window (ContestTitleWindowLX, ContestTitleWindowLY,
                  ContestTitleWindowRX, ContestTitleWindowRY);
          SetBackground (SelectedColors.ContestTitleWindowBackground);
          SetColor      (SelectedColors.ContestTitleWindowColor);
          END;

      CountryNameWindow:
          BEGIN
          Window (CountryNameWindowLX, CountryNameWindowLY,
                  CountryNameWindowRX, CountryNameWindowRY);
          SetBackground (SelectedColors.CountryNameWindowBackground);
          SetColor      (SelectedColors.CountryNameWindowColor);
          END;

      DateWindow:
          IF CallWindowPosition = NormalCallWindowPosition THEN
              BEGIN
              Window (DateWindowLX, DateWindowLY, DateWindowRX, DateWindowRY);
              SetBackground (SelectedColors.DateWindowBackground);
              SetColor      (SelectedColors.DateWindowColor);
              END
          ELSE
              BEGIN
              Window (DateWindowUpLX, DateWindowUpLY, DateWindowUpRX, DateWindowUpRY);
              SetBackground (SelectedColors.DateWindowBackground);
              SetColor      (SelectedColors.DateWindowColor);
              END;

      DupeInfoWindow:
          BEGIN
          Window (DupeInfoWindowLX, DupeInfoWindowLY, DupeInfoWindowRX, DupeInfoWindowRY);
          SetBackground (SelectedColors.DupeInfoWindowBackground);
          SetColor      (SelectedColors.DupeInfoWindowColor);
          END;

      DupeSheetWindow:
          BEGIN
          IF SuperDupeSheet THEN
              Window (SuperDupeSheetWindowLX,
                      SuperDupeSheetWindowLY,
                      SuperDupeSheetWindowRX,
                      SuperDupeSheetWindowRY)

          ELSE
              Window (NormalDupeSheetWindowLX,
                      NormalDupeSheetWindowLY,
                      NormalDupeSheetWindowRX,
                      NormalDupeSheetWindowRY);

          SetBackground (SelectedColors.DupeSheetWindowBackground);
          SetColor      (SelectedColors.DupeSheetWindowColor);
          END;




      EditableLogWindow:
          BEGIN
          Window (EditableLogWindowLX, EditableLogWindowLY, EditableLogWindowRX, EditableLogWindowRY);
          SetBackground (SelectedColors.EditableLogWindowBackground);
          SetColor      (SelectedColors.EditableLogWindowColor);
          END;

      ExchangeWindow:
          IF OpMode = SearchAndPounceOpMode THEN
              BEGIN
              Window (ExchangeWindowLX, ExchangeWindowLY, ExchangeWindowRX, ExchangeWindowRY);
              SetBackground (SelectedColors.ExchangeSAndPWindowBackground);
              SetColor      (SelectedColors.ExchangeWindowColor);
              END
          ELSE
              BEGIN
              Window (ExchangeWindowLX, ExchangeWindowLY, ExchangeWindowRX, ExchangeWindowRY);
              SetBackground (SelectedColors.ExchangeWindowBackground);
              SetColor      (SelectedColors.ExchangeWindowColor);
              END;

      FreeMemoryWindow:
          BEGIN
          Window (FreeMemoryWindowLX, FreeMemoryWindowLY, FreeMemoryWindowRX, FreeMemoryWindowRY);
          SetBackground (SelectedColors.FreeMemoryWindowBackground);
          SetColor      (SelectedColors.FreeMemoryWindowColor);
          END;

      FrequencyOneWindow:
          BEGIN
          Window (FrequencyOneWindowLX, FrequencyOneWindowLY, FrequencyOneWindowRX, FrequencyOneWindowRY);
          SetBackground (SelectedColors.FrequencyOneWindowBackground);
          SetColor      (SelectedColors.FrequencyOneWindowColor);
          END;

      FrequencyTwoWindow: {KK1L: 6.73}
          BEGIN
          Window (FrequencyTwoWindowLX, FrequencyTwoWindowLY, FrequencyTwoWindowRX, FrequencyTwoWindowRY);
          SetBackground (SelectedColors.FrequencyTwoWindowBackground);
          SetColor      (SelectedColors.FrequencyTwoWindowColor);
          END;

      FunctionKeyWindow:
          BEGIN
          Window (FunctionKeyWindowLX, FunctionKeyWindowLY, FunctionKeyWindowRX, FunctionKeyWindowRY);
          SetBackground (SelectedColors.FunctionKeyWindowBackground);
          SetColor      (SelectedColors.FunctionKeyWindowColor);
          END;

      HourRateWindow:
          BEGIN
          Window (AlarmWindowLX, AlarmWindowLY, AlarmWindowRX, AlarmWindowRY);
          SetBackground (SelectedColors.RateWindowBackground);
          SetColor      (SelectedColors.RateWindowColor);
          END;

      InsertWindow:
          BEGIN
          Window (InsertWindowLX, InsertWindowLY, InsertWindowRX, InsertWindowRY);
          SetBackground (SelectedColors.InsertWindowBackground);
          SetColor      (SelectedColors.InsertWindowColor);
          END;

      MultiplierInformationWindow:
          BEGIN
          Window (MultiplierInformationWindowLX, MultiplierInformationWindowLY,
                  MultiplierInformationWindowRX, MultiplierInformationWindowRY);
          SetBackground (SelectedColors.MultiplierInformationWindowBackground);
          SetColor      (SelectedColors.MultiplierInformationWindowColor);
          END;

      NamePercentageWindow:
          BEGIN
          Window (NamePercentageWindowLX, NamePercentageWindowLY, NamePercentageWindowRX, NamePercentageWindowRY);
          SetBackground (SelectedColors.NamePercentageWindowBackground);
          SetColor      (SelectedColors.NamePercentageWindowColor);
          END;

      NameSentWindow:
          BEGIN
          Window (NameSentWindowLX, NameSentWindowLY, NameSentWindowRX, NameSentWindowRY);
          SetBackground (SelectedColors.NameSentWindowBackground);
          SetColor      (SelectedColors.NameSentWindowColor);
          END;




      PossibleCallWindow:
          BEGIN
          Window (PossibleCallWindowLX, PossibleCallWindowLY, PossibleCallWindowRX, PossibleCallWindowRY);
          SetBackground (SelectedColors.PossibleCallWindowBackground);
          SetColor      (SelectedColors.PossibleCallWindowColor);
          END;

      QSOInformationWindow:
          BEGIN
          Window (QSOInformationWindowLX, QSOInformationWindowLY,
                  QSOInformationWindowRX, QSOInformationWindowRY);
          SetBackground (SelectedColors.QSOInformationWindowBackground);
          SetColor      (SelectedColors.QSOInformationWindowColor);
          END;

      QSONumberWindow:
          IF CallWindowPosition = NormalCallWindowPosition THEN
              BEGIN
              Window (QSONumberWindowLX, QSONumberWindowLY, QSONumberWindowRX, QSONumberWindowRY);
              SetBackground (SelectedColors.QSONumberWindowBackground);
              SetColor      (SelectedColors.QSONumberWindowColor);
              END
          ELSE
              BEGIN
              Window (QSONumberWindowUpLX, QSONumberWindowUpLY, QSONumberWindowUpRX, QSONumberWindowUpRY);
              SetBackground (SelectedColors.QSONumberWindowBackground);
              SetColor      (SelectedColors.QSONumberWindowColor);
              END;

      QTCNumberWindow:
          BEGIN
          Window (QTCNumberWindowLX, QTCNumberWindowLY, QTCNumberWindowRX, QTCNumberWindowRY);
          SetBackground (SelectedColors.QTCNumberWindowBackground);
          SetColor      (SelectedColors.QTCNumberWindowColor);
          END;

      QuickCommandWindow:
          BEGIN
          Window (QuickCommandWindowLX, QuickCommandWindowLY, QuickCommandWindowRX, QuickCommandWindowRY);
          SetBackground (SelectedColors.QuickCommandWindowBackground);
          SetColor      (SelectedColors.QuickCommandWindowColor);
          END;

      QuickCommand2Window:
          BEGIN
          Window (QuickCommand2WindowLX, QuickCommand2WindowLY, QuickCommand2WindowRX, QuickCommand2WindowRY);
          SetBackground (SelectedColors.QuickCommandWindowBackground);
          SetColor      (SelectedColors.QuickCommandWindowColor);
          END;

      RadioWindow:
          BEGIN
          Window (RadioWindowLX, RadioWindowLY, RadioWindowRX, RadioWindowRY);
          SetBackground (SelectedColors.RadioOneWindowBackground); {KK1L: 6.73 Updated color variable}
          SetColor      (SelectedColors.RadioOneWindowColor);      {KK1L: 6.73 Updated color variable}
          END;

      RateWindow:
          BEGIN
          Window (RateWindowLX, RateWindowLY, RateWindowRX, RateWindowRY);
          SetBackground (SelectedColors.RateWindowBackground);
          SetColor      (SelectedColors.RateWindowColor);
          END;

      RTTYWindow:
          BEGIN
          Window (RTTYWindowLX, RTTYWindowLY, RTTYWindowRX, BandMapWindowRY - 5);
          SetBackground (SelectedColors.RTTYWindowBackground);
          SetColor      (SelectedColors.RTTYWindowColor);
          END;

      RTTYInverseWindow:
          BEGIN
          Window (RTTYInverseWindowLX, BandMapWindowRY - 4, RTTYInverseWindowRX, BandMapWindowRY);
          SetBackground (SelectedColors.RTTYInverseWindowBackground);
          SetColor      (SelectedColors.RTTYInverseWindowColor);
          END;

      RemainingMultsWindow:
          BEGIN
          IF BigRemainingList THEN
              Window (BigRemainingMultsWindowLX,
                      BigRemainingMultsWindowLY,
                      BigRemainingMultsWindowRX,
                      BigRemainingMultsWindowRY)
          ELSE
              Window (RemainingMultsWindowLX,
                      RemainingMultsWindowLY,
                      RemainingMultsWindowRX,
                      RemainingMultsWindowRY);

          SetBackground (SelectedColors.RemainingMultsWindowBackground);
          SetColor      (SelectedColors.RemainingMultsWindowColor);
          END;

      TotalScoreWindow:
          BEGIN
          Window (TotalScoreWindowLX, TotalScoreWindowLY, TotalScoreWindowRX, TotalScoreWindowRY);
          SetBackground (SelectedColors.TotalScoreWindowBackground);
          SetColor      (SelectedColors.TotalScoreWindowColor);
          END;

      TotalWindow:
          BEGIN
          Window (TotalWindowLX, TotalWindowLY, TotalWindowRX, TotalWindowRY);
          SetBackground (SelectedColors.TotalWindowBackground);
          SetColor      (SelectedColors.TotalWindowColor);
          END;

      UserInfoWindow:
          BEGIN
          Window (UserInfoWindowLX, UserInfoWindowLY, UserInfoWindowRX, UserInfoWindowRY);
          SetBackground (SelectedColors.UserInfoWindowBackground);
          SetColor      (SelectedColors.UserInfoWindowColor);
          END;

      WholeScreenWindow:
          BEGIN
          Window (1, 1, 80, ScreenHeight);
          SetBackground (SelectedColors.WholeScreenBackground);
          SetColor      (SelectedColors.WholeScreenColor);
          END;

      ELSE SendMorse ('Illegal window name');
      END;  { of case }

    END;




PROCEDURE SetActiveWindow (WindowName: WindowType);

{ This procedure will set up the window paramters for the specified window.
  Nothing is saved on the SavedWindowList here.                       }

    BEGIN
    SetWindow (WindowName);
    ActiveWindow := WindowName;

    IF (ActiveWindow = ExchangeWindow) AND (ExchangeWindowCursorPosition = 0) THEN
        ExchangeWindowCursorPosition := 1;
    END;



PROCEDURE SaveAndSetActiveWindow (WindowName: WindowType);

    BEGIN
    SaveActiveWindow;
    SetActiveWindow (WindowName);
    IF WindowName = ExchangeWindow THEN
        GoToXY (ExchangeWindowCursorPosition, 1)
    ELSE
        GoToXY (1,1);
    END;



PROCEDURE SaveSetAndClearActiveWindow (WindowName: WindowType);

    BEGIN
    IF ActiveWindow = ExchangeWindow THEN ExchangeWindowCursorPosition := 0;
    SaveActiveWindow;

    { Set active window - RT201 0E32:0D1B }

    SetActiveWindow (WindowName);
    ClrScr;
    END;



PROCEDURE ClearWindow (WindowName: WindowType);

{ This procedure will clear out the window specified.  The active window
  does not change.                                                      }

    BEGIN
    IF WindowName = ExchangeWindow THEN ExchangeWindowCursorPosition := 0;

    IF ActiveWindow <> WindowName  THEN
        BEGIN
        SaveAndSetActiveWindow (WindowName);
        ClrScr;
        RestorePreviousWindow;
        END
    ELSE
        ClrScr;
    END;



PROCEDURE RemoveWindow (WindowName: WindowType);

    BEGIN
    IF WindowName = ExchangeWindow     THEN ExchangeWindowCursorPosition := 0;
    IF WindowName = PossibleCallWindow THEN PossibleCallList.NumberPossibleCalls := 0;
    IF ActiveWindow <> WindowName      THEN SaveAndSetActiveWindow (WindowName);
    RemoveAndRestorePreviousWindow;
    END;





PROCEDURE DisplayBandMode (Band: BandType; Mode: ModeType; UpdateRadio: BOOLEAN);

    BEGIN
    IF (Band = LastDisplayedBand) AND
       (Mode = LastDisplayedMode) AND
       (FMMode = LastDisplayedFMMode) THEN Exit;

    LastDisplayedBand := Band;
    LastDisplayedMode := Mode;
    LastDisplayedFMMode := FMMode;

    SaveSetAndClearActiveWindow (BandModeWindow);
    Write (BandString [Band]);

    IF FMMode THEN
        Write ('FM ')
    ELSE
        CASE Mode OF
            CW:      Write ('CW ');
            Digital: Write ('DIG');
            Phone:   Write ('SSB');
            ELSE     Write (' ? ');
            END;

    RestorePreviousWindow;

    IF (ActiveRadio = RadioOne) THEN OutputBandInfo (RadioOne, Band);

    IF (ActiveRadio = RadioTwo) THEN OutputBandInfo (RadioTwo, Band);

    IF NOT UpdateRadio THEN Exit;

    IF ((ActiveRadio = RadioOne) AND (Radio1Type <> NoInterfacedRadio)) OR
       ((ActiveRadio = RadioTwo) AND (Radio2Type <> NoInterfacedRadio)) AND UpdateRadio THEN
           SetRadioFreq (ActiveRadio, FreqMemory [Band, Mode], ActiveMode, 'A');
    END;




PROCEDURE DisplayContestTitle (Title: Str80);

VAR StringLength: INTEGER;

    BEGIN
    IF Title <> '' THEN
        BEGIN
        SaveSetAndClearActiveWindow (ContestTitleWindow);
        StringLength := Length (Title);
        StringLength := StringLength DIV 2;
        StringLength := 40 - ContestTitleWindowLX - StringLength;

        FOR StringLength := 1 TO StringLength DO
            Title := ' ' + Title;

        Write (Title);
        RestorePreviousWindow;
        END;
    END;


PROCEDURE DisplayCodeSpeed (WPM: INTEGER;
                            SendingEnabled: BOOLEAN;
                            DVPEnabled: BOOLEAN;
                            Mode: ModeType);

    BEGIN
    IF Mode = CW THEN
        BEGIN
        DisplayedCodeSpeed := WPM;
        SaveSetAndClearActiveWindow (CodeSpeedWindow);
        IF SendingEnabled THEN
            Write (' ', WPM, ' WPM')
        ELSE
            Write (WPM, ' NO CW');
        RestorePreviousWindow;
        END
    ELSE
        BEGIN
        SaveSetAndClearActiveWindow (CodeSpeedWindow);

        IF DVPEnable THEN
            BEGIN
            IF DVPEnabled THEN
                Write (' DVP ON')
            ELSE
                Write (' DVP OFF');
            END
        ELSE
            IF (ActiveDVKPort <> nil) THEN
                BEGIN
                IF DVPEnabled THEN
                    Write (' DVK ON')
                ELSE
                    Write (' DVK OFF');
                END
            ELSE
                BEGIN
                TextBackground (SelectedColors.WholeScreenBackground);
                ClrScr;
                END;

        RestorePreviousWindow;
        END;

    {IF ActiveRadio = RadioOne THEN  }
    {    RadioOneSpeed := WPM        }
    {ELSE                            }
    {    RadioTwoSpeed := WPM;       }
    SpeedMemory[ActiveRadio] := WPM; {KK1L: 6.73}
    END;


PROCEDURE DisplayFreeMemory;

    BEGIN
    SaveSetAndClearActiveWindow (FreeMemoryWindow);
//    Write ('Mem =', MemAvail:7);
    Write ('Mem = A Lot');
    RestorePreviousWindow;
    END;


PROCEDURE DisplayMultiMessages;

VAR Band: BandType;
    Mode: ModeType;
    TempString: Str40;
    PassFreqStr, RunFreqStr: Str20;

    BEGIN
    SaveSetAndClearActiveWindow (EditableLogWindow);

    IF K1EANetworkEnable THEN
        BEGIN
        FOR Band := Band160 TO Band10 DO
            WITH K1EAMultiInfo [Band] DO
                BEGIN
                Str (PassFreq, PassFreqStr);
                Str (RunFreq, RunFreqStr);

                Insert ('.', PassFreqStr, Length (PassFreqStr) - 2);
                Delete (PassFreqStr, Length (PassFreqStr) - 1, 2);

                Insert ('.', RunFreqStr, Length (RunFreqStr) - 2);
                Delete (RunFreqStr, Length (RunFreqStr) - 1, 2);

                Write ('Run: ', RunFreqStr:7, '  Pass: ', PassFreqStr:7);

                IF WhereX > 40 THEN
                    BEGIN
                    WriteLn;
                    ClrEol;
                    END
                ELSE
                    GoToXY (41, WhereY);
                END;
        END

    ELSE  { TR network }
        FOR Band := Band160 TO BandLight DO
            FOR Mode := CW TO Phone DO
                IF MultiStatus [Band, Mode] <> nil THEN
                    BEGIN
                    TempString := BandString [Band] + ModeString [Mode] + ' ' + MultiStatus [Band, Mode]^;

                    Write (TempString);

                    IF WhereX > 40 THEN
                        BEGIN
                        WriteLn;
                        ClrEol;
                        END
                    ELSE
                        GoToXY (41, WhereY);
                    END;

    RestorePreviousWindow;
    END;




PROCEDURE DisplayNameSent (Name: Str80);

    BEGIN
    IF Name = '' THEN
        BEGIN
        SaveSetAndClearActiveWindow (NameSentWindow);
        RemoveAndRestorePreviousWindow;
        END
    ELSE
        BEGIN
        SaveSetAndClearActiveWindow (NameSentWindow);
        Write (' ', Name);
        RestorePreviousWindow;
        END;
    END;




PROCEDURE DisplayInsertMode (InsertMode: BOOLEAN);

    BEGIN
    RemoveWindow (CountryNameWindow);

    SaveSetAndClearActiveWindow (InsertWindow);

    IF InsertMode THEN
        BEGIN
        Write (' INSERT');
        RestorePreviousWindow;
        END
    ELSE
        RemoveAndRestorePreviousWindow;
    END;



PROCEDURE DisplayCT1BOHData;

VAR Band: BandType;
    TotalTimeOn: INTEGER;
    Percent: REAL;
    BandTotals: ARRAY [Band160..Band10] OF INTEGER;

    BEGIN
    SaveSetAndClearActiveWindow (EditableLogWindow);

    TotalTimeOn := 0;

    FOR Band := Band160 TO Band10 DO
        TotalTimeOn := TotalTimeOn + TimeSpentByBand [Band];

    WriteLn ('           160         80         40         20         15         10');

    Write   ('Time on');

    FOR Band := Band160 TO Band10 DO
        BEGIN
        IF TotalTimeOn = 0 THEN
            Percent := 0.0
        ELSE
            Percent := (TimeSpentByBand [Band] / TotalTimeOn) * 100;

        Write (TimeSpentByBand [Band]:5, Percent:5:1, '%');
        END;

    WriteLn;

    FOR Band := Band160 TO Band10 DO
        BEGIN
        BandTotals [Band] := 0;

        FOR Continent := NorthAmerica TO UnknownContinent DO
            BandTotals [Band] := BandTotals [Band] + ContinentQSOCount [Band, Continent];
        END;

    Write ('Europe ');

    FOR Band := Band160 TO Band10 DO
        BEGIN
        IF BandTotals [Band] = 0 THEN
            Percent := 0.0
        ELSE
            Percent := (ContinentQSOCount [Band, Europe] / BandTotals [Band]) * 100;

        Write (ContinentQSOCount [Band, Europe]:5, Percent:5:1, '%');
        END;

    WriteLn;

    Write ('NA     ');

    FOR Band := Band160 TO Band10 DO
        BEGIN
        IF BandTotals [Band] = 0 THEN
            Percent := 0.0
        ELSE
            Percent := (ContinentQSOCount [Band, NorthAmerica] / BandTotals [Band]) * 100;

        Write (ContinentQSOCount [Band, NorthAmerica]:5, Percent:5:1, '%');
        END;

    WriteLn;

    Write ('Asia   ');

    FOR Band := Band160 TO Band10 DO
        BEGIN
        IF BandTotals [Band] = 0 THEN
            Percent := 0.0
        ELSE
            Percent := (ContinentQSOCount [Band, Asia] / BandTotals [Band]) * 100;

        Write (ContinentQSOCount [Band, Asia]:5, Percent:5:1, '%');
        END;

    RestorePreviousWindow;
    END;


PROCEDURE DisplayEditableLog (EditableLog: LogEntryArray);

VAR Entry: INTEGER;
    TempString: Str160;

    BEGIN
    EditableLogDisplayed := True;

    SaveSetAndClearActiveWindow (EditableLogWindow);
    GridSquareListShown := False;

    FOR Entry := 1 TO NumberEditableLines DO
        BEGIN
        IF Length (EditableLog [Entry]) > 79 THEN
            BEGIN
            TempString := Copy (EditableLog [Entry], 1, 78);
            TempString := TempString + '+';
            Write (TempString);
            END
        ELSE
            Write (EditableLog [Entry]);
        IF Entry < NumberEditableLines THEN
            GoToXY (1, WhereY + 1);
        END;

    RestorePreviousWindow;
    END;



PROCEDURE DisplayNamePercentage (TotalNamesSent: INTEGER; QSONumber: INTEGER);

VAR Percentage: REAL;

    BEGIN
    IF (QSONumber > 0) AND SayHiEnable THEN
        BEGIN
        Percentage := TotalNamesSent / QSONumber;
        Percentage := Percentage * 100;
        SaveSetAndClearActiveWindow (NamePercentageWindow);
        Write (' ', Percentage:3:1, '%');
        RestorePreviousWindow;
        END;
    END;



PROCEDURE DisplayNextQSONumber (QSONumber: INTEGER);

    BEGIN
    SaveSetAndClearActiveWindow (QSONumberWindow);
    IF QSONumber > 9999 THEN
        Write (QSONumber:5)
    ELSE
        Write (QSONumber:4);
    Window (QSONumberWindowRX, QSONumberWindowRY, QSONumberWindowRX, QSONumberWindowRY);
    TextBackground (SelectedColors.WholeScreenBackground);
    ClrScr;
    RestorePreviousWindow;
    END;




PROCEDURE SortPossibleCalls (VAR PossibleCalls: PossibleCallRecord);

VAR Change: BOOLEAN;
    Range, CallAddress: INTEGER;
    Temp: PossibleCallEntry;

    BEGIN
    Range := PossibleCalls.NumberPossibleCalls - 1;

    REPEAT
        Change := False;
        FOR CallAddress := 1 TO Range DO
            WITH PossibleCalls DO
                IF List [CallAddress].Call < List [CallAddress + 1].Call THEN
                    BEGIN
                    Temp := List [CallAddress];
                    List [CallAddress] := List [CallAddress + 1];
                    List [CallAddress + 1] := Temp;
                    Change := True;
                    END;

        Dec (Range);
    UNTIL (NOT Change) OR (Range < 1);
    END;




PROCEDURE DisplayPossibleCalls (VAR PossibleCalls: PossibleCallRecord);

VAR PossibleCall: INTEGER;
    Call: CallString;
    CharacterPosition: INTEGER;

    BEGIN
    SaveSetAndClearActiveWindow (PossibleCallWindow);

    CharacterPosition := PossibleCallWindowLX;

    IF PossibleCalls.NumberPossibleCalls > 0 THEN
        BEGIN
        FOR PossibleCall := 0 TO PossibleCalls.NumberPossibleCalls - 1 DO
            BEGIN
            Call := PossibleCalls.List [PossibleCall].Call;

            IF PossibleCalls.List [PossibleCall].Dupe THEN
                BEGIN
                TextBackground (SelectedColors.PossibleCallWindowDupeBackground);
                TextColor      (SelectedColors.PossibleCallWindowDupeColor);
                END
            ELSE
                BEGIN
                TextBackground (SelectedColors.PossibleCallWindowBackground);
                TextColor      (SelectedColors.PossibleCallWindowColor);
                END;

            IF CharacterPosition + Length (Call) + 2 < PossibleCallWindowRX THEN
                BEGIN
                IF PossibleCall = PossibleCalls.CursorPosition THEN
                    Write ('<', Call, '>')
                ELSE
                    Write (' ', Call, ' ');
                CharacterPosition := CharacterPosition + Length (Call) + 2;
                END
            ELSE
                PossibleCalls.NumberPossibleCalls := PossibleCall;
            END;
        END
    ELSE
        BEGIN
        GoToXY (29, WhereY);
        Write ('No calls found');
        END;

    RestorePreviousWindow;
    END;


PROCEDURE DisplayQTCNumber (QTCNumber: INTEGER);

    BEGIN
    SaveSetAndClearActiveWindow (QTCNumberWindow);
    Write ('Have ', QTCNumber, ' QTCs');
    RestorePreviousWindow;
    END;



PROCEDURE DisplayCountryName (Call: CallString);

VAR TempString: Str80;
    Country: INTEGER;

    BEGIN

    SaveSetAndClearActiveWindow (CountryNameWindow);

    Country := CountryTable.GetCountry (Call, True);

    TempString := CountryTable.GetCountryName (Country);

    WHILE Length (TempString) > CountryNameWindowRX - CountryNameWindowLX - 2 DO
        Delete (TempString, Length (TempString), 1);

    Write (TempString);
    RestorePreviousWindow;
    END;



PROCEDURE DisplayRadio (Radio: RadioType);

    BEGIN
    SaveSetAndClearActiveWindow (RadioWindow);

    CASE Radio OF {KK1L: 6.73 Added color controls}
        RadioOne:
            BEGIN
            SetColor (SelectedColors.RadioOneWindowColor);
            SetBackground (SelectedColors.RadioOneWindowBackground);
            Write (RadioOneName);
            END;
        RadioTwo:
            BEGIN
            SetColor (SelectedColors.RadioTwoWindowColor);
            SetBackground (SelectedColors.RadioTwoWindowBackground);
            Write (RadioTwoName);
            END;
        END;

    RestorePreviousWindow;
    END;



PROCEDURE DisplayRate (Rate: INTEGER);

    BEGIN
    SaveSetAndClearActiveWindow (RateWindow);

    IF Rate < 1000 THEN
        Write ('Rate = ', Rate)
    ELSE
        Write ('Rate= ', Rate);

    RestorePreviousWindow;

    IF NOT AlarmSet THEN
        BEGIN
        SaveSetAndClearActiveWindow (HourRateWindow);

        CASE HourDisplay OF
            ThisHour:      Write ('This hr = ', TotalThisHour);
            LastSixtyMins: Write ('Last 60 = ', TotalLastSixty);
            BandChanges:   Write ('Bnd Chg = ', BandChangesThisHour);
            END;

        RestorePreviousWindow;
        END;
    END;




PROCEDURE DisplayTotalScore (Score: LONGINT);

VAR Message: INTEGER;

    BEGIN
    SaveSetAndClearActiveWindow (TotalScoreWindow);
    Write   (Score:8, ' Pts');
    RestorePreviousWindow;

    IF NumberTotalScoreMessages > 0 THEN
        FOR Message := 0 TO NumberTotalScoreMessages - 1 DO
            IF TotalScoreMessages [Message].Score < Score THEN
              IF TotalScoreMessages [Message].Score >= 0 THEN
                  BEGIN
                  SaveSetAndClearActiveWindow (QuickCommandWindow);
                  Write (TotalScoreMessages [Message].MessageString);
                  Congrats;
                  Wait (2000);
                  TotalScoreMessages [Message].Score := -1;
                  RestorePreviousWindow;
                  END;

    END;




PROCEDURE QuickDisplay (Prompt: Str80);

    BEGIN
    SaveSetAndClearActiveWindow (QuickCommandWindow);
    Write (Prompt);
    RestorePreviousWindow;
    END;


PROCEDURE QuickDisplay2 (Prompt: Str80);

    BEGIN
    SaveSetAndClearActiveWindow (QuickCommand2Window);
    Write (Prompt);
    RestorePreviousWindow;
    END;


PROCEDURE QuickDisplayError (Prompt: Str80);

    BEGIN
    SaveSetAndClearActiveWindow (QuickCommandWindow);
    TextColor (Red);
    TextBackground (Black);
    Write (Prompt);
    Tone.DoABeep (Warning);
    RestorePreviousWindow;
    END;



FUNCTION QuickEditResponse (Prompt: Str80;
                            MaxInputLength: INTEGER): Str80;

{ Version 6.44 - added return of ESCAPE value instead of null string }

{KK1L: 6.73 Returns an 'CTRL' at the end if CTRL-Enter was used and two radios are active.}

VAR InputString: Str80;
    Key: Char;

    BEGIN
    Tone.DoABeep (PromptBeep);

    SaveSetAndClearActiveWindow (QuickCommandWindow);

    Write (Prompt);

    InputString := '';

    REPEAT
        REPEAT millisleep UNTIL NewKeyPressed;

        Key := NewReadKey;

        CASE Key OF
            EscapeKey:
                IF InputString = '' THEN
                    BEGIN
                    QuickEditResponse := EscapeKey;
                    RemoveAndRestorePreviousWindow;
                    Exit;
                    END
                ELSE
                    BEGIN
                    GoToXY (WhereX - Length (InputString), WhereY);
                    ClrEol;
                    InputString := '';
                    END;

            BackSpace:
                BEGIN
                IF Length (InputString) > 0 THEN
                    BEGIN
                    InputString [0] := Chr (Length (InputString) - 1);
                    GoToXY (WhereX - 1, WhereY);
                    ClrEol;
                    END;
                END;

            CarriageReturn:
                BEGIN
                QuickEditResponse := InputString;
                IF ctrlenter THEN  { ControlEnter, not Control-J }
                    IF SingleRadioMode THEN
                        QuickEditResponse := InputString
                    ELSE
                        QuickEditResponse := InputString + 'CTRL';
                RemoveAndRestorePreviousWindow;
                Exit;
                END;

            ControlJ: {KK1L: 6.73}
                BEGIN
                QuickEditResponse := InputString;
                IF ctrlenter THEN  { ControlEnter, not Control-J }
                    IF SingleRadioMode THEN
                        QuickEditResponse := InputString
                    ELSE
                        QuickEditResponse := InputString + 'CTRL';
                RemoveAndRestorePreviousWindow;
                Exit;
                END;

            NullKey:
                BEGIN
                NewReadKey;
                END;

            ELSE
                BEGIN
                Write (Key);
                InputString := InputString + Key;
                END;

            IF Length (InputString) = MaxInputLength THEN
                BEGIN
                QuickEditResponse := InputString;
                RemoveAndRestorePreviousWindow;
                Exit;
                END;

            END;  { of case }

    UNTIL FALSE;
    END;




FUNCTION QuickEditInteger (Prompt: Str80; MaxInputLength: INTEGER): LONGINT;

{ This function will put up the prompt specified and get the numeric input
  that is put in.  Only integers will be processed and if the number of
  integers specified is entered, a return will not be waited for.  The
  characters are converted to an integer.  If an escape is hit, the
  response will be -1.                                                }

VAR InputString: Str80;
    InputValue: LONGINT;
    xResult: INTEGER;

    BEGIN
    QuickEditInteger := -1;

    InputString := QuickEditResponse (Prompt, MaxInputLength);
    IF InputString = EscapeKey THEN Exit;

    IF StringIsAllNumbers (InputString) AND (InputString <> '') THEN
        BEGIN
        Val (InputString, InputValue, xResult);
        IF xResult = 0 THEN QuickEditInteger := InputValue;
        END;
    END;




FUNCTION QuickEditFreq (Prompt: Str80; MaxInputLength: INTEGER): LONGINT;

{ This function will put up the prompt specified and get the numeric input
  that is put in.  Only integers will be processed and if the number of
  integers specified is entered, a return will not be waited for.  The
  characters are converted to an integer.  If an escape is hit, the
  response will be -1.                                                }

{KK1L: 6.73 Result will be negative if CTRL-Enter was used to enter the frequency}
{           and two radios are active. This is used to set the inactive radio.}

VAR InputString: Str80;
    KHzString, HertzString: Str20;
    InputValue: LONGINT;
    xResult: INTEGER;
    CTRLStr: Str80; {KK1L: 6.73}

    BEGIN
    QuickEditFreq := -1;

    {KK1L: 6.73 Changed to return a 'CTRL' at the end if CTRL-Enter was used and two radios are active.}

    InputString := QuickEditResponse (Prompt, MaxInputLength);
    IF (InputString = EscapeKey) OR (InputString = '') THEN Exit;

    CTRLStr := InputString;                                                {KK1L: 6.73}
    Delete (CTRLStr, 1, (Length (CTRLStr) - 4));

    IF CTRLStr = 'CTRL' THEN  Delete (InputString, (Length (InputString) - 3), 4); {KK1L: 6.73}

    IF StringHas (InputString, '.') THEN
        BEGIN
        KHzString := PrecedingString (InputString, '.');
        HertzString := PostcedingString (InputString, '.');

        WHILE Length (HertzString) < 3 DO
            HertzString := HertzString + '0';

        Val (KHzString + HertzString, InputValue, xResult);
        END
    ELSE
        BEGIN
        InputString := InputString + '000';
        Val (InputString, InputValue, xResult);
        END;

    IF (CTRLStr = 'CTRL') AND (xResult = 0) THEN InputValue := InputValue * (-1) {KK1L: 6.73}
    ELSE IF xResult <> 0 THEN Exit;

    { This used to be all over LOGSUBS2, I moved it here }

    IF (InputValue > 1000) AND (InputValue < 1000000) THEN
        CASE ActiveBand OF
            Band160: InputValue := InputValue +  1000000;
            Band80:  InputValue := InputValue +  3000000;
            Band40:  InputValue := InputValue +  7000000;
            Band20:  InputValue := InputValue + 14000000;
            Band15:  InputValue := InputValue + 21000000;
            Band10:  InputValue := InputValue + 28000000;
            END;

    QuickEditFreq := InputValue;
    END;




FUNCTION QuickEditReal (Prompt: Str80; MaxInputLength: INTEGER): REAL;

{ This function will put up the prompt specified and get the numeric input
  that is put in.  Only integers will be processed and if the number of
  integers specified is entered, a return will not be waited for.  The
  characters are converted to an integer.  If an escape is hit, the
  response will be -1.                                                }

VAR InputString: Str80;
    xResult: INTEGER;
    InputValue: REAL;

    BEGIN
    QuickEditReal := -1;

    InputString := QuickEditResponse (Prompt, MaxInputLength);
    IF InputString = EscapeKey THEN Exit;

    IF InputString <> '' THEN
        BEGIN
        Val (InputString, InputValue, xResult);

        IF xResult = 0 THEN
            QuickEditReal := InputValue;
        END;
    END;




PROCEDURE ResetSavedWindowListAndPutUpCallWindow;

    BEGIN
    ExchangeWindowCursorPosition := 0;
    NumberSavedWindows := 0;
    SetActiveWindow (CallWindow);
    ClrScr;
    END;




PROCEDURE PutUpExchangeWindow;

    BEGIN
    SaveSetAndClearActiveWindow (ExchangeWindow);
    RestorePreviousWindow;
    END;


PROCEDURE ActivateExchangeWindow;

{ This procedure will make the exchange window active with the cursor
  at position it was at before.                                    }

    BEGIN
    SaveAndSetActiveWindow (ExchangeWindow);
    GoToXY (ExchangeWindowCursorPosition, WhereY)
    END;




PROCEDURE DisplayRemainingMults (RemainingMults: RemainingMultListPointer;
                                 RemMultDisplay: RemainingMultiplierType);


VAR TempString: Str80;
    NumberRemaining, RemainingMult: INTEGER;
    CleanSweep: BOOLEAN;

    BEGIN
    SaveSetAndClearActiveWindow (RemainingMultsWindow);

    CleanSweep := RemainingMultDisplayMode = Erase;

    CASE RemMultDisplay OF
        Domestic:
            NumberRemaining := DomQTHTable.NumberRemainingMults;

        DX:
            NumberRemaining := CountryTable.NumberRemainingCountries;

        Zone:
            NumberRemaining := MaxNumberOfZones;

        END;

    FOR RemainingMult := 0 TO NumberRemaining - 1 DO
        IF RemainingMults^ [RemainingMult] THEN
            BEGIN
            CleanSweep := False;
            Break;
            END;

    IF CleanSweep THEN
        BEGIN
        WriteLn ('CLEAN SWEEP!!');
        WriteLn;
        WriteLn ('CONGRATULATIONS!!');
        RestorePreviousWindow;
        Exit;
        END;

    FOR RemainingMult := 0 TO NumberRemaining - 1 DO
        BEGIN
        CASE RemainingMultDisplayMode OF
            Erase:
                IF RemainingMults^ [RemainingMult] THEN
                    BEGIN
                    CASE RemMultDisplay OF
                        Domestic:
                            BEGIN
                            TempString := DomQTHTable.GetDomMultName (RemainingMult);

                            WHILE Pos (' ', TempString) > 0 DO
                                TempString [Pos (' ', TempString)] := '/';
                            END;


                        DX: BEGIN
                            TempString := CountryTable.GetDXMultName (RemainingMult);

                            WHILE Pos (' ', TempString) > 0 DO
                                TempString [Pos (' ', TempString)] := '/';
                            END;


                        Zone: IF ActiveZoneMult <> EUHFCYear THEN
                                  Str (RemainingMult + 1:2, TempString)
                              ELSE
                                  Str (RemainingMult:2, TempString);

                        END;

                    IF (WhereX > 1) AND (WhereX < Lo (WindMax)) THEN
                        Write (' ');

                    IF WhereX + Length (TempString) > Lo (WindMax) THEN
                        WriteLn;

                    Write (TempString);
                    END;

            HiLight:
                BEGIN
                CASE RemMultDisplay OF
                    Domestic:
                        BEGIN
                        TempString := DomQTHTable.GetDomMultName (RemainingMult);

                        WHILE Pos (' ', TempString) > 0 DO
                            TempString [Pos (' ', TempString)] := '/';
                        END;


                    DX: BEGIN
                        TempString := CountryTable.GetDXMultName (RemainingMult);

                        WHILE Pos (' ', TempString) > 0 DO
                            TempString [Pos (' ', TempString)] := '/';
                        END;


                    Zone: IF ActiveZoneMult <> EUHFCYear THEN
                              Str (RemainingMult + 1:2, TempString)
                          ELSE
                              Str (RemainingMult:2, TempString)

                    END;


                TextColor (SelectedColors.RemainingMultsWindowColor);

                IF (WhereX > 1) AND (WhereX < Lo (WindMax)) THEN
                    Write (' ');

                IF WhereX + Length (TempString) > Lo (WindMax) THEN
                    WriteLn;

                IF NOT RemainingMults^ [RemainingMult] THEN
                    TextColor (SelectedColors.RemainingMultsWindowSubdue);

                Write (TempString);
                END;

            END;
        END;

    RestorePreviousWindow;
    END;




PROCEDURE DecrementBandMapTimes;

{ Decrements all times by one.  Any that were at zero get deleted. }

VAR BandMapEntryRecord, PreviousBandMapEntryRecord: BandMapEntryPointer;
    MinutesLeft: BYTE;
    Band: BandType;
    Mode: ModeType;

    BEGIN
    FOR Band := Band160 TO Band2 DO
        FOR Mode := CW TO Phone DO
            IF BandMapFirstEntryList [Band, Mode] <> nil THEN
                BEGIN
                PreviousBandMapEntryRecord := nil;
                BandMapEntryRecord     := BandMapFirstEntryList [Band, Mode];

                WHIlE BandMapEntryRecord <> nil DO
                    BEGIN
                    MinutesLeft := BandMapEntryRecord^.StatusByte AND $3F;

                    IF MinutesLeft = 0 THEN   { Time to die }
                        BEGIN
                        IF PreviousBandMapEntryRecord = nil THEN { This is the first one in list }
                            BEGIN
                            BandMapFirstEntryList [Band, Mode] := BandMapEntryRecord^.NextEntry;
                            Dispose (BandMapEntryRecord);
                            BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];
                            Continue;
                            END;

                        { Wasn't the first record - change NextExtry of previous
                          record to point to the NextEntry }

                        PreviousBandMapEntryRecord^.NextEntry := BandMapEntryRecord^.NextEntry;

                        { Free up the memory }

                        Dispose (BandMapEntryRecord);

                        { We want to process the next band map entry next }

                        BandMapEntryRecord := PreviousBandMapEntryRecord^.NextEntry;
                        Continue;
                        END;

                    { We aren't going to delete this entry - decrement the minute count }

                    Dec (MinutesLeft);

                    BandMapEntryRecord^.StatusByte := BandMapEntryRecord^.StatusByte AND $C0;
                    BandMapEntryRecord^.StatusByte := BandMapEntryRecord^.StatusByte  OR MinutesLeft;

                    { Move to the next entry.  Remember the address of this entry in
                      case I need to link its NextEntry to a different place }

                    PreviousBandMapEntryRecord := BandMapEntryRecord;
                    BandMapEntryRecord         := BandMapEntryRecord^.NextEntry;
                    END;
                END;
    END;


PROCEDURE ResetBandMapTimes;

{KK1L: 6.70 Sets all times to BandmapDecayTime. Called when BandMapDecayTime is changed.}

VAR BandMapEntryRecord: BandMapEntryPointer;
    Band: BandType;
    Mode: ModeType;

    BEGIN
    FOR Band := Band160 TO Band2 DO
        FOR Mode := CW TO Phone DO
            IF BandMapFirstEntryList [Band, Mode] <> nil THEN
                BEGIN
                BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

                WHIlE BandMapEntryRecord <> nil DO
                    BEGIN
                    BandMapEntryRecord^.StatusByte := BandMapEntryRecord^.StatusByte AND $C0;
                    BandMapEntryRecord^.StatusByte := BandMapEntryRecord^.StatusByte  OR BandMapDecayTime;
                    BandMapEntryRecord := BandMapEntryRecord^.NextEntry;
                    END;
                END;
    END;



PROCEDURE UpdateTimeAndRateDisplays (ShowTime: BOOLEAN; DoRadios: BOOLEAN);

LABEL IgnoreRadioOneFreq, IgnoreRadioTwoFreq;

VAR DateString, TimeString, FullTimeString, HourString, DayString: Str20;
    Hour, Minute, Second, Hundredths: WORD;
    AlarmInteger, RecordNumber, IntegerTime, RateMinute: INTEGER;
    Freq: LONGINT;
    Band: BandType;
    Mode: ModeType;

    BEGIN
    IF ShowTime THEN
        BEGIN
        FullTimeString := GetFullTimeString;

  MILLISLEEP; //KS try again
        GetTime (Hour, Minute, Second, Hundredths); {KK1L: 6.71a}

        { Check to see if we should do radio polling stuff or not }

        IF (FullTimeString = LastFullTimeString) AND (Hundredths < LastSecond100 + (FreqPollRate div 10)) THEN
            Exit;

        LastSecond100 := Hundredths; {KK1L: 6.71a}

        { Check to see if we have an interfaced radio to look at }

        IF NOT PollRadioOne THEN
            DisplayFrequency (0, RadioOne);

        IF DoRadios AND (Radio1Type <> NoInterfacedRadio) AND PollRadioOne THEN
            BEGIN
            Inc (Radio1InquireCount);

            IF (Radio1CommandBufferStart = Radio1CommandBufferEnd) AND     {KK1L: 6.71a removed}{KK1L: 6.72}
               ((Radio1InquireCount > 1) OR (Radio1InquireCount < 0) OR
               (ActiveRadio = RadioOne)) THEN
                BEGIN
                IF GetRadioParameters (RadioOne, '', Freq, Band, Mode, TRUE, False) THEN
                    BEGIN
                    {KK1L: 6.71 Need to have other radio info ready for ExchangeRadios}
                    {KK1L: 6.72 Don't need now that I understand what I removed above}
                    {IF (Radio2Type <> NoInterfacedRadio) AND (Radio2CommandBufferStart = Radio2CommandBufferEnd) THEN}
                    {    IF GetRadioParameters (RadioTwo, '', DummyFreq, DummyBand, DummyMode, TRUE) THEN             }
                    {        BEGIN                                                                                    }
                    {        BandMemory [RadioTwo] := DummyBand;                                                      }
                    {        ModeMemory [RadioTwo] := DummyMode;                                                      }
                    {        IF FrequencyMemoryEnable THEN                                                            }
                    {            FreqMemory [DummyBand, DummyMode] := DummyFreq;                                      }
                    {        END;                                                                                     }

                    { Now in 1999, I know what this was trying to do...
                      it was trying to avoid things getting screwed up
                      while transmitting on the split frequency. }

                    { This is taken out - not sure what the split stuff
                      was trying to do 19-Aug-97  }

                    { Put back in for testing in 6.48 }

                    IF (Abs (Freq - SplitFreq) <= 1000) THEN
                           GoTo IgnoreRadioOneFreq;

                    {KK1L: 6.72 Use FreqPollRate instead of 1000 to keep constant response.}
                    {           FreqPollRate in ms relates to 1KHz per second.}
                    {KK1L: 6.72 Scale FreqPollRate to allow variable tuning rate sensitivity.}
                    RadioOnTheMove[RadioOne] := ((PreviousRadioOneFreq <> 0) AND
                                         (Abs (PreviousRadioOneFreq - Freq) > (FreqPollRate * AutoSAPEnableRate div 1000))) OR
                                         (ModeMemory [RadioOne] <> Mode);

                    {KK1L: 6.73 Used for AutoSAPEnable. Fixes entry into SAP when just changing bands or modes}
                    RadioMovingInBandMode[RadioOne] := (RadioOnTheMove[RadioOne]) AND
                                                       (ModeMemory [RadioOne] = Mode) AND
                                                       (BandMemory [RadioOne] = Band);

                    BandMemory [RadioOne] := Band;
                    ModeMemory [RadioOne] := Mode;

                    IF FrequencyMemoryEnable THEN
                        FreqMemory [Band, Mode] := Freq;

                    {KK1L: 6.73 Created to allow freq compare of second radio to band map freq. Could use it elswhere too.}
                    CurrentFreq[RadioOne] := Freq;

                    DisplayFrequency (Freq, RadioOne); {KK1L: 6.73 Added Radio}

                    IF (ActiveRadio = RadioOne) THEN
                        BEGIN

                        IF ((Band <> LastDisplayedBand) OR (Mode <> LastDisplayedMode) OR
                            (Band <> LastRadioOneBand)  OR (Mode <> LastRadioOneMode)) THEN
                                IF Radio1TrackingEnable THEN
                                    BEGIN
                                    ActiveBand := Band;
                                    ActiveMode := Mode;
                                    DisplayBandMode (ActiveBand, ActiveMode, False);

                                    VisibleDupeSheetChanged := True;

                                    IF Mode <> LastRadioOneMode THEN
                                        BEGIN
                                        DisplayCodeSpeed (CodeSpeed,
                                                          CWEnabled,
                                                          DVPOn,
                                                          ActiveMode);

                                        DisplayAutoSendCharacterCount;
                                        END;

                                    LastRadioOneBand := Band;
                                    LastRadioOneMode := Mode;
                                    END;

                        { See what radio two might be up to }

                        IF TwoRadioState = TwoRadiosDisabled THEN  { No 2nd radio }
                            BEGIN
                            IF ((Freq <> BandMapCursorFrequency) OR (BandMapMode <> ActiveMode)) AND (Freq <> 0) THEN
                                BEGIN
                                BandMapCursorFrequency := Freq;
                                BandMapBand := Band;
                                BandMapMode := Mode;
                                DisplayBandMap;
                                END;
                            END
                        ELSE
                            BEGIN
                            IF BandMapEnable THEN
                                IF ((Band = BandMapBand) AND (Freq <> BandMapCursorFrequency) AND (Freq <> 0)) OR
                                    (RadioOnTheMove[RadioOne]) THEN {KK1L: 6.73 Consolidated logic}
                                    BEGIN
                                    BandMapCursorFrequency := Freq;
                                    BandMapBand := Band;
                                    BandMapMode := Mode;
                                    DisplayBandMap;
                                    END;
                            END;
                        END
                    ELSE
                        BEGIN

                        IF (TwoRadioState <> TwoRadiosDisabled) THEN  {KK1L: 6.73 Added to display inactive radio freq}
                            DisplayFrequency (Freq, RadioOne); {KK1L: 6.73 Display Radio One}

                    { Okay, this isn't the active radio, but we might have
                      something to do if the band map is active.  If we are
                      tuning to new frequencies on this radio, then we are
                      going to assume that it is appropriate for the band
                      map on this band to be displayed! }

                        IF (TwoRadioState <> TwoRadiosDisabled) AND BandMapEnable AND RadioOnTheMove[RadioOne] THEN
                            BEGIN
                            BandMapBand := Band;
                            BandMapMode := Mode;
                            BandMapCursorFrequency := Freq;
                            DisplayBandMap;
                            END;

                    { We should also update the band output if it has
                      changed bands. }

                            OutputBandInfo (RadioOne, Band);
                        END;

                    {KK1L: 6.71 Same as what was LastRadio1Freq set in LOGK1EA. I made these variables}
                    {           the same. It is set by the GetRadioParameter call.}
                    {KK1L: 6.71b Put this back to fix AutoSAPEnable}
                    PreviousRadioOneFreq := Freq;
                    END
                ELSE
                    DisplayFrequency (0, RadioOne); {KK1L: 6.73 Added Radio}

        IgnoreRadioOneFreq:

                Radio1InquireCount := 1;
                END;
            {QuickDisplay('Done with radio one'); }{KK1L: 6.72 DEBUG}
            END;

        IF NOT PollRadioTwo THEN
            DisplayFrequency (0, RadioTwo);

        {IF DoRadios AND (Radio2Type <> NoInterfacedRadio) AND (Radio2PollCount = 0) THEN} {KK1L: 6.71a removed}
        IF DoRadios AND (Radio2Type <> NoInterfacedRadio) AND PollRadioTwo THEN  {KK1L: 6.72 Added PollRadioTwo}
            BEGIN
            Inc (Radio2InquireCount);
            {QuickDisplay2('Working with radio two'); }{KK1L: 6.72 DEBUG}

            {Radio2PollCount := Radio2UpdateSeconds;} {KK1L: 6.71a removed}

            IF (Radio2CommandBufferStart = Radio2CommandBufferEnd) AND    {KK1L: 6.71a removed}{KK1L: 6.72 put back}
               ((Radio2InquireCount > 1) OR (Radio2InquireCount < 0) OR
               (ActiveRadio = RadioTwo)) THEN
            {IF (Radio2CommandBufferStart = Radio2CommandBufferEnd) AND   } {KK1L: 6.72 removed...killed update 2nd radio}
            {   (ActiveRadio = RadioTwo) THEN                             }
                BEGIN
                IF GetRadioParameters (RadioTwo, '', Freq, Band, Mode, TRUE, False) THEN
                {KK1L: 6.71a Added Polling parm to proc}
                    BEGIN
                    {KK1L: 6.71 Need to have other radio info ready for ExchangeRadios}
                    {KK1L: 6.72 Don't need now that I understand what I removed above}
                    {IF (Radio1Type <> NoInterfacedRadio) AND (Radio1CommandBufferStart = Radio1CommandBufferEnd) THEN}
                    {    IF GetRadioParameters (RadioOne, '', DummyFreq, DummyBand, DummyMode, TRUE) THEN             }
                    {        BEGIN                                                                                    }
                    {        BandMemory [RadioOne] := DummyBand;                                                      }
                    {        ModeMemory [RadioOne] := DummyMode;                                                      }
                    {        IF FrequencyMemoryEnable THEN                                                            }
                    {            FreqMemory [DummyBand, DummyMode] := DummyFreq;                                      }
                    {        END;                                                                                     }

                    IF (Abs (Freq - SplitFreq) <= 1000) THEN
                           GoTo IgnoreRadioTwoFreq;

                    {KK1L: 6.72 Use FreqPollRate instead of 1000 to keep constant response}
                    {           FreqPollRate in ms relates to 1KHz per second.}
                    {KK1L: 6.72 Scale FreqPollRate to allow variable tuning rate sensitivity.}
                    RadioOnTheMove[RadioTwo] := ((PreviousRadioTwoFreq <> 0) AND
                                         (Abs (PreviousRadioTwoFreq - Freq) > (FreqPollRate * AutoSAPEnableRate div 1000))) OR
                                         (ModeMemory [RadioTwo] <> Mode);

                    {KK1L: 6.73 Used for AutoSAPEnable. Fixes entry into SAP when just changing bands or modes}
                    RadioMovingInBandMode[RadioTwo] := (RadioOnTheMove[RadioTwo]) AND
                                                       (ModeMemory [RadioTwo] = Mode) AND
                                                       (BandMemory [RadioTwo] = Band);

                    BandMemory [RadioTwo] := Band;
                    ModeMemory [RadioTwo] := Mode;

                    IF FrequencyMemoryEnable THEN
                        FreqMemory [Band, Mode] := Freq;

                    {KK1L: 6.73 Created to allow freq compare of second radio to band map freq. Could use it elswhere too.}
                    CurrentFreq[RadioTwo] := Freq;

                    DisplayFrequency (Freq, RadioTwo);

                    IF (ActiveRadio = RadioTwo) THEN
                        BEGIN
                        IF (Band <> LastDisplayedBand) OR (Mode <> LastDisplayedMode) OR
                           (Band <> LastRadioTwoBand)  OR (Mode <> LastRadioTwoMode) THEN
                               IF Radio2TrackingEnable THEN
                                   BEGIN
                                   ActiveBand := Band;
                                   ActiveMode := Mode;

                                   VisibleDupeSheetChanged := True;

                                   DisplayBandMode (ActiveBand, ActiveMode, False);

                                   IF Mode <> LastRadioOneMode THEN
                                       DisplayCodeSpeed (CodeSpeed,
                                                         CWEnabled,
                                                         DVPOn,
                                                         ActiveMode);

                                   LastRadioTwoBand := Band;
                                   LastRadioTwoMode := Mode;
                                   END;

                        { See what the second radio might be up to }

                        IF TwoRadioState = TwoRadiosDisabled THEN
                            BEGIN

                            IF (Freq <> BandMapCursorFrequency) AND (Freq <> 0) THEN
                                BEGIN
                                BandMapBand := Band;
                                BandMapMode := Mode;
                                BandMapCursorFrequency := Freq;
                                DisplayBandMap;
                                END;
                            END
                        ELSE
                            BEGIN
                            IF BandMapEnable THEN
                                IF ((Band = BandMapBand) AND (Freq <> BandMapCursorFrequency) AND (Freq <> 0)) OR
                                    (RadioOnTheMove[RadioTwo]) THEN {KK1L: 6.73 consolodated logic}
                                    BEGIN
                                    BandMapCursorFrequency := Freq;
                                    BandMapBand := Band;
                                    BandMapMode := Mode;
                                    DisplayBandMap;
                                    END;
                            END;
                        END
                    ELSE
                        BEGIN

                        IF (TwoRadioState <> TwoRadiosDisabled) THEN  {KK1L: 6.73 Added to display inactive radio freq}
                            DisplayFrequency (Freq, RadioTwo); {KK1L: 6.73 Display Radio Two}

                    { Okay, this isn't the active radio, but we might have
                      something to do if the band map is active.  If we are
                      tuning to new frequencies on this radio, then we are
                      going to assume that it is appropriate for the band
                      map on this band to be displayed! }

                        IF (TwoRadioState <> TwoRadiosDisabled) AND BandMapEnable AND RadioOnTheMove[RadioTwo] THEN
                            BEGIN
                            BandMapBand := Band;
                            BandMapMode := Mode;
                            BandMapCursorFrequency := Freq;
                            DisplayBandMap;
                            END;

                    { We should also update the band output if it has
                      changed bands. }

                            OutputBandInfo (RadioTwo, Band);
                        END;

                    {KK1L: 6.71 Same as what was LastRadio2Freq set in LOGK1EA. I made these variables}
                    {           the same. It is set by the GetRadioParameter call.}
                    {KK1L: 6.71b Put this back to fix AutoSAPEnable}
                    PreviousRadioTwoFreq := Freq;
                    END
                ELSE
                    DisplayFrequency (0, RadioTwo); {KK1L: 6.73 Added Radio}

        IgnoreRadioTwoFreq:

                Radio2InquireCount := 1;
                END;
            {QuickDisplay('Done with radio two');} {KK1L: 6.72 DEBUG}

        {KK1L: 6.71a This bit (til the next end) moved here so it only occurs once per second rather than}    END;
        {           the once per 50ms or so that the radio stuff occurs}

        IF FullTimeString = LastFullTimeString THEN Exit; {KK1L: 6.71a This limits further execution to once per second}

        LastFullTimeString := FullTimeString;

        IF ReminderPostedCount > 0 THEN Dec (ReminderPostedCount);

        SaveAndSetActiveWindow (TotalWindow);
        Write (' ' + FullTimeString);

        IF TenMinuteRule <> NoTenMinuteRule THEN
            BEGIN
            GoToXY (1, 2);
            Write ('  ', ElaspedTimeString (TenMinuteTime.Time));
            END;

        RestorePreviousWindow;
        END;

    TimeString := GetTimeString;
    HourString := PrecedingString (TimeString, ':');

    IF TimeString <> LastDisplayedTime THEN  { See if new minute }
        BEGIN
        LastDisplayedTime := TimeString;
        Inc(MinutesSinceLastBMUpdate); {KK1L: 6.65}

        IF (ActiveBand >= Band160) AND (ActiveBand <= Band10) THEN
            Inc (TimeSpentByBand [ActiveBand]);

        AutoTimeQSOCount := 0;

        {KK1L: 6.65 Added this check to allow for > 63 minute BM decay}
        IF MinutesSinceLastBMUpdate >= BandMapDecayMultiplier THEN
            BEGIN
            MinutesSinceLastBMUpdate := 0;
            DecrementBandMapTimes;
            IF BandMapEnable THEN DisplayBandMap;
            END;

        SaveSetAndClearActiveWindow (ClockWindow);
        Write (TimeString);
        RestorePreviousWindow;

        DateString := GetDateString;

        IF DateString <> LastDisplayedDate THEN
            BEGIN
            SaveSetAndClearActiveWindow (DateWindow);
            Write (DateString);
            RestorePreviousWindow;
            LastDisplayedDate := DateString;
            END;

        IF NumberReminderRecords > 0 THEN
            BEGIN
            IntegerTime := GetIntegerTime;
            DayString   := UpperCase (GetDayString);
            DateString  := UpperCase (DateString);

//            FOR RecordNumber := 0 TO NumberReminderRecords - 1 DO
            RecordNumber := -1;
            while ((RecordNumber + 1) <= NumberReminderRecords - 1) do
            begin
                inc(RecordNumber);
                IF Reminders^ [RecordNumber].Time = IntegerTime THEN
                    IF (Reminders^ [RecordNumber].DateString = DateString) OR
                       (Reminders^ [RecordNumber].DayString  = DayString) OR
                       (Reminders^ [RecordNumber].DayString = 'ALL') THEN
                           BEGIN
                           SaveSetAndClearActiveWindow (QuickCommandWindow);
                           SetColor (SelectedColors.AlarmWindowColor);
                           SetBackground (SelectedColors.AlarmWindowBackground);
                           ClrScr;
                           Write (Reminders^ [RecordNumber].Message);
                           IF Reminders^ [RecordNumber].Alarm THEN
                               WakeUp
                           ELSE
                               Congrats;
                           RestorePreviousWindow;
                           RecordNumber := NumberReminderRecords - 1;
                           ReminderPostedCount := 60;
                           END;
            end;
            END;

        IF AlarmSet THEN
            BEGIN
            AlarmInteger := AlarmHour * 100 + AlarmMinute;

            IF AlarmInteger = GetIntegerTime THEN
                BEGIN
                WakeUp;
                AlarmMinute := AlarmMinute + 4;

                IF AlarmMinute > 59 THEN
                    BEGIN
                    AlarmMinute := AlarmMinute - 60;
                    AlarmHour := AlarmHour + 1;
                    IF AlarmHour > 23 THEN
                        AlarmHour := AlarmHour - 24;
                    END;
                END;

            IF (Hour = AlarmHour) AND (Minute = AlarmMinute) THEN
                BEGIN
                WakeUp;
                AlarmMinute := AlarmMinute + 4;

                IF AlarmMinute > 59 THEN
                    BEGIN
                    AlarmMinute := AlarmMinute - 60;
                    AlarmHour := AlarmHour + 1;
                    IF AlarmHour > 23 THEN
                        AlarmHour := AlarmHour - 24;
                    END;
                END;
            END;

        IF (NumberContactsThisMinute = 0) AND (WakeUpTimeOut > 0) THEN
            BEGIN
            Inc (WakeUpCount);
            IF (WakeUpCount >= WakeUpTimeOut) AND NOT AlarmSet THEN WakeUp;
            END
        ELSE
            WakeUpCount := 0;

        { Fix up the rate array. First, shuffle the minutes }

        FOR RateMinute := 60 DOWNTO 2 DO
            BEGIN
            RateMinuteArray [RateMinute].QSOs   := RateMinuteArray [RateMinute - 1].QSOs;
            RateMinuteArray [RateMinute].Points := RateMinuteArray [RateMinute - 1].Points;
            END;

        { Put new values on top }

        RateMinuteArray [1].QSOs   := NumberContactsThisMinute;
        RateMinuteArray [1].Points := NumberQSOPointsThisMinute;

        { Compute rate = # QSOs or points in last 10 minutes }

        Rate := 0;

        FOR RateMinute := 1 TO 10 DO
            CASE RateDisplay OF
                QSOs:   Rate := Rate + RateMinuteArray [RateMinute].QSOs;
                Points: Rate := Rate + RateMinuteArray [RateMinute].Points;
                END;

        { Compute last sixty minute totals }

        TotalLastSixty := 0;

        FOR RateMinute := 1 TO 60 DO
            CASE RateDisplay OF
                QSOs:   TotalLastSixty := TotalLastSixty + RateMinuteArray [RateMinute].QSOs;
                Points: TotalLastSixty := TotalLastSixty + RateMinuteArray [RateMinute].Points;
                END;

        CASE RateDisplay OF
            QSOs:   TotalThisHour := TotalThisHour + NumberContactsThisMinute;
            Points: TotalThisHour := TotalThisHour + NumberQSOPointsThisMinute;
            END;

        { Zero out minute totals }

        NumberContactsThisMinute  := 0;
        NumberQSOPointsThisMinute := 0;

        { Compute rate }

        IF NumberMinutesProgramRunning >= 10 THEN
            Rate := Rate * 6
        ELSE
            BEGIN
            Inc (NumberMinutesProgramRunning);
            IF NumberMinutesProgramRunning > 0 THEN

               { Runtime 215 at next line - added ()'s to fix }

                Rate := Round (Rate * (60 / NumberMinutesProgramRunning))
            ELSE
                Rate := 0;
            END;

        IF HourString <> LastDisplayedHour THEN
            BEGIN
            TotalThisHour := 0;
            BandChangesThisHour := 0;
            LastDisplayedHour := HourString;
            END;

        DisplayRate (Rate);

        IF (ActivePacketPort <> nil ) AND (PacketReturnPerMinute <> 0) THEN
            BEGIN
            Dec (PacketReturnCount);

            IF PacketReturnCount = 0 THEN
                BEGIN
                SendPacketMessage (CarriageReturn);
                QuickDisplay ('Carriage return sent to packet port.  (per PACKET RETURN PER MINUTE)');
                PacketReturnCount := PacketReturnPerMinute;
                END;
            END;
        END;
    END;




PROCEDURE IncrementTime (Count: INTEGER);

VAR Hours, Minutes, Seconds, Hundredths: Word;
    Year, Month, Day, DayOfWeek: Word;

    BEGIN
    AutoTimeQSOCount := 0;

    GetTime (Hours, Minutes, Seconds, Hundredths);
    Minutes := Minutes + Count;

    IF Minutes > 59 THEN
        BEGIN
        Minutes := Minutes - 60;
        Inc (Hours);

        IF Hours > 23 THEN
            BEGIN
            Hours := Hours - 24;
            GetDate (Year, Month, Day, DayOfWeek);
            Inc (Day);
            SetDate (Year, Month, Day);
            END;
        END;

    SetTime (Hours, Minutes, 0, 0);
    LastDisplayedTime := '';
    LastDisplayedDate := '';
    UpdateTimeAndRateDisplays (True, False);
    END;




PROCEDURE CheckBand (Band: BandType);

    BEGIN
    IF Band <> LastBand THEN
        IF Band <> NoBand THEN
            BEGIN
            Inc (BandChangesThisHour);
            SaveAndSetActiveWindow (HourRateWindow);
            Write ('Bnd Chg = ', BandChangesThisHour);
            RestorePreviousWindow;
            END;

    LastBand := Band;
    END;



PROCEDURE ClearAutoSendDisplay;

    BEGIN
    SaveAndSetActiveWindow (WholeScreenWindow);
    GoToXY (CallWindowLX, CallWindowLY - 1);
    Write ('          ');
    RestorePreviousWindow;
    END;




PROCEDURE DisplayAutoSendCharacterCount;

    BEGIN
    SaveAndSetActiveWindow (WholeScreenWindow);

    IF CallWindowPosition = NormalCallWindowPosition THEN
        BEGIN
        GoToXY (CallWindowLX, CallWindowLY - 1);
        Write ('          ');

        IF (AutoSendCharacterCount > 0) AND AutoSendEnable AND
           (ActiveMode = CW) AND NOT SearchAndPounceMode THEN
            BEGIN
            GoToXY (CallWindowLX + AutoSendCharacterCount - 1, CallWindowLY - 1);
            Write ('|');
            END;

        END
    ELSE
        BEGIN
        GoToXY (CallWindowLX, CallWindowUpLY + 1);
        Write ('          ');

        IF (AutoSendCharacterCount > 0) AND AutoSendEnable AND
           (ActiveMode = CW) AND NOT SearchAndPounceMode THEN
            BEGIN
            GoToXY (CallWindowLX + AutoSendCharacterCount - 1, CallWindowUpLY + 1);
            Write ('');
            END;

        END;

    RestorePreviousWindow;
    END;




PROCEDURE DeleteBandMapEntry (VAR Entry: BandMapEntryPointer);

VAR BandMapEntryRecord, PreviousBandEntryRecord: BandMapEntryPointer;
    StartBand, StopBand, Band: BandType;
    StartMode, StopMode, Mode: ModeType;

    BEGIN
    IF NOT BandMapEnable THEN Exit;

    IF BandMapAllBands THEN
        BEGIN
        IF VHFBandsEnabled THEN {KK1L: 6.64 Keep band map within contest limits}
            BEGIN
            StartBand := Band160;
            StopBand  := Band2;
            END
        ELSE
            BEGIN
            StartBand := Band160;
            StopBand  := Band12; {KK1L: 6.65 fixes WARC display enable}
            END;
        END
    ELSE
        BEGIN
        StartBand := BandMapBand;
        StopBand  := BandMapBand;
        END;

    IF BandMapAllModes THEN
        BEGIN
        StartMode := CW;
        StopMode  := Phone;
        END
    ELSE
        BEGIN
        StartMode := BandMapMode;
        StopMode  := BandMapMode;
        END;

    FOR Band := StartBand TO StopBand DO
        FOR Mode := StartMode TO StopMode DO
            BEGIN

            IF (NOT WARCBandsEnabled) AND
               ((Band = Band30) OR (Band = Band17) OR (Band = Band12)) THEN
                Continue; {KK1L: 6.64 Keep band map within contest limits}

            PreviousBandEntryRecord := nil;
            BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

            WHIlE BandMapEntryRecord <> nil DO
                BEGIN
                IF BigCompressedCallsAreEqual (Entry^.Call, BandMapEntryRecord^.Call) THEN
                    IF Abs (Entry^.Frequency - BandMapEntryRecord^.Frequency) <= BandMapGuardBand THEN
                        BEGIN

                        { We have found the entry to delete }

                        IF PreviousBandEntryRecord = nil THEN   { This is the first entry }
                            BEGIN
                            BandMapFirstEntryList [Band, Mode] := BandMapEntryRecord^.NextEntry;
                            Entry                              := BandMapEntryRecord^.NextEntry;
                            Dispose (BandMapEntryRecord);
                            Exit;
                            END;

                        { Not the first record }

                        PreviousBandEntryRecord^.NextEntry := BandMapEntryRecord^.NextEntry;
                        Entry                              := BandMapEntryRecord^.NextEntry;

                        { Was it the last one?  If so, make the active entry
                          the previous record }

                        IF Entry = nil THEN Entry := PreviousBandEntryRecord;
                        Dispose (BandMapEntryRecord);
                        Exit;
                        END;

                PreviousBandEntryRecord := BandMapEntryRecord;
                BandMapEntryRecord      := BandMapEntryRecord^.NextEntry;
                END;
            END;
    END;




PROCEDURE SetUpBandMapEntry (BandMapData: BandMapEntryPointer; Radio: RadioType); {KK1L: 6.73 Added Radio}

{KK1L: 6.64 Added sense of mode from frequency so radio would be set to mode}
{      when bandmapentry is selected}
{KK1L: 6.64 Added taking radio out of split when QSXOffset=0}
{KK1L: 6.64 Added QSXOffset scaling to make range within INTEGER}

VAR EntryBand: BandType;
    EntryMode: ModeTYpe;
    QSXOffsetInHertz: LONGINT;

    BEGIN
    BandMapCursorData := BandMapData;
    WITH BandMapData^ DO
        IF Frequency > 0 THEN
            BEGIN
            GetBandMapBandModeFromFrequency (Frequency, EntryBand, EntryMode);

            IF QSXOffset <> 0 THEN
                BEGIN
                {KK1L: 6.64 Scaling added to keep offset within INTEGER range.}
                {QSXOffsetInHertz := QSXOffset * 10;}
                {KK1L: 6.64 Not needed. Only LONGINT seems to work as QSXOffset type}
                QSXOffsetInHertz := QSXOffset;
                {KK1L: 6.64 Trying to add resonable functionality to split stuff}
                CASE BandMapSplitMode OF
                    ByCutoffFrequency:
                        BEGIN
                        {KK1L: 6.71 moved ahead of 'A' because for TS850 need to make B active to change mode}
                        SetRadioFreq (Radio, Frequency + QSXOffsetInHertz, EntryMode, 'B');
                        SetRadioFreq (Radio, Frequency, EntryMode, 'A');
                        END;
                    AlwaysPhone:
                        BEGIN
                        {KK1L: 6.71 moved ahead of 'A' because for TS850 need to make B active to change mode}
                        SetRadioFreq (Radio, Frequency + QSXOffsetInHertz, Phone, 'B');
                        SetRadioFreq (Radio, Frequency, Phone, 'A');
                        END;
                    END;
                PutRadioIntoSplit (Radio); {KK1L: 6.71 Moved here from before case for TS850 change}
                END
            ELSE {KK1L: 6.64 Takes radio back out of split if not required for next entry}
                BEGIN
                SetRadioFreq (Radio, Frequency, EntryMode, 'A');
                PutRadioOutOfSplit (Radio);
                END;
            END;
    END;




FUNCTION NextNonDupeEntryInDisplayedBandMap (Band: BandType; Mode: ModeType): BOOLEAN;

{ Data will be in BandMapCursorData record }
{KK1L: 6.64 Added to allow stepping through the displayed bandmap rather than just in}
{      current band and mode.}

VAR BandMapEntryRecord: BandMapEntryPointer;
    CallSign: CallString;
    StartBand, StopBand: BandType;
    StartMode, StopMode: ModeType;
    RadioToUse: RadioType; {KK1L: 6.73}

    BEGIN
    NextNonDupeEntryInDisplayedBandMap := False;

    IF CommandUseInactiveRadio THEN  {KK1L: 6.73}
        RadioToUse := InactiveRadio
    ELSE
        RadioToUse := ActiveRadio;

    IF BandMapAllBands THEN
        BEGIN
        IF VHFBandsEnabled THEN {KK1L: 6.64 Keep band map within contest limits}
            BEGIN
            StartBand := Band160;
            StopBand  := Band2;
            END
        ELSE
            BEGIN
            StartBand := Band160;
            StopBand  := Band12; {KK1L: 6.65 fixes WARC display enable}
            END;
        END
    ELSE
        BEGIN
        StartBand := BandMapBand;
        StopBand  := BandMapBand;
        END;

    IF BandMapAllModes THEN
        BEGIN
        StartMode := CW;
        StopMode  := Phone;
        END
    ELSE
        BEGIN
        StartMode := BandMapMode;
        StopMode  := BandMapMode;
        END;

    FOR Band := StartBand TO StopBand DO
        FOR Mode := StartMode TO StopMode DO
            BEGIN

            IF (NOT WARCBandsEnabled) AND
               ((Band = Band30) OR (Band = Band17) OR (Band = Band12)) THEN
                Continue; {KK1L: 6.64 Keep band map within contest limits}

            BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

            WHIlE BandMapEntryRecord <> nil DO
               BEGIN
               Callsign := BandMapExpandedString (BandMapEntryRecord^.Call);
               {KK1L: 6.73 Use LastDisplayedFreq instead of DisplayedFrequency to support control-A}
               IF BandMapEntryRecord^.Frequency > LastDisplayedFreq[RadioToUse] THEN
                   IF Abs (LastDisplayedFreq[RadioToUse] - BandMapEntryRecord^.Frequency) > BandMapGuardBand THEN
                       IF (Copy (CallSign, 1, 3) <> 'CQ/') THEN  {KK1L: 6.69 added to skip CQ frequency}
                           IF ((BandMapEntryRecord^.StatusByte AND $40) = 0) THEN
                               BEGIN
                               SetUpBandMapEntry (BandMapEntryRecord, RadioToUse); {KK1L: 6.73}
                               NextNonDupeEntryInDisplayedBandMap := True;
                               Exit;
                               END;

               BandMapEntryRecord := BandMapEntryRecord^.NextEntry;
               END;
            END;


    { Got to the end of the list without finding anything.  Start at the
      beginning and find the first non dupe entry }

    FOR Band := StartBand TO StopBand DO
        FOR Mode := StartMode TO StopMode DO
            BEGIN
            BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

            WHIlE BandMapEntryRecord <> nil DO
                BEGIN
                Callsign := BandMapExpandedString (BandMapEntryRecord^.Call);
                IF (Copy (CallSign, 1, 3) <> 'CQ/') THEN  {KK1L: 6.69 added to skip CQ frequency}
                    IF ((BandMapEntryRecord^.StatusByte AND $40) = 0) THEN
                        BEGIN
                        BandMapCursorData := BandMapEntryRecord;
                        NextNonDupeEntryInDisplayedBandMap := True;
                        Exit;
                        END;

                BandMapEntryRecord := BandMapEntryRecord^.NextEntry;
                END;
            END;
    END;


{KK1L: 6.68}
FUNCTION NextMultiplierEntryInDisplayedBandMap (Band: BandType; Mode: ModeType): BOOLEAN;

{ Data will be in BandMapCursorData record }
{KK1L: 6.68 Added to allow stepping through the displayed bandmap rather than just in}
{      current band and mode.}

VAR BandMapEntryRecord: BandMapEntryPointer;
    CallSign: CallString;
    StartBand, StopBand: BandType;
    StartMode, StopMode: ModeType;
    RadioToUse: RadioType; {KK1L: 6.73}

    BEGIN
    NextMultiplierEntryInDisplayedBandMap := False;

    IF CommandUseInactiveRadio THEN  {KK1L: 6.73}
        RadioToUse := InactiveRadio
    ELSE
        RadioToUse := ActiveRadio;

    IF BandMapAllBands THEN
        BEGIN
        IF VHFBandsEnabled THEN {KK1L: 6.64 Keep band map within contest limits}
            BEGIN
            StartBand := Band160;
            StopBand  := Band2;
            END
        ELSE
            BEGIN
            StartBand := Band160;
            StopBand  := Band12; {KK1L: 6.65 fixes WARC display enable}
            END;
        END
    ELSE
        BEGIN
        StartBand := BandMapBand;
        StopBand  := BandMapBand;
        END;

    IF BandMapAllModes THEN
        BEGIN
        StartMode := CW;
        StopMode  := Phone;
        END
    ELSE
        BEGIN
        StartMode := BandMapMode;
        StopMode  := BandMapMode;
        END;

    FOR Band := StartBand TO StopBand DO
        FOR Mode := StartMode TO StopMode DO
            BEGIN

            IF (NOT WARCBandsEnabled) AND
               ((Band = Band30) OR (Band = Band17) OR (Band = Band12)) THEN
                Continue; {KK1L: 6.64 Keep band map within contest limits}

            BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

            WHIlE BandMapEntryRecord <> nil DO
               BEGIN
               Callsign := BandMapExpandedString (BandMapEntryRecord^.Call);
               {KK1L: 6.73 Use LastDisplayedFreq instead of DisplayedFrequency to support control-A}
               IF BandMapEntryRecord^.Frequency > LastDisplayedFreq[RadioToUse] THEN
                   IF Abs (LastDisplayedFreq[RadioToUse] - BandMapEntryRecord^.Frequency) > BandMapGuardBand THEN
                      IF (Copy (CallSign, 1, 3) <> 'CQ/') THEN  {KK1L: 6.69 added to skip CQ frequency}
                           IF ((BandMapEntryRecord^.StatusByte AND $80) <> 0) THEN
                               BEGIN
                               SetUpBandMapEntry (BandMapEntryRecord, RadioToUse); {KK1L: 6.73}
                               NextMultiplierEntryInDisplayedBandMap := True;
                               Exit;
                               END;

               BandMapEntryRecord := BandMapEntryRecord^.NextEntry;
               END;
            END;


    { Got to the end of the list without finding anything.  Start at the
      beginning and find the first non dupe entry }

    FOR Band := StartBand TO StopBand DO
        FOR Mode := StartMode TO StopMode DO
            BEGIN
            BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

            WHIlE BandMapEntryRecord <> nil DO
                BEGIN
                Callsign := BandMapExpandedString (BandMapEntryRecord^.Call);
                IF (Copy (CallSign, 1, 3) <> 'CQ/') THEN  {KK1L: 6.69 added to skip CQ frequency}
                    IF ((BandMapEntryRecord^.StatusByte AND $80) <> 0) THEN
                        BEGIN
                        BandMapCursorData := BandMapEntryRecord;
                        NextMultiplierEntryInDisplayedBandMap := True;
                        Exit;
                        END;

                BandMapEntryRecord := BandMapEntryRecord^.NextEntry;
                END;
            END;
    END;


FUNCTION NextNonDupeEntryInBandMap (Band: BandType; Mode: ModeType): BOOLEAN;

{ Data will be in BandMapCursorData record }
VAR BandMapEntryRecord: BandMapEntryPointer;
    CallSign: CallString;
    RadioToUse: RadioType; {KK1L: 6.73}

    BEGIN
    NextNonDupeEntryInBandMap := False;

    IF CommandUseInactiveRadio THEN  {KK1L: 6.73}
        RadioToUse := InactiveRadio
    ELSE
        RadioToUse := ActiveRadio;

    IF BandMapFirstEntryList [Band, Mode] = nil THEN Exit;

    BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

    { Search through the bandmap to find the next non dupe entry. }

    WHIlE BandMapEntryRecord <> nil DO
        BEGIN
        Callsign := BandMapExpandedString (BandMapEntryRecord^.Call);
        {KK1L: 6.73 Use LastDisplayedFreq instead of DisplayedFrequency to support control-A}
        IF BandMapEntryRecord^.Frequency > LastDisplayedFreq[RadioToUse] THEN
            IF Abs (LastDisplayedFreq[RadioToUse] - BandMapEntryRecord^.Frequency) > BandMapGuardBand THEN
                IF (Copy (CallSign, 1, 3) <> 'CQ/') THEN  {KK1L: 6.69 added to skip CQ frequency}
                    IF ((BandMapEntryRecord^.StatusByte AND $40) = 0) THEN
                        BEGIN
                        SetUpBandMapEntry (BandMapEntryRecord, RadioToUse); {KK1L: 6.73}
                        NextNonDupeEntryInBandMap := True;
                        Exit;
                        END;

        BandMapEntryRecord := BandMapEntryRecord^.NextEntry;
        END;

    { Got to the end of the list without finding anything.  Start at the
      beginning and find the first non dupe entry }

    BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

    WHIlE BandMapEntryRecord <> nil DO
        BEGIN
        Callsign := BandMapExpandedString (BandMapEntryRecord^.Call);
        IF (Copy (CallSign, 1, 3) <> 'CQ/') THEN  {KK1L: 6.69 added to skip CQ frequency}
            IF ((BandMapEntryRecord^.StatusByte AND $40) = 0) THEN
                BEGIN
                BandMapCursorData := BandMapEntryRecord;
                NextNonDupeEntryInBandMap := True;
                Exit;
                END;

        BandMapEntryRecord := BandMapEntryRecord^.NextEntry;
        END;
    END;


{KK1L: 6.68}
FUNCTION NextMultiplierEntryInBandMap (Band: BandType; Mode: ModeType): BOOLEAN;

{ Data will be in BandMapCursorData record }
VAR BandMapEntryRecord: BandMapEntryPointer;
    CallSign: CallString;
    RadioToUse: RadioType; {KK1L: 6.73}

    BEGIN
    NextMultiplierEntryInBandMap := False;

    IF CommandUseInactiveRadio THEN  {KK1L: 6.73}
        RadioToUse := InactiveRadio
    ELSE
        RadioToUse := ActiveRadio;

    IF BandMapFirstEntryList [Band, Mode] = nil THEN Exit;

    BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

    { Search through the bandmap to find the next non dupe entry. }

    WHIlE BandMapEntryRecord <> nil DO
        BEGIN
        Callsign := BandMapExpandedString (BandMapEntryRecord^.Call);
        {KK1L: 6.73 Use LastDisplayedFreq instead of DisplayedFrequency to support control-A}
        IF BandMapEntryRecord^.Frequency > LastDisplayedFreq[RadioToUse] THEN
            IF Abs (LastDisplayedFreq[RadioToUse] - BandMapEntryRecord^.Frequency) > BandMapGuardBand THEN
                IF (Copy (CallSign, 1, 3) <> 'CQ/') THEN  {KK1L: 6.69 added to skip CQ frequency}
                    IF ((BandMapEntryRecord^.StatusByte AND $80) <> 0) THEN
                        BEGIN
                        SetUpBandMapEntry (BandMapEntryRecord, RadioToUse); {KK1L: 6.73}
                        NextMultiplierEntryInBandMap := True;
                        Exit;
                        END;

        BandMapEntryRecord := BandMapEntryRecord^.NextEntry;
        END;

    { Got to the end of the list without finding anything.  Start at the
      beginning and find the first non dupe entry }

    BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

    WHIlE BandMapEntryRecord <> nil DO
        BEGIN
        Callsign := BandMapExpandedString (BandMapEntryRecord^.Call);
        IF (Copy (CallSign, 1, 3) <> 'CQ/') THEN  {KK1L: 6.69 added to skip CQ frequency}
            IF ((BandMapEntryRecord^.StatusByte AND $80) <> 0) THEN
                BEGIN
                BandMapCursorData := BandMapEntryRecord;
                NextMultiplierEntryInBandMap := True;
                Exit;
                END;

        BandMapEntryRecord := BandMapEntryRecord^.NextEntry;
        END;
    END;





PROCEDURE GetBandMapBandModeFromFrequency (Frequency: LONGINT;
                                           VAR Band: BandType;
                                           VAR Mode: ModeType);

{ Mode might be setup by someone else - in which case, don't mess with it }

VAR TempMode: ModeType;

    BEGIN
    CalculateBandMode (Frequency, Band, TempMode);
    IF (Mode <> CW) AND (Mode <> Phone) THEN
        BEGIN
        IF Frequency > BandMapModeCutoffFrequency [Band] THEN
            Mode := Phone
        ELSE
            Mode := CW;
        END;
    END;




PROCEDURE AddBandMapModeCutoffFrequency (Freq: LONGINT);

VAR Band: BandType;
    TempMode: ModeType;

    BEGIN
    CalculateBandMode (Freq, Band, TempMode);

    IF (Band >= Band160) AND (Band <= Band2) THEN
        BandMapModeCutoffFrequency [Band] := Freq;
    END;


PROCEDURE AddBandMapPlaceHolder;

{KK1L: 6.65 Created proc to add a dummy call to the BM. It is invoked by CTRL-INS.}
{           Its purpose is to put a unique ID in the BM with multiplier set when you}
{           tune across a station, catch a partial call, and don't want to wait.}

VAR Call: CallString;
    Hours, Minutes, Seconds, Hundredths: WORD;
    TempString: Str20;
    Frequency: LONGINT;
    TempMode: ModeType;

    BEGIN
    GetTime (Hours, Minutes, Seconds, Hundredths);
    Str(Minutes, TempString);
    Call := 'NEW' + TempString;
    Str(Seconds, TempString);
    Call := Call + TempString;
    {KK1L: 6.68 Added stuff to get freq if no radio connected. Used to simply use BandMapFrequency.}
    IF GetRadioParameters (ActiveRadio, '', Frequency, Band, TempMode, FALSE, False) THEN
        AddBandMapEntry (Call, Frequency, 0, TempMode, False,  False, BandMapDecayTime)
    ELSE
        IF AskForFrequencies THEN
            BEGIN
            Frequency := QuickEditFreq ('Enter frequency for ' + Call + ' in kHz : ', 10);

            IF Frequency <> -1 THEN
                BEGIN
                AddBandMapEntry (Call, Frequency, 0, ActiveMode, False,  False, BandMapDecayTime);
                BandMapCursorFrequency := Frequency; {KK1L: 6.68 band map will track manual entry}
                END;
            END;
    DisplayBandMap;
    END;


PROCEDURE AddBandMapEntry (Call: CallString;
                           Frequency: LONGINT;
                           QSXFrequency: LONGINT;
                           Mode: ModeType;
                           Dupe: BOOLEAN;
                           Mult: BOOLEAN;
                           MinutesLeft: BYTE);

{KK1L: 6.64 Added QSXOffset scaling to make range within INTEGER}

VAR LastBandMapEntryRecord: BandMapEntryPointer;
    TempBandMapEntryRecord, BandMapEntryRecord: BandMapEntryPointer;
    StatusByte: BYTE;
    QSXOffset: LONGINT; {KK1L: 6.64 Tried INTEGER, but no joy.}
    CompressedCall: EightBytes;
    EntryAdded: BOOLEAN;
    Band: BandType;

    BEGIN
    IF NOT BandMapEnable THEN Exit;

    { The band map seemed to be crashing with SHF frequencies }

    IF Frequency > 150000000 THEN Exit;
    IF Frequency < 10 THEN Exit;

    BigCompressFormat (Call, CompressedCall);

    { Fix up status byte }

    StatusByte := MinutesLeft AND $3F;

    IF Dupe THEN StatusByte := StatusByte OR $40;
    IF Mult THEN StatusByte := StatusByte OR $80;

    { Compute QSX offset value - zero means none }

    IF QSXFrequency = 0 THEN
        QSXOffset := 0
    ELSE
        IF Abs (QSXFrequency - Frequency) < 327670 THEN
            {KK1L: 6.64 Scaling added to keep offset within INTEGER range.}
            {SXOffset := (QSXFrequency - Frequency) DIV 10}
            {KK1L: 6.64 Not needed. Only LONGINT seems to work as QSXOffset type}
            QSXOffset := (QSXFrequency - Frequency)
        ELSE
            QSXOffset := 0;   { Unexpected result }

    GetBandMapBandModeFromFrequency (Frequency, Band, Mode);

    IF Band = NoBand THEN Exit;

    { See if the list of entries for this band mode is empty }

    IF BandMapFirstEntryList [Band, Mode] = nil THEN   { Nothing in bandmap yet }
        BEGIN
        New (BandMapFirstEntryList [Band, Mode]);
        BandMapFirstEntryList [Band, Mode]^.Call       := CompressedCall;
        BandMapFirstEntryList [Band, Mode]^.Frequency  := Frequency;
        BandMapFirstEntryList [Band, Mode]^.QSXOffset  := QSXOffset;
        BandMapFirstEntryList [Band, Mode]^.StatusByte := StatusByte;
        BandMapFirstEntryList [Band, Mode]^.NextEntry  := nil;
        Exit;
        END;

    { There are some entries for this band/mode.  Get setup to look
      through the list }

    LastBandMapEntryRecord := nil;
    BandMapEntryRecord     := BandMapFirstEntryList [Band, Mode];

    { Search through the bandmap to find the right place for this entry.
      We keep the frequencies in order, so we need to step through the
      linked list until we find a record with a higher frequency than
      the one being added.  We squeeze this new entry in before that one,
      but check to see if the frequency is nearly the same first. }

    EntryAdded := False;

    WHIlE BandMapEntryRecord <> nil DO
        BEGIN

        { See if the frequency is nearly the same - and if so, we will
          use this record. }

        IF Abs (Frequency - BandMapEntryRecord^.Frequency) <= BandMapGuardBand THEN
            BEGIN
            IF EntryAdded THEN  { Already added it - so delete this one }
                BEGIN

                { We should not be able to be here without
                  LastBandMapEntryRecord being setup to something...

                  We are going to set the .NextEntry pointer of the
                  previous record to point to the NextEntry as indicated
                  in the .NextEntry value of the current record }

                LastBandMapEntryRecord^.NextEntry := BandMapEntryRecord^.NextEntry;
                TempBandMapEntryRecord := BandMapEntryRecord^.NextEntry;

                { Delete the current record. Runtime 204 here? }

                { Runtime 204 here when SPACE entered for frequency }

                Dispose (BandMapEntryRecord);

                { Setup current entry with this next entry.
                  LastBandMapEntryRecord is fine the way it is }

                BandMapEntryRecord := LastBandMapEntryRecord^.NextEntry;
                Continue;
                END;

            { Entry not yet added - take over this record }

            BandMapEntryRecord^.Call       := CompressedCall;
            BandMapEntryRecord^.Frequency  := Frequency;
            BandMapEntryRecord^.QSXOffset  := QSXOffset;
            BandMapEntryRecord^.StatusByte := StatusByte;
            EntryAdded := True;

            { Setup to examine next record and then continue }

            LastBandMapEntryRecord := BandMapEntryRecord;
            BandMapEntryRecord     := BandMapEntryRecord^.NextEntry;
            Continue;
            END;

        IF BigCompressedCallsAreEqual (CompressedCall, BandMapEntryRecord^.Call) THEN
            BEGIN
            { We have found the same call in the band map.  If the frequency
              was close enough - we would have already used it.  Therefore,
              we can probably delete this call. }

            { However, we need to check for the special case that this is the
              first entry in the list - and therefore wouldn't have been
              added yet }

            IF LastBandMapEntryRecord = nil THEN  { This is the 1st entry }
                BEGIN

                { New for 6.50 - if there are no other entries - just use
                  this one again }

                IF BandMapEntryRecord^.NextEntry = nil THEN  { no next entry }
                    BEGIN
                    BandMapEntryRecord^.Call       := CompressedCall;
                    BandMapEntryRecord^.Frequency  := Frequency;
                    BandMapEntryRecord^.QSXOffset  := QSXOffset;
                    BandMapEntryRecord^.StatusByte := StatusByte;
                    Exit;
                    END;

                { New for 6.50 - if the frequency is less than the next
                  bandmap entries, we should also just use this one }


                IF Frequency < BandMapEntryRecord^.Frequency THEN
                    BEGIN
                    BandMapEntryRecord^.Call       := CompressedCall;
                    BandMapEntryRecord^.Frequency  := Frequency;
                    BandMapEntryRecord^.QSXOffset  := QSXOffset;
                    BandMapEntryRecord^.StatusByte := StatusByte;
                    Exit;
                    END;

                { Okay - we have found a record with the same call, but we
                  can't leave it here because the frequency indicates that
                  it should be furthur in the list somewhere.

                  Delete the active record and do the splice so the list
                  still works.  This is a unique way of deleting an entry
                  as we have to worry about the BandMapFirstEntry entry. }

                BandMapFirstEntryList [Band, Mode] := BandMapEntryRecord^.NextEntry;
                TempBandMapEntryRecord := BandMapEntryRecord^.NextEntry;
                Dispose (BandMapEntryRecord);

                BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];
                Continue;
                END;

            { Found an entry with the same callsign - it isn't the first
              entry of the list.  The frequencies aren't close enough that
              we would have already used this record. }

            { New in 6.50 - See if the frequency is still less than the
              next entry - or perhaps that the next entry does not exist.
              In either case, we can still use this record. }

            { See if there are no other entries }

            IF NOT EntryAdded THEN
                BEGIN
                IF BandMapEntryRecord^.NextEntry = nil THEN
                    BEGIN
                    BandMapEntryRecord^.Call       := CompressedCall;
                    BandMapEntryRecord^.Frequency  := Frequency;
                    BandMapEntryRecord^.QSXOffset  := QSXOffset;
                    BandMapEntryRecord^.StatusByte := StatusByte;
                    Exit;
                    END;

            { See if the frequency is less than the next entry }



                IF Frequency < BandMapEntryRecord^.Frequency THEN
                    BEGIN
                    BandMapEntryRecord^.Call       := CompressedCall;
                    BandMapEntryRecord^.Frequency  := Frequency;
                    BandMapEntryRecord^.QSXOffset  := QSXOffset;
                    BandMapEntryRecord^.StatusByte := StatusByte;
                    Exit;
                    END;
                END;

            { Okay - we can't use this record, so we will delete it and
              splice the pointers. }

            LastBandMapEntryRecord^.NextEntry := BandMapEntryRecord^.NextEntry;
            TempBandMapEntryRecord := BandMapEntryRecord^.NextEntry;

            Dispose (BandMapEntryRecord);
            BandMapEntryRecord := LastBandMapEntryRecord^.NextEntry;
            Continue;
            END;

        { See if the frequency is less than the next record.  If so, this
          is where we want to put it }

        IF (NOT EntryAdded) AND
           (Frequency < BandMapEntryRecord^.Frequency) THEN
            BEGIN

            { New for 6.50 }

            IF LastBandMapEntryRecord = nil THEN  { This is the first entry }
                BEGIN
                BandMapEntryRecord := New (BandMapEntryPointer);

                { Do the splice }

                BandMapEntryRecord^.NextEntry := BandMapFirstEntryList [Band, Mode];

                BandMapFirstEntryList [Band, Mode] := BandMapEntryRecord;

                { Save the data }

                BandMapEntryRecord^.Call       := CompressedCall;
                BandMapEntryRecord^.Frequency  := Frequency;
                BandMapEntryRecord^.QSXOffset  := QSXOffset;
                BandMapEntryRecord^.StatusByte := StatusByte;

                EntryAdded := True;

                { Setup to go through the rest of the list }

                LastBandMapEntryRecord := BandMapEntryRecord;
                BandMapEntryRecord     := BandMapEntryRecord^.NextEntry;
                Continue;
                END;

            { We need to squeeze an new entry in here }

            TempBandMapEntryRecord := BandMapEntryRecord;  { Remember }

            BandMapEntryRecord := New (BandMapEntryPointer);

            { Do the splice }

            LastBandMapEntryRecord^.NextEntry := BandMapEntryRecord;

            BandMapEntryRecord^.NextEntry  := TempBandMapEntryRecord;

            { Fill in the data for the new record }

            BandMapEntryRecord^.Call       := CompressedCall;
            BandMapEntryRecord^.Frequency  := Frequency;
            BandMapEntryRecord^.QSXOffset  := QSXOffset;
            BandMapEntryRecord^.StatusByte := StatusByte;

            EntryAdded := True;

            { Setup to continue looking at the rest of the entries.  We
              need to do this in case there is an entry on this band/mode
              later on with the same callsign that needs to be deleted. }

            LastBandMapEntryRecord := BandMapEntryRecord;
            BandMapEntryRecord     := BandMapEntryRecord^.NextEntry;
            Continue;
            END;

        { Point to the next entry in the list }

        LastBandMapEntryRecord := BandMapEntryRecord;
        BandMapEntryRecord     := BandMapEntryRecord^.NextEntry;
        END;

    IF EntryAdded THEN
      BEGIN
      Exit;
      END;


    { We got to the end of the list without finding the call or a place to
      add it.  Add to end of list. }

    BandMapEntryRecord := New (BandMapEntryPointer);

    LastBandMapEntryRecord^.NextEntry := BandMapEntryRecord;

    BandMapEntryRecord^.Call         := CompressedCall;
    BandMapEntryRecord^.Frequency    := Frequency;
    BandMapEntryRecord^.QSXOffset    := QSXOffset;
    BandMapEntryRecord^.StatusByte   := StatusByte;
    BandMapEntryRecord^.NextEntry    := nil;
    END;




PROCEDURE GoToProperXY (NumberEntriesDisplayed: INTEGER;
                        NumberRows: INTEGER;
                        NumberColumns: INTEGER);

VAR Row, Column, ColumnOffset: INTEGER;

    BEGIN
    Row := NumberEntriesDisplayed MOD NumberRows;   { Zero based row }

    ColumnOffset := 80 DIV NumberColumns;
    Column := (NumberEntriesDisplayed DIV NumberRows) * ColumnOffset;

    { Make them start at one now }

    Inc (Row);
    Inc (Column);

    GoToXY (Column, Row);
    END;




PROCEDURE SaveBandMap;

{ Saves the band map data to BandMapFileName }

VAR BandMapEntryRecord: BandMapEntryPointer;
    FileWrite: FILE;
    xResult: INTEGER;
    Mode: ModeType;
    Band: BandType;
    DummyBandMapRecord: BandMapEntry;

    BEGIN
    BigCompressFormat ('XXXXXXXXXXXX', DummyBandMapRecord.Call);

    DummyBandMapRecord.StatusByte := $55;
    DummyBandMapRecord.Frequency  := 0;
    DummyBandMapRecord.NextEntry  := nil;

    Assign  (FileWrite, BandMapFileName);
    ReWrite (FileWrite, 1);

    BlockWrite (FileWrite, BandMapFileVersion, SizeOf (BandMapFileVersion), xResult);
    BlockWrite (FileWrite, BandMapDecayValue, SizeOf (BandMapDecayValue), xResult); {KK1L: 6.70 Keeps map and program in synch}

    FOR Band := Band160 TO Band2 DO
        FOR Mode := CW TO Phone DO
            IF BandMapFirstEntryList [Band, Mode] = nil THEN
                BlockWrite (FileWrite, DummyBandMapRecord, SizeOf (DummyBandMapRecord), xResult)
            ELSE
                BEGIN
                BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];  { 1st entry of linked list }

                WHIlE BandMapEntryRecord <> nil DO
                    BEGIN
                    BlockWrite (FileWrite, BandMapEntryRecord^, SizeOf (BandMapEntryRecord^), xResult);
                    BandMapEntryRecord  := BandMapEntryRecord^.NextEntry;
                    END;
                END;

    Close (FileWrite);
    END;




PROCEDURE LoadBandMap;

{ Loads band map data from BandMapFileName.  Please only call when the
  program has just started and there isn't any possibility of any band
  map information already being there. }

VAR PreviousBandMapEntryRecord, BandMapEntryRecord: BandMapEntryPointer;
    TempBandMapEntryRecord: BandMapEntry;
    FileRead: FILE;
    xResult: INTEGER;
    Band: BandType;
    Mode: ModeType;
    TempChar: CHAR;

    BEGIN
    FOR Band := Band160 TO Band2 DO
        FOR Mode := CW TO Phone DO
            IF BandMapFirstEntryList [Band, Mode] <> nil THEN
                Exit;                                          { You didn't listen! }

    IF NOT FileExists (BandMapFileName) THEN Exit;

    Assign (FileRead, BandMapFileName);
    Reset  (FileRead, 1);

    BlockRead (FileRead, TempChar, SizeOf (TempChar), xResult);

    IF (TempChar <> BandMapFileVersion) OR Eof (FileRead) THEN
        BEGIN
        Close (FileRead);
        Exit;
        END;

    {KK1L: 6.70 Keeps bandmap and program in synch}
    BlockRead (FileRead, BandMapDecayValue, SizeOf (BandMapDecayValue), xResult);
    BandMapDecayMultiplier  := (BandMapDecayValue div 64) + 1; {KK1L: 6.70}
    BandMapDecayTime := BandMapDecayValue div BandMapDecayMultiplier; {KK1L: 6.70}

    {KK1L: 6.64 Only place to guarantee Column zero is the first displayed column!}
    FirstDisplayedBandMapColumn := 0;
    FOR Band := Band160 TO Band2 DO
        FOR Mode := CW TO Phone DO
            IF NOT Eof (FileRead) THEN
                BEGIN
                BlockRead (FileRead, TempBandMapEntryRecord, SizeOf (TempBandMapEntryRecord), xResult);

                IF TempBandMapEntryRecord.Frequency <> 0 THEN  { not filler }
                    BEGIN
                    New (BandMapFirstEntryList [Band, Mode]);

                    BandMapFirstEntryList [Band, Mode]^ := TempBandMapEntryRecord;

                    BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

                    WHILE BandMapEntryRecord^.NextEntry <> nil DO
                        BEGIN
                        PreviousBandMapEntryRecord := BandMapEntryRecord;

                        BandMapEntryRecord := New (BandMapEntryPointer);
                        PreviousBandMapEntryRecord^.NextEntry := BandMapEntryRecord;
                        BlockRead (FileRead, BandMapEntryRecord^, SizeOf (BandMapEntryRecord^), xResult);
                        END;

                    END;
                END;

    Close (FileRead);
    END;


PROCEDURE GetBandMapDisplayInfo (VAR MaxEntriesPerPage: INTEGER;
                                 VAR NumberBandMapRows: INTEGER);

    BEGIN
    IF BandMapWindowRY = 43 THEN {KK1L: 6.64 43 line screen I assume?}
        BEGIN
        IF (MultiInfoMessage = '') THEN
            BEGIN
            MaxEntriesPerPage := 85;
            NumberBandMapRows := 17;
            END
        ELSE
            BEGIN
            MaxEntriesPerPage := 65;
            NumberBandMapRows := 13;
            END
        END
    ELSE         {KK1L: 6.64 50 line screen}
        BEGIN
        IF (MultiInfoMessage = '') THEN
            BEGIN
            MaxEntriesPerPage := 120;
            NumberBandMapRows := 24;
            END
        ELSE
            BEGIN
            MaxEntriesPerPage := 100;
            NumberBandMapRows := 20;
            END;
        END;
  END;


PROCEDURE DisplayBandMap;

VAR StartBand, StopBand: BandType;
    StartMode, StopMode: ModeType;

    BandMapEntryRecord: BandMapEntryPointer;
    NumberEntriesDisplayed, MaxEntriesPerPage, NumberBandMapRows,
      FirstDisplayableBandMapCursor, CurrentCursor,
      LastDisplayableBandMapCursor, DummyEntryNumber: INTEGER;
    WindowCall, BandMapCall, PassFreqStr, RunFreqStr: CallString;
    FreqString: Str20;
    DoBlink: INTEGER;
    Band: BandType;
    Mode: ModeType;
    TempString: Str20;
    MinutesLeft: BYTE;

    BEGIN
    {KK1L: 6.64 Need to add starting column adjustment to DisplayBandMap. This will allow for}
    {      bandmap entries beyond the screen dimensions. The 'global' variable}
    {      FirstDisplayedBandMapColumn is set to zero in LoadBandMap and adjusted as needed}
    {      by ShowBandMapCursor. Only a screen's worth of entries will be displayed.}
    {KK1L: 6.64 I chose somewhat of a brute force approach rather than rewrite the whole proc.}
    {      I mearly IF'd around the GoToProperXY calls when the CurrentCursor position was}
    {      not within the displayed region of the bandmap. NumberEntriesDisplayed is a reference}
    {      to the cursor on the screen, CurrentCursor is a reference to where in the displayable}
    {      bandmap we are from a record perspective, FirstDisplayableBandMapCursor is directly}
    {      referenceable to CurrentCursor.}

    BandMapBlinkingCall := '';

    {KK1L: 6.73 added AND DisplayBandMapEnable because it keeps VGA mode supressed if not active at start up}
    IF (ScreenHeight < 40) AND (NOT DisplayBandMapEnable) THEN Exit;  { We only do band maps on VGA/EGA }

    IF NOT BandMapEnable THEN Exit;

    {KK1L: 6.68 Tree had this check when BandMapRecord did not cover all bands. Removed!}
    {IF VHFBandsEnabled THEN }{KK1L: 6.64 Keep band map within contest limits}
    {  IF (BandMapBand > Band2) OR (BandMapMode > Phone) THEN Exit}
    {ELSE}
    {  IF (BandMapBand > Band12) OR (BandMapMode > Phone) THEN Exit; } {KK1L: 6.65 fixes WARC display enable}

    SaveSetAndClearActiveWindow (BandMapWindow);

    {KK1L: 6.64 set value for use in DisplayBandMap}
    CalculateNumberVisibleBandMapEntries (NumberBandMapEntries, DummyEntryNumber, False);

    IF BandMapAllBands THEN
        BEGIN
        IF VHFBandsEnabled THEN {KK1L: 6.64 Keep band map within contest limits}
            BEGIN
            StartBand := Band160;
            StopBand  := Band2;
            END
        ELSE
            BEGIN
            StartBand := Band160;
            StopBand  := Band12; {KK1L: 6.65 fixes WARC display enable}
            END;
        END
    ELSE
        BEGIN
        StartBand := BandMapBand;
        StopBand  := BandMapBand;
        END;

    IF BandMapAllModes THEN
        BEGIN
        StartMode := CW;
        StopMode  := Phone;
        END
    ELSE
        BEGIN
        StartMode := BandMapMode;
        StopMode  := BandMapMode;
        END;

    REPEAT  {KK1L: 6.69 allows BM to line up with radio after "end" or "home"}

        GetBandMapDisplayInfo (MaxEntriesPerPage, NumberBandMapRows);
        FirstDisplayableBandMapCursor := FirstDisplayedBandMapColumn*NumberBandMapRows;
        LastDisplayableBandMapCursor := FirstDisplayedBandMapColumn*NumberBandMapRows+MaxEntriesPerPage;
        NumberEntriesDisplayed := 0;
        CurrentCursor := 0;
        FoundCursor := FALSE;
        {KK1L: 6.64 BandMapCursorFrequency is set earlier by Tree to be the radio frequency. }
        {      This code checks to see if freq is contained in range. If not and there }
        {      are entries outside the display then move the display.}
        {KK1L: 6.65 Added check for InEditBandMap to replace setting BandMapCursorFrequency := 0 }
        {           as a check for being in EditBandMap. This still keeps the display from }
        {           being controled by the radio freq and allows the cursor to be the entry }
        {           point for EditBandMap.}

        IF (BandMapCursorFrequency < FirstDisplayedBandMapFrequency) AND
           (FirstDisplayableBandMapCursor > 0) AND (BandMapCursorFrequency <> 0) AND
           (FirstDisplayedBandMapColumn > 0) AND (NOT InEditBandMap) THEN
            BEGIN
            DEC(FirstDisplayedBandMapColumn);
            FirstDisplayableBandMapCursor := FirstDisplayedBandMapColumn*NumberBandMapRows;
            LastDisplayableBandMapCursor := FirstDisplayedBandMapColumn*NumberBandMapRows+MaxEntriesPerPage;
            END
        ELSE IF (BandMapCursorFrequency > LastDisplayedBandMapFrequency) AND
                (NumberBandMapEntries > LastDisplayableBandMapCursor) AND
                (NumberBandMapEntries > MaxEntriesPerPage) AND
                (BandMapCursorFrequency <> 0) AND (NOT InEditBandMap) THEN
            BEGIN
            INC(FirstDisplayedBandMapColumn);
            FirstDisplayableBandMapCursor := FirstDisplayedBandMapColumn*NumberBandMapRows;
            LastDisplayableBandMapCursor := FirstDisplayedBandMapColumn*NumberBandMapRows+MaxEntriesPerPage;
          END;

        {KK1L: 6.69 Trying to keep from getting into an infinite loop with the REPEAT UNTIL!}
        IF (BandMapCursorFrequency < FirstDisplayedBandMapFrequency) AND
           (FirstDisplayableBandMapCursor > 0) AND (BandMapCursorFrequency <> 0) AND
           (FirstDisplayedBandMapColumn > 0) AND (NOT InEditBandMap) THEN
            NeedToSynchBandMap := True
        ELSE IF (BandMapCursorFrequency > LastDisplayedBandMapFrequency) AND
                (NumberBandMapEntries > LastDisplayableBandMapCursor) AND
                (NumberBandMapEntries > MaxEntriesPerPage) AND
                (BandMapCursorFrequency <> 0) AND (NOT InEditBandMap) THEN
            NeedToSynchBandMap := True
        ELSE
            NeedToSynchBandMap := False;

        {KK1L: 6.64 Once done comparing radio freq then reset for next time}
        FirstDisplayableBandMapFrequency := 0;
        FirstDisplayedBandMapFrequency := 0;
        LastDisplayableBandMapFrequency := 0;
        LastDisplayedBandMapFrequency := 0;

        {KK1L: 6.64 show bandmap range information to bottom line center of display}
        GoToXY (34, NumberBandMapRows+1);
        TextColor (White);
        Write (BandString [StartBand], ModeString [StartMode], ' - ');
        {KK1L: 6.67 Added if to range display so 12m is only shown when appropriate}
        IF (NOT WARCBandsEnabled) AND (StopBand = Band12) THEN
            Write (BandString [Band10], ModeString [StopMode])
        ELSE
            Write (BandString [StopBand], ModeString [StopMode]);

        {KK1L: 6.67 Added indicator for dupes display mode}
        GoToXY (17, NumberBandMapRows+1);
        TextColor (White);
        IF BandMapDupeDisplay THEN
            Write ('DUPES ON ')
        ELSE
            Write ('DUPES OFF');

        {KK1L: 6.xx Ready for mults mode}
        {GoToXY (51, NumberBandMapRows+1); }
        {TextColor (White);                }
        {IF BandMapMultsOnlyDisplay THEN   }
        {    Write ('MULTS ONLY');         }
        {ELSE                              }
        {    Write ('          ');         }

        {KK1L: 6.64 Show markers to tell if bandmap spills off either or both sides of display}
        IF FirstDisplayableBandMapCursor > 0 THEN
          BEGIN
          GoToXY (2, NumberBandMapRows+1);
          Write ('<====more');
          END
        ELSE
          BEGIN
          GoToXY (2, NumberBandMapRows+1);
          Write ('         ');
          END;
        IF NumberBandMapEntries > LastDisplayableBandMapCursor THEN
          BEGIN
          GoToXY (71, NumberBandMapRows+1);
          Write ('more====>');
          END
        ELSE
          BEGIN
          GoToXY (71, NumberBandMapRows+1);
          Write ('         ');
          END;
        TextColor (SelectedColors.BandMapWindowBackground);

        FOR Band := StartBand TO StopBand DO
            FOR Mode := StartMode TO StopMode DO
                BEGIN

                IF (NOT WARCBandsEnabled) AND
                   ((Band = Band30) OR (Band = Band17) OR (Band = Band12)) THEN
                    Continue; {KK1L: 6.64 Keep band map within contest limits}

                BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

                WHIlE BandMapEntryRecord <> nil DO
                   BEGIN
                   { If it is a dupe and we don't display dupes - skip it }

                   {KK1L: 6.65 Following IF not needed. I It actually gets in the way of both}
                   {           the CallWindowShowAllSpots option as well as not letting      }
                   {           undisplayed (dupe) spots age!                                 }
                   {IF (NOT BandMapDupeDisplay) AND                               }
                   {   ((BandMapEntryRecord^.StatusByte AND $40) <> 0) THEN       }
                   {      BEGIN                                                   }
                   {      BandMapEntryRecord := BandMapEntryRecord^.NextEntry;    }
                   {      Continue;                                               }
                   {      END;                                                    }
                   {KK1L: 6.64 Set FirstDisplayedBandMapFrequency for use on next call to DisplayBandMap}
                   {KK1L: 6.65 Changed to = NumberBandMapRows from 0 to mimic display movement of EditBM}
                   {KK1L: 6.65 Changed it back because I like it better}
                   {KK1L: 6.69 Changed to = NumberBandMapRows from 0 to mimic display movement of EditBM}
                   IF NumberEntriesDisplayed = NumberBandMapRows THEN
                     FirstDisplayedBandMapFrequency := BandMapEntryRecord^.Frequency;
                   {KK1L: 6.64 Set FirstDisplayableBandMapFrequency for use on next call to DisplayBandMap}
                   IF CurrentCursor = 0 THEN
                     FirstDisplayableBandMapFrequency := BandMapEntryRecord^.Frequency;
                   { Position write position in proper place }
                   {KK1L: 6.64 but only if in the range we want to display}
                   {KK1L: 6.65 Moved further down within existing IF...THEN. It's quicker}
                   {IF (CurrentCursor >= FirstDisplayableBandMapcursor) AND              }
                   {   (CurrentCursor <  LastDisplayableBandMapcursor) THEN              }
                   {  GoToProperXY (NumberEntriesDisplayed, NumberBandMapRows, 5);       }

                   WITH BandMapEntryRecord^ DO
                       BEGIN
                       WindowCall := BandMapExpandedString (Call);
                       BandMapCall := WindowCall;
                       {KK1L: 6.64 Sometimes calls wrap into the next field. Truncate to 7 chars for display}
                       {      CallString is 12 chars long. No IF needed since Delete handles strings}
                       {      shorter than index requested. This handles the display. There is a}
                       {      similar change in ShowBandMapCursor to allow the call to show }
                       {      as full size when the cursor passes over it.}
                       Delete(BandMapCall, 8, 12);
                       WHILE (Length(BandMapCall) < 7) DO BandMapCall := BandMapCall + ' '; {KK1L: 6.69 neatens display}
                       {KK1L: 6.64 the dupechecking stuff needed so as not to blink dupes if not displayed}
                       IF (Abs (Frequency - BandMapCursorFrequency) <= BandMapGuardBand) AND
                          (Copy (BandMapCall, 1, 3) <> 'CQ/') THEN  {KK1L: 6.68 changed to below to show CQ} {KK1L: 6.72}
                       {KK1L: 6.72 Removed. Many folks                              }
                       { ended up logging "CQ/xxxx" calls which show up as Portugal.}
                       {IF (Abs (Frequency - BandMapCursorFrequency) <= BandMapGuardBand) THEN}
                              BEGIN
                              IF CallWindowShowAllSpots THEN {KK1L: 6.65}
                                 BEGIN
                                 DoBlink := Blink;
                                 BandMapBlinkingCallRecord := BandMapEntryRecord;
                                 BandMapBlinkingCall := WindowCall;
                                 FoundCursor := TRUE;
                                 IF NOT InEditBandMap THEN
                                   BandMapCursorData := BandMapEntryRecord; {KK1L: 6.65 allows EditBM entry at cursor}
                                 END
                              ELSE IF ((StatusByte AND $40) <> 0) AND BandMapDupeDisplay THEN
                                 BEGIN
                                 DoBlink := Blink;
                                 BandMapBlinkingCallRecord := BandMapEntryRecord;
                                 BandMapBlinkingCall := WindowCall;
                                 FoundCursor := TRUE;
                                 IF NOT InEditBandMap THEN
                                   BandMapCursorData := BandMapEntryRecord; {KK1L: 6.65 allows EditBM entry at cursor}
                                 END
                              ELSE IF ((StatusByte AND $40) = 0) THEN
                                 BEGIN
                                 DoBlink := Blink;
                                 BandMapBlinkingCallRecord := BandMapEntryRecord;
                                 BandMapBlinkingCall := WindowCall;
                                 FoundCursor := TRUE;
                                 IF NOT InEditBandMap THEN
                                   BandMapCursorData := BandMapEntryRecord; {KK1L: 6.65 allows EditBM entry at cursor}
                                 END;
                              END
                          ELSE
                              DoBlink := 0;

                       MinutesLeft := StatusByte AND $3F;

                       IF MinutesLeft > (BandMapDecayTime * 0.96) THEN TextColor (White + DoBlink) ELSE
                           IF MinutesLeft > (BandMapDecayTime * 0.83) THEN TextColor (Yellow + DoBlink) ELSE
                               IF MinutesLeft > (BandMapDecayTime * 0.5) THEN TextColor (LightBlue + DoBlink) ELSE
                                   TextColor (Blue + DoBlink);

                       Str (Frequency, FreqString);
                       Delete (FreqString, Length (FreqString) - 1, 2);
                       Insert ('.', FreqString, Length (FreqString));

                       {KK1L: 6.64, but only if in the range we want to display}
                       {KK1L: 6.73 Added AND DisplayBandMapEnable}
                       IF (CurrentCursor >= FirstDisplayableBandMapcursor) AND
                          (CurrentCursor <  LastDisplayableBandMapcursor) AND
                          (DisplayBandMapEnable) THEN
                         BEGIN
                         GoToProperXY (NumberEntriesDisplayed, NumberBandMapRows, 5); {KK1L: 6.65 Moved here from earlier}
                         IF QSXOffset = 0 THEN
                             BEGIN
                             {KK1L: 6.64 check dupe display status before adding *}
                             {KK1L: 6.65 Moved Inc to inside because now all entries are processed}
                             IF ((StatusByte AND $40) <> 0) AND BandMapDupeDisplay THEN
                                 BEGIN
                                 Write (FreqString, ' *', BandMapCall);
                                 Inc (NumberEntriesDisplayed);
                                 END
                             ELSE
                                 IF (StatusByte AND $80) <> 0 THEN
                                     BEGIN
                                     Write (FreqString, ' m', BandMapCall);
                                     Inc (NumberEntriesDisplayed);
                                     END
                                 ELSE IF (StatusByte AND $40 = 0) THEN  {KK1L: 6.65 Added IF}
                                     BEGIN
                                     Write (FreqString, '  ', BandMapCall);
                                     Inc (NumberEntriesDisplayed);
                                     END;
                             END
                         ELSE
                             BEGIN
                             {KK1L: 6.64 check dupe display status before adding x*}
                             {KK1L: 6.65 Moved Inc to inside because now all entries are processed}
                             IF ((StatusByte AND $40) <> 0) AND BandMapDupeDisplay THEN
                                 BEGIN
                                 Write (FreqString, 'x*', BandMapCall);
                                 Inc (NumberEntriesDisplayed);
                                 END
                             ELSE
                                 IF (StatusByte AND $80) <> 0 THEN
                                     BEGIN
                                     Write (FreqString, 'xm', BandMapCall);
                                     Inc (NumberEntriesDisplayed);
                                     END
                                 ELSE IF (StatusByte AND $40 = 0) THEN  {KK1L: 6.65 Added IF}
                                     BEGIN
                                     Write (FreqString, 'x ', BandMapCall);
                                     Inc (NumberEntriesDisplayed);
                                     END;

                             END;
                         END;
                       END;

                   IF (NumberEntriesDisplayed >= MaxEntriesPerPage) THEN
                     LastDisplayableBandMapFrequency := BandMapEntryRecord^.Frequency;  {KK1L: 6.64}
                   {KK1L: 6.65 subtracted NumberBandMapRows from LastDispBMCursor to mimic display movement of EditBM}
                   {KK1L: 6.65 changed it back. I like it better.}
                   {KK1L: 6.69 subtracted NumberBandMapRows from LastDispBMCursor to mimic display movement of EditBM}
                   IF (CurrentCursor <  LastDisplayableBandMapcursor - NumberBandMapRows) THEN {KK1L: 6.65 < from <= }
                     LastDisplayedBandMapFrequency := BandMapEntryRecord^.Frequency;  {KK1L: 6.64}

                   {KK1L: 6.64, but only if in the range we want to display}
                   IF (CurrentCursor >= FirstDisplayableBandMapcursor) AND
                      (CurrentCursor <  LastDisplayableBandMapcursor) THEN
                     GoToProperXY (NumberEntriesDisplayed, NumberBandMapRows, 5);

                   {KK1L: 6.65 Added the conditional Inc since I now allow all entries (displayed or}
                   {           otherwise) to enter the loop. This was for CallWindowShowAllSpots}
                   IF ((BandMapEntryRecord^.StatusByte AND $40) <> 0) AND BandMapDupeDisplay THEN
                     Inc (CurrentCursor)
                   ELSE IF ((BandMapEntryRecord^.StatusByte AND $40) = 0) THEN
                     Inc (CurrentCursor);

                   BandMapEntryRecord  := BandMapEntryRecord^.NextEntry;
                   END;


                END;

    UNTIL ((BandMapCursorFrequency >= FirstDisplayedBandMapFrequency) AND
           (BandMapCursorFrequency <= LastDisplayedBandMapFrequency)  AND
           (NeedToSynchBandMap))
          OR (NOT NeedToSynchBandMap) OR (BandMapCursorFrequency = 0);
          {KK1L: 6.69 allows BM to line up with radio after "end" or "home"}

    NeedToSynchBandMap := False; {KK1L: 6.69 reset after it has done its job}

    { We display the MultiInfoMessage only if we have set ours to be <> null }

    IF MultiInfoMessage <> '' THEN
        BEGIN
        {GoToXY (1, Hi (WindMax) - Hi (WindMin)  - 3);}
        GoToXY (1, NumberBandMapRows + 2); {KK1L: 6.68}

        TextColor (Black);
        TextBackGround (White);

        ClrEol;   { Added in 6.76 }

        IF K1EANetworkEnable THEN
            BEGIN
            FOR Band := Band160 TO Band10 DO
                WITH K1EAMultiInfo [Band] DO
                    BEGIN
                    Str (PassFreq, PassFreqStr);
                    Str (RunFreq, RunFreqStr);

                    Insert ('.', PassFreqStr, Length (PassFreqStr) - 2);
                    Delete (PassFreqStr, Length (PassFreqStr) - 1, 2);

                    Insert ('.', RunFreqStr, Length (RunFreqStr) - 2);
                    Delete (RunFreqStr, Length (RunFreqStr) - 1, 2);

                    Write ('Run: ', RunFreqStr:7, '  Pass: ', PassFreqStr:7);

                    IF WhereX > 40 THEN
                        BEGIN
                        WriteLn;
                        ClrEol;
                        END
                    ELSE
                        GoToXY (41, WhereY);
                    END;

            END

        ELSE { TR Network }
            BEGIN
            FOR Band := Band160 TO BandLight DO
                FOR Mode := CW TO Phone DO
                    IF MultiStatus [Band, Mode] <> nil THEN
                        BEGIN
                        ClrEol;
                        TempString := BandString [Band] + ModeString [Mode] + ' ' + MultiStatus [Band, Mode]^;

                        Write (TempString);

                        IF WhereX > 40 THEN
                            BEGIN
                            WriteLn;
                            ClrEol;
                            END
                        ELSE
                            GoToXY (41, WhereY);
                        END;
            END;

        { Clear any remaining rows }

        WHILE WhereY < (NumberBandMapRows + 5) DO {KK1L: 6.68}
            BEGIN
            GoToXY (1, WhereY + 1);
            ClrEol;
            END;

        END;

    RestorePreviousWindow;
    END;




PROCEDURE ShowBandMapCursor (CursorPosition: INTEGER;
                             NumberVisibleBandMapEntries: INTEGER;
                             Hide: BOOLEAN);

{ Cursor position is an integer count of the position on the band map,
  starting at zero.  The BandMapCursorData will be updated to point to
  the contents of the spot located at that position. }
{KK1L: 6.64 Changed the name of NumberEntriesDisplayed to NumberVisibleBandMapEntries to}
{      better reflect its use.}

{KK1L: 6.65 NOTE THAT THIS PROC IS ONLY CALLED FROM EditBandMap. THAT PREDICATES SOME OF}
{           THE CODE HERE. JUST BE AWARE OF THAT.}

VAR FreqString: Str10;
    BandMapCall: CallString;
    MinutesLeft, OnScreenCursorPosition,
      MaxEntriesPerPage, NumberBandMapRows:                       INTEGER;

    BEGIN
    SaveAndSetActiveWindow (BandMapWindow);

    {KK1L: 6.64 Need to determine if CursorPosition given fits within currently displayed BM and}
    {      adjust accordingly. DisplayBandMap must be called to make the adjustment}
    GetBandMapDisplayInfo (MaxEntriesPerPage, NumberBandMapRows);
    {BandMapColumnsNeeded := NumberVisibleBandMapEntries MOD NumberBandMapRows;}
    {KK1L: 6.64 Calculate OnScreenCursorPosition from CursorPosition of displayable entries}
    OnScreenCursorPosition := CursorPosition - (FirstDisplayedBandMapColumn*NumberBandMapRows);
    {KK1L: 6.64 If OnScreenCursor position in the fifth(last) column and more could be displayed then}
    {      shift the display.}
    {KK1L: 6.65 Changed to WHILE...DO because keying End (from EditBandMap) would not shift more }
    {           than on column!! This is not needed for radio freq tracking (in DisplayBandMap)}
    IF (OnScreenCursorPosition > (4*NumberBandMapRows)-1) AND
       (NumberVisibleBandMapEntries > (FirstDisplayedBandMapColumn+5)*NumberBandMapRows) THEN
      WHILE (OnScreenCursorPosition > (4*NumberBandMapRows)-1) AND
         (NumberVisibleBandMapEntries > (FirstDisplayedBandMapColumn+5)*NumberBandMapRows) DO
        BEGIN
        Inc(FirstDisplayedBandMapColumn);
        OnScreenCursorPosition := CursorPosition - (FirstDisplayedBandMapColumn*NumberBandMapRows);
        DisplayBandMap;
        END
    {KK1L: 6.64 If OnScreenCursor position in the first column and there are more to the left then}
    {      shift the display.}
    {KK1L: 6.65 Changed to WHILE...DO because keying Home (from EditBandMap) would not shift more }
    {           than on column!! This is not needed for radio freq tracking (in DisplayBandMap)}
    ELSE IF (OnScreenCursorPosition < NumberBandMapRows) AND (FirstDisplayedBandMapColumn > 0) THEN
      WHILE (OnScreenCursorPosition < NumberBandMapRows) AND (FirstDisplayedBandMapColumn > 0) DO
        BEGIN
        Dec(FirstDisplayedBandMapColumn);
        OnScreenCursorPosition := CursorPosition - (FirstDisplayedBandMapColumn*NumberBandMapRows);
        DisplayBandMap;
        END;

    GoToProperXY (OnScreenCursorPosition, NumberBandMapRows, 5);

    IF NumberVisibleBandMapEntries >0 THEN {KK1L: 6.64 covers case of only dupes in bandmap}
      BEGIN
        WITH BandMapCursorData^ DO
            BEGIN
            BandMapCall := BandMapExpandedString (Call);
            {KK1L: 6.64 Sometimes calls wrap into the next field. Truncate to 7 chars for display}
            {      CallString is 12 chars long. No IF needed since Delete handles strings}
            {      shorter than index requested. This change also truncates the call when the}
            {      cursor passes over it.}
            Delete(BandMapCall, 8, 12);
            WHILE (Length(BandMapCall) < 7) DO BandMapCall := BandMapCall + ' '; {KK1L: 6.69 neatens display}
            IF Hide THEN
                BEGIN
                MinutesLeft := StatusByte AND $3F;

                IF MinutesLeft > (BandMapDecayTime * 0.96) THEN TextColor (White) ELSE
                    IF MinutesLeft > (BandMapDecayTime * 0.83) THEN TextColor (Yellow) ELSE
                        IF MinutesLeft > (BandMapDecayTime * 0.5) THEN TextColor (LightBlue) ELSE
                            TextColor (Blue);

                TextBackground (SelectedColors.BandMapWindowBackground);
                END
            ELSE
                BEGIN
                TextColor      (SelectedColors.BandMapWindowBackground);
                TextBackground (SelectedColors.BandMapWindowColor);
                END;


            Str (Frequency, FreqString);
            Delete (FreqString, Length (FreqString) - 1, 2);
            Insert ('.', FreqString, Length (FreqString));

            IF QSXOffset = 0 THEN
                BEGIN
                IF (StatusByte AND $40) <> 0 THEN
                    Write (FreqString, ' *', BandMapCall)
                ELSE
                    IF (StatusByte AND $80) <> 0 THEN
                        Write (FreqString, ' m', BandMapCall)
                    ELSE
                        Write (FreqString, '  ', BandMapCall);
                END
            ELSE
                BEGIN
                IF (StatusByte AND $40) <> 0 THEN
                    Write (FreqString, 'x*', BandMapCall)
                ELSE
                    IF (StatusByte AND $80) <> 0 THEN
                        Write (FreqString, 'xm', BandMapCall)
                    ELSE
                        Write (FreqString, 'x ', BandMapCall);

                END;
            END;
      END
      ELSE {KK1L: 6.64 case where only dupes in the bandmap}
        BEGIN
        TextColor(Red);
        Write ('Nothing to display!');
        END;
    RestorePreviousWindow;

    End;



PROCEDURE UpdateBlinkingBandMapCall;

    BEGIN
    IF NOT BandMapEnable THEN Exit;

    IF BandMapBlinkingCallRecord <> nil THEN
        WITH BandMapBlinkingCallRecord^ DO
            BEGIN
            StatusByte := StatusByte AND $C0;
            StatusByte := (BandMapDecayTime AND $3F) OR StatusByte;
            END;

    DisplayBandMap;
    END;




PROCEDURE UpdateTenMinuteDate (Band: BandType; Mode: ModeType);

    BEGIN
    IF (Band <> TenMinuteTime.Band) OR (Mode <> TenMinuteTime.Mode) THEN
        BEGIN
        TenMinuteTime.Band := Band;
        TenMinuteTime.Mode := Mode;
        WITH TenMinuteTime.Time DO GetTime (Hour, Minute, Second, Sec100);
        END;
    END;




PROCEDURE CalculateNumberVisibleBandMapEntries (VAR NumberVisibleBandMapEntries: INTEGER;
                                                VAR CursorEntryNumber: INTEGER;
                                                StartAtTop: BOOLEAN);

{ New in 6.49 }
{ Compute number of band map entries and which entry number the
  BandMapCursorData (global variable) resides at. }

VAR Band, StartBand, StopBand: BandType;
    Mode, StartMode, StopMode: ModeType;
    BandMapEntryRecord: BandMapEntryPointer;

    BEGIN
    IF BandMapAllBands THEN
        BEGIN
        IF VHFBandsEnabled THEN {KK1L: 6.64 Keep band map within contest limits}
            BEGIN
            StartBand := Band160;
            StopBand  := Band2;
            END
        ELSE
            BEGIN
            StartBand := Band160;
            StopBand  := Band12;  {KK1L: 6.65 fixes WARC display enable}
            END;
        END
    ELSE
        BEGIN
        StartBand := BandMapBand;
        StopBand  := BandMapBand;
        END;

    IF BandMapAllModes THEN
        BEGIN
        StartMode := CW;
        StopMode  := Phone;
        END
    ELSE
        BEGIN
        StartMode := BandMapMode;
        StopMode  := BandMapMode;
        END;

    NumberVisibleBandMapEntries := 0;
    CursorEntryNumber           := 0;

    IF StartAtTop THEN
        BandMapCursorData := BandMapFirstEntryList [StartBand, StartMode];

    FoundCursor := False;

    FOR Band := StartBand TO StopBand DO
        FOR Mode := StartMode TO StopMode DO
            BEGIN

            IF (NOT WARCBandsEnabled) AND
               ((Band = Band30) OR (Band = Band17) OR (Band = Band12)) THEN
                Continue; {KK1L: 6.64 Keep band map within contest limits}

            BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

            WHILE BandMapEntryRecord <> nil DO
                BEGIN
                IF ((BandMapEntryRecord^.StatusByte AND $40) <> 0) AND BandMapDupeDisplay THEN
                    BEGIN {entry is a dupe and we are displaying dupes }
if bandmapcursordata <> nil then
                    IF BandMapEntryRecord^.Frequency = BandMapCursorData^.Frequency THEN
                      BEGIN
                      BandMapCursorData := BandMapEntryRecord;
                      FoundCursor := True;
                      END;
                    IF NOT FoundCursor THEN Inc (CursorEntryNumber);
                    Inc (NumberVisibleBandMapEntries);
                    END
                ELSE IF ((BandMapEntryRecord^.StatusByte AND $40) = 0) THEN
                    BEGIN {KK1L: 6.64 entry is not a dupe so just check it}
if bandmapcursordata <> nil then
                    IF BandMapEntryRecord^.Frequency = BandMapCursorData^.Frequency THEN
                      BEGIN
                      BandMapCursorData := BandMapEntryRecord;
                      FoundCursor := True;
                      END;
                    IF NOT FoundCursor THEN Inc (CursorEntryNumber);
                    Inc (NumberVisibleBandMapEntries);
                    END;
                BandMapEntryRecord := BandMapEntryRecord^.NextEntry;
                END;
            END;
    IF NOT FoundCursor THEN
      {KK1L: 6.64 covers the case of the cursor on a dupe when BandMapDupeDisplay is toggled FALSE}
      {      Really it just finds the first appropriate entry to display at entry zero}
      {      It covers cases of single band, all bands, all modes, all bands and modes}
      BEGIN
      CursorEntryNumber := 0;
      GetRecordForBandMapCursor(BandMapCursorData, CursorEntryNumber);
      END;

    END;



FUNCTION GetRecordForBandMapCursor (VAR Entry: BandMapEntryPointer;
                                        CursorEntryNumber: INTEGER) : BOOLEAN;
{ KK1L: 6.64 Rewritten as function for possible multiple use. Code updated to work properly }
{       when displaying dupes or not.                                                  }
{ KK1L: 6.65 CursorEntryNumber did not need to be "pass by reference". Changed to "pass by value"}

VAR EntryNumber : INTEGER;
    Band, StartBand, StopBand: BandType;
    Mode, StartMode, StopMode: ModeType;

  BEGIN
    IF BandMapAllBands THEN
        BEGIN
        IF VHFBandsEnabled THEN {KK1L: 6.64 Keep band map within contest limits}
            BEGIN
            StartBand := Band160;
            StopBand  := Band2;
            END
        ELSE
            BEGIN
            StartBand := Band160;
            StopBand  := Band12;  {KK1L: 6.65 fixes WARC display enable}
            END;
        END
    ELSE
        BEGIN
        StartBand := BandMapBand;
        StopBand  := BandMapBand;
        END;

    IF BandMapAllModes THEN
        BEGIN
        StartMode := CW;
        StopMode  := Phone;
        END
    ELSE
        BEGIN
        StartMode := BandMapMode;
        StopMode  := BandMapMode;
        END;

    EntryNumber := 0;
    GetRecordForBandMapCursor := FALSE;

    FOR Band := StartBand TO StopBand DO
        BEGIN
        FOR Mode := StartMode TO StopMode DO
          BEGIN

          IF (NOT WARCBandsEnabled) AND
             ((Band = Band30) OR (Band = Band17) OR (Band = Band12)) THEN
              Continue; {KK1L: 6.64 Keep band map within contest limits}

          Entry := BandMapFirstEntryList [Band, Mode];
          WHILE Entry <> nil DO
            BEGIN

            IF (EntryNumber = CursorEntryNumber) THEN
              BEGIN {KK1L: 6.64 found a cursor match. Get Entry^ in synch then set BandMapCursorData}
              IF (NOT BandMapDupeDisplay) THEN {KK1L: 6.64 if not displaying dupes find the next non-dupe}
                WHILE ((Entry^.StatusByte AND $40) <> 0) AND (Entry <> nil) DO
                  Entry := Entry^.NextEntry;
              IF (Entry <> nil) THEN
                BEGIN {KK1L: 6.64 Only set data if not nil}
                BandMapCursorData := Entry;
                GetRecordForBandMapCursor := TRUE;
                Exit; {KK1L: 6.64 Found a match. Let's get out of here}
                END;
              Break; {KK1L: 6.64 Ran out of entries. Jump out of WHILE Entry <> nil to next Mode/Band}
              END

            ELSE {KK1L: 6.64 not a cursor match. Go to next keeping Entry^ and EntryNumber in synch with display}
              BEGIN
              IF (NOT BandMapDupeDisplay) THEN {KK1L: 6.64 if not displaying dupes find the next non-dupe}
                WHILE ((Entry^.StatusByte AND $40) <> 0) AND (Entry <> nil) DO
                  Entry := Entry^.NextEntry; {KK1L: 6.64 non-dupe of current cursor}
              IF Entry <> nil THEN {KK1L: 6.64 Only increment if not starting a new band/mode}
                BEGIN
                Entry := Entry^.NextEntry;
                Inc(EntryNumber);
                END;
              END;

            END;
          END;
        END;
  END;



PROCEDURE EditBandMap;

{ BandMapCursorData gets left with data from active entry.  Frequency will
  be = 0 if no entry left.  }

VAR CursorEntryNumber, XPos, YPos, MaxEntriesPerPage, NumberBandMapRows: INTEGER;
    Button1, Button2: BOOLEAN;
    BandMapEntryRecord: BandMapEntryPointer;

    BEGIN
    IF NOT BandMapEnable THEN Exit;

    {KK1L: 6.64 This is how I keep the bandmap from whigging out while trying to navigate with the}
    {      cursor. If freq=0 then the radio tracking logic is ignored in DisplayBandMap. I}
    {      reset BandMapCursorFrequency back just before exiting for Esc, Enter or otherwise.}

    {KK1L: 6.65 Removed following two lines and now use InEditBandMap as flag to control DisplayBandMap.}
    {TempFreq := BandMapCursorFrequency;}
    {BandMapCursorFrequency := 0;       }

    CheckMouse (Xpos, YPos, Button1, Button2);   { Clear counters }

    {KK1L: 6.65 Changed "start at top" to False. True was moving cursor to entry zero!}
    {           This allows us to enter the BM for edit at the current cursor position.}
    CalculateNumberVisibleBandMapEntries (NumberBandMapEntries, CursorEntryNumber, False);
    GetBandMapDisplayInfo (MaxEntriesPerPage, NumberBandMapRows);

    IF NumberBandMapEntries = 0 THEN Exit;   { New in 6.49 }

    InEditBandMap := True;
    EscapeFromEditBandMap := False;

    {KK1L: 6.71 Changed menu string to include hotkeys}
    QuickDisplay ('Arrows=move RET=Select DEL=Delete M=modes D=dupes B=bands ESC=quit');

    { Now - highlight the entry where the cursor is and process keystrokes }

    {KK1L: 6.65 Removed following to allow us to enter the BM for edit at the current cursor position.}
    {BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];}
    {CursorEntryNumber :=0;}

    REPEAT
        ShowBandMapCursor (CursorEntryNumber, NumberBandMapEntries, False);  { BandMapCursorData will have data }

{        BandMapStatusString := '';                                                             }
{        IF BandMapAllBands THEN BandMapStatusString := BandMapStatusString + 'ALL bands '      }
{        ELSE BandMapStatusString := BandMapStatusString + 'CURRENT band ';                     }
{        IF BandMapAllModes THEN BandMapStatusString := BandMapStatusString + 'ALL modes '      }
{        ELSE BandMapStatusString := BandMapStatusString + 'CURRENT mode ';                     }
{        IF BandMapDupeDisplay THEN BandMapStatusString := BandMapStatusString + 'WITH dupes '  }
{        ELSE BandMapStatusString := BandMapStatusString + 'NO dupes ';                         }
{        QuickDisplay2(BandMapStatusString);                                                    }

        REPEAT millisleep UNTIL NewKeyPressed OR
                     (MouseEnable AND CheckMouse (XPos, YPos, Button1, Button2));

        IF NewKeyPressed THEN
            BEGIN
            CASE NewReadKey OF

                EscapeKey:
                    BEGIN
                    ShowBandMapCursor (CursorEntryNumber, NumberBandMapEntries, True);
                    RemoveWindow (QuickCommandWindow);
                    {KK1L 6.65 no longer needed}
                    { BandMapCursorFrequency := TempFreq;} {KK1L: 6.64 restore radio freq value}
                    {BandMapCursorData := nil;} {KK1L: 6.64 Keeps call from showing up in call window}
                    InEditBandMap := False;
                    EscapeFromEditBandMap := True; {KK1L: 6.65 Replaces BandMapCursorDate := nil above}
                    NeedToSynchBandMap := True; {KK1L: 6.69}
                    DisplayBandMap; {KK1L: 6.69 synchs the map to the radio}
                    Exit;
                    END;

                CarriageReturn:
                    BEGIN
                    IF (BandMapBand = BandMemory[InactiveRadio]) and 
                      (BandMapBand <> BandMemory[ActiveRadio]) THEN {KK1L: 6.73}
                        SetUpBandMapEntry (BandMapCursorData, InactiveRadio) {KK1L: 6.73 Added InactiveRadio}
                    ELSE
                        SetUpBandMapEntry (BandMapCursorData, ActiveRadio); {KK1L: 6.73 Added ActiveRadio}
                    ShowBandMapCursor (CursorEntryNumber, NumberBandMapEntries, True);
                    RemoveWindow (QuickCommandWindow);
                    {KK1L 6.65 no longer needed}
                    {BandMapCursorFrequency := TempFreq;} {KK1L: 6.64 restore radio freq value}
                    InEditBandMap := False;
                    Exit;
                    END;

                'd', 'D' : { KK1L: 6.64 Added ability to toggle dupe display on the fly }
                    BEGIN
                    BandMapDupeDisplay := NOT BandMapDupeDisplay;
                    CursorEntryNumber := 0;
                    GetRecordForBandMapCursor (BandMapEntryRecord, CursorEntryNumber);
                    ShowBandMapCursor (CursorEntryNumber, NumberBandMapEntries, False);
                    DisplayBandMap;
                    END;
                'b', 'B' : { KK1L: 6.64 Added ability to toggle band display on the fly }
                    BEGIN
                    BandMapAllBands := NOT BandMapAllBands;
                    CursorEntryNumber := 0;
                    GetRecordForBandMapCursor (BandMapEntryRecord, CursorEntryNumber);
                    ShowBandMapCursor (CursorEntryNumber, NumberBandMapEntries, False);
                    DisplayBandMap;
                    END;
                'm', 'M' : { KK1L: 6.64 Added ability to toggle mode display on the fly }
                    BEGIN
                    BandMapAllModes := NOT BandMapAllModes;
                    CursorEntryNumber := 0;
                    GetRecordForBandMapCursor (BandMapEntryRecord, CursorEntryNumber);
                    ShowBandMapCursor (CursorEntryNumber, NumberBandMapEntries, False);
                    DisplayBandMap;
                    END;
                'g', 'G' : { KK1L: 6.68 Added ability to toggle mult display on the fly }
                    BEGIN
                    BandMapMultsOnly := NOT BandMapMultsOnly;
                    CursorEntryNumber := 0;
                    GetRecordForBandMapCursor (BandMapEntryRecord, CursorEntryNumber);
                    ShowBandMapCursor (CursorEntryNumber, NumberBandMapEntries, False);
                    DisplayBandMap;
                    END;

                NullKey:
                    BEGIN
                    ShowBandMapCursor (CursorEntryNumber, NumberBandMapEntries, True);

                    CASE NewReadKey OF
                        DeleteKey:
                            IF BandMapCursorData <> Nil then
                            BEGIN
                            IF BandMapCursorData^.Frequency > 0 THEN
                                BEGIN
                                DeleteBandMapEntry (BandMapCursorData);
                                {CalculateNumberVisibleBandMapEntries (NumberBandMapEntries, CursorEntryNumber, False);}
                                DisplayBandMap;

                                {IF NumberBandMapEntries = 0 THEN}
                                {    BEGIN}
                                {    RemoveWindow (QuickCommandWindow);}
                                {    }{KK1L 6.65 no longer needed}
                                {    }{BandMapCursorFrequency := TempFreq; }{KK1L: 6.64 restore radio freq value}
                                {    InEditBandMap := False;}
                                {    Exit;}
                                {    END;}
                                END;
                             END;

                        DownArrow:
                            IF (CursorEntryNumber < NumberBandMapEntries - 1) THEN
                                 Inc (CursorEntryNumber);

                        UpArrow:
                            IF CursorEntryNumber > 0 THEN
                                 Dec (CursorEntryNumber);

                        RightArrow:
                                IF CursorEntryNumber + NumberBandMapRows < NumberBandMapEntries THEN
                                    CursorEntryNumber := CursorEntryNumber + NumberBandMapRows;

                        HomeKey:
                            BEGIN
                            CursorEntryNumber := 0;
                            END;

                        LeftArrow:
                                IF CursorEntryNumber > (NumberBandMapRows-1) THEN
                                    CursorEntryNumber := CursorEntryNumber - NumberBandMapRows;

                        EndKey:
                            BEGIN
                            CursorEntryNumber := NumberBandMapEntries - 1;
                            END;

                        END;

                    {KK1L: 6.64 Rewrote the match-cursor-to-data routine as a function}
                    GetRecordForBandMapCursor (BandMapEntryRecord, CursorEntryNumber);
                    ShowBandMapCursor (CursorEntryNumber, NumberBandMapEntries, False);
                    END;

                END;  { of case }
            END

        ELSE    { Must of been a mouse hit }
                { KK1L: 6.64 The mouse stuff is working! I don't know what it acts like on a slow}
                {       machine. I could be pretty funky looking. Button 2 toggles dupe display}
                {       rather than delete and entry. Button 1 selects an entry. I would like}
                {       to make mouse navigation available from the main logging screen by pressing}
                {       button 2. I will look at that.}
            BEGIN
            IF Button1 THEN
                BEGIN
                SetUpBandMapEntry (BandMapCursorData, ActiveRadio); {KK1L: 6.73 Adeed ActiveRadio}
                DisplayBandMap;
                RemoveWindow (QuickCommandWindow);
                {KK1L 6.65 no longer needed}
                {BandMapCursorFrequency := TempFreq; }{KK1L: 6.64 restore radio freq value}
                InEditBandMap := False;
                Exit;
                END;

            IF Button2 THEN
                BEGIN
                BandMapDupeDisplay := NOT BandMapDupeDisplay;
                DisplayBandMap;
                REPEAT millisleep UNTIL KeyPressed OR ((CheckMouse (XPos, YPos, Button1, Button2)) AND (NOT Button2));
                  {IF BandMapCursorData^.Frequency > 0 THEN                                                         }
                  {    BEGIN                                                                                        }
                  {    DeleteBandMapEntry (BandMapCursorData);                                                      }
                  {    CalculateNumberVisibleBandMapEntries (NumberBandMapEntries, CursorEntryNumber, False);}
                  {    DisplayBandMap;                                                                              }
                  {                                                                                                 }
                  {    IF NumberBandMapEntries = 0 THEN                                                      }
                  {        BEGIN                                                                                    }
                  {        RemoveWindow (QuickCommandWindow);                                                       }
                  {        Exit;                                                                                    }
                  {        END;                                                                                     }
                  {    END;                                                                                         }
                END;


            IF XPos > 9 THEN
                BEGIN
                IF CursorEntryNumber + NumberBandMapRows < NumberBandMapEntries THEN
                  CursorEntryNumber := CursorEntryNumber + NumberBandMapRows;
                DisplayBandMap;
                END;

            IF XPos < -9 THEN
                BEGIN
                IF CursorEntryNumber > NumberBandMapRows - 1 THEN
                  CursorEntryNumber := CursorEntryNumber - NumberBandMapRows;
                DisplayBandMap;
                END;

            IF YPos > 3 THEN
                BEGIN
                IF CursorEntryNumber < NumberBandMapEntries - 1 THEN
                    Inc (CursorEntryNumber);
                DisplayBandMap;
                END;

            IF YPos < -3 THEN
                BEGIN
                IF CursorEntryNumber > 0 THEN Dec (CursorEntryNumber);
                DisplayBandMap;
                END;

            GetRecordForBandMapCursor (BandMapEntryRecord, CursorEntryNumber);
            END;

    UNTIL False;
    END;




PROCEDURE DecrementQTCCount (Call: CallString; Count: INTEGER);

VAR Station: INTEGER;
    CompressedCall: EightBytes;

    BEGIN
    BigCompressFormat (Call, CompressedCall);

    IF (NumberQTCStations = 0) OR (Count = 0) THEN Exit;

    FOR Station := 0 TO NumberQTCStations - 1 DO
        IF BigCompressedCallsAreEqual (QTCDataArray ^[Station].Call, CompressedCall) THEN
            WHILE (QTCDataArray ^[Station].NumberQTCs > 0) AND (Count > 0) DO
                BEGIN
                Dec (QTCDataArray ^[Station].NumberQTCs);
                Dec (Count);
                END;

    END;




FUNCTION TotalNumberQTCsProcessed: INTEGER;

VAR Total, Station: INTEGER;

    BEGIN
    IF NumberQTCStations = 0 THEN
        BEGIN
        TotalNumberQTCsProcessed := 0;
        Exit;
        END;

    Total := 0;

    FOR Station := 0 TO NumberQTCStations - 1 DO
        Total := Total + QTCDataArray ^[Station].NumberQTCs;

    TotalNumberQTCsProcessed := Total;
    END;




PROCEDURE AddQSOToPendingQTCList (Time: INTEGER; Call: CallString; QSONumber: INTEGER);

VAR TempString: Str20;

    BEGIN
    IF TotalPendingQTCs >= PendingQTCArraySize - 1 THEN
        BEGIN
        QuickDisplayError ('Unable add QTCs.  Please send some and then restart the program.');
        Exit;
        END;

    PendingQTCArray ^[NextQTCToBeAdded].Time := Time;
    BigCompressFormat (Call, PendingQTCArray ^[NextQTCToBeAdded].Call);
    PendingQTCArray ^[NextQTCToBeAdded].QSONumber := QSONumber;

    Inc (NextQTCToBeAdded);

    IF NextQTCToBeAdded = PendingQTCArraySize THEN
        NextQTCToBeAdded := 0;

    IF TotalPendingQTCs > PendingQTCArraySize - 30 THEN
        BEGIN
        Str (PendingQTCArraySize, TempString);
        QuickDisplayError ('PLEASE send some QTCs!!  You are close to the ' + TempString + ' QTC limit.');
        ReminderPostedCount := 60;
        END;
    END;





FUNCTION TotalPendingQTCs: INTEGER;

    BEGIN
    IF NextQTCToBeAdded >= NextQTCToBeSent THEN
        TotalPendingQTCs := NextQTCToBeAdded - NextQTCToBeSent
    ELSE
        TotalPendingQTCs := (NextQTCToBeAdded + PendingQTCArraySize) - NextQTCToBeSent;
    END;




FUNCTION NumberAvailableQTCsForThisCall (Call: CallString): INTEGER;

VAR QTC, NumberQTCsPending: INTEGER;
    CompressedCall: EightBytes;

    BEGIN
    NumberQTCsPending := TotalPendingQTCs;

    IF NumberQTCsPending = 0 THEN
        BEGIN
        NumberAvailableQTCsForThisCall := 0;
        Exit;
        END;

    IF NumberQTCsPending > 10 THEN NumberQTCsPending := 10;

    BigCompressFormat (Call, CompressedCall);

    FOR QTC := 0 TO NumberQTCsPending - 1 DO
        IF BigCompressedCallsAreEqual (CompressedCall, PendingQTCArray ^[NextQTCToBeSent + QTC].Call) THEN
            BEGIN
            NumberAvailableQTCsForThisCall := QTC;
            Exit;
            END;

    NumberAvailableQTCsForThisCall := NumberQTCsPending;
    END;


FUNCTION PopNextQTCToBeSent (VAR FullString: Str80): Str80;

VAR TimeString, QSONumberString, CallString: Str20;
    CharPointer: INTEGER;

    BEGIN
    IF NextQTCToBeSent = NextQTCToBeAdded THEN   { Nothing available }
        BEGIN
        PopNextQTCToBeSent := '';
        Exit;
        END;

    Str (PendingQTCArray ^[NextQTCToBeSent].Time, TimeString);

    CallString := BigExpandedString (PendingQTCArray ^[NextQTCToBeSent].Call);

    IF Length (CallString) > 0 THEN
        FOR CharPointer := 1 TO Length (CallString) DO
            IF CallString [CharPointer] = ' ' THEN
                CallString [CharPointer] := '/';

    Str (PendingQTCArray ^[NextQTCToBeSent].QSONumber, QSONumberString);

    WHILE Length (TimeString) < 4 DO TimeString := '0' + TimeString;

    FullString := TimeString + ' ' + CallString + ' ' + QSONumberString;

    IF QTCMinutes THEN
        BEGIN
        IF (TimeString [1] = LastQTCTimeSent [1]) AND
           (TimeString [2] = LastQTCTimeSent [2]) THEN
               BEGIN
               LastQTCTimeSent := TimeString;
               Delete (TimeString, 1, 2);
               END
           ELSE
               LastQTCTimeSent := TimeString;
        END;

    PopNextQTCToBeSent := TimeString + ' ' + CallString + ' ' + QSONumberString;

    Inc (NextQTCToBeSent);

    IF NextQTCToBeSent = PendingQTCArraySize THEN
        NextQTCToBeSent := 0;
    END;




PROCEDURE OutputQSOorMultStatusString (OutputString: Str160);

VAR Cursor: INTEGER;

    BEGIN
    GetRidOfPostcedingSpaces (OutputString);

    IF WhereX > 1 THEN WriteLn;     { Added in 6.28 }

    IF Length (OutputString) <= ((MultiplierInformationWindowRX - MultiplierInformationWindowLX) + 2) - WhereX THEN

        { There is enough room for the whole string }

        Write (OutputString)
    ELSE
        BEGIN
        IF StringHas (OutputString, CarriageReturn) THEN
            BEGIN
            WriteLn (PrecedingString (OutputString, CarriageReturn));

            OutputString := PostcedingString (OutputString, CarriageReturn);

            IF Copy (OutputString, 1, 1) = LineFeed THEN
                Delete (OutputString, 1, 1);

            Write (OutputString);
            END
        ELSE
            WHILE OutputString <> '' DO
                BEGIN
                Cursor := (MultiplierInformationWindowRX - MultiplierInformationWindowLX) + 2;

                IF WhereX > 1 THEN Cursor := (Cursor - WhereX) + 1;

                WHILE OutputString [Cursor] <> ' ' DO
                    BEGIN
                    Dec (Cursor);

                    IF Cursor = 0 THEN
                        BEGIN
                        Write (OutputString);
                        Exit;
                        END;
                    END;

                Write (Copy (OutputString, 1, Cursor - 1));
                Delete (OutputString, 1, Cursor);
                IF OutputString <> '' THEN WriteLn;
                END;
        END;
    END;




PROCEDURE DisplayPrefixInfo (Prefix: Str20);

    BEGIN
    IF PrefixInfoFileName = '' THEN Exit;
    END;




PROCEDURE DisplayUserInfo (Call: CallString);

VAR Data: DataBaseEntryRecord;
    TempString, InfoString, Command, CustomString: Str40;
    Zone: INTEGER;
    FoundCall: BOOLEAN;
    Heading: INTEGER;

    BEGIN
    IF UserInfoShown = NoUserInfo THEN Exit;

    SaveSetAndClearActiveWindow (UserInfoWindow);

    IF Call = '' THEN
        BEGIN
        ClrScr;
        RestorePreviousWindow;
        Exit;
        END;

    FoundCall := CD.GetEntry (Call, Data);

    InfoString := '';

    CASE UserInfoShown OF
        NameInfo:         IF FoundCall THEN InfoString := Data.Name;
        QTHInfo:          IF FoundCall THEN InfoString := Data.QTH;
        CheckSectionInfo: IF FoundCall THEN InfoString := Data.Check + Data.Section;
        SectionInfo:      IF FoundCall THEN InfoString := Data.Section;
        OldCallInfo:      IF FoundCall THEN InfoString := Data.OldCall;
        FOCInfo:          IF FoundCall THEN InfoString := Data.FOC;
        GridInfo:         IF FoundCall THEN
                              BEGIN
                              InfoString := Data.Grid;

                              IF (MyGrid <> '') AND LooksLikeAGrid (Data.Grid) THEN
                                  BEGIN
                                  SaveSetAndClearActiveWindow (BeamHeadingWindow);
                                  Heading := Round (GetBeamHeading (MyGrid, Data.Grid));
                                  Write (Data.Grid, ' at ', Heading, DegreeSymbol);
                                  RestorePreviousWindow;
                                  END;
                              END;

        CQZoneInfo:
            IF FoundCall AND (Data.CQZone <> '') THEN
                InfoString := Data.CQZone
            ELSE
                BEGIN
                Zone := CountryTable.GetCQZone (Call);
                IF Zone > 0 THEN Str (Zone, InfoString);
                END;

        ITUZoneInfo:
            IF FoundCall AND (Data.ITUZone <> '') THEN
                InfoString := Data.ITUZone
            ELSE
                BEGIN
                Zone := CountryTable.GetITUZone (Call);
                IF Zone > 0 THEN Str (Zone, InfoString);
                END;


        User1Info:        IF FoundCall THEN InfoString := Data.User1;
        User2Info:        IF FoundCall THEN InfoString := Data.User2;
        User3Info:        IF FoundCall THEN InfoString := Data.User3;
        User4Info:        IF FoundCall THEN InfoString := Data.User4;
        User5Info:        IF FoundCall THEN InfoString := Data.User5;

        CustomInfo:
            BEGIN
            InfoString := '';
            CustomString := CustomUserString;

            WHILE CustomString <> '' DO
                BEGIN
                Command := RemoveFirstString (CustomString);

                IF Command = 'CQZONE' THEN
                    IF FoundCall AND (Data.CQZone <> '') THEN
                        InfoString := InfoString + Data.CQZone + ' '
                    ELSE
                        BEGIN
                        Zone := CountryTable.GetCQZone (Call);

                        IF Zone > 0 THEN
                            BEGIN
                            Str (Zone, TempString);
                            InfoString := InfoString + TempString + ' ';
                            END;
                        END;

                IF Command = 'ITUZONE' THEN
                    IF FoundCall AND (Data.ITUZone <> '') THEN
                        InfoString := InfoString + Data.ITUZone + ' '
                    ELSE
                        BEGIN
                        Zone := CountryTable.GetITUZone (Call);

                        IF Zone > 0 THEN
                            BEGIN
                            Str (Zone, TempString);
                            InfoString := InfoString + TempString + ' ';
                            END;
                        END;

                IF Command = 'NAME' THEN
                    IF FoundCall AND (Data.Name <> '') THEN
                        InfoString := InfoString + Data.Name + ' ';

                IF Command = 'QTH' THEN
                    IF FoundCall AND (Data.QTH <> '') THEN
                        InfoString := InfoString + Data.QTH + ' ';

                IF Command = 'SECTION' THEN
                    IF FoundCall AND (Data.Section <> '') THEN
                        InfoString := InfoString + Data.Section + ' ';

            { If they entered USER # instead of USER#, make it work - 6.75 }

                IF Command = 'USER' THEN
                    BEGIN
                    Command := RemoveFirstString (CustomString);

                    IF (Command >= '1') AND (Command <= '5') THEN
                        BEGIN
                        CASE Command [1] OF
                            '1': IF FoundCall AND (Data.User1 <> '') THEN
                                     InfoString := InfoString + Data.User1 + ' ';
                            '2': IF FoundCall AND (Data.User2 <> '') THEN
                                     InfoString := InfoString + Data.User1 + ' ';
                            '3': IF FoundCall AND (Data.User3 <> '') THEN
                                     InfoString := InfoString + Data.User1 + ' ';
                            '4': IF FoundCall AND (Data.User4 <> '') THEN
                                     InfoString := InfoString + Data.User1 + ' ';
                            '5': IF FoundCall AND (Data.User5 <> '') THEN
                                     InfoString := InfoString + Data.User1 + ' ';
                            END;
                        END
                    ELSE
                        CustomString := Command + ' ' + CustomString; { put it back }
                    END;

                IF Command = 'USER1' THEN
                    IF FoundCall AND (Data.User1 <> '') THEN
                        InfoString := InfoString + Data.User1 + ' ';

                IF Command = 'USER2' THEN
                    IF FoundCall AND (Data.User2 <> '') THEN
                        InfoString := InfoString + Data.User2 + ' ';

                IF Command = 'USER3' THEN
                    IF FoundCall AND (Data.User3 <> '') THEN
                        InfoString := InfoString + Data.User3 + ' ';

                IF Command = 'USER4' THEN
                    IF FoundCall AND (Data.User4 <> '') THEN
                        InfoString := InfoString + Data.User4 + ' ';

                IF Command = 'USER5' THEN
                    IF FoundCall AND (Data.User5 <> '') THEN
                        InfoString := InfoString + Data.User5 + ' ';

                IF Command = 'GRID' THEN
                    IF FoundCall AND (Data.Grid <> '') THEN
                        BEGIN
                        InfoString := InfoString + Data.Grid + ' ';

                        IF (MyGrid <> '') AND LooksLikeAGrid (Data.Grid) THEN
                            BEGIN
                            SaveSetAndClearActiveWindow (BeamHeadingWindow);
                            Heading := Round (GetBeamHeading (MyGrid, Data.Grid));
                            Write (Data.Grid, ' at ', Heading, DegreeSymbol);
                            RestorePreviousWindow;
                            END;
                        END;

                IF Command = 'FOC' THEN
                    IF FoundCall AND (Data.FOC <> '') THEN
                        InfoString := InfoString + Data.FOC + ' ';

                IF Command = 'CHECK' THEN
                    IF FoundCall AND (Data.Check <> '') THEN
                        InfoString := InfoString + Data.Check + ' ';

                IF Command = 'OLDCALL' THEN
                    IF FoundCall AND (Data.OldCall <> '') THEN
                        InfoString := InfoString + Data.OldCall + ' ';

                IF Command = 'TENTEN' THEN
                    IF FoundCall AND (Data.TenTen <> '') THEN
                        InfoString := InfoString + Data.TenTen + ' ';
                END;
            END;

        END;

    IF InfoString <> '' THEN
        BEGIN
        WHILE Length (InfoString) < 32 DO
            InfoString := ' ' + InfoString + ' ';

        Write (InfoString);
        END;

    RestorePreviousWindow;
    END;




FUNCTION RTTYInverse (InputString: STRING): STRING;

VAR Address: INTEGER;

    BEGIN
    IF InputString = '' THEN
        BEGIN
        RTTYInverse := '';
        Exit;
        END;

    FOR Address := 0 TO Length (InputString) DO
        CASE InputString [Address] OF
            'E': InputString [Address] := '3';
            'A': InputString [Address] := '-';
            'S': InputString [Address] := ' ';
            'I': InputString [Address] := '8';
            'U': InputString [Address] := '7';
            'D': InputString [Address] := '$';
            'R': InputString [Address] := '4';
            'J': InputString [Address] := '''';
            'N': InputString [Address] := ',';
            'F': InputString [Address] := '!';
            'C': InputString [Address] := ':';
            'K': InputString [Address] := '(';
            'T': InputString [Address] := '5';
            'Z': InputString [Address] := '"';
            'L': InputString [Address] := ')';
            'W': InputString [Address] := '2';
            'H': InputString [Address] := '#';
            'Y': InputString [Address] := '6';
            'P': InputString [Address] := '0';
            'Q': InputString [Address] := '1';
            'O': InputString [Address] := '9';
            'B': InputString [Address] := '?';
            'G': InputString [Address] := '@';
            'M': InputString [Address] := '.';
            'X': InputString [Address] := '/';
            'V': InputString [Address] := ';';

            '3': InputString [Address] := 'E';
            '-': InputString [Address] := 'A';
            '': InputString [Address] := 'S';
            '8': InputString [Address] := 'I';
            '7': InputString [Address] := 'U';
            '$': InputString [Address] := 'D';
            '4': InputString [Address] := 'R';
            '''': InputString [Address] := 'J';
            ',': InputString [Address] := 'N';
            '!': InputString [Address] := 'F';
            ':': InputString [Address] := 'C';
            '(': InputString [Address] := 'K';
            '5': InputString [Address] := 'T';
            '"': InputString [Address] := 'Z';
{KS
            '(': InputString [Address] := 'L';
}
            '2': InputString [Address] := 'W';
            '#': InputString [Address] := 'H';
            '6': InputString [Address] := 'Y';
            '0': InputString [Address] := 'P';
            '1': InputString [Address] := 'Q';
            '9': InputString [Address] := 'O';
            '?': InputString [Address] := 'B';
            '@': InputString [Address] := 'G';
            '.': InputString [Address] := 'M';
            '/': InputString [Address] := 'X';
            ';': InputString [Address] := 'V';
            END;

    RTTYInverse := InputString;
    END;




PROCEDURE CheckRTTY;

{ Checks for any new received characters and puts them up in the display }

VAR TempString: STRING;
    RTTYByte: BYTE;
    RTTYChar: CHAR;

    BEGIN
    IF RTTYReceiveCharBuffer.IsEmpty THEN Exit;  { No new characters }

    { Suck characters from RTTYReceiveBuffer into TempString. }

    TempString := '';

    WHILE RTTYReceiveCharBuffer.GetNextByte (RTTYByte) DO
        BEGIN
        RTTYChar := Chr (RTTYByte);

        TempString := TempString + RTTYChar;

        IF Length (TempString) = 255 THEN Break;      { The rest can wait }
        END;

    SaveAndSetActiveWindow (RTTYWindow);
    GoToXY (RTTYX, RTTYY);
    Write (TempString);
    RTTYX := WhereX;
    RTTYY := WhereY;
    RestorePreviousWindow;

    TempString := RTTYInverse (TempString);

    SaveAndSetActiveWindow (RTTYInverseWindow);
    GoToXY (RTTYInverseX, RTTYInverseY);
    Write (TempString);
    RTTYInverseX := WhereX;
    RTTYInverseY := WhereY;
    RestorePreviousWindow;
    END;



PROCEDURE UpdateK1EAStationInfo (Field: K1EAStationInfoFieldType;
                                 StationIDChar: CHAR;
                                 MessageString: Str20);

VAR Freq: LONGINT;
    xResult: INTEGER;
    Band: BandType;
    Mode: ModeType;

    BEGIN
    GetRidOfPostcedingSpaces (MessageString);
    Val (MessageString, Freq, xResult);

    IF xResult <> 0 THEN
        BEGIN
        SendMorse ('ooops');
        Exit;
        END;

    Band := NoBand;

    CalculateBandMode (Freq, Band, Mode);

    IF Band = NoBand THEN Exit;

    IF Field = Pass THEN
        K1EAMultiInfo [Band].PassFreq := Freq;

    IF Field = Run THEN
        K1EAMultiInfo [Band].RunFreq := Freq;
    END;



PROCEDURE WindInit;

VAR Band: BandType;
    Mode: ModeType;
    Minute: INTEGER;

    BEGIN
    Write ('Disposing of multi message arrays.  Let N6TR know if program hangs now.');

    FOR Band := Band160 TO ALL DO
        FOR Mode := CW TO Both DO
            BEGIN
            IF MultiStatus [Band, Mode] <> nil THEN
                Dispose (MultiStatus [Band, Mode]);
            MultiStatus [Band, Mode] := nil;
            END;

    {KK1L: 6.65 Expanded array to cover all cases to keep BM from going whacko when tuning out of band}
    FOR Band := Band160 TO NoBand DO
        FOR Mode := CW TO FM DO
            BandMapFirstEntryList [Band, Mode] := nil;

    BandMapBand := Band160;
    BandMapMode := CW;

    FOR Minute := 1 TO 60 DO
        BEGIN
        RateMinuteArray [Minute].QSOs := 0;
        RateMinuteArray [Minute].Points := 0;
        END;

    GoToXY (1, WhereY);
    ClrEol;

    AlarmSet := False;

    BandChangesThisHour    := 0;
    BandMapBlinkingCall    := '';
    BandMapCursorFrequency := 0;
    BandMapEntryInCallWindow := False;
    BandMapFileVersion     := 'C'; {KK1L: 6.68 Should have changed this at 6.64. Never checked anyway!}
                                   {KK1L: 6.70 Changed to 'C' since now saving BandMapDecayTime}

    FOR Band := Band160 TO Band10 DO
        FOR Continent := NorthAmerica TO UnknownContinent DO
            ContinentQSOCount [Band, Continent] := 0;

    CalledFromCQMode        := TRUE; {KK1L: 6.73}
    CallLastTimeIWasHere    := ''; {KK1L: 6.73 Used in LOGSUBS2 to trigger band map info update}
    CommandUseInactiveRadio := FALSE; {KK1L: 6.73 Global var to support vectoring commands to inactive radio}

    DisplayBandMapEnable   := TRUE; {KK1L: 6.73}
    DisplayedFrequency     := 0;
    DXMultLimit            := 32766; {KK1L: 6.65 Added to check mult limit for NEQP}

    EscapeFromEditBandMap  := False; {KK1L: 6.65}

    FoundCursor            := False; {KK1L: 6.65}
    FirstDisplayableBandMapFrequency  := 0; {KK1L: 6.64}
    FirstDisplayedBandMapFrequency := 0; {KK1L: 6.64}

    FrequencyDisplayed     := False;
    FreqPollRate           := 250; {KK1L: 6.71a default Frequency Poll Rate in milliseconds}{KK1L: 6.73 was 100}

    GridSquareListShown    := False;

    InEditBandMap          := False;

    JustLoadingBandMapWithNoRadio := False; {KK1L: 6.68}

    LastBand               := NoBand;
    LastCQFrequency        := 0;
    LastCQMode             := CW;

    LastDisplayableBandMapFrequency := 0; {KK1L: 6.64}
    LastDisplayedBandMapFrequency := 0; {KK1L: 6.64}
    LastDisplayedBand      := NoBand;
    LastDisplayedDate      := '';
    LastDisplayedFMMode    := False;
    LastDisplayedFreq[RadioOne]      := 0;
    LastDisplayedFreq[RadioTwo]     := 0; {KK1L: 6.73}
    LastDisplayedHour      := '';
    LastDisplayedMode      := NoMode;
    LastDisplayedTime      := '';

    LastEditedBandMapEntry := 0;

    LastHeadingShown       := 0;

    LastRadioOneBand       := NoBand;
    LastRadioOneMode       := NoMode;

    LastRadioTwoBand       := NoBand;
    LastRadioTwoMode       := NoMode;

    LastSecond100          := 0; {KK1L: 6.71a}

    MarkTime (TenMinuteTime.Time);
    MinutesSinceLastBMUpdate := 0; {KK1L: 6.65}

    MouseEnable            := False;

    NeedToSynchBandMap     := True;  {KK1L: 6.69}
    NoMultMarineMobile     := False; {KK1L: 6.68 Added for WRTC 2002 as flag to not count /MM or /AM as mults or countries}
    NumberMinutesProgramRunning := 0;
    NumberReminderRecords       := 0;
    NumberSavedWindows          := 0;
    NumberTotalScoreMessages    := 0;

    OkayToPutUpBandMapCall      := True;
    OnDeckCall                  := '';

    PacketReturnCount           := 2;
    PacketSpotComment           := ''; {KK1L: 6.68 Need to change when added to LOGCFG.DAT read.}
    PreviousRadioOneFreq        := 0; {KK1L: 6.71b To fix AutoSAPModeEnable}
    PreviousRadioTwoFreq        := 0; {KK1L: 6.71b To fix AutoSAPModeEnable}

    RadioOnTheMove[RadioOne]    := False;
    RadioOnTheMove[RadioTwo]    := False;

    Radio1InquireCount          := 0;
    Radio2InquireCount          := 0;

    Rate                        := 0;
    ReminderPostedCount         := 0;

    RTTYX := 1;
    RTTYY := 1;

    RTTYInverseX := 1;
    RTTYInverseY := 1;

    SearchAndPounceMode         := False;
    SpecialCommand              := NoSpecialCommand;
    SuperDupeSheet              := False;

    TenMinuteTime.Band          := NoBand;
    TenMinuteTime.Mode          := NoMode;

    WideFreqDisplay             := FALSE; {KK1L: 6.73 Never changed anywhere...yet.}

    FOR Band := Band160 TO Band10 DO
        TimeSpentByBand [Band] := 0;

    TRFree := False;

    VisibleDupeSheetChanged     := True;
    WakeUpCount                 := 0;
    END;


    BEGIN
    WindInit;
    END.
