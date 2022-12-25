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

UNIT LogStuff;

{ This unit has a bunch of subroutines that are generic for the logging
  program.  They are stuck here because they can stand alone, or need
  very little VAR interfacing.  This is so they will be out of the way
  and be common to all programs.

  Here are some paragraphs to remind myself how a QSO gets logged - written
  in 2022 as I tried to remember how this beast works.

  When a RETURN is pressed when there is something in the exchange window,
  the function ParametersOkay will be called (from WindowEditor in logsubs2.pas).
  ParametersOkay resisdes in TR.PAS of all places!  It looks at the ExchangeString
  along with the call, band, mode and frequency and works on a record that will
  have all of the QSO information (ContestExchange type).

  The ParametersOkay function will look in the ExchangeString to see if a
  corrected callsign appears there.

  We seemm to have a global variable called ParameterOkayMode which has the
  following settings: Standard, QSLButDoNotLog and QSLAndLog.  This shows
  up as the config command QSL MODE which you can find in the Control-J menu.

  Standard: Needs correc info to QSL and log
  QSLButNoLog: Needs correct info to log, not to QSL
  QSLAndLog: No syntax checking of exchange

  It seems I added this back in 1995 or something.  I have no memory of this.

  ParametersOkay takes setting up the DX location or prefix for the call into
  the ContestExchange.

  It then will rely on ProcessExchange to parse the exchange data from the
  Exchange Window into the ContestExchagne based upon the ActiveExchange.
  You can find that routine in this unit.

  Finally, ParametersOkay will call a procedure to set ther proper QSOPoints.
  This is done with CalculateQSOPoints which is here in this unit and uses
  either the ActiveQSOPointMethod or the more generic QSOPoints globals.

  If the ParametersOkay function is happy (returnning TRUE), then the
  procedure LogContact in logsubs2 gets called.

  LogContact will check to see if this is a dupe, determine if there are
  any multiplier flags to set and generate the log string.

  LogContact will call the MakeLogString function is found here in this unit.
  It mostly relies on the Exchangeinformation flags to determine what fields
  to look at in the ContestExchange and pull data out of.  If you are
  not happy with what you see going into the editable window - then
  this is where you need to go.

  Next, LogContact will call PushLogStringIntoEditableLogAndLogPopedQSO
  which pretty much describes what it done.

  A new QSO will get stuffed into the bottom of the editable log window.
  The .TMP text file is a copy of this window.  When a QSO pops off the
  top, it will essentially move from the .TMP file to the .DAT file.

  HOWEVER, it is actually more complicated than that.

  PushLogStringintoEditableLogAndLogPopedQSO can be found in LOGSUBS2.

  It will send the QSO off to the network if you are sending QSOs immediately.

  It will then call the Procedure PutContactIntoLogFile which will look at
  the log string and take care of formating the page for the people who
  still like to print out their log pages.

  It passes the log entry to PutLogEntryIntoSheet in logedit.pas which will
  see if any multiplier flags need to be set without any regard to what is
  in the editable window.  It will add the QSO to the dupe and multiplier
  sheets.

  FINALLY - the QSO gets added to the .DAT file using WriteLogEntry which is
  here in this unit.

}

{$O+}
{$V-}

INTERFACE

USES Dos, Printer, Tree, Country9, ZoneCont, LogSCP, trCrt, LogWind, LogCW,
     LogDupe, LogGrid, LogHelp, LogK1EA, LogDVP, LogDom, SlowTree, K1EANet,
     datetimec,radio;

CONST
    { Control Byte Constants for Multi-Multi communications }

//    MultiMessageBufferSize = 32;
    MultiMessageBufferSize = 256;

    MultiTalkMessage           = 1; { TR6.75 - Supported by K1EANetwork }
    MultiPacketReceivedSpot    = 2; { No longer used.  Uses 4 all the time }
    MultiPacketMessageToSend   = 3;
    MultiPacketReceivedMessage = 4;
    MultiQSOData               = 5; { TR6.75 - Supported by CT Network }
    MultiBandMapMessage        = 6; { TR6.76 - Supported by CT Network }
    MultiTimeMessage           = 7; { TR6.75 - Reception only from CT Network }
    MultiInformationMessage    = 8;
    MultiConfigurationMessage  = 9; { Not supported on CT Network }

    DefaultTimeToLive = 30;
    LookForDupes = True;
    DoNotLookForDupes = NOT LookForDupes;


    ProcessedMultiMessageBufferLength = 256;

TYPE
     SSExchangeType = RECORD
         Number: STRING [4];
         Prec:   Char;
         Check:  STRING [2];
         Section: Str10;
         END;

    RememberRecord = RECORD
        Frequency: LONGINT;
        Band: BandType;
        Mode: ModeType;
        END;

    RememberRadioType = ARRAY [RadioOne..RadioTwo] OF RememberRecord;

    HelloRecPtr = ^HelloRec;

    HelloRec = RECORD
        ID:         STRING [6];
        Greeting:   STRING [6];
        NextRecord: HelloRecPtr;
        END;

    EntryArray = ARRAY [1..10] OF Str20;

    QuickQSLKeyType = (NoQuickQSLKey,
                       QuickKey1,
                       QuickKey2);




    ParameterOkayModeType = (NoParameterOkayMode,
                             Standard,
                             QSLButDoNotLog,
                             QSLAndLog);

    EntryName = (Unknown,
                 Number,
                 Precedence,
                 Call,
                 Check,
                 Section,
                 NumberPrecedence,
                 CheckSection,
                 NumberPrecedenceCheckSection);

    KeyHistoryRecord = RECORD
        Key:  CHAR;
        Time: TimeRecord;
        END;

    DirectionType = (DirectionUp, DirectionDown);

    TwoRadioAction = (CallPutUp,
                      SpaceBarPressed,
                      F1Pressed,
                      F2Pressed,
                      FootSwitchWasPressed,
                      ReturnPressed,
                      EscapePressed,
                      ContactDone);

    DualingCQStates = (NoDualingCQs,
                       DualGettingExchange,
                       DualSendingCQ,
                       DualSendingExchange,
                       DualSendingQSL,
                       SendingDupeMessage,
                       WaitingForCallsignInput);

    DupeCheckSoundType = (DupeCheckNoSound,
                          DupeCheckBeepIfDupe,
                          DupeCheckGratsIfMult);

    ProcessedMultiMessageRecord = RECORD
        Source: BYTE;
        Serial: BYTE;
        Check:  INTEGER;
        END;

    ProcessedMultiMessageBufferType = ARRAY [0..ProcessedMultiMessageBufferLength - 1] OF
        ProcessedMultiMessageRecord;

    MultiMessageMemoryRecord = RECORD
        Message:    STRING [100];
        TimeMark:   TimeRecord;
        RetryCount: BYTE;
        QSL:        BOOLEAN;
        Warnings:   BYTE;
        END;

    MultiMessageListArrayType = ARRAY [0..MultiMessageBufferSize - 1] OF MultiMessageMemoryRecord;
    MultiMessageListPointer   = ^MultiMessageListArrayType;



VAR

    AddedNoteString:        STRING [100];
    AllCWMessagesChainable: BOOLEAN;
    AltDBand:               BandType;
    AltDMode:               ModeType;
    AlwaysCallBlindCQ:      BOOLEAN;
    AskIfContestOver:       BOOLEAN;
    AutoCallTerminate:      BOOLEAN;
    AutoDisplayDupeQSO:     BOOLEAN;
    AutoQSLCount:           BYTE;
    AutoQSLInterval:        BYTE;
    AutoQSONumberDecrement: BOOLEAN;
    AutoQSYRequestEnable:   BOOLEAN;
    AutoSAPEnable:          BOOLEAN;

    BandMapCallWindowEnable: BOOLEAN;
    BandMapInfoCall:         CallString;
    BeepEvery10QSOs:         BOOLEAN;
    BeSilent:                BOOLEAN;

    CallAlreadySent:        BOOLEAN;
    CallWindowString:       Str80;
    CallsignICameBackTo:    CallString;
    CheckLogFileSize:       BOOLEAN;
    ColumnDupeSheetEnable:  BOOLEAN;
    ComputerID:             CHAR;
    ControlBreakStatus:     BOOLEAN;
    CountryInformationFile: Str40;
    CQMenu:                 Str80;

    Debug:                   BOOLEAN;
    DEEnable:                BOOLEAN;
    DefaultRST:              Str10;
    DoingColors:             BOOLEAN;
    DoingRescore:            BOOLEAN; {KK1L: 6.71}
    DualingCQState:          DualingCQStates;
    DupeCheckSound:          DupeCheckSoundType;
    DupeInfoCall:            CallString;
    DupeInfoCallPrompt:      CallString; {KK1L: 6.73}
    DupeSheetFileEnable:     BOOLEAN;
    DVKPlaying:              BOOLEAN;
    DVKStartTime:            TimeRecord;

    EnableHeadphoneSwitching:   BOOLEAN;
    EscapeDeletedCallEntry:     Str20;
    EscapeDeletedExchangeEntry: Str40;
    EscapeExitsSearchAndPounce: BOOLEAN;
    ExchangeHasBeenSent:        BOOLEAN;
    ExchangeErrorMessage:       Str80;
    ExchangeMemoryFileEnable:   BOOLEAN;
    ExchangeWindowString:       Str80;

    FakePacket:                 BOOLEAN;
    FakeBandMap:                BOOLEAN;
    FirstHelloRecord:           HelloRecPtr;
    FirstMultiMessage:          INTEGER;
    FloppyFileSaveFrequency:    INTEGER;
    FloppyFileSaveName:         Str40;
    FootSwitchPressedBefore:    BOOLEAN;
    ForcedEntry:                BOOLEAN; {KK1L: 6.70 switch used in JCTRL2 to add comments to LOGCFG}

    GridMapCenter: Str20;

    InactiveRigCallingCQ:     BOOLEAN;
    InitialExchangePutUp:     BOOLEAN;
    InitialExchangeOverwrite: BOOLEAN; {KK1L: 6.70}
    InsertMode:               BOOLEAN;
    IntercomFileEnable:       BOOLEAN;
    IntercomFileOpen:         BOOLEAN;
    IntercomFileWrite:        TEXT;

    KeyHistory:                KeyHistoryRecord;
    KeyPadCWMemories:          BOOLEAN;

    LastDeletedLogEntry:       Str160;
    LastDisplayedBreakTime:    INTEGER;
    LastHelloRecord:           HelloRecPtr;
    LastMultiMessage:          INTEGER;
    LastMultiInfoMessageSum:   ARRAY [BandType, ModeType] OF BYTE;
    LastQSOLogged:             ContestExchange;

    LeaveCursorInCallWindow:   BOOLEAN;
    LogBadQSOString:           Str80;
    LogFileRead:               TEXT;
    LogFrequencyEnable:        BOOLEAN;
    LogRSSent:                 Str10;
    LogRSTSent:                Str10;
    LogWithSingleEnter:        BOOLEAN;
    LookForRSTSent:            BOOLEAN;
    LookingForCQExchange:      BOOLEAN;

    MessageEnable:           BOOLEAN;
    ModemPortBaudRate:       LONGINT;
    MultiInfoMessageTimeout: TimeRecord;
    MultiMultsOnly:          BOOLEAN;
    MultiplierFileEnable:    BOOLEAN;
    MultiMessageBuffer:      ARRAY [1..5] OF Str80;
    MultiRememberBuffer:     MultiMessageListPointer;
    MultiRetryTime:          BYTE;
    MultiSerialNumber:       INTEGER;
    MultiUpdateMultDisplay:  BOOLEAN;
    MultReportMinimumBands:  BYTE;
    MyIOTA:                  Str20;

    NameCallsignPutUp:     CallString;
    NameFlagEnable:        BOOLEAN;
    NoLog:                 BOOLEAN;

    OldCWTone: INTEGER;

    PacketAutoCR:            BOOLEAN;
    PacketFile:              BOOLEAN;
    PacketFileRead:          FILE;
    PacketSpotDisable:       BOOLEAN;
    PacketSpotEditEnable:    BOOLEAN;
    ParameterOkayMode:       ParameterOkayModeType;
    PartialCallFileEnable:   BOOLEAN;
    PartialCallMultsEnable:  BOOLEAN;
    PossibleCallAcceptKey:   CHAR;
    PossibleCallEnable:      BOOLEAN;
    PossibleCallLeftKey:     CHAR;
    PossibleCallRightKey:    CHAR;
    PreviousQSOReceivedData: ContestExchange;
    PrinterEnabled:          BOOLEAN;
    ProcessedMultiMessages:  ProcessedMultiMessageBufferType;

    ProcessedMultiMessagesStart: INTEGER;
    ProcessedMultiMessagesEnd:   INTEGER;

    QSONumberForThisQSO: INTEGER;   { Used for classic mode QSOs }

    QSONumberByBand: BOOLEAN;
    QSXEnable:       BOOLEAN;

    QuickQSL:     QuickQSLKeyType;   { to indicate a quick QSL is desired }
    QuickQSLKey1: CHAR;
    QuickQSLKey2: CHAR;

    Radio1BaudRate:      LONGINT;
    Radio2BaudRate:      LONGINT;
    Radio1IDCharacter:   CHAR;
    Radio2IDCharacter:   CHAR;
    Radio1CwReverse:     BOOLEAN;
    Radio2CwReverse:     BOOLEAN;
    RadioSetFreq:        LONGINT;
    RandomCQMode:        BOOLEAN;
    RandomNameEnable:    BOOLEAN;
    ReadInCallsign:      CallString;
    ReadInLogComputerID: CHAR;
    ReadInLog:           BOOLEAN;
    ReadInLogBand:       BandType;
    ReadInLogDateString: Str10;
    ReadInLogExchange:   Str40;
    ReadInLogFileName:   Str40;
    ReadInLogFileOpen:   BOOLEAN;
    ReadInLogFileRead:   TEXT;
    ReadInLogMode:       ModeType;
    ReadInLogRST:        Str10;
    ReadInLogTimeString: Str10;

    ReceivedData:        ContestExchange;
    Remember:            RememberRadioType;
    RememberBand:        BandType;
    RememberMode:        ModeType;
    RemoteTerminalBand:  BandType;
    RemoteTerminalMode:  ModeType;

    SayHiRateCutoff:            INTEGER;
    SendCompleteFourLetterCall: BOOLEAN;
    SendQSOImmediately:         BOOLEAN;
    SeventyThreeMessageSent:    BOOLEAN;
    SCPMinimumLetters:          BYTE;

    SendExchangeKeyWhenCWHasStopped: CHAR;

    Sheet:                      DupeAndMultSheet;
    ShortIntegers:              BOOLEAN;
    ShowSearchAndPounce:        BOOLEAN;
    SingleRadioMode:            BOOLEAN;
    SkipActiveBand:             BOOLEAN;
    SpaceBarDupeCheckEnable:    BOOLEAN;
    SSEx:                       SSExchangeType;
    StartSendingNowKey:         CHAR;
    StationInformationCall:     CallString;
    SwitchRadioKey:             CHAR;

    TailEnding:                BOOLEAN;
    TailEndKey:                CHAR;
    TailEndCallString:         CallString;

    TotalDomesticContacts:     INTEGER;
    TotalDXContacts:           INTEGER;
    TotalIntermediateContacts: INTEGER;

    UpdateRestartFileEnable:  BOOLEAN;

    WaitForStrength: BOOLEAN;

    WindowDupeCheckCall: Str20;



    PROCEDURE AgeReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);
    PROCEDURE AgeReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);

    PROCEDURE BandChange (VAR ActiveBand: BandType; Direction: DirectionType);

    PROCEDURE CalculateQSOPoints (VAR RXData: ContestExchange);
    PROCEDURE ChapterReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);
    PROCEDURE ChapterReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);

    PROCEDURE CheckForLostMultiMessages;
    PROCEDURE CreateAndSendCQMultiInfoMessage;
    PROCEDURE CreateAndSendSAPMultiInfoMessage;

    PROCEDURE DisplayGridSquareStatus (Call: CallString);
    PROCEDURE DisplayMultiMessageBuffer;

    FUNCTION  DVKRecentlyStarted (MaxElaspedSec100: LONGINT): BOOLEAN;
    PROCEDURE DVKStamp;

    FUNCTION  GetCorrectedCallFromExchangeString (VAR ExchangeString: Str80): Str80;
    FUNCTION  GetMultiPortCommand: STRING;
    FUNCTION  GetSentRSTFromExchangeString (VAR ExchangeString: Str40): Str20;

    PROCEDURE IncrementQTCCount (Call: CallString);

    FUNCTION  K3IsStillTalking: BOOLEAN;
    FUNCTION  KeyRecentlyPressed (Key: CHAR; MaxElaspedSec100: LONGINT): BOOLEAN;
    PROCEDURE KeyStamp (Key: CHAR);

    PROCEDURE LoadSpecialHelloFile;
    FUNCTION  LogFileLooksOkay: BOOLEAN;
    PROCEDURE LogStringToRXData (LogString: Str80; VAR RXData: ContestExchange);
    FUNCTION  LooksLikeACallSign (Call: Str40): BOOLEAN;

    FUNCTION  MakeLogString (RXData: ContestExchange): Str80;
    FUNCTION  MarineOrAirMobileStation (Call: CallString): Boolean; {KK1L: 6.68 Used in WRTC 2002}

    PROCEDURE NextPage;

    PROCEDURE NewBandMapEntry (Call: CallString;
                               Frequency: LONGINT;
                               QSXFrequency: LONGINT;
                               Mode: ModeType;
                               Dupe: BOOLEAN;
                               Mult: BOOLEAN;
                               MinutesLeft: INTEGER;
                               SendToMulti: BOOLEAN);


    FUNCTION  NumberQTCsThisStation (Call: CallString): INTEGER;
    PROCEDURE NameReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);
    PROCEDURE NameReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);

    PROCEDURE PassStationToCTNetwork;

    PROCEDURE PrintLogHeader;

    FUNCTION  ProcessExchange (ExchangeString: Str80; VAR RData: ContestExchange): BOOLEAN;
    PROCEDURE ProcessPartialCallAndInitialExchange (RXData: ContestExchange);
    FUNCTION  ProperSalutation (Call: CallString): Str80;

    PROCEDURE PushMultiMessageBuffer (Message: Str80);

    PROCEDURE RememberRadioFrequency (Radio: RadioType);
    PROCEDURE RestoreRadioFrequency (Radio: RadioType);

    PROCEDURE ReviewBackCopyFiles;
    PROCEDURE RotorControl (Heading: INTEGER);

    PROCEDURE SaveLogFileToFloppy;
    PROCEDURE SayHello (Call: CallString);
    PROCEDURE SayName  (Call: CallString);
    PROCEDURE SendMultiInfoMessage (Band: BandType; Mode: ModeType; Message: Str80);
    PROCEDURE SendMultiCommand (Source: BYTE; Destination: BYTE; ControlByte: BYTE; Message: STRING);

    PROCEDURE SendSalutation (Call: CallString);
    PROCEDURE ShowName (Call: CallString);

    PROCEDURE ShowPreviousDupeQSOs (Call: CallString;
                                    Band: BandType;
                                    Mode: ModeType);
    PROCEDURE SlowDown;
    PROCEDURE SpeedUp;

    PROCEDURE StuffInit;

    { Header and Stamp routines - used when generating a log string }

    PROCEDURE BandModeDateTimeNumberCallNameSentStamp (Exchange: ContestExchange; VAR LogString: Str80);
    PROCEDURE BandModeDateTimeNumberCallNameSentHeader (VAR LogString: Str80; VAR Underline: Str80);
    PROCEDURE CheckReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);
    PROCEDURE CheckReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);
    PROCEDURE ClassReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);
    PROCEDURE ClassReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

    PROCEDURE KidsReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);
    PROCEDURE KidsReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

    PROCEDURE MultiplierHeader (VAR LogString: Str80; VAR Underline: Str80);
    PROCEDURE MultiplierStamp (Exchange: ContestExchange; VAR LogString: Str80);
    PROCEDURE PowerReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);
    PROCEDURE PowerReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);
    PROCEDURE PrecedenceReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);
    PROCEDURE PrecedenceReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);
    PROCEDURE QSONumberReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);
    PROCEDURE QSONumberReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);
    PROCEDURE QSOPointHeader (VAR LogString: Str80; VAR Underline: Str80);
    PROCEDURE QSOPointStamp (Exchange: ContestExchange; VAR LogString: Str80);
    PROCEDURE QTHReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);
    PROCEDURE QTHReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

    PROCEDURE PostalCodeReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);
    PROCEDURE PostalCodeReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

    PROCEDURE RandomCharsSentAndReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);
    PROCEDURE RandomCharsSentAndReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

    PROCEDURE RSTReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);
    PROCEDURE RSTReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);
    PROCEDURE RSTSentHeader (VAR LogString: Str80; VAR Underline: Str80);
    PROCEDURE RSTSentStamp (Exchange: ContestExchange; VAR LogString: Str80);

    PROCEDURE TenTenNumReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);
    PROCEDURE TenTenNumReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

    PROCEDURE ZoneReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);
    PROCEDURE ZoneReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

    PROCEDURE WriteLogEntry (Entry: Str80);

IMPLEMENTATION
uses keycode,beep;




FUNCTION K3IsStillTalking: BOOLEAN;

    BEGIN
    IF ActiveRadio = RadioOne THEN
        BEGIN
        IF (Radio1Type = K3) OR (Radio1Type = K4) THEN
            K3IsStillTalking := Rig1.K3IsSTillTalking
        ELSE
            K3IsStillTalking := False;

        Exit;
        END;

    IF ActiveRadio = RadioTwo THEN
        BEGIN
        IF (Radio2Type = K3) OR (Radio2Type = K4) THEN
            K3IsStillTalking := Rig2.K3IsSTillTalking
        ELSE
            K3IsStillTalking := False;

        Exit;
        END;

    K3ISStillTalking := False;
    END;



PROCEDURE BandChange (VAR ActiveBand: BandType;
                          Direction: DirectionType);

    BEGIN
    IF Direction = DirectionUp THEN
      BEGIN
      CASE ActiveBand OF
        Band160: ActiveBand := Band80;
        Band80:  ActiveBand := Band40;

        Band40:  IF WARCBandsEnabled THEN
                     ActiveBand := Band30
                 ELSE
                     ActiveBand := Band20;

        Band30:  ActiveBand := Band20;

        Band20:  IF WARCBandsEnabled THEN
                     ActiveBand := Band17
                 ELSE
                     ActiveBand := Band15;

        Band17:  ActiveBand := Band15;

        Band15:  IF WARCBandsEnabled THEN
                     ActiveBand := Band12
                 ELSE
                     ActiveBand := Band10;

        Band12:  ActiveBand := Band10;

        Band10:  IF VHFBandsEnabled THEN
                     ActiveBand := Band6
                 ELSE
                     ActiveBand := Band160;

        Band6:     ActiveBand := Band2;
        Band2:     ActiveBand := Band222;
        Band222:   ActiveBand := Band432;
        Band432:   ActiveBand := Band902;
        Band902:   ActiveBand := Band1296;
        Band1296:  ActiveBand := Band2304;
        Band2304:  ActiveBand := Band3456;
        Band3456:  ActiveBand := Band5760;
        Band5760:  ActiveBand := Band10G;
        Band10G:   ActiveBand := Band24G;
        Band24G:   ActiveBand := BandLight;
        BandLight: IF HFBandEnable THEN
                       ActiveBand := Band160
                   ELSE
                       ActiveBand := Band6;

        ELSE      ActiveBand := Band160;


                                   BandChange (ActiveBand, Direction);
        END;

      IF SkipActiveBand AND ((BandMemory [RadioOne] = ActiveBand) OR
                             (BandMemory [RadioTwo] = ActiveBand)) THEN
                                 BandChange (ActiveBand, Direction);
      END

    ELSE  {KK1l: 6.72 Note Direction = DirectionDown}
      BEGIN
      CASE ActiveBand OF
        Band160: IF VHFBandsEnabled THEN
                     ActiveBand := BandLight
                 ELSE
                     ActiveBand := Band10;

        Band80:  ActiveBand := Band160;
        Band40:  ActiveBand := Band80;

        Band30:  ActiveBand := Band40;

        Band20:  IF WARCBandsEnabled THEN
                     ActiveBand := Band30
                 ELSE
                     ActiveBand := Band40;

        Band17:  ActiveBand := Band20;


        Band15:  IF WARCBandsEnabled THEN
                     ActiveBand := Band17
                 ELSE
                     ActiveBand := Band20;

        Band12:  ActiveBand := Band15;

        Band10:  IF WARCBandsEnabled THEN
                     ActiveBand := Band12
                 ELSE
                     ActiveBand := Band15;

        Band6:   IF HFBandEnable THEN
                     ActiveBand := Band10
                 ELSE
                     ActiveBand := BandLight;

        Band2:   ActiveBand := Band6;

        Band222:   ActiveBand := Band2;
        Band432:   ActiveBand := Band222;
        Band902:   ActiveBand := Band432;
        Band1296:  ActiveBand := Band902;
        Band2304:  ActiveBand := Band1296;
        Band3456:  ActiveBand := Band2304;
        Band5760:  ActiveBand := Band3456;
        Band10G:   ActiveBand := Band5760;
        Band24G:   ActiveBand := Band10G;
        BandLight: ActiveBand := Band24G;

        ELSE      ActiveBand := Band160;
        END;

      IF SkipActiveBand AND ((BandMemory [RadioOne] = ActiveBand) OR
                             (BandMemory [RadioTwo] = ActiveBand)) THEN
                                 BandChange (ActiveBand, Direction);
      END;


    IF ((ActiveRadio = RadioOne) AND (Radio1Type <> NoInterfacedRadio)) OR
       ((ActiveRadio = RadioTwo) AND (Radio2Type <> NoInterfacedRadio)) THEN
           DisplayBandMode (ActiveBand, ActiveMode, True)
       ELSE
           DisplayBandMode (ActiveBand, ActiveMode, False);

    END;




FUNCTION NumberQTCsThisStation (Call: CallString): INTEGER;

VAR Station: INTEGER;
    CompressedCall: EightBytes;

    BEGIN
    NumberQTCsThisStation := 0;
    IF NumberQTCStations = 0 THEN Exit;
    BigCompressFormat (Call, CompressedCall);

    FOR Station := 0 TO NumberQTCStations - 1 DO
        IF BigCompressedCallsAreEqual (QTCDataArray ^[Station].Call, CompressedCall) THEN
            BEGIN
            NumberQTCsThisStation := QTCDataArray ^[Station].NumberQTCs;
            Exit;
            END;
    END;



PROCEDURE IncrementQTCCount (Call: CallString);

VAR Station: INTEGER;
    CompressedCall: EightBytes;

    BEGIN
    BigCompressFormat (Call, CompressedCall);

    IF NumberQTCStations = 0 THEN
        BEGIN
        QTCDataArray ^[NumberQTCStations].Call := CompressedCall;
        QTCDataArray ^[NumberQTCStations].NumberQTCs := 1;
        Inc (NumberQTCStations);
        Exit;
        END;

    FOR Station := 0 TO NumberQTCStations - 1 DO
        IF BigCompressedCallsAreEqual (QTCDataArray ^[Station].Call, CompressedCall) THEN
            BEGIN
            Inc (QTCDataArray ^[Station].NumberQTCs);
            Exit;
            END;

    QTCDataArray ^[NumberQTCStations].Call       := CompressedCall;
    QTCDataArray ^[NumberQTCStations].NumberQTCs := 1;
    Inc (NumberQTCStations);
    END;



PROCEDURE BandModeDateTimeNumberCallNameSentStamp (Exchange: ContestExchange; VAR LogString: Str80);

VAR TimeString, QSONumberString: Str20;

    BEGIN
    IF FMMode THEN
        LogString := BandString [Exchange.Band] + ModeString [FM]
    ELSE
        LogString := BandString [Exchange.Band] + ModeString [Exchange.Mode];

    WHILE Length (LogString) < LogEntryDayAddress - 1 DO
        LogString := LogString + ' ';

    IF ReadInLog THEN
        LogString := LogString + ReadInLogDateString
    ELSE
        IF Exchange.Date <> '' THEN
            LogString := LogString + Exchange.Date
        ELSE
            LogString := LogString + GetDateString;

    WHILE Length (LogString) < LogEntryHourAddress - 1 DO
        LogString := LogString + ' ';

    IF ReadInLog THEN
        LogString := LogString + ReadInLogTimeString
    ELSE
        IF Exchange.Time >= 0 THEN
            BEGIN
            Str (Exchange.Time, TimeString);
            WHILE Length (TimeString) < 4 DO TimeString := '0' + TimeString;
            Insert (':', TimeString, 3);
            LogString := LogString + TimeString;
            END
        ELSE
            LogString := LogString + GetTimeString;

    WHILE Length (LogString) < LogEntryQSONumberAddress - 1 DO
        LogString := LogString + ' ';

    IF LogFrequencyEnable THEN
        BEGIN
        CASE Exchange.Band OF
            Band160: Exchange.Frequency := Exchange.Frequency -   1000000;
            Band80:  Exchange.Frequency := Exchange.Frequency -   3000000;
            Band40:  Exchange.Frequency := Exchange.Frequency -   7000000;
            Band30:  Exchange.Frequency := Exchange.Frequency -  10000000;
            Band20:  Exchange.Frequency := Exchange.Frequency -  14000000;
            Band17:  Exchange.Frequency := Exchange.Frequency -  18000000;
            Band15:  Exchange.Frequency := Exchange.Frequency -  21000000;
            Band12:  Exchange.Frequency := Exchange.Frequency -  24000000;
            Band10:  Exchange.Frequency := Exchange.Frequency -  28000000;
            Band6:   Exchange.Frequency := Exchange.Frequency -  54000000;
            Band2:   Exchange.Frequency := Exchange.Frequency - 144000000;
            END;

        IF Exchange.Frequency < 0 THEN Exchange.Frequency := 0;

        Exchange.Frequency := Exchange.Frequency DIV 1000;

        Str (Exchange.Frequency, QSONumberString);

        WHILE Length (QSONumberString) < 3 DO
            QSONumberString := '0' + QSONumberString;

        QSONumberString := '.' + QSONumberString;
        END
    ELSE
        Str (Exchange.NumberSent, QSONumberString);

    WHILE Length (QSONumberString) < 4 DO
        QSONumberString := ' ' + QSONumberString;

    LogString := LogString + QSONumberString;

    IF ReadInLog THEN
        LogString := LogString + ReadInLogComputerID
    ELSE
        IF ComputerID <> Chr (0) THEN
            LogString := LogString + ComputerID
        ELSE
            IF (ActiveRadio = RadioOne) AND (Radio1IDCharacter <> Chr (0)) THEN
                LogString := LogString + Radio1IDCharacter
            ELSE
                IF (ActiveRadio = RadioTwo) AND (Radio2IDCharacter <> Chr (0)) THEN
                    LogString := LogString + Radio2IDCharacter;

    WHILE Length (LogString) < LogEntryCallAddress - 1 DO
        LogString := LogString + ' ';

    LogString := LogString + Exchange.Callsign;

    WHILE Length (LogString) < LogEntryNameSentAddress - 1 DO
        LogString := LogString + ' ';

    IF NameFlagEnable THEN
        IF Exchange.NameSent THEN
            LogString := LogString + '*';

    WHILE Length (LogString) < LogEntryExchangeAddress - 1 DO
        LogString := LogString + ' ';
    END;


PROCEDURE BandModeDateTimeNumberCallNameSentHeader (VAR LogString: Str80; VAR Underline: Str80);

    BEGIN
    LogString := ' Band    Date    Time  QSO#  Call worked';
    Underline := ' ----    ----    ----  ----  -----------';

    WHILE Length (LogString) < LogEntryExchangeAddress - 1 DO
        BEGIN
        LogString := LogString + ' ';
        Underline := Underline + ' ';
        END;
    END;



PROCEDURE ClassReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

VAR ClassString: Str20;

    BEGIN
    ClassString := Exchange.Classs;
    WHILE Length (ClassString) < 7 DO
        ClassString := ClassString + ' ';

    LogString := LogString + '  ' + ClassString;
    END;


PROCEDURE ClassReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);

    BEGIN
    LogString := LogString + 'Class  ';
    UnderLine := Underline + '-----  ';
    END;

PROCEDURE KidsReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

    BEGIN
    LogString := LogString + Exchange.Kids;
    END;


PROCEDURE KidsReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);

    BEGIN
    LogString := LogString + 'Exchange';
    UnderLine := Underline + '--------  ';
    END;

PROCEDURE QSONumberReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

VAR QSONumberString: Str80;

    BEGIN
    QSONumberString := '';
    IF Exchange.NumberReceived >= 0 THEN
        Str (Exchange.NumberReceived, QSONumberString);

    {KK1L: 6.70 Sometimes there is just not a pretty way to do it!!}
    {           Keeps the Power and QSO numbers lined up in log.}
    IF (ActiveExchange = RSTQTHNameAndFistsNumberOrPowerExchange) AND
       (Length (QSONumberString) = 0) THEN Exit;

    IF (ActiveExchange = RSTQTHNameAndFistsNumberOrPowerExchange) THEN {KK1L: 6.70}
        WHILE Length (QSONumberString) < 5 DO
            QSONumberString := ' ' + QSONumberString
    ELSE
        WHILE Length (QSONumberString) < 4 DO
            QSONumberString := ' ' + QSONumberString;

    QSONumberString := QSONumberString + '  ';
    QSONumberString [0] := Chr (6);
    LogString := LogString + QSONumberString;
    END;


PROCEDURE QSONumberReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);

    BEGIN
    LogString := LogString + 'Rcvd  ';
    UnderLine := Underline + '----  ';
    END;

PROCEDURE RSTSentStamp (Exchange: ContestExchange; VAR LogString: Str80);

VAR RSTString: Str80;

    BEGIN
    IF ReadInLog THEN
        RSTString := ' ' + ReadInLogRST + '     '
    ELSE
        RSTString := ' ' + Exchange.RSTSent + '     ';

    RSTString [0] := Chr (5);
    LogString := LogString + RSTString;
    END;


PROCEDURE RSTSentHeader (VAR LogString: Str80; VAR Underline: Str80);

    BEGIN
    LogString := LogString + 'Sent ';
    UnderLine := Underline + '---- ';
    END;



PROCEDURE RSTReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

VAR RSTString: Str80;

    BEGIN
    RSTString := ' ' + Exchange.RSTReceived + '     ';
    RSTString [0] := Chr (5);
    LogString := LogString + RSTString;
    END;

PROCEDURE RandomCharsSentAndReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);

    BEGIN
    LogString := LogString + 'Sent   Rcvd   ';
    UnderLine := Underline + '----   ----   ';
    END;


PROCEDURE RandomCharsSentAndReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

VAR CharsString: Str20;

    BEGIN
    CharsString := Exchange.RandomCharsSent;

    WHILE Length (CharsString) < 7 DO
        CharsString := CharsString + ' ';

    LogString := LogString + CharsString;

    CharsString := Exchange.RandomCharsReceived;

    WHILE Length (CharsString) < 7 DO
        CharsString := CharsString + ' ';

    LogString := LogString + CharsString;
    END;



PROCEDURE PostalCodeReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);

    BEGIN
    LogString := LogString + 'Post Code ';
    UnderLine := Underline + '--------- ';
    END;


PROCEDURE PostalCodeReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

VAR CharsString: Str20;

    BEGIN
    CharsString := Exchange.PostalCode;

    WHILE Length (CharsString) < 10 DO
        CharsString := CharsString + ' ';

    LogString := LogString + CharsString;
    END;


PROCEDURE RSTReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);

    BEGIN
    LogString := LogString + 'Rcvd ';
    UnderLine := Underline + '---- ';
    END;


PROCEDURE CheckReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

VAR CheckString: Str80;

    BEGIN
    CheckString := Exchange.Check + '     ';
    CheckString [0] := Chr (3);
    LogString := LogString + CheckString;
    END;

PROCEDURE CheckReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);

    BEGIN
    LogString := LogString + 'Ck ';
    UnderLine := Underline + '-- ';
    END;

PROCEDURE PrecedenceReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

VAR PrecedenceString: Str80;

    BEGIN
    PrecedenceString := Exchange.Precedence + '   ';
    PrecedenceString [0] := Chr (2);
    LogString := LogString + PrecedenceString;
    END;


PROCEDURE PrecedenceReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);

    BEGIN
    LogString := LogString + 'P ';
    UnderLine := Underline + '- ';
    END;




PROCEDURE QTHReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

{ QTH must always be last before mults.  We will now always log what was
  actually typed in by the operator if it is a domesitc QTH. }
{KK1L: 6.70 Except for FISTS Sprint! For this the QTH comes after RST}

VAR QTHString, PrefectureString: Str80;

    BEGIN
    IF ActiveExchange = CWTExchange THEN
        BEGIN
        QTHString := Exchange.QTHString;

        WHILE Length (QTHString) < 8 DO
            QTHString := QTHString + ' ';

        LogString := LogString + QTHString;
        Exit;
        END;

    IF (ActiveExchange = RSTQTHExchange) OR
       (ActiveExchange = QSONumberDomesticOrDXQTHExchange) THEN
        BEGIN
        IF LiteralDomesticQTH THEN
            QTHString := Exchange.QTHString + '                      '
        ELSE
            QTHString := Exchange.DomesticQTH + '                      ';

        QTHString [0] := Chr (22);
        LogString := LogString + QTHString;
        Exit;
        END;

    IF ActiveExchange = RSTAndContinentExchange THEN
        BEGIN
        QTHString := Exchange.QTHString;
        WHILE Length (QTHString) < 22 DO QTHString := QTHString + ' ';
        LogString := LogString + QTHString;
        Exit;
        END;

    IF (ActiveExchange = RSTAllJAPrefectureAndPrecedenceExchange) OR
       (ActiveExchange = RSTPrefectureExchange) THEN
        BEGIN
        PrefectureString := Exchange.DomesticQTH;
        Delete (PrefectureString, 1, 1);

        CASE Length (PrefectureString) OF
            0: PrefectureString := '    ';
            1: PrefectureString := '  ' + PrefectureString + ' ';
            2: PrefectureString := ' ' + PrefectureString + ' ';
            3: PrefectureString := PrefectureString + ' ';
            ELSE
                IF Length (PrefectureString) > 4 THEN
                    PrefectureString [0] := Chr (4);
            END;

        LogString := LogString + PrefectureString + ' ';
        Exit;
        END;

    IF ActiveExchange = RSTNameAndQTHExchange THEN
        BEGIN
        QTHString := Exchange.QTHString + '               ';
        QTHString [0] := Chr (10);
        LogString := LogString + QTHString;
        Exit;
        END;

    IF ActiveExchange = RSTQSONumberAndPossibleDomesticQTHExchange THEN
        BEGIN
        QTHString := '';

        IF Exchange.DomesticQTH <> '' THEN
            BEGIN
            IF LiteralDomesticQTH THEN
                QTHString := Exchange.QTHString
            ELSE
                QTHString := Exchange.DomesticQTH;
            END;

        LogString := LogString + QTHString;

        IF Length (LogString) > LogEntryMultAddress - 2 THEN
            LogString [0] := Chr (LogEntryMultAddress - 2);
        Exit;
        END;

    IF ActiveExchange = RSTQTHNameAndFistsNumberOrPowerExchange THEN {KK1L: 6.70}
        BEGIN
        QTHString := Exchange.QTHString;
        WHILE Length (QTHString) < 2 DO QTHString := QTHString + ' ';
        LogString := LogString + QTHString + '  ';
        Exit;
        END;

    { All other exchanges }

    IF Exchange.DomesticQTH <> '' THEN
        BEGIN
        IF LiteralDomesticQTH THEN
            QTHString := Exchange.QTHString
        ELSE
            QTHString := Exchange.DomesticQTH;
        END
    ELSE
        BEGIN
        GetDXQTH (Exchange);   { 6.30 }
        QTHString := Exchange.DXQTH; {KK1L: 6.72 NOTE this is where DXQTH makes it to the log}
        END;

    LogString := LogString + QTHString;

    IF Length (LogString) > LogEntryMultAddress - 2 THEN
        LogString [0] := Chr (LogEntryMultAddress - 2);

    END;


PROCEDURE QTHReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);

    BEGIN
    IF ActiveExchange = RSTQTHExchange THEN
        BEGIN
        LogString := LogString + 'Qth Received          ';
        UnderLine := UnderLine + '------------          ';
        Exit;
        END;

    IF (ActiveExchange = RSTAllJAPrefectureAndPrecedenceExchange) OR
       (ActiveExchange = RSTPrefectureExchange) THEN
        BEGIN
        LogString := LogString + 'Pref ';
        UnderLine := Underline + '---- ';
        Exit;
        END;

    IF ActiveExchange = RSTNameAndQTHExchange THEN
        BEGIN
        LogString := LogString + 'Qth       ';
        UnderLine := Underline + '---       ';
        END
    ELSE
        BEGIN
        LogString := LogString + ' Qth  ';
        Underline := Underline + '----- ';
        END;
    END;



PROCEDURE MultiplierStamp (Exchange: ContestExchange; VAR LogString: Str80);

{ The first instruction of this procedure deserves some explaining.  Since
  this routine gets used for two things: making log strings to put into
  the editable window and fixing multipier strings.  The first instruction
  allows it to be used for both but not add spaces if we are fixing a
  multiplier string.                                                   }

VAR MultString, ZoneString: Str80;

    BEGIN
    IF (ActiveDomesticMult = NoDomesticMults) AND (ActiveDXMult = NoDXMults) AND
       (ActivePrefixMult = NoPrefixMults) AND (ActiveZoneMult = NoZoneMults) THEN
           Exit;

    IF ActiveExchange = RSTQTHNameAndFistsNumberOrPowerExchange THEN Exit;

    IF Length (LogString) > 20 THEN
        WHILE Length (LogString) < LogEntryMultAddress - 1 DO
            LogString := LogString + ' ';

    MultString := '';

    IF Exchange.DomesticMult THEN
        BEGIN
        MultString := Exchange.DomMultQTH;
        END;

    IF Exchange.DXMult THEN
        IF MultString = '' THEN
            MultString := Exchange.DXQTH
        ELSE
            MultString := MultString + ' ' + Exchange.DXQTH;

    IF Exchange.PrefixMult THEN
        IF MultString = '' THEN
            MultString := Exchange.Prefix
        ELSE
            MultString := MultString + ' ' + Exchange.Prefix;

    IF Exchange.ZoneMult THEN
        BEGIN
        ZoneString := Exchange.Zone;

        WHILE Length (ZoneString) < 2 DO
            ZoneString := '0' + ZoneString;

        IF MultString = '' THEN
            MultString := ZoneString
        ELSE
            MultString := MultString + ' ' + ZoneString;
        END;

    LogString := LogString + MultString;
    END;



PROCEDURE MultiplierHeader (VAR LogString: Str80; VAR Underline: Str80);

    BEGIN
    IF (ActiveDomesticMult = NoDomesticMults) AND (ActiveDXMult = NoDXMults) AND
       (ActivePrefixMult = NoPrefixMults) AND (ActiveZoneMult = NoZoneMults) THEN
           Exit;

    IF ActiveExchange = RSTQTHNameAndFistsNumberOrPowerExchange THEN Exit;

    WHILE Length (LogString) < LogEntryMultAddress - 1 DO
        LogString := LogString + ' ';

    WHILE Length (Underline) < LogEntryMultAddress - 1 DO
        Underline := Underline + ' ';

    LogString := LogString + 'Mults   ';
    Underline := Underline + '-----   ';
    END;


PROCEDURE PowerReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

VAR TempString: Str80;

    BEGIN
    TempString := Exchange.Power;

    {KK1L: 6.70 Sometimes there is just not a pretty way to do it!!}
    {           Keeps the Power and QSO numbers lined up in log.}
    IF (ActiveExchange = RSTQTHNameAndFistsNumberOrPowerExchange) AND
       (Length (TempString) = 0) THEN Exit;

    IF (ActiveExchange = RSTQTHNameAndFistsNumberOrPowerExchange) THEN {KK1L: 6.70}
        WHILE Length (TempString) < 5 DO
            TempString := ' ' + TempString
    ELSE
        WHILE Length (TempString) < 4 DO
            TempString := ' ' + TempString;

    LogString := LogString + TempString + '  ';
    END;


PROCEDURE PowerReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);

    BEGIN
    LogString := LogString + 'Power ';
    Underline := Underline + '----- ';
    END;

PROCEDURE ZoneReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

VAR TempString: Str80;

    BEGIN
    IF Exchange.Zone <> '' THEN
        BEGIN
        TempString := Exchange.Zone;

        WHILE Length (TempString) < 2 DO
            TempString := '0' + TempString;

        TempString := ' ' + TempString + ' ';
        LogString := LogString + TempString + ' ';
        END
    ELSE
        LogString := LogString + '     ';
    END;

PROCEDURE TenTenNumReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);

    BEGIN
    LogString := LogString + ' 1010# ';
    Underline := Underline + ' ----- ';
    END;

PROCEDURE TenTenNumReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

VAR TenTenNumberString: Str20;

    BEGIN
    IF Exchange.TenTenNum > 0 THEN
        BEGIN
        Str (Exchange.TenTenNum, TenTenNumberString);

        WHILE Length (TenTenNumberString) < 6 DO
            TenTenNumberString := ' ' + TenTenNumberString;
        END
    ELSE
        TenTenNumberString := '      ';

    LogString := LogString + TenTenNumberString + ' ';
    END;



PROCEDURE ZoneReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);

    BEGIN
    LogString := LogString + 'Zone ';
    Underline := Underline + '---- ';
    END;



PROCEDURE AgeReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

VAR TempString: Str80;

    BEGIN
    TempString := Exchange.Age;

    WHILE Length (TempString) < 2 DO
        TempString := '0' + TempString;

    TempString := ' ' + TempString + ' ';

    LogString := LogString + TempString;
    END;

PROCEDURE AgeReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);

    BEGIN
    LogString := LogString + 'Age ';
    Underline := Underline + '--- ';
    END;

PROCEDURE NameReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

VAR TempString: Str80;

    BEGIN
    TempString := Exchange.Name;

    {KK1L: 6.70 Changed spacing slightly to line up with output}
    IF ActiveExchange = RSTQTHNameAndFistsNumberOrPowerExchange THEN
        WHILE Length (TempString) < 10 DO
            TempString := TempString + ' '
    ELSE
        WHILE Length (TempString) < 12 DO
            TempString := TempString + ' ';

    LogString := LogString + TempString;
    END;

PROCEDURE NameReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);

    BEGIN
    LogString := LogString + 'Name        ';
    Underline := Underline + '----        ';
    END;


PROCEDURE ChapterReceivedHeader (VAR LogString: Str80; VAR Underline: Str80);

    BEGIN
    LogString := LogString + 'Chp ';
    Underline := Underline + '--- ';
    END;


PROCEDURE ChapterReceivedStamp (Exchange: ContestExchange; VAR LogString: Str80);

VAR TempString: Str20;

    BEGIN
    TempString := Copy (Exchange.Chapter, 1, 4);

    TempString := TempString + ' ';

    WHILE Length (TempString) < 4 DO TempString := ' ' + TempString;

    LogString := LogString + TempString;
    END;



PROCEDURE QSOPointStamp (Exchange: ContestExchange; VAR LogString: Str80);

VAR QSOPointString: Str80;

    BEGIN
    WHILE Length (LogString) < LogEntryPointsAddress - 1 DO
        LogString := LogString + ' ';

    Str (Exchange.QSOPoints, QSOPointString);

    IF Length (QSOPointString) = 1 THEN
        QSOPointString := ' ' + QSOPointString;

    LogString := LogString + QSOPointString;

    IF ShowSearchAndPounce THEN
        IF Exchange.SearchAndPounce THEN
            LogString := LogString + '$';

    END;



PROCEDURE QSOPointHeader (VAR LogString: Str80; VAR Underline: Str80);

    BEGIN
    WHILE Length (LogString) < LogEntryPointsAddress - 2 DO
        LogString := LogString + ' ';

    WHILE Length (Underline) < LogEntryPointsAddress - 3 DO
        Underline := Underline + ' ';

    LogString := LogString + 'Pts';
    Underline := Underline + '---';
    END;



FUNCTION ProperSalutation (Call: CallString): Str80;

VAR TempQTHData: QTHRecord;
    Hours, ZeroZuluOffset: WORD;
    CurrentHelloRecord: HelloRecPtr;
    ID: CallString;

    BEGIN
    LocateCall (Call, TempQTHData, True);

    IF FirstHelloRecord <> nil THEN
        BEGIN
        ID := UpperCase (CountryTable.GetCountryID (TempQTHData.Country));
        CurrentHelloRecord := FirstHelloRecord;

        REPEAT
            IF UpperCase (CurrentHelloRecord^.ID) = ID THEN
                BEGIN
                ProperSalutation := CurrentHelloRecord^.Greeting;
                Exit;
                END;

            CurrentHelloRecord := CurrentHelloRecord^.NextRecord;
        UNTIL CurrentHelloRecord = Nil;
        END;

    IF CountryTable.ZoneMode = CQZoneMode THEN
        BEGIN
        CASE TempQTHData.Zone OF
          -1: ZeroZuluOffset := 18;  { These are US calls not in a proper zone }
           1: ZeroZuluOffset := 14;
           2: ZeroZuluOffset := 19;
           3: ZeroZuluOffset := 16;
           4: ZeroZuluOffset := 18;
           5: ZeroZuluOffset := 19;
           6: ZeroZuluOffset := 17;
           7: ZeroZuluOffset := 18;
           8: ZeroZuluOffset := 19;
           9: ZeroZuluOffset := 18;
          10: ZeroZuluOffset := 19;
          11: ZeroZuluOffset := 21;
          12: ZeroZuluOffset := 19;
          13: ZeroZuluOffset := 20;
          14: ZeroZuluOffset :=  0;
          15: ZeroZuluOffset :=  1;
          16: ZeroZuluOffset :=  2;
          17: ZeroZuluOffset :=  4;
          18: ZeroZuluOffset :=  6;
          19: ZeroZuluOffset :=  9;
          20: ZeroZuluOffset :=  2;
          21: ZeroZuluOffset :=  4;
          22: ZeroZuluOffset :=  5;
          23: ZeroZuluOffset :=  7;
          24: ZeroZuluOffset :=  8;
          25: ZeroZuluOffset :=  9;
          26: ZeroZuluOffset :=  7;
          27: ZeroZuluOffset :=  8;
          28: ZeroZuluOffset :=  7;
          29: ZeroZuluOffset :=  8;
          30: ZeroZuluOffset := 10;
          31: ZeroZuluOffset := 14;
          32: ZeroZuluOffset := 12;
          33: ZeroZuluOffset :=  0;
          34: ZeroZuluOffset :=  2;
          35: ZeroZuluOffset :=  0;
          36: ZeroZuluOffset :=  0;
          37: ZeroZuluOffset :=  2;
          38: ZeroZuluOffset :=  1;
          39: ZeroZuluOffset :=  3;
          40: BEGIN
              ProperSalutation := 'TU';
              Exit;
              END;
          END;

        END
    ELSE
        BEGIN
        CASE TempQTHData.Zone OF
           1: ZeroZuluOffset := 14;
           2: ZeroZuluOffset := 16;
           3: ZeroZuluOffset := 17;
           4: ZeroZuluOffset := 18;
           5: ZeroZuluOffset := 19;
           6: ZeroZuluOffset := 16;
           7: ZeroZuluOffset := 18;
           8: ZeroZuluOffset := 19;
           9: ZeroZuluOffset := 19;
          10: ZeroZuluOffset := 17;

          11: ZeroZuluOffset := 19;
          12: ZeroZuluOffset := 19;
          13: ZeroZuluOffset := 21;
          14: ZeroZuluOffset := 19;
          15: ZeroZuluOffset := 21;
          16: ZeroZuluOffset := 20;
          17: ZeroZuluOffset := 23;
          18: ZeroZuluOffset :=  1;
          19: ZeroZuluOffset :=  2;
          20: ZeroZuluOffset :=  4;

          21: ZeroZuluOffset :=  4;
          22: ZeroZuluOffset :=  5;
          23: ZeroZuluOffset :=  6;
          24: ZeroZuluOffset :=  7;
          25: ZeroZuluOffset :=  8;
          26: ZeroZuluOffset :=  9;
          27: ZeroZuluOffset :=  1;
          28: ZeroZuluOffset :=  2;
          29: ZeroZuluOffset :=  3;
          30: ZeroZuluOffset :=  4;

          31: ZeroZuluOffset :=  5;
          32: ZeroZuluOffset :=  6;
          33: ZeroZuluOffset :=  7;
          34: ZeroZuluOffset :=  8;
          35: ZeroZuluOffset :=  9;
          36: ZeroZuluOffset :=  1;
          37: ZeroZuluOffset :=  1;
          38: ZeroZuluOffset :=  3;
          39: ZeroZuluOffset :=  3;
          40: ZeroZuluOffset :=  4;

          41: ZeroZuluOffset :=  5;
          42: ZeroZuluOffset :=  5;
          43: ZeroZuluOffset :=  6;
          44: ZeroZuluOffset :=  8;
          45: ZeroZuluOffset :=  9;
          46: ZeroZuluOffset :=  0;
          47: ZeroZuluOffset :=  1;
          48: ZeroZuluOffset :=  2;
          49: ZeroZuluOffset :=  6;
          50: ZeroZuluOffset :=  8;

          51: ZeroZuluOffset :=  9;
          52: ZeroZuluOffset :=  0;
          53: ZeroZuluOffset :=  3;
          54: ZeroZuluOffset :=  7;
          55: ZeroZuluOffset :=  9;
          56: ZeroZuluOffset := 12;
          57: ZeroZuluOffset :=  2;
          58: ZeroZuluOffset :=  8;
          59: ZeroZuluOffset := 10;
          60: ZeroZuluOffset := 11;

          61: ZeroZuluOffset := 13;
          62: ZeroZuluOffset := 13;
          63: ZeroZuluOffset := 13;
          64: ZeroZuluOffset := 10;
          65: ZeroZuluOffset := 10;
          66: ZeroZuluOffset := 22;

          ELSE
              BEGIN
              ProperSalutation := 'TU';
              Exit;
              END;
          END;
        END;

    Hours := GetIntegerTime DIV 100;
    Hours := (Hours + ZeroZuluOffset) MOD 24;

    IF (Hours >= 2)  AND (Hours <= 11) THEN ProperSalutation := 'GM';
    IF (Hours >= 12) AND (Hours <= 17) THEN ProperSalutation := 'GA';
    IF (Hours >= 18)  OR (Hours <=  1) THEN ProperSalutation := 'GE';
    END;




PROCEDURE LoadSpecialHelloFile;

VAR FileRead: TEXT;
    FileName, FileString: Str80;
    Greeting, ID: Str20;

    BEGIN
    IF FirstHelloRecord <> nil THEN Exit;

    FileName := FindDirectory ('HELLO.DAT') + DirectorySeparator + 'HELLO.DAT';

    IF OpenFileForRead (FileRead, FileName) THEN
        BEGIN
        WHILE NOT Eof (FileRead) DO
            BEGIN
            ReadLn (FileRead, FileString);
            GetRidOfPrecedingSpaces (FileString);
            ID := RemoveFirstString (FileString);
            GetRidOfPrecedingSpaces (FileString);
            GetRidOfPostcedingSpaces (FileString);
            Greeting := FileString;

            IF FirstHelloRecord = nil THEN
                BEGIN
                FirstHelloRecord := New (HelloRecPtr);
                FirstHelloRecord^.ID := ID;
                FirstHelloRecord^.Greeting := Greeting;
                FirstHelloRecord^.NextRecord := Nil;
                LastHelloRecord := FirstHelloRecord;
                END
            ELSE
                BEGIN
                LastHelloRecord^.NextRecord := New (HelloRecPtr);
                LastHelloRecord := LastHelloRecord^.NextRecord;
                LastHelloRecord^.ID := ID;
                LastHelloRecord^.Greeting := Greeting;
                LastHelloRecord^.NextRecord := Nil;
                END;
            END;

        Close (FileRead);
        END;
    END;



PROCEDURE SendSalutation (Call: CallString);

    BEGIN
    IF Call = '' THEN Exit;
    AddStringToBuffer (ProperSalutation (Call), CWTone);
    END;


PROCEDURE SayHello (Call: CallString);

VAR Salutation, Name: Str20;
    RandomNumber: INTEGER;

    BEGIN
    Call := RootCall (Call);
    Name := UpperCase (CD.GetName (Call));

    IF (Name <> '') AND (Name <> 'CLUB') THEN
        BEGIN
        Salutation := ProperSalutation (Call);
        IF Salutation = 'TU' THEN Salutation := 'HI';
        RandomNumber := Random (10);
        IF RandomNumber >= 7 THEN Salutation := 'HI';
        AddStringToBuffer (Salutation + ' ' + Name + ' ', CWTone);
        ReceivedData.NameSent := True;
        END
    ELSE
        ReceivedData.NameSent := False;
    END;


PROCEDURE ShowName (Call: CallString);

VAR Name: Str20;

    BEGIN
    Call := RootCall (Call);

    NameCallsignPutUp := Call;
    Name := CD.GetName (Call);

    IF Name <> '' THEN
        BEGIN
        DisplayNameSent (Name);
        ReceivedData.NameSent := True;
        END
    ELSE
        BEGIN
        RemoveWindow (NameSentWindow);
        ReceivedData.NameSent := False;
        END;
    END;



PROCEDURE SayName (Call: CallString);

VAR Name: Str20;

    BEGIN
    Call := RootCall (Call);
    Name := UpperCase (CD.GetName (Call));

    IF (Name <> '') AND (Name <> 'CLUB') THEN
        BEGIN
        AddStringToBuffer (Name + ' ', CWTone);
        ReceivedData.NameSent := True;
        END
    ELSE
        RemoveWindow (NameSentWindow);
    END;


FUNCTION DVKRecentlyStarted (MaxElaspedSec100: LONGINT): BOOLEAN;

    BEGIN
    IF NOT DVKPlaying THEN
        BEGIN
        DVKRecentlyStarted := False;
        Exit;
        END;

    DVKRecentlyStarted := MaxElaspedSec100 >= ElaspedSec100 (DVKStartTime);
    END;

PROCEDURE DVKStamp;

    BEGIN
    DVKPlaying := True;

    GetTime (DVKStartTime.Hour,
             DVKStartTime.Minute,
             DVKStartTime.Second,
             DVKStartTime.Sec100);
    END;


FUNCTION KeyRecentlyPressed (Key: CHAR; MaxElaspedSec100: LONGINT): BOOLEAN;

    BEGIN
    KeyRecentlyPressed := False;
    IF Key <> KeyHistory.Key THEN Exit;
    KeyRecentlyPressed := MaxElaspedSec100 >= ElaspedSec100 (KeyHistory.Time);
    END;


PROCEDURE KeyStamp (Key: CHAR);

    BEGIN
    KeyHistory.Key := Key;

    WITH KeyHistory DO
        BEGIN
        GetTime (Time.Hour, Time.Minute, Time.Second, Time.Sec100);
        END;
    END;



PROCEDURE ParseExchange (Exchange: Str80; VAR FirstString, SecondString, ThirdString: Str20);

{ Simply parses the exchange into three strings }

    BEGIN
    FirstString  := '';
    SecondString := '';
    ThirdString  := '';

    IF Length (Exchange) = 0 THEN Exit;

    IF StringHas (Exchange, ' ') THEN
        BEGIN
        FirstString := PrecedingString (Exchange, ' ');
        Delete (Exchange, 1, Length (FirstString) + 1);
        GetRidOfPrecedingSpaces (Exchange);

        IF StringHas (Exchange, ' ') THEN
            BEGIN
            SecondString := PrecedingString (Exchange, ' ');
            Delete (Exchange, 1, Length (SecondString));
            GetRidOfPrecedingSpaces (Exchange);
            ThirdString := Exchange;
            END
         ELSE
             SecondString := Exchange;
        END
    ELSE
        FirstString := Exchange;
    END;



FUNCTION ProcessKidsExchange (ExchangeString: Str80; VAR RData: ContestExchange): BOOLEAN;

    BEGIN
    RData.Kids := ExchangeString;
    ProcessKidsExchange := True;
    END;



FUNCTION ProcessClassAndDomesticOrDXQTHExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ If the call is a domestic call, there must be a space with the Class
  before the space and the Domestic QTH after it.  If it is a not a domestic
  call the exchange must just be the Class.  In both cases, the Class must
  be at least two characters in length. }
{KK1L: 6.68 This needed to change for the new FD rules which allow all of Region 2 to participate.}
{           Now DX can send a class. Let's still force a check for domesticity and require both a }
{           class and section. For others if the second string of the exchange is blank I will }
{           "let it slide" and assume the first string is class (likely "DX"). }

VAR TempString, TempString2: Str20;

    BEGIN
    ProcessClassAndDomesticOrDXQTHExchange := False;

    IF DomesticCountryCall (RXData.Callsign) THEN
        BEGIN
        ParseExchange (Exchange, RXData.Classs, RXData.QTHString, TempString);
        IF NOT FoundDomesticQTH (RXData) THEN Exit;
        END
    ELSE
        BEGIN
        {ParseExchange (Exchange, RXData.Class, RXData.QTHString, TempString);} {pre 6.68 statement}
        ParseExchange (Exchange, RXData.Classs, TempString, TempString2);
        IF TempString <> '' THEN RXData.QTHString := TempString;
        END;

    ProcessClassAndDomesticOrDXQTHExchange := (Length (RXData.Classs) = 2) OR
                                              (Length (RXData.Classs) = 3);
    END;



FUNCTION ProcessRSTAndContinentExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;



{ Any digits found at the start of the exchange will be used to determine
  the RS(T).  If the first character is not a digit, the default RS(T)
  will be used and the whole exchange will be used as the QTH. }

VAR TempString: Str80;
    QTHFound: BOOLEAN;
    ContPrefix: Str20;

    BEGIN
    QTHFound := False;
    RXData.QTH.Continent := UnknownContinent;

    ProcessRSTAndContinentExchange := False;
    GetRidOfPrecedingSpaces (Exchange);
    GetRidOfPostcedingSpaces (Exchange);

    IF NOT StringHas (Exchange, ' ') AND NOT StringIsAllNumbers (Exchange) THEN
        BEGIN
        RXData.RSTReceived := DefaultRST;

        ContPrefix := UpperCase (Copy (Exchange, 1, 2));

        IF ContPrefix = 'AF' THEN RXData.QTH.Continent := Africa;
        IF ContPrefix = 'AS' THEN RXData.QTH.Continent := Asia;
        IF ContPrefix = 'OC' THEN RXData.QTH.Continent := Oceania;
        IF ContPrefix = 'EU' THEN RXData.QTH.Continent := Europe;

        IF (ContPrefix = 'SO') OR (ContPrefix = 'SA') THEN RXData.QTH.Continent := SouthAmerica;
        IF (ContPrefix = 'NO') OR (ContPrefix = 'NA') THEN RXDAta.QTH.Continent := NorthAmerica;

        RXData.QTHString := Exchange;

        ProcessRSTAndContinentExchange := RXData.QTH.Continent <> UnknownContinent;
        Exit;
        END;

    WHILE Exchange <> '' DO
        BEGIN
        TempString := RemoveLastString (Exchange);

        IF StringIsAllNumbers (TempString) AND (RXData.RSTReceived = '') THEN
            BEGIN
            IF Length (TempString) = 1 THEN
                BEGIN
                CASE ActiveMode OF
                    CW:    RXData.RSTReceived := '5' + TempString + '9';
                    Phone: RXData.RSTReceived := '5' + TempString;
                    END;
                END
            ELSE
                RXData.RSTReceived := TempString;
            END
        ELSE
            IF NOT QTHFound THEN
                BEGIN
                ContPrefix := UpperCase (Copy (Exchange, 1, 2));

                IF ContPrefix = 'AF' THEN RXData.QTH.Continent := Africa;
                IF ContPrefix = 'AS' THEN RXData.QTH.Continent := Asia;
                IF ContPrefix = 'OC' THEN RXData.QTH.Continent := Oceania;
                IF ContPrefix = 'EU' THEN RXData.QTH.Continent := Europe;

                IF (ContPrefix = 'SO') OR (ContPrefix = 'SA') THEN RXData.QTH.Continent := SouthAmerica;
                IF (ContPrefix = 'NO') OR (ContPrefix = 'NA') THEN RXDAta.QTH.Continent := NorthAmerica;

                RXData.QTHString := Exchange;

                IF RXData.QTH.Continent <> UnknownContinent THEN
                    BEGIN
                    QTHFound := True;
                    RXData.QTHString := TempString;
                    END;
                END;
        END;

    IF QTHFound THEN
        BEGIN
        IF RXData.RSTReceived = '' THEN
            RXData.RSTReceived := DefaultRST;
        ProcessRSTAndContinentExchange := True;
        END;
    END;



FUNCTION ProcessCWTExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

VAR TempString: Str80;
    FirstString, ThirdSTring, PossibleNumberString: Str20;
    Index: INTEGER;

    BEGIN
    ProcessCWTExchange := False;
    IF Exchange = '' THEN Exit;
    TempString := Exchange;
    Exchange := '';

    { We use the QSO number field for the CWT Number }

    RXData.NumberReceived := -1;
    RXData.QTHString := '';  { Gets used for QTH or "CWA" }
    RXData.Name := '';

    { We typically have one string that contains some numbers.  Pull that part of the
      exchange string out of exchange string. }

    WHILE TempString <> '' DO
         BEGIN
         FirstString := RemoveFirstString (TempString);

         IF StringHasNumber (FirstString) THEN  { Found the number - need to uncut #s }
             BEGIN
             PossibleNumberString := FirstString;
             FOR Index := 1 TO Length (PossibleNumberString) DO
                 BEGIN
                 IF PossibleNumberString [Index] = 'A' THEN PossibleNumberString [Index] := '1';
                 IF PossibleNumberString [Index] = 'U' THEN PossibleNumberString [Index] := '2';
                 IF PossibleNumberString [Index] = 'E' THEN PossibleNumberString [Index] := '5';
                 IF PossibleNumberString [Index] = 'N' THEN PossibleNumberString [Index] := '9';
                 IF PossibleNumberString [Index] = 'T' THEN PossibleNumberString [Index] := '0';
                 END;

             IF StringIsAllNumbers (PossibleNumberString) THEN
                 Val (PossibleNumberString, RXData.NumberReceived)
             ELSE
                 Exchange := Exchange + FirstString;

             END
         ELSE
             Exchange := Exchange + FirstString + ' ';
         END;

     { We have pulled out the member number (if there was one) Now process the rest of the
      exchange string.   We assume that the name is always before the QTH }

    ParseExchange (Exchange, RXData.Name, RXData.QTHString, ThirdString);
    ProcessCWTExchange := ((RXData.NumberReceived <> -1) OR (RXData.QTHString <> '')) AND (RXData.Name <> '');
    END;



FUNCTION ProcessNameQTHAndPossibleTenTenNumberExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

VAR TempString: Str80;
    ThirdSTring, NumberString: Str20;

    BEGIN
    ProcessNameQTHAndPossibleTenTenNumberExchange := False;

    IF Exchange = '' THEN Exit;

    TempString := Exchange;
    Exchange := '';

    RXData.TenTenNum := -1;

    WHILE TempString <> '' DO
         BEGIN
         NumberString := RemoveFirstString (TempString);

         IF StringIsAllNumbers (NumberString) THEN
             Val (NumberString, RXData.TenTenNum)
         ELSE
             Exchange := Exchange + NumberString + ' ';
         END;

    ParseExchange (Exchange, RXData.Name, RXData.QTHString, ThirdString);

    IF NOT DomesticCountryCall (RXData.Callsign) THEN
        ProcessNameQTHAndPossibleTenTenNumberExchange := True
    ELSE
        ProcessNameQTHAndPossibleTenTenNumberExchange := FoundDomesticQTH (RXData);

    END;



FUNCTION ProcessNameAndDomesticOrDXQTHExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ Two entries with space, name is first. IF DX, then no QTH. }

VAR ThirdString: Str20;

    BEGIN
    ProcessNameAndDomesticOrDXQTHExchange := False;
    IF Exchange = '' THEN Exit;

    ParseExchange (Exchange, RXData.Name, RXData.QTHString, ThirdString);

    IF NOT DomesticCountryCall (RXData.Callsign) THEN
        ProcessNameAndDomesticOrDXQTHExchange := True
    ELSE
        ProcessNameAndDomesticOrDXQTHExchange := FoundDomesticQTH (RXData);

    END;


FUNCTION ProcessQSONumberAndDomesticQTHExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ If the the two entries are separated by a space, they may appear in either
  order.  If only one entry appears, it is assumed to be the Domestic QTH
  and the QSO number will be one.  }

VAR xResult: INTEGER;
    NumberString, Str1, Str2, Str3, Str4: Str20;

    BEGIN
    ProcessQSONumberAndDomesticQTHExchange := False;
    IF Length (Exchange) = 0 THEN Exit;

    Str1 := '';
    Str2 := '';
    Str3 := '';
    Str4 := '';

    RXData.QTHString := '';

    ParseExchange (Exchange, Str1, Str2, Str3);

    IF StringHas (Str3, ' ') THEN
        BEGIN
        Str4 := PostcedingString (Str3, ' ');
        Str3 := PrecedingString (Str3, ' ');
        END;

    { Do the number - it is easy }

    IF StringIsAllNumbers (Str4) THEN
        NumberString := Str4
    ELSE
        IF StringIsAllNumbers (Str3) THEN
            NumberString := Str3
        ELSE
            IF StringIsAllNumbers (Str2) THEN
                NumberString := Str2
            ELSE
                IF StringIsAllNumbers (Str1) THEN
                    NumberString := Str1
                ELSE
                    NumberString := '1';


    { Now the QTH - which isn't so hard either }

    IF (NOT StringIsAllNumbers (Str4)) AND (Str4 <> '') THEN
        RXData.QTHString := Str4
    ELSE
        IF (NOT StringIsAllNumbers (Str3)) AND (Str3 <> '') THEN
            RXData.QTHString := Str3
        ELSE
            IF (NOT StringIsAllNumbers (Str2)) AND (Str2 <> '') THEN
                RXData.QTHString := Str2
            ELSE
                IF (NOT StringIsAllNumbers (Str1)) AND (Str1 <> '') THEN
                    RXData.QTHString := Str1;

    IF RXData.QTHString = '' THEN Exit;

    Val (NumberString, RXData.NumberReceived, xResult);
    IF xResult <> 0 THEN Exit;
    ProcessQSONumberAndDomesticQTHExchange := FoundDomesticQTH (RXData);
    END;



FUNCTION ProcessQSONumberAndNameExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

VAR TempString, NumberString, NameString: Str40;
    xResult: INTEGER;

    BEGIN
    NumberString := '';
    NameString   := '';

    ProcessQSONumberAndNameExchange := False;

    WHILE (Exchange <> '') AND ((NumberString = '') OR (NameString = '')) DO
        BEGIN
        TempString := RemoveLastString (Exchange);

        IF StringIsAllNumbers (TempString) THEN
            NumberString := TempString
        ELSE
            NameString := TempString;

        END;

    IF (NumberString = '') OR (NameString = '') THEN Exit;

    RXData.Name := NameString;

    Val (NumberString, RXData.NumberReceived, xResult);

    ProcessQSONumberAndNameExchange := xResult = 0;
    END;



FUNCTION ProcessRSTAndOrGridSquareExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

VAR TestString: Str20;

    BEGIN
    ProcessRSTAndOrGridSquareExchange := False;
    RXData.DomesticQTH := '';
    RXData.RSTReceived := '';

    IF Length (Exchange) = 0 THEN Exit;

    WHILE Exchange <> '' DO
        BEGIN
        TestString := RemoveLastString (Exchange);

        IF GoodLookingGrid (TestString) THEN
            BEGIN
            IF RXData.DomesticQTH = '' THEN
                BEGIN
                RXData.DomesticQTH := TestString;
                RXData.QTHString := TestString;
                ProcessRSTAndOrGridSquareExchange := True;
                END;
            END
        ELSE
            IF (RXData.RSTReceived = '') AND StringIsAllNumbers (TestString) THEN
                BEGIN
                IF Length (TestString) = 1 THEN
                    BEGIN
                    CASE ActiveMode OF
                        CW:    RXData.RSTReceived := '5' + TestString + '9';
                        Phone: RXData.RSTReceived := '5' + TestString;
                        END;
                    END
                ELSE
                    RXData.RSTReceived := TestString;

                ProcessRSTAndOrGridSquareExchange := True;
                END;
        END;
    END;


FUNCTION ProcessRSTAndGridSquareExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

VAR TestString: Str20;

    BEGIN
    ProcessRSTAndGridSquareExchange := False;
    RXData.DomesticQTH := '';
    RXData.RSTReceived := '';

    IF Length (Exchange) = 0 THEN Exit;

    WHILE Exchange <> '' DO
        BEGIN
        TestString := RemoveLastString (Exchange);

        IF GoodLookingGrid (TestString) THEN
            BEGIN
            IF RXData.QTHString = '' THEN
                RXData.QTHString := TestString;
            END
        ELSE
            IF (RXData.RSTReceived = '') AND StringIsAllNumbers (TestString) THEN
                BEGIN
                IF Length (TestString) = 1 THEN
                    BEGIN
                    CASE ActiveMode OF
                        CW:    RXData.RSTReceived := '5' + TestString + '9';
                        Phone: RXData.RSTReceived := '5' + TestString;
                        END;
                    END
                ELSE
                    RXData.RSTReceived := TestString;

                END;
        END;

    IF (RXData.DomesticQTH <> '') AND (RXData.RSTReceived = '') THEN
        BEGIN
        CASE ActiveMode OF
            Phone: RXData.RSTReceived := '59';
            ELSE   RXData.RSTReceived := '599';
            END;
        END;

    ProcessRSTAndGridSquareExchange := FoundDomesticQTH (RXData);
    END;



FUNCTION ProcessNameAndPossibleGridSquareExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

VAR TestString: Str80;

    BEGIN
    ProcessNameAndPossibleGridSquareExchange := False;
    RXData.DomesticQTH := '';
    RXData.Name := '';

    IF Length (Exchange) = 0 THEN Exit;

    WHILE Exchange <> '' DO
        BEGIN
        TestString := RemoveLastString (Exchange);

        IF GoodLookingGrid (TestString) THEN
            BEGIN
            IF RXData.DomesticQTH = '' THEN
                BEGIN
                RXData.DomesticQTH := TestString;
                RXData.QTHString := TestString;
                END;
            END
        ELSE
            IF RXData.Name = '' THEN RXData.Name := TestString;
        END;

    ProcessNameAndPossibleGridSquareExchange := RXData.Name <> '';
    END;



FUNCTION ProcessQSONumberAndDomesticOrDXQTHExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

VAR xResult: INTEGER;
    FirstString, SecondString, ThirdString: Str20;

    BEGIN
    ProcessQSONumberAndDomesticOrDXQTHExchange := False;
    IF Length (Exchange) = 0 THEN Exit;

    IF NOT DomesticCountryCall (RXData.Callsign) THEN
        BEGIN
        ParseExchange (Exchange, FirstString, SecondString, ThirdString);

        IF StringIsAllNumbers (FirstString) THEN
            BEGIN
            Val (FirstString, RXData.NumberReceived, xResult);
            ProcessQSONumberAndDomesticOrDXQTHExchange := xResult = 0;
            END
        ELSE
            IF StringIsAllNumbers (SecondString) THEN
                BEGIN
                Val (SecondString, RXData.NumberReceived, xResult);
                ProcessQSONumberAndDomesticOrDXQTHExchange := xResult = 0;
                END;

        RXData.QTHString := CountryTable.GetCountryID (RXData.QTH.Country);
        END
    ELSE
        ProcessQSONumberAndDomesticOrDXQTHExchange :=
            ProcessQSONumberAndDomesticQTHExchange (Exchange, RXData);

    END;



PROCEDURE ParseExchangeIntoFields (Exchange: Str80;
                                   VAR Entries: EntryArray;
                                   VAR NumberEntries: INTEGER);

    BEGIN
    NumberEntries := 0;

    WHILE Exchange <> '' DO
        BEGIN
        Inc (NumberEntries);
        Entries [NumberEntries] := RemoveFirstString (Exchange);
        IF NumberEntries = 10 THEN Exit;
        END;
    END;




PROCEDURE ParseFourFields( sExch: Str80; VAR s1, s2, s3, s4: Str20);

TYPE pSTRING = ^STRING;

VAR CharIndex, iDstPtrIndex: INTEGER;
    bInField: BOOLEAN;
    aps: ARRAY [0..3] of pSTRING;
    ps: pSTRING;
    c: CHAR;

    BEGIN

    s1 := '';
    s2 := '';
    s3 := '';
    s4 := '';

    aps [0] := @s1;
    aps [1] := @s2;
    aps [2] := @s3;
    aps [3] := @s4;

    CharIndex := 1;
    iDstPtrIndex := 0;
    bInField := FALSE;

    WHILE (CharIndex <= Length( sExch)) AND (iDstPtrIndex <= 3) DO
        BEGIN
        c := sExch[ CharIndex];
        Inc( CharIndex);
        IF c <> ' ' THEN
            BEGIN
            IF NOT bInField THEN
                BEGIN
                bInField := TRUE;
                ps := aps[ iDstPtrIndex];
                ps^ := ps^ + c;
                END
            ELSE
                ps^ := ps^ + c;
            END
        ELSE
            IF bInField THEN
                BEGIN
                bInFIeld := FALSE;
                Inc( iDstPtrIndex);
                END
        END;

    END;



FUNCTION LooksLikeACallSign (Call: Str40): BOOLEAN;

TYPE GOT = (gotNIL, gotLETTER, gotNUMBER);

VAR CharIndex, nChanges: INTEGER;
    gotWhat: GOT;

    BEGIN
    LooksLikeACallsign := False;

    gotWhat := gotNIL;
    nChanges := 0;

    FOR CharIndex := 1 TO Length (Call) DO
        BEGIN
        IF (Call [CharIndex] >= 'A') AND (Call [CharIndex] <= 'Z') THEN
            BEGIN
            IF gotWhat = gotNUMBER THEN Inc (nChanges);
            gotWhat := gotLETTER;
            END
        ELSE
            IF (Call [CharIndex] >= '0') AND (Call [CharIndex] <= '9') THEN
                BEGIN
                { Calls don't end with numbers unless already have '/' }

                IF CharIndex = Length (Call) THEN Exit;

                { Calls don't start with two numbers }

                IF (gotWhat = gotNumber) AND (CharIndex = 2) THEN Exit;

                IF gotWhat = gotLETTER THEN Inc (nChanges);

                gotWhat := gotNUMBER;
                END
            ELSE
                IF Call [CharIndex] = '/' THEN
                    BEGIN
                    IF ((CharIndex > 3) AND (CharIndex <> Length (Call)) OR
                       ((CharIndex < Length (Call)) AND (CharIndex > 1))) THEN
                           BEGIN
                           LooksLikeACallsign := True;
                           Exit;
                           END;
                    END;

        IF ((CharIndex = 1) OR (CharIndex = Length (Call))) AND (Call [CharIndex] = '/') THEN
            BEGIN
            LooksLikeACallsign := False;
            Exit;
            END;
        END;

    LooksLikeACallsign := (nChanges >= 2) AND (gotWhat = gotLETTER);
    END;



FUNCTION ProcessQSONumberNameChapterAndQTHExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ Used for the QCWA contest.  The Chapter might be 2 or 3 integers, of AL
  for At Large.

  Valid formats:
      QSO# NAME CHAPTER QTH or NAME CHAPTER QTH QSO#

  }

VAR FirstString, SecondString, ThirdString, FourthString: Str20;

    BEGIN
    ProcessQSONumberNameChapterAndQTHExchange := False;

    IF Length (Exchange) < 9 THEN Exit;

    ParseFourFields (Exchange, FirstString, SecondString, ThirdString, FourthString);

    IF StringIsAllNumbers (FirstString) THEN
        BEGIN
        IF NOT (StringIsAllNumbers (ThirdString) OR (UpperCase (ThirdString) = 'AL')) THEN Exit;

        Val (FirstString, RXData.NumberReceived);

        RXData.Name      := SecondString;
        RXData.Chapter   := UpperCase (ThirdString);
        RXData.DomesticQTH := FourthString;
        END

    ELSE { Number at end? }

        BEGIN
        IF NOT StringIsAllNumbers (FourthString) THEN Exit;
        IF NOT (StringIsAllNumbers (SecondString) OR (UpperCase (SecondString) = 'AL')) THEN Exit;

        Val (FourthString, RXData.NumberReceived);

        RXData.Name      := FirstString;
        RXData.Chapter   := UpperCase (SecondString);
        RXData.DomesticQTH := ThirdString;
        END;

    ProcessQSONumberNameChapterAndQTHExchange := True;
    END;



FUNCTION ProcessCheckAndChapterOrQTHExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ Used for the QCWA Golden Anniversay contest.  The Chapter might be 2 or 3
  integers, or AL for At Large with QTH.  The CHECK is two numbers (year).

  Valid formats:
      CHECK CHAPTER or CHECK QTH

  }

VAR FirstString, SecondString, ThirdString, FourthString: Str20;

    BEGIN
    ProcessCheckAndChapterOrQTHExchange := False;

    IF Length (Exchange) < 4 THEN Exit;

    ParseFourFields (Exchange, FirstString, SecondString, ThirdString, FourthString);

    IF StringIsAllNumbers (FirstString) THEN
        BEGIN
        RXData.Check := FirstString;

        IF SecondString <> '' THEN
            BEGIN
            RXData.DomesticQTH := SecondString;
            RXData.QTHString   := SecondString;
            ProcessCheckAndChapterOrQTHExchange := True;
            Exit;
            END;
        END;
    END;



FUNCTION ProcessQSONumberNameAndDomesticOrDXQTHExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;


{ The exchange must be at least 6 characters in length, with each entry
  separated by a space.

  > The new parse routine will put the first four exchange fields into
  > separate strings.  Each is checked to see if it looks like a callsign.
  > If it does, it is used to update the call and is removed; the other
  > strings move up.  What's left constitute the first, second, and third
  > entries described below.

  If the first entry is the QSO number, the second
  entry must be the name and the last entry the QTH string.  If the second
  entry is the QSO number, the first entry must be the name and the last
  entry the QTH.  If the last entry is to be the QSO number (this is
  normal if the program already knew the name and QTH as an initial
  exchange), then the first entry is the name, the second the QTH.  The
  QTH string is ignored if the call is outside the 49 states or Canada.
  If the name is different than that in the name database, it will be
  changed.  }

VAR TempString: Str20;
    NumberEntries: INTEGER;
    DX: BOOLEAN;
    EntryList: EntryArray;

    BEGIN
    ProcessQSONumberNameAndDomesticOrDXQTHExchange := False;

    IF Length (Exchange) < 4 THEN
        BEGIN
        ExchangeErrorMessage := 'Not enough info in exchange!!';
        Exit;
        END;

    DX := NOT DomesticCountryCall (RXData.Callsign);

    ParseExchangeIntoFields (Exchange, EntryList, NumberEntries);

    { If we have multiple number entries at the end of the exchange,
      filter out the previous ones }

    WHILE (NumberEntries > 3) AND
           StringIsAllNumbers (EntryList [NumberEntries]) AND
           StringIsAllNumbers (EntryList [NumberEntries - 1]) DO
               BEGIN
               EntryList [NumberEntries - 1] := EntryList [NumberEntries];
               Dec (NumberEntries);
               END;

    IF NumberEntries <= 3 THEN    { Use old method }
        BEGIN
        IF StringIsAllNumbers (EntryList [1]) THEN
            BEGIN
            Val (EntryList [1], RXData.NumberReceived);
            RXData.Name := EntryList [2];
            IF NumberEntries >= 3 THEN RXData.QTHString := EntryList [3];
            END
        ELSE
            IF StringIsAllNumbers (EntryList [2]) THEN
                BEGIN
                Val (EntryList [2], RXData.NumberReceived);
                RXData.Name := EntryList [1];
                IF NumberEntries >= 3 THEN RXData.QTHString := EntryList [3];
                END
            ELSE
                IF StringIsAllNumbers (EntryList [3]) THEN
                    BEGIN
                    Val (EntryList [3], RXData.NumberReceived);
                    RXData.Name := EntryList [1];
                    RXData.QTHString := EntryList [2];
                    END
                ELSE
                    BEGIN
                    ExchangeErrorMessage := 'No QSO number found!!';
                    Exit;
                    END;
        END

    ELSE  { We have four or more entries, use new procedure }
        IF (NumberEntries = 4) THEN
            BEGIN
            IF StringIsAllNumbers (EntryList [2]) THEN
                BEGIN
                Val (EntryList [2], RXData.NumberReceived);
                RXData.Name := EntryList [3];
                RXData.QTHString := EntryList [4];
                END
            ELSE
                IF StringIsAllNumbers (EntryList [3]) THEN
                    BEGIN
                    Val (EntryList [3], RXData.NumberReceived);
                    RXData.Name := EntryList [4];
                    RXData.QTHString := EntryList [2];
                    END
                ELSE
                    IF StringIsAllNumbers (EntryList [1]) AND StringIsAllNumbers (EntryList [4]) THEN
                        BEGIN
                        Val (EntryList [4], RXData.NumberReceived);
                        RXData.Name := EntryList [2];
                        RXData.QTHString := EntryList [3];
                        END;


            END
        ELSE

        { Last chance - five or more entries }

            IF StringIsAllNumbers (EntryList [NumberEntries - 2]) THEN
                BEGIN
                Val (EntryList [NumberEntries - 2], RXData.NumberReceived);
                RXData.Name := EntryList [NumberEntries - 1];
                RXData.QTHString := EntryList [NumberEntries];
                END;

    IF (RXData.Name = '') OR ((RXData.QTHString = '') AND NOT DX) THEN
        BEGIN
        ExchangeErrorMessage := 'Missing QTH and/or name!!';
        Exit;
        END;

    IF Length (RXData.Name) < Length (RXData.QTHString) THEN
        BEGIN
        IF RXData.Name <> CD.GetName (RXData.Callsign) THEN
            IF (Length (RXData.Name) = 2) OR (Length (RXData.Name) = 3) THEN
                BEGIN
                TempString := RXData.Name;
                RXData.Name := RXData.QTHString;
                RXData.QTHString := TempString;
                END;
        END;

{ The DX QTH gets put in by the GetDXQTH routine in LogDupe when the mult
  check was performmed.  Otherwise, FoundDomesticQTH will put the domestic
  QTH in there for us.  }

    IF NOT DomesticCountryCall (RXData.Callsign) THEN
        ProcessQSONumberNameAndDomesticOrDXQTHExchange := True
    ELSE
        IF FoundDomesticQTH (RXData) THEN
            ProcessQSONumberNameAndDomesticOrDXQTHExchange := True
        ELSE
            BEGIN
            TempString := RXData.Name;
            RXData.Name := RXData.QTHString;
            RXData.QTHString := TempString;

            IF FoundDomesticQTH (RXData) THEN
                ProcessQSONumberNameAndDomesticOrDXQTHExchange := True
            ELSE
                ExchangeErrorMessage := 'Improper domesitc QTH or missing name!!';

            END;
    END;



PROCEDURE ProcessSSEntry (InputString: Str80);

VAR NumberStr, TempString: Str20;

    BEGIN
    TempString := InputString;

    NumberStr := '';

  { Gobble up all the leading numbers }

    WHILE StringIsAllNumbers (Copy (TempString, 1, 1)) DO
        BEGIN
        NumberStr := NumberStr + Copy (TempString, 1, 1);
        Delete (TempString, 1, 1);
        END;

    IF Length (NumberStr) > 4 THEN Exit;  { I don't like this! }

    IF (TempString = '') THEN  { All we had was numbers.  Is it a check? }
        BEGIN
        IF (Length (NumberStr) = 2) THEN
            BEGIN
            IF SSEx.Check = '' THEN
                SSEx.Check := NumberStr
            ELSE
                IF (SSEx.Number = '') AND (SSEx.Prec <> Chr (0)) THEN
                    SSEx.Number := NumberStr;
            END
        ELSE
            IF SSEx.Number = '' THEN SSEx.Number := NumberStr;

        Exit;
        END;

  { Gee, this next one works even if the guy only entered A B or Q! }

    IF (TempString = 'A') OR (TempString = 'B') OR (TempString = 'Q') OR
       (TempString = 'U') OR (TempString = 'M') OR (TempString = 'S') THEN
           BEGIN
           IF SSEx.Number = ''    THEN SSEx.Number := NumberStr;
           IF SSEx.Prec = Chr (0) THEN SSEx.Prec := TempString [1];
           Exit;
           END;

  { There is more than one character left in the string. }

    IF (Copy (TempString, 1, 1) = 'A') OR
       (Copy (TempString, 1, 1) = 'B') OR
       (Copy (TempString, 1, 1) = 'Q') OR
       (Copy (TempString, 1, 1) = 'U') OR
       (Copy (TempString, 1, 1) = 'M') OR
       (Copy (TempString, 1, 1) = 'S') THEN

  { We might have a precedence and more info.  The only legal thing after
    a real precedence is a number.  Otherwise, it must be part of the
    section (ie: AB or AL) }

        IF StringIsAllNumbers (Copy (TempString, 2, 1)) THEN

  { Okay, we have the A/B/Q/U/M/S followed by a number.  We will
    assume this to be the precedence. }

            BEGIN
            IF SSEx.Number = ''    THEN SSEx.Number := NumberStr;
            IF SSEx.Prec = Chr (0) THEN SSEx.Prec := TempString [1];
            IF SSEx.Check = ''     THEN SSEx.Check := Copy (TempString, 2, 2);
            Delete (TempString, 1, 3);
            IF SSEx.Section = ''   THEN SSEx.Section := TempString;
            Exit;
            END;

  { We must be looking at a check and section, or maybe just a section }

    IF Length (NumberStr) = 2 THEN
        IF SSEx.Check = '' THEN SSEx.Check := NumberStr;

    IF SSEx.Section = '' THEN SSEx.Section := TempString;
    END;



FUNCTION ProcessQSONumberPrecedenceCheckDomesticQTHExchange (Exchange: Str80;
           VAR RXData: ContestExchange): BOOLEAN;

{ A new improved version!  It tries to maintain compatability with the old
  version, but allows the following:

  If an entry has one, two, three or four digits and the letter A, B or Q
  at the end, then it is assumed to be a number and a precedence.  The last
  of any of these type of entries is used.

  If an entry has two numbers and then at least two letters after it, it is
  assumed to be a check and a section.  Again, the last of any of these type
  of entries is used.

  If an entry is at least four chars long and does not start or end with a
  number or if it has a / in it, then it is assumed to be a callsign.  The
  call will override the callsign window call if it is different.  Again,
  the last on of these is used.
}

VAR Entries: ARRAY [0..10] OF Str20;
    NumberEntries: INTEGER;
    Entry, xResult: INTEGER;

    BEGIN
    ProcessQSONumberPrecedenceCheckDomesticQTHExchange := False;

    SSEx.Number  := '';
    SSEx.Prec    := Chr (0);
    SSEx.Check   := '';
    SSEx.Section := '';

    IF Length (Exchange) < 6 THEN Exit;

    NumberEntries := 0;

    WHILE (Exchange <> '') AND (NumberEntries <= 10) DO
        BEGIN
        Entries [NumberEntries] := RemoveFirstString (Exchange);
        Inc (NumberEntries);
        END;

    FOR Entry := NumberEntries - 1 DOWNTO 0 DO
        BEGIN
        ProcessSSEntry (Entries [Entry]);

        IF (SSEx.Number  <> '') AND
           (SSEx.Prec    <> Chr (0)) AND
           (SSEx.Check   <> '') AND
           (SSEx.Section <> '') THEN
               BEGIN
               Val (SSEx.Number, RXData.NumberReceived, xResult);
               IF xResult <> 0 THEN Exit;

               RXData.Precedence := SSEx.Prec;
               RXData.Check      := SSEx.Check;

               IF (Length (SSEx.Check) <> 2) OR NOT StringIsAllNumbers (SSEx.Check) THEN
                   Exit;

               RXData.QTHString := SSEx.Section;

               IF FoundDomesticQTH (RXData) THEN
                   BEGIN
                   ProcessQSONumberPrecedenceCheckDomesticQTHExchange := True;
                   Exit;
                   END;
               END;
        END;
    END;



FUNCTION LooksLikeRST (Ex: Str80; VAR RST: RSTString; Mode: ModeType): BOOLEAN;

    BEGIN
    LooksLikeRST := False;

    IF Mode = CW THEN
        BEGIN
        IF Length (Ex) <> 3 THEN Exit;

        IF (Ex [1] >= '1') AND (Ex [1] <= '5') AND
           (Ex [2] >= '1') AND (Ex [2] <= '9') AND
           (((Ex [3] >= '1') AND (Ex [3] <= '9')) OR (UpCase (Ex [3]) = 'A')) THEN
               BEGIN
               LooksLikeRST := True;
               RST := Ex;
               END;
        Exit;
        END
    ELSE
        CASE Length (Ex) OF

            2: IF (Ex [1] >= '1') AND (Ex [1] <= '5') AND
                  (Ex [2] >= '1') AND (Ex [2] <= '9') THEN
                      BEGIN
                      LooksLikeRST := True;
                      RST := Ex;
                      END;

            3: IF UpperCase (Copy (Ex, Length (Ex), 1)) = 'A' THEN
                   IF (ActiveBand = Band6) OR (ActiveBand = Band2) THEN
                       IF (Ex [1] >= '1') AND (Ex [1] <= '5') AND
                          (Ex [2] >= '1') AND (Ex [2] <= '9') THEN
                              BEGIN
                              LooksLikeRST := True;
                              RST := Ex;
                              END;
            END;

    END;



FUNCTION ValidAllJAPrefecture (Prefecture: Str20): BOOLEAN;

VAR PrefectureValue, xResult: INTEGER;

    BEGIN
    IF Copy (Prefecture, 1, 1) = 'p' THEN Delete (Prefecture, 1, 1);

    Val (Prefecture, PrefectureValue, xResult);

    IF xResult <> 0 THEN
        BEGIN
        ValidAllJAPrefecture := False;
        Exit;
        END;

    ValidAllJAPrefecture := ((PrefectureValue > 1)   AND (PrefectureValue < 51)) OR
                            ((PrefectureValue > 100) AND (PrefectureValue < 115));
    END;




FUNCTION ProcessRSTAllJAPrefectureAndPrecedenceExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;


{ Updated for new rules.  Exchange now RS(T) + Prefixture # + Power (A/B/C) }

VAR ExchangeString: Str20;
    Entries: ARRAY [0..10] OF Str20;
    NumberEntries: INTEGER;

    BEGIN
    Exchange := UpperCase (Exchange);

    RXData.RSTReceived := DefaultRST;

    ProcessRSTAllJAPrefectureAndPrecedenceExchange := False;

    { Get power out of the way }

    IF StringHas (Exchange, 'H') THEN
        BEGIN
        RXData.Precedence := 'H';
        Delete (Exchange, Pos ('H', Exchange), 1);
        END
    ELSE
        IF StringHas (Exchange, 'M') THEN
            BEGIN
            RXData.Precedence := 'M';
            Delete (Exchange, Pos ('M', Exchange), 1);
            END
        ELSE
            IF Stringhas (Exchange, 'L') THEN
                BEGIN
                RXData.Precedence := 'L';
                Delete (Exchange, Pos ('L', Exchange), 1);
                END
            ELSE
                IF Stringhas (Exchange, 'P') THEN
                    BEGIN
                    RXData.Precedence := 'P';
                    Delete (Exchange, Pos ('P', Exchange), 1);
                    END;

    GetRidOfPostcedingSpaces (Exchange);

    NumberEntries := 0;

    ExchangeString := Exchange;

    WHILE ExchangeString <> '' DO
        BEGIN
        Entries [NumberEntries] := RemoveFirstString (ExchangeString);
        GetRidOfPrecedingSpaces (Entries [NumberEntries]);
        Inc (NumberEntries);
        END;

    IF NumberEntries = 1 THEN         { Everything as one entry }
        BEGIN
        IF ActiveMode = CW THEN
            BEGIN
            CASE Length (Entries [0]) OF
                2, 3: IF ValidAllJAPrefecture (Entries [0]) THEN
                         RXData.QTHString := 'p' + Entries [0];

                4: IF Copy (Entries [0], 2, 1) = '1' THEN  { S + 3 dig Pref? }
                       IF ValidAllJAPrefecture (Copy (Entries [0], 2, 3)) THEN
                           BEGIN
                           RXData.QTHString := 'p' + Copy (Entries [0], 2, 3);
                           RXData.RSTReceived [2] := Entries [0] [1];
                           END;

                5: IF ValidAllJAPrefecture (Copy (Entries [0], 4, 2)) THEN
                       BEGIN
                       RXData.QTHString := 'p' + Copy (Entries [0], 4, 2);
                       RXData.RSTReceived := Copy (Entries [0], 1, 3);
                       END;

                6: IF ValidAllJAPrefecture (Copy (Entries [0], 4, 3)) THEN
                       BEGIN
                       RXData.QTHString := 'p' + Copy (Entries [0], 4, 3);
                       RXData.RSTReceived := Copy (Entries [0], 1, 3);
                       END;


                END;
            END
        ELSE
            BEGIN   { SSB }
            CASE Length (Entries [0]) OF

                2: IF ValidAllJAPrefecture (Entries [0]) THEN
                       RXData.QTHString := 'p' + Entries [0];

                3: IF ValidAllJAPrefecture (Entries [0]) THEN
                       RXData.QTHString := 'p' + Entries [0]
                   ELSE
                       IF ValidAllJAPrefecture (Copy (Entries [0], 2, 2)) THEN
                           BEGIN
                           RXData.QTHString := Copy (Entries [0], 2, 2);
                           RXData.RSTReceived [2] := Entries [0] [1];
                           END;

                4: IF Copy (Entries [0], 2, 1) = '1' THEN
                       BEGIN
                       IF ValidAllJAPrefecture (Copy (Entries [0], 2, 3)) THEN
                           BEGIN
                           RXData.QTHString := 'p' + Copy (Entries [0], 2, 3);
                           RXData.RSTReceived [2] := Entries [0] [1];
                           END;
                       END
                   ELSE
                       IF ValidAllJAPrefecture (Copy (Entries [0], 3, 2)) THEN
                           BEGIN
                           RXData.QTHString := 'p' + Copy (Entries [0], 3, 2);
                           RXData.RSTReceived := Copy (Entries [0], 1, 2);
                           END;

                5: IF ValidAllJAPrefecture (Copy (Entries [0], 3, 3)) THEN
                       BEGIN
                       RXData.QTHString := 'p' + Copy (Entries [0], 3, 3);
                       RXData.RSTReceived := Copy (Entries [0], 1, 2);
                       END;

                END;

            END;
        END;

    { Two entries to look at }

    IF NumberEntries = 2 THEN
        BEGIN
        IF ValidAllJAPrefecture (Entries [0]) THEN
            IF NOT ValidAllJAPrefecture (Entries [1]) THEN
                BEGIN
                RXData.QTHString := Entries [0];

                CASE Length (Entries [1]) OF
                    1: RXData.RSTReceived [2] := Entries [1] [1];

                    2: IF ActiveMode = Phone THEN
                           RXData.RSTReceived := Entries [1];

                    3: IF ActiveMode = CW THEN
                           RXData.RSTReceived := Entries [1];

                    END;
                END;

        IF ValidAllJAPrefecture (Entries [1]) THEN
            IF NOT ValidAllJAPrefecture (Entries [0]) THEN
                BEGIN
                RXData.QTHString := Entries [1];

                CASE Length (Entries [0]) OF
                    1: RXData.RSTReceived [2] := Entries [0] [1];

                    2: IF ActiveMode = Phone THEN
                           RXData.RSTReceived := Entries [0];

                    3: IF ActiveMode = CW THEN
                           RXData.RSTReceived := Entries [0];

                    END;
                END;

        IF ValidAllJAPrefecture (Entries [0]) THEN
            IF ValidAllJAPrefecture (Entries [1]) THEN  { Both valid }
                BEGIN
                CASE Length (Entries [0]) OF
                    1: RXData.RSTReceived [2] := Entries [0] [1];
                    2: IF ActiveMode = Phone THEN RXData.RSTReceived := Entries [0];
                    3: IF ActiveMode = CW    THEN RXData.RSTReceived := Entries [0];
                    END;

                RXData.QTHString := Entries [1];
                END;

        END;

    IF NOT ValidAllJAPrefecture (RXData.QTHString) THEN Exit;
    IF NOT FoundDomesticQTH (RXData) THEN Exit;

    ProcessRSTAllJAPrefectureAndPrecedenceExchange := RXData.Precedence <> '';
    END;



FUNCTION ProcessRSTAndAgeExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ The exchange must be made of only digits and spaces.  If the exchange has
  a space, the RS(T) is assumed to be the first entry, and the age the
  second.  You can enter just the strength of the RS(T) or the full report.
  The age must be two digits.  You can omit the space if you like.  The
  last two digits will be used for the age and the digits before that for
  the RS(T) information.                        }

VAR ExchangeString: Str20;
    Entries: ARRAY [0..10] OF Str20;
    NumberEntries: INTEGER;

    BEGIN
    RXData.RSTReceived := DefaultRST;

    ProcessRSTAndAgeExchange := False;

    IF NOT StringIsAllNumbersOrSpaces (Exchange) THEN Exit;

    NumberEntries := 0;

    ExchangeString := Exchange;

    WHILE ExchangeString <> '' DO
        BEGIN
        Entries [NumberEntries] := RemoveFirstString (ExchangeString);
        GetRidOfPrecedingSpaces (Entries [NumberEntries]);
        Inc (NumberEntries);
        END;

    IF (NumberEntries = 1) AND (Length (Entries [0]) = 2) THEN
        BEGIN
        RXData.Age := Entries [0];
        ProcessRSTAndAgeExchange := True;
        Exit;
        END;

    IF NumberEntries = 2 THEN
        BEGIN
        IF LooksLikeRST (Entries [0], RXData.RSTReceived, ActiveMode) THEN
            RXData.Age := Entries [1]
        ELSE
            IF LooksLikeRST (Entries [1], RXData.RSTReceived, ActiveMode) THEN
                RXData.Age := Entries [0]
            ELSE
                RXData.Age := Entries [1];

        ProcessRSTAndAgeExchange := Length (RXData.Age) = 2;
        Exit;
        END;

    IF NumberEntries = 3 THEN
        BEGIN
        IF LooksLikeRST (Entries [1], RXData.RSTReceived, ActiveMode) THEN
            RXData.Age := Entries [2]
        ELSE
            IF LooksLikeRST (Entries [2], RXData.RSTReceived, ActiveMode) THEN
                RXData.Age := Entries [1]
            ELSE
                RXData.Age := Entries [2];

        ProcessRSTAndAgeExchange := Length (RXData.Age) = 2 ;
        Exit;
        END;

    CASE Length (Exchange) OF
        2: BEGIN
           RXData.Age := Exchange;
           ProcessRSTAndAgeExchange := True;
           END;

        3: BEGIN
           Delete (DefaultRST, 2, 1);
           Insert (Exchange [1], DefaultRST, 2);
           RXData.RSTReceived := DefaultRST;
           Delete (Exchange, 1, 1);
           RXData.Age := Exchange;
           ProcessRSTAndAgeExchange := Length (RXData.Age) = 2 ;
           END;

        4: BEGIN
           IF ActiveMode <> Phone THEN Exit;
           RXData.RSTReceived := Copy (Exchange, 1, 2);
           RXData.Age := Copy (Exchange, 3, 2);
           ProcessRSTAndAgeExchange := True;
           END;

        5: BEGIN
           IF ActiveMode <> CW THEN Exit;
           RXData.RSTReceived := Copy (Exchange, 1, 3);
           RXData.Age := Copy (Exchange, 4, 2);
           ProcessRSTAndAgeExchange := True;
           END;
       END; { of case Length (Exchange) }
     END;



FUNCTION ProcessRSTAndPrefectureExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

VAR ExchangeString: Str20;
    Entries: ARRAY [0..10] OF Str20;
    NumberEntries: INTEGER;

    BEGIN
    RXData.RSTReceived := DefaultRST;

    ProcessRSTAndPrefectureExchange := False;

    IF NOT StringIsAllNumbersOrSpaces (Exchange) THEN Exit;

    NumberEntries := 0;

    ExchangeString := Exchange;

    WHILE ExchangeString <> '' DO
        BEGIN
        Entries [NumberEntries] := RemoveFirstString (ExchangeString);
        GetRidOfPrecedingSpaces (Entries [NumberEntries]);
        Inc (NumberEntries);
        END;

    IF (NumberEntries = 1) AND (Length (Entries [0]) = 2) THEN
        BEGIN
        RXData.QTHString := 'p' + Entries [0];
        ProcessRSTAndPrefectureExchange := FoundDomesticQTH (RXData);
        Exit;
        END;

    IF NumberEntries = 2 THEN
        BEGIN
        IF LooksLikeRST (Entries [0], RXData.RSTReceived, ActiveMode) THEN
            RXData.QTHString := 'p' + Entries [1]
        ELSE
            IF LooksLikeRST (Entries [1], RXData.RSTReceived, ActiveMode) THEN
                RXData.QTHString := 'p' + Entries [0]
            ELSE
                RXData.QTHString := 'p' + Entries [1];

        ProcessRSTAndPrefectureExchange := FoundDomesticQTH (RXData);
        Exit;
        END;

    IF NumberEntries = 3 THEN
        BEGIN
        IF LooksLikeRST (Entries [1], RXData.RSTReceived, ActiveMode) THEN
            RXData.QTHString := 'p' + Entries [2]
        ELSE
            IF LooksLikeRST (Entries [2], RXData.RSTReceived, ActiveMode) THEN
                RXData.QTHString := 'p' + Entries [1]
            ELSE
                RXData.QTHString := 'p' + Entries [2];

        ProcessRSTAndPrefectureExchange := FoundDomesticQTH (RXData);
        Exit;
        END;

    CASE Length (Exchange) OF
        2: BEGIN
           RXData.QTHString := 'p' + Exchange;
           ProcessRSTAndPrefectureExchange := FoundDomesticQTH (RXData);
           END;

        3: BEGIN
           Delete (DefaultRST, 2, 1);
           Insert (Exchange [1], DefaultRST, 2);
           RXData.RSTReceived := DefaultRST;
           Delete (Exchange, 1, 1);
           RXData.QTHString := 'p' + Exchange;
           ProcessRSTAndPrefectureExchange := FoundDomesticQTH (RXData);
           END;

        4: BEGIN
           IF ActiveMode <> Phone THEN Exit;
           RXData.RSTReceived := Copy (Exchange, 1, 2);
           RXData.QTHString := 'p' + Copy (Exchange, 3, 2);
           ProcessRSTAndPrefectureExchange := FoundDomesticQTH (RXData);
           END;

        5: BEGIN
           IF ActiveMode <> CW THEN Exit;
           RXData.RSTReceived := Copy (Exchange, 1, 3);
           RXData.QTHString := 'p' + Copy (Exchange, 4, 2);
           ProcessRSTAndPrefectureExchange := FoundDomesticQTH (RXData);
           END;
       END; { of case Length (Exchange) }
     END;



FUNCTION ProcessRSTAndDomesticQTHExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ Any digits found at the start of the exchange will be used to determine
  the RS(T).  If the first character is not a digit, the default RS(T)
  will be used and the whole exchange will be used as the QTH. }

VAR TempString: Str80;
    QTHFound: BOOLEAN;

    BEGIN
    QTHFound := False;

    ProcessRSTAndDomesticQTHExchange := False;
    GetRidOfPrecedingSpaces (Exchange);
    GetRidOfPostcedingSpaces (Exchange);

    IF NOT StringHas (Exchange, ' ') AND NOT StringIsAllNumbers (Exchange) THEN
        BEGIN
        RXData.RSTReceived := DefaultRST;
        RXData.QTHString := Exchange;
        ProcessRSTAndDomesticQTHExchange := FoundDomesticQTH (RXData);
        Exit;
        END;

    WHILE Exchange <> '' DO
        BEGIN
        TempString := RemoveLastString (Exchange);

        IF StringIsAllNumbers (TempString) AND (RXData.RSTReceived = '') THEN
            BEGIN
            IF Length (TempString) = 1 THEN
                BEGIN
                CASE ActiveMode OF
                    CW:    RXData.RSTReceived := '5' + TempString + '9';
                    Phone: RXData.RSTReceived := '5' + TempString;
                    END;
                END
            ELSE
                RXData.RSTReceived := TempString;
            END
        ELSE
            IF NOT QTHFound THEN
                BEGIN
                RXData.QTHString := TempString;
                IF FoundDomesticQTH (RXData) THEN QTHFound := True;
                END;
        END;

    IF QTHFound THEN
        BEGIN
        IF RXData.RSTReceived = '' THEN
            RXData.RSTReceived := DefaultRST;
        ProcessRSTAndDomesticQTHExchange := True;
        Exit;
        END;

    END;


{KK1L: 6.72}
FUNCTION ProcessRSTAndJAPrefectureExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ A single entry is assumed to be the exchange and not RST. Two entries assume }
{ the RS(T) is the first provided it is 3 characters or less                   }

VAR TempString: Str80;
    QTHFound: BOOLEAN;

    BEGIN
    QTHFound := False;

    ProcessRSTAndJAPrefectureExchange := False;
    GetRidOfPrecedingSpaces (Exchange);
    GetRidOfPostcedingSpaces (Exchange);

    IF NOT StringHas (Exchange, ' ') THEN
        BEGIN
        RXData.RSTReceived := DefaultRST;
        RXData.QTHString := Exchange;
        ProcessRSTAndJAPrefectureExchange := FoundDomesticQTH (RXData);
        Exit;
        END;

    WHILE Exchange <> '' DO
        BEGIN
        TempString := RemoveLastString (Exchange);

        IF StringIsAllNumbers (TempString) AND (RXData.RSTReceived = '') AND (Length (TempString) < 4) THEN
            BEGIN
            IF Length (TempString) = 1 THEN
                BEGIN
                CASE ActiveMode OF
                    CW:    RXData.RSTReceived := '5' + TempString + '9';
                    Phone: RXData.RSTReceived := '5' + TempString;
                    END;
                END
            ELSE
                RXData.RSTReceived := TempString;
            END
        ELSE
            IF NOT QTHFound THEN
                BEGIN
                RXData.QTHString := TempString;
                IF FoundDomesticQTH (RXData) THEN QTHFound := True;
                END;
        END;

    IF QTHFound THEN
        BEGIN
        IF RXData.RSTReceived = '' THEN
            RXData.RSTReceived := DefaultRST;
        ProcessRSTAndJAPrefectureExchange := True;
        Exit;
        END;

    END;



FUNCTION ProcessRSTAndDomesticOrDXQTHExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

    BEGIN
    ProcessRSTAndDomesticOrDXQTHExchange := False;

    IF DomesticCountryCall (RXData.Callsign) THEN
        BEGIN
        ProcessRSTAndDomesticOrDXQTHExchange := ProcessRSTAndDomesticQTHExchange (Exchange, RXData);
        Exit;
        END;

    IF NOT ValidRST (Exchange, RXData.RSTReceived, ActiveMode) THEN Exit;
    ProcessRSTAndDomesticOrDXQTHExchange := True;
    END;



FUNCTION ProcessRSTNameAndQTHExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ Each entry of the exchange must be separated by spaces.  The following
  formats work:

   Entry  #1   Entry  #2   Entry  #3
   ---------   ---------   ---------
                                         (RS(T) = default, no QTH or name)
   Name                                  (RS(T) = default, no QTH)
   Name        QTH                       (RS(T) = default)
   Name        QTH         RS(T)         (Enter whole RS(T))
   Name        RS(T)                     (No QTH)
   Name        RS(T)       QTH           (Enter whole RS(T))
   RS(T)                                 (Enter whole RS(T), no QTH or name)
   RS(T)       Name                      (Enter whole RS(T), no QTH)
   RS(T)       QTH         Name          (Enter whole RS(T))             }

VAR FirstString, SecondString, ThirdString: Str20;

    BEGIN
    ProcessRSTNameAndQTHExchange := True;

    IF Exchange = '' THEN
        BEGIN
        RXData.RSTReceived := DefaultRST;
        Exit;
        END;

    ParseExchange (Exchange, FirstString, SecondString, ThirdString);

    IF StringIsAllNumbers (FirstString) THEN
        BEGIN
        RXData.RSTReceived := FirstString;
        IF ThirdString <> '' THEN
            BEGIN
            RXData.QTHString := SecondString;
            RXData.Name      := ThirdString;
            END
        ELSE
            RXData.Name      := SecondString;
        Exit;
        END;

    IF StringIsAllNumbers (SecondString) THEN
        BEGIN
        RXData.Name        := FirstString;
        RXData.RSTReceived := SecondString;
        RXData.QTHString   := ThirdString;
        Exit;
        END;

    IF StringIsAllNumbers (ThirdString) THEN
        BEGIN
        RXData.Name        := FirstString;
        RXData.QTHString   := SecondString;
        RXData.RSTReceived := ThirdString;
        Exit;
        END;

    IF ThirdString <> '' THEN
        BEGIN
        ProcessRSTNameAndQTHExchange := False;
        Exit;
        END;

    RXData.Name        := FirstString;
    RXData.QTHString   := SecondString;
    RXData.RSTReceived := DefaultRST;
    END;



PROCEDURE LookForCutNumbers (VAR Exchange: Str80);

VAR TempString, NewExchange: Str80;
    Address: INTEGER;

    BEGIN
    NewExchange := '';

    WHILE Exchange <> '' DO
        BEGIN
        TempString := RemoveFirstString (Exchange);

        IF NOT StringIsAllNumbers (TempString) THEN
            BEGIN
            FOR Address := 1 TO Length (TempString) DO
                CASE TempString [Address] OF

                    'T', 'O': TempString [Address] := '0';
                    'A':      TempString [Address] := '1';
                    'U':      TempString [Address] := '2';
                    'E':      TempString [Address] := '5';
                    'N':      TempString [Address] := '9';
                    END;
            END;

        NewExchange := NewExchange + TempString + ' '
        END;

    GetRidOfPostcedingSpaces (NewExchange);
    Exchange := NewExchange;
    END;



FUNCTION ProcessRSTAndQSONumberExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ Each entry of the exchange must be separated by spaces.  The following
  formats work:

   Entry  #1   Entry  #2
   ---------   ---------
   RS(T)       QSO Number
   QSO Number                (RS(T) = default)

  You can enter just the strength of the RS(T) if you like.

  Cut QSO numbers are changed to numeric. }

VAR xResult: INTEGER;

    BEGIN
    ProcessRSTAndQSONumberExchange := False;

    LookForCutNumbers (Exchange);

    IF StringIsAllNumbers (Exchange) THEN
        BEGIN
        Val (Exchange, RXData.NumberReceived, xResult);
        IF xResult <> 0 THEN Exit;
        RXData.RSTReceived := DefaultRST;
        ProcessRSTAndQSONumberExchange := True;
        END
    ELSE
        IF ValidRST (Exchange, RXData.RSTReceived, ActiveMode) THEN
            BEGIN
            Exchange := RemoveFirstString (Exchange);
            Val (Exchange, RXData.NumberReceived, xResult);
            IF xResult <> 0 THEN Exit;
            ProcessRSTAndQSONumberExchange := True;
            END;
    END;



FUNCTION LooksLikePower (TestString: Str20): BOOLEAN;

{ Returns true if entry is all numbers, or all numbers with a W at the end }

VAR CharPos: INTEGER;

    BEGIN
    LooksLikePower := False;

    IF TestString = '' THEN Exit;

    TestString := UpperCase (TestString);

    FOR CharPos := 1 TO Length (TestString) DO
        BEGIN
        IF ((TestString [CharPos]  < '0') OR (TestString [CharPos]  > '9')) AND
            (TestString [CharPos] <> 'W') AND (TestString [CharPos] <> 'M') AND
            (TestString [CharPos] <> 'R') THEN Exit;
        END;

    LooksLikePower := True;
    END;

FUNCTION ProcessRSTPossibleDomesticQTHAndPowerExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ RST MaybeQTH and Power.  Power will either have numbers and a W in it, or
  four numbers (for ARCI member number) }


VAR FirstString, SecondString, ThirdString: Str20;


    BEGIN
    ProcessRSTPossibleDomesticQTHAndPowerExchange:= False;

    ParseExchange (Exchange, FirstString, SecondString, ThirdString);

    IF StringIsAllNumbers (ThirdString) AND (Length (ThirdString) <= 3) THEN
        BEGIN
        IF NOT ValidRST (ThirdString, RXData.RSTReceived, ActiveMode) THEN Exit;
        END
    ELSE
        IF StringIsAllNumbers (SecondString) AND (Length (SecondString) <= 3) THEN
            BEGIN
            IF NOT ValidRST (SecondString, RXData.RSTReceived, ActiveMode) THEN Exit;
            END
        ELSE
            IF StringIsAllNumbers (FirstString) AND (Length (FirstString) <= 3) THEN
                BEGIN
                IF NOT ValidRST (FirstString, RXData.RSTReceived, ActiveMode) THEN Exit;
                END
            ELSE
                IF ActiveMode = CW THEN
                    RXData.RSTReceived := '599'
                ELSE
                    RXData.RSTReceived := '59';


    IF LooksLikePower (ThirdString) THEN
        RXData.Power := ThirdString
    ELSE
        IF LooksLikePower (SecondString) THEN
            RXData.Power := SecondString
        ELSE
            IF LooksLikePower (FirstString) THEN RXData.Power := FirstString;

    IF (ThirdString <> '') AND (NOT LooksLikePower (ThirdString)) THEN
        RXData.QTHString := ThirdString
    ELSE
        IF (SecondString <> '') AND (NOT LooksLikePower (SecondString)) THEN
            RXData.QTHString := SecondString
        ELSE
            IF (FirstString <> '') AND (NOT LooksLikePower (FirstString)) THEN
                RXData.QTHString := FirstString;

    IF RXData.Power = '' THEN Exit;

    { RXData.DXQTH is only there if it is a DX call }

    IF RXData.DXQTH = '' THEN
        ProcessRSTPossibleDomesticQTHAndPowerExchange := FoundDomesticQTH (RXData)
    ELSE
        ProcessRSTPossibleDomesticQTHAndPowerExchange := True;
    END;


FUNCTION ProcessRSTQSONumberAndDomesticQTHExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ Each entry of the exchange must be separated by spaces.  The following
  formats work:

   Entry  #1   Entry  #2   Entry  #3
   ---------   ---------   ---------
   QTH                                    (RS(T) = default, QSO number = 1)
   QTH         QSO Number                 (RS(T) = default)
   QTH         RS(T)       QSO Number
   QSO Number  QTH                        (RS(T) = default)
   RS(T)       QTH         QSO Number
   RS(T)       QSO Number  QTH

  You can enter just the strength of the RS(T) if desired.      }

VAR NumberString, FirstString, SecondString, ThirdString: Str20;

    BEGIN
    ProcessRSTQSONumberAndDomesticQTHExchange := False;
    IF Exchange = '' THEN Exit;

    ParseExchange (Exchange, FirstString, SecondString, ThirdString);

    IF NOT StringIsAllNumbers (FirstString) THEN
        BEGIN
        RXData.QTHString := FirstString;
        IF ThirdString <> '' THEN
            NumberString := SecondString + ' ' + ThirdString
        ELSE
            NumberString := SecondString;
        END
    ELSE
        IF NOT StringIsAllNumbers (SecondString) THEN
            BEGIN
            RXData.QTHString := SecondString;
            IF ThirdString <> '' THEN
                NumberString := FirstString + ' ' + ThirdString
            ELSE
                NumberString := FirstString;
            END
        ELSE
            BEGIN
            RXData.QTHString := ThirdString;
            NumberString := FirstString + ' ' + SecondString;
            END;

    IF ProcessRSTAndQSONumberExchange (NumberString, RXData) THEN
        IF RXData.QTHString <> '' THEN
            ProcessRSTQSONumberAndDomesticQTHExchange := FoundDomesticQTH (RXData);
    END;



FUNCTION ProcessRSTQSONumberAndGridSquareExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ Each entry of the exchange must be separated by spaces.  The following
  formats work:

   Entry  #1   Entry  #2   Entry  #3
   ---------   ---------   ---------
   Grid                                    (RS(T) = default, QSO number = 1)
   Grid        QSO Number                  (RS(T) = default)
   Grid        RS(T)       QSO Number
   QSO Number  Grid                        (RS(T) = default)
   RS(T)       Grid        QSO Number
   RS(T)       QSO Number  Grid

  You can enter just the strength of the RS(T) if desired.      }

VAR NumberString, FirstString, SecondString, ThirdString: Str20;

    BEGIN
    ProcessRSTQSONumberAndGridSquareExchange := False;
    IF Exchange = '' THEN Exit;

    ParseExchange (Exchange, FirstString, SecondString, ThirdString);

    IF NOT StringIsAllNumbers (FirstString) THEN
        BEGIN
        RXData.QTHString := FirstString;

        IF ThirdString <> '' THEN
            NumberString := SecondString + ' ' + ThirdString
        ELSE
            NumberString := SecondString;
        END
    ELSE
        IF NOT StringIsAllNumbers (SecondString) THEN
            BEGIN
            RXData.QTHString := SecondString;
            IF ThirdString <> '' THEN
                NumberString := FirstString + ' ' + ThirdString
            ELSE
                NumberString := FirstString;
            END
        ELSE
            BEGIN
            RXData.QTHString := ThirdString;
            NumberString := FirstString + ' ' + SecondString;
            END;

    IF ProcessRSTAndQSONumberExchange (NumberString, RXData) THEN
        IF GoodLookingGrid (RXData.QTHString) THEN
            ProcessRSTQSONumberAndGridSquareExchange := FoundDomesticQTH (RXData);
    END;



FUNCTION ProcessRSTQSONumberAndPossibleDomesticQTHExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ Each entry of the exchange must be separated by spaces.  The following
  formats work:

   Entry  #1   Entry  #2   Entry  #3
   ---------   ---------   ---------
   QSO Number                             (RS(T) = default, No QTH)
   QTH                                    (RS(T) = default, QSO number = 1)
   QTH         QSO Number                 (RS(T) = default)
   QTH         RS(T)       QSO Number
   QSO Number  QTH                        (RS(T) = default)
   RS(T)       QTH         QSO Number
   RS(T)       QSO Number                 (No QTH)
   RS(T)       QSO Number  QTH

  You can enter just the strength of the RS(T) if desired.      }

VAR NumberString, FirstString, SecondString, ThirdString: Str20;

    BEGIN
    ProcessRSTQSONumberAndPossibleDomesticQTHExchange := False;
    IF Exchange = '' THEN Exit;

    ParseExchange (Exchange, FirstString, SecondString, ThirdString);

    IF NOT StringIsAllNumbers (FirstString) THEN
        BEGIN
        RXData.QTHString := FirstString;

        IF ThirdString <> '' THEN
            NumberString := SecondString + ' ' + ThirdString
        ELSE
            NumberString := SecondString;
        END
    ELSE
        IF NOT StringIsAllNumbers (SecondString) THEN
            BEGIN
            RXData.QTHString := SecondString;
            IF ThirdString <> '' THEN
                NumberString := FirstString + ' ' + ThirdString
            ELSE
                NumberString := FirstString;
            END
        ELSE
            BEGIN
            RXData.QTHString := ThirdString;
            NumberString := FirstString + ' ' + SecondString;
            END;

    IF ProcessRSTAndQSONumberExchange (NumberString, RXData) THEN
        IF RXData.QTHString = '' THEN
            ProcessRSTQSONumberAndPossibleDomesticQTHExchange := True
        ELSE
            ProcessRSTQSONumberAndPossibleDomesticQTHExchange := FoundDomesticQTH (RXData);
    END;

{KK1L: 6.73 For Michigan QSO Party NOT WORKING!!!!!!}
FUNCTION ProcessQSONumberAndPossibleDomesticQTHExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ Each entry of the exchange must be separated by spaces.  The following
  formats work:

   Entry  #1   Entry  #2   Entry  #3
   ---------   ---------   ---------
   QSO Number                             (No QTH)
   QTH                                    (QSO number = 1)
   QTH         QSO Number
   QSO Number  QTH                                            }

VAR NumberString, FirstString, SecondString, ThirdString: Str20;

    BEGIN
    ProcessQSONumberAndPossibleDomesticQTHExchange := False;
    IF Exchange = '' THEN Exit;

    ParseExchange (Exchange, FirstString, SecondString, ThirdString);

    IF NOT StringIsAllNumbers (FirstString) THEN
        BEGIN
        RXData.QTHString := FirstString;
        NumberString := SecondString;
        END
    ELSE
        IF NOT StringIsAllNumbers (SecondString) THEN
            BEGIN
            RXData.QTHString := SecondString;
            NumberString := FirstString;
            END;

    IF StringIsAllNumbers (NumberString) THEN
        IF RXData.QTHString = '' THEN
            ProcessQSONumberAndPossibleDomesticQTHExchange := True
        ELSE
            ProcessQSONumberAndPossibleDomesticQTHExchange := FoundDomesticQTH (RXData);
    END;



FUNCTION ProcessNZFieldDayExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ Each entry of the exchange must be separated by spaces.  The RST is
  optional.  The following formats will work:

  If in New Zealand:

   Entry  #1   Entry  #2   Entry  #3
   ---------   ---------   ---------
   RST         QSO Number  Zone
   QSO Number  Zone

  You can enter just the strength of the RS(T) if desired.

  If not in New Zealand:

   Entry  #1   Entry  #2
   ---------   ---------
   RST         QSO Number
   QSO Number

    }

VAR FirstString, SecondString, ThirdString: Str20;

    BEGIN
    ProcessNZFieldDayExchange := False;
    IF Exchange = '' THEN Exit;

    IF CountryTable.GetCountryId (RXData.QTH.Country) = 'ZL' THEN
        BEGIN
        ParseExchange (Exchange, FirstString, SecondString, ThirdString);

        IF ThirdString <> '' THEN
            BEGIN
            IF Length (ThirdString) > 2 THEN Exit;
            RXData.Zone := ThirdString;
            END
        ELSE
            BEGIN
            IF (SecondString = '') OR (Length (SecondString) > 2) THEN Exit;
            RXData.Zone := SecondString;
            IF Length (RXData.Zone) = 1 THEN RXData.Zone := '0' + RXData.Zone;
            END;

        IF Length (RXData.Zone) = 1 THEN RXData.Zone := '0' + RXData.Zone;
        RemoveLastString (Exchange);

        IF NOT StringIsAllNumbers (RXData.Zone) THEN Exit;
        END;

    IF ProcessRSTAndQSONumberExchange (Exchange, RXData) THEN
        ProcessNZFieldDayExchange := True;
    END;



FUNCTION ProcessRSTQTHNameAndFistsNumberOrPowerExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ 599 QTH NAME FISTS# or 599 QTH NAME POWER.  Assumed to be FISTS # unless
  the number has a W at the end of it.  RST is optional.  QTH is required
  even for DX.  Name can only be one word.  Spaces required for each entry. }

VAR NumberEntries, Address: INTEGER;
    DX: BOOLEAN;
    EntryList: EntryArray;

    BEGIN
    ProcessRSTQTHNameAndFistsNumberOrPowerExchange := False;

    IF Length (Exchange) < 4 THEN
        BEGIN
        ExchangeErrorMessage := 'Not enough info in exchange!!';
        Exit;
        END;

    DX := NOT DomesticCountryCall (RXData.Callsign);

    ParseExchangeIntoFields (Exchange, EntryList, NumberEntries);

    IF StringIsAllNumbers (EntryList [1]) THEN
        BEGIN
        RXData.RSTReceived := EntryList [1];

        { Remove the RST from the entry list }

        IF NumberEntries > 1 THEN
            FOR Address := 1 TO NumberEntries - 1 DO
                EntryList [Address] := EntryList [Address + 1];
        Dec (NumberEntries);
        END
    ELSE
        RXData.RSTReceived := '599';  { Use default RST }

    { We have removed the RST if it was there.  Should have QTH, Name
      and power or member # }

    IF NumberEntries < 2 THEN Exit; { Not enough data }

    IF (NumberEntries = 2) AND DX THEN
        BEGIN
        RXData.QTHString := RXData.QTH.CountryID;
        RXData.Name := EntryList [1];

        IF NumberEntries > 1 THEN
            FOR Address := 1 TO NumberEntries - 1 DO
                EntryList [Address] := EntryList [Address + 1];
        Dec (NumberEntries);
        END
    ELSE
        BEGIN
        RXData.QTHString := EntryList [1];
        RXData.Name      := EntryList [2];

        FOR Address := 1 TO NumberEntries - 2 DO
                EntryList [Address] := EntryList [Address + 2];
        NumberEntries := NumberEntries - 2;
        END;

    { We should now just have the power or member number left }

    IF NumberEntries <> 1 THEN Exit;

    IF StringIsAllNumbers (EntryList [1]) THEN { member number }
        Val (EntryList [1], RXData.NumberReceived)
    ELSE
        RXData.Power := EntryList [1];

    IF DX THEN
        ProcessRSTQTHNameAndFistsNumberOrPowerExchange := True
    ELSE
        ProcessRSTQTHNameAndFistsNumberOrPowerExchange := FoundDomesticQTH (RXData);
    END;



FUNCTION ProcessRSTQSONumberAndRandomCharactersExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ Each entry of the exchange must be separated by spaces.  The following
  formats work:

   Entry  #1   Entry  #2   Entry  #3
   ---------   ---------   ---------
   Chars                                  (RS(T) = default, QSO number = 1)
   Chars       QSO Number                 (RS(T) = default)
   Chars       RS(T)       QSO Number
   QSO Number  Chars                      (RS(T) = default)
   RS(T)       Chars       QSO Number
   RS(T)       QSO Number  Chars

  You can enter just the strength of the RS(T) if desired.      }

VAR NumberString, FirstString, SecondString, ThirdString: Str20;

    BEGIN
    ProcessRSTQSONumberAndRandomCharactersExchange := False;
    IF Exchange = '' THEN Exit;

    ParseExchange (Exchange, FirstString, SecondString, ThirdString);

    IF NOT StringIsAllNumbers (FirstString) THEN
        BEGIN
        RXData.RandomCharsReceived := FirstString;
        IF ThirdString <> '' THEN
            NumberString := SecondString + ' ' + ThirdString
        ELSE
            NumberString := SecondString;
        END
    ELSE
        IF NOT StringIsAllNumbers (SecondString) THEN
            BEGIN
            RXData.RandomCharsReceived := SecondString;
            IF ThirdString <> '' THEN
                NumberString := FirstString + ' ' + ThirdString
            ELSE
                NumberString := FirstString;
            END
        ELSE
            BEGIN
            RXData.RandomCharsReceived := ThirdString;
            NumberString := FirstString + ' ' + SecondString;
            END;

    IF ProcessRSTAndQSONumberExchange (NumberString, RXData) THEN
        ProcessRSTQSONumberAndRandomCharactersExchange := (Length (RXData.RandomCharsReceived) = 5) AND
                                                          (Length (RXData.RandomCharsSent) = 5);

    END;



FUNCTION ProcessRSTAndPostalCodeExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ Legal entries HA1 1TA, RS(T) HA1 1TA or HA1 1TA RS(T) }

VAR FirstString, SecondString, ThirdString: Str20;

    BEGIN
    ProcessRSTAndPostalCodeExchange := False;
    IF Exchange = '' THEN Exit;

    ParseExchange (Exchange, FirstString, SecondString, ThirdString);

    IF NOT StringIsAllNumbers (FirstString) THEN
        BEGIN
        RXData.PostalCode  := FirstString + ' ' + SecondString;
        RXData.RSTReceived := ThirdString;

        IF RXData.RSTReceived = '' THEN
            RXData.RSTReceived := DefaultRST;
        END
    ELSE
        BEGIN
        RXData.PostalCode  := SecondString + ' ' + ThirdString;
        RXData.RSTReceived := FirstString;
        END;

    RXData.QTHString := Copy (RXData.PostalCode, 1, 2);

    ProcessRSTAndPostalCodeExchange := FoundDomesticQTH (RXData);
    END;



FUNCTION ProcessRSTAndQSONumberOrDomesticQTHExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

    BEGIN
    IF (Copy (RXData.Callsign, 1, 3) = 'VE0') AND
       (ActiveQSOPointMethod = RACQSOPointMethod) THEN
           BEGIN
           ProcessRSTAndQSONumberOrDomesticQTHExchange := ProcessRSTAndQSONumberExchange (Exchange, RXData);
           Exit;
           END;

    IF DomesticCountryCall (RXData.Callsign) THEN
        ProcessRSTAndQSONumberOrDomesticQTHExchange := ProcessRSTAndDomesticQTHExchange (Exchange, RXData)
    ELSE
        ProcessRSTAndQSONumberOrDomesticQTHExchange := ProcessRSTAndQSONumberExchange (Exchange, RXData);
    END;



FUNCTION ProcessRSTAndQTHExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

VAR RSTString: Str20;

    BEGIN
    GetRidOfPrecedingSpaces (Exchange);
    GetRidOfPostcedingSpaces (Exchange);

    IF Exchange = '' THEN
        BEGIN
        ProcessRSTAndQTHExchange := False;
        Exit;
        END;

    ProcessRSTAndQTHExchange := True;

    RSTString := PrecedingString (Exchange, ' ');

    IF StringIsAllNumbersOrSpaces (RSTString) THEN
        BEGIN
        RXData.RSTReceived := RSTString;
        RXData.QTHString   := PostcedingString (Exchange, ' ');
        END
    ELSE
        BEGIN
        RSTString := Exchange;

        WHILE StringHas (RSTString, ' ') DO
            RSTString := PostcedingString (RSTString, ' ');

        IF StringIsAllNumbersOrSpaces (RSTString) THEN
            BEGIN
            RXData.RSTReceived := RSTString;
            RXData.QTHString := Copy (Exchange, 1, Length (Exchange) - Length (RSTString) - 1);
            END
        ELSE
            BEGIN
            RXData.RSTReceived := DefaultRST;
            RXData.QTHString := Exchange;
            END;
        END;
    END;



FUNCTION ProcessRSTZoneAndPossibleDomesticQTHExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ Each entry of the exchange must be separated by spaces.  The following
  formats work:

   Entry  #1   Entry  #2   Entry  #3
   ---------   ---------   ---------
   RS(T)       Zone        QTH
   RS(T)       Zone                      (No QTH)
   Zone                                  (RS(T) = default, no QTH)
   Zone        QTH                       (RS(T) = default)

  You can enter just the strength of the RS(T) if you like.       }

VAR FirstString, SecondString, ThirdString: Str20;

    BEGIN
    ProcessRSTZoneAndPossibleDomesticQTHExchange := False;
    IF Exchange = '' THEN Exit;

    RXData.QTHString := '';

    ParseExchange (Exchange, FirstString, SecondString, ThirdString);

    IF ThirdString <> '' THEN
        BEGIN
        IF NOT ValidRST (FirstString, RXData.RSTReceived, ActiveMode) THEN Exit;
        RXData.Zone      := SecondString;
        RXData.QTHString := ThirdString;
        END
    ELSE
        IF SecondString <> '' THEN
            BEGIN
            IF NOT StringIsAllNumbers (SecondString) THEN
                BEGIN
                RXData.RSTReceived := DefaultRST;
                RXData.Zone        := FirstString;
                RXData.QTHString   := SecondString;
                END
            ELSE
                BEGIN
                IF NOT ValidRST (FirstString, RXData.RSTReceived, ActiveMode) THEN Exit;
                RXData.Zone := SecondString;
                END;
            END
        ELSE
            BEGIN
            RXData.RSTReceived := DefaultRST;
            RXData.Zone := Exchange;
            END;

    IF NOT StringIsAllNumbers (RXData.Zone) THEN Exit;

    IF Length (RXData.Zone) = 1 THEN RXData.Zone := '0' + RXData.Zone;

    IF RXData.QTHString <> '' THEN
        ProcessRSTZoneAndPossibleDomesticQTHExchange := FoundDomesticQTH (RXData)
    ELSE
        ProcessRSTZoneAndPossibleDomesticQTHExchange := True;
    END;

FUNCTION ProcessRSTAndDomesticQTHOrZoneExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ Each entry of the exchange must be separated by spaces.  The following
  formats work:

   Entry  #1   Entry  #2
   ---------   ---------
   RS(T)       QTH
   RS(T)       Zone
   Zone                                  (RS(T) = default, no QTH)
   QTH                                   (RS(T) = default)
   QTH         RS(T)
   QTH         RST                       Need three digits of RST }


VAR FirstString, SecondString, ThirdString: Str20;

    BEGIN
    ProcessRSTAndDomesticQTHOrZoneExchange := False;
    IF Exchange = '' THEN Exit;

    RXData.QTHString := '';

    ParseExchange (Exchange, FirstString, SecondString, ThirdString);

    IF ThirdString <> '' THEN
        BEGIN
        FirstString := SecondString;
        SecondString := ThirdString;
        END;

    IF SecondString <> '' THEN
        BEGIN
        IF NOT StringIsAllNumbers (SecondString) THEN  { SecondString = QTH }
            BEGIN
            IF NOT ValidRST (FirstString, RXData.RSTReceived, ActiveMode) THEN Exit;
            RXData.QTHString   := SecondString;
            ProcessRSTAndDomesticQTHOrZoneExchange := FoundDomesticQTH (RXData);
            END
        ELSE
            BEGIN
            IF NOT ValidRST (SecondString, RXData.RSTReceived, ActiveMode) THEN Exit;
            RXData.Zone := FirstString;
            ProcessRSTAndDomesticQTHOrZoneExchange := StringIsAllNumbers (FirstString) AND
                                                              (Length (FirstString) <= 2);
            END;

        Exit;
        END
    ELSE
        BEGIN
        RXData.RSTReceived := DefaultRST;

        IF StringIsAllNumbers (FirstString) THEN  { Zone }
            BEGIN
            RXData.Zone := FirstString;
            ProcessRSTAndDomesticQTHOrZoneExchange := StringIsAllNumbers (FirstString) AND
                                                              (Length (FirstString) <= 2);
            END
        ELSE
            BEGIN
            RXData.QTHString := FirstString;
            ProcessRSTAndDomesticQTHOrZoneExchange := FoundDomesticQTH (RXData);
            END;

        Exit;
        END;

    END;

FUNCTION ProcessRSTAndPowerExchange (Exchange:Str80; VAR RXData: ContestExchange): BOOLEAN;

{ Each entry of the exchange must be separated by spaces.  The following
  formats work:

   Entry  #1   Entry  #2
   ---------   ---------
   RS(T)       Power
   Power                     (RS(T) = default)

  You can enter just the strength of the RS(T) if you like.       }

    BEGIN
    ProcessRSTAndPowerExchange := False;

    IF StringHas (Exchange, ' ') THEN
        BEGIN
        IF NOT ValidRST (Exchange, RXData.RSTReceived, ActiveMode) THEN Exit;
        GetRidOfPrecedingSpaces (Exchange);
        RXData.Power := Exchange;
        END
    ELSE
        BEGIN
        RXData.RSTReceived := DefaultRST;
        RXData.Power := Exchange;
        END;

    ProcessRSTAndPowerExchange := RXData.Power <> '';
    END;



FUNCTION ProcessRSTAndZoneExchange (Exchange: Str80; VAR RXData: ContestExchange): BOOLEAN;

{ Each entry of the exchange must be separated by spaces.  The following
  formats work:

   Entry  #1   Entry  #2
   ---------   ---------
   RS(T)       Zone
   Zone                       (RS(T) = default)
   Zone        RST            (RST must be three characters, CW only)

  You can enter just the strength of the RS(T) if you like.    }

VAR FirstString, SecondString, ThirdString: Str20;

    BEGIN
    ProcessRSTAndZoneExchange := False;
    IF NOT StringIsAllNumbersOrSpaces (Exchange) THEN Exit;
    RXData.RSTReceived := DefaultRST;

    IF StringHas (Exchange, ' ') THEN
        BEGIN
        IF ActiveMode = CW THEN
            BEGIN
            ParseExchange (Exchange, FirstString, SecondString, ThirdString);

            IF (Length (SecondString) = 3) AND (Length (FirstString) < 3) THEN
                BEGIN
                RXData.RSTReceived := SecondString;
                RXData.Zone := FirstString;
                IF Length (RXData.Zone) = 1 THEN
                    Insert ('0', RXData.Zone, 1);
                ProcessRSTAndZoneExchange := True;
                Exit;
                END;
            END;

        IF NOT ValidRST (Exchange, RXData.RSTReceived, ActiveMode) THEN Exit;
        GetRidOfPrecedingSpaces (Exchange);
        END;

    CASE Length (Exchange) OF
        1: BEGIN
           RXData.Zone := '0' + Exchange;
           ProcessRSTAndZoneExchange := True;
           END;

        2: BEGIN
           RXData.Zone := Exchange;

           IF ActiveZoneMult = EUHFCYear THEN
               BEGIN
               ProcessRSTAndZoneExchange := True;
               Exit;
               END;

           IF (CountryTable.ZoneMode = CQZoneMode) AND (ActiveZoneMult <> JAPrefectures) THEN
               IF (Exchange [1] > '5') OR ((Exchange [1] = '4') AND (Exchange [2] >= '1')) THEN
                   Exit;

           IF ActiveZoneMult = JAPrefectures THEN
               IF (Exchange [1] > '6') OR ((Exchange [1] = '5') AND (Exchange [2] >= '1')) THEN
                   Exit;

           ProcessRSTAndZoneExchange := True;
           END;

        END;
    END;



PROCEDURE SaveLogFileToFloppy;

    BEGIN
    IF FloppyFileSaveName = '' THEN Exit;
    SaveSetAndClearActiveWindow (QuickCommandWindow);
    Write ('Saving ', LogFileName, ' to ', FloppyFileSaveName);
    RestorePreviousWindow;

    IF NOT CopyFile (LogFileName, FloppyFileSaveName) THEN
        BEGIN
        IF IOResult = 0 THEN;

        IF StringHas (MyCall, 'K9NX') THEN
            BEGIN
            CASE Random (5) OF
                0: QuickDisplay ('Duh!!!!  Check the drive Dave.');
                1: QuickDisplay ('duhhhhh ....bonehead you forgot to put the floppy in the drive.');
                2: QuickDisplay ('La disque de floppy n''est pas marche!!');
                3: QuickDisplay ('I''m sorry Dave.  I''m afraid I can''t do that.');
                4: QuickDisplay ('Flippy sove fialed!!  Chack Dave.');
                END;
            END
        ELSE
            QuickDisplay ('Floppy save failed!!  Check drive.');

        Tone.DoABeep (Warning);
        ReminderPostedCount := 30;
        END
    ELSE
        QuickDisplay ('File saved to floppy successfully');
    END;



PROCEDURE RememberRadioFrequency (Radio: RadioType);

    BEGIN
    IF (Radio = RadioOne) AND (Radio1ControlPort <> nil) OR
       (Radio = RadioTwo) AND (Radio2ControlPort <> nil) THEN
           BEGIN
           WITH Remember [Radio] DO
               IF NOT GetRadioParameters (Radio, '', Frequency, Band, Mode, FALSE, FALSE) THEN
                   Frequency := 0;
           Exit;
           END;

    Remember [Radio].Frequency := 0;
    END;



PROCEDURE RestoreRadioFrequency (Radio: RadioType);

    BEGIN
    IF Remember [Radio].Frequency = 0 THEN Exit;
    WITH Remember [Radio] DO SetRadioFreq (Radio, Frequency, Mode, 'A');
    END;



PROCEDURE ReviewBackCopyFiles;

VAR LastFile: Str40;
    Command: Str20;

    BEGIN
    SaveSetAndClearActiveWindow (BigWindow);
    LastFile := '';

    REPEAT
        QuickDisplay ('Use cursor to select the file you want to process and press RETURN');
        ClrScr;
        WriteLnCenter ('BACKCOPY FILE REVIEW');
        WriteLn;
        WriteLn ('Select backcopy file to review or delete : ');

        LastFile := ShowDirectoryAndSelectFile ('*.bcp', '', False);

        IF (LastFile = '') OR (LastFile = EscapeKey) THEN
            BEGIN
            RestorePreviousWindow;
            VisibleDupesheetRemoved := True;
            Exit;
            END;

        REPEAT
            Command := UpperCase (QuickEditResponse (LastFile + ': R to review   D to delete  ESCAPE to quit', 1));
            IF Command = 'R' THEN DVPListenMessage (LastFile, False);
            IF Command = 'D' THEN DeleteFile (LastFile);
        UNTIL (Command = '') OR (Command = 'D') OR (Command = EscapeKey);

    UNTIL False;
    END;



FUNCTION GetCorrectedCallFromExchangeString (VAR ExchangeString: Str80): Str80;

VAR PotentialCall, TempString: Str40;

    BEGIN
    GetCorrectedCallFromExchangeString := '';

    TempString := ExchangeString;

    IF CallsignUpdateEnable AND StringHas (TempString, ' ') THEN
        WHILE TempString <> '' DO
            BEGIN
            PotentialCall := RemoveLastString (TempString);

            IF LooksLikeACallsign (PotentialCall) THEN
                BEGIN
                GetCorrectedCallFromExchangeString := PotentialCall;

                { Delete all the stuff we have }

                Delete (ExchangeString, 1, Length (TempString));

                { Remove the callsign }

                RemoveFirstString (ExchangeString);

                { Rebuild the exchange string without the callsign }

                ExchangeString := TempString + ' ' + ExchangeString;
                Exit;
                END;
            END;
    END;



FUNCTION ISentThisMultiMessage (MultMessage: STRING): BOOLEAN;

{ Works for both N6TR and K1EA network }

VAR LookAddress: INTEGER;
    S1: STRING;

    BEGIN
    ISentThisMultiMessage := False;

    IF NetDebug THEN
        BEGIN
        SaveSetAndClearActiveWindow (DupeSheetWindow);

        WriteLn ('ISentThisMultiMessage called with : ');

        IF K1EANetworkEnable THEN
            WriteLn (Copy (MultMessage, 1, Length (MultMessage) - 1))
        ELSE
            WriteLn (Copy (MultMessage, 10, Ord (MultMessage [8])));

        WriteLn;
        WriteLn ('The following messages are in the message list : ');
        END;


    IF K1EANetworkEnable THEN  { K1EA network }
        BEGIN
        IF MultMessage [2] = K1EAStationID THEN
            BEGIN
            ISentThisMultiMessage := True;

            IF NetDebug THEN
                BEGIN
                WriteLn ('I found my station ID - found!');
                RestorePreviousWindow;
                Exit;
                END;

            END
        ELSE
            IF NetDebug THEN
                WriteLn ('This is not my message');
        END

    ELSE  { N6TR Network }

        BEGIN
        { See if there is anything in the list to check }

        IF FirstMultiMessage = LastMultiMessage THEN
            BEGIN
            IF NetDebug THEN
                BEGIN
                WriteLn ('Nothing in the remember list.');
                RestorePreviousWindow;
                END;
            Exit;
            END;

        LookAddress := LastMultiMessage;

        WHILE LookAddress <> FirstMultiMessage DO
            BEGIN
            S1 := MultiRememberBuffer^ [LookAddress].Message;

            IF NetDebug AND (NOT MultiRememberBuffer^ [LookAddress].QSL) THEN
                WriteLn (Copy (S1, 10, Ord (S1 [8])));

            IF (Copy (S1, 1, 5) = Copy (MultMessage, 1, 5)) AND
               (S1 [8] = MultMessage [8]) AND
               (Copy (S1, 10, Ord (S1 [8])) = Copy (MultMessage, 10, Ord (S1 [8]))) THEN
                   BEGIN
                   IF NetDebug THEN
                       BEGIN
                       WriteLn ('Found it!!');
                       RestorePreviousWindow;
                       END;

                   MultiRememberBuffer^ [LookAddress].QSL := True;

                   ISentThisMultiMessage := True;
                   Exit;
                   END;

            LookAddress := (LookAddress + 1) MOD MultiMessageBufferSize;
            END;
        END;

    IF NetDebug THEN
        BEGIN
        WriteLn ('Entry not found.');
        RestorePreviousWindow;
        END;
    END;



PROCEDURE CheckForLostMultiMessages;

{ This procedure will look at the MultiMessageMemory and see if there are
  any entries that should either be resent or deleted.  It will stop after
  it finds one to send so it doesn't hog the computer too long. }

VAR LookAddress: INTEGER;

    BEGIN
    LookAddress := LastMultiMessage;

    WHILE LookAddress <> FirstMultiMessage DO
        BEGIN
        WITH MultiRememberBuffer^ [LookAddress] DO
            IF NOT QSL THEN
                BEGIN
                IF RetryCount <= 10 THEN
                    BEGIN

                    IF ((RetryCount <  2) AND (ElaspedSec100 (TimeMark) > MultiRetryTime * 100) OR
                        (RetryCount >= 2) AND (ElaspedSec100 (TimeMark) > 3000)) THEN
                            BEGIN
                            IF RetryCount >= 3 THEN Tone.DoABeep (Single);
                            SendMultiMessage (Message);
                            MarkTime (TimeMark);
                            Inc (RetryCount);
                            END;
                    END;

                IF (RetryCount >= 5) AND (Warnings = 0) THEN
                    BEGIN
                    Tone.DoABeep (Warning);
                    QuickDisplay ('Please check multi network');
                    ReminderPostedCount := 60;
                    Warnings := 1;
                    END;

                IF (RetryCount > 10) AND (Warnings = 1) THEN
                    BEGIN
                    Tone.DoABeep (Warning);
                    QuickDisplay ('Possible data loss on multi network.');
                    ReminderPostedCount := 60;
                    Warnings := 2;
                    END;

                END;

        LookAddress := (LookAddress + 1) MOD MultiMessageBufferSize;
        END;
    END;



FUNCTION ValidCheckSum (Message: STRING): BOOLEAN;

{ Returns TRUE if the check sum looks valid for the message.  Works for
  both K1EA and N6TR network. }

VAR CharPointer: INTEGER;
    CheckSum:    WORD;
    K1EACheckSum: BYTE;
    TempString: STRING;

    BEGIN
    IF Length (Message) < 8 THEN
        BEGIN
        ValidCheckSum := False;
        Exit;
        END;

    IF K1EANetworkEnable THEN  { K1EA network }
        BEGIN
        K1EACheckSum := 0;

        { We count up to all but the last character }

        FOR CharPointer := 1 TO Length (Message) - 1 DO
            K1EACheckSum := K1EACheckSum + Ord (Message [CharPointer]);

        K1EACheckSum := K1EACheckSum OR $80;

        ValidCheckSum := K1EACheckSum = Ord (Message [Length (Message)]);

{       ValidCheckSum := True;}

        IF K1EACheckSum <> Ord (Message [Length (Message)]) THEN
            IF NetDebug THEN
                BEGIN
                TempString := '*** NOT VALID CHECKSUM BUT USED ANYWAY ***';
                BlockWrite (NetDebugBinaryInput, TempString [1], Length (TempString));
                END;
        END

    ELSE         { TR network }
        BEGIN
        CheckSum := 0;

        FOR CharPointer := 1 TO 5 DO
            CheckSum := CheckSum + Ord (Message [CharPointer]);

        FOR CharPointer := 8 TO Length (Message) DO
            CheckSum := CheckSum + Ord (Message [CharPointer]);

        ValidCheckSum := (Hi (CheckSum) = Ord (Message [6])) AND
                         (Lo (CheckSum) = Ord (Message [7]));
        END;
    END;



FUNCTION TimeToDie (VAR Message: STRING): BOOLEAN;

{ This function will decrement the hop count in the message.  If it has
  expired, it will return TRUE.  Will always return False if we are in
  the K1EANetworkMode }

VAR TimeToLive: BYTE;
    CheckSum: WORD;

    BEGIN
    IF K1EANetworkEnable THEN
        BEGIN
        TimetoDie := False;
        Exit;
        END;

    TimeToLive := Ord (Message [9]);
    Dec (TimeToLive);

    IF TimeToLive <= 0 THEN
        BEGIN
        TimeToDie := True;
        Exit;
        END;

    TimeToDie := False;
    Message [9] := Chr (TimeToLive);

    CheckSum := Ord (Message [6]);
    CheckSum := Swap (CheckSum);
    CheckSum := CheckSum + Ord (Message [7]);
    Dec (CheckSum);
    Message [6] := Chr (Hi (CheckSum));
    Message [7] := Chr (Lo (CheckSum));
    END;



FUNCTION ThisMessageIsForMe (Message: STRING): BOOLEAN;

{ This function will return TRUE if the message is addressed to us or is
  a broadcast message. }

VAR Destination: BYTE;

    BEGIN
    IF NOT K1EANetworkEnable THEN  { N6TR Network mode }
        BEGIN
        Destination := Ord (Message [2]);

        ThisMessageIsForMe := (Destination = $FF) OR
                              (Destination = MultiBandAddressArray [ActiveBand]);
        END
    ELSE
        { At this point, I am not aware of any commands on the K1EA network
          that would not be for me.  This might change as I get into it
          more }

        ThisMessageIsForMe := True;
    END;



FUNCTION WeHaveProcessedThisMessage (Message: STRING): BOOLEAN;

{ This function will look at the message passed to it and see if we have
  seen this message before.  If we have, it will return TRUE.  If not, it
  will add it to the list of messages we have seen and return FALSE. }


VAR Source, Serial: BYTE;
    ActiveMessage:  INTEGER;
    CheckSum:       WORD;

    BEGIN
    IF K1EANetworkEnable THEN { Do not expect repeat messages without retries }
        BEGIN
        WeHaveProcessedThisMessage := False;
        Exit;
        END

    ELSE
        BEGIN                          { N6TR Network Mode }
        Source := Ord (Message [1]);
        Serial := Ord (Message [5]);

        CheckSum := Ord (Message [6]);
        CheckSum := Swap (CheckSum);
        CheckSum := CheckSum + Ord (Message [7]);

        CheckSum := CheckSum - Ord (Message [9]); { Make sure TTL isn't a factor }
        END;

    { See if we have a virgin list.  If so, add the entry }

    IF ProcessedMultiMessagesStart = ProcessedMultiMessagesEnd THEN
        BEGIN
        WeHaveProcessedThisMessage := False;
        ProcessedMultiMessages [ProcessedMultiMessagesStart].Source := Source;
        ProcessedMultiMessages [ProcessedMultiMessagesStart].Serial := Serial;
        ProcessedMultiMessages [ProcessedMultiMessagesStart].Check  := CheckSum;
        Inc (ProcessedMultiMessagesEnd);             { Increments to one }
        Exit;
        END;

    { Start at the end of the list }

    ActiveMessage := ProcessedMultiMessagesEnd - 1;

    IF ActiveMessage < 0 THEN
        ActiveMessage := ProcessedMultiMessageBufferLength - 1;

    REPEAT
        IF (ProcessedMultiMessages [ActiveMessage].Source = Source) AND
           (ProcessedMultiMessages [ActiveMessage].Serial = Serial) AND
           (ProcessedMultiMessages [ActiveMessage].Check  = CheckSum) THEN
               BEGIN
               WeHaveProcessedThisMessage := True;
               Exit;
               END;

        IF ActiveMessage = ProcessedMultiMessagesStart THEN  { End of list }
            BEGIN
            ActiveMessage := ProcessedMultiMessagesEnd;
            ProcessedMultiMessages [ActiveMessage].Source := Source;
            ProcessedMultiMessages [ActiveMessage].Serial := Serial;
            ProcessedMultiMessages [ActiveMessage].Check  := CheckSum;

            Inc (ProcessedMultiMessagesEnd);

            IF ProcessedMultiMessagesEnd >= ProcessedMultiMessageBufferLength THEN
                ProcessedMultiMessagesEnd := 0;

            IF ProcessedMultiMessagesEnd = ProcessedMultiMessagesStart THEN
                BEGIN
                Inc (ProcessedMultiMessagesStart);

                IF ProcessedMultiMessagesStart >= ProcessedMultiMessageBufferLength THEN
                    ProcessedMultiMessagesStart := 0;
                END;

            WeHaveProcessedThisMessage := False;
            Exit;
            END;

        Dec (ActiveMessage);

        IF ActiveMessage < 0  THEN
            ActiveMessage := ProcessedMultiMessageBufferLength - 1;

    UNTIL False;
    END;



FUNCTION ThisIsAMultiTalkMessage (MessageString: STRING): BOOLEAN;

    BEGIN
    IF K1EANetworkEnable THEN
        ThisIsAMultiTalkMessage := MessageString [1] = 'T'
    ELSE
        ThisIsAMultiTalkMessage := Ord (MessageString [3]) = MultiTalkMessage;
    END;



FUNCTION GetMultiPortCommand: STRING;

{ This will retrieve a multi command string from the MultiReceiveCharBuffer.
  It works for either K1EA or N6TR network modes!! }

VAR TempString: STRING;

    BEGIN
    GetMultiPortCommand := '';

    IF K1EANetworkEnable THEN
        BEGIN
        IF NOT MultiReceiveCharBuffer.GetNextLine (TempString) THEN
            Exit;
        END
    ELSE  { TR Network }
        IF NOT MultiReceiveCharBuffer.GetSlippedString (TempString) THEN
            Exit;

    IF TempString = '' THEN Exit;

    IF NetDebug THEN
        BlockWrite (NetDebugBinaryInput, TempString [1], Length (TempString));

    IF NOT ValidCheckSum (TempString) THEN { Works for TR and CT }
        BEGIN
        IF NetDebug THEN
            BEGIN
            TempString := '*** NOT VALID CHECKSUM ***';
            BlockWrite (NetDebugBinaryInput, TempString [1], Length (TempString));
            END;

        Exit;
        END;

    { Okay, we have a valid message to deal with.  If it is one of ours,
      we will ignore it, unless it was MultiTalkMessage }

    IF ISentThisMultiMessage (TempString) THEN        { It is one of ours }
        BEGIN
        IF ThisMessageIsForMe (TempString) THEN
            IF NOT WeHaveProcessedThisMessage (TempString) THEN
                IF ThisIsAMultiTalkMessage (TempString) THEN
                    GetMultiPortCommand := TempString; { Process my own message }

        Exit;
        END;

    { If this message has been hoping around too long, ignore it }

    IF TimeToDie (TempString) THEN Exit;

    { Okay, we have a valid message, and it isn't ours, pass it on. }

    IF K1EANetworkEnable THEN
        { LineFeed Makes SendMultiMessage not add checksum or LF }

        SendMultiMessage (TempString + LineFeed)
    ELSE
        SendMultiMessage (TempString);

    IF NOT ThisMessageIsForMe (TempString) THEN Exit;

    { We have a message for me to act on (unless I have already seen it) }

    IF WeHaveProcessedThisMessage (TempString) THEN
        BEGIN
        IF NetDebug THEN
            BEGIN
            TempString := '*** PROCESSED BEFORE ***';
            BlockWrite (NetDebugBinaryInput, TempString [1], Length (TempString));
            END;

        Exit;
        END;

    GetMultiPortCommand := TempString;
    END;



PROCEDURE RememberSentMessage (MultMessage: STRING);

{ This procedure will take the message that was sent and remember it along
  with a time stamp, so that if it doesn't get QSLed after a certain amount
  of time, the message can be resent. }

    BEGIN
    WITH MultiRememberBuffer^ [FirstMultiMessage] DO
        BEGIN
        Message := MultMessage;
        MarkTime (TimeMark);
        RetryCount := 0;
        QSL := False;
        Warnings := 0;
        END;

    FirstMultiMessage := (FirstMultiMessage + 1) MOD MultiMessageBufferSize;

    IF FirstMultiMessage = LastMultiMessage THEN
        LastMultiMessage := (LastMultiMessage + 1) MOD MultiMessageBufferSize;

    END;



PROCEDURE SendMultiCommand (Source:       BYTE;
                            Destination:  BYTE;
                            ControlByte:  BYTE;
                            Message:      STRING);

{ This procedure will generate the multi command and pass it to the engine
  in LOGK1EA that sends the bytes to the port.  The format for the message
  is as follows:

    Source:        BYTE;
    Destination:   BYTE;
    ControlByte:   BYTE;
    SerialNumber:  WORD;
    CheckSum:      WORD;
    MessageLength: BYTE;
    TimeToLive:    INTEGER;
    Message:       A bunch of ASCII characters (MessageLength of them);

}


VAR TempString: STRING;
    CheckSum: WORD;
    CharPointer: INTEGER;

    BEGIN
    TempString := Concat (Chr (Source),
                          Chr (Destination),
                          Chr (ControlByte),
                          Chr (Hi (MultiSerialNumber)),
                          Chr (Lo (MultiSerialNumber)),
                          Chr (0),
                          Chr (0),
                          Chr (Length (Message)),
                          Chr (DefaultTimeToLive),
                          Message);

    Inc (MultiSerialNumber);

    CheckSum := 0;

    FOR CharPointer := 1 TO Length (TempString) DO
        CheckSum := CheckSum + Ord (TempString [CharPointer]);

    TempString [6] := Chr (Hi (CheckSum));
    TempString [7] := Chr (Lo (CheckSum));

    SendMultiMessage    (TempString);
    RememberSentMessage (TempString);
    END;



PROCEDURE StuffInit;

    BEGIN
    AddedNoteString            := '';
    AutoQSLCount               := 0;
    AutoQSLInterval            := 0;

    BandMapInfoCall            := '';
    BeSilent                   := False;

    ControlBMemory             := '';
    CodeSpeedIncrement         := 3; {KK1L: 6.72}

    DualingCQState             := NoDualingCQs;

    DVKStamp;
    DVKPlaying                 := False;

    EscapeDeletedCallEntry     := '';
    EscapeDeletedExchangeEntry := '';
    ExchangeHasBeenSent        := False;

    FirstHelloRecord             := Nil;
    FirstMultiMessage            := 0;
    FootSwitchPressedBefore      := False;
    LastMultiMessage             := 0;

    InactiveRigCallingCQ       := False;
    IntercomFileOpen           := False;

    LastDisplayedBreakTime     := -1;

    LastHelloRecord            := Nil;
    LogBadQSOString            := '';
    LookingForCQExchange       := False;

    MultiSerialNumber          := 0;

    NameCallsignPutUp          := '';

    ProcessedMultiMessagesStart := 0;
    ProcessedMultiMessagesEnd   := 0;

    RadioSetFreq               := 0;
    RateDisplay                := QSOs;
    ReadInLogFileOpen          := False;

    SendExchangeKeyWhenCWHasStopped := NullKey;

    SeventyThreeMessageSent    := False;
    StationInformationCall     := '';

    VideoGameLength            := 5;
    VisibleDupeSheetRemoved    := False;

    WindowDupeCheckCall        := '';

    MultiMessageBuffer [1] := '';
    MultiMessageBuffer [2] := '';
    MultiMessageBuffer [3] := '';
    MultiMessageBuffer [4] := '';
    MultiMessageBuffer [5] := '';
    END;



PROCEDURE SpeedUp;

    BEGIN
    {IF CodeSpeed < 96 THEN }
    IF CodeSpeed < (99 - CodeSpeedIncrement) THEN {KK1L: 6.72}
        BEGIN
        {SetSpeed (CodeSpeed + 3);}
        SetSpeed (CodeSpeed + CodeSpeedIncrement); {KK1L: 6.72}
        DisplayCodeSpeed (CodeSpeed, CWEnabled, DVPOn, ActiveMode);
        END;
    END;



PROCEDURE ShowPreviousDupeQSOs (Call: CallString;
                                Band: BandType;
                                Mode: ModeType);

{ This routine will display previous duplicate QSOs with the station
  indicated. }

VAR FileString: Str80;
    FileRead: TEXT;

    BEGIN
    IF Call = '' THEN Exit;

    Call := UpperCase (Call);

    IF OpenFileForRead (FileRead, LogFileName) THEN
        BEGIN
        GridSquareListShown := False;
        SaveSetAndClearActiveWindow (EditableLogWindow);
        WriteLnCenter ('Previous QSOs with ' + Call);

        WHILE (NOT Eof (FileRead)) AND (NOT KeyPressed) DO
            BEGIN
            ReadLn (FileRead, FileString);
            FileString := UpperCase (FileString);

            IF Pos (Call, FileString) > 0 THEN
                IF ((Band = GetLogEntryBand (FileString)) OR NOT QSOByBand) AND
                   ((Mode = GetLogEntryMode (FileString)) OR NOT QSOByMode) THEN
                       WriteLn (FileString);

            END;
        END;

    Close (FileRead);
    RestorePreviousWindow;
    END;



PROCEDURE SlowDown;

    BEGIN
    {IF CodeSpeed > 4 THEN}
    IF CodeSpeed > (CodeSpeedIncrement + 1) THEN {KK1L: 6.72}
        BEGIN
        {SetSpeed (CodeSpeed - 3);}
        SetSpeed (CodeSpeed - CodeSpeedIncrement); {KK1L: 6.72}
        DisplayCodeSpeed (CodeSpeed, CWEnabled, DVPOn, ActiveMode);
        END;
    END;



PROCEDURE NewBandMapEntry (Call: CallString;
                           Frequency: LONGINT;
                           QSXFrequency: LONGINT;
                           Mode: ModeType;
                           Dupe: BOOLEAN;
                           Mult: BOOLEAN;
                           MinutesLeft: INTEGER;
                           SendToMulti: BOOLEAN);

VAR QSXString, FreqString, UnixTimeString: Str20;
    K1EAString: STRING;
    UnixTime: LONGINT;

    BEGIN
    AddBandMapEntry (Call, Frequency, QSXFrequency, Mode, Dupe, Mult, MinutesLeft);

    Str (Frequency, FreqString);
    Str (QSXFrequency, QSXString);

    IF (ActiveMultiPort <> nil) AND SendToMulti THEN
        BEGIN

        IF K1EANetworkEnable THEN
            BEGIN
            UnixTime := GetUnixTime;
            Str (UnixTime, UnixTimeString);

            K1EAString := 'C' + K1EAStationID + ' 599 ' +
                          FreqString + ' ' + QSXString + ' ' +
                          UnixTimeString + ' 0 ' +
                          GetK1EABandIntegerFromFrequency (Frequency);

                          IF ActiveMode = CW THEN
                              K1EAString := K1EAString + ' 1 '
                          ELSE
                              K1EAString := K1EAString + ' 2 ';

                          K1EAString := K1EAString + Call + ' * 0 ' +
                                        K1EAStationID + ' 0  0 ';

            { SendMultiMessage will add the checksum and newline }

            SendMultiMessage (K1EAString);
            END

        ELSE

            { TR Network }

            SendMultiCommand (MultiBandAddressArray [ActiveBand], $FF,
                              MultiBandMapMessage, Call + ' ' + FreqString + ' ' + QSXString);
        END;

    DisplayBandMap;
    END;



PROCEDURE PushMultiMessageBuffer (Message: Str80);

    BEGIN
    MultiMessageBuffer [1] := MultiMessageBuffer [2];
    MultiMessageBuffer [2] := MultiMessageBuffer [3];
    MultiMessageBuffer [3] := MultiMessageBuffer [4];
    MultiMessageBuffer [4] := MultiMessageBuffer [5];
    MultiMessageBuffer [5] := Message;
    END;


PROCEDURE DisplayMultiMessageBuffer;

    BEGIN
    IF ActiveMultiPort <> nil THEN
        BEGIN
        SaveSetAndClearActiveWindow (EditableLogWindow);
        WriteLn (MultiMessageBuffer [1]);
        WriteLn (MultiMessageBuffer [2]);
        WriteLn (MultiMessageBuffer [3]);
        WriteLn (MultiMessageBuffer [4]);
        Write   (MultiMessageBuffer [5]);
        RestorePreviousWindow;
        END;
    END;



PROCEDURE DisplayGridSquareStatus (Call: CallString);

VAR Range, Address: INTEGER;
    Change: BOOLEAN;
    TempCall: CallString;

    BEGIN
    NumberGridSquaresInList := 0;

    IF (ActiveDomesticMult = GridSquares) AND RoverCall (Call) THEN
        BEGIN
        CreateGridSquareList (Call, ActiveBand);

        IF NumberGridSquaresInList > 0 THEN
            BEGIN
            IF NumberGridSquaresInList > 1 THEN
                BEGIN
                Range := NumberGridSquaresInList - 2;

                REPEAT
                    Change := False;

                    FOR Address := 0 TO Range DO
                        IF GridSquareList [Address] > GridSquareList [Address + 1] THEN
                            BEGIN
                            TempCall := GridSquareList [Address];
                            GridSquareList [Address] := GridSquareList [Address+ 1];
                            GridSquareList [Address + 1] := TempCall;
                            Change := True;
                            END;

                    Dec (Range);
                UNTIL (NOT Change) OR (Range < 0);
                END;


            SaveSetAndClearActiveWindow (EditableLogWindow);

            FOR Address := 0 TO NumberGridSquaresInList - 1 DO
                BEGIN
                IF WhereX >= 76 THEN WriteLn;

                IF WhereX > 1 THEN Write (' ');

                Write (GridSquareList [Address]);
                END;

            RestorePreviousWindow;
            GridSquareListShown := True;
            END;
        END;
    END;



FUNCTION LogFileLooksOkay: BOOLEAN;

{ Returns TRUE if the LOG.DAT file looks about right for the number of
  QSOs the program thinks we have }

VAR NumberContacts: LONGINT;
    ErrorPercent: REAL;

    BEGIN
    LogFileLooksOkay := False;

    NumberContacts := (GetFileSize (LogFileName)) DIV 84;

    IF NumberContacts = 0 THEN
        IF QSOTotals [All, Both] > 2 THEN
            Exit
        ELSE
            BEGIN
            LogFileLooksOkay := True;
            Exit;
            END;

    IF QSOTotals [All, Both] = 0 THEN Exit;

    IF Abs (QSOTotals [All, Both] - NumberContacts) < 10 THEN
        BEGIN
        LogFileLooksOkay := True;
        Exit;
        END;

    ErrorPercent := ((QSOTotals [All, Both] - NumberContacts) / QSOTotals [All, Both]) * 100;
    LogFileLooksOkay := (ErrorPercent < 3.0) AND (ErrorPercent > -3.0);
    END;



PROCEDURE WriteLogEntry (Entry: Str80);

VAR FileWrite: TEXT;

    BEGIN
    Assign  (FileWrite, LogFileName);
    Append  (FileWrite);
    WriteLn (FileWrite, Entry);
    Close   (FileWrite);

    IF PrinterEnabled THEN
        BEGIN
        {$I-}
        WriteLn (Lst, Entry);
        IF IOResult <> 0 THEN SendMorse ('PRINTER FAILURE');
        {$I+}
        END;
    END;



PROCEDURE NextPage;

VAR FileWrite: TEXT;

    BEGIN
    Assign  (FileWrite, LogFileName);
    Append  (FileWrite);
    Write   (FileWrite, ControlL);
    Close   (FileWrite);

    IF PrinterEnabled THEN
        BEGIN
        {$I-}
        Write (Lst, ControlL);
        IF IOResult <> 0 THEN SendMorse ('     PRINTER FAILURE');
        {$I+}
        END;

    END;



PROCEDURE PrintLogHeader;

VAR LogString, UnderLine: Str80;
//    PageNumber: INTEGER;
    BEGIN
//    PageNumber := (QSOTotals [All, Both] DIV 50) + 1;

    WriteLogEntry (ContestTitle);
    WriteLogEntry (LogSubTitle);
    WriteLogEntry ('');

    BandModeDateTimeNumberCallNameSentHeader (LogString, Underline);

    { These are hacks when the very nice way just isn't efficient enough }

    { Note that the RSTQTHNameAndFistsNumberOrPowerExchange has the
      multiplier header and stamp functions wired to do nothing }

    {KK1L: 6.70 Changed spacing slightly to line up with output}
    IF ActiveExchange = RSTQTHNameAndFistsNumberOrPowerExchange THEN
        BEGIN
        LogString := LogString + ' TXR  RXR QTH NAME      NUM/PWR';
        UnderLine := Underline + ' ---  --- --- ----      -------';
        END
    ELSE
        BEGIN
        { Very nice generic way of doing things }

        WITH ExchangeInformation DO
            BEGIN
            IF RST THEN
                BEGIN
                RSTSentHeader     (LogString, Underline);
                RSTReceivedHeader (LogString, Underline);
                END;


            IF Classs       THEN ClassReceivedHeader      (LogString, UnderLine);
            IF QSONumber   THEN QSONumberReceivedHeader  (LogString, UnderLine);
            IF PostalCode  THEN PostalCodeReceivedHeader (LogString, UnderLine);
            IF RandomChars THEN RandomCharsSentAndReceivedHeader (LogString, Underline);
            IF Power       THEN PowerReceivedHeader      (LogString, Underline);
            IF Name        THEN NameReceivedHeader       (LogString, Underline);
            IF Chapter     THEN ChapterReceivedHeader    (LogString, Underline);
            IF Age         THEN AgeReceivedHeader        (LogString, Underline);
            IF Precedence  THEN PrecedenceReceivedHeader (LogString, UnderLine);
            IF Check       THEN CheckReceivedHeader      (LogString, UnderLine);
            IF Zone        THEN ZoneReceivedHeader       (LogString, UnderLine);
            IF TenTenNum   THEN TenTenNumReceivedHeader  (LogString, UnderLine);
            IF QTH         THEN QTHReceivedHeader        (LogString, UnderLine);
            END;
        END;

    MultiplierHeader (LogString, UnderLine);
    QSOPointHeader (LogString, UnderLine);
    WriteLogEntry (LogString);
    WriteLogEntry (Underline);
    END;



FUNCTION MakeLogString (RXData: ContestExchange): Str80;

{ This function will take the information in the contest exchange record
  passed to it and generate a log entry string from it.  }

VAR LogString: Str80;

    BEGIN
    LogString := '';

    BandModeDateTimeNumberCallNameSentStamp (RXData, LogString);

    WITH ExchangeInformation DO
        BEGIN
        IF RST THEN
            BEGIN
            RSTSentStamp     (RXData, LogString);
            RSTReceivedStamp (RXData, LogString);
            END;

        IF LogBadQSOString <> '' THEN
            BEGIN
            LogString := LogString + LogBadQSOString;
            MultiplierStamp (RXData, LogString);
            QSOPointStamp   (RXData, LogString);
            MakeLogString := LogString;
            Exit;
            END;

        {KK1L: 6.70 Sometimes there is just not a pretty way to do it!!}

        CASE ActiveExchange OF
            RSTQTHNameAndFistsNumberOrPowerExchange:  { KK1L added this in 6.70 }
                BEGIN
                IF QTH         THEN QTHReceivedStamp        (RXData, LogString);
                IF Name        THEN NameReceivedStamp       (RXData, LogString);
                IF QSONumber   THEN QSONumberReceivedStamp  (RXData, LogString);
                IF Power       THEN PowerReceivedStamp      (RXData, LogString);
                END;

            { N6TR decided to give in for the CWT }

            ELSE
                BEGIN
                IF Kids        THEN KidsReceivedStamp       (RXData, LogString);
                IF Classs      THEN ClassReceivedStamp      (RXData, LogString);
                IF QSONumber   THEN QSONumberReceivedStamp  (RXData, LogString);
                IF PostalCode  THEN PostalCodeReceivedStamp (RXData, LogString);
                IF RandomChars THEN RandomCharsSentAndReceivedStamp (RXData, LogString);
                IF Power       THEN PowerReceivedStamp      (RXData, LogString);
                IF Age         THEN AgeReceivedStamp        (RXData, LogString);
                IF Name        THEN NameReceivedStamp       (RXData, LogString);
                IF Chapter     THEN ChapterReceivedStamp    (RXData, LogString);
                IF Precedence  THEN PrecedenceReceivedStamp (RXData, LogString);
                IF Check       THEN CheckReceivedStamp      (RXData, LogString);
                IF Zone        THEN ZoneReceivedStamp       (RXData, LogString);
                IF TenTenNum   THEN TenTenNumReceivedStamp  (RXData, LogString);
                IF QTH         THEN QTHReceivedStamp        (RXData, LogString);
                END;

            END; { of case ActiveExchange }
        END;

    MultiplierStamp (RXData, LogString);
    QSOPointStamp   (RXData, LogString);
    MakeLogString := LogString;
    END;



PROCEDURE CalculateQSOPoints (VAR RXData: ContestExchange);

VAR MyZoneValue, RXDataZoneValue: INTEGER;
    Distance: LONGINT;
    RXCtyID, TheirID, CountryID: CallString;

    BEGIN
    IF (QSOPointsDomesticCW >= 0) AND (RXData.Mode = CW) AND (RXData.DomesticQTH <> '') THEN
        BEGIN
        RXData.QSOPoints := QSOPointsDomesticCW;
        Exit;
        END;

    IF (QSOPointsDXCW >= 0) AND (RXData.Mode = CW) AND (RXData.DomesticQTH = '') THEN
        BEGIN
        RXData.QSOPoints := QSOPointsDXCW;
        Exit;
        END;

    IF (QSOPointsDomesticPhone >= 0) AND (RXData.Mode = Phone) AND (RXData.DomesticQTH <> '') THEN
        BEGIN
        RXData.QSOPoints := QSOPointsDomesticPhone;
        Exit;
        END;

    IF (QSOPointsDXPhone >= 0) AND (RXData.Mode = Phone) AND (RXData.DomesticQTH = '') THEN
        BEGIN
        RXData.QSOPoints := QSOPointsDXPhone;
        Exit;
        END;

    RXCtyID := CountryTable.GetCountryID (RXData.QTH.Country);

    CASE ActiveQSOPointMethod OF

        AllAsianQSOPointMethod:
            BEGIN
            IF MyContinent = Asia THEN
                BEGIN
                IF RXData.QTH.Continent <> Asia THEN
                    BEGIN
                    CASE RXData.Band OF
                        Band160: RXData.QSOPoints := 9;
                        Band80:  RXData.QSOPoints := 6;
                        Band10:  RXData.QSOPoints := 6;
                        ELSE     RXData.QSOPoints := 3;
                        END;
                    END
                ELSE
                    IF MyCountry <> RXCtyID THEN
                        BEGIN
                        CASE RXData.Band OF
                            Band160: RXData.QSOPoints := 3;
                            Band80:  RXData.QSOPoints := 2;
                            Band10:  RXData.QSOPoints := 2;
                            ELSE     RXData.QSOPoints := 1;
                            END;
                        END
                    ELSE
                        BEGIN
                        RXData.QSOPoints := 0;
                        RXData.InhibitMults := True;  { Same country }
                        END;

                END

            ELSE  { I am not in Asia }

                IF RXData.QTH.Continent = Asia THEN
                    BEGIN
                    CASE RXData.Band OF
                        Band160: RXData.QSOPoints := 3;
                        Band80:  RXData.QSOPoints := 2;
                        Band10:  RXData.QSOPoints := 2;
                        ELSE     RXData.QSOPoints := 1;
                        END;
                    END
                ELSE
                    BEGIN
                    RXData.QSOPoints := 0;
                    RXData.InhibitMults := True;
                    END;

            END;

        ARCIQSOPointMethod:
            BEGIN
            IF StringIsAllNumbers (RXData.Power) THEN
                RXData.QSOPoints := 5
            ELSE
                IF MyContinent = RXData.QTH.Continent THEN
                    RXData.QSOPoints := 2
                ELSE
                    RXData.QSOPoints := 4;
            END;


        ARIQSOPointMethod:
            IF (RXCtyID = 'I') OR (RXCtyID = 'IS') THEN
                RXData.QSOPoints := 10
            ELSE
                IF RXData.QTH.Continent <> MyContinent THEN
                    RXData.QSOPoints := 3
                ELSE
                    IF RXCtyID <> MyCountry THEN
                        RXData.QSOPoints := 1
                    ELSE
                        RXData.QSOPoints := 0;

        ARRLDXQSOPointMethod:
            IF (MyCountry = 'K') OR
               (MyCountry = 'VE') THEN
                BEGIN
                IF (RXCtyID <> 'K') AND (RXCtyID <> 'VE') THEN
                    RXData.QSOPoints := 3
                ELSE
                    BEGIN
                    RXData.QSOPoints := 0;
                    RXData.InhibitMults := True;
                    END;
                END
            ELSE
                IF (RXCtyID = 'K') OR (RXCtyID = 'VE') THEN
                    RXData.QSOPoints := 3
                ELSE
                    BEGIN
                    RXData.QSOPoints := 0;
                    RXData.InhibitMults := True;
                    END;

        ARRLFieldDayQSOPointMethod:
            IF StringHas (UpperCase (MyFDClass), 'D')    AND
               Stringhas (UpperCase (RXData.Classs), 'D') THEN
                   RXData.QSOPoints := 0
               ELSE
                   IF RXData.Mode = Phone THEN
                       RXData.QSOPoints := 1
                   ELSE
                       RXData.QSOPoints := 2;

        ARRL160QSOPointMethod:
            IF DomesticCountryCall (RXData.Callsign) THEN
                RXData.QSOPoints := 2
            ELSE
                IF DomesticCountryCall (MyCall) THEN
                    RXData.QSOPoints := 5
                ELSE
                    RXData.QSOPoints := 0;

        ARRL10QSOPointMethod:
            IF ((Copy (RXData.Callsign, Length (RXData.Callsign) - 1, 2) = '/T') OR
                (Copy (RXData.Callsign, Length (RXData.Callsign) - 1, 2) = '/N')) AND
               (RXData.Mode = CW) THEN
                   RXData.QSOPoints := 8
               ELSE
                   IF RXData.Mode = CW THEN
                       RXData.QSOPoints := 4
                   ELSE
                       RXData.QSOPoints := 2;

        ARRLVHFQSOPointMethod:
            CASE RXData.Band OF
                Band6:     RXData.QSOPoints := 1;
                Band2:     RXData.QSOPoints := 1;
                Band222:   RXData.QSOPoints := 2;
                Band432:   RXData.QSOPoints := 2;
                Band902:   RXData.QSOPoints := 3;
                Band1296:  RXData.QSOPoints := 3;
                Band2304:  RXData.QSOPoints := 4;
                Band3456:  RXData.QSOPoints := 4;
                Band5760:  RXData.QSOPoints := 4;
                Band10G:   RXData.QSOPoints := 4;
                Band24G:   RXData.QSOPoints := 4;
                BandLight: RXData.QSOPoints := 4;
                ELSE       RXData.QSOPoints := 0;
                END;

        ARRLVHFSSPointMethod:
            CASE RXData.Band OF
                Band6:     RXData.QSOPoints := 1;
                Band2:     RXData.QSOPoints := 1;
                Band222:   RXData.QSOPoints := 2;
                Band432:   RXData.QSOPoints := 2;
                Band902:   RXData.QSOPoints := 4;
                Band1296:  RXData.QSOPoints := 4;
                Band2304:  RXData.QSOPoints := 8;
                Band3456:  RXData.QSOPoints := 8;
                Band5760:  RXData.QSOPoints := 8;
                Band10G:   RXData.QSOPoints := 8;
                Band24G:   RXData.QSOPoints := 8;
                BandLight: RXData.QSOPoints := 8;
                ELSE       RXData.QSOPoints := 0;
                END;

        BalticQSOPointMethod:
            BEGIN
            IF (MyCountry = 'ES') OR (MyCountry = 'YL') OR (MyCountry = 'LY') THEN
                BEGIN
                IF RXData.QTH.Continent = Europe THEN
                    RXData.QSOPoints := 1
                ELSE
                    RXData.QSOPoints := 2;
                END
            ELSE
                IF (RXCtyID = 'ES') OR (RXCtyID = 'YL') OR (RXCtyID = 'LY') THEN
                    BEGIN
                    IF MyContinent = Europe THEN
                        RXData.QSOPoints := 10
                    ELSE
                        RXData.QSOPoints := 20;
                    END
                ELSE
                    RXData.QSOPoints := 1;
            END;

        CQ160QSOPointMethod:
            IF RXCtyID = MyCountry THEN
                RXData.QSOPoints := 2
            ELSE
                IF RXData.QTH.Continent = MyContinent THEN
                    RXData.QSOPoints := 5
                ELSE
                    RXData.QSOPoints := 10;

        CQMQSOPointMethod:
            IF RXData.QTH.Continent <> MyContinent THEN
                RXData.QSOPoints := 3
            ELSE
                IF RXCtyID <> MyCountry THEN
                    RXData.QSOPoints := 2
                ELSE
                    RXData.QSOPoints := 1;

        CQVHFQSOPointMethod:
            BEGIN
            CASE RXData.Band OF
               { Band6, Band2:      RXData.QSOPoints := 1;}  {KK1L: 6.73 new rules for 2002}
               { Band222, Band432:  RXData.QSOPoints := 2;}  {KK1L: 6.73 new rules for 2002}
               { Band902, Band1296: RXData.QSOPoints := 4;}  {KK1L: 6.73 new rules for 2002}
               { Band2304:          RXData.QSOPoints := 6;}  {KK1L: 6.73 new rules for 2002}
               { Band3456:          RXData.QSOPoints := 6;}  {KK1L: 6.73 new rules for 2002}
               { Band5760:          RXData.QSOPoints := 6;}  {KK1L: 6.73 new rules for 2002}
               { Band10G:           RXData.QSOPoints := 6;}  {KK1L: 6.73 new rules for 2002}
               { Band24G:           RXData.QSOPoints := 6;}  {KK1L: 6.73 new rules for 2002}
               { BandLight:         RXData.QSOPoints := 6;}  {KK1L: 6.73 new rules for 2002}
                Band2:             RXData.QSOPoints := 2; {KK1L: 6.73 new rules for 2002}
                Band6:             RXData.QSOPoints := 1; {KK1L: 6.73 new rules for 2002}
                ELSE               RXData.QSOPoints := 0;
                END;

            IF (RXData.Mode = CW) AND (RXData.QSOPoints > 0) THEN
                Inc (RXData.QSOPoints);
            END;

        CQWPXQSOPointMethod:
          BEGIN
          { In 2022 - QSOs with Russia and EU do not count for the WPX test }

          IF (RXCtyID = 'UA') OR (RXCtyID = 'UA2') OR (RXCtyID = 'UA9') OR
             (RXCtyID = 'R1FJ') OR (RXCtyID = 'EU') THEN
              BEGIN
              RXData.QSOPoints := 0;
              Exit;
              END;

          IF RXData.QTH.Continent = MyContinent THEN
              BEGIN
              IF RXCtyID = MyCountry THEN
                  RXData.QSOPoints := 1
              ELSE
                  BEGIN
                  CASE RXData.Band OF
                      Band160: RXData.QSOPoints := 2;
                      Band80:  RXData.QSOPoints := 2;
                      Band40:  RXData.QSOPoints := 2;
                      Band20:  RXData.QSOPoints := 1;
                      Band15:  RXData.QSOPoints := 1;
                      Band10:  RXData.QSOPoints := 1;
                      END;

                  IF MyContinent = NorthAmerica THEN
                      RXData.QSOPoints := RXData.QSOPoints + RXData.QSOPoints;
                  END;
              END
          ELSE
              CASE RXData.Band OF
                  Band160: RXData.QSOPoints := 6;
                  Band80:  RXData.QSOPoints := 6;
                  Band40:  RXData.QSOPoints := 6;
                  Band20:  RXData.QSOPoints := 3;
                  Band15:  RXData.QSOPoints := 3;
                  Band10:  RXData.QSOPoints := 3;
                  END;
          END;

        CQWPXRTTYQSOPointMethod:
          BEGIN
          IF RXData.QTH.Continent = MyContinent THEN
              BEGIN
              IF RXCtyID = MyCountry THEN
                  RXData.QSOPoints := 1
              ELSE
                  RXData.QSOPoints := 2
              END
          ELSE
              RXData.QSOPoints := 3;

          IF (RXData.Band = Band80) OR (RXData.Band = Band40) THEN
              RXData.QSOPoints := RXData.QSOPoints + RXData.QSOPoints;

          END;


        CQWWQSOPointMethod:
          IF RXData.QTH.Continent <> MyContinent THEN
              RXData.QSOPoints := 3
          ELSE
              IF RXCtyID <> MyCountry THEN
                  IF MyContinent <> NorthAmerica THEN
                      RXData.QSOPoints := 1
                  ELSE
                      RXData.QSOPoints := 2
              ELSE RXData.QSOPoints := 0;

        CQWWRTTYQSOPointMethod:
          IF RXData.QTH.Continent <> MyContinent THEN
              RXData.QSOPoints := 3
          ELSE
              IF RXCtyID <> MyCountry THEN
                      RXData.QSOPoints := 2
                  ELSE
                      RXData.QSOPoints := 1;

        CroatianQSOPointMethod:
            IF RXCtyID = '9A' THEN
                BEGIN
                CASE RXData.Band OF
                    Band160: RXData.QSOPoints := 10;
                     Band80: RXData.QSOPoints := 10;
                     Band40: RXData.QSOPoints := 10;
                     Band20: RXData.QSOPoints :=  6;
                     Band15: RXData.QSOPoints :=  6;
                     Band10: RXData.QSOPoints :=  6;
                     END;
                END
            ELSE
                IF RXData.QTH.Continent = MyContinent THEN
                    BEGIN
                    CASE RXData.Band OF
                        Band160: RXData.QSOPoints := 2;
                         Band80: RXData.QSOPoints := 2;
                         Band40: RXData.QSOPoints := 2;
                         Band20: RXData.QSOPoints := 1;
                         Band15: RXData.QSOPoints := 1;
                         Band10: RXData.QSOPoints := 1;
                         END;
                    END
                ELSE
                    CASE RXData.Band OF
                        Band160: RXData.QSOPoints := 6;
                         Band80: RXData.QSOPoints := 6;
                         Band40: RXData.QSOPoints := 6;
                         Band20: RXData.QSOPoints := 3;
                         Band15: RXData.QSOPoints := 3;
                         Band10: RXData.QSOPoints := 3;
                         END;


        EuropeanFieldDayQSOPointMethod:
           BEGIN
           RXData.QSOPoints := 1;

           CountryID := MyCountry;

           IF (CountryID = 'F') OR (CountryID = 'OE') THEN
              BEGIN
              IF RXCtyID = MyCountry THEN
                  BEGIN
                  IF PortableStation (RXData.Callsign) THEN
                      RXData.QSOPoints := 50
                  ELSE
                      RXData.QSOPoints := 10;
                  END
              ELSE
                  IF RXData.QTH.Continent = Europe THEN
                      BEGIN
                      IF PortableStation (RXData.Callsign) THEN
                          RXData.QSOPoints := 5
                      ELSE
                          RXData.QSOPoints := 1;
                      END
                  ELSE
                      RXData.QSOPoints := 3;

              Exit;
              END;

           IF CountryID = 'OZ' THEN
              BEGIN
              IF RXCtyID = MyCountry THEN
                  BEGIN
                  IF PortableStation (RXData.Callsign) THEN
                      RXData.QSOPoints := 10
                  ELSE
                      RXData.QSOPoints := 1;
                  END
              ELSE
                  IF RXData.QTH.Continent = Europe THEN
                      BEGIN
                      IF PortableStation (RXData.Callsign) THEN
                          RXData.QSOPoints := 5
                      ELSE
                          RXData.QSOPoints := 3;
                      END
                  ELSE
                      RXData.QSOPoints := 6;

              Exit;
              END;

          IF CountryID = 'HB' THEN
              BEGIN
              IF PortableStation (RXData.Callsign) THEN
                  RXData.QSOPoints := 5
              ELSE
                  IF RXData.QTH.Continent = Europe THEN
                      RXData.QSOPoints := 1
                  ELSE
                      RXData.QSOPoints := 2;
              Exit;
              END;

          IF CountryID = 'I' THEN
              BEGIN
              IF PortableStation (RXData.Callsign) THEN
                  RXData.QSOPoints := 6
              ELSE
                  IF RXData.QTH.Continent = Europe THEN
                      RXData.QSOPoints := 1
                  ELSE
                      RXData.QSOPoints := 2;

              IF (RxData.Band = Band160) OR (RXData.Band = Band80) THEN
                  RXData.QSOPoints := RXData.QSOPoints * 2;
              Exit;
              END;

          { Anywhere else, including DL and PA }

          IF PortableStation (RXData.Callsign) THEN
              BEGIN
              IF RXData.QTH.Continent = Europe THEN
                  RXData.QSOPoints := 4
              ELSE
                  RXData.QSOPoints := 6;
              END
          ELSE
              BEGIN
              IF RXData.QTH.Continent = Europe THEN
                  RXData.QSOPoints := 2
              ELSE
                  RXData.QSOPoints := 3;
              END;

          IF CountryID [1] = 'G' THEN
              IF (RxData.Band = Band160) OR (RXData.Band = Band10) THEN
                  RXData.QSOPoints := RXData.QSOPoints * 2;

          END;

        EuropeanSprintQSOPointMethod:
            IF MyContinent = Europe THEN
                RXData.QSOPoints := 1
            ELSE
                IF RXData.QTH.Continent = Europe THEN
                    RXData.QSOPoints := 1
                ELSE
                    RXData.QSOPoints := 0;

        EuropeanVHFQSOPointMethod:
            IF MyGrid <> '' THEN
                RXData.QSOPoints := GetEuropeanDistanceBetweenGrids (MyGrid, RXData.DomesticQTH);

        FistsQSOPointMethod:
            IF RXData.NumberReceived > 0 THEN
                RXData.QSOPoints := 5
            ELSE
                RXData.QSOPoints := 2;

        HADXQSOPointMethod:
            IF RXCtyID = 'HA' THEN
                RXData.QSOPoints := 6
            ELSE
                IF RXData.QTH.Continent <> MyContinent THEN
                    RXData.QSOPoints := 3
                ELSE
                    RXData.QSOPoints := 0;

        HelvetiaQSOPointMethod:
            IF MyCountry <> 'HB' THEN
                BEGIN
                IF RXCtyID = 'HB' THEN
                    RXData.QSOPoints := 3
                ELSE
                    RXData.QSOPoints := 0;
                END
            ELSE
                IF RXData.QTH.Continent = Europe THEN
                    RXData.QSOPoints := 1
                ELSE
                    RXData.QSOPoints := 3;

        IARUQSOPointMethod:
            IF RXData.DomesticQTH = '' THEN
                BEGIN
                Val (RXData.Zone, RXDataZoneValue);
                Val (MyZone,      MyZoneValue);

                IF RXDataZoneValue = MyZoneValue THEN
                    RXData.QSOPoints := 1
                ELSE
                    IF RXData.QTH.Continent = MyContinent THEN
                        RXData.QSOPoints := 3
                    ELSE
                        RXData.QSOPoints := 5;
                END
            ELSE
                RXData.QSOPoints := 1;

        InternetSixQSOPointMethod:
            BEGIN
            IF RXData.QTH.Continent <> MyContinent THEN
                RXData.QSOPoints := 3
            ELSE
                IF MyContinent <> NorthAmerica THEN
                    RXData.QSOPOints := 1
                ELSE
                    BEGIN
                    TheirID := RXCtyID;
                    IF TheirID = 'VE' THEN TheirID := 'K';

                    IF (MyCounty <> 'K') AND (MyCountry <> 'VE') THEN
                        BEGIN                                  { I am DX }
                        IF TheirID = 'K' THEN
                            RXData.QSOPoints := 3
                        ELSE
                            RXData.QSOPoints := 1;
                        END
                    ELSE
                        IF TheirID <> 'K' THEN                 { I am W/VE }
                            RXData.QSOPoints := 3
                        ELSE
                            RXData.QSOPoints := 1;
                    END;
            END;

        IOTAQSOPointMethod:
            IF (RXData.DomesticQTH <> '') AND
               (UpperCase (RXData.DomesticQTH) <> UpperCase (MyIOTA)) THEN
                   RXData.QSOPoints := 15
               ELSE
                   RXData.QSOPoints := 3;

        JapanInternationalDXQSOPointMethod:
            BEGIN
            CASE RXData.Band OF
                Band160: RXData.QSOPoints := 4;
                Band80:  RXData.QSOPoints := 2;
                Band40:  RXData.QSOPoints := 1;
                Band20:  RXData.QSOPoints := 1;
                Band15:  RXData.QSOPoints := 1;
                Band10:  RXData.QSOPoints := 2;
                END;

            IF MyCountry = 'JA' THEN
                BEGIN
                IF RXCtyID = 'JA' THEN
                    BEGIN
                    RXData.QSOPoints := 0;
                    RXData.InhibitMults := True;
                    END;
                END
            ELSE
                IF (RXCtyID <> 'JA') AND (Copy (RXCtyID, 1, 2) <> 'JD') THEN
                    BEGIN
                    RXData.QSOPoints := 0;
                    RXData.InhibitMults := True;
                    END;
            END;

        KCJQSOPointMethod:
            IF MyCountry = 'JA' THEN
                BEGIN
                IF (RXCtyID = 'JA') OR (RXCtyID = 'JD1') THEN
                    RXData.QSOPoints := 1
                ELSE
                    RXData.QSOPoints := 5;
                END
            ELSE
                BEGIN
                IF (RXCtyID = 'JA') OR (RXCtyID = 'JD1') THEN
                    RXData.QSOPoints := 1
                END;

        MMCQSOPointMethod:
           IF RXData.QTH.Continent <> MyContinent THEN
              RXData.QSOPoints := 5
           ELSE
              IF RXCtyID = MyCountry THEN
                 RXData.QSOPoints := 1
              ELSE
                 RXData.QSOPoints := 3;

        MQPQSOPointMethod:
            IF RXData.Callsign = 'W0EF' THEN
                RXData.QSOPoints := 10
            ELSE
                IF RXData.Mode = CW THEN
                    RXData.QSOPoints := 2
                ELSE
                    RXData.QSOPoints := 1;

        NZFieldDayQSOPointMethod:
            IF RXCtyID = 'ZL' THEN
                BEGIN
                IF RXData.Mode = CW THEN
                    RXData.QSOPoints := 5
                ELSE
                    RXData.QSOPoints := 3;
                END
            ELSE
                RXData.QSOPoints := 10;

        OKDXQSOPointMethod:
            IF (MyCountry = 'OK') OR (MyCountry = 'OM') THEN
                BEGIN
                IF RXData.QTH.Continent <> Europe THEN
                    RXData.QSOPoints := 3
                ELSE
                    IF (RXCtyID <> 'OK') AND (RXCtyID <> 'OM') THEN
                        RXData.QSOPoints := 1
                    ELSE
                        RXData.QSOPoints := 0;
                END
            ELSE
                IF (RXCtyID = 'OK') OR (RXCtyID = 'OM') THEN
                    BEGIN
                    IF MyContinent = Europe THEN
                        RXData.QSOPoints := 1
                    ELSE
                        RXData.QSOPoints := 3;
                    END
                ELSE
                    RXData.QSOPoints := 0;

        RACQSOPointMethod:
            IF RXCtyID = 'VE' THEN
                BEGIN
                IF Pos ('RAC', RXData.Callsign) > 0 THEN
                    RXData.QSOPoints := 20
                ELSE
                    RXData.QSOPoints := 10;
                END
            ELSE
                RXData.QSOPoints := 2;

        RSGBQSOPointMethod:
            IF MyCountry = RXCtyID THEN
                RXData.QSOPoints := 0
            ELSE
                CASE RXData.QTH.Continent OF
                    Europe:  RXData.QSOPoints := 5;
                    Oceania: RXData.QSOPoints := 30;
                    ELSE     RXData.QSOPoints := 15;
                    END;

        RussianDXQSOPointMethod:
            BEGIN
            IF (MyCountry = 'UA') OR
               (MyCountry = 'UA2') OR
               (MyCountry = 'UA9') OR
               (MyCountry = 'R1FJ') OR
               (MyCountry = 'CE9') OR
               (MyCountry = 'R1MV') THEN
                BEGIN
                IF RXData.QTH.Continent <> MyContinent THEN
                    RXData.QSOPoints := 5
                ELSE
                    IF RXCtyID <> MyCountry THEN
                        RXData.QSOPoints := 3
                    ELSE
                        RXData.QSOPoints := 2;
                END

            { I am not in Russia }

            ELSE
                IF (RXCtyID = 'UA') OR
                   (RXCtyID = 'UA2') OR
                   (RXCtyID = 'UA9') OR
                   (RXCtyID = 'R1FJ') OR
                   (RXCtyID = 'CE9') OR
                   (RXCtyID = 'R1MV') THEN
                        RXData.QSOPoints := 10
                    ELSE
                        IF RXData.QTH.Continent <> MyContinent THEN
                            RXData.QSOPoints := 5
                        ELSE
                            IF RXCtyID <> MyCountry THEN
                                RXData.QSOPoints := 3
                            ELSE
                                RXData.QSOPoints := 2;
            END;

        SalmonRunQSOPointMethod:
            BEGIN
            IF RXData.Mode = CW THEN
                RXData.QSOPoints := 4
            ELSE
                RXData.QSOPoints := 2;
            END;


        ScandinavianQSOPointMethod:
            IF ScandinavianCountry (MyCountry) THEN
                BEGIN
                IF ScandinavianCountry (RXCtyID) THEN
                    RXData.QSOPoints := 0
                ELSE
                    IF RXData.QTH.Continent = Europe THEN
                        RXData.QSOPoints := 2
                    ELSE
                        RXData.QSOPoints := 3;
                END
            ELSE
                BEGIN
                IF ScandinavianCountry (RXCtyID) THEN
                    RXData.QSOPoints := 1
                ELSE
                    RXData.QSOPoints := 0;

                IF MyContinent <> Europe THEN
                    IF (RXData.Band = Band160) OR (RXData.Band = Band80) OR (RXData.Band = Band40) THEN
                        RXData.QSOPoints := RXData.QSOPoints * 3;
                END;

        SLFivePointQSOMethod:
            IF Copy (RXData.Callsign, 1, 2) = 'SL' THEN
                RXData.QSOPoints := 5
            ELSE
                RXData.QSOPoints := 1;


        SouthAmericanWWQSOPointMethod:
            BEGIN
            IF MyContinent = SouthAmerica THEN
                BEGIN
                IF RXData.QTH.Continent = SouthAmerica THEN
                    RXData.QSOPoints := 2
                ELSE
                    RXData.QSOPoints := 10;
                END
            ELSE
                IF RXData.QTH.Continent = SouthAmerica THEN
                    RXData.QSOPoints := 10
                ELSE
                    RXData.QSOPoints := 2;
            END;


        StewPerryQSOPointMethod:
            BEGIN
            IF (MyGrid <> '') AND (RXData.DomesticQTH <> '') THEN
                BEGIN
                Distance := GetDistanceBetweenGrids (MyGrid, RXData.DomesticQTH);
                RXData.QSOPoints := (Distance DIV 500) + 1;
                END
            ELSE
                RXData.QSOPoints := 1;
            END;

        TenTenQSOPointMethod:
            IF RXData.TenTenNum <> -1 THEN
                RXData.QSOPoints := 2
            ELSE
                RXData.QSOPoints := 1;

        TOECQSOPointMethod:
            IF RXData.QTH.Continent <> MyContinent THEN
                RXData.QSOPoints := 3
            ELSE
                IF StringHas (RXData.CallSign, '/M') THEN
                    RXData.QSOPoints := 3
                ELSE
                    RXData.QSOPoints := 1;

        UBAQSOPointMethod:
            IF RXCtyID = 'ON' THEN
                RXData.QSOPoints := 10
            ELSE
                IF RXData.QTH.Continent = Europe THEN
                    RXData.QSOPoints := 3
                ELSE
                    RXData.QSOPoints := 1;

        UkrainianQSOPointMethod:
            IF RXCtyID = 'UR' THEN
                RXData.QSOPoints := 10
            ELSE
                IF RXCtyID = MyCountry THEN
                    RXData.QSOPoints := 1
                ELSE
                    IF RXData.QTH.Continent = MyContinent THEN
                        RXData.QSOPoints := 2
                    ELSE
                        RXData.QSOPoints := 3;

        VKZLQSOPointMethod:
            IF (MyContinent = Oceania) THEN
                BEGIN
                CASE RXData.Band OF
                    Band160: RXData.QSOPoints := 20;
                    Band80:  RXData.QSOPoints := 10;
                    Band40:  RXData.QSOPoints :=  5;
                    Band20:  RXData.QSOPoints :=  1;
                    Band15:  RXData.QSOPoints :=  2;
                    Band10:  RXData.QSOPoints :=  3;
                    END;
                END
            ELSE
                IF RXData.QTH.Continent = Oceania THEN
                    BEGIN
                    CASE RXData.Band OF
                        Band160: RXData.QSOPoints := 20;
                        Band80:  RXData.QSOPoints := 10;
                        Band40:  RXData.QSOPoints :=  5;
                        Band20:  RXData.QSOPoints :=  1;
                        Band15:  RXData.QSOPoints :=  2;
                        Band10:  RXData.QSOPoints :=  3;
                        END;
                    END
                ELSE
                    RXData.QSOPoints := 0;

        WAGQSOPointMethod:
            IF MyCountry = 'DL' THEN
                BEGIN
                IF RXCtyID = 'DL' THEN
                    RXData.QSOPoints := 1
                ELSE
                    IF RXData.QTH.Continent = Europe THEN
                        RXData.QSOPoints := 3
                    ELSE
                        RXData.QSOPoints := 5;
                END
            ELSE
                BEGIN
                IF RXCtyID = 'DL' THEN
                    RXData.QSOPoints := 3
                ELSE
                    RXData.QSOPoints := 0;
                END;

        WAEQSOPointMethod:
            IF MyContinent = Europe THEN
                IF (RXData.QTH.Continent <> Europe) AND (RXData.Band <> Band160) THEN
                    RXData.QSOPoints := 1
                ELSE
                    RXData.QSOPoints := 0
            ELSE
                IF (RXData.QTH.Continent = Europe) AND (RXData.Band <> Band160) THEN
                    RXData.QSOPoints := 1
                ELSE
                    RXData.QSOPoints := 0;

        WWLQSOPointMethod:
            BEGIN
            IF (MyGrid <> '') AND (RXData.DomesticQTH <> '') THEN
                BEGIN
                Distance := GetDistanceBetweenGrids (MyGrid, RXData.DomesticQTH);
                RXData.QSOPoints := (Distance DIV 500) + 1;

                IF (RXData.Band = Band80) OR (RXData.Band = Band160) THEN
                    RXData.QSOPoints := RXData.QSOPoints + RXData.QSOPoints;

                IF RXData.Band = Band160 THEN
                    RXData.QSOPoints := RXData.QSOPoints + RXData.QSOPoints;
                END
            ELSE
                RXData.QSOPoints := 1;
            END;


        YODXQSOPointMethod:
          IF RXCtyID = 'YO' THEN
              RXData.QSOPoints := 8
          ELSE
              IF MyCountry <> RXCtyID THEN
                  IF RXData.QTH.Continent <> MyContinent THEN
                      RXData.QSOPoints := 4
                  ELSE
                      RXData.QSOPoints := 2
              ELSE
                  RXData.QSOPoints := 0; {KK1L: 6.71 Same country QSO}

        NoQSOPointMethod:  RXData.QSOPoints := 0;

        AlwaysOnePointPerQSO: RXData.QSOPoints := 1;
        OnePointPerQSO:       RXData.QSOPoints := 1;
        TwoPointsPerQSO:      RXData.QSOPoints := 2;
        ThreePointsPerQSO:    RXData.QSOPoints := 3;
        TenPointsPerQSO:      RXData.QSOPoints := 10;

        OnePhoneTwoCWThreeDigital:
            CASE RXData.Mode OF
                Phone:   RXData.QSOPoints := 1;
                CW:      RXData.QSOPoints := 2;
                Digital: RXData.QSOPoints := 3;
                END;

        TwoPhoneFourCW:
          IF RXData.Mode = CW THEN RXData.QSOPoints := 4
          ELSE RXData.QSOPoints := 2;

        TwoPhoneThreeCW:
          IF RXData.Mode = CW THEN RXData.QSOPoints := 3
          ELSE RXData.QSOPoints := 2;

        OnePhoneTwoCW:
          IF RXData.Mode = CW THEN RXData.QSOPoints := 2
          ELSE RXData.QSOPoints := 1;

        ThreePhoneFiveCW:
          IF RXData.Mode = CW THEN RXData.QSOPoints := 5
          ELSE RXData.QSOPoints := 3;

        TwoEuropeFiveOther: {WRTC 2018}
            IF MarineOrAirMobileStation (RXData.Callsign) THEN
                RXData.QSOPoints := 5
            ELSE
                IF RXData.QTH.Continent = Europe THEN
                    RXData.QSOPoints := 2
                ELSE
                    RXData.QSOPoints := 5;


        END;   { of case }
    END;



PROCEDURE SendMultiInfoMessage (Band: BandType; Mode: ModeType; Message: Str80);

VAR Sum: BYTE;

    BEGIN
    Sum := CheckSum (Message);

    IF Sum = LastMultiInfoMessageSum [Band, Mode] THEN Exit;

    LastMultiInfoMessageSum [Band, Mode] := Sum;

    Message := AddBand (Band) + AddMode (Mode) + Message;

    SendMultiCommand (MultiBandAddressArray [Band],
                      $FF,
                      MultiInformationMessage,
                      Message);
    END;



PROCEDURE RotorControl (Heading: INTEGER);

VAR TempString: Str20;

    BEGIN
    IF ActiveRotatorPort <> nil THEN
        BEGIN
        Str (Heading, TempString);
        WHILE Length (TempString) < 3 DO TempString := '0' + TempString;

        IF ActiveRotatorType = OrionRotator THEN
            BEGIN
            TempString := '#' + TempString + CarriageReturn;
            SendRotatorMessage (TempString);
            Exit;
            END;

        IF ActiveRotatorType = DCU1Rotator THEN
            BEGIN
            SendRotatorMessage ('AP1' + TempString + ';');
            SendRotatorMessage ('AM1' + ';');
            END;

        {KK1L: 6.71}
        IF ActiveRotatorType = YaesuRotator THEN
            BEGIN
            TempString := 'M' + TempString + CarriageReturn;
            SendRotatorMessage (TempString);
            END;
        END;
    END;




FUNCTION MarineOrAirMobileStation (Call: CallString): Boolean;

{KK1L: 6.68 This function will return TRUE if the callsign passed to it is marine
  or air mobile station. }

VAR TempString: Str20;

    BEGIN
    MarineOrAirMobileStation := False;
    TempString := PostcedingString (Call, '/');

    IF StringHas (TempString, '/') THEN
        TempString := PostcedingString (TempString, '/');

    IF Length (TempString) = 2 THEN
        BEGIN
        IF ((TempString = 'MM') OR (TempString = 'AM')) THEN
            MarineOrAirMobileStation := True;
        END;
    END;



FUNCTION ProcessExchange (ExchangeString: Str80; VAR RData: ContestExchange): BOOLEAN;

    BEGIN
    CASE ActiveExchange OF

        CheckAndChapterOrQTHExchange:
            ProcessExchange := ProcessCheckAndChapterOrQTHExchange (ExchangeString, RData);

        ClassDomesticOrDXQTHExchange:
            ProcessExchange := ProcessClassAndDomesticOrDXQTHExchange (ExchangeString, RData);

        KidsDayExchange:
            ProcessExchange := ProcessKidsExchange (ExchangeString, RData);

        CWTExchange:
            ProcessExchange := ProcessCWTExchange (ExchangeString, RData);

        NameQTHAndPossibleTenTenNumber:
            ProcessExchange := ProcessNameQTHAndPossibleTenTenNumberExchange (ExchangeString, RData);

        NameAndDomesticOrDXQTHExchange:
            ProcessExchange := ProcessNameAndDomesticOrDXQTHExchange (ExchangeString, RData);

        NameAndPossibleGridSquareExchange:
            ProcessExchange := ProcessNameAndPossibleGridSquareExchange (ExchangeString, RData);

        QSONumberAndNameExchange:
            ProcessExchange := ProcessQSONumberAndNameExchange (ExchangeString, RData);

        QSONumberDomesticOrDXQTHExchange:
            ProcessExchange := ProcessQSONumberAndDomesticOrDXQTHExchange (ExchangeString, RData);

        QSONumberDomesticQTHExchange:
            ProcessExchange := ProcessQSONumberAndDomesticQTHExchange (ExchangeString, RData);

        QSONumberNameChapterAndQTHExchange:
            ProcessExchange := ProcessQSONumberNameChapterAndQTHExchange (ExchangeString, RData);

        QSONumberNameDomesticOrDXQTHExchange:
            ProcessExchange := ProcessQSONumberNameAndDomesticOrDXQTHExchange (ExchangeString, RData);

        RSTAgeExchange:
            ProcessExchange := ProcessRSTAndAgeExchange (ExchangeString, RData);

        RSTAllJAPrefectureAndPrecedenceExchange:
            ProcessExchange := ProcessRSTAllJAPrefectureAndPrecedenceExchange (ExchangeString, RData);

        RSTAndContinentExchange:
            ProcessExchange := ProcessRSTAndContinentExchange (ExchangeString, RData);

        RSTAndDomesticQTHOrZoneExchange:
            ProcessExchange := ProcessRSTAndDomesticQTHOrZoneExchange (ExchangeString, RData);

        RSTAndGridExchange:
            ProcessExchange := ProcessRSTAndGridSquareExchange (ExchangeString, RData);

        RSTAndOrGridExchange:
            ProcessExchange := ProcessRSTAndOrGridSquareExchange (ExchangeString, RData);

        RSTAndPostalCodeExchange:
            ProcessExchange := ProcessRSTAndPostalCodeExchange (ExchangeString, RData);

        RSTAndQSONumberOrDomesticQTHExchange:
            ProcessExchange := ProcessRSTAndQSONumberOrDomesticQTHExchange (ExchangeString, RData);

        RSTDomesticQTHExchange:
            ProcessExchange := ProcessRSTAndDomesticQTHExchange (ExchangeString, RData);

        RSTDomesticOrDXQTHExchange:
            ProcessExchange := ProcessRSTAndDomesticORDXQTHExchange (ExchangeString, RData);

{       RSTDomesticQTHOrQSONumberExchange:
            IF RData.DXQTH <> '' THEN
                ProcessExchange := ProcessRSTAndQSONumberExchange (ExchangeString, RData)
            ELSE
                ProcessExchange := ProcessRSTAndDomesticQTHExchange (ExchangeString, RData);

                Changed in 6.57

}
        RSTDomesticQTHOrQSONumberExchange:
            IF DomesticCountryCall (RData.Callsign) THEN
                ProcessExchange := ProcessRSTAndDomesticQTHExchange (ExchangeString, RData)
            ELSE
                ProcessExchange := ProcessRSTAndQSONumberExchange (ExchangeString, RData);

        RSTNameAndQTHExchange:
            ProcessExchange := ProcessRSTNameAndQTHExchange (ExchangeString, RData);

        RSTPossibleDomesticQTHAndPower:
            ProcessExchange := ProcessRSTPossibleDomesticQTHAndPowerExchange (ExchangeString, RData);

        RSTPowerExchange:
            ProcessExchange := ProcessRSTAndPowerExchange (ExchangeString, RData);

        RSTPrefectureExchange:
            ProcessExchange := ProcessRSTAndPrefectureExchange (ExchangeString, RData);

        RSTQSONumberExchange:
            ProcessExchange := ProcessRSTAndQSONumberExchange (ExchangeString, RData);

        NZFieldDayExchange:
            ProcessExchange := ProcessNZFieldDayExchange (ExchangeString, RData);

        RSTQSONumberAndDomesticQTHExchange:
            ProcessExchange := ProcessRSTQSONumberAndDomesticQTHExchange (ExchangeString, RData);

        RSTQSONumberAndGridSquareExchange:
            ProcessExchange := ProcessRSTQSONumberAndGridSquareExchange (ExchangeString, RData);

        RSTQSONumberAndPossibleDomesticQTHExchange:
            ProcessExchange := ProcessRSTQSONumberAndPossibleDomesticQTHExchange (ExchangeString, RData);

        QSONumberAndPossibleDomesticQTHExchange:
            ProcessExchange := ProcessQSONumberAndPossibleDomesticQTHExchange (ExchangeString, RData); {KK1L: 6.73}

        RSTQSONumberAndRandomCharactersExchange:
            ProcessExchange := ProcessRSTQSONumberAndRandomCharactersExchange (ExchangeString, RData);

        RSTQTHNameAndFistsNumberOrPowerExchange:
            ProcessExchange := ProcessRSTQTHNameAndFistsNumberOrPowerExchange (ExchangeString, RData);

        RSTQTHExchange:
            ProcessExchange := ProcessRSTAndQTHExchange (ExchangeString, RData);

        RSTZoneAndPossibleDomesticQTHExchange:
            ProcessExchange := ProcessRSTZoneAndPossibleDomesticQTHExchange (ExchangeString, RData);

        RSTZoneExchange:
            ProcessExchange := ProcessRSTAndZoneExchange (ExchangeString, RData);

        RSTZoneOrSocietyExchange:
            IF StringIsAllNumbersOrSpaces (ExchangeString) THEN
                ProcessExchange := ProcessRSTAndZoneExchange (ExchangeString, RData)
            ELSE
                BEGIN
                ExchangeString := UpperCase (ExchangeString);
                ProcessExchange := ProcessRSTAndDomesticQTHExchange (ExchangeString, RData);
                END;

        QSONumberPrecedenceCheckDomesticQTHExchange:
            ProcessExchange := ProcessQSONumberPrecedenceCheckDomesticQTHExchange (ExchangeString, RData);

        RSTLongJAPrefectureExchange: {KK1L: 6.72 JA}
            BEGIN
            ExchangeString := UpperCase (ExchangeString);
            ProcessExchange := ProcessRSTAndJAPrefectureExchange (ExchangeString, RData);
            END;

        END;  { of case }
    END;



PROCEDURE LogStringToRXData (LogString: Str80; VAR RXData: ContestExchange);

VAR ExchangeString: Str80;

    BEGIN
    ExchangeString := GetLogEntryExchangeString (LogString);

    IF ExchangeInformation.RST THEN RemoveFirstString (ExchangeString);

    ProcessExchange (ExchangeString, RXData);
    END;



PROCEDURE ProcessPartialCallAndInitialExchange (RXData: ContestExchange);

    BEGIN
    IF PartialCallEnable OR ExchangeMemoryEnable THEN
        AddCallToPartialList (RXData.Callsign, GetInitialExchangeStringFromContestExchange (RXData));
    END;



PROCEDURE CreateAndSendSAPMultiInfoMessage;

VAR TempString: Str80;
    NumberString: Str20;
    Index: INTEGER;

    BEGIN
    IF K1EANetworkEnable THEN Exit;

    TempString := MultiInfoMessage;

    IF TempString = '' THEN Exit;

    IF Pos ('$', TempString) > 0 THEN
        BEGIN
        Index := Pos ('$', TempString);
        Delete (TempString, Index, 1);
        Insert ('S&P', TempString, Index);
        END;

    IF Pos ('%', MultiInfoMessage) > 0 THEN
        BEGIN
        Index := Pos ('%', TempString);
        Delete (TempString, Index, 1);
        Str (Rate, NumberString);
        Insert (NumberString, TempString, Index);
        END;

    SendMultiInfoMessage (ActiveBand, ActiveMode, TempString);
    END;



PROCEDURE CreateAndSendCQMultiInfoMessage;

VAR TempString, NumberString: Str80;
    Index: INTEGER;
    Freq: LONGINT;
    FreqStr: Str20;

    BEGIN
    TempString := MultiInfoMessage;

    IF K1EANetworkEnable THEN
        BEGIN

        { Send run message with current stable frequency }

        CASE ActiveRadio OF
            RadioOne: Freq := StableRadio1Freq;
            RadioTwo: Freq := StableRadio2Freq;
            ELSE      Freq := 0;
            END;

        IF Freq > 0 THEN
            BEGIN
            Str (Freq, FreqStr);
            TempString := 'M' + K1EAStationID + ' ' + FreqStr + ' ';
            SendMultiMessage (TempString);
            UpdateK1EAStationInfo (Run, K1EAStationID, FreqStr);
            END;

        Exit;
        END;

    { TR Network }

    IF MultiInfoMessage = '' THEN Exit;

    IF Pos ('$', TempString) > 0 THEN
        BEGIN
        Index := Pos ('$', TempString);
        Delete (TempString, Index, 1);

        IF LastDisplayedFreq[RadioOne] > 0 THEN
            Str (LastDisplayedFreq[RadioOne], NumberString)
        ELSE
            IF RadioSetFreq > 0 THEN
                Str (RadioSetFreq, NumberString)
            ELSE
                NumberString := '00000';

        Delete (NumberString, Length (NumberString), 1);
        Delete (NumberString, Length (NumberString), 1);
        Insert ('.', NumberString, Length (NumberString));
        Insert (NumberString, TempString, Index);
        END;

    IF Pos ('%', MultiInfoMessage) > 0 THEN
        BEGIN
        Index := Pos ('%', TempString);
        Delete (TempString, Index, 1);
        Str (Rate, NumberString);
        Insert (NumberString, TempString, Index);
        END;

    SendMultiInfoMessage (ActiveBand, ActiveMode, TempString);
    END;


FUNCTION GetSentRSTFromExchangeString (VAR ExchangeString: Str40): Str20;

VAR PotentialRSTSent, TempString: Str40;
    RSTString: Str20;

    BEGIN
    GetSentRSTFromExchangeString := '';

    IF NOT LookForRSTSent THEN Exit;

    TempString := ExchangeString;

    WHILE TempString <> '' DO
        BEGIN
        PotentialRSTSent := RemoveLastString (TempString);

        IF Copy (PotentialRSTSent, 1, 1) = 'S' THEN
            BEGIN
            Delete (PotentialRSTSent, 1, 1);

            IF StringIsAllNumbers (PotentialRSTSent) THEN
                IF LooksLikeRST (PotentialRSTSent, RSTString, ActiveMode) THEN
                    BEGIN
                    GetSentRSTFromExchangeString := RSTString;

                    { Delete all the stuff we have }

                    Delete (ExchangeString, 1, Length (TempString));

                    { Remove the callsign }

                    RemoveFirstString (ExchangeString);

                    { Rebuild the exchange string without the callsign }

                    ExchangeString := TempString + ' ' + ExchangeString;
                    Exit;
                    END;

            END;
        END;
    END;



FUNCTION SpecAndExchangeMatch (Spec: Str40; Exchange: Str40): BOOLEAN;

    BEGIN
    SpecAndExchangeMatch := False;
    END;



FUNCTION FoundExchangeFromTemplate (Template: STRING; ExchangeString: Str40; VAR RData: ContestExchange): BOOLEAN;

VAR FormatSpec, TestSpec, TestExchange, Exchange: Str40;

    BEGIN
    FoundExchangeFromTemplate := False;

    FormatSpec := PrecedingString (Template, '|');

    Exchange := ExchangeString;

    WHILE (FormatSpec <> '') AND (Exchange <> '') DO
        BEGIN
        TestSpec := RemoveFirstString (FormatSpec);
        TestExchange := RemoveFirstString (Exchange);

        IF NOT SpecAndExchangeMatch (TestSpec, TestExchange) THEN Exit;
        END;

    { We have found that the format spec and the exchange match up }

    END;



FUNCTION ProcessTemplateExchange (ExchangeString: Str40; VAR RData: ContestExchange): BOOLEAN;

VAR Entry: INTEGER;

    BEGIN
    ProcessTemplateExchange := False;

    IF NumberExchangeTemplates = 0 THEN Exit;

    FOR Entry := 0 TO NumberExchangeTemplates - 1 DO
        IF FoundExchangeFromTemplate (ExchangeTemplateList [Entry]^, ExchangeString, RData) THEN
            BEGIN
            ProcessTemplateExchange := True;
            Exit;
            END;

    END;



PROCEDURE PassStationToCTNetwork;

VAR PassString: Str40;

    BEGIN
    IF NOT K1EANetworkEnable THEN Exit;

    PassString := QuickEditResponse ('Enter pass info (i.e., KQ2M 14150.2) : ', 30);

    IF PassString <> '' THEN
        SendMultiMessage ('P' + K1EAStationID + ' ' + PassString + ' ');
    END;



    BEGIN
    CD.CellBuffer.MaximumMemoryToUse := 40000;
    ExchangeErrorMessage := '';
    StuffInit;
    END.
