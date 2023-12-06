//
//Copyright Larry Tyree, N6TR, 2011,2012,2013,2014,2015,2022
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

UNIT TBSIQ_Subs;

{ Subroutines used by the 2BSIQ module and the QSOMachineObject for which tw o instances make up the operator
  interface }

{$O+}
{$V-}

INTERFACE

USES Dos, Tree, LogWind, LogDupe, LogStuff, ZoneCont, Country9,
     slowtree, so2r, LogCW, LogDVP, LogDom, Printer, LogK1EA, LogHelp, LogGrid, trCrt,
     jctrl2,LogPack,LogWAE, LogEdit,LogSCP,datetimec,radio,ctypes,xkb,timer,TBSIQ_CW,
     cfgcmd, k1eanet, foot, N4OGW, N1MM, LogUDP;

TYPE

    { These window names are part of template for the QSO machine object.  When you
      use the procedure SetTBSIQWindow, it will figure out which radio is being
      used and call the appropriate LogWind window }

    TBSIQ_WindowType = (TBSIQ_CallWindow,
                        TBSIQ_CWMessageWindow,
                        TBSIQ_ExchangeWindow,
                        TBSIQ_InsertWindow,
                        TBSIQ_PossibleCallWindow,
                        TBSIQ_QSONumberWindow,
                        TBSIQ_StartSendingWindow,
                        TBSIQ_StateMachineStatusWindow);

    { High level state machine states }

    TBSIQ_QSOStateType = (QST_None,
                          QST_Idle,
                          QST_CallingCQ,
                          QST_AutoCQCalling,
                          QST_AutoCQListening,
                          QST_CQCalled,
                          QST_AltDInput,
                          QST_AutoStartSending,
                          QST_CQStationBeingAnswered,
                          QST_CQStationBeingAnsweredSilent,
                          QST_CQExchangeBeingSent,
                          QST_CQExchangeBeingSentAndExchangeWindowUp,
                          QST_CQWaitingForExchange,
                          QST_CQSending73Message,
                          QST_SearchAndPounceInit,
                          QST_SearchAndPounce,
                          QST_SearchAndPounceSpaceBarPressed,
                          QST_SendingKeyboardCW,
                          QST_StartSendingKeyboardCW,
                          QST_SendingKeyboardCWWaiting);

    QSOMachineObject = CLASS
        { These paramaters need to be set for the specific instance using the
          InitializeQSOMachne procedure }

        KeyboardFileDescriptor: CINT;  { Indicates which keyboard file to use }
        Radio: RadioType;              { Indicates which radio to use }
        WindowLocationX: INTEGER;      { X and Y location of upper left corner }
        WindowLocationY: INTEGER;

        { Internal Variables }

        AutoCQDelayTime: INTEGER;      { In 100's of milliseconds }
        AutoCQFinishTime: TimeRecord;  { Marks time when Auto-CQ finished }
        AutoCQMemory: CHAR;            { Function Key memory to send for AutoCQ }

        AutoStartSendEnable: BOOLEAN;         { Cleverly named different than the global AutoSendEnable }
        AutoStartSendCharacterCount: INTEGER; { Cleverly named different than the global AutoSendCharacterCount }
        AutoStartSendStationCalled: BOOLEAN;

        Band: BandType;
        BandMapCallPutUp: CallString;
        BeSilent: BOOLEAN;   { Used to indicate we should not send CW for command }

        CallsignICameBackTo: STRING;
        CallWindowString: STRING;
        CallWindowCursorPosition: INTEGER;
        CharacterInput: CHAR;             { Used to send letters to the other radio using AltD }
        ClearKeyCache: BOOLEAN;
        CodeSpeed: INTEGER;
        CWMessageDisplayed: STRING;

        DisablePutUpBandMapCall: BOOLEAN;
        DisplayedBand: BandType;
        DisplayedFrequency: LONGINT;
        DisplayedInsertIndicator: InsertIndicatorType;
        DisplayedMode: ModeType;
        DisplayedTXColor: TXColorType;
        DoingPTTTest: BOOLEAN;

        DualModeMemory: BOOLEAN;    { Used to remember that a keystroke has already happened }
        DualModeKey: CHAR;
        DualModeExtendedKey: CHAR;

        DupeShown: BOOLEAN;
        DupeShownCallsign: CallString;
        DupeShownTime: TimeRecord;

        ExchangeWindowIsUp: BOOLEAN;
        ExchangeWindowString: STRING;
        ExchangeWindowCursorPosition: INTEGER;

        Frequency: LONGINT;                 { The most current frequency for the radio }

        InitialExchangePutUp: BOOLEAN;

        K3RXPollActive: BOOLEAN;
        KeyboardCWMessage: STRING;

        LastDisplayedQSONumber: LONGINT;
        LastFrequency: LONGINT;
        LastFullTimeString: STRING;          { Used for 1 second timer }
        LastFunctionKeyTime: TimeRecord;
        LastPartialCall: CallString;
        LastQSOState: TBSIQ_QSOStateType;
        LastPossibleCall: CallString;
        LastSCPCall: CallString;             { The last call processed by the SCP engine }

        LocalInsertMode: BOOLEAN;
        LoggedSAndPCall: CallString;
        LoggedSAndPCallTime: TimeRecord;

        Mode: ModeType;

        OkaytoPutUpBandMapCall: BOOLEAN;

        TBSIQPossibleCallList: CallListRecord;  { We have specific ones for each instance }

        PTTState: BOOLEAN;
        PTTTestTimer: INTEGER;

        QSONumberForThisQSO: INTEGER;
        QSONumberForPreviousQSO: INTEGER;
        QSONumberUnused: BOOLEAN;

        QSOState: TBSIQ_QSOStateType;

        RadioFrequencySettledCount: INTEGER;
        RadioMovingInBandMode: BOOLEAN;      { Replaces the classis RadioMovingInBandMode [radio] }
        RadioOnTheMove: BOOLEAN;             { Replaces the classic RadioOntheMove [radio] }
        RadioInterfaced: InterfacedRadioType;
        RememberQSOState: TBSIQ_QSOStateType;

        SCPScreenFull: BOOLEAN;
        SearchAndPounceStationCalled: BOOLEAN;
        SearchAndPounceExchangeSent: BOOLEAN;

        { SSBTransmissionStarted is intended to fill in the gap from when you initiate
          a tranmission on SSB until the radio has told you it is transmitting.  This
          typically can take a second or so.  It is used to keep the other transmitting
          from initiating a tranmission during this gap. }

        SSBTransmissionStarted: BOOLEAN;

        StationInformationCall: CallString;

        TBSIQ_ActiveWindow: TBSIQ_WindowType;

        TransmitCountDown: INTEGER;      { Set > 0 for # of seconds to fake "I am transmitting" }

        PROCEDURE AppendCWMessageDisplay (Message: STRING);

        PROCEDURE CheckQSOStateMachine;
        PROCEDURE ClearAutoStartSendDisplay;  { for use during S&P }

        FUNCTION  DisableTransmitting: BOOLEAN;

        PROCEDURE DisplayActiveRadio;              { Shows TX next to active radio }
        PROCEDURE DisplayAutoStartSendCharacterCount;
        PROCEDURE DisplayBandMode;
        PROCEDURE DisplayCodeSpeed;
        PROCEDURE DisplayFrequency;
        PROCEDURE DisplayInsertMode;
        PROCEDURE DisplayPossibleCalls (VAR List: CallListRecord);
        PROCEDURE DisplayQSONumber;
        PROCEDURE DisplaySCPCall (Call: CallString);
        PROCEDURE DisplayTXColor;     { Red, Yellow or Blue}
        PROCEDURE DisplayUserInfo (Call: CallString);
        PROCEDURE DoPossibleCalls (Callsign: CallString);

        FUNCTION  ExpandCrypticString (SendString: STRING): STRING;

        PROCEDURE SetUpNextQSONumber;

        FUNCTION  IAmTransmitting: BOOLEAN;

        PROCEDURE InitializeQSOMachine (KBFile: CINT;
                                        RadioID: RadioType;
                                        WinX, WinY: INTEGER);

        FUNCTION  LegalKey (KeyChar: CHAR): BOOLEAN;

        PROCEDURE ListenToBothRadios;
        PROCEDURE ListenToOtherRadio;

        PROCEDURE PTTTest;

        PROCEDURE ReadSMeter;
        PROCEDURE RemoveExchangeWindow;
        PROCEDURE RemovePossibleCallWindow;
        PROCEDURE RemoveQSONumberWindow;

        PROCEDURE SendFunctionKeyMessage (Key: CHAR; VAR Message: STRING);

        PROCEDURE SendKeyboardInput;
        PROCEDURE SetCodeSpeed (Speed: INTEGER);
        PROCEDURE SetTBSIQWindow (TBSIQ_Window: TBSIQ_WindowType);
        PROCEDURE ShowCWMessage (Message: STRING);
        PROCEDURE ShowDupeMessage (Message: STRING);
        PROCEDURE ShowStateMachineStatus;
        PROCEDURE ShowStationInformation (Call: CallString);
        PROCEDURE ShowTransmitStatus;
        FUNCTION  SMeterReading: INTEGER;
        PROCEDURE SuperCheckPartial;
        PROCEDURE SwapWindows;     { Moves from exchange <> CQ window }

        PROCEDURE UpdateRadioDisplay;  { Band/mode/frequency }

        FUNCTION  WindowDupeCheck: BOOLEAN;

        PROCEDURE WindowEditor (VAR WindowString: Str80;
                                VAR KeyChar: CHAR;
                                VAR ExtendedKeyChar: CHAR;
                                VAR ActionRequired: BOOLEAN);

        PROCEDURE WriteCharacter (Ch: CHAR);
        END;

    KeyStatusRecord = RECORD
        KeyPressedCode: LONGINT;   { Remembered KB_Code }
        KeyChar: CHAR;             { Character for ReadKey }
        ExtendedKey: BOOLEAN;      { Indicates that the character is from an extended key }
        KeyPressed: BOOLEAN;       { Indicates that we have a key ready to share }

        ExtendedKeyNullSent: BOOLEAN;  { Indicates if the null return for an extended key has been sent }

        { These remember which keys have been pressed and not yet released }

        LeftShiftKeyPressed:    BOOLEAN;
        RightShiftKeyPressed:   BOOLEAN;
        LeftAltKeyPressed:      BOOLEAN;
        RightAltKeyPressed:     BOOLEAN;
        LeftControlKeyPressed:  BOOLEAN;
        RightControlKeyPressed: BOOLEAN;
        END;

VAR

    R1KeyboardID: CINT;
    R2KeyboardID: CINT;

    Radio1KeyStatus: KeyStatusRecord;
    Radio2KeyStatus: KeyStatusRecord;

    { Well - just by assiging these are a variable, the objects will get created on
      the heap automagically }

    Radio1QSOMachine: QSOMachineObject;
    Radio2QSOMachine: QSOMachineObject;

    RData: ContestExchange;  { Used to talk between TBSIQ_ParametersOkay and TBSIQ_LogContact }

    TBSIQ_BandMapFocus: RadioType;   { gets used for the visible dupesheet too }
    TBSIQ_SCPFocus: RadioType;       { which radio has precedence over SCP display }


FUNCTION  InitializeKeyboards: BOOLEAN;
FUNCTION  NewInitializeKeyboards: BOOLEAN;

PROCEDURE TBSIQ_CheckDualingCQState;
PROCEDURE TBSIQ_CheckTestLoopState;
FUNCTION  TBSIQ_KeyPressed (Radio: RadioType): BOOLEAN;  { Radio = 1 or 2 }
PROCEDURE TBSIQ_LogContact (VAR RXData: ContestExchange);

FUNCTION  TBSIQ_ParametersOkay (Call: CallString; QSONumberSent: INTEGER; ExchangeString: Str80;
                                Band: BandType; Mode: ModeType; Freq: LONGINT;
                                VAR RData: ContestExchange): BOOLEAN;

PROCEDURE TBSIQ_PushLogStringIntoEditableLogAndLogPopedQSO (LogString: Str80; MyQSO: BOOLEAN);
PROCEDURE TBSIQ_PutContactIntoLogFile (LogString: Str80);
FUNCTION  TBSIQ_ReadKey (Radio: RadioType): CHAR;
PROCEDURE TBSIQ_UpdateTimeAndRateDisplays;  { Not radio specific }

PROCEDURE PaintVerticalLine;
FUNCTION  ValidFunctionKey (Key: CHAR): BOOLEAN;

IMPLEMENTATION

USES KeyCode, blcksock, Sockets, BaseUnix;  { last three are for TCP support }

TYPE
    FileRecord = RECORD  { This is the data record that we read off of the keyboard "files"  }
        Sec:  QWORD;
        USec: QWORD;
        KB_Type: WORD;
        KB_Code: WORD;
        KB_Value: LONGINT;
        END;

    DualingCQStates = (NoDualingCQs,
                       DualingCQOnRadioOne,
                       DualingCQOnRadioTwo);

    TestLoopStates = (NoTestLoop,
                      TestLoopStart,
                      TestLoopSendingMessage,
                      TestLoopDelay1,
                      TestLoopAssertingPTT,
                      TestLoopDelay2,
                      TestLoopTestSmeter,
                      TestLoopStop);

CONST InitialTransmitCountdown = 3;

VAR DualingCQState: DualingCQStates;
    LoopCount: INTEGER;
    TestLoopDelayCount: INTEGER;
    TestLoopState:      TestLoopStates;



PROCEDURE QSOMachineObject.SetUpNextQSONumber;

{ Normally, this would come from GetNextQSONumber in LOGWIND, however, if the QSO
  number on the other radio hasn't been used - we can steal it.  Note that we will
  never steal a QSO number that is less than the one we just sent.

  However, if QSONumberByBand is true - we can short circuit all of this logic and
  just generate a QSO number that is one more than the QSO totals for the ActiveBand }

VAR TempQSOTotals: QSOTotalArray;

    BEGIN
    IF QSONumberByBand THEN
        BEGIN
        TempQSOTotals := QSOTotals;    { Get global QSO totals in dupesheet }
        VisibleLog.IncrementQSOTotalsWithContentsOfEditableWindow (TempQSOTotals);  { Add in EditableLog QSOs }
        QSONumberForThisQSO := TempQSOTotals [Band, Both] + 1;
        Exit;
        END;

    CASE Radio OF
        RadioOne:
            IF Radio2QSOMachine.QSONumberUnused THEN   { Steal QSO number from Radio 2 }
                IF Radio2QSOMachine.QSONumberForThisQSO > QSONumberForThisQSO THEN
                    BEGIN
                    QSONumberForThisQSO := Radio2QSOMachine.QSONumberForThisQSO;
                    Radio2QSOMachine.QSONumberForThisQSO := GetNextQSONumber;
                    Radio2QSOMachine.DisplayQSONumber;
                    Exit;
                    END;

        RadioTwo:
            IF Radio1QSOMachine.QSONumberUnused THEN
                IF Radio1QSOMachine.QSONumberForThisQSO > QSONumberForthisQSO THEN
                    BEGIN
                    QSONumberForThisQSO := Radio1QSOMachine.QSONumberForThisQSO;
                    Radio1QSOMachine.QSONumberForThisQSO := GetNextQSONumber;
                    Radio1QSOMachine.DisplayQSONumber;
                    Exit;
                    END;

        END;  { of CASE Radio }

    { We can't steal the QSO number from the other radio - so get our own new number }

    QSONumberForThisQSO := GetNextQSONumber;   { from LOGWIND }
    QSONumberUnused := True;
    END;



PROCEDURE TBSIQ_CheckDualingCQState;

{ Checks to see if something should be done with the dualing CQs }

VAR Message: STRING;
    QSONumberString: Str20;

    BEGIN
    IF DualingCQState = NoDualingCQs THEN Exit;

    IF DualingCQState = DualingCQOnRadioOne THEN
        BEGIN
        IF Radio1QSOMachine.IAmTransmitting THEN Exit;

        { Done with the CQ - now to to the other radio }

        ActiveKeyer.SetActiveRadio (RadioTwo);
        SetUpToSendOnActiveRadio;

        WITH Radio2QSOMachine DO
            BEGIN
            SendFunctionKeyMessage (AltF1, Message);
            ListenToOtherRadio;
            Str (QSONumberForThisQSO, QSONumberString);
            NewBandMapEntry ('CQ/' + QSONumberString, Frequency, 0, Mode, False, False, BandMapDecayTime, True);
            LastCQFrequency := Frequency;
            LastCQMode := Mode;
            END;

        DualingCQState := DualingCQOnRadioTwo;
        END;

    IF DualingCQState = DualingCQOnRadioTwo THEN
        BEGIN
        IF Radio2QSOMachine.IAmTransmitting THEN Exit;

        ActiveKeyer.SetActiveRadio (RadioOne);
        SetUpToSendOnActiveRadio;

        WITH Radio1QSOMachine DO
            BEGIN
            SendFunctionKeyMessage (AltF1, Message);
            ListenToOtherRadio;
            Str (QSONumberForThisQSO, QSONumberString);
            NewBandMapEntry ('CQ/' + QSONumberString, Frequency, 0, Mode, False, False, BandMapDecayTime, True);
            LastCQFrequency := Frequency;
            LastCQMode := Mode;
            END;

        DualingCQState := DualingCQOnRadioOne;
        END;
    END;



PROCEDURE TBSIQ_CheckTestLoopState;

VAR SMeter: INTEGER;
    TempString, SMeterReadingString: STRING;

{ Set TestLoopDelayCount to number of seconds and test when it goes to zero }

    BEGIN
    CASE TestLoopState OF
        NoTestLoop:
            BEGIN
            TestLoopDelayCount := 0;
            LoopCount := 0;
            Exit;
            END;

        TestLoopStart:   { First thing we need to do is play a message on radio 1 }
            BEGIN
            IF TestLoopDelayCount > 0 THEN Exit;   { In case someone wanted a delay }

            Radio2QSOMachine.ShowCWMessage ('Starting DVK');
            Rig2.DirectCommand ('SW17;');
            TestLoopDelayCount := 10;
            TestLoopState := TestLoopSendingMessage;
            Inc (LoopCount);
            ClrScr;
            Write ('Loops = ', LoopCount);
            END;

        TestLoopSendingMessage:
            BEGIN
            Radio2QSOMachine.ShowCWMessage ('Sending DVK Message');
            IF TestLoopDelayCount > 0 THEN Exit;

            { We are done with the DVK - now turn on the PTT }

            Radio2QSOMachine.ShowCWMessage ('Attempting to turn on PTT');

            ActiveKeyer.SetActiveRadio (RadioTwo);
            ActiveKeyer.PTTForceOn;
            TestLoopDelayCount := 5;

            TestLoopState := TestLoopAssertingPTT;
            END;

        TestLoopAssertingPTT:
            BEGIN
            Radio2QSOMachine.ShowCWMessage ('PTT on - waiting');
            IF TestLoopDelayCount > 1 THEN Exit;

            { Now - take an Smeter reading on radio two }

            SMeter := Radio1QSOMachine.SMeterReading;
            Str (SMeter, SMeterReadingString);
            Radio1QSOMachine.ShowCWMessage (SMeterReadingString);

            IF SMeter <= 6 THEN
                BEGIN
                Str (LoopCount, TempString);
                Radio1QSOMachine.ShowCWMessage ('FAILURE!! Loops = ' + TempString);
                ActiveKeyer.PTTUnforce;
                TestLoopState := TestLoopStop;
                END
            ELSE
                BEGIN
                ActiveKeyer.PTTUnforce;
                TestLoopDelayCount := 3;
                TestLoopState := TestLoopStart;
                END;
            END;

        TestLoopDelay2:
            BEGIN
            END;

        TestLoopTestSmeter:
            BEGIN
            END;

        TestLoopStop:
            BEGIN
            { We just stay here }
            END;

        END;  { of CASE TestLoopState }
    END;



PROCEDURE QSOMachineObject.DisplayUserInfo (Call: CallString);

VAR UserInfoString: STRING;

    BEGIN
    UserInfoString := GetUserInfoString (Call);

    IF UserInfoString <> '' THEN
        BEGIN
        CASE Radio OF
            RadioOne: SaveSetAndClearActiveWindow (TBSIQ_R1_UserInfoWindow);
            RadioTwo: SaveSetAndClearActiveWindow (TBSIQ_R2_UserInfoWindow);
            END;

        Write (UserInfoString);
        RestorePreviousWindow;
        END
    ELSE
        CASE Radio OF
            RadioOne: RemoveWindow (TBSIQ_R1_UserInfoWindow);
            RadioTwo: RemoveWindow (TBSIQ_R2_UserInfoWindow);
            END;
    END;



PROCEDURE QSOMachineObject.ShowStationInformation (Call: CallString);

VAR Name: STRING;

    BEGIN
    IF Copy (Call, 1, 3) = 'CQ-' THEN Exit;

    { ShowName Call); }

    Name := CD.GetName (RootCall (Call));

    IF Name <> '' THEN
        BEGIN
        CASE Radio OF
            RadioOne: SaveSetAndClearActiveWindow (TBSIQ_R1_NameWindow);
            RadioTwo: SaveSetAndClearActiveWindow (TBSIQ_R2_NameWindow);
            END;

        Write (Name);
        RestorePreviousWindow;
        END
    ELSE
        CASE Radio OF
            RadioOne: RemoveWindow (TBSIQ_R1_NameWindow);
            RadioTwo: RemoveWindow (TBSIQ_R2_NameWindow);
            END;

    { IF QTCsEnabled THEN
          DisplayQTCNumber (NumberQTCsThisStation (StandardCallFormat (Call, False)));  }

    { I guess for now - I will use the standard windows for this }

    IF ContestName <> 'General QSOs' THEN
        BEGIN
        VisibleLog.ShowMultiplierStatus (Call);
        VisibleLog.ShowQSOStatus        (Call);
        END;

    IF ActiveDomesticMult <> GridSquares THEN
        DisplayBeamHeading (Call);

    DisplayUserInfo (Call);
    END;




PROCEDURE QSOMachineObject.SetCodeSpeed (Speed: INTEGER);

    BEGIN
    DisplayCodeSpeed;
    SpeedMemory [Radio] := CodeSpeed;

    { Only set the speed immediately if the other radio isn't sending something }

    CASE Radio OF
        RadioOne:
            IF TBSIQ_CW_Engine.CWFinished (RadioTwo) THEN
                BEGIN
                ActiveKeyer.SetActiveRadio (RadioOne);
                SetSpeed (CodeSpeed);
                SetRelayForActiveRadio (ActiveRadio);
                SetRelayForActiveRadio (ActiveRadio);
                END;

        RadioTwo:
            IF TBSIQ_CW_Engine.CWFinished (RadioOne) THEN
                BEGIN
                ActiveKeyer.SetActiveRadio (RadioTwo);
                SetSpeed (CodeSpeed);
                SetRelayForActiveRadio (ActiveRadio);
                SetRelayForActiveRadio (ActiveRadio);
                END;

        END;  { of case }
    END;



PROCEDURE QSOMachineOBject.DoPossibleCalls (Callsign: CallString);

VAR List: CallListRecord;

    BEGIN
    IF LastPossibleCall = Callsign THEN Exit;
    LastPossibleCall := Callsign;

    IF NOT PossibleCallEnable THEN Exit;

    VisibleLog.GeneratePossibleCalls (Callsign, Band, Mode, List);
    DisplayPossibleCalls (List);
    END;



PROCEDURE CreateAndSendPacketSpot (PacketSpotCall: CallString;
                                   PacketSpotFreq: LONGINT);

VAR TempString: Str80;

    BEGIN
    { Make sure this isn't a CQ Entry in the bandmap }

    IF NOT LooksLikeACallsign (PacketSpotCall) THEN Exit;

    IF PrecedingString  (PacketSpotCall, '/') = 'CQ' THEN
        Exit;

    IF Length (PacketSpotCall) = 8 THEN
        IF Copy (PacketSpotCall, 1, 4) = Copy (PacketSpotCall, 5, 4) THEN
            Exit;

    IF Length (PacketSpotCall) = 10 THEN
        IF Copy (PacketSpotCall, 1, 5) = Copy (PacketSpotCall, 6, 5) THEN
            Exit;

    IF Length (PacketSpotCall) = 12 THEN
        IF Copy (PacketSpotCall, 1, 6) = Copy (PacketSpotCall, 7, 5) THEN
            Exit;

    Str (PacketSpotFreq, TempString);

    Delete (TempString, Length (TempString) - 1, 2);
    Insert ('.', TempString, Length (TempString));

    TempString := 'DX ' + TempString + ' ' + PacketSpotCall;
    SendPacketMessage (TempString + CarriageReturn);
    END;



FUNCTION QSOMachineObject.WindowDupeCheck: BOOLEAN;

{ Taken from LOGSUBS2.  Returns TRUE if the CallWindow is a dupe.
  It is assumed you are in the CallWindow }

VAR MultString: Str40;
    Mult: BOOLEAN;

    BEGIN
    WindowDupeCheck := False;

    IF Length (CallWindowString) < 2 THEN Exit;

    BandMapBand := Band;
    BandMapMode := Mode;

    { Might we want to send a spot to packet? }

    IF QSOState = QST_SearchAndPounce THEN
        IF Packet.AutoSpotEnable THEN
            IF ActivePacketPort <> nil THEN
                IF NOT PacketSpotDisable THEN
                    CreateAndSendPacketSpot (CallWindowString, Frequency);

    IF VisibleLog.CallIsADupe (CallWindowString, Band, Mode) THEN
        BEGIN
        IF TBSIQ_ActiveWindow = TBSIQ_ExchangeWindow THEN SwapWindows;;
        WindowDupeCheck := True;
        GoToXY (Length (CallWindowString) + 1, WhereY);
        Write (' DUPE');

        ShowDupeMessage (CallWindowString + ' is a DUPE!');
        DupeShown := True;
        DupeShownCallsign := CallWindowString;
        MarkTime (DupeShownTime);
        DisplayEditableLog (VisibleLog.LogEntries);

        { Panic delete if I pressed F1 by mistake }

        IF KeyRecentlyPressed (F1, 200) THEN    { Withing two seconds }
            IF Mode = CW THEN
                TBSIQ_CW_Engine.ClearMessages (Radio, True);

        ShowStationInformation (CallWindowString);
        DoPossibleCalls (CallWindowString);

        IF BandMapEnable AND (QSOState = QST_SearchAndPounce) THEN
            BEGIN
            BandMapCursorFrequency := DisplayedFrequency;
            BandMapMode := Mode;
            BandMapBand := Band;
            TBSIQ_BandMapFocus := Radio;
            BandMapCallPutUp := CallWindowString;

            { First True is dupe - second one is SendToMulti.  We use False for
              the mult flag since dupes seldom are mults }

            NewBandMapEntry (CallWindowString, DisplayedFrequency, 0, Mode, True, False, BandMapDecayTime, True);
            DisablePutUpBandMapCall := False;
            END;

        IF QSOState <> QST_SearchAndPounce THEN
            BEGIN
            RemoveExchangeWindow;
            QSOState := QST_Idle;
            END
        ELSE
            BEGIN
            BandMapCursorFrequency := DisplayedFrequency;
            BandMapMode := Mode;
            BandMapBand := Band;
            TBSIQ_BandMapFocus := Radio;
            BandMapCallPutUp := CallWindowString;

            { First True is dupe - second one is SendToMulti.  We use False for
              the mult flag since dupes seldom are mults }

            NewBandMapEntry (CallWindowString, Frequency, 0, Mode, True, False, BandMapDecayTime, True);

            SwapWindows;
            ClrScr;
            ExchangeWindowString := '';
            ExchangeWindowCursorPosition := 1;
            SwapWindows;
            OkayToPutUpBandMapCall := False;
            DisablePutUpBandMapCall := False;
            END;

        DisplayInsertMode;

        IF NOT QTCsEnabled THEN
            BEGIN
            EscapeDeletedCallEntry := CallWindowString;
            CallWindowString := '';
            END;

        ClrScr;
        CallWindowString := '';
        CallWindowCursorPosition := 1;
        END  { of dupe }

    ELSE
        BEGIN  { Not a dupe }
        ShowStationInformation (CallWindowString);
        DoPossibleCalls (CallWindowString);

        IF BandMapEnable AND (QSOState = QST_SearchAndPounce) THEN
            BEGIN
            VisibleLog.DetermineIfNewMult (CallWindowString, ActiveBand, ActiveMode, MultString);
            Mult := MultString <> '';

            { False = not a dupe  True = SendToMulti }

            BandMapCursorFrequency := DisplayedFrequency;
            BandMapMode := Mode;
            BandMapBand := Band;
            TBSIQ_BandMapFocus := Radio;
            BandMapCallPutUp := CallWindowString;

            { First false is not dupe - first true is SendToMulti }

            NewBandMapEntry (CallWindowString, DisplayedFrequency, 0, ActiveMode, False, Mult, BandMapDecayTime, True);
            DisablePutUpBandMapCall := False;
            END;
        END;

    END;


FUNCTION FoundCommand (Radio: RadioType; VAR SendString: Str160): BOOLEAN;

{ Taken from LOGSUBS2 and customized for this environment }

VAR FileName, CommandString: Str40;
    TempInt: INTEGER;

    BEGIN
    FoundCommand := False;

    CommandUseInactiveRadio := FALSE; {KK1L: 6.73 Global var to support vectoring commands to inactive radio}

    WHILE StringHas (SendString, ControlC) DO
        BEGIN
        IF NOT StringHas (SendString, ControlD) THEN Exit;

        FoundCommand := StringHas (SendString, ControlD);

        CommandString := UpperCase (BracketedString (SendString, ControlC, ControlD));
        Delete (SendString, Pos (ControlC, SendString), Pos (ControlD, SendString) - Pos (ControlC, SendString) + 1);

        IF Copy (CommandString, 1, 1) = ControlA THEN  {KK1L: 6.73 Vector commands to inactive radio with CTRL-A}
            BEGIN
            CommandUseInactiveRadio := TRUE;
            Delete (CommandString, 1, 1);
            END;

        IF StringHas (CommandString, '=') THEN
            BEGIN
            FileName := PostcedingString (CommandString, '=');
            CommandString := PrecedingString (CommandString, '=');
            END;

        IF CommandString = 'BANDUP' THEN
            BEGIN
            RememberFrequency; {KK1L: 6.72 Added to match all other calls. Needed for loss of coms}
            BandUp;
            END;

        IF CommandString = 'BANDDOWN' THEN
            BEGIN
            RememberFrequency; {KK1L: 6.72 Added to match all other calls. Needed for loss of coms}
            BandDown;
            END;

        IF CommandString = 'CONTROLENTER'   THEN CWMessageCommand := CWCommandControlEnter;
        IF CommandString = 'CQMODE'         THEN CWMessageCommand := CWCommandCQMode;
        IF CommandString = 'CWENABLETOGGLE' THEN CWEnable := NOT CWEnable;

        IF CommandString = 'CWMONITORON' THEN
            BEGIN
            IF OldCWTone = 0 THEN OldCWTone := 700;
            CWTone := OldCWTone;
            AddStringToBuffer ('', CWTone);
            END;

        IF CommandString = 'DVKDELAY' THEN
            IF StringIsAllNumbers (FileName) THEN
                BEGIN
                Val (FileName, TempInt);
                SetDVKDelay (TempInt);
                END;

        IF CommandString = 'CWMONITOROFF'   THEN
            IF CWTone <> 0 THEN
                BEGIN
                OldCWTone := CWTone;
                CWTone := 0;
                AddStringToBuffer ('', CWTone);
                END;

        IF CommandString = 'DISABLECW'      THEN CWEnable := False;
        IF CommandString = 'ENABLECW'       THEN CWEnable := True;

        IF Commandstring = 'NEXTBANDMAP'    THEN
            BEGIN
            IF NOT CommandUseInactiveRadio THEN {KK1L: 6.73 applies to active radio only}
                CallWindowString := ''; {KK1L: 6.68 forces band map call to callwindow}

            GoToNextBandMapFrequency;

            IF NOT CommandUseInactiveRadio THEN {KK1L: 6.73 applies to active radio only}
                CWMessageCommand := CWCommandSAPMode;  {KK1L: 6.68 Takes you to S&P mode when surfing band map}
            END;

        {KK1L: 6.64 finds next entry from displayed bandmap rather than just current band/mode}

        IF Commandstring = 'NEXTDISPLAYEDBANDMAP'    THEN
            BEGIN
            IF NOT CommandUseInactiveRadio THEN {KK1L: 6.73 applies to active radio only}
                CallWindowString := ''; {KK1L: 6.68 forces band map call to callwindow}

            GoToNextDisplayedBandMapFrequency;

            IF NOT CommandUseInactiveRadio THEN {KK1L: 6.73 applies to active radio only}
                CWMessageCommand := CWCommandSAPMode;  {KK1L: 6.68 Takes you to S&P mode when surfing band map}
            END;

        {KK1L: 6.68}
        IF Commandstring = 'NEXTMULTBANDMAP'    THEN
            BEGIN
            IF NOT CommandUseInactiveRadio THEN {KK1L: 6.73 applies to active radio only}
                CallWindowString := ''; {KK1L: 6.68 forces band map call to callwindow}

            GoToNextMultBandMapFrequency;

            IF NOT CommandUseInactiveRadio THEN {KK1L: 6.73 applies to active radio only}
                CWMessageCommand := CWCommandSAPMode;  {KK1L: 6.68 Takes you to S&P mode when surfing band map}
            END;
        {KK1L: 6.68}
        IF Commandstring = 'NEXTMULTDISPLAYEDBANDMAP'    THEN
            BEGIN
            IF NOT CommandUseInactiveRadio THEN {KK1L: 6.73 applies to active radio only}
                CallWindowString := ''; {KK1L: 6.68 forces band map call to callwindow}

            GoToNextMultDisplayedBandMapFrequency;

            IF NOT CommandUseInactiveRadio THEN {KK1L: 6.73 applies to active radio only}
                CWMessageCommand := CWCommandSAPMode;  {KK1L: 6.68 Takes you to S&P mode when surfing band map}
            END;

        IF CommandString = 'LASTCQFREQ'     THEN
            BEGIN
            IF NOT CommandUseInactiveRadio THEN {KK1L: 6.73 applies to active radio only}
                CallWindowString := ''; {KK1L: 6.69 clears callwindow}

            GoToLastCQFrequency;

            IF NOT CommandUseInactiveRadio THEN {KK1L: 6.73 applies to active radio only}
                CWMessageCommand := CWCommandCQMode;   {KK1L: 6.68 Takes you to CQ mode when returning to CQ freq}
            END;

        IF CommandString = 'QSY'            THEN CWMessageCommand := CWCommandQSY;

        IF CommandString = 'SAPMODE'        THEN CWMessageCommand := CWCommandSAPMode;

        IF Copy (CommandString, 1, 5) = 'SPEED' THEN
            BEGIN
            Delete (CommandString, 1, 5);

            IF StringIsAllNumbers (CommandString) THEN
                BEGIN
                Val (CommandString, TempInt);
                SetSpeed (TempInt);
                DisplayCodeSpeed (CodeSpeed, CWEnabled, DVPOn, ActiveMode);
                END
            ELSE
                BEGIN
                WHILE Copy (CommandString, 1, 1) = '+' DO
                    BEGIN
                    Delete (CommandString, 1, 1);

                    IF CodeSpeed < 99 THEN
                        BEGIN
                        SetSpeed (CodeSpeed + 1);
                        DisplayCodeSpeed (CodeSpeed, CWEnabled, DVPOn, ActiveMode);
                        END;
                    END;

                WHILE Copy (CommandString, 1, 1) = '-' DO
                    BEGIN
                    Delete (CommandString, 1, 1);

                    IF CodeSpeed > 1 THEN
                        BEGIN
                        SetSpeed (CodeSpeed - 1);
                        DisplayCodeSpeed (CodeSpeed, CWEnabled, DVPOn, ActiveMode);
                        END;
                    END;
                END;
            END;

        IF CommandString = 'SO2R' THEN
            BEGIN
            if filename = 'RX1' then so2rbox.setrcvfocus(RX1);
            if filename = 'RX2' then so2rbox.setrcvfocus(RX2);
            if filename = 'STEREO' then so2rbox.setrcvfocus(STEREO);
            if filename = 'LATCHON' then so2rbox.setlatch(true);
            if filename = 'LATCHOFF' then so2rbox.setlatch(false);

            if filename = 'LATCHTOGGLE' then
                so2rbox.setlatch(not so2rbox.getlatch);

            if filename = 'RXA' then
                IF  Radio = Radioone then
                    so2rbox.setrcvfocus(RX1)
                else
                    so2rbox.setrcvfocus(RX2);

            if filename = 'RXI' then
                IF Radio = radioone then
                    so2rbox.setrcvfocus(RX2)
                else
                    so2rbox.setrcvfocus(RX1);

//            IF SendString <> '' THEN
//                REPEAT millisleep UNTIL RadioSendBufferEmpty (ActiveRadio);
            END;


        IF CommandString = 'SRS' THEN
            BEGIN
            { If someone is sending an SRS command and the mode if SSB, we should
              probably assume they are sending a voice message.  We should set
              the TransmitCountdown counter. }

            CASE Radio OF
                RadioOne: Radio1QSOMachine.TransmitCountdown := InitialTransmitCountdown;
                RadioTwo: Radio2QSOMachine.TransmitCountdown := InitialTransmitCountdown;
                END;

            IF Radio = radioone then
                rig1.directcommand (filename)
            ELSE
                rig2.directcommand (filename);
            END;


        IF CommandString = 'SRS1' THEN
            Rig1.directcommand (filename);

        IF CommandString = 'SRS2' THEN
            Rig2.directcommand (filename);

        IF CommandString = 'SRSI' THEN
            IF Radio = radioone then
                rig2.directcommand(filename)
            else
                rig1.directcommand(filename);

        IF CommandString = 'TOGGLECW'        THEN ToggleCW (False);
        IF CommandString = 'TOGGLEMODES'     THEN ToggleModes;
        IF CommandString = 'TOGGLESTEREOPIN' THEN ToggleStereoPin; {KK1L: 6.71}
        END;

    CommandUseInactiveRadio := FALSE; {KK1L: 6.73 Put back to normal so other calls default to active radio}
    END;



PROCEDURE ResetKeyStatus (Radio: RadioType);

    BEGIN
    IF Radio = RadioOne THEN
        WITH Radio1KeyStatus DO
            BEGIN
            KeyPressedCode := 0;
            KeyPressed             := FALSE;
            ExtendedKey            := FALSE;
            ExtendedKeyNullSent    := FALSE;
            LeftShiftKeyPressed    := FALSE;
            RightShiftKeyPressed   := FALSE;
            LeftAltKeyPressed      := FALSE;
            RightAltKeyPressed     := FALSE;
            LeftControlKeyPressed  := FALSE;
            RightControlKeyPressed := FALSE;
            END;

    IF Radio = RadioTwo THEN
        WITH Radio2KeyStatus DO
            BEGIN
            KeyPressedCode := 0;
            KeyPressed             := FALSE;
            ExtendedKey            := FALSE;
            ExtendedKeyNullSent    := FALSE;
            LeftShiftKeyPressed    := FALSE;
            RightShiftKeyPressed   := FALSE;
            LeftAltKeyPressed      := FALSE;
            RightAltKeyPressed     := FALSE;
            LeftControlKeyPressed  := FALSE;
            RightControlKeyPressed := FALSE;
            END;
    END;



PROCEDURE TBSIQ_ExitProgram;

VAR TempString: Str160;

    BEGIN
    IF (ParamCount > 0) AND (ParamStr (1) = 'EXIT') THEN
        BEGIN
        SetWindow (WholeScreenWindow);
        NormVideo;
        TextMode (OriginalTextMode);
        ClrScr;

        IF BandMapEnable THEN SaveBandMap;

        Sheet.SaveRestartFile;

        IF IntercomFileOpen THEN Close (IntercomFileWrite);

        NormVideo;
        TextMode (OriginalTextMode);
        ClrScr;
        UnInitializeKeyer;
        SetCBreak (ControlBreakStatus);
        Halt;
        END;

    TempString := QuickEditResponse ('Do you really want to exit the program? (Y/N) : ', 1);
    IF UpperCase (TempString) <> 'Y' THEN Exit;

    IF BackCopyEnable THEN StopBackCopy;

    IF NetDebug THEN
        BEGIN
        Close (NetDebugBinaryOutput);
        Close (NetDebugBinaryInput);
        END;

    SetWindow (WholeScreenWindow);
    NormVideo;
    TextMode (OriginalTextMode);
    ClrScr;

    IF DVPEnable THEN DVPUnInit;

    IF BandMapEnable THEN SaveBandMap;

    Sheet.SaveRestartFile;

    IF IntercomFileOpen THEN Close (IntercomFileWrite);

    NormVideo;
    TextMode (OriginalTextMode);
    ClrScr;
    UnInitializeKeyer;
    SetCBreak (ControlBreakStatus);
    Halt;
    END;



PROCEDURE TBSIQ_DisplayBuffer (Buffer: SendBufferType;
                               BufferStart: INTEGER;
                               BufferEnd: INTEGER);


VAR BufferAddress: INTEGER;

    BEGIN
    ClrScr;

    IF BufferStart = BufferEnd THEN
        BEGIN
        Write ('Buffer empty - RETURN to stop');
        Exit;
        END;

    BufferAddress := BufferStart;

    WHILE BufferAddress <> BufferEnd DO
        BEGIN
        Write (Buffer [BufferAddress]);
        Inc (BufferAddress);
        IF BufferAddress = 256 THEN BufferAddress := 0;
        END;
    END;



PROCEDURE TBSIQ_DeleteLastContact;

    BEGIN
    VisibleLog.DeleteLastLogEntry;
    UpdateTotals;
    VisibleLog.ShowRemainingMultipliers;
    VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);
    DisplayTotalScore (TotalScore);

    IF VisibleDupeSheetEnable THEN
        BEGIN
        VisibleDupeSheetChanged := True;
        VisibleLog.DisplayVisibleDupeSheet (ActiveBand, ActiveMode);
        END;
    END;



PROCEDURE TBSIQ_CheckBandMap;

{ Looks at the status of the two radios and determines which one should have the focus
  of the bandmap.  If it changes, the new bandmap will be displayed.  Tried to also make
  this work for the dupesheet which never seemed to be getting redisplayed. }

VAR RadioThatShouldHaveFocus: RadioType;

    BEGIN
    { First - we look at the state of both radios and decide which one we think
      shuld have the band map focus }

    { Defaults }

    RadioThatShouldHaveFocus := TBSIQ_BandMapFocus;

    { If one radio is in S&P and the other isn't - then focus on the S&P radio }

    IF (Radio1QSOMachine.QSOState =  QST_SearchAndPounce) AND
       (Radio2QSOMachine.QSOState <> QST_SearchAndPounce) THEN
           RadioThatShouldHaveFocus := RadioOne;

    IF (Radio2QSOMachine.QSOState =  QST_SearchAndPounce) AND
       (Radio1QSOMachine.QSOState <> QST_SearchAndPounce) THEN
           RadioThatShouldHaveFocus := RadioTwo;

    IF (Radio1QSOMachine.QSOState = QST_SearchAndPounce) AND
       (Radio2QSOMachine.QSOState = QST_SearchAndPounce) THEN
           BEGIN
           IF Radio1QSOMachine.RadioOnTheMove THEN RadioThatShouldHaveFocus := RadioOne;
           IF Radio2QSOMachine.RadioOnTheMove THEN RadioThatShouldHaveFocus := RadioTwo;
           END;

   { Let's add the case where both rigs are in CQ mode, but one is on the move.  This
     is typical in the CW Sprint where you need to look at the visible dupeshet of
     the band that the radio is moving on in preparation of pressing the SPACE BAR
     and pouncing on a station }

   IF (Radio1QSOMachine.QSOState <> QST_SearchAndPounce) AND
      (Radio2QSOMachine.QSOState <> QST_SearchAndPounce) THEN
           BEGIN
           IF Radio1QSOMachine.RadioOnTheMove THEN RadioThatShouldHaveFocus := RadioOne;
           IF Radio2QSOMachine.RadioOnTheMove THEN RadioThatShouldHaveFocus := RadioTwo;
           END;

    IF RadioThatShouldHaveFocus <> TBSIQ_BandMapFocus THEN  { We need to change }
        BEGIN
        TBSIQ_BandMapFocus := RadioThatShouldHaveFocus;

        { Not sure if this is better than just sending the Radio's band/mode }

        BandMapBand := BandMemory [TBSIQ_BandMapFocus];
        BandMapMode := ModeMemory [TBSIQ_BandMapFocus];

        IF VisibleDupeSheetEnable THEN
            BEGIN
            VisibleDupeSheetChanged := True;
            VisibleLog.DisplayVisibleDupeSheet (BandMapBand, BandMapMode);
            END;

        CASE TBSIQ_BandMapFocus OF
            RadioOne: BandMapCursorFrequency := Radio1QSOMachine.Frequency;
            RadioTwo: BandMapCursorFrequency := Radio2QSOMachine.Frequency;
            END;  { of CASE }

        { New and improved? }

        BandMapBLinkingCall := '';
        DisplayBandMap;
        END;
    END;



PROCEDURE TBSIQ_CheckMultiState;

VAR MultiString, MessageString: STRING;
    ModeString, MultString, Call: CallString;
    Band: BandType;
    Mode: ModeType;
    Points: INTEGER;
    Freq, QSX: LONGINT;
    ControlByte: BYTE;
    RXData: ContestExchange;
    Year, Month, Day, DayOfWeek, Hour, Minute, Second, Sec100: WORD;
    NewYear, NewMonth, NewDay, NewHour, NewMinute, NewSecond: WORD;
    Dupe, Mult, FirstCommand, NewMult: BOOLEAN;
    FileWrite: TEXT;
    SplitByte: BYTE;

    BEGIN
    IF NOT K1EANetworkEnable THEN CheckForLostMultiMessages;

    MultiString := GetMultiPortCommand;

    IF MultiString = '' THEN Exit;

    IF K1EANetworkEnable THEN
        BEGIN

        { Message string to not have message type, source or checksum }

        MessageString := MultiString;
        RemoveFirstString (MessageString);  { Delete message type & source }
        Delete (MessageString, Length (MessageString), 1); { Delete checksum }

        CASE MultiString [1] OF

            'B': Packet.ProcessPacketMessageFromNetWork (MessageString);

            'C': { Band map message }
                BEGIN

{ C1 599 Freq QSX UnixTime 0 band mode call * 0 1 0 0 }

                RemoveFirstString (MultiString);  { C1 }
                RemoveFirstString (MultiString);  { 599 }

                Freq := RemoveFirstLongInteger (MultiString);
                QSX  := RemoveFirstLongInteger (MultiString);

                RemoveFirstString (MultiString);  { UnixTime }
                SplitByte := RemoveFirstLongInteger (MultiString);  { 0 or 1 }
                RemoveFirstString (MultiString);  { band }

                SplitByte := SplitByte AND $01;

                IF SplitByte = 0 THEN QSX := 0;

                ModeString := RemoveFirstString (MultiString);  { mode }

                Call := RemoveFirstString (MultiString);

                { Set mode to a safe value in case it can't be calculated }

                IF ModeString = '1' THEN
                    Mode := CW
                ELSE
                    Mode := Phone;

                CalculateBandMode (Freq, Band, Mode);

                IF (Band <> NoBand) AND (Mode <> NoMode) THEN { Added in 6.25 }
                    BEGIN
                    Dupe := VisibleLog.CallIsADupe (Call, Band, ActiveMode);

                    { These didn't have the NOT in them until 6.36 }

                    IF NOT MultByBand THEN Band := All;
                    IF NOT MultByMode THEN Mode := Both;

                    { Runtime 201 here when hitting F1s - probably on 160.
                      Initialized Mode to Active mode before calling
                      CalculateBandMode }

                    VisibleLog.DetermineIfNewMult (Call, Band, Mode, MultString);
                    Mult := MultString <> '';

                    { SendToMulti = False }

                    NewBandMapEntry (Call, Freq, QSX, Mode, Dupe, Mult, BandMapDecayTime, False)
                    END;

                END;

            'G': BEGIN  { Pass frequency information }
                 UpdateK1EAStationInfo (Pass, MultiString [2], MessageString);
                 END;

            'L', 'U':
                BEGIN  { Log QSO }
                MultiString := ConvertK1EANetworkLogMessageToN6TRLogString (MultiString);
                ParseExchangeIntoContestExchange (MultiString, RXData);

               { These next steps are unique for K1EA network entries that
                  have no QSO point information, multiplier information or
                  even the sent QSO Number when they come in. }

                LocateCall (RXData.Callsign, RXData.QTH, True);
                IF DoingDXMults THEN GetDXQTH (RXData);
                CalculateQSOPoints (RXData);
                VisibleLog.ProcessMultipliers (RXData);
                RXData.NumberSent := TotalContacts + 1;

                { Need to convert RXData back to a N6TR Log Entry string }

                MessageString := MakeLogString (RXData);

                { Now you can do everything that is normally done with a
                  TR Log entry }

                Call   := GetLogEntryCall      (MessageString);
                Band   := GetLogEntryBand      (MessageString);
                Mode   := GetLogEntryMode      (MessageString);
                Points := GetLogEntryQSOPoints (MessageString);

                CheckBand (Band);

                NewMult := GetLogEntryMultString (MessageString) <> '';

                IF SendQSOImmediately THEN
                    TBSIQ_PushLogStringIntoEditableLogAndLogPopedQSO (MessageString, False)
                ELSE
                    TBSIQ_PutContactIntoLogFile (MessageString);

                Inc (NumberContactsThisMinute);
                NumberQSOPointsThisMinute := NumberQSOPointsThisMinute + Points;

                IF ActiveWindow <> DupeSheetWindow THEN { no packet }
                    BEGIN
                    DisplayTotalScore (TotalScore);
                    DisplayNamePercentage (TotalNamesSent + VisibleLog.NumberNamesSentInEditableLog, TotalContacts);
                    UpdateTotals;
                    END;

                DisplayTotalScore (TotalScore);
                DisplayInsertMode (InsertMode);

                DisplayNextQSONumber (QSONumberForThisQSO);

                IF FloppyFileSaveFrequency > 0 THEN
                    IF QSOTotals [All, Both] > 0 THEN
                        IF QSOTotals [All, Both] MOD FloppyFileSaveFrequency = 0 THEN
                            SaveLogFileToFloppy;

                IF UpdateRestartFileEnable THEN Sheet.SaveRestartFile;

                IF MultiUpdateMultDisplay AND NewMult THEN
                    VisibleLog.ShowRemainingMultipliers;

                IF BandMapEnable THEN {KK1L: 6.69 should get BM matching new data}
                    BEGIN
                    UpdateBandMapMultiplierStatus;
                    UpdateBandMapDupeStatus(RXData.Callsign, RXData.Band, RXData.Mode, True);
                    END;

                END;

            'M': BEGIN  { Run frequency information }
                 UpdateK1EAStationInfo (Run, MultiString [2], MessageString);
                 END;

            'T', 'P':   { Talk or pass message }
                 BEGIN
                 QuickDisplay (MessageString);
                 ReminderPostedCount := 60;

                 PushMultiMessageBuffer (MessageString);

                 IF IntercomFileOpen THEN
                     WriteLn (IntercomFileWrite, GetTimeString, ' ', MessageString);

                 END;

            'Y': BEGIN  { DOS time sync message }
                 NewHour   := RemoveFirstLongInteger (MessageString);
                 NewMinute := RemoveFirstLongInteger (MessageString);
                 NewSecond := RemoveFirstLongInteger (MessageString);
                 NewDay    := RemoveFirstLongInteger (MessageString);
                 NewMonth  := RemoveFirstLongInteger (MessageString);
                 NewYear   := RemoveFirstLongInteger (MessageString);

                 GetDate (Year, Month, Day, DayOfWeek);

                 IF (Year <> NewYear) OR (Month <> NewMonth) OR (Day <> NewDay) THEN
                     SetDate (NewYear, NewMonth, NewDay);

                 GetTime (Hour, Minute, Second, Sec100);

                 IF (Hour <> NewHour) OR (Minute <> NewMinute) OR (Abs (Second - NewSecond) > 3) THEN
                     SetTime (NewHour, NewMinute, NewSecond, 0);
                 END;


            END;  { of CASE MultiString [1] }

        END

    ELSE   { N6TR Network Mode }
        BEGIN
        MessageString [0] := MultiString [8];
        Move (MultiString [10], MessageString [1], Ord (MultiString [8]));

        ControlByte := Ord (MultiString [3]);

        CASE ControlByte OF
            MultiInformationMessage:
                BEGIN
                Band := RemoveBand (MessageString);
                Mode := RemoveMode (MessageString);
                if ((Mode < Low(ModeType)) or (Mode > High(ModeType))) then
                   Mode := NoMode;

                IF MultiStatus [Band, Mode] = nil THEN New (MultiStatus [Band, Mode]);
                MultiStatus [Band, Mode]^ := MessageString;
                END;

            MultiTimeMessage:
                BEGIN
                Year   := RemoveFirstLongInteger (MessageString);
                Month  := RemoveFirstLongInteger (MessageString);
                Day    := RemoveFirstLongInteger (MessageString);
                Hour   := RemoveFirstLongInteger (MessageString);
                Minute := RemoveFirstLongInteger (MessageString);
                Second := RemoveFirstLongInteger (MessageString);

                SetTime (Hour, Minute, Second, 0);
                SetDate (Year, Month, Day);
                END;

            MultiBandMapMessage:
                BEGIN
                Call := RemoveFirstString (MessageString);
                Freq := RemoveFirstLongInteger (MessageString);
                QSX  := RemoveFirstLongInteger (MessageString);

                Mode := ActiveMode;

                CalculateBandMode (Freq, Band, Mode);

                IF (Band <> NoBand) AND (Mode <> NoMode) THEN { Added in 6.25 }
                    BEGIN
                    Dupe := VisibleLog.CallIsADupe (Call, Band, ActiveMode);

                    { These didn't have the NOT in them until 6.36 }

                    IF NOT MultByBand THEN Band := All;
                    IF NOT MultByMode THEN Mode := Both;

                    { Runtime 201 here when hitting F1s - probably on 160.
                      Initialized Mode to Active mode before calling
                      CalculateBandMode }

                    VisibleLog.DetermineIfNewMult (Call, Band, Mode, MultString);
                    Mult := MultString <> '';
                                                                    { SendToMulti = False }
                    NewBandMapEntry (Call, Freq, QSX, Mode, Dupe, Mult, BandMapDecayTime, False)
                    END;
                END;

            MultiTalkMessage:
                BEGIN
                MessageString := BandString [MultiMessageSourceBand (Ord (MultiString [1]))] + ': ' + MessageString;
                QuickDisplay (MessageString);
                ReminderPostedCount := 60;

                PushMultiMessageBuffer (MessageString);

                IF IntercomFileOpen THEN
                    WriteLn (IntercomFileWrite, GetTimeString, ' ', MessageString);
                END;

            MultiPacketReceivedMessage:
                Packet.ProcessPacketMessageFromNetWork (MessageString);

            MultiPacketMessageToSend:
                IF ActivePacketPort <> nil THEN SendPacketMessage (MessageString);

            MultiQSOData:
                BEGIN
                Call   := GetLogEntryCall      (MessageString);
                Band   := GetLogEntryBand      (MessageString);
                Mode   := GetLogEntryMode      (MessageString);
                Points := GetLogEntryQSOPoints (MessageString);

                CheckBand (Band);

                NewMult := GetLogEntryMultString (MessageString) <> '';

                IF SendQSOImmediately THEN
                    TBSIQ_PushLogStringIntoEditableLogAndLogPopedQSO (MessageString, False)
                ELSE
                    TBSIQ_PutContactIntoLogFile (MessageString);

                Inc (NumberContactsThisMinute);
                NumberQSOPointsThisMinute := NumberQSOPointsThisMinute + Points;

                IF ActiveWindow <> DupeSheetWindow THEN { no packet }
                    BEGIN
                    DisplayTotalScore (TotalScore);
                    DisplayNamePercentage (TotalNamesSent + VisibleLog.NumberNamesSentInEditableLog, TotalContacts);
                    UpdateTotals;
                    END;

                DisplayTotalScore (TotalScore);
                DisplayInsertMode (InsertMode);
                DisplayNextQSONumber (QSONumberForThisQSO);

                IF FloppyFileSaveFrequency > 0 THEN
                    IF QSOTotals [All, Both] > 0 THEN
                        IF QSOTotals [All, Both] MOD FloppyFileSaveFrequency = 0 THEN
                            SaveLogFileToFloppy;

                IF UpdateRestartFileEnable THEN Sheet.SaveRestartFile;

                IF MultiUpdateMultDisplay AND NewMult THEN
                    VisibleLog.ShowRemainingMultipliers;

                IF BandMapEnable THEN {KK1L: 6.69 should get BM matching new data}
                    BEGIN
                    UpdateBandMapMultiplierStatus;
                    UpdateBandMapDupeStatus(RXData.Callsign, RXData.Band, RXData.Mode, True);
                    END;

                END;

            MultiConfigurationMessage:
                BEGIN
                FirstCommand := False;
                ProcessConfigInstruction (MessageString, FirstCommand);

                IF OpenFileForAppend (FileWrite, LogConfigFileName) THEN
                    BEGIN
                    WriteLn (FileWrite, MessageString);
                    Close (FileWrite);
                    END;
                END;

            END;   { of case }
        END;
    END;




PROCEDURE TBSIQ_UpdateTimeAndRateDisplays;

{ This is a global routine that gets called from the highest level of the program.
  It is generic and not radio specific.  If you want to execute something that is
  specific to a radio - please see the UpdateRadioDisplay procedure in the
  QSOMachineObject. }

VAR TimeString, FullTimeString, HourString: Str20;
    RateMinute: INTEGER;

    BEGIN
    IF FootSwitchMode <> PreviousFootSwitchMode THEN
        BEGIN
        IF FootSwitchMode = Normal THEN
            ActiveKeyer.LetFootSwitchControlPTT  { This does nothing for YCCC }
        ELSE
            IF ActiveKeyer = ArdKeyer THEN
                ArdKeyer.ClearFootSwitchControlPTT;

        PreviousFootSwitchMode := FootSwitchMode;
        END;

    TBSIQ_CheckDualingCQState;  { The orchestral director of the dualing CQ process }
    TBSIQ_CheckTestLoopState;   { Test loop process }
    TBSIQ_CheckBandMap;         { Sees if the bandmap and visible dupesheet are on the right radio }

    Packet.CheckPacket;         { See if any spots have come in for the bandmap }

    IF ActiveMultiPort <> nil THEN TBSIQ_CheckMultiState;

    { Send some oxygen to the N4OGW bandmap if it is there }

    IF N4OGW_RadioOne_BandMap_IP <> '' THEN N4OGW_RadioOne_BandMap.Heartbeat;
    IF N4OGW_RadioTwo_BandMap_IP <> '' THEN N4OGW_RadioTwo_BandMap.Heartbeat;

    { And also the N1MM QSO portal }

    IF N1MM_UDP_Port > 0 THEN N1MM_QSO_Portal.Heartbeat;

    { See if the clock has ticked a second }

    FullTimeString := GetFullTimeString;

    IF FullTimeString = LastFullTimeString THEN Exit;  { Nothing to do }
    LastFullTimeString := FullTimeString;

    { Code following this will be exectued once for each new calendar second }
    { The time gets displayed in the upper left corner of the Total Window }

    IF TestLoopDelayCount > 0 THEN
        Dec (TestLoopDelayCount);

    SaveAndSetActiveWindow (TotalWindow);
    Write (' ' + FullTimeString);

    { If we are counting down a ten minute rule - it goes under the time display }

    IF TenMinuteRule <> NoTenMinuteRule THEN
        BEGIN
        GoToXY (1, 2);
        Write ('  ', ElaspedTimeString (TenMinuteTime.Time));
        END;

    RestorePreviousWindow;

    { Take care of things we only want to do every second }

    IF ReminderPostedCount > 0 THEN Dec (ReminderPostedCount);

    { Now we see if we have a new minute - GetTimeString does not have seconds }

    TimeString := GetTimeString;

    IF TimeString = LastDisplayedTime THEN Exit;  { Nothing more to do }
    LastDisplayedTime := TimeString;

    { Code after here is only executed once a minute }

    Inc (MinutesSinceLastBMUpdate);

    IF BandMapEnable AND (MinutesSinceLastBMUpdate >= BandMapDecayMultiplier) THEN
        BEGIN
        MinutesSinceLastBMUpdate := 0;
        DecrementBandMapTimes;
        DisplayBandMap;
        END;

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

    { See if we have moved into a new calendar hour }

    HourString := PrecedingString (TimeString, ':');

    IF HourString <> LastDisplayedHour THEN
        BEGIN
        TotalThisHour := 0;
        BandChangesThisHour := 0;
        LastDisplayedHour := HourString;
        END;

    { Display the rate }

    SaveSetAndClearActiveWindow (TBSIQ_RateWindow);

    IF Rate < 1000 THEN
        Write ('Rate = ', Rate)
    ELSE
        Write ('Rate= ', Rate);

    RestorePreviousWindow;

    SaveSetAndClearActiveWindow (TBSIQ_HourRateWindow);

    CASE HourDisplay OF
        ThisHour:      Write ('This hr = ', TotalThisHour);
        LastSixtyMins: Write ('Last 60 = ', TotalLastSixty);
        BandChanges:   Write ('Bnd Chg = ', BandChangesThisHour);
        END;

    RestorePreviousWindow;
    END;



PROCEDURE QSOMachineObject.ClearAutoStartSendDisplay;

    BEGIN
    CASE Radio OF
        RadioOne: SaveAndSetActiveWindow (TBSIQ_R1_StartSendingWindow);
        RadioTwo: SaveAndSetActiveWindow (TBSIQ_R2_StartSendingWindow);
        END;  { of case }

    ClrScr;
    RestorePreviousWindow;
    END;



PROCEDURE QSOMachineObject.DisplaySCPCall (Call: CallString);

    BEGIN
    { See if we are out of room }

    IF WhereX + Length (Call) >= 78 THEN
        BEGIN
        IF WhereY = 5 THEN
            BEGIN
            SCPScreenFull := True;
            Exit;
            END;

        WriteLn;
        END;

    IF WhereX > 1 THEN Write (' ');

    IF VisibleLog.CallIsADupe (Call, Band, Mode) THEN
        BEGIN
        TextBackground (SCPDupeBackground);
        TextColor (SCPDupeColor);
        Write (Call);
        TextBackground (SelectedColors.EditableLogWindowBackground);
        TextColor (SelectedColors.EditableLogWindowColor);
        END
    ELSE
        Write (Call);

    END;



PROCEDURE QSOMachineObject.SwapWindows;

    BEGIN
    IF TBSIQ_ActiveWindow = TBSIQ_CallWindow THEN
        SetTBSIQWindow (TBSIQ_ExchangeWindow)
    ELSE
        SetTBSIQWindow (TBSIQ_CallWindow);
    END;



PROCEDURE QSOMachineObject.SuperCheckPartial;

{ Super check partial routine leveraged from LOGEDIT - however, I assume
  that I am only here if the CallWindowString is >= SCPMinimumLetters }

VAR Call: CallString;

    BEGIN
    IF CD.SCPDisabledByApplication THEN Exit;

    IF CallWindowString = LastSCPCall THEN Exit;
    LastSCPCall := CallWindowString;

    SCPScreenFull := False;

    IF NOT CD.PartialCallSetup (CallWindowString) THEN Exit;

    EditableLogDisplayed := False;
    GridSquareListShown := False;

    SaveSetAndClearActiveWindow (EditableLogWindow);

    CD.NumberSCPCalls := 0;

    REPEAT
        Call := CD.GetNextPartialCall;

        IF Call <> '' THEN
            BEGIN
            DisplaySCPCall (Call);
            Inc (CD.NumberSCPCalls);
            END;

        IF SCPScreenFull THEN Break;

        IF TBSIQ_KeyPressed (Radio) THEN
            BEGIN
            LastSCPCall := '';
            Break;
            END;

    UNTIL (Call = '') OR TBSIQ_KeyPressed (Radio);;

    RestorePreviousWindow;
    END;



PROCEDURE QSOMachineObject.SendKeyboardInput;

{ This procedure will take input from the keyboard and send it until a
  return is pressed.

  This routine needs to be aware that the other radio might be sending
  a CW message and wait until it is over before sending any characters.

  It will prevent the other transmitter from doing anything until the
  message is complate.

  The complete message will be shown in the CWMessageWindow.  Perhaps
  someday, we will change the color on text that has been sent.  This
  probably could be integrated into the main loop of CheckQSOStateMachine,
  but it lives here for now. }

VAR Key: CHAR;
    TimeMark: TimeRecord;
    Buffer: SendBufferType;
    BufferStart, BufferEnd: INTEGER;

    BEGIN
    BufferStart := 0;
    BufferEnd := 0;
    Buffer[0] := ' '; //to kill buffer not initialized warning

    IF NOT CWEnable THEN Exit;

    ActiveRadio := Radio;
    ActiveMode := Mode;

    SendingOnRadioOne := False;
    SendingOnRadioTwo := False;
    SetUpToSendOnActiveRadio;

    CASE Radio OF
        RadioOne: SaveAndSetActiveWindow (TBSIQ_R1_CWMessageWindow);
        RadioTwo: SaveAndSetActiveWindow (TBSIQ_R2_CWMessageWindow);
        END;

    ClrScr;
    Write ('Keyboard CW - ENTER to exit.');

    REPEAT
        MarkTime (TimeMark);

        REPEAT
            IF ActiveKeyer.BufferEmpty THEN
                IF BufferStart <> BufferEnd THEN
                    BEGIN
                    ActiveKeyer.AddCharacterToBuffer (Buffer [BufferStart]);
                    Inc (BufferStart);
                    IF BufferStart = 256 THEN BufferStart := 0;
                    TBSIQ_DisplayBuffer (Buffer, BufferStart, BufferEnd);
                    END;
            millisleep;
        UNTIL TBSIQ_KeyPressed (Radio);

        Key := UpCase (TBSIQ_ReadKey (Radio));

        IF Key >= ' ' THEN
            BEGIN
            IF BufferStart = BufferEnd THEN ClrScr;
            Buffer [BufferEnd] := Key;
            Inc (BufferEnd);
            IF BufferEnd = 256 THEN BufferEnd := 0;
            Write (Key);
            END
        ELSE
            CASE Key OF
                CarriageReturn:
                    BEGIN
                    WHILE BufferStart <> BufferEnd DO
                        BEGIN
                        ActiveKeyer.AddCharacterToBuffer (Buffer [BufferStart]);
                        Inc (BufferStart);
                        IF BufferStart = 256 THEN BufferStart := 0;
                        END;

                    ActiveKeyer.PTTUnForce;
                    RemoveAndRestorePreviousWindow;
                    ClearKeyCache := True;
                    Exit;
                    END;

                BackSpace:
                    IF BufferEnd <> BufferStart THEN
                        BEGIN
                        Dec (BufferEnd);
                        IF BufferEnd < 0 THEN BufferEnd := 255;
                        TBSIQ_DisplayBuffer (Buffer, BufferStart, BufferEnd);
                        END;

                EscapeKey:
                    BEGIN
                    FlushCWBufferAndClearPTT;
                    RemoveAndRestorePreviousWindow;
                    ClearKeyCache := True;
                    Exit;
                    END;

                NullKey:
                    CASE NewReadKey OF
                        F10: BEGIN
                             FlushCWBufferAndClearPTT;
                             RemoveAndRestorePreviousWindow;
                             ClearKeyCache := True;
                             Exit;
                             END;

                        DeleteKey:
                            IF BufferEnd <> BufferStart THEN
                                BEGIN
                                Dec (BufferEnd);
                                IF BufferEnd < 0 THEN BufferEnd := 255;
                                TBSIQ_DisplayBuffer (Buffer, BufferStart, BufferEnd);
                                END;

                        END;
                END;

    UNTIL False;
    END;



FUNCTION QSOMachineObject.ExpandCrypticString (SendString: STRING): STRING;

{ This is a scaled down version of what is in classic mode }

VAR CharacterCount: INTEGER;
    TempString, QSONumberString, NewSendString: STRING;
    SendChar: CHAR;
    TempFreq: LONGINT;

    BEGIN
    FoundCommand (Radio, SendString);

    IF SendString = '' THEN
        BEGIN
        ExpandCrypticString := '';
        Exit;
        END;

    NewSendString := '';

    FOR CharacterCount := 1 TO Length (SendString) DO
        BEGIN
        SendChar := SendString [CharacterCount];

        CASE SendChar OF

            { # can't get a new serial number - it has to use the QSONumberForThisQSO }

            '#': BEGIN
                 IF AutoQSONumberDecrement AND (CallWindowString = '') AND (ExchangeWindowString = '')  THEN
                     Str (QSONumberForPreviousQSO, QSONumberString)
                 ELSE
                     Str (QSONumberForThisQSO, QSONumberString);

                 NewSendString := NewSendString + QSONumberString;
                 END;

            '_': NewSendString := NewSendString + ' ';  { Leading space }

            ControlD: IF TBSIQ_CW_Engine.CWBeingSent (Radio) THEN NewSendString := NewSendString + ' ';

            '@': BEGIN
                 { The old routine actually did the callsign update here - but I am not
                   going to support that out of the gate. }

                NewSendString := NewSendString + CallWindowString;
                CallsignICameBackTo := CallWindowString;
                END;

            ':': BEGIN   { Forget everything and setup to send CW from keyboard }
                 ExpandCrypticString := '';  { Make sure we don't try to send something }
                 RememberQSOState := QSOState;
                 QSOState := QST_StartSendingKeyboardCW;
                 Exit;
                 END;

            '\': NewSendString := NewSendString + MyCall;

            '|': NewSendString := NewSendString + ReceivedData.Name;

            '{': NewSendString := NewSendString + ReceivedData.Callsign;

            '>': ClearRIT;

            ControlQ:   { Send frequency of other radio }
                BEGIN
                IF Radio = RadioOne THEN
                    TempFreq := Radio2QSOMachine.DisplayedFrequency
                ELSE
                    TempFreq := Radio1QSOMachine.DisplayedFrequency;

                TempFreq := Round (TempFreq / 1000);  { Compute kHz }
                Str (TempFreq, TempString);
                NewSendString := NewSendString + TempString;

                TBSIQ_PushLogStringIntoEditableLogAndLogPopedQSO (GetDateString + ' ' + GetTimeString + ': ' + 'Asked ' + CallWindowString + ' to QSY to ' + TempString, True);
                END;

            { Not a special character - just add it as is }

            ELSE NewSendString := NewSendString + SendChar;
            END;
        END;

    ExpandCrypticString := NewSendString;
    END;



PROCEDURE QSOMachineObject.DisplayAutoStartSendCharacterCount;

    BEGIN
    CASE Radio OF
        RadioOne: SaveAndSetActiveWindow (TBSIQ_R1_StartSendingWindow);
        RadioTwo: SaveAndSetActiveWindow (TBSIQ_R2_StartSendingWindow);
        END;  { of case }

    ClrScr;

    IF (AutoStartSendCharacterCount > 0) AND AutoStartSendEnable THEN
        BEGIN
        GoToXY (AutoStartSendCharacterCount + 1, 1);
        Write ('|');
        END;

    RestorePreviousWindow;
    END;



PROCEDURE QSOMachineObject.DisplayQSONumber;

{ Uses QSONumberForThisQSO }

    BEGIN
    IF QSONumberForThisQSO = LastDisplayedQSONumber THEN Exit;

    LastDisplayedQSONumber := QSONumberForThisQSO;

    CASE Radio OF
        RadioOne: SaveAndSetActiveWindow (TBSIQ_R1_QSONumberWindow);
        RadioTwo: SaveAndSetActiveWindow (TBSIQ_R2_QSONumberWindow);
        END;  { of case }

    ClrScr;

    IF QSONumberForThisQSO > 9999 THEN
        Write (QSONumberForThisQSO:5)
    ELSE
        Write (QSONumberForThisQSO:4);

    { Create a clear space after the number }

    CASE Radio OF
        RadioOne: Window (TBSIQ_R1_QSONumberWindowRX, TBSIQ_R1_QSONumberWindowRY, TBSIQ_R1_QSONumberWindowRX, TBSIQ_R1_QSONumberWindowRY);
        RadioTwo: Window (TBSIQ_R2_QSONumberWindowRX, TBSIQ_R2_QSONumberWindowRY, TBSIQ_R2_QSONumberWindowRX, TBSIQ_R2_QSONumberWindowRY);
        END;

    TextBackground (SelectedColors.WholeScreenBackground);
    ClrScr;
    RestorePreviousWindow;
    END;



PROCEDURE QSOMachineObject.RemoveQSONumberWindow;

    BEGIN
    CASE Radio OF
        RadioOne: RemoveWindow (TBSIQ_R1_QSONumberWindow);
        RadioTwo: RemoveWindow (TBSIQ_R2_QSONumberWindow);
        END;  { of case }
    END;



PROCEDURE QSOMachineObject.ShowTransmitStatus;

{ Turns on the TX indicator and the colored squares depending on status }

    BEGIN
    TBSIQ_CW_Engine.ShowActiveRadio;
    END;



PROCEDURE QSOMachineObject.ListenToBothRadios;

{ Puts the headphones into stereo mode }

    BEGIN
    IF EnableHeadphoneSwitching THEN
        so2rbox.setrcvfocus (Stereo);
    END;



PROCEDURE QSOMachineObject.ListenToOtherRadio;

{ We need to check to make sure the other radio is not busy sending a message
  before actually doing this.  If so, just do nothing.  This routine will get
  called again }

    BEGIN
    IF NOT EnableHeadphoneSwitching THEN Exit;

    IF Radio = RadioOne THEN
        BEGIN
        IF TBSIQ_CW_Engine.CWBeingSent (RadioTwo) THEN Exit;
        so2rbox.setrcvfocus (RX2);
        END;

    IF Radio = RadioTwo THEN
        BEGIN
        IF TBSIQ_CW_Engine.CWBeingSent (RadioOne) THEN Exit;
        so2rbox.setrcvfocus (RX1);
        END;
    END;


FUNCTION QSOMachineObject.SMeterReading: INTEGER;

VAR Response: STRING;
    CaughtUp: BOOLEAN;
    TempInt: INTEGER;

    BEGIN
    { Stop polling and let whatever messages are being received finish }

    CASE Radio OF
        RadioOne:
            WITH Rig1 DO
                BEGIN
                REPEAT UNTIL CalmDownEveryone;
                REPEAT
                    Response := DirectCommandAndResponse ('SM;');
                    Timer (CaughtUp);
                UNTIL Response <> '';
                END;

        RadioTwo:
            WITH Rig2 DO
                BEGIN
                REPEAT UNTIL CalmDownEveryone;
                REPEAT
                    Response := DirectCommandAndResponse ('SM;');
                    Timer (caughtup);
                UNTIL Response <> '';
                END;
        END; { of CASE Radio }

    { Response looks like SM0003; }

    Delete (Response, 1, 2);
    Delete (Response, Length (Response), 1);

    Val (Response, TempInt);
    SMeterReading := TempInt;
    END;



PROCEDURE QSOMachineObject.ReadSMeter;

VAR Response: STRING;
    CaughtUp: BOOLEAN;

    BEGIN
    { Stop polling and let whatever messages are being received finish }

    CASE Radio OF
        RadioOne:
            WITH Rig1 DO
                BEGIN
                REPEAT UNTIL CalmDownEveryone;
                REPEAT
                    Response := DirectCommandAndResponse ('SM;');
                    Timer (caughtup);
                UNTIL Response <> '';
                END;

        RadioTwo:
            WITH Rig2 DO
                BEGIN
                REPEAT UNTIL CalmDownEveryone;
                REPEAT
                    Response := DirectCommandAndResponse ('SM;');
                    Timer (caughtup);
                UNTIL Response <> '';
                END;
        END; { of CASE Radio }

    ShowCWMessage (Response);  { Polling is automatically restarted }
    END;



PROCEDURE QSOMachineObject.PTTTest;

    BEGIN
    ShowCWMessage ('Doing PTT Test');
    DoingPTTTest := True;
    PTTState := True;
    PTTTestTimer := 3;
    ActiveKeyer.SetActiveRadio (Radio);
    ActiveKeyer.PTTForceOn;
    END;



PROCEDURE QSOMachineObject.CheckQSOStateMachine;

VAR Key, ExtendedKey: CHAR;
    ExpandedString, TempString, InitialExchange, Message, WindowString: STRING;
    QSONumberString: STr20;
    ThisIsADupe, ActionRequired: BOOLEAN;
    Freq: LONGINT;
    RealFreq: REAL;
    xResult: INTEGER;
    MultString: Str20;
    Mult: BOOLEAN;

    BEGIN
    UpdateRadioDisplay;  { Update radio band/mode/frequency }

    IF PartialCallEnable AND (LastPartialCall <> CallWindowString) THEN
        BEGIN
        VisibleLog.GeneratePartialCalls (CallWindowString, Band, Mode, TBSIQPossibleCallList);
        DisplayPossiblecalls (TBSIQPossibleCallList);
        LastPartialCall := CallWindowString;
        END;

    { Tend to the N4OGW band map as needed }

    CASE Radio OF
        RadioOne:
            IF N4OGW_RadioOne_BandMap_IP <> '' THEN
                BEGIN
                { Check to see if TX mode should be disabled }

                IF (N4OGW_RadioOne_BandMap.TXMode) THEN CWStillBeingSent;

                { See if we have a command }

                WHILE N4OGW_RadioOne_BandMap.QTC_Count > 0 DO
                    ProcessN4OGWCommand (N4OGW_RadioOne_BandMap.GetNextQTC);
                END;

        RadioTwo:
            IF N4OGW_RadioTwo_BandMap_IP <> '' THEN
                BEGIN
                { Check to see if TX mode should be disabled }

                IF (N4OGW_RadioTwo_BandMap.TXMode) THEN CWStillBeingSent;

                { See if we have a command }

                WHILE N4OGW_RadioTwo_BandMap.QTC_Count > 0 DO
                    ProcessN4OGWCommand (N4OGW_RadioTwo_BandMap.GetNextQTC);
                END;

        END;  { of CASE }

    { Show new QSO state if diffrent and update TX indicator }

    IF QSOState <> LastQSOState THEN
        BEGIN
        ShowStateMachineStatus;
        LastQSOState := QSOState;
        ShowTransmitStatus;                { Update TX and Cue indicators }

        ActiveBand := Band;

        IF (QSOState <> QST_SearchAndPounce) AND (QSOState <> QST_SearchAndPounceInit) THEN
            DisplayAutoStartSendCharacterCount;
        END;

    { Do not process any keystrokes while auto start send active on the
      other radio.  This is probably mostly okay as I doubt someone will
      be actively putting in characters on one keyboard and pressing a
      key on the other keyboard instantly.  One possible exception might
      be the footswitch if we get that implemented. }

    CASE Radio OF
        RadioOne: IF Radio2QSOMachine.QSOState = QST_AutoStartSending THEN Exit;
        RadioTwo: IF Radio1QSOMachine.QSOState = QST_AutoStartSending THEN Exit;
        END;

    { Unlike the WindowEditor in LOGSUBS2, WindowEditor here will not block execution.
      It will return right away with ActionRequired = FALSE if no key was pressed.
      In the case of QSOState = SendingKeyboardCW, all keystrokes will be returned
      instantly. }

    { We want to get to some states before the WindowEditor is called }

    { We are going to have different behaviour here depending on whether we are
      in TBSIQDualMode mode or not.  }

    IF TBSIQDualMode AND DualModeMemory THEN
        BEGIN
        IF DisableTransmitting THEN Exit;

        { We are done transmitting and can now process the keystroke that was
          waiting }

        Key := DualModeKey;
        ExtendedKey := DualModeExtendedKey;
        ActionRequired := True;
        DualModeMemory := False;

        { Now - we can process the remembered key stroke }
        END

    ELSE { Normal mode }
        IF (QSOState <> QST_StartSendingKeyboardCW) THEN
            WindowEditor (WindowString, Key, ExtendedKey, ActionRequired);

    { This is a VERY POWERFUL command...  not totally sure of the consequences just
      yet - but basically I am trying to lock out anything being done if both rigs
      are not on CW - and there is ActionRequired while the other transmitter is
      busy sending.

      Note - whatever the key was that was pressed is forgotten - there is no
      key memory here. }

    IF ActionRequired AND DisableTransmitting THEN
        BEGIN
        IF TBSIQDualMode THEN
            BEGIN
            DualModeMemory := True;
            DualModeKey := Key;
            DualModeExtendedKey := ExtendedKey;
            END;

        Exit;
        END;

    CASE QSOState OF

        QST_AltDInput:  { Send characters to the other radio's call window }
                        { This got kind of cofusing and I think I am no longer doing this }
            BEGIN
            IF Key = EscapeKey THEN
                QSOState := RememberQSOState;

            IF ActionRequired THEN
                CASE Radio OF
                    RadioOne: Radio2QSOMachine.CharacterInput := Key;
                    RadioTwo: Radio1QSOMachine.CharacterInput := Key;
                    END;

            IF Key = CarriageReturn THEN
                QSOState := RememberQSOState;
            END;

        QST_Idle, QST_CQCalled:
            BEGIN
            { Clear the auto start send station called flag if the CallWindow is mostly empty }

            IF Length (CallWindowString)  <= 2 THEN
                AutoStartSendStationCalled := False;

            { See if we have a keystroke to look at }

            IF ActionRequired THEN
                BEGIN

                { Not a function key message }

                CASE Key OF
                    NullKey:     { Extended key }
                        BEGIN
                        IF ValidFunctionKey (ExtendedKey) THEN  { Send function key message }
                            BEGIN
                            SendFunctionKeyMessage (ExtendedKey, Message);

                            { Maybe add F9 here? }

                            IF (ExtendedKey = F1) OR (ExtendedKey = F2) THEN
                                BEGIN
                                QSOState := QST_CallingCQ;
                                Str (QSONumberForThisQSO, QSONumberString);
                                NewBandMapEntry ('CQ/' + QSONumberString, Frequency, 0, Mode, False, False, BandMapDecayTime, True);
                                LastCQFrequency := Frequency;
                                LastCQMode := Mode;
                                END;
                            Exit;
                            END;

                        IF ExtendedKey = CarriageReturn THEN  { don't send any CW }
                            BEGIN
                            CallsignICameBackTo := WindowString;
                            QSOState := QST_CQStationBeingAnsweredSilent;
                            Exit;
                            END;
                        END;  { of NullKey case }

                    CarriageReturn:
                        IF TBSIQ_ActiveWindow = TBSIQ_CallWindow THEN
                            BEGIN
                            IF WindowString = 'UPDATEQSONR' THEN
                                BEGIN
                                QSONumberForThisQSO := GetNextQSONumber;   { from LOGEDIT }
                                QSONumberUnused := True;
                                DisplayQSONumber;
                                CallwindowString := '';
                                CallWindowCursorPosition := 1;
                                ClrScr;
                                Exit;
                                END;

                            IF WindowString = 'PTTTEST' THEN
                                PTTTest;

                            IF WindowString = 'SMETER' THEN
                                BEGIN
                                ReadSMeter;
                                CallwindowString := '';
                                CallWindowCursorPosition := 1;
                                ClrScr;
                                Exit;
                                END;

                            IF WindowString = 'TESTLOOP' THEN
                                BEGIN
                                TestLoopState := TestLoopStart;
                                CallwindowString := '';
                                CallWindowCursorPosition := 1;
                                ClrScr;
                                Exit;
                                END;

                            IF WindowString = '' THEN
                                BEGIN
                                SendFunctionKeyMessage (F1, Message);
                                QSOState := QST_CallingCQ;
                                Str (QSONumberForThisQSO, QSONumberString);
                                NewBandMapEntry ('CQ/' + QSONumberString, Frequency, 0, Mode, False, False, BandMapDecayTime, True);
                                LastCQFrequency := Frequency;
                                LastCQMode := Mode;
                                END
                            ELSE
                                BEGIN  { We have a callsign to send }
                                QSONumberUnused := False;

                                IF StringIsAllNumbersOrDecimal (WindowString) THEN
                                    BEGIN
                                    IF Length (WindowString) = 3 THEN
                                        BEGIN
                                        CASE BandMemory[Radio] OF
                                            Band160: TempString := '1'   + WindowString;
                                            Band80:  TempString := '3'   + WindowString;
                                            Band40:  TempString := '7'   + WindowString;
                                            Band30:  TempString := '10'  + WindowString;
                                            Band20:  TempString := '14'  + WindowString;
                                            Band17:  TempString := '18'  + WindowString;
                                            Band15:  TempString := '21'  + WindowString;
                                            Band12:  TempString := '24'  + WindowString;
                                            Band10:  TempString := '28'  + WindowString;
                                            Band6:   TempString := '50'  + WindowString;
                                            ELSE     TempString := '144' + WindowString;
                                            END;
                                        END
                                    ELSE
                                        TempString := WindowString;

                                    IF StringHas (TempString, '.') THEN
                                        BEGIN
                                        Val (TempString, RealFreq, xResult);
                                        Freq := Round (RealFreq * 1000.0);
                                        END
                                    ELSE
                                        Val (TempString + '000', Freq, xResult);

                                    IF xResult = 0 THEN
                                        SetRadioFreq (Radio, Freq, Mode, 'A');

                                    CallWindowString := '';
                                    CallWindowCursorPosition := 1;
                                    ClrScr;
                                    END
                                ELSE
                                    BEGIN
                                    IF AutoPartialCallFetch AND ((Length (CallWindowString) = 3) OR (Length (CallWindowString) = 2)) THEN
                                        BEGIN
                                        IF TBSIQPossibleCallList.NumberCalls = 1 THEN
                                            BEGIN
                                            ClrScr;
                                            Write (TBSIQPossibleCallList.CallList [0].Call);
                                            WindowString := TBSIQPossibleCallList.CallList [0].Call;;
                                            CallWindowString := WindowString;
                                            END;
                                        END;

                                    CASE Mode OF
                                        CW:
                                            BEGIN
                                            TBSIQ_CW_Engine.CueCWMessage (WindowString, Radio, CWP_High, True);
                                            CallsignICameBackTo := WindowString;
                                            ShowCWMessage (WindowString);
                                            QSOState := QST_CQStationBeingAnswered;
                                            END;

                                        Digital:
                                            IF Mode = Digital THEN
                                                BEGIN
                                                ActiveRadio := Radio;
                                                ActiveMode := Mode;
                                                CallsignICameBackTo := WindowString;
                                                QSOState := QST_CQStationBeingAnswered;
                                                END;

                                        Phone:
                                            QSOState := QST_CQStationBeingAnswered;

                                        END;  { of CASE Mode }
                                    END;
                                END;
                            END;

                    EscapeKey:  { We got here with an empty window }
                        BEGIN
                        ClrScr;  { Just to make sure }
                        DisplayEditableLog (VisibleLog.LogEntries);
                        QSOState := QST_Idle;
                        ShowTransmitStatus;

                        CASE Radio OF
                            RadioOne: RemoveWindow (TBSIQ_R1_NameWindow);
                            RadioTwo: RemoveWindow (TBSIQ_R2_NameWindow);
                            END;
                        END;

                    TabKey:
                        QSOState := QST_SearchAndPounceInit;

                    SpaceBar:
                        IF CallWindowString = '' THEN
                            QSOState := QST_SearchAndPounceSpaceBarPressed;

                    END; { of case Key }

                END  { of QST_Idle and ActionRequired }
            ELSE

                { There are some actions taken just based upon entering a certain number of
                  letters in the CallWindow }

                BEGIN
                { Check to see if AutoStartSend should be triggered.  }

                IF AutoStartSendEnable AND (Mode = CW) THEN
                    IF Length (CallWindowString) > AutoStartSendCharacterCount THEN
                        AutoStartSendStationCalled := True;

                IF (Mode = CW) AND AutoStartSendEnable AND
                   (AutoStartSendCharacterCount > 0) AND
                   (Length (CallWindowString) = AutoStartSendCharacterCount) AND
                   (NOT StringIsAllNumbersOrDecimal (CallWindowString)) AND
                   (NOT StringHas (CallWindowString, '/')) AND
                   (NOT AutoStartSendStationCalled) AND
                   (WhereX > Length (CallWindowString)) THEN
                       BEGIN
                       TBSIQ_CW_Engine.CueCWMessage (CallWindowString, Radio, CWP_High, True);
                       CallsignICameBackTo := CallWindowString;
                       ShowCWMessage (CallWindowString);
                       ShowStationInformation (CallWindowString);
                       AutoStartSendStationCalled := True;
                       QSOState := QST_AutoStartSending;
                       END;

                { Update SuperCheckPartial }

                IF SCPMinimumLetters > 0 THEN
                    IF Length (CallWindowString) >= SCPMinimumLetters THEN
                        SuperCheckPartial;
                END;

            END;  { Of QST_Idle or QST_CQCalled }

        QST_AutoStartSending:  { We have started sending a callsign - can only be here if in CW }
            BEGIN
            QSONumberUnused := False;

            { There are a number of possible things that can happen here:

              1. Someone typed another letter to add to the callsign.
              2. Someone hit the backspace key to possibly delete an unsent letter.
              3. Someone hit escape with an empty window and wants to back up.
              4. Someone hit RETURN to end input of the callsign.
              5. All of the entered letters have been sent - send exchange. }

            { Firstly - we are going to check to see if there are no new characterrs
              to send - and we have finished sending CW }

            IF (NOT ActionRequired) AND TBSIQ_CW_Engine.CWFinished (Radio) THEN   { we are done sending the call }
                BEGIN
                IF AutoCallTerminate THEN
                    BEGIN
                    IF (NOT AutoDupeEnableCQ) OR (NOT WindowDupeCheck) THEN
                        BEGIN
                        ExpandedString := ExpandCrypticString (CQExchange);
                        AppendCWMessageDisplay (ExpandedString);
                        TBSIQ_CW_Engine.CueCWMessage (ExpandedString, Radio, CWP_High, False);
                        QSOState := QST_CQExchangeBeingSent;
                        END
                    ELSE
                        BEGIN  { Is a dupe }
                        ExpandedString := ExpandCrypticString (QSOBeforeMessage);
                        TBSIQ_CW_Engine.CueCWMessage (ExpandedString, Radio, CWP_High, False);
                        ShowCWMessage (ExpandedString);
                        QSOState := QST_Idle;
                        END;

                    ShowStationInformation (CallWindowString);
                    END;
                END;

            IF NOT ActionRequired THEN Exit;  { No keystroke to respond to }

            { We have a keystroke to act upon - and we don't care of we are done sending CW }

            CASE Key OF
                EscapeKey:
                    BEGIN
                    QSOState := QST_Idle;  { Do not start sending the exchange! }
                    END;

                CarriageReturn:
                    BEGIN
                    IF (NOT AutoDupeEnableCQ) OR (NOT WindowDupeCheck) THEN
                        BEGIN
                        CASE Mode OF
                            CW, Digital:
                                BEGIN
                                IF Mode = CW THEN
                                    BEGIN
                                    ExpandedString := ExpandCrypticString (CQExchange);
                                    AppendCWMessageDisplay (ExpandedString);
                                    TBSIQ_CW_Engine.CueCWMessage (ExpandedString, Radio, CWP_High, False);
                                    END;

                                IF Mode = Digital THEN
                                    BEGIN
                                    ExpandedString := ExpandCrypticString (GetExMemoryString (Digital, F2));
                                    AppendCWMessageDisplay (ExpandedString);
                                    ActiveRadio := Radio;
                                    ActiveMode := Mode;

                                    { We have waited until now to send the callsign }

                                    TransmitCountdown := InitialTransmitCountdown;
                                    FinishRTTYTransmission (CallsignICameBackTo + ExpandedString);
                                    END;
                                END;

                            { On phone - we expecte the ExpandCrypticString procedure to actually
                              send commands to the radio to start a voice memory }

                            Phone:
                                IF CQPhoneExchange <> '' THEN
                                    BEGIN
                                    ExpandedString := ExpandCrypticString (CQPhoneExchange);
                                    TransmitCountdown := InitialTransmitCountdown;
                                    END;

                            END;

                        QSOState := QST_CQExchangeBeingSent;
                        Exit;
                        END
                    ELSE
                        BEGIN  { Call is a dupe }
                        IF Mode = CW THEN
                            BEGIN
                            ExpandedString := ExpandCrypticString (QSOBeforeMessage);
                            TBSIQ_CW_Engine.CueCWMessage (ExpandedString, Radio, CWP_High, False);
                            ShowCWMessage (ExpandedString);
                            END;

                        IF Mode = Digital THEN
                            BEGIN
                            ExpandedString := ExpandCrypticString (GetEXMemoryString (Digital, F2));
                            ActiveRadio := Radio;
                            ActiveMode := Mode;
                            TransmitCountdown := InitialTransmitCountdown;
                            FinishRTTYTransmission (ExpandedString);
                            ShowCWMessage (ExpandedString);
                            END;

                        IF Mode = Phone THEN  { Guess I am okay doing nothing ATM }
                            BEGIN
                            END;

                        QSOState := QST_Idle;
                       END;
                    END;

                BackSpace:  { See if we can delete an unsent character }
                    IF TBSIQ_CW_Engine.DeleteLastCharacter (Radio) THEN
                        BEGIN
                        Delete (CallWindowString, Length (CallWindowString), 1);
                        Delete (CallsignICameBackTo, Length (CallSignICameBackTo), 1);
                        ClrScr;
                        Write (CallWindowString);
                        Dec (CallWindowCursorPosition);
                        END;

                ELSE
                    BEGIN
                    Key := UpCase (Key);

                    IF ((Key >= '0') AND (Key <= 'Z')) OR (Key = '/') THEN
                        BEGIN
                        CallsignICameBackTo := CallSignICameBackTo + Key;

                        { Note that this bypasses the CW message cues }

                        IF Mode = CW THEN
                            BEGIN
                            TBSIQ_CW_Engine.AddCharacterToBuffer (Key, Radio);
                            ShowCWMessage (CallsignICameBackTo);
                            END;

                        { Update SuperCheckPartial }

                        IF SCPMinimumLetters > 0 THEN
                            IF Length (CallWindowString) >= SCPMinimumLetters THEN
                                SuperCheckPartial;
                        END;
                    END;

                END; { of case Key }
            END;


        QST_CallingCQ:
            BEGIN
            ListenToOtherRadio;   { Is this okay if my message is in the cue? }

            IF Mode = CW THEN
                BEGIN
                IF TBSIQ_CW_Engine.CWFinished (Radio) THEN
                    BEGIN
                    ListenToBothRadios;
                    ShowCWMessage ('');
                    QSOState := QST_CQCalled;
                    END;
                END
            ELSE
                IF NOT IAmTransmitting THEN
                    BEGIN
                    ListenToBothRadios;
                    ShowCWMessage ('');
                    QSOState := QST_CQCalled;
                    END;
            END;

        QST_AutoCQCalling:
            BEGIN
            IF IAmTransmitting THEN
                ListenToOtherRadio
            ELSE
                BEGIN   { Done with transmission }
                ListenToBothRadios;
                Str (AutoCQDelayTime DIV 100, TempString);

                IF AutoCQDelayTime MOD 100 <> 0 THEN
                    TempString := TempString + '.5'
                ELSE
                    TempString := TempString + '.0';

                ShowCWMessage ('Listen time = ' + TempString + ' secs (PgUp/Dn)');
                QSOState := QST_AutoCQListening;
                MarkTime (AutoCQFinishTime);
                END;
            END;

        QST_AutoCQListening:
            BEGIN
            { If a key was pressed - WindowEditor will put us into CQIdle state }

            { Check timer }

            IF ElaspedSec100 (AutoCQFinishTime) >= AutoCQDelayTime THEN
                BEGIN
                SendFunctionKeyMessage (AutoCQMemory, Message);
                ShowCWMessage ('AutoCQ: ' + Message);
                QSOState := QST_AutoCQCalling;
                ListenToOtherRadio;
                END;
            END;

        QST_CQStationBeingAnswered:
            BEGIN
            QSONumberUnused := False;
            ListenToOtherRadio;

            { If we are on phone - let's assume we are going to be in transmit mode for at
              least a couple of seconds }

            { We used to not do this until CW was done }

            IF (NOT AutoDupeEnableCQ) OR (NOT WindowDupeCheck) THEN
                BEGIN
                CASE Mode OF
                    CW:
                        BEGIN
                        ExpandedString := ExpandCrypticString (CQExchange);
                        TBSIQ_CW_Engine.CueCWMessage (ExpandedString, Radio, CWP_High, False);
                        AppendCWMessageDisplay (ExpandedString);
                        END;

                    Digital:
                        BEGIN
                        ExpandedString := ExpandCrypticString (GetEXMemoryString (Digital, F2));

                        ActiveMode := Mode;
                        ActiveRadio := Radio;

                        { We have waited until now to send the callsign }

                        TransmitCountdown := InitialTransmitCountdown;
                        FinishRTTYTransmission (ExpandedString);
                        AppendCWMessageDisplay (ExpandedString);
                        END;

                    Phone:
                        IF CQPhoneExchange <> '' THEN
                            BEGIN
                            ExpandedString := ExpandCrypticString (CQPhoneExchange);
                            TransmitCountdown := InitialTransmitCountdown;
                            END;

                    END;

                ShowStationInformation (CallsignICameBackTo);

                QSOState := QST_CQExchangeBeingSent;
                Exit;
                END
            ELSE
                BEGIN
                CASE Mode OF
                    CW:
                        BEGIN
                        ExpandedString := ExpandCrypticString (QSOBeforeMessage);
                        TBSIQ_CW_Engine.CueCWMessage (ExpandedString, Radio, CWP_High, False);
                        ShowCWMessage (ExpandedString);
                        END;

                    Digital:
                        BEGIN
                        ExpandedString := ExpandCrypticString (QSOBeforeMessage);
                        ActiveMode := Mode;
                        ActiveRadio := Radio;

                        { We have waited until now to send the callsign }

                        TransmitCountdown := InitialTransmitCountdown;
                        FinishRTTYTransmission (CallsignICameBackTo + ExpandedString);
                        AppendCWMessageDisplay (ExpandedString);
                        END;

                    Phone:
                        IF QSOBeforeMessage <> '' THEN
                            BEGIN
                            ExpandedString := ExpandCrypticString (QSOBeforeMessage);
                            TransmitCountdown := InitialTransmitCountdown;
                            END;

                    END; { of CASE Mode }

                QSOState := QST_Idle;
                END;

            END;

        QST_CQStationBeingAnsweredSilent:  { Special version that sends no CW }
            BEGIN
            ListenToOtherRadio;
            QSOState := QST_CQExchangeBeingSent;
            END;

        { We are sending the CQ exchange to the guy who came back.  We can get the
          exchange window prepared for data entry - and put the initial exchange there if
          we have one }

        QST_CQExchangeBeingSent:
            BEGIN
            DisplayEditableLog (VisibleLog.LogEntries);

            DoPossibleCalls (CallWindowString);
            ListenToOtherRadio;

            { Put up exchange window and any initial exchange }

            IF TBSIQ_Activewindow = TBSIQ_CallWindow THEN
                BEGIN
                SetTBSIQWindow (TBSIQ_ExchangeWindow);
                Clrscr;
                ExchangeWindowString := '';
                ExchangeWindowCursorPosition := 1;
                END;

            { Now see if we have an initial exchange }

            InitialExchange := InitialExchangeEntry (CallWindowString);

            IF InitialExchange <> '' THEN
                BEGIN
                ExchangeWindowString := InitialExchange;
                Write (ExchangeWindowString);
                ExchangeWindowCursorPosition := Length (ExchangeWindowString) + 1;

                IF InitialExchangeOverwrite THEN
                    InitialExchangePutUp := ExchangeWindowString <> '';
                END;

            QSOState := QST_CQExchangeBeingSentAndExchangeWindowUp;
            END;

        { We are here if we have put up the exchange window and initial exchange - waiting
          for CW to be done - any edits made - and then RETURN to be pressed }

        QST_CQExchangeBeingSentAndExchangeWindowUp:
            BEGIN
            ListenToOtherRadio;

            DisplayUserInfo (CallWindowString);

            IF ActionRequired THEN
                CASE Key OF
                    CarriageReturn:
                        BEGIN
                        { Not sure what to do about this }
                        END;

                    NullKey:
                        BEGIN
                        { on the fence about function key memories while the CQ Exchange is being sent }

                        IF ValidFunctionKey (ExtendedKey) THEN  { Send function key message }
                            BEGIN
                            SendFunctionKeyMessage (ExtendedKey, Message);
                            Exit;
                            END;

                        { Extened keys other than function keys }

                        CASE ExtendedKey OF
                            UpArrow:
                                IF TBSIQ_ActiveWindow = TBSIQ_ExchangeWindow THEN
                                    SetTBSIQWindow (TBSIQ_CallWindow);

                            DownArrow:
                                IF TBSIQ_ActiveWindow = TBSIQ_CallWindow THEN
                                    SetTBSIQWindow (TBSIQ_ExchangeWindow);

                            END;  { of case ExtendedKey }
                        END;

                    TabKey: SwapWindows;
                    END;  { of CASE Key }

            IF TBSIQ_CW_Engine.CWFinished (Radio) THEN
                BEGIN
                ListenToBothRadios;
                QSOState := QST_CQWaitingForExchange;
                END;
            END;

        { Am waiting for a RETURN to be pressed and have someting that looks legit in both
          the CallWindow and ExchangeWindow that I can log }

        QST_CQWaitingForExchange:
            BEGIN
            IF ActionRequired THEN
                CASE Key OF
                    EscapeKey:
                        IF TBSIQ_ActiveWindow = TBSIQ_ExchangeWindow THEN
                            BEGIN
                            RemoveExchangeWindow;
                            SetTBSIQWindow (TBSIQ_CallWindow);
                            QSOState := QST_CQCalled;
                            END
                        ELSE
                            BEGIN
                            RemoveExchangeWindow;
                            ClrScr;
                            CallWindowString := '';
                            CallWindowCursorPosition := 1;
                            QSOState := QST_Idle;
                            END;

                    CarriageReturn, '\':
                        BEGIN
                        DisplayUserInfo (CallWindowString);
                        IF TBSIQ_ParametersOkay (CallWindowString,
                                                 QSONumberForThisQSO,
                                                 ExchangeWindowString,
                                                 Band, Mode, Frequency, RData) THEN
                            BEGIN
                            IF NOT BeSilent THEN
                                BEGIN

                                { Let's build the string we need to send to acknowledge the QSO }

                                TempString := '';

                                { This ignores the partial callsign correction possibility }

                                IF Mode <> Phone THEN
                                    IF (RData.Callsign <> CallsignICameBackTo) THEN
                                        BEGIN
                                        TempString := TempString + RData.Callsign + ' ';
                                        CallsignICameBackTo := RData.Callsign;
                                        END;

                                IF Mode = Phone THEN
                                    BEGIN
                                    CASE Key OF
                                        CarriageReturn: TempString := TempString + QSLPhoneMessage;
                                        END;
                                    END

                                ELSE
                                    CASE Key OF
                                        CarriageReturn: TempString := TempString + QSLMessage;
                                        '\':            TempString := TempString + QuickQSLMessage1;
                                        ']':            TempString := TempString + QuickQSLMessage2;
                                        END;

                                CASE Mode OF
                                    CW:
                                        BEGIN
                                        ExpandedString := ExpandCrypticString (TempString);
                                        TBSIQ_CW_Engine.CueCWMessage (ExpandedString, Radio, CWP_High, False);
                                        ShowCWMessage (ExpandedString);
                                        END;

                                    Digital:
                                        BEGIN
                                        ExpandedString := ExpandCrypticString (TempString);
                                        TransmitCountDown := InitialTransmitCountdown;
                                        ActiveMode := Mode;
                                        ActiveRadio := Radio;
                                        ShowCWMessage (ExpandedString);
                                        FinishRTTYTransmission (ExpandedString);
                                        END;

                                    Phone:
                                        IF TempString <> '' THEN
                                            BEGIN
                                            ExpandCrypticString (TempString);
                                            TransmitCountDown := InitialTransmitCountdown;
                                            END;

                                    END;  { of CASE Mode }
                                END;

                            RData.Radio := Radio;
                            TBSIQ_LogContact (RData);

                            IF VisibleDupeSheetEnable THEN
                                BEGIN
                                VisibleDupeSheetChanged := True;
                                VisibleLog.DisplayVisibleDupeSheet (Band, Mode);
                                TBSIQ_BandMapFocus := Radio;
                                END;

                            ShowStationInformation (CallWindowString);
                            QSOState := QST_CQSending73Message;
                            AutoStartSendStationCalled := False;

                            QSONumberForPreviousQSO := QSONumberForThisQSO;
                            SetUpNextQSONumber;
                            END

                        ELSE
                            IF TBSIQ_ActiveWindow = TBSIQ_CallWindow THEN
                                SwapWindows;
                        END;

                    NullKey:
                        BEGIN
                        IF ValidFunctionKey (ExtendedKey) THEN  { Send function key message }
                            BEGIN
                            SendFunctionKeyMessage (ExtendedKey, Message);

                            IF Mode = CW THEN
                                QSOState := QST_CQExchangeBeingSentAndExchangeWindowUp;
                            Exit;
                            END;

                        { Extened keys other than function keys }

                        CASE ExtendedKey OF
                            CarriageReturn:   { Do not send any CW }
                                IF TBSIQ_ParametersOkay (CallWindowString,
                                                         QSONumberForThisQSO,
                                                         ExchangeWindowString,
                                                         Band, Mode, Frequency, RData) THEN
                                    BEGIN
                                    RData.Radio := Radio;
                                    TBSIQ_LogContact (RData);

                                    IF VisibleDupeSheetEnable THEN
                                        BEGIN
                                        VisibleDupeSheetChanged := True;
                                        VisibleLog.DisplayVisibleDupeSheet (Band, Mode);
                                        TBSIQ_BandmapFocus := Radio;
                                        END;

                                    ShowStationInformation (CallWindowString);
                                    QSOState := QST_CQSending73Message;
                                    AutoStartSendStationCalled := False;

                                    QSONumberForPreviousQSO := QSONumberForThisQSO;
                                    SetUpNextQSONumber;
                                    END;

                            UpArrow:
                                IF TBSIQ_ActiveWindow = TBSIQ_ExchangeWindow THEN
                                    SetTBSIQWindow (TBSIQ_CallWindow);

                            DownArrow:
                                IF TBSIQ_ActiveWindow = TBSIQ_CallWindow THEN
                                    SetTBSIQWindow (TBSIQ_ExchangeWindow);

                            END;  { of case ExtendedKey }
                        END;  { of NullKey}

                    END; { of CASE Key }
            END;  { of QST_WaitingForExchange }

        { QSO is logged and are sending the QSO Message }

        QST_CQSending73Message:
            BEGIN
            ListenToOtherRadio;

            { Moved this up here instead of waiting for CW to end }

            IF ExchangeWindowIsUp THEN
                BEGIN
                RemoveExchangeWindow;
                CallWindowString := '';
                CallWindowCursorPosition := 1;
                SetTBSIQWindow (TBSIQ_CallWindow);
                ClrScr;
                END;

            IF Mode = CW THEN
                BEGIN
                IF TBSIQ_CW_Engine.CWFinished (Radio) THEN
                    BEGIN
                    ListenToBothRadios;
                    ShowCWMessage ('');
                    QSOState := QST_Idle;
                    END;
                END
            ELSE
                IF NOT IAmTransmitting THEN
                    BEGIN
                    ListenToBothRadios;
                    ShowCWMessage ('');
                    QSOState := QST_Idle;
                    END;
            END;

        QST_StartSendingKeyboardCW:  { Only called once }
            BEGIN
            KeyboardCWMessage := '';
            QSOState := QST_SendingKeyboardCWWaiting;

            IF Mode = Digital THEN
                StartRTTYTransmission ('');
            END;

        QST_SendingKeyboardCWWaiting:
            BEGIN
            { Not sure if I need this step for sure - but will leave it in the chain }

            IF ActionRequired THEN
                BEGIN
                END;

            QSOState := QST_SendingKeyboardCW;
            END;

        QST_SendingKeyboardCW:
            BEGIN
            IF ActionRequired THEN
                CASE Key OF
                    CarriageReturn, EscapeKey:
                        BEGIN
                        QSOState := RememberQSOState;

                        IF Mode = Digital THEN
                            FinishRTTYTransmission ('');

                        Exit;
                        END;

                    ELSE
                        IF (Key >= ' ') AND (Key <= 'z') THEN
                            BEGIN
                            IF Mode = CW THEN
                                BEGIN
                                TBSIQ_CW_Engine.CueCWMessage (Key, Radio, CWP_High, False);
                                KeyboardCWMessage := KeyboardCWMessage + Key;
                                ShowCWMessage (KeyboardCWMessage);
                                END;

                            IF Mode = Digital THEN
                                BEGIN
                                ContinueRTTYTransmission (Key);
                                KeyboardCWMessage := KeyboardCWMessage + Key;
                                ShowCWMessage (KeyboardCWMessage);
                                END;
                            END;

                    END;  { of CASE Key }
            END;

        QST_SearchAndPounceInit:  { Executed once when entering S&P Mode }
            BEGIN
            QSONumberUnused := False;

            IF NOT TBSIQ_CW_Engine.CWFinished (Radio) THEN Exit;

            { Determines color of exchange window }

            CASE Radio OF
                RadioOne: R1_OpMode := SearchAndPounceOpMode;
                RadioTwo: R2_OpMode := SearchandPounceOpMode;
                END;  { of case }

            RemovePossibleCallWindow;

            TBSIQ_BandMapFocus := Radio;

            ClearAutoStartSendDisplay;

            ExchangeWindowString := '';
            ExchangeWindowCursorPosition := 1;

            SetTBSIQWindow (TBSIQ_ExchangeWindow);
            ClrScr;

            SetTBSIQWindow (TBSIQ_CallWindow);
            IF CallWindowString = '' THEN ClrScr;  { In case someone wanted it cleared }

            SearchAndPounceStationCalled := False;
            SearchAndPounceExchangeSent := False;
            QSOState := QST_SearchAndPounce;
            END;

        QST_SearchAndPounceSpaceBarPressed:
            BEGIN
            { We come here when someone pressed the SPACE BAR with an empty
              call window - so we need to jump into S&P mode and send my
              callsign. Also - if the other instance of the radio is only
              calling CQ - we need to stop the CW so the call can be made
              quickly.  }

            ClearAutoStartSendDisplay;

            ExchangeWindowString := '';
            ExchangeWindowCursorPosition := 1;

            SetTBSIQWindow (TBSIQ_ExchangeWindow);
            ClrScr;
            SetTBSIQWindow (TBSIQ_CallWindow);

            { Stop CW on the other radio if just calling CQ }

            CASE Radio OF
                RadioOne:
                    IF Radio2QSOMachine.QSOState = QST_CallingCQ THEN
                        TBSIQ_CW_Engine.ClearMessages (RadioTwo, True);

                RadioTwo:
                    IF Radio1QSOMachine.QSOState = QST_CallingCQ THEN
                        TBSIQ_CW_Engine.ClearMessages (RadioOne, True);
                END;

            SendFunctionKeyMessage (F1, Message);

            SearchAndPounceStationCalled := True;
            SearchAndPounceExchangeSent := False;
            QSOState := QST_SearchAndPounce;

            BandMapBand := DisplayedBand;
            BandMapMode := DisplayedMode;
            BandMapCursorFrequency := Frequency;
            TBSIQ_BandMapFocus := Radio;
            END;

        QST_SearchAndPounce:
            BEGIN
            { SCP doesn't do anything if called with previous string }

            IF SCPMinimumLetters > 0 THEN
                IF Length (CallWindowString) >= SCPMinimumLetters THEN
                    SuperCheckPartial;

            IF ActionRequired THEN
                BEGIN
                CASE Key OF
                    TabKey: SwapWindows;

                    SpaceBar:
                        BEGIN
                        SendFunctionKeyMessage (F1, Message);
                        SearchAndPounceStationCalled := True;
                        SearchAndPounceExchangeSent := False;
                        ShowStationInformation (CallWindowString);
                        END;

                    EscapeKey:   { Only get this if we have an empty string }
                        IF TBSIQ_ActiveWindow = TBSIQ_ExchangeWindow THEN
                            BEGIN
                            SetTBSIQWindow (TBSIQ_CallWindow);
                            SearchAndPounceStationCalled := False;
                            SearchAndPounceExchangeSent := False;  { I guess }
                            END
                        ELSE
                            BEGIN
                            QSOState := QST_Idle;
                            RemoveExchangeWindow;
                            END;

                    CarriageReturn:
                        BEGIN
                        IF StringIsAllNumbersOrDecimal (CallWindowString) THEN
                            BEGIN
                            IF Length (WindowString) = 3 THEN
                                BEGIN
                                CASE BandMemory[Radio] OF
                                    Band160: TempString := '1'   + WindowString;
                                    Band80:  TempString := '3'   + WindowString;
                                    Band40:  TempString := '7'   + WindowString;
                                    Band30:  TempString := '10'  + WindowString;
                                    Band20:  TempString := '14'  + WindowString;
                                    Band17:  TempString := '18'  + WindowString;
                                    Band15:  TempString := '21'  + WindowString;
                                    Band12:  TempString := '24'  + WindowString;
                                    Band10:  TempString := '28'  + WindowString;
                                    Band6:   TempString := '50'  + WindowString;
                                    ELSE     TempString := '144' + WindowString;
                                    END;
                                END
                            ELSE
                                TempString := WindowString;

                            IF StringHas (TempString, '.') THEN
                                BEGIN
                                Val (TempString, RealFreq, xResult);
                                Freq := Round (RealFreq * 1000.0);
                                END
                            ELSE
                                Val (TempString + '000', Freq, xResult);

                            IF xResult = 0 THEN
                                SetRadioFreq (Radio, Freq, Mode, 'A');

                            CallWindowString := '';
                            CallWindowCursorPosition := 1;
                            ClrScr;
                            Exit;
                            END;

                        { This is a thing I don't normally do when programming - but it is very
                          important that WindowDupeCheck be last in the next IF statement!  You
                          do not want it executed if you have started the QSO }

                        IF AutoDupeEnableSandP AND (NOT SearchAndPounceStationCalled) AND WindowDupeCheck THEN
                            BEGIN  { get out of here }
                            SearchAndPounceStationCalled := False;
                            Exit;
                            END;

                        { Not a dupe - or we are far enough along in the QSO not to care anymore }

                        IF NOT SearchAndPounceStationCalled THEN
                            BEGIN
                            SendFunctionKeyMessage (F1, Message);
                            SearchAndPounceStationCalled := True;

                            { Now - do we go to the exchange window? }

                            IF GoodCallSyntax (CallWindowString) THEN
                                BEGIN
                                SetTBSIQWindow (TBSIQ_ExchangeWindow);

                                IF ExchangeWindowString = '' THEN
                                    BEGIN
                                    InitialExchange := InitialExchangeEntry (CallWindowString);

                                    IF InitialExchange <> '' THEN
                                        BEGIN
                                        ExchangeWindowString := InitialExchange;
                                        Write (ExchangeWindowString);
                                        ExchangeWindowCursorPosition := Length (ExchangeWindowString) + 1;
                                        END;
                                    END;

                                DoPossibleCalls (CallWindowString);
                                END;
                            END

                        { Send the exchange if not sent already and try to log the QSO }

                        ELSE
                            BEGIN
                            { Send exchange if it hasn't already been sent }

                            IF NOT SearchAndPounceExchangeSent THEN
                                BEGIN
                                CASE Mode OF
                                    Digital:
                                        BEGIN
                                        ActiveRadio := Radio;
                                        ActiveMode := Mode;
                                        ExpandedString := ExpandCrypticString (GetEXMemoryString (Digital, F2));
                                        ShowCWMessage (ExpandedString);
                                        TransmitCountdown := InitialTransmitCountdown;
                                        FinishRTTYTransmission (ExpandedString);
                                        END;

                                    CW: BEGIN
                                        ExpandedString := ExpandCrypticString (SearchAndPounceExchange);
                                        TBSIQ_CW_Engine.CueCWMessage (ExpandedString, Radio, CWP_High, False);
                                        ShowCWMessage (ExpandedString);
                                        END;

                                    Phone:
                                        IF SearchAndPouncePhoneExchange <> '' THEN
                                            BEGIN
                                            TransmitCountdown := InitialTransmitCountdown;
                                            ExpandedString := ExpandCrypticString (SearchAndPouncePhoneExchange);
                                            ShowCWMessage ('Sent S&P PhoneExchange');
                                            END;

                                    END;  { of CASE Mode }

                                SearchAndPounceExchangeSent := True;
                                END;

                            { Maybe we haven't entered an exchange yet }

                            IF ExchangeWindowString = '' THEN
                                BEGIN
                                IF TBSIQ_ActiveWindow = TBSIQ_CallWindow THEN
                                    SwapWindows;

                                InitialExchange := InitialExchangeEntry (CallWindowString);

                                IF InitialExchange <> '' THEN
                                    BEGIN
                                    ExchangeWindowString := InitialExchange;
                                    Write (ExchangeWindowString);
                                    ExchangeWindowCursorPosition := Length (ExchangeWindowString) + 1;
                                    END;
                                END;

                            { Now we try to log the QSO }

                            IF TBSIQ_ParametersOkay (CallWindowString,
                                                     QSONumberForThisQSO,
                                                     ExchangeWindowString,
                                                     Band, Mode, Frequency, RData) THEN
                                BEGIN
                                LoggedSAndPCall := CallWindowString;
                                MarkTime (LoggedSAndPCallTime);

                                RData.SearchAndPounce := True;
                                RData.Radio := Radio;
                                TBSIQ_LogContact (RData);

                                IF VisibleDupeSheetEnable THEN
                                    BEGIN
                                    VisibleDupeSheetChanged := True;
                                    VisibleLog.DisplayVisibleDupeSheet (Band, Mode);
                                    TBSIQ_BandMapFocus := Radio;
                                    END;

                                ShowStationInformation (CallWindowString);

                                ExchangeWindowString := '';
                                ExchangeWindowCursorPosition := 1;
                                SetTBSIQWindow (TBSIQ_ExchangeWindow);
                                ClrScr;

                                CallWindowString := '';
                                CallWindowCursorPosition := 1;
                                SetTBSIQWindow (TBSIQ_CallWindow);
                                ClrScr;

                                QSONumberForPreviousQSO := QSONumberForThisQSO;
                                SetUpNextQSONumber;

                                IF SprintQSYRule THEN
                                    BEGIN
                                    QSOState := QST_Idle;
                                    RemoveExchangeWindow;
                                    ENd
                                ELSE
                                    QSOState := QST_SearchAndPounceInit;
                                END
                            ELSE
                                ShowCWMessage ('Unable to log this QSO yet');
                            END;
                        END;  { of CarriageReturn }

                    NullKey:
                        BEGIN
                        IF ValidFunctionKey (ExtendedKey) THEN  { Send function key message }
                            BEGIN
                            IF ExtendedKey = F1 THEN    { If the other radio is calling CQ - make it stop }
                               CASE Radio OF
                                   RadioOne:
                                       IF Radio2QSOMachine.QSOState = QST_CallingCQ THEN
                                           TBSIQ_CW_Engine.ClearMessages (RadioTwo, True);

                                   RadioTwo:
                                       IF Radio1QSOMachine.QSOState = QST_CallingCQ THEN
                                           TBSIQ_CW_Engine.ClearMessages (RadioOne, True);
                                   END;

                            SendFunctionKeyMessage (ExtendedKey, Message);

                            IF ExtendedKey = F1 THEN
                                BEGIN
                                IF GoodCallSyntax (CallWindowString) THEN
                                    BEGIN
                                    ThisIsADupe := VisibleLog.CallIsADupe (CallWindowString, Band, Mode);

                                    IF NOT ThisIsADupe THEN
                                        BEGIN
                                        VisibleLog.DetermineIfNewMult (CallWindowString, Band, Mode, MultString);
                                        Mult := MultString <> '';
                                        END
                                    ELSE
                                        Mult := False;

                                    BandMapCursorFrequency := DisplayedFrequency;
                                    BandMapMode := Mode;
                                    BandMapBand := Band;
                                    TBSIQ_BandMapFocus := Radio;
                                    BandMapCallPutUp := CallWindowString;

                                    { Last TRUE indicates to SendToMulti }

                                    NewBandMapEntry (CallWindowString, Frequency, 0, Mode, ThisIsADupe, Mult, BandMapDecayTime, True);
                                    DisablePutUpBandMapCall := False;

                                    SetTBSIQWindow (TBSIQ_ExchangeWindow);

                                    IF ExchangeWindowString = '' THEN
                                        BEGIN
                                        InitialExchange := InitialExchangeEntry (CallWindowString);

                                        IF InitialExchange <> '' THEN
                                            BEGIN
                                            ExchangeWindowString := InitialExchange;
                                            Write (ExchangeWindowString);
                                            ExchangeWindowCursorPosition := Length (ExchangeWindowString) + 1;
                                            END;
                                        END;
                                    END;

                                SearchAndPounceStationCalled := True;
                                END;

                            END

                        ELSE
                            CASE ExtendedKey OF
                                CarriageReturn:   { Just like CarriageReturn without CW }
                                    BEGIN
                                    IF AutoDupeEnableSandP AND (NOT SearchAndPounceStationCalled) AND WindowDupeCheck THEN
                                        BEGIN  { get out of here }
                                        QSOState := QST_SearchAndPounceInit;
                                        Exit;
                                        END;

                                    { Not a dupe - or we are far enough along in the QSO not to care anymore }

                                    IF NOT SearchAndPounceStationCalled THEN
                                        BEGIN
                                        IF GoodCallSyntax (CallWindowString) THEN
                                            BEGIN
                                            SetTBSIQWindow (TBSIQ_ExchangeWindow);

                                            IF ExchangeWindowString = '' THEN
                                                BEGIN
                                                InitialExchange := InitialExchangeEntry (CallWindowString);

                                                IF InitialExchange <> '' THEN
                                                    BEGIN
                                                    ExchangeWindowString := InitialExchange;
                                                    Write (ExchangeWindowString);
                                                    ExchangeWindowCursorPosition := Length (ExchangeWindowString) + 1;
                                                    END;
                                                END;

                                            DoPossibleCalls (CallWindowString);
                                            END;

                                        SearchAndPounceStationCalled := True;
                                        END

                                    { Send the exchange if not sent already and try to log the QSO }

                                    ELSE
                                        BEGIN
                                        { Maybe we haven't entered an exchange yet }

                                        IF ExchangeWindowString = '' THEN
                                            BEGIN
                                            IF TBSIQ_ActiveWindow = TBSIQ_CallWindow THEN
                                                SwapWindows;

                                            InitialExchange := InitialExchangeEntry (CallWindowString);

                                            IF InitialExchange <> '' THEN
                                                BEGIN
                                                ExchangeWindowString := InitialExchange;
                                                Write (ExchangeWindowString);
                                                ExchangeWindowCursorPosition := Length (ExchangeWindowString) + 1;
                                                END;
                                            END;

                                        { Now we try to log the QSO }

                                        IF TBSIQ_ParametersOkay (CallWindowString,
                                                                 QSONumberForThisQSO,
                                                                 ExchangeWindowString,
                                                                 Band, Mode, Frequency, RData) THEN
                                            BEGIN
                                            EscapeDeletedCallEntry := CallWindowString;
                                            RData.SearchAndPounce := True;
                                            RData.Radio := Radio;
                                            TBSIQ_LogContact (RData);

                                            IF VisibleDupeSheetEnable THEN
                                                BEGIN
                                                VisibleDupeSheetChanged := True;
                                                VisibleLog.DisplayVisibleDupeSheet (Band, Mode);
                                                TBSIQ_BandMapFocus := Radio;
                                                END;

                                            ShowStationInformation (CallWindowString);

                                            ExchangeWindowString := '';
                                            ExchangeWindowCursorPosition := 1;
                                            SetTBSIQWindow (TBSIQ_ExchangeWindow);
                                            ClrScr;

                                            CallWindowString := '';
                                            CallWindowCursorPosition := 1;
                                            SetTBSIQWindow (TBSIQ_CallWindow);
                                            ClrScr;

                                            QSONumberForPreviousQSO := QSONumberForThisQSO;
                                            SetUpNextQSONumber;

                                            IF SprintQSYRule THEN
                                                BEGIN
                                                QSOState := QST_Idle;
                                                RemoveExchangeWindow;
                                                ENd
                                            ELSE
                                                QSOState := QST_SearchAndPounceInit;
                                            END
                                        ELSE
                                            ShowCWMessage ('Unable to log this QSO yet');
                                        END;
                                    END;  { of CarriageReturn }

                                UpArrow:
                                    IF TBSIQ_ActiveWindow = TBSIQ_ExchangeWindow THEN
                                        SetTBSIQWindow (TBSIQ_CallWindow);

                                DownArrow:
                                    IF TBSIQ_ActiveWindow = TBSIQ_CallWindow THEN
                                        SetTBSIQWindow (TBSIQ_ExchangeWindow);

                                AltZ:   { I don't think this ever happens - AltZ handeled in WindowEditor }
                                    BEGIN
                                    IF TBSIQ_ActiveWindow = TBSIQ_CallWindow THEN
                                        SetTBSIQWindow (TBSIQ_ExchangeWindow);

                                    Clrscr;
                                    ExchangeWindowString := '';
                                    ExchangeWindowCursorPosition := 1;

                                    InitialExchange := InitialExchangeEntry (CallWindowString);

                                    IF InitialExchange <> '' THEN
                                        BEGIN
                                        ExchangeWindowString := InitialExchange;
                                        Write (ExchangeWindowString);
                                        ExchangeWindowCursorPosition := Length (ExchangeWindowString) + 1;
                                        END;

                                    ShowStationInformation (CallWindowString);
                                    END;

                                END;  { of CASE ExtendedKey }
                        END;

                    END; { of case Key }
                END;

            { Hand around if the band map is enabled }

            IF NOT BandMapEnable THEN Exit;                        { Go away }

            { and the bandmap focus is this radio }

            IF TBSIQ_BandMapFocus <> Radio THEN Exit;

            { If we have tuned away from an entry that was put up from the band map
              we need to clear the call window }

            IF TBSIQ_ActiveWindow = TBSIQ_CallWindow THEN
                IF BandMapCallPutUp = CallWindowString THEN
                    IF (BandMapBlinkingCall <> CallWindowString) THEN
                        BEGIN
                        SetTBSIQWindow (TBSIQ_ActiveWindow);  { Makes sure we are active }
                        ClrScr;
                        CallWindowString := '';
                        CallWindowCursorPosition := 1;
                        BandMapCallPutUp := '';
                        END;

            { Now we see if there is a new blinking call to put up }

            IF CallWindowString <> '' THEN Exit;                   { Only do it to an empty window }
            IF BandMapBlinkingCall = '' THEN Exit;                 { No blinking call }
            IF DisablePutUpBandMapCall THEN Exit;                  { Someone pressed a key }
            IF TBSIQ_ActiveWindow <> TBSIQ_CallWindow THEN Exit;   { Not in call window }
            IF NOT OkayToPutUpBandMapCall THEN Exit;               { Call is a dupe }

            { Not sure I need to redo this frequency check }

            { This is sometimes putting the band map call up on the wrong radio }

            IF (ABS (BandMapBlinkingCallRecord^.Frequency - Frequency) <= BandMapGuardBand) THEN
                BEGIN
                SetTBSIQWindow (TBSIQ_ActiveWindow);  { Makes sure we are active }
                CallWindowString := BandMapBlinkingCall;
                ClrScr;
                Write (CallWindowString);
                CallWindowCursorPosition := Length (CallWindowString) + 1;

                { Remember that this is the callsign we have put up }

                BandMapCallPutUp := BandMapBlinkingCall;
                END;

            END; { of QST_SearchAndPounce }

        END;  { of case QSOState }
    END;



PROCEDURE QSOMachineObject.DisplayFrequency;

VAR TempFreq: REAL;
    TempkHz: LONGINT;

    BEGIN
    IF Frequency = DisplayedFrequency THEN Exit;
    DisplayedFrequency := Frequency;

    CASE Radio OF
        RadioOne: SaveSetAndClearActiveWindow (TBSIQ_R1_FrequencyWindow);
        RadioTwo: SaveSetAndClearActiveWindow (TBSIQ_R2_FrequencyWindow);
        END;

    IF Frequency <> 0 THEN
        BEGIN
        TempFreq := Frequency DIV 100;
        TempkHz := Trunc (TempFreq);
        Write (TempkHz / 10:0:1, ' kHz');
        END;

    RestorePreviousWindow;
    END;



PROCEDURE QSOMachineObject.DisplayBandMode;

    BEGIN
    IF (Band <> DisplayedBand) OR (Mode <> DisplayedMode)  THEN
        BEGIN
        CASE Radio OF
            RadioOne: SaveSetAndClearActiveWindow (TBSIQ_R1_BandModeWindow);
            RadioTwo: SaveSetAndClearActiveWindow (TBSIQ_R2_BandModeWindow);
            END;

        Write (BandString [Band]);

        CASE Mode OF
            CW:      Write ('CW ');
            Digital: Write ('DIG');
            Phone:   Write ('SSB');
            ELSE     Write ('???');
            END;

        RestorePreviousWindow;

        DisplayedBand := Band;
        DisplayedMode := Mode;
        DisplayCodeSpeed;
        END;
    END;



PROCEDURE QSOMachineObject.UpdateRadioDisplay;

{ Checks the radio band/mode/frequency and updates as appropriate.  In the
  classic user interface in LogWind - this was part of the routine that
  also updated the time and rate displays.  Here - we just focus on the
  radio stuff.  The time and rate stuff is global and in the procedure
  TBSIQ_UpdateTimeAndRateDisplays.

  A new feature is to check to see if the radio is transmitting, and if so,
  put up the red box.  This is primarily used for SSB and RTTY as the CW
  arbritrator does this for CW. }

VAR FrequencyChange, TempFreq: LONGINT;
    TempBand: BandType;
    TempMode: ModeType;
    TimeString: STRING;
    TempQSOTotals: QSOTotalArray;

    BEGIN
    IF QSONumberByBand THEN
        BEGIN
        TempQSOTotals := QSOTotals;    { Get global QSO totals in dupesheet }
        VisibleLog.IncrementQSOTotalsWithContentsOfEditableWindow (TempQSOTotals);  { Add in EditableLog QSOs }
        QSONumberForThisQSO := TempQSOTotals [Band, Both] + 1;
        END;

    DisplayQSONumber;  { Just to always make sure it is right }

    IF DupeShown THEN
        IF ElaspedSec100 (DupeShownTime) > 300 THEN
            BEGIN
            CASE Radio OF
                RadioOne: RemoveWindow (TBSIQ_R1_CWMessageWindow);
                RadioTwo: RemoveWindow (TBSIQ_R2_CWMessageWindow);
                END;

            DupeShown := False;
            DupeShownCallsign := '';
            END;

    IF LoggedSandPCall <> '' THEN
        IF ElaspedSec100 (LoggedSAndPCallTime) > 500 THEN
            LoggedSAndPCall := '';

    { Get a new reading from the clock }

    TimeString := GetFullTimeString;

    { Get the radio information - note that this will get the last read data
      from the radio - not ask for a fresh set of data.  That means it might
      be a few hundred miliiseconds old }

    IF NOT GetRadioParameters (Radio, ' ', TempFreq, TempBand, TempMode, True, False) THEN
        BEGIN
        Frequency := 0;
        DisplayFrequency;
        RadioOnTheMove := False;
        Exit;
        END;

    Frequency := TempFreq;
    Band := TempBand;
    Mode := TempMode;

    DisplayFrequency;
    DisplayBandMode;

    { Determine if the radio appears to be "on the move" }

    IF (LastFrequency <> 0) AND (Frequency <> 0) THEN
        BEGIN
        FrequencyChange := Abs (LastFrequency - Frequency);

        CASE Radio OF
            RadioOne:
                RadioOnTheMove := (FrequencyChange > Rig1FreqPollRate * AutoSAPEnableRate DIV 1000) OR
                                  (ModeMemory [RadioOne] <> Mode);

            RadioTwo:
                RadioOnTheMove := (FrequencyChange > Rig2FreqPollRate * AutoSAPEnableRate DIV 1000) OR
                                  (ModeMemory [RadioTwo] <> Mode);

            END; { of CASE }

        RadioMovingInBandMode := RadioOnTheMove AND (ModeMemory [Radio] = Mode) AND (BandMemory [Radio] = Band);

        { Was > 0.  This clears the flag set when a key is pressed }

        IF FrequencyChange > BandMapGuardBand THEN DisablePutUpBandMapCall := False;

        IF FrequencyChange > 0 THEN
            CASE Radio OF
                RadioOne:
                    IF (N4OGW_RadioOne_BandMap_IP <> '') THEN
                        N4OGW_RadioOne_Bandmap.SetCenterFrequency (Frequency);

                RadioTwo:
                    IF (N4OGW_RadioTwo_BandMap_IP <> '') THEN
                        N4OGW_RadioTwo_Bandmap.SetCenterFrequency (Frequency);

                END;  { of CASE }
        END;

    { W9CF adds this to make his antenna switch work }

    IF Band <> BandMemory [Radio] THEN
        OutputBandInfo (Radio, Band);

    LastFrequency := Frequency;
    BandMemory [Radio] := Band;
    ModeMemory [Radio] := Mode;

    IF FrequencyMemoryEnable THEN
        FreqMemory [Band, Mode] := Frequency;

    { The one thing we use RadioOnTheMove for within the specific radio instance is to
      put the radio into the S&P if not already }

    IF RadioMovingInBandMode AND AutoSAPEnable AND (QSOState <> QST_SearchAndPounce) THEN
        QSOState := QST_SearchAndPounceInit;

    { We use RadioFrequencySettledCount to see if we have landed on a frequency }

    IF RadioMovingInBandMode AND (QSOState = QST_SearchAndPounce) THEN
        RadioFrequencySettledCount := 1;

    DisplayTXColor;
    DisplayActiveRadio;

    { Check to see if the second clock has ticked }

    IF TimeString = LastFullTimeString THEN Exit;
    LastFullTimeString := TimeString;

    { We are now only executing this code once a second - per radio }

    IF TransmitCountDown > 0 THEN Dec (TransmitCountDown);

    { PTTTest is used for testing purposes only }

    IF DoingPTTTest THEN
        BEGIN
        Dec (PTTTestTimer);

        IF PTTTestTimer <= 0 THEN
            BEGIN
            PTTTestTimer := 3;
            IF PTTState THEN  { PTT is on }
                BEGIN
                ActiveKeyer.PTTUnForce;;
                ShowCWMessage ('PTT On');
                END
            ELSE
                BEGIN
                ActiveKeyer.PTTForceOn;
                ShowCWMessage ('PTT Off');
                END;

            PTTState := NOT PTTState;
            END;
        END;

    { Update the cursor frequency of the band map and display the band map }

    IF (TBSIQ_BandMapFocus = Radio) AND (BandMapCursorFrequency <> Frequency) THEN
        BEGIN
        BandMapCursorFrequency := Frequency;
        DisplayBandMap;
        END;

    IF (RadioFrequencySettledCount > 0) AND (QSOState = QST_SearchAndPounce) THEN
        Dec (RadioFrequencySettledCount)
    ELSE
        OkayToPutUpBandMapCall := True;
    END;



PROCEDURE QSOMachineObject.ShowCWMessage (Message: STRING);

    BEGIN
    CWMessageDisplayed := Message;

    CASE Radio OF
        RadioOne: SaveSetAndClearActiveWindow (TBSIQ_R1_CWMessageWindow);
        RadioTwo: SaveSetAndClearActiveWindow (TBSIQ_R2_CWMessageWindow);
        END;

    Write (CWMessageDisplayed);
    RestorePreviousWindow;
    END;


PROCEDURE QSOMachineObject.ShowDupeMessage (Message: STRING);

    BEGIN
    CWMessageDisplayed := Message;

    CASE Radio OF
        RadioOne: SaveSetAndClearActiveWindow (TBSIQ_R1_CWMessageWindow);
        RadioTwo: SaveSetAndClearActiveWindow (TBSIQ_R2_CWMessageWindow);
        END;

    SetBackground (Red);
    SetColor (Blue);
    ClrScr;
    Write (CWMessageDisplayed);
    RestorePreviousWindow;
    END;


PROCEDURE QSOMachineObject.AppendCWMessageDisplay (Message: STRING);

    BEGIN
    CWMessageDisplayed := CWMessageDisplayed + Message;

    CASE Radio OF
        RadioOne: SaveSetAndClearActiveWindow (TBSIQ_R1_CWMessageWindow);
        RadioTwo: SaveSetAndClearActiveWindow (TBSIQ_R2_CWMessageWindow);
        END;

    Write (CWMessageDisplayed);
    RestorePreviousWindow;
    END;



PROCEDURE QSOMachineObject.ShowStateMachineStatus;

    BEGIN
    CASE Radio OF
        RadioOne: SaveSetAndClearActiveWindow (TBSIQ_R1_StateMachineStatusWindow);
        RadioTwo: SaveSetAndClearActiveWindow (TBSIQ_R2_StateMachineStatusWindow);
        END;

    CASE QSOState OF
        QST_AltDInput: Write ('Sending input to other radio');
        QST_Idle: Write ('CQ Mode - Idle');
        QST_AutoStartSending: Write ('Auto start send started');
        QST_CallingCQ: Write ('CQing');

        QST_CQCalled:
            BEGIN
            Write ('CQ Called');
            IF DualingCQState <> NoDualingCQs THEN Write (' Dualing CQs');
            END;

        QST_AutoCQCalling: Write ('Calling Auto-CQ');
        QST_AutoCQListening: Write ('Auto-CQ listening');
        QST_CQStationBeingAnswered: Write ('CQ Station Being Answered');
        QST_CQExchangeBeingSent: Write ('Exchange being sent');
        QST_CQExchangeBeingSentAndExchangeWindowUp: Write ('Exchange being sent + ExWindow');
        QST_CQWaitingForExchange: Write ('Waiting for exchange');
        QST_CQSending73Message: Write ('Sending 73 message');
        QST_SearchAndPounce: Write ('Search and Pounce');
        QST_SearchAndPounceInit: Write ('Search and Pounce');
        QST_StartSendingKeyboardCW: Write ('Starting keyboard CW');
        QST_SendingKeyboardCW: Write ('Keyboard CW - RETURN to end');
        QST_SendingKeyboardCWWaiting: Write ('Keyboard CW - waiting on other TX')
        ELSE Write ('???');
        END;

    Write (' ', TransmitCountdown);

    RestorePreviousWindow;
    END;



PROCEDURE QSOMachineObject.SetTBSIQWindow (TBSIQ_Window: TBSIQ_WindowType);

{ This is all done without doing anything to the saved window list.  I guess we feel
  that this will only be happening when the saved window list is empty.  In all cases,
  you need the "generic" TBSIQ_ window name which gets expanded into specific windows
  that LOGWIND understands depending on which radio you are using. }

    BEGIN
    TBSIQ_ActiveWindow := TBSIQ_Window;

    CASE Radio OF
        RadioOne:
            CASE TBSIQ_Window OF
                TBSIQ_CallWindow:
                    IF ActiveWindow <> TBSIQ_R1_CallWindow THEN
                        BEGIN
                        SetActiveWindow (TBSIQ_R1_CallWindow);
                        GoToXY (CallWindowCursorPosition, 1);
                        END;

                TBSIQ_StartSendingWindow:
                    IF ActiveWindow <> TBSIQ_R1_StartSendingWindow THEN
                        SetActiveWindow (TBSIQ_R1_StartSendingWindow);

                TBSIQ_ExchangeWindow:
                    IF ActiveWindow <> TBSIQ_R1_ExchangeWindow THEN
                        BEGIN
                        SetActiveWindow (TBSIQ_R1_ExchangeWindow);

                        IF NOT ExchangeWindowIsUp THEN
                            BEGIN
                            ClrScr;
                            ExchangeWindowIsUp := True;
                            END;

                        GoToXY (ExchangeWindowCursorPosition, 1);
                        END;
                END;  { of case }

        RadioTwo:
            CASE TBSIQ_Window OF
                TBSIQ_CallWindow:
                    IF ActiveWindow <> TBSIQ_R2_CallWindow THEN
                        BEGIN
                        SetActiveWindow (TBSIQ_R2_CallWindow);
                        GoToXY (CallWindowCursorPosition, 1);
                        END;

                TBSIQ_StartSendingWindow:
                    IF ActiveWindow <> TBSIQ_R2_StartSendingWindow THEN
                        SetActiveWindow (TBSIQ_R2_StartSendingWindow);

                TBSIQ_ExchangeWindow:
                    IF ActiveWindow <> TBSIQ_R2_ExchangeWindow THEN
                        BEGIN
                        SetActiveWindow (TBSIQ_R2_ExchangeWindow);

                        IF NOT ExchangeWindowIsUp THEN
                            BEGIN
                            ClrScr;
                            ExchangeWindowIsUp := True;
                            END;

                        GoToXY (ExchangeWindowCursorPosition, 1);
                        END;
                END;  { of case }
        END;  { of case Radio }
    END;



PROCEDURE QSOMachineObject.InitializeQSOMachine (KBFile: CINT;
                                                 RadioID: RadioType;
                                                 WinX, WinY: INTEGER);

{ Here we setup the various windows for the QSO Machine and get it all ready to start
  running people.  All of the windows are referenced from WindowLocationX and
  WindowLocationY which need to be set for this specific instance. }

    BEGIN
    { Store the values in this instance }

    KeyboardFileDescriptor := KBFile;
    Radio := RadioID;
    WindowLocationX := WinX;
    WindowLocationY := WinY;

    { Initialize parameters }

    AutoCQDelayTime := 400;     { Four seconds }
    AutoCQMemory := Chr (0);

    AutoStartSendEnable := AutoSendEnable;                  { Adopt global as initial value }
    AutoStartSendCharacterCount := AutoSendCharacterCount;  { Adopt global as initial value }

    AutoStartSendStationCalled := False;
    CallWindowString := '';
    CallWindowCursorPosition := 1;
    Characterinput := Chr(0) ;                               { AltD path to other radio }
    CodeSpeed := SpeedMemory [Radio];
    CWMessageDisplayed := '';
    DisablePutUpBandMapCall := False;
    DisplayedBand := NoBand;
    DisplayedFrequency := 0;
    DisplayedInsertIndicator := NoInsertIndicator;
    DisplayedMode := NoMode;
    DisplayedTXColor := NoTXColor;
    DoingPTTTest := False;
    DualModEMemory := False;
    ExchangeWindowString := '';
    ExchangeWindowCursorPosition := 1;
    ExchangeWindowIsUp := False;
    K3RXPollActive := False;
    LastDisplayedQSONumber := -1;
    LocalInsertMode := InsertMode;                          { Need to get the global Insert Mode here }
    LastFrequency := 0;
    LastPartialCall := '';
    MarkTime (LastFunctionKeyTime);
    LastQSOState := QST_None;
    OkayToPutUpBandMapCall := False;
    QSOState := QST_Idle;
    RadioOnTheMove := False;
    SearchAndPounceExchangeSent := False;
    SearchAndPounceStationCalled := False;
    SSBTransmissionStarted := False;
    TransmitCountDown := 0;

    { Setup the window locations derived from the X,Y reference }

    CASE Radio OF
        RadioOne:
            BEGIN
            { BandMode - save level as Call Window - off to the left }

            TBSIQ_R1_BandModeWindowLX := WindowLocationX;
            TBSIQ_R1_BandModeWindowLY := WindowLocationY + 1;
            TBSIQ_R1_BandModeWindowRX := WindowLocationX + 6;
            TBSIQ_R1_BandModeWindowRY := WindowLocationY + 1;

            TBSIQ_R1_CallWindowLX := WindowLocationX + 13;
            TBSIQ_R1_CallWindowLY := WindowLocationY + 1;
            TBSIQ_R1_CallWindowRX := WindowLocationX + 26;
            TBSIQ_R1_CallWindowRY := WindowLocationY + 1;

            { Code speed - same level as Exchange Window - off to the left }

            TBSIQ_R1_CodeSpeedWindowLX := WindowLocationX;
            TBSIQ_R1_CodeSpeedWindowLY := WindowLocationY + 3;
            TBSIQ_R1_CodeSpeedWindowRX := WindowLocationX + 10;
            TBSIQ_R1_CodeSpeedWindowRY := WindowLocationY + 3;

            { CW Status Window sits just below the Exchange window }

            TBSIQ_R1_CWMessageWindowLX := WindowLocationX + 2;
            TBSIQ_R1_CWMessageWindowLY := WindowLocationY + 4;
            TBSIQ_R1_CWMessageWindowRX := WindowLocationX + 37;
            TBSIQ_R1_CWMessageWindowRY := WindowLocationY + 4;

            TBSIQ_R1_ExchangeWindowLX := WindowLocationX + 13;
            TBSIQ_R1_ExchangeWindowLY := WindowLocationY + 3;
            TBSIQ_R1_ExchangeWindowRX := WindowLocationX + 34;
            TBSIQ_R1_ExchangeWindowRY := WindowLocationY + 3;

            { Just below BandMode }

            TBSIQ_R1_FrequencyWindowLX := WindowLocationX + 1;
            TBSIQ_R1_FrequencyWindowLY := WindowLocationY + 2;
            TBSIQ_R1_FrequencyWindowRX := WindowLocationX + 13;
            TBSIQ_R1_FrequencyWindowRY := WindowLocationY + 2;

            { Just the right of the call window }

            TBSIQ_R1_InsertWindowLX := WindowLocationX + 29;
            TBSIQ_R1_InsertWindowLY := WindowLocationY + 1;
            TBSIQ_R1_InsertWindowRX := WindowLocationX + 36;
            TBSIQ_R1_InsertWindowRY := WindowLocationY + 1;

            TBSIQ_R1_NameWindowLX := WindowLocationX + 15;
            TBSIQ_R1_NameWindowLY := WindowLocationY + 2;
            TBSIQ_R1_NameWindowRX := WindowLocationX + 25;
            TBSIQ_R1_NameWindowRY := WindowLocationY + 2;

            { Just below the exchange window }

            TBSIQ_R1_PossibleCallWindowLX := WindowLocationX;
            TBSIQ_R1_PossibleCallWindowLY := WindowLocationY + 5;
            TBSIQ_R1_PossibleCallWindowRX := WindowLocationX + 38;
            TBSIQ_R1_PossibleCallWindowRY := WindowLocationY + 5;


            TBSIQ_R1_QSONumberWindowLX := WindowLocationX + 8;
            TBSIQ_R1_QSONumberWindowLY := WindowLocationY + 1;
            TBSIQ_R1_QSONumberWindowRX := WindowLocationX + 12;
            TBSIQ_R1_QSONumberWindowRY := WindowLocationY + 1;

            { Just below the exchange window - overlaps possible calls }

            TBSIQ_R1_QuickCommandWindowLX := WindowLocationX;
            TBSIQ_R1_QuickCommandWindowLY := WindowLocationY + 4;
            TBSIQ_R1_QuickCommandWindowRX := WindowLocationX + 39;
            TBSIQ_R1_QuickCommandWindowRY := WindowLocationY + 4;

            TBSIQ_R1_StartSendingWindowLX := WindowLocationX + 12;
            TBSIQ_R1_StartSendingWindowLY := WindowLocationY;
            TBSIQ_R1_StartSendingWindowRX := WindowLocationX + 18;
            TBSIQ_R1_StartSendingWindowRY := windowLocationY;

            { Below CW Message }

            TBSIQ_R1_StateMachineStatusWindowLX := WindowLocationX + 2;
            TBSIQ_R1_StateMachineStatusWindowLY := WindowLocationY + 6;
            TBSIQ_R1_StateMachineStatusWindowRX := WindowLocationX + 37;
            TBSIQ_R1_StateMachineStatusWindowRY := WindowLocationY + 6;

            TBSIQ_R1_TransmitIndicatorWindowLX := WindowLocationX + 0;
            TBSIQ_R1_TransmitIndicatorWindowLY := WindowLocationY + 0;
            TBSIQ_R1_TransmitIndicatorWindowRX := WindowLocationX + 7;
            TBSIQ_R1_TransmitIndicatorWindowRY := WindowLocationY + 0;

            { Between CallWindow and ExchangeWindow }

            TBSIQ_R1_UserInfoWindowLX := WindowLocationX + 29;
            TBSIQ_R1_UserInfoWindowLY := WindowLocationY + 0;
            TBSIQ_R1_UserInfoWindowRX := WindowLocationX + 38;
            TBSIQ_R1_UserInfoWindowRY := WindowLocationY + 0;

            RadioInterfaced := Radio1Type;
            END;

        RadioTwo:
            BEGIN
            { BandMode - save level as Call Window - off to the left }

            TBSIQ_R2_BandModeWindowLX := WindowLocationX;
            TBSIQ_R2_BandModeWindowLY := WindowLocationY + 1;
            TBSIQ_R2_BandModeWindowRX := WindowLocationX + 6;
            TBSIQ_R2_BandModeWindowRY := WindowLocationY + 1;

            TBSIQ_R2_CallWindowLX := WindowLocationX + 13;
            TBSIQ_R2_CallWindowLY := WindowLocationY + 1;
            TBSIQ_R2_CallWindowRX := WindowLocationX + 26;
            TBSIQ_R2_CallWindowRY := WindowLocationY + 1;

            { Code speed - same level as Exchange Window - off to the left }

            TBSIQ_R2_CodeSpeedWindowLX := WindowLocationX;
            TBSIQ_R2_CodeSpeedWindowLY := WindowLocationY + 3;
            TBSIQ_R2_CodeSpeedWindowRX := WindowLocationX + 10;
            TBSIQ_R2_CodeSpeedWindowRY := WindowLocationY + 3;

            { CW Status Window sits just below the Exchange window }

            TBSIQ_R2_CWMessageWindowLX := WindowLocationX + 2;
            TBSIQ_R2_CWMessageWindowLY := WindowLocationY + 4;
            TBSIQ_R2_CWMessageWindowRX := WindowLocationX + 37;
            TBSIQ_R2_CWMessageWindowRY := WindowLocationY + 4;

            TBSIQ_R2_ExchangeWindowLX := WindowLocationX + 13;
            TBSIQ_R2_ExchangeWindowLY := WindowLocationY + 3;
            TBSIQ_R2_ExchangeWindowRX := WindowLocationX + 34;
            TBSIQ_R2_ExchangeWindowRY := WindowLocationY + 3;

            { Just below BandMode }

            TBSIQ_R2_FrequencyWindowLX := WindowLocationX + 1;
            TBSIQ_R2_FrequencyWindowLY := WindowLocationY + 2;
            TBSIQ_R2_FrequencyWindowRX := WindowLocationX + 13;
            TBSIQ_R2_FrequencyWindowRY := WindowLocationY + 2;

            { Just to the right of the call window }

            TBSIQ_R2_InsertWindowLX := WindowLocationX + 29;
            TBSIQ_R2_InsertWindowLY := WindowLocationY + 1;
            TBSIQ_R2_InsertWindowRX := WindowLocationX + 36;
            TBSIQ_R2_InsertWindowRY := WindowLocationY + 1;

            TBSIQ_R2_NameWindowLX := WindowLocationX + 15;
            TBSIQ_R2_NameWindowLY := WindowLocationY + 2;
            TBSIQ_R2_NameWindowRX := WindowLocationX + 25;
            TBSIQ_R2_NameWindowRY := WindowLocationY + 2;

            { Just below the exchange window }

            TBSIQ_R2_PossibleCallWindowLX := WindowLocationX;
            TBSIQ_R2_PossibleCallWindowLY := WindowLocationY + 5;
            TBSIQ_R2_PossibleCallWindowRX := WindowLocationX + 38;
            TBSIQ_R2_PossibleCallWindowRY := WindowLocationY + 5;

            TBSIQ_R2_QSONumberWindowLX := WindowLocationX + 8;
            TBSIQ_R2_QSONumberWindowLY := WindowLocationY + 1;
            TBSIQ_R2_QSONumberWindowRX := WindowLocationX + 12;
            TBSIQ_R2_QSONumberWindowRY := WindowLocationY + 1;

            { Just below the exchange window - overlaps possible calls }

            TBSIQ_R2_QuickCommandWindowLX := WindowLocationX;
            TBSIQ_R2_QuickCommandWindowLY := WindowLocationY + 4;
            TBSIQ_R2_QuickCommandWindowRX := WindowLocationX + 39;
            TBSIQ_R2_QuickCommandWindowRY := WindowLocationY + 4;

            TBSIQ_R2_StartSendingWindowLX := WindowLocationX + 12;
            TBSIQ_R2_StartSendingWindowLY := WindowLocationY;
            TBSIQ_R2_StartSendingWindowRX := WindowLocationX + 18;
            TBSIQ_R2_StartSendingWindowRY := windowLocationY;

            { Below CW Message }

            TBSIQ_R2_StateMachineStatusWindowLX := WindowLocationX + 2;
            TBSIQ_R2_StateMachineStatusWindowLY := WindowLocationY + 6;
            TBSIQ_R2_StateMachineStatusWindowRX := WindowLocationX + 37;
            TBSIQ_R2_StateMachineStatusWindowRY := WindowLocationY + 6;

            TBSIQ_R2_TransmitIndicatorWindowLX := WindowLocationX + 0;
            TBSIQ_R2_TransmitIndicatorWindowLY := WindowLocationY + 0;
            TBSIQ_R2_TransmitIndicatorWindowRX := WindowLocationX + 7;
            TBSIQ_R2_TransmitIndicatorWindowRY := WindowLocationY + 0;

            { Between CallWindow and ExchangeWindow }

            TBSIQ_R2_UserInfoWindowLX := WindowLocationX + 29;
            TBSIQ_R2_UserInfoWindowLY := WindowLocationY + 0;
            TBSIQ_R2_UserInfoWindowRX := WindowLocationX + 38;
            TBSIQ_R2_UserInfoWindowRY := WindowLocationY + 0;

            RadioInterfaced := Radio2Type;
            END;

        END;

    { Now that the windows are defined, we can display stuff }

    DisplayAutoStartSendCharacterCount;   { This doesn't seem to work }
    DisplayCodeSpeed;
    DisplayEditableLog (VisibleLog.LogEntries);
    DisplayInsertMode;
    ShowStateMachineStatus;
    R1_OpMode := CQOpMode;       { Determines color of exchange window }

    { Get the next QSO Number }

    QSONumberForThisQSO := GetNextQSONumber;   { Comes from LOGWIND }

    DisplayQSONumber;
    QSONumberUnused := True;

    { Put up a blank call window }

    SetTBSIQWindow (TBSIQ_CallWindow);
    ClrScr;
    END;



FUNCTION QSOMachineObject.LegalKey (KeyChar: CHAR): BOOLEAN;

{ Adapts to what the TBSIQ_ActiveWindow is }

    BEGIN
    LegalKey := True;

    { Keys that can show up in either window }

    IF (KeyChar >= '0') AND (KeyChar <= '9') THEN Exit;
    IF (KeyChar >= 'A') AND (KeyChar <= 'Z') THEN Exit;
    IF KeyChar = '/' THEN Exit;

    { Legal keys in CallWindow or CWMessageWindow only }

    IF (TBSIQ_ActiveWindow = TBSIQ_CallWindow) OR (TBSIQ_ActiveWindow = TBSIQ_CWMessageWindow) THEN
        BEGIN
        IF KeyChar = '?' THEN Exit;
        IF KeyChar = '.' THEN Exit;
        IF KeyChar = '\' THEN Exit;
        END;

    { Additional legal keys if typing in a CW message  - maybe not used }

    IF TBSIQ_ActiveWindow = TBSIQ_CWMessageWindow THEN
        BEGIN
        IF KeyChar = ',' THEN Exit;
        If KeyChar = '-' THEN Exit;
        IF KeyChar = ' ' THEN Exit;
        END;

    { legal keys in exchange window }

    IF TBSIQ_ActiveWindow = TBSIQ_ExchangeWindow THEN
        BEGIN
        IF KeyChar IN AccentedChars THEN Exit;
        END;

    LegalKey := False;
    END;



PROCEDURE QSOMachineObject.RemovePossibleCallWindow;

    BEGIN
    LastPossibleCall := '';

    CASE Radio OF
        RadioOne: RemoveWindow (TBSIQ_R1_PossibleCallWindow);
        RadioTwo: RemoveWindow (TBSIQ_R2_PossibleCallWindow);
        END;
    END;



PROCEDURE QSOMachineObject.DisplayPossibleCalls (VAR List: CallListRecord);

{ Specific for TBSIQ - just uses half the screen }

    BEGIN
    CASE Radio OF
        RadioOne: SaveSetAndClearActiveWindow (TBSIQ_R1_PossibleCallWindow);
        RadioTwo: SaveSetAndClearActiveWindow (TBSIQ_R2_PossibleCallWindow);
        END;

    DisplayPossibleCallsInActiveWindow (List);
    END;



PROCEDURE QSOMachineObject.DisplayTXColor;

{ Displays the TX indicator if it is different than what is already
  displayed }

VAR Color: TXColorType;

    BEGIN
    { Note that the following code will have a funny result if you are sending
      CW by hand.  While the CW is actually being generated - the TX indicator
      will be red.  If you just send a dit - it will revert to blue really
      fast - all before the radio has had time to come back with TX status.
      So it will go blue again until the next poll when the radio will say
      it is done transmitting.  Not sure this matters, but it can be fixed
      by not looking at IAmTransmitting when in CW }

    CASE Mode OF
        CW: BEGIN
            Color := TBSIQ_CW_Engine.GetTransmitColor (Radio);

            IF Color = TX_Blue THEN
                IF DualModeMemory THEN
                    Color := TX_Yellow;
            END;

        Phone, Digital:
            IF IAmTransmitting THEN
                Color := TX_Red
            ELSE
                IF DualModeMemory THEN
                    Color := TX_Yellow
                ELSE
                    Color := TX_Blue;
        END;

    IF DisplayedTXColor <> Color THEN
        BEGIN
        CASE Radio OF
            RadioOne: SaveAndSetActiveWindow (TBSIQ_R1_TransmitIndicatorWindow);
            RadioTwo: SaveAndSetActiveWindow (TBSIQ_R2_TransmitIndicatorWindow);
            END;

        CASE Color OF
            TX_Red:    SetBackground (Red);
            TX_Yellow: SetBackground (Yellow);
            TX_Blue:   SetBackground (Blue);
            END;

        ClrScr;
        RestorePreviousWindow;
        DisplayedTXColor := Color;
        END;
    END;



PROCEDURE QSOMachineObject.DisplayActiveRadio;

    BEGIN
    TBSIQ_CW_Engine.ShowActiveRadio;
    END;


PROCEDURE QSOMachineObject.DisplayInsertMode;

    BEGIN
    IF (DisplayedInsertIndicator = NoInsertIndicator) OR
       ((DisplayedInsertIndicator = InsertOffIndicator) AND LocalInsertMode) OR
       ((DisplayedInsertIndicator = InsertOnIndicator) AND NOT LocalInsertMode) THEN
           BEGIN
           CASE Radio OF
               RadioOne: SaveSetAndClearActiveWindow (TBSIQ_R1_InsertWindow);
               RadioTwo: SaveSetAndClearActiveWindow (TBSIQ_R2_InsertWindow);
               END;

           IF LocalInsertMode THEN
               BEGIN
               Write (' INSERT');
               RestorePreviousWindow;
               DisplayedInsertIndicator := InsertOnIndicator;
               END
           ELSE
               BEGIN
               RemoveAndRestorePreviousWindow;
               DisplayedInsertIndicator := InsertOffIndicator;
               END;
           END;
    END;



PROCEDURE QSOMachineObject.DisplayCodeSpeed;

    BEGIN
    CASE Radio OF
        RadioOne: SaveSetAndClearActiveWindow (TBSIQ_R1_CodeSpeedWindow);
        RadioTwo: SaveSetAndClearActiveWindow (TBSIQ_R2_CodeSpeedWindow);
        END;

    IF Mode = CW THEN
        Write (' ', CodeSpeed:2, ' WPM')
    ELSE
        Write ('       ');

    { This is coming up at the start of the 2BSIQ.  Not sure yet how to
      differentiate ActiveRadio and "CW" }

    IF ActiveRadio = Radio THEN
        Write (' TX');

    RestorePreviousWindow;
    END;



PROCEDURE PacketWindow;

VAR Key: CHAR;
    TimeMark: TimeRecord;

    BEGIN
    PacketWindowUp := True;
    RITEnable := False;
    QuickDisplay ('You are now talking to your packet port.  Use Control-B to exit.');

    SaveSetAndClearActiveWindow (DupeSheetWindow);
    ClrScr;
    Packet.DisplayPacketDisplayBuffer;

    { Show chars from incomplete line that isn't in the PacketDisplayBuffer }

    Write (Packet.PacketDisplayLine);

    MarkTime (TimeMark);

    REPEAT
        IF NewKeyPressed THEN
            BEGIN
            MarkTime (TimeMark);

            Key := NewReadKey;

            IF Key = ControlB THEN
                BEGIN
                RestorePreviousWindow;
                RemoveWindow (BigWindow);
                VisibleLog.SetUpEditableLog;
                UpdateTotals;
                VisibleLog.ShowRemainingMultipliers;
                VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);

                IF VisibleDupeSheetEnable THEN
                    BEGIN
                    VisibleDupeSheetChanged := True;
                    VisibleLog.DisplayVisibleDupeSheet (ActiveBand, ActiveMode);
                    END;

                RemoveWindow (QuickCommandWindow);
                PacketWindowUp := False;
                RITEnable := True;
                Exit;
                END;

            { I am not going to support any commands like we do in the classic version }

            IF Key = NullKey THEN
                BEGIN
                Key := NewReadKey;

                CASE Key OF

//These send the arrow key combinations that
//should give the command history for a linux shell

                  UpArrow:  BEGIN
                    SendChar (ActivePacketPort,chr($1b));
                    SendChar (ActivePacketPort,chr($5b));
                    SendChar (ActivePacketPort,chr($41));
                    END;

                  DownArrow: BEGIN
                    SendChar (ActivePacketPort,chr($1b));
                    SendChar (ActivePacketPort,chr($5b));
                    SendChar (ActivePacketPort,chr($42));
                    END;

                  LeftArrow: BEGIN
                    SendChar (ActivePacketPort,chr($1b));
                    SendChar (ActivePacketPort,chr($5b));
                    SendChar (ActivePacketPort,chr($44));
                    END;

                  RightArrow: BEGIN
                    SendChar (ActivePacketPort,chr($1b));
                    SendChar (ActivePacketPort,chr($5b));
                    SendChar (ActivePacketPort,chr($43));
                    END;
                  END;  { of CASE ExtendedKey }
                END

            ELSE       { not null key  }
                SendChar (ActivePacketPort, Key);
            END;

        Packet.CheckPacketBuffer (True);
        Packet.CheckPacketMessage;

        Wait (4);
    UNTIL ElaspedSec100 (TimeMark) > 2000;   { Timeout }

    RestorePreviousWindow;

    RemoveWindow (BigWindow);
    VisibleLog.SetUpEditableLog;
    UpdateTotals;
    VisibleLog.ShowRemainingMultipliers;
    VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);

    IF VisibleDupeSheetEnable THEN
        BEGIN
        VisibleDupeSheetChanged := True;
        VisibleLog.DisplayVisibleDupeSheet (ActiveBand, ActiveMode);
        END;

    RemoveWindow (QuickCommandWindow);
    PacketWindowUp := False;
    RITEnable := True;
    END;



PROCEDURE QSOMachineObject.WindowEditor (VAR WindowString: Str80;
                                         VAR KeyChar: CHAR;
                                         VAR ExtendedKeyChar: CHAR;
                                         VAR ActionRequired: BOOLEAN);

{ 99.44 percent of this code is copied from the LOGSUBS2.PAS window editor.  It has
  been modified to work in the 2BSIQ environment.  Then I removed the stuff I didn't
  think would be used in the 2BSIQ case.

  For the 2BSIQ case, this editor needs to be a bit smarter and realize that the
  other keyboard will need to have its window editor be able to process a keystroke.
  Because of that, this routine must not end up in a REPEAT UNTIL KeyPressed state.
  Once it has finished its business, it needs to exit.

  If there is an action for the calling routine to be done based upon some keystroke
  that is being returned, the keystroke will be in the KeyChar and ExtendedKeyChar
  parameters.  If these are both null, then there is no action for the calling
  routine to take.

  See the notes in LOGSUBS2 for "the rest of the story"

  During initial testing, we discovered that you can not assume that
  TBSIQ_ActiveWindow is actually your active window when writing chars
  to the screen.  If it possible the other instance of the QSOMachine
  changed it to a different window...  so you should check ActiveWindow
  first and change it to the right window before doing anything.

  Something new - this needs to deal with the OkayToPutUpBandMapCall by
  turning it off if any key is pressed }

VAR QSOCount, CursorPosition, CharPointer, Count: INTEGER;
    Key, PreviousCursorChar: CHAR;
    Message, TempString: STRING;
    TempExchange: ContestExchange;
    PacketSpotCall: CallString;

    BEGIN
    BeSilent := False;   { Set this TRUE when exiting if you don't want CW sent }
    CheckForRemovedDupeSheetWindow;

    ActionRequired := False;
    ExtendedKeyChar := Chr (0);   { Default values if we exit early }
    KeyChar := Chr (0);

    { If someone set the ClearKeyCache flag - we should gobble up any characters
      that are waiting for me and do nothing with them }

    IF ClearKeyCache THEN
        BEGIN
        Count := 30;

        REPEAT
            Dec (Count);
            Millisleep;

            IF TBSIQ_KeyPressed (Radio) THEN
                BEGIN
                TBSIQ_ReadKey (Radio);
                Count := 30;
                END;

        UNTIL Count = 0;

        ClearKeyCache := False;
        ResetKeyStatus (Radio);
        Exit;
        END;

    { We thought about looking at the footswitch here - but really, any footswitch
      operations that require action need to be focused on the active radio and
      since we can't tell if we are the active radio down this low, we just can't
      deal with anything. }

    { However, if we are doing dual mode - we will want to use the footswitch
      in an interlocked way for the SSB radio }

    IF NOT ((TBSIQ_KeyPressed (Radio)) OR (CharacterInput <> Chr (0))) THEN
        Exit;  { No reason to be here }

    { A keystroke will stop the DualingCQ activity.  Note that you are likely
      on the wrong radio with the CW or for sure SSB }

    IF (DualingCQState <> NoDualingCQs) AND (Mode = Phone) THEN  { Make me the TX radio }
        BEGIN
        { Try and stop the message on the other radio? }

        CASE Radio OF
            RadioOne: rig2.directcommand ('RX;');
            RadioTwo: rig1.directcommand ('RX;');
            END;

        { For now - I am not sure I want to do this with DualMode mode }

        IF NOT TBSIQDualMode THEN
            BEGIN
            ActiveRadio := Radio;
            ActiveKeyer.SetActiveRadio (Radio);   { This seems necessary }
            TBSIQ_CW_Engine.ShowActiveRadio;
            END;
        END;

    DualingCQState := NoDualingCQs;

    { A keystroke will clear the OkayToPutUpBandMapCall flag  and also disable
      the feature until we tune to a new frequency }

    OkayToPutUpBandMapCall := False;
    DisablePutUpBandMapCall := True;

    { Make sure proper window is active - also set up the window strings
      and cursor positions }

    SetTBSIQWindow (TBSIQ_ActiveWindow);  { Sets cursor }

    CASE TBSIQ_ActiveWindow OF
        TBSIQ_CallWindow:
            BEGIN
            WindowString := CallWindowString;
            CursorPosition := CallWindowCursorPosition;
            END;

        TBSIQ_ExchangeWindow:
            BEGIN
            WindowString := ExchangeWindowString;
            CursorPosition := ExchangeWindowCursorPosition;
            END;
        END;

    { See what key was pressed }

    KeyChar := UpCase (TBSIQ_ReadKey (Radio));

    { Default conditions if we exit soon }

    ActionRequired := True;
    ExtendedKeyChar := Chr (0);

    { Special keystrokes }

    IF (QSOState = QST_StartSendingKeyboardCW) OR (QSOState = QST_SendingKeyboardCW) THEN
        Exit;  { All keystrokes handled there }

    IF KeyChar <> NullKey THEN
        IF (QSOState = QST_AutoCQListening) OR (QSOState = QST_AutoCQCalling) THEN
            BEGIN
            QSOState := QST_CQCalled;
            ShowCWMessage ('AutoCQ aborted');
            END;

    { Check for keys that are variables and require action by caller }

    IF KeyChar = QuickQSLKey1 THEN Exit;
    IF KeyChar = QuickQSLKey2 THEN Exit;
    IF KeyChar = TailEndKey THEN Exit;

    { Deal with packet spot key }

    IF KeyChar = PacketSpotKey THEN
        BEGIN
        IF ActivePacketPort <> nil THEN
            IF NOT PacketSpotDisable THEN
                BEGIN
                IF ActiveWindow = CallWindow THEN
                    PacketSpotCall := WindowString
                ELSE
                    PacketSpotCall := CallWindowString;

                IF PacketSpotCall <> '' THEN
                    CreateAndSendPacketSpot (PacketSpotCall, Frequency);
                END;

        ActionRequired := False;
        Exit;
        END;

    IF KeyChar = PossibleCallRightKey THEN
        BEGIN
        WITH TBSIQPossibleCallList DO
            IF (NumberCalls > 0) AND (CursorPosition < NumberCalls - 1) THEN
                BEGIN
                Inc (CursorPosition);
                DisplayPossibleCalls (TBSIQPossibleCallList);
                END;
        Exit;
        END;

    IF KeyChar = PossibleCallLeftKey THEN
        BEGIN
        WITH TBSIQPossibleCallList DO
            IF (NumberCalls > 0) AND (CursorPosition > 0) THEN
                BEGIN
                Dec (CursorPosition);
                DisplayPossibleCalls (TBSIQPossibleCallList);
                END;
        Exit;
        END;

    IF KeyChar = PossibleCallAcceptKey THEN
        BEGIN
        WITH TBSIQPossibleCallList DO
            IF NumberCalls > 0 THEN
                BEGIN
                WITH TBSIQPossibleCallList DO
                    CallWindowString := CallList [CursorPosition].Call;

                CallWindowCursorPosition := Length (CallWindowString) + 1;

                IF TBSIQ_ActiveWindow = TBSIQ_CallWindow THEN
                    BEGIN
                    ClrScr;
                    Write (WindowString);
                    END
                ELSE
                    BEGIN
                    SwapWindows;
                    ClrScr;
                    Write (CallWindowString);
                    SwapWindows;
                    END;
            END;
        Exit;
        END;

    { After the CASE statement - whatever WindowString is will be saved in either
      CallWindowString or ExchangeWindowString and also the cursor position in
      CallCursorPosition or ExchangeCursorPosition. }

    CASE KeyChar OF

        ControlDash:
            IF GetCQMemoryString (Mode, AltF1) <> '' THEN
                BEGIN
                IF DualingCQState = NoDualingCQs THEN
                    BEGIN
                    SendFunctionKeyMessage (AltF1, Message);

                    IF Mode <> CW THEN
                        TransmitCountDown := InitialTransmitCountdown;

                    CASE Radio OF
                        RadioOne: DualingCQState := DualingCQOnRadioOne;
                        RadioTwo: DualingCQState := DualingCQOnRadioTwo;
                        END;  { of CASE }
                    END
                ELSE
                    DualingCQState := NoDualingCQs;
                END;

        EscapeKey:
            BEGIN

            { The ESCAPE KEY is a panic key that will step things back one step for every
              press.  The first thing that needs to be checked is to see if there is a CW
              message being sent - or one in the cue - that needs to be aborted/deleted.  }

            IF Mode = CW THEN
                IF TBSIQ_CW_Engine.ClearMessages (Radio, True) THEN   { was something to stop }
                    BEGIN
                    { If you have triggered AutoStartSend - we need to tell someone that
                      the ESCAPE KEY was pressed so that it doesn't move on to sending the
                      CQ Exchange }

                    ActionRequired := QSOState = QST_AutoStartSending;
                    Exit;
                    END;

           { If there is no CW to be stopped / deleted, then the next thing the ESCAPE key will
              do is erase whatever data is in the current window }

            IF WindowString <> '' THEN
                BEGIN
                IF TBSIQ_ActiveWindow = TBSIQ_CallWindow THEN
                    BEGIN
                    EscapeDeletedCallEntry := WindowString;
                    NameCallsignPutUp := '';
                    END
                ELSE
                    EscapeDeletedExchangeEntry := WindowString;

                ClrScr;
                WindowString := '';
                CursorPosition := 1;
                END
            ELSE
                { Nothing for WindowEditor to do - let the caller deal with it }
                { We have ActionRequired and KeyChar = EscapeKey }

                Exit;

            END;  { of Escape Key }

        ControlA:      { Move back to start of previous word }
            IF CursorPosition > 1 THEN
                BEGIN
                REPEAT
                    Dec (CursorPosition);
                    PreviousCursorChar := WindowString [CursorPosition - 1];
                UNTIL ((WindowString [CursorPosition] <> ' ') AND
                       (PreviousCursorChar = ' ')) OR (CursorPosition = 1);

                GoToXY (CursorPosition, WhereY);
                END;


        ControlB:
            IF ActivePacketPort <> nil THEN
                BEGIN
                PacketWindow;
                ClearKeyCache := True;
                END;

        ControlC: Exit;   { Vertical move takes us out of this window }

        ControlD:       { Move forward one character }
            IF CursorPosition <= Length (WindowString) THEN
                BEGIN
                Inc (CursorPosition);
                GoToXY (CursorPosition, WhereY);
                END;


        ControlF:  { Move forward to start of next word }
            IF CursorPosition <= Length (WindowString) THEN
                BEGIN
                REPEAT
                    PreviousCursorChar := WindowString [CursorPosition];
                    Inc (CursorPosition);
                UNTIL ((WindowString [CursorPosition] <> ' ') AND
                       (PreviousCursorChar = ' ')) OR (CursorPosition = Length (WindowString) + 1);
                GoToXY (CursorPosition, WhereY);
                END;

        { Not clear to me at the momeent if these next few should move out of here or not }

        ControlJ:
            BEGIN
            ProcessConfigurationInput;
            RemoveWindow (QuickCommandWindow);
            ClearKeyCache := True;
            END;

        ControlL:
            BEGIN
            ViewLogFile;
            VisibleDupeSheetRemoved := True;
            ClearKeyCache := True;
            END;

        ControlN:
            BEGIN
            RITEnable := False;

            AddedNoteString := QuickEditResponse ('Enter note : ', 60);

            IF (AddedNoteString <> '') AND (AddedNoteString <> EscapeKey) THEN
                BEGIN
                IF ComputerID = NullKey THEN
                    AddedNoteString := '; ' + GetDateString + ' ' +
                                              GetTimeString + ' : ' +
                                              AddedNoteString
                ELSE
                    AddedNoteString := '; ' + GetDateString + ' ' +
                                              GetTimeString + ' ' +
                                              ComputerID + ': ' +
                                              AddedNoteString;

                WHILE Length (AddedNoteString) > 79 DO
                    Delete (AddedNoteString, Length (AddedNoteString), 1);

                TBSIQ_PushLogStringIntoEditableLogAndLogPopedQSO (AddedNoteString, True);
                END;

            RITEnable := True;
            ClearKeyCache := True;
            END;

        ControlO:
            VisibleLog.ShowMissingMultiplierReport;

        ControlP:
            BEGIN
            IF TBSIQ_ActiveWindow = TBSIQ_CallWindow THEN
                BEGIN
                ShowStationInformation (WindowString);
                DisplayGridSquareStatus (WindowString);
                DoPossibleCalls (WindowString);
                END
            ELSE
                BEGIN
                ShowStationInformation (CallWindowString);
                DisplayGridSquareStatus (CallWindowString);
                DoPossibleCalls (CallWindowString);
                END;
            END;


        ControlR:   { Restore previously deleted string }
            BEGIN
            CASE TBSIQ_ActiveWindow OF
                TBSIQ_CallWindow:
                    IF EscapeDeletedCallEntry <> '' THEN
                        BEGIN
                        WindowString := EscapeDeletedCallEntry;
                        ClrScr;
                        Write (WindowString);
                        END;

                TBSIQ_ExchangeWindow:
                    IF EscapeDeletedExchangeEntry <> '' THEN
                        BEGIN
                        WindowString := EscapeDeletedExchangeEntry;
                        ClrScr;
                        Write (WindowString);
                        END;

                END;  { of case }
            END;

        ControlS:   { Back character }
            IF CursorPosition > 1 THEN
                BEGIN
                Dec (CursorPosition);
                GoToXY (CursorPosition, WhereY);
                END;

        ControlT:  { Fawncy delete to start of next word or truncate }
            IF CursorPosition <= Length (WindowString) THEN
                BEGIN
                TempString := WindowString;
                TempString [0] := Chr (CursorPosition - 1);
                CharPointer := CursorPosition;
                REPEAT
                    Inc (CharPointer);
                UNTIL (CharPointer = Length (WindowString)) OR
                      (WindowString [CharPointer] = ' ');

                IF CharPointer = Length (WindowString) THEN
                    BEGIN
                    ClrEOL;
                    WindowString [0] := Chr (CursorPosition - 1);
                    END
                ELSE
                    BEGIN
                    Inc (CharPointer);
                    FOR CharPointer := CharPointer TO Length (WindowString) DO
                        TempString := TempString + WindowString [CharPointer];
                    ClrScr;
                    Write (TempString);
                    WindowString := TempString;
                    GoToXY (CursorPosition, WhereY);
                    END;
                END
            ELSE
                IF CursorPosition = Length (WindowString) THEN
                    BEGIN
                    ClrEol;
                    WindowString [0] := Chr (Length (WindowString) - 1);
                    END;

        ControlW:   { The old K1EA wipe command }
            BEGIN
            IF TBSIQ_ActiveWindow = TBSIQ_CallWindow THEN
                EscapeDeletedCallEntry := WindowString
            ELSE
                EscapeDeletedExchangeEntry := WindowString;

            WindowString := '';
            ClrScr;
            NameCallsignPutUp := '';
            END;

        ControlX: Exit;  { Vertical move out of this window }

        ControlY:
            UpdateBlinkingBandMapCall;

        ControlZ: Exit;

        { Maybe add Shift-Enter here - but it needs to get defined in KeyCode.PAS }

        ControlBackSlash:
            BEGIN
            BeSilent := True;
            KeyChar := CarriageReturn;
            Exit;
            END;

        ControlRightBracket:
            DisplayCT1BOHData;

        CarriageReturn:
            BEGIN
            { There is a bunch of stuff that Carriage Return can do like sending your radio to a new frequency,
              but for now - I am going to ignore all of that and just return a CarriageReturn to the calling
              routine. }

            Exit;
            END;

        TabKey: Exit;  { Mostly used to enter Search And Pounce?  }

        BackSpace:
            BEGIN
            IF InitialExchangeOverwrite AND InitialExchangePutUp THEN
                InitialExchangePutUp := False;  { Cancel it - we are going to edit it }

            OkayToPutUpBandMapCall := False;

            { Backspace needs to be AutoStartSend aware }

            IF QSOState = QST_AutoStartSending THEN Exit;

            IF CursorPosition > 1 THEN
                BEGIN
                FOR CharPointer := CursorPosition - 1 TO Length (WindowString) - 1 DO
                    WindowString [CharPointer] := WindowString [CharPointer + 1];

                WindowString [0] := Chr (Length (WindowString) - 1);
                ClrScr;
                Write (WindowString);
                Dec (CursorPosition);
                GoToXY (CursorPosition, WhereY);
                END;
            END;

        SpaceBar:
            IF TBSIQ_ActiveWindow = TBSIQ_ExchangeWindow THEN
                BEGIN
                { Very special case:  We have a virgin initial exchange posted in the exchange
                  window and someone has typed a key while in the exchange window and the
                  global InitialExchangeOverwrite is set.  We need to erase the initial
                  exchange - WAIT - what if it was like a carriage return!!??!! }

                IF InitialExchangeOverwrite AND InitialExchangePutUp THEN
                    BEGIN
                    IF (TBSIQ_ActiveWindow = TBSIQ_ExchangeWindow) THEN
                        BEGIN
                        ClrScr;
                        WindowString := '';
                        ExchangeWindowString := '';
                        CursorPosition := 1;
                        InitialExchangePutUp := False;
                        END;
                    END;

                IF LocalInsertMode AND (CursorPosition <= Length (WindowString)) THEN  { Squeeze in new character }
                    BEGIN
                    IF CursorPosition > 1 THEN
                        BEGIN
                        TempString := Copy (WindowString, 1, CursorPosition - 1) +
                                            KeyChar +
                                            Copy (WindowString, CursorPosition, Length (WindowString) - CursorPosition + 1);
                        END
                    ELSE
                        TempString := KeyChar + WindowString;

                    WindowString := TempString;
                    ClrScr;
                    Write (WindowString);
                    Inc (CursorPosition);
                    GoToXY (CursorPosition, 1);
                    END
                ELSE
                    IF CursorPosition <= Length (WindowString) THEN
                        BEGIN
                        WindowString [CursorPosition] := KeyChar;
                        Write (KeyChar);
                        Inc (CursorPosition);
                        END
                    ELSE
                        BEGIN
                        WindowString := WindowString + KeyChar;
                        Write (KeyChar);
                        Inc (CursorPosition);
                        END;
                END
            ELSE
                { Call Window active }

                BEGIN
                IF CallWindowString = '' THEN Exit;  { Let calling procedure deal with it }

                IF WindowDupecheck THEN
                    BEGIN
                    WindowString := '';
                    CursorPosition := 1;
                    END;
                END;

        NullKey:
            BEGIN
            ExtendedKeyChar := TBSIQ_ReadKey (Radio);

            { Many Altkeys that hardly ever get used are currently not here. See the
              WindowEditor in LOGSUBS2.PAS if you want to put some of them back in }

            CASE ExtendedKeyChar OF

                  AltC:
                      BEGIN
                      ActionRequired := False;

                      IF ValidFunctionKey (AutoCQMemory) THEN
                          BEGIN
                          SendFunctionKeyMessage (AutoCQMemory, Message);
                          ShowCWMessage ('AutoCQ: ' + Message);
                          QSOState := QST_AutoCQCalling;
                          ListenToOtherRadio;
                          END;
                      END;

{                 AltD:
                      BEGIN
                      RememberQSOState := QSOState;
                      QSOState := QST_AltDInput;
                      ActionRequired := False;
                      END;
}

                  AltE: BEGIN
                      RITEnable := False;
                      VisibleLog.EditLog;

                      { We have things to clear up }

                      PaintVerticalLine;
                      RITEnable := True;
                      UpdateTotals;
                      VisibleLog.ShowRemainingMultipliers;
                      VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);
                      DisplayTotalScore (TotalScore);

                      IF VisibleDupeSheetEnable THEN
                          BEGIN
                          VisibleDupeSheetChanged := True;
                          VisibleLog.DisplayVisibleDupeSheet (ActiveBand, ActiveMode);
                          END;

                      VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);
                      ClearKeyCache := True;
                      END;

                AltG: SwapMultDisplay;

                AltH: BEGIN
                      PutUpHelpMenu (False);   { No update of classic time/rate }
                      RestorePreviousWindow;
                      VisibleDupeSheetRemoved := True;
                      ClearKeyCache := True;
                      END;

                AltI:
                    IF TBSIQ_ActiveWindow = TBSIQ_ExchangeWindow THEN
                        BEGIN
                        IncrementASCIIInteger (ExchangeWindowString);
                        ClrScr;
                        Write (ExchangeWindowString);
                        CursorPosition := WhereX;
                        END;

                AltK: ToggleCW (True);

                AltL: BEGIN
                      RITEnable := False;
                      IF TBSIQ_Activewindow = TBSIQ_CallWindow THEN
                          VisibleLog.SearchLog (WindowString)
                      ELSE
                          VisibleLog.SearchLog (CallWindowString);

                      RITEnable := True;
                      ClearKeyCache := True;
                      END;

                  AltP:
                      BEGIN
                      RITEnable := False;
                      ActiveMode := Mode;
                      MemoryProgram;
                      RITEnable := True;
                      VisibleLog.SetUpEditableLog;
                      ClearKeyCache := True;
                      ActionRequired := False;
                      END;

                  AltQ:
                      BEGIN
                      ActionRequired := False;

                      ShowCWMessage ('Press function key memory to repeat');

                      REPEAT
                      UNTIL TBSIQ_KeyPressed (Radio);
                      Key := TBSIQ_ReadKey (Radio);

                      IF Key = NullKey THEN
                          BEGIN
                          { Get function key }

                          Key := TBSIQ_ReadKey (Radio);

                          IF ValidFunctionKey (Key) THEN
                              BEGIN
                              AutoCQMemory := Key;
                              SendFunctionKeyMessage (AutoCQMemory, Message);
                              ShowCWMessage ('AutoCQ: ' + Message);
                              QSOState := QST_AutoCQCalling;
                              ListenToOtherRadio;
                              END;
                          END;
                      END;

                  AltR:
                      BEGIN
                      IF ActiveRadio = RadioOne THEN
                          ActiveRadio := RadioTwo
                      ELSE
                          ActiveRadio := RadioOne;

                      SetUpToSendOnActiveRadio;
                      TBSIQ_CW_Engine.ShowActiveRadio;
                      END;

                  AltS:
                      BEGIN
                      CWEnabled := True;
                      CodeSpeed := QuickEditInteger ('Enter WPM code speed : ', 2);
                      SetCodeSpeed (CodeSpeed);
                      ClearKeyCache := True;
                      END;

                AltT: BEGIN
                      TimeAndDateSet;
                      ClearKeyCache := True;
                      END;

                AltU: BEGIN
                      FOR QSOCount := 1 TO NumberEditableLines DO
                          BEGIN
                          TempString := '';
                          TBSIQ_PushLogStringIntoEditableLogAndLogPopedQSO (TempString, True);
                          END;

                      DeleteFile (LogTempFileName);
                      IF UpdateRestartFileEnable THEN Sheet.SaveRestartFile;
                      UpdateTotals;
                      END;

                AltX: BEGIN
                      TBSIQ_ExitProgram;
                      ClearKeyCache := True;
                      END;

                AltY: TBSIQ_DeleteLastContact;

                AltZ:
                    BEGIN  { Well - let's see if this works okay down here }

                    { We might be editing the WindowString with the CallWindow active }

                    IF TBSIQ_ActiveWindow = TBSIQ_CallWindow THEN
                        BEGIN
                        CallWindowString := WindowString;
                        CallWindowCursorPosition := CursorPosition;

                        { Go ahead and set the exchange window up }

                        SetTBSIQWindow (TBSIQ_ExchangeWindow);
                        END;

                    WindowString := InitialExchangeEntry (CallWindowString);

                    { So whether or not we got an initial exchange we are going to
                      want to end up in the exchange window }

                    ClrScr;
                    Write (WindowString);
                    CursorPosition := Length (WindowString) + 1;
                    ShowStationInformation (CallWindowString);
                    END;  { of AltZ }

                AltDash:
                    BEGIN
                    IF AutoStartSendCharacterCount > 0 THEN
                        AutoStartSendEnable := NOT AutoStartSendEnable;

                    DisplayAutoStartSendCharacterCount;
                    END;

                HomeKey:
                    BEGIN
                    CursorPosition := 1;
                    GoToXY (CursorPosition, WhereY);
                    END;

                EndKey:
                    BEGIN
                    CursorPosition := Length (WindowString) + 1;
                    GoToXY (CursorPosition, 1);
                    END;

                DeleteKey:
                    BEGIN
                    IF LocalInsertMode THEN
                        BEGIN
                        IF CursorPosition <= Length (WindowString) THEN
                            BEGIN
                            FOR CharPointer := CursorPosition TO Length (WindowString) - 1 DO
                                WindowString [CharPointer] := WindowString [CharPointer + 1];

                            WindowString [0] := Chr (Length (WindowString) - 1);
                            ClrScr;
                            Write (WindowString);
                            GoToXY (CursorPosition, WhereY);
                            END
                        ELSE
                            IF CursorPosition = Length (WindowString) THEN
                                BEGIN
                                ClrEol;
                                WindowString [0] := Chr (Length (WindowString) - 1);
                                END;
                        END
                    ELSE
                        BEGIN
                        WindowString [CursorPosition] := ' ';
                        ClrScr;
                        Write (WindowString);
                        GoToXY (CursorPosition, WhereY);
                        END;

{                   ShowPartialCallMults (WindowString); }

                    END;

                LeftArrow:
                    IF CursorPosition > 1 THEN
                        BEGIN
                        Dec (CursorPosition);
                        GoToXY (CursorPosition, 1);
                        END;

                RightArrow:
                    IF CursorPosition <= Length (WindowString) THEN
                        BEGIN
                        Inc (CursorPosition);
                        GoToXY (CursorPosition, 1);
                        END;

                PageUpKey:
                    IF QSOState = QST_AutoCQListening THEN
                        BEGIN
                        AutoCQDelayTime := AutoCQDelayTime + 50;
                        Str (AutoCQDelayTime DIV 100, TempString);

                        IF AutoCQDelayTime MOD 100 <> 0 THEN
                            TempString := TempString + '.5'
                        ELSE
                            TempString := TempString + '.0';

                        ShowCWMessage ('Listen time = ' + TempString + ' secs (PgUp/Dn)');
                        END
                    ELSE
                        IF CodeSpeed < 99 - CodeSpeedIncrement THEN
                            BEGIN
                            CodeSpeed := CodeSpeed + CodeSpeedIncrement;
                            SetCodeSpeed (CodeSpeed);
                            END;

                PageDownKey:
                    IF QSOState = QST_AutoCQListening THEN
                        BEGIN
                        IF AutoCQDelayTime > 100 THEN
                            BEGIN
                            AutoCQDelayTime := AutoCQDelayTime - 50;
                            Str (AutoCQDelayTime DIV 100, TempString);

                            IF AutoCQDelayTime MOD 100 <> 0 THEN
                                TempString := TempString + '.5'
                            ELSE
                                TempString := TempString + '.0';

                            ShowCWMessage ('Listen time = ' + TempString + ' secs (PgUp/Dn)');
                            END;
                        END
                    ELSE
                        IF CodeSpeed > 1 + CodeSpeedIncrement THEN
                            BEGIN
                            CodeSpeed := CodeSpeed - CodeSpeedIncrement;
                            SetCodeSpeed (CodeSpeed);
                            END;

                { It appears we used ControlUpArrow to MoveGridMap }

                UpArrow:  { Edit log - same as Alt-E? - not sure this should stay here  }
                    IF (TBSIQ_Activewindow = TBSIQ_CallWindow) AND (WindowString = '') THEN
                        BEGIN
                        RITEnable := False;
                        VisibleLog.EditLog;
                        RITEnable := True;
                        UpdateTotals;
                        VisibleLog.ShowRemainingMultipliers;
                        VisibleLog.DisplayGridMap (Band, Mode);
  {                     DisplayTotalScore (TotalScore); }
                        DisplayInsertMode;
                        ClearKeyCache := True;
                        END
                    ELSE
                        Exit;     { Let the calling routine decide what to do }

                { It appears ControlDownArrow was used to move the Grid Map down }

                DownArrow: Exit;

                InsertKey:
                    BEGIN
                    LocalInsertMode := NOT LocalInsertMode;
                    DisplayInsertMode;
                    END;

                { Any other ExtendedKeys not captured here will be dealt with by the calling Procedre }

                ELSE Exit;

                END;  { of case }
            END; { of ExtendedKey }

        ELSE  { Likely we hae a "normal" key that just needs to be added to the window }

            IF LegalKey (KeyChar) THEN
                BEGIN
                { Very special case:  We have a virgin initial exchange posted in the exchange
                  window and someone has typed a key while in the exchange window and the
                  global InitialExchangeOverwrite is set.  We need to erase the initial
                  exchange - WAIT - what if it was like a carriage return!!??!! }

                IF InitialExchangeOverwrite AND InitialExchangePutUp THEN
                    BEGIN
                    IF (TBSIQ_ActiveWindow = TBSIQ_ExchangeWindow) THEN
                        BEGIN
                        ClrScr;
                        WindowString := '';
                        ExchangeWindowString := '';
                        CursorPosition := 1;
                        InitialExchangePutUp := False;
                        END;
                    END;

                { Don't add characters to a band map callsign }

                IF TBSIQ_ActiveWindow = TBSIQ_CallWindow THEN
                    IF BandMapCallPutUp <> '' THEN
                        BEGIN
                        ClrScr;
                        WindowString := '';
                        CursorPosition := 1;
                        BandMapCallPutUp := '';
                        OkayToPutUpBandMapCall := False;
                        END;

                IF LocalInsertMode AND (CursorPosition <= Length (WindowString)) THEN  { Squeeze in new character }
                    BEGIN
                    IF CursorPosition > 1 THEN
                        BEGIN
                        TempString := Copy (WindowString, 1, CursorPosition - 1) +
                                            KeyChar +
                                            Copy (WindowString, CursorPosition, Length (WindowString) - CursorPosition + 1);
                        END
                    ELSE
                        TempString := KeyChar + WindowString;

                    WindowString := TempString;
                    ClrScr;
                    Write (WindowString);

                    Inc (CursorPosition);
                    GoToXY (CursorPosition, 1);
                    END
                ELSE
                    IF CursorPosition <= Length (WindowString) THEN   { We just overwrite the character }
                        BEGIN
                        WindowString [CursorPosition] := KeyChar;
                        Write (KeyChar);
                        Inc (CursorPosition);
                        GoToXY (CursorPosition, 1);
                        END
                    ELSE
                        BEGIN
                        WindowString := WindowString + KeyChar;
                        Write (KeyChar);
                        Inc (CursorPosition);
                        GoToXY (CursorPosition, 1);
                        END;

                { To show domestic multiplier status as it is entered in }

                IF (TBSIQ_ActiveWindow = TBSIQ_ExchangeWindow) AND DoingDomesticMults THEN
                    BEGIN
                    TempExchange.QTHString := GetLastString (WindowString);

                    IF DoingDomesticMults AND FoundDomesticQTH (TempExchange) THEN
                        VisibleLog.ShowDomesticMultiplierStatus (TempExchange.DomesticQTH);
                    END;

                { Need to be aware that if a character is added during QST_AutoStartSending that we
                  need to add it to the CW Buffer.  We do this by setting the ActionRequired flag so
                  that the QSOMachine will look for it.  We will need to be able to exit here and
                  have everything else done that is normally done before exiting EXCEPT setting the
                  ActionRequired flag to FALSE }

                IF QSOState = QST_AutoStartSending THEN
                    BEGIN
                    ActionRequired := True;
                    CallWindowString := WindowString;
                    CallWindowCursorPosition := CursorPosition;
                    Exit;
                    END;
                END;

        END;  { of case KeyChar }

    { If we got here - some case of KeyChar was found that did something that did not require the calling
       procedure to do anything.  We will return Chr (0) for KeyChar and ExtendedKey so the calling procedure
       doesn't do anything }

    KeyChar := Chr (0);
    ExtendedKeyChar := Chr (0);
    ActionRequired := False;

    { Remember the window string and cursor position for the active window }

    CASE TBSIQ_ActiveWindow OF
        TBSIQ_CallWindow:
            BEGIN
            CallWindowString := WindowString;
            CallWindowCursorPosition := CursorPosition;
            END;

        TBSIQ_ExchangeWindow:
            BEGIN
            ExchangeWindowString := WindowString;
            ExchangeWindowCursorPosition := CursorPosition;
            END;
        END;  { of case }
    END;



PROCEDURE QSOMachineObject.WriteCharacter (Ch: CHAR);

{ This is maybe a good idea - not being used currently }

    BEGIN
    CASE TBSIQ_ActiveWindow OF

        TBSIQ_CallWindow:
            BEGIN
            { First - we need to make sure that mine is the active window }

            SetTBSIQWindow (TBSIQ_CallWindow);

            { And the cursor is in the right place }

            GotoXY (CallWindowCursorPosition, 1);

            { Write the character }

            Write (Ch);

            Insert (Ch, CallWindowString, CallWindowCursorPosition);
            Inc (CallWindowCursorPosition);
            END;

       TBSIQ_ExchangeWindow:
            BEGIN
            { First - we need to make sure that mine is the active window }

            SetTBSIQWindow (TBSIQ_ExchangeWindow);

            { And the cursor is in the right place }

            GotoXY (ExchangeWindowCursorPosition, 1);

            { Write the character }

            Write (Ch);

            Insert (Ch, ExchangeWindowString, ExchangeWindowCursorPosition);
            Inc (ExchangeWindowCursorPosition);
            END;

       END;  { of case }
    END;



PROCEDURE QSOMachineObject.RemoveExchangeWindow;

    BEGIN
    ExchangeWindowString := '';
    ExchangeWindowCursorPosition := 1;
    ExchangeWindowIsUp := False;

    CASE Radio OF
        RadioOne:
            BEGIN
            RemoveWindow (TBSIQ_R1_ExchangeWindow);
            R1_OpMode := CQOpMode;
            END;

        RadioTwo:
            BEGIN
            RemoveWindow (TBSIQ_R2_ExchangeWindow);
            R2_OpMode := CQOpMode;
            END;

        END;  { of case Radio }
    END;



FUNCTION ControlAltOrShiftKeyAction (KeyboardRecord: FileRecord; VAR KeyStatus: KeyStatusRecord): BOOLEAN;

{ Will look to see if the keyboard record indicates if a control, alt or shift key has been pressed
  or unpressed and update the flags in the KeyStatus record.  Returns TRUE if a key was processed }

    BEGIN
    ControlAltOrShiftKeyAction := True;  { Default if we exit }

    { KB_Value = 2 seems to be if the key is repeating when held down for a long time }

    WITH KeyboardRecord DO
      CASE KB_Code OF
        29: BEGIN   { LeftControlKey }
            KeyStatus.LeftControlKeyPressed := (KB_Value = 1) OR (KB_Value = 2);
            Exit;
            END;

        42: BEGIN  { LeftShiftKey }
            KeyStatus.LeftShiftKeyPressed := (KB_Value = 1) OR (KB_Value = 2);
            Exit;
            END;

        54: BEGIN  { RightShiftKey }
            KeyStatus.RightShiftKeyPressed := (KB_Value = 1) OR (KB_Value = 2);
            Exit;
            END;

        56: BEGIN  { LeftAltKey }
            KeyStatus.LeftAltKeyPressed := (KB_Value = 1) OR (KB_Value = 2);
            Exit;
            END;

        97: BEGIN  { RightControlKey }
            KeyStatus.RightControlKeyPressed := (KB_Value = 1) OR (KB_Value = 2);
            Exit;
            END;

       100: BEGIN  { RightAltKey }
            KeyStatus.RightAltKeyPressed := (KB_Value = 1) OR (KB_Value = 2);
            Exit;
            END;

        END;  { of case }

    ControlAltOrShiftKeyAction := False;
    END;



PROCEDURE ProcessKeyboardRecord (KeyboardRecord: FileRecord; VAR KeyStatus: KeyStatusRecord);

{ Looks at the data in KB_Type, KB_Code and KB_Value and updates the values in the KeyStatus
  record as appropriate.  If we have something to report back with ReadKey - it will set the
  KeyPressed value to TRUE.  If is assumed KeyPressed is FALSE or we wouldn't be here }

VAR ControlKey, AltKey, ShiftKey: BOOLEAN;

    BEGIN
    { We only look at the record if the KB_Type = 1 }

    IF KeyboardRecord.KB_Type <> 1 THEN Exit;

    { See if this record is indicating the change of state of an Alt/Shift/Control key }

    IF ControlAltOrShiftKeyAction (KeyboardRecord, KeyStatus) THEN Exit; { Nothing to report }

    { If this is indicating that a key has been released - we really don't care }

    IF KeyboardRecord.KB_Value = 0 THEN Exit;

    { It is possible that someone is using the keyboard - but TRLog is not the focus. }

    IF NOT xfocused THEN Exit;

    { Nice to clear these out so they don't accumulate }

    WHILE KeyPressed DO ReadKey;

    { Most likely values that we will be returnning with }

    KeyStatus.KeyPressedCode := KeyboardRecord.KB_Code;
    KeyStatus.KeyChar := Chr (0);
    KeyStatus.ExtendedKey := False;
    KeyStatus.KeyPressed := True;

    ControlKey := KeyStatus.LeftControlKeyPressed OR KeyStatus.RightControlKeyPressed;
    AltKey     := KeyStatus.LeftAltKeyPressed OR KeyStatus.RightAltKeyPressed;
    ShiftKey   := KeyStatus.LeftShiftKeyPressed OR KeyStatus.RightShiftKeyPressed;

    CASE KeyboardRecord.KB_Code OF
         1: KeyStatus.KeyChar := EscapeKey;     { Escape key }

         2: BEGIN
            KeyStatus.KeyChar := '1';
            IF ShiftKey THEN KeyStatus.KeyChar := '!';
            IF AltKey THEN BEGIN KeyStatus.KeyChar := Alt1; KeyStatus.ExtendedKey := True; END;
            END;

         3: BEGIN
            KeyStatus.KeyChar := '2';
            IF ShiftKey THEN KeyStatus.KeyChar := '@';
            IF AltKey THEN BEGIN KeyStatus.KeyChar := Alt2; KeyStatus.ExtendedKey := True; END;
            END;

         4: BEGIN
            KeyStatus.KeyChar := '3';
            IF ShiftKey THEN KeyStatus.KeyChar := '#';
            IF AltKey THEN BEGIN KeyStatus.KeyChar := Alt3; KeyStatus.ExtendedKey := True; END;
            END;

         5: BEGIN
            KeyStatus.KeyChar := '4';
            IF ShiftKey THEN KeyStatus.KeyChar := '$';
            IF AltKey THEN BEGIN KeyStatus.KeyChar := Alt4; KeyStatus.ExtendedKey := True; END;
            END;

         6: BEGIN
            KeyStatus.KeyChar := '5';
            IF ShiftKey THEN KeyStatus.KeyChar := '%';
            IF AltKey THEN BEGIN KeyStatus.KeyChar := Alt5; KeyStatus.ExtendedKey := True; END;
            END;

         7: BEGIN
            KeyStatus.KeyChar := '6';
            IF ShiftKey THEN KeyStatus.KeyChar := '^';
            IF AltKey THEN BEGIN KeyStatus.KeyChar := Alt6; KeyStatus.ExtendedKey := True; END;
            END;

         8: BEGIN
            KeyStatus.KeyChar := '7';
            IF ShiftKey THEN KeyStatus.KeyChar := '&';
            IF AltKey THEN BEGIN KeyStatus.KeyChar := Alt7; KeyStatus.ExtendedKey := True; END;
            END;

         9: BEGIN
            KeyStatus.KeyChar := '8';
            IF ShiftKey THEN KeyStatus.KeyChar := '*';
            IF AltKey THEN BEGIN KeyStatus.KeyChar := Alt8; KeyStatus.ExtendedKey := True; END;
            END;

        10: BEGIN
            KeyStatus.KeyChar := '9';
            IF ShiftKey THEN KeyStatus.KeyChar := '(';
            IF AltKey THEN BEGIN KeyStatus.KeyChar := Alt9; KeyStatus.ExtendedKey := True; END;
            END;

        11: BEGIN
            KeyStatus.KeyChar := '0';
            IF ShiftKey THEN KeyStatus.KeyChar := ')';
            IF AltKey THEN BEGIN KeyStatus.KeyChar := Alt0; KeyStatus.ExtendedKey := True; END;
            END;

        12: BEGIN
            KeyStatus.KeyChar := '-';
            IF ShiftKey THEN KeyStatus.KeyChar := '_';
            IF ControlKey THEN KeyStatus.KeyChar := ControlDash;  { Not extended }
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltDash; KeyStatus.ExtendedKey := True; END;
            END;

        13: BEGIN
            KeyStatus.KeyChar := '=';
            IF ShiftKey THEN KeyStatus.KeyChar := '+';
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltEqual; KeyStatus.ExtendedKey := True; END;
            END;

        14: KeyStatus.KeyChar := Backspace;     { Backspace }

        15: IF ShiftKey THEN                    { Tab }
                KeyStatus.KeyChar := ShiftTab
            ELSE
                KeyStatus.KeyChar := TabKey;

        16: BEGIN
            KeyStatus.KeyChar := 'q';
            IF ShiftKey THEN KeyStatus.KeyChar := 'Q';
            IF ControlKey THEN KeyStatus.KeyChar := ControlQ;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltQ; KeyStatus.ExtendedKey := True; END;
            END;

        17: BEGIN
            KeyStatus.KeyChar := 'w';
            IF ShiftKey THEN KeyStatus.KeyChar := 'W';
            IF ControlKey THEN KeyStatus.KeyChar := ControlW;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltW; KeyStatus.ExtendedKey := True; END;
            END;

        18: BEGIN
            KeyStatus.KeyChar := 'e';
            IF ShiftKey THEN KeyStatus.KeyChar := 'E';
            IF ControlKey THEN KeyStatus.KeyChar := ControlE;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltE; KeyStatus.ExtendedKey := True; END;
            END;

        19: BEGIN
            KeyStatus.KeyChar := 'r';
            IF ShiftKey THEN KeyStatus.KeyChar := 'R';
            IF ControlKey THEN KeyStatus.KeyChar := ControlR;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltR; KeyStatus.ExtendedKey := True; END;
            END;

        20: BEGIN
            KeyStatus.KeyChar := 't';
            IF ShiftKey THEN KeyStatus.KeyChar := 'T';
            IF ControlKey THEN KeyStatus.KeyChar := ControlT;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltT; KeyStatus.ExtendedKey := True; END;
            END;

        21: BEGIN
            KeyStatus.KeyChar := 'y';
            IF ShiftKey THEN KeyStatus.KeyChar := 'Y';
            IF ControlKey THEN KeyStatus.KeyChar := ControlY;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltY; KeyStatus.ExtendedKey := True; END;
            END;

        22: BEGIN
            KeyStatus.KeyChar := 'u';
            IF ShiftKey THEN KeyStatus.KeyChar := 'U';
            IF ControlKey THEN KeyStatus.KeyChar := ControlU;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltU; KeyStatus.ExtendedKey := True; END;
            END;

        23: BEGIN
            KeyStatus.KeyChar := 'i';
            IF ShiftKey THEN KeyStatus.KeyChar := 'I';
            IF ControlKey THEN KeyStatus.KeyChar := ControlI;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltI; KeyStatus.ExtendedKey := True; END;
            END;

        24: BEGIN
            KeyStatus.KeyChar := 'o';
            IF ShiftKey THEN KeyStatus.KeyChar := 'O';
            IF ControlKey THEN KeyStatus.KeyChar := ControlO;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltO; KeyStatus.ExtendedKey := True; END;
            END;

        25: BEGIN
            KeyStatus.KeyChar := 'p';
            IF ShiftKey THEN KeyStatus.KeyChar := 'P';
            IF ControlKey THEN KeyStatus.KeyChar := ControlP;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltP; KeyStatus.ExtendedKey := True; END;
            END;


        26: BEGIN
            KeyStatus.KeyChar := '[';
            IF ShiftKey THEN KeyStatus.KeyChar := '{';
            IF ControlKey THEN KeyStatus.KeyChar := ControlLeftBracket;
            END;

        27: BEGIN
            KeyStatus.KeyChar := ']';
            IF ShiftKey THEN KeyStatus.KeyChar := '}';
            IF ControlKey THEN KeyStatus.KeyChar := ControlRightBracket;
            END;

        28: BEGIN                               { Carriage Return }
            KeyStatus.KeyChar := CarriageReturn;
            IF ControlKey THEN KeyStatus.ExtendedKey := True;
            IF AltKey THEN KeyStatus.ExtendedKey := True;   { Treat the same as control-enter }
            END;

        { 29: We don't need the left control key - it does not generate a response }

        30: BEGIN
            KeyStatus.KeyChar := 'a';
            IF ShiftKey THEN KeyStatus.KeyChar := 'A';
            IF ControlKey THEN KeyStatus.KeyChar := ControlA;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltA; KeyStatus.ExtendedKey := True; END;
            END;

        31: BEGIN
            KeyStatus.KeyChar := 's';
            IF ShiftKey THEN KeyStatus.KeyChar := 'S';
            IF ControlKey THEN KeyStatus.KeyChar := ControlS;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltS; KeyStatus.ExtendedKey := True; END;
            END;

        32: BEGIN
            KeyStatus.KeyChar := 'd';
            IF ShiftKey THEN KeyStatus.KeyChar := 'D';
            IF ControlKey THEN KeyStatus.KeyChar := ControlD;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltD; KeyStatus.ExtendedKey := True; END;
            END;

        33: BEGIN
            KeyStatus.KeyChar := 'f';
            IF ShiftKey THEN KeyStatus.KeyChar := 'F';
            IF ControlKey THEN KeyStatus.KeyChar := ControlF;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltF; KeyStatus.ExtendedKey := True; END;
            END;

        34: BEGIN
            KeyStatus.KeyChar := 'g';
            IF ShiftKey THEN KeyStatus.KeyChar := 'G';
            IF ControlKey THEN KeyStatus.KeyChar := ControlG;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltG; KeyStatus.ExtendedKey := True; END;
            END;

        35: BEGIN
            KeyStatus.KeyChar := 'h';
            IF ShiftKey THEN KeyStatus.KeyChar := 'H';
            IF ControlKey THEN KeyStatus.KeyChar := ControlH;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltH; KeyStatus.ExtendedKey := True; END;
            END;

        36: BEGIN
            KeyStatus.KeyChar := 'j';
            IF ShiftKey THEN KeyStatus.KeyChar := 'J';
            IF ControlKey THEN KeyStatus.KeyChar := ControlJ;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltJ; KeyStatus.ExtendedKey := True; END;
            END;

        37: BEGIN
            KeyStatus.KeyChar := 'k';
            IF ShiftKey THEN KeyStatus.KeyChar := 'K';
            IF ControlKey THEN KeyStatus.KeyChar := ControlK;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltK; KeyStatus.ExtendedKey := True; END;
            END;

        38: BEGIN
            KeyStatus.KeyChar := 'l';
            IF ShiftKey THEN KeyStatus.KeyChar := 'L';
            IF ControlKey THEN KeyStatus.KeyChar := ControlL;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltL; KeyStatus.ExtendedKey := True; END;
            END;

        39: IF ShiftKey THEN                    { ; key }
                KeyStatus.KeyChar := ':'
            ELSE
                KeyStatus.KeyChar := ';';

        40: IF ShiftKey THEN                    { ' key }
                KeyStatus.KeyChar := '"'
            ELSE
                KeyStatus.KeyChar := '''';

        41: IF ShiftKey THEN                    { ` key }
                KeyStatus.KeyChar := '~'
            ELSE
                KeyStatus.KeyChar := '`';

        43: BEGIN
            KeyStatus.KeyChar := '\';
            IF ShiftKey THEN KeyStatus.KeyChar := '|';
            IF ControlKey THEN KeyStatus.KeyChar := ControlBackSlash;
            END;

        44: BEGIN
            KeyStatus.KeyChar := 'z';
            IF ShiftKey THEN KeyStatus.KeyChar := 'Z';
            IF ControlKey THEN KeyStatus.KeyChar := ControlZ;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltZ; KeyStatus.ExtendedKey := True; END;
            END;

        45: BEGIN
            KeyStatus.KeyChar := 'x';
            IF ShiftKey THEN KeyStatus.KeyChar := 'X';
            IF ControlKey THEN KeyStatus.KeyChar := ControlX;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltX; KeyStatus.ExtendedKey := True; END;
            END;

        46: BEGIN
            KeyStatus.KeyChar := 'c';
            IF ShiftKey THEN KeyStatus.KeyChar := 'C';
            IF ControlKey THEN KeyStatus.KeyChar := ControlC;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltC; KeyStatus.ExtendedKey := True; END;
            END;

        47: BEGIN
            KeyStatus.KeyChar := 'v';
            IF ShiftKey THEN KeyStatus.KeyChar := 'V';
            IF ControlKey THEN KeyStatus.KeyChar := ControlV;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltV; KeyStatus.ExtendedKey := True; END;
            END;

        48: BEGIN
            KeyStatus.KeyChar := 'b';
            IF ShiftKey THEN KeyStatus.KeyChar := 'B';
            IF ControlKey THEN KeyStatus.KeyChar := ControlB;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltB; KeyStatus.ExtendedKey := True; END;
            END;

        49: BEGIN
            KeyStatus.KeyChar := 'n';
            IF ShiftKey THEN KeyStatus.KeyChar := 'N';
            IF ControlKey THEN KeyStatus.KeyChar := ControlN;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltN; KeyStatus.ExtendedKey := True; END;
            END;

        50: BEGIN
            KeyStatus.KeyChar := 'm';
            IF ShiftKey THEN KeyStatus.KeyChar := 'M';
            IF ControlKey THEN KeyStatus.KeyChar := ControlM;
            IF AltKey THEN BEGIN KeyStatus.KeyChar := AltM; KeyStatus.ExtendedKey := True; END;
            END;

        51: BEGIN
            KeyStatus.KeyChar := ',';
            IF ShiftKey THEN KeyStatus.KeyChar := '<';
            END;

        52: BEGIN
            KeyStatus.KeyChar := '.';
            IF ShiftKey THEN KeyStatus.KeyChar := '>';
            END;

        53: BEGIN                               { F1 }
            KeyStatus.KeyChar := '/';
            IF ShiftKey THEN KeyStatus.KeyChar := '?';
            END;

        { 54: TBSIQ_ReadKey := TBSIQ_RightShiftKey; }
        { 55: TBSIQ_ReadKey := TBSIQ_NumPadStar; }
        { 56: TBSIQ_ReadKey := TBSIQ_LeftAltKey; }

        57: BEGIN
            KeyStatus.KeyChar := ' ';
            END;

        59: BEGIN                               { F1 }
            KeyStatus.ExtendedKey := True;
            KeyStatus.KeyChar := F1;
            IF ControlKey THEN KeyStatus.KeyChar := ControlF1;
            IF ShiftKey THEN KeyStatus.KeyChar := ShiftF1;
            IF AltKey THEN KeyStatus.KeyChar := AltF1;
            END;

        60: BEGIN                               { F2 }
            KeyStatus.ExtendedKey := True;
            KeyStatus.KeyChar := F2;
            IF ControlKey THEN KeyStatus.KeyChar := ControlF2;
            IF ShiftKey THEN KeyStatus.KeyChar := ShiftF2;
            IF AltKey THEN KeyStatus.KeyChar := AltF2;
            END;

        61: BEGIN                               { F3 }
            KeyStatus.ExtendedKey := True;
            KeyStatus.KeyChar := F3;
            IF ControlKey THEN KeyStatus.KeyChar := ControlF3;
            IF ShiftKey THEN KeyStatus.KeyChar := ShiftF3;
            IF AltKey THEN KeyStatus.KeyChar := AltF3;
            END;

        62: BEGIN                               { F4 }
            KeyStatus.ExtendedKey := True;
            KeyStatus.KeyChar := F4;
            IF ControlKey THEN KeyStatus.KeyChar := ControlF4;
            IF ShiftKey THEN KeyStatus.KeyChar := ShiftF4;
            IF AltKey THEN KeyStatus.KeyChar := AltF4;
            END;

        63: BEGIN                               { F5 }
            KeyStatus.ExtendedKey := True;
            KeyStatus.KeyChar := F5;
            IF ControlKey THEN KeyStatus.KeyChar := ControlF5;
            IF ShiftKey THEN KeyStatus.KeyChar := ShiftF5;
            IF AltKey THEN KeyStatus.KeyChar := AltF5;
            END;

        64: BEGIN                               { F6 }
            KeyStatus.ExtendedKey := True;
            KeyStatus.KeyChar := F6;
            IF ControlKey THEN KeyStatus.KeyChar := ControlF6;
            IF ShiftKey THEN KeyStatus.KeyChar := ShiftF6;
            IF AltKey THEN KeyStatus.KeyChar := AltF6;
            END;

        65: BEGIN                               { F7 }
            KeyStatus.ExtendedKey := True;
            KeyStatus.KeyChar := F7;
            IF ControlKey THEN KeyStatus.KeyChar := ControlF7;
            IF ShiftKey THEN KeyStatus.KeyChar := ShiftF7;
            IF AltKey THEN KeyStatus.KeyChar := AltF7;
            END;

        66: BEGIN                               { F8 }
            KeyStatus.ExtendedKey := True;
            KeyStatus.KeyChar := F8;
            IF ControlKey THEN KeyStatus.KeyChar := ControlF8;
            IF ShiftKey THEN KeyStatus.KeyChar := ShiftF8;
            IF AltKey THEN KeyStatus.KeyChar := AltF8;
            END;

        67: BEGIN                               { F9 }
            KeyStatus.ExtendedKey := True;
            KeyStatus.KeyChar := F9;
            IF ControlKey THEN KeyStatus.KeyChar := ControlF9;
            IF ShiftKey THEN KeyStatus.KeyChar := ShiftF9;
            IF AltKey THEN KeyStatus.KeyChar := AltF9;
            END;

        68: BEGIN                               { F10 }
            KeyStatus.ExtendedKey := True;
            KeyStatus.KeyChar := F10;
            IF ControlKey THEN KeyStatus.KeyChar := ControlF10;
            IF ShiftKey THEN KeyStatus.KeyChar := ShiftF10;
            IF AltKey THEN KeyStatus.KeyChar := AltF10;
            END;

        { 69: TBSIQ_ReadKey := TBSIQ_NumLock; }
        { 71: TBSIQ_ReadKey := TBSIQ_NumPad7; }
        { 72: TBSIQ_ReadKey := TBSIQ_NumPad8; }
        { 73: TBSIQ_ReadKey := TBSIQ_NumPad9; }
        { 74: TBSIQ_ReadKey := TBSIQ_NumPadDash; }
        { 75: TBSIQ_ReadKey := TBSIQ_NumPad4; }
        { 76: TBSIQ_ReadKey := TBSIQ_NumPad5; }
        { 77: TBSIQ_ReadKey := TBSIQ_NumPad6; }
        { 78: TBSIQ_ReadKey := TBSIQ_NumPadPlus; }
        { 79: TBSIQ_ReadKey := TBSIQ_NumPad1; }

        { 80: TBSIQ_ReadKey := TBSIQ_NumPad2; }
        { 81: TBSIQ_ReadKey := TBSIQ_NumPad3; }
        { 82: TBSIQ_ReadKey := TBSIQ_NumPad0; }
        { 83: TBSIQ_ReadKey := TBSIQ_NumPadPeriod; }

        87: BEGIN                               { F11 }
            KeyStatus.ExtendedKey := True;
            KeyStatus.KeyChar := F11;
            IF ControlKey THEN KeyStatus.KeyChar := ControlF11;
            IF ShiftKey THEN KeyStatus.KeyChar := ShiftF11;
            IF AltKey THEN KeyStatus.KeyChar := AltF11;
            END;

        88: BEGIN                               { F12 }
            KeyStatus.ExtendedKey := True;
            KeyStatus.KeyChar := F12;
            IF ControlKey THEN KeyStatus.KeyChar := ControlF12;
            IF ShiftKey THEN KeyStatus.KeyChar := ShiftF12;
            IF AltKey THEN KeyStatus.KeyChar := AltF12;
            END;

        { 96: TBSIQ_ReadKey := TBSIQ_NumPadEnter; }
        { 97: TBSIQ_ReadKey := TBSIQ_RightControlKey; }
        { 98: TBSIQ_ReadKey := TBSIQ_NumPadSlash; }
       { 100: TBSIQ_ReadKey := TBSIQ_RightAltKey; }

       102: BEGIN                              { Home key }
            KeyStatus.ExtendedKey := True;
            KeyStatus.KeyChar := HomeKey;
            IF ControlKey THEN KeyStatus.KeyChar := ControlHome;
            END;

       103: BEGIN                              { Up Arrow }
            KeyStatus.ExtendedKey := True;
            KeyStatus.KeyChar := UpArrow;
            IF ControlKey THEN KeyStatus.KeyChar := ControlUpArrow;
            IF AltKey THEN KeyStatus.KeyChar := AltUpArrow;
            END;

       73, 104: BEGIN                               { Page Up }
                KeyStatus.ExtendedKey := True;
                KeyStatus.KeyChar := PageUpKey;
                IF ControlKey THEN KeyStatus.KeyChar := ControlPageUp;
                END;

       105: BEGIN                               { Left arrow }
            KeyStatus.ExtendedKey := True;
            KeyStatus.KeyChar := LeftArrow;
            IF ControlKey THEN KeyStatus.KeyChar := ControlLeftArrow;
            END;

       106: BEGIN                               { Right arrow }
            KeyStatus.ExtendedKey := True;
            KeyStatus.KeyChar := RightArrow;
            IF ControlKey THEN KeyStatus.KeyChar := ControlRightArrow;
            END;

       107: BEGIN                               { End key }
            KeyStatus.ExtendedKey := True;
            KeyStatus.KeyChar := EndKey;
            IF ControlKey THEN KeyStatus.KeyChar := ControlEnd;
            END;

       108: BEGIN                               { Down Arrow }
            KeyStatus.ExtendedKey := True;
            KeyStatus.KeyChar := DownArrow;
            IF ControlKey THEN KeyStatus.KeyChar := ControlDownArrow;
            IF AltKey THEN KeyStatus.KeyChar := AltDownArrow;
            END;

       81, 109: BEGIN                               { Page Down}
                KeyStatus.ExtendedKey := True;
                KeyStatus.KeyChar := PageDownKey;
                IF ControlKey THEN KeyStatus.KeyChar := ControlPageDown;
                END;

       110: BEGIN                               { Insert }
            KeyStatus.ExtendedKey := True;
            KeyStatus.KeyChar := InsertKey;
            IF ControlKey THEN KeyStatus.KeyChar := ControlInsert;
            IF AltKey THEN KeyStatus.KeyChar := AltInsert;
            END;

       111: BEGIN                               { Delete }
            KeyStatus.ExtendedKey := True;
            KeyStatus.KeyChar := DeleteKey;
            IF ControlKey THEN KeyStatus.KeyChar := ControlDelete;
            IF AltKey THEN KeyStatus.KeyChar := AltDelete;
            END;

        { These are for K5TR's apple keyboard }

        183: BEGIN                               { Apple F13 }
             KeyStatus.ExtendedKey := True;
             KeyStatus.KeyChar := ControlF3;
             END;

        184: BEGIN                               { Apple F14 }
             KeyStatus.ExtendedKey := True;
             KeyStatus.KeyChar := ControlF4;
             END;

        185: BEGIN                               { Apple F15 }
             KeyStatus.ExtendedKey := True;
             KeyStatus.KeyChar := ControlF5;
             END;

        END;  { of case }
    END;



FUNCTION TBSIQ_KeyPressed (Radio: RadioType): BOOLEAN;

{ Will look to see if either of the "by-id" files for keyboards have some
  new data to read.  If so - we will actually read the data to see what
  is there.

  The "by-id" files get three events per keystroke.  One will have all
  zeros for the KB_Type, KB_Code and KB_Value.  We ignore that none.  It
  will not generate a TRUE result to this function.

  When a key is depressed - we find that the KB_Type and KB_Value are
  both 1 and the KB_Code corresponds to the key.  For most keys, this
  will generate a TRUE result and the KB_Code value will be saved so
  that when a ReadKey is performmed, the value will be returned.

  However, in the case of pressing a Shift, Ctrl or Alt key - this procedure
  will return FALSE as it is waiting for a second key to be pressed.

  The global variables RadioKeyStatus1 and RadioKeyStatus2 are used to remember
  if the key is pressed.  These   values will remain TRUE until we see
  a "unpressed" event for the key.

  If you are interested to find out if someone is holding down say the
  shift key for purposes of RIT movement - you just can reference the
  global variable.

  I pretty much ignore the possibility of the CAPS LOCK key being active.
  All letter keys will return lower case unless a SHIFT key is pressed.
  Most calling routines are only interested in upper case, but it is their
  responsibility to convert lower case to uppoer case.

  As much as possible - the responses to extended keys will mimic the standard
  I/O - with a null character being returned first and then an extended key.
  This maintains compatability with routines written for the standard I/O.

  At the time of writing this - I have no idea how the auto-repeat
  function of the keyboard will affect this.  I think it will get
  multiple key pressed events, but we shall see.

  ALSO - if KeyPressed is called and a code is remembered, and KeyPressed
  is called again before that code is read with ReadKey, KeyPressed will
  return TRUE and keep the code the same. This mimics the behavior of the
  standard KeyPressed routine.  }

VAR FDS: Tfdset;
    KeyboardDataRecord: FileRecord;

    BEGIN
    { See if we already seen a key pressed that has not been read with ReadKey }

    IF (Radio = RadioOne) AND Radio1KeyStatus.KeyPressed THEN
        BEGIN
        TBSIQ_KeyPressed := True;
        Exit;
        END;

    IF (Radio = RadioTwo) AND Radio2KeyStatus.KeyPressed THEN
        BEGIN
        TBSIQ_KeyPressed := True;
        Exit;
        END;

    { Go check the "by-id" keyboard files to see if there is data there to read }

    fpfd_zero (FDS);

    CASE Radio OF
        RadioOne:
            BEGIN
            fpfd_set (R1KeyboardID, FDS);
            fpSelect (R1KeyboardID + 1, @FDS, nil, nil, 0);

            IF fpfd_ISSET (R1KeyboardID, FDS) > 0 THEN
                BEGIN
                FPRead (R1KeyboardID, KeyboardDataRecord, SizeOf (KeyboardDataRecord));
                ProcessKeyboardRecord (KeyboardDataRecord, Radio1KeyStatus);
                TBSIQ_KeyPressed := Radio1KeyStatus.KeyPressed;
                END
            ELSE
                TBSIQ_KeyPressed := False;
            END;

        RadioTwo:
            BEGIN
            fpfd_set (R2KeyboardID, FDS);
            fpSelect (R2KeyboardID + 1, @FDS, nil, nil, 0);

            IF fpfd_ISSET (R2KeyboardID, FDS) > 0 THEN
                BEGIN
                FPRead (R2KeyboardID, KeyboardDataRecord, SizeOf (KeyboardDataRecord));
                ProcessKeyboardRecord (KeyboardDataRecord, Radio2KeyStatus);
                TBSIQ_KeyPressed := Radio2KeyStatus.KeyPressed;
                END
            ELSE
                TBSIQ_KeyPressed := False;
            END;

       END;  { of case }

   END;



FUNCTION UnpackNextCharacter (VAR KeyStatus: KeyStatusRecord): CHAR;

{ Called with the data collected by KeyPressed.  Spits out the next character to be returned
  by ReadKey.  If it is an extended key - then a null will be sent and the ExtendedKey flag
  cleared.  It is assumed this won't ever get called if there isn't a key pressed. }

    BEGIN
    WITH KeyStatus DO
        BEGIN
        IF ExtendedKey THEN
            BEGIN
            UnpackNextCharacter := Chr (0);
            ExtendedKey := False;
            END
        ELSE
            BEGIN
            UnpackNextCharacter := KeyChar;
            KeyPressed := False;
            END;
        END;
    END;



FUNCTION TBSIQ_ReadKey (Radio: RadioType): CHAR;

    BEGIN

    { If we haven't found a key already - then wait for it }
    { I really don't think this shouild happen - so let's complain if it does }

    REPEAT UNTIL TBSIQ_KeyPressed (Radio);

    { Unpack the keyboard data and send a character back }

    CASE Radio OF
        RadioOne: TBSIQ_ReadKey := UnpackNextCharacter (Radio1KeyStatus);
        RadioTwo: TBSIQ_ReadKey := UnpackNextCharacter (Radio2KeyStatus);
        END;  { of case }

    END;



FUNCTION InitializeKeyboards: BOOLEAN;

{ Get the file names of the "by-id" files and determine which radio
  corresponds to which files.  The Radio1Filedescriptior and
  R2KeyboardID will be set and the files will be open }

VAR FileInfo: SearchRec;
    FileNameKB1, FileNameKB2: STRING;
    TempFD: CINT;

    BEGIN
    FileNameKB1 := '';
    FilenameKB2 := '';

    FindFirst ('/dev/input/by-id/*-kbd', Archive, FileInfo);

    WHILE DosError = 0 DO
        BEGIN
        IF FileNameKB1 = '' THEN
            FileNameKB1 := FileInfo.Name
        ELSE
            IF FileNameKB2 = '' THEN
                BEGIN
                FileNameKB2 := FileInfo.Name;
                Break;
                END;

        FindNext (FileInfo);
        END;

    R1KeyboardID := FpOpen ('/dev/input/by-id/' + FileNameKB1, O_RdOnly);
    R2KeyboardID := FpOpen ('/dev/input/by-id/' + FileNameKB2, O_RdOnly);

    WriteLn ('Press a key on the Radio 1 keyboard');

    REPEAT
    UNTIL TBSIQ_KeyPressed (RadioOne) OR TBSIQ_KeyPressed (RadioTwo);

    IF TBSIQ_KeyPressed (RadioTwo) THEN
        BEGIN
        WHILE TBSIQ_KeyPressed (RadioTwo) DO TBSIQ_ReadKey (RadioTwo);
        TempFD := R1KeyboardID;
        R1KeyboardID := R2KeyboardID;
        R2KeyboardID := TempFD;
        END
    ELSE
        WHILE TBSIQ_KeyPressed (RadioOne) DO TBSIQ_ReadKey (RadioOne);

    WriteLn ('Press a key on the Radio 2 keyboard (or press Radio 1 key again to abort)');

    REPEAT UNTIL (TBSIQ_KeyPressed (RadioOne) OR TBSIQ_KeyPressed (RadioTwo));

    IF TBSIQ_KeyPressed (RadioOne) THEN
        BEGIN
        WHILE TBSIQ_KeyPressed (RadioOne) DO TBSIQ_ReadKey (RadioOne);
        WriteLn ('Radio 1 key pressed.  Aborting,');
        InitializeKeyboards := FALSE;
        Exit;
        END;

    IF TBSIQ_KeyPressed (RadioTwo) THEN
        BEGIN
        WriteLn ('Radio 2 key Pressed.  You are all set.');
        WHILE TBSIQ_KeyPressed (RadioTwo) DO TBSIQ_ReadKey (RadioTwo);
        InitializeKeyboards := True;
        Exit;
        END;

    InitializeKeyboards := False;
    END;



FUNCTION NewInitializeKeyboards: BOOLEAN;

{ Get the file names of the "by-id" files and determine which radio
  corresponds to which files.  The Radio1Filedescriptior and
  R2KeyboardID will be set and the files will be open }

TYPE KeyboardFileRecord = RECORD
        FileID: CINT;
        FileName: STRING;
        END;

VAR FileInfo: SearchRec;
    TimeOut, Address, NumberFiles: INTEGER;
    KBData: ARRAY [0..10] OF KeyboardFileRecord;
    KeyboardDataRecord: FileRecord;
    Keyboard1Found, Keyboard2Found: BOOLEAN;
    KeyStatus: KeyStatusRecord;
    FDS: Tfdset;

    BEGIN
    NumberFiles := 0;

    FindFirst ('/dev/input/by-path/*-kbd', Archive, FileInfo);

    WHILE DosError = 0 DO
        BEGIN
        WITH KBData [Numberfiles] DO
            BEGIN
            FileName := Fileinfo.Name;
            FileID := 0;
            Inc (NumberFiles);
            END;

        FindNext (FileInfo);
        END;

    IF NumberFiles < 2 THEN
        BEGIN
        WriteLn ('Unable to find two keyboards');
        NewInitializeKeyboards := False;
        Exit;
        END;

    FOR Address := 0 TO NumberFiles - 1 DO
        WITH KBData [Address] DO
            FileID := FpOpen ('/dev/input/by-path/' + FileName, O_RdOnly);

    Keyboard1Found := False;
    Keyboard2Found := False;

    WriteLn ('Press a key on the Radio 1 keyboard');

    TimeOut := 0;

    REPEAT
        FOR Address := 0 TO NumberFiles - 1 DO
            WITH KBData [Address] DO
                BEGIN
                WITH KeyStatus DO
                    BEGIN
                    KeyPressedCode := 0;
                    KeyPressed             := FALSE;
                    ExtendedKey            := FALSE;
                    ExtendedKeyNullSent    := FALSE;
                    LeftShiftKeyPressed    := FALSE;
                    RightShiftKeyPressed   := FALSE;
                    LeftAltKeyPressed      := FALSE;
                    RightAltKeyPressed     := FALSE;
                    LeftControlKeyPressed  := FALSE;
                    RightControlKeyPressed := FALSE;
                    END;

                fpfd_set (FileID, FDS);
                fpSelect (FileID + 1, @FDS, nil, nil, 0);

                IF fpfd_ISSET (FileID, FDS) > 0 THEN
                    BEGIN
                    FPRead (FileID, KeyboardDataRecord, SizeOf (KeyboardDataRecord));

                    ProcessKeyboardRecord (KeyboardDataRecord, Radio1KeyStatus);

                    IF Radio1KeyStatus.KeyPressed THEN
                        BEGIN
                        R1KeyboardID := FileID;
                        Keyboard1Found := True;
                        FileID := 0;
                        Break;
                        END;
                    END;
                END;

        MilliSleep;
        Inc (TimeOut);
    UNTIL Keyboard1Found OR (TimeOut > 10000);

    IF TimeOut > 10000 THEN
        BEGIN
        WriteLn ('TIMEOUT!!  No keystroke found.');
        NewInitializeKeyboards := False;
        Exit;
        END;

    IF NOT Keyboard1Found THEN
        BEGIN
        WriteLn ('No keyboard found for Radio 1');
        NewInitializeKeyboards := False;
        Exit;
        END;

    WriteLn ('Press a key on the Radio 2 keyboard');

    TimeOut := 0;

    REPEAT
        FOR Address := 0 TO NumberFiles - 1 DO
            WITH KBData [Address] DO
                IF FileID > 0 THEN
                    BEGIN
                    WITH KeyStatus DO
                        BEGIN
                        KeyPressedCode := 0;
                        KeyPressed             := FALSE;
                        ExtendedKey            := FALSE;
                        ExtendedKeyNullSent    := FALSE;
                        LeftShiftKeyPressed    := FALSE;
                        RightShiftKeyPressed   := FALSE;
                        LeftAltKeyPressed      := FALSE;
                        RightAltKeyPressed     := FALSE;
                        LeftControlKeyPressed  := FALSE;
                        RightControlKeyPressed := FALSE;
                        END;

                    fpfd_zero (FDS);
                    fpfd_set (FileID, FDS);
                    fpSelect (FileID + 1, @FDS, nil, nil, 0);

                    IF fpfd_ISSET (FileID, FDS) > 0 THEN
                        BEGIN
                        FPRead (FileID, KeyboardDataRecord, SizeOf (KeyboardDataRecord));

                        ProcessKeyboardRecord (KeyboardDataRecord, KeyStatus);

                        IF KeyStatus.KeyPressed THEN
                            BEGIN
                            R2KeyboardID := FileID;
                            Keyboard2Found := True;
                            FileID := 0;
                            Break;
                            END;
                        END;
                    END;

        MilliSleep;
        Inc (TimeOut);
    UNTIL Keyboard2Found OR (TimeOut > 10000);

    IF TimeOut > 10000 THEN
        BEGIN
        WriteLn ('TIMEOUT!!  No keystroke found.');
        NewInitializeKeyboards := False;
        Exit;
        END;


    IF NOT Keyboard2Found THEN
        BEGIN
        WriteLn ('No keyboard found for Radio 2');
        NewInitializeKeyboards := False;
        Exit;
        END;

    WHILE TBSIQ_KeyPressed (RadioOne) DO TBSIQ_ReadKey (RadioOne);
    WHILE TBSIQ_KeyPressed (RadioTwo) DO TBSIQ_ReadKey (RadioTwo);

    NewInitializeKeyboards := True;
    END;



FUNCTION ValidFunctionKey (Key: CHAR): BOOLEAN;

{ Call with the extended key char }

    BEGIN
    ValidFunctionKey :=
       ((Key >= F1)         AND (Key <= F10)) OR
       ((Key >= F11)        AND (Key <= F12)) OR
       ((Key >= ShiftF1)    AND (Key <= ShiftF10)) OR
       ((Key >= ShiftF11)   AND (Key <= ShiftF12)) OR
       ((Key >= ControlF1)  AND (Key <= ControlF10)) OR
       ((Key >= ControlF11) AND (Key <= ControlF12)) OR
       ((Key >= AltF1)      AND (Key <= AltF10)) OR
       ((Key >= AltF11)     AND (Key <= AltF12));
    END;



FUNCTION QSOMachineObject.IAmTransmitting: BOOLEAN;

{ Typically called by the other radio to see if this one is transmitting }

VAR TransmitStatus: BOOLEAN;

    BEGIN
    CASE Mode OF

        { For CW - this is pretty straightforward }

        CW: BEGIN
            IAmTransmitting := NOT TBSIQ_CW_Engine.CWFinished (Radio);
            Exit;

            { So - I exited above - if I do the test below - it delays the
              clearing of the RED flag by about a half second.  I think this
              exposure is okay.  It just means you are NOT locked out if you
              are sending a message by hand. }

            { Let's go see if the radio is actually transmitting - maybe a
              hand sent message?  I realize this isn't perfect - but it's
              worth a shot }

            CASE Radio OF
                RadioOne: IAmTransmitting := Rig1.K3IsStillTalking;
                RadioTwo: IAmTransmitting := Rig2.K3IsStillTalking;
                END;  { of CASE }
            END;

        { For Phone or digital, we are relaying on the radio to tell us
          if it is transmitting or not.  This is tricky, because there
          is a significant delay from when we tell the radio to start a
          transmmission until it will show it has started.  We need to
          use a timer from whenever we tell the radio to do a transmission
          to help make sure we return the right status.

          That timer is TransmitCountDown and it is typically set to
          3 seconds whenever a command is sent to the radio to start
          a tranmisssion.  When it gets to zero - we will then start
          relying on the radio for the status. }


        Phone, Digital:
            BEGIN
            IF TransmitCountDown > 0 THEN
                BEGIN
                IAmTransmitting := True;

                { We are going to enable faster polling of the K3 so
                  we don't have to wait for the 300 ms delay that
                  the IF; command has on the TX status.  On December 4,
                  2023, I added the "AND NOT K3RXPollActive" so we don't
                  keep resending the command }

                IF (RadioInterfaced = K4) AND NOT K3RXPollActive THEN
                    BEGIN
                    CASE Radio OF
                        RadioOne: Rig1.SetK3TXPollMode (True);  { Kind of a misname here }
                        RadioTwo: Rig2.SetK3TXPollMode (True);  { Kind of a misname here }
                        END;

                    K3RXPollActive := True;
                    END;

                Exit;
                END;

            { We drop down here if the IAmTransmitting timeout has occurred. }

            { We are now going to rely on the radio status }

            CASE Radio OF
                RadioOne: TransmitStatus := Rig1.K3IsStillTalking;
                RadioTwo: TransmitStatus := Rig2.K3IsStillTalking;
                END;  { of CASE }

            { At some point we will go back to the normal method of monitoring
              the TX state (using the IF command), but we need to wait a little
              while before doing that as that TX state will stay true for about
              a half second }

            IF TransmitStatus THEN
                BEGIN
                IAmTransmitting := True;
                Exit;
                END;

            { The radio has gone into RX - we can turn off the fast polling }

            IF K3RXPollActive THEN
                BEGIN
                CASE Radio OF
                    RadioOne: Rig1.SetK3TXPollMode (False);
                    RadioTwo: Rig2.SetK3TXPollMode (False);
                    END;

                K3RXPollActive := False;
                END;

            IAmTransmitting := False;
            END;

        END;  { of case Mode }

    END;



FUNCTION QSOMachineObject.DisableTransmitting: BOOLEAN;

{ This is used to determine if a transmission can be started NOW.  If both
  radios are on CW - the CW sending engine will deal with any conflicts.
  But if either rig is on something other than CW - then we need to be more
  careful and not start a message when the other radio is transmitting.

  Note that the IAmTransmitting status stakes a second or so to go TRUE
  after starting transmission on the other radio. }

    BEGIN
    CASE Radio OF
        RadioOne:
            BEGIN
            IF (Mode = CW) and (Radio2QSOMachine.Mode = CW) THEN
                BEGIN
                DisableTransmitting := False;
                Exit;
                END;

            { One radio or the other is on phone }

            DisableTransmitting := Radio2QSOMachine.IAmTransmitting OR
                                   Radio2QSOMachine.SSBTransmissionStarted;
            Exit;
            END;

        RadioTwo:
            BEGIN
            IF (Mode = CW) and (Radio1QSOMachine.Mode = CW) THEN
                BEGIN
                DisableTransmitting := False;
                Exit;
                END;

            { One radio or the other is on phone }

            DisableTransmitting := Radio1QSOMachine.IAmTransmitting OR
                                   Radio1QSOMachine.SSBTransmissionStarted;
            Exit;
            END;

        END;  { of case Radio }
    END;



PROCEDURE QSOMachineObject.SendFunctionKeyMessage (Key: CHAR; VAR Message: STRING);

{ New and improved to make sure we are using the right mode }

    BEGIN
    IF NOT ValidFunctionKey (Key) THEN Exit;

    IF ElaspedSec100 (LastFunctionKeyTime) < 10 THEN Exit;

    MarkTime (LastFunctionKeyTime);

    { When doing ExpandCrypticString, a function key is likely to start some
      kind of transmission.  This is okay if both rigs are on CW - or if the
      other radio is not busy transmitting. }

    IF DisableTransmitting THEN Exit;

    { If we are dealing with mixed modes - we need to make sure we set
      things up for the right mode }

    IF TBSIQDualMode THEN   { New for TBSIQ }
        BEGIN
        ActiveRadio := Radio;
        ActiveMode := Mode;
        ActiveKeyer.SetActiveRadio (Radio);
        END;

    { Do we get a CQ message or an Exchange one? }

    IF (QSOState = QST_Idle) OR (QSOState = QST_CallingCQ) OR
       (QSOState = QST_AutoCQListening) OR
       (QSOState = QST_CQCalled) OR (QSOState = QST_AutoStartSending) OR
       (QSOState = QST_CQSending73Message) THEN
           Message := GetCQMemoryString (Mode, Key)
       ELSE
           CASE Key OF
               F1: BEGIN
                   IF Mode = CW THEN
                       BEGIN
                       Message := '\';
                       SearchAndPounceStationCalled := True;
                       ENd
                   ELSE
                       Message := GetExMemoryString (Mode, Key);
                   END;

               F2: CASE Mode OF
                       CW:      BEGIN
                                Message := SearchAndPounceExchange;
                                SearchAndPounceExchangeSent := True;
                                END;

                       Phone:   BEGIN
                                Message := ExpandCrypticString (SearchAndPouncePhoneExchange);
                                IF Message = '' THEN
                                    Message := ExpandCrypticString (GetExMemoryString (Phone, Key));
                                END;

                       Digital: Message := ExpandCrypticString (GetExMemoryString (Digital, Key));
                       END;  { of CASE Mode }

               ELSE
                   Message := ExpandCrypticString (GetEXMemoryString (Mode, Key));

               END;  { of CASE Key }

    { New on 30-Sep-2022 - hmm - should I do this if the message is only going
      into the cue?  Am I doing this for SSB only?

      And when I am trying to send a CW message when ActiveMode is Phone (dual
      mode case) - I need to have both the ActiveRadio set to Radio and
      ActiveMode set to CW...  so - maybe it is okay to do this all of the
      time?  I don't need the TransmitCountDown set for CW }

    IF Message <> '' THEN
        BEGIN
        { This is what used to be here }

        IF (ActiveMode = Phone) OR (ActiveMode = Digital) THEN
            BEGIN
            ActiveRadio := Radio;
            ActiveMode := Mode;   { We don't need this - mode never changes? }
            TransmitCountDown := InitialTransmitCountdown;
            END;
        END;

    { This sends the message if you are on SSB }

    Message := ExpandCrypticString (Message);

    IF (Mode = CW) AND (Message <> '') THEN
        BEGIN
        TBSIQ_CW_Engine.CueCWMessage (Message, Radio, CWP_High, False);
        ShowCWMessage (Message);
        END;

    IF (Mode = Digital) AND (Message <> '') THEN
        BEGIN
        FinishRTTYTransmission (Message);
        ShowCWMessage (Message);
        END;
    END;



FUNCTION TBSIQ_ParametersOkay (Call: CallString;
                               QSONumberSent: INTEGER;
                               ExchangeString: Str80;
                               Band: BandType;
                               Mode: ModeType;
                               Freq: LONGINT;
                               VAR RData: ContestExchange): BOOLEAN;

{ This is very similar to the ParametersOKay function in TR.PAS, but
  modified to work in the 2BSIQ environment of two window.

  This function get called when a carriage return has been pressed when
  entering exchange data.  It will look at the data in the exchange
  window and decide if enough information is there to log the contact.
  If something is missing, a False response will be generated.  It the
  correct information is there, a True response will be generated and
  the appropriate fields in the ContestExchange record will be updated.

  It is the responsibility of this function to put the proper multiplier
  information into the proper fields in the RData record.  The
  information in the RData.QTH is "raw" information and may need
  to be modified before putting it into the DomesticQTH, DXQTH, Prefix or
  Zone fields of RData.  This has the effect of doing away with
  most of the meaning of the active multiplier flags except to know that
  the multiplier is switched on.                                        }

VAR I: INTEGER;
    TempString: Str80;
    Hours, Minutes, Seconds, Hundreths: Word;

    BEGIN
    TBSIQ_ParametersOkay := False;  { Default in case of early escape }

    ExchangeErrorMessage := '';
    LogBadQSOString := '';

    GetRidOfPostcedingSpaces (ExchangeString);
    IF ExchangeString = '' THEN Exit;             { No exhange - abort }

    { Figure out the time }

    GetTime (Hours, Minutes, Seconds, Hundreths);

    I := Hours;

    IF HourOffset <> 0 THEN
        BEGIN
        I := I + HourOffset;
        IF I > 23 THEN I := I - 24;
        IF I < 0  THEN I := I + 24;
        END;

    Hours := I;

    { Start building up the RData data structure with the data }

    ClearContestExchange (RData);

    RData.Callsign := Call;
    RData.Callsign := GetCorrectedCallFromExchangeString (ExchangeString);

    { See if there is a different RST in the exchange window }

    TempString := GetSentRSTFromExchangeString (ExchangeString);
    IF TempString <> '' THEN RData.RSTSent := TempString;

    IF RData.Callsign = '' THEN
        RData.Callsign := Call
    ELSE
        BEGIN        { Callsign was updated in the exchange window }

        { For now - I am not going to bother displaying the callsign

        CallWindowString := RData.Callsign;
        SaveAndSetActiveWindow (CallWindow);
        ClrScr;
        Write (CallWindowString);
        RestorePreviousWindow;  }
        END;

    IF ParameterOkayMode = QSLAndLog THEN
        BEGIN
        RData.Time       := Hours * 100 + Minutes;
        RData.Band       := Band;
        RData.Mode       := Mode;

        IF QSONumberSent > 0 THEN
            RData.NumberSent := QSONumberSent
        ELSE
            RData.NumberSent := TotalContacts + 1;

        RData.Frequency  := Freq;

        IF RData.Mode = PHONE THEN
            DefaultRST := '59'
        ELSE
            DefaultRST := '599';

        IF RData.RSTSent = '' THEN
            IF RData.Mode = Phone THEN
                RData.RSTSent := LogRSSent
            ELSE
                RData.RSTSent := LogRSTSent;

        LocateCall (RData.Callsign, Rdata.QTH, True);

        IF DoingDXMults THEN GetDXQTH (RData);

        IF DoingPrefixMults THEN
            CASE ActivePrefixMult OF

                BelgiumPrefixes:
                    IF RData.QTH.CountryID = 'ON' THEN
                        RData.Prefix := RData.QTH.Prefix;

                SACDistricts: RData.Prefix := SACDistrict (RData.QTH);

                Prefix: RData.Prefix := RData.QTH.Prefix;

                SouthAmericanPrefixes:
                    IF RData.QTH.Continent = SouthAmerica THEN
                        RData.Prefix := RData.QTH.Prefix;

                NonSouthAmericanPrefixes:
                    IF RData.QTH.Continent <> SouthAmerica THEN
                        RData.Prefix := RData.QTH.Prefix;
                END;

        GetRidOfPrecedingSpaces  (ExchangeString);
        GetRidOfPostcedingSpaces (ExchangeString);

        TBSIQ_ParametersOkay := True;
        LogBadQSOString := ExchangeString;
        CalculateQSOPoints (RData);
        Exit;
        END;

    IF NOT GoodCallSyntax (RData.Callsign) THEN
        BEGIN
        { QuickDisplay (RData.Callsign + ' has improper syntax!!');}
        Exit;
        END;

    RData.Time       := Hours * 100 + Minutes;
    RData.Band       := Band;
    RData.Mode       := Mode;

    IF QSONumberSent > 0 THEN
        RData.NumberSent := QSONumberSent
    ELSE
        RData.NumberSent := TotalContacts + 1;

    RData.Frequency  := Freq;

    IF RData.RSTSent = '' THEN
        IF RData.Mode = Phone THEN
            RData.RSTSent := LogRSSent
        ELSE
            RData.RSTSent := LogRSTSent;

    IF ActiveMode = PHONE THEN
        DefaultRST := '59'
    ELSE
        DefaultRST := '599';

    LocateCall (RData.Callsign, RData.QTH, True);

    IF DoingDXMults THEN GetDXQTH (RData);

    IF DoingPrefixMults THEN
        CASE ActivePrefixMult OF

            BelgiumPrefixes:
                IF RData.QTH.CountryID = 'ON' THEN
                    RData.Prefix := RData.QTH.Prefix;

            SACDistricts: RData.Prefix := SACDistrict (RData.QTH);

            Prefix: RData.Prefix := RData.QTH.Prefix;

            SouthAmericanPrefixes:
                IF RData.QTH.Continent = SouthAmerica THEN
                    RData.Prefix := RData.QTH.Prefix;

            NonSouthAmericanPrefixes:
                IF RData.QTH.Continent <> SouthAmerica THEN
                    RData.Prefix := RData.QTH.Prefix;
            END;

    GetRidOfPrecedingSpaces  (ExchangeString);
    GetRidOfPostcedingSpaces (ExchangeString);

    TBSIQ_ParametersOkay := ProcessExchange (ExchangeString, RData);
    CalculateQSOPoints (RData);
    END;



PROCEDURE TBSIQ_LogContact (VAR RXData: ContestExchange);

{ This procedure will log the contact just completed.  It will be
  pushed onto the editable log and the log entry popped off the editable
  log will be examined and written to the LOG.DAT file.                 }

VAR LogString: Str80;

    BEGIN
    RXData.TimeSeconds := GetTimeSeconds;

    VisibleDupeSheetChanged := True;

    IF LastDeletedLogEntry <> '' THEN
        BEGIN
        LastDeletedLogEntry := '';
        RemoveWindow (QuickCommandWindow);
        END;

    LastQSOLogged := RXData;

    IF TenMinuteRule <> NoTenMinuteRule THEN
        UpdateTenMinuteDate (RXData.Band, RXData.Mode);

    { IF NOT TailEnding THEN RemoveWindow (PossibleCallWindow); }

    { This is simplified from LOGSUBS2 }

    IF VisibleLog.CallIsADupe (RXData.Callsign, RXData.Band, RXData.Mode) THEN
        RXData.QSOPoints := 0
    ELSE
        VisibleLog.ProcessMultipliers (RXData);  { Yes! This is in LOGEDIT.PAS }

    LogString := MakeLogString (RXData);  { Yes!  This is in LOGSTUFF.PAS }

    IF (RXData.Band >= Band160) AND (RXData.Band <= Band10) THEN
        Inc (ContinentQSOCount [RXData.Band, RXData.QTH.Continent]);

    TBSIQ_PushLogStringIntoEditableLogAndLogPopedQSO (LogString, True);

    IF DoingDomesticMults AND
       (MultByBand OR MultByMode) AND
       (RXData.DomesticQTH <> '') THEN
           VisibleLog.ShowDomesticMultiplierStatus (RXData.DomMultQTH);

    Inc (NumberContactsThisMinute);
    NumberQSOPointsThisMinute := NumberQSOPointsThisMinute + RXData.QSOPoints;

{   DisplayTotalScore (TotalScore); }

    IF FloppyFileSaveFrequency > 0 THEN
        IF QSOTotals [All, Both] > 0 THEN
            IF QSOTotals [All, Both] MOD FloppyFileSaveFrequency = 0 THEN
                SaveLogFileToFloppy;

    IF UpdateRestartFileEnable THEN Sheet.SaveRestartFile;

    UpdateTotals;
    DisplayTotalScore (TotalScore);
    VisibleLog.ShowRemainingMultipliers;

    IF BandMapEnable THEN
        BEGIN
        UpdateBandMapMultiplierStatus;
        UpdateBandMapDupeStatus (RXData.Callsign, RXData.Band, RXData.Mode, True);
        END;


    IF (QSO_UDP_IP <> '') AND (QSO_UDP_Port <> 0) THEN
        BEGIN
        RXData.Date := GetFullDateString;
        SendQSOToUDPPort (RXData);
        END;
    END;



PROCEDURE TBSIQ_PushLogStringIntoEditableLogAndLogPopedQSO (LogString: Str80; MyQSO: BOOLEAN);

VAR TempString: STRING;
    TempData: ContestExchange;

    BEGIN
    { Leveraged from LOGSUBS2.PAS }

    IF (ActiveMultiPort <> nil) AND SendQSOImmediately AND MyQSO THEN
        BEGIN
        GetRidOfPostcedingSpaces (LogString);

        IF ((LogString <> '') AND NOT MultiMultsOnly) OR
           (GetLogEntryMultString (LogString) <> '') THEN
               IF K1EANetworkEnable THEN
                   BEGIN

                   { Don't send notes }

                   IF Copy (LogString, 1, 1) <> ';' THEN
                       BEGIN
                       TempString := ConvertN6TRLogStringToK1EANetworkFormat (LogString);
                       SendMultiMessage (TempString);
                       END;
                   END
               ELSE
                   BEGIN
                   SendMultiCommand (MultiBandAddressArray [ActiveBand],
                                     $FF, MultiQSOData, LogString);
                   END;
        END;


    LogString := VisibleLog.PushLogEntry (LogString);

    { LogString is now what popped off the top of the editable window }

    GetRidOfPostcedingSpaces (LogString);

    IF LogString <> '' THEN
        BEGIN
        TBSIQ_PutContactIntoLogFile (LogString);

        { From LOGSUBS2.PAS }

        IF ParseExchangeIntoContestExchange (LogString, TempData) THEN
            BEGIN
            IF (ActiveMultiPort <> nil) AND (NOT SendQSOImmediately) THEN
                BEGIN
                IF (NOT MultiMultsOnly) OR
                   (GetLogEntryMultString (LogString) <> '') THEN
                    IF K1EANetworkEnable THEN
                        BEGIN

                        { Don't send notes }

                        IF Copy (LogString, 1, 1) <> ';' THEN
                            BEGIN
                            TempString := ConvertN6TRLogStringToK1EANetworkFormat (LogString);
                            SendMultiMessage (TempString);
                            END;
                        END
                    ELSE
                        BEGIN
                        SendMultiCommand (MultiBandAddressArray [ActiveBand],
                                          $FF, MultiQSOData, LogString);
                        END;
            END


        ELSE  { QSO doesn't make sense - probably a note }

            IF (ActiveMultiPort <> nil) AND (NOT SendQSOImmediately) THEN
                IF K1EANetworkEnable THEN
                    BEGIN

                    { I don't know how to do this on the K1EA network }

                    END
                ELSE
                    BEGIN
                    SendMultiCommand (MultiBandAddressArray [ActiveBand],
                                      $FF, MultiQSOData, LogString);
                    END;
            END;
        END;

    END;



PROCEDURE TBSIQ_PutContactIntoLogFile (LogString: Str80);

VAR Time, QSONumber: INTEGER;
    Call, Exchange, LoggedCallsign: Str20;
    FileWrite: TEXT;

    BEGIN
    IF Copy (LogString, 1, 1) = ';' THEN  { Its a note - just copy it and be done }
        BEGIN
        WriteLogEntry (LogString);
        Exit;
        END;

    GetRidOfPostcedingSpaces (LogString);

    IF LogString <> '' THEN
        BEGIN
        IF LogString [LogEntryNameSentAddress] = '*' THEN
            Inc (TotalNamesSent);

        IF QSOTotals [All, Both] MOD ContactsPerPage = 0 THEN
            BEGIN
            IF QSOTotals [All, Both] > 0 THEN
                NextPage;
            PrintLogHeader;
            END;

        IF QSOTotals [All, Both] MOD ContactsPerPage > 9 THEN
            IF QSOTotals [All, Both] MOD 10 = 0 THEN
                WriteLogEntry ('');

        VisibleLog.PutLogEntryIntoSheet (LogString);
        WriteLogEntry                   (LogString);

        IF UnknownCountryFileEnable THEN
            BEGIN
            LoggedCallsign := GetLogEntryCall (LogString);
            IF CountryTable.GetCountry (LoggedCallsign, True) = -1 THEN
                IF OpenFileForAppend (FileWrite, UnknownCountryFileName) THEN
                    BEGIN
                    WriteLn (FileWrite, LogString);
                    Close (FileWrite);
                    END;
            END;

        { We only add QSOs to the pending list if we got QSO points for it }

        IF QTCsEnabled AND (MyContinent <> Europe) AND
           (GetLogEntryQSOPoints (LogString) > 0) THEN
               BEGIN
               Time := GetLogEntryIntegerTime (LogString);
               Call := GetLogEntryCall (LogString);
               Exchange := GetLogEntryExchangeString (LogString);
               GetRidOfPrecedingSpaces (Exchange);
               Exchange := PostcedingString (Exchange, ' ');
               GetRidOfPrecedingSpaces (Exchange);
               Exchange := PostcedingString (Exchange, ' ');
               GetRidOfPrecedingSpaces  (Exchange);
               GetRidOfPostcedingSpaces (Exchange);
               Val (Exchange, QSONumber);
               AddQSOToPendingQTCList (Time, Call, QSONumber);
               END;

        IF SingleBand <> All THEN
            BEGIN
            IF GetLogEntryBand (LogString) = SingleBand THEN
                TotalQSOPoints := TotalQSOPoints + GetLogEntryQSOPoints (LogString);
            END
        ELSE
            TotalQSOPoints := TotalQSOPoints + GetLogEntryQSOPoints (LogString);
        END;
    END;




PROCEDURE PaintVerticalLine;

{ Paints a vertical line between the radios on the 2BSIQ display }

    BEGIN
    SaveAndSetActiveWindow (WholeScreenWindow);
    GoToXY (40, 19);
    Write ('|');
    GoToXY (40, 20);
    Write ('|');
    GoToXY (40, 21);
    Write ('|');
    GoToXY (40, 22);
    Write ('|');
    GoToXY (40, 23);
    Write ('|');
    GoToXY (40, 24);
    Write ('|');
    GoToXY (40, 25);
    Write ('|');
    RestorePreviousWindow;
    END;



    BEGIN
    TBSIQ_BandMapFocus := NoRadio;

    ResetKeyStatus (RadioOne);
    ResetKeyStatus (RadioTwo);
    END.
