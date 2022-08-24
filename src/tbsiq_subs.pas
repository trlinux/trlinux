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

{ Subroutines used by the 2BSIQ module and the QSOMachineObject for which two instances make up the operator
  interface }

{$O+}
{$V-}

INTERFACE

USES Dos, Tree, LogWind, LogDupe, LogStuff, ZoneCont, Country9,
     so2r, LogCW, LogDVP, LogDom, Printer, LogK1EA, LogHelp, LogGrid, trCrt,
     jctrl2,LogPack,LogWAE, LogEdit,LogSCP,datetimec,radio,ctypes,xkb,timer,TBSIQ_CW;

TYPE

    { These window names are part of template for the QSO machine object.  When you
      use the procedure SetTBSIQWindowm, it will figure out which radio is being
      used and call the appropriate LogWind window }

    TBSIQ_WindowType = (TBSIQ_CallWindow,
                        TBSIQ_StartSendingWindow,
                        TBSIQ_ExchangeWindow,
                        TBSIQ_CWMessageWindow,
                        TBSIQ_StateMachineStatusWindow);

    TBSIQ_QSOStateType = (QST_None,
                          QST_Idle,
                          QST_CallingCQ,
                          QST_CQCalled,
                          QST_AutoStartSending,
                          QST_CQStationBeingAnswered,
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

        TBSIQ_ActiveWindow: TBSIQ_WindowType;

        AutoStartSendStationCalled: BOOLEAN;

        Band: BandType;
        BeSilent: BOOLEAN;   { Used to indicate we should not send CW for command }

        CallsignICameBackTo: STRING;
        CallWindowString: STRING;
        CallWindowCursorPosition: INTEGER;
        ClearKeyCache: BOOLEAN;
        CodeSpeed: INTEGER;
        CWMessageDisplayed: STRING;

        DisplayedBand: BandType;
        DisplayedFrequency: LONGINT;
        DisplayedMode: ModeType;

        ExchangeWindowIsUp: BOOLEAN;
        ExchangeWindowString: STRING;
        ExchangeWindowCursorPosition: INTEGER;

        Frequency: LONGINT;

        InitialExchangePutUp: BOOLEAN;

        KeyboardCWMessage: STRING;

        LastQSOState: TBSIQ_QSOStateType;
        LastSCPCall: CallString;

        Mode: ModeType;

        QSOState: TBSIQ_QSOStateType;

        PreviousQSOState: TBSIQ_QSOStateType;

        SearchAndPounceStationCalled: BOOLEAN;
        SearchAndPounceExchangeSent: BOOLEAN;

        SCPScreenFull: BOOLEAN;
        StartSendingCursorPosition: INTEGER; { initially = AutoSendCharacterCount }

        PROCEDURE CheckQSOStateMachine;
        PROCEDURE ClearAutoSendDisplay;  { for use during S&P }

        PROCEDURE DisplayAutoSendCharacterCount;
        PROCEDURE DisplayBandMode;
        PROCEDURE DisplayCodeSpeed;
        PROCEDURE DisplayFrequency;
        PROCEDURE DisplaySCPCall (Call: CallString);

        FUNCTION  ExpandCrypticString (SendString: STRING): STRING;

        PROCEDURE InitializeQSOMachine (KBFile: CINT;
                                        RadioID: RadioType;
                                        WinX, WinY: INTEGER);

        FUNCTION  LegalKey (KeyChar: CHAR): BOOLEAN;

        PROCEDURE ListenToBothRadios;
        PROCEDURE ListenToOtherRadio;

        PROCEDURE RemoveExchangeWindow;

        PROCEDURE SendFunctionKeyMessage (Key: CHAR; VAR Message: STRING);

        PROCEDURE SendKeyboardInput;
        PROCEDURE SetTBSIQWindow (TBSIQ_Window: TBSIQ_WindowType);
        PROCEDURE ShowCWMessage (Message: STRING);
        PROCEDURE ShowStateMachineStatus;
        PROCEDURE ShowTransmitStatus;
        PROCEDURE SuperCheckPartial;
        PROCEDURE SwapWindows;     { Moves from exchange <> CQ window }

        PROCEDURE UpdateRadioDisplay;  { Band/mode/frequency }

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

    TBSIQ_QSOState: TBSIQ_QSOStateType;


FUNCTION  InitializeKeyboards: BOOLEAN;

FUNCTION  TBSIQ_KeyPressed (Radio: RadioType): BOOLEAN;  { Radio = 1 or 2 }

PROCEDURE TBSIQ_LogContact (VAR RXData: ContestExchange);

FUNCTION  TBSIQ_ParametersOkay (Call: CallString; ExchangeString: Str80;
                                Band: BandType; Mode: ModeType; Freq: LONGINT;
                                VAR RData: ContestExchange): BOOLEAN;

PROCEDURE TBSIQ_PushLogStringIntoEditableLogAndLogPopedQSO (LogString: Str80; MyQSO: BOOLEAN);
PROCEDURE TBSIQ_PutContactIntoLogFile (LogString: Str80);

FUNCTION  TBSIQ_ReadKey (Radio: RadioType): CHAR;

PROCEDURE TBSIQ_UpdateTimeAndRateDisplays;  { Not radio specific }

FUNCTION  ValidFunctionKey (Key: CHAR): BOOLEAN;

IMPLEMENTATION

USES KeyCode, BaseUnix;

TYPE
    FileRecord = RECORD  { This is the data record that we read off of the keyboard "files"  }
        Sec:  QWORD;
        USec: QWORD;
        KB_Type: WORD;
        KB_Code: WORD;
        KB_Value: LONGINT;
        END;



FUNCTION FoundCommand (VAR SendString: Str160): BOOLEAN;

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

        IF CommandString = 'BANDUP'         THEN
            BEGIN
            RememberFrequency; {KK1L: 6.72 Added to match all other calls. Needed for loss of coms}
            BandUp;
            END;
        IF CommandString = 'BANDDOWN'       THEN
            BEGIN
            RememberFrequency; {KK1L: 6.72 Added to match all other calls. Needed for loss of coms}
            BandDown;
            END;

        IF CommandString = 'CONTROLENTER'   THEN CWMessageCommand := CWCommandControlEnter;
        IF CommandString = 'CQMODE'         THEN CWMessageCommand := CWCommandCQMode;
        IF CommandString = 'CWENABLETOGGLE' THEN CWEnable := NOT CWEnable;

        IF CommandString = 'CWMONITORON'    THEN
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
            begin
               if activeradio = radioone then
                  so2rbox.setrcvfocus(RX1)
               else
                  so2rbox.setrcvfocus(RX2)
            end;
            if filename = 'RXI' then
            begin
               if activeradio = radioone then
                  so2rbox.setrcvfocus(RX2)
               else
                  so2rbox.setrcvfocus(RX1)
            end;

//            IF SendString <> '' THEN
//                REPEAT millisleep UNTIL RadioSendBufferEmpty (ActiveRadio);
            END;


        IF CommandString = 'SRS' THEN
            BEGIN
               if activeradio = radioone then
                  rig1.directcommand(filename)
               else
                  rig2.directcommand(filename);
            END;

        IF CommandString = 'SRS1' THEN
            BEGIN
               rig1.directcommand(filename);
            END;

        IF CommandString = 'SRS2' THEN
            BEGIN
               rig2.directcommand(filename);
            END;

        IF CommandString = 'SRSI' THEN
            BEGIN
               if activeradio = radioone then
                  rig2.directcommand(filename)
               else
                  rig1.directcommand(filename);
            END;

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


PROCEDURE TBSIQ_UpdateTimeAndRateDisplays;

VAR TimeString, FullTimeString, HourString: Str20;
    RateMinute: INTEGER;

    BEGIN
    FullTimeString := GetFullTimeString;

    IF FullTimeString = LastFullTimeString THEN Exit;  { Nothing to do }
    LastFullTimeString := FullTimeString;

    { Code following this will be exectued once for each new calendar second }
    { The time gets displayed in the upper left corner of the Total Window }

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

    HourString := PrecedingString (TimeString, ':');

    Inc (MinutesSinceLastBMUpdate); {KK1L: 6.65}

    {KK1L: 6.65 Added this check to allow for > 63 minute BM decay}
    IF MinutesSinceLastBMUpdate >= BandMapDecayMultiplier THEN
        BEGIN
        MinutesSinceLastBMUpdate := 0;
        DecrementBandMapTimes;
        IF BandMapEnable THEN DisplayBandMap;
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



PROCEDURE QSOMachineObject.ClearAutoSendDisplay;

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

    SCPScreenFull := False;
    LastSCPCall := CallWindowString;

    IF NOT CD.PartialCallSetup (CallWindowString) THEN Exit;

    EditableLogDisplayed := False;
    GridSquareListShown := False;

    SaveSetAndClearActiveWindow (EditableLogWindow);

    REPEAT
        Call := CD.GetNextPartialCall;

        IF Call <> '' THEN DisplaySCPCall (Call);

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

VAR CharacterCount: INTEGER;
    NewSendString: STRING;
    SendChar: CHAR;

{ This is a very scaled down version of what is in the main program }

    BEGIN
    FoundCommand (SendString);

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
            '#': BEGIN
                 { For now - I am going to ignore QSO numbers.  }
                 END;

            '_': NewSendString := NewSendString + ' ';  { Leading space }

            ControlD: IF TBSIQ_CW_Engine.CWBeingSent (Radio) THEN NewSendString := NewSendString + ' ';

            '@': BEGIN
                 { The old routine actually did the callsign update here - but I am not
                   going to support that out of the gate. }

                NewSendString := NewSendString + CallWindowString;
                END;

            ':': BEGIN   { Forget everything and setup to send CW from keyboard }
                 ExpandCrypticString := '';  { Make sure we don't try to send something }
                 PreviousQSOState := QSOState;
                 QSOState := QST_StartSendingKeyboardCW;
                 Exit;
                 END;

            '\': NewSendString := NewSendString + MyCall;

            '|': NewSendString := NewSendString + ReceivedData.Name;

            '{': NewSendString := NewSendString + ReceivedData.Callsign;

            '>': ClearRIT;

            { Not a special character - just add it as is }

            ELSE NewSendString := NewSendString + SendChar;
            END;
        END;

    ExpandCrypticString := NewSendString;
    END;



PROCEDURE QSOMachineObject.DisplayAutoSendCharacterCount;

    BEGIN
    CASE Radio OF
        RadioOne: SaveAndSetActiveWindow (TBSIQ_R1_StartSendingWindow);
        RadioTwo: SaveAndSetActiveWindow (TBSIQ_R2_StartSendingWindow);
        END;  { of case }

    ClrScr;

    IF (StartSendingCursorPosition > 0) AND AutoSendEnable THEN
        BEGIN
        GoToXY (StartSendingCursorPosition + 1, 1);
        Write ('|');
        END;

    RestorePreviousWindow;
    END;



PROCEDURE QSOMachineObject.ShowTransmitStatus;

{ Turns on the TX indicator }

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


PROCEDURE QSOMachineObject.CheckQSOStateMachine;

VAR Key, ExtendedKey: CHAR;
    ExpandedString, TempString, InitialExchange, Message, WindowString: STRING;
    ActionRequired: BOOLEAN;

    BEGIN
    UpdateRadioDisplay;  { Update radio band/mode/frequency }

    { Show new QSO state if diffrent and update TX indicator }

    IF QSOState <> LastQSOState THEN
        BEGIN
        ShowStateMachineStatus;
        LastQSOState := QSOState;
        ShowTransmitStatus;  { Update TX and cue indicators }

        IF (QSOState <> QST_SearchAndPounce) AND (QSOState <> QST_SearchAndPounceInit) THEN
            DisplayAutoSendCharacterCount;
        END;

    { Do not process any keystrokes while auto start send active on the
      other radio }

    CASE Radio OF
        RadioOne: IF Radio2QSOMachine.QSOState = QST_AutoStartSending THEN Exit;
        RadioTwo: IF Radio1QSOMachine.QSOState = QST_AutoStartSending THEN Exit;
        END;

    { Unlike the WindowEditor in LOGSUBS2, this will not block execution.  It will return
      right away with ActionRequired = FALSE if no key was pressed.  In the case of
      QSOState = SendingKeyboardCW, all keystrokes will be returned instantly. }

    { We want to get to some states before the WindowEditor is called }

    IF QSOState <> QST_StartSendingKeyboardCW THEN
        WindowEditor (WindowString, Key, ExtendedKey, ActionRequired);

    CASE QSOState OF

        QST_Idle, QST_CQCalled:
            BEGIN
            { Clear the auto start send station called flag if the CallWindow is empty }

            IF CallWindowString = '' THEN AutoStartSendStationCalled := False;

            { See if we have a keystroke to look at }

            IF ActionRequired THEN
                BEGIN
                IF (Key = Chr (0)) AND ValidFunctionKey (ExtendedKey) THEN  { Send function key message }
                    BEGIN
                    SendFunctionKeyMessage (ExtendedKey, Message);

                    { Maybe add F9 here? }

                    IF (ExtendedKey = F1) OR (ExtendedKey = F2) THEN
                        QSOState := QST_CallingCQ;

                    ShowCWMessage (Message);
                    Exit;
                    END;

                { Not a function key message }

                CASE Key OF
                    CarriageReturn:
                        IF TBSIQ_ActiveWindow = TBSIQ_CallWindow THEN
                            IF WindowString = '' THEN
                                BEGIN
                                SendFunctionKeyMessage (F1, Message);
                                ShowCWMessage (Message);
                                QSOState := QST_CallingCQ;
                                END
                            ELSE
                                BEGIN  { We have a callsign to send }
                                TBSIQ_CW_Engine.CueCWMessage (WindowString, Radio, CWP_High);
                                CallsignICameBackTo := WindowString;
                                ShowCWMessage (WindowString);
                                QSOState := QST_CQStationBeingAnswered;
                                END;
                    EscapeKey:
                        BEGIN
                        QSOState := QST_Idle;
                        ShowTransmitStatus;
                        END;

                    TabKey:
                        QSOState := QST_SearchAndPounceInit;

                    SpaceBar:
                        IF CallWindowString = '' THEN
                            QSOState := QST_SearchAndPounceSpaceBarPressed
                        ELSE
                            BEGIN
                            { Do a dupecheck on the callsign }

                            END;


                    END; { of case Key }

                END  { of QST_Idle and ActionRequired }
            ELSE

                { There are some actions taken just based upon entering a certain number of
                  letters in the CallWindow }

                BEGIN
                { Check to see if AutoStartSend should be triggered }

                IF AutoSendEnable AND (StartSendingCursorPosition > 0) AND (ActiveMode = CW) THEN
                    IF Length (CallWindowString) = AutoSendCharacterCount THEN
                        IF NOT StringIsAllNumbersOrDecimal (CallWindowString) THEN
                            IF NOT StringHas ('/', CallWindowString) THEN
                                IF NOT AutoStartSendStationCalled THEN
                                    BEGIN
                                    { We need to start sending the callsign }

                                    TBSIQ_CW_Engine.CueCWMessage (CallWindowString, Radio, CWP_High);
                                    CallsignICameBackTo := CallWindowString;
                                    ShowCWMessage (CallWindowString);
                                    QSOState := QST_AutoStartSending;
                                    AutoStartSendStationCalled := True;    { This makes sure we don't call again }
                                    END;

                { Check SuperCheckPartial }

                IF SCPMinimumLetters > 0 THEN
                    IF Length (CallWindowString) >= SCPMinimumLetters THEN
                        SuperCheckPartial;

                END;

            END;  { Of QST_Idle or QST_CQCalled }

        QST_AutoStartSending:  { We have started sending a callsign }
            BEGIN
            { There are a number of possible things that can happen here:

              1. Someone typed another letter to add to the callsign.
              2. Someone hit the backspace key to possibly delete an unsent letter.
              3. Someone hit escape with an empty window and wants to back up.
              4. Someone hit RETURN to end input of the callsign.
              5. All of the entered letters have been sent - send exchange. }

            IF TBSIQ_CW_Engine.CWFinished (Radio) THEN   { we are done sending the call }
                IF AutoCallTerminate THEN
                    BEGIN
                    ExpandedString := ExpandCrypticString (CQExchange);
                    TBSIQ_CW_Engine.CueCWMessage (ExpandedString, Radio, CWP_Urgent);
                    ShowCWMessage (ExpandedString);
                    QSOState := QST_CQExchangeBeingSent;
                    Exit;
                    END;

            IF NOT ActionRequired THEN Exit;  { No keystroke to respond to }

            CASE Key OF
                EscapeKey:
                    QSOState := QST_Idle;

                CarriageReturn:
                    BEGIN
                    ExpandedString := ExpandCrypticString (CQExchange);
                    TBSIQ_CW_Engine.CueCWMessage (ExpandedString, Radio, CWP_Urgent);
                    ShowCWMessage (ExpandedString);
                    QSOState := QST_CQExchangeBeingSent;
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
                        TBSIQ_CW_Engine.AddCharacterToBuffer (Key, Radio);
                        CallsignICameBackTo := CallSignICameBackTo + Key;
                        ShowCWMessage (CallsignICameBackTo);
                        END;
                    END;

                END; { of case Key }
            END;


        QST_CallingCQ:
            BEGIN
            ListenToOtherRadio;   { Is this okay if my message is in the cue? }

            IF TBSIQ_CW_Engine.CWFinished (Radio) THEN
                BEGIN
                ListenToBothRadios;
                ShowCWMessage ('');
                QSOState := QST_CQCalled;
                END;
            END;

        QST_CQStationBeingAnswered:
            BEGIN
            ListenToOtherRadio;

            { We used to not do this until CW was done }

            ExpandedString := ExpandCrypticString (CQExchange);
            TBSIQ_CW_Engine.CueCWMessage (ExpandedString, Radio, CWP_Urgent);
            ShowCWMessage (ExpandedString);
            QSOState := QST_CQExchangeBeingSent;
            END;

        { We are sending the CQ exchange to the guy who came back.  We can get the
          exchange window prepared for data entry - and put the initial exchange there if
          we have one }

        QST_CQExchangeBeingSent:
            BEGIN
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
                END;

            QSOState := QST_CQExchangeBeingSentAndExchangeWindowUp;
            END;

        { We are here if we have put up the exchange window and initial exchange - waiting
          for CW to be done - any edits made - and then RETURN to be pressed }

        QST_CQExchangeBeingSentAndExchangeWindowUp:
            BEGIN
            ListenToOtherRadio;

            IF ActionRequired THEN
                CASE Key OF
                    NullKey:
                        BEGIN
                        { on the fence about function key memories while the CQ Exchange is being sent }

                        IF ValidFunctionKey (ExtendedKey) THEN  { Send function key message }
                            BEGIN
                            SendFunctionKeyMessage (ExtendedKey, Message);
                            ShowCWMessage (Message);
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

                    CarriageReturn:
                        IF TBSIQ_ParametersOkay (CallWindowString, ExchangeWindowString, Band, Mode, Frequency, RData) THEN
                            BEGIN

                            IF NOT BeSilent THEN
                                BEGIN
                                { Let's build the string we need to send to acknowledge the QSO }

                                TempString := '';

                                { This ignores the partial callsign correction possibility }

                                IF (RData.Callsign <> CallsignICameBackTo) THEN
                                    BEGIN
                                    TempString := TempString + RData.Callsign + ' ';
                                    CallsignICameBackTo := RData.Callsign;
                                    END;

                                TempString := TempString + QSLMessage;

                                ExpandedString := ExpandCrypticString (TempString);
                                TBSIQ_CW_Engine.CueCWMessage (ExpandedString, Radio, CWP_High);
                                ShowCWMessage (ExpandedString);
                                END;

                            TBSIQ_LogContact (RData);
                            QSOState := QST_CQSending73Message;
                            END;

                    NullKey:
                        BEGIN
                        IF ValidFunctionKey (ExtendedKey) THEN  { Send function key message }
                            BEGIN
                            SendFunctionKeyMessage (ExtendedKey, Message);
                            ShowCWMessage (Message);
                            QSOState := QST_CQExchangeBeingSentAndExchangeWindowUp;
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
                        END;  { of NullKey}

                    END; { of CASE Key }
            END;  { of QST_WaitingForExchange }

        { QSO is logged and are sending the QSO Message }

        QST_CQSending73Message:
            BEGIN
            ListenToOtherRadio;

            { Moved this up here instead of waiting for CW to end }

            RemoveExchangeWindow;
            CallWindowString := '';
            CallWindowCursorPosition := 1;
            SetTBSIQWindow (TBSIQ_CallWindow);
            ClrScr;

            IF TBSIQ_CW_Engine.CWFinished (Radio) THEN
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
                        QSOState := PreviousQSOState;
                        Exit;
                        END;

                    ELSE
                        IF (Key >= ' ') AND (Key <= 'z') THEN
                            BEGIN
                            TBSIQ_CW_Engine.CueCWMessage (Key, Radio, CWP_High);
                            KeyboardCWMessage := KeyboardCWMessage + Key;
                            ShowCWMessage (KeyboardCWMessage);
                            END;

                    END;  { of CASE Key }
            END;

        QST_SearchAndPounceInit:  { Executed once when entering S&P Mode }
            BEGIN
            IF NOT TBSIQ_CW_Engine.CWFinished (Radio) THEN Exit;

            BandMapBand := DisplayedBand;
            DisplayBandMap;

            ClearAutoSendDisplay;

            ExchangeWindowString := '';
            ExchangeWindowCursorPosition := 1;

            SetTBSIQWindow (TBSIQ_ExchangeWindow);
            ClrScr;
            SetTBSIQWindow (TBSIQ_CallWindow);

            SearchAndPounceStationCalled := False;
            SearchAndPounceExchangeSent := False;
            QSOState := QST_SearchAndPounce;
            END;

        QST_SearchAndPounceSpaceBarPressed:
            BEGIN
            { We come here when someone pressed the SPACE BAR with an empty
              call window - so we need to jump into S&P mode and send my
              callsign. }

            ClearAutoSendDisplay;

            ExchangeWindowString := '';
            ExchangeWindowCursorPosition := 1;

            SetTBSIQWindow (TBSIQ_ExchangeWindow);
            ClrScr;
            SetTBSIQWindow (TBSIQ_CallWindow);

            SendFunctionKeyMessage (F1, Message);
            ShowCWMessage (Message);

            SearchAndPounceStationCalled := True;
            SearchAndPounceExchangeSent := False;
            QSOState := QST_SearchAndPounce;

            BandMapBand := DisplayedBand;
            DisplayBandMap;
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
                        ShowCWMessage (Message);
                        SearchAndPounceStationCalled := True;
                        END;

                    EscapeKey:   { Only get this if we have an empty string }
                        IF TBSIQ_ActiveWindow = TBSIQ_ExchangeWindow THEN
                            BEGIN
                            SetTBSIQWindow (TBSIQ_CallWindow);
                            SearchAndPounceExchangeSent := False;  { I guess }
                            END
                        ELSE
                            BEGIN
                            QSOState := QST_Idle;
                            RemoveExchangeWindow;
                            END;

                    CarriageReturn:
                        BEGIN
                        IF NOT SearchAndPounceStationCalled THEN
                            BEGIN
                            SendFunctionKeyMessage (F1, Message);
                            ShowCWMessage (Message);

                            IF CallWindowString <> '' THEN
                                BEGIN

                                { Not sure about doing anything here }

                                END;

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

                            SearchAndPounceStationCalled := True;
                            END

                        ELSE
                            BEGIN
                            { Send exchange if it hasn't alread been sent }

                            IF NOT SearchAndPounceExchangeSent THEN
                                BEGIN
                                ExpandedString := ExpandCrypticString (SearchAndPounceExchange);
                                TBSIQ_CW_Engine.CueCWMessage (ExpandedString, Radio, CWP_Urgent);
                                ShowCWMessage (ExpandedString);
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

                            IF TBSIQ_ParametersOkay (CallWindowString, ExchangeWindowString, Band, Mode, Frequency, RData) THEN
                                BEGIN
                                TBSIQ_LogContact (RData);

                                ExchangeWindowString := '';
                                ExchangeWindowCursorPosition := 1;
                                SetTBSIQWindow (TBSIQ_ExchangeWindow);
                                ClrScr;

                                CallWindowString := '';
                                CallWindowCursorPosition := 1;
                                SetTBSIQWindow (TBSIQ_CallWindow);
                                ClrScr;

                                QSOState := QST_SearchAndPounceInit;
                                END;
                            END;
                        END;  { of CarriageReturn }

                    NullKey:
                        BEGIN
                        IF ValidFunctionKey (ExtendedKey) THEN  { Send function key message }
                            BEGIN
                            SendFunctionKeyMessage (ExtendedKey, Message);
                            ShowCWMessage (Message);

                            IF ExtendedKey = F1 THEN
                                BEGIN
                                IF CallWindowString <> '' THEN
                                    BEGIN

                                    { Hmm - not sure I want to do anything }

                                    END;

                                SearchAndPounceStationCalled := True;
                                END;

                            IF ExtendedKey = F2 THEN
                                SearchAndPounceExchangeSent := True;
                            END

                        ELSE
                            CASE ExtendedKey OF
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
                                    END;

                                END;  { of CASE ExtendedKey }
                        END;

                    END; { of case Key }
                END; { of ActionRequired }
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
        END;
    END;



PROCEDURE QSOMachineObject.UpdateRadioDisplay;

{ Checks the radio band/mode/frequency and updates as appropriate }

VAR TempFreq: LONGINT;
    TempBand: BandType;
    TempMode: ModeType;

    BEGIN
    { Get the radio parameters for the proper radio }

    CASE Radio OF
        RadioOne:
            IF NOT GetRadioParameters (RadioOne, ' ', TempFreq, TempBand, TempMode, True, False) THEN
                BEGIN
                Write ('.');
                Frequency := 0;
                DisplayFrequency;
                Exit;
                END;

        RadioTwo:
            IF NOT GetRadioParameters (RadioTwo, ' ', TempFreq, TempBand, TempMode, True, False) THEN
                BEGIN
                Write ('.');
                Frequency := 0;
                DisplayFrequency;
                Exit;
                END;

        END;

    Frequency := TempFreq;
    Band := TempBand;
    Mode := TempMode;

    DisplayFrequency;
    DisplayBandMode;

    BandMemory [Radio] := Band;
    ModeMemory [Radio] := Mode;

    IF FrequencyMemoryEnable THEN
        FreqMemory [Band, Mode] := Frequency;
    END;



PROCEDURE QSOMachineObject.ShowCWMessage (Message: STRING);

    BEGIN
    CASE Radio OF
        RadioOne: SaveSetAndClearActiveWindow (TBSIQ_R1_CWMessageWindow);
        RadioTwo: SaveSetAndClearActiveWindow (TBSIQ_R2_CWMessageWindow);
        END;

    Write (Message);
    CWMessageDisplayed := Message;
    RestorePreviousWindow;
    END;



PROCEDURE QSOMachineObject.ShowStateMachineStatus;

    BEGIN
    CASE Radio OF
        RadioOne: SaveSetAndClearActiveWindow (TBSIQ_R1_StateMachineStatusWindow);
        RadioTwo: SaveSetAndClearActiveWindow (TBSIQ_R2_StateMachineStatusWindow);
        END;

    CASE QSOState OF
        QST_Idle: Write ('CQ Mode - Idle');
        QST_AutoStartSending: Write ('Auto start send started');
        QST_CallingCQ: Write ('CQing');
        QST_CQCalled: Write ('CQ Called');
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
    CWMessageDisplayed := '';
    AutoStartSendStationCalled := False;
    SearchAndPounceStationCalled := False;
    StartSendingCursorPosition := AutoSendCharacterCount;

    { Setup the window locations derived from the X,Y reference }

    CASE Radio OF
        RadioOne:
            BEGIN
            { BandMode - save level as Call Window - off to the left }

            TBSIQ_R1_BandModeWindowLX := WindowLocationX;
            TBSIQ_R1_BandModeWindowLY := WindowLocationY + 1;
            TBSIQ_R1_BandModeWindowRX := WindowLocationX + 7;
            TBSIQ_R1_BandModeWindowRY := WindowLocationY + 1;

            TBSIQ_R1_StartSendingWindowLX := WindowLocationX + 12;
            TBSIQ_R1_StartSendingWindowLY := WindowLocationY;
            TBSIQ_R1_StartSendingWindowRX := WindowLocationX + 18;
            TBSIQ_R1_StartSendingWindowRY := windowLocationY;

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
            TBSIQ_R1_CWMessageWindowLY := WindowLocationY + 5;
            TBSIQ_R1_CWMessageWindowRX := WindowLocationX + 37;
            TBSIQ_R1_CWMessageWindowRY := WindowLocationY + 5;

            TBSIQ_R1_ExchangeWindowLX := WindowLocationX + 13;
            TBSIQ_R1_ExchangeWindowLY := WindowLocationY + 3;
            TBSIQ_R1_ExchangeWindowRX := WindowLocationX + 34;
            TBSIQ_R1_ExchangeWindowRY := WindowLocationY + 3;

            { Just below BandMode }

            TBSIQ_R1_FrequencyWindowLX := WindowLocationX + 1;
            TBSIQ_R1_FrequencyWindowLY := WindowLocationY + 2;
            TBSIQ_R1_FrequencyWindowRX := WindowLocationX + 13;
            TBSIQ_R1_FrequencyWindowRY := WindowLocationY + 2;

            { Just below the exchange window }

            TBSIQ_R1_PossibleCallWindowLX := WindowLocationX;
            TBSIQ_R1_PossibleCallWindowLY := WindowLocationY + 4;
            TBSIQ_R1_PossibleCallWindowRX := WindowLocationX + 39;
            TBSIQ_R1_PossibleCallWindowRY := WindowLocationY + 4;

            { Just below the exchange window - overlaps possible calls }

            TBSIQ_R1_QuickCommandWindowLX := WindowLocationX;
            TBSIQ_R1_QuickCommandWindowLY := WindowLocationY + 4;
            TBSIQ_R1_QuickCommandWindowRX := WindowLocationX + 39;
            TBSIQ_R1_QuickCommandWindowRY := WindowLocationY + 4;

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

            TBSIQ_R1_UserInfoWindowLX := WindowLocationX + 14;
            TBSIQ_R1_UserInfoWindowLY := WindowLocationY + 4;
            TBSIQ_R1_UserInfoWindowRX := WindowLocationX + 25;
            TBSIQ_R1_UserInfoWindowRY := WindowLocationY + 4;
            END;

        RadioTwo:
            BEGIN
            { BandMode - save level as Call Window - off to the left }

            TBSIQ_R2_BandModeWindowLX := WindowLocationX;
            TBSIQ_R2_BandModeWindowLY := WindowLocationY + 1;
            TBSIQ_R2_BandModeWindowRX := WindowLocationX + 7;
            TBSIQ_R2_BandModeWindowRY := WindowLocationY + 1;

            TBSIQ_R2_StartSendingWindowLX := WindowLocationX + 12;
            TBSIQ_R2_StartSendingWindowLY := WindowLocationY;
            TBSIQ_R2_StartSendingWindowRX := WindowLocationX + 18;
            TBSIQ_R2_StartSendingWindowRY := windowLocationY;

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
            TBSIQ_R2_CWMessageWindowLY := WindowLocationY + 5;
            TBSIQ_R2_CWMessageWindowRX := WindowLocationX + 37;
            TBSIQ_R2_CWMessageWindowRY := WindowLocationY + 5;

            TBSIQ_R2_ExchangeWindowLX := WindowLocationX + 13;
            TBSIQ_R2_ExchangeWindowLY := WindowLocationY + 3;
            TBSIQ_R2_ExchangeWindowRX := WindowLocationX + 34;
            TBSIQ_R2_ExchangeWindowRY := WindowLocationY + 3;

            { Just below BandMode }

            TBSIQ_R2_FrequencyWindowLX := WindowLocationX + 1;
            TBSIQ_R2_FrequencyWindowLY := WindowLocationY + 2;
            TBSIQ_R2_FrequencyWindowRX := WindowLocationX + 13;
            TBSIQ_R2_FrequencyWindowRY := WindowLocationY + 2;

            { Just below the exchange window }

            TBSIQ_R2_PossibleCallWindowLX := WindowLocationX;
            TBSIQ_R2_PossibleCallWindowLY := WindowLocationY + 4;
            TBSIQ_R2_PossibleCallWindowRX := WindowLocationX + 39;
            TBSIQ_R2_PossibleCallWindowRY := WindowLocationY + 4;

            { Just below the exchange window - overlaps possible calls }

            TBSIQ_R2_QuickCommandWindowLX := WindowLocationX;
            TBSIQ_R2_QuickCommandWindowLY := WindowLocationY + 4;
            TBSIQ_R2_QuickCommandWindowRX := WindowLocationX + 39;
            TBSIQ_R2_QuickCommandWindowRY := WindowLocationY + 4;

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

            TBSIQ_R2_UserInfoWindowLX := WindowLocationX + 14;
            TBSIQ_R2_UserInfoWindowLY := WindowLocationY + 4;
            TBSIQ_R2_UserInfoWindowRX := WindowLocationX + 25;
            TBSIQ_R2_UserInfoWindowRY := WindowLocationY + 4;
            END;

        END;

    { These are the two windows we need to remember the string and cursor positions of }

    CallWindowString := '';
    CallWindowCursorPosition := 1;

    ExchangeWindowString := '';
    ExchangeWindowCursorPosition := 1;
    ExchangeWindowIsUp := False;

    DisplayAutoSendCharacterCount;

    CodeSpeed := SpeedMemory [Radio];
    DisplayCodeSpeed;

    { Put up a blank call window }

    SetTBSIQWindow (TBSIQ_CallWindow);
    ClrScr;

    LastQSOState := QST_None;
    QSOState := QST_Idle;
    ShowStateMachineStatus;

    DisplayedBand := NoBand;
    DisplayedMode := NoMode;
    DisplayedFrequency := 0;
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



PROCEDURE QSOMachineObject.DisplayCodeSpeed;

    BEGIN
    CASE Radio OF
        RadioOne: SaveSetAndClearActiveWindow (TBSIQ_R1_CodeSpeedWindow);
        RadioTwo: SaveSetAndClearActiveWindow (TBSIQ_R2_CodeSpeedWindow);
        END;

    Write (' ', CodeSpeed:2, ' WPM');

    IF ActiveRadio = Radio THEN
        Write (' TX');

    RestorePreviousWindow;
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
  first and change it to the right window before doing anything. }

VAR CursorPosition, CharPointer, Count: INTEGER;
    PreviousCursorChar: CHAR;
    InitialExchange, TempString: STRING;
    TempExchange: ContestExchange;

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

    IF NOT TBSIQ_KeyPressed (Radio) THEN Exit;  { No reason to be here }

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

    IF (QSOState = QST_StartSendingKeyboardCW) OR (QSOState = QST_SendingKeyboardCW) THEN
        Exit;  { All keystrokes handled there }

    { Check for keys that are variables and require action by caller }

    IF KeyChar = QuickQSLKey1 THEN Exit;
    IF KeyChar = QuickQSLKey2 THEN Exit;
    IF KeyChar = TailEndKey THEN Exit;

    { After the CASE statement - whatever WindowString is will be saved in either
      CallWindowString or ExchangeWindowString and also the cursor position in
      CallCursorPosition or ExchangeCursorPosition. }

    CASE KeyChar OF

        EscapeKey:
            BEGIN
            { The ESCAPE KEY is a panic key that will step things back one step for every
              press.  The first thing that needs to be checked is to see if there is a CW
              message being sent - or one in the cue - that needs to be aborted/deleted.  }

            IF TBSIQ_CW_Engine.ClearMessages (Radio, True) THEN   { was something to stop }
                BEGIN
                ActionRequired := False;
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
                VisibleLog.DoPossibleCalls (WindowString);
                END
            ELSE
                BEGIN
                ShowStationInformation (CallWindowString);
                DisplayGridSquareStatus (CallWindowString);
                VisibleLog.DoPossibleCalls (CallWindowString);
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
                IF InsertMode AND (CursorPosition <= Length (WindowString)) THEN  { Squeeze in new character }
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
                Exit;  { Let someone do a dupe check? }

        NullKey:
            BEGIN
            ExtendedKeyChar := TBSIQ_ReadKey (Radio);

            { Many Altkeys that hardly ever get used are currently not here. See the
              WindowEditor in LOGSUBS2.PAS if you want to put some of them back in }

            CASE ExtendedKeyChar OF

                AltE: BEGIN
                      RITEnable := False;
                      VisibleLog.EditLog;
                      RITEnable := True;
                      UpdateTotals;
                      VisibleLog.ShowRemainingMultipliers;
                      VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);
{                     DisplayTotalScore (TotalScore); }
                      LastTwoLettersCrunchedOn := '';

                      IF VisibleDupeSheetEnable THEN
                          BEGIN
                          VisibleDupeSheetChanged := True;
                          VisibleLog.DisplayVisibleDupeSheet (ActiveBand, ActiveMode);
                          END;

                      VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);

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
                      MemoryProgram;
                      RITEnable := True;
                      VisibleLog.SetUpEditableLog;
                      ClearKeyCache := True;
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
                      SpeedMemory [Radio] := CodeSpeed;
                      DisplayCodeSpeed;

                      { Only set the speed immediately if the other radio isn't sending something }

                      CASE Radio OF
                          RadioOne:
                              IF TBSIQ_CW_Engine.CWFinished (RadioTwo) THEN
                                  BEGIN
                                  { Fake SetUpToSendOnActiveRadio to do an update }

                                  SendingOnRadioOne := False;
                                  SendingOnRadioTwo := False;
                                  SetUpToSendOnActiveRadio;  { Maybe not a good idea }
                                  END;

                          RadioTwo:
                              IF TBSIQ_CW_Engine.CWFinished (RadioOne) THEN
                                  BEGIN
                                  { Fake SetUpToSendOnActiveRadio to do an update }

                                  SendingOnRadioOne := False;
                                  SendingOnRadioTwo := False;
                                  SetUpToSendOnActiveRadio;  { Maybe not a good idea }
                                  END;

                          END;  { of case }

                      ClearKeyCache := True;
                      END;

                AltT: BEGIN
                      TimeAndDateSet;
                      ClearKeyCache := True;
                      END;

                AltX: BEGIN
                      TBSIQ_ExitProgram;
                      ClearKeyCache := True;
                      END;

                AltY: TBSIQ_DeleteLastContact;

                AltZ:
                    BEGIN  { Well - let's see if this works okay down here }

                    InitialExchange := InitialExchangeEntry (CallWindowString);

                    IF InitialExchange <> '' THEN   { Hmm - maybe this test should go away }
                        BEGIN
                        IF TBSIQ_ActiveWindow = TBSIQ_CallWindow THEN
                            BEGIN
                            SetTBSIQWindow (TBSIQ_ExchangeWindow);
                            ExchangeWindowString := InitialExchange;
                            ExchangeWindowCursorPosition := Length (ExchangeWindowString) + 1;
                            ClrScr;
                            Write (ExchangeWindowString);
                            SetTBSIQWindow (TBSIQ_CallWindow);
                            END
                        ELSE
                            BEGIN
                            WindowString := InitialExchange;
                            CursorPosition := Length (ExchangeWindowString) + 1;
                            ClrScr;
                            Write (WindowString);
                            END;
                        END;
                    END; { of AltZ }

                AltDash:   { Uses the global AutoSendEnable }
                    BEGIN
                    IF AutoSendCharacterCount > 0 THEN
                        AutoSendEnable := NOT AutoSendEnable;

                    DisplayAutoSendCharacterCount;
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
                    IF InsertMode THEN
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
                    IF CodeSpeed < 99 - CodeSpeedIncrement THEN
                        BEGIN
                        CodeSpeed := CodeSpeed + CodeSpeedIncrement;
                        DisplayCodeSpeed;
                        SpeedMemory [Radio] := CodeSpeed;

                        { Only set the speed immediately if the other radio isn't sending something }

                        CASE Radio OF
                            RadioOne:
                                IF TBSIQ_CW_Engine.CWFinished (RadioTwo) THEN
                                    BEGIN
                                    { Fake SetUpToSendOnActiveRadio to do an update }

                                    SendingOnRadioOne := False;
                                    SendingOnRadioTwo := False;
                                    SetUpToSendOnActiveRadio;  { Maybe not a good idea }
                                    END;

                            RadioTwo:
                                IF TBSIQ_CW_Engine.CWFinished (RadioOne) THEN
                                    BEGIN
                                    { Fake SetUpToSendOnActiveRadio to do an update }

                                    SendingOnRadioOne := False;
                                    SendingOnRadioTwo := False;
                                    SetUpToSendOnActiveRadio;  { Maybe not a good idea }
                                    END;
                            END;  { of case }
                        END;

                PageDownKey:
                    IF CodeSpeed > 1 + CodeSpeedIncrement THEN
                        BEGIN
                        CodeSpeed := CodeSpeed - CodeSpeedIncrement;
                        DisplayCodeSpeed;
                        SpeedMemory [Radio] := CodeSpeed;

                        { Only set the speed immediately if the other radio isn't sending something }

                        CASE Radio OF
                            RadioOne:
                                IF TBSIQ_CW_Engine.CWFinished (RadioTwo) THEN
                                    BEGIN
                                    { Fake SetUpToSendOnActiveRadio to do an update }

                                    SendingOnRadioOne := False;
                                    SendingOnRadioTwo := False;
                                    SetUpToSendOnActiveRadio;  { Maybe not a good idea }
                                    END;

                            RadioTwo:
                                IF TBSIQ_CW_Engine.CWFinished (RadioOne) THEN
                                    BEGIN
                                    { Fake SetUpToSendOnActiveRadio to do an update }

                                    SendingOnRadioOne := False;
                                    SendingOnRadioTwo := False;
                                    SetUpToSendOnActiveRadio;  { Maybe not a good idea }
                                    END;
                            END;  { of case }
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
                        DisplayInsertMode (InsertMode);
                        DisplayNextQSONumber (TotalContacts + 1);
                        LastTwoLettersCrunchedOn := '';
                        ClearKeyCache := True;
                        END
                    ELSE
                        Exit;     { Let the calling routine decide what to do }

                { It appears ControlDownArrow was used to move the Grid Map down }

                DownArrow: Exit;

                InsertKey:
                    BEGIN
                    InsertMode := NOT InsertMode;
                    DisplayInsertMode (InsertMode);
                    END;

                { Any other ExtendedKeys not captured here will be dealt with by the calling Procedre }

                ELSE Exit;

                END;  { of case }
            END; { of ExtendedKey }

        ELSE  { Likely we hae a "normal" key that just needs to be added to the window }

            IF LegalKey (KeyChar) THEN
                BEGIN
                IF InsertMode AND (CursorPosition <= Length (WindowString)) THEN  { Squeeze in new character }
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
            RemoveWindow (TBSIQ_R1_ExchangeWindow);

        RadioTwo:
            RemoveWindow (TBSIQ_R2_ExchangeWindow);

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

        { I am not going to deal with Control or Shift Enter right now - but feel free }
        { Back in the DOS version - I was reading a port directly to detect them }

        28: BEGIN                               { Carriage Return }
            KeyStatus.KeyChar := CarriageReturn;
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
                KeyStatus.KeyChar := '`'
            ELSE
                KeyStatus.KeyChar := '~';

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

    IF NOT TBSIQ_KeyPressed (Radio) THEN
        Write ('***');

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



FUNCTION ValidFunctionKey (Key: CHAR): BOOLEAN;

{ Call with the extended key char }

    BEGIN
    ValidFunctionKey :=
       ((Key >= F1)         AND (Key <= F10)) OR
       ((Key >= F11)        AND (Key <= F12));

{       OR
       ((Key >= ShiftF1)    AND (Key <= ShiftF10)) OR
       ((Key >= ShiftF11)   AND (Key <= ShiftF12)) OR
       ((Key >= ControlF1)  AND (Key <= ControlF10)) OR
       ((Key >= ControlF11) AND (Key <= ControlF12)) OR
       ((Key >= AltF1)      AND (Key <= AltF10)) OR
       ((Key >= AltF11)     AND (Key <= AltF12));}
    END;



PROCEDURE QSOMachineObject.SendFunctionKeyMessage (Key: CHAR; VAR Message: STRING);

    BEGIN
    IF NOT ValidFunctionKey (Key) THEN Exit;

    IF (QSOState = QST_Idle) OR (QSOState = QST_CallingCQ) OR
       (QSOState = QST_CQCalled) OR (QSOState = QST_AutoStartSending) OR
       (QSOState = QST_CQSending73Message) THEN
           Message := GetCQMemoryString (CW, Key)
       ELSE
           BEGIN
           IF Key = F1 THEN
               Message := '\'
           ELSE
               IF Key = F2 THEN
                   Message := SearchAndPounceExchange
               ELSE
                   Message := GetEXMemoryString (CW, Key);
           END;

    Message := ExpandCrypticString (Message);

    IF Message <> '' THEN;
        TBSIQ_CW_Engine.CueCWMessage (Message, Radio, CWP_High);
    END;



FUNCTION TBSIQ_ParametersOkay (Call: CallString;
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
        RData.NumberSent := TotalContacts + 1;
        RData.Frequency  := Freq;

        IF Mode = PHONE THEN
            DefaultRST := '59'
        ELSE
            DefaultRST := '599';

        IF RData.RSTSent = '' THEN
            IF ActiveMode = Phone THEN RData.RSTSent := LogRSSent
            ELSE RData.RSTSent := LogRSTSent;

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
    RData.NumberSent := TotalContacts + 1;
    RData.Frequency  := Freq;

    IF RData.RSTSent = '' THEN
        IF ActiveMode = Phone THEN RData.RSTSent := LogRSSent
        ELSE RData.RSTSent := LogRSTSent;

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

    IF ExchangeErrorMessage <> '' THEN
        BEGIN
        QuickDisplayError (ExchangeErrorMessage);
        ReminderPostedCount := 60;
        END;

    CalculateQSOPoints (RData);
    END;



PROCEDURE TBSIQ_LogContact (VAR RXData: ContestExchange);

{ This procedure will log the contact just completed.  It will be
  pushed onto the editable log and the log entry popped off the editable
  log will be examined and written to the LOG.DAT file.                 }

VAR LogString: Str80;

    BEGIN
    VisibleDupeSheetChanged := True;

    LastTwoLettersCrunchedOn := '';

    IF LastDeletedLogEntry <> '' THEN
        BEGIN
        LastDeletedLogEntry := '';
        RemoveWindow (QuickCommandWindow);
        END;

    WindowDupeCheckCall := RXData.Callsign;

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

    IF BandMapEnable THEN
        BEGIN
        UpdateBandMapMultiplierStatus;
        UpdateBandMapDupeStatus (RXData.Callsign, RXData.Band, RXData.Mode, True);
        END;
    END;



PROCEDURE TBSIQ_PushLogStringIntoEditableLogAndLogPopedQSO (LogString: Str80; MyQSO: BOOLEAN);

VAR RData: ContestExchange;

    BEGIN
    LogString := VisibleLog.PushLogEntry (LogString);

    { LogString is now what popped off the top of the editable window }

    GetRidOfPostcedingSpaces (LogString);

    IF LogString <> '' THEN
        BEGIN
        TBSIQ_PutContactIntoLogFile (LogString);

        IF ParseExchangeIntoContestExchange (LogString, RData) THEN
            ProcessPartialCallAndInitialExchange (RData);
        END;
    END;



PROCEDURE TBSIQ_PutContactIntoLogFile (LogString: Str80);

VAR Time, QSONumber: INTEGER;
    Call, Exchange, LoggedCallsign: Str20;
    FileWrite: TEXT;

    BEGIN
    IF Copy (LogString, 1, 1) = ';' THEN
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





    BEGIN
    ResetKeyStatus (RadioOne);
    ResetKeyStatus (RadioTwo);
    END.
