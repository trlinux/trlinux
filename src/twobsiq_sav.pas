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

UNIT TwoBSIQ;

{$O+}
{$V-}

INTERFACE

USES Dos, Tree, LogWind, LogDupe, LogStuff, ZoneCont, Country9,
     LogCW, LogDVP, LogDom, Printer, LogK1EA, LogHelp, LogGrid, trCrt,
     timer,LogSCP,datetimec,radio,KeyCode,TBSIQ_CW;

PROCEDURE TwoBandSIQ;

IMPLEMENTATION

USES TBSIQ_Subs;

PROCEDURE Initialize2BSIQOperatorInterface;

    BEGIN
    { Remove windows below the editable log window so we have room }

    RemoveWindow (CallWindow);
    RemoveWindow (CodeSpeedWindow);
    RemoveWindow (ExchangeWindow);
    RemoveWindow (QuickCommandWindow);
    RemoveWindow (DateWindow);
    RemoveWindow (BandModeWindow);
    RemoveWindow (FunctionKeyWindow);
    RemoveWindow (FreeMemoryWindow);
    RemoveWindow (FrequencyOneWindow);
    RemoveWindow (FrequencyTwoWindow);
    RemoveWindow (HourRateWindow);
    RemoveWindow (InsertWindow);
    RemoveWindow (NamePercentageWindow);
    RemoveWindow (PossibleCallWindow);
    RemoveWindow (QSONumberWindow);
    RemoveWindow (QuickCommandWindow);
    RemoveWindow (RadioWindow);
    RemoveWindow (RateWindow);
    RemoveWindow (UserInfoWindow);

    { Create specific instances of the two objects }

    Radio1QSOMachine := QSOMachineObject.Create;
    Radio2QSOMachine := QSOMachineObject.Create;

    { Set up specific data for each instance of the QSOMachines.  The two
      numbers are the X Y reference points for the radio specific display }

    Radio1QSOMachine.InitializeQSOMachine (R1KeyboardID, RadioOne, 1, 19);
    Radio2QSOMachine.InitializeQSOMachine (R2KeyboardID, RadioTwo, 40, 19);
    END;



PROCEDURE TBSIQ_CheckEverything;

{ A placeholder for my own version of CheckEverything specific to TBSIQ.  This will
  get called very often so all of the state machines at work get a chance to update }

    BEGIN
    TBSIQ_CW_Engine.CheckMessages;
    TBSIQ_UpdateTimeAndrateDisplays;
    END;



PROCEDURE Do2BSIQ;

VAR Key, ExtendedKey: CHAR;
    Message, WindowString: STRING;
    ActionRequired: BOOLEAN;
    MessageNumber: INTEGER;

    BEGIN
    REPEAT
        MilliSleep;  { This seems necessary or radio display doesn't work }

        TBSIQ_CheckEverything;  { Generic (non-radio specific) routines that need oxygen }

        { Unlike what you see in LOGSUBS2, WindowEditor here is non-blocking.  If there is
          no key pressed, it will return instantly with ActionRequired set to FALSE. }

        WITH Radio1QSOMachine DO
            BEGIN
            UpdateRadioDisplay;  { Update randio band/mode/frequency }

            WindowEditor (WindowString, Key, ExtendedKey, ActionRequired);

            CASE Radio1QSOMachine.QSOState OF

                QST_Idle, QST_CQCalled:
                    IF ActionRequired THEN
                        BEGIN
                        IF (Key = Chr (0)) AND ValidFunctionKey (ExtendedKey) THEN  { Send function key message }
                            BEGIN
                            TBSIQ_SendFunctionKeyMessage (Radio, ExtendedKey, CW, CQOpMode, Message);
                            IF (ExtendedKey = F1) OR (ExtendedKey = F2) THEN
                                BEGIN
                                QSOState := QST_CallingCQ;
                                ShowStateMachineStatus;
                                END;

                            ShowCWMessage (Message);
                            Continue;
                            END;

                        { Not a function key message }

                        CASE Key OF
                            CarriageReturn:
                                IF TBSIQ_ActiveWindow = TBSIQ_CallWindow THEN
                                    IF WindowString = '' THEN
                                        BEGIN
                                        TBSIQ_SendFunctionKeyMessage (Radio, F1, CW, CQOpMode, Message);
                                        ShowCWMessage (Message);
                                        QSOState := QST_CallingCQ;
                                        ShowStateMachineStatus;
                                        END
                                    ELSE
                                        BEGIN  { We have a callsign to send }
                                        TBSIQ_CW_Engine.CueCWMessage (WindowString, Radio, CWP_High, MessageNumber);
                                        ShowCWMessage (WindowString);
                                        QSOState := QST_CQStationBeingAnswered;
                                        ShowStateMachineStatus;
                                        END;
                            EscapeKey:
                                QSOState := QST_Idle;

                            END; { of case Key }

                        END; { of QST_Idle and ActionRequired }

                QST_CallingCQ:
                    BEGIN
                    IF TBSIQ_CW_Engine.CWFinished (Radio) THEN
                        BEGIN
                        ShowCWMessage ('');
                        QSOState := QST_CQCalled;
                        ShowStateMachineStatus;
                        END;
                    END;

                QST_CQStationBeingAnswered:
                    BEGIN
                    { See if a function key was pressed to send an Exchange message }

                    IF ActionRequired AND (Key = Chr (0)) AND ValidFunctionKey (ExtendedKey) THEN  { Send function key message }
                        BEGIN
                        TBSIQ_SendFunctionKeyMessage (Radio, ExtendedKey, CW, SearchAndPounceOpMode, Message);
                        ShowCWMessage (Message);
                        Continue;
                        END;

                    { Waiting for the CW of the call who answered me to finish }

                    IF TBSIQ_CW_Engine.CWFinished (Radio) THEN
                        BEGIN
                        TBSIQ_CW_Engine.CueCWMessage (CQExchange, Radio, CWP_High, MessageNumber);
                        ShowCWMessage (CQExchange);
                        QSOState := QST_CQExchangeBeingSent;
                        ShowStateMachineStatus;
                        END;
                    END;

                { We are sending the CQ exchange to the guy who came back.  We can get the
                  exchange window prepared for data entry - and put the initial exchange there if
                  we have one }

                QST_CQExchangeBeingSent:
                    BEGIN
                    { Put up exchange window and any initial exchange }

                    IF TBSIQ_Activewindow = TBSIQ_CallWindow THEN
                        ActivateExchangeWindow;

                    QSOState := QST_CQExchangeBeingSentAndExchangeWindowUp;
                    ShowStateMachineStatus;
                    END;

                { We are here if we have put up the exchange window and initial exchange - waiting
                  for CW to be done - any edits made - and then RETURN to be pressed }

                QST_CQExchangeBeingSentAndExchangeWindowUp:
                    BEGIN
                    { See if a function key was pressed to send an Exchange message }

                    IF ActionRequired AND (Key = Chr (0)) AND ValidFunctionKey (ExtendedKey) THEN  { Send function key message }
                        BEGIN
                        TBSIQ_SendFunctionKeyMessage (Radio, ExtendedKey, CW, SearchAndPounceOpMode, Message);
                        ShowCWMessage (Message);
                        Continue;
                        END;

                    IF TBSIQ_CW_Engine.CWFinished (Radio) THEN
                        BEGIN
                        QSOState := QST_CQWaitingForExchange;
                        ShowStateMachineStatus;
                        END;
                    END;

                { Am waiting for a RETURN to be pressed and have someting that looks legit in both
                  the CallWindow and ExchangeWindow that I can log }

                QST_CQWaitingForExchange:
                    BEGIN
                    { See if a function key was pressed to send an Exchange message }

                    IF ActionRequired AND (Key = Chr (0)) AND ValidFunctionKey (ExtendedKey) THEN  { Send function key message }
                        BEGIN
                        TBSIQ_SendFunctionKeyMessage (Radio, ExtendedKey, CW, SearchAndPounceOpMode, Message);
                        ShowCWMessage (Message);
                        Continue;
                        END;

                    IF Key = CarriageReturn THEN  { See if we can log this QSO }
                        BEGIN
                        IF TBSIQ_ParametersOkay (CallWindowString, ExchangeWindowString, Band, Mode, Frequency, RData) THEN
                            BEGIN
                            TBSIQ_CW_Engine.CueCWMessage (QSLMessage, Radio, CWP_High, MessageNumber);
                            ShowCWMessage (QSLMessage);
                            TBSIQ_LogContact (RData);
                            QSOState := QST_CQSending73Message;
                            ShowStateMachineStatus;
                            END;
                        END;
                    END;

                { The QSO is logged and we are sending whatever it is we need to send to finish
                  the QSO }

                QST_CQSending73Message:
                    BEGIN
                    IF TBSIQ_CW_Engine.CWFinished (Radio) THEN
                        BEGIN
                        ShowCWMessage ('');
                        QSOState := QST_Idle;
                        ShowStateMachineStatus;
                        RemoveExchangeWindow;

                        CallWindowString := '';
                        CallWindowCursorPosition := 1;
                        Set_TBSIQ_Window (TBSIQ_CallWindow);
                        ClrScr;
                        END;
                    END;

                END;  { of case QSOState }
            END; { with Radio 1 }

        { Check on the second radio }

        WITH Radio2QSOMachine DO
            BEGIN
            UpdateRadioDisplay;  { Update randio band/mode/frequency }

            WindowEditor (WindowString, Key, ExtendedKey, ActionRequired);

            CASE Radio2QSOMachine.QSOState OF

                QST_Idle, QST_CQCalled:
                    IF ActionRequired THEN
                        BEGIN
                        IF (Key = Chr (0)) AND ValidFunctionKey (ExtendedKey) THEN  { Send function key message }
                            BEGIN
                            TBSIQ_SendFunctionKeyMessage (Radio, ExtendedKey, CW, CQOpMode, Message);
                            IF (ExtendedKey = F1) OR (ExtendedKey = F2) THEN
                                BEGIN
                                QSOState := QST_CallingCQ;
                                ShowStateMachineStatus;
                                END;

                            ShowCWMessage (Message);
                            Continue;
                            END;

                        { Not a function key message }

                        CASE Key OF
                            CarriageReturn:
                                IF TBSIQ_ActiveWindow = TBSIQ_CallWindow THEN
                                    IF WindowString = '' THEN
                                        BEGIN
                                        TBSIQ_SendFunctionKeyMessage (Radio, F1, CW, CQOpMode, Message);
                                        ShowCWMessage (Message);
                                        QSOState := QST_CallingCQ;
                                        ShowStateMachineStatus;
                                        END
                                    ELSE
                                        BEGIN  { We have a callsign to send }
                                        TBSIQ_CW_Engine.CueCWMessage (WindowString, Radio, CWP_High, MessageNumber);
                                        ShowCWMessage (WindowString);
                                        QSOState := QST_CQStationBeingAnswered;
                                        ShowStateMachineStatus;
                                        END;

                            EscapeKey:
                                QSOState := QST_Idle;

                            END; { of case Key }

                        END; { of QST_Idle and ActionRequired }

                QST_CallingCQ:
                    BEGIN
                    IF TBSIQ_CW_Engine.CWFinished (Radio) THEN
                        BEGIN
                        ShowCWMessage ('');
                        QSOState := QST_CQCalled;
                        ShowStateMachineStatus;
                        END;
                    END;

                QST_CQStationBeingAnswered:
                    BEGIN
                    { See if a function key was pressed to send an Exchange message }

                    IF ActionRequired AND (Key = Chr (0)) AND ValidFunctionKey (ExtendedKey) THEN  { Send function key message }
                        BEGIN
                        TBSIQ_SendFunctionKeyMessage (Radio, ExtendedKey, CW, SearchAndPounceOpMode, Message);
                        ShowCWMessage (Message);
                        Continue;
                        END;

                    { Waiting for the CW of the call who answered me to finish }

                    IF TBSIQ_CW_Engine.CWFinished (Radio) THEN
                        BEGIN
                        TBSIQ_CW_Engine.CueCWMessage (CQExchange, Radio, CWP_High, MessageNumber);
                        ShowCWMessage (CQExchange);
                        QSOState := QST_CQExchangeBeingSent;
                        ShowStateMachineStatus;
                        END;
                    END;

                { We are sending the CQ exchange to the guy who came back.  We can get the
                  exchange window prepared for data entry - and put the initial exchange there if
                  we have one }

                QST_CQExchangeBeingSent:
                    BEGIN
                    { Put up exchange window and any initial exchange }

                    IF TBSIQ_Activewindow = TBSIQ_CallWindow THEN
                        ActivateExchangeWindow;

                    QSOState := QST_CQExchangeBeingSentAndExchangeWindowUp;
                    ShowStateMachineStatus;
                    END;

                { We are here if we have put up the exchange window and initial exchange - waiting
                  for CW to be done - any edits made - and then RETURN to be pressed }

                QST_CQExchangeBeingSentAndExchangeWindowUp:
                    BEGIN
                    { See if a function key was pressed to send an Exchange message }

                    IF ActionRequired AND (Key = Chr (0)) AND ValidFunctionKey (ExtendedKey) THEN  { Send function key message }
                        BEGIN
                        TBSIQ_SendFunctionKeyMessage (Radio, ExtendedKey, CW, SearchAndPounceOpMode, Message);
                        ShowCWMessage (Message);
                        Continue;
                        END;

                    IF TBSIQ_CW_Engine.CWFinished (Radio) THEN
                        BEGIN
                        QSOState := QST_CQWaitingForExchange;
                        ShowStateMachineStatus;
                        END;
                    END;

                { Am waiting for a RETURN to be pressed and have someting that looks legit in both
                  the CallWindow and ExchangeWindow that I can log }

                QST_CQWaitingForExchange:
                    BEGIN
                    { See if a function key was pressed to send an Exchange message }

                    IF ActionRequired AND (Key = Chr (0)) AND ValidFunctionKey (ExtendedKey) THEN  { Send function key message }
                        BEGIN
                        TBSIQ_SendFunctionKeyMessage (Radio, ExtendedKey, CW, SearchAndPounceOpMode, Message);
                        ShowCWMessage (Message);
                        Continue;
                        END;

                    IF Key = CarriageReturn THEN  { See if we can log this QSO }
                        BEGIN
                        IF TBSIQ_ParametersOkay (CallWindowString, ExchangeWindowString, Band, Mode, Frequency, RData) THEN
                            BEGIN
                            TBSIQ_CW_Engine.CueCWMessage (QSLMessage, Radio, CWP_High, MessageNumber);
                            ShowCWMessage (QSLMessage);
                            TBSIQ_LogContact (RData);
                            QSOState := QST_CQSending73Message;
                            ShowStateMachineStatus;
                            END;
                        END;
                    END;

                { The QSO is logged and we are sending whatever it is we need to send to finish
                  the QSO }

                QST_CQSending73Message:
                    BEGIN
                    IF TBSIQ_CW_Engine.CWFinished (Radio) THEN
                        BEGIN
                        ShowCWMessage ('');
                        QSOState := QST_Idle;
                        ShowStateMachineStatus;
                        RemoveExchangeWindow;
                        CallWindowString := '';
                        CallWindowCursorPosition := 1;
                        Set_TBSIQ_Window (TBSIQ_CallWindow);
                        ClrScr;
                        END;
                    END;

                END;  { of case QSOState }
            END; { of with Radio2 }
    UNTIL False;
    END;



PROCEDURE TwoBandSIQ;

    BEGIN
    SetActiveWindow (EditableLogWindow);
    ClrScr;
    WriteLn ('Welcome to the TR Log 2BSIQ Program - Enter at your own peril.');

    IF NOT InitializeKeyboards THEN
        BEGIN
        Writeln ('Keyboards not properly initialized.');
        WHILE KeyPressed DO ReadKey;
        WaitForKeyPressed;
        Exit;
        END;

    ClrScr;
    WriteLn ('Welcome to the TR Log 2BSIQ Program - Enter at your own peril.');
    WriteLn;
    WriteLn ('You will now be in 2BSIQ until you exit the program and restart.');

    Initialize2BSIQOperatorInterface;
    Do2BSIQ;
    END;



    BEGIN
    END.
