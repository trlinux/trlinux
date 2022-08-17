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

UNIT TwoBSIQ;

{$O+}
{$V-}

{

TODO LIST

- There are some corner cases with auto start send edits while the other Tx
  is sending that need to be worked out.  This liekly won't be an issue
  for most of the time - so maybe wait to see if it ever actually causes a
  problem.  Maybe the bug actually doesn't exist...

- S&P mode work to be done: Need to make F1 and F2 work in S&P mode

CHANGE LOG

14-Aug-2022 (or before)

  - Added TX to the CW Speed display to show which rig has the TX focus if you
    start sending by hand.  You can change it to the other rig with AltR (on
    either keyboard).

15-Aug-2022

  - Made PageUp/PageDn work to change CW speeds in real time
  - Made TX indicator stay when changing code speed
  - Enabled numeric keypad PageUp/Dn keys to control CW speed
  - Started implemention of SearchAndPounce Mode - not completed yet.

16-Aug-2022

  - Implemented AltZ to recompute initial exchange based upon call window.
    This perhaps works a little different from AltZ in the main program
    as you can use it in any QSOState.  If the exchange window is not up,
    it will put it up for you but not change the QSO state.  It will only
    put up an intiial exchange if there is one for the callsign - otherwise,
    it will leave the Exchange Window alone.  Not sure if this is the best
    way - but give it a try.

  - Fixed a few places ClearKeyCache needed to be set - like when exiting
    the ControlJ menu.  This flag will gobble up any characters
    that are processed by the "normal" keyboard interface (which bypasses
    totally the two keyboard process).

  - Verified that CallsignUpdateEnable works - and made the exchange window
    wider so there was room for doing that.

  - Cleaned up display (Call and Exchange Windows) while QSL Message being
    sent instead of waiting for it to end.

  - Supporting a reduced set of cryptic CW characters.  # is currently a
    concept not implemented yet.  Have _ (leading space), @ (call window
    string with no fancy stuff), : (send keyboard input which kind of works
    but messes up the display - so needs work), \ (send MyCall), | (send
    RX data name), LeftCurlyBracket (send RX data callsign) and > (clear RIT).
    These are used in any of the function key memories or things like CQ
    EXCHANGE and QSL MESSAGE.

  - Fixed bug that would stop CW from being sent if the CW speed was changed
    on the other radio.

}

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

    { Hack to eliminate AutoStartSend cursor if it is there }

    SetWindow (WholeScreenWindow);
    GoToXY (CallWindowLX, CallWindowLY - 1);
    Write ('          ');

    { Create specific instances of the two objects }

    Radio1QSOMachine := QSOMachineObject.Create;
    Radio2QSOMachine := QSOMachineObject.Create;

    { Set up specific data for each instance of the QSOMachines.  The two
      numbers are the X Y reference points for the radio specific display }

    Radio1QSOMachine.InitializeQSOMachine (R1KeyboardID, RadioOne, 1, 19);
    Radio2QSOMachine.InitializeQSOMachine (R2KeyboardID, RadioTwo, 40, 19);

    ActiveRadio := RadioOne;
    SetUpToSendOnActiveRadio;
    TBSIQ_CW_Engine.ShowActiveRadio;
    END;



PROCEDURE Do2BSIQ;

{ This is the main loop at the highest level for 2BSIQ }

    BEGIN
    REPEAT
        MilliSleep;     { This seems necessary or radio display doesn't work }
        TBSIQ_CW_Engine.CheckMessages;
        TBSIQ_UpdateTimeAndrateDisplays;
        Radio1QSOMachine.CheckQSOStateMachine;
        Radio2QSOMachine.CheckQSOStateMachine;
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
    WriteLn ('This screen will refresh your editable log as you make QSOs');

    Initialize2BSIQOperatorInterface;
    Do2BSIQ;
    END;



    BEGIN
    END.
