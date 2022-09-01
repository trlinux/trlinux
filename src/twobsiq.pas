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

- "ActiveBand" cursor on the band totals display is not right.

- Occasionally, the auto start send doesn't start if I am busy on the other radio.
  Maybe fixed - see 31-Aug.

- Insert mode display.

- Implement ShowStationInformation

- Implement PossibleCalls

- Anyway to quickly turn on monitor tone for manual sending?  either automagic or manual

- When starting AutoStartSend when the other radio is sending CW - the indicator is
  yellow until the exchange starts.  This isn't a big deal and perhaps more trouble
  than it is worth to fix it.

 - Add some kind of display of next QSO number which can be used for SSB.

CHANGE LOG

31-Aug-200

  - Came up with my prototype for doing QSO Numbers.  The key to this is a text file
    called SERIALNUMBER.TXT.  If there is no file, then the first QSO number that will
    be sent will be #1.  Whenever a new QSO number is sent - it is assigned to that
    QSO that is progress.  For each - and entry into the SERIALNUMBER.TXT file will be
    made that looks like this:

    1 K3LR 7027940 40 CW
    2 N2NT 14035700 20 CW

    If a QSO ends up not getting logged - the entry stays in this file and the next
    QSO number will be fetched when needed.

    This file will be looked at when the program starts up to determine what the next
    QSO number should be.  It will be one plus that last number in the file.

    Using the # in the exchange memories during the QSO before it is logged will repeat
    the same number that was originally sent.


  - Made AutoStartSend start even if the call window string is > than the send
    count.  Not sure it matters - but perhaps addresses the bug where occasionally
    auto start send doesn't start when busy with the other radio.  I could not
    reproduce this failure in the lab - so not really sure it fixed anything.

  - Fixed flashing cursor between call windows when both rigs sending 73 message.

  - I believe I have made SSB work with function keys with 2BSIQ.

24-Aug-2022

  - Added dupe checking for both CQ and S&P modes (if enabled).

  - Bandmaps is stort of starting to work - but not sure it is 100 percent yet.

  - Fixed CWT multiplier calculation.

  - Occasionally, Autostart send is brain dead if very busy on other radio.
  - Need to setup @ + CQExchange on F3

23-Aug-2022

  - Search And Pounce implemented.  Probably lots of missing details, but you
    can actually work guys with it.  Probably not working on SSB yet.

  - One new feature with S&P.  If you have an empty call window - press F1 to
    send your callsign and then enter a call - and then hit RETURN (nothing yet
    entered into the exchange window) - the initial exchange will get filled
    in the exchange window - and if there is enough info there to log the QSO,
    it will be logged instantly.

  - Allowed SPACE BAR with nothing in call window to jump into S&P and send
    my call.

  - Added support for commands in function key and QSO messages.  A few were
    deleted that make no sense in the context of 2BSIQ.  Also, LogLastCall
    turned out to be difficult to implement without major surgery of the
    existing code - so it was deleted.

19-Aug-2022

  - In order to guarantee we don't get a message interjected from
    the other radio while finishing off an auto start send - I have
    disabled the keyboard during the brief time auto start send has
    started sending CW - until the call is terminated.

  - AltP now works in 2BSIQ.

  - Added new LOGCFG command - 2BSIQ HEADPHONE SWITCHING ENABLE (TRUE or FALSE).
    This is also in the ControlJ menu.  If FALSE, no headphone switching
    is done with 2BSIQ.  Gets rid of annoying relay clicks if you are
    not using it.

  - Made SO2R MICROPHONE RELAY ENABLE work with SO2R mini.

  - Changed how the colored displays work for the transmit message status.
    The information flows from the message cue now instead of being set
    by the state machine.

  - Changed how CQ messages are processed so that there is no longer a
    window where you can press a key on the other radio and interject a
    message between the callsign and CQ exchange.

  - Did a bunch of work with the CW message cue - getting priorities to
    work - although they really aren't doing anything for me at this point,
    so you should not see any impact.


18-Aug-2022

  - Good enough support for : in CW message (expected to be in F10)

  - Copied SCP code from LOGEDIT.PAS into TBSIQ_SUBS and cleaned up the
    routine to use TBSIQ keystrokes.  Not sure this resulted in any change
    to how it appears to work.

17-Aug-2022

  - Fixed a bug that was causing hangs when employing auto start send.  I
    was getting stuck in the CWFinished loop because I kept setting the Index
    (temporary pointer) to the start of the cue.

  - Made F1 and F2 work when exchange window up.

  - Fixed intiial exchanges when working a station previously logged.

  - Fixed bug of empty function key exchanges sending garbage.

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

15-Aug-2022

  - Made PageUp/PageDn work to change CW speeds in real time
  - Made TX indicator stay when changing code speed
  - Enabled numeric keypad PageUp/Dn keys to control CW speed
  - Started implemention of SearchAndPounce Mode - not completed yet.

14-Aug-2022 (or before)

  - Added TX to the CW Speed display to show which rig has the TX focus if you
    start sending by hand.  You can change it to the other rig with AltR (on
    either keyboard).

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
        TBSIQ_UpdateTimeAndrateDisplays;
        Radio1QSOMachine.CheckQSOStateMachine;
        TBSIQ_CW_Engine.CheckMessages;
        Radio2QSOMachine.CheckQSOStateMachine;
        TBSIQ_CW_Engine.CheckMessages;
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
