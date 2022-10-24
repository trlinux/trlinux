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
   This might be fixed as of 4-Sep - waiting to see if it shows up again.

 - Anyway to quickly turn on monitor tone for manual sending?  either automagic or manual

 - When starting AutoStartSend when the other radio is sending CW - the indicator is
   yellow until the exchange starts.  This isn't a big deal and perhaps more trouble
   than it is worth to fix it.

 - In Classic mode - pressing F1 in S&P with a call in the call window does
   indeed put you in the exchange window with the initial exchange, except
   it is at the start of the window where the InitialExchangeCursorPos = AtEnd

 - RepeatSearchAndPounceExchange not used in 2BSIQ yet.  Will always use the non
   repeat one.

 - Steal QSO number from other radio if no activity?

CHANGE LOG - this is really mostly 2BSIQ - see TR.PAS for everything else

24-Oct-2022

  - Think I got the band map blinking call stuff going into the call window
    working well.

  - First escape will clear call window - second one will clear possible call diplay.
    the possible call display also disappears when a QSO is made.

  - Made editable log appear after initialization of keyboards.  Before, you had to make
    a QSO first.

23-Oct-2022

  - AutoCQ supported.  Works basically the same as classic - exccept the default
    listen time is four seconds.  You can adjust it during listening time in
    half second increments using PageUp/Dn.  PageUp/Dn during transmit will
    change the CW speed.

22-Oct-2022

 - In S&P - made new band map entry when calling station.

18-Oct-2022

 - More work with band map and radio on the move to make things as identical to
   the classic mode as possible.  It appears to be working as intended.

 - One of the two new kittens has discovered the radio room - so we might have some
   new "features" added by paw prints on the keyboard.

17-Oct-2022

  - Much work done improving band map operation.  It seems to basically work but
    might hae some rough edges.

  - Made S&P Exchange window conform to established colors (green).

  - Made state of CQSending73Message last until message complete on both
    SSB and RTTY.  This helps keep the display of the message up for awhile.

  - Made DualingCQs work on RTTY.

  - Changed the scope of disabling keys from initiating action whlie the other
    radio is transmitting.  This was done because I didn't have an effective
    lockout for the QSL message.  Basically - anything that could possibly start
    a transmission (when both rigs are not on CW) will be defeated until the
    transmission is complete.  You can add characters to the window you are
    in and perform operations like AltE - but some things like hitting TAB
    to enter Search And Pounce mode are also disabled.  Just to be clear, keys
    are disabled - not buffered.  You will have to hit the key again after
    the transmission is complete.

  - Kept Alt-E from messing up the call window for radio two.  It was trying to
    display the INSERT mode status.  Note that Alt-E never allows INSERT status
    anyway.

  - RETURN in S&P mode if the station already called was not sending the S&P
    exchange (at least on RTTY - but maybe SSB).  This is now fixed.

  - Changed minimum time for the RED TX indicator for SSB and RTTY to 3 seconds
    instead of two.  I was occasionally seeing it drop out before the rig status
    updated to TX.

16-Oct-2022

  - Cleaned up digital use of exchange function keys.  See notes in main program.
  - Some work keeping "CW" messages up on RTTY and Phone

15-Oct-2022

  - Added support for RTTY (with K3/K4).  Did play a little bit with Phone stuff
    too - trying to improve lockouts.

10-Oct-2022

  - Added explicit "listen to other rig" when calling dualing CQ.

9-Oct-2022

 - Improved Dualing CQs to work on SSB.  Again, it does not automatically
   send a blind CQ or pick up again after a QSO is logged like it does in classic
   mode.  You have to manually restart it with the Control-Dash key on either
   keyboard.  The first CQ will be sent on the radio/keyboard the control-dash
   is pressed on.

 - The faster poll rate didn't seem to really be working - and the response
   seemed okay without it - so I removed it.

8-Oct-2022

 - Added immediate flag for transmit status on SSB.

- Put K3/K4 radio into faster poll rate for TX status when looking for the
   end of a transmission.

- Got DualingCQs kind of working on CW - aborted by any keystroke.  Use
  Control-Dash to start or restart it.

5-Oct-2022

 - Rate windows moved to just above the QSONeeds window.

 - New QSO number methodology.  Next QSO number comes from looking at the log
   files.  Also implemented consistently with classic mode.

2-Oct-2022

 - Change how the TX ID and TX Color and Insert get displayed - I remember what
   was last displayed and if someone is asking to display the same thing, I do
   not do anything.  This eliminated some scenarios where rapid flashing would
   occur.

- Momentarily removed the lockout from the transmitter being busy.

30-Sep-2022

 - Fixed cursor being in wrong place after initializing the second radio.

29-Sep-2022

 - Enabled frequency input in call window for VFO A
 - Fixed control/shift function key memories not working.
 - Added support for F13, F14 and F15 on an apple keyboard > Control-F3, F4 and F5.

28-Sep-2022

 - Found a case where AutoStartSend didn't want to work.  Changed ActiveMode to Mode
   as a qualifier and that seemed to fix it.

 - Improved how the Alt-P memories are shown with regard to mode.

27-Sep-2022

 - Fixed CW messages getting sent when not in CW mode

 - Support for QSO number on SSB.  You need to put # in the apporpirate memory.
   Note that if you put the # in the S&P SSB EXCHANGE message - you have to
   press F2 for that message to be "executed" and thus generate a QSO Number.

9-Sep-2022

 - Made ControlEnter work for moving forward in a QSO without sending CW.

8-Sep-2022

 - ShowStationInfo mostly working.  QSO and Mult status uses classic windows
 - Added vertical line between the two QSO machines.
 - TotalScore now getting updated after each QSO.

7-Sep-2022

 - Added Alt-U command

 - PossibleCall window getting removed after logging QSO - or enough ESCAPE
   presses.

4-Sep-2022

 - Possible Calls added.  Moved the CW message display up to make room.
 - Noticed rig two windows were starting at column 40, not 41.
 - Made two keyboards with same mfg id work.  Can also use laptop keyboard.
 - Insert mode display added
 - Eliminated *** debug message when unknown key pressed

3-Sep-2022

  - Implmeneted SPRINT QSY RULE
  - Tried again to fix the AutoStartSend being brain dead occasionally.  Need to test in CWT.
  - Much improvement with BandMap
  - Added support for \ key send QuickQSLMessage1.  Currently hardwired.
  - Fixed speed changes during CW message aborting message.

  - Made hitting F1 or RETURN when calling a station in S&P put you in the
    exchange window with the initial exchange if the call appears valid.
    Note that the cursor is always at the end - irregardless of the setting
    for initial exchange cursor pos.

2-Sep-2022

 - Fixed S/N not going past 1 in S&P.
 - Fixed AltZ operation so the exchange window always comes up and is correct
 - Made # with no callwindow and no exchange window contents in 2BSIQ send previous QSO #

 - Did a little code cleanup in LogWind in the UpdateTimeAndRate displays with the
   radio display code.

 - Fixed ESCAPE key with blank exchange window in S&P not resetting the flag indicating
   that the callsign has been sent.  Note that in normal mode - if you press ESCAPE with
   and empty exchange window - the call window clears too.  In 2BSIQ - you first end up
   going to the call window - and then clearing it with a second escape.  Not sure this
   inconsistency is a problem.

**** 0.54 release

1-Sep-2022

 - Attempted to make active radio go to the one you just hit a function key for.

 - Made SRS command go to the radio tied to the instance it was initiated from

 - Added CWO contest and added CWT and CWO to the log prompt menu.

 - Changed format of the SERIALNUMBER.TXT file to have the prefix QNR: for lines
   created when reserving a QSO number.  I updated the format shown under 31-Aug.

 - Added QLQ: entry to SERIALNUMBER.TXT to show the contents of the call window
   when the QSO is actually logged.

   QLQ: 14 2022-09-01 21:55:03 K7BH

 - Made the QSO number sent get logged.

 - Fixed bug where exchanges were being put in the CW buffer as urgent priority and
   could come out before the callsign was sent in some cases.

31-Aug-2022

  - Came up with my prototype for doing QSO Numbers.  The key to this is a text file
    called SERIALNUMBER.TXT.  If there is no file, then the first QSO number that will
    be sent will be #1.  Whenever a new QSO number is sent - it is assigned to that
    QSO that is progress.  For each - and entry into the SERIALNUMBER.TXT file will be
    made that looks like this (updated with 1-Sep changes):

    QNR: 1 2022-08-31 21:23:50 K3LR 7027940 40 CW

    If a QSO ends up not getting logged - the entry stays in this file and the next
    QSO number will be fetched when needed.

    This file will be looked at when the program starts up to determine what the next
    QSO number should be.  It will be one plus that last number in the file.

    Using the # in the exchange memories during the QSO before it is logged will repeat
    the same number that was originally sent.

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
    { We need to trick the QSONumber generator into giving us the last QSO
      Number given out again.  NextQSONumberToGiveOut should be at least
      equal to 2, but we check just in case }

    IF NextQSONumberToGiveOut > 1 THEN
        Dec (NextQSONumberToGiveOut);

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
    Radio2QSOMachine.InitializeQSOMachine (R2KeyboardID, RadioTwo, 41, 19);

    PaintVerticalLine;

    ActiveRadio := RadioOne;
    SetUpToSendOnActiveRadio;
    TBSIQ_CW_Engine.ShowActiveRadio;
    TBSIQ_BandMapFocus := RadioOne;
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

    IF NOT NewInitializeKeyboards THEN
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
