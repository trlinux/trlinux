//
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

// To make a new version:
//
// 1. Update the src/versions.inc file
// 2. Update the Makefile in the upper directory with the vesrion #
// 3. Update the release notes.

PROGRAM ContestLoggingProgram;
{$linklib curl}
{$linklib X11}
{$linklib c}
{$linklib gcc}
{$V-}

{  CHANGE LOG

???? - When a callsign is in the call window from a QSY using the N4OGW bandmap and
       you want to edit the call - ESCAPE will clear the whole entry - but backspace
       does nothing.  Maybe you can make backspace also clear the whole entry - or
       perhaps start editing the call (and get rid of the initial exchange stuff).

     - When you click on a spot then enter the call and it is a dupe - it disappears
       which is different behavior than if you do it just tuning to a frequency .

     - Although when working someone in S&P - after you are done - the call is gone


TODO List after 2023 WPX CW:

 - Bandmap is challenged indicating mult status of portable callsigns (prefix)
 - Some bandmap confusion on which band is dispayed with TBSIQ
   grep  - Update whole bandmap when any QSO made - am seeing lots of calls not disappearing
   when they are now dupes.
 - Not getting SCP info updated when editing middle of callsign (TBSIQ?)
 - Alt-I seems to work once but not again (TBSIQ?)

02-Oct-2024
 - Added the score reporter category fields to the control-J menu.  This
   allows for you to set them up quickly for the contest you are running
   instead of having to edit your config file.  Also - the score reporter
   contest name and multiplier configuration is automatically derived
   from the MY CONTEST statement.  For those contest names that include
   a mode (like CQ-WW-CW) - the mode will be appended at the time the
   score report is generated based upon the active mode.

   The legal values are in the score reporter specification here:

   blog.contestonlinescore.com/online-scoring-xml-specification/

   The legal values are:

   CATEGORY ASSISTED     (ASSISTED, NON-ASSISTED)
   CATEGORY BAND         (ALL, 160M, 80M, 40M, 20M, 15M, 10M)
   CATEGORY MODE         (CW, DIGI, RTTY, SSB, PSK, FT8, FT4, MIXED)
   CATEGORY OPERATOR     (SINGLE-OP, MULTI-ONE, MULTI-TWO, MULTI-MULTI)
   CATEGORY POWER        (HIGH POWER, LOW POWER, QRP)
   CATEGORY TRANSMITTER  (ONE, TWO, UNLIMITED)
   CATEGORY OVERLAY      (CLASSIC, ROOKIE, TB-WIRES, WIRE-ONLY)

29-Sep-2024
 - Made SCORE REPORT ENABLE available on the Control-J menu.

24-Sep-2024
 - More Salmon run domestic file cleanup - added all of the VE mults.  Also
   fixed issues with PE and MA.

 - Fixed NORMAL PTT mode not working.

19-Sep-2024
 - Changed Salmon Run QSO point method to 3 CW and 2 SSB and fixed mult by mode.

13-Sep-2024
 - Added SST contest.

11-Sep-2024
 - Fixed QSO points not being present on log entries pushed up out of the
   editable window - thus you are only getting QSO points in the editable
   window towards your TotalScore.  This bug was introduced in August 2024.

10-Sep-2024
 - Lots of work with the bandmap and visible dupesheet focus in TBSIQ.  Seems
   to work much better now. Also added display to the visible dupesheet showing
   which band/mode is displayed (in lower right corner).

 - Fixed missing space after initial exchange put in exchange window.  Also
   fixed showing multiplier status if possible.

09-Sep-2024 (after CW Sprint)
 - In TBSIQ - any keystroke pressed on the incative radio when AUTO START SENDING
   was active on the active radio are not processed until the auto start is done.
   Now - an ESCAPE KEY will get processed right away (to stop sending CW so you
   can come back to the station ASAP).

25-Aug-2024
 - Added up NAFILEWRITE startup command which will create an ADIF file out
   of a NA bin file.  Currently forcing the mode to CW.

13-Aug-2024
 - Fixed bug with hexdump utility not printing out the 4 MSBs of hex values.

11-Aug-2024
 - When reading in a log that has frequency data in the QSO number field -
   the program will just count the QSOs and use that as the QSO totals
   for generatring QSO numbers instead of the high QSO number sent.

10-Aug-2024
 - Removed MULTI MULTS ONLY command.

 - Fixed DX mults showing up twice in NAQP log entries

 - Fixed DX mults not showing up after scrolling out of the editable log window.

7-Aug-2024
 - Changed AUTO SIDETONE ENABLE to AUTO SIDETONE LEVEL where zero will disable the
   feature and anything above zero will be the monitor level that will be used
   when sending paddle CW.  Again, this only will work with K3/K4 radios.

Release 0.60 - June 20, 2024 > 16-Jul-2024

16-Jul-2024
 - Fixed initial exchange not getting updated as you worked guys in IARU and
   likely other contests.

 - Removed PARTIAL CALL LOAD LOG ENABLE.  It is essentially TRUE now all of the
   time.  The only reason for not doing this was a speed issue back in the old
   DOS days.


17-Jun-2024
 - Lots of work with QSO numbers coming in over the network.  Many bugs fixed and
   operation made more robust.  This includes integration with 2BSIQ.

08-Jun-2024
 - Improved QSO number over network.  Now providing the ability to send the QSO number
   back to the master computer when exiting an instance of the program that has
   MultiRequestQSONumber set to TRUE.  Sometimes, it seems there is some delay on
   getting an initial QSO number when starting up the program, but I have put retries
   in and it will eventually get it.  After that - it seems solid.

   There are status messages on both sides of the request/response with time stamps
   to provide visability on what is actually happening.

 - Added PING command ih the call window.  Enter PING and RETURN and see if there
   is someone answering you on the other side of the network.  The delay time only
   has 10 ms resolution - so is typically shown as zero.  This is non blocking, so
   if there is no response - life goes on.

23-May-2024
 - Discovered I broke the bandmap on 1-May and reverted back to a LOGWIND.PAS that was
   committed before that.  Then merged the new QSO number stuff into that file and it
   seems to be okay now.  Saved a copy of the bad logwind file to inspect after WPX CW.
   It's brokenbandmap.pas or something like that.

22-May-2024
 - Added support to pull QSO numbers over the network from a "master" computer.  To enable
   this - you need to set MULTI REQUEST QSO NUMBER = TRUE on the slave computers.  When you
   enable this - QSO numbers will only be reserved when you need them.  This basically
   means you won't see the QSO number until your exchange window comes up.  Note that there
   could be a delay to this if the computer giving out the QSO numbers is busy or down.

   This is mostly intended for non 2BSIQ applications - but will hopefully work in that
   mode as well at some point.

 - Removed display of name percentage - just not worth trying to figure out the right
   denomintor with the changes to QSO numbers.

 - Removed DVP support.  Some risk that I affected DVK support which I tried to leave in.

19-May-2024
 - Decided to remove any support for the K1EA network.  This was pretty much used one
   time and not for 20 years.

17-May-2024
 - Added MULTI REQUEST QSO NUMBER.  This will force a program to request QSO numbers from
   a TR networked computer when it needs one.  If there are multiple computers on the network,
   you should set up all computers but one to have MULTI REQUEST QSO NUMBER = TRUE.  This
   will result in only one computer supplying QSO numbers.  The QSO number will be requested
   when the exchange window becomes active (or the $ character is found in a CW message).  The
   instance getting the QSO number should make every effort to use that QSO number eventually
   in case the first QSO is somehow aborted.  There is no mechanism to "return" a QSO number.
   It will just be skipped in the log if not eventually used.

1-May-2024
 - Some clean up on Bandmap display stuff.  Tried to make sure that the dupe display
   or not is cleaned up and updated when changing states.  Also - replaced the ambigious
   * for displaying dupes in the bandmap with a lower case d.

18-March-2024
 - Added ability to send QSOs to N1MM using a UDP port.  To enable this, we need to
   enter the IP address for the machine running N1MM using N1MM EXPORT IP ADDRESS.
   This is currently only coded for CQ-WPX-SSB.  Additionally, you can use N1MM
   EXPORT OPERATOR to indicate the operator callsign.

1-March-2024
 - Fixed PACKET SPOTS = MULT not working

29-Feb-2024
 - Disabled the ControlInsert command - which adds a placeholder to the bandmap

30-Jan-2024
 - When the footswitch mode is to start sending - I won't do it if there is nothing in the
   call window now (same behavior as when you press the auto start send key

 - If you escaped out of sending a callsign with auto start send and cleared the window,
   the auto start send will be re-enabled.   Hopefully this was enough to fix the issue
   of it not being enabled some of the time.  However, I will also re-enable it if you
   press F1 or F2 to call CQ.  I think this isn't a bug in 2BSIQ.

22-Jan-2024
  - Added new command - AUTO SIDETONE ENABLE (default = FALSE).  When enabled, this will
    attempt to enable the radio sidetone for manually sent CW and then silence the sidetone
    when going back to sending a programmed message.  This likely only works if you are
    using the Arduino Keyer and only with dn Elecraft K3/K4.  It is primarily designed
    for 2BSIQ operation but seems to work fine in classic mode even with two radios.

19-Jan-2024
  - Added new way to do multi - using UDP.  MULTI PORT = UDP <ip address> <port>.  The IP
    address is to the next machine in the loop.  Last computer needs to loop back to the
    first computer.

08-Jan-2024
 - Some minor fixes to using RTTY in 2BSIQ mode.
 - Added MST contest.

24-Dec-2023
 - When building a Cabrillo log - you now have the option to ignore the LONGLOG.DAT
   file is you don't want to deal with it.  It is used to get exact frequency
   information into your Cabrillo log.

 - Same as above - for the ADIF convert.


16-Dec-2023
 - Changed how the control-C and control-D are parsed for messages in a
   function key memory.  The last control-D will be used to parse the
   command, so that commands that contain a control-D can be used.  Note
   that this means you really can only have one valid command in a message.

 - Removed WARC bands from Continent Report

05-Dec-2023
 - Added new command TBSIQ DUAL MODE - default FALSE and it is in the ControlJ
   menu.  This is used to enable all of the changes made to TBSIQ to handle the
   special case of mixed mode operation.

03-Dec-2023
 - We were dropping the PTT after sending an auto start sending call before
   the exchange was being sent.  This is because we had ripped out some of
   the PTT Force on code - and in putting it back in - discovered a bug in
   the Arduino where it was unasserting PTT after the timeout even if
   PTT force on was active.  So - this results in an Arduino code update
   in addition to some pascal code.  Also - made the arudino keyer pascal
   code report back that CW was not being sent even if the PTT was still
   active - to avoid being trapped there.

01-Dec-2023
 - When using TWO VFO MODE and you go to a frequency that has an entry in the
   band map - instead of doing a dupecheck if you press the SPACE BAR again,
   it will go back to your CQ frequency.  Also made it so a dupe check with a
   dupe will update the bandmap time stamp.  Some other minor tweaks to how it
   works when finding a dupe on the other VFO.

Release 0.59 - 28-Nov-2023

28-Nov-2023
 - Increased size of possible/partial call list from 200 to 600.
 - Added check to not blow up the SCP list when adding one too many
   entries from the visibile dupesheet.
 - Eliminated double entries when adding call from editable dupsheet into SCP.

26-Nov-2023
 - Fixed score calculation when re-reading in log without .RST file.

22-Nov-2023
 - Fixed sending zone out when sending UDP QSO packets.

18-Oct-2023
 - I had a feature that in classic mode would not add characters to a call window
   entry if the call window entry is the band map binking call.  This manifasted
   itself as follows: Type in ZD9, do a dupecheck (and put it on the band map) so
   you can see the beam heading.  Then try to enter a W and log the callsign.  Pressing
   the W clears the call window..  So - I defeated this feature for better or worse.

27-Sep-2023
  - Added support in KEYERARD.PAS for W9CF added parallel port commands
  - Added support in Arduino code for W9CF parallel port commands.

18-Sep-2023
  - Fixed Cabrillo output for WA-SALMON-RUN

14-Sep-2023
  - Changed the domestic file used for the CW Sprint from S48P8.DOM to SPRINT.DOM
  - For the Sprint, added Hawaii as a domestic mult.

09-Aug-2023
  - Fixed bug where the initial exchanges were getting screwed up if you
    loaded a log without PARTIAL CALL LOAD LOG ENABLE = TRUE. The exchange
    info wasn't initialized and it was being added to the AllCallList.

16-Jun-2023
  - Removed exchange window when returnning from SO2V QSO or not

14-Jun-2023
  - Added memory of calls sent to N4OGW and made them get deleted after an hour

13-Jun-2023
  - Fixed issue with QSO number being inserted at the start of Cabrillo
    lines for contests that do not use serial numbers (like the June VHF
    test).

1-Jun-2023
  - Fixed WPX Prefixes mults not being flagged when receiving QSO_UDP packets.

25-May-2023

  - Made it so QSOs that were imported using N1MM UDP PORT are not sent out to the
    QSO UDP IP / QSO UDP PORT output so we don't get an infinite loop.

09-May-2023

  - Fixed QSOByBand and QSOByMode for 7QP

04-Apr-2023

    - Fixed frequency in ADIF file when using LONGLOG.DAT file

28-Mar-2023

     - Fixed N1MM QSO packets not processing QSO points.

27-Mar-2023

    - Taught the ADIF conversion routine to use the LONGLOG.DAT if a LONGLOG
      file is used for the input.

26-Mar-2023

  - Fixed Russia counting as zero points.

24-Mar-2023

  - Made prefix mults get processed (0119Z after contest start)

  - Improved POST Cabrillo file generator to incorporte data in the LONGLOG.DAT
    file for frequency and sent QSO Number.

22-Mar-2023

  - Added a new file - LONGLOG.DAT which will have all of the information that is
    generated for the normal LOG.DAT entries and add more accurate frequency and
    time data along with an ID for which radio was active.  This file is written
    when the QSO is made and will not reflect any changes made with the Alt-E
    editor.  It is intended to support a future change to the Cabrillo spec where
    the frequency and time will be more exact.  It also likely makes using LOG
    FREQUENCY ENABLE less useful as keeping the sent QSO number is much more
    important especially in the TBSIQ case.

    At some point, the Cabrillo generator in POST will need to be made aware of
    this file and incorporate the new data.  This will probably happen shortly
    after the 2023 WPX SSB event.

21-Mar-2023

  - Fixed QSONumberByBand for classic mode.  Was broken when we adopted the new QSO Number
    system for TBSIQ a couple of months ago.

20-Mar-2023

  - If you are on a frequency where the call window is populated with a callsign from
    the bandmap and start to enter a new callsign, the window will clear out first.  This
    was invented first in TBSIQ and I made it work the same way in classic mode now.  So
    many times I would tune into a frequency and not even be aware that there was a callsign
    polulated and start enterting a call to do a dupe check on it.

19-Mar-2023

  - When on a frequency with BandMapBlinking call - pressing BACKSPACE now will work.
    Before, the call was being overwritten with the BandMapBLinkingCall.


14-Mar-2023

  - Did a few tweaks to the TWO VFO MODE.  If you are in that mode with an empty
    call window - you can swap back and forth between VFOs by hitting the SPACE
    BAR.  If have something in the call window - pressing space bar will do a dupe
    check.  You exit S&P and return to the "CQ" frequency by either exiting S&P with
    an escape key - or pressing SPACE BAR with an empty call window.

08-Mar-2023

   - Rename AUTO SCP CALL FETCH to AUTO PARTIAL CALL FETCH to more accurately describe
     which window it works with.  Also made it work if two letters resolves to a single
     entry in the partial call window.

06-Mar-2023

   - Reworked the logscp.pas file to not use the global variable PossibleCallList.
     Instead, the routines will work with the data structure CallListRecord.  There
     is an instance of that as a global variable called CD.SCPCallList that replaces
     the old PossibleCallList.

   - Removed support for INITIAL.EX file - please use TRMASTER.DTA

   - Improved format of QSO records send with QSO UDP PORT/QSO UDP IP to use new
     lines and tabs in front of each field.  This copies how N1MM and DX Log send
     their data.

02-Mar-2023

   - Note there is a new Arduino release - SO2RMini_2023_Feb_28.  This has a PTT
     fix and code added by N4OGW to suuport integration of the SO2R Mini with the
     TRLog firmware for his so2sdr program.

   - Added some support for some of the typical multi-multi contests to the new
     N1MM_UDP_PORT function.  There are still lots of exchanges that will not be
     parsed correctly.  Also - currently, there is no validity check on the data
     before attempting to log the QSO.

28-Feb-2023

   - Added N1MM_UDP_PORT config command which will accept "industry standard" QSO
     records and log them.

24-Feb-2023

  - Added indication of the band/mode for the reminaing multiplier display.
  - Updated remaining country list to include PJ4 and PJ5.  Removed four
    rarer countries from both remaining country lists to make room.

23-Feb-2023

 - AUTO SCP CALL FETCH feature implemented (default FALSE - available on ControlJ menu)
   When TRUE - if you have entered three letters and they resolve so that only one SCP
   call is shown - pressing RETURN will use that SCP call.  Works in both classic and
   TBSIQ.

11-Jan-2023

 - Made some minor tweaks to the QSO UDP record so that K3IT's Qsorder program will be happy
   with our UDP packets including SO2R support.

08-Jan-2023

 - Changed default of BANDMAP ALL MODES to TRUE to get around weird bug I was
   having during an RTTY contest.

07-Jan-2023

 - Fixed RTTY messages being saved incorrectly into CFG file.

06-Jan-2023

 - Added UDP export of QSO information.  New LOGCFG commands:

     QSO UDP IP = 192.168.0.100
     QSO UDP PORT = 10009

   Many data fields are being parsed, but some are not yet fully engaged (like frequency information).
   However, there should be enough information there for the basic information of the QSO.


27-Dec-2022

 - Expanded the N4OGW commands to support two radios:

    N4OGW RADIO ONE BANDMAP IP        defauly is blank string (Use 127.0.0.1 for local running bandmap )
    N4OGW RADIO ONE BANDMAP PORT      default is zero
    N4OGW RADIO ONE BANDMAP UDP PORT  default is 45454
    N4OGW RADIO TWO BANDMAP IP        default is blank string (Use 127.0.0.1 for local running bandmap )
    N4OGW RADIO TWO BANDMAP PORT      default is zero
    N4OGW RADIO TWO BANDMAP UDP PORT  default is 45454

    When using two band maps - TR will send any spotted call to both bandmaps.
    When a call is deleted - it will also be deleted in both bandmaps.

    Really - the only time the two are handled distinctly is when they generate a frequency command
    from a mouse click and it needs to be routed to the proper radio.  However, we can get extra
    bonus points for being careful when we get a delete command and only removing the specific
    frequency for that callsign.  Note that this will generate a non-specific-frequency delete
    command to N4OGW which will remove the call from all frequencies on his bandmap.  This could
    perhaps be worked around by detecing this condition in TR Log and then sending a new spot to
    N4OGW for the instances of the call that should stay displayed.

    The two bandmaps can likely share the same UDP port number for sending data back to the logging
    program.  There is a RadioNr field in the packet that will make it clear which radio should
    respond to the command.

25-Dec-2022

  - Fixed QSOs this hour display - was stuck at zero.

  - Added display of working directory for TRMaster file to both Control-J and
    the Statistics display in POST (function Z at the TRMASTER menu).  The default
    location appears to be .trlog in the users home directory.

24-Dec-2022

  - Added Control-Q to CW message - will send integer kHz of the other radio's
    frequency.  It will also generate a note in the log indicating who was asked
    to go where.

23-Dec-2022

  - When a band map entry times out (over 60 minutes) it will also get deleted in the
    N4OGW bandmap as well.  Pretty sure this will delete any instance of that call regardless
    of the band however.

20-Dec-2022

  - Did some work to simplify the WindowEditor routine.
  - Fixed bug where I coudln't overwrite a band map displayed callsign by typing over it

17-Dec-2022

  - Added TWO VFO MODE (control-J) which when enabled will process a space bar at an empty
    call window in CQ mode to swap the A and B VFOs and put you into S&P mode.  Once out of
    S&P mode - you are back on your CQ frequency.  This currently only works with Kenwood
    and Elecraft radios.

11-Dec-2022

   - Some rough edges with bandmap blinking call resolved.  Seems to work great with the
     N4OGW bandmap now.  Made calls from the N4OGW bandmap go to VFO A for now.  Not sure
     how things will work with the second VFO or even a different rig ATM

10-Dec-2022

   - N4OGW bandmap interface is now feature complete.  Will need some adjustement of the
     color coding to make it look better.  Some features not yet supported in 2BSIQ.

05-Dec-2022

   - Added support for N4OGW so2r-bandmap.  Use N4OGW BAND MAP IP and N4OGW BAND MAP PORT
     in your config file to enable.

03-Dec-2022

   - Fixed CQ with footswitch restarting message as footswitch still pressed

29-Nov-2022

   - Fixed new bug where exiting F10 (sending keyboard CW) left PTT hanging that
     was introduced with the SO2R mini on 28-Nov-2022.

28-Nov-2022

  - Changed SO2R Mini CW character command from ^ to ; (semi-colon)
  - Changed SO2R Mini version to TRCW V4
  - Changed SO2R Mini to not drop PTT when seeing PTT unforce command when more chars to be sent.
  - Had a few places where I had removed PTT unforce from classic interface that could be put back.
  - Added support for ^ half space to SO2R Mini

03-Nov-2022

  - There is an issue with using the SO2R Mini with the ^ character in CW messages.
    Previously, using a ^ in a CW message would produce a half space.  For the SO2R
    Mini, this is a prefix for a command - which will be the next character in the
    message.  Here are the valid SO2R mini commands:

    ^D - Long Dash
    ^F - Speed up CW by 2 WPM
    ^H - Half dit space
    ^I - Switch headphones to inactive radio
    ^J - Switch headphones to active radio
    ^S - Slow down CW by 2 WPM
    ^X - Switch headphones to Radio 1
    ^Y - Switch headphones to Radio 2
    ^Z - Switch headphones to Stereo

    Also - I found a bug with the implmentation of the ^H command in the SO2R Mini
    firmware that has been fixed with release dated Nov 3, 2022.

02-Nov-2022

  - With the SO2R mini, the weight display in the ControlJ menu was being divided
    by 128 instead of 100 - and thus was displaying wrong.  If you entered a new
    weight, it was being processed correctly - but still displayed wrong.

17-Oct-2022

  - Added JARTS contest - Cabrillo too.  The muiltiplier stuff isn't working yet.

05-Oct-2022

  - Implemented new QSO Number metodology.  The next QSO number to be used will
    be determined when looking at the .DAT and .TMP files.  They will be scanned
    for the largest QSO number that was given out and the next QSO number will
    be one more.  Note that it appears QSONumberByBand might be broken.

04-Oct-2022

 - Fixed bug of Cabrillo file not having END-OF-FILE

29-Sep-2022

 - Winkey fix from W9CF

26-Sep-2022

 - Added functionality for RTTY commands to go to K2/K3/K4 radio (not 2BSIQ yet)
   Note - for most of the messages - we use the CW messages to create strings to
   send to the K3/K4 radios.  However, there are some issues with using the EXCHANGE
   CW memories since F1 gets hardwired to send the callsign - not an actual memory.
   So - we support EX DIGITAL MEMORY as a prefix for the function key memories and
   use these with in digital mode instead of MY CALL and CQ EXCHANGE.

 - Fixed Cabrillo output for CQ WW RTTY to show DX instead of country ID

23-Sep-2022

 - Improved 2BSIQ with SSB.  F1-F4 are locked out if either rig is on SSB
   and the other rig is transmitting.

22-Sep-2022

 - Made Control-Dash Alternating CQs work with K3/K4 radios.  Not sure how
   elegantly we are exiting it.

20-Sep-2022

 - Added ControlB for message commands to force subsequent commands to go to the
   active radio (opposite of Control-A command which steers subsequent commands
   to the inactive radio).

08-Sep-2022

 - Added New York QSO party.  Two new DOM files - NYQP.DOM and NYQPNY.DOM.  New QSO
   point method OnePhoneTwoCWThreeDigital.  Put MY STATE = NY in your logcfg before
   the CONTEST = NYQP statement.

????-2022


 - Made UA and EU QSOs not count for WPX
 - Fixed Paddle CW Speed so that zero will track the Computer Speed (keyerard + Ardunio)
 - Fixed the PTT Hold time for Paddle Sent CW (most in Ardunio)
 - Fixed not iniitializing the serial port speed for the Arduino (keyerard.pas)

09-May-2022 - Version 0.52ish (N6TR developmental fork)

 - Started with 0.49 I think from W9CF
 - Added keyerard
 - Some minor fixes to make Alt-Q work with keyerard
 - Added CWT contest - might need some cleanup on headers of printable log page
 - Removed unused xResults and got to zero "notes" during compile
 - Added a lengthy explaining of how a QSO gets logged to the start of LOGSUBS2.PAS

   To make TRTTY, change TREE.PAS so that RTY shows up between CW and SSB.
   Change the mode change command.
   Change SendCrypticCW.
   UpdateTotals.

 - Added LOGCFG and CTRL-J menu command BAND MAP MULTS ONLY to support only
   displaying multipliers in the band map.

 - Added mulitplier only display band map hot key option. When editting the
   band map via CTRL-END hitting "G" multiplier only display mode on/off.
   STILL NEED TO ADD THE DISPLAY PORTION OF THIS FEATURE

 - DisplayBandMapEnable to allow function of bandmap without displaying
   it. ***VARIABLE INSTALLED BUT NOT DETECTED***

 - Fixed a "gotcha" when working a station using ALT-D. If you had
   called the station on the inactive radio and hit ALT-R to swap
   radios rather than ESC to abort it the program would get a bit
   balled up. This no longer allowed and you are warned to do the
   right thing. ***I COMMENTED THE FIX OUT IN LOGSUBS2.PAS. GEORGE
   ALMOST CAME UNGLUED ABOUT THIS ONE! HE KNOWS WHAT HE IS DOING
   WHEN HE SWAPS RADIOS IN THE MIDDLE OF A 2ND RADIO QSO AND SWAPS
   BACK. AT LEAST ON BETA TESTER CAUGHT THIS LONG STANDING BUG. IT
   NEEDS TO BE FIXED ANOTHER WAY OR HAVE A VARIABLE CONTROL WHETHER
   OR NOT THE FIX IS TURNED ON. MAYBE SOMETHING CALLED
   "I KNOW WHAT I AM DOING = TRUE" HI HI.

 - Try as I might, I can't make a bandmap entry come up in split when
   entered in TR.  Works okay from CT network and perhaps packet.

Version 6.76 - 20 October 2003

 - Fixed incorrect time on logged QSO when using HOUR OFFSET <> 0.

 - Many K1EA network enhancements:

    - Added support for packet commands.  TR can even be running on the
      computer the TNC is hooked up to if you want.

    - Added support for bandmap commands.

    - Do not pass note log entries (Alt-N) to the network.

    - Fixed K1EA STATION ID in config file (was STATION ID).

    - Added support for K1EA pass and run frequency display in multi info
      display.  This display is shown either at the bottom of the bandmap
      or in the editable log window when you press Control-E.

    - Implemented PASSFREQ command by typing in \PASSFREQ in the call
      window.  Allows you to enter the frequency you want multipliers
      passed to.

    - If using K1EA network, Alt-D will allow you to pass a station.

Version 6.75 - 12 October 2003

 - Added ORION Radio type.  It mostly works.

 - Fixed up ARRLSECT.DOM with NT for the VE8/VY1 section name.  Note that
   you will now have to enter NTEX or NTX for the North Texas section.

 - Minor changes to CALQSOW6.DOM to allow MARN and SCV to show up in
   Marin and Santa Clara counties.  Same for SCV in CALCTY.DOM.

 - Removed LEGACY RADIO IO MODE.  The program should now work fine without
   it.  Please report any radio control issues that you see with this
   version.  There might be some slight performance degradation with
   Kenwood radios.  Also note that the frequency display on Kenwoods will
   disappear if you are continuously tuning the radio.  This is because
   the radio will not respond to frequency requests while it is being
   tuned.  When you stop tuning, the frequency display should reappear.
   You can increase the KENWOOD RESPONSE TIMEOUT if this bothers you.
   However, increasing this will slow down the program if your radio is
   off.  This can be fixed by disabling polling in the Control-J menu.

 - Added 19200 and 57600 baud support to the loopback test (TR LOOPBACK).

 - Added SENT ALT-D SPOTS TO PACKET (control-J, default = FALSE).  When
   you use Alt-D to do a dupe check on the second radio, the program
   will automatically generate a packet spot.

 - Replaced FREQUENCY ADDER with FREQUENCY ADDER RADIO ONE and FREQUENCY
   ADDER RADIO TWO.  The old FREQUENCY ADDER command will set both
   radios to the same frequency adder.

 - Added ALT-D BUFFER ENABLE parameter (control-J - default = FALSE) to
   control Alt-D buffer feature added in 6.73.

 - Added UNIXTIME start-up command.  Allows you to enter integer UNIX
   time and get year, month, day, hour, minute, second, or vis-a-versa.

 - Added K1EA STATION ID - which is a character used to identify the
   computer on the K1EA network.

 - Added K1EA NETWORK ENABLE (default false - control-J) which makes TR
   talk K1EAish on the network port.

Version 6.74 - Released 31-Aug-03

 - Added LEGACY RADIO IO MODE (available on Control-J - initial value =
   FALSE).  When TRUE, this backs out some of the recent changes made
   to the Icom and Omni-VI radio communication.  It should work the
   same as versions before about 6.69.  (Please note this was removed
   in 6.75).

 - Added WAE Cabrillo support.  Also made 4 digit time get logged in
   WAE files even if using QTC MINUTES.

 - Made IARU and CQ contests ask for ARRL section if in W/VE.

 - Fixed problem with exchange window and bandmap - only being able to
   enter one letter.

 - Fixed simulator in FD not sending sections.

 - Added REGION ONE FIELD DAY to Cabrillo - using same template as WPX.

Version 6.73 - Released around 1 June 2003.

 - Fixed a problem with initial exchange for rover calls.

 - Fixed RESCORE command line option from aborting when DVP ENABLE = TRUE
   and DVPTSR or SBDVP were not loaded.

 - Fixed AutoSAPEnable so S&P mode is only entered when changing
   frequencies within the same band and mode. Band changes and mode
   changes no longer trigger S&P mode.

 - Added OMNI6 and OMNIVI radio types. Removed the strict communications
   checking which caused trouble for some Ten Tec Omni6 users. ***THERE
   MAY BE SOME ISSUES WITH THIS. IF YOU RUN INTO TROUBLE PLEASE REVERT
   BACK TO YOUR PREVIOUS ICOM RIG DEFINITION.***

 - Added ARGO radio type.  Same warning as the OMNIVI.

 - INITIAL EXCHANGE OVERWRITE now controls overwrite action for all
   initial exchanges, not just custom ones. The default remains FALSE.

 - The install program now asks if you would like
       BUFFERS=15,0
       FILES=40
       STACKS=18,512
   added to CONFIG.SYS.

 - A tone is no longer produced when using left CTRL-SHIFT as a PTT. If
   TUNING WITH DITS = TRUE a tone is still produced.

 - It is no longer possible to send a CQ frequency marker as a packet
   spot. (Even though CQ markers do not show up in the call window from
   the band map anymore.)

 - An initial exchange now includes a space as the very first character.
   With INITIAL EXCHANGE CURSOR POS = AT START you would have to remember
   to add a space before hitting Enter to log the contact. This makes
   logging more consistent.

 - CTRL-A can now be used to send many commands to the inactive radio (in
   addition to sending CW or DVP/DVK messages to the inactive radio
   already supported). Commands supported at this time are:
        NEXTBANDMAP, NEXTDISPLAYEDBANDMAP, NEXTMULTBANDMAP,
        NEXTMULTDISPLAYEDBANDMAP, LASTCQFREQ.
   Put the CTRL-A within the CTRL-C/CTRL-D pair. For example
   <03><01>NEXTBANDMAP<04>. HINT: To be consistent with the keystrokes
   CTRL-PageUp, CTRL-PageDown (CW Speed), and CTRL-Enter (for freq
   control) operating on the inactive radio you might want to program
   Fx to be <03>NEXTBANDMAP<04> and CTRL-Fx to be
   <03><01>NEXTBANDMAP<04>.

 - Mixed mode SO2R now works.

 - Fixed deleting a log entry (using ALT-Y) not properly updating the
   dupe status of the subsequent band map entry if it was not the
   active band/mode (ie. made using ALT-D).

 - Fixed CTRL-PgDn/CTRL-PgUp to use CODE SPEED INCREMENT value.

 - Direct frequency entry for inactive radio using CTRL-Enter. You can
   now enter a frequency directly into the inactive radio right from
   the call window just like you would for the active radio. Just use
   CTRL-Enter to enter it rather than Enter.

 - Can set the frequency of the B VFO using the direct frequency entry
   in the call window by ending the frequency with "B". This works with
   the new CTRL-Enter frequency entry to control the inactive radio as
   well. Examples: 14250B, 250B, 14.250B.

 - Enhanced support of the band map with the inactive radio. Call, band,
   and mult needed information will show up as you tune the inactive
   radio. The band map call is also loaded into the ALT-D buffer (dupe
   check on inactive radio). The band map call is automatically filled
   in for you when doing ALT-D. You can overwrite it by typing a new
   call. AUTO S&P ENABLE SENSITIVITY effects the tuning sensitivity
   in the band map for the inactive radio. Setting this value to 100
   really improves operation. I am working on fixing the program
   so tuning the inactive radio works like the active radio.

 - Added TUNE ALT-D ENABLE option to the CTRL-J menu and
   LOGCFG.DAT. When in CQ mode on the active radio the band map call
   of the inactive radio is automatically entered to be ready to work
   on the inactive radio with the space bar (if not a dupe) as you
   tune the inactive radio. The default is FALSE.
   AUTO S&P ENABLE SENSITIVITY effects the tuning sensitivity
   in the band map for the inactive radio. Setting this value to 100
   really improves operation. I am working on fixing the program
   so tuning the inactive radio works like the active radio.

 - Super Check Partial is active when using ALT-D (if SCP is enabled).

 - ALT-D messaging has been improved to try and keep the operator
   informed of the state of SO2R condtions. The function has remained
   the same, but the messaging is more thorough.

 - Added a frequency display for the second radio. The frequency
   of the active radio is highlighted, while the frequency of the
   inactive radio is dimmed.

 - Added color selection for the radio name window when each radio
   is selected. RADIO ONE WINDOW COLOR and RADIO ONE WINDOW BACKGROUND,
   RADIO TWO WINDOW COLOR and RADIO TWO WINDOW BACKGROUND. For
   backward compatibility the original RADIO WINDOW COLOR and
   BACKGROUND will set both radio one and radio two colors. Remember
   these statements need to come before the "DISPLAY MODE =" statement
   in LOGCFG.DAT to have an effect.

 - Fixed a shift key tuning bug when K2 radio was defined.

 - Michigan QSO Party support improved. RST is no longer included in
   the log. A QTH is required as part of the exchange even for DX
   stations sending "DX". "DX" will be shown incorrectly as a mult
   on TRLog. Cabrillo generation fixed per K8CC.

 - POST Cabrillo generator will correctly generate M/2 entries for
   NAQP.

 - Fix scoring for CQ VHF contest. Now only 6m and 2m count.

Version 6.72 - released 25 December 2002

 - Updated ALT-P message entry to recognize hex codes entered between
   < and >. Now for example <03>EXCHANGERADIOS<04> will be interpreted
   correctly when entered using the ALT-P menu. You can still enter
   control characters directly when preceded by CTRL-P as before.

 - Fixed flashing frequency display when using the footswitch input
   for PTT introduced with fast frequency polling.

 - ARRL RTTY Roundup now uses the correct domestic multiplier file
   which includes DC.

 - Added support for six digit Japanese prefectures as multipliers.
   Digits are now recognizable as multipliers in domestic multiplier
   files. A new exchange type is RSTLongJAPrefectureExchange. A new
   contest template has been added "JA LONG PREFECT" as listed in
   FCONTEST.PAS. WARNING: Large domestic multiplier files demand a
   great deal of memory.

 - Fixed SCP reaction time bug introduced with fast frequency polling.

 - Added support for the following extended ASCII characters:
   A-umlaut, A-ring, and O-umlaut. They will be recognized in the
   exchange window only and can be sent as CW in messages and from
   the keyboard.

 - The CW speed increment used for PGUP/PGDN is customizable between
   1 and. 10 WPM. Use CTRL-J or LOGCFG.DAT command CW SPEED INCREMENT.

 - Summary file generation now follows the naming convention for
   multiple logs in a directory. The standard name is SUMMARY.DAT,
   will be have SUM suffix if a log other than LOG.DAT is selected.

 - Fixed Auto CQ DVK message not being repeated. There is no way to
   detect the end of a DVK message. The timing for DVK messages is
   measured from beginning to beginning again. DVP still works like
   CW messages.

 - Added POLL RADIO ONE and POLL RADIO TWO options to the CTRL-J
   and LOGCFG.DAT. This was requested to allow for radios which
   were not polled properly to be connected to allow commands to
   be passed to them. The default is TRUE which is how TRLog
   works normally.

 - Because of some difficulties mislogging band mapped CQ
   frequency markers they are no longer popped into the logging
   window as you tune by them in S&P mode.

 - TRLog no longer sends any filter information to Icom radios when
   setting the frequency and mode, except for IC-765. This radio has
   the narrow filter selected when in CW mode. This takes care of the
   "Hey, my filter setting changed" problems while addressing the
   needs of the IC-765 users.

 - Added '*' as a character to insert in a CW or RTTY message.
   This will act just like the '@' (insert the call from the call
   window) character unless an Alt-D call (dupe check and work
   call on inactive radio) is available. Then that call will be used
   first. This is to allow a user to program a CW message for the
   second radio when you have to be on the first radio rather than
   following the Alt-D SO2R protocol.

 - Added PACKET SPOT PREFIX ONLY option to the CTRL-J menu and
   LOGCFG.DAT. TRLog leads the charge with the option to limit
   packet spot calls to only the prefix. The default is FALSE.

 - Added Icom hand shaking for "good" response from rig. For each
   command sent. IcomCommandPause is now irrelavent.
   IcomResponseTimeOut default is back to 300ms. Flush
   RadioControlPort prior to sending new commands to be ready for
   acknowledgement. More robust frequency and mode checking too.
   Four byte frequency data is now only supported for IC735.

 - A warning message is displayed if (some) communication problems
   are detected with interfaced radios.

 - Fixed "ALT-R" bug introduced in version 6.71.

 - Added AUTO S&P ENABLE SENSITIVITY option to the CTRL-J menu and
   LOGCFG.DAT. The range is 10 Hz/sec to 10000 Hz/sec. The default
   is 1000 Hz/sec which is the same as previous versions.

 - Fixed AUTO S&P ENABLE sensitivity too high in version 6.71b.
   See above.

Version 6.71b - released 1 November 2002

 - Fixed AUTO SAP ENABLE not working in version 6.71.

 - Fixed Auto CQ DVP and DVK messages not stopping immediately when
   ESC or a letter is entered broken in version 6.71.

Version 6.71a - released 31 October 2002

 - Fixed Auto CQ not being able to escape out broken in version 6.71.

Version 6.71 - released 30 october 2002

 - Fixed Cabrillo generator for CQWW M/2. It now adds transmitter field.

 - Fixed AutoCQ for DVP and DVK so it times the listening time rather
   than the time from the start of the message. I also reversed the
   PageUp and PageDown action so PageUp lengthens listening time and
   PageDown shortens it. I was forever mixing that up.

 - Added two more DVK pins to the parallel port, pins 7 and 8. They are
   activated by messages DVK5 and DVK6 respectively.

 - Fixed DVKDelay and some other SO2R DVK related issues.

 - Added PACKET SPOT COMMENT to CTRL-J and LOGCFG.DAT options. This is a
   15 character value you can specify which will be sent in the comment
   field of a sent packet spot.

 - Cabrillo title fixed for OCEANIA-DX contest.

 - Control-V (read in a config file) improved. Will warn if file was not
   found. The EX or CQ menu will also be immediately updated on screen.

 - Salmon Run contest setup does not ask if you are in Washington twice
   anymore. MULT BY MODE = FALSE is now the default per new rules.
   A summary sheet is now supported. There was another QSO party which
   asked for the state twice. That is also fixed.

 - Added check for Multi-Two to NAQP cabrillo generator.

 - Changed VHF designators in Cabrillo generator to new values per Trey.

 - Added support for new WX0B SO2R and ZS4TX Super Combo Keyer II
   Stereo/Mono input. The state of either pin 5 or pin 9 of a parallel
   port may be toggled using STEREO PIN HIGH = TRUE or FALSE (default)
   in LOGCFG.DAT or the CTRL-J menu as well as the function key command
   TOGGLESTEREOPIN. Use STEREO CONTROL PORT = 1/2/3 and STEREO CONTROL
   PIN = 5/9 in LOGCFG.DAT to set the parallel port and pin to use. If
   you use pin 5 then you cannot use the same port for DVK control. If
   you use pin 9 then you cannot use the same port for band output. You
   can probably think of other uses for this function!

 - B VFO mode now set correctly on Kenwoods for split mode.

 - Fixed IARU initial exchange bug...again!

 - Fixed bug where CTRL-A in a programmed message did not adapt to mode of
   inactive radio when operating SO2R on different modes (ie. IARU,
   ARRL 10m)

 - Added Yaesu rotator command function. Use ROTATOR TYPE = YAESU in
   LOGCFG.DAT.

 - Changed band map edit help string.

 - Added ExchangeRadios message command. The purpose is to make it easy
   to find a new run freq on the second radio (presumably not on the best
   antenna system) and swap it to the primary antenna setup. This feature
   does not yet support split mode nor the setting of VFO B (Only VFO A
   data is exchanged). How to enter program a message command it outlined
   in section 5.2 of the manual (page 40 of the version 6.63 manual).

 - Updated YO DX contest rules. Significant changes from previous rules.

 - Added TR RESCORE command line option. This is the solution to those
   contests which are hard to mult check. It can also be used to simply
   rescore a contest instead of TR READ. Like the POST multiplier check
   option TR RESCORE will save the current log file to PLOGxxx.BAK and
   use that to rerun the contest and create a new log file. CW SPEED is
   set to 99, CW TONE is set to zero, and AUTO DUPE ENABLEs are set to
   false. A current hazard is that if you hit a key while the program
   is running it will stop! Exit the program as you normally would when
   the log is done being read in. You can now continue with POST.

 - Upgraded the radio interface section to speed up radio frequency
   tracking. In particular (at least with Kenwoods) you will see a much
   snappier response displaying the frequency after spinning the dial.

 - Added FREQUENCY POLL RATE option to the CTRL-J menu and LOGCFG.DAT.
   The range is 10 to 1000 in milliseconds. I recommend running at 100ms.
   It really boosts the response time as you tune. 100ms is the default,
   and is quite reasonable polling at 10 times a second!

 - Fixed NONSSB or NONCW contacts from locking TRLog if they scrolled
   off the top of the editable window.

Version 6.70 - released 10 July 2002

 - Have FISTS Sprint now logging properly. Multiplier counting does not
   work. The score will only show QSO points with no mults counted.
   Exchange checking needs improvement. I hope to refine this more for
   the Fall sprint.

 - Fixed changing band map decay time from within TR from causing band
   map to appear not to be decaying. Changing the decay time from the
   CTRL-J menu now resets all band map entries to BAND MAP DECAY TIME.
   This value is also now saved in BANDMAP.BIN so if you restart TR the
   decay times will be compatible.

 - Added a LOGCFG option to allow for a single keystroke to overwrite
   a custom initial exchange called INITIAL EXCHANGE OVERWRITE. The
   default is FALSE to maintain backward compatibility.

 - Fixed some 15m spots below 21.200MHz showing as phone mode spots.

 - Added support for new WAE multipliers for European stations.
   (Still need to update cabrillo generator for new entry categories)

Version 6.69 - released 01 July 2002

 - Band map attempts to maintain some consitency across the network. As
   contacts pass into the editable window from the network the band map
   is updated with dupe info.

 - Fixed band map getting out of synch with radio when using "home" or
   "end" cursor keys during band map edit.

 - Fixed LASTCQFREQ command not clearing call window.

 - Fixed LASTCQFREQ not switching to CQ mode when AUTO S&P ENABLE = TRUE
   for the most part. I have seen it switch to S&P, and I think it has
   something to do with the timing of the radio frequency polling.

 - Fixed mode being properly detected for CW reverse and RTTY reverse for
   ICOM radios.

 - TR will ask for Class and Section for Field Day setup. (Added to free
   version 1.04 based on TR version 6.68)

 - Fixed band map mode not switching properly when swapping radios.

 - Fixed band map not starting in same mode/band as TR with no radio
   connected.

Version 6.68 - released 10 June 2002

 - Added support for ARRL Field Day 2002 rules allowing Region 2
   participation. Now exchange can be class and/or QTH for DX.
   Nothing else has changed in how TR handles DX stations. Scoring
   will remain incorrect as it always has been.

 - Fixed multi info display overwriting band map info.

 - Added support for WRTC 2002. New contest type is "WRTC 2002". All
   scoring is supported including /MM and /AM as 2 points no mult.

 - Upgraded support for IARU contest as described below.

 - If using WYSIWYGDomestic as the domestic multiplier you can also
   specify a domestic multiplier file to provide a list of needed mults,
   but still enter any characters sent as an exchange. This works
   particularly well for the IARU Championship where you have a partial
   list of HQ stations you want to keep track of, but need to be able to
   work an HQ station sending an unexpected designator. ALT-G will toggle
   through the multiplier displays. This in now the default setting for
   the IARU Championship with IARUHQ.DOM set as the domestic multiplier
   file. This file with many known and recently active designators is
   provided.

 - Fixed bug where repeatedly hitting down arrow in CQ mode with nothing
   in the call window would cause strange things to happen.

 - Fixed last CQ frequency not being set when calling CQ by the enter key
   and an empty call window. This was not working even though it showed
   up in the band map.

 - Auto CQ frequency will now show up in the band map and be saved for
   the last CQ funtion.

 - Last CQ frequency and mode are now saved in RESTART.BIN. Older files
   will no longer function and will be ignored.

 - LASTCQFREQ command now switches the program to CQ mode when used.

 - The band map now functions quite well with no radio connected. The
   band map mode now tracks with the logging mode when no radio is
   connected. The band map used to always show CW mode when no radio was
   connected.

 - When loading the band map from the call window without a radio connected
   you no longer have to manually erase the call from the window to load
   another.  It now works just like when a call is put in the call window
   while tuning S&P with the band map enabled. Typing any character other
   than "enter" will replace the old call with the character typed. (This
   is much harder to describe than it is to just try and see what I mean).
   It saves one keystroke and was really added to help out the WRTC 2002
   competitors.

 - When asked to enter a frequency with no radio connected with the band map
   enabled you no longer need to enter MHz. It now works just like entering
   a frequency in the call window when controlling the radio. Example: When
   logging 20m entering either "230" or "14230" will load the band map with
   someone getting QRM'd by SSTV =;)

 - NEXTBANDMAP type commands now switch the program to S&P mode when used.

 - Fixed NEXTBANDMAP type commands not putting call in call window when
   something was in the window already.

 - Fixed band map not properly accepting or displaying VHF entries entered
   from within TR.

 - Added NEXTMULTBANDMAP function key command. Cycles through the mutliplier
   entries of current band and mode in the band map.

 - Added NEXTMULTDISPLAYEDBANDMAP function key command. Cycles through the
   mutliplier entries of the displayed band map.

 - Added NEXTMULTBANDMAP and NEXTMULTDISPLAYEDBANDMAP footswitch functions.

 - A connected radio will now return to simplex if a non split frequency is
   entered in the call window.

 - INSTALL.EXE will now copy FCONTEST.PAS to the install directory.

 - Fixed IARU initial exchange bug.

Version 6.67 - released on 10 May 2002 quickly for FIST Sprint!

 - Fixed band map range display when not showing WARC bands.

 - Added indicator to band map for dupes on/off.

 - Fixed name truncated in FISTS Sprint.

 - Fixed Cabrillo generator for New England QSO Party.

Version 6.65a - released on 28 April 2002

 - Fixed delay for SBDVP messages being recorded or sent. Version 1.02 of
   SBDVP now uses a unique identifier rather than the same on that K1EA DVP
   used. This allows me to steer around the code added just after 6.63 which
   broke SBDVP. You need to get the latest version from Kevin Schmidt, W9CF
   at http://fermi.la.asu.edu/w9cf/sbdvp/src/sbdvp102.zip

 - Put the DVPActive check back in for K1EA DVP.

 - Included the New England QSO Party DOM files this time =:-)

Version 6.65 - released on 26 April 2002

 - Added support for New England QSO Party. Includes DX multiplier limit
   of 20 for stations within New England! Should score properly. POST
   will generate cabrillo.

 - Temporarily removed check for DVPActive pending an update of SBDVP
   from W9CF. This check broke SBDVP and helped K1EA DVP.

 - Fixed CQ and EX MENUes to display even if more than 79 characters.

 - Band map fixes
     Fixed random garbage from showing up when tuned out of band.
     WARC bands now display properly (but after the 'contest' HF bands).
     Fixed calls with slashes not having dupe state set correctly.
     Fixed LOGCFG.DAT recognizing BAND MAP SPLIT MODE = BY CUTOFF FREQ.
     Fixed Home and End keys when editting large band maps.
     Cursor always returns to Home when changing display modes while editting
       the band map. Too many special cases to check for otherwise.

 - Band map improvements
     Added LOGCFG command CALL WINDOW SHOW ALL SPOTS. The default is false.
       When true in S&P mode all spots will show up in the call window
       as you tune by even if they are not being displayed in the band map.
     Added CTRL-Delete keystroke to delete current (highlighted) entry in
       the band map from the logging window. You can use this to delete
       undisplayed spots if they are in showing in the call window with
       CALL WINDOW SHOW ALL SPOTS = TRUE.
     Added CTRL-Insert "placeholder" keystroke. This cool feature will
       insert an entry in the band map with a unique 'call'. This is useful
       when S&P and you think the station is a new one, but the timing is
       wrong to call. With one keystroke you can mark the frequency and
       move on. The format of the 'call' is NEWmmss, where mm is the minute
       and ss is the second in the hour the frequency is marked.
     Changed the BAND MAP DECAY value to be any value up to 999 minutes!!
       That should satisfy even the most stale spotster =:-)
     When editting the band map the cursor is now placed at the current
       frequency if displayed.

Version 6.64 - released 07-April-2002 - major bandmap overhaul - KK1L.

 - Major band map fixes.
     Fixed no dupe display. No more squirrelly behavior.
     Fixed cursor navigation for all supported display options.
     Fixed Esc putting band map entry into call window when exiting map.
     Fixed a multiplier band map entry not becoming a dupe when worked.
     Fixed Alt-Y (delete log entry) so band map properly reflects the change.
     Fixed radio coming out of split mode when simplex entry.
     Fixed bug in save of QSX frequency. (QSX will be invalid from saved maps)
       Note: Some radios do not support changing mode of B VFO (Kenwoods). As
       a result the VFO mode will not update even though the freq is correct.

 - Major band map improvements.
     Added scrolling support when there are more entries than fit on the
       screen. Both tuning the radio and moving the cursor will cause the
       display to track. This works with the mouse too.
     Added hot key options. When editting the band map via CTRL-END hitting
       "D" or mouse button 2 will toggle dupe/no dupe display mode.
       "M" will toggle current mode/all mode display mode.
       "B" will toggle current band/all band display mode.
     Band map display now pays attention to VHFBandsEnabled and
       WARCBandsEnabled. (HF bands are always enabled).
     Added LOGCFG command BAND MAP SPLIT MODE. Choices are BY CUTOFF FREQ and
       AWAYS PHONE. Default is BY CUTOFF FREQ. This is an attempt to properly
       handle the mode of split band map entries. The default allows tailoring
       the ranges using the existing BAND MAP CUTOFF FREQUENCY command.
     Added NEXTDISPLAYEDBANDMAP function key command. NEXTBANDMAP cycles
       through the non-dupe entries of the current band and mode. This function
       cycles through all the displayed non-dupe entries.
     Added NEXTDISPLAYEDBANDMAP footswitch function.

 - Added option to ALT-T. Just hit enter without entering the time and if
   the network is active you will be asked if you want to pass time to other
   computers on the network.

 - After playing a Control-A message on inactive radio with DVP or DVK,
   the active radio was enabled.  This is now fixed so that the inactive
   radio stays enabled.

 - Fixed POST so that if you are trying to edit your TRMASTER file in
   a different directory than the one that the file is located in, the
   program will let you.

Version 6.63a - released on 24 February 2002

 - Really fixed sent exchange in ARRL DX.

Version 6.63 - released on 24 February 2002

 - When fetching a call from the exchange window with @ and CALLSIGN
   UPDATE ENABLE = TRUE, we now update the CALLSIGN I CAME BACK TO.

 - Fixed Alt-D not putting proper mult status into bandmap.

 - Fixed ARRL section so it can be different than the QTH sent for
   ARRL DX Cabrillo logs.

 - Added some more Icom Radios.  Here is a complete list now:

   IC706, IC706II, IC706IIG, IC707, IC725, IC726, IC728, IC729, IC735,
   IC736, IC737, IC738, IC746, IC746PRO, IC756, IC756PRO, IC756PROII,
   IC761, IC765, IC775, IC781.

 - Added K2 as a radio type.

 - Added ARRL RTTY ROUNDUP to startup menu.  Added Cabrillo support for
   it as well.  Removed zone multipliers. (Released POST 6.62.5)

Version 6.63 - released on 24 February 2002

 - When fetching a call from the exchange window with @ and CALLSIGN
   UPDATE ENABLE = TRUE, we now update the CALLSIGN I CAME BACK TO.

 - Fixed Alt-D not putting proper mult status into bandmap.

 - Fixed ARRL section so it can be different than the QTH sent for
   ARRL DX Cabrillo logs.

 - Added some more Icom Radios.  Here is a complete list now:

   IC706, IC706II, IC706IIG, IC707, IC725, IC726, IC728, IC729, IC735,
   IC736, IC737, IC738, IC746, IC746PRO, IC756, IC756PRO, IC756PROII,
   IC761, IC765, IC775, IC781.

 - Added K2 as a radio type.

 - Added ARRL RTTY ROUNDUP to startup menu.  Added Cabrillo support for
   it as well.  Removed zone multipliers. (Released POST 6.62.5)

Version 6.62 - released 1 January 2002.

 - Fixed Cabrillo for the Stew Perry contest.

 - Improved startup of Stew to ask for Grid Square.

 - Added IC706 and IC746.

Version 6.61 - released on 28 December 2001.

 - Fixed ARRL10 meter Cabrillo so ARRL Section not automatically used
   as sent exchange.

 - Fixed hang if putting entry into bandmap that isn't on a frequency
   in an amateur band.

 - Fixed band map entries not consistently making it into the call
   window when S&P.

 - Added QSO POINT METHOD = FISTS and EXCHANGE RECEIVED = RST QTH NAME
   AND FISTS NUMBER OR POWER.  Added FISTS contest.  One exception made
   is that DX mults are not counted again on different bands.  TR has
   no way to treat domestic and DX mults differently.  Since the mult
   status isn't communicated with the Cabrillo output - I assume the
   sponsors will correctly determine your multiplier totals and
   calculate the correct score.  However, there is too much information
   being exchanged in this contest and TR can't display it all.  Will
   work on a solution for the next release.

 - Allowed display of QSO status on other bands, even if MULTIPLE BANDS
   = FALSE when doing multi.

 - Fixed up the NRAU contest to use updated NRAU.DOM file.  Changed name
   of the contest to NRAU Baltic (Thanks SM3CER and ES5QX).

 - Added post support for Baltic contest.

 - Fixed FQP problems: Manatee mult, mult by mode (thanks N2CU).

 - Added BAND MAP DISPLAY CQ (Control-J, True).

 - Fixed QSO points for non W/VE, but ARRL sections when working DX.

 - Changed Minnesota QSO Party to use Name and QTH exchange.

 - Fixed contest name for ARRL September VHF contest.

 - Added Cabrillo WPX style support for AP Sprint and SAC contests.

 - Updated domestic file for YO DX (thanks SM3CER).

 - Changed name of the VK ZL Oceania contest to Oceania Contest.
   Added Cabrillo support using the WPX format.

 - Attempted to Icom type radios to same filter bandwidth as they
   were already in.  Please let me know how this is working and
   what rought edges remain.

 - Created the following radio IDs: IC707, IC725, IC726, IC728,
   IC729, IC735, IC736, IC737, IC738, IC761, IC765, IC775, IC781.
   The receiver addresses are properly initialized for these, so
   you don't have to set them anymore.  However, the radio ID
   commands still work in case you have changed them from the
   default - or need to support a radio not listed.

 - Made LookForOnDeckCall (in LogWind) handle / in first character.

 - Made \ commands only work when in call window (even though there
   aren't any currently enabled).

 - Added UNLIMITED category to Cabrillo routine for the SS.

 - Fixed Nu entry in P14.DOM file (which isn't actually used in any
   pre-programmed contest).

 - Added S50P14DC.DOM and used it for the ARRL 10 meter contest.

Version 6.60 - 11 November 2001

 - Ignore ESCAPE EXITS SEARCH AND POUNCE = FALSE if you have a call
   ready for a second radio QSO (Alt-D).  This allows you to use the
   escape key to abort a second radio QSO even if ESCAPE EXITS SEARCH
   AND POUNCE = FALSE.

 - Added FT817 support.

 - Added utility to POST to allow the times in your log to be changed
   by a fixed number of minutes.  This is very handy if you had your
   clock set wrong.  It will also fix the dates.

 - Increased the allowable length of the function key messages.

 - Some minor improvements to the two radio mode.  Using Alt-R will
   clear the InactiveRadioCallingCQ flag to give better results.
   Entering a callsign into the call window after a dummy CQ will
   prevent an automatic CQ from launching after the second radio
   QSO is logged.  Pressing RETURN will then answer that station.

Version 6.59 - 19 August 2001

 - When receiving a RST and SERIAL number exchange, you can increment
   a serial number in the exchange window by pressing I (for increment).

 - Fixed up SCP COUNTRY STRING functionality.  Only put the ! at the
   first character position if you want to exclude countries.  Use a
   comma between each country listed.  Spaces are optional.  For
   example:  K, VE, KP2, KP4, KH6, KL will include those six countries.
   !JA, K, VE  will exclude those three countries.

 - Fixed OnDeckCall after doing a CQ on the inactive radio, so that
   it comes out on the right radio.  OnDeckCall is a callsign that
   is entered in the exchange window with a / in front of it.  When
   this is done, the callsign will be put into the call window when
   you log the QSO.  You will have to hit ENTER again to call the
   this station.

 - Made Control-Enter when finsihing a two radio QSO not send CW.
   Instead, a CQ will be launched on the run radio.

 - Re-enabled digital mode in Cabrillo file.

 - Included the NAQP.DOM file that disappeared in the past couple of
   releases.

 - Allowed control characters in RTTY SEND and RECEIVE command by
   putting <> around two character hex value (i.e., <3F><FB><03> ).

 - Allow control characters to be specified for function key memories
   in the config file (using the above syntax).  These will not work
   if you use the Alt-P command.

 - Removed auto restart of auto CQ after a CQ mode QSO.  I should have
   trusted my original instints about this and never have put it in.

 - Fixed FQP exchange for RST and QTH.

Version 6.58 - 24 April 2001

 - Removed RTTY SEND CHARACTER and RTTY RECEIVE CHARACTER.  Replaced
   with RTTY SEND STRING and RTTY RECEIVE STRING.

 - Added FLORIDA QSO PARTY (or just FQP).

 - Changed GetLogEntryQSONumber to allow 5 digit QSO Numbers.

 - Improved responsiveness of footswitch for non PTT commands.

 - Fixed START SENDING NOW KEY = SPACE so space will work.

Version 6.57 - 11 March 2001

 - Fixed on deck call (\N6TR) being sent on speaker.
- Fixed ARRL 160 setup if you were in Hawaii/Alaska or US possession.

 - Added new LOGCFG command - COUNT DOMESTIC COUNTRIES.  Default is
   FALSE.  Lets you count DX countries for domestic QSOs in addition
   to the domestic multiplier.

 - Fixed up Russian DX contest QSO points, added CE9 as a domestic
   country and fixed initial exchange so it is blank for non russians.
   Also, made sure domestic countries count along with the oblast.

 - Added UBA Contest and QSO Point method.

 - Added CQ WPX RTTY contest - and CQ WPX RTTY QSO point method.

 - Improved immunity of Cabrillo generator to non QSO entries (like
   headers).

 - Fixed LOG WITH SINGLE ENTER config command problem (always on).

 - Made AutoCQs resume automatically after QSO.

 - Fixed PE (Prince Edward Island) in domestic files that have Canada.

Version 6.56 - 1 December 2000

 - Created packetmess startup command (not to be documented).

 - Fixed runtime 204 with invalid input for frequency.

 - Doubled length of DVK start pulse.

 - Fixed FT100 frequency reading and split operation.

Version 6.55 - 14 November 2000

 - Allowed Alt-D command to be process after entering a callsign
   and AUTO CALL TERMINATE.  However, the CQ EXCHANGE won't be sent
   until the Alt-D window is closed.  However, at least now you won't
   be entering the callsign into the call window.

 - Added DUPE SHEET ENABLE (Control-J - True).  When FALSE disables
   the addition of calls to the dupesheet.

 - When entering an exchange while making a second radio QSO with the
   Alt-D function, you may now enter a callsign of a station who is
   answering your "dummy" CQ.  You can enter it in the exchange window
   by putting a / in the front of the call (i.e., 12A 67NTX /N6TR).
   When you do this - there will be no CQ called at the end of the
   QSO and N6TR will be put into the call window ready for you to
   answer by pressing RETURN.

 - Control-] gives CT1BOH info screen.  This shows the time on
   distribution by band in minutes and percent.  It also shows
   you QSO distribution for Europe, NA and Asia by band with
   percent.

 - New RESTART.BIN version number.

 - Fixed blinking band map display - added band/mode in upper right
   corner.

 - If sending radio command with a message that gets ignored during
   transmit - and sending something in the same message - it is now
   fixed so that the CW won't start until after the transmittiers
   send buffer is empty.  (LOGSUBS2).

 - Added crash protection if you have a Control-C without Control-D in
   function key message. (LOGSUBS2).

 - Added SRSI to SRS commands to send inactive radio to chosen frequency.
   (LOGSUBS2).

 - Fixed TenTen for custom initial exchanges.

Version 6.54 - 26 October 2000

 - Fixed POST DAT file merge procedure.

 - Fixed PTT TURN ON DELAY just before sending exchange with auto start
   send/terminate.

 - Corrected call message not working in RTTY.

Version 6.53 - 3 October 2000

 - Added CQ WW RTTY contest and CQ WW RTTY QSO POINT METHOD

 - Updated VK/ZL contest.

 - Changed Salmon Run QSO Point Method to 4 CW and 2 SSB.

 - Added ALWAYS CALL BLIND CQ (default = FALSE, Control-J).  Will launch
   exchange memory F7 automatically on CW when the CQ EXCHANGE is finished.

 - Made CALLSIGN UPDATE ENABLE work when using @ in exchange.

 - Added NU to ARRL DX contest for DX stations.

 - Changed NO to NG in HUNGARY.DOM

 - Fixed "C" and "N" to resend call and number in WAE when using
   QTC EXTRA SPACE.

 - Made "UP" case insensitive when in the CALL WINDOW POSITION command.

 - Fixed PACKET SPOT KEY in config file.

 - Made START SENDING NOW KEY = SPACE only work when in the call window
   with at least one letter in the window.

 - Improved NAQP VE mults.

Version 6.52 - Released on 4 August 2000.

 - Added PACKET SPOT KEY (default = `).

 - Allowed SPACE for START SENDING NOW KEY command.

 - Added PACKET SPOT DISABLE (false, Control-J).  When TRUE, the ` key
   is disabled.

 - Added ability to program delay into DVK messages.  Use Control-C
   followed by DVKDELAY=xxxx and then Control-D in the message.
   xxxx is the number of milliseconds to delay.

 - Fixed up UKRAINIAN contest: now appears on menu, uses RST QSO NUMBER OR
   DOMESTIC QTC exchange, counts CQDXCC countries, puts UR in as a domestic
   country and has the contest listed on the menu.

 - Allowed 00 for Zone exchange in EU contest.

Version 6.51 - Released on 1 August 2000.

 - When TAB MODE = CONTROLF, I also made Shift-Tab work as Control-A.

 - Added OHIO QSO PARTY.  Make sure MY STATE = OH before your contest
   statement in the LOGCFG.DAT file to make it work for Ohio stations.

 - Added AP SPRINT contest (Asia-Pacific Sprint).

 - Put VE2 into ITU Zone 4.

 - Updated VE multiplier names in most of the .DOM files.

 - Added CALL WINDOW POSITION (Normal or Up - no control-J - Normal);

 - Exit editable log edit with downarror from bottom entry.

 - Made default RST work with RSTAndPostalCode exchange (ROPOCCO test).

 - Added LOG WITH SINGLE ENTER command (control-J - false).

TRFree Version 1.02 Released.

 - Fixed wrong IARU zone for MyZone.

Version 6.50 - Released on 4 June 2000.

 - Refined AddBandMapEntry to try and eliminate some of the record
   Disposal activity in a vain attempt to solve some RT #204 errors.
   Compiled with symbol stuff enabled in logwind.

 - Much Cabrillo work.

 - Fixed three letter call from getting blanked out.

 - Fixed Band/mode remaining mult window format when mult by mode.

 - Fixed ASK FOR FREQUENCIES with no input from crashing program.

 - Updated All Asian with new QSO point structure.

Version 6.49 - Released on 2 May 2000

 - Fixed garbage when deleting last entry in band map.

 - Fixed ARRL DX Cabrillo routine to support DX stations and multi two.

 - Fixed dupes in possible calls from SCP.

 - Fixed RT 201 at end of POST UEA or UEFF.

 - Improved LocateCall to instantly process the same call if it was the
   last call processed.

Version 6.48 - Released 29 January 2000.

 - Fixed Icom bugs in LOGK1EA.

 - Fix up split freq stuff in logwind.

 - Added WestCentralFlorida section (WCF).

 - Fixed runtime error 204 at 0CB9:017B (in SCP routine).  This was
   never in a released version.

 - Added load of country file when doing log prompt so the country stuff
   gets initialized okay.

 - Updated TenTenQSO Point Method.

 - Added SlowTree unit and made it an overlay.  Frees up about 24K.

 - New TRMASTER stuff to fix dupes and allow lots more calls in POST.

Version 6.47 - Released 13 January 2000.

 - Added NAQP.DOM for VY0.

 - Improved Base64Decoder.

 - Fixed Cabrillo format for putting space after number sent in SS.

 - Added BAND MAP ALL BANDS and BAND MAP ALL MODES.  Both Contol-J - False.

 - Added NEXTBANDMAP CW function key command.

 - NEXT BANDMAP and CW GRANT footswitch mode.

 - Added digital filter for Icom frequency reading.

 - Fixed multi/network crash by increasing stack size to 24500.

 - Removed LOGDUPE and LOGSTUFF from overlays.  Reduced overlay buffer to
   100K.

 - Totally reworked bandmap stuff - increased speed - reduce memory used.

 - Added BAND MAP DUPE DISPLAY to the config file commands.

 - Removed AutoStartSend arrow showing up after Control-J in S&P mode.

 - Made MULTI INFO MESSAGE change in Control-J be sent right away.

Version 6.46 - Released 21 November 1999

 - Increased the size of the overlay buffer.  This will result in
   slightly less free memory, but reduce the amount of activity of
   overlays being loaded.  On some slower machines - the new setting
   was too aggressive and resulted in poor performance.  This might
   also be the fix for some of the packet/netowrk issues seen by some.

 - Implemented new packet unit.  Improved QSX parsing.

 - Removed \ON and \OFF commands.

 - Changed .CAB Cabrillo file extension to .CBR.

 - Improved Alt-T to allow input of seconds.

 - Floppy error message not displayed.

 - Removed Icom transceive stuff from interrupt routine in LOGK1EA.

 - Added PACKETINPUTFILE startup command.  Next parameter is filename
   of ASCII file in format from OH2AQ queries.

 - Fixed JIDX summary sheet.

 - Fixed notes from creating strange partial call entries.

 - Fixed delay when starting a CW message with ALL CW MESSAGES CHAINABLE.

 - Fixed TWO RADIO MODE getting confused if you hit ESCAPE while
   the program is calling the station you did Alt-D on.  Also,
   add note to manual to make sure SPRINT QSY MODE is not used
   in SS with TWO RADIO MODE.

 - Moved display with AUTO DISPLAY DUPE QSO to after CW starts up.

 - Fixed blank line before TAB MODE entry in Control-J.

 - Fixed Radio Two Tracking enable display in Control-J.

 - Fixed radio type having to be upper case for radio one in logcfg file.

Version 6.45 - Released 24 October 1999?

 - Fixed POST Q C sometimes not processing names.

 - Multiplier POST TRMASTER improvements:
     - Made POST U E S merge duplicates.
     - Fixed bug with POST U E F V when not saving all calls.
     - Eliminated JA cell.
     - Maximum cell size 65000 - with error if anything bigger.
     - Fixed UEFF for non TR Log files and no data (just callsigns).

 - Added HEXCONVERT startup command.  Convert either Hex to Decimal or
   Decimal to Hex.

 - Fixed bug with SS start up information not being saved to config file.

 - Made ] send last RST even after QSO is logged.

 - Added initial attempt at Cabrillo format for SS and CQ WW.  Still some
   improvements to be made.

 - Added SWAP RADIO RELAY SENSE to allow the polarity of the radio control
   relay to be reversed.

 - Improved GetGrid defaults for YB and BY (Thanks VR2BG).

 - Fixed precedence question for startup for SS.

 - Fixed WAE bugs with QTCQRS and QTC EXTRA SPACE.

 - Fixed packet spots > 2 GHz from crashing program.

 - Fixed FT1000MP acting like FT1000 (1/16th frequency displayed).

 - Added ViewRadioDebug startup command.  Allows you to view the contents
   of a radio.dbg file (created with the startup command radiodebug) using
   the same algorithm that the program would use if it was interfaced to
   the radio.

Version 6.44 - 27 September 1999

 - Fixed Icom radios not displaying 100 and 1000 MHz characters.

 - Added BAND MAP DUPE DISPLAY (default = TRUE, Control-J).  When FALSE,
   dupes are not shown in the band map.

 - Made STDCFG.DAT commands execute before the .CFG file.

 - Added SWAP PACKET SPOT RADIOS (default = FALSE, Control-J) which will
   reverse which radio gets selected when sending a packet spot to a
   specific radio when using the Alt-U command and left or right keys.

 - Changed IC756 to operate like IC781 with respect to setting the narrow
   filter when updating frequency.

 - Fixed mults for OJ0 in SAC contest.

 - Improved default grid calculations for bunch of countries which improves
   the accuracy of sunrise/sunset, distance and beam headings.  Countries
   that now use prefix include CE, CP, EA, EU, HK, JA, K, LU, OA, OH, PY,
   SM, UA, UA9, UK, UN, UR, VE, VK, XE, YV, ZL, ZP and ZS.  UA, UA9, UK, UN
   and UR use full oblasts for even better resolution.

 - Disabled AUTO SEND CHARACTER COUNT feature if the call entered so far
   has / in it.  This keeps it from starting for calls like JA1/WB6ZVC.

 - Fixed REMINDERS and TOTAL SCORE MESSAGES in config file.

 - Attempted to fix bug with band map call sometimes blank in call
   window.  For those of you who have seen this - please let me know
   if I have succeded.

 - Updated SWEEPSTAKES to accept U M and S for precedences.

 - Fixed jerky CW with Keyboard Sending if radio interfaced.

 - Fixed PADDLE BUG ENABLE command in config file.

 - Fixed DIGITAL MODE ENABLE entry into config file when being saved
   in Control-J menu.

 - Made IOTA summary sheet ask if you were at home or DX-pedition.

 - Added UKRAINIAN contest and QSO POINT METHOD = UKRAINIAN.

 - Added BALTIC contest and QSO POINT METHOD = BALTIC.

 - Updated Russian contest - HFC - KVP - CROATIAN contests.

 - Added QTC QRS (Control-J - True) will slow down QTCs being sent.

 - Added QTC EXTRA SPACE (Control-J - True) will add extra spaces to
   QTCs being sent.

 - Made ESCAPE KEY abort sending of "QTC?" message.

 - Made ControlEnter and AltK key work when receiving QTCs.

 - Allowed cursor keys to edit QTC received message.  Also, INSERT and
   the cursor control keys (ControlA, S, D, F, G and T).


 - Fixed sending QTCs to a station that is contained in the QTC.

Version 6.43 released on 18 August 1999.

 - Updated HFC contest and improved defaults.

 - Made Domestic mult status from initial exchange use last entry first.

 - Fixed SENDCW command.

 - Fixed ARRL FD exchanges coming from TRMASTER when using simulator.

 - TUNE FOR DITS (control-J false).  Sends dits at 75 WPM when left shift
   and control keys depressed.

 - Fixed floppy save crashing.  Also improved its speed if more than
   65K of memory is available.

 - Reworked much of the memory stuff in LogSCP.  Fixed problem wtih
   Dispose (FileBuffer) in LogSCP when disabling by application.

 - Improved memory usage of TR (moved LOGDUPE and LOGSTUFF to overlay).

 - Fixed POST not allowing you to specify log name if no .CFG files.

Version 6.42 released on 5 July 1999.

 - Undid problem with IARU and countries created in 6.30.

 - Fixed cursor in wrong window after sending keyboard CW.

 - Fixed QUESTION MARK CHARACTER and SLASH MARK CHARACTER to work
   even if USE BIOS KEY CALLS = TRUE.

 - Made REGION ONE FIELD DAY /m stations count as /p stations.

 - Made initial exchanges for FD come up using MY FD CLASS and MY SECTION.

 - Fixed FT100 frequency display.

Version 6.41 released on 7 June 1999.

 - Added KIDS DAY contest.  Simply logs whatever you put into the
   exchange window.  Added KIDS exchange received.

 - Added USE BIOS KEY CALLS - (default = TRUE, Control-J).  When FALSE,
   TR uses its own routines to detect and read key strokes.  When TRUE,
   TR will use BIOS calls to get this information.  This may mean that
   the F11 and F12 keys won't work.  However, it will hopefully allow
   other programs that are looking for keystrokes to work with TR.

 - Fixed SCP crash if you enter ///.

 - Improved memory efficiency of TRMASTER stuff - plus allowed much bigger
   cells if you have the memory for it.

 - Fixed bug with .DTA file merge hanging (POST U E F D).

 - Fixed POST U S (restart file summary) to work with new log files.

 - Made file entries that don't look like a grid be ignored during UEFF.

 - Fixed inability to add K1OJ's name to the database.

 - Fixed POST U S (generate summary of restart file) command to work with
   new filenames.

 - Made POST U E F F be able to process up to 10 items at once.

 - Made POST less memory hungry.

 - Added buffer for keyboard CW with display.  Supports backspace or delete.

 - Allow HELLO.DAT in same directory as other files (CTY.DAT).

 - Fixed QUICK QSL KEY 2 command (was setting value for KEY 1).

 - Made Control-U use call in window string if CALLSIGN UPDATE ENABLE
   was enabled.

Version 6.40 - Big Dayton Release - 10 May 99

 - Improved showing of beam headings for grids during entering in exchange
   window - also when coming from TRMASTER as initial exchange.

 - Made initial exchanges from TRMASTER that are DomesticQTHs show their
   mult status when coming up in the exchange window.

 - Allowed TR to be started up with the prefix of a config file specified
   which will automatically start that contest up (no selection screen).

 - Fixed sent RST not being used with TR READ.

 - Made /P calls not drop the /P when doing lookups in TRMASTER except
   for name or FOC numbers.

 - Fixed IS0 with wrong QSO points and exchange in ARI test.

 - Fixed NAME FLAG ENABLE not working (name flag was always enabled).

 - Fixed VE0 in RAC contest so it takes a serial number, but still counts
   for 10 points.

 - Created P13.DOM with VY0 added for RAC contest.

 - Added ROTATOR PORT and ROTATOR TYPE (DCU1 or ORION).  The ORION PORT
   command is still supported.  Use Control-P to send the rotator to
   the last displayed beam heading.

 - Added FT100 to radio list.

 - Fixed USER 4 and USER 5 overwrite change commands (was affecting
   USER 3 status).

 - Added BAND MAP DECAY TIME (control-J, default = 60) in minutes.  Note
   this applies to entries as they are added to the band map.  Entries
   made with a different value will still decay with their old time.

 - Added RST PREFECTURE as legal value for EXCHANGE RECEIVED.  This is
   used for the JA INTERNATIONAL DX contest.  Allows integers to be
   entered which have a "p" added to the front of them so they work
   as domestic multipliers.

 - Changed prefecture multipliers for JA INTERNATIONAL DX contest
   domestic multipliers similar to ALL JA contest.  Improved visability
   of remaining mults during QSO process.

 - Allowed six digit grid squares to be entered instead of four, but
   still use four digits for the mult.

 - Added PACKET LOG FILENAME command (control-J).  When set to a filename,
   all packet spots received will be saved to this file.

 - Fixed RANDOM CQ MODE messing up the Control-J menu.

 - Fixed DKT in OKOM.DOM to DKL.

 - Updated intial CQ EXCHANGE for ROPOCO contest to 5NN ).  Eliminated
   postal code form start up questions.  Added ROPOCO.DOM file and enabled
   domestic multipliers.

 - Added EUROPEAN SPRINT contest.

 - Added EUROPEAN SPRINT as legal QSO POINT METHOD.

 - Added QSO NUMBER AND NAME as legal EXCHANGE RECEIVED.

 - Added AUTO RETURN TO CQ MODE (Control-J - default = TRUE).  When TRUE
   and you press the RETURN key in S&P mode - with the cursor in the call
   window AND no information in the call or exchange windows - you will
   be put into the CQ mode and CQ MESSAGE F1 will be initiated.

 - Fixed All Asian and All JA entries in log prompt.

 - Eliminated prefectures 115 to 118 in All JA.

 - Pretty much gave up on Aurora stuff.

 - Fixed sent RST not working when using [ character in exchange.

 - Fixed LOOK FOR RST SENT not showing up in Control-J menu.

 - Added Alt-N option for sending Control-J parameter to network.

 - Fixed missing exchange for MICH QSO PARTY outside of MI.

 - Released a special 6.39 to Tack with All JA improvements including
   remaining display, made precedence H/M/L/P and added prefectures 48-50.

Version 6.39 released on 15-Apr-99.

 - Updated IOTA QSO Point method to new 15/3 formula.

 - Attempted to fix crash with big packet frequencies.

 - Made ZZ00 not be accepted as a legal grid in exchanges.

 - Made notes in LOGCFG file that start with ; require space before the ;.

 - Fixed garbage in call window when using Control-B command with
   Auto-CQ active.

 - Fixed turning on of DVP in Control-J.  Made DVP ON disappear when
   turning off DVP from Control-J.

 - Made PTT HOLD COUNT of 0 not "hang" PTT.

 - Removed RST ALL JA PREFECTURE AND POWER exchange and replaced it
   with RST ALL JA PREFECTURE AND PRECEDENCE exchange since the ALL JA
   contest now uses a single letter (A, B or C) to identify power.

   Allows most any order for exchange information, but no duplication
   of entry data allowed.  57923C will work as well (RST 579, Pref = 23
   and power = C).  On CW, 23 579 C will also work, but on SSB, it won't.

 - Fixed summary sheet not coming up for Russian contest.

 - Added ask for your state if selecting any of the QSO parties from menu.

 - Added MIGHIGAN QSO PARTY (OR MICH QSO PARTY).

 - Changed MQP to MN QSO PARTY (to avoid confusion with MICH QSO PARTY).

 - Updated SP DX contest for non SP stations.  Added SP.DOM.

 - Added LOOK FOR RST SENT (false - Control-J).  Allows sent RST to be
   entered with S prefix in exchange field.

 - Allowed for aurora RS(T)s on 6 and 2 meters for both sent and received.

 - Added RADIUS OF EARTH parameter (default = 0).  Enter in meters.
   When something is entered, the default internal to the program will
   be overwritten.

 - Fixed bug when trying to set a key (like start sending now) to ;.

Version 6.38 - 16 March 1999.

 - Fixed domestic filenames for Wisconsin QSO Party.

 - Fixed WWL contest multipliers (Grid fields instead of grid squares
   and MULT BY MODE = FALSE).

 - Allowed ESCAPE EXITS SEARCH AND POUNCE in addition to ESCAPE EXITS
   SEARCH AND POUNCE MODE.

Version 6.37 - 11 March 1999.

 - Added ESCAPE EXITS SEARCH AND POUNCE MODE (false - control-J).

 - Added RSGB QSO point method (my country = 0, Europe = 5, Oceania = 30,
   else 15).

 - NAME FLAG ENABLE (default - true - controlJ).
 - Fixed bug with extra CW being sent with QSL BUT NO LOG option.
 - Added - to send 1.5 dah length (see Dat in LOGK1EA.PAS).
 - Fixed summary sheet not working for Croatian and Minnesota QSO Party.
 - Added WISCONSIN QSO PARTY (or WQP).
 - Fixed band map cursor if multi info window is up.
 - Added brace character to send complete corrected callsign in call ok message.
 - Fixed null character appearing in notes.
 - Tried to fix missing grids from band map updates in Stew Perry.

Version 6.36 - December 7 1998.

 - Made POST come up with selection menu if more than one contest.
 - Added TR NEW to startup new contest.
 - Turned off exchange memory enable for ARRL 10 contest.
 - Added automatic CW messages to ARRL 10.
 - Made MY QTH which acts the same as MY STATE in LOGCFG.DAT.
 - Made band map get updated with new mult status when working a mult.
 - Fixed bug with QTCs when you get close to 600 of them.
 - Made Control-L give you the choice of looking at QTC.DAT file if QTCs.
 - Improved some keystrokes in WAE QTC.
 - Added note to log when QTC completed.
 - Fixed up WAE QTC init during program restart.  Ignores zero point QSOs.
 - Added CW commands SRS SRS1 and SRS2 with after = command to radio.
 - Fixed Alt-= hanging with CW TONE when turning it off but key down.
 - Fixed last total line of POST L C not showing up.
 - Not allow JA Prefectures > 50.
 - Went to European distance for European VHF QSO Point method.
 - Maybe improved accuracy of distance calculations.
 - SCP COUNTRY STRING allows ! or - at start to make it exclude.
 - STDCFG.DAT.
 - Made POST's L P (pull computer ID into log) command process notes.
 - Added computer ID to notes added with ControlN.
 - Made " clear RIT.
 - Fixed POST's R L from crashing.

Version 6.35 - 2 Dec 98.

 - Fixed up several POST report procedures to prevent crashses.
 - Made TR Read pass computer ID.
 - Allowed space for computer ID in POST L P command.

Version 6.34 - 20 Nov 98 - also TRP for N5RZ.

 - Added MODEM PORT.

 - Added routine to detect LOGCFG.DAT file and convert to new format.
 - Changed to contest specific config and log files in TR.

 - Fixed Control-F1 not working for DVP and DVK program.

 - Fixed up DVK and two radio stuff.

 - Changed WPX QSO Point method to give one point for own country (regardless
   of band).

Version 6.34 beta - to W6QHS on 17 Nov.

 - Added support for sending exchange in two radio mode with DVK.

 - Fixed Exchange F1 not being sent when pressing RETURN with call in
   call window in search and pounce.

 - Changed LOG FILE NAME command to use SetUpFileNames.
 - Allowed ControlF1-F10 to also program DVK (not just DVP).

Version 6.33 - Released on 5 November 1998 (in time for JIDX)

 - Went to far calls for everything.
 - Fixed JIDX so you can enter prefectures 41 to 50.
 - Implemented MY PREC, MY CHECK and MY SECTION.
 - Added complete exchanges for SS if using MY PREC, MY CHECK and MY SECTION.
 - Fixed MY POSTAL CODE not being saved to config file upon start up.
 - Eliminated possibility of INSERT window showing up with country name.

Version 6.32 - Released on 30 October 1998

 - Created POST U E C to allow clearing out a data field in TRMASTER file.
 - Fixed USER4 and USER5 for USER INFO SHOWN.
 - Fixed USER4 and USER5 for initial exchange.
 - Improve startup of ROPOCO and standard messages.
 - Added SCP COUNTRY STRING - Control-J - null string.
 - Added Description function to LogSCP - saving about 7K of code.
 - Made IOTA QSO Point method to count 2 points if island in my country.
 - Made band map edit go back to last edit position.
 - Fixed mult and QSX flag on band map entries disappearing when highlighted.
 - Band map edit fixed up.
 - Band map fixed up when displaying all bands.
 - Added WAG contest.
 - Added WAG QSO point method.
 - Made random CQs ignore messages that are blank.
 - Updated CTY.DAT file.
 - Made ROPOCO test allow space in middle of postal code.
 - When using LastName - will skip up above blank log entries now.
 - Made European / Non-Scandinavian QSOs on low bands count 1 point, not 3.
 - Added QSO Point scoring for VK/ZL stations in VK/ZL contest.
 - Improved LastName to accept all numbers in Sprint.
 - Improved contest selection menu to allow you to start entering name.
 - Change TenTen QSO Points to 3 (DX & TenTen), 2 (not DX & TenTen) and 1.
 - Made NAQP process QTH while entering into exchange - even with name.
 - Fixed Chapter working in initial exchanges.
 - Fix for DX entries in Eu Sprint.

Version 6.31 - Released on 7 October, 1998.

 - Fixed RST DOMESTIC QTH to do number correctly if > 2 entries.
 - Severe surgery to POST - with log file text files reformatted.
 - Fixed DIG problem with POST.
 - Removed N6TR Duping file generation utility.
 - Eliminated possibility of 0460Z as a sunrise/set time.
 - Fixed up CALQSOW6.DOM so only one CA.
 - Added PADDLE SPEED command (default = 0 & Control-J).
 - Added COAX startup option.
 - Made 10 meter QSOs in VK/ZL worth 3 points (was 2 points).
 - Made ExchangeMemoryEnable control initial exchanges from editable log.
 - Made MY STATE get used for initial INTERNET SPRINT exchanges (not ORE).
 - Fixed garbage contest name in log prompt.

Version 6.30 - Released on September 23, 1998.

 + If doing Alt-c, then do Control-B and exit, then display screwed.
 + Is Question mark character in the manual?
 + Quick grab of spot then back to run freq - use previous CQ freq.
 + Side band changing on MP.

 - PACC works for PA stations now.
 - Domestic mults for PACC not working right when scolling off screen.
 - Fixed GetPrefix in Tree so F/N6TR gives F0.
 - Added PACC COUNTRIES AND PREFIXES as a DX MULTIPLIER type.
 - Improved RSTAndDomesticOrDXQTH to take its que from DOMESTIC COUNTRY.
 - Made QTH STAMP print country if no DX mults to deal with.
 - Added SALMON RUN contest.
 - Added QSO POINT METHOD = SALMON RUN.
 - Added MINNESOTA QSO PARTY (MQP).
 - Added QSO POINT METHOD = MQP (2 ssb, 1 CW and 10 for W0EF).
 - Added WORLD WIDE LOCATOR (WWL) contest.
 - Added RST AND GRID exchange.
 - Added WWL QSO POINT method.
 - Enhanced sprint exchange parser.  Allowed xxx yyy zzz # name QTH and
   also multiple number entries at end (uses last one).

 - Added asking for name/qth when starting sprint up.
 - Added RADIO ONE TRACKING ENABLE and RADIO TWO TRACKING ENABLE (TRUE & ^J).
 - Fixed Alt-F keys not sending message in CQ mode.
 - Fixed initial exchange when using tail end key.
 - Fixed POST R L to do any band/mode.
 - Fixed USER 4 and USER 5 not working for USER INFO.
 - Added AN as legal prefix for IOTA designator.
 - Made summary sheet work for REGION 1 FIELD DAY as well as REGION ONE.
 - Eliminated crash if hit Shift-Tab during middle of CQ QSO.
 - Fixed SendCW start up command.
 - Made POST LC handle WARC and VHF/UHF bands.
 - Allow 57600 baud for radio interface.
 - Fixed FT840 support (was broken by adding the 847 in 6.29).

Version 6.29 - 24 July 1998

 - Added MY IOTA - needs to be set to make QSO points work.
 - Fixed QSO points for IOTA for working own island.
 - Fixed ROPOCO QSO POINT Method.
 - Fixed 80 characater limitation to ARRL Log format.
 - Added CQ VHF back to log prompt list.
 - Moved YT from NW to YU in P12.DOM.
 - Added summary sheet for ROPOCO contest.

TRFree 1.01 - 5 July 1998

 - Added conditional compiler directives to make TRFree easy.

Version 6.28 - 5 July 1998

 - Fixed hang of POST UEFF if lower case character in call.
 - Improved ESCAPE action when doing overwrite flag status in POST UEF.
 - Added PREFIX INFO FILE NAME - but it doesn't do anything!
 - Added TEN POINTS PER QSO as a QSO POINT METHOD.
 - Made Digital mode and radio interfaces work.
 - Added MY POSTAL CODE (null string).
 - Added MY FD CLASS (control-J - null string).
 - Added ARRL FD QSO point method.  1 for SSB, 2 for CW, zero for 1D to 1D
 - Added ROPOCO contest (UK Rotating Postal Code).
 - Added RST AND POSTAL CODE exchange type.
 - Made POST's L C command handle WARC and VHF/UHF logs.
 - Improved Icom interface to handle Transceive mode input.
 - Fixed problem with dupesheets not being done for all bands.
 - Added PORTTOFILE start up command.
 - Added IOTADomestic multiplier type.
 - Made GetDomQTH routine parse IOTA designators better (EU5 -> EU005).
 - Made time display get updated when sending from keyboard.
 - Fixed up Region One Field Day for OZ and UK.
 - Fixed bug with SCP not working all the time.
 - Move grid map with Control-Left and Right keys (sorry - no up/down).
 - Made date work back to 1900 (was 1988).

Version 6.27 - 11 June 1998

 - Added MULTI PORT BAUD RATE (default - 4800).
 - Made RIT only active on Kenwoods.
 - Added BAND MAP ALL BANDS (contro-J - False);
 - Fixed bug with callsigns sometimes disappearing when pressing SPACE BAR.
 - Made Region 1 Field Day in UK count double QSO points for 160 & 10.
 - Fixed problems with lower case in grid squares.
 - Made TR READ not make dates in upper case.
 - Fixed possible dupes in partial call list.
 - Added LOGLASTCALL function key command.
 - Fixed display of prefixes during get zone routine.
 - Fixed bug with > 6 character calls not working with partial calls.
 - Fixed bug with beeping computers when calling CQ and network.

Version 6.26 - 24 May 1998

 - Fixed packet spots over network not being processed.
 - Added sent information to ARRL log process.

Version 6.25+ - 11 May 1998  (Released as 6.25) - Dayton version.

 - Changed REGION ONE FIELD DAY to make mults CQ DXCC and MULT BY BAND.
 - New CRT.TPU and CRT.TPP to fix Runtime 200 problems.

Version 6.25 - 5 May 1998

 - Tried to fix 300 MHz Pentium II POST operation.
 - Put up menu of contests during prompt operation - uses arrow keys.
 - Made REGION 1 FIELD DAY work for REGION ONE FIELD DAY.
 - Made JIDX work for JA INTERNATIONAL DX contest.
 - Added WARC bands to POST's continent report.
 - Added RUSSIAN DX contest.
 - Added RUSSIAN DX to QSO point method.
 - Fixed clean up of country name and insert windows if dupe.
 - Fixed crash of ControlJ if use PageDown at end of list.
 - Eliminated QRP adder for JA INTERNATIONAL DX test.
 - Fixed ZoneMode for JA INTERNATIONAL DX (to CQ).
 - Added DIGITAL MODE ENABLE (Control-J - default = False).
 - New ARRL Field Day rules implemented.
 - Improved POST L M for Helvetia contest.
 - Change RSTQSONumberAndPossibleDomesticQTH to ignore a QTH entry for DX.
 - Above fixes TR READ not working for Helvetia contest.
 - Fixed POST crashing during mult check.
 - Fixed QSO Number not updating when band change and QSONumberByBand.
 - Fixed N6RO crashes - low level buffer stuff in Tree.
 - Added PACKETFILE startup command.
 - Fixed JA1 backspace backspace backspace JA1 not working with SCP.
 - Fixed Alt-D band map entries (were on active band with no call).
 - For Region 1 FD, made any unknown country act like DL 2/3 4/6.

 - Fixed 201 errors because of uninitialized mode after calling calculate
   band/mode when on 160.

 - Fixed 201 error when bogus long partial call.

 - Fixed Tuning bug (uninitialized CWBuffer).

Version 6.24 released on 22 March 1998.

 - Fixed distance calculation error introduced with 6.23.
 - Fixed total score always being zero.
 - Fixed the beam heading window problem.

Version 6.23 released 21 March 1998

 - Added multi info display to bottom of bandmap if it is enabled.
 - Used NewReadKey in more places in LOGSUBS2 so Question Mark Char works.
 - Made ? work in tail ends.
 - Allowed question mark to be used during tail end.
 - Reduced memory consumption from 6.22 by 35.7K!!!
 - Fixed packet not flagging mults correctly in bandmap.
 - Fixed RT 215 from trying to do beam heading to unknown country.
 - Fixed RT 215 in ElaspedTimeSec100 (2716:1c9d).
 - Fixed some other RT 215's
 - Made Debug and CWTone = 0 changes bands occasionally.
 - Fixed bug with Partial calls in SCP trying to do stuff after disabled.
 - Improved CompressThreeCharacters so it can't generate bogus results.
 - Really speeded up TR DEBUG for CW TONE = 0.
 - Fixed a number of undetected range overflows.
 - Band map shows happy space for non dupe multipliers (Country/Prefix/Zone).
 - Moved WAEQTC to own unit - removed process function key for now - 5K saved.
 - Fixed bug where FreeMemory didn't update with ESCAPE in SSB mode.
 - Created LogDDX units - 21K memory savings!
 - Removed random delays in TR READ function.
 - Fixed bug with not reading in QSO points correctly from LOG.DAT file.

Version 6.22 released on 6 February 1998

 - Fixed band changes this hour for multi network.
 - No overwrite of ? characters in INSERT mode.
 - Cursor always goes to ?, even if in INSERT mode.
 - Added FINDFILE start up command.
 - Added HELP start up command.
 - Allowed second ESCAPE to exit you from packet edit entry.
 - Increased size of prefixes in CTY.DAT to 7 characters.
 - Fixed possible DVP init bug.
 - Made ` use last QSO's call if callwindow blank.
 - Fixed bug with inactive CW Speed not being used for Control-A messages.
 - Fixed bug with distances in European DX contest.
 - Added CQOpMode status.
 - Allowed DownArrow to be used to enter exchange before call in CQ MODE.
 - Cleaned up memory dump of TRMASTER some.
 - Changed to S48P13DC.DOM for CQ 160.
 - Fixed bug with / stations not being called by name.
 - Allowed editable window to edit initial exchange and possible calls.
 - Re-arranged where the big CASE statement for the exchange processor is.

Version 6.21 released on 16 February 1998.

 - Added BAND CHANGES to HOUR DISPLAY.
 - Fixed ARRL DX remaining display if prompted for your call.
 - Made PACC QSOByMode = FALSE;
 - Improved sprint parser to handle 4 or 5 entries.
 - Made Sunrise/Sunset use MyGrid if Call = MyCall.
 - Added RADIO ONE ID CHARACTER and RADIO TWO ID CHARACTER - ^J - Chr (0).
 - Fixed problem with DX QTH showing up in next QSO mult with 2 radios.
 - Fixed JST for radio two.
 - Added sunrise/sunset time automatic in the beam heading window.
 - Changed default background color for beam heading window to white.
 - Extended beam heading window over 10 spaces to the left.
 - Fixed bug with Alt-D QSO leaving cursor in call window when space bar.
 - Made Alt-D work on SSB even if the DVP is not enabled.
 - Tried to make the program less memory hungry.
 - Went to more accurate distance calculator.
 - Fixed BROADCAST ALL PACKET DATA not supported in LOGCFG.
 - Made Packet Window and Control-J timeouts only work when using multi port.

Version 6.20 released on 5 January 1998.

 - Fixed CW not sending callsign I came back to.
 - Added CountryInformationFile.
 - Added LastCountryCall feature to GetCountry.
 - Removed beep when sending radio information to other station.
 - Made ESCAPE in CQ MODE with no entry in call window repaint editable log.
 - Added ControlHome to view last five multi talk messages.
 - Made message aborted with ESCAPE key not get sent.

Version 6.19 released on 4 January 1998.

 - Added MULTI MULT UPDATE DISPLAY (default = false - Control-J).
 - Added 20 second timer to keyboard input.
 - Added F1 footswitch mode.
 - Check CQ Zones <= 40.
 - Made SCP abort if key pressed.
 - Made SCP only happen if more characters not waiting.
 - Made POST's R Q case sensitive.
 - Allowed DUPECHECK or DUPE CHECK for footswitch mode.
 - Improved ControlEnd to set you up to use RETURN to call station in S&P.
 - Fixed dualing CQs on CW.
 - Added CROATIAN contest and CROATIAN QSO point method.
 - Added TEN MINUTE RULE (NONE, TIME OF FIRST QSO).
 - Fixed FT1000MP again.
 - Added SKIP ACTIVE BAND (Control-J - false).
 - Added SPEED - Function Key Message command.  Either value or + or -.
 - Allowed W M and R in "power" for ARCI contest.

 - Improvements to DVK operation:
     o Escape will abort any message (not just F1).
     o DVK0 now works in message (was broken).

Version 6.18 released on 6 December 1997.

 - Added ARCI QSO POINT METHOD for ARCI.
 - Added RST POSSIBLE DOMESTIC QTH AND POWER exchange for ARCI.
 - Switched mode and frequency for MP back the way it was (freq then mode).
 - Prevented possible crash if packet frequency > 2,147 MHz.

Version 6.17 released 23 November 1997.

 - Fixed S&P exchange sent using F2 after the QSO with no entry.
 - Made CONTEST NAME part of RESTART.BIN so if it changes, gets ignored.
 - Got rid of OH2MM's special dot and went to a dash.
 - Went to DisplayedFrequency for ParametersOkay calls - no more zero?
 - Improved transitions between radios or messages with DVP.

Version 6.16 with DVP fixes (sent only to K5TR).

 - Fixed dualing CQ with DVP.
 - Reversed order of sending mode and frequency to FT1000/920/MP.

Version 6.16 - Released 11 November 1997.

 - Fixed SS exchange with space at end being viewed as a call.
 - Made Two radio stuff all work with DVP.
 - Extended band output port to work up to 1296.  See LOGK1EA.PAS.
 - Fixed overflow when total QSO points for a log gets over 32K.
 - Fixed BandMap showing dupes when calling in S&P.
 - Added Control-PageUp and Control-PageDown to adjust inactive radio speed.
 - Changed bandmap access key to Control-End.
 - Added DUPECHECK to footswtich modes.
 - Fixed PTT not working if you use special dits/dah only in message.
 - Added HOUR DISPLAY command (control-J - default = THIS HOUR) - LAST SIXTY MINUTES

Version 6.15 - Released 29 Oct.

 - Fixed Y and Z in grids (not allowed now).

 - Fixed multiple back saves with same QSO number so the A B C are there.

 - Added COPY FILES command to LOGCFG.DAT = SourceDir FileMask DestDir.
   Use . for current directory.

 - Fixed DVP messages not being copied to DVPPath if it was set with the
   DVP PATH logcfg.dat command.

 - Asked if DVP messages at DVPPath should be copied to current directory
   when exiting the program.

 - Made packet spots in CW/SSB gray areas just keep ActiveMode.
 - Eliminated first two letters of country name from getting erased.
 - Added CWMONITORON and CWMONITOROFF function key commands.
 - Added PACKET AUTO CR command (control-J - false) - CR when exit ^B.
 - Added TRACE startup command.
 - Fixed terrible packet bug.
 - Some clean up of .DOM files.
 - Added PACKETSIMULATE start up command.
 - Added PASSTHROUGH start up command - ties two serial ports together.

Version 6.14 - Released 23 October.

 - Used Wait instead of Delay in wait loop in DVP.

Version 6.13 - Released on 22 October.

 - Split info from SH/DX command.
 - Control-O menu support.
 - Added PACKET SPOT EDIT ENABLE (default = FALSE - Control-J).
 - Added QSX ENABLE (default = TRUE - Control-J).
 - Added QSX frequency support to band map.
 - Fixed FT920 for radio 2.
 - Fixed TAIL END MESSAGE not being written correctly to LOGCFG.DAT file.

Version 6.12 - Released on 12 October.

 - Updated QSO points for VK/ZL contest.
 - Added ControlE command if your multi message <> null string - display.
 - Added MULTI INFO MESSAGE.
 - Added HP startup command.
 - Added LC startup feature (calculates inductance or capacitance).
 - Added SERIAL 5 PORT ADDRESS and SERIAL 6 PORT ADDRESS (hex).
 - Allowed SERIAL 5 and SERIAL 6 ports to be used for anything.
 - Fixed inc/dec speed at extremes.
 - Fixed no PTT from F10 if FootSwitchMode = NORMAL.
 - When in S&P - if you press F1 or CR to call station - clear exchange sent.
 - Made a few changes to CALQSOW6.DOM.
 - Added ORION PORT.  Use Control-P to send it there.
 - Added warning about unmarked dupes not having correct QSO points POST L D.
 - Fixed ControlA/ControlB for DVP.
 - Made Grid Map update after Alt-E.
 - Fixed FM mode display.

Version 6.11A - Released on 10 September 1997.

 - QSO points fixed for SAC.

Version 6.11 - Released on 10 September 1997.

 - Fixed scoring for SAC contest if in SAC.
 - Made visible dupesheet update with Alt-R.
 - Much improved operator interface to dualing CQ mode when it is on.
 - Added SEND COMPLETE FOUR LETTER CALL (control-J - False) - corrected calls.
 - Made original logfiles in POST use PLOG###.BAK where ### increments.
 - Fixed Band map restart not working.
 - Fixed spaces in duping sheet file names.

Version 6.10 - Released on 29 August 1997

 - Fixed dupesheets for VHF/UHF and WARC bands.
 - Showed active radio at power up.
 - Made CQ EXCHANGE work with DVK.
 - Made character in Auto-CQ abort CQ in progress.
 - Added TAB MODE (NORMAL or CONTROLF) - Control-J - normal.
 - Added CLEAR DUPE SHEET (only works when running cfg file with Control-V).
 - Increased size of name window by 2 characters.
 - Improved on screen instructions when executing Control-J command (2 lines).
 - Made Alt-G in Control-J write to specified file.
 - Fixed problem with active radio flicking PTT when sending on inactive rig.
 - Fixed problem with two radio keying if using same port.
 - Added MESSAGE ENABLE (control-J - false) - controls Alt-P - O messages.
 - Added ability to enter tenths of frequency in call window.
 - Fixed runtime 204 error in band map (memory locations from bandmap.bin).

Version 6.07 - Released to K5TR on 21 August for testing - no POST changes.

 - Fixed visible dupesheet spurious calls.

 - Changed Control-V command to execute additional config file.  Use
   INSERT key for old function (turning on/off insert mode).

Version 6.06 - Released to K5TR on 20 August for testing - no POST changes.

 - Fixed NORMAL footswitch mode so CW can be sent.
 - Fixed IARU default exchanges to use IARU zones, not CQ ones.
 - Added function key commands DISABLECW, ENABLECW and CWENABLETOGGLE.
   These change the value of CW ENABLE.
 - Type DEBUG in call window and hit RETURN to re-enable simulator debug.
   Eliminates having to stop the program and retyping TR DEBUG.
 - Increased Icom Command Pause default to 300ms and timeout to 500ms.
 - Fixed visible dupesheet random formating problem (column mode).
 - Fixed last entry of visible dupesheet possibly being out of order.
 - Added CONTROL ENTER to foot switch modes (advance QSO with no CW).
 - Fixed bug where ControlPageDown doesn't send radio to freq if in S&P
   mode and also made it do initial exchange when going into S&P mode.
 - Added BANDMAP.BIN - remembers band map when program stop/started.
 - Fixed WAIT FOR STRENTH (was still waiting for a keystroke).
 - Added START SENDING to FootSwitch Mode - starts sending callwindow.
 - Added LOG FREQUENCY ENABLE command (control-J, default = false).
   Replaces serial # with frequency (without leading megahertz).
   Frequency has a leading decimal point to make it clear it isn't a
   QSO number.
 - Disabled band map switching to a tuning radio if not two radio mode.
   Previously it would do this even if TWO RADIO MODE = FALSE.
 - Fixed bug with Radio Two not updating band output port if it was
   the inactive radio.
 - Added AUTO S&P ENABLE (control-J, default = false) to jump into
   search and pounce mode when tuning the radio more than 1 kHz.
 - Added FREQUENCY MEMORY command.  Used to tell the program the
   defaults to use when QSYing to a specific band/mode.  Simply
   tell it the frequency and the program will figure out the proper
   band/mode for it.  If you are trying to set a SSB frequency that
   is considered the CW band, put SSB before the frequency (i.e.
   FREQUENCY MEMORY = SSB 7050).  Note that these defaults will be
   over-written if you set FREQUENCY MEMORY ENABLE = TRUE.
 - Added defaults for 2 meter frequency memory.
 - Made FrequencyMemory get saved in RESTART.BIN file.
 - Added FREQUENCY MEMORY ENABLE command (control-J, default = TRUE).
   This turns on the memory for each band/mode.  When you come back
   to the band/mode (with either radio), it will go to the previous
   frequency.
 - Made frequency memory work - it was disabled.
 - Made CQ MENU come up if in SSB (not sure why I made it CW only).
 - Fixed frequency display sometimes not showing up when switching
   between radios.
 - Made zone check against CTY.DAT in POST L M an option.
 - Fixed bug where bandmap and visible dupesheet might be written twice.
 - Fixed band map opeartion on 10 meters (showed all the bands before).
 - Allowed three digits to be entered into call window for QSY.

Version 6.05 - Released 24 June 1997.

 - Added DISTANCE MODE (NONE, MILES, KM) - Control-J - none.
 - RFI counter measures for Control-J
 - Fixed Control-J bug near end of window.
 - Added BEEP ENABLE (default = TRUE) + ControlJ.
 - Added WAIT FOR STRENGTH (default = TRUE) + ControlJ.
 - Added NL section (VO1/VO2).
 - Fixed some problems with frequency memory for VHF.
 - Fixed inability to program FT920.
 - Made initial ex of grid show beam heading and also user info.
 - Added RANDOM CQ MODE.
 - RFI steps during auto CQ.
 - Fixed Pentium II divide by zero.

Version 6.04 - Released 13 June 1997.

 - Disabled beam headings for DXCC countries if ActiveDomesticMult = GridSq.
 - Fixed RT200 from own grid square.
 - Added FREQUENCY ADDER (Control-K) - Initial value = 0.
 - If no multipliers in contest, made summary sheet make total pts = score.

Version 6.03 - Released 8 June 1997

 - Added Split support and VFOB for Icom.
 - Added FT920 as radio type (I hope).
 - Added auto beam headings for grid squares.
 - Added NO POLL DURING PTT (control-J) - default = False.
 - Fixed missing CarriageReturn for packet spots from networked computer.
 - Fixed POST's R L report so it shows all mults.
 - Added DUPE CHECK SOUND (^J) = NONE, DUPE BEEP, MULT FANFARE.  (DUPE BEEP).
 - Fixed lower case true not working with LEADING ZEROS command in LOGCFG.
 - Added cut serial number support for RST QSO NUMBER exchange.
 - Eliminated possibility of long call window message overflowing variables.

Version 6.02 - Released 26 May 1997

 - Added TR FOOTSWITCHDEBUG for testing footswitch with paddle.
 - Added LASTCQFREQ CW MessageCommand and footswitch LAST CQ FREQ.
 - Made my own InitializeSerialPort procedure.
 - Fixed band map when calling station in S&P.
 - Fixed ARRL DX contest startup - my state bug.
 - Fixed search and pounce update problem.
 - Added RADIO ONE UPDATE SECONDS and RADIO TWO UPDATE SECONDS (Control-J).

Version 6.01 - Released 12 May 1997 (Dayton release)

 - Added INTERNET SIX contest.
 - Enhanced questions at start of program.
 - Added INTERCOM FILE ENABLE - interstation talk messages to INTERCOM.TXT.
 - Added IOPort startup (not in 6.01 manual) also PORT.
 - FOOT SWITCH MODE (DISABLED, NORMAL, QSO NORMAL, QSO QUICK, SWAP RADIOS)
 - Made COM1 and COM2 assume normal I/O addresses if zeros found in table.
 - POST Q C was rewritting NAMES.NEW each time instead of appending.  Fixed.
 - Fixed not calling portables by name in SayName (was only fixed in SayHello)
 - Added FOOT SWITCH PORT command - uses pin 15 - does carriage return.
 - Eliminated possible runtime error because I checked for sufficient memory
   for SCP file buffer after allocating memory.

 - Fixed not having acces to USER4 and USER5 for updates.
 - Created LogPack unit - cleaned up a lot of the packet stuff.

Version 6.00 - Released 14 April 1997

 - Added RST AND CONTINENT exchange.
 - Updated SOUTH AMERICAN WW contest.
 - Added TEXAS QSO PARTY - beware of MY STATE = TEX or MY STATE = TX.
 - Added DVP PATH logcfg command + control J.
 - Made AutoSendCharacterCount letter in call do SCP.
 - Fixed not saying hello to portable stations.
 - Moved all display cleanup stuff in S&P to just before LogContact.
 - When callign station with RETURN in S&P - add to band map.
 - When in S&P and updated call in exchange - update bandmap & station info.
 - Made ShowStationInformation do nothing if called with same callsign.
 - Fixed formating problem in Auto CQ if time >= 10 seconds.
 - Made CQ entries in band map show up if using DVK (was only DVP or CW).
 - Made Control-B command work during Auto-CQ (need to exit for next CQ).
 - Made BROADCAST ALL PACKET DATA not do anything if no network port.
 - Fixed SLASH MARK CHARACTER -> CHAR from Control-J

Version 5.99 + 5db - Released 14 March 1997

 - Reduced MultiDelayTime values to 0 for slow and 1 for fast.
 - Fixed bug with GetSlippedString and if character added during procedure.
 - No more beeps for first couple of multi message retries.
 - Allowed messages sent during control-B on network computer to go to TNC.
 - Allowed Control-B on network computer to review packet data.
 - Added BROADCAST ALL PACKET DATA (default = TRUE).
 - Added QSO NUMBER BY BAND (default = FALSE).
 - START SENDING NOW KEY fixed but START SENDING CALL KEY still works.
 - Fixed QUESTION MARK CHAR coming out of control-j.
 - Created FT900 setting.  Also fixed radio two for Yaesu.  Ignore - in name.
 - Improved instructions for generic TRMASTER data from file routine.
 - Added USER4 and USER5 to TRMASTER, INITIAL EXCHANGE and USER INFO.
 - Fixed BeSilent not working in SSB during S&P.
 - Disabled RIT during ControlJ.
 - Made beam heading show country ID instead of callsign.
 - Made DVK ON initial value.
 - Improved Alt-K message when in phone.
 - Improved DVK memroy initial values.
 - Added DVK write ability.
 - Made DVK/DVP Off/On window disappear if no DVK or DVP enabled.

Version 5.99 - Released 25 February 1997

 - Improved DVK messages (DVK ON/OFF from Alt-k).
 - Went to DVK1 to DVK4 to send DVK messages (defaults to CQF1 to F4).
 - Also made ESCAPE work for DVK message abort.
 - Made F10 abort DVK message if the same port isn't used for band output.
 - For Ten-Ten - eliminated Domestic and DX mults - switched to prefix only.
 - Made F1 in S&P flush CW buffer if not AllCwMessagesChainable.
 - Allowed Alt-L during Auto-CQs.
 - Added Control-[ @ to send a space and call window if a ? is in it.
 - Fixed showing domestic mult status in ARRL DX with non numeric initial ex.
 - Fixed Control-Enter if in S&P and call window to log QSO.
 - Added CQMODE and SAPMODE CW commands.
 - CW ENABLE command added (default = TRUE).
 - Fixed missing equal sign after ICOM COMMAND PAUSE command from Control-J.
 - Made F10 stop keyboard CW (just like ESCAPE).
 - Improved POST's dupesheet performance by alot.
 - Added SPACE BAR DUPE CHECK ENABLE command (default = TRUE).
 - Fixed INITIAL EXCHANGE = QTH coming up with VE initial exs all the time.
 - Made Alt-Z show station info.

Version 5.98 - Released on 1 February 1997

 - Updated OKOM.DOM file.
 - Added ControlLeftBracket CW commands: 2 = NOT FWE.  3-8 vary it.
 - Added FARNSWORTH SPEED parameter - initial value = 25.
 - Improved SendCW command - pause plus allow use of special CW characters.
 - Eliminated error message at end of UUENCODE procedure from end statement.
 - Made AutoCQ times work on half seconds.
 - Made sure ; character in contest name doesn't confuse POST.
 - Added INITIAL EXCHANGE CURSOR POS with AT START or AT END.
 - Fixed title for ARRL VHF SS contest.
 - Kept POST U E F N however.
 - Changed POST's U E F menu so one entry does all logs -> TRMASTER.
 - Added choice if calls not in database added when doing vanity update.
 - Fixed multiplier check off sheet (POST R M C).

Version 5.96 - Released on 12 January 1997

 - Added way to load in ARRL 10 meter log into TRMASTER.
 - Added BANDMAP and PACKET start up commands for testing/demo data.
 - Added all fields to USER INFO SHOWN:
         NAME      QTH       SECTION   OLD CALL  GRID      CHECK SECTION
         ZONE      USER 1    USER 2    USER 3    CUSTOM    FOC NUMBER

 - Added CUSTOM USER STRING command with the same values as CUSTOM INITIAL
   EXCHANGE STRING.

 - Fixed O command in U E E editor (Old Call) - it would do nothing before.
 - Added CUSTOM as an INITIAL EXCHANGE.

 - Added CUSTOM INITIAL EXCHANGE STRING to specify custom initial exchange.
   Legal entries are

         CQZONE    ITUZONE   NAME      QTH       SECTION   USER1     USER2
         USER3     GRID      FOC       CHECK     OLDCALL   TENTEN.

 - Eliminated bad crash if trying to use SCP with too little free memory.
 - Send corrected call before 73 message with QSLButDoNotLog & bad exchange.
 - Eliminated possible truncation of sprint simulator exchange.
 - Fixed nameless calls being fetched in SPRINT simulator.
 - Added QCWA GOLDEN contest (barely) + CheckAndChapterOrQTHExchange.
 - Made | character only send name and space if there is a name there.
 - Disabled shift keys during Alt-L.
 - Added POST R L - List multipliers worked.
 - Made GetBeamHeading not crash if target grid same grid as MY GRID.
 - Made entering DX QTH in Sprint unnecessary.

Version 5.95 released on 21 December 1996.

 - Fixed KC6 six letter call which matched 5 letter Palau call bring in V6.
 - Fixed KA2 six letter call which matched 5 letter JA call being in JA.
 - Made HOME GRID only put you in VGA mode if domestic mult = grid squares.
 - Added ARCI QSO PARTY
 - Made multiple entries during add file not generate dupes in TRMASTER.

Version 5.95 Beta to WB5VZL on 18 December.

 - Took MaximumCountries from 400 to 510.
 - Made sure random calls won't come up with FOC numbers.
 - Added FOC NUMBER to INITIAL EXCHANGE and USER INFO SHOWN.
 - Added way to move sections from ARRL 160 log to TRMASTER database.
 - Made all ARRL Section QSOs in ARRL 160 count two points.
 - Added DISTANCE start up for calculating distance between two grids.
 - Fixed multiplier check for ARRL 10 meter contest.
 - Added ARRL 10 QSOPointMethod.
 - Fixed RSTandQSONumber so it will work with TR READ and ARRL 10 dx stations.
 - Fixed some potential blowups with different grids in grid distance.
 - Fixed LPT1 problem with video and paddle port.
 - Made POST's QSL routines use NAMES.NEW for new names.
 - Made POST's QSL routines only use TRMASTER database.
 - Fixed POST save notes to file not closing output file.
 - Fixed byte count in DeBase64 + more improvements.
 - Fixed Section initial exchange so it doesn't give check and section.

Version 5.94 - Released on 7 December 1996.

 - Removed RIT commands for Yaesu.
 - Fixed FT1000 commands in LOGCFG.DAT file.
 - Added OldCall as a USER FIELD which can be displayed.
 - Added routine to load in Old call / New call update file.
 - Created OldCall field in TRMASTER database.
 - Fixed QSL label T command - was broken when went to CTY.DAT file.

Version 5.93 - Released on 4 December 1996

 - Made calls to GetCountry and GetZone with previous call happen real fast.
 - Made inactive radio's band output port follow radio band change.
 - Added Section initial exchange.
 - Added FARNSWORTH ENABLE (False + Control-J) < 25 WPM.
 - Made SCP work with characters added during AutoStartSend.
 - Made reminder posted messages stay up fixed number of seconds (60).
 - Made packet spot set ReminderPostedCount to 60 seconds.
 - Improved multi nework error messages (warning after 5 retries).
 - Made BeSilent (Control-Enter or Control-\) work in S&P mode.
 - Added IC781 as radio type and send narrow filter command with mode.

Version 5.92 - Released for W7RM multi-multi.

 - Added summary sheet for Stew Perry contest.
 - Made FT990 be its own rig.
 - Added Control-W to erase window contents without stopping CW.
 - Cleaned up help screens.

Version 5.91 - Released 19 November 1996

 - Made Control-\ or Control-Enter not disable function key CW.
 - Changed FT990 RIT increment from 10 to 20 hertz (same as 4.05).
 - Fixed bug with no CW space after / sign.

Version 5.90 - Released 18 November 1996

 - Made = key work again (QuickQSLMessage2).
 - Added split frequency set for 990.
 - Put RIT back in for 990.
 - Made FT990 and FT890 same thing.
 - Added MULTI RETRY COUNT (default 30 - minimum 3) + Control-J.
 - Added PACKET ADD LF command (Control-J).
 - Improved base 64 decode.
 - Fixed mode for JST radio.

Version 5.89 - Released 12 November 1996

 - Fixed network.
 - Added NETDEBUG startup command -> NET.DBG.
 - Add Control-X (decrease) and Control-Y (increase) dynamic weight commands.

Version 5.88 - Released on 8 November.

 - Added RST AND OR GRID exchange.
 - Added STEW PERRY QSO POINT METHOD.
 - Added STEW PERRY Contest.
 - Fixed POST bug: if overwrite true, old data would be lost if no new data.
 - Made CALLSIGN UPDATE ENABLE = TRUE as a default for the Sweepstakes.
 - Fixed calls with / in exchange being ignored if CALLSIGN UPDATE ENABLE.
 - Made Control-Enter work just like Control-\ (no CW).
 - Made MM/ be in no country.
 - Control-\ can be used at the start of CQ MODE QSO with no CW sent.
 - Added SL FIVE POINT qso point method = 5 points for SL prefix, else 1 pt.
 - Added CWSpeedFromDatabase flag (control-J).
 - Added speed field to TRMASTER.DTA.
 - Prevented station information from band map from over-writing CQ QSO info.
 - Prevented band map from over-writing dupe info window if call ready.
 - Created RELAY CONTROL PORT with 1 2 or 3.  Uses pin 14 to control relay.
 - Fixed garbage in call window if editing log during Alt-Q or Alt-C.
 - Made CQ band map entries no longer blink.
 - Fixed off/on time commands.
 - Changed Alt-N to -.
 - Added split command for FT1000.

Version 5.87 - 31 October 1996 - Used in the SS CW contest.

 - Went back to better memory setup and overlays.
 - Fixed VGADisplayEnable so it did something - default TRUE.

Version 5.86 - 28 October 1996

 - Moved from \ for split input to Alt-N.
 - Fixed AutoDisplayDbupeQSO and SCP incompatability.
 - Non exisitant country shown in beam headings.
 - Fixed divide by zero error for tan causing problems with beam headings.
 - Added beam heading and distance utility in POST.
 - Added name editor for TRMASTER.DAT.
 - Added SCP WINDOW DUPE COLOR and SCP WINDOW DUPE BACKGROUND commands.
 - Added USER INFO WINDOW and USER INFO BACKGROUND parameters.
 - Added USER INFO SHOWN with NONE, USER 1, USER 2 & USER 3. Control-J.
 - Added Initial exchanges User 1, User 2 and User 3.
 - Added User1, User2 and User3 fields to LOGSCP = 12 characters.
 - Fixed reinitialization of remaining dx list if change size with ^J.
 - Made auto start send arrow disappear if in SSB.
 - Made DVPon/CW Enable messages follow mode from radio.
 - Made bandmap ignore split freq if within 1 khz.
 - Made tuning radios in two radio mode grab band map.
 - Added Control-Y to update blinking band map entry.
 - Eliminated double display of band map in some cases and band map catatonia.
 - Eliminated 3V and CX from short country list so it all fits in window.
 - Big remaining country function was broken by CTY.DAT - now fixed.
 - Allowed just last three digits of QSX frequency.
 - Made \ open up a window so you can still be using the call window.
 - Added the new initial exchanges to the Control-J menu.
 - New delay prodedure.

Version 5.85 - 23 October 1996 - TR only.

 - Added BAND MAP GUARD BAND (control-J).
 - Eliminated backup file feature of database editor (no more win95 crash?).
 - Fixed where POST gets names for QSL labels.
 - Added way to pull data from another .DTA to TRMASTER.
 - Added more conversion routines for file to TRMASTER.
 - Added better defaults for VE call area for CQ Zones.

Version 5.84 - 13 October 1996

 - Fixed RT 201 error if illegal char found in .ASC file during build.
 - Fixed RT 204 when adding files to TRMASTER.
 - Fixed potential file read error when fetching random calls from TRMASTER.

Version 5.83-VZL post only to WB5VZL

 - Tried to eliminate dupes in .ASC file for WB5VZL - may have fixed WIN95.
 - Made more overlays for POST.
 - Removed K1EA convert file convert to LOG.DAT command.
 - QSO# sent with S&P F2 send #-1 w/blank windows + cursor in call window.
 - Added AUTO QSO NUMBER DECREMENT flag (control-J - default = FALSE).

Version 5.83-JST - Beta release to KD2NT to test JST radio fix.

 - Added H0 command to JST radio after setting band/mode.

Version 5.83 - Released 4 October 1996

 - Fixed bug in R M H rate sheet if done on the 9th of the month.
 - Fixed install program.
 - Put Kenwood radio in split if \ command.
 - Added MY GRID (for QSO points and beam) and GRID MAP CENTER.
 - Eliminated HOME GRID

Version 5.82 - Released 1 October 1996

 - Removed degtag and miniprop conversion from POST.
 - Blew away BEAM HEADING FILE NAME.
 - Fixed bug with LEAVE CURSOR IN CALL WINDOW and not multiple bands.
 - Implemented Country9 into POST.
 - Added beam headings if home grid setup.
 - Added GRID startup command.
 - Added JST245 radio interface + JST RESPONSE TIME parameter (^J).
 - Added B64DECODE and UUDECODE startup commands.
 - Made Sprint exchange hacker very much smarter - even uses name database.
 - Made CD.GetEntries for the last call use memory.

Version 5.81 - Released 17 September 1996.

 - Fixed crash if computing distance to your own grid (made it 0 KM).
 - Added YAESU RESPONSE TIMEOUT (100) and KENWOOD RESPONSE TIMOUT (25) w/ ^J.
 - Added ICOM COMMAND PAUSE (200), ICOM RESPONSE TIMEOUT (100) with ControlJ.
 - Made my own Wait routine.
 - Added / as a way to set B vfo (Kenwoods at least).
 - Fixed bug with show search and pounce.
 - Allowed 6 digit grid squares when starting up program for home grid.

Version 5.80 - Released 13 September 1996.

 - Made get check and section also setup states.
 - Fixed IC-761!
 - Added European HFC and KVP contests.
 - If no mults, made SCORE show total QSO points.
 - Fixed EU VHF so no mults - and allow exchange to work without them.
 - If cell found without any calls - fixed crash of possible call list.
 - Made up some random calls if no .DTA file around.

Version 5.78 - Release 7 September 1996.

 - Many fixes and enhancements to .DTA stuff.
 - Fixed crash if using EUROPEAN VHF TEST and no home grid set.

Version 5.77 - Release 3 September 1996.

 - Added E command to POST's utility menu to edit .DTA file.
 - Added NAME QTH, CHECK SECTION, QTH and GRID initial exchange types.
 - Installed new TRMASTER.DTA file.
 - No more Alt-N command.
 - Separate arrow and page-up/down keys.
 - Added search to hexdump.
 - Fixed bug with POSTMULT rate sheet at end of file.
 - Added CHECK LOG FILE SIZE (default = false) with Control-J.
 - Fixed floppy save when error - no crash.  Added nice messages and beeps.
 - Updated visible dupesheet after log edit.

Version 5.76 - Released 22 August 1996

 - Fixed GridFields - in TR and POST mult check.
 - Made it so any control character terminates a callsign in the SCP file.
 - Changed overlay buffer from 90K to 70K.
 - Eliminated range checking in LogDupe (saves 5K).
 - Eliminated hang if not enough room for name database at startup.
 - Created NewReadKey and NewKeyPressed to allow F11 and F12!

 - Fixed bug with SAC prefixes not being processed correctly if in editable
   window and SAC prefix different than natural prefix.

Version 5.75 - Released 13 August 1996

 - Made SingleRadioMode over-ride two radio functions.
 - Made illegal StartDVK entries not clobber the I/O ports.
 - Added LEADING ZERO CHARACTER with ^J toggle T, O and 0.
 - Made LEADING ZEROS an integer - but still allowed Y to set it to 3 (^J).
 - Improved SAC mults for calls like 8S3BG.
 - Fixed MULT REPORT MINIMUM BANDS in LOGCFG file (was COUNTRIES).
 - Made SIMULATOR ENABLE = FALSE over ride a previous = TRUE entry.
 - Added Multiplier Reports - CTY/ZONE first/total by band.
 - When loading in LOG.DAT file - no crash if illegal mode with okay band.
 - Made FindDirectory look in more places for files.
 - Fixed ITU Zone 30 to be in Asia, not Europe.
 - Made AUTO DUPE ENABLE CQ and AUTO DUPE ENABLE S AND P (both ^j).
 - Made install program no long CD into target directory.
 - Added QSO POINTS DOMESTIC/DX CW and PHONE commands - no ^J.
 - Added bigger remaining country list - BIG REMAINING LIST = TRUE (^J).

Version 5.74 - Released at WRTC on 11 July 1996

 - Fixed when using StartSendingNowKey delete key causes garbage.
 - Added AUTO DISPLAY DUPE QSO with default = FALSE.

Version 5.73 - Released on 4 July 1996

 - Added PgUp/Dn to auto CQ message.
 - Added VGA DISPLAY ENABLE.
 - Improved the list of domestic countries for FIELD DAY.
 - Put checks of packet and multi port in Alt-C loop.
 - Improved POST (L)og Mer(g)e routine.  Checks for matched entries with
   a 29 QSO buffer.

Version 5.72 - Release on 17 June 1996

 - Added PTT TURN ON DELAY parameter - default = 15 * 1.7 ms.
 - Fixed bands above 2304 not showing up in summary sheet.
 - Fixed secondse -> seconds in auto cq.
 - First attempt of Grid Map.

Version 5.71 - Release on 14 June 1996 - to W7YAQ mostly.

 - Added FIELD DAY to simulator.
 - Made AUTO DUPE ENABLE = FALSE for VHF contests.
 - Fixed non /R calls in VHF not being flagged as dupes.
 - If no I/O COM3 or COM4 address in ROM table, use 3E8 & 2E8 (a la COM34).

Version 5.70 - Release on 7 June 1996.

 - Made non OK/OM station in OK DX test have mult by mode and band.

Version 5.69 - Released around 5 PM on 6 June 1996.

 - Made sure this version didn't have debug stuff on.
 - Made /R stations initial exchange not have space and allow overwrite.
 - Made grid map display go away when doing lots of Escapes.
 - Fixed alphabetical order of grid map display.

Version 5.68 - Released around 2 PM on 6 June 1996.

 - Made ARRL VHF contests have QSO BY MODE = FALSE.

Version 5.67 - Released to WB5VZL on 6 June 1996.

 - Many rover fixes (it was using root calls which came out /R!!).
 - Made QSO needs have better format + reverse video for other mode.
 - Made QSO BY MODE = TRUE for VHF contests.
 - Added FM - but it is really SSB in disguise.

Version 5.66 - Released to WB5VZL on 6 June 1996.

 - Only allow dupes and show grid status if /R shows up in VHF call.
 - Made VHF contests have auto-dupe enable on.
 - Removed WRTC halt and band limitations.

Version 5.65 - WRTC - Released on 6 June 1996.

 - Allowed 3 different mults to be active.
 - Made program only cover 40, 20, 15 and 10.
 - If not WRTC then halt program.

Version 5.65 - Released on 3 June 1996

 - Added WRTC contest.
 - Region one FD, if in DL, work non portable outside Eu, was 4 pts, now 3.
 - Added HF BAND ENABLE (false unless VHF contest).
 - Removed 47G band.
 - Better formatting of domestic remaining mults.
 - Made remaining mults work for VHF/UHF bands.

Version 5.64 - 8088  version sent to WB5VZL on 24 May 1996.

Version 5.63+ - Sent to WB5VZL on 20 May 1996.

 - Made QSO points work right when working dupe in new grid square.
 - Showed grids when working station if domestic mult = grid squares.
 - Added PACKET RETURN PER MINUTE count (0-10 minutes).
 - Made AutoDupeEnable = FALSE for VHF conetsts.
 - Added display of grid squares worked for each station.
 - Added 3GH, 5GH, 10G, 24G, 47G and LGT bands.
 - Made PacketBeep do something.
 - Fixed missing AddDomesticCountry commands for the internet sprint.
 - Improved GoodCallSyntax for 3 letter calls w/o a number (for CALLTEST).

Version 5.63 - Released on 13 April 1996

 - Made Control-L behave itself if lots of memory (maximum buffers used).
 - TRP no longer distributed with program.  Available by special request.
 - Allow negative input for HOUR OFFSET in Control-J menu.
 - Fixed possible garbage in call window after Control-B CW message & S&P.
 - Fixed problem with zone over-ride not always working for .CTY files.
 - Made the ninth call district in VE count as zone 5/9 instead of zero.
 - Made Helvetia contest count HB9 as a DX mult if you are in HB9.
 - Updated OK/OM contest to new rules (1994 rules).
 - Made POST always ask if you want to renumber your log in L C.

Version 5.62 - Released to the world on 8 March 1996

 - Fixed illegal memory operation when working QSOs that are on spot list.

Version 5.62 Beta - Released to WB5VZL to play with.

 - Fixed numerous illegal pointer operations.
 - Fixed bug in summary sheet if number of total domestic mults > 255.
 - Went to protected mode!!
 - Fixed bug with incorrect mult totlas if reloading in ARRL DX log.

Version 5.61 on 31 January 1996.

 - Fixed DX stations in Sprint coming up with a domestic mult also.
 - Made spaces around equal sign not necessary in beam heading file.
 - Added showing domestic multiplier status if only thing in exchange window.
 - Added QCWA contest (sort of...  didn't do multipliers).
 - Added MULT REPORT MINIMUM COUNTRIES paramater (^J) for ^O display Def = 4.
 - Added .DOM file for NRAU.
 - Made DXMultArray size go to 500 (was 350) for FOC guys.
 - Don't ask if contest is over if editable log is empty.
 - Changed title for domestic mults in totals.
 - Made Remaining Mults and band map update with Alt-R or rig band change.
 - Added version display in Control-J
 - Added Branch Zones to legal zone multipliers.
 - Some minor cleanup in conjunction with manual cleanup.

Version 5.60 on 16 January 1996.

 - Added BAND MAP CALL WINDOW ENABLE (in Control-J, default = TRUE).
 - Added Alt-G to Control-J.
 - Fixed bug with some GetDXQTH (NAQP non NA countries counting as mults).
 - Improved CW Memory defaults for NAQP.
 - Fixed PTT Delay.
 - Added LOG PULL to POST.
 - Added COMPUTER ID command.

Version 5.59 on 4 January 1996

 - Added FT1000MP CW REVERSE command.
 - Fixed reading mode from FT1000MP (should be the last bug).
 - Made GetRadioInformation do something with FT1000MP.
 - Fixed spelling of ALL CW MESSAGES CHAINABLE when ControlJ adds to LOGCFG.
 - Added VHF enable for RAC contest.

Version 5.58 on 1 January 1996

 - Added ALL CW MESSAGES CHAINABLE command.
 - Made SPACE BAR while AutoCQ and TwoRadioMode QSO QRV instant.
 - Fixed possible garbage in call window when doing commands with AutoCQ.
 - AutoCQ now times silence gap, not start times (for CW at least).
 - Added CountsSinceLastCW in LOGK1EA.
 - Changed GridLoc to allow QSOs on different modes as well as bands.
 - Added prompt and warning message if program is waiting for RST input.
 - Made sure monitor tones get turned off if done in Control-J.
 - Numerous spelling cleaups (mostly in help).
 - Made SCP automatic if SCP MINIMUM CHARACTERS > 0.
 - Added SCP MINIMUM CHARACTERS.
 - Made Control-D in function key CW message make the message chainable.
 - Fixed PageDown key operation in Control-J menu.
 - Made entry with a / but no numbers not get counted as a valid callsign.
 - Made QSLAndLog compatible with CallsignUpdateEnable.
 - Changed Compatable to Compatible in POSTLOG.
 - Added AskIfContestOver.
 - Fixed garbage OKOM.DOM file.
 - Made Alt-A with empty call window and SCP display clear the display.
 - Fixed AddDomesticCountry ('W') -> ('K') for defaults in FContest.
 - Made MultByMode for RAC contest.

Version 5.57 on 19 Dec 1995

 - Added summary sheet for TenTen contest.
 - Added RAC contest (including summary sheets).
 - Added NZ FIELD DAY contest (including summary sheets).
 - Added NZFieldDayQSOPointMethod.
 - Added NZFieldDayExchange.
 - Made ARRL output strip off $.
 - Reworked FT1000MP GetFreq stuff.
 - Fixed GetZone for USA stations (broke it with a POST zone over ride fix).

Version 5.56+ - Beta test for KE9I and WB5VZL - 11 Dec 95

 - Added 3 ms delay to DVP command too (not just init).
 - Made FT1000MP use normal CW instead of CW-R (old FT1000 500Hz mode).
 - Made SCP support ?.
 - Made LOGDVP not have Halt in it.  Makes CW error instead and disables DVP.
 - Fixed zone override in CQWW.CTY and IARU.CTY not working for some ctys.

Version 5.56 - 9 December 1995

 - Fixed showing domestic multiplier status for 10 meter contest.
 - Fixed bug preventing FT890 from working as Rig 2 interfaced radio.
 - Fixed bug preventing 1200 baud Icom radio from working as Rig 2
   interfaced radio.

 - Added support for FT1000MP.

Version 5.55 - 30 November 1995

 - Added SINGLE RADIO MODE to disable Alt-R command.

 - Added command support for function keys: BANDUP, BANDDOWN, DUPECHECK,
   TOGGLECW, TOGGLEMODES, SWAPRADIOS.  Use Control-C and Control-D.

 - Made band map update instantly to band changes from Alt-B/V/M.
 - Slowed down 1000 count DVP to 3 milliseconds per second.
 - Fixed error messages if country files not found.
 - Added ADD DOMESTIC COUNTRY command (with CLEAR).
 - Added ASK FOR FREQUENCIES.
 - Added NO LOG.
 - Added DOMESTIC FILENAME command for LOGCFG.
 - Fixed POST's multiplier check (whew!).
 - Fixed ARRL 160 DomesticFileName (was none).
 - Fixed program crash when using ` key to send packet spots.

Version 5.54 - 19 November 1995 (not distributed on the internet).

 - Fixed bug with ControlF message program toggling.
 - Put SHIFT KEY ENABLE back in.  It didn't make it from Louisana.

Version 5.53 - 16 November 1995

 - Fixed error writing summary sheet info to \log\name if none found.
 - Fixed created directory problem with install.
 - Improved message that comes up to tell you about no names database.

Version 5.52+++ - Beta test.

 - Made AB all alone as a section work.

Version 5.52++ - Beta test version.

 - KEYPAD CW MEMORIES tied to CQ ControlF1 to F10.
 - Even better SS exchange hacker.
 - Fixed minor format error possibility with remaining display.
 - Made default remaining country list if none found in .CTY file.
 - Added TEN TEN contest.
 - Added "TEN TEN"  QSO POINT METHOD.
 - Made callsign update from exchange use last call, not first.
 - Enhanced SS exchange cruncher.  34A67 EPA now works.
 - If packet spot is NoBand or NoMode, ignore it so no more runtime 201 error.

Version 5.52 on 11 November 1995

 - Added Three Phone Five CW QSO Point Method.
 - Made two radio stuff not send any CW if active mode <> CW.
 - Changed Multi baud rate to 2400 baud (was 4800).
 - Made dupes get sent to the other computers.
 - Fixed ugly multi bug.  Saving other peoples QSOs to my message list.
 - Allowed band changes to be made during 2 radio QSOs.
 - Made Multi port get checked if waiting for CW to end in DEBUG mode.
 - Added HEXDUMP command (new and improved with END, PageUp and PageDown).

Non version used at W5WMU in SS CW.

 - Allow exchange being sent in two radio mode to be aborted with escape.
 - Allow speed changes during second radio QSOs.
 - Made Alt-D with no call input update BandMapBlinkingCall if there is one.
 - Added HEXDUMP feature.
 - Added switch to disable/enable RIT shift key function (SHIFT KEY ENABLE).
 - Made the CQ sent with two radio QSO in progress come from Alt-F3.
 - Made the CQ sent on inactive radio during dualing CQ QSO come from Alt-F2.
 - Fixed problem with random domestic QTHs for ARRL sections (garbage).
 - Fixed double spacking in packet window.

Version 5.51 on 30 October 1995

 - Added support for SS in simulator.
 - Made install 2.1 ask about overwriting .CTY files.
 - Made \log\name be the choice if two names files.
 - Make remaining mults use index bytes instead of strings.
 - Added SCP FILENAME command.
 - Fixed HiLight to HILIGHT in LOGCFG.DAT.
 - Fixed Alt-G not needed at start of CQ WW.
 - Change REMAINING DOM MULT DISPLAY to REMAINING MULT DISPLAY MODE.

Version 5.50 on 21 October 1995 (beta test)

 - Added LITERAL DOMESTIC QTH flag.
 - Removed DOMESTIC STATES and DOMESTIC PROVINCES commands.
 - Made REMAINING DOM MULT DISPLAY command + ControlJ = NONE, ERASE, HILIGHT
 - Eliminated DC Enable flag.

 - Change dom mults to NONE, DOMESTIC FILE, GRID SQUARES, GRID FIELDS and
   WYSIWYG.

 - Made GridSquares, GridFields, WYSIWYG Domestic and NoDomesticMults work.
 - Fixed club calls for S5.
 - Added SHOW SEARCH AND POUNCE.
 - Added DOMESTIC QTH DATA FILENAME command.
 - Made remaining domestic mults get generated from LogDom.
 - Created LogDom unit with DomQTHTable object.
 - Eliminated any reference to \log\dvp.
 - If not DVPPATH then use current directory for DVP files.
 - Made .CTY files get loaded from PATH directories (or \log\name if none).
 - Fixed garbage in rem mults display if name memory disable (K8JLF bug).
 - Made remaining mult display come up in a better place.
 - Made name database file look through path statement (and it works!).
 - Changed INSTALL program to just dump all files into specificed directory.
 - Fixed UA9 being in Europe for IARU.

 !!! - Many runtime 201 errors with packet...  maybe getting unknown
       country from packet spots?

       + Added length check to PacketMessage before adding chars in LogK1EA.
       + Moved update of PacketBufferLine to CheckPacketPort procedure.

 - Fixed PTT Enable so that PTT is off during CW when FALSE (K2MM sat there).

Version 5.26 on 31 August 1995.

 - Better fix for QSO/MULT needs for VHF bands.
 - Better fix of ARRL format for 6 and 2 meters.

Version 5.25 on 31 August 1995.

 - Fixed ARRL format for 222 and up bands.
 - Made QSO needs work for VHF contests.
 - Added name database signature feature to Q S in post.
 - Added PADDLE BUG.
 - Improved defaults for Internet Sprint.

Version 5.24 (released to the world on 20 July 1995).

 - Fixed DEBUG.
 - Added selectable SPACES to QSL labels.

Version 5.24 on 16 July 1995 (sent to Italy)

 - Fixed write file not getting closed when finishing POST's Q S command.

Version 5.23 on 04 July 1995 - First Z80Op release!!

 - Added CQ VHF contest.
 - Fixed QSL BUT DO NT LOG.
 - Made CQExchangeNameKnown get ignored if SayHiEnable = False.
 - Changed overlay buffer size to 70K.
 - Made Zonecont, LogEdit, LogName, LogDVP and LogCW to overlays.

 - Move several routines into LOGHELP: SetAlarm, LogView, EditLog,
   AddReminder, CheckForName, LoopBack, SultanScoreFile.

 - Switched to pre assigned arrays for BandString and ModeString.

Version 5.22 on 27 May 1995

 - Made POSSIBLE CALLS that are dupes show up in red.
 - Updated MAINTAIN to work with 950 cals.
 - Fixed last entry of 0 call area on visible dupesheet being screwy.
 - Added PACKET BAUD RATE command.  Legal values are 1200, 2400 and 4800 baud.

Version 5.21 on 14 April 1995

 - Fixed QSL MODE command so STANDARD doesn't count as QSL AND LOG.
 - Minor + major Sultan improvements: fixed score list & added list output.
 - Checked for Mode <> NoMode when accessing DupeList.QSOTotals (caused 201).

Version 5.20 on 19 March 1995

 - Added W3YY .DBF convert routine.
 - Some enhancements to Sultan.
 - Created BeepEvery10QSOs.
 - Made initial exchange file get ignored if using RESTART.BIN.
 - Made domestic QTHs get logged as entered, except when shown as mult.
 - Crated TOEC contest.
 - Replaced "escapes" with "shift-tab" in S&P information display.

Version 5.19 on 5 March 1995

 - Added GRID LOC contest.

Version 5.18 on 4 March 1995

 - Changed from Delete to Move when deleting packet character sent.
 - Fixed crash if you enter no call in sultan mode.

Version 5.17 on 27 February 1995

 - Made Sheet.Dispose&Initialize clear/dispose of the remaining mult arrays.
 - Added SULTAN option.
 - Made it so INITIAL.EX files with lower case don't generate duplicate calls.
 - Fixed wrong .CTY files getting copied during INSTALL.
 - Updated default messages for sprint.
 - Fixed 201 error in GetPartialCall when Address >= NumberPartialCalls.
 - Fixed bracketed string if start string found at start of long string.
 - Made trailing space on contest name not screw up summary sheets.
 - Made POST's multiplier report (R M) procedure ignore *DUPE* as a mult.
 - Made FT890 choice work for RADIO TWO as well as RADIO ONE.

Version 5.16 on 30 January 1995

 - Prevented station information being displayed for CQ band map entries.
 - Added FT-890 choice for rig type.
 - Put InitializeKeyer after all possible halts during setup.
 - Added time updates to multi network (use Alt-T command).
 - Added band map support to multi network.
 - Changed some multi stuff so each command has unique ID number.
 - Made SEND QSO IMMEDIATELY = TRUE the default.

Version 5.15 on 22 January 1995

 - Made multi QSO information go into partial call and initial exchange mem.
 - Added SEND QSO IMMEDIATELY flag (for multi-multi).
 - Added RADIO ONE RECEIVER ADDRESS and RADIO TWO RECEIVER ADDRESS.
 - Made frequency display for radio truncate below 100 hz.
 - Slowed down commands going to IC735 - looks solid. Added mode commands.
 - Made JD1 count for JA INTERNATIONAL DX test.
 - Made GetPartialCall exit if # PartialCalls = 0.  Might fix some 201 errors!?

Version 5.14 on 8 January 1995

 - Added \OFF and \ON commands for breaks.
 - Added QSO POINT METHOD ALWAYS ONE POINT PER QSO for Internet Sprint.
 - Made multiplier information on keystokes go to sleep when exchange window.
 - Put warning message about unknown countries back in LOGDUPE.
 - Made DUPES or ZEROS not disappear when doing mult check.

Version 5.13 on 26 December 1994

 - Change I1 and IS0 to I and IS when checking for DX Multiplier type.
 - Made POST's dupe routine clear *DUPE* if the QSO isn't a dupe.
 - Removed warning message about unknown domestic multiplier in LOGDUPE.
 - Implemented new POST L M mult check.  Removed Zone and DX mult checks.
 - Fixed dupesheets if they have an unknown (-1) country.

Version 5.12 on 11 December 1994

 - Moved random characters to after QSO number on log sheet.
 - Created separate overlay unit for FoundContest routine saving 9K of RAM.
 - Made POST L C create band logs up to 2304 MHz.
 - Created PARTIAL CALL MULT INFO ENABLE flag (in control-J).
 - Made callsign starting with Q not hang up GetCountry/Zone/Continent.
 - Made ARRL output for post squelch comments and name asterisks.
 - Made partial call mult check work for DX, Prefix and Zone mults.
 - Made partial call mult check work even if not mult by band or mode.
 - Added MOUSE ENABLE command.
 - Made POSTs dupesheet (R D) routine ask for which country list to use.
 - Made band map initial exchanges show up in call window.
 - Made band map initial exchanges show up again in dupe info window.
 - Made GetCountry return -1 if the call is a /MM or MM/.
 - Fixed initialization of country table happen after selecting CQ WW.

Version 5.11 on 29 November 1994

 - Made Control-U show up when using multi.
 - Fixed packet spots not showing up when using multi network.
 - Made AltB/V and AltM work if illegal mode.
 - Made it impossible for RESTART.BIN to specify illegal band/mode.
 - Fixed POST L G Merge command so won't hang if long file second.
 - Eliminated CW being sent when using two radio mode on phone.
 - Made call window get updated with corrected call from exchange.
 - Added LOOPBACK option.
 - New multi protocol.
 - Fixed install program to actually overwrite cqww and iaru.cty files.
 - Fixed callsign not getting sent if 2nd radio QSO aborted after CQ done.

Version 5.10 on 23 November 1944 before new multi stuff.

 - If tail ending and then no call entered, used to loose previous QSO.
 - Found I was doing 2 keyer inits if MultiPort used and 3 diff serial inits.
 - Went to timer interrupt supported beeps.

Version 5.09 on 12 November 1994

 - Removed all Beeps!!
 - Compiled with stack checking on.
 - Added PADDLE MONITOR TONE.
 - Fixed do you want to stop now message in post contest log processor.
 - Made QSO and multiplier status windows show up if using super dupesheet.
 - Fixed POST counting *DUPE* and *ZERO* as multipliers during L C command.
 - Created CALLSIGN UPDATE ENABLE to control exchange window call sniffer.
 - Fixed DupeSheet bug in Entry Exists (number calls in block computed wrong).
 - Got rid of Control-R in edit window (whatever it was for).
 - Made check part of initial exchange stuff.
 - Made Ten-Tec work (first pass).
 - Fixed non active radio from clobering freq display if not valid freq.
 - Fixed vE2 entered as section coming up as Ps instead of Pq.

Version 5.08 on 30 October 1994

 - Fixed KC6xx and KG4xx being in USA.
 - Fixed missing ex window if previous QSO was dupe w/dupe check after edit.
 - Fixed AutoCQ hanging if ESCAPE pressed.
 - Made packet information stay up longer.
 - Made dupes not show up in Control-U packet window.
 - Put ActiveBand and ActiveMode into RESTART.BIN.
 - Really tried to make sure BeSilent gets cleared, even if no QSO logged.
 - Fixed bug where U C command in post would crash after several uses.
 - Made separate serial inverts for each rig.

Version 5.07 on 25 October 1994

 - Made many functions active when packet window is up.
 - Made shift key RIT go to sleep when packet window is up.
 - Made time and rate displays (and radio freq) get updated during controlB.
 - Made multi QSOs get logged if packet window up (no updates on screen).
 - Improved packet buffer so we don't miss characters.
 - Maybe fixed runtime some 201 errors (when getting packet spots).
 - Some packet clean up.
 - Added PACKET BEEP command.
 - Added PACKET BAND SPOTS command (defualt = FALSE unless doing multi).
 - Added packet support for multi network.

Version 5.06 on 6 October 1994

 - If first byte of Yaesu string = 0 or FF, it gets ignored.
 - Fixed visible dupesheet coming up when not enabled after Alt-Y.
 - Made band map work for whole band regardless of mode.
 - If in S&P and F2, then put call in call window and RET then RET, send ex.
 - BandMap improvement: two radio & after S&P QSO - no info in ex/cl windows.
 - Improved GetRadioString routine to try to eliminate extra characters.

Version 5.05 on 27 September 1994

 - Made a different exchange over-ride initial.ex entry for next time.
 - Added RadioDebug mode (TR RADIODEBUG) creates RADIO.DBG file.
 - Made calls that start with the number zero not crash the country table.
 - Added ..._. feature if using auto start send key and edit call before CR.
 - Made long calls use root call in visible dupesheet when in editable wind.
 - Made initial exchange work for some of the portable calls with initial.ex.
 - Improved format of output when using LOG DUPE procedure.
 - Fixed bug in LOG DUPE routine caused in 5.02 by going to one dupesheet.

Version 5.04 on 15 September 1994

 - Made Column Dupesheet only work with super dupesheet.
 - Fixed bug with first long call entry in dupesheet being screwed.

Version 5.03 on 11 September 1994

 - New SPRINT EXCHANGE Hacker.
 - Made up arrow with empty call window put you in Alt-E editor.
 - Added CONFIRM EDIT CHANGES command (for Alt-E exit).
 - Some POST clean up for VHF/UHF bands.
 - Made QSO count on summary sheet match log (was one lower).
 - At end of Control-B CW on inactive rig, switch paddle back to active rig.
 - Made EscapeKey get you out of entering time with Alt-T.
 - Fixed up DualingCQs so they work better.
 - Made two radio CQ get interrupted when a station calls.
 - Baby mouse steps (pretty crude) allows band map entry selection w/mouse.
 - If in S&P and press F1, went from length call > 3 to good call syntax
   to decide to go into exchange window.

Version 5.02 VHF Beta test

 - Improved format of contests at start of program w/o LOGCFG.
 - Made BAND statement accept more bands.
 - Made DUPE and ZERO goto *DUPE* and *ZERO*.  We should document this better.
 - Made dupe check in POST a two pass operation to save memory.
 - Fixed nasty bug in name database if delete function used.
 - Added ARRL VHF QSO and ARRL VHF SWEEPSTAKES contest and VHF/UHF bands.
 - Made remaining mults show up if using SuperDupeSheet.
 - Made the word DUPE not get moved into exchange windows when doing TR READ.
 - Made column dupesheet enable.
 - Improved default CW messages for SPRINT, using MyName and MyQTH.

Version 5.01 on 2-September-1994 (BBS, disks and uuencode release).

 - Fixed message relating to type LOG to start program again.
 - Fixed CBREAK so it really gets set back to original state when exit.
 - Made PacketCharReady and PacketSendChar and added DRSI as port type.

Version 5.00 on 29-August-1994 (BBS distribution only)

 - Made band map cursor stay in bounds.
 - Added ` character anto spot call window contents to packet.
 - Fixed CQ-# spot in band map when hitting space bar with empty call in SSB.
 - Made QuickEditResponse function not force upper case.
 - Put date and time stamp on notes added using ControlN.
 - Fixed purple exchange window coming up when tuning off band map.
 - Made packet message that includes MY CALL show up on screen with a beep.
 - Improved packet spot interaction with band map.
 - Eliminated bug where packet window would blank out.
 - If not TWO RADIO MODE, allowed band map to show up on band/mode change.
 - Fixed SH/DX not getting into band map.
 - Rewrote whole VisibleDupeSheet to make it FAST instead of SLOW!
 - If EGA/VGA and no BandMap, then visible dupesheet uses lower window.
 - Fixed LOG.DAT always being the file when doing calculate totals in POST.
 - Put in warning message when QTC buffer almost full (PLEASE SEND QTCS!!).
 - Improved memory conservation for WAE in Europe (and a little outside too).
 - Fixed KG4 always being in Guat. Bay.
 - Fixed KC6X being in Belau.
 - Added user name security check.
 - Made LOG READ command process keystrokes as if entered manually.
 - Fixed band map entries that get deleted so they free up dynamic memory.
 - Added Province12 for NAQP (vo1, vo2, ve1-ve8, vy1, vy2).
 - Fixed VisibleDupesheet getting overwritten in upper right by update totals.
 - Did summary sheets!!!
 - Improved checking for enough memory before allocation and fewer halts.
 - Made tuned in station on band map show up in call window/exchange window.
 - Added Control-Function key instant DVP progrm with it on the air (no PTT).
 - Move some syntax checking for Kenwood into GetRadioInfoString.
 - Made frequency display use up to 4 second old reading if nothing from rig.
 - Made Control-R memories separate for call window and exchange window.
 - Turned off ControlBreak in DOS while program running.  Remembers state.
 - Added CQs to band map.
 - ControlPageDown can now select a spot. radio QSYs + call window updated.
 - Made new band map entries on same freq as old ones, delete the old ones.
 - Made dupes in band map have * and do same colors as non dupes.
 - Fixed bogus frequency display when hitting keys.
 - Fixed LOG.OVR -> TR.OVR in LogOInit.

 Version 4.29 on 17 August 1994

 - Improved operation of END and PageDown when doing ControlJ.
 - Fixed 12 A 67 ST 42 so that it comes out 12 A 42 ST, not 67 A 42 ST.
 - Made 1A 67LAX 332 not blow up precedence in Sweepstakes.
 - Made program not ask for save names if Control-Y has been executed.
 - REMAINING MULTS WINDOW SUBDUE -> REMAINING MULTS WINDOW SUBDUE COLOR.
 - Made LOGCFG.DAT allow tab white spaces.
 - Made GetRidOfSpaces routines do the same for tabs.
 - Allowed YES response to work for Control-Y (was set for single character).
 - Made Control-P work even if you are in the Exchange Window.
 - Fixed ExchangeWindowCursorPosition if initial exchange done with F1 in S&P.
 - Made Control-Dash alternating CQs have higher duty cycle + auto continue.
 - Made frequency display clear if active radio not interfaced.
 - Fixed initial exchange memory for six character portable calls (AC1O/4).
 - Made AutoStartSend arrow come up if new value programmed with Control-J.
 - Made highlighted band indicator follow radio initiaited band change.
 - Made BandMap entry turn red if it was already mapped and S&P QSO occurs.
 - Made wait for return string from radio abort if a key is pressed.
 - Changed IOTA QSO points for own country from 0 to 2 points.
 - Once again fixed Control-J CWTone changes to affect paddle immediately.

 Version 4.28 on 3 August 1994

 - Made SetUpToSendOnActiveRadio only FlushCWBuffer if changing keyer port.
 - Fixed proper salutation if using ARRL Country file.

 Version 4.27 on 1 August 1994

 - Made the escape key know what the DVP was up to.
 - Fixed proper salutation if using IARU zones.
 - Added EXCHANGE WINDOW S&P BACKGROUND command.
 - Made exchange window come up in green if in S&P.
 - Improved QTC function while in Europe receiving QTCs.
 - Added AUTO QSL INTERVAL if 2, then alternates between QUICK and QSL.
 - Made it so an entry to exchange window that is all numbers doesn't QSY.
 - Added RADIO ONE BAND OUTPUT PORT and RADIO TWO BAND OUTPUT PORT commands.
 - Got rid of F10 clear for DVK interface (because of Top Ten Devices).
 - Made AutoDupeCheck with Carriage return in S&P follow AutoDupeEnable.
 - Added IC735 support with RECEIVER ADDRESS parameter (in decimal please).
 - Added InternetSprint contest.

 - Major rework of inactive rig CW sending.  Fixed bug where band on Drake
   kept changing to TS-850S band when doing ControlA/B.

 - Fixed Shift-Tab so it really gets you out of search and pounce.
 - Eliminated F3 getting programmed with S&P exchange.
 - Made frequency get remembered from last displayed value.
 - Added frequency display.
 - Allowed frequency input when in S&P

Version 4.26 on 22 July 1994

 - Made LOG READ not send CW if CW TONE = 0. Also set AUTO DUPE ENABLE = FALSE.
 - Made LOG READ funtion use log's date and time.
 - Fixed mult by mode in IARU.
 - Fixed proper zone file not being loaded for Zone report feature.

Version 4.25 on 20 July 1994

 - Added Control Page-Down to delete a call in band map.
 - Made Control-W send previous-previous SprINT name.
 - Made packet spots for dupes get ignored, also added them to band map.
 - Added BAND MAP ENABLE command.

Version 4.24 on 14-July-1994

 - Tried new Yaesu RIT command.
 - Added Control-J command: PTT ENABLE to allow PTT signal to be off for QSK.
 - Disabled radio update to active band when two radio things happening.
 - Went to new Control-J format to allow easier additions in the future.

 - Created QSL MODE command for LOGCFG.DAT.  Also for Control-J
     (STANDARD, QSL BUT NO LOG, QSL AND LOG.

 - Added display of country name when showing station information.
 - Made IARU recompute zone after loading in IARU file.
 - Did DVPInit if turning on DVP from ControlJ menu.
 - Improved questions in POST L C and fixed separate band logs.
 - Fixed divide by zero if 0 QSOs per page in POST L C.
 - Made name stay on screen if multiple dupechecks.

Version 4.23 on 30-Jun-94

 - Got rid of 'AAA' debug CW message when sending function key in S&P.
 - Did better job fixing KC6 calls, and fixed KC4 and KG4 calls for USA too.

Version 4.22 on 25-June-94

 - Fixed .CTY files so KC6xxx is USA, not Belau.
 - Made packet listings go away when work.
 - Made new packet stuff work (left/right arrow).
 - Made ShiftTab get you out of S&P mode instead of TAB.
 - Made TAB in S&P swap windows like it used to.
 - Added prefix multiplier check routine.
 - Improved Merge command to shuffle logs by time/date.
 - Made country file loader see if country file already loaded.
 - Added Initial exchange to JControl.
 - Added QSO point method for RegionOneFieldDay if you are in Germany (DL).
 - Change standard call format some so EA1AB/8/P shows up as EA8/EA1AB
 - First pass at doing two rig interfaces...
 - Fixed frequency inquire for FT series radios (now it follows them!!).
 - Made radio inquires only happen every 10 seconds or so.
 - Made direct frequency entry change band of logging program instantly.
 - Made pin 17 of parallel connector go high so you can use it for a pullup?

Version 4.21 on 7-June-94

 - Made radio inquire go away if buffer in use (ie: doing RIT scan).
 - Fixed Yaesu!!, however, still need to figure out reading freq and RIT.
 - Added WARC bands to band/mode calculation.
 - Made program follow radio band and mode.
 - Allowed frequency to be input from call window.
 - Fixed Kenwood radio control when not in slow.
 - Made Exchange Window go away after S and P QSO from Space Bar.
 - Prevented turning on DVPEnable in Control-J if not DVPTSR.
 - If you turn on backcopy from Control-J, only really turns on if DVPTSR.
 - Made Alt-X stop backcopy only if answer yes.
 - Exchange function key fix.

Version 4.20 on 28-May-94

 - Eliminated KEYER = CPU command when generating a LOGCFG.DAT file.
 - Added RadioControlPortDelay so I can delay 50 ms between chars for Yaesu.
 - Fixed occasional bad call from GetRandomCall routine.
 - Fixed some POST reports that were looking for USA to be 'W', not 'K'.
 - Increased number of arrays in LOGDUPE to make up for smaller arrays.

Version 4.19 on 16-May-94.

 - Fixed rate to show up after the first minute.
 - Went to new name database format with reduced memory (about 18K calls).
 - Fixed COM4 address.
 - Eliminated MM3, KEYER, KEYER RADIO ONE and KEYER RADIO TWO commands.
 - Changed LOGCW memories to pointers.
 - Halved number of QSOs per dupe array.
 - Changed array sizes in LOGDUPE to get rid of noband/nomode.

Version 4.18 on 25 April 1994.

 - Made numbers in callsigns of visible dupesheet line up.
 - Added RATE DISPLAY (QSOs or QSOPoints).

Version 4.17 on 11 April 1994.

 - Made backcopy files get stored in current directory instead of DVPPATH.log
 - Made Alt1 thru Alt5 playback last 1, 2, 4, 8 or 16 seconds of backcopy.
 - Changed MaximumFileNames in TREE.PAS from 1000 to 200.
 - Added Alt-= command if in SSB and DVPEnabled to review backcopy files.
 - Turned off backcopy when exiting program.
 - Put note in log when doing a backcopy.
 - Made Alt-6 to Alt-0 do backcopy saves if backcopy on, else increment time.
 - Made Alt-K control DVP.
 - Fixed DVP files not getting copied into DVPPATH at start of program.
 - Assigned default values for the DVP filenames in message memories.
 - Removed QuickCommandWindow after Control-J is done.
 - Reset LastTwoLettersCrunchedOn when QSO log, deleted/undeleted or edited.
 - Made worked mults on remaining mults display use subdue color.
 - Created REMAINING MULTS WINDOW SUBDUE for LOGCFG.DAT.
 - Improved QSO by band and QSO by mode messages in POST L C.
 - Put Control-P on cryptic CW menu when using Alt-P.
 - Partials calls get displayed while entering a call into Alt-D (Dupecheck).
 - Made TAB get you out of Search and Pounce without clearing call windows.
 - Moved band off margin and right hand justified for ARRL log output (POST).
 - Made code speed separate for each radio.
 - Added clock display with seconds!!
 - Made TimerInterrupt go fast again, but with SLOW parameter string option.
 - Fixed CQ and EX function keys changes not written to LOGCFG.DAT correctly.

Version 4.16 on 25 March 1994.

 - Added DomesticCountryCall process with ADD DOMESTIC COUNTRY commands.
 - Added ADD DOMESTIC COUNTRY = CLEAR command.
 - Increased delay in DVP loop from 1 to 3 (1000 counts).
 - WONDERFUL new Control-J format.  Many new entries too!!  Alt-W for write.
 - Fixed names load problem.
 - Changed install program to make \log\dvp directory.
 - Made the default band for the WAE be 80 meters instead of 160.
 - Changed QSO Point descriptions from SSB to PHONE.

Version 4.15 around 10 March 1994

 - Fixed Control-O feature.
 - Fixed RTS signal for PacketPort: Beware of using it as CW port.
 - Added DVP first wave of functionality.

Version 4.14 on 26-Feb-1994

 - Fixed runtime error when doing QSL labels (three to a label).
 - Added long partial calls.

Version 4.13 on 23-Feb-1994

 - Added auto CQs for DVK on phone!!
 - Fixes to check partial.
 - Made SwapRadio clear any CW that was being sent.
 - Removed , for tail ending feature.
 - Made .CTY file natural prefix get added automatically if not there.
 - Eliminated possibility of serial port over-run.
 - Added summary sheet for ARRL DX (domestic).
 - Fixed POSTs mult check routine which, broken when we went to .CTY files.
 - Added WILDCARD PARTIALS command for LOGCFG.  Default is TRUE.
 - Added universal check partial function.
 - Added INITIAL EXCHANGE FILE function.
 - Fixed possibility of sending CW when hitting space bars while in SSB.

VERSION 4.12 on 8-Feb-1994.

 - Added DVK PORT command for LOGCFG.DAT (same syntax as PADDLE PORT).
 - Made F1, F2, F3, F4 and F10 do the DVK if DVK PORT <> NoPort.
 - Made installation program - with PATH modification too!!
 - Added ASCII 148/153 ---.
 - Added ASCII 132/142 .-.-
 - Added ASCII 134/143 .--.-
 - Made if answer to Control-B but in S&P, made it come up in CQ Mode.

VERSION 4.11 on 2-Feb-94.

 - Added READ command line option.
 - Got rig of messagepost
 s when reading partial calls in from LOG.DAT file.
 - Fixed InactiveRigCallingCQ initialization.
 - Fixed many LastBlock := NumberCalls DIV 200 +1 - (NumberCalls -1) DIV 200 + 1
 - Removed forced mult by band.

Rereleased Version 4.10 on 25-Jan-94

 - Fixed some W1 references in GetDomesticQTH.

VERSION 4.10 on 23-Jan-94.

 - Made Control-B flag not get reset from QSL messages.
 - Added Control-B to indicate inactive rig calling cq for auto swap.
 - Fixed program crash when pressing F1 in S&P without a callsign.
 - Made it necessary to enter YES to ControlY prompt.
 - Added support for \log\name\HELLO.DAT file.
 - Made beam heading only use memory if loaded.
 - Made Alt-Equal cha]nge CW tone right away.
 - Added REMAINING MULTS command to cty files.  Used for DX mults only.
 - Went to K1EA style country files.
 - Got rid of MyCQZone and MyITUZone.  Made MyZone.
 - Allowed overlay files to be copied to EMS if available.
 - Made ExMenu appear when going into S&P mode.

 - Some little two radio fixes.  Better results if you use ESCAPE when
     still sending DE callsign.

 - Made ControlA CW message compatable with two radio stuff.  This allows
     you to add another CQ on the run radio using control-A, and to ask for
     fills.

 - Made four character entries that don't look like a callsign, but do
     look like an ARRL section count as the QTH for SS exchanges.

VERSION 4.04 used in Internet Sprint and SS CW.

 - Added Control-A character to CW messages to send message on inactive rig.

VERSION 4.03 modemed to N6AA on 15 Oct 93.

 - Made multi messages without BAND or ALL be the same as ALL.
 - Made NAME MEMORY DISABLE command for LOGCFG and made Control-Y put it in.
 - Fixed UA1N so it would work.
 - Fixed Country ID for many of the new russian countries.
 - Change YU4 to T9 on remaining country displays.

VERSION 4.02 released to the world on 15 Oct 93

 - More help message improvements.
 - Fixed bug when using Alt-D and SpaceBar if not using TwoRadio or Packet.
 - Fixed Control-J not being able to turn off TwoRadioMode.
 - Cleaned up and updated the online help.
 - Made electronic mail show station of origin.
 - Made Delete Last Character when AutoStartEnabled only if CWEnabled.
 - Added Multi E-Mail to other terminals.
 - Added multi stuff again.

VERSION 4.01 release on 3 Oct 1993

 - Improved efficiency of TimerInterrupt code so program works on 4.77 8088?
 - Went back to using PORTs directly for character in/out/ready. No more BIOS.
 - Added POSSIBLE CALL WINDOW DUPE color and background.
 - Made possible calls show the dupes.
 - Made dupesheet use columns when possible.
 - Fixed VisibleDupeSheet bug when more than 30 calls per call area.
 - Made 9M4 same as 9M2.
 - Made AltZ work even if ExchangeWindowCursorPosition > 0.
 - Fixed zones for OM.
 - Made UA5 count as UA: (and UB5 and so on).

VERSION 4.00 sent to Japan and Europe.

 - Switched to Int 14H in BIOS for SendChar/SendByte & ReadChar & CharReady.
 - Made ESCAPE key always stop any CW in WAEQTC.
 - Fixed QTC minutes so it always sends the whole time for message nr 1.
 - Fixed ProperSalutation to take HOUR OFFSET into account.
 - Fixed country ID for the new UR (Ukraine) from UB to UR.
 - Any time CallWindowString gets cleared, the previous contents -> ControlR.
 - Made auto ask of QTC? only happen if you are receiving QTCs.
 - Fixed bug where asking for QTC after dupe hangs radio PTT on.
 - Added Control-Dash for DualingCQs.
 - Made AutoStartSend arrow reflect changes in Control-J.
 - Added Summary Sheet procedure in POST.
 - Added QTCMinutes command.
 - Allowed QTCs to be stopped in the middle.  Also some minor clean up.
 - Added SingleBand command.
 - Fixed AddBigCallAddress in LogDupe so we can have more than one array.
 - Made StartSending arrow show up after packet spot worked.
 - Added Control-Y function (dumps names).

Released Version 4.00 to Japan (10-August-1993).

 - Fixed short integers coming with S when restarting program.
 - Made RSTDomesticOrDXQTH handle NY 579 as an input.
 - Fixed PTT drop out between sending callsign and exchange with AutoSend.
 - Eliminated range checking in compiler.
 - Fixed Visible Dupesheet hanging up the program with > 200 calls.
 - No more RIT activity when doing keyboard CW or tuning.
 - Added EscapeDeletedEntry which gets recalled with ControlR.
 - Added INPUT CONFIG FILE instruction.
 - Added the FT1000 to rig support!!
 - Made the RIT function go to sleep when Alt-E, Alt-P, Control-B or Alt-O.
 - Made the program smart enough to locate the correct I/O ports (28-Jul-93)
 - Added delete capability to AutoStartSending.
 - Added name edit Y and Z commands.
 - Added Band Change report to POST
 - Added ALL JA contest.
 - Support KCJ contest in Japan (simulator too!).
 - Support JA INTL DX with simulator.
 - Made JA INTL DX work if in Japan.
 - Made All Asian work if you are in Asia.
 - Added InhibitMults function to contest exchange.
 - Added EIGHT BIT PACKET PORT command.QR  Made packet port default to 7E1.
 - Fixed unknown call garbage on continent report if more than 10.
 - Fixed IARU remaining mults display for initial ex and when QSLing.
 - Fixed no country I]D for OM.
 - Made CW element length calculation use Round.

}


Uses
     CThreads,
     Country9,
     DOS,
     FContest,
     JCtrl1,
     JCtrl2,
     CfgCmd,
     LogCfg,
     LogCW,
     LogDom,
     LogDupe,
     LogDDX,
     LogEdit,
     LogGrid,
     LogHelp,
     LogHP,
     LogMenu,
     LogPack,
     LogProm,
     LogQSONr,
     LogSCP,
     LogStuff,
     LogUDP,
     LogWAE,
     LogWind,
     N1MM,
     N4OGW,
     Printer,
     TwoBSIQ,
     trCrt,
     SlowTree,
     Tree,
     ZoneCont,
     LogK1EA,
     baseunix,
     memlinux,
     linuxsound,
     timer,
     datetimec,
     beep,
     foot,
     xkb,
     radio,
     so2r,
     keycode,
     libcurl,
     sysutils;

procedure dumpexceptioncallstack(e: exception);
var
   i: integer;
   frames: ppointer;
   report: string;
begin
   report := 'Program exception ' + lineending +
      'Stacktrace:' + lineending + lineending;
   if e <> nil then
   begin
      report := report + 'Exception class: ' + e.classname + lineending +
         'Message: ' + e.message + lineending;
   end;
   report := report + backtracestrfunc(exceptaddr);
   frames := exceptframes;
   for i := 0 to exceptframecount-1 do
     report := report + lineending+backtracestrfunc(frames[i]);
   write(stderr,report);
   halt;
end;

TYPE QTCActionType = (NoQTCAction, AbortThisQTC, SaveThisQTC);

VAR TempString: Str20;

{ Some stuff called from here but contained in LOGSUBS2 }

PROCEDURE LogLastCall; FORWARD;
PROCEDURE ProcessExchangeFunctionKey (ExtendedKey: CHAR); FORWARD;
FUNCTION  FoundCommand (VAR SendString: Str160): BOOLEAN; FORWARD;

{$I LogSubs1}

FUNCTION ParametersOkay (Call: CallString;
                         ExchangeString: Str80;
                         Band: BandType;
                         Mode: ModeType;
                         Freq: LONGINT;
                         VAR RData: ContestExchange): BOOLEAN;

{ This function get called when a carriage return has been pressed when
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
    GetRidOfPostcedingSpaces (ExchangeString);

    LookForOnDeckCall (ExchangeString);

    ExchangeErrorMessage := '';

    IF NoLog THEN
        BEGIN
        ParametersOkay := False;
        QuickDisplay ('Sorry!!   NO LOG = TRUE which prohibits logging QSOs on this computer');
        Tone.DoABeep (ThreeHarmonics);
        Exit;
        END;

    LogBadQSOString := '';

    GetTime (Hours, Minutes, Seconds, Hundreths);

    I := Hours;

    IF HourOffset <> 0 THEN
        BEGIN
        I := I + HourOffset;
        IF I > 23 THEN I := I - 24;
        IF I < 0  THEN I := I + 24;
        END;

    Hours := I;

    ParametersOkay := False;

    { Need this in case we exit soon }

    RData.Callsign := Call;

    IF (ExchangeString = '') AND NOT (ActiveExchange = RSTNameAndQTHExchange) THEN Exit;

    RData.Callsign := GetCorrectedCallFromExchangeString (ExchangeString);

    TempString := GetSentRSTFromExchangeString (ExchangeString);

    IF TempString <> '' THEN RData.RSTSent := TempString;

    IF RData.Callsign = '' THEN
        RData.Callsign := Call
    ELSE
        BEGIN
        CallWindowString := RData.Callsign;
        SaveAndSetActiveWindow (CallWindow);
        ClrScr;
        Write (CallWindowString);
        RestorePreviousWindow;
        END;

    IF ParameterOkayMode = QSLAndLog THEN
        BEGIN
        RData.Time       := Hours * 100 + Minutes;
        RData.Band       := Band;
        RData.Mode       := Mode;
        RData.Frequency  := Freq;

        IF ActiveMode = PHONE THEN
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

        ParametersOkay := True;
        LogBadQSOString := ExchangeString;
        CalculateQSOPoints (RData);
        Exit;
        END;

    IF NOT GoodCallSyntax (RData.Callsign) THEN
        BEGIN
        Tone.DoABeep (Warning);
        QuickDisplay (RData.Callsign + ' has improper syntax!!');
        Exit;
        END;

    RData.Time       := Hours * 100 + Minutes;
    RData.Band       := Band;
    RData.Mode       := Mode;
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

    ParametersOkay := ProcessExchange (ExchangeString, RData);

    IF ExchangeErrorMessage <> '' THEN
        BEGIN
        QuickDisplayError (ExchangeErrorMessage);
        ReminderPostedCount := 60;
        END;

    CalculateQSOPoints (RData);
    END;



FUNCTION HeapFunc (Size: Word): INTEGER;

    BEGIN
    HeapFunc := 1;
    END;



PROCEDURE SendFileCW;

VAR FileRead: TEXT;
    FileString: Str160;

    BEGIN
    IF OpenFileForRead (FileRead, ParamStr (2)) THEN
        BEGIN
        SetUpFileNames ('LOGCFG');
        ReadInConfigFile ('');

        ClrScr;
        WriteLn ('Ready to send CW for file ', ParamStr (2),'.  Press any key to start : ');

        REPEAT millisleep UNTIL KeyPressed;
        WHILE KeyPressed DO ReadKey;

        WriteLn ('Sending CW.  Press ESCAPE to abort or PageUp/Down to adjust speed : ');

        WHILE NOT Eof (FileRead) DO
            BEGIN
            ReadLn (FileRead, FileString);

            SendCrypticCWString (FileString);
            SendCrypticCWString ('    ');

            REPEAT
                IF KeyPressed THEN
                    CASE ReadKey OF
                        EscapeKey: BEGIN
                                   FlushCWBufferAndClearPTT;
                                   Exit;
                                   END;

                        NullKey:
                            CASE ReadKey OF
                                PageUpKey:   SpeedUp;
                                PageDownKey: SlowDown;
                                END;

                        END;

            UNTIL NOT CWStillBeingSent;
            END;


        Close (FileRead);
        END;
    END;


{$I LogSubs2}

var oa4,na4,oa8,na8,oa11,na11:  PSigActionRec;

//   procedure openconsole;cdecl;external; //need for linux beep
//   procedure getlegacy;cdecl;external; //read bios memory
//   function serialaddress(i: integer):integer;cdecl;external;
//   function paralleladdress(i: integer):integer;cdecl;external;

    BEGIN
//try
//begin

{$IFDEF Debug}

    WriteLn ('Program compiled in debug mode');
    WaitForKeyPressed;

{$ENDIF}

{$IFDEF TRFree}

    TRFree := True;
    FreeStartUpScreen;

{$ENDIF}

   curl_global_init(CURL_GLOBAL_ALL); //must be called while just 1 thread.

// Free pascal installs its own signal handlers. However, in order for users
// to get a core dump file that we can analyze in case of a crash, I
// reinstall the default signal handler for signals 4,8, and 11
// 4 = SIGILL = illegal instruction
// 8 = SIGFPE = floating point exception
// 11 = SIGSEGV = segmentation fault
    new(na4);
    new(oa4);
    na4^.sa_Handler := SigActionHandler(SIG_DFL);
    fillchar(na4^.Sa_Mask,sizeof(na4^.sa_mask),#0);
    na4^.Sa_Flags:=0;
    na4^.Sa_Restorer:=Nil;
    fpSigAction(SIGILL,na4,oa4);

    new(na8);
    new(oa8);
    na8^.sa_Handler := SigActionHandler(SIG_DFL);
    fillchar(na8^.Sa_Mask,sizeof(na8^.sa_mask),#0);
    na8^.Sa_Flags:=0;
    na8^.Sa_Restorer:=Nil;
    fpSigAction(SIGFPE,na8,oa8);

    new(na11);
    new(oa11);
    na11^.sa_Handler := SigActionHandler(SIG_DFL);
    fillchar(na11^.Sa_Mask,sizeof(na11^.sa_mask),#0);
    na11^.Sa_Flags:=0;
    na11^.Sa_Restorer:=Nil;
    fpSigAction(SIGSEGV,na11,oa11);

    RememberCWSpeed := 0;
//KS console beep only works if root
    if (FpGeteuid() = 0) then openconsole;

    IF UpperCase (ParamStr (1)) <> 'PACKETSIMULATE' THEN setupkeyboard;

    FootSwitchDebug := False;
    NetDebug        := False;
    FakePacket      := False;
    FakeBandMap     := False;
    Trace           := False;

//    CheckForName;


    GetCBreak (ControlBreakStatus);
    SetCBreak (False);

    OriginalTextMode := LastMode;

    TempString := '';

    IF ParamCount > 0 THEN
        IF UpperCase (ParamStr (1)) = 'SENDCW' THEN
            BEGIN
            SendFileCW;
            UnInitializeKeyer;
            TextMode (OriginalTextMode);
            SetCBreak (ControlBreakStatus);
            Exit;
            END
        ELSE
            IF UpperCase (ParamStr (1)) = 'NEW' THEN
                BEGIN
                PullLogCfgInformationOutOfThisPerson;
                OperateContest;
                Exit;
                END
            ELSE
                IF FileExists (ParamStr (1) + '.CFG') THEN
                    TempString := ParamStr (1)
                ELSE
                    BEGIN
                    LookForCommands;

                    TempString := SelectAvailableLog;

                    IF TempString = 'EscapeKeyPressed' THEN
                        BEGIN
                        TextMode (LastMode);
                        Exit;
                        END;
                    END;

    IF TempString = '' THEN
        TempString := SelectAvailableLog;

    IF TempString <> '' THEN SetUpFileNames (TempString);

    OperateContest;
//end;
//except
//on e: exception do
//   dumpexceptioncallstack(e);
//end;
    END.
