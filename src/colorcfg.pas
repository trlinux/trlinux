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

{ This file contains the configuration commands for setting the
  different window colors.  There is one complete set for the color
  mode and another for monochrome.  Setting the DISPLAY MODE option
  will select the one you want.  This allows you to set up your
  favorite colors when using a color monitor, but keep the setting you
  may like best when you use your monochrome laptop.

  As with the LOGCFG.PAS file, the ID is the first part of the configuration
  command and CMD is what you put after the equal sign.  An example:

  Color Color Alarm Window = Yellow

  This command will set the character color in the Alarm Window to Yellow
  if you are in the color display mode.

  The possible colors are:

  Black, Blue, Green, Cyan, Red, Magenta, Brown, LightGray, DarkGray, Yellow,
  LightBlue, LightGreen, LightCyan, LightRed, LightMagenta, and White.

  }

FUNCTION ValidColorCommand (CMD: Str80; ID: Str80): BOOLEAN;

    BEGIN
    ValidColorCommand := False;
    IF NOT ((StringHas (CMD, 'COLOR')) OR (StringHas (CMD, 'BACKGROUND'))) THEN Exit;
    IF NOT StringHas (CMD, 'WINDOW') THEN Exit;
    ValidColorCommand := True;

    IF CMD = 'ALARM WINDOW COLOR' THEN
        ColorColors.AlarmWindowColor := GetColorInteger (ID);

    IF CMD = 'ALARM WINDOW BACKGROUND' THEN
        ColorColors.AlarmWindowBackground := GetColorInteger (ID);

    IF CMD = 'BAND MAP WINDOW COLOR' THEN
        ColorColors.BandMapWindowColor := GetColorInteger (ID);

    IF CMD = 'BAND MAP WINDOW BACKGROUND' THEN
        ColorColors.BandMapWindowBackground := GetColorInteger (ID);

    IF CMD = 'BAND MODE WINDOW COLOR' THEN
        ColorColors.BandModeWindowColor := GetColorInteger (ID);

    IF CMD = 'BAND MODE WINDOW BACKGROUND' THEN
        ColorColors.BandModeWindowBackground := GetColorInteger (ID);

    IF CMD = 'BEAM HEADING WINDOW COLOR' THEN
        ColorColors.BeamHeadingWindowColor := GetColorInteger (ID);

    IF CMD = 'BEAM HEADING WINDOW BACKGROUND' THEN
        ColorColors.BeamHeadingWindowBackground := GetColorInteger (ID);

    IF CMD = 'BIG WINDOW COLOR' THEN
        ColorColors.BigWindowColor := GetColorInteger (ID);

    IF CMD = 'BIG WINDOW BACKGROUND' THEN
        ColorColors.BigWindowBackground := GetColorInteger (ID);

    IF CMD = 'CALL WINDOW COLOR' THEN
        ColorColors.CallWindowColor := GetColorInteger (ID);

    IF CMD = 'CALL WINDOW BACKGROUND' THEN
        ColorColors.CallWindowBackground := GetColorInteger (ID);

    IF CMD = 'CLOCK WINDOW COLOR' THEN
        ColorColors.ClockWindowColor := GetColorInteger (ID);

    IF CMD = 'CLOCK WINDOW BACKGROUND' THEN
        ColorColors.ClockWindowBackground := GetColorInteger (ID);

    IF CMD = 'CODE SPEED WINDOW COLOR' THEN
        ColorColors.CodeSpeedWindowColor := GetColorInteger (ID);

    IF CMD = 'CODE SPEED WINDOW BACKGROUND' THEN
        ColorColors.CodeSpeedWindowBackground := GetColorInteger (ID);

    IF CMD = 'CONTEST TITLE WINDOW COLOR' THEN
        ColorColors.ContestTitleWindowColor := GetColorInteger (ID);

    IF CMD = 'CONTEST TITLE WINDOW BACKGROUND' THEN
        ColorColors.ContestTitleWindowBackground := GetColorInteger (ID);

    IF CMD = 'DATE WINDOW COLOR' THEN
        ColorColors.DateWindowColor := GetColorInteger (ID);

    IF CMD = 'DATE WINDOW BACKGROUND' THEN
        ColorColors.DateWindowBackground := GetColorInteger (ID);

    IF CMD = 'DUPE INFO WINDOW COLOR' THEN
        ColorColors.DupeInfoWindowColor := GetColorInteger (ID);

    IF CMD = 'DUPE INFO WINDOW BACKGROUND' THEN
        ColorColors.DupeInfoWindowBackground := GetColorInteger (ID);

    IF CMD = 'DUPESHEET WINDOW COLOR' THEN
        ColorColors.DupeSheetWindowColor := GetColorInteger (ID);

    IF CMD = 'DUPESHEET WINDOW BACKGROUND' THEN
        ColorColors.DupeSheetWindowBackground := GetColorInteger (ID);

    IF CMD = 'EDITABLE LOG WINDOW COLOR' THEN
        ColorColors.EditableLogWindowColor := GetColorInteger (ID);

    IF CMD = 'EDITABLE LOG WINDOW BACKGROUND' THEN
        ColorColors.EditableLogWindowBackground := GetColorInteger (ID);

    IF CMD = 'EXCHANGE WINDOW COLOR' THEN
        ColorColors.ExchangeWindowColor := GetColorInteger (ID);

    IF CMD = 'EXCHANGE WINDOW BACKGROUND' THEN
        ColorColors.ExchangeWindowBackground := GetColorInteger (ID);

    IF CMD = 'EXCHANGE WINDOW S&P BACKGROUND' THEN
        ColorColors.ExchangeSAndPWindowBackground := GetColorInteger (ID);

    IF CMD = 'FREE MEMORY WINDOW COLOR' THEN
        ColorColors.FreeMemoryWindowColor := GetColorInteger (ID);

    IF CMD = 'FREE MEMORY WINDOW BACKGROUND' THEN
        ColorColors.FreeMemoryWindowBackground := GetColorInteger (ID);

    IF CMD = 'FUNCTION KEY WINDOW COLOR' THEN
        ColorColors.FunctionKeyWindowColor := GetColorInteger (ID);

    IF CMD = 'FUNCTION KEY WINDOW BACKGROUND' THEN
        ColorColors.FunctionKeyWindowBackground := GetColorInteger (ID);

    IF CMD = 'INSERT WINDOW COLOR' THEN
        ColorColors.InsertWindowColor := GetColorInteger (ID);

    IF CMD = 'INSERT WINDOW BACKGROUND' THEN
        ColorColors.InsertWindowBackground := GetColorInteger (ID);

    IF CMD = 'MULTIPLIER INFORMATION WINDOW COLOR' THEN
        ColorColors.MultiplierInformationWindowColor := GetColorInteger (ID);

    IF CMD = 'MULTIPLIER INFORMATION WINDOW BACKGROUND' THEN
        ColorColors.MultiplierInformationWindowBackground := GetColorInteger (ID);

    IF CMD = 'NAME PERCENTAGE WINDOW COLOR' THEN
        ColorColors.NamePercentageWindowColor := GetColorInteger (ID);

    IF CMD = 'NAME PERCENTAGE WINDOW BACKGROUND' THEN
        ColorColors.NamePercentageWindowBackground := GetColorInteger (ID);

    IF CMD = 'NAME SENT WINDOW COLOR' THEN
        ColorColors.NameSentWindowColor := GetColorInteger (ID);

    IF CMD = 'NAME SENT WINDOW BACKGROUND' THEN
        ColorColors.NameSentWindowBackground := GetColorInteger (ID);

    IF CMD = 'POSSIBLE CALL WINDOW COLOR' THEN
        ColorColors.PossibleCallWindowColor := GetColorInteger (ID);

    IF CMD = 'POSSIBLE CALL WINDOW BACKGROUND' THEN
        ColorColors.PossibleCallWindowBackground := GetColorInteger (ID);

    IF CMD = 'POSSIBLE CALL WINDOW DUPE COLOR' THEN
        ColorColors.PossibleCallWindowDupeColor := GetColorInteger (ID);

    IF CMD = 'POSSIBLE CALL WINDOW DUPE BACKGROUND' THEN
        ColorColors.PossibleCallWindowDupeBackground := GetColorInteger (ID);

    IF CMD = 'QSO INFORMATION WINDOW COLOR' THEN
        ColorColors.QSOInformationWindowColor := GetColorInteger (ID);

    IF CMD = 'QSO INFORMATION WINDOW BACKGROUND' THEN
        ColorColors.QSOInformationWindowBackground := GetColorInteger (ID);

    IF CMD = 'QSO NUMBER WINDOW COLOR' THEN
        ColorColors.QSONumberWindowColor := GetColorInteger (ID);

    IF CMD = 'QSO NUMBER WINDOW BACKGROUND' THEN
        ColorColors.QSONumberWindowBackground := GetColorInteger (ID);

    IF CMD = 'QTC NUMBER WINDOW COLOR' THEN
        ColorColors.QTCNumberWindowColor := GetColorInteger (ID);

    IF CMD = 'QTC NUMBER WINDOW BACKGROUND' THEN
        ColorColors.QTCNumberWindowBackground := GetColorInteger (ID);

    IF CMD = 'QUICK COMMAND WINDOW COLOR' THEN
        ColorColors.QuickCommandWindowColor := GetColorInteger (ID);

    IF CMD = 'QUICK COMMAND WINDOW BACKGROUND' THEN
        ColorColors.QuickCommandWindowBackground := GetColorInteger (ID);

    IF CMD = 'RADIO WINDOW COLOR' THEN {KK1L: 6.73}
        BEGIN
        ColorColors.RadioOneWindowColor := GetColorInteger (ID);
        ColorColors.RadioTwoWindowColor := GetColorInteger (ID);
        END;

    IF CMD = 'RADIO WINDOW BACKGROUND' THEN {KK1L: 6.73}
        BEGIN
        ColorColors.RadioOneWindowBackground := GetColorInteger (ID);
        ColorColors.RadioTwoWindowColor := GetColorInteger (ID);
        END;

    IF CMD = 'RADIO ONE WINDOW COLOR' THEN {KK1L: 6.73}
        ColorColors.RadioOneWindowColor := GetColorInteger (ID);

    IF CMD = 'RADIO ONE WINDOW BACKGROUND' THEN {KK1L: 6.73}
        ColorColors.RadioOneWindowBackground := GetColorInteger (ID);

    IF CMD = 'RADIO TWO WINDOW COLOR' THEN {KK1L: 6.73}
        ColorColors.RadioTwoWindowColor := GetColorInteger (ID);

    IF CMD = 'RADIO TWO WINDOW BACKGROUND' THEN {KK1L: 6.73}
        ColorColors.RadioTwoWindowBackground := GetColorInteger (ID);

    IF CMD = 'RATE WINDOW COLOR' THEN
        ColorColors.RateWindowColor := GetColorInteger (ID);

    IF CMD = 'RATE WINDOW BACKGROUND' THEN
        ColorColors.RateWindowBackground := GetColorInteger (ID);

    IF CMD = 'RTTY WINDOW COLOR' THEN
        ColorColors.RTTYWindowColor := GetColorInteger (ID);

    IF CMD = 'RTTY WINDOW BACKGROUND' THEN
        ColorColors.RTTYWindowBackground := GetColorInteger (ID);

    IF CMD = 'RTTY INVERSE WINDOW COLOR' THEN
        ColorColors.RTTYInverseWindowColor := GetColorInteger (ID);

    IF CMD = 'RTTY INVERSE WINDOW BACKGROUND' THEN
        ColorColors.RTTYInverseWindowBackground := GetColorInteger (ID);

    IF CMD = 'REMAINING MULTS WINDOW SUBDUE COLOR' THEN
        ColorColors.RemainingMultsWindowSubdue := GetColorInteger (ID);

    IF CMD = 'REMAINING MULTS WINDOW COLOR' THEN
        ColorColors.RemainingMultsWindowColor := GetColorInteger (ID);

    IF CMD = 'REMAINING MULTS WINDOW BACKGROUND' THEN
        ColorColors.RemainingMultsWindowBackground := GetColorInteger (ID);

    IF CMD = 'TOTAL WINDOW COLOR' THEN
        ColorColors.TotalWindowColor := GetColorInteger (ID);

    IF CMD = 'TOTAL WINDOW BACKGROUND' THEN
        ColorColors.TotalWindowBackground := GetColorInteger (ID);

    IF CMD = 'TOTAL SCORE WINDOW COLOR' THEN
        ColorColors.TotalScoreWindowColor := GetColorInteger (ID);

    IF CMD = 'TOTAL SCORE WINDOW BACKGROUND' THEN
        ColorColors.TotalScoreWindowBackground := GetColorInteger (ID);

    IF CMD = 'USER INFO WINDOW COLOR' THEN
        ColorColors.UserInfoWindowColor := GetColorInteger (ID);

    IF CMD = 'USER INFO WINDOW BACKGROUND' THEN
        ColorColors.UserInfoWindowBackground := GetColorInteger (ID);

    IF CMD = 'WHOLE SCREEN WINDOW COLOR' THEN
        ColorColors.WholeScreenColor := GetColorInteger (ID);

    IF CMD = 'WHOLE SCREEN WINDOW BACKGROUND' THEN
        ColorColors.WholeScreenBackground := GetColorInteger (ID);

    IF CMD = 'SCP WINDOW DUPE COLOR' THEN
        SCPDupeColor := GetColorInteger (ID);

    IF CMD = 'SCP WINDOW DUPE BACKGROUND' THEN
        SCPDupeBackground := GetColorInteger (ID);
    END;
