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

UNIT K1EANet;

{$O+}
{$V-}

INTERFACE

USES Dos, Tree, LogWind, LogK1EA, SlowTree,datetimec;


VAR K1EAStationID: CHAR;

FUNCTION  ConvertN6TRLogStringToK1EANetworkFormat (N6TRLogString: STRING): STRING;

FUNCTION  ConvertK1EANetworkLogMessageToN6TRLogString (K1EANetworkLogMessage: STRING): STRING;

FUNCTION  GetK1EABandIntegerFromFrequency (Frequency: LONGINT): CHAR;
FUNCTION  GetUnixTime: LONGINT;

PROCEDURE TestUnixTimeConversionRoutines;

IMPLEMENTATION

uses radio;



FUNCTION ConvertN6TRLogStringToK1EANetworkFormat (N6TRLogString: STRING): STRING;

{ This procedure will convert a N6TR Log string to format required to send
  to the K1EA netowrk.

Input string format :

 80CW  04-Oct-03 16:57    1  JA1KSO      *  599  599  25            JA 25    3

Output string format :

L7 599 14025000 14025000 1062530106 0 4 1 RU1A 16 0 7 7407 0 ? <cr>

Station 7 has just logged RU1A with a received RST of 599.  Both VFOs are
on 14025000.  The TIME is in seconds since Jan 1, 1980.  0 is the status
which means simplex and the only one we will bother sending.  4 is the band
(160 = 1, 10 = 6).  1=CW (2=SSB).  16 is the received zone.

The next four values are not used.  The ? is a checksum - add up all of
the characters in the string so far, or with $80.  Then a <cr>.   }

VAR BandChar, ModeChar: CHAR;
    Band: BandType;
    Mode: ModeType;
    Call: CallString;
    ZoneString, RSTSTring, ExchangeString, TimeString, DateString: Str20;
    FrequencyString, UnixTimeString: Str20;
    Year, Month, Day, Hour, Minute: WORD;
    Freq, DefaultFreq, UnixTime: LONGINT;
    K1EAString: STRING;

    BEGIN
    Band := GetLogEntryBand (N6TRLogString);

    CASE Band OF
        Band160: BEGIN
                 BandChar := '1';
                 DefaultFreq := 1800000;
                 END;

        Band80:  BEGIN
                 BandChar := '2';
                 DefaultFreq := 3500000;
                 END;

        Band40:  BEGIN
                 BandChar := '3';
                 DefaultFreq := 7000000;
                 END;

        Band20:  BEGIN
                 BandChar := '4';
                 DefaultFreq := 14000000;
                 END;

        Band15:  BEGIN
                 BandChar := '5';
                 DefaultFreq := 21000000;
                 END;

        Band10:  BEGIN
                 BandChar := '6';
                 DefaultFreq := 28000000;
                 END;

        ELSE     BEGIN
                 BandChar := '?';
                 DefaultFreq := 0;
                 END;
        END;

    Mode := GetLogEntryMode (N6TRLogString);

    CASE Mode OF
        CW:    ModeChar := '1';
        Phone: ModeChar := '2';
        ELSE   ModeChar := '?';
        END;

    Call := GetLogEntryCall (N6TRLogString);

    { Figure out the Unix time }

    DateString := GetLogEntryDateString (N6TRLogString);

    DateStringToYearMonthDay (DateString, Year, Month, Day);

    TimeString := GetLogEntryTimeString (N6TRLogString);

    TimeStringToHourAndMinute (TimeString, Hour, Minute);

    UnixTime := Norm2Unix (Year, Month, Day, Hour, Minute, 0);
    Str (UnixTime, UnixTimeString);

    ExchangeString := GetLogEntryExchangeString (N6TRLogString);

    RemoveFirstString (ExchangeString); { Dump sent RST }
    RSTString := RemoveFirstString (ExchangeString); { Received RST }
    ZoneString := RemoveFirstString (ExchangeString); { Recevied Zone }

    IF ActiveRadio = RadioOne THEN
        BEGIN
        Freq := StableRadio1Freq;
        IF Freq = 0 THEN Freq := DefaultFreq;
        Str (Freq, FrequencyString);
        END
    ELSE
        BEGIN
        Freq := StableRadio2Freq;
        IF Freq = 0 THEN Freq := DefaultFreq;
        Str (Freq, FrequencyString);
        END;

    K1EAString := 'L' + K1EAStationID + ' ' + RSTString + ' ' +
                  FrequencyString + ' ' + FrequencyString + ' ' +
                  UnixTimeString + ' 0 ' + BandChar + ' ' + ModeChar +
                  ' ' + Call + ' ' + ZoneString + ' 0 ' + K1EAStationID + ' 1 0 ';

    ConvertN6TRLogStringToK1EANetworkFormat := K1EAString;
    END;



FUNCTION ConvertK1EANetworkLogMessageToN6TRLogString (K1EANetworkLogMessage: STRING): STRING;

{ This procedure will convert a K1EA network log message to the N6TR log
  string format

Input string format :

L7 599 14025000 14025000 1062530106 0 4 1 RU1A 16 0 7 7407 0 ? <cr>

Station 7 has just logged RU1A with a received RST of 599.  Both VFOs are
on 14025000.  The TIME is in seconds since Jan 1, 1980.  0 is the status
which means simplex and the only one we will bother sending.  4 is the band
(160 = 1, 10 = 6).  1=CW (2=SSB).  16 is the received zone.

The next four values are not used.  The ? is a checksum - add up all of
the characters in the string so far, or with $80.  Then a <cr>.

Output string format :

 80CW  04-Oct-03 16:57    0  JA1KSO      *  599  599  25                     0

This will also handle the update format:

U7 599 7025000 7025000 1062530106 0 3 1 G4LNS 14 0 2 1 0 599 7025000 7025000 ...

}

VAR RSTString, UnixTimeString, BandString, ModeString, CallSign, ZoneString: Str20;
    BandStringChar, ModeStringChar: CHAR;
    UnixDate: LONGINT;
    xResult, Year, Month, Day, Hour, Minute, Second: WORD;
    DayString, MonthString, YearString, HourString, MinuteString: Str20;

    N6TRLogString: STRING;

    BEGIN
    { Parse the message into individual strings }

    RemoveFirstString (K1EANetworkLogMessage);  { Get rid of message type and source }

    RSTString       := RemoveFirstString (K1EANetworkLogMessage);

    RemoveFirstString (K1EANetworkLogMessage);  { Don't need frequency }
    RemoveFirstString (K1EANetworkLogMessage);  { Don't need second frequency string }

    UnixTimeString  := RemoveFirstString (K1EANetworkLogMessage);

    RemoveFirstString (K1EANetworkLogMessage); { Don't need simplex byte }

    BandString      := RemoveFirstString (K1EANetworkLogMessage);
    ModeString      := RemoveFirstString (K1EANetworkLogMessage);
    CallSign        := RemoveFirstString (K1EANetworkLogMessage);
    ZoneString      := RemoveFirstString (K1EANetworkLogMessage);

    BandStringChar := BandString [1];
    ModeStringChar := ModeString [1];

    CASE BandStringChar OF
        '1': N6TRLogString := '160';
        '2': N6TRLogString := ' 80';
        '3': N6TRLogString := ' 40';
        '4': N6TRLogString := ' 20';
        '5': N6TRLogString := ' 15';
        '6': N6TRLogString := ' 10';
        ELSE N6TRLogString := '???';
        END;

    CASE ModeStringChar OF
        '1': N6TRLogString := N6TRLogString + 'CW ';
        '2': N6TRLogString := N6TRLogString + 'SSB';
        ELSE N6TRLogString := N6TRLogString + '???';
        END;

    Val (UnixTimeString, UnixDate, xResult);
    if xResult <> 0 then UnixDate := 0; //This shouldn't happen

    Unix2Norm (UnixDate, Year, Month, Day, Hour, Minute, Second);

    Str (Day, DayString);

    IF Length (DayString) = 1 THEN DayString := '0' + DayString;

    CASE Month OF
         1: MonthString := 'JAN';
         2: MonthString := 'FEB';
         3: MonthString := 'MAR';
         4: MonthString := 'APR';
         5: MonthString := 'MAY';
         6: MonthString := 'JUN';
         7: MonthString := 'JUL';
         8: MonthString := 'AUG';
         9: MonthString := 'SEP';
        10: MonthString := 'OCT';
        11: MonthString := 'NOV';
        12: MonthString := 'DEC';
        ELSE
            MonthString := '???';
        END;

    Str (Year, YearString);

    YearString := Copy (YearString, Length (YearString) - 1, 2);

    Str (Hour, HourString);

    IF Length (HourString) = 1 THEN HourString := '0' + HourString;

    Str (Minute, MinuteString);

    IF Length (MinuteString) = 1 THEN MinuteString := '0' + MinuteString;

    N6TRLogString := N6TRLogString + ' ' + DayString + '-' +
                     MonthString + '-' + YearString + ' ' +
                     HourString + ':' + MinuteString;


    { We will make the sent QSO number = 0 for now }

    N6TRLogString := N6TRLogString + '    0  ';

    { Add the callsign }

    WHILE Length (Callsign) < 12 DO Callsign := Callsign + ' ';

    N6TRLogString := N6TRLogString + Callsign + '   ';

    { Now force either 59 or 599 as the sent RS(T) }

    CASE ModeStringChar OF
        '1': N6TRLogString := N6TRLogString + '599  ';
        '2': N6TRLogString := N6TRLogString + '59   ';
        ELSE N6TRLogString := N6TRLogString + '???  ';
        END;

    N6TRLogString := N6TRLogString + RSTString;

    WHILE Length (N6TRLogString) < 54 DO
        N6TRLogString := N6TRLogString + ' ';

    N6TRLogString := N6TRLogString + ZoneString;

    WHILE Length (N6TRLogString) < 77 DO
        N6TRLogString := N6TRLogString + ' ';

    { Someone else will have to figure out the QSO points - we will just
      put 3 there for now }

    N6TRLogString := N6TRLogString + '3';

    ConvertK1EANetworkLogMessageToN6TRLogString := N6TRLogString;
    END;




PROCEDURE TestUnixTimeConversionRoutines;

VAR InputString: Str20;
    Year, Month, Day, Hour, Minute, Second: WORD;
    UnixDate: LONGINT;

    BEGIN
    REPEAT
        InputString := GetResponse ('Enter either Unix or dd-mmm-yy : ');

        IF InputString = '' THEN Exit;

        IF StringIsAllNumbers (InputString) THEN
            BEGIN
            Val (InputString, UnixDate);

            Unix2Norm (UnixDate, Year, Month, Day, Hour, Minute, Second);

            WriteLn ('Year = ', Year);
            WriteLn ('Month = ', Month);
            WriteLn ('Day = ', Day);
            WriteLn ('Hour = ', Hour);
            WriteLn ('Minute = ', Minute);
            WriteLn ('Second = ', Second);
            END
        ELSE
            BEGIN
            DateStringToYearMonthDay (InputString, Year, Month, Day);

            WriteLn ('Year = ', Year, '  Month = ', Month, '  Day = ', Day);

            InputString := GetResponse ('Enter time hh:mm : ');

            TimeStringToHourAndMinute (InputString, Hour, Minute);

            WriteLn ('Hour = ', Hour, '  Minute = ', Minute);

            UnixDate := Norm2Unix (Year, Month, Day, Hour, Minute, 0);

            WriteLn ('Unix time = ', UnixDate);
            END;

    WriteLn;
    UNTIL False;
    END;



FUNCTION GetUnixTime: LONGINT;

VAR Year, Month, Day, DayOfWeek, Hour, Minute, Second, Secs100: WORD;

    BEGIN
    GetDate (Year, Month, Day, DayOfWeek);
    GetTime (Hour, Minute, Second, Secs100);
    GetUnixTime := Norm2Unix (Year, Month, Day, Hour, Minute, Second);
    END;



FUNCTION GetK1EABandIntegerFromFrequency (Frequency: LONGINT): CHAR;

    BEGIN
    IF Frequency < 3000000 THEN
        BEGIN
        GetK1EABandIntegerFromFrequency := '1';
        Exit;
        END;

    IF Frequency < 6000000 THEN
        BEGIN
        GetK1EABandIntegerFromFrequency := '2';
        Exit;
        END;

    IF Frequency < 13000000 THEN
        BEGIN
        GetK1EABandIntegerFromFrequency := '3';
        Exit;
        END;

    IF Frequency < 20000000 THEN
        BEGIN
        GetK1EABandIntegerFromFrequency := '4';
        Exit;
        END;

    IF Frequency < 2400000 THEN
        BEGIN
        GetK1EABandIntegerFromFrequency := '5';
        Exit;
        END;

    GetK1EABandIntegerFromFrequency := '6';
    END;



    BEGIN
    K1EAStationId := Chr (0);
    END.
