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

UNIT LogQSONr;

{$O+}
{$V-}

INTERFACE

USES SlowTree, Tree;

TYPE
    QSONumberObject = OBJECT
        QSONumberByBand: BOOLEAN;
        QSONumberMatrix: ARRAY [BandType] OF INTEGER;
        FUNCTION  GetCurrentQSONumber (Band: BandType): INTEGER;  { Don't think anyone uses this }
        PROCEDURE Init;
        FUNCTION  InitializeQSONumbersFromLogFile (FileName: STRING): BOOLEAN;
        FUNCTION  ReserveNewQSONumber (Band: BandType): INTEGER;
        FUNCTION  ReturnQSONumber (Band: BandType; QSONumber: INTEGER): BOOLEAN;

        PROCEDURE SetCurrentQSONumber (Band: BandType; QSONumber: INTEGER);
        END;

VAR QNumber: QSONumberObject;

IMPLEMENTATION



FUNCTION QSONumberObject.InitializeQSONumbersFromLogFile (FileName: STRING): BOOLEAN;

{ Will read in the log file (either a LOG.DAT or LOG.TMP type file) and update the
  QSONumberMatrix with the highest sent QSO numbers found.  This is intended to be
  done only once at the start of the program.  It is up to the code to keep the
  values up to date.

  New in Aug-2024, if the QSO number field has a decimal in it - indicating it is
  frequency data - we will just count up the QSOs }

VAR FileRead: TEXT;
    BandModeString, QSONumberString, FileString: STRING;
    QSONumber: INTEGER;
    LogEntryBand: BandType;
    LogEntryMode: ModeType;

    BEGIN
    IF NOT OpenFileForRead (FileRead, FileName) THEN
        BEGIN
        InitializeQSONumbersFromLogFile := False;
        Exit;
        END;

    WHILE NOT Eof (FileRead) DO
        BEGIN
        ReadLn (FileRead, FileString);
        BandModeString := RemoveFirstString (FileString);   { Band/Mode }
        DecodeBandModeString (BandModeString, LogEntryBand, LogEntryMode);  { In tree.pas }
        RemoveFirstString (FileString);   { Date }
        RemoveFirstString (FileString);   { Time }

        IF (FileString <> '') AND (LogEntryBand <> NoBand) AND (LogEntryMode <> NoMode) THEN
            BEGIN
            QSONumberString := RemoveFirstString (FileString);  { QSO Number }

            IF StringIsAllNumbers (QSONumberString) THEN
                BEGIN
                Val (QSONumberString, QSONumber);

                { We update totals for the band and all bands }

                IF QSONumber > QSONumberMatrix [All] THEN
                    QSONumberMatrix [All] := QSONumber;

                IF QSONumber > QSONumberMatrix [LogEntryBand] THEN
                    QSONumberMatrix [LogEntryBand] := QSONumber;
                END
            ELSE
                BEGIN
                { It appears someone used the QSO number field for frequency data.
                  In this case - we will just increment the QSO numbers by one }

                Inc (QSONumberMatrix [All]);
                Inc (QSONumberMatrix [LogEntryBand]);
                END;
            END;
        END;

    Close (FileRead);
    InitializeQSONumbersFromLogFile := True;
    END;



PROCEDURE QSONumberObject.Init;

VAR Band: BandType;

    BEGIN
    FOR Band := Band160 TO NoBand DO
        QSONumberMatrix [Band] := 0;
    END;



FUNCTION QSONumberObject.GetCurrentQSONumber (Band: BandType): INTEGER;

{ Similar to ReserveNextQSONumber except it returns the current QSO number
  without reserving a new one.  In other words, it will return the last
  reserved number for the band/mode.

  Note that it is expected that if the reserved QSO number came from the network
  in LogStuff - that the value in the "local" matrix was updated so that if
  someone called this routine - they will get the same QSO number that they
  received over the network when it was reserved. }

    BEGIN
    IF NOT QSONumberByBand THEN Band := All;
    GetCurrentQSONumber := QSONumberMatrix [Band];
    END;



PROCEDURE QSONumberObject.SetCurrentQSONumber (Band: BandType; QSONumber: INTEGER);

{ Used to set the QSO number to a specific value.  Used when we are getting
  our QSO numbers from someone else - but want to keep the local copy of
  the QSONumberMatrix up to date for some unknown reason }

    BEGIN
    IF NOT QSONumberByBand THEN Band := All;
    QSONumberMatrix [Band] := QSONumber;
    END;



FUNCTION QSONumberObject.ReserveNewQSONumber (Band: BandType): INTEGER;

{ Used to be GetNextQSONumber - but now clearly indicates that the number
  returned will be reserved - and thus never given out again.  This new
  procedure requires the band and mode that are to be used to generate
  the number.  This was done to support requests over the network and
  also to make 2BSIQ operation more understandable.

  The global QSONumberByMode and QSONumberByBand are both used to determine
  if the band or mode needs to be factored into the process.

  The initial contents of the array used to keep track of QSO numbers is
  all zeros with a new log.  If a log is loaded in, the highest QSO number
  for each "slot" is computed looking at the sent QSO numbers in the log
  and set to the last QSO number sent on each band/mode.  If you are not
  using QSOByBand or QSOByMode - you will look at All (for band) and Both
  (for mode) }


    BEGIN
    IF NOT QSONumberByBand THEN Band := All;

    { We need to first increment to the new QSO number since the totals in this array
      are what was sent previously }

    Inc (QSONumberMatrix [Band]);
    ReserveNewQSONumber := QSONumberMatrix [Band];
    END;



FUNCTION QSONumberObject.ReturnQSONumber (Band: BandType; QSONumber: INTEGER): BOOLEAN;

{ Will try to return an unused QSO number so someone else can use it. Returns
  TRUE is successful }

    BEGIN
    IF NOT QSONumberByBand THEN Band := All;

    IF QSONumberMatrix [Band] = QSONumber THEN  { Okay to return it }
        IF QSONumber > 0 THEN
            BEGIN
            Dec (QSONumberMatrix [Band]);
            ReturnQSONumber := True;
            Exit;
            END;

    ReturnQSONumber := False;
    END;



    BEGIN
    END.
