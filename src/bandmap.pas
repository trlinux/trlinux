;//
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
//<http://www.gnu.org/licenses/>.                                                         w
//

UNIT BandMap;

{ This is a totally new bandmap created in 2024 by combing through
  the existing bandmap code with a fine toothed comb.  Some unused
  functionality was removed.  Most of the old comments from KK1L
  were removed as they often were cancelled out with other changes
  and it was more confusing trying to follow the comments than it
  was to just follow the code.

  Decided to make it an object using CLASS.

  The bandmap itself is found in a bunch of arrays - one per band.
  The old bandmap had separate arrays for CW and SSB, but they are
  combined now.  Each entry however will have the mode and this is
  either the "real" mode of an entry added from the radio - or a
  "calculated" mode based upon the band map frequency thresholds.

  If you don't want WARC or VHF entries showing up - then make sure
  WARCBandsEnabled and/or VHFBandsEnabled are false.  Even if you
  have these set to false - the data will still be in the band
  map arrays. }

{$O+}
{$V-}

INTERFACE

USES LogGrid, LogDom, LogK1EA, SlowTree, Tree, trCrt, Dos, LogSCP,
     ZoneCont, Country9, datetimec, radio, N4OGW, LogWind;

CONST
    Blink = 128;
    BandMapFileName = 'BANDMAP.BIN';
    FileVersion = 'D';  { Was C before October 2024 }

TYPE
    BandMapEntry = RECORD
        Call:       STRING [12];
        Frequency:  LONGINT;      { In Hertz }
        Mode:       ModeType;     { Either determine from radio making entry - or freq limits }
        QSXOffset:  LONGINT;      { + or - offset from Frequency }
        StatusByte: BYTE;         { Bits 0-5 = # minutes remaining (0-63)
                                    Bit 6 = Dupe   Bit 7 = Mult   }
        PreviousEntry: POINTER;   { New for October 2024 - makes it easier to manage delete }
        NextEntry:  POINTER;
        END;

    BandMapEntryPointer  = ^BandMapEntry;
    BandMapSplitModeType = (ByCutoffFrequency, AlwaysPhone); {KK1L: 6.64}

    BandMapClass = CLASS
        AllBands:            BOOLEAN;                 { Show all bands as one bandmap }
        AllModes:            BOOLEAN;                 { Show both modes as one bandmap }
        Band:                BandType;                { Current band of the bandmap being shown }
        Mode:                ModeType;                { Current mode of the bandmap being shown }
        BlinkingCall:        CallString;              { If a call is blinking - this is it }
        BlinkingCallRecord:  BandMapEntryPointer;     { Full data record of blinking call }
        CursorEntry:         BandMapEntryPointer;     { Full data record of entry with cursor }
        CursorFrequency:     LONGINT;                 { Frequency of entry with cursor }
        DisplayCQ:           BOOLEAN;                 { Enable showing CQ frequencies on band map }
        DisplayEnable:       BOOLEAN;                 { If FALSE - no display of the bandmap }
        ModeCutoffFrequency: ARRAY [Band160..Band2] OF LONGINT;  { Used to determine mode if not specified }
        DecayTime:           INTEGER;                 { Timeout for entries }
        DecayValue:          INTEGER; {KK1L: 6.65}
        DecayMultiplier:     INTEGER; {KK1L: 6.65}
        DupeDisplay:         BOOLEAN;                 { Whether to show dupes or not }
        Enable:              BOOLEAN;                 { Turns the bandmap on or off }
        EntryInCallWindow:   BOOLEAN;                 { If we have put an entry in the call window while tuning }

        FirstDisplayableFrequency: LONGINT; {KK1L: 6.64}
        FirstDisplayedColumn:      INTEGER; {KK1L: 6.64 allows tracking of bandmap display overflow}
        FirstDisplayedFrequency:   LONGINT; {KK1L: 6.64}

        { First entry list used to have by mode - but I have removed that.  We
          are now putting the mode (if known) into the band map entry itself. }

        FirstEntryList:            ARRAY [Band160..NoBand] OF BandMapEntryPointer;

        GuardBand:                 INTEGER;           { How close VFO needs to be for an entry to blink }
        InEdit:                    BOOLEAN;           { Internally used to tell display when we are editing }
        JustLoadingWithNoRadio:    BOOLEAN;           { ??? see logsubs2.pas }
        LastDisplayableFrequency:  LONGINT; {KK1L: 6.64}
        LastDisplayedFrequency:    LONGINT; {KK1L: 6.64}
        LastEditedEntry:           INTEGER;
        MinutesSinceLastUpdate:    INTEGER; {KK1L: 6.65}
        MultsOnly:                 BOOLEAN; {KK1L: 6.68}
        NumberEntries:             INTEGER; {KK1L: 6.64 needed a global version of NumberVisibleBMEntries}
        OkayToPutUpCall:           BOOLEAN;
        SplitMode:                 BandMapSplitModeType; {KK1L: 6.64}
        StartingFrequencies:       ARRAY [BandType] OF LONGINT;
        StoppingFrequencies:       ARRAY [BandType] OF LONGINT;
        SyncNeeded:                BOOLEAN;
        WindowRY:                  INTEGER;

        PROCEDURE AddModeCutoffFrequency (Freq: LONGINT);  { Any frequency above will be SSB }

        PROCEDURE AddEntry (Call: CallString;
                            Frequency: LONGINT;
                            QSXFrequency: LONGINT;
                            Mode: ModeType;                { If you know the mode - otherwise NoMode }
                            Dupe: BOOLEAN;
                            Mult: BOOLEAN;
                            MinutesLeft: BYTE);

        PROCEDURE CalculateNumberVisibleEntries (VAR NumberVisibleBandMapEntries: INTEGER;
                                                 VAR CursorEntryNumber: INTEGER);

        PROCEDURE Create;   { In additional to automatically allocating memory - inits the band arrays }

        PROCEDURE DecrementTimes;  { Subtract one minute - if zero - delete entry }

        PROCEDURE DeleteCallFromN4OGWBandmap (Call: Callstring);
        PROCEDURE DeleteCall (Call: CallString);    { Does it for all bands/modes }
        PROCEDURE DeleteEntry (VAR Entry: BandMapEntryPointer);

        PROCEDURE Display;
        PROCEDURE DisplayBandMode (Band: BandType; Mode: ModeType; UpdateRadio: BOOLEAN);

        PROCEDURE Edit;

        PROCEDURE GetDisplayInfo (VAR MaxEntriesPerPage: INTEGER; VAR NumberBandMapRows: INTEGER);

        PROCEDURE GetBandModeFromFrequency (Frequency: LONGINT;
                                                   VAR Band: BandType;
                                                   VAR Mode: ModeType);

        FUNCTION  GetRecordForCursor (VAR Entry: BandMapEntryPointer;
                                                 CursorEntryNumber: INTEGER) : BOOLEAN;

        PROCEDURE LoadFromFile;

        FUNCTION  NextNonDupeEntry (Band: BandType; Mode: ModeType): BOOLEAN;
        FUNCTION  NextMultiplierEntry (Band: BandType; Mode: ModeType): BOOLEAN; {KK1L: 6.68}
        FUNCTION  NextNonDupeEntryDisplayed (Band: BandType; Mode: ModeType): BOOLEAN;
        FUNCTION  NextMultiplierEntryDisplayed (Band: BandType; Mode: ModeType): BOOLEAN; {KK1L: 6.68}

        PROCEDURE RemoveCallFromLinkedList (CallsignToRemove: CallString; Band: BandType);
        PROCEDURE ResetTimes; { Set decay times to new value for all entries }

        PROCEDURE SaveToFile;
        PROCEDURE SendCallsToN4OGW;
        PROCEDURE SetRadioUpForEntry (Entry: BandMapEntryPointer; Radio: RadioType);

        PROCEDURE ShowCursor (CursorPosition: INTEGER;
                              NumberVisibleBandMapEntries: INTEGER;
                              Hide: BOOLEAN);

        PROCEDURE UpdateBlinkingCall;
        END;  { of CLASS }

VAR
    BandMap: BandMapClass;   { Still needs to execute create to allocate memory }

IMPLEMENTATION

Uses LogStuff,keycode,beep,xkb,timer;



PROCEDURE BandMapClass.DecrementTimes;

{ Call this once a minute.  Decrements all times by one.  Any that were at
  zero get deleted.  This does not actually display the updated band map.
  Any call that gets deleted will also be sent to the N4OGW bandmap for
  deletion. }

VAR EntryRecord: BandMapEntryPointer;
    MinutesLeft: BYTE;
    Band: BandType;

    BEGIN
    FOR Band := Band160 TO NoBand DO
        IF FirstEntryList [Band] <> nil THEN
            BEGIN
            EntryRecord := FirstEntryList [Band];

            WHIlE EntryRecord <> nil DO
                BEGIN
                MinutesLeft := EntryRecord^.StatusByte AND $3F;

                IF MinutesLeft = 0 THEN   { Time to die }
                    BEGIN
                    DeleteCallFromN4OGWBandMap (EntryRecord^.Call);

                    { Remove it from the linked list. EntryRecord will be updated
                      to the next entry in the list - nil if nothing left }

                    DeleteEntry (EntryRecord);
                    Continue;
                    END;

                { We aren't going to delete this entry - decrement the minute count }

                Dec (MinutesLeft);

                { Save the new minutes left value in the first 6 bits of StatusByte }

                EntryRecord^.StatusByte := EntryRecord^.StatusByte AND $C0;
                EntryRecord^.StatusByte := EntryRecord^.StatusByte OR MinutesLeft;

                { Move to the next entry.  Remember the address of this entry in
                  case I need to link its NextEntry to a different place }

                EntryRecord := EntryRecord^.NextEntry;  { Point to next entry }
                END;
            END;
    END;



PROCEDURE BandMapClass.ResetTimes;

{ Sets all times to new DecayTime. Called when DecayTime is changed. }

VAR EntryRecord: BandMapEntryPointer;
    Band: BandType;

    BEGIN
    FOR Band := Band160 TO NoBand DO
        IF FirstEntryList [Band] <> nil THEN
            BEGIN
            EntryRecord := FirstEntryList [Band];

            WHIlE EntryRecord <> nil DO
                BEGIN
                EntryRecord^.StatusByte := EntryRecord^.StatusByte AND $C0;
                EntryRecord^.StatusByte := EntryRecord^.StatusByte OR DecayTime;
                EntryRecord := EntryRecord^.NextEntry;
                END;
            END;
    END;



PROCEDURE BandMapClass.DeleteCallFromN4OGWBandMap (Call: Callstring);

{ Deletes it from both radios - should this be in N4OGW? }

    BEGIN
    IF N4OGW_RadioOne_BandMap_IP <> '' THEN
        N4OGW_RadioOne_BandMap.DeleteCallsign (Call);

    IF N4OGW_RadioTwo_BandMap_IP <> '' THEN
        N4OGW_RadioTwo_BandMap.DeleteCallsign (Call);

    END;



PROCEDURE BandMapClass.DeleteEntry (VAR Entry: BandMapEntryPointer);

{ Deletes the record from the band map.  Entry will point to the next entry in
  the bandmap for the same band - or nil if nothing left }

VAR PreviousEntryRecord: BandMapEntryPointer;

    BEGIN
    IF Entry = nil THEN Exit;

    { If this is the first entry in the list for this band - update the pointer to
      the first entry. }

    IF Entry.PreviousEntry = nil THEN    { First entry for this band }
        BEGIN
        FirstEntryList [Entry.Band] := Entry.NextEntry;
        Dispose (Entry);
        Entry := FirstEntryList [Entry.Band];
        Exit;
        END;

    { We have a valid PreviousEntry pointer.  We need to make the NextEntry value
      for the previous entry equal to the next value for the current entry }

    PreviousEntryRecord := Entry.PreviousEntry;  { Remember }
    PreviousEntryRecord^.NextEntry := Entry^.NextEntry;

    { Dispose of the current entry and move to the next entry }

    Dispose (Entry);
    Entry := PreviousEntryRecord^.NextEntry;

    { If the next entry isn't nil - set it's previous entry record pointer }

    IF Entry <> nil THEN
        Entry.PreviousEntry := PreviousEntryRecord;
    END;



PROCEDURE BandMapClass.DeleteCall (Call: CallString);

{ This routine only gets called when the N4OGW bandmap deletes a callsign based
  upon user input.  It will delete all instances of the callsign from the
  bandmap since we don't know what band it is for }

VAR EntryRecord: BandMapEntryPointer;
    Band: BandType;

    BEGIN
    FOR Band := Band160 TO NoBand DO
        IF FirstEntryList [Band] <> nil THEN
            BEGIN
            EntryRecord := FirstEntryList [Band];

            WHIlE EntryRecord <> nil DO
                IF EntryRecord^.Call = Call THEN
                    DeleteEntry (EntryRecord)     { Points to next entry after delete }
                ELSE
                    EntryRecord := EntryRecord^.NextEntry;
                END;
            END;
    END;



PROCEDURE BandMapClass.SetRadioUpForEntry (Entry: BandMapEntryPointer; Radio: RadioType);

{ Sets up the specified radio to make a QSO with the selected entry }

    BEGIN
    CursorEntry := Entry;   { Is this necessary? }

    WITH Entry^ DO
        IF Frequency > 0 THEN
            IF QSXOffset <> 0 THEN
                BEGIN
                CASE BandMapSplitMode OF
                    ByCutoffFrequency:   { Mode determined by the bandmap entry }
                        BEGIN
                        SetRadioFreq (Radio, Frequency + QSXOffset, Mode, 'B');
                        SetRadioFreq (Radio, Frequency, Mode, 'A');
                        END;

                    AlwaysPhone:
                        BEGIN
                        SetRadioFreq (Radio, Frequency + QSXOffset, Phone, 'B');
                        SetRadioFreq (Radio, Frequency, Phone, 'A');
                        END;
                    END;  { of CASE BandMapSplitMode }

                PutRadioIntoSplit (Radio);
                END
            ELSE
                BEGIN
                SetRadioFreq (Radio, Frequency, Mode, 'A');
                PutRadioOutOfSplit (Radio);
                END;
    END;



FUNCTION BandMapClass.NextNonDupeEntryDisplayed (Band: BandType; Mode: ModeType): BOOLEAN;

{ If one found - returns TRUE with Data in CursorData record }

VAR BandMapEntryRecord: BandMapEntryPointer;
    CallSign: CallString;
    StartBand, StopBand: BandType;
    StartMode, StopMode: ModeType;
    RadioToUse: RadioType; {KK1L: 6.73}

    BEGIN
    NextNonDupeEntryDisplayed := False;

    IF CommandUseInactiveRadio THEN  {KK1L: 6.73}
        RadioToUse := InactiveRadio
    ELSE
        RadioToUse := ActiveRadio;

    IF AllBands THEN
        BEGIN
        IF VHFBandsEnabled THEN {KK1L: 6.64 Keep band map within contest limits}
            BEGIN
            StartBand := Band160;
            StopBand  := Band2;
            END
        ELSE
            BEGIN
            StartBand := Band160;
            StopBand  := Band12; {KK1L: 6.65 fixes WARC display enable}
            END;
        END
    ELSE
        BEGIN
        StartBand := Band;
        StopBand  := Band;
        END;

    IF BandMapAllModes THEN
        BEGIN
        StartMode := CW;
        StopMode  := Phone;
        END
    ELSE
        BEGIN
        StartMode := BandMapMode;
        StopMode  := BandMapMode;
        END;

    FOR Band := StartBand TO StopBand DO
        FOR Mode := StartMode TO StopMode DO
            BEGIN

            IF (NOT WARCBandsEnabled) AND
               ((Band = Band30) OR (Band = Band17) OR (Band = Band12)) THEN
                Continue; {KK1L: 6.64 Keep band map within contest limits}

            BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

            WHIlE BandMapEntryRecord <> nil DO
               BEGIN
               Callsign := BandMapExpandedString (BandMapEntryRecord^.Call);
               {KK1L: 6.73 Use LastDisplayedFreq instead of DisplayedFrequency to support control-A}
               IF BandMapEntryRecord^.Frequency > LastDisplayedFreq[RadioToUse] THEN
                   IF Abs (LastDisplayedFreq[RadioToUse] - BandMapEntryRecord^.Frequency) > BandMapGuardBand THEN
                       IF (Copy (CallSign, 1, 3) <> 'CQ/') THEN  {KK1L: 6.69 added to skip CQ frequency}
                           IF ((BandMapEntryRecord^.StatusByte AND $40) = 0) THEN
                               BEGIN
                               SetUpBandMapEntry (BandMapEntryRecord, RadioToUse); {KK1L: 6.73}
                               NextNonDupeEntryDisplayed := True;
                               Exit;
                               END;

               BandMapEntryRecord := BandMapEntryRecord^.NextEntry;
               END;
            END;


    { Got to the end of the list without finding anything.  Start at the
      beginning and find the first non dupe entry }

    FOR Band := StartBand TO StopBand DO
        FOR Mode := StartMode TO StopMode DO
            BEGIN
            BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

            WHIlE BandMapEntryRecord <> nil DO
                BEGIN
                Callsign := BandMapExpandedString (BandMapEntryRecord^.Call);
                IF (Copy (CallSign, 1, 3) <> 'CQ/') THEN  {KK1L: 6.69 added to skip CQ frequency}
                    IF ((BandMapEntryRecord^.StatusByte AND $40) = 0) THEN
                        BEGIN
                        BandMapCursorData := BandMapEntryRecord;
                        NextNonDupeEntryDisplayed := True;
                        Exit;
                        END;

                BandMapEntryRecord := BandMapEntryRecord^.NextEntry;
                END;
            END;
    END;



FUNCTION BandMapClass.NextMultiplierEntryDisplayed (Band: BandType; Mode: ModeType): BOOLEAN;

{ Data will be in BandMapCursorData record }
{KK1L: 6.68 Added to allow stepping through the displayed bandmap rather than just in}
{      current band and mode.}

VAR BandMapEntryRecord: BandMapEntryPointer;
    CallSign: CallString;
    StartBand, StopBand: BandType;
    StartMode, StopMode: ModeType;
    RadioToUse: RadioType; {KK1L: 6.73}

    BEGIN
    NextMultiplierEntryDisplayed := False;

    IF CommandUseInactiveRadio THEN  {KK1L: 6.73}
        RadioToUse := InactiveRadio
    ELSE
        RadioToUse := ActiveRadio;

    IF BandMapAllBands THEN
        BEGIN
        IF VHFBandsEnabled THEN {KK1L: 6.64 Keep band map within contest limits}
            BEGIN
            StartBand := Band160;
            StopBand  := Band2;
            END
        ELSE
            BEGIN
            StartBand := Band160;
            StopBand  := Band12; {KK1L: 6.65 fixes WARC display enable}
            END;
        END
    ELSE
        BEGIN
        StartBand := BandMapBand;
        StopBand  := BandMapBand;
        END;

    IF BandMapAllModes THEN
        BEGIN
        StartMode := CW;
        StopMode  := Phone;
        END
    ELSE
        BEGIN
        StartMode := BandMapMode;
        StopMode  := BandMapMode;
        END;

    FOR Band := StartBand TO StopBand DO
        FOR Mode := StartMode TO StopMode DO
            BEGIN

            IF (NOT WARCBandsEnabled) AND
               ((Band = Band30) OR (Band = Band17) OR (Band = Band12)) THEN
                Continue; {KK1L: 6.64 Keep band map within contest limits}

            BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

            WHIlE BandMapEntryRecord <> nil DO
               BEGIN
               Callsign := BandMapExpandedString (BandMapEntryRecord^.Call);
               {KK1L: 6.73 Use LastDisplayedFreq instead of DisplayedFrequency to support control-A}
               IF BandMapEntryRecord^.Frequency > LastDisplayedFreq[RadioToUse] THEN
                   IF Abs (LastDisplayedFreq[RadioToUse] - BandMapEntryRecord^.Frequency) > BandMapGuardBand THEN
                      IF (Copy (CallSign, 1, 3) <> 'CQ/') THEN  {KK1L: 6.69 added to skip CQ frequency}
                           IF ((BandMapEntryRecord^.StatusByte AND $80) <> 0) THEN
                               BEGIN
                               SetUpBandMapEntry (BandMapEntryRecord, RadioToUse); {KK1L: 6.73}
                               NextMultiplierEntryDisplayed := True;
                               Exit;
                               END;

               BandMapEntryRecord := BandMapEntryRecord^.NextEntry;
               END;
            END;


    { Got to the end of the list without finding anything.  Start at the
      beginning and find the first non dupe entry }

    FOR Band := StartBand TO StopBand DO
        FOR Mode := StartMode TO StopMode DO
            BEGIN
            BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

            WHIlE BandMapEntryRecord <> nil DO
                BEGIN
                Callsign := BandMapExpandedString (BandMapEntryRecord^.Call);
                IF (Copy (CallSign, 1, 3) <> 'CQ/') THEN  {KK1L: 6.69 added to skip CQ frequency}
                    IF ((BandMapEntryRecord^.StatusByte AND $80) <> 0) THEN
                        BEGIN
                        BandMapCursorData := BandMapEntryRecord;
                        NextMultiplierEntryDisplayed := True;
                        Exit;
                        END;

                BandMapEntryRecord := BandMapEntryRecord^.NextEntry;
                END;
            END;
    END;


FUNCTION BandMapClass.NextNonDupeEntry (Band: BandType; Mode: ModeType): BOOLEAN;

{ Data will be in BandMapCursorData record }
VAR BandMapEntryRecord: BandMapEntryPointer;
    CallSign: CallString;
    RadioToUse: RadioType; {KK1L: 6.73}

    BEGIN
    NextNonDupeEntry := False;

    IF CommandUseInactiveRadio THEN  {KK1L: 6.73}
        RadioToUse := InactiveRadio
    ELSE
        RadioToUse := ActiveRadio;

    IF BandMapFirstEntryList [Band, Mode] = nil THEN Exit;

    BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

    { Search through the bandmap to find the next non dupe entry. }

    WHIlE BandMapEntryRecord <> nil DO
        BEGIN
        Callsign := BandMapExpandedString (BandMapEntryRecord^.Call);
        {KK1L: 6.73 Use LastDisplayedFreq instead of DisplayedFrequency to support control-A}
        IF BandMapEntryRecord^.Frequency > LastDisplayedFreq[RadioToUse] THEN
            IF Abs (LastDisplayedFreq[RadioToUse] - BandMapEntryRecord^.Frequency) > BandMapGuardBand THEN
                IF (Copy (CallSign, 1, 3) <> 'CQ/') THEN  {KK1L: 6.69 added to skip CQ frequency}
                    IF ((BandMapEntryRecord^.StatusByte AND $40) = 0) THEN
                        BEGIN
                        SetUpBandMapEntry (BandMapEntryRecord, RadioToUse); {KK1L: 6.73}
                        NextNonDupeEntry := True;
                        Exit;
                        END;

        BandMapEntryRecord := BandMapEntryRecord^.NextEntry;
        END;

    { Got to the end of the list without finding anything.  Start at the
      beginning and find the first non dupe entry }

    BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

    WHIlE BandMapEntryRecord <> nil DO
        BEGIN
        Callsign := BandMapExpandedString (BandMapEntryRecord^.Call);
        IF (Copy (CallSign, 1, 3) <> 'CQ/') THEN  {KK1L: 6.69 added to skip CQ frequency}
            IF ((BandMapEntryRecord^.StatusByte AND $40) = 0) THEN
                BEGIN
                BandMapCursorData := BandMapEntryRecord;
                NextNonDupeEntry := True;
                Exit;
                END;

        BandMapEntryRecord := BandMapEntryRecord^.NextEntry;
        END;
    END;



FUNCTION BandMapClass.NextMultiplierEntry (Band: BandType; Mode: ModeType): BOOLEAN;

{ Data will be in BandMapCursorData record }

VAR BandMapEntryRecord: BandMapEntryPointer;
    CallSign: CallString;
    RadioToUse: RadioType; {KK1L: 6.73}

    BEGIN
    NextMultiplierEntry := False;

    IF CommandUseInactiveRadio THEN  {KK1L: 6.73}
        RadioToUse := InactiveRadio
    ELSE
        RadioToUse := ActiveRadio;

    IF BandMapFirstEntryList [Band, Mode] = nil THEN Exit;

    BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

    { Search through the bandmap to find the next non dupe entry. }

    WHIlE BandMapEntryRecord <> nil DO
        BEGIN
        Callsign := BandMapExpandedString (BandMapEntryRecord^.Call);
        {KK1L: 6.73 Use LastDisplayedFreq instead of DisplayedFrequency to support control-A}
        IF BandMapEntryRecord^.Frequency > LastDisplayedFreq[RadioToUse] THEN
            IF Abs (LastDisplayedFreq[RadioToUse] - BandMapEntryRecord^.Frequency) > BandMapGuardBand THEN
                IF (Copy (CallSign, 1, 3) <> 'CQ/') THEN  {KK1L: 6.69 added to skip CQ frequency}
                    IF ((BandMapEntryRecord^.StatusByte AND $80) <> 0) THEN
                        BEGIN
                        SetUpBandMapEntry (BandMapEntryRecord, RadioToUse); {KK1L: 6.73}
                        NextMultiplierEntry := True;
                        Exit;
                        END;

        BandMapEntryRecord := BandMapEntryRecord^.NextEntry;
        END;

    { Got to the end of the list without finding anything.  Start at the
      beginning and find the first non dupe entry }

    BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

    WHIlE EntryRecord <> nil DO
        WITH EntryRecord^ DO
            BEGIN
            IF (Copy (Call, 1, 3) <> 'CQ/') THEN   { Skip CQ entries }
                IF (StatusByte AND $80) <> 0 THEN  { Multiplier flag set }
                    BEGIN
                    BandMapCursorData := EntryRecord;
                    NextMultiplierEntry := True;
                    Exit;
                    END;

            EntryRecord := NextEntry;   { Point to the next entry in the list }
            END;
    END;



PROCEDURE BandMapClass.GetBandModeFromFrequency (Frequency: LONGINT;
                                                 VAR Band: BandType;
                                                 VAR Mode: ModeType);

{ Mode might be setup by someone else - in which case, don't mess with it }

VAR TempMode: ModeType;

    BEGIN
    CalculateBandMode (Frequency, Band, TempMode);  { Comes from TREE.PAS - could be undefined mode }

    { We are going to ignore the mode suggestion from TREE.PAS and instead
      rely on BandMapModeCutoffFrequency }

    IF Mode = NoMode THEN
        BEGIN
        IF Frequency >= BandMapModeCutoffFrequency [Band] THEN
            Mode := Phone
        ELSE
            Mode := CW;
        END;
    END;



PROCEDURE BandMapClass.AddModeCutoffFrequency (Freq: LONGINT);

VAR Band: BandType;
    TempMode: ModeType;

    BEGIN
    CalculateBandMode (Freq, Band, TempMode);

    IF Band <> NoBand THEN
        ModeCutoffFrequency [Band] := Freq;
    END;



PROCEDURE BandMapClass.RemoveCallFromLinkedList (CallsignToRemove: CallString;
                                                 Band: BandType);

{ Removes any instances of the callsign from the bandmap for the specified band }

VAR Entry, PreviousEntry: BandMapEntryPointer;
    N4OGW_Notified: BOOLEAN;  { Only does it once }

    BEGIN
    IF FirstEntryList [Band] = nil THEN Exit;   { No entries for this band }

    Entry := FirstEntryList [Band];
    PreviousEntry := nil;

    N4OGW_Notified := False;

    WHILE Entry <> nil DO
        IF CallsignToRemove = Entry^.Call THEN
            BEGIN
            IF PreviousEntry = nil THEN     { This is the first entry in the list }
                BEGIN
                FirstEntryList [Band] := Entry^.NextEntry;
                Dispose (Entry);
                Entry := FirstEntryList [Band];
                Entry^.PreviousEntry := nil;
                END
            ELSE
                BEGIN  { Previous record is valid }
                PreviousEntry^.NextEntry := Entry^.NextEntry;     { this could be nil }
                Dispose (Entry);
                Entry := PreviousEntry^.NextEntry;

                { Need to set the previous entry for this entry to the right place }

                Entry^.PreviousEntry := PreviousEntry;
                END;

            IF NOT N4OGW_Notified THEN
                BEGIN
//              Removed this because I think it did bad things 2-Dec-2023

//              IF (N4OGW_RadioOne_BandMap_Port <> 0) AND (N4OGW_RadioOne_BandMap_IP <> '') THEN
//                  BEGIN
//                  N4OGW_RadioOne_BandMap.DeleteCallsign (Call);
//                  N4OGW_RadioOne_BandMap.WriteToDebugFile ('N4OGW Radio One notified of a removal');
//                  END;

//              IF (N4OGW_RadioTwo_BandMap_Port <> 0) AND (N4OGW_RadioTwo_BandMap_IP <> '') THEN
//                  BEGIN
//                  N4OGW_RadioTwo_BandMap.DeleteCallsign (Call);
//                  N4OGW_RadioTwo_BandMap.WriteToDebugFile ('N4OGW Radio Two notified of a removal');
//                  END;

                N4OGW_Notified := True;
                END;
            END
        ELSE
            BEGIN
            PreviousEntry := Entry;
            Entry := ActiveEntry^.NextEntry;
            END;
    END;



PROCEDURE BandMapClass.AddBandMapEntry (Call: CallString;
                                        Frequency: LONGINT;
                                        QSXFrequency: LONGINT;
                                        Mode: ModeType;
                                        Dupe: BOOLEAN;
                                        Mult: BOOLEAN;
                                        MinutesLeft: BYTE);

{KK1L: 6.64 Added QSXOffset scaling to make range within INTEGER}

VAR LastBandMapEntryRecord: BandMapEntryPointer;
    TempBandMapEntryRecord, BandMapEntryRecord: BandMapEntryPointer;
    StatusByte: BYTE;
    QSXOffset: LONGINT; {KK1L: 6.64 Tried INTEGER, but no joy.}
    CompressedCall: EightBytes;
    Band: BandType;

    BEGIN
    IF NOT Enable THEN
        BEGIN
        { Send to N4OGW bandmap if it is enabled }

        IF (N4OGW_RadioOne_BandMap_Port <> 0) AND (N4OGW_RadioOne_BandMap_IP <> '') THEN
            N4OGW_RadioOne_BandMap.SendBandMapCall (Call, Frequency, Dupe, Mult);

        IF (N4OGW_RadioTwo_BandMap_Port <> 0) AND (N4OGW_RadioTwo_BandMap_IP <> '') THEN
            N4OGW_RadioTwo_BandMap.SendBandMapCall (Call, Frequency, Dupe, Mult);

        Exit;
        END;

    { The band map seemed to be crashing with SHF frequencies }

    IF Frequency > 150000000 THEN Exit;
    IF Frequency < 10 THEN Exit;

    GetBandMapBandModeFromFrequency (Frequency, Band, Mode);
    IF Band = NoBand THEN Exit;

    { We are going to remove any instances of this call in the current
      band/mode bandmap. This is new in 2022 to makes things more simple }

    RemoveCallFromBandMapLinkedList (Call, Band, Mode);

    BigCompressFormat (Call, CompressedCall);  { Remember when memory was an issue? }

    { Fix up status byte }

    StatusByte := MinutesLeft AND $3F;

    IF Dupe THEN StatusByte := StatusByte OR $40;
    IF Mult THEN StatusByte := StatusByte OR $80;

    { Compute QSX offset value - zero means none }

    IF QSXFrequency = 0 THEN
        QSXOffset := 0
    ELSE
        IF Abs (QSXFrequency - Frequency) < 327670 THEN
            {KK1L: 6.64 Scaling added to keep offset within INTEGER range.}
            {SXOffset := (QSXFrequency - Frequency) DIV 10}
            {KK1L: 6.64 Not needed. Only LONGINT seems to work as QSXOffset type}
            QSXOffset := (QSXFrequency - Frequency)
        ELSE
            QSXOffset := 0;   { Unexpected result }

    { See if the list of entries for this band mode is empty }

    IF BandMapFirstEntryList [Band, Mode] = nil THEN   { Nothing in bandmap yet }
        BEGIN
        New (BandMapFirstEntryList [Band, Mode]);
        BandMapFirstEntryList [Band, Mode]^.Call       := CompressedCall;
        BandMapFirstEntryList [Band, Mode]^.Frequency  := Frequency;
        BandMapFirstEntryList [Band, Mode]^.QSXOffset  := QSXOffset;
        BandMapFirstEntryList [Band, Mode]^.StatusByte := StatusByte;
        BandMapFirstEntryList [Band, Mode]^.NextEntry  := nil;

        { Send to N4OGW bandmaps }

        IF (N4OGW_RadioOne_BandMap_Port <> 0) AND (N4OGW_RadioOne_BandMap_IP <> '') THEN
            N4OGW_RadioOne_BandMap.SendBandMapCall (Call, Frequency, Dupe, Mult);

        IF (N4OGW_RadioTwo_BandMap_Port <> 0) AND (N4OGW_RadioTwo_BandMap_IP <> '') THEN
            N4OGW_RadioTwo_BandMap.SendBandMapCall (Call, Frequency, Dupe, Mult);

        Exit;
        END;

    { There are some entries for this band/mode.  Get setup to look
      through the list }

    LastBandMapEntryRecord := nil;
    BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

    { Search through the bandmap to find the right place for this entry.
      We keep the frequencies in order, so we need to step through the
      linked list until we find a record with a higher frequency than
      the one being added.  We squeeze this new entry in before that one,
      but check to see if the frequency is nearly the same first. }

    WHIlE BandMapEntryRecord <> nil DO
        BEGIN
        IF Abs (Frequency - BandMapEntryRecord^.Frequency) <= BandMapGuardBand THEN
            BEGIN
            { Remove old call from the N4OGW band maps and send the new one }

            IF (N4OGW_RadioOne_BandMap_Port <> 0) AND (N4OGW_RadioOne_BandMap_IP <> '') THEN
                IF BigExpandedString (BandMapEntryRecord^.Call) <> Call THEN
                    BEGIN
                    N4OGW_RadioOne_BandMap.DeleteCallsign (BandMapExpandedString (BandMapEntryRecord^.Call));
                    N4OGW_RadioOne_BandMap.SendBandMapCall (Call, Frequency, Dupe, Mult);
                    END;

            IF (N4OGW_RadioTwo_BandMap_Port <> 0) AND (N4OGW_RadioTwo_BandMap_IP <> '') THEN
                IF BigExpandedString (BandMapEntryRecord^.Call) <> Call THEN
                    BEGIN
                    N4OGW_RadioTwo_BandMap.DeleteCallsign (BandMapExpandedString (BandMapEntryRecord^.Call));
                    N4OGW_RadioTwo_BandMap.SendBandMapCall (Call, Frequency, Dupe, Mult);
                    END;

            BandMapEntryRecord^.Call       := CompressedCall;
            BandMapEntryRecord^.Frequency  := Frequency;
            BandMapEntryRecord^.QSXOffset  := QSXOffset;
            BandMapEntryRecord^.StatusByte := StatusByte;

            { We are now done - there are no instances of the same call further in the
              band map now that we removed all of them earlier.  Since we took over an
              existing band map entry - we will assume there isn't another one just
              above it that is too close }

            Exit;  { is a wonderful thing }
            END;

        IF (Frequency < BandMapEntryRecord^.Frequency) THEN  { We need to put this record in here }
            BEGIN

            { If the first entry in the list has a frequency higher than the one being added,
              we need to create a new entry and make it the first one in the linked list }

            IF LastBandMapEntryRecord = nil THEN   { Tells us we are the first record }
                BEGIN
                BandMapEntryRecord := New (BandMapEntryPointer);

                { Do the splice }

                BandMapEntryRecord^.NextEntry := BandMapFirstEntryList [Band, Mode];
                BandMapFirstEntryList [Band, Mode] := BandMapEntryRecord;

                { Save the data }

                BandMapEntryRecord^.Call       := CompressedCall;
                BandMapEntryRecord^.Frequency  := Frequency;
                BandMapEntryRecord^.QSXOffset  := QSXOffset;
                BandMapEntryRecord^.StatusByte := StatusByte;

                IF (N4OGW_RadioOne_BandMap_Port <> 0) AND (N4OGW_RadioOne_BandMap_IP <> '') THEN
                    N4OGW_RadioOne_BandMap.SendBandMapCall (Call, Frequency, Dupe, Mult);

                IF (N4OGW_RadioTwo_BandMap_Port <> 0) AND (N4OGW_RadioTwo_BandMap_IP <> '') THEN
                    N4OGW_RadioTwo_BandMap.SendBandMapCall (Call, Frequency, Dupe, Mult);

                Exit;  { is a wonderful thing }
                END;

            { We need to squeeze an new entry in here }

            IF (N4OGW_RadioOne_BandMap_Port <> 0) AND (N4OGW_RadioOne_BandMap_IP <> '') THEN
                N4OGW_RadioOne_BandMap.SendBandMapCall (Call, Frequency, Dupe, Mult);

            IF (N4OGW_RadioTwo_BandMap_Port <> 0) AND (N4OGW_RadioTwo_BandMap_IP <> '') THEN
                N4OGW_RadioTwo_BandMap.SendBandMapCall (Call, Frequency, Dupe, Mult);

            TempBandMapEntryRecord := BandMapEntryRecord;  { Remember }

            BandMapEntryRecord := New (BandMapEntryPointer);

            { Do the splice }

            LastBandMapEntryRecord^.NextEntry := BandMapEntryRecord;
            BandMapEntryRecord^.NextEntry  := TempBandMapEntryRecord;

            { Fill in the data for the new record }

            BandMapEntryRecord^.Call       := CompressedCall;
            BandMapEntryRecord^.Frequency  := Frequency;
            BandMapEntryRecord^.QSXOffset  := QSXOffset;
            BandMapEntryRecord^.StatusByte := StatusByte;
            Exit;
            END;

        { Point to the next entry in the list }

        LastBandMapEntryRecord := BandMapEntryRecord;
        BandMapEntryRecord     := BandMapEntryRecord^.NextEntry;
        END;

    { We got to the end of the list without finding the call or a place to
      add it.  Add to end of list. }

    BandMapEntryRecord := New (BandMapEntryPointer);
    LastBandMapEntryRecord^.NextEntry := BandMapEntryRecord;

    BandMapEntryRecord^.Call         := CompressedCall;
    BandMapEntryRecord^.Frequency    := Frequency;
    BandMapEntryRecord^.QSXOffset    := QSXOffset;
    BandMapEntryRecord^.StatusByte   := StatusByte;
    BandMapEntryRecord^.NextEntry    := nil;

    IF (N4OGW_RadioOne_BandMap_Port <> 0) AND (N4OGW_RadioOne_BandMap_IP <> '') THEN
       N4OGW_RadioOne_BandMap.SendBandMapCall (Call, Frequency, Dupe, Mult);

    IF (N4OGW_RadioTwo_BandMap_Port <> 0) AND (N4OGW_RadioTwo_BandMap_IP <> '') THEN
       N4OGW_RadioTwo_BandMap.SendBandMapCall (Call, Frequency, Dupe, Mult);
   END;




PROCEDURE BandMapClass.GoToProperXY (NumberEntriesDisplayed: INTEGER;
                                     NumberRows: INTEGER;
                                     NumberColumns: INTEGER);

VAR Row, Column, ColumnOffset: INTEGER;

    BEGIN
    Row := NumberEntriesDisplayed MOD NumberRows;   { Zero based row }

    ColumnOffset := 80 DIV NumberColumns;
    Column := (NumberEntriesDisplayed DIV NumberRows) * ColumnOffset;

    { Make them start at one now }

    Inc (Row);
    Inc (Column);

    GoToXY (Column, Row);
    END;



PROCEDURE BandMapClass.SaveBandMap;

{ Saves the band map data to BandMapFileName }

VAR EntryRecord: BandMapEntryPointer;
    FileWrite: FILE;
    xResult: INTEGER;
    Mode: ModeType;
    Band: BandType;
    DummyBandMapRecord: BandMapEntry;

    BEGIN
    BigCompressFormat ('XXXXXXXXXXXX', DummyBandMapRecord.Call);

    DummyBandMapRecord.StatusByte := $55;
    DummyBandMapRecord.Frequency  := 0;
    DummyBandMapRecord.NextEntry  := nil;

    Assign  (FileWrite, BandMapFileName);
    ReWrite (FileWrite, 1);

    BlockWrite (FileWrite, FileVersion, SizeOf (BandMapFileVersion), xResult);
    BlockWrite (FileWrite, DecayValue, SizeOf (BandMapDecayValue), xResult); {KK1L: 6.70 Keeps map and program in synch}

    FOR Band := Band160 TO Band2 DO
        FOR Mode := CW TO Phone DO
            IF BandMapFirstEntryList [Band, Mode] = nil THEN
                BlockWrite (FileWrite, DummyBandMapRecord, SizeOf (DummyBandMapRecord), xResult)
            ELSE
                BEGIN
                BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];  { 1st entry of linked list }

                WHIlE BandMapEntryRecord <> nil DO
                    BEGIN
                    BlockWrite (FileWrite, BandMapEntryRecord^, SizeOf (BandMapEntryRecord^), xResult);
                    BandMapEntryRecord  := BandMapEntryRecord^.NextEntry;
                    END;
                END;

    Close (FileWrite);
    END;



PROCEDURE BandMapClass.LoadBandMap;

{ Loads band map data from BandMapFileName.  Please only call when the
  program has just started and there isn't any possibility of any band
  map information already being there. }

VAR PreviousBandMapEntryRecord, BandMapEntryRecord: BandMapEntryPointer;
    TempBandMapEntryRecord: BandMapEntry;
    FileRead: FILE;
    xResult: INTEGER;
    Band: BandType;
    Mode: ModeType;
    TempChar: CHAR;

    BEGIN
    FOR Band := Band160 TO NoBand DO
        IF BandMapFirstEntryList [Band, Mode] <> nil THEN
            Exit;                          { You didn't listen! }

    IF NOT FileExists (BandMapFileName) THEN Exit;

    Assign (FileRead, BandMapFileName);
    Reset  (FileRead, 1);

    BlockRead (FileRead, TempChar, SizeOf (TempChar), xResult);

    { See if the version of the BIN file is the same as what we expect }

    IF (TempChar <> FileVersion) OR Eof (FileRead) THEN
        BEGIN
        Close (FileRead);
        Exit;
        END;

    {KK1L: 6.70 Keeps bandmap and program in synch}
    BlockRead (FileRead, BandMapDecayValue, SizeOf (BandMapDecayValue), xResult);
    BandMapDecayMultiplier  := (BandMapDecayValue div 64) + 1; {KK1L: 6.70}
    BandMapDecayTime := BandMapDecayValue div BandMapDecayMultiplier; {KK1L: 6.70}

    {KK1L: 6.64 Only place to guarantee Column zero is the first displayed column!}
    FirstDisplayedBandMapColumn := 0;
    FOR Band := Band160 TO Band2 DO
        FOR Mode := CW TO Phone DO
            IF NOT Eof (FileRead) THEN
                BEGIN
                BlockRead (FileRead, TempBandMapEntryRecord, SizeOf (TempBandMapEntryRecord), xResult);

                IF TempBandMapEntryRecord.Frequency <> 0 THEN  { not filler }
                    BEGIN
                    New (BandMapFirstEntryList [Band, Mode]);

                    BandMapFirstEntryList [Band, Mode]^ := TempBandMapEntryRecord;

                    BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

                    WHILE BandMapEntryRecord^.NextEntry <> nil DO
                        BEGIN
                        PreviousBandMapEntryRecord := BandMapEntryRecord;

                        BandMapEntryRecord := New (BandMapEntryPointer);
                        PreviousBandMapEntryRecord^.NextEntry := BandMapEntryRecord;
                        BlockRead (FileRead, BandMapEntryRecord^, SizeOf (BandMapEntryRecord^), xResult);
                        END;

                    END;
                END;

    Close (FileRead);
    END;


PROCEDURE BandMapClass.GetBandMapDisplayInfo (VAR MaxEntriesPerPage: INTEGER;
                                              VAR NumberBandMapRows: INTEGER);

    BEGIN
    IF BandMapWindowRY = 43 THEN {KK1L: 6.64 43 line screen I assume?}
        BEGIN
        IF (MultiInfoMessage = '') THEN
            BEGIN
            MaxEntriesPerPage := 85;
            NumberBandMapRows := 17;
            END
        ELSE
            BEGIN
            MaxEntriesPerPage := 65;
            NumberBandMapRows := 13;
            END
        END
    ELSE         {KK1L: 6.64 50 line screen}
        BEGIN
        IF (MultiInfoMessage = '') THEN
            BEGIN
            MaxEntriesPerPage := 120;
            NumberBandMapRows := 24;
            END
        ELSE
            BEGIN
            MaxEntriesPerPage := 100;
            NumberBandMapRows := 20;
            END;
        END;
  END;



PROCEDURE BandMapClass.SendBandMapCallsToN4OGW;

{ Used to send all of your bandmap calls to the N4OGW band map }

VAR BandMapEntryRecord: BandMapEntryPointer;
    Band: BandType;
    Mode: ModeType;

    BEGIN
    FOR Band := Band160 TO Band2 DO
        FOR Mode := CW TO Phone DO
            IF BandMapFirstEntryList [Band, Mode] <> nil THEN
                BEGIN
                BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

                WHIlE BandMapEntryRecord <> nil DO
                    BEGIN
                    IF N4OGW_RadioOne_BandMap_IP <> '' THEN
                        WITH BandMapEntryRecord^ DO
                            N4OGW_RadioOne_BandMap.SendBandMapCall (BigExpandedString (Call),
                                                                    Frequency,
                                                                    (StatusByte AND $40) = 1,
                                                                    (StatusByte AND $80) = 1);

                    IF N4OGW_RadioTwo_BandMap_IP <> '' THEN
                        WITH BandMapEntryRecord^ DO
                            N4OGW_RadioTwo_BandMap.SendBandMapCall (BigExpandedString (Call),
                                                                    Frequency,
                                                                    (StatusByte AND $40) = 1,
                                                                    (StatusByte AND $80) = 1);

                    { Move to the next entry.  }

                    BandMapEntryRecord := BandMapEntryRecord^.NextEntry;
                    END;
                END;
    END;



PROCEDURE BandMapClass.Display;

VAR StartBand, StopBand: BandType;
    StartMode, StopMode: ModeType;

    EntryRecord: BandMapEntryPointer;
    NumberEntriesDisplayed, MaxEntriesPerPage, NumberRows, FirstDisplayableCursor, CurrentCursor: INTEGER;
    LastDisplayableCursor, DummyEntryNumber: INTEGER;
    WindowCall, Call: CallString;
    FreqString: Str20;
    DoBlink: INTEGER;
    Band: BandType;
    Mode: ModeType;
    TempString: Str20;
    MinutesLeft: BYTE;
    tempy: longint;

    BEGIN
    {KK1L: 6.64 Need to add starting column adjustment to DisplayBandMap. This will allow for
           bandmap entries beyond the screen dimensions. The 'global' variable
           FirstDisplayedBandMapColumn is set to zero in LoadBandMap and adjusted as needed
           by ShowBandMapCursor. Only a screen's worth of entries will be displayed. }

    {KK1L: 6.64 I chose somewhat of a brute force approach rather than rewrite the whole proc.
           I mearly IF'd around the GoToProperXY calls when the CurrentCursor position was
           not within the displayed region of the bandmap. NumberEntriesDisplayed is a reference
           to the cursor on the screen, CurrentCursor is a reference to where in the displayable
           bandmap we are from a record perspective, FirstDisplayableBandMapCursor is directly
           referenceable to CurrentCursor. }

    BandMapBlinkingCall := '';

    IF NOT BandMapEnable THEN Exit;
    IF ScreenHeight < 40 THEN Exit;  { We only do band maps on VGA/EGA }

    SaveSetAndClearActiveWindow (BandMapWindow);

    {KK1L: 6.64 set value for use in DisplayBandMap}
    CalculateNumberVisibleBandMapEntries (NumberBandMapEntries, DummyEntryNumber, False);

    IF BandMapAllBands THEN
        BEGIN
        IF VHFBandsEnabled THEN {KK1L: 6.64 Keep band map within contest limits}
            BEGIN
            StartBand := Band160;
            StopBand  := Band2;
            END
        ELSE
            BEGIN
            StartBand := Band160;
            StopBand  := Band12; {KK1L: 6.65 fixes WARC display enable}
            END;
        END
    ELSE
        BEGIN
        StartBand := BandMapBand;
        StopBand  := BandMapBand;
        END;

    IF BandMapAllModes THEN
        BEGIN
        StartMode := CW;
        StopMode  := Phone;
        END
    ELSE
        BEGIN
        StartMode := BandMapMode;
        StopMode  := BandMapMode;
        END;

    REPEAT  {KK1L: 6.69 allows BM to line up with radio after "end" or "home"}

        GetBandMapDisplayInfo (MaxEntriesPerPage, NumberBandMapRows);
        FirstDisplayableBandMapCursor := FirstDisplayedBandMapColumn*NumberBandMapRows;
        LastDisplayableBandMapCursor := FirstDisplayedBandMapColumn*NumberBandMapRows+MaxEntriesPerPage;
        NumberEntriesDisplayed := 0;
        CurrentCursor := 0;
        FoundCursor := FALSE;
        {KK1L: 6.64 BandMapCursorFrequency is set earlier by Tree to be the radio frequency. }
        {      This code checks to see if freq is contained in range. If not and there }
        {      are entries outside the display then move the display.}
        {KK1L: 6.65 Added check for InEditBandMap to replace setting BandMapCursorFrequency := 0 }
        {           as a check for being in EditBandMap. This still keeps the display from }
        {           being controled by the radio freq and allows the cursor to be the entry }
        {           point for EditBandMap.}

        IF (BandMapCursorFrequency < FirstDisplayedBandMapFrequency) AND
           (FirstDisplayableBandMapCursor > 0) AND (BandMapCursorFrequency <> 0) AND
           (FirstDisplayedBandMapColumn > 0) AND (NOT InEditBandMap) THEN
            BEGIN
            DEC(FirstDisplayedBandMapColumn);
            FirstDisplayableBandMapCursor := FirstDisplayedBandMapColumn*NumberBandMapRows;
            LastDisplayableBandMapCursor := FirstDisplayedBandMapColumn*NumberBandMapRows+MaxEntriesPerPage;
            END
        ELSE IF (BandMapCursorFrequency > LastDisplayedBandMapFrequency) AND
                (NumberBandMapEntries > LastDisplayableBandMapCursor) AND
                (NumberBandMapEntries > MaxEntriesPerPage) AND
                (BandMapCursorFrequency <> 0) AND (NOT InEditBandMap) THEN
            BEGIN
            INC(FirstDisplayedBandMapColumn);
            FirstDisplayableBandMapCursor := FirstDisplayedBandMapColumn*NumberBandMapRows;
            LastDisplayableBandMapCursor := FirstDisplayedBandMapColumn*NumberBandMapRows+MaxEntriesPerPage;
          END;

        {KK1L: 6.69 Trying to keep from getting into an infinite loop with the REPEAT UNTIL!}
        IF (BandMapCursorFrequency < FirstDisplayedBandMapFrequency) AND
           (FirstDisplayableBandMapCursor > 0) AND (BandMapCursorFrequency <> 0) AND
           (FirstDisplayedBandMapColumn > 0) AND (NOT InEditBandMap) THEN
            NeedToSynchBandMap := True
        ELSE IF (BandMapCursorFrequency > LastDisplayedBandMapFrequency) AND
                (NumberBandMapEntries > LastDisplayableBandMapCursor) AND
                (NumberBandMapEntries > MaxEntriesPerPage) AND
                (BandMapCursorFrequency <> 0) AND (NOT InEditBandMap) THEN
            NeedToSynchBandMap := True
        ELSE
            NeedToSynchBandMap := False;

        {KK1L: 6.64 Once done comparing radio freq then reset for next time}
        FirstDisplayableBandMapFrequency := 0;
        FirstDisplayedBandMapFrequency := 0;
        LastDisplayableBandMapFrequency := 0;
        LastDisplayedBandMapFrequency := 0;

        {KK1L: 6.64 show bandmap range information to bottom line center of display}
        GoToXY (34, NumberBandMapRows+1);
        TextColor (White);
        Write (BandString [StartBand], ModeString [StartMode], ' - ');
        {KK1L: 6.67 Added if to range display so 12m is only shown when appropriate}
        IF (NOT WARCBandsEnabled) AND (StopBand = Band12) THEN
            Write (BandString [Band10], ModeString [StopMode])
        ELSE
            Write (BandString [StopBand], ModeString [StopMode]);

        {KK1L: 6.67 Added indicator for dupes display mode}
        GoToXY (17, NumberBandMapRows+1);
        TextColor (White);

        IF BandMapDupeDisplay THEN
            Write ('DUPES ON ')
        ELSE
            Write ('DUPES OFF');

        {KK1L: 6.xx Ready for mults mode}
        {GoToXY (51, NumberBandMapRows+1); }
        {TextColor (White);                }
        {IF BandMapMultsOnlyDisplay THEN   }
        {    Write ('MULTS ONLY');         }
        {ELSE                              }
        {    Write ('          ');         }

        {KK1L: 6.64 Show markers to tell if bandmap spills off either or both sides of display}
        IF FirstDisplayableBandMapCursor > 0 THEN
          BEGIN
          GoToXY (2, NumberBandMapRows+1);
          Write ('<====more');
          END
        ELSE
          BEGIN
          GoToXY (2, NumberBandMapRows+1);
          Write ('         ');
          END;
        IF NumberBandMapEntries > LastDisplayableBandMapCursor THEN
          BEGIN
          GoToXY (71, NumberBandMapRows+1);
          Write ('more====>');
          END
        ELSE
          BEGIN
          GoToXY (71, NumberBandMapRows+1);
          Write ('         ');
          END;
        TextColor (SelectedColors.BandMapWindowBackground);

        FOR Band := StartBand TO StopBand DO
            FOR Mode := StartMode TO StopMode DO
                BEGIN

                IF (NOT WARCBandsEnabled) AND
                   ((Band = Band30) OR (Band = Band17) OR (Band = Band12)) THEN
                    Continue; {KK1L: 6.64 Keep band map within contest limits}

                BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

                WHIlE BandMapEntryRecord <> nil DO
                   BEGIN
                   { If it is a dupe and we don't display dupes - skip it }

                   {KK1L: 6.65 Following IF not needed. I It actually gets in the way of both}
                   {           the CallWindowShowAllSpots option as well as not letting      }
                   {           undisplayed (dupe) spots age!                                 }
                   {IF (NOT BandMapDupeDisplay) AND                               }
                   {   ((BandMapEntryRecord^.StatusByte AND $40) <> 0) THEN       }
                   {      BEGIN                                                   }
                   {      BandMapEntryRecord := BandMapEntryRecord^.NextEntry;    }
                   {      Continue;                                               }
                   {      END;                                                    }

                   {KK1L: 6.64 Set FirstDisplayedBandMapFrequency for use on next call to DisplayBandMap}
                   {KK1L: 6.65 Changed to = NumberBandMapRows from 0 to mimic display movement of EditBM}
                   {KK1L: 6.65 Changed it back because I like it better}
                   {KK1L: 6.69 Changed to = NumberBandMapRows from 0 to mimic display movement of EditBM}

                   IF NumberEntriesDisplayed = NumberBandMapRows THEN
                     FirstDisplayedBandMapFrequency := BandMapEntryRecord^.Frequency;

                   {KK1L: 6.64 Set FirstDisplayableBandMapFrequency for use on next call to DisplayBandMap}

                   IF CurrentCursor = 0 THEN
                     FirstDisplayableBandMapFrequency := BandMapEntryRecord^.Frequency;

                   { Position write position in proper place }
                   {KK1L: 6.64 but only if in the range we want to display}
                   {KK1L: 6.65 Moved further down within existing IF...THEN. It's quicker}
                   {IF (CurrentCursor >= FirstDisplayableBandMapcursor) AND              }
                   {   (CurrentCursor <  LastDisplayableBandMapcursor) THEN              }
                   {  GoToProperXY (NumberEntriesDisplayed, NumberBandMapRows, 5);       }

                   WITH BandMapEntryRecord^ DO
                       BEGIN
                       WindowCall := BandMapExpandedString (Call);
                       BandMapCall := WindowCall;

                       {KK1L: 6.64 Sometimes calls wrap into the next field. Truncate to 7 chars for display}
                       {      CallString is 12 chars long. No IF needed since Delete handles strings}
                       {      shorter than index requested. This handles the display. There is a}
                       {      similar change in ShowBandMapCursor to allow the call to show }
                       {      as full size when the cursor passes over it.}

                       Delete(BandMapCall, 8, 12);
                       WHILE (Length(BandMapCall) < 7) DO BandMapCall := BandMapCall + ' '; {KK1L: 6.69 neatens display}

                       { Determine if this entry should be blinking (for non-CQ entries) }

                       IF (Abs (Frequency - BandMapCursorFrequency) <= BandMapGuardBand) AND
                          (Copy (BandMapCall, 1, 3) <> 'CQ/') THEN
                              BEGIN
                              IF CallWindowShowAllSpots THEN {KK1L: 6.65}
                                 BEGIN
                                 DoBlink := Blink;
                                 BandMapBlinkingCallRecord := BandMapEntryRecord;
                                 BandMapBlinkingCall := WindowCall;
                                 FoundCursor := TRUE;
                                 IF NOT InEditBandMap THEN
                                   BandMapCursorData := BandMapEntryRecord; {KK1L: 6.65 allows EditBM entry at cursor}
                                 END
                              ELSE IF ((StatusByte AND $40) <> 0) AND BandMapDupeDisplay THEN
                                 BEGIN
                                 DoBlink := Blink;
                                 BandMapBlinkingCallRecord := BandMapEntryRecord;
                                 BandMapBlinkingCall := WindowCall;
                                 FoundCursor := TRUE;
                                 IF NOT InEditBandMap THEN
                                   BandMapCursorData := BandMapEntryRecord; {KK1L: 6.65 allows EditBM entry at cursor}
                                 END
                              ELSE IF ((StatusByte AND $40) = 0) THEN
                                 BEGIN
                                 DoBlink := Blink;
                                 BandMapBlinkingCallRecord := BandMapEntryRecord;
                                 BandMapBlinkingCall := WindowCall;
                                 FoundCursor := TRUE;
                                 IF NOT InEditBandMap THEN
                                   BandMapCursorData := BandMapEntryRecord; {KK1L: 6.65 allows EditBM entry at cursor}
                                 END;
                              END
                          ELSE
                              DoBlink := 0;

                       MinutesLeft := StatusByte AND $3F;

                       IF MinutesLeft > (BandMapDecayTime * 0.96) THEN TextColor (White + DoBlink) ELSE
                           IF MinutesLeft > (BandMapDecayTime * 0.83) THEN TextColor (Yellow + DoBlink) ELSE
                               IF MinutesLeft > (BandMapDecayTime * 0.5) THEN TextColor (LightBlue + DoBlink) ELSE
                                   TextColor (Blue + DoBlink);

                       Str (Frequency, FreqString);
                       Delete (FreqString, Length (FreqString) - 1, 2);
                       Insert ('.', FreqString, Length (FreqString));

                       {KK1L: 6.64, but only if in the range we want to display}
                       {KK1L: 6.73 Added AND DisplayBandMapEnable}
                       IF (CurrentCursor >= FirstDisplayableBandMapcursor) AND
                          (CurrentCursor <  LastDisplayableBandMapcursor) AND
                          (DisplayBandMapEnable) THEN
                         BEGIN
                         GoToProperXY (NumberEntriesDisplayed, NumberBandMapRows, 5); {KK1L: 6.65 Moved here from earlier}
                         IF QSXOffset = 0 THEN
                             BEGIN
                             {KK1L: 6.64 check dupe display status before adding *}
                             {KK1L: 6.65 Moved Inc to inside because now all entries are processed}
                             IF ((StatusByte AND $40) <> 0) AND BandMapDupeDisplay THEN
                                 BEGIN
                                 Write (FreqString, ' *', BandMapCall);
                                 Inc (NumberEntriesDisplayed);
                                 END
                             ELSE
                                 IF (StatusByte AND $80) <> 0 THEN
                                     BEGIN
                                     Write (FreqString, ' m', BandMapCall);
                                     Inc (NumberEntriesDisplayed);
                                     END
                                 ELSE IF (StatusByte AND $40 = 0) THEN  {KK1L: 6.65 Added IF}
                                     BEGIN
                                     Write (FreqString, '  ', BandMapCall);
                                     Inc (NumberEntriesDisplayed);
                                     END;
                             END
                         ELSE
                             BEGIN
                             {KK1L: 6.64 check dupe display status before adding x*}
                             {KK1L: 6.65 Moved Inc to inside because now all entries are processed}
                             IF ((StatusByte AND $40) <> 0) AND BandMapDupeDisplay THEN
                                 BEGIN
                                 Write (FreqString, 'x*', BandMapCall);
                                 Inc (NumberEntriesDisplayed);
                                 END
                             ELSE
                                 IF (StatusByte AND $80) <> 0 THEN
                                     BEGIN
                                     Write (FreqString, 'xm', BandMapCall);
                                     Inc (NumberEntriesDisplayed);
                                     END
                                 ELSE IF (StatusByte AND $40 = 0) THEN  {KK1L: 6.65 Added IF}
                                     BEGIN
                                     Write (FreqString, 'x ', BandMapCall);
                                     Inc (NumberEntriesDisplayed);
                                     END;

                             END;
                         END;
                       END;

                   IF (NumberEntriesDisplayed >= MaxEntriesPerPage) THEN
                     LastDisplayableBandMapFrequency := BandMapEntryRecord^.Frequency;  {KK1L: 6.64}
                   {KK1L: 6.65 subtracted NumberBandMapRows from LastDispBMCursor to mimic display movement of EditBM}
                   {KK1L: 6.65 changed it back. I like it better.}
                   {KK1L: 6.69 subtracted NumberBandMapRows from LastDispBMCursor to mimic display movement of EditBM}
                   IF (CurrentCursor <  LastDisplayableBandMapcursor - NumberBandMapRows) THEN {KK1L: 6.65 < from <= }
                     LastDisplayedBandMapFrequency := BandMapEntryRecord^.Frequency;  {KK1L: 6.64}

                   {KK1L: 6.64, but only if in the range we want to display}
                   IF (CurrentCursor >= FirstDisplayableBandMapcursor) AND
                      (CurrentCursor <  LastDisplayableBandMapcursor) THEN
                     GoToProperXY (NumberEntriesDisplayed, NumberBandMapRows, 5);

                   {KK1L: 6.65 Added the conditional Inc since I now allow all entries (displayed or}
                   {           otherwise) to enter the loop. This was for CallWindowShowAllSpots}
                   IF ((BandMapEntryRecord^.StatusByte AND $40) <> 0) AND BandMapDupeDisplay THEN
                     Inc (CurrentCursor)
                   ELSE IF ((BandMapEntryRecord^.StatusByte AND $40) = 0) THEN
                     Inc (CurrentCursor);

                   BandMapEntryRecord  := BandMapEntryRecord^.NextEntry;
                   END;


                END;

    UNTIL ((BandMapCursorFrequency >= FirstDisplayedBandMapFrequency) AND
           (BandMapCursorFrequency <= LastDisplayedBandMapFrequency)  AND
           (NeedToSynchBandMap))
          OR (NOT NeedToSynchBandMap) OR (BandMapCursorFrequency = 0);
          {KK1L: 6.69 allows BM to line up with radio after "end" or "home"}

    NeedToSynchBandMap := False; {KK1L: 6.69 reset after it has done its job}

    { We display the MultiInfoMessage only if we have set ours to be <> null }

    IF MultiInfoMessage <> '' THEN
        BEGIN
        {GoToXY (1, Hi (WindMax) - Hi (WindMin)  - 3);}
        GoToXY (1, NumberBandMapRows + 2); {KK1L: 6.68}

        TextColor (Black);
        TextBackGround (White);

        ClrEol;   { Added in 6.76 }

        FOR Band := Band160 TO BandLight DO
            FOR Mode := CW TO Phone DO
                IF MultiStatus [Band, Mode] <> nil THEN
                    BEGIN
                    ClrEol;
                    TempString := BandString [Band] + ModeString [Mode] + ' ' + MultiStatus [Band, Mode]^;

                    Write (TempString);

                    IF WhereX > 40 THEN
                        BEGIN
                        WriteLn;
                        ClrEol;
                        END
                    ELSE
                        GoToXY (41, WhereY);
                    END;

        { Clear any remaining rows }

        WHILE WhereY < (NumberBandMapRows + 5) DO {KK1L: 6.68}
            BEGIN
            tempy := WhereY;
            GoToXY (1, WhereY + 1);
            if (tempy = WhereY) then break;//ugly patch for ugly KK1L code above
            ClrEol;
            END;

        END;

    RestorePreviousWindow;
    END;



PROCEDURE BandMapClass.ShowCursor (CursorPosition: INTEGER;
                                   NumberVisibleBandMapEntries: INTEGER;
                                   Hide: BOOLEAN);

{ Cursor position is an integer count of the position on the band map,
  starting at zero.  The BandMapCursorData will be updated to point to
  the contents of the spot located at that position. }
{KK1L: 6.64 Changed the name of NumberEntriesDisplayed to NumberVisibleBandMapEntries to}
{      better reflect its use.}

{KK1L: 6.65 NOTE THAT THIS PROC IS ONLY CALLED FROM EditBandMap. THAT PREDICATES SOME OF}
{           THE CODE HERE. JUST BE AWARE OF THAT.}

VAR FreqString: Str10;
    BandMapCall: CallString;
    MinutesLeft, OnScreenCursorPosition,
      MaxEntriesPerPage, NumberBandMapRows:                       INTEGER;

    BEGIN
    SaveAndSetActiveWindow (BandMapWindow);

    {KK1L: 6.64 Need to determine if CursorPosition given fits within currently displayed BM and}
    {      adjust accordingly. DisplayBandMap must be called to make the adjustment}
    GetBandMapDisplayInfo (MaxEntriesPerPage, NumberBandMapRows);
    {BandMapColumnsNeeded := NumberVisibleBandMapEntries MOD NumberBandMapRows;}
    {KK1L: 6.64 Calculate OnScreenCursorPosition from CursorPosition of displayable entries}
    OnScreenCursorPosition := CursorPosition - (FirstDisplayedBandMapColumn*NumberBandMapRows);
    {KK1L: 6.64 If OnScreenCursor position in the fifth(last) column and more could be displayed then}
    {      shift the display.}
    {KK1L: 6.65 Changed to WHILE...DO because keying End (from EditBandMap) would not shift more }
    {           than on column!! This is not needed for radio freq tracking (in DisplayBandMap)}
    IF (OnScreenCursorPosition > (4*NumberBandMapRows)-1) AND
       (NumberVisibleBandMapEntries > (FirstDisplayedBandMapColumn+5)*NumberBandMapRows) THEN
      WHILE (OnScreenCursorPosition > (4*NumberBandMapRows)-1) AND
         (NumberVisibleBandMapEntries > (FirstDisplayedBandMapColumn+5)*NumberBandMapRows) DO
        BEGIN
        Inc(FirstDisplayedBandMapColumn);
        OnScreenCursorPosition := CursorPosition - (FirstDisplayedBandMapColumn*NumberBandMapRows);
        DisplayBandMap;
        END
    {KK1L: 6.64 If OnScreenCursor position in the first column and there are more to the left then}
    {      shift the display.}
    {KK1L: 6.65 Changed to WHILE...DO because keying Home (from EditBandMap) would not shift more }
    {           than on column!! This is not needed for radio freq tracking (in DisplayBandMap)}
    ELSE IF (OnScreenCursorPosition < NumberBandMapRows) AND (FirstDisplayedBandMapColumn > 0) THEN
      WHILE (OnScreenCursorPosition < NumberBandMapRows) AND (FirstDisplayedBandMapColumn > 0) DO
        BEGIN
        Dec(FirstDisplayedBandMapColumn);
        OnScreenCursorPosition := CursorPosition - (FirstDisplayedBandMapColumn*NumberBandMapRows);
        DisplayBandMap;
        END;

    GoToProperXY (OnScreenCursorPosition, NumberBandMapRows, 5);

    IF NumberVisibleBandMapEntries >0 THEN {KK1L: 6.64 covers case of only dupes in bandmap}
      BEGIN
        WITH BandMapCursorData^ DO
            BEGIN
            BandMapCall := BandMapExpandedString (Call);
            {KK1L: 6.64 Sometimes calls wrap into the next field. Truncate to 7 chars for display}
            {      CallString is 12 chars long. No IF needed since Delete handles strings}
            {      shorter than index requested. This change also truncates the call when the}
            {      cursor passes over it.}
            Delete(BandMapCall, 8, 12);
            WHILE (Length(BandMapCall) < 7) DO BandMapCall := BandMapCall + ' '; {KK1L: 6.69 neatens display}
            IF Hide THEN
                BEGIN
                MinutesLeft := StatusByte AND $3F;

                IF MinutesLeft > (BandMapDecayTime * 0.96) THEN TextColor (White) ELSE
                    IF MinutesLeft > (BandMapDecayTime * 0.83) THEN TextColor (Yellow) ELSE
                        IF MinutesLeft > (BandMapDecayTime * 0.5) THEN TextColor (LightBlue) ELSE
                            TextColor (Blue);

                TextBackground (SelectedColors.BandMapWindowBackground);
                END
            ELSE
                BEGIN
                TextColor      (SelectedColors.BandMapWindowBackground);
                TextBackground (SelectedColors.BandMapWindowColor);
                END;


            Str (Frequency, FreqString);
            Delete (FreqString, Length (FreqString) - 1, 2);
            Insert ('.', FreqString, Length (FreqString));

            IF QSXOffset = 0 THEN
                BEGIN
                IF (StatusByte AND $40) <> 0 THEN
                    Write (FreqString, ' *', BandMapCall)
                ELSE
                    IF (StatusByte AND $80) <> 0 THEN
                        Write (FreqString, ' m', BandMapCall)
                    ELSE
                        Write (FreqString, '  ', BandMapCall);
                END
            ELSE
                BEGIN
                IF (StatusByte AND $40) <> 0 THEN
                    Write (FreqString, 'x*', BandMapCall)
                ELSE
                    IF (StatusByte AND $80) <> 0 THEN
                        Write (FreqString, 'xm', BandMapCall)
                    ELSE
                        Write (FreqString, 'x ', BandMapCall);

                END;
            END;
      END
      ELSE {KK1L: 6.64 case where only dupes in the bandmap}
        BEGIN
        TextColor(Red);
        Write ('Nothing to display!');
        END;

    RestorePreviousWindow;
    END;



PROCEDURE BandMapClass.UpdateBlinkingCall;

{ Resets the decay time of the blinking band map call record to the
  BandMapDecayTime value }

    BEGIN
    IF NOT Enable THEN Exit;

    IF BandMapBlinkingCallRecord <> nil THEN
        WITH BandMapBlinkingCallRecord^ DO
            BEGIN
            StatusByte := StatusByte AND $C0;
            StatusByte := (BandMapDecayTime AND $3F) OR StatusByte;
            END;

    DisplayBandMap;
    END;



PROCEDURE BandMapClass.CalculateNumberVisibleEntries (VAR NumberVisibleEntries: INTEGER;
                                                      VAR CursorEntryNumber: INTEGER);

{ Compute # of entries and which entry number the CursorData resides at. }

VAR Band, StartBand, StopBand: BandType;
    Mode, StartMode, StopMode: ModeType;
    BandMapEntryRecord: BandMapEntryPointer;

    BEGIN
    IF BandMapAllBands THEN
        BEGIN
        IF VHFBandsEnabled THEN    {KK1L: 6.64 Keep band map within contest limits}
            BEGIN
            StartBand := Band160;
            StopBand  := Band2;
            END
        ELSE
            BEGIN
            StartBand := Band160;
            StopBand  := Band12;   {KK1L: 6.65 fixes WARC display enable}
            END;
        END
    ELSE
        BEGIN
        StartBand := BandMapBand;
        StopBand  := BandMapBand;
        END;

    IF BandMapAllModes THEN
        BEGIN
        StartMode := CW;
        StopMode  := Phone;
        END
    ELSE
        BEGIN
        StartMode := BandMapMode;
        StopMode  := BandMapMode;
        END;

    NumberVisibleBandMapEntries := 0;
    CursorEntryNumber           := 0;

    FoundCursor := False;

    FOR Band := StartBand TO StopBand DO
        FOR Mode := StartMode TO StopMode DO
            BEGIN

            IF (NOT WARCBandsEnabled) AND
               ((Band = Band30) OR (Band = Band17) OR (Band = Band12)) THEN
                Continue; {KK1L: 6.64 Keep band map within contest limits}

            BandMapEntryRecord := BandMapFirstEntryList [Band, Mode];

            WHILE BandMapEntryRecord <> nil DO
                BEGIN
                IF ((BandMapEntryRecord^.StatusByte AND $40) <> 0) AND BandMapDupeDisplay THEN
                    BEGIN {entry is a dupe and we are displaying dupes }
if bandmapcursordata <> nil then
                    IF BandMapEntryRecord^.Frequency = BandMapCursorData^.Frequency THEN
                      BEGIN
                      BandMapCursorData := BandMapEntryRecord;
                      FoundCursor := True;
                      END;
                    IF NOT FoundCursor THEN Inc (CursorEntryNumber);
                    Inc (NumberVisibleBandMapEntries);
                    END
                ELSE IF ((BandMapEntryRecord^.StatusByte AND $40) = 0) THEN
                    BEGIN {KK1L: 6.64 entry is not a dupe so just check it}
if bandmapcursordata <> nil then
                    IF BandMapEntryRecord^.Frequency = BandMapCursorData^.Frequency THEN
                      BEGIN
                      BandMapCursorData := BandMapEntryRecord;
                      FoundCursor := True;
                      END;
                    IF NOT FoundCursor THEN Inc (CursorEntryNumber);
                    Inc (NumberVisibleBandMapEntries);
                    END;
                BandMapEntryRecord := BandMapEntryRecord^.NextEntry;
                END;
            END;
    IF NOT FoundCursor THEN
      {KK1L: 6.64 covers the case of the cursor on a dupe when BandMapDupeDisplay is toggled FALSE}
      {      Really it just finds the first appropriate entry to display at entry zero}
      {      It covers cases of single band, all bands, all modes, all bands and modes}
      BEGIN
      CursorEntryNumber := 0;
      GetRecordForBandMapCursor(BandMapCursorData, CursorEntryNumber);
      END;

    END;



FUNCTION BandMapClass.GetRecordForBandMapCursor (VAR Entry: BandMapEntryPointer;
                                                 CursorEntryNumber: INTEGER) : BOOLEAN;

{ Looks through the whole bandmap to get the full data entry for the
  specified cursor number.  Returns TRUE if it was found. }

VAR EntryNumber : INTEGER;
    Band, StartBand, StopBand: BandType;
    Mode, StartMode, StopMode: ModeType;

    BEGIN
    IF BandMapAllBands THEN
        BEGIN
        IF VHFBandsEnabled THEN {KK1L: 6.64 Keep band map within contest limits}
            BEGIN
            StartBand := Band160;
            StopBand  := Band2;
            END
        ELSE
            BEGIN
            StartBand := Band160;
            StopBand  := Band12;  {KK1L: 6.65 fixes WARC display enable}
            END;
        END
    ELSE
        BEGIN
        StartBand := BandMapBand;
        StopBand  := BandMapBand;
        END;

    IF BandMapAllModes THEN
        BEGIN
        StartMode := CW;
        StopMode  := Phone;
        END
    ELSE
        BEGIN
        StartMode := BandMapMode;
        StopMode  := BandMapMode;
        END;

    EntryNumber := 0;
    GetRecordForBandMapCursor := FALSE;

    FOR Band := StartBand TO StopBand DO
        BEGIN
        FOR Mode := StartMode TO StopMode DO
          BEGIN

          IF (NOT WARCBandsEnabled) AND
             ((Band = Band30) OR (Band = Band17) OR (Band = Band12)) THEN
              Continue; {KK1L: 6.64 Keep band map within contest limits}

          Entry := BandMapFirstEntryList [Band, Mode];
          WHILE Entry <> nil DO
            BEGIN
            IF (EntryNumber = CursorEntryNumber) THEN
              BEGIN {KK1L: 6.64 found a cursor match. Get Entry^ in synch then set BandMapCursorData}
              IF (NOT BandMapDupeDisplay) THEN {KK1L: 6.64 if not displaying dupes find the next non-dupe}
                   repeat
                      if entry <> nil then
                         if (entry^.statusbyte and $40) <> 0 then
                            entry := entry^.nextentry
                         else break;
                   until entry = nil;
              IF (Entry <> nil) THEN
                BEGIN {KK1L: 6.64 Only set data if not nil}
                BandMapCursorData := Entry;
                GetRecordForBandMapCursor := TRUE;
                Exit; {KK1L: 6.64 Found a match. Let's get out of here}
                END;
              Break; {KK1L: 6.64 Ran out of entries. Jump out of WHILE Entry <> nil to next Mode/Band}
              END

            ELSE {KK1L: 6.64 not a cursor match. Go to next keeping Entry^ and EntryNumber in synch with display}
              BEGIN
              IF (NOT BandMapDupeDisplay) THEN {KK1L: 6.64 if not displaying dupes find the next non-dupe}
                   repeat
                      if entry <> nil then
                         if (entry^.statusbyte and $40) <> 0 then
                            entry := entry^.nextentry
                         else break;
                   until entry = nil;
              IF Entry <> nil THEN {KK1L: 6.64 Only increment if not starting a new band/mode}
                BEGIN
                Entry := Entry^.NextEntry;
                Inc(EntryNumber);
                END;
              END;

            END;
          END;
        END;
  END;



PROCEDURE BandMapClass.EditBandMap;

{ CursorData gets left with data from active entry.  Frequency will
  be = 0 if no entry left.  }

VAR CursorEntryNumber, XPos, YPos, MaxEntriesPerPage, NumberBandMapRows: INTEGER;
    Button1, Button2: BOOLEAN;
    BandMapEntryRecord: BandMapEntryPointer;

    BEGIN
    IF NOT BandMapEnable THEN Exit;

    CalculateNumberVisibleEntries (NumberEntries, CursorEntryNumber, False);
    GetDisplayInfo (MaxEntriesPerPage, NumberRows);

    IF NumberEntries = 0 THEN Exit;

    InEditBandMap := True;
    EscapeFromEditBandMap := False;

    QuickDisplay ('Arrows=move RET=Select DEL=Delete M=modes D=dupes B=bands ESC=quit');

    REPEAT
        ShowCursor (CursorEntryNumber, NumberEntries, False);  { CursorData will have data }

        REPEAT millisleep UNTIL NewKeyPressed;

        CASE UpCase (NewReadKey) OF

            EscapeKey:
                BEGIN
                ShowBandMapCursor (CursorEntryNumber, NumberBandMapEntries, True);  { True hides cursor }
                RemoveWindow (QuickCommandWindow);
                InEditBandMap := False;
                EscapeFromEditBandMap := True;
                NeedToSynchBandMap := True;
                DisplayBandMap;
                Exit;
                END;

            CarriageReturn:
                BEGIN
                IF CursorData = nil then exit;

                IF (BandMapBand = BandMemory [InactiveRadio]) AND
                   (BandMapBand <> BandMemory [ActiveRadio]) THEN
                    SetUpBandMapEntry (BandMapCursorData, InactiveRadio)
                ELSE
                    SetUpBandMapEntry (BandMapCursorData, ActiveRadio);

                ShowBandMapCursor (CursorEntryNumber, NumberBandMapEntries, True);  { True hides cursor }
                RemoveWindow (QuickCommandWindow);
                InEditBandMap := False;
                Exit;
                END;

            'D':
                BEGIN
                BandMapDupeDisplay := NOT BandMapDupeDisplay;
                CursorEntryNumber := 0;
                GetRecordForBandMapCursor (BandMapEntryRecord, CursorEntryNumber);
                ShowBandMapCursor (CursorEntryNumber, NumberBandMapEntries, False); { False shows cursor }
                Display;
                END;

            'B':
                BEGIN
                BandMapAllBands := NOT BandMapAllBands;
                CursorEntryNumber := 0;
                GetRecordForBandMapCursor (BandMapEntryRecord, CursorEntryNumber);
                ShowBandMapCursor (CursorEntryNumber, NumberBandMapEntries, False); { False shows cursor }
                Display;
                END;

            'M':
                BEGIN
                BandMapAllModes := NOT BandMapAllModes;
                CursorEntryNumber := 0;
                GetRecordForBandMapCursor (BandMapEntryRecord, CursorEntryNumber);
                ShowBandMapCursor (CursorEntryNumber, NumberBandMapEntries, False); { False shows cursor }
                Display;
                END;

            'G':
                BEGIN
                BandMapMultsOnly := NOT BandMapMultsOnly;
                CursorEntryNumber := 0;
                GetRecordForBandMapCursor (BandMapEntryRecord, CursorEntryNumber);
                ShowBandMapCursor (CursorEntryNumber, NumberBandMapEntries, False); { False shows cursor }
                Display;
                END;

            NullKey:
                BEGIN
                ShowCursor (CursorEntryNumber, NumberBandMapEntries, True);

                CASE NewReadKey OF
                    DeleteKey:
                        IF BandMapCursorData <> nil then
                            IF BandMapCursorData^.Frequency > 0 THEN
                                BEGIN
                                DeleteEntry (BandMapCursorData);
                                Display;
                                END;
                    DownArrow:
                        IF (CursorEntryNumber < NumberBandMapEntries - 1) THEN
                             Inc (CursorEntryNumber);
                    UpArrow:
                        IF CursorEntryNumber > 0 THEN
                             Dec (CursorEntryNumber);
                    RightArrow:
                            IF CursorEntryNumber + NumberBandMapRows < NumberBandMapEntries THEN
                                CursorEntryNumber := CursorEntryNumber + NumberBandMapRows;
                    HomeKey:
                        CursorEntryNumber := 0;

                    LeftArrow:
                            IF CursorEntryNumber > (NumberBandMapRows-1) THEN
                                CursorEntryNumber := CursorEntryNumber - NumberBandMapRows;

                    EndKey:
                        CursorEntryNumber := NumberBandMapEntries - 1;

                    END;  { of CASE ExtendedKey }

                GetRecordForBandMapCursor (BandMapEntryRecord, CursorEntryNumber);
                ShowBandMapCursor (CursorEntryNumber, NumberBandMapEntries, False); { False shows cursor }
                END;

            END;  { of case NewReadKey }

    UNTIL False;  { Only way out is an ESCAPE or RETURN }
    END;



PROCEDURE BandMapClass.Create;

{ Will automatically allocate space for the BandMap class on the heap }

VAR Band: BandType;

    BEGIN
    FOR Band := Band160 TO NoBand DO FirstEntryList [Band] := nil;
    END;



    BEGIN
    BandMap := BandMapClass.Create;
    END.
