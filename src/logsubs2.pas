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

{$V-}
PROCEDURE CreateAndSendPacketSpot (PacketSpotCall: CallString;
                                   PacketSpotFreq: LONGINT);

VAR TempStr1, TempStr2, TempString: Str80;

    BEGIN
    TempStr1 := PrecedingString  (PacketSpotCall, '/');
    TempStr2 := PostcedingString (PacketSpotCall, '/');

    IF (TempStr1 = 'CQ') AND StringIsAllNumbers (TempStr2) THEN
        EXIT;

    IF PacketSpotPrefixOnly THEN
        PacketSpotCall := GetPrefix (PacketSpotCall)
    ELSE
        IF NOT GoodCallSyntax (PacketSpotCall) THEN
            Exit;

    IF PacketSpotFreq = 0 THEN Exit;

    Str (PacketSpotFreq, TempString);

    Delete (TempString, Length (TempString) - 1, 2);
    Insert ('.', TempString, Length (TempString));

    TempString := 'DX ' + TempString + ' ' + PacketSpotCall + ' ' + PacketSpotComment;

    IF PacketSpotEditEnable THEN
        BEGIN
        TempString := TempString + ' ';
        SaveSetAndClearActiveWindow (QuickCommandWindow);
        TempString := LineInput ('Spot = ', TempString, False, False);
        RestorePreviousWindow;
        END;

    IF (TempString <> '') AND (TempString <> EscapeKey) THEN
        BEGIN
        IF ActivePacketPort <> nil THEN
            BEGIN
            SendPacketMessage (TempString + CarriageReturn);
            QuickDisplay ('Sent to packet : ' + TempString);
            END
        ELSE
            BEGIN
            IF K1EANetworkEnable THEN
                SendMultiMessage ('B' + K1EAStationID + ' ' + TempString)
            ELSE
                SendMultiCommand (MultiBandAddressArray [ActiveBand],
                                  $FF, MultiPacketMessageToSend, TempString + CarriageReturn);

            QuickDisplay ('Sent to packet via network : ' + TempString);
            END;
        END;
    END;

PROCEDURE SetUpAltDDupeCheck (Call: CallString; AddToBandMap: BOOLEAN);

{ This procedure will setup the AltD DupeInfo window for the callsign passed
  to it.  It will use the inactive radio's band/mode for dupe determination.

  It will also set AltDDupeCheckCall to the callsign being checked and
  TwoRadioState to CallReady if it isn't a dupe }

VAR SpeedString, MultString: Str20;
    Band: BandType;
    Mode: ModeType;
    Frequency: LONGINT;
    Mult: BOOLEAN;

    BEGIN
    IF Call = '' THEN
        BEGIN
        AltDDupeCheckCall := '';
        RemoveWindow (DupeInfoWindow);
        Exit;
        END;

    SaveSetAndClearActiveWindow (DupeInfoWindow);

    Str (SpeedMemory [InactiveRadio], SpeedString);

    IF VisibleLog.CallIsADupe (Call,
                               BandMemory [InactiveRadio],
                               ModeMemory [InactiveRadio]) THEN

       BEGIN
       WriteLn (Call + ' DUPE!! on ' + BandString [BandMemory [InactiveRadio]] + ModeString [ModeMemory [InactiveRadio]]);
       Write (InitialExchangeEntry (BandMapBlinkingCall));;

       IF (DupeCheckSound <> DupeCheckNoSound) AND AddToBandMap THEN
           Tone.DoABeep (ThreeHarmonics);

       AltDDupeCheckDisplayedCall := Call;

       { Show some status for the call }

       DisplayGridSquareStatus (Call);
       VisibleLog.ShowQSOStatus (Call);
       VisibleLog.ShowMultiplierStatus (Call);

       { Add this to the band map }

       IF BandMapEnable AND AddToBandMap THEN
           IF GetRadioParameters (InactiveRadio, '', Frequency, Band, Mode, FALSE, False) THEN
               BEGIN
               BandMapCursorFrequency := Frequency;
               NewBandMapEntry (Call, Frequency, 0, Mode, True, False, BandMapDecayTime, True);
               END
           ELSE
               IF AskForFrequencies THEN
                   BEGIN
                   Frequency := QuickEditFreq ('Enter frequency for ' + Call + ' in kHz : ', 10);

                   IF Frequency <> -1 THEN
                       BEGIN
                       NewBandMapEntry (Call, Frequency, 0, ModeMemory [InactiveRadio],
                                        True, False, BandMapDecayTime, True);

                       BandMapCursorFrequency := Frequency; {KK1L: 6.68 band map will track manual entry}

                       JustLoadingBandMapWithNoRadio := True; {KK1L: 6.68}
                       END;
                   END;

       RestorePreviousWindow;

       IF SendAltDSpotsToPacket AND (Frequency <> -1) THEN
           CreateAndSendPacketSpot (Call, Frequency);

       Exit;
       END;

    { This guy is not a dupe - go ahead and setup to work him }

    AltDBand := BandMemory [InactiveRadio];
    AltDMode := ModeMemory [Inactiveradio];

    IF ModeMemory [InactiveRadio] = CW THEN
        WriteLn (Call + ' OK!! at ' + SpeedString + ' WPM')
    ELSE
        WriteLn (Call + ' OK!!');

    Write ('Space bar for ', BandString [BandMemory [InactiveRadio]],
            ModeString [ModeMemory [InactiveRadio]], ' QSO');

    AltDDupeCheckDisplayedCall := Call;

    DisplayGridSquareStatus (Call);
    VisibleLog.ShowQSOStatus (Call);
    VisibleLog.ShowMultiplierStatus (Call);

    IF TwoRadioState <> TwoRadiosDisabled THEN
        BEGIN
        TwoRadioState := Idle;
        CheckTwoRadioState (CallPutUp);
        AltDDupeCheckCall := Call;
        END;

    IF BandMapEnable AND AddToBandMap THEN
        IF GetRadioParameters (InactiveRadio, '', Frequency, Band, Mode, FALSE, False) THEN
            BEGIN
            BandMapCursorFrequency := Frequency;

            VisibleLog.DetermineIfNewMult (Call,
                                           BandMemory [InactiveRadio],
                                           ModeMemory [InactiveRadio],
                                           MultString);

            Mult := MultString <> '';

            NewBandMapEntry (Call, Frequency, 0, Mode, False, Mult, BandMapDecayTime, True);
            DisplayBandMap;
            END
        ELSE
            IF AskForFrequencies THEN
                BEGIN
                Frequency := QuickEditFreq ('Enter frequency for ' + CallWindowString + ' in kHz : ', 10);

                IF Frequency <> -1 THEN
                    BEGIN
                    VisibleLog.DetermineIfNewMult (Call, BandMemory [InactiveRadio],
                                                   ModeMemory [InactiveRadio], MultString);

                    Mult := MultString <> '';

                    {KK1L: 6.73 changed CheckMode to ModeMemory [InactiveRadio]}

                    NewBandMapEntry (Call, Frequency, 0, ModeMemory [InactiveRadio], False, Mult,
                                     BandMapDecayTime, True);


                    BandMapCursorFrequency := Frequency; {KK1L: 6.68 band map will track manual entry}

                    JustLoadingBandMapWithNoRadio := True; {KK1L: 6.68}
                    END;
                END;

    IF DupeCheckSound = DupeCheckGratsIfMult THEN
        BEGIN
        VisibleLog.DetermineIfNewMult (Call, BandMemory [InactiveRadio], ModeMemory [InactiveRadio], TempString);
        IF TempString <> '' THEN Tone.DoABeep (BeepCongrats);
        END;

    IF SendAltDSpotsToPacket AND (Frequency <> -1) THEN
        CreateAndSendPacketSpot (Call, Frequency);

    RestorePreviousWindow;
    END;


PROCEDURE PutContactIntoLogFile (LogString: Str80);

VAR Time, QSONumber, Result: INTEGER;
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
               Val (Exchange, QSONumber, Result);
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



PROCEDURE PushLogStringIntoEditableLogAndLogPopedQSO (LogString: Str80;
                                                      MyQSO: BOOLEAN);

VAR RData: ContestExchange;
    TempString: STRING;

    BEGIN
    IF (ActiveMultiPort <> nil) AND SendQSOImmediately AND MyQSO THEN
        BEGIN
        GetRidOfPostcedingSpaces (LogString);

        IF ((LogString <> '') AND NOT MultiMultsOnly) OR
           (GetLogEntryMultString (LogString) <> '') THEN
               IF K1EANetworkEnable THEN
                   BEGIN

                   { Don't send notes }

                   IF Copy (LogString, 1, 1) <> ';' THEN
                       BEGIN
                       TempString := ConvertN6TRLogStringToK1EANetworkFormat (LogString);
                       SendMultiMessage (TempString);
                       END;
                   END
               ELSE
                   BEGIN
                   SendMultiCommand (MultiBandAddressArray [ActiveBand],
                                     $FF, MultiQSOData, LogString);
                   END;
        END;

    LogString := VisibleLog.PushLogEntry (LogString);

    { LogString is now what popped off the top of the editable window }

    GetRidOfPostcedingSpaces (LogString);

    IF LogString <> '' THEN
        BEGIN
        PutContactIntoLogFile (LogString);

        IF ParseExchangeIntoContestExchange (LogString, RData) THEN
            BEGIN
            ProcessPartialCallAndInitialExchange (RData);

            IF (ActiveMultiPort <> nil) AND (NOT SendQSOImmediately) THEN
                BEGIN
                IF (NOT MultiMultsOnly) OR
                   (GetLogEntryMultString (LogString) <> '') THEN
                    IF K1EANetworkEnable THEN
                        BEGIN

                        { Don't send notes }

                        IF Copy (LogString, 1, 1) <> ';' THEN
                            BEGIN
                            TempString := ConvertN6TRLogStringToK1EANetworkFormat (LogString);
                            SendMultiMessage (TempString);
                            END;
                        END
                    ELSE
                        BEGIN
                        SendMultiCommand (MultiBandAddressArray [ActiveBand],
                                          $FF, MultiQSOData, LogString);
                        END;
            END


        ELSE  { QSO doesn't make sense - probably a note }

            IF (ActiveMultiPort <> nil) AND (NOT SendQSOImmediately) THEN
                IF K1EANetworkEnable THEN
                    BEGIN

                    { I don't know how to do this on the K1EA network }

                    END
                ELSE
                    BEGIN
                    SendMultiCommand (MultiBandAddressArray [ActiveBand],
                                      $FF, MultiQSOData, LogString);
                    END;
            END;
        END;
    END;



PROCEDURE ProcessCallWindowCommand (Command: Str80);

{ This procedure gets called if a command in the call window was typed in
  starting with a \ character. }

VAR Freq: LONGINT;
    TempString: Str80;

    BEGIN
    Delete (Command, 1, 1);

    IF (Command = 'PASSFREQ') AND K1EANetworkEnable THEN
        BEGIN
        Freq := QuickEditFreq ('Enter pass freq in kHz : ', 10);
        Str (Freq, TempString);

        { Make sure my display is updated }

        UpdateK1EAStationInfo (Pass, K1EAStationID, TempString);

        { Send to K1EA network }

        SendMultiMessage ('G' + K1EAStationId + ' ' + TempString);
        END;

{   Disabled in 6.46

    IF TakingABreak AND ((Command = 'TIMEON') OR (Command = 'ON')) THEN
        BEGIN
        ThisBreak := ElaspedMinutes (OffTimeStart);

        TotalOffTime := TotalOffTime + ThisBreak;
        TakingABreak := False;

        SaveSetAndClearActiveWindow (QuickCommandWindow);
        Write ('Total off time is now = ', MinutesToTimeString (TotalOffTime), ' minutes');
        ReminderPostedCount := 60;
        RestorePreviousWindow;

        TempString := '; Break finished at ' + GetFullTimeString +
                      '  This break = ' + MinutesToTimeString (ThisBreak) +
                      '  Total off time = ' + MinutesToTimeString (TotalOffTime);

        PushLogStringIntoEditableLogAndLogPopedQSO (TempString, True);
        END;

    IF (NOT TakingABreak) AND ((Command = 'TIMEOFF') OR (Command = 'OFF')) THEN
        BEGIN
        MarkTime (OffTimeStart);
        TakingABreak := True;
        LastDisplayedBreakTime := -1;
        TempString := '; Break started at ' + GetFullTimeString;
        PushLogStringIntoEditableLogAndLogPopedQSO (TempString, True);
        END;
}
    END;



PROCEDURE MoveEditableLogIntoLogFile;

VAR QSOCount: INTEGER;
    TempString: Str80;

    BEGIN
    FOR QSOCount := 1 TO NumberEditableLines DO
        BEGIN
        TempString := '';
        PushLogStringIntoEditableLogAndLogPopedQSO (TempString, True);
        END;

    DeleteFile (LogTempFileName);
    IF UpdateRestartFileEnable THEN Sheet.SaveRestartFile;
    END;



PROCEDURE CheckMultiState;

VAR MultiString, MessageString: STRING;
    ModeString, MultString, Call: CallString;
    Band: BandType;
    Mode: ModeType;
    Points: INTEGER;
    Freq, QSX: LONGINT;
    ControlByte: BYTE;
    RXData: ContestExchange;
    Year, Month, Day, DayOfWeek, Hour, Minute, Second, Sec100: WORD;
    NewYear, NewMonth, NewDay, NewHour, NewMinute, NewSecond: WORD;
    Dupe, Mult, FirstCommand, NewMult: BOOLEAN;
    FileWrite: TEXT;
    SplitByte: BYTE;

    BEGIN
    IF NOT K1EANetworkEnable THEN CheckForLostMultiMessages;

    MultiString := GetMultiPortCommand;

    IF MultiString = '' THEN Exit;

    IF K1EANetworkEnable THEN
        BEGIN

        { Message string to not have message type, source or checksum }

        MessageString := MultiString;
        RemoveFirstString (MessageString);  { Delete message type & source }
        Delete (MessageString, Length (MessageString), 1); { Delete checksum }

        CASE MultiString [1] OF

            'B': Packet.ProcessPacketMessageFromNetWork (MessageString);

            'C': { Band map message }
                BEGIN

{ C1 599 Freq QSX UnixTime 0 band mode call * 0 1 0 0 }

                RemoveFirstString (MultiString);  { C1 }
                RemoveFirstString (MultiString);  { 599 }

                Freq := RemoveFirstLongInteger (MultiString);
                QSX  := RemoveFirstLongInteger (MultiString);

                RemoveFirstString (MultiString);  { UnixTime }
                SplitByte := RemoveFirstLongInteger (MultiString);  { 0 or 1 }
                RemoveFirstString (MultiString);  { band }

                SplitByte := SplitByte AND $01;

                IF SplitByte = 0 THEN QSX := 0;

                ModeString := RemoveFirstString (MultiString);  { mode }

                Call := RemoveFirstString (MultiString);

                { Set mode to a safe value in case it can't be calculated }

                IF ModeString = '1' THEN
                    Mode := CW
                ELSE
                    Mode := Phone;

                CalculateBandMode (Freq, Band, Mode);

                IF (Band <> NoBand) AND (Mode <> NoMode) THEN { Added in 6.25 }
                    BEGIN
                    Dupe := VisibleLog.CallIsADupe (Call, Band, ActiveMode);

                    { These didn't have the NOT in them until 6.36 }

                    IF NOT MultByBand THEN Band := All;
                    IF NOT MultByMode THEN Mode := Both;

                    { Runtime 201 here when hitting F1s - probably on 160.
                      Initialized Mode to Active mode before calling
                      CalculateBandMode }

                    VisibleLog.DetermineIfNewMult (Call, Band, Mode, MultString);

                    Mult := MultString <> '';

                    { SendToMulti = False }

                    NewBandMapEntry (Call, Freq, QSX, Mode, Dupe, Mult, BandMapDecayTime, False)
                    END;

                END;

            'G': BEGIN  { Pass frequency information }
                 UpdateK1EAStationInfo (Pass, MultiString [2], MessageString);
                 END;

            'L', 'U':
                BEGIN  { Log QSO }
                MultiString := ConvertK1EANetworkLogMessageToN6TRLogString (MultiString);
                ParseExchangeIntoContestExchange (MultiString, RXData);

                ProcessPartialCallAndInitialExchange (RXData);

                { These next steps are unique for K1EA network entries that
                  have no QSO point information, multiplier information or
                  even the sent QSO Number when they come in. }

                LocateCall (RXData.Callsign, RXData.QTH, True);
                IF DoingDXMults THEN GetDXQTH (RXData);
                CalculateQSOPoints (RXData);
                VisibleLog.ProcessMultipliers (RXData);
                RXData.NumberSent := TotalContacts + 1;

                { Need to convert RXData back to a N6TR Log Entry string }

                MessageString := MakeLogString (RXData);

                { Now you can do everything that is normally done with a
                  TR Log entry }

                Call   := GetLogEntryCall      (MessageString);
                Band   := GetLogEntryBand      (MessageString);
                Mode   := GetLogEntryMode      (MessageString);
                Points := GetLogEntryQSOPoints (MessageString);

                CheckBand (Band);

                NewMult := GetLogEntryMultString (MessageString) <> '';

                ParseExchangeIntoContestExchange (MessageString, RXData);
                ProcessPartialCallAndInitialExchange (RXData);

                IF SendQSOImmediately THEN
                    PushLogStringIntoEditableLogAndLogPopedQSO (MessageString, False)
                ELSE
                    PutContactIntoLogFile (MessageString);

                Inc (NumberContactsThisMinute);
                NumberQSOPointsThisMinute := NumberQSOPointsThisMinute + Points;

                IF ActiveWindow <> DupeSheetWindow THEN { no packet }
                    BEGIN
                    DisplayTotalScore (TotalScore);
                    DisplayNamePercentage (TotalNamesSent + VisibleLog.NumberNamesSentInEditableLog, TotalContacts);
                    UpdateTotals;
                    CheckAvailableMemory;
                    END;

                DisplayTotalScore (TotalScore);
                DisplayInsertMode (InsertMode);
                DisplayNextQSONumber (TotalContacts + 1);

                IF FloppyFileSaveFrequency > 0 THEN
                    IF QSOTotals [All, Both] > 0 THEN
                        IF QSOTotals [All, Both] MOD FloppyFileSaveFrequency = 0 THEN
                            SaveLogFileToFloppy;

                IF UpdateRestartFileEnable THEN Sheet.SaveRestartFile;

                IF MultiUpdateMultDisplay AND NewMult THEN
                    VisibleLog.ShowRemainingMultipliers;

                IF BandMapEnable THEN {KK1L: 6.69 should get BM matching new data}
                    BEGIN
                    UpdateBandMapMultiplierStatus;
                    UpdateBandMapDupeStatus(RXData.Callsign, RXData.Band, RXData.Mode, True);
                    END;

                END;

            'M': BEGIN  { Run frequency information }
                 UpdateK1EAStationInfo (Run, MultiString [2], MessageString);
                 END;

            'T', 'P':   { Talk or pass message }
                 BEGIN
                 QuickDisplay (MessageString);
                 Tone.DoABeep (BeepCongrats);
                 ReminderPostedCount := 60;

                 PushMultiMessageBuffer (MessageString);

                 IF IntercomFileOpen THEN
                     WriteLn (IntercomFileWrite, GetTimeString, ' ', MessageString);

                 END;

            'Y': BEGIN  { DOS time sync message }
                 NewHour   := RemoveFirstLongInteger (MessageString);
                 NewMinute := RemoveFirstLongInteger (MessageString);
                 NewSecond := RemoveFirstLongInteger (MessageString);
                 NewDay    := RemoveFirstLongInteger (MessageString);
                 NewMonth  := RemoveFirstLongInteger (MessageString);
                 NewYear   := RemoveFirstLongInteger (MessageString);

                 GetDate (Year, Month, Day, DayOfWeek);

                 IF (Year <> NewYear) OR (Month <> NewMonth) OR (Day <> NewDay) THEN
                     SetDate (NewYear, NewMonth, NewDay);

                 GetTime (Hour, Minute, Second, Sec100);

                 IF (Hour <> NewHour) OR (Minute <> NewMinute) OR (Abs (Second - NewSecond) > 3) THEN
                     SetTime (NewHour, NewMinute, NewSecond, 0);
                 END;


            END;  { of CASE MultiString [1] }

        END

    ELSE   { N6TR Network Mode }
        BEGIN
        MessageString [0] := MultiString [8];
        Move (MultiString [10], MessageString [1], Ord (MultiString [8]));

        ControlByte := Ord (MultiString [3]);

        CASE ControlByte OF
            MultiInformationMessage:
                BEGIN
                Band := RemoveBand (MessageString);
                Mode := RemoveMode (MessageString);
                if ((Mode < Low(ModeType)) or (Mode > High(ModeType))) then
                   Mode := NoMode;

                IF MultiStatus [Band, Mode] = nil THEN New (MultiStatus [Band, Mode]);
                MultiStatus [Band, Mode]^ := MessageString;
                END;

            MultiTimeMessage:
                BEGIN
                Year   := RemoveFirstLongInteger (MessageString);
                Month  := RemoveFirstLongInteger (MessageString);
                Day    := RemoveFirstLongInteger (MessageString);
                Hour   := RemoveFirstLongInteger (MessageString);
                Minute := RemoveFirstLongInteger (MessageString);
                Second := RemoveFirstLongInteger (MessageString);

                SetTime (Hour, Minute, Second, 0);
                SetDate (Year, Month, Day);
                END;

            MultiBandMapMessage:
                BEGIN
                Call := RemoveFirstString (MessageString);
                Freq := RemoveFirstLongInteger (MessageString);
                QSX  := RemoveFirstLongInteger (MessageString);

                Mode := ActiveMode;

                CalculateBandMode (Freq, Band, Mode);

                IF (Band <> NoBand) AND (Mode <> NoMode) THEN { Added in 6.25 }
                    BEGIN
                    Dupe := VisibleLog.CallIsADupe (Call, Band, ActiveMode);

                    { These didn't have the NOT in them until 6.36 }

                    IF NOT MultByBand THEN Band := All;
                    IF NOT MultByMode THEN Mode := Both;

                    { Runtime 201 here when hitting F1s - probably on 160.
                      Initialized Mode to Active mode before calling
                      CalculateBandMode }

                    VisibleLog.DetermineIfNewMult (Call, Band, Mode, MultString);

                    Mult := MultString <> '';
                                                                    { SendToMulti = False }
                    NewBandMapEntry (Call, Freq, QSX, Mode, Dupe, Mult, BandMapDecayTime, False)
                    END;
                END;

            MultiTalkMessage:
                BEGIN
                MessageString := BandString [MultiMessageSourceBand (Ord (MultiString [1]))] + ': ' + MessageString;
                QuickDisplay (MessageString);
                Tone.DoABeep (BeepCongrats);
                ReminderPostedCount := 60;

                PushMultiMessageBuffer (MessageString);

                IF IntercomFileOpen THEN
                    WriteLn (IntercomFileWrite, GetTimeString, ' ', MessageString);
                END;

            MultiPacketReceivedMessage:
                Packet.ProcessPacketMessageFromNetWork (MessageString);

            MultiPacketMessageToSend:
                IF ActivePacketPort <> nil THEN SendPacketMessage (MessageString);

            MultiQSOData:
                BEGIN
                Call   := GetLogEntryCall      (MessageString);
                Band   := GetLogEntryBand      (MessageString);
                Mode   := GetLogEntryMode      (MessageString);
                Points := GetLogEntryQSOPoints (MessageString);

                CheckBand (Band);

                NewMult := GetLogEntryMultString (MessageString) <> '';

                ParseExchangeIntoContestExchange (MessageString, RXData);
                ProcessPartialCallAndInitialExchange (RXData);

                IF SendQSOImmediately THEN
                    PushLogStringIntoEditableLogAndLogPopedQSO (MessageString, False)
                ELSE
                    PutContactIntoLogFile (MessageString);

                Inc (NumberContactsThisMinute);
                NumberQSOPointsThisMinute := NumberQSOPointsThisMinute + Points;

                IF ActiveWindow <> DupeSheetWindow THEN { no packet }
                    BEGIN
                    DisplayTotalScore (TotalScore);
                    DisplayNamePercentage (TotalNamesSent + VisibleLog.NumberNamesSentInEditableLog, TotalContacts);
                    UpdateTotals;
                    CheckAvailableMemory;
                    END;

                DisplayTotalScore (TotalScore);
                DisplayInsertMode (InsertMode);
                DisplayNextQSONumber (TotalContacts + 1);

                IF FloppyFileSaveFrequency > 0 THEN
                    IF QSOTotals [All, Both] > 0 THEN
                        IF QSOTotals [All, Both] MOD FloppyFileSaveFrequency = 0 THEN
                            SaveLogFileToFloppy;

                IF UpdateRestartFileEnable THEN Sheet.SaveRestartFile;

                IF MultiUpdateMultDisplay AND NewMult THEN
                    VisibleLog.ShowRemainingMultipliers;

                IF BandMapEnable THEN {KK1L: 6.69 should get BM matching new data}
                    BEGIN
                    UpdateBandMapMultiplierStatus;
                    UpdateBandMapDupeStatus(RXData.Callsign, RXData.Band, RXData.Mode, True);
                    END;

                END;

            MultiConfigurationMessage:
                BEGIN
                FirstCommand := False;
                ProcessConfigInstruction (MessageString, FirstCommand);

                IF OpenFileForAppend (FileWrite, LogConfigFileName) THEN
                    BEGIN
                    WriteLn (FileWrite, MessageString);
                    Close (FileWrite);
                    END;
                END;

            END;   { of case }
        END;
    END;



FUNCTION WindowDupeCheck: BOOLEAN;

{ Returns TRUE if the call window has a dupe in it. }

VAR Frequency: LONGINT;
    Band:      BandType;
    Mode:      ModeType;
    RememberTime: TimeRecord;
    MultString, TempString: Str40;
    Mult: BOOLEAN;

    BEGIN
    WindowDupeCheck := False;

    IF Length (CallWindowString) < 2 THEN Exit;
    WindowDupeCheckCall := CallWindowString;

    BandMapBand := ActiveBand;
    BandMapMode := ActiveMode; {KK1L: 6.69 should be here too for no radio connected.}

    IF VisibleLog.CallIsADupe (CallWindowString, ActiveBand, ActiveMode) THEN
        BEGIN
        IF ActiveWindow = ExchangeWindow THEN RestorePreviousWindow;
        WindowDupeCheck := True;
        GoToXY (Length (CallWindowString) + 1, WhereY);
        Write (' DUPE');

        IF DupeCheckSound <> DupeCheckNoSound THEN Tone.DoABeep (ThreeHarmonics);

        MarkTime (RememberTime);

        IF ReminderPostedCount = 0 THEN
            QuickDisplay (CallWindowString + ' was a dupe.');

        IF KeyRecentlyPressed (F1, 200) THEN  { Withing two seconds }
            FlushCWBufferAndClearPTT;

        ShowStationInformation (CallWindowString);

        DisplayGridSquareStatus (CallWindowString);

        IF BandMapEnable AND (OpMode = SearchAndPounceOpMode) THEN
            IF GetRadioParameters (ActiveRadio, '', Frequency, Band, Mode, FALSE, False) THEN
                BEGIN
                BandMapCursorFrequency := Frequency;

                VisibleLog.DetermineIfNewMult (CallWindowString, ActiveBand, ActiveMode, MultString);

                Mult := MultString <> '';

                { Send to Multi = TRUE }

                NewBandMapEntry (CallWindowString, Frequency, 0, ActiveMode, True, Mult, BandMapDecayTime, True);
                END
            ELSE
                IF AskForFrequencies THEN
                    BEGIN
                    Frequency := QuickEditFreq ('Enter frequency for ' + CallWindowString + ' in kHz : ', 10);

                    IF Frequency <> -1 THEN
                        BEGIN
                        VisibleLog.DetermineIfNewMult (CallWindowString, ActiveBand, ActiveMode, MultString);

                        Mult := MultString <> '';

                        { Send to multi = TRUE }

                        NewBandMapEntry (CallWindowString, Frequency, 0, ActiveMode, True, Mult, BandMapDecayTime, True);
                        BandMapCursorFrequency := Frequency; {KK1L: 6.68 band map will track manual entry}
                        JustLoadingBandMapWithNoRadio := True; {KK1L: 6.68}
                        END;
                    END;

        VisibleLog.DoPossibleCalls (CallWindowString);
        ExchangeWindowString := '';
        ClearWindow (ExchangeWindow);

        REPEAT
            IF ActiveMultiPort <> nil THEN CheckMultiState;
            UpdateTimeAndRateDisplays (True, True);
            Packet.CheckPacket;
            millisleep;
        UNTIL ElaspedSec100 (RememberTime) >= 50;

        RemoveWindow (NameSentWindow);
        RemoveWindow (CountryNameWindow);

        DisplayInsertMode (InsertMode);

        IF NOT QTCsEnabled THEN
            BEGIN
            EscapeDeletedCallEntry := CallWindowString;
            CallWindowString := '';
            END;

        ClrScr;
        Write (CallWindowString);
        END
    ELSE
        BEGIN
        StationInformationCall := '';
        ShowStationInformation (CallWindowString);
        DisplayGridSquareStatus (CallWindowString);

        IF BandMapEnable THEN
            IF GetRadioParameters (ActiveRadio, '', Frequency, Band, Mode, FALSE, False) THEN
                BEGIN
                BandMapCursorFrequency := Frequency;

                VisibleLog.DetermineIfNewMult (CallWindowString, ActiveBand, ActiveMode, MultString);

                Mult := MultString <> '';

                NewBandMapEntry (CallWindowString, Frequency, 0, ActiveMode, False, Mult, BandMapDecayTime, True);
                END
            ELSE
                IF AskForFrequencies THEN
                    BEGIN
                    Frequency := QuickEditFreq ('Enter frequency for ' + CallWindowString + ' in kHz : ', 10);

                    IF Frequency <> -1 THEN
                        BEGIN
                        VisibleLog.DetermineIfNewMult (CallWindowString, ActiveBand, ActiveMode, MultString);

                        Mult := MultString <> '';

                        NewBandMapEntry (CallWindowString, Frequency, 0, ActiveMode, False, Mult, BandMapDecayTime, True);
                        BandMapCursorFrequency := Frequency; {KK1L: 6.68 band map will track manual entry}
                        JustLoadingBandMapWithNoRadio := True; {KK1L: 6.68}
                        END;
                    END;

        IF DupeCheckSound = DupeCheckGratsIfMult THEN
            BEGIN
            VisibleLog.DetermineIfNewMult (CallWindowString, ActiveBand, ActiveMode, TempString);
            IF TempString <> '' THEN Tone.DoABeep (BeepCongrats);
            END;
        END;
    END;



FUNCTION GoodCallPutUp: BOOLEAN;

{ This function will move the call with the cursors around it from the
  possible call window to the call window.  It will be checked to see if
  it is a dupe.  If it is, a FALSE return will be generated.  Otherwise,
  a TRUE response is generated and if any initial exchange exists, it is
  put in the exchange window.  The exchange window will always be left
  active, with the cursor at 1, 1  }

VAR InitialExchange: Str80;

    BEGIN
    GoodCallPutUp := False;

    WITH PossibleCallList DO
        IF NumberPossibleCalls > 0 THEN
            BEGIN
            CallWindowString := List [CursorPosition].Call;
            ClearWindow (ExchangeWindow);

            IF ActiveWindow = ExchangeWindow THEN
                RestorePreviousWindow;

            ClrScr;
            Write (CallWindowString);

            IF WindowDupeCheck THEN Exit;

            ActivateExchangeWindow;

            InitialExchange := InitialExchangeEntry (CallWindowString);

            IF InitialExchange <> '' THEN
                BEGIN
                ClrScr;
                ExchangeWindowString := InitialExchange;
                Write (ExchangeWindowString);

                {TR6.74 - need to do this since IntialExchangeEntry does not }

                IF InitialExchangeOverwrite THEN
                    InitialExchangePutUp := ExchangeWindowString <> '';

                IF InitialExchangeCursorPos = AtEnd THEN
                    GoToXY (Length (InitialExchange) + 1, 1)
                ELSE
                    GoToXY (1, 1);
                END
            ELSE
                Write (ExchangeWindowString);

            ShowStationInformation (CallWindowString);
            DisplayGridSquareStatus (CallWindowString);
            GoodCallPutUp := True;
            END;
    END;


PROCEDURE ToggleStereoPin; {KK1L: 6.71}
    BEGIN
    StereoPinState := not StereoPinState;
    SetStereoPin (StereoControlPin, StereoPinState);
    END;

PROCEDURE ToggleModes;

    BEGIN
    IF (MultipleModesEnabled) OR (TotalContacts = 0) THEN
        BEGIN
        IF (ActiveMode = Phone) AND (ActiveBand >= Band6) AND (NOT FMMode) THEN
            FMMode := True
        ELSE
            BEGIN
            FMMode := False;

            IF DigitalModeEnable THEN
                BEGIN
                CASE ActiveMode OF
                    Phone:   ActiveMode := CW;
                    CW:      ActiveMode := Digital;
                    Digital: ActiveMode := Phone;
                    ELSE     ActiveMode := CW;
                    END;
                END
            ELSE
                CASE ActiveMode OF
                    Phone: ActiveMode := CW;
                    CW:    ActiveMode := Phone;
                    ELSE   ActiveMode := CW;
                    END;
            END;

        DisplayBandMode  (ActiveBand, ActiveMode,
                         (ActiveBand <= Band10) OR (ActiveBand = Band30) OR
                         (ActiveBand = Band17) OR (ActiveBand = Band12));

        DisplayCodeSpeed (CodeSpeed, CWEnabled, DVPOn, ActiveMode);

        {KK1L: 6.73 This gets done in UpdateTimeAndRateDisplay. Only do if no radio connected.}
        IF (ActiveRadio = RadioOne) AND ((Radio1ControlPort = nil) OR (NOT PollRadioOne)) THEN
            ModeMemory [RadioOne] := ActiveMode
        ELSE IF (ActiveRadio = RadioTwo) AND ((Radio2ControlPort = nil) OR (NOT PollRadioTwo)) THEN
            ModeMemory [RadioTwo] := ActiveMode;

        UpdateTotals;

        IF QSOByMode THEN VisibleDupeSheetChanged := True;

        IF MultByMode THEN
            BEGIN
            VisibleLog.ShowRemainingMultipliers;
            VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);
            END;

        BandMapBand := ActiveBand;
        BandMapMode := ActiveMode; {KK1L: 6.68 BM now tracks mode with no radio connected on mode change}
        DisplayBandMap;
        END;
    END;



PROCEDURE DupeCheckOnInactiveRadio;

{ This will perform a dupe check on the inactive radio }

VAR Mode: ModeType;
    Band: BandType;
    Frequency: LONGINT;
    MultString, TempString: Str40;
    Mult: BOOLEAN;
    AltDDupeCheckCall: CallString;

    BEGIN
    IF SingleRadioMode THEN
        BEGIN
        QuickDisplay ('Alt-D command disabled by SINGLE RADIO MODE = TRUE');
        Tone.DoABeep (Single);
        Wait (3000);
        Exit;
        END;

    RITEnable := False;

//    Str (SpeedMemory [InactiveRadio], SpeedString); {KK1L: 6.73 Used to use a variable CheckSpeed}

    BandMapBand := BandMemory [InactiveRadio];
    BandMapMode := ModeMemory [InactiveRadio];
    DisplayBandMap;

    TempString := UpperCase (QuickEditResponseWithPartials
                   ('Enter call to be checked on ' + BandString [BandMemory[InactiveRadio]] +
                    ModeString [ModeMemory[InactiveRadio]] + ' : ', 12));
    IF (TempString <> EscapeKey) AND (TempString <> '') THEN
            AltDDupeCheckCall := TempString
    else
            AltDDupeCheckCall := '';

    IF (AltDDupeCheckCall <> '') AND (AltDDupeCheckCall <> EscapeKey) THEN
        SetUpAltDDupeCheck (AltDDupeCheckCall, True);

    RITEnable := True;
    END;



PROCEDURE SwapMultDisplay;

    BEGIN
    IF NumberDifferentMults > 1 THEN
        BEGIN

        CASE RemainingMultDisplay OF
            Domestic:
                IF DoingDXMults THEN
                    RemainingMultDisplay := DX
                ELSE
                    IF DoingZoneMults THEN
                        RemainingMultDisplay := Zone;

            DX: IF DoingZoneMults THEN
                    RemainingMultDisplay := Zone
                ELSE
                    {IF DoingDomesticMults AND (ActiveDomesticMult <> WYSIWYGDomestic) THEN}
                    IF (DoingDomesticMults AND (DomesticQTHDataFileName <> '')) THEN
                       {KK1L: 6.68 changed above to allow domestic mults to be displayed if there is a dom file}
                        RemainingMultDisplay := Domestic;

            Zone:
                {IF DoingDomesticMults AND (ActiveDomesticMult = DomesticFile)}
                IF (DoingDomesticMults AND (DomesticQTHDataFileName <> '')) THEN
                    {KK1L: 6.68 changed above to allow domestic mults to be displayed if there is a dom file}
                    RemainingMultDisplay := Domestic
                ELSE
                    IF DoingDXMults THEN
                        RemainingMultDisplay := DX;

            END;

        VisibleLog.ShowRemainingMultipliers;
        END;
    END;



PROCEDURE DeleteLastContact;

    BEGIN
    VisibleLog.DeleteLastLogEntry;
    UpdateTotals;
    VisibleLog.ShowRemainingMultipliers;
    VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);
    DisplayTotalScore (TotalScore);
    DisplayInsertMode (InsertMode);
    DisplayNextQSONumber (TotalContacts + 1);
    IF VisibleDupeSheetEnable THEN
        BEGIN
        VisibleDupeSheetChanged := True;
        VisibleLog.DisplayVisibleDupeSheet (ActiveBand, ActiveMode);
        END;
    END;



PROCEDURE LogBackCopy (Seconds: INTEGER);

VAR SecString, TempString: Str80;

    BEGIN
    Str (Seconds, SecString);
    Str (TotalContacts + 1, TempString);

    TempString := '; Backcopy made for QSO #' + TempString + ' at ' +
                  GetTimeString + ' for ' + SecString + ' seconds.';

    PushLogStringIntoEditableLogAndLogPopedQSO (TempString, True);
    END;



PROCEDURE PlayLastSeconds (Seconds: INTEGER);

VAR FileRead: TEXT;

    BEGIN
    SaveBackCopyFile (DVPPath + 'TEMP.BCP', Seconds);
    REPEAT millisleep UNTIL OpenFileForRead (FileRead, DVPPath + 'TEMP.BCP');
    Wait (100);

    Close (FileRead);

    DVPListenMessage ('TEMP.BCP', True);
    END;



PROCEDURE ExitProgram;

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
        LNoSound;
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

    IF AskIfContestOver AND NOT VisibleLog.EditableLogIsEmpty THEN
        BEGIN
        TempString := QuickEditResponse ('Is the contest over (add last five QSOs to LOG.DAT)? (Y/N) : ', 1);
        IF UpperCase (TempString) = 'Y' THEN MoveEditableLogIntoLogFile;
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
    LNoSound;
    SetCBreak (ControlBreakStatus);
    Halt;
    END;



PROCEDURE TalkToRTTYPort;

VAR Key: CHAR;

    BEGIN
    IF ActiveRTTYPort = nil THEN Exit;

    QuickDisplay ('What you type will go to the TNC.  Press ESCAPE to stop.');

    REPEAT
        REPEAT
            CheckRTTY;
        UNTIL KeyPressed;

        Key := ReadKey;

        IF Key = EscapeKey THEN
            BEGIN
            QuickDIsplay ('Back to net');
            Exit;
            END;

//        Port [RTTYPortAddress] := Ord (Key);
    UNTIL False;
    END;




PROCEDURE PacketWindow;

VAR Key: CHAR;
    CommandLine: Str80;
    TimeMark: TimeRecord;

    BEGIN
    PacketWindowUp := True;

    RITEnable := False;

    QuickDisplay ('You are now talking to your packet port.  Use Control-B to exit.');

    SaveSetAndClearActiveWindow (DupeSheetWindow);
    ClrScr;

    Packet.DisplayPacketDisplayBuffer;

    { Show chars from incomplete line that isn't in the PacketDisplayBuffer }

    Write (Packet.PacketDisplayLine);

    CommandLine := '';

    MarkTime (TimeMark);

    REPEAT
        IF ActiveMultiPort <> nil THEN CheckMultiState;

        UpdateTimeAndRateDisplays (True, True);


        IF NewKeyPressed THEN
            BEGIN
            MarkTime (TimeMark);

            Key := NewReadKey;

            IF Key = ControlB THEN
                BEGIN
                IF PacketAutoCR AND (CommandLine <> '') THEN
                    IF ActivePacketPort <> nil THEN
                        SendChar (ActivePacketPort, Key)
                    ELSE
                        IF ActiveMultiPort <> nil THEN
                            BEGIN
                            IF K1EANetworkEnable THEN
                                SendMultiMessage ('B' + K1EAStationID + ' ' + CommandLine)
                            ELSE
                                SendMultiCommand (MultiBandAddressArray [ActiveBand], $FF,
                                                  MultiPacketMessageToSend , CommandLine);
                            END;

                RestorePreviousWindow;

                RemoveWindow (BigWindow);
                VisibleLog.SetUpEditableLog;
                UpdateTotals;
                VisibleLog.ShowRemainingMultipliers;
                VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);

                IF VisibleDupeSheetEnable THEN
                    BEGIN
                    VisibleDupeSheetChanged := True;
                    VisibleLog.DisplayVisibleDupeSheet (ActiveBand, ActiveMode);
                    END;

                RemoveWindow (QuickCommandWindow);
                PacketWindowUp := False;
                RITEnable := True;
                Exit;
                END;

            IF Key = NullKey THEN
                BEGIN
                Key := NewReadKey;

                CASE Key OF
                  AltA: IF (CallWindowString = '') AND (LastSCPCall = '') THEN
                            SetAlarm
                        ELSE
                            VisibleLog.SuperCheckPartial (CallWindowString, False, ActiveRadio);
                            {KK1L: 6.73 Added ActiveRadio}

                  AltB: BEGIN
                        RememberFrequency;
                        BandUp;
                        {KK1L: 6.73 NOTE the following statement is redundant. It is in BandUp and BandDown.}
                        IF QSONumberByBand THEN
                            DisplayNextQSONumber (TotalContacts + 1);
                        END;

                  AltD: IF K1EANetworkEnable THEN
                            PassStationToCTNetwork
                        ELSE
                            DupeCheckOnInactiveRadio;

                  AltK: ToggleCW (True);

                  AltM: BEGIN
                        RememberFrequency;
                        ToggleModes;
                        END;

                  AltR: BEGIN
                        IF (TwoRadioState = StationCalled) THEN {KK1L: 6.73}
                            BEGIN
                            Tone.DoABeep(Warning);
                            QuickDisplay('You are working a station on the 2nd radio. Escape to cancel first!');
                            END
                        ELSE
                            BEGIN
                            SwapRadios;
                            InactiveRigCallingCQ := False;
                            Str (SpeedMemory[InactiveRadio], SpeedString); {KK1L: 6.73 Used to use a variable CheckSpeed}
                            END;
                        END;

                  AltS: SetNewCodeSpeed;
                  AltT: TimeAndDateSet;

                  AltV: BEGIN
                        RememberFrequency;
                        BandDown;
                        END;

                  AltX: ExitProgram;

                  AltY: BEGIN
                        DeleteLastContact;
                        LastTwoLettersCrunchedOn := '';
                        END;

                  PageUpKey:   SpeedUp;
                  PageDownKey: SlowDown;
//These send the arrow key combinations that
//should give the command history for a linux shell
                  UpArrow:  BEGIN
                    SendChar (ActivePacketPort,chr($1b));
                    SendChar (ActivePacketPort,chr($5b));
                    SendChar (ActivePacketPort,chr($41));
                    END;

                  DownArrow: BEGIN
                    SendChar (ActivePacketPort,chr($1b));
                    SendChar (ActivePacketPort,chr($5b));
                    SendChar (ActivePacketPort,chr($42));
                    END;

                  LeftArrow: BEGIN
                    SendChar (ActivePacketPort,chr($1b));
                    SendChar (ActivePacketPort,chr($5b));
                    SendChar (ActivePacketPort,chr($44));
                    END;

                  RightArrow: BEGIN
                    SendChar (ActivePacketPort,chr($1b));
                    SendChar (ActivePacketPort,chr($5b));
                    SendChar (ActivePacketPort,chr($43));
                    END;

                  ELSE
                      SendFunctionKeyMessage (Key, OpMode);
                      {KK1L: note: this is where to add switch to SAPMode if NextBandMap command}
                  END;
                END

            ELSE
                BEGIN
                IF ActivePacketPort <> nil THEN
                    SendChar (ActivePacketPort, Key)
                ELSE
                    IF Key <> BackSpace THEN
                        BEGIN
                        IF Key <> CarriageReturn THEN
                            Write (Key)
                        ELSE
                            WriteLn (Key);
                        END
                    ELSE
                        IF WhereX > 1 THEN
                            BEGIN
                            GoToXY (WhereX - 1, WhereY);
                            Write (' ');
                            GoToXY (WhereX - 1, WhereY);
                            END;

                IF Key <> BackSpace THEN
                    CommandLine := CommandLine + Key
                ELSE
                    Delete (CommandLine, Length (CommandLine), 1);
                END;

            IF Key = CarriageReturn THEN
                BEGIN
                IF (ActivePacketPort = nil) AND (ActiveMultiPort <> nil) THEN
                    BEGIN
                    IF K1EANetworkEnable THEN
                        SendMultiMessage ('B' + K1EAStationID + ' ' + CommandLine)
                    ELSE
                        SendMultiCommand (MultiBandAddressArray [ActiveBand], $FF,
                                          MultiPacketMessageToSend , CommandLine);
                    END;

                CommandLine := '';
                END;

            END;

        Packet.CheckPacketBuffer (True);
        Packet.CheckPacketMessage;

        Wait (4);
    UNTIL (ActiveMultiPort <> nil) AND (ElaspedSec100 (TimeMark) > 2000);

    IF PacketAutoCR AND (CommandLine <> '') THEN
        IF ActivePacketPort <> nil THEN
            SendChar (ActivePacketPort, Key)
        ELSE
            IF ActiveMultiPort <> nil THEN
                BEGIN
                IF K1EANetworkEnable THEN
                    SendMultiMessage ('B' + K1EAStationID + ' ' + CommandLine)
                ELSE
                    SendMultiCommand (MultiBandAddressArray [ActiveBand], $FF,
                                      MultiPacketMessageToSend , CommandLine);
                END;

    RestorePreviousWindow;

    RemoveWindow (BigWindow);
    VisibleLog.SetUpEditableLog;
    UpdateTotals;
    VisibleLog.ShowRemainingMultipliers;
    VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);

    IF VisibleDupeSheetEnable THEN
        BEGIN
        VisibleDupeSheetChanged := True;
        VisibleLog.DisplayVisibleDupeSheet (ActiveBand, ActiveMode);
        END;

    RemoveWindow (QuickCommandWindow);
    PacketWindowUp := False;
    RITEnable := True;
    END;


FUNCTION CheckNullKeys: CHAR; {KK1L: 6.71 I needed this twice in AutoCQResume, so I split it out.}
                              {KK1L: 6.71a Made function return key to check}
VAR Key: CHAR;
    BEGIN

    IF NewKeyPressed THEN
        BEGIN
        Key := NewReadKey;
        CheckNullKeys := Key; {KK1L: 6.71a}

        CASE Key OF
            NullKey:
                CASE (NewReadKey) OF
                    AltA: IF (CallWindowString = '') AND (LastSCPCall = '')  THEN
                              SetAlarm
                          ELSE
                              VisibleLog.SuperCheckPartial (CallWindowString, False, ActiveRadio);
                              {KK1L: 6.73 Added ActiveRadio}

                    AltD: IF K1EANetworkEnable THEN
                              PassStationToCTNetwork
                          ELSE
                              DupeCheckOnInactiveRadio;

                    AltE: BEGIN
                          RITEnable := False;
                          VisibleLog.EditLog;
                          RITEnable := True;
                          UpdateTotals;
                          VisibleLog.ShowRemainingMultipliers;
                          VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);
                          DisplayTotalScore (TotalScore);
                          DisplayInsertMode (InsertMode);
                          DisplayNextQSONumber (TotalContacts + 1);
                          LastTwoLettersCrunchedOn := '';

                          IF VisibleDupeSheetEnable THEN
                              BEGIN
                              VisibleDupeSheetChanged := True;
                              VisibleLog.DisplayVisibleDupeSheet (ActiveBand, ActiveMode);
                              END;

                          VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);
                          END;

                    AltF: SaveLogFileToFloppy;
                    AltG: SwapMultDisplay;

                    AltH: BEGIN
                          PutUpHelpMenu;
                          RestorePreviousWindow;
                          VisibleDupeSheetRemoved := True;
                          END;

                    AltL: BEGIN
                          RITEnable := False;
                          VisibleLog.SearchLog (CallWindowString);
                          RITEnable := True;
                          END;

                    AltO: BEGIN
                          RITEnable := False;
                          AddReminder;
                          RITEnable := True;
                          END;

                    AltP: BEGIN
                          RITEnable := False;
                          shiftchange(0);
                          MemoryProgram;
                          if (shiftkeyenable = shift) then shiftchange(1);
                          if (shiftkeyenable = altshift) then shiftchange(2);
                          RITEnable := True;
                          VisibleLog.SetUpEditableLog;
                          SaveSetAndClearActiveWindow (QuickCommandWindow);
                          END;

                    AltS: SetNewCodeSpeed;

                    AltT: TimeAndDateSet;

                    AltU: BEGIN
                          MoveEditableLogIntoLogFile;
                          UpdateTotals;
                          END;

                    AltW: WakeUpCount := 0;

                    AltX: BEGIN
                          RestorePreviousWindow;
                          ExitProgram;
                          SaveSetAndClearActiveWindow (QuickCommandWindow); {KK1L: 6.71}
                          END;

                    AltY: BEGIN
                          DeleteLastContact;
                          LastTwoLettersCrunchedOn := '';
                          END;

                    AltDash:
                        BEGIN
                        IF AutoSendCharacterCount > 0 THEN
                            AutoSendEnable := NOT AutoSendEnable;

                        DisplayAutoSendCharacterCount;
                        END;

                    PageDownKey: {KK1L: 6.71 Was PageUpKey}
                        IF AutoCQDelayTime > 1 THEN
                            Dec (AutoCQDelayTime);

                    PageUpKey: {KK1L: 6.71 Was PageDownKey}
                        Inc (AutoCQDelayTime);

                    ControlEnd:
                        BEGIN
                        EditBandMap;
                        SaveSetAndClearActiveWindow (QuickCommandWindow);
                        END;

                    ControlInsert: {KK1L: 6.65 Insert BM place holder entry}
                        BEGIN
                        AddBandMapPlaceHolder;
                        END;

                    ControlDelete: {KK1L: 6.65 Delete BM entry while logging}
                        BEGIN
                        IF BandMapCursorData^.Frequency > 0 THEN
                            BEGIN
                            DisplayBandMap; {sets FoundCursor true if cursor on valid entry}
                            IF FoundCursor THEN
                                BEGIN
                                DeleteBandMapEntry (BandMapCursorData);
                                DisplayBandMap;
                                END;
                            END;
                        END;

                    END;

            ControlB:
                IF (ActivePacketPort <> nil) OR
                   (ActiveMultiPort <> nil) OR
                   (ActiveRTTYPort <> nil) THEN
                    BEGIN
                    PacketWindow;
                    ResetSavedWindowListAndPutUpCallWindow;
                    SaveSetAndClearActiveWindow (QuickCommandWindow);
                    END;

            ControlJ:
                BEGIN
                RITEnable := False;

                ProcessConfigurationInput;
                ClrScr;
                DisplayAutoSendCharacterCount;

                RITEnable := True;
                END;

            ControlL:
                BEGIN
                ViewLogFile;
                VisibleDupeSheetRemoved := True;
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

                    PushLogStringIntoEditableLogAndLogPopedQSO (AddedNoteString, True);
                    END;

                RITEnable := True;
                END;

            ControlO: VisibleLog.ShowMissingMultiplierReport;

            ELSE
                BEGIN
                FlushCWBufferAndClearPTT;
                RestorePreviousWindow;
                RemoveWindow (QuickCommandWindow);
                KeyPressedMemory := Key;
                CheckNullKeys := EscapeKey; {KK1L: 6.71a}
                Exit;
                END;

            END; { Of Case Key }

        ClrScr;

        Write ('Repeating ', KeyId (AutoCQMemory), '  Listen time = ',
               (AutoCQDelayTime / 2):2:1, ' - PgUp/Dn to adjust or ESCAPE     ');

        END;
    END;


PROCEDURE AutoCQResume (SkipFirstMessage: BOOLEAN);

VAR CQMemory, SendChar: CHAR;
    CharacterCount: INTEGER;
    FileName, TempString: Str80;
    QSONumberString: Str20;
    LastDisplayedTimeElasped, Count, TimeElasped: INTEGER;
    StartOfLastPhoneMessage: TimeRecord;
    TimeOut: BYTE; {KK1L: 6.71b}


    BEGIN
    LastDisplayedTimeElasped := 0;
    SetUpToSendOnActiveRadio;
    KeyPressedMemory := Chr (0);
    CWEnabled := True;

    IF NOT (((AutoCQMemory >= F1)  AND (AutoCQMemory <= AltF10)) OR
            ((AutoCQMemory >= F11) AND (AutoCQMemory <= AltF12))) THEN
                Exit;

    IF AutoCQDelayTime > 0 THEN
        BEGIN
        SaveSetAndClearActiveWindow (QuickCommandWindow);

        Write ('Repeating ', KeyId (AutoCQMemory), '  Listen time = ',
                (AutoCQDelayTime / 2):2:1, ' - PgUp/Dn to adjust or ESCAPE     ');

        CQMemory := AutoCQMemory;

        REPEAT
            {KK1L: 6.68 Set LastCQ stuff when calling CQ using auto CQ}
            IF FrequencyDisplayed THEN
                BEGIN
                LastCQFrequency := DisplayedFrequency;
                LastCQMode      := ActiveMode;
                END
            ELSE
                LastCQFrequency := 0;

            IF (AutoCQMemory = F1) AND RandomCQMode THEN
                BEGIN
                Count := 0;

                REPEAT
                    Inc (Count);

                    CASE Random (4) OF
                        0: CQMemory := F1;
                        1: CQMemory := F2;
                        2: CQMemory := F3;
                        3: CQMemory := F4;
                        END;

                UNTIL (GetCQMemoryString (ActiveMode, CQMemory) <> '') OR (Count > 1000); {KK1L: 6.73 Added mode}
                END;

            IF ActiveMode <> Phone THEN
                BEGIN
                TempString := GetCQMemoryString (ActiveMode, CQMemory); {KK1L: 6.73 Added mode}

                IF NOT SkipFirstMessage THEN
                    FOR CharacterCount := 1 TO Length (TempString) DO
                        BEGIN
                        SendChar := TempString [CharacterCount];

                        CASE SendChar OF
                             '\': AddStringToBuffer (MyCall, CWTone);
                             '>': ClearRIT;
                            ELSE AddStringToBuffer (SendChar, CWTone);
                            END;
                        END;

                END
            ELSE  {KK1L: NOTE Phone mode}
                BEGIN
                TempString := GetCQMemoryString (ActiveMode, CQMemory); {KK1L: 6.73 Added mode}

                {KK1L: 6.72 Pulled out the DVK code from the REPEAT...UNTIL. Can't tell end of DVK message}
                {REPEAT }{KK1L: 6.71}
                {    IF DVPEnable THEN}
                {        BEGIN}
                {        WHILE TempString <> '' DO}
                {            BEGIN}
                {            FileName := RemoveFirstString (TempString);}
                {            GetRidOfPrecedingSpaces (FileName);}
                {            IF NOT SkipFirstMessage THEN DVPPlayMessage (FileName);}
                {            END;}
                {        END}
                {    ELSE}
                {        IF (NOT SkipFirstMessage) AND (ActiveDVKPort <> NoPort) THEN} {KK1L: 6.71 added DVK check}
                {            SendDVKMessage (TempString); }
                { }
                {    IF CheckNullKeys = EscapeKey THEN Exit;} {KK1L: 6.71a Check if key fell out of loop or escape}
                { }
                {    Wait (20); } {KK1L: 6.71}
                {UNTIL NOT (DVPMessagePlaying OR DVKMessagePlaying); }{KK1L: 6.71 should start timer at END of message}

                {KK1L: 6.72 Replaced above with this}
                IF DVPEnable THEN
                    REPEAT
                        WHILE TempString <> '' DO
                            BEGIN
                            FileName := RemoveFirstString (TempString);
                            GetRidOfPrecedingSpaces (FileName);
                            IF NOT SkipFirstMessage THEN DVPPlayMessage (FileName);
                            END;

                        IF CheckNullKeys = EscapeKey THEN
                            BEGIN
                            IF DVPEnable AND DVPMessagePlaying THEN {KK1L: 6.71b Kill DVP}
                                BEGIN
                                TimeOut := 0;

                                DVPStopPlayback;
                                REPEAT
                                    Wait (5);
                                    Inc (TimeOut);
                                UNTIL (NOT DVPMessagePlaying) OR (TimeOut > 30);
                                END;
                            Exit;
                            END;

                    Wait (20);
                    UNTIL NOT DVPMessagePlaying
                ELSE
                    BEGIN
                    IF (NOT SkipFirstMessage) AND DVKEnable THEN
                        SendDVKMessage (TempString)
                    ELSE
                        FoundCommand(TempString);

                    IF CheckNullKeys = EscapeKey THEN
                        BEGIN
                        IF DVKEnable THEN StartDVK(0); {KK1L: 6.71b Kill DVK}
                        Exit;
                        END;
                    END;

                MarkTime (StartOfLastPhoneMessage); {KK1L: 6.72 Or End in the case of DVP}
                END;

            SkipFirstMessage := False;

            {KK1L: 6.68 From here to REPEAT added to put autoCQ in band map and send multi info message.}
            IF BandMapEnable AND (LastDisplayedFreq[RadioOne] <> 0) AND (OpMode = CQOpMode) AND BandMapDisplayCQ THEN
                BEGIN
                Str (TotalContacts + 1, QSONumberString);
                BandMapCursorFrequency := DisplayedFrequency;
                NewBandMapEntry ('CQ/' + QSONumberString,
                                 DisplayedFrequency, 0, ActiveMode,
                                 False, False, BandMapDecayTime, True);
                LastCQFrequency := DisplayedFrequency;
                LastCQMode      := ActiveMode;
                END;

            IF ActiveMultiPort <> nil THEN
                CreateAndSendCQMultiInfoMessage;

            REPEAT

                IF ActiveMode <> Phone THEN
                    BEGIN
                    TimeElasped := ActiveKeyer.GetCountsSinceLastCW DIV 298;
                    END
                ELSE
                    TimeElasped := MicroTimeElapsed (StartOfLastPhoneMessage) DIV 50;

                IF LastDisplayedTimeElasped <> TimeElasped THEN
                    BEGIN
                    GoToXY (WhereX - 4, WhereY);

                    IF TimeElasped < 20 THEN Write (' ');

                    Write ((TimeElasped/2):2:1);
                    LastDisplayedTimeElasped := TimeElasped;
                    END;

                IF CheckNullKeys = EscapeKey THEN Exit; {KK1L: 6.71a Check if key fell out of loop or escape}

                {KK1L: 6.72 NOTE Timing delays can occur if there are radio communcations problems due to    }
                {                various timeouts, etc in the polling called from UpdateTimeAndRateDisplays. }
                UpdateTimeAndRateDisplays (True, True);

                IF ActiveMultiPort <> nil THEN CheckMultiState;

                Packet.CheckPacket;

            Wait (4);

            UNTIL TimeElasped >= AutoCQDelayTime;

        UNTIL False;
        END;
    END;



PROCEDURE AutoCQ;

VAR Time, Result: INTEGER;
    Key: CHAR;
    TempString: Str80;

    BEGIN
    SetUpToSendOnActiveRadio;
    SaveSetAndClearActiveWindow (QuickCommandWindow);

    Write ('Press the memory key you want to repeat');

    REPEAT millisleep UNTIL NewKeyPressed;

    Key := Upcase (NewReadKey);

    IF Key <> NullKey THEN
        BEGIN
        RemoveAndRestorePreviousWindow;
        Exit;
        END;

    Key := NewReadKey;

    IF NOT (((Key >= F1) AND (Key <= AltF10)) OR ((Key >= F11) AND (Key <= AltF12))) THEN
        BEGIN
        RemoveAndRestorePreviousWindow;
        Exit;
        END;

    AutoCQMemory := Key;


    REPEAT
        ClrScr;

        IF ActiveMode = Phone THEN
            IF DVPEnable THEN {KK1L: 6.72 Now need to differentiate DVP and DVK}
            {Write ('Number of seconds between start of transmissions : ')} {KK1L: 6.71 fixed this in AutoCQResume}
                Write ('Number of seconds of listening time : ') {KK1L: 6.71 fixed this in AutoCQResume}
            ELSE
                Write ('Number of seconds between start of transmissions : ') {KK1L: 6.72}
        ELSE
            Write ('Number of seconds of listening time : ');

        TempString := '';

        REPEAT
            REPEAT millisleep UNTIL NewKeyPressed;

            Key := Upcase (NewReadKey);

            IF Key = EscapeKey THEN
                BEGIN
                RemoveAndRestorePreviousWindow;
                Exit;
                END;

            IF Key = Backspace THEN
                IF Length (TempString) > 0 THEN
                    BEGIN
                    Tempstring [0] := Chr (Length (TempString) - 1);
                    GoToXY (WhereX - 1, WhereY);
                    ClrEol;
                    END;

            IF (Key >= '0') AND (Key <= '9') THEN
                BEGIN
                TempString := TempString + Key;
                Write (Key);
                END;

        UNTIL (Length (TempString) = 2) OR (Key = CarriageReturn);

        Val (TempString, Time, Result);
        Time := Time * 2;

    UNTIL (Time >= 1) AND (Time <= 99);

    AutoCQDelayTime := Time;

    RemoveAndRestorePreviousWindow;
    AutoCQResume (False);
    END;



PROCEDURE ShowPartialCallMults (WindowString: Str20);

VAR TestString: Str20;
    TempString: Str40;
    TempExchange: ContestExchange;
    Distance, Heading: INTEGER;

    BEGIN
    IF NOT PartialCallMultsEnable THEN Exit;

    IF NOT (ActiveWindow = CallWindow) THEN
        IF ActiveDomesticMult <> GridSquares THEN Exit;

    IF Length (WindowString) < 2 THEN Exit;

    TempString := WindowString;

    WHILE TempString <> '' DO
        BEGIN
        TestString := RemoveFirstString (TempString);

        IF (MyGrid <> '') AND LooksLikeAGrid (TestString) THEN
            BEGIN
            SaveSetAndClearActiveWindow (BeamHeadingWindow);
            Heading := Round (GetBeamHeading (MyGrid, TestString));
            Write (TestString, ' ', Heading, DegreeSymbol);

            IF DistanceMode <> NoDistanceDisplay THEN
                BEGIN
                Distance := GetDistanceBetweenGrids (MyGrid, TestString);

                IF DistanceMode = DistanceMiles THEN
                    BEGIN
                    Distance := Round (Distance / 1.6);
                    Write (' ', Distance, 'm');
                    END
                ELSE
                    Write (' ', Distance, 'km');
                END;

            RestorePreviousWindow;
            END;

        IF DoingDomesticMults THEN
            BEGIN
            TempExchange.QTHString := RemoveFirstString (TestString);

            IF NOT (StringIsAllNumbersOrSpaces (TempExchange.QTHString)) THEN

            IF FoundDomesticQTH (TempExchange) THEN
                BEGIN
                VisibleLog.ShowDomesticMultiplierStatus (TempExchange.DomesticQTH);
                Exit;
                END;
            END;
        END;

    VisibleLog.ShowMultiplierStatus (WindowString);
    END;



PROCEDURE CheckEverything (VAR KeyChar: CHAR; VAR WindowString: Str80);

VAR Result: INTEGER;
    BandMapInitialExchange: Str20;
    TimeOut: BYTE;
    PacketChar: CHAR;
    InactiveRadioMode: ModeType;

    BEGIN
    IF ActiveMultiPort <> nil THEN CheckMultiState;

    IF ActiveRTTYPort <> nil THEN CheckRTTY;

    {KK1L: 6.72 Moved here to speed up SCP. UpdateTimeAndRateDisplays hogs a bit of time now.}
    IF (ActiveWindow = CallWindow) AND (SCPMinimumLetters > 0) AND (NOT NewKeyPressed) THEN
        VisibleLog.SuperCheckPartial (WindowString, True, ActiveRadio); {KK1L: 6.73 Added ActiveRadio}

    {UpdateTimeAndRateDisplays (True, True);} {KK1L: 6.72 Moved from here to the end of the procedure.}

    IF PacketFile THEN
        BEGIN
        BlockRead (PacketFileRead, PacketChar,  SizeOf (PacketChar), Result);
        PacketReceiveCharBuffer.AddEntry (Ord (PacketChar) AND $7F);

        IF Eof (PacketFileRead) THEN
            BEGIN
            Close (PacketFileRead);
            PacketFile := False;
            QuickDisplay ('Packet file processing completed.');
            END;
        END;

    Packet.CheckPacket;

    IF TakingABreak THEN
        BEGIN
        Result := ElaspedMinutes (OffTimeStart);

        IF Result <> LastDisplayedBreakTime THEN
            BEGIN
            SaveSetAndClearActiveWindow (QuickCommandWindow);
            Write ('BREAK!!  This break = ', MinutesToTimeString (Result),
                   '  Total time off = ', MinutesToTimeString (Result + TotalOffTime),
                   '  \ON to end break');
            RestorePreviousWindow;
            LastDisplayedBreakTime := Result;
            END;
        END;

    IF (ActiveBand <> RememberBand) OR (ActiveMode <> RememberMode) THEN
        BEGIN
        UpdateTotals;

        IF ((MultByBand AND (ActiveBand <> RememberBand)) OR
            (MultByMode AND (ActiveMode <> RememberMode))) THEN
                BEGIN
                VisibleLog.ShowRemainingMultipliers;
                VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);
                END;

        IF ActiveBand <> RememberBand THEN
            BEGIN
            BandMapBand := ActiveBand;
            BandMapMode := ActiveMode; {KK1L: 6.69}
            DisplayBandMap;
            END;

        RememberBand := ActiveBand;
        RememberMode := ActiveMode;

        IF VisibleDupeSheetEnable THEN
            VisibleLog.DisplayVisibleDupeSheet (ActiveBand, ActiveMode);
        END;

    IF Debug OR ReadInLog THEN
        BEGIN
        REPEAT
            IF ActiveMultiPort <> nil THEN CheckMultiState;
            UpdateTimeAndRateDisplays (True, True);
            Packet.CheckPacket;
        UNTIL (NOT CWStillBeingSent and  not dvpmessageplaying) OR NewKeyPressed;

        IF NewKeyPressed THEN
            BEGIN
            Debug := False;

            IF ReadInLog THEN
                BEGIN
                ReadInLog := False;
                Close (ReadInLogFileRead);
                END;

            KeyChar := EscapeKey;
            SendMorse ('SK');
            END
        ELSE
            BEGIN

            { If we are in the exchange window, do a Return to log the QSO }

            IF Debug OR (ReadInLog AND NOT (ActiveWindow = CallWindow)) THEN
                BEGIN
                KeyChar := CarriageReturn;
                Exit;
                END;

            { We are in the call window }

            IF ReadInLog THEN
                BEGIN
                IF (Length (ReadInCallsign) = Length (WindowString)) AND
                   (Length (ReadInCallsign) > 0) THEN
                       KeyChar := CarriageReturn
                ELSE
                    IF WindowString = '' THEN
                        BEGIN
                        ReadInCallsign := GetNextCallFromReadInLog;
                        KeyChar := ReadInCallsign [1];
                        END
                    ELSE
                        KeyChar := ReadInCallsign [Length (WindowString) + 1];
                END
            ELSE

            { Debug doesn't get a callsign from here - it gets it from either
              a QSL Message or a CQ }

                Exit;

            END;
        END;

    { Do all the right things with the new callsign }

    IF (ActiveWindow = CallWindow) AND PartialCallEnable THEN
        IF Sheet.TwoLetterCrunchProcess (WindowString) THEN
             BEGIN
             VisibleLog.GeneratePartialCallList (WindowString,
                                                 ActiveBand,
                                                 ActiveMode,
                                                 PossibleCallList);

             DisplayPossibleCalls (PossibleCallList);
             END;

    {KK1L: 6.72 Moved ahead of UpdateTimeAndRateDisplays to boost reation time}
    {IF (ActiveWindow = CallWindow) AND (SCPMinimumLetters > 0) AND (NOT NewKeyPressed) THEN }
    {    VisibleLog.SuperCheckPartial (WindowString, True);                                  }

    IF (DualingCQState <> NoDualingCQs) AND NOT (CWStillBeingSent OR DVPMessagePlaying OR DVKMessagePlaying) THEN
        CASE DualingCQState OF

            DualSendingQSL, SendingDupeMessage:
                BEGIN
                SwapRadios;
                SendCrypticMessage (GetCQMemoryString (ActiveMode, AltF1)); {KK1L: 6.73 Added mode}

                IF (ActiveMode = Phone) AND DVPActive THEN
                    BEGIN
                    TimeOut := 0;

                    REPEAT
                        Wait (5);
                        Inc (TimeOut);
                    UNTIL DVPMessagePlaying OR (TimeOut > 60) OR DVKMessagePlaying;
                    END;

                DualingCQState := DualSendingCQ;
                InactiveRigCallingCQ := False;
                QuickDisplay ('Dualing CQ mode active.  Use Control-Dash to cancel');
                END;

            DualSendingExchange:
                BEGIN
                IF GetCQMemoryString (ActiveMode, AltF2) <> '' THEN {KK1L: 6.73 Added mode}
                    BEGIN
                     {KK1L: 6.73 Added mode}
                    SendCrypticMessage (ControlA + ControlB + GetCQMemoryString (ActiveMode, AltF2));
                    QuickDisplay ('Dualing CQ mode active.  Sending dummy CQ on inactive radio');

                    IF (ActiveMode = Phone) AND DVPActive THEN
                        BEGIN
                        TimeOut := 0;

                        REPEAT
                            Wait (5);
                            Inc (TimeOut);
                        UNTIL DVPMessagePlaying OR (TimeOut > 60) OR DVKMessagePlaying;
                        END;
                    END
                ELSE
                    QuickDisplay ('Dualing CQ mode active.  No CQ found in AltF2 to send on inactive radio.');

                DualingCQState := DualGettingExchange;
                END;


            DualSendingCQ:
                IF (WindowString = '') AND (ActiveWindow = CallWindow) THEN
                    BEGIN
                    SwapRadios;
                    SendFunctionKeyMessage (AltF1, CQOpMode);

                    IF (ActiveMode = Phone) AND DVPActive THEN
                        BEGIN
                        TimeOut := 0;

                        REPEAT
                            Wait (5);
                            Inc (TimeOut);
                        UNTIL DVPMessagePlaying OR (TimeOut > 60) OR DVKMessagePlaying;
                        END;
                    END;

            END;

    { W9RE }

    IF KeyersSwapped THEN
        BEGIN
        IF (ActiveMode = CW) AND NOT CWStillBeingSent THEN SetUpToSendOnActiveRadio;

        IF (ActiveMode = Phone) THEN
            BEGIN
            IF DVPActive AND NOT DVPMessagePlaying THEN SetUpToSendOnActiveRadio;
            IF DVKEnable AND NOT DVKMessagePlaying THEN SetUpToSendOnActiveRadio;
            END;
        END;

    IF (ActiveMode = CW) AND (SendExchangeKeyWhenCWHasStopped <> NullKey) THEN
        IF NOT CWStillBeingSent THEN
            BEGIN
            ProcessExchangeFunctionKey (SendExchangeKeyWhenCWHasStopped);
            SendExchangeKeyWhenCWHasStopped := NullKey;
            END;

    IF (DualingCQState = DualSendingCQ) AND (WindowString <> '') AND
       (ActiveWindow = CallWindow) THEN
           BEGIN
           SwapRadios;
           DualingCQState := WaitingForCallsignInput;
           QuickDisplay ('Dualing CQ mode active.  Enter rest of call that answered then ENTER');
           END;

    { See if there is something to be done as a result of a change in
      the band map blinking call }

    IF BandMapEnable THEN
        {IF (BandMapBlinkingCall <> BandMapInfoCall) THEN } { Different call }
        {KK1L: 6.73 Keeps station ready to work whenever tuned to a BM entry or NEXTBANDMAP}
        {I tried these things to allow tuning the inactive radio to load DupeInfoCall correctly. }
        {The problem is that if you tune slower than AutoSAPEnableRate then the call will not update.}
        {Of course you need to limit entry here otherwise the display will blink like crazy.}
        IF ((BandMapBlinkingCall <> CallLastTimeIWasHere) OR
           (BandMapBlinkingCall <> DupeInfoCallPrompt)) THEN
//if bandmapblinkingcall <> calllasttimeiwashere then
            BEGIN
            CallLastTimeIWasHere := BandMapBlinkingCall; {KK1L: 6.73}

            IF BandMapBlinkingCall <> '' THEN
                BEGIN
                BandMapInitialExchange := InitialExchangeEntry (BandMapBlinkingCall);

                {IF TwoRadioState <> CallReady THEN }{KK1L: 6.73}
                    BEGIN
                    {SaveSetAndClearActiveWindow (DupeInfoWindow);} {KK1L: 6.73 Moved to below.}
                    {WriteLn (BandMapBlinkingCall);}  {KK1L: 6.73 Moved to below.}
                    {Write (BandMapInitialExchange);} {KK1L: 6.73 Moved to below.}
                    {RestorePreviousWindow;}          {KK1L: 6.73 Moved to below.}

                    {KK1L: 6.73 Displays BM entry from inactive radio. Will set up to work if not dupe.}
                    {           Really only want this stuff to happen when tuning inactive radio.}
                    IF BandMapBand = BandMemory[InactiveRadio] THEN {KK1L: 6.73 tuning inactive radio.}
                        BEGIN
                        IF VisibleLog.CallIsADupe (BandMapBlinkingCall, BandMapBand, BandMapMode) THEN
                            BEGIN
                            SaveSetAndClearActiveWindow (DupeInfoWindow);
                            WriteLn (BandMapBlinkingCall + ' DUPE!!');
//next line fixed blinking on dupes ks
//                            DupeInfoCallPrompt := BandMapBlinkingCall;
                            Write ('on            ' + BandString [BandMapBand] + ModeString [BandMapMode]);
                            VisibleLog.ShowQSOStatus (BandMapBlinkingCall);
                            VisibleLog.ShowMultiplierStatus (BandMapBlinkingCall);
                            RestorePreviousWindow;

                            {SaveSetAndClearActiveWindow (AltCallWindow); }
                            {Write (BandMapBlinkingCall, ' Dupe!');       }
                            {RestorePreviousWindow;                       }
                            END
                        ELSE {KK1L: 6.73 Not a dupe. Set up to work with space bar.}
                            BEGIN
                            SaveSetAndClearActiveWindow (DupeInfoWindow);
                            {KK1L: 6.73 add condition to make auto ALT-D an option. Added TuneDupeCheckEnable}
                            IF (OpMode = CQOpMode) THEN
                                BEGIN
                                IF ModeMemory[InactiveRadio] = CW THEN
                                    WriteLn (BandMapBlinkingCall + ' OK!! at ' + SpeedString + ' WPM')
                                ELSE
                                    WriteLn (BandMapBlinkingCall + ' OK!!');
                                DupeInfoCallPrompt := BandMapBlinkingCall;
                                IF TuneDupeCheckEnable THEN
                                    BEGIN
                                    Write ('Space bar for ', BandString [BandMapBand], ModeString [BandMapMode], ' QSO');
                                    DupeInfoCall := BandMapBlinkingCall;
                                    TwoRadioState := CallReady; {KK1l: 6.73 This sets up for automatic ATL-D}
                                    END;
                                END
                            ELSE
                                BEGIN
                                DupeInfoCallPrompt := BandMapBlinkingCall;
                                IF TuneDupeCheckEnable THEN
                                    BEGIN
                                    WriteLn (BandMapBlinkingCall + ' OK!!          ');
                                    Write ('CQ mode for   ', BandString [BandMapBand], ModeString [BandMapMode], ' QSO');
                                    END;
                                END;

                            VisibleLog.ShowQSOStatus (BandMapBlinkingCall);
                            VisibleLog.ShowMultiplierStatus (BandMapBlinkingCall);
                            RestorePreviousWindow;

                            {SaveSetAndClearActiveWindow (AltCallWindow);  }
                            {Write (BandMapBlinkingCall, ' Call Ready!');  }
                            {DupeInfoCall := BandMapBlinkingCall;          }
                            {RestorePreviousWindow;                        }
                            END;
                        END;
                    END;

{  Moved this down 7 lines because it would over-write info for CQ QSO in
   progress.

                ShowStationInformation (BandMapBlinkingCall); }

                IF (ActiveWindow = CallWindow) AND
                   (OpMode = SearchAndPounceOpMode) AND
                   (WindowString <> BandMapBlinkingCall) AND
                   (BandMapBand = ActiveBand) AND
                   (OkayToPutUpBandMapCall) AND
                   (BandMapCallWindowEnable) THEN
                       BEGIN
                       { Moved this from up 7 lines in 5.88 }

                       ShowStationInformation (BandMapBlinkingCall);

                       WindowString := BandMapBlinkingCall;
                       ClrScr;
                       Write (' ' + WindowString + '  ' + BandMapInitialExchange);
                       GoToXY (Length (WindowString) + 1, WhereY);

                       { Why is this? - Removed in 6.44 }

                       { RestorePreviousWindow;  }

                       BandMapEntryInCallWindow := True;
                       END;
                END
            ELSE                 { Need to clear out the old call }

                { BandMapBlinkingCall is now null }

                BEGIN
                IF (ActiveWindow = CallWindow) THEN
                    BEGIN

                    { Clear this out if it is what I put up there before }

                    IF WindowString = BandMapInfoCall THEN
                        BEGIN
                        EscapeDeletedCallEntry := WindowString;
                        WindowString := '';
                        ClrScr;

                        { In 6.44 - moved this into WindowString = BandMapInfoCall }

                        IF ExchangeWindowString <> '' THEN
                            BEGIN
                            EscapeDeletedExchangeEntry := ExchangeWindowString;
                            ExchangeWindowString := '';
                            SaveSetAndClearActiveWindow (ExchangeWindow);
                            Write (ExchangeWindowString);
                            ExchangeWindowCursorPosition := Length (ExchangeWindowString) + 1;
                            RestorePreviousWindow;
                            END;

                        { Added in 6.61 to fix entries not coming up later }

                        {OkayToPutUpBandMapCall := True;}{KK1L: 6.73 To fix call popping up after working them.}
                        END;
                    END;

                {KK1L: 6.73 Really only want this stuff to happen when tuning inactive radio.}
                {           This clears DupeInfoCall because BandMapBlinkingEntry is null here.}
                IF BandMapBand = BandMemory[InactiveRadio] THEN
                    BEGIN
                    {KK1L: 6.73 Really only need this here and not above. I don't want to change to IDLE state}
                    {           whenever BandMapBlinkingCall = nil, but only when nil and the radio is moving.}
                    {           This does tie the movement sensitivity to the AutoSAPEnableRate.              }
                    {IF ((ActiveRadio = RadioOne) AND (RadioOnTheMove[RadioTwo])) OR   }
                    {   ((ActiveRadio = RadioTwo) AND (RadioOnTheMove[RadioOne])) THEN }
                    {IF RadioOnTheMove[InactiveRadio] THEN} {KK1L: 6.73 ????}
                        BEGIN
                        IF TuneDupeCheckEnable THEN
                            BEGIN
                            TwoRadioState := Idle;
                            DupeInfoCall := BandMapBlinkingCall;
                            END;
                        SaveSetAndClearActiveWindow (DupeInfoWindow);
                        DupeInfoCallPrompt := BandMapBlinkingCall;
                        WriteLn (BandMapBlinkingCall);
                        WriteLn ('');
                        VisibleLog.ShowQSOStatus (BandMapBlinkingCall);
                        VisibleLog.ShowMultiplierStatus (BandMapBlinkingCall);
                        RestorePreviousWindow;
                        END;
                    END;
                END;

            BandMapInfoCall := BandMapBlinkingCall;
            END;

    UpdateTimeAndRateDisplays (True, True); {KK1L: 6.72 Moved here to speed up SCP and other things}
    IF RadioOnTheMove[ActiveRadio] THEN {KK1L: 6.73 To fix call popping up after already working them.}
        OkayToPutUpBandMapCall := True
    ELSE
        OkayToPutUpBandMapCall := False;

    END;



PROCEDURE GoToLastCQFrequency;

    BEGIN
    IF LastCQFrequency > 0 THEN
        BEGIN
        IF CommandUseInactiveRadio THEN {KK1L: 6.73}
            BEGIN
            SetRadioFreq (InactiveRadio, LastCQFrequency, LastCQMode, 'A');
            CASE InactiveRadio OF
                RadioOne:
                    BEGIN
                    PreviousRadioOneFreq := LastCqFrequency; {KK1L: 6.73 Forces CQ mode for AutoSAPEnable}
                    END;
                RadioTwo:
                    BEGIN
                    PreviousRadioTwoFreq := LastCqFrequency; {KK1L: 6.73 Forces CQ mode for AutoSAPEnable}
                    END;
                END;
            END
        ELSE
            BEGIN
            SetRadioFreq (ActiveRadio, LastCQFrequency, LastCQMode, 'A');
            CASE ActiveRadio OF {KK1L: 6.69 Keeps LASTCQFREQ from changing to S&P Mode}
                RadioOne:
                    BEGIN
                    PreviousRadioOneFreq := LastCqFrequency; {KK1L: 6.73 Forces CQ mode for AutoSAPEnable}
                    END;
                RadioTwo:
                    BEGIN
                    PreviousRadioTwoFreq := LastCqFrequency; {KK1L: 6.73 Forces CQ mode for AutoSAPEnable}
                    END;
                END;
            END;
        END;
    END;


PROCEDURE GoToNextBandMapFrequency;
    BEGIN
    {KK1L: 6.73}
    IF (CommandUseInactiveRadio) AND  {KK1L: 6.73}
       (NextNonDupeEntryInBandMap (BandMemory[InactiveRadio], ModeMemory[InactiveRadio])) THEN
            SetUpBandMapEntry (BandMapCursorData, InactiveRadio)
    ELSE
        IF NextNonDupeEntryInBandMap (ActiveBand, ActiveMode) THEN
            BEGIN
            SetUpBandMapEntry (BandMapCursorData, ActiveRadio); {KK1L: Added ActiveRadio}

            IF ActiveWindow = ExchangeWindow THEN
                BEGIN
                ClrScr;
                ExchangeWindowString := '';
                RestorePreviousWindow;
                END;
            END;
    END;


{KK1L: 6.68}
PROCEDURE GoToNextMultBandMapFrequency;
    BEGIN
    IF (CommandUseInactiveRadio) AND  {KK1L: 6.73}
       (NextMultiplierEntryInBandMap (BandMemory[InactiveRadio], ModeMemory[InactiveRadio])) THEN
            SetUpBandMapEntry (BandMapCursorData, InactiveRadio)
    ELSE
        IF NextMultiplierEntryInBandMap (ActiveBand, ActiveMode) THEN
            BEGIN
            SetUpBandMapEntry (BandMapCursorData, ActiveRadio); {KK1L: Added ActiveRadio}

            IF ActiveWindow = ExchangeWindow THEN
                BEGIN
                ClrScr;
                ExchangeWindowString := '';
                RestorePreviousWindow;
                END;
            END;
    END;



PROCEDURE GoToNextDisplayedBandMapFrequency;
    BEGIN
    IF (CommandUseInactiveRadio) AND  {KK1L: 6.73}
       (NextNonDupeEntryInDisplayedBandMap (BandMemory[InactiveRadio], ModeMemory[InactiveRadio])) THEN
            SetUpBandMapEntry (BandMapCursorData, InactiveRadio)
    ELSE
        IF NextNonDupeEntryInDisplayedBandMap (ActiveBand, ActiveMode) THEN
            BEGIN
            SetUpBandMapEntry (BandMapCursorData, ActiveRadio); {KK1L: Added ActiveRadio}

            IF ActiveWindow = ExchangeWindow THEN
                BEGIN
                ClrScr;
                ExchangeWindowString := '';
                RestorePreviousWindow;
                END;
            END;
    END;


{KK1L: 6.68}
PROCEDURE GoToNextMultDisplayedBandMapFrequency;
    BEGIN
    IF (CommandUseInactiveRadio) AND  {KK1L: 6.73}
       (NextMultiplierEntryInDisplayedBandMap (BandMemory[InactiveRadio], ModeMemory[InactiveRadio])) THEN
            SetUpBandMapEntry (BandMapCursorData, InactiveRadio)
    ELSE
        IF NextMultiplierEntryInDisplayedBandMap (ActiveBand, ActiveMode) THEN
            BEGIN
            SetUpBandMapEntry (BandMapCursorData, ActiveRadio); {KK1L: Added ActiveRadio}

            IF ActiveWindow = ExchangeWindow THEN
                BEGIN
                ClrScr;
                ExchangeWindowString := '';
                RestorePreviousWindow;
                END;
            END;
    END;



FUNCTION FoundCommand (VAR SendString: Str160): BOOLEAN;

VAR FileName, CommandString: Str40;
    FirstCommand: BOOLEAN;
    TempInt, xResult: INTEGER;

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
                Val (FileName, TempInt, xResult);
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
        IF CommandString = 'DUPECHECK'      THEN DupeCheckOnInactiveRadio;

        IF CommandString = 'ENABLECW'       THEN CWEnable := True;

        IF CommandString = 'EXCHANGERADIOS' THEN ExchangeRadios; {KK1L: 6.71}

        IF CommandString = 'EXECUTE'        THEN
            BEGIN
            RunningConfigFile := True;
            ClearDupeSheetCommandGiven := False;

            FirstCommand := False;

            IF FileExists (FileName) THEN
                LoadInSeparateConfigFile (FileName, FirstCommand, MyCall);

            IF ClearDupeSheetCommandGiven THEN
                BEGIN
                MoveEditableLogIntoLogFile;
                UpdateTotals;
                Sheet.ClearDupeSheet;
                END;

            RunningConfigFile := False;
            END;

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

        IF CommandString = 'LOGLASTCALL'    THEN LogLastCall;

        IF CommandString = 'QSY'            THEN CWMessageCommand := CWCommandQSY;

        IF CommandString = 'SAPMODE'        THEN CWMessageCommand := CWCommandSAPMode;

        IF Copy (CommandString, 1, 5) = 'SPEED' THEN
            BEGIN
            Delete (CommandString, 1, 5);

            IF StringIsAllNumbers (CommandString) THEN
                BEGIN
                Val (CommandString, TempInt, xResult);
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

        IF CommandString = 'SWAPRADIOS'     THEN
            BEGIN
            SwapRadios;
            {KK1L: 6.73 Used to use a variable CheckSpeed}
            Str (SpeedMemory[InactiveRadio], SpeedString);
            END;

        IF CommandString = 'TOGGLECW'        THEN ToggleCW (False);
        IF CommandString = 'TOGGLEMODES'     THEN ToggleModes;
        IF CommandString = 'TOGGLESTEREOPIN' THEN ToggleStereoPin; {KK1L: 6.71}
        END;

    CommandUseInactiveRadio := FALSE; {KK1L: 6.73 Put back to normal so other calls default to active radio}
    END;



PROCEDURE DoAltZ (VAR WindowString: Str80; VAR KeyChar: CHAR);

    BEGIN
    IF ActiveWindow = ExchangeWindow THEN
        BEGIN
        ExchangeWindowString := InitialExchangeEntry (CallWindowString);
        ClrScr;
        Write (ExchangeWindowString);

        {TR6.74 - because InitialExchangeEntry no longer does this }

        IF InitialExchangeOverwrite THEN
            InitialExchangePutUp := ExchangeWindowString <> '';

        IF InitialExchangeCursorPos = AtStart THEN
            GoToXY (1, 1)
        ELSE
            GoToXY (1, Length (ExchangeWindowString) + 1);

        ExchangeWindowCursorPosition := WhereY;
        ShowStationInformation (CallWindowString);
        END
    ELSE
        BEGIN
        IF ActiveWindow = CallWindow THEN
            ExchangeWindowString := InitialExchangeEntry (WindowString)
        ELSE
            ExchangeWindowString := InitialExchangeEntry (CallWindowString);

        {TR6.74 - because InitialExchangeEntry no longer does this }

        IF InitialExchangeOverwrite THEN
            InitialExchangePutUp := ExchangeWindowString <> '';

        SaveSetAndClearActiveWindow (ExchangeWindow);
        Write (ExchangeWindowString);

        IF InitialExchangeCursorPos = AtStart THEN
            GoToXY (1, 1)
        ELSE
            GoToXY (1, Length (ExchangeWindowString) + 1);

        ExchangeWindowCursorPosition := WhereY;
        RestorePreviousWindow;

        ShowStationInformation (WindowString);

        KeyChar := ControlX;
        END;
    END;



PROCEDURE WindowEditor (VAR WindowString: Str80;
                        VAR KeyChar: CHAR;
                        VAR ExtendedKeyChar: CHAR);

{ This procedure is inteded to do all the editing functions for the
  given window string.  It is assumed that the window is set up already
  for the string.  If there is some inital value for the string, pass it
  in WindowString, otherwise make it a null string.  If a return, a vertical
  cursor move, escape with no input, or a function key is entered, the
  procedure will halt and return the character that was received.  If the
  key is a function key, only the null character is read.

  A new change is to not do anymore dupechecks in this procedure.  If a
  dupe check is attempted, the procedure will exit with the KeyChar =
  SpaceBar.  Dupe checks are attempted in one of two ways:  If the
  space bar is pressed and the active window is the call window or if the
  space bar is pressed and the active window is the exchange window and
  there is no data entered yet.                                          }


VAR Number, xResult, CursorPosition, CharPointer, InsertCursorPosition: INTEGER;
    Freq: LONGINT;
    PreviousCursorChar: CHAR;
    TempString: String [255];
    PacketSpotCall, BandMapCall: CallString;
    FileName, FirstString: Str80;
    TempBand: BandType;
    TempFreq: LONGINT;
    TempMode: ModeType;
    Dest: BYTE;
    TempExchange: ContestExchange;
    NumberString: Str20;
    FirstCommand, FootSwitchPressed: BOOLEAN;
    RealFreq: REAL;
    TestByte, TimeOut: BYTE;
    VFOString: Str80; {KK1L: 6.73 added VFOString}
    VFOChar: Char; {KK1L 6.73}
    RadioToSet: RadioType; {KK1L: 6.73}


    BEGIN
    ExtendedKeyChar := Chr (0);

    IF (ControlBMemory <> '') OR (OnDeckCall <> '') THEN
        BEGIN
        ClrScr;
        WindowString := ControlBMemory + OnDeckCall;

        IF ActiveWindow = CallWindow THEN
            CallWindowString := WindowString;

        Write (WindowString);

        { Added in 6.59 }

        IF (OnDeckCall <> '') AND InactiveRigCallingCQ THEN
            BEGIN
            IF ActiveMode = CW THEN REPEAT millisleep UNTIL NOT CWStillBeingSent;
            SwapRadios;
            SetUpToSendOnActiveRadio;
            InactiveRigCallingCQ := False;
            END;

        ControlBMemory := '';
        OnDeckCall := '';
        END;

    REPEAT
        IF SpecialCommand = SendF1Message THEN
            BEGIN
            {KK1L: We get in here when hitting enter from S&P with nothing in either window!}
            InactiveRigCallingCQ := False;
            SetUpToSendOnActiveRadio;

            IF MessageEnable AND NOT ((CWTone = 0) AND Debug) THEN
                SendFunctionKeyMessage (F1, CQOpMode);

            {KK1L: 6.68 Set LastCQ stuff when calling CQ by the enter key with no call entered}
            IF FrequencyDisplayed THEN
                BEGIN
                LastCQFrequency := DisplayedFrequency;
                LastCQMode      := ActiveMode;
                END
            ELSE
                LastCQFrequency := 0;

            SpecialCommand := NoSpecialCommand;
            END;

        CheckForRemovedDupeSheetWindow;

        {IF ((ActiveWindow = CallWindow) AND (WindowString = '')) OR
           ((ActiveWindow = ExchangeWindow) AND (CallWindowString = '')) THEN
               OkayToPutUpBandMapCall := True;} {KK1L: 6.73 To fix call popping up after working them.}

        IF (ActiveMultiPort <> nil) AND (MultiInfoMessage <> '') THEN
            MarkTime (MultiInfoMessageTimeout);

        REPEAT
            IF (ActiveMultiPort <> nil) AND (MultiInfoMessage <> '') THEN
                IF MicroTimeElapsed (MultiInfoMessageTimeout) > 10000 THEN
                    IF NOT K1EANetworkEnable THEN
                        BEGIN
                        SendMultiInfoMessage (ActiveBand, ActiveMode, 'OpeRATor is asleep!');
                        MarkTime (MultiInfoMessageTimeout);
                        END;

            CheckEverything (KeyChar, WindowString); {KK1L: NOTE UpdateTimeAndRateDisplay, polling, etc}

            if (FootSwitchMode = CWGrant) then
                FootSwitchPressed := Footsw.getState
            else
                FootSwitchPressed := Footsw.getDebouncedState;

            IF FootSwitchPressed THEN
                BEGIN
                CASE FootSwitchMode OF

                    FootSwitchF1:
                        BEGIN
                        IF OpMode = CQOpMode THEN
                            SendFunctionKeyMessage (F1, CQOpMode)
                        ELSE
                            ProcessExchangeFunctionKey (F1);

                        FootSwitchPressed := False;
                        END;

                    FootSwitchControlEnter:
                        BEGIN
                        FootSwitchPressed := False;
                        BeSilent := True;
                        KeyChar := CarriageReturn;
                        Exit;
                        END;

                    FootSwitchDisabled:
                        FootSwitchPressed := False;

                    FootSwitchLastCQFreq:
                        BEGIN
                        GoToLastCQFrequency;
                        FootSwitchPressed := False;
                        END;

                    FootSwitchNextBandMap:
                        BEGIN
                        GoToNextBandMapFrequency;
                        FootSwitchPressed := False;
                        END;

                    FootSwitchNextMultBandMap: {KK1L: 6.68}
                        BEGIN
                        GoToNextMultBandMapFrequency;
                        FootSwitchPressed := False;
                        END;

                    FootSwitchNextDisplayedBandMap: {KK1L: 6.64}
                        BEGIN
                        GoToNextDisplayedBandMapFrequency;
                        FootSwitchPressed := False;
                        END;

                    FootSwitchNextMultDisplayedBandMap: {KK1L: 6.68}
                        BEGIN
                        GoToNextMultDisplayedBandMapFrequency;
                        FootSwitchPressed := False;
                        END;

                    FootSwitchDupeCheck:
                        BEGIN
                        DupeCheckOnInactiveRadio;
                        FootSwitchPressed := False;
                        END;

                    StartSending:
                        IF ActiveMode = CW THEN
                            BEGIN
                            KeyChar := StartSendingNowKey;
                            Exit;
                            END;

                    SwapRadio:
                        BEGIN
                        SwapRadios;
                        FootSwitchPressed := False;
                        Str (SpeedMemory[InactiveRadio], SpeedString); {KK1L: 6.73 Used to use a variable CheckSpeed}
                        END;

                    Normal:
                        BEGIN
                        IF TwoRadioState <> TwoRadiosDisabled THEN
                            CheckTwoRadioState (FootswitchWasPressed);

                        FootSwitchPressed := False;
                        END;
                    END;

                END;

            {IF (AutoSAPEnable) AND (OpMode = CQOpMode) THEN                    }
            {    IF ((ActiveRadio = RadioOne) AND RadioOnTheMove[RadioOne]) OR  }
            {       ((ActiveRadio = RadioTwo) AND RadioOnTheMove[RadioTwo]) THEN}
            IF (AutoSAPEnable) AND (OpMode = CQOpMode) THEN
                IF (RadioMovingInBandMode[ActiveRadio]) THEN
                    BEGIN
                    KeyChar := TabKey;
                    Exit;
                    END;

            Wait (6);

            IF (ParamCount > 0) AND (ParamStr (1) = 'EXIT') THEN ExitProgram;

        UNTIL NewKeyPressed OR ReadInLog OR Debug OR FootSwitchPressed;

        IF FootSwitchPressed THEN
            BEGIN
            CASE FootSwitchMode OF
                QSONormal: BEGIN
                           KeyChar := CarriageReturn;
                           Exit;
                           END;

                QSOQuick:  BEGIN
                           IF LookingForCQExchange THEN
                               KeyChar := QuickQSLKey1
                           ELSE
                               KeyChar := CarriageReturn;
                           Exit;
                           END;
                END;
            END;

        IF NOT (ReadInLog OR Debug) THEN
            BEGIN
            KeyChar := Upcase (NewReadKey);

            IF KeyChar IN AccentedChars THEN {KK1L: 6.72 This will uppercase the accented characters}
                BEGIN
                IF KeyChar = Chr(132) THEN KeyChar := Chr(142);
                IF KeyChar = Chr(134) THEN KeyChar := Chr(143);
                IF KeyChar = Chr(148) THEN KeyChar := Chr(153);
                END;
            END;

        CursorPosition := WhereX;

        IF NOT (((KeyChar >= '0') AND (KeyChar <= '9')) OR
                ((KeyChar >= 'A') AND (KeyChar <= 'Z'))) THEN
                   IF ActiveWindow = ExchangeWindow THEN
                       InitialExchangePutUp := False;

        IF (OpMode = CQOpMode) AND (ActiveMode = CW) THEN
            IF (ActiveWindow = CallWindow) THEN
                IF Length (CallWindowString) >= 1 THEN
                    IF KeyChar = StartSendingNowKey THEN
                        Exit;

{ Add attempt to show domestic multiplier status as it is entered in }

        IF (ActiveWindow = ExchangeWindow) AND (Length (WindowString) >= 2) AND DoingDomesticMults THEN
            BEGIN
            { GetLastString is an attempt to make JOE TX work }

            TempExchange.QTHString := GetLastString (WindowString);

            IF DoingDomesticMults AND FoundDomesticQTH (TempExchange) THEN
                VisibleLog.ShowDomesticMultiplierStatus (TempExchange.DomesticQTH);
            END;

        CASE KeyChar OF

          '"': BEGIN
               RITEnable := False;
               ClearRIT;

               IF ActiveMultiPort <> nil THEN
                   BEGIN
                   TempString := QuickEditResponse ('Enter (BAND MESSAGE) : ', 50);
                   GetRidOfPrecedingSpaces (TempString);

                   IF (TempString <> '') AND (TempString <> EscapeKey) THEN
                       BEGIN
                       FirstString := UpperCase (PrecedingString (TempString, ' '));

                       Dest := $FF;

                       IF FirstString = '160' THEN Dest := MultiBandAddressArray [Band160];
                       IF FirstString =  '80' THEN Dest := MultiBandAddressArray [Band80];
                       IF FirstString =  '40' THEN Dest := MultiBandAddressArray [Band40];
                       IF FirstString =  '30' THEN Dest := MultiBandAddressArray [Band30];
                       IF FirstString =  '20' THEN Dest := MultiBandAddressArray [Band20];
                       IF FirstString =  '17' THEN Dest := MultiBandAddressArray [Band17];
                       IF FirstString =  '15' THEN Dest := MultiBandAddressArray [Band15];
                       IF FirstString =  '12' THEN Dest := MultiBandAddressArray [Band12];
                       IF FirstString =  '10' THEN Dest := MultiBandAddressArray [Band10];
                       IF FirstString =   '6' THEN Dest := MultiBandAddressArray [Band6];
                       IF FirstString =   '2' THEN Dest := MultiBandAddressArray [Band2];
                       IF FirstString = '222' THEN Dest := MultiBandAddressArray [Band222];
                       IF FirstString = '432' THEN Dest := MultiBandAddressArray [Band432];
                       IF FirstString = '902' THEN Dest := MultiBandAddressArray [Band902];
                       IF FirstString = '1GH' THEN Dest := MultiBandAddressArray [Band1296];
                       IF FirstString = '2GH' THEN Dest := MultiBandAddressArray [Band2304];

                       IF Dest <> $FF THEN RemoveFirstString (TempString);

                       IF K1EANetworkEnable THEN
                           SendMultiMessage ('T' + K1EAStationID + '0 ' + TempString)
                       ELSE
                           SendMultiCommand (MultiBandAddressArray [ActiveBand],
                                             Dest,
                                             MultiTalkMessage,
                                             TempString);
                       END;
                   END;

               RITEnable := True;
               END;

          EscapeKey:
              BEGIN
//quick and dirty escape kills rtty -- also backs up one step, but
//that's why it's dirty
   if ((ActiveMode = Digital) and (ActiveRttyPort <> nil)) then
       activerttyport.putchar(chr(27));
              {KK1L: 6.73 For SO2R will force redisplay SO2R info for BandMapBlinkinCall}
              IF (OpMode = CQOpMode) AND (BandMapBand = BandMemory[InactiveRadio]) THEN
                  CallLastTimeIWasHere := '';

              IF (TwoRadioState = StationCalled) THEN
                  BEGIN
                  CheckTwoRadioState (EscapePressed);
                  WindowString := '';
                  ClrScr;
                  NameCallsignPutUp := '';
                  SetUpToSendOnActiveRadio;
                  Exit;
                  END;

              IF DualingCQState <> NoDualingCQs THEN
                  BEGIN
                  DualingCQState := NoDualingCQs;
                  QuickDisplay ('AutoCQ function aborted with ESCAPE key.  Press again to stop CQ');
                  END
              ELSE
                  IF ((ActiveMode = CW) AND CWStillBeingSent) OR
                     ((ActiveMode = Phone) AND DVPEnable AND (DVPMessagePlaying OR DVKMessagePlaying)) OR
                     (ActiveMode = Phone) AND DVKEnable AND
                      DVKRecentlyStarted (400) THEN  { Within 4 seconds }
                          BEGIN
                          IF ActiveMode = CW THEN
                              BEGIN
                              FlushCWBufferAndClearPTT;
                              DualingCQState := NoDualingCQs;
                              QuickDisplay ('ENTER a callsign.  SPACE for dupecheck.  Alt-H for help.');
                              END
                          ELSE
                              IF DVPEnable AND DVPActive THEN
                                  BEGIN
                                  DVPStopPlayback;

                                  TimeOut := 0;

                                  IF ActiveMode = Phone THEN
                                      REPEAT
                                          Wait (5);
                                          Inc (TimeOut);
                                      UNTIL (NOT DVPMessagePlaying) OR (TimeOut > 60);
                                  END
                              ELSE
                                  SendDVKMessage('DVK0'); {Kills message}
                          END
                      ELSE
                          BEGIN
                          IF WindowString = '' THEN
                              BEGIN
                              InactiveRigCallingCQ := False;
                              Exit;
                              END;

                          IF ActiveWindow = CallWindow THEN
                              EscapeDeletedCallEntry := WindowString
                          ELSE
                              EscapeDeletedExchangeEntry := WindowString;

                          WindowString := '';
                          ClrScr;
                          NameCallsignPutUp := '';
                          END;
                  END;

          ControlA:
              IF CursorPosition > 1 THEN
                  BEGIN
                  REPEAT
                      Dec (CursorPosition);
                      PreviousCursorChar := WindowString [CursorPosition - 1];
                  UNTIL ((WindowString [CursorPosition] <> ' ') AND
                         (PreviousCursorChar = ' ')) OR (CursorPosition = 1);
                  GoToXY (CursorPosition, WhereY);
                  END;

          ControlB:
              IF (ActivePacketPort <> nil) OR
                 (ActiveMultiPort <> nil) OR
                 (ActiveRTTYPort <> nil) THEN
                  PacketWindow;

          ControlC: Exit;

          ControlD:
              IF CursorPosition <= Length (WindowString) THEN
                  GoToXY (CursorPosition + 1, WhereY);

          ControlE: IF (MultiInfoMessage <> '') OR K1EANetworkEnable THEN
                        DisplayMultiMessages
                    ELSE
                        Exit;

          ControlF:
              IF CursorPosition <= Length (WindowString) THEN
                  BEGIN
                  REPEAT
                      PreviousCursorChar := WindowString [CursorPosition];
                      Inc (CursorPosition);
                  UNTIL ((WindowString [CursorPosition] <> ' ') AND
                         (PreviousCursorChar = ' ')) OR (CursorPosition = Length (WindowString) + 1);
                  GoToXY (CursorPosition, WhereY);
                  END;

          ControlG:
              IF ActiveDomesticMult = GridSquares THEN
                  VisibleLog.DisplayGridMap (ActiveBand, ActiveMode)
              ELSE
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

          ControlJ:
             if not ctrlenter then
//              IF Port [$60] = $24 THEN  { Control-J, not ControlEnter }
//  if true then
                  BEGIN
                  ProcessConfigurationInput;
                  RemoveWindow (QuickCommandWindow);

                  { DisplayAutoSendCharacterCount;  Not necessary ? }
                  END
              ELSE { Control-Enter }
                  BEGIN
                  {KK1L: 6.73 Added this block to use ControlEnter to program Inactive Radio frequency}

                  VFOString := WindowString;
                  Delete (VFOString, 1, Length (VFOString)-1); {KK1L: 6.73 Get last character as VFO to set}
                  VFOChar := VFOString[1];
                  TempString := WindowString;
                  Delete (TempString, Length (TempString), 1); {KK1L: 6.73 Remove last character}

                  IF ((VFOString = 'A') OR (VFOString = 'B')) AND
                     (StringIsAllNumbersOrDecimal (TempString)) THEN {KK1L: 6.73 Strip VFO char from end}
                      WindowString := TempString {KK1L: 6.73 Remove last character}
                  ELSE
                      VFOChar := 'A'; {KK1L: 6.73 Assume setting A VFO}

                  IF (StringIsAllNumbersOrDecimal (WindowString)) AND
                     (ActiveWindow = CallWindow) AND
                     (Length (WindowString) >= 3) THEN
                         BEGIN
                         IF Length (WindowString) = 3 THEN
                             BEGIN
                             CASE BandMemory[InactiveRadio] OF
                                 Band160: TempString := '1'   + WindowString;
                                 Band80:  TempString := '3'   + WindowString;
                                 Band40:  TempString := '7'   + WindowString;
                                 Band30:  TempString := '10'  + WindowString;
                                 Band20:  TempString := '14'  + WindowString;
                                 Band17:  TempString := '18'  + WindowString;
                                 Band15:  TempString := '21'  + WindowString;
                                 Band12:  TempString := '24'  + WindowString;
                                 Band10:  TempString := '28'  + WindowString;
                                 Band6:   TempString := '50'  + WindowString;
                                 ELSE     TempString := '144' + WindowString;
                                 END;
                             END
                         ELSE
                             TempString := WindowString;

                         IF StringHas (TempString, '.') THEN
                             BEGIN
                             Val (TempString, RealFreq, xResult);
                             Freq := Round (RealFreq * 1000.0);
                             END
                         ELSE
                             Val (TempString + '000', Freq, xResult);

                         TempMode := ModeMemory[InactiveRadio];

                         IF xResult = 0 THEN
                             BEGIN
                             PutRadioOutOfSplit(InactiveRadio);
                             SetRadioFreq (InactiveRadio, Freq, TempMode, VFOChar); {KK1L: 6.73}
                             END;

                         CalculateBandMode (Freq, TempBand, TempMode);

                         BandMemory [InactiveRadio] := TempBand; {KK1L: 6.73 NOTE calculated above}

                         WindowString := '';
                         ClrScr;
                         END
                  ELSE {KK1L: 6.73 This is what used to be here}
                      BEGIN
                      BeSilent := True;
                      KeyChar := CarriageReturn;
                      Exit;
                      END;
                  END;

          ControlK: BEGIN
                    {KK1L: 6.72 Made the text a little more clear}
                    TempString := QuickEditResponse ('Do you really want to clear the dupesheet? (Spell Y E S/NO ) : ', 3);

                    IF UpperCase (TempString) = 'YES' THEN
                        BEGIN
                        MoveEditableLogIntoLogFile;
                        UpdateTotals;
                        Sheet.ClearDupeSheet;
                        QuickDisplay ('Sheet cleared!  To restore, delete RESTART.BIN and start program over.');
                        END;
                    END;

          ControlL: BEGIN
                    ViewLogFile;
                    VisibleDupeSheetRemoved := True;
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

                  PushLogStringIntoEditableLogAndLogPopedQSO (AddedNoteString, True);
                  END;

              RITEnable := True;
              END;

          ControlO: VisibleLog.ShowMissingMultiplierReport;

          ControlP:
              BEGIN
              IF ActiveWindow = CallWindow THEN
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

              IF ActiveRotatorPort <> nil THEN
                  RotorControl (LastHeadingShown);
              END;

          ControlQ:
              IF QTCsEnabled THEN
                  BEGIN
                  IF (ActiveWindow = ExchangeWindow) THEN
                      WAEQTC (CallWindowString)
                  ELSE
                      WAEQTC (WindowString);

                  DisplayTotalScore (TotalScore);

                  IF QTCNote <> '' THEN
                      BEGIN
                      PushLogStringIntoEditableLogAndLogPopedQSO ('; ' + QTCNote, True);
                      QTCNote := '';
                      END;
                  END;

          ControlR: IF ActiveWindow = CallWindow THEN
                        BEGIN
                        IF EscapeDeletedCallEntry <> '' THEN
                            BEGIN
                            WindowString := EscapeDeletedCallEntry;
                            Write (WindowString);
                            END;
                        END
                    ELSE
                        IF EscapeDeletedExchangeEntry <> '' THEN
                            BEGIN
                            WindowString := EscapeDeletedExchangeEntry;
                            Write (WindowString);
                            END;

          ControlS: IF CursorPosition > 1 THEN
                        GoToXY (CursorPosition - 1, WhereY);

          ControlT:
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

          ControlU: IF ActivePacketPort <> nil THEN
                        BEGIN
                        IF PacketMemoryRequest THEN
                            BEGIN
                            WindowString := CallWindowString;
                            KeyChar := ControlU;  { Packet spot setup to work }
                            Exit;
                            END;
                        END
                    ELSE
                        IF ActiveRTTYPort <> nil THEN
                            TalkToRTTYPort;

          ControlV:
              BEGIN
              RunningConfigFile := True;
              ClearDupeSheetCommandGiven := False;

              FileName := QuickEditResponse ('Enter config file to execute : ', 20);

              IF (FileName <> '') AND (FileName <> EscapeKey) THEN
                  BEGIN
                  FirstCommand := False;

                  IF FileExists (FileName) THEN
                      LoadInSeparateConfigFile (FileName, FirstCommand, MyCall)
                  ELSE
                      QuickDisplayError('File not found!'); {KK1L: 6.71}

                  IF ClearDupeSheetCommandGiven THEN
                      BEGIN
                      MoveEditableLogIntoLogFile;
                      UpdateTotals;
                      Sheet.ClearDupeSheet;
                      END;

                  RunningConfigFile := False;

                  IF OpMode = CQOpMode THEN {KK1L: 6.71 Forces display of menu with new CFG file load.}
                    PutUpCQMenu
                  ELSE
                    PutUpExchangeMenu;
                  END;
              END;

          ControlW: BEGIN
                    IF ActiveWindow = CallWindow THEN
                        EscapeDeletedCallEntry := WindowString
                    ELSE
                        EscapeDeletedExchangeEntry := WindowString;

                    WindowString := '';
                    ClrScr;
                    NameCallsignPutUp := '';
                    END;

          ControlX: Exit;

          ControlY: UpdateBlinkingBandMapCall;

          ControlZ: Exit;


          ControlDash: IF GetCQMemoryString (ActiveMode, AltF1) = '' THEN {KK1L: 6.73 Added mode}
                           QuickDisplay ('No CQ message programmed into CQ MEMORY AltF1.')
                       ELSE
                           BEGIN
                           IF DualingCQState = NoDualingCQs THEN
                               BEGIN
                               QuickDisplay ('Dualing CQ active.  Start entering call or Control-Dash/Escape to stop.');
                               SendFunctionKeyMessage (AltF1, CQOpMode);
                               DualingCQState := DualSendingCQ;

                               IF (ActiveMode = Phone) AND DVPActive THEN
                                   BEGIN
                                   TimeOut := 0;

                                   REPEAT
                                       Wait (5);
                                       Inc (TimeOut);
                                   UNTIL DVPMessagePlaying OR (TimeOut > 60) OR DVKMessagePlaying;
                                   END;
                               END
                           ELSE
                               BEGIN
                               DualingCQState := NoDualingCQs;
                               QuickDisplay ('Dual CQ mode cancelled by Control-Dash');
                               END;
                           END;

          ControlBackSlash:
              BEGIN
              BeSilent := True;
              KeyChar := CarriageReturn;
              Exit;
              END;

          ControlRightBracket:
              DisplayCT1BOHData;

          CarriageReturn:  {KK1L: note: This is where I can trap commands from the call window!}

             if ctrlenter then
                  BEGIN
                  {KK1L: 6.73 Added this block to use ControlEnter to program Inactive Radio frequency}

                  VFOString := WindowString;
                  Delete (VFOString, 1, Length (VFOString)-1); {KK1L: 6.73 Get last character as VFO to set}
                  VFOChar := VFOString[1];
                  TempString := WindowString;
                  Delete (TempString, Length (TempString), 1); {KK1L: 6.73 Remove last character}

                  IF ((VFOString = 'A') OR (VFOString = 'B')) AND
                     (StringIsAllNumbersOrDecimal (TempString)) THEN {KK1L: 6.73 Strip VFO char from end}
                      WindowString := TempString {KK1L: 6.73 Remove last character}
                  ELSE
                      VFOChar := 'A'; {KK1L: 6.73 Assume setting A VFO}

                  IF (StringIsAllNumbersOrDecimal (WindowString)) AND
                     (ActiveWindow = CallWindow) AND
                     (Length (WindowString) >= 3) THEN
                         BEGIN
                         IF Length (WindowString) = 3 THEN
                             BEGIN
                             CASE BandMemory[InactiveRadio] OF
                                 Band160: TempString := '1'   + WindowString;
                                 Band80:  TempString := '3'   + WindowString;
                                 Band40:  TempString := '7'   + WindowString;
                                 Band30:  TempString := '10'  + WindowString;
                                 Band20:  TempString := '14'  + WindowString;
                                 Band17:  TempString := '18'  + WindowString;
                                 Band15:  TempString := '21'  + WindowString;
                                 Band12:  TempString := '24'  + WindowString;
                                 Band10:  TempString := '28'  + WindowString;
                                 Band6:   TempString := '50'  + WindowString;
                                 ELSE     TempString := '144' + WindowString;
                                 END;
                             END
                         ELSE
                             TempString := WindowString;

                         IF StringHas (TempString, '.') THEN
                             BEGIN
                             Val (TempString, RealFreq, xResult);
                             Freq := Round (RealFreq * 1000.0);
                             END
                         ELSE
                             Val (TempString + '000', Freq, xResult);

                         TempMode := ModeMemory[InactiveRadio];

                         IF xResult = 0 THEN
                             BEGIN
                             PutRadioOutOfSplit(InactiveRadio);
                             SetRadioFreq (InactiveRadio, Freq, TempMode, VFOChar); {KK1L: 6.73}
                             END;

                         CalculateBandMode (Freq, TempBand, TempMode);

                         BandMemory [InactiveRadio] := TempBand; {KK1L: 6.73 NOTE calculated above}

                         WindowString := '';
                         ClrScr;
                         END
                  ELSE {KK1L: 6.73 This is what used to be here}
                      BEGIN
                      BeSilent := True;
                      KeyChar := CarriageReturn;
                      Exit;
                      END;
//                  END;
              end
              else
              BEGIN //open 1




              BeSilent := False;

              IF WindowString = 'DEBUG' THEN
                  BEGIN
                  Debug := True;
                  WindowString := '';
                  ClrScr;
                  END;

              VFOString := WindowString;                   {KK1L: 6.73}
              Delete (VFOString, 1, Length (VFOString)-1); {KK1L: 6.73 Get last character as VFO to set}
              VFOChar := VFOString[1];
              TempString := WindowString;                  {KK1L: 6.73}
              Delete (TempString, Length (TempString), 1);  {KK1L: 6.73 Remove last character}

              IF ((VFOString = 'A') OR (VFOString = 'B')) AND
                 (StringIsAllNumbersOrDecimal (TempString)) THEN {KK1L: 6.73 Strip VFO char from end}
                  WindowString := TempString {KK1L: 6.73 Remove last character}
              ELSE
                  VFOChar := 'A'; {KK1L: 6.73 Assume setting A VFO}

              IF (StringIsAllNumbersOrDecimal (WindowString)) AND
                 (ActiveWindow = CallWindow) AND
                 (Length (WindowString) >= 3) THEN
                  BEGIN
                  IF Length (WindowString) = 3 THEN
                      BEGIN
                      CASE ActiveBand OF
                          Band160: TempString := '1'   + WindowString;
                          Band80:  TempString := '3'   + WindowString;
                          Band40:  TempString := '7'   + WindowString;
                          Band30:  TempString := '10'  + WindowString;
                          Band20:  TempString := '14'  + WindowString;
                          Band17:  TempString := '18'  + WindowString;
                          Band15:  TempString := '21'  + WindowString;
                          Band12:  TempString := '24'  + WindowString;
                          Band10:  TempString := '28'  + WindowString;
                          Band6:   TempString := '50'  + WindowString;
                          ELSE     TempString := '144' + WindowString;
                          END;
                      END
                  ELSE
                      TempString := WindowString;

                  IF StringHas (TempString, '.') THEN
                      BEGIN
                      Val (TempString, RealFreq, xResult);
                      Freq := Round (RealFreq * 1000.0);
                      END
                  ELSE
                      Val (TempString + '000', Freq, xResult);

                  IF xResult = 0 THEN
                      BEGIN
                      PutRadioOutOfSplit (ActiveRadio);
                      SetRadioFreq (ActiveRadio, Freq, ActiveMode, VFOChar); {KK1L: 6.73}
                      RadioSetFreq := Freq;
                      END;

                  TempMode := ActiveMode;

                  CalculateBandMode (Freq, TempBand, TempMode);

                  ActiveBand := TempBand;

                  IF ActiveRadio = RadioOne THEN
                      BandMemory [RadioOne] := ActiveBand
                  ELSE
                      BandMemory [RadioTwo] := ActiveBand;

                  DisplayBandMode (ActiveBand, ActiveMode, False);
                  WindowString := '';
                  ClrScr;
                  END
              ELSE
                  IF (Copy (WindowString, 1, 1) = '\') AND
                     (ActiveWindow = CallWindow) THEN          { Added in 6.61 }
                      BEGIN
                      ProcessCallWindowCommand (WindowString); {KK1L: note: maybe commands would go in this proc?}
                      ClrScr;
                      WindowString := '';
                      END
                  ELSE
                      Exit;
              END; //close 1


          NullKey:
              BEGIN
              ExtendedKeyChar := NewReadKey;

              CASE ExtendedKeyChar OF
                  Alt1: IF BackCopyEnable THEN
                            PlayLastSeconds (2)
                        ELSE
                            IF IncrementTimeEnable THEN IncrementTime (1);

                  Alt2: IF BackCopyEnable THEN
                            PlayLastSeconds (3)
                        ELSE
                            IF IncrementTimeEnable THEN IncrementTime (2);

                  Alt3: IF BackCopyEnable THEN
                            PlayLastSeconds (4)
                        ELSE
                            IF IncrementTimeEnable THEN IncrementTime (3);

                  Alt4: IF BackCopyEnable THEN
                            PlayLastSeconds (8)
                        ELSE
                            IF IncrementTimeEnable THEN IncrementTime (4);

                  Alt5: IF BackCopyEnable THEN
                            PlayLastSeconds (16)
                        ELSE
                            IF IncrementTimeEnable THEN IncrementTime (5);

                  Alt6: IF BackCopyEnable THEN
                            BEGIN
                            SaveBackCopy (TotalContacts + 1, 5);
                            LogBackCopy (5);
                            END
                        ELSE
                            IF IncrementTimeEnable THEN IncrementTime (6);

                  Alt7: IF BackCopyEnable THEN
                            BEGIN
                            SaveBackCopy (TotalContacts + 1, 10);
                            LogBackCopy (10);
                            END
                        ELSE
                            IF IncrementTimeEnable THEN IncrementTime (7);

                  Alt8: IF BackCopyEnable THEN
                            BEGIN
                            SaveBackCopy (TotalContacts + 1, 15);
                            LogBackCopy (15);
                            END
                        ELSE
                            IF IncrementTimeEnable THEN IncrementTime (8);

                  Alt9: IF BackCopyEnable THEN
                            BEGIN
                            SaveBackCopy (TotalContacts + 1, 20);
                            LogBackCopy (20);
                            END
                        ELSE
                            IF IncrementTimeEnable THEN IncrementTime (9);

                  Alt0: IF BackCopyEnable THEN
                            BEGIN
                            SaveBackCopy (TotalContacts + 1, 30);
                            LogBackCopy (30);
                            END
                        ELSE
                            IF IncrementTimeEnable THEN IncrementTime (10);

                  AltEqual:
                      IF (ActiveMode = Phone) AND DVPActive THEN
                          ReviewBackCopyFiles
                      ELSE
                          IF CWTone <> 0 THEN
                              BEGIN
                              OldCWTone := CWTone;
                              CWTone := 0;
                              AddStringToBuffer ('', CWTone);
                              LNoSound;
                              END
                          ELSE
                              BEGIN
                              IF OldCWTone = 0 THEN
                                  OldCWTone := 700;
                              CWTone := OldCWTone;
                              AddStringToBuffer ('', CWTone);
                              END;

                  AltA: IF (ActiveWindow = CallWindow) THEN
                            BEGIN
                            IF (WindowString = '') AND (LastSCPCall = '') THEN
                                SetAlarm
                            ELSE
                                VisibleLog.SuperCheckPartial (WindowString, False, ActiveRadio);
                                {KK1L: 6.73 Added ActiveRadio}
                            END
                        ELSE
                            IF (CallWindowString = '') AND (LastSCPCall = '')  THEN
                                SetAlarm
                            ELSE
                                VisibleLog.SuperCheckPartial (CallWindowString, False, ActiveRadio);
                                {KK1L: 6.73 Added ActiveRadio}


                  AltB: BEGIN
                        RememberFrequency;
                        BandUp;
                        END;

                  AltD: IF K1EANetworkEnable THEN
                            PassStationToCTNetwork
                        ELSE
                            DupeCheckOnInactiveRadio;

                  AltE: BEGIN
                        RITEnable := False;
                        VisibleLog.EditLog;
                        RITEnable := True;
                        UpdateTotals;
                        VisibleLog.ShowRemainingMultipliers;
                        VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);
                        DisplayTotalScore (TotalScore);
                        DisplayInsertMode (InsertMode);
                        DisplayNextQSONumber (TotalContacts + 1);
                        LastTwoLettersCrunchedOn := '';

                        IF VisibleDupeSheetEnable THEN
                            BEGIN
                            VisibleDupesheetChanged := True;
                            VisibleLog.DisplayVisibleDupeSheet (ActiveBand, ActiveMode);
                            END;
                        END;

                  AltF: SaveLogFileToFloppy;
                  AltG: SwapMultDisplay;

                  AltH: BEGIN
                        PutUpHelpMenu;
                        RestorePreviousWindow;
                        VisibleDupeSheetRemoved := True;
                        END;

                  AltI: IF ActiveWindow = ExchangeWindow THEN
                            BEGIN
                            IncrementASCIIInteger (ExchangeWindowString);
                            ClrScr;
                            Write (ExchangeWindowString);
                            END;

                  AltJ: BEGIN
                        MultiplierAlarm := NOT MultiplierAlarm;
                        IF MultiplierAlarm THEN Tone.DoABeep (BeepCongrats);
                        END;

                  AltK: ToggleCW (True);

                  AltL: BEGIN
                        RITEnable := False;
                        IF ActiveWindow = CallWindow THEN
                            VisibleLog.SearchLog (WindowString)
                        ELSE
                            VisibleLog.SearchLog (CallWindowString);
                        RITEnable := True;
                        END;

                  AltM: BEGIN
                        RememberFrequency;
                        ToggleModes;
                        DisplayAutoSendCharacterCount;
                        END;

                  AltN:
                      BEGIN
                      Freq := QuickEditFreq ('Enter transmit frequency (kiloHertz) : ', 10);

                      {KK1L: 6.73 -1 means ESC or null, otherwise if negative then CTRL-Enter used to enter.}
                      RadioToSet := ActiveRadio;

                      IF Freq < -2 THEN
                          BEGIN
                          Freq := Freq * (-1);
                          RadioToSet := InactiveRadio;
                          END;

                      IF (Freq > 1000) AND (Freq < 1000000) THEN
                          CASE BandMemory[RadioToSet] OF  {KK1L: 6.73 Was ActiveBand}
                              Band80: Freq := Freq +  3000000;
                              Band40: Freq := Freq +  7000000;
                              Band20: Freq := Freq + 14000000;
                              Band15: Freq := Freq + 21000000;
                              Band10: Freq := Freq + 28000000;
                              END;

                      IF Freq > 1000000 THEN
                          BEGIN
                          {SetRadioFreq (ActiveRadio, Freq, ActiveMode, 'B');}
                          {PutRadioIntoSplit (ActiveRadio);} {KK1L: 6.71 Moved here from before case for TS850 change}
                          SetRadioFreq (RadioToSet, Freq, ModeMemory[RadioToSet], 'B'); {KK1L: 6.73}
                          PutRadioIntoSplit (RadioToSet); {KK1L: 6.73}
                          SplitFreq := Freq;
                          END;
                      BandMapCursorFrequency := Freq; {KK1L: 6.68 Band map tracks transmit freq}
                      DisplayBandMap; {KK1L: 6.68}
                      END;

                  AltO: BEGIN
                        RITEnable := False;
                        AddReminder;
                        RITEnable := True;
                        END;

                  AltP: BEGIN
                        RITEnable := False;
                        shiftchange(0);
                        MemoryProgram;
                        if (shiftkeyenable = shift) then shiftchange(1);
                        if (shiftkeyenable = altshift) then shiftchange(2);
                        RITEnable := True;
                        VisibleLog.SetUpEditableLog;
                        END;

                  AltR: BEGIN
                        {KK1L: 6.73 Not implimented because George did not like it AT ALL!}
                        {IF (TwoRadioState = StationCalled) THEN }{KK1L: 6.73}
                        {    BEGIN }
                        {    DoABeep(Warning); }
                        {    QuickDisplay('You are working a station on the 2nd radio. Escape to cancel first!'); }
                        {    END }
                        {ELSE }
                            BEGIN
                            SwapRadios;
                            InactiveRigCallingCQ := False;
                            Str (SpeedMemory[InactiveRadio], SpeedString); {KK1L: 6.73 Used to use a variable CheckSpeed}
                            END;
                        END;

                  AltS: SetNewCodeSpeed;
                  AltT: TimeAndDateSet;

                  AltU: BEGIN
                        MoveEditableLogIntoLogFile;
                        UpdateTotals;
                        END;

                  AltV: BEGIN
                        RememberFrequency;
                        BandDown;
                        END;

                  AltW: BEGIN
                        WakeUpCount := 0;
                        SaveSetAndClearActiveWindow (QuickCommandWindow);
                        Write ('Wake up count reset to zero.');
                        DelayOrKeyPressed (2000);
                        RemoveAndRestorePreviousWindow;
                        END;

                  AltX: ExitProgram;

                  AltY: BEGIN
                        DeleteLastContact;
                        LastTwoLettersCrunchedOn := '';
                        END;

                  AltZ: BEGIN
                        DoAltZ (WindowString, KeyChar);
                        IF KeyChar = ControlX THEN Exit;
                        END;

                  AltDash:
                      BEGIN
                      IF AutoSendCharacterCount > 0 THEN
                          AutoSendEnable := NOT AutoSendEnable;

                      DisplayAutoSendCharacterCount;
                      END;

                  HomeKey: GoToXY (1, WhereY);

                  EndKey:  GoToXY (Length (WindowString) + 1, WhereY);

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

                      ShowPartialCallMults (WindowString);
                      END;

                  LeftArrow:
                      IF CursorPosition > 1 THEN
                          GoToXY (CursorPosition - 1, WhereY);

                  RightArrow:
                      IF CursorPosition <= Length (WindowString) THEN
                          GoToXY (CursorPosition + 1, WhereY);

                  PageUpKey: SpeedUp;
                  PageDownKey: SlowDown;

                  ControlPageDown: {KK1L: 6.73 Used CodeSpeedIncrement and change to array for speed}
                      BEGIN
                      IF SpeedMemory[InactiveRadio] > CodeSpeedIncrement THEN
                          SpeedMemory[InactiveRadio] := SpeedMemory[InactiveRadio] - CodeSpeedIncrement;
                      Str (SpeedMemory[InactiveRadio], SpeedString); {KK1L: 6.73 Set string to display speed for alt-d}
                      {KK1L: 6.73}
                      IF (AltDDupeCheckDisplayedCall <> '') AND
                         (OpMode = CQOpMode) AND
                         (ModeMemory[InactiveRadio] = CW) AND
                         (TwoRadioState = CallReady) THEN
                          BEGIN
                          SaveAndSetActiveWindow (DupeInfoWindow);
                          WriteLn (AltDDupeCheckDisplayedCall + ' OK!! at ' + SpeedString + ' WPM');
                          RestorePreviousWindow;
                          END;
                      END;

                  ControlPageUp: {KK1L: 6.73 Used CodeSpeedIncrement and change to array for speed}
                      BEGIN
                      IF SpeedMemory[InactiveRadio] < (99 - CodeSpeedIncrement) THEN
                          SpeedMemory[InactiveRadio] := SpeedMemory[InactiveRadio] + CodeSpeedIncrement;
                      Str (SpeedMemory[InactiveRadio], SpeedString); {KK1L: 6.73 Set string to display speed for alt-d}
                      {KK1L: 6.73}
                      IF (AltDDupeCheckDisplayedCall <> '') AND
                         (OpMode = CQOpMode) AND
                         (ModeMemory[InactiveRadio] = CW) AND
                         (TwoRadioState = CallReady) THEN
                          BEGIN
                          SaveAndSetActiveWindow (DupeInfoWindow);
                          WriteLn (AltDDupeCheckDisplayedCall + ' OK!! at ' + SpeedString + ' WPM');
                          RestorePreviousWindow;
                          END;
                      END;

                  ControlHome:
                      IF ActiveMultiPort <> nil THEN
                          DisplayMultiMessageBuffer;

                  ControlInsert: {KK1L: 6.65 Insert BM place holder entry}
                      BEGIN
                      AddBandMapPlaceHolder;
                      END;

                  ControlDelete: {KK1L: 6.65 Delete BM entry while logging}
                      BEGIN
                      IF (BandMapCursorData <> nil)
                          and (BandMapCursorData^.Frequency > 0) THEN
                          BEGIN
                          DisplayBandMap; {sets FoundCursor true if cursor on valid entry}
                          IF FoundCursor THEN
                              BEGIN
                              DeleteBandMapEntry (BandMapCursorData);
                              DisplayBandMap;
                              END;
                          END;
                      END;

                  ControlEnd:
                      IF BandMapEnable THEN
                          BEGIN
                          EditBandMap;

                          {KK1L: 6.65 Added NOT EscapeFromEditBandMap to not display call in window on escape}
                          IF (BandMapCursorData <> nil) AND (NOT EscapeFromEditBandMap) THEN
                              WITH BandMapCursorData^ DO
                                  BEGIN
                                  { Fix up the windows }

                                  BandMapCall := BandMapExpandedString (Call);

                                  IF (OpMode = SearchAndPounceOpMode) THEN
                                      BEGIN
                                      IF ActiveWindow = CallWindow THEN
                                          BEGIN
                                          WindowString := BandMapCall;
                                          ClrScr;
                                          Write (WindowString);
                                          END
                                      ELSE
                                          BEGIN
                                          ClrScr;
                                          WindowString := '';
                                          ExchangeWindowCursorPosition := 1;

                                          CallWindowString := BandMapCall;
                                          RestorePreviousWindow;
                                          ClrScr;
                                          Write (CallWindowString);
                                          ActivateExchangeWindow;
                                          KeyChar := TabKey;
                                          Exit;
                                          END;
                                      END
                                  ELSE
                                      IF (ActiveWindow = CallWindow) THEN
                                          BEGIN
                                          RemoveWindow (ExchangeWindow);
                                          ResetSavedWindowListAndPutUpCallWindow;
                                          ExchangeHasBeenSent := False;
                                          {WindowString := BandMapCall;} {KK1L: 6.73}
                                          {Write (WindowString);}        {KK1L: 6.73}
                                          {KeyChar := TabKey;}           {KK1L: 6.73}

                                          {KK1L: 6.73 For SO2R keeps CQMode when moving in inactive radio bandmap}
                                          IF BandMapBand = BandMemory[ActiveRadio] THEN
                                              BEGIN
                                              WindowString := BandMapCall;
                                              Write (WindowString);
                                              KeyChar := TabKey;
                                              END;
                                          Exit;
                                          END
                                      ELSE
                                          Begin
                                          Tone.DoABeep (Single);
                                          QuickDisplay ('Please ESCAPE out of this QSO before using bandmap');
                                          END;
                                  END;
                          END;


                  UpArrow:
                      IF ControlKeyPressed THEN
                          MoveGridMap (UpArrow)
                      ELSE
                          IF (ActiveWindow = CallWindow) AND (WindowString = '') THEN
                              BEGIN
                              RITEnable := False;
                              VisibleLog.EditLog;
                              RITEnable := True;
                              UpdateTotals;
                              VisibleLog.ShowRemainingMultipliers;
                              VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);
                              DisplayTotalScore (TotalScore);
                              DisplayInsertMode (InsertMode);
                              DisplayNextQSONumber (TotalContacts + 1);
                              LastTwoLettersCrunchedOn := '';
                              END
                          ELSE
                              Exit;


                  DownArrow: IF ControlKeyPressed THEN
                                 MoveGridMap (DownArrow)
                             ELSE
                                 Exit;

                  ControlLeftArrow,
                  ControlRightArrow:
                      IF GridMapCenter <> '' THEN
                          BEGIN
                          MoveGridMap (ExtendedKeyChar);
                          VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);
                          END;


                  InsertKey:
                      BEGIN
                      InsertMode := NOT InsertMode;
                      DisplayInsertMode (InsertMode);
                      END;

                  ELSE Exit;
                  END;  { of case }
              END;

          TabKey: Exit;

          BackSpace:
            IF CursorPosition > 1 THEN
                BEGIN
                FOR CharPointer := CursorPosition - 1 TO Length (WindowString) - 1 DO
                    WindowString [CharPointer] := WindowString [CharPointer + 1];
                WindowString [0] := Chr (Length (WindowString) - 1);

                ClrScr;
                Write (WindowString);
                GoToXY (CursorPosition - 1, WhereY);
                ShowPartialCallMults (WindowString);
                END;


         '-': BEGIN
              Freq := QuickEditFreq ('Enter transmit frequency (kiloHertz) : ', 10);

              {KK1L: 6.73 -1 means ESC or null, otherwise if negative then CTRL-Enter used to enter.}
              RadioToSet := ActiveRadio;
              IF Freq < -2 THEN
                  BEGIN
                  Freq := Freq * (-1);
                  RadioToSet := InactiveRadio;
                  END;

              IF (Freq > 1000) AND (Freq < 1000000) THEN
                  CASE BandMemory[RadioToSet] OF     {KK1L: 6.73 Was ActiveBand}
                      Band80: Freq := Freq +  3000000;
                      Band40: Freq := Freq +  7000000;
                      Band20: Freq := Freq + 14000000;
                      Band15: Freq := Freq + 21000000;
                      Band10: Freq := Freq + 28000000;
                      END;

              IF Freq > 1000000 THEN
                  BEGIN
                  {SetRadioFreq (ActiveRadio, Freq, ActiveMode, 'B'); }
                  {PutRadioIntoSplit (ActiveRadio);} {KK1L: 6.71 Moved here from before case for TS850 change}
                  SetRadioFreq (RadioToSet, Freq, ModeMemory[RadioToSet], 'B'); {KK1L: 6.73}
                  PutRadioIntoSplit (RadioToSet); {KK1L: 6.73}
                  SplitFreq := Freq;
                  END;
              END;


          ELSE        { of case }
            BEGIN
            IF KeyChar = SpaceBar THEN
                IF (ActiveWindow = CallWindow) OR (WindowString = '') THEN
                    BEGIN
                    RemoveWindow (PossibleCallWindow);
                    RemoveWindow (NameSentWindow);
                    RemoveWindow (QTCNumberWindow);
                    Exit;
                    END;

            IF KeyChar = PacketSpotKey THEN
                IF (ActivePacketPort <> nil) OR (ActiveMultiPort <> nil) THEN
                    IF NOT PacketSpotDisable THEN
                        BEGIN
                        IF ActiveWindow = CallWindow THEN
                            PacketSpotCall := WindowString
                        ELSE
                            PacketSpotCall := CallWindowString;

                        IF PacketSpotCall = '' THEN PacketSpotCall := VisibleLog.LastCallsign;

                        IF ActiveRadio = RadioOne THEN
                            TempFreq := StableRadio1Freq
                        ELSE
                            TempFreq := StableRadio2Freq;

                        IF TempFreq = 0 THEN
                            BEGIN
                            TempFreq := QuickEditFreq ('Enter spot frequency for ' + PacketSpotCall + ' : ', 10);
                            IF TempFreq <= 0 THEN Continue;
                            END;

                        CreateAndSendPacketSpot (PacketSpotCall, TempFreq);
                        END;

//            IF KeypadCWMemories AND ((KeyChar >= '0') AND (KeyChar <= '9')) THEN
//                BEGIN
//                TestByte := Port [$60];

//                IF (TestByte >= $47) AND (TestByte <= $52) THEN   { Keypad integer pressed }
//                    BEGIN
//                    SendFunctionKeyMessage (Chr (Ord (KeyChar) - Ord ('0') + Ord (ControlF1)), CQOpMode);
//                    Continue;
//                    END;
//                END;

            IF (KeyChar = 'I') AND (ActiveWindow = ExchangeWindow) AND
               (ActiveExchange = RSTQSONumberExchange) AND
               (StringIsAllNumbers (WindowString)) THEN
                   BEGIN { not null }
                   NumberString := GetLastString (WindowString);
                   Val (NumberString, Number, xResult);
                   IF xResult = 0 THEN
                       BEGIN
                       Inc (Number);
                       Str (Number, NumberString);
                       RemoveLastString (WindowString);
                       GetRidOfPostcedingSpaces (WindowString);
                       IF WindowString <> '' THEN
                           WindowString := WindowString + ' ' + NumberString
                       ELSE
                           WindowString := NumberString;

                       ClrScr;
                       Write (WindowString);
                       Continue;
                       END;
                   END;

            {KK1L: note: This is where "normal" characters are processed in the call window}
            IF (KeyChar = ' ') OR (KeyChar = '/') OR
               ((KeyChar = '\') AND (WindowString = '') AND (ActiveWindow = CallWindow)) OR
               ((KeyChar >= '0') AND (KeyChar <= '9')) OR
               ((KeyChar >= 'A') AND (KeyChar <= 'Z')) OR
               ((ActiveWindow = CallWindow) AND (KeyChar = '?')) OR
               ((ActiveWindow = CallWindow) AND (KeyChar = '.') AND StringIsAllNumbersOrDecimal (WindowString)) OR
               ((ActiveWindow = ExchangeWindow) AND (KeyChar IN AccentedChars)) THEN
               {KK1L: 6.72 Added check for accented characters for the Swedes, etc.}
                BEGIN
                {KK1L: 6.68 Added the OR JustLoadingBandMapWithNoRadio to prevent having to ESC to clear the call}
                {           window after being asked for the frequency when loading the band map by S&P with no }
                {           radio interface. Originally added for WRTC 2002 radio B rules.}
                IF (ActiveWindow = CallWindow) AND (BandMapEntryInCallWindow OR JustLoadingBandMapWithNoRadio) THEN
                {KK1L: note: this is where the call window gets cleared when you type a character and the the call}
                {            had been put there by tuning with the band map enabled.}
                    BEGIN
                    WindowString := '';
                    ClrScr;
                    BandMapEntryInCallWindow := False;
                    {OkayToPutUpBandMapCall := False;}  {KK1L: 6.73 To fix call popping up after working them.}
                    JustLoadingBandMapWithNoRadio := False; {KK1L: 6.68}
                    END;

                IF InitialExchangePutUp AND (ActiveWindow = ExchangeWindow) THEN {KK1L: 6.73 NOTE Single char clears window}
                    BEGIN
                    WindowString := KeyChar;
                    ClrScr;
                    Write (WindowString);
                    InitialExchangePutUp := False;
                    END
                ELSE
                    {KK1L: 6.72 NOTE This is where I can add the auto Alt-Z stuff}
                    IF Length (WindowString) < SizeOf (WindowString) - 5 THEN
                        BEGIN
                        { Added overwrite of ? character in 6.22 }


                        IF InsertMode AND (CursorPosition <= Length (WindowString)) AND (WindowString [WhereX] <> '?') THEN
                            BEGIN
                            InsertCursorPosition := CursorPosition;

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
                            GoToXY (InsertCursorPosition + 1, WhereY);
                            END
                        ELSE
                            IF CursorPosition <= Length (WindowString) THEN
                                BEGIN
                                WindowString [CursorPosition] := KeyChar;
                                Write (KeyChar);
                                END
                            ELSE
                                BEGIN
                                WindowString := WindowString + KeyChar;
                                Write (KeyChar);

                                IF (ActiveMode = CW) AND (OpMode = CQOpMode) AND
                                   AutoSendEnable AND
                                   (AutoSendCharacterCount = Length (WindowString)) AND
                                   (NOT StringIsAllNumbersOrDecimal (WindowString)) AND
{                                  (NOT (Copy (WindowString, 1, 1) = '\')) THEN 6.44  }
                                   (NOT StringHas (WindowString, '/')) THEN
                                       IF NOT CallAlreadySent THEN
                                           BEGIN
                                           KeyChar := StartSendingNowKey;
                                           Exit;
                                           END;
                                END;

                        ShowPartialCallMults (WindowString);

                        IF (ActiveWindow = CallWindow) AND
                           (Length (WindowString) = 1) AND
                           InactiveRigCallingCQ THEN
                               BEGIN
                               SwapRadios;
                               SetUpToSendOnActiveRadio;
                               InactiveRigCallingCQ := False;

                               IF (OpMode = SearchAndPounceOpMode) THEN ControlBMemory := KeyChar;

                               KeyChar := ControlB;
                               Exit;
                               END;
                        END;

                { To show domestic multiplier status as it is entered in }

                IF (ActiveWindow = ExchangeWindow) AND DoingDomesticMults THEN
                    BEGIN
                    TempExchange.QTHString := GetLastString (WindowString);

                    IF DoingDomesticMults AND FoundDomesticQTH (TempExchange) THEN
                        VisibleLog.ShowDomesticMultiplierStatus (TempExchange.DomesticQTH);
                    END;

                END
            ELSE
                BEGIN
                IF (KeyChar = QuickQSLKey1) OR
                   (KeyChar = QuickQSLKey2) OR
                   (KeyChar = TailEndKey) THEN
                       BEGIN
                       BeSilent := False;
                       Exit;
                       END;

                IF KeyChar = PossibleCallRightKey THEN
                       PossibleCallCursorRight;

                IF KeyChar = PossibleCallLeftKey THEN
                       PossibleCallCursorLeft;

                IF KeyChar = PossibleCallAcceptKey THEN
                    BEGIN
                    IF ActiveWindow = CallWindow THEN
                        BEGIN
                        WITH PossibleCallList DO
                            IF NumberPossibleCalls > 0 THEN
                                BEGIN
                                WindowString := List [CursorPosition].Call;
                                ClrScr;
                                Write (WindowString);
                                END;
                        END
                    ELSE
                        BEGIN
                        WITH PossibleCallList DO
                            IF NumberPossibleCalls > 0 THEN
                                BEGIN
                                RestorePreviousWindow;
                                ClrScr;
                                CallWindowString := List [CursorPosition].Call;
                                Write (CallWindowString);
                                SaveAndSetActiveWindow (ExchangeWindow);
                                END;
                        END;
                    END;
                END;
            END;

          END;  { of case }
    UNTIL False;
    END;



PROCEDURE ProcessExchangeFunctionKey (ExtendedKey: CHAR);

{ This procedure is used when a function key has been pressed when the
  exchange menu is up (either CQ or pounce).  A function key will send
  the memory that is assigned to that key in the CQ exchange mode. }

    BEGIN
    SetUpToSendOnActiveRadio;

    SetSpeed (DisplayedCodeSpeed);

{

    IF ActiveMode <> CW THEN
        BEGIN
        SendFunctionKeyMessage (ExtendedKey, NotCQMode);
        IF (ExtendedKey = F2) THEN ExchangeHasBeenSent := True;
        IF (ExtendedKey = F1) THEN ExchangeHasBeenSent := False;
        Exit;
        END;
}

    CASE ExtendedKey OF
        F1: BEGIN
            IF (TwoRadioState = StationCalled) THEN
                CheckTwoRadioState (F1Pressed)
            ELSE
                BEGIN
                IF KeyRecentlyPressed (F1, 150) THEN  { 1.5 seconds }
                    BEGIN
                    IF ActiveMode = CW THEN
                        FlushCWBufferAndClearPTT
                    ELSE
                        IF DVPActive THEN DVPStopPlayback;

                    KeyStamp (NullKey);
                    Exit;
                    END
                ELSE
                    BEGIN
                    IF ActiveMode = CW THEN
                        BEGIN
                        IF NOT AllCWMessagesChainable THEN FlushCWBufferAndClearPTT;

                        IF (KeyRecentlyPressed (F1, 600)) OR (NOT DEEnable) THEN
                            SendStringAndStop (MyCall)
                        ELSE
                            BEGIN
                            FlushCWBufferAndClearPTT;
                            SendStringAndStop ('DE ' + MyCall);
                            END;
                        END
                    ELSE
                        IF ActiveMode = Digital THEN
                            BEGIN
                            SendStringAndStop (MyCall + ' ' + MyCall + ' ' + MyCall);
                            END
                        ELSE
                            SendFunctionKeyMessage (F1, SearchAndPounceOpMode);

                    InactiveRigCallingCQ := False;
                    END;
                END;

            ExchangeHasBeenSent := False;
            END;

        F2: IF TwoRadioState = StationCalled THEN
                CheckTwoRadioState (F2Pressed)
            ELSE
                BEGIN
                IF ActiveMode = CW THEN
                    BEGIN
                    IF NOT AllCWMessagesChainable THEN FlushCWBufferAndClearPTT;

                    IF ExchangeHasBeenSent AND (RepeatSearchAndPounceExchange <> '') THEN
                        SendCrypticMessage (RepeatSearchAndPounceExchange)
                    ELSE
                        BEGIN
                        IF (ActiveWindow = CallWindow) AND
                           (CallWindowString = '') AND (ExchangeWindowString = '') THEN
                               BEGIN
                               IF RepeatSearchAndPounceExchange <> '' THEN
                                   SendCrypticMessage (RepeatSearchAndPounceExchange)
                               ELSE
                                   SendCrypticMessage (SearchAndPounceExchange);

                               KeyStamp (ExtendedKey);
                               Exit;
                               END
                           ELSE
                               SendCrypticMessage (SearchAndPounceExchange);
                        END;
                    END
                ELSE
                    SendFunctionKeyMessage (F2, SearchAndPounceOpMode);

                ExchangeHasBeenSent := True;
                END;

        ELSE
            BEGIN
            SendFunctionKeyMessage (ExtendedKey, SearchAndPounceOpMode);

            IF ExtendedKey = AltF10 THEN
                BEGIN
                CallSignICameBackTo := CallWindowString;
                ShowStationInformation (CallWindowString);
                END;
            END;

        END;

    KeyStamp (ExtendedKey);
    END;



PROCEDURE LogContact (VAR RXData: ContestExchange);

{ This procedure will log the contact just completed.  It will be
  pushed onto the editable log and the log entry popped off the editable
  log will be examined and written to the LOG.DAT file.                 }

VAR LogString: Str80;
    Address: INTEGER;

    BEGIN
    IF Trace THEN Write ('!');

    VisibleDupeSheetChanged := True;

    IF ActivePacketPort <> nil THEN
        Packet.DeletePacketEntry (RXData.Callsign, RXData.Band, RXData.Mode);

    LastTwoLettersCrunchedOn := '';

    IF AutoTimeIncrementQSOs > 0 THEN
        BEGIN
        Inc (AutoTimeQSOCount);
        IF AutoTimeQSOCount >= AutoTimeIncrementQSOs THEN
            IncrementTime (1);
        END;

    IF LastDeletedLogEntry <> '' THEN
        BEGIN
        LastDeletedLogEntry := '';
        RemoveWindow (QuickCommandWindow);
        END;

    WindowDupeCheckCall := RXData.Callsign;

    LastQSOLogged := RXData;

    IF TenMinuteRule <> NoTenMinuteRule THEN UpdateTenMinuteDate (RXData.Band, RXData.Mode);

    IF NOT DDX (VerifyContact) THEN RXData.QSOPoints := 0;

    IF NOT TailEnding THEN RemoveWindow (PossibleCallWindow);

    IF Trace THEN Write ('@');

    IF VisibleLog.CallIsADupe (RXData.Callsign, RXData.Band, RXData.Mode) OR
       ((ActiveDomesticMult = GridSquares) AND RoverCall (RXData.Callsign) AND (NumberGridSquaresInList > 0)) THEN
        IF NOT (ActiveQSOPointMethod = AlwaysOnePointPerQSO) THEN
            BEGIN
            IF Trace THEN Write ('#');

            IF (ActiveDomesticMult = GridSquares) AND RoverCall (RXData.Callsign) THEN
                BEGIN
                IF NumberGridSquaresInList > 0 THEN
                    FOR Address := 0 TO NumberGridSquaresInList - 1 DO
                        IF RXData.DomesticQTH = GridSquareList [Address] THEN
                            BEGIN
                            RXData.QSOPoints := 0;

                            IF ReminderPostedCount = 0 THEN
                                BEGIN
                                QuickDisplay ('You already worked ' + RXData.Callsign + ' in ' + RXData.DomesticQTH + '!!');

                                IF DupeCheckSound <> DupeCheckNoSound THEN Tone.DoABeep (ThreeHarmonics);

                                ReminderPostedCount := 60;
                                END;
                            Break;
                            END;
                END
            ELSE
                BEGIN
                IF ReminderPostedCount = 0 THEN
                    BEGIN
                    QuickDisplay (RXData.Callsign + ' is a dupe and will be logged with zero QSO points.');
                    IF DupeCheckSound <> DupeCheckNoSound THEN  Tone.DoABeep (ThreeHarmonics);
                    END;

                RXData.QSOPoints := 0;
                END;
            END;

    IF Trace THEN Write ('+');

    VisibleLog.ProcessMultipliers (RXData);

    IF Trace THEN Write ('%');

    LogString := MakeLogString (RXData);

    CheckBand (RXData.Band);

    IF (RXData.Band >= Band160) AND (RXData.Band <= Band10) THEN
        Inc (ContinentQSOCount [RXData.Band, RXData.QTH.Continent]);

    PushLogStringIntoEditableLogAndLogPopedQSO (LogString, True);

    IF Trace THEN Write ('&');

    IF NOT TailEnding THEN ShowStationInformation (RXData.Callsign);

    IF Trace THEN Write ('\');

    IF DoingDomesticMults AND
       (MultByBand OR MultByMode) AND
       (RXData.DomesticQTH <> '') THEN
           VisibleLog.ShowDomesticMultiplierStatus (RXData.DomMultQTH);

    IF Trace THEN Write ('*');

    Inc (NumberContactsThisMinute);
    NumberQSOPointsThisMinute := NumberQSOPointsThisMinute + RXData.QSOPoints;

    DisplayTotalScore (TotalScore);
    DisplayNamePercentage (TotalNamesSent + VisibleLog.NumberNamesSentInEditableLog, TotalContacts);
    CheckAvailableMemory;

    IF BeepEvery10QSOs AND (TotalContacts MOD 10 = 0) THEN QuickBeep;

    IF Trace THEN Write ('(');

    IF FloppyFileSaveFrequency > 0 THEN
        IF QSOTotals [All, Both] > 0 THEN
            IF QSOTotals [All, Both] MOD FloppyFileSaveFrequency = 0 THEN
                SaveLogFileToFloppy;

    IF Trace THEN Write (')');

    IF UpdateRestartFileEnable THEN Sheet.SaveRestartFile;

    BeSilent := False;
    NameCallsignPutUp := '';

    IF CheckLogFileSize THEN
        IF NOT LogFileLooksOkay THEN
            BEGIN
            QuickDisplay ('LOG FILE SIZE CHECK FAILED!!!!');
            Tone.DoABeep (Warning);
            ReminderPostedCount := 60;
            END;

    IF Trace THEN Write ('-');

    IF CWSpeedFromDataBase AND (RememberCWSpeed <> 0) THEN
        BEGIN
        SetSpeed (RememberCWSpeed);
        RememberCWSpeed := 0;
        DisplayCodeSpeed (CodeSpeed, CWEnabled, DVPOn, ActiveMode);
        END;

    IF (DDXState <> Off) AND Debug AND (CWTone = 0) THEN
        IF Random (100) = 0 THEN
            BandUp;

    IF BandMapEnable THEN
      BEGIN
      UpdateBandMapMultiplierStatus;
      {KK1L: 6.64 Need to change dupe status for this contact as well}
      UpdateBandMapDupeStatus(RXData.Callsign, RXData.Band, RXData.Mode, True);
      END;
    END;



PROCEDURE LogLastCall;

VAR Hours, Minutes, Seconds, Hundreths: Word;
    I: INTEGER;

    BEGIN
    GetTime (Hours, Minutes, Seconds, Hundreths);

    I := Hours;

    IF HourOffset <> 0 THEN
        BEGIN
        I := I + HourOffset;
        IF I > 23 THEN I := I - 24;
        IF I < 0  THEN I := I + 24;
        END;

    Hours := I;

    WITH LastQSOLogged DO
        BEGIN
        NumberSent := TotalContacts + 1;
        Band := ActiveBand;
        Mode := ActiveMode;
        Time := Hours * 100 + Minutes;
        END;

    LogContact (LastQSOLogged);

    VisibleLog.ShowQSOStatus (LastQSOLogged.Callsign);
    UpdateTotals;

    IF ReceivedData.DomesticMult OR
       ReceivedData.DXMult OR
       ReceivedData.ZoneMult THEN
           VisibleLog.ShowRemainingMultipliers;

    IF ReceivedData.DomesticMult THEN
        VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);

    CleanUpDisplay;
    END;



FUNCTION SearchAndPounce: BOOLEAN;

LABEL ControlEnterCommand1, ControlEnterCommand2;

{ Returns FALSE when we don't want to search and pounce anymore. }

VAR Key, ExtendedKey: CHAR;
    MultString: Str80;
    TempFreq: LONGINT;
    TempBand: BandType;
    TempMode: ModeType;
    Mult: BOOLEAN;

    BEGIN
    OpMode := SearchAndPounceOpMode;
    BandMapBand := ActiveBand;
    DisplayBandMap;

    IF ActiveMultiPort <> nil THEN
        CreateAndSendSAPMultiInfoMessage;

    LogBadQSOString := '';

    CQRITEnabled := False;

    ClearAutoSendDisplay;

    ResetSavedWindowListAndPutUpCallWindow;
    Write (CallWindowString);

    PutUpExchangeWindow;
    PutUpExchangeMenu;

    IF ReminderPostedCount = 0 THEN
        QuickDisplay ('You are in the Search And Pounce mode.  Press Shift-Tab to exit.');

    ExchangeWindowString := '';

    IF AltDDupeCheckCall <> '' THEN {KK1L: 6.73 Keeps SO2R message in line with reality}
        BEGIN
        SaveAndSetActiveWindow (DupeInfoWindow);
        WriteLn (AltDDupeCheckCall + ' OK!!          ');
        IF (TwoRadioState = StationCalled) AND (OpMode = CQOpMode) THEN
            Write ('Enter to complete QSO    '); {KK1L: 6.73 The stuff normally following this should remain.}
        RestorePreviousWindow;
        END;

{   Removed in 6.19 - I decided that it would be better to leave the
    cursor in the call window so you can call the guy with a RETURN.

    Whoops - we need this for two radio stuff.  Put back in if
    TwoRadioState = StationCalled. }

    IF TwoRadioState = StationCalled THEN
        BEGIN
        ActivateExchangeWindow;

        ExchangeWindowString := InitialExchangeEntry (CallWindowString);
        ClrScr;
        Write (ExchangeWindowString);

        {TR6.74 - because InitialExchangeEntry no longer does this }

        IF InitialExchangeOverwrite THEN
            InitialExchangePutUp := ExchangeWindowString <> '';

        IF InitialExchangeCursorPos = AtStart THEN
            GoToXY (1, 1)
        ELSE
            GoToXY (1, Length (ExchangeWindowString) + 1);

        ExchangeWindowCursorPosition := WhereY;
        END;

    IF KeyRecentlyPressed (F1, 100) THEN DDX (MaybeRespondToMyCall);

    SearchAndPounce := False; { Presumed exit value unless a QSO is finished
                                and we aren't using the SprintQSYRule. }
    REPEAT
        IF ActiveWindow = CallWindow THEN
            BEGIN
            WindowEditor (CallWindowString, Key, ExtendedKey);

            CASE Key OF
                TabKey:
                    ActivateExchangeWindow;

                ControlB:  { Someone answering a Control-B CQ }
                    BEGIN
                    ExchangeWindowString := '';
                    NameCallsignPutUp := '';
                    RemoveWindow (ExchangeWindow);
                    OpMode := CQOpMode;
                    Exit;
                    END;

                ControlU:  { A packet spot is ready to be worked }
                    BEGIN
                    ResetSavedWindowListAndPutUpCallWindow;
                    Write (CallWindowString);

                    PutUpExchangeWindow;
                    PutUpExchangeMenu;

                    IF ReminderPostedCount = 0 THEN
                        QuickDisplay ('You are in the Search And Pounce mode.  Press escapes to exit.');

                    ActivateExchangeWindow;
                    ExchangeWindowString := InitialExchangeEntry (CallWindowString);
                    ClrScr;
                    Write (ExchangeWindowString);

                    {TR6.74 - need to do this since IntialExchangeEntry does not }

                    IF InitialExchangeOverwrite THEN
                        InitialExchangePutUp := ExchangeWindowString <> '';

                    IF InitialExchangeCursorPos = AtStart THEN
                        GoToXY (1, 1)
                    ELSE
                        GoToXY (1, Length (ExchangeWindowString) + 1);

                    ExchangeWindowCursorPosition := WhereY;
                    END;

                EscapeKey:
                    BEGIN
                    IF (ExchangeWindowString = '') AND
                       (EscapeExitsSearchAndPounce OR (TwoRadioState = CallReady)) THEN
                        BEGIN
                        NameCallsignPutUp := '';
                        CleanUpDisplay;
                        RemoveWindow (ExchangeWindow);
                        {KK1L: 6.73 Clears the DupeInfoStuff I'm working on. Add the DupeInfo check}
                        {IF NOT VisibleDupesheetEnable THEN}
                        IF (NOT VisibleDupesheetEnable) AND (AltDDupeCheckDisplayedCall = '') THEN
                            BEGIN
                            RemoveWindow (QSOInformationWindow);
                            RemoveWindow (MultiplierInformationWindow);
                            END;
                        SearchAndPounce := False;
                        OpMode := CQOpMode;

                        IF AltDDupeCheckCall <> '' THEN {KK1L: 6.73 Keeps SO2R message in line with reality}
                            BEGIN
                            SaveAndSetActiveWindow (DupeInfoWindow);
                            IF ModeMemory[InactiveRadio] = CW THEN
                                WriteLn (AltDDupeCheckCall + ' OK!! at ' + SpeedString + ' WPM')
                            ELSE
                                WriteLn (AltDDupeCheckCall + ' OK!!');
                            Write ('Space bar for '); {KK1L: 6.73 The stuff normally following this should remain.}
                            RestorePreviousWindow;
                            END;

                        Exit;
                        END
                    ELSE
                        BEGIN
                        ClearWindow (ExchangeWindow);
                        ExchangeWindowString := '';
                        CleanUpDisplay;
                        RemoveWindow (QSOInformationWindow);
                        RemoveWindow (MultiplierInformationWindow);
                        END;
                    END;

                SpaceBar:
                    IF (Length (CallWindowString) > 0) AND SpaceBarDupeCheckEnable THEN
                        WindowDupeCheck
                    ELSE
                        BEGIN
                        ProcessExchangeFunctionKey (F1);
                        DDX (MaybeRespondToMyCall);
                        END;

                NullKey:
                    CASE ExtendedKey OF
                        UpArrow, DownArrow:
                            BEGIN      {KK1L testing}
                            ActivateExchangeWindow;
                            {QuickDisplay2('ActivateExchangeWindow1');}
                            END;

                        ShiftTab:
                            BEGIN
                            ExchangeWindowString := '';
                            RemoveWindow (ExchangeWindow);
                            SearchAndPounce := False;
                            OpMode := CQOpMode;

                            IF AltDDupeCheckCall <> '' THEN {KK1L: 6.73 Keeps SO2R message in line with reality}
                                BEGIN
                                SaveAndSetActiveWindow (DupeInfoWindow);
                                IF ModeMemory[InactiveRadio] = CW THEN
                                    WriteLn (AltDDupeCheckCall + ' OK!! at ' + SpeedString + ' WPM')
                                ELSE
                                    WriteLn (AltDDupeCheckCall + ' OK!!');
                                Write ('Space bar for '); {KK1L: 6.73 The stuff normally following this should remain.}
                                RestorePreviousWindow;
                                END;

                            Exit;
                            END;

                        ELSE
                            BEGIN
                            ProcessExchangeFunctionKey (ExtendedKey);

                            IF CWMessageCommand = CWCommandCQMode THEN
                                BEGIN
                                ExchangeWindowString := '';
                                NameCallsignPutUp := '';

                                CleanUpDisplay;
                                RemoveWindow (ExchangeWindow);

                                {KK1L: 6.73 Clears the DupeInfoStuff I'm working on. Add the DupeInfo check}
                                {IF NOT VisibleDupesheetEnable THEN}
                                IF (NOT VisibleDupesheetEnable) AND (AltDDupeCheckCall = '') THEN
                                    BEGIN
                                    RemoveWindow (QSOInformationWindow);
                                    RemoveWindow (MultiplierInformationWindow);
                                    END;

                                CWMessageCommand := NoCWCommand;

                                OpMode := CQOpMode;

                                IF AltDDupeCheckCall <> '' THEN {KK1L: 6.73 Keeps SO2R message in line with reality}
                                    BEGIN
                                    SaveAndSetActiveWindow (DupeInfoWindow);
                                    IF ModeMemory[InactiveRadio] = CW THEN
                                        WriteLn (AltDDupeCheckCall + ' OK!! at ' + SpeedString + ' WPM')
                                    ELSE
                                        WriteLn (AltDDupeCheckCall + ' OK!!');
                                    Write ('Space bar for '); {KK1L: 6.73 The stuff normally following this should remain.}
                                    RestorePreviousWindow;
                                    END;

                                Exit;
                                END;

                            IF CWMessageCommand = CWCommandControlEnter THEN
                                BEGIN
                                BeSilent := True;
                                CWMessageCommand := NoCWCommand;
                                GoTo ControlEnterCommand1;
                                END;

                            IF KeyRecentlyPressed (F1, 100) THEN
                                BEGIN
                                IF Length (CallWindowString) > 0 THEN
                                    BEGIN
                                    DisplayGridSquareStatus (CallWindowString);
                                    ShowStationInformation (CallWindowString);
                                    VisibleLog.DoPossibleCalls (CallWindowString);
                                    END;

                                IF GoodCallSyntax (CallWindowString) THEN
                                    BEGIN
                                    ActivateExchangeWindow;

                                    IF ExchangeWindowString = '' THEN
                                        BEGIN
                                        ExchangeWindowString := InitialExchangeEntry (CallWindowString);
                                        ClrScr;
                                        Write (ExchangeWindowString);

                                        {TR6.74 - need to do this since IntialExchangeEntry does not }

                                        IF InitialExchangeOverwrite THEN
                                            InitialExchangePutUp := ExchangeWindowString <> '';

                                        IF InitialExchangeCursorPos = AtStart THEN
                                            GoToXY (1, 1)
                                        ELSE
                                            GoToXY (1, WhereY);

                                        ExchangeWindowCursorPosition := WhereY;
                                        END;
                                    END;
                                END;
                            END;
                        END;



                CarriageReturn:
                    BEGIN
ControlEnterCommand1:

                    IF (ExchangeWindowString = '') AND (CallWindowString = '') THEN
                        IF AutoReturnToCQMode THEN
                            BEGIN
                            NameCallsignPutUp := '';
                            CleanUpDisplay;
                            RemoveWindow (ExchangeWindow);
                            {KK1L: 6.73 Clears the DupeInfoStuff I'm working on. Add the DupeInfo check}
                            {IF NOT VisibleDupesheetEnable THEN}
                            IF (NOT VisibleDupesheetEnable) AND (AltDDupeCheckCall = '') THEN
                                BEGIN
                                RemoveWindow (QSOInformationWindow);
                                RemoveWindow (MultiplierInformationWindow);
                                END;
                            IF AltDDupeCheckCall <> '' THEN {KK1L: 6.73 Keeps SO2R message in line with reality}
                                BEGIN
                                SaveAndSetActiveWindow (DupeInfoWindow);
                                IF ModeMemory[InactiveRadio] = CW THEN
                                    WriteLn (AltDDupeCheckCall + ' OK!! at ' + SpeedString + ' WPM')
                                ELSE
                                    WriteLn (AltDDupeCheckCall + ' OK!!');
                                Write ('Space bar for '); {KK1L: 6.73 The stuff normally following this should remain.}
                                RestorePreviousWindow;
                                END;
                            SearchAndPounce := False;
                            OpMode := CQOpMode;

                            SpecialCommand := SendF1Message;
                            Exit;
                            END;

                    IF ExchangeWindowString = '' THEN
                        BEGIN
                        IF (Length (CallWindowString) >= 3) AND
                           ((NOT VisibleLog.CallIsADupe (CallWindowString, ActiveBand, ActiveMode)) OR
                            (NOT AutoDupeEnableSandP)) THEN
                               BEGIN
                               ExchangeHasBeenSent := False;

                               IF GoodCallSyntax (CallWindowString) THEN
                                   BEGIN
                                   IF ActiveMode = CW THEN
                                       BEGIN
                                       SetSpeed (DisplayedCodeSpeed);
                                       InactiveRigCallingCQ := False;

                                       IF MessageEnable AND NOT BeSilent THEN
                                           BEGIN
                                           IF DEEnable THEN
                                               SendStringAndStop ('DE ' + MyCall)
                                           ELSE
                                               SendStringAndStop (MyCall);
                                           KeyStamp (F1);
                                           END;
                                       END
                                   ELSE
                                       IF ActiveMode = Digital THEN
                                           BEGIN
                                           SendStringAndStop (MyCall + ' ' + MyCall);
                                           END
                                       ELSE
                                           BEGIN
                                           IF DVPEnable AND MessageEnable AND NOT BeSilent THEN
                                               SendFunctionKeyMessage (F1, SearchAndPounceOpMode);

                                           IF DVKEnable AND NOT BeSilent THEN
                                               {KK1L: 6.73 Added mode to GetExMemoryString}
                                               SendDVKMessage (GetEXMemoryString (ActiveMode, F1));
                                           END;

                                   DisplayGridSquareStatus (CallWindowString);
                                   ShowStationInformation (CallWindowString);

                                   IF BandMapEnable THEN
                                       IF GetRadioParameters (ActiveRadio, '', TempFreq, TempBand, TempMode, FALSE, False) THEN
                                           BEGIN
                                           BandMapInfoCall := CallWindowString;

                                           VisibleLog.DetermineIfNewMult (CallWindowString,
                                                                          ActiveBand,
                                                                          ActiveMode,
                                                                          MultString);

                                           Mult := MultString <> '';

                                           NewBandMapEntry (CallWindowString,
                                                            TempFreq, 0, ActiveMode,
                                                            VisibleLog.CallIsADupe (CallWindowString, ActiveBand, ActiveMode),
                                                            Mult,
                                                            BandMapDecayTime, True);
                                           END;

                                   IF ExchangeWindowString = '' THEN
                                       BEGIN
                                       ExchangeWindowString := InitialExchangeEntry (CallWindowString);
                                       ActivateExchangeWindow;
                                       ClrScr;
                                       Write (ExchangeWindowString);

                                       {TR6.74 - need to do this since IntialExchangeEntry does not }

                                       IF InitialExchangeOverwrite THEN
                                           InitialExchangePutUp := ExchangeWindowString <> '';


                                       IF InitialExchangeCursorPos = AtStart THEN GoToXY (1, 1);
                                       ExchangeWindowCursorPosition := WhereY;
                                       END
                                   ELSE
                                       ActivateExchangeWindow;

                                   IF LeaveCursorInCallWindow THEN
                                       RestorePreviousWindow;

                                   VisibleLog.DoPossibleCalls (CallWindowString);
                                   DDX (MaybeRespondToMyCall);
                                   END;
                               END
                           ELSE
                               IF NOT WindowDupeCheck THEN
                                   ActivateExchangeWindow;
                        END
                    ELSE
                        BEGIN
                        IF TwoRadioState = StationCalled THEN
                            CheckTwoRadioState (ReturnPressed)
                        ELSE
                            IF MessageEnable AND (NOT ExchangeHasBeenSent) AND (NOT BeSilent) AND MessageEnable THEN
                                BEGIN
                                IF ActiveMode = CW THEN
                                    SendCrypticMessage (SearchAndPounceExchange)
                                ELSE
                                    SendCrypticMessage (SearchAndPouncePhoneExchange);
                                ExchangeHasBeenSent := True;
                                END;

                        IF ParametersOkay (CallWindowString,
                                           ExchangeWindowString,
                                           ActiveBand,
                                           ActiveMode,
                                           DisplayedFrequency,
                                           ReceivedData) THEN
                            BEGIN
                            ReceivedData.SearchAndPounce := True;

                            LogContact (ReceivedData);

                            ShowStationInformation (ReceivedData.Callsign);
                            UpdateTotals;

                            IF ReceivedData.DomesticMult OR
                               ReceivedData.DXMult OR
                               ReceivedData.ZoneMult THEN
                                   VisibleLog.ShowRemainingMultipliers;

                            IF ReceivedData.DomesticMult THEN
                                VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);

                            RemoveWindow (QSOInformationWindow);
                            RemoveWindow (MultiplierInformationWindow);
                            RemoveWindow (ExchangeWindow);
                            CleanUpDisplay;
                            EscapeDeletedCallEntry := CallWindowString;
                            CallWindowString := '';
                            ExchangeWindowString := '';
                            ClearWindow (ExchangeWindow);
                            ResetSavedWindowListAndPutUpCallWindow;

                            IF SprintQSYRule THEN
                                QuickDisplay ('SPRINT QSY RULE!!!');

                            IF BandMapEnable THEN
                                IF GetRadioParameters (ActiveRadio, '', TempFreq, TempBand, TempMode, FALSE, False) THEN
                                    BEGIN
                                    BandMapInfoCall := ReceivedData.Callsign;

                                    VisibleLog.DetermineIfNewMult (BandMapInfoCall,
                                                                   TempBand,
                                                                   TempMode,
                                                                   MultString);

                                    Mult := MultString <> '';

                                    NewBandMapEntry (ReceivedData.Callsign,
                                                     TempFreq, 0, TempMode,
                                                     True,
                                                     Mult,
                                                     BandMapDecayTime, True);
                                    END;


                            DDX (QSLMyExchange);

                            SearchAndPounce := NOT SprintQSYRule;
                            OpMode := CQOpMode;
                            Exit;
                            END
                        ELSE
                            IF GoodCallSyntax (CallWindowString) THEN
                                ActivateExchangeWindow;
                        END;
                    END;

                ELSE
                    ActivateExchangeWindow;

                END;      { of case }

            END;  { of call window active }

        IF ActiveWindow = ExchangeWindow THEN
            BEGIN
            ExchangeWindowCursorPosition := WhereY;

            WindowEditor (ExchangeWindowString, Key, ExtendedKey);

            CASE Key OF
                TabKey:
                    RestorePreviousWindow;

                ControlU:  { A packet spot is ready to be worked }
                    BEGIN
                    ResetSavedWindowListAndPutUpCallWindow;
                    Write (CallWindowString);

                    PutUpExchangeWindow;
                    PutUpExchangeMenu;

                    IF ReminderPostedCount = 0 THEN
                        QuickDisplay ('You are in the Search And Pounce mode.  Press escapes to exit.');

                    ActivateExchangeWindow;
                    ExchangeWindowString := InitialExchangeEntry (CallWindowString);
                    ClrScr;
                    Write (ExchangeWindowString);

                    {TR6.74 - need to do this since IntialExchangeEntry does not }

                    IF InitialExchangeOverwrite THEN
                        InitialExchangePutUp := ExchangeWindowString <> '';

                    IF InitialExchangeCursorPos = AtStart THEN GoToXY (1, 1);
                    ExchangeWindowCursorPosition := WhereY;
                    END;

                EscapeKey:
                    BEGIN
                    NameCallsignPutUp := '';

                    IF CallWindowString <> '' THEN
                        BEGIN
                        ClearWindow (CallWindow);
                        EscapeDeletedCallEntry := CallWindowString;
                        CallWindowString := '';

                        IF NOT VisibleDupeSheetEnable THEN
                            BEGIN
                            RemoveWindow (QSOInformationWindow);
                            RemoveWindow (MultiplierInformationWindow);
                            END;
                        CleanUpDisplay;
                        END
                    ELSE
                        IF EscapeExitsSearchAndPounce OR (TwoRadioState = CallReady) THEN
                            BEGIN
                            CleanUpDisplay;
                            RemoveWindow (ExchangeWindow);
                            RemoveWindow (QSOInformationWindow);
                            RemoveWindow (MultiplierInformationWindow);
                            OpMode := CQOpMode;
                            Exit;
                            END;

                    ResetSavedWindowListAndPutUpCallWindow;
                    END;


                SpaceBar:
                    IF (Length (CallWindowString) > 0) AND SpaceBarDupeCheckEnable THEN
                        WindowDupeCheck
                    ELSE
                        BEGIN
                        ProcessExchangeFunctionKey (F1);
                        END;

                NullKey:
                    CASE ExtendedKey OF
                        UpArrow, DownArrow:
                            BEGIN      {KK1L testing}
                            RestorePreviousWindow;
                            {QuickDisplay2('RestorePreviousWindow1');}
                            END;

                        ShiftTab:
                            BEGIN
                            ExchangeWindowString := '';
                            RemoveWindow (ExchangeWindow);
                            OpMode := CQOpMode;

                            IF AltDDupeCheckCall <> '' THEN {KK1L: 6.73 Keeps SO2R message in line with reality}
                                BEGIN
                                SaveAndSetActiveWindow (DupeInfoWindow);
                                IF ModeMemory[InactiveRadio] = CW THEN
                                    WriteLn (AltDDupeCheckCall + ' OK!! at ' + SpeedString + ' WPM')
                                ELSE
                                    WriteLn (AltDDupeCheckCall + ' OK!!');
                                Write ('Space bar for '); {KK1L: 6.73 The stuff normally following this should remain.}
                                RestorePreviousWindow;
                                END;

                            Exit;
                            END;

                        ELSE
                            BEGIN
                            ProcessExchangeFunctionKey (ExtendedKey);

                            IF CWMessageCommand = CWCommandCQMode THEN
                                BEGIN
                                ExchangeWindowString := '';
                                NameCallsignPutUp := '';

                                CleanUpDisplay;
                                RemoveWindow (ExchangeWindow);

                                {KK1L: 6.73 Clears the DupeInfoStuff I'm working on. Add the DupeInfo check}
                                {IF NOT VisibleDupesheetEnable THEN}
                                IF (NOT VisibleDupesheetEnable) AND (AltDDupeCheckCall = '') THEN
                                    BEGIN
                                    RemoveWindow (QSOInformationWindow);
                                    RemoveWindow (MultiplierInformationWindow);
                                    END;

                                CWMessageCommand := NoCWCommand;

                                OpMode := CQOpMode;

                                IF AltDDupeCheckCall <> '' THEN {KK1L: 6.73 Keeps SO2R message in line with reality}
                                    BEGIN
                                    SaveAndSetActiveWindow (DupeInfoWindow);
                                    IF ModeMemory[InactiveRadio] = CW THEN
                                        WriteLn (AltDDupeCheckCall + ' OK!! at ' + SpeedString + ' WPM')
                                    ELSE
                                        WriteLn (AltDDupeCheckCall + ' OK!!');
                                    Write ('Space bar for '); {KK1L: 6.73 The stuff normally following this should remain.}
                                    RestorePreviousWindow;
                                    END;

                                Exit;
                                END;

                            IF CWMessageCommand = CWCommandControlEnter THEN
                                BEGIN
                                BeSilent := True;
                                CWMessageCommand := NoCWCommand;
                                GoTo ControlEnterCommand2;
                                END;

                            END;
                        END;

                CarriageReturn:
                    BEGIN

ControlEnterCommand2:

                    IF TwoRadioState = StationCalled THEN
                        CheckTwoRadioState (ReturnPressed)
                    ELSE
                        IF NOT ExchangeHasBeenSent THEN
                            BEGIN
                            IF MessageEnable AND NOT BeSilent THEN
                                IF ActiveMode <> Phone THEN
                                    SendCrypticMessage (SearchAndPounceExchange)
                                ELSE
                                    SendCrypticMessage (SearchAndPouncePhoneExchange);

                            ExchangeHasBeenSent := True;
                            END;

                    IF ParametersOkay (CallWindowString,
                                       ExchangeWindowString,
                                       ActiveBand,
                                       ActiveMode,
                                       DisplayedFrequency,
                                       ReceivedData) THEN
                        BEGIN
                        ReceivedData.SearchAndPounce := True;

                        LogContact (ReceivedData);

                        ShowStationInformation (ReceivedData.Callsign);

                        UpdateTotals;

                        IF ReceivedData.DomesticMult OR
                           ReceivedData.DXMult OR
                           ReceivedData.ZoneMult THEN
                               VisibleLog.ShowRemainingMultipliers;

                        IF ReceivedData.DomesticMult THEN
                            VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);

                        CleanUpDisplay;
                        EscapeDeletedCallEntry := CallWindowString;
                        CallWindowString := '';
                        ExchangeWindowString := '';
                        RemoveWindow (ExchangeWindow);
                        ResetSavedWindowListAndPutUpCallWindow;

                        IF SprintQSYRule THEN
                            QuickDisplay ('SPRINT QSY RULE!!!');

                        IF BandMapEnable THEN
                            IF GetRadioParameters (ActiveRadio, '', TempFreq, TempBand, TempMode, FALSE, False) THEN
                                BEGIN
                                BandMapInfoCall := ReceivedData.Callsign;

                                VisibleLog.DetermineIfNewMult (BandMapInfoCall,
                                                               TempBand,
                                                               TempMode,
                                                               MultString);

                                Mult := MultString <> '';

                                NewBandMapEntry (ReceivedData.Callsign,
                                                 TempFreq, 0, TempMode,
                                                 True,
                                                 Mult,
                                                 BandMapDecayTime, True);
                                END;


                        DDX (QSLMyExchange);

                        OpMode := CQOpMode;
                        SearchAndPounce := NOT SprintQSYRule;
                        Exit;
                        END;
                    END;

                ELSE RestorePreviousWindow;

                END; { of case}
            END;     { of exchange window active }
    UNTIL FALSE;
    END;



PROCEDURE GetInitialCall;

VAR Key, TempKey, ExtendedKey : CHAR;
    SearchAndPounceStatus, SpecialRadioSwap {, StationCalled}: BOOLEAN;
    EditingCallsignSent: BOOLEAN;
    TimeOut: BYTE;

    BEGIN
    OpMode := CQOpMode;

    EditingCallsignSent := False;
    SpecialRadioSwap    := False;
    CallAlreadySent     := False;

    RemoveWindow (ExchangeWindow);
    ResetSavedWindowListAndPutUpCallWindow;

    EscapeDeletedCallEntry := CallWindowString;
    CallWindowString     := '';
    ExchangeWindowString := '';

    CleanUpDisplay;
//OK to here

    IF DDXState = SAndPExchangeSent THEN
        DDX (NormalContactComplete);

    REPEAT
        PutUpCQMenu;

        IF ReminderPostedCount = 0 THEN
            QuickDisplay ('ENTER a callsign.  SPACE for dupecheck.  Alt-H for help.');

        DDX (MaybeSendANewCall);

        IF Debug AND (DDXState = CallSentInResponseToACQ) THEN
            BEGIN
            CallWindowString := DDXCall1;
            Write (CallWindowString);
            END;

        IF KeyPressedMemory = ' ' THEN
            BEGIN
            Key := ' ';
            KeyPressedMemory := Chr (0);
            END
        ELSE
            IF ActiveWindow = CallWindow THEN
                WindowEditor (CallWindowString, Key, ExtendedKey)
            ELSE
                WindowEditor (ExchangeWindowString, Key, ExtendedKey);


        IF (Key = StartSendingNowKey) AND (ActiveWindow = CallWindow) AND (Length (CallWindowString) >= 1) THEN
            BEGIN
            IF ReminderPostedCount = 0 THEN
                IF AutoCallTerminate THEN
                    QuickDisplay ('Continue entering characters.  Auto terminate active!!')
                ELSE
                    QuickDisplay ('Continue entering characters and press RETURN when done.');

            IF Length (CallWindowString) > 0 THEN
                BEGIN
                IF MessageEnable THEN
                    BEGIN
                    AddStringToBuffer (CallWindowString, CWTone);
                    PTTForceOn;
                    END;

                IF (SCPMinimumLetters > 0) AND (NOT NewKeyPressed) THEN {KK1L: 6.73 Added ActiveRadio}
                    VisibleLog.SuperCheckPartial (CallWindowString, True, ActiveRadio);

                REPEAT
                    REPEAT
                        IF (ActiveMode = CW) AND CWEnabled AND (NOT ReadInLog) AND
                            AutoCallTerminate AND NOT CWStillBeingSent THEN
                                BEGIN
                                CallAlreadySent := True;
                                CallsignICameBackTo := CallWindowString;
                                Exit;
                                END;
                       millisleep;
                    UNTIL NewKeyPressed OR ReadInLog;

                    IF ReadInLog THEN
                        BEGIN
                        IF (Length (ReadInCallsign) = Length (CallWindowString)) AND
                           (Length (ReadInCallsign) > 0) THEN
                               TempKey := CarriageReturn
                        ELSE
                            BEGIN
                            TempKey := ReadInCallsign [Length (CallWindowString) + 1];
                            Wait (Random (400));
                            END;
                        END

                    ELSE TempKey := Upcase (NewReadKey);

                    CASE TempKey OF
                        BackSpace:

                            IF EditingCallsignSent THEN
                                BEGIN
                                IF Length (CallWindowString) > 0 THEN
                                    BEGIN
                                    GoToXY (WhereX - 1, 1);
                                    ClrEol;
                                    Delete (CallWindowString, Length (CallWindowString), 1);
                                    END
                                END
                            ELSE
                                IF (CWEnabled AND DeleteLastCharacter) OR NOT CWEnabled THEN
                                    BEGIN
                                    GoToXY (WhereX - 1, 1);
                                    ClrEol;
                                    Delete (CallWindowString, Length (CallWindowString), 1);
                                    END
                                ELSE
                                    BEGIN
                                    AddStringToBuffer ('!', CWTone);
                                    GoToXY (WhereX - 1, 1);
                                    ClrEol;
                                    Delete (CallWindowString, Length (CallWindowString), 1);
                                    EditingCallsignSent := True;
                                    END;


                        NullKey:
                            BEGIN
                            CallAlreadySent := True;
                            CallsignICameBackTo := CallWindowString;

                            CASE NewReadKey OF
                                AltB: BEGIN
                                      RememberFrequency; {KK1L: 6.72 Added to match all other calls. Needed for loss of coms}
                                      BandUp;
                                      END;

                                AltD: IF K1EANetworkEnable THEN
                                          PassStationToCTNetwork
                                      ELSE
                                          DupeCheckOnInactiveRadio;

                                AltG: SwapMultDisplay;
                                AltK: ToggleCW (True);
                                AltM: BEGIN
                                      ToggleModes;
                                      DisplayAutoSendCharacterCount;
                                      END;

                                AltR: BEGIN
                                      IF NOT SingleRadioMode THEN
                                        BEGIN
                                        IF (TwoRadioState = StationCalled) THEN {KK1L: 6.73}
                                          BEGIN
                                          Tone.DoABeep(Warning);
                                          QuickDisplay('You are working a station on the 2nd radio. Escape to cancel first!');
                                          END
                                        ELSE
                                          BEGIN
                                          SwapRadios;
                                          DisplayAutoSendCharacterCount;
                                          {KK1L: 6.73 Used to use a variable CheckSpeed}
                                          Str (SpeedMemory[InactiveRadio], SpeedString);
                                          END;
                                        END;
                                      END;

                                AltV: BEGIN
                                      RememberFrequency; {KK1L: 6.72 Added to match all other calls. Needed for loss of coms}
                                      BandDown;
                                      END;

                                AltY: DeleteLastContact;
                                PageUpKey: SpeedUp;
                                PageDownKey: SlowDown;
                                END;
                            END;

                        CarriageReturn:
                            BEGIN
                            CallAlreadySent := True;
                            CallsignICameBackTo := CallWindowString;
                            Exit;
                            END;

                        EscapeKey:
                            BEGIN

                            IF ((ActiveMode = CW) AND CWStillBeingSent) OR
                               ((ActiveMode = Phone) AND (DVPMessagePlaying OR DVKMessagePlaying)) THEN
                                BEGIN
                                IF ActiveMode = CW THEN
                                    FlushCWBufferAndClearPTT
                                ELSE
                                    IF DVPActive THEN
                                        BEGIN
                                        DVPStopPlayback;

                                        TimeOut := 0;

                                        REPEAT
                                            Wait (5);
                                            Inc (TimeOut);
                                        UNTIL (NOT DVPMessagePlaying) OR (TimeOut > 60);
                                        END;
                                END
                            ELSE
                                BEGIN
                                FlushCWBufferAndClearPTT;
                                EscapeDeletedCallEntry := CallWindowString;
                                CallWindowString := '';
                                ClrScr;
                                RemoveWindow (ExchangeWindow);
                                IF NOT VisibleDupeSheetEnable THEN
                                    BEGIN
                                    RemoveWindow (QSOInformationWindow);
                                    RemoveWindow (MultiplierInformationWindow);
                                    END;
                                NameCallsignPutUp := '';
                                RemoveWindow (QuickCommandWindow);
                                CleanUpDisplay;
                                END;
                            END;

                        ELSE
                            IF (ValidCallCharacter (TempKey)) OR (TempKey = '?') THEN
                                BEGIN
                                EditingCallsignSent := False;
                                CallWindowString := CallWindowString + TempKey;
                                AddStringToBuffer (TempKey, CWTone);
                                Write (TempKey);

                                IF PartialCallEnable THEN
                                    IF Sheet.TwoLetterCrunchProcess (CallWindowString) THEN
                                        BEGIN
                                        VisibleLog.GeneratePartialCallList (CallWindowString,
                                                        ActiveBand,
                                                        ActiveMode,
                                                        PossibleCallList);
                                        DisplayPossibleCalls (PossibleCallList);
                                        END;

                                IF (SCPMinimumLetters > 0) AND (NOT NewKeyPressed) THEN {KK1L: 6.73 Added ActiveRadio}
                                    VisibleLog.SuperCheckPartial (CallWindowString, True, ActiveRadio);
                                END;

                        END;  { of case }

                UNTIL TempKey = EscapeKey;
                END;
            END
        ELSE
          CASE Key OF

            EscapeKey:
                BEGIN
                IF ((ActiveMode = CW) AND CWStillBeingSent) OR
                   ((ActiveMode = Phone) AND (DVPMessagePlaying OR DVKMessagePlaying)) THEN
                       BEGIN
                       IF ActiveMode = CW THEN
                          FlushCWBufferAndClearPTT
                       ELSE
                          IF DVPActive THEN
                              BEGIN
                              DVPStopPlayback;

                              TimeOut := 0;

                              REPEAT
                                  Wait (5);
                                  Inc (TimeOut);
                              UNTIL (NOT DVPMessagePlaying) OR (TimeOut > 60);
                          END;
                       END
                   ELSE
                       IF ActiveWindow = ExchangeWindow THEN
                           BEGIN
                           IF ExchangeWindowString <> '' THEN
                               BEGIN
                               ExchangeWindowString := '';
                               ClrScr;
                               END
                           ELSE
                               RemoveAndRestorePreviousWindow;
                           END
                       ELSE
                           BEGIN
                           EscapeDeletedCallEntry := CallWindowString;
                           CallWindowString := '';
                           ClrScr;

                           ExchangeWindowString := '';
                           RemoveWindow (ExchangeWindow);
                           RemoveWindow (QSOInformationWindow);
                           RemoveWindow (MultiplierInformationWindow);
                           RemoveWindow (QuickCommandWindow);
                           NameCallsignPutUp := '';

                           { This is new for 5.88 }

//                           IF TwoRadioState = CallReady THEN
//                               TwoRadioState := Idle;

                           CleanUpDisplay;
                           DisplayEditableLog (VisibleLog.LogEntries);
                           END;
                END;

            SpaceBar:
                IF (AltDDupeCheckDisplayedCall <> '') AND (CallWindowString = '') THEN
                    BEGIN
                    FlushCWBufferAndClearPTT;
                    if dvpenable and dvpactive then dvpstopplayback;
                    if DVKEnable then senddvkmessage('DVK0');

                    IF (TwoRadioState = CallReady) THEN
                        CheckTwoRadioState (SpaceBarPressed) {KK1L: 6.73 Should modify to handle Alt-D from SAP mode}
                    ELSE
                        BEGIN
                        SwapRadios;  { Changes band/mode and display }
                        SpecialRadioSwap := True;
                        END;

                    IF TwoRadioState <> CallReady THEN
                        BEGIN
                        CallWindowString := AltDDupeCheckDisplayedCall;
                        ResetSavedWindowListAndPutUpCallWindow;
                        Write (CallWindowString);
                        ShowStationInformation (CallWindowString);
                        DisplayGridSquareStatus (CallWindowString);
                        VisibleLog.DoPossibleCalls (CallWindowString);

                        IF (Length (CallWindowString) >= 3) AND (ExchangeWindowString = '') THEN
                            BEGIN
                            ActivateExchangeWindow;
                            ExchangeWindowString := InitialExchangeEntry (CallWindowString);
                            ClrScr;
                            Write (ExchangeWindowString);

                            {TR6.74 - need to do this since IntialExchangeEntry does not }

                            IF InitialExchangeOverwrite THEN
                                InitialExchangePutUp := ExchangeWindowString <> '';

                            IF InitialExchangeCursorPos = AtStart THEN GoToXY (1, 1);
                            ExchangeWindowCursorPosition := WhereY;
                            RestorePreviousWindow;
                            END;

                        REPEAT
                            PutUpExchangeWindow;
                            DisplayNextQSONumber (TotalContacts + 1);
                            ClearContestExchange (ReceivedData);
                            ExchangeHasBeenSent := False;
                            SearchAndPounceStatus := SearchAndPounce;
                        UNTIL (NOT SearchAndPounceStatus) OR (TwoRadioState = SendingExchange);

                        ClearContestExchange (ReceivedData);  { To try and fix a problem
                                                                with Two Radio mode and DX
                                                                mults showing up in next QSO }
                        IF ActiveRadio = RadioOne THEN
                            BEGIN
                            CQRITEnabled := Radio1Type = TS850;
                            END
                        ELSE
                            CQRITEnabled := Radio2Type = TS850;

                        IF (TwoRadioState = SendingExchange) THEN
                            CheckTwoRadioState (ContactDone)
                        ELSE
                            IF SpecialRadioSwap THEN
                                BEGIN
                                SwapRadios;
                                SpecialRadioSwap := False;
                                END;

                        DisplayAutoSendCharacterCount;

                        IF SearchAndPounceStatus THEN
                            BEGIN
                            RemoveWindow (DupeInfoWindow);

                            EscapeDeletedCallEntry := CallWindowString;
                            CallWindowString := '';
                            RemoveWindow (ExchangeWindow);
                            ResetSavedWindowListAndPutUpCallWindow;
                            END;
                        END;
                    END

{ Still a SpaceBar, but not doing AltDDupeCheckCall }

                ELSE
                    IF (CallWindowString = '') OR NOT SpaceBarDupeCheckEnable THEN
                        BEGIN
                        IF CWStillBeingSent THEN FlushCWBufferAndClearPTT;      { Clear CW sent on Inactive Radio}

                        SetUpToSendOnActiveRadio;

                        InactiveRigCallingCQ := False;

                        IF MessageEnable THEN
                            BEGIN
                            IF ActiveMode = CW THEN
                                BEGIN
                                IF DEEnable THEN
                                    SendStringAndStop ('DE ' + MyCall)
                                ELSE
                                    SendStringAndStop (MyCall);
                                END
                            ELSE
                                IF ActiveMode = Digital THEN
                                    SendStringAndStop (MyCall + ' ' + MyCALL)
                                ELSE
                                    SendFunctionKeyMessage (F1, SearchAndPounceOpMode);
                            END;

                        KeyStamp (F1);

                        REPEAT
                            PutUpExchangeWindow;
                            DisplayNextQSONumber (TotalContacts + 1);
                            ClearContestExchange (ReceivedData);
                            ExchangeHasBeenSent := False;
                        UNTIL NOT SearchAndPounce;

                        ClearContestExchange (ReceivedData);

                        IF ActiveRadio = RadioOne THEN
                            BEGIN
                            CQRITEnabled := Radio1Type = TS850;
                            END
                        ELSE
                            CQRITEnabled := Radio2Type = TS850;


                        RemoveWindow (ExchangeWindow);

                        DisplayAutoSendCharacterCount;

                        EscapeDeletedCallEntry := CallWindowString;

                        IF CallWindowString = '' THEN
                            ResetSavedWindowListAndPutUpCallWindow;
                        END
                    ELSE
                        BEGIN
                        IF WindowDupeCheck THEN RemoveWindow (ExchangeWindow);
                        RestorePreviousWindow;
                        END;

            TabKey, ControlU:
                BEGIN
                REPEAT
                    PutUpExchangeWindow;
                    DisplayNextQSONumber (TotalContacts + 1);
                    ClearContestExchange (ReceivedData);
                    ExchangeHasBeenSent := False;
                UNTIL NOT SearchAndPounce;

                ClearContestExchange (ReceivedData);

                IF ActiveRadio = RadioOne THEN
                    BEGIN
                    CQRITEnabled := Radio1Type = TS850;
                    END
                ELSE
                    CQRITEnabled := Radio2Type = TS850;


                DisplayAutoSendCharacterCount;

                IF CallWindowString = '' THEN
                    BEGIN
                    RemoveWindow (ExchangeWindow);
                    ResetSavedWindowListAndPutUpCallWindow;
                    END;
                END;

            NullKey:
                BEGIN                          { special key }
                IF (((ExtendedKey >= F1)         AND (ExtendedKey <= F10)) OR
                    ((ExtendedKey >= F11)        AND (ExtendedKey <= F12)) OR
                    ((ExtendedKey >= ControlF1)  AND (ExtendedKey <= ControlF10)) OR
                    ((ExtendedKey >= ControlF11) AND (ExtendedKey <= ControlF12)) OR
                    ((ExtendedKey >= AltF1)      AND (ExtendedKey <= AltF10)) OR
                    ((ExtendedKey >= AltF11)     AND (ExtendedKey <= AltF12)) OR
                    ((ExtendedKey >= ShiftF1)    AND (ExtendedKey <= ShiftF10)) OR
                    ((ExtendedKey >= ShiftF11)   AND (ExtendedKey <= ShiftF12))) THEN
                        BEGIN
                        KeyStamp (ExtendedKey);
                        SendFunctionKeyMessage (ExtendedKey, CQOpMode);

                        IF (ExtendedKey = F1) OR (ExtendedKey = F2) THEN
                            IF FrequencyDisplayed THEN
                                BEGIN
                                LastCQFrequency := DisplayedFrequency;
                                LastCQMode      := ActiveMode;
                                END
                            ELSE
                                LastCQFrequency := 0;

                        IF CWMessageCommand = CWCommandSAPMode THEN
                            BEGIN
                            CWMessageCommand := NoCWCommand;

                            IF AltDDupeCheckCall <> '' THEN {KK1L: 6.73 Keeps SO2R message in line with reality}
                                BEGIN
                                SaveAndSetActiveWindow (DupeInfoWindow);
                                WriteLn (AltDDupeCheckCall + ' OK!!          ');
                                Write ('CQ mode for   '); {KK1L: 6.73 The stuff normally following this should remain.}
                                RestorePreviousWindow;
                                END;

                            REPEAT
                                PutUpExchangeWindow;
                                DisplayNextQSONumber (TotalContacts + 1);
                                ClearContestExchange (ReceivedData);
                                ExchangeHasBeenSent := False;
                            UNTIL NOT SearchAndPounce;

                            ClearContestExchange (ReceivedData);

                            IF ActiveRadio = RadioOne THEN
                                BEGIN
                                CQRITEnabled := Radio1Type = TS850;
                                END
                            ELSE
                                CQRITEnabled := Radio2Type = TS850;

                            DisplayAutoSendCharacterCount;

                            IF CallWindowString = '' THEN
                                BEGIN
                                RemoveWindow (ExchangeWindow);
                                ResetSavedWindowListAndPutUpCallWindow;
                                END;
                            END;

                        IF (CWMessageCommand = CWCommandControlEnter) THEN
                            BEGIN
                            BeSilent := True;
                            CWMessageCommand := NoCWCommand;

                            IF Length (CallWindowString) > 1 THEN
                                BEGIN
                                CallsignICameBackTo := CallWindowString;
                                Exit;
                                END;
                            END;

                        IF ((ExtendedKey = F1) OR (ExtendedKey = F2)) THEN
                            IF DDXState <> Off THEN
                                BEGIN
                                DDXState := WaitingForCQ;
                                DDX (MaybeSendANewCall);
                                END;

                        IF ExtendedKey = F9 THEN DDX (RepeatCallsign);
                        END
                    ELSE
                        CASE ExtendedKey OF

                            DownArrow:    { Request to enter exchange before call }
                                IF ActiveWindow <> ExchangeWindow THEN   {KK1L: 6.68 fixes multi downarrows causing problems}
                                    IF ExchangeWindowString = '' THEN
                                        BEGIN
                                        {QuickDisplay2('trouble brewing');} {KK1L testing}
                                        SaveSetAndClearActiveWindow (ExchangeWindow);
                                        END
                                    ELSE
                                        BEGIN
                                        {QuickDisplay2('more trouble brewing');}
                                        ActivateExchangeWindow;
                                        END
                                ELSE RestorePreviousWindow; {KK1L: 6.68 now downarrow here works like in S&P mode}

                            UpArrow: RestorePreviousWindow;

//                            AltC: IF (ActiveMode = CW) OR DVKEnable OR DVPEnable THEN
                              AltC:
                                      BEGIN
                                      AutoCQResume (False);

                                      IF ValidCallCharacter (UpCase (KeyPressedMemory)) THEN
                                          BEGIN
                                          CallWindowString := UpCase (KeyPressedMemory);
                                          Write (CallWindowString);
                                          END;
                                      END;

//                            AltQ: IF (ActiveMode = CW) OR DVKEnable OR (DVPEnable) THEN
                              AltQ:
                                      BEGIN
                                      EscapeDeletedCallEntry := CallWindowString;
                                      CallWindowString := '';
                                      RemoveWindow (PossibleCallWindow);
                                      ClearWindow (CallWindow);
                                      AutoCQ;

                                      IF ValidCallCharacter (UpCase (KeyPressedMemory)) THEN
                                          BEGIN
                                          CallWindowString := UpCase (KeyPressedMemory);
                                          Write (CallWindowString);
                                          END;
                                      END;

                            END;  { of case }

                    END; { of null key case }

            CarriageReturn:
                IF Length (CallWindowString) > 1 THEN
                    BEGIN
                    CallsignICameBackTo := CallWindowString;
                    Exit;
                    END
                ELSE
                    IF Length (CallWindowString) = 0 THEN
//                        IF (ActiveMode = CW) OR DVPEnable OR DVKEnable THEN
                        IF (true) THEN
                            BEGIN
                            InactiveRigCallingCQ := False;
                            SetUpToSendOnActiveRadio;

                            IF MessageEnable AND NOT ((CWTone = 0) AND Debug) THEN
                                SendFunctionKeyMessage (F1, CQOpMode);

                            { This is where the initial CQ gets sent if you
                              are in debug mode! }

                            IF DDXState <> Off THEN
                                BEGIN
                                DDXState := WaitingForCQ;
                                DDX (MaybeSendANewCall);
                                END
                            END;
            END;  { of case }
    UNTIL FALSE;
    END;



FUNCTION GotExchange: BOOLEAN;

{ This function takes care of putting up the exchange menu and operator
  I/O until an exchange is entered or an escape to quit.  A true response
  means that a good exchange has been found.  If the big + key is used to
  enter the exchange instead of a return, the flag QuickQSL is set true. }

LABEL ControlEnterCommand1, ControlEnterCommand2;

VAR CharPtr: INTEGER;
    Key, ExtendedKey: CHAR;
    WindowString: Str80;

    BEGIN
    OpMode := CQOpMode;

    GotExchange := False;
    QuickQSL    := NoQuickQSLKey;
    TailEnding  := False;

    LookingForCQExchange := True;

    IF ReminderPostedCount = 0 THEN
        QuickDisplay ('Enter exchange.  Press ENTER to log, ESCAPE to abort QSO.');

    IF ActiveMode = CW THEN PutUpExchangeMenu;

    SaveSetAndClearActiveWindow (ExchangeWindow);

    IF ExchangeWindowString = '' THEN
        BEGIN
        IF ReadInLog THEN
            ExchangeWindowString := ReadInLogExchange
        ELSE
            BEGIN
            ExchangeWindowString := InitialExchangeEntry (CallWindowString);

            {TR6.74 - need to do this since IntialExchangeEntry does not }

            IF InitialExchangeOverwrite THEN
                InitialExchangePutUp := ExchangeWindowString <> '';
            END;

        END;

    {KK1L: 6.73 I want to add a space in front of the InitialExchange for K9PG. I do this in InitialExchangeEntry.}
    {GetRidOfPrecedingSpaces (ExchangeWindowString);}

    IF ExchangeWindowString <> '' THEN
        BEGIN
        Write (ExchangeWindowString);
        IF InitialExchangeCursorPos = AtStart THEN GoToXY (1, 1);
        END;

    ExchangeWindowCursorPosition := WhereY;

    IF LeaveCursorInCallWindow THEN RestorePreviousWindow;

    DDX (SendExchange);

    IF DualingCQState = WaitingForCallsignInput THEN
        DualingCQState := DualSendingExchange;

    IF LogWithSingleEnter THEN
        BEGIN
        IF ParametersOkay (CallWindowString,
                           ExchangeWindowString,
                           ActiveBand,
                           ActiveMode,
                           DisplayedFrequency,
                           ReceivedData) THEN
            BEGIN
            GotExchange := True;
            RemoveWindow (ExchangeWindow);
            LookingForCQExchange := False;
            Exit;
            END;
        END;

    { We are now ready to accept input.  The CQ EXCHANGE is being sent,
      all of the windows are setup and we are ready to go }

    IF (ActiveMode = CW) AND AlwaysCallBlindCQ THEN
        SendExchangeKeyWhenCWHasStopped := F7;

    REPEAT
    MILLISLEEP; // KS does this work????
        IF ActiveWindow = CallWindow THEN
            BEGIN
            WindowString := CallWindowString;

            IF Length (WindowString) > 0 THEN
                FOR CharPtr := 1 TO Length (WindowString) DO
                    IF WindowString [CharPtr] = '?' THEN
{                        IF InsertMode THEN                   Version 6.22
                            GoToXY (CharPtr + 1, WhereY)
                        ELSE}
                            GoToXY (CharPtr, WhereY);

            WindowEditor (WindowString, Key, ExtendedKey);

            CallWindowString := WindowString;

            CASE Key OF

                EscapeKey:
                    BEGIN
                    RemoveWindow (ExchangeWindow);
                    GotExchange := False;
                    LookingForCQExchange := False;
                    Exit;
                    END;

                SpaceBar: IF SpaceBarDupeCheckEnable THEN
                              IF WindowDupeCheck THEN
                                  BEGIN
                                  RemoveWindow (ExchangeWindow);
                                  GotExchange := False;
                                  LookingForCQExchange := False;
                                  Exit;
                                  END;


                NullKey:
                    CASE ExtendedKey OF
                        UpArrow, DownArrow:
                            BEGIN      {KK1L testing}
                            ActivateExchangeWindow;
                            {QuickDisplay2('ActivateExchangeWindow2');}
                            END;

                        ELSE
                            BEGIN
                            ProcessExchangeFunctionKey (ExtendedKey);

                            IF CWMessageCommand = CWCommandControlEnter THEN
                                BEGIN
                                BeSilent := True;
                                CWMessageCommand := NoCWCommand;
                                GoTo ControlEnterCommand1;
                                END;
                            END;
                        END;


                CarriageReturn:
                    BEGIN

ControlEnterCommand1:

                    IF ParametersOkay (CallWindowString,
                                       ExchangeWindowString,
                                       ActiveBand,
                                       ActiveMode,
                                       DisplayedFrequency,
                                       ReceivedData) THEN
                        BEGIN
                        GotExchange := True;
                        RemoveWindow (ExchangeWindow);
                        LookingForCQExchange := False;
                        Exit;
                        END
                    ELSE
                        BEGIN
                        IF ParameterOkayMode = QSLButDoNotLog THEN
                            BEGIN
                            SendCorrectCallIfNeeded;
                            Send73Message;
                            QuickDisplay ('PLEASE FIX EXCHANGE for ' + CallsignICameBackTo + '!!');
                            END;

                        ActivateExchangeWindow;
                        END;
                   END

                ELSE
                    BEGIN
                    IF (Key = QuickQSLKey1) OR
                       (Key = QuickQSLKey2) THEN
                        BEGIN
                        IF ParametersOkay (CallWindowString,
                                           ExchangeWindowString,
                                           ActiveBand,
                                           ActiveMode,
                                           DisplayedFrequency,
                                           ReceivedData) THEN
                               BEGIN
                               GotExchange := True;
                               RemoveWindow (ExchangeWindow);
                               IF Key = QuickQSLKey1 THEN
                                   QuickQSL := QuickKey1
                               ELSE
                                   QuickQSL := QuickKey2;

                               LookingForCQExchange := False;
                               Exit;
                               END
                           ELSE
                               BEGIN
                               IF ParameterOkayMode = QSLButDoNotLog THEN
                                   BEGIN
                                   SendCorrectCallIfNeeded;
                                   Send73Message;
                                   QuickDisplay ('PLEASE FIX EXCHANGE!!');
                                   END;
                               ActivateExchangeWindow;
                               END;
                        END
                    ELSE
                        IF Key = TailEndKey THEN
                            BEGIN
                            IF ParametersOkay (CallWindowString,
                                               ExchangeWindowString,
                                               ActiveBand,
                                               ActiveMode,
                                               DisplayedFrequency,
                                               ReceivedData) THEN
                                BEGIN
                                GotExchange := True;
                                RemoveWindow (ExchangeWindow);
                                TailEnding := True;
                                LookingForCQExchange := False;
                                Exit;
                                END
                            ELSE
                                BEGIN
                                IF ParameterOkayMode = QSLButDoNotLog THEN
                                    BEGIN
                                    SendCorrectCallIfNeeded;
                                    Send73Message;
                                    QuickDisplay ('PLEASE FIX EXCHANGE for ' + CallsignICameBackTo + '!!');
                                    END;
                                ActivateExchangeWindow;
                                END;
                            END
                        ELSE
                            ActivateExchangeWindow;
                    END;
                END; { of case}
            END;  { of call window active }

        IF ActiveWindow = ExchangeWindow THEN
            BEGIN
            WindowEditor (ExchangeWindowString, Key, ExtendedKey);

            CASE Key OF

                EscapeKey: BEGIN
                           RemoveWindow (ExchangeWindow);
                           GotExchange := False;
                           LookingForCQExchange := False;
                           Exit;
                           END;

                SpaceBar: IF SpaceBarDupeCheckEnable THEN
                              IF WindowDupeCheck THEN
                                  BEGIN
                                  RemoveWindow (ExchangeWindow);
                                  GotExchange := False;
                                  LookingForCQExchange := False;
                                  Exit;
                                  END;

                NullKey:
                    CASE ExtendedKey OF
                        UpArrow, DownArrow:
                            BEGIN      {KK1L testing}
                            RestorePreviousWindow;
                            {QuickDisplay2('RestorePreviousWindow2');}
                            END;

                        ELSE
                            BEGIN
                            ProcessExchangeFunctionKey (ExtendedKey);

                            IF CWMessageCommand = CWCommandControlEnter THEN
                                BEGIN
                                BeSilent := True;
                                CWMessageCommand := NoCWCommand;
                                GoTo ControlEnterCommand2;
                                END;
                            END;

                        END;



                CarriageReturn:
                    BEGIN

ControlEnterCommand2:

                    IF ParametersOkay (CallWindowString,
                                       ExchangeWindowString,
                                       ActiveBand,
                                       ActiveMode,
                                       DisplayedFrequency,
                                       ReceivedData) THEN
                        BEGIN
                        GotExchange := True;
                        RemoveWindow (ExchangeWindow);
                        LookingForCQExchange := False;
                        Exit;
                        END
                    ELSE
                        IF ParameterOkayMode = QSLButDoNotLog THEN
                            BEGIN
                            SendCorrectCallIfNeeded;
                            Send73Message;
                            QuickDisplay ('PLEASE FIX EXCHANGE for ' + CallsignICameBackTo + '!!');
                            END;
                    END;

                ELSE
                    BEGIN
                    IF Key = TailEndKey THEN
                        BEGIN
                        IF ParametersOkay (CallWindowString,
                                           ExchangeWindowString,
                                           ActiveBand,
                                           ActiveMode,
                                           DisplayedFrequency,
                                           ReceivedData) THEN
                            BEGIN
                            GotExchange := True;
                            RemoveWindow (ExchangeWindow);
                            TailEnding := True;
                            LookingForCQExchange := False;
                            Exit;
                            END
                        ELSE
                            IF ParameterOkayMode = QSLButDoNotLog THEN
                                BEGIN
                                SendCorrectCallIfNeeded;
                                Send73Message;
                                QuickDisplay ('PLEASE FIX EXCHANGE for ' + CallsignICameBackTo + '!!');
                                END;

                        END
                    ELSE
                        IF (Key = QuickQSLKey1) OR
                           (Key = QuickQSLKey2) THEN
                            BEGIN
                            IF ParametersOkay (CallWindowString,
                                               ExchangeWindowString,
                                               ActiveBand,
                                               ActiveMode,
                                               DisplayedFrequency,
                                               ReceivedData) THEN
                                BEGIN
                                GotExchange := True;
                                RemoveWindow (ExchangeWindow);
                                IF Key = QuickQSLKey1 THEN
                                    QuickQSL := QuickKey1
                                ELSE
                                    QuickQSL := QuickKey2;
                                LookingForCQExchange := False;
                                Exit;
                                END
                            ELSE
                                IF ParameterOkayMode = QSLButDoNotLog THEN
                                    BEGIN
                                    SendCorrectCallIfNeeded;
                                    Send73Message;
                                    QuickDisplay ('PLEASE FIX EXCHANGE for ' + CallsignICameBackTo + '!!');
                                    END;

                            END
                        ELSE
                            RestorePreviousWindow;
                    END;
                END; { of case}
            END;  { of exchange window active }

    UNTIL FALSE;
    END;



PROCEDURE OperateContest;

VAR MTotals: MultTotalArrayType;
    TempKey: CHAR;
    RememberInactiveCQ: BOOLEAN;
    RememberTime: TimeRecord;
    DXSpot: DXSpotType;

    BEGIN
    ReadInConfigFile ('');

    IF FakePacket THEN
        BEGIN
        WITH DXSpot DO
            BEGIN
            Call := 'TO0R';
            Frequency       := 1831000;
            QSXFrequency    := 1835000;
            FrequencyString := '1831.0';
            Band := Band160;
            Mode := CW;
            SourceCall := 'K7RAT';
            END;

        Packet.PushPacketSpot (DXSpot);

        WITH DXSpot DO
            BEGIN
            Call := 'ON4UN';
            Frequency       := 1832000;
            QSXFrequency    := 0;
            FrequencyString := '1832.0';
            Band := Band160;
            Mode := CW;
            SourceCall := 'K7RAT';
            END;

        Packet.PushPacketSpot (DXSpot);

        WITH DXSpot DO
            BEGIN
            Call := 'GW3YDX';
            Frequency       := 1833000;
            QSXFrequency    := 0;
            FrequencyString := '1833.0';
            Band := Band160;
            Mode := CW;
            SourceCall := 'K7RAT';
            END;

        Packet.PushPacketSpot (DXSpot);

        WITH DXSpot DO
            BEGIN
            Call := 'GI3OQR';
            Frequency       := 1834000;
            QSXFrequency    := 0;
            FrequencyString := '1834.0';
            Band := Band160;
            Mode := CW;
            SourceCall := 'K7RAT';
            END;

        Packet.PushPacketSpot (DXSpot);


        WITH DXSpot DO
            BEGIN
            Call := 'SM4CAN';
            Frequency       := 1835000;
            QSXFrequency    := 0;
            FrequencyString := '1835.0';
            Band := Band160;
            Mode := CW;
            SourceCall := 'K7RAT';
            END;

        Packet.PushPacketSpot (DXSpot);

        WITH DXSpot DO
            BEGIN
            Call := 'SP5EWY';
            Frequency       := 1836000;
            QSXFrequency    := 1846000;
            FrequencyString := '1836.0';
            Band := Band160;
            Mode := CW;
            SourceCall := 'K7RAT';
            END;

        Packet.PushPacketSpot (DXSpot);

        WITH DXSpot DO
            BEGIN
            Call := 'OZ8RO';
            Frequency       := 1837000;
            QSXFrequency    := 0;
            FrequencyString := '1837.0';
            Band := Band160;
            Mode := CW;
            SourceCall := 'K7RAT';
            END;

        Packet.PushPacketSpot (DXSpot);

        WITH DXSpot DO
            BEGIN
            Call := 'G3XTT';
            Frequency       := 1838000;
            QSXFrequency    := 0;
            FrequencyString := '1838.0';
            Band := Band160;
            Mode := CW;
            SourceCall := 'K7RAT';
            END;

        Packet.PushPacketSpot (DXSpot);

        WITH DXSpot DO
            BEGIN
            Call := 'EA6ACC';
            Frequency       := 1839000;
            QSXFrequency    := 0;
            FrequencyString := '1839.0';
            Band := Band160;
            Mode := CW;
            SourceCall := 'K7RAT';
            END;

        Packet.PushPacketSpot (DXSpot);

        WITH DXSpot DO
            BEGIN
            Call := 'TI9CF';
            Frequency       := 1840000;
            QSXFrequency    := 0;
            FrequencyString := '1840.0';
            Band := Band160;
            Mode := CW;
            SourceCall := 'K7RAT';
            END;

        Packet.PushPacketSpot (DXSpot);
        END;

    IF FakeBandMap THEN
        BEGIN
        AddBandMapEntry ('TO0R',   1831000, 1835000, CW, False,  True, 60);
        AddBandMapEntry ('ON4UN',  1832000,       0, CW, False, False, 55);
        AddBandMapEntry ('GW3YDX', 1833000,       0, CW, True,  False, 50);
        AddBandMapEntry ('GI3OQR', 1834000,       0, CW, False, False, 45);
        AddBandMapEntry ('SM4CAN', 1835000,       0, CW, True,  False, 40);
        AddBandMapEntry ('SP5EWY', 1836000, 1846000, Phone, True,  False, 35);
        AddBandMapEntry ('OZ8RO',  1837000,       0, Phone, True,  False, 30);
        AddBandMapEntry ('G3XTT',  1838000,       0, Phone, False, True,  25);
        AddBandMapEntry ('EA6ACC', 1839000,       0, Phone, False, False, 20);
        AddBandMapEntry ('TI9CF',  1840000,       0, Phone, False, False, 10);
        END;

    IF SayHiEnable THEN
        DisplayNamePercentage (TotalNamesSent + VisibleLog.NumberNamesSentInEditableLog, TotalContacts);

    SetStereoPin (StereoControlPin, StereoPinState); {KK1L: 6.71}
    DisplayRadio (ActiveRadio);
    DisplayTotalScore (TotalScore);
    ClearContestExchange (ReceivedData);
    SetUpToSendOnActiveRadio;

    REPEAT
        SeventyThreeMessageSent := False;
        ExchangeHasBeenSent := False;
        LogBadQSOString := '';

        IF TailEnding THEN
            BEGIN
            PreviousQSOReceivedData := ReceivedData;
            CallsignICameBackTo     := TailEndCallString;
            CallWindowString        := TailEndCallString;
            ClrScr;
            Write (CallWindowString);
            ClearContestExchange (ReceivedData);
            ExchangeWindowString := '';
            END
        ELSE
            BEGIN
            Sheet.MultSheetTotals (MTotals);
            UpdateTotals;
            IF ReceivedData.DomesticMult OR ReceivedData.DXMult OR ReceivedData.ZoneMult THEN
                VisibleLog.ShowRemainingMultipliers;

            IF ReceivedData.DomesticMult THEN
                VisibleLog.DisplayGridMap (ActiveBand, ActiveMode);

            ClearContestExchange (ReceivedData);

            GetInitialCall;

            WindowDupeCheckCall := CallsignICameBackTo;
            END;

        IF (ActiveMode = CW) AND (NOT TailEnding) AND (NOT CallAlreadySent) THEN
            BEGIN
            IF MessageEnable AND NOT BeSilent THEN
                IF NOT (Debug AND (CWTone = 0)) THEN
                    AddStringToBuffer (CallsignICameBackTo, CWTone);

            CallAlreadySent := True;
            END;

        IF (ActiveMode = Digital) AND (NOT CallAlreadySent) THEN
            StartRTTYTransmission (CallsignICameBackTo + ' ');

        IF DualingCQState = DualSendingCQ THEN
            BEGIN
            SwapRadios;
            DualingCQState := DualSendingExchange;
            END;

        IF AutoDupeEnableCQ AND
           VisibleLog.CallIsADupe (CallsignICameBackTo, ActiveBand, ActiveMode) THEN
            BEGIN
            IF ReminderPostedCount = 0 THEN
                QuickDisplay (CallsignICameBackTo + ' is a dupe!!');

            IF DupeCheckSound <> DupeCheckNoSound THEN Tone.DoABeep (ThreeHarmonics);

            ShowName (CallsignICameBackTo);
            DisplayCountryName (CallsignICameBackTo);
            DisplayUserInfo (CallsignICameBackTo);

            IF ActiveMode = CW THEN
                BEGIN
                IF QTCsEnabled THEN
                    BEGIN
                    DisplayQTCNumber (NumberQTCsThisStation (StandardCallFormat (CallWindowString, False)));

                    IF NumberQTCsThisStation (StandardCallFormat (CallWindowString, False)) < 10 THEN
                        BEGIN
                        IF QTCsEnabled AND (MyContinent = Europe) THEN
                            BEGIN
                            AddStringToBuffer (' B4 ', CWTone);
                            WAEQTC (CallWindowString);
                            END
                        ELSE
                            IF MessageEnable AND NOT BeSilent THEN
                                SendCrypticMessage (QSOBeforeMessage);
                        END
                    ELSE
                        IF MessageEnable AND NOT BeSilent THEN
                            SendCrypticMessage (QSOBeforeMessage);
                    END
                ELSE
                    IF MessageEnable AND NOT BeSilent THEN
                        SendCrypticMessage (QSOBeforeMessage);

                IF DualingCQState <> NoDualingCQs THEN
                    DualingCQState := SendingDupeMessage;
                END;

            IF ActiveMode = Digital THEN
                FinishRTTYTransmission (QSOBeforeMessage);

            IF ActiveMode = Phone THEN
                BEGIN
                SendCrypticMessage (QSOBeforePhoneMessage);

                Write (' DUPE!!');
                EscapeDeletedCallEntry := CallWindowString;
                CallWindowString := '';

                IF QTCsEnabled THEN
                    DisplayQTCNumber (NumberQTCsThisStation (StandardCallFormat (CallWindowString, False)))
                ELSE
                    BEGIN
                    MarkTime (RememberTime);

                    REPEAT
                        IF ActiveMultiPort <> nil THEN CheckMultiState;
                        UpdateTimeAndRateDisplays (True, True);
                        Packet.CheckPacket;
                    UNTIL ElaspedSec100 (RememberTime) >= 30;

                    ClrScr;
                    END;
                END;

            IF AutoDisplayDupeQSO THEN
                BEGIN
                ShowPreviousDupeQSOs (CallsignICameBackTo, ActiveBand, ActiveMode);
                EditableLogDisplayed := True;
                END;

            IF TailEnding THEN
                BEGIN
                ReceivedData.SearchAndPounce := False;
                LogContact (PreviousQSOReceivedData);
                TailEnding := False;
                END;

            IF ContestName <> 'General QSOs' THEN
                BEGIN
                VisibleLog.ShowMultiplierStatus (CallsignICameBackTo);
                VisibleLog.ShowQSOStatus        (CallsignICameBackTo);
                END;

            DisplayUserInfo (CallsignICameBackTo);
            DisplayBeamHeading (CallsignICameBackTo);
            END

        ELSE       { not a dupe or not AutoDupeEnable }
            BEGIN
            IF MessageEnable AND NOT BeSilent THEN
                IF NOT (Debug AND (CWTone = 0)) THEN
                    AddOnCQExchange;

            ShowName (CallsignICameBackTo);
            DisplayGridSquareStatus (CallsignICameBackTo);

            DisplayUserInfo (CallsignICameBackTo);
            DisplayBeamHeading (CallsignICameBackTo);
            DisplayCountryName (CallsignICameBackTo);

            IF QTCsEnabled THEN
                DisplayQTCNumber (NumberQTCsThisStation (CallWindowString));

            IF ContestName <> 'General QSOs' THEN
                BEGIN
                VisibleLog.ShowMultiplierStatus (CallWindowString);
                VisibleLog.ShowQSOStatus        (CallWindowString);
                END;

            VisibleLog.DoPossibleCalls      (CallWindowString);

            IF TailEnding THEN
                BEGIN
                ReceivedData.SearchAndPounce := False;
                LogContact (PreviousQSOReceivedData);
                TailEnding := False;
                END;

            IF GotExchange THEN
                BEGIN
                IF TailEnding THEN
                    BEGIN
                    TailEndCallString := '';
                    SendCorrectCallIfNeeded;
                    IF MessageEnable THEN AddStringToBuffer (TailEndMessage + ' ', CWTone);

                    REPEAT
                        REPEAT millisleep UNTIL NewKeyPressed;
                        TempKey := UpCase (NewReadKey);
                        IF TempKey <> CarriageReturn THEN
                            IF ValidCallCharacter (TempKey) OR (TempKey = '?') THEN
                                BEGIN
                                IF MessageEnable THEN
                                    AddStringToBuffer (TempKey, CWTone);

                                TailEndCallString := TailEndCallString + TempKey;
                                END;
                    UNTIL TempKey = CarriageReturn;

                    IF TailEndCallString = '' THEN
                        BEGIN
                        TailEnding := False;
                        ReceivedData.SearchAndPounce := False;
                        LogContact (ReceivedData);
                        END;
                    END
                ELSE
                    BEGIN
                    IF ActiveMode = CW THEN
                        BEGIN
                        RememberInactiveCQ := InactiveRigCallingCQ;

                        SendCorrectCallIfNeeded;

                        IF NOT (Debug AND (CWTone = 0)) THEN
                            BEGIN
                            IF MessageEnable AND NOT (BeSilent OR TailEnding) THEN
                                IF QuickQSL <> NoQuickQSLKey THEN
                                    BEGIN
                                    IF QuickQSL = QuickKey1 THEN
                                        SendCrypticMessage (QuickQSLMessage1)
                                    ELSE
                                        SendCrypticMessage (QuickQSLMessage2);
                                    END
                                ELSE
                                    IF ExchangeInformation.Age THEN
                                        BEGIN
                                        IF ReceivedData.Age = '00' THEN
                                            Send88Message
                                        ELSE
                                            Send73Message;
                                        END
                                    ELSE
                                        Send73Message;

                            InactiveRigCallingCQ := RememberInactiveCQ;
                            END;

                        END
                    ELSE
                        IF MessageEnable AND NOT BeSilent THEN
                            BEGIN
                            IF QuickQSL <> NoQuickQSLKey THEN
                                SendCrypticMessage (QuickQSLPhoneMessage)
                            ELSE
                                Send73Message;
                            END;

                    IF DualingCQState = DualGettingExchange THEN
                         DualingCQState := DualSendingQSL;

                    BeSilent := False;

                    IF NOT TailEnding THEN
                        BEGIN
                        ReceivedData.SearchAndPounce := False;
                        LogContact (ReceivedData);
                        END;

                    END;
                END
            ELSE
                IF CWSpeedFromDatabase AND (RememberCWSpeed > 0) THEN
                    BEGIN
                    SetSpeed (RememberCWSpeed);
                    RememberCWSpeed := 0;
                    END;
            END;
    UNTIL False;
    END;



