PROCEDURE LogContact (VAR RXData: ContestExchange);

{ This procedure will log the contact just completed.  It will be
  pushed onto the editable log and the log entry popped off the editable
  log will be examined and written to the LOG.DAT file.

  New for October 2024 is the ability to have two (or more) domestic
  QTHs separated with /'s and generate multiple QSO entries.  }

VAR LogString: Str80;
    FirstSlashPosition, Address: INTEGER;

    BEGIN
    IF Pos ('/', RXData.QTHString) > 0 THEN  { Someone is sending multiple counties }
        BEGIN
        FirstSlashPosition := Pos ('/', RXData.QTHString);
        RXData.LeftOverQTH := Copy (RXData.QTHString,
                                    FirstSlashPosition + 1,
                                    Length (RXData.QTHString) - FirstSlashPosition);
        RXData.QTHString := Copy (RXData.QTHString, 1, FirstSlashPosition - 1);
        END;

    RXData.NumberSent := QSONumberForThisQSO;
    QSONumberForThisQSO := -1;

    RXData.TimeSeconds := GetTimeSeconds;    { Add seconds for better resolution for those who want it }
    RXData.Radio := ActiveRadio;

    VisibleDupeSheetChanged := True;

    IF ActivePacketPort <> nil THEN
        Packet.DeletePacketEntry (RXData.Callsign, RXData.Band, RXData.Mode);

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

  { We now go into a loop that will log the QSO - and also take a peek at the
    LeftOverQTH string to see if more QSOs can be logged }

  REPEAT

      { Special weirdness to deal with dupes or rovers in new grids }

      IF VisibleLog.CallIsADupe (RXData.Callsign, RXData.Band, RXData.Mode) OR
           ((ActiveDomesticMult = GridSquares) AND RoverCall (RXData.Callsign) AND (NumberGridSquaresInList > 0)) THEN
            IF NOT (ActiveQSOPointMethod = AlwaysOnePointPerQSO) THEN  { Used for like internet sprints? }
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

        PushLogStringIntoEditableLogAndLogPopedQSO (LogString);

        IF Trace THEN Write ('&');

        { RXData has the data from the QSO we just worked - not the one that
          popped off the top of the editable window }

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

        IF Trace THEN Write ('(');

        IF FloppyFileSaveFrequency > 0 THEN
            IF QSOTotals [All, Both] > 0 THEN
                IF QSOTotals [All, Both] MOD FloppyFileSaveFrequency = 0 THEN
                    SaveLogFileToFloppy;

        IF Trace THEN Write (')');

        IF UpdateRestartFileEnable THEN Sheet.SaveRestartFile;

        BeSilent := False;
        NameCallsignPutUp := '';

        IF Trace THEN Write ('-');

        IF CWSpeedFromDataBase AND (RememberCWSpeed <> 0) THEN
            BEGIN
            SetSpeed (RememberCWSpeed);
            RememberCWSpeed := 0;
            DisplayCodeSpeed (CodeSpeed, CWEnabled, False, ActiveMode);
            END;

        IF (DDXState <> Off) AND Debug AND (CWTone = 0) THEN
            IF Random (100) = 0 THEN
                BandUp;

        IF BandMapEnable THEN
            BEGIN
            UpdateBandMapMultiplierStatus;
            {KK1L: 6.64 Need to change dupe status for this contact as well - does the new display }
            UpdateBandMapDupeStatus(RXData.Callsign, RXData.Band, RXData.Mode, True);
            END;

        { New for Jan 2023 - send QSO data to UDP port }

        IF (QSO_UDP_IP <> '') AND (QSO_UDP_Port <> 0) THEN
            BEGIN
            RXData.Date := GetFullDateString;
            SendQSOToUDPPort (RXData);
            END;

        { New for Mar 2024 - send to N1MM using WSJT port.  Note that
          we always send N1MM QSOs immediately because it's the only
          time we really have all of the information we need including
          the exact frequency }

        IF N1MM_QSO_Portal.Output_IPAddress <> '' THEN
            BEGIN
            RXData.Date := GetFullDateString;
            N1MM_QSO_Portal.SendQSOToN1MM (RXData);
            END;

        { See if we are done or if there are more QTHs to process }

        IF RXData.LeftOverQTH = '' THEN Exit;

        RXData.QTHString := RXData.LeftOverQTH;
        RXData.LeftOverQTH := '';

        { See if we still have multiple QTHs }

        IF Pos ('/', RXData.QTHString) > 0 THEN  { Still multiple counties }
            BEGIN
            FirstSlashPosition := Pos ('/', RXData.QTHString);
            RXData.LeftOverQTH := Copy (RXData.QTHString,
                                        FirstSlashPosition + 1,
                                        Length (RXData.QTHString) - FirstSlashPosition);
            RXData.QTHString := Copy (RXData.QTHString, 1, FirstSlashPosition - 1);
            END;

        { Protect from a bad QTH }

        IF NOT FoundDomesticQTH (RXData) THEN Exit;

    UNTIL False;

    DisplayBandMap;  { Update the bandmap with new dupe and mult data }
    END;



