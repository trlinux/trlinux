PROCEDURE TBSIQ_LogContact (VAR RXData: ContestExchange);

{ This procedure will log the contact just completed.  It will be
  pushed onto the editable log and the log entry popped off the editable
  log will be examined and written to the LOG.DAT file.

  It is highly leveraged from the LogContact procedure in logsubs2.pas.
  And in October 2024, we added multi-county parsing using / in the
  exchange string. }

VAR LogString: Str80;
    FirstSlashPosition: INTEGER;
    WeAreLooping: BOOLEAN;

    BEGIN
    WeAreLooping := False;  { Way to decide if I should worry about zeroing out QSO points }

    IF Pos ('/', RXData.QTHString) > 0 THEN  { Someone is sending multiple counties }
        BEGIN
        FirstSlashPosition := Pos ('/', RXData.QTHString);
        RXData.LeftOverQTH := Copy (RXData.QTHString,
                                    FirstSlashPosition + 1,
                                    Length (RXData.QTHString) - FirstSlashPosition);
        RXData.QTHString := Copy (RXData.QTHString, 1, FirstSlashPosition - 1);
        WeAreLooping := True;
        END;

    RXData.TimeSeconds := GetTimeSeconds;

    VisibleDupeSheetChanged := True;

    IF LastDeletedLogEntry <> '' THEN
        BEGIN
        LastDeletedLogEntry := '';
        RemoveWindow (QuickCommandWindow);
        END;

    LastQSOLogged := RXData;

    IF TenMinuteRule <> NoTenMinuteRule THEN
        UpdateTenMinuteDate (RXData.Band, RXData.Mode);

    { IF NOT TailEnding THEN RemoveWindow (PossibleCallWindow); }

    { This is simplified from LOGSUBS2 }

    REPEAT  { We are going to loop if we have multiple QTHs }

        { We used to set the QSO points to zero if this was a dupe - but it
          will always be a dupe - and we really want to process the mults
          as well }

        { Well - we need to for normal contests }

        IF NOT DisplayDupeQTHs THEN RXData.QSOPoints := 0;

        VisibleLog.ProcessMultipliers (RXData);  { Yes! This is in LOGEDIT.PAS }
        LogString := MakeLogString (RXData);     { Yes! This is in LOGSTUFF.PAS }

        IF (RXData.Band >= Band160) AND (RXData.Band <= Band10) THEN
            Inc (ContinentQSOCount [RXData.Band, RXData.QTH.Continent]);

        TBSIQ_PushLogStringIntoEditableLogAndLogPopedQSO (LogString, True);

        IF DoingDomesticMults AND
           (MultByBand OR MultByMode) AND
           (RXData.DomesticQTH <> '') THEN
               VisibleLog.ShowDomesticMultiplierStatus (RXData.DomMultQTH);

        Inc (NumberContactsThisMinute);
        NumberQSOPointsThisMinute := NumberQSOPointsThisMinute + RXData.QSOPoints;

       {   DisplayTotalScore (TotalScore); }

        IF FloppyFileSaveFrequency > 0 THEN
            IF QSOTotals [All, Both] > 0 THEN
                IF QSOTotals [All, Both] MOD FloppyFileSaveFrequency = 0 THEN
                    SaveLogFileToFloppy;

        IF UpdateRestartFileEnable THEN Sheet.SaveRestartFile;

        UpdateTotals;
        DisplayTotalScore (TotalScore);
        VisibleLog.ShowRemainingMultipliers;

        IF BandMapEnable THEN
            BEGIN
            UpdateBandMapMultiplierStatus;
            UpdateBandMapDupeStatus (RXData.Callsign, RXData.Band, RXData.Mode, True);
            END;

        { New for Jan 2023 - send QSO data to UDP port }

        IF (QSO_UDP_IP <> '') AND (QSO_UDP_Port <> 0) THEN
            BEGIN
            RXData.Date := GetFullDateString;
            SendQSOToUDPPort (RXData);
            END;

        { New for Mar 2024 - send to N1MM using WSJT port.  We always
          send the QSO immediately because this is when we have all
          of the information including frequency data available }

        IF N1MM_QSO_Portal.Output_IPAddress <> '' THEN
            BEGIN
            RXData.Date := GetFullDateString;
            N1MM_QSO_Portal.SendQSOToN1MM (RXData);
            END;

        { See if we are done }

        IF RXData.LeftOverQTH = '' THEN Exit;

        RXData.QTHString := RXData.LeftOverQTH;
        RXData.LeftOverQTH := '';

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
    DisplayBandMap;
    END;



