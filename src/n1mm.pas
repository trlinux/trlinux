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

UNIT N1MM;

{ Here we have various routines that are used when interfacing to N1MM over
  the LAN. Currently - the only operation that can be performmed is to listen
  to a UDP port for QSO information.

  The necessary routine to log the QSO is also here. }

{$O+}
{$V-}

INTERFACE

USES LogStuff, LogUDP, LogWind, LogDupe, LogEdit, SlowTree, datetimec, Tree, KeyCode,
     Country9, Sockets, UnixType, BaseUnix, portname;

CONST MaxQTCs = 10;

TYPE
    N1MM_Object = OBJECT
        Socket: LONGINT;

        QTC_Count: INTEGER;    { Number of UDP messages cued up }
        QTC_Buffer: ARRAY [0..MaxQTCs - 1] OF STRING;

        UDP_PortNumber: LONGINT;
        UDP_Socket: LONGINT;
        UDP_Message: STRING;

        FUNCTION  Check_UDP_Port: BOOLEAN;  { Returns true if a message was found }
        FUNCTION  GetNextQTC: STRING;       { Pulls the next QTC off the buffer }

        { These next two are the only procedures that should be used to make this work }

        PROCEDURE Heartbeat;                { Call this often to give oxygen to UDP polling }
        PROCEDURE Init;                     { Sets up the UDP port and QTC Buffer }

        { The LogContacts will happen due to the Heartbeat }

        PROCEDURE LogContacts;              { This will take and UDP packets that have been
                                              saved up and push them into the log. Anything
                                              displayed will be generic enough that it will
                                              be okay for classic or TBSIQ modes. }

        PROCEDURE LogN1MMContact (RXData: ContestExchange);  { Logs the QSO }
        PROCEDURE PushLogStringIntoEditableLogAndLogPopedQSO (LogString: Str80; MyQSO: BOOLEAN);
        PROCEDURE PutContactIntoLogFile (LogString: Str80);
        END;

VAR  N1MM_QSO_Portal: N1MM_Object;

IMPLEMENTATION

CONST N1MM_DebugFileName = 'N1MM_debug.txt';

PROCEDURE N1MM_Object.Init;

VAR SocketAddr: TINetSockAddr;
    ConnectResult: INTEGER;
    FileWrite: TEXT;

    BEGIN
    QTC_Count := 0;
    UDP_PortNumber := N1MM_UDP_Port;

    { Setup the UDP port.  }

    UDP_Socket := fpSocket (AF_INET, SOCK_DGRAM, IPPROTO_UDP);

    SocketAddr.sin_family := AF_INET;
    SocketAddr.sin_port := htons (UDP_PortNumber);
    SocketAddr.sin_addr.s_addr := INADDR_ANY;

    ConnectResult := fpBind (UDP_Socket, @SocketAddr, SizeOf (SocketAddr));

    OpenFileForAppend (FileWrite, N1MM_DebugFileName);

    IF ConnectResult <> 0 THEN
        WriteLn (FileWrite, 'Unable to connect to UDP port ', UDP_PortNumber)
    ELSE
        WriteLn (FileWrite, 'Port ', UDP_PortNumber, ' open for input.  Socket = ',  UDP_Socket);

    Close (FileWrite);
    END;



FUNCTION N1MM_Object.GetNextQTC: STRING;

{ Pulls the oldest QTC off the QTC buffer }

VAR Index: INTEGER;

    BEGIN
    IF QTC_Count = 0 THEN
        BEGIN
        GetNextQTC := '';
        Exit;
        END;

    GetNextQTC := QTC_Buffer [0];

    IF QTC_Count > 1 THEN
        FOR Index := 0 TO QTC_Count - 2 DO
            QTC_Buffer [Index] := QTC_Buffer [Index + 1];

    Dec (QTC_Count);
    END;



FUNCTION N1MM_Object.Check_UDP_Port: BOOLEAN;

{ Returns true if a message was found.  The message will be in the string UDP_Message }

VAR BytesRead: INTEGER;
    FDS: Tfdset;
    FileWrite: TEXT;

    BEGIN
    { Set up FDS and inquire if there is some data available }

    fpFd_zero (FDS);
    fpFd_set (UDP_Socket, FDS);
    fpSelect (UDP_Socket+1, @FDS, nil, nil, 0);

    IF fpFd_IsSet (UDP_Socket, FDS) = 0 THEN   { No data yet }
        BEGIN
        Check_UDP_Port := False;
        Exit;
        END;

    { We have some data to read - put it starting at char index 1 of message string }

    BytesRead := fpRecv (UDP_Socket, @UDP_MEssage[1], 255, 0);

    IF BytesRead = -1 THEN
        BEGIN
        Check_UDP_Port := False;
        Exit;
        END;

    SetLength (UDP_Message, BytesRead);

    OpenFileForAppend (FileWrite, N1MM_DebugFileName);
    WriteLn (UDP_Message);
    Close (FileWrite);

    Check_UDP_Port := True;
    END;



PROCEDURE N1MM_Object.Heartbeat;

{ This should be called as often as possible.  It will check to see if a UDP
  message has arrived and put it into the QTC buffer if so.  It will also call
  the routine to take any contents of the QTC buffer and log them. }

    BEGIN
    { Check the UDP port to see if there are any messages waiting }

    WHILE Check_UDP_Port AND (QTC_Count < MaxQTCs) DO
        BEGIN
        QTC_Buffer [QTC_Count] := UDP_Message;
        Inc (QTC_Count);
        END;

    LogContacts;
    END;



PROCEDURE N1MM_Object.PutContactIntoLogFile (LogString: Str80);

VAR Time, QSONumber: INTEGER;
    Call, Exchange, LoggedCallsign: Str20;
    FileWrite: TEXT;

    BEGIN
    IF Copy (LogString, 1, 1) = ';' THEN  { Its a note - just copy it and be done }
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
               Val (Exchange, QSONumber);
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



PROCEDURE N1MM_Object.PushLogStringIntoEditableLogAndLogPopedQSO (LogString: Str80; MyQSO: BOOLEAN);

{ This is pretty much a copy of what is in tbsiq_subs.pas }

VAR RData: ContestExchange;

    BEGIN
    LogString := VisibleLog.PushLogEntry (LogString);

    { LogString is now what popped off the top of the editable window }

    GetRidOfPostcedingSpaces (LogString);

    IF LogString <> '' THEN
        BEGIN
        PutContactIntoLogFile (LogString);

        IF ParseExchangeIntoContestExchange (LogString, RData) THEN
            ProcessPartialCallAndInitialExchange (RData);
        END;
    END;



PROCEDURE N1MM_Object.LogN1MMContact (RXData: ContestExchange);

{ This procedure will log the contact just completed.  It will be
  pushed onto the editable log and the log entry popped off the editable
  log will be examined and written to the LOG.DAT file.

  This is just a minor scale down of the version found in tbsiq_subs.pas which is a
  scaled down version of the one found in logsubs2.pas.  It is left as an exercise to
  the student to see if the one in TBSIQ can just leverage this rountine.  Not much
  was removed from it.

  One possible trouble area here has to do with QSO numbers - but I can't worry too
  much about this at the moment }

VAR LogString: Str80;

    BEGIN
    CalculateQSOPoints (RXData);

    VisibleDupeSheetChanged := True;

    LastTwoLettersCrunchedOn := '';

    IF LastDeletedLogEntry <> '' THEN
        LastDeletedLogEntry := '';

    LastQSOLogged := RXData;

    IF TenMinuteRule <> NoTenMinuteRule THEN
        UpdateTenMinuteDate (RXData.Band, RXData.Mode);

    { This is simplified from LOGSUBS2 }

    IF VisibleLog.CallIsADupe (RXData.Callsign, RXData.Band, RXData.Mode) THEN
        RXData.QSOPoints := 0
    ELSE
        VisibleLog.ProcessMultipliers (RXData);  { This is in LOGEDIT.PAS }

    LogString := MakeLogString (RXData);  { This is in LOGSTUFF.PAS }

    IF (RXData.Band >= Band160) AND (RXData.Band <= Band10) THEN
        Inc (ContinentQSOCount [RXData.Band, RXData.QTH.Continent]);

    PushLogStringIntoEditableLogAndLogPopedQSO (LogString, True);

    IF DoingDomesticMults AND
       (MultByBand OR MultByMode) AND
       (RXData.DomesticQTH <> '') THEN
           VisibleLog.ShowDomesticMultiplierStatus (RXData.DomMultQTH);

    Inc (NumberContactsThisMinute);
    NumberQSOPointsThisMinute := NumberQSOPointsThisMinute + RXData.QSOPoints;

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

    { This might not be a good thing to do.  FIrst off - Qsorder will not do anything
      useful as a result.  If I am sending this to yet anotehr computer - what if it
      is the one that sent the message.  Will it at some point send the QSO back to me? }

    IF (QSO_UDP_IP <> '') AND (QSO_UDP_Port <> 0) THEN
        BEGIN
        RXData.Date := GetFullDateString;
        SendQSOToUDPPort (RXData);
        END;
    END;



PROCEDURE N1MM_Object.LogContacts;

{ This is the meat of the N1MM interface.  Any UDP messages will be in the QTC Buffer
  wull be parsed into a TR Log RXData record and then the normal TR Log loggin routines
  will take that record - parse it into a Log String and push it into the editable
  log window. }

VAR N1MMString: STRING;
    RXData: ContestExchange;

    BEGIN
    WHILE QTC_Count > 0 DO
        BEGIN
        N1MMString := GetNextQTC;
        CreateRXDataFromUDPMessage (N1MMString, RXData);  { This is in LogUDP.pas }
        LogN1MMContact (RXData);
        END;
    END;



    BEGIN
    N1MM_UDP_Port := 0;    { Declared in logwind.pas }
    END.
