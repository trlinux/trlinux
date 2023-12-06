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

UNIT N4OGW;

{ Here we interface to the so2sdr-bandmap program that N4OGW wrote.  It is essentially
  a "blind skimmer" that uses an SDR radio to get a waterfall display of the band with
  labels that are sent from bandmap entries in TR Log.  It also supports color coding
  of the calls and traces to indicate which ones are new stations or multipliers.

  To use this - you need to create the instance for the bandmap you want using

  N4OGW_BandMap_RadioOne := N4OGW_BandMapObject.Create;

  Then N4OGWBandMap1.Init (IPAddress: String; Port: LONGINT): BOOLEAN;

  }

{$O+}
{$V-}

INTERFACE

USES SlowTree, datetimec, Tree, KeyCode, Sockets, UnixType, BaseUnix, portname;

CONST MaxQTCs = 10;

TYPE
    SpotListEntry = RECORD
        Callsign: CallString;
        SecondsRemaining: INTEGER;
        END;

    SpotListType = RECORD
        SpotListArray: ARRAY [9..1000] OF SpotListEntry;
        NumberSpots: INTEGER;
        END;

    N4OGW_BandMap_Object = CLASS
        BandMapCenterFrequency: LONGINT;
        IPAddress: STRING;
        PortNumber: LONGINT;
        Socket: LONGINT;

        QTC_Count: INTEGER;    { Number of UDP messages cued up }
        QTC_Buffer: ARRAY [0..MaxQTCs - 1] OF STRING;

        SecondsMemory: WORD;  { Used to remember seconds value }

        TXMode: BOOLEAN;
        TXModeTimeout: INTEGER;     { Timer used to count down the TX hold time }

        SpotList: SpotListType;

        UDP_PortNumber: LONGINT;
        UDP_Socket: LONGINT;
        UDP_Message: STRING;

        PROCEDURE AddCallToSpotArray (Call: CallString);
        FUNCTION  Check_UDP_Port: BOOLEAN;  { Returns true if a message was found }
        PROCEDURE CheckSpotArray;
        PROCEDURE DeleteCallsign (Call: STRING);
        FUNCTION  GetNextQTC: STRING;
        PROCEDURE Heartbeat; { Call this often to give oxygen to UDP polling and TX timeout }
        PROCEDURE Init (IP: STRING; Port: LONGINT; UDP_Port: LONGINT);

        PROCEDURE SendBandMapCall (Call: CallString; Frequency: LongInt;
                                   Dupe: BOOLEAN; Mult: BOOLEAN);

        PROCEDURE SetCenterFrequency (Frequency: INTEGER);
        PROCEDURE SetTXMode;
        PROCEDURE SetRXMode;
        FUNCTION  UDP_Normal_Status_Message (Message: STRING): BOOLEAN;
        PROCEDURE WriteToDebugFile (Entry: STRING);
        END;

    N4OGW_Frequency_Control_Type = (N4OGW_FC_VFOA, N4OGW_FC_VFOB, N4OGW_FC_Auto);

VAR
    N4OGW_Frequency_Control: N4OGW_Frequency_Control_Type;

    N4OGW_RadioOne_BandMap_IP: STRING;             { Global variables used for config file parsing }
    N4OGW_RadioTwo_BandMap_IP: STRING;             { Global variables used for config file parsing }
    N4OGW_RadioOne_BandMap_Port: LONGINT;          { Global variables used for config file parsing }
    N4OGW_RadioTwo_BandMap_Port: LONGINT;          { Global variables used for config file parsing }
    N4OGW_RadioOne_BandMap_UDP_Port: LONGINT;      { Global variables used for config file parsing }
    N4OGW_RadioTwo_BandMap_UDP_Port: LONGINT;      { Global variables used for config file parsing }

    N4OGW_RadioOne_BandMap: N4OGW_BandMap_Object;
    N4OGW_RadioTwo_BandMap: N4OGW_BandMap_Object;

    N4OGW_UDP_Port_Initialized: BOOLEAN;           { Gets set to TRUE when the first instance opens the port }

FUNCTION GetExactTimeString: STRING;

IMPLEMENTATION

CONST DebugFileName = 'N4OGW_debug.txt';



FUNCTION  N4OGW_BandMap_Object.GetNextQTC: STRING;

{ Pulls the oldest QTC off the QTC buffer }

VAR Index: INTEGER;

    BEGIN
    IF QTC_Count = 0 THEN
        BEGIN
        GetNextQTC := '';
        Exit;
        END;

    GetNextQTC := QTC_Buffer [0];

    WriteToDebugFile ('QTC_Buffer = ' + QTC_Buffer [0]);

    IF QTC_Count > 1 THEN
        FOR Index := 0 TO QTC_Count - 2 DO
            QTC_Buffer [Index] := QTC_Buffer [Index + 1];

    Dec (QTC_Count);
    END;



FUNCTION GetExactTimeString: STRING;

VAR Temp1, Temp2, Temp3: String[5];
    Hours, Minutes, Seconds, Hundredths: Word;

    BEGIN
    GetTime (Hours, Minutes, Seconds, Hundredths);

    Str (Hours,  Temp1);
    Str (Minutes, Temp2);
    Str (Seconds, Temp3);

    IF Length (Temp1) < 2 THEN Temp1 := '0' + Temp1;
    IF Length (Temp2) < 2 THEN Temp2 := '0' + Temp2;
    IF Length (Temp3) < 2 THEN Temp3 := '0' + Temp3;

    GetExactTimeString := Temp1 + ':' + Temp2 + ':' + Temp3;
    END;


PROCEDURE N4OGW_BandMap_Object.WriteToDebugFile (Entry: STRING);

VAR FileWrite: TEXT;

    BEGIN
    IF StringHas (Entry, 'xyzzy') THEN
        BEGIN
        OpenFileForAppend (FileWrite, DebugFileName);
        WriteLn (FileWrite, GetDateString, ' ', GetExactTimeString, ' N4OGW says hi');
        Close (FileWrite);
        END
    ELSE
        BEGIN
        OpenFileForAppend (FileWrite, DebugFileName);
        WriteLn (FileWrite, GetDateString, ' ', GetExactTimeString, ' ', Entry);
        Close (FileWrite);
        END;
    END;



PROCEDURE N4OGW_BandMap_Object.DeleteCallsign (Call: STRING);

{ Removes the callsign label for the indicated call }

VAR SendString: STRING;

    BEGIN
    SendString := 'd'  + Chr (Length (Call)) + Call;
    FpSend (Socket, @SendString [1], Length (SendString), 0);
    WriteToDebugFile ('DeleteCallsign called with ' + Call);
    END;


PROCEDURE N4OGW_BandMap_Object.SetTXMode;

VAR SendString: Str20;

    BEGIN
    IF TXMode THEN Exit;   { Someone already set it }

    { We need to tell N4OGW we are transmitting now }

    SendString := 't' + Chr(0);
    FpSend (Socket, @SendString [1], Length (SendString), 0);
    TXMode := True;
    TXModeTimeout := 0;  { Stop counting down to turn off TX mode }

    WriteToDebugFile ('Sent t command (transmit mode');
    END;


PROCEDURE N4OGW_BandMap_Object.SetRXMode;

VAR SendString: STRING;

    BEGIN
//    IF NOT TXMode THEN
//        BEGIN
//        WritetoDebugFile ('Asked to SetRXMode but not in TXMode');
//        Exit;   { We are not in TX mode - nothing to do }
//        END;

//    IF TXModeTimeout > 0 THEN Exit;  { We are already doing a TX timeout }

    { We actually don't set the RX Mode just yet - we need to delay a couple
      of seconds to make sure the pipeline including RX latency is all
      processed before doing this.  So - we set a timer to create this delay }

    TXModeTimeout := 2;
    END;


PROCEDURE N4OGW_BandMap_Object.Init (IP: STRING; Port: LONGINT; UDP_Port: LONGINT);

VAR SocketAddr: TINetSockAddr;
    ConnectResult: INTEGER;

    BEGIN
    BandMapCenterFrequency := 0;
    IPAddress := IP;
    PortNumber := Port;
    QTC_Count := 0;
    SpotList.NumberSpots := 0;
    TXMode := False;
    TXModeTimeout := 0;
    UDP_PortNumber := UDP_Port;

    { Setup the TCP port }

    Socket := fpSocket (AF_INET, SOCK_STREAM, 0);  { TCP port }

    SocketAddr.sin_family := AF_INET;
    SocketAddr.sin_port := htons (PortNumber);
    SocketAddr.sin_addr := StrToNetAddr (IPAddress);

    ConnectResult := fpConnect (Socket, @SocketAddr, SizeOf (SocketAddr));

    IF ConnectResult <> 0 THEN
        BEGIN
        WriteLn ('Unable to connect to ' + IPAddress + ' port ', PortNumber);
        WaitForKeyPressed;
        Exit;
        END;

    WriteToDebugFile ('TR Log Init for ' + IPAddress);

    { Setup the UDP port.  Note that if we only need to set it up once since all
      commands from whatever bandmaps that are out there all come to one place }

    IF NOT N4OGW_UDP_Port_Initialized THEN
        BEGIN
        UDP_Socket := fpSocket (AF_INET, SOCK_DGRAM, IPPROTO_UDP);

        SocketAddr.sin_family := AF_INET;
        SocketAddr.sin_port := htons (UDP_PortNumber);
        SocketAddr.sin_addr.s_addr := INADDR_ANY;

        ConnectResult := fpBind (UDP_Socket, @SocketAddr, SizeOf (SocketAddr));

        IF ConnectResult <> 0 THEN
            BEGIN
            WriteLn ('Unable to connect to UDP port ', PortNumber);
            WaitForKeyPressed;
            Exit;
            END;

        N4OGW_UDP_Port_Initialized := True;  { Make sure we don't do this again }
        WriteToDebugFile ('TR Log UDP init for port');
        END;

    END;



PROCEDURE N4OGW_BandMap_Object.AddCallToSpotArray (Call: CallString);

VAR Address: INTEGER;

    BEGIN
    { See if the call is already in the list - and if so - update the time remaining }

    IF SpotList.NumberSpots > 0 THEN
        FOR Address := 0 TO SpotList.NumberSpots - 1 DO
            IF SpotList.SpotListArray [Address].Callsign = Call THEN
                BEGIN
                SpotList.SpotListArray [Address].SecondsRemaining := 3600;
                Exit;
                END;

    { Add call to the spot list and set to default time remaining }

    IF SpotList.NumberSpots < 1000 THEN
        BEGIN
        SpotList.SpotListArray [SpotList.NumberSpots].Callsign := Call;
        SpotList.SpotListArray [SpotList.NumberSpots].SecondsRemaining := 3600;
        Inc (SpotList.NumberSpots);
        END
    ELSE
        WriteToDebugFile ('SPOT ARRAY IS FULL!!  Trying to add ' + Call);
    END;



PROCEDURE N4OGW_BandMap_Object.CheckSpotArray;

{ Decrements minutes remaining for all entries.  If remaining time goes to
  zero for a call - it is deleted }

VAR Address: INTEGER;
    NewArrayAddress, NumberEntriesDeleted: INTEGER;

    BEGIN

    { This code has a problem with memory affecting other variables }

    IF SpotList.NumberSpots = 0 THEN Exit;  { Nothing to do }

    NewArrayAddress := 0;
    NumberEntriesDeleted := 0;

    { Decrement time and see if it went to zero or below }

    FOR Address := 0 TO SpotList.NumberSpots - 1 DO
        BEGIN
        Dec (SpotList.SpotListArray [Address].SecondsRemaining);

        IF SpotList.SpotListArray [Address].SecondsRemaining <= 0 THEN
            BEGIN
            Inc (NumberEntriesDeleted);
            DeleteCallSign (SpotList.SpotListArray [Address].Callsign);
            WriteToDebugFile ('TIMEOUT!!  We decided to delete ' + SpotList.SpotListArray [Address].Callsign);
            END

        ELSE
           { This entry is okay - save it on the "new array" }

            BEGIN
            IF NumberEntriesDeleted > 0 THEN
                SpotList.SpotListArray [NewArrayAddress] := SpotList.SpotListArray [Address];

            Inc (NewArrayAddress);
            END;

        END;

    SpotList.NumberSpots := NewArrayAddress;
    END;



PROCEDURE N4OGW_BandMap_Object.SendBandMapCall (Call: CallString;
                                                Frequency: LongInt;
                                                Dupe: BOOLEAN;
                                                Mult: BOOLEAN);


VAR FrequencyString, SendString: STRING;

    BEGIN
    AddCallToSpotArray (Call);

    Str (Frequency, FrequencyString);

    IF Dupe THEN  { Blue call and signal }
        BEGIN
        SendString := 'a ' + Call + ',' + FrequencyString + ',' +
                      Chr (0) + Chr (0) + Chr (0) + Chr (1) + Chr (0) + Chr (0) + Chr (1);

        SendString [2] := Chr (Length (SendString) - 2);
        FpSend (Socket, @SendString [1], Length (SendString), 0);
        WriteToDebugFile ('Sent a command with ' + Call + ' on ' + FrequencyString + ' as a dupe');
        Exit;
        END;

    IF Mult THEN  { Red call and signal }
        BEGIN
        SendString := 'a ' + Call + ',' + FrequencyString + ',' +
                      Chr (000) + Chr (0) + Chr (0) + Chr (0) + Chr (1) + Chr (0) + Chr (1);

        SendString [2] := Chr (Length (SendString) - 2);
        FpSend (Socket, @SendString [1], Length (SendString), 0);
        WriteToDebugFile ('Sent a command with ' + Call + ' on ' + FrequencyString + ' as a mult');
        Exit;
        END;

    { Regular entry - Green call and signal }

    SendString := 'a ' + Call + ',' + FrequencyString + ',' +
                  Chr (0) + Chr (000) + Chr (000) + Chr (1) + Chr (0) + Chr (1) + Chr (1);

    SendString [2] := Chr (Length (SendString) - 2);
    FpSend (Socket, @SendString [1], Length (SendString), 0);

    WriteToDebugFile ('Sent ' + Call + ' on ' + FrequencyString + ' as a regular entry');
    END;



PROCEDURE N4OGW_BandMap_Object.SetCenterFrequency (Frequency: INTEGER);

{ Sets the displayed center frequency of the N4OGW bandmap }

VAR FrequencyString, SendString: STRING;

    BEGIN
    IF Frequency <> BandMapCenterFrequency THEN
        BEGIN
        Str (Frequency, FrequencyString);
        SendString := 'f' + Chr (Length (FrequencyString)) + FrequencyString;
        FpSend (Socket, @SendString [1], Length (SendString), 0);
        BandMapCenterFrequency := Frequency;

        WriteToDebugFile ('Set new center frequency to ' + FrequencyString);
        END;
    END;



FUNCTION N4OGW_BandMap_Object.Check_UDP_Port: BOOLEAN;  { Returns true if a message was found }

VAR BytesRead: INTEGER;
    FDS: Tfdset;

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

    IF StringHas (UDP_Message, 'operation') OR StringHas (UDP_Message, 'freq') THEN
        WriteToDebugFile ('Received UDP Message = ' + UDP_Message);

    Check_UDP_Port := True;
    END;



FUNCTION N4OGW_BandMap_Object.UDP_Normal_Status_Message (Message: STRING): BOOLEAN;

{ Looks at the UDP message to see if it looks like the once a second keep
  alive message from N4OGW }

    BEGIN
    UDP_Normal_Status_Message := NOT StringHas (Message, 'freq');
    END;



PROCEDURE N4OGW_BandMap_Object.Heartbeat;

{ This should be called as often as possible.  It will deal with any timeout
  with the TX status and also check the UDP port.  If there is a command from
  the UDP port that needs attention, it will be pushed into the QTC_Buffer }

VAR Hours, Minutes, Seconds, Sec100: WORD;
    SendString: STRING;

    BEGIN
    { Check the UDP port to see if there are any messages waiting }

    WHILE Check_UDP_Port AND (QTC_Count < MaxQTCs) DO
        IF NOT UDP_Normal_Status_Message (UDP_Message) THEN
            BEGIN
            QTC_Buffer [QTC_Count] := UDP_Message;
            Inc (QTC_Count);

            IF QTC_Count > 5 THEN
                WriteToDebugFile ('QTC count is more than 5!!');
            END;


    { Check system clock so we can execute some stuff once a second }

    GetTime (Hours, Minutes, Seconds, Sec100);

    IF Seconds = SecondsMemory THEN Exit;
    SecondsMemory := Seconds;

    { We are now executing this code only once a second }

{   CheckSpotArray;}

    IF TXModeTimeout > 0 THEN
        BEGIN
            SendString := 'r' + Chr(0);
            FpSend (Socket, @SendString [1], Length (SendString), 0);
            TXMode := False;
            WriteToDebugFile ('Sent r command (receive mode) in heartbeat.');
        END;
    END;



    BEGIN
    N4OGW_RadioOne_BandMap_IP := '';
    N4OGW_RadioOne_BandMap_Port := 0;
    N4OGW_RadioOne_BandMap_UDP_Port := 45454;
    N4OGW_RadioTwo_BandMap_IP := '';
    N4OGW_RadioTwo_BandMap_Port := 0;
    N4OGW_RadioTwo_BandMap_UDP_Port := 45454;
    N4OGW_UDP_Port_Initialized := False;
    END.
