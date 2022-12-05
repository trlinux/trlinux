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

  N4OGWBandMap := N4OGWBandMapObject.Create;

  Then N4OGWBandMap1.Init (IPAddress: String; Port: LONGINT): BOOLEAN;  }

{$O+}
{$V-}

INTERFACE

USES SlowTree, Tree, Sockets, keycode;

TYPE
    N4OGWBandMapObject = CLASS
        IPAddress: STRING;
        PortNumber: LONGINT;
        Socket: LONGINT;

        PROCEDURE Init (IP: STRING; Port: LONGINT);

        PROCEDURE SendBandMapCall (Call: CallString; Frequency: LongInt;
                                   Dupe: BOOLEAN; Mult: BOOLEAN);

        PROCEDURE SetCenterFrequency (Frequency: INTEGER);
        END;

VAR
    N4OGWBandMapIP: STRING;     { Global variables used for config file parsing }
    N4OGWBandMapPort: LONGINT;  { Global variables used for config file parsing }

    N4OGWBandMap: N4OGWBandMapObject;

IMPLEMENTATION



PROCEDURE N4OGWBandMapObject.Init (IP: STRING; Port: LONGINT);

VAR SocketAddr: TINetSockAddr;
    ConnectResult: INTEGER;

    BEGIN
    IPAddress := IP;
    PortNumber := Port;

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

    END;



PROCEDURE N4OGWBandMapObject.SendBandMapCall (Call: CallString;
                                              Frequency: LongInt;
                                              Dupe: BOOLEAN;
                                              Mult: BOOLEAN);

VAR FrequencyString, SendString: STRING;

    BEGIN
    Str (Frequency, FrequencyString);

    IF Dupe THEN  { Blue call and signal }
        BEGIN
        SendString := 'a ' + Call + ',' + FrequencyString + ',' +
                      Chr (0) + Chr (0) + Chr (255) + Chr (0) + Chr (0) + Chr (1) + Chr (1);

        SendString [2] := Chr (Length (SendString) - 2);
        FpSend (Socket, @SendString [1], Length (SendString), 0);
        Exit;
        END;

    IF Mult THEN  { Red call and signal }
        BEGIN
        SendString := 'a ' + Call + ',' + FrequencyString + ',' +
                      Chr (255) + Chr (0) + Chr (0) + Chr (1) + Chr (0) + Chr (0) + Chr (1);

        SendString [2] := Chr (Length (SendString) - 2);
        FpSend (Socket, @SendString [1], Length (SendString), 0);
        Exit;
        END;

    { Regular entry - Green call and signal }

    SendString := 'a ' + Call + ',' + FrequencyString + ',' +
                  Chr (0) + Chr (255) + Chr (255) + Chr (0) + Chr (1) + Chr (1) + Chr (1);

    SendString [2] := Chr (Length (SendString) - 2);
    FpSend (Socket, @SendString [1], Length (SendString), 0);
    END;



PROCEDURE N4OGWBandMapObject.SetCenterFrequency (Frequency: INTEGER);

{ Sets the displayed center frequency of the N4OGW bandmap }

VAR FrequencyString, SendString: STRING;

    BEGIN
    Str (Frequency, FrequencyString);
    SendString := 'f' + Chr (Length (FrequencyString)) + FrequencyString;
    FpSend (Socket, @SendString [1], Length (SendString), 0);
    END;



PROCEDURE ListenToUDP;

{ this doesn't really work yet }

VAR SocketAddr: TINetSockAddr;
    ReceiveFlags, Socket: LONGINT;
    ReadChar: CHAR;
    ConnectResult: INTEGER;
    Broadcast: INTEGER;
    len: Tsocklen;

    BEGIN
    Socket := fpSocket (AF_INET, SOCK_DGRAM, 0);

    SocketAddr.sin_family := AF_INET;
    SocketAddr.sin_port := htons (45454);
    SocketAddr.sin_addr := StrToNetAddr ('192.168.1.255');

    Broadcast := 1;

    len := sizeof (SocketAddr);

    WriteLn ('going to listen');
    fprecvfrom (Socket, @ReadChar, 1, 0, @SocketAddr, @len);
    Write (ReadChar);
    END;



    BEGIN
    N4OGWBandMapIP := '';
    N4OGWBandMapPort := 0;
    END.
