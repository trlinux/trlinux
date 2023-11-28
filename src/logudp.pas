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

UNIT LogUDP;

{ This unit is focused on generating and sending a UDP packet when a QSO
  is logged.  It will accept the RXData record, parse it into the UDP
  packet and then send it on its way.  The two globals QSO_UDP_IP and
  QSO_UDP_Port need to be setup in order for it to work. Any initilization
  is done when the first attempt to send data is made. }

{$O+}
{$V-}

{ Here is the description and notes for the protocol from the N1MM
  website:

  First, the QSO itself:

<?xml version="1.0" encoding="utf-8"?>
<contactinfo>
    <app>N1MM</app>
    <contestname>CWOPS</contestname>
    <contestnr>73</contestnr>
    <timestamp>2020-01-17 16:43:38</timestamp>
    <mycall>W2XYZ</mycall>
    <band>3.5</band>
    <rxfreq>352519</rxfreq>
    <txfreq>352519</txfreq>
    <operator></operator>
    <mode>CW</mode>
    <call>WlAW</call>
    <countryprefix>K</countryprefix>
    <wpxprefix>Wl</wpxprefix>
    <stationprefix>W2XYZ</stationprefix>
    <continent>NA</continent>
    <snt>599</snt>
    <sntnr>5</sntnr>
    <rcv>599</rcv>
    <rcvnr>0</rcvnr>
    <gridsquare></gridsquare>
    <exchangel></exchangel>
    <section></section>
    <comment></comment>
    <qth></qth>
    <name></name>
    <power></power>
    <misctext></misctext>
    <zone>0</zone>
    <prec></prec>
    <ck>0</ck>
    <ismultiplierl>l</ismultiplierl>
    <ismultiplier2>0</ismultiplier2>
    <ismultiplier3>0</ismultiplier3>
    <points>l</points>
    <radionr>l</radionr>
    <run1run2>1<run1run2>
    <RoverLocation></RoverLocation>
    <RadioInterfaced>l</RadioInterfaced>
    <NetworkedCompNr>0</NetworkedCompNr>
    <IsOriginal>False</IsOriginal>
    <NetBiosName></NetBiosName>
    <IsRunQSO>0</IsRunQSO>
    <StationName>CONTEST-PC</StationName>
    <ID>f9ffac4fcd3e479ca86e137df1338531</ID>
    <IsClaimedQso>True</IsClaimedQso>
</contactinfo>

NOTES

IsOriginal indicates that this is the station on which this contact
was initially logged to differentiate from another station that may
be forwarding the contact record.

StationName is the netbios name of the station that sent this packet,
not necessarily the name of the station that logged this contact.

Power, Name and QTH all refer to information about the stataion being
worked.  For the ARRL DX Contest, power is the power of the station
being worked.

Run1 and Run2 refer to the run radio number in a multi-2 arrangement.

Band is composed of 2 or 3 characters that may include localized delimiters.
For example, 80 meters may be "3.5" or "3,5", 160 meters at "1.8" or "1,8".

ID is a unique

ID is a 32 byte unique GUID identifier for each contact in the log. Note that
it is sent as 2 hex characters per byte.

IsClaimedQso will default to TRUE for all contacts and set to FALSE when a
contact is declared to be an X-QSO.

ADIF fields in ContactInfo – It may be tempting to assume that there is a
one-to-one correspondence between fields in the contactinfo messages and
similar fields in the ADIF specification, but this is not always the case.

The best example is the Section field in the contactinfo message; this means
whatever the rules for the particular contest define it to mean. In a few
contests, the Section field is the same as the ADIF ARRL_SECT; in many more
contests and for most geographical locations, it may be the same as STATE
in ADIF. However, in some contests for some geographical locations (e.g.
Alaska, Hawaii, the District of Columbia, the Canadian Atlantic provinces,
Canadian northern territories) the section identifier specified in the contest
results might not match either ADIF field. Another example is the Zone field,
which often means the same as CQZ in ADIF, but in some contests it means the
same as ITUZ. Yet another example is the frequency field, which is not in the
same format as in ADIF.  In the contact info and radio info message, the
frequency is exported in units of 10 Hz.

Contact Replace
The Contact Replace packet contains the same fields as Contact Info above,
but with <contactreplace > XML tags.

<?xml version="1.0" encoding="utf-8"?>
<contactreplace>
    <app>N1MM</app>
    <contestname>CWOPS</contestname>
    <contestnr>73</contestnr>
    <timestamp>2020-01-17 16 :43:38</timestamp>
    <mycall>W2XYZ</mycall>
    .
    .
    .
</contactreplace>

IMPORTANT NOTE TO DEVELOPERS:
When a user edits an existing contact record, the program will send a pair of
UDP packets: A <contactdelete> packet, followed by a <contactreplace> packet.

The <contactdelete> packet will contain the existing record's callsign and
timestamp, in the event that the <contactreplace> packet includes a new
timestamp or callsign for this record.

Contact Delete

<?xml version="1.0" encoding="utf-8"?>
<contactdelete>
    <app>N1MM</app>
    <timestamp>2020-01-17 16:43:38</timestamp>
    <call>WlAW</call>
    <contestnr>73</contestnr>
    <StationName>CONTEST-PC</StationName>
    <ID>a1b2c3d4e5f6g7h</ID>
</contactdelete>

   End of description of format from the N1MM website }

INTERFACE

USES Dos, Tree, ZoneCont, Country9, datetimec, KeyCode, Sockets,
     Radio, LogWind, LogDupe, UnixType, BaseUnix, portname;

CONST BufferSize = 4096;

VAR
    QSO_UDP_IP: STRING;
    QSO_UDP_Port: LONGINT;

PROCEDURE SendQSOToUDPPort (RXData: ContestExchange);

IMPLEMENTATION

TYPE

    UDPRecordType = RECORD
        app: Str40;
        contestname: Str80;
        contestnr: Str20;
        timestamp: Str40;        { 2020-01-17 16:43:38 }
        mycall: Str20;
        band: Str20;
        rxfreq: Str20;           { 352519 - so 10's of hertz }
        txfreq: Str20;
        operator_call: Str40;
        mode: Str20;
        call: Str20;             { Station worked }
        countryprefix: Str20;
        wpxprefix: Str20;
        stationprefix: Str20;
        continent: Str20;
        snt: Str40;
        sntnr: Str20;
        rcv: Str40;
        rcvnr: Str20;
        gridsquare: Str20;
        exchangel: Str20;
        section: Str20;
        comment: STRING;
        qth: STRING;
        name: Str40;
        power: Str20;
        misctext: STRING;
        zone: Str20;
        prec: Str20;
        ck: Str20;
        ismultiplierl: Str20;
        ismultiplier2: Str20;
        ismultiplier3: Str20;
        points: Str20;
        radionr: Str20;
        run1run2: Str20;
        RoverLocation: Str20;
        RadioInterfaced: Str20;
        NetworkedCompNr: Str20;
        IsOriginal: Str20;
        NetBiosName: Str20;
        IsRunQSO: Str20;
        StationName: Str40;
        ID: Str40;
        IsClaimedQso: Str20;
        END;

VAR
    NumberBytesInUDPBuffer: INTEGER;
    UDPBuffer: ARRAY [0..BufferSize -1] OF CHAR;

    UDP_Record: UDPRecordType;
    QSO_UDP_Socket: LONGINT;
    QSOUDPOutputPortOpen: BOOLEAN;



PROCEDURE ClearUDPRecord;

    BEGIN
    WITH UDP_Record DO
        BEGIN
        app := '';
        contestname := '';
        contestnr := '';;
        timestamp := '';
        mycall := '';
        band := '';
        rxfreq := '';
        txfreq := '';
        operator_call := '';
        mode := '';
        call := '';
        countryprefix := '';
        wpxprefix := '';
        stationprefix := '';
        continent := '';
        snt := '';
        sntnr := '';
        rcv := '';
        rcvnr := '';
        gridsquare := '';
        exchangel := '';
        section := '';
        comment := '';
        qth :=  '';
        name :=  '';
        power := '';
        misctext := '';
        zone := '';
        prec := '';
        ck := '';
        ismultiplierl := '';
        ismultiplier2 := '';
        ismultiplier3 := '';
        points := '';
        radionr := '';
        run1run2 := '';
        RoverLocation := '';
        RadioInterfaced := '';
        NetworkedCompNr := '';
        IsOriginal := '';
        NetBiosName := '';
        IsRunQSO := '';
        StationName := '';
        ID := '';
        IsClaimedQso := '';
        END;
    END;



PROCEDURE MergeRXExchangeToUDPRecord (RXData: ContestExchange);

{ Takes the data found in the RXData record and puts it into the UDPRecord }

VAR DayString, MonthString, YearString, TimeString, SecondsString, TempString: Str20;

    BEGIN
    WITH RXData DO
        BEGIN
        { First, we will take care of the stuff that needs to be there for any QSO }

        UDP_Record.MyCall := MyCall;          { A global that comes from LOGWIND.PAS }
        UDP_Record.Call := Callsign;          { Callsign of station being worked }

        UDP_Record.Mode := ModeString [Mode]; { Until someone tells me different }
        GetRidOfPostcedingSpaces (UDP_Record.Mode);

        UDP_Record.CountryPrefix := DXQTH;    { DXCC Prefix }
        UDP_Record.WPXPrefix := Prefix;       { WPX Prefix }

        IF Frequency <> 0 THEN
            BEGIN
            Str (Frequency, TempString);
            Delete (TempString, Length (TempString), 1);  { Make 10's of hertz }
            UDP_Record.rxfreq := TempString;
            UDP_Record.txfreq := TempString;  { was rxfreq before 11-23-2023 }
            END;

        {  These came from N2IC who gave me a copy of the source code in N1MM.
           I don't yet have these bands - 4700.0, 76000.0, 122250.0, 134000.0 and 241000.0 }

        CASE Band OF
            Band160: UDP_Record.band := '1.8';
            Band80:  UDP_Record.band := '3.5';
            Band40:  UDP_Record.band := '7';
            Band30:  UDP_Record.band := '10';
            Band20:  UDP_Record.band := '14';
            Band17:  UDP_Record.band := '18';
            Band15:  UDP_Record.band := '21';
            Band12:  UDP_Record.band := '24';
            Band10:  UDP_Record.band := '28';
            Band6:   UDP_Record.band := '50';
            Band2:   UDP_Record.band := '144';
            Band222: UDP_Record.band := '222';
            Band432: UDP_Record.band := '420';
            Band902: UDP_Record.band := '902';
            Band1296: UDP_Record.band := '1240';
            Band2304: UDP_Record.band := '2300';
            Band3456: UDP_Record.band := '3300';
            Band5760: UDP_Record.band := '5650';
            Band10G:  UDP_Record.band := '10000';
            Band24G:  UDP_Record.band := '24000';

//          Band47G:  UDP_Record.band := '47000';
//          Band76G:  UDP_Record.band := '76000';
//          Band122G:  UDP_Record.band := '122250';
//          Band241G:  UDP_Record.band := '241000';

            BandLight:UDP_Record.band := '4E+08';
            ELSE UDP_Record.band := '???';
            END;

       { The date/time go into the single entry timestamp.  The ContestExchange has the
         format dd-mmm-yy.  This must be converted to 2020-01-17.  The time is in an
         integer number of minutes in integer format and needs to be comverted to a
         string, but already has hours * 100.  Seconds gets added on using TimeSeconds }

       { The Date field looks like 06-Jan-2023 }

        YearString := PrecedingString (Date, '-');
        MonthString := UpperCase (BracketedString (Date, '-', '-'));
        DayString := PostcedingString (Date, MonthString + '-');

        IF Length (DayString) < 2 THEN DayString := '0' + DayString;

        { And now for the time - which is an integer with the hours counting
          as 100's }

        Str (Time, TimeString);
        WHILE Length (TimeString) < 4 DO TimeString := '0' + TimeString;
        Insert (':', TimeString, 3);

        Str (TimeSeconds, SecondsString);
        IF Length (SecondsString) < 2 THEN SecondsString := '0' + SecondsString;

        TimeString := TimeString + ':' + SecondsString;

        UDP_Record.timeStamp := YearString + '-' + MonthString + '-' + DayString + ' ' + TimeString;

        { Now for the fields that may - or may not - have any data in them }

        IF Age <> ''         THEN UDP_Record.rcv := Age;
        IF Chapter <> ''     THEN UDP_Record.rcv := Chapter;
        IF Check <> ''       THEN UDP_Record.ck  := Check;
        IF Classs <> ''      THEN UDP_Record.rcv := Classs;
        IF DomesticQTH <> '' THEN UDP_Record.qth := DomesticQTH;
        IF Kids <> ''        THEN UDP_Record.rcv  := Kids;
        IF Name <> ''        THEN UDP_Record.Name := Name;

        IF NumberReceived <> -1 THEN
            BEGIN
            Str (NumberReceived, TempString);
            UDP_Record.rcvnr := TempString;
            END;

        IF NumberSent <> -1 THEN
            BEGIN
            Str (NumberSent, TempString);
            UDP_Record.sntnr := TempString;
            END;

        IF PostalCode <> ''      THEN UDP_Record.rcv := PostalCode;
        IF Power <> ''           THEN UDP_Record.power := Power;
        IF Precedence <> Chr (0) THEN UDP_Record.prec := Precedence;

        IF Prefecture <> -1 THEN
            BEGIN
            Str (Prefecture, TempString);
            UDP_Record.rcv := TempString;
            END;

        IF QSOPoints <> -1 THEN
            BEGIN
            Str (QSOPoints, TempString);
            UDP_Record.points := TempString;
            END;

        CASE Radio OF
            RadioOne: UDP_Record.RadioNr := '1';
            RadioTwo: UDP_Record.RadioNr := '2';
            END;  { of CASE }

        IF RandomCharsSent <> '' THEN UDP_Record.snt := RandomCharsSent;
        IF RandomCharsReceived <> '' THEN UDP_Record.rcv := RandomCharsReceived;

        IF TenTenNum <> -1 THEN
            BEGIN
            Str (TenTenNum, TempString);
            UDP_Record.rcvnr := TempString;
            END;

        IF Zone <> '' THEN UDP_Record.zone := Zone;

{       RXData Fields that are ignored

        DomesticMult:    BOOLEAN;
        DomMultQTH:      DomesticMultiplierString;
        DXMult:          BOOLEAN;
        DXQTH:           DXMultiplierString;
        InhibitMults:    BOOLEAN;
        PrefixMult:      BOOLEAN;
        QTH:             QTHRecord;
        QTHString:       STRING [30];
        SearchAndPounce: BOOLEAN;
        ZoneMult:        BOOLEAN;   }

        { Do these last - I am not sure why }

        IF RSTReceived <> '' THEN
            IF UDP_Record.rcv = '' THEN
                UDP_Record.rcv := RSTReceived
            ELSE
                UDP_Record.rcv := RSTReceived + ' ' + UDP_Record.rcv;

        IF RSTSent<> '' THEN
            IF UDP_Record.snt = '' THEN
                UDP_Record.snt := RSTSent
            ELSE
                UDP_Record.snt := RSTSent+ ' ' + UDP_Record.snt;
        END;
    END;



PROCEDURE AddStringToUDPBuffer (AddString: STRING);

{ Adds string to UDP Buffer }

VAR Index: INTEGER;

    BEGIN
    IF Length (AddString) > 0 THEN
        BEGIN
        FOR Index := 1 TO Length (AddString) DO
            BEGIN
            UDPBuffer [NumberBytesInUDPBuffer] := AddString [Index];
            Inc (NumberBytesInUDPBuffer);

            IF NumberBytesInUDPBuffer >= BufferSize THEN
                BEGIN
                WriteLn ('UDPBuffer ERROR');
                WaitForKeyPressed;
                Halt;
                END;
            END;

        UDPBuffer [NumberBytesInUDPBuffer] := CarriageReturn;
        Inc (NumberBytesInUDPBuffer);
        UDPBuffer [NumberBytesInUDPBuffer] := LineFeed;
        Inc (NumberBytesInUDPBuffer);
        END;

    END;



PROCEDURE BuildUDPPAcket;

{ Takes the data found in the global UDPRecord and creates a buffer with the data
  ready to send out to the UDP port.  The buffer is a global array of characters }

    BEGIN
    NumberBytesInUDPBuffer := 0;    { Start with and empty buffer }

    AddStringToUDPBuffer ('<?xml version=''1.0'' encoding=''utf-8''?>');
    AddStringToUDPBuffer ('<contactinfo>');
    AddStringToUDPBuffer (TabKey + '<app>TRLogLinux</app>');
    AddStringtoUDPBuffer (TabKey + '<contestname>' + ContestName + '</contestname>');

    WITH Udp_Record DO
        BEGIN
        AddStringToUDPBuffer (TabKey + '<timestamp>' + TimeStamp + '</timestamp>');
        AddStringtoUDPBuffer (TabKey + '<mycall>' + MyCall + '</mycall>');
        AddStringToUDPBuffer (TabKey + '<band>' + Band + '</band>');
        AddStringToUDPBuffer (TabKey + '<rxfreq>' + RXFreq + '</rxfreq>');
        AddStringtoUDPBuffer (TabKey + '<txfreq>' + TXFreq + '</txfreq>');

        IF Operator_Call <> '' THEN AddStringToUDPBuffer (TabKey + '<operator>' + Operator_Call + '</operator>');

        AddStringtoUDPBuffer (TabKey + '<mode>' + Mode + '</mode>');
        AddStringtoUDPBuffer (TabKey + '<call>' + Call + '</call>');
        AddStringtoUDPBuffer (TabKey + '<countryprefix>' + CountryPrefix + '</countryprefix>');
        AddStringtoUDPBuffer (TabKey + '<wpxprefix>' + WPXPrefix + '</wpxprefix>');
        AddStringtoUDPBuffer (TabKey + '<stationprefix>' + StationPrefix + '</stationprefix>');
        AddStringtoUDPBuffer (TabKey + '<continent>' + Continent + '</continent>');

        IF snt <> '' THEN AddStringtoUDPBuffer (TabKey + '<snt>' + snt + '</snt>');
        IF sntnr <> '' THEN AddStringtoUDPBuffer (TabKey + '<sntnr>' + SntNr + '</sntnr>');
        IF rcv <> '' THEN AddStringtoUDPBuffer (TabKey + '<rcv>' + Rcv + '</rcv>');
        IF rcvnr <> '' THEN AddStringtoUDPBuffer (TabKey + '<rcvnr>' + RcvNr + '</rcvnr>');
        IF GridSquare <> '' THEN AddStringtoUDPBuffer (TabKey + '<gridsquare>' + GridSquare + '</gridsquare>');
        IF Exchangel <> '' THEN AddStringtoUDPBuffer (TabKey + '<exchangel>' + exchangel + '</exchangel>');
        IF Section <> '' THEN AddStringtoUDPBuffer (TabKey + '<section>' + Section + '</section>');
        IF Comment <> '' THEN AddStringtoUDPBuffer (TabKey + '<comment>' + Comment + '</comment>');
        IF QTH <> '' THEN AddStringtoUDPBuffer (TabKey + '<qth>' + QTH + '</qth>');
        IF Name <> '' THEN AddStringtoUDPBuffer (TabKey + '<name>' + Name + '</name>');
        IF Power <> '' THEN AddStringtoUDPBuffer (TabKey + '<power>' + Power + '</power>');
        IF MiscText <> '' THEN AddStringtoUDPBuffer (TabKey + '<misctext>' + MiscText + '</misctext>');
        IF Zone <> '' THEN AddStringtoUDPBuffer (TabKey + '<zone>' + Zone + '</zone>');
        IF Prec <> '' THEN AddStringtoUDPBuffer (TabKey + '<prec>' + Prec + '</prec>');
        IF Ck <> '' THEN AddStringtoUDPBuffer (TabKey + '<ck>' + Ck + '</ck>');
        IF IsMultiplierl <> '' THEN AddStringtoUDPBuffer (TabKey + '<ismultiplierl>' + IsMultiplierl + '</ismultiplierl>');
        IF IsMultiplier2 <> '' THEN AddStringtoUDPBuffer (TabKey + '<ismultiplier2>' + IsMultiplier2 + '</ismultiplier2>');
        IF IsMultiplier3 <> '' THEN AddStringtoUDPBuffer (TabKey + '<ismultiplier3>' + IsMultiplier3 + '</ismultiplier3>');
        IF Points <> '' THEN AddStringtoUDPBuffer (TabKey + '<points>' + Points + '</points>');
        IF Radionr <> '' THEN AddStringtoUDPBuffer (TabKey + '<radionr>' + RadioNr + '</radionr>');
        IF Run1Run2 <> '' THEN AddStringtoUDPBuffer (TabKey + '<run1run2>' + Run1Run2 + '<run1run2>');
        IF RoverLocation <> '' THEN AddStringtoUDPBuffer (TabKey + '<RoverLocation>' + RoverLocation + '</RoverLocation>');
        IF RadioInterfaced <> '' THEN AddStringToUDPBuffer (TabKey + '<RadioInterfaced>' + RadioInterfaced + '</RadioInterfaced>');

        { For now - we always send zero for this }

        AddStringtoUDPBuffer (TabKey + '<NetworkedCompNr>' + '0' + '</NetworkedCompNr>');

        IF IsOriginal <> '' THEN AddStringtoUDPBuffer (TabKey + '<IsOriginal>' + IsOriginal + '</IsOriginal>');
        IF NetBiosName <> '' THEN AddStringtoUDPBuffer (TabKey + '<NetBiosName>' + NetBIOSName + '</NetBiosName>');
        IF IsRunQSO <> '' THEN AddStringtoUDPBuffer (TabKey + '<IsRunQSO>' + IsRunQSO + '</IsRunQSO>');
        IF StationName <> '' THEN AddStringtoUDPBuffer (TabKey + '<StationName>' + StationName + '</StationName>');
        IF ID <> '' THEN AddStringtoUDPBuffer (TabKey + '<ID>' + ID + '</ID>');
        IF IsClaimedQSO <> '' THEN AddStringtoUDPBuffer (TabKey + '<IsClaimedQso>' + IsCLaimedQSO + '</IsClaimedQso> ');
        END;

    AddStringToUDPBuffer ('</contactinfo>');
    END;



PROCEDURE SendQSOToUDPPort (RXData: ContestExchange);

    BEGIN
    IF NOT QSOUDPOutputPortOpen THEN
        QSOUDPOutputPortOpen := OpenUDPPortForOutput (QSO_UDP_IP, QSO_UDP_Port, QSO_UDP_Socket);

    IF QSOUDPOutputPortOpen THEN
        BEGIN
        ClearUDPRecord;
        MergeRXExchangeToUDPRecord (RXData);
        BuildUDPPacket;
        FPSend (QSO_UDP_Socket, @UDPBuffer, NumberBytesInUDPBuffer, 0);
        END
    ELSE
        BEGIN
        Write ('UDP PORT ERROR');
        Halt;
        END;
    END;



    BEGIN
    QSO_UDP_IP := '';
    QSO_UDP_Port := 0;
    QSOUDPOutputPortOpen := False;
    END.
