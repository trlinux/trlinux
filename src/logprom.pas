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

UNIT LogProm;

{$O+}

INTERFACE

Uses Country9, LogGrid, ZoneCont, LogWind, LogMenu, trCrt, FContest, SlowTree, Tree;

PROCEDURE PullLogCfgInformationOutOfThisPerson;


IMPLEMENTATION
uses keycode,portname;

VAR FileWrite: TEXT;

procedure ChooseSerialPort(initialString :string; finalstring: string);
var buf: PChararray;
   nch,nd,i,nf,dnum,j: integer;
   portkey: char;
   tempstring: str80;
   c: char;
   tp: pchar;
begin
   nch := 80;
   nd := 10;
   setlength(buf,nd);
   for i := 0 to nd-1 do getmem(buf[i],nch);
   nf := findserial(buf,nch,nd);
   if (nf > nd) then
   begin
      nf := nd;
   end;
   if nf > 0 then
   begin
      writeln;
      writeln('Serial ports found');
      writeln;
      for i := 0 to nf-1 do
      begin
         writeln('[',i,'] ',buf[i]);
      end;
      writeln;
      writeln('Choose a number in the brackets above, before the device name you want');
      repeat
      begin
         repeat
            PortKey := UpCase (GetKey ('Which serial port do you want to use?:'));
         until (PortKey = '0') OR (PortKey = '1') OR (PortKey = '2')
            OR (PortKey = '3') OR (PortKey = '4') OR (PortKey = '5')
            OR (PortKey = '6') OR (PortKey = '7') OR (PortKey = '8')
            OR (PortKey = '9');
         writeln;
         case PortKey of
            '0': dnum := 0;
            '1': dnum := 1;
            '2': dnum := 2;
            '3': dnum := 3;
            '4': dnum := 4;
            '5': dnum := 5;
            '6': dnum := 6;
            '7': dnum := 7;
            '8': dnum := 8;
            '9': dnum := 9;
         end;
      end;
      until dnum < nf;
      tp := buf[dnum];
      for j := 0 to nch do
      begin
         c := Char(tp[j]);
         if ((c = Chr(0)) or (c = ' ')) then break;
         tempstring[j+1] := c;
         tempstring[0] := Char(j+1);
      end;
      writeln(FileWrite,initialstring,tempstring,finalstring);
      for i := 0 to nd-1 do freemem(buf[i],nch);
   end
   else
      writeln('No Serial ports found');
end;

procedure ChooseParallelPort(initialString :string);
var buf: PChararray;
   nch,nd,i,nf,dnum,j: integer;
   portkey: char;
   tempstring: str80;
   c,paddlekey: char;
   tp: pchar;
begin
   nch := 80;
   nd := 10;
   setlength(buf,nd);
   for i := 0 to nd-1 do getmem(buf[i],nch);
   nf := findparallel(buf,nch,nd);
   if (nf > nd) then
   begin
      nf := nd;
   end;
   if nf > 0 then
   begin
      writeln;
      writeln('Parallel ports found');
      writeln;
      for i := 0 to nf-1 do
      begin
         writeln('[',i,'] ',buf[i]);
      end;
      writeln;
      writeln('Choose a number in the brackets above, before the device name you want');
      repeat
      begin
         repeat
            PortKey := UpCase (GetKey ('Which parallel port do you want to use?:'));
         until (PortKey = '0') OR (PortKey = '1') OR (PortKey = '2')
            OR (PortKey = '3') OR (PortKey = '4') OR (PortKey = '5')
            OR (PortKey = '6') OR (PortKey = '7') OR (PortKey = '8')
            OR (PortKey = '9');
         writeln;
         case PortKey of
            '0': dnum := 0;
            '1': dnum := 1;
            '2': dnum := 2;
            '3': dnum := 3;
            '4': dnum := 4;
            '5': dnum := 5;
            '6': dnum := 6;
            '7': dnum := 7;
            '8': dnum := 8;
            '9': dnum := 9;
         end;
      end;
      until dnum < nf;
      tp := buf[dnum];
      for j := 0 to nch do
      begin
         c := Char(tp[j]);
         if ((c = Chr(0)) or (c = ' ')) then break;
         tempstring[j+1] := c;
         tempstring[0] := Char(j+1);
      end;
      writeln(FileWrite,initialstring,tempstring);
        REPEAT
            PaddleKey := UpCase (GetKey
                ('Do you want to enable the paddle input for this parallel port? (Y/N) : '));
            IF PaddleKey = EscapeKey THEN Exit;
        UNTIL (PaddleKey = 'Y') OR (PaddleKey = 'N');
        WriteLn;
      writeln(FileWrite,'PADDLE PORT = PARALLEL ',tempstring);
      for i := 0 to nd-1 do freemem(buf[i],nch);
   end
   else
      writeln('No parallel ports found');
end;

PROCEDURE SetUpCWStuff;

VAR TypeKey, InvKey, CWKey: CHAR;
   Winkey,Yccc: boolean;

    BEGIN
    Winkey := false;
    Yccc := false;
    ClearScreenAndTitle ('SET UP CW OUTPUT PORTS');

    WriteLn ('If you are going to send CW from the program, you will need to tell the ');
    WriteLn ('program which port to use for each radio.  The ports can be serial,');
    WriteLn ('parallel , a Winkeyer port, or a YCCC SO2R box.  If you are using');
    WriteLn ('a serial port, you can optionally invert the signal which makes ');
    WriteLn ('it easier to interface to a negative voltage radio.  If you');
    WriteLn ('select a parallel port, you can also specify the port to have');
    WriteLn (' a paddle connected to it.');

    WriteLn;

    REPEAT
        CWKey := UpCase (GetKey ('Do you want to set up a port to send CW with? (Y/N) : '));
        IF CWKey = EscapeKey THEN Exit;
    UNTIL (CWKey = 'Y') OR (CWKey = 'N');
    WriteLn;

    IF CWKey = 'N' THEN Exit;

    REPEAT
        TypeKey := UpCase (GetKey ('Serial, parallel Winkeyer or YCCC box CW port for radio one? (S/P/W/Y) : '));
        If TypeKey = EscapeKey THEN Exit;
    UNTIL (TypeKey = 'S') OR (TypeKey = 'P') OR (TypeKey = 'W') OR (TypeKey = 'Y');
    WriteLn;

    Case TypeKey of
    'S': BEGIN
        TextColor (Cyan);

        WriteLn;
        WriteLn ('If you are keying a negative voltage radio with a serial port, you can have');
        WriteLn ('the signal inverted so you can simply use a PNP transistor to key the radio.');
    REPEAT
        InvKey := UpCase (GetKey ('Normal or inverted keying for radio one? (N/I) : '));
        If InvKey = EscapeKey THEN Exit;
    UNTIL (InvKey = 'N') OR (TypeKey = 'I');

    if InvKey = 'N' then
        ChooseSerialPort('KEYER RADIO ONE OUTPUT PORT = SERIAL ','')
    else
        ChooseSerialPort('KEYER RADIO ONE OUTPUT PORT = SERIAL ','INVERT');

    end;

    'P': BEGIN
         ChooseParallelPort('KEYER RADIO ONE OUTPUT PORT = PARALLEL ');

        END;

     'W':begin
         Winkey := true;
         ChooseSerialPort('WINKEYER PORT = SERIAL ','');
         end;

     'Y':begin
         Yccc := true;
         writeln(FileWrite,'YCCC SO2R BOX ENABLE = TRUE');
         end;
    end;

    WriteLn;

    if winkey or yccc then exit; //both radio's set up
    REPEAT
        CWKey := UpCase (GetKey ('Do you want to set up a port to send CW with on the second radio? (Y/N) : '));
        IF CWKey = EscapeKey THEN Exit;
    UNTIL (CWKey = 'Y') OR (CWKey = 'N');
    WriteLn;

    IF CWKey = 'N' THEN Exit;

    REPEAT
        TypeKey := UpCase (GetKey ('Serial or parallel CW port interface for radio two? (S/P) : '));
        If TypeKey = EscapeKey THEN Exit;
    UNTIL (TypeKey = 'S') OR (TypeKey = 'P');
    WriteLn;

    IF TypeKey = 'S' THEN
       BEGIN
        TextColor (Cyan);

        WriteLn;
        WriteLn ('If you are keying a negative voltage radio with a serial port, you can have');
        WriteLn ('the signal inverted so you can simply use a PNP transistor to key the radio.');
    REPEAT
        InvKey := UpCase (GetKey ('Normal or inverted keying for radio two? (N/I) : '));
        If InvKey = EscapeKey THEN Exit;
    UNTIL (InvKey = 'N') OR (TypeKey = 'I');

    if InvKey = 'N' then
        ChooseSerialPort('KEYER RADIO TWO OUTPUT PORT = SERIAL ','')
    else
        ChooseSerialPort('KEYER RADIO TWO OUTPUT PORT = SERIAL ','INVERT');
    end
    else
         ChooseParallelPort('KEYER RADIO ONE OUTPUT PORT = PARALLEL ');

        WriteLn;
    END;


PROCEDURE SetUpRadioStuff;

VAR RadioKey: CHAR;
    TempString: Str40;
    Address: INTEGER;

    BEGIN
    ClrScr;
    TextColor (Yellow);

    WriteLnCenter ('SET UP SERIAL INTERFACE TO RADIO CONNECTED TO COMPUTER');
    WriteLn;
    TextColor (Cyan);

    WriteLn ('If you have a radio connected interfaced to your computer via a serial port');
    WriteLn ('so that you can send and receive commands, you can answer these questions to');
    WriteLn ('set up the interface.  The default baud rate is 4800 baud.  If you need a ');
    WriteLn ('different baud rate, add RADIO ONE BAUD RATE or RADIO TWO BAUD RATE to your');
    WriteLn ('config file after this procedure is over.');

    WriteLn;

    REPEAT
        RadioKey := UpCase (GetKey ('Do you want to set up a serial port for radio one? (Y/N) : '));
        IF RadioKey = EscapeKey THEN Exit;
    UNTIL (RadioKey = 'Y') OR (RadioKey = 'N');
    WriteLn;

    IF RadioKey = 'N' THEN Exit;

    REPEAT
        RadioKey := UpCase (GetKey ('First letter of radio brand (K)enwood or K2, (I)com: '));
        IF RadioKey = EscapeKey THEN Exit;
    UNTIL (RadioKey = 'K') OR (RadioKey = 'I');
    WriteLn;

    CASE RadioKey OF
        'I': BEGIN
             TempString := GetResponse ('Enter the Icom model number (i.e. IC781) : ');
             WriteLn (FileWrite, 'RADIO ONE TYPE = ', TempString);

             Address := GetValue ('Enter receiver address for this radio (check you manual) : ');

             IF Address >= 0 THEN
                 WriteLn (FileWrite, 'RADIO ONE RECEIVER ADDRESS = ', Address);

             END;

        'K': WriteLn (FileWrite, 'RADIO ONE TYPE = TS850S');

        END;

        writeln('Which serial port are you connecting radio one to?');
    WriteLn;
    ChooseSerialPort('RADIO ONE CONTROL PORT = SERIAL ','');
    WriteLn;

    REPEAT
        RadioKey := UpCase (GetKey ('Do you want to set up a serial port for radio two? (Y/N) : '));
        IF RadioKey = EscapeKey THEN Exit;
    UNTIL (RadioKey = 'Y') OR (RadioKey = 'N');
    WriteLn;

    IF RadioKey = 'N' THEN Exit;

    REPEAT
        RadioKey := UpCase (GetKey ('First letter of radio two brand (K)enwood or K2, (I)com: '));
        IF RadioKey = EscapeKey THEN Exit;
    UNTIL (RadioKey = 'K') OR (RadioKey = 'I');
    WriteLn;

    CASE RadioKey OF
        'I': BEGIN
             TempString := GetResponse ('Enter the Icom model number (i.e. IC781) : ');
             WriteLn (FileWrite, 'RADIO TWO TYPE = ', TempString);

             Address := GetValue ('Enter receiver address for this radio (check you manual) : ');

             IF Address >= 0 THEN
                 WriteLn (FileWrite, 'RADIO TWO RECEIVER ADDRESS = ', Address);

             END;

        'K': WriteLn (FileWrite, 'RADIO TWO TYPE = TS850S');

        END;

        writeln('Which serial port are you connecting radio two to?');
    WriteLn;
    ChooseSerialPort('RADIO TWO CONTROL PORT = SERIAL ','');
    WriteLn;

    END;


PROCEDURE SetUpPacketStuff;

VAR RadioKey: CHAR;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('SET UP PACKET');
    WriteLn;
    TextColor (Cyan);

    Writeln('If you want connect using the internet to a packet cluster choose yes.');
    Writeln('When trlog starts you can type control-b to get to the packet window. You will');
    Writeln('have a command prompt in the control-b window and can type a command like');
    Writeln('telnet n7az.net');
    Writeln('to connect to your favorite cluster.');
    Writeln;

    REPEAT
        RadioKey := UpCase (GetKey ('Do you want to set up packet? (Y/N) : '));
        IF RadioKey = EscapeKey THEN Exit;
    UNTIL (RadioKey = 'Y') OR (RadioKey = 'N');
    WriteLn;

    IF RadioKey = 'N' THEN Exit;

    WriteLn (FileWrite, 'PACKET PORT = SERIAL SHELL');
    WriteLn;
    END;


PROCEDURE SetUpMultiStuff;

VAR PortKey, RadioKey: CHAR;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('MULTI PORT SET UP');
    WriteLn;
    TextColor (Cyan);

    WriteLn ('If you are using a multi network by connecting computers together with the ');
    WriteLn ('serial port, this is where you tell the program which port you are using.');
    WriteLn;
    WriteLn ('You can also select a COMPUTER ID for this computer.  The COMPUTER ID gets');
    WriteLn ('appended to the QSO number for QSOs made on this computer.  This allows you');
    WriteLn ('to sort out the logs by computer after the contest (using POST).');
    WriteLn;

    REPEAT
        RadioKey := UpCase (GetKey ('Do you need to set up the multi port? (Y/N) : '));
        IF RadioKey = EscapeKey THEN Exit;
    UNTIL (RadioKey = 'Y') OR (RadioKey = 'N');
    WriteLn;

    IF RadioKey = 'N' THEN Exit;

    REPEAT
        PortKey := UpCase (GetKey ('Which serial port is the network on? (1-4) : '));
    UNTIL (PortKey = '1') OR (PortKey = '2') OR (PortKey = '3') OR (PortKey = '4');
    WriteLn;

    CASE PortKey OF
        '1': WriteLn (FileWrite, 'MULTI PORT = SERIAL 1');
        '2': WriteLn (FileWrite, 'MULTI PORT = SERIAL 2');
        '3': WriteLn (FileWrite, 'MULTI PORT = SERIAL 3');
        '4': WriteLn (FileWrite, 'MULTI PORT = SERIAL 4');
        END;

    WriteLn;

    REPEAT
        RadioKey := UpCase (GetKey ('Do you want to set a computer ID letter for this computer? (Y/N) : '));
        IF RadioKey = EscapeKey THEN Exit;
    UNTIL (RadioKey = 'Y') OR (RadioKey = 'N');
    WriteLn;

    IF RadioKey = 'N' THEN Exit;

    REPEAT
        PortKey := UpCase (GetKey ('Enter single character ID for this computer (A-Z or escape for none) : '));
        IF PortKey = EscapeKey THEN Exit;
    UNTIL (PortKey >= 'A') AND (PortKey <= 'Z');
    WriteLn;

    WriteLn (FileWrite, 'COMPUTER ID = ', PortKey);
    END;



PROCEDURE ManualConfigurationTitle;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('CONFIGURATION FILE GENERATOR');
    WriteLn;
    TextColor (Cyan);
    END;



PROCEDURE InverseVideo (Entry: INTEGER; StartY: INTEGER; NumberRows: INTEGER; Contest: Str20);

VAR Row, Col: INTEGER;

    BEGIN
    WHILE Length (Contest) < 20 DO Contest := Contest + ' ';

    NoCursor;
    Col := (Entry DIV NumberRows) * 20 + 1;
    Row := (Entry MOD NumberRows);
    GoToXY (Col, Row + StartY);

    TextBackGround (White);
    TextColor (Black);
    Write (Contest);
    END;

PROCEDURE NormalVideo (Entry: INTEGER;  StartY: INTEGER; NumberRows: INTEGER; Contest: Str20);

VAR Row, Col: INTEGER;

    BEGIN
    WHILE Length (Contest) < 20 DO Contest := Contest + ' ';

    Col := (Entry DIV NumberRows) * 20 + 1;
    Row := (Entry MOD NumberRows);

    NoCursor;
    GoToXY (Col, Row + StartY);

    TextBackGround (Black);
    TextColor (Yellow);
    Write (Contest);
    END;



FUNCTION GetContestName: Str80;

VAR Contests: ARRAY [0..80] OF Str20;
    NumberContests, Address, Index, StartY, Entry: INTEGER;
    ActiveEntry, NumberRows, BubbleCount: INTEGER;
    EntryString, TempString: Str80;
    Key: CHAR;

    BEGIN
    ClrScr;
    WriteLn ('Select the contest you wish to operate (use arrow keys) : ');
    WriteLn;

    GetContestName := '';
    EntryString := '';

    { You get these for free }

    Contests [0] := 'REGION 1 FIELD DAY';
    Contests [1] := 'FIELD DAY';
    Contests [2] := 'JA INTERNATIONAL DX';
    Contests [3] := 'NZ FIELD DAY';
    Contests [4] := 'SOUTH AMERICAN WW';
    Contests [5] := 'STEW PERRY';
    Contests [6] := 'WRTC 2018'; {KK1L: 6.68 moved here for TRFree 1.04}

    NumberContests := 7;

    IF NOT TRFree THEN
        BEGIN
        Contests [7]  := 'YO DX';
        Contests [8]  := 'ALL ASIAN';
        Contests [9]  := 'ALL JA';
        Contests [10] := 'ARCI';
        Contests [11] := 'ARI';
        Contests [12] := 'ARRL 10';
        Contests [13] := 'ARRL 160';
        Contests [14] := 'ARRL DX';
        Contests [15] := 'ARRL VHF QSO';
        Contests [16] := 'ARRL VHF SS';
        Contests [17] := 'CAL QSO PARTY';
        Contests [18] := 'COUNTY HUNTER';
        Contests [19] := 'CQ 160';
        Contests [20] := 'CQ M';
        Contests [21] := 'CQ WPX';
        Contests [22] := 'CQ WW';
        Contests [23] := 'CROATIAN';
        Contests [24] := 'CWO';
        Contests [25] := 'CWT';
        Contests [26] := 'EUROPEAN HFC';
        Contests [27] := 'EUROPEAN VHF';
        Contests [28] := 'GENERAL QSO';
        Contests [29] := 'GRID LOC';
        Contests [30] := 'HA DX';
        Contests [31] := 'HELVETIA';
        Contests [32] := 'IARU';
        Contests [33] := 'INTERNET SPRINT';
        Contests [34] := 'IOTA';
        Contests [35] := 'KCJ';
        Contests [36] := 'KVP';
        Contests [37] := 'MICH QSO PARTY';
        Contests [38] := 'MINN QSO PARTY';
        Contests [39] := 'NA QSO';
        Contests [40] := 'NEW ENGLAND QSO';
        Contests [41] := 'NRAU BALTIC';
        Contests [42] := 'OK DX';
        Contests [43] := 'PACC';
        Contests [44] := 'QCWA';
        Contests [45] := 'QCWA GOLDEN';
        Contests [46] := 'RAC';
        Contests [47] := 'ROPOCO';
        Contests [48] := 'RUSSIAN DX';
        Contests [49] := 'SAC';
        Contests [50] := 'SALMON RUN';
        Contests [51] := 'SP DX';
        Contests [52] := 'SPRINT';
        Contests [53] := 'SWEEPSTAKES';
        Contests [54] := 'TEN TEN';
        Contests [55] := 'TEXAS QSO PARTY';
        Contests [56] := 'TOEC';
        Contests [57] := 'OCEANIA';  { Was VK/ZL }
        Contests [58] := 'WAE';
        Contests [59] := 'WAG';
        Contests [60] := 'WISCONSIN QSO PARTY';
        Contests [61] := 'WWL';
        Contests [62] := 'XMAS';
        Contests [63] := 'CQ VHF';
        Contests [64] := 'EU SPRINT';
        Contests [65] := 'KIDS DAY';
        Contests [66] := 'FLORIDA QSO PARTY';
        Contests [67] := 'AP SPRINT';
        Contests [68] := 'OHIO QSO PARTY';
        Contests [69] := 'UKRAINIAN';
        Contests [70] := 'CQ WW RTTY';
        Contests [71] := 'CQ WPX RTTY';
        Contests [72] := 'UBA';
        Contests [73] := 'FISTS';
        Contests [74] := 'BALTIC';
        Contests [75] := 'ARRL RTTY ROUNDUP';
        Contests [76] := 'MARCONI MEMORIAL';
        Contests [77] := 'NYQP';
        Contests [78] := 'MST';
        Contests [79] := 'SST';

        NumberContests := 80;  { One more than last index value }
        END;

    StartY := WhereY;
    ActiveEntry := 0;

    { Sort them in case I put one in the wrong place or the end }

    IF NumberContests > 1 THEN
        BEGIN
        Index := NumberContests - 2;

        FOR BubbleCount := 1 TO NumberContests - 1 DO
            BEGIN
            FOR Address := 0 TO Index DO
                IF Contests [Address] > Contests [Address + 1] THEN
                    BEGIN
                    TempString := Contests [Address + 1];
                    Contests [Address + 1] := Contests [Address];
                    Contests [Address] := TempString;
                    END;
            Dec (Index);
            END;
        END;

    NumberRows := NumberContests DIV 4 + 1;

    { Put up Initial Display }

    InverseVideo (0, StartY, NumberRows, Contests [0]);

    FOR Entry := 1 TO NumberContests - 1 DO
        NormalVideo (Entry, StartY, NumberRows, Contests [Entry]);

    ActiveEntry := 0;

    REPEAT
        Key := UpCase (ReadKey);

        CASE Key OF
            EscapeKey, ControlC:
                BEGIN
                GetContestName := '';
                TextBackground (Black);
                TextColor (Yellow);
                SmallCursor;
                ClrScr;
                TextMode (LastMode);
                Halt;
                END;

            CarriageReturn:
                BEGIN
                GetContestName := Contests [ActiveEntry];
                TextBackground (Black);
                TextColor (Yellow);
                SmallCursor;
                ClrScr;
                Exit;
                END;

            BackSpace:
                IF EntryString <> '' THEN
                    Delete (EntryString, Length (EntryString), 1);

            NullKey:
                BEGIN
                Key := ReadKey;

                CASE Key OF
                    DeleteKey:
                        IF EntryString <> '' THEN
                            Delete (EntryString, Length (EntryString), 1);

                    RightArrow:
                        IF ActiveEntry < NumberContests - NumberRows THEN
                            BEGIN
                            NormalVideo (ActiveEntry, StartY, NumberRows, Contests [ActiveEntry]);
                            ActiveEntry := ActiveEntry + NumberRows;
                            InverseVideo (ActiveEntry, StartY, NumberRows, Contests [ActiveEntry]);
                            END;

                    LeftArrow:
                        IF ActiveEntry >= NumberRows THEN
                            BEGIN
                            NormalVideo (ActiveEntry, StartY, NumberRows, Contests [ActiveEntry]);
                            ActiveEntry := ActiveEntry - NumberRows;
                            InverseVideo (ActiveEntry, StartY, NumberRows, Contests [ActiveEntry]);
                            END;

                    UpArrow:
                        IF ActiveEntry > 0 THEN
                            BEGIN
                            NormalVideo (ActiveEntry, StartY, NumberRows, Contests [ActiveEntry]);
                            ActiveEntry := ActiveEntry - 1;
                            InverseVideo (ActiveEntry, StartY, NumberRows, Contests [ActiveEntry]);
                            END;

                    DownArrow:
                        IF ActiveEntry + 1 < NumberContests THEN
                            BEGIN
                            NormalVideo (ActiveEntry, StartY, NumberRows, Contests [ActiveEntry]);
                            ActiveEntry := ActiveEntry + 1;
                            InverseVideo (ActiveEntry, StartY, NumberRows, Contests [ActiveEntry]);
                            END;

                    HomeKey:
                        BEGIN
                        NormalVideo (ActiveEntry, StartY, NumberRows, Contests [ActiveEntry]);
                        ActiveEntry := 0;
                        InverseVideo (ActiveEntry, StartY, NumberRows, Contests [ActiveEntry]);
                        END;

                    EndKey:
                        BEGIN
                        NormalVideo (ActiveEntry, StartY, NumberRows, Contests [ActiveEntry]);
                        ActiveEntry := NumberContests - 1;
                        InverseVideo (ActiveEntry, StartY, NumberRows, Contests [ActiveEntry]);
                        END;

                    END;
                END;

            ELSE
                IF ((Key >= 'A') AND (Key <= 'Z')) OR (Key = ' ') OR ((Key >= '0') OR (Key <= '9')) THEN
                    BEGIN
                    EntryString := EntryString + Key;

                    FOR Entry := 0 TO NumberContests - 1 DO
                        IF Copy (Contests [Entry], 1, Length (EntryString)) = EntryString THEN
                            BEGIN
                            NormalVideo (ActiveEntry, StartY, NumberRows, Contests [ActiveEntry]);
                            ActiveEntry := Entry;
                            InverseVideo (ActiveEntry, StartY, NumberRows, Contests [ActiveEntry]);
                            Break;
                            END;
                    END;
            END;
    UNTIL False;
    END;



PROCEDURE ManualConfigFileGenerator;

VAR Key, SimulatorKey: Char;
    FileName, MyState, MyName, MyQTH: Str20;
    FileString, ContestName: Str80;
    TempQTH: QTHRecord;
    FileRead: TEXT;

    BEGIN
    ManualConfigurationTitle;

    IF NOT CountryTable.LoadInCountryFile THEN
        BEGIN
        ReportError ('Unable to find CTY.DAT country file!!');
        WriteLn ('Make sure this file is in the same directory as the program.');
        Halt;
        END;

    MyName  := '';
    MyState := '';
    MyQTH   := '';

    MyCall := UpperCase (GetResponse ('Enter your complete callsign (none to abort) : '));

    IF MyCall = '' THEN
        BEGIN
        TextMode (LastMode);
        Halt;
        END;

    LocateCall (MyCall, TempQTH, True);
    MyCountry     := CountryTable.GetCountryID (TempQTH.Country);
    MyContinent   := TempQTH.Continent;
    Str (TempQTH.Zone,  MyZone);
    CountryString   := MyCountry;
    ContinentString := CountryTable.GetContinentName (MyContinent);

    ContestName := GetContestName;

    IF ContestName = '' THEN
        BEGIN
        TextMode (LastMode);
        ReportError ('No contest chosen.');
        Halt;
        END;

    IF NOT FoundContest (ContestName) THEN
        BEGIN
        ReportError ('I am sorry - I do not know that contest.  Please tell N6TR.');
        TextMode (LastMode);
        Halt;
        END;

    IF UpperCase (ContestName) = 'NEW ENGLAND QSO' THEN
        BEGIN
        REPEAT
            Key := UpCase (GetKey ('Are you in New England? (Y/N) : '));

            IF Key = EscapeKey THEN
                BEGIN
                TextMode (LastMode);
                Halt;
                END;

        UNTIL (Key = 'Y') OR (Key = 'N');
        WriteLn;
        IF Key = 'Y' THEN
            BEGIN
            REPEAT
              MyState := GetResponse ('Enter your New England state abreviation (ME, NH, VT, MA, CT, RI): ');
            UNTIL (UpperCase (Copy (MyState, 1, 2)) = 'ME') OR
                  (UpperCase (Copy (MyState, 1, 2)) = 'NH') OR
                  (UpperCase (Copy (MyState, 1, 2)) = 'VT') OR
                  (UpperCase (Copy (MyState, 1, 2)) = 'MA') OR
                  (UpperCase (Copy (MyState, 1, 2)) = 'CT') OR
                  (UpperCase (Copy (MyState, 1, 2)) = 'RI');
            END;
            MyState := (UpperCase (Copy (MyState, 1, 2)));
        END;

    {KK1L: 6.69}
    IF UpperCase (ContestName) = 'FIELD DAY' THEN
        BEGIN
        Key := UpCase (GetKey ('Are you operating from North or South America? (Y/N) : '));
        IF Key = 'Y' THEN
            BEGIN
            WriteLn;
            MyFDClass := UpperCase (GetResponse ('Enter your Field Day Class (2A, 1D, 34A, etc) : '));
            MySection := UpperCase (GetResponse ('Enter your ARRL section or "DX" : '));
            WriteLn ('Thanks!!  I will use this information to generate your CW messages.');
            END
        ELSE
            MyFDClass := 'DX';
        END;

    IF UpperCase (ContestName) = 'CAL QSO PARTY' THEN
        BEGIN
        REPEAT
            Key := UpCase (GetKey ('Are you in California? (Y/N) : '));

            IF Key = EscapeKey THEN
                BEGIN
                TextMode (LastMode);
                Halt;
                END;

        UNTIL (Key = 'Y') OR (Key = 'N');
        WriteLn;
        IF Key = 'Y' THEN MyState := 'CA' ELSE MyState := '';
        END;

    {KK1L: 6.71 One user configured WRTC instead of IARU. This is an attempt to eliminate that error.}
    IF Pos ('WRTC', UpperCase (ContestName)) > 0  THEN
        BEGIN
        REPEAT
            WriteLn ('WRTC is by invitation only. It is different than the IARU HF Championship.');
            Key := UpCase (GetKey ('Did you really mean to configure the IARU HF Championship instead? (Y/N) : '));

            IF Key = EscapeKey THEN
                BEGIN
                TextMode (LastMode);
                Halt;
                END;

        UNTIL (Key = 'Y') OR (Key = 'N');
        WriteLn;
        IF Key = 'Y' THEN
            BEGIN
            ContestName := 'IARU';
            WriteLn;
            WriteLn ('I will set you up for the IARU HF Championship.');
            WriteLn;
            END;
        END;

    IF UpperCase (ContestName) = 'FLORIDA QSO PARTY' THEN
        BEGIN
        REPEAT
            Key := UpCase (GetKey ('Are you in Florida? (Y/N) : '));

            IF Key = EscapeKey THEN
                BEGIN
                TextMode (LastMode);
                Halt;
                END;

        UNTIL (Key = 'Y') OR (Key = 'N');
        WriteLn;
        IF Key = 'Y' THEN MyState := 'FL' ELSE MyState := '';
        END;

    IF UpperCase (ContestName) = 'MICH QSO PARTY' THEN
        BEGIN
        REPEAT
            Key := UpCase (GetKey ('Are you in Michigan? (Y/N) : '));

            IF Key = EscapeKey THEN
                BEGIN
                TextMode (LastMode);
                Halt;
                END;

        UNTIL (Key = 'Y') OR (Key = 'N');
        WriteLn;
        IF Key = 'Y' THEN MyState := 'MI' ELSE MyState := '';
        END;

    IF UpperCase (ContestName) = 'MINN QSO PARTY' THEN
        BEGIN
        REPEAT
            Key := UpCase (GetKey ('Are you in Minnesota? (Y/N) : '));

            IF Key = EscapeKey THEN
                BEGIN
                TextMode (LastMode);
                Halt;
                END;

        UNTIL (Key = 'Y') OR (Key = 'N');
        WriteLn;
        IF Key = 'Y' THEN MyState := 'MN' ELSE MyState := '';
        END;

    IF UpperCase (ContestName) = 'OHIO QSO PARTY' THEN
        BEGIN
        REPEAT
            Key := UpCase (GetKey ('Are you in Ohio? (Y/N) : '));

            IF Key = EscapeKey THEN
                BEGIN
                TextMode (LastMode);
                Halt;
                END;

        UNTIL (Key = 'Y') OR (Key = 'N');
        WriteLn;
        IF Key = 'Y' THEN MyState := 'OH' ELSE MyState := '';
        END;

    IF UpperCase (ContestName) = 'SALMON RUN' THEN
        BEGIN
        REPEAT
            Key := UpCase (GetKey ('Are you in Washington? (Y/N) : '));

            IF Key = EscapeKey THEN
                BEGIN
                TextMode (LastMode);
                Halt;
                END;

        UNTIL (Key = 'Y') OR (Key = 'N');
        WriteLn;
        IF Key = 'Y' THEN MyState := 'WA' ELSE MyState := '';
        END;

    IF UpperCase (ContestName) = 'STEW PERRY' THEN
        BEGIN
        MyGrid := UpperCase (GetResponse ('Enter your four digit grid square : '));
        END;

    IF UpperCase (ContestName) = 'WISCONSIN QSO PARTY' THEN
        BEGIN
        REPEAT
            Key := UpCase (GetKey ('Are you in Wisconsin? (Y/N) : '));

            IF Key = EscapeKey THEN
                BEGIN
                TextMode (LastMode);
                Halt;
                END;

        UNTIL (Key = 'Y') OR (Key = 'N');
        WriteLn;
        IF Key = 'Y' THEN MyState := 'WI' ELSE MyState := '';
        END;

    IF UpperCase (ContestName) = 'ARRL 10' THEN
        IF (MyCountry = 'K') OR (MyCountry = 'VE') OR
           (MyCountry = 'KH6') OR (MyCountry = 'KL') THEN
               BEGIN
               MyState := UpperCase (GetResponse ('Enter the QTH that you want to send : '));
               WriteLn ('Thanks!!  I will use this information to generate your CW messages.');
               END;

    IF UpperCase (ContestName) = 'SWEEPSTAKES' THEN
        BEGIN
        MyPrec := UpperCase (GetResponse ('Enter your precedence : '));
        MyCheck := UpperCase (GetResponse ('Enter your check (last two digits of year licensed) : '));
        MySection := UpperCase (GetResponse ('Enter your ARRL section : '));
        WriteLn ('Thanks!!  I will use this information to generate your CW messages.');
        END;

    IF UpperCase (ContestName) = 'SPRINT' THEN
        BEGIN
        MyName := UpperCase (GetResponse ('Enter the name you want to use : '));
        MyState := UpperCase (GetResponse ('Enter your QTH that you want to send : '));
        WriteLn ('Thanks!!  I will use this information to generate your CW messages.');
        END;

    IF UpperCase (ContestName) = 'TEXAS QSO PARTY' THEN
        BEGIN
        REPEAT
            Key := UpCase (GetKey ('Are you in Texas? (Y/N) : '));

            IF Key = EscapeKey THEN
                BEGIN
                TextMode (LastMode);
                Halt;
                END;

            UNTIL (Key = 'Y') OR (Key = 'N');
        WriteLn;
        IF Key = 'Y' THEN MyState := 'TX' ELSE MyState := '';
        END;

    TextColor (Cyan);

    WriteLn ('TR is about to save this information to your configuration file.  Normally,');
    WriteLn ('the configuration file is called LOGCFG.DAT.  However, you can choose to use');
    WriteLn ('a name that is more descriptive, like 98SSCW.CFG.  This will also allow you');
    WriteLn ('to have different contest files reside in the same directory.  Your log file');
    WriteLn ('will be named 98SSCW.LOG instead of LOG.DAT, and the RESTART.BIN file will be');
    WriteLn ('named 98SSCW.RST.  The LOG.TMP file would be named 98SSCW.TMP.');
    WriteLn;
    WriteLn ('If you have more than one set of files present when TR starts up, it will show');
    WriteLn ('you a menu of the different contest files, and you can select which one to use.');
    WriteLn;

    REPEAT
        FileName := GetResponse ('Enter filename or just RETURN to use the default names : ');

        IF StringHas (FileName, '.') THEN FileName := PrecedingString (FileName, '.');

        IF FileName = '' THEN
            FileName := 'LOGCFG.DAT'
        ELSE
            FileName := FileName + '.CFG';

        IF FileExists (FileName) THEN
            BEGIN
            REPEAT
                Key := Upcase (GetKey (FileName + ' already exists.  Okay to overwrite? (Y/N) : '));

            IF Key = EscapeKey THEN
                BEGIN
                TextMode (LastMode);
                Halt;
                END;

            UNTIL (Key = 'Y') OR (Key = 'N');
            END;

    UNTIL (NOT FileExists (FileName)) OR (Key = 'Y');

    IF FileName = 'LOGCFG.DAT' THEN
        DeleteFile ('RESTART.BIN')
    ELSE
        DeleteFile (PrecedingString (FileName, '.') + '.RST');

    SetUpFileNames (PrecedingString (FileName, '.'));

    OpenFileForWrite (FileWrite, LogConfigFileName);

    WriteLn (FileWrite, 'MY CALL = ', MyCall);

    IF MyQTH     <> '' THEN WriteLn (FileWrite, 'MY QTH = ', MyQTH);
    IF MyState   <> '' THEN WriteLn (FileWrite, 'MY STATE = ', MyState);
    IF MyName    <> '' THEN WriteLn (FileWrite, 'MY NAME = ', MyName);
    IF MyPrec    <> '' THEN WriteLn (FileWrite, 'MY PREC = ', MyPrec);
    IF MyCheck   <> '' THEN WriteLn (FileWrite, 'MY CHECK = ', MyCheck);
    IF MySection <> '' THEN WriteLn (FileWrite, 'MY SECTION = ', MySection);
    IF MyFDClass <> '' THEN WriteLn (FileWrite, 'MY FD CLASS = ', MyFDClass); {KK1L: 6.69}

    WriteLn (FileWrite, 'CONTEST = ', ContestName);

    ManualConfigurationTitle;

//Everyone wants color now. Don't even ask.
    WriteLn (FileWrite, 'DISPLAY MODE = COLOR');

    REPEAT
        SimulatorKey := UpCase (GetKey ('Do you want to use the simulator? (Y/N) : '));

            IF Key = EscapeKey THEN
                BEGIN
                TextMode (LastMode);
                Halt;
                END;

    UNTIL (SimulatorKey = 'Y') OR (SimulatorKey = 'N');
    WriteLn;

    IF SimulatorKey = 'Y' THEN
    begin
        WriteLn (FileWrite, 'SIMULATOR ENABLE = TRUE');
        WriteLn (FileWrite, 'BEEP SOUNDCARD ENABLE = TRUE');
    end

    ELSE
        BEGIN
        SetUpCWStuff;
        SetUpRadioStuff;
        SetUpPacketStuff;
//        SetUpMultiStuff;
        END;

    Close (FileWrite);

    ClrScr;

    TextColor (Cyan);
    WriteLn ('Your config file has been saved as ', LogConfigFileName, '.  Here is what it');
    WriteLn ('looks like: ');
    WriteLn;

    OpenFileForRead (FileRead, LogConfigFileName);

    WHILE NOT Eof (FileRead) DO
        BEGIN
        ReadLn (FileRead, FileString);
        WriteLn (FileString);
        END;

    Close (FileRead);
    WaitForKeyPressed;
    END;



PROCEDURE PullLogCfgInformationOutOfThisPerson;

VAR Key: Char;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('WELCOME TO THE TR LOGGING PROGRAM');
    WriteLn;
    TextColor (Cyan);
    WriteLn;

    REPEAT
        Key := UpCase (GetKey ('Do you want to step through setting up a new contest? (Y/N) : '));

        IF Key = EscapeKey THEN
            BEGIN
            TextMode (LastMode);
            Halt;
            END;

    UNTIL (Key = 'Y') OR (Key = 'N');
    WriteLn;

    IF Key = 'N' THEN
        BEGIN
        WriteLn;
        WriteLn ('Bye-bye');
        TextMode (LastMode);
        Halt;
        END;

    ManualConfigFileGenerator;
    END;



    BEGIN
    END.
