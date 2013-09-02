UNIT LogPack;

{ This unit has most of the stuff used to make the packet work.  }

{$O+}
{$F+}
{$V-}

INTERFACE


USES LogDupe, LogEdit, LogStuff, LogK1EA, Country9, Tree, LogWind, trCrt,
     LogGrid;

CONST PacketSpotListLength = 10;
TYPE
    PacketSpotModeType = (AllSpots, MultSpots);

    SpotModeType = (NormalSpot, SHDXSpot);

    DXSpotType = RECORD
        Band:            BandType;     { The band of the spot }
        Call:            CallString;   { The callsign spotted }
        Frequency:       LONGINT;      { LONGINT of spotted frequency }
        FrequencyString: Str20;        { The frequency in string format }
        Mode:            ModeType;     { The mode of the spot }
        Notes:           Str40;        { Any notes }
        PacketString:    STRING;       { Original packet string as received }
        QSXFrequency:    LONGINT;      { Any QSX frequency }
        SpotMode:        SpotModeType; { NormalSpot or SHDXSpot }
        SourceCall:      CallString;   { Callsign of station making the post }
        TimeString:      STRING [5];   { Time shown in packet spot - includes the Z }
        END;

    PacketEntry = RECORD
        Frequency:    LONGINT;
        QSXFrequency: LONGINT;
        Call:         CallString;
        Time:         STRING [5];
        END;

    PacketObject = OBJECT
        BroadcastAllPacketData:   BOOLEAN;

        EightBitPacketPort:       BOOLEAN;

        PacketBandSpots:          BOOLEAN;
        PacketBaudRate:           LONGINT;
        PacketBeep:               BOOLEAN;

        PacketDisplayBuffer:      ARRAY [0..13] OF Str80;
        PacketDisplayBufferStart: INTEGER;
        PacketDisplayBufferEnd:   INTEGER;
        PacketDisplayLine:        STRING;

        PacketInputFileName:      Str40;
        PacketInputFileOpen:      BOOLEAN;
        PacketInputFileRead:      TEXT;

        PacketLogFileName:        Str40;

        PacketMemory:             ARRAY [0..PacketSpotListLength] OF PacketEntry;

        { Start = first entry of list.  End = cell after last data }

        PacketMemoryStart:        INTEGER;
        PacketMemoryEnd:          INTEGER;
        PacketMessage:            STRING;
        PacketMessageFromNetwork: Str80;
        PacketSpots:              PacketSpotModeType;

        PROCEDURE AnalyzePacketString (PacketString: STRING);

        { This procedure will look at the packet message and try to
          determine what kind of message it is, and route it to the
          right place.  Note that the PacketDisplay during control-B
          and data the PacketDisplayBuffer is handled by a different
          routine.  It can identify a DX announcmenet, a talk message
          sent to MyCall or ALL, and responses to a SH/DX command.}

        PROCEDURE CheckPacket;

        { This is the hearbeat of all of the packet stuff.  Needs to be
          called often so things don't pileup in the input buffer. }


        PROCEDURE CheckPacketBuffer (DisplayNewData: BOOLEAN);

        { Looks at the packet input buffer to see if any new data is
          there.  It places it in the DisplayBuffer, PacketDisplayLine
          and PacketMessage so other routines can find it.  If
          DisplayNewData is set - new data will be written to the CRT also. }


        PROCEDURE CheckPacketDisplayLine (VAR DisplayLine: STRING);

        { Checks the PacketDisplayLine string to see if there is now a
          completed line (terminated by line feed) there that can be
          pushed into the PacketDisplayBuffer. }


        PROCEDURE CheckPacketMessage;

        { Checks the PacketMessage variable to see if there is any data
          there that needs to be acted on by AnalyzePacketMessage.  Gets
          called whenever CheckPacket is called. }


        PROCEDURE DeletePacketEntry (DeleteCall: CallString;
                                     DeleteBand: BandType;
                                     DeleteMode: ModeType);

        { Removes the specified spot from the Control-U memory }


        PROCEDURE DeletePacketSpot (SpotAddress: INTEGER);

        { Removes the selected spot from the Control-U memory }


        PROCEDURE DisplayPacketDisplayBuffer;

        { Does NOT display the PacketDisplayLine which has incompleted
          line data in it. }


        PROCEDURE Init;

        { Gets executed at power up automatically }

        PROCEDURE ProcessPacketMessageFromNetWork (MessageString: STRING);

        { Gets called when a packet message is received over the network }


        PROCEDURE ProcessPacketSpot (DXSpot: DXSpotType);

        { Gets called when a packet spot (DX de) has been detected by
          AnalyzePacketMessage }


        PROCEDURE PushPacketSpot (DXSpot: DXSpotType);

        { Gets called by ProcessPacketSpot if it decides to save the DX spot
          for the Control-U command to access. }


        PROCEDURE PushPacketMemory (DXSpot: DXSpotType);

        { Used to push a spot into the Control-U packet spot buffer. }

        END;


VAR Packet: PacketObject;
    PacketSpotKey: CHAR;
    PacketWindowUp: BOOLEAN;
    StartTime: TimeRecord;
    PacketInputFileDelay: INTEGER;
    PacketSpotPrefixOnly: BOOLEAN; {KK1L: 6.72}

IMPLEMENTATION
uses keycode,beep;



PROCEDURE FixUpQSXFreq (VAR QSXFreq: REAL; PacketFrequency: LONGINT);

VAR BaseFreq: LONGINT;

    BEGIN
    IF QSXFreq = 0 THEN Exit;

    BaseFreq := Trunc (PacketFrequency / 1000000) * 1000000;

    IF QSXFreq < 10.0 THEN         { Someone put mHz in? }
        BEGIN
        QSXFreq := Int (QSXFreq * 1000000.0);
        Exit;
        END;

    IF QSXFreq < 1000.0 THEN       { Someone entered last three digits }
        BEGIN
        QSXFreq := BaseFreq + (QSXFreq * 1000.0);
        Exit;
        END;

    { Someone entered the whole frequency in kilohertz }

    QSXFreq := QSXFreq * 1000.0;
    END;



FUNCTION LookForQSXFrequency (PacketFrequency: LONGINT; Notes: Str40): LONGINT;

{ Returns zero if nothing found. }

VAR QSXFreq: REAL;
    Result: INTEGER;
    BaseFreq: LONGINT;
    FrequencyString, TempString: Str80;

    BEGIN
    LookForQSXFrequency := 0;

    IF NOT QSXEnable THEN Exit;

    IF Notes = '' THEN Exit;

    Notes := UpperCase (Notes);

    IF StringHas (Notes, 'QSX') OR StringHas (Notes, 'LISTENING') THEN
        BEGIN
        IF StringHas (Notes, 'QSX') THEN
            TempString := PostcedingString (Notes, 'QSX')
        ELSE
            IF StringHas (Notes, 'LISTENING') THEN
                TempString := PostcedingString (Notes, 'LISTENING');

        FrequencyString := GetFirstString (TempString);

        IF StringIsAllNumbersOrDecimal (FrequencyString) THEN
            BEGIN
            IF StringHas (FrequencyString, '.') THEN
                BEGIN
                Val (FrequencyString, QSXFreq, Result);

                { Convert MHz to Hz }

                IF QSXFreq < 1500 THEN       { Assume MHz }
                    LookForQSXFrequency := Round (QSXFreq * 1000000.0)
                ELSE
                    LookForQSXFrequency := Round (QSXFreq * 1000.0);
                Exit;
                END;

            { String is all numbers - probably in kHz }

            IF Length (FrequencyString) >= 4 THEN  { Entered complete kHz }
                BEGIN
                Val (FrequencyString, QSXFreq, Result);

                { Convert KHz to Hz }

                LookForQSXFrequency := Round (QSXFreq * 1000.0);
                Exit;
                END;

            IF Length (FrequencyString) = 3 THEN   { Entered kHz w/o MHz }
                BEGIN
                Val (FrequencyString, QSXFreq, Result);

                { Compute MHz part of PacketFrequency }

                BaseFreq := Trunc (PacketFrequency / 1000000) * 1000000;

                { Add them together }

                LookForQSXFrequency := Round (BaseFreq + (QSXFreq * 1000.0));
                Exit;
                END;
            END;
        END;

    IF StringHas (Notes, 'UP') THEN
        BEGIN
        TempString := PostcedingString (Notes, 'UP');

        FrequencyString := GetFirstString (TempString);

        IF Length (FrequencyString) <= 2 THEN   { Entered kHz offset }
            BEGIN
            Val (FrequencyString, QSXFreq, Result);

            { Add them together }

            LookForQSXFrequency := Round (PacketFrequency + (QSXFreq * 1000.0));
            Exit;
            END;
        END;

    IF StringHas (Notes, 'DOWN') THEN
        BEGIN
        TempString := PostcedingString (Notes, 'DOWN');

        FrequencyString := GetFirstString (TempString);

        IF Length (FrequencyString) <= 2 THEN   { Entered kHz offset }
            BEGIN
            Val (FrequencyString, QSXFreq, Result);

            { Add them together }

            LookForQSXFrequency := Round (PacketFrequency - (QSXFreq * 1000.0));
            Exit;
            END;
        END;

    END;




FUNCTION TalkOrAnnounceMessage (PacketSpot: STRING): BOOLEAN;


VAR FirstString, LastString: Str80;

    BEGIN
    FirstString := GetFirstString (PacketSpot);
    LastString  := GetLastString  (PacketSpot);

    TalkOrAnnounceMessage := False;

    IF LastString = '>' THEN Exit;

    IF (FirstString = 'ALL') OR StringHas (FirstString, MyCall) THEN
        TalkOrAnnounceMessage := True;
    END;



FUNCTION FoundDXSpot (InputString: STRING; VAR DXSpot: DXSpotType): BOOLEAN;

{ Looks at the string passed to it to determine if it looks like a DX
  packet spot.  The format expected is as follows:

         1         2         3         4         5         6         7
123456789012345678901234567890123456789012345678901234567890123456789012345

DX de OK1CF:     10107.1  WP4KGR       Marconi station                2148Z   }


VAR Result: INTEGER;
    TempFrequency: REAL;

    BEGIN
    FoundDXSpot := False;

    IF UpperCase (Copy (InputString, 1, 5)) <> 'DX DE' THEN Exit;

    RemoveFirstString (InputString);  { DX }
    RemoveFirstString (InputString);  { de }

    WITH DXSpot DO
        BEGIN
        PacketString := InputString;

        SourceCall := UpperCase (RemoveFirstString (InputString));
        SourceCall := PrecedingString (SourceCall, ':');

        FrequencyString := RemoveFirstString (InputString);

        Val (FrequencyString, TempFrequency, Result);

        IF Result <> 0 THEN Exit;

        IF TempFrequency < MaxLongInt DIV 9 THEN
            TempFrequency := TempFrequency * 1000;      { Convert kHz to Hz }

        { See if more than 2,000,000,000 Hz or 2 GHz }

        IF (TempFrequency > 2000000000) OR (TempFrequency < 0) THEN
            Exit;

        { Convert real frequency to LONGINT }

        Frequency := Round (TempFrequency);

        Mode := ActiveMode;  { In case on 160 }

        CalculateBandMode (Frequency, Band, Mode);

        IF Band = NoBand THEN Exit;
        IF Mode = NoMode THEN Exit;

        Call := RemoveFirstString (InputString);

        IF NOT GoodCallSyntax (Call) THEN Exit;

        TimeString := RemoveLastString (InputString);

        GetRidOfPrecedingSpaces  (InputString);
        GetRidOfPostcedingSpaces (InputString);

        Notes := InputString;

        QSXFrequency := LookForQSXFrequency (Frequency, Notes);

        SpotMode := NormalSpot;

        FoundDXSpot := True;
        END;
    END;



FUNCTION ShowDXResponse (InputString: STRING; VAR DXSpot: DXSpotType): BOOLEAN;

{ Processes responses to SH/DX commands.  Examples include :

3503.2   DL7ON       16-Nov-1999 0603Z                             DL <N4ZR>
3503.0   V47BY       16-Nov-1999 0559Z  QSX 3506.4                 V4 <VE2ZP>
14202.0  CU3EYS      16-Nov-1999 0555Z                             CU <K7WT>
14084.3  ZD7MY       16-Nov-1999 0555Z  RTTY. 569 in Toronto,On   ZD7 <VA3NA>
14240.0  V31JP       16-Nov-1999 0554Z                             V3 <KA8ZPE>
144200.0  WB9TFS     14-Nov-1999 0508Z  EM72 > EM55                 K <N4LGY>
144200.0  W4KPA      14-Nov-1999 0446Z  EM73 > EM55                 K <N4LGY>
144209.0  W1REZ      13-Nov-1999 2208Z  fn55 peaking 330 degs       K <KU2A>
144205.0  W8KX       13-Nov-1999 2111Z  en72>fn43                   K <WA1T>
144219.7  K3KYR      13-Nov-1999 2110Z  FN24 aurora qtf 330         K <KU2A> }

VAR DateString: Str40;
    Result: INTEGER;
    TempFrequency: REAL;

    BEGIN
    ShowDXResponse := False;

    IF StringHas (InputString, 'SH/DX') THEN Exit;

    WITH DXSpot DO
        BEGIN
        PacketString    := InputString;

        FrequencyString := RemoveFirstString (InputString);
        Call            := RemoveFirstString (InputString);
        DateString      := RemoveFirstString (InputString);
        TimeString      := RemoveFirstString (InputString);

        SourceCall      := RemoveLastString (InputString);

        Notes := InputString;

        IF NOT GoodCallSyntax (Call) THEN Exit;

        IF NOT StringHas (FrequencyString, '.') THEN Exit;

        Val (FrequencyString, TempFrequency, Result);
        IF Result <> 0 THEN Exit;

        { Look at the date and time syntax }

        IF NOT StringHas (DateString, '-')      THEN Exit;

        IF NOT StringHas (TimeString, 'Z')      THEN Exit;
        IF NOT Length (TimeString) = 5 THEN Exit;
        IF NOT StringIsAllNumbers (Copy (TimeString, 1, 4)) THEN Exit;

        { Convert the frequency }

        IF TempFrequency < MaxLongInt DIV 9 THEN
            TempFrequency := TempFrequency * 1000;    { Convert kHz to Hz }

        { See if more than 2,000,000,000 Hz or 2 GHz }

        IF (TempFrequency > 2000000000) OR (TempFrequency < 0) THEN
            Exit;

        Frequency := Round (TempFrequency);

        Mode := ActiveMode;  { In case on 160 }

        CalculateBandMode (Frequency, Band, Mode);

        IF Band = NoBand THEN Exit;
        IF Mode = NoMode THEN Exit;

        QSXFrequency := LookForQSXFrequency (Frequency, Notes);

        { Get rid of <>'s around source call }

        IF Copy (SourceCall, 1, 1) = '<' THEN Delete (SourceCall, 1, 1);

        IF Copy (SourceCall, Length (SourceCall), 1) = '>' THEN
            Delete (SourceCall, Length (SourceCall), 1);

        SpotMode := SHDXSpot;

        ShowDXResponse := True;
        END;

    END;



FUNCTION OH2AQFileInputSpot (InputString: STRING; VAR DXSpot: DXSpotType): BOOLEAN;

{
JG3UVN    21420.5 IH9P        33                            0922 31 Oct 1999
MI0BTM    28435.0 TA2KB                                     0934 31 Oct 1999
RX3DCX    14296.5 R3K                                       1025 31 Oct 1999
2E1GOR    28377.4 UA1QV                                     1027 31 Oct 1999
DL9YAJ    21227.7 4U1VIC                                    1029 31 Oct 1999
CT2CVE    28614.5 OK1ARI      contest                       1030 31 Oct 1999
}

VAR DateString: Str40;
    Result: INTEGER;
    TempFrequency: REAL;

    BEGIN
    OH2AQFileInputSpot := False;

    WITH DXSpot DO
        BEGIN
        PacketString := InputString;

        SourceCall      := RemoveFirstString (InputString);
        FrequencyString := RemoveFirstString (InputString);
        Call            := RemoveFirstString (InputString);

        { Get rid of date }

        RemoveLastString (InputSTring);
        RemoveLastString (InputSTring);
        RemoveLastString (InputSTring);

        TimeString := RemoveLastString (InputSTring) + 'Z';

        Notes := InputSTring;

        IF NOT GoodCallSyntax (Call) THEN Exit;

        IF NOT StringHas (FrequencyString, '.') THEN Exit;

        Val (FrequencyString, TempFrequency, Result);
        IF Result <> 0 THEN Exit;

        { Look at the date and time syntax }

        IF NOT StringHas (TimeString, 'Z')      THEN Exit;
        IF NOT Length (TimeString) = 5 THEN Exit;
        IF NOT StringIsAllNumbers (Copy (TimeString, 1, 4)) THEN Exit;

        { Convert the frequency }

        IF TempFrequency < MaxLongInt DIV 9 THEN
            TempFrequency := TempFrequency * 1000;    { Convert kHz to Hz }

        { See if more than 2,000,000,000 Hz or 2 GHz }

        IF (TempFrequency > 2000000000) OR (TempFrequency < 0) THEN
            Exit;

        Frequency := Round (TempFrequency);

        Mode := ActiveMode;  { In case on 160 }

        CalculateBandMode (Frequency, Band, Mode);

        IF Band = NoBand THEN Exit;
        IF Mode = NoMode THEN Exit;

        QSXFrequency := LookForQSXFrequency (Frequency, Notes);

        { Get rid of <>'s around source call }

        IF Copy (SourceCall, 1, 1) = '<' THEN Delete (SourceCall, 1, 1);

        IF Copy (SourceCall, Length (SourceCall), 1) = '>' THEN
            Delete (SourceCall, Length (SourceCall), 1);

        SpotMode := SHDXSpot;

        OH2AQFileInputSpot := True;
        END;

    END;



FUNCTION PacketFileInputSpot (InputString: STRING; VAR DXSpot: DXSpotType): BOOLEAN;

{
7002.0   EL2WW       01-Oct-1999 0001Z  workable                   EL <W7IUV>
28480.1  VK1TX       01-Oct-1999 0001Z  TEX                        VK <W8KVU>
7066.4   9H0VRZ      01-Oct-1999 0002Z                             9H <9K2HN>
}

VAR DateString: Str40;
    Result: INTEGER;
    TempFrequency: REAL;

    BEGIN
    PacketFileInputSpot := False;

    WITH DXSpot DO
        BEGIN
        PacketString := InputString;

        FrequencyString := RemoveFirstString (InputString);
        Call            := RemoveFirstString (InputString);

        { Get rid of date }

        RemoveFirstString (InputSTring);

        TimeString := RemoveFirstString (InputSTring);

        SourceCall := RemoveLastString (InputString);

        Notes := InputSTring;

        IF NOT GoodCallSyntax (Call) THEN Exit;

        IF NOT StringHas (FrequencyString, '.') THEN Exit;

        Val (FrequencyString, TempFrequency, Result);
        IF Result <> 0 THEN Exit;

        { Look at the date and time syntax }

        IF NOT StringHas (TimeString, 'Z')      THEN Exit;
        IF NOT Length (TimeString) = 5 THEN Exit;
        IF NOT StringIsAllNumbers (Copy (TimeString, 1, 4)) THEN Exit;

        { Convert the frequency }

        IF TempFrequency < MaxLongInt DIV 9 THEN
            TempFrequency := TempFrequency * 1000;    { Convert kHz to Hz }

        { See if more than 2,000,000,000 Hz or 2 GHz }

        IF (TempFrequency > 2000000000) OR (TempFrequency < 0) THEN
            Exit;

        Frequency := Round (TempFrequency);

        Mode := ActiveMode;  { In case on 160 }

        CalculateBandMode (Frequency, Band, Mode);

        IF Band = NoBand THEN Exit;
        IF Mode = NoMode THEN Exit;

        QSXFrequency := LookForQSXFrequency (Frequency, Notes);

        { Get rid of <>'s around source call }

        IF Copy (SourceCall, 1, 1) = '<' THEN Delete (SourceCall, 1, 1);

        IF Copy (SourceCall, Length (SourceCall), 1) = '>' THEN
            Delete (SourceCall, Length (SourceCall), 1);

        SpotMode := SHDXSpot;

        PacketFileInputSpot := True;
        END;

    END;



PROCEDURE PacketObject.AnalyzePacketString (PacketString: STRING);

{ This procedure will look at the packet message and try to determine what
  kind of message it is, and route it to the right place.  Note that the
  PacketDisplay during control-B and data the PacketDisplayBuffer is
  handled by a different routine.

  It can identify a DX announcmenet, a talk message sent to MyCall or ALL,
  and responses to a SH/DX command.}

VAR DXSpot: DXSpotType;
    Message: STRING;

    BEGIN
//    WHILE Pos (CarriageReturn, PacketString) > 0 DO
//        Delete (PacketString, Pos (CarriageReturn, PacketString), 1);

    WHILE Pos (LineFeed, PacketString) > 0 DO
        Delete (PacketString, Pos (LineFeed, PacketString), 1);

    GetRidOfPrecedingSpaces (PacketString);

    Message := UpperCase (PacketString);

    IF FoundDXSpot (PacketString, DXSpot) OR ShowDXResponse (PacketString, DXSpot) THEN
        BEGIN
        ProcessPacketSpot (DXSpot);
        Exit;
        END;

    { Might this be a talk or an announce message that I need to display? }

    IF TalkOrAnnounceMessage (PacketString) THEN
        BEGIN
        IF PacketBeep THEN Tone.DoABeep (Single);

        IF Length (PacketString) > 72 THEN
            QuickDisplay (Copy (PacketString, 1, 72))
        ELSE
            QuickDisplay (PacketString);

        Exit;
        END;
    END;



PROCEDURE PacketObject.CheckPacketBuffer (DisplayNewData: BOOLEAN);

{ This procedure examines the PacketBuffer to see if there is any new
  data there.

  If new data is found, and the DisplayNewData flag is TRUE, the data
  will be displayed.  This is used if the Control-B packet window is up.

  Any complete lines found will be pushed into the PacketDisplayBuffer
  and also sent to the network it is enabled.

  Any remainig characters from incomplete lines will be saved in the
  global string PacketDisplayLine.  This string should be displayed
  whenever painting a new Control-B window - since the data won't be
  in the PacketDisplayBuffer (yet).

  Finally, all new data will be appended to the global string
  PacketMessage.  This is what gets looked at for new commands coming
  in from packet.  }

VAR TempString: STRING;
    PacketByte: BYTE;

    BEGIN
    IF PacketReceiveCharBuffer.IsEmpty THEN Exit;  { No new characters }

    { Suck characters from PacketReceiveBuffer into TempString. }

    TempString := '';

    WHILE PacketReceiveCharBuffer.GetNextByte (PacketByte) DO
        BEGIN
        if ((Chr(PacketByte) <> CarriageReturn) and
            (Chr(PacketByte) <> ControlG)) then
        begin
           TempString := TempString + Chr (PacketByte);
           IF Chr(PacketByte) = LineFeed THEN Break;
           IF Length (TempString) = 255 THEN Break;      { The rest can wait }
        end;
        END;

    { If the Control-B window is up, then we will be told to write this
      data so it appears in real time on the screen (instead of waiting
      until the line is complete.  }

    IF DisplayNewData THEN Write (TempString);

    { Append data to PacketDisplayLine }

    if ((length(packetdisplayline)+length(tempstring)) <256) then
       PacketDisplayLine := PacketDisplayLine + TempString
    else if length(tempstring) = 255 then
       PacketDisplayLine := tempstring
    else
       PacketDisplayLine := copy(packetdisplayline,length(packetdisplayline)
          +length(tempstring)-254,255-length(tempstring))+tempstring;

    { See if we have a new line that can be pushed into the DisplayBuffer.
      If the MultiNetwork is up and packet data is enabled, this routine
      will also send the new complete line out on the network.     }

    IF (Length(PacketDisplayLine) = 1)
        and (copy(PacketDisplayLine,1,1) = linefeed) then
       PacketDisplayLine := ''
    else
       CheckPacketDisplayLine (PacketDisplayLine);

    { Update the PacketMessage global with this data }

    if ((length(packetmessage)+length(tempstring)) <256) then
       PacketMessage := PacketMessage + TempString
    else if length(tempstring) = 255 then
       PacketMessage := tempstring
    else
       PacketMessage := copy(packetmessage,length(packetmessage)
          +length(tempstring)-254,255-length(tempstring))+tempstring;
       

    END;


PROCEDURE PacketObject.CheckPacketDisplayLine (VAR DisplayLine: STRING);

{ Looks at the string passed to it to see if a complete line (terminated
  with a line feed) is present.  If so, it removes it from the string
  and puts into the PacketDisplayBuffer.

  It also sends the completed line to the multi network if it is enabled.

  The PacketDisplayBuffer [0..12] of String80 is an indexed rotating
  buffer defined by PacketDisplayBufferStart and PacketDisplayBufferEnd.
  Start is the first line displayed and end is the last line displayed. }

VAR TempString: STRING;
    LineWidth: INTEGER;
    FileWrite: TEXT;

    BEGIN

       LineWidth := Length(DisplayLine);
       IF copy(DisplayLine,Linewidth,1) <> LineFeed THEN EXIT;
       Delete(DisplayLine,LineWidth,1);
       TempString := DisplayLine;

{ Send this message to the network if appropriate }

       IF BroadcastAllPacketData AND (ActivePacketPort <> nil)
          AND (ActiveMultiPort <> nil) THEN
          SendMultiCommand (MultiBandAddressArray [ActiveBand], $FF,
                            MultiPacketReceivedMessage, TempString);

{ Save this message to the packet log file if there is one }

       IF PacketLogFileName <> '' THEN
          IF OpenFileForAppend (FileWrite, PacketLogFileName) THEN
          BEGIN
             WriteLn (FileWrite, TempString);
             Close (FileWrite);
          END;

{ Put the message into the packet display buffer }
       WHILE Length(TempString) > 0 DO
       BEGIN
          IF Length(TempString) > 80 THEN
          BEGIN
             PacketDisplayBuffer [PacketDisplayBufferEnd] :=
                Copy(TempString,1,80);
             Delete(TempString,1,80);
          END
          ELSE
          BEGIN
             PacketDisplayBuffer [PacketDisplayBufferEnd] := TempString;
             TempString := '';
          END;

          Inc (PacketDisplayBufferEnd);
          IF PacketDisplayBufferEnd >= 13 THEN PacketDisplayBufferEnd := 0;
          IF PacketDisplayBufferStart = PacketDisplayBufferEnd THEN
          BEGIN
             Inc (PacketDisplayBufferStart);
             IF PacketDisplayBufferStart >= 13 THEN
                 PacketDisplayBufferStart := 0;
          END;
       END;

       DisplayLine := '';

    END;


PROCEDURE PacketObject.Init;

    BEGIN
    PacketDisplayLine           := '';
    PacketMessage               := '';
    PacketMessageFromNetwork    := '';
    PacketMemoryStart           := 0;
    PacketMemoryEnd             := 0;
    PacketDisplayBufferStart    := 0;
    PacketDisplayBufferEnd      := 0;
    PacketInputFileName         := '';
    PacketInputFileOpen         := False;
    PacketInputFileDelay        := 1;
    END;



PROCEDURE PacketObject.ProcessPacketMessageFromNetWork (MessageString: STRING);

{ This routine will handle a packet message that came in over the multi
  network }

    BEGIN
    { If we are hooked up to the TNC, we can ignore it }

    IF ActivePacketPort <> nil THEN Exit;

    { If the ControlB window is up - display the message as it comes in }

    IF PacketWindowUp THEN WriteLn (MessageString);

    { We feed the message to both PacketMessage and PacketDisplayLine for
      processing. }

    PacketMessage := MessageString + LineFeed;
    PacketDisplayLine := PacketMessage;

    CheckPacketMessage;
    CheckPacketDisplayLine (PacketDisplayLine);
    END;



PROCEDURE PacketObject.ProcessPacketSpot (DXSpot: DXSpotType);

{ Processes a packet spot.  Puts it in the band map (if enabled).  Pushes
  it onto the packet spot buffer. }

VAR MultString: Str20;
    Dupe, Mult: BOOLEAN;

    BEGIN
    WITH DXSpot DO
        BEGIN
        IF (Band = Band160) AND StringHas (MyCall, 'N6TR') THEN
            CheckForNewCountryForTreeOn160 (Call);

        { We ignore dupes, except to put on the band map if enabled.  We
          don't send this around to the network because they will get the
          information themselves from the packet spot. }

        IF VisibleLog.CallIsADupe (Call, Band, Mode) THEN
            BEGIN
            IF BandMapEnable THEN                              {Dupe, Mult}                 {Send to mult}
                NewBandMapEntry (Call, Frequency, QSXFrequency, Mode, True, False, BandMapDecayTime, False);
            Exit;
            END;

        { Call is not a dupe }

        IF BandMapEnable OR (PacketSpots = MultSpots) THEN
            BEGIN
            VisibleLog.DetermineIfNewMult (Call, Band, Mode, MultString);
            Mult := MultString <> '';
            END;

        IF BandMapEnable THEN
            NewBandMapEntry (Call, Frequency, QSXFrequency, Mode, False, Mult, BandMapDecayTime, False);

        IF (NOT PacketBandSpots) OR (Band = ActiveBand) THEN
            IF (PacketSpots <> MultSpots) OR Mult THEN
                PushPacketSpot (DXSpot);
        END;
    END;



PROCEDURE PacketObject.PushPacketSpot (DXSpot: DXSpotType);

{ Takes the spot information and saves it for Control-U. }

VAR TempString: Str80;

    BEGIN
    WITH DXSpot DO
        IF SpotMode = NormalSpot THEN
            BEGIN
            IF PacketBeep THEN Tone.DoABeep (Single);

            QuickDisplay (SourceCall + ' says ' + Call + ' is on ' + FrequencyString + '  ' + Notes);
            ReminderPostedCount := 60;
            END;

    PushPacketMemory (DXSpot);
    END;



PROCEDURE PacketObject.PushPacketMemory (DXSpot: DXSpotType);

VAR Address: INTEGER;

    BEGIN
    WITH DXSpot DO
        IF SpotMode = NormalSpot THEN  { Normal spot - put at the start }

            BEGIN
            Dec (PacketMemoryStart);

            IF PacketMemoryStart < 0 THEN
                PacketMemoryStart := PacketSpotListLength;

            PacketMemory [PacketMemoryStart].Call := Call;
            PacketMemory [PacketMemoryStart].Frequency    := Frequency;
            PacketMemory [PacketMemoryStart].QSXFrequency := QSXFrequency;
            PacketMemory [PacketMemoryStart].Time := TimeString;

            IF PacketMemoryStart = PacketMemoryEnd THEN
                BEGIN
                Dec (PacketMemoryEnd);

                IF PacketMemoryEnd < 0 THEN
                    PacketMemoryEnd := PacketSpotListLength;
                END;
            END

        ELSE  { SH/DX response = put it at the end }

            BEGIN
            PacketMemory [PacketMemoryEnd].Call := Call;
            PacketMemory [PacketMemoryEnd].Frequency := Frequency;
            PacketMemory [PacketMemoryEnd].QSXFrequency := QSXFrequency;
            PacketMemory [PacketMemoryEnd].Time := TimeString;
            Inc (PacketMemoryEnd);

            IF PacketMemoryEnd > PacketSpotListLength THEN
                PacketMemoryEnd := 0;

            IF PacketMemoryStart = PacketMemoryEnd THEN
                BEGIN
                Inc (PacketMemoryStart);

                IF PacketMemoryStart > PacketSpotListLength THEN
                    PacketMemoryStart := 0;
                END;
            END;
    END;



PROCEDURE PacketObject.DeletePacketSpot (SpotAddress: INTEGER);

VAR SourceAddress: INTEGER;

    BEGIN
    IF SpotAddress = PacketMemoryStart THEN
        BEGIN
        Inc (PacketMemoryStart);

        IF PacketMemoryStart > PacketSpotListLength THEN
            PacketMemoryStart := 0;
        Exit;
        END;

    { This loop gets stuck!! }

    REPEAT
        SourceAddress := SpotAddress + 1;             { Point to next cell }

        IF SourceAddress > PacketSpotListLength THEN  { See if we looped }
            SourceAddress := 0;

        IF SourceAddress = PacketMemoryEnd THEN       { We are done? }
            BEGIN
            Dec (PacketMemoryEnd);

            IF PacketMemoryEnd < 0 THEN               { Back end up by one }
                PacketMemoryEnd := PacketSpotListLength;

            Exit;
            END;

        PacketMemory [SpotAddress] := PacketMemory [SourceAddress];

        Inc (SpotAddress);

        IF SpotAddress > PacketSpotListLength THEN
            SpotAddress := 0;

    UNTIL False;
    END;



PROCEDURE PacketObject.DeletePacketEntry (DeleteCall: CallString;
                             DeleteBand: BandType;
                             DeleteMode: ModeType);

VAR PacketAddress, Spot: INTEGER;
    Band: BandType;
    Mode: ModeType;

    BEGIN
    FOR Spot := 1 TO PacketSpotListLength DO
        BEGIN
        PacketAddress := PacketMemoryStart + Spot - 1;

        IF PacketAddress > PacketSpotListLength THEN
            PacketAddress := PacketAddress - (PacketSpotListLength + 1);

        IF PacketAddress = PacketMemoryEnd THEN Exit;

        WITH PacketMemory [PacketAddress] DO
            IF (Call = DeleteCall) THEN
                BEGIN
                Mode := ActiveMode;

                CalculateBandMode (Frequency, Band, Mode);

                IF (Band = DeleteBand) AND (Mode = DeleteMode) THEN
                    BEGIN
                    DeletePacketSpot (PacketAddress);
                    Exit;
                    END;
                END;
        END;
    END;



PROCEDURE PacketObject.DisplayPacketDisplayBuffer;

VAR Address: INTEGER;

    BEGIN
    Address := PacketDisplayBufferStart;

    WHILE Address <> PacketDisplayBufferEnd DO
        BEGIN
        WriteLn (PacketDisplayBuffer [Address]);

        Inc (Address);
        IF Address >= 13 THEN Address := 0;
        END;

    END;



PROCEDURE PacketObject.CheckPacketMessage;

    BEGIN
//    WHILE StringHas (PacketMessage, LineFeed) DO
    WHILE Pos(LineFeed,PacketMessage) <> 0 DO
        BEGIN
        AnalyzePacketString (PrecedingString (PacketMessage, LineFeed));

        { Remove the data we have processed }

        PacketMessage := PostcedingString (PacketMessage, LineFeed);

        WHILE (Copy (PacketMessage, 1, 1) = CarriageReturn) OR
              (Copy (PacketMessage, 1, 1) = LineFeed) DO
                  Delete (PacketMessage, 1, 1);
        END;
    END;



PROCEDURE PacketObject.CheckPacket;

{ This is the routine that needs to be called often by the program so
  the packet stuff all works.   }

VAR PacketInputString: STRING;
    DXSpot: DXSpotType;

    BEGIN
    IF PacketInputFileName <> '' THEN
        BEGIN
        IF NOT PacketInputFileOpen THEN
            BEGIN
            PacketInputFileOpen := OpenFileForRead (PacketInputFileRead, PacketInputFileName);
            MarkTime (StartTime);
            END;

        IF PacketInputFileOpen THEN
            BEGIN
            IF ElaspedSec100 (StartTime) >= PacketInputFileDelay THEN
                BEGIN
                ReadLn (PacketInputFileRead, PacketInputString);

                IF OH2AQFileInputSpot (PacketInputString, DXSpot) THEN
                    BEGIN
                    QuickDisplay (Copy (DXSpot.PacketString, 1, 70));
                    ProcessPacketSpot (DXSpot);
                    END;

                IF PacketFileInputSpot (PacketInputString, DXSpot) THEN
                    BEGIN
                    QuickDisplay (Copy (DXSpot.PacketString, 1, 70));
                    ProcessPacketSpot (DXSpot);

                    IF BroadcastAllPacketData AND (ActivePacketPort <> nil) AND (ActiveMultiPort <> nil) THEN
                        SendMultiCommand (MultiBandAddressArray [ActiveBand], $FF,
                        MultiPacketReceivedMessage, DXSpot.PacketString);
                    END;


                IF Eof (PacketInputFileRead) THEN
                    BEGIN
                    Close (PacketInputFileRead);
                    PacketInputFileName := '';
                    END;

                MarkTime (StartTime);
                END;
            END;
        END;

    IF ActivePacketPort <> nil THEN   { Added in 6.25 }
        BEGIN
        CheckPacketBuffer (False);
        CheckPacketMessage;
        END;
    END;



    BEGIN
    PacketWindowUp := False;
    Packet.Init;
    END.
