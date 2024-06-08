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

UNIT LogCfg;

{$O+}

INTERFACE

USES trcrt, LogStuff, LogSCP, LogCW, LogWind, LogDupe, ZoneCont,
     LogGrid, LogDom, FContest, Country9, LogEdit, LogDDX,
     LogWAE, LogHP, LogPack, LogK1EA, DOS, LogHelp, LogProm, CfgCmd,
     SlowTree, Tree, LogMenu, K1EANet,communication,linuxsound,N4OGW, N1MM, logqsonr;


    FUNCTION  LoadInSeparateConfigFile (FileName: STRING;
                                        VAR FirstCommand: BOOLEAN;
                                        Call: CallString): BOOLEAN;

    PROCEDURE LookForCommands;
    PROCEDURE ReadInConfigFile (Call: CallString);

{$IFDEF TRFree}

CONST Version = '1.06 - Freeware version based on TR Log 6.69';

{$ELSE}

//CONST Version = 'Linux 0.02'; //based on 6.76
{$I versions.inc} // put versions here for easy update

{$ENDIF}

var keyerdebug: boolean;
    nostdcfg: boolean;

IMPLEMENTATION
uses keycode,foot,radio,rig,kenwood,icom,rigctld,timer;

{$I CfgDef}
{$I ColorCfg}
{$I postcfg}



PROCEDURE PushLogFiles (var LastPushedLogName: Str20);

{ This procedure will take the current active log file and create a
  backup file with the filename PLOG###.BAK.  ## is intially 01, and
  then increments each time.  The active log file is removed. }

VAR FileNumber: INTEGER;
    TempString: Str20;

    BEGIN
    FileNumber := 0;

    REPEAT
        Str (FileNumber, TempString);
        WHILE Length (TempString) < 3 DO TempString := '0' + TempString;

        TempString := 'PLOG' + Tempstring + '.BAK';

        IF NOT FileExists (TempString) THEN
            BEGIN
            RenameFile (LogFileName, TempString);

            LastPushedLogName := TempString;
            Exit;
            END;

        Inc (FileNumber);

    UNTIL FileNumber > 1000;

    ReportError ('Unable to create backup file!!');
    Halt;
    END;


FUNCTION ConfigurationOkay: BOOLEAN;

    BEGIN
    WriteLn ('At configuration okay.');

    ConfigurationOkay := False;

    IF MyCall = '' THEN
        BEGIN
        ClrScr;
        ReportError ('No callsign specified!!');
        Exit;
        END;

    IF FloppyFileSaveFrequency > 0 THEN
        IF FloppyFileSaveName = '' THEN
            BEGIN
            ClrScr;
            ReportError ('No floppy file save name specified!!');
            Exit;
            END;

    ConfigurationOkay := True;
    END;



PROCEDURE SetUpGlobalsAndInitialize;

VAR FileWrite: TEXT;
    Minute: INTEGER;

    BEGIN
    WriteLn ('Initializing program..');

    IF (N4OGW_RadioOne_BandMap_IP <> '') AND (N4OGW_RadioOne_BandMap_Port <> 0) THEN
        BEGIN
        N4OGW_RadioOne_BandMap := N4OGW_BandMap_Object.Create;

        N4OGW_RadioOne_BandMap.Init (N4OGW_RadioOne_BandMap_IP,
                                     N4OGW_RadioOne_BandMap_Port,
                                     N4OGW_RadioOne_BandMap_UDP_Port);
        END;

    IF (N4OGW_RadioTwo_BandMap_IP <> '') AND (N4OGW_RadioTwo_BandMap_Port <> 0) THEN
        BEGIN
        N4OGW_RadioTwo_BandMap := N4OGW_BandMap_Object.Create;

        N4OGW_RadioTwo_BandMap.Init (N4OGW_RadioTwo_BandMap_IP,
                                     N4OGW_RadioTwo_BandMap_Port,
                                     N4OGW_RadioTwo_BandMap_UDP_Port);
        END;

    IF N1MM_UDP_Port > 0 THEN N1MM_QSO_Portal.Init;

    IF QTCsEnabled THEN LoadQTCDataFile;

    IF DomesticQTHDataFileName <> '' THEN
        IF NOT DomQTHTable.LoadInDomQTHFile (DomesticQTHDataFileName) THEN
            BEGIN
            ReportError ('Unable to find ' + DomesticQTHDataFileName + '!!');
            Halt;
            END;

    IF (BandMapEnable AND DisplayBandMapEnable) OR (ActiveRTTYPort = nil) OR NetDebug OR
       VisibleDupesheetEnable OR
       ((GridMapCenter <> '') AND (ActiveDomesticMult = GridSquares)) THEN
           IF VGADisplayEnable THEN
               TextMode (C80 + Font8X8);   { Select 42 or 50 line mode }

    WriteLn ('Initializing program...');

    ScreenHeight := Hi (WindMax) + 1;
    BandMapWindowRY := ScreenHeight;

    SuperDupeSheetWindowRY := BandMapWindowRY;

    SuperDupeSheet := (BandMapWindowRY >= 40) AND (NOT BandMapEnable);

    IF BandMapEnable AND (BandMapWindowRY < 40) THEN
        BEGIN
        ReportError ('Your display will not support the band map!!');
        BandMapEnable := False;
        END;

    IF (ActiveRTTYPort <> nil) AND (BandMapWindowRY < 40) THEN
        BEGIN
        ReportError ('Your display will not support RTTY display!!');
        ActiveRTTYPort := nil;
        END;

    IF DoingRescore THEN   {KK1L: 6.71}{KK1L: 6.73 Moved here from end of proc. Now works to force DVPEnable=FALSE}
        BEGIN
        CodeSpeed := 99;
        CWTone := 0;
        AutoDupeEnableCQ := False;
        AutoDupeEnableSAndP := False;
        PollRadioOne := FALSE; {KK1L: 6.73}
        PollRadioTwo := FALSE; {KK1L: 6.73}
        END;

    BandMemory [RadioOne] := ActiveBand;
    BandMemory [RadioTwo] := ActiveBand;
    ModeMemory [RadioOne] := ActiveMode;
    ModeMemory [RadioTwo] := ActiveMode;

    ActiveRadio := RadioOne;
    InactiveRadio := RadioTwo; {KK1L: 6.73}

    {RadioOneSpeed := CodeSpeed;}
    {RadioTwoSpeed := CodeSpeed;}
    SpeedMemory[RadioOne] := CodeSpeed; {KK1L: 6.73}
    SpeedMemory[RadioTwo] := CodeSpeed; {KK1L: 6.73}

    TotalQSOPoints := 0;

    IF AutoTimeIncrementQSOs <> 0 THEN IncrementTimeEnable := True;

    DoingDomesticMults := ActiveDomesticMult <> NoDomesticMults;
    DoingDXMults       := ActiveDXMult       <> NoDXMults;
    DoingPrefixMults   := ActivePrefixMult   <> NoPrefixMults;
    DoingZoneMults     := ActiveZoneMult     <> NoZoneMults;

    NumberDifferentMults := 0;

    {KK1L: 6.68 This may need to change to something like...don't know. It works as is.  }
    {IF (DoingDomesticMults)AND                                                          }
    {   ((DomesticQTHDataFileName <> '') OR (ActiveDomesticMult = WYSIWYGDomestic)) THEN }
    IF (DoingDomesticMults) AND
       ((ActiveDomesticMult = DomesticFile) OR (ActiveDomesticMult = WYSIWYGDomestic)) THEN
        IF RemainingMultDisplay = NoRemMultDisplay THEN
             RemainingMultDisplay := Domestic;

    IF DoingDomesticMults THEN
        Inc (NumberDifferentMults);

    IF DoingDXMults THEN
        BEGIN
        Inc (NumberDifferentMults);
        IF RemainingMultDisplay = NoRemMultDisplay THEN
            RemainingMultDisplay := DX;
        END;

    IF DoingZoneMults THEN
        BEGIN
        Inc (NumberDifferentMults);
        IF RemainingMultDisplay = NoRemMultDisplay THEN
            RemainingMultDisplay := Zone;
        END;

    IF DoingPrefixMults THEN Inc (NumberDifferentMults);

    SetUpExchangeInformation (ActiveExchange, ExchangeInformation);

    IF QTCsEnabled THEN LoadQTCDataFile;

    LoadSpecialHelloFile;

    IF DDXState <> Off THEN
        BEGIN
          ActiveKeyer := CPUKeyer; //only CPUKeyer with simulator
          CPUKeyer.SetActiveRadio(NoRadio);
        END;

    TailEnding := False;
    Sheet.SheetInitAndLoad;

    IF BandMapEnable THEN LoadBandMap;

    IF NOT FileExists (LogFileName) THEN
        IF OpenFileForWrite (FileWrite, LogFileName) THEN
            Close (FileWrite)
        ELSE
            Halt;

    SetActiveWindow (WholeScreenWindow);
    TextBackGround (SelectedColors.WholeScreenBackground);
    ClrScr;

    VisibleLog.SetUpEditableLog;

    IF DoingDomesticMults OR DoingDXMults OR DoingZoneMults THEN
        VisibleLog.ShowRemainingMultipliers;

    DisplayContestTitle (ContestTitle);

    DisplayFreeMemory;

    IF ActiveRotatorPort <> nil THEN
        CASE ActiveRotatorType OF
            DCU1Rotator:  ActiveRotatorPort.setparams(4800,8,NoParity,1);
            OrionRotator: ActiveRotatorPort.setparams(9600,8,NoParity,1);
            YaesuRotator: ActiveRotatorPort.setparams(9600,8,NoParity,1);
 {KK1L: 6.71}
            END;

    IF ActiveModemPort <> nil THEN
        BEGIN
        IF (ActiveModemPort = Radio1ControlPort) OR
           (ActiveModemPort = Radio2ControlPort) OR
           (ActiveModemPort = ActiveRotatorPort)   OR
           (ActiveModemPort = ActivePacketPort) THEN
               BEGIN
               ReportError ('Modem serial port assigned to two functions!!!');
               Halt;
               END;

        ActiveModemPort.setparams(ModemPortBaudRate,8,NoParity,2);
        END;

    IF ActivePacketPort <> nil THEN
        BEGIN
        IF Packet.EightBitPacketPort THEN
            ActivePacketPort.setparams(Packet.PacketBaudRate,8,NoParity,2)
        ELSE
            ActivePacketPort.setparams(Packet.PacketBaudRate,7,EvenParity,1);
        END;

    IF ActiveRTTYPort <> nil THEN
        BEGIN
        IF EightBitRttyPort THEN {KK1L: 6.71 Added 8N1 port type for RTTY}
            ActiveRTTYPort.setparams(4800,8,NoParity,2)
 {KK1L: 6.73 Tried 2 stop bits}
        ELSE
            ActiveRTTYPort.setparams(4800,7,EvenParity,1);
        END;

    IF ActiveMultiPort <> nil THEN
        BEGIN
        IF (ActiveMultiPort = Radio1ControlPort) OR
           (ActiveMultiPort = Radio2ControlPort) OR
           (ActiveMultiPort = ActiveRotatorPort)   OR
           (ActiveMultiPort = ActivePacketPort) THEN
               BEGIN
               ReportError ('Multi serial port assigned for two functions!!!');
               Halt;
               END;

        ActiveMultiPort.setparams(MultiPortBaudRate,8,NoParity,2);
        END;

    IF (ActiveMultiPort <> nil) OR (MultiUDPPort > -1) THEN
        New (MultiRememberBuffer);

    IF IntercomFileEnable THEN
        BEGIN
        OpenFileForAppend (IntercomFileWrite, 'INTERCOM.TXT');
        IntercomFileOpen := True;
        END;

    IF Radio1ControlPort <> nil THEN
        BEGIN
        IF (Radio1ControlPort = Radio2ControlPort) OR
           (Radio1ControlPort = ActivePacketPort) THEN
               BEGIN
               ReportError ('Radio 1 serial port assigned for two functions!!!');
               Halt;
               END;

        CASE Radio1Type OF

            RIGCTL: begin
               rig1 := rigctldctl.create(radiodebugmode);
               rig1.setport(radio1controlport);
               if (radiooneresponsetimeout <> 0 ) then
                  rig1.responsetimeout(radiooneresponsetimeout)
               else
                  radiooneresponsetimeout := rig1.getresponsetimeout();
               addtimer(@rig1.timer);
            end;

            IC706, IC706II, IC706IIG, IC707, IC725, IC726, IC728, IC729, IC735,
            IC746, IC746PRO, IC756, IC756PRO, IC756PROII,  IC736, IC737, IC738,
            IC761, IC765, IC775, IC781: begin
               Radio1ControlPort.setparams(Radio1BaudRate,8,NoParity,1);
               rig1 := icomctl.create(radiodebugmode);
               icomctl(rig1).setcivaddress(radio1receiveraddress);
               rig1.setport(radio1controlport);
               if (radiooneresponsetimeout <> 0 ) then
                  rig1.responsetimeout(radiooneresponsetimeout)
               else
                  radiooneresponsetimeout := rig1.getresponsetimeout();
               addtimer(@rig1.timer);
            end;

            TS850, K2, K3, K4: begin
               Radio1ControlPort.setparams(Radio1BaudRate,8,NoParity,2);
               rig1 := kenwoodctl.create(radiodebugmode);
               rig1.setport(radio1controlport);
               if (radiooneresponsetimeout <> 0 ) then
                  rig1.responsetimeout(radiooneresponsetimeout)
               else
                  radiooneresponsetimeout := rig1.getresponsetimeout();
               addtimer(@rig1.timer);
            end;
            ELSE
                Radio1ControlPort.setparams(Radio1BaudRate,8,NoParity,2);
            END;

        END else rig1 := radioctl.create(radiodebugmode); //dummy

    IF Radio2ControlPort <> nil THEN
        BEGIN
        IF (Radio2ControlPort = ActivePacketPort) THEN
            BEGIN
            ReportError ('Radio 2 serial port assigned for two functions!!!');
            Halt;
            END;

        CASE Radio2Type OF
            RIGCTL: begin
               rig2 := rigctldctl.create(radiodebugmode);
               rig2.setport(radio2controlport);
               if (radiotworesponsetimeout <> 0 ) then
                  rig2.responsetimeout(radiotworesponsetimeout)
               else
                  radiotworesponsetimeout := rig2.getresponsetimeout();
               addtimer(@rig2.timer);
            end;

            IC706, IC706II, IC706IIG, IC707, IC725, IC726, IC728, IC729, IC735,
            IC746, IC746PRO, IC756, IC756PRO, IC756PROII,  IC736, IC737, IC738,
            IC761, IC765, IC775, IC781: begin
               Radio2ControlPort.setparams(Radio2BaudRate,8,NoParity,1);
               rig2 := icomctl.create(radiodebugmode);
               icomctl(rig2).setcivaddress(radio2receiveraddress);
               rig2.setport(radio2controlport);
               if (radiotworesponsetimeout <> 0 ) then
                  rig2.responsetimeout(radiotworesponsetimeout)
               else
                  radiotworesponsetimeout := rig2.getresponsetimeout();
               addtimer(@rig2.timer);
            end;

            TS850, K2, K3, K4: begin
               Radio2ControlPort.setparams(Radio2BaudRate,8,NoParity,2);
               rig2 := kenwoodctl.create(radiodebugmode);
               rig2.setport(radio2controlport);
               if (radiotworesponsetimeout <> 0 ) then
                  rig2.responsetimeout(radiotworesponsetimeout)
               else
                  radiotworesponsetimeout := rig2.getresponsetimeout();
               addtimer(@rig2.timer);
            end;
            ELSE
                Radio2ControlPort.setparams(Radio2BaudRate,8,NoParity,2);
            END;
        END else rig2 := radioctl.create(radiodebugmode); //dummy

    rig1.setpolltime(Rig1FreqPollRate);
    rig2.setpolltime(Rig2FreqPollRate);
    rig1.setcwreverse(Radio1CwReverse);
    rig2.setcwreverse(Radio2CwReverse);

    if scorerpt.enabled then
    begin
       scorerpt.setcall(MyCall);
       scorerpt.setup;
       addtimer(@scorerpt.timer);
    end;

    ActiveKeyer.debug(keyerdebug);
    InitializeKeyer;

    IF ActiveRadio = RadioOne THEN
        BEGIN
        ActiveKeyer.SetActiveRadio(RadioOne);
        ActiveMode  := ModeMemory [RadioOne];
        ActiveBand  := BandMemory [RadioOne];
        END
    ELSE
        BEGIN
        ActiveKeyer.SetActiveRadio(RadioTwo);
        ActiveMode  := ModeMemory [RadioTwo];
        ActiveBand  := BandMemory [RadioTwo];
        END;

    ActiveKeyer.SetMonitorTone(CWTone);

    { Speical footswitch mode will be lost with Initlialize }

    IF FootSwitchMode = TBSIQSSB THEN
        ArdKeyer.FootSwitch2BSIQSSB;

    DisplayCodeSpeed (CodeSpeed, CWEnabled, False, ActiveMode);

    {RadioOneSpeed := CodeSpeed;}
    {RadioTwoSpeed := CodeSpeed;}
    SpeedMemory[RadioOne] := CodeSpeed; {KK1L: 6.73}
    SpeedMemory[RadioTwo] := CodeSpeed; {KK1L: 6.73}
    Str (SpeedMemory[RadioOne], SpeedString); {KK1L: 6.73 Initialize SpeedString for ALT-D use.}

    SetSpeed (CodeSpeed);

    NumberContactsThisMinute := 0;
    FOR Minute := 1 TO 10 DO RateMinuteArray [Minute].QSOs   := 0;
    FOR Minute := 1 TO 10 DO RateMinuteArray [Minute].Points := 0;
    BigCursor;
    DisplayInsertMode (InsertMode);
    AltDDupeCheckCall := '';
    DupeInfoCall := '';

    IF AutoSendCharacterCount > 0 THEN
        BEGIN
        AutoSendEnable := True;
        DisplayAutoSendCharacterCount;
        END;

    IF ReadInLog THEN
        BEGIN
        AutoDupeEnableCQ := False;

        IF CWTone = 0 THEN
            BEGIN
            FlushCWBufferAndClearPTT;
            CWEnabled := False;
            END;
        END;

    { Look through the two log files to get the highest QSO numbers sent thus far }

    QNumber.Init;  { Set all QSO numbers to zero }
    QNumber.InitializeQSONumbersFromLogFile (LogFileName);
    QNumber.InitializeQSONumbersFromLogFile (LogTempFileName);
    END;



FUNCTION LoadInSeparateConfigFile (FileName: STRING;
                                   VAR FirstCommand: BOOLEAN;
                                   Call: CallString): BOOLEAN;

VAR ConfigRead: TEXT;
    FileString: STRING;
    LineNumber: INTEGER;

    BEGIN
    LoadInSeparateConfigFile := False;
    LineNumber := 1;

    GetRidOfPrecedingSpaces (FileName);

    IF OpenFileForRead (ConfigRead, FileName) THEN
        BEGIN
        WHILE NOT Eof (ConfigRead) DO
            BEGIN
            ReadLn (ConfigRead, FileString);

            IF StringHas (UpperCase (FileString), 'MY CALL') AND (Call <> '') THEN
                BEGIN
                FirstCommand := False;
                Continue;
                END;

            IF NOT ProcessConfigInstruction (FileString, FirstCommand) THEN
                BEGIN
                WriteLn;
                WriteLn ('INVALID STATEMENT IN ', FileName, '!!  Line ', LineNumber);
                WriteLn (FileString);
                Exit;
                END;

            Inc (LineNumber);
            END;

        Close (ConfigRead);
        LoadInSeparateConfigFile := True;
        END
    ELSE
        BEGIN
        WriteLn;
        WriteLn ('Unable to find file ', FileName, '!!');
        Exit;
        END;
    END;



PROCEDURE ReadInConfigFile (Call: CallString);

{ This procedure will read in the config file which contains the
  initial values for several global variables.  This makes it easier to
  restart the program in case of a power failure. }

VAR FileString: STRING;
    FirstCommand: BOOLEAN;
    LineNumber: INTEGER;
    FileName: String;
    TempQTH: QTHRecord;

    BEGIN
    MyCall := '';

    ClrScr;
    GoToXY (1, 10);
    TextColor (Yellow);
    WriteLnCenter ('N6TR LOGGING PROGRAM');
    WriteLn;
    WriteLnCenter ('Version ' + Version);
//    WriteLn;
//    WriteLnCenter ('Registered to ' + UserNameString);

    GoToXY (1, 20);
    WriteLnCenter ('Initializing program.  Please wait...');
    TextColor (Yellow);

    SetConfigurationDefaultValues;

    IF NOT CountryTable.LoadInCountryFile THEN
        BEGIN
        ReportError ('Unable to find CTY.DAT country file!!');
        WriteLn ('Make sure this file is in the same directory as the program.');
        Halt;
        END;

    IF Call <> '' THEN
        BEGIN
        MyCall := UpperCase (Call);
        LocateCall (MyCall, TempQTH, True);
        MyCountry     := CountryTable.GetCountryID (TempQTH.Country);
        MyContinent   := TempQTH.Continent;
        Str (TempQTH.Zone,  MyZone);
        CountryString   := MyCountry;
        ContinentString := CountryTable.GetContinentName (MyContinent);
        END;

    ClearDomesticCountryList;

    IF NOT FileExists (LogConfigFileName) THEN
        PullLogCfgInformationOutOfThisPerson;

    { Load in standard config file first }

    FirstCommand := False;      { Disable looking for MY CALL command }

    Filename := '';
    IF NOT NoStdcfg then
       FileName := FindDirectory ('STDCFG.DAT') + DirectorySeparator + 'STDCFG.DAT';

    IF FileExists (FileName) THEN
        IF NOT LoadInSeparateConfigFile (FileName, FirstCommand, Call) THEN
            BEGIN
            WriteLn ('Program halted...  Unable to open standard config file.');
            Halt;
            END;

    { Now open the specific config file }

    IF OpenFileForRead (ConfigFileRead, LogConfigFileName) THEN
        BEGIN
        LineNumber := 1;
        FirstCommand := True;

        WHILE NOT Eof (ConfigFileRead) DO
            BEGIN
            ReadLn (ConfigFileRead, FileString);

            IF StringHas (UpperCase (FileString), 'MY CALL') AND (Call <> '') THEN
                BEGIN
                FirstCommand := False;
                Continue;
                END;

            IF (Copy (FileString, 1, 1) = ';') OR
               (Copy (FileString, 1, 2) = ' ;') THEN
                   Continue;

            IF StringHas (FileString, '  ;') THEN
                FileString := PrecedingString (FileString, ';');

            IF StringHas (UpperCase (FileString), 'INPUT CONFIG FILE') THEN
                BEGIN
                FileName := PostcedingString (FileString, '=');

                IF NOT LoadInSeparateConfigFile (FileName,
                                                 FirstCommand,
                                                 Call) THEN
                    BEGIN
                    WriteLn ('Program halted...  Unable to load in ', FileName);
                    Halt;
                    END;
                END
            ELSE
                IF NOT ProcessConfigInstruction (FileString, FirstCommand) THEN
                    BEGIN
                    WriteLn;
                    WriteLn ('INVALID STATEMENT IN CONFIG FILE!!  Line ', LineNumber);
                    WriteLn (FileString);
                    WriteLn ('Program halted...');
                    Halt;
                    END;

            Inc (LineNumber);
            END;

        Close (ConfigFileRead);
        END
    ELSE
        Halt;

    WriteLn ('Your call is ', MyCall, '  Your country is ', MyCountry);

    CASE MyContinent OF
        NorthAmerica:     WriteLn ('Your continent is North America.');
        SouthAmerica:     WriteLn ('Your continent is South America.');
        Europe:           WriteLn ('Your continent is Europe.');
        Africa:           WriteLn ('Your continent is Africa.');
        Asia:             WriteLn ('Your continent is Asia.');
        Oceania:          WriteLn ('Your continent is Oceania.');
        UnknownContinent: WriteLn ('Your continent is unknown.');
        END;

    TextMode (LastMode);

    IF NOT ConfigurationOkay THEN
        BEGIN
        WriteLn ('Program halted...');
        Halt;
        END;

    SetUpGlobalsAndInitialize;
    END;



PROCEDURE LookForCommands;

VAR ParameterCount: INTEGER;
    LastPushedLogName: Str20; {KK1L: 6.71}
    TempString: Str40;
    Key: CHAR;

    BEGIN
    PacketFile := False;

//    FOR ParameterCount := 1 TO ParamCount DO

    ParameterCount := 0;
    while ((ParameterCount + 1) <= ParamCount) do
        BEGIN
        inc(ParameterCount);
        IF UpperCase (ParamStr (ParameterCount)) = 'B64DECODE' THEN
            BEGIN
            Bin64Decode;
            Halt;
            END;

        IF UpperCase (ParamStr (ParameterCount)) = 'BANDMAP' THEN
            FakeBandMap := True;

        IF UpperCase (ParamStr (ParameterCount)) = 'COAX' THEN
            BEGIN
            CoaxLength;
            Halt;
            END;

        IF UpperCase (ParamStr (ParameterCount)) = 'DEBUG' THEN Debug := True;

        IF UpperCase (ParamStr (ParameterCount)) = 'DISTANCE' THEN
            BEGIN
            ComputeGridDistance;
            Halt;
            END;

        IF UpperCase (ParamStr (ParameterCount)) = 'FINDFILE' THEN
            BEGIN
            WriteLn (FindDirectory (ParamStr (ParameterCount + 1)));
            Halt;
            END;

        IF UpperCase (ParamStr (ParameterCount)) = 'FOOTSWITCHDEBUG' THEN
            FootSwitchDebug := True;

        IF UpperCase (ParamStr (ParameterCount)) = 'GRID' THEN
            BEGIN
            TellMeMyGrid;
            Halt;
            END;

        IF UpperCase (ParamStr (ParameterCount)) = 'TCPTEST' THEN
            BEGIN
            TcpTest;
            Halt;
            END;

        IF UpperCase (ParamStr (ParameterCount)) = 'HP' THEN
            BEGIN
            HP;
            Halt;
            END;

        IF UpperCase (ParamStr (ParameterCount)) = 'HEXDUMP' THEN
            BEGIN
            HexDump;
            Halt;
            END;

        IF UpperCase (ParamStr (ParameterCount)) = 'HEXCONVERT' THEN
            BEGIN
            HexConvert;
            Halt;
            END;

        IF (UpperCase (ParamStr (ParameterCount)) = 'HELP') OR
           (UpperCase (ParamStr (ParameterCount)) = '?') THEN
               BEGIN
               StartUpHelp;
               Halt;
               END;

        IF UpperCase (ParamStr (ParameterCount)) = 'KC' THEN
            BEGIN
            ShowKeyCodes;
            Halt;
            END;

        IF UpperCase (ParamStr (ParameterCount)) = 'LC' THEN
            BEGIN
            Inductance;
            Halt;
            END;

        IF UpperCase (ParamStr (ParameterCount)) = 'LOOPBACK' THEN
            BEGIN
            LoopBackTest;
            Halt;
            END;

        IF UpperCase (ParamStr (ParameterCount)) = 'UNIXTIME' THEN
            BEGIN
            TestUnixTimeConversionRoutines;
            Halt;
            END;

        IF UpperCase (ParamStr (ParameterCount)) = 'NETDEBUG' THEN NetDebug := True;
        IF UpperCase (ParamStr (ParameterCount)) = 'NOSTDCFG' THEN NoStdcfg := True;

        IF UpperCase (ParamStr (ParameterCount)) = 'PACKET' THEN
            FakePacket := True;

        IF UpperCase (ParamStr (ParameterCount)) = 'PACKETFILE' THEN
            BEGIN
            WriteLn ('Opening ', ParamStr (ParameterCount + 1), ' as a packet file to process.');

            IF FileExists (ParamStr (ParameterCount + 1)) THEN
                BEGIN
                WriteLn ('Found file');
                Assign (PacketFileRead, ParamStr (ParameterCount + 1));
                Reset  (PacketFileRead, 1);
                PacketFile := True;
                END;
            END;

        IF UpperCase (ParamStr (ParameterCount)) = 'PACKETINPUTFILE' THEN
            BEGIN
            Packet.PacketInputFileName := ParamStr (ParameterCount + 1);

            IF StringIsAllNumbers (ParamStr (ParameterCount + 2)) THEN
                BEGIN
                TempString := ParamStr (ParameterCount + 2);
                Val (TempString, PacketInputFileDelay);
                END;
            END;

        IF UpperCase (ParamStr (ParameterCount)) = 'PACKETMESS' THEN
            PacketMess;

        IF (UpperCase (ParamStr (ParameterCount)) = 'PORT') OR
           (UpperCase (ParamStr (ParameterCount)) = 'PORTS') THEN
               BEGIN
               ShowIOPorts;
               Halt;
               END;

        IF UpperCase (ParamStr (ParameterCount)) = 'PORTTOFILE' THEN
            BEGIN
            PortToFile;
            Halt;
            END;

        IF UpperCase (ParamStr (ParameterCount)) = 'PACKETSIMULATE' THEN
            BEGIN
            PacketSimulate;
            Halt;
            END;

        IF UpperCase (ParamStr (ParameterCount)) = 'PASSTHROUGH' THEN
            BEGIN
            PassThrough;
            Halt;
            END;

        IF UpperCase (ParamStr (ParameterCount)) = 'RADIODEBUG' THEN RadioDebugMode := True;

        IF UpperCase (ParamStr (ParameterCount)) = 'KEYERDEBUG' THEN
           keyerdebug := True;

        IF UpperCase (ParamStr (ParameterCount)) = 'READ'  THEN
            BEGIN
            ReadInLog := True;
            ReadInLogFileName := ParamStr (ParameterCount + 1);
            Inc (ParameterCount);

            REPEAT
                Key := UpCase (GetKey ('Do you really want to read in ' + ReadInLogFilename + '? (Y/N) : '));
                IF Key = EscapeKey THEN Halt;
            UNTIL (Key = 'Y') OR (Key = 'N');

            IF Key = 'N' THEN
                BEGIN
                ReadInLog := False;
                ReadInLogFileName := '';
                END;
            END;

        {KK1L: 6.71 Added as a multiplier and dupe check}
        IF UpperCase (ParamStr (ParameterCount)) = 'RESCORE'  THEN
            BEGIN
            ReadInLog := True;
            DoingRescore := True;
            TempString := SelectAvailableLog;
            IF TempString <> '' THEN SetUpFileNames (TempString);
            {SetUpFileNames ('LOGCFG');}
            PushLogFiles (LastPushedLogName);
            ReadInLogFileName := LastPushedLogName;
            WriteLn('Ready to rescore ', ReadInLogFileName, '!');
            END;


        IF UpperCase (ParamStr (ParameterCount)) = 'SUN' THEN
            BEGIN
            SunriseSunset;
            Halt;
            END;

        IF UpperCase (ParamStr (ParameterCount)) = 'TALKDEBUG' THEN TalkDebugMode := True;

        IF UpperCase (ParamStr (ParameterCount)) = 'TRACE' THEN
            Trace := True;

        IF UpperCase (ParamStr (ParameterCount)) = 'UDPSEND' THEN
            BEGIN
            UDPSend;
            Halt;
            END;

        IF UpperCase (ParamStr (ParameterCount)) = 'UDPRX' THEN
            BEGIN
            UDPRX;
            Halt;
            END;

        IF UpperCase (ParamStr (ParameterCount)) = 'UUDECODE' THEN
            BEGIN
            UUDecode;
            Halt;
            END;

        IF UpperCase (ParamStr (ParameterCount)) = 'VIEWRADIODEBUG' THEN
            BEGIN
            ViewRadioDebug;
            Halt;
            END;

        END;
    END;




    BEGIN
    RemainingMultDisplayMode := NoRemainingMults;
    RunningConfigFile := False;
    keyerdebug := false;
    nostdcfg := false;
    END.

