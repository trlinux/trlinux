//{$mode objfpc}
UNIT CfgCMD;

{$O+}
{$F+}
{$V-}

INTERFACE

USES SlowTree, Tree, LogStuff, LogSCP, LogCW, LogWind, LogDupe, ZoneCont,
     LogGrid, LogDom, FContest, LogDVP, Country9, LogEdit, LogDDX,
     LogHP, LogWAE, LogPack, LogK1EA, DOS, LogHelp, LogProm, trCrt, K1EANet,
     communication,linuxsound;


    FUNCTION  ProcessConfigInstruction (FileString: STRING; VAR FirstCommand: BOOLEAN): BOOLEAN;

    PROCEDURE SniffOutControlCharacters (VAR TempString: STRING);

VAR ConfigFileRead: TEXT;
    ClearDupeSheetCommandGiven: BOOLEAN;
    RunningConfigFile: BOOLEAN;  { True when using Control-V command }

IMPLEMENTATION
uses keycode,foot,keyers,xkb,so2r;

{$I ColorCfg}
{$I PostCfg}



PROCEDURE SniffOutControlCharacters (VAR TempString: STRING);

VAR NumericString: Str20;
    StringLength, NumericValue, Result: INTEGER;
    Count: Integer;

    BEGIN
    IF TempString = '' THEN Exit;

    StringLength := Length (TempString);

    Count := 1;

    WHILE Count <= Length (TempString) - 3 DO
        BEGIN
        IF (TempString [Count] = '<') AND (TempString [Count + 3] = '>') THEN
            BEGIN
            NumericString := UpperCase (Copy (TempString, Count + 1, 2));

            HexToInteger (NumericString, NumericValue, Result);

            IF Result = 0 THEN
                BEGIN
                Delete (TempString, Count, 4);
                Insert (Chr (NumericValue), TempString, Count);
                END;
            END;

        Inc (Count);
        END;
    END;



FUNCTION ProcessConfigInstructions1 (ID: Str80; CMD: STRING): BOOLEAN;

VAR Result, Speed, TempValue: INTEGER;
    TempLongInt: LONGINT;
    tempstring: string;

    BEGIN
    ProcessConfigInstructions1 := False;

    IF ID = 'ALL CW MESSAGES CHAINABLE' THEN
        BEGIN
        AllCWMessagesChainable := Upcase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'ALT-D BUFFER ENABLE' THEN
        BEGIN
        AltDBufferEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'ALWAYS CALL BLIND CQ' THEN
        BEGIN
        AlwaysCallBlindCQ := Upcase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'ASK FOR FREQUENCIES' THEN
        BEGIN
        AskForFrequencies := Upcase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'ASK IF CONTEST OVER' THEN
        BEGIN
        AskIfContestOver := Upcase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'AUTO CALL TERMINATE' THEN
        BEGIN
        AutoCallTerminate := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'AUTO DISPLAY DUPE QSO' THEN
        BEGIN
        AutoDisplayDupeQSO := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF (ID = 'AUTO DUPE ENABLE') OR (ID = 'AUTO DUPE ENABLE CQ') THEN
        BEGIN
        AutoDupeEnableCQ := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'AUTO DUPE ENABLE S AND P' THEN
        BEGIN
        AutoDupeEnableSAndP := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'AUTO QSL INTERVAL' THEN
        BEGIN
        Val (CMD, AutoQSLInterval, Result);
        ProcessConfigInstructions1 := Result = 0;
        IF Result = 0 THEN AutoQSLCount := AutoQSLInterval;
        Exit;
        END;

    IF ID = 'AUTO QSO NUMBER DECREMENT' THEN
        BEGIN
        AutoQSONumberDecrement := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'AUTO RETURN TO CQ MODE' THEN
        BEGIN
        AutoReturnToCQMode := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'AUTO S&P ENABLE' THEN
        BEGIN
        AutoSAPEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'AUTO S&P ENABLE SENSITIVITY' THEN {KK1L: 6.72}
        BEGIN
        Val (CMD, AutoSAPEnableRate, Result);
        ProcessConfigInstructions1 := Result = 0;
        IF Result = 0 THEN
          BEGIN
          IF NOT ((AutoSAPEnableRate > 9) AND (AutoSAPEnableRate < 10001)) THEN
              AutoSAPEnableRate := 1000;
          END;
        Exit;
        END;

    IF ID = 'AUTO SEND CHARACTER COUNT' THEN
        BEGIN
        Val (CMD, AutoSendCharacterCount, Result);
        ProcessConfigInstructions1 := Result = 0;
        Exit;
        END;

    IF ID = 'AUTO TIME INCREMENT' THEN
        BEGIN
        Val (CMD, AutoTimeIncrementQSOs, Result);
        ProcessConfigInstructions1 := Result = 0;
        Exit;
        END;

    IF ID = 'BACKCOPY ENABLE' THEN
        BEGIN
        BackCopyEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'BAND' THEN
        BEGIN
        ActiveBand := NoBand;
        IF CMD = '160' THEN ActiveBand := Band160;
        IF CMD = '80'  THEN ActiveBand := Band80;
        IF CMD = '40'  THEN ActiveBand := Band40;
        IF CMD = '30'  THEN ActiveBand := Band30;
        IF CMD = '20'  THEN ActiveBand := Band20;
        IF CMD = '17'  THEN ActiveBand := Band17;
        IF CMD = '15'  THEN ActiveBand := Band15;
        IF CMD = '12'  THEN ActiveBand := Band12;
        IF CMD = '10'  THEN ActiveBand := Band10;
        IF CMD = '6'  THEN ActiveBand := Band6;
        IF CMD = '2'  THEN ActiveBand := Band2;
        IF CMD = '222'  THEN ActiveBand := Band222;
        IF CMD = '432'  THEN ActiveBand := Band432;
        IF CMD = '902'  THEN ActiveBand := Band902;
        IF CMD = '1GH'  THEN ActiveBand := Band1296;
        IF CMD = '2GH'  THEN ActiveBand := Band2304;
        IF CMD = '3GH'  THEN ActiveBand := Band3456;
        IF CMD = '5GH'  THEN ActiveBand := Band5760;
        IF CMD = '10G'  THEN ActiveBand := Band10G;
        IF CMD = '24G'  THEN ActiveBand := Band24G;
        IF CMD = 'LGT'  THEN ActiveBand := BandLight;

        ProcessConfigInstructions1 := ActiveBand <> NoBand;
        Exit;
        END;

    IF ID = 'BAND MAP ALL BANDS' THEN
        BEGIN
        BandMapAllBands := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'BAND MAP ALL MODES' THEN
        BEGIN
        BandMapAllModes := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'BAND MAP MULTS ONLY' THEN  {KK1L: 6.68}
        BEGIN
        BandMapMultsOnly := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'BAND MAP CALL WINDOW ENABLE' THEN
        BEGIN
        BandMapCallWindowEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'BAND MAP CUTOFF FREQUENCY' THEN
        BEGIN
        Val (CMD, TempLongInt, Result);

        IF Result = 0 THEN
            BEGIN
            AddBandMapModeCutoffFrequency (TempLongInt);
            ProcessConfigInstructions1 := True;
            END;

        Exit;
        END;

    IF ID = 'BAND MAP DECAY TIME' THEN
        BEGIN
        Val (CMD, BandMapDecayValue, Result);
        ProcessConfigInstructions1 := Result = 0;
        IF Result = 0 THEN
          BEGIN
          BandMapDecayMultiplier  := (BandMapDecayValue div 64) + 1; {KK1L: 6.65}
          BandMapDecayTime := BandMapDecayValue div BandMapDecayMultiplier; {KK1L: 6.65}
          END;
        Exit;
        END;

    IF ID = 'BAND MAP DISPLAY CQ' THEN
        BEGIN
        BandMapDisplayCQ := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'BAND MAP ENABLE' THEN
        BEGIN
        BandMapEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'BAND MAP DUPE DISPLAY' THEN
        BEGIN
        BandMapDupeDisplay := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'BAND MAP GUARD BAND' THEN
        BEGIN
        Val (CMD, BandMapGuardBand, Result);
        ProcessConfigInstructions1 := Result = 0;
        Exit;
        END;

    IF ID = 'BAND MAP SPLIT MODE' THEN
        BEGIN
        CMD := UpperCase (CMD);

        IF CMD = 'BY CUTOFF FREQ' THEN BandMapSplitMode := ByCutoffFrequency;
        IF CMD = 'ALWAYS PHONE'   THEN BandMapSplitMode := AlwaysPhone;

        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'BEEP SOUNDCARD ENABLE' THEN
       BEGIN
       BeepSoundCardEnable := UpCase (CMD [1]) = 'T';
       ProcessConfigInstructions1 := True;
       soundmode(0);
       if not dvpsetup then beginsound;
       Exit;
       END;

    IF ID = 'BEEP ENABLE' THEN
        BEGIN
        Tone.SetBeepEnable(UpCase (CMD [1]) = 'T');
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'BEEP EVERY 10 QSOS' THEN
        BEGIN
        BeepEvery10QSOs := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'BIG REMAINING LIST' THEN
        BEGIN
        BigRemainingList := UpCase (CMD [1]) = 'T';
        IF NOT CountryTable.CustomRemainingCountryListFound THEN
            CountryTable.MakeDefaultRemainingCountryList;
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'BROADCAST ALL PACKET DATA' THEN
        BEGIN
        Packet.BroadcastAllPacketData := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;


    IF (ID = 'CALL OK NOW MESSAGE') OR (ID = 'CALL OK NOW CW MESSAGE') THEN
        BEGIN
        CorrectedCallMessage := UpperCase (CMD);
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'CALL OK NOW SSB MESSAGE' THEN
        BEGIN
        CorrectedCallPhoneMessage := UpperCase (CMD);
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'CALL WINDOW SHOW ALL SPOTS' THEN
        BEGIN
        CallWindowShowAllSpots := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'CALL WINDOW POSITION' THEN
        BEGIN
        CallWindowPosition := NormalCallWindowPosition;
        CMD := UpperCase (CMD);
        IF Stringhas (CMD, 'UP') THEN CallWindowPosition := UpOneCallWindowPosition;
        ProcessConfigInstructions1 := True;
        END;

    IF ID = 'CALLSIGN UPDATE ENABLE' THEN
        BEGIN
        CallsignUpdateEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'CHECK LOG FILE SIZE' THEN
        BEGIN
        CheckLogFileSize := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'CLEAR DUPE SHEET' THEN
        BEGIN
        IF (UpCase (CMD [1]) = 'T') AND RunningConfigFile THEN
            ClearDupeSheetCommandGiven := True;

        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'CODE SPEED' THEN
        BEGIN
        IF StringIsAllNumbers (CMD) THEN
            BEGIN
            VAL (CMD, Speed, Result);
            IF Result = 0 THEN
                BEGIN
                CodeSpeed := Speed;
                ProcessConfigInstructions1 := True;
                END;
            END;
        Exit;
        END;

    IF ID = 'COLUMN DUPESHEET ENABLE' THEN
        BEGIN
        ColumnDupeSheetEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'COMPUTER ID' THEN
        BEGIN
        CMD := UpperCase (CMD);

        IF CMD = 'NONE' THEN
            ComputerID := Chr (0)
        ELSE
            IF CMD <> '' THEN
                BEGIN
                IF (CMD [1] >= 'A') AND (CMD [1] <= 'Z') THEN
                    ComputerId := CMD [1];
                END
            ELSE
                ComputerID := Chr (0);

        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'CONFIRM EDIT CHANGES' THEN
        BEGIN
        ConfirmEditChanges := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'COPY FILES' THEN
        BEGIN
        CopyFiles (RemoveFirstString (CMD), RemoveFirstString (CMD), RemoveFirstString (CMD));
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'COUNT DOMESTIC COUNTRIES' THEN
        BEGIN
        CountDomesticCountries := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'COUNTRY INFORMATION FILE' THEN
        BEGIN
        CountryInformationFile := CMD;
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF (ID = 'CQ EXCHANGE') OR (ID = 'CQ CW EXCHANGE') THEN
        BEGIN
        CQExchange := UpperCase (CMD);
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'CQ SSB EXCHANGE' THEN
        BEGIN
        CQPhoneExchange := UpperCase (CMD);
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF (ID = 'CQ EXCHANGE NAME KNOWN') OR (ID = 'CQ CW EXCHANGE NAME KNOWN') THEN
        BEGIN
        CQExchangeNameKnown := UpperCase (CMD);
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'CQ SSB EXCHANGE NAME KNOWN' THEN
        BEGIN
        CQPhoneExchangeNameKnown := UpperCase (CMD);
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF StringHas (ID, 'CQ MEMORY F') OR StringHas (ID, 'CQ CW MEMORY F') THEN
        BEGIN
        ProcessConfigInstructions1 := True;
        SniffOutControlCharacters (CMD);
        ID := PostcedingString (ID, 'MEMORY F');

        IF ID = '1'  THEN BEGIN SetCQMemoryString (CW, F1,  CMD); Exit; END;
        IF ID = '2'  THEN BEGIN SetCQMemoryString (CW, F2,  CMD); Exit; END;
        IF ID = '3'  THEN BEGIN SetCQMemoryString (CW, F3,  CMD); Exit; END;
        IF ID = '4'  THEN BEGIN SetCQMemoryString (CW, F4,  CMD); Exit; END;
        IF ID = '5'  THEN BEGIN SetCQMemoryString (CW, F5,  CMD); Exit; END;
        IF ID = '6'  THEN BEGIN SetCQMemoryString (CW, F6,  CMD); Exit; END;
        IF ID = '7'  THEN BEGIN SetCQMemoryString (CW, F7,  CMD); Exit; END;
        IF ID = '8'  THEN BEGIN SetCQMemoryString (CW, F8,  CMD); Exit; END;
        IF ID = '9'  THEN BEGIN SetCQMemoryString (CW, F9,  CMD); Exit; END;
        IF ID = '10' THEN BEGIN SetCQMemoryString (CW, F10, CMD); Exit; END;
        IF ID = '11' THEN BEGIN SetCQMemoryString (CW, F11, CMD); Exit; END;
        IF ID = '12' THEN BEGIN SetCQMemoryString (CW, F12, CMD); Exit; END;

        ProcessConfigInstructions1 := False;
        Exit;
        END;

    IF StringHas (ID, 'CQ MEMORY ALTF') OR StringHas (ID, 'CQ CW MEMORY ALTF') THEN
        BEGIN
        ProcessConfigInstructions1 := True;
        SniffOutControlCharacters (CMD);
        ID := PostcedingString (ID, 'ALTF');

        IF ID = '1'  THEN BEGIN SetCQMemoryString (CW, AltF1,  CMD); Exit; END;
        IF ID = '2'  THEN BEGIN SetCQMemoryString (CW, AltF2,  CMD); Exit; END;
        IF ID = '3'  THEN BEGIN SetCQMemoryString (CW, AltF3,  CMD); Exit; END;
        IF ID = '4'  THEN BEGIN SetCQMemoryString (CW, AltF4,  CMD); Exit; END;
        IF ID = '5'  THEN BEGIN SetCQMemoryString (CW, AltF5,  CMD); Exit; END;
        IF ID = '6'  THEN BEGIN SetCQMemoryString (CW, AltF6,  CMD); Exit; END;
        IF ID = '7'  THEN BEGIN SetCQMemoryString (CW, AltF7,  CMD); Exit; END;
        IF ID = '8'  THEN BEGIN SetCQMemoryString (CW, AltF8,  CMD); Exit; END;
        IF ID = '9'  THEN BEGIN SetCQMemoryString (CW, AltF9,  CMD); Exit; END;
        IF ID = '10' THEN BEGIN SetCQMemoryString (CW, AltF10, CMD); Exit; END;
        IF ID = '11' THEN BEGIN SetCQMemoryString (CW, AltF11, CMD); Exit; END;
        IF ID = '12' THEN BEGIN SetCQMemoryString (CW, AltF12, CMD); Exit; END;

        ProcessConfigInstructions1 := False;
        Exit;
        END;

    IF StringHas (ID, 'CQ MEMORY CONTROLF') OR StringHas (ID, 'CQ CW MEMORY CONTROLF') THEN
        BEGIN
        ProcessConfigInstructions1 := True;
        SniffOutControlCharacters (CMD);
        ID := PostcedingString (ID, 'CONTROLF');

        IF ID = '1'  THEN BEGIN SetCQMemoryString (CW, ControlF1,  CMD); Exit; END;
        IF ID = '2'  THEN BEGIN SetCQMemoryString (CW, ControlF2,  CMD); Exit; END;
        IF ID = '3'  THEN BEGIN SetCQMemoryString (CW, ControlF3,  CMD); Exit; END;
        IF ID = '4'  THEN BEGIN SetCQMemoryString (CW, ControlF4,  CMD); Exit; END;
        IF ID = '5'  THEN BEGIN SetCQMemoryString (CW, ControlF5,  CMD); Exit; END;
        IF ID = '6'  THEN BEGIN SetCQMemoryString (CW, ControlF6,  CMD); Exit; END;
        IF ID = '7'  THEN BEGIN SetCQMemoryString (CW, ControlF7,  CMD); Exit; END;
        IF ID = '8'  THEN BEGIN SetCQMemoryString (CW, ControlF8,  CMD); Exit; END;
        IF ID = '9'  THEN BEGIN SetCQMemoryString (CW, ControlF9,  CMD); Exit; END;
        IF ID = '10' THEN BEGIN SetCQMemoryString (CW, ControlF10, CMD); Exit; END;
        IF ID = '11' THEN BEGIN SetCQMemoryString (CW, ControlF11, CMD); Exit; END;
        IF ID = '12' THEN BEGIN SetCQMemoryString (CW, ControlF12, CMD); Exit; END;

        ProcessConfigInstructions1 := False;
        Exit;
        END;

    IF StringHas (ID, 'CQ SSB MEMORY F') THEN
        BEGIN
        ProcessConfigInstructions1 := True;
        SniffOutControlCharacters (CMD);
        ID := PostcedingString (ID, 'MEMORY F');

        IF ID = '1'  THEN BEGIN SetCQMemoryString (Phone, F1,  CMD); Exit; END;
        IF ID = '2'  THEN BEGIN SetCQMemoryString (Phone, F2,  CMD); Exit; END;
        IF ID = '3'  THEN BEGIN SetCQMemoryString (Phone, F3,  CMD); Exit; END;
        IF ID = '4'  THEN BEGIN SetCQMemoryString (Phone, F4,  CMD); Exit; END;
        IF ID = '5'  THEN BEGIN SetCQMemoryString (Phone, F5,  CMD); Exit; END;
        IF ID = '6'  THEN BEGIN SetCQMemoryString (Phone, F6,  CMD); Exit; END;
        IF ID = '7'  THEN BEGIN SetCQMemoryString (Phone, F7,  CMD); Exit; END;
        IF ID = '8'  THEN BEGIN SetCQMemoryString (Phone, F8,  CMD); Exit; END;
        IF ID = '9'  THEN BEGIN SetCQMemoryString (Phone, F9,  CMD); Exit; END;
        IF ID = '10' THEN BEGIN SetCQMemoryString (Phone, F10, CMD); Exit; END;
        IF ID = '11' THEN BEGIN SetCQMemoryString (Phone, F11, CMD); Exit; END;
        IF ID = '12' THEN BEGIN SetCQMemoryString (Phone, F12, CMD); Exit; END;

        ProcessConfigInstructions1 := False;
        Exit;
        END;

    IF StringHas (ID, 'CQ SSB MEMORY ALTF') THEN
        BEGIN
        ProcessConfigInstructions1 := True;
        SniffOutControlCharacters (CMD);
        ID := PostcedingString (ID, 'ALTF');

        IF ID = '1'  THEN BEGIN SetCQMemoryString (Phone, AltF1,  CMD); Exit; END;
        IF ID = '2'  THEN BEGIN SetCQMemoryString (Phone, AltF2,  CMD); Exit; END;
        IF ID = '3'  THEN BEGIN SetCQMemoryString (Phone, AltF3,  CMD); Exit; END;
        IF ID = '4'  THEN BEGIN SetCQMemoryString (Phone, AltF4,  CMD); Exit; END;
        IF ID = '5'  THEN BEGIN SetCQMemoryString (Phone, AltF5,  CMD); Exit; END;
        IF ID = '6'  THEN BEGIN SetCQMemoryString (Phone, AltF6,  CMD); Exit; END;
        IF ID = '7'  THEN BEGIN SetCQMemoryString (Phone, AltF7,  CMD); Exit; END;
        IF ID = '8'  THEN BEGIN SetCQMemoryString (Phone, AltF8,  CMD); Exit; END;
        IF ID = '9'  THEN BEGIN SetCQMemoryString (Phone, AltF9,  CMD); Exit; END;
        IF ID = '10' THEN BEGIN SetCQMemoryString (Phone, AltF10, CMD); Exit; END;
        IF ID = '11' THEN BEGIN SetCQMemoryString (Phone, AltF11, CMD); Exit; END;
        IF ID = '12' THEN BEGIN SetCQMemoryString (Phone, AltF12, CMD); Exit; END;

        ProcessConfigInstructions1 := False;
        Exit;
        END;

    IF StringHas (ID, 'CQ SSB MEMORY CONTROLF') THEN
        BEGIN
        ProcessConfigInstructions1 := True;
        SniffOutControlCharacters (CMD);
        ID := PostcedingString (ID, 'CONTROLF');

        IF ID = '1'  THEN BEGIN SetCQMemoryString (Phone, ControlF1,  CMD); Exit; END;
        IF ID = '2'  THEN BEGIN SetCQMemoryString (Phone, ControlF2,  CMD); Exit; END;
        IF ID = '3'  THEN BEGIN SetCQMemoryString (Phone, ControlF3,  CMD); Exit; END;
        IF ID = '4'  THEN BEGIN SetCQMemoryString (Phone, ControlF4,  CMD); Exit; END;
        IF ID = '5'  THEN BEGIN SetCQMemoryString (Phone, ControlF5,  CMD); Exit; END;
        IF ID = '6'  THEN BEGIN SetCQMemoryString (Phone, ControlF6,  CMD); Exit; END;
        IF ID = '7'  THEN BEGIN SetCQMemoryString (Phone, ControlF7,  CMD); Exit; END;
        IF ID = '8'  THEN BEGIN SetCQMemoryString (Phone, ControlF8,  CMD); Exit; END;
        IF ID = '9'  THEN BEGIN SetCQMemoryString (Phone, ControlF9,  CMD); Exit; END;
        IF ID = '10' THEN BEGIN SetCQMemoryString (Phone, ControlF10, CMD); Exit; END;
        IF ID = '11' THEN BEGIN SetCQMemoryString (Phone, ControlF11, CMD); Exit; END;
        IF ID = '12' THEN BEGIN SetCQMemoryString (Phone, ControlF12, CMD); Exit; END;

        ProcessConfigInstructions1 := False;
        Exit;
        END;

    IF ID = 'CQ MENU' THEN
        BEGIN
        Delete(CMD, 80, 100); {KK1L: 6.65 limits CMD to size of ExchangeFunctionKeyMenu}
        CQMenu := CMD;
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'CURTIS KEYER MODE' THEN
        BEGIN
        case Char(uppercase(cmd)[1]) of
        'A': 
           begin
              CPUKeyer.SetCurtisMode(ModeA);
              WinKey.SetCurtisMode(ModeA);
              ProcessConfigInstructions1 := True;
           end;
        'B':
           begin
              CPUKeyer.SetCurtisMode(ModeB);
              WinKey.SetCurtisMode(ModeB);
              ProcessConfigInstructions1 := True;
           end;
        'U':
           begin
              CPUKeyer.SetCurtisMode(Ultimatic);
              WinKey.SetCurtisMode(Ultimatic);
              ProcessConfigInstructions1 := True;
           end;
        end;
        Exit;
     END;

    IF ID = 'CUSTOM INITIAL EXCHANGE STRING' THEN
        BEGIN
        CustomInitialExchangeString := UpperCase (CMD);
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'CUSTOM USER STRING' THEN
        BEGIN
        CustomUserString := UpperCase (CMD);
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'CW ENABLE' THEN
        BEGIN
        CWEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'CW SPEED FROM DATABASE' THEN
        BEGIN
        CWSpeedFromDataBase := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'CW SPEED INCREMENT' THEN  {KK1L: 6.72}
        BEGIN
        VAL (CMD, TempValue, Result);
        IF (TempValue > 0) AND (TempValue < 11) THEN
            BEGIN
            CodeSpeedIncrement := TempValue;
            ProcessConfigInstructions1 := True;
            END;
        Exit;
        END;

    IF ID = 'CW TONE' THEN
        BEGIN
        VAL (CMD, TempValue, Result);
        IF Result = 0 THEN
            BEGIN
            CWTone := TempValue;
            ProcessConfigInstructions1 := True;
            END;
        Exit;
        END;

    IF ID = 'DE ENABLE' THEN
        BEGIN
        DEEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'DIGITAL MODE ENABLE' THEN
        BEGIN
        DigitalModeEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'DISPLAY BAND MAP ENABLE' THEN  {KK1L: 6.73 Supress display of BM but keep function}
        BEGIN
        DisplayBandMapEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'DISPLAY MODE' THEN
        BEGIN
        IF UpperCase (CMD) = 'COLOR' THEN
            BEGIN
            DoingColors := True;
            SelectedColors := ColorColors;
            ProcessConfigInstructions1 := True;
            END;

        IF UpperCase (CMD) = 'MONO'  THEN
            BEGIN
            DoingColors := False;
            SelectedColors := MonoColors;
            ProcessConfigInstructions1 := True;
            END;
        Exit;
        END;



    IF ID = 'DISTANCE MODE' THEN
        BEGIN
        DistanceMode := NoDistanceDisplay;

        CMD := UpperCase (CMD);

        IF CMD = 'MILES' THEN DistanceMode := DistanceMiles;
        IF CMD = 'KM'    THEN DistanceMode := DistanceKM;

        ProcessConfigInstructions1 := (DistanceMode <> NoDistanceDisplay) OR (CMD = 'NONE');
        END;


    IF ID = 'DVK PORT' THEN
        BEGIN
        ActiveDVKPort := nil;
        IF StringHas(UpperCase(CMD),'PARALLEL') THEN
           BEGIN
              tempstring := wordafter(CMD,'PARALLEL');
              ActiveDVKPort := parallelportx(findportx(tempstring));
              if ActiveDVKPort = nil then
                 begin
                    ActiveDVKPort := parallelportx.create(tempstring);
                    addport(ActiveDVKPort);
                 end;
           END;

        IF ActiveDVKPort <> nil THEN
            BEGIN
            SetCQMemoryString (Phone,  F1, 'DVK1');
            SetCQMemoryString (Phone,  F2, 'DVK2');
            SetCQMemoryString (Phone,  F3, 'DVK3');
            SetCQMemoryString (Phone,  F4, 'DVK4');
            SetCQMemoryString (Phone,  F5, '');
            SetCQMemoryString (Phone,  F6, '');
            SetCQMemoryString (Phone,  F7, '');
            SetCQMemoryString (Phone,  F8, '');
            SetCQMemoryString (Phone,  F9, '');
            SetCQMemoryString (Phone, F10, 'DVK0');

            SetEXMemoryString (Phone,  F1, '');
            SetEXMemoryString (Phone,  F2, '');
            SetEXMemoryString (Phone,  F3, '');
            SetEXMemoryString (Phone,  F4, '');
            SetEXMemoryString (Phone,  F5, '');
            SetEXMemoryString (Phone,  F6, '');
            SetEXMemoryString (Phone,  F7, '');
            SetEXMemoryString (Phone,  F8, '');
            SetEXMemoryString (Phone,  F9, '');
            SetCQMemoryString (Phone, F10, 'DVK0');

            CorrectedCallPhoneMessage          := '';
            CQPhoneExchange                    := '';
            CQPhoneExchangeNameKnown           := '';
            QSLPhoneMessage                    := '';
            QSOBeforePhoneMessage              := '';
            QuickQSLPhoneMessage               := '';
            RepeatSearchAndPouncePhoneExchange := '';
            SearchAndPouncePhoneExchange       := '';

            SetEXMemoryString (Phone, AltF1,  '');
            SetEXMemoryString (Phone, AltF2,  '');
            SetEXMemoryString (Phone, AltF3,  '');
            SetEXMemoryString (Phone, AltF4,  '');
            SetEXMemoryString (Phone, AltF5,  '');
            SetEXMemoryString (Phone, AltF6,  '');
            SetEXMemoryString (Phone, AltF7,  '');
            SetEXMemoryString (Phone, AltF8,  '');
            SetEXMemoryString (Phone, AltF9,  '');
            SetEXMemoryString (Phone, AltF10, '');

            DVPOn := True;
            END;

        ProcessConfigInstructions1 := (ActiveDVKPort <> nil) OR (CMD = 'NONE');
        Exit;
        END;

    IF ID = 'DUPE CHECK SOUND' THEN
        BEGIN
        CMD := UpperCase (CMD);

        IF CMD = 'NONE'         THEN DupeCheckSound := DupeCheckNoSound;
        IF CMD = 'DUPE BEEP'    THEN DupeCheckSound := DupeCheckBeepIfDupe;
        IF CMD = 'MULT FANFARE' THEN DupeCheckSound := DupeCheckGratsIfMult;

        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'DUPE SHEET ENABLE' THEN
        BEGIN
        Sheet.DupeSheetEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'DVP ENABLE' THEN
        BEGIN
        DVPEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'DVP PATH' THEN
        BEGIN
        CfgDvpPath := CMD;
        ProcessConfigInstructions1 := True;
        Exit;
        END;


    IF ID = 'EIGHT BIT PACKET PORT' THEN
        BEGIN
        Packet.EightBitPacketPort := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'EIGHT BIT RTTY PORT' THEN {KK1L: 6.71 Added for George Fremin's RTTY modem}
        BEGIN
        EightBitRTTYPort := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF (ID = 'ESCAPE EXITS SEARCH AND POUNCE MODE') OR
       (ID = 'ESCAPE EXITS SEARCH AND POUNCE') THEN
            BEGIN
            EscapeExitsSearchAndPounce := UpCase (CMD [1]) = 'T';
            ProcessConfigInstructions1 := True;
            Exit;
            END;

    IF StringHas (ID, 'EX MEMORY F') OR StringHas (ID, 'EX CW MEMORY F') THEN
        BEGIN
        ProcessConfigInstructions1 := True;
        SniffOutControlCharacters (CMD);
        ID := PostcedingString (ID, 'MEMORY F');

        IF ID = '3'  THEN BEGIN SetExMemoryString (CW, F3,  CMD); Exit; END;
        IF ID = '4'  THEN BEGIN SetExMemoryString (CW, F4,  CMD); Exit; END;
        IF ID = '5'  THEN BEGIN SetExMemoryString (CW, F5,  CMD); Exit; END;
        IF ID = '6'  THEN BEGIN SetExMemoryString (CW, F6,  CMD); Exit; END;
        IF ID = '7'  THEN BEGIN SetExMemoryString (CW, F7,  CMD); Exit; END;
        IF ID = '8'  THEN BEGIN SetExMemoryString (CW, F8,  CMD); Exit; END;
        IF ID = '9'  THEN BEGIN SetExMemoryString (CW, F9,  CMD); Exit; END;
        IF ID = '10' THEN BEGIN SetExMemoryString (CW, F10, CMD); Exit; END;
        IF ID = '11' THEN BEGIN SetExMemoryString (CW, F11, CMD); Exit; END;
        IF ID = '12' THEN BEGIN SetExMemoryString (CW, F12, CMD); Exit; END;

        ProcessConfigInstructions1 := False;
        Exit;
        END;



    IF StringHas (ID, 'EX MEMORY ALTF') OR StringHas (ID, 'EX CW MEMORY ALTF') THEN
        BEGIN
        ProcessConfigInstructions1 := True;
        SniffOutControlCharacters (CMD);
        ID := PostcedingString (ID, 'ALTF');

        IF ID = '1'  THEN BEGIN SetExMemoryString (CW, AltF1,  CMD); Exit; END;
        IF ID = '2'  THEN BEGIN SetExMemoryString (CW, AltF2,  CMD); Exit; END;
        IF ID = '3'  THEN BEGIN SetExMemoryString (CW, AltF3,  CMD); Exit; END;
        IF ID = '4'  THEN BEGIN SetExMemoryString (CW, AltF4,  CMD); Exit; END;
        IF ID = '5'  THEN BEGIN SetExMemoryString (CW, AltF5,  CMD); Exit; END;
        IF ID = '6'  THEN BEGIN SetExMemoryString (CW, AltF6,  CMD); Exit; END;
        IF ID = '7'  THEN BEGIN SetExMemoryString (CW, AltF7,  CMD); Exit; END;
        IF ID = '8'  THEN BEGIN SetExMemoryString (CW, AltF8,  CMD); Exit; END;
        IF ID = '9'  THEN BEGIN SetExMemoryString (CW, AltF9,  CMD); Exit; END;
        IF ID = '10' THEN BEGIN SetExMemoryString (CW, AltF10, CMD); Exit; END;
        IF ID = '11' THEN BEGIN SetExMemoryString (CW, AltF11, CMD); Exit; END;
        IF ID = '12' THEN BEGIN SetExMemoryString (CW, AltF12, CMD); Exit; END;

        ProcessConfigInstructions1 := False;
        Exit;
        END;

    IF StringHas (ID, 'EX MEMORY CONTROLF') OR StringHas (ID, 'EX CW MEMORY CONTROLF') THEN
        BEGIN
        ProcessConfigInstructions1 := True;
        SniffOutControlCharacters (CMD);
        ID := PostcedingString (ID, 'CONTROLF');

        IF ID = '1'  THEN BEGIN SetExMemoryString (CW, ControlF1,  CMD); Exit; END;
        IF ID = '2'  THEN BEGIN SetExMemoryString (CW, ControlF2,  CMD); Exit; END;
        IF ID = '3'  THEN BEGIN SetExMemoryString (CW, ControlF3,  CMD); Exit; END;
        IF ID = '4'  THEN BEGIN SetExMemoryString (CW, ControlF4,  CMD); Exit; END;
        IF ID = '5'  THEN BEGIN SetExMemoryString (CW, ControlF5,  CMD); Exit; END;
        IF ID = '6'  THEN BEGIN SetExMemoryString (CW, ControlF6,  CMD); Exit; END;
        IF ID = '7'  THEN BEGIN SetExMemoryString (CW, ControlF7,  CMD); Exit; END;
        IF ID = '8'  THEN BEGIN SetExMemoryString (CW, ControlF8,  CMD); Exit; END;
        IF ID = '9'  THEN BEGIN SetExMemoryString (CW, ControlF9,  CMD); Exit; END;
        IF ID = '10' THEN BEGIN SetExMemoryString (CW, ControlF10, CMD); Exit; END;
        IF ID = '11' THEN BEGIN SetExMemoryString (CW, ControlF11, CMD); Exit; END;
        IF ID = '12' THEN BEGIN SetExMemoryString (CW, ControlF12, CMD); Exit; END;

        ProcessConfigInstructions1 := False;
        Exit;
        END;

    IF StringHas (ID, 'EX SSB MEMORY F') THEN
        BEGIN
        ProcessConfigInstructions1 := True;
        SniffOutControlCharacters (CMD);
        ID := PostcedingString (ID, 'MEMORY F');

        IF ID = '1'  THEN BEGIN SetExMemoryString (Phone, F1,  CMD); Exit; END;
        IF ID = '2'  THEN BEGIN SetExMemoryString (Phone, F2,  CMD); Exit; END;
        IF ID = '3'  THEN BEGIN SetExMemoryString (Phone, F3,  CMD); Exit; END;
        IF ID = '4'  THEN BEGIN SetExMemoryString (Phone, F4,  CMD); Exit; END;
        IF ID = '5'  THEN BEGIN SetExMemoryString (Phone, F5,  CMD); Exit; END;
        IF ID = '6'  THEN BEGIN SetExMemoryString (Phone, F6,  CMD); Exit; END;
        IF ID = '7'  THEN BEGIN SetExMemoryString (Phone, F7,  CMD); Exit; END;
        IF ID = '8'  THEN BEGIN SetExMemoryString (Phone, F8,  CMD); Exit; END;
        IF ID = '9'  THEN BEGIN SetExMemoryString (Phone, F9,  CMD); Exit; END;
        IF ID = '10' THEN BEGIN SetExMemoryString (Phone, F10, CMD); Exit; END;
        IF ID = '11' THEN BEGIN SetExMemoryString (Phone, F11, CMD); Exit; END;
        IF ID = '12' THEN BEGIN SetExMemoryString (Phone, F12, CMD); Exit; END;

        ProcessConfigInstructions1 := False;
        Exit;
        END;



    IF StringHas (ID, 'EX SSB MEMORY ALTF') THEN
        BEGIN
        ProcessConfigInstructions1 := True;
        SniffOutControlCharacters (CMD);
        ID := PostcedingString (ID, 'ALTF');

        IF ID = '1'  THEN BEGIN SetExMemoryString (Phone, AltF1,  CMD); Exit; END;
        IF ID = '2'  THEN BEGIN SetExMemoryString (Phone, AltF2,  CMD); Exit; END;
        IF ID = '3'  THEN BEGIN SetExMemoryString (Phone, AltF3,  CMD); Exit; END;
        IF ID = '4'  THEN BEGIN SetExMemoryString (Phone, AltF4,  CMD); Exit; END;
        IF ID = '5'  THEN BEGIN SetExMemoryString (Phone, AltF5,  CMD); Exit; END;
        IF ID = '6'  THEN BEGIN SetExMemoryString (Phone, AltF6,  CMD); Exit; END;
        IF ID = '7'  THEN BEGIN SetExMemoryString (Phone, AltF7,  CMD); Exit; END;
        IF ID = '8'  THEN BEGIN SetExMemoryString (Phone, AltF8,  CMD); Exit; END;
        IF ID = '9'  THEN BEGIN SetExMemoryString (Phone, AltF9,  CMD); Exit; END;
        IF ID = '10' THEN BEGIN SetExMemoryString (Phone, AltF10, CMD); Exit; END;
        IF ID = '11' THEN BEGIN SetExMemoryString (Phone, AltF11, CMD); Exit; END;
        IF ID = '12' THEN BEGIN SetExMemoryString (Phone, AltF12, CMD); Exit; END;

        ProcessConfigInstructions1 := False;
        Exit;
        END;

    IF StringHas (ID, 'EX SSB MEMORY CONTROLF') THEN
        BEGIN
        ProcessConfigInstructions1 := True;
        SniffOutControlCharacters (CMD);
        ID := PostcedingString (ID, 'CONTROLF');

        IF ID = '1'  THEN BEGIN SetExMemoryString (Phone, ControlF1,  CMD); Exit; END;
        IF ID = '2'  THEN BEGIN SetExMemoryString (Phone, ControlF2,  CMD); Exit; END;
        IF ID = '3'  THEN BEGIN SetExMemoryString (Phone, ControlF3,  CMD); Exit; END;
        IF ID = '4'  THEN BEGIN SetExMemoryString (Phone, ControlF4,  CMD); Exit; END;
        IF ID = '5'  THEN BEGIN SetExMemoryString (Phone, ControlF5,  CMD); Exit; END;
        IF ID = '6'  THEN BEGIN SetExMemoryString (Phone, ControlF6,  CMD); Exit; END;
        IF ID = '7'  THEN BEGIN SetExMemoryString (Phone, ControlF7,  CMD); Exit; END;
        IF ID = '8'  THEN BEGIN SetExMemoryString (Phone, ControlF8,  CMD); Exit; END;
        IF ID = '9'  THEN BEGIN SetExMemoryString (Phone, ControlF9,  CMD); Exit; END;
        IF ID = '10' THEN BEGIN SetExMemoryString (Phone, ControlF10, CMD); Exit; END;
        IF ID = '11' THEN BEGIN SetExMemoryString (Phone, ControlF11, CMD); Exit; END;
        IF ID = '12' THEN BEGIN SetExMemoryString (Phone, ControlF12, CMD); Exit; END;

        ProcessConfigInstructions1 := False;
        Exit;
        END;

    IF ID = 'EX MENU' THEN
        BEGIN
        Delete(CMD, 80, 100); {KK1L: 6.65 limits CMD to size of ExchangeFunctionKeyMenu}
        ExchangeFunctionKeyMenu := CMD;
        ProcessConfigInstructions1 := True;
        Exit;
        END;

    IF ID = 'EXCHANGE MEMORY ENABLE' THEN
        BEGIN
        ExchangeMemoryEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions1 := True;
        Exit;
        END;
    END;



FUNCTION ProcessConfigInstructions2 (ID: Str80; CMD: STRING): BOOLEAN;

VAR Result,tempint: INTEGER;
    TimeString, DateString, DayString: Str20;
    TempFreq, TempLongInt: LONGINT;
    TempBand: BandType;
    TempMode: ModeType;
    tempstring: string;
    tempport: keyerportx;

    BEGIN
    ProcessConfigInstructions2 := False;

    IF ID = 'FARNSWORTH ENABLE' THEN
        BEGIN
        CPUKeyer.SetFarnsworthEnable(UpCase (CMD [1]) = 'T');
        WinKey.SetFarnsworthEnable(UpCase (CMD [1]) = 'T');
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'FARNSWORTH SPEED' THEN
        BEGIN
        Val (CMD, tempint, Result);
        CPUKeyer.SetFarnsworthSpeed(tempint);
        WinKey.SetFarnsworthSpeed(tempint);
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'FLOPPY FILE SAVE FREQUENCY' THEN
        BEGIN
        Val (CMD, FloppyFileSaveFrequency, Result);
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'FLOPPY FILE SAVE NAME' THEN
        BEGIN
        FloppyFileSaveName := CMD;
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'FOOT SWITCH MODE' THEN
        BEGIN
        FootSwitchMode := FootSwitchDisabled;
        CPUKeyer.setCwGrant(false);
        Winkey.setCwGrant(false);
        CMD := UpperCase (CMD);

        IF CMD = 'NORMAL'                  THEN FootSwitchMode := Normal;
        IF CMD = 'F1'                      THEN FootSwitchMode := FootSwitchF1;
        IF CMD = 'LAST CQ FREQ'            THEN FootSwitchMode := FootSwitchLastCQFreq;
        IF CMD = 'NEXT BANDMAP'            THEN FootSwitchMode := FootSwitchNextBandMap;
        IF CMD = 'NEXT DISP BANDMAP'       THEN FootSwitchMode := FootSwitchNextDisplayedBandMap; {KK1L: 6.64}
        IF CMD = 'NEXT MULT BANDMAP'       THEN FootSwitchMode := FootSwitchNextMultBandMap;          {KK1L: 6.68}
        IF CMD = 'NEXT MULT DISP BANDMAP'  THEN FootSwitchMode := FootSwitchNextMultDisplayedBandMap; {KK1L: 6.68}
        IF CMD = 'DUPE CHECK'              THEN FootSwitchMode := FootSwitchDupecheck;
        IF CMD = 'DUPECHECK'               THEN FootSwitchMode := FootSwitchDupecheck;
        IF CMD = 'QSO NORMAL'              THEN FootSwitchMode := QSONormal;
        IF CMD = 'QSO QUICK'               THEN FootSwitchMode := QSOQuick;
        IF CMD = 'CONTROL ENTER'           THEN FootSwitchMode := FootSwitchControlEnter;
        IF CMD = 'START SENDING'           THEN FootSwitchMode := StartSending;
        IF CMD = 'SWAP RADIOS'             THEN FootSwitchMode := SwapRadio;
        IF CMD = 'CW GRANT'                THEN
        Begin
            FootSwitchMode := CWGrant;
            CPUKeyer.setCwGrant(true);
            WinKey.setCwGrant(true);
        End;

        ProcessConfigInstructions2 := (FootSwitchMode <> FootSwitchDisabled) OR
                                    (CMD = 'DISABLED');
        Exit;
        END;


    IF ID = 'FOOT SWITCH PORT' THEN
        BEGIN
        IF StringHas(UpperCase(CMD),'SERIAL') THEN
           BEGIN
              tempstring := wordafter(CMD,'SERIAL');
              Footsw.setPort(findportx(tempstring));
              if not Footsw.portDefined then
                 begin
                    tempport := serialportx.create(tempstring);
                    Footsw.setPort(tempport);
                    addport(tempport);
                 end;
           END
        ELSE IF StringHas(UpperCase(CMD),'PARALLEL') THEN
           BEGIN
              tempstring := wordafter(CMD,'PARALLEL');
              Footsw.setPort(findportx(tempstring));
              if not Footsw.portDefined then
                 begin
                    tempport := parallelportx.create(tempstring);
                    Footsw.setPort(tempport);
                    addport(tempport);
                 end;
           END;
        ProcessConfigInstructions2 := (Footsw.portDefined) OR
                                    (CMD = 'NONE');
        Exit;
        END;

    IF ID = 'FREQUENCY ADDER' THEN
        BEGIN
        Val (CMD, Radio1FrequencyAdder, Result);
        Radio2FrequencyAdder := Radio1FrequencyAdder;
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'FREQUENCY ADDER RADIO ONE' THEN
        BEGIN
        Val (CMD, Radio1FrequencyAdder, Result);
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'FREQUENCY ADDER RADIO TWO' THEN
        BEGIN
        Val (CMD, Radio2FrequencyAdder, Result);
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'FREQUENCY MEMORY' THEN
        BEGIN
        IF StringHas (CMD, 'SSB') THEN
            BEGIN
            Delete (CMD, Pos ('SSB ', CMD), 4);

            Val (CMD, TempFreq, Result);

            IF Result = 0 THEN
                BEGIN
                CalculateBandMode (TempFreq, TempBand, TempMode);
                FreqMemory [TempBand, Phone] := TempFreq;
                ProcessConfigInstructions2 := True;
                Exit;
                END;
            END
        ELSE
            BEGIN
            Val (CMD, TempFreq, Result);

            IF Result = 0 THEN
                BEGIN
                CalculateBandMode (TempFreq, TempBand, TempMode);
                FreqMemory [TempBand, TempMode] := TempFreq;
                ProcessConfigInstructions2 := True;
                END;
            END;

        Exit;
        END;


    IF ID = 'FREQUENCY MEMORY ENABLE' THEN
        BEGIN
        FrequencyMemoryEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    {KK1L: 6.71}
    IF ID = 'FREQUENCY POLL RATE' THEN
        BEGIN
        Val (CMD, TempLongInt, Result);
        IF (TempLongInt >= 10) AND (TempLongInt <= 1000) THEN {KK1L: 6.72}
            FreqPollRate := TempLongInt
        ELSE
            FreqPollRate := 250; {KK1L: 6.73 Better resutls with Icom and other radios.}
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'FT1000MP CW REVERSE' THEN
        BEGIN
        FT1000MPCWReverse := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'GRID MAP CENTER' THEN
        BEGIN
        IF LooksLikeAGrid (CMD) OR (CMD = '') THEN
            BEGIN
            GridMapCenter := Copy (CMD, 1, 4);
            ProcessConfigInstructions2 := True;
            END;
        Exit;
        END;


    IF ID = 'HF BAND ENABLE' THEN
        BEGIN
        HFBandEnable := UpCase (CMD [1])= 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'HOUR DISPLAY' THEN
        BEGIN
        IF UpperCase (CMD) = 'THIS HOUR'          THEN HourDisplay := ThisHour;
        IF UpperCase (CMD) = 'LAST SIXTY MINUTES' THEN HourDisplay := LastSixtyMins;
        IF UpperCase (CMD) = 'BAND CHANGES'       THEN HourDisplay := BandChanges;
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'HOUR OFFSET' THEN
        BEGIN
        Val (CMD, HourOffset, Result);
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'ICOM COMMAND PAUSE' THEN
        BEGIN
        Val (CMD, IcomCommandPause, Result);
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'ICOM RESPONSE TIMEOUT' THEN
        BEGIN
        Val (CMD, IcomResponseTimeout, Result);
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'INCREMENT TIME ENABLE' THEN
        BEGIN
        IncrementTimeEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'INITIAL EXCHANGE' THEN
        BEGIN
        IF UpperCase (CMD) = 'NONE' THEN
            BEGIN
            ActiveInitialExchange := NoInitialExchange;
            ProcessConfigInstructions2 := True;
            END;

        IF UpperCase (CMD) = 'CUSTOM' THEN
            BEGIN
            ActiveInitialExchange := CustomInitialExchange;
            ProcessConfigInstructions2 := True;
            END;

        IF UpperCase (CMD) = 'NAME' THEN
            BEGIN
            ActiveInitialExchange := NameInitialExchange;
            ProcessConfigInstructions2 := True;
            END;

        IF UpperCase (CMD) = 'ZONE' THEN
            BEGIN
            ActiveInitialExchange := ZoneInitialExchange;
            ProcessConfigInstructions2 := True;
            END;

        IF UpperCase (CMD) = 'NAME QTH' THEN
            BEGIN
            ActiveInitialExchange := NameQTHInitialExchange;
            ProcessConfigInstructions2 := True;
            END;

        IF UpperCase (CMD) = 'CHECK SECTION' THEN
            BEGIN
            ActiveInitialExchange := CheckSectionInitialExchange;
            ProcessConfigInstructions2 := True;
            END;

        IF UpperCase (CMD) = 'SECTION' THEN
            BEGIN
            ActiveInitialExchange := SectionInitialExchange;
            ProcessConfigInstructions2 := True;
            END;

        IF UpperCase (CMD) = 'QTH' THEN
            BEGIN
            ActiveInitialExchange := QTHInitialExchange;
            ProcessConfigInstructions2 := True;
            END;

        IF UpperCase (CMD) = 'GRID' THEN
            BEGIN
            ActiveInitialExchange := GridInitialExchange;
            ProcessConfigInstructions2 := True;
            END;

        IF UpperCase (CMD) = 'FOC NUMBER' THEN
            BEGIN
            ActiveInitialExchange := FOCInitialExchange;
            ProcessConfigInstructions2 := True;
            END;

        IF UpperCase (CMD) = 'USER 1' THEN
            BEGIN
            ActiveInitialExchange := User1InitialExchange;
            ProcessConfigInstructions2 := True;
            END;

        IF UpperCase (CMD) = 'USER 2' THEN
            BEGIN
            ActiveInitialExchange := User2InitialExchange;
            ProcessConfigInstructions2 := True;
            END;

        IF UpperCase (CMD) = 'USER 3' THEN
            BEGIN
            ActiveInitialExchange := User3InitialExchange;
            ProcessConfigInstructions2 := True;
            END;

        IF UpperCase (CMD) = 'USER 4' THEN
            BEGIN
            ActiveInitialExchange := User4InitialExchange;
            ProcessConfigInstructions2 := True;
            END;

        IF UpperCase (CMD) = 'USER 5' THEN
            BEGIN
            ActiveInitialExchange := User5InitialExchange;
            ProcessConfigInstructions2 := True;
            END;

        Exit;
        END;


    IF ID = 'INITIAL EXCHANGE FILENAME' THEN
        BEGIN
        InitialExchangeFilename := CMD;
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    {KK1L: 6.70}
    IF ID = 'INITIAL EXCHANGE OVERWRITE' THEN
        BEGIN
        InitialExchangeOverwrite := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'INITIAL EXCHANGE CURSOR POS' THEN
        BEGIN
        IF StringHas (UpperCase (CMD), 'END')   THEN InitialExchangeCursorPos := AtEnd;
        IF StringHas (UpperCase (CMD), 'START') THEN InitialExchangeCursorPos := AtStart;
        ProcessConfigInstructions2 := True;
        Exit;
        END;



    IF ID = 'INSERT MODE' THEN
        BEGIN
        InsertMode := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;


    IF ID = 'INTERCOM FILE ENABLE' THEN
        BEGIN
        IntercomFileenable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;


    IF ID = 'JST RESPONSE TIMEOUT' THEN
        BEGIN
        Val (CMD, JSTResponseTimeout, Result);
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'K1EA NETWORK ENABLE' THEN
        BEGIN
        K1EANetworkEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'K1EA STATION ID' THEN
        BEGIN
        K1EAStationID := UpCase (CMD [1]);
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'KENWOOD RESPONSE TIMEOUT' THEN
        BEGIN
        Val (CMD, KenwoodResponseTimeout, Result);
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'WINKEYER PORT' THEN
        BEGIN
        tempport := nil;

        IF StringHas(UpperCase(CMD),'SERIAL') THEN
           BEGIN
              tempstring := wordafter(CMD,'SERIAL');
              tempport := findportx(tempstring);
              if tempport = nil then
                 begin
                    tempport := serialportx.create(tempstring);
                    addport(tempport);
                 end;
           END;
        WinKey.setPort(serialportx(tempport));
        ActiveKeyer := WinKey;
        ProcessConfigInstructions2 :=
           (tempport <> nil) OR (CMD = 'NONE');
        Exit;
        END;

    IF (ID = 'YCCC SO2R BOX ENABLE') AND (UpCase (CMD [1]) = 'T') THEN
        BEGIN
        ActiveKeyer := YcccKey;
        Footsw := footso2r;
        ProcessConfigInstructions2 := true;
        Exit;
        END;

    IF (ID = 'SO2R HEADPHONE MODE') THEN
        BEGIN
        IF StringHas(UpperCase(CMD),'NORMAL') THEN
        begin
            so2rbox.setheadphonemode(HNORMAL);
            ProcessConfigInstructions2 := true;
            exit;
        end;
        IF StringHas(UpperCase(CMD),'SPATIAL') THEN
        begin
            so2rbox.setheadphonemode(HSPATIAL);
            ProcessConfigInstructions2 := true;
            exit;
        end;
        IF StringHas(UpperCase(CMD),'SYMMETRIC') THEN
        begin
            so2rbox.setheadphonemode(HSYMMETRIC);
            ProcessConfigInstructions2 := true;
            exit;
        end;
        
        ProcessConfigInstructions2 := false;
        Exit;
        END;

    IF (ID = 'SO2R BLEND ENABLE') then
        BEGIN
        IF (UpCase (CMD [1]) = 'T') THEN
           so2rbox.setblend(true)
        else
           so2rbox.setblend(false);

        ProcessConfigInstructions2 := true;
        Exit;
        END;

    IF (ID = 'SO2R BLEND') then
        BEGIN
        Val (CMD, tempint, Result);
        if (tempint > 255) then tempint := 255;
        if (tempint < 0) then tempint := 0;
        so2rbox.blendvalue(tempint);

        ProcessConfigInstructions2 := true;
        Exit;
        END;

    IF (ID = 'SO2R MICROPHONE RELAY ENABLE') then
        BEGIN
        IF (UpCase (CMD [1]) = 'T') THEN
           so2rbox.setmicrelay(true)
        else
           so2rbox.setmicrelay(false);

        ProcessConfigInstructions2 := true;
        Exit;
        END;

    IF (ID = 'SO2R RIG1 MAP') then
        BEGIN
        Val (CMD, tempint, Result);
        if (tempint > 4) then tempint := 4;
        if (tempint < -4) then tempint := -4;
        so2rbox.setrig1map(tempint);

        ProcessConfigInstructions2 := true;
        Exit;
        END;

    IF (ID = 'SO2R RIG2 MAP') then
        BEGIN
        Val (CMD, tempint, Result);
        if (tempint > 4) then tempint := 4;
        if (tempint < -4) then tempint := -4;
        so2rbox.setrig2map(tempint);

        ProcessConfigInstructions2 := true;
        Exit;
        END;

    IF ID = 'KEYER OUTPUT PORT' THEN
        BEGIN
        tempport := nil;

        IF StringHas(UpperCase(CMD),'SERIAL') THEN
           BEGIN
              tempstring := wordafter(CMD,'SERIAL');
              tempport := findportx(tempstring);
              if tempport = nil then
                 begin
                    tempport := serialportx.create(tempstring);
                    addport(tempport);
                 end;
              if StringHas (UpperCase(CMD), 'INVERT') then 
                 serialportx(tempport).invert(true);
              tempport.key(false);
           END
        ELSE IF StringHas(UpperCase(CMD),'PARALLEL') THEN
           BEGIN
              tempstring := wordafter(CMD,'PARALLEL');
              tempport := findportx(tempstring);
              if tempport = nil then
                 begin
                    tempport := parallelportx.create(tempstring);
                    addport(tempport);
                 end;
           END;

        CPUKeyer.setRadioOnePort(tempport);
        CPUKeyer.setRadioTwoPort(tempport);
        ActiveKeyer := CPUKeyer;
        ProcessConfigInstructions2 :=
           (tempport <> nil) OR (CMD = 'NONE');
        Exit;
        END;


    IF ID = 'KEYER RADIO ONE OUTPUT PORT' THEN
        BEGIN
        tempport := nil;

        IF StringHas(UpperCase(CMD),'SERIAL') THEN
           BEGIN
              tempstring := wordafter(CMD,'SERIAL');
              tempport := findportx(tempstring);
              if tempport = nil then
                 begin
                    tempport := serialportx.create(tempstring);
                    addport(tempport);
                 end;
              if StringHas (UpperCase(CMD), 'INVERT') then 
                 serialportx(tempport).invert(true);
              tempport.key(false);
           END
        ELSE IF StringHas(UpperCase(CMD),'PARALLEL') THEN
           BEGIN
              tempstring := wordafter(CMD,'PARALLEL');
              tempport := findportx(tempstring);
              if tempport = nil then
                 begin
                    tempport := parallelportx.create(tempstring);
                    addport(tempport);
                 end;
           END;

        CPUKeyer.setRadioOnePort(tempport);
        ActiveKeyer := CPUKeyer;

        ProcessConfigInstructions2 := (tempport <> nil) OR (CMD = 'NONE');
        Exit;
        END;

    IF ID = 'KEYER RADIO TWO OUTPUT PORT' THEN
        BEGIN
        tempport := nil;

        IF StringHas(UpperCase(CMD),'SERIAL') THEN
           BEGIN
              tempstring := wordafter(CMD,'SERIAL');
              tempport := findportx(tempstring);
              if tempport = nil then
                 begin
                    tempport := serialportx.create(tempstring);
                    addport(tempport);
                 end;
              if StringHas (UpperCase(CMD), 'INVERT') then 
                 serialportx(tempport).invert(true);
              tempport.key(false);
           END
        ELSE IF StringHas(UpperCase(CMD),'PARALLEL') THEN
           BEGIN
              tempstring := wordafter(CMD,'PARALLEL');
              tempport := findportx(tempstring);
              if tempport = nil then
                 begin
                    tempport := parallelportx.create(tempstring);
                    addport(tempport);
                 end;
           END;

        CPUKeyer.setRadioTwoPort(tempport);
        ActiveKeyer := CPUKeyer;

        ProcessConfigInstructions2 := (tempport <> nil) OR (CMD = 'NONE');
        Exit;
        END;

    IF ID = 'KEYPAD CW MEMORIES' THEN
        BEGIN
        KeypadCWMemories := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'LEADING ZEROS' THEN
        BEGIN
        CMD := UpperCase (CMD);

        IF StringIsAllNumbers (CMD) THEN
            BEGIN
            Val (CMD, LeadingZeros, Result);
            ProcessConfigInstructions2 := (Result = 0) AND
                                        (LeadingZeros < 4) AND
                                        (LeadingZeros >= 0);
            Exit;
            END
        ELSE
            IF (CMD [1] = 'T') OR (CMD [1] = 't') THEN
                LeadingZeros := 3
            ELSE
                LeadingZeros := 0;

        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'LEADING ZERO CHARACTER' THEN
        BEGIN
        LeadingZeroCharacter := UpCase (CMD [1]);
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'LEAVE CURSOR IN CALL WINDOW' THEN
        BEGIN
        LeaveCursorInCallWindow := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'LITERAL DOMESTIC QTH' THEN
        BEGIN
        LiteralDomesticQTH := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'LOG FREQUENCY ENABLE' THEN
        BEGIN
        LogFrequencyEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'LOG RST SENT'   THEN
        BEGIN
        LogRSTSent   := CMD;
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'LOG RS SENT'    THEN
        BEGIN
        LogRSSent    := CMD;
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'LOG WITH SINGLE ENTER' THEN
        BEGIN
        LogWithSingleEnter := Upcase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'LOOK FOR RST SENT' THEN
        BEGIN
        LookForRSTSent := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'MESSAGE ENABLE' THEN
        BEGIN
        MessageEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'MODE' THEN
        BEGIN
        ActiveMode := NoMode;
        IF (UpperCase (CMD) = 'SSB') OR (UpperCase (CMD) = 'PHONE') THEN
            ActiveMode := Phone;
        IF UpperCase (CMD) = 'CW' THEN ActiveMode := CW;
        ProcessConfigInstructions2 := ActiveMode <> NoMode;
        Exit;
        END;

    IF ID = 'MODEM PORT' THEN
        BEGIN
        ActiveModemPort := nil;
        IF StringHas(UpperCase(CMD),'SERIAL') THEN
           BEGIN
              tempstring := wordafter(CMD,'SERIAL');
              ActiveModemPort := serialportx(findportx(tempstring));
              if ActiveModemPort = nil then
                 begin
                    ActiveModemPort := serialportx.create(tempstring);
                    addport(ActiveModemPort);
                 end;
           END;
        ProcessConfigInstructions2 := (ActiveModemPort <> nil) OR (CMD = 'NONE');
        Exit;
        END;

    IF ID = 'MODEM PORT BAUD RATE' THEN
        BEGIN
        Val (CMD, ModemPortBaudRate, Result);
        ProcessConfigInstructions2 := (Result = 0) AND (ModemPortBaudRate <= 4800);
        Exit;
        END;

    IF ID = 'MOUSE ENABLE' THEN
        BEGIN
        MouseEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF (ID = 'MULT REPORT MINIMUM BANDS') OR (ID = 'MULT REPORT MINIMUM COUNTRIES') THEN
        BEGIN
        Val (CMD, MultReportMinimumBands, Result);
        ProcessConfigInstructions2 := (Result = 0) AND
                                    (MultReportMinimumBands <  6) AND
                                    (MultReportMinimumBands >= 2);
        Exit;
        END;

    IF ID = 'MULTI INFO MESSAGE' THEN
        BEGIN
        MultiInfoMessage := CMD;
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'MULTI MULTS ONLY' THEN
        BEGIN
        MultiMultsOnly := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'MULTI PORT' THEN
        BEGIN
        ActiveMultiPort := nil;
        IF StringHas(UpperCase(CMD),'SERIAL') THEN
           BEGIN
              tempstring := wordafter(CMD,'SERIAL');
              ActiveMultiPort := serialportx(findportx(tempstring));
              if ActiveMultiPort = nil then
                 begin
                    ActiveMultiPort := serialportx.create(tempstring);
                    addport(ActiveMultiPort);
                 end;
           END;

        IF ActiveMultiPort <> nil THEN Packet.PacketBandSpots := True;
        ProcessConfigInstructions2 := (ActiveMultiPort <> nil) OR (CMD = 'NONE');
        Exit;
        END;

    IF ID = 'MULTI PORT BAUD RATE' THEN
        BEGIN
        Val (CMD, MultiPortBaudRate, Result);
        ProcessConfigInstructions2 := (Result = 0) AND (MultiPortBaudRate <= 4800);
        Exit;
        END;

    IF ID = 'MULTI RETRY TIME' THEN
        BEGIN
        Val (CMD, MultiRetryTime, Result);
        ProcessConfigInstructions2 := (Result = 0) AND (MultiRetryTime >= 3);
        Exit;
        END;

    IF ID = 'MULTI UPDATE MULT DISPLAY' THEN
        BEGIN
        MultiUpdateMultDisplay := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'MULTIPLE BANDS' THEN
        BEGIN
        MultipleBandsEnabled := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'MULTIPLE MODES' THEN
        BEGIN
        MultipleModesEnabled := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'MY GRID' THEN
        BEGIN
        MyGrid := UpperCase (CMD);
        ProcessConfigInstructions2 := LooksLikeAGrid (MyGrid);
        Exit;
        END;

    IF ID = 'MY IOTA' THEN
        BEGIN
        MyIOTA := UpperCase (CMD);
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'NAME FLAG ENABLE' THEN
        BEGIN
        NameFlagEnable := Upcase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'NO LOG' THEN
        BEGIN
        NoLog := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'NO POLL DURING PTT' THEN
        BEGIN
        NoPollDuringPTT := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'ORION PORT' THEN
        BEGIN
        ActiveRotatorType := OrionRotator;

        ActiveRotatorPort := nil;
        IF StringHas(UpperCase(CMD),'SERIAL') THEN
           BEGIN
              tempstring := wordafter(CMD,'SERIAL');
              ActiveRotatorPort := serialportx(findportx(tempstring));
              if ActiveRotatorPort = nil then
                 begin
                    ActiveRotatorPort := serialportx.create(tempstring);
                    addport(ActiveRotatorPort);
                 end;
           END;

        ProcessConfigInstructions2 := (ActiveRotatorPort <> nil) OR (CMD = 'NONE');
        Exit;
        END;

    IF ID = 'ORION RESPONSE TIMEOUT' THEN
        BEGIN
        Val (CMD, OrionResponseTimeout, Result);
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'PACKET ADD LF' THEN
        BEGIN
        PacketAddLF := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'PACKET AUTO CR' THEN
        BEGIN
        PacketAutoCR := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'PACKET BAND SPOTS' THEN
        BEGIN
        Packet.PacketBandSpots := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'PACKET BAUD RATE' THEN
        BEGIN
        Val (CMD, Packet.PacketBaudRate, Result);
        ProcessConfigInstructions2 := (Result = 0) AND (Packet.PacketBaudRate <= 9600);
        Exit;
        END;


    IF ID = 'PACKET BEEP' THEN
        BEGIN
        Packet.PacketBeep := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'PACKET LOG FILENAME' THEN
        BEGIN
        Packet.PacketLogFileName := CMD;
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'PACKET PORT' THEN
        BEGIN
        ActivePacketPort := nil;
        IF StringHas(UpperCase(CMD),'SERIAL') THEN
           BEGIN
              tempstring := wordafter(CMD,'SERIAL');
              ActivePacketPort := serialportx(findportx(tempstring));
              if ActivePacketPort = nil then
                 begin
                    ActivePacketPort := serialportx.create(tempstring);
                    addport(ActivePacketPort);
                 end;
           END;
        ProcessConfigInstructions2 := (ActivePacketPort <> nil) OR (CMD = 'NONE');
        Exit;
        END;

    IF ID = 'PACKET PORT BAUD RATE' THEN
        BEGIN
        Val (CMD, Packet.PacketBaudRate, Result);
        ProcessConfigInstructions2 := (Result = 0) AND (Packet.PacketBaudRate <= 4800);
        Exit;
        END;

    IF ID = 'PACKET RETURN PER MINUTE' THEN
        BEGIN
        Val (CMD, PacketReturnPerMinute, Result);
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'PACKET SPOT EDIT ENABLE' THEN
        BEGIN
        PacketSpotEditEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'PACKET SPOT DISABLE' THEN
        BEGIN
        PacketSpotDisable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'PACKET SPOT KEY' THEN
        BEGIN
        PacketSpotKey := CMD [1];
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'PACKET SPOT PREFIX ONLY' THEN {KK1L: 6.72}
        BEGIN
        PacketSpotPrefixOnly := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'PACKET SPOTS' THEN
        BEGIN
        CMD := UpperCase (Copy (CMD, 1, 1));

        IF CMD = 'A' THEN
            BEGIN
            Packet.PacketSpots := AllSpots;
            ProcessConfigInstructions2 := True;
            END
        ELSE
            IF CMD = 'M' THEN
                BEGIN
                Packet.PacketSpots := MultSpots;
                ProcessConfigInstructions2 := True;
                END;
        END;

    IF ID = 'PADDLE BUG ENABLE' THEN
        BEGIN
        CPUKeyer.SetPaddleBug(UpCase (CMD [1]) = 'T');
        Winkey.SetPaddleBug(UpCase (CMD [1]) = 'T');
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'PADDLE MONITOR TONE' THEN
        BEGIN
        Val (CMD, tempint, Result);
        CPUKeyer.SetPaddleMonitorTone(tempint);
        Winkey.SetPaddleMonitorTone(tempint);
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'PADDLE PORT' THEN
        BEGIN
        tempport := nil;
        IF StringHas(UpperCase(CMD),'SERIAL') THEN
           BEGIN
              tempstring := wordafter(CMD,'SERIAL');
              tempport := findportx(tempstring);
              if tempport = nil then
                 begin
                    tempport := serialportx.create(tempstring);
                    addport(tempport);
                 end;
              CPUKeyer.SetActivePaddlePort(tempport);
              ActiveKeyer := CPUKeyer;
           END
        ELSE IF StringHas(UpperCase(CMD),'PARALLEL') THEN
           BEGIN
              tempstring := wordafter(CMD,'PARALLEL');
              tempport := findportx(tempstring);
              if tempport = nil then
                 begin
                    tempport := parallelportx.create(tempstring);
                    addport(tempport);
                 end;
              CPUKeyer.SetActivePaddlePort(tempport);
              ActiveKeyer := CPUKeyer;
           END;

        ProcessConfigInstructions2 := (tempport <> nil) OR (CMD = 'NONE');
        Exit;
        END;

    IF ID = 'PADDLE SPEED' THEN
        BEGIN
        Val (CMD, tempint, Result);
        CPUKeyer.SetPaddleSpeed(tempint);
        Winkey.SetPaddleSpeed(tempint);
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'PADDLE PTT HOLD COUNT' THEN
        BEGIN
        Val (CMD, tempint, Result);
        CPUKeyer.SetPaddlePTTHoldCount(tempint);
        Winkey.SetPaddlePTTHoldCount(tempint);
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'PARTIAL CALL ENABLE' THEN
        BEGIN
        PartialCallEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'PARTIAL CALL LOAD LOG ENABLE' THEN
        BEGIN
        PartialCallLoadLogEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'PARTIAL CALL MULT INFO ENABLE' THEN
        BEGIN
        PartialCallMultsEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'POLL RADIO ONE' THEN {KK1L: 6.72}
        BEGIN
        PollRadioOne := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'POLL RADIO TWO' THEN {KK1L: 6.72}
        BEGIN
        PollRadioTwo := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'POSSIBLE CALL ACCEPT KEY' THEN
        BEGIN
        PossibleCallAcceptKey := CMD [1];
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'POSSIBLE CALL LEFT KEY' THEN
        BEGIN
        PossibleCallLeftKey := CMD [1];
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'POSSIBLE CALL RIGHT KEY' THEN
        BEGIN
        PossibleCallRightKey := CMD [1];
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'POSSIBLE CALLS' THEN
        BEGIN
        PossibleCallEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'POSSIBLE CALL MODE' THEN
        BEGIN
        IF WRTC2002 THEN   {KK1L: 6.69 Don't allow WRTC folks to get SCP data from TRMaster}
            BEGIN
            CD.PossibleCallAction := LogOnly;
            ProcessConfigInstructions2 := True;
            END
        ELSE
            BEGIN
            IF UpperCase (CMD) = 'ALL' THEN
                BEGIN
                CD.PossibleCallAction := AnyCall;
                ProcessConfigInstructions2 := True;
                END;

            IF StringHas (UpperCase (CMD), 'NAME') THEN
                BEGIN
                CD.PossibleCallAction := OnlyCallsWithNames;
                ProcessConfigInstructions2 := True;
                END;

            IF UpperCase (CMD) = 'LOG ONLY' THEN
                BEGIN
                CD.PossibleCallAction := LogOnly;
                ProcessConfigInstructions2 := True;
                END;
            END;

        Exit;
        END;

    IF ID = 'PREFIX INFO FILENAME' THEN
        BEGIN
        PrefixInfoFileName := CMD;
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'PRINTER ENABLE' THEN
        BEGIN
        PrinterEnabled := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;


    IF ID = 'PTT ENABLE' THEN
        BEGIN
        CPUKeyer.SetPTTEnable(UpCase (CMD [1]) = 'T');
        Winkey.SetPTTEnable(UpCase (CMD [1]) = 'T');
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF (ID = 'PTT TURN ON DELAY') THEN
        BEGIN
        Val (CMD, tempint, Result);
        CPUKeyer.SetPTTTurnOnDelay(tempint);
        Winkey.SetPTTTurnOnDelay(tempint);
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF (ID = 'QSL MESSAGE') OR (ID = 'QSL CW MESSAGE') THEN
        BEGIN
        QSLMessage := CMD;
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'QSL SSB MESSAGE' THEN
        BEGIN
        QSLPhoneMessage := CMD;
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'QSL MODE' THEN
        BEGIN
        ParameterOkayMode := NoParameterOkayMode;
        CMD := UpperCase (CMD);
        IF StringHas (CMD, 'STAN') THEN ParameterOkayMode := Standard;

        IF StringHas (CMD, 'BUT')  THEN
            ParameterOkayMode := QSLButDoNotLog
        ELSE
            IF StringHas (CMD, 'QSL')  THEN ParameterOkayMode := QSLAndLog;

        ProcessConfigInstructions2 := ParameterOkayMode <> NoParameterOkayMode;
        Exit;
        END;

    IF (ID = 'QSO BEFORE MESSAGE') OR (ID = 'QSO BEFORE CW MESSAGE') THEN
        BEGIN
        QSOBeforeMessage := CMD;
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'QSO BEFORE SSB MESSAGE' THEN
        BEGIN
        QSOBeforePhoneMessage := CMD;
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'QSO NUMBER BY BAND' THEN
        BEGIN
        QSONumberByBand := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'QSX ENABLE' THEN
        BEGIN
        QSXEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'QTC ENABLE' THEN
        BEGIN
        QTCsEnabled := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'QTC EXTRA SPACE' THEN
        BEGIN
        QTCExtraSpace := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'QTC MINUTES' THEN
        BEGIN
        QTCMinutes := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'QTC QRS' THEN
        BEGIN
        QTCQRS := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'QUESTION MARK CHAR' THEN
        BEGIN
        QuestionMarkChar := CMD [1];
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF (ID = 'QUICK QSL KEY') OR (ID = 'QUICK QSL KEY 1') THEN
        BEGIN
        QuickQSLKey1 := CMD [1];
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'QUICK QSL KEY 2' THEN
        BEGIN
        QuickQSLKey2 := CMD [1];
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF (ID = 'QUICK QSL MESSAGE') OR
       (ID = 'QUICK QSL CW MESSAGE') OR
       (ID = 'QUICK QSL MESSAGE 1') OR
       (ID = 'QUICK QSL CW MESSAGE 1') THEN
            BEGIN
            QuickQSLMessage1 := CMD;
            ProcessConfigInstructions2 := True;
            Exit;
            END;

    IF ID = 'QUICK QSL MESSAGE 2' THEN
        BEGIN
        QuickQSLMessage2 := CMD;
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'QUICK QSL SSB MESSAGE' THEN
        BEGIN
        QuickQSLPhoneMessage := CMD;
        ProcessConfigInstructions2 := True;
        END;


    IF ID = 'RADIO ONE BAND OUTPUT PORT' THEN
        BEGIN
        Radio1BandOutputPort := nil;
        IF StringHas(UpperCase(CMD),'PARALLEL') THEN
           BEGIN
              tempstring := wordafter(CMD,'PARALLEL');
              Radio1BandOutputPort := parallelportx(findportx(tempstring));
              if Radio1BandOutputPort = nil then
                 begin
                    Radio1BandOutputPort := parallelportx.create(tempstring);
                    addport(Radio1BandOutputPort);
                 end;
           END;
        ProcessConfigInstructions2 := Radio1BandOutputPort <> nil;
        END;

    IF ID = 'RADIO TWO BAND OUTPUT PORT' THEN
        BEGIN
        Radio2BandOutputPort := nil;
        IF StringHas(UpperCase(CMD),'PARALLEL') THEN
           BEGIN
              tempstring := wordafter(CMD,'PARALLEL');
              Radio2BandOutputPort := parallelportx(findportx(tempstring));
              if Radio2BandOutputPort = nil then
                 begin
                    Radio2BandOutputPort := parallelportx.create(tempstring);
                    addport(Radio2BandOutputPort);
                 end;
           END;
        ProcessConfigInstructions2 := Radio2BandOutputPort <> nil;
        END;

    IF ID = 'RADIO ONE BAUD RATE' THEN
        BEGIN
        Val (CMD, Radio1BaudRate, Result);
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'RADIO TWO BAUD RATE' THEN
        BEGIN
        Val (CMD, Radio2BaudRate, Result);
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'RADIO ONE CONTROL PORT' THEN
        BEGIN
        Radio1ControlPort := nil;
        IF StringHas(UpperCase(CMD),'SERIAL') THEN
           BEGIN
              tempstring := wordafter(CMD,'SERIAL');
              Radio1ControlPort := serialportx(findportx(tempstring));
              if Radio1ControlPort = nil then
                 begin
                    Radio1ControlPort := serialportx.create(tempstring);
                    addport(Radio1ControlPort);
                 end;
           END;

        ProcessConfigInstructions2 := (Radio1ControlPort <> nil) OR (CMD = 'NONE');
        Exit;
        END;

    IF ID = 'RADIO TWO CONTROL PORT' THEN
        BEGIN
        Radio2ControlPort := nil;
        IF StringHas(UpperCase(CMD),'SERIAL') THEN
           BEGIN
              tempstring := wordafter(CMD,'SERIAL');
              Radio2ControlPort := serialportx(findportx(tempstring));
              if Radio2ControlPort = nil then
                 begin
                    Radio2ControlPort := serialportx.create(tempstring);
                    addport(Radio2ControlPort);
                 end;
           END;

        ProcessConfigInstructions2 := (Radio2ControlPort <> nil) OR (CMD = 'NONE');
        Exit;
        END;

    IF ID = 'RADIO ONE ID CHARACTER' THEN
        BEGIN
        CMD := UpperCase (CMD);

        IF CMD = 'NONE' THEN
            Radio1IDCharacter := Chr (0)
        ELSE
            IF CMD <> '' THEN
                BEGIN
                IF (CMD [1] >= 'A') AND (CMD [1] <= 'Z') THEN
                    Radio1IDCharacter := CMD [1];
                END
            ELSE
                Radio1IDCharacter := Chr (0);

        ProcessConfigInstructions2 := True;
        Exit;
        END;



    IF ID = 'RADIO TWO ID CHARACTER' THEN
        BEGIN
        CMD := UpperCase (CMD);

        IF CMD = 'NONE' THEN
            Radio2IDCharacter := Chr (0)
        ELSE
            IF CMD <> '' THEN
                BEGIN
                IF (CMD [1] >= 'A') AND (CMD [1] <= 'Z') THEN
                    Radio2IDCharacter := CMD [1];
                END
            ELSE
                Radio2IDCharacter := Chr (0);

        ProcessConfigInstructions2 := True;
        Exit;
        END;



    IF ID = 'RADIO ONE NAME' THEN
        BEGIN
        WHILE Length (CMD) < 7 DO
            CMD := ' ' + CMD;

        GetRidOfPostcedingSpaces (CMD);

        RadioOneName := CMD;
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'RADIO TWO NAME' THEN
        BEGIN
        WHILE Length (CMD) < 7 DO
            CMD := ' ' + CMD;

        GetRidOfPostcedingSpaces (CMD);

        RadioTwoName := CMD;
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'RADIO ONE RECEIVER ADDRESS' THEN
        BEGIN
        Val (CMD, Radio1ReceiverAddress, Result);
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'RADIO TWO RECEIVER ADDRESS' THEN
        BEGIN
        Val (CMD, Radio2ReceiverAddress, Result);
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'RADIO ONE TRACKING ENABLE' THEN
        BEGIN
        Radio1TrackingEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'RADIO TWO TRACKING ENABLE' THEN
        BEGIN
        Radio2TrackingEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'RADIO ONE TYPE' THEN
        BEGIN
        Radio1Type := NoInterfacedRadio;

        Cmd := UpperCase (Cmd);

        IF Pos (CMD, '-') > 0 THEN Delete (CMD, Pos (CMD, '-'), 1);

        { No radios ending with A }

        IF Copy (CMD, Length (CMD), 1) = 'A' THEN
            Delete (CMD, Length (CMD), 1);

        IF Copy (CMD, 1, 2) = 'JS' THEN
            BEGIN
            Radio1Type := JST245;
            Radio1ControlDelay := 1;
            END;

        IF Copy (CMD, 1, 2) = 'K2' THEN
            BEGIN
            {Radio1Type := K2;}
            Radio1Type := TS850; {KK1L:6.73 missing "OR K2"s in TS850 statements in LOGSUBS2. Easier fix!}
            Radio1ControlDelay := 1;
            END;

        IF Copy (CMD, 1, 2) = 'TS' THEN
            BEGIN
            Radio1Type := TS850;
            Radio1ControlDelay := 1;
            END;

        IF Copy (CMD, 1, 3) = 'FT1' THEN
            BEGIN
            IF StringHas (CMD, 'MP') THEN
                Radio1Type := FT1000MP
            ELSE
                Radio1Type := FT1000;

            Radio1ControlDelay := 1;
            END;

        IF (CMD = 'FT840') OR (CMD = 'FT890') OR (CMD = 'FT900') THEN
            BEGIN
            Radio1Type := FT890;

            Radio1ControlDelay := 1;
            END;

        IF CMD = 'FT920' THEN
            BEGIN
            Radio1Type := FT920;
            Radio1ControlDelay := 1;
            END;

        IF CMD = 'FT100' THEN
            BEGIN
            Radio1Type := FT100;
            Radio1ControlDelay := 1;
            END;

        IF (CMD = 'FT817') OR (CMD = 'FT897') THEN {KK1L: 6.73 Added FT897 support. Reports say FT817 works well for it.}
            BEGIN
            Radio1Type := FT817;
            Radio1ControlDelay := 1;
            END;

        IF CMD = 'FT847' THEN
            BEGIN
            Radio1Type := FT847;
            Radio1ControlDelay := 1;
            END;

        IF CMD = 'FT990' THEN
            BEGIN
            Radio1Type := FT990;
            Radio1ControlDelay := 1;
            END;

        IF Copy (CMD, 1, 2) = 'IC' THEN
            BEGIN
            IF CMD = 'IC706' THEN
                BEGIN
                Radio1Type := IC706;
                Radio1ReceiverAddress := $48;
                END;

            IF CMD = 'IC706II' THEN
                BEGIN
                Radio1Type := IC706II;
                Radio1ReceiverAddress := $4E;
                END;

            IF CMD = 'IC706IIG' THEN
                BEGIN
                Radio1Type := IC706IIG;
                Radio1ReceiverAddress := $58;
                END;

            IF CMD = 'IC707' THEN
                BEGIN
                Radio1Type := IC707;
                Radio1ReceiverAddress := $3E;
                END;

            IF CMD = 'IC725' THEN
                BEGIN
                Radio1Type := IC725;
                Radio1ReceiverAddress := $28;
                END;

            IF CMD = 'IC726' THEN
                BEGIN
                Radio1Type := IC726;
                Radio1ReceiverAddress := $30;
                END;

            IF CMD = 'IC728' THEN
                BEGIN
                Radio1Type := IC728;
                Radio1ReceiverAddress := $38;
                END;

            IF CMD = 'IC729' THEN
                BEGIN
                Radio1Type := IC729;
                Radio1ReceiverAddress := $3A;
                END;

            IF CMD = 'IC735' THEN
                BEGIN
                Radio1Type := IC735;
                Radio1ReceiverAddress := $04;
                END;

            IF CMD = 'IC736' THEN
                BEGIN
                Radio1Type := IC736;
                Radio1ReceiverAddress := $40;
                END;

            IF CMD = 'IC737' THEN
                BEGIN
                Radio1Type := IC737;
                Radio1ReceiverAddress := $3C;
                END;

            IF CMD = 'IC738' THEN
                BEGIN
                Radio1Type := IC738;
                Radio1ReceiverAddress := $44;
                END;

            IF CMD = 'IC746' THEN
                BEGIN
                Radio1Type := IC746;
                Radio1ReceiverAddress := $56;
                END;

            IF CMD = 'IC746PRO' THEN
                BEGIN
                Radio1Type := IC746PRO;
                Radio1ReceiverAddress := $56;
                END;

            IF CMD = 'IC756' THEN
                BEGIN
                Radio1Type := IC756;
                Radio1ReceiverAddress := $50;
                END;

            IF CMD = 'IC756PRO' THEN
                BEGIN
                Radio1Type := IC756PRO;
                Radio1ReceiverAddress := $5C;
                END;

            IF CMD = 'IC756PROII' THEN
                BEGIN
                Radio1Type := IC756PROII;
                Radio1ReceiverAddress := $64;
                END;

            IF CMD = 'IC761' THEN
                BEGIN
                Radio1Type := IC761;
                Radio1ReceiverAddress := $1E;
                END;

            IF CMD = 'IC765' THEN
                BEGIN
                Radio1Type := IC765;
                Radio1ReceiverAddress := $2C;
                END;

            IF CMD = 'IC775' THEN
                BEGIN
                Radio1Type := IC775;
                Radio1ReceiverAddress := $46;
                END;

            IF CMD = 'IC781' THEN
                BEGIN
                Radio1Type := IC781;
                Radio1ReceiverAddress := $26;
                END;

            Radio1ControlDelay := 8;
            END;

        {KK1L: 6.73 Added direct TenTec support}

        IF StringHas (CMD, 'OMNI') THEN
            BEGIN
            Radio1Type := OMNI6;
            Radio1ReceiverAddress := $04;
            END;

        IF CMD = 'ORION' THEN
            BEGIN
            Radio1Type := Orion;
            Radio1BaudRate := 57600;
            END;

        IF CMD = 'ARGO' THEN
            BEGIN
            Radio1Type := ARGO;
            Radio1ReceiverAddress := $04;
            END;

        ProcessConfigInstructions2 := (Radio1Type <> NoInterfacedRadio) OR
                                      (CMD = 'NONE');
        END;

    IF ID = 'RADIO TWO TYPE' THEN
        BEGIN
        Radio2Type := NoInterfacedRadio;

        CMD := UpperCase (CMD);

        IF Pos (CMD, '-') > 0 THEN Delete (CMD, Pos (CMD, '-'), 1);

        { No radios ending with A }

        IF Copy (CMD, Length (CMD), 1) = 'A' THEN
            Delete (CMD, Length (CMD), 1);

        IF Copy (CMD, 1, 2) = 'JS' THEN
            BEGIN
            Radio2Type := JST245;
            Radio2ControlDelay := 1;
            END;

        IF Copy (CMD, 1, 2) = 'K2' THEN
            BEGIN
            {Radio2Type := K2;}
            Radio2Type := TS850; {KK1L:6.73 missing "OR K2"s in TS850 statements in LOGSUBS2. Easier fix!}
            Radio2ControlDelay := 1;
            END;

        IF Copy (CMD, 1, 2) = 'TS' THEN
            BEGIN
            Radio2Type := TS850;
            Radio2ControlDelay := 1;
            END;

        IF Copy (CMD, 1, 3) = 'FT1' THEN
            BEGIN
            IF StringHas (CMD, 'MP') THEN
                Radio2Type := FT1000MP
            ELSE
                Radio2Type := FT1000;

            Radio2ControlDelay := 1;
            END;

        IF (CMD = 'FT840') OR (CMD = 'FT890') OR (CMD = 'FT900') THEN
            BEGIN
            Radio2Type := FT890;
            Radio2ControlDelay := 1;
            END;

        IF CMD = 'FT920' THEN
            BEGIN
            Radio2Type := FT920;
            Radio2ControlDelay := 1;
            END;

        IF (CMD = 'FT817') OR (CMD = 'FT897') THEN {KK1L: 6.73 Added FT897 support. Reports say FT817 works well for it.}
            BEGIN
            Radio2Type := FT817;
            Radio2ControlDelay := 1;
            END;

        IF CMD = 'FT847' THEN
            BEGIN
            Radio2Type := FT847;
            Radio2ControlDelay := 1;
            END;

        IF CMD = 'FT990' THEN
            BEGIN
            Radio2Type := FT990;
            Radio2ControlDelay := 1;
            END;

        IF Copy (CMD, 1, 2) = 'IC' THEN
            BEGIN
            IF CMD = 'IC706' THEN
                BEGIN
                Radio2Type := IC706;
                Radio2ReceiverAddress := $48;
                END;

            IF CMD = 'IC706II' THEN
                BEGIN
                Radio2Type := IC706II;
                Radio2ReceiverAddress := $4E;
                END;

            IF CMD = 'IC706IIG' THEN
                BEGIN
                Radio2Type := IC706IIG;
                Radio2ReceiverAddress := $58;
                END;

            IF CMD = 'IC707' THEN
                BEGIN
                Radio2Type := IC707;
                Radio2ReceiverAddress := $3E;
                END;

            IF CMD = 'IC725' THEN
                BEGIN
                Radio2Type := IC725;
                Radio2ReceiverAddress := $28;
                END;

            IF CMD = 'IC726' THEN
                BEGIN
                Radio2Type := IC726;
                Radio2ReceiverAddress := $30;
                END;

            IF CMD = 'IC728' THEN
                BEGIN
                Radio2Type := IC728;
                Radio2ReceiverAddress := $38;
                END;

            IF CMD = 'IC729' THEN
                BEGIN
                Radio2Type := IC729;
                Radio2ReceiverAddress := $3A;
                END;

            IF CMD = 'IC735' THEN
                BEGIN
                Radio2Type := IC735;
                Radio2ReceiverAddress := $04;
                END;

            IF CMD = 'IC736' THEN
                BEGIN
                Radio2Type := IC736;
                Radio2ReceiverAddress := $40;
                END;

            IF CMD = 'IC737' THEN
                BEGIN
                Radio2Type := IC737;
                Radio2ReceiverAddress := $3C;
                END;

            IF CMD = 'IC738' THEN
                BEGIN
                Radio2Type := IC738;
                Radio2ReceiverAddress := $44;
                END;

            IF CMD = 'IC746' THEN
                BEGIN
                Radio2Type := IC746;
                Radio2ReceiverAddress := $56;
                END;

            IF CMD = 'IC746PRO' THEN
                BEGIN
                Radio2Type := IC746PRO;
                Radio2ReceiverAddress := $56;
                END;

            IF CMD = 'IC756' THEN
                BEGIN
                Radio2Type := IC756;
                Radio2ReceiverAddress := $50;
                END;

            IF CMD = 'IC756PRO' THEN
                BEGIN
                Radio2Type := IC756PRO;
                Radio2ReceiverAddress := $5C;
                END;

            IF CMD = 'IC756PROII' THEN
                BEGIN
                Radio2Type := IC756PROII;
                Radio2ReceiverAddress := $64;
                END;

            IF CMD = 'IC761' THEN
                BEGIN
                Radio2Type := IC761;
                Radio2ReceiverAddress := $1E;
                END;

            IF CMD = 'IC765' THEN
                BEGIN
                Radio2Type := IC765;
                Radio2ReceiverAddress := $2C;
                END;

            IF CMD = 'IC775' THEN
                BEGIN
                Radio2Type := IC775;
                Radio2ReceiverAddress := $46;
                END;

            IF CMD = 'IC781' THEN
                BEGIN
                Radio2Type := IC781;
                Radio2ReceiverAddress := $26;
                END;


            Radio2ControlDelay := 8;
            END;

        {KK1L: 6.73 Added direct TenTec support}

        IF StringHas (CMD, 'OMNI') THEN
            BEGIN
            Radio2Type := OMNI6;
            Radio2ReceiverAddress := $04;
            END;

        IF CMD = 'ORION' THEN
            BEGIN
            Radio2Type := Orion;
            Radio2BaudRate := 57600;
            END;

        IF CMD = 'ARGO' THEN
            BEGIN
            Radio2Type := ARGO;
            Radio2ReceiverAddress := $04;
            END;

        ProcessConfigInstructions2 := (Radio2Type <> NoInterfacedRadio) OR
                                    (CMD = 'NONE');
        END;


    IF ID = 'RADIO ONE UPDATE SECONDS' THEN
        BEGIN
        Val (CMD, Radio1UpdateSeconds, Result);
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'RADIO TWO UPDATE SECONDS' THEN
        BEGIN
        Val (CMD, Radio2UpdateSeconds, Result);
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'RADIUS OF EARTH' THEN
        BEGIN
        Val (CMD, RadiusOfEarth, Result);
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'RANDOM CQ MODE' THEN
        BEGIN
        RandomCQMode := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'RATE DISPLAY' THEN
        BEGIN
        IF UpperCase (CMD) = 'QSO POINTS' THEN RateDisplay := Points;
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'RELAY CONTROL PORT' THEN
        BEGIN
        IF StringHas(UpperCase(CMD),'SERIAL') THEN
           BEGIN
              tempstring := wordafter(CMD,'SERIAL');
              RelayControlPort := findportx(tempstring);
              if RelayControlPort = nil then
                 begin
                    RelayControlPort := serialportx.create(tempstring);
                    addport(RelayControlPort);
                 end;
              if StringHas (UpperCase(CMD), 'INVERT') then
                 serialportx(RelayControlPort).invert(true);
              RelayControlPort.key(false);
           END
        ELSE IF StringHas(UpperCase(CMD),'PARALLEL') THEN
           BEGIN
              tempstring := wordafter(CMD,'PARALLEL');
              RelayControlPort := parallelportx(findportx(tempstring));
              if RelayControlPort = nil then
                 begin
                    RelayControlPort := parallelportx.create(tempstring);
                    addport(RelayControlPort);
                 end;
           END;
        ProcessConfigInstructions2 := (RelayControlPort <> nil) OR (CMD = 'NONE');
        Exit;
        END;

    IF ID = 'RECEIVER ADDRESS' THEN
        BEGIN
        Val (CMD, Radio1ReceiverAddress, Result);
        Radio2ReceiverAddress := Radio1ReceiverAddress;
        ProcessConfigInstructions2 := Result = 0;
        Exit;
        END;

    IF ID = 'REMAINING MULT DISPLAY MODE' THEN
        BEGIN
        RemainingMultDisplayMode := NoRemainingMults;

        CMD := UpperCase (CMD);

        IF CMD = 'ERASE'   THEN RemainingMultDisplayMode := Erase;
        IF CMD = 'HILIGHT' THEN RemainingMultDisplayMode := HiLight;

        ProcessConfigInstructions2 := (RemainingMultDisplayMode <> NoRemainingMults) OR
                                    (CMD = 'NONE');

        Exit;
        END;

    IF ID = 'REMINDER' THEN
        BEGIN
        IF NumberReminderRecords >= MaximumReminderRecords THEN
            BEGIN
            Write ('Maximum number of reminders exceeded!!');
            Exit;
            END;

        IF NumberReminderRecords = 0 THEN New (Reminders);

        Reminders^ [NumberReminderRecords].DateString := '';
        Reminders^ [NumberReminderRecords].DayString  := '';
        Reminders^ [NumberReminderRecords].Alarm := False;

        CMD := UpperCase (CMD);

        TimeString := Copy (CMD, 1, 4);

        IF NOT StringIsAllNumbers (TimeString) THEN
            BEGIN
            Write ('Invalid reminder time!!');
            Exit;
            END;

        Val (TimeString, Reminders^ [NumberReminderRecords].Time, Result);

        DateString := BracketedString (CMD, ' ON ', '');

        IF StringHas (DateString, 'ALARM') THEN
            BEGIN
            Reminders^ [NumberReminderRecords].Alarm := True;
            DateString := BracketedString (DateString, '', 'ALARM');
            END;

        GetRidOfPostcedingSpaces (DateString);

        IF StringHas (DateString, '-') THEN
            BEGIN
            CASE Length (DateString) OF
                8: IF (DateString [2] <> '-') OR (DateString [6] <> '-') THEN
                       BEGIN
                       Write ('Invalid reminder date!!');
                       Exit;
                       END
                   ELSE
                       DateString := '0' + DateString;

                9: IF (DateString [3] <> '-') OR (DateString [7] <> '-') THEN
                       BEGIN
                       Write ('Invalid reminder date!!');
                       Exit;
                       END;

                ELSE
                    Write ('Invalid reminder date!!');
                END;
            Reminders^ [NumberReminderRecords].DateString := DateString;
            END
        ELSE
            BEGIN
            DayString := Copy (DateString, Length (DateString) - 2, 3);
            IF (DayString <> 'DAY') AND (DayString <> 'ALL') THEN
                BEGIN
                Write ('Invalid reminder date!!');
                Exit;
                END;

            Reminders^ [NumberReminderRecords].DayString := DateString;
            END;

        ReadLn (ConfigFileRead, Reminders^ [NumberReminderRecords].Message);
        Inc (NumberReminderRecords);
        ProcessConfigInstructions2 := True;
        Exit;
        END;



    IF (ID = 'REPEAT S&P EXCHANGE') OR (ID = 'REPEAT S&P CW EXCHANGE') THEN
        BEGIN
        RepeatSearchAndPounceExchange := CMD;
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'REPEAT S&P SSB EXCHANGE' THEN
        BEGIN
        RepeatSearchAndPouncePhoneExchange := CMD;
        ProcessConfigInstructions2 := True;
        Exit;
        END;

    IF ID = 'ROTATOR PORT' THEN
        BEGIN
        ActiveRotatorPort := nil;
        IF StringHas(UpperCase(CMD),'SERIAL') THEN
           BEGIN
              tempstring := wordafter(CMD,'SERIAL');
              ActiveRotatorPort := serialportx(findportx(tempstring));
              if ActiveRotatorPort = nil then
                 begin
                    ActiveRotatorPort := serialportx.create(tempstring);
                    addport(ActiveRotatorPort);
                 end;
           END;

        ProcessConfigInstructions2 := (ActiveRotatorPort <> nil) OR (CMD = 'NONE');
        Exit;
        END;

    IF ID = 'ROTATOR TYPE' THEN
        BEGIN
        ActiveRotatorType := NoRotator;

        Cmd := UpperCase (Cmd);

        IF StringHas (CMD, 'DCU') THEN ActiveRotatorType := DCU1Rotator;
        IF StringHas (CMD, 'ORI') THEN ActiveRotatorType := OrionRotator;
        IF StringHas (CMD, 'YAE') THEN ActiveRotatorType := YaesuRotator; {KK1L: 6.71}

        ProcessConfigInstructions2 := (ActiveRotatorType <> NoRotator) AND (CMD <> 'NONE');
        Exit;
        END;

    IF ID = 'RTTY RECEIVE STRING' THEN
        BEGIN
        RTTYReceiveString := CMD;
        ProcessConfigInstructions2 := True;
        SniffOutControlCharacters (RTTYReceiveString);
        Exit;
        END;

    IF ID = 'RTTY SEND STRING' THEN
        BEGIN
        RTTYSendString := CMD;
        ProcessConfigInstructions2 := True;
        SniffOutControlCharacters (RTTYSendString);
        Exit;
        END;

    IF ID = 'RTTY PORT' THEN
        BEGIN
        ActiveRTTYPort := nil;
        IF StringHas(UpperCase(CMD),'SERIAL') THEN
           BEGIN
              tempstring := wordafter(CMD,'SERIAL');
              ActiveRTTYPort := serialportx(findportx(tempstring));
              if ActiveRTTYPort = nil then
                 begin
                    ActiveRTTYPort := serialportx.create(tempstring);
                    addport(ActiveRTTYPort);
                 end;
           END;

        ProcessConfigInstructions2 := (ActiveRTTYPort <> nil) OR (CMD = 'NONE');
        Exit;
        END;

    END;



FUNCTION ProcessConfigInstructions3 (ID: Str80; CMD: STRING): BOOLEAN;

VAR Result,tempint: INTEGER;
    tempstring: string;

    BEGIN
    IF (ID = 'S&P EXCHANGE') OR (ID = 'S&P CW EXCHANGE') THEN
        BEGIN
        SearchAndPounceExchange := CMD;
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'S&P SSB EXCHANGE' THEN
        BEGIN
        SearchAndPouncePhoneExchange := CMD;
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'SAY HI ENABLE' THEN
        BEGIN
        SayHiEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;


    IF ID = 'SAY HI RATE CUTOFF' THEN
        BEGIN
        Val (CMD, SayHiRateCutOff, Result);
        ProcessConfigInstructions3 := Result = 0;
        Exit;
        END;

    IF ID = 'SCP COUNTRY STRING' THEN
        BEGIN
        CD.CountryString := UpperCase (CMD);

        IF CD.CountryString <> '' THEN
            IF Copy (CD.CountryString, Length (CD.CountryString), 1) <> ',' THEN
                CD.CountryString := CD.CountryString + ',';

        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'SCP MINIMUM LETTERS' THEN {KK1L: 6.68 0 for WRTC2002}
        IF (NOT WRTC2002) THEN
            BEGIN
            Val (CMD, SCPMinimumLetters, Result);
            ProcessConfigInstructions3 := Result = 0;
            Exit;
            END
        ELSE
            BEGIN
            SCPMinimumLetters := 0;
            ProcessConfigInstructions3 := True;
            END;

    IF ID = 'SEND ALT-D SPOTS TO PACKET' THEN
        BEGIN
        SendAltDSpotsToPacket := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'SEND COMPLETE FOUR LETTER CALL' THEN
        BEGIN
        SendCompleteFourLetterCall := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'SEND QSO IMMEDIATELY' THEN
        BEGIN
        SendQSOImmediately := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'STEREO CONTROL PIN' THEN {KK1L: 6.71}
       BEGIN
       Val (CMD, StereoControlPin, Result);
       IF (StereoControlPin <> 5) AND (StereoControlPin <> 9) THEN StereoControlPin := 9;
       ProcessConfigInstructions3 := Result = 0;
       Exit;
       END;

    IF ID = 'STEREO CONTROL PORT' THEN {KK1L: 6.71}
        BEGIN
        ActiveStereoPort := nil;
        IF StringHas(UpperCase(CMD),'PARALLEL') THEN
           BEGIN
              tempstring := wordafter(CMD,'PARALLEL');
              ActiveStereoPort := parallelportx(findportx(tempstring));
              if ActiveStereoPort = nil then
                 begin
                    ActiveStereoPort := parallelportx.create(tempstring);
                    addport(ActiveStereoPort);
                 end;
           END;
        ProcessConfigInstructions3 := (ActiveStereoPort <> nil) OR (CMD = 'NONE');
        END;

    IF ID = 'STEREO PIN HIGH' THEN {KK1L: 6.71}
        BEGIN
        StereoPinState := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'SHIFT KEY ENABLE' THEN
        BEGIN
        If UpCase (CMD [1]) = 'T' then
           begin
           ShiftKeyEnable := Shift;
           shiftchange(1);
           end
        else if UpCase (CMD [1]) = 'A' then
           begin
           ShiftKeyEnable := AltShift;
           shiftchange(2);
           end
        else
           begin
           ShiftKeyEnable := None;
           shiftchange(0);
           end;
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'SHORT INTEGERS' THEN
        BEGIN
        ShortIntegers := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'SHORT 0' THEN
        BEGIN
        Short0 := CMD [1];
        ProcessConfigInstructions3 := True;
        END;

    IF ID = 'SHORT 1' THEN
        BEGIN
        Short1 := CMD [1];
        ProcessConfigInstructions3 := True;
        END;

    IF ID = 'SHORT 2' THEN
        BEGIN
        Short2 := CMD [1];
        ProcessConfigInstructions3 := True;
        END;

    IF ID = 'SHORT 9' THEN
        BEGIN
        Short9 := CMD [1];
        ProcessConfigInstructions3 := True;
        END;

    IF ID = 'SHOW SEARCH AND POUNCE' THEN
        BEGIN
        ShowSearchAndPounce := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'SIMULATOR ENABLE' THEN
        BEGIN
        IF UpCase (CMD [1]) = 'T' THEN
            DDXState := StandBy
        ELSE
            DDXState := Off;

        ProcessConfigInstructions3 := True;
        END;

    IF ID = 'SINGLE RADIO MODE' THEN
        BEGIN
        SingleRadioMode := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'SKIP ACTIVE BAND' THEN
        BEGIN
        SkipActiveBand := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'SLASH MARK CHAR' THEN
        BEGIN
        SlashMarkChar := CMD [1];
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'SOUNDCARD DEVICE' THEN
        BEGIN
        sounddevice := CMD;
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'SPACE BAR DUPE CHECK ENABLE' THEN
        BEGIN
        SpaceBarDupeCheckEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'SPRINT QSY RULE' THEN
        BEGIN
        SprintQSYRule := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF (ID = 'START SENDING NOW KEY') OR (ID = 'START SENDING CALL KEY') THEN
        BEGIN
        IF UpperCase (CMD) = 'SPACE' THEN
            StartSendingNowKey := ' '
        ELSE
            StartSendingNowKey := CMD [1];
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'SWAP PACKET SPOT RADIOS' THEN
        BEGIN
        SwapPacketSpotRadios := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'SWAP PADDLES' THEN
        BEGIN
        CPUKeyer.SetSwapPaddles(UpCase (CMD [1]) = 'T');
        WinKey.SetSwapPaddles(UpCase (CMD [1]) = 'T');
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'SWAP RADIO RELAY SENSE' THEN
        BEGIN
        SwapRadioRelaySense := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'TAB MODE' THEN
        BEGIN
        CMD := UpperCase (CMD);

        IF CMD = 'NORMAL'   THEN TabMode := NormalTabMode;
        IF CMD = 'CONTROLF' THEN TabMode := ControlFTabMode;

        ProcessConfigInstructions3 := True;
        Exit;
        END;


    IF ID = 'TAIL END KEY' THEN
        BEGIN
        TailEndKey := CMD [1];
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF (ID = 'TAIL END MESSAGE') OR (ID = 'TAIL END CW MESSAGE') THEN
        BEGIN
        TailEndMessage := CMD;
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'TAIL END SSB MESSAGE' THEN
        BEGIN
        TailEndPhoneMessage := CMD;
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'TEN MINUTE RULE' THEN
        BEGIN
        CMD := UpperCase (CMD);
        TenMinuteRule := NoTenMinuteRule;

        IF CMD = 'TIME OF FIRST QSO' THEN TenMinuteRule := TimeOfFirstQSO;

        ProcessConfigInstructions3 := (TenMinuteRule <> NoTenMinuteRule) OR
                                    (CMD = 'NONE');
        Exit;
        END;

    IF ID = 'TOTAL SCORE MESSAGE' THEN
        BEGIN
        IF NumberTotalScoreMessages < 10 THEN
            BEGIN
            Val (CMD, TotalScoreMessages [NumberTotalScoreMessages].Score, Result);
            ReadLn (ConfigFileRead, TotalScoreMessages [NumberTotalScoreMessages].MessageString);
            Inc (NumberTotalScoreMessages);
            ProcessConfigInstructions3 := Result = 0;
            END
        ELSE
            Write ('Too many TOTAL SCORE MESSAGEs!!');
        Exit;
        END;

    IF (ID = 'TUNE DUPE CHECK ENABLE') OR (ID = 'TUNE ALT-D ENABLE') THEN {KK1L: 6.73}
        BEGIN
        TuneDupeCheckEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'TUNE WITH DITS' THEN
        BEGIN
        CPUKeyer.SetTuneWithDits(UpCase (CMD [1]) = 'T');
        Winkey.SetTuneWithDits(UpCase (CMD [1]) = 'T');
        ProcessConfigInstructions3 := True;
        Exit;
        END;


    IF ID = 'TWO RADIO MODE' THEN
        BEGIN
        IF UpCase (CMD [1]) = 'T' THEN
            TwoRadioState := Idle
        ELSE
            TwoRadioState := TwoRadiosDisabled;
        ProcessConfigInstructions3 := True;
        Exit;
        END;



    IF UpperCase (ID) = 'UPDATE RESTART FILE ENABLE' THEN
        BEGIN
        UpdateRestartFileEnable := Upcase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF UpperCase (ID) = 'USE BIOS KEY CALLS' THEN
        BEGIN
        UseBIOSKeyCalls := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF UpperCase (ID) = 'USER INFO SHOWN' THEN
        BEGIN
        UserInfoShown := NoUserInfo;
        CMD := UpperCase (CMD);

        IF CMD = 'NONE'    THEN UserInfoShown := NoUserInfo;
        IF CMD = 'NAME'    THEN UserInfoShown := NameInfo;
        IF CMD = 'QTH'     THEN UserInfoShown := QTHInfo;
        IF CMD = 'SECTION' THEN UserInfoShown := SectionInfo;
        IF CMD = 'GRID'    THEN UserInfoShown := GridInfo;
        IF CMD = 'CUSTOM'  THEN UserInfoShown := CustomInfo;

        IF (CMD = 'OLD CALL')      OR (CMD = 'OLDCALL')      THEN UserInfoShown := OldCallInfo;
        IF (CMD = 'CHECK SECTION') OR (CMD = 'CHECKSECTION') THEN UserInfoShown := CheckSectionInfo;
        IF (CMD = 'CQ ZONE')       OR (CMD = 'CQZONE')       THEN UserInfoShown := CQZoneInfo;
        IF (CMD = 'ITU ZONE')      OR (CMD = 'ITUZONE')      THEN UserInfoShown := ITUZoneInfo;
        IF (CMD = 'FOC NUMBER')    OR (CMD = 'FOCNUMBER')    THEN UserInfoShown := FocInfo;

        IF (CMD = 'USER 1') OR (CMD = 'USER1') THEN UserInfoShown := User1Info;
        IF (CMD = 'USER 2') OR (CMD = 'USER2') THEN UserInfoShown := User2Info;
        IF (CMD = 'USER 3') OR (CMD = 'USER3') THEN UserInfoShown := User3Info;
        IF (CMD = 'USER 4') OR (CMD = 'USER4') THEN UserInfoShown := User4Info;
        IF (CMD = 'USER 5') OR (CMD = 'USER5') THEN UserInfoShown := User5Info;

        ProcessConfigInstructions3 := (UserInfoShown <> NoUserInfo) OR (CMD = 'NONE');
        Exit;
        END;

    IF UpperCase (ID) = 'VGA DISPLAY ENABLE' THEN
        BEGIN
        VGADisplayEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF UpperCase (ID) = 'VHF BAND ENABLE' THEN
        BEGIN
        VHFBandsEnabled := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'VIDEO GAME LENGTH' THEN
        BEGIN
        Val (CMD, VideoGameLength, Result);
        ProcessConfigInstructions3 := Result = 0;
        Exit;
        END;

    IF UpperCase (ID) = 'VISIBLE DUPESHEET' THEN
        BEGIN
        VisibleDupesheetEnable := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF UpperCase (ID) = 'WAKE UP TIME OUT' THEN
        BEGIN
        Val (CMD, WakeUpTimeOut, Result);
        ProcessConfigInstructions3 := Result = 0;
        Exit;
        END;

    IF ID = 'WARC BAND ENABLE' THEN
        BEGIN
        WARCBandsEnabled := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'WAIT FOR STRENGTH' THEN
        BEGIN
        WaitForStrength := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'WEIGHT' THEN
        BEGIN
        Val (CMD, tempint, Result);
        CPUKeyer.setWeight(tempint);
        Winkey.setWeight(tempint);
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'WIDE FREQUENCY DISPLAY' THEN
        BEGIN
        WideFreqDisplay := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'WILDCARD PARTIALS' THEN
        BEGIN
        WildCardPartials := UpCase (CMD [1]) = 'T';
        ProcessConfigInstructions3 := True;
        Exit;
        END;

    IF ID = 'YAESU RESPONSE TIMEOUT' THEN
        BEGIN
        Val (CMD, YaesuResponseTimeout, Result);
        ProcessConfigInstructions3 := Result = 0;
        Exit;
        END;
    END;



FUNCTION ProcessConfigInstruction (FileString: STRING; VAR FirstCommand: BOOLEAN): BOOLEAN;

VAR Count, Result, Memory: INTEGER;
    Directory, ID: Str80;
    CMD: STRING;
    TimeString, DateString, DayString: Str20;
    TempFreq: LONGINT;
    TempBand: BandType;
    TempMode: ModeType;

    BEGIN
    FOR Count := 1 TO Length (FileString) DO
        IF FileString [Count] = TabKey THEN FileString [Count] := ' ';

    IF (Copy (FileString, 1, 1) = ';') OR
       (Copy (FileString, 1, 2) = ' ;') THEN
           BEGIN
           ProcessConfigInstruction := True;
           Exit;
           END;

    IF StringHas (FileString, '  ;') THEN
        FileString := PrecedingString (FileString, ';');

    GetRidOfPrecedingSpaces (FileString);

    IF FileString = '' THEN
        BEGIN
        ProcessConfigInstruction := True;
        Exit;
        END;

    ID  := UpperCase (PrecedingString  (FileString, '='));
    CMD := PostcedingString (FileString, '=');

    GetRidOfPrecedingSpaces  (ID);
    GetRidOfPrecedingSpaces  (CMD);
    GetRidOfPostcedingSpaces (ID);
    GetRidOfPostcedingSpaces (CMD);

    ProcessConfigInstruction := False;

    IF FirstCommand THEN
        IF ID <> '' THEN
            IF ID <> 'MY CALL' THEN
                BEGIN
                Write ('The first command in config file must be the MY CALL statement!!');
                Exit;
                END
            ELSE
                FirstCommand := False;

    IF ValidColorCommand (ID, CMD) THEN
        BEGIN
        ProcessConfigInstruction := True;
        Exit;
        END;

    IF ProcessPostConfigInstruction (ID, CMD) THEN
        BEGIN
        ProcessConfigInstruction := True;
        Exit;
        END;

    IF ProcessConfigInstructions1 (ID, CMD) THEN
        BEGIN
        ProcessConfigInstruction := True;
        Exit;
        END;

    IF ProcessConfigInstructions2 (ID, CMD) THEN
        BEGIN
        ProcessConfigInstruction := True;
        Exit;
        END;

    IF ProcessConfigInstructions3 (ID, CMD) THEN
        BEGIN
        ProcessConfigInstruction := True;
        Exit;
        END;

    IF ID = '' THEN ProcessConfigInstruction := True;
    END;



    BEGIN
    END.

