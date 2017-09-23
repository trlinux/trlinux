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

UNIT JCtrl2;

{$O+}

INTERFACE

USES Tree, LogStuff, LogGrid, LogSCP, LogCW, LogWind, LogDupe, ZoneCont,
     LogCfg, LogDom, LogDVP, Country9, LogEdit, trCrt, LogK1EA, DOS, LogHelp,
     SlowTree, LogWAE, LogPack, LogDDX, JCtrl1, K1EANet;

PROCEDURE DisplayStatus (FirstEntryDisplayed: MenuEntryType; ActiveEntry: MenuEntryType);
FUNCTION  GetActiveLineFromEntryString (EntryString: Str80): MenuEntryType;
FUNCTION  ParameterToString (Line: MenuEntryType): Str80;
PROCEDURE ProcessConfigurationInput;
PROCEDURE ProcessInput (ActiveLine: MenuEntryType);
PROCEDURE WriteParameterToLOGCFGFile (FileName: Str80; Line: MenuEntryType);

IMPLEMENTATION

uses linuxsound,keycode,beep,foot,keyers,xkb,so2r;

PROCEDURE DisplayStatus (FirstEntryDisplayed: MenuEntryType; ActiveEntry: MenuEntryType);

VAR LastEntry, Entry: MenuEntryType;
    Line: WORD;

    BEGIN
    ClrScr;
    LastEntry := FirstEntryDisplayed;

    IF LastEntry < LastMenuEntry THEN
        FOR Line := 1 TO 16 DO
            BEGIN
            IF LastEntry = Pred (LastMenuEntry) THEN Break;
            LastEntry := Succ (LastEntry);
            END;

    FOR Entry := FirstEntryDisplayed TO LastEntry DO
        BEGIN
        IF Entry > FirstEntryDisplayed THEN WriteLn;
        DisplayStatusLine (Entry, Entry = ActiveEntry);

        WHILE WhereX < 40 DO Write (' ');

        DisplayInfoLine   (Entry, Entry = ActiveEntry);
        END;
    END;



PROCEDURE ProcessInput (ActiveLine: MenuEntryType);

VAR TempHour, TempMinute, TempInt, Result: INTEGER;
    TempReal: REAL;
    TempString: Str20;
    TempChar: CHAR;
    TempLongInt: LONGINT;

    BEGIN
    Changed [ActiveLine] := True;

    CASE ActiveLine OF
      DMF, DVK, MCL, MCN, MCU, MZN, VER:
           BEGIN
           Tone.DoABeep (Single);
           QuickDisplay ('You can only change this in your config file.');
           Wait (3000);
           RemoveWindow (QuickCommandWindow);
           END;

      ABE: AltDBufferEnable       := NOT AltDBufferEnable;
      ABC: AlwaysCallBlindCQ      := NOT AlwaysCallBlindCQ;
      ACC: AllCWMessagesChainable := NOT AllCWMessagesChainable;
      AFF: AskForFrequencies      := NOT AskForFrequencies;
      AIO: AskIfContestOver       := NOT AskIfContestOver;
      ACT: AutoCallTerminate      := NOT AutoCallTerminate;
      ADP: AutoDisplayDupeQSO     := NOT AutoDisplayDupeQSO;
      ADE: AutoDupeEnableCQ       := NOT AutoDupeEnableCQ;
      ADS: AutoDupeEnableSAndP    := NOT AutoDupeEnableSAndP;

      AQI: BEGIN
           Inc (AutoQSLInterval);
           IF AutoQSLInterval > 6 THEN
               AutoQSLInterval := 0;

           AutoQSLCount := AutoQSLInterval;
           END;

      AQD: AutoQSONumberDecrement := NOT AutoQSONumberDecrement;
      ASP: AutoSAPEnable          := NOT AutoSAPEnable;
      ASR: BEGIN {KK1L: 6.72}
           TempInt := QuickEditInteger ('Enter new Auto SAP Enable sensitivity in Hz/sec) : ', 5);
           IF (TempInt > 9) AND (TempInt < 10001)THEN
               AutoSAPEnableRate := TempInt;
           END;
      ARC: AutoReturnToCQMode     := NOT AutoReturnToCQMode;

      ASC: BEGIN
           Inc (AutoSendCharacterCount);

           IF AutoSendCharacterCount > 6 THEN
               BEGIN
               AutoSendCharacterCount := 0;
               AutoSendEnable := False;
               END
           ELSE
               AutoSendEnable := True;

           DisplayAutoSendCharacterCount;
           END;

      ATI: BEGIN
           Inc (AutoTimeIncrementQSOs);
           IF (AutoTimeIncrementQSOs > 6) THEN
               AutoTimeIncrementQSOs := 0;
           END;

      BEN: BEGIN
           IF DVPEnable THEN
               BEGIN
               BackCopyEnable := NOT BackCopyEnable;
               IF (BackCopyEnable) AND DVPActive THEN
                   StartBackCopy
               ELSE
                   StopBackCopy;
               END
           ELSE
               BackCopyEnable := False;
           END;


      BAB: BEGIN
           BandMapAllBands         := NOT BandMapAllBands;
           DisplayBandMap;
           END;

      BAM: BEGIN
           BandMapAllModes         := NOT BandMapAllModes;
           DisplayBandMap;
           END;

     { BMO: BEGIN                                                         }
     {      BandMapMultsOnly        := NOT BandMapMultsOnly; }{KK1L: 6.xx}
     {      DisplayBandMap;                                               }
     {      END;                                                          }

      BCW: BandMapCallWindowEnable := NOT BandMapCallWindowEnable;

      BDD: BEGIN
           BandMapDupeDisplay      := NOT BandMapDupeDisplay;
           DisplayBandMap;
           END;

      BMD: BEGIN
           TempInt := QuickEditInteger ('Enter new band map decay time (minutes) : ', 3);
           IF TempInt <> - 1 THEN
               BEGIN
               BandMapDecayValue := TempInt; {KK1L: 6.65}
               BandMapDecayMultiplier  := (BandMapDecayValue div 64) + 1; {KK1L: 6.65}
               BandMapDecayTime := BandMapDecayValue div BandMapDecayMultiplier; {KK1L: 6.65}
               ResetBandMapTimes; {KK1L: 6.70 Need to set BM times to new decay time}
               {ForcedEntry := True; }{KK1L: 6.70 Switch to add comment since I wrote the entry automatically}
               {WriteParameterToLOGCFGFile (LogConfigFileName, ActiveLine); }{KK1L: 6.70}
               END;
           END;

      BCQ: BandMapDisplayCQ := NOT BandMapDisplayCQ;

      BME: BEGIN
           BandMapEnable := NOT BandMapEnable;
           IF BandMapEnable THEN DisplayBandMap ELSE RemoveWindow (BandMapWindow);
           END;

      BMG: BEGIN
           TempInt := QuickEditInteger ('Enter new band map guard band (hertz) : ', 6);
           IF TempInt <> - 1 THEN
               BandMapGuardBand := TempInt;
           END;

      {KK1L: 6.64}
      BSM: CASE BandMapSplitMode OF
               ByCutoffFrequency: BandMapSplitMode := AlwaysPhone;
               AlwaysPhone:       BandMapSplitMode := ByCutoffFrequency;
               END;

      BNA: Tone.SetBeepEnable(NOT Tone.GetBeepEnable);
      BSA: BEGIN
              BeepSoundCardEnable := NOT BeepSoundCardEnable;
              IF BeepSoundCardEnable THEN 
              begin
                 soundmode(0);
                 dvpenable := false;
              end
              else soundmode(1);
              IF not dvpsetup then
              begin
                 IF BeepSoundCardEnable then beginsound else endsound;
              end;
           END;
      BET: BeepEvery10QSOs  := NOT BeepEvery10QSOs;

      BRL: BEGIN
           BigRemainingList := NOT BigRemainingList;
           CountryTable.MakeDefaultRemainingCountryList;
           ChangedRemainingMults := True;
           END;

      BPD: Packet.BroadcastAllPacketData := NOT Packet.BroadcastAllPacketData;

      {KK1L: 6.65}
      SAS: CallWindowShowAllSpots := NOT CallWindowShowAllSpots;

      CAU: CallsignUpdateEnable := NOT CallsignUpdateEnable;

      CLF: CheckLogFileSize := NOT CheckLogFileSize;

      CDE: BEGIN
           ColumnDupesheetEnable := NOT ColumnDupesheetEnable;

           IF VisibleDupeSheetEnable THEN
               BEGIN
               VisibleDupeSheetChanged := True;
               VisibleLog.DisplayVisibleDupeSheet (ActiveBand, ActiveMode);
               END;
           END;

      CID: BEGIN
           TempString := UpperCase (QuickEditResponse ('Enter computer id (A-Z or RETURN for none) : ', 1));

           IF Length (TempString) > 0 THEN
               BEGIN
               TempChar := TempString [1];

               IF (TempChar >= 'A') AND (TempChar <= 'Z') THEN
                   ComputerID := TempChar
               ELSE
                   ComputerID := Chr (0);
               END
           ELSE
               ComputerID := Chr (0);
           END;

      CEC: ConfirmEditChanges    := NOT ConfirmEditChanges;

      CIF: CountryInformationFile := QuickEditResponse ('Enter new country information filename : ', 30);

      CKM:
      begin
         if ActiveKeyer.GetCurtisMode = ModeA then
            ActiveKeyer.SetCurtisMode(ModeB)
         else
         begin
            if ActiveKeyer.GetCurtisMode = ModeB then
               ActiveKeyer.SetCurtisMode(Ultimatic) 
            else
               ActiveKeyer.SetCurtisMode(ModeA);
         end;
      end;
      CWE: CWEnable              := NOT CWEnable;
      CWS: CWSpeedFromDatabase   := NOT CWSpeedFromDataBase;

      CSI: BEGIN  {KK1L: 6.72}
           TempInt := QuickEditInteger ('Enter new CW increment (1 to 10 WPM) : ', 3);
           IF (TempInt > 0) AND (TempInt < 11) THEN
               CodeSpeedIncrement := TempInt;
           END;

      CWT: BEGIN
           TempInt := QuickEditInteger ('Enter new CW monitor tone (0 to disable) : ', 6);
           IF TempInt <> - 1 THEN
               CWTone := TempInt;
           AddStringToBuffer ('', CWTone);
           END;

      DEE: DEEnable          := NOT DEEnable;
      DIG: DigitalModeEnable := NOT DigitalModeEnable;

      DIS: CASE DistanceMode OF
               NoDistanceDisplay: DistanceMode := DistanceMiles;
               DistanceMiles:     DistanceMode := DistanceKM;
               DistanceKM:        DistanceMode := NoDistanceDisplay;
               END;

      DCS: CASE DupeCheckSound OF
               DupeCheckNoSound:     DupeCheckSound := DupeCheckBeepIfDupe;
               DupeCheckBeepIfDupe:  DupeCheckSound := DupeCheckGratsIfMult;
               DupeCheckGratsIfMult: DupeCheckSound := DupeCheckNoSound;
               END;

      DSE: Sheet.DupeSheetEnable := NOT Sheet.DupeSheetEnable;

      DVC: DVKControlKeyRecord := NOT DVKControlKeyRecord;

      DVE: BEGIN
           IF NOT DVPActive THEN
               BEGIN
               Tone.DoABeep (ThreeHarmonics);
               QuickDisplay ('SORRY!  DVPTSR was not loaded.');
               Wait (3000);
               END;

           DVPEnable := (NOT DVPEnable) AND DVPActive;
           IF DVPEnable THEN DVPInit
           ELSE
               RemoveWindow (CodeSpeedWindow);
           END;

      DVP: DVPPath := QuickEditResponse ('Enter new DVP PATH = ', 40);

      EES: EscapeExitsSearchAndPounce := NOT EscapeExitsSearchAndPounce;

      EME: ExchangeMemoryEnable := NOT ExchangeMemoryEnable;
      FWE: ActiveKeyer.SetFarnsworthEnable(NOT ActiveKeyer.GetFarnsworthEnable);

      FWS: BEGIN
           TempInt := QuickEditInteger ('Enter new farnsworth speed cut in value : ', 2);
           IF TempInt <> - 1 THEN
               ActiveKeyer.SetFarnsworthSpeed(TempInt);
           END;

      FSF: BEGIN
           TempInt := QuickEditInteger ('Enter new floppy file save frequency : ', 5);
           IF TempInt <> - 1 THEN
               FloppyFileSaveFrequency := TempInt;
           END;

      FSE: FloppyFileSaveName := QuickEditResponse ('Enter new floppy file save name : ', 20);

      FSM: CASE FootSwitchMode OF
               FootSwitchDisabled:                 FootSwitchMode := Normal;
               Normal:                             FootSwitchMode := FootSwitchF1;
               FootSwitchF1:                       FootSwitchMode := FootSwitchLastCQFreq;
               FootSwitchLastCQFreq:               FootSwitchMode := FootSwitchNextBandMap;
               FootSwitchNextBandMap:              FootSwitchMode := FootSwitchNextDisplayedBandMap; {KK1L: 6.64}
               FootSwitchNextDisplayedBandMap:     FootSwitchMode := FootSwitchNextMultBandMap;      {KK1L: 6.68}
               FootSwitchNextMultBandMap:          FootSwitchMode := FootSwitchNextMultDisplayedBandMap; {KK1L: 6.68}
               FootSwitchNextMultDisplayedBandMap: FootSwitchMode := FootSwitchDupeCheck;
               FootSwitchDupeCheck:                FootSwitchMode := QSONormal;
               QSONormal:                          FootSwitchMode := QSOQuick;
               QSOQuick:                           FootSwitchMode := FootSwitchControlEnter;
               FootSwitchControlEnter:             FootSwitchMode := StartSending;
               StartSending:                       FootSwitchMode := SwapRadio;
               SwapRadio:
                  Begin
                    FootSwitchMode := CWGrant;
                    ActiveKeyer.setCwGrant(true);
                  End;
               CWGrant:
                  Begin
                    FootSwitchMode := FootSwitchDisabled;
                    ActiveKeyer.setCwGrant(false);
                  End;
               END;

      FA1: BEGIN
           TempLongInt := QuickEditInteger ('Enter radio 1 frequency adder : ', 11);

           IF TempLongInt <> -1 THEN
               Radio1FrequencyAdder := TempLongInt;
           END;

      FA2: BEGIN
           TempLongInt := QuickEditInteger ('Enter radio 2 frequency adder : ', 11);

           IF TempLongInt <> -1 THEN
               Radio2FrequencyAdder := TempLongInt;
           END;

      FPR: BEGIN {KK1L: 6.71}
           TempLongInt := QuickEditInteger ('Enter freq poll rate in ms (10-1000) : ', 4);
           IF (TempLongInt >= 10) AND (TempLongInt <= 1000) THEN FreqPollRate := TempLongInt; {KK1L: 6.72}
           END;

      FME: FrequencyMemoryEnable := NOT FrequencyMemoryEnable;
      FCR: FT1000MPCWReverse     := NOT FT1000MPCWReverse;

      GMC: BEGIN
           TempString := UpperCase (QuickEditResponse ('Enter new center for grid map : ', 4));
           IF GoodLookingGrid (TempString) OR (TempString = '') THEN
               GridMapCenter:= TempString;
           END;


      HFE: HFBandEnable      := NOT HFBandEnable;

      HDP: CASE HourDisplay OF
               ThisHour:      HourDisplay := LastSixtyMins;
               LastSixtyMins: HourDisplay := BandChanges;
               BandChanges:   HourDisplay := ThisHour;
               END;

      HOF: BEGIN
           TempString := QuickEditResponse ('Enter new hour offset : ', 5);

           IF TempString <> '' THEN
               BEGIN
               Val (TempString, TempInt, Result);
               IF Result = 0 THEN HourOffset := TempInt;
               END;
           END;

      ICP: BEGIN
           TempString := QuickEditResponse ('Enter Icom command delay in ms (default = 300) : ', 5);

           IF TempString <> '' THEN
               BEGIN
               Val (TempString, TempInt, Result);
               IF Result = 0 THEN IcomCommandPause := TempInt;
               END;
           END;


      ITE: IncrementTimeEnable     := NOT IncrementTimeEnable;

      IFE: BEGIN
           IntercomFileEnable      := NOT IntercomFileEnable;

           IF IntercomFileEnable THEN
               BEGIN
               OpenFileForAppend (IntercomFileWrite, 'INTERCOM.TXT');
               IntercomFileOpen := True;
               END
           ELSE
               IF IntercomFileOpen THEN
                   BEGIN
                   Close (IntercomFileWrite);
                   IntercomFileOpen := False;
                   END;
           END;

      IEX: BEGIN
           Inc (ActiveInitialExchange);

           IF ActiveInitialExchange > CustomInitialExchange THEN
               ActiveInitialExchange := NoInitialExchange;
           END;

      IXO: InitialExchangeOverwrite := NOT InitialExchangeOverwrite; {KK1L: 6.70}

      IEC: IF InitialExchangeCursorPos = AtEnd THEN
               InitialExchangeCursorPos := AtStart
           ELSE
               InitialExchangeCursorPos := AtEnd;

      KNE: K1EANetworkEnable := NOT K1EANetworkEnable;

      KSI: BEGIN
           TempString := UpperCase (QuickEditResponse ('Enter station ID character : ', 1));

           IF Length (TempString) > 0 THEN
               BEGIN
               TempChar := TempString [1];

               IF (TempChar >= '0') AND (TempChar <= 'Z') THEN
                   K1EAStationID := TempChar
               ELSE
                   K1EAStationID := Chr (0);
               END
           ELSE
               K1EAStationID := Chr (0);
           END;

      KCM: KeypadCWMemories        := NOT KeypadCWMemories;

      LDZ: CASE LeadingZeros OF
               0: LeadingZeros := 1;
               1: LeadingZeros := 2;
               2: LeadingZeros := 3;
               3: LeadingZeros := 0;
               END;

      LZC: CASE LeadingZeroCharacter OF
               'T': LeadingZeroCharacter := 'O';
               'O': LeadingZeroCharacter := '0';
               '0': LeadingZeroCharacter := 'T';
               END;

      LCI: LeaveCursorInCallWindow := NOT LeaveCursorInCallWindow;
      LFE: LogFrequencyEnable      := NOT LogFrequencyEnable;

      LRS: LogRSSent  := QuickEditResponse ('Enter RS to show as sent in log : ', 5);

      LDQ: LiteralDomesticQTH := NOT LiteralDomesticQTH;

      LRT: LogRSTSent := QuickEditResponse ('Enter RST to show as sent in log : ', 5);

      LSE: LogWithSingleEnter := NOT LogWithSingleEnter;
      LFR: LookForRSTSent     := NOT LookForRSTSent;
      MSE: MessageEnable      := NOT MessageEnable;
      MEN: MouseEnable        := NOT MouseEnable;

      MFD: MyFDClass := UpperCase (QuickEditResponse ('Enter field day class : ', 6));

      MGR: BEGIN
           TempString := UpperCase (QuickEditResponse ('Enter new grid square : ', 6));
           IF GoodLookingGrid (TempString) OR (TempString = '') THEN
               MyGrid := TempString;
           END;

      MIO: BEGIN
           TempString := UpperCase (QuickEditResponse ('Enter new value fop MY IOTA : ', 6));
           MyIOTA := TempString;
           END;


      MRM: BEGIN
           Inc (MultReportMinimumBands);
           IF MultReportMinimumBands >= 6 THEN
               MultReportMinimumBands := 2;
           END;

      MIM: BEGIN
           MultiInfoMessage := QuickEditResponse ('Enter new multi info message (15 chars max) : ', 15);

           IF OpMode = CQOpMode THEN
               CreateAndSendCQMultiInfoMessage
           ELSE
               CreateAndSendSAPMultiInfoMessage;
           END;

      MMO: MultiMultsOnly        := NOT MultiMultsOnly;

      MRT: BEGIN
           TempInt := QuickEditInteger ('Enter new multi retry timeout (minimum 3 seconds) : ', 6);
           IF TempInt >= 3 THEN MultiRetryTime := TempInt;
           END;

      MUM: MultiUpdateMultDisplay := NOT MultiUpdateMultDisplay;
      MBA: MultipleBandsEnabled   := NOT MultipleBandsEnabled;
      MMD: MultipleModesEnabled   := NOT MultipleModesEnabled;
      NFE: NameFlagEnable         := NOT NameFlagEnable;
      NLQ: NoLog                  := NOT NoLog;
      NPP: NoPollDuringPTT        := NOT NoPollDuringPTT;
      PAL: PacketAddLF            := NOT PacketAddLF;
      PAR: PacketAutoCR           := NOT PacketAutoCR;

      PBS: Packet.PacketBandSpots       := NOT Packet.PacketBandSpots;
      PBP: Packet.PacketBeep            := NOT Packet.PacketBeep;

      PLF: Packet.PacketLogFileName := QuickEditResponse ('Enter packet log file name (none to disable) : ', 20);

      PRM: BEGIN
           Inc (PacketReturnPerMinute);
           IF PacketReturnPerMinute > 10 THEN
               PacketReturnPerMinute := 0;
           END;

      PSC: BEGIN {KK1L: 6.71 Implimented what I started in 6.68}
           PacketSpotComment := UpperCase (QuickEditResponse ('Enter a comment to send with each spot : ', 15));
           END;

      PKD: PacketSpotDisable    := NOT PacketSpotDisable;
      PSE: PacketSpotEditEnable := NOT PacketSpotEditEnable;
      SPO: PacketSpotPrefixOnly := NOT PacketSpotPrefixOnly; {KK1L: 6.72}

      PSP: IF Packet.PacketSpots = AllSpots THEN
               Packet.PacketSpots := MultSpots
           ELSE
               Packet.PacketSpots := AllSpots;

      PBE: ActiveKeyer.SetPaddleBug(NOT ActiveKeyer.GetPaddleBug);

      PHC: BEGIN
           TempInt := QuickEditInteger ('Enter new PTT hold count : ', 4);
           IF TempInt <> - 1 THEN
               ActiveKeyer.SetPaddlePTTHoldCount(TempInt);
           END;

      PMT: BEGIN
           TempInt := QuickEditInteger ('New paddle monitor tone (Hz) : ', 4);
           IF TempInt <> - 1 THEN ActiveKeyer.SetPaddleMonitorTone(TempInt);
           LNoSound;
           END;

      PSD: BEGIN
           TempInt := QuickEditInteger ('New paddle speed (0 to disable) : ', 4);

           IF TempInt <> - 1 THEN
               BEGIN
               ActiveKeyer.SetPaddleSpeed(TempInt);
               ActiveKeyer.SetSpeed (TempInt);
               END;
           END;

      PCE: PartialCallEnable        := NOT PartialCallEnable;
      PCL: PartialCallLoadLogEnable := NOT PartialCallLoadLogEnable;
      PCM: PartialCallMultsEnable   := NOT PartialCallMultsEnable;
      PR1: PollRadioOne             := NOT PollRadioOne; {KK1L: 6.72}
      PR2: PollRadioTwo             := NOT PollRadioTwo; {KK1L: 6.72}
      PCA: PossibleCallEnable       := NOT PossibleCallEnable;

      PCN: CASE CD.PossibleCallAction OF  {KK1L: 6.69 support log only check partial}
               AnyCall:            CD.PossibleCallAction := OnlyCallsWithNames;
               OnlyCallsWithNames: CD.PossibleCallAction := LogOnly;
               LogOnly:            CD.PossibleCallAction := AnyCall;
               END;

      PEN: PrinterEnabled           := NOT PrinterEnabled;
      PTT: ActiveKeyer.SetPTTEnable(NOT ActiveKeyer.GetPTTEnable);

      PTD: BEGIN
           TempInt := QuickEditInteger ('Enter PTT delay count : ', 4);
           IF TempInt <> - 1 THEN ActiveKeyer.SetPTTTurnOnDelay(TempInt);
           END;

      QMD: CASE ParameterOkayMode OF
               Standard:       ParameterOkayMode := QSLButDoNotLog;
               QSLButdoNotLog: ParameterOkayMode := QSLAndLog;
               QSLAndLog:      ParameterOkayMode := Standard;
               END;

      QNB: QSONumberByBand := NOT QSONumberByBand;
      QSX: QSXEnable       := NOT QSXEnable;

      QES: QTCExtraSpace := NOT QTCExtraSpace;
      QRS: QTCQRS := NOT QTCQRS;

      QMC: BEGIN
           TempString := QuickEditResponse ('Enter keyboard character to use for ? : ', 1);

           IF Length (TempString) > 0 THEN
               QuestionMarkChar := TempString [1];
           END;

      R1I: BEGIN
           TempString := UpperCase (QuickEditResponse ('Enter rig 1 ID character (A-Z or RETURN for none) : ', 1));

           IF Length (TempString) > 0 THEN
               BEGIN
               TempChar := TempString [1];

               IF (TempChar >= 'A') AND (TempChar <= 'Z') THEN
                   Radio1IDCharacter := TempChar
               ELSE
                   Radio1IDCharacter := Chr (0);
               END
           ELSE
               Radio1IDCharacter := Chr (0);
           END;

      R1T: Radio1TrackingEnable := NOT Radio1TrackingEnable;

      R1U: BEGIN
           TempInt := QuickEditInteger ('Enter new value for RADIO ONE UPDATE SECONDS = ', 3);
           IF TempInt <> -1 THEN Radio1UpdateSeconds:= TempInt;
           END;


      R2I: BEGIN
           TempString := UpperCase (QuickEditResponse ('Enter rig 2 ID character (A-Z or RETURN for none) : ', 1));

           IF Length (TempString) > 0 THEN
               BEGIN
               TempChar := TempString [1];

               IF (TempChar >= 'A') AND (TempChar <= 'Z') THEN
                   Radio2IDCharacter := TempChar
               ELSE
                   Radio2IDCharacter := Chr (0);
               END
           ELSE
               Radio2IDCharacter := Chr (0);
           END;

      R2T: Radio2TrackingEnable := NOT Radio2TrackingEnable;

      R2U: BEGIN
           TempInt := QuickEditInteger ('Enter new value for RADIO TWO UPDATE SECONDS = ', 3);
           IF TempInt <> -1 THEN Radio2UpdateSeconds:= TempInt;
           END;


      RCQ: RandomCQMode := NOT RandomCQMode;

      RDS: BEGIN
           CASE RateDisplay OF
               QSOs:   RateDisplay := Points;
               Points: RateDisplay := QSOs;
               END;
           END;

      RMD: CASE RemainingMultDisplayMode OF
               NoRemainingMults: RemainingMultDisplayMode := Erase;
               Erase:               RemainingMultDisplayMode := HiLight;
               HiLight:             RemainingMultDisplayMode := NoRemainingMults;
               END;
      RT1: BEGIN
           TempString := QuickEditResponse ('Enter timeout in milliseconds: ', 5);

           IF TempString <> '' THEN
               BEGIN
                  Val (TempString, TempInt, Result);
                  IF Result = 0 THEN
                  begin
                     RadioOneResponseTimeout := TempInt;
                     rig1.responsetimeout(radiooneresponsetimeout);
                  end;
               END;
           END;

      RT2: BEGIN
           TempString := QuickEditResponse ('Enter timeout in milliseconds: ', 5);

           IF TempString <> '' THEN
               BEGIN
                  Val (TempString, TempInt, Result);
                  IF Result = 0 THEN
                  begin
                     RadioTwoResponseTimeout := TempInt;
                     rig2.responsetimeout(radiotworesponsetimeout);
                  end;
               END;
           END;

      SO2RLM: so2rbox.setlatch(not so2rbox.getlatch);

      SO2RHM: case so2rbox.getheadphonemode of
                 HNORMAL: so2rbox.setheadphonemode(HSPATIAL);
                 HSPATIAL: so2rbox.setheadphonemode(HSYMMETRIC);
                 HSYMMETRIC: so2rbox.setheadphonemode(HNORMAL);
              end;

      SO2RBE: so2rbox.setblend(not so2rbox.getblend);

      SO2RBV: begin
                 tempint := quickeditinteger('Enter new BLEND VALUE = ',5);
                 if tempint <> - 1 then
                 so2rbox.blendvalue(tempint);
              end;

      SO2RMR: so2rbox.setmicrelay(not so2rbox.getmicrelay);

      SO2RM1: case so2rbox.getrig1map of
              0: so2rbox.setrig1map(1);
              1: so2rbox.setrig1map(2);
              2: so2rbox.setrig1map(3);
              3: so2rbox.setrig1map(4);
              4: so2rbox.setrig1map(0);
              end;

      SO2RM2: case so2rbox.getrig2map of
              0: so2rbox.setrig2map(1);
              1: so2rbox.setrig2map(2);
              2: so2rbox.setrig2map(3);
              3: so2rbox.setrig2map(4);
              4: so2rbox.setrig2map(0);
              end;
 
       

      SHE: SayHiEnable := NOT SayHiEnable;

      SHC: BEGIN
           TempInt := QuickEditInteger ('Enter new SAY HI RATE CUTOFF = ', 5);
           IF TempInt <> - 1 THEN
               SayHiRateCutoff := TempInt;
           END;

      SCS: BEGIN
           CD.CountryString := UpperCase (QuickEditResponse ('Enter new SCP Country String : ', 40));

           IF CD.CountryString <> '' THEN
               IF Copy (CD.CountryString, Length (CD.CountryString), 1) <> ',' THEN
                   CD.CountryString := CD.CountryString + ',';
           END;

      SML: IF NOT WRTC2002 THEN    {KK1L: 6.68 Keep WRTC folks from using SCP}
               CASE SCPMinimumLetters OF
                   0: SCPMinimumLetters := 3;
                   3: SCPMinimumLetters := 4;
                   4: SCPMinimumLetters := 5;
                   ELSE SCPMinimumLetters := 0;
                   END
               ELSE SCPMinimumLetters := 0;

      SAD: SendAltDSpotsToPacket := NOT SendAltDSpotsToPacket;

      SCF: SendCompleteFourLetterCall := NOT SendCompleteFourLetterCall;

      SPS: StereoPinState := NOT StereoPinState; {KK1L: 6.71}

      SQI: SendQSOImmediately  := NOT SendQSOImmediately;
      SKE: CASE ShiftKeyEnable OF
           None:
             BEGIN
                ShiftKeyEnable := Shift;
                shiftchange(1);
             END;
           Shift:
             BEGIN
                ShiftKeyEnable := AltShift;
                shiftchange(2);
             END;
           AltShift:
             BEGIN
                ShiftKeyEnable := None;
                shiftchange(0);
             END;
           END;
      SIN: ShortIntegers       := NOT ShortIntegers;
      SSP: ShowSearchAndPounce := NOT ShowSearchAndPounce;

      SEN: IF DDXState = Off THEN
           //    DDXState := StandBy
           DDXState := Off; //can't change simulator from ctrl-j
//           ELSE
//               DDXState := Off;

      SRM: SingleRadioMode := NOT SingleRadioMode;
      SAB: SkipActiveBand := NOT SkipActiveBand;

      SMC: BEGIN
           TempString := QuickEditResponse ('Enter keyboard character to use for / : ', 1);

           IF Length (TempString) > 0 THEN
               SlashMarkChar := TempString [1];
           END;


      SBD: SpaceBarDupeCheckEnable := NOT SpaceBarDupeCheckEnable;

      SQR: SprintQSYRule        := NOT SprintQSYRule;
      SRP: SwapPacketSpotRadios := NOT SwapPacketSpotRadios;
      SWP: ActiveKeyer.SetSwapPaddles(NOT ActiveKeyer.GetSwapPaddles);
      SWR: SwapRadioRelaySense  := NOT SwapRadioRelaySense;

      TAB: CASE TabMode OF
               NormalTabMode:   TabMode := ControlFTabMode;
               ControlFTabMode: TabMode := NormalTabMode;
               END;

      TMR: CASE TenMinuteRule OF
               NoTenMinuteRule: TenMinuteRule := TimeOfFirstQSO;
               TimeOfFirstQSO:  TenMinuteRule := NoTenMinuteRule;
               END;

      TOT: BEGIN
           TempString := QuickEditResponse ('Enter new off time value (minutes) : ', 6);

           IF StringHas (TempString, ':') THEN
               BEGIN
               Val (PrecedingString (TempString, ':'), TempHour, Result);
               IF Result = 0 THEN
                   BEGIN
                   Val (PostcedingString (TempString, ':'), TempMinute, Result);
                   IF Result = 0 THEN
                       TotalOffTime := TempHour * 60 + TempMinute;
                   END;
               END
           ELSE
               BEGIN
               Val (TempString, TempMinute, Result);
               IF Result = 0 THEN TotalOffTime := TempInt;
               END;
           END;

      TDE: BEGIN
               TuneDupeCheckEnable := NOT TuneDupeCheckEnable; {KK1L: 6.73}
               TwoRadioState := Idle;
           END;

      TWD: ActiveKeyer.SetTuneWithDits(NOT ActiveKeyer.GetTuneWithDits);

      TRM: IF TwoRadioState = TwoRadiosDisabled THEN
               TwoRadioState := Idle
           ELSE
               TwoRadioState := TwoRadiosDisabled;

      URF: UpdateRestartFileEnable := NOT UpdateRestartFileEnable;

      UIS: BEGIN
           Inc (UserInfoShown);
           IF UserInfoShown > CustomInfo THEN UserInfoShown := NoUserInfo;
           END;

      VDE: VGADisplayEnable := NOT VGADisplayEnable;
      VBE: BEGIN
           VHFBandsEnabled := NOT VHFBandsEnabled;
           DisplayBandMap;
           END;

      VDS: VisibleDupesheetEnable := NOT VisibleDupesheetEnable;

      WFS: WaitForStrength := NOT WaitForStrength;

      WUT: BEGIN
           TempInt := QuickEditInteger ('Enter new value for WAKE UP TIME OUT = ', 5);
           IF TempInt <> -1 THEN
               WakeUpTimeOut := TempInt;
           END;

      WBE: BEGIN
           WARCBandsEnabled := NOT WARCBandsEnabled;
           DisplayBandMap;
           END;

      WEI: BEGIN
           TempReal := QuickEditReal ('Enter new weight value (.5 - 1.5) : ', 7);
           Write (TempReal, ' ');
           IF TempReal <> -1 THEN
               ActiveKeyer.SetWeight(TempReal);
           END;

      WCP: WildCardPartials := NOT WildCardPartials;


      END;

    END;



PROCEDURE WriteParameterToLOGCFGFile (FileName: Str80; Line: MenuEntryType);

VAR FileWrite: TEXT;

    BEGIN
    OpenFileForAppend (FileWrite, FileName);

    IF ForcedEntry THEN {KK1L: 6.70} {KK1L: 6.70 turned this off. Chose to save value in BANDMAP.BIN}
        BEGIN
        CASE Line OF
          BMD: WriteLn (FileWrite, ';**Added automatically to keep band map decay consistent');
        END;
        ForcedEntry := False; {KK1L: 6.70 Done using the switch}
        END;

    Write (FileWrite, Description (Line), ' = ');

    CASE Line OF

      ACC: WriteLn (FileWrite, AllCWMessagesChainable);
      ABE: WriteLn (FileWrite, AltDBufferEnable);
      ABC: WriteLn (FileWrite, AlwaysCallBlindCQ);
      AFF: WriteLn (FileWrite, AskForFrequencies);
      AIO: WriteLn (FileWrite, AskIfContestOver);
      ACT: WriteLn (FileWrite, AutoCallTerminate);
      ADP: WriteLn (FileWrite, AutoDisplayDupeQSO);
      ADE: WriteLn (FileWrite, AutoDupeEnableCQ);
      ADS: WriteLn (FileWrite, AutoDupeEnableSAndP);
      AQI: WriteLn (FileWrite, AutoQSLInterval);
      AQD: WriteLn (FileWrite, AutoQSONumberDecrement);
      ASP: WriteLn (FileWrite, AutoSAPEnable);
      ASR: WriteLn (FileWrite, AutoSAPEnableRate); {KK1L: 6.72}
      ARC: WriteLn (FileWrite, AutoReturnToCQMode);
      ASC: WriteLn (FileWrite, AutoSendCharacterCount);
      ATI: WriteLn (FileWrite, AutoTimeIncrementQSOs);

      BEN: WriteLn (FileWrite, BackCopyEnable);
      BAB: WriteLn (FileWrite, BandMapAllBands);
      BAM: WriteLn (FileWrite, BandMapAllModes);
      {BMO: WriteLn (FileWrite, BandMapMultsOnly);} {KK1L: 6.xx}
      BCW: WriteLn (FileWrite, BandMapCallWindowEnable);
      BDD: WriteLn (FileWrite, BandMapDupeDisplay);
      BMD: WriteLn (FileWrite, BandMapDecayValue);
      BCQ: WriteLn (FileWrite, BandMapDisplayCQ);
      BME: WriteLn (FileWrite, BandMapEnable);
      BMG: WriteLn (FileWrite, BandMapGuardBand);

      {KK1L: 6.64}
      BSM: CASE BandMapSplitMode OF
               ByCutoffFrequency: WriteLn (FileWrite, 'BY CUTOFF FREQ');
               AlwaysPhone:       WriteLn (FileWrite, 'ALWAYS PHONE');
               END;

      BNA: WriteLn (FileWrite, Tone.GetBeepEnable);
      BSA: WriteLn (FileWrite, BeepSoundCardEnable);
      BET: WriteLn (FileWrite, BeepEvery10QSOs);
      BRL: WriteLn (FileWrite, BigRemainingList);
      BPD: WriteLn (FileWrite, Packet.BroadcastAllPacketData);
      CAU: WriteLn (FileWrite, CallsignUpdateEnable);
      CLF: WriteLn (FileWrite, CheckLogFileSize);
      CDE: WriteLn (FileWrite, ColumnDupeSheetEnable);
      CID: WriteLn (FileWrite, ComputerID);
      CEC: WriteLn (FileWrite, ConfirmEditChanges);
      CIF: WriteLn (FileWrite, CountryInformationFile);

      CKM: IF ActiveKeyer.GetCurtisMode = ModeA THEN
               WriteLn (FileWrite, 'A')
           ELSE
           begin
              if ActiveKeyer.GetCurtisMode = ModeB then
                  WriteLn (FileWrite, 'B')
              else
                  WriteLn (FileWrite, 'U');
           end;

      CWE: WriteLn (FileWrite, CWEnable);
      CWS: WriteLn (FileWrite, CWSpeedFromDatabase);
      CSI: WriteLn (FileWrite, CodeSpeedIncrement); {KK1L: 6.72}
      CWT: WriteLn (FileWrite, CWTone);

      DEE: WriteLn (FileWrite, DEEnable);

      DIG: WriteLn (FileWrite, DigitalModeEnable);

      DIS: CASE DistanceMode OF
               NoDistanceDisplay: WriteLn (Filewrite, 'NONE');
               DistanceMiles:     WriteLn (Filewrite, 'MILES');
               DistanceKM:        WriteLn (Filewrite, 'KM');
               END;

      DCS: CASE DupeCheckSound OF
               DupeCheckNoSound:     WriteLn (FileWrite, 'NONE');
               DupeCheckBeepIfDupe:  WriteLn (FileWrite, 'DUPE BEEP');
               DupeCheckGratsIfMult: WriteLn (FileWrite, 'MULT FANFARE');
               END;

      DSE: WriteLn (FileWrite, Sheet.DupeSheetEnable);

      DVC: WriteLn (FileWrite, DVKControlKeyRecord);
      DVE: WriteLn (FileWrite, DVPEnable);
      DVP: WriteLn (FileWrite, DVPPath);

      EES: WriteLn (FileWrite, EscapeExitsSearchAndPounce);
      EME: WriteLn (FileWrite, ExchangeMemoryEnable);
      FWE: WriteLn (FileWrite, ActiveKeyer.GetFarnsworthEnable);
      FWS: WriteLn (FileWrite, ActiveKeyer.GetFarnsworthSpeed);
      FSF: WriteLn (FileWrite, FloppyFileSaveFrequency);
      FSE: WriteLn (FileWrite, FloppyFileSaveName);

      FSM: CASE FootSwitchMode OF
               FootSwitchDisabled:                  WriteLn (FileWrite, 'DISABLED');
               FootSwitchF1:                        WriteLn (FileWrite, 'F1');
               FootSwitchLastCQFreq:                WriteLn (FileWrite, 'LAST CQ FREQ');
               FootSwitchNextBandMap:               WriteLn (FileWrite, 'NEXT BANDMAP');
               FootSwitchNextDisplayedBandMap:      WriteLn (FileWrite, 'NEXT DISP BANDMAP'); {KK1L: 6.64}
               FootSwitchNextMultBandMap:           WriteLn (FileWrite, 'NEXT MULT BANDMAP'); {KK1L: 6.68}
               FootSwitchNextMultDisplayedBandMap:  WriteLn (FileWrite, 'NEXT MULT DISP BANDMAP'); {KK1L: 6.68}
               FootSwitchDupeCheck:                 WriteLn (FileWrite, 'DUPE CHECK');
               Normal:                              WriteLn (FileWrite, 'NORMAL');
               QSONormal:                           WriteLn (FileWrite, 'QSO NORMAL');
               QSOQuick:                            WriteLn (FileWrite, 'QSO QUICK');
               FootSwitchControlEnter:              WriteLn (FileWrite, 'CONTROL ENTER');
               StartSending:                        WriteLn (FileWrite, 'START SENDING');
               SwapRadio:                           WriteLn (FileWrite, 'SWAP RADIOS');
               CWGrant:                             WriteLn (FileWrite, 'CW GRANT');
               END;

      FA1: WriteLn (FileWrite, Radio1FrequencyAdder);
      FA2: WriteLn (FileWrite, Radio2FrequencyAdder);
      FPR: WriteLn (FileWrite, FreqPollRate); {KK1L: 6.71}
      FME: WriteLn (FileWrite, FrequencyMemoryEnable);
      FCR: WriteLn (FileWrite, FT1000MPCWReverse);
      GMC: WriteLn (FileWrite, GridMapCenter);
      HFE: WriteLn (FileWrite, HFBandEnable);

      HDP: CASE HourDisplay OF
               ThisHour:      WriteLn (FileWrite, 'THIS HOUR');
               LastSixtyMins: WriteLn (FileWrite, 'LAST SIXTY MINUTES');
               BandChanges:   WriteLn (FileWrite, 'BAND CHANGES');
               END;

      HOF: WriteLn (FileWrite, HourOffset);
      ICP: WriteLn (FileWrite, IcomCommandPause);

      ITE: WriteLn (FileWrite, IncrementTimeEnable);
      IFE: WriteLn (FileWrite, IntercomFileEnable);

      IEX: CASE ActiveInitialExchange OF
               NoInitialExchange:           WriteLn (FileWrite, 'NONE');
               NameInitialExchange:         WriteLn (FileWrite, 'NAME');
               NameQTHInitialExchange:      WriteLn (FileWrite, 'NAME QTH');
               CheckSectionInitialExchange: WriteLn (FileWrite, 'CHECK SECTION');
               SectionInitialExchange:      WriteLn (FileWrite, 'SECTION');
               QTHInitialExchange:          WriteLn (FileWrite, 'QTH');
               FOCInitialExchange:          WriteLn (FileWrite, 'FOC NUMBER');
               GridInitialExchange:         WriteLn (FileWrite, 'GRID');
               ZoneInitialExchange:         WriteLn (FileWrite, 'ZONE');
               User1InitialExchange:        WriteLn (FileWrite, 'USER 1');
               User2InitialExchange:        WriteLn (FileWrite, 'USER 2');
               User3InitialExchange:        WriteLn (FileWrite, 'USER 3');
               User4InitialExchange:        WriteLn (FileWrite, 'USER 4');
               User5InitialExchange:        WriteLn (FileWrite, 'USER 5');
               CustomInitialExchange:       WriteLn (FileWrite, 'CUSTOM');
               END;

      IXO: WriteLn (FileWrite, InitialExchangeOverwrite); {KK1L: 6.70}

      IEC: IF InitialExchangeCursorPos = AtStart THEN
               WriteLn (FileWrite, 'AT START')
           ELSE
               WriteLn (FileWrite, 'AT END');

      KNE: WriteLn (FileWrite, K1EANetworkEnable);
      KSI: WriteLn (FileWrite, K1EAStationID);
      KCM: WriteLn (FileWrite, KeypadCWMemories);

      LDZ: WriteLn (FileWrite, LeadingZeros);
      LZC: WriteLn (FileWrite, LeadingZeroCharacter);
      LCI: WriteLn (FileWrite, LeaveCursorInCallWindow);
      LFE: WriteLn (FileWrite, LogFrequencyEnable);
      LRS: WriteLn (FileWrite, LogRSSent);
      LDQ: WriteLn (FileWrite, LiteralDomesticQTH);
      LRT: WriteLn (FileWrite, LogRSTSent);
      LSE: WriteLn (FileWrite, LogWithSingleEnter);
      LFR: WriteLn (FileWrite, LookForRSTSent);

      MSE: WriteLn (FileWrite, MessageEnable);
      MEN: WriteLn (FileWrite, MouseEnable);
      MFD: WriteLn (FileWrite, MyFDClass);
      MGR: WriteLn (FileWRite, MyGrid);
      MIO: WriteLn (FileWrite, MyIOTA);
      MRM: WriteLn (FileWrite, MultReportMinimumBands);
      MIM: WriteLn (FileWrite, MultiInfoMessage);

      MMO: WriteLn (FileWrite, MultiMultsOnly);
      MRT: WriteLn (FileWrite, MultiRetryTime);
      MUM: WriteLn (FileWrite, MultiUpdateMultDisplay);
      MBA: WriteLn (FileWrite, MultipleBandsEnabled);
      MMD: WriteLn (FileWrite, MultipleModesEnabled);

      NFE: WriteLn (FileWrite, NameFlagEnable);
      NLQ: WriteLn (FileWrite, NoLog);
      NPP: WriteLn (FileWrite, NoPollDuringPTT);
      PAL: WriteLn (FileWrite, PacketAddLF);
      PAR: WriteLn (FileWRite, PacketAutoCR);
      PBS: WriteLn (FileWrite, Packet.PacketBandSpots);
      PBP: WriteLn (FileWrite, Packet.PacketBeep);
      PLF: WriteLn (FileWrite, Packet.PacketLogFileName);

      PRM: WriteLn (FileWrite, PacketReturnPerMinute);
      PSC: WriteLn (FileWrite, PacketSpotComment); {KK1L: 6.71 Implimented what I started in 6.68}
      PKD: WriteLn (FileWrite, PacketSpotDisable);
      PSE: WriteLn (FileWrite, PacketSpotEditEnable);
      SPO: WriteLn (FileWrite, PacketSpotPrefixOnly); {KK1L: 6.72}

      PSP: IF Packet.PacketSpots = AllSpots THEN
               WriteLn (FileWrite, 'ALL')
           ELSE
               WriteLn (FileWrite, 'MULT');

      PBE: WriteLn (FileWrite, ActiveKeyer.GetPaddleBug);

      PHC: WriteLn (FileWrite, ActiveKeyer.GetPaddlePTTHoldCount);
      PMT: WriteLn (FileWrite, ActiveKeyer.GetPaddleMonitorTone);
      PSD: WriteLn (FileWrite, ActiveKeyer.GetPaddleSpeed);
      PCE: WriteLn (FileWrite, PartialCallEnable);
      PCL: WriteLn (FileWrite, PartialCallLoadLogEnable);
      PCM: WriteLn (FileWrite, PartialCallMultsEnable);
      PR1: WriteLn (FileWrite, PollRadioOne); {KK1L: 6.72}
      PR2: WriteLn (FileWrite, PollRadioTwo); {KK1L: 6.72}
      PCA: WriteLn (FileWrite, PossibleCallEnable);

      PCN: CASE CD.PossibleCallAction OF  {KK1L: 6.69 support log only check partial}
               AnyCall:            WriteLn (FileWrite, 'ALL');
               OnlyCallsWithNames: WriteLn (FileWrite, 'NAMES');
               LogOnly:            WriteLn (FileWrite, 'LOG ONLY');
               END;

      PEN: WriteLn (FileWrite, PrinterEnabled);
      PTT: WriteLn (FileWrite, ActiveKeyer.GetPTTEnable);
      PTD: WriteLn (FileWrite, ActiveKeyer.GetPTTTurnOnDelay);

      QMD: CASE ParameterOkayMode OF
               Standard:       WriteLn (FileWrite, 'STANDARD');
               QSLButdoNotLog: WriteLn (FileWrite, 'QSL BUT DO NOT LOG');
               QSLAndLog:      WriteLn (FileWrite, 'QSL AND LOG');
               END;

      QNB: WriteLn (FileWrite, QSONumberByBand);
      QSX: WriteLn (FileWrite, QSXEnable);
      QES: WriteLn (FileWrite, QTCExtraSpace);
      QRS: WriteLn (FileWrite, QTCQRS);
      QMC: WriteLn (FileWrite, QuestionMarkChar);

      R1I: WriteLn (FileWrite, Radio1IDCharacter);
      R1T: WriteLn (FileWrite, Radio1TrackingEnable);
      R1U: WriteLn (FileWrite, Radio1UpdateSeconds);
      R2I: WriteLn (FileWrite, Radio2IDCharacter);
      R2T: WriteLn (FileWrite, Radio2TrackingEnable);
      R2U: WriteLn (FileWrite, Radio2UpdateSeconds);

      RCQ: WriteLn (FileWrite, RandomCQMode);

      RDS: CASE RateDisplay OF
               QSOs:   WriteLn (FileWrite, 'QSOs');
               Points: WriteLn (FileWrite, 'QSO Points');
               END;

      RMD: CASE RemainingMultDisplayMode OF
               NoRemainingMults: WriteLn (FileWrite, 'None');
               Erase:               WriteLn (FileWrite, 'Erase');
               HiLight:             WriteLn (FileWrite, 'HiLight');
               END;

      RT1: WriteLn (FileWrite, RadioOneResponseTimeout);

      RT2: WriteLn (FileWrite, RadioTwoResponseTimeout);

      SHE: WriteLn (FileWrite, SayHiEnable);
      SO2RLM: writeln(FileWrite,so2rbox.getlatch);
      SO2RHM: case so2rbox.getheadphonemode of
          HNORMAL: WriteLn (FileWrite,'NORMAL');
          HSPATIAL: WriteLn (FileWrite,'SPATIAL');
          HSYMMETRIC: WriteLn (FileWrite,'SYMMETRIC');
          end;
      SO2RBE: writeln(FileWrite,so2rbox.getblend);
      SO2RBV: writeln(FileWrite,so2rbox.getblendvalue);
      SO2RMR: writeln(FileWrite,so2rbox.getmicrelay);
      SO2RM1: writeln(FileWrite,so2rbox.getrig1map);
      SO2RM2: writeln(FileWrite,so2rbox.getrig2map);
      SHC: WriteLn (FileWrite, SayHiRateCutoff);
      SCS: WriteLn (FileWrite, CD.CountryString);
      SML: WriteLn (FileWrite, SCPMinimumLetters);
      SAD: WriteLn (FileWrite, SendAltDSpotsToPacket);
      SCF: WriteLn (FileWrite, SendCompleteFourLetterCall);
      SPS: WriteLn (FileWrite, StereoPinState); {KK1L: 6.71}
      SQI: WriteLn (FileWrite, SendQSOImmediately);
      SKE: WriteLn (FileWrite, ShiftKeyEnable);
      SIN: WriteLn (FileWrite, ShortIntegers);
      SSP: WriteLn (FileWrite, ShowSearchAndPounce);

      SEN: IF DDXState = Off THEN
               WriteLn (FileWrite, 'FALSE')
           ELSE
               WriteLn (FileWrite, 'TRUE');

      SRM: WriteLn (FileWrite, SingleRadioMode);
      SAB: WriteLn (FileWrite, SkipActiveBand);
      {KK1L: 6.65}
      SAS: WriteLn (FileWrite, CallWindowShowAllSpots);
      SMC: WriteLn (FileWrite, SlashMarkChar);
      SBD: WriteLn (FileWrite, SpaceBarDupeCheckEnable);
      SQR: WriteLn (FileWrite, SprintQSYRule);
      SRP: WriteLn (FileWrite, SwapPacketSpotRadios);
      SWP: WriteLn (FileWrite, ActiveKeyer.GetSwapPaddles);
      SWR: WriteLn (FileWrite, SwapRadioRelaySense);

      TAB: CASE TabMode OF
               NormalTabMode:   WriteLn (FileWrite, 'NORMAL');
               ControlFTabMode: WriteLn (FileWrite, 'CONTROLF');
               END;

      TMR: CASE TenMinuteRule OF
               NoTenMinuteRule: WriteLn (FileWrite, 'NONE');
               TimeOfFirstQSO:  WriteLn (FileWrite, 'TIME OF FIRST QSO');
               END;

      TDE: WriteLn (FileWrite, TuneDupeCheckEnable); {KK1L: 6.73}

      TWD: WriteLn (FileWrite, ActiveKeyer.GetTuneWithDits);

      TRM: IF TwoRadioState <> TwoRadiosDisabled THEN
               WriteLn (FileWrite, 'TRUE')
           ELSE
               WriteLn (FileWrite, 'FALSE');

      URF: WriteLn (FileWrite, UpdateRestartFileEnable);

      UIS: CASE UserInfoShown OF
               NoUserInfo:       WriteLn (Filewrite, 'NONE');
               NameInfo:         WriteLn (FileWrite, 'NAME');
               QTHInfo:          WriteLn (FileWrite, 'QTH');
               CheckSectionInfo: WriteLn (FileWrite, 'CHECK SECTION');
               SectionInfo:      WriteLn (FileWrite, 'SECTION');
               OldCallInfo:      WriteLn (FileWrite, 'OLD CALL');
               FOCInfo:          WriteLn (FileWrite, 'FOC NUMBER');
               GridInfo:         WriteLn (FileWrite, 'GRID');
               ITUZoneInfo:      WriteLn (FileWrite, 'ITU ZONE');
               CQZoneInfo:       WriteLn (FileWrite, 'CQ ZONE');
               User1Info:        WriteLn (FileWrite, 'USER 1');
               User2Info:        WriteLn (FileWrite, 'USER 2');
               User3Info:        WriteLn (FileWrite, 'USER 3');
               User4Info:        WriteLn (FileWrite, 'USER 4');
               User5Info:        WriteLn (FileWrite, 'USER 5');
               CustomInfo:       WriteLn (FileWrite, 'CUSTOM');
               END;

      VDE: WriteLn (FileWrite, VGADisplayEnable);
      VBE: WriteLn (FileWrite, VHFBandsEnabled);
      VDS: WriteLn (FileWrite, VisibleDupesheetEnable);
      WFS: WriteLn (FileWrite, WaitForStrength);
      WUT: WriteLn (FileWrite, WakeUpTimeOut);
      WBE: WriteLn (FileWrite, WARCBandsEnabled);
      WEI: WriteLn (FileWrite, ActiveKeyer.GetWeight:3:2);
      WCP: WriteLn (FileWrite, WildCardPartials);
      END;

    Close (FileWrite);
    END;



FUNCTION ParameterToString (Line: MenuEntryType): Str80;

{ Returns string with value for indictated line parameter }

VAR TempString: Str40;

    BEGIN
    TempString := 'FALSE';

    CASE Line OF
      ACC: IF AllCWMessagesChainable THEN TempString := 'TRUE';
      ABE: IF AltDBufferEnable THEN TempString := 'TRUE';
      ABC: IF AlwaysCallBlindCQ THEN TempString := 'TRUE';
      AFF: IF AskForFrequencies THEN TempString := 'TRUE';
      AIO: IF AskIfContestOver THEN TempString := 'TRUE';
      ACT: IF AutoCallTerminate THEN TempString := 'TRUE';
      ADP: IF AutoDisplayDupeQSO THEN TempString := 'TRUE';
      ADE: IF AutoDupeEnableCQ THEN TempString := 'TRUE';
      ADS: IF AutoDupeEnableSAndP THEN TempString := 'TRUE';
      AQI: Str (AutoQSLInterval, TempString);

      AQD: IF AutoQSONumberDecrement THEN TempString := 'TRUE';
      ASP: IF AutoSAPEnable THEN TempString := 'TRUE';
      ASR: Str (AutoSAPEnableRate, TempString); {KK1L: 6.72}
      ASC: Str (AutoSendCharacterCount, TempString);
      ATI: Str (AutoTimeIncrementQSOs, TempString);

      BEN: IF BackCopyEnable THEN TempString := 'TRUE';
      BAB: IF BandMapAllBands THEN TempString := 'TRUE';
      BAM: IF BandMapAllModes THEN TempString := 'TRUE';
      {BMO: IF BandMapMultsOnly THEN TempString := 'TRUE';} {KK1L: 6.xx}
      BCW: IF BandMapCallWindowEnable THEN TempString := 'TRUE';
      BDD: IF BandMapDupeDisplay THEN TempString := 'TRUE';
      BMD: Str (BandMapDecayValue, TempString);
      BCQ: IF BandMapDisplayCQ THEN TempString := 'TRUE';
      BME: IF BandMapEnable THEN TempString := 'TRUE';
      BMG: Str (BandMapGuardBand, TempString);

      {KK1L: 6.64}
      BSM: CASE BandMapSplitMode OF
               ByCutoffFrequency: TempString := 'BY CUTOFF FREQ';
               AlwaysPhone:       TempString := 'ALWAYS PHONE';
               END;

      BNA: IF Tone.GetBeepEnable THEN TempString := 'TRUE';
      BSA: IF BeepSoundCardEnable THEN TempString := 'TRUE';
      BET: IF BeepEvery10QSOs THEN TempString := 'TRUE';
      BRL: IF BigRemainingList THEN TempString := 'TRUE';
      BPD: IF Packet.BroadcastAllPacketData THEN TempString := 'TRUE';

      CAU: IF CallsignUpdateEnable THEN TempString := 'TRUE';
      CLF: IF CheckLogFileSize THEN TempString := 'TRUE';
      CDE: IF ColumnDupeSheetEnable THEN TempString := 'TRUE';
      CID: TempString := ComputerID;
      CEC: IF ConfirmEditChanges THEN TempString := 'TRUE';
      CIF: TempString := CountryInformationFile;

      CKM: IF ActiveKeyer.GetCurtisMode = ModeA THEN
               TempString := 'A'
           ELSE
           begin
             if ActiveKeyer.GetCurtisMode = ModeB then
                TempString := 'B'
             else
                TempString := 'U';
           end;

      CWE: IF CWEnable THEN TempString := 'TRUE';
      CWS: IF CWSpeedFromDataBase THEN TempString := 'TRUE';
      CSI: Str (CodeSpeedIncrement, TempString);
      CWT: Str (CWTone, TempString);

      DEE: IF DEEnable THEN TempString := 'TRUE';
      DIG: IF DigitalModeEnable THEN TempString := 'TRUE';

      DIS: CASE DistanceMode OF
               NoDistanceDisplay: TempString := 'NONE';
               DistanceMiles:     TempString := 'MILES';
               DistanceKM:        TempString := 'KM';
               END;

      DMF: TempString := DomQTHTable.ActiveDomQTHFile;

      DCS: CASE DupeCheckSound OF
               DupeCheckNoSound:     TempString := 'NONE';
               DupeCheckBeepIfDupe:  TempString := 'DUPE BEEP';
               DupeCheckGratsIfMult: TempString := 'MULT FANFARE';
               END;

      DSE: IF Sheet.DupeSheetEnable THEN TempString := 'TRUE';

      DVC: IF DVKControlKeyRecord THEN TempString := 'TRUE';
      DVE: IF DVPEnable THEN TempString := 'TRUE';
      DVP: TempString := DVPPath;
      EES: IF EscapeExitsSearchAndPounce THEN TempString := 'TRUE';
      EME: IF ExchangeMemoryEnable THEN TempString := 'TRUE';
      FWE: IF ActiveKeyer.GetFarnsworthEnable THEN TempString := 'TRUE';
      FWS: Str (ActiveKeyer.GetFarnsworthSpeed, TempString);
      FSF: Str (FloppyFileSaveFrequency, TempString);
      FSE: TempString := FloppyFileSaveName;

      FSM: CASE FootSwitchMode OF
               FootSwitchDisabled:                  TempString := 'DISABLED';
               FootSwitchF1:                        TempString := 'F1';
               FootSwitchLastCQFreq:                TempString := 'LAST CQ FREQ';
               FootSwitchNextBandMap:               TempString := 'NEXT BANDMAP';
               FootSwitchNextDisplayedBandMap:      TempString := 'NEXT DISP BANDMAP'; {KK1L: 6.64}
               FootSwitchNextMultBandMap:           TempString := 'NEXT MULT BANDMAP'; {KK1L: 6.68}
               FootSwitchNextMultDisplayedBandMap:  TempString := 'NEXT MULT DISP BANDMAP'; {KK1L: 6.68}
               FootSwitchDupeCheck:                 TempString := 'DUPE CHECK';
               Normal:                              TempString := 'NORMAL';
               QSONormal:                           TempString := 'QSO NORMAL';
               QSOQuick:                            TempString := 'QSO QUICK';
               FootSwitchControlEnter:              TempString := 'CONTROL ENTER';
               StartSending:                        TempString := 'START SENDING';
               SwapRadio:                           TempString := 'SWAP RADIOS';
               CWGrant:                             TempString := 'CW GRANT';
               END;

      FA1: Str (Radio1FrequencyAdder, TempString);
      FA2: Str (Radio2FrequencyAdder, TempString);
      FPR: Str (FreqPollRate, TempString); {KK1L: 6.71}
      FME: IF FrequencyMemoryEnable THEN TempString := 'TRUE';
      FCR: IF FT1000MPCWReverse THEN TempString := 'TRUE';
      GMC: TempString := GridMapCenter;
      HFE: IF HFBandEnable THEN TempString := 'TRUE';

      HDP: CASE HourDisplay OF
               ThisHour:      TempString := 'THIS HOUR';
               LastSixtyMins: TempString := 'LAST SIXTY MINUTES';
               BandChanges:   TempString := 'BAND CHANGES';
               END;

      HOF: Str (HourOffset, TempString);

      ICP: Str (IcomCommandPause, TempString);

      ITE: IF IncrementTimeEnable THEN TempString := 'TRUE';
      IFE: IF IntercomFileEnable THEN TempString := 'TRUE';

      IEX: CASE ActiveInitialExchange OF
               NoInitialExchange:           TempString := 'NONE';
               NameInitialExchange:         TempString := 'NAME';
               NameQTHInitialExchange:      TempString := 'NAME QTH';
               CheckSectionInitialExchange: TempString := 'CHECK SECTION';
               SectionInitialExchange:      TempString := 'SECTION';
               QTHInitialExchange:          TempString := 'QTH';
               FOCInitialExchange:          TempString := 'FOC NUMBER';
               GridInitialExchange:         TempString := 'GRID';
               ZoneInitialExchange:         TempString := 'ZONE';
               User1InitialExchange:        TempString := 'USER 1';
               User2InitialExchange:        TempString := 'USER 2';
               User3InitialExchange:        TempString := 'USER 3';
               User4InitialExchange:        TempString := 'USER 4';
               User5InitialExchange:        TempString := 'USER 5';
               CustomInitialExchange:       TempString := 'CUSTOM';
               END;

      IXO: IF InitialExchangeOverwrite THEN TempString := 'TRUE'; {KK1L: 6.70}

      IEC: CASE InitialExchangeCursorPos OF
               AtStart: TempString := 'AT START';
               AtEnd:   TempString := 'AT END';
               END;

      KNE: IF K1EANetworkEnable THEN TempString := 'TRUE';
      KSI: TempString := K1EAStationID;
      KCM: IF KeyPadCWMemories THEN TempString := 'TRUE';
      LDZ: Str (LeadingZeros, TempString);
      LZC: TempString := LeadingZeroCharacter;
      LCI: IF LeaveCursorInCallWindow THEN TempString := 'TRUE';
      LFE: IF LogFrequencyEnable THEN TempString := 'TRUE';
      LRS: TempString := LogRSSent;
      LDQ: IF LiteralDomesticQTH THEN TempString := 'TRUE';
      LRT: TempString := LogRSTSent;
      LSE: IF LogWithSingleEnter THEN TempString := 'TRUE';
      LFR: IF LookForRSTSent THEN TempString := 'TRUE';
      MSE: IF MessageEnable THEN TempString := 'TRUE';
      MEN: IF MouseEnable THEN TempString := 'TRUE';
      MRM: Str (MultReportMinimumBands, TempString);
      MIM: TempString := MultiInfoMessage;
      MMO: IF MultiMultsOnly THEN TempString := 'TRUE';
      MRT: Str (MultiRetryTime, TempString);
      MUM: IF MultiUpdateMultDisplay THEN TempString := 'TRUE';
      MBA: IF MultipleBandsEnabled THEN TempString := 'TRUE';
      MMD: IF MultipleModesEnabled THEN TempString := 'TRUE';
      MFD: TempString := MyFDClass;
      MGR: TempString := MyGrid;
      MIO: TempString := MyIOTA;
      NFE: IF NameFlagEnable THEN TempString := 'TRUE';
      NLQ: IF NoLog THEN TempString := 'TRUE';
      NPP: IF NoPollDuringPTT THEN TempString := 'TRUE';

      PAL: IF PacketAddLF THEN TempString := 'TRUE';
      PAR: IF PacketAutoCR THEN TempString := 'TRUE';
      PBS: IF Packet.PacketBandSpots THEN TempString := 'TRUE';
      PBP: IF Packet.PacketBeep THEN TempString := 'TRUE';

      PRM: Str (PacketReturnPerMinute, TempString);
      PSC: TempString := PacketSpotComment; {KK1L: 6.71 Implimented what I started in 6.68}
      PKD: IF PacketSpotDisable    THEN TempString := 'TRUE';
      PSE: IF PacketSpotEditEnable THEN TempString := 'TRUE';
      SPO: IF PacketSpotPrefixOnly THEN TempString := 'TRUE'; {KK1L: 6.72}

      PSP: IF Packet.PacketSpots = AllSpots THEN
               TempString := 'ALL'
           ELSE
               TempString := 'MULT';

      PBE: IF ActiveKeyer.GetPaddleBug THEN TempString := 'TRUE';
      PMT: Str (ActiveKeyer.GetPaddleMonitorTone, TempString);
      PHC: Str (ActiveKeyer.GetPaddlePTTHoldCount, TempString);
      PSD: Str (ActiveKeyer.GetPaddleSpeed, TempString);
      PCE: IF PartialCallEnable THEN TempString := 'TRUE';
      PCL: IF PartialCallLoadLogEnable THEN TempString := 'TRUE';
      PCM: IF PartialCallMultsEnable THEN TempString := 'TRUE';
      PR1: IF PollRadioOne THEN TempString := 'TRUE'; {KK1L: 6.72}
      PR2: IF PollRadioTwo THEN TempString := 'TRUE'; {KK1L: 6.72}
      PCA: IF PossibleCallEnable THEN TempString := 'TRUE';

      PCN: CASE CD.PossibleCallAction OF  {KK1L: 6.69 support log only check partial}
               AnyCall:            TempString := 'ALL';
               OnlyCallsWithNames: TempString := 'NAMES';
               LogOnly:            TempString := 'LOG ONLY';
               END;

      PEN: IF PrinterEnabled THEN TempString := 'TRUE';
      PTT: IF ActiveKeyer.GetPTTEnable THEN TempString := 'TRUE';
      PTD: Str (ActiveKeyer.GetPTTTurnOnDelay, TempString);

      QMD: CASE ParameterOkayMode OF
               Standard:       TempString := 'Standard';
               QSLButdoNotLog: TempString := 'QSL but no log';
               QSLAndLog:      TempString := 'QSL and log';
               END;

      QNB: IF QSONumberByBand THEN TempString := 'TRUE';
      QSX: IF QSXEnable THEN TempString := 'TRUE';
      QMC: TempString := QuestionMarkChar;

      R1I: TempString := Radio1IDCharacter;
      R1T: IF Radio1TrackingEnable THEN TempString := 'TRUE';
      R1U: Str (Radio1UpdateSeconds, TempString);
      R2I: TempString := Radio2IDCharacter;
      R2T: Str (Radio1UpdateSeconds, TempString);
      R2U: Str (Radio2UpdateSeconds, TempString);

      RCQ: IF RandomCQMode THEN TempString := 'TRUE';

      RDS: CASE RateDisplay OF
               QSOs:   TempString := 'QSOs';
               Points: TempString := 'QSO Points';
               END;

      RMD: CASE RemainingMultDisplayMode OF
               NoRemainingMults: TempString := 'None';
               Erase:            TempString := 'Erase';
               HiLight:          TempString := 'HiLight';
               END;

      RT1: Str (RadioOneResponseTimeout, TempString);
      RT2: Str (RadioTwoResponseTimeout, TempString);

      SHE: IF SayHiEnable THEN TempString := 'TRUE';
      SHC: Str (SayHiRateCutoff, TempString);
      SCS: TempString := CD.CountryString;
      SML: Str (SCPMinimumLetters, TempString);
      SAD: IF SendAltDSpotsToPacket THEN TempString := 'TRUE';
      SCF: IF SendCompleteFourLetterCall THEN TempString := 'TRUE';
      SPS: IF StereoPinState THEN TempString := 'TRUE'; {KK1L: 6.71}
      SQI: IF SendQSOImmediately THEN TempString := 'TRUE';
      SKE: CASE ShiftKeyEnable OF
           None: TempString := 'FALSE';
           Shift: TempString := 'TRUE';
           AltShift: TempString := 'ALT';
           END;
      SIN: IF ShortIntegers THEN TempString := 'TRUE';
      SSP: IF ShowSearchAndPounce THEN TempString := 'TRUE';

      SEN: IF DDXState = Off THEN TempString := 'TRUE';

      SRM: IF SingleRadioMode THEN TempString := 'TRUE';
      SAB: IF SkipActiveBand THEN TempString := 'TRUE';

      {KK1L: 6.65}
      SAS: IF CallWindowShowAllSpots THEN TempString := 'TRUE';
      SMC: TempString := SlashMarkChar;
      SBD: IF SpaceBarDupeCheckEnable THEN TempString := 'TRUE';
      SQR: IF SprintQSYRule THEN TempString := 'TRUE';
      SRP: IF SwapPacketSpotRadios THEN TempString := 'TRUE';
      SWP: IF ActiveKeyer.GetSwapPaddles THEN TempString := 'TRUE';
      SWR: IF SwapRadioRelaySense THEN TempString := 'TRUE';

      TAB: BEGIN
           IF TabMode = NormalTabMode   THEN TempString := 'NORMAL';
           IF TabMode = ControlFTabMode THEN TempString := 'CONTROLF';
           END;

      TMR: CASE TenMinuteRule OF
               NoTenMinuteRule: TempString := 'NONE';
               TimeOfFirstQSO:  TempString := 'TIME OF FIRST QSO';
               END;

      TOT: TempString := MinutesToTimeString (TotalOffTime);

      TDE: IF TuneDupeCheckEnable THEN TempString := 'TRUE';

      TWD: IF ActiveKeyer.GetTuneWithDits THEN TempString := 'TRUE';

      TRM: IF TwoRadioState <> TwoRadiosDisabled THEN TempString := 'TRUE';

      URF: IF UpdateRestartFileEnable THEN TempString := 'TRUE';

      UIS: CASE UserInfoShown OF
               NoUserInfo:       TempString := 'NONE';
               NameInfo:         TempString := 'NAME';
               QTHInfo:          TempString := 'QTH';
               CheckSectionInfo: TempString := 'CHECK SECTION';
               SectionInfo:      TempString := 'SECTION';
               OldCallInfo:      TempString := 'OLD CALL';
               FOCInfo:          TempString := 'FOC NUMBER';
               GridInfo:         TempString := 'GRID';
               ITUZoneInfo:      TempString := 'ITU ZONE';
               CQZoneInfo:       TempString := 'CQ ZONE';
               User1Info:        TempString := 'USER 1';
               User2Info:        TempString := 'USER 2';
               User3Info:        TempString := 'USER 3';
               User4Info:        TempString := 'USER 4';
               User5Info:        TempString := 'USER 5';
               CustomInfo:       TempString := 'CUSTOM';
               END;

      VDE: IF VGADisplayEnable THEN TempString := 'TRUE';
      VBE: IF VHFBandsEnabled THEN TempString := 'TRUE';
      VDS: IF VisibleDupesheetEnable THEN TempString := 'TRUE';
      WFS: IF WaitForStrength THEN TempString := 'TRUE';
      WUT: Str (WakeUpTimeOut, TempString);
      WBE: IF WARCBandsEnabled THEN TempString := 'TRUE';
      WEI: Str (ActiveKeyer.GetWeight:3:2, TempString);
      WCP: IF WildCardPartials THEN TempString := 'TRUE';
      ELSE TempString := '';
      END;

    ParameterToString := TempString;
    END;



PROCEDURE SendParameterToNetwork (Line: MenuEntryType);

VAR TempString: STRING;

    BEGIN
    IF K1EANetworkEnable THEN Exit;

    IF ActiveMultiPort = nil THEN Exit;

    TempString := ParameterToString (Line);

    IF TempString <> '' THEN
        TempString := Description (Line) + ' = ' + TempString;

    SendMultiCommand (MultiBandAddressArray [ActiveBand], $FF,
                      MultiConfigurationMessage, TempString);
    END;



FUNCTION GetActiveLineFromEntryString (EntryString: Str80): MenuEntryType;

VAR en,Entry: MenuEntryType;

    BEGIN
    WHILE EntryString <> '' DO
        BEGIN
        FOR Entry := NoMenuEntry TO LastMenuEntry DO
            IF Copy (Description (Entry), 1, Length (EntryString)) = EntryString THEN
                BEGIN
                en := entry;
                IF Entry = LastMenuEntry THEN en:= Pred (Entry);

                GetActiveLineFromEntryString := en;
                Exit;
                END;

    { Not found - Delete last letter and go from there }

        Delete (EntryString, Length (EntryString), 1);
        END;

    { No good - go to first line }

    Entry := NoMenuEntry;
    Entry := Succ (Entry);
    GetActiveLineFromEntryString := Entry;
    END;



PROCEDURE ProcessConfigurationInput;

VAR FirstEntryShown, MenuEntry, ActiveLine: MenuEntryType;
    Key: CHAR;
    Line: WORD;
    EntryString, FileName: Str40;
    TimeMark: TimeRecord;

    BEGIN
    ChangedRemainingMults := False;

    FOR MenuEntry := NoMenuEntry TO LastMenuEntry DO
        Changed [MenuEntry] := False;

    NoCursor;
    RemoveWindow (EditableLogWindow);
    RemoveWindow (TotalWindow);

    SaveSetAndClearActiveWindow (BigWindow);

    FirstEntryShown := NoMenuEntry;               { Start at top of list }
    FirstEntryShown := Succ (FirstEntryShown);    { Move to first entry }

    ActiveLine := FirstEntryShown;

    MarkTime (TimeMark);

    EntryString := '';

    REPEAT
        DisplayStatus (FirstEntryShown, ActiveLine);
        QuickDisplay  ('Arrow/pageup/pagedn keys or 1st letter to select item.  RETURN to modify');

        IF ActiveMultiPort <> nil THEN
            QuickDisplay2 ('AltW: save entry  Alt-N: to network  Alt-G: all changes to file or ESCAPE')
        ELSE
            QuickDisplay2 ('Alt-W to save to cfg file Alt-G to save all changes to file  ESCAPE exits');

        REPEAT
            IF (ActiveMultiPort <> nil) AND (ElaspedSec100 (TimeMark) > 2000) THEN
                BEGIN
                IF ChangedRemainingMults THEN
                    BEGIN
                    ClrScr;
                    Write ('Please wait.. rebuilding remaining mult list...');
                    Sheet.SetUpRemainingMultiplierArrays;
                    END;

                VisibleDupeSheetRemoved := True;
                RemoveWindow (QuickCommand2Window);
                RemoveAndRestorePreviousWindow;
                BigCursor;
                Exit;
                END;

        UNTIL KeyPressed;

        MarkTime (TimeMark);

        Key := UpCase (ReadKey);

        CASE Key OF
            NullKey:
                CASE (ReadKey) OF
                    AltG:
                        BEGIN
                        RemoveWindow (QuickCommand2Window);

                        FileName := QuickEditResponse ('Enter filename to save all changes to (none for cfg file) : ', 30);

                        IF FileName <> EscapeKey THEN
                            BEGIN
                            IF FileName = '' THEN FileName := LogConfigFileName;

                            FOR MenuEntry := NoMenuEntry TO LastMenuEntry DO
                                IF Changed [MenuEntry] THEN
                                    WriteParameterToLOGCFGFile (FileName, MenuEntry);
                            END;
                        END;

                    AltN: SendParameterToNetwork (ActiveLine);

                    AltW: WriteParameterToLOGCFGFile (LogConfigFileName, ActiveLine);

                    DownArrow:
                        BEGIN
                        IF Ord (ActiveLine) < Ord (LastMenuEntry) - 1 THEN Inc (ActiveLine);

                        IF Ord (FirstEntryShown) < Ord (ActiveLine) - 10 THEN
                            FirstEntryShown := Succ (FirstEntryShown);
                        END;

                    UpArrow:
                        BEGIN
                        IF Ord (ActiveLine) > 1 THEN Dec (ActiveLine);
                        IF (Ord (FirstEntryShown) > 1) AND (Ord (FirstEntryShown) > Ord (ActiveLine) - 2) THEN
                            FirstEntryShown := Pred (FirstEntryShown);
                        END;

                    PageUpKey:
                        BEGIN
                        FOR Line := 1 TO 16 DO
                            BEGIN


                            IF ActiveLine      > Succ (NoMenuEntry) THEN ActiveLine      := Pred (ActiveLine);
                            IF FirstEntryShown > Succ (NoMenuEntry) THEN FirstEntryShown := Pred (FirstEntryShown);
                            END;
                        END;

                    PageDownKey:
                        BEGIN
                        FOR Line := 1 TO 16 DO
                            BEGIN
                            IF Ord (FirstEntryShown) < Ord (LastMenuEntry) - 10 THEN
                                BEGIN
                                IF Ord (ActiveLine) < Ord (LastMenuEntry) - 1 THEN Inc (ActiveLine);
                                FirstEntryShown := Succ (FirstEntryShown);
                                END;
                            END;
                        END;

                    HomeKey:
                        BEGIN
                        ActiveLine := NoMenuEntry;
                        ActiveLine := Succ (ActiveLine);
                        FirstEntryShown := ActiveLine;
                        END;

                    EndKey:
                        BEGIN
                        FirstEntryShown := LastMenuEntry;
                        ActiveLine := LastMenuEntry;
                        ActiveLine := Pred (ActiveLine);

                        FOR Line := 1 TO 17 DO FirstEntryShown := Pred (FirstEntryShown);
                        END;

                    DeleteKey:
                        IF EntryString <> '' THEN
                            Delete (EntryString, Length (EntryString), 1);

                    END;

            BackSpace:
                IF EntryString <> '' THEN
                    Delete (EntryString, Length (EntryString), 1);

            CarriageReturn:
                BEGIN
                ProcessInput (ActiveLine);
                EntryString := '';
                END;

            EscapeKey: BEGIN
                       IF ChangedRemainingMults THEN
                           BEGIN
                           ClrScr;
                           Write ('Please wait.. rebuilding remaining mult list...');
                           Sheet.SetUpRemainingMultiplierArrays;
                           END;

                       VisibleDupeSheetRemoved := True;
                       RemoveWindow (QuickCommand2Window);
                       RemoveAndRestorePreviousWindow;
                       BigCursor;
                       Exit;
                       END;

            ELSE
                IF ((Key >= 'A') AND (Key <= 'Z')) OR (Key = ' ') THEN
                    BEGIN
                    EntryString := EntryString + Key;

                    ActiveLine := GetActiveLineFromEntryString (EntryString);

                    IF Length (EntryString) = 1 THEN
                        FirstEntryShown := ActiveLine
                    ELSE
                        IF Ord (ActiveLine) - Ord (FirstEntryShown) > 16 THEN
                            REPEAT
                                FirstEntryShown := Succ (FirstEntryShown);
                            UNTIL Ord (ActiveLine) - Ord (FirstEntryShown) <= 10;

                    END;

            END;

    Wait (4);  { Six meter noise suppression }

    UNTIL False
    END;



    BEGIN
    END.

