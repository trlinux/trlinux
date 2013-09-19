UNIT JCtrl1;

{$O+}

INTERFACE

USES Tree, LogStuff, LogGrid, LogSCP, LogCW, LogWind, LogDupe, ZoneCont,
     LogCfg, LogDom, LogDVP, Country9, LogEdit, trCrt, LogK1EA, DOS, LogHelp,
     SlowTree, LogWAE, LogPack, LogDDX, K1EANet;


TYPE MenuEntryType = (NoMenuEntry,
                      ACC,
                      ABE,
                      ABC,
                      AFF,
                      AIO,
                      ACT,
                      ADP,
                      ADE,
                      ADS,
                      AQI,
                      AQD,
                      ASP,
                      ASR, {KK1L: 6.72}
                      ARC,
                      ASC,
                      ATI,
                      BEN,
                      BAB,
                      BAM,
                      BCW,
                      BMD,
                      {BMO, {KK1L: 6.xx}
                      BCQ,
                      BDD,
                      BME,
                      BMG,
                      BSM,
                      BNA,
                      BSA,
                      BET,
                      BRL,
                      BPD,
                      SAS,
                      CAU,
                      CLF,
                      CDE,
                      CID,
                      CEC,
                      CIF,
                      CKM,
                      CWE,
                      CWS,
                      CSI, {KK1L: 6.72}
                      CWT,
                      DEE,
                      DIG,
                      DIS,
                      DMF,
                      DCS,
                      DSE,
                      DVK,
                      DVE,
                      DVP,
                      EES,
                      EME,
                      FWE,
                      FWS,
                      FSF,
                      FSE,
                      FSM,
                      FA1,
                      FA2,
                      FPR, {KK1L: 6.71a FrequencyPollRate}
                      FME,
                      FCR,
                      GMC,
                      HFE,
                      HDP,
                      HOF,
                      ICP,
                      IRT,
                      ITE,
                      IEX,
                      IXO, {KK1L: 6.70}
                      IEC,
                      IFE,
                      JRT,
                      KNE,
                      KSI,
                      KRT,
                      KCM,
                      LDZ,
                      LZC,
                      LCI,
                      LDQ,
                      LFE,
                      LRS,
                      LRT,
                      LSE,
                      LFR,
                      MSE,
                      MEN,
                      MRM,
                      MIM,
                      MMO,
                      MRT,
                      MUM,
                      MBA,
                      MMD,
                      MCL,
                      MCN,
                      MCU,
                      MFD,
                      MGR,
                      MIO,
                      MZN,
                      NFE,
                      NLQ,
                      NPP,
                      PAL,
                      PAR,
                      PBS,
                      PBP,
                      PLF,
                      PRM,
                      PSC, {KK1L: 6.71 Coded for PacketSpotComment started in 6.68}
                      PKD,
                      PSE,
                      SPO, {KK1L: 6.72}
                      PSP,
                      PBE,
                      PMT,
                      PHC,
                      PSD,
                      PCE,
                      PCL,
                      PCM,
                      PR1, {KK1L: 6.72 PollRadioOne}
                      PR2, {KK1L: 6.72 PollRadioOne}
                      PCA,
                      PCN,
                      PEN,
                      PTT,
                      PTD,
                      QMD,
                      QNB,
                      QSX,
                      QES,
                      QRS,
                      QMC,
                      R1I,
                      R1T,
                      R1U,
                      R2I,
                      R2T,
                      R2U,
                      RCQ,
                      RDS,
                      RMD,
                      SHE,
                      SO2RHM,
                      SO2RBE,
                      SO2RBV,
                      SO2RMR,
                      SO2RM1,
                      SO2RM2,
                      SHC,
                      SCS,
                      SML,
                      SAD,
                      SCF,
                      SQI,
                      SKE,
                      SIN,
                      SSP,
                      SEN,
                      SRM,
                      SAB,
                      SMC,
                      SBD,
                      SQR,
                      SPS, {KK1L: 6.71 StereoPinState}
                      SRP,
                      SWP,
                      SWR,
                      TAB,
                      TMR,
                      TOT,
                      TDE, {KK1L: 6.73 TuneDupeCheckEnable}
                      TWD,
                      TRM,
                      URF,
                      UIS,
                      VER,
                      VDE,
                      VBE,
                      VDS,
                      WFS,
                      WUT,
                      WBE,
                      WEI,
                      WCP,
                      YRT,
                      LastMenuEntry);


VAR FileRead: TEXT;
    ChangedRemainingMults: BOOLEAN;
    DisplayString: Str80;
    Changed: ARRAY [MenuEntryType] OF BOOLEAN;

FUNCTION  Description (Line: MenuEntryType): Str80;
PROCEDURE DisplayStatusLine (Line: MenuEntryType; Active: BOOLEAN);
PROCEDURE DisplayInfoLine (Line: MenuEntryType; Active: BOOLEAN);

IMPLEMENTATION

uses linuxsound,beep,foot,keyers,so2r;

FUNCTION Description (Line: MenuEntryType): Str80;

    BEGIN
    CASE Line OF
      NoMenuEntry: Description := '0';

      ACC: Description := 'ALL CW MESSAGES CHAINABLE';
      ABE: Description := 'ALT-D BUFFER ENABLE';
      ABC: Description := 'ALWAYS CALL BLIND CQ';
      AFF: Description := 'ASK FOR FREQUENCIES';
      AIO: Description := 'ASK IF CONTEST OVER';
      ACT: Description := 'AUTO CALL TERMINATE';
      ADP: Description := 'AUTO DISPLAY DUPE QSO';
      ADE: Description := 'AUTO DUPE ENABLE CQ';
      ADS: Description := 'AUTO DUPE ENABLE S AND P';
      AQI: Description := 'AUTO QSL INTERVAL';
      AQD: Description := 'AUTO QSO NUMBER DECREMENT';
      ASP: Description := 'AUTO S&P ENABLE';
      ASR: Description := 'AUTO S&P ENABLE SENSITIVITY'; {KK1L: 6.72}
      ARC: Description := 'AUTO RETURN TO CQ MODE';
      ASC: Description := 'AUTO SEND CHARACTER COUNT';
      ATI: Description := 'AUTO TIME INCREMENT';

      BEN: Description := 'BACKCOPY ENABLE';
      BAB: Description := 'BAND MAP ALL BANDS';
      BAM: Description := 'BAND MAP ALL MODES';
      BCW: Description := 'BAND MAP CALL WINDOW ENABLE';
      BMD: Description := 'BAND MAP DECAY TIME';
      BCQ: Description := 'BAND MAP DISPLAY CQ';
      BDD: Description := 'BAND MAP DUPE DISPLAY';
      BME: Description := 'BAND MAP ENABLE';
      BMG: Description := 'BAND MAP GUARD BAND';
      BSM: Description := 'BAND MAP SPLIT MODE';
      BNA: Description := 'BEEP ENABLE';
      BSA: Description := 'BEEP SOUNDCARD ENABLE';
      BET: Description := 'BEEP EVERY 10 QSOS';
      BRL: Description := 'BIG REMAINING LIST';
      BPD: Description := 'BROADCAST ALL PACKET DATA';

      {KK1L: 6.65}
      SAS: Description := 'CALL WINDOW SHOW ALL SPOTS';
      CAU: Description := 'CALLSIGN UPDATE ENABLE';
      CLF: Description := 'CHECK LOG FILE SIZE';
      CDE: Description := 'COLUMN DUPESHEET ENABLE';
      CID: Description := 'COMPUTER ID';
      CEC: Description := 'CONFIRM EDIT CHANGES';
      CIF: Description := 'COUNTRY INFORMATION FILE';
      CKM: Description := 'CURTIS KEYER MODE';
      CWE: Description := 'CW ENABLE';
      CWS: Description := 'CW SPEED FROM DATABASE';
      CSI: Description := 'CW SPEED INCREMENT'; {KK1L: 6.72}
      CWT: Description := 'CW TONE';

      DEE: Description := 'DE ENABLE';
      DIG: Description := 'DIGITAL MODE ENABLE';
      DIS: Description := 'DISTANCE MODE';
      DMF: Description := 'DOMESTIC FILENAME';
      DCS: Description := 'DUPE CHECK SOUND';
      DSE: Description := 'DUPE SHEET ENABLE';
      DVK: Description := 'DVK PORT';
      DVE: Description := 'DVP ENABLE';
      DVP: Description := 'DVP PATH';

      EES: Description := 'ESCAPE EXITS SEARCH AND POUNCE';
      EME: Description := 'EXCHANGE MEMORY ENABLE';

      FWE: Description := 'FARNSWORTH ENABLE';
      FWS: Description := 'FARNSWORTH SPEED';
      FSF: Description := 'FLOPPY FILE SAVE FREQUENCY';
      FSE: Description := 'FLOPPY FILE SAVE NAME';
      FSM: Description := 'FOOT SWITCH MODE';
      FA1: Description := 'FREQUENCY ADDER RADIO ONE';
      FA2: Description := 'FREQUENCY ADDER RADIO TWO';
      FPR: Description := 'FREQUENCY POLL RATE'; {KK1L: 6.71a}
      FME: Description := 'FREQUENCY MEMORY ENABLE';
      FCR: Description := 'FT1000MP CW REVERSE';

      GMC: Description := 'GRID MAP CENTER';

      HFE: Description := 'HF BAND ENABLE';
      HDP: Description := 'HOUR DISPLAY';
      HOF: Description := 'HOUR OFFSET';

      ICP: Description := 'ICOM COMMAND PAUSE';
      IRT: Description := 'ICOM RESPONSE TIMEOUT';
      ITE: Description := 'INCREMENT TIME ENABLE';
      IFE: Description := 'INTERCOM FILE ENABLE';
      IEX: Description := 'INITIAL EXCHANGE';
      IXO: Description := 'INITIAL EXCHANGE OVERWRITE'; {KK1L: 6.70}
      IEC: Description := 'INITIAL EXCHANGE CURSOR POS';

      JRT: Description := 'JST RESPONSE TIMEOUT';

      KNE: Description := 'K1EA NETWORK ENABLE';
      KSI: Description := 'K1EA STATION ID';
      KRT: Description := 'KENWOOD RESPONSE TIMEOUT';
      KCM: Description := 'KEYPAD CW MEMORIES';

      LDZ: Description := 'LEADING ZEROS';
      LZC: Description := 'LEADING ZERO CHARACTER';
      LCI: Description := 'LEAVE CURSOR IN CALL WINDOW';
      LDQ: Description := 'LITERAL DOMESTIC QTH';
      LFE: Description := 'LOG FREQUENCY ENABLE';
      LRS: Description := 'LOG RS SENT';
      LRT: Description := 'LOG RST SENT';
      LSE: Description := 'LOG WITH SINGLE ENTER';
      LFR: Description := 'LOOK FOR RST SENT';

      MSE: Description := 'MESSAGE ENABLE';
      MEN: Description := 'MOUSE ENABLE';
      MRM: Description := 'MULT REPORT MINIMUM BANDS';
      MIM: Description := 'MULTI INFO MESSAGE';
      MMO: Description := 'MULTI MULTS ONLY';
      MRT: Description := 'MULTI RETRY TIME';
      MUM: Description := 'MULTI UPDATE MULT DISPLAY';
      MBA: Description := 'MULTIPLE BANDS';
      MMD: Description := 'MULTIPLE MODES';
      MCL: Description := 'MY CALL';
      MCN: Description := 'MY CONTINENT';
      MCU: Description := 'MY COUNTRY';
      MFD: Description := 'MY FD CLASS';
      MGR: Description := 'MY GRID';
      MIO: Description := 'MY IOTA';
      MZN: Description := 'MY ZONE';

      NFE: Description := 'NAME FLAG ENABLE';
      NLQ: Description := 'NO LOG';
      NPP: Description := 'NO POLL DURING PTT';

      PAL: Description := 'PACKET ADD LF';
      PAR: Description := 'PACKET AUTO CR';
      PBS: Description := 'PACKET BAND SPOTS';
      PBP: Description := 'PACKET BEEP';
      PLF: Description := 'PACKET LOG FILENAME';
      PRM: Description := 'PACKET RETURN PER MINUTE';
      PSC: Description := 'PACKET SPOT COMMENT'; {KK1L: 6.71 Implimented what I started in 6.68}
      PKD: Description := 'PACKET SPOT DISABLE';
      PSE: Description := 'PACKET SPOT EDIT ENABLE';
      SPO: Description := 'PACKET SPOT PREFIX ONLY'; {KK1L: 6.72}
      PSP: Description := 'PACKET SPOTS';
      PBE: Description := 'PADDLE BUG ENABLE';
      PMT: Description := 'PADDLE MONITOR TONE';
      PHC: Description := 'PADDLE PTT HOLD COUNT';
      PSD: Description := 'PADDLE SPEED';
      PCE: Description := 'PARTIAL CALL ENABLE';
      PCL: Description := 'PARTIAL CALL LOAD LOG ENABLE';
      PCM: Description := 'PARTIAL CALL MULT INFO ENABLE';
      PR1: Description := 'POLL RADIO ONE';
      PR2: Description := 'POLL RADIO TWO';
      PCA: Description := 'POSSIBLE CALLS';
      PCN: Description := 'POSSIBLE CALL MODE';
      PEN: Description := 'PRINTER ENABLE';
      PTT: Description := 'PTT ENABLE';
      PTD: Description := 'PTT TURN ON DELAY';

      QMD: Description := 'QSL MODE';
      QNB: Description := 'QSO NUMBER BY BAND';
      QSX: Description := 'QSX ENABLE';
      QES: Description := 'QTC EXTRA SPACE';
      QRS: Description := 'QTC QRS';
      QMC: Description := 'QUESTION MARK CHAR';

      R1I: Description := 'RADIO ONE ID CHARACTER';
      R1T: Description := 'RADIO ONE TRACKING ENABLE';
      R1U: Description := 'RADIO ONE UPDATE SECONDS';
      R2I: Description := 'RADIO TWO ID CHARACTER';
      R2T: Description := 'RADIO TWO TRACKING ENABLE';
      R2U: Description := 'RADIO TWO UPDATE SECONDS';
      RCQ: Description := 'RANDOM CQ MODE';
      RDS: Description := 'RATE DISPLAY';
      RMD: Description := 'REMAINING MULT DISPLAY MODE';

      SO2RHM: Description := 'SO2R HEADPHONE MODE';
      SO2RBE: Description := 'SO2R BLEND ENABLE';
      SO2RBV: Description := 'SO2R BLEND';
      SO2RMR: Description := 'SO2R MICROPHONE RELAY ENABLE';
      SO2RM1: Description := 'SO2R RIG1 MAP';
      SO2RM2: Description := 'SO2R RIG2 MAP';
      SHE: Description := 'SAY HI ENABLE';
      SHC: Description := 'SAY HI RATE CUTOFF';
      SCS: Description := 'SCP COUNTRY STRING';
      SML: Description := 'SCP MINIMUM LETTERS';
      SAD: Description := 'SEND ALT-D SPOTS TO PACKET';
      SCF: Description := 'SEND COMPLETE FOUR LETTER CALL';
      SQI: Description := 'SEND QSO IMMEDIATELY';
      SKE: Description := 'SHIFT KEY ENABLE';
      SIN: Description := 'SHORT INTEGERS';
      SSP: Description := 'SHOW SEARCH AND POUNCE';
      SEN: Description := 'SIMULATOR ENABLE';
      SRM: Description := 'SINGLE RADIO MODE';
      SAB: Description := 'SKIP ACTIVE BAND';
      SMC: Description := 'SLASH MARK CHAR';
      SBD: Description := 'SPACE BAR DUPE CHECK ENABLE';
      SQR: Description := 'SPRINT QSY RULE';
      SPS: Description := 'STEREO PIN HIGH'; {KK1L: 6.71}
      SRP: Description := 'SWAP PACKET SPOT RADIOS';
      SWP: Description := 'SWAP PADDLES';
      SWR: Description := 'SWAP RADIO RELAY SENSE';

      TAB: Description := 'TAB MODE';
      TMR: Description := 'TEN MINUTE RULE';
      TOT: Description := 'TOTAL OFF TIME';
      TDE: Description := 'TUNE ALT-D ENABLE'; {KK1L: 6.73}
      TWD: Description := 'TUNE WITH DITS';
      TRM: Description := 'TWO RADIO MODE';

      URF: Description := 'UPDATE RESTART FILE ENABLE';
      UIS: Description := 'USER INFO SHOWN';

      VER: Description := 'VERSION';
      VDE: Description := 'VGA DISPLAY ENABLE';
      VBE: Description := 'VHF BAND ENABLE';
      VDS: Description := 'VISIBLE DUPESHEET';

      WFS: Description := 'WAIT FOR STRENGTH';
      WUT: Description := 'WAKE UP TIME OUT';
      WBE: Description := 'WARC BAND ENABLE';
      WEI: Description := 'WEIGHT';
      WCP: Description := 'WILDCARD PARTIALS';

      YRT: Description := 'YAESU RESPONSE TIMEOUT';
      LastMenuEntry: Description := 'ZZZ';
      ELSE Description := '???';
      END;

    END;



PROCEDURE DisplayStatusLine (Line: MenuEntryType; Active: BOOLEAN);

VAR ChangedRemainingMults: BOOLEAN;

    BEGIN
    IF Active THEN
        BEGIN
        TextColor (ActiveBackground);
        TextBackground (ActiveColor);
        END;

    Write (Description (Line), ' = ');

    CASE Line OF
      ACC: Write (AllCWMessagesChainable);
      ABE: Write (AltDBufferEnable);
      ABC: Write (AlwaysCallBlindCQ);
      AFF: Write (AskForFrequencies);
      AIO: Write (AskIfContestOver);
      ACT: Write (AutoCallTerminate);
      ADP: Write (AutoDisplayDupeQSO);
      ADE: Write (AutoDupeEnableCQ);
      ADS: Write (AutoDupeEnableSAndP);
      AQI: Write (AutoQSLInterval);
      AQD: Write (AutoQSONumberDecrement);
      ASP: Write (AutoSAPEnable);
      ASR: Write (AutoSAPEnableRate); {KK1L: 6.72}
      ARC: Write (AutoReturnToCQMode);
      ASC: Write (AutoSendCharacterCount);
      ATI: Write (AutoTimeIncrementQSOs);

      BEN: Write (BackCopyEnable);
      BAB: Write (BandMapAllBands);
      BAM: Write (BandMapAllModes);
      {BMO: Write (BandMapMultsOnly); {KK1L: 6.xx}
      BCW: Write (BandMapCallWindowEnable);
      BMD: Write (BandMapDecayValue);
      BCQ: Write (BandMapDisplayCQ);
      BDD: Write (BandMapDupeDisplay);
      BME: Write (BandMapEnable);
      BMG: Write (BandMapGuardBand);

      {KK1L: 6.64}
      BSM: CASE BandMapSplitMode OF
               ByCutoffFrequency: Write ('BY CUTOFF FREQ');
               AlwaysPhone:       Write ('ALWAYS PHONE');
               END;

      BNA: Write (Tone.GetBeepEnable);
      BSA: Write (BeepSoundCardEnable);
      BET: Write (BeepEvery10QSOs);
      BRL: Write (BigRemainingList);
      BPD: Write (Packet.BroadcastAllPacketData);

      CAU: Write (CallsignUpdateEnable);
      CLF: Write (CheckLogFileSize);
      CDE: Write (ColumnDupeSheetEnable);
      CID: Write (ComputerID);
      CEC: Write (ConfirmEditChanges);
      CIF: Write (CountryInformationFile);

      CKM: IF ActiveKeyer.GetCurtisMode = ModeA then
           begin
               Write ('A');
           end
           else
           begin
              if ActiveKeyer.GetCurtisMode = Ultimatic then
                 write('U') else Write ('B');
           end;

      CWE: Write (CWEnable);
      CWS: Write (CWSpeedFromDataBase);
      CSI: Write (CodeSpeedIncrement); {KK1L: 6.72}
      CWT: Write (CWTone);

      DEE: Write (DEEnable);
      DIG: Write (DigitalModeEnable);

      DIS: CASE DistanceMode OF
               NoDistanceDisplay: Write ('NONE');
               DistanceMiles:     Write ('MILES');
               DistanceKM:        Write ('KM');
               END;

      DMF: Write (DomQTHTable.ActiveDomQTHFile);

      DCS: CASE DupeCheckSound OF
               DupeCheckNoSound:     Write ('NONE');
               DupeCheckBeepIfDupe:  Write ('DUPE BEEP');
               DupeCheckGratsIfMult: Write ('MULT FANFARE');
               END;

      DSE: Write (Sheet.DupeSheetEnable);
      DVK: begin
         if ActiveDVKPort = nil then
            write('No Port')
         else
            write(ActiveDVKPort.devname);
      end;

      DVE: Write (DVPEnable);
      DVP: Write (DVPPath);
      EES: Write (EscapeExitsSearchAndPounce);
      EME: Write (ExchangeMemoryEnable);
      FWE: Write (ActiveKeyer.GetFarnsworthEnable);
      FWS: Write (ActiveKeyer.GetFarnsworthSpeed);
      FSF: Write (FloppyFileSaveFrequency);
      FSE: Write (FloppyFileSaveName);

      FSM: CASE FootSwitchMode OF
               FootSwitchDisabled:                  Write ('DISABLED');
               FootSwitchF1:                        Write ('F1');
               FootSwitchLastCQFreq:                Write ('LAST CQ FREQ');
               FootSwitchNextBandMap:               Write ('NEXT BANDMAP');
               FootSwitchNextDisplayedBandMap:      Write ('NEXT DISP BANDMAP'); {KK1L: 6.64}
               FootSwitchNextMultBandMap:           Write ('NEXT MULT BANDMAP'); {KK1L: 6.68}
               FootSwitchNextMultDisplayedBandMap:  Write ('NEXT MULT DISP BANDMAP'); {KK1L: 6.68}
               FootSwitchDupeCheck:                 Write ('DUPE CHECK');
               Normal:                              Write ('NORMAL');
               QSONormal:                           Write ('QSO NORMAL');
               QSOQuick:                            Write ('QSO QUICK');
               FootSwitchControlEnter:              Write ('CONTROL ENTER');
               StartSending:                        Write ('START SENDING');
               SwapRadio:                           Write ('SWAP RADIOS');
               CWGrant:                             Write ('CW GRANT');
               END;

      FA1: Write (Radio1FrequencyAdder);
      FA2: Write (Radio2FrequencyAdder);
      FPR: Write (FreqPollRate); {KK1L: 6.71a}
      FME: Write (FrequencyMemoryEnable);
      FCR: Write (FT1000MPCWReverse);
      GMC: Write (GridMapCenter);
      HFE: Write (HFBandEnable);

      HDP: CASE HourDisplay OF
               ThisHour:      Write ('THIS HOUR');
               LastSixtyMins: Write ('LAST SIXTY MINUTES');
               BandChanges:   Write ('BAND CHANGES');
               END;

      HOF: Write (HourOffset);

      ICP: Write (IcomCommandPause);
      IRT: Write (IcomResponseTimeout);

      ITE: Write (IncrementTimeEnable);
      IFE: Write (IntercomFileEnable);

      IEX: CASE ActiveInitialExchange OF
               NoInitialExchange:           Write ('NONE');
               NameInitialExchange:         Write ('NAME');
               NameQTHInitialExchange:      Write ('NAME QTH');
               CheckSectionInitialExchange: Write ('CHECK SECTION');
               SectionInitialExchange:      Write ('SECTION');
               QTHInitialExchange:          Write ('QTH');
               FOCInitialExchange:          Write ('FOC NUMBER');
               GridInitialExchange:         Write ('GRID');
               ZoneInitialExchange:         Write ('ZONE');
               User1InitialExchange:        Write ('USER 1');
               User2InitialExchange:        Write ('USER 2');
               User3InitialExchange:        Write ('USER 3');
               User4InitialExchange:        Write ('USER 4');
               User5InitialExchange:        Write ('USER 5');
               CustomInitialExchange:       Write ('CUSTOM');
               END;

      IEC: CASE InitialExchangeCursorPos OF
               AtStart: Write ('AT START');
               AtEnd:   Write ('AT END');
               END;

      IXO: Write (InitialExchangeOverwrite); {KK1L: 6.70}

      JRT: Write (JSTResponseTimeout);
      KNE: Write (K1EANetworkEnable);
      KSI: Write (K1EAStationID);
      KRT: Write (KenwoodResponseTimeout);
      KCM: Write (KeyPadCWMemories);
      LDZ: Write (LeadingZeros);
      LZC: Write (LeadingZeroCharacter);
      LCI: Write (LeaveCursorInCallWindow);
      LFE: Write (LogFrequencyEnable);
      LRS: Write (LogRSSent);
      LDQ: Write (LiteralDomesticQTH);
      LRT: Write (LogRSTSent);
      LSE: Write (LogWithSingleEnter);
      LFR: Write (LookForRSTSent);
      MSE: Write (MessageEnable);
      MEN: Write (MouseEnable);
      MRM: Write (MultReportMinimumBands);
      MIM: Write (MultiInfoMessage);
      MMO: Write (MultiMultsOnly);
      MRT: Write (MultiRetryTime);
      MUM: Write (MultiUpdateMultDisplay);
      MBA: Write (MultipleBandsEnabled);
      MMD: Write (MultipleModesEnabled);
      MCL: Write (MyCall);
      MCN: Write (GetContinentName (MyContinent));
      MCU: Write (MyCountry);
      MFD: Write (MyFDClass);
      MGR: Write (MyGrid);
      MIO: Write (MyIOTA);
      MZN: Write (MyZone);
      NFE: Write (NameFlagEnable);
      NLQ: Write (NoLog);
      NPP: Write (NoPollDuringPTT);

      PAL: Write (PacketAddLF);
      PAR: Write (PacketAutoCR);
      PBS: Write (Packet.PacketBandSpots);
      PBP: Write (Packet.PacketBeep);
      PLF: Write (Packet.PacketLogFileName);

      PRM: Write (PacketReturnPerMinute);
      PSC: Write (PacketSpotComment); {KK1L: 6.71 Implimented what I started in 6.68}
      PKD: Write (PacketSpotDisable);
      PSE: Write (PacketSpotEditEnable);
      SPO: Write (PacketSpotPrefixOnly); {KK1L: 6.72}

      PSP: IF Packet.PacketSpots = AllSpots THEN
               Write ('ALL')
           ELSE
               Write ('MULT');

      PBE: Write (ActiveKeyer.GetPaddleBug);
      PMT: Write (ActiveKeyer.GetPaddleMonitorTone);
      PHC: Write (ActiveKeyer.GetPaddlePTTHoldCount);
      PSD: Write (ActiveKeyer.GetPaddleSpeed);
      PCE: Write (PartialCallEnable);
      PCL: Write (PartialCallLoadLogEnable);
      PCM: Write (PartialCallMultsEnable);
      PR1: Write (PollRadioOne); {KK1L: 6.72 PollRadioOne}
      PR2: Write (PollRadioTwo); {KK1L: 6.72 PollRadioTwo}
      PCA: Write (PossibleCallEnable);

      PCN: CASE CD.PossibleCallAction OF  {KK1L: 6.69 support log only check partial}
               AnyCall:            Write ('ALL');
               OnlyCallsWithNames: Write ('NAMES');
               LogOnly:            Write ('LOG ONLY');
               END;

      PEN: Write (PrinterEnabled);
      PTT: Write (ActiveKeyer.GetPTTEnable);
      PTD: Write (ActiveKeyer.GetPTTTurnOnDelay);

      QMD: CASE ParameterOkayMode OF
               Standard:       Write ('Standard');
               QSLButdoNotLog: Write ('QSL but no log');
               QSLAndLog:      Write ('QSL and log');
               END;

      QNB: Write (QSONumberByBand);
      QES: Write (QTCExtraSpace);
      QRS: Write (QTCQRS);
      QSX: Write (QSXEnable);

      QMC: Write (QuestionMarkChar);

      R1I: Write (Radio1IDCharacter);
      R1T: Write (Radio1TrackingEnable);
      R1U: Write (Radio1UpdateSeconds);
      R2I: Write (Radio2IDCharacter);
      R2T: Write (Radio2TrackingEnable);
      R2U: Write (Radio2UpdateSeconds);

      RCQ: Write (RandomCQMode);

      RDS: CASE RateDisplay OF
               QSOs:   Write ('QSOs');
               Points: Write ('QSO Points');
               END;

      RMD: CASE RemainingMultDisplayMode OF
               NoRemainingMults: Write ('None');
               Erase:            Write ('Erase');
               HiLight:          Write ('HiLight');
               END;

      SO2RHM: Case so2rbox.getheadphonemode of
         HNORMAL: write ('NORMAL');
         HSYMMETRIC: write ('SYMMETRIC');
         HSPATIAL: write ('SPATIAL');
         end;

      SO2RBE: write(so2rbox.getblend);
      SO2RBV: write(so2rbox.getblendvalue);
      SO2RMR: write(so2rbox.getmicrelay);
      SO2RM1: write(so2rbox.getrig1map);
      SO2RM2: write(so2rbox.getrig2map);

      SHE: Write (SayHiEnable);
      SHC: Write (SayHiRateCutoff);
      SCS: Write (CD.CountryString);
      SML: Write (SCPMinimumLetters);
      SAD: Write (SendAltDSpotsToPacket);
      SCF: Write (SendCompleteFourLetterCall);
      SPS: Write (StereoPinState); {KK1L: 6.71}
      SQI: Write (SendQSOImmediately);
      SKE: Write (ShiftKeyEnable);
      SIN: Write (ShortIntegers);
      SSP: Write (ShowSearchAndPounce);

      SEN: IF DDXState = Off THEN
               Write ('FALSE')
           ELSE
               Write ('TRUE');

      SRM: Write (SingleRadioMode);
      SAB: Write (SkipActiveBand);
      SAS: Write (CallWindowShowAllSpots);
      SMC: Write (SlashMarkChar);
      SBD: Write (SpaceBarDupeCheckEnable);
      SQR: Write (SprintQSYRule);
      SRP: Write (SwapPacketSpotRadios);
      SWP: Write (ActiveKeyer.GetSwapPaddles);
      SWR: Write (SwapRadioRelaySense);

      TAB: BEGIN
           IF TabMode = NormalTabMode   THEN Write ('NORMAL');
           IF TabMode = ControlFTabMode THEN Write ('CONTROLF');
           END;

      TMR: CASE TenMinuteRule OF
               NoTenMinuteRule: Write ('NONE');
               TimeOfFirstQSO:  Write ('TIME OF FIRST QSO');
               END;

      TOT: Write (MinutesToTimeString (TotalOffTime));

      TDE: Write (TuneDupeCheckEnable); {KK1L: 6.73}

      TWD: Write (ActiveKeyer.GetTuneWithDits);

      TRM: IF TwoRadioState <> TwoRadiosDisabled THEN
               Write ('TRUE')
           ELSE
               Write ('FALSE');

      URF: Write (UpdateRestartFileEnable);

      UIS: CASE UserInfoShown OF
               NoUserInfo: Write ('NONE');
               NameInfo:         Write ('NAME');
               QTHInfo:          Write ('QTH');
               CheckSectionInfo: Write ('CHECK SECTION');
               SectionInfo:      Write ('SECTION');
               OldCallInfo:      Write ('OLD CALL');
               FOCInfo:          Write ('FOC NUMBER');
               GridInfo:         Write ('GRID');
               ITUZoneInfo:      Write ('ITU ZONE');
               CQZoneInfo:       Write ('CQ ZONE');
               User1Info:        Write ('USER 1');
               User2Info:        Write ('USER 2');
               User3Info:        Write ('USER 3');
               User4Info:        Write ('USER 4');
               User5Info:        Write ('USER 5');
               CustomInfo:       Write ('CUSTOM');
               END;

      VER: Write (Version);
      VDE: Write (VGADisplayEnable);
      VBE: Write (VHFBandsEnabled);
      VDS: Write (VisibleDupesheetEnable);
      WFS: Write (WaitForStrength);
      WUT: Write (WakeUpTimeOut);
      WBE: Write (WARCBandsEnabled);
      WEI: Write (ActiveKeyer.GetWeight:3:2);
      WCP: Write (WildCardPartials);
      YRT: Write (YaesuResponseTimeout);
      ELSE Exit;
      END;

    END;



PROCEDURE DisplayInfoLine (Line: MenuEntryType; Active: BOOLEAN);

    BEGIN
    CASE Line OF
      ACC: IF AllCWMessagesChainable THEN
               Write ('All CW messages can be chained together')
           ELSE
               Write ('Only msgs with ^D at start will chain');

      ABE: IF AltDBufferEnable THEN
               Write ('Alt-D starts with bandmap or last entry')
           ELSE
               Write ('Alt-D prompt has no initial call shown');

      ABC: IF AlwaysCallBlindCQ THEN
               Write ('EX F7 always sent after CQ EXCHANGE')
           ELSE
               Write ('Blind CQs disabled');

      AFF: IF AskForFrequencies THEN
               Write ('Ask for band map freqs if no interface')
           ELSE
               Write ('Do not prompt for band map frequencies');

      AIO: IF AskIfContestOver THEN
               Write ('When program exit, ask if contest over')
           ELSE
               Write ('Do not ask if contest over when exiting');

      ACT: IF AutoCallTerminate THEN
               Write ('Start exchange when auto CW done')
           ELSE
               Write ('Requires ENTER key before exchange sent');

      ADP: IF AutoDisplayDupeQSO THEN
               Write ('Display previous dupes of station')
           ELSE
               Write ('Do not display previous dupe QSOs');

      ADE: IF AutoDupeEnableCQ THEN
               Write ('Send QSO BEFORE MESSAGE to dupes')
           ELSE
               Write ('Work and log dupes in CQ mode');

      ADS: IF AutoDupeEnableSAndP THEN
               Write ('Do not call dupes in S&P with RETURN')
           ELSE
               Write ('Call dupes in S&P mode with RETURN');

      AQI: IF AutoQSLInterval > 0 THEN
               Write ('Number QSOs that use QUICK QSL message')
           ELSE
               Write ('Always use QSL MESSAGE when QSLing');

      AQD: IF AutoQSONumberDecrement THEN
               Write ('If in S&P & blank windows, decrement #')
           ELSE
               Write ('No auto decrement if in S&P & no input');

      ASP: IF AutoSAPEnable THEN
               Write ('Jump into S&P mode if tuning VFO')
           ELSE
               Write ('Do not jump into S&P mode when tuning');

      ASR: Write ('Auto SAP Enable Sensitivity (Hz/sec)'); {KK1L: 6.72}

      ARC: IF AutoReturnToCQMode THEN
               Write ('CQ F1 if RETURN in S&P & blank windows')
           ELSE
               Write ('Stay in S&P if RETURN with blank windows');

      ASC: IF AutoSendCharacterCount = 0 THEN
               Write ('Auto start send feature disabled')
           ELSE
               Write ('Char position where auto CW starts');

      ATI: IF AutoTimeIncrementQSOs > 0 THEN
               Write ('Number QSOs for auto minute increment')
           ELSE
               Write ('Auto time increment disabled');

      BEN: IF BackCopyEnable THEN
               Write ('DVP BackCopy is enabled')
           ELSE
               Write ('DVP BackCopy is disabled');

      BAB: IF BandMapAllBands THEN
               Write ('All bands shown on band map')
           ELSE
               Write ('Only active band shown on band map');

      BAM: IF BandMapAllModes THEN
               Write ('All modes shown on band map')
           ELSE
               Write ('Only active mode shown on band map');

      {KK1L: 6.xx}
      {BMO: IF BandMapMultsOnly THEN
               Write ('Only multipliers shown on band map')
           ELSE
               Write ('Not only multipliers shown on band map');}

      BCW: IF BandMapCallWindowEnable THEN
               Write ('Band map blinking call in call window')
           ELSE
               Write ('No band map calls in call window');

      BMD: Write ('Band map entry decay time (minutes)'); {KK1L: 6.65}

      BCQ: IF BandMapDisplayCQ THEN
               Write ('CQs entered into bandmap')
           ELSE
               Write ('CQs not entered into bandmap');

      BDD: IF BandMapDupeDisplay THEN
               Write ('Band map shows all calls - even dupes')
           ELSE
               Write ('Band map does not show dupes');

      BME: IF BandMapEnable THEN
               Write ('Band map enabled (needs 42/50 lines)')
           ELSE
               Write ('Band map display is disabled');

      BMG: Write ('Blink if freq is within this limit (hz)');

      {KK1L: 6.64}
      BSM: CASE BandMapSplitMode OF
               ByCutoffFrequency: Write ('Use BandMapCutoffFrequency to set mode.');
               AlwaysPhone:       Write ('Split entries always phone mode.');
               END;

      BNA: IF Tone.GetBeepEnable THEN
               Write ('Beeps enabled')
           ELSE
               Write ('Beeps disabled - computer speaker quiet');

      BSA: IF BeepSoundCardEnable THEN
               Write ('Soundcard Beeps')
           ELSE
               Write ('PC Speaker Beeps');

      BET: IF BeepEvery10QSOs THEN
               Write ('Short beep after each 10th QSO')
           ELSE
               Write ('No beep to signal each 10th QSO');

      BRL: IF BigRemainingList THEN
               Write ('Large window for remaining mults')
           ELSE
               Write ('Normal remaining mults window');

      BPD: IF Packet.BroadcastAllPacketData THEN
               Write ('All packet data sent to network')
           ELSE
               Write ('Only spots and talk data to network');

      {KK1L: 6.65}
      SAS: IF CallWindowShowAllSpots THEN
               Write ('All spots shown in call window')
           ELSE
               Write ('Displayed spots shown in call window');

      CAU: IF CallsignUpdateEnable THEN
               Write ('Updated calls looked for in exchange')
           ELSE
               Write ('No call updates looked for in exchange');

      CLF: IF CheckLogFileSize THEN
               Write ('Log file size checked after each QSO')
           ELSE
               Write ('No special checking of log file size');

      CDE: IF ColumnDupeSheetEnable THEN
               Write ('Vis dupesheet uses new column/district')
           ELSE
               Write ('Visible sheet runs districts together');

      CID: IF ComputerID = Chr (0) THEN
               Write ('No computer ID set (used for multi')
           ELSE
               Write ('Computer ID as shown appears in log');

      CEC: IF ConfirmEditChanges THEN
               Write ('Prompt for Y key when exiting AltE')
           ELSE
               Write ('Save AltE changes without asking if ok');

      CIF: Write ('Name of country information file');

      CKM: Write ('Select desired keyer operation mode');

      CWE: IF CWEnable THEN
               Write ('CW enabled')
           ELSE
               Write ('CW disabled (except from paddle)');

      CWS: IF CWSpeedFromDataBase THEN
               Write ('CQ exchange speed in WPM from TRMASTER')
           ELSE
               Write ('Exchange speed from database disabled');

      CSI: Write ('PGUP/PGDN increment from 1 to 10 WPM'); {KK1L: 6.72}

      CWT: BEGIN
           IF CWTone > 0 THEN
               Write ('Computer speaker CW monitor in Hertz')
           ELSE
               Write ('Computer speaker CW monitor disabled');
           LNoSound;
           END;

      DEE: IF DEEnable THEN
               Write ('Send DE when calling in S&P mode')
           ELSE
               Write ('No DE sent when calling in S&P mode');

      DIG: IF DigitalModeEnable THEN
               Write ('CW, DIG and SSB modes enabled')
           ELSE
               Write ('CW and SSB modes enabled');

      DIS: CASE DistanceMode OF
               NoDistanceDisplay: Write ('No display of distance');
               DistanceMiles:     Write ('Distance shown in miles');
               DistanceKM:        Write ('Distance shown in KM');
               END;

      DMF: Write ('Name of domestic mult file');

      DCS: CASE DupeCheckSound OF
               DupecheckNoSound:     Write ('SILENT DUPE CHECKING');
               DupeCheckBeepIfDupe:  Write ('BEEP IF DUPE WHEN SPACE BAR');
               DupeCheckGratsIfMult: Write ('BEEP IF DUPE - FANFARE IF MULT');
               END;

      DSE: IF Sheet.DupeSheetEnable THEN
               Write ('Calls will be added to dupesheet')
           ELSE
               Write ('Calls will not be added to dupesheet');

      DVK: IF ActiveDVKPort = nil THEN
               Write ('No DVK port selected')
           ELSE
               Write ('DVK enabled on the port shown');

      DVE: IF DVPEnable THEN
           begin
               Write ('DVP is enabled');
               if not dvpsetup then dvpinit;
           end
           ELSE
           begin
               Write ('DVP is not enabled');
           end;

      DVP: Write ('DVP PATH = ');

      EES: IF EscapeExitsSearchAndPounce THEN
               Write ('ESCAPE key will exit S&P mode')
           ELSE
               Write ('Use SHIFT-TAB to exist S&P mode');

      EME: IF ExchangeMemoryEnable THEN
               Write ('Exchange memory is enabled')
           ELSE
               Write ('Exchange memory is not enabled');

      FWE: IF ActiveKeyer.GetFarnsworthEnable THEN
               Write ('Expand character spaces < 25 WPM')
           ELSE
               Write ('No expanding of spaces < 25 WPM');

      FWS: Write ('Speed where farnsworth cuts in below');

      FSF: IF FloppyFileSaveFrequency = 0 THEN
               Write ('Floppy backups of LOG.DAT are disabled')
           ELSE
               Write ('Number QSOs between saves to floppy');

      FSE: Write ('File to save to when doing floppy save');

      FSM: CASE FootSwitchMode OF
               FootSwitchF1:                        Write ('Foot switch sends F1 message');
               FootSwitchDisabled:                  Write ('Foot switch disabled');
               FootSwitchLastCQFreq:                Write ('Go to last CQ frequency');
               FootSwitchNextBandMap:               Write ('Next non dupe band/mode entry in bandmap');
               FootSwitchNextDisplayedBandMap:      Write ('Next non dupe displayed band map entry'); {KK1L: 6.64}
               FootSwitchNextMultBandMap:           Write ('Next mult band/mode entry in bandmap'); {KK1L: 6.68}
               FootSwitchNextMultDisplayedBandMap:  Write ('Next multiplier displayed band map'); {KK1L: 6.68}
               FootSwitchDupeCheck:                 Write ('Do Alt-D dupe check command');
               Normal:                              Write ('Foot switch keys active radio PTT');
               QSONormal:                           Write ('Acts like pressing ENTER key');
               QSOQuick:                            Write ('Like ENTER key except for Quick QSL msg');
               FootSwitchControlEnter:              Write ('Execute Control-Enter function (no cw)');
               StartSending:                        Write ('Start sending call in call window on CW');
               SwapRadio:                           Write ('Swaps radios (like Alt-R command)');
               CWGrant:                             Write ('CW Grant mode - no CW until pressed');
               END;

      FA1: IF Radio1FrequencyAdder <> 0 THEN
               Write ('Amount to add to radio 1 frequency')
           ELSE
               Write ('No adder to radio 1 frequency');

      FA2: IF Radio2FrequencyAdder <> 0 THEN
               Write ('Amount to add to radio 1 frequency')
           ELSE
               Write ('No adder to radio 2 frequency');

      FPR: Write ('Rate in ms the radio is polled for freq'); {KK1L: 6.71a}

      FME: IF FrequencyMemoryEnable THEN
               Write ('Remember freqs for each band/mode')
           ELSE
               Write ('Do not remember freqs from band/mode');

      FCR: IF FT1000MPCWReverse THEN
               Write ('FT1000MP / FT920 use CW Reverse mode')
           ELSE
               Write ('FT1000MP / FT920 use normal CW mode');

      GMC: IF GridMapCenter = '' THEN
               Write ('No grid map defined')
           ELSE
               Write ('Grid map center location');

      HFE: IF HFBandEnable THEN
               Write ('HF Bands enabled.')
           ELSE
               Write ('HF Bands no enabled.');

      HDP: CASE HourDisplay OF
               ThisHour:      Write ('Show # of QSOs in this hour');
               LastSixtyMins: Write ('Show # of QSOs in last 60 minutes');
               BandChanges:   Write ('Show # band changes in this hour');
               END;

      HOF: Write ('Offset from computer time to UTC time');

      ICP: Write ('Command delay in ms (default = 300)');

      IRT: Write ('Response timeout in ms (default = 100)');

      ITE: IF IncrementTimeEnable THEN
               Write ('Alt1 to Alt0 keys enabled to bump time')
           ELSE
               Write ('Alt1 to Alt0 keys disabled to bump time');

      IFE: IF IntercomFileEnable THEN
               Write ('Inter-station messages to INTERCOM.TXT')
           ELSE
               Write ('INTERCOM.TXT file disabled');

      IEX: CASE ActiveInitialExchange OF
               NoInitialExchange:           Write ('Only exchange memory used');
               NameInitialExchange:         Write ('Name from TRMASTER database');
               NameQTHInitialExchange:      Write ('Name and QTH from TRMASTER database');
               CheckSectionInitialExchange: Write ('Check section from TRMASTER database');
               SectionInitialExchange:      Write ('ARRL Section from TRMASTER database');
               QTHInitialExchange:          Write ('QTH from TRMASTER database');
               FOCInitialExchange:          Write ('FOC number from TRMASTER database');
               GridInitialExchange:         Write ('Grid from TRMASTER database');
               ZoneInitialExchange:         Write ('Compute zone from callsign');
               User1InitialExchange:        Write ('Use TRMASTER user 1 field initial ex');
               User2InitialExchange:        Write ('Use TRMASTER user 2 field initial ex');
               User3InitialExchange:        Write ('Use TRMASTER user 3 field initial ex');
               User4InitialExchange:        Write ('Use TRMASTER user 4 field initial ex');
               User5InitialExchange:        Write ('Use TRMASTER user 5 field initial ex');
               CustomInitialExchange:       Write ('Uses CUSTOM INITIAL EXCHANGE STRING');
               END;

      {KK1L: 6.70} {KK1L: 6.73 Changed wording to cover expansion of feature to ALL initial exhanges}
      IXO: IF InitialExchangeOverwrite THEN
               Write ('Keystrokes overwrite initial exchange')
           ELSE
               Write ('Keystrokes add to initial exchange');

      IEC: CASE InitialExchangeCursorPos OF
               AtStart: Write ('Cursor at start of initial exchange');
               AtEnd:   Write ('Cursor at end of initial exhange');
               END;

      JRT: Write ('Response timeout in ms (default = 100)');

      KNE: IF K1EANetworkEnable THEN
               Write ('Use K1EA network protocol')
           ELSE
               Write ('Use N6TR network protocol');

      KSI: Write ('Station ID used on K1EA network');

      KRT: Write ('Response timeout in ms (default = 25)');

      KCM: IF KeyPadCWMemories THEN
               Write ('Numeric keypad sends CQ Ctrl-F1 to F10')
           ELSE
               Write ('Normal function for keypad (no cw)');

      LDZ: IF LeadingZeros > 0 THEN
               Write ('Number leading zeros in serial number')
           ELSE
               Write ('Leading zeros not sent with serial #s');

      LZC: Write ('Used for serial number leading zeros');

      LCI: IF LeaveCursorInCallWindow THEN
               Write ('Cursor stays in call window during QSO')
           ELSE
               Write ('Cursor moves to exchange window for ex');

      LFE: IF LogFrequencyEnable THEN
               Write ('Write freq in log instead of QSO number')
           ELSE
               Write ('Write QSO number in log - not freq');

      LRS: Write ('Default SSB report shown in logsheet');

      LDQ: IF LiteralDomesticQTH THEN
               Write ('Domestic QTHs shown as entered')
           ELSE
               Write ('Domestic QTHs shown as in .DOM file');

      LRT: Write ('Default CW report shown in logsheet');

      LSE: IF LogWithSingleEnter THEN
               Write ('Log QSOs with first ENTER')
           ELSE
               Write ('Log QSOs with second ENTER');

      LFR: IF LookForRSTSent THEN
               Write ('Look for S579 or S57 in exchange')
           ELSE
               Write ('Do not look for sent RS(T) in exchange');

      MSE: IF MessageEnable THEN
               Write ('Alt-P O messages enabled')
           ELSE
               Write ('Automatic Alt-P O messages disabled');

      MEN: IF MouseEnable THEN
               Write ('Mouse activity enabled')
           ELSE
               Write ('Mouse disabled');

      MRM: Write ('Threshold # bands for Control-O report');

      MIM: Write ('Multi status msg - $=Freq/S&P %=Rate ');

      MMO: IF MultiMultsOnly THEN
               Write ('Only mult QSOs are passed to other stns')
           ELSE
               Write ('All QSOs are passed to other stns');

      MRT: Write ('Multi network retry time in seconds');

      MUM: IF MultiUpdateMultDisplay THEN
               Write ('Rem mult display updated from net QSOs')
           ELSE
               Write ('Mults updated when QSO made or band chg');

      MBA: IF MultipleBandsEnabled THEN
               Write ('You can change bands after 1st QSO')
           ELSE
               Write ('You can''t change bands after 1st QSO');

      MMD: IF MultipleModesEnabled THEN
               Write ('You can change modes after 1st QSO')
           ELSE
               Write ('You can''t change modes after 1st QSO');

      MCL: Write ('Call as set by MY CALL in cfg file');
      MCN: Write ('Continent set by MY CALL in cfg file');
      MCU: Write ('Country as set by MY CALL');
      MFD: Write ('Class for ARRL Field Day');
      MGR: Write ('Reference grid used for beam headings');
      MIO: Write ('IOTA Reference Designator');
      MZN: Write ('Set by MY CALL / MY ZONE in cfg file');

      NFE: IF NameFlagEnable THEN
               Write ('Asterisk shows calls with known name')
           ELSE
               Write ('No asterisk to flag known names');

      NLQ: IF NoLog THEN
               Write ('No QSOs can be logged on this computer')
           ELSE
               Write ('QSOs may be logged on this computer');

      NPP: IF NoPollDuringPTT THEN
               Write ('Interfaced radio not polled when xmit')
           ELSE
               Write ('Interfaced radio polled during xmit');

      PAL: IF PacketAddLF THEN
               Write ('Line feed added after return for packet')
           ELSE
               Write ('No line feeds added to packet returns');

      PAR: IF PacketAutoCR THEN
               Write ('Return sent when exiting Control-B')
           ELSE
               Write ('No return sent when exiting Control-B');

      PBS: IF Packet.PacketBandSpots THEN
               Write ('Packet spots shown only for active band')
           ELSE
               Write ('All spots shown regardless of band');

      PBP: IF Packet.PacketBeep THEN
               Write ('Beep when packet spots come in')
           ELSE
               Write ('Display incoming spots without beep');

      PLF: IF Packet.PacketLogFileName = '' THEN
               Write ('Packet log file disabled')
           ELSE
               Write ('Packet log file enabled to file shown');

      PRM: IF PacketReturnPerMinute = 0 THEN
               Write ('Normal packet operation')
           ELSE
               Write ('RETURN sent every ', PacketReturnPerMinute, ' minutes');

      PSC: Write ('Comment sent with each packet spot'); {KK1L: 6.71 Implimented what I started in 6.68}

      PKD: IF PacketSpotDisable THEN
               Write ('Making spots with ` key is disabled')
           ELSE
               Write ('Making spots with ` key is enabled.');

      PSE: IF PacketSpotEditEnable THEN
               Write ('Outgoing spots shown for edit')
           ELSE
               Write ('Outgoing spots not shown for edit');

      SPO: IF PacketSpotPrefixOnly THEN {KK1L: 6.72}
               Write ('Outgoing spot prefix only')
           ELSE
               Write ('Outgoing spot is full call');

      PSP: IF Packet.PacketSpots = AllSpots THEN
               Write ('All spots from packet are shown')
           ELSE
               Write ('Only multiplier spots are shown');

      PBE: IF ActiveKeyer.GetPaddleBug THEN
               Write ('Dah contact of paddle = bug')
           ELSE
               Write ('Normal keyer dahs');

      PHC: Write ('Number dit times before PTT drops out');

      PMT: Write ('Monitor tone for CW sent with paddle');

      PSD: IF ActiveKeyer.GetPaddleSpeed = 0 THEN
               Write ('Paddle speed same as computer speed')
           ELSE
               Write ('Speed to send paddle CW with');

      PCE: IF PartialCallEnable THEN
               Write ('Partial calls will be shown')
           ELSE
               Write ('Partial calls will not be shown');

      PCL: IF PartialCallLoadLogEnable THEN
               Write ('If new LOG.DAT, partial calls loaded')
           ELSE
               Write ('Partials not loaded from new LOG.DAT');

      PCM: IF PartialCallMultsEnable THEN
               Write ('Mult info shown for partial calls')
           ELSE
               Write ('Mult info not shown for partial calls');

      PR1: IF PollRadioOne THEN {KK1L: 6.72}
               Write ('Radio one polled for freq/mode info')
           ELSE
               Write ('No polling for radio one');

      PR2: IF PollRadioTwo THEN {KK1L: 6.72}
               Write ('Radio two polled for freq/mode info')
           ELSE
               Write ('No polling for radio two');

      PCA: IF PossibleCallEnable THEN
               Write ('Possible (unique-1) calls will be shown')
           ELSE
               Write ('Possible (unique-1) calls not shown');

      PCN: CASE CD.PossibleCallAction OF  {KK1L: 6.69 support log only check partial}
               AnyCall:            Write ('Show all possible calls');
               OnlyCallsWithNames: Write ('Only show possible calls with names');
               LogOnly:            Write ('Only show possible calls from log');
               END;

      PEN: IF PrinterEnabled THEN
               Write ('Each QSO off editable window is printed')
           ELSE
               Write ('Real time printing is disabled');

      PTT: IF ActiveKeyer.GetPTTEnable THEN
               Write ('PTT control signal is enabled')
           ELSE
               Write ('PTT control signal is disabled (QSK)');

      PTD: Write ('PTT delay before CW sent (* 1.7 ms)');

      QMD: CASE ParameterOkayMode OF
               Standard:       Write ('Needs correct info to QSL & log');
               QSLButdoNotLog: Write ('Needs correct info to log, not to QSL');
               QSLAndLog:      Write ('No syntax checking of exchange');
               END;

      QNB: IF QSONumberByBand THEN
               Write ('Separate QSO numbers sent by band')
           ELSE
               Write ('Total QSOs used for QSO number');

      QES: IF QTCExtraSpace THEN
               Write ('Add extra spaces when sending QTCs')
           ELSE
               Write ('No extra spaces when sending QTCs');

      QRS: IF QTCQRS THEN
               Write ('QRS when sending QTCs')
           ELSE
               Write ('No QRS when sending QTCs');

      QSX: IF QSXEnable THEN
               Write ('QSX info from packet spots enabled')
           ELSE
               Write ('QSX info from packet spots disabled');

      QMC: Write ('Keyboard character used for ?');

      R1I: Write ('Char appended to QSO number for rig 1');

      R1T: IF Radio1TrackingEnable THEN
               Write ('Radio 1 band/mode tracking enabled')
           ELSE
               Write ('Radio 1 band/mode tracking disabled');

      R1U: IF Radio1UpdateSeconds = 0 THEN
               Write ('Normal operation')
           ELSE
               Write ('# seconds between frequency updates');

      R2I: Write ('Char appended to QSO number for rig 2');

      R2T: IF Radio2TrackingEnable THEN
               Write ('Radio 2 band/mode tracking enabled')
           ELSE
               Write ('Radio 2 band/mode tracking disabled');

      R2U: IF Radio2UpdateSeconds = 0 THEN
               Write ('Normal operation')
           ELSE
               Write ('# seconds between frequency updates');

      RCQ: IF RandomCQMode THEN
               Write ('Auto CQ picks F1-F4 at random')
           ELSE
               Write ('Auto CQ works normally');

      RDS: CASE RateDisplay OF
               QSOs:   Write ('Rate displays show QSOs');
               Points: Write ('Rate displays show QSO points');
               END;

      RMD: CASE RemainingMultDisplayMode OF
               NoRemainingMults: Write ('No remaining mult display');
               Erase:            Write ('Remaining mult erased when worked');
               HiLight:          Write ('Unworked remaining mults highlighted');
               END;

      SO2RHM: case so2rbox.getheadphonemode of
                 HNORMAL: write('Normal Stereo');
                 HSPATIAL: write('Rig 1 always left, Rig 2 always right');
                 HSYMMETRIC: write('Listen to one rig at a time - No stereo!');
              end;

      SO2RBE: If so2rbox.getblend then
                 write('Right/Left headphone blending on')
              else
                 write('Right/Left headphone blending off');

      SO2RBV: write('Right/Left Blend amount: min 0, max 255');

      SO2RMR: if so2rbox.getmicrelay then
                 write('Mic relay on')
              else
                 write('Mic relay off');

      SO2RM1: case so2rbox.getrig1map of
              0: write('Rig 1 default connector');
              1: write ('Rig 1 connector 1');
              2: write ('Rig 1 connector 2');
              3: write ('Rig 1 connector 3');
              4: write ('Rig 1 connector 4');
              -1: write ('Rig 1 connector 1 stored in EEPROM');
              -2: write ('Rig 1 connector 2 stored in EEPROM');
              -3: write ('Rig 1 connector 3 stored in EEPROM');
              -4: write ('Rig 1 connector 4 stored in EEPROM');
              end;

      SO2RM2: case so2rbox.getrig2map of
              0: write('Rig 2 default connector');
              1: write ('Rig 2 connector 1');
              2: write ('Rig 2 connector 2');
              3: write ('Rig 2 connector 3');
              4: write ('Rig 2 connector 4');
              -1: write ('Rig 2 connector 1 stored in EEPROM');
              -2: write ('Rig 2 connector 2 stored in EEPROM');
              -3: write ('Rig 2 connector 3 stored in EEPROM');
              -4: write ('Rig 2 connector 4 stored in EEPROM');
              end;


      SHE: IF SayHiEnable THEN
               Write ('Name database available to send names')
           ELSE
               Write ('Name sending is disabled');

      SHC: Write ('Rate above which name calling will stop');

      SCS: IF CD.CountryString = '' THEN
               Write ('All SCP calls displayed')
           ELSE
               Write ('Countries that SCP calls displayed');

      SML: IF SCPMinimumLetters = 0 THEN
               Write ('Auto Super Check Partial disabled')
           ELSE
               Write ('Minimum characters for Auto SCP');

      SAD: IF SendAltDSpotsToPacket THEN
               Write ('Alt-D entries sent to packet')
           ELSE
               Write ('Alt-D entries not sent to packet');

      SCF: IF SendCompleteFourLetterCall THEN
               Write ('Send all of 4 letter corrected callsign')
           ELSE
               Write ('Send prefix/suffix of 4 letter calls');


      SPS: IF StereoPinState THEN  {KK1L: 6.71}
               Write ('Stereo Control Pin high')
           ELSE
               Write ('Stereo Control Pin low');

      SQI: IF SendQSOImmediately THEN
               Write ('QSO sent to Multi port when logged')
           ELSE
               Write ('QSO sent when scrolled off edit window');

      SKE: IF ShiftKeyEnable = Shift THEN
               Write ('Shift keys enabled for RIT and S&P QSY')
           ELSE IF ShiftKeyEnable = AltShift THEN
               Write ('Alt-Shift keys for RIT and S&P QSY')
           ELSE
               Write ('Shift keys disabled for RIT and S&P QSY');

      SIN: IF ShortIntegers THEN
               Write ('Short integers used in QSO numbers')
           ELSE
               Write ('No short integers used in QSO numbers');

      SSP: IF ShowSearchAndPounce THEN
               Write ('S&P QSOs marked with "s" in log')
           ELSE
               Write ('S&P QSOs not marked in log');

      SEN: IF DDXState = Off THEN
               Write ('Simulator operation disabled')
           ELSE
               Write ('Simulator operation enabled');

      SRM: IF SingleRadioMode THEN
               Write ('Swap radio command (Alt-R) disabled')
           ELSE
               Write ('Swap radio command (Alt-R) enabled');

      SAB: IF SkipActiveBand THEN
               Write ('Alt-B skips active band of other rig')
           ELSE
               Write ('Alt-B doesn''t skip other active band');

      SMC: Write ('Keyboard character used for / character');

      SBD: IF SpaceBarDupeCheckEnable THEN
               Write ('Space does dupe check if call entered')
           ELSE
               Write ('Space always sends call & puts in S&P');

      SQR: IF SprintQSYRule THEN
               Write ('After S&P QSO, goes into CQ mode')
           ELSE
               Write ('Stay in S&P mode after S&P QSO');

      SRP: IF SwapPacketSpotRadios THEN
               Write ('Radio 1 is left of radio 2')
           ELSE
               Write ('Radio 1 is right of radio 2');

      SWP: IF ActiveKeyer.GetSwapPaddles THEN
               Write ('Swap dit and dah paddle connections')
           ELSE
               Write ('Normal dit and dah paddle connections');

      SWR: IF SwapRadioRelaySense THEN
               Write ('Radio One = 0 volts on relay output')
           ELSE
               Write ('Radio One = 5 volts on relay output');

      TAB: CASE TabMode OF
               NormalTabMode:   Write ('When edit, tab moves to next field');
               ControlFTabMode: Write ('When edit, tab moves to next word');
               END;

      TMR: CASE TenMinuteRule OF
               NoTenMinuteRule: Write ('No ten minute display');
               TimeOfFirstQSO:  Write ('Show time since first QSO on band/mode');
               END;

      TOT: Write ('Total off time taken so far');

      TDE: IF TuneDupeCheckEnable THEN {KK1L: 6.73}
               Write ('Tuning enables 2nd radio dupe check')
           ELSE
               Write ('Only ALT-D enables 2nd radio dupe check');

      TWD: IF ActiveKeyer.GetTuneWithDits THEN
               Write ('Left Control & Shift keys tune w/dits')
           ELSE
               Write ('Left Control & Shift keys key rig');

      TRM: IF TwoRadioState <> TwoRadiosDisabled THEN
               Write ('Special two radio mode is enabled')
           ELSE
               Write ('Special two radio mode is disabled');

      URF: IF UpdateRestartFileEnable THEN
               Write ('RESTART.BIN updated after each QSO')
           ELSE
               Write ('RESTART.BIN updated when exiting LOG');

      UIS: CASE UserInfoShown OF
               NoUserInfo: Write ('No user data shown from TRMASTER');

               NameInfo:         Write ('Name from TRMASTER');
               QTHInfo:          Write ('QTH from TRMASTER');
               CheckSectionInfo: Write ('Check and ARRL section from TRMASTER');
               SectionInfo:      Write ('ARRL section from TRMASTER');
               OldCallInfo:      Write ('Previous callsign from TRMASTER');
               FOCInfo:          Write ('FOC number from TRMASTER');
               GridInfo:         Write ('Grid square from TRMASTER');
               CQZoneInfo:       Write ('CQ zone from TRMASTER or CTY.DAT');
               ITUZoneInfo:      Write ('ITU zone from TRMASTER or CTY.DAT');
               User1Info:        Write ('Data from TRMASTER USER 1 shown');
               User2Info:        Write ('Data from TRMASTER USER 2 shown');
               User3Info:        Write ('Data from TRMASTER USER 3 shown');
               User4Info:        Write ('Data from TRMASTER USER 4 shown');
               User5Info:        Write ('Data from TRMASTER USER 5 shown');
               CustomInfo:       Write ('Use CUSTOM USER STRING');
               END;

      VER: Write ('Program version (can''t be changed)');

      VDE: IF VGADisplayEnable THEN
               Write ('VGA mode enabled at program start')
           ELSE
               Write ('VGA mode disabled at program start');

      VBE: IF VHFBandsEnabled THEN
               Write ('6 and 2 meters are enabled')
           ELSE
               Write ('VHF bands skipped with Alt-B or Alt-V');

      VDS: IF VisibleDupesheetEnable THEN
               Write ('Visible dupesheet is displayed')
           ELSE
               Write ('Visible dupesheet is not displayed');

      WFS: IF WaitForStrength THEN
               Write ('If [ in CW message - wait for input')
           ELSE
               Write ('Assume Strength = 9 if CW done with [');

      WUT: IF WakeUpTimeOut = 0 THEN
               Write ('Wake up time out is disabled')
           ELSE
               Write ('# minutes without a QSO causing alarm');

      WBE: IF WARCBandsEnabled THEN
               Write ('WARC bands are enabled')
           ELSE
               Write ('WARC bands skipped with Alt-B or Alt-V');

      WEI: Write ('Keying weight');

      WCP: IF WildCardPartials THEN
               Write ('Calls with partial anywhere are shown')
           ELSE
               Write ('Only calls starting with partial shown');

      YRT: Write ('Response timeout in ms (default = 100)');
      END;

    IF Active THEN
        BEGIN
        TextColor (ActiveColor);
        TextBackground (ActiveBackground);
        END;
    END;


    BEGIN
    END.

