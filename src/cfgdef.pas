{ Here are the default values that are used if nothing else in the
  configuration file addresses these variables.                     }

PROCEDURE SetConfigurationDefaultValues;

    BEGIN
    ActiveBand             := Band160;
    ActiveDomesticMult     := NoDomesticMults;
    ActiveDVKPort          := nil;
    ActiveDXMult           := NoDXMults;
    ActiveExchange         := NoExchangeReceived;
    ActiveInitialExchange  := NoInitialExchange;
    ActiveModemPort        := nil;
    ActiveMode             := CW;
    ActivePrefixMult       := NoPrefixMults;
    ActiveQSOPointMethod   := NoQSOPointMethod;
    ActiveRadio            := RadioOne;
    ActiveRotatorType      := NoRotator;
    ActiveZoneMult         := NoZoneMults;
    AllCWMessagesChainable := False;
    AltDBufferEnable       := False;
    AlwaysCallBlindCQ      := False;
    AskForFrequencies      := True;
    AskIfContestOver       := True;
    AutoCallTerminate      := False;
    AutoDupeEnableCQ       := True;
    AutoDupeEnableSAndP    := True;
    AutoQSONumberDecrement := False;
    AutoReturnToCQMode     := True;
    AutoSAPEnable          := False;
    AutoSAPEnableRate      := 1000; {KK1L 6.72}
    AutoSendCharacterCount := 0;
    AutoTimeIncrementQSOs  := 0;

    BandMapAllBands         := False;
    BandMapAllModes         := False;
    BandMapMultsOnly        := False; {KK1L: 6.68}
    BandMapCallWindowEnable := True;
    BandMapDisplayCQ        := True;

    BandMapModeCutoffFrequency [Band160] :=   1840000;
    BandMapModeCutoffFrequency [Band80]  :=   3700000;
    BandMapModeCutoffFrequency [Band40]  :=   7100000;
    BandMapModeCutoffFrequency [Band30]  :=  10150000;
    BandMapModeCutoffFrequency [Band20]  :=  14100000;
    BandMapModeCutoffFrequency [Band17]  :=  18110000;
    BandMapModeCutoffFrequency [Band15]  :=  21200000;
    BandMapModeCutoffFrequency [Band12]  :=  24930000;
    BandMapModeCutoffFrequency [Band10]  :=  28300000;
    BandMapModeCutoffFrequency [Band6]   :=  50100000;
    BandMapModeCutoffFrequency [Band2]   := 144200000;


    BandMapDecayTime        := 60;
    BandMapDecayValue       := 60; {KK1L: 6.65}
    BandMapDecayMultiplier  := 1; {KK1L: 6.65}
    BandMapDupeDisplay      := True;
    BandMapGuardBand        := 200;
    Tone.SetBeepEnable(True);
    BeepSoundCardEnable     := False;
    BeepEvery10QSOs         := False;
    BigRemainingList        := False;
    BandMapSplitMode        := ByCutoffFrequency; {KK1L: 6.64}

    Packet.BroadcastAllPacketData  := True;

    CallsignUpdateEnable    := False;
    CallWindowPosition      := NormalCallWindowPosition;
    CallWindowShowAllSpots  := False;
    CFGDVPPath              := '';
    CheckLogFileSize        := False;
    CodeSpeed               := 35;
    ColumnDupeSheetEnable   := False;
    ComputerID              := Chr (0);
    ContactsPerPage         := 50;
    ConfirmEditChanges      := True;
    ContestTitle            := '';
    ContinentString         := '';
    CorrectedCallMessage    := '} OK %';

    CorrectedCallPhoneMessage := 'CORCALL.DVP';
    CountDomesticCountries    := False;
    CountryInformationFile    := '';
    CountryString   := '';
    CQExchange          := '';
    CQExchangeNameKnown := '';
    CQMenu := 'F1/2-CQ F3-Ex F4-73 F5-Call F6-DECall F7-WkdB4 F8-Agn F9-? F10-Keyboard CW';
    CQP := False;
    CQPhoneExchange := 'CQECXHNG.DVP';
    CQPhoneExchangeNameKnown := 'CQEXNAME.DVP';
    CustomInitialExchangeString := '';
    CustomUserString            := '';
    CWEnable  := True;
    CWEnabled := True;
    CWSpeedFromDataBase := False;
    CWTone := 700;

    DDXCall1                := '';
    DDXCall2                := '';
    DDXState                := Off;
    DEEnable                := True;
    DigitalModeEnable       := False;
    DistanceMode            := NoDistanceDisplay;
    DoingColors             := False;
    DomesticQTHDataFileName := '';
    DupeCheckSound          := DupeCheckBeepIfDupe;
    DVPEnable               := False;

    Packet.EightBitPacketPort := False;

    EightBitRTTYPort := False; {KK1L: 6.71}

    EnableSixDVKMessages       := True; {KK1L: 6.72}
    EscapeExitsSearchAndPounce := True;
    ExchangeFunctionKeyMenu := '';

    ExchangeInformation.Age           := False;
    ExchangeInformation.Check         := False;
    ExchangeInformation.Class         := False;
    ExchangeInformation.Name          := False;
    ExchangeInformation.Precedence    := False;
    ExchangeInformation.Power         := False;
    ExchangeInformation.RandomChars   := False;
    ExchangeInformation.RST           := False;
    ExchangeInformation.QSONumber     := False;
    ExchangeInformation.QTH           := False;
    ExchangeInformation.Zone          := False;
    ExchangeInformation.ZoneOrSociety := False;

    ExchangeMemoryEnable := True;

    CPUKeyer.SetFarnsworthEnable(False);
    Winkey.SetFarnsworthEnable(False);
    YcccKey.SetFarnsworthEnable(False);
    CPUKeyer.SetFarnsworthSpeed(25);
    Winkey.SetFarnsworthSpeed(25);
    Yccckey.SetFarnsworthSpeed(25);

    FloppyFileSaveName := 'LOGBACK.DAT';
    FloppyFileSaveFrequency := 0;

    FootSwitchMode := FootSwitchDisabled;

    ForcedEntry := False; {KK1L: 6.70 switch used in JCTRL2 to add comments to LOGCFG}

    FreqMemory [Band160, CW] :=   1820000;
    FreqMemory [Band80, CW]  :=   3525000;
    FreqMemory [Band40, CW]  :=   7025000;
    FreqMemory [Band30, CW]  :=  10105000;
    FreqMemory [Band20, CW]  :=  14025000;
    FreqMemory [Band17, CW]  :=  18068000;
    FreqMemory [Band15, CW]  :=  21025000;
    FreqMemory [Band12, CW]  :=  24890000;
    FreqMemory [Band10, CW]  :=  28050000;
    FreqMemory [Band6,  CW]  :=  50125000;
    FreqMemory [Band2,  CW]  := 144100000;

    FreqMemory [Band160, Phone] :=   1840000;
    FreqMemory [Band80, Phone]  :=   3850000;
    FreqMemory [Band40, Phone]  :=   7225000;
    FreqMemory [Band30, Phone]  :=  10000000;
    FreqMemory [Band20, Phone]  :=  14225000;
    FreqMemory [Band17, Phone]  :=  18110000;
    FreqMemory [Band15, Phone]  :=  21300000;
    FreqMemory [Band12, Phone]  :=  24930000;
    FreqMemory [Band10, Phone]  :=  28400000;
    FreqMemory [Band6,  Phone]  :=  50125000;
    FreqMemory [Band2,  Phone]  := 144200000;

    FrequencyMemoryEnable := True;

    FT1000MPCWReverse := False;

    GridMapCenter := '';

    HFBandEnable := True;
    HourDisplay  := ThisHour;

    IncrementTimeEnable      := False;
    InitialExchangeCursorPos := AtEnd;
    InitialExchangeOverwrite := False; {KK1L: 6.70}
    InsertMode               := True;
    IntercomFileEnable       := False;

    K1EANetworkEnable       := False;
    KeyPadCWMemories        := False;

    LeaveCursorInCallWindow := False;
    LeadingZeros            := 0;
    LeadingZeroCharacter    := 'T';
    LiteralDomesticQTH      := False;

    LogFrequencyEnable      := False;
    LogRSTSent              := '599';
    LogRSSent               := '59';
    LogSubTitle             := '';
    LogWithSingleEnter      := False;
    LookForRSTSent          := False;

    MessageEnable          := True;
    ModemPortBaudRate      := 4800;
    MultiInfoMessage       := '';
    MultiMultsOnly         := False;
    MultipleBandsEnabled   := True;
    MultipleModesEnabled   := True;
    MultiPortBaudRate      := 4800;
    MultiRetryTime         := 30;
    MultiUpdateMultDisplay := True;
    MultReportMinimumBands := 4;



    MyCall       := '';
    MyCheck      := '';
    MyContinent  := UnknownContinent;
    MyCountry    := '';
    MyFDClass    := '';
    MyGrid       := '';
    MyIOTA       := '';
    MyPostalCode := '';
    MyPrec       := '';
    MySection    := '';
    MyState      := '';
    MyZone       := '';

    NameFlagEnable  := True;
    NoLog           := False;
    NoPollDuringPTT := False;

    PacketAddLF           := False;
    PacketAutoCR          := False;
    PacketMessMode        := False;
    PacketReturnPerMinute := 0;
    PacketSpotComment     := ''; {KK1L: 6.68} {KK1L: 6.71 moved here from LOGWIND.PAS}
    PacketSpotDisable     := False;
    PacketSpotEditEnable  := False;
    PacketSpotKey         := '`';
    PacketSpotPrefixOnly  := False; {KK1L: 6.72}

    Packet.PacketBandSpots   := False;
    Packet.PacketBaudRate    := 2400;
    Packet.PacketBeep        := True;
    Packet.PacketLogFileName := '';
    Packet.PacketSpots       := MultSpots;

    CPUKeyer.SetPaddleMonitorTone(700);
    Winkey.SetPaddleMonitorTone(700);
    Yccckey.SetPaddleMonitorTone(700);
    CPUKeyer.SetPaddleBug(False);
    Winkey.SetPaddleBug(False);
    CPUKeyer.SetPaddleSpeed(0);
    Winkey.SetPaddleSpeed(0);
    Yccckey.SetPaddleSpeed(0);
    ParameterOkayMode        := Standard;
    PartialCallEnable        := True;
    PartialCallLoadLogEnable := False;
    PartialCallMultsEnable   := True;
    PossibleCallAcceptKey    := ';';
    PossibleCallEnable       := True;
    PossibleCallLeftKey      := ',';
    PossibleCallRightKey     := '.';

    PrefixInfoFileName       := '';
    PrinterEnabled           := False;
    CPUKeyer.SetPTTEnable(True);
    Winkey.SetPTTEnable(True);
    YcccKey.SetPTTEnable(True);

    QSLMessage             := '73 \ TEST';
    QSLPhoneMessage        := 'QSL.DVP';
    QSOBeforeMessage       := ' SRI QSO B4 73 \ TEST';
    QSOBeforePhoneMessage  := 'QSOB4.DVP';
    QTCMinutes             := False;
    QTCsEnabled            := False;
    QSONumberByBand        := False;
    QSOPointsDomesticCW    := -1;
    QSOPointsDomesticPhone := -1;
    QSOPointsDXCW          := -1;
    QSOPointsDXPhone       := -1;
    QSXEnable              := True;
    QTCExtraSpace          := True;
    QTCQRS                 := True;
    QuickQSLKey1           := '\';
    QuickQSLKey2           := '=';
    QuickQSLMessage1       := 'TU';
    QuickQSLMessage2       := 'EE';
    QuickQSLPhoneMessage   := 'QUICKQSL.DVP';

    Radio1BaudRate := 4800;
    Radio2BaudRate := 4800;
    Radio1IDCharacter := Chr (0);
    Radio2IDCharacter := Chr (0);
    Radio1Type := NoInterfacedRadio;
    Radio2Type := NoInterfacedRadio;
    Radio1ControlPort := nil;
    Radio2ControlPort := nil;
    Radio1ReceiverAddress := 4;
    Radio2ReceiverAddress := 4;
    Radio1TrackingEnable := True;
    Radio2TrackingEnable := True;

    RadioOneName := ' Rig 1';
    RadioTwoName := ' Rig 2';

    Radio1UpdateSeconds := 0;
    Radio2UpdateSeconds := 0;

    RadiusOfEarth := 0.0;
    RandomCQMode := False;
    RandomNameEnable := False;
    RelayControlPort := nil;
    RemainingMultDisplayMode := HiLight;
    RememberDDXCallsign := '';
    RepeatSearchAndPounceExchange := '';
    RepeatSearchAndPouncePhoneExchange := 'RPTSPEX.DVP';

    RTTYReceiveString := ControlR;
    RTTYSendString    := ControlT;

    SayHiEnable       := False;
    SayHiRateCutoff   := 200;

    SCPDupeBackground := Black;
    SCPDupeColor      := Red;
    SCPMinimumLetters := 0;

    SearchAndPounceExchange      := '';
    SearchAndPouncePhoneExchange := 'SAPEXCHG.DVP';
    SendAltDSpotsToPacket        := False;
    SendCompleteFourLetterCall   := False;
    SendQSOImmediately           := True;

    SetCQMemoryString (CW, F1, 'CQTEST \ \ TEST');
    SetCQMemoryString (CW, F2, 'CQTEST CQTEST CQTEST \ \ TEST');
    SetCQMemoryString (CW, F4, '73 \ TEST');
    SetCQMemoryString (CW, F6, 'DE \');
    SetCQMemoryString (CW, F7, 'SRI QSO B4 73 \ TEST');
    SetCQMemoryString (CW, F8, 'AGN');
    SetCQMemoryString (CW, F9, '?');
    SetCQMemoryString (CW, F10, ':');
    SetExMemoryString (CW, F8, 'EE');
    SetExMemoryString (CW, F9, '?');
    SetExMemoryString (CW, F10, ':');
    SetExMemoryString (CW, AltF1, 'UR CALL?');
    SetExMemoryString (CW, AltF2, 'AGN?');
    SetExMemoryString (CW, AltF10, 'IS UR CALL @?');

    SetCQMemoryString (Phone, F1,  'CQF1.DVP');
    SetCQMemoryString (Phone, F2,  'CQF2.DVP');
    SetCQMemoryString (Phone, F3,  'CQF3.DVP');
    SetCQMemoryString (Phone, F4,  'CQF4.DVP');
    SetCQMemoryString (Phone, F5,  'CQF5.DVP');
    SetCQMemoryString (Phone, F6,  'CQF6.DVP');
    SetCQMemoryString (Phone, F7,  'CQF7.DVP');
    SetCQMemoryString (Phone, F8,  'CQF8.DVP');
    SetCQMemoryString (Phone, F9,  'CQF9.DVP');
    SetCQMemoryString (Phone, F10, 'CQF10.DVP');

    SetEXMemoryString (Phone, F1,  'MYCALL.DVP');
    SetEXMemoryString (Phone, F2,  'EXF2.DVP');
    SetEXMemoryString (Phone, F3,  'EXF3.DVP');
    SetEXMemoryString (Phone, F4,  'EXF4.DVP');
    SetEXMemoryString (Phone, F5,  'EXF5.DVP');
    SetEXMemoryString (Phone, F6,  'EXF6.DVP');
    SetEXMemoryString (Phone, F7,  'EXF7.DVP');
    SetEXMemoryString (Phone, F8,  'EXF8.DVP');
    SetEXMemoryString (Phone, F9,  'EXF9.DVP');
    SetEXMemoryString (Phone, F10, 'EXF10.DVP');

    SetEXMemoryString (Phone, AltF1,  'ALTEXF1.DVP');
    SetEXMemoryString (Phone, AltF2,  'ALTEXF2.DVP');
    SetEXMemoryString (Phone, AltF3,  'ALTEXF3.DVP');
    SetEXMemoryString (Phone, AltF4,  'ALTEXF4.DVP');
    SetEXMemoryString (Phone, AltF5,  'ALTEXF5.DVP');
    SetEXMemoryString (Phone, AltF6,  'ALTEXF6.DVP');
    SetEXMemoryString (Phone, AltF7,  'ALTEXF7.DVP');
    SetEXMemoryString (Phone, AltF8,  'ALTEXF8.DVP');
    SetEXMemoryString (Phone, AltF9,  'ALTEXF9.DVP');
    SetEXMemoryString (Phone, AltF10, 'ALTEXF10.DVP');

    Sheet.DupeSheetEnable := True;

    ShiftKeyEnable := Shift;

    Short0 := 'T';
    Short1 := 'A';
    Short2 := '2';
    Short9 := 'N';
    ShortIntegers := False;
    ShowSearchAndPounce := False;
    SingleBand := All;
    SingleRadioMode := False;
    SkipActiveBand := False;
    SpaceBarDupeCheckEnable := True;
    SprintQSYRule := False;

    StartingFrequencies [Band160] :=   1800000;
    StartingFrequencies [Band80]  :=   3500000;
    StartingFrequencies [Band40]  :=   7000000;
    StartingFrequencies [Band30]  :=  10000000;
    StartingFrequencies [Band20]  :=  14000000;
    StartingFrequencies [Band17]  :=  18000000;
    StartingFrequencies [Band15]  :=  21000000;
    StartingFrequencies [Band12]  :=  24000000;
    StartingFrequencies [Band10]  :=  28000000;
    StartingFrequencies [Band6]   :=  50000000;
    StartingFrequencies [Band2]   := 144000000;

    StartSendingNowKey := '''';

    StoppingFrequencies [Band160] :=   2000000;
    StoppingFrequencies [Band80]  :=   4000000;
    StoppingFrequencies [Band40]  :=   7500000;
    StoppingFrequencies [Band30]  :=  11000000;
    StoppingFrequencies [Band20]  :=  14500000;
    StoppingFrequencies [Band17]  :=  19000000;
    StoppingFrequencies [Band15]  :=  21500000;
    StoppingFrequencies [Band12]  :=  25000000;
    StoppingFrequencies [Band10]  :=  30000000;
    StoppingFrequencies [Band6]   :=  54000000;
    StoppingFrequencies [Band2]   := 148000000;

    SwapPacketSpotRadios := False;
    CPUKeyer.SetSwapPaddles(False);
    Winkey.SetSwapPaddles(False);
    Yccckey.SetSwapPaddles(False);
    SwapRadioRelaySense := False;

    TabMode         := NormalTabMode;
    TailEndKey      := ']';
    TailEndMessage  := 'R';
    TailEndPhoneMessage := 'TAILEND.DVP';
    TenMinuteRule   := NoTenMinuteRule;
    CPUKeyer.SetTuneWithDits(False);
    Winkey.SetTuneWithDits(False);
    Yccckey.SetTuneWithDits(False);
    TuneDupeCheckEnable := False; {KK1L: 6.73}
    TwoRadioState   := TwoRadiosDisabled;

    UnknownCountryFileEnable := False;
    UnknownCountryFileName   := 'UNKNOWN.CTY';
    UpdateRestartFileEnable  := True;
    UserInfoShown            := NoUserInfo;

    VGADisplayEnable := True;
    VHFBandsEnabled := False;
    VisibleDupesheetEnable := False;

    WakeUpTimeout := 0;
    WaitForStrength := True;
    WARCBandsEnabled := False;
    WildCardPartials := True;



    WITH MonoColors DO
        BEGIN
        AlarmWindowBackground                 := Black;
        AlarmWindowColor                      := White;
        AltCallWindowBackground               := Black; {KK1L: 6.73}
        AltCallWindowColor                    := White; {KK1L: 6.73}
        BandMapWindowBackground               := Black;
        BandMapWindowColor                    := White;
        BandModeWindowBackground              := Black;
        BandModeWindowColor                   := White;
        BeamHeadingWindowBackground           := Black;
        BeamHeadingWindowColor                := White;
        BigWindowBackground                   := Black;
        BigWindowColor                        := White;
        CallWindowBackground                  := Black;
        CallWindowColor                       := White;
        ClockWindowBackground                 := Black;
        ClockWindowColor                      := White;
        CodeSpeedWindowBackground             := Black;
        CodeSpeedWindowColor                  := White;
        ContestTitleWindowBackground          := Black;
        ContestTitleWindowColor               := White;
        CountryNameWindowBackground           := Black;
        CountryNameWindowColor                := White;
        DateWindowBackground                  := Black;
        DateWindowColor                       := White;
        DupeInfoWindowBackground              := Black;
        DupeInfoWindowColor                   := White;
        DupeSheetWindowBackground             := Black;
        DupeSheetWindowColor                  := White;
        EditableLogWindowBackground           := Black;
        EditableLogWindowColor                := White;
        ExchangeSAndPWindowBackground         := Black;
        ExchangeWindowBackground              := Black;
        ExchangeWindowColor                   := White;
        FreeMemoryWindowBackground            := Black;
        FreeMemoryWindowColor                 := White;

        FrequencyOneWindowBackground          := Black;
        FrequencyTwoWindowBackground          := Black; {KK1L: 6.73}

        FrequencyOneWindowColor               := White;
        FrequencyTwoWindowColor               := White; {KK1L: 6.73}

        FrequencyOneWindowHighlight           := White; {KK1L: 6.73}
        FrequencyTwoWindowHighlight           := White; {KK1L: 6.73}

        FunctionKeyWindowBackground           := Black;
        FunctionKeyWindowColor                := White;
        InsertWindowBackground                := Black;
        InsertWindowColor                     := White;
        MultiplierInformationWindowBackground := Black;
        MultiplierInformationWindowColor      := White;
        NamePercentageWindowBackground        := Black;
        NamePercentageWindowColor             := White;
        NameSentWindowBackground              := Black;
        NameSentWindowColor                   := White;
        PossibleCallWindowBackground          := Black;
        PossibleCallWindowColor               := White;
        PossibleCallWindowDupeBackground      := Black;
        PossibleCallWindowDupeColor           := Yellow;
        QSOInformationWindowBackground        := Black;
        QSOInformationWindowColor             := White;
        QSONumberWindowBackground             := Black;
        QSONumberWindowColor                  := White;
        QTCNumberWindowBackground             := Black;
        QTCNumberWindowColor                  := White;
        QuickCommandWindowBackground          := Black;
        QuickCommandWindowColor               := White;
        RadioOneWindowBackground              := Black; {KK1L: 6.73 Changed from RadioWindowBackground}
        RadioOneWindowColor                   := White; {KK1L: 6.73 Changed from RadioWindowColor}
        RadioTwoWindowBackground              := Black; {KK1L: 6.73}
        RadioTwoWindowColor                   := White; {KK1L: 6.73}
        RateWindowBackground                  := Black;
        RateWindowColor                       := White;
        RTTYWindowBackground                  := Black;
        RTTYWindowColor                       := White;
        RemainingMultsWindowBackground        := Black;
        RemainingMultsWindowColor             := White;
        RemainingMultsWindowSubdue            := Black;
        TotalScoreWindowBackground            := Black;
        TotalScoreWindowColor                 := White;
        TotalWindowBackground                 := Black;
        TotalWindowColor                      := White;
        UserInfoWindowColor                   := White;
        UserInfoWindowBackground              := Black;
        WholeScreenColor                      := White;
        WholeScreenBackground                 := Black;
        END;

    WITH ColorColors DO
        BEGIN
        AlarmWindowBackground := Red;
        AlarmWindowColor      := Black;
        AltCallWindowBackground := Blue;  {KK1L: 6.73}
        AltCallWindowColor      := LightGray; {KK1L: 6.73}



        BandMapWindowBackground     := Cyan;
        BandMapWindowColor          := Black;
        BandModeWindowBackground    := Cyan;
        BandModeWindowColor         := Black;
        BeamHeadingWindowBackground := White;
        BeamHeadingWindowColor      := Black;
        BigWindowBackground         := Cyan;
        BigWindowColor              := Black;

        CallWindowBackground         := Magenta;
        CallWindowColor              := Yellow;

        ClockWindowBackground        := Cyan;
        ClockWindowColor             := Black;

        CodeSpeedWindowBackground    := Cyan;
        CodeSpeedWindowColor         := Black;

        ContestTitleWindowBackground := White;
        ContestTitleWindowColor      := Black;

        CountryNameWindowBackground := Cyan;
        CountryNameWindowColor      := Black;

        DateWindowBackground := Cyan;
        DateWindowColor      := Black;

        DupeInfoWindowBackground := Cyan;
        DupeInfoWindowColor      := Black;

        DupesheetWindowBackground := Cyan;
        DupesheetWindowColor      := Black;

        EditableLogWindowBackground := Cyan;
        EditableLogWindowColor      := Black;

        ExchangeSAndPWindowBackground := Green;
        ExchangeWindowBackground      := Magenta;
        ExchangeWindowColor           := Yellow;

        FreeMemoryWindowBackground := White;
        FreeMemoryWindowColor      := Black;

        FrequencyOneWindowBackground := Blue;
        FrequencyOneWindowColor      := LightGray;
        FrequencyOneWindowHighlight  := White;

        FrequencyTwoWindowBackground := Blue;
        FrequencyTwoWindowColor      := LightGray;
        FrequencyTwoWindowHighlight  := White;

        FunctionKeyWindowBackground      := White;
        FunctionKeyWindowColor           := Black;

        InsertWindowBackground := Cyan;
        InsertWindowColor      := Black;

        MultiplierInformationWindowBackground := Cyan;
        MultiplierInformationWindowColor      := Black;

        NamePercentageWindowBackground := Cyan;
        NamePercentageWindowColor      := Black;

        NameSentWindowBackground       := Cyan;
        NameSentWindowColor            := Black;

        PossibleCallWindowBackground := Cyan;
        PossibleCallWindowColor      := Black;

        PossibleCallWindowDupeBackground := Black;
        PossibleCallWindowDupeColor      := Red;

        QSOInformationWindowBackground := Cyan;
        QSOInformationWindowColor      := Black;

        QSONumberWindowBackground      := Cyan;
        QSONumberWindowColor           := Black;

        QTCNumberWindowBackground      := Cyan;
        QTCNumberWindowColor           := Black;

        QuickCommandWindowBackground   := White;
        QuickCommandWindowColor        := Blue;

        RadioOneWindowBackground       := Cyan;  {KK1L: 6.73 Changed from RadioWindowBackground}
        RadioOneWindowColor            := Black; {KK1L: 6.73 Changed from RadioWindowColor}
        RadioTwoWindowBackground       := Cyan;  {KK1L: 6.73}
        RadioTwoWindowColor            := Black; {KK1L: 6.73}
        RateWindowBackground           :=   Cyan;
        RateWindowColor                :=   Black;

        RemainingMultsWindowBackground := Blue;
        RemainingMultsWindowColor      := White;
        RemainingMultsWindowSubdue     := Cyan;

        RTTYWindowColor                := Cyan;
        RTTYWindowBackground           := Black;
        RTTYInverseWindowColor         := Black;
        RTTYInverseWindowBackground    := White;

        TotalScoreWindowBackground := White;
        TotalScoreWindowColor      := Black;

        TotalWindowBackground      := Blue;
        TotalWindowColor           := White;

        UserInfoWindowBackground   := Blue;
        UserInfoWindowColor        := White;

        WholeScreenColor := Yellow;
        WholeScreenBackground := Blue;
        END;

    SelectedColors := MonoColors;
    END;
