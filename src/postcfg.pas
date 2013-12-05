{ This is a file that gets included into LOGCFG.PAS and POSTSUBS.PAS }

FUNCTION ProcessPostConfigInstruction (ID: Str80; CMD: Str80): BOOLEAN;

VAR TempValue, xResult: INTEGER;
    TempQTH: QTHRecord;

    BEGIN
    ProcessPostConfigInstruction := False;

    { These are free }

    IF ID = 'CONTACTS PER PAGE' THEN
        BEGIN
        VAL (CMD, TempValue, xResult);
        IF xResult = 0 THEN
            BEGIN
            ContactsPerPage := TempValue;
            ProcessPostConfigInstruction := True;
            END;
        Exit;
        END;

    IF ID = 'CONTEST' THEN
        BEGIN
        CMD := UpperCase (CMD);
        ProcessPostConfigInstruction := FoundContest (CMD);
        Exit;
        END;

    IF ID = 'LOG FILE NAME' THEN
        BEGIN
        IF (Length (CMD) <= 8) AND NOT StringHas (CMD, '.') THEN
            BEGIN
            SetUpFileNames (LogFileName);
            ProcessPostConfigInstruction := True;
            END
        ELSE
            ReportError ('LOG FILE NAME command has illegal format.  Must be <= 8 chars and no dot.');
        Exit;
        END;

    IF ID = 'LOG SUB TITLE'  THEN
        BEGIN
        LogSubTitle := CMD;
        ProcessPostConfigInstruction := True;
        Exit;
        END;

    IF ID = 'MY CALL' THEN
        BEGIN
        MyCall := UpperCase (CMD);
        LocateCall (MyCall, TempQTH, True);
        MyCountry     := CountryTable.GetCountryID (TempQTH.Country);
        MyContinent   := TempQTH.Continent;
        Str (TempQTH.Zone,  MyZone);
        CountryString   := MyCountry;
        ContinentString := CountryTable.GetContinentName (MyContinent);
        ProcessPostConfigInstruction := True;
        Exit;
        END;

    IF ID = 'MY CHECK' THEN
        BEGIN
        MyCheck := UpperCase (CMD);
        ProcessPostConfigInstruction := True;
        Exit;
        END;

    IF ID = 'MY CONTINENT' THEN
        BEGIN
        ContinentString := CMD;
        IF UpperCase (CMD) = 'EU' THEN
            BEGIN
            MyContinent := Europe;
            ProcessPostConfigInstruction := True;
            END;

        IF UpperCase (CMD) = 'NA' THEN
            BEGIN
            MyContinent := NorthAmerica;
            ProcessPostConfigInstruction := True;
            END;

        IF UpperCase (CMD) = 'SA' THEN
            BEGIN
            MyContinent := SouthAmerica;
            ProcessPostConfigInstruction := True;
            END;

        IF UpperCase (CMD) = 'AF' THEN
            BEGIN
            MyContinent := Africa;
            ProcessPostConfigInstruction := True;
            END;

        IF UpperCase (CMD) = 'AS' THEN
            BEGIN
            MyContinent := Asia;
            ProcessPostConfigInstruction := True;
            END;

        IF UpperCase (CMD) = 'OC' THEN
            BEGIN
            MyContinent := Oceania;
            ProcessPostConfigInstruction := True;
            END;

        Exit;
        END;

    IF ID = 'MY COUNTRY' THEN
        BEGIN
        LocateCall (CMD, TempQTH, True);
        MyCountry     := CountryTable.GetCountryID (TempQTH.Country);
        MyContinent   := TempQTH.Continent;

        Str (TempQTH.Zone,  MyZone);

        CountryString   := MyCountry;
        ContinentString := CountryTable.GetContinentName (MyContinent);
        ProcessPostConfigInstruction := True;
        Exit;
        END;

    IF ID = 'MY ZONE' THEN
        BEGIN
        MyZone     := CMD;
        ProcessPostConfigInstruction := True;
        Exit;
        END;


    IF ID = 'MY FD CLASS' THEN
        BEGIN
        MyFDClass := UpperCase (CMD);
        ProcessPostConfigInstruction := True;
        Exit;
        END;

    IF ID = 'MY NAME' THEN
        BEGIN
        MyName := UpperCase (CMD);
        ProcessPostConfigInstruction := True;
        Exit;
        END;

    IF ID = 'MY POSTAL CODE' THEN
        BEGIN
        MyPostalCode := UpperCase (CMD);
        ProcessPostConfigInstruction := True;
        Exit;
        END;

    IF ID = 'MY PREC' THEN
        BEGIN
        MyPrec := UpperCase (CMD);
        ProcessPostConfigInstruction := True;
        Exit;
        END;

    IF ID = 'MY SECTION' THEN
        BEGIN
        MySection := UpperCase (CMD);
        ProcessPostConfigInstruction := True;
        Exit;
        END;

    IF (ID = 'MY STATE') OR (ID = 'MY QTH') THEN
        BEGIN
        MyState := UpperCase (CMD);
        ProcessPostConfigInstruction := True;
        Exit;
        END;

    IF ID = 'SINGLE BAND SCORE' THEN
        BEGIN
        SingleBand := NoBand;
        IF CMD = '160' THEN SingleBand := Band160;
        IF CMD = '80'  THEN SingleBand := Band80;
        IF CMD = '40'  THEN SingleBand := Band40;
        IF CMD = '20'  THEN SingleBand := Band20;
        IF CMD = '15'  THEN SingleBand := Band15;
        IF CMD = '10'  THEN SingleBand := Band10;
        ProcessPostConfigInstruction := SingleBand <> NoBand;
        Exit;
        END;

    IF ID = 'UNKNOWN COUNTRY FILE ENABLE' THEN
        BEGIN
        UnknownCountryFileEnable := UpCase (CMD [1]) = 'T';
        ProcessPostConfigInstruction := True;
        Exit;
        END;

    IF ID = 'UNKNOWN COUNTRY FILE NAME' THEN
        BEGIN
        UnknownCountryFileName := CMD;
        ProcessPostConfigInstruction := True;
        Exit;
        END;

    { These you pay for }

    IF NOT TRFree THEN
        BEGIN
        IF ID = 'ADD DOMESTIC COUNTRY' THEN
            BEGIN
            IF UpperCase (CMD) = 'CLEAR' THEN
                BEGIN
                Write ('Executing ClearDomesticCountryList procedure....');
                ClearDomesticCountryList;
                WriteLn (' completed.');
                END
            ELSE
                BEGIN
                Write ('Adding ', CMD, ' to Domestic Country List...');
                AddDomesticCountry (CMD);
                WriteLn (' complete.');
                END;

            ProcessPostConfigInstruction := True;
            Exit;
            END;

        IF ID = 'CONTEST NAME' THEN
            BEGIN
            ContestName := CMD;
            ProcessPostConfigInstruction := True;
            ContestTitle := GetYearString + ' ' + ContestName + ' ' + MyCall;
            Exit;
            END;

        IF ID = 'CONTEST TITLE' THEN
            BEGIN
            ContestTitle := CMD;
            ProcessPostConfigInstruction := True;
            Exit;
            END;

        IF (ID = 'DOMESTIC FILENAME') OR (ID = 'DOMESTIC QTH DATA FILENAME') THEN
            BEGIN
            DomesticQTHDataFileName := CMD;
            ProcessPostConfigInstruction := True;
            Exit;
            END;

        IF ID = 'DOMESTIC MULTIPLIER' THEN
            BEGIN
            CMD := UpperCase (CMD);
            ActiveDomesticMult := NoDomesticMults;

            IF CMD = 'DOMESTIC FILE' THEN
                ActiveDomesticMult     := DomesticFile;

            IF CMD = 'GRID SQUARES' THEN
                ActiveDomesticMult := GridSquares;

            IF CMD = 'GRID FIELDS' THEN
                ActiveDomesticMult := GridFields;

            IF CMD = 'WYSIWYG' THEN
                ActiveDomesticMult := WYSIWYGDomestic;

            ProcessPostConfigInstruction := (ActiveDomesticMult <> NoDomesticMults) OR (CMD = 'NONE');
            Exit;
            END;

    

        IF ID = 'DX MULTIPLIER' THEN
            BEGIN
            CMD := UpperCase (CMD);
            ActiveDXMult := NoDXMults;

            IF CMD = 'ARRL DXCC' THEN
                ActiveDXMult := ARRLDXCC;

            IF CMD = 'ARRL DXCC WITH NO ARRL SECTIONS' THEN
                ActiveDXMult := ARRLDXCCWithNoARRLSections;

            IF CMD = 'ARRL DXCC WITH NO I OR IS0' THEN
                ActiveDXMult := ARRLDXCCWithNoIOrIS0;

            IF CMD = 'ARRL DXCC WITH NO USA OR CANADA' THEN
                ActiveDXMult := ARRLDXCCWithNoUSAOrCanada;

            IF CMD = 'ARRL DXCC WITH NO USA CANADA KH6 OR KL7' THEN
                ActiveDXMult := ARRLDXCCWithNoUSACanadaKH6OrKL7;

            IF CMD = 'CQ DXCC' THEN
                ActiveDXMult := CQDXCC;

            IF CMD = 'CQ DXCC WITH NO HB9' THEN
                ActiveDXMult := CQDXCCWithNoHB9;

            IF CMD = 'CQ DXCC WITH NO OK' THEN
                ActiveDXMult := CQDXCCWithNoOK;

            IF CMD = 'CQ DXCC WITH NO USA OR CANADA' THEN
                ActiveDXMult := CQDXCCWithNoUSAOrCanada;

            IF CMD = 'CQ EUROPEAN COUNTRIES' THEN
                ActiveDXMult := CQEuropeanCountries;

            IF CMD = 'CQ NON EUROPEAN COUNTRIES' THEN
                ActiveDXMult := CQNonEuropeanCountries;

            IF CMD = 'NON SOUTH AMERICAN COUNTRIES' THEN
                ActiveDXMult := NonSouthAmericanCountries;

            IF CMD = 'NORTH AMERICAN ARRL DXCC WITH NO USA CANADA OR KL7' THEN
                ActiveDXMult := NorthAmericanARRLDXCCWithNoUSACanadaOrkL7;

            IF CMD = 'PACC COUNTRIES AND PREFIXES' THEN
                ActiveDXMult := PACCCountriesAndPrefixes;

            ProcessPostConfigInstruction := (ActiveDXMult <> NoDXMults) OR (CMD = 'NONE');
            Exit;
            END;

        IF ID = 'EXCHANGE RECEIVED' THEN
            BEGIN
            CMD := UpperCase (CMD);
            ActiveExchange := NoExchangeReceived;

            IF CMD = 'CHECK AND CHAPTER OR QTH EXCHANGE' THEN
                ActiveExchange := CheckAndChapterOrQTHExchange;

            IF CMD = 'CLASS DOMESTIC OR DX QTH' THEN
                ActiveExchange := ClassDomesticOrDXQTHExchange;

            IF CMD = 'KIDS DAY EXCHANGE' THEN
                ActiveExchange := KidsDayExchange;

            IF CMD = 'NAME DOMESTIC OR DX QTH' THEN
                ActiveExchange := NameAndDomesticOrDXQTHExchange;

            IF CMD = 'NAME AND POSSIBLE GRID SQUARE' THEN
                ActiveExchange := NameAndPossibleGridSquareExchange;

            IF CMD = 'NAME QTH AND POSSIBLE TEN TEN NUMBER' THEN
                ActiveExchange := NameQTHAndPossibleTenTenNumber;

            IF CMD = 'QSO NUMBER AND NAME' THEN
                ActiveExchange := QSONumberAndNameExchange;

            IF CMD = 'QSO NUMBER DOMESTIC OR DX QTH' THEN
                ActiveExchange := QSONumberDomesticOrDXQTHExchange;

            IF CMD = 'QSO NUMBER DOMESTIC QTH' THEN
                ActiveExchange := QSONumberDomesticQTHExchange;

            IF CMD = 'QSO NUMBER NAME CHAPTER AND QTH' THEN
                ActiveExchange := QSONumberNameChapterAndQTHExchange;

            IF CMD = 'QSO NUMBER NAME DOMESTIC OR DX QTH' THEN
                ActiveExchange := QSONumberNameDomesticOrDXQTHExchange;

            IF CMD = 'QSO NUMBER PRECEDENCE CHECK SECTION' THEN
                ActiveExchange := QSONumberPrecedenceCheckDomesticQTHExchange;

            IF CMD = 'RST AGE' THEN
                ActiveExchange := RSTAgeExchange;

            IF CMD = 'RST ALL JA PREFECTURE AND PRECEDENCE' THEN
                ActiveExchange := RSTALLJAPrefectureAndPrecedenceExchange;

            IF CMD = 'RST AND CONTINENT' THEN
                ActiveExchange := RSTAndContinentExchange;

            IF CMD = 'RST AND GRID' THEN
                ActiveExchange := RSTAndGridExchange;

            IF CMD = 'RST AND OR GRID' THEN
                ActiveExchange := RSTAndOrGridExchange;

            IF CMD = 'RST AND POSTAL CODE' THEN
                ActiveExchange := RSTAndPostalCodeExchange;

            IF CMD = 'RST DOMESTIC OR DX QTH' THEN
                ActiveExchange := RSTDomesticOrDXQTHExchange;

            IF CMD = 'RST DOMESTIC QTH' THEN
                ActiveExchange := RSTDomesticQTHExchange;

            IF CMD = 'RST DOMESTIC QTH OR QSO NUMBER' THEN
                ActiveExchange := RSTDomesticQTHOrQSONumberExchange;

            IF CMD = 'RST NAME QTH' THEN
                ActiveExchange := RSTNameAndQTHExchange;

            IF CMD = 'RST POSSIBLE DOMESTIC QTH AND POWER' THEN
                ActiveExchange := RSTPossibleDomesticQTHAndPower;

            IF CMD = 'RST POWER' THEN
                ActiveExchange := RSTPowerExchange;

            IF CMD = 'RST PREFECTURE' THEN
                ActiveExchange := RSTPrefectureExchange;

            IF CMD = 'RST QSO NUMBER' THEN
                ActiveExchange := RSTQSONumberExchange;

            IF CMD = 'RST QSO NUMBER AND DOMESTIC QTH' THEN
                ActiveExchange := RSTQSONumberAndDomesticQTHExchange;

            IF CMD = 'RST QSO NUMBER AND GRID SQUARE' THEN
                ActiveExchange := RSTQSONumberAndGridSquareExchange;

            IF CMD = 'RST QSO NUMBER AND POSSIBLE DOMESTIC QTH' THEN
                ActiveExchange := RSTQSONumberAndPossibleDomesticQTHExchange;

            IF CMD = 'RST QSO NUMBER AND RANDOM CHARACTERS' THEN
                ActiveExchange := RSTQSONumberAndRandomCharactersExchange;

            IF CMD = 'RST QTH NAME AND FISTS NUMBER OR POWER' THEN
                ActiveExchange := RSTQTHNameAndFistsNumberOrPowerExchange;

            IF CMD = 'RST QSO NUMBER OR DOMESTIC QTH' THEN
                ActiveExchange := RSTAndQSONumberOrDomesticQTHExchange;

            IF CMD = 'RST QTH' THEN
                ActiveExchange := RSTQTHExchange;

            IF CMD = 'RST ZONE' THEN
                ActiveExchange := RSTZoneExchange;

            IF CMD = 'RST ZONE AND POSSIBLE DOMESTIC QTH' THEN
                ActiveExchange := RSTZoneAndPossibleDomesticQTHExchange;

            IF CMD = 'RST ZONE OR SOCIETY' THEN
                ActiveExchange := RSTZoneOrSocietyExchange;

            ProcessPostConfigInstruction := ActiveExchange <> NoExchangeReceived;
            Exit;
            END;

        IF ID = 'MULT BY BAND' THEN
            BEGIN
            MultByBand := UpCase (CMD [1]) = 'T';
            ProcessPostConfigInstruction := True;
            Exit;
            END;

        IF ID = 'MULT BY MODE' THEN
            BEGIN
            MultByMode   := UpCase (CMD [1]) = 'T';
            ProcessPostConfigInstruction := True;
            Exit;
            END;

        IF ID = 'PREFIX MULTIPLIER' THEN
            BEGIN
            IF UpperCase (CMD) = 'NONE' THEN
                BEGIN
                ActivePrefixMult := NoPrefixMults;
                ProcessPostConfigInstruction := True;
                END;

            IF UpperCase (CMD) = 'PREFIX' THEN
                BEGIN
                ActivePrefixMult := Prefix;
                ProcessPostConfigInstruction := True;
                END;

            IF UpperCase (CMD) = 'SOUTH AMERICAN PREFIXES' THEN
                BEGIN
                ActivePrefixMult := SouthAmericanPrefixes;
                ProcessPostConfigInstruction := True;
                END;

            Exit;
            END;

        IF ID = 'QSO BY BAND' THEN
            BEGIN
            QSOByBand := UpCase (CMD [1]) = 'T';
            ProcessPostConfigInstruction := True;
            Exit;
            END;

        IF ID = 'QSO BY MODE' THEN
            BEGIN
            QSOByMode := UpCase (CMD [1]) = 'T';
            ProcessPostConfigInstruction := True;
            Exit;
            END;


    

        IF ID = 'QSO POINT METHOD' THEN
            BEGIN
            CMD := UpperCase (CMD);
            ActiveQSOPointMethod := NoQSOPointMethod;

            IF CMD = 'ALL ASIAN' THEN
                ActiveQSOPointMethod := AllAsianQSOPointMethod;

            IF CMD = 'ARCI' THEN
                ActiveQSOPointMethod := ARCIQSOPointMethod;

            IF CMD = 'ARI' THEN
                ActiveQSOPointMethod := ARIQSOPointMethod;

            IF CMD = 'ARRL DX' THEN
                ActiveQSOPointMethod := ARRLDXQSOPointMethod;

            IF CMD = 'ARRL 160' THEN
                ActiveQSOPointMethod := ARRL160QSOPointMethod;

            IF CMD = 'ARRL 10' THEN
                ActiveQSOPointMethod := ARRL10QSOPointMethod;

            IF CMD = 'ARRL VHF' THEN
                ActiveQSOPointMethod := ARRLVHFQSOPointMethod;

            IF CMD = 'ARRL VHF SS' THEN
                ActiveQSOPointMethod := ARRLVHFSSPointMethod;

            IF CMD = 'BALTIC' THEN
                ActiveQSOPointMethod := BalticQSOPointMethod;

            IF CMD = 'CQ 160' THEN
                ActiveQSOPointMethod := CQ160QSOPointMethod;

            IF CMD = 'CQ M' THEN
                ActiveQSOPointMethod := CQMQSOPointMethod;

            IF CMD = 'CQ WW' THEN
                ActiveQSOPointMethod := CQWWQSOPointMethod;

            IF CMD = 'CQ WW RTTY' THEN
                ActiveQSOPointMethod := CQWWRTTYQSOPointMethod;

            IF CMD = 'CQ WPX' THEN
                ActiveQSOPointMethod := CQWPXQSOPointMethod;

            IF CMD = 'CQ WPX RTTY' THEN
                ActiveQSOPointMethod := CQWPXRTTYQSOPointMethod;

            IF CMD = 'CROATIAN' THEN
                ActiveQSOPointMethod := CroatianQSOPointMethod;

            IF CMD = 'EUROPEAN FIELD DAY' THEN
                ActiveQSOPointMethod := EuropeanFieldDayQSOPointMethod;

            IF CMD = 'EUROPEAN SPRINT' THEN
                ActiveQSOPointMethod := EuropeanSprintQSOPointMethod;

            IF CMD = 'EUROPEAN VHF' THEN
                ActiveQSOPointMethod := EuropeanVHFQSOPointMethod;

            IF CMD = 'FISTS' THEN
                ActiveQSOPointMethod := FistsQSOPointMethod;

            IF CMD = 'HA DX' THEN
                ActiveQSOPointMethod := HADXQSOPointMethod;

            IF CMD = 'HELVETIA' THEN
                ActiveQSOPointMethod := HelvetiaQSOPointMethod;

            IF CMD = 'IARU' THEN
                ActiveQSOPointMethod := IARUQSOPointMethod;

            IF CMD = 'IOTA' THEN
                ActiveQSOPointMethod := IOTAQSOPointMethod;

            IF CMD = 'JA INTERNATIONAL DX' THEN
                ActiveQSOPointMethod := JapanInternationalDXQSOPointMethod;

            IF CMD = 'KCJ' THEN
                ActiveQSOPointMethod := KCJQSOPointMethod;

            IF CMD = 'MQP' THEN
                ActiveQSOPointMethod := MQPQSOPointMethod;

            IF CMD = 'NZ FIELD DAY' THEN
                ActiveQSOPointMethod := NZFieldDayQSOPointMethod;

            IF CMD = 'OK DX' THEN
                ActiveQSOPointMethod := HADXQSOPointMethod;

            IF CMD = 'REGION ONE FIELD DAY' THEN
                ActiveQSOPointMethod := EuropeanFieldDayQSOPointMethod;

            IF CMD = 'RSGB' THEN
                ActiveQSOPointMethod := RSGBQSOPointMethod;

            IF CMD = 'RUSSIAN DX' THEN
                ActiveQSOPointMethod := RussianDXQSOPointMethod;

            IF CMD = 'SALMON RUN' THEN
                ActiveQSOPointMethod := SalmonRunQSOPointMethod;

            IF CMD = 'SCANDINAVIAN' THEN
                ActiveQSOPointMethod := ScandinavianQSOPointMethod;

            IF CMD = 'SL FIVE POINT' THEN
                ActiveQSOPointMethod := SLFivePointQSOMethod;

            IF CMD = 'SOUTH AMERICAN WW' THEN
                ActiveQSOPointMethod := SouthAmericanWWQSOPointMethod;

            IF CMD = 'SOUTH AMERICAN' THEN
                ActiveQSOPointMethod := SouthAmericanQSOPointMethod;

            IF CMD = 'STEW PERRY' THEN
                ActiveQSOPointMethod := StewPerryQSOPointMethod;

            IF CMD = 'TEN TEN' THEN
                ActiveQSOPointMethod := TenTenQSOPointMethod;

            IF CMD = 'TOEC' THEN
                ActiveQSOPointMethod := TOECQSOPointMethod;

            IF CMD = 'UBA' THEN
                ActiveQSOPointMethod := UBAQSOPointMethod;

            IF CMD = 'UKRAINIAN' THEN
                ActiveQSOPointMethod := UkrainianQSOPointMethod;

            IF CMD = 'VK ZL' THEN
                ActiveQSOPointMethod := VKZLQSOPointMethod;

            IF CMD = 'WAE' THEN
                ActiveQSOPointMethod := WAEQSOPointMethod;

            IF CMD = 'WWL' THEN
                ActiveQSOPointMethod := WWLQSOPointMethod;

            IF CMD = 'YO DX' THEN
                ActiveQSOPointMethod := YODXQSOPointMethod;

            IF CMD = 'ONE POINT PER QSO' THEN
                ActiveQSOPointMethod := OnePointPerQSO;

            IF CMD = 'TWO POINTS PER QSO' THEN
                ActiveQSOPointMethod := TwoPointsPerQSO;

            IF CMD = 'THREE POINTS PER QSO' THEN
                ActiveQSOPointMethod := ThreePointsPerQSO;

            IF CMD = 'TWO PHONE THREE CW' THEN
                ActiveQSOPointMethod := TwoPhoneThreeCW;

            IF CMD = 'TWO PHONE FOUR CW' THEN
                ActiveQSOPointMethod := TwoPhoneFourCW;

            IF CMD = 'ONE PHONE TWO CW' THEN
                ActiveQSOPointMethod := OnePhoneTwoCW;

            IF CMD = 'THREE PHONE FIVE CW' THEN
                ActiveQSOPointMethod := ThreePhoneFiveCW;

            IF CMD = 'TEN POINTS PER QSO' THEN
                ActiveQSOPointMethod := TenPointsPerQSO;

            ProcessPostConfigInstruction := (ActiveQSOPointMethod <> NoQSOPointMethod) OR (CMD = 'NONE');
            Exit;
            END;

        IF ID = 'QSO POINTS DOMESTIC CW' THEN
            BEGIN
            Val (CMD, QSOPointsDomesticCW, xResult);
            ProcessPostConfigInstruction := xResult = 0;
            Exit;
            END;

        IF ID = 'QSO POINTS DOMESTIC PHONE' THEN
            BEGIN
            Val (CMD, QSOPointsDomesticPhone, xResult);
            ProcessPostConfigInstruction := xResult = 0;
            Exit;
            END;

        IF ID = 'QSO POINTS DX CW' THEN
            BEGIN
            Val (CMD, QSOPointsDXCW, xResult);
            ProcessPostConfigInstruction := xResult = 0;
            Exit;
            END;

        IF ID = 'QSO POINTS DX PHONE' THEN
            BEGIN
            Val (CMD, QSOPointsDXPhone, xResult);
            ProcessPostConfigInstruction := xResult = 0;
            Exit;
            END;

        IF ID = 'ZONE MULTIPLIER' THEN
            BEGIN
            IF UpperCase (CMD) = 'NONE' THEN
                BEGIN
                ActiveZoneMult := NoZoneMults;
                ProcessPostConfigInstruction := True;
                END;

            IF UpperCase (CMD) = 'BRANCH ZONES' THEN
                BEGIN
                ActiveZoneMult := BranchZones;
                ProcessPostConfigInstruction := True;
                END;

            IF UpperCase (CMD) = 'CQ ZONES' THEN
                BEGIN
                CountryTable.ZoneMode := CQZoneMode;
                ActiveZoneMult := CQZones;
                ActiveInitialExchange := ZoneInitialExchange;
                ProcessPostConfigInstruction := True;
                END;

            IF UpperCase (CMD) = 'ITU ZONES' THEN
                BEGIN
                CountryTable.ZoneMode := ITUZoneMode;
                ActiveZoneMult := ITUZones;
                ActiveInitialExchange := ZoneInitialExchange;
                ProcessPostConfigInstruction := True;
                END;

            IF UpperCase (CMD) = 'JA PREFECTURES' THEN
                BEGIN
                ActiveZoneMult := JAPrefectures;
                ProcessPostConfigInstruction := True;
                END;

            Exit;
            END;
        END;

    END;

