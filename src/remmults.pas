PROCEDURE DupeAndMultSheet.CancelOutNewDomesticMultWeHaveWorked (MultString: Str20;
                                                                 Band: BandType;
                                                                 Mode: ModeType);
VAR Address: INTEGER;

    BEGIN
    IF DomQTHTable.NumberRemainingMults = 0 THEN Exit;

    Address := DomQTHTable.GetDomMultInteger (MultString);

    IF Address <> -1 THEN
        RemMultMatrix [Band, Mode, Domestic]^ [Address] := False;
    END;



PROCEDURE DupeAndMultSheet.CancelOutNewDXMultWeHaveWorked (MultString: Str20;
                                                           Band: BandType;
                                                           Mode: ModeType);
VAR Address: INTEGER;

    BEGIN
    IF CountryTable.NumberRemainingCountries = 0 THEN Exit;

    Address := CountryTable.GetDXMultInteger (MultString);

    IF Address <> -1 THEN
        RemMultMatrix [Band, Mode, DX]^ [Address] := False;
    END;



PROCEDURE DupeAndMultSheet.CancelOutNewZoneMultWeHaveWorked (MultString: Str20;
                                                             Band: BandType;
                                                             Mode: ModeType);
VAR Address, Result: INTEGER;

    BEGIN
    IF RemMultMatrix [Band, Mode, Zone] = nil then Exit;

    Val (MultString, Address, Result);

    IF Result <> 0 THEN Exit;

    IF ActiveZoneMult <> EuHFCYear THEN
        Dec (Address);

    RemMultMatrix [Band, Mode, Zone]^ [Address] := False;
    END;



PROCEDURE DupeAndMultSheet.CancelOutRemainingMultsWeHaveWorked (Band: BandType;
                                                                Mode: ModeType;
                                                                MultType: RemainingMultiplierType);

VAR Result, NumberMults, Mult, Address: INTEGER;

    BEGIN
    IF MultType = Domestic THEN
        BEGIN
        NumberMults := MultSheet.Totals [Band, Mode].NumberDomesticMults;

        IF (NumberMults = 0) OR (RemMultMatrix [Band, Mode, Domestic] = nil) THEN Exit;

        FOR Mult := 0 TO NumberMults - 1 DO
            BEGIN
            Address := DomQTHTable.GetDomMultInteger (ExpandedString (MultSheet.DomesticList [Band, Mode]^ [Mult]));

            IF Address <> -1 THEN
                RemMultMatrix [Band, Mode, Domestic]^ [Address] := False;
            END;
        END;

    IF MultType = DX THEN
        BEGIN
        NumberMults := MultSheet.Totals [Band, Mode].NumberDXMults;

        IF (NumberMults = 0) OR (RemMultMatrix [Band, Mode, DX] = nil) THEN Exit;

        FOR Mult := 0 TO NumberMults - 1 DO
            BEGIN
            Address := CountryTable.GetDXMultInteger (ExpandedString (MultSheet.DXList [Band, Mode]^ [Mult]));

            IF Address <> -1 THEN
                RemMultMatrix [Band, Mode, DX]^ [Address] := False;
            END;
        END;

    IF MultType = Zone THEN
        BEGIN
        NumberMults := MultSheet.Totals [Band, Mode].NumberZoneMults;

        IF (NumberMults = 0) OR (RemMultMatrix [Band, Mode, Zone] = nil) THEN Exit;

        FOR Mult := 0 TO NumberMults - 1 DO
            BEGIN
            Val (ExpandedString (MultSheet.ZoneList [Band, Mode]^ [Mult]), Address, Result);

           IF ActiveZoneMult <> EUHFCYear THEN
                Dec (Address);  { Zone 1 is address 0 }

            IF (Address >= 0) AND (Address < 100) AND (Result = 0) THEN
                RemMultMatrix [Band, Mode, Zone]^ [Address] := False;
            END;
        END;
    END;



PROCEDURE DupeAndMultSheet.SetUpRemainingMultiplierArrays;

VAR StartBand, FinishBand, Band: BandType;
    StartMode, FinishMode, Mode: ModeType;
    Address: INTEGER;

    BEGIN
    IF MultByBand THEN
        BEGIN
        StartBand  := Band160;
        FinishBand := Band2304;
        END
    ELSE
        BEGIN
        StartBand  := All;
        FinishBand := All;
        END;

    IF MultByMode THEN
        BEGIN
        StartMode  := CW;
        FinishMode := Phone;
        END
    ELSE
        BEGIN
        StartMode  := Both;
        FinishMode := Both;
        END;

    {IF ActiveDomesticMult = DomesticFile THEN}
    {KK1L: 6.68 Supports saving dom mults when using a dom mult file at any time}
    IF (DoingDomesticMults AND (DomesticQTHDataFileName <> '')) THEN
        BEGIN
        IF DomQTHTable.NumberRemainingMults > 0 THEN
            FOR Band := StartBand TO FinishBand DO
                FOR Mode := StartMode TO FinishMode DO
                    BEGIN
                    IF RemMultMatrix [Band, Mode, Domestic] = nil THEN
                        IF MaxAvail > SizeOf (RemainingMultList) THEN
                            BEGIN
                            New (RemMultMatrix [Band, Mode, Domestic]);

                            FOR Address := 0 TO DomQTHTable.NumberRemainingMults - 1 DO
                                RemMultMatrix [Band, Mode, Domestic]^ [Address] := True;
                            END;

                    CancelOutRemainingMultsWeHaveWorked (Band, Mode, Domestic);
                    END;
        END;

    IF (DoingDXMults) THEN
        BEGIN
        IF CountryTable.NumberRemainingCountries > 0 THEN
            FOR Band := StartBand TO FinishBand DO
                FOR Mode := StartMode TO FinishMode DO
                    BEGIN
                    IF RemMultMatrix [Band, Mode, DX] = nil THEN
                        IF MaxAvail > SizeOf (RemainingMultList) THEN
                            BEGIN
                            New (RemMultMatrix [Band, Mode, DX]);

                            FOR Address := 0 TO CountryTable.NumberRemainingCountries - 1 DO
                                RemMultMatrix [Band, Mode, DX]^ [Address] := True;
                            END;

                    CancelOutRemainingMultsWeHaveWorked (Band, Mode, DX);
                    END;
        END;

    IF DoingZoneMults THEN
        BEGIN
        CASE ActiveZoneMult OF
            CQZones:       MaxNumberOfZones := 40;
            EUHFCYear:     MaxNumberOfZones := 100;
            ITUZones:      MaxNumberOfZones := 75;
            JAPrefectures: MaxNumberOfZones := 50;
            BranchZones:   MaxNumberOfZones := 99;
            ELSE           MaxNumberOfZones := 0;
            END;

        IF MaxNumberOfZones > 0 THEN
            FOR Band := StartBand TO FinishBand DO
                FOR Mode := StartMode TO FinishMode DO
                    BEGIN
                    IF RemMultMatrix [Band, Mode, Zone] = Nil THEN
                        IF MaxAvail > SizeOf (RemainingMultList) THEN
                            BEGIN
                            New (RemMultMatrix [Band, Mode, Zone]);

                            FOR Address := 0 TO MaxNumberOfZones - 1 DO
                                RemMultMatrix [Band, Mode, Zone]^ [Address] := True;
                            END;

                    CancelOutRemainingMultsWeHaveWorked (Band, Mode, Zone);
                    END;
        END;
    END;


