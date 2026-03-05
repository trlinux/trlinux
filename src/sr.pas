PROCEDURE TransferCabrilloInformationIntoScoreReportRecord;

    ScoreReporterCabrilloContest: STRING;                  { Set in fcontest.pas }
    ScoreReporterCabrilloCategory: CabrilloCategoryRecord; { Set in ControlJ }


{ This procedure bridges the gap from the Cabrillo category data that is in the
  Control-J menu to the category fields in the scorereport class.  It is intended
  to be called when generating a new score report so that any changes made are
  reflected in the data }

    BEGIN
    WITH ScoreReporterCabrilloCategory DO
        BEGIN
        CASE CategoryAssisted OF
            NoCategoryAssistedType: scorereport.setclassassisted ('');
            AssistedType:           scorereport.setclassassisted ('ASSISTED');
            ELSE                    scorereport.setclassassisted ('NON-ASSISTED');
            END;  { of CASE CategoryAssisted }

        CASE CategoryBand OF
            NoCategoryBandType: scorereport.setclassbands ('');
            SingleBand160Type:  scorereport.setclassbands ('160M');
            SingleBand80Type:   scorereport.setclassbands ('80M');
            SingleBand40Type:   scorereport.setclassbands ('40M');
            SingleBand20Type:   scorereport.setclassbands ('20M');
            SingleBand15Type:   scorereport.setclassbands ('15M');
            SingleBand10Type:   scorereport.setclassbands ('10M');
            ELSE                scorereport.setclassbands ('ALL');
            END;  { of CASE CategoryBand }

       CASE CategoryMode OF
           NoCategoryModeType: scorereport.setclassmode ('');
           CWModeType:         scorereport.setclassmode ('CW');
           DigiModeType:       scorereport.setclassmode ('DIGI');
           RTTYModeType:       scorereport.setclassmode ('RTTY');
           SSBModeType:        scorereport.setclassmode ('PH');
           ELSE                scorereport.setclassmode ('MIXED');
           END;

       { No checklog indicator for Scoreboard - and we can't leave it blank }

       CASE CategoryOperator OF
           MultiOperatorType: scorereport.setops ('MULTI-OP');
           ELSE               scorereport.setops ('SINGLE-OP');
           END;

       CASE CategoryPower OF
           NoCategoryPowerType: scorereport.setclasspower ('');
           HighPowerType:       scorereport.setclasspower ('HIGH');
           LowPowerType:        scorereport.setclasspower ('LOW');
           QRPPowerType:        scorereport.setclasspower ('QRP');
           END;

       { Currently - I don't think anything in CategoryStation corresponds
         to any field in the scorereporter data structure

       CASE CategoryStation OF
           NoCategoryStationType:
           DistributedStationType:
           FixedStationType:
           MobileStationType:
           PortableStationType:
           RoverStationType:
           RoverLimitedStationType:
           RoverUnlimitedStationType:
           ExpeditionStationType:
           HQStationType:
           SchoolStationType:
           ExplorerStationType:
           END;   }

       { Currently - I don't think anything in CategoryTime corresponds to
         any field in the scorereporter data structure

       CASE CategoryTime OF
           NoCategoryTimeType:
           SixHourTimeType:
           EightHourTimeType:
           TwelveHourTimeType:
           TwentyFourHourTimeType:
           END;  }

       CASE CategoryTransmitter OF
           NoCategoryTransmitterType: scorereporter.setclasstransmitter ('');
           OneTransmitterType:        scorereporter.setclasstransmitter ('ONE');
           TwoTransmitterType:        scorereporter.setclasstransmitter ('TWO');
           LimitedTransmitterType:    scorereporter.setclasstransmitter ('');
           UnlimitedTransmitterType:  scorereporter.setclasstransmitter ('UNLIMITED');
           SWLTransmitterType:        scorereporter.setclasstransmitter ('');
           END;

       CASE CategoryOverlay OF
           NoCategoryOverlayType: scorereporter.setclassoverlay ('');
           ClassicOverlayType:    scorereporter.setclassoverlay ('CLASSIC');
           RookieOverlayType:     scorereporter.setclassoverlay ('ROOKIE');
           TBWiresOverlayType:    scorereporter.setclassoverlay ('TB-WIRES');
           YouthOverlayType:      scorereporter.setclassoverlay ('');
           NoviceTechOverlayType: scorereporter.setclassoverlay ('');
           YLOverlayType:         scorereporter.setclassoverlay ('');
           END;

    END;



