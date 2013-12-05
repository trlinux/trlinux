UNIT LogDDX;

{$O+}
{$V-}

INTERFACE

USES Dos, Printer, Tree, Country9, ZoneCont, LogSCP, LogWind, LogCW,
     LogDupe, LogGrid, LogHelp, LogK1EA, LogDVP, LogDom, LogStuff, LogEdit;

TYPE
    DDXActivity = (MaybeSendANewCall,
                   MaybeRespondToMyCall,
                   NormalContactComplete,
                   QSLMyExchange,
                   RepeatCallsign,
                   RepeatExchange,
                   SendCorrectedCallMessage,
                   SendExchange,
                   SendNewCall,
                   VerifyContact);

    DDXStates = (Off,
                 CallSentInResponseToACQ,
                 CQExchangeSent,
                 CQSent,
                 QTCInProgress,
                 ReadyForAnotherCall,
                 RepeatCall,
                 SAndPExchangeSent,
                 SAndPExchangeSentAndAskedForQTC,
                 Standby,
                 WaitingForCQ,
                 WaitingForYourCall,
                 WaitingForYourExchange);


VAR
    DDXState:                DDXStates;
    DDXCall1:                CallString;
    DDXCall2:                CallString;
    DDXNextExchange:         Str20;
    DDXNextExCall:           CallString;
    DDXTone1:                INTEGER;
    DDXTone2:                INTEGER;
    RememberDDXCallsign:     CallString;

    FUNCTION  DDX (Activity: DDXActivity): BOOLEAN;

    PROCEDURE DDXAnswerACQ (VAR Call: CallString;
                            VAR Tone: INTEGER;
                            StationToBeCalled: CallString);

    FUNCTION  DDXExchange (SendingStation: CallString;
                           ReceivingStation: CallString): Str80;

    PROCEDURE DDXSendSAndPExchange (SendingStation: CallString;
                                    ReceivingStation:  CallString;
                                    Tone: INTEGER);
    FUNCTION  GetNextCallFromReadInLog: CallString;
    FUNCTION  GetRandomDDXCallsign (Exchange: ExchangeType): CallString;
    FUNCTION  GetRandomDomesticQTH (Call: CallString): Str80;

IMPLEMENTATION
uses keycode,radio,timer;

FUNCTION RandomRST: Str20;

    BEGIN
    CASE Random (40) OF
            0: RandomRST := '559 ';
            1: RandomRST := '569 ';
            2: RandomRST := '579 ';
            3: RandomRST := '589 ';
         4..9: RandomRST := '599 ';
       10..25: RandomRST := '5NN ';
         ELSE  RandomRST := '5NN';
         END;
     END;



FUNCTION PossiblyMakeCallsignRare (InputCall: CallString): CallString;

VAR ID, Call: CallString;
    Country: INTEGER;

    BEGIN
    IF Random (15) = 0 THEN
        BEGIN
        REPEAT
            REPEAT
                Call := CD.GetRandomCall;
            UNTIL (Pos ('?', Call) = 0);

            Country := CountryTable.GetCountry (Call, True);
            ID := CountryTable.GetCountryID (Country);
        UNTIL (ID <> 'DA') AND (ID <> 'F')  AND
              (ID <> 'EA') AND (ID <> 'HC') AND
              (ID <> 'HL') AND (ID <> 'LA') AND
              (ID <> 'LZ') AND (ID <> 'OE') AND
              (ID <> 'G')  AND (ID <> 'HA') AND
              (ID <> 'HB') AND (ID <> 'I')  AND
              (ID <> 'JA')  AND (ID <> 'OH') AND
              (ID <> 'OK')  AND (ID <> 'PA') AND
              (ID <> 'SM')  AND (ID <> 'SP') AND
              (ID <> 'UA')  AND (ID <> 'UA9') AND
              (ID <> 'UR')  AND (ID <> 'LY') AND
              (ID <> 'EU')  AND (ID <> '4L') AND
              (ID <> 'VE')  AND (ID <> 'VK') AND
              (ID <> 'K')   AND (ID <> 'YU') AND
              (ID <> 'XE')  AND (ID <> 'KH6') AND
              (ID <> 'KL') AND (ID <> 'IS0');

        PossiblyMakeCallsignRare := Call;
        END
    ELSE
        PossiblyMakeCallsignRare := InputCall;
    END;



FUNCTION GetRandomDDXCallsign (Exchange: ExchangeType): CallString;

VAR CountryID, Call: CallString;

    BEGIN
    Call := '';

    CASE Exchange OF
        QSONumberNameDomesticOrDXQTHExchange:
            REPEAT
                Call := CD.GetRandomCall;
            UNTIL (DomesticCountryCall (Call)) AND
                  (Pos ('?', Call) = 0) AND
                  (CD.GetName (Call) <> '');

        ClassDomesticOrDXQTHExchange,
        QSONumberDomesticOrDXQTHExchange,
        QSONumberDomesticQTHExchange,
{KS        QSONumberNameDomesticOrDXQTHExchange,}
        QSONumberPrecedenceCheckDomesticQTHExchange,
        RSTDomesticOrDXQTHExchange,
        RSTDomesticQTHExchange:
            BEGIN
            IF ActiveDomesticMult = DomesticFile THEN
                BEGIN
                IF DomQTHTable.ActiveDomQTHFile = 'ARRLSECT.DOM' THEN
                    REPEAT
                        Call := CD.GetRandomCall;
                    UNTIL (DomesticCountryCall (Call)) AND (Pos ('?', Call) = 0);

                IF DomQTHTable.ActiveDomQTHFile = 'HUNGARY.DOM' THEN
                    REPEAT
                        Call := CD.GetRandomCall;
                    UNTIL CountryTable.GetCountryID (CountryTable.GetCountry (Call, True)) = 'HA';

                IF (DomQTHTable.ActiveDomQTHFile [1] = 'S') AND (DomQTHTable.ActiveDomQTHFile [4] = 'P') THEN
                    REPEAT
                        Call := CD.GetRandomCall;
                    UNTIL (DomesticCountryCall (Call)) AND (Pos ('?', Call) = 0);

                IF DomQTHTable.ActiveDomQTHFile = 'SWISS.DOM' THEN
                    REPEAT
                        Call := CD.GetRandomCall;
                    UNTIL CountryTable.GetCountryID (CountryTable.GetCountry (Call, True)) = 'HB';

                IF DomQTHTable.ActiveDomQTHFile = 'JAPREF.DOM' THEN
                    REPEAT
                        Call := CD.GetRandomCall;
                    UNTIL CountryTable.GetCountryID (CountryTable.GetCountry (Call, True)) = 'JA';
                END;

            END;


        RSTAgeExchange:
            BEGIN
            IF MyContinent <> Asia THEN
                BEGIN
                REPEAT
                    Call := CD.GetRandomCall;
                UNTIL CountryTable.GetContinent (Call) = Asia;
                END
            ELSE
                BEGIN
                REPEAT
                    Call := CD.GetRandomCall;
                UNTIL CountryTable.GetContinent (Call) <> Asia;
                END;
            END;

        RSTPowerExchange:
            BEGIN
            REPEAT
                Call   := CD.GetRandomCall;
            UNTIL NOT DomesticCountryCall (Call);
            Call := PossiblyMakeCallsignRare (Call);
            END;

        RSTQSONumberExchange,
        RSTZoneExchange:
            BEGIN
            IF ActiveZoneMult = JAPrefectures THEN
                BEGIN
                REPEAT
                    Call := CD.GetRandomCall
                UNTIL CountryTable.GetCountryID (CountryTable.GetCountry (Call, True)) = 'JA';
                GetRandomDDXCallsign := Call;
                Exit;
                END;

            IF ContestName = 'Scandinavian Contest' THEN
                BEGIN
                IF ScandinavianCountry (MyCountry) THEN
                    BEGIN
                    REPEAT
                        Call := CD.GetRandomCall;
                    UNTIL NOT ScandinavianCountry (CountryTable.GetCountryID (CountryTable.GetCountry (Call, True)));
                    END
                ELSE
                    BEGIN
                    REPEAT
                        Call := CD.GetRandomCall;
                    UNTIL ScandinavianCountry (CountryTable.GetCountryID (CountryTable.GetCountry (Call, True)));
                    END;

                GetRandomDDXCallsign := Call;
                Exit;
                END;


            CASE ActiveDXMult OF
                ARRLDXCC:       { Must be the JA INTL DX contest in Japan }
                    BEGIN
                    REPEAT
                        Call := CD.GetRandomCall;
                    UNTIL CountryTable.GetCountryID (CountryTable.GetCountry (Call, True)) <> 'JA';
                    GetRandomDDXCallsign := Call;
                    Exit;
                    END;


                CQDXCCWithNoUSAOrCanada,
                ARRLDXCCWithNoUSAOrCanada:
                    REPEAT
                        Call := CD.GetRandomCall;
                        Call := PossiblyMakeCallsignRare (Call);
                        CountryID := CountryTable.GetCountryID (CountryTable.GetCountry (Call, True));
                    UNTIL (CountryID <> 'K') AND (CountryID <> 'VE');

                ARRLDXCCWithNoUSACanadakH6OrkL7,
                ARRLDXCCWithNoARRLSections:
                    REPEAT
                        Call := CD.GetRandomCall;
                        Call := PossiblyMakeCallsignRare (Call);
                        CountryID := CountryTable.GetCountryID (CountryTable.GetCountry (Call, True));
                    UNTIL (CountryID <> 'K')   AND (CountryID <> 'VE') AND
                          (CountryID <> 'KH6') AND (CountryID <> 'KL');

                ARRLDXCCWithNoIOrIS0:
                    REPEAT
                        Call := CD.GetRandomCall;
                        Call := PossiblyMakeCallsignRare (Call);
                        CountryID := CountryTable.GetCountryID (CountryTable.GetCountry (Call, True));
                    UNTIL (CountryID <> 'I') AND (CountryID <> 'IS0');

                CQNonEuropeanCountries, CQNonEuropeanCountriesAndWAECallRegions: {KK1L: 6.70 added for new WAE rules}
                    BEGIN
                    REPEAT
                        Call := CD.GetRandomCall;
                        Call := PossiblyMakeCallsignRare (Call);
                    UNTIL CountryTable.GetContinent (Call) <> Europe;
                    END;

                CQEuropeanCountries:
                    BEGIN
                    REPEAT
                        Call := CD.GetRandomCall;
                        Call := PossiblyMakeCallsignRare (Call);
                    UNTIL CountryTable.GetContinent (Call) = Europe;
                    END;

                CQDXCCWithNoHB9:
                    REPEAT
                        Call := CD.GetRandomCall;
                        Call := PossiblyMakeCallsignRare (Call);
                        CountryID := CountryTable.GetCountryID (CountryTable.GetCountry (Call, True));
                    UNTIL (CountryID <> 'HB');

                NorthAmericanARRLDXCCWithNoUSACanadaOrkL7:
                    BEGIN
                    REPEAT
                        Call := CD.GetRandomCall;
                        Call := PossiblyMakeCallsignRare (Call);
                        CountryID := CountryTable.GetCountryID (CountryTable.GetCountry (Call, True));
                    UNTIL (CountryID <> 'K')   AND (CountryID <> 'VE') AND
                          (CountryID <> 'KL') AND
                          (CountryTable.GetContinent (Call) = NorthAmerica);
                    END;

                NonSouthAmericanCountries:
                    BEGIN
                    REPEAT
                        Call := CD.GetRandomCall;
                        Call := PossiblyMakeCallsignRare (Call);
                    UNTIL CountryTable.GetContinent (Call) <> SouthAmerica;
                    END;

                ELSE
                    BEGIN
                    Call := CD.GetRandomCall;
                    Call := PossiblyMakeCallsignRare (Call);
                    END;

                END;
            END;

        RSTZoneOrSocietyExchange:
            BEGIN
            Call := CD.GetRandomCall;
            Call := PossiblyMakeCallsignRare (Call);
            END;

        END;

    IF Call = '' THEN Call := CD.GetRandomCall;

    GetRandomDDXCallsign := Call;
    END;



PROCEDURE DDXAnswerACQ (VAR Call: CallString;
                        VAR Tone: INTEGER;
                        StationToBeCalled: CallString);

{ This procedure can be called when you would like the DDX to answer a
  CQ that has just been sent by the LOG user.  If it decides to actually
  answer the CQ, it will set DDXState to AnsweredACQ, otherwise it will
  be left unchanged.  The parameter Call will be set to the callsign.
  This will be null if no call was made.  The parameter Tone is the tone
  the call was made at.  You will need to remember this so you can send
  the exchange at the same tone.                                          }

    BEGIN
    IF (Random (15) > 0) OR (CWTone = 0) THEN
        BEGIN
        Call := GetRandomDDXCallsign (ActiveExchange);

        IF CWTone <> 0 THEN
            BEGIN
            Tone := Random (1000) + 200;

            CASE Random (15) OF
                 1: AddStringToBuffer ('DE ' + Call, Tone);
                 2: AddStringToBuffer (StationToBeCalled + ' DE ' + Call, Tone);
                 3: AddStringToBuffer ('DE ' + Call, Tone);
                 4: AddStringToBuffer ('DE ' + Call, Tone);
                 ELSE AddStringToBuffer (Call, Tone);
                 END;
            END;

        DDXState := CallSentInResponseToACQ;
        RememberDDXCallsign := Call;
        END
    ELSE
        Call := '';
    END;



FUNCTION GetRandomDomesticQTH (Call: CallString): Str80;

VAR TempString: Str80;
    QTH: Str20;
    Number: Char;

    BEGIN
    TempString := NumberPartOfString (Call);
    Number := TempString [1];

    IF (ActiveDomesticMult = DomesticFile) AND
       ((DomQTHTable.ActiveDomQTHFile = 'JAPREF.DOM') OR
        (DomQTHTable.ActiveDomQTHFile = 'JAPREFCT.DOM')) THEN

         IF CountryTable.GetCountryId (CountryTable.GetCountry (Call, True)) = 'JA' THEN
            BEGIN
            CASE Number OF
              '1': CASE Random (8) OF
                       0: QTH := 'TK';
                       1: QTH := 'KN';
                       2: QTH := 'CB';
                       3: QTH := 'IB';
                       4: QTH := 'ST';
                       5: QTH := 'YN';
                       6: QTH := 'TG';
                       7: QTH := 'GM';
                       END;

              '2': CASE Random (4) OF
                       0: QTH := 'AC';
                       1: QTH := 'ME';
                       2: QTH := 'SO';
                       3: QTH := 'GF';
                       END;

              '3': CASE Random (6) OF
                       0: QTH := 'KT';
                       1: QTH := 'OS';
                       2: QTH := 'WK';
                       3: QTH := 'HG';
                       4: QTH := 'SI';
                       5: QTH := 'NR';
                       END;

              '4': CASE Random (6) OF
                       0: QTH := 'OY';
                       1: QTH := 'HS';
                       2: QTH := 'YG';
                       3: QTH := 'SN';
                       4: QTH := 'TT';
                       5: QTH := 'YG';
                       END;

              '5': CASE Random (4) OF
                       0: QTH := 'KC';
                       1: QTH := 'EH';
                       2: QTH := 'KA';
                       3: QTH := 'TS';
                       END;

              '6': CASE Random (8) OF
                       0: QTH := 'KG';
                       1: QTH := 'OT';
                       2: QTH := 'KM';
                       3: QTH := 'MZ';
                       4: QTH := 'FO';
                       5: QTH := 'SG';
                       6: QTH := 'ON';
                       7: QTH := 'NS';
                       END;

              '7': CASE Random (6) OF
                       0: QTH := 'MG';
                       1: QTH := 'AT';
                       2: QTH := 'FS';
                       3: QTH := 'YM';
                       4: QTH := 'IT';
                       5: QTH := 'AM';
                       END;

              '8': CASE Random (14) OF
                       0: QTH := 'AB';
                       1: QTH := 'HD';
                       2: QTH := 'HY';
                       3: QTH := 'IR';
                       4: QTH := 'IS';
                       5: QTH := 'KK';
                       6: QTH := 'KR';
                       7: QTH := 'NM';
                       8: QTH := 'OM';
                       9: QTH := 'RM';
                      10: QTH := 'SB';
                      11: QTH := 'SC';
                      12: QTH := 'SY';
                      13: QTH := 'TC';
                      END;

              '9': CASE Random (3) OF
                       0: QTH := 'FI';
                       1: QTH := 'TY';
                       2: QTH := 'IK';
                       END;

              '0': CASE Random (2) OF
                       0: QTH := 'NN';
                       1: QTH := 'NI';
                       END;
              END;
            END
        ELSE
            BEGIN
            CASE CountryTable.GetContinent (Call) OF
                Africa:       QTH := 'AF';
                Asia:         QTH := 'AS';
                Europe:       QTH := 'EU';
                NorthAmerica: QTH := 'NA';
                SouthAmerica: QTH := 'SA';
                Oceania:      QTH := 'OC';
                END;
            GetRandomDomesticQTH := QTH;
            Exit;
            END;

    IF CountryTable.GetCountryID (CountryTable.GetCountry (Call, True)) = 'KH6' THEN QTH := 'HAW';
    IF CountryTable.GetCountryID (CountryTable.GetCountry (Call, True)) = 'KL' THEN QTH := 'AK';

    IF CountryTable.GetCountryID (CountryTable.GetCountry (Call, True)) = 'VE' THEN
        BEGIN
        CASE Number OF
            '1', '9':
                BEGIN
                CASE Random (3) OF
                     0: QTH := 'NB';
                     1: QTH := 'NS';
                     2: QTH := 'PEI';
                     END;
                IF Copy (Call, 1, 3) = 'VY1' THEN QTH := 'YUK';
                IF Copy (Call, 1, 3) = 'VO1' THEN QTH := 'NF';
                END;

            '2': IF Copy (Call, 1, 3) = 'VY2' THEN
                     QTH := 'LAB'
                 ELSE
                     QTH := 'QUE';

            '3': IF (ActiveExchange =
                     QSONumberPrecedenceCheckDomesticQTHExchange) then
                     CASE Random (4) OF
                             0: QTH := 'ONS';
                             1: QTH := 'ONN';
                             2: QTH := 'ONE';
                             3: QTH := 'GTA';
                             END
                 else
                    QTH := 'ON';

            '4': QTH := 'MB';
            '5': QTH := 'SK';
            '6': QTH := 'AB';
            '7': QTH := 'BC';
            '8': QTH := 'NWT';
            END;
        END;

    IF CountryTable.GetCountryID (CountryTable.GetCountry (Call, True)) = 'KP4' THEN
        QTH := 'PR';

    IF CountryTable.GetCountryID (CountryTable.GetCountry (Call, True)) = 'KP2' THEN
        QTH := 'VI';

    IF CountryTable.GetCountryID (CountryTable.GetCountry (Call, True)) = 'K' THEN
        BEGIN
        IF Random (30) = 0 THEN
            BEGIN
            Str (Random (10), TempString);
            Number := TempString [1];
            END;

        CASE Number OF
            '1': IF (ActiveExchange = QSONumberPrecedenceCheckDomesticQTHExchange) OR
                    (ActiveExchange = ClassDomesticOrDXQTHExchange) THEN
                     BEGIN
                     CASE Random (7) OF
                             0: QTH := 'CT';
                             1: QTH := 'EMA';
                             2: QTH := 'WMA';
                             3: QTH := 'ME';
                             4: QTH := 'NH';
                             5: QTH := 'RI';
                             6: QTH := 'VT';
                             END;
                     END
                 ELSE
                     CASE Random (6) OF
                             0: QTH := 'CT';
                             1: QTH := 'MA';
                             2: QTH := 'ME';
                             3: QTH := 'NH';
                             4: QTH := 'RI';
                             5: QTH := 'VT';
                             END;

            '2': IF (ActiveExchange = QSONumberPrecedenceCheckDomesticQTHExchange) OR
                    (ActiveExchange = ClassDomesticOrDXQTHExchange) THEN
                     BEGIN
                     CASE Random (6) OF
                             0: QTH := 'ENY';
                             1: QTH := 'WNY';
                             2: QTH := 'NLI';
                             3: QTH := 'NNJ';
                             4: QTH := 'SNJ';
                             5: QTH := 'NNY';
                             END;
                     END
                 ELSE
                     CASE Random (2) OF
                         0: QTH := 'NY';
                         1: QTH := 'NJ';
                         END;

            '3': IF (ActiveExchange = QSONumberPrecedenceCheckDomesticQTHExchange) OR
                    (ActiveExchange = ClassDomesticOrDXQTHExchange) THEN
                     BEGIN
                     CASE Random (4) OF
                             0: QTH := 'EPA';
                             1: QTH := 'WPA';
                             2: QTH := 'MD';
                             3: QTH := 'DEL';
                             END;
                     END
                 ELSE
                     CASE Random (4) OF
                               0: QTH := 'MD';
                               1: QTH := 'PA';
                               2: QTH := 'DEL';
                               3: QTH := 'MD';
                               END;

            '4': IF (ActiveExchange = QSONumberPrecedenceCheckDomesticQTHExchange) OR
                    (ActiveExchange = ClassDomesticOrDXQTHExchange) THEN
                     BEGIN
                     CASE Random (10) OF
                            0: QTH := 'ALA';
                            1: QTH := 'NFL';
                            2: QTH := 'GA';
                            3: QTH := 'KY';
                            4: QTH := 'NC';
                            5: QTH := 'SC';
                            6: QTH := 'TN';
                            7: QTH := 'VA';
                            8: QTH := 'SFL';
                            9: QTH := 'WCF';
                          END;
                     END
                 ELSE
                     CASE Random (12) OF
                            0: QTH := 'ALA';
                          1,8: QTH := 'FLA';
                          2,9: QTH := 'GA';
                            3: QTH := 'KY';
                            4: QTH := 'NC';
                            5: QTH := 'SC';
                         6,10: QTH := 'TN';
                         7,11: QTH := 'VA';
                          END;

            '5': IF (ActiveExchange = QSONumberPrecedenceCheckDomesticQTHExchange) OR
                    (ActiveExchange = ClassDomesticOrDXQTHExchange) THEN
                     BEGIN
                     CASE Random (8) OF
                            0: QTH := 'ARK';
                            1: QTH := 'LA';
                            2: QTH := 'MS';
                            3: QTH := 'NM';
                            4: QTH := 'OK';
                            5: QTH := 'NTX';
                            6: QTH := 'STX';
                            7: QTH := 'WTX';
                          END;
                     END
                 ELSE
                     CASE Random (9) OF
                               0: QTH := 'ARK';
                               1: QTH := 'LA';
                               2: QTH := 'MS';
                               3: QTH := 'NM';
                               4: QTH := 'OK';
                         5,6,7,8: QTH := 'TX';
                               END;

            '6': IF (ActiveExchange = QSONumberPrecedenceCheckDomesticQTHExchange) OR
                    (ActiveExchange = ClassDomesticOrDXQTHExchange) THEN
                     BEGIN
                     CASE Random (9) OF
                            0: QTH := 'EBAY';
                            1: QTH := 'LAX';
                            2: QTH := 'SDGO';
                            3: QTH := 'ORG';
                            4: QTH := 'SBAR';
                            5: QTH := 'SCV';
                            6: QTH := 'SF';
                            7: QTH := 'SJV';
                            8: QTH := 'SV';
                          END;
                     END
                 ELSE
                     QTH := 'CA';

            '7': IF (ActiveExchange = QSONumberPrecedenceCheckDomesticQTHExchange) OR
                    (ActiveExchange = ClassDomesticOrDXQTHExchange) THEN
                     BEGIN
                     CASE Random (9) OF
                               0: QTH := 'AZ';
                               1: QTH := 'IDA';
                               2: QTH := 'MT';
                               3: QTH := 'NV';
                               4: QTH := 'ORE';
                               5: QTH := 'UT';
                               6: QTH := 'WWA';
                               7: QTH := 'EWA';
                               8: QTH := 'WYO';
                               END;
                     END
                 ELSE
                     CASE Random (12) OF
                           0,8,9: QTH := 'AZ';
                               1: QTH := 'IDA';
                               2: QTH := 'MT';
                               3: QTH := 'NV';
                               4: QTH := 'ORE';
                               5: QTH := 'UT';
                         6,10,11: QTH := 'WA';
                               7: QTH := 'WYO';
                               END;

            '8': CASE Random (7) OF
                     0,3,4: QTH := 'MI';
                     1,5,6: QTH := 'OH';
                         2: QTH := 'WVA';
                            END;

            '9': CASE Random (5) OF
                     0,3: QTH := 'IN';
                     1,4: QTH := 'IL';
                       2: QTH := 'WI';
                          END;

            '0': CASE Random (15) OF
                       0,8,9: QTH := 'CO';
                        1,10: QTH := 'IA';
                           2: QTH := 'KS';
                     3,11,12: QTH := 'MN';
                     4,13,14: QTH := 'MO';
                           5: QTH := 'NE';
                           6: QTH := 'ND';
                           7: QTH := 'SD';
                              END;

            END;     { of case statement }
        END;

    GetRandomDomesticQTH := QTH;
    END;



FUNCTION GetNextCallFromReadInLog: CallString;

VAR Band: BandType;
    Mode: ModeType;
    FileString: Str80;

    BEGIN
    GetNextCallFromReadInLog := '';

    IF NOT ReadInLogFileOpen THEN
        BEGIN
        IF OpenFileForRead (ReadInLogFileRead, ReadInLogFileName) THEN
            ReadInLogFileOpen := True
        ELSE
            BEGIN
            ReadInLog := False;
            Exit;
            END;
        END;

    WHILE NOT Eof (ReadInLogFileRead) DO
        BEGIN
        REPEAT
            ReadLn (ReadInLogFileRead, FileString);
       {    FileString := UpperCase (FileString);    removed 6.27 }
            Band := GetLogEntryBand (FileString);
            Mode := GetLogEntryMode (FileString);
        UNTIL EOf (ReadInLogFileRead) OR (Band <> NoBand);

        IF (Band <> NoBand) AND (Mode <> NoMode) THEN
            BEGIN
            GetNextCallFromReadInLog := UpperCase (GetLogEntryCall (FileString));
            ReadInLogComputerID      := GetLogEntryComputerID (FileString);
            ReadInLogDateString      := GetLogEntryDateString (FileString);
            ReadinLogTimeString      := GetLogEntryTimeString (FileString);
            ReadInLogExchange        := GetLogEntryExchangeString (FileString);

            IF StringHas (ReadInLogExchange, 'DUPE') THEN
                ReadInLogExchange := PrecedingString (ReadInLogExchange, 'DUPE');

            IF NOT ((ActiveExchange = CheckAndChapterOrQTHExchange) OR
                    (ActiveExchange = ClassDomesticOrDXQTHExchange) OR
                    (ActiveExchange = NameAndDomesticOrDXQTHExchange) OR
                    (ActiveExchange = NameQTHAndPossibleTenTenNumber) OR
                    (ActiveExchange = NameAndPossibleGridSquareExchange) OR
                    (ActiveExchange = QSONumberDomesticOrDXQTHExchange) OR
                    (ActiveExchange = QSONumberDomesticQTHExchange) OR
                    (ActiveExchange = QSONumberNameChapterAndQTHExchange) OR
                    (ActiveExchange = QSONumberNameDomesticOrDXQTHExchange) OR
                    (ActiveExchange = QSONumberPrecedenceCheckDomesticQTHExchange)) THEN
                        ReadInLogRST := RemoveFirstString (ReadInLogExchange);

            IF (Mode <> ReadInLogMode) OR (Band <> ReadInLogBand) THEN
                BEGIN
                ActiveMode := Mode;
                ActiveBand := Band;

                IF ((ActiveRadio = RadioOne) AND (Radio1Type <> NoInterfacedRadio)) OR
                   ((ActiveRadio = RadioTwo) AND (Radio2Type <> NoInterfacedRadio)) THEN
                       DisplayBandMode (ActiveBand, ActiveMode, True)
                   ELSE
                       DisplayBandMode (ActiveBand, ActiveMode, False);

                ReadInLogMode := Mode;
                ReadInLogBand := Band;
                END;

            Exit;
            END;
        END;

    Close (ReadInLogFileRead);
    ReadInLog := False;
    ReadInLogFileOpen := False;
    END;



FUNCTION DDXExchange (SendingStation: CallString;
                      ReceivingStation: CallString): Str80;

VAR CallSent: BOOLEAN;
    CharPointer, Power, QSONumber: INTEGER;
    CheckSection, NameString, QTHString, QSONumberString: Str20;
    TempString: Str40;
    Exchange: Str80;
    Number: CHAR;

    BEGIN
    CASE ActiveExchange OF

        ClassDomesticOrDXQTHExchange:

            { We have the following five cases:
                 1. InitialExchangeEntry is null.
                 2. InitialExchangeEntry is complete exchange with space.
                 3. InitialExchangeEntry is class only.
                 4. InitialExchangeEntry is section only.
                 5. InitialExchangeEntry is class and section.  }

            BEGIN
            Exchange := InitialExchangeEntry (SendingStation);

            { Band aid to fix this is a space is at the start }

            GetRidOfPrecedingSpaces (Exchange);

            { Let's see if we need to add a class to the exchange 1 & 4 }

            IF (Exchange = '') OR (NOT StringIsAllNumbers (Copy (Exchange, 1, 1))) THEN
                BEGIN
                CASE Random (20) OF
                    0,1,2,3: QSONumber := 1;
                    4,5,6:   QSONumber := 2;
                    7,8:     QSONumber := 3;
                    9,10:    QSONumber := 4;
                    11:      QSONumber := 5;
                    12:      QSONumber := 6;
                    13:      QSONumber := 7;
                    14:      QSONumber := 8;
                    15:      QSONumber := 9;
                    16:      QSONumber := 10;
                    17:      QSONumber := 11;
                    18:      QSONumber := 12;
                    19:      QSONumber := 47;
                    END;

                Str (QSONumber, QSONumberString);

                CASE Random (8) OF
                    0, 1, 2: Exchange := QSONumberString + 'A ' + Exchange;
                    3, 4:    Exchange := QSONumberString + 'B ' + Exchange;
                    5:       Exchange := QSONumberString + 'C ' + Exchange;
                    6:       Exchange := QSONumberString + 'D ' + Exchange;
                    7:       Exchange := QSONumberString + 'E ' + Exchange;
                    END;
                END;

            { Do we need to add a section? 1 & 3 }

            IF PostcedingString (Exchange, ' ') = '' THEN
                Exchange := Exchange + GetRandomDomesticQTH (SendingStation);

            DDXExchange := Exchange;
            END;

        QSONumberNameDomesticOrDXQTHExchange:
            BEGIN
            CallSent := False;
            Exchange := ReceivingStation + ' ';

            IF Random (2) = 0 THEN
                BEGIN
                CallSent := True;
                Exchange := Exchange + SendingStation + ' ';
                END;

            IF Random (3) = 0 THEN Exchange := Exchange + 'NR ';

            IF SendingStation = DDXNextExCall THEN
                Exchange := Exchange + DDXNextExchange
            ELSE
                BEGIN
                QSONumber := Random (TotalContacts + 10) + 1;
                Str (QSONumber, QSONumberString);
                NameString := CD.GetName (SendingStation);
                QTHString := GetRandomDomesticQTH (SendingStation);

                Exchange := Exchange + QSONumberString + ' ' + NameString + ' '
                            + QTHString;

                Inc (QSONumber);
                Str (QSONumber, QSONumberString);

                DDXNextExchange := QSONumberString + ' ' + NameString + ' ' + QTHString;
                DDXNextExCall := SendingStation;
                END;

            IF NOT CallSent THEN
                Exchange := Exchange + ' ' + SendingStation;

            DDXExchange := Exchange;
            END;

        QSONumberPrecedenceCheckDomesticQTHExchange:
            BEGIN
            Str (Random (TotalContacts + 10 + (TotalContacts DIV 100)) + 1, Exchange);

            IF Random (4) = 0 THEN
                WHILE Length (Exchange) < 3 DO
                    IF Random (7) = 0 THEN
                        Exchange := 'O' + Exchange
                    ELSE
                        Exchange := '0' + Exchange;

            CASE Random (12) OF
                0: Exchange := Exchange + ' ' + 'Q';
                1: Exchange := Exchange + ' ' + 'A';
                2: Exchange := Exchange + ' ' + 'B';
                3: Exchange := 'NR' + ' ' + Exchange + ' ' + 'Q';
                4: Exchange := 'NR' + ' ' + Exchange + ' ' + 'A';
                5: Exchange := 'NR' + ' ' + Exchange + ' ' + 'B';
                6: Exchange := ReceivingStation + Exchange + ' ' + 'Q';
                7: Exchange := ReceivingStation + Exchange + ' ' + 'A';
                8: Exchange := ReceivingStation + Exchange + ' ' + 'B';
                9: Exchange := ReceivingStation + ' NR' + ' ' + Exchange + ' ' + 'Q';
               10: Exchange := ReceivingStation + ' NR' + ' ' + Exchange + ' ' + 'A';
               11: Exchange := ReceivingStation + ' NR' + ' ' + Exchange + ' ' + 'B';
                END;

            CheckSection := InitialExchangeEntry (SendingStation);

            GetRidOfPrecedingSpaces (CheckSection);

            IF CheckSection = '' THEN
                BEGIN
                Str (Random (40) + 55, TempString);
                CheckSection := TempString + ' ' + GetRandomDomesticQTH (SendingStation);
                END
            ELSE
                WHILE Pos ('  ', CheckSection) > 0 DO
                    Delete (CheckSection, Pos ('  ', CheckSection), 1);

            DDXExchange := Exchange + ' ' + SendingStation + ' ' + CheckSection;
            END;

        RSTAgeExchange:
            BEGIN
            Exchange := InitialExchangeEntry (SendingStation);

            GetRidOfPrecedingSpaces (Exchange);

            IF Exchange = '' THEN
                BEGIN
                Power := Random (60);
                IF (Power > 0) AND (Power < 12) THEN Power := Power + 25;
                Str (Power, Exchange);
                END;
            IF Exchange = '0' THEN Exchange := '00';
            DDXExchange := RandomRST + Exchange;
            END;

        RSTDomesticQTHExchange:
            BEGIN
            Exchange := InitialExchangeEntry (SendingStation);

            GetRidOfPrecedingSpaces (Exchange);

            IF Exchange = '' THEN
                Exchange := GetRandomDomesticQTH (SendingStation);
            DDXExchange := RandomRST + Exchange;
            END;

        RSTPowerExchange:
            BEGIN
            Exchange := InitialExchangeEntry (SendingStation);

            GetRidOfPrecedingSpaces (Exchange);

            IF Exchange = '' THEN
                BEGIN
                Power := Random (12);
                CASE Power OF
                    0: Exchange := '005';
                    1: Exchange := '020';
                    2: Exchange := '100';
                    3: Exchange := '200';
                    4: Exchange := '300';
                    5: Exchange := '400';
                    6: Exchange := '500';
                    7: Exchange := '600';
                    8: Exchange := '800';
                    9: Exchange := '1000';
               10, 11: Exchange := 'KW';
                    END;
                END;

            DDXExchange := RandomRST + Exchange;
            END;

        RSTQSONumberExchange:
            BEGIN
            QSONumber := Random (100) + 1;
            IF Random (5) = 0 THEN QSONumber := QSONumber * 10 + Random (10);
            Str (QSONumber, Exchange);

            IF Random (2) = 0 THEN
                BEGIN
                IF QSONumber < 10  THEN Exchange := '0' + Exchange;
                IF QSONumber < 100 THEN Exchange := '0' + Exchange;
                END;

            IF Random (2) = 0 THEN
                FOR CharPointer := 1 TO Length (Exchange) DO
                    BEGIN
                    IF Exchange [CharPointer] = '1' THEN Exchange [CharPointer] := 'A';
                    IF Exchange [CharPointer] = '9' THEN Exchange [CharPointer] := 'N';
                    IF Exchange [CharPointer] = '0' THEN Exchange [CharPointer] := 'T';
                    END;

            DDXExchange := RandomRST + Exchange;
            END;

        RSTZoneExchange,
        RSTZoneOrSocietyExchange,
        RSTZoneAndPossibleDomesticQTHExchange:
            BEGIN
            IF ActiveZoneMult = JAPrefectures THEN
                BEGIN
                TempString := NumberPartOfString (SendingStation);
                Number := TempString [1];

                CASE Number OF
                    '1': Str (Random (8) + 10, Exchange);
                    '2': Str (Random (4) + 18, Exchange);
                    '3': Str (Random (5) + 22, Exchange);
                    '4': Str (Random (5) + 31, Exchange);
                    '5': Str (Random (4) + 36, Exchange);
                    '6': Str (Random (7) + 40, Exchange);
                    '7': Str (Random (6) + 2,  Exchange);
                    '8': Exchange := '1';
                    '9': Str (Random (3) + 28, Exchange);
                    '0': Str (Random (2) + 8,  Exchange);
                    END;

                IF Length (Exchange) < 2 THEN Exchange := '0' + Exchange;
                END
            ELSE
                Exchange := InitialExchangeEntry (SendingStation);

            GetRidOfPrecedingSpaces (Exchange);

            DDXExchange := RandomRST + Exchange;
            END;

        ELSE
            DDXExchange := '';
        END;
    END;



PROCEDURE DDXSendSAndPExchange (SendingStation: CallString;
                                ReceivingStation:  CallString;
                                Tone: INTEGER);

VAR CorrectDDXCallSent: BOOLEAN;
    Name: Str20;

    BEGIN
    IF CWTone = 0 THEN
        BEGIN
        DDXState := SAndPExchangeSent;
        Exit;
        END;

    CorrectDDXCallSent := CallsignICameBackTo = SendingStation;
    Wait (100);
    Wait (Random (200));

    IF ReceivingStation = DDXCall1 THEN
        CASE Random (7) OF
            0: AddStringToBuffer ('R ', Tone);
            1: AddStringToBuffer ('QSL ', Tone);
            END;

    IF NOT CorrectDDXCallSent THEN
        BEGIN
        IF Random (2) = 0 THEN
            BEGIN
            AddStringToBuffer ('DE ' + SendingStation + ' ', Tone);
            CorrectDDXCallSent := True;
            END;
        END
    ELSE
        BEGIN
        Name := CD.GetName (ReceivingStation);

        IF ReceivingStation = 'N6ZZ' THEN
            IF Random (2) = 0 THEN Name := 'DICK';

        IF Name <> '' THEN
            CASE Random (15) OF
                0: AddStringToBuffer ('HI ' + Name + ' ', Tone);
                1: AddStringToBuffer (ProperSalutation (ReceivingStation) + ' ' + Name + ' ', Tone);
                2: AddStringToBuffer ('FB ' + Name + ' ', Tone);
                END;
        END;

    AddStringToBuffer (DDXExchange (SendingStation, ReceivingStation), Tone);

    IF NOT CorrectDDXCallSent THEN
        AddStringToBuffer (' DE ' + SendingStation, Tone);

    DDXState := SAndPExchangeSent;

    IF QTCsEnabled THEN
        IF Random (5) = 0 THEN
            BEGIN
            AddStringToBuffer (' QTC?', Tone);
            DDXState := SAndPExchangeSentAndAskedForQTC;
            END;
    END;



FUNCTION DDX (Activity: DDXActivity): BOOLEAN;

VAR Tone: INTEGER;

    BEGIN
    DDX := True;
    IF DDXState = Off THEN Exit;

    CASE Activity OF
        MaybeSendANewCall:
            BEGIN
            IF DDXState <> WaitingForCQ THEN Exit;

            IF (CWTone <> 0) THEN
                BEGIN
                REPEAT millisleep UNTIL NOT CWStillBeingSent;
                Wait (100 + Random (200));
                END;

            DDXAnswerACQ (DDXCall1, DDXTone1, MyCall);

            IF DDXState <> CallSentInResponseToACQ THEN
                DDXState := WaitingForCQ;
            END;

        RepeatCallsign:
            BEGIN
            REPEAT millisleep UNTIL NOT CWStillBeingSent;

            IF DDXState = CallSentInResponseToACQ THEN
                CASE Random (2) OF
                     0: AddStringToBuffer (DDXCall1, DDXTone1);
                     1: AddStringToBuffer ('DE ' + DDXCall1, DDXTone1);
                     END;
            END;

        SendExchange:
            BEGIN
            IF CWTone <> 0 THEN
                REPEAT millisleep UNTIL NOT CWStillBeingSent;

            IF DDXState = CallSentInResponseToACQ THEN
                DDXSendSAndPExchange (DDXCall1, MyCall, DDXTone1);
            END;

        VerifyContact:
            IF (ReceivedData.Callsign <> RememberDDXCallsign) AND (RememberDDXCallsign <> '') THEN
                BEGIN
                DDX := False;
                REPEAT millisleep UNTIL NOT CWStillBeingSent;
                Wait (100);
                Wait (Random (300));

                IF RememberDDXCallsign = DDXCall2 THEN
                    Tone := DDXTone2
                ELSE
                    Tone := DDXTone1;

                CASE Random (4) OF
                    0: AddStringToBuffer ('U GOT MY CALL WRONG DE ' + RememberDDXCallsign, Tone);
                    1: AddStringToBuffer ('PSE CORRECT MY CALL DE ' + RememberDDXCallsign, Tone);
                    2: AddStringToBuffer ('NO  MY CALL IS ' + RememberDDXCallsign, Tone);
                    3: AddStringToBuffer (ReceivedData.Callsign + ' is wrong  DE ' + RememberDDXCallsign, Tone);
                  END;
                END;

        NormalContactComplete:
            IF SprintQSYRule THEN
                BEGIN
                REPEAT
                    IF NewKeyPressed THEN
                        IF NewReadKey = EscapeKey THEN
                            BEGIN
                            DDXState := WaitingForCQ;
                            FlushCWBufferAndClearPTT;
                            Exit;
                            END;
                    millisleep;
                UNTIL NOT CWStillBeingSent;

                IF Random (2) = 0 THEN
                    BEGIN
                    Wait (200 + Random (200));

                    WHILE Random (2) = 0 DO
                        BEGIN
                        CASE Random (4) OF
                            0: AddStringToBuffer ('CQ NA ' + DDXCall1, DDXTone1);
                            1: AddStringToBuffer ('NA ' + DDXCall1, DDXTone1);
                            2: AddStringToBuffer ('CQ NA ' + DDXCall1 + ' ' + DDXCall1 + ' NA', DDXTone1);
                            3: AddStringToBuffer ('NA ' + DDXCall1 + ' NA', DDXTone1);
                            END;
                        REPEAT millisleep UNTIL NOT CWStillBeingSent;
                        Wait (1000 + Random (2000));
                        END;
                    END
                ELSE
                    Wait (50 + Random (100));

                REPEAT
                    DDXAnswerACQ (DDXCall2, DDXTone2, DDXCall1);
                UNTIL DDXCall2 <> '';

                CallsignICameBackTo := DDXCall1;

                REPEAT
                    IF NewKeyPressed THEN
                        IF NewReadKey = EscapeKey THEN
                            BEGIN
                            DDXState := WaitingForCQ;
                            FlushCWBufferAndClearPTT;
                            Exit;
                            END;
                    millisleep;
                UNTIL NOT CWStillBeingSent;

                Wait (100 + Random (200));

                DDXSendSAndPExchange (DDXCall1, DDXCall2, DDXTone1);

                CallsignICameBackTo := DDXCall2;

                REPEAT
                    IF NewKeyPressed THEN
                        IF NewReadKey = EscapeKey THEN
                            BEGIN
                            DDXState := WaitingForCQ;
                            FlushCWBufferAndClearPTT;
                            Exit;
                            END;
                    millisleep;
                UNTIL NOT CWStillBeingSent;

                Wait (100 + Random (200));

                DDXSendSAndPExchange (DDXCall2, DDXCall1, DDXTone2);
                DDXState := CQSent;

                REPEAT
                    IF NewKeyPressed THEN
                        IF NewReadKey = EscapeKey THEN
                            BEGIN
                            DDXState := WaitingForCQ;
                            FlushCWBufferAndClearPTT;
                            Exit;
                            END;
                    millisleep;
                UNTIL NOT CWStillBeingSent;

                Wait (100 + Random (200));

                CASE Random (3) OF
                    0: AddStringToBuffer ('TU', DDXTone1);
                    1: AddStringToBuffer ('EE', DDXTone1);
                    2: AddStringToBuffer ('QSL', DDXTone1);
                    END;

                DDXState := WaitingForYourCall;

                REPEAT
                    IF NewKeyPressed THEN
                        IF NewReadKey = EscapeKey THEN
                            BEGIN
                            DDXState := WaitingForCQ;
                            FlushCWBufferAndClearPTT;
                            Exit;
                            END;
                    millisleep;
                UNTIL NOT CWStillBeingSent;

//                WHILE NewKeyPressed DO Key := NewReadKey;
                WHILE NewKeyPressed DO NewReadKey;

                DelayOrKeyPressed (200 + Random (200));
                IF NewKeyPressed THEN Exit;

                REPEAT
                    CASE Random (4) OF
                        0: AddStringToBuffer ('CQ NA ' + DDXCall2, DDXTone2);
                        1: AddStringToBuffer ('NA ' + DDXCall2, DDXTone2);
                        2: AddStringToBuffer ('CQ NA ' + DDXCall2 + ' ' + DDXCall2 + ' NA', DDXTone2);
                        3: AddStringToBuffer ('NA ' + DDXCall2 + ' NA', DDXTone2);
                        END;

                    REPEAT
                        IF NewKeyPressed THEN
                            IF NewReadKey = EscapeKey THEN
                                BEGIN
                                DDXState := WaitingForCQ;
                                FlushCWBufferAndClearPTT;
                                Exit;
                                END;
                        millisleep;
                    UNTIL NOT CWStillBeingSent;

                    DelayOrKeyPressed (2000 + Random (2000));
                    IF NewKeyPressed THEN Exit;
                UNTIL False;
                END
            ELSE
                DDXState := WaitingForCQ;

        MaybeRespondToMyCall:
            IF DDXState = WaitingForYourCall THEN
                BEGIN
                IF CWTone <> 0 THEN
                    BEGIN
                    REPEAT millisleep UNTIL NOT CWStillBeingSent;
                    Wait (100 + Random (200));
                    END;

                DDXSendSAndPExchange (DDXCall2, MyCall, DDXTone2);
                DDXState := WaitingForYourExchange;
                END;

        QSLMyExchange:
            IF DDXState = WaitingForYourExchange THEN
                BEGIN
                IF CWTone <> 0 THEN
                    BEGIN
                    REPEAT millisleep UNTIL NOT CWStillBeingSent;
                    Wait (100 + Random (200));

                    CASE Random (3) OF
                        0: AddStringToBuffer ('TU', DDXTone2);
                        1: AddStringToBuffer ('EE', DDXTone2);
                        2: AddStringToBuffer ('QSL', DDXTone2);
                        END;
                    END;

                DDXState := WaitingForCQ;
                END;

        END;
    END;



    BEGIN
    DDXNextExchange            := '';
    DDXNextExCall              := '';
    Debug                      := False;
    END.
