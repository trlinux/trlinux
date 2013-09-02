UNIT beep;
{$mode objfpc}

INTERFACE

Type
    BeepType = (Single, BeepCongrats, ThreeHarmonics, PromptBeep, Warning);

    Beeper = class
       Private
          BeepCount:  INTEGER;
          BeepEnable: BOOLEAN;
          BeepFreq:   INTEGER;
          ActiveBeep: BeepType;
       Public
          PROCEDURE DoABeep (TypeOfBeep: BeepType);
          PROCEDURE Timer;
          FUNCTION GetBeepEnable:boolean;
          PROCEDURE SetBeepEnable(on: boolean);
          FUNCTION IsBeeping:boolean;
    end;

IMPLEMENTATION
uses linuxsound;

PROCEDURE Beeper.DoABeep (TypeOfBeep: BeepType);
    BEGIN
    IF NOT BeepEnable THEN
        BEGIN
        LNoSound;
        Exit;
        END;

    ActiveBeep := TypeOfBeep;

    CASE ActiveBeep OF
        ThreeHarmonics:
            BEGIN
            LSound (BeepFreq);
            BeepCount := 75;
            END;

        Warning:
            BEGIN
            LSound (1500);
            BeepCount := 150;
            END;

        Single:
            BEGIN
            LSound (2000);
            BeepCount := 75;
            END;

        PromptBeep:
            BEGIN
            LSound (2000);
            BeepCount := 100;
            END;


        BeepCongrats:
            BEGIN
            LSound (530);
            BeepCount := 240;
            END;
        END;
    END;

FUNCTION Beeper.GetBeepEnable:boolean;
BEGIN
   GetBeepEnable := BeepEnable;
END;

PROCEDURE Beeper.SetBeepEnable(on: boolean);
BEGIN
   BeepEnable := on;
END;

FUNCTION Beeper.IsBeeping:boolean;
BEGIN
   IsBeeping := BeepCount <> 0;
END;

PROCEDURE Beeper.Timer;
BEGIN
    IF BeepCount > 0 THEN
        BEGIN
        Dec (BeepCount);

        IF BeepCount = 0 THEN
            LNoSound
        ELSE
            CASE ActiveBeep OF
                BeepCongrats:
                    BEGIN
                    IF BeepCount = 210 THEN LSound  (660);
                    IF BeepCount = 180 THEN LSound  (790);
                    IF BeepCount = 150 THEN LSound (1060);
                    IF BeepCount =  90 THEN LSound  (790);
                    IF BeepCount =  60 THEN LSound (1060);
                    END;

                ThreeHarmonics:
                    BEGIN
                    IF BeepCount = 50 THEN LSound (2000);
                    IF BeepCount = 25 THEN LSound (4000);
                    END;

                Warning:
                    BEGIN
                    IF (BeepCount = 125) OR
                       (BeepCount =  75) OR
                       (BeepCount =  25) THEN LSound (1000);
                    IF (BeepCount = 100) OR (BeepCount = 50) THEN LSound (1500);
                    END;

                PromptBeep:
                   BEGIN
                   IF (BeepCount = 80) OR (BeepCount = 40) THEN LNoSound;
                   IF (BeepCount = 60) OR (BeepCount = 20) THEN LSound (2000);
                   END;


                END;
        END;
   END;

END.
