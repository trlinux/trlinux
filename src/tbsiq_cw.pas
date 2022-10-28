//
//Copyright Larry Tyree, N6TR, 2011,2012,2013,2014,2015,2022
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

UNIT TBSIQ_CW;

{ Support for the TBSIQ module for sending CW.  Has the object TBSIQ_CWEngine }

{$O+}
{$V-}

INTERFACE

USES Dos, Tree, LogWind, LogDupe, LogStuff, ZoneCont, Country9,
     timer,LogCW, LogDVP, LogDom, Printer, LogK1EA, LogHelp, LogGrid, trCrt,
     keycode,jctrl2,LogPack,LogWAE, LogEdit,LogSCP,datetimec,radio,ctypes,xkb;


CONST
    MaximumCuedMessages = 40;
    SerialNumberFileName = 'SERIALNUMBER.TXT';

TYPE
    TBSIQ_CW_PriorityType = (CWP_Low,
                             CWP_Medium,
                             CWP_High,
                             CWP_Urgent);

    TXColorType = (NoTXColor, TX_Red, TX_Yellow, TX_Blue);

    InsertIndicatorType = (NoInsertIndicator, InsertOffIndicator, InsertOnIndicator);

    CuedMessageType = RECORD
        Message: STRING;
        Radio: RadioType;
        Priority: TBSIQ_CW_PriorityType;
        END;

    SeriaLNumberObject = CLASS

        NextSerialNumber: INTEGER;

        PROCEDURE CreateQSONumberLoggedEntry (SerialNumber: INTEGER; Call: CallString);

        FUNCTION  GetNextSerialNumber (Callsign: CallString;
                                       Frequency: LONGINT;
                                       Band: BandType;
                                       Mode: ModeType): INTEGER;

        PROCEDURE InitializeSerialNumbers;
        END;


    TBSIQ_CWEngineObject = CLASS

        CueHead: INTEGER;
        CueTail: INTEGER;

        LastActiveRadioShown: RadioType;
        MessageCue: ARRAY [0..MaximumCuedMessages - 1] OF CuedMessageType;
        MessageStarted: BOOLEAN;

        PROCEDURE AddcharacterToBuffer (AddChar: CHAR; AddRadio: RadioType);
        PROCEDURE CheckMessages;    { Call often to make the message cue work }
        FUNCTION  ClearMessages (Radio: RadioType; InProcess: BOOLEAN): BOOLEAN;
        PROCEDURE CueCWMessage (Message: STRING; Radio: RadioType; Priority: TBSIQ_CW_PriorityType);
        FUNCTION  CWFinished (Radio: RadioType): BOOLEAN;
        FUNCTION  CWBeingSent (Radio: RadioType): BOOLEAN;
        FUNCTION  DeleteLastCharacter (Radio: RadioType): BOOLEAN;
        FUNCTION  GetTransmitColor (Radio: RadioType): TXColorType;
        FUNCTION  MessageInCue (Radio: RadioType): BOOLEAN;
        PROCEDURE SendNextMessage (PriorityLevel: TBSIQ_CW_PriorityType);
        PROCEDURE ShowActiveRadio;  { Shows which radio has focus for CW sending }
        END;

VAR
    TBSIQ_CW_Engine: TBSIQ_CWEngineObject;
    SerialNumberEngine: SerialNumberObject;


IMPLEMENTATION


FUNCTION SerialNumberObject.GetNextSerialNumber (Callsign: CallString;
                                                 Frequency: LONGINT;
                                                 Band: BandType;
                                                 Mode: ModeType): INTEGER;

VAR FileWrite: TEXT;

    BEGIN
    GetNextSerialNumber := NextSerialNumber;

    IF OpenFileForAppend (FileWrite, SerialNumberFileName) THEN
        BEGIN
        WriteLn (FileWrite, 'QNR: ', NextSerialNumber, ' ',
                                     GetFullDateString, ' ',
                                     GetFullTimeString, ' ',
                                     CallSign, ' ',
                                     Frequency, ' ',
                                     BandString [Band], ' ',
                                     ModeString [Mode]);

        Close (FileWrite);
        END;

    Inc (NextSerialNumber);
    END;



PROCEDURE SerialNumberObject.CreateQSONumberLoggedEntry (SerialNumber: INTEGER; Call: CallString);

VAR FileWrite: TEXT;

    BEGIN
    IF OpenFileForAppend (FileWrite, SerialNumberFilename) THEN
        BEGIN
        WriteLn (FileWrite, 'QLQ: ', SerialNumber, ' ',
                                     GetFullDateString, ' ',
                                     GetFullTimeString, ' ',
                                     Call);

        Close (FileWrite);
        END;
    END;


PROCEDURE SerialNumberObject.InitializeSerialNumbers;

{ Simply reads the serial number file and figures out what the
  next serial number should be.  }

VAR FileRead: TEXT;
    NumberString, DataFlag, FileString: STRING;

    BEGIN
    IF OpenFileForRead (FileRead, SerialNumberFileName) THEN
        BEGIN
        WHILE NOT Eof (FileRead) DO
            BEGIN
            ReadLn (FileRead, FileString);

            { First element indicates what kind of data this is }

            DataFlag := RemoveFirstString (FileString);

            IF DataFlag = 'QNR:' THEN    { Serial Number request data }
                NumberString := RemoveFirstString (FileString);
            END;

        Close (FileRead);

        { NumberString has last entry }

        Val (NumberString, NextSerialNumber);
        Inc (NextSerialNumber);
        END

    ELSE
        NextSerialNumber := 1;
    END;



FUNCTION TBSIQ_CWEngineObject.GetTransmitColor (Radio: RadioType): TXColorType;

    BEGIN
    IF (ActiveMode = CW) AND (ActiveRadio = Radio) AND CWStillBeingSent THEN
        BEGIN
        GetTransmitColor := TX_Red;
        Exit;
        END;

   IF MessageInCue (Radio) THEN
       BEGIN
       GetTransmitColor := TX_Yellow;
       Exit;
       END;

   GetTransmitColor := TX_Blue;
   END;


PROCEDURE TBSIQ_CWEngineObject.ShowActiveRadio;

{ Perhaps this is okay here.  Removes any ambiguity about which radio
  is active }

    BEGIN
    IF ActiveRadio = LastActiveRadioShown THEN Exit;
    LastActiveRadioShown := ActiveRadio;

    CASE ActiveRadio OF
        NoRadio:
            BEGIN
            SaveAndSetActiveWindow (TBSIQ_R1_CodeSpeedWindow);
            GoToXY (9, 1);
            Write ('  ');
            RestorePreviousWindow;

            SaveAndSetActiveWindow (TBSIQ_R2_CodeSpeedWindow);
            GoToXY (9, 1);
            Write ('  ');
            RestorePreviousWindow;
            END;

        RadioOne:
            BEGIN
            SaveAndSetActiveWindow (TBSIQ_R1_CodeSpeedWindow);
            GoToXY (9, 1);
            Write ('TX');
            RestorePreviousWindow;

            SaveAndSetActiveWindow (TBSIQ_R2_CodeSpeedWindow);
            GoToXY (9, 1);
            Write ('  ');
            RestorePreviousWindow;
            END;

        RadioTwo:
            BEGIN
            SaveAndSetActiveWindow (TBSIQ_R1_CodeSpeedWindow);
            GoToXY (9, 1);
            Write ('  ');
            RestorePreviousWindow;

            SaveAndSetActiveWindow (TBSIQ_R2_CodeSpeedWindow);
            GoToXY (9, 1);
            Write ('TX');
            RestorePreviousWindow;
            END;
        END;
    END;



PROCEDURE TBSIQ_CWEngineObject.CueCWMessage (Message: STRING;
                                             Radio: RadioType; Priority:
                                             TBSIQ_CW_PriorityType);

{ Just like SendCWMessage - however it inserts the message into the cue instead of sending it immediately.
  This is first in first out for now.  A message number gives a way to check the status of the mssage }

    BEGIN
    MessageCue [CueHead].Message  := Message;
    MessageCue [CueHead].Radio    := Radio;
    MessageCue [CueHead].Priority := Priority;

    { Point head to next entry to be filled in }

    Inc (CueHead);
    IF CueHead = MaximumCuedMessages THEN CueHead := 0;
    END;



FUNCTION TBSIQ_CWEngineObject.DeleteLastCharacter (Radio: RadioType): BOOLEAN;

{ Attempts to delete the last character sent to the CWCharacterBuffer.  Returns
  TRUE if successful.  Note that this might only work with the SO2R Mini }

    BEGIN
    DeleteLastCharacter := ActiveKeyer.DeleteLastCharacter;
    END;



PROCEDURE TBSIQ_CWEngineObject.AddcharacterToBuffer (AddChar: CHAR; AddRadio: RadioType);

VAR TestTail: INTEGER;

    BEGIN
    IF ActiveRadio = AddRadio THEN  { We can just add the character to the keyer }
        BEGIN
        AddStringToBuffer (AddChar, CWTone);
        Exit;
        END;

    { The message needs to be added to the cued message for this radio }

    IF CueHead = CueTail THEN  { Unexpected this is }
        Exit;

    TestTail := CueTail;

    WHILE TestTail <> CueHead DO
        WITH MessageCue [TestTail] DO
            BEGIN
            IF AddRadio = Radio THEN
                BEGIN
                Message := Message + Addchar;
                Exit;
                END;

            Inc (TestTail);
            IF TestTail = MaximumCuedMessages THEN TestTail := 0;
            END;
    END;



FUNCTION TBSIQ_CWEngineObject.ClearMessages (Radio: RadioType; InProcess: BOOLEAN): BOOLEAN;

{ Well remove any messages in the cue for this radio - and optionally stop sending a message that
  might be in process of being sent.  Will return TRUE if there was something deleted. }

VAR Index: INTEGER;

    BEGIN
    ClearMessages := False;

    IF CueHead <> CueTail THEN      { Something in the cue }
        BEGIN
        Index := CueTail;    { Start at the next message to be popped off }

        IF MessageCue [Index].Radio = Radio THEN
            BEGIN
            MessageCue [Index].Message := '';            { Empty means it will be ignored }
            ClearMessages := True;
            END;
        END;

    { See if a message being sent should be deleted }

    IF InProcess THEN
        IF (ActiveRadio = Radio) AND CWStillBeingSent THEN
            BEGIN
            FlushCWBufferAndClearPTT;
            ClearMessages := True;
            END;
    END;



FUNCTION TBSIQ_CWEngineObject.CWFinished (Radio: RadioType): BOOLEAN;

{ Basically works like NOT CWStillBeingSent, but for a specific radio and will also check the CW
  cue.  Returns TRUE is there is no CW being sent or cued up for the radio. }

VAR Index: INTEGER;

    BEGIN
    IF CueHead <> CueTail THEN      { Something in the cue }
        BEGIN
        Index := CueTail;

        WHILE Index <> CueHead DO
            BEGIN
            IF MessageCue [Index].Radio = Radio THEN
                BEGIN
                CWFinished := False;
                Exit;
                END;

            Inc (Index);
            IF Index = MaximumCuedMessages THEN Index := 0;
            END;
        END;

    { See if a message is currently being sent }

    IF (ActiveRadio = Radio) AND CWStillBeingSent THEN
        BEGIN
        CWFinished := False;
        Exit;
        END;

    CWFinished := True;
    END;



FUNCTION TBSIQ_CWEngineObject.CWBeingSent (Radio: RadioType): BOOLEAN;

{ This is kind of like the above - but it only checks to see if the indicated radio is sending
  a message.  Used to avoid both radios trying to "listen to the other radio" when a message
  is cued. }

    BEGIN
    CWBeingSent := (ActiveRadio = Radio) AND CWStillBeingSent;
    END;



PROCEDURE TBSIQ_CWEngineObject.SendNextMessage (PriorityLevel: TBSIQ_CW_PriorityType);

VAR LowerPriorityMessageFound: BOOLEAN;
    CueIndex: INTEGER;

    BEGIN
    LowerPriorityMessageFound := False;

    { Look through the cue to find a message that matches the priority level }

    IF CueHead = CueTail THEN Exit;  { Just to be sure }

    CueIndex := CueTail;

    WHILE CueHead <> CueIndex DO
        BEGIN
        WITH MessageCue [CueIndex] DO
            IF Message <> '' THEN        { If the message got set to null - it was deleted }
                BEGIN
                IF Priority = PriorityLevel THEN
                    BEGIN
                    IF ActiveRadio <> Radio THEN
                        BEGIN
                        ActiveRadio := Radio;
                        SetUpToSendOnActiveRadio;
                        END;

                    AddStringToBuffer (Message, CWTone);
                    MessageStarted := True;

                    Message := '';    { This essentially deletes the message }

                    IF NOT LowerPriorityMessageFound THEN  { We can delete the entry }
                        BEGIN
                        Inc (CueTail);
                        IF CueTail = MaximumCuedMessages THEN CueTail := 0;
                        END;

                    Exit;  { Get out of here }
                    END
                ELSE
                    BEGIN
                    LowerPriorityMessageFound := True;    { We can't delete this }
                    END;
                END
            ELSE
                IF NOT LowerPriorityMessageFound THEN     { Remove empty message from cue }
                    BEGIN
                    Inc (CueTail);
                    IF CueTail = MaximumCuedMessages THEN CueTail := 0;
                    END;

        Inc (CueIndex);
        IF CueIndex = MaximumCuedMessages THEN CueIndex := 0;
        END;
    END;



FUNCTION TBSIQ_CWEngineObject.MessageInCue (Radio: RadioType): BOOLEAN;

{ Returns TRUE if a message is in the cue for the specified radio }

VAR CueIndex: INTEGER;

    BEGIN
    MessageInCue := False;

    IF CueHead = CueTail THEN Exit;  { No messages in cue }

    CueIndex := CueTail;

    WHILE CueHead <> CueIndex DO
        BEGIN
        IF MessageCue [CueIndex].Radio = Radio THEN
            IF MessageCue [CueIndex].Message <> '' THEN
                BEGIN
                MessageInCue := True;
                Exit;
                END;

        Inc (CueIndex);
        IF CueIndex = MaximumCuedMessages THEN CueIndex := 0;
        END;
    END;



PROCEDURE TBSIQ_CWEngineObject.CheckMessages;

{ This is the "heartbeat" of the CW message cue.  Call this early and
  often so that any cued messages will be sent after the current message
  is complete }

    BEGIN
    IF CWStillBeingSent  THEN Exit;   { Still sending some CW }
    IF CueHead = CueTail THEN Exit;  { Nothing to tend to }

    { Find the next message to send in the cue }

    MessageStarted := False;

    SendNextMessage (CWP_Urgent);
    IF NOT MessageStarted THEN SendNextMessage (CWP_High);
    IF NOT MessageStarted THEN SendNextMessage (CWP_Medium);
    IF NOT MessageStarted THEN SendNextMessage (CWP_Low);

    { If we started a message - stay here until it starts }

    IF MessageStarted THEN REPEAT UNTIL CWStillBeingSent;
    END;



    BEGIN
    TBSIQ_CW_Engine := TBSIQ_CWEngineObject.Create;
    TBSIQ_CW_Engine.CueHead := 0;
    TBSIQ_CW_Engine.CueTail := 0;
    TBSIQ_CW_Engine.LastActiveRadioShown := NoRadio;

    SeriaLNumberEngine := SerialNumberObject.Create;
    SerialNumberEngine.InitializeSerialNumbers;

    ActiveRadio := NoRadio;
    END.

