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
    MaximumCuedMessages = 20;

TYPE
    CuedMessageStatusType = (MessageCued,
                             MessageBeingSent,
                             MessageNotInCue);

    TBSIQ_CWStateType = (TBSIQCWState_Idle);

    TBSIQ_CW_PriorityType = (CWP_Low,
                             CWP_Medium,
                             CWP_High,
                             CWP_Urgent);

    CuedMessageType = RECORD
        Message: STRING;
        Radio: RadioType;
        Priority: TBSIQ_CW_PriorityType;
        END;

    TBSIQ_CWEngineObject = CLASS

        CueHead: INTEGER;
        CueTail: INTEGER;

        MessageNumberBeingSent: INTEGER;      { A -1 here will indicate that no message is being sent }

        MessageCue: ARRAY [0..MaximumCuedMessages - 1] OF CuedMessageType;

        PROCEDURE AddcharacterToBuffer (AddChar: CHAR; AddRadio: RadioType);
        PROCEDURE CheckMessages;    { Call often to make the message cue work }
        FUNCTION  ClearMessages (Radio: RadioType; InProcess: BOOLEAN): BOOLEAN;
        PROCEDURE CueCWMessage (Message: STRING; Radio: RadioType; Priority: TBSIQ_CW_PriorityType; VAR MessageNumber: INTEGER);
        FUNCTION  CuedMessageStatus (MessageNumber: INTEGER): CuedMessageStatusType;
        FUNCTION  CWFinished (Radio: RadioType): BOOLEAN;
        FUNCTION  CWBeingSent (Radio: RadioType): BOOLEAN;
        FUNCTION  DeleteLastCharacter (Radio: RadioType): BOOLEAN;
        PROCEDURE ShowActiveRadio;  { Shows which radio has focus for CW sending }
        END;

VAR
    TBSIQ_CW_Engine: TBSIQ_CWEngineObject;


IMPLEMENTATION

PROCEDURE TBSIQ_CWEngineObject.ShowActiveRadio;

    BEGIN
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
                                             TBSIQ_CW_PriorityType;
                                             VAR MessageNumber: INTEGER);

{ Just like SendCWMessage - however it inserts the message into the cue instead of sending it immediately.
  This is first in first out for now.  A message number gives a way to check the status of the mssage }

    BEGIN
    MessageCue [CueHead].Message  := Message;
    MessageCue [CueHead].Radio    := Radio;
    MessageCue [CueHead].Priority := Priority;
    MessageNumber := CueHead;

    { Point head to next entry to be filled in }

    Inc (CueHead);
    IF CueHead = MaximumCuedMessages THEN CueHead := 0;
    END;



FUNCTION TBSIQ_CWEngineObject.CuedMessageStatus (MessageNumber: INTEGER): CuedMessageStatusType;

VAR Index: INTEGER;

    BEGIN
    { Need to see if the MessageNumber, which is an index into the Cue - is in the cue.  Sorry for the
      crude way of determing this.

      I moved this first since it doesn't rely on any interaction with the
      Arduino and thus should run pretty fast }

    IF CueHead <> CueTail THEN  { Something in the cue }
        BEGIN
        Index := CueTail;    { Start at the beginning of the cue }

        REPEAT
            IF Index = MessageNumber THEN
                BEGIN
                CuedMessageStatus := MessageCued;
                Exit;
                END;

            Inc (Index);
            IF Index = MaximumCuedMessages THEN Index := 0;
        UNTIL Index = CueHead;
        END;

    { See if this is the message being sent }

    IF MessageNumber = MessageNumberBeingSent THEN
        BEGIN
        { If the message is currently being sent - see if CW is still active }

        IF NOT CWStillBeingSent THEN
            BEGIN
            CuedMessageStatus := MessageNotInCue;
            MessageNumberBeingSent := -1;
            Exit;
            END;

        CuedMessageStatus := MessageBeingSent;
        Exit;
        END;

    { Either the cue is empty - or the MessageNumber is no longer in the cue }

    CuedMessageStatus := MessageNotInCue;
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
        BEGIN
        Exit;
        END;

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

    { This is also somewhat unexpected too - since I was sure a message was being sent }
    END;



FUNCTION TBSIQ_CWEngineObject.ClearMessages (Radio: RadioType; InProcess: BOOLEAN): BOOLEAN;

{ Well remove any messages in the cue for this radio - and optionally stop sending a message that
  might be in process of being sent.  Will return TRUE if there was something deleted. }

VAR Index: INTEGER;

    BEGIN
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
            MessageNumberBeingSent := -1;
            ClearMessages := True;
            END;

    ClearMessages := False;
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



PROCEDURE TBSIQ_CWEngineObject.CheckMessages;

{ This is the "heartbeat" of the CW message cue.  Call this early and often so that any cued
  messages will be sent after the current message is complete }

VAR MessageStarted: BOOLEAN;

    BEGIN
    IF (MessageNumberBeingSent = -1) AND (CueHead = CueTail) THEN Exit;  { Nothing to tend to }

    { We are sending some CW - let's see if we are done with it }

    IF CWStillBeingSent THEN Exit;

    MessageNumberBeingSent := -1;  { Clear the status of what message is being sent }

    { If nothing else in the buffer, we are done }

    IF CueHead = CueTail THEN Exit;

    { Start sending the next message }

    MessageStarted := False;

    WITH MessageCue [CueTail] DO
        BEGIN
        IF Message <> '' THEN        { If the message got set to null - it was deleted }
            BEGIN
            IF ActiveRadio <> Radio THEN
                BEGIN
                ActiveRadio := Radio;
                SetUpToSendOnActiveRadio;
                END;

            AddStringToBuffer (Message, CWTone);
            MessageNumberBeingSent := CueTail;
            MessageStarted := True;
            END;

        { Take this message off the cue }

        Inc (CueTail);
        IF CueTail = MaximumCuedMessages THEN CueTail := 0;
        END;

    IF MessageStarted THEN REPEAT UNTIL CWStillBeingSent;
    END;



    BEGIN
    TBSIQ_CW_Engine := TBSIQ_CWEngineObject.Create;
    TBSIQ_CW_Engine.CueHead := 0;
    TBSIQ_CW_Engine.CueTail := 0;
    ActiveRadio := NoRadio;
    END.

