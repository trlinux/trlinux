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

unit keyers;
{$mode objfpc}

INTERFACE

USES trcrt,communication,beep,foot,radio;

CONST
    CWBufferSize           = 1024;

TYPE
    SendStatusType = (NothingBeingSent, DitBeingSent, DahBeingSent);
    CurtisMode = (ModeA, ModeB, Ultimatic, ModeNL);

     Keyer = class(TInterfacedObject)
        PROCEDURE AddCharacterToBuffer (Character: CHAR);virtual;abstract;
        PROCEDURE AddStringToBuffer (Msg: String; Tone: INTEGER);virtual;abstract;
        FUNCTION  BufferEmpty: BOOLEAN;virtual;abstract;
        FUNCTION  CWStillBeingSent: BOOLEAN;virtual;abstract;
        FUNCTION  DeleteLastCharacter: BOOLEAN;virtual;abstract;

        PROCEDURE FlushCWBuffer;virtual;abstract;
        PROCEDURE InitializeKeyer;virtual;abstract;
        PROCEDURE PTTForceOn;virtual;abstract;
        PROCEDURE PTTUnForce;virtual;abstract;
        PROCEDURE SetSpeed (Speed: INTEGER);virtual;abstract;
        Function  GetSpeed:INTEGER;virtual;abstract;
        PROCEDURE UnInitializeKeyer;virtual;abstract;
        FUnCTION  PTTAssertedStill: BOOLEAN;virtual;abstract;
        PROCEDURE SetPaddlePttHoldCount(Count: INTEGER);virtual;abstract;
        PROCEDURE SetPaddleBug(On: Boolean);virtual;abstract;
        PROCEDURE SetPaddleMonitorTone(tone: Integer);virtual;abstract;
        PROCEDURE SetPaddleSpeed(speed: Integer);virtual;abstract;
        PROCEDURE LetFootSwitchControlPtt;virtual;abstract;
        PROCEDURE Timer;virtual;abstract;
        Function GetPaddleBug:boolean;virtual;abstract;
        Function GetPaddleMonitorTone:integer;virtual;abstract;
        Function GetPaddleSpeed:integer;virtual;abstract;
        Function GetPaddlePTTHoldCount:integer;virtual;abstract;
        PROCEDURE SetWeight(w: real);virtual;abstract;
        Function GetWeight:real;virtual;abstract;
        PROCEDURE SetFarnsworthEnable(on: boolean);virtual;abstract;
        Function GetFarnsworthEnable:boolean;virtual;abstract;
        PROCEDURE SetFarnsworthSpeed(speed: integer);virtual;abstract;
        Function GetFarnsworthSpeed:integer;virtual;abstract;
        PROCEDURE SetPTTTurnOnDelay(delay: integer);virtual;abstract;
        Function GetPTTTurnOnDelay:integer;virtual;abstract;
        Procedure SetPTTEnable(on: boolean);virtual;abstract;
        Function GetPTTEnable:boolean;virtual;abstract;
        Procedure SetTuneWithDits(on: boolean);virtual;abstract;
        Function GetTuneWithDits:boolean;virtual;abstract;
        Procedure SetMonitorTone(tone: integer);virtual;abstract;
        Function GetMonitorTone:integer;virtual;abstract;
        Procedure SetCountsSinceLastCW(count: integer);virtual;abstract;
        Function GetCountsSinceLastCW:integer;virtual;abstract;
        Procedure SetCurtisMode(m: CurtisMode);virtual;abstract;
        Function GetCurtisMode:CurtisMode;virtual;abstract;
        Function GetKeyerInitialized:boolean;virtual;abstract;
        Procedure SetSwapPaddles(on: boolean);virtual;abstract;
        Function GetSwapPaddles:boolean;virtual;abstract;
        Function GetPTTAsserted:boolean;virtual;abstract;
        Procedure SetPTTFootSwitch(on :boolean);virtual;abstract;
        Procedure SetActiveRadio(r: RadioType);virtual;abstract;

        Procedure SetFootSwitch(f: FootSwitchx);virtual;abstract;
        Procedure SetCwGrant(on: boolean);virtual;abstract;
        Procedure dvpptt(on: boolean);virtual;abstract;
        Procedure debug(on: boolean);virtual;
        protected
          debugoutput: boolean;
          debugfile: textfile;
        END;

implementation
uses sysutils;
  procedure keyer.debug(on: boolean);
  begin
     debugoutput := on;
     if not on then exit;
     assignfile(debugfile,'keyer_debug.out');
     {$I+}
     try
        rewrite(debugfile);
     except
        on E: EInOutError do
        begin
           writeln('Error opening keyer_debug.out. Detail: '
              + E.ClassName + '/' +E.Message);
           halt;
        end;
     end;
  end;

END.
