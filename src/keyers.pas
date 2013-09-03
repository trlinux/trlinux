unit keyers;
{$mode objfpc}

INTERFACE

USES trcrt,communication,beep,foot,radio;

CONST
    CWBufferSize           = 1024;

TYPE
    SendStatusType = (NothingBeingSent, DitBeingSent, DahBeingSent);
    CurtisMode = (ModeA, ModeB, Ultimatic);
 
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
        Function GetSpeed:INTEGER;virtual;abstract;
        PROCEDURE UnInitializeKeyer;virtual;abstract;
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
        END;

implementation
END.
