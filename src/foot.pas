unit foot;
{$mode objfpc}

interface

uses communication;

type
   FootSwitchx = class
      private
         State: boolean;
         DebouncedState: boolean;
         LastState: boolean;
         Port: keyerportx;
         Count: integer;
      public
        Constructor create;
        Procedure Timer;
        Function getState:boolean;
        Function getDebouncedState:boolean;
        Procedure setPort(kp: keyerportx);
        Function portDefined:boolean;
   end;

   FootSwitchModeType = (CWGrant,
                         FootSwitchDisabled,
                         FootSwitchF1,
                         FootSwitchLastCQFreq,
                         FootSwitchNextBandMap,
                         FootSwitchNextDisplayedBandMap,
                         FootSwitchNextMultBandMap,
                         FootSwitchNextMultDisplayedBandMap,
                         FootSwitchDupeCheck,
                         Normal,
                         QSONormal,
                         QSOQuick,
                         FootSwitchControlEnter,
                         StartSending,
                         SwapRadio);

implementation

Const
   DebounceCount = 5;


Procedure FootSwitchx.Timer;
begin
   if Port = nil then exit;
   State := Port.footswitch;
   if (State <> DebouncedState) then
   begin
      inc(Count); 
      if Count > DebounceCount then DebouncedState := State;
   end
   else Count := 0;
end;

Constructor FootSwitchx.create;
begin
   State := false;
   DebouncedState := false;
   LastState := false;
   Port := nil;
   Count := 0;
end;

Function FootSwitchx.getState:boolean;
begin
  getState := State;
end;

Function FootSwitchx.getDebouncedState:boolean;
begin
   getDebouncedState := DebouncedState;
end;

Procedure FootSwitchx.setPort(kp: keyerportx);
begin
   Port := kp;
end;

Function FootSwitchx.portDefined:boolean;
begin
   portDefined := Port <> nil;
end;

end.
