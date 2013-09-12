unit foot;
{$mode objfpc}

interface

uses communication;

type
   FootSwitchx = class
      protected
         State: boolean;
         DebouncedState: boolean;
         FootswitchRead: boolean;
         LastState: boolean;
         Port: keyerportx;
         Count: integer;
      public
        Constructor create;
        Procedure Timer;virtual;
        Function getState:boolean;virtual;
        Function getDebouncedState:boolean;virtual;
        Procedure setPort(kp: keyerportx);virtual;
        Function portDefined:boolean;virtual;
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
   FootSwitchRead := false;
end;

Function FootSwitchx.getState:boolean;
begin
  getState := State;
end;

Function FootSwitchx.getDebouncedState:boolean;
begin
// return true only the first time footswitch goes from off to on
   getDebouncedState := false;
   if DebouncedState then
   begin
      if FootSwitchRead then exit;
      getDebouncedState := true;
      FootSwitchRead := true;
      exit;
   end
   else
   begin
      FootSwitchRead := false;
   end;
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
