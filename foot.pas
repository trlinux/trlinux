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
