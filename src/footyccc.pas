unit footyccc;
{$mode objfpc}

interface

uses communication,foot,so2r;

type
   FootSwitchYcccx = class(FootSwitchx)
      private
         yccc: so2rinterface;

      public
        Constructor create(ycccin: so2rinterface);
        Procedure Timer;override;
        Function portDefined:boolean;override;
   end;

implementation

Const
   DebounceCount = 5;


Procedure FootSwitchYcccx.Timer;
begin
   State := yccc.footswitchpressed;
   if (State <> DebouncedState) then
   begin
      inc(Count); 
      if Count > DebounceCount then DebouncedState := State;
   end
   else Count := 0;
end;

Constructor FootSwitchYcccx.create(ycccin: so2rinterface);
begin
   State := false;
   DebouncedState := false;
   LastState := false;
   Port := nil;
   Count := 0;
   yccc := ycccin;
end;

Function FootSwitchYcccx.portDefined:boolean;
begin
   portDefined := true; //if yccc box exists, foot switch signal exists
end;

end.
