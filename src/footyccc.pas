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
   inherited create;
   yccc := ycccin;
end;

Function FootSwitchYcccx.portDefined:boolean;
begin
   portDefined := true; //if yccc box exists, foot switch signal exists
end;

end.
