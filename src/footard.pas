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

unit footArd;
{$mode objfpc}

{ A copy of footyccc but specific for the Ardunio }

interface

uses communication,foot,so2r;

type
   FootSwitchArdx = class(FootSwitchx)
      private
         Ard: so2rinterface;

      public
        Constructor create (Ardin: so2rinterface);
        Procedure Timer;override;
        Function portDefined:boolean;override;
   end;

implementation

Const
   DebounceCount = 5;


Procedure FootSwitchArdx.Timer;
begin
   State := Ard.footswitchpressed;
   if (State <> DebouncedState) then
   begin
      inc(Count);
      if Count > DebounceCount then DebouncedState := State;
   end
   else Count := 0;
end;

Constructor FootSwitchArdx.create(Ardin: so2rinterface);
begin
   inherited create;
   Ard := Ardin;
end;

Function FootSwitchArdx.portDefined:boolean;
begin
   portDefined := true; //if Arduino box exists, foot switch signal exists
end;

end.

