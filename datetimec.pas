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

unit datetimec;
{$L dt}
interface
   procedure gettime(var hour:word ; var min:word; var sec:word;
      var sec100:word);
   procedure getdate(var year:word; var month:word; var day:word;
      var wday:word);
   procedure setdate(year:word; month:word; day:word);
   procedure settime(hour:word; min:word; sec:word; sec100:word);

implementation
   procedure gettimec(hour:pointer; min:pointer; sec:pointer;
      sec100:pointer );cdecl;external;
   procedure getdatec(year:pointer; month:pointer; mday:pointer; wday:pointer);
      cdecl;external;
   procedure setdatec(year:word; month:word; day:word);cdecl;external;
   procedure settimec(hour:word; min:word; sec:word; sec100:word);
      cdecl;external;

   procedure gettime(var hour:word ; var min:word; var sec:word;
      var sec100:word);
   begin
      gettimec(addr(hour),addr(min),addr(sec),addr(sec100));
   end;

   procedure getdate(var year:word; var month:word; var day:word;
      var wday:word);
   begin
      getdatec(addr(year),addr(month),addr(day),addr(wday));
   end;

   procedure setdate(year:word; month:word; day:word);
   begin
      setdatec(year,month,day);
   end;

   procedure settime(hour:word; min:word; sec:word; sec100:word);
   begin
     settimec(hour,min,sec,sec100);
   end;
end.
