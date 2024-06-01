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

unit logdvp;
{$mode objfpc}

interface

uses tree;


var
    backcopyenable: boolean;
    cfgdvppath:     str40;
    dvppath:        str80;

    procedure startbackcopy;
    procedure stopbackcopy;
    procedure savebackcopy (qsonumber: integer; seconds: integer);
    procedure savebackcopyfile (filename: str40; seconds: integer);
    procedure dvplistenmessage (filename: str80; usedvppath: boolean);
    procedure dvprecordmessage (filename: str80; onair: boolean);
    procedure dvpplaymessage (filename: str80);
    procedure dvpinit;
    procedure dvpuninit;
    procedure dvpstopplayback;
    function  dvpmessageplaying: boolean;

implementation

uses logk1ea,linuxsound,sysutils;

var
    f: pchar;

procedure startbackcopy;
begin
end;

procedure stopbackcopy;
begin
end;

procedure savebackcopy (qsonumber: integer; seconds: integer);
begin
end;

procedure savebackcopyfile (filename: str40; seconds: integer);
begin
end;

function  dvpactive: boolean;
begin
   dvpactive := true;
end;

procedure dvplistenmessage (filename: str80; usedvppath: boolean);
begin
   if not dvpsetup then exit;
   strpcopy(f,filename);
   playfile(f,0);
end;

procedure dvprecordmessage (filename: str80; onair: boolean);
begin
end;

procedure dvpplaymessage (filename: str80);
var delay: longint;
begin
   if not dvpsetup then exit;
   ActiveKeyer.flushcwbuffer;
   delay := 0;
   if ActiveKeyer.GetPttEnable then
   begin
      ActiveKeyer.dvpptt(true);
      delay := round(real(ActiveKeyer.getpttturnondelay())*1.7);
   end;
   strpcopy(f,filename);
   playfile(f,delay);
end;

procedure dvpinit;
begin
//writeln(stderr,'dvpinit');
   if not BeepSoundCardEnable then beginsound;
   f := stralloc(81);
   soundmode(1);
   dvpon := true;
   dvpsetup := true;
//writeln(stderr,'dvpinit done');
//flush(stderr);
end;

procedure dvpuninit;
begin
   if not dvpsetup then exit;
   if not BeepSoundCardEnable then endsound;
   strdispose(f);
   soundmode(0);
   dvpon := false;
   dvpsetup := false;
//writeln(stderr,'dvpuninit done');
//flush(stderr);
end;

procedure dvpstopplayback;
begin
   if not dvpsetup then exit;
   if dvpmessageplaying then soundmode(2);
   activekeyer.dvpptt(false);
end;

function  dvpmessageplaying: boolean;
begin
   if not dvpsetup then
   begin
      dvpmessageplaying := false;
      exit;
   end;
   dvpmessageplaying := playingfile;
end;

end.
