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
    function  dvpactive: boolean;
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
   playfile(f);
end;

procedure dvprecordmessage (filename: str80; onair: boolean);
begin
end;

procedure dvpplaymessage (filename: str80);
begin
   if not dvpsetup then exit;
{
   if PttEnable then
   begin
      ActiveKeyerPort.ptt(true);
want to turn on ptt then delay 
then start file and flag timer routine to turn off ptt
      dvpptt := true;
   end;
}
   strpcopy(f,filename);
   playfile(f);
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
