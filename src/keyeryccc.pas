////$Id: keyers.pas,v 1.9 2013/05/29 15:37:45 schmidt Exp $
unit keyeryccc;
{$mode objfpc}

INTERFACE

USES trcrt,communication,beep,foot,radio,keyers,so2r;

TYPE

     YcccKeyer = class(keyer,so2rinterface)
     public
        Constructor create;
        procedure setrcvfocus;
        procedure setxmtfocus;
        procedure setrig1band;
        procedure setrig2band;
        function getrcvfocus:integer;
        function getxmtfocus:integer;
     end;

IMPLEMENTATION

Uses keycode,linuxsound,xkb,sysutils;

Constructor YcccKeyer.create;
begin
end;

procedure YcccKeyer.setrcvfocus;
begin
end;

procedure YcccKeyer.setxmtfocus;
begin
end;

procedure YcccKeyer.setrig1band;
begin
end;

procedure YcccKeyer.setrig2band;
begin
end;

function YcccKeyer.getrcvfocus:integer;
begin
  getrcvfocus := 0;
end;

function YcccKeyer.getxmtfocus:integer;
begin
  getxmtfocus := 0;
end;

end.
