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

{$mode objfpc}
unit communication;

interface
uses baseunix,unix,termio,ieee1284,sysutils,ctypes;

const buflen = 16384;


type
  ParityType = (NoParity, EvenParity, OddParity);
  buftype = array [0..(buflen-1)] of char;
  keyerportx = class
    function devname: string; virtual; abstract;
    procedure key(on: boolean); virtual; abstract;
    procedure ptt(on: boolean); virtual; abstract;
    function readdit: boolean; virtual; abstract;
    function readdah: boolean; virtual; abstract;
    function footswitch: boolean; virtual; abstract;
    procedure relayhigh(on: boolean); virtual; abstract;
  end;
  nullportx = class(keyerportx)
     public
     constructor create;
     function devname: string; override;
     procedure key(on: boolean); override;
     procedure ptt(on: boolean); override;
     function readdit: boolean; override;
     function readdah: boolean; override;
     function footswitch: boolean; override;
     procedure relayhigh(on: boolean); override; 
     destructor destroy;override;
  end;
  serialportx = class(keyerportx)
    private
       fd: longint;
       pid,pid2: TPid;
       slave: string;
       buf1,buf2: buftype;
       istart,iend: integer;
       dev: string;
       ncport: ansistring;
       inverted: boolean;
       procedure readbuf;
       procedure shellfork;
       procedure ncatforkr;
       procedure ncatforks;
       procedure rigctldforkr;
       procedure rigctldforkn;
    public
    constructor create(devicename: string); 
    destructor destroy;override;
    function charready: boolean;
    function readchar: char;
    procedure putchar(c: char);
    procedure key(on: boolean); override;
    procedure ptt(on: boolean); override;
    function readdit: boolean; override;
    function readdah: boolean; override;
    function footswitch: boolean; override;
    procedure relayhigh(on: boolean); override;
    procedure invert(inv: boolean);
    procedure setparams(baud: longint; bytesize: integer; Parity:
       paritytype; stopbits: integer);
    function devname: string; override;
    procedure refork;
    procedure rcvflush;
  end;

  parallelportx = class(keyerportx)
    private
       pport: Pparport;
       dev: string;
       databyte: byte;
    public   
    constructor create(devicename: string);
    destructor destroy;override;
    procedure key(on: boolean); override;
    procedure ptt(on: boolean); override;
    function readdit: boolean; override;
    function readdah: boolean; override;
    function footswitch: boolean; override;
    function devname: string; override;
    procedure writedata(mask: byte; data: byte);
    procedure relayhigh(on: boolean); override;
  end;

// linked list of ports
  portlinkpointer = ^portlink;
  portlink = record
     port: keyerportx;
     next: portlinkpointer;
  end;

  procedure addport(p: keyerportx);
  function findportx(devicename: string): keyerportx;

  function getpt:cint;cdecl;external;
  function grantpt(fd:cint):cint;cdecl;external;
  function unlockpt(fd:cint):cint;cdecl;external;
  function ptsname_r(fd:cint; buf:Pchar; buflen: size_t):cint;cdecl;external;
  procedure diewithparent;cdecl;external;
  procedure hangupwithparent;cdecl;external;

implementation 
uses trcrt,lowlatency; //trcrt for delay

const EMITTERS = $01;
      CWPIN = $08;
      PTTPIN = $04;
      PPADDLEDIT = $20;
      PPADDLEDAH = $10;
      PFOOTSWITCH = $08;
      PRELAY = $02;

var head: portlinkpointer = nil;
    last,cur: portlinkpointer;

procedure addport(p: keyerportx);
begin
   new(cur);
   cur^.port := p;
   cur^.next := nil;
   if head = nil then
      head := cur
   else
      last^.next := cur;
   last := cur;
end;

function findportx(devicename: string): keyerportx;
begin
   findportx := nil;
   cur := head;
   while cur <> nil do
   begin
      if cur^.port.devname = devicename then
      begin
         findportx := cur^.port;
         exit;
      end;
      cur := cur^.next;
   end;
end;

{$I+}
constructor serialportx.create(devicename: string);
var tempfile: text;
    tios: termios;
    sset: serial_struct;
//    i,flags: integer;
begin
   if pos('PTY',upcase(devicename)) = 1 then
      begin
         fd := getpt;
         grantpt(fd);
         unlockpt(fd);
         assign(tempfile,devicename);
         rewrite(tempfile);
         setlength(slave,200);
         ptsname_r(fd,@slave[1],200);
         setlength(slave,strlen(pchar(@slave[0])));
         write(tempfile,slave);
         close(tempfile);
      end
   else if pos('SHELL',upcase(devicename)) = 1 then
      begin
         fd := getpt;
         grantpt(fd);
         unlockpt(fd);
         setlength(slave,200);
         ptsname_r(fd,@slave[1],200);
         setlength(slave,strlen(pchar(@slave[0])));
         shellfork;
      end
   else if pos('NCAT',upcase(devicename)) = 1 then
      begin
         fd := getpt;
         grantpt(fd);
         unlockpt(fd);
         setlength(slave,200);
         ptsname_r(fd,@slave[1],200);
         setlength(slave,strlen(pchar(@slave[0])));
         ncatforkr;
         ncatforks;
      end
   else if pos('RIGCTLD',upcase(devicename)) = 1 then
      begin
         dev := devicename;
         fd := getpt;
         grantpt(fd);
         unlockpt(fd);
         setlength(slave,200);
         ptsname_r(fd,@slave[1],200);
         setlength(slave,strlen(pchar(@slave[0])));
         rigctldforkr;
         delay(100);
         rigctldforkn;
      end
   else
      begin
         fd := fpopen(devicename,O_RDWR or O_NONBLOCK or O_NOCTTY);
         if (fd >= 0) then //set low latency flag for ftdi chips
         begin
            fpioctl(fd,TIOCGSERIAL,@sset);
            sset.flags := sset.flags or ASYNC_LOW_LATENCY;
            fpioctl(fd,TIOCSSERIAL,@sset);
            fpclose(fd);
            fd := fpopen(devicename,O_RDWR or O_NONBLOCK or O_NOCTTY);
         end;
      end;
   if fd < 0 then
   begin
      clrscr;
      writeln(stderr,'error opening serial port ' + devicename);
      writeln;
      halt;
   end;
   fpfcntl(fd,F_SETFL,O_NONBLOCK); //set to nonblocking once open
   istart := 0;
   iend := 0;
   dev := devicename;
   inverted := false;

   tcgetattr(fd,tios);
   if pos('SHELL',upcase(devicename)) = 1 then
   begin
      tios.c_lflag := tios.c_lflag or ECHO or ECHOE or ECHOK or ECHONL;
      tios.c_oflag := tios.c_oflag or ONLCR;
      tcflush(fd, TCIOFLUSH);
   end
   else 
   begin
//      tios.c_iflag := tios.c_iflag and (not (IGNBRK or BRKINT
//          or PARMRK or ISTRIP or INLCR or IGNCR or ICRNL or IXON ));
//      tios.c_oflag := tios.c_oflag and (not OPOST);
//      tios.c_lflag := tios.c_lflag and (not (ICANON or ECHO or ECHONL
//          or ISIG or IEXTEN));
//      tios.c_cflag := tios.c_cflag and (not (CSIZE or PARENB));
//      tios.c_cflag := tios.c_cflag or CS8;
      cfmakeraw(tios);
      tcsetattr(fd, TCSANOW, tios)
   end;
end;   

procedure serialportx.shellfork;
var n,n0: integer;
    enew: PPchar;
    fdslave: longint;
begin
   pid := fpfork;
   if (pid = 0) then
   begin
      fpsetsid; //become a new session so our pty is the controlling tty
      hangupwithparent;
      fdslave := fpopen(slave,O_RDWR);
      fpclose(0);
      fpclose(1);
      fpclose(2);
      fpdup(fdslave);
      fpdup(fdslave);
      fpdup(fdslave);
      n0 := 0;
      n := 0;
      while envp[n] <> Nil do
      begin
         if (strlcomp(envp[n],'TERM=',5) <> 0 ) and
            (strlcomp(envp[n],'LS_OPTIONS=',11) <> 0 ) and
            (strlcomp(envp[n],'LINES=',6) <> 0 ) then inc(n0);
         inc(n);
      end;
      getmem(enew,(n0+4)*sizeof(Pchar));
      n0 := 0;
      n := 0;
      while envp[n] <> Nil do
      begin
         if (strlcomp(envp[n],'TERM=',5) <> 0 ) and
            (strlcomp(envp[n],'LS_OPTIONS=',11) <> 0 ) and
            (strlcomp(envp[n],'LINES=',6) <> 0 ) then
            begin
               enew[n0] := envp[n];
               inc(n0);
            end;
         inc(n);
      end;
      enew[n0] := 'LINES=13';
      enew[n0+1] := 'LS_OPTIONS=-l -T 0 ';
      enew[n0+2] := 'TERM=glasstty';
      enew[n0+3] := Nil;
      fpExecle ('/bin/bash',['-i'],enew);
   end;
end;

procedure serialportx.ncatforkr;
var fdslave: longint;
    p,temp: ansistring;
    port: PChar;
begin
   temp := dev;
   delete(temp,1,5);
   p := copy(temp,1,pos(';',temp)-1);
   port := PChar(p);
   pid := fpfork;
   if (pid = 0) then
   begin
      diewithparent;
      fdslave := fpopen(slave,O_RDWR);
      fpclose(1);
      fpdup(fdslave);
      fpclose(0);
      fpclose(2);
      fpExeclp('ncat',['-l','-k','--recv-only',port]);
   end;
end;

procedure serialportx.rigctldforkr;
var temp: ansistring;
    port,rigdev,rignumber,baud,civ: PChar;
    colonpos: longint;
begin
   temp := dev;
   delete(temp,1,8);
   colonpos := pos(';',temp);
   port := Pchar('--port=' + copy(temp,1,colonpos-1));
   ncport := copy(temp,1,colonpos-1);
   delete(temp,1,colonpos);
   colonpos := pos(';',temp);
   rigdev := Pchar('--rig-file=' + copy(temp,1,colonpos-1));
   delete(temp,1,colonpos);
   colonpos := pos(';',temp);
   rignumber := Pchar('--model=' + copy(temp,1,colonpos-1));
   delete(temp,1,colonpos);
   colonpos := pos(';',temp);
   baud := Pchar('--serial-speed=' + copy(temp,1,colonpos-1));
   delete(temp,1,colonpos);
   colonpos := pos(';',temp);
   civ := Pchar('--civaddr=' + copy(temp,1,colonpos-1));
   pid := fpfork;
   if (pid = 0) then
   begin
      diewithparent;
      fpclose(0);
      fpclose(1);
      fpclose(2);
      if length(baud) = 3 then
         fpExeclp('rigctld',[port,rigdev,rignumber,civ])
      else
         fpExeclp('rigctld',[port,rigdev,rignumber,baud,civ]);
   end
end;

procedure serialportx.rigctldforkn;
var fdslave: longint;
begin
   pid2 := fpfork;
   if (pid2 = 0) then
   begin
      diewithparent;
      fdslave := fpopen(slave,O_RDWR);
      fpclose(0);
      fpclose(1);
      fpclose(2);
      fpdup(fdslave);
      fpdup(fdslave);
      fpdup(fdslave);
      fpExeclp('ncat',['localhost',Pchar(ncport)]);
   end;
end;

procedure serialportx.ncatforks;
var fdslave: longint;
    h,p,temp: ansistring;
    host,port: PChar;
begin
   temp := dev;
   delete(temp,1,5);
   delete(temp,1,pos(';',temp));
   h := copy(temp,1,pos(':',temp)-1);
   delete(temp,1,pos(':',temp));
   p := temp;
   host := PChar(h);
   port := PChar(p);
   pid2 := fpfork;
   if (pid2 = 0) then
   begin
      diewithparent;
      fdslave := fpopen(slave,O_RDONLY);
      fpclose(0);
      fpdup(fdslave);
      fpclose(1);
      fpclose(2);
      fpExeclp('ncat',['--send-only',host,port]);
   end;
end;

procedure serialportx.refork;
var status:cint;
begin
   if pos('SHELL',upcase(dev)) = 1 then
   begin
      if (pid <> 0) then
      begin
         if (fpwaitpid(pid,status,WNOHANG) = pid) then shellfork;
      end;
   end;
   if pos('NCAT',upcase(dev)) = 1 then
   begin
      if (pid <> 0) then
      begin
         if (fpwaitpid(pid,status,WNOHANG) = pid) then ncatforkr;
      end;
      if (pid2 <> 0) then
      begin
         if (fpwaitpid(pid2,status,WNOHANG) = pid2) then ncatforks;
      end;
   end;
   if pos('RIGCTLD',upcase(dev)) = 1 then
   begin
      if (pid <> 0) then
      begin
         if (fpwaitpid(pid,status,WNOHANG) = pid) then rigctldforkr;
      end;
      if (pid2 <> 0) then
      begin
         if (fpwaitpid(pid2,status,WNOHANG) = pid2) then rigctldforkn;
      end;
   end;
end;

procedure serialportx.rcvflush;
begin
   tcflush(fd,TCIFLUSH);
end;

destructor serialportx.destroy;
begin
   fpClose(fd);
   if (pid <> 0) then fpKill(pid,9);
   if (pid2 <> 0) then fpKill(pid2,9);
end;

procedure serialportx.setparams(baud: longint; bytesize: integer; Parity:
   paritytype; stopbits: integer);
var
  tios: termios;
  baudbits: longint;
begin
//  FillChar(tios, SizeOf(tios), #0);
  tcgetattr(fd,tios);


  baudbits := 0;
  case baud of
    50: baudbits := B50;
    75: baudbits := B75;
    110: baudbits := B110;
    134: baudbits := B134;
    150: baudbits := B150;
    200: baudbits := B200;
    300: baudbits := B300;
    600: baudbits := B600;
    1200: baudbits := B1200;
    1800: baudbits := B1800;
    2400: baudbits := B2400;
    4800: baudbits := B4800;
    19200: baudbits := B19200;
    38400: baudbits := B38400;
    57600: baudbits := B57600;
    115200: baudbits := B115200;
    230400: baudbits := B230400;
    460800: baudbits := B460800;
    else baudbits := B9600;
  end;
  cfsetispeed(tios,baudbits);
  cfsetospeed(tios,baudbits);

  tios.c_cflag := tios.c_cflag or CREAD or CLOCAL;

  tios.c_cflag := tios.c_cflag and (not CSIZE);
  case ByteSize of
    5: tios.c_cflag := tios.c_cflag or CS5;
    6: tios.c_cflag := tios.c_cflag or CS6;
    7: tios.c_cflag := tios.c_cflag or CS7;
    else tios.c_cflag := tios.c_cflag or CS8;
  end;

  tios.c_cflag := tios.c_cflag and (not (PARENB or PARODD));
  case Parity of
    OddParity: tios.c_cflag := tios.c_cflag or PARENB or PARODD;
    EvenParity: tios.c_cflag := tios.c_cflag or PARENB;
  end;

  tios.c_cflag := tios.c_cflag and (not CSTOPB);
  if StopBits = 2 then
    tios.c_cflag := tios.c_cflag or CSTOPB;

//  if RtsCtsFlowControl in Flags then
//    tios.c_cflag := tios.c_cflag or CRTSCTS;

  tcflush(fd, TCIOFLUSH);
  tcsetattr(fd, TCSANOW, tios);
end;

procedure serialportx.invert(inv: boolean);
begin
   inverted := inv;
end;

procedure serialportx.key(on: boolean);
const
  DTR: Cardinal = TIOCM_DTR;
begin
  if on or ((not on) and inverted) then
    fpioctl(fd, TIOCMBIS, @DTR)
  else
    fpioctl(fd, TIOCMBIC, @DTR);
end;

procedure serialportx.ptt(on: boolean);
const
  RTS: Cardinal = TIOCM_RTS;
begin
  if on or ((not on) and inverted) then
    fpioctl(fd, TIOCMBIS, @RTS)
  else
    fpioctl(fd, TIOCMBIC, @RTS);
end;

procedure serialportx.relayhigh(on: boolean); //relay high uses PTT pin
const
  RTS: Cardinal = TIOCM_RTS;
begin
  if on or ((not on) and inverted) then
    fpioctl(fd, TIOCMBIS, @RTS)
  else
    fpioctl(fd, TIOCMBIC, @RTS);
end;

procedure serialportx.readbuf;
var icount,i,maxread: integer;
begin
   if iend = istart then 
      maxread := buflen-1
   else
      maxread := ((buflen - iend + istart) mod buflen) - 1;
   icount := fpread(fd,buf1,maxread);
//   icount := fpread(fd,buf1,buflen);
   if icount <> 0 then
   begin
      for i := 0 to icount-1 do
      begin
         buf2[iend] := buf1[i];
         iend := (iend + 1) mod buflen;
      end;
   end;
end;

function serialportx.charready: boolean;
begin
   self.readbuf;
   charready :=  (istart <> iend);
end;

function serialportx.readchar:char;
begin
   readchar := buf2[istart];
   istart := (istart + 1) mod buflen;
end;

procedure serialportx.putchar(c: char);
begin
   fpwrite(fd,c,1);
end;

function serialportx.devname: string;
begin
   devname := dev;
end;

function serialportx.readdit: boolean;
var flags: Cardinal;
begin
   fpioctl(fd,TIOCMGET,@flags);
   readdit :=  not ((Flags and TIOCM_CTS) <> 0);
end;

function serialportx.readdah: boolean;
var flags: Cardinal;
begin
   fpioctl(fd,TIOCMGET,@flags);
   readdah := not ((Flags and TIOCM_DSR) <>0);
end;

function serialportx.footswitch: boolean;
var flags: Cardinal;
begin
   fpioctl(fd,TIOCMGET,@flags);
   footswitch := ((Flags and TIOCM_CD) <>0);
end;

constructor parallelportx.create(devicename: string);
var i:integer;
    portlist: parport_list;
begin
   if devicename = 'yccc' then
   begin
      dev := devicename; //quick and dirty method to use yccc pins as dvk
      exit;
   end;
   ieee1284_find_ports(@portlist,0);
   if portlist.portc = 0 then
   begin
      writeln(stderr,'no parallel ports found');
      halt;
   end;
   pport := nil;
   for i:=0 to portlist.portc-1 do
   begin
      if portlist.portv^[i].filename = devicename then
         pport := portlist.portv^+i;
   end;
   if pport = nil then
   begin   
      writeln(stderr,'parallel port device not found');
      halt;
   end;
   dev := devicename;
   i := ieee1284_open(pport,0,nil);
   ieee1284_free_ports(@portlist);
   ieee1284_claim(pport);
   databyte := 0;
   ieee1284_write_data(pport,databyte);
   relayhigh(true); //for pullup
end;   

destructor parallelportx.destroy;
begin
end;

procedure parallelportx.key(on: boolean);
var b: byte;
begin
  b := byte(ieee1284_read_control(pport));
  if on then
     begin
        b := (b or CWPIN) and (not EMITTERS);
        ieee1284_write_control(pport,b);
     end
  else
     begin
        if (PTTPIN and b) = 0 then
           b := (b or EMITTERS) and (not CWPIN) //last one turn off emitters
        else
           b := b and (not CWPIN);
        ieee1284_write_control(pport,b);
     end;
end;

procedure parallelportx.ptt(on: boolean);
var b: byte;
begin
  b := byte(ieee1284_read_control(pport));
  if on then
     begin
        b := (b or PTTPIN) and (not EMITTERS);
        ieee1284_write_control(pport,b);
     end
  else
     begin
        if (CWPIN and b) = 0 then
           b := (b or EMITTERS) and (not PTTPIN) //last one turn off emitters
        else
           b := b and (not PTTPIN);
        ieee1284_write_control(pport,b);
     end;
end;

procedure parallelportx.relayhigh(on: boolean);
var b: byte;
begin
  b := byte(ieee1284_read_control(pport));
  if on then //inverted
     begin
        b := b and (not PRELAY);
     end
  else
     begin
        b := b or PRELAY
     end;
  ieee1284_write_control(pport,b);
end;

function parallelportx.readdit: boolean;
var b: byte;
begin
   b := ieee1284_read_status(pport);
   readdit := not ((b and PPADDLEDIT) <> 0);
end;

function parallelportx.readdah: boolean;
var b: byte;
begin
   b := ieee1284_read_status(pport);
   readdah := not ((b and PPADDLEDAH) <> 0);
end;

function parallelportx.footswitch: boolean;
var b: byte;
begin
   b := ieee1284_read_status(pport);
   footswitch := not ((b and PFOOTSWITCH) <> 0);
end;

procedure parallelportx.writedata(mask: byte; data: byte);
begin
   databyte := (databyte and (not mask)) or (data and mask);
   ieee1284_write_data(pport,databyte);
end;

function parallelportx.devname: string;
begin
   devname := dev;
end;

constructor nullportx.create;
begin
end;

destructor nullportx.destroy;
begin
end;

procedure nullportx.key(on: boolean);
begin
end;

procedure nullportx.ptt(on: boolean);
begin
end;

function nullportx.devname: string;
begin
   devname := 'null';
end;

function nullportx.readdit: boolean;
begin
   readdit := false;
end;

function nullportx.readdah: boolean;
begin
   readdah := false;
end;

function nullportx.footswitch: boolean;
begin
   footswitch := false;
end;

procedure nullportx.relayhigh(on: boolean);
begin
end;



end.
