unit keyeryccc;
{$mode objfpc}

INTERFACE

USES trcrt,communication,beep,foot,radio,keyers,so2r,ycccprotocol;

CONST
    CWBufferSize = 1024;
    ResponseBufferSize = 1024;
    MAX_STR = 255;
    DATA_SIZE = 3;

TYPE
     YcccKeyerBuffer = Array [0..CWBufferSize-1] of Char;
     YcccResponse_t = record
        cmd: byte;
        val: byte;
     end;

     YcccKeyer = class(keyer,so2rinterface)
     private
        hiddev: Pointer;
        keyer_config: keyer_config_t;
        keyer_control: keyer_control_t;
        so2r_state: so2r_state_t;
        so2r_config: so2r_config_t;
        so2r_switches: so2r_switches_t;
        hidbytes: Array [0..2] of byte;
        nbytes: integer;

        snddata: array[0..DATA_SIZE] of byte;
        rcvdata: array[0..DATA_SIZE] of byte;
        responsebuffer: array [0..ResponseBufferSize-1] of YcccResponse_t;
        responsebufferstart: integer;
        responsebufferend: integer;
        idle: boolean;
        
        CodeSpeed:        INTEGER;
        Delta: integer;
        curtmode: CurtisMode;
        KeyerInitialized: BOOLEAN;
        PaddleBug:               BOOLEAN;
        PaddleSpeed:            INTEGER;
        SwapPaddles:              BOOLEAN;

        CountsSinceLastCW: LONGINT;
        PTTEnable:              BOOLEAN;
        PTTTurnOnDelay:         INTEGER;
        PaddlePTTHold: integer;
        PaddlePTTHoldCount: integer;
        NormalPTTHold: integer;
        TuningWithDits:           BOOLEAN;
        Tuning:             BOOLEAN;
        TuneWithDits:       BOOLEAN;
        Weight:             Integer;
        CWBuffer: YcccKeyerBuffer;
        CWBufferStart:         INTEGER;
        CWBufferEnd:           INTEGER;
        PTTAsserted:    BOOLEAN; { Indicates that the PTT signal is asserted.
                               Comes on PTTTurnOnDelay before CW starts.  }
        PTTFootSwitch:  BOOLEAN; { Indicates that the footswitch wants the PTT
                               to be on. }
        PTTForcedon:  boolean;
        DoingPaddle:  BOOLEAN;
        LastFootSwitchStatus: BOOLEAN;
        FsCwGrant: Boolean;
        Footsw: FootSwitchx;
        mirror: byte;
        blend: integer;
        map1: integer;
        map2: integer;
        procedure sendcmd(cmd: integer; val: integer);
        procedure flushLocal;
        procedure readresponses;
        
    public

        Constructor create;
        PROCEDURE AddCharacterToBuffer (Character: CHAR);override;
        PROCEDURE AddStringToBuffer (Msg: String; Tone: INTEGER);override;
        FUNCTION  BufferEmpty: BOOLEAN;override;
        FUNCTION  CWStillBeingSent: BOOLEAN;override;
        FUNCTION  DeleteLastCharacter: BOOLEAN;override;

        PROCEDURE FlushCWBuffer;override;
        PROCEDURE InitializeKeyer;override;
        PROCEDURE PTTForceOn;override;
        PROCEDURE PTTUnForce;override;
        PROCEDURE SetSpeed (Speed: INTEGER);override;
        Function GetSpeed:INTEGER;override;
        PROCEDURE UnInitializeKeyer;override;
        PROCEDURE SetPaddlePttHoldCount(Count: INTEGER);override;
        PROCEDURE SetPaddleBug(On: Boolean);override;
        PROCEDURE SetPaddleMonitorTone(tone: Integer);override;
        PROCEDURE SetPaddleSpeed(speed: Integer);override;
        PROCEDURE LetFootSwitchControlPtt;override;
        PROCEDURE Timer;override;
        Function GetPaddleBug:boolean;override;
        Function GetPaddleMonitorTone:integer;override;
        Function GetPaddleSpeed:integer;override;
        Function GetPaddlePTTHoldCount:integer;override;
        PROCEDURE SetWeight(w: real);override;
        Function GetWeight:real;override;
        PROCEDURE SetFarnsworthEnable(on: boolean);override;
        Function GetFarnsworthEnable:boolean;override;
        PROCEDURE SetFarnsworthSpeed(speed: integer);override;
        Function GetFarnsworthSpeed:integer;override;
        PROCEDURE SetPTTTurnOnDelay(delay: integer);override;
        Function GetPTTTurnOnDelay:integer;override;
        Procedure SetPTTEnable(on: boolean);override;
        Function GetPTTEnable:boolean;override;
        Procedure SetTuneWithDits(on: boolean);override;
        Function GetTuneWithDits:boolean;override;
        Procedure SetMonitorTone(tone: integer);override;
        Function GetMonitorTone:integer;override;
        Procedure SetCountsSinceLastCW(count: integer);override;
        Function GetCountsSinceLastCW:integer;override;
        Procedure SetCurtisMode(m: CurtisMode);override;
        Function GetCurtisMode:CurtisMode;override;
        Function GetKeyerInitialized:boolean;override;
        Procedure SetSwapPaddles(on: boolean);override;
        Function GetSwapPaddles:boolean;override;
        Function GetPTTAsserted:boolean;override;
        Procedure SetPTTFootSwitch(on :boolean);override;
        Procedure SetActiveRadio(r: RadioType);override;
        Procedure SetFootSwitch(f: FootSwitchx);override;
        Procedure SetCwGrant(on: boolean);override;

        Procedure SetPort(port: serialportx);

        procedure setrig1band(band: integer);
        procedure setrig2band(band: integer);
        procedure setheadphonemode(hmode: hmode_t);
        procedure setblend(on: boolean);
        procedure blendvalue(val: integer);
        procedure setmicrelay(on: boolean);
        procedure setrig1map(val: integer);
        procedure setrig2map(val: integer);
        procedure setrcvfocus(rcvfocus: rcvfocus_t);
        function footswitchpressed:boolean;
        function getheadphonemode:hmode_t;
        function getblend:boolean;
        function getblendvalue:integer;
        function getmicrelay:boolean;
        function getrig1map:integer;
        function getrig2map:integer;

     end;

IMPLEMENTATION

Uses keycode,linuxsound,xkb,sysutils,cwstring,hidp;


procedure YcccKeyer.setrig1band(band: integer);
var aux_info: aux_info_t;
    i: integer;
begin
   if not KeyerInitialized then exit;
   aux_info.val := 0;
   aux_info.update := 1;
   aux_info.aux:= band;
   i := aux_info.val;
   sendcmd(CMD_AUX_PORT1,i);
end;


procedure YcccKeyer.setrig2band(band: integer);
var aux_info: aux_info_t;
    i: integer;
begin
   if not KeyerInitialized then exit;
   aux_info.val := 0;
   aux_info.update := 1;
   aux_info.aux:= band;
   i := aux_info.val;
   sendcmd(CMD_AUX_PORT2,i);
end;


Constructor YcccKeyer.create;
begin
   LastFootSwitchStatus := false;
   CountsSinceLastCW := 0;
   CodeSpeed := 35;
   Delta := 0;
   idle := true;
   Doingpaddle := false;
   PaddleBug := false;
   KeyerInitialized := False;
   Tuning := False;
   Weight := 128;
   PTTFootSwitch := False;
   TuningWithDits := False;
   TuneWithDits := False;
   FsCwGrant := False;
   CurtMode := ModeB;
   PTTEnable := false;
   PTTTurnOnDelay := 0;
   NormalPTTHold := 15; //ms
   PaddlePTTHold := 13; //dit counts
   PaddlePTTHoldCount := -1;
   swappaddles := false;
   CurtMode := ModeB;
   keyer_config.val := 0; //zero all bits
   keyer_control.val := 0;
   keyer_control.pot_off := 1;
   so2r_state.val := 0;
   so2r_state.stereo := 1;
   so2r_config.val:= 0;
   so2r_config.relays := 1;
   so2r_switches.val := 0;
   blend := 0;
   nbytes := 0;
   map1 := 0; //keep eeprom default
   map2 := 0; //keep eeprom default
   PTTForcedon := false;
end;

Procedure YcccKeyer.InitializeKeyer;
var 
   tempstring: ucs4string;
   rc: longint;
   ifirm1,ifirm2: integer;
   map: so2r_map_t;
begin
   setlength(tempstring,MAX_STR);
   rc := hid_init;
   if rc <> 0 then
   begin
      clrscr;
      writeln('Human interface device routines not initializing');
      writeln('These are needed for the YCCC SO2R box');
      writeln('return code ',rc);
      halt;
   end;
   hiddev := hid_open(VENDOR_ID,PRODUCT_ID,Pointer(0));
   if hiddev = nil then
   begin
      clrscr;
      writeln('No YCCC SO2R box found.');
      writeln('Check cables and make sure the YCCC SO2R box is powered up.');
      writeln('Check that you have set up udev rules so that you have');
      writeln('read/write permission for the YCCC SO2R box usb device.');
      writeln('See the documentation file trloglinux.pdf for details.');
      halt;
   end;

   rc := hid_get_manufacturer_string(hiddev,@tempstring[0],MAX_STR);
//writeln(stderr,' return code ',rc);
//writeln(stderr,' manufacturer = ',UCS4StringToUnicodeString(tempstring));
   rc := hid_get_product_string(hiddev,@tempstring[0],MAX_STR);
//writeln(stderr,' return code ',rc);
//writeln(stderr,' product = ',UCS4StringToUnicodeString(tempstring));
//rc := hid_get_serial_number_string(hiddev,@tempstring[0],MAX_STR);
//writeln(stderr,' return code ',rc);
//writeln(stderr,' serial = ',UCS4StringToUnicodeString(tempstring));
tempstring := nil;

// get firmware version -- this needs to be done more intelligently
// in case the box sends unsolicited stuff.
//snddata[0] := $00;
//snddata[1] := CMD_QUERY;
//snddata[2] := CMD_QUERY;
//rc := hid_write(hiddev,@snddata[0],DATA_SIZE);
//writeln(stderr,'return code ',rc);
//rc := hid_read(hiddev,@rcvdata[0],DATA_SIZE);
//ifirm1 := rcvdata[2] and $0f;
//ifirm2 := (rcvdata[2] shr 4) and $0f;
//writeln(stderr,'Firmware version  = ',ifirm2,'.',ifirm1);

   rc := hid_set_nonblocking(hiddev,1);
//writeln(stderr,'set_nonblocking return code ',rc);

// clear out any garbage sent by the so2r box before we connected
   while (hid_read(hiddev,@rcvdata[0],3) > 0) do continue;
   
   sendcmd(CMD_KEYER_CONFIG,keyer_config.val);
   sendcmd(CMD_KEYER_CONTROL,keyer_control.val);
   sendcmd(CMD_SO2R_STATE,so2r_state.val);
   sendcmd(CMD_SO2R_CONFIG,so2r_config.val);
   sendcmd(CMD_KEYER_PTT_PRE,PTTTurnOnDelay);
   sendcmd(CMD_SO2R_BLEND,blend);
   sendcmd(CMD_KEYER_SPEED,codespeed);
   sendcmd(CMD_KEYER_PTT_POST,NormalPTTHold);
   sendcmd(CMD_KEYER_WEIGHT,weight);
   if map1 <> 0 then
   begin
      map.val := 0;
      if (map1 < 0) then 
      begin
         map.radio := -map1-1;
         map.eeprom := 1;
         map.current:= 1;
      end
      else
      begin
         map.radio := map1-1;
         map.eeprom := 0;
         map.current:= 1;
      end;
      sendcmd(CMD_SO2R_MAP1,map.val);
   end;
   if map2 <> 0 then
   begin
      map.val := 0;
      if (map2 < 0) then 
      begin
         map.radio := -map2-1;
         map.eeprom := 1;
         map.current:= 1;
      end
      else
      begin
         map.radio := map2-1;
         map.eeprom := 0;
         map.current:= 1;
      end;
      sendcmd(CMD_SO2R_MAP2,map.val);
   end;

   CWBufferStart := 0;
   CWBufferEnd := 0;
   mirror := 0;
   responsebufferstart := 0;
   responsebufferend := 0;
   PTTAsserted := False;
   KeyerInitialized := True;
end;


PROCEDURE YcccKeyer.SetWeight(w: real);
begin
   weight := round(128.0*w);
   if weight < 64 then weight := 64;
   if weight > 192 then weight := 192;
   if KeyerInitialized then 
   begin
      sendcmd(CMD_KEYER_WEIGHT,weight);
   end;
end;

Function YcccKeyer.GetWeight:real;
begin
   GetWeight := real(weight)/(128.0);
end;

Procedure YcccKeyer.SetPTTTurnOnDelay(delay: integer);
begin
   PTTTurnOnDelay := round(real(delay)*1.7);
   if PTTTurnOnDelay > 255 then PTTTurnOnDelay := 255;
   if KeyerInitialized then 
   begin
      sendcmd(CMD_KEYER_PTT_PRE,PTTTurnOnDelay);
   end;
end;

Function YcccKeyer.GetPTTTurnOnDelay:integer;
begin
   GetPTTTurnOnDelay := Round(real(PTTTurnOnDelay)/1.7);
end;

Procedure YcccKeyer.SetActiveRadio(r: RadioType);
begin
   case r of
      RadioOne:
      begin
         so2r_state.tx2 := 0;
      end;

      RadioTwo:
      begin
         so2r_state.tx2 := 1;
      end;
   end;
   if KeyerInitialized then 
   begin
      flushcwbuffer; //probably not necessary
      sendcmd(CMD_SO2R_STATE,so2r_state.val);
   end;
end;

Procedure YcccKeyer.SetPaddlePttHoldCount(Count: INTEGER);
begin
   PaddlePTTHold := count;
end;

Function YcccKeyer.GetPaddlePttHoldCount:integer;
begin
   GetPaddlePttHoldCount := PaddlePTTHold;
end;

Procedure YcccKeyer.PTTForceOn;
begin
   PTTforcedon := true;
end;

Procedure YcccKeyer.PTTUnForce;
begin
   PTTforcedon := false;
end;

procedure YcccKeyer.SetSpeed (Speed: integer);
begin
   CodeSpeed := Speed;
   if CodeSpeed > 99 then CodeSpeed := 99;
   if CodeSpeed < 5 then CodeSpeed := 5;
   if KeyerInitialized then 
   begin
      sendcmd(CMD_KEYER_SPEED,codespeed);
   end;
end;

function YcccKeyer.GetSpeed:integer;
begin
   GetSpeed := CodeSpeed;
end;

procedure YcccKeyer.SetSwapPaddles(on: boolean);
begin
   SwapPaddles := on;
   if Swappaddles then keyer_config.paddle_rev := 1
      else keyer_config.paddle_rev := 0;
   if KeyerInitialized then 
   begin
      flushcwbuffer;
      sendcmd(CMD_KEYER_CONFIG,keyer_config.val);
   end;
end;

function YcccKeyer.GetSwapPaddles:boolean;
begin
   GetSwapPaddles := SwapPaddles;
end;

procedure YcccKeyer.SetCurtisMode(m: CurtisMode);
begin
   CurtMode := m;
   if CurtMode = ModeA then keyer_config.timing_a := 1;

   if CurtMode = Ultimatic then
      keyer_config.keyer_type := 1
   else
      keyer_config.keyer_type := 0;
   if KeyerInitialized then 
   begin
      flushcwbuffer;
      sendcmd(CMD_KEYER_CONFIG,keyer_config.val);
   end;
end;

function YcccKeyer.GetCurtisMode:CurtisMode;
begin
   GetCurtisMode := CurtMode;
end;

procedure YcccKeyer.SetPaddleBug(on: boolean);
begin
end;

function YcccKeyer.GetPaddleBug:boolean;
begin
   GetPaddleBug := false;
end;

function YcccKeyer.GetKeyerInitialized:boolean;
begin
   GetKeyerInitialized := KeyerInitialized;
end;

procedure YcccKeyer.AddCharacterToBuffer (Character: char);
var c: char;
begin
   c := upcase(Character);
   case c of
      'A'..'Z', '0'..'9', '.', ',', '?', '/', '+', '=', ' ':
      begin
         CWBuffer[CWBufferEnd] := c;
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
      end;

      '<':
      begin
         CWBuffer[CWBufferEnd] := '>';
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
      end;

//other yccckeyer characters
      '"','#','$','%',Char($27),'(',')','*',':',';','@','-':
      begin
         CWBuffer[CWBufferEnd] := c;
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
      end;

      Char(134), Char(143):
      begin // A-ring .--.-
         CWBuffer[CWBufferEnd] := '<';
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
      end;

      Char(132), Char(142):
      begin // A-umlaut .-.-
         CWBuffer[CWBufferEnd] := '*';
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
      end;

      
      Char(148), Char(153):
      begin // ---.
         CWBuffer[CWBufferEnd] := '`';
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
      end;

      '!':
      begin //...-. yccc can't send
         CWBuffer[CWBufferEnd] := 'S';
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
         CWBuffer[CWBufferEnd] := 'N';
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
      end;

      '&':
      begin
         CWBuffer[CWBufferEnd] := ']'; // AS -- wait character
         CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
      end;

      ControlE:
      begin //73 percent dah not implemented
      end;

        ControlDash:
        begin
           //150 percent dah not implemented
        end;

        ControlK:
        begin
           //normal dah not implemented
        end;

        ControlN:
        begin
           //113 percent dah not implemented
        end;

        ControlO:
        begin
          //127 percent dah not implemented
        end;

        ControlP:
        begin
          //60 percent dit not implemented
        end;

        ControlQ:
        begin
          //80 percent dit not implemented
        end;

        ControlBackSlash:
        begin
          //normal dit not implemented
        end;

        ControlV:
        begin
          //120 percent dit not implemented
        end;

        ControlL:
        begin
          //140 percent dit not implemented
        end;

        ControlF:   { Speed up command }
        begin
          //increase speed by 6 percent
           CWBuffer[CWBufferEnd] := c;
           CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
        end;

        ControlS:   { Slow down command }
        begin
          //decrease speed by 6 percent
           CWBuffer[CWBufferEnd] := c;
           CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
        end;


        ControlX:   { Decrease weight command }
        begin
           //decrease weight by .03 not implemented
        end;
 
        ControlY:   { Increase weight command }
        begin
           //increase weight by .03 not implemented
        end;

        '^':
        begin
           CWBuffer[CWBufferEnd] := '%';
           CWBufferEnd := (CWBufferEnd+1) mod CWBufferSize;
        end;
    end;
end;

Procedure YcccKeyer.SetPTTEnable(on: boolean);
begin
   PTTEnable := on;
   if PTTEnable then
      keyer_config.ptt := 1
   else
      keyer_config.ptt := 0;
   if KeyerInitialized then 
   begin
      sendcmd(CMD_KEYER_CONFIG,keyer_config.val);
   end;
end;

Function YcccKeyer.GetPTTEnable:boolean;
begin
   GetPTTEnable := PTTEnable;
end;

procedure YcccKeyer.UninitializeKeyer;
begin
   if KeyerInitialized then 
   begin
      KeyerInitialized := false;
//put box in a reasonable state
      keyer_control.pot_off := 0;
      keyer_control.pot_on := 1;
      sendcmd(CMD_KEYER_CONTROL,keyer_control.val);
      so2r_state.val := 0;
      so2r_state.stereo := 1;
      sendcmd(CMD_SO2R_STATE,so2r_state.val);
      so2r_config.val:= 0;
      so2r_config.relays := 1;
      sendcmd(CMD_SO2R_CONFIG,so2r_config.val);
      keyer_config.ptt := 1;
      sendcmd(CMD_KEYER_CONFIG,keyer_config.val);
      hid_close(hiddev);
      hid_exit;
   end;
end;

procedure YcccKeyer.SetTuneWithDits(on: boolean);
begin
   TuneWithDits := on;
end;

Function YcccKeyer.GetTuneWithDits:boolean;
begin
   GetTuneWithDits := TuneWithDits;
end;

function YcccKeyer.GetCountsSinceLastCW:integer;
begin
   GetCountsSinceLastCW := CountsSinceLastCw;
end;

procedure YcccKeyer.SetCountsSinceLastCW(count: integer);
begin
   CountsSinceLastCW := count;
end;

function YcccKeyer.GetFarnsworthEnable:boolean;
begin
   GetFarnsworthEnable := false;
end;

procedure YcccKeyer.SetFarnsworthEnable(on: boolean);
begin
end;

procedure YcccKeyer.SetFarnsworthSpeed(speed: integer);
begin
end;

function YcccKeyer.GetFarnsworthSpeed:integer;
begin
    GetFarnsworthSpeed := 0;
end;

procedure YcccKeyer.SetPTTFootSwitch(on :boolean);
begin
   PTTFootSwitch := on;
end;

function YcccKeyer.GetMonitorTone:integer;
begin
   GetMonitorTone := 0;
end;

procedure YcccKeyer.SetMonitorTone(tone: integer);
begin
end;

procedure YcccKeyer.SetPaddleMonitorTone(tone: integer);
begin
end;

function YcccKeyer.GetPaddleMonitorTone:integer;
begin
   GetPaddleMonitorTone := 0;
end;

procedure YcccKeyer.setpaddlespeed(speed: integer);
begin
   paddlespeed := speed; //does nothing for now
end;

procedure YcccKeyer.SetCwGrant(on: boolean);
begin
   FsCwGrant := on;
end;

procedure YcccKeyer.SetFootSwitch(f: FootSwitchx);
begin
   Footsw := f;
end;

procedure YcccKeyer.LetFootSwitchControlPTT;
begin
   //does nothing for now
end;

Function YcccKeyer.GetPaddleSpeed:integer;
begin
   getpaddlespeed := paddlespeed;
 //does nothing for now
end;

Function YcccKeyer.GetPTTAsserted:boolean;
begin
   getPTTAsserted := PTTAsserted;
end;

procedure YcccKeyer.Timer;
var
    i: integer;
    cs: integer;
    abortreason: keyer_abort_t;
begin
   if not KeyerInitialized then exit;
   if ctrlshift then //yccc keyer can't tune with dits
   begin
      if not tuning then
      begin
         tuning := true;
         keyer_control.val := 0;
         keyer_control.tune_on := 1;
         sendcmd(CMD_KEYER_CONTROL,keyer_control.val);
      end;
   end
   else if tuning then
   begin
      tuning := false;
      keyer_control.val := 0;
      keyer_control.tune_off := 1;
      sendcmd(CMD_KEYER_CONTROL,keyer_control.val);
   end;

   readresponses;
   while responsebufferstart <> responsebufferend do
   begin
      case responsebuffer[responsebufferstart].cmd of

         CMD_KEYER_ABORT:
         begin
            abortreason.val := responsebuffer[responsebufferstart].val;
            if (abortreason.paddle = 1) then flushlocal;
         end;
 
         CMD_SO2R_SWITCHES:
         begin
            so2r_switches.val := responsebuffer[responsebufferstart].val;
         end;

         CMD_KEYER_EVENT:
         begin
            case responsebuffer[responsebufferstart].val of
               KEYER_EVENT_END_CHAR:
               begin
                  idle := false;
                  doingpaddle := false;
                  mirror := 0;
               end;

               KEYER_EVENT_IDLE:
               begin
                  idle := true;
                  doingpaddle := false;
                  mirror := 0;
               end;

               KEYER_EVENT_CLEAR:
               begin
                  idle := true;
                  doingpaddle := false;
                  mirror := 0;
               end;

               KEYER_EVENT_PADDLE:
               begin
                  idle := false;
                  doingpaddle := true;
                  flushlocal;
                  if pttenable then
                  begin
                     paddlepttholdcount := round(714.0/codespeed)*paddlePTThold;
                     so2r_state.ptt := 1; //force ptt on for hold count
                     sendcmd(CMD_SO2R_STATE,so2r_state.val);
                  end;
               end;
            end;
         end;
      end;
      responsebufferstart := (responsebufferstart+1) mod ResponseBufferSize;
   end;


   while (mirror = 0) and (not doingpaddle)
       and (cwbufferstart <> cwbufferend) do
   begin
      i := Integer(cwbuffer[cwbufferstart]);
      case i of
         $06:
         begin
            cs := CodeSpeed+delta;
            inc(cs);
            if cs > 25 then inc(cs);
            if cs > 35 then inc(cs);
            if cs > 45 then inc(cs);
            if cs > 99 then cs := 99;
            delta := cs-CodeSpeed;
            sendcmd(CMD_KEYER_DELTA,delta);
            cwbufferstart := (cwbufferstart+1) mod CWBufferSize;
         end;

         $13:
         begin
            cs := CodeSpeed+delta;
            if cs > 49 then Dec (cs);
            if cs > 38 then Dec (cs);
            if cs > 27 then Dec (cs);
            if cs > 6 then Dec (cs);
            delta := cs-Codespeed;
            sendcmd(CMD_KEYER_DELTA,delta);
            cwbufferstart := (cwbufferstart+1) mod CWBufferSize;
         end;

         else
         begin
            cwbufferstart := (cwbufferstart+1) mod CWBufferSize;
            mirror := i;
            idle := false;
            sendcmd(CMD_KEYER_CHAR,i);
         end;

      end;

   end;

   if idle then
   begin
      if countssincelastcw = 0 then countssincelastcw := 1;
      if (delta <> 0) then
      begin
         delta := 0;
         sendcmd(CMD_KEYER_DELTA,0);
      end;
   end
   else
      countssincelastcw := 0;

   if pttenable and (pttforcedon and (so2r_state.ptt = 0)) then
   begin
      so2r_state.ptt := 1;
      sendcmd(CMD_SO2R_STATE,so2r_state.val);
   end;

   if (not doingpaddle) and (paddlePTTholdcount >= 0) then
      dec(paddlePTTholdcount);

   if (not pttforcedon) and (so2r_state.ptt = 1)
      and (paddlepttholdcount <= 0) then
   begin
      so2r_state.ptt := 0;
      sendcmd(CMD_SO2R_STATE,so2r_state.val);
   end;

   pttasserted := pttenable and (pttforcedon or (not idle));
end;

function YcccKeyer.CWStillBeingSent:boolean;
begin
   CWStillBeingSent := (not idle) or (not bufferempty);
end;

function YcccKeyer.BufferEmpty:boolean;
begin
   BufferEmpty := (cwbufferstart = cwbufferend) and (mirror = 0);
end;

procedure YcccKeyer.AddStringToBuffer (Msg: String; Tone: INTEGER);
var i: integer;
begin
   for i := 1 to length(msg) do
   begin
      AddCharacterToBuffer(Msg[i]);
   end;
end;

function YcccKeyer.DeleteLastCharacter:boolean;
begin
   if cwbufferstart <> cwbufferend then
   begin
      cwbufferend := (cwbufferend -1 + CWBufferSize) mod CWBufferSize;
      DeleteLastCharacter := true;
      exit;
   end;
   DeleteLastCharacter := (mirror <> 0);
   if DeleteLastCharacter then
   begin
      sendcmd(CMD_KEYER_OVERWRITE,0);
      mirror := 0;
   end;
end;

procedure YcccKeyer.flushCwBuffer;
begin
   cwbufferstart := 0;
   cwbufferend := 0;
   mirror := 0;
   sendcmd(CMD_KEYER_ABORT,0);
   delta := 0;
   sendcmd(CMD_KEYER_DELTA,0);
end;

procedure YcccKeyer.flushLocal;
begin
   cwbufferstart := 0;
   cwbufferend := 0;
   mirror := 0;
   delta := 0;
   sendcmd(CMD_KEYER_DELTA,0);
end;

procedure YcccKeyer.sendcmd(cmd: integer; val: integer);
var rc: integer;
begin
   snddata[0] := 0;
   snddata[1] := cmd;
   snddata[2] := val;
   rc := hid_write(hiddev,@snddata[0],DATA_SIZE);

//writeln(stderr,'sndcmd ',Inttohex(cmd,2),' ',Inttohex(val,2));
//flush(stderr);
//if (rc <> 3) then
//   writeln(stderr,'sndcmd ',cmd,' ',val,' wrote ',rc,' bytes');

end;

procedure YcccKeyer.readresponses;
var n,i: integer;
var i1,i2,i3: integer;
begin
   while true do
   begin
      n := hid_read(hiddev,@rcvdata[0],3);
      if (n <= 0) then exit;
//if (n <> 2 ) then writeln(stderr,n,' bytes read');
//      for i := 0 to n-1 do
//      begin
//         hidbytes[nbytes] := rcvdata[i];
//         inc(nbytes);
//         if nbytes = 2 then
//         begin
//            nbytes := 0;
//            i1 := hidbytes[0];
//            i2 := hidbytes[1];
//            writeln(stderr,'response ',inttohex(i1,2),' ',inttohex(i2,2));
//            flush(stderr);
//            responsebuffer[responsebufferend].cmd := hidbytes[0];
//            responsebuffer[responsebufferend].val := hidbytes[1];
      if (n = 2) then
      begin
//i1 := rcvdata[0];
//i2 := rcvdata[1];
//writeln(stderr,'response ',inttohex(i1,2),' ',inttohex(i2,2));
//flush(stderr);
         responsebuffer[responsebufferend].cmd := rcvdata[0];
         responsebuffer[responsebufferend].val := rcvdata[1];
         responsebufferend := (responsebufferend + 1) mod ResponseBufferSize;
      end;
   end;
end;

Procedure YcccKeyer.SetPort(port: serialportx);
begin
end;

procedure yccckeyer.setheadphonemode(hmode: hmode_t);
begin
   case hmode of
      HNORMAL: so2r_config.typex := SO2R_CONFIG_NORMAL;
      HSYMMETRIC: so2r_config.typex := SO2R_CONFIG_SYMMETRIC;
      HSPATIAL: so2r_config.typex := SO2R_CONFIG_SPATIAL;
   end;
   if KeyerInitialized then
   begin
      sendcmd(CMD_SO2R_CONFIG,so2r_config.val);
   end;
end;

function yccckeyer.getheadphonemode:hmode_t;
begin
   case so2r_config.typex of
      SO2R_CONFIG_NORMAL: getheadphonemode := HNORMAL;
      SO2R_CONFIG_SYMMETRIC: getheadphonemode := HSYMMETRIC;
      SO2R_CONFIG_SPATIAL: getheadphonemode := HSPATIAL;
   end;
end;

procedure yccckeyer.setblend(on: boolean);
begin
   if on then
      so2r_config.blend := 1
   else
      so2r_config.blend := 0;
   if KeyerInitialized then
   begin
      sendcmd(CMD_SO2R_CONFIG,so2r_config.val);
   end;
end;

function yccckeyer.getblend:boolean;
begin
   getblend := (so2r_config.blend = 1);
end;

procedure yccckeyer.blendvalue(val: integer);
begin
   if val < 0 then val := 0;
   if val > 255 then val := 255;
   blend := val;
   if KeyerInitialized then 
   begin
      sendcmd(CMD_SO2R_BLEND,blend);
   end;
end;

function yccckeyer.getblendvalue:integer;
begin
   getblendvalue := blend;
end;

procedure yccckeyer.setmicrelay(on: boolean);
begin
   if on then
      so2r_config.relays:= 1
   else
      so2r_config.relays:= 0;
   if KeyerInitialized then
   begin
      sendcmd(CMD_SO2R_CONFIG,so2r_config.val);
   end;
end;

function yccckeyer.getmicrelay:boolean;
begin
   getmicrelay := (so2r_config.relays = 1);
end;


procedure yccckeyer.setrig1map(val: integer);
var
   map: so2r_map_t;
begin
   if (val < -4) then val := -4;
   if (val > 4) then val := 4;
   map1 := val;
   if KeyerInitialized and (map1 <> 0) then
   begin
      map.val := 0;
      if (map1 < 0) then 
      begin
         map.radio := -map1-1;
         map.eeprom := 1;
         map.current:= 1;
      end
      else
      begin
         map.radio := map1-1;
         map.eeprom := 0;
         map.current:= 1;
      end;
      sendcmd(CMD_SO2R_MAP1,map.val);
   end;
end;

function yccckeyer.getrig1map:integer;
begin
   getrig1map := map1;
end;

procedure yccckeyer.setrig2map(val: integer);
var
   map: so2r_map_t;
begin
   if (val < -4) then val := -4;
   if (val > 4) then val := 4;
   map2 := val;
   if KeyerInitialized and (map2 <> 0) then
   begin
      map.val := 0;
      if (map2 < 0) then 
      begin
         map.radio := -map2-1;
         map.eeprom := 1;
         map.current:= 1;
      end
      else
      begin
         map.radio := map2-1;
         map.eeprom := 0;
         map.current:= 1;
      end;
      sendcmd(CMD_SO2R_MAP2,map.val);
   end;
end;

function yccckeyer.getrig2map:integer;
begin
   getrig2map := map2;
end;

procedure YcccKeyer.setrcvfocus(rcvfocus: rcvfocus_t);
begin
   case rcvfocus of
      RX1:
      begin
         so2r_state.stereo := 0;
         so2r_state.rx2 := 0;
      end;

      RX2:
      begin
         so2r_state.stereo := 0;
         so2r_state.rx2 := 1;
      end;

      STEREO:
      begin
         so2r_state.stereo := 1;
      end;
   end;
   if KeyerInitialized then
   begin
      sendcmd(CMD_SO2R_STATE,so2r_state.val);
   end;
end;

function YcccKeyer.footswitchpressed:boolean;
begin
   footswitchpressed := so2r_switches.ptt = 1;
end;

END.
