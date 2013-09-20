unit linuxsound;
{$mode objfpc}  
interface

var
    BeepSoundCardEnable: boolean;
    dvpsetup: boolean = false;
    sounddevice: string[80] = 'hw:0,0';

procedure lsound(hz: longint);
procedure lnosound;
procedure beginsound;
procedure endsound;
procedure openconsole;
procedure playfile(f: pchar);
procedure soundmode(m: longint);
function playingfile:boolean;

implementation

uses  
  sysutils {$ifdef unix},cthreads{$endif},baseunix,unix,sndfile;

{$linklib asound}

Const
  SND_PCM_STREAM_PLAYBACK = 0;
  SND_PCM_ACCESS_RW_INTERLEAVED = 3;
  SND_PCM_FORMAT_S16_LE = 2;
  EPIPE = 32;
  KIOCSOUND = 19247;
  KDMKTONE = 19248;

var
   rc: longint;
   handle,params,swparams: pointer;
   device: pchar;
   rate: longint = 44100;
   dir: longint;
   buffer: ^smallint;
   hznow: longint = 0;
   dspopen: longint = 0;
   fdconsole: longint = -1;
   filename: pchar = nil;
   mode: longint = 0; // 0 = beep 1 = files 2 = stop file
   playing: longint = 0;
   bufsize: longint;
   persize: longint;
   buffertime: longint = 30000; //in microseconds
   periodtime: longint = 15000; //in microseconds

   function snd_pcm_open(handle: pointer; device: pchar; streamtype: longint;
      f: longint):longint;cdecl;external;
   procedure snd_pcm_hw_params_malloc(params: pointer);cdecl;external;
   procedure snd_pcm_sw_params_malloc(params: pointer);cdecl;external;
   procedure snd_pcm_hw_params_free(params: pointer);cdecl;external;
   procedure snd_pcm_hw_params_set_access(handle: pointer; params:pointer;
      indata:longint);cdecl;external;
   procedure snd_pcm_hw_params_set_format(handle: pointer; params:pointer;
      indata:longint);cdecl;external;
   procedure snd_pcm_hw_params_set_channels(handle: pointer; params:pointer;
      indata:longint);cdecl;external;
   procedure snd_pcm_hw_params_set_rate_near(handle: pointer;params:pointer;
      val: pointer; dir:pointer);cdecl;external;
   procedure snd_pcm_hw_params_set_period_time_near(handle: pointer;
      params:pointer; val: pointer; dir:pointer);cdecl;external;
   procedure snd_pcm_hw_params_set_buffer_time_near(handle: pointer;
      params:pointer; val: pointer; dir:pointer);cdecl;external;
   procedure snd_pcm_hw_params_get_buffer_size(params:pointer; val: pointer);
      cdecl;external;
   function snd_pcm_hw_params(handle:pointer; params:pointer):longint;cdecl;
      external;
   procedure snd_pcm_hw_params_get_rate(params:pointer;
      rate:pointer ;dir:pointer);cdecl;external;
   procedure snd_pcm_hw_params_get_period_size(params:pointer;
      val:pointer ;dir:pointer);cdecl;external;
   procedure snd_pcm_hw_params_any(handle:pointer; params:pointer);
      cdecl;external;
   procedure snd_pcm_drain(handle:pointer);cdecl;external;
   procedure snd_pcm_close(handle:pointer);cdecl;external;
   function snd_pcm_drop(handle:pointer):longint;cdecl;external;
   function snd_pcm_writei(handle:pointer;buffer:pointer;frames:longint):
      longint;cdecl;external;
   function snd_pcm_hw_params_can_pause(handle:pointer):longint;cdecl;external;
   function snd_pcm_hw_params_can_resume(handle:pointer):longint;cdecl;external;
   function snd_pcm_start(handle:pointer):longint;cdecl;external;
   function snd_pcm_prepare(handle:pointer):longint;cdecl;external;
   function snd_pcm_sw_params_set_start_threshold(handle:pointer;
      params:pointer; val:longint):longint;cdecl;external;
   function snd_pcm_sw_params_set_avail_min(handle:pointer; params:pointer;
      val:longint):longint;cdecl;external;
   function snd_pcm_sw_params(handle:pointer; params: pointer):longint;
      cdecl;external;
   function snd_pcm_sw_params_current(handle:pointer; params: pointer):longint;
      cdecl;external;

procedure lsound(hz:longint);
var val:longint;
begin
   if beepsoundcardenable then
   begin
      interlockedexchange(hznow,hz);
   end
   else 
   begin
      if (fdconsole = -1) then exit;
      if (hz = 0) then
      begin
         fpioctl(fdconsole, KIOCSOUND,nil);
      end
      else 
      begin
         val := (2000 << 16) + (1193180 div hz); //max 2 seconds 
         fpioctl(fdconsole, KDMKTONE, pointer(val));
      end;
   end;
end;

procedure lnosound;
begin
   if beepsoundcardenable then
   begin
     interlockedexchange(hznow,0);
   end
   else
   begin
      if (fdconsole = -1) then exit;
      fpioctl(fdconsole, KIOCSOUND,nil);
   end;
end;

procedure openalsa;
begin
   handle := nil;
   params := nil;
   snd_pcm_open(@handle,device,SND_PCM_STREAM_PLAYBACK,0);
   snd_pcm_hw_params_malloc(@params);
   snd_pcm_sw_params_malloc(@swparams);
   snd_pcm_hw_params_any(handle,params);
   snd_pcm_hw_params_set_access(handle,params,SND_PCM_ACCESS_RW_INTERLEAVED);
   snd_pcm_hw_params_set_format(handle,params,SND_PCM_FORMAT_S16_LE);
   snd_pcm_hw_params_set_channels(handle,params,2);
   snd_pcm_hw_params_set_rate_near(handle,params,@rate,@dir);
   snd_pcm_hw_params_set_period_time_near(handle,params,@periodtime,@dir);
   snd_pcm_hw_params_set_buffer_time_near(handle,params,@buffertime,@dir);
   snd_pcm_hw_params_get_buffer_size(params,@bufsize);
   snd_pcm_hw_params_get_period_size(params,@persize,@dir);
   snd_pcm_hw_params(handle,params);
   snd_pcm_hw_params_get_rate(params,@rate,@dir);
   snd_pcm_sw_params_current(handle,swparams);
   snd_pcm_sw_params_set_start_threshold(handle,swparams,
      (bufsize div persize)*persize);
   snd_pcm_sw_params_set_avail_min(handle,swparams,persize);
   snd_pcm_sw_params(handle,swparams);
   getmem(buffer,persize*4);
//writeln(stderr,'rate ',rate);
//writeln(stderr,'period size ',persize);
//writeln(stderr,'buffer size',bufsize);
//flush(stderr);
end;

procedure closealsa;
begin
  snd_pcm_drain(handle);
  snd_pcm_close(handle);
  snd_pcm_hw_params_free(params);
  freemem(buffer);
end;

function soundthread(p : pointer) : ptrint;  
var hz,hzold: longint;
    count: longint = 0;
    i: integer;
    req,rem: timespec;
    fac: real;
    dsp: longint;
    f,fx: pchar;
    soundfile: psndfile;
    info: tsf_info;
    pl,m: longint;
    s,c,ds,dc,temp: real;
begin  
  dsp := interlockedcompareexchange(dspopen,1,0);
  if dsp = 1 then
  begin
     strdispose(device);
     soundthread := 0;
     endthread;
     exit;
  end;
  openalsa;
  dsp := 1;
  hz := interlockedexchange(hznow,0);
  fac := 2.0*Pi/rate;
  hzold := 0;
     while true do
     begin
       if (mode = 0) then
       begin
        hz := interlockedexchangeadd(hznow,0);
        if hz < 0 then
        begin
           closealsa;
           dsp := interlockedexchange(dspopen,0);
           strdispose(device);
           soundthread := 0;
           endthread;
           exit;
        end;
        if (hz <> hzold) then 
        begin
           if count <> 0 then snd_pcm_drop(handle);
           count := 0;
        end;
        if hz > 0 then
        begin
           if count = 0 then
           begin
              s := 0.0;
              c := 10000.0;
              ds := sin(fac*hz);
              dc := cos(fac*hz);
              rc := snd_pcm_prepare(handle);
           end;
           for i:=0 to persize-1 do
           begin
              temp := s*dc+c*ds;
              c := c*dc-s*ds;
              s := temp;
              buffer[2*i] := trunc(s);
              buffer[2*i+1] := buffer[2*i];
              inc(count);
           end;
           rc := snd_pcm_writei(handle,buffer,persize);
           if rc = -EPIPE then
           begin
//              writeln(stderr,'underrun');
              snd_pcm_prepare(handle);
           end;
        end
        else
        begin
           if ((dsp = 1) and (count <> 0)) then snd_pcm_drop(handle);
           count := 0;
           req.tv_sec := 0;
           req.tv_nsec := 100000;
           fpNanoSleep(@req,@rem);
        end;
        hzold := hz;
      end
      else
      begin
        fx := nil;
        f := interlockedexchange(filename,fx);
        if (f <> nil) then
        begin
           pl := interlockedexchange(playing,1);
           soundfile := sf_open(f,SFM_READ,@info);
           if ( (info.channels = 2) and (info.samplerate = 44100)) then
           begin
              rc := snd_pcm_prepare(handle);
              count := sf_read_short(soundfile,buffer,persize*2);
              while count <> 0 do
              begin
                 m := interlockedexchange(mode,1);
                 if (m = 2) then
                 begin
                    snd_pcm_drop(handle);
                    count := 0;
                 end
                 else
                 begin
                    rc := snd_pcm_writei(handle,buffer,persize);
                    if rc = -EPIPE then 
                    begin
//                       writeln(stderr,'underrun');
                       snd_pcm_prepare(handle);
                    end;
                    count := sf_read_short(soundfile,buffer,persize*2);
                 end;
              end;
              snd_pcm_drain(handle);
              pl := interlockedexchange(playing,0);
           end
        end
        else
        begin
           req.tv_sec := 0;
           req.tv_nsec := 100000;
           fpNanoSleep(@req,@rem);
        end;
      end;
   end;
   soundthread := 0;
   strdispose(device);
   endthread;
end;

procedure beginsound;
var i : longint;  
begin
   device := stralloc(81);
   strpcopy(device,sounddevice);
   i := 1;
   BeginThread(@soundthread,pointer(i));  
end;

procedure endsound;
begin
  interlockedexchange(hznow,-1);
end;

procedure openconsole;
begin
   fdconsole := fpopen('/dev/console', O_RDONLY or O_NONBLOCK);
end;

procedure playfile(f: pchar); 
var ftemp: pchar;
begin
   ftemp := f;
   interlockedexchange(filename,ftemp);
end;

procedure soundmode(m: longint);
begin
   interlockedexchange(mode,m);
end;

function playingfile:boolean;
begin
   playingfile := (interlockedexchangeadd(playing,0) = 1);
end;

end.
