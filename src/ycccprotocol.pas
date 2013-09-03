unit ycccprotocol;
interface
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  { Copyright 2006, 2009 Paul Young
   *
   * Protocol
   *
   * Author                Date          Comment
   *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * Paul Young             12/05/06      Original.
   *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * Paul Young             1/19/08       Added Keyer control.
   *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * Paul Young             12/21/08       Added radio map.
   *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * Paul Young             4/16/09       Misc changes for emulator.
   *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * Paul Young             8/20/09       Add reverse stereo.
   ******************************************************************* }
{$ifndef PROTOCOL_H}
{$define PROTOCOL_H}  
  { The USB messages are two bytes long. The first byte specifies the }
  { command and the second is the value sent or received. }
  { These are the vendor and product IDs of the SO2R Box }

  const
    VENDOR_ID = $16c0;    { See http://www.voti.nl/pids }
    PRODUCT_ID = $065e;    { 1630 }
    PROTOCOL_VERSION = $01;    
  { The query command }
    CMD_QUERY = $00;    
  { The box commands }
    CMD_BOX_PATCHLEVEL = $01;    { Send current patch level }
    CMD_BOX_VERSPECIAL = $02;    { Send the version special info }
    CMD_BOX_UPDATE = $08;    { Firmware update }
  { These are reserved but probably not implemented in shipped code }
    CMD_BOX_DEBUG1 = $0a;    { Debug 1 }
    CMD_BOX_DEBUG2 = $0b;    { Debug 2 }
    CMD_BOX_DEBUG3 = $0c;    { Debug 3 }
    CMD_BOX_DEBUG4 = $0d;    { Debug 4 }
  { The keyer commands }
    CMD_KEYER_STATUS = $10;    { Idle, sending }
    CMD_KEYER_SPEED = $11;    { Set/get keyer speed }
    CMD_KEYER_CONFIG = $12;    { Set/get keyer configuration bits }
    CMD_KEYER_CHAR = $13;    { Set next character to send }
    CMD_KEYER_OVERWRITE = $14;    { Overwrite next character to send }
    CMD_KEYER_ABORT = $15;    { Abort sending from computer }
    CMD_KEYER_CONTROL = $16;    {  Misc control functions }
    CMD_KEYER_EVENT = $17;    {  Event notification }
    CMD_KEYER_DELTA = $18;    {  Delta speed (queued) }
    CMD_KEYER_PTT_PRE = $19;    {  PTT time before transmitting }
    CMD_KEYER_PTT_POST = $1a;    {  PTT time after transmitting }
    CMD_KEYER_WEIGHT = $1b;    {  Keyer weight }
    CMD_KEYER_POT_SPEED = $1c;    {  Pot speed }
    CMD_KEYER_SENDING_CHAR = $1d;    {  Character is being sent }

type
    keyer_config_t = bitpacked record
       case integer of
       0: (
          keyer_type: 0..7;
          paddle_rev:  0..1;
          space_seven: 0..1;
          timing_a: 0..1;
          tight: 0..1;
          ptt: 0..1;
          );
       1: (val: 0..255);
    end;

const
    M_KEYER_CONFIG_KEYER_TYPE = $07;    
    M_KEYER_CONFIG_PADDLE_REV = $08;    
    M_KEYER_CONFIG_SPACE_SEVEN = $10;    
    M_KEYER_CONFIG_TIMING_A = $20;    
    M_KEYER_CONFIG_TIGHT = $40;    
    M_KEYER_CONFIG_PTT = $80;    

type
    keyer_status_t = bitpacked record
       case integer of
       0: (
          state          : 0..7;
          disabled       : 0..1;
          ready          : 0..1;
          reserved_1     : 0..7;
          );
       1: (val: 0..255);
    end;

const

    M_KEYER_STATUS_STATE = $07;    
    M_KEYER_STATUS_DISABLED = $08;    
    M_KEYER_STATUS_READY = $10;    
  { Keyer min and max speeds }
    KEYER_SPEED_MIN = 2;    { Minimum speed }
    KEYER_SPEED_MAX = 99;    { Maximum speed }
  { Keyer types }
    KEYER_TYPE_IAMBIC = 0;    {  Both paddles alternate }
    KEYER_TYPE_ULTIMATIC = 1;    {  Both paddles send last pressed }
    KEYER_TYPE_DIT = 2;    {  Both paddles send dit }
    KEYER_TYPE_DAH = 3;    {  Both paddles send dah }
    KEYER_MAX_TYPE = KEYER_TYPE_DAH;    
  { Keyer visible states }
    KEYER_VSTATE_IDLE = 0;    { Keyer is idle }
    KEYER_VSTATE_PADDLE = 1;    { Keyer is sending from paddle }
    KEYER_VSTATE_REMOTE = 2;    { Keyer is sending from computer }
    KEYER_VSTATE_TUNE = 3;    { Keyer is sending daaaaaaaaah }

type
    keyer_control_t = bitpacked record
       case integer of
       0: (
          tune_off       : 0..1;
          tune_on        : 0..1;
          pot_off        : 0..1;
          pot_on         : 0..1;
          reserved_1     : 0..15;
          );
       1: (val: 0..255);
    end;

const

    M_KEYER_CONTROL_TUNE_OFF = $01;    
    M_KEYER_CONTROL_TUNE_ON = $02;    
    M_KEYER_CONTROL_POT_OFF = $04;    
    M_KEYER_CONTROL_POT_ON = $08;    
  { Keyer abort reasons }

type
    keyer_abort_t = bitpacked record
       case integer of
       0: (
          command        : 0..1;
          paddle         : 0..1;
          tx_changed     : 0..1;
          reserved_1     : 0..31;
          );
       1: (val: 0..255);
    end;

const

    M_KEYER_ABORT_COMMAND = $01;    
    M_KEYER_ABORT_PADDLE = $02;    
    M_KEYER_ABORT_TX_CHANGED = $04;    
  { Keyer events }
    KEYER_EVENT_IGNORE = 0;    
    KEYER_EVENT_END_CHAR = 1;    { Keyer has sent the end of a character }
    KEYER_EVENT_IDLE = 2;    { Keyer has finished sending }
    KEYER_EVENT_CLEAR = 3;    { Keyer buffer was cleared with overwrite 0 }
    KEYER_EVENT_PADDLE = 4;    { Paddle was used }
  { The aux commands }
    CMD_AUX_PORT1 = $20;    
    CMD_AUX_PORT2 = $21;    
    CMD_AUX_PORT3 = $22;    
    CMD_AUX_PORT4 = $23;    
  { Aux info }

type
    aux_info_t = bitpacked record
       case integer of
       0: (
          aux            : 0..15;
          update         : 0..1;
          reserved_1     : 0..7;
          );
       1: (val: 0..255);
    end;

const

    M_AUX_INFO_AUX = $0F;    
    M_AUX_INFO_APDATE = $10;    
  { The SO2R commands }
    CMD_SO2R_STATE = $30;    
    CMD_SO2R_CONFIG = $31;    
    CMD_SO2R_SWITCHES = $32;    
    CMD_SO2R_BLEND = $33;    
    CMD_SO2R_MAP1 = $34;    
    CMD_SO2R_MAP2 = $35;    
    CMD_SO2R_CONFIG2 = $36;    
  { SO2R State }

type
    so2r_state_t = bitpacked record
       case integer of
       0: (
          tx2            : 0..1;
          rx2            : 0..1;
          stereo         : 0..1;
          ptt            : 0..1;
          stereo_reverse : 0..1;
          reserved_1     : 0..7;
          );
       1: (val: 0..255);
    end;

const

    M_SO2R_STATE_TX2 = $01;    
    M_SO2R_STATE_RX2 = $02;    
    M_SO2R_STATE_STEREO = $04;    
    M_SO2R_STATE_PTT = $08;    
    M_SO2R_STATE_REVERSE = $10;    
  { SO2R Configuration }

type
    so2r_config_t = bitpacked record
       case integer of
       0: (
          typex          : 0..7;
          blend          : 0..1;
          relays         : 0..1;
          reserved_1     : 0..7;
          );
       1: (val: 0..255);
    end;

const

    M_SO2R_CONFIG_TYPE = $07;    
    M_SO2R_CONFIG_BLEND = $08;    
  { SO2R Configurations }
    SO2R_CONFIG_NORMAL = $00;    { Normal stereo mode }
    SO2R_CONFIG_SYMMETRIC = $01;    { Left and right always same }
    SO2R_CONFIG_SPATIAL = $02;    { Left and right keep positions }
  { SO2R Box switches }

type
    so2r_switches_t = bitpacked record
       case integer of
       0: (
          tx1            : 0..1;
          tx2            : 0..1;
          rx1            : 0..1;
          rx2            : 0..1;
          ptt            : 0..1;
          reserved_1     : 0..7;
          );
       1: (val: 0..255);
    end;

const

    M_SO2R_SWITCHES_TX1 = $01;    
    M_SO2R_SWITCHES_TX2 = $02;    
    M_SO2R_SWITCHES_RX1 = $04;    
    M_SO2R_SWITCHES_RX2 = $08;    
    M_SO2R_SWITCHES_PTT = $10;    
  { SO2R Box radio mapping }

type
    so2r_map_t = bitpacked record
       case integer of
       0: (
          radio          : 0..15;
          eeprom         : 0..1;
          current        : 0..1;
          reserved_1     : 0..3;
          );
       1: (val: 0..255);
    end;

const

    M_SO2R_MAP_RADIO = $0f;    
    M_SO2R_MAP_EEPROM = $10;    
    M_SO2R_MAP_CURRENT = $20;    

type
    so2r_map_ret_t = bitpacked record
       case integer of
       0: (
          current        : 0..15;
          eeprom         : 0..15;
          );
       1: (val: 0..255);
    end;

const

    M_SO2R_MAP_RET_CURRENT = $0f;    
    M_SO2R_MAP_RET_EEPROM = $f0;    
  { Protocol error }
    CMD_ERROR = $7f;    
  { Status }
    STATUS_SUCCESS = $00;    
    STATUS_BADVALUE = $01;    
    STATUS_BUSY = $02;    
    STATUS_LATE = $03;    
{$endif}

implementation


end.
