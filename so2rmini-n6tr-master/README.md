# N6TR firmware for the So2r-mini

This is an Arduino firmware developed by N6TR for the SO2R Mini keyer/SO2R
controller:

[SO2R Mini](https://nn1c.org/so2r/)

## Contest logging programs that currently support this firmware:

* [TR Linux](https://github.com/trlinux/trlinux)
* [SO2SDR](https://github.com/n4ogw/so2sdr)


## Features:

* Can use either Arduino Nano or Nano Every
* CW sidetone output
* optional 16 line I2C IO expander using MCP23017

## Configuring

* Auxiliary IO: uncomment
   
     #define AUXILIARY_PORT

* Send status bytes (required for SO2SDR): uncomment
   
     #define SEND_STATUS_BYTES
	
## Commands

The serial port to the PC runs at 19200 baud. Characters sent to the device
are sent as Morse. Commands to the device start with a semicolon.
The letter following ; can be either lower or uppercase. The following
commands can be embedded within a CW message:

* ;d : send long dah
* ;f : increase CW speed 2 WPM
* ;h : insert half dit space
* ;i : switch headphones to inactive radio
* ;j : switch headphones to active radio
* ;s : decrease CW speed 2 WPM
* ;x : switch headphones to radio 1
* ;y : switch headphones to radio 2
* ;z : put headphones in stereo


The following bytes are interpreted as control commands. Some
require a second data byte

* <01>  Send version # to host (no data)
* <02>  SO2R relay commands. Data byte: bit 0: turns K1 on/off;
  bit 1: turns K2 on/off; bit 2: turns K3 on/off
* <03>  Computer CW sidetone. Data byte: sidetone frequency in Hz in
multiples of 10 (50 = 500 Hz for example)
* <04>  Paddle CW sidetone. Data byte: sidetone frequency in Hz in
multiples of 10 (50 = 500 Hz for example)
* <05>  Dit/dah orientation. Data byte: 0 versus not 0 swaps dah/dit
* <06>  Keyer weight. Weight in percent = (data byte)/100
* <07>  CW character offset time
* <08>  CW Speed for computer CW. Data byte = CW speed.
* <09>  CW Speed for paddle CW  Data byte = CW speed (00 = track computer speed)
* <0A>  PTT Control. Nonzero data byte turns on PTT
* <0B>  Radio select. Data byte: either 1 or 2 for radio 1 or 2.
* <0C>  Query if CW still being sent (no data)
* <0D>  Query # of characters left in CW buffer  (no data)
* <0E>  PTT assert time before CW. Data byte = time in ms
* <0F>  PTT hold time for computer CW. Data byte = time in ms
* <10>  PTT hold time for paddle CW. Data byte = time in ms
* <11>  Query Footswitch state (no data)  (01 = pressed)
* <12>  Immediate stop sending now - clear PTT and buffer (no data)
* <13>  Stop sending after letter is completed (no data)
* <14>  Set Curtis keying mode (data: 00=A  01=B  02=NL)
* <15>  Paddle Bug Mode (data: 00 = disabled)
* <16>  PTT Enable (data 00= diabled)
* <19>  Footswith to PTT enable (00= no PTT, 01 = control PTT)
* <1A>  Delete last character in send buffer (returns 01 if successful)
* <1B>  Return number of counts since last CW (250 ms / count)
* <1C>  Send a ? on the inactive radio at the next word space
* <1D>  Send data to I/O expander
