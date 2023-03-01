#include "TimerOne.h"

// 18-Apr-2022 This keyer program was initially part of the contest simulator program V5.  Rmoved DDX stuff.
// 19-Apr-2022 Forked into an SO2R Mini version with pin assignments changed to support the NN1C SO2R mini
// 27-Apr-2022 Forked again - removed Fred and Tina CW decoders.  Moved serial data check into main loop
// 28-Apr-2022 Fixed format of {'s.  Added global variables affected by host commands + support of some response

// 29-Apr-2022 Forked again - decided to reduce the size of the CWBuffer and create a character buffer instead.
// I also made the CW engine work mostly exactly the same as the old TR CW engine - computing the dit and dah
// lengths just before they get sent intead of when the characters come down.  This helps support dynamic speed
// changes and perhaps a speed pot someday.

// 14-May-2022 Improved footswitch code and got it back into the Timer loop.

// 28-Nov-2022 Changed CW command character from ^ to ; (semi-colon)
// 28-Nov-2022 Changed version to TRCW V4
// 28-Nov-2022 Added support for CW character ^ as half space.
// 28-Nov-2022 Changed PTT Unforce so that is there are characters in the buffer, PTT stays on
// 28-Nov-2022 Fixed bug where any PTT command would turn it off instantly.

// 29-Nov-2022 Cleared PTT if things were idle and PTTForceOn goes away

// 27-Feb-2023 Likely fixed bug introduced on 29-Nov where there was a glitch in the PTT signal with footswitch
// 27-Feb-2023 Also fixed bug where footswitch was always enabled due to bad use of logic symbol

// 28-Feb-2023 Received update from N4OGW which is now adopted as the source

// Folded in N4OGW PTT feedback mechanism. 

// Uncomment next line to enable N4OGW PTT status updates

//#define SEND_STATUS_BYTES
#ifdef SEND_STATUS_BYTES
const unsigned char sending1=0x01;
const unsigned char sending2=0x02;
const unsigned char not_sending=0x00;
#endif

// SO2R Mini pin assignments

const int radioTwoPTTOutput = 2;
const int radioOnePTTOutput = 3;
const int radioTwoCWOutput = 4;
const int radioOneCWOutput = 5;

int ditInput = 6;                 // Input pin for dit paddle contact
int dahInput = 7;                 // Input pin for dah paddle contact
const int pttInput = 8;           // Footswitch input
const int k1Output = 9;           // K1 moves the headphones from Radio 1 to Radio 2
const int k2Output = 10;          // K2 puts headphones in stereo mode (needs K1 active)
const int k3Output = 11;          // K3 selects which radio the mic goes to (off = R1)
const int speakerOutput = 12;
const int led = 13;

// Paddle send status states

const int PADDLE_SEND_STATUS_NOTHING_BEING_SENT = 0;
const int PADDLE_SEND_STATUS_DIT_BEING_SENT     = 1;
const int PADDLE_SEND_STATUS_DAH_BEING_SENT     = 2;

// CW Engine states

const int CWS_Idle = 0;
const int CWS_WaitingForPTTTurnOn = 1;
const int CWS_SendingElement = 2;
const int CWS_PTTTurningOff = 3;

int CWState = CWS_Idle;

// Curtis mode states

const int CURTIS_MODE_A  = 0;
const int CURTIS_MODE_B  = 1;
const int CURTIS_MODE_NL = 2;  // Default
const int ULTIMATIC = 3;

// ULTIMATIC MEMORY

const int NoFirstElement = 0;
const int DitSentFirst = 1;
const int DahSentFirst = 2;

// CW Buffer

#define CWBufferSize 100     // Used for sending a CW character - an array of timings - and whether the key is up or down

struct CWBufferEntry
{
  unsigned char Data;       // 0 for keyup  1 for keydown
  unsigned char Time;       // Uses LOGK!EA times - dit is 10, dah is 30 - dat is 45
};

struct CWBufferType
{
  CWBufferEntry CWBufferArray [CWBufferSize];
  int Head;                      // points to the next entry to be added to the buffer
  int Tail;                      // points to the next entry to be popped out of the buffer when Head <> Tail
};

CWBufferType CWBuffer;

// Character Buffer

#define CharacterBufferSize 200     // Used for characters to be sent coming from the host until they are ready to be sent

struct CharacterBufferType
{
  char CharacterBufferArray [CharacterBufferSize];
  int Head;           // points to the next entry to be added to buffer
  int Tail;           // points to the next entry to be pooped out of the buffer when head <> tail
};

CharacterBufferType CharacterBuffer;

// Keyer stuff

int CurtisMode = CURTIS_MODE_NL;               // CURTIS_MODE_A or CURTIS_MODE_B or CURTIS_MODE_NL oe ULTIMATIC
int UltimaticFirstElement = NoFirstElement;
int DisableDitMemory = 0;               // Length of time to disable dit memory when in A or NL Curtis Mode
int DisableDahMemory = 0;               // Length of time to disable dah memory when in A or NL Curtis Mode

boolean DitMemory = false;
boolean DahMemory = false;
boolean PaddleActive = false;
boolean WaitingForCWCommand = false;

int FootswitchPressedCount = 0;
int FootswitchNotPressedCount = 0;

boolean IsolatedDitContactFound = false;
boolean IsolatedDahContactFound = false;

int ComputerCWSpeed = 30;                // Speed of host CW being
int PaddleCWSpeed = 0;                  // Speed of paddle CW

int PTTForceOn = 0;                      // 1 = PTT forced on
int PTTTurnOnTime = 5;                   // Time PTT asserted before CW
int PTTOutputEnable = 1;                 // Enable PTT ouput
int PTTHoldTimeComputer = 5;             // PTT hold time after computer CW in ms
int PTTHoldTimePaddle = 10;              // PTT hold time after paddle CW in dit lengths
int PTTTurnOffTime;                      // This gets set either to PTTHoldTimeComputer or PTTHoldTimePaddlle at the start of CW
int PTTState = 0;                        // Used to let everyone know if the PTT is "asserted" or not (regardkess of PTTIEnable)
boolean PTTAssertedByFootswitch = false; // PTT is being asserted by footswitch

boolean FootswitchPressedBefore = false; // Used for debouncing

int FSReading1 = 0;
int FSReading2 = 0;
int FSReading3 = 0;
int FSReading4 = 0;
int FSReading5 = 0;
int FSsum = 0;

boolean DebouncedFootswitchPressed = false; 

// variables used by keyer

int CWElementTimer = 0;
int PTTTurnOffTimer = 0;
int PTTTurnOnTimer = 0;
unsigned long CountsSinceLastCW = 0;

boolean DoingCharacterSpace = false;

int PaddleSendStatus = PADDLE_SEND_STATUS_NOTHING_BEING_SENT;

int CWSpeed = 30;
float Weight = 1.05;

int RadioSelect = 1;                    // 1=Radio1

int PaddleBugMode = 0;                  // 1=Bug mode
int FarnsworthSpeed = 0;                // 0=disabled
int FootswitchMode = 1;                 // 0-do nothing 1=control PTT

int TuneWithDits = 0;
int ElementAdder = 0;                   // ms offset to dit/dah/dat lengths to compensate for rig keying


int CWTone = 500;                       // Monitor tone for the CW currently being sent (zero = no tone)
int ComputerCWTone = 500;               // CW Pitch for computer sent CW


int PaddleTone = 700;                   // CW Pitch for paddle sent CW

int DitLength;


// Other stuff

char HostCommand = 0;   // Used to remember control character command frost host

// Here is the setup routine

void setup()

{
  pinMode(led, OUTPUT);                    // Initialize output for LED

  pinMode(dahInput, INPUT_PULLUP);
  pinMode(ditInput, INPUT_PULLUP);
  pinMode(pttInput, INPUT_PULLUP);

  pinMode(radioOneCWOutput, OUTPUT);
  pinMode(radioOnePTTOutput, OUTPUT);
  pinMode(radioTwoCWOutput, OUTPUT);
  pinMode(radioTwoPTTOutput, OUTPUT);

  pinMode(k1Output, OUTPUT);
  pinMode(k2Output, OUTPUT);
  pinMode(k3Output, OUTPUT);

  Timer1.initialize(1000);                 // Timer interrupt initialized with 1 ms period
  Timer1.attachInterrupt(TimerInterrupt);  // Timer interrupt service routine

  randomSeed (analogRead(0));              // Random number generator initialization

  CWBuffer.Head = 0;
  CWBuffer.Tail = 0;

  CharacterBuffer.Head = 0;
  CharacterBuffer.Tail = 0;

  //Serial.begin(115200);
  Serial.begin(19200);

  HostCommand = 0;

}


void Dit ()

// Put a dit (and dit space) into the CW sending buffer

{
  CWBuffer.CWBufferArray[CWBuffer.Head].Data = 1;
  CWBuffer.CWBufferArray[CWBuffer.Head].Time = 10;
  CWBuffer.Head++;
  if (CWBuffer.Head >= CWBufferSize) CWBuffer.Head = 0;

  CWBuffer.CWBufferArray[CWBuffer.Head].Data = 0;
  CWBuffer.CWBufferArray[CWBuffer.Head].Time = 10;
  CWBuffer.Head++;
  if (CWBuffer.Head >= CWBufferSize) CWBuffer.Head = 0;
}

void Dah ()

// Put a dah (and a dit space) into the CW sending buffer

{
  CWBuffer.CWBufferArray[CWBuffer.Head].Data = 1;
  CWBuffer.CWBufferArray[CWBuffer.Head].Time = 30;
  CWBuffer.Head++;
  if (CWBuffer.Head >= CWBufferSize) CWBuffer.Head = 0;

  CWBuffer.CWBufferArray[CWBuffer.Head].Data = 0;
  CWBuffer.CWBufferArray[CWBuffer.Head].Time = 10;
  CWBuffer.Head++;
  if (CWBuffer.Head >= CWBufferSize) CWBuffer.Head = 0;
}

void Dat ()

// A long dah - this was supported in TR Log - so I support it here too

{
  CWBuffer.CWBufferArray[CWBuffer.Head].Data = 1;
  CWBuffer.CWBufferArray[CWBuffer.Head].Time = 45;
  CWBuffer.Head++;
  if (CWBuffer.Head >= CWBufferSize) CWBuffer.Head = 0;

  CWBuffer.CWBufferArray[CWBuffer.Head].Data = 0;
  CWBuffer.CWBufferArray[CWBuffer.Head].Time = 10;
  CWBuffer.Head++;
  if (CWBuffer.Head >= CWBufferSize) CWBuffer.Head = 0;
}

void TriggerDisableDahMemory ()

// Might need some work as I changed from dit lengths in ms to quasi dit lengths

{
  switch ( CurtisMode )
  {
    case CURTIS_MODE_A:  { DisableDahMemory = 10 * (120.0 / CWSpeed); break; }
    case CURTIS_MODE_B:  { DisableDahMemory = 0; break; }
    case CURTIS_MODE_NL: { DisableDahMemory = 5 * (120.0 / CWSpeed); break; }
  }
}

void TriggerDisableDitMemory ()

// Might need some work asI changed from dit lengths in ms to quasi dit lengths

{
  switch ( CurtisMode )
  {
    case CURTIS_MODE_A:  { DisableDitMemory = 30 * (120.0 / CWSpeed); break; }
    case CURTIS_MODE_B:  { DisableDitMemory = 0; break; }
    case CURTIS_MODE_NL: { DisableDitMemory = 15 * (120.0 / CWSpeed); break; }
  }
}

void TurnOnPTT ()

// Just for safety - we turn off the PTT for the inactive radio

{
  CountsSinceLastCW = 0;
  PTTState = 1;
  switch ( RadioSelect )
  {
    case 1:
      {
        digitalWrite (radioTwoPTTOutput, LOW);
        if (PTTOutputEnable) { digitalWrite (radioOnePTTOutput, HIGH); }
#ifdef SEND_STATUS_BYTES
        Serial.write(sending1);
#endif        
        break;
      }
    case 2:
      {
        digitalWrite (radioOnePTTOutput, LOW);
        if (PTTOutputEnable) digitalWrite (radioTwoPTTOutput, HIGH);
#ifdef SEND_STATUS_BYTES
        Serial.write(sending2);
#endif        
        break;
      }
  }
}

void TurnOffPTT ()

// Just for safety - we turn off both rigs

{
  PTTState = 0;
  digitalWrite (radioOnePTTOutput, LOW);
  digitalWrite (radioTwoPTTOutput, LOW);
  CountsSinceLastCW = 0;
  CWSpeed = ComputerCWSpeed;  // In case someone messed it up with speed-up/dn
#ifdef SEND_STATUS_BYTES
  Serial.write(not_sending);
#endif

}

void TurnOnCW ()

// Just for safety - we turn off the CW for the inactive radio

{
  digitalWrite (led, HIGH);
  if (CWTone > 0) tone (speakerOutput, CWTone);

  switch ( RadioSelect )
  {
    case 1:
      {
        digitalWrite (radioTwoCWOutput, LOW);
        digitalWrite (radioOneCWOutput, HIGH);
        break;
      }
    case 2:
      {
        digitalWrite (radioOneCWOutput, LOW);
        digitalWrite (radioTwoCWOutput, HIGH);
        break;
      }
  }
}

void TurnOffCW ()

// Just for safety - we turn off both rigs

{
  digitalWrite (led, LOW);
  noTone (speakerOutput);
  digitalWrite (radioOneCWOutput, LOW);
  digitalWrite (radioTwoCWOutput, LOW);
}

void ExecuteCWCommand (char CWCommand)

// Here is a list of the supported commands:
//
// ;D - Long dah
// ;F - Speed up CW Speed 2 WPM
// ;H - Half dit space
// ;I - Switch headphones to inactive radio
// ;J - Switch headphones to active radio
// ;S - Slow CW Speed 2 WPM
// ;X - Switch headphones to R1
// ;Y - Switch headphones to R2
// ;Z - Put headphones in stereo

{
  // make sure any commands are in lower case so they match cases
      
  if ((CWCommand >= 'A') && (CWCommand <= 'Z'))
    CWCommand = CWCommand + ('a' - 'A'); 
  
  switch (CWCommand)
  {
    case 'd': { Dat(); break; }
    case 'f': { CWSpeed = CWSpeed + 2; break; }
    
    case 'h': 
    {
      CWBuffer.CWBufferArray[CWBuffer.Head].Data = 0;
      CWBuffer.CWBufferArray[CWBuffer.Head].Time = 1.5 * DitLength;
      CWBuffer.Head++;
      if (CWBuffer.Head >= CWBufferSize) CWBuffer.Head = 0;
      break; 
    }

    case 'i':   // Switch headphones to inactive radio
    {
      if (RadioSelect == 1)  // Radio 1 is active
      {
        digitalWrite (k1Output, HIGH);  // K1 On - Headphones to K2
        digitalWrite (k2Output, LOW);  //   K2 Off - Headphones to Radio 2 (if K1 on)
      }
      else
      {
        digitalWrite (k1Output, LOW); break;  // Headphones on rig 1
      }
      break;
    }

    case 'j':   // Switch headphones to active radio
    {
      if (RadioSelect == 1)  // Radio 1 is active
      {
        digitalWrite (k1Output, LOW);   // Headphones on rig 1  
      }
      else
      {
        digitalWrite (k1Output, HIGH);  // K1 On - Headphones to K2
        digitalWrite (k2Output, LOW);   // K2 Off - Headphones to Radio 2 (if K1 on)
      }
      break;
    }
    
    case 's': { CWSpeed = CWSpeed - 2; break; }

    case 'x': { digitalWrite (k1Output, LOW); break; } // Headphones on rig 1
    
    case 'y':   // Headphones on rig 2
    {
      digitalWrite (k1Output, HIGH);  // K1 On - Headphones to K2
      digitalWrite (k2Output, LOW);   // K2 Off - Headphones to Radio 2 (if K1 on)
      break;
    }

    case 'z':   // Headphones in stereo
    {
      digitalWrite (k1Output, HIGH);  // K1 On - Headphones to K2
      digitalWrite (k2Output, HIGH);  // K2 On  - Stereo Mode (if K1 on)
      break;
    }
            
  }  // end of switch
}


void CueUpNextCharacter()

// This gets called when we want to see if there is a letter left in the character buffer that
// needs to be converted into dits and dahs and put in the CW Buffer.
//
// SendMorseCharacter will put the CW elements into the CWBuffer.  The CWSendingEngine
// will notice there it something in the CWBuffer and start sending it the next time
// it gets called.
//
// We also allow for some commands to be embedded in the CW message - these will be detected
// here and whatever actions need to be taken will be taken.  These commands are all two byte
// sequences where the first letter is the ;
//
// Here is a list of the supported commands:
//
// ;D - Long dah
// ;F - Speed up CW Speed 2 WPM
// ;H - Half dit space (note that a ^ all be itself can also be used for this }
// ;I - Switch headphones to inactive radio
// ;J - Switch headphones to active radio
// ;S - Slow CW Speed 2 WPM
// ;X - Switch headphones to R1
// ;Y - Switch headphones to R2
// ;Z - Put headphones in stereo
//
 
{
  if (CharacterBuffer.Head != CharacterBuffer.Tail)  // We have a character to pull off the buffer
  {
    char CharToSend = CharacterBuffer.CharacterBufferArray [CharacterBuffer.Tail];

    CharacterBuffer.Tail++;
    if (CharacterBuffer.Tail >= CharacterBufferSize) CharacterBuffer.Tail = 0;

    if (WaitingForCWCommand)  // We had a ^ before and were waiting for the command to show up
    {
      ExecuteCWCommand (CharToSend);
      WaitingForCWCommand = false;
      return;
    }
    
    if (CharToSend == ';')   // ^ indicates this is a CW command - need a second byte
    {      
      if (CharacterBuffer.Head != CharacterBuffer.Tail)  // we have a command character on the buffer
      {    
      char Command = CharacterBuffer.CharacterBufferArray [CharacterBuffer.Tail];

      CharacterBuffer.Tail++;
      if (CharacterBuffer.Tail >= CharacterBufferSize) CharacterBuffer.Tail = 0;

      ExecuteCWCommand (Command); 
      return;        
               
      }   // end of getting second character off buffer
      else
      {
        // We are here because we found a ^ but the command is not yet in the CW buffer
        // We will remember this and wait for the character to show up

        WaitingForCWCommand = true;
        return;
      }
    }     // end of it char is ^ for command character
    
    else  // Normal CW character - go send it
    {
      SendMorseCharacter (CharToSend);
      PTTTurnOffTime = PTTHoldTimeComputer;  // This is in ms
    }
  }  // end of if something in the character buffer
}

  
void StartSendingNextCWElement ()

// Takes the next CW element off the CWBuffer and gets it started

{
  if (CWBuffer.Head != CWBuffer.Tail)      // There is something in the buffer to send
    {
      // The time in the CWBuffer = 10 for a dit time.  We have to factor in the CW Speed to get
      // the actual time.
      
      CWElementTimer = CWBuffer.CWBufferArray[CWBuffer.Tail].Time * (120.0 / CWSpeed);

      // We set the flag DoingCharacterSpace as in indication that we are near the end of this letter

      DoingCharacterSpace = bitRead (CWBuffer.CWBufferArray [CWBuffer.Tail].Data, 1);

      // Bit zero indicates if the key is up or down for this element

      if (bitRead (CWBuffer.CWBufferArray [CWBuffer.Tail].Data, 0))  // We test bit zero as other bits might be used for something
      {
        TurnOnCW ();
        CWElementTimer =  (CWElementTimer * Weight) + ElementAdder;
      }
      else
      {
        TurnOffCW ();
        CWElementTimer = (CWElementTimer * (1 / Weight)) - ElementAdder;
      }

      CWBuffer.Tail++;
      if (CWBuffer.Tail >= CWBufferSize) CWBuffer.Tail = 0;
    }
}

void CWSendingEngine()

// Expected to be called by the TimerInterupt once a milliscecond
//
// CWState is used here to implment a state machine

{
  // Take care of any timers 

  if ( DisableDitMemory > 0 )  { DisableDitMemory--; }
  if ( DisableDahMemory > 0 )  { DisableDahMemory--; }
  
  if (PTTState == 0) 
    {
      if (CountsSinceLastCW <= 1000000) CountsSinceLastCW++; 
    }
  else
    CountsSinceLastCW = 0;

  // CW Sending state machine
  
  switch (CWState)
  {
    case CWS_Idle:  // Nothing is happening - PTTState should be off
    {
      // Need something here to kick things off if someone just put stuff 
      // in the character buffer

      if ((CWBuffer.Head == CWBuffer.Tail) and (CharacterBuffer.Head != CharacterBuffer.Tail))
      {
        CueUpNextCharacter ();
      }
      
      if (CWBuffer.Head != CWBuffer.Tail)  // something here to send
      {
        // We need to deal with any PTT Turn On Time first

        if (PTTTurnOnTime > 0)
            {
              PTTTurnOnTimer = PTTTurnOnTime;
              CWState = CWS_WaitingForPTTTurnOn;
              TurnOnPTT ();
              return;
            }

        // No PTT Turn delay required

        TurnOnPTT ();
        StartSendingNextCWElement ();
        CWState = CWS_SendingElement;
        return;
      }

      // Still idle - nothing to do   

      // Let's make sure the PTT is off if it isn't forced on

      if ((!PTTForceOn) & (!PTTAssertedByFootswitch))     
      {
        digitalWrite (radioTwoPTTOutput, LOW);
        digitalWrite (radioOnePTTOutput, LOW);
      }
  
      break;
    }            // End of CWS_Idle case

    case CWS_WaitingForPTTTurnOn:
    {
      if (PTTTurnOnTimer > 0) PTTTurnOnTimer--;
      if (PTTTurnOnTimer > 0) return;  // not done yet

      // PTT Turn on timer done

      StartSendingNextCWElement ();
      CWState = CWS_SendingElement;
      break;
    }

    case CWS_SendingElement:
    {
      if (CWElementTimer > 0) CWElementTimer--;
      if (CWElementTimer > 0) return;  // we are not done yet

      // Done with current element - if there is more cue it up

      if (CWBuffer.Head != CWBuffer.Tail)
        {
          StartSendingNextCWElement ();
          return;
        }

      // Done with current CW character - let the paddle know

      PaddleSendStatus = PADDLE_SEND_STATUS_NOTHING_BEING_SENT;
      
      // Do we have something in the characgter buffer?

      if (CharacterBuffer.Head != CharacterBuffer.Tail)
        {
          CueUpNextCharacter (); // Gets it into the CW Buffer

          // In a special case - the character buffer might have ended with some kind
          // of command that does not put anything into the CW Buffer - so we need 
          // to handle that situation here

          if (CWBuffer.Tail == CWBuffer.Head)  // Nothing to send
            {
              PTTTurnOffTimer = PTTTurnOffTime;
              CWState = CWS_PTTTurningOff;
              return;
            }

         // Have a new character to send - it's in the CWBuffer now

         StartSendingNextCWElement ();
         return;
        }

        // Looks like we are really done sending CW
        
      PTTTurnOffTimer = PTTTurnOffTime;
      CWState = CWS_PTTTurningOff;
      break;
    }

    case CWS_PTTTurningOff:
    {
      if (CWBuffer.Head != CWBuffer.Tail) // someone put something in the CW Buffer to send
        {
          StartSendingNextCWElement ();
          CWState = CWS_SendingElement;
          return;
        }

      if (CharacterBuffer.Head != CharacterBuffer.Tail)  // computer sent us a character
        {
          CueUpNextCharacter ();

          if (CWBuffer.Tail != CWBuffer.Head)  // something showed up in the CW Buffer
            {
              StartSendingNextCWElement ();
              CWState = CWS_SendingElement;
            }    
          return;          
        }

      // Nothing new to send has shown up - keep counting down the PTTTurnOffTimer

      if (PTTTurnOffTimer > 0) PTTTurnOffTimer--;
      if (PTTTurnOffTimer > 0) return;   // not done yet

      // The PTTTurnOffTimer has expired with no new CW to send - shut things down

      TurnOffPTT ();
      CWState = CWS_Idle;
      break;      
    }    // end of CWS_PTTTurningOff case
    
  }  // end of switch for CWState
}

void CheckOnPaddleInputs()

  {
    // First - if nothing is being sent but someone triggered the dit or dah memory 
    
    if (PaddleSendStatus == PADDLE_SEND_STATUS_NOTHING_BEING_SENT)
    {
      if (DitMemory == 1)
      {
        Dit();
        PaddleSendStatus = PADDLE_SEND_STATUS_DIT_BEING_SENT;
        DitMemory = 0;
        TriggerDisableDahMemory ();
      }
      else
      {
        if (DahMemory == 1)
        {
          Dah();
          PaddleSendStatus = PADDLE_SEND_STATUS_DAH_BEING_SENT;
          DahMemory = 0;
          TriggerDisableDitMemory ();
        }
      }
    }

    // Deal with ultimatic mode special case

    if (CurtisMode == ULTIMATIC) 
    {
      // See if both dit and dah are pressed.
      
      if ((digitalRead (ditInput) == LOW) and (digitalRead (dahInput) == LOW))
      {
        if (PaddleSendStatus == PADDLE_SEND_STATUS_NOTHING_BEING_SENT)
        {
          if (UltimaticFirstElement == DitSentFirst)
          {
            Dah();
            PaddleSendStatus = PADDLE_SEND_STATUS_DAH_BEING_SENT;        
          }
          else
          {
            Dit();
            PaddleSendStatus = PADDLE_SEND_STATUS_DIT_BEING_SENT;
          }
        }  
      return;  // Do not go any further
      }

      if ((digitalRead (ditInput) == HIGH) and (digitalRead (dahInput) == HIGH))
      {
        UltimaticFirstElement = NoFirstElement;   // Clear remembering if we sent a dit or dah first
      }
      
    }

    
    if (digitalRead (ditInput) == LOW)
    {
      CWTone = PaddleTone;
      PTTTurnOffTime = PTTHoldTimePaddle * (1200 / CWSpeed);
      
      if (PaddleCWSpeed != 0) 
        CWSpeed = PaddleCWSpeed;
      else
        CWSpeed = ComputerCWSpeed;

      if (PTTState == 0)
      {
        TurnOnPTT ();
        PTTTurnOnTimer = PTTTurnOnTime;
      }

      switch (PaddleSendStatus)
      {
        case PADDLE_SEND_STATUS_NOTHING_BEING_SENT:
          {          
            CWBuffer.Head = CWBuffer.Tail;  // Abort any CW that was being sent from the host 
            CharacterBuffer.Head = CharacterBuffer.Tail;

            if (UltimaticFirstElement == NoFirstElement)
              UltimaticFirstElement = DitSentFirst;
         
            Dit();  // Put a dit in the CW Buffer 
            PaddleSendStatus = PADDLE_SEND_STATUS_DIT_BEING_SENT;
            TriggerDisableDahMemory ();          
            break;
          }

        case PADDLE_SEND_STATUS_DAH_BEING_SENT:
          {
            if (DisableDitMemory == 0)  DitMemory = 1;
            break;
          }
      }
    }

    if (digitalRead (dahInput) == LOW)
    {
      CWTone = PaddleTone;
      PTTTurnOffTime = PTTHoldTimePaddle * (1200 / CWSpeed);
      
      if (PaddleCWSpeed != 0) 
        CWSpeed = PaddleCWSpeed;
      else
        CWSpeed = ComputerCWSpeed;

      if (PTTState == 0)
      {
        TurnOnPTT ();
        PTTTurnOnTimer = PTTTurnOnTime;
      }

      switch (PaddleSendStatus)
      {
        case PADDLE_SEND_STATUS_NOTHING_BEING_SENT:
          {                     
            CWBuffer.Head = CWBuffer.Tail;      // Abort any CW that was being sent from the host
            CharacterBuffer.Head = CharacterBuffer.Tail;

            if (UltimaticFirstElement == NoFirstElement)
              UltimaticFirstElement = DahSentFirst;

            Dah();
            PaddleSendStatus = PADDLE_SEND_STATUS_DAH_BEING_SENT;
            TriggerDisableDitMemory ();
            break;
          }

        case PADDLE_SEND_STATUS_DIT_BEING_SENT:
          {
            if (DisableDahMemory == 0) DahMemory = 1;
            break;
          }
      }  // end of switch
    }
  }

void SendMorseCharacter (char morsechar)

{  
  CWTone = ComputerCWTone;

  if (PTTState == 0)
  {
    // We need to turn on the PTT
    TurnOnPTT ();
    PTTTurnOnTimer = PTTTurnOnTime;
    CWSpeed = ComputerCWSpeed;
  }
  
  if ((morsechar >= 'A') && (morsechar <= 'Z'))
    morsechar = morsechar + ('a' - 'A');  
  
  switch (morsechar) 
    {
      case ' ':  {
                 CWBuffer.CWBufferArray[CWBuffer.Head].Data = 0;
                 CWBuffer.CWBufferArray[CWBuffer.Head].Time = 3 * DitLength;
                 CWBuffer.Head++;            
                 if (CWBuffer.Head >= CWBufferSize) CWBuffer.Head = 0;
                 break; 
                 }

      case '_':  {
                 CWBuffer.CWBufferArray[CWBuffer.Head].Data = 0;
                 CWBuffer.CWBufferArray[CWBuffer.Head].Time = 3 * DitLength;
                 CWBuffer.Head++;            
                 if (CWBuffer.Head >= CWBufferSize) CWBuffer.Head = 0;
                 break; 
                 }

       case '^': {
                 CWBuffer.CWBufferArray[CWBuffer.Head].Data = 0;
                 CWBuffer.CWBufferArray[CWBuffer.Head].Time = 1.5 * DitLength;
                 CWBuffer.Head++;            
                 if (CWBuffer.Head >= CWBufferSize) CWBuffer.Head = 0;
                 break; 
                 }
           
      case '.': { Dit(); Dah(); Dit(); Dah(); Dit(); Dah(); break; }
      case ',': { Dah(); Dah(); Dit(); Dit(); Dah(); Dah(); break; }
      case '?': { Dit(); Dit(); Dah(); Dah(); Dit(); Dit(); break; }
      case '+': { Dit(); Dah(); Dit(); Dah(); Dit();        break; }
      case '<': { Dit(); Dit(); Dit(); Dah(); Dit(); Dah(); break; }
      case '=': { Dah(); Dit(); Dit(); Dit(); Dah();        break; }
      case '!': { Dit(); Dit(); Dit(); Dah(); Dit();        break; }
      case '&': { Dit(); Dah(); Dit(); Dit(); Dit();        break; } 
      
      case '-': { Dat();                             break; }   // longer dah
 
      case '/': { Dah(); Dit(); Dit(); Dah(); Dit(); break; }
      case '0': { Dah(); Dah(); Dah(); Dah(); Dah(); break; }
      case '1': { Dit(); Dah(); Dah(); Dah(); Dah(); break; }
      case '2': { Dit(); Dit(); Dah(); Dah(); Dah(); break; }
      case '3': { Dit(); Dit(); Dit(); Dah(); Dah(); break; }
      case '4': { Dit(); Dit(); Dit(); Dit(); Dah(); break; }
      case '5': { Dit(); Dit(); Dit(); Dit(); Dit(); break; }
      case '6': { Dah(); Dit(); Dit(); Dit(); Dit(); break; }
      case '7': { Dah(); Dah(); Dit(); Dit(); Dit(); break; }
      case '8': { Dah(); Dah(); Dah(); Dit(); Dit(); break; }
      case '9': { Dah(); Dah(); Dah(); Dah(); Dit(); break; }
      
      case 'a': { Dit(); Dah();                      break; }
      case 'b': { Dah(); Dit(); Dit(); Dit();        break; }
      case 'c': { Dah(); Dit(); Dah(); Dit();        break; }
      case 'd': { Dah(); Dit(); Dit();               break; }
      case 'e': { Dit();                             break; }
      case 'f': { Dit(); Dit(); Dah(); Dit();        break; }
      case 'g': { Dah(); Dah(); Dit();               break; }
      case 'h': { Dit(); Dit(); Dit(); Dit();        break; }
      case 'i': { Dit(); Dit();                      break; }
      case 'j': { Dit(); Dah(); Dah(); Dah();        break; }
      case 'k': { Dah(); Dit(); Dah();               break; }
      case 'l': { Dit(); Dah(); Dit(); Dit();        break; }
      case 'm': { Dah(); Dah();                      break; }
      case 'n': { Dah(); Dit();                      break; }
      case 'o': { Dah(); Dah(); Dah();               break; }
      case 'p': { Dit(); Dah(); Dah(); Dit();        break; }
      case 'q': { Dah(); Dah(); Dit(); Dah();        break; }
      case 'r': { Dit(); Dah(); Dit();               break; }
      case 's': { Dit(); Dit(); Dit();               break; }
      case 't': { Dah();                             break; }
      case 'u': { Dit(); Dit(); Dah();               break; }
      case 'v': { Dit(); Dit(); Dit(); Dah();        break; }
      case 'w': { Dit(); Dah(); Dah();               break; }
      case 'x': { Dah(); Dit(); Dit(); Dah();        break; }
      case 'y': { Dah(); Dit(); Dah(); Dah();        break; }
      case 'z': { Dah(); Dah(); Dit(); Dit();        break; }
      
  }       // end of switch

  // Now add a character space after the letter.  We assert bit 1 to indicate it is a character space
    
  CWBuffer.CWBufferArray[CWBuffer.Head].Data = 2;   // Special value to indicate end of character space
  CWBuffer.CWBufferArray[CWBuffer.Head].Time = 20;  // 3 sounds too long - appears TR used 2 dit lengths
  CWBuffer.Head++;
  if (CWBuffer.Head >= CWBufferSize) {CWBuffer.Head = 0; }
}


void SendMorseMessage (char *textptr)

{
  CWTone = ComputerCWTone;
  CWSpeed = ComputerCWSpeed;

  while (*textptr != 0)                                 //  until found nul character
  {
    SendMorseCharacter (*textptr);                      // Go send the character
    textptr++;                                          // Increment to next character
  }
}


void ProcessHostSerialCharacter (int hostchar)

  // Any bytes coming down from the host will be processed here.  If a character is a legal CW character, it will be added to the
  // CW Buffer.  If the character is a control character (01-1F) - then it will either be acted upon or remembered waiting for
  // a data byte to come next.

  // <01>  Send version # to host
  // <02>  SO2R relay commands
  // <03>  Computer CW sidetone
  // <04>  Paddle CW sidetone
  // <05>  Dit/dah orientation
  // <06>  Keyer weight
  // <07>  CW character offset time
  // <08>  CW Speed for computer CW
  // <09>  CW Speed for paddle CW (00 = track computer speed)
  // <0A>  PTT Control
  // <0B>  Radio select
  // <0C>  Query if CW still being sent
  // <0D>  Query # of characters left in CW buffer
  // <0E>  PTT assert time before CW
  // <0F>  PTT hold time for computer CW
  // <10>  PTT hold time for paddle CW
  // <11>  Footswitch state (01 = pressed)
  // <12>  Immediate stop sending now - clear PTT and buffer
  // <13>  Stop sending after letter is completed
  // <14>  Set Curtis keying mode (00=A  01=B  02=NL)
  // <15>  Paddle Bug Mode (00 = disaboed)
  // <16>  PTT Enable (00= diabled)
  // <17>  Tune with dots (use 12 to stop)
  // <18>  Fasrnswoth (00=desable >0 = speed)
  // <19>  Footswith to PTT enable (00= no ptt)
  // <1A>  Delete last character in send buffer (returns 01 if successful)

  {

    if (HostCommand != 0)   // We were waiting for a second byte of data to process
    {
      switch (HostCommand)
      {
        case 0x02:     // SO2R relay bit pattern
          {
            if bitRead (hostchar, 0)
              digitalWrite (k1Output, HIGH);  // K1 On - Headphones to K2
            else
              digitalWrite (k1Output, LOW);  // K1 Off - Headphones to Radio 1

            if bitRead (hostchar, 1)
              digitalWrite (k2Output, HIGH);  //  K2 On  - Stereo Mode (if K1 on)
            else
              digitalWrite (k2Output, LOW);  //   K2 Off - Headphones to Radio 2 (if K1 on)

            if bitRead (hostchar, 2)
              digitalWrite (k3Output, HIGH);  // K3 On - Mic to radio 2
            else
              digitalWrite (k3Output, LOW);  // K3 Off - Mic to radio 1
            break;  
          }

        case 0x03:    // Computer sent CW tone
          {
            ComputerCWTone = hostchar * 10;
            break;
          }

        case 0x04:    // Paddle CW tone
          {
            PaddleTone = hostchar * 10;
            break;
          }

        case 0x05:    // Paddle pin assignments
          {
            if (hostchar == 0)
            {
              dahInput = 7;
              ditInput = 6;
            }
            else
            {
              dahInput = 6;
              ditInput = 7;
            }
            break;
          }

        case 0x06:    // Keyer weight in percent
          {
            Weight = hostchar / 100.0;
            break;
          }

        case 0x07:    // CW Character Offset
          {
            ElementAdder = hostchar - 0x80;
            break;
          }

        case 0x08:   // Keyer speed for compute CW
          {
            ComputerCWSpeed = hostchar;
            CWSpeed = ComputerCWSpeed;
            break;
          }

        case 0x09:  // Keyer speed for paddle CW
          {
            PaddleCWSpeed = hostchar;
            CWSpeed = PaddleCWSpeed;
            break;
          }

        case 0x0A: // PTT forced on
          {
            PTTForceOn = hostchar;   // We will let the timer interupt take care of actually doing it            
            break;
          }

        case 0x0B: // Radio select
          {

            if (RadioSelect != hostchar)
            {
              CharacterBuffer.Head = CharacterBuffer.Tail;
              CWBuffer.Head = CWBuffer.Tail;
              TurnOffCW ();
              TurnOffPTT ();
              RadioSelect = hostchar;        
            }
            break;
          }

        case 0x0E: // PTT assert time before CW
          {
            PTTTurnOnTime = hostchar;
            break;
          }

        case 0x0F: // PTT hold time computer CW
          {
            PTTHoldTimeComputer = hostchar;
            break;
          }

        case 0x10:  // PTT hold time paddle CW
          {
            PTTHoldTimePaddle = hostchar;  // Hold time in dit lengths
            break;
          }

        case 0x14:  // Set Curtis keying mode
          {
            CurtisMode = hostchar;
            break;
          }

        case 0x15:  // Paddle Bug Mode
          {
            PaddleBugMode = hostchar;
            break;
          }

        case 0x16:  // PTT Enable
          {
            PTTOutputEnable = hostchar;
            break;
          }

        case 0x17:  // Tune with dits
          {
            TuneWithDits = hostchar;  // We will let the timer interrupt deal with this
            break;
          }

        case 0x18:
          {
            FarnsworthSpeed = hostchar;  // zero = disabled
            break;
          }

        case 0x19:
          {
            FootswitchMode = hostchar;  // 01 would make footswitch control PTT
            break;
          }          


          
      } // end of switch

      // Clear out control character memory

      HostCommand = 0;
      return;
    }

    if (hostchar >= ' ')   // Valid CW character - send to character buffer
    {
      CharacterBuffer.CharacterBufferArray [CharacterBuffer.Head] = hostchar;
      CharacterBuffer.Head++;
      if (CharacterBuffer.Head >= CharacterBufferSize)
      {
        CharacterBuffer.Head = 0;
      }
      CWSpeed = ComputerCWSpeed;

      // I used to do this - but it was causing the PTT to come on for things like ;Y      
      // TurnOnPTT ();   // Need to get PTT turned on ASAP 
      return;
    }

    else   // control character - this is a command byte
    {
      switch (hostchar)
      {
        case 0:      // ignored
          {
            HostCommand = 0;

            CharacterBuffer.CharacterBufferArray [CharacterBuffer.Head] = '4';
            CharacterBuffer.Head++;
            if (CharacterBuffer.Head >= CharacterBufferSize)
            {
              CharacterBuffer.Head = 0;
            }
            break;
          }

        case 0x01:   // send version back to host - we assume the host is listening for this
          {
            Serial.print ("TRCW V4");
            break;
          }

        case 0x0c:   // Query if CW still being sent  0=None 1=PTT hold time  2=CW
          {
            if (CWState == CWS_Idle)
            {          
              char temp = 0;
              Serial.write (temp);
              return;
            }

            if (CWState == CWS_PTTTurningOff)
            {
              char temp = 1;
              Serial.write (temp);
              return;
            }
            
            char temp = 2;    // CW engine is busyn
            Serial.write (temp);
            break;
          }

        case 0x0d:   // Query number of chars in CW buffer not counting one being sent
          {
            char NumberChars = 0;
            int TestTail;

            TestTail = CharacterBuffer.Tail;

            while (TestTail != CharacterBuffer.Head)
            {
              if (NumberChars < 255) { NumberChars++; }
              TestTail++;
              if (TestTail >= CharacterBufferSize) { TestTail = 0; }
            }
            
            Serial.write (NumberChars);
            break;         
          }

        case 0x11:   // Query footswitch state
          {
            char temp;

            if (DebouncedFootswitchPressed)
            {
              temp = 1;
              Serial.write (temp);
            }
            else
            {
              temp = 0;
              Serial.write (temp);
            }
            break;  
          }

        case 0x12:  // Immediate stop sending, clear buffer and PTT
        {
          CharacterBuffer.Head = CharacterBuffer.Tail;
          CWBuffer.Head = CWBuffer.Tail;            
          TurnOffCW ();
          TurnOffPTT ();
          break;
        }

        case 0x13:  // Stop stending after letter complete
        {
          CharacterBuffer.Head = CharacterBuffer.Tail;
          break;
        }

        case 0x1a:  // Delete last unsent character  returns 01 if one was deleted
        {
          char temp;
          
          if (CharacterBuffer.Head == CharacterBuffer.Tail)
          {
            temp = 0;
            Serial.write (temp);  // nothing in character buffer to delete
          }
          else
          {
            CharacterBuffer.Head--;
            if (CharacterBuffer.Head < 0) { CharacterBuffer.Head = CharacterBufferSize - 1; }
            temp = 1;
            Serial.write (temp);      
          }
          break;
        }

        case 0x1b:  // Return number of counts (250 ms / count)
          {
            char temp;
            int Milliseconds250 = CountsSinceLastCW / 250;

            temp = lowByte (Milliseconds250);
            Serial.write (temp);

            temp = highByte (Milliseconds250);
            Serial.write (temp);
            break;  
          }

        default:     // All other control characters require a data byte
          {
            HostCommand = hostchar;
            return;
          }

      }    // end of switch
    }
  }

void CheckOnFootswitch ()

// Expected to get called once a millisecond

{

  // First - we deal with the case where the footswitch is actually controlling the
  // PTT output for the selected radio.  We will only do this if someone has not set
  // PTTForceOn to be TRUE.

  if (!PTTForceOn)
  {
    if (FootswitchMode == 1) 
    {
    if (digitalRead (pttInput) == LOW)  // footswitch pressed
      {
        switch ( RadioSelect )
        {
          case 1:
            {
              digitalWrite (radioTwoPTTOutput, LOW);
              digitalWrite (radioOnePTTOutput, HIGH);
              #ifdef SEND_STATUS_BYTES
              Serial.write(sending1);
              #endif
              break;
            }
          case 2:
            {
              digitalWrite (radioOnePTTOutput, LOW);
              digitalWrite (radioTwoPTTOutput, HIGH);
              #ifdef SEND_STATUS_BYTES
              Serial.write(sending2);
              #endif
              break;
            }
        }  // end of switch
        
        PTTAssertedByFootswitch = true;
        
      }
  
    else   // footswitch is not pressed
      {
        if (PTTAssertedByFootswitch) 
        {
          if (PTTState == 0) TurnOffPTT ();
          PTTAssertedByFootswitch = false;
        }
        
        // We might have the PTT asserted from the footswitch and PTTState is active
        // but PTTOutputEnable is false - thus meaning we need to turn off the port
  
        if ((PTTState == 1) and (PTTOutputEnable == 0)) 
        {
          TurnOffPTT ();
        }
        
      }
    }
  }
  
  if (PTTForceOn)       // Someone wants the PTT on no matter what 
  {
    switch ( RadioSelect )
    {
      case 1:
        {
          digitalWrite (radioTwoPTTOutput, LOW);
          digitalWrite (radioOnePTTOutput, HIGH);
          #ifdef SEND_STATUS_BYTES
          Serial.write(sending1);
          #endif
          break;
        }
      case 2:
        {
          digitalWrite (radioOnePTTOutput, LOW);
          digitalWrite (radioTwoPTTOutput, HIGH);
          #ifdef SEND_STATUS_BYTES
          Serial.write(sending2);
          #endif
          break;
        }
    }                   
  }  
  
  // End of dealing with controlling the PTT - now let's update the debounced footswitch status
  // I know - it's kind of lazy to not remember what we read for the PTT - but it costs nothing to do it again

  // We used five readings in a row that are consisten to assert the footswitch
  // Shift the readings by one

  FSReading5 = FSReading4;
  FSReading4 = FSReading3;
  FSReading3 = FSReading2;
  FSReading2 = FSReading1;
  FSReading1 = digitalRead (pttInput);

  FSsum = FSReading5 + FSReading4 + FSReading3 + FSReading2 + FSReading1;

  DebouncedFootswitchPressed = (FSsum == 0);

}


void TimerInterrupt ()

// Stuff here will get called every millisecond

{
  CWSendingEngine();      // Check on the CW Sending Engine
  CheckOnPaddleInputs();  // Look at the paddle inputs
  CheckOnFootswitch();    // See if we need to update the PTT based upon the footswitch
}

  // the loop routine runs over and over again forever

void loop()

{
  noInterrupts ();
  
  while (Serial.available() > 0)  
  { 
    ProcessHostSerialCharacter (Serial.read());
  }

  interrupts ();
}
