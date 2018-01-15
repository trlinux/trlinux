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

#include <time.h>
#include <signal.h>
#include <sys/time.h>
#include <stdlib.h>
#include <fcntl.h>
#include <linux/kd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/prctl.h>
#include <unistd.h>
#include <locale.h>
#include <langinfo.h>
#include <sys/ioctl.h>
#include <string.h>

#ifndef CONSOLEONLY
#include <X11/Xlib.h>
#include <X11/keysym.h>

Display *display;
Window xtermwindow,root;
KeyCode shiftl,shiftr,controll,controlr;
int lctrlind,lctrlmask;
int rctrlind,rctrlmask;
int enterind,entermask;
short controlshift,altshiftl,altshiftr,focus,bothshift;
int ishift;
#endif
int console = 1;

short xfocused() {
#ifndef CONSOLEONLY
   Window root,focused;
   int size;
   Atom request,type;
   long nitems,bytesafter;
   unsigned char *prop;
   if (console) return 0x00;
   request=XInternAtom(display,"_NET_ACTIVE_WINDOW",0);
   root=XDefaultRootWindow(display);
   XGetWindowProperty(display,root,request,0,(~0L),0,AnyPropertyType
      ,&type,&size,&nitems,&bytesafter,&prop);
   focused = *((Window*) prop);
   free(prop);
   return (focused == xtermwindow)?0xff:0x00;
#else
   return 0x00;
#endif
}

void setupkeyboard() {
#ifndef CONSOLEONLY
   int i,j,minkc,maxkc,ksperkc;
   KeySym *ks;
   console = getenv("WINDOWID")?0:1;
   if (console) return;
   xtermwindow = (Window) strtol(getenv("WINDOWID"),0,0);
   display = XOpenDisplay(0x00);
   XDisplayKeycodes(display,&minkc,&maxkc);
   ks = XGetKeyboardMapping(display,minkc,maxkc-minkc+1,&ksperkc);
   for (j=0;j<ksperkc;j++) {
      for (i=0;i<maxkc-minkc+1;i++) {
         switch (ks[i*ksperkc+j]) {
            case (XK_Control_L):
               lctrlind = (i+minkc)/8;
               lctrlmask = 1 << ((i+minkc) % 8);
               break;
            case (XK_Control_R):
               rctrlind = (i+minkc)/8;
               rctrlmask = 1 << ((i+minkc) % 8);
               break;
            case (XK_Return):
               enterind = (i+minkc)/8;
               entermask = 1 << ((i+minkc) % 8);
               break;
         }
      }
   }
   XFree(ks);
   shiftl = XKeysymToKeycode(display,XK_Shift_L);
   shiftr = XKeysymToKeycode(display,XK_Shift_R);
   controll = XKeysymToKeycode(display,XK_Control_L);
   controlr = XKeysymToKeycode(display,XK_Control_R);
   XGrabKey(display,shiftl
      ,ControlMask,xtermwindow,True,GrabModeAsync,GrabModeAsync);

//   XGrabKey(display,shiftl
//      ,Mod1Mask,xtermwindow,True,GrabModeAsync,GrabModeAsync);
//   XGrabKey(display,shiftr
//      ,Mod1Mask,xtermwindow,True,GrabModeAsync,GrabModeAsync);

   XGrabKey(display,shiftl
      ,0,xtermwindow,True,GrabModeAsync,GrabModeAsync);
   XGrabKey(display,shiftr
      ,0,xtermwindow,True,GrabModeAsync,GrabModeAsync);

   XGrabKey(display,shiftr
      ,ShiftMask,xtermwindow,True,GrabModeAsync,GrabModeAsync);
   XGrabKey(display,shiftl
      ,ShiftMask,xtermwindow,True,GrabModeAsync,GrabModeAsync);
   XGrabKey(display,controll
      ,ShiftMask,xtermwindow,True,GrabModeAsync,GrabModeAsync);
   controlshift = 0x00;
   altshiftl = 0x00;
   altshiftr = 0x00;
   bothshift = 0x00;
   XSelectInput(display,xtermwindow,FocusChangeMask);
   focus = xfocused();
   ishift = 1; //shift key tuning
#endif
}

void shiftchange(int is) {
#ifndef CONSOLEONLY
   if (console) return;
   switch (ishift) {
      case (0):
         break;
      case (1):
         XUngrabKey(display,shiftl,0,xtermwindow);
         XUngrabKey(display,shiftr,0,xtermwindow);
         break;
      case (2):
         XUngrabKey(display,shiftl,Mod1Mask,xtermwindow);
         XUngrabKey(display,shiftr,Mod1Mask,xtermwindow);
         break;
   } 

   ishift = is;
   switch (ishift) {
      case (0): //No shift key tuning
         break;
      case (1):
         XGrabKey(display,shiftl,0,xtermwindow,True
            ,GrabModeAsync,GrabModeAsync);
         XGrabKey(display,shiftr,0,xtermwindow,True
            ,GrabModeAsync,GrabModeAsync);
      case (2):
         XGrabKey(display,shiftl,Mod1Mask,xtermwindow,True
            ,GrabModeAsync,GrabModeAsync);
         XGrabKey(display,shiftr,Mod1Mask,xtermwindow,True
            ,GrabModeAsync,GrabModeAsync);
    }
#endif
}

void updatestate() {
#ifndef CONSOLEONLY
   int i,n;
   XEvent ev;
   unsigned int state;
   if (console) return;
   n = XPending(display);
   for (i=0;i<n;i++) {
      XNextEvent(display,&ev);
      switch (ev.type) {
         case FocusIn:
            focus = 0xff;
            break;
         case FocusOut:
            focus = 0x00;
            controlshift = 0x00;
            altshiftl = 0x00;
            altshiftr = 0x00;
            bothshift = 0x00;
            break;
         case KeyPress:
            state = ev.xkey.state & (ControlMask|Mod1Mask|ShiftMask);
            if (ev.xkey.keycode == shiftl) {
               switch (state) {
                  case ControlMask:
                     controlshift = 0xff;
                     break;
                  case 0:
                  case Mod1Mask:
                     altshiftl = 0xff;
                     break;
                  case ShiftMask:
                     bothshift = 0xff;
                     break;
               }
            }
            else
            if (ev.xkey.keycode == shiftr) {
               switch (state) {
                  case 0:
                  case Mod1Mask:
                     altshiftr = 0xff;
                     break;
                  case ShiftMask:
                     bothshift = 0xff;
                     break;
               }
            }
            else
            if ((ev.xkey.keycode == controll) && (state == ShiftMask)) {
                controlshift = 0xff;
            }
            break;

         case KeyRelease:
            if (ev.xkey.keycode == shiftl) {
               controlshift = 0x00;
               altshiftl = 0x00;
               bothshift = 0x00;
            }
            else
            if (ev.xkey.keycode == shiftr) {
               controlshift = 0x00;
               altshiftr = 0x00;
               bothshift = 0x00;
            }
            else
            if (ev.xkey.keycode == controll) {
               controlshift = 0x00;
            }
            break;
      }
//      XAllowEvents(display, ReplayKeyboard, ev.xkey.time);
//      XFlush(display);
   }
#endif
}


short ctrlshift() {
#ifndef CONSOLEONLY
   if (console) return 0x00;
   updatestate();
   return controlshift;
#else
   return 0x00;
#endif
}

short ctrlenter() {
#ifndef CONSOLEONLY
   char keys[32];
   if (console) return 0x00;
   updatestate();
   if (focus) {
      XQueryKeymap(display,keys);
      if (((keys[rctrlind] & rctrlmask) | (keys[lctrlind] & lctrlmask))
         && (keys[enterind] & entermask))
         return 0xff;
   }
   return 0x00;
#else
   return 0x00;
#endif
}

short ctrl() {
#ifndef CONSOLEONLY
   char keys[32];
   if (console) return 0x00;
   updatestate();
   if (focus) {
      XQueryKeymap(display,keys);
      if ((keys[rctrlind] & rctrlmask) | (keys[lctrlind] & lctrlmask))
         return 0xff;
   }
   return 0x00;
#else
   return 0x00;
#endif
}

short ritshift() {
#ifndef CONSOLEONLY
   if (console) return 0x00;
   updatestate();
   if (focus) {
      if (bothshift) return 0x03;
      if (altshiftl) return 0x02;
      if (altshiftr) return 0x01;
   }
   return 0x00;
#else
   return 0x00;
#endif
}

short BYTDUPE(int *c, short n, int *a) {
    short i;
    for (i=0;i<n;i++) if (*c == a[i]) return 0xff;
    return 0;
}

short NUMBYTES(int *call1, int *call2) {
    short i = 0;
    int cmp;
    cmp = *call1 ^ *call2;
    if ((cmp & 0xff) == 0) i++;
    if ((cmp & 0xff00) == 0) i++;
    if ((cmp & 0xff0000) == 0) i++;
    if ((cmp & 0xff000000) == 0) i++;
    return i;
}

short BYTADDR(int *call, short ncall, int *carray) {
    short i;
    for (i=0;i<ncall;i++) if (*call == carray[i]) return i;
    return ncall;
}

static int fdconsole = -1;

void openconsole() {
   fdconsole = open("/dev/console", O_RDONLY|O_NONBLOCK);
}

void sounder(int hz) {
   if (fdconsole == -1) return;
   if (hz == 0) {
      ioctl(fdconsole, KIOCSOUND,0);
   } else {
      ioctl(fdconsole, KIOCSOUND, 1193180/hz);
   }
//   ioctl(fdconsole, KDMKTONE, (duration<<16)+(1193180/hz));
}

void linuxsoundon(int hz) {
    sounder(hz);
}

void linuxnosound() {
   sounder(0);
}

static short serial[4],parallel[3];

short serialaddress(short i) {
   if ((i < 1) || (i >4)) return 0;
   return serial[i-1];
}

short paralleladdress(short i) {
   if ((i < 1) || (i > 3)) return 0;
   return parallel[i-1];
}

void getlegacy() {
   int memfd,i;
   off_t off,offset = 0x400;
   short address[7];
   if ((memfd = open("/dev/mem",O_RDONLY)) == -1) {
      perror("legacy /dev/mem");
   }
   if ((off = lseek(memfd,offset,SEEK_SET)) == -1) {
      perror("legacy lseek");
   }

   if ((i = read(memfd,&address,7*sizeof(short))) == -1) {
      perror("legacy read");
   }
   serial[0] = address[0];
   serial[1] = address[1];
   serial[2] = address[2];
   serial[3] = address[3];
   parallel[0] = address[4];
   parallel[1] = address[5];
   parallel[2] = address[6];
}

void diewithparent() {
   prctl(PR_SET_PDEATHSIG,SIGKILL);
}

void hangupwithparent() {
   prctl(PR_SET_PDEATHSIG,SIGHUP);
}

void getdegree(char *d) {
   d[0] = (char) 0xb0; //default to iso8859
   d[1] = (char) 0x00;
   setlocale(LC_CTYPE,"");
   if (strcmp(nl_langinfo(CODESET),"UTF-8") == 0) {
      d[0] = (char) 0xc2;
      d[1] = (char) 0xb0;
      d[2] = (char) 0x00;
   }
}
