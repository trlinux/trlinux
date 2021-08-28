#Copyright Larry Tyree, N6TR, 2011,2012,2013,2014,2015.

#This file is part of TR log for linux.

#TR log for linux is free software: you can redistribute it and/or
#modify it under the terms of the GNU General Public License as
#published by the Free Software Foundation, either version 2 of the
#License, or (at your option) any later version.

#TR log for linux is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#You should have received a copy of the GNU General
#    Public License along with TR log for linux.  If not, see
#<http://www.gnu.org/licenses/>.

PC=fpc
CFLAGS=-O2

SRC=\
   xkb.pas\
   beep.pas\
   foot.pas\
   keyers.pas\
   keyerk1ea.pas\
   keyerwin.pas\
   keyeryccc.pas\
   radio.pas\
   scorereporter.pas\
   cfgcmd.pas\
   cfgdef.pas\
   colorcfg.pas\
   country9.pas\
   fcontest.pas\
   jctrl1.pas\
   jctrl2.pas\
   k1eanet.pas\
   logcfg.pas\
   logcw.pas\
   logddx.pas\
   logdom.pas\
   logdupe.pas\
   logdvp.pas\
   logedit.pas\
   loggrid.pas\
   loghelp.pas\
   loghp.pas\
   logk1ea.pas\
   logmenu.pas\
   logpack.pas\
   logprom.pas\
   logscp.pas\
   logsort.pas\
   logstuff.pas\
   logsubs1.pas\
   logsubs2.pas\
   logwae.pas\
   logwind.pas\
   postcfg.pas\
   remmults.pas\
   slowtree.pas\
   tr.pas\
   tree.pas\
   memlinux.pas\
   linuxsound.pas\
   communication.pas\
   lowlatency.pas\
   ieee1284.pas\
   so2r.pas\
   timer.pas\
   trcrt.pas\
   ycccprotocol.pas\
   zonecont.pas\
   portname.pas\
   rig.pas\
   kenwood.pas\
   icom.pas\
   rigctld.pas\
   datetimec.pas

trlog: $(SRC) hidapi.h hid.o dupe.o dt.o findports.o scoreposter.o
	 $(PC) -O2 -B -Mobjfpc -g -gl -otrlog tr

ccode: hidapi.h hid.o dupe.o dt.o findports.o scoreposter.o curlroutines.o

install:
#	sudo install -m 4755 trlog ../log/trlog
	install -m 755 trlog ../log/trlog

clean:
	rm -f *\.o *\.ppu *\.res

distclean:
	rm -f *\.rst *\.o *\.ppu *\.res trlog post

dupe.o:dupe.c

hid.o:hid.c

dt.o:dt.c

scoreposter.o:scoreposter.c

findports.o:findports.c

dupepas.o:dupepas.pas
	$(PC) -O2 dupepas.pas

tar:
	rm -f *\.o *\.ppu tr
	( cd ..; tar --exclude CVS -czf trlinux_$(RELEASE)_src.tgz src )

