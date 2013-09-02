SRC=\
   xkb.pas\
   beep.pas\
   foot.pas\
   keyers.pas\
   keyerk1ea.pas\
   keyerwin.pas\
   keyeryccc.pas\
   radio.pas\
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
   ieee1284.pas\
   timer.pas\
   trcrt.pas\
   zonecont.pas\
   portname.pas\
   datetimec.pas


trlog: $(SRC) dupe.o dt.o findports.o
#	fpc -Xt -B -Mtp -g -otrlog tr\
             -k-logg -k-lvorbis -k-lFLAC -k-lvorbisenc -k-lm
#	fpc -dBUSYWAIT -B -Mtp -g -otrlog tr -k-rpath=\$$ORIGIN/lib -k'-z origin' -k-lX11
	fpc -O2 -B -Mtp -g -otrlog tr -k-rpath=\$$ORIGIN/lib -k'-z origin' -k-lX11

install:
#	sudo install -m 4755 trlog ../log/trlog
	install -m 755 trlog ../log/trlog

clean:
	rm -f *\.o *\.ppu

distclean:
	rm -f *\.rst *\.o *\.ppu trlog post

dupe.o:dupe.c
	gcc -c dupe.c

findports.o:findports.c
	gcc -c findports.c

dupepas.o:dupepas.pas
	fpc -O2 dupepas.pas

tar:
	rm -f *\.o *\.ppu tr
	( cd ..; tar --exclude CVS -czf trlinux_$(RELEASE)_src.tgz src )

