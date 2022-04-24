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

RELEASE=0.51

all:
	test 2 -eq `grep -c "Linux $(RELEASE)" src/versions.inc`
	(cd doc; make clean; make; make clean)
	(cd src; make clean; make; make install; make clean;\
            make -f Makefile.post; make clean)
	(cd log; strip trlog; strip post)
	
trlinux-r$(RELEASE).txz:
	( cd .. ; \
          mkdir trlinux-r$(RELEASE); \
          cp -r trlinux/doc trlinux-r$(RELEASE)/doc ;\
          cp -r trlinux/log trlinux-r$(RELEASE)/log ;\
          cp -r trlinux/files trlinux-r$(RELEASE)/files; \
          cp trlinux/RELEASE_NOTES trlinux-r$(RELEASE) ; \
          tar --exclude CVS --exclude TRMASTER.DTA.save --exclude TRMASTER.DTA\
              -cJf $@ trlinux-r$(RELEASE)/doc trlinux-r$(RELEASE)/log \
               trlinux-r$(RELEASE)/files trlinux-r$(RELEASE)/RELEASE_NOTES )

trlinux-r$(RELEASE)_64bit.tgz:
	( cd .. ; \
          mkdir trlinux-r$(RELEASE); \
          cp -r trlinux/doc trlinux-r$(RELEASE)/doc ;\
          cp -r trlinux/log trlinux-r$(RELEASE)/log ;\
          cp -r trlinux/files trlinux-r$(RELEASE)/files; \
          cp trlinux/RELEASE_NOTES trlinux-r$(RELEASE) ; \
          tar --exclude CVS --exclude TRMASTER.DTA.save --exclude TRMASTER.DTA\
              -czf $@ trlinux-r$(RELEASE)/doc trlinux-r$(RELEASE)/log \
               trlinux-r$(RELEASE)/files trlinux-r$(RELEASE)/RELEASE_NOTES )

trlinuxsrc-r$(RELEASE).tgz:
	( cd .. ; \
          mkdir trlinuxsrc-r$(RELEASE); \
          cp -r trlinux/doc trlinuxsrc-r$(RELEASE)/doc ;\
          cp -r trlinux/log trlinuxsrc-r$(RELEASE)/log ;\
          cp -r trlinux/src trlinuxsrc-r$(RELEASE)/src;\
          cp -r trlinux/files trlinuxsrc-r$(RELEASE)/files; \
          cp trlinux/RELEASE_NOTES trlinuxsrc-r$(RELEASE) ; \
          cp trlinux/COPYING trlinuxsrc-r$(RELEASE) ; \
          cp trlinux/COPYRIGHT trlinuxsrc-r$(RELEASE) ; \
          tar --exclude CVS --exclude TRMASTER.DTA.save --exclude TRMASTER.DTA\
              -czf $@ trlinuxsrc-r$(RELEASE)/doc trlinuxsrc-r$(RELEASE)/log \
               trlinuxsrc-r$(RELEASE)/files \
               trlinuxsrc-r$(RELEASE)/RELEASE_NOTES trlinuxsrc-r$(RELEASE)/src )
