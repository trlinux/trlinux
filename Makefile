RELEASE=0.37

all:
	test 2 -eq `grep -c "Linux $(RELEASE)" src/versions.inc`
	(cd doc; make; make clean)
	(cd src; make; make install; make clean;\
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
          tar --exclude CVS --exclude TRMASTER.DTA.save --exclude TRMASTER.DTA\
              -czf $@ trlinuxsrc-r$(RELEASE)/doc trlinuxsrc-r$(RELEASE)/log \
               trlinuxsrc-r$(RELEASE)/files \
               trlinuxsrc-r$(RELEASE)/RELEASE_NOTES trlinuxsrc-r$(RELEASE)/src )
