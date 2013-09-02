RELEASE=0.22

all:
	test 2 -eq `grep -c "Linux $(RELEASE)" src/versions.inc`
	(cd doc; make; make clean)
	(cd src; make; make install; make clean;\
            make -f Makefile.post; make clean)
	(cd log; strip trlog; strip post)
	
trlinux-r$(RELEASE).txz:
	( cd .. ; \
          strip trlinux/log/trlog;\
          strip trlinux/log/post;\
          mkdir trlinux-r$(RELEASE); \
          cp -r trlinux/doc trlinux-r$(RELEASE)/doc ;\
          cp -r trlinux/log trlinux-r$(RELEASE)/log ;\
          cp -r trlinux/files trlinux-r$(RELEASE)/files; \
          cp trlinux/RELEASE_NOTES trlinux-r$(RELEASE) ; \
          tar --exclude CVS --exclude TRMASTER.DTA.save --exclude TRMASTER.DTA\
              -cJf $@ trlinux-r$(RELEASE)/doc trlinux-r$(RELEASE)/log \
               trlinux-r$(RELEASE)/files trlinux-r$(RELEASE)/RELEASE_NOTES )
