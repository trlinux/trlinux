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

PATH := $(PATH):/usr/share/texmf/bin
SHELL := env PATH=$(PATH) /bin/sh

HOMELIB=/usr/local/circuit_macros
M4=m4\
   $(HOMELIB)/gpic.m4\
   $(HOMELIB)/libgen.m4\
   $(HOMELIB)/libcct.m4\
   $(HOMELIB)/liblog.m4\
   $(HOMELIB)/lib3D.m4\
   $(HOMELIB)/darrow.m4

BASE=trloglinux

.SUFFIXES: .tex .m4 .jpg .eps .ps .pdf

$(BASE).pdf: $(BASE).tex
	find . -name \*eps -exec epstopdf {} \;
	pdflatex $(BASE)
	makeindex $(BASE)
	pdflatex $(BASE)
	pdflatex $(BASE)

$(BASE).dvi: $(BASE).tex
	latex $(BASE)
	latex $(BASE)
	latex $(BASE)

distclean:
	rm -f *\.pdf *\.log  *\.end
	rm -f *\.aux *\.toc *\.lof
	rm -f *\.bbl *\.blg *\.dvi
	rm -f *\.ilg *\.idx *\.ind

clean:
	rm -f *\.log  *\.end
	rm -f *\.aux *\.toc *\.lof
	rm -f *\.bbl *\.blg *\.dvi
	rm -f *\.ilg *\.idx *\.ind


.m4.tex:
	$(M4) $< |gpic -t > $@
	echo '\centerline{\box\graph}' >> $@

.tex.eps:
	echo $*
	echo $@
	echo "\documentclass[12pt]{article}" > temp.tex
	echo "\pagestyle{empty}" >> temp.tex
	echo "\begin{document}" >> temp.tex
	echo "\begin{figure}[h]" >> temp.tex
	echo "\include{$*}" >> temp.tex
	echo "\end{figure}" >> temp.tex
	echo "." >> temp.tex
	echo "\end{document}" >> temp.tex
	latex temp.tex
	dvips -E -o $@ temp.dvi

.eps.pdf:
	epstopdf $<

