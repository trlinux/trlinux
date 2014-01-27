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
clean:
	rm -f *\.log  *\.end
	rm -f *\.aux *\.toc *\.lof
	rm -f *\.bbl *\.blg *\.dvi


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

