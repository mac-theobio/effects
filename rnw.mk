%.pdf: %.Rnw
	R CMD Stangle $<
	R CMD Sweave $<
	pdflatex $*.tex
	bibtex $*
	pdflatex $*.tex
	pdflatex $*.tex
