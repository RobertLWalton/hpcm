.SUFFIXES:	.tex .png

TARGETS:	2d-points 2d-directions

2d-points:	2d-points-1.png

2d-directions:	2d-directions-1.png

2d-translation:	2d-translation-1.png

%-1.png:	%.tex
	rm -f $*.pdf $*-*.png
	pdflatex $*.tex
	pdftoppm -png $*.pdf $*
clean:
	rm -f *.aux *.log *.pdf *.png
