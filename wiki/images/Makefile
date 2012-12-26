# Makefile to make hpcm/wiki graphic images using tikz
#
#	File:	Makefile
#	Author:	walton@seas.harvard.edu
#	Date:	Tue Dec 25 20:58:07 EST 2012

.SUFFIXES:	.tex .png

HOST=problems1.seas.harvard.edu

all:
	@if test "${HOST}" == "`hostname -f`"; \
	then \
	    chmod a+r *.tex Makefile; \
	    chmod -f a+r *.png; \
	    chmod a+x . .. ../.. ../../..; \
	    chmod a+r .; \
	    shopt -s nullglob; \
	    ls -l *.tex Makefile *.png; \
	fi


2d-points:	2d-points-1.png

2d-directions:	2d-directions-1.png

2d-translation:	2d-translation-1.png

2d-sum-of-translations:	2d-sum-of-translations-1.png

2d-scalar-products-of-translation:	\
	2d-scalar-products-of-translation-1.png

2d-vector-addition:	2d-vector-addition-1.png

2d-vector-subtraction:	2d-vector-subtraction-1.png

2d-vector-rotation:	2d-vector-rotation-1.png

2d-rotation-coordinates:	\
	2d-rotation-coordinates-1.png

2d-point-to-line-segment:	\
	2d-point-to-line-segment-1.png

2d-vector-scalar-product:	\
	2d-vector-scalar-product-1.png


%-1.png:	%.tex
	rm -f $*.pdf $*-*.png
	pdflatex $*.tex
	pdftoppm -png $*.pdf $*
clean:
	rm -f *.aux *.log *.pdf *.png
