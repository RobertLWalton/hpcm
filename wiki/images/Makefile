# Makefile to make hpcm/wiki graphic images using tikz
#
#	File:	Makefile
#	Author:	walton@seas.harvard.edu
#	Date:	Thu Mar 28 10:10:09 EDT 2013

.SUFFIXES:	.tex .png

HOST=problems1.seas.harvard.edu

all:
	@H=`hostname`; \
            if test `expr "${HOST}" : "$$H"` -gt 0; \
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

2d-polar-coordinates:	\
	2d-polar-coordinates-1.png

2d-vector-rotation:	2d-vector-rotation-1.png

2d-rotation-coordinates:	\
	2d-rotation-coordinates-1.png

2d-point-to-line-segment:	\
	2d-point-to-line-segment-1.png

2d-vector-scalar-product:	\
	2d-vector-scalar-product-1.png

2d-point-to-line-segment-2:	\
	2d-point-to-line-segment-2-1.png

probability-dice-markov-process:	\
	probability-dice-markov-process-1.png

probability-pagerank-markov-process:	\
	probability-pagerank-markov-process-1.png

text-parse-tree:	\
	text-parse-tree-1.png

text-abstract-tree:	\
	text-abstract-tree-1.png

binary-tree-paths:	\
	binary-tree-paths-1.png

binary-tree-sorted:	\
	binary-tree-sorted-1.png

binary-tree-heap:	\
	binary-tree-heap-1.png

binary-tree-balancing:	\
	binary-tree-balancing-1.png

binary-tree-rotations:	\
	binary-tree-rotations-1.png

binary-tree-pointer-rotation:	\
	binary-tree-pointer-rotation-1.png


%-1.png:	%.tex
	rm -f $*.pdf $*-*.png
	pdflatex $*.tex
	pdftoppm -png -r 100 $*.pdf $*
clean:
	rm -f *.aux *.log *.pdf *.png
