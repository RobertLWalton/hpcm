# HPCM Judging Source Code Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sun Apr  3 21:53:02 EDT 2016
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.

# Kill all implicit rules
#
.SUFFIXES:
%.out:	%

all: scorediff chkpage

scorediff:	scorediff.cc
	g++ -o scorediff scorediff.cc

test: scorediff_test1.score scorediff_test2.score

scorediff_test1.score: \
    OPTIONS=-debug -column 50 \
            -float 0 0 50 -integer 0 0 50

scorediff_test2.score: \
    OPTIONS=-debug -filtered -column 50 \
	           -float 0 0 50 -integer 0 0 50

scorediff_test3.score: \
    OPTIONS=-filtered -column 50

%.score:	scorediff %.out %.test %.reference
	rm -f $*.score
	scorediff ${OPTIONS} $*.out $*.test > $*.score
	diff -u $*.score $*.reference

chkpage:	chkpage.c
	gcc -o chkpage chkpage.c

clean:
	rm -f scorediff chkpage scorediff_test*.score
