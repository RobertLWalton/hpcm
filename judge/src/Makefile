# HPCM Judging Source Code Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Wed Feb  1 07:13:02 EST 2006
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2006/02/01 12:10:43 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.14 $

# Kill all implicit rules
#
.SUFFIXES:
%.out:	%

all: scorediff chkpage

scorediff:	scorediff.cc
	g++ -o scorediff scorediff.cc

test: scorediff_test1 scorediff_test2

scorediff_test1:	scorediff_test1.score

scorediff_test2:	scorediff_test2.score

scorediff_test1.score:	scorediff \
			scorediff_test1.out \
			scorediff_test1.test \
			scorediff_test1.reference
	rm -f scorediff_test1.score
	scorediff -debug \
	          -column 50 \
	          -float 0 0 50 \
		  -integer 0 0 50 \
		  scorediff_test1.out \
	          scorediff_test1.test \
		  > scorediff_test1.score
	diff scorediff_test1.score \
	     scorediff_test1.reference

scorediff_test2.score:	scorediff \
			scorediff_test2.out \
			scorediff_test2.test \
			scorediff_test2.reference
	rm -f scorediff_test2.score
	scorediff -debug \
		  -filtered \
	          -column 50 \
	          -float 0 0 50 \
		  -integer 0 0 50 \
		  scorediff_test2.out \
	          scorediff_test2.test \
		  > scorediff_test2.score
	diff scorediff_test2.score \
	     scorediff_test2.reference

chkpage:	chkpage.c
	gcc -o chkpage chkpage.c

clean:
	rm -f scorediff chkpage scorediff_test*.score
