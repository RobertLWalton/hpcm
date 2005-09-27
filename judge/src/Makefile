# HPCM Judging Source Code Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Tue Sep 27 02:02:43 EDT 2005
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2005/09/27 05:57:31 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.11 $

# Kill all implicit rules
#
.SUFFIXES:

all: scorediff chkpage

scorediff:	scorediff.cc
	g++ -o scorediff scorediff.cc

test: scorediff_test1

scorediff_test1:	scorediff_test1.score

scorediff_test1.score:	scorediff \
			scorediff_test1.out \
			scorediff_test1.test \
			scorediff_test1.reference
	rm -f scorediff_test1.score
	scorediff -column 50 \
	          -float 0 0 50 \
		  -integer 0 0 50 \
		  scorediff_test1.out \
	          scorediff_test1.test \
		  > scorediff_test1.score
	diff scorediff_test1.score \
	     scorediff_test1.reference

chkpage:	chkpage.c
	gcc -o chkpage chkpage.c

clean:
	rm -f scorediff chkpage scorediff_test*.score
