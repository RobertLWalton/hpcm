# HPCM Judging Source Code Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Wed Apr 24 11:55:30 EDT 2002
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/04/24 16:02:16 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.9 $

# Kill all implicit rules
#
.SUFFIXES:

all: scorediff chkpage

scorediff:	scorediff.cc
	g++ -o scorediff scorediff.cc

scorediff_test.score:	scorediff \
			scorediff_test.out \
			scorediff_test.test \
			scorediff_test.reference
	rm -f scorediff_test.score
	scorediff -column 50 -float 0 0 50 -integer 0 0 50 \
		   scorediff_test.out \
	           scorediff_test.test \
		   > scorediff_test.score
	diff scorediff_test.score \
	     scorediff_test.reference

chkpage:	chkpage.c
	gcc -o chkpage chkpage.c

clean:
	rm -f scorediff chkpage scorediff_test.score
