# HPCM Judging Source Code Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sun Jul  1 01:02:56 EDT 2001
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2001/07/01 05:13:30 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.6 $

# Kill all implicit rules
#
.SUFFIXES:

all: scorediff chkpage

scorediff:	scorediff.cc
	g++ -o scorediff scorediff.cc

scorediff_test.score:	scorediff2 \
			scorediff_test.out \
			scorediff_test.test \
			scorediff_test.reference
	rm -f scorediff_test.score
	scorediff2 scorediff_test.out \
	           scorediff_test.test \
		   > scorediff_test.score
	diff scorediff_test.score \
	     scorediff_test.reference

chkpage:	chkpage.c
	gcc -o chkpage chkpage.c

clean:
	rm -f scorediff chkpage scorediff_test.score
