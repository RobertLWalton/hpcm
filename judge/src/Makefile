# HPCM Judging Source Code Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Mon Aug 28 15:58:49 EDT 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: acm-cont $
#   $Date: 2000/08/28 20:52:36 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.3 $

# Kill all implicit rules
#
.SUFFIXES:

all: scorediff

scorediff:	scorediff.cc
	g++ -o scorediff scorediff.cc

clean:
	rm -f scorediff
