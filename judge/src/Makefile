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
#   $Author: hc3 $
#   $Date: 2001/01/10 10:54:53 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.5 $

# Kill all implicit rules
#
.SUFFIXES:

all: scorediff chkpage

scorediff:	scorediff.cc
	g++ -o scorediff scorediff.cc

chkpage:	chkpage.c
	gcc -o chkpage chkpage.c

clean:
	rm -f scorediff chkpage
