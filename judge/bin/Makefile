# Programming Contest Bin Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sat Aug 19 05:59:26 EDT 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: acm-cont $
#   $Date: 2000/08/19 16:13:34 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.1 $

all:	scorediff

# Kill all implicit rules
#
.SUFFIXES:

scorediff:	../src/scorediff.cc
	rm -f scorediff
	(cd ../src; make scorediff)
	cp -p ../src/scorediff .
	chmod 500 scorediff

clean:
	rm -f scorediff
