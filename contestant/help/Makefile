# HPCM Contestant Help Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sat Sep 30 08:54:54 EDT 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2000/09/30 13:46:12 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.2 $

all:	modes

# Kill all implicit rules
#
.SUFFIXES:

modes:
	for x in common formal informal email; do \
	    if test -d $$x; then chmod a+r $$x/*; fi; \
	done

clean:
