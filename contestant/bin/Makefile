# HPCM Contestant Bin Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Mon Sep  4 04:08:14 EDT 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2000/09/18 08:30:21 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.2 $

all:	hpcm_sandbox modes

# Kill all implicit rules
#
.SUFFIXES:

hpcm_sandbox:	../../secure/src/hpcm_sandbox.c
	rm -f hpcm_sandbox
	(cd ../../secure/src; make hpcm_sandbox)
	cp -p ../../secure/src/hpcm_sandbox .
	chmod 555 hpcm_sandbox

modes:
	chmod a+rx TRASH_ACCOUNT help hpcm_sandbox \
		   hpcm_sendmail hpcm_submit print \
		   print2 scoreboard xcsh

clean:
	rm -f hpcm_sandbox
