# HPCM Secure Source Code Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Mon Aug 28 15:58:23 EDT 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: acm-cont $
#   $Date: 2000/09/03 19:45:33 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.3 $

# Kill all implicit rules
#
.SUFFIXES:

all: hpcm_sandbox

hpcm_sandbox:	hpcm_sandbox.c
	gcc -o hpcm_sandbox hpcm_sandbox.c

clean:
	rm -f hpcm_sandbox

test:
	-exit 1
