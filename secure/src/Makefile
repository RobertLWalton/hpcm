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
#   $Date: 2000/08/29 10:01:47 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.2 $

# Kill all implicit rules
#
.SUFFIXES:

all: sandbox

sandbox:	sandbox.c
	gcc -o sandbox sandbox.c

clean:
	rm -f sandbox
