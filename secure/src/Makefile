# HPCM Secure Source Code Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Mon Sep 11 06:13:04 EDT 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: acm-cont $
#   $Date: 2000/09/11 10:19:46 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.4 $

# Kill all implicit rules
#
.SUFFIXES:

all: hpcm_sandbox

hpcm_sandbox:	hpcm_sandbox.c
	gcc -o hpcm_sandbox hpcm_sandbox.c

clean:
	rm -f hpcm_sandbox
