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
#   $Author: hc3 $
#   $Date: 2000/09/18 02:24:52 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.5 $

# Kill all implicit rules
#
.SUFFIXES:

all: hpcm_sandbox hpcm_sendmail

hpcm_sandbox:	hpcm_sandbox.c
	gcc -o hpcm_sandbox hpcm_sandbox.c

hpcm_sendmail:	hpcm_sendmail.c
	gcc -o hpcm_sendmail hpcm_sendmail.c

clean:
	rm -f hpcm_sandbox hpcm_sendmail
