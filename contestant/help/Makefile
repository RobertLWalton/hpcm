# HPCM Contestant Help Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Wed Nov  8 00:51:40 EST 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2000/11/08 05:52:36 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.5 $

all:	emailsh

# Kill all implicit rules
#
.SUFFIXES:

emailsh:
	if test ! -r email/sh/hpcm_extract; then \
	    cd email/sh; \
	    ln -s ../../../bin/hpcm_extract .; \
	fi

clean:
