# HPCM Contestant Help Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sun Jan 13 20:29:06 EST 2002
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/01/14 15:34:18 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.6 $

all:	emailsh

# Kill all implicit rules
#
.SUFFIXES:

emailsh:
	if test ! -r email/sh/hpcm_extract; then \
	    cd email/sh; \
	    ln -s ../../../bin/hpcm_extract .; \
	fi
	if test ! -r email/sh/hpcm_submit; then \
	    cd email/sh; \
	    ln -s ../../../bin/hpcm_submit .; \
	fi

clean:
	rm email/sh/hpcm_extract email/sh/hpcm_submit
