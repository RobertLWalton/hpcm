# HPCM Judge's Bin Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Mon Sep  4 04:11:56 EDT 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: acm-cont $
#   $Date: 2000/09/04 09:17:59 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.2 $

all:	scorediff hpcm_sandbox

# Kill all implicit rules
#
.SUFFIXES:

# Location of secure hpcm_sandbox
#
SECURE_HPCM_SANDBOX=/sbin/hpcm_sandbox

scorediff:	../src/scorediff.cc
	rm -f scorediff
	(cd ../src; make scorediff)
	cp -p ../src/scorediff .
	chmod 500 scorediff

hpcm_sandbox:	../../secure/src/hpcm_sandbox.c
	rm -f hpcm_sandbox
	@if test -x ${SECURE_HPCM_SANDBOX}; \
        then make --no-print-directory \
	          secure_hpcm_sandbox; \
        else make --no-print-directory \
	          unsecure_hpcm_sandbox; fi

secure_hpcm_sandbox:
	ln -s ${SECURE_HPCM_SANDBOX} hpcm_sandbox
	@if test hpcm_sandbox -ot \
		 ../../secure/src/hpcm_sandbox.c; \
	    then echo WARNING: ${SECURE_HPCM_SANDBOX} \
	    	      is out of date; fi

unsecure_hpcm_sandbox:
	@echo ${SECURE_HPCM_SANDBOX} does not exist
	@echo Making UNSECURE hpcm_sandbox
	(cd ../../secure/src; make hpcm_sandbox)
	cp -p ../../secure/src/hpcm_sandbox .
	chmod 500 hpcm_sandbox

clean:
	rm -f scorediff hpcm_sandbox
