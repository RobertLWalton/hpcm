# HPCM Judge's Bin Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Mon Sep  4 11:05:15 EDT 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2000/10/07 10:12:27 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.6 $

all:	scorediff hpcm_sandbox_x hpcm_print_passwords

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

hpcm_sandbox_x:	../../secure/src/hpcm_sandbox.c
	rm -f hpcm_sandbox_x
	@if test -x ${SECURE_HPCM_SANDBOX}; \
        then make --no-print-directory \
	          secure_hpcm_sandbox_x; \
        else make --no-print-directory \
	          unsecure_hpcm_sandbox_x; fi

secure_hpcm_sandbox_x:
	ln -s ${SECURE_HPCM_SANDBOX} hpcm_sandbox_x
	@if test hpcm_sandbox_x -ot \
		 ../../secure/src/hpcm_sandbox.c; \
	    then echo ===============================; \
	         echo WARNING: ${SECURE_HPCM_SANDBOX} \
	    	      is out of date; \
	         echo ===============================; \
	    fi

unsecure_hpcm_sandbox_x:
	@echo ${SECURE_HPCM_SANDBOX} does not exist
	@echo ======================================
	@echo Making UNSECURE hpcm_sandbox_x
	@echo ======================================
	(cd ../../secure/src; make hpcm_sandbox)
	cp -p ../../secure/src/hpcm_sandbox \
	    hpcm_sandbox_x
	chmod 500 hpcm_sandbox_x

hpcm_print_passwords:
	if test ! -L hpcm_print_passwords; then \
	    ln -s \
	        ../../secure/bin/hpcm_print_passwords \
		.; \
	    fi

clean:
	rm -f scorediff hpcm_sandbox_x
