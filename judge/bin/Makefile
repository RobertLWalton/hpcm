# HPCM Judge's Bin Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Tue Feb 12 19:50:05 EST 2002
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/02/13 00:51:09 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.17 $

all:	scorediff hpcm_sandbox_x hpcm_print_passwords \
	hpcm_email_passwords chkpage hpcm_clisp \
	hpcm_clisp.lsp hpcm_extract_test_replies

# Kill all implicit rules
#
.SUFFIXES:

# Directory containing root setuid hpcm_sandbox
#
SECURE_BIN=/sbin

scorediff:	../src/scorediff.cc
	rm -f scorediff
	(cd ../src; make scorediff)
	cp -p ../src/scorediff .
	chmod 550 scorediff

hpcm_sandbox_x:	../../secure/src/hpcm_sandbox.c
	rm -f hpcm_sandbox_x
	@if test -x ${SECURE_BIN}/hpcm_sandbox; \
        then make --no-print-directory \
	          secure_hpcm_sandbox_x; \
        else make --no-print-directory \
	          unsecure_hpcm_sandbox_x; fi

secure_hpcm_sandbox_x:
	ln -s ${SECURE_BIN}/hpcm_sandbox hpcm_sandbox_x
	@if test hpcm_sandbox_x -ot \
		 ../../secure/src/hpcm_sandbox.c; \
	    then echo ===============================; \
	         echo WARNING: \
		      ${SECURE_BIN}/hpcm_sandbox \
	    	      is out of date; \
	         echo ===============================; \
	    fi

unsecure_hpcm_sandbox_x:
	@echo ${SECURE_BIN}/hpcm_sandbox does not exist
	@echo ======================================
	@echo Making UNSECURE hpcm_sandbox_x
	@echo ======================================
	(cd ../../secure/src; make hpcm_sandbox)
	cp -p ../../secure/src/hpcm_sandbox \
	    hpcm_sandbox_x
	chmod 550 hpcm_sandbox_x

hpcm_print_passwords:
	ln -s ../../secure/bin/hpcm_print_passwords .

hpcm_email_passwords:
	ln -s ../../secure/bin/hpcm_email_passwords .

hpcm_clisp:
	ln -s ../../contestant/bin/hpcm_clisp .

hpcm_clisp.lsp:
	ln -s ../../contestant/bin/hpcm_clisp.lsp .

hpcm_extract_test_replies:
	C=../../contestant/bin; \
	    ln -s $$C/hpcm_extract_test_replies .

chkpage:	../src/chkpage.c
	rm -f chkpage
	(cd ../src; make chkpage)
	cp -p ../src/chkpage .
	chmod 550 chkpage

clean:
	rm -f scorediff hpcm_sandbox_x \
		hpcm_print_passwords chkpage \
		hpcm_email_passwords \
		hpcm_clisp hpcm_clisp.lsp \
		hpcm_judging.rc \
		hpcm_extract_test_replies
