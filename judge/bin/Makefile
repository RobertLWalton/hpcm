# HPCM Judge's Bin Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Fri Nov  1 02:01:56 EST 2002
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/11/01 07:02:58 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.22 $

all:	scorediff hpcm_sandbox_x hpcm_print_passwords \
	hpcm_email_passwords chkpage \
	hpcm_clisp.lsp hpcm_extract_test_replies \
	filtered_diff hpcm_helper javahelp \
	check_security

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
	chmod a-w,o-rwx scorediff

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

check_security:
	@if test ! -u hpcm_sandbox_x; \
	    then echo ===============================; \
	         echo WARNING: \
		      hpcm_sandbox_x \
	    	      is not set-user-id; \
		 ls -lL hpcm_sandbox_x; \
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
	chmod a-w,o-rwx hpcm_sandbox_x

hpcm_print_passwords:
	ln -s ../../secure/bin/hpcm_print_passwords .

hpcm_email_passwords:
	ln -s ../../secure/bin/hpcm_email_passwords .

hpcm_clisp.lsp:
	ln -s ../../contestant/bin/hpcm_clisp.lsp .

hpcm_extract_test_replies:
	C=../../contestant/bin; \
	    ln -s $$C/hpcm_extract_test_replies .

filtered_diff:
	ln -s ../../contestant/bin/filtered_diff .

hpcm_helper:
	ln -s ../../contestant/bin/hpcm_helper .

javahelp:
	ln -s ../../contestant/bin/javahelp .

chkpage:	../src/chkpage.c
	rm -f chkpage
	(cd ../src; make chkpage)
	cp -p ../src/chkpage .
	chmod a-w,o-rwx chkpage

clean:
	rm -f scorediff hpcm_sandbox_x \
		hpcm_print_passwords chkpage \
		hpcm_email_passwords \
		hpcm_clisp.lsp \
		hpcm_judging.rc \
		hpcm_extract_test_replies \
		filtered_diff
