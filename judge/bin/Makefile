# HPCM Judge's Bin Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sat Jan 20 06:01:17 EST 2007
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2007/01/20 11:02:41 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.29 $

all:	scorediff hpcm_sandbox_x hpcm_print_passwords \
	hpcm_remote_sendmail \
	hpcm_email_passwords chkpage \
	hpcm_clisp.lsp hpcm_extract_test_replies \
	filtered_diff email_diff hpcm_helper help \
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
	(cd ../../secure/src; make hpcm_sandbox)
	ln -s ${SECURE_BIN}/hpcm_sandbox hpcm_sandbox_x
	@if cmp ${SECURE_BIN}/hpcm_sandbox.compile \
	        ../../secure/src/hpcm_sandbox; \
	 then x=x; \
	 else echo ===============================; \
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

hpcm_remote_sendmail:
	ln -s ../../contestant/bin/hpcm_remote_sendmail .

hpcm_clisp.lsp:
	ln -s ../../contestant/bin/hpcm_clisp.lsp .

hpcm_extract_test_replies:
	C=../../contestant/bin; \
	    ln -s $$C/hpcm_extract_test_replies .

filtered_diff:
	ln -s ../../contestant/bin/filtered_diff .

email_diff:
	ln -s ../../contestant/bin/email_diff .

hpcm_helper:
	ln -s ../../contestant/bin/hpcm_helper .

help:
	ln -s hpcm_help help

chkpage:	../src/chkpage.c
	rm -f chkpage
	(cd ../src; make chkpage)
	cp -p ../src/chkpage .
	chmod a-w,o-rwx chkpage

clean:
	rm -f scorediff hpcm_sandbox_x \
		hpcm_print_passwords chkpage \
		hpcm_email_passwords \
		hpcm_remote_sendmail \
		hpcm_clisp.lsp \
		hpcm_judging.rc \
		hpcm_extract_test_replies \
		filtered_diff \
		email_diff \
		hpcm_helper \
		help
