# HPCM Judge's Bin Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sun Feb 22 14:03:53 EST 2009
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2009/02/22 20:55:12 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.31 $

all:	scorediff hpcm_sandbox_secure \
        hpcm_print_passwords \
	hpcm_remote_sendmail \
	hpcm_email_passwords chkpage \
	hpcm_clisp.lsp hpcm_extract_test_replies \
	filtered_diff email_diff hpcm_helper help

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

hpcm_sandbox_unsecure:	../../secure/src/hpcm_sandbox.c
	rm -f hpcm_sandbox_unsecure
	(cd ../../secure/src; make hpcm_sandbox)
	cp -p ../../secure/src/hpcm_sandbox \
	    hpcm_sandbox_unsecure
	chmod a-w,o-rwx hpcm_sandbox_unsecure
	make --no-print-directory \
	      hpcm_sandbox_check_date; \

hpcm_sandbox_secure:
	@if ! -x hpcm_sandbox_unsecure; \
	then make --no-print-directory \
	          hpcm_sandbox_unsecure; \
	fi
	@if test -x ${SECURE_BIN}/hpcm_sandbox; \
	then echo ln -s ${SECURE_BIN}/hpcm_sandbox \
		  hpcm_sandbox_secure; \
	     ln -s ${SECURE_BIN}/hpcm_sandbox \
		   hpcm_sandbox_secure; \
	     make --no-print-directory \
		   hpcm_sandbox_check_date; \
	fi
	make --no-print-directory \
	     hpcm_sandbox_check_security

hpcm_sandbox_check_date:
	@if test ! -r \
	    ${SECURE_BIN}/hpcm_sandbox.compiled; \
	then x=x; \
	elif cmp ${SECURE_BIN}/hpcm_sandbox.compiled \
	         hpcm_sandbox_unsecure; \
	then x=x; \
	else echo ===============================; \
	     echo WARNING: \
		  ${SECURE_BIN}/hpcm_sandbox \
		  is out of date; \
	     echo ===============================; \
	fi

hpcm_sandbox_check_security:
	@if test ! -u hpcm_sandbox_secure; \
	then echo ===============================; \
	     echo WARNING: hpcm_sandbox \
		  is not set-user-id; \
	     echo ===============================; \
	fi

hpcm_print_passwords:
	ln -s ../../secure/bin/hpcm_print_passwords .

hpcm_email_passwords:
	ln -s ../../secure/bin/hpcm_email_passwords .

hpcm_remote_sendmail:
	ln -s \
	   ../../contestant/bin/hpcm_remote_sendmail .

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
	rm -f scorediff hpcm_sandbox_unsecure \
	        hpcm_sandbox_secure \
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
