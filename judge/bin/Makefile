# HPCM Judge's Bin Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sat Jul  8 15:37:23 EDT 2017
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2014/09/29 06:53:30 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.45 $

all:	scorediff grant_access \
	hpcm_sandbox_unsecure \
        hpcm_sandbox_secure \
        hpcm_print_passwords \
	hpcm_remote_sendmail \
	hpcm_queue \
	printer_client printer_server \
	hpcm_email_accounts chkpage \
	hpcm_extract_test_replies \
	filtered_diff email_diff hpcm_helper \
	hpcm_submit hpcm_test startup_helper \
	elinks_helper elinks_startup_message \
	help printer_pipe \
	hpcm_display \
	hpcm_display_graph \
	hpcm_display_segments \
	hpcm_display_polygon \
	jdebug ldebug hpcm_less pdf

# Kill all implicit rules
#
.SUFFIXES:

# Directory containing root setuid hpcm_sandbox
#
SECURE_BIN=/usr/sbin

scorediff:	../src/scorediff.cc
	rm -f scorediff
	(cd ../src; make scorediff)
	cp -p ../src/scorediff .
	chmod a-w,o-rwx scorediff

grant_access:
	ln -s \
	   ../../contestant/bin/grant_access .

hpcm_sandbox_unsecure:	../../secure/src/hpcm_sandbox.c
	rm -f hpcm_sandbox_unsecure
	(cd ../../secure/src; make hpcm_sandbox)
	cp -p ../../secure/src/hpcm_sandbox \
	    hpcm_sandbox_unsecure
	chmod a-w,o-rwx hpcm_sandbox_unsecure
	make --no-print-directory \
	      hpcm_sandbox_check_date; \

hpcm_sandbox_secure:	hpcm_sandbox_unsecure
	@if test -x ${SECURE_BIN}/hpcm_sandbox_secure; \
	then if test ! -x ./hpcm_sandbox_secure; \
	     then echo ln -s \
	            ${SECURE_BIN}/hpcm_sandbox_secure \
		    hpcm_sandbox_secure; \
	          ln -s \
		    ${SECURE_BIN}/hpcm_sandbox_secure \
		    hpcm_sandbox_secure; \
	     fi; \
	     make --no-print-directory \
		   hpcm_sandbox_check_date; \
	fi
	@make --no-print-directory \
	     hpcm_sandbox_check_security

hpcm_sandbox_check_date:	hpcm_sandbox_unsecure
	@if test ! -r \
	    ${SECURE_BIN}/hpcm_sandbox.compiled; \
	then x=x; \
	elif cmp ${SECURE_BIN}/hpcm_sandbox.compiled \
	         hpcm_sandbox_unsecure; \
	then x=x; \
	else echo ===============================; \
	     echo WARNING: \
		  ${SECURE_BIN}/hpcm_sandbox_secure \
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

hpcm_email_accounts:
	ln -s ../../secure/bin/hpcm_email_accounts .

hpcm_remote_sendmail:
	ln -s \
	   ../../contestant/bin/hpcm_remote_sendmail .

hpcm_queue:
	ln -s ../../contestant/bin/hpcm_queue .

printer_client:
	ln -s ../../contestant/bin/printer_client .

printer_server:
	ln -s ../../contestant/bin/printer_server .

hpcm_extract_test_replies:
	C=../../contestant/bin; \
	    ln -s $$C/hpcm_extract_test_replies .

filtered_diff:
	ln -s ../../contestant/bin/filtered_diff .

email_diff:
	ln -s ../../contestant/bin/email_diff .

hpcm_helper:
	ln -s ../../contestant/bin/hpcm_helper .

startup_helper:
	ln -s ../../contestant/bin/startup_helper .

elinks_helper:
	ln -s ../../contestant/bin/elinks_helper .

hpcm_submit:
	ln -s ../../contestant/bin/hpcm_submit .

hpcm_test:
	ln -s ../../contestant/bin/hpcm_test .

elinks_startup_message:
	ln -s ../../contestant/bin/elinks_startup_message .

help:
	ln -s hpcm_help help

chkpage:	../src/chkpage.c
	rm -f chkpage
	(cd ../src; make chkpage)
	cp -p ../src/chkpage .
	chmod a-w,o-rwx chkpage

printer_pipe:
	ln -s ../../contestant/bin/printer_pipe .

hpcm_display:
	ln -s ../../contestant/bin/hpcm_display .

hpcm_display_graph:
	ln -s ../../contestant/bin/hpcm_display_graph .

hpcm_display_segments:
	ln -s ../../contestant/bin/hpcm_display_segments .

hpcm_display_polygon:
	ln -s ../../contestant/bin/hpcm_display_polygon .

jdebug:
	ln -s ../../contestant/bin/jdebug .

ldebug:
	ln -s ../../contestant/bin/ldebug .

hpcm_less:
	ln -s ../../contestant/bin/hpcm_less .

pdf:
	ln -s ../../contestant/bin/pdf .

clean:
	rm -f scorediff grant_access \
		hpcm_sandbox_unsecure \
	        hpcm_sandbox_secure \
		hpcm_print_passwords chkpage \
		hpcm_email_accounts \
		hpcm_remote_sendmail \
		hpcm_queue \
		hpcm_judging.rc \
		hpcm_extract_test_replies \
		filtered_diff \
		email_diff \
		hpcm_helper \
		hpcm_submit \
		hpcm_test \
		startup_helper \
		elinks_helper \
		elinks_startup_message \
		help \
		printer_pipe \
		hpcm_display_graph \
		hpcm_display_segments \
		jdebug ldebug hpcm_less
