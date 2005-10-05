# HPCM Contestant Bin Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Wed Oct  5 07:52:01 EDT 2005
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2005/10/05 11:51:16 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.18 $

all:	hpcm_sandbox hpcm_sendmail modes

# Kill all implicit rules
#
.SUFFIXES:

hpcm_sandbox:	../../secure/src/hpcm_sandbox.c
	rm -f hpcm_sandbox
	(cd ../../secure/src; make hpcm_sandbox)
	cp -p ../../secure/src/hpcm_sandbox .
	chmod 555 hpcm_sandbox

hpcm_sendmail:	../../secure/src/hpcm_sendmail.c
	rm -f hpcm_sendmail
	(cd ../../secure/src; make hpcm_sendmail)
	cp -p ../../secure/src/hpcm_sendmail .
	chmod 555 hpcm_sendmail
	chmod u+s hpcm_sendmail

modes:
	chmod a+rx . TRASH_ACCOUNT SETUP_PROTOTYPE \
	           help hpcm_extract hpcm_get \
		   hpcm_submit print print2 fprint \
		   printer printer_pipe scoreboard \
		   xcsh hpcm_clisp \
		   hpcm_extract_test_replies \
		   filtered_diff hpcm_helper javahelp
	chmod a+r procmailrc hpcm_clisp.lsp
	chmod a+x .. ../.. ../../..

clean:
	rm -f hpcm_sandbox hpcm_sendmail
