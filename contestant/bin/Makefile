# HPCM Contestant Bin Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Tue Feb 12 06:09:35 EST 2002
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/02/12 11:08:53 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.14 $

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
		   hpcm_submit print print2 fprint lpq \
		   printer_pipe scoreboard xcsh \
		   hpcm_clisp hpcm_extract_test_replies
	chmod a+r procmailrc hpcm_clisp.lsp
	chmod a+x .. ../.. ../../..

clean:
	rm -f hpcm_sandbox hpcm_sendmail
