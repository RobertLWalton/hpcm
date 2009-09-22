# HPCM Contestant Bin Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Tue Sep 22 05:19:04 EDT 2009
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2009/09/22 09:19:27 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.22 $

all:	hpcm_sandbox hpcm_sendmail clisp modes

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

clisp:
	rm -f clisp
	ln -s `which clisp` clisp

modes:
	chmod a+rx . TRASH_ACCOUNT SETUP_PROTOTYPE \
	           help hpcm_extract hpcm_get \
		   hpcm_submit print print2 fprint \
		   printer printer_pipe scoreboard \
		   xcsh hpcm_clisp \
		   hpcm_extract_test_replies \
		   filtered_diff email_diff \
		   hpcm_helper javahelp stlhelp \
		   startup_helper w3m_helper
	chmod a+r procmailrc hpcm_clisp.lsp \
	          w3m_startup_message
	chmod a+x .. ../.. ../../..

clean:
	rm -f hpcm_sandbox hpcm_sendmail clisp
