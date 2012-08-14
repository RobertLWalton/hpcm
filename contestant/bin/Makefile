# HPCM Contestant Bin Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Mon Aug 13 23:14:26 EDT 2012
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2012/08/14 03:18:08 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.29 $

all:	hpcm_sandbox hpcm_sendmail hpcm_display_graph \
	hpcm_display_segments clisp modes

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

hpcm_display_graph:	../src/hpcm_display_graph.cc
	rm -f hpcm_display_graph
	(cd ../src; make hpcm_display_graph)
	cp -p ../src/hpcm_display_graph .
	chmod 555 hpcm_display_graph

hpcm_display_segments:	../src/hpcm_display_segments.cc
	rm -f hpcm_display_segments
	(cd ../src; make hpcm_display_segments)
	cp -p ../src/hpcm_display_segments .
	chmod 555 hpcm_display_segments

clisp:
	rm -f clisp
	ln -s `which clisp` clisp

modes:
	chmod a+rx . TRASH_ACCOUNT SETUP_PROTOTYPE \
	           help hpcm_extract hpcm_get \
		   hpcm_decode \
		   hpcm_submit print print2 fprint \
		   printer printer_pipe scoreboard \
		   xsh xcsh grant_access hpcm_clisp \
		   hpcm_extract_test_replies \
		   filtered_diff email_diff \
		   hpcm_helper javahelp stlhelp \
		   startup_helper elinks_helper \
		   authorize
	chmod a+r procmailrc hpcm_clisp.lsp \
	          elinks_startup_message
	chmod a+x .. ../.. ../../..

clean:
	rm -f hpcm_sandbox hpcm_sendmail \
	      hpcm_display_graph hpcm_display_segments \
	      clisp
