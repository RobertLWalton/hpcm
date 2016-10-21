# HPCM Contestant Bin Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Fri Oct 21 04:56:40 EDT 2016
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2013/10/19 08:46:59 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.34 $

all:	pdf \
        hpcm_sandbox hpcm_sendmail hpcm_finish \
        hpcm_display_graph \
	hpcm_display_segments modes

# Kill all implicit rules
#
.SUFFIXES:

pdf:	print
	ln -s print pdf

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

hpcm_finish:	../../secure/src/hpcm_finish.c
	rm -f hpcm_finish
	(cd ../../secure/src; make hpcm_finish)
	cp -p ../../secure/src/hpcm_finish .
	chmod 555 hpcm_finish
	chmod u+s hpcm_finish

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

modes:
	chmod a+rx . TRASH_ACCOUNT SETUP_PROTOTYPE \
	           help hpcm_extract hpcm_get \
		   hpcm_decode hpcm_less \
		   hpcm_submit print print2 fprint \
		   printer printer_pipe scoreboard \
		   hpcm_queue who_where \
		   hpcm_remote_sendmail \
		   printer_client printer_server \
		   xsh xcsh grant_access \
		   hpcm_test \
		   hpcm_extract_test_replies \
		   filtered_diff email_diff \
		   hpcm_helper javahelp c++help \
		   pythonhelp \
		   startup_helper elinks_helper \
		   authorize jdebug ldebug
	chmod a+r procmailrc \
	          elinks_startup_message
	chmod a+x .. ../.. ../../..

clean:
	rm -f hpcm_sandbox hpcm_sendmail hpcm_finish \
	      hpcm_display_graph hpcm_display_segments
