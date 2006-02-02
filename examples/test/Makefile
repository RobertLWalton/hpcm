# Makefile for testing.
#
# File:		Makefile
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Thu Feb  2 05:11:14 EST 2006
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2006/02/02 10:09:06 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.8 $

# Kill all implicit rules
#
.SUFFIXES:

all:	setup test diff

setup:
	@echo SETUP
	makecontest

test:
	@echo RECEIVING
	@for x in test_mail/*/Received_Mail; \
	    do \
	    receivemail mail <$$x | dispatchmail mail; \
	    done

diff:
	@echo DIFFING
	@for x in test_mail/*/*; \
	    do \
	    f=`basename $$x`; \
	    if test "$$f" == "CVS"; then continue; fi; \
	    if test "$$f" == "Received_Mail"; \
	       then continue; fi; \
	    y=`dirname $$x`; \
	    z=`basename $$y`; \
	    if test "$$z" == "CVS"; then continue; fi; \
	    echo $$z/$$f; \
	    diff test_mail/$$z/$$f \
	         mail/$$z/$$f; \
	    done

dangerous_test_clean:
	rm -rf mail/* flag log replies mbox

dangerous_clean:
	rm -rf mail help info* solutions* TODO \
	       flag log \
	       scoreboard.mail Scorefinder_Output \
	       Scoeboard_PID replies mbox web web.tar
