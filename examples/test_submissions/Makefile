# Makefile for test in contestant account.
#
# File:		Makefile
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Wed Jul 31 05:32:48 EDT 2002
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/07/31 09:28:35 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.4 $

# This Makefile is to be used to test HPCM from a
# contestant account.  Instructions are:
#
# 1. Copy the contents of this hpcm/examples/test_
#    submissions directory to a fresh directory in a
#    contestant account.  Log into the contestant
#    account and make the fresh directory current.
#
# 2. Empty the contest account mailbox of any previous
#    test replies, and in the current directory run one
#    of:
#
#	make test_email
#	make test_informal
#	make test_formal
#
#    according to the type of contest.
#
# 3. Wait for the autojudge to return a reply to all
#    the submissions caused by the last step.  Wait
#    for at least 1 minute after the last reply to be
#    sure replies involving Timeout errors are returned,
#    as a 60 second timeout is employed in some tests.
#
# 4. Copy all the mail into the `mbox' file in the
#    current directory and run:
#
#	make extract_replies
#	make diff_replies
#    
#    Check that any listed differences are inconsequen-
#    tial.
	

all:
	@echo "Use one of:"
	@echo "    make test_email"
	@echo "    make test_informal"
	@echo "    make test_formal"
	@echo "according to the contest type."

.SUFFIXES:
.SUFFIXES:	.mail .send

.mail.send:
	hpcm_sendmail <$*.mail
	sleep 2

TEST_PREREQUISITES=
S=.
include Makefile.mk
