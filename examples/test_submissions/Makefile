# Makefile for test in contestant account.
#
# File:		Makefile
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Wed Jan 24 02:13:07 EST 2007
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2007/01/24 07:49:38 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.7 $

# This Makefile is to be used to test HPCM from a
# contestant account.  Instructions are:
#
# 1.  Copy the contents of this hpcm/examples/test_
#     submissions directory to a fresh directory in a
#     contestant account.  Log into the contestant
#     account and make the fresh directory current.
#
# 2.  Empty the contest account mailbox of any previous
#     emails.
#
# 3a. If the account is local (provided by the judge),
#     or if you have set up the UNIX Tools so that
#     hpcm_sendmail behaves as if were local, run
#     ALL of the following which apply:
#
#	make test_local
#
#	make test_get.local
#	    if the contest is informal
#
#	make test_forbidden.local
#	    if the contest is informal and if
#	    contestants are not allowed to get all
#	    problems at once
#
# 3b. If the account is an email account (your own
#     account NOT provided by the judge) and you do
#     not wish to use the UNIX Tools, put the judge's
#     email address in the ./Contest_Address file and
#     run ALL of the following which apply:
#
#	make test_email
#
#	make test_forbidden.email
#	    if the contest is informal and if
#	    contestants are not allowed to get all
#	    problems at once
#
# 4.  Wait for the autojudge to return a reply to all
#     the submissions caused by the last step.  Wait
#     for several (e.g. 3) minutes after the last reply
#     to be sure replies involving Timeout errors are
#     returned, as a 60 second timeout is employed in
#     some tests.
#
# 5a. If the account is local (provided by the judge),
#     copy all the mail into the `mbox' file in the
#     current directory and run:
#
#	make extract_replies
#
#     and then ONE of:
#
#	make manual_diff_local
#	    for a formal contest
#
#	make auto_diff_local
#	    for an untimed contest
#
#	make auto_diff_informal
#	    for an informal contest
#
#     In the above you can change `auto' to `manual'
#     and vice versa according to whether scoring is
#     completely automatic or a combination of automatic
#     and manual.
#
#     If contestants are NOT permitted to get all
#     problems at once, as in some informal contests,
#     run in addition:
#
#	make test_forbidden.diff
#    
#     Check that any listed differences are inconsequen-
#     tial.
#
# 5b. If you have your own account and have either the
#     hpcm/contestant/bin or hpcm/judge/bin directories
#     in your PATH, you can follow the procedure in 5a
#     changing `local' to `email' (even if you have used
#     the UNIX Tools).
#
# 5c. Otherwise if 5a and 5b do not apply, examine each
#     email reply by hand to compare its X-HPCM-Test-
#     Subject field with the results in the body of the
#     reply. 
	

all:
	@echo "See comments in this Makefile for" \
	      "documentation."

.SUFFIXES:

S=.
include Makefile.mk
