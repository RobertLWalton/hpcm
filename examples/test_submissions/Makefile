# Makefile for test in contestant account.
#
# File:		Makefile
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Tue Feb  5 07:14:54 EST 2002
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/02/05 12:16:00 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.1 $

all:	test_all

.SUFFIXES:
.SUFFIXES:	.mail .send

.mail.send:
	hpcm_sendmail <$*.mail
	sleep 2

test_all:	test_count_correct \
		test_count_incorrect \
		test_system_error \
		test_get \
		test_query \
		test_problem_name

TEST_PREREQUISITES=
include Makefile.mk
