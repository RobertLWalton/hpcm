# Makefile for test in contestant account.
#
# File:		Makefile
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Tue Feb 19 03:57:03 EST 2002
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/02/19 08:56:04 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.3 $

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
include Makefile.mk
