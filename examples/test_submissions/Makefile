# Makefile for test in contestant account.
#
# File:		Makefile
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Fri Feb 15 23:56:37 EST 2002
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/02/16 04:55:54 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.2 $

all:	test_all

.SUFFIXES:
.SUFFIXES:	.mail .send

.mail.send:
	hpcm_sendmail <$*.mail
	sleep 2

TEST_PREREQUISITES=
include Makefile.mk
