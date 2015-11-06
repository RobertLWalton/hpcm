# HPCM Contestant Test Submissions Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Fri Nov  6 04:16:29 EST 2015
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

all:	modes

# Kill all implicit rules
#
.SUFFIXES:

modes:
	chmod a+rx . correct email \
	           extraction incorrect
	chmod a+r correct/* email/* \
	          extraction/* incorrect/*
