# HPCM Master Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Fri Sep  8 04:24:48 EDT 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: acm-cont $
#   $Date: 2000/09/08 08:24:42 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.1 $

all:	submakes

# Kill all implicit rules
#
.SUFFIXES:

submakes:
	(cd judge/bin; make)
	(cd contestant/bin; make)
