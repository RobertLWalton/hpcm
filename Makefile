# HPCM Master Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Mon Sep 25 03:44:12 EDT 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2000/09/25 07:39:50 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.5 $

all:	submakes

# Kill all implicit rules
#
.SUFFIXES:
.SUFFIXES:	.print

# The following must be done to make sure things
# are ready to run.
#
submakes:
	(cd ./contestant/bin/; make)
	(cd ./contestant/help/; make)
	(cd ./judge/bin/; make)

# Print files keyed for `user', `remote', etc.
#
user.print \
remote.print \
pascal.print \
system.print\
test.print \
problem.print \
library.print:
	echo `sed <File_List -n -e \
		'/^$*		*/s///p' `

# Print both problem and library files:
#
librarian.print:
	echo `sed <File_List -n -e \
		'/^problem		*/s///p' \
		'/^library		*/s///p' `
