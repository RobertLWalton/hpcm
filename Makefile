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
#   $Author: hc3 $
#   $Date: 2000/09/19 19:20:51 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.2 $

all:	submakes

# Kill all implicit rules
#
.SUFFIXES:

submakes:
	(cd judge/bin; make)
	(cd contestant/bin; make)

print_system:
	echo `grep -v '^./contest/' File_List | \
		grep -v '^./problem_library/' | \
		grep -v '^./judge/test/' `

print_test:
	echo `grep '^./judge/test/' File_List `

print_library:
	echo `grep '^./problem_library/' File_List `

print_contest:
	echo `grep '^./contest/' File_List `
