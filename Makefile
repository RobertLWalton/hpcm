# HPCM Master Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Tue Nov  7 01:23:46 EST 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2000/11/07 06:23:11 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.6 $

all:	submakes

# Kill all implicit rules
#
.SUFFIXES:
.SUFFIXES:	.files

# The following must be done to make sure things
# are ready to run.
#
submakes:
	(cd ./contestant/bin/; make)
	(cd ./contestant/help/; make)
	(cd ./judge/bin/; make)

# Print files keyed for `user', `remote', etc.
#
user.files \
remote.files \
pascal.files \
system.files\
test.files \
problem.files \
library.files:
	@echo `sed <File_List -n -e \
		'/^$*		*/s///p' `

# Print both problem and library files:
#
librarian.files:
	@echo `sed <File_List -n \
		-e '/^problem		*/s///p' \
		-e '/^library		*/s///p' `
