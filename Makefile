# HPCM Master Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sat Nov 11 10:38:13 EST 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2000/11/11 15:49:52 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.7 $

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
common.files \
email.files \
informal.files \
formal.files \
demo.files \
pascal.files \
system.files\
test.files \
problem.files \
library.files:
	@sed <File_List -n -e '/^$*		*/s///p'
