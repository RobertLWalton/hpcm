# HPCM Master Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sun Nov 19 06:06:08 EST 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2000/11/19 11:04:38 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.8 $

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

informal.files:		inform.files

# Print files keyed for `user', `remote', etc.
#
common.files \
email.files \
inform.files \
formal.files \
demo.files \
pascal.files \
system.files\
test.files \
problem.files \
library.files:
	@sed <File_List -n -e '/^$*		*/s///p'
