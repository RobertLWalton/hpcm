# HPCM Master Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Mon Dec 11 15:05:03 EST 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2000/12/12 03:39:16 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.10 $

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
example.files \
demo.files \
pascal.files \
doc.files\
system.files\
test.files \
tdata.files \
problem.files \
library.files \
legal.files:
	@sed <File_List -n -e '/^$*		*/s///p'
