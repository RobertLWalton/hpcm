# HPCM Master Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sun Jan  7 22:02:51 EST 2001
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2001/01/08 04:26:34 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.11 $

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
	(cd ./judge/doc/; make)

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

all.files:
	@sed <File_List -n \
	     -e '/^[a-z][a-z]*		*/s///p'
