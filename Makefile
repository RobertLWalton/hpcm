# HPCM Master Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Wed Jan 10 04:32:19 EST 2001
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2001/01/10 10:54:49 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.14 $

# See STATUS file for description of versions.
#
VERSION=0.0

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
exmpl.files \
demo.files \
pascal.files \
doc.files\
system.files\
test.files \
tdata.files \
prob.files \
plib.files \
legal.files:
	@sed <File_List -n -e '/^N*$*		*/s///p'

distributable.files:
	@sed <File_List -n \
	     -e '/^[a-z][a-z]*		*/s///p'

non-distributable.files:
	@sed <File_List -n \
	     -e '/^N[a-z][a-z]*		*/s///p'

all.files:
	@sed <File_List -n \
	     -e '/^N*[a-z][a-z]*		*/s///p'

# Make tar files.
#
tar:	hpcm-${VERSION}.tar \
        hpcm-non-distributable-${VERSION}.tar

hpcm-${VERSION}.tar:	distributable-files-${VERSION}
	rm -f hpcm-${VERSION}.tar
	cd ..; tar cf hpcm/hpcm-${VERSION}.tar \
	   --files-from \
	   hpcm/distributable-files-${VERSION}

hpcm-non-distributable-${VERSION}.tar:	\
		non-distributable-files-${VERSION}
	rm -f hpcm-non-distributable-${VERSION}.tar
	cd ..; tar cf \
	   hpcm/hpcm-non-distributable-${VERSION}.tar \
	   --files-from \
	   hpcm/non-distributable-files-${VERSION}

distributable-files-${VERSION}:	File_List Makefile
	rm -f distributable-files-${VERSION}
	make --no-print-directory \
	     distributable.files \
	     | sed -e 's/^\./hpcm/' \
	     > distributable-files-${VERSION}

non-distributable-files-${VERSION}:	\
				File_List Makefile
	rm -f non-distributable-files-${VERSION}
	make --no-print-directory \
	     non-distributable.files \
	     | sed -e 's/^\./hpcm/' \
	     > non-distributable-files-${VERSION}

cleanall:
	for x in `find . -name Makefile -print`; \
	    do \
	    ( cd `dirname $$x`; make clean ) \
	    done

clean:
	rm -f distributable-files-${VERSION} \
	      non-distributable-files-${VERSION} \
	      hpcm-${VERSION}.tar \
	      hpcm-non-distributable-${VERSION}.tar
