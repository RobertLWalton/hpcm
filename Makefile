# HPCM Master Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Mon Jan 15 17:25:56 EST 2001
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2001/01/15 22:34:03 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.17 $

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

non_distributable.files:
	@sed <File_List -n \
	     -e '/^N[a-z][a-z]*		*/s///p'

all.files:
	@sed <File_List -n \
	     -e '/^N*[a-z][a-z]*		*/s///p'

# Make MD5 Signatures File:
#
HPCM_MD5_Signatures:	signatures_header \
			distributable_files_${VERSION} \
		    non_distributable_files_${VERSION}
	rm -f HPCM_MD5_Signatures
	cat signatures_header >  HPCM_MD5_Signatures
	echo ============== Date of Signatures: \
	     "`date`" >>  HPCM_MD5_Signatures
	cd ..; \
	   md5sum `cat \
	      hpcm/distributable_files_${VERSION} \
	      hpcm/non_distributable_files_${VERSION} \
	            ` \
	      >>  hpcm/HPCM_MD5_Signatures

# Check MD5 Signatures
#
md5_check:
	cd ..; \
	   sed < hpcm/HPCM_MD5_Signatures \
	       -e '1,/^=====/'d | \
	       md5sum --check | \
	       grep -v '^[^ 	]*: OK$$'
		

# Make tar files.
#
tar:	cleantar hpcm_${VERSION}.tar \
        hpcm_non_distributable_${VERSION}.tar

hpcm_${VERSION}.tar:	HPCM_MD5_Signatures \
                        distributable_files_${VERSION}
	rm -f hpcm_${VERSION}.tar
	cd ..; tar cf hpcm/hpcm_${VERSION}.tar \
	   hpcm/HPCM_MD5_Signatures \
	   `cat hpcm/distributable_files_${VERSION}`

hpcm_non_distributable_${VERSION}.tar:	\
		non_distributable_files_${VERSION}
	rm -f hpcm_non_distributable_${VERSION}.tar
	cd ..; tar cf \
	   hpcm/hpcm_non_distributable_${VERSION}.tar \
	   `cat hpcm/non_distributable_files_${VERSION}`

distributable_files_${VERSION}:	File_List Makefile
	rm -f distributable_files_${VERSION}
	make --no-print-directory \
	     distributable.files \
	     | sed -e 's/^\./hpcm/' \
	     > distributable_files_${VERSION}

non_distributable_files_${VERSION}:	\
				File_List Makefile
	rm -f non_distributable_files_${VERSION}
	make --no-print-directory \
	     non_distributable.files \
	     | sed -e 's/^\./hpcm/' \
	     > non_distributable_files_${VERSION}

cleanall:
	for x in `find . -name Makefile -print`; \
	    do \
	    ( cd `dirname $$x`; make clean ) \
	    done

clean:	cleantar

cleantar:
	rm -f HPCM_MD5_Signatures \
	      distributable_files_${VERSION} \
	      non_distributable_files_${VERSION} \
	      hpcm_${VERSION}.tar \
	      hpcm_non_distributable_${VERSION}.tar
