# HPCM Master Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Fri Jan 24 08:28:42 EST 2003
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2003/01/24 14:02:50 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.23 $

# See STATUS file for description of versions.
#
VERSION=1.000

all:	submakes

signatures:	HPCM_MD5_Signatures

# slocs:	make .sloc files: see below.

# Kill all implicit rules
#
.SUFFIXES:

# The following must be done to make sure things are
# ready to run when there are auxiliary judges.
#
aux auxiliary:	submakes
	@echo Setting Group Permissions
	find . -perm +g+r -or \
	       -perm +u+r -print -exec chmod g+r {} \;
	find . -perm +g+x -or \
	       -perm +u+x -print -exec chmod g+x {} \;

# The following must be done to make sure things
# are ready to run.
#
submakes:
	(cd ./contestant/bin/; make)
	(cd ./contestant/help/; make)
	(cd ./judge/bin/; make)
	(cd ./judge/doc/; make)

# Make SLOC count files.
#
slocs:
	rm -f source.slocs solution.slocs
	sloc_counts -s `file_list 'src | lib | bin' ` \
		    > source.slocs
	sloc_counts -s `file_list 'solution' ` \
		    > solution.slocs

# Make MD5 Signatures File:
#
HPCM_MD5_Signatures:	signatures_header \
			distributable_files_${VERSION} \
		    non_distributable_files_${VERSION}
	rm -f HPCM_MD5_Signatures
	echo "HPCM MD5 SIGNATURES" \
	     > HPCM_MD5_Signatures
	echo "---- --- ----------" \
	     >> HPCM_MD5_Signatures
	echo "" \
	     >> HPCM_MD5_Signatures
	echo "${COPYRIGHT}" \
	     >> HPCM_MD5_Signatures
	echo "" \
	     >> HPCM_MD5_Signatures
	cat signatures_header \
	     >>  HPCM_MD5_Signatures
	echo "" \
	     >> HPCM_MD5_Signatures
	echo "HPCM Version: ${VERSION}" \
	     >>  HPCM_MD5_Signatures
	echo "Date of Signatures:" \
	     "`date`" \
	     >>  HPCM_MD5_Signatures
	echo " ====== " " ====== " \
	     " ====== " " ====== " \
	     " ====== " " ====== " \
	     " ====== " " ====== " \
	     >> HPCM_MD5_Signatures
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
	       -e '1,/^ ====== /'d | \
	       md5sum --check 2>&1 | \
	       sed -e '/^[^ 	]*: OK$$/d'
		

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
	file_list public \
	     | sed -e 's/^\./hpcm/' \
	     > distributable_files_${VERSION}

non_distributable_files_${VERSION}:	\
				File_List Makefile
	rm -f non_distributable_files_${VERSION}
	file_list '! public' \
	     | sed -e 's/^\./hpcm/' \
	     > non_distributable_files_${VERSION}

cleanall:
	for x in `find . -name Makefile -print`; \
	    do \
	    ( cd `dirname $$x`; make clean ) \
	    done

clean:	cleantar cleanslocs

cleantar:
	rm -f HPCM_MD5_Signatures \
	      distributable_files_${VERSION} \
	      non_distributable_files_${VERSION} \
	      hpcm_${VERSION}.tar \
	      hpcm_non_distributable_${VERSION}.tar

cleanslocs:
	rm -f *.slocs
