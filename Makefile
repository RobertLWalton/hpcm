# HPCM Master Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Wed Feb 19 03:03:03 EST 2003
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2003/02/19 08:10:53 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.27 $

# See STATUS file for description of versions.
#
VERSION=1.000

# Tar extension, zip option, and unzip option.
# Can be set to use zipped or unzipped tar files.
#
TAREXT = .tar.gz
TARZIP = --gzip
TARUNZIP = --gunzip
# TAREXT = .tar
# TARZIP =
# TARUNZIP =

# Abbreviation to shorten lines.
#
NONDIS = non_distributable

# Commands.
#
# make
# make all
#	Set up hpcm, e.g., set o+x permissions on files
#	that need it, compile binaries that need it.
#
# make aux
# make auxiliary
#	Set up hpcm so it can be used both normally
#	and by auxiliary judges.  Like `make all' but
#	also adds group permissions to files and
#	directories.
#
# make slocs
#	Make sloc count files, source.slocs and
#	solution.slocs.
#
# make tar
#	Make distributable and non-distributable tar
#	files:
#		hpcm_${VERSION}${TAREXT},
#		hpcm_solutions_${VERSION}${TAREXT}
#       	hpcm_${NONDIS}_${VERSION}${TAREXT}
#
# make signatures
#	Make HPCM_MD5_Signatures file, a copyrighted
#	file with signatures of all tar'able files.
#
# make md5check
#	Check the signatures in HPCM_MD5_Signatures.
#
# make cvssignatures
#	Make HPCM_CVS_MD5_Signatures file from
#	hpcm_cvs_${VERSION}${TAREXT}.  You must make
#	this last file by hand; it is a tar file con-
#	taining the CVS root for HPCM.  The signatures
#	file is a copyrighted file with signatures
#	of all files in the cvs tar file.
#
# make md5cvscheck
#	Check the signatures in HPCM_CVS_MD5_Signatures.
#
# make clean
#	Clean everything in this directory.
#
# make cleanall
#	Clean everything in this directory and all its
#	subdirectories that have Makefiles.  Runs
#	`make clean' in all these directories.

HPCM_CVS_MD5_Signatures:	signatures_header cvstmp

all:	submakes

signatures:	HPCM_MD5_Signatures
cvssignatures:	HPCM_CVS_MD5_Signatures

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
			hpcm_${VERSION}.files \
		    hpcm_solutions_${VERSION}.files \
		    hpcm_${NONDIS}_${VERSION}.files
	@if test "${COPYRIGHT}" = ""; \
	then echo COPYRIGHT not defined; exit 1; fi
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
	      hpcm/hpcm_${VERSION}.files \
	      hpcm/hpcm_solutions_${VERSION}.files \
	      hpcm/hpcm_${NONDIS}_${VERSION}.files \
	            ` \
	      >>  hpcm/HPCM_MD5_Signatures

# Check MD5 Signatures
#
md5check:
	cd ..; \
	   sed < hpcm/HPCM_MD5_Signatures \
	       -e '1,/^ ====== /'d | \
	       md5sum --check 2>&1 | \
	       sed -e '/^[^ 	]*: OK$$/d'

# Make cvstmp, a directory that holds the cvs files
# to be MD5 summed.  You must make hpcm_cvs_
# ${VERSION}${TAREXT} by hand.
#
cvstmp:	hpcm_cvs_${VERSION}${TAREXT}
	rm -rf cvstmp
	mkdir cvstmp
	cd cvstmp; \
	    tar xf ../hpcm_cvs_${VERSION}${TAREXT} \
	        ${TARUNZIP}
	cp hpcm_cvs_${VERSION}${TAREXT} cvstmp

# Make MD5 CVS Signatures File:
#
HPCM_CVS_MD5_Signatures:	signatures_header cvstmp
	@if test "${COPYRIGHT}" = ""; \
	then echo COPYRIGHT not defined; exit 1; fi
	rm -f HPCM_CVS_MD5_Signatures
	echo "HPCM CVS MD5 SIGNATURES" \
	     > HPCM_CVS_MD5_Signatures
	echo "---- --- --- ----------" \
	     >> HPCM_CVS_MD5_Signatures
	echo "" \
	     >> HPCM_CVS_MD5_Signatures
	echo "${COPYRIGHT}" \
	     >> HPCM_CVS_MD5_Signatures
	echo "" \
	     >> HPCM_CVS_MD5_Signatures
	cat signatures_header \
	     >>  HPCM_CVS_MD5_Signatures
	echo "" \
	     >> HPCM_CVS_MD5_Signatures
	echo "HPCM Version: ${VERSION}" \
	     >>  HPCM_CVS_MD5_Signatures
	echo "Date of Signatures:" \
	     "`date`" \
	     >>  HPCM_CVS_MD5_Signatures
	echo " ====== " " ====== " \
	     " ====== " " ====== " \
	     " ====== " " ====== " \
	     " ====== " " ====== " \
	     >> HPCM_CVS_MD5_Signatures
	cd cvstmp; \
	   md5sum `find . -type f -print` \
	      >>  ../HPCM_CVS_MD5_Signatures

# Check CVS MD5 Signatures
#
md5cvscheck:	cvstmp
	cd cvstmp; \
	   sed < ../HPCM_CVS_MD5_Signatures \
	       -e '1,/^ ====== /'d | \
	       md5sum --check 2>&1 | \
	       sed -e '/^[^ 	]*: OK$$/d'
		

# Make tar files.
#
tar:	hpcm_${VERSION}${TAREXT} \
        hpcm_solutions_${VERSION}${TAREXT} \
        hpcm_${NONDIS}_${VERSION}${TAREXT}

hpcm_${VERSION}${TAREXT}:	HPCM_MD5_Signatures \
                        hpcm_${VERSION}.files
	d=`pwd`;d=`basename $$d`; test $$d = hpcm
	rm -f hpcm_${VERSION}${TAREXT}
	cd ..; tar cf hpcm/hpcm_${VERSION}${TAREXT} \
	   hpcm/HPCM_MD5_Signatures \
	   `cat hpcm/hpcm_${VERSION}.files` \
	   ${TARZIP}

hpcm_solutions_${VERSION}${TAREXT}:	\
                        hpcm_solutions_${VERSION}.files
	d=`pwd`;d=`basename $$d`; test $$d = hpcm
	rm -f hpcm_${VERSION}${TAREXT}
	cd ..; \
	   tar cf \
	       hpcm/hpcm_solutions_${VERSION}${TAREXT} \
	   `cat hpcm/hpcm_solutions_${VERSION}.files` \
	   ${TARZIP}

hpcm_${NONDIS}_${VERSION}${TAREXT}:	\
		hpcm_${NONDIS}_${VERSION}.files
	d=`pwd`;d=`basename $$d`; test $$d = hpcm
	rm -f hpcm_${NONDIS}_${VERSION}${TAREXT}
	cd ..; tar cf \
	   hpcm/hpcm_${NONDIS}_${VERSION}${TAREXT} \
	   `cat hpcm/hpcm_${NONDIS}_${VERSION}.files` \
	   ${TARZIP}

# Make files that list all distributable non-solution,
# all distributable solution, and all non-distributable
# files.  Begin each file name with `hpcm/'.
#
hpcm_${VERSION}.files:	File_List Makefile
	rm -f hpcm_${VERSION}.files
	file_list 'public & ! solution' \
	     | sed -e 's/^\./hpcm/' \
	     > hpcm_${VERSION}.files

hpcm_solutions_${VERSION}.files:	File_List \
					Makefile
	rm -f hpcm_solutions_${VERSION}.files
	file_list 'public & solution' \
	     | sed -e 's/^\./hpcm/' \
	     > hpcm_solutions_${VERSION}.files

hpcm_${NONDIS}_${VERSION}.files:	\
				File_List Makefile
	rm -f hpcm_${NONDIS}_${VERSION}.files
	file_list '! public' \
	     | sed -e 's/^\./hpcm/' \
	     > hpcm_${NONDIS}_${VERSION}.files

cleanall:
	for x in `find . -name Makefile -print`; \
	    do \
	    ( cd `dirname $$x`; make clean ) \
	    done

clean:	cleantar cleanslocs cleancvs

cleantar:
	rm -f HPCM_MD5_Signatures \
	      hpcm_${VERSION}.files \
	      hpcm_solutions_${VERSION}.files \
	      hpcm_${NONDIS}_${VERSION}.files \
	      hpcm_${VERSION}${TAREXT} \
	      hpcm_solutions_${VERSION}${TAREXT} \
	      hpcm_${NONDIS}_${VERSION}${TAREXT}

cleanslocs:
	rm -f *.slocs

cleancvs:
	rm -rf cvstmp
	rm -f HPCM_CVS_MD5_Signatures
