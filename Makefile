# HPCM Master Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sun Mar  9 09:34:54 EST 2003
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2003/03/09 14:45:20 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.34 $

# See STATUS file for description of versions.
#
VERSION=01_000

# Tar extension, zip option, and unzip option.
# Can be set to use zipped or unzipped tar files.
#
TAREXT = .tgz
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
#
#		hpcm_${VERSION}${TAREXT}
#		hpcm_${VERSION}_solutions${TAREXT}
#       	hpcm_${VERSION}_${NONDIS}${TAREXT}
#
# make signatures
#	Make HPCM_${VERSION}_MD5_Signatures file, a
#	copyrighted file with signatures of all tar'able
#	files.
#
# make md5check
#	Check the signatures in HPCM_${VERSION}_MD5_
#	Signatures.
#
# make cvssignatures
#	Make HPCM_${VERSION}_CVS_MD5_Signatures file
#	from hpcm_${VERSION}_cvs${TAREXT}.  You must
#	make this last file by hand; it is a tar file
#	containing the CVS root for HPCM.  The
#	signatures file is a copyrighted file with
#	signatures of all files in the cvs tar file.
#
# make md5cvscheck
#	Check the signatures in HPCM_${VERSION}_CVS_MD5_
#	Signatures.
#
# make web
#	Make web directory containing public files, and
#	web${TAREXT} which is a tar of that directory.
#
# make clean
#	Clean everything in this directory.
#
# make cleanall
#	Clean everything in this directory and all its
#	subdirectories that have Makefiles.  Runs
#	`make clean' in all these directories.

all:	submakes

signatures:	HPCM_${VERSION}_MD5_Signatures

cvssignatures:	HPCM_${VERSION}_CVS_MD5_Signatures

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
HPCM_${VERSION}_MD5_Signatures:	signatures_header \
		    hpcm_${VERSION}.files \
		    hpcm_${VERSION}_solutions.files \
		    hpcm_${VERSION}_${NONDIS}.files
	@if test "${HPCM_COPYRIGHT}" = ""; \
	then echo HPCM_COPYRIGHT not defined; exit 1; fi
	rm -f HPCM_${VERSION}_MD5_Signatures
	echo "HPCM ${VERSION} MD5 SIGNATURES" \
	     > HPCM_${VERSION}_MD5_Signatures
	echo "---- ------ --- ----------" \
	     >> HPCM_${VERSION}_MD5_Signatures
	echo "" \
	     >> HPCM_${VERSION}_MD5_Signatures
	echo "${HPCM_COPYRIGHT}" \
	     >> HPCM_${VERSION}_MD5_Signatures
	echo "" \
	     >> HPCM_${VERSION}_MD5_Signatures
	cat signatures_header \
	     >>  HPCM_${VERSION}_MD5_Signatures
	echo "" \
	     >> HPCM_${VERSION}_MD5_Signatures
	echo "HPCM Version: ${VERSION}" \
	     >>  HPCM_${VERSION}_MD5_Signatures
	echo "Date of Signatures:" \
	     "`date`" \
	     >>  HPCM_${VERSION}_MD5_Signatures
	echo " ====== " " ====== " \
	     " ====== " " ====== " \
	     " ====== " " ====== " \
	     " ====== " " ====== " \
	     >> HPCM_${VERSION}_MD5_Signatures
	cd ..; \
	   md5sum `cat \
	      hpcm/hpcm_${VERSION}.files \
	      hpcm/hpcm_${VERSION}_solutions.files \
	      hpcm/hpcm_${VERSION}_${NONDIS}.files \
	            ` \
	      >>  hpcm/HPCM_${VERSION}_MD5_Signatures

# Check MD5 Signatures
#
md5check:
	cd ..; \
	   sed < hpcm/HPCM_${VERSION}_MD5_Signatures \
	       -e '1,/^ ====== /'d | \
	       md5sum --check 2>&1 | \
	       sed -e '/^[^ 	]*: OK$$/d'

# Make cvstmp, a directory that holds the cvs files
# to be MD5 summed.  You must make hpcm_cvs_
# ${VERSION}${TAREXT} by hand.
#
cvstmp:	hpcm_${VERSION}_cvs${TAREXT}
	rm -rf cvstmp
	mkdir cvstmp
	cd cvstmp; \
	    tar xf ../hpcm_${VERSION}_cvs${TAREXT} \
	        ${TARUNZIP}

# Make MD5 CVS Signatures File:
#
HPCM_${VERSION}_CVS_MD5_Signatures:	\
		signatures_header cvstmp
	@if test "${HPCM_COPYRIGHT}" = ""; \
	then echo HPCM_COPYRIGHT not defined; exit 1; fi
	rm -f HPCM_${VERSION}_CVS_MD5_Signatures
	echo "HPCM ${VERSION} CVS MD5 SIGNATURES" \
	     > HPCM_${VERSION}_CVS_MD5_Signatures
	echo "---- ------ --- --- ----------" \
	     >> HPCM_${VERSION}_CVS_MD5_Signatures
	echo "" \
	     >> HPCM_${VERSION}_CVS_MD5_Signatures
	echo "${HPCM_COPYRIGHT}" \
	     >> HPCM_${VERSION}_CVS_MD5_Signatures
	echo "" \
	     >> HPCM_${VERSION}_CVS_MD5_Signatures
	cat signatures_header \
	     >>  HPCM_${VERSION}_CVS_MD5_Signatures
	echo "" \
	     >> HPCM_${VERSION}_CVS_MD5_Signatures
	echo "HPCM Version: ${VERSION}" \
	     >>  HPCM_${VERSION}_CVS_MD5_Signatures
	echo "Date of Signatures:" \
	     "`date`" \
	     >>  HPCM_${VERSION}_CVS_MD5_Signatures
	echo " ====== " " ====== " \
	     " ====== " " ====== " \
	     " ====== " " ====== " \
	     " ====== " " ====== " \
	     >> HPCM_${VERSION}_CVS_MD5_Signatures
	md5sum hpcm_${VERSION}_cvs${TAREXT} \
	  >>  HPCM_${VERSION}_CVS_MD5_Signatures
	cd cvstmp; \
	   md5sum `find . -type f -print` \
	      >>  ../HPCM_${VERSION}_CVS_MD5_Signatures

# Check CVS MD5 Signatures
#
md5cvscheck:	cvstmp
	cd cvstmp; \
	   sed < ../HPCM_${VERSION}_CVS_MD5_Signatures \
	       -e '1,/^ ====== /'d | \
	       md5sum --check 2>&1 | \
	       sed -e '/^[^ 	]*: OK$$/d'
		

# Make tar files.
#
tar:	hpcm_${VERSION}${TAREXT} \
        hpcm_${VERSION}_solutions${TAREXT} \
        hpcm_${VERSION}_${NONDIS}${TAREXT}

hpcm_${VERSION}${TAREXT}:	\
		HPCM_${VERSION}_MD5_Signatures \
		HPCM_${VERSION}_CVS_MD5_Signatures \
		hpcm_${VERSION}.files
	d=`pwd`;d=`basename $$d`; test $$d = hpcm
	rm -f hpcm_${VERSION}${TAREXT}
	cd ..; tar cf hpcm/hpcm_${VERSION}${TAREXT} \
	   hpcm/HPCM_${VERSION}_MD5_Signatures \
	   hpcm/HPCM_${VERSION}_CVS_MD5_Signatures \
	   `cat hpcm/hpcm_${VERSION}.files` \
	   ${TARZIP}

hpcm_${VERSION}_solutions${TAREXT}:	\
                        hpcm_${VERSION}_solutions.files
	d=`pwd`;d=`basename $$d`; test $$d = hpcm
	rm -f hpcm_${VERSION}_solutions${TAREXT}
	cd ..; \
	   tar cf \
	       hpcm/hpcm_${VERSION}_solutions${TAREXT} \
	   `cat hpcm/hpcm_${VERSION}_solutions.files` \
	   ${TARZIP}

hpcm_${VERSION}_${NONDIS}${TAREXT}:	\
		hpcm_${VERSION}_${NONDIS}.files
	d=`pwd`;d=`basename $$d`; test $$d = hpcm
	rm -f hpcm_${VERSION}_${NONDIS}${TAREXT}
	cd ..; tar cf \
	   hpcm/hpcm_${VERSION}_${NONDIS}${TAREXT} \
	   `cat hpcm/hpcm_${VERSION}_${NONDIS}.files` \
	   ${TARZIP}

# Make files that list all distributable non-solution,
# all distributable solution, and all non-distributable
# files.  Begin each file name with `hpcm/'.
#
hpcm_${VERSION}.files:	File_List
	rm -f hpcm_${VERSION}.files
	file_list 'public & ! solution' \
	     | sed -e 's/^\.\//hpcm\//' \
	     > hpcm_${VERSION}.files

hpcm_${VERSION}_solutions.files:	File_List
	rm -f hpcm_${VERSION}_solutions.files
	file_list 'public & solution' \
	     | sed -e 's/^\.\//hpcm\//' \
	     > hpcm_${VERSION}_solutions.files

hpcm_${VERSION}_${NONDIS}.files:	File_List
	rm -f hpcm_${VERSION}_${NONDIS}.files
	file_list '! public' \
	     | sed -e 's/^\.\//hpcm\//' \
	     > hpcm_${VERSION}_${NONDIS}.files

# Make web directory to distribute HPCM.
#
web:	cleanweb hpcm_${VERSION}${TAREXT} \
		 HPCM_${VERSION}_MD5_Signatures \
		 HPCM_${VERSION}_CVS_MD5_Signatures \
		 web_index.html
	@if test "${HPCM_WEB_CONTACT}" = ""; \
	then echo HPCM_WEB_CONTACT not set; \
	     exit 1; fi
	cd judge/doc; make overview.txt \
			   installing_hpcm.txt \
			   judging.txt
	mkdir web
	cp -p hpcm_${VERSION}${TAREXT} \
	      HPCM_${VERSION}_MD5_Signatures \
	      HPCM_${VERSION}_CVS_MD5_Signatures \
	      STATUS \
	      web
	cp -p judge/doc/overview.txt \
	      judge/doc/installing_hpcm.txt \
	      judge/doc/judging.txt \
	      web
	MD5SUM=`md5sum web/hpcm_${VERSION}${TAREXT}`; \
	   MD5SUM=`expr "$${MD5SUM}" : \
	                 '[ 	]*\([^ 	]*\)[ 	]' `; \
	   sed \
	      <web_index.html \
	      -e '/VERSION/s//${VERSION}/g' \
	      -e '/TAREXT/s//${TAREXT}/g' \
	      -e '/CONTACT/s//${HPCM_WEB_CONTACT}/g' \
	      -e '/MD5SUM/s//'$${MD5SUM}'/g' \
	      > web/index.html
	chmod 444 web/*
	chmod 755 web
	@if test "${HPCM_WEB_PASSWORD}" = ""; \
	then echo HPCM_WEB_PASSWORD not set; \
	     echo Not putting solutions into web; \
	else make web_solutions; fi
	cd web; tar cf ../web${TAREXT} * ${TARZIP}

web_solutions:	web/index.html \
		web_solutions_index.html \
		hpcm_${VERSION}_solutions${TAREXT}
	test "${HPCM_WEB_PASSWORD}" != ""
	mkdir web/private 
	mkdir web/private/${HPCM_WEB_PASSWORD}
	chmod 711 web/private
	chmod 755 web/private/${HPCM_WEB_PASSWORD}
	cp -p hpcm_${VERSION}_solutions${TAREXT} \
	      web/private/${HPCM_WEB_PASSWORD}
	MD5SUM=`md5sum web/private/*/*${TAREXT}` ; \
	  MD5SUM=`expr "$${MD5SUM}" : \
	               '[ 	]*\([^ 	]*\)[ 	]' `; \
	  sed \
	    <web_solutions_index.html \
	    -e '/VERSION/s//${VERSION}/g' \
	    -e '/TAREXT/s//${TAREXT}/g' \
	    -e '/CONTACT/s//${HPCM_WEB_CONTACT}/g' \
	    -e '/MD5SUM/s//'$${MD5SUM}'/g' \
	    >web/private/${HPCM_WEB_PASSWORD}/index.html
	chmod 444 web/private/${HPCM_WEB_PASSWORD}/*


cleanall:
	for x in `find . -name Makefile -print`; \
	    do \
	    ( cd `dirname $$x`; make clean ) \
	    done

clean:	cleantar cleanslocs cleancvs cleanweb

cleantar:
	rm -f HPCM_${VERSION}_MD5_Signatures \
	      hpcm_${VERSION}.files \
	      hpcm_${VERSION}_solutions.files \
	      hpcm_${VERSION}_${NONDIS}.files \
	      hpcm_${VERSION}${TAREXT} \
	      hpcm_${VERSION}_solutions${TAREXT} \
	      hpcm_${VERSION}_${NONDIS}${TAREXT}

cleanslocs:
	rm -f *.slocs

cleancvs:
	rm -rf cvstmp
	rm -f HPCM_${VERSION}_CVS_MD5_Signatures

cleanweb:
	rm -rf web web${TAREXT}
