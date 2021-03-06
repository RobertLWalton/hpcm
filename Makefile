# HPCM Master Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Thu Oct 15 12:32:56 EDT 2015
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2014/12/29 08:59:21 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.66 $

# Include file that contains the following variables:
#
#	VERSION		HPCM_xx_yyy
#
#	COPYRIGHT	Signatures file copyright
#			notice.
#
#	WEB_CONTACT	Email address of contact to be
#			listed on HPCM web pages.
#
# See STATUS file for description of versions.
#
include VERSION

# Optional include file that contains the following
# variable:
#
#
#	WEB_PASSWORD	Password that is part of hidden
#			URL name of solutions web sub-
#			directory.  If not set, then no
#			solutions subdirectory of the
#			web directory is made.
#
-include HPCM_WEB_SOLUTIONS_PASSWORD

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

# Files to be archived
#
ARCHIVE_FILES = \
	hpcm_${VERSION}_cvs.tgz \
	HPCM_${VERSION}_CVS_Signatures \
	HPCM_${VERSION}_Signatures \
	hpcm_${VERSION}_non_distributable.tgz \
	hpcm_${VERSION}_solutions.tgz \
	hpcm_${VERSION}.tgz \
	hpcm_${VERSION}_web.tgz

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
# make files
#	Make *.files files, and in particular
#	file_list.files and cvs.files.
#
#	file_list.files are all files listed in
#	the File_List file.  cvs.files are all files
#	in the CVS repository (determined by cvs log).
#	These lists can be directly diff'ed to see
#	the differences between these two lists.
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
#	Before these files are made, you must make
#	by hand the file
#
#		hpcm_${VERSION}_cvs.tgz
#
#	which can be any tar of the HPCM cvs director-
#	ies.  This might be made, for example, by
#
#	    cd HPCM-CVSROOT-DIRECTORY
#	    tar zcf hpcm_${VERSION}_cvs.tgz CVSROOT hpcm
#
#	Also, before these files are made you must de-
#	fine the COPYRIGHT environment variable to be
#	the copyright notice to be put into the Signa-
#       tures files (see below).
#
# make signatures
#	Make HPCM_${VERSION}_Signatures file, a
#	copyrighted file with signatures of all tar'able
#	files.  COPYRIGHT must be defined (see above).
#
# make check
#	Check the signatures in HPCM_${VERSION}_
#       Signatures, except for non-existent (e.g. non-
#       distributed) files.
#
# make checkall
#	Check ALL the signatures in HPCM_${VERSION}_
#       Signatures, and list non-existent files.
#
# make cvssignatures
#	Make HPCM_${VERSION}_CVS_Signatures file
#	from hpcm_${VERSION}_cvs${TAREXT}.  You must
#	make this last file by hand; it is a tar file
#	containing the CVS root for HPCM.  The
#	signatures file is a copyrighted file with
#	signatures of all files in the cvs tar file.
#	COPYRIGHT must be defined (see above).
#
# make cvscheck
#	Check the signatures in HPCM_${VERSION}_CVS_
#	Signatures.
#
# make web
#	Make web directory containing public files, and
#	hpcm_${VERSION}_web${TAREXT} which is a tar of
#	that directory.  In order to do this you need
#	to set the following environment variables:
#
#	    COPYRIGHT
#		This is the copyright notice to be put
#		in the Signatures files.
#
#	    WEB_CONTACT
#		Email address of contact to be listed on
#		web pages.
#
#	    WEB_PASSWORD
#		If this is defined, solutions are put in
#		the web subpage named
#
#		    private/${WEB_PASSWORD}
#
#		where the private directory is not read-
#		able except by its owner.
#
# make archive
#	Make the ARCHIVE/hpcm_${VERSION} containing the
#	ARCHIVE_FILES.
#	
# make clean
#	Clean everything in this directory but not the
#       ARCHIVE.
#
# make cleanall
#	Clean everything in this directory and all its
#	subdirectories that have Makefiles.  Runs
#	`make clean' in all these directories.  Does NOT
#       clean the ARCHIVE.
#
# make cleanarchive
#	Remove ARCHIVE/hpcm_${VERSION}.

all:	submakes public project

libraries/public:
	(cd libraries; rm -f public; \
	 ln -s ~/hpcm_public public)

public:	libraries/public
	@if test ! -d libraries/public; then \
	    echo "=========================="; \
	    echo "WARNING: libraries/public" \
	         "not defined"; \
	    echo "=========================="; \
	    fi

libraries/project:
	(cd libraries; rm -f project; \
	 ln -s ~/hpcm_project project)

project:	libraries/project
	@if test ! -d libraries/project; then \
	    echo "=========================="; \
	    echo "WARNING: libraries/project" \
	         "not defined"; \
	    echo "=========================="; \
	    fi

signatures:	HPCM_${VERSION}_Signatures

cvssignatures:	HPCM_${VERSION}_CVS_Signatures

files:		hpcm_${VERSION}.files \
		hpcm_${VERSION}_solutions.files \
		hpcm_${VERSION}_${NONDIS}.files \
		file_list.files cvs.files

# Kill all implicit rules
#
.SUFFIXES:

# A prerequisite that is never there: used to force
# rules to fire every time.
#
NO_SUCH_FILE:

# The following must be done to make sure things are
# ready to run when there are auxiliary judges.
#
# Note: -perm +g+r (or +g+x) must be put AFTER -prune
# clauses as it will prevent them from being executed
# and thus pruning.
#
aux auxiliary:	submakes
	@echo Setting Group Permissions
	find ~ -type l -or \
	       -name secure -not -path ~/hpcm/secure \
	             -prune -or \
	       -name .ssh -prune -or \
	       -name .Xauthority -or \
	       -path ~/judging_'*'_identity -or \
	       -perm -g+r -or \
	       -perm -u+r -print -exec chmod g+r {} \;
	find ~ -type l -or \
	       -name secure -not -path ~/hpcm/secure \
	             -prune -or \
	       -name .ssh -prune -or \
	       -name .Xauthority -or \
	       -perm -g+x -or \
	       -perm -u+x -print -exec chmod g+x {} \;

# The following must be done to make sure things
# are ready to run.
#
submakes:
	(cd ./contestant/bin/; make)
	(cd ./contestant/help/; make)
	(cd ./contestant/test_submissions/; make)
	(cd ./judge/bin/; make)
	(cd ./judge/doc/; make)

# Make SLOC count files.
#
slocs:	source.slocs solution.slocs

source.slocs:
	rm -f source.slocs
	sloc_counts -s `file_list 'src | lib | bin' ` \
		    > source.slocs

solution.slocs:
	rm -f solution.slocs
	sloc_counts -s `file_list 'solution' ` \
		    > solution.slocs

# Make Signatures File:
#
HPCM_${VERSION}_Signatures:	Makefile \
		    signatures_header \
		    hpcm_${VERSION}.files \
		    hpcm_${VERSION}_solutions.files \
		    hpcm_${VERSION}_${NONDIS}.files
	rm -f HPCM_${VERSION}_Signatures
	echo "HPCM ${VERSION}" \
	     > HPCM_${VERSION}_Signatures
	echo "---- ------" \
	     >> HPCM_${VERSION}_Signatures
	echo "" \
	     >> HPCM_${VERSION}_Signatures
	echo "${COPYRIGHT}" \
	     >> HPCM_${VERSION}_Signatures
	echo "" \
	     >> HPCM_${VERSION}_Signatures
	cat signatures_header \
	     >>  HPCM_${VERSION}_Signatures
	echo "" \
	     >> HPCM_${VERSION}_Signatures
	echo "HPCM Version: ${VERSION}" \
	     >>  HPCM_${VERSION}_Signatures
	echo "Date of Signatures:" \
	     "`date`" \
	     >>  HPCM_${VERSION}_Signatures
	echo " ====== " " ====== " \
	     " ====== " " ====== " \
	     " ====== " " ====== " \
	     " MD5 Signatures:" \
	     >> HPCM_${VERSION}_Signatures
	cd ..; \
	   md5sum `cat \
	      hpcm/hpcm_${VERSION}.files \
	      hpcm/hpcm_${VERSION}_solutions.files \
	      hpcm/hpcm_${VERSION}_${NONDIS}.files \
	            ` \
	      >>  hpcm/HPCM_${VERSION}_Signatures
	echo " ====== " " ====== " \
	     " ====== " " ====== " \
	     " ====== " " ====== " \
	     " SHA1 Signatures:" \
	     >> HPCM_${VERSION}_Signatures
	cd ..; \
	   sha1sum `cat \
	      hpcm/hpcm_${VERSION}.files \
	      hpcm/hpcm_${VERSION}_solutions.files \
	      hpcm/hpcm_${VERSION}_${NONDIS}.files \
	            ` \
	      >>  hpcm/HPCM_${VERSION}_Signatures
	chmod a-wx HPCM_${VERSION}_Signatures

# Check signatures except for non-existent files.
#
check:
	cd ..; \
	   sed < hpcm/HPCM_${VERSION}_Signatures \
	       -e '1,/ ======   MD5 Signatures:$$/'d \
	       -e '/ ======   SHA1 Signatures:$$/,$$'d | \
	       md5sum --check 2>&1 | \
	       sed -e '/^[^ 	]*: OK$$/d' \
		   -e '/^[^ 	]*: FAILED open or read/d' \
	           -e '/: No such file or directory$$/d'
	cd ..; \
	   sed < hpcm/HPCM_${VERSION}_Signatures \
	       -e '1,/ ======   SHA1 Signatures:$$/'d | \
	       sha1sum --check 2>&1 | \
	       sed -e '/^[^ 	]*: OK$$/d' \
		   -e '/^[^ 	]*: FAILED open or read/d' \
	           -e '/: No such file or directory$$/d'

# Check all signatures.
#
checkall:
	cd ..; \
	   sed < hpcm/HPCM_${VERSION}_Signatures \
	       -e '1,/ ======   MD5 Signatures:$$/'d \
	       -e '/ ======   SHA1 Signatures:$$/,$$'d | \
	       md5sum --check 2>&1 | \
	       sed -e '/^[^ 	]*: OK$$/d' \
	           -e '/: No such file or directory$$/d'
	cd ..; \
	   sed < hpcm/HPCM_${VERSION}_Signatures \
	       -e '1,/ ======   SHA1 Signatures:$$/'d | \
	       sha1sum --check 2>&1 | \
	       sed -e '/^[^ 	]*: OK$$/d' \
	           -e '/: No such file or directory$$/d'

# Make cvsroot, a directory that holds the cvs files
# to be signature summed.  You must make hpcm_cvs_
# ${VERSION}${TAREXT} by hand: see `make tar' at the
# beginning of this file.
#
cvsroot:	hpcm_${VERSION}_cvs${TAREXT}
	rm -rf cvsroot
	mkdir cvsroot
	cd cvsroot; \
	    tar xf ../hpcm_${VERSION}_cvs${TAREXT} \
	        ${TARUNZIP}

# Make CVS Signatures File:
#
HPCM_${VERSION}_CVS_Signatures:	\
		signatures_header Makefile cvsroot
	rm -f HPCM_${VERSION}_CVS_Signatures
	echo "HPCM ${VERSION} CVS" \
	     > HPCM_${VERSION}_CVS_Signatures
	echo "---- ------ ---" \
	     >> HPCM_${VERSION}_CVS_Signatures
	echo "" \
	     >> HPCM_${VERSION}_CVS_Signatures
	echo "${COPYRIGHT}" \
	     >> HPCM_${VERSION}_CVS_Signatures
	echo "" \
	     >> HPCM_${VERSION}_CVS_Signatures
	cat signatures_header \
	     >>  HPCM_${VERSION}_CVS_Signatures
	echo "" \
	     >> HPCM_${VERSION}_CVS_Signatures
	echo "HPCM Version: ${VERSION}" \
	     >>  HPCM_${VERSION}_CVS_Signatures
	echo "Date of Signatures:" \
	     "`date`" \
	     >>  HPCM_${VERSION}_CVS_Signatures
	echo " ====== " " ====== " \
	     " ====== " " ====== " \
	     " ====== " " ====== " \
	     " MD5 Signatures:" \
	     >> HPCM_${VERSION}_CVS_Signatures
	md5sum hpcm_${VERSION}_cvs${TAREXT} \
	  >>  HPCM_${VERSION}_CVS_Signatures
	cd cvsroot; md5sum `find . -type f -print | \
	                    sed -e '/^\.\//s///' ` \
	  >> ../HPCM_${VERSION}_CVS_Signatures
	echo " ====== " " ====== " \
	     " ====== " " ====== " \
	     " ====== " " ====== " \
	     " SHA1 Signatures:" \
	     >> HPCM_${VERSION}_CVS_Signatures
	sha1sum hpcm_${VERSION}_cvs${TAREXT} \
	  >>  HPCM_${VERSION}_CVS_Signatures
	cd cvsroot; sha1sum `find . -type f -print | \
	                    sed -e '/^\.\//s///' ` \
	  >> ../HPCM_${VERSION}_CVS_Signatures
	chmod a-wx HPCM_${VERSION}_CVS_Signatures

# Check CVS Signatures
#
cvscheck:	cvsroot
	cd cvsroot; \
	   sed < ../HPCM_${VERSION}_CVS_Signatures \
	       -e '1,/ ======   MD5 Signatures:$$/'d \
	       -e '/ ======   SHA1 Signatures:$$/,$$'d \
	       -e '/hpcm_.*_cvs\${TAREXT}/s//..\/&/' | \
	       md5sum --check 2>&1 | \
	       sed -e '/^[^ 	]*: OK$$/d'
	cd cvsroot; \
	   sed < ../HPCM_${VERSION}_CVS_Signatures \
	       -e '1,/ ======   SHA1 Signatures:$$/'d \
	       -e '/hpcm_.*_cvs\${TAREXT}/s//..\/&/' | \
	       sha1sum --check 2>&1 | \
	       sed -e '/^[^ 	]*: OK$$/d'
		

# Make tar files.
#
tar:	hpcm_${VERSION}${TAREXT} \
        hpcm_${VERSION}_solutions${TAREXT} \
        hpcm_${VERSION}_${NONDIS}${TAREXT}

hpcm_${VERSION}${TAREXT}:	Makefile \
		HPCM_${VERSION}_Signatures \
		HPCM_${VERSION}_CVS_Signatures \
		hpcm_${VERSION}.files
	d=`pwd`;d=`basename $$d`; test $$d = hpcm
	rm -f hpcm_${VERSION}${TAREXT}
	cd ..; tar cf hpcm/hpcm_${VERSION}${TAREXT} \
	   hpcm/HPCM_${VERSION}_Signatures \
	   hpcm/HPCM_${VERSION}_CVS_Signatures \
	   `cat hpcm/hpcm_${VERSION}.files` \
	   ${TARZIP}

hpcm_${VERSION}_solutions${TAREXT}:	Makefile \
                        hpcm_${VERSION}_solutions.files
	d=`pwd`;d=`basename $$d`; test $$d = hpcm
	rm -f hpcm_${VERSION}_solutions${TAREXT}
	cd ..; \
	   tar cf \
	       hpcm/hpcm_${VERSION}_solutions${TAREXT} \
	   `cat hpcm/hpcm_${VERSION}_solutions.files` \
	   ${TARZIP}

hpcm_${VERSION}_${NONDIS}${TAREXT}:	Makefile \
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

# Make a sorted list of all files in a distribution.
#
file_list.files:	hpcm_${VERSION}.files \
		hpcm_${VERSION}_solutions.files \
		hpcm_${VERSION}_${NONDIS}.files
	rm -f file_list.files
	cat hpcm_${VERSION}.files \
	    hpcm_${VERSION}_solutions.files \
	    hpcm_${VERSION}_${NONDIS}.files | \
	    sort > file_list.files

# Make a sorted list of all files checked into cvs.
# This can be diff'ed with file_list.files to see
# which files are missing from the distribution.
#
CVSEXP = ^.*cvsroot[^\/]*\/hpcm\/\(.*\),v$$
cvs.files:	NO_SUCH_FILE
	rm -f cvs.files
	cvs log -R | \
	    sed -n '-e/${CVSEXP}/s//hpcm\/\1/p' | \
	    grep -v '\/Attic\/' | \
	    grep -v '\/\.cvsignore$$' | \
	    sort > cvs.files

# Make web directory to distribute HPCM.
#
web:		Makefile STATUS \
		hpcm_${VERSION}${TAREXT} \
		hpcm_${VERSION}_solutions${TAREXT} \
		HPCM_${VERSION}_Signatures \
		HPCM_${VERSION}_CVS_Signatures \
		web_index.html \
		web_solutions_index.html
	cd judge/doc; make overview.txt \
			   installing_hpcm.txt \
			   judging.txt
	rm -rf web
	mkdir web
	cp -p hpcm_${VERSION}${TAREXT} \
	      HPCM_${VERSION}_Signatures \
	      HPCM_${VERSION}_CVS_Signatures \
	      STATUS \
	      web
	cp -p judge/doc/overview.txt \
	      judge/doc/installing_hpcm.txt \
	      judge/doc/judging.txt \
	      web
	FILE=web/hpcm_${VERSION}${TAREXT}; \
	   MD5SUM=`md5sum $${FILE}`; \
	   MD5SUM=`expr "$${MD5SUM}" : \
	                 '[ 	]*\([^ 	]*\)[ 	]' `; \
	   SHA1SUM=`sha1sum $${FILE}`; \
	   SHA1SUM=`expr "$${SHA1SUM}" : \
	                 '[ 	]*\([^ 	]*\)[ 	]' `; \
	   sed \
	      <web_index.html \
	      -e '/VERSION/s//${VERSION}/g' \
	      -e '/TAREXT/s//${TAREXT}/g' \
	      -e '/CONTACT/s//${WEB_CONTACT}/g' \
	      -e '/MD5SUM/s//'$${MD5SUM}'/g' \
	      -e '/SHA1SUM/s//'$${SHA1SUM}'/g' \
	      > web/index.html
	chmod 444 web/*
	chmod 755 web
	@if test "${WEB_PASSWORD}" = ""; \
	then echo NOTE: WEB_PASSWORD not set';' \
	          Not putting solutions into web; \
	else make web_solutions; fi

web_solutions:	Makefile \
		web/index.html \
		web_solutions_index.html \
		hpcm_${VERSION}_solutions${TAREXT}
	test "${WEB_PASSWORD}" != ""
	mkdir web/private 
	mkdir web/private/${WEB_PASSWORD}
	chmod 711 web/private
	chmod 755 web/private/${WEB_PASSWORD}
	cp -p hpcm_${VERSION}_solutions${TAREXT} \
	      web/private/${WEB_PASSWORD}
	FILE=web/private/*/*${TAREXT}; \
	   MD5SUM=`md5sum $${FILE}`; \
	   MD5SUM=`expr "$${MD5SUM}" : \
	                 '[ 	]*\([^ 	]*\)[ 	]' `; \
	   SHA1SUM=`sha1sum $${FILE}`; \
	   SHA1SUM=`expr "$${SHA1SUM}" : \
	                 '[ 	]*\([^ 	]*\)[ 	]' `; \
	  sed \
	    <web_solutions_index.html \
	    -e '/VERSION/s//${VERSION}/g' \
	    -e '/TAREXT/s//${TAREXT}/g' \
	    -e '/CONTACT/s//${WEB_CONTACT}/g' \
	    -e '/MD5SUM/s//'$${MD5SUM}'/g' \
	    -e '/SHA1SUM/s//'$${SHA1SUM}'/g' \
	    >web/private/${WEB_PASSWORD}/index.html
	chmod 444 web/private/${WEB_PASSWORD}/*

hpcm_${VERSION}_web${TAREXT}:	web
	rm -f hpcm_${VERSION}_web${TAREXT}
	cd web; tar cf ../hpcm_${VERSION}_web${TAREXT} \
		       * ${TARZIP}
	chmod a-wx hpcm_${VERSION}_web${TAREXT}

archive:	ARCHIVE/hpcm_${VERSION}

ARCHIVE/hpcm_${VERSION}:	${ARCHIVE_FILES}
	rm -rf ARCHIVE/hpcm_${VERSION}
	mkdir ARCHIVE/hpcm_${VERSION}
	cp -p ${ARCHIVE_FILES} ARCHIVE/hpcm_${VERSION}
	chmod 400 ARCHIVE/hpcm_${VERSION}/*

cleanarchive:
	rm -rf ARCHIVE/hpcm_${VERSION}

cleanall:
	for x in `find . -name Makefile -print`; \
	    do \
	    ( cd `dirname $$x`; make clean ) \
	    done

clean:	cleantar cleanslocs cleancvs cleanweb

cleantar:
	rm -f HPCM_${VERSION}_Signatures \
	      hpcm_${VERSION}.files \
	      hpcm_${VERSION}_solutions.files \
	      hpcm_${VERSION}_${NONDIS}.files \
	      file_list.files cvs.files \
	      hpcm_${VERSION}${TAREXT} \
	      hpcm_${VERSION}_solutions${TAREXT} \
	      hpcm_${VERSION}_${NONDIS}${TAREXT}

cleanslocs:
	rm -f *.slocs

cleancvs:
	rm -rf cvsroot
	rm -f HPCM_${VERSION}_CVS_Signatures

cleanweb:
	rm -rf web hpcm_${VERSION}_web${TAREXT}
