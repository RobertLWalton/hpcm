# HPCM Contestant Help Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Wed Oct 13 09:14:49 EDT 2010
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2010/10/13 13:34:23 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.31 $

# `make help.pdf' and `make demos.pdf' make printouts of
# all the help and demos files respectively.  Note that
# there is no `help/this_contest' file in help.pdf.
#
# Execute `make EXTRAS="filename ..." help+.pdf' to
# make a .pdf file with 'filename ...' followed by the
# help index and the help files.  Typically filename
# is a version of this_contest.
#
# You can also make help.ps, help+.ps, and demos.ps,
# which are just the .ps versions of the .pdf files.
#
# In addition to help.ps and demos.ps there are files
# advanced_help.ps and advanced_demos.ps that have
# information about more sophisticated algorithms.
# To make these, just use `make advanced_help.ps',
# etc.
#
# To make web help pages, execute `make web' and copy
# the `web' directory to you web server, where it will
# become a directory containing an `index.html' file.
# The web.tar file contains the contents of the `web'
# directory, to make copying easy.  The web page
# contains help.pdf and demos.pdf, as well individual
# copies of all the help and demos files and indices
# for these.

all:	emailsh modes STL_doc

pdf:	help.pdf demos.pdf \
	advanced_help.pdf advanced_demos.pdf
ps:	help.ps demos.ps \
	advanced_help.ps advanced_demos.ps

# Kill all implicit rules and make new rule:
#
.SUFFIXES:
.SUFFIXES: .ps .pdf
.ps.pdf:
	ps2pdf $*.ps $*.pdf

HELP_FILES = \
	contest/formal_contest \
	contest/informal_contest \
	contest/untimed_contest \
	common/xterminals \
	common/solving \
	email/email_solving \
	common/input \
	common/output \
	common/print \
	common/commonlisp \
	common/scores \
	common/scoreboard \
	common/c++ \
	common/java \
	common/advice

# Removed from help files.
#	email/email_unix_tools

ADVANCED_HELP_FILES = \
	advanced/2D_geometry \
	advanced/dynamic_programming \
	advanced/breadth_first_search \
	advanced/constrained_search \
	advanced/network_flows \
	advanced/optimal_matchings \
	advanced/3D_geometry

# Not included
#	advanced/geometry_exercises


DEMOS_FILES = \
	../../problem_library/demos/count/Makefile \
	../../problem_library/demos/count/README \
	../../problem_library/demos/count/count.in \
	../../problem_library/demos/count/count.test \
	../../problem_library/demos/count/count.txt \
	../../problem_library/demos/count/count1.c \
	../../problem_library/demos/count/count1.cc \
	../../problem_library/demos/count/count1.java \
	../../problem_library/demos/count/count1.lsp \
	../../problem_library/demos/javaio/javaio.java \
	../../problem_library/demos/javaio/Makefile \
	../../problem_library/demos/javaio/javaio.in \
	../../problem_library/demos/javaio/javaio.test

ADVANCED_DEMOS_FILES = \
	../../problem_library/demos/intersections/intersections.txt \
	../../problem_library/demos/intersections/intersections.cc \
	../../problem_library/demos/csearch/csearch.txt \
	../../problem_library/demos/csearch/csearch.cc

help_index:	index
	rm -f help_index
	T='	'; \
	echo >>help_index \
	     "HELP INDEX$$T$$T    `date`"
	echo >>help_index ""
	A='^\*\*\*[[:space:]][[:space:]]*'; \
	D='[^[:space:]]*\/'; \
	M='[^[:space:]\/]*'; \
	E='[[:space:]].*'; \
	sed -n <index >>help_index \
	    -e ': start' \
	    -e '/^#[[:space:]]*end_help_index/Q' \
	    -e '/'$$A'.* not_help_index/b skip' \
	    -e '/^#/d' \
	    -e 'b accept' \
	    -e ': skip' \
	    -e '/^[A-Z]/b start' \
	    -e 's/.*//' \
	    -e 'n' \
	    -e 'b skip' \
	    -e ': accept' \
	    -e 's/'$$A$$D'\('$$M'\)'$$E'/\1/' \
	    -e 'p'

demos_index:	../../problem_library/index
	rm -f demos_index
	T='	'; \
	echo >>demos_index \
	     "DEMOS INDEX$$T$$T    `date`"
	echo >>demos_index ""
	sed -n <../../problem_library/index >>demos_index \
	    -e '/^#[[:space:]]*end_demos_index/Q' \
	    -e '/^#/d' \
	    -e '/^\*\*\*[[:space:]]/d' \
	    -e 'p'

advanced_help_index:	index
	rm -f advanced_help_index
	T='	'; \
	echo >>advanced_help_index \
	     "ADVANCED HELP INDEX$$T   `date`"
	echo >>advanced_help_index ""
	A='^\*\*\*[[:space:]][[:space:]]*'; \
	D='[^[:space:]]*\/'; \
	M='[^[:space:]\/]*'; \
	E='[[:space:]].*'; \
	sed -n <index >>advanced_help_index \
	    -e ': start' \
	    -e '/^#[[:space:]]*begin_advanced_help/b accept' \
	    -e 's/.*//' \
	    -e 'n' \
	    -e 'b start' \
	    -e ': accept' \
	    -e '/^#[[:space:]]*end_advanced_help/Q' \
	    -e '/^#/b skip' \
	    -e 's/'$$A$$D'\('$$M'\)'$$E'/\1/' \
	    -e 'p' \
	    -e ': skip' \
	    -e 's/.*//' \
	    -e 'n' \
	    -e 'b accept'

advanced_demos_index:	../../problem_library/index
	rm -f advanced_demos_index
	T='	'; \
	echo >>advanced_demos_index \
	     "ADVANCED DEMOS INDEX$$T   `date`"
	echo >>advanced_demos_index ""
	sed -n <../../problem_library/index \
		>>advanced_demos_index \
	    -e ': start' \
	    -e '/^#[[:space:]]*begin_advanced_demos/b accept' \
	    -e 's/.*//' \
	    -e 'n' \
	    -e 'b start' \
	    -e ': accept' \
	    -e '/^#[[:space:]]*end_advanced_demos/Q' \
	    -e '/^#/b skip' \
	    -e '/^\*\*\*[[:space:]]/b skip' \
	    -e 'p' \
	    -e ': skip' \
	    -e 's/.*//' \
	    -e 'n' \
	    -e 'b accept'

help.ps:	help_index ${HELP_FILES}
	rm -rf help.ps
	../../judge/bin/fprint > help.ps -o \
	    help_index ${HELP_FILES}

help+.ps:	help_index ${HELP_FILES}
	rm -rf help+.ps
	../../judge/bin/fprint > help+.ps -o \
	    ${EXTRAS} help_index ${HELP_FILES}

demos.ps:	demos_index ${DEMOS_FILES}
	rm -rf demos.ps
	../../judge/bin/fprint > demos.ps -o \
	    demos_index ${DEMOS_FILES}

advanced_help.ps:	advanced_help_index \
			${ADVANCED_HELP_FILES}
	rm -rf advanced_help.ps
	../../judge/bin/fprint > advanced_help.ps -o \
	    advanced_help_index ${ADVANCED_HELP_FILES}

advanced_demos.ps:	advanced_demos_index \
			${ADVANCED_DEMOS_FILES}
	rm -rf advanced_demos.ps
	../../judge/bin/fprint \
	    > advanced_demos.ps -o \
	    advanced_demos_index \
	    ${ADVANCED_DEMOS_FILES}

emailsh:	email/sh/hpcm_extract \
		email/sh/hpcm_submit \
		email/sh/hpcm_clisp \
		email/sh/hpcm_clisp.lsp

email/sh/hpcm_extract:
	cd email/sh; \
	    ln -s ../../../bin/hpcm_extract .

email/sh/hpcm_submit:
	cd email/sh; \
	    ln -s ../../../bin/hpcm_submit .

email/sh/hpcm_clisp:
	cd email/sh; \
	    ln -s ../../../bin/hpcm_clisp .

email/sh/hpcm_clisp.lsp:
	cd email/sh; \
	    ln -s ../../../bin/hpcm_clisp.lsp .

# Make STL HTML documentation directory.
#
STL_doc:	private/STL_doc.tar.gz
	rm -rf STL_doc
	tar zxpf private/STL_doc.tar.gz
	touch STL_doc
	find STL_doc -type d -exec chmod a+rx {} \;
	find STL_doc -type f -exec chmod a+r {} \;

# Web_do merely causes `make web' to remake the
# web directory even if it exists.
#
web_do:

web:	all help.pdf demos.pdf \
	advanced_help.pdf advanced_demos.pdf web_do
	rm -rf web.tar web
	mkdir web
	cp -rpL `ls | grep -v '^web$$' | \
	              grep -v '^private$$' | \
	              grep -v '^help.ps$$' | \
	              grep -v '^help+.ps$$' | \
	              grep -v '^demos.ps$$' | \
	              grep -v '^advanced_help.ps$$' | \
	              grep -v '^advanced_demos.ps$$' ` \
	   web
	cp -rpL ../../problem_library/demos web/demos
	rm -f `find web -name '*.rc' -print`
	rm -rf `find web -name CVS -print`
	rm -f `find web \
	             -name '*this_contest_prototype' \
		     -print`
	rm -f web/Makefile
	find web -type f -exec chmod 444 {} \;
	find web -type d -exec chmod 755 {} \;
	cd web; tar cf ../web.tar *

modes:
	chmod a+x . .. ../.. ../../..

clean:
	rm -rf email/sh/hpcm_extract \
	       email/sh/hpcm_submit \
	       email/sh/hpcm_clisp \
	       email/sh/hpcm_clisp.lsp \
	       web web.tar \
	       help_index demos_index \
	       help.ps help+.ps demos.ps \
	       help.pdf help+.pdf demos.pdf \
	       STL_doc
