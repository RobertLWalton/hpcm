# HPCM Contestant Help Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Wed Nov 28 00:21:59 EST 2012
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2012/11/28 06:12:50 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.35 $

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
# To make web help pages, execute `make web', copy
# the `web.tar' file to your web server and unpack
# it to make a directory containing the `index.html'
# file, .pdf files, and files listed in indices.
# web.tar contains the contents of the `web'
# subdirectory.

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

WEB = ""

FORMAL_HELP_FILES = \
	contest/formal_contest \
	common/xterminals \
	common/solving \
	common/input \
	common/output \
	common/print \
	common/scores \
	common/scoreboard \
	common/c++ \
	common/java \
	common/advice

HELP_FILES = \
	${FORMAL_HELP_FILES} \
	contest/informal_contest \
	contest/untimed_contest \
	email/email_solving \
	common/commonlisp


# Removed from help files.
#	email/email_unix_tools

ADVANCED_HELP_FILES = \
	advanced/dynamic_programming \
	advanced/2D_geometry \
	advanced/breadth_first_search \
	advanced/constrained_search \
	advanced/network_flows \
	advanced/optimal_matchings \
	advanced/3D_geometry

# Not included
#	advanced/geometry_exercises


DEMOS_FILES = \
	../../problem_library/demos/c/reverser/reverser.txt \
	../../problem_library/demos/c/reverser/sample.in \
	../../problem_library/demos/c/reverser/sample.test \
	../../problem_library/demos/c/reverser/reverser.c \
	../../problem_library/demos/c/summer/summer.txt \
	../../problem_library/demos/c/summer/sample.in \
	../../problem_library/demos/c/summer/sample.test \
	../../problem_library/demos/c/summer/summer.c

ADVANCED_DEMOS_FILES = \
	../../problem_library/demos/intersections/intersections.txt \
	../../problem_library/demos/intersections/intersections.cc \
	../../problem_library/demos/csearch/csearch.txt \
	../../problem_library/demos/csearch/csearch.cc

help_index:	index
	rm -f help_index help_index.html help_index.files
	../../judge/bin/makehelp \
	    help_index "HELP INDEX" index ${WEB} \
	    ${HELP_FILES}

help.ps:	help_index ${HELP_FILES}
	rm -rf help.ps
	../../judge/bin/fprint > help.ps -o \
	    help_index `cat help_index.files`

help+.ps:	help_index ${HELP_FILES}
	rm -rf help+.ps
	../../judge/bin/fprint > help+.ps -o \
	    ${EXTRAS} \
	    help_index `cat help_index.files`

formal_help_index:	index
	rm -f formal_help_index \
	      formal_help_index.html \
	      formal_help_index.files
	../../judge/bin/makehelp \
	    formal_help_index "HELP INDEX" index ${WEB} \
	    ${FORMAL_HELP_FILES}

formal_help.ps:	formal_help_index ${FORMAL_HELP_FILES}
	rm -rf formal_help.ps
	../../judge/bin/fprint > formal_help.ps -o \
	    formal_help_index \
	    `cat formal_help_index.files`

formal_help+.ps:	formal_help_index \
			${FORMAL_HELP_FILES}
	rm -rf formal_help+.ps
	../../judge/bin/fprint > formal_help+.ps -o \
	    ${EXTRAS} \
	    formal_help_index \
	    `cat formal_help_index.files`

advanced_help_index:	index
	rm -f advanced_help_index \
	      advanced_help_index.html \
	      advanced_help_index.files
	../../judge/bin/makehelp \
	    advanced_help_index "ADVANCED HELP INDEX" \
	    index ${WEB} ${ADVANCED_HELP_FILES}

advanced_help.ps:	advanced_help_index \
			${ADVANCED_HELP_FILES}
	rm -rf advanced_help.ps
	../../judge/bin/fprint > advanced_help.ps -o \
	    advanced_help_index \
	    `cat advanced_help_index.files`

demos_index:	../../problem_library/demos_index
	rm -f demos_index \
	      demos_index.html \
	      demos_index.files
	../../judge/bin/makehelp \
	    demos_index "DEMOS INDEX" \
	    ../../problem_library/demos_index ${WEB} c


demos.ps:	demos_index ${DEMOS_FILES}
	rm -rf demos.ps
	../../judge/bin/fprint > demos.ps -o \
	    demos_index `cat demos_index.files`

advanced_demos_index:	../../problem_library/index
	rm -f advanced_demos_index \
	      advanced_demos_index.html \
	      advanced_demos_index.files
	../../judge/bin/makehelp \
	    advanced_demos_index \
	    "ADVANCED DEMOS INDEX" \
	    ../../problem_library/index ${WEB} \
	    demos/intersections demos/csearch

advanced_demos.ps:	advanced_demos_index \
			${ADVANCED_DEMOS_FILES}
	rm -rf advanced_demos.ps
	../../judge/bin/fprint > advanced_demos.ps -o \
	    advanced_demos_index \
	    `cat advanced_demos_index.files`

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

# Web_do merely causes `make web' to remake web.tar
# even if it exists.
#
web_do:

web:	web_do
	rm -rf web web.tar
	make clean
	mkdir web
	make WEB=web all pdf
	rm -rf web.tar
	cp index.html web/index.html
	chmod 444 web/index.html
	cp *.pdf web/
	chmod 444 web/*.pdf
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
	       formal_help_index \
	       advanced_help_index \
	       advanced_demos_index \
	       *.files *.ps *.pdf \
	       STL_doc
