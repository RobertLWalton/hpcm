# HPCM Contestant Help Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Thu Feb 14 09:38:19 EST 2013
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2013/02/14 15:48:21 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.45 $

# `make help.pdf' and `make demos.pdf' make printouts of
# all the help and demos files respectively.  Note that
# there is no `help/this_contest' file in help.pdf.
#
# In addition to help.pdf and demos.pdf there are files
# advanced_help.pdf and advanced_demos.pdf that have
# information about more sophisticated algorithms.
# To make these, just use `make advanced_help.pdf',
# etc.
#
# To make web help pages, execute `make web', copy
# the `web.tar' file to your web server and unpack
# it to make a directory containing the `index.html'
# file, .pdf files, and files listed in indices.
# web.tar contains the contents of the `web'
# subdirectory.

all:	modes STL_doc

pdf:	help.pdf \
	c_basic_io_demos.pdf \
	c_advanced_io_demos.pdf \
	c_algorithm_demos.pdf \
	cc_basic_io_demos.pdf \
	cc_advanced_io_demos.pdf \
	cc_algorithm_demos.pdf \
	java_basic_io_demos.pdf \
	java_advanced_io_demos.pdf \
	java_algorithm_demos.pdf \
	lsp_basic_io_demos.pdf \
	lsp_advanced_io_demos.pdf \
	lsp_algorithm_demos.pdf

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
	common/c \
	common/c++ \
	common/gdb \
	common/java \
	common/jdebug \
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

help_index:	index
	rm -f help_index help_index.html \
	      help_index.files
	../../judge/bin/makehelp \
	    help_index "HELP INDEX" index ${WEB} \
	    ${HELP_FILES}

help.ps:	help_index
	rm -rf help.ps
	../../judge/bin/fprint > help.ps -o \
	    `cat help_index.files`

formal_help_index:	index
	rm -f formal_help_index \
	      formal_help_index.html \
	      formal_help_index.files
	../../judge/bin/makehelp \
	    formal_help_index "HELP INDEX" index \
	    ${WEB} ${FORMAL_HELP_FILES}

formal_help.ps:	formal_help_index
	rm -rf formal_help.ps
	../../judge/bin/fprint > formal_help.ps -o \
	    `cat formal_help_index.files`

advanced_help_index:	index
	rm -f advanced_help_index \
	      advanced_help_index.html \
	      advanced_help_index.files
	../../judge/bin/makehelp \
	    advanced_help_index "ADVANCED HELP INDEX" \
	    index ${WEB} ${ADVANCED_HELP_FILES}

advanced_help.ps:	advanced_help_index
	rm -rf advanced_help.ps
	../../judge/bin/fprint > advanced_help.ps -o \
	    `cat advanced_help_index.files`

# If we replace %_demos_index by %demos_index we get
# circularity with ../../problem_library/demos_index.
#
%_demos_index:	../../problem_library/demos_index
	rm -f $*_demos_index \
	      $*_demos_index.html \
	      $*_demos_index.files
	../../judge/bin/makehelp \
	    $*_demos_index ${DEMOS_TITLE} \
	    ../../problem_library/demos_index ${WEB} \
	    $*_help

%_demos.ps:	%_demos_index
	rm -rf $*_demos.ps
	../../judge/bin/fprint > $*_demos.ps -o \
	    `egrep -v '.*\.(in|test).*' \
	           $*_demos_index.files`

c_basic_io_demos_index: \
    DEMOS_TITLE = "C BASIC IO DEMOS INDEX"
c_advanced_io_demos_index: \
    DEMOS_TITLE = "C ADVANCED IO DEMOS INDEX"
c_algorithm_demos_index: \
    DEMOS_TITLE = "C ALGORITHM DEMOS INDEX"

cc_basic_io_demos_index: \
    DEMOS_TITLE = "C++ BASIC IO DEMOS INDEX"
cc_advanced_io_demos_index: \
    DEMOS_TITLE = "C++ ADVANCED IO DEMOS INDEX"
cc_algorithm_demos_index: \
    DEMOS_TITLE = "C++ ALGORITHM DEMOS INDEX"

java_basic_io_demos_index: \
    DEMOS_TITLE = "JAVA BASIC IO DEMOS INDEX"
java_advanced_io_demos_index: \
    DEMOS_TITLE = "JAVA ADVANCED IO DEMOS INDEX"
java_algorithm_demos_index: \
    DEMOS_TITLE = "JAVA ALGORITHM DEMOS INDEX"

lsp_basic_io_demos_index: \
    DEMOS_TITLE = "COMMONLISP BASIC IO DEMOS INDEX"
lsp_advanced_io_demos_index: \
    DEMOS_TITLE = "COMMONLISP ADVANCED IO DEMOS INDEX"
lsp_algorithm_demos_index: \
    DEMOS_TITLE = \
        "COMMONLISP ALGORITHM DEMOS INDEX"

all_demos_index: DEMOS_TITLE = "DEMOS INDEX"

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
	    `grep -v 'sample.*' \
	             advanced_demos_index.files`

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
	make WEB=web help_index all_demos_index
	make pdf
	mkdir web/algorithms
	chmod 755 web/algorithms
	cp -p ../../wiki/print/*.pdf web/algorithms
	chmod 444 web/algorithms/*.pdf
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
	       *help_index *demos_index \
	       formal_help_index \
	       advanced_help_index \
	       advanced_demos_index \
	       *.files *.ps *.pdf \
	       *_index.html \
	       STL_doc
