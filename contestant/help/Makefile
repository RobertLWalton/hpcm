# HPCM Contestant Help Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Thu Oct  7 08:26:10 EDT 2004
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2004/10/07 12:31:04 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.18 $

# To make web help pages, execute `make web' and copy
# the `web' directory to you web server, where it will
# become a directory containing an `index.html' file.
# The web.tar file contains the contents of the `web'
# directory, to make copying easy.
#
# Execute `make EXTRAS="filename ..." help+.ps' to
# make a .ps file with 'filename ...' followed by the
# help index and the help files.  Typically filename
# is a version of this_contest.

all:	emailsh

ps:	help.ps demos.ps

# Kill all implicit rules
#
.SUFFIXES:

HELP_FILES = \
	formal/formal_contest \
	informal/informal_contest \
	common/xterminals \
	email/email_contest \
	email/email_unix_tools \
	common/solving \
	common/input \
	common/output \
	common/print \
	common/commonlisp \
	common/scores \
	common/scoreboard \
	common/c++ \
	common/java \
	common/advice \
	advanced/breadth_first_search \
	advanced/dynamic_programming \
	advanced/2D_geometry \
	advanced/3D_geometry \
	advanced/network_flows \
	advanced/optimal_matchings

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
	    -e '/^#/d' \
	    -e '/'$$A'.* all/b skip' \
	    -e 'b accept' \
	    -e ': skip' \
	    -e 's/.*//' \
	    -e 'n' \
	    -e '/'$$A'.* all/b skip' \
	    -e '/'$$A'/!b skip' \
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
	    -e '/^#/d' \
	    -e '/^[^[:space:]].* Problems/,$$d' \
	    -e '/^\*\*\*[[:space:]]/d' \
	    -e 'p'

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


# Web_do merely causes `make web' to remake the
# web directory even if it exists.
#
web_do:

web:	all help.ps demos.ps web_do
	rm -rf web.tar web
	mkdir web
	cp -rpL `ls|grep -v '^web$$'` web
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

clean:
	rm -rf email/sh/hpcm_extract \
	       email/sh/hpcm_submit \
	       email/sh/hpcm_clisp \
	       email/sh/hpcm_clisp.lsp \
	       web web.tar \
	       help_index demos_index \
	       help.ps demos.ps
