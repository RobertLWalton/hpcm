# HPCM Contestant Help Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Fri Aug  2 10:27:20 EDT 2002
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/08/02 14:27:36 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.9 $

# To make web help pages, execute `make web_page' and
# copy the `web' directory to you web server, where it
# will become a directory containing an `index.html'
# file.  The web.tar file contains the contents of the
# `web' directory, to make copying easy.

all:	emailsh

# Kill all implicit rules
#
.SUFFIXES:

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

web_page:	all
	rm -rf web.tar web
	mkdir web
	cp -rpL `ls|grep -v '^web$$'` web
	cp -rpL ../../problem_library/demos web/demos
	rm -rf `find web -name CVS -print`
	rm -f web/Makefile
	find web -type f -exec chmod 444 {} \;
	find web -type d -exec chmod 755 {} \;
	cd web; tar cf ../web.tar *

clean:
	rm -rf email/sh/hpcm_extract \
	       email/sh/hpcm_submit \
	       email/sh/hpcm_clisp \
	       email/sh/hpcm_clisp.lsp \
	       web web.tar
