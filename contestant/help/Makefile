# HPCM Contestant Help Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Tue Sep 24 08:39:41 EDT 2002
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/09/24 12:41:31 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.10 $

# To make web help pages, execute `make web' and copy
# the `web' directory to you web server, where it will
# become a directory containing an `index.html' file.
# The web.tar file contains the contents of the `web'
# directory, to make copying easy.

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


# Web_do merely causes `make web' to remake the
# web directory even if it exists.
#
web_do:

web:	all web_do
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
