# HPCM Secure Source Code Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Wed Apr  3 08:08:35 EST 2002
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/04/03 13:07:13 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.9 $

# Kill all implicit rules
#
.SUFFIXES:

SECURE_BIN=/sbin

all: hpcm_sandbox hpcm_sendmail

root_install:	${SECURE_BIN}/hpcm_sandbox

${SECURE_BIN}/hpcm_sandbox:	hpcm_sandbox.c
	test `id -un` = root
	test -d ${SECURE_BIN}
	test -w ${SECURE_BIN}
	rm -f hpcm_sandbox
	make --no-print-directory hpcm_sandbox
	-mv -f ${SECURE_BIN}/hpcm_sandbox \
	       ${SECURE_BIN}/hpcm_sandbox-
	mv -f hpcm_sandbox \
	      ${SECURE_BIN}/hpcm_sandbox
	chmod 755 ${SECURE_BIN}/hpcm_sandbox
	chmod u+s ${SECURE_BIN}/hpcm_sandbox
	ls -l ${SECURE_BIN}/hpcm_sandbox*
	@if test ! -L ../../judge/bin/hpcm_sandbox_x; \
	 then \
	     make --no-print-directory hpcm_sandbox_x; \
	 elif cmp -s ${SECURE_BIN}/hpcm_sandbox \
	             ../../judge/bin/hpcm_sandbox_x; \
	 then \
	     do_nothing=; \
	 else \
	     make --no-print-directory hpcm_sandbox_x; \
	 fi

hpcm_sandbox_x:	
	rm -f ../../judge/bin/hpcm_sandbox_x
	cd ../../judge/bin; \
	    make SECURE_BIN="${SECURE_BIN}" \
			hpcm_sandbox_x

hpcm_sandbox:	hpcm_sandbox.c
	gcc -o hpcm_sandbox hpcm_sandbox.c

hpcm_sendmail:	hpcm_sendmail.c
	gcc -o hpcm_sendmail hpcm_sendmail.c

clean:
	rm -f hpcm_sandbox hpcm_sendmail
