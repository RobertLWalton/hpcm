# HPCM Secure Source Code Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Fri Dec  8 23:29:13 EST 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2000/12/09 05:12:58 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.7 $

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
	chmod a+rx hpcm_sandbox
	chmod u+s hpcm_sandbox
	-mv -f ${SECURE_BIN}/hpcm_sandbox \
	       ${SECURE_BIN}/hpcm_sandbox-
	mv -f hpcm_sandbox \
	      ${SECURE_BIN}/hpcm_sandbox
	ls -l ${SECURE_BIN}/hpcm_sandbox*
	@if test ! -L ../../judge/bin/hpcm_sandbox_x; \
	    then make --no-print-directory \
	              hpcm_sandbox_x; \
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
