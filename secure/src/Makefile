# HPCM Secure Source Code Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sat Jan 20 06:58:43 EST 2007
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2007/01/20 11:58:59 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.12 $

# Kill all implicit rules
#
.SUFFIXES:

SECURE_BIN=/sbin

all: hpcm_sandbox hpcm_sendmail hpcm_wrapper

root_install:	${SECURE_BIN}/hpcm_sandbox
root_uninstall:	${SECURE_BIN}_uninstall

${SECURE_BIN}/hpcm_sandbox:	hpcm_sandbox.c
	test `id -un` = root
	test -d ${SECURE_BIN}
	test -w ${SECURE_BIN}
	rm -f hpcm_sandbox
	make --no-print-directory hpcm_sandbox
	-mv -f ${SECURE_BIN}/hpcm_sandbox \
	       ${SECURE_BIN}/hpcm_sandbox-
	-mv -f ${SECURE_BIN}/hpcm_sandbox.compiled \
	       ${SECURE_BIN}/hpcm_sandbox.compiled-
	mv -f hpcm_sandbox \
	      ${SECURE_BIN}/hpcm_sandbox.compiled
	cp -p ${SECURE_BIN}/hpcm_sandbox.compiled \
	      ${SECURE_BIN}/hpcm_sandbox
	chmod 644 ${SECURE_BIN}/hpcm_sandbox.compiled
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

${SECURE_BIN}_uninstall:
	test `id -un` = root
	test -d ${SECURE_BIN}
	test -w ${SECURE_BIN}
	-mv -f ${SECURE_BIN}/hpcm_sandbox- \
	       ${SECURE_BIN}/hpcm_sandbox
	-mv -f ${SECURE_BIN}/hpcm_sandbox.compiled- \
	       ${SECURE_BIN}/hpcm_sandbox.compiled

hpcm_sandbox_x:	
	rm -f ../../judge/bin/hpcm_sandbox_x
	cd ../../judge/bin; \
	    ln -s ${SECURE_BIN}/hpcm_sandbox \
	          hpcm_sandbox_x

hpcm_sandbox:	hpcm_sandbox.c
	gcc -o hpcm_sandbox hpcm_sandbox.c

hpcm_sendmail:	hpcm_sendmail.c
	gcc -o hpcm_sendmail hpcm_sendmail.c

hpcm_wrapper:	hpcm_wrapper.c
	gcc -o hpcm_wrapper hpcm_wrapper.c

clean:
	rm -f hpcm_sandbox hpcm_sendmail hpcm_wrapper
