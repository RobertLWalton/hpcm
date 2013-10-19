# HPCM Secure Source Code Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sat Oct 19 04:47:38 EDT 2013
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2013/10/19 08:47:50 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.17 $

# Kill all implicit rules
#
.SUFFIXES:

SECURE_BIN=/sbin

all: hpcm_sandbox hpcm_sendmail hpcm_finish hpcm_wrapper

root_install:	${SECURE_BIN}/hpcm_sandbox
root_uninstall:	${SECURE_BIN}_uninstall

${SECURE_BIN}/hpcm_sandbox:	\
		${SECURE_BIN}/hpcm_sandbox.compiled
	test `id -un` = root
	test -d ${SECURE_BIN}
	test -w ${SECURE_BIN}
	-mv -f ${SECURE_BIN}/hpcm_sandbox \
	       ${SECURE_BIN}/hpcm_sandbox-
	cp -p ${SECURE_BIN}/hpcm_sandbox.compiled \
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

${SECURE_BIN}/hpcm_sandbox.compiled:	hpcm_sandbox.c
	test `id -un` = root
	test -d ${SECURE_BIN}
	test -w ${SECURE_BIN}
	rm -f hpcm_sandbox
	make --no-print-directory hpcm_sandbox
	-mv -f ${SECURE_BIN}/hpcm_sandbox.compiled \
	       ${SECURE_BIN}/hpcm_sandbox.compiled-
	mv -f hpcm_sandbox \
	      ${SECURE_BIN}/hpcm_sandbox.compiled
	chmod 644 ${SECURE_BIN}/hpcm_sandbox.compiled
	ls -l ${SECURE_BIN}/hpcm_sandbox.*

${SECURE_BIN}_uninstall:
	test `id -un` = root
	test -d ${SECURE_BIN}
	test -w ${SECURE_BIN}
	-mv -f ${SECURE_BIN}/hpcm_sandbox- \
	       ${SECURE_BIN}/hpcm_sandbox
	-mv -f ${SECURE_BIN}/hpcm_sandbox.compiled- \
	       ${SECURE_BIN}/hpcm_sandbox.compiled
	ls -l ${SECURE_BIN}/hpcm_sandbox*

hpcm_sandbox_x:	
	rm -f ../../judge/bin/hpcm_sandbox_x
	cd ../../judge/bin; \
	    ln -s ${SECURE_BIN}/hpcm_sandbox \
	          hpcm_sandbox_x

hpcm_sandbox:	hpcm_sandbox.c
	gcc -o hpcm_sandbox hpcm_sandbox.c

hpcm_sendmail:	hpcm_sendmail.c
	gcc -o hpcm_sendmail hpcm_sendmail.c

hpcm_finish:	hpcm_finish.c
	gcc -o hpcm_finish hpcm_finish.c

hpcm_wrapper:	hpcm_wrapper.c
	gcc -o hpcm_wrapper hpcm_wrapper.c

clean:
	rm -f hpcm_sandbox hpcm_sendmail hpcm_finish \
	      hpcm_wrapper
