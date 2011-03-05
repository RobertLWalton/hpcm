# HPCM Quick Tarball Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sat Mar  5 10:53:14 EST 2011
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2011/03/05 15:53:22 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.2 $

CVSROOT=walton@login.seas.harvard.edu:/group/hc3/cvsroot

hpcm_2%.tgz:	hpcm
	tar zcf hpcm_2%.tgz hpcm

hpcm:
	cvsout ${CVSROOT} hpcm
	(cd hpcm/judge/doc; make )
	rm -rf hpcm/problem_library/legal
