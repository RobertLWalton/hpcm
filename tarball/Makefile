# HPCM Quick Tarball Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Mon Mar  7 14:07:09 EST 2011
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2011/03/07 19:12:12 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.7 $

CVSROOT=walton@login.seas.harvard.edu:/group/hc3/cvsroot

hpcm_2%.tgz:	IMPORTANT_README_2%
	rm -rf tmp
	make tmp/hpcm
	rm -f tmp/hpcm/IMPORTANT_README
	cp -p IMPORTANT_README_2$* \
	      tmp/hpcm/IMPORTANT_README
	(cd tmp/hpcm/judge/doc; make )
	(cd tmp/hpcm/judge/bin; make clean )
	(cd tmp/hpcm/judge/src; make clean )
	(cd tmp/hpcm/contestant/bin; make clean )
	(cd tmp/hpcm/secure/src; make clean )
	(cd tmp; rm -rf hpcm/problem_library/legal )
	(cd tmp; rm -rf hpcm/binders )
	(cd tmp; find hpcm -name CVS -exec rm -rf {} ';')
	(cd tmp; tar zcf ../$@ hpcm )

tmp/hpcm:
	rm -rf tmp
	mkdir tmp
	(cd tmp; cvsout ${CVSROOT} hpcm )
