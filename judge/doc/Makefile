# HPCM Judge's Doc Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Mon Nov 27 14:50:45 EST 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2000/11/27 20:45:42 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.2 $

DOCS=	autodispatch.doc \
	autoinfo.doc \
	autojudge.doc \
	autoscore.doc \
	difffilter.doc \
	dispatchmail.doc \
	extractmail.doc \
	hpcm_print_passwords.doc \
	hpcm_sandbox.doc \
	lookat.doc \
	makeMakefile.doc \
	makejudgingdir.doc \
	makescoreboard.doc \
	manualreply.doc \
	receivemail.doc \
	reply.doc \
	scoreboard.doc \
	scorediff.doc \
	scorefinder.doc \
	sendscoreboard.doc \
	startcontest.doc \
	testproblems.doc \
	tracker.doc \
	trackfinder.doc

all:	${DOCS} overview.doc

# Kill all implicit rules
#
.SUFFIXES:

fprint:	${DOCS} overview.doc
	fprint overview.doc ${DOCS}

fcheck:	${DOCS}
	fcheck ${DOCS}

hpcm_judging.rc:
	ln -s ../lib/hpcm_judging.rc

autodispatch.doc:	hpcm_judging.rc \
			../bin/autodispatch
	rm -f autodispatch.doc
	-../bin/autodispatch -doc >autodispatch.doc
autoinfo.doc:	hpcm_judging.rc ../bin/autoinfo
	rm -f autoinfo.doc
	-../bin/autoinfo -doc >autoinfo.doc
autojudge.doc:	hpcm_judging.rc ../bin/autojudge
	rm -f autojudge.doc
	-../bin/autojudge -doc >autojudge.doc
autoscore.doc:	hpcm_judging.rc ../bin/autoscore
	rm -f autoscore.doc
	-../bin/autoscore -doc >autoscore.doc
difffilter.doc:	hpcm_judging.rc ../bin/difffilter
	rm -f difffilter.doc
	-../bin/difffilter -doc >difffilter.doc
dispatchmail.doc:	hpcm_judging.rc \
			../bin/dispatchmail
	rm -f dispatchmail.doc
	-../bin/dispatchmail -doc >dispatchmail.doc
extractmail.doc:	hpcm_judging.rc \
			../bin/extractmail
	rm -f extractmail.doc
	-../bin/extractmail -doc >extractmail.doc
hpcm_print_passwords.doc:	hpcm_judging.rc \
			../bin/hpcm_print_passwords
	rm -f hpcm_print_passwords.doc
	-../bin/hpcm_print_passwords -doc >hpcm_print_passwords.doc
hpcm_sandbox.doc:	hpcm_judging.rc \
			../bin/hpcm_sandbox
	rm -f hpcm_sandbox.doc
	-../bin/hpcm_sandbox -doc >hpcm_sandbox.doc
lookat.doc:	hpcm_judging.rc ../bin/lookat
	rm -f lookat.doc
	-../bin/lookat -doc >lookat.doc
makeMakefile.doc:	hpcm_judging.rc \
			../bin/makeMakefile
	rm -f makeMakefile.doc
	-../bin/makeMakefile -doc >makeMakefile.doc
makejudgingdir.doc:	hpcm_judging.rc \
			../bin/makejudgingdir
	rm -f makejudgingdir.doc
	-../bin/makejudgingdir -doc >makejudgingdir.doc
makescoreboard.doc:	hpcm_judging.rc \
			../bin/makescoreboard
	rm -f makescoreboard.doc
	-../bin/makescoreboard -doc >makescoreboard.doc
manualreply.doc:	hpcm_judging.rc \
			../bin/manualreply
	rm -f manualreply.doc
	-../bin/manualreply -doc >manualreply.doc
overview.doc:	hpcm_judging.rc \
			../bin/overview
	rm -f overview.doc
	-../bin/overview -doc >overview.doc
receivemail.doc:	hpcm_judging.rc \
			../bin/receivemail
	rm -f receivemail.doc
	-../bin/receivemail -doc >receivemail.doc
reply.doc:	hpcm_judging.rc ../bin/reply
	rm -f reply.doc
	-../bin/reply -doc >reply.doc
scoreboard.doc:	hpcm_judging.rc ../bin/scoreboard
	rm -f scoreboard.doc
	-../bin/scoreboard -doc >scoreboard.doc
scorediff.doc:	hpcm_judging.rc ../bin/scorediff
	rm -f scorediff.doc
	-../bin/scorediff -doc >scorediff.doc
scorefinder.doc:	hpcm_judging.rc \
			../bin/scorefinder
	rm -f scorefinder.doc
	-../bin/scorefinder -doc >scorefinder.doc
sendscoreboard.doc:	hpcm_judging.rc \
			../bin/sendscoreboard
	rm -f sendscoreboard.doc
	-../bin/sendscoreboard -doc >sendscoreboard.doc
startcontest.doc:	hpcm_judging.rc \
			../bin/startcontest
	rm -f startcontest.doc
	-../bin/startcontest -doc >startcontest.doc
testproblems.doc:	hpcm_judging.rc \
			../bin/testproblems
	rm -f testproblems.doc
	-../bin/testproblems -doc >testproblems.doc
tracker.doc:	hpcm_judging.rc ../bin/tracker
	rm -f tracker.doc
	-../bin/tracker -doc >tracker.doc
trackfinder.doc:	hpcm_judging.rc \
			../bin/trackfinder
	rm -f trackfinder.doc
	-../bin/trackfinder -doc >trackfinder.doc

clean:
	rm -f scorediff hpcm_sandbox_x \
		hpcm_print_passwords

