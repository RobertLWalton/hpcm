# HPCM Judge's Doc Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sun Jan  7 07:50:28 EST 2001
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2001/01/07 13:51:25 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.3 $

# List DOCS in order of printing.
#
DOCS=	overview.doc \
	installing_hpcm.doc \
        judge/autodispatch.doc \
	judge/autoinfo.doc \
	judge/autojudge.doc \
	judge/autoscore.doc \
	judge/difffilter.doc \
	judge/dispatchmail.doc \
	judge/extractmail.doc \
	contestant/fprint.doc \
	contestant/help.doc \
	contestant/hpcm_extract.doc \
	contestant/hpcm_get.doc \
	secure/hpcm_make_accounts.doc \
	judge/hpcm_print_passwords.doc \
	contestant/hpcm_sandbox.doc \
	judge/hpcm_sandbox.doc \
	contestant/hpcm_sendmail.doc \
	secure/hpcm_set_passwords.doc \
	contestant/hpcm_submit.doc \
	secure/hpcm_TRASH_ACCOUNTS.doc \
	judge/lookat.doc \
	contestant/lpq.doc \
	judge/makeMakefile.doc \
	judge/makejudgingdir.doc \
	judge/makescoreboard.doc \
	judge/manualreply.doc \
	contestant/print.doc \
	contestant/print2.doc \
	judge/receivemail.doc \
	judge/reply.doc \
	contestant/scoreboard.doc \
	judge/scoreboard.doc \
	judge/scorediff.doc \
	judge/scorefinder.doc \
	contestant/SETUP_PROTOTYPE.doc \
	judge/startcontest.doc \
	judge/testproblems.doc \
	judge/tracker.doc \
	judge/trackfinder.doc \
	contestant/TRASH_ACCOUNT.doc \
	contestant/xcsh.doc

all:	judge contestant secure ${DOCS}

# Kill all implicit rules
#
.SUFFIXES:

fprint:	${DOCS}
	fprint ${DOCS}

fcheck:	${DOCS}
	fcheck ${DOCS}

hpcm_judging.rc:
	ln -s ../lib/hpcm_judging.rc

judge:
	mkdir judge

contestant:
	mkdir contestant

secure:
	mkdir secure

# ../bin docs
#
judge/autodispatch.doc:	hpcm_judging.rc \
			../bin/autodispatch
	rm -f judge/autodispatch.doc
	-../bin/autodispatch -doc \
	    >judge/autodispatch.doc
judge/autoinfo.doc:	hpcm_judging.rc ../bin/autoinfo
	rm -f judge/autoinfo.doc
	-../bin/autoinfo -doc \
	    >judge/autoinfo.doc
judge/autojudge.doc:	hpcm_judging.rc ../bin/autojudge
	rm -f judge/autojudge.doc
	-../bin/autojudge -doc \
	    >judge/autojudge.doc
judge/autoscore.doc:	hpcm_judging.rc ../bin/autoscore
	rm -f judge/autoscore.doc
	-../bin/autoscore -doc \
	    >judge/autoscore.doc
judge/difffilter.doc:	hpcm_judging.rc ../bin/difffilter
	rm -f judge/difffilter.doc
	-../bin/difffilter -doc \
	    >judge/difffilter.doc
judge/dispatchmail.doc:	hpcm_judging.rc \
			../bin/dispatchmail
	rm -f judge/dispatchmail.doc
	-../bin/dispatchmail -doc \
	    >judge/dispatchmail.doc
judge/extractmail.doc:	hpcm_judging.rc \
			../bin/extractmail
	rm -f judge/extractmail.doc
	-../bin/extractmail -doc \
	    >judge/extractmail.doc
judge/hpcm_print_passwords.doc:	hpcm_judging.rc \
			../bin/hpcm_print_passwords
	rm -f judge/hpcm_print_passwords.doc
	-../bin/hpcm_print_passwords -doc \
	    >judge/hpcm_print_passwords.doc
judge/hpcm_sandbox.doc:	hpcm_judging.rc \
			../bin/hpcm_sandbox
	rm -f judge/hpcm_sandbox.doc
	-../bin/hpcm_sandbox -doc \
	    >judge/hpcm_sandbox.doc
installing_hpcm.doc:	hpcm_judging.rc \
			../bin/installing_hpcm
	rm -f installing_hpcm.doc
	-../bin/installing_hpcm -doc \
	    >installing_hpcm.doc
judge/lookat.doc:	hpcm_judging.rc ../bin/lookat
	rm -f judge/lookat.doc
	-../bin/lookat -doc >judge/lookat.doc
judge/makeMakefile.doc:	hpcm_judging.rc \
			../bin/makeMakefile
	rm -f judge/makeMakefile.doc
	-../bin/makeMakefile -doc \
	    >judge/makeMakefile.doc
judge/makejudgingdir.doc:	hpcm_judging.rc \
			../bin/makejudgingdir
	rm -f judge/makejudgingdir.doc
	-../bin/makejudgingdir -doc \
	    >judge/makejudgingdir.doc
judge/makescoreboard.doc:	hpcm_judging.rc \
			../bin/makescoreboard
	rm -f judge/makescoreboard.doc
	-../bin/makescoreboard -doc \
	    >judge/makescoreboard.doc
judge/manualreply.doc:	hpcm_judging.rc \
			../bin/manualreply
	rm -f judge/manualreply.doc
	-../bin/manualreply -doc \
	    >judge/manualreply.doc
overview.doc:	hpcm_judging.rc \
			../bin/overview
	rm -f overview.doc
	-../bin/overview -doc >overview.doc
judge/receivemail.doc:	hpcm_judging.rc \
			../bin/receivemail
	rm -f judge/receivemail.doc
	-../bin/receivemail -doc >judge/receivemail.doc
judge/reply.doc:	hpcm_judging.rc ../bin/reply
	rm -f judge/reply.doc
	-../bin/reply -doc >judge/reply.doc
judge/scoreboard.doc:	hpcm_judging.rc ../bin/scoreboard
	rm -f judge/scoreboard.doc
	-../bin/scoreboard -doc >judge/scoreboard.doc
judge/scorediff.doc:	hpcm_judging.rc ../bin/scorediff
	rm -f judge/scorediff.doc
	-../bin/scorediff -doc >judge/scorediff.doc
judge/scorefinder.doc:	hpcm_judging.rc \
			../bin/scorefinder
	rm -f judge/scorefinder.doc
	-../bin/scorefinder -doc >judge/scorefinder.doc
judge/startcontest.doc:	hpcm_judging.rc \
			../bin/startcontest
	rm -f judge/startcontest.doc
	-../bin/startcontest -doc \
	    >judge/startcontest.doc
judge/testproblems.doc:	hpcm_judging.rc \
			../bin/testproblems
	rm -f judge/testproblems.doc
	-../bin/testproblems -doc \
	    >judge/testproblems.doc
judge/tracker.doc:	hpcm_judging.rc ../bin/tracker
	rm -f judge/tracker.doc
	-../bin/tracker -doc >judge/tracker.doc
judge/trackfinder.doc:	hpcm_judging.rc \
			../bin/trackfinder
	rm -f judge/trackfinder.doc
	-../bin/trackfinder -doc >judge/trackfinder.doc

# ../../secure/bin docs
#
secure/hpcm_make_accounts.doc:	\
		../../secure/bin/hpcm_make_accounts
	rm -f secure/hpcm_make_accounts.doc
	-../../secure/bin/hpcm_make_accounts -doc \
	    >secure/hpcm_make_accounts.doc
secure/hpcm_set_passwords.doc:	\
		../../secure/bin/hpcm_set_passwords
	rm -f secure/hpcm_set_passwords.doc
	-../../secure/bin/hpcm_set_passwords -doc \
	    >secure/hpcm_set_passwords.doc
secure/hpcm_TRASH_ACCOUNTS.doc:	\
		../../secure/bin/hpcm_TRASH_ACCOUNTS
	rm -f secure/hpcm_TRASH_ACCOUNTS.doc
	-../../secure/bin/hpcm_TRASH_ACCOUNTS -doc \
	    >secure/hpcm_TRASH_ACCOUNTS.doc

# ../../contestant/bin docs
#
contestant/fprint.doc:	../../contestant/bin/fprint
	rm -f contestant/fprint.doc
	-../../contestant/bin/fprint -doc \
	    >contestant/fprint.doc
contestant/help.doc:	../../contestant/bin/help
	rm -f contestant/help.doc
	-../../contestant/bin/help -doc \
	    >contestant/help.doc
contestant/hpcm_extract.doc:	\
		../../contestant/bin/hpcm_extract
	rm -f contestant/hpcm_extract.doc
	-../../contestant/bin/hpcm_extract -doc \
	    >contestant/hpcm_extract.doc
contestant/hpcm_get.doc:	\
		        ../../contestant/bin/hpcm_get
	rm -f contestant/hpcm_get.doc
	-../../contestant/bin/hpcm_get -doc \
	    >contestant/hpcm_get.doc
contestant/hpcm_sandbox.doc:	\
		../../contestant/bin/hpcm_sandbox
	rm -f contestant/hpcm_sandbox.doc
	-../../contestant/bin/hpcm_sandbox -doc \
	    >contestant/hpcm_sandbox.doc
contestant/hpcm_sendmail.doc:	\
		../../contestant/bin/hpcm_sendmail
	rm -f contestant/hpcm_sendmail.doc
	-../../contestant/bin/hpcm_sendmail -doc \
	    >contestant/hpcm_sendmail.doc
contestant/hpcm_submit.doc:	\
		../../contestant/bin/hpcm_submit
	rm -f contestant/hpcm_submit.doc
	-../../contestant/bin/hpcm_submit -doc \
	    >contestant/hpcm_submit.doc
contestant/lpq.doc:	../../contestant/bin/lpq
	rm -f contestant/lpq.doc
	-../../contestant/bin/lpq -doc \
	    >contestant/lpq.doc
contestant/print.doc:	../../contestant/bin/print
	rm -f contestant/print.doc
	-../../contestant/bin/print -doc \
	    >contestant/print.doc
contestant/print2.doc:	../../contestant/bin/print2
	rm -f contestant/print2.doc
	-../../contestant/bin/print2 -doc \
	    >contestant/print2.doc
contestant/scoreboard.doc:	\
		../../contestant/bin/scoreboard
	rm -f contestant/scoreboard.doc
	-../../contestant/bin/scoreboard -doc \
	    >contestant/scoreboard.doc
contestant/SETUP_PROTOTYPE.doc:	\
		../../contestant/bin/SETUP_PROTOTYPE
	rm -f contestant/SETUP_PROTOTYPE.doc
	-../../contestant/bin/SETUP_PROTOTYPE -doc \
	    >contestant/SETUP_PROTOTYPE.doc
contestant/TRASH_ACCOUNT.doc:	\
		../../contestant/bin/TRASH_ACCOUNT
	rm -f contestant/TRASH_ACCOUNT.doc
	-../../contestant/bin/TRASH_ACCOUNT -doc \
	    >contestant/TRASH_ACCOUNT.doc
contestant/xcsh.doc:	../../contestant/bin/xcsh
	rm -f contestant/xcsh.doc
	-../../contestant/bin/xcsh -doc \
	    >contestant/xcsh.doc

clean:
	rm -rf overview.doc installing_hpcm.doc \
	       judge contestant secure \
	       hpcm_judging.rc
