# HPCM Judge's Doc Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Fri Jan 12 22:01:02 EST 2001
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2001/01/13 02:58:53 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.6 $

# make
#	Makes documentation as fprintable .doc files in
#	this subdirectory tree.
#
# make fprint
#	Makes and fprints documentation.  Overview and
#	installing_hpcm are first, and the rest is
#	program documentation files in alphabetical
#	order.
#
# make fcheck
#	Makes and runs fcheck on documentation.

# List DOCS in order of printing.
#
DOCS=	overview.doc \
	installing_hpcm.doc \
        judge/autodispatch.doc \
	judge/autoinfo.doc \
	judge/autojudge.doc \
	judge/autoscore.doc \
	judge/chkpage.doc \
	judge/difffilter.doc \
	judge/dispatchmail.doc \
	judge/extractmail.doc \
	judge/fcheck.doc \
	judge/fcheckc.doc \
	judge/fprint.doc \
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
			../../judge/bin/autodispatch
	rm -f judge/autodispatch.doc
	-../../judge/bin/autodispatch -doc \
	    >judge/autodispatch.doc
judge/autoinfo.doc:	hpcm_judging.rc \
		        ../../judge/bin/autoinfo
	rm -f judge/autoinfo.doc
	-../../judge/bin/autoinfo -doc \
	    >judge/autoinfo.doc
judge/autojudge.doc:	hpcm_judging.rc \
		        ../../judge/bin/autojudge
	rm -f judge/autojudge.doc
	-../../judge/bin/autojudge -doc \
	    >judge/autojudge.doc
judge/autoscore.doc:	hpcm_judging.rc \
		        ../../judge/bin/autoscore
	rm -f judge/autoscore.doc
	-../../judge/bin/autoscore -doc \
	    >judge/autoscore.doc
judge/chkpage.doc:	hpcm_judging.rc \
			../../judge/bin/chkpage
	rm -f judge/chkpage.doc
	-../../judge/bin/chkpage -doc \
	    >judge/chkpage.doc
judge/difffilter.doc:	hpcm_judging.rc \
		        ../../judge/bin/difffilter
	rm -f judge/difffilter.doc
	-../../judge/bin/difffilter -doc \
	    >judge/difffilter.doc
judge/dispatchmail.doc:	hpcm_judging.rc \
			../../judge/bin/dispatchmail
	rm -f judge/dispatchmail.doc
	-../../judge/bin/dispatchmail -doc \
	    >judge/dispatchmail.doc
judge/extractmail.doc:	hpcm_judging.rc \
			../../judge/bin/extractmail
	rm -f judge/extractmail.doc
	-../../judge/bin/extractmail -doc \
	    >judge/extractmail.doc
judge/fcheck.doc:	hpcm_judging.rc \
			../../judge/bin/fcheck
	rm -f judge/fcheck.doc
	-../../judge/bin/fcheck -doc \
	    >judge/fcheck.doc
judge/fcheckc.doc:	hpcm_judging.rc \
			../../judge/bin/fcheckc
	rm -f judge/fcheckc.doc
	-../../judge/bin/fcheckc -doc \
	    >judge/fcheckc.doc
judge/fprint.doc:	hpcm_judging.rc \
			../../judge/bin/fprint
	rm -f judge/fprint.doc
	-../../judge/bin/fprint -doc \
	    >judge/fprint.doc
judge/hpcm_print_passwords.doc:	hpcm_judging.rc \
		../../judge/bin/hpcm_print_passwords
	rm -f judge/hpcm_print_passwords.doc
	-../../judge/bin/hpcm_print_passwords -doc \
	    >judge/hpcm_print_passwords.doc
judge/hpcm_sandbox.doc:	hpcm_judging.rc \
			../../judge/bin/hpcm_sandbox
	rm -f judge/hpcm_sandbox.doc
	-../../judge/bin/hpcm_sandbox -doc \
	    >judge/hpcm_sandbox.doc
installing_hpcm.doc:	hpcm_judging.rc \
			../../judge/bin/installing_hpcm
	rm -f installing_hpcm.doc
	-../../judge/bin/installing_hpcm -doc \
	    >installing_hpcm.doc
judge/lookat.doc:	hpcm_judging.rc \
		        ../../judge/bin/lookat
	rm -f judge/lookat.doc
	-../../judge/bin/lookat -doc >judge/lookat.doc
judge/makeMakefile.doc:	hpcm_judging.rc \
			../../judge/bin/makeMakefile
	rm -f judge/makeMakefile.doc
	-../../judge/bin/makeMakefile -doc \
	    >judge/makeMakefile.doc
judge/makejudgingdir.doc:	hpcm_judging.rc \
			../../judge/bin/makejudgingdir
	rm -f judge/makejudgingdir.doc
	-../../judge/bin/makejudgingdir -doc \
	    >judge/makejudgingdir.doc
judge/makescoreboard.doc:	hpcm_judging.rc \
			../../judge/bin/makescoreboard
	rm -f judge/makescoreboard.doc
	-../../judge/bin/makescoreboard -doc \
	    >judge/makescoreboard.doc
judge/manualreply.doc:	hpcm_judging.rc \
			../../judge/bin/manualreply
	rm -f judge/manualreply.doc
	-../../judge/bin/manualreply -doc \
	    >judge/manualreply.doc
overview.doc:	hpcm_judging.rc \
			../../judge/bin/overview
	rm -f overview.doc
	-../../judge/bin/overview -doc >overview.doc
judge/receivemail.doc:	hpcm_judging.rc \
			../../judge/bin/receivemail
	rm -f judge/receivemail.doc
	-../../judge/bin/receivemail -doc \
	    >judge/receivemail.doc
judge/reply.doc:	hpcm_judging.rc \
		        ../../judge/bin/reply
	rm -f judge/reply.doc
	-../../judge/bin/reply -doc >judge/reply.doc
judge/scoreboard.doc:	hpcm_judging.rc \
		        ../../judge/bin/scoreboard
	rm -f judge/scoreboard.doc
	-../../judge/bin/scoreboard -doc \
	    >judge/scoreboard.doc
judge/scorediff.doc:	hpcm_judging.rc \
		        ../../judge/bin/scorediff
	rm -f judge/scorediff.doc
	-../../judge/bin/scorediff -doc \
	    >judge/scorediff.doc
judge/scorefinder.doc:	hpcm_judging.rc \
			../../judge/bin/scorefinder
	rm -f judge/scorefinder.doc
	-../../judge/bin/scorefinder -doc \
	    >judge/scorefinder.doc
judge/startcontest.doc:	hpcm_judging.rc \
			../../judge/bin/startcontest
	rm -f judge/startcontest.doc
	-../../judge/bin/startcontest -doc \
	    >judge/startcontest.doc
judge/testproblems.doc:	hpcm_judging.rc \
			../../judge/bin/testproblems
	rm -f judge/testproblems.doc
	-../../judge/bin/testproblems -doc \
	    >judge/testproblems.doc
judge/tracker.doc:	hpcm_judging.rc \
		        ../../judge/bin/tracker
	rm -f judge/tracker.doc
	-../../judge/bin/tracker -doc \
	    >judge/tracker.doc
judge/trackfinder.doc:	hpcm_judging.rc \
			../../judge/bin/trackfinder
	rm -f judge/trackfinder.doc
	-../../judge/bin/trackfinder -doc \
	    >judge/trackfinder.doc

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
