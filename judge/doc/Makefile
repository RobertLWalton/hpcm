# HPCM Judge's Doc Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Wed Jul 31 04:41:02 EDT 2002
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/07/31 08:35:36 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.19 $

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
	judging.doc \
        judge/autodispatch.doc \
	judge/autoinfo.doc \
	judge/autojudge.doc \
	judge/autoscore.doc \
	judge/chkpage.doc \
	judge/dispatchmail.doc \
	judge/hpcm_email_passwords.doc \
	judge/extractmail.doc \
	judge/fcheck.doc \
	judge/fcheckc.doc \
	judge/file_list.doc \
	judge/fprint.doc \
	contestant/filtered_diff.doc \
	contestant/fprint.doc \
	contestant/help.doc \
	contestant/hpcm_clisp.doc \
	contestant/hpcm_extract.doc \
	contestant/hpcm_extract_test_replies.doc \
	contestant/hpcm_get.doc \
	secure/hpcm_make_accounts.doc \
	judge/hpcm_print_passwords.doc \
	contestant/hpcm_sandbox.doc \
	judge/hpcm_sandbox.doc \
	contestant/hpcm_sendmail.doc \
	judge/hpcm_sendmail.doc \
	secure/hpcm_set_passwords.doc \
	contestant/hpcm_submit.doc \
	secure/hpcm_TRASH_ACCOUNTS.doc \
	judge/jprint.doc \
	judge/lookat.doc \
	judge/lpq.doc \
	contestant/lpq.doc \
	judge/makejudgingdir.doc \
	judge/makescoreboard.doc \
	judge/manualreply.doc \
	contestant/print.doc \
	contestant/print2.doc \
	contestant/printer_pipe.doc \
	judge/printer_broadcast.doc \
	judge/receivemail.doc \
	judge/remakeMakefile.doc \
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

# List sources in order of printing.
#
SOURCES = \
	../lib/hpcm_judging.rc \
	../lib/judging_common.tcl \
	../lib/display_common.tcl \
	../lib/scoring_common.tcl

all:	bin judge contestant secure test ${DOCS}

# Kill all implicit rules
#
.SUFFIXES:

fprint:	${DOCS} ${SOURCES}
	fprint ${DOCS} ${SOURCES}

fcheck:	${DOCS} ${SOURCES}
	fcheck ${DOCS}
	fcheckc ${SOURCES}

hpcm_judging.rc:
	ln -s ../lib/hpcm_judging.rc

bin:
	cd ../bin; make

judge:
	mkdir judge

contestant:
	mkdir contestant

secure:
	mkdir secure

test:
	mkdir test

clean:
	rm -rf overview.doc installing_hpcm.doc \
	       judging.doc \
	       judge contestant secure test \
	       hpcm_judging.rc

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
judge/dispatchmail.doc:	hpcm_judging.rc \
			../../judge/bin/dispatchmail
	rm -f judge/dispatchmail.doc
	-../../judge/bin/dispatchmail -doc \
	    >judge/dispatchmail.doc
judge/hpcm_email_passwords.doc:	hpcm_judging.rc \
		../../judge/bin/hpcm_email_passwords
	rm -f judge/hpcm_email_passwords.doc
	-../../judge/bin/hpcm_email_passwords -doc \
	    >judge/hpcm_email_passwords.doc
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
judge/file_list.doc:	hpcm_judging.rc \
			../../judge/bin/file_list
	rm -f judge/file_list.doc
	-../../judge/bin/file_list -doc \
	    >judge/file_list.doc
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
judge/hpcm_sendmail.doc:	hpcm_judging.rc \
			../../judge/bin/hpcm_sendmail
	rm -f judge/hpcm_sendmail.doc
	-../../judge/bin/hpcm_sendmail -doc \
	    >judge/hpcm_sendmail.doc
installing_hpcm.doc:	hpcm_judging.rc \
			../../judge/bin/installing_hpcm
	rm -f installing_hpcm.doc
	-../../judge/bin/installing_hpcm -doc \
	    >installing_hpcm.doc
judging.doc:	hpcm_judging.rc \
			../../judge/bin/judging
	rm -f judging.doc
	-../../judge/bin/judging -doc >judging.doc
judge/jprint.doc:	hpcm_judging.rc \
		        ../../judge/bin/jprint
	rm -f judge/jprint.doc
	-../../judge/bin/jprint -doc >judge/jprint.doc
judge/lookat.doc:	hpcm_judging.rc \
		        ../../judge/bin/lookat
	rm -f judge/lookat.doc
	-../../judge/bin/lookat -doc >judge/lookat.doc
judge/lpq.doc:	../../judge/bin/lpq
	rm -f judge/lpq.doc
	-../../judge/bin/lpq -doc >judge/lpq.doc
judge/remakeMakefile.doc:	hpcm_judging.rc \
			../../judge/bin/remakeMakefile
	rm -f judge/remakeMakefile.doc
	-../../judge/bin/remakeMakefile -doc \
	    >judge/remakeMakefile.doc
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
judge/printer_broadcast.doc:	hpcm_judging.rc \
			../../judge/bin/printer_broadcast
	rm -f judge/printer_broadcast.doc
	-../../judge/bin/printer_broadcast -doc \
	    >judge/printer_broadcast.doc
judge/receivemail.doc:	hpcm_judging.rc \
			../../judge/bin/receivemail
	rm -f judge/receivemail.doc
	-../../judge/bin/receivemail -doc \
	    >judge/receivemail.doc
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
contestant/filtered_diff.doc: \
		../../contestant/bin/filtered_diff
	rm -f contestant/filtered_diff.doc
	-../../contestant/bin/filtered_diff -doc \
	    >contestant/filtered_diff.doc
contestant/fprint.doc:	../../contestant/bin/fprint
	rm -f contestant/fprint.doc
	-../../contestant/bin/fprint -doc \
	    >contestant/fprint.doc
contestant/help.doc:	../../contestant/bin/help
	rm -f contestant/help.doc
	-../../contestant/bin/help -doc \
	    >contestant/help.doc
contestant/hpcm_clisp.doc:	\
		../../contestant/bin/hpcm_clisp
	rm -f contestant/hpcm_clisp.doc
	-../../contestant/bin/hpcm_clisp -doc \
	    >contestant/hpcm_clisp.doc
contestant/hpcm_extract.doc:	\
		../../contestant/bin/hpcm_extract
	rm -f contestant/hpcm_extract.doc
	-../../contestant/bin/hpcm_extract -doc \
	    >contestant/hpcm_extract.doc
ETR=hpcm_extract_test_replies
contestant/${ETR}.doc:	\
		../../contestant/bin/${ETR}
	rm -f contestant/${ETR}.doc
	-../../contestant/bin/${ETR} -doc \
	    >contestant/${ETR}.doc
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
contestant/printer_pipe.doc:	\
	    ../../contestant/bin/printer_pipe
	rm -f contestant/printer_pipe.doc
	-../../contestant/bin/printer_pipe -doc \
	    >contestant/printer_pipe.doc
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
