# HPCM Judge's Doc Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sat Dec 30 04:22:48 EST 2006
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2006/12/30 09:23:11 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.31 $

# make
#	Makes documentation as fprintable .txt files in
#	this subdirectory tree.
#
# make fprint
#	Makes and fprints documentation.  Overview doc-
#	uments are first, and the rest is program docu-
#	mentation files in alphabetical order.
#
# make fcheck
#	Makes and fchecks documentation.

# List DOCS in order of printing.
#
DOCS=	overview.txt \
	installing_hpcm.txt \
	judging.txt \
        judge/autodispatch.txt \
	judge/autoinfo.txt \
	judge/autojudge.txt \
	judge/chkpage.txt \
	judge/diff_contest.txt \
	judge/dispatchmail.txt \
	contestant/email_diff.txt \
	judge/extractmail.txt \
	judge/fcheck.txt \
	judge/fcheckc.txt \
	judge/file_list.txt \
	contestant/filtered_diff.txt \
	judge/fprint.txt \
	contestant/fprint.txt \
	contestant/help.txt \
	judge/hpcm_clisp.txt \
	contestant/hpcm_clisp.txt \
	secure/hpcm_email_passwords.txt \
	contestant/hpcm_extract.txt \
	contestant/hpcm_extract_test_replies.txt \
	contestant/hpcm_get.txt \
	judge/hpcm_help.txt \
	contestant/hpcm_helper.txt \
	secure/hpcm_make_accounts.txt \
	secure/hpcm_print_passwords.txt \
	contestant/hpcm_remote_sendmail.txt \
	contestant/hpcm_sandbox.txt \
	judge/hpcm_sandbox.txt \
	contestant/hpcm_sendmail.txt \
	judge/hpcm_sendmail.txt \
	secure/hpcm_set_passwords.txt \
	contestant/hpcm_submit.txt \
	secure/hpcm_TRASH_ACCOUNTS.txt \
	secure/hpcm_wrapper.txt \
	judge/javahelp.txt \
	contestant/javahelp.txt \
	judge/jfilter.txt \
	judge/jprint.txt \
	judge/lookat.txt \
	judge/printer.txt \
	contestant/printer.txt \
	judge/makecontest.txt \
	judge/makescoreboard.txt \
	judge/makeweb.txt \
	judge/manualreply.txt \
	contestant/print.txt \
	contestant/print2.txt \
	contestant/printer_pipe.txt \
	judge/printer_broadcast.txt \
	judge/receivemail.txt \
	judge/remakeMakefile.txt \
	judge/rerun_contest.txt \
	contestant/scoreboard.txt \
	judge/scoreboard.txt \
	judge/scorediff.txt \
	judge/scorefinder.txt \
	judge/scoretable.txt \
	contestant/SETUP_PROTOTYPE.txt \
	judge/sloc_counts.txt \
	judge/startcontest.txt \
	judge/stlhelp.txt \
	contestant/stlhelp.txt \
	judge/testproblems.txt \
	judge/tracker.txt \
	judge/trackfinder.txt \
	contestant/TRASH_ACCOUNT.txt \
	contestant/xcsh.txt

# List sources in order of printing.
#
SOURCES = \
	../lib/hpcm_judging.rc \
	../lib/judging_common.tcl \
	../lib/display_common.tcl \
	../lib/scoreboard_common.tcl \
	../lib/scoring_common.tcl

all:	bin judge contestant secure ${DOCS}

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
	cd ../../secure/src; make
	cd ../../contestant/bin; make
	cd ../bin; make

judge:
	mkdir judge

contestant:
	mkdir contestant

secure:
	mkdir secure

clean:
	rm -rf overview.txt installing_hpcm.txt \
	       judging.txt \
	       judge contestant secure \
	       hpcm_judging.rc

# ../bin docs
#
judge/autodispatch.txt:	hpcm_judging.rc \
			../../judge/bin/autodispatch
	rm -f judge/autodispatch.txt
	-../../judge/bin/autodispatch -doc \
	    >judge/autodispatch.txt
judge/autoinfo.txt:	hpcm_judging.rc \
		        ../../judge/bin/autoinfo
	rm -f judge/autoinfo.txt
	-../../judge/bin/autoinfo -doc \
	    >judge/autoinfo.txt
judge/autojudge.txt:	hpcm_judging.rc \
		        ../../judge/bin/autojudge
	rm -f judge/autojudge.txt
	-../../judge/bin/autojudge -doc \
	    >judge/autojudge.txt
judge/chkpage.txt:	hpcm_judging.rc \
			../../judge/bin/chkpage
	rm -f judge/chkpage.txt
	-../../judge/bin/chkpage -doc \
	    >judge/chkpage.txt
judge/diff_contest.txt:	hpcm_judging.rc \
			../../judge/bin/diff_contest
	rm -f judge/diff_contest.txt
	-../../judge/bin/diff_contest -doc \
	    >judge/diff_contest.txt
judge/dispatchmail.txt:	hpcm_judging.rc \
			../../judge/bin/dispatchmail
	rm -f judge/dispatchmail.txt
	-../../judge/bin/dispatchmail -doc \
	    >judge/dispatchmail.txt
judge/extractmail.txt:	hpcm_judging.rc \
			../../judge/bin/extractmail
	rm -f judge/extractmail.txt
	-../../judge/bin/extractmail -doc \
	    >judge/extractmail.txt
judge/fcheck.txt:	hpcm_judging.rc \
			../../judge/bin/fcheck
	rm -f judge/fcheck.txt
	-../../judge/bin/fcheck -doc \
	    >judge/fcheck.txt
judge/fcheckc.txt:	hpcm_judging.rc \
			../../judge/bin/fcheckc
	rm -f judge/fcheckc.txt
	-../../judge/bin/fcheckc -doc \
	    >judge/fcheckc.txt
judge/file_list.txt:	hpcm_judging.rc \
			../../judge/bin/file_list
	rm -f judge/file_list.txt
	-../../judge/bin/file_list -doc \
	    >judge/file_list.txt
judge/fprint.txt:	hpcm_judging.rc \
			../../judge/bin/fprint
	rm -f judge/fprint.txt
	-../../judge/bin/fprint -doc \
	    >judge/fprint.txt
judge/hpcm_clisp.txt:	hpcm_judging.rc \
			../../judge/bin/hpcm_clisp
	rm -f judge/hpcm_clisp.txt
	-../../judge/bin/hpcm_clisp -doc \
	    >judge/hpcm_clisp.txt
judge/hpcm_help.txt:	hpcm_judging.rc \
		../../judge/bin/hpcm_help
	rm -f judge/hpcm_help.txt
	-../../judge/bin/hpcm_help -doc \
	    >judge/hpcm_help.txt
judge/hpcm_sandbox.txt:	hpcm_judging.rc \
			../../judge/bin/hpcm_sandbox
	rm -f judge/hpcm_sandbox.txt
	-../../judge/bin/hpcm_sandbox -doc \
	    >judge/hpcm_sandbox.txt
judge/hpcm_sendmail.txt:	hpcm_judging.rc \
			../../judge/bin/hpcm_sendmail
	rm -f judge/hpcm_sendmail.txt
	-../../judge/bin/hpcm_sendmail -doc \
	    >judge/hpcm_sendmail.txt
installing_hpcm.txt:	hpcm_judging.rc \
			../../judge/bin/installing_hpcm
	rm -f installing_hpcm.txt
	-../../judge/bin/installing_hpcm -doc \
	    >installing_hpcm.txt
judging.txt:	hpcm_judging.rc \
			../../judge/bin/judging
	rm -f judging.txt
	-../../judge/bin/judging -doc >judging.txt
judge/javahelp.txt:	hpcm_judging.rc \
		        ../../judge/bin/javahelp
	rm -f judge/javahelp.txt
	-../../judge/bin/javahelp -doc \
	    >judge/javahelp.txt
judge/jfilter.txt:	hpcm_judging.rc \
		        ../../judge/bin/jfilter
	rm -f judge/jfilter.txt
	-../../judge/bin/jfilter -doc >judge/jfilter.txt
judge/jprint.txt:	hpcm_judging.rc \
		        ../../judge/bin/jprint
	rm -f judge/jprint.txt
	-../../judge/bin/jprint -doc >judge/jprint.txt
judge/lookat.txt:	hpcm_judging.rc \
		        ../../judge/bin/lookat
	rm -f judge/lookat.txt
	-../../judge/bin/lookat -doc >judge/lookat.txt
judge/printer.txt:	../../judge/bin/printer
	rm -f judge/printer.txt
	-../../judge/bin/printer -doc >judge/printer.txt
judge/remakeMakefile.txt:	hpcm_judging.rc \
			../../judge/bin/remakeMakefile
	rm -f judge/remakeMakefile.txt
	-../../judge/bin/remakeMakefile -doc \
	    >judge/remakeMakefile.txt
judge/rerun_contest.txt:	hpcm_judging.rc \
		        ../../judge/bin/rerun_contest
	rm -f judge/rerun_contest.txt
	-../../judge/bin/rerun_contest -doc \
	    >judge/rerun_contest.txt
judge/makecontest.txt:	hpcm_judging.rc \
			../../judge/bin/makecontest
	rm -f judge/makecontest.txt
	-../../judge/bin/makecontest -doc \
	    >judge/makecontest.txt
judge/makescoreboard.txt:	hpcm_judging.rc \
			../../judge/bin/makescoreboard
	rm -f judge/makescoreboard.txt
	-../../judge/bin/makescoreboard -doc \
	    >judge/makescoreboard.txt
judge/makeweb.txt:	hpcm_judging.rc \
			../../judge/bin/makeweb
	rm -f judge/makeweb.txt
	-../../judge/bin/makeweb -doc \
	    >judge/makeweb.txt
judge/manualreply.txt:	hpcm_judging.rc \
			../../judge/bin/manualreply
	rm -f judge/manualreply.txt
	-../../judge/bin/manualreply -doc \
	    >judge/manualreply.txt
overview.txt:	hpcm_judging.rc \
			../../judge/bin/overview
	rm -f overview.txt
	-../../judge/bin/overview -doc >overview.txt
judge/printer_broadcast.txt:	hpcm_judging.rc \
		    ../../judge/bin/printer_broadcast
	rm -f judge/printer_broadcast.txt
	-../../judge/bin/printer_broadcast -doc \
	    >judge/printer_broadcast.txt
judge/receivemail.txt:	hpcm_judging.rc \
			../../judge/bin/receivemail
	rm -f judge/receivemail.txt
	-../../judge/bin/receivemail -doc \
	    >judge/receivemail.txt
judge/scoreboard.txt:	hpcm_judging.rc \
		        ../../judge/bin/scoreboard
	rm -f judge/scoreboard.txt
	-../../judge/bin/scoreboard -doc \
	    >judge/scoreboard.txt
judge/scorediff.txt:	hpcm_judging.rc \
		        ../../judge/bin/scorediff
	rm -f judge/scorediff.txt
	-../../judge/bin/scorediff -doc \
	    >judge/scorediff.txt
judge/scorefinder.txt:	hpcm_judging.rc \
			../../judge/bin/scorefinder
	rm -f judge/scorefinder.txt
	-../../judge/bin/scorefinder -doc \
	    >judge/scorefinder.txt
judge/scoretable.txt:	hpcm_judging.rc \
			../../judge/bin/scoretable
	rm -f judge/scoretable.txt
	-../../judge/bin/scoretable -doc \
	    >judge/scoretable.txt
judge/sloc_counts.txt:	hpcm_judging.rc \
			../../judge/bin/sloc_counts
	rm -f judge/sloc_counts.txt
	-../../judge/bin/sloc_counts -doc \
	    >judge/sloc_counts.txt
judge/startcontest.txt:	hpcm_judging.rc \
			../../judge/bin/startcontest
	rm -f judge/startcontest.txt
	-../../judge/bin/startcontest -doc \
	    >judge/startcontest.txt
judge/stlhelp.txt:	hpcm_judging.rc \
		        ../../judge/bin/stlhelp
	rm -f judge/stlhelp.txt
	-../../judge/bin/stlhelp -doc \
	    >judge/stlhelp.txt
judge/testproblems.txt:	hpcm_judging.rc \
			../../judge/bin/testproblems
	rm -f judge/testproblems.txt
	-../../judge/bin/testproblems -doc \
	    >judge/testproblems.txt
judge/tracker.txt:	hpcm_judging.rc \
		        ../../judge/bin/tracker
	rm -f judge/tracker.txt
	-../../judge/bin/tracker -doc \
	    >judge/tracker.txt
judge/trackfinder.txt:	hpcm_judging.rc \
			../../judge/bin/trackfinder
	rm -f judge/trackfinder.txt
	-../../judge/bin/trackfinder -doc \
	    >judge/trackfinder.txt

# ../../secure/bin docs
#
secure/hpcm_email_passwords.txt:    hpcm_judging.rc \
		../../secure/bin/hpcm_email_passwords
	rm -f secure/hpcm_email_passwords.txt
	-../../secure/bin/hpcm_email_passwords -doc \
	    >secure/hpcm_email_passwords.txt
secure/hpcm_make_accounts.txt:	\
		../../secure/bin/hpcm_make_accounts
	rm -f secure/hpcm_make_accounts.txt
	-../../secure/bin/hpcm_make_accounts -doc \
	    >secure/hpcm_make_accounts.txt
secure/hpcm_print_passwords.txt:    hpcm_judging.rc \
		../../secure/bin/hpcm_print_passwords
	rm -f secure/hpcm_print_passwords.txt
	-../../secure/bin/hpcm_print_passwords -doc \
	    >secure/hpcm_print_passwords.txt
secure/hpcm_set_passwords.txt:	\
		../../secure/bin/hpcm_set_passwords
	rm -f secure/hpcm_set_passwords.txt
	-../../secure/bin/hpcm_set_passwords -doc \
	    >secure/hpcm_set_passwords.txt
secure/hpcm_TRASH_ACCOUNTS.txt:	\
		../../secure/bin/hpcm_TRASH_ACCOUNTS
	rm -f secure/hpcm_TRASH_ACCOUNTS.txt
	-../../secure/bin/hpcm_TRASH_ACCOUNTS -doc \
	    >secure/hpcm_TRASH_ACCOUNTS.txt

# ../../secure/src docs
#
secure/hpcm_wrapper.txt:    hpcm_judging.rc \
		../../secure/src/hpcm_wrapper
	rm -f secure/hpcm_wrapper.txt
	-../../secure/src/hpcm_wrapper -doc \
	    >secure/hpcm_wrapper.txt

# ../../contestant/bin docs
#
contestant/email_diff.txt: \
		../../contestant/bin/email_diff
	rm -f contestant/email_diff.txt
	-../../contestant/bin/email_diff -doc \
	    >contestant/email_diff.txt
contestant/filtered_diff.txt: \
		../../contestant/bin/filtered_diff
	rm -f contestant/filtered_diff.txt
	-../../contestant/bin/filtered_diff -doc \
	    >contestant/filtered_diff.txt
contestant/fprint.txt:	../../contestant/bin/fprint
	rm -f contestant/fprint.txt
	-../../contestant/bin/fprint -doc \
	    >contestant/fprint.txt
contestant/help.txt:	../../contestant/bin/help
	rm -f contestant/help.txt
	-../../contestant/bin/help -doc \
	    >contestant/help.txt
contestant/hpcm_clisp.txt:	\
		../../contestant/bin/hpcm_clisp
	rm -f contestant/hpcm_clisp.txt
	-../../contestant/bin/hpcm_clisp -doc \
	    >contestant/hpcm_clisp.txt
contestant/hpcm_extract.txt:	\
		../../contestant/bin/hpcm_extract
	rm -f contestant/hpcm_extract.txt
	-../../contestant/bin/hpcm_extract -doc \
	    >contestant/hpcm_extract.txt
ETR=hpcm_extract_test_replies
contestant/${ETR}.txt:	\
		../../contestant/bin/${ETR}
	rm -f contestant/${ETR}.txt
	-../../contestant/bin/${ETR} -doc \
	    >contestant/${ETR}.txt
contestant/hpcm_get.txt:	\
		        ../../contestant/bin/hpcm_get
	rm -f contestant/hpcm_get.txt
	-../../contestant/bin/hpcm_get -doc \
	    >contestant/hpcm_get.txt
contestant/hpcm_helper.txt:	\
		        ../../contestant/bin/hpcm_helper
	rm -f contestant/hpcm_helper.txt
	-../../contestant/bin/hpcm_helper -doc \
	    >contestant/hpcm_helper.txt
contestant/hpcm_remote_sendmail.txt:	\
	    ../../contestant/bin/hpcm_remote_sendmail
	rm -f contestant/hpcm_remote_sendmail.txt
	-../../contestant/bin/hpcm_remote_sendmail -doc \
	    >contestant/hpcm_remote_sendmail.txt
contestant/hpcm_sandbox.txt:	\
		../../contestant/bin/hpcm_sandbox
	rm -f contestant/hpcm_sandbox.txt
	-../../contestant/bin/hpcm_sandbox -doc \
	    >contestant/hpcm_sandbox.txt
contestant/hpcm_sendmail.txt:	\
		../../contestant/bin/hpcm_sendmail
	rm -f contestant/hpcm_sendmail.txt
	-../../contestant/bin/hpcm_sendmail -doc \
	    >contestant/hpcm_sendmail.txt
contestant/hpcm_submit.txt:	\
		../../contestant/bin/hpcm_submit
	rm -f contestant/hpcm_submit.txt
	-../../contestant/bin/hpcm_submit -doc \
	    >contestant/hpcm_submit.txt
contestant/javahelp.txt:	\
		../../contestant/bin/javahelp
	rm -f contestant/javahelp.txt
	-../../contestant/bin/javahelp -doc \
	    >contestant/javahelp.txt
contestant/printer.txt:	../../contestant/bin/printer
	rm -f contestant/printer.txt
	-../../contestant/bin/printer -doc \
	    >contestant/printer.txt
contestant/print.txt:	../../contestant/bin/print
	rm -f contestant/print.txt
	-../../contestant/bin/print -doc \
	    >contestant/print.txt
contestant/print2.txt:	../../contestant/bin/print2
	rm -f contestant/print2.txt
	-../../contestant/bin/print2 -doc \
	    >contestant/print2.txt
contestant/printer_pipe.txt:	\
	    ../../contestant/bin/printer_pipe
	rm -f contestant/printer_pipe.txt
	-../../contestant/bin/printer_pipe -doc \
	    >contestant/printer_pipe.txt
contestant/scoreboard.txt:	\
		../../contestant/bin/scoreboard
	rm -f contestant/scoreboard.txt
	-../../contestant/bin/scoreboard -doc \
	    >contestant/scoreboard.txt
contestant/SETUP_PROTOTYPE.txt:	\
		../../contestant/bin/SETUP_PROTOTYPE
	rm -f contestant/SETUP_PROTOTYPE.txt
	-../../contestant/bin/SETUP_PROTOTYPE -doc \
	    >contestant/SETUP_PROTOTYPE.txt
contestant/stlhelp.txt:	\
		../../contestant/bin/stlhelp
	rm -f contestant/stlhelp.txt
	-../../contestant/bin/stlhelp -doc \
	    >contestant/stlhelp.txt
contestant/TRASH_ACCOUNT.txt:	\
		../../contestant/bin/TRASH_ACCOUNT
	rm -f contestant/TRASH_ACCOUNT.txt
	-../../contestant/bin/TRASH_ACCOUNT -doc \
	    >contestant/TRASH_ACCOUNT.txt
contestant/xcsh.txt:	../../contestant/bin/xcsh
	rm -f contestant/xcsh.txt
	-../../contestant/bin/xcsh -doc \
	    >contestant/xcsh.txt
