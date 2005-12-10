# Makefile for an `email' Contest
#
# File:		Makefile
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sat Dec 10 09:24:03 EST 2005
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2005/12/10 15:38:51 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.59 $

# To make an email contest named `foo':
#
# 1. Create a judging account (see `Judging', hpcm/
#    judging/bin/judging) and import the HPCM system
#    into its ~/hpcm directory (see `Installing HPCM',
#    hpcm/judge/bin/installing_hpcm).
#
# 2. Create a judging directory ~/judging_foo_password
#    where the password is needed to keep outsiders from
#    guessing the path to the received submissions,
#    because JAVA requires this path be a+x to access
#    .class files inside the sandbox.
#
#    This password is never really used, and so can be
#    something awfully unguessable.
#
# 3. Put copies of the files in this directory into
#    your judging directory, and edit the names in
#    hpcm_judging.rc appropriately.
#
# 4. Run `make' in your judging directory.
#
#    You should rerun `make' whenever you may have
#    changed something it uses to build the contest
#    files.  The worst that can happen from running
#    `make' unnecessarily is to get redundant instruc-
#    tions in the ./TODO file (see below).
#
# 5. Follow the instructions in the TODO file that
#    will have been created in the judging directory.
#    After you follow an instruction, edit it out of
#    the TODO file.
#
#    If an instruction says `rerun makecontest', do this
#    by rerunning `make'.
#
#    Among other things you will be asked to edit email
#    routing information into ~/.procmailrc.

# 6. Review the `makescoreboard' program (see `make-
#    scoreboard -doc').  Use `make start_scoreboard' to
#    start the scoreboard job, `make check_scoreboard'
#    to check job status, `make stop_scoreboard' to stop
#    the job, and make `update_scoreboard' to provide an
#    instant update of the scoreboard.
#
#    The scoreboard.notes file is appended to the score-
#    board.  Edit this file as you wish.
#
# 7. Contest judging is automatic.  If you set the
#    `reply_manager' and `log_manager' variables in
#    hpcm_judging.rc to your email address, you will be
#    notified of any errors and of any questions submit-
#    ted by contestants.  You many use the `tracker'
#    program to find unchecked errors.
#
#    Note that you do NOT need to execute `startcontest'
#    to start an email contest.  This is only needed for
#    formal contests.
#
# 8. Test the contest by putting the contest address in
#    the Contest_Address file in the judging directory
#    and executing `make test'.  Then read each email
#    reply and compare its X-HPCM-Test-Subject field
#    with the results in the body of the reply.
#
#    You can read and analyze the email replies auto-
#    matically by placing them in an `mbox' file in the
#    judging directory and in that directory executing:
#
#		make extract_replies
#		make diff_replies

#    It is also possible to transmit the contents of
#    the hpcm/examples/test_submissions directory to
#    a remote location along with the hpcm_sendmail
#    program and use `make test_email' in that directory
#    to send test submissions.  See the hpcm/examples/
#    test_submissions/Makefile file.
#
#    Note that one of the test submissions times out,
#    and for that submission the autojudge will take
#    over a minute to reply.
#
#    You can run subsets of the tests by using `make
#    test_xxx' for various `xxx;: see hpcm/examples/
#    test_submissions/Makefile.mk.
#
# 9.  Usually email contests do not have printouts, but
#     just have web pages (see below).   If you do want
#     to print out handouts.  Here are some ways to
#     print everything but the problems, where
#     THIS_CONTEST denotes either
#
#	 ~/judging_foo_password/help/this_contest
#     or
#	 ~/judging_foo_password/info/help/this_contest
#
#     which are the same file.
#
#     A. Execute:
#
#	     cd ~/hpcm/contestant/
#	     make ps
#	     lpr help.ps demos.ps
#	     fprint THIS_CONTEST
#
#	 This makes a generic help printout good for all
#        contests, a generic demos printout, and a small
#        contest specific this_contest printout.

#     B.  Execute:
#
#	      cd ~/hpcm/contestant/
#	      make EXTRAS=THIS_CONTEST help+.ps demos.ps
#	      lpr help+.ps demos.ps
#
#	 This makes a contest specific help printout
#	 consisting of the this_contest file followed by
#	 the same generic help files as in help.ps
#	 above, and also makes a generic demos printout.
#
#      C. Execute
#
#	       cd ~/judging_foo_password
#	       make print_help
#
#         remove the `demos' and `problems' pages, and
#	  reorder the other pages so `index' is first
#	  and the other help file pages are in order
#	  they are listed in the index.  This produces
#	  a `help handout' specific to the contest.
#	  Unlike the help handouts above, this handout
#	  does not contain help for contest types that
#	  differ from the current contest (e.g., no
#	  formal contest help for an email contest),
#
#         Then execute
#
#	       cd ~/judging_foo_password
#	       make print_demos
#
#         and make a `demos handout' that consists of
#	  these pages, in the order given in the `demos'
#	  help page, preceded by that `demos' help page
#	  (which was printed by `make print_help').

#     These handouts can be given out when the contest
#     is set up.  Usually no printout of the problems is
#     given out for an email contest, as contestants get
#     problem statements one problem at a time.
#
#     If you do want to make a printout of the problems,
#     execute
#
#	       cd ~/judging_foo_password
#	       make print_problems
#
#     and make a `problems handout' that consists of
#     these pages, in the order given in the `problems'
#     help page, preceded by that `problems' help page.
#     Discard any demonstration .txt files printed.
#
# 10. A web page containing non-contest-specific help
#     and demos files can be made according to instruc-
#     tions in hpcm/contestant/help/Makefile.
#
# 11. A web page containing problem descriptions, and
#     optionally solutions, can be made by defining the
#     web page parameters in hpcm_judging.rc and execut-
#     ing `make web'.
#
#     A directory named ./web containing this web page
#     and associated files is created, and a tar file
#     ./web.tar of this directory is also created.
#     The contents of the web directory must be copied
#     to a location where it may be put on the WWW,
#     and the tar file makes this easy.

# The location of the hpcm directory.
#
HPCM=${HOME}/hpcm

# Parameters for running tests
#
SENDMAIL = /usr/sbin/sendmail -oi -t
S=${HPCM}/examples/test_submissions
R=${HPCM}/examples/test_submissions/email_replies

# Kill all implicit rules
#
.SUFFIXES:
.SUFFIXES:	.mail .send

all:
	makecontest
	chmod a+x . ..


# Web_do merely causes `make web' to remake the
# web directory even if it exists.
#
web_do:

web:	web_do
	rm -rf web web.tar
	makeweb
	cd web; tar cf ../web.tar *


test:	test_all

scoreboard:
	scorefinder | scoreboard

start_scoreboard:
	makescoreboard start

check_scoreboard:
	makescoreboard status

stop_scoreboard:
	makescoreboard stop

update_scoreboard:
	makescoreboard update

print_help:
	fprint info/help/*

print_demos:
	fprint info/demos/*/*

print_problems:
	fprint info/help/problems
	@for x in info/problems/*/*; \
	    do case "$$x" in \
	    *.txt) echo fprint $$x; fprint $$x;; \
	    *.ps)  echo lpr $$x; lpr $$x;; \
	    *.html | *.htm) \
	           echo YOU MUST use a browser \
			to print $$x by hand;; \
	    esac; done


# Tests

Contest_Address:
	@echo "ERROR: put the contest address in the" \
	      "file Contest_Address"
	@echo "The address should be" \
	      '`'password'<'`id -un`@`hostname`">'"
	@echo "where \`password' is as in ~/.procmailrc"
	exit 1

.mail.send:
	( echo To: `cat Contest_Address`; \
	  cat $*.mail) | ${SENDMAIL}
	sleep 2

test_all:	test_email

TEST_PREREQUISITES=Contest_Address ./mail
include ${HPCM}/examples/test_submissions/Makefile.mk


dangerous_test_clean:
	rm -rf mail/* flag log replies mbox

dangerous_clean:
	rm -rf mail help info* solutions* TODO \
	       Contest_Address flag log \
	       scoreboard.mail Scorefinder_Output \
	       Scoeboard_PID replies mbox web web.tar
