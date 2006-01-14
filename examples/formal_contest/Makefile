# Makefile for a Formal Contest
#
# File:		Makefile
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sat Jan 14 11:11:15 EST 2006
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2006/01/14 16:09:39 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.36 $

# To make a formal contest named `foo':
#
# 1. Create a judging account (see `Judging', hpcm/
#    judging/bin/judging) and import the HPCM system
#    into its ~/hpcm directory (see `Installing HPCM',
#    hpcm/judge/bin/installing_hpcm).  One judging
#    account with its ~/hpcm directory can be used by
#    many contests, so you may not need to make a new
#    judging account.
#
# 2. Make a contestant prototype account, which we will
#    refer to here as PPP-proto.  Its name should end
#    with `-proto' or `_proto'.  Follow the instructions
#    in:
#
#	~/hpcm/contestant/bin/SETUP_PROTOTYPE -doc
#
#    One prototype directory can be used for many con-
#    tests, so you may not need to make a new prototype
#    directory for this contest.
#
# 3. Create a judging directory ~/judging_foo_password
#    where this password is needed to keep outsiders
#    from guessing the path to the received submissions,
#    because JAVA requires this path be a+x to access
#    .class files inside the sandbox.
#
#    This password is never really used, and so can be
#    something awfully unguessable.
#
# 4. Put copies of the files in this directory into
#    your judging directory, and edit the information in
#    hpcm_judging.rc to define your contest.
#
# 5. Run `make' in your judging directory.
#
#    You should rerun `make' whenever you may have
#    changed something it uses to build the contest
#    files.  The worst that can happen from running
#    `make' unnecessarily is to get redundant instruc-
#    tions in the ./TODO file (see below).

# 6. Follow the instructions in the TODO file that
#    will have been created in the judging directory.
#    After you follow an instruction, edit it out of
#    the TODO file.
#
# 7.  Contest judging is partly automatic.  You must
#     run the `tracker' program to find submission
#     directories in which `manualreply' is to be run
#     (by `tracker') to compute manual scores, reply to
#     queries, and examine system errors.  You need to
#     practice with `manualreply' before the contest.
#
# 8.  Test the contest by using the test contestant
#     account.  Try submitting the demos/count solution.
#
# 9.  You can also test the contest by executing
#     `make test'.  Then read each email reply and
#     compare its X-HPCM-Test-Subject field with the
#     results in the body of the reply.

#     You can read and analyze the email replies auto-
#     matically by placing them in an `mbox' file in the
#     judging directory and in that directory executing:
#
#		make extract_replies
#		make diff_replies
#
#     For the formal contest, there are no automatic
#     replies for queries and system errors.  Some of
#     the test messages sent by `make test' are queries
#     or intentionally trigger system errors.  These
#     should be verified by running `tracker' in the
#     contest judging directory and replying using `er'
#     and then `sr' under manualreply.  If this is done
#     for ONLY queries and system errors before the
#     `mbox' file mentioned above is made, and if NO
#     actual editing is done during the `er' command,
#     then `make diff_replies' above will also check
#     the system response to queries and system errors.
#
#     It is also possible to transmit the contents of
#     the hpcm/examples/test_submissions directory to
#     a directory in the test contestant account and
#     use `make test_formal' in that directory to send
#     test submissions.  See the hpcm/examples/test_
#     submissions/Makefile file.
#
#     Note that one of the test submissions times out,
#     and for that submission the autojudge will take
#     over a minute to reply.
#
#     You can run subsets of the tests by using `make
#     test_xxx' for various `xxx;: see hpcm/examples/
#     test_submissions/Makefile.mk.

# 10. You may want to print out handouts.  Here are
#     some ways to make printouts, where THIS_CONTEST
#     denotes either
#
#	 ~/judging_foo_password/help/this_contest
#     or
#	 ~/contest_foo/help/this_contest
#
#     which are the same file.  The .ps files are post-
#     script files ready to print or put on the web.
#
#     A. To make help and demos printouts applicable to
#	 all contests
#
#	     cd ~/hpcm/contestant/help
#	     make ps
#	     lpr help.ps demos.ps
#
#     B. To make a help printout that begins with
#        THIS_CONTEST
#
#	      cd ~/hpcm/contestant/help
#	      make EXTRAS=THIS_CONTEST help+.ps
#	      lpr help+.ps
#
#     C. To make help, problems, or demos printouts
#	 specific to this contest
#
#	       cd ~/judging_foo_password
#	       make help+.ps
#	       make problem.ps
#	       make demos.ps
#
#        This help+.ps will have THIS_CONTEST at its
#	 beginning.
#
#     D. To make a 1 sheet printout of THIS_CONTEST
#
#	       make this_contest.ps
#
# 11. A web page containing non-contest-specific help
#     and demos files can be made according to instruc-
#     tions in hpcm/contestant/help/Makefile.
#
# 12. At the end of the contest, the procedure for
#     finishing is typically the following.  A final
#     scoreboard is made automatically; in some cases
#     it appears in scoreboard.final. However, this
#     scoreboard generally has asterisks because the
#     human judge has not finished manually reviewing
#     all the submissions at the end of the contest
#     when this scoreboard is automatically made.  To
#     remove the asterisks just finish the manual
#     review, remove the final scoreboard file, and
#     wait a minute for the scoreboard to be remade.
#     The scoreboard is automatically made if it does
#     not exist or has a modification time preceding
#     the end of contest.  The automatic scoreboard
#     maker polls about once a minute to see if it has
#     to do anything.

# 13. A web page containing problem descriptions, and
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
S=${HPCM}/examples/test_submissions
R=${HPCM}/examples/test_submissions/formal_replies

# Kill all implicit rules
#
.SUFFIXES:
.SUFFIXES:	.mail .send

all:
	makecontest


# Web_do merely causes `make web' to remake the
# web directory even if it exists.
#
web_do:

web:	web_do
	rm -rf web web.tar
	makeweb
	cd web; tar cf ../web.tar *


test:	test_all

trashable:
	makecontest -trashable

nontrashable:
	rm -f ./contest/trashable

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

help+.ps:	help_print_files
	rm -f help+.ps
	fprint -o `cat help_print_files` > help+.ps

demos.ps:	demos_print_files
	rm -f demos.ps

problems.ps:	problems_print_files
	rm -f problems.ps
	fprint -o `cat problems_print_files` \
	       > problems.ps

this_contest.ps: help/this_contest
	rm -f this_contest.ps
	fprint -o help/this_contest >this_contest.ps

help_print_files problems_print_files \
    demos_print_files:	help/this_contest
	makecontest -print_files

help/this_contest:
	makecontest -this_contest

# Tests

.mail.send:
	hpcm_sendmail <$*.mail
	sleep 2

test_all:	test_formal

TEST_PREREQUISITES= ./mail
include ${HPCM}/examples/test_submissions/Makefile.mk

dangerous_test_clean:
	rm -rf mail/* flag log replies mbox

dangerous_clean:
	rm -rf mail help flag log web web.tar \
	       solutions* TODO replies mbox *.ps \
	       `readlink contest` \
	       contest \
	       help_print_files* \
	       problems_print_files* \
	       demos_print_files* \
	       Scorefinder* \
	       Scoreboard_PID \
	       Startcontest_PID
