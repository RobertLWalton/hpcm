# Makefile for a Formal Contest
#
# File:		Makefile
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sat Jan 21 05:17:16 EST 2006
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2006/01/21 10:15:14 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.40 $
#
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
# 6. Follow the instructions in the TODO file that
#    will have been created in the judging directory.
#    After you follow an instruction, edit it out of
#    the TODO file.
#
# 7.  Contest judging is partly automatic.  You must
#     run the `tracker' program to find submission
#     directories in which `manualreply' is run by
#     `tracker' to compute manual scores, reply to
#     queries, and examine system errors.  You need to
#     practice with `manualreply' before the contest.
#     You can do this by testing as below and using the
#     `lookat' instead of `tracker' to invoke `manual-
#     reply'.
#
# 8.  Test the contest by using the test contestant
#     account.  Try submitting the demos/count solution.
#
# 9.  You can also test the contest by executing
#     `make test'.  Then read each email reply and
#     compare its X-HPCM-Test-Subject field with the
#     results in the body of the reply.
#
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
#     You can also transmit the contents of the hpcm/
#     examples/test_submissions directory to a directory
#     in the test contestant account and then use
#     `make test_formal' in that directory to send test
#     submissions.  See the hpcm/examples/test_
#     submissions/Makefile file.
#
#     Note that serveral of the test submissions time
#     out, and for those submissions the autojudge will
#     take over a minute to reply.
#
#     You can run subsets of the tests by using `make
#     test_xxx' for various `xxx;: see hpcm/examples/
#     test_submissions/Makefile.mk.
#
# 10. A web page containing problem descriptions, and
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
#
#     It is also possible to post a copy of the score-
#     board on the web.  See scoreboard_addresses in
#     hpcm_judging.rc.
#
# 11. You may want to print out handouts.  Here are
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
#     A. To make help, problems, or demos printouts
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
#     B. To make help and demos printouts applicable to
#	 all contests
#
#	     cd ~/hpcm/contestant/help
#	     make ps
#	     lpr help.ps demos.ps
#
#     C. To make a help printout that begins with
#        THIS_CONTEST
#
#	      cd ~/hpcm/contestant/help
#	      make EXTRAS=THIS_CONTEST help+.ps
#	      lpr help+.ps
#
#     D. To make a 1 sheet printout of THIS_CONTEST
#
#	       make this_contest.ps
#
# 12. At the end of the contest, the procedure for
#     finishing is typically the following.  A final
#     scoreboard is made automatically in the file
#     ./contest/scoreboard.final from the stop time
#     of the contest until 10 minutes after this stop
#     time.  During these 10 minutes you should finish
#     manual review of all submissions made before the
#     stop time, and if you succeed, the last versoin
#     of scoreboard.final written will have the final
#     scores.  Otherwise this last version automatically
#     written will have asterisks.  In this case,
#     continue manual review until you have finished
#     with all submissions before the stop time (you can
#     ignore those after the stop time), and then delete
#     the scoreboard.final file and wait (up to 1 minute
#     for it to reappear.
#
# 13. There are two background processes whose status
#     you may want to monitor.  The `startcontest'
#     process starts the contest by granting permissions
#     to the ./contest/problems directory and ./contest/
#     help/problems file.  The `makescoreboard' process
#     remakes the scoreboard if necessary every minute.
#
#     These processes are started and managed by `make',
#     and in the absence of special problems, rerunning
#     `make' should be all that is necessary.
#
#     You can get status, stop, and start processe by
#
#		startcontest status
#		startcontest stop
#	        startcontest start
#		makescoreboard status
#		makescoreboard stop
#	        makescoreboard start
#
#     If the makescoreboard process is stopped, the
#     scoreboard can be updated by
#
#		makescoreboard update

# Parameters:
#
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
#
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
	fprint -o `cat demos_print_files` > demos.ps

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
#
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
	       Startcontest_PID \
	       post_contest_reconfiguration
