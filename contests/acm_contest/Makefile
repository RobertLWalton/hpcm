# Makefile for a Formal Contest
#
# File:		Makefile
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sun Oct 29 09:33:02 EST 2006
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2006/10/29 14:30:38 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.48 $
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
#     account.  Try submitting several demos/count
#     solutions.
#
# 9.  You can also test the contest by executing
#     `make test'.  Then read each email reply and
#     compare its X-HPCM-Test-Subject field with the
#     results in the body of the reply.
#
#     Note that serveral of the test submissions time
#     out, and for those submissions the autojudge will
#     take over a minute to reply.
#
#     You can run subsets of the tests by using `make
#     test_xxx' for various `xxx;: see hpcm/examples/
#     test_submissions/Makefile.mk.
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
#     denotes any of
#
#	 ~/judging_foo_password/help/this_contest
#     or
#	 ~/contest_foo/help/this_contest
#     or
#	 ~/judging_foo_password/info/help/this_contest
#
#     which have the same contents (at most one of the
#     latter two files exist).  The .pdf files made
#     by the commands below are ready to print or put on
#     the web.
#
#     You can also make .ps files instead of .pdf files
#     simply by changing .pdf to .ps in all the commands
#     below.
#
#     A. To make help, problems, or demos printouts
#	 specific to this contest
#
#	       cd ~/judging_foo_password
#	       make help+.pdf
#	       make problems.pdf
#	       make demos.pdf
#
#        help+.pdf will have THIS_CONTEST at its begin-
#	 ning.
#
#     B. To make help and demos printouts applicable to
#	 all contests
#
#	     cd ~/hpcm/contestant/help
#	     make help.pdf demos.pdf
#
#	 These can then be used with either
#
#	       cd ~/judging_foo_password
#	       make problems.pdf
#	       make this_contest.pdf
#
#	 which makes separate printouts for problems
#	 and for THIS_CONTEST, or with
#
#	       cd ~/judging_foo_password
#	       make problems+.pdf
#
#	 which makes a single printout containing
#	 THIS_CONTEST followed by the problems.
#
#     C. To make a printout of all help files that
#	 begins with THIS_CONTEST
#
#	      cd ~/hpcm/contestant/help
#	      make EXTRAS=THIS_CONTEST help+.pdf
#
# 12. At the end of the contest, the procedure for
#     finishing is typically the following.  A final
#     scoreboard is made automatically in the file
#     ./contest/scoreboard.final from the stop time
#     of the contest until 10 minutes after this stop
#     time.  During these 10 minutes you should finish
#     manual review of all submissions made before the
#     stop time, and if you succeed, the last version
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
#     You can get status, stop, and start these proces-
#     ses by
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
#
# Locations of test data.
#
S=${HPCM}/examples/test_submissions
R=${HPCM}/examples/test_submissions/formal_replies

# Kill all implicit rules:
#
.SUFFIXES:

# Contest Making Rules:

all:
	makecontest

# Web Page Rules:

# Web_do_nothing merely causes `make web' to remake the
# web directory even if it exists.
#
web_do_nothing:

web:	web_do_nothing
	makecontest -web

# Printout Rules:

.SUFFIXES: .ps .pdf
.ps.pdf:
	ps2pdf $*.ps $*.pdf

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

problems+.ps:	help/this_contest problems_print_files
	rm -f problems+.ps
	fprint -o help/this_contest \
	       `cat problems_print_files` \
	       > problems+.ps

this_contest.ps: help/this_contest
	rm -f this_contest.ps
	fprint -o help/this_contest >this_contest.ps


# Print_files_do_nothing merely causes the print files
# to be remade whenever required regardless of pre-
# requisits.
#
print_files_do_nothing:

help_print_files problems_print_files \
    demos_print_files:	print_files_do_nothing
	makecontest

help/this_contest: print_files_do_nothing
	makecontest -this_contest

# Test Rules:

test:	test_formal

.SUFFIXES: .mail .send

.mail.send:
	hpcm_sendmail <$*.mail
	sleep 2

TEST_PREREQUISITES= ./mail
include ${HPCM}/examples/test_submissions/Makefile.mk

# Cleanup Rules:

dangerous_test_clean:
	rm -rf mail/* flag log replies mbox

dangerous_clean:
	-makescoreboard stop
	-startcontest stop
	rm -rf mail help flag log web web.tar \
	       solutions* TODO replies mbox *.ps *.pdf \
	       `readlink contest` \
	       contest \
	       Contest_Address \
	       help_print_files* \
	       problems_print_files* \
	       demos_print_files* \
	       solution_files* \
	       Scorefinder* \
	       Scoreboard_PID \
	       Startcontest_PID \
	       post_contest_reconfiguration
