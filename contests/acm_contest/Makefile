# Makefile for an ACM Contest
#
# File:		Makefile
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Mon May 20 03:01:32 EDT 2013
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2013/05/20 07:03:46 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.59 $
#
# To make an ACM contest named `foo':
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
# 3. Create a judging directory ~/judging_foo_PASSWORD
#    where PASSWORD is a random string of characters and
#    is needed to keep outsiders from guessing the path
#    to the received submissions, because JAVA requires
#    this path be a+x to access .class files inside the
#    sandbox.
#
#    This PASSWORD is never really used, and so can be
#    something awfully unguessable.
#
# 4. Put copies of the files in the directory containing
#    this file into your judging directory, and edit the
#    information in hpcm_judging.rc to define your
#    contest.
#
# 5. Run `make' in your judging directory.
#
# 6. Follow the instructions in the TODO file that
#    will have been created in the judging directory.
#    After you follow an instruction, edit it out of
#    the TODO file.
#
#    Some of the instructions cause the creating of
#    contestant accounts.  Generally all contestant
#    accounts in an ACM contest are assigned the same
#    password, which you assign when you edit the
#    ./contest/secure/passwords file to contain the
#    account names and passwords.  You should create
#    a special contestant `test account' that you
#    yourself use for testing the contest.
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
# 8. Try out your test account.  First do `hpcm_get' to
#    get demos and problems and then copy ~/demos/
#    solutions/reverser/reverser.EXT into ~/demos/
#    reverser, where EXT is the extension of your
#    favorite language, and then run `make submit'
#    in ~/demos/reverser and read your e-mail.  You can
#    also try
#
#        hpcm_test ~/demos/solutions
#
#    which will submit all the demo solutions.
# 
# 9. You can also test the contest by executing
#    `make test'.  Then read each email reply and
#    compare its X-HPCM-Test-Subject field with the
#    results in the body of the reply.
#
#    Note that some of the test submissions time
#    out, and for those submissions the autojudge will
#    take several tens of seconds to reply.
#
#    There are more tests defined in the `make_test'
#    file.  These are checked as above by using
#    X-HPCM-Test-Subject if it exists, or by demanding
#    a score of `Completely Correct' otherwise.
#
#    After running `make test' you may want to clean
#    out all submissions made so far, as one of the
#    tests makes a rather large output file.  Cleaning
#    all submissions made so far can be done by
#
#    	make dangerous_submissions_clean
#
#    WARNING: Do NOT DO THIS if any contestants have
#    submitted anything, as it will clean out their
#    submissions too.
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
#     board on the web.  See scoreboard_commands in
#     ./hpcm_judging.rc.
#
# 11. To print handouts the following should be useful:
#
#	 make problems.pdf
#	     Makes printout of problems.
#
#	 make this_contest.pdf
#	     Makes printout of the help/this_contest
#	     file (not usually done)
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
#
# 14. The contest is automatically converted from a
#     formal contest to an untimed practice some time
#     (typically, 1 hour) after the contest ends.  After
#     this you may wish to give contestants access to
#     their accounts by
#
#	1. Removing passwords from ./contest/secure/
#          passwords, running `make', and following the
#          TODO instruction to change the passwords
#          of accounts to random character strings.
#
#       2. Creating the file ./contest/secure/passwords_
#	   header and running the hpcm_email_passwords
#	   program to send the passwords to the contes-
#	   tants.  See `hpcm_email_passwords -doc'.

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
	rm -f $*.pdf
	ps2pdf $*.ps $*.pdf

problems.ps:	problems_print_files
	rm -f problems.ps
	fprint -o `cat problems_print_files` \
	       > problems.ps

this_contest.ps: help/this_contest
	rm -f this_contest.ps
	fprint -o help/this_contest >this_contest.ps


# Print_files_do_nothing merely causes the print files
# to be remade whenever required regardless of pre-
# requisites.
#
print_files_do_nothing:

problems_print_files:	print_files_do_nothing
	makecontest

help/this_contest: print_files_do_nothing
	makecontest -this_contest

# Test Rules (see make_test global variable in
# hpcm_judging.rc):
#
-include make_test

# Cleanup Rules:

dangerous_submissions_clean:
	rm -rf mail/* flag log

dangerous_complete_clean:
	-makescoreboard stop
	-startcontest stop
	rm -rf mail rerun help flag log make_test \
	       secure .ssh web* \
	       solutions* TODO *.ps *.pdf \
	       `readlink contest` \
	       contest \
	       Contest_Address \
	       help_print_files* \
	       problems_print_files* \
	       demos_print_files* \
	       solution_files* \
	       Scorefinder* \
	       Startcontest_PID \
	       Scoreboard_PID \
	       scoreboard.web \
	       demos_solution_files* \
	       problems_solution_files*
