# Makefile for an Untimed Contest
#
# File:		Makefile
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Fri Nov  6 05:04:38 EST 2015
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
#   $Revision: 1.10 $
#
# To make an untimed contest named `foo':
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
#    Some of the instructions may cause the creating of
#    contestant accounts.  Generally all contestant
#    accounts in an untimed contest are assigned
#    different random passwords by `makecontest' after
#    you have added account names to the ./contest/
#    secure/passwords file.  If there are contestant
#    accounts for the contest, create a special
#    contestant `test account' that you yourself use for
#    testing the contest, and may put `none' as the
#    password for that account in the passwords file,
#    so you can control the test account password by the
#    UNIX passwd program.
#
# 7. Contest judging is automatic.  You can use the
#    `lookat' program to invoke `manualreply' to
#    examine submissions, if necessary.
#
# 8. Try out your test account.  First do `hpcm_get' to
#    get demos and problems and then copy ~/demos/
#    solutions/reverser/reverser.EXT into ~/demos/
#    reverser, where EXT is the extension of your
#    favorite language, and then run `make submit'
#    in ~/demos/reverser and read your e-mail.
#
#    You can also try
#
#        hpcm_test solutions
#        hpcm_test incorrect
#        hpcm_test correct
#        hpcm_test extraction
#
#    which will submit all the demo solutions,
#    a bunch of incorrect submissions, a bunch of
#    correct submissions, and a bunch of submissions
#    with extraction errors, respectively.  For the
#    the incorrect and extraction submissions, the
#    X-HPCM-Test-Subject field in the email tells
#    what score or error message is expected.
#
#  9. A web page containing problem descriptions, and
#     optionally solutions, is made using the web page
#     parameters in hpcm_judging.rc.
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
# 10. To print handouts the following should be useful:
#
#	 make problems.pdf
#	     Makes printout of problems.
#
#	 make this_contest.pdf
#	     Makes printout of the help/this_contest
#	     file (not usually done)
#
# 11. There is one background process whose status you
#     may want to monitor: the `makescoreboard' process
#     that remakes the scoreboard if necessary every
#     minute.
#
#     This process is started and managed by `make',
#     and in the absence of special problems, rerunning
#     `make' should be all that is necessary.
#
#     You can get status, stop, and start this process
#     by
#
#		makescoreboard status
#		makescoreboard stop
#	        makescoreboard start
#
#     If the makescoreboard process is stopped, the
#     scoreboard can be updated by
#
#		makescoreboard update
#
# 12. If you have made contestant accounts, give contes-
#     tants access to their accounts by creating the
#     file ./contest/secure/passwords_header and running
#     the hpcm_email_passwords program to email the
#     passwords to the contestants.  Or run hpcm_print_
#     passwords to print the passwords to hand out to
#     the contestants.  See `hpcm_email_passwords -doc'
#     and `hpcm_print_passwords -doc'.

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

# Cleanup Rules:

dangerous_submissions_clean:
	rm -rf mail/* flag log

dangerous_complete_clean:
	-makescoreboard stop
	rm -rf mail rerun help flag log \
	       secure .ssh web* \
	       info* solutions* \
	       TODO *.ps *.pdf \
	       `readlink contest` \
	       contest \
	       Contest_Address \
	       help_print_files* \
	       problems_print_files* \
	       demos_print_files* \
	       solution_files* \
	       Scorefinder* \
	       Scoreboard_PID \
	       scoreboard.web \
	       demos_solution_files* \
	       problems_solution_files*
