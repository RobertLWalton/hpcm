# Makefile for an Untimed Contest
#
# File:		Makefile
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Mon Jan 22 07:05:13 EST 2007
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2007/01/23 03:13:08 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.74 $
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
# 7.  Contest judging is automatic.  You can use the
#     `lookat' program to invoke `manualreply' to
#     examine submissions, if necessary.
#
# 8.  If the contest allows contestants to email sub-
#     missions from their own accounts, test the contest
#     by sending email to the contest autojudge with
#     `Subject: get help'.  Then try emailing several
#     demos/count solutions and the demos/javaio solu-
#     tion with subjects of the form `submit count.xxx'
#     (change `count1' to `count' in the subject) or
#     `submit javaio.java'.  See the info/help/email_
#     contests file for more details.
#
#     If the contest has contestant accounts, try
#     submitting demos/count solutions from your test
#     account.
#
# 9.  You can also test the contest by executing
#     `make test_email' if the contest is accessible
#     to contestants by email, and `make test_local'
#     if the contest is accessible by local accounts.
#     Then read each email reply and compare its
#     X-HPCM-Test-Subject field with the results in the
#     body of the reply.
#
#     Note that several of the test submissions time
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
#     You can also transmit the contents of the hpcm/
#     examples/test_submissions directory to a directory
#     in the test account and then use `make test_email'
#     in that directory to send test submissions.  If
#     there are no contestant accounts, you can trans-
#     mit the test_submissions directory to any UNIX
#     account, set up the contestant UNIX tools for that
#     account, and use `make test_email'.  See info/
#     help/email_unix_tools and the hpcm/examples/test_
#     submissions/Makefile file.
#
# 10. A web page containing problem descriptions, and
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
#     which have the same contents when they exist.  The
#     .pdf files made by the commands below are ready to
#     print or put on the web.
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
# 12. There is one background process whose status you
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
# 13. If you have made contestant accounts, give contes-
#     tants access to their accounts by creating the
#     file ./contest/secure/passwords_header and running
#     the hpcm_email_passwords program to email the
#     passwords to the contestants.  Or run hpcm_print_
#     passwords to print the passwords to hand out to
#     the contestants.  See `hpcm_email_passwords -doc'
#     and `hpcm_print_passwords -doc'.

# Parameters:
#
# The location of the hpcm directory.
#
HPCM=${HOME}/hpcm
#
# Locations of test data.
#
S=${HPCM}/examples/test_submissions
R=${HPCM}/examples/test_submissions/email_replies
#
# Sendmail program with arguments:
#
SENDMAIL = /usr/sbin/sendmail -oi -t

# Kill all implicit rules:
#
.SUFFIXES:

# Contest Making Rules:

all:
	makecontest

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
# requisites.
#
print_files_do_nothing:

help_print_files problems_print_files \
    demos_print_files:	print_files_do_nothing
	makecontest

help/this_contest: print_files_do_nothing
	makecontest -this_contest

# Test Rules:
#
include ${HPCM}/examples/test_submissions/Makefile.mk

# Cleanup Rules:

dangerous_test_clean:
	rm -rf mail/* flag log replies mbox

dangerous_clean:
	-makescoreboard stop
	rm -rf mail rerun help flag log \
	       secure .ssh web* \
	       info* solutions* \
	       TODO replies mbox *.ps *.pdf \
	       `readlink contest` \
	       contest \
	       Contest_Address \
	       help_print_files* \
	       problems_print_files* \
	       demos_print_files* \
	       solution_files* \
	       Scorefinder* \
	       Scoreboard_PID \
	       scoreboard.web
