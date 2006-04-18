# Makefile for an Untimes Contest
#
# File:		Makefile
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Mon Apr 17 19:31:57 EDT 2006
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2006/04/18 00:34:40 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.62 $
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
# 7.  Contest judging is automatic.  You can use the
#     `lookat' program to invoke `manualreply' to
#     examine submissions, if necessary.
#
# 8.  Test the contest by sending the contest mail
#     with the Subject `get help'.  Then try emailing
#     several demos/count and the demos/javaio
#     solutions with Subjects of the form `submit
#     count.xxx' (change `count1' to `count' in the
#     subject) or `submit javaio.java'.  See the
#     info/help/email_contests file for more details.
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
#     You can also transmit the contents of the hpcm/
#     examples/test_submissions directory to a directory
#     in the test contestant account and then use
#     `make test_email' in that directory to send
#     test submissions, provided you have set up the
#     contestant unix tools for the test contestant
#     account.  See info/help/email_unix_tools and the
#     hpcm/examples/test_submissions/Makefile file.
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

# Web Page Rules:

# Web_do_nothing merely causes `make web' to remake the
# web directory even if it exists.
#
web_do_nothing:

web:	web_do_nothing
	rm -rf web web.tar
	makeweb
	cd web; tar cf ../web.tar *

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
	makecontest -print_files

help/this_contest: print_files_do_nothing
	makecontest -this_contest

# Test Rules:

test:	test_email

.SUFFIXES: .mail .send

.mail.send:
	( echo To: `cat Contest_Address`; cat $*.mail) \
	    | ${SENDMAIL}
	sleep 2

TEST_PREREQUISITES=Contest_Address ./mail
include ${HPCM}/examples/test_submissions/Makefile.mk

# Cleanup Rules:

dangerous_test_clean:
	rm -rf mail/* flag log replies mbox

dangerous_clean:
	rm -rf mail help flag log web web.tar \
	       info* solutions* \
	       TODO replies mbox *.ps *.pdf \
	       Contest_Address \
	       help_print_files* \
	       problems_print_files* \
	       demos_print_files* \
	       Scorefinder* \
	       Scoreboard_PID \
	       scoreboard.mail
