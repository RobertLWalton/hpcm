# Makefile for a Formal Contest
#
# File:		Makefile
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Tue Sep 24 00:44:02 EDT 2002
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/09/24 04:43:19 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.19 $

# To make a formal contest named `foo':
#
# 1. Create a judging account (see `Judging', hpcm/
#    judging/bin/judging) and import the HPCM system
#    into its ~/hpcm directory (see `Installing HPCM',
#    hpcm/judge/bin/installing_hpcm).
#
# 2. Make a contestant prototype account, which we will
#    refer to here as PPP.  Follow the instructions in:
#
#	~/hpcm/contestant/bin/SETUP_PROTOTYPE -doc
#
#    Below we will make a symbolic link to ~PPP/home,
#    which is called the prototype directory.
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
#    your judging directory, and edit the names in
#    hpcm_judging.rc appropriately.  In this Makefile
#    edit just the INDICES variable below at this time.
#
#    Be sure you edit the scoreboard_stop_time to be
#    some time in the future.

# 5. Make directory ~/contest_foo.  Then make the
#    symbolic links:
#
#      ~/judging_foo_password/contest -> ~/contest_foo
#      ~/contest_foo/bin -> ~/hpcm/contestant/bin
#      ~/contest_foo/home -> ~PPP/home
#
#    by executing:
#
#	cd ~/judging_foo_password
#	ln -s ~/contest_foo contest
#
#	cd ~/contest_foo
#	ln -s ~/hpcm/contestant/bin bin
#	ln -s ~PPP/home home
#
# 6. Move the file scoreboard.1head into the ./contest
#    directory (which is now linked to ~/contest_foo)
#    by executing:
#
#	cd ~/judging_foo_password
#	mv scoreboard.1head contest
#
# 7. Create a file named ~/contest_foo/secure/passwords
#    by executing:
#
#	cd ~/contest_foo
#	mkdir secure
#	chmod 700 secure
#	cd secure
#	vi passwords  (or use another editor)
#	# edit passwords and quit editor
#	chmod 600 passwords

#    The `~/contest_foo/secure' directory contains stuff
#    that the judge MUST keep secret.  The `passwords'
#    file in this directory should be just a list of
#    all the contestant accounts that will be created,
#    one account name per line, with each account name
#    followed by a colon and the password for the
#    account.  An example file would be:
#
#	conttest:tiamfp
#	cont1:tiamfp
#	cont2:tiamfp
#	.....
#	cont20:tiamfp
#
#    Be sure to include a contestant account which
#    you will use to test the system, and to which no
#    actual contestant will be assigned.  Here we
#    included `conttest' for this purpose.
#
#    Here we assume all accounts share the same
#    password that is not going to be given to any
#    contestant.  The judges log in for contestants
#    in a formal contest.  In the above example we
#    have chosen `tiamfp' as the common password.
#
# 8. Run `make' in your judging directory.
#
#    You should rerun `make' whenever you may have
#    changed something it uses to build the contest
#    files.  The worst that can happen from running
#    `make' unnecessarily is to get redundant instruc-
#    tions in the ./TODO file (see below).

# 9. Follow the instructions in the TODO file that
#    will have been created in the judging directory.
#    After you follow an instruction, edit it out of
#    the TODO file.
#
#    If an instruction says `rerun makejudgingdir',
#    do this by rerunning `make'.
#
#    Among other things you will be asked to edit
#    authentication keys into hpcm_judging.rc in the
#    judging directory and email routing information
#    into ~/.procmailrc.
#
#    Another thing you will be asked to do is edit
#    the WHERE fields in the who_where file.  If
#    you do not, printouts will contain the account
#    name.  If you do, be sure the WHERE fields are
#    short enough not to overrun other printout
#    header fields.
#
# 10. Rerun `make' in the judging directory.  This is
#     because you edited `help/this_contest' in the last
#     step and need to install the new version.
#
# 11. Be sure the file ~/contest_foo/secure/passwords
#     has the correct contents.  Each line of this file
#     has the format:
#
#      account-name:password:who:c-email:t-email:comment
#
#     with `:' separated fields.  Fields at the end,
#     optionally including the password and everything
#     after it, can be omitted.  Fields in the middle,
#     including the password, can be omitted by writing
#     consecutive `:'s.  Here `who' is a short team
#     name, c-email is the coach's email address, and
#     t-email is the team's email address if different
#     from c-email.

#     Executing `make', or more specifically:
#
#		cd ~/judging_foo_password
#		makejudgingdir -secure
#
#     will add random passwords to this file for any
#     account that lacks a password.  Giving an account
#     a password of `none' in this file will cause all
#     HPCM programs to avoid dealing with the account
#     password: i.e., HPCM programs that assign or use a
#     password will skip any account with password
#     `none'.
#
#     You should have one of the contestant accounts be
#     a test account that you can use to check whether
#     submission, etc., works.
#
#     You can change ~/contest_foo/secure/passwords at
#     any time.  Whenever you make a change that
#     involves something OTHER than a password, reexe-
#     cute `make', or execute more specifically:
#
#		cd ~/judging_foo_password
#		makejudgingdir -who_where
#
#     to move the updated info into the ~/contest_foo/
#     who_where file.  Whenever you change passwords,
#     you may need to rerun hpcm_set_passwords in the
#     manner described below.
#
# 12. Run `make trashable' in your judging directory.
#     This makes the file ./contest/trashable which
#     lists the contestant accounts that can be reini-
#     tialized by `TRASH_ACCOUNT'.  These accounts have
#     presumably not been made yet (but its OK if they
#     already exist).

# 13. Then as root (you must log into the root account,
#     you cannot use `su', as the paths will not be
#     right), run the following commands, where JJJ is
#     the judge's account:
#
#	cd ~JJJ/hpcm/secure/bin
#	./hpcm_make_accounts ~JJJ/contest_foo
#	./hpcm_set_passwords ~JJJ/contest_foo
#	./hpcm_TRASH_ACCOUNTS ~JJJ/contest_foo
#
#     These will successively make the accounts listed
#     in ~JJJ/contest_foo/secure/passwords, set the
#     passwords of these accounts to the passwords list-
#     ed in that file, and reinitialize these accounts
#     to have exactly the same files (and symbolic
#     links) as the prototype account did the last time
#     SETUP_PROTOTYPE was run in the prototype account
#     (except for the fact that in each contestant
#     account ~/.hpcm_contest is symbolically linked to
#     ~JJJ/contest_foo after the contestant account has
#     been trashed, and certain files and directories
#     in the prototype account are omitted, e.g.,
#     .Xauthority, .history, `home', and `bin').
#
#     This finishes everything you have to do in the
#     root account.
#
# 14. Run `make nontrashable' in your judging directory.
#     This removes the file ./contest/trashable and pre-
#     vents any contestants from accidentally trashing
#     their own account by running TRASH_ACCOUNT (which
#     requires the contestant's account name appear in
#     ~JJJ/contest_foo/trashable).

# 15. Run `make start_scoreboard' to start the score-
#     board background process.  This process makes
#
#		./contest/scoreboard.1
#
#     at intervals if something has changed.  This
#     background process can be stopped by the
#     `make stop_scoreboard' command, its status
#     can be checked by `make check_scoreboard',
#     and the scoreboard can be instantly updated
#     by `make update_scoreboard'.
#
#     The file ./contest/scoreboard.1head, which is
#     prefixed to the file ./contest/scoreboard.1 when
#     the latter is made automatically, can be
#     edited to include contest announcements.  The
#     ./contest/scoreboard.[234] files are the 2'nd,
#     3'rd, and 4'th pages of the scoreboard, and can be
#     created and edited to include more contest an-
#     nouncements.  Only as many of these files as are
#     needed should exist and be world readable.
#
# 16. Be sure to remember to RUN `startcontest' at the
#     time the contest starts.  Also, edit hpcm_
#     judging.rc in the judging directory at that time
#     to set the correct start time for the contest and
#     exclude the demo problems and any test account
#     from the scoreboard.
#
# 17. Contest judging is partly automatic.  You must
#     run the `tracker' program to find submission
#     directories in which `manualreply' is to be run
#     (by `tracker') to compute manual scores, reply to
#     queries, and examine system errors.  You need to
#     practice with `manualreply' before the contest.
#

# 18. If you want to freeze the scoreboard near the end
#     of the contest, edit the freeze time in hpcm_
#     judging.rc accordingly.  Othewise make this time
#     equal to or later than the stop time.
#
# 19. Test the contest by using the test contestant
#     account.  Try submitting the demos/count solution.
#
# 20. You can also test the contest by executing
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
#

# 21. You may want to print out handouts.  The following
#     is a suggestion.  Execute `make print_help', re-
#     move the `demos' and `problems' pages, and reorder
#     the other pages so `index' is first and the other
#     help file pages are in order they are listed in
#     the index.  This produces a `help handout'.
#     Then `make print_problems', and make a `problems
#     handout' that consists of these pages, in the
#     order given in the `problems' help page, preced-
#     ed by that `problems' help page.  Discard any
#     demonstration .txt files printed.
#
#     Then `make print_demos', and make a `demos
#     handout' that consists of these pages, in the
#     order given in the `demos' help page, preceded by
#     that `demos' help page.
#
#     The help and demos handout can be given out before
#     the contest starts, along with any extra sheets
#     on such topics as where teams are located and
#     when and where the food will be (or this info can
#     be put into this_contest).  The problems handout
#     is given out at the very start of the contest
#     proper.
#
# 22. A web page containing non-contest-specific help
#     and demos files can be made according to instruc-
#     tions in hpcm/contestant/help/Makefile.

# The location of the hpcm directory.
#
HPCM=${HOME}/hpcm

# Parameters for running tests
#
S=${HPCM}/examples/test_submissions

# Kill all implicit rules
#
.SUFFIXES:
.SUFFIXES:	.mail .send

# INDICES below is a description of the help, problems,
# and demos files to be available to contestants.
# INDICES consists of a series of entries each concern-
# ing an index file.  For example, the entry:
#
#	    ${HPCM}/contestant/help/index, \
#		common/=help/, \
#		formal/=help/, \
#		advanced/=help/, \
#		    formal:include, \
#		    common:include, \
#		    advanced:include
#
# concerns the index file whose name (relative to this
# directory) is ${HPCM}/contestant/help/index.  You
# you should look at this file.  Each item in this file
# begins with a line of the format:
#
#	*** name option ...

# The instruction `common/=help/' says replace the
# initial substring `common/' with `help/' in the item
# name to make the final file name in the `info' direc-
# tory for email contests or `contest' directory for
# formal or informal contests.  The instruction
# `common:include' says replace the option `common' in
# the index file item by the option `include'.  After
# all replacements are made in the first line of the
# item, the item is used if and only if it has the
# `include' option.  And if used, the item represents a
# file or subdirectory in one of the three contest di-
# rectories `help/', `demos/', or `problems/'.
#
# Note that in `help/index' below (not the same as
# `../contestant/help/index') the items already have the
# `include' option.  `help/index' is used to start the
# contestants `help/index' file and provide a `this_
# contest' item for the file `help/this_contest'.
#
# Note that in `${HPCM}/problem_library/index' demo
# items already have file names beginning with `demos/'.
#
# Note each index file entry consists of a series of
# arguments to makejudgingdir, all but the last of
# which end in `,'.  The first argument is the index
# file name and the other arguments are replacement
# instructions, with `=' for name replacements, and `:'
# for option replacements.  For more details, see the
# documentation of makejudgingdir (run `makejudgingdir
# -doc').

INDICES=    help/index, \
		=help/ \
	    ${HPCM}/contestant/help/index, \
		common/=help/, \
		formal/=help/, \
		advanced/=help/, \
		    formal:include, \
		    common:include, \
		    advanced:include \
	    ${HPCM}/problem_library/index, \
		public/=problems/, \
		private/=problems/, \
		    demo:include, \
		    practice01:include

all:
	makejudgingdir -formal ${INDICES}
	chmod a+x . ..

test:	test_all

trashable:
	makejudgingdir -trashable

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

print_help:
	fprint contest/help/*

print_demos:
	fprint contest/demos/*/*

print_problems:
	@for x in solutions/*/*; \
	    do case "$$x" in \
	    *.txt) echo fprint $$x; fprint $$x;; \
	    *.ps)  echo lpr $$x; lpr $$x;; \
	    *.html | *.htm) \
	           echo YOU MUST use a browser \
			to print $$x by hand;; \
	    esac; done

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
	rm -rf mail help flag log web \
	       solutions* TODO replies mbox \
	       contest/help* \
	       contest/problems* \
	       contest/demos* \
	       contest/scoreboard.1 \
	       contest/printer_map \
	       contest/printer \
	       contest/printer_jobs \
	       contest/who_where \
	       contest/secure
