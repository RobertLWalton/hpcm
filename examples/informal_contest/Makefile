# Makefile for an Informal Contest
#
# File:		Makefile
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sun Sep 23 21:36:44 EDT 2001
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2001/09/24 01:31:31 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.19 $

# To make an informal contest named `foo':
#
# 1. Import the HPCM system into ~/hpcm.  See directions
#    in `Installing HPCM' (hpcm/judge/bin/installing_
#    hpcm).
#
# 2. Make a contestant prototype account, which we will
#    refer to here as PPP.  Follow the instructions in:
#
#	~/hpcm/contestant/bin/SETUP_PROTOTYPE -doc
#
#    Below we will make a symbolic link to ~PPP/home.
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
# 5. Make directory ~/contest_foo.  Then make the
#    symbolic links:
#
#      ~/judging_foo_password/contest -> ~/contest_foo
#      ~/contest_foo/contestant -> ~/hpcm/contestant
#      ~/contest_foo/home -> ~PPP/home
#
#    by executing:

#	cd ~/judging_foo_password
#	ln -s ~/contest_foo contest
#
#	cd ~/contest_foo
#	ln -s ~/hpcm/contestant contestant
#	ln -s ~PPP/home home
#
# 6. Move the file scoreboard.1head into the ./contest
#    directory (which is now linked to ~/contest_foo)
#    by executing:
#
#	cd ~/judging_foo_password
#	move scoreboard.1head contest
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
#
#    The `~/contest_foo/secure' directory contains stuff
#    that the judge MUST keep secret.  The `passwords'
#    file in this directory should be just a list of
#    all the contestant accounts that will be created,
#    one account name per line.  An example file would
#    be:
#
#	conttest:none
#	cont1
#	cont2
#	.....
#	cont20

#    Be sure to include a contestant account which
#    you will use to test the system, and to which no
#    actual contestant will be assigned.  Here we
#    included `conttest' for this purpose.  Also, we
#    put a `:none' after it.  This is in the password
#    position and signals that we are NOT going to let
#    the computer assign a password to this account.
#    You must then assign the password manually by
#    executing `passwd conttest' in the root account.
#
# 8. Run `make' in your judging directory.  Note this
#    runs `startcontest', which merely gives a+r
#    permission to the ./contest/help/problems file
#    that lists the problems.
#
#    You should rerun `make' whenever you may have
#    changed something it uses to build the contest
#    files.  The worst that can happen from running
#    `make' unnecessarily is to get redundant instruc-
#    tions in the ./TODO file (see below).
#
# 9. Follow the instructions in the TODO file that
#    will have been created in the judging directory.
#    After you follow an instruction, edit it out of
#    the TODO file.
#
#    If an instruction says `rerun makejudgingdir',
#    do this by rerunning `make'.
#
#    Among other things you will be asked to set edit
#    authentication keys into hpcm_judging.rc in the
#    judging directory and routing information into
#    ~/.procmailrc.
#
#    For an informal contest, you may not need to edit
#    the WHERE fields in the who_where file.  If you do
#    not, printouts will contain the account name.

#    Also, `make' executes `startcontest' so it is
#    not necessary to execute that separately.
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
#
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
#
# 13. Then as root (you must log into the root account,
#     you cannot use `su', as the paths will not be
#     right), run the following commands, where JJJ is
#     the judge's account:
#
#	cd ~JJJ/hpcm/secure/bin
#	./hpcm_make_accounts ~JJJ/contest_foo
#	./hpcm_set_passwords ~JJJ/contest_foo
#	./hpcm_TRASH_ACCOUNTS ~JJJ/contest_foo

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
#
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

#     The file ./contest/scoreboard.1head, which is
#     prefixed to the file ./contest/scoreboard.1 when
#     the latter is made automatically, can be
#     edited to include contest announcements.  The
#     ./contest/scoreboard.[234] files are the 2'nd,
#     3'rd, and 4'th pages of the scoreboard, and can be
#     created and edited to include more contest an-
#     nouncements.  Only as many of these files as are
#     needed should exist.
#
# 16. Contest judging is automatic.  If you set the
#     `reply_manager' and `log_manager' variables to
#     your email address, you will be notified of any
#     errors or questions.  You many use the `tracker'
#     program to find unchecked errors.
#
# 17. Test the contest by using the test contestant
#     account.  Try submitting the demos/count solution.

# The location of the hpcm directory.
#
HPCM=${HOME}/hpcm

# INDICES below is a description of the help, problems,
# and demos files to be available to contestants.
# INDICES consists of a series of entries each concern-
# ing an index file.  For example, the entry:
#
#	    ${HPCM}/contestant/help/index, \
#		common/=help/, \
#		informal/=help/, \
#		advanced/=help/, \
#		    informal:include, \
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
# Note that in `help/index' the items already have the
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
		informal/=help/, \
		advanced/=help/, \
		    informal:include, \
		    common:include, \
		    advanced:include \
	    ${HPCM}/problem_library/index, \
		public/=problems/, \
		private/=problems/, \
		    demo:include, \
		    practice01:include

all:
	makejudgingdir -informal ${INDICES}
	chmod a+x . ..
	startcontest

trashable:
	makejudgingdir -trashable

nontrashable:
	rm -f ./contest/trashable

scoreboard:
	scorefinder | scoreboard

start_scoreboard:
	makescoreboard -start

check_scoreboard:
	makescoreboard -status

stop_scoreboard:
	makescoreboard -stop

update_scoreboard:
	makescoreboard 0
