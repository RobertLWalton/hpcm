# Makefile for an `informal' Contest
#
# File:		Makefile
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sun Sep 16 03:59:15 EDT 2001
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2001/09/16 08:00:13 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.14 $

# To make an informal contest named `foo':
#
# 1. Import the HPCM system into ~/hpcm.  See directions
#    in `Installing HPCM' (hpcm/judge/bin/installing_
#    hpcm).
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
#    hpcm_judging.rc appropriately.  In this Makefile
#    edit just the INDICES variable below at this time.
#
# 4. Make a contest directory named ~/contest_foo.
#    Symbolically link this to the name
#	~/judging_foo_password/contest
#    by executing inside the judging directory:
#	ln -s ~/contest_foo contest
#
# 5. Symbolically link ~/hpcm/contestant to the
#    name ~/contest_foo/contestant by executing
#    inside the contest directory:
#	ln -s ~/hpcm/contestant contestant
#
# 6. Move the file scoreboard.1head into the ./contest
#    directory (which is now linked to ~/contest_foo).
#

# 7. Create a file named ./contest/secure/passwords
#    that is just a list of all the contestant accounts
#    that will be created.  You must create the
#    ./contest/secure directory.  Be sure this direc-
#    tory and all the files in it are accessible only
#    by the judge's account JJJ.
#
#    Be sure to include a contestant account which
#    you will use to test the system, and to which no
#    actual contestant will be assigned.
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
#    Among other things you will be asked to set edit
#    authentication keys into hpcm_judging.rc in the
#    judging directory and routing information into
#    ~/.procmailrc.
#

#    For an informal contest, you may not need to
#    edit the WHERE fields in the who_where file.  If
#    you do not, printouts will contain the account
#    name.
#
#    Also, `make' executes `startcontest' so it is
#    not necessary to execute that separately.
#
# 10. Rerun `make' in the judging directory.  This is
#     because you edited `help/this_contest' in the last
#     step and need to install the new version.
#
# 11. One of the instructions will be to make a con-
#     testant prototype account: see
#
#	~/hpcm/contestant/bin/SETUP_PROTOTYPE -doc
#
#     Following these directions, you first make a brand
#     new account, the prototype account.  Then you make
#     a prototype directory inside the judge's account,
#     with a name like
#
#	~/prototype_home
#
#     This directory is owned by the prototype account,
#     and is a+rx.  This directory is then symbolically
#     linked to the name ~/contest_foo/home by executing
#     inside the contest directory:
#
#	ln -s ~/prototype_home home

#     Next, before you run SETUP_PROTOTYPE in the proto-
#     type account, move the `procmailrc' file from the
#     judging account to the prototype account's actual
#     home directory (not to ~/prototype_home) , chang-
#     ing its name to `.procmailrc' in that directory.
#     This is a bit tricky, since you are moving a file
#     across accounts: be sure the result has mode 600
#     and owner and group of the prototype account.
#     The `.procmailrc' file must be owned by the
#     account in whose home directory it is placed:
#     this is why it cannot be put in ~/prototype_home
#     like other files (e.g., .cshrc).
#
#     Then run:
#	  ~JJJ/contest_foo/bin/SETUP_PROTOTYPE \
#             ~JJJ/contest_foo
#
#     in prototype account, according to the directions
#     in the documentation of SETUP_PROTOTYPE.
#
#     One prototype directory can be used for many con-
#     tests, so you may not need to make a new prototype
#     directory for this contest, but you will need to
#     symbolically link ~/contest_foo/home.
#
# 12. Be sure you follow the instructions in the SETUP_
#     PROTOTYPE documentation about editing prototype
#     home directory files such as .cshrc to setup the
#     right contestant environment.  Adding ~/.hpcm_
#     contest/bin to the PATH is one necessary thing,
#     but there may be other things the environment
#     needs.  However, you can also do this editing
#     after SETUP_PROTOTYPE is run, as files like .cshrc
#     are NOT in setup.tar: the tar file used to ini-
#     tialize contestant account contents.  What is in
#     setup.tar is only a symbolic link to ~/.hpcm_
#     contest/home/.cshrc.

#     Any such editing must be done by logging into
#     the prototype account and cd'ing to .hpcm_
#     contest/home in that account.
#
#     Another good thing to add to the environment is
#     `alias ll -lL'.  This is actually of more use in
#     formal contests, where gotten files are symbol-
#     ically linked, and contestants may be confused by
#     all the symbolic links.  But prototype home dir-
#     ectories may be shared between informal and for-
#     mal contests, so it is useful to have this envir-
#     onment setting.
#
#
# 13. Be sure there are appropriate account names and
#     passwords in the file ./contest/secure/passwords,
#     as you are about to make these accounts and set
#     these passwords.  The command
#
#		makejudgingdir -secure
#
#     will add random passwords to this file for any
#     account that lacks a password.  The format of the
#     file lines is:
#
#		account-name:password:...
#
#     with `:' separated fields.  Fields at the end,
#     optionally including the password and everything
#     after it, can be omitted.
#
#     You should have one of these contestant accounts
#     be a test account that you can use to check
#     whether the system is running.

# 14. Run `make trashable' in your judging directory.
#     This makes the file ./contest/trashable which
#     lists the contestant accounts that can be reini-
#     tialized by `TRASH_ACCOUNT'.  These accounts have
#     presumably not been made yet (but its OK if they
#     already exist).
#
#
# 15. Then as root (you must log into the root account,
#     you cannot use `su', as the paths will not be
#     right), run the following commands:
#
#	cd ~JJJ/hpcm/secure/bin
#	./hpcm_make_accounts ~JJJ/contest_foo
#	./hpcm_set_passwords ~JJJ/contest_foo
#	./hpcm_TRASH_ACCOUNTS ~JJJ/contest_foo
#
#     These will successively make the accounts listed
#     in ./contest/secure/passwords, set the passwords
#     of these accounts to the passwords listed in that
#     file, and reinitialize these accounts to have
#     exactly the same files (and symbolic links) as the
#     prototype account did the last time SETUP_PROTO-
#     TYPE was run in the prototype account (except for
#     the fact that in each contestant account ~/.hpcm_
#     contest is symbolically linked to ~JJJ/contest_foo
#     after the contestant account has been trashed,
#     regardless of which contest ~/.hpcm_contest was
#     linked to in the prototype account).
#
# 16. Run `make nontrashable' in your judging directory.
#     This removes the file ./contest/trashable and pre-
#     vents any contestants from accidentally trashing
#     their own account by running TRASH_ACCOUNT (which
#     requires the contestant's account name appear in
#     ~JJJ/contest_foo/trashable).

# 17. Run `makescoreboard -start' to start the score-
#     board background process.  This process makes
#		./contest/scoreboard.1
#     at intervals if something has changed.  This
#     background process can be stopped by the
#     `makescoreboard -stop' command, its status
#     can be checked by `makescoreboard -status',
#     and the scoreboard can be instantly updated
#     by `makescoreboard 0'.
#
#
# 18. Contest judging is automatic.  If you set the
#     `reply_manager' and `log_manager' variables to
#     your email address, you will be notified of any
#     errors or questions.  You many use the `tracker'
#     program to find unchecked errors.
#
# 19. Test the contest by using the test contestant
#     account.

# The location of the hpcm directory.
#
HPCM=${HOME}/hpcm

# INDICES below is a description of the help, problems,
# and demos files to be available to contestants.
# INDICES consists of a series of entries each concern-
# ing an index file.  For #example, the entry:
#
#	    ${HPCM}/contestant/help/index, \
#		common/=help/, \
#		email/=help/, \
#		    email:include, \
#		    common:include \
#		    advanced:include \
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
# file or subdirectory in one of the three directories
# `help/', `demos/', or `problems/'.
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
		    common:include \
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
