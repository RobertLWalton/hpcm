# Makefile for an `informal' Contest
#
# File:		Makefile
# Date:		Sun Dec 10 05:03:15 EST 2000
#
# To make an informal contest named `foo':
#
#  1. Import the HPCM system into ~/hpcm.
#
#  2. Assuming /sbin is the right directory for secure
#     root setuid programs, run:
#
#			cd ~/hpcm/secure/src
#			su
#			make root_install
#			exit
#
#     This installs the hpcm_sandbox program as a setuid
#     root program in the system /sbin directory, which
#     is the appropriate thing for Linux.  If you put
#     this setuid binary in another place, say `/foo',
#     you should replace the `make' command above by
#
#		make SECURE_BIN=/foo root_install
#
#     If you skip this step, an unsecure hpcm_sandbox
#     will be installed (in the next step) for the
#     judge, which will mean that malicious contestant
#     programs could corrupt the judging files.
#
#  3. Run		cd ~/hpcm
#			make
#
#     This sets some file modes and makes a few binary
#     files.
#
#  4. Put ~/hcpm/judge/bin in the PATH of the current
#     (judge's) account.  Do NOT put ~/hpcm/contestant/
#     bin in that PATH.
#

#  5. Create a judging directory ~/judging_foo_password
#     where the password is needed to keep outsiders
#     from guessing the path to the received submis-
#     sions, because JAVA requires this path be a+x to
#     access .class files inside the sandbox.
#
#     This password is never really used, and so can be
#     something awfully unguessable.
#
#  6. Put copies of the files in this directory into
#     your judging directory, and edit the names and
#     passwords in these files appropriately.  Edit the
#     index specifications (value of the INDICES vari-
#     able below) to specify the problems you want (run
#     `makejudgingdir -doc' to find the format and mean-
#     ing of index specifications).
#
#  7. Make a contest directory named ~/contest_foo.
#     Symbolically link this to the name
#	~/judging_foo_password/contest
#     by executing inside the judging directory:
#	ln -s ~/contest_foo contest
#
#  8. Symbolically link ~/hpcm/contestant to the
#     name ~/contest_foo/contestant by executing
#     inside the contest directory:
#	ln -s ~/hpcm/contestant contestant
#
#  9. Move the file scoreboard.1head into the
#     ~/contest_foo (now also the ./contest) directory.
#

# 10. Create a file named contest_foo/secure/passwords
#     that is just a list of all the contestant accounts
#     that will be created.  You must create the
#     contest_foo/secure directory.  Be sure this
#     directory and all the files in it are accessible
#     only to the judge's account JJJ.
#
#     Be sure to include a `contestant account' which
#     you will use to test the system, and to which no
#     actual contestant will be assigned.
#
# 11. Run `make this_contest' and then edit the `./help/
#     this_contest' file.
#
# 12. Run `make' in your judging directory.  Note this
#     runs `startcontest', which merely gives a+r
#     permission to the ./contest/help/problems file
#     that lists the problems.
#
#     You should rerun `make' whenever you may have
#     changed something it uses to build the contest
#     files.  The worst that can happen from running
#     `make' unnecessarily is to get redundant instruc-
#     tions in the ./TODO file (see below).
#
# 13. Follow the instructions in the TODO file that
#     will have been created in the judging directory.
#     After you follow an instruction, edit it out of
#     the TODO file.
#
#     Among other things you will be asked to set edit
#     authentication keys into hpcm_judging.rc in the
#     judging directory and routing information into
#     ~/.procmailrc.
#

#     For an informal contest, you may not need to
#     edit the WHERE fields in the who_where file.  If
#     you do not, printouts will contain the account
#     name.
#
#     Also, `make;' executes `startcontest' so it is
#     not necessary to execute it separately.
#
# 14. One of the instructions will be to make a con-
#     testant prototype account: see
#
#	~/hpcm/contestant/bin/SETUP_PROTOTYPE -doc
#
#     This makes a prototype directory, with a name like
#
#	~/prototype_home
#
#     Symbolically link this directory to the name
#     ~/contest_foo/home by executing inside the contest
#     directory:
#
#	ln -s ~/prototype_home home
#
#     One prototype directory can be used for many con-
#     tests, so you may not need to make a new prototype
#     directory for this contest, but you will need to
#     symbolically link ~/contest_foo/home.
#

#     Be sure you follow the instructions in the SETUP_
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
#
# 15. Be sure there are appropriate account names and
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
#

# 16. Run `make trashable' in your judging directory.
#     This makes the file ./contest/trashable which
#     lists the contestant accounts that can be reini-
#     tialized by `TRASH_ACCOUNT'.  These accounts have
#     presumably not been made yet (but its OK if they
#     already exist).
#
# 17. Then as root (you must log into the root account,
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
# 18. Run `make nontrashable' in your judging directory.
#     This removes the file ./contest/trashable and pre-
#     vents any contestants from accidentally trashing
#     their own account by running TRASH_ACCOUNT (which
#     requires the contestant's account name appear in
#     ~JJJ/contest_foo/trashable).
#

# 19. Run `makescoreboard -start' to start the score-
#     board background process.  This process makes
#		./contest/scoreboard.1
#     at intervals if something has changed.  This
#     background process can be stopped by the
#     `makescoreboard -stop' command, and its status
#     can be checked by `makescoreboard -status'.
#
# 20. Contest judging is automatic.  If you set the
#     `reply_manager' and `log_manager' variables to
#     your email address, you will be notified of any
#     errors or questions.  You many use the `tracker'
#     program to find unchecked errors.

HPCM=${HOME}/hpcm

INDICES=    help/index, \
	    =help/ \
	    ${HPCM}/contestant/help/index, \
	    common/=help/, \
	    informal/=help/, \
	    informal:include, \
	    common:include \
	    ${HPCM}/problem_library/index, \
	    public/=problems/, \
	    private/=problems/, \
	    demo:include, \
	    preliminary96:include, \
	    selection96:include, \
	    selection97:include

all:
	makejudgingdir -informal ${INDICES}
	chmod a+x . ..
	startcontest

this_contest:
	makejudgingdir -this_contest

trashable:
	makejudgingdir -trashable

nontrashable:
	rm -f ./contest/trashable

scoreboard:
	scorefinder | scoreboard

start_scoreboard:
	makescoreboard -start

stat_scoreboard:
	makescoreboard -status

stop_scoreboard:
	makescoreboard -stop

update_scoreboard:
	makescoreboard 0



# Author:	Bob Walton (walton@deas.harvard.edu)
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2000/12/10 10:28:57 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.7 $
