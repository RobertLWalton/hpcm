# Makefile for an `informal' Contest
#
# File:		Makefile
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Tue Jan  9 13:17:34 EST 2001
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2001/01/09 18:30:55 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.1 $

# To make a formal contest named `foo':
#
#  1. Import the HPCM system into ~/hpcm.  See direc-
#     tions in `Installing HPCM' (hpcm/judge/bin/
#     installing_hpcm).
#
#  2. Create a judging directory ~/judging_foo_password
#     where the password is needed to keep outsiders
#     from guessing the path to the received submis-
#     sions, because JAVA requires this path be a+x to
#     access .class files inside the sandbox.
#
#     This password is never really used, and so can be
#     something awfully unguessable.
#
# 3.  Put copies of the files in this directory into
#     your judging directory, and edit the names and
#     passwords in these files appropriately.  Edit the
#     index specifications specifying the problems you
#     want to include (at the end of the value of the
#     INDICES variable below) to specify the problems
#     you want (run `makejudgingdir -doc' to find the
#     format and meaning of index specifications).
#
#  4. Make a contest directory named ~/contest_foo.
#     Symbolically link this to the name
#	~/judging_foo_password/contest
#     by executing inside the judging directory:
#	ln -s ~/contest_foo contest
#
#  5. Symbolically link ~/hpcm/contestant to the
#     name ~/contest_foo/contestant by executing
#     inside the contest directory:
#	ln -s ~/hpcm/contestant contestant
#
#  6. Move the file scoreboard.1head into the ./contest
#     directory (which is now linked to ~/contest_foo).
#

#  7. Create a file named ./contest/secure/passwords
#     that is just a list of all the contestant accounts
#     that will be created.  You must create the
#     ./contest/secure directory.  Be sure this direc-
#     tory and all the files in it are accessible only
#     by the judge's account JJJ.
#
#     Be sure to include a contestant account which
#     you will use to test the system, and to which no
#     actual contestant will be assigned.
#
#  8. Run `make this_contest' and then edit the `./help/
#     this_contest' file.
#
#  9. Run `make' in your judging directory.
#
#     You should rerun `make' whenever you may have
#     changed something it uses to build the contest
#     files.  The worst that can happen from running
#     `make' unnecessarily is to get redundant instruc-
#     tions in the ./TODO file (see below).
#
# 10. Follow the instructions in the TODO file that
#     will have been created in the judging directory.
#     After you follow an instruction, edit it out of
#     the TODO file.
#
#     Among other things you will be asked to set edit
#     authentication keys into hpcm_judging.rc in the
#     judging directory and routing information into
#     ~/.procmailrc.
#
#     Another thing you will be asked to do is edit
#     the WHERE fields in the who_where file.  If
#     you do not, printouts will contain the account
#     name.  If you do, be sure the WHERE fields are
#     short enough not to overrun other printout
#     header fields.

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
#
#     Note that you do NOT need a .procmailrc file for
#     a formal contest.  But you might want to include
#     one (see ../informal_contest/Makefile) if you are
#     going to share the prototype home directory
#     between formal and informal contests.
#
#     Then run:
#	  ~JJJ/contest_foo/bin/SETUP_PROTOTYPE \
#             ~JJJ/contest_foo in the
#     in prototype account, according to the directions
#     in the documentation of SETUP_PROTOTYPE.
#
#     One prototype directory can be used for many con-
#     tests, so you may not need to make a new prototype
#     directory for this contest, but you will need to
#     symbolically link ~/contest_foo/home.

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
#
#     Any such editing must be done by logging into
#     the prototype account and cd'ing to .hpcm_
#     contest/home in that account.
#
#     Two good things to add to .cshrc for a formal
#     contest are `alias ll -lL' and `unset autologout'.
#     The first is useful because in formal contests
#     files gotten by hpcm_get are symbolically linked,
#     and contestants may be confused by all the symbol-
#     ic links.  The second is useful if the judges are
#     logging in contestants, as without it the computer
#     may log contestants out at awkward times.
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
#
# 14. Run `make trashable' in your judging directory.
#     This makes the file ./contest/trashable which
#     lists the contestant accounts that can be reini-
#     tialized by `TRASH_ACCOUNT'.  These accounts have
#     presumably not been made yet (but its OK if they
#     already exist).
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
#
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

# 18. Be sure to remember to RUN `startcontest' at
#     the time the contest starts.  Also, edit
#     hpcm_judging.rc in the judging directory at
#     that time to set the correct start time for
#     the contest, exclude the demo problems, and
#     exclude any test account.
#
# 19. Contest judging is partly automatic.  You must
#     run the `tracker' program to find submission
#     directories in which `manualreply' is to be run
#     (by `tracker') to compute manual scores, reply to
#     queries, and examine system errors.  You need to
#     practice with `manualreply' before the contest.
#
# 20. If you want to freeze the scoreboard near the end
#     of the contest, follow the directions in
#     `makescoreboard -doc'

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
	    preliminary97:include, \
	    bospre2000:include

all:
	makejudgingdir -informal ${INDICES}
	chmod a+x . ..

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
