# Makefile for an `informal' Contest
#
# File:		Makefile
# Date:		Thu Nov 30 21:52:05 EST 2000
#
# To make an informal contest named `foo':
#
# 1. Import the HPCM system into ~/hpcm.
#
# 2. Create a judging directory ~/judging_foo_password
#    where the password is needed to keep outsiders from
#    guessing the path to the received submissions,
#    because java requires this path be a+x to access
#    .class files inside the sandbox.
#
#    This password is never really used, and so can be
#    something awfully unguessable.
#
# 3. Put copies of the files in this directory into
#    your judging directory, and edit the names and
#    passwords in these files appropriately.
#
# 4. Make a contest directroy named ~/contest_foo.
#    Symbolically link this to the name
#	~/judging_foo_password/contest
#
# 5. Symbolically link ~/hpcm/contestant to the
#    name ~/contest_foo/contestant.
#
# 6. Move the file scoreboard.1head into the
#    ~/contest_foo (now also the ./contest) directory.
#
# 7. Create a file named contest_foo/secure/passwords
#    that is just a list of all the contestant accounts
#    that will be created.  You must create the
#    contest_foo/secure directory.  Be sure this
#    directory and all the files in it are accessible
#    only to the judge's account JJJ.
#
# 8. Run `make' in your judging directory.
#
# 9. Follow the instructions in the TODO file that
#    will have been created in the judging directory.
#    After you follow an instruction, edit it out of
#    the TODO file.
#
# 9. Make a new account, the prototype account.  Make a
#    directory ~/prototype_home which is owned by that
#    account and has mode a+rx,u+w.  Symbolically link
#    this directory to the name ~/contest_foo/home.
#
#    Log into the prototype account and run
#
# /home/JJJ/contest_foo/contestant/bin/SETUP_PROTOTYPE \
#		/home/JJJ/contest_foo
#
#    where JJJ is the name of the contest judging ac-
#    count.  This will move files and directories into
#    the ~/prototype_home directory, make them a+r or
#    a+rx, and make symbolic links to these moved files
#    and directories in the home directory of the proto-
#    type account.  It will also make the files
#	    /home/JJJ/contest_foo/home/setup.tar
#	    /home/JJJ/contest_foo/home/setup.ls
#    that define the initial contents of each contestant
#    account to be identical to that of the prototype
#    account after the above file and directory
#    movement.
#
# 10. Run `make trashable' in your judging directory.
#     This makes the file contest_foo/trashable which
#     lists the contestant accounts that can be
#     reinitialized by `TRASH_ACCOUNT'.  These accounts
#     may not be made yet, however.
#     however, ma
#
# 9. Use `make start_scoreboard' to start the scoreboard
#    crontab job.  Use `make check_scoreboard' to check
#    the crontab job status, and `make stop_scoreboard'
#    to stop the crontab job.
#
# 10. Contest judging is automatic.  If you set the
#     `reply_manager' variable to your email address, you
#     will be notified of any errors or questions.  You
#     many use the `tracker' program to find unchecked
#     errors.

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

trashable:
	makejudgingdir -trashable

scoreboard:
	scorefinder | scoreboard

start_contest:
	startcontest

start_scoreboard:
	rm -f Scoreboard_PID
	makescoreboard &; echo $$! >Scoreboard_PID

stop_scoreboard:
	kill -HUP `cat Scoreboard_PID`
	sleep 1
	kill -HUP `cat Scoreboard_PID`

update_scoreboard:
	makescoreboard 0
#
# Author:	Bob Walton (walton@deas.harvard.edu)
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2000/12/01 10:23:36 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.1 $
