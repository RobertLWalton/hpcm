# Makefile for an `email' Contest
#
# File:		Makefile
# Date:		Tue Jan  9 12:12:40 EST 2001
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
#   $Date: 2001/01/09 17:37:13 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.11 $

# To make an email contest named `foo':
#
# 1. Import the HPCM system into ~/hpcm.  See directions
#    in `Installing HPCM' (hpcm/judge/bin/installing_
#    hpcm).
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
#    passwords in these files appropriately.  Edit the
#    index specifications specifying the problems you
#    want to include (at the end of the value of the
#    INDICES variable below) to specify the problems you
#    want (run `makejudgingdir -doc' to find the format
#    and meaning of index specifications).
#
# 4. Run `make this_contest' and then edit the `./help/
#    this_contest' file.
#
# 5. Run `make' in your judging directory.
#
#    You should rerun `make' whenever you may have
#    changed something it uses to build the contest
#    files.  The worst that can happen from running
#    `make' unnecessarily is to get redundant instruc-
#    tions in the ./TODO file (see below).

# 6. Follow the instructions in the TODO file that
#    will have been created in the judging directory.
#    After you follow an instruction, edit it out of
#    the TODO file.
#
#    Among other things you will be asked to set edit
#    routing information into ~/.procmailrc.
#
# 7. Review the -email option of the `makescoreboard'
#    program (see `makescoreboard -doc').  If you wish
#    to use this to send the scoreboard somewhere to be
#    posted on the web, edit the SCOREBOARD_ADDRESS
#    parameter in this Makefile, and use `make start_
#    scoreboard' to start the scoreboard job.  Use
#    `make check_scoreboard' to check job status,
#    `make stop_scoreboard' to stop the job, and
#    make `update_scoreboard' to provide an instant
#    update of the scoreboard.
#
# 8. Contest judging is automatic.  If you set the
#    `reply_manager' and `log_manager' variables in
#    hpcm_judging.rc to your email address, you will be
#    notified of any errors and of any questions submit-
#    ted by contestants.  You many use the `tracker'
#    program to find unchecked errors.
#
#    Note that you do NOT need to execute `startcontest'
#    to start an email contest.  This is only needed for
#    formal contests.

HPCM=${HOME}/hpcm

SCOREBOARD_ADDRESS="password"<account@computer>

INDICES=    help/index, \
	    =help/ \
	    ${HPCM}/contestant/help/index, \
	    common/=help/, \
	    email/=help/, \
	    email:include, \
	    common:include \
	    ${HPCM}/problem_library/index, \
	    public/=problems/, \
	    private/=problems/, \
	    demo:include, \
	    preliminary96:include, \
	    preliminary97:include, \
	    bospre2000:include

all:
	makejudgingdir -email ${INDICES}
	chmod a+x . ..

this_contest:
	makejudgingdir -this_contest

scoreboard:
	scorefinder | scoreboard

start_scoreboard:
	makescoreboard -start \
	               -email '${SCOREBOARD_ADDRESS}'

check_scoreboard:
	makescoreboard -status

stop_scoreboard:
	makescoreboard -stop

update_scoreboard:
	makescoreboard -email '${SCOREBOARD_ADDRESS}' \
		       0
