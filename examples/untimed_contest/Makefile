# Makefile for an `email' Contest
#
# File:		Makefile
# Date:		Thu Nov 30 19:51:59 EST 2000
#
# To make an email contest named `foo':
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
# 4. Run `make' in your judging directory.
#
# 5. Follow the instructions in the TODO file that
#    will have been created in the judging directory.
#    After you follow an instruction, edit it out of
#    the TODO file.
#
# 6. Use `make start_scoreboard' to start the scoreboard
#    crontab job.  Use `make check_scoreboard' to check
#    the crontab job status, and `make stop_scoreboard'
#    to stop the crontab job.
#
# 7. Contest judging is automatic.  If you set the
#    `reply_manager' variable to your email address, you
#    will be notified of any errors or questions.  You
#    many use the `tracker' program to find unchecked
#    errors.

HPCM=${HOME}/hpcm

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
	    selection96:include, \
	    selection97:include

all:
	makejudgingdir -email ${INDICES}
	chmod a+x . ..

scoreboard:
	scorefinder | scoreboard

start_scoreboard:
	crontab crontab

check_scoreboard:
	-crontab -l

stop_scoreboard:
	crontab -r

update_scoreboard:
	sendscoreboard \
        '"wyg2bpitbp scoreboard"<hc3@deas.harvard.edu>'

clean:
	rm -rf info solutions mail help
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
#   $Date: 2000/12/01 02:35:58 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.6 $
