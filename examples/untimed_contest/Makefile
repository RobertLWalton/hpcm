# Makefile for an `email' Contest
#
# File:		Makefile
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sun Jan 27 19:48:20 EST 2002
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/01/28 00:50:26 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.26 $

# To make an email contest named `foo':
#
# 1. Create a judging account (see `Judging', hpcm/
#    judging/bin/judging) and import the HPCM system
#    into its ~/hpcm directory (see `Installing HPCM',
#    hpcm/judge/bin/installing_hpcm).
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
# 4. Run `make' in your judging directory.
#
#    You should rerun `make' whenever you may have
#    changed something it uses to build the contest
#    files.  The worst that can happen from running
#    `make' unnecessarily is to get redundant instruc-
#    tions in the ./TODO file (see below).
#
# 5. Follow the instructions in the TODO file that
#    will have been created in the judging directory.
#    After you follow an instruction, edit it out of
#    the TODO file.
#
#    If an instruction says `rerun makejudgingdir',
#    do this by rerunning `make'.
#
#    Among other things you will be asked to set edit
#    routing information into ~/.procmailrc.

# 6. Review the `makescoreboard' program (see `make-
#    scoreboard -doc').  Use `make start_scoreboard' to
#    start the scoreboard job, `make check_scoreboard'
#    to check job status, `make stop_scoreboard' to stop
#    the job, and make `update_scoreboard' to provide an
#    instant update of the scoreboard.
#
#    The scoreboard.notes file is appended to the score-
#    board.  Edit this file as you wish.
#
# 7. Contest judging is automatic.  If you set the
#    `reply_manager' and `log_manager' variables in
#    hpcm_judging.rc to your email address, you will be
#    notified of any errors and of any questions submit-
#    ted by contestants.  You many use the `tracker'
#    program to find unchecked errors.
#
#    Note that you do NOT need to execute `startcontest'
#    to start an email contest.  This is only needed for
#    formal contests.
#
# 8. Test the contest by cd'ing to the `hpcm/judge/test'
#    directory, executing in that directory:
#
#	echo >.HPCM_ADDRESS 'judges-email-address'
#
#    and then executing `make'.  Then read your e-mail
#    to see if the X-HPCM-Test-Subject field in the
#    reply messages from the judge matches the score or
#    other output in the message.  Note that one of the
#    submissions times out, and for that the judge will
#    take over a minute to reply.
#
#    Note also that some or all of the submissions gen-
#    erated by the `test_submit' part of `make' may be
#    for problems you did not include in your contest,
#    and these will generate `illegal name' messages
#    instead of `Completely Correct' messages.  You may
#    wish to edit hpcm/judge/test/Makefile to fix this.

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
#		email/=help/, \
#		advanced/=help/, \
#		    email:include, \
#		    common:include, \
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
		email/=help/, \
		advanced/=help/, \
		    email:include, \
		    common:include, \
		    advanced:include \
	    ${HPCM}/problem_library/index, \
		public/=problems/, \
		private/=problems/, \
		    demo:include, \
		    practice01:include

all:
	makejudgingdir -email ${INDICES}
	chmod a+x . ..

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
