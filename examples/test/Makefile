# Makefile for testing.
#
# File:		Makefile
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Wed Sep 28 14:59:11 EDT 2005
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2005/09/28 18:53:51 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.3 $

# The location of the hpcm directory.
#
HPCM=${HOME}/hpcm

# Kill all implicit rules
#
.SUFFIXES:

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

INDICES=    ${HPCM}/problem_library/test/test_index, \
		=problems/

all:	setup test diff

setup:
	makejudgingdir -solutions -mail ${INDICES}
	chmod a+x . ..

test:
	for x in test_mail/*/Received_Mail; \
	    do \
	    receivemail mail <$$x | dispatchmail mail; \
	    done

diff:
	for x in test_mail/*/Reply_Mail; \
	    do \
	    y=`dirname $$x`; \
	    z=`basename $$y`; \
	    echo mail/$$z/Reply_Mail; \
	    diff test_mail/$$z/Reply_Mail \
	         mail/$$z/Reply_Mail; \
	    done

dangerous_test_clean:
	rm -rf mail/* flag log replies mbox

dangerous_clean:
	rm -rf mail help info* solutions* TODO \
	       flag log \
	       scoreboard.mail Scorefinder_Output \
	       Scoeboard_PID replies mbox web web.tar
