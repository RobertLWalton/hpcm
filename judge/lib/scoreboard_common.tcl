#!/bin/sh -f
#
# Runs the scorefinder program to get scores, and
# formats the scores into a table according to
# parameters set in hpcm_judging.rc.
#
# File:		scoreboard
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Tue Sep 19 05:52:16 EDT 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2000/09/19 10:01:42 $
#   $RCSfile: scoreboard_common.tcl,v $
#   $Revision: 1.5 $
#
# The next line starts tcl \
exec tcl "$0" "$@"

# Use tcl rather than tclsh so that `signal' is defined.

# Include common code and parameters:
#
set lib_directory "[file dirname $argv0]/../lib"
source $lib_directory/judging_common.tcl
set log_globally yes
catch {

set document "
scoreboard \[score_file \[parameters_file\] \]

    This program runs `scorefinder' to get scores,
    and organizes the scores into a table.  A score
    is ignored unless

    	its submission time is equal to or greater than
	the value of `scoreboard_start_time';

    	its submission time is equal to or less than
	the value of `scoreboard_stop_time';

	its problem is in the list which is the value of
	`scoreboard_problems', unless that list is
	empty;

	its submitter is in the list which is the value
	of `scoreboard_submitters', unless that list is
	empty.

    The scoreboard has a line for each submitter of the
    form

    submitter  score1  score2  ......  time_score

    Here each score is the score for a problem, and has
    the form `h:mm/n', where h:mm is the time in hour:
    minute format of the first correct submission, and
    n is the number of submissions up to and including
    the first submission.  If n = 0, the problem score
    is `......'.  If there is no correct submission but
    n != 0, the problem score is `..../n'.

    Submission times are measured from the `scoreboard_
    start_time', and are actually measured in seconds.
    The time_score is the sum of all the correct
    submission times, plus a penalty time the number
    of incorrect submissions that are for a problem
    for which there is a later correct submission,
    divided by the scoreboard unit, where all times are
    in seconds.  The scoreboard unit is the value of
    `scoreboard_unit', and is measured in seconds.  The
    penalty is the value of `scoreboard_penalty', and
    is in seconds, regardless of the scoreboard unit.

    The total score is given in seconds, and is preceded
    with an `*' if there is a non-final score involved
    in its computation.  If the scoring mode is `auto',
    all scores are final.  If `manual', only manual
    scores are considered at all, and automatic scores
    are totally ignored.  If `auto+manual', then auto-
    matic scores other than `Completely Correct' are not
    final, and other scores are final.

    The scoreboard groups the teams by the number of
    problems they have correct, and within each group
    sorts teams by total score.

    Normaly no parameters are given to this program, but
    for test purposes and for historical rerun purposes
    a score_file and parameters_file may be given.  The
    score_file is used as the source of scores in place
    of the output of the `scorefinder' program.  The
    parameters_file is read and may contain parameter
    settings, e.g. `scoreboard_problems' etc., in the
    same manner as the normal parameters file."


# If any arguments, print documentation and exit.
#
if { $argc > 2 || [regexp {^-} [lindex $argv 0]] } {
    puts $document
    exit 1
}

# Initialize program parameters.
#
set score_file "| scorefinder"
if { $argc >= 1 } {
    set score_file [lindex $argv 0]
    if { $argc == 2 } {
	source [lindex $argv 1]
    }
}

# The value $submitter-array($problem) is a list of
# items that represent the times and scores read from
# the input.  Here the `scores' are the codes `mc',
# `ac' `mi', or `ai'.  Each item has the form
#
#	{ time-in-seconds score }
#
# where all the times have 15 digits exactly, so they
# can be sorted.
#
# An array element may not exists, in which case it
# should be treated as equal to the empty list.

# The following are the lists of all submitters and
# problems being processed.
#
set submitters ""
set problems ""

# Open a pipe from `scorefinder'.
#
set scores_ch [open $score_file r]

# Read the scores and accumulate them in the submitter
# arrays.
while { "yes" } {
    set line [gets $scores_ch]
    if { [eof $scores_ch] } break

    if { [llength $line] < 4 } {
    	error "Bad input line from `scorefinder': $line"
    }

    set date		[lindex $line 0]
    set submitter	[lindex $line 1]
    set problem		[lindex $line 2]
    set score		[lindex $line 3]

    if { [llength $scoreboard_problems] > 0 \
         && [lsearch -exact $scoreboard_problems \
                         $problem] < 0 } {
    	continue;
    }

    if { [llength $scoreboard_submitters] > 0 \
         && [lsearch -exact $scoreboard_submitters \
                         $submitter] < 0 } {
    	continue;
    }

    if { [lsearch -exact $problems $problem] < 0 } {
	lappend problems $problem
    }

    if { [lsearch -exact $submitters $submitter] < 0 } {
	lappend submitters $submitter
    }

    set time [filename_date_to_clock $date]

    if { $time < $scoreboard_start_time } {
    	continue;
    }

    if { $time > $scoreboard_stop_time } {
    	continue;
    }

    if { [info exists $submitter-array($problem)] } {
	lappend $submitter-array($problem) \
		[list [format {%015d} $time] $score]
    } else {
	set $submitter-array($problem) \
	    [list [list [format {%015d} $time] $score]]
    }
}
close $scores_ch

# Sort the problems alphabetically.
#
set sorted_problems [lsort $problems]

# Create the score list.  Each item in this list
# has the form:
#
#	{ ccc.ttttttttt.sss problems_correct
#	  time_score modifier
#	  submitter problem_score ... }
#
# This begins with a code to sort on, followed by the
# number of correct problems, followed by the score in
# seconds.  The modifier is `f' if all inputs used to
# compute the previous numbers were final, and `n' if
# some were not final.  The submitter is the submitter:
# each submitter has one item in this list.  The
# problem_scores are the scores as they will be printed
# in the output.
#
# In the sort code ccc is the number of correct problems
# in three digits with leading zeros, ttttttttt is
# 10**9 - 1 - the time_score, in 9 digits with leading
# zeros, and sss is the total number of submissions,
# in 3 digits with leading zeros (those with more sub-
# missions are deemed more interesting, other things
# being equal, which they may be true among submitters
# with no correct submissions).  The sort code is
# 000.999999999.000 iff the submitter has made no sub-
# missions, and all the problem_scores are `......'.
# 
# To print an output line for the submitter, the sub-
# mitter needs to be truncated to 8 characters and
# printed left adjusted in 8 columns.  Each problem
# score needs to be printed right adjusted in 8 columns.
# The time_score needs to be changed to have leading
# blanks and printed right adjusted in 8 columns.  If
# modifier is `n' a `*' should replace the first blank
# before the time_score.  All submitters with the same
# number of problems correct are grouped under the
# header `# Problems Correct:'.
#
set score_list ""
foreach submitter $submitters {

    set time_score 0
    set problems_correct 0
    set modifier f
    set problem_scores ""
    set submissions 0

    foreach problem $sorted_problems {
	set problem_incorrect 0
	set problem_time ""
	set problem_modifier f
	if { [info exists \
		   $submitter-array($problem)] } {
	    set problem_list \
	        [set $submitter-array($problem)]
	} else {
	    set problem_list ""
	}
	foreach item [lsort $problem_list] {
	    set score [lindex $item 1]
	    if { [lsearch -exact {mc ac} $score] \
                 >= 0 } {
		set problem_time [lindex $item 0]
		if { ! [regexp {^0*([1-9][0-9]*)$} \
		               $problem_time forget \
			       problem_time] } {
		    set problem_time 0
		}
		set problem_time \
		    [expr { $problem_time \
			    - \
			    $scoreboard_start_time }]
		incr submissions
		break
	    } else {
		if { $score == "ai" \
		     && $scoring_mode != "auto" } {
		    set problem_modifier n
		}
		incr problem_incorrect
		incr submissions
	    }
	}

	if { $problem_time != "" } {
	    set mm [expr { ( $problem_time + 30 ) \
			   / 60 }]
	    set h [expr { $mm / 60 }]
	    set mm [expr { $mm - 60 * $h }]
	    set c [expr { $problem_incorrect + 1 }]
	    if { $h <= 99 && $scoreboard_unit < 86400 } {
		# 86400 = 24 * 60 * 60 = 1 day in seconds
		set problem_score "[format {%02d} $mm]/$c"
		if { $h != 0 } {
		    set problem_score "$h:$problem_score"
		}
	    } else {
		set t [expr { $problem_time \
			      + \
			      $scoreboard_start_time }]
		set d [clock format $t -format "%b%d"]
		set problem_score "$d/$c"
	    }
	    incr problems_correct
	    incr time_score \
		 [expr { ( $problem_time \
		           + $scoreboard_penalty \
			     * $problem_incorrect ) \
			 / $scoreboard_unit }]

	} elseif { $problem_incorrect != 0 } {
	    set problem_score "..../$problem_incorrect"
	} else {
	    set problem_score "......"
	}

	if { $problem_modifier == "n" } {
	    set problem_score "*$problem_score"
	    set modifier n
	}
	lappend problem_scores $problem_score
    }

    set ccc [format {%03d} $problems_correct]
    set ttttttttt \
        [format {%09d} [expr 999999999 - $time_score]]
    set sss [format {%03d} $submissions]

    lappend score_list \
	    "$ccc.$ttttttttt.$sss\
	     $problems_correct $time_score\
	     $modifier $submitter $problem_scores"
}

# Start output of scoreboard.
#
set spaces16 "                "
set spaces8  [string range $spaces16 0 7]

# Output lines labeling problems at top of scoreboard.
#
proc label_problems {} {

    global sorted_problems spaces16

    set count  0
    set indent 8
    foreach problem $sorted_problems {
	    if { $count % 8 == 0 } {
		if { $count > 0 } {
		    puts ""
		}
		puts -nonewline \
		     [string range $spaces16 \
			     0 [expr { $indent - 1 }]]
		incr indent 2
	    }
	    incr count
	    puts -nonewline [format { %7.7s} $problem]
    }
    puts ""
}

# Loop though sorted score_list and output each
# submitter.
#
set current_problems_correct ""
foreach item [lsort -decreasing $score_list] {

    if { [lindex $item 0] == "000.999999999.000" } {
	break
    }

    set problems_correct [lindex $item 1]
    set time_score [lindex $item 2]
    set modifier [lindex $item 3]
    set submitter [lindex $item 4]
    set problem_scores [lreplace $item 0 4]

    if { $time_score >= 1000000 } {
	set time_score "[expr { $time_score / 1000 }]k"
    }
    if { $modifier == "n" } {
    	set time_score "*$time_score"
    }

    if { $problems_correct \
         != $current_problems_correct } {
    	set current_problems_correct $problems_correct
	puts "Teams with $problems_correct Problems\
	      Correct:"
	label_problems
	set item_count 1
    } else {
	if { $item_count % 4 == 0 } {
	    puts ""
	}
	incr item_count
    }

    set count 0
    set indent 8
    foreach score $problem_scores {
	if { $count % 8 == 0 } {
	    if { $count == 8 } {
	    	puts [format { %7.7s} $time_score]
	    } elseif { $count > 0 } {
	    	puts ""
	    }
	    if { $count == 0 } {
		puts -nonewline \
		     [format {%-7.7s } $submitter]
	    } else {
		puts -nonewline \
		     [string range $spaces16 \
			     0 [expr { $indent - 1 }]]
	    }
	    incr indent 2
	}
	incr count
	puts -nonewline [format { %7.7s} $score]
    }

    if { $count <= 8 } {
	puts [format { %7.7s} $time_score]
    } else {
    	puts ""
    }
}

exit 0

# Include common error catching code:
#
} caught_output
caught_error
