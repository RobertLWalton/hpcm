#!/bin/sh -f
#
# Runs the scorefinder program to get scores, and
# formats the scores into a table according to
# parameters set in hpcm_judging.rc.
#
# File:		scoreboard
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Thu Sep 28 20:21:26 EDT 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2000/09/29 01:51:30 $
#   $RCSfile: scoreboard_common.tcl,v $
#   $Revision: 1.7 $
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
scoreboard \[parameters_file\]

    This program inputs the output of `scorefinder' to
    get scores, and organizes the scores into a table.
    A score is ignored unless

    	its submission time is equal to or greater than
	the value of `scoreboard_start_time', unless
	that value is \"\";

    	its submission time is equal to or less than
	the value of `scoreboard_stop_time', unless
	that value is \"\";

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
    the form `X:YYuN', where X:YY is the time in units u
    (s = seconds, m = minutes, h = hours, d = days) the
    first correct submission, and N is the number of
    submissions up to and including the first submis-
    sion.  If N = 0, the problem score is `......'.  If
    there is no correct submission but n != 0, the
    problem score is `..../N'.  If there is a correct
    submission but the start time of the problem cannot
    be computed, the score is `undef/N'.

    Submission times are measured from the `scoreboard_
    start_time', and are actually measured in seconds,
    unless the `scoreboard_start_time' is \"\", in which
    case each problem's submission time is the time the
    problem description was gotten.  The time_score is
    the sum of all the correct submission times, plus a
    penalty time the number of incorrect submissions
    that are for a problem for which there is a later
    correct submission, where all times are in seconds.
    The penalty is the value of `scoreboard_penalty',
    and is in seconds.

    The total score is given in seconds, and is preceded
    with an `*' if there is a non-final score involved
    in its computation.  If the total score is too large
    or cannot be computed because of an `undef' problem
    score, the total score is `undef'.  If the scoring
    mode is `auto', all scores are final.  If `manual',
    only manual scores are considered at all, and auto-
    matic scores are totally ignored.  If `auto+manual',
    then automatic scores other than `Completely
    Correct' are not final, and other scores are final.

    The scoreboard groups the teams by the number of
    problems they have correct, and within each group
    sorts teams by total score.

    A parameters_file may be given to this program that
    will be loaded into the program after hpcm_jud-
    ging.rc.  This parameters_file may contain parameter
    settings, e.g. `scoreboard_problems' etc., in the
    same manner as the normal parameters file."


# If any non parameters_file arguments, print
# documentation and exit.
#
if { $argc > 1 \
     || ( $argc == 1 \
          && [regexp {^-} [lindex $argv 0]] ) } {
    puts $document
    exit 1
}

# Initialize program parameters.
#
if { $argc == 1 } {
    source [lindex $argv 0]
}

# The value ${submitter}_array($problem) is a list of
# items that represent the times and score codes read
# from the input.  Here the codes are
#
#	dg	problem description gotten
#	ac	automatically scored correct
#	ai	automatically scored incorrect
#	mc	manually scored correct
#	mi	manually scored incorrect
#
# Each item has the form
#
#	{ time-in-seconds code }
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

# Read the scores and accumulate them in the submitter
# arrays.
while { "yes" } {
    set line [gets stdin]
    if { [eof stdin] } break

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

    if { $scoreboard_start_time != "" \
         && $time < $scoreboard_start_time } {
    	continue;
    }

    if { $scoreboard_stop_time != "" \
         && $time > $scoreboard_stop_time } {
    	continue;
    }

    if { [info exists ${submitter}_array($problem)] } {
	lappend ${submitter}_array($problem) \
		[list [format {%015d} $time] $score]
    } else {
	set ${submitter}_array($problem) \
	    [list [list [format {%015d} $time] $score]]
    }
}

# Sort the problems alphabetically.
#
set sorted_problems [lsort $problems]

# Return a time in 6 characters or less, the last
# character indicating unit.  The format is either
#
#	MM:SSs		s = denotes seconds
#	HH:MMm		m = denotes minutes
#	DD:HHh		h = denotes hours
#	DDDDDd		d = denotes days
#
proc format_time { time } {
    set MM [expr { $time / 60 }]
    set SS [expr { $time - 60 * $MM } ] 
    set HH [expr { $MM / 60 }]
    set MM [expr { $MM - 60 * $HH }]
    set DD [expr { $HH / 24 }]
    set HH [expr { $HH - 24 * $DD }]
    if { $DD > 99 } {
    	return "${DD}d"
    } elseif { $DD > 0 } {
    	return "[format {%d:%02d} $DD $HH]h"
    } elseif { $HH > 0 } {
    	return "[format {%d:%02d} $HH $MM]m"
    } else {
    	return "[format {%d:%02d} $MM $SS]s"
    }
}

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
set max_time_score 999999999
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
		   ${submitter}_array($problem)] } {
	    set problem_list \
	        [set ${submitter}_array($problem)]
	} else {
	    set problem_list ""
	}

	set problem_start_time $scoreboard_start_time

	foreach item [lsort $problem_list] {
	    set score [lindex $item 1]
	    set item_time [lindex $item 0]
	    if { ! [regexp {^0*([1-9][0-9]*)$} \
			   $item_time forget \
			   item_time] } {
		set item_time 0
	    }
	    if { [lsearch -exact {mc ac} $score] \
                 >= 0 } {
		if { $problem_start_time != "" } {
		    set problem_time \
			[expr \
			  { $item_time \
			    - \
			    $problem_start_time }]
		} else {
		    set problem_time undef
		}
		incr submissions
		break
	    } elseif { [lsearch -exact {mi ai} $score] \
                       >= 0 } {
		if { $score == "ai" \
		     && $scoring_mode != "auto" } {
		    set problem_modifier n
		}
		incr problem_incorrect
		incr submissions
	    } elseif { $score == "dg" } {
	    	if { $problem_start_time == "" } {
		    set problem_start_time \
		    	$item_time
		}
	    }
	}

	if { $problem_time == "undef" } {

	    set c [expr { $problem_incorrect + 1 }]
	    set problem_score "undef/$c"
	    set time_score $max_time_score
	    incr problems_correct

	} elseif { $problem_time != "" } {

	    set c [expr { $problem_incorrect + 1 }]
	    set problem_score \
	        "[format_time $problem_time]$c"
	    set problem_increment \
		[expr { ( $problem_time \
		          + $scoreboard_penalty \
			    * $problem_incorrect ) }]
	    if { [expr { $max_time_score \
		            - $problem_increment }] \
		 < $time_score } {
		set time_score $max_time_score
	    } else {
		incr time_score $problem_increment
	    }
	    incr problems_correct

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

# Output lines labeling problems at top of scoreboard.
#
proc label_problems {} {

    global sorted_problems spaces16

    set count  0
    set indent 13
    foreach problem $sorted_problems {
	    if { $count % 7 == 0 } {
		if { $count == 7 } {
		    puts [format { %10.10s} "total"]
		} elseif { $count > 0 } {
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
    if { $count <= 7 } {
	puts [format { %10.10s} "total"]
    } else {
    	puts ""
    }
}

# Loop though sorted score_list and output each
# submitter.
#
set current_problems_correct ""
set number_of_problems [llength $sorted_problems]
foreach item [lsort -decreasing $score_list] {

    if { [lindex $item 0] == "000.999999999.000" } {
	break
    }

    set problems_correct [lindex $item 1]
    set time_score [lindex $item 2]
    set modifier [lindex $item 3]
    set submitter [lindex $item 4]
    set problem_scores [lreplace $item 0 4]

    if { $time_score == $max_time_score } {
	set time_score undef
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
	if { $item_count % 4 == 0 \
	     && $number_of_problems <= 7 } {
	    puts ""
	}
	incr item_count
    }

    set count 0
    set indent 13
    foreach score $problem_scores {
	if { $count % 7 == 0 } {
	    if { $count == 7 } {
	    	puts [format { %10.10s} $time_score]
	    } elseif { $count > 0 } {
	    	puts ""
	    }
	    if { $count == 0 } {
		puts -nonewline \
		     [format {%-12.12s } $submitter]
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

    if { $count <= 7 } {
	puts [format { %10.10s} $time_score]
    } else {
    	puts ""
    }
}

exit 0

# Include common error catching code:
#
} caught_output
caught_error
