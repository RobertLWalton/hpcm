#!/bin/sh
#
# Functions to read scorefinder output into an
# internal database and help build a scoreboard.
#
# File:		scoreboard_common.tcl
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Thu Feb  7 13:04:49 EST 2002
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/02/07 18:53:46 $
#   $RCSfile: scoreboard_common.tcl,v $
#   $Revision: 1.28 $
#
#
# Note: An earlier version of this code used to be in
# scoreboard.

# Table of Contents
#
#	Including this Code
#	Scorefinder Data Base
#	Scoreboard Data Base

# Including this Code
# --------- ---- ----

# Include this code in TCL program via:
#
#	set lib_directory \
#	    "[file dirname $argv0]/../lib"
#	source $lib_directory/judging_common.tcl
#	source $lib_directory/scoreboard_common.tcl
#	catch {
#
#	... your program ...
#	... (do not change argc, argv0, argv)
#	... terminates with `exit 0' or `error ...'
#
#	} caught_output
#	caught_error
#
# See $lib_directory/judging_common.tcl for more
# details.

# Scorefinder Data Base
# ---------- ---- ----

# The value scoreboard_array(submitter/problem) is a
# list of items that represent the times and score codes
# read from the input.  Here the codes are
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
# After the scoreboard_array is `pruned' (see below),
# an item is added at the beginning of each array ele-
# ment with code `st' that contains the start time of
# the problem.
#
# An array element may not exist, in which case it
# should be treated as equal to the empty list.
#
# Before pruning, all data from scorefinder whose sub-
# mitter and problem match any regular expressions in
# the scoreboard_submitters and scoreboard_problems glo-
# bal variables are present, in unsorted order.
#
# After pruning, the items in each array element are
# sorted and data meets the following requirements:
#
#    1. Items latter than a correct (ac or mc) item in
#	the same array element have been deleted.
#
#    2. Items later than the problem stop time have been
#       deleted.
#
#    3. Array elements pertaining to a submitter not
#       meeting the cut time have been deleted.

# Function to read scorefinder output and compute
# scoreboard_arary.  Scorefinder output is read from
# input_ch (which is NOT closed by this function).
#
proc compute_scoreboard_array { input_ch } {

    global scoreboard_problems scoreboard_submitters \
	   problem_atoms problem_values \
	   submitter_atoms submitter_values \
           scoreboard_array

    # Compile scoreboard_problems and scoreboard_
    # submitters.
    #
    if { [llength $scoreboard_problems] > 0 } {
	set problem_expression \
	    [compile_logical_expression \
		$scoreboard_problems no_abbreviations \
		problem_atoms problem_values ]
    } else {
	set problem_expression ""
    }
    if { [llength $scoreboard_submitters] > 0 } {
	set submitter_expression \
	    [compile_logical_expression \
		$scoreboard_submitters \
		no_abbreviations \
		submitter_atoms submitter_values ]
    } else {
	set submitter_expression ""
    }

    # Read the scores and accumulate them in the
    # scoreboard_array.
    #
    if { [array exists scoreboard_array] } {
	unset scoreboard_array
    }
    while { "yes" } {
	set line [gets $input_ch]
	if { [eof $input_ch] } break

	if { [llength $line] < 4 } {
	    error "Bad input line from `scorefinder':\
	    	   $line"
	}

	set date	[lindex $line 0]
	set submitter	[lindex $line 1]
	set problem	[lindex $line 2]
	set score	[lindex $line 3]

	if { $problem_expression != "" } {
	     foreach i [array names problem_atoms] {
		 set problem_values($i) \
		     [regexp "^$problem_atoms($i)\$" \
			     $problem]
	     }
	     if { ! [expr $problem_expression] } \
		 continue;
	}

	if { $submitter_expression != "" } {
	     foreach i [array names submitter_atoms] {
		 set submitter_values($i) \
		     [regexp "^$submitter_atoms($i)\$" \
			     $submitter]
	     }
	     if { ! [expr $submitter_expression] } \
		 continue;
	}

	set time [filename_date_to_clock $date]

	set sap $submitter/$problem
	if { [info exists \ scoreboard_array($sap)] } {
	    lappend scoreboard_array(sap) \
		    [list [format {%015d} $time] $score]
	} else {
	    set scoreboard_array(sap) \
		[list [list [format {%015d} $time] \
		            $score]]
	}
    }
}

# Prune the scoreboard_array.  Uses scoreboard_start_
# time, scoreboard_stop_time, scoreboard_correct_cut_
# time, and scoreboard_incorrect_cut_time global vari-
# ables as inputs.
#
proc prune_scoreboard_array { } {

    global scoreboard_start_time \
           scoreboard_stop_time \
           scoreboard_correct_cut_time \
           scoreboard_incorrect_cut_time \
	   scoreboard_array

    # Compile times.
    #
    # start_mode is one of:
    #
    #	"" 		no start_time given
    #	team		start_time is time team gets
    #			first problem
    #   absolute	start_time is date and time
    #
    # stop_mode, correct_cut_mode, and
    # incorrect_cut_mode are one of:
    #
    #	"" 		no stop_time given
    #   absolute	stop_time is date and time
    #   relative	stop_time is relative to problem
    #			start time
    #
    set start_time $scoreboard_start_time
    if { [regexp {^(|team)$} $start_time] } {
        set start_mode $start_time
    } elseif { [regexp {(|-|+)[0-9]+} $start_time] } {
        error "scoreboard_start_time is relative"
    if { ! [regexp {^(|team)$} $start_time] } {
        set start_mode absolute
	set start_time [clock scan $start_time]
    }
    set stop_time $scoreboard_stop_time
    if { $stop_time == "" } {
        set stop_mode $stop_time
    } elseif { [regexp {(|-|+)[0-9]+} $stop_time] } {
        set stop_mode relative
    if { ! [regexp {^(|team)$} $stop_time] } {
        set stop_mode absolute
	set stop_time [clock scan $stop_time]
    }
    set correct_cut_time $scoreboard_correct_cut_time
    if { $correct_cut_time == "" } {
        set correct_cut_mode $correct_cut_time
    } elseif { [regexp {(|-|+)[0-9]+} \
                       $correct_cut_time] } {
        set correct_cut_mode relative
    if { ! [regexp {^(|team)$} $correct_cut_time] } {
        set correct_cut_mode absolute
	set correct_cut_time \
	    [clock scan $correct_cut_time]
    }
    set incorrect_cut_time \
        $scoreboard_incorrect_cut_time
    if { $incorrect_cut_time == "" } {
        set incorrect_cut_mode $incorrect_cut_time
    } elseif { [regexp {(|-|+)[0-9]+} \
                       $incorrect_cut_time] } {
        set incorrect_cut_mode relative
    if { ! [regexp {^(|team)$} $incorrect_cut_time] } {
        set incorrect_cut_mode absolute
	set incorrect_cut_time \
	    [clock scan $incorrect_cut_time]
    }

    # Sort scoreboard_array elements and compute team
    # start times.
    #
    foreach sap [array names scoreboard_array] {
        set items [lsort $scoreboard_array($sap)]
	set scoreboard_array($sap) $items
	if { $start_mode == "team" } {
	    regexp {^([^/]*)/([^/]*)$} $sap forget \
	           submitter problem
	    set t [lindex [lindex $items 0] 0]
	    if {    ! [info exists \
	                    start_array($submitter)] \
	         || $t < $start_array($submitter) } {
	        set start_array($submitter) $t
	    }
	}
    }

    # For each scoreboard_array element, add start time,
    # prune items after stop time, and compute submitter
    # last correct and last incorrect times.
    #
    foreach sap [array names scoreboard_array] {
        set items $scoreboard_array($sap)

    }



	if { $start_time != "" && $start_time != "team" \
	     && $time < $start_time } {
	    continue;
	}

	if { $stop_time != "" && $time > $stop_time } {
	    continue;
	}

	if { ! [lcontain $problems $problem] } {
	    lappend problems $problem
	}

	if { ! [lcontain $submitters $submitter] } {
	    lappend submitters $submitter
	}

	if { $start_time == "team" \
	     && $score == "dg" \
	     && ( ! [info exists \
			  start_time_array($submitter)] \
		  || $start_time_array($submitter) \
		     > $time ) } {
	     set start_time_array($submitter) $time
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
# being equal, which may be true among submitters
# with no correct submissions).  The sort code is
# 000.999999999.000 iff the submitter has made no sub-
# missions and all the problem_scores are `......'.
#
set score_list ""
set max_time_score 999999999

# Which codes are correct and incorrect (others
# are ignored.
# 
if { $scoring_mode == "manual" } {
	set ccodes {mc ac}
	set icodes {mi}
} else {
	set ccodes {mc ac}
	set icodes {mi ai}
}

foreach submitter $submitters {

    # Compute total time score, number of problems cor-
    # rect, modifier (f or n), total number of submis-
    # sions.
    #
    set time_score 0
    set problems_correct 0
    set modifier f
    set submissions 0

    # Compute the problem scores for the score list
    # item in $problem_scores.
    #
    set problem_scores ""
    foreach problem $sorted_problems {

	# Compute problem start time, which may equal
	# "" or "team".
	#
	set problem_start_time $start_time
	if { $problem_start_time == "team"
	     && [info exists \
	              start_time_array($submitter)] } {
	    set problem_start_time \
		$start_time_array($submitter)
	}

	# Compute problem time, number of incorrect
	# submissions, and problem modifier.
	#
	set problem_time	""
	set problem_incorrect	0
	set problem_modifier	f

	if { [info exists \
		   ${submitter}_array($problem)] } {
	    set problem_list \
	        [set ${submitter}_array($problem)]
	} else {
	    set problem_list ""
	}

	foreach item [lsort $problem_list] {
	    set score [lindex $item 1]
	    set item_time [lindex $item 0]
	    if { ! [regexp {^0*([1-9][0-9]*)$} \
			   $item_time forget \
			   item_time] } {
		set item_time 0
	    }

	    if { [lcontain $ccodes $score] } {

		# Score is correct.

		if { $problem_start_time != "" \
		     && $problem_start_time != "team" \
		     		} {
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

	    } elseif { [lcontain $icodes $score] } {

		# Score is incorrect.

		if { $score == "ai" \
		     && $scoring_mode != "auto" } {
		    set problem_modifier n
		}
		incr problem_incorrect
		incr submissions

	    } elseif { $score == "dg" } {

		# Time is when problem was gotten.

	    	if { $problem_start_time == "" } {
		    set problem_start_time \
		    	$item_time
		}
	    }
	}

	# Add to time_score and count of problems cor-
	# rect, and compute problem_score.
	#
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

	# Append problem score to problem_scores list.
	#
	lappend problem_scores $problem_score
    }

    # Add score list item to score list.
    #
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
set spaces32 "                                "
set spaces64 $spaces32$spaces32
set spaces128 $spaces64$spaces64
set spaces256 $spaces128$spaces128

# Compute number of problems per line (ppl).
# Also max_column of output (first column is 0).
# But if there are no problems, exit program without
# producing any output.
#
# scoreboard_width is the number of columns in the
# scoreboard.

set ppl [expr { ( $scoreboard_width - 20 ) / 10 } ]
set np [llength $sorted_problems]
if { $np == 0 } { exit 0 }
if { $np < $ppl } { set ppl $np }
set max_column [expr { 20 + 10 * $ppl - 1 }]

# Compute line marker characters.  First and last
# line markers are SPACE, and in last line all
# spaces will be replaced by underbars (_).
#
# [string index $markers N] marks line N.  The first
# character of `markers' is unused.
#
# scoreboard_markers are the marker characters used,
# in order.
#
set nl [expr { ( $np + $ppl - 1 ) / $ppl }]
if { $nl <= 2 } {
    set markers "X  "
} else {
    set markers $scoreboard_markers
    set markers "X [string range $markers 0 \
                           [expr { $nl - 3 }]] "
}

# Output labeling lines.
#
proc label_problems {} {

    global sorted_problems spaces256 ppl markers \
           max_column

    set count  0
    set marker $markers
    foreach problem $sorted_problems {
	    if { $count % $ppl == 0 } {
	        if { $count > 0 } {
		    puts $line
		}
		set marker [string range $marker 1 end]
		set m [string index $marker 0]

		if { $count == 0 } {
		    set line "Contestant Name"
		} elseif { $count == $ppl } {
		    set line "Correct/Tot_Time"
		} else {
		    set line ""
		}
		set line [format {%-20.20s} $line]
	    }
	    incr count
	    set line "$line$m[format {%9.9s} $problem]"
    }
    if { $count <= $ppl } {
	puts $line
        set line "Correct/Tot_Time"
    }
    puts [translit " " "_" \
		   [string range $line$spaces256 \
			   0 $max_column]]
}

label_problems

# Loop though sorted score_list and output each
# submitter.
#
set current_problems_correct ""
set number_of_problems [llength $sorted_problems]
foreach item [lsort -decreasing $score_list] {

    # If team has no submissions, stop the presses.
    #
    if { [lindex $item 0] == "000.999999999.000" } {
	break
    }

    set problems_correct [lindex $item 1]
    set time_score [lindex $item 2]
    set modifier [lindex $item 3]
    set submitter [lindex $item 4]
    set problem_scores [lreplace $item 0 4]

    set total_score $time_score

    if { $total_score == $max_time_score } {
	set total_score undef
    }

    set total_score "$problems_correct/$total_score"

    if { $modifier == "n" } {
    	set total_score "*${total_score}"
    }

    # Print the line(s) for the team.
    #
    set count 0
    set marker $markers
    foreach score $problem_scores {
	if { $count % $ppl == 0 } {
	    if { $count > 0 } {
		puts $line
	    }
	    set marker [string range $marker 1 end]
	    set m [string index $marker 0]

	    if { $count == 0 } {
		set line $submitter
	    } elseif { $count == $ppl } {
		set line "$total_score"
	    } else {
		set line ""
	    }
	    set line [format {%-20.20s} $line]
	}
	incr count

	set line "$line$m[format {%9.9s} $score]"
    }
    if { $count <= $ppl } {
	puts $line
        set line $total_score
    }
    puts [translit " " "_" \
		   [string range $line$spaces256 \
			   0 $max_column]]
}

exit 0

# Include common error catching code:
#
} caught_output
caught_error
