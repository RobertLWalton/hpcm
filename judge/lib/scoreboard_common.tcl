# Functions to read scorefinder output into an
# internal database and help build a scoreboard.
#
# File:		scoreboard_common.tcl
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sat Feb 16 21:18:03 EST 2002
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/02/17 02:18:57 $
#   $RCSfile: scoreboard_common.tcl,v $
#   $Revision: 1.35 $
#
#
# Note: An earlier version of this code used to be in
# scoreboard.

# Table of Contents
#
#	Including this Code
#	Scorefinder Data Base
#	Scoreboard Data Base
#	Scoreboard Functions

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
# ----------- ---- ----

# The value scoreboard_array(submitter/problem) is a
# list of items that represent the times and score codes
# read from the input.  Here the codes are
#
#	g	problem description gotten
#	ACF	where
#	  A = a    if automatic score
#	    = m    if manual score
#	  C = c    if correct score
#	    = i    if incorrect score
#	  F = f    if final score
#	    = n    if non-final score
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
# ment with code `s' that contains the start time of
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
#    1. Items later than a correct (.c.) item in
#	the same array element have been deleted.
#
#    2. Items later than the problem stop time have been
#       deleted.
#
#    3. Array elements pertaining to a submitter not
#       meeting the cut times have been deleted.
#
#    4. If the scoreboard_start_time is "" or "team",
#       any array element whose earliest item (before
#	adding the start time "s" item) does not have
#	code "g" is deleted.

# Function to read scorefinder output and compute
# scoreboard_array.  Scorefinder output is read from
# input_ch (which is NOT closed by this function).
#
proc compute_scoreboard_array { input_ch } {

    global scoreboard_problems scoreboard_submitters \
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
	set code	[lindex $line 3]

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
	set item [list [format {%015d} $time] $code]
	if { [info exists scoreboard_array($sap)] } {
	    lappend scoreboard_array($sap) $item
	} else {
	    set scoreboard_array($sap) [list $item]
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
           scoreboard_final_cut_time \
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
    } elseif { [regexp {^(|\+)[0-9]+$} $start_time] } {
        error "scoreboard_start_time is relative"
    } else {
        set start_mode absolute
	set start_time [clock scan $start_time]
    }
    set stop_time $scoreboard_stop_time
    if { $stop_time == "" } {
        set stop_mode $stop_time
    } elseif { [regexp {^(|\+)[0-9]+$} $stop_time] } {
        set stop_mode relative
    } else {
        set stop_mode absolute
	set stop_time [clock scan $stop_time]
    }
    set correct_cut_time $scoreboard_correct_cut_time
    if { $correct_cut_time == "" } {
        set correct_cut_mode $correct_cut_time
    } elseif { [regexp {^(|\+)[0-9]+$} \
                       $correct_cut_time] } {
        set correct_cut_mode relative
    } else {
        set correct_cut_mode absolute
	set correct_cut_time \
	    [clock scan $correct_cut_time]
    }
    set incorrect_cut_time \
        $scoreboard_incorrect_cut_time
    if { $incorrect_cut_time == "" } {
        set incorrect_cut_mode $incorrect_cut_time
    } elseif { [regexp {^(|\+)[0-9]+$} \
                       $incorrect_cut_time] } {
        set incorrect_cut_mode relative
    } else {
        set incorrect_cut_mode absolute
	set incorrect_cut_time \
	    [clock scan $incorrect_cut_time]
    }
    set final_cut_time \
        $scoreboard_final_cut_time
    if { $final_cut_time == "" } {
        set final_cut_mode $final_cut_time
    } elseif { [regexp {^(|\+)[0-9]+$} \
                       $final_cut_time] } {
        set final_cut_mode relative
    } else {
        set final_cut_mode absolute
	set final_cut_time \
	    [clock scan $final_cut_time]
    }


    # Sort scoreboard_array elements and compute team
    # start times.  Delete elements whose first item
    # is before an absolute start time.
    #
    foreach sap [array names scoreboard_array] {
        set items [lsort $scoreboard_array($sap)]
	set scoreboard_array($sap) $items
	if { $start_mode == "absolute" } {
	    set item [lindex $items 0]
	    set t [lindex $item 0]
	    regexp {^0+(0|[1-9].*)$} $t forget t
	    if { $t < $start_time } {
	        unset scoreboard_array($sap)
	    }
	} elseif { $start_mode == "team" } {
	    regexp {^([^/]*)/([^/]*)$} $sap forget \
	           submitter problem
	    set item [lindex $items 0]
	    set t [lindex $item 0]
	    regexp {^0+(0|[1-9].*)$} $t forget t
	    set c [lindex $item 1]
	    if { $c != "g" } {
	        unset scoreboard_array($sap)
	    } elseif { \
	           ! [info exists \
	                   start_array($submitter)] \
	        || $t < $start_array($submitter) } {
	        set start_array($submitter) $t
	    }
	}
    }

    # For each scoreboard_array element, add start time,
    # set cut_array(submitter) if some submission satis-
    # fies the cut times, and then prune items after
    # stop time.
    #
    foreach sap [array names scoreboard_array] {
        set items $scoreboard_array($sap)
	regexp {^([^/]*)/([^/]*)$} $sap forget \
	       submitter problem

	# Compute times.
	#
	switch $start_mode {
	    "" {
		set item [lindex $items 0]
		set t [lindex $item 0]
		regexp {^0+(0|[1-9].*)$} $t forget t
		set c [lindex $item 1]
		if { $c != "g" } {
		    unset scoreboard_array($sap)
		    continue
		}
		set start $t
	    }
	    team {
		set start $start_array($submitter)
	    }
	    absolute {
		set start $start_time
	    }
	}
	switch $stop_mode {
	    "" {
		set stop ""
	    }
	    relative {
		set stop \
		    [expr { $start + $stop_time }]
	    }
	    absolute {
		set stop $stop_time
	    }
	}
	switch $correct_cut_mode {
	    "" {
		set correct_cut ""
	    }
	    relative {
		set correct_cut \
		    [expr { $start \
		            + $correct_cut_time }]
	    }
	    absolute {
		set correct_cut $correct_cut_time
	    }
	}
	switch $incorrect_cut_mode {
	    "" {
		set incorrect_cut ""
	    }
	    relative {
		set incorrect_cut \
		    [expr { $start \
		            + $incorrect_cut_time }]
	    }
	    absolute {
		set incorrect_cut $incorrect_cut_time
	    }
	}
	switch $final_cut_mode {
	    "" {
		set final_cut ""
	    }
	    relative {
		set final_cut \
		    [expr { $start \
		            + $final_cut_time }]
	    }
	    absolute {
		set final_cut $final_cut_time
	    }
	}

	# Edit list of items and set cut_array(submit-
	# ter) to yes iff there is a correct (or incor-
	# rect not following a correct) score for a pro-
	# blem and submitter after the problem (in)cor-
	# rect cut time.
	#
        set new_items \
	    [list [list [format {%015d} $start] s]]
	foreach item $items {
	    set time [lindex $item 0]
	    regexp {^0+(0|[1-9].*)$} $time forget time
	    set code [lindex $item 1]

	    if { $stop == "" || $time <= $stop } {
		lappend new_items $item
	    }

	    if { [regexp {.c.} $code] } {
	        if {    $correct_cut != "" \
		     && $correct_cut <= $time \
		     && (    $final_cut == "" \
		          || $time <= $final_cut ) } {
		    set cut_array($submitter) yes
		}
		break
	    } elseif { [regexp {.i.} $code] } {
	        if {    $incorrect_cut != "" \
		     && $incorrect_cut <= $time \
		     && (    $final_cut == "" \
		          || $time <= $final_cut ) } {
		    set \
		      cut_array($submitter) yes
		}
	    }
	}
        set scoreboard_array($sap) $new_items
    }

    # If there are cut times, delete submitters that
    # make neither cut.
    #
    if {    $correct_cut_mode != "" \
	 || $incorrect_cut_mode != "" } {
	foreach sap [array names scoreboard_array] {
	    regexp {^([^/]*)/([^/]*)$} $sap forget \
		   submitter problem
	    if { ! [info exists \
	                 $cut_array($submitter)] } {
		unset scoreboard_array($sap)
	    }
        }
    }
}

# Scoreboard Data Base
# ---------- ---- ----

# The scoreboard_list global variable is a decreasing
# order sorted list of items, each of the form:
#
#	{ ccc.ttttttttt.sss problems_correct
#	  time_score modifier
#	  submitter problem_score ... }
#
# Each item begins with a code to sort on, followed by
# the number of correct problems, followed by the score
# in seconds.  The modifier is `f' if all inputs used to
# compute the previous numbers were final, and `n' if
# some were not final.  The submitter is the submitter:
# each submitter has one item in this list.  The pro-
# blem_scores are the scores as they will be printed in
# the output.  The problem names corresponding in order
# to these scores are in the scoreboard_problem_list
# global variable.  This list is sorted.
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
# These submitters should probably be omitted from any
# printed scoreboard.
#
# Note that scoreboard_list is sorted in DESCREASING
# order.
#
set scoreboard_list ""
set scoreboard_problem_list ""

# Function to compute scoreboard_list and scoreboard_
# problem_list from pruned scoreboard_array.
#
proc compute_scoreboard_list {} {

    global scoreboard_array scoreboard_list \
           scoreboard_problem_list scoreboard_penalty


    # Compute lists of submitters and problems.
    #
    set problems ""
    set submitters ""
    foreach sap [array names scoreboard_array] {
	regexp {^([^/]*)/([^/]*)$} $sap forget \
	       submitter problem
	if { ! [lcontain $submitters $submitter] } {
	    lappend submitters $submitter
	}
	if { ! [lcontain $problems $problem] } {
	    lappend problems $problem
	}
    }
    set problems [lsort $problems]
    set scoreboard_problem_list $problems


    # Compute scoreboard_list.
    #
    set scoreboard_list ""
    set max_time_score 999999999
    foreach submitter $submitters {

	# Compute total time score, number of problems
	# correct, modifier (f or n), total number of
	# submissions.
	#
	set time_score 0
	set problems_correct 0
	set modifier f
	set submissions 0

	# Compute the problem scores for the score list
	# item in $problem_scores.
	#
	set problem_scores ""
	foreach problem $problems {

	    set sap $submitter/$problem
	    if { [info exists \
		       scoreboard_array($sap)] } {
		set problem_items \
		       $scoreboard_array($sap)
	    } else {
		set problem_items ""
	    }

	    # Compute problem time, number of incorrect
	    # submissions, and problem modifier.  Pro-
	    # blem_time is "" if no correct submission.
	    #
	    set problem_start_time	""
	    set problem_time		""
	    set problem_incorrect	0
	    set problem_modifier	f

	    foreach item $problem_items {

		set code [lindex $item 1]
		set item_time [lindex $item 0]
		regexp {^0+(0|[1-9].*)$} $item_time \
		       forget item_time

		if { $code == "s" } {

		    # Time is when problem was gotten.

		    set problem_start_time $item_time

		} elseif { [regexp {.c.} $code] } {

		    # Score is correct.

		    set problem_time \
			[expr \
			  { $item_time \
			    - \
			    $problem_start_time }]

		    set problem_increment \
			[expr { \
			   $problem_time \
			   + $scoreboard_penalty \
			     * $problem_incorrect }]
		    if { [expr { \
		            $max_time_score \
			    - $problem_increment }] \
			 < $time_score } {
			set time_score $max_time_score
		    } else {
			incr time_score \
			     $problem_increment
		    }
		    incr problems_correct
		    incr submissions

		} elseif { [regexp {.i.} $code] } {

		    # Score is incorrect.

		    incr problem_incorrect
		    incr submissions
		}

		if { [regexp {..n} $code] } {
		    set problem_modifier n
		    set modifier n
		}
	    }

	    # Append problem score to problem_scores
	    # list.
	    #
	    lappend problem_scores \
	            [format_problem_score \
	    		 $problem_time \
			 $problem_incorrect \
			 $problem_modifier]
	}

	# Add score list item to score list.
	#
	set ccc [format {%03d} $problems_correct]
	set ttttttttt \
	    [format {%09d} \
	            [expr { $max_time_score \
		            - $time_score }]]
	set sss [format {%03d} $submissions]

	lappend scoreboard_list \
		[concat [list $ccc.$ttttttttt.$sss \
		              $problems_correct \
			      $time_score \
		              $modifier $submitter ] \
		        $problem_scores]
    }

    set scoreboard_list \
        [lsort -decreasing $scoreboard_list]
}


# Scoreboard Functions
# ---------- ---------

# Return the printed score of a problem, given the
# problem time (which is "" if the problem was never
# correct), the number of incorrect submissions, and
# the modifier which is "n" if unreviewed non-final
# scores exist.
#
# The time if present is encoded in 6 characters or
# less, the last character indicating unit.  The format
# is one of:
#
#	MM:SSs		s = denotes seconds
#	HH:MMm		m = denotes minutes
#	DD:HHh		h = denotes hours
#	DDDDDd		d = denotes days
#
# If the time is not present, `....' is used to repre-
# sent the missing time.  If the number of submissions
# is not 0, this is appended to the returned score.  If
# the modifier is "n", "*" is prefixed to the score.
#
proc format_problem_score { time incorrect modifier } {

    if { $time != "" } {

	set MM [expr { $time / 60 }]
	set SS [expr { $time - 60 * $MM } ] 
	set HH [expr { $MM / 60 }]
	set MM [expr { $MM - 60 * $HH }]
	set DD [expr { $HH / 24 }]
	set HH [expr { $HH - 24 * $DD }]
	if { $DD > 99 } {
	    set score "${DD}d"
	} elseif { $DD > 0 } {
	    set score "[format {%d:%02d} $DD $HH]h"
	} elseif { $HH > 0 } {
	    set score "[format {%d:%02d} $HH $MM]m"
	} else {
	    set score "[format {%d:%02d} $MM $SS]s"
	}

	incr incorrect 1
	set score $score$incorrect

    } elseif { $incorrect != 0 } {
	set score ..../$incorrect
    } else {
	set score ......
    }

    if { $modifier == "n" } {
	set score *$score
    }

    return $score
}
