# Functions to read scorefinder output into an
# internal database and help build a scoreboard.
#
# File:		scoreboard_common.tcl
# Author:	Bob Walton (walton@seas.harvard.edu)
# Date:		Sun Sep 29 15:16:45 EDT 2013
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2013/09/30 00:26:04 $
#   $RCSfile: scoreboard_common.tcl,v $
#   $Revision: 1.68 $
#
#
# Note: An earlier version of this code used to be in
# scoretable.

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
#	... terminates with `exit 0', `exit 1', or
#	... `error ...'
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
#	ACFS	where
#	  A = a    score is from Auto_Score
#	    = m    score is from Manual_Score
#	  C = c    `Completely Correct' score
#	    = i    otherwise
#	  F = f    final score (Reply_Mail file exists)
#	    = n    otherwise
#	  S = i    submit qualifier is `in'
#	    = o    submit qualifier is `inout'
#	    = f    submit qualifier is `first'
#	    = s    submit qualifier is `summary'
#	    = x    otherwise
#	s	problem start time (added during
#		   pruning: see below)
#
# Each item has the form
#
#	{ time-in-seconds code }
#
# where all the times have 15 digits exactly, so they
# can be sorted.  The items of scoreboard_array(
# submitter/problem) are sorted (during pruning),
# except for first element, which always has the `s'
# code.
#
# After the scoreboard_array is `pruned' (see below),
# an item is added at the beginning of each array ele-
# ment with code `s' that contains the start time of
# the problem.  This start time is computed as follows:
#
#    scoreboard_start_time	start time value
#
#	""			""
#	problem			time problem gotten
#	team			time submitter got
#				first problem
#	<DATE-AND-TIME>		<DATE-AND-TIME>
#
# An array element may not exist, in which case it
# should be treated as equal to the empty list.
#
# Before pruning, all data from scorefinder are present
# whose submitter and problem match any logical expres-
# sions in the scoreboard_submitters and scoreboard_
# problems global variables, and whose scorefinder codes
# match the regular expression selected by the scoring_
# mode and the scoreboard_mode_array.  The unpruned data
# are in unsorted order.
#
# After pruning, the items in each array element are
# sorted and data meets the following requirements:
#
#    1. Items later than a correct (.c..) item in
#	the same array element have been deleted.
#
#    2. Items later than the problem stop time have been
#       deleted.
#
#    3. Array elements pertaining to a submitter not
#       meeting the cut times have been deleted.
#
#    4. If the scoreboard_start_time is "problem" or
#	"team", any array element is deleted if its
#	earliest item (before pruning or adding the
#	start time "s" item) does not have code "g".
#
#    5. If the scoreboard_start_time is a date and time,
#	any array element is deleted if its earliest
#	item is before this contest start time.
#
#    6. All "g" code items are deleted.

# Function to read scorefinder output and compute
# scoreboard_array.  Scorefinder output is read from
# input_ch (which is NOT closed by this function).
#
proc compute_scoreboard_array { input_ch } {

    global scoreboard_problems scoreboard_submitters \
           scoreboard_array scoreboard_mode_array \
	   scoring_mode

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
    set code_regexp \
        "^$scoreboard_mode_array($scoring_mode)\$"
    while { "yes" } {
	set line [gets $input_ch]
	if { [eof $input_ch] } break

	if {     [catch { set len [llength $line] }] \
	     || $len < 4 } {
	    error "Bad input line from `scorefinder':\
	    	   $line"
	}

	set date	[lindex $line 0]
	set submitter	[lindex $line 1]
	set problem	[lindex $line 2]
	set code	[lindex $line 3]

	if { ! [regexp $code_regexp $code] } continue

	if { $problem_expression != "" } {
	     foreach i [array names problem_atoms] {
		 set problem_values($i) \
		     [regexp "^($problem_atoms($i))\$" \
			     $problem]
	     }
	     if { [catch { \
	             set v [expr $problem_expression] \
		         }] } { \
	         error "Badly constructed\
		        scoreboard_problems logical\
		        expression:\
		        $scoreboard_problems"
	     } elseif { ! $v } continue;
	}

	if { $submitter_expression != "" } {
	     foreach i [array names submitter_atoms] {
		 set submitter_values($i) \
		     [regexp \
		         "^($submitter_atoms($i))\$" \
			 $submitter]
	     }
	     if { [catch { \
	             set v \
		         [expr $submitter_expression] \
		         }] } { \
	         error "Badly constructed\
		        scoreboard_submitters logical\
		        expression:\
		        $scoreboard_submitters"
	     } elseif { ! $v } continue;
	}

	set time [filename_date_to_clock $date]

	set sap $submitter/$problem
	set item [list [format {%015d} $time] $code]
	lappend scoreboard_array($sap) $item
    }
}

# Prune the scoreboard_array.  Uses scoreboard_start_
# time, scoreboard_stop_time, scoreboard_correct_cut_
# time, scoreboard_incorrect_cut_time, and scoreboard_
# final_cut global variables as inputs.
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
    #	"" 		problem start time is unknown
    #	problem 	start_time is when problem is
    #			gotten
    #	team		start_time is time team gets
    #			first problem
    #   absolute	start_time is date and time
    #
    # stop_mode is one of:
    #
    #	"" 		no stop_time given
    #   absolute	stop_time is date and time
    #   relative	stop_time is relative to problem
    #			start time
    #
    # correct_cut_mode, incorrect_cut_mode, and final_
    # cut_mode are like stop_mode.
    #
    set start_time $scoreboard_start_time
    if { [regexp {^(|problem|team)$} $start_time] } {
        set start_mode $start_time
    } elseif { [regexp {^(|\+)[0-9]+$} $start_time] } {
        error "scoreboard_start_time is relative"
    } else {
        set start_mode absolute
	if { [catch { set start_time \
	                  [clock scan $start_time] }] \
			  	} {
	    error "Badly formatted\
	           scoreboard_start_time:\
	           $scoreboard_start_time"
	}
    }
    set stop_time $scoreboard_stop_time
    if { $stop_time == "" } {
        set stop_mode $stop_time
    } elseif { [regexp {^(|\+)[0-9]+$} $stop_time] } {
        set stop_mode relative
	if { $start_mode == "" } {
	    error "stop time is relative and start \
	           time is not given"
	}
    } else {
        set stop_mode absolute
	if { [catch { set stop_time \
	                  [clock scan $stop_time] }] } {
	    error "Badly formatted\
	           scoreboard_stop_time:\
		   $scoreboard_stop_time"
	}
    }
    set correct_cut_time $scoreboard_correct_cut_time
    if { $correct_cut_time == "" } {
        set correct_cut_mode $correct_cut_time
    } elseif { [regexp {^(|\+)[0-9]+$} \
                       $correct_cut_time] } {
        set correct_cut_mode relative
	if { $start_mode == "" } {
	    error "correct cut time is relative and \
	           start time is not given"
	}
    } else {
        set correct_cut_mode absolute
	if { [catch { set correct_cut_time \
	                  [clock scan \
			         $correct_cut_time] }] \
				 	} {
	        error "Badly formatted\
		       scoreboard_correct_cut_time:\
		       $scoreboard_correct_cut_time"
	    }
    }
    set incorrect_cut_time \
        $scoreboard_incorrect_cut_time
    if { $incorrect_cut_time == "" } {
        set incorrect_cut_mode $incorrect_cut_time
    } elseif { [regexp {^(|\+)[0-9]+$} \
                       $incorrect_cut_time] } {
        set incorrect_cut_mode relative
	if { $start_mode == "" } {
	    error "incorrect cut time is relative and \
	           start time is not given"
	}
    } else {
        set incorrect_cut_mode absolute
	if { [catch { set incorrect_cut_time \
	                  [clock scan \
			         $incorrect_cut_time] \
				       }] } {
	        error "Badly formatted\
		       scoreboard_incorrect_cut_time:\
		       $scoreboard_incorrect_cut_time"
	    }
    }
    set final_cut_time \
        $scoreboard_final_cut_time
    if { $final_cut_time == "" } {
        set final_cut_mode $final_cut_time
    } elseif { [regexp {^(|\+)[0-9]+$} \
                       $final_cut_time] } {
        set final_cut_mode relative
	if { $start_mode == "" } {
	    error "final cut time is relative and \
	           start time is not given"
	}
    } else {
        set final_cut_mode absolute
	if { [catch { set final_cut_time \
	                  [clock scan \
			         $final_cut_time] }] \
					    } {
	        error "Badly formatted\
		       scoreboard_final_cut_time:\
		       $scoreboard_final_cut_time"
	    }
    }


    # Sort scoreboard_array elements and compute team
    # start times.  Delete elements whose first item
    # is before an absolute start time or whose first
    # item does not have code "g" if problem start
    # time is computed from when problem was gotten.
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
	} elseif { [lcontain {problem team} \
	                     $start_mode] } {
	    set item [lindex $items 0]
	    set c [lindex $item 1]
	    if { $c != "g" } {
	        unset scoreboard_array($sap)
	    } elseif { $start_mode == "team" } {
		regexp {^([^/]*)/([^/]*)$} $sap forget \
		       submitter problem
		set t [lindex $item 0]
		regexp {^0+(0|[1-9].*)$} $t forget t
	        if { ! [info exists \
	                     start_array($submitter)] \
	             || $t < $start_array($submitter) \
		     		} {
		    set start_array($submitter) $t
		}
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
	    	set start ""
	    }
	    problem {
		set item [lindex $items 0]
		set t [lindex $item 0]
		regexp {^0+(0|[1-9].*)$} $t forget t
		set c [lindex $item 1]
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
	# rect cut time but before any final cut time.
	# Delete "g" items.  Delete items after any
	# stop time.
	#
	if { $start_mode != "" } {
	    set start [format {%015d} $start]
	}
	set new_items [list [list $start s]]
	foreach item $items {
	    set time [lindex $item 0]
	    regexp {^0+(0|[1-9].*)$} $time forget time
	    set code [lindex $item 1]

	    if { $code != "g" \
	         && ( $stop == "" \
		      || $time <= $stop ) } {
		lappend new_items $item
	    }

	    if { [regexp {.c..} $code] } {
	        if {    $correct_cut != "" \
		     && $correct_cut <= $time \
		     && (    $final_cut == "" \
		          || $time <= $final_cut ) } {
		    set cut_array($submitter) yes
		}
		break
	    } elseif { [regexp {.i..} $code] } {
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

# The scoreboard_list global variable is an increasing
# order sorted list of items, each of the form:
#
#	{ sort_code submitter number_of_submissions
#	  ranking_score problem_score ... }
#
# Each item begins with a code to sort on, followed by
# submitter, total number of submissions, printable
# ranking score for the submitter, and printable
# problem scores for each problem for the submitter.
#
# Each submitter has one item in this list.  The problem
# problem names corresponding in order to these scores
# are in the scoreboard_problem_list global variable,
# which is sorted.
#
# The ranking_score may be "".
#
# The format of the sort_code and ranking_score depend
# upon the settings of scoreboard_sort_mode, which can
# have the folloiwng values:
#
#			sort_code	ranking_score
#
#   submitter		submitter	""
#
#   time		PPPP.TTTTTTTTT	P/TTT
#
#   problems		PPPP.submitter	P
#
#   score		SSSSS.SS	SSSSS.SS
#
#   problems/score	PPPP.SSSSSS.SS	P/SSSSS.SS
#
#   score/problems	SSSSSS.SS.PPPP	SSSSS.SS/PPPP
#
# Here P is the number of correct problems, and PPPP
# is 9999-P in 4 digits with high order zeros if
# necessary.  TTT is the total time in seconds, and
# TTTTTTTTT is that time in 9 digits with high order
# zeros as necessary.  SSSSS.SS is the score to 2
# decimal places and SSSSSS.SS is 999999.99-SSSSS.SS
# with two decimal places and high order zeros if
# necessary to make 6 integer digits.
#
# The scoreboard_list is sorted in INCREASING order.
# Thus higher P values are earlier, higher TTT values
# are later, higher SSSS.SS values are earlier,
# lexically later submitters are later.
#
# The format of the problem_scores is as returned by
# format_problem_score defined elsewhere.  The problem_
# scores will not be more than 9 characters long.
#
set scoreboard_list ""
set scoreboard_problem_list ""

# Function to compute scoreboard_list and scoreboard_
# problem_list from pruned scoreboard_array.
#
proc compute_scoreboard_list {} {

    global scoreboard_array scoreboard_list \
           scoreboard_problem_list \
	   scoreboard_sort_mode \
	   scoreboard_start_time

    # Compute lists of submitters and problems.
    #
    foreach sap [array names scoreboard_array] {
	regexp {^([^/]*)/([^/]*)$} $sap forget \
	       submitter problem

	set code \
	    [lindex [lindex $scoreboard_array($sap) \
	                    end] \
		    1]
	if { [regexp {.[ic]..} $code] } {
	    set submitter_present($submitter) yes
	    set problem_present($problem) yes
	}
    }
    set submitters \
        [lsort [array names submitter_present]]
    set problems \
        [lsort [array names problem_present]]

    set scoreboard_problem_list $problems


    # Compute scoreboard_list.
    #
    set scoreboard_list ""
    foreach submitter $submitters {

	# Compute total time score, total qualifier
	# score, number of problems correct, modifier
	# (f or n), and total number of submissions.
	# Modifier is f unless some problem has the
	# modifier n.
	#
	set total_time 0
	set total_score 0.0

	set problems_correct 0
	set submissions 0

	set modifier f

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

	    # Compute problem start time, problem time,
	    # number of incorrect submissions, problem
	    # modifier (f or n), and number of incorrect
	    # submissions with each qualifier (in,
	    # inout, etc.).
	    #
	    # Problem start time is "" if scoreboard_
	    # start_time is "", and is otherwise a time
	    # in the format of [clock seconds].
	    #
	    # Problem time is "" if there is no correct
	    # submission.  Otherwise it is the time of
	    # the correct submission in [clock seconds]
	    # format if the problem start time is "", or
	    # is the time of the correct submission
	    # minus the problem start time, in seconds,
	    # if the problem start time is not "".
	    #
	    set problem_start_time	""
	    set problem_time		""
	    set problem_incorrect	0
	    set problem_modifier	f

	    # Counts of incorrect submissions with
	    # given submit qualifiers.
	    #
	    set problem_i_qualified	0
	    set problem_o_qualified	0
	    set problem_x_qualified	0

	    foreach item $problem_items {

		set code [lindex $item 1]
		set item_time [lindex $item 0]
		regexp {^0+(0|[1-9].*)$} $item_time \
		       forget item_time

		if { $code == "s" } {

		    # Time is problem start time.

		    set problem_start_time $item_time

		} elseif { [regexp {.c..} $code] } {

		    # Score is correct.

		    if { $problem_start_time == "" } {
			set problem_time $item_time
		    } else {
			set problem_time \
			    [expr \
			      { $item_time \
				- \
				$problem_start_time }]
		    }
		    incr problems_correct
		    incr submissions

		} elseif { [regexp {.i..} $code] } {

		    # Score is incorrect.

		    set qualifier [string index $code 3]
		    if { ! [lcontain {i o x} \
		    		     $qualifier] } {
		        error "bad qualifier in\
			       scorefinder code: $code"
		    }
		    incr problem_${qualifier}_qualified

		    incr problem_incorrect
		    incr submissions
		}

		if { [regexp {..n.} $code] } {
		    set problem_modifier n
		    set modifier n
		}
	    }

	    # Append problem score to problem_scores
	    # list and update total_time and qualifier
	    # score.
	    #
	    lappend problem_scores \
	            [format_problem_score \
			 total_time \
			 total_score \
			 $problem \
	    		 $problem_time \
			 $problem_incorrect \
			 $problem_modifier \
			 [list $problem_i_qualified \
			       $problem_o_qualified \
			       $problem_x_qualified]]
	}

	# Compute sort_code and mark problems_correct
	# and ranking_score missing when appropriate.
	#
	set P $problems_correct
	set PPPP [format {%04d} [expr { 9999 - $P }]]
	set sort_code ""
	set ranking_score ""

	if { $scoreboard_sort_mode == "submitter" } {
	    set sort_code $submitter
	    set ranking_score ""
	} elseif {    $scoreboard_sort_mode \
	           == "problems" } {
	    set sort_code $PPPP
	    set ranking_score $P
	} elseif { $scoreboard_sort_mode == "time" } {
	    if { $scoreboard_start_time == "" } {
	        error "scoreboard_sort_mode is `time'\
		       but scoreboard_start_time is\
		       \"\""
	    }
	    set TTT $total_time
	    set TTTTTTTTT [format {%09d} $TTT]
	    set sort_code "$PPPP.$TTTTTTTTT"
	    set ranking_score "$P/$TTT"
	} else {
	    set SSSSS [format {%.2f} $total_score]
	    set SSSSSSSSS \
	        [format {%09.2f} \
		        [expr 999999.99 - $SSSSS]]
	    if { $scoreboard_sort_mode == "score" } {
	        set sort_code $SSSSSSSSS
		set ranking_score $SSSSS
	    } elseif {    $scoreboard_sort_mode
	               == "problems/score" } {
	        set sort_code $PPPP.$SSSSSSSSS
		set ranking_score $P/$SSSSS
	    } elseif {    $scoreboard_sort_mode
	               == "score/problems" } {
	        set sort_code $SSSSSSSSS.$PPPP
		set ranking_score $SSSSS/$P
	    }
	}

	if { $sort_code == "" } {
	    error "`$scoreboard_sort_mode' is a bad\
	           value for scoreboard_sort_mode"
	}

	if { $ranking_score != "" \
	     && \
	     $modifier == "n" } {
	    set ranking_score "*${ranking_score}"
	}
	    
	# Add score list item to score list.
	#
	lappend scoreboard_list \
		[concat [list $sort_code $submitter \
		              $submissions \
			      $ranking_score ] \
		        $problem_scores]
    }

    set scoreboard_list [lsort $scoreboard_list]
}


# Scoreboard Functions
# ---------- ---------

# Return the printable score of a problem.  The
# arguments are:
#
#   total_time_name
#       Name of variable containing account total_time
#   total_score_name
#       Name of variable containing account total_score
#   problem
#       Name of problem
#   problem_time
#       If "", problem has not been solved.
#       Else if scoreboard_start_time != "", this is the
#            difference between the time of the first
#            correct submission and the problem start
#            time, in seconds.
#       Else this is the time of the first correct
#            submission as per [clock seconds].
#   incorrect
#       Number of incorrect submissions
#   modifier 
#       "n" if some submission scores have not yet been
#       reviewed by the human judge
#   qualified_submissions
#       [list I O X] where I is the number of "in"
#       incorrect submissions, "O" the number of "inout"
#       incorrect submissions, and X the number of ""
#       incorrect submissions.
#     
# The problem score is added to total_score and if
# problem_time and scoreboard_start_time are both not
# "" then the problem_time is added to total_time.
#
# The value returned has 9 or fewer characters unless
# there are more than 99 submissions.
#
# If problem_time is "" indicating the problem is
# not correct, then if incorrect is 0, "......" is
# returned, but if incorrect is N > 0, "..../N" is
# returned if scoreboard_unsolved_mode is "count",
# and "IiOoX" is returned if scoreboard_unsolved_mode
# is "qualifier".
#
# Otherwise if the problem_time is NOT "" then the
# problem score is computed and added to total_score.
# If scoreboard_solved_mode is "score" then "SSS/N"
# is returned where SSS is the problem score rounded
# to the nearest integer and N = incorrect + 1 is the
# number of submissions.
#
# If the problem_time is NOT "" and scoreboard_start_
# time is NOT "", then problem_time is the elapsed time
# from the problem start time, and it is added to total_
# time.  Then if scoreboard_solved_mode is "time",
# "TTT/N" is returned where TTT is the problem time and
# N = incorrect + 1 is the number of submissions.  Here
# TTT has the first of the following formats that fits
# into 6 characters:
#
#	MM:SSs		s = denotes seconds
#	HH:MMm		m = denotes minutes
#	DD:HHh		h = denotes hours
#	DDDDDd		d = denotes days
#
# If the problem_time is NOT "" and scoreboard_start_
# time IS "", then problem_time is the actual time of
# first correct submission.  If problem_solved_mode is
# "date", this time is returned in the format:
#
#	ddmmmyy
#
# where dd is the day of the month, mmm the month in 3
# letters, and yy the last two digits of the year.  If
# there would be more than 9 characters in the printable
# score, the year is omitted.
#
# If the modifier is "n" and either there were submis-
# sions, correct or not, "*" is prefixed to the score to
# indicate the score is subject to change by manual
# review.
#
proc format_problem_score \
    { total_time_name total_score_name
      problem problem_time incorrect modifier \
      qualified_submissions } {

    global scoreboard_start_time \
    	   scoreboard_penalty \
           scoreboard_solved_mode \
           scoreboard_unsolved_mode \
	   scoreboard_factor \
	   scoreboard_max_score

    # Compute:
    #
    #	long_score	use if short enough
    #	short_score	use otherwise

    if { $problem_time == "" } {

        # Problem is unsolved.

        if { $incorrect == 0 } {
	    set long_score "......"
	    set short_score $long_score
	} elseif {    $scoreboard_unsolved_mode \
	           == "count" } {
	    set long_score "..../$incorrect"
	    set short_score ".../$incorrect"
	} elseif {    $scoreboard_unsolved_mode \
	           == "qualifier" } {
	    set i 0
	    set long_score ""
	    while { $i < 3 } {
	        set count \
		    [lindex $qualified_submissions $i]
	        set postfix \
		    [lindex {i o ""} $i]
		set long_score \
		    "$long_score$count$postfix"
		incr i
	    }
	    set short_score $long_score
	} else {
	    error "`$scoreboard_unsolved_mode' is bad\
	           value for scoreboard_unsolved_mode"
	}
    } else {

        # Problem is solved.

        set submissions [expr $incorrect + 1]
	set long_score ""
	    # If not reset then scoreboard_solved_mode
	    # has a bad value.

	# Compute problem score
	#
        if { [info exists \
	           scoreboard_max_score($problem)] } {
	    set score $scoreboard_max_score($problem)
	} else {
	    set score 100.0
	}
	set i 0
	while { $i < 3 } {
	    set count [lindex $qualified_submissions $i]
	    set qualifier [lindex {in inout ""} $i]
	    if { [info exists \
	               scoreboard_factor($qualifier)] \
		       } {
		set factor \
		    $scoreboard_factor($qualifier)
		set score \
		    [expr {   $score \
		            * pow ($factor, $count) }]
	    }
	    incr i
	}
        upvar $total_score_name total_score
	set total_score [expr $total_score + $score]

	if { $scoreboard_solved_mode == "score" } {
	    set round_score [expr { round($score) }]
	    set long_score "$round_score/$submissions"
	    set short_score $round_score
	}

	if { $scoreboard_start_time != "" } {

	    upvar $total_time_name total_time
	    set max_total_time 999999999
	    set problem_increment \
		[expr {   $problem_time \
			+   $scoreboard_penalty \
			  * $incorrect }]
	    if {   [expr { $max_total_time \
			   - $problem_increment }] \
		 < $total_time } {
		set total_time $max_total_time
	    } else {
		incr total_time $problem_increment
	    }

	    if { $scoreboard_solved_mode == "time" } {

		set MM [expr { $problem_time / 60 }]
		set SS \
		    [expr { $problem_time - 60 * $MM } ]
		set HH [expr { $MM / 60 }]
		set MM [expr { $MM - 60 * $HH }]
		set DD [expr { $HH / 24 }]
		set HH [expr { $HH - 24 * $DD }]
		if { $DD > 99 } {
		    set long_score "${DD}d"
		} elseif { $DD > 0 } {
		    set long_score \
			"[format {%d:%02d} $DD $HH]h"
		} elseif { $HH > 0 } {
		    set long_score \
			"[format {%d:%02d} $HH $MM]m"
		} else {
		    set long_score \
			"[format {%d:%02d} $MM $SS]s"
		}

		set long_score \
		    "$long_score/$submissions"
		set short_score $long_score
	    }
	} elseif { $scoreboard_solved_mode == "time" } {
	    error "scoreboard_solved_mode is `time' but\
	           scoreboard_start_time is \"\""
	}

	if { $scoreboard_solved_mode == "date" } {
	    set short_score \
		[string tolower \
		   [clock format $problem_time \
			  -format {%d%b%y}]]
	    set long_score $short_score/$submissions
	}

	if { $long_score == "" } {
	    error "`$scoreboard_solved_mode' is bad\
		   value for scoreboard_solved_mode"
	}
    }

    if {    $modifier == "n" \
         && (    $incorrect != 0
	      || $problem_time != "" ) } {
	set long_score "*$long_score"
	set short_score "*$short_score"
    }

    if { [string length $long_score] <= 9 } {
        return $long_score
    } else {
        return $short_score
    }
}
