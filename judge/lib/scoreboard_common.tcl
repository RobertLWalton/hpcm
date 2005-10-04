# Functions to read scorefinder output into an
# internal database and help build a scoreboard.
#
# File:		scoreboard_common.tcl
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Tue Oct  4 10:23:13 EDT 2005
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2005/10/04 14:49:05 $
#   $RCSfile: scoreboard_common.tcl,v $
#   $Revision: 1.49 $
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
# except `s' start time code items are always first
# (they are added during pruning).
#
# After the scoreboard_array is `pruned' (see below),
# an item is added at the beginning of each array ele-
# ment with code `s' that contains the start time of
# the problem.  If the start time is unknown, because
# the scoreboard_start_time parameter is "", the time
# in the code `s' item will be "".
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
#	item is before the start time.
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
#	{ sort_code number_of_submissions
#	  problems_correct total_score modifier
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
# global variable.  This last list is sorted.
#
# If scoreboard_start_time is "", the total_score is "".
# If scoreboard_display_correct is `no', the problems_
# correct and total_score will both be "".
#
# The format of the problem_scores and sort_code depend
# upon the settings of various scoreboard parameters.
# The problem_scores will not be more than 9 characters
# long except in cases too rare to be given serious con-
# sideration.  The scoreboard_list is sorted in the
# order that submitters are to be displayed on the
# scoreboard.
#
# If scoreboard_use_qualifiers is `no', scoreboard_
# display_correct and scoreboard_display_incorrect are
# both `yes', and scoreboard_start_time is NOT "", the
# sort code is
#
#		     cccc.ttttttttt.ssss
#
# where cccc is 10**4 - 1 - the number of correct
# problems in four digits with leading zeros, ttttttttt
# is the total_score, actually the total time, in 9
# digits with leading zeros, and ssss is 10**4 - 1 - the
# total number of submissions, in 4 digits with leading
# zeros.
#
# If scoreboard_use_qualifiers is `yes', and scoreboard_
# display_correct and scoreboard_display_incorrect are
# both `yes', the sort code is
#
#		     cccc.qqqqqqqqq.ssss
#
# where cccc is 10**4 - 1 - the number of correct
# problems in four digits with leading zeros, qqqqqqqqq
# is 10**9 - 1 - 100 * the qualifier score, in 9 digits
# with leading zeros, and ssss is 10**4 - 1 - the total
# number of submissions, in 4 digits with leading zeros.
# This reflects the fact that higher time scores are
# worse but higher qualifier scores are better.
#
# The scoreboard_list is sorted in INCREASING order.
# Hence submitters with more correct submissions will be
# first.  Those with more submissions will be first,
# other things being equal, which seems appropriate for
# submitters with no correct submissions.
#
# The sort code is changed according to the setting of
# three scoreboard parameters as follows:
#
#    SUQ = scoreboard_uses_qualifier
#    SDC = scoreboad_display_correct
#    SST = $scoreboard_start_time != ""
#    SDI = scoreboad_display_incorrect
#    
#	SDC/SUQ/SST/SDI	    SORT_CODE
#
#	no /---/---/---	    submitters_name
#
#	yes/yes/---/yes     cccc.qqqqqqqqq.ssss
#
#	yes/yes/---/no      cccc.qqqqqqqqq
#			    (.ssss is omitted)
#
#	yes/no /no /---	    cccc.submitters_name
#
#	yes/no /yes/yes	    cccc.ttttttttt.ssss
#			    (as in text above)
#
#	yes/no /yes/no	    cccc.ttttttttt
#			    (.ssss is omitted)
#
# Here qqqqqqqqq is the total score computed from the
# submit qualifier, instead of from times.
#
# Note that if scoreboard_display_incorrect is `yes',
# problems or submitters for which there have been only
# incorrect submissions will appear in the problem or
# submitter list, but if scoreboard_display_incorrect is
# `no', they will not.
#
set scoreboard_list ""
set scoreboard_problem_list ""

# Function to compute scoreboard_list and scoreboard_
# problem_list from pruned scoreboard_array.
#
proc compute_scoreboard_list {} {

    global scoreboard_array scoreboard_list \
           scoreboard_problem_list scoreboard_penalty \
	   scoreboard_display_correct \
	   scoreboard_display_incorrect \
	   scoreboard_start_time \
	   scoreboard_use_qualifiers

    if { ! [lcontain {yes no} \
    		     $scoreboard_display_correct] } {
        error "scoreboard_display_correct is not yes \
	       or no"
    }
    if { ! [lcontain {yes no} \
    		     $scoreboard_display_incorrect] } {
        error "scoreboard_display_incorrect is not yes \
	       or no"
    }

    # Compute lists of submitters and problems.
    #
    # If we are displaying count of incorrect problems,
    # mark a submitter or problem present if there is
    # any submission for the submitter or problem, else
    # mark a submitter or problem present only there is
    # a correct submission for the submitter or problem.
    #
    if { $scoreboard_display_incorrect } {
        set present_regexp {.[ic]..}
    } else {
        set present_regexp {.c..}
    }

    foreach sap [array names scoreboard_array] {
	regexp {^([^/]*)/([^/]*)$} $sap forget \
	       submitter problem

	set code \
	    [lindex [lindex $scoreboard_array($sap) \
	                    end] \
		    1]
	if { [regexp $present_regexp $code] } {
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
    set max_total_score 999999999
    foreach submitter $submitters {

	# Compute total score, number of problems
	# correct, modifier (f or n), total number of
	# submissions, qualifier_score.
	#
	set time_score 0
	set qualifier_score 0.0

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
	    # number of incorrect submissions, and
	    # problem modifier.
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
	    set problem_f_qualified	0
	    set problem_s_qualified	0
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

			set problem_increment \
			    [expr { \
			       $problem_time \
			       + $scoreboard_penalty \
				 * $problem_incorrect }]
			if { [expr { \
				$max_total_score \
				- \
				$problem_increment }] \
			     < $time_score } {
			    set time_score \
			        $max_total_score
			} else {
			    incr time_score \
				 $problem_increment
			}
		    }
		    incr problems_correct
		    incr submissions

		} elseif { [regexp {.i..} $code] } {

		    # Score is incorrect.

		    set qualifier [string index $code 3]
		    if { ! [lcontain {i o f s x} \
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
	    # list.
	    #
	    lappend problem_scores \
	            [format_problem_score \
	    		 $problem_time \
			 $problem_incorrect \
			 $problem_modifier \
			 qualifier_score \
			 [list $problem_i_qualified \
			       $problem_o_qualified \
			       $problem_f_qualified \
			       $problem_s_qualified \
			       $problem_x_qualified]]
	}

	# Compute sort_code and mark problems_correct
	# and total_score missing when appropriate.
	#
	set cccc \
	    [format {%04d} \
		    [expr { 9999 - $problems_correct }]]
	set ssss \
	    [format {%04d} \
		    [expr { 9999 - $submissions }]]
	if { ! $scoreboard_display_correct } {
	    set sort_code $submitter
	    set problems_correct ""
	    set total_score ""
	} elseif { $scoreboard_use_qualifiers } {
	    set qqqqqqqqq \
		[format {%09d} \
		        [expr { 999999999 \
			        - round \
				  (   100 \
				    * $qualifier_score \
				  ) }]]
	    if { $scoreboard_display_incorrect } {
		set sort_code $cccc.$qqqqqqqqq.$ssss
	    } else {
		set sort_code $cccc.$qqqqqqqqq
	    }
	    set total_score $qualifer_score
	} elseif { $scoreboard_start_time == "" } {
		set sort_code $cccc.$submitter
		set total_score ""
	} else {
	    set ttttttttt \
		[format {%09d} $time_score ]
	    if { $scoreboard_display_incorrect } {
		set sort_code $cccc.$ttttttttt.$ssss
	    } else {
		set sort_code $cccc.$ttttttttt
	    }
	    set total_score $time_score
	}

	# Add score list item to score list.
	#
	lappend scoreboard_list \
		[concat [list $sort_code $submissions \
		              $problems_correct \
			      $total_score \
		              $modifier $submitter ] \
		        $problem_scores]
    }

    set scoreboard_list [lsort $scoreboard_list]
}


# Scoreboard Functions
# ---------- ---------

# Return the printable score of a problem, given the
# problem time (which is "" if the problem was never
# correct), the number of incorrect submissions, and
# the modifier which is "n" if unreviewed non-final
# scores exist.  The problem time is the date and time
# (as per [clock seconds]) of the correct submission if
# scoreboard_start_time is "", and is otherwise the 
# number of seconds between the problem start time and
# the correct submission.  The arguments involving
# qualified submissions are explained below.
#
# The value returned has 9 or fewer characters as long
# as there are at most 98 incorrect submissions, or
# there is no `*' prefix (see below) and at most 998
# incorrect submissions, or scoreboard_display_incorrect
# is `no', or scoreboard_use_qualifiers is yes and there
# are not too many kinds of qualifiers being used.
#
# If the problem time is "" indicating the problem is
# not correct, the problem score is `......' if there
# are no submissions or if scoreboard_display_incorrect
# is `no', and is otherwise `..../N', where N is the
# number of incorrect submissions.
#
# Otherwise if scoreboard_use_qualifiers is `yes', the
# the score for the problem is computed as a floating
# point number, as accurately as possible, and is added
# to the qualified_score whose name must be given as an
# argument.  In this case the qualified_submissions
# argument must be given as a list of the format
#
#		I O F S X
#
# where I, O, F, S, X represent the following:
#
#    I  number `in' qualified incorrect submissions
#    O  number `inout' qualified incorrect submissions
#    F  number `first' qualified incorrect submissions
#    S  number `summary' qualified incorrect submissions
#    X  number unqualified incorrect submissions
#
# The order of the list given above matches the order
# of scoreboard_qualifier_factors: see hpcm_judging.rc.
#
# If scoreboard_use_qualifiers and scoreboard_display_
# incorrect are both `yes', the problem score has the
# form
#
#		IiOoFfSsX
#
# where I, O, F, S, X are as above, and if any are 0,
# the it and any lower case letter following it are
# omitted (instead of 2i1o0f0s2 it is 2i1o2).
#
# If, however, scoreboard_display_incorrect is `no', the
# problem score is just the problem score proper,
# rounded to the nearest integer.
#
# If scoreboard_use_qualified is `no' and scoreboard_
# start_time is "", the problem time is the date and
# time as per [clock seconds], and this is encoded in
# the problem score as:
#
#	ddmmmyy
#
# where dd is the day of the month, mmm the month in 3
# letters, and yy the last two digits of the year.  If
# there would be more than 9 characters in the printable
# score, the year is omitted.
#
# If scoreboard_use_qualified is `no' and scoreboard_
# start_time is NOT "", the problem time an elapsed
# time, and is encode in the problem score as:
#
#	MM:SSs		s = denotes seconds
#	HH:MMm		m = denotes minutes
#	DD:HHh		h = denotes hours
#	DDDDDd		d = denotes days
#
# If the modifier is "n" and either there was a correct
# submission or the scoreboard_display_incorrect is
# `yes', "*" is prefixed to the score to indicate the
# score is subject to change by manual review.
#
proc format_problem_score { time incorrect modifier \
    { qualifier_score_name ""} \
    { qualified_submissions "" } } {

    global scoreboard_start_time \
           scoreboard_display_incorrect \
	   scoreboard_use_qualifiers \
	   scoreboard_factors

    # Compute:
    #
    #	long_score	use if short enough
    #	short_score	use otherwise

    if { $time == "" } {
        if {    $scoreboard_display_incorrect \
	     && $incorrect != 0 } {
	    set long_score ..../$incorrect
	} else {
	    set long_score ......
	}
	set short_score $long_score
    } elseif { $scoreboard_use_qualifiers } {
        upvar $qualifier_score_name qualifier_score
	set score 100.0
	set long_score ""
	set i 0
	foreach c $qualified_submissions {
	    set factor $scoreboard_qualifier_factors($i)
	    set postfix [lindex {i o f s ""} $i]
	    incr i
	    if { $c == 0 } continue
	    set score \
	        [expr { $score * pow ($factor, $c) }]
	    if { $scoreboard_display_incorrect } {
	        set long_score \
		    "$long_score$c$postfix"
	    }
	}
	if {    ! $scoreboard_display_incorrect \
	     || $incorrect == 0 } {
	    set long_score [expr { round($score) }]
	}
	set short_score $long_score
	set qualifier_score \
	    [expr $qualifier_score + $score]

    } elseif { $scoreboard_start_time != "" } {

	set MM [expr { $time / 60 }]
	set SS [expr { $time - 60 * $MM } ] 
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

	if { $scoreboard_display_incorrect } {
	    incr incorrect 1
	    set long_score $long_score$incorrect
	}
	set short_score $long_score

    } else {

        # scoreboard_start_time == ""

        set long_score \
	    [string tolower \
	       [clock format $time -format {%d%b%y}]]
        set short_score \
	    [string tolower \
	       [clock format $time -format {%d%b}]]

	if { $scoreboard_display_incorrect } {
	    incr incorrect 1
	    set long_score $long_score/$incorrect
	    set short_score $short_score/$incorrect
	}
    }

    if { $modifier == "n" } {
	if {    $scoreboard_display_incorrect \
	     || $time != "" } {
	    set long_score *$long_score
	    set short_score *$short_score
	}
    }

    if { [string length $long_score] <= 9 } {
        return $long_score
    } else {
        return $short_score
    }
}
