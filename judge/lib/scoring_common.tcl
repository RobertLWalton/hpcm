# Common TCL Code for Scoring and Scoring Displays
#
# File:		scoring_common.tcl
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Mon Aug 27 05:12:49 EDT 2001
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2001/08/27 10:33:51 $
#   $RCSfile: scoring_common.tcl,v $
#   $Revision: 1.2 $
#
#
# Note: An earlier version of this code used to be in
# manualreply.

# Table of Contents
#
#	Including this Code
#	Scoring Data Base
#	Scoring Functions

# Including this Code
# --------- ---- ----


# Include this code in TCL program via:
#
#	set lib_directory \
#	    "[file dirname $argv0]/../lib"
#	source $lib_directory/judging_common.tcl
#	source $lib_directory/scoring_common.tcl
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

# Scoring Data Base
# ------- ---- ----

# The value of `instruction_array(type)' is the part of
# the scoring instructions related to a given type of
# difference.  For a difference type like `case', if
# that is present in the scoring instructions, the value
# of `instruction_array(case)' is just `case'.  For a
# difference type like `number', if `number 0.01 3.0' is
# present in the scoring instructions, for example, the
# value of `instruction_array(number)' is `number 0.01
# 3.0'.  All elements of instruction_array correspond
# to differences listed in the scoring instructions.
#
# `score_array' is computed from the first line of the
# .score file in a manner strictly analogous to the way
# `instruction_array' is computed from the scoring
# instructions.
#
# Note that the `number' type may appear in `instruc-
# tion_array' but not in `score_array'.  However, if
# `number' is in the scoring instructions but `integer'
# or `float' is not, then `integer' or `float' will be
# added to the `instruction_array' copying the absolute
# and relative differences from the `number' entry in
# that array.  So if the array has a `number' entry it
# will always have an `integer' and `float' entry.
#
# `proof_array' is computed from all the proofs in the
# .score file.  For each difference type, `proof_
# array(type)' is a list of all the proofs in the .score
# file for that type, where each proof is a list with
# the format:
#
#	sort-ID output-line test-line \
#		output-begin-column output-end-column \
#		test-begin-column test-end-column \
#		optional-absolute-difference \
#		optional-relative-difference
#
# Here the absolute and relative differences are includ-
# ed only for the types `integer' and `float'.  The
# sort-ID is a 12 digit number the first 6 digits of
# which are the output-line number and the last 6 digits
# of which are the test-line number.  The list stored
# in each `proof_array(type)' is sorted, thereby sorting
# the proofs in the list first by output-line number and
# second by test line number.


# Function to compute the `instruction_array' using
# the current value of `find_scoring_instructions'.
#
proc compute_instruction_array { }
{
    global instruction_array

    compute_scoring_array instruction_array \
    			  [find_scoring_instructions] \
			  "scoring instructions"

    if { [info exists instruction_array(number)] } {
	if { ! [info exists \
	             instruction_array(integer)] } {
	     set instruction_array(integer) \
	         $instruction_array(number)
	}
	if { ! [info exists \
	             instruction_array(float)] } {
	     set instruction_array(float) \
	         $instruction_array(number)
	}
    }
}

# Function to compute `score_array' and `proof_array'
# from the *.score file.  If a single argument is given,
# it is the *.score file name.  Otherwise, the file name
# is retrieved using `get_listed_files' and code in the
# `display_common.tcl' function package, which must be
# loaded in this case.  In this case there must be at
# most one *.score file in the list of file names ret-
# urned by `get_listed_files'.
#
proc compute_score_and_proof_arrays { args }
{
    global proof_array

    switch [llength $args] {
        { 0
	    set items [get_file_items {.*\.score}]

	    if { [llength $items] != 1 } {
		error "Too many *.score files"
	    }

	    set filename [lindex [lindex 1 $items] 2]
	}
	{ 1
	    set filename [lindex $args 0]
	}
	{ default
	    error "too many args to\
	           compute_score_and_proof_arrays"
	}
    }

    set score_ch [open $filename r]

    compute_scoring_array score_array [gets $score_ch] \
			  ".score file first line"

    foreach type [array names proof_array] {
        unset proof_array($type)
    }

    while { "yes" } {
        set line [gets $score_ch]
	if { [eof $score_ch] } break

	set work $line

	if { [llength $work] < 2 } {
	    error "too short proof line: $line"
	}
	set output_line [lindex $work 0]
	set test_line [lindex $work 1]
	set sort_id \
	    [format "%06d%06d" $output_line $test_line]
	set work [lrange $work 2 end]

	while { [llength $work] >= 4 } {
	    set output_begin_column [lindex $work 0]
	    set output_end_column   [lindex $work 1]
	    set test_begin_column   [lindex $work 2]
	    set test_end_column     [lindex $work 3]
	    set work [lrange 4 end]

	    while { [regexp {^[a-zA-Z]} \
	                    [lindex $work 0] } {
		set type [lindex $work 0]
		if { [lsearch -exact \
	              {number integer float} $type] \
		     >= 0 } {
		    if { [llength $work] < 3 } {
			error "too short proof line:\
			       $line"
		    }
		    set differences [lrange $work 1 2]
		    set work [lrange $work 3 end]
		} else {
		    set differences ""
		    set work [lrange $work 1 end]
		}

		set proof [list $sort_id \
				$output_line \
				$test_line \
				$output_begin_column \
				$output_end_column \
				$test_begin_column \
				$test_end_column]
		set proof [concat $proof $differences]
		lappend proof_array($type) $proof
	    }
	}
	if { [llength $work] != 0 } {
	    error "too short proof line: $line"
	}
    }
    close $score_ch

    foreach type [array names proof_array] {
        set proof_array($type) \
	    [lsort $proof_array($type)]
    }
}

# Function to do the common work of the above functions.
#
proc compute_scoring_array { array line name } {

    global instruction_array score_array

    foreach type [array names $array] {
        unset $array($name)
    }

    set state type
    foreach item $line {
	switch $state {
	    { type
		set $array($type) $item
		if { [lsearch -exact \
	              {number integer float} $item] \
		     >= 0 } {
		    set previous $item
		    set state first
		}
	    }
	    { first
	    	lappend $array($previous) $item
		set state second
	    }
	    { second
	    	lappend $array($previous) $item
		set state type
	    }
	}
    }
    if { $state != "type" } {
        error "Incomplete $name"
    }
}

# Scoring Functions
# ------- ---------

# Function that computes the .score file given the
# .out and .test files.  The scoring instructions and
# `difference_type_proof_limit' value from `hpcm_judg-
# ing.rc' are used to obtain an optimal set of proofs.
#
# The file names used are $basename.out, $basename.test,
# and $basename.score.
#
# `compute_instruction_array' must be called before
# this routine is called.
#
proc compute_score_file { basename } {

    global difference_type_proof_limit \
           instruction_array

    set limits "-limits $difference_type_proof_limit"

    foreach type [array names instruction_array] {
	set arguments $instruction_array($type)
	set limits \
	    [concat $limits \
	            [list -[lindex $arguments 0]] \
	            [range 1 end $arguments]]
        if { [lsearch -exact {number integer float} \
	                     $type] >= 0 } {
	    lappend limits $difference_type_proof_limit
	}
    }

    eval [list exec scorediff] $limits \
         [list $output_file $test_file > $score_file]
}

# Compute score based on the scoring databases computed
# by `compute_instruction_array' and `compute_score_and_
# proof_arrays'.  These functions must be called first.
# Return the score.
#
proc compute_score { } {

    global instruction_array score_array

    set score ""

    foreach type [array names score_array] {

        # If instruction for the difference type exists,
	# continue loop, unless difference type is
	# integer or float and instruction absolute or
	# relative difference is less than score
	# absolute or relative difference.

        if { [info exists instruction_array($type)] } {

	    # If integer or float type and instruction
	    # absolute or relative difference is less
	    # than score difference, return `Incorrect
	    # Output'.

	    if { [lsearch -exact {integer float} \
	                         $type] >= 0 } {
		set score score_array($type)
		set instruction instruction_array($type)
		if {    [lindex $score 1] \
		     <= [lindex $instruction 1] \
		     && \
		        [lindex $score 2] \
		     <= [lindex $instruction 2] } {
		    continue
		}
	    } else {
		continue
	    }
	}

	switch $type {

	    integer -
	    float -
	    eof2 -
	    nonblank {
		set score "Incorrect Output"
		break
	    }

	    eof1 {
		set score "Incomplete Output"
	    }

	    decimal -
	    exponent -
	    sign -
	    case -
	    column -
	    whitespace -
	    beginspace -
	    endspace -
	    linespace -
	    spacebreak -
	    linebreak {
	        if { $score == "" } {
		    set score "Formatting Error" 
		}
	    }

	    none { }
	    default {
		error "Unknown scorediff result: $type" 
	    }
	}
    }

    if { $score == "" } {
        set score "Completely Correct"
    }

    return $score
}
