# Common TCL Code for Scoring and Scoring Displays
#
# File:		scoring_common.tcl
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sat Sep  8 07:43:05 EDT 2001
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2001/09/08 13:03:11 $
#   $RCSfile: scoring_common.tcl,v $
#   $Revision: 1.14 $
#
#
# Note: An earlier version of this code used to be in
# manualreply.

# Table of Contents
#
#	Including this Code
#	Scoring Data Base
#	Scoring Functions
#	Proof Display

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
# tion_array', but not in `score_array'.  However, if
# `number' is in the scoring instructions, but `integer'
# or `float' is not, then `integer' or `float' will be
# added to the `instruction_array' copying the absolute
# and relative differences from the `number' entry in
# that array.  So if the array has a `number' entry it
# will always have an `integer' and `float' entry.
#
# Also if `number' is in the scoring instructions then
# `decimal', `exponent', and `sign' are automatically
# added to the instruction array.
#
# The `space' type may appear in `instruction_array' but
# not in the `score_array'.  When `space' is in the
# scoring instructions, `whitespace', `beginspace',
# `linespace', and `endspace' are automatically added to
# the instruction array.
#
# The `words-are-format' type may appear in `instruc-
# tion_array' but not in `score_array'.  The presence of
# this type just causes `word' and `word-eof2' differ-
# ences to signal `Formatting Error' instead of
# `Incorrect Output'.
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
# second by test-line number.
#
# The following global variable is the file name of the
# of the score file used to load the proof_array.
#
set score_filename ""
#
# The following is a list of the fake `instruction_
# array' difference types that cannot be in `score_
# array' and therefore should not be passed as options
# to the scorediff program.
#
set fake_instruction_types {
    number space words-are-format
}

# Function to compute the `instruction_array' using
# the current value of `find_scoring_instructions'.
#
proc compute_instruction_array { } {

    global instruction_array

    compute_scoring_array instruction_array \
    			  [find_scoring_instructions] \
			  "scoring instructions"

    if { [info exists instruction_array(number)] } {
        set differences \
	    [lrange $instruction_array(number) 1 end]
	if { ! [info exists \
	             instruction_array(integer)] } {
	     set instruction_array(integer) \
	         [concat integer $differences]
	}
	if { ! [info exists \
	             instruction_array(float)] } {
	     set instruction_array(float) \
	         [concat float $differences]
	}
	set instruction_array(decimal) decimal
	set instruction_array(exponent) exponent
	set instruction_array(sign) sign
    }

    if { [info exists instruction_array(space)] } {
	set instruction_array(whitespace) whitespace
	set instruction_array(beginspace) beginspace
	set instruction_array(linespace) linespace
	set instruction_array(endspace) endspace
    }
}

# Function to compute `score_array' and `proof_array'
# from the *.score file.  If a single argument is given,
# it is the *.score file name.  Otherwise, the file name
# is retrieved using `get_listed_files' and code in the
# `display_common.tcl' function package, which must be
# loaded in this case.  In this case there must be
# exactly one *.score file in the list of file names
# returned by `get_listed_files'.
#
proc compute_score_and_proof_arrays { args } {

    global proof_array score_filename score_array

    switch [llength $args] {
        0 {
	    set items [get_file_items {.*\.score}]

	    if { [llength $items] == 0 } {
		error "No *.score files"
	    } elseif { [llength $items] > 1 } {
		error "Too many *.score files"
	    }

	    set filename [lindex [lindex $items 0] 2]
	}
	1 {
	    set filename [lindex $args 0]
	}
	default {
	    error "too many args to\
	           compute_score_and_proof_arrays"
	}
    }

    set score_ch [open $filename r]
    set score_filename $filename

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

	if { [catch { expr { $output_line + \
	                     $test_line } }] } {
	    error "bad line numbers: $line"
	}

	set sort_id \
	    [format {%06d%06d} $output_line $test_line]
	set work [lrange $work 2 end]

	while { [llength $work] >= 4 } {
	    set output_begin_column [lindex $work 0]
	    set output_end_column   [lindex $work 1]
	    set test_begin_column   [lindex $work 2]
	    set test_end_column     [lindex $work 3]
	    set work [lrange $work 4 end]

	    if { [catch { \
	             expr { $output_begin_column + \
		            $output_end_column + \
		            $test_begin_column + \
			    $test_end_column } }] } {
		error "bad column numbers: $line"
	    }

	    set proof [list $sort_id \
			    $output_line \
			    $test_line \
			    $output_begin_column \
			    $output_end_column \
			    $test_begin_column \
			    $test_end_column]

	    while { [regexp {^[a-zA-Z]} \
	                    [lindex $work 0]] } {
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

		lappend proof_array($type) \
		        [concat $proof $differences]
	    }
	}

	if { [llength $work] != 0 } {
	    error "too long proof line: $line"
	}
    }
    close $score_ch

    foreach type [array names proof_array] {
        set proof_array($type) \
	    [lsort $proof_array($type)]
    }
}

# Function to do the common work of the above functions.
# Array is the array to compute, line is the input line,
# and name is a description of the line for error mes-
# sages.
#
proc compute_scoring_array { xxx_array line name } {

    upvar $xxx_array array

    foreach type [array names array] {
        unset array($type)
    }

    set state type
    foreach item $line {
	switch $state {
	    type {
		set array($item) $item
		if { [lsearch -exact \
	              {number integer float} $item] \
		     >= 0 } {
		    set previous $item
		    set state first
		}
	    }
	    first {
	    	lappend array($previous) $item
		set state second
	    }
	    second {
	    	lappend array($previous) $item
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
# the `difference_type_proof_limit' value from `hpcm_
# judging.rc' are used to obtain an optimal set of
# proofs.
#
# The file names used are $basename.out, $basename.test,
# and $basename.score.
#
# `compute_instruction_array' must be called before
# this routine is called.
#
proc compute_score_file { basename } {

    global difference_type_proof_limit \
           instruction_array fake_instruction_types

    set limits "-all $difference_type_proof_limit"

    foreach type [array names instruction_array] {

        if { [lsearch $fake_instruction_types \
	              $type] >= 0 } continue

	set arguments $instruction_array($type)
	set limits \
	    [concat $limits \
	            [list -[lindex $arguments 0]] \
	            [lrange $arguments 1 end]]
        if { [lsearch -exact {number integer float} \
	                     $type] >= 0 } {
	    lappend limits $difference_type_proof_limit
	}
    }

    eval [list exec scorediff] $limits \
         [list $basename.out $basename.test \
	       > $basename.score]
}

# Computes score based on the scoring databases computed
# by `compute_instruction_array' and `compute_score_and_
# proof_arrays'.  These functions must be called first.
# Returns the score.  Also sets the variables:
#
#	incorrect_output_types
#	incomplete_output_types
#	formatting_error_types
#
# to lists of the differences which support these
# scores; e.g., incorrect_output_types might include
# `word' if that is not just a formatting error, or
# `integer' if the limits on an integer difference
# were exceeded.
#
proc compute_score { } {

    global instruction_array score_array \
    	   incorrect_output_types \
    	   incomplete_output_types \
    	   formatting_error_types

    set incorrect_output_types ""
    set incomplete_output_types ""
    set formatting_error_types ""

    foreach type [array names score_array] {

        # If instruction for the difference type exists,
	# continue loop, unless difference type is
	# integer or float and instruction absolute or
	# relative difference is less than score
	# absolute or relative difference.

        if { [info exists instruction_array($type)] } {

	    if { [lsearch -exact {integer float} \
	                         $type] >= 0 } {
		set s $score_array($type)
		set i $instruction_array($type)
		if { [lindex $s 1] <= [lindex $i 1] \
		     && \
		     [lindex $s 2] <= [lindex $i 2] } {
		    continue
		}
	    } else continue
	}

	switch $type {

	    integer -
	    float -
	    infinity -
	    integer-eof2 -
	    float-eof2 {
		lappend incorrect_output_types $type
	    }

	    word-eof2 -
	    word {
		set waf words-are-format
		if { [info exists \
		           instruction_array($waf)] } {
		    lappend formatting_error_types $type
		} else {
		    lappend incorrect_output_types $type
		}
	    }

	    word-eof1 -
	    integer-eof1 -
	    float-eof1 {
		lappend incomplete_output_types $type
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
		lappend formatting_error_types $type
	    }

	    none { }
	    default {
		error "Unknown scorediff result: $type" 
	    }
	}
    }


    if { $incorrect_output_types != "" } {
    	return "Incorrect Output"
    } elseif { $incomplete_output_types != "" } {
    	return "Incomplete Output"
    } elseif { $formatting_error_types != "" } {
    	return "Formatting Error"
    } else {
        return "Completely Correct"
    }
}

# Proof Display
# ----- -------

# The proof display code displays one proof at a time.
# There is a current difference type, and for each
# difference type, there is a current proof.  The
# current proof of the current difference type can be
# displayed.
#
set current_type ""
#
# If the current_type is "", it should be taken to be
# the first element of [concat $incorrect_output_types
# $incomplete_output_types $formatting_error_types].
#
# current_proof_array(type) is the index+1 of the
# current proof in proof_array(type).  If current_
# proof_array(type) is unset, it should be taken as if
# it were set to 1.

# Function to set the current_type and its current_
# proof_array element.  Takes a list of arguments which
# are processed in order.  An alphabetic argument with
# at least two letters is matched to the beginning of a
# difference type name that has some proofs in proof_
# array, and switches to that difference type.  A number
# N switches to the N'th proof of the current_type.  An
# `n' goes to the next proof of the current_type; a `p'
# to the previous proof of the current type.  After all
# arguments are processed, checks that the result
# designates an existing proof.
#
# If called with no arguments, merely ensures that
# current_type and its current_proof_array element are
# set if they can be set without error.
#
# If there is no error, window_error is set to "" and
# `yes' is returned.  Otherwise window_error is set to
# an error description, `no' is returned, and no change
# is made in current_type or current_proof_array.
#
proc get_proof { args } {

    global window_error proof_array \
           current_type current_proof_array \
    	   incorrect_output_types \
           incomplete_output_types \
	   formatting_error_types

    set type $current_type

    if { $type == "" } {
        set type [lindex [concat \
			    $incorrect_output_types \
			    $incomplete_output_types \
			    $formatting_error_types] 0]
    }

    if { $type == "" } {
        set type [lindex [array names proof_array] 0]
    }

    if { [info exists current_proof_array($type)] } {
        set n $current_proof_array($type)
    } else {
        set n 1
    }

    foreach arg $args {

        if { [regexp {^[a-z][a-z]} $arg] } {
	    set found ""
	    foreach t [array names proof_array] {
	        if { $arg == $t } {
		    set found $t
		    break
	        } elseif { [regexp "^$arg" $t] } {
		    lappend found $t
		}
	    }
	    if { [llength $found] == 0 } {
		set window_error \
		    "There are no proofs whose\
		     difference type begins with\
		     `$arg'"
		return no
	    } elseif { [llength $found] > 1 } {
		set window_error \
		    "There is more than one difference\
		     type with proofs whose name begins\
		     with `$arg'"
		return no
	    } else {
	        set type [lindex $found 0]

		if { [info exists \
		      current_proof_array($type)] } {
		    set n $current_proof_array($type)
		} else {
		    set n 1
		}
	    }
	} elseif { [regexp {^[0-9]+$} $arg] } {
	    set n $arg
	} else {
	    switch -exact $arg {
	        n { incr n }
		p { incr n -1 }
		default {
		    set window_error \
			"Cannot understand `$arg'."
		    return no
		}
	    }
	}
    }

    if { $type == "" } {
        set window_error \
	    "There are no displayable proofs."
	return no
    }

    if { ! [info exists proof_array($type)] \
         || [llength $proof_array($type)] \
	    == 0 } {
        set window_error \
	    "There are no displayable proofs of type\
	     `$type'"
	return no
    }

    set max [llength $proof_array($type)]

    if { $n < 1 || $max < $n } {
        set window_error \
	    "There is no $n'th proof of type `$type'"
	return no
    }

    set current_type $type
    set current_proof_array($type) $n
}

# Display the current proof.  The following functions
# must have been called first:
#
#	compute_instruction_array
#	compute_score_and_proof_arrays
#	compute_score
#
# The files referenced in the proof are $basename.out
# and $basename.test.
#
# If there is no error, window_error is set to "" and
# `yes' is returned.  Otherwise window_error is set to
# an error description and `no' is returned.
#
# If there is no error, sets the `last_display'
# variable to `proof'.
#
proc set_proof_display { } {

    global current_type current_proof_array \
    	   proof_array score_filename \
	   window_height window_info_height \
	   window_error \
	   last_display

    if { [get_proof] == "no" } {
    	return no
    }

    set proofs $proof_array($current_type)

    set i $current_proof_array($current_type)

    set proof [lindex $proofs [expr { $i - 1 }]]

    set oline [lindex $proof 1]
    set tline [lindex $proof 2]
    set oc1   [lindex $proof 3]
    set oc2   [lindex $proof 4]
    set tc1   [lindex $proof 5]
    set tc2   [lindex $proof 6]
    set oh    [list [list $oline $oc1 $oc2]]
    set th    [list [list $tline $tc1 $tc2]]
    set desc  [concat $current_type \
    		      [lrange $proof 7 end]]
    set desc  "\[$i\]  $desc"

    # L is the number of lines of each file that are to
    # be displayed before and after the principal
    # display for that file.  The 8 includes the prompt,
    # error line, blank line, 3 bar lines, and the 2
    # principal display lines.
    #
    set L [expr { ( $window_height \
                        - $window_info_height - 8 ) \
		      / 4 }]

    set omin [expr $oline - $L]
    if { $omin < 1 } { set omin 1 }
    set tmin [expr $tline - $L]
    if { $tmin < 1 } { set tmin 1 }

    set basename [file rootname $score_filename]
    set_window_display \
        "[compute_file_display $basename.out \
	                       out_file_array \
			       $omin \
			       [expr { $oline + $L }] \
			       $oh \
        ][compute_file_display $basename.test \
	                       test_file_array \
			       $tmin \
			       [expr { $tline + $L }] \
			       $th \
	][bar_with_text $desc]"

    set last_display proof
}

# Display summary information about existing proofs in
# the info part of the display.  The following functions
# must have been called first:
#
#	compute_instruction_array
#	compute_score_and_proof_arrays
#	compute_score
#
# Extra are lines added to the end of the info.
#
proc set_proof_info { extra } {

    global incorrect_output_types \
           incomplete_output_types \
	   formatting_error_types \
	   proof_array score_array \
	   window_info_height

    set error_types \
        [concat $incorrect_output_types \
	        $incomplete_output_types \
	        $formatting_error_types]
    set non_error_types ""
    foreach type [array names proof_array] {
        if { [lsearch -exact $error_types $type] < 0 } {
	    lappend non_error_types
	}
    }

    set info ""
    set lines 0

    foreach x {Incorrect_Output Incomplete_Output \
                                Formatting_Error \
				Non_Error } {
	set types [set [string tolower $x]_types]
	if { $types == "" } continue
	set info "$info[split $x "_"]:"
	foreach t $types {
	    set info "$info $score_array($t)"
	}
	set info "$info\n"
	incr lines
    }
    set info "$info    n = next proof    p\
                         = previous proof$extra"
    incr lines 2
    incr lines [llength [split $extra "\n"]]

    set window_info_height $lines
    set_window_info $info
}
