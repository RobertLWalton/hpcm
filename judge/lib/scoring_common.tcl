# Common TCL Code for Scoring and Scoring Displays
#
# File:		scoring_common.tcl
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sat Mar  9 09:35:57 EST 2002
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/03/09 15:09:22 $
#   $RCSfile: scoring_common.tcl,v $
#   $Revision: 1.27 $
#
#
# Note: An earlier version of this code used to be in
# manualreply.

# Table of Contents
#
#	Including this Code
#	Scoring Data Base
#	Scoring Functions
#	Scoring Reply Functions
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
#	... terminates with `exit 0', `exit 1', or
#	... `error ...'
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
# this type just causes `word', `word-eof2', and `word-
# eof1' differences to signal `Formatting Error' instead
# of `Incorrect Output' or `Incomplete Output'.
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
#		difference-type-name \
#		optional-absolute-difference \
#		optional-relative-difference
#
# Here the absolute and relative differences are includ-
# ed only for the types `integer' and `float'.  The
# sort-ID is an 18 digit number the first 6 digits of
# which are the output-line number, the next 3 digits of
# which are the output-column number, the next 6 digits
# of which are the test-line number, and the last 3
# digits of which are the test-column number.  The list
# stored in each `proof_array(type)' is sorted, thereby
# sorting the proofs in the list first by output-line
# number, then by output-column number, etc.
#
# The following global variable is the file name of the
# of the score file used to load the proof_array.
#
set score_filename ""
#
# The following is a list of the scoring instruction
# difference types that should not be passed as options
# to the scorediff program, and are not returned in the
# first line of the scorediff output or stored in
# `score_array'.
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

    if { [array exists proof_array] } {
        unset proof_array
    }

    while { "yes" } {
        set line [gets $score_ch]
	if { [eof $score_ch] } break

	set work $line

	if { [catch { llength $work }] } {
	    error "proof line is not a TCL\
	           list:\n  $line"
	}
	if { [llength $work] < 2 } {
	    error "too short proof line: $line"
	}
	set output_line [lindex $work 0]
	set test_line [lindex $work 1]

	if { [catch { expr { $output_line + \
	                     $test_line } }] } {
	    error "bad line numbers: $line"
	}

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

	    set sort_id \
		[format {%06d%03d%06d%03d} \
		        $output_line \
			$output_begin_column \
			$test_line \
			$test_begin_column]

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
		if { [lcontain {number integer float} \
		               $type] } {
		    if { [llength $work] < 3 } {
			error "too short proof line:\
			       $line"
		    }
		    set difference [lrange $work 0 2]
		    set work [lrange $work 3 end]
		} else {
		    set difference [list $type]
		    set work [lrange $work 1 end]
		}

		lappend proof_array($type) \
		        [concat $proof $difference]
	    }
	}

	if { [llength $work] != 0 } {
	    error "extra stuff at end of proof\
	           line:\n  $line"
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

    if { [catch {llength $line}] } {
        error "scoring instructions or scorediff output\
	       first line is not a TCL list:\n $line"
    }

    upvar $xxx_array array

    if { [array exists array] } {
        unset array
    }

    set state type
    foreach item $line {
	switch $state {
	    type {
		set array($item) $item
		if { [lcontain {number integer float} \
		               $item] } {
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

# Function that computes the .score file given the .out
# and .test files (or .fout and .ftest files).  The
# scoring instructions and the `difference_type_proof_
# limit' value from `hpcm_judging.rc' are used to obtain
# an optimal set of proofs.  The scorediff program is
# called by `scorediff outfile testfile > scorefile'.
#
# `compute_instruction_array' must be called before
# this routine is called.
#
proc compute_score_file { outfile testfile scorefile } {

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
        if { [lcontain {number integer float} $type] } {
	    lappend limits $difference_type_proof_limit
	}
    }

    eval [list exec scorediff] $limits \
         [list $outfile $testfile > $scorefile]
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
# to lists of difference types which support these
# scores; e.g., incorrect_output_types might include
# `word' if that is not just a formatting error, or
# `integer' if the limits on an integer difference
# were exceeded.
#
# Returns score, which is:
#
#    Incorrect Output	if any incorrect output
#			difference types are in
#			score_array, but are not
#			disabled by instruction_array
#			(e.g. by allowed number
#			differences)
#
#			i.e. if incorrect_output_types
#			is not empty
# else
#
#    Incomplete Output	if any incomplete output
#			difference types are in
#			score_array
#
#			i.e. if incomplete_output_types
#			is not empty
#
# else
#
#    Formatting Error	if any formatting error
#			difference types are in
#			score_array, but are not
#			disabled by being in
#			instruction_array
#
#			i.e. if formatting_error_types
#			is not empty
#
# else
#
#    Completely Correct
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

	    if { [lcontain {integer float} $type] } {
		set s $score_array($type)
		set i $instruction_array($type)
		if { [lindex $s 1] <= [lindex $i 1] \
		     && \
		     [lindex $s 2] <= [lindex $i 2] } {
		    continue
		}
	    } else continue
	}

	set waf \
	    [info exists \
	          instruction_array(words-are-format)]
	switch -- $type {

	    integer -
	    float -
	    infinity -
	    integer-eof2 -
	    float-eof2 {
		lappend incorrect_output_types $type
	    }

	    word-eof2 -
	    word {
		if { $waf } {
		    lappend formatting_error_types $type
		} else {
		    lappend incorrect_output_types $type
		}
	    }

	    word-eof1 {
		if { $waf } {
		    lappend formatting_error_types $type
		} else {
		    lappend incomplete_output_types \
		    	    $type
		}
	    }

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

# Scoring Reply Functions
# ------- ----- ---------

# Function to process $response_instructions_file value
# and produce a $reply_file+ file by calling compose_
# reply.  Documentation of the $response_instructions_
# file value is found in hpcm_judging.rc with the
# default_response_instructions global variable.  The
# value of this global variable is used if the
# $response_instructions_file does not exist, or is
# added to the end of the value of that file if the file
# does exist.
#
# The compose_reply_options are a list of options,
# such as -cc and -error, that are passed to compose_
# reply.  The response instruction command CC will
# also force a -cc option to compose_reply.
#
# The global variables scoring_mode, auto_score,
# manual_score, and proposed_score must be set.  The
# latter two can be set to `None' if unused.
#
# The global variables `submitted_problem' and
# `submitted_extension' must be set to the problem name
# and extension.
#
# This function returns a list of commands whose execu-
# tion merely serve to specify options for disposition
# of $reply_file+.  Specifically, any FINAL, NOT-FINAL,
# NO-REPLY, or EDIT commands that are executed are
# merely returned in this list, and have no direct
# effect on the contents of $reply_file+.  This list
# is guarenteed to have exactly one of FINAL, NOT-
# FINAL, or NO-REPLY, and may be passed to send_
# response.
#
proc compose_response { { compose_reply_options "" } } {

    global default_response_instructions \
           response_instructions_file

    if { [catch { llength \
		      $default_response_instructions \
		      		}] } {
	error "default_response_instructions value is\
	       not a TCL list"
    }

    # Compute response instructions.
    #
    if { [file exists $response_instructions_file] } {
        set response_instructions \
            [read_entire_file \
	         $response_instructions_file]
	if { [catch { llength \
			  $response_instructions }] } {
	    error "$response_instructions_file file\
		   value is not a TCL list"
	}
        set response_instructions \
            [concat $response_instructions \
	    	    $default_response_instructions]
    } else {
	set response_instructions \
	    $default_response_instructions
    }

    # Execute first pass over response instructions
    # and compute a list of action response instructions
    # to be executed in `commands'.
    #
    set commands ""
    parse_block $response_instructions commands

    # Execute second pass on action response instructions
    # and return those whose action is to be performed by
    # our caller (i.e., are instructions for disposal of
    # $reply_file+).
    #
    return [execute_response_commands \
    	        $compose_reply_options $commands]
}

# Function to send a $reply_file+ produced by compose_
# response.  Takes as input the list of commands
# returned by compose_response.  Does the following
#
# If the list contains FINAL: calls send_reply.
# If the list contains NOT-FINAL: calls
#      send_reply -notfinal.
# If the list contains NO-REPLY: does nothing.
# If none or more than one of the above applies,
# call error instead.
#
proc send_response { commands } {

    global response_instructions_file

    if { [llength [intersect \
    		      {FINAL NOT-FINAL NO-REPLY} \
		      $commands]] != 1 } {
	error "In $response_instructions_file file\
	       value, not exactly one of\nNO-REPLY,\
	       FINAL, and NOT-FINAL were executed:\n \
	       $commands"
    }
    if { [lcontain $commands FINAL] } {
    	send_reply
    } elseif { [lcontain $commands NOT-FINAL] } {
    	send_reply -notfinal
    } elseif { [lcontain $commands NO-REPLY] } {
    	return
    }
}

# Function to execute the if-statements in a block and
# add to the list of commands.  Stop at end of block or
# at EXIT.  Return `yes' if EXIT found, `no' otherwise.
#
proc parse_block { block commands } {

    if { [catch { llength $block }] } {
	error "$response_instructions_file file\
	       value part is not a TCL list:\n   \
	       $block"
    }

    upvar $commands c

    set mode none
    foreach item $block {
        switch $mode {
	    if_expression {
	        set if_value \
		    [expr { ! $if_done \
		            && \
		            [eval_response_if $item] }]
		set mode if_block
	    }
	    if_block {
	        if { $if_value } {
		    if { [parse_block $item c] } {
		        return yes
		    }
		    set if_done 1
		}
		set mode after_if_block
	    }
	    after_if_block {
	        switch -- $item {
		    elseif {
			set mode if_expression
		    }
		    else {
			set mode else_block
		    }
		    default {
		        set mode none
		    }
		}
	    }
	    else_block {
	        if { ! $if_done } {
		    if { [parse_block $item c] } {
		        return yes
		    }
		}
		set mode after_else_block
	    }
	    after_else_block {
	        set mode none
	    }
	}

	if { $mode == "none" } {
	    switch -- $item {
		EXIT {
		    return yes
		}
		if {
		    set mode if_expression
		    set if_done 0
		}
		default {
		    lappend c $item
		}
	    }
	}
    }
    return no
}

# Function to evaluate if-statement expression.
#
proc eval_response_if { item } {
    global scoring_mode auto_score manual_score \
           proposed_score
    set manual [expr { $manual_score != "None" }]
    set proposed [expr { $proposed_score != "None" }]
    return [expr $item]
}

# Function to execute response instruction commands
# after if-statement processing.
#
proc execute_response_commands \
	{ compose_reply_options commands } {

    global response_instructions_file \
    	   auto_score manual_score proposed_score \
	   submitted_problem submitted_extension

    set problem $submitted_problem$submitted_extension

    set processed_commands ""
    set return_commands ""
    foreach command $commands {
	if { [catch { set length [llength $command] }] \
		    } {
	    response_error $command
	}
        set opcode [lindex $command 0]
        switch -- $opcode {
	    FINAL	-
	    NOT-FINAL	-
	    NO-REPLY	-
	    EDIT	{
	    	if { $length != 1 } {
		    response_error $command
		}
	        lappend return_commands $opcode
	    }

	    CC		{
	    	if { $length != 1 } {
		    response_error $command
		}
		if { ! [lcontains \
			    $compose_reply_options \
			    -cc] } {
		    lappend compose_reply_options -cc
		}
	    }

	    BLANK		-
	    RECEIVED-HEADER	-
	    RECEIVED-BODY	{
	    	if { $length != 1 } {
		    response_error $command
		}
	        lappend processed_commands $command
	    }
	    INPUT		{
	    	if { $length != 2 } {
		    response_error $command
		}
	        lappend processed_commands $command
	    }

	    LINE	-
	    LINES	-
	    BAR		{
	        set new_command $opcode
		switch -- $opcode {
		    LINE {
			if { $length != 2 } {
			    response_error $command
			}
		    }
		    BAR {
			if { $length > 2 } {
			    response_error $command
			}
		    }
		}
		foreach string  \
			[lrange $command 1 end] {
		    regsub -all -- {-PROBLEM-} \
		           $string $problem string
		    regsub -all -- {-AUTO-SCORE-} \
		           $string $auto_score string
		    regsub -all -- {-MANUAL-SCORE-} \
		           $string $manual_score string
		    regsub -all -- {-PROPOSED-SCORE-} \
		           $string $proposed_score \
			   string
		    lappend new_command $string
		}
		lappend processed_commands $new_command
	    }

	    FIRST	-
	    SUMMARY	{
	    	if { $length != 1 } {
		    response_error $command
		}
	        error "Not implemented yet: $command"
	    }

	    default	{
		error "In $response_instructions_file\
		       file value, unrecognized\
		       command: $command"
	    }
	}
    }

    if { [llength [intersect \
    		      {FINAL NOT-FINAL NO-REPLY} \
		      $return_commands]] != 1 } {
	error "In $response_instructions_file file\
	       value, not exactly one of NO-REPLY,\
	       FINAL, and NOT-FINAL were executed:\
	       $return_commands"
    }

    if {    [lcontain $return_commands FINAL] \
         || [lcontain $return_commands NOT-FINAL] } {
	eval compose_reply $compose_reply_options \
			   $processed_commands
    }
    return $return_commands
}
proc response_error { command } {
    global response_instructions_file
    error "In $response_instructions_file file value,\
           badly formatted command: $command."
}

# Proof Display
# ----- -------

# The proof display code displays one proof at a time.
# There is a current proof group, and for each group,
# there is a current proof.  The current proof of the
# current group can be displayed.
#
# The main proof groups are:
#
#	io:  incorrect output proofs
#	ic:  incomplete output proofs
#	fe:  formatting error proofs
#	ne:  non error proofs
#
# Note that is one of these groups has no proofs, then
# that group does not actually exist, as its proofs
# cannot be displayed.
#
# In addition, each proof difference type for which
# there are some proofs forms a group by itself.  E.g.,
# if there are `word' proofs then there is a `word'
# group.
#
set current_group ""
#
# If the current_group is "", it should be taken to be
# the first one of `io', `ie', `fe', or `ne' whose
# corresponding list of proofs is non-empty.
#
# The proofs for proof group xx are listed in sorted
# order in proof_group_array(xx).  Note that if an
# element of this array would be an empty list, instead
# the element does not exist.
#
# current_group_array(xx) is the index+1 of the current
# proof in proof_group_array(xx).  If current_proof_
# array(xx) is unset, it should be taken as if it were
# set to 1.



# Function to set the current_group and its current_
# proof_array element.  Takes a list of arguments which
# are processed in order.  The arguments may be as
# follows:
#
#   An argument of at least two characters beginning
#   with a letter is matched to each group name that has
#   some proofs.  If the argument equals a group name,
#   that group is selected.  Otherwise the argument may
#   match one or more group names, and if it matches
#   exactly one, the named group is selected.  To match
#   a group name, the argument and the group name are
#   split into parts separated by `-', and each argument
#   part must equal the beginning of the corresponding
#   group name part.  The number `-'s in the argument
#   and group name must also be equal.
#
#   A numeric argument switches to the #'th proof of
#   the current_group.
#
#   An argument `n' goes to the next proof of the
#   current_group.
#
#   An argument `p' goes to the previous proof of the
#   current_group.
#
# After all arguments are processed, this function
# checks that the result designates an existing proof.
#
# If called with no arguments, merely ensures that
# current_group and its current_group_array element are
# set if they can be set without error.
#
# If there is no error, window_error is set to "" and
# `yes' is returned.  Otherwise window_error is set to
# an error description, `no' is returned, and no change
# is made in current_group or current_group_array.
#
# The following functions must have been called before
# this function is called:
#
#	compute_instruction_array
#	compute_score_and_proof_arrays
#	compute_score
#	compute_proof_info
#
proc get_proof { args } {

    global window_error proof_group_array \
           current_group current_group_array

    set group $current_group
        
    if { $group != "" } {
        set n $current_group_array($group)
    } else {
        set n 1
    }

    foreach arg $args {

        if { [regexp {^[a-z].} $arg] } {
	    regsub -all -- {-} $arg {[^-]*-} regexp
	    set regexp "^$regexp\[^-\]*\$"

	    set found ""
	    foreach t [array names proof_group_array] {
	        if { $arg == $t } {
		    set found $t
		    break
		} elseif { [regexp $regexp $t] } {
		    lappend found $t
		}
	    }
	    if { [llength $found] == 0 } {
		set window_error \
		    "There is no proof group with\
		     proofs whose name matches `$arg'"
		return no
	    } elseif { [llength $found] > 1 } {
		set window_error \
		    "There is more than one proof group\
		     with proofs whose name matches\
		     `$arg'"
		return no
	    } else {
	        set group [lindex $found 0]

		if { [info exists \
		      current_group_array($group)] } {
		    set n $current_group_array($group)
		} else {
		    set n 1
		}
	    }
	} elseif { [regexp {^[0-9]+$} $arg] } {
	    set n $arg
	} else {
	    switch -- $arg {
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

    if { $group == "" } {
        set window_error \
	    "There are no displayable proofs."
	return no
    }

    if { ! [info exists proof_group_array($group)] } {
        set window_error \
	    "There are no displayable proofs in proof\
	     group `$group'"
	return no
    }

    set max [llength $proof_group_array($group)]

    if { $n < 1 || $max < $n } {
        set window_error \
	    "There is no $n'th proof of group `$group'"
	return no
    }

    set current_group $group
    set current_group_array($group) $n
    return yes
}

# Display the current proof.  The following functions
# must have been called first:
#
#	compute_instruction_array
#	compute_score_and_proof_arrays
#	compute_score
#	compute_proof_info
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

    global current_group current_group_array \
    	   proof_group_array score_filename \
	   window_height window_info_height \
	   window_error last_display

    if { [get_proof] == "no" } {
    	return no
    }

    set proofs $proof_group_array($current_group)

    set i $current_group_array($current_group)

    set proof [lindex $proofs [expr { $i - 1 }]]

    set oline [lindex $proof 1]
    set tline [lindex $proof 2]
    set oc1   [lindex $proof 3]
    set oc2   [lindex $proof 4]
    set tc1   [lindex $proof 5]
    set tc2   [lindex $proof 6]
    set oh    [list [list $oline $oc1 $oc2]]
    set th    [list [list $tline $tc1 $tc2]]
    set desc  [lrange $proof 7 end]
    if { [lcontain {io ic fe ne} $current_group] } {
	set desc  "\[$current_group $i\]  $desc"
    } else {
	set desc  "\[$i\]  $desc"
    }

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

# Return summary information about existing proofs in as
# lines intended for inclusion in the info part of the
# display.  The following functions must have been
# called first:
#
#	compute_instruction_array
#	compute_score_and_proof_arrays
#	compute_score
#
# Text listing the proof difference types in several
# lines is returned.  Also variables used by get_proof
# are initialized.
#
# The proof difference types are grouped according to
# the evidence they give.  The groups are:
#
#	Incorrect Output (io)
#	Incomplete Output (ic)
#	Formatting Error (fe)
#	Non Error (ne)
#
set proof_group_name_array(io) {Incorrect Output}
set proof_group_name_array(ic) {Incomplete Output}
set proof_group_name_array(fe) {Formatting Error}
set proof_group_name_array(ne) {Non Error}
#
proc compute_proof_info { } {

    global incorrect_output_types \
           incomplete_output_types \
	   formatting_error_types \
	   non_error_types \
	   proof_array score_array proof_group_array \
	   proof_group_name_array \
	   current_group current_group_array \
	   window_info_height

    set error_types \
        [concat $incorrect_output_types \
	        $incomplete_output_types \
	        $formatting_error_types]
    set non_error_types ""
    foreach type [array names score_array] {
        if { ! [lcontain $error_types $type] } {
	    lappend non_error_types
	}
    }

    if { [array exists current_group_array] {
        unset current_group_array
    }
    if { [array exists proof_group_array] {
        unset proof_group_array
    }
    foreach group [array names proof_array] {
        set proof_group_array($group) \
	    $proof_array($group)
	set current_group_array($group) 1
    }

    set io_types $incorrect_output_types
    set ic_types $incomplete_output_types
    set fe_types $formatting_error_types
    set ne_types $non_error_types

    set current_group ""
    set info ""
    foreach group {io ic fe ne} {
	set types [set ${group}_types]
	if { $types == "" } continue

	set name $proof_group_name_array($group)
	set more "$name ($group):"
	set column [string length $more]
	set info "$info$more"
	set proofs ""
	foreach t $types {
	    if { [info exists proof_array($t)] } { \
	        set tproofs $proof_array($t)
	    } else {
	    	set tproofs ""
	    }

	    set n [llength $tproofs]
	    set more "  \[$n\] $score_array($t)"
	    incr column [string length $more]
	    if { $column > 80 } {
		set column [string length $more]
		incr column 2
	        set more "\n  $more"
	    }
	    set info "$info$more"
	    set proofs [concat $proofs $tproofs]
	}
	if { $proofs != "" } {
	    set proof_group_array($group) \
	        [lsort $proofs]
	    set current_group_array($group) 1
	    if { $current_group == "" } {
	        set current_group $group
	    }
	}

	set info "$info\n"
    }

    return $info
}
