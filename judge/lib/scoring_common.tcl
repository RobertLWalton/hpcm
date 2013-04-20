# Common TCL Code for Scoring and Scoring Displays
#
# File:		scoring_common.tcl
# Author:	Bob Walton (walton@seas.harvard.edu)
# Date:		Mon Mar 18 06:22:17 EDT 2013
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2013/04/20 08:44:28 $
#   $RCSfile: scoring_common.tcl,v $
#   $Revision: 1.74 $
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
# difference.  For a difference type like `letter-case',
# if that is present in the scoring instructions, the
# value of `instruction_array(letter-case)' is just
# `letter-case'.  For a difference type like `number',
# if `number 0.01 -' is present in the scoring
# instructions, for example, the value of `instruction_
# array(number)' is `number 0.01 -'.  Elements of
# instruction_array correspond to differences listed in
# the scoring_instructions global variable.
#
# `score_array' is computed from the first line of the
# .score file in a manner strictly analogous to the way
# `instruction_array' is computed from the scoring
# instructions.
#
# However, the first line of the .score file contains
# markers (OGN:OCN-TGN:TCN - see scorediff documenta-
# tion) in addition to difference types.  `score_marker_
# array' is computed when `score_array' is computed.
# `score_marker_array(DT)' holds the marker associated
# with the value of `score_array(DT)', when the later
# exists.  The value of `score_marker_array(DT)' is the
# marker turned into the TCL list `OGN OCN TGN TCN'.
# If a marker is missing, the value is given as "".
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
# this type just causes the following differences to
# signal `Formatting Error' instead of `Incorrect
# Output' or `Incomplete Output':
#
#	word  word-boc  word-bog  word-eof
#	      boc-word  bog-word  eof-word
#
# The `nosign' and `nonumber' types may appear in
# `instruction_array' but not in `score_array'.  These
# are just options passed to scorediff that determine
# how scorediff parses its input into words and numbers.
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
# The global variable difference_type_proof_limit in
# hpcm_judging.rc is used to set the limit on the number
# of proofs of one type computed during scoring.  It is
# used to set the -all option to the scorediff program.
#
# The following global variable is the file name of the
# of the score file used to load the proof_array.  This
# is set when the score file loaded into proof_array.
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
#
# The following is a list of the scoring instruction
# difference types that SHOULD be passed as options
# to the scorediff program, but are not returned in the
# first line of the scorediff output or stored in
# `score_array'.
#
set option_instruction_types {
    nosign nonumber
}

# Function to compute the `instruction_array' using
# the given instructions from the source with the
# given name.
#
proc compute_instruction_array { instructions name } {

    global instruction_array

    compute_scoring_array instruction_array \
    			  $instructions $name

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

# Function to compute `score_array', `score_marker_
# array' and `proof_array' from the *.score file.  The
# argument is the .score file name.
#
proc compute_score_and_proof_arrays { score_file } {

    global proof_array score_array score_marker_array \
           score_filename

    set score_filename $score_file
    set score_ch [open $score_file r]

    compute_scoring_array score_array [gets $score_ch] \
			  "$score_file first line" \
			  score_marker_array

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
# Array is the array to compute, instructions is the
# input line or scoring_instructions variable value,
# and name is a description of the instructions for
# error messages.  Marker_array is the associate of
# score_array that holds the markers from the first
# line of a score file: if its name is given, the
# instructions must have markers.
#
proc compute_scoring_array \
	{ array_name instructions name \
	  { marker_array_name "" } } {

    if { [catch {llength $instructions}] } {
        error "$name is not a TCL list:\n$instructions"
    }

    upvar $array_name array

    if { [array exists array] } {
        unset array
    }
    
    if { $marker_array_name != "" } {
        set has_markers 1
	upvar $marker_array_name marker_array
	if { [array exists marker_array] } {
	    unset marker_array
	}
    } else {
        set has_markers 0
    }

    set state type
    set marker ""
    set marker_regexp \
        {^([0-9]+):([0-9]+)-([0-9]+):([0-9]+)$}
    foreach item $instructions {
	switch $state {
	    type {
		if {    $has_markers \
		     && [regexp {^[0-9]} $item] } {
		    if { ! [regexp $marker_regexp \
				   $item forget \
				   OGN OCN TGN TCN] } {
		        error "$name has badly\
			       formatted marker: $item"
		    }
		    set marker \
		        [list $OGN $OCN $TGN $TCN]
		} else {
		    set array($item) $item
		    if { $has_markers } {
		        set marker_array($item) $marker
		    }
		    if { [lcontain \
		              {number integer float} \
			      $item] } {
			set previous $item
			set state first
		    }
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
# and .test files (or .{jf,j,f}out and .{jf,j,f}test
# files).  The scorediff program is called by
#
#     scorediff OPTION ... outfile testfile > scorefile
#
# where the scoring instructions and the `difference_
# type_proof_limit' value from `hpcm_judging.rc' are
# used to compute OPTIONs that obtain an optimal set of
# proofs.  The `options' argument can be used to pass
# additional options to scorediff.
#
# Any negative numbers in `float' or `integer'
# instructions are converted to `-' before they are
# passed to scorediff.
#
# `compute_instruction_array' must be called before
# this routine is called.
#
proc compute_score_file { outfile testfile scorefile \
			  { options {} } } {

    global difference_type_proof_limit \
           instruction_array fake_instruction_types

    lappend options -all $difference_type_proof_limit

    foreach type [array names instruction_array] {

        if { [lcontain $fake_instruction_types \
	               $type] } continue

	set arguments $instruction_array($type)
	lappend options -[lindex $arguments 0]
	foreach arg [lrange $arguments 1 end] {
	    if {    ! [catch { set test \
	                           [expr $arg < 0] }] \
		 && $test } {
	        set arg "-"
	    }
	    lappend options $arg
	}
	# The next line is necessary else the proof
	# limit for the type is set to 0.
	#
        if { [lcontain {number integer float} $type] } {
	    lappend options $difference_type_proof_limit
	}
    }

    eval [list exec scorediff] $options \
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
# Additionally sets the associated variables:
#
#	incorrect_output_marker
#	incomplete_output_marker
#	formatting_error_marker
#
# to the smallest markers associated with any of the
# difference types listed in the associated _types
# variables.  Here smallest is 4-tuple comparison,
# and the markers are lists `OGN OCN TGN TCN'.  If
# there are no markers listed for one of these vari-
# ables, the value of the variable is "".
#
# Also sets
#
#	end_marker
#
# to the marker associated with eof-eof, and sets
#
#	largest_marker
#
# to the largest marker of any difference type.
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
# Return -1, 0, 1 according to whether old < new,
# old == new, or old > new, where old and new are
# markers each of which is a list of 4 integers.
#
proc compare_markers { new old } {
    if { [lindex $new 0] > [lindex $old 0] } {
	return 1
    } elseif { [lindex $new 0] < [lindex $old 0] } {
	return -1
    } elseif { [lindex $new 1] > [lindex $old 1] } {
	return 1
    } elseif { [lindex $new 1] < [lindex $old 1] } {
	return -1
    } elseif { [lindex $new 2] > [lindex $old 2] } {
	return 1
    } elseif { [lindex $new 2] < [lindex $old 2] } {
	return -1
    } elseif { [lindex $new 3] > [lindex $old 3] } {
	return 1
    } elseif { [lindex $new 3] < [lindex $old 3] } {
	return -1
    } else {
        return 0
    }
}
proc compute_score { } {

    global instruction_array score_array \
           score_marker_array \
    	   incorrect_output_types \
    	   incomplete_output_types \
    	   formatting_error_types \
    	   incorrect_output_marker \
    	   incomplete_output_marker \
    	   formatting_error_marker \
	   end_marker \
	   largest_marker

    # Note end_types is not global.

    set incorrect_output_types ""
    set incomplete_output_types ""
    set formatting_error_types ""
    set end_types ""
    set incorrect_output_marker ""
    set incomplete_output_marker ""
    set formatting_error_marker ""
    set end_marker ""
    set largest_marker ""

    set waf \
	[info exists \
	      instruction_array(words-are-format)]

    foreach type [array names score_array] {

        # If instruction for the difference type exists,
	# continue loop, unless difference type is
	# integer or float and instruction absolute
	# difference is less than score absolute
	# difference OR the instruction relative
	# difference is less than score relative
	# difference.

        if { [info exists instruction_array($type)] } {

	    if { [lcontain {integer float} $type] } {
		set s $score_array($type)
		set i $instruction_array($type)
		if { [lindex $s 1] <= [lindex $i 1] \
		     || \
		     [lindex $s 2] <= [lindex $i 2] } {
		    continue
		}
	    } else continue
	}

	switch -- $type {

	    integer -
	    float -
	    infinity -
	    number-boc -
	    number-bog -
	    number-eof -
	    boc-number -
	    boc-bog -
	    boc-eof -
	    bog-number -
	    bog-boc -
	    bog-eof {
		set kind incorrect_output
	    }

	    number-word -
	    boc-word -
	    bog-word -
	    word-number -
	    word-boc -
	    word-bog -
	    word-eof -
	    word {
		if { $waf } {
		    set kind formatting_error
		} else {
		    set kind incorrect_output
		}
	    }

	    eof-word {
		if { $waf } {
		    set kind formatting_error
		} else {
		    set kind incomplete_output
		}
	    }

	    
	    eof-boc -
	    eof-bog -
	    eof-number {
		set kind incomplete_output
	    }

	    decimal -
	    exponent -
	    sign -
	    letter-case -
	    column -
	    whitespace -
	    beginspace -
	    endspace -
	    linespace -
	    spacebreak -
	    linebreak {
		set kind formatting_error
	    }

	    eof-eof {
	        set kind end
	    }

	    default {
		error "Unknown scorediff result: $type" 
	    }
	}

	lappend ${kind}_types $type
	set new $score_marker_array($type)
	if { $new == "" } continue

	set old [set ${kind}_marker]
	if {    $old == "" \
	     || [compare_markers $new $old] < 0 } {
	    set ${kind}_marker $new
	}

	set old $largest_marker
	if {    $old == "" \
	     || [compare_markers $new $old] > 0 } {
	    set largest_marker $new
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

# Function to process response instructions and produce
# a Reply_Mail+ file by calling compose_reply.  Docu-
# mentation of response instructions is found in
# hpcm_judging.rc along with the default value of the
# `response_instructions' global variable.
#
# The compose_reply_options are a list of options,
# such as -cc and -error, that are passed to compose_
# reply.  The response instruction command CC will
# also force a -cc option to compose_reply.
#
# The global variables scoring_mode, auto_score,
# auto_score_marker, manual_score, and proposed_score
# must be set.  The latter two can be set to `None'
# if unused.  Auto_score_marker can be set to "" if
# unused.
#
# The global variables `submitted_problem' and
# `submitted_extension' must be set to the problem name
# and extension.  The global variable `submit_qualifier'
# must be set (it can be "").
#
# The global variables problem_required_files and
# problem_optional_files should be set as they are after
# any problem.rc file is sourced.  They need not exist,
# and will be set to their defaults if they do not.
#
# The global variable problem_input_names must exist and
# already be set either to its problem.rc given value or
# to its default (the list of names N such that N.in or
# N.jin exist, excluding the problem name itself if
# there are other names).
#
# This function returns a list of commands whose execu-
# tion merely serve to specify options for disposition
# of Reply_Mail+.  Specifically, any FINAL, NOT-FINAL,
# NO-REPLY, or EDIT commands that are executed are
# merely returned in this list, and have no direct
# effect on the contents of Reply_Mail+.  This list
# is guarenteed to have exactly one of FINAL, NOT-
# FINAL, or NO-REPLY, and may be passed to send_
# response.
#
proc compose_response { { compose_reply_options "" } } {

    global response_instructions \
           response_instructions_globals \
	   submit_qualifier response_qualifier \
	   default_correct_qualifier \
	   default_incorrect_qualifier \
	   auto_score

    if { $submit_qualifier == "" } {
        if { $auto_score == "Completely Correct" } {
	    set response_qualifier \
	        $default_correct_qualifier
	} else {
	    set response_qualifier \
	        $default_incorrect_qualifier
	}
    } else {
	set response_qualifier $submit_qualifier
    }

    if { [catch { llength $response_instructions }] } {
	error "response_instructions value is\
	       not a TCL list"
    }

    # Execute first pass over response instructions
    # and compute in `commands' a list of response
    # instructions to be executed in the second pass.
    # The first pass removes if ... elseif ... else
    # and produces a linear list of unconditional
    # response instructions. 
    #
    set commands ""
    parse_block $response_instructions commands \
   		$response_instructions_globals \
		{ { manual \
		      { $manual_score != "None" }} \
		  { proposed \
		      { $proposed_score != "None" }} }

    # Execute second pass on action response instruc-
    # tions and return those whose action is to be per-
    # formed by our caller (i.e., are instructions for
    # disposal of Reply_Mail+).
    #
    return [execute_response_commands \
    	        $compose_reply_options $commands]
}

# Function to send the Reply_Mail+ file produced by
# compose_response.  Takes as input the list of commands
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

    if { [llength [intersect \
    		      {FINAL NOT-FINAL NO-REPLY} \
		      $commands]] != 1 } {
	error "In `response_instructions'\
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

# Helper function for (helper functions of) execute_
# response_commands below.
#
# Given a proof from proof_array, output commands
# describing it to processed commands.
#
# The following global variables must be set:
#
# process_proof_file	.{jf,j,f,}out file name
# process_proof_ch	.{jf,j,f,}out file channel id
# process_proof_prefix	1 to remove first character
#			of displayed line, 0 not to
# process_proof_number	line number of next line
#			to be read from channel
# process_proof_line	last line read from channel
#			without first process_proof_
#			prefix characters
#
#
proc process_proof { proof processed_commands } {

    upvar $processed_commands pc

    global submitted_problem \
	   process_proof_file process_proof_ch \
	   process_proof_prefix \
    	   process_proof_number process_proof_line

    set number [lindex $proof 1]
    set column [lindex $proof 3]
    incr column

    if { $process_proof_number > $number } {
        error "Proof out of sequence,\
	       file: $process_proof_file,\
	       last line read is number\
	       $process_proof_number,\nproof\
	       is: $proof"
    }
    while { $process_proof_number < $number } {
        set process_proof_line [gets $process_proof_ch]
	if { [eof $process_proof_ch] } {
	    set process_proof_line \
	        "<<---END-OF-FILE--->>"
	}
	set process_proof_line \
	    [string range $process_proof_line \
	            $process_proof_prefix end]
	incr process_proof_number
    }
    lappend pc [list BAR "FILE $process_proof_file,\
                           LINE $number,\
                           COLUMN $column:"]
    lappend pc [list LINE $process_proof_line]
}


# Helper function for execute_response_commands below.
# Just processes IN or OUT command out-of-line to make
# code neater.
#
proc process_in_or_out_command \
	{ command processed_commands } {

    upvar $processed_commands pc

    global auto_score_marker problem_input_names

    set score_file [lindex $auto_score_marker 0]
    set group [lindex $auto_score_marker 1]
    set case [lindex $auto_score_marker 2]

    if { $command == "IN" } {
        set kind input
    } else {
        set kind output
    }

    if { $score_file != "" } {
	set name [file rootname $score_file]
	set jin_exists [file exists $name.jin]
    }

    if {    $score_file == "" \
         || (    ! $jin_exists \
	      && [llength $problem_input_names] < 2 ) \
	 || (    $jin_exists \
	      && (    $group == "" \
    	           || $case == "" \
                   || $case == 0 ) ) } {
	lappend pc \
	     BAR \
	     [list LINE "Sorry, the information needed\
	     to locate the judge's $kind for the"] \
	     [list LINE "failed test case is\
	     unavaliable."]
        return
    }

    if { ! [regexp {^(.*)score$} $score_file] } {
	error "SYSTEM ERROR: non-*score score file"
    }

    if { $jin_exists } {
	set id $name:$group:$case
	if { $command == "IN" } {
	    file delete -force -- $id.in
	    exec jfilter -c $group:$case $name.jin \
	         $id.in >@ stdout
	    lappend pc [list BAR "Judge's Input $id:"]
	    lappend pc [list INPUT $id.in]
	} else {
	    if { ! [file exists $name.test] } {
		error "SYSTEM ERROR: no $name.test file"
	    }
	    file delete -force -- $id.test
	    exec jfilter -c $group:$case $name.jin \
		 $name.test $id.test >@ stdout
	    lappend pc [list BAR "Judge's Output $id:"]
	    lappend pc [list INPUT $id.test]
	}
    } else {
	if { $command == "IN" } {
	    lappend pc [list BAR "Judge's Input $name:"]
	    lappend pc [list INPUT $name.in]
	} else {
	    if { ! [file exists $name.test] } {
		error "SYSTEM ERROR: no $name.test file"
	    }
	    lappend pc \
	            [list BAR "Judge's Output $name:"]
	    lappend pc [list INPUT $name.test]
	}
    }
}

# Helper function for compose_response above.  Executes
# response instruction commands after if-statement
# processing.
#
proc execute_response_commands \
	{ compose_reply_options commands } {

    global auto_score manual_score proposed_score \
	   submitted_problem submitted_extension \
	   submit_qualifier \
	   judging_directory \
	   problem_required_files \
	   problem_optional_files

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
		if { ! [lcontain \
			    $compose_reply_options \
			    -cc] } {
		    lappend compose_reply_options -cc
		}
	    }

	    BLANK		-
	    RECEIVED-HEADER	-
	    RECEIVED-FULL-HEADER	-
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
		    regsub -all -- \
		           {-SUBMIT-QUALIFIER-} \
		           $string $submit_qualifier \
			   string
		    lappend new_command $string
		}
		lappend processed_commands $new_command
	    }

	    CERR	{
	    	if { $length != 1 } {
		    response_error $command
		}
		set file $submitted_problem.cerr
		lappend processed_commands \
			[list INPUT $file]
	    }

	    SOLUTION	{
	    	if { $length != 1 } {
		    response_error $command
		}
		set sdir $judging_directory/solutions
		set sdir $sdir/$submitted_problem

		set solution_files ""
		if { [info exists \
		           problem_required_files] } {
		    lappend_lists solution_files \
		         $problem_required_files
		} elseif { $submitted_extension \
		           != "" } {
		    lappend solution_files $problem
		}
		if { [info exists \
		           problem_optional_files] } {
		    lappend_lists solution_files \
		         $problem_optional_files
		}
		    
	        foreach file $solution_files {
		    set f $sdir/$file
		    lappend processed_commands \
			    [list BAR $file:]
		    lappend processed_commands \
			    [list INPUT $f]
		}
	    }

	    IN  -
	    OUT	{
	    	if { $length != 1 } {
		    response_error $command
		}
		process_in_or_out_command \
		    $command processed_commands
	    }

	    ERROR {
	    	if { $length != 2 } {
		    response_error $command
		}
		error [lindex $command 1]
	    }

	    default	{
		error "In `response_instructions'\
		       value, unrecognized\
		       command: $command"
	    }
	}
    }

    if { [llength [intersect \
    		      {FINAL NOT-FINAL NO-REPLY} \
		      $return_commands]] != 1 } {
	error "In `response_instructions'\
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

# Helper function for execute_response_commands above.
#
proc response_error { command } {
    error "In `response_instructions' value,\
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
# Note that if one of these groups has no proofs, then
# that group does not actually exist, and its proofs
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
# the first one of `io', `ic', `fe', or `ne' whose
# corresponding list of proofs is non-empty.
#
# The proofs for proof group xx are listed in sorted
# order in proof_group_array(xx).  Note that if an
# element of this array would be an empty list, instead
# the element does not exist.  Elements of this array
# have the same format as elements of proof_array; and
# in fact proof_group_array(xx) == proof_array(xx) if
# the latter exists (which it does NOT if xx == io, ic,
# fe, or ne).
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
#   group name part.  The number of `-'s in the argument
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

	    # Match argument to group name.

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

	    # Set number of current proof to argument.

	    set n $arg

	} else {

	    # Process p and n commands.

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

        # This should never happen.

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
# The files referenced in the proof have names identical
# to score_filename with `score' at the end replaced by
# `out' or `test'.
#
# If there is no error, window_error is set to "" and
# `yes' is returned.  Otherwise window_error is set to
# an error description and `no' is returned.
#
# At its beginning this function calls `get_proof'
# without arguments and returns `no' if that call
# returns `no'.
#
# This function calls compute_file_display for the out
# file and out_file_array and for  the test file and
# test_file_array.
#
# If there is no error, this function sets the
# `last_display' variable to `proof'.
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

    if { ! [regexp {^(.*)score$} $score_filename \
    		   forget prefix] } {
	error "SYSTEM ERROR: non-*score score file"
    }
    set out_file ${prefix}out
    set test_file ${prefix}test

    set prefix [regexp {\.j[^.]*out$} $out_file]

    set_window_display \
        "[compute_file_display $out_file $prefix \
	                       out_file_array \
			       $omin \
			       [expr { $oline + $L }] \
			       $oh \
        ][compute_file_display $test_file $prefix \
	                       test_file_array \
			       $tmin \
			       [expr { $tline + $L }] \
			       $th \
	][bar_with_text $desc]"

    set last_display proof
}

# Return summary information about existing proofs as
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
# are initialized or computed: namely, current_group,
# current_group_array, proof_group_array.  Current_group
# is set to the first of io, ic, fe, or ne that has
# some proofs (see below), or is set to "" if there
# are no proofs.
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

    if { [array exists current_group_array] } {
        unset current_group_array
    }
    if { [array exists proof_group_array] } {
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
