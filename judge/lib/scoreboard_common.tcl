#!/bin/sh
#
# Runs the scorefinder program to get scores, and
# formats the scores into a table according to
# parameters set in hpcm_judging.rc.
#
# File:		scoreboard
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sat Oct 27 07:26:35 EDT 2001
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2001/10/27 12:07:08 $
#   $RCSfile: scoreboard_common.tcl,v $
#   $Revision: 1.25 $
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
scoreboard \[ parameter-file-name | \\
             parameter-name-value-list \] ...

    When this program starts, it processes its arguments
    as follows.  Any named parameter file is `sourced'.
    Any argument that begins with a `{' is treated as a
    TCL list (after the outer `{ }' brackets are re-
    moved) of items each of the format `{PN V}'.  For
    each item, the global variable `scoreboard_PN' is
    set to the value V.

    This program inputs the output of `scorefinder' to
    get scores, and organizes the scores into a table.
    A score is ignored unless

    	its submission time is equal to or greater than
	the value of `scoreboard_start_time', unless
	that value is \"\" or \"team\";

    	its submission time is equal to or less than
	the value of `scoreboard_stop_time', unless
	that value is \"\";

	its problem satisfies the logical expression in
	`scoreboard_problems', or that expression is an
	empty list;

	its submitter satisfies the logical expression
	in `scoreboard_submitters', or that expression
	is an empty list;

    The logical expressions are TCL lists whose elements
    are operators, parentheses `(' and `)', and atoms.
    The allowed operators are `!', `&', `^', and `|' in
    highest precedence first order, denoting `not',
    `and', `exclusive-or', and `inclusive-or'.  Opera-
    tors and parentheses MUST be surrounded by white-
    space.

    The atoms `0' and `1' represent `false' and `true'.
    All other atoms are regular expressions that are
    matched to the ENTIRE problem name or submitter
    name, and are true iff they match.

    Some simple examples of logical expressions are:

      ! count		Match all names but `count'.

      count | pascal	Match only `count' and `pascal'.

      count|pascal	Ditto, but with only one regular
      			expression atom that contains a
			`|', instead of two atoms and a
			`|' logical operator.

      bpi\[0-9]+		Match all names of the\
      				form
      			`bpi' followed by one or more
			digits.

      bpi.* & ! bpit	Match all names beginning with
			`bpi' except the one name
			`bpit'.

    The regular expressions are TCL regular expressions.
    Note that a regular expression must not as a charac-
    ter string be the same as an operator or parenthesis
    or special atom `0' or `1'.

    The scoreboard has a line for each submitter of the
    form

    submitter  score1  score2  ......  time_score

    Here each score but the `time_score' is the score
    for a problem, and has the form `X:YYuN', where X:YY
    is the time in units u (s = seconds, m = minutes,
    h = hours, d = days) of the first correct submis-
    sion, and N is the number of submissions up to and
    including the first correct submission.  If N = 0,
    the problem score is `......'.  If there is no cor-
    rect submission but N != 0, the problem score is
    `..../N'.  If there is a correct submission but the
    start time of the problem cannot be computed, the
    score is `undef/N'.  If a problem time is computed
    using a non-final score (an automatically computed
    score that is yet to be reviewed by the human
    judge), the problem score begins with an `*'.

    The score for a problem is a time equal to the pro-
    blem submission time minus the problem start time.
    The start time of a problem is start time of the
    contest in the variable `scoreboard_start_time', un-
    less this variable's value is \"\", in which case
    each problem's start time is time the problem des-
    cription was gotten (as recorded in a $gotten_file\
    				file
    by autoinfo), or unless `score_start_time's value is
    \"team\", in which case each problem's start time is
    the time the team (submitter) got its first scorable
    problem (i.e., problem allowed and not denied).  The
    time_score for each submitter is the sum of all the
    correct problem times for that submitter, plus a
    penalty times the number of incorrect submissions
    that are for a problem for which there is a later
    correct submission.  All times are actually measured
    in seconds, even though the problem times are often
    reported on the scoreboard itself only to the near-
    est minute or hour.  The penalty is the value of
    `scoreboard_penalty', and is in seconds.

    The total score is given in seconds, and is preceded
    with an `*' if there is a non-final score involved
    in its computation.  If the total score cannot be
    computed because of an `undef' problem score, the
    total score is `undef'.  If the scoring mode is
    `auto', all scores are final.  If `manual', only
    manual scores are considered at all, and automatic
    scores are totally ignored.  If `auto+manual', then
    automatic scores other than `Completely Correct' are
    not final, and other scores are final.

    The scoreboard groups the teams by the number of
    problems they have correct, and within each group
    sorts teams by total score.

    A parameters_file may be given to this program that
    will be loaded into the program after hpcm_jud-
    ging.rc.  This parameters_file may contain parameter
    settings, e.g. `scoreboard_problems' etc., in the
    same manner as the normal parameters file."

# } This closes extra left bracket in documentation so
# outer {} brackets work.

# On first -doc* argument, print documentation and exit.
#
if { [regexp {^-doc} [lindex $argv 0]] } {
    puts $document
    exit 1
}

# Process arguments.
#
set LB "{"
# } to balance left bracket in last line
#
foreach arg $argv {
    if { [regexp "^${LB}" $arg] } {
	foreach item [lindex $arg 0] {
	    set name  [lindex $item 0]
	    set value [lindex $item 1]
	    if { ! [info exists scoreboard_$name] } {
	    	error "scoreboard_$name does not exist"
	    }
	    set scoreboard_$name $value
	}
    } else {
	source $arg
    }
}

# Compile scoreboard_problems and scoreboard_submitters.
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
	    $scoreboard_submitters no_abbreviations \
	    submitter_atoms submitter_values ]
} else {
    set submitter_expression ""
}

# Compile times.
#
set start_time $scoreboard_start_time
if { ! [regexp {^(|team)$} $start_time] } {
    set start_time [clock scan $start_time]
}
set stop_time $scoreboard_stop_time
if { $stop_time != "" } {
    set stop_time [clock scan $stop_time]
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
# An array element may not exist, in which case it
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

    if { $start_time != "" && $start_time != "team" \
         && $time < $start_time } {
    	continue;
    }

    if { $stop_time != "" && $time > $stop_time } {
    	continue;
    }

    if { [lsearch -exact $problems $problem] < 0 } {
	lappend problems $problem
    }

    if { [lsearch -exact $submitters $submitter] < 0 } {
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
	set ccodes {mc}
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

	    if { [lsearch -exact $ccodes $score] \
                 >= 0 } {

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

	    } elseif { [lsearch -exact $icodes $score] \
                       >= 0 } {

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
