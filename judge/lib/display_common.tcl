# Common TCL Code for Displays
#
# File:		display_common.tcl
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Fri Aug 24 22:56:45 EDT 2001
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2001/08/25 02:48:50 $
#   $RCSfile: display_common.tcl,v $
#   $Revision: 1.1 $
#
#
# Note: An earlier version of this code used to be in
# manualreply.

# Table of Contents
#
#	Including this Code
#	Display
#	Reading Files
#	File List
#	Response Functions
#	Locking Functions
#	Reply Functions

# Including this Code
# --------- ---- ----


# Include this code in TCL program via:
#
#	set lib_directory \
#	    "[file dirname $argv0]/../lib"
#	source $lib_directory/judging_common.tcl
#	source $lib_directory/display_common.tcl
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

# Display
# -------
#
# The display data and functions manage the display.

# Several of the following functions add text lines to
# the end of a variable, rather than writing to stdout.
# By accumulating text in a variable and writing it in
# a single puts statement, and by designing successive
# puts strings to have the same layout, we get the
# effect of having a single window.
#
# The window has the following parts which are display-
# ed in the order given:
#
#	window_blank	blank lines above window
#	window_display	file list or file
#	window_info	information specific to window
#	1 blank line
#	window_error	error line just above prompt
#	window_prompt	prompt and user response line
#
# The last two parts are 1 line each.  The height of
# window_info is carefully controlled, the height of the
# window_display is suggested (its actually a maximum),
# and the height of the entire window is controlled
# unless it is too large.  The window_display should end
# with a `==========' bar to separate it from the win-
# dow_info.
#
set window_blank ""
set window_display ""
set window_info ""
set window_error ""
set window_prompt ""
#
# The following are imported from hpcm_judging.rc.
# Window_height is the nominal height of the visible
# part of the window, and window_blank_height is the
# height of the blank part of the window on top of the
# visible part if the visible part is its nominal
# height.
# 
# set window_blank_height 16
# set window_height 24
#
# The following is reset frequently as window_info
# changes.
#
set window_info_height 8

set window_newlines "\n\n\n\n\n\n\n\n\n\n"
set window_newlines "$window_newlines$window_newlines"
set window_newlines "$window_newlines$window_newlines"
set window_newlines "$window_newlines$window_newlines"
set window_newlines "$window_newlines$window_newlines"

# Display the current window.
#
proc display_window {} {

    global window_blank window_display window_info \
    	   window_error window_prompt

    set b $window_blank
    set d $window_display
    set i $window_info
    set e $window_error
    set p $window_prompt

    # We try to output the display all-at-once to get
    # the effect of a quick refresh.
    #
    puts -nonewline "$b\n$d\n$i\n\n$e\n$p"
    flush stdout
}

# Set the window_info variable the info part of the next
# window to be displayed.  Add blank lines to the top of
# the given `info' to make it have window_info_height
# lines, or call error if the `info' has more than
# window_info_height lines.
#
proc set_window_info { info } {

    global window_info window_info_height \
    	   window_newlines

    set split [split $info "\n"]
    set height [llength $split]
    if { $height > $window_info_height } {
    	error "window info too high:\n$info"
    }
    set offset [expr { $window_info_height \
    		       - $height - 1 }]
    set window_info \
        "[string range $window_newlines \
	         0 $offset]$info"
}


# Set the window_display and window_blank variable to
# specify the blank and display parts of the next window
# to be displayed.  The number of blank lines in window_
# blank is set to be sure the entire displayed window
# with blank lines will have window_height + window_
# blank_height lines.
#
proc set_window_display { display } {

    global window_blank window_display \
    	   window_height window_info_height \
	   window_blank_height window_newlines

    set height [llength [split $display "\n"]]

    # We need to leave 3 lines for the window_prompt,
    # window_error, and the blank line above these.
    #
    set offset \
        [expr { $window_height \
	        + $window_blank_height \
	        - $window_info_height - 3 \
		- $height - 1 }]
    set window_blank \
        [string range $window_newlines 0 $offset]
    set window_display $display
}

# The window bar is 80 `='s on a line, possibly with
# text replacing some of the `='s a the end of the
# line.
#
set window_bar "===================="
set window_bar "$window_bar$window_bar"
set window_bar "$window_bar$window_bar"

# Return a window bar with the given text at its end.
#
proc bar_with_text { text } {
    global window_bar
    set l [expr 80 - 2 - [string length $text]]
    return "[string range $window_bar 0 $l] $text"
}

# Set the window display to display the first lines
# of the file.  Set the last_display variable to
# `file'.  If the file is unreadable or is not a plain
# file, switch to displaying the file list instead.
#
proc set_file_display { filename } {

    global window_height window_info_height \
           last_display

    if { ! [file readable $filename] \
         || ! [file isfile $filename] } {
	refresh_file_list
    	set_file_list_display
	return
    }

    # Height is the number of lines of the file that are
    # displayed.  The 5 includes the prompt, error line,
    # blank line, and two bar lines.
    #
    set height [expr { $window_height \
                       - $window_info_height - 5 }]

    # Compute the window display in $display.
    #
    set display [bar_with_text "$filename:"]

    set file_ch [open $filename r]
    set n 0
    while { "yes" } {
    	set line [gets $file_ch]
	if { [eof $file_ch] } break

	incr n

	if { $n <= $height } {
	    set display "$display\n$line"
	}
    }
    close $file_ch

    if { $n > $height } {
	set more [expr { $n - $height }]
    	set bar [bar_with_text ". . . . .\
		 there are $more more lines in this\
		 file"]
    } else {
        set bar [bar_with_text " end-of-file"]
    }

    # Set the window display and indicate a file was
    # the last thing displayed.
    #
    set_window_display "$display\n$bar"
    set last_display file
}

# Reading Files
# ------- -----
#
# Following data and functions copy information from
# files in the current directory into variables within
# this program whenever the files change.

# Read a 1-line file and return its line.  Check for
# the file being non-existing, unreadable, and empty,
# and in these cases return `none', `UNREADABLE', and
# `EMPTY'.
#
proc read_score { score_file } {

    if { [file exists $score_file] } {
	if { ! [file readable $score_file] } {
	    return UNREADABLE
	} else {
	    set score [read_file $score_file]
	    if { $score == "" } {
		return EMPTY
	    } else {
	    	return $score
	    }
	}
    } else {
	return none
    }
}


# Functions to read information from files.  There is
# one of these per file that can be read.  These
# functions must take no arguments; they are called
# when it is discovered their file has been modified.
#
# For each of the below files, read_array(filename) is
# set to the name of the function that reads the file
#
# These files are actually read by refresh_file_list
# below.  This sets mtime_array(filename) to the mtime
# of the file just before it is read, if the file
# exists.  If the file does not exist, mtime_array(
# filename) must not exist.
#
# The variables must be initialized to their settings
# when the files do not exist.

# Read $auto_score_file
#
proc read_auto_score {} {

    global auto_score_file auto_score

    set auto_score [read_score $auto_score_file]
}
set read_array($auto_score_file) read_auto_score
set auto_score none

# Read $manual_score_file
#
proc read_manual_score {} {

    global manual_score_file manual_score

    set manual_score [read_score $manual_score_file]
}
set read_array($manual_score_file) read_manual_score
set manual_score none

# Read $scoring_instructions_file
#
proc read_scoring_instructions {} {

    global scoring_instructions

    set scoring_instructions [find_scoring_instructions]
}
set read_array($scoring_instructions_file) \
    read_scoring_instructions
set scoring_instructions $scoring_instructions_default

# Read the received mail header.  Set the submitted_
# file, program, and extension variables.  If the
# subject is inappropriate to a submission, set these
# variables to "".
#
proc read_received_file {} {

    global received_file message_subject \
	   submitted_file submitted_program \
	   submitted_extension

    if { ! [file readable $received_file] } {
	set submitted_file      ""
	set submitted_program   ""
	set submitted_extension ""
	return
    }

    set received_ch [open $received_file r]
    read_header $received_ch
    close $received_ch

    # Compute submitted program.
    #
    if { [llength $message_subject] == 2 \
	 && [lindex $message_subject 0] == "submit" } {
	set submitted_file \
	    [lindex $message_subject 1]
	set submitted_program \
	    [file rootname $submitted_file]
	set submitted_extension \
	    [file extension $submitted_file]
    } else {
	set submitted_file      ""
	set submitted_program   ""
	set submitted_extension ""
    }
}
set read_array($received_file) read_received_file
set submitted_file      ""
set submitted_program   ""
set submitted_extension ""


# File List
# ---- ----
#
# Data and functions that maintain the list of files in
# the current directory.

# List of files in the current directory.  The N'th
# item on the list is itself a list of subitems
# with the format:
#
#	ctime mtime filename comment new
#
# The total list is sorted by ctime.  Ctime equals
# mtime, the file's modification time, with 0's prepend-
# ed so all ctimes are the same length and alphanumeric
# comparison of items will be the same as numeric com-
# parison of mtimes.
#
# After sorting, entries of the form:
#
#	TBD TBD filename comment ""
#
# may be added to the end of the list of items to
# indicate files that could be made but do not yet
# exist.
#
# The comment is to be printed after the filename if
# the file name is not too long.  Generally non-
# error log file names are short enough for the comment,
# and error log file names are too long.
#
# `New' is `*' if the file is new since the last time
# the file list was computed, and the previous file list
# was not empty.  Otherwise `new' is "".
#
# Below `file_list' is a simple unsorted list of file
# names that can be lsearch'ed.  `file_list_items' has
# the list of items described above, sorted by mtimes.
# The number of files is in `file_list_length'.  The
# mtime of the $received_file file is in `file_list_
# origin_mtime', unless there is no such file, in which
# case the mtime of the newest file is there instead.
#
# When building the file list, the information read by
# the read_array functions is refreshed as necessary,
# using corresponding mtime_array information where
# mtime_array(filename) is the mtime of the file just
# before read_array(filename) was last called, and
# mtime_array(filename) does not exist if the file does
# not exist.
#
set file_list ""
set file_list_items ""
set file_list_length 0
set file_list_origin_mtime 0

# Recompute the file list.  New files are marked unless
# the file list was previously empty.
#
# If `submitted_program' is not "" and the .out and
# .test files for the submitted program exist, then
# TBD file entries for the .diff and .bdiff files
# are added to the end of the file list.
#
proc refresh_file_list { } {

    global file_list file_list_items file_list_length \
    	   file_list_origin_mtime received_file \
	   read_array mtime_array submitted_program

    set new_file_list [glob -nocomplain *]
    set new_file_list_items ""
    set n 0
    set origin ""
    set newest 0
    foreach file $new_file_list {

        # Set ctime, mtime, and filename of item.
	#
	set mtime [file mtime $file]
	if { $file == $received_file } {
	    set origin $mtime
	}
	if { $mtime > $newest } {
	    set newest $mtime
	}
    	set item [format {%025d} $mtime]
	lappend item $mtime
	lappend item $file

	# Set comment of item.
	#
	set size [file size $file]
	if { [file isdirectory $file] } {
	    lappend item "(directory)"
	} elseif { $size == 0 } {
	    lappend item "(empty)"
	} elseif { ! [file readable $file] } {
	    lappend item "(unreadable file)"
	} else {
	    set file_ch [open $file r]
	    set line [gets $file_ch]
	    gets $file_ch
	    if { [eof $file_ch] \
	         && [string length $line] < 35 } {
	    	lappend item "{$line}"
	    } else {
		lappend item "(${size}b)"
	    }
	    close $file_ch
	}

	# Set `new' field of item.
	#
	set new ""
	if { $file_list_length != 0 \
	     && [lsearch -exact $file_list $file] \
	        < 0 } {
	    set new "*"
	}
	lappend item $new

	# Add item to new file list.
	#
	lappend new_file_list_items $item
	incr n

	# Process file if it is one from which we
	# read information and it has changed since
	# last call to refresh_file_list.
	#
	if { [info exists read_array($file)] } {
	    if { ! [info exists mtime_array($file)] \
	         || $mtime_array($file) < $mtime } {
	    	set mtime_array($file) $mtime
		eval $read_array($file)
	    }
	}
    }

    # Process any files from which we read information
    # which have been deleted.
    #
    foreach file [array names read_array] {
        if { [lsearch -exact $new_file_list $file] < 0 \
	     && [lsearch -exact $file_list $file] >= 0 \
	     } {
	    # File has been deleted.
	    unset mtime_array($file)
	    eval $read_array($file)
	}
    }

    # Set file list global data to newly computed file
    # list.
    #
    set file_list $new_file_list
    set file_list_items [lsort $new_file_list_items]
    set file_list_length $n

    if { $origin != "" } {
    	set file_list_origin_mtime $origin
    } else {
    	set file_list_origin_mtime $newest
    }

    # Add TBD files (non-existant .*diff files) to the
    # file list if appropriate.
    #
    if { $submitted_program != "" } {
	set diff_file  $submitted_program.diff
	set bdiff_file $submitted_program.bdiff
	set out_file   $submitted_program.out
	set test_file  $submitted_program.test

	if { [lsearch -exact $file_list $out_file] >= 0
	     && [lsearch -exact $file_list $test_file] \
		    >= 0 } {
	    if { [lsearch -exact $file_list \
				 $diff_file] \
		  < 0 } {
		lappend file_list_items \
		        [list TBD TBD \
			      $diff_file (TBD) ""]
		lappend file_list $diff_file
		incr file_list_length
	    }
	    if { [lsearch -exact $file_list \
				 $bdiff_file] \
		  < 0 } {
		lappend file_list_items \
		        [list TBD TBD \
			      $bdiff_file (TBD) ""]
		lappend file_list $bdiff_file
		incr file_list_length
	    }
	}
    }

}

# Set the window display to the file list.  Set the
# last_display variable to `file_list'.
#
proc set_file_list_display {} {

    global file_list_items file_list_origin_mtime \
    	   window_bar last_display

    set display "$window_bar"

    set n 0
    set previous ""

    # If `previous' is non-empty it is the previous
    # item and has no more than 39 characters.
    #
    foreach item $file_list_items {
	incr n
	set time [lindex $item 1]
	if { $time == "TBD" } {
	    set tttt TBD
	} else {
	    set time [expr { $time \
			     - $file_list_origin_mtime
                           }]
	    if { $time < 0 } {
		set sign "-"
		set time [expr { - $time }]
	    } else {
		set sign ""
	    }
	    set mm [expr { $time / 60 }]
	    set hh [expr { $mm / 60 }]
	    set mm [expr { $mm - 60 * $hh }]
	    set tttt "$sign[format {%d:%02d} $hh $mm]"
	    if { [string length $tttt] > 6 } {
		set tttt "${sign}inf"
	    }
	}
	set next [format {%3d. %6.6s%1.1s %s} \
			 $n \
			 $tttt [lindex $item 4] \
			 [lindex $item 2]]
	set commented "${next} [lindex $item 3]"

	if { [string length $commented] <= 80 } {
	    set next $commented
	}
	if { [string length $next] > 39 } {
	    if { $previous != "" } {
	    	set next "$previous\n$next"
	    }
	    set previous ""
	} elseif { $previous != "" } {
	    set next [format {%-40s%s} $previous $next]
	    set previous ""
	} else {
	    set previous $next
	    set next ""
	}

	if { $next != "" } {
	    set display "$display\n$next"
	}
    }

    if { $previous != "" } {
	set display "$display\n$previous"
    }

    set_window_display "$display\n$window_bar"
    set last_display file_list
}

# Given a file number, set `last_file' to the name of
# the corresponding file, set `window_error' to "", and
# return `yes'.  If there is an error, leave `last_file'
# alone, set `window_error' to the error description,
# and return `no'.
#
# Whenever `last_file' is changed, if the new name is
# of the form $submitted_program.ext for some extension
# ext, and the array entry make_file_array(.ext) exists,
# the value of that array entry is called to make or
# update the new `last_file' file.  The called procedure
# may not succeed: its perfectly possible to set `last_
# file' to a non-existant file.
#
proc get_file { number } {

    global file_list_items file_list_length \
	   last_file window_error make_file_array \
	   submitted_program

    set window_error ""
    if { $number == "" } {
	if { $last_file == "" } {
	    set window_error "No previous file!"
	    return no
	} else {
	    return yes
	}
    } elseif { $number < 1 \
               || $number > $file_list_length } {
	set window_error "Bad file number: $number"
	return no
    } else {
	set last_file \
            [lindex [lindex $file_list_items \
                            [expr { $number - 1 }]] 2]

	if { $submitted_program == \
		[file rootname $last_file] } {
	    set extension [file extension $last_file]
	    if { [info exists \
		       make_file_array($extension)] } {
		eval $make_file_array($extension)
	    }
	}
	return yes
    }
}

# Procedure to make $submitted_program.diff if
# possible.
#
proc make_diff {} {

    global submitted_program

    set diff_file $submitted_program.diff
    set out_file  $submitted_program.out
    set test_file $submitted_program.test

    if { ! [file readable $out_file] }  return
    if { ! [file readable $test_file] } return

    if { [file exists $diff_file] } {
        if { [file mtime $diff_file] \
	         < [file mtime $out_file] \
	     || \
	     [file mtime $diff_file] \
	         < [file mtime $test_file] } {
	    file delete -force $diff_file
	} else {
	    return
	}
    }

    write_file $diff_file \
	       "===== diff $out_file $test_file"
    catch { exec diff $out_file $test_file \
                      >>& $diff_file }
}
set make_file_array(.diff) make_diff

# Procedure to make $submitted_program.bdiff if
# possible.
#
proc make_bdiff {} {

    global submitted_program

    set bdiff_file $submitted_program.bdiff
    set out_file  $submitted_program.out
    set test_file $submitted_program.test

    if { ! [file readable $out_file] }  return
    if { ! [file readable $test_file] } return

    if { [file exists $bdiff_file] } {
        if { [file mtime $bdiff_file] \
	         < [file mtime $out_file] \
	     || \
	     [file mtime $bdiff_file] \
	         < [file mtime $test_file] } {
	    file delete -force $bdiff_file
	} else {
	    return
	}
    }

    write_file $bdiff_file \
	       "===== diff -b $out_file $test_file"
    catch { exec diff -b $out_file $test_file \
                      >>& $bdiff_file }
}
set make_file_array(.bdiff) make_bdiff


# Response Functions
# -------- ---------

# Wait for an answer line to be input.  If it is y, Y,
# yes, or YES, return `yes'.  If it is n, N, no, or NO,
# return no.  If it is an end of file, exit manualreply.
# Otherwise redisplay with an error message saying the
# answer could not be understood.  Upon return, window_
# error is set to "".
#
proc yes? {} {

    global window_error

    while { "yes" } {
    	set answer [string trim [gets stdin]]
	if { [eof stdin] } {
	    puts "Exiting manualreply"
	    exit_cleanup
	    exit 0
	}
        set window_error ""
	switch -- $answer {
	    y	-
	    yes	-
	    Y	-
	    YES	{ return yes }
	    n	-
	    no	-
	    N	-
	    NO	{ return no }
	}
	if { $answer == "" } {
	    set window_error "ERROR: empty input"
	} else {
	    set window_error \
		    "ERROR: could not understand\
		    	    `$answer'"
	}
	display_window
    }
}


# Print `\nType ENTER or RETURN to continue ', and then
# wait for any line to be input.  When a line is input,
# return, but if an end of file is input, exit
# manualreply.
#
proc continue? {} {

    puts -nonewline \
         "\nType ENTER or RETURN to continue "
    flush stdout
    gets stdin
    if { [eof stdin] } {
	puts "Exiting manualreply"
	exit_cleanup
	exit 0
    }
}
 
# If the character string argument is not empty, print
# it (with appended newline), and wait for ENTER or
# RETURN to be typed by the user before continuing.
#
proc out_check { out } {
    if { $out == "" } return
    puts $out
    continue?
}


# Locking Functions
# ------- ---------

# Lock current directory with $dispatch_pid_file.  If
# $dispatch_pid_file already exists, let our user
# take corrective action.
#
proc get_lock {} {

    global dispatch_pid_file window_error window_bar \
    	   window_prompt


    while { "yes" } {

	if { [dispatch_lock] == "yes" } break

	set time [expr { [clock seconds] - \
			 [file mtime \
			       $dispatch_pid_file] }]

	if { ! [catch {
		   set pid \
		       [read_file \
			   $dispatch_pid_file] }] } {
	    set tree_display \
		[display_process_tree $pid]
	    set tree_display \
		"Process Tree:\n\n$tree_display"
	    set window_error ""
	    set b $window_bar
	    set_window_display "$b\n$tree_display\n$b"
	} else {
	    refresh_file_list
	    set_file_list_display
	    set window_error \
		"ERROR: cannot read $dispatch_pid_file"
	}

	set window_info_height 8
	set_window_info "
$dispatch_pid_file file exists and is $time seconds old.
Maybe `autodispatch' or another `manualreply' is\
					      running.

u = update above info		d = delete\
				    $dispatch_pid_file
k = kill -INT process tree	x = exit this program
m = kill -KILL process tree"

	set window_prompt "> "

	while { "yes" } {
	
	    display_window

	    set answer [string trim [gets stdin]]
	    set window_error ""
	    switch -- $answer {
		u   {   break
		    }
		k   {   catch {
			    signal_process_tree INT \
				[read_file \
				    $dispatch_pid_file]
			} out
			out_check $out
			break
		    }
		m   {   catch {
			    signal_process_tree KILL \
				[read_file \
				    $dispatch_pid_file]
			} out
			out_check $out
			break
		    }
		d   {   file delete -force \
				    $dispatch_pid_file
			break
		    }
		x   {
			exit_cleanup
			exit 0
		    }
		""   {   if { [eof stdin] } {
			    exit_cleanup
			    exit 0
			} else {
			    set window_error \
				"ERROR: empty input"
			}
		    }
		default
		    {
			set window_error \
			    "ERROR: unknown\
			     answer `$answer'!"
		    }
	    }
	}
    }

    # Set exit_cleanup function (called before all exits
    # in judging common code) to unlock current
    # directory.
    #
    proc exit_cleanup {} {
	dispatch_unlock
    }
}

# Unlock current directory.
#
proc clear_lock {} {

    dispatch_unlock
    proc exit_cleanup {} {}
}


# Reply Functions
# ----- ---------

# Compose a reply indicating the assignment of the
# proposed score as the final score, and indicating
# whether this overrides or confirms any previous
# automatically or manually assigned score.  Return
# `yes' if this is a normal transaction and the
# reply does not need human editing.  Return `no' if
# the reply needs to be edited.
#
proc compose_score_reply {} {

    global submitted_file proposed_score \
    	   manual_score auto_score scoring_mode

    if { $manual_score == "none" } {
	if { $scoring_mode == "manual" } {
	    compose_reply "For $submitted_file\
	        your final score is:" \
		"" \
		"    $proposed_score" \
		"" \
		"(see below for submission time)."
	    return yes
	} elseif { [regexp {manual} $scoring_mode] } {
	    if { $proposed_score == $auto_score } {
		compose_reply "For $submitted_file\
		    your previous automatic score of:" \
		    "" \
		    "    $auto_score" \
		    "" \
		    "has been verified and confirmed by\
		     the human judge!" \
		    "Thus your FINAL score is:" \
		    "" \
		    "    $proposed_score" \
		    "" \
		    "for this submission (see below\
		     for submission time)."
		 return yes
	    } else {
		compose_reply "For $submitted_file your\
		               previous automatic score\
			       of:" \
		    "" \
		    "    $auto_score" \
		    "" \
		    "has been OVERRIDDEN by the human\
		     judge!" \
		    "" \
		    "The human judge has assigned the\
		     NEW FINAL score:" \
		    "" \
		    "    $proposed_score" \
		    "" \
		    "to this submission (see below\
		     for submission time)."
		 return yes
	    }
	} elseif { $scoring_mode == "auto" } {
	    if { $proposed_score == $auto_score } {
		compose_reply "For $submitted_file\
		    your PREVIOUS FINAL automatic score\
		    of:" \
		    "" \
		    "    $auto_score" \
		    "" \
		    "has been CONFIRMED by the human\
		     judge!" \
		    "" \
		    "(See below for submission time)."

		 return no
	    } else {
		compose_reply "For $submitted_file\
		    your PREVIOUS FINAL automatic score\
		    of:" \
		    "" \
		    "    $auto_score" \
		    "" \
		    "has been UNEXPECTEDLY OVERRIDDEN\
		     by the human judge!" \
		    "" \
		    "The human judge has assigned the\
		     NEW FINAL SCORE:" \
		    "" \
		    "    $proposed_score" \
		    "" \
		    "(See below for submission time)."
		 return no
	    }
	} else {
	    error "Bad scoring mode: `$scoring mode'"
	}
    } else { # $manual_score != "none"
	if { $proposed_score == $manual_score } {
	    compose_reply "For $submitted_file\
		your PREVIOUS FINAL manual score\
		of:" \
		"" \
		"    $manual_score" \
		"" \
		"has been CONFIRMED by the human\
		 judge!" \
		"" \
		"(See below for submission time)."
	     return no
	} else {
	    compose_reply "For $submitted_file\
		your PREVIOUS FINAL manual score\
		of:" \
		"" \
		"    $manual_score" \
		"" \
		"has been UNEXPECTEDLY OVERRIDDEN\
		 by the human judge!" \
		"" \
		"The human judge has assigned the\
		 NEW FINAL SCORE:" \
		"" \
		"    $proposed_score" \
		"" \
		"(See below for submission time)."
	     return no
	}
    }
}

# Send any previously composed replay and set the
# manual score equal to the proposed score and the
# proposed score to `none'.
#
proc send_score_reply {} {

    global manual_score_file proposed_score \
           manual_score score_flag_file

    send_reply

    write_file $manual_score_file $proposed_score
    set_flag $score_flag_file
    set manual_score $proposed_score
}
