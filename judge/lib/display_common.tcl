# Common TCL Code for Displays
#
# File:		display_common.tcl
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Wed Jan 30 01:00:24 EST 2002
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/01/30 06:00:50 $
#   $RCSfile: display_common.tcl,v $
#   $Revision: 1.28 $
#
#
# Note: An earlier version of this code used to be in
# manualreply.

# Table of Contents
#
#	Including this Code
#	Display
#	Response Functions
#	Locking Functions
#	File List
#	File Creating
#	File Cacheing
#	Displaying Files
#	Query Functions

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
# Of the following variables, window_error and window_
# prompt can be set directly, while the other variables
# should be set by the set_window_display and set_
# window_info functions.
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


# Constant for use in functions.
#
set window_newlines "\n\n\n\n\n\n\n\n\n\n"
set window_newlines "$window_newlines$window_newlines"
set window_newlines "$window_newlines$window_newlines"
set window_newlines "$window_newlines$window_newlines"
set window_newlines "$window_newlines$window_newlines"

# Display the current window.
#
# By accumulating text and writing it in a single puts
# statement, and by designing successive puts strings to
# have the same layout, we get the effect of having a
# single window.
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

# Set the window_info variable to the info part of the
# next window to be displayed.  Add blank lines to the
# top of the given `info' to make it have window_info_
# height lines, or call error if the `info' has more
# than window_info_height lines.
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

# Response Functions
# -------- ---------

# Wait for an answer line to be input.  If it is y, Y,
# yes, or YES, return `yes'.  If it is n, N, no, or NO,
# return no.  If it is an end of file, exit program.
# Otherwise redisplay with an error message saying the
# answer could not be understood.  Upon return, window_
# error is set to "".
#
proc yes? {} {

    global window_error argv0

    while { "yes" } {
    	set answer [string trim [gets stdin]]
	if { [eof stdin] } {
	    puts "\nExiting $argv0"
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
# program.
#
proc continue? {} {

    global argv0

    puts -nonewline \
         "\nType ENTER or RETURN to continue "
    flush stdout
    gets stdin
    if { [eof stdin] } {
	puts "\nExiting $argv0"
	exit_cleanup
	exit 0
    }
}
 
# If the character string argument is not empty, print
# it (with appended newline), and call `continue?'.
#
proc out_check { out } {
    if { $out == "" } return
    puts $out
    continue?
}


# Locking Functions
# ------- ---------

# Lock current directory with $dispatch_pid_file.  If
# $dispatch_pid_file already exists, communicate with
# the user through the window an let the user take
# corrective action.
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


# File List
# ---- ----
#
# Data and functions that maintain the list of files
# to be displayed.  Usually these files are in the
# current directory.

# List of files that are currently displayable.  This
# list is stored in the global `file_list' variable.
# The N'th item on the list is itself a list of sub-
# items with the format:
#
#	ctime mtime filename comment new
#
# The total list is sorted by ctime.  Ctime equals
# mtime, the file's modification time, with 0's prepend-
# ed so all ctimes are the same length and alphanumeric
# comparison of items will be the same as numeric com-
# parison of mtimes.  If a file does not exists (e.g.,
# .diff files for which .out and .test files exist),
# ctime and mtime are `TBD', and items with this ctime
# get sorted to the end of the list.
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
# The variable `file_list_origin_mtime' holds the mtime
# that is to be used as the origin when displaying the
# listed files.
#
# The file list is built by calling `get_listed_files'
# and using the result of that and the value of the
# previous `file_list' variable to compute a new
# `file_list' variable value.  See `get_listed_files'
# above.
#
# This function implements file cacheing (see File
# Cacheing below) as follows.  When building the file
# list, the information read by the read_array functions
# is refreshed as necessary, using corresponding mtime_
# array information where mtime_array(filename) is the
# mtime of the file just before read_array(filename) was
# last called, and mtime_array(filename) does not exist
# if the file does not exist.  If a file is not in the
# new list but has an mtime_array entry, this entry is
# deleted, and the file's read_array function is called.
#
set file_list ""
set file_list_origin_mtime 0

# Compute the names of the files to be displayed.  This
# default function can be overridden by redefining it
# in any program.
#
# This default function returns files in the current
# directory, adding an xxx.diff and xxx.bdiff file
# if these do not already exist for every pair of
# files xxx.out xxx.test that do exist, provided the
# current directory is writable.
#
# This function also computes file_list_mtime_origin
# if it can.  This default function sets this to the
# mtime of $received_file if that file exists.
#
proc get_listed_files { } {

    global file_list_origin_mtime received_file

    set listed_files [glob -nocomplain *]

    # Compute names of non-existent files to be added
    # to the list because they can be made on demand.

    set extra_files ""
    if { [file writable "."] } {
	foreach file $listed_files {
	    if { [regexp {^(.*)\.out$} $file \
				       forget base] } {
		if { [lsearch -exact $listed_files \
				     $base.test] >= 0 \
				     } {
		    if { [lsearch -exact $listed_files \
				  $base.diff] < 0 } {
			lappend extra_files $base.diff
		    }
		    if { [lsearch -exact $listed_files \
				  $base.bdiff] < 0 } {
			lappend extra_files $base.bdiff
		    }
		}
	    }
	}
    }

    # Comupte mtime if $received_file exists.

    if { [lsearch -exact $listed_files $received_file] \
         >= 0 } {
	set file_list_origin_mtime \
	    [file mtime $received_file]
    }

    return [concat $listed_files $extra_files]
}

# Return item of file with a given name in the file
# list, or return "" if file is not in the list.
#
proc get_file_item { filename } {

    global file_list

    if { $file_list == "" } {
    	refresh_file_list
    }

    foreach item $file_list {
        if { [lindex $item 2] == $filename } {
	    return $item
	}
    }

    return ""
}

# Return list of all the items whose file names match
# a given regexp.  The regexp must match the entire
# filename (a ^ and $ will be added to the regexp by
# this function).
#
proc get_file_items { regexp } {

    global file_list

    if { $file_list == "" } {
    	refresh_file_list
    }

    set result ""

    foreach item $file_list {
        if { [regexp "^$regexp\$" [lindex $item 2]] } {
	    lappend result $item
	}
    }

    return $result
}

# Recompute the file list.  New files are marked unless
# the file list was previously empty.
#
proc refresh_file_list { } {

    global file_list file_list_origin_mtime \
	   read_array mtime_array

    set file_list_origin_mtime ""
    set new_file_list ""
    set newest 0
    foreach file [get_listed_files] {

        # Set ctime, mtime, and filename of item.
	#
	# Beware of file being link, in which case
	# mtime does not work.
	#
	if { [catch { set mtime [file mtime $file] }] \
		} {
	    if { ! [catch { file lstat $file temp }] } {
		set mtime $temp(mtime)
	    } else {
	        set mtime TBD
	    }
	}

	if { $mtime == "TBD" } {
	    set item TBD
	} else {
	    set item [format {%025d} $mtime]
	    if { $mtime > $newest } {
		set newest $mtime
	    }
	}
	lappend item $mtime
	lappend item $file

	# Set comment of item.
	#
	
	if { $mtime == "TBD" } {
	    lappend item "(TBD)"
	} elseif { ! [file exists $file] } {
	    lappend item "(dangling link)"
	} elseif { [file isdirectory $file] } {
	    lappend item "(directory)"
	} elseif { [set size [file size $file]] == 0 } {
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
	if { $file_list != "" \
	     && [get_file_item $file] == "" } {
	    set new "*"
	}
	lappend item $new

	# Add item to new file list.
	#
	lappend new_file_list $item

	# Process file if it is one from which we
	# read information and it has changed since
	# last call to refresh_file_list.
	#
	if { [info exists read_array($file)] \
	     && $mtime != "TBD" \
	     && ( ! [info exists mtime_array($file)] \
	          || $mtime_array($file) < $mtime ) } {
	    set mtime_array($file) $mtime
	    eval $read_array($file)
	}
    }

    # Set file list global data to newly computed file
    # list.
    #
    set file_list [lsort $new_file_list]

    if { $file_list_origin_mtime == "" } {
    	set file_list_origin_mtime $newest
    }

    # Process any files from which we read information
    # which have been deleted.
    #
    foreach file [array names mtime_array] {
        if { [set item [get_file_item $file]] == ""
	     ||
	     [lindex $item 0] == "TBD" } {
	    # File has been deleted.
	    unset mtime_array($file)
	    eval $read_array($file)
	}
    }
}

# Given a file id, set `last_file' to the name of
# the corresponding file, set `window_error' to "", and
# return `yes'.  If there is an error, leave `last_file'
# alone, set `window_error' to the error description,
# and return `no'.
#
# The file id may be a number in the file list, may be
# a file extension, or if it begins with an upper case
# letter, may be an initial segment of the file name.
# It is an error if it is ambiguous.
#
# This function implements file creation (see File
# Creation below) as follows.  Whenever `last_file' is
# changed, if the new name is of the form xxx.ext for
# some extension ext, and the array entry make_file_
# array(.ext) exists, the value of that array entry is
# called with the file name as argument to make or
# update the new `last_file' file.  The called procedure
# may not succeed: its perfectly possible to set
# `last_file' to a non-existant file.
#
proc get_file { id } {

    global file_list \
	   last_file window_error make_file_array

    set window_error ""

    if { $id == "" } {
	if { $last_file == "" } {
	    set window_error "No previous file!"
	    return no
	} else {
	    return yes
	}
    }

    if { [regexp {^[0-9]+$} $id] } {
        if { $id < 1 || $id > [llength $file_list] } {
	    set window_error "Bad file number: $id"
	    return no
	} else {
	    set last_file \
		[lindex [lindex $file_list \
                            [expr { $id - 1 }]] 2]
	}
    } else {
        set found ""
	foreach item $file_list {
	    set filename [lindex $item 2]
	    if { ( [regexp {^\.} $id] \
	    	   && \
	           [regexp "\\$id\$" $filename] ) \
	         || \
		 ( [regexp {^[A-Z]} $id] \
		   && \
		   [regexp "^$id" $filename] ) \
		 || \
		 $id == $filename } { \
	        lappend found $filename
	    }
	}

	set l [llength $found]
	if { $l == 0 } {
	    set window_error \
	        "Bad file extension, beginning, or\
		 name: $id"
	    return no
	} elseif { $l == 1 } {
	    set last_file $found
	} else {
	    set window_error \
	        "Ambiguous file extension, beginning\
		 or name: $id"
	    return no
	}
    }

    set extension [file extension $last_file]
    if { [info exists \
	       make_file_array($extension)] } {
	eval [list $make_file_array($extension) \
	           $last_file]
    }
    return yes
}

# File Cacheing
# ---- --------
#
# Following data and functions copy information from
# files in the display `file_list' into variables within
# this program whenever the files change.  These func-
# tions are called by the refresh_file_list function
# above when the file_list is recomputed.

# Functions to read information from files.  There is
# one of these per file that can be read.  These
# functions must take no arguments; they are called
# when it is discovered their file has been modified.
#
# For each of the below files, read_array(filename) is
# set to the name of the function that reads the file
#
# These files are actually read by refresh_file_list
# (see File List above).  This sets mtime_array(
# filename) to the mtime of the file just before it is
# read, if the file exists.  If the file does not exist,
# mtime_array(filename) must not exist.
#
# The variables must be initialized to their settings
# when the files do not exist.

# Read a 1-line file and return its line.  Check for
# the file being non-existing, unreadable, and empty,
# and in these cases return `None', `UNREADABLE', and
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
	return None
    }
}


# Read $auto_score_file
#
proc read_auto_score {} {

    global auto_score_file auto_score

    set auto_score [read_score $auto_score_file]
}
set read_array($auto_score_file) read_auto_score
set auto_score None

# Read $manual_score_file
#
proc read_manual_score {} {

    global manual_score_file manual_score

    set manual_score [read_score $manual_score_file]
}
set read_array($manual_score_file) read_manual_score
set manual_score None

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
# problem, and extension variables.  If the subject is
# inappropriate to a submission, set these variables to
# "".
#
proc read_received_file {} {

    global received_file message_subject \
	   submitted_problem submitted_extension

    if { ! [file readable $received_file] } {
	set submitted_problem   ""
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
	set submitted_name \
	    [lindex $message_subject 1]
	set submitted_problem \
	    [file rootname $submitted_name]
	set submitted_extension \
	    [file extension $submitted_name]
    } else {
	set submitted_problem   ""
	set submitted_extension ""
    }
}
set read_array($received_file) read_received_file
set submitted_problem   ""
set submitted_extension ""


# File Creation
# ---- --------
#
# Following data and functions create files that are
# in the file list but may not exist yet or may be out
# of date.  This file creation is done by the get_file
# function when the user designates a specific file to
# be examined.

# If a file with name of the form xxx.ext for some
# extension ext is to be displayed, and the array
# entry make_file_array(.ext) exists, the value of that
# array entry is called with the file name as argument
# to make or update the file.  The called procedure need
# not succeed: the file need not exist after the
# procedure finishes.

set make_file_array(.diff) make_diff
set make_file_array(.bdiff) make_diff

# Procedure to make xxx.diff or xxx.bdiff if possible.
#
proc make_diff { file } {

    set base [file rootname $file]

    set diff_file $file
    set out_file  $base.out
    set test_file $base.test

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

    if { [file extension $file] == ".diff" } {
	set options ""
    } else {
	set options "-b"
    }

    diff_files $out_file $test_file $diff_file $options
}

# Displaying Files
# ---------- -----

# Set the window display to the file_list.  Set the
# `last_display' variable to `file_list'.
#
proc set_file_list_display {} {

    global file_list file_list_origin_mtime \
    	   window_bar last_display

    set display "$window_bar"

    set n 0
    set previous ""

    # If `previous' is non-empty it is the previous
    # item and has no more than 39 characters.
    #
    foreach item $file_list {
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

# Set the window display to display the first lines
# of the file.  Set the `last_display' variable to
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

# Function that reads a file into an array.  When first
# called for the file, this function opens the file and
# reads lines of the file into the array until a given
# line number is reached.  On subsequent calls, if a
# larger line number is given, this function reads more
# of the file into the array, until the given line
# number is reached.  Lines are numbered 1, 2, ..., and
# if the array is named x_array, then x_array(1) is the
# 1'st line, x_array(2) the 2'nd line, etc.
#
# Note that reading stops on an end of file even if
# the given line number has not been reached.  In this
# case, array elements for lines off the end of the
# file will not exist.
#
proc read_file_array { filename xxx_array linenumber } {

    upvar $xxx_array array

    if { ! [info exists array(ch)] } {

        set ch [open $filename r]
	set lastline 0

	set array(filename) $filename
	set array(ch) $ch
	set array(lastline) $lastline

    } else {

        set ch $array(ch)
        set lastline $array(lastline)

    }

    if { $ch == "CLOSED" } return

    while { $lastline < $linenumber } {
	set line [gets $ch]
	if { [eof $ch] } {
	    close $ch
	    set array(ch) CLOSED
	    break
	}
        incr lastline
	set array($lastline) $line
    }

    set array(lastline) $lastline
}

# Delete an array used to store lines of a file.  This
# function closes any open channel kept by the array.
#
proc close_file_array { xxx_array } {

    upvar $xxx_array array

    if { [info exists array(ch)] } {
    	set ch $array(ch)
	if { $ch != "CLOSED"} {
	    close $ch
	}

	foreach n [array names array] {
	    unset array($n)
	}
    }
}


# Function to return a line with tabs expanded into
# spaces.
#
proc tab_expand { line } {
    set segments [split $line "\t"]
    set output ""
    set column -8
    set blanks "        "
    foreach segment $segments {
        if { $column == -8 } {
	    set column 0
	} else {
	    set b [expr { 8 - $column % 8 }]
	    incr column $b
	    incr b -1
	    set output \
	        "$output[string range $blanks 0 $b]"
	}
	set output "$output$segment"
        incr column [string length $segment]
    }

    return $output
}

# Function to compute a display of N lines of a file
# with some text highlighted.  The display is returned.
# The first line consts of a bar (===) with the file
# name and line numbers being displayed.  The highlights
# argument is a list of items of the form
#
#	line-number first-column last-column
#
# which must be in sorted order.  The file will be
# read into the array with the read_file_array
# function.
#
# The returned display consists of lines each ending
# with \n.  There is NO ending bar line: the last
# line returned is a file line.
#
# If the file ends before the last line number, the
# first returned line after the last real file line is:
#
#	---------- END OF FILE ----------
#
# and the remaining returned lines are blank.
#
proc compute_file_display \
	{ filename xxx_array first_line_number \
	                     last_line_number \
			     highlights } {

    upvar $xxx_array array

    global highlight_on highlight_off

    set hon $highlight_on
    set hoff $highlight_off
    set hlength [string length "$hon$hoff"]

    read_file_array $filename array $last_line_number

    set output [bar_with_text "$filename: lines\
    		$first_line_number-$last_line_number:"]
    set output "$output\n"

    set i $first_line_number
    set eofyet no

    while { "yes" } {
	set j [lindex [lindex $highlights 0] 0]
        if { $j == "" || $j >= $i } break
	set highlights [lrange $highlights 1 end]
    }

    while { $i <= $last_line_number } {

        if { ! [info exists array($i)] } {
	    if { $eofyet } {
	        set line ""
	    } else {
	        set eofyet yes
	        set line "----------\
			  END OF FILE\
			  ----------"
		if { [llength $highlights] > 0 } {
		    set line "$hon$line$hoff"
		}
	    }
	} else {

	    set line $array($i)
	    if { $j == $i } {
	        set line [tab_expand $line]
		set k 0
		while { "yes" } {
		    set h [lindex $highlights 0]
		    set c1 [lindex $h 1]
		    set c2 [lindex $h 2]
		    incr c1 $k
		    incr c2 $k
		    incr k $hlength
		    set l1 [string range $line 0 \
				 [expr { $c1 - 1 }]]
		    set l2 [string range $line $c1 $c2]
		    set l3 [string range $line \
				 [expr { $c2 + 1 }] end]
		    set line "$l1$hon$l2$hoff$l3"

		    set highlights \
		        [lrange $highlights 1 end]
		    set j [lindex [lindex $highlights \
		                          0] 0]
		    if { $j == "" || $j != $i } break
		}
	    }
	}
	set output "$output$line\n"
	incr i
    }

    return $output
}


# Query Functions
# ----- ---------

# Create and mail a query to the judge.  Part of the
# header is given as a single argument, which may
# include several lines, all but the last ending in
# a line feed.
#
proc send_query { header } {

    global window_error window_prompt \
    	   contest_directory

    set i 0
    while { "yes" } {
	set filename "/tmp/manualreply-$i.email"
        if { [create_file $filename] } break
	incr i
    }
    set ch [open $filename w]
    puts -nonewline $ch "$header\n\nEDIT THIS BODY"
    close $ch

    while { "yes" } {
	edit_file $filename
	set_file_display $filename

	set window_error ""

	set window_prompt \
	    "Do you want to send this to the judge? > "

	display_window

	if { [yes?] } {
	    if { [catch {
	              exec hpcm_sendmail < $filename } \
		  out] } {
		puts "ERROR sending to judge:"
		puts $out
		continue?
		set window_error "Message aborted!"
	    } else {
		set window_error \
		    "Message sent to judge!"
	    }
	    break
	} else {

	    set window_prompt \
		"Do you want to send edit it some\
		 more? > "

	    display_window

	    if { [yes?] == "no" } {
		set window_error "Message aborted!"
		break
	    }
	}
    }

    file delete -force $filename
}
