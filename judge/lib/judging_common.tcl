# Common TCL Code for Judging
#
# File:		judging_common.tcl
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Fri Aug 18 05:21:41 EDT 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: acm-cont $
#   $Date: 2000/08/20 14:26:51 $
#   $RCSfile: judging_common.tcl,v $
#   $Revision: 1.5 $
#

# Include this code in TCL program via:
#
#	set lib_directory \
#	    "[file dirname $argv0]/../lib"
#	source "$lib_directory/judging_common.tcl"
#	catch {
#
#	... your program ...
#	.., (do not change argc, argv0, argv)
#	... terminates with `exit 0' ...
#
#	} caught_output
#	caught_error
#

# The catch and `caught_error' function catches all
# program errors and causes them to be announced on the
# standard output, be logged in the error log file, and
# return an exit code of 1 from the program.

# Default error log file name.  For use if we cannot
# find judging parameters.
#
set default_error_log_file "~/HPCM_Error_Log"

# Judging parameters file name:
#
set judging_parameters_file "hpcm_judging.rc"

# Function called at end of program when a fatal error
# in the program is caught.
#
proc caught_error {} {
    global caught_output
    log_error $caught_output
    exit 1
}

# Function called to log an error when the program
# may want to continue.
#
proc log_error { error_output } {

    global argv0 argv errorCode errorInfo \
	   default_error_log_file \
	   log_directory error_log_file

    puts "ERROR caught for $argv0 $argv"
    puts $error_output

    if { [info exists log_directory] \
         && [info exists error_log_file] \
	 && $log_directory != "" \
	 && $error_log_file != "" } {

        set log_file "$log_directory/$error_log_file"

	if { ! [file writable $log_file] } {
	    if { [catch {
		      file mkdir $log_directory
		  }] \
		 || ! [file writable $log_directory] \
		 || [file exists $log_file] } {
		set log_file $default_error_log_file
	    }
	}
    } else {
    	set log_file $default_error_log_file
    }

    puts "Logging to $log_file"
    set log_ch [open $log_file a]
    puts $log_ch "----------------------------------"
    puts $log_ch "$argv0 $argv"
    puts $log_ch [clock format [clock seconds]]
    puts $log_ch "pwd: [pwd]"
    puts $log_ch ""
    puts $log_ch $error_output
    puts $log_ch ""
    puts $log_ch "errorCode: $errorCode"
    puts $log_ch "errorInfo:"
    puts $log_ch $errorInfo
    close $log_ch
}
    
# Construct a mail reply file and send it to the
# sender of any received mail file.  The option `-all'
# causes the entire received mail file to be included
# immediately at the end of the message.  Otherwise
# just the header is included.  In either case this
# information is separated from the rest of the message
# by a blank space and a line containing `-----'
#
# Non-option arguments are lines to be copied into the
# body of the reply.  The reply header is automatically
# produced.
#
# When the message is sent it is copied to the history
# file.
#
# Any previous reply file is deleted.
#
proc reply { args } {

    global received_file reply_file history_file \
           From_line_regexp sendmail_program

    set all_option no
    if { [llength $args] >= 1 \
         && [lindex $args 0] == "-all" } {
        set all_option yes
	set args [lreplace $args 0 0]
    }

    set From_line [file tail [pwd]]
    if { ! [regexp $From_line_regexp $From_line] } {
        error \
	    "Current directory name is not\
	     a mail file `From line':\n\
	     \    $From_line"
    }

    set to [lindex $From_line 1]

    if { ! [file readable $received_file] } {
        error "Cannot read $received_file"
    }
    set received_ch [open $received_file r]

    set subject ""

    while { "yes" } {
    	set line [gets $received_ch]
	if { [eof $received_ch] } break
	if { [regexp {^Subject:(.*)$} \
	             $line all subject] } break
    }
    close $received_ch

    if { [file exists $history_file] \
         && ! [file writable $history_file] } {
	 error "Cannot write $history_file"
    }

    if { [file exists $reply_file] } {
    	file delete -force $reply_file
    }

    set reply_ch    [open $reply_file w]
    set history_ch  [open $history_file a]
    set received_ch [open $received_file]

    puts $history_ch "From [id user]@[info hostname]\
		      [clock format [clock seconds]]"
    puts $reply_ch   "To: $to"
    puts $history_ch "To: $to"
    puts $reply_ch   "Subject: RE:$subject"
    puts $history_ch "Subject: RE:$subject"
    puts $reply_ch   ""
    puts $history_ch ""
    foreach line $args {
        puts $reply_ch   $line
        puts $history_ch $line
    }
    puts $reply_ch   ""
    puts $history_ch ""
    set dashes \
        "----------------------------------------"
    puts $reply_ch   "$dashes This message replies to:"
    puts $history_ch "$dashes This message replies to:"

    while { "yes" } {
	set line [gets $received_ch]
	if { [eof $received_ch] } {
	    break
	} elseif { [regexp $From_line_regexp \
			   $line] } {
	    set line ">$line"
	} elseif { ( $all_option != "yes" ) \
		   && ! [regexp {:} $line] } {
	    break
	}
	puts $reply_ch  $line
	puts $history_ch $line
    }
    # A blank line is needed before the next `From'
    # line so the `From' line will be recognized.
    #
    puts $history_ch ""

    close $reply_ch
    close $history_ch
    close $received_ch

    exec $sendmail_program < $reply_file $to
}

# Set interrupt signal to cause an error.
#
if { [info command signal] == "signal" } {
    signal error SIGINT
}

# Locate the directory containing the judging
# parameters file.  This should be unique.
#
set judging_parameters_file_list ""
foreach d ". .. ../.. ../../.. ../../../.." {
    if { [file exists \
	       "$d/$judging_parameters_file"] } {
	lappend judging_parameters_file_list \
		"$d/$judging_parameters_file"
	set judging_parameters_directory $d
    }
}

# If directory containing judging parameters file
# is unique, source the file, and set juding_parameters_
# directory to the directory name.  Otherwise call
# error.
#
if { [llength $judging_parameters_file_list] == 1 } {
    if { [file readable \
               $judging_parameters_file_list] } {
	source $judging_parameters_file_list
    } else {
	error "$judging_parameters_file_list\
	       not readable"
    }
} elseif { [llength $judging_parameters_file_list] \
           == 0 } {
    error "$judging_parameters_file not found"
} else {
    error \
	"Too many $judging_parameters_file files:\n\
	\    $judging_parameters_file_list"
}
