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
#   $Date: 2000/08/21 05:09:29 $
#   $RCSfile: judging_common.tcl,v $
#   $Revision: 1.8 $
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
    eval compose_reply $args
    send_reply
}

# This composes the message to be sent by `reply' in
# the file `${reply_file}+'.  This reply can then be
# manually edited before sending it with `send_reply'.
#
proc compose_reply { args } {

    global received_file reply_file From_line_regexp

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

    set received_ch [open $received_file r]

    set subject ""

    while { "yes" } {
    	set line [gets $received_ch]
	if { [eof $received_ch] } break
	if { [regexp {^Subject:(.*)$} \
	             $line all subject] } break
    }
    close $received_ch

    if { [file exists ${reply_file}+] } {
    	file delete -force ${reply_file}+
    }

    set reply_ch    [open ${reply_file}+ w]
    set received_ch [open $received_file]

    puts $reply_ch   "To: $to"
    puts $reply_ch   "Subject: RE:$subject"
    puts $reply_ch   ""
    foreach line $args {
        puts $reply_ch   $line
    }
    puts $reply_ch   ""
    set dashes \
        "----------------------------------------"
    puts $reply_ch   "$dashes This message replies to:"

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
    }

    close $reply_ch
    close $received_ch
}

# This function renames `${reply_file}+' to
# `$reply_file' and emails this file.
#
proc send_reply {} {

    global reply_file history_file \
           From_line_regexp sendmail_program

    set From_line [file tail [pwd]]
    if { ! [regexp $From_line_regexp $From_line] } {
        error \
	    "Current directory name is not\
	     a mail file `From line':\n\
	     \    $From_line"
    }

    set to [lindex $From_line 1]

    set reply_ch [open ${reply_file}+ r]
    set history_ch  [open $history_file a]

    puts $history_ch "From [id user]@[info hostname]\
		      [clock format [clock seconds]]"

    set header yes
    set to_ok  no
    while { "yes" } {
    	set line [gets $reply_ch]
	if { [eof $reply_ch] } {
	    break
	} elseif { $header && $line == "To: $to" } {
	    set to_ok yes
	} elseif { $header && $to_ok == "no" \
	           && [regexp {^To:} $line] } {
	    puts $history_ch ""
	    puts $history_ch "`To:' line has been\
	                      tampered with"
	    puts $history_ch "THIS MESSAGE WAS NOT\
	                      BEEN SENT!"
	    puts $history_ch ""
	    close $history_ch
	    error "`To:' line has been tampered with\
	           in ${reply_file}+:\n\
	           \    $line"
	} elseif { ! [regexp {:} $line] } {
	    set header no
	}

	puts $history_ch $line
    }

    if { $to_ok == "no" } {
	puts $history_ch \
	    ""
	puts $history_ch \
	    "`To:' line has been tampered with"
	puts $history_ch \
	    "THIS MESSAGE WAS NOT BEEN SENT!"
	puts $history_ch \
	    ""
	close $history_ch
	error "`To:' line has deleted from\
	       ${reply_file}+"
    }

    # A blank line is needed before the next `From'
    # line so the `From' line will be recognized.
    #
    puts $history_ch ""

    close $history_ch

    file rename -force "${reply_file}+" $reply_file

    exec $sendmail_program < $reply_file $to
}

# The following function returns the subject of the
# $received_file in the current directory.  Both the
# `Subject:' and any following whitespace are stripped
# from the result.  If no subject line is found, "" is
# returned.
# 
proc find_subject {} {

    global From_line_regexp received_file

    set subject ""

    set received_ch [open $received_file r]

    while { "yes" } {
        set line [gets $received_ch]
	if { [eof $received_ch] } {
	    break
	} elseif { [regexp \
		"^Subject:\[\ \t\]*(\[^\ \t\].*)\$" \
		$line all subject] } {
	    break
	} elseif { [regexp $From_line_regexp $line] } {
	    # From line is OK
	} elseif { [regexp {:} $line] } {
	    # Non-subject header line is OK
	} else {
	    break
	}
    }
    close $received_file

    return $subject
}

# Find the `From line' as the tail of the current
# directory name.  Call error if this does not have
# the form of a `From line'.
#
proc find_From_line {} {
    global From_line_regexp

    set From_line [file tail [pwd]]

    if { ! [regexp $From_line_regexp $From_line] } {
        error "Current directory name does not end\
	      "in a `From line':\n\
	      \    [pwd]"
    }
    return $From_line
}

# Write entire file to channel.
#
proc putfile { filename
               { ch stdout }
	       { line_count 1000000000 } } {

    set file_ch [open $filename r]
    set count 0

    while { "yes" } {
        set line [gets $file_ch]
	if { [eof $file_ch] } break
	incr count
	if { $count <= $line_count } {
	    puts $ch $line
	}
    }

    close $file_ch
    if { $count > $line_count } {
    	puts $ch "... THERE ARE\
	          [expr { $count - $line_count } ]\
		  MORE LINES ..."
    }
}

# Get first line of file.  If file has no first line,
# return "".
#
proc readfile { filename } {
    set file_ch [open $filename r]
    set line [gets $file_ch]
    if { [eof $file_ch] } {
    	set line ""
    }
    close $file_ch
    return $line
}

# Write one-line file.
#
proc writefile { filename line } {
    set file_ch [open $filename w]
    puts $file_ch $line
    close $file_ch
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
