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
#   $Date: 2000/08/25 17:28:13 $
#   $RCSfile: judging_common.tcl,v $
#   $Revision: 1.16 $
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

# Default error log directory name.  For use if we
# cannot find judging parameters.
#
set default_log_directory "~/HPCM_Error_Log"

# Judging parameters file name:
#
set judging_parameters_file "hpcm_judging.rc"

# Replace all line feeds in a string by spaces, then
# replace any all whitespace string by the empty string,
# then strip all whitespace from the beginning and end
# of the string, and lastly return the resulting string.
#
proc prepare_field_value { value } {

    regsub -all "\n" $value "\ " value

    regexp "^()\[\ \t\]+\$" $value \
           forget value

    regexp "^(.*\[^\ \t\])\[\ \t\]+\$" $value \
           forget value
    regexp "^\[\ \t\]+(\[^\ \t\].*)\$" $value \
           forget value

    return value


# Convert a [clock seconds] value into a date in
# the form yyyy-mm-dd-hh:mm:ss that is useable as
# part of a filename.
#
proc clock_to_filename_date { clock } {
    return [clock format $clock \
                  -format {%Y-%m-%d-%H:%M:%S}]
}

# Do the reverse conversion to that of the above
# function.
#
proc filename_date_to_clock { date } {
    set n {([0-9]+)}
    if { ! [regexp "^$n-$n-$n-$n:$n:$n\$" $date all \
    	           year month day \
		   hour minute second] } {
	error "Not a legal filename date:    $date"
    }
    return [clock scan \
	    "$month/$day/$year $hour:$minute:$second"]
}

# Convert a `From' line from a mail message into a
# directory name of the form dddd-[<SU>]-{<ssss>}
# where dddd is the date in filename format and ssss
# is the sender field of the `From' line.
#
proc From_line_to_dirname { From_line } {

    global From_line_regexp

    if { ! [regexp $From_line_regexp \
                   $From_line all sender date] } {
	error "Not a legal mail file `From' line:\n\
	      \    $From_line"
    }

    set d [clock_to_filename_date [clock scan $date]]
    return "$d-\[<SU>\]-{<$sender>}"
}

# Do the reverse conversion to that of the above
# function.
#
proc dirname_to_From_line { dirname } {

    if { ! [regexp {^(.*)-[<S.>]-{<(.*)>}$} $dirname \
                   all date sender] } {
        error \
	    "Directory name is not encoded version\
	     of mail file `From line':\n\
	     \    $dirname"
    }

    return "From $sender \
            [clock format \
	           [filename_date_to_clock $date]]"
}

# Extract the sender from a directory name that encodes
# a `From' line.
#
proc dirname_to_sender { dirname } {

    if { ! [regexp {^(.*)-[<S.>]-{<(.*)>}$} $dirname \
                   all date sender] } {
        error \
	    "Directory name is not encoded version\
	     of mail file `From line':\n\
	     \    $dirname"
    }

    return $sender
}

# Returns 1 iff the current directory is checkeed, 0 if
# it is unchecked, and calls error if the directory has
# no check mark.
#
proc is_checked {} {
    set old_dir [file tail [pwd]]
    if { ! [regexp {^(.*{<[^>]*)([UC])(>}.*)$} $old_dir a b c] } {
	error "$old_dir does not include a checkmark"
    }
    return [expr { $b == "C" }]
    }
}

# Function to change the name of the current directory
# from ...{<...U>}... to ...{<...C>}...; that is, the U
# meaning `unchecked' is changed to C meaning `checked'.
#
proc make_checked {} {
    set old_dir [file tail [pwd]]
    if { ! [regexp {^(.*{<[^>]*)([UC])(>}.*)$} $old_dir a b c] } {
	error "$old_dir does not include a checkmark"
    }
    if { $b != "C" } {
	file rename "../$old_dir" "../${a}C${c}"
    }
}

# Function to change the name of the current directory
# from ...{<...C>}... to ...{<...U>}...; that is, the C
# meaning `checked' is changed to U meaning `unchecked'.
#
proc make_unchecked {} {
    set old_dir [file tail [pwd]]
    if { ! [regexp {^(.*{<[^>]*)([UC])(>}.*)$} $old_dir a b c] } {
	error "$old_dir does not include a checkmark"
    }
    if { $b != "U" } {
	file rename "../$old_dir" "../${a}U${c}"
    }
}

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
# The error information is written as a separate file.
# If the log_globally variable does not exist or does
# not equal `yes' and the current directory is writable,
# the file is written into the current directory.
# Otherwise if the log_directory variable exists and
# names a directory that exists or can be made and is
# writable once it is made, then the file is written
# into this directory.  Otherwise the file is written
# into $default_log_directory, which is defined above
# as an emergency last resort.  The format of the file
# name is:
#
#	d-[<EU>]-{<p>}-u
#
# where d = is the date in filename date format
#       u = random 6 digit number for uniqueness
#       p = name of executing program
#
# EU means the file is for an Error that is Unchecked,
# i.e., not yet seen by a person.  This part of the
# file name may be change to EC when a person checks off
# on the error.
#
proc log_error { error_output } {

    global argv0 argv errorCode errorInfo \
	   default_log_directory \
	   log_directory

    # Write error to standard output.
    #
    puts "ERROR caught for $argv0 $argv"
    puts $error_output

    # Compute $log_dir, the logging directory
    # to be used.  Make it if necessary.  Be
    # sure its writable.
    #
    if { ( ! [info exists log_globally] \
           ||  $log_globally != "yes" ) \
	 && [file writable "."] } {
        set log_dir "."
    } elseif { [info exists log_directory] \
	       && ! [catch { file mkdir \
	                          $log_directory } ] \
	       && [file writable $log_directory] } {
    	set log_dir $log_directory
    } else {
	file mkdir $default_log_directory
    	set log_dir $default_log_directory
    }

    # Create $log_file filename for output.
    #
    set count 0
    while { "yes" } {
        set t [clock_to_filename_date [clock seconds]]
        set u [format %06d \
		  [expr { [clock clicks] % 1000000 } ]]
        set p [file tail $argv0]
        set log_file \
	    "$log_dir/${t}-\[<EU>\]-{<${p}>}-${u}"

	if { ! [catch { create_file $log_file } ] } {
	    break
	}

	incr count

	if { $count > 10 } {

	    # Desparation move.  Should never happen.
	    #
	    set e LOGGING-FILENAME-GENERATION-ERROR
	    set log_file "$log_dir/${e}-\[<EU>\]"
	    break
	}
    }

    # Write error to $log_file file.
    #
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

# Read an email message header from the channel. If given, the
# first line of the header is the second argument (note that email
# header lines cannot be empty.  Stop reading at the first empty
# line and discard that line.
#
# The results are returned in global variables:
#
#	message_header		All the lines of the header
#	message_From_line	The first line if it begins with
#				`^From\ '.
#	message_from		The `From:' field value.
#	message_to		The `To:' field value.
#	message_reply_to	The `Reply-To:' field value.
#	message_subject		The `Subject:' field value.
#	message_date		The `Date:' field value.
#
# All the values have the final \n stripped off.  All the field
# values have the `field-name:' stripped off.  If there are
# two copies of a field, only the last is recorded.
#
proc read_header { ch { first_line "" } } {
    global message_header message_From_line \
           message_from message_to message_date \
	   message_reply_to message_subject

    set message_header		""
    set message_From_line	""
    set message_from		""
    set message_to		""
    set message_date		""
    set message_reply_to	""
    set message_subject		""

    set line $first_line
    if { $line == "" } {
        set line [gets $ch]
	if { [eof $ch] } return
    }

    if { [regexp "^From\ " $line } {
    	set message_From_line $line
    }

    while { "yes" } {
    	if { $line == "" } break

	set found_field no
	foreach fieldname \
	        "to subject from date reply-to" {
	    if { [regexp -nocase \
	                 "^${fieldname}:(.*)\$"
			 wholeline fieldvalue] } {
		set varname "message_$fieldname"
		subexp -all -- "-" $varname "_" varname
		set $varname $fieldvalue

		while { "yes" } {
		    set line [gets $ch]
		    if { [eof $ch] \\
			 || ! [regexp "^\ \t" $line] } \
			break;
		    set $varname \
			"[set $varname]\n$line"
		    set message_header \
			"$message_header\n$line"
		}
		set found_field yes
		break
	    }
	}

	if { $found_field == "no" } {
	    set message_header "$message_header\n$line"
	    set line [gets $ch]
	}

	if { [eof $ch] break }
    }
}

# Using information from the last call to read_header,
# return the `To:' field value for a reply to that
# message.  This is the message `Reply-to' field if that
# is not empty, or the message `From' filed if that is
# not empty, or `UNKNOWN' as a last resort.
#
proc compute_message_reply_to {} {

    global message_from message_reply_to

    if { $message_reply_to != "" } {
	return $message_reply_to
    } elseif { $message_reply_from != "" } {
	return $message_reply_from
    } else {
    	return "UNKNOWN"
    }
}
    
# Construct a mail reply file and send it to the sender
# of any received mail file.  The From, To, Subject, and
# Date fields of the received mail Header are copied at
# the end of the message.  The option `-all' causes the
# body of the received mail file to be included.  In
# any case this information is separated from the rest
# of the message by a blank space and a line containing
# `-----'
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

    global received_file reply_file \
           message_From_line \
           message_from message_to message_date \
	   message_reply_to message_subject

    # If -all is present, set `all_option' to `yes' and
    # shift the arguments left.
    #
    set all_option no
    if { [llength $args] >= 1 \
         && [lindex $args 0] == "-all" } {
        set all_option yes
	set args [lreplace $args 0 0]
    }

    set received_ch [open $received_file r]

    read_header $received_ch

    if { [file exists ${reply_file}+] } {
    	file delete -force ${reply_file}+
    }

    set reply_ch    [open ${reply_file}+ w]

    puts $reply_ch   "To:[compute_message_reply_to]"
    puts $reply_ch   "Subject: RE:$message_subject"
    puts $reply_ch   ""
    foreach line $args {
        puts $reply_ch   $line
    }
    puts $reply_ch   ""
    puts $reply_ch   "------------------------------\
                      This message replies to:"

    set From_line_sent no

    if { [regexp "\[^\ \t\n\]" $message_to]  } {
	puts $reply_ch "To:$message_to"
    }

    if { [regexp "\[^\ \t\n\]" $message_from]  } {
	puts $reply_ch "From:$message_from"
    } else if { $message_From_line != "" } {
	puts $reply_ch ">$message_From_line
	set From_line_sent yes
    }

    if { [regexp "\[^\ \t\n\]" $message_date]  } {
	puts $reply_ch "Date:$message_date"
    } else if { $From_line_sent == "no" \
                && $message_From_line != "" } {
	puts $reply_ch ">$message_From_line
	set From_line_sent yes
    }

    if { [regexp "\[^\ \t\n\]" $message_reply_to]  } {
	puts $reply_ch "Subject:$message_reply_to"
    }

    if { [regexp "\[^\ \t\n\]" $message_subject]  } {
	puts $reply_ch "Subject:$message_subject"
    }

    puts $reply_ch ""

    while { "yes" } {
	set line [gets $received_ch]
	if { [eof $received_ch] } {
	    break
	} else {
	    puts $reply_ch "$line"
	}
    }

    close $reply_ch
    close $received_ch
}

# This function renames `${reply_file}+' to
# `$reply_file' and emails this file.
#
proc send_reply {} {

    global reply_file history_file

    set history_ch  [open $history_file a]

    puts $history_ch "From [id user]@[info hostname]\
		      [clock format [clock seconds]]"

    put_file "${reply_file}+" $history_ch

    # A blank line is needed before the next `From'
    # line so the `From' line will be recognized.
    #
    puts $history_ch ""

    close $history_ch

    file rename -force "${reply_file}+" $reply_file

    # Send the reply_file.  If there is a bad `To:'
    # address, there will be an error, which will
    # usually be logged locally.
    #
    send_mail $reply_file
}

# The following function returns the subject of the
# $received_file in the current directory.  Both the
# `Subject:' and any preceding or following whitespace
# are stripped from the result, and any line feeds
# are placed by spaces.
#
# If no `Subject:' line is found in the header, or if
# the -nobody argument is given and the body contains a
# non-blank line, then error is called with an appropri-
# ate error message.
#
# Read_header is called and its global message_...
# variables are set.
#
proc find_subject { { option -body } } {

    global received_file message_subject

    if { $option == "-body" } {
	set body yes
    } elseif { $option == "-nobody" } {
	set body no
    } else {
        error "Bad argument to find_subject: $option"
    }

    set received_ch [open $received_file r]

    read_header $received_ch

    set subject [prepare_field_value $message_subject]

    if { $subject == "" } {
    	error "Empty subject"
    }

    if { $body == "no" } {

	# Check that body is blank.
	#
	while { "yes" } {
	    if { [eof $received_ch] } {
		break
	    } elseif { [regexp "^\[\ \t\]*\$" $line] } {
		# blank lines are ok
	    } else {
		error "Non-blank body line:\n\
		      \    $line"
	    }
	    set line [gets $received_ch]
	}
    }

    close $received_ch

    return $subject
}

# Find the scoring instructions in the $scoring_
# instructions file or in the $default_scoring_
# instructions.
#
proc find_scoring_instructions {} {
    global scoring_instructions_file \
	   scoring_instructions_default

    if { ! [file exists $scoring_instructions_file] } {
	return $scoring_instructions_default
    } else {
        return [read_file $scoring_instructions_file]
    }
}

# Write entire file to channel.
#
proc put_file { filename
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
proc read_file { filename } {
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
proc write_file { filename line } {
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
foreach __d__ ". .. ../.. ../../.. ../../../.." {
    if { [file exists \
	       "$__d__/$judging_parameters_file"] } {
	lappend judging_parameters_file_list \
		"$__d__/$judging_parameters_file"
	set judging_parameters_directory $__d__
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
