# Common TCL Code for Judging
#
# File:		judging_common.tcl
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Tue Apr  2 10:05:55 EDT 2013
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2013/04/03 00:56:28 $
#   $RCSfile: judging_common.tcl,v $
#   $Revision: 1.161 $
#

# Table of Contents
#
#	Including this Code
#	List Functions
#	Documentation Functions
#	Dispatch Locking Functions
#	Date Functions
#	Checked File Functions
#	Error Logging Functions
#	Message Header Functions
#	Reply Functions
#	File Functions
#	Flag Functions
#	Make Functions
#	Logical Expression Compilation
#	Parse Functions
#	Inline Code


# Including this Code
# --------- ---- ----


# Include this code in TCL program via:
#
#	set lib_directory \
#	    "[file dirname $argv0]/../lib"
#	source $lib_directory/judging_common.tcl
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
# Put the command:
#
#	set log_globally yes
#
# in front of
#
#	source $lib_directory/judging_common.tcl
#
# if you want errors to be logged in the log directory
# instead of the current directory.  Put
#
#	set log_disable yes
#
# if you do NOT want any errors written to log files.
# Put
#
#	set log_quiet yes
#
# if you do NOT want any errors written to stderr.
# See `log_error' below.
#
# Put the command
#
#	set judging_directory "SOME-DIRECTORY"
#
# in front of
#
#	source $lib_directory/judging_common.tcl
#
# if the current program is to pretend that SOME-
# DIRECTORY is the judging directory and that there is
# a link from SOME-DIRECTORY/hpcm_judging.rc to
#
#	$lib_directory/hpcm_judging.rc
#
# Usually SOME-DIRECTORY is '.', and log_disable is set
# to `yes'.
#
# The catch and `caught_error' function catches all
# program errors and causes them to be announced on the
# standard error output and be logged according to the
# log_disable, log_globally, and log_mode settings.
# This can cause errors to be recorded in an error log
# file and to generate email notification.

# Default error log directory name.  For use if we
# cannot find judging parameters.
#
set default_log_directory $env(HOME)/HPCM_Error_Log

# Hook to retry when a caught_error happens.  The
# caught_error function calls this first, then logs
# the error, then calls exit_cleanup, and then
# exits with 0 status.  If this function is NOT
# defined, it is assumed to be a no operation.  The
# function may be defined before judging_common.tcl
# is sourced.
#
# proc caught_error_retry {} {}

# Exit cleanup.  Called to do special cleanup before
# exit.  Default does nothing.  This proc may be
# redefined by program.
#
proc exit_cleanup {} {}

# List Functions
# ---- ---------

# Return 1 is element is an element of a the list, and 0
# otherwise.
#
proc lcontain { list element } {
    return [expr [lsearch -exact $list $element] >= 0]
}

# Append lists to a list.  Like `eval lappend var $list'
# but the latter fails if $list contains a \n.
#
proc lappend_lists { varname args } {
    upvar $varname var
    foreach arg $args {
        foreach element $arg {
	    lappend var $element } }
}

# Return sorted intersection of two lists.
#
proc intersect { list1 list2 } {
    set result {}
    foreach x [lsort $list1] {
        if { [lcontain $list2 $x] } {
	    lappend result $x
	}
    }
    return $result
}

# Append an element to a list if the element is NOT
# already in the list.
#
proc ladd { listname element } {
    upvar $listname list
    if { [lsearch -exact $list $element] < 0 } {
        lappend list $element
    }
}

# Remove an element from a list if the element is in the
# list.  Assumes the element is in the list at most
# once.
#
proc lsub { listname element } {
    upvar $listname list
    set i [lsearch -exact $list $element]
    if { $i >= 0 } {
        set list [lreplace $list $i $i]
    }
}

# Documentation Functions
# ------------- ---------

# These functions process text, and are use primarily
# but not exclusively for documentation.

# If the condition is true in the caller's environment,
# evaluate the text expression in the caller's environ-
# ment, as if it were enclosed in " quotes, and return
# its value.  Else return "".
#
proc include_if { condition textexpr } {
    if { [uplevel [list expr $condition]] } {
        return [uplevel "join \[list \"$textexpr\"\]"]
    } else {
        return ""
    }
}

# Function to include lists in documentation with given
# indent and given maximum length (defaulting to 56).
# If the list is empty, print <empty-list>
#
proc indent_list { indent list { maxlen 56 } } {
    if { "$list" == "" } {
        set list "<empty-list>"
    }
    set column $indent
    set out ""
    foreach item $list {
        set len [string length $item]
        if {    $column != $indent \
	     && $column + $len + 1 > $maxlen } {
	    set out "$out\n"
	    set column 0
	    while { $column < $indent } {
	        set out "$out "
		incr column
	    }
	}
	if { $column == $indent } {
	    set out "$out$item"
	} else {
	    set out "$out $item"
	    incr column
	}
	incr column $len
    }
    return $out
}

# Dispatch Locking Functions
# -------- ------- ---------


# Lock given directory by creating Dispatch_PID file
# in the directory (defaults to current directory).
# Return `yes' if success, and `no' if failure.
#
proc dispatch_lock { { directory . } } {

    set lock_file $directory/Dispatch_PID

    if { [create_file $lock_file] } {

	# Store the current process ID in Dispatch_PID
	# file.
	#
	write_file $lock_file [current_pid]
	return yes
    } else {
    	return no
    }
}

# Unlock given directory (defaults to current directory)
# by deleting any existing Dispatch_PID file in the
# directory.
#
proc dispatch_unlock { { directory . } } {

    set lock_file $directory/Dispatch_PID

    file delete -force $lock_file
}

# Date Functions
# ---- ---------


# Convert a [clock seconds] value into a date in
# the form yyyy-mm-dd-hh:mm:ss that is usable as
# part of a filename.  Respect the use_gmt global
# variable.
#
proc clock_to_filename_date { clock } {
    global use_gmt
    return [clock format $clock \
                  -format {%Y-%m-%d-%H:%M:%S} \
		  -gmt $use_gmt]
}

# Do the reverse conversion to that of the above
# function.  Respect the use_gmt global variable.
#
proc filename_date_to_clock { date } {
    global use_gmt
    set n {([0-9]+)}
    if { ! [regexp "^$n-$n-$n-$n:$n:$n\$" $date forget \
    	           year month day \
		   hour minute second] } {
	error "Not a legal filename date:    $date"
    }
    return [clock scan \
	    "$month/$day/$year $hour:$minute:$second" \
	    -gmt $use_gmt]
}

# Given a time that may be relative to a base time,
# return the time converted to an absolute time.  A
# relative time is an optional sign followed by decimal
# digits, and is measured in seconds from the base time.
#
proc absolute_time { time base_time } {
    if { [regexp {^([+|-]|)[0-9]+$} $time] } {
        set t [clock scan $base_time]
	incr t $time
	set time [clock format $t]
    }
    return $time
}

# Return true if the current time is later than a given
# (absolute) time.
#
proc is_later { time } {
    set time [clock scan $time]
    return [expr $time < [clock seconds]]
}


# Checked File Functions
# ------- ---- ---------


# Returns 1 iff the filename is checked, and 0 iff it is
# unchecked.  Calls error if the filename has no check
# mark.
#
# To have a check mark, the filename must have the form
# ...<<...>>-checked-... or ...<<...>>-unchecked-... .
# Note that characters in the ...'s after the << must
# not contain < or >.
#
proc is_checked { filename } {
    set e \
	{^(.*<<[^<>]*>>-)(unchecked|checked)(-[^<>]*)$}
    if { ! [regexp $e $filename forget a b c] } {
	error "$filename does not include a checkmark"
    }
    return [expr { $b == "checked" }]
}

# Function to change the name of a file from
# ...<<...>>-unchecked-... to ...<<...>>-checked-...;
# Return new filename.
#
proc make_checked { filename } {
    set e \
	{^(.*<<[^<>]*>>-)(unchecked|checked)(-[^<>]*)$}
    if { ! [regexp $e $filename forget a b c] } {
	error "$filename does not include a checkmark"
    }
    if { $b != "checked" } {
	file rename -force $filename ${a}checked${c}
    }
    return ${a}checked${c}
}

# Function to change the name of a file from
# ...<<...>>-checked-... to ...<<...>>-unchecked-...;
# Return new filename.
#
proc make_unchecked { filename } {
    set e \
	{^(.*<<[^<>]*>>-)(unchecked|checked)(-[^<>]*)$}
    if { ! [regexp $e $filename forget a b c] } {
	error "$filename does not include a checkmark"
    }
    if { $b != "unchecked" } {
	file rename -force $filename ${a}unchecked${c}
    }
    return ${a}unchecked${c}
}

# Error Logging Functions
# ---- -------- ---------


# Function called at end of program when a fatal error
# in the program is caught.  Since the error is logged,
# we return exit code 0 so the program that called
# this program does not also report an error.
#
proc caught_error {} {
    global caught_output
    if { [llength [info procs \
                        caught_error_retry]] != 0 } {
	caught_error_retry
    }
    log_error $caught_output
    exit_cleanup
    exit 0
}

# Function called to log an error when the program
# may want to continue.  The error is printed on the
# standard error output unless `log_quiet' exists and
# equals `yes'.  Unless `log_disable' exists and equals
# `yes', the error is also written to a file.  In this
# case, if `log_mode' is `auto' or `auto+manual', the
# first error of a program is also emailed to any
# submitter/requester identified in the Received_Mail
# file, and if the `log_mode' is `auto', this mail is
# cc'ed to any log manager.  This mail will contain a
# copy of any X-HPCM-Test-Subject field that is in any
# Received_Mail file if the error is being logged in the
# current directory (see below).
#
# When the error information is written to a file, a
# separate file is created for each error.  If the
# `log_globally' variable does not exist or does not
# equal `yes' and the current directory is writable, the
# file is written into the current directory.  Otherwise
# if $judging_directory/log names a directory that
# exists or can be made and is writable once it is made,
# then the file is written into this directory.  Other-
# wise the file is written into $default_log_directory,
# which is defined above as an emergency last resort.
#
# The format of the file name is:
#
#	dddd-uuuu-<<pppp>>-unchecked-error
#
# where dddd = is the date in filename date format
#       uuuu = random 6 digit number for uniqueness
#       pppp = name of executing program
#
# The word `unchecked' in the name will be changed to
# `checked' when a person checks off on the error.
#
# If the error is emailed to the submitter/requester,
# the mail sent is put in a separate file whose name
# is as just given but with the `.mail' extension.
#
# Recording an error in a log file always sets the
# Needs_Reply_Flag.

# Number of calls to log_error during this program.
#
# If log_error_count > log_error_maximum then
# log_disable is set to `yes'.
#
set log_error_count 0
#
proc log_error { error_output } {

    global argv0 argv errorCode errorInfo \
	   default_log_directory judging_directory \
	   log_globally log_disable log_quiet log_mode \
	   log_error_count log_error_maximum \
	   log_manager \
	   message_From_line message_to message_from \
	   message_date message_subject \
	   message_x_hpcm_test_subject

    set log_directory $judging_directory/log

    # Increment count of calls to log error and if
    # appropriate set log_disable to `yes' or exit
    # from program.
    #
    incr log_error_count
    if { $log_error_count > 1001 } {
        exit 2
    } elseif { $log_error_count > 1000 } {
	exit_cleanup
        exit 2
    } elseif { $log_error_count > $log_error_maximum } {
    	set log_disable yes
    }
    if {    [info exists log_quiet] \
         && $log_quiet == "yes" } {
        set noisy no
    } else {
        set noisy yes
    }

    # If `log_disable' is `yes', do not write to file,
    # but print errorCode and errorInfo to standard
    # error output and return.  Do this before writing
    # error, so that error will be visible at end of
    # printout.
    #
    if {    [info exists log_disable] \
         && $log_disable == "yes" } {
	if { $noisy } {
	    puts stderr "------------------------------"
	    puts stderr "ERROR during $argv0 $argv"
	    puts stderr "errorCode: $errorCode"
	    puts stderr "errorInfo follows:"
	    puts stderr "--------------------"
	    puts stderr $errorInfo
	    puts stderr "--------------------"
	    puts stderr $error_output
	    puts stderr "--------------------"
	    puts stderr "ABOVE ERROR during $argv0\
	    				    $argv"
	    puts stderr "------------------------------"
	}
	return
    }

    # Write error to standard error output.
    #
    if { $noisy } {
	puts stderr "------------------------------"
	puts stderr "ERROR during $argv0 $argv"
	puts stderr "--------------------"
	puts stderr $error_output
	puts stderr "------------------------------"
    }

    # Compute $log_dir, the logging directory to be
    # used.  Make it if necessary.  Be sure it is
    # writable.
    #
    if { ( ! [info exists log_globally] \
           ||  $log_globally != "yes" ) \
	 && [file writable "."] } {
        set log_dir "."
    } elseif {    ! [catch { file mkdir \
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
        set date [clock seconds]
        set d [clock_to_filename_date $date]
        set u [format %06d \
		  [expr { [clock clicks] % 1000000 } ]]
        set p [file tail $argv0]
	regsub -all {<} $p "{{" p
	regsub -all {>} $p "}}" p
        set log_file \
	  "$log_dir/${d}-${u}-<<${p}>>-unchecked-error"

	if { [create_file $log_file] } {
	    break
	}

	incr count

	if { $count > 100 } {

	    # Desperation move.  Should never happen.
	    #
	    set e LOGGING-FILENAME-GENERATION
	    set e ${e}-unchecked-error
	    set log_file $log_dir/$e
	    break
	}
    }

    # Write error to $log_file file.
    #
    if { $noisy } {
	puts stderr "-----"
	puts stderr "Logging to $log_file"
    }

    set log_ch [open $log_file a]
    puts $log_ch "----------------------------------"
    puts $log_ch "ERROR during $argv0 $argv"
    puts $log_ch "-----"
    puts $log_ch $error_output
    puts $log_ch "-----"
    puts $log_ch "date: [clock format $date]"
    puts $log_ch "pwd: [pwd]"
    puts $log_ch "errorCode: $errorCode"
    puts $log_ch "errorInfo follows:"
    puts $log_ch "-----"
    puts $log_ch $errorInfo
    puts $log_ch "----------------------------------"
    close $log_ch

    # If log_mode is `auto' or `auto+manual' and log_
    # error_count is 1 try to send mail.
    #
    if { [regexp {auto} $log_mode] \
         && $log_error_count == 1 } {

	# Compute the submitter/requester email address
	# in `to' and the log manager email address in
	# `cc'.  These equal "" if the address is not
	# to be used.
	#
	# When done, `received_ch' is not "" iff
	# the Received_Mail file header was read.
	#
	set to		""
	set cc		""
	set received_ch ""

	if { $log_mode == "auto" } {
	    set cc [string trim $log_manager]
	}

        if { $log_dir == "." \
	     && [file exists Received_Mail] } {

	    set received_ch [open Received_Mail r]
	    read_header $received_ch
	    close $received_ch

	    set to [string trim \
	    		   [compute_message_reply_to]]
	    if { $to == "UNKNOWN" } {
	    	set to ""
	    }
	}

	# If `to' or `cc' are not "" compute and send
	# mail.
	#
	if { $to != "" || $cc != "" } {

	    # It is important not to send the full
	    # pathname of the log file to a submitter/
	    # requester for security reasons.

	    if { $log_dir == "." } {
		set dir_tail [file tail [pwd]]
	    } else {
		set dir_tail [file tail $log_dir]
	    }
	    set log_tail [file tail $log_file]

	    # Open mail file and write `To:' and `Cc:'
	    # fields.
	    #
	    set mail_ch [open $log_file.mail w]
	    if { $to != "" } {
		puts $mail_ch "To: $to"
		if { $cc != "" } {
		    puts $mail_ch "Cc: $cc"
		}
	    } else {
		puts $mail_ch "To: $cc"
	    }

	    # Write reply-to field equal to Received_
	    # Mail `To' field, if that exists.
	    #
	    if {    $received_ch != "" \
	         && [info exists message_to] \
	         && $message_to != "" } {
		puts $mail_ch "Reply-To:$message_to"
	    }

	    # Write `Subject:' field, `X-HPCM-Test-
	    # Subject' field, and announce error.
	    #
	    puts $mail_ch "Subject: $log_tail"
	    puts $mail_ch "         in $dir_tail"
	    if { $received_ch != "" \
	         && $message_x_hpcm_test_subject \
		    != "" } {
	        set mts $message_x_hpcm_test_subject
		puts $mail_ch \
		     "X-HPCM-Test-Subject:$mts"
	    }
	    puts $mail_ch ""
	    puts $mail_ch "System error:"
	    puts $mail_ch ""
	    puts $mail_ch $error_output
	    puts $mail_ch ""

	    # If message header available, include info
	    # from it.
	    #
	    if { $received_ch != "" } {
		puts $mail_ch "While processing:"
		puts $mail_ch ""
		puts $mail_ch $message_From_line
		puts $mail_ch "From:$message_from"
		puts $mail_ch "Date:$message_date"
		puts $mail_ch \
		     "Subject:$message_subject"
		puts $mail_ch ""
	    }

	    # If mail is to going to requester/sub-
	    # mitter, tell them what to do.
	    #
	    if { $to != "" } {
		if { [regexp {manual} $log_mode] } {
		    puts $mail_ch \
			 "The judge will look at this\
			  and give you further\
			  instructions as necessary,"
		    puts $mail_ch \
			  "but you might be able to\
			   correct the problem yourself\
			   and resubmit."
		} else {
		    puts $mail_ch \
			 "System is automatic without\
			  immediate human monitoring."
		    if { $cc != "" } {
			puts $mail_ch \
			     "Please correct and\
			      resubmit, or wait for"
			puts $mail_ch "a response from\
			               $cc."
		    } else {
			puts $mail_ch \
			     "Please correct and\
			      resubmit, or contact\
			      the person"
			puts $mail_ch \
			     "responsible for this\
			      site."
		    }
		}
	    }

	    # Close mail file and send mail.
	    #
	    close $mail_ch
	    if { $noisy } {
		puts stderr ""
		puts stderr "Mailing $log_tail"
		puts stderr "To $to $cc"
	    }
	    send_mail $log_file.mail
	}
    }
    if { $noisy } {
	puts stderr "------------------------------"
    }

    set_flag Needs_Reply_Flag
}

# Message Header Functions
# ------- ------ ---------

# To read an email message, open the email file for
# reading on a channel, and apply the functions:
#
#	read_header $ch ...
#	read_part_header $ch
#	read_part_line $ch eof_found
#	read_part_line $ch eof_found
#	. . . . .
#	read_part_line $ch eof_found
#
# If only the email header is needed, only read_header
# is needed.

# Read an email message header from the channel. If the
# first_line argument is given, it is the first line of
# the header (note that email header lines cannot be
# empty).  Stop reading at the first empty line and
# discard that line.
#
# The results are returned in global variables.
#
#	message_header		All the lines of the
#				header (but NOT the
#				'^From\ ' line), without
#				the trailing `\n'.
#	message_From_line	The first line if it
#				begins with `^From\ '.
#	message_from		`From:' field value.
#	message_to		`To:' field value.
#	message_reply_to	`Reply-To:' field value.
#	message_subject		`Subject:' field value.
#	message_date		`Date:' field value.
#	message_content_transfer_encoding
#				`Content-Transfer-
#				 Encoding:' field value.
#	message_content_type	`Content-Type:' field
#				value.
#	message_x_hpcm_date	`X-HPCM-Date:' field
#				value.
#	message_x_hpcm_reply_to	`X-HPCM-Reply-To:'
#				field value.
#	message_x_hpcm_signature `X-HPCM-Signature:'
#				field value.
#	message_x_hpcm_signature_ok
#				`X-HPCM-Signature-OK:'
#				field value.
#	message_x_hpcm_test_subject
#				`X-HPCM-Test-Subject:'
#				field value.
#
# ALL these values have the final \n stripped off.  All
# the field values have the `field-name:' stripped off.
# Field values may be multi-line, but if there are two
# copies of a field, only the last is recorded.  If
# there are no copies of a field in the message, the
# message_... global variable for that field is set to
# "".
#
# If an omit_fields argument is given, it is a list of
# field names (written with all lower case letters) that
# are to be deleted from the header as it is read, and
# thereby completely ignored.  The typical value is:
#
#	{ x-hpcm-signature-ok }
#
#
proc read_header { ch { first_line "" }
                      { omit_fields "" } } {

    global message_header \
	   message_From_line \
           message_from \
	   message_to \
	   message_date \
	   message_reply_to \
	   message_subject \
	   message_content_transfer_encoding \
	   message_content_type \
	   message_x_hpcm_date \
	   message_x_hpcm_reply_to \
	   message_x_hpcm_signature \
	   message_x_hpcm_signature_ok \
	   message_x_hpcm_test_subject

    set message_header			""

    set message_From_line		""
    set message_from			""
    set message_to			""
    set message_date			""
    set message_reply_to		""
    set message_subject			""
    set message_content_transfer_encoding ""
    set message_content_type		""
    set message_x_hpcm_date		""
    set message_x_hpcm_reply_to		""
    set message_x_hpcm_signature	""
    set message_x_hpcm_signature_ok	""
    set message_x_hpcm_test_subject	""

    set fields "from to date reply-to subject\
    	        content-transfer-encoding content-type \
                x-hpcm-reply-to x-hpcm-date\
		x-hpcm-signature x-hpcm-signature-ok \
		x-hpcm-test-subject"

    set ws "\[\t\ \n\r\f\]"
    set nws "\[^\t\ \n\r\f\]"

    # Get first line of message.
    #
    set line $first_line
    if { $line == "" } {
        set line [gets $ch]
	if { [eof $ch] } return
    }

    # Set `From ' if found line.
    #
    if { [regexp "^From${ws}" $line] } {
    	set message_From_line $line
	set line [gets $ch]
	if { [eof $ch] } return
    }

    # Loop until empty line looking for fields listed
    # in the `fields' variable above.
    #
    while { "yes" } {
    	if { $line == "" } break

	# Process any extra lines in a multi-line field,
	# and read first line AFTER field value (or
	# EOF).
	#
	set lines $line
	while { "yes" } {
	    set line [gets $ch]
	    if { [eof $ch] \
		 || ! [regexp \
			   "^\[\ \t\]" \
			   $line] } \
		break;
	    set lines "$lines\n$line"
	}
	if { $message_header == "" } {
	    set message_header $lines
	} else {
	    set message_header "$message_header\n$lines"
	}

	# Loop through field names we are looking for.
	#
	foreach fieldname $fields {
	    if { [regexp \
	    	    -nocase \
		    "^(${fieldname})${ws}*:(.*)\$" \
		    $lines \
		    forget realname fieldvalue] } {

		# Come here when lines is for the
		# field we are looking for.

		# If field is not to be omitted, set
		# the global variables for it and for
		# the entire message header.
		#
		if { ! [lcontain $omit_fields \
			         $fieldname] } {
		    set varname message_$fieldname
		    regsub -all -- "-" $varname "_" \
		           varname
		    set $varname $fieldvalue
		}

		break
	    }
	}

	# If next line is really an EOF, break.
	#
	if { [eof $ch] } break
    }
}


# Read email body part from the channel.  Must be called
# after read_header has been called, and before read_
# part_line is called.  Read_part_line must be called to
# read the lines of the body part.
#
# Prepares read_part_line to read lines from the first
# NON-EMPTY part that has an acceptable Content-Type and
# Content-Transfer-Encoding, as determined by the
# content_type_values and content_transfer_encoding_
# values global variables (see hpcm_judging.rc).  A non-
# multipart message is treated as if its body were the
# one and only part of the message.  It is an error if a
# multipart message has no parts, empty or not.  How-
# ever, if there are only empty parts, there is no
# error, and the body as read by read_part_line is
# merely empty.
#
# By empty part we mean a part with no non-whitespace
# characters in its body.
#
# Some results are returned in the following global
# variables:
#
#	message_part_boundary	`boundary=' parameter
#				value within the
#				`Content-Type:' field
#				value for a multipart
#				message. "" for non-
#				multipart messages,
#				and possibly also for
#				a multipart message.
#	message_part_content_transfer_encoding
#				`Content-Transfer-
#				 Encoding:' field value
#				of message part, or of
#				whole message for non-
#				multipart messages.
#				Defaults to 7bit.
#	message_part_content_type
#				`Content-Type:' field
#				value of message part,
#				or of whole message for
#				non-multipart messages.
#				Defaults to
#				`text/plain'.
#	message_part_error	Set to error message if
#			        there is an error, or to
#				"" if there is no error.
#
# All the values have the final \n stripped off.  All
# the field values have the `field-name:' stripped off.
# Field values may be multi-line, but if there are two
# copies of a field, only the last is recorded.
#
# The global message_terminator is used.  This is a
# regular expression that may match a line in a message
# body to signal that the line and the rest of the body
# are to be ignored (the ignored lines may contain a
# signature or a Yahoo advertisement).  See hpcm_
# judging.rc.
#
# The message_translated_... global variables are set
# to define the part lines: see read_part_line.  Only
# this function and read_part_line use these variables.
#
proc read_part_header { ch } {

    global message_terminator \
	   message_content_transfer_encoding \
	   message_content_type \
	   message_part_boundary \
	   message_part_content_transfer_encoding \
	   message_part_content_type \
	   message_part_error

    global content_type_values \
	   content_transfer_encoding_values

    global message_translated_body \
    	   message_translated_index \
    	   message_translated_length

    set message_part_boundary		""
    
    set message_part_error		""

    set message_translated_body		""
    set message_translated_index	-1
    set message_translated_length	0

    set WS "\t\ \n\r\f"
    set ws "\[${WS}\]"
    set nws "\[^${WS}\]"

    # Remember if this is a multipart message.
    #
    set multipart [regexp -nocase "^${ws}*multipart" \
    		           $message_content_type]

    # For any multipart message compute the boundary.
    #
    if { $multipart } {

	# An unquoted boundary value ends with ; or the
	# end of field.

	set b0 "\[${WS};\]boundary${ws}*=${ws}*"
	set b1 "${b0}\"(\[^\"\]*)\""
	set b2 "${b0}(\[^;\]*)(;|\$)"
	set boundary ""
	# Must use b1 FIRST!
	if { [regexp -nocase $b1 \
		     $message_content_type \
		     forget boundary] } {
	} elseif { [regexp -nocase $b2 \
		           $message_content_type \
		           forget boundary] } {
	} else {

	    set mct $message_content_type
	    set message_part_error \
		"No boundary parameter\
		in:\nContent-Type:$mct"
	    return
	}

	# Whitespace must be trimmed from ends of
	# boundary, and internal whitespace replaced
	# by single SPACES.
	#
	set boundary [string trim $boundary]
	regsub -all "${ws}+" $boundary \
		    " " boundary
	set message_part_boundary $boundary
    }


    # Loop through parts until we find a NON-EMPTY one
    # with legal Content-Type and Content-Transfer-
    # Encoding.  Set message_translated_... variables
    # to the first non-empty legal part found, or the
    # last empty part found.
    #
    set line ""
    while { "yes" } {

    	# If multipart, skip to next message boundary
	# and find part Content-Type and Content-
	# Transfer-Encoding.
	#
	if { $multipart } {

	    set type text/plain
	    set encoding 7bit

	    set bnd "--$message_part_boundary"
	    set bndlen [string length $bnd]
	    while { "yes" } {
		if { [eof $ch] } break
		if { [string equal \
		             -length $bndlen \
			     $line $bnd] } \
		    break
		set line [gets $ch]
	    }

	    # If no boundary line found, break.
	    #
	    if { [eof $ch] } break

	    # If last boundary found, break.
	    #
	    set endbnd "$bnd--"
	    set endbndlen $bndlen
	    incr endbndlen 2
	    if { [string equal -length $endbndlen \
			       $line $endbnd] } \
	    	break

	    # Find part fields.
	    #
	    set ct "content-type${ws}*:(.*)\$"
	    set cte "content-transfer-encoding"
	    set cte "${cte}${ws}*:(.*)\$"
	    set next_line [gets $ch]
	    while { "yes" } {
		set line $next_line
		if { [eof $ch] } break
		if { $line == "" } break
		if { [regexp {^--} $line] } break

		set next_line [gets $ch]
		while { ! [eof $ch] \
		        && [regexp "^\[\ \t\]" \
			           $next_line] } {
		    set line "$line\n$next_line"
		    set next_line [gets $ch]
		}
		if { [regexp -nocase $ct $line \
			     forget type] } {
		} elseif { [regexp -nocase \
				   $cte $line \
				   forget \
				   encoding] } {
		}
	    }
	}

	# If not multipart, set the type and encoding
	# from the message header.
	#
	if { ! $multipart } {
	    
	    set type $message_content_type
	    set encoding \
	        $message_content_transfer_encoding
	}

	# Set empty type and encoding to defaults.
	#
	if { ! [regexp $nws $type] } {
	    set type text/plain
	}
	if { ! [regexp $nws $encoding] } {
	    set encoding 7bit
	}

	# If type and encoding are not legal, continue
	# to the next part.
	#
	set ctv $content_type_values
	set ctev \
	    $content_transfer_encoding_values
	if {    ! [regexp -nocase \
	                  "^${ws}*(${ctv})\$" $type] \
	     || ! [regexp -nocase "^${ws}*(${ctev})\$" \
	                  $encoding] } {
	    if { $multipart } {
	    	continue
	    } else break
	}

	# Set part encoding and type.
	#
	set message_part_content_transfer_encoding \
	    $encoding
	set message_part_content_type $type

	# Accumulate the translation as a list of lines
	# in `translated'.

	set translated ""
	if { $multipart \
	     && [string equal -length $bndlen \
			$line $bnd] } {
	    # Avoid getting more lines if we are
	    # already at part ending boundary.
	} elseif { [regexp -nocase \
	                   "^${ws}*(7bit|8bit)" \
			   $encoding] } {

	    while { "yes" } {
		set line [gets $ch]
		if { [eof $ch] } break
		if { $multipart \
		     && [string equal \
				-length $bndlen \
				$line $bnd] } \
		    break
		lappend translated $line
	    }

	} elseif { [regexp -nocase \
			   "^${ws}*quoted-printable" \
			   $encoding] } {
	    while { "yes" } {
		set line [gets $ch]
		if { [eof $ch] } break
		if { $multipart \
		     && [string equal \
				-length $bndlen \
				$line $bnd] } \
		    break

		if { [regexp {=} $line] } {

		    while { [regexp {^(.*)=$} \
				    $line forget \
				    line] } {
			set line "${line}[gets $ch]"
		    }

		    regsub -all {=09} $line "\t" line
		    regsub -all {=0C} $line "\f" line
		    regsub -all {=20} $line "\ " line
		    regsub -all "\r" $line "" line

		    # This must be LAST!

		    regsub -all {=3D} $line "=" line
		}

		lappend translated $line
	    }
	    

	} elseif { [regexp -nocase "^${ws}*base64" \
			   $encoding] } {
	    set body ""
	    while { "yes" } {
		set line [gets $ch]
		if { [eof $ch] } break
		if { $multipart \
		     && [string equal \
				-length $bndlen \
				$line $bnd] } \
		    break
		set body "$body[string trim $line]\n"
	    }
	    if { [catch {
		    set translated \
			[translate_base64 $body] } \
			out] } {
		set message_part_error \
		    "Error translating base64\
		     message:\n$out"
	    } else {
		regsub -all "\r\n" $translated \
			    "\n" translated
		regsub "\n\$" $translated "" translated
		set translated \
		    [split $translated "\n"]
	    }
	} else {
	    error "Unhandled encoding type:\
	           $encoding"
	}

	# If message_terminator is not "", look for
	# it in translated message and delete it and
	# lines after it.
	#
	# Also compute length of translated message.
	#
	if { $message_terminator != "" } {
	    set length 0
	    set term "^${message_terminator}\$"
	    foreach tline $translated {
	        if { [regexp -nocase $term $tline] } {
		    set last $length
		    incr last -1
		    set translated \
			[lrange $translated 0 $last]
		    break
		}
	        incr length
	    }
	} else {
	    set length [llength $translated]
	}

	set message_translated_body $translated
	set message_translated_index 0
	set message_translated_length $length

	# If body not empty, end search for parts.
	#
	set body_non_empty no
	foreach tline $translated {
	    if { [regexp $nws $tline] } {
		set body_non_empty yes
		break
	    }
	}
	if { $body_non_empty || ! $multipart } break
    }

    # If no legal part found, empty or not, set
    # message_part_error.
    #
    if { $message_translated_index < 0 } {
        set message_part_error \
	    "No message part found with legal\
	     Content-Type and\
	     legal Content-Transfer-Encoding"
    }
}

# Using information from the last call to read_part_
# header, return the next body line.  Set the end_of_
# file variable to "yes" if at end of part, and to "no"
# otherwise.  Return line, or return "" on end of file.
#
# The information from read_part_header, internal to
# that routine and this, is
#
#	message_translated_body
#	    List of lines of body.
#	message_translated_length
#	    Length of this list.
#	message_translated_index
#	    Index (0, 1, 2, ...) of next line to return.
#
proc read_part_line { ch end_of_file } {

    global message_translated_index \
	   message_translated_length \
	   message_translated_body

    upvar $end_of_file eof
    set eof no

    if { $message_translated_index < 0 } {
        error "read_part_line called when\
	       read_part_header was not called or\
	       read_part_header returned a\
	       message_part_error"
    }

    if { $message_translated_index \
	 < $message_translated_length } {
	set line [lindex $message_translated_body \
			 $message_translated_index]
	incr message_translated_index
	return $line
    } else {
	set eof yes
	return ""
    }
}

# Read message body lines using read_part_line.  Return
# `yes' if no non-blank line encounted before eof.
# Return `no' otherwise.
#
proc blank_body { ch } {

    while { "yes" } {
	set line [read_part_line $ch eof]
	if { $eof } {
	    return yes
	} elseif { [regexp "^\[\ \t\r\n\f\]*\$" \
			   $line] } {
	    # blank lines are ok
	} else {
	    return no
	}
    }
}

# Using information from the last call to read_header,
# return the `To:' field value for a reply to that
# message.  This is the message `X-HPCM-Reply-To' field
# if that is not empty, or the `Reply-To' field if that
# is not empty, or the message `From' field if that is
# not empty, or the address in a `From' line if that
# is not empty, or `UNKNOWN' as a last resort.
#
proc compute_message_reply_to {} {

    global message_x_hpcm_reply_to message_reply_to \
           message_from message_From_line

    if { [regexp "\[^\ \t\n\r\f\]" \
	         $message_x_hpcm_reply_to] } {
	return $message_x_hpcm_reply_to
    } elseif { [regexp "\[^\ \t\n\r\f\]" \
                       $message_reply_to] } {
	return $message_reply_to
    } elseif { [regexp "\[^\ \t\n\r\f\]" \
                       $message_from] } {
	return $message_from
    } elseif { [catch { set len \
    			    [llength \
			         $message_From_line] \
				 	}] } {
        return "UNKNOWN"
    } elseif { $len >= 2 } {
	return [lindex $message_From_line 1]
    } else {
    	return "UNKNOWN"
    }
}

# Using the current directory name, compute the list of
# addresses to cc a message to.  Uses the global
# variable `response_manager_map'.  See hpcm_judging.rc
# for details.  Returns "" if no addresses to cc to,
# or if current directory name does not have format
# date-<<submitter>>-submission.
#
proc compute_response_cc {} {

    global response_manager_map

    set dir [file tail [pwd]]
    if { ! [regexp {^[^<>]*-<<(.*)>>-submission$} \
    		   $dir forget submitter] } {
    	return "";
    }

    set addresses ""
    foreach pair $response_manager_map {
        set regexp [lindex $pair 0]
        set address [lindex $pair 1]
	if { [regexp $regexp $submitter] } {
	    lappend addresses $address
	}
    }

    return [join $addresses ", "]
}




# Using information from the last call to read_header,
# return the date that best timestamps the message.
# This is the message `X-HPCM-date' field if that is not
# empty, or the `Date' field if that is not empty, or
# the date in the message `From' line if that is not
# empty, or the current date and time as a last resort.
#
proc compute_message_date {} {

    global message_x_hpcm_date message_date \
           message_From_line

    if { [regexp "\[^\ \t\n\r\f\]" \
	         $message_x_hpcm_date] } {
	set date $message_x_hpcm_date
    } elseif { [regexp "\[^\ \t\n\r\f\]" \
                       $message_date] } {
	set date $message_date
    } elseif { [catch { set len \
    			    [llength \
			         $message_From_line] \
				 	}] } {
    	return [clock format [clock seconds]]
    } elseif { $len >= 3 } {
	return [lrange $message_From_line 2 end]
    } else {
    	return [clock format [clock seconds]]
    }

    # Date may have +0000 at end; change to -0000.
    # Mail uses +0000 to indicate GMT; TCLSH uses -0000
    # and is out of date in this respect.
    #
    set date [regsub "\[+\]0000\$" $date "-0000"]
    return $date
}

# Compute whether the header just read by read_header
# is authentic.  Return `yes' if it is, `no' if it is
# not.  If the header does not already have the same
# answer recorded in its last `X-HPCM-Signature-OK'
# field, add that field with the newly computed value
# to the end of the header stored in the message_...
# global variables.
#
# If needed the required key is found in the authentica-
# tion_keys global variable if it is there, or in the
# $judging_directory/contest/secure/hpcm_sendmail.rc
# file otherwise.
#
proc compute_authentication {} {

    global message_x_hpcm_reply_to \
           message_x_hpcm_date \
	   message_x_hpcm_signature \
           message_x_hpcm_signature_ok \
	   message_header \
	   authentication_keys

    # If no signature is present in the message header
    # (or the signature there is inadequate), result is
    # no.
    #
    if {     [catch { \
                set len \
		    [llength \
		       $message_x_hpcm_signature] }] \
         || $len < 2 } {
    	set result no
    } else {
        set keyname \
	    [lindex $message_x_hpcm_signature 0]
        set signature \
	    [lindex $message_x_hpcm_signature 1]

	# Find key and set result yes temporarily, or
	# do not find key and set result no.
	#
	if { ! [catch \
	          { set key \
		        $authentication_keys($keyname) \
		        }] } {
	    set result yes
	} else {
	    set v [read_sendmail_rc]
	    set key [lindex $v 1]
	    if {    $keyname == [lindex $v 2] \
	         && $key != "" } {
		set result yes
	    } else {
	        set result no
	    }
	}

	# If result set to yes above, check authentica-
	# tion and recompute result.
	#
	if { $result } {

	    # We can compute a signature: do so.
	    #
	    set d "X-HPCM-Date:$message_x_hpcm_date\n"
	    set r $message_x_hpcm_reply_to
	    set r "X-HPCM-Reply-To:$r\n"
	    set v "$d$r$key\n"
	    set computed_signature \
	        [compute_signature $v]

	    # Check signature and set result.
	    #
	    if { $signature == $computed_signature } {
	    	set result yes
	    } else {
	        set result no
	    }
	}
    }

    # Record result in message header if it is not
    # already there (as X-HPCM-Signature-Ok field
    # value).
    #
    if { [string trim $message_x_hpcm_signature_ok] \
         != $result } {
        set message_x_hpcm_signature_ok " $result"
	set message_header \
	    "$message_header\nX-HPCM-Signature_OK:\
	     $result"
    }

    return $result
}

# Check whether a header just read by `read_header'
# is authentic.  If the `use_authentication' global
# variable is `no', the answer is always `yes'.
# Otherwise the answer is read from the `X-HPCM-
# signature-ok' field, if that is present.  Otherwise
# the answer is computed, saved in the `X-HPCM-
# signature-ok' field of the header stored in the
# message_... global variables, and returned.
#
proc header_is_authentic {} {

    global message_x_hpcm_signature_ok \
           use_authentication

    if { $use_authentication != "yes" } {
    	return yes
    }

    set ok [string trim $message_x_hpcm_signature_ok]

    if { $ok == "yes" || $ok == "no" } {
    	return $ok
    }

    return [compute_authentication]
}

# Read the $judging_directory/contest/secure/hpcm_
# sendmail.rc file and return a list of the form
#
#	{ To-value Key-value Key-Name-value }
#
# All values have whitespace trimmed from their ends.
# Missing values return as "".  If file does not exist,
# all values return as "".
#
proc read_sendmail_rc { } {
    global judging_directory
    set dir $judging_directory/contest/secure
    if { ! [file exists $dir/hpcm_sendmail.rc] } {
        return [list "" "" ""]
    }
    set ch [open $dir/hpcm_sendmail.rc r]
    set to ""
    set key ""
    set key_name ""
    while { "yes" } {
        set line [gets $ch]
	if { [eof $ch] } break
	regexp -nocase {^To:(.*)$} $line forget to
	regexp -nocase {^Key:(.*)$} $line forget key
	regexp -nocase {^Key-Name:(.*)$} \
	       $line forget key_name
    }
    close $ch
    return [list [string trim $to] \
                 [string trim $key] \
		 [string trim $key_name]]
}

# Reply Functions
# ----- ---------

# Construct a mail reply file and send it to the sender
# of any received mail file.
#
# The reply header is automatically produced using the
# following options.  The -cc option causes the reply to
# be cc'ed via the `response_manager_map'.  The -error
# option changes the reply subject from `RE:...' to
# `Errors In:...'.
#
# Non-option arguments are commands chosen from the
# following list:
#
#	{ LINE "string" }
#	    Includes the string followed by a line feed.
#
#	{ LINES "string" ... }
#	    Includes the strings each followed by a line
#	    feed.
#
#	BLANK
#	    Include a blank line.
#
#	BAR
#	    Includes a 72 character ----- line.
#
#	{ BAR "string" }
#	    Includes a ----- line ending with a space
#	    followed by the string.  The line will have
#	    exactly 72 characters.
#
#	{ INPUT filename }
#	    Includes the contents of the file, followed
#	    by a line feed if the file does not end with
#	    a line feed.
#
#	RECEIVED-HEADER
#	    Includes the Date, To, From, Reply-To, and
#	    Subject fields of the Received_Mail file
#	    message.
#
#	RECEIVED-FULL-HEADER
#	    Includes the ENTIRE header of the Received_
#	    Mail file message.
#
#	RECEIVED-BODY
#	    Includes the body of the Received_Mail file
#	    message.  This is the entire body, including
#	    part delimiters (because these may be in
#	    error).  This command can appear at most
#	    once.
#
# Options must precede commands.
#
# When the message is sent it is copied to the reply
# history file.
#
# Any previous reply file is deleted.
#
# The -notfinal option to send_reply is NOT available
# with this function.
#
proc reply { args } {
    eval compose_reply $args
    send_reply
}

# This composes the message to be sent by `reply' in
# the file `Reply_Mail+'.  This reply can then be
# manually edited before sending it with `send_reply'.
#
proc compose_reply { args } {

    global message_header \
           message_From_line message_to \
           message_from message_date \
	   message_reply_to message_subject \
	   message_x_hpcm_test_subject

    # Remove options from the arguments and remember.
    #
    set cc_option no
    set errors_option no
    while { [llength $args] >= 1 } {
	if { [lindex $args 0] == "-cc" } {
	    set cc_option yes
	} elseif { [lindex $args 0] == "-errors" } {
	    set errors_option yes
	} else {
	    break
	}
	set args [lrange $args 1 end]
    }

    # Read Received_Mail file header.
    #
    set received_ch [open Received_Mail r]
    read_header $received_ch

    # Delete any existing Reply_Mail+ file and open that
    # file for writing.
    #
    if { [file exists Reply_Mail+] } {
    	file delete -force Reply_Mail+
    }
    set reply_ch    [open Reply_Mail+ w]

    # Write header.
    #
    puts $reply_ch   "To:[compute_message_reply_to]"
    if { $cc_option } {
        set response_cc [compute_response_cc]
	if { $response_cc != "" } {
	    puts $reply_ch "Cc: $response_cc"
	}
    }
    puts $reply_ch "Reply-To:$message_to"
    if { $errors_option } {
        puts $reply_ch \
	     "Subject: Errors In:$message_subject"
    } else {
        puts $reply_ch \
	     "Subject: RE:$message_subject"
    }
    if { $message_x_hpcm_test_subject != "" } {
        set mts $message_x_hpcm_test_subject
	puts $reply_ch "X-HPCM-Test-Subject:$mts"
    }
    puts $reply_ch   ""

    # Process commands.
    #
    set bar "------------------------"
    set bar $bar$bar$bar
    foreach command $args {
        switch -- [lindex $command 0] {

	    LINE {
		puts $reply_ch [lindex $command 1]
	    }
	    LINES {
	        foreach s [lrange $command 1 end] {
		    puts $reply_ch $s
		}
	    }
	    BLANK {
		puts $reply_ch ""
	    }
	    BAR {
	        if { [llength $command] == 1 } {
		    puts $reply_ch $bar
		} else {
		    set s [lindex $command 1]
		    set l [string length $s]
		    incr l 1
		    puts $reply_ch \
		    	 "[string range $bar $l end] $s"
		}
	    }
	    INPUT {
		set file [lindex $command 1]
	        if { [file readable $file] } {
		    puts $reply_ch \
			 [read_entire_file \
			      [lindex $command 1]]
		} else {
		    puts $reply_ch \
		         "Sorry, file $file is not\
			  available."
		}
	    }
	    RECEIVED-HEADER {

		set nws "\[^\ \t\n\r\f\]"
		if { ! [regexp $nws $message_from] \
		     || \
		     ! [regexp $nws $message_date] \
			       		} {
		    puts $reply_ch ">$message_From_line"
		}
		if { [regexp $nws $message_to] } {
		    puts $reply_ch "To:$message_to"
		}
		if { [regexp $nws $message_from] } {
		    puts $reply_ch "From:$message_from"
		}
		if { [regexp $nws $message_date] } {
		    puts $reply_ch "Date:$message_date"
		}
		if { [regexp $nws $message_reply_to] } {
		    puts $reply_ch \
		         "Reply-To:$message_reply_to"
		}
		if { [regexp $nws $message_subject] } {
		    puts $reply_ch \
		         "Subject:$message_subject"
		}
	    }
	    RECEIVED-FULL-HEADER {

		puts $reply_ch $message_header
	    }
	    RECEIVED-BODY {

		while { "yes" } {
		    set line [gets $received_ch]
		    if { [eof $received_ch] } {
			break
		    } else {
			puts $reply_ch "$line"
		    }
		}
	    }
	    default {
	        error "Bad command to compose_reply:\
		       $command"
	    }
	}
    }

    # Close files and return.
    #
    close $reply_ch
    close $received_ch
}

# This function copies the `Reply_Mail+' file into
# into the `Reply_Mail_History' file and then emails
# the `Reply_Mail+' file.
#
# Before doing the above, this program deletes any
# Reply_Mail file.
#
# After doing the above, this program renames
# `Reply_Mail+' to `Reply_Mail', unless the -notfinal
# option is given, in which case this program simply
# deletes `Reply_Mail+'.
#
proc send_reply { args } {

    set not_final no
    foreach arg $args {
        if { $arg == "-notfinal" } {
	    set not_final yes
	} else {
	    error "Bad arg to send_reply: $arg"
	}
    }

    # Delete any Reply_Mail file.
    #
    if { [file exists Reply_Mail] } {
	file delete -force Reply_Mail
    }

    # Copy to the Reply_Mail_History file.
    #
    set history_ch  [open Reply_Mail_History a]
    puts $history_ch "From [account_name]@[host_name]\
		      [clock format [clock seconds]]"
    put_file Reply_Mail+ $history_ch

    # An empty line is needed before the next `From'
    # line so the `From' line will be recognized.
    #
    puts $history_ch ""
    close $history_ch

    # Send the Reply_Mail+ file.  If there is a bad
    # `To:' address, there may be an error, which will
    # usually be logged by an error file in the
    # current directory.  Otherwise a bad address
    # will cause return mail from the mailer
    # daemon.
    #
    send_mail Reply_Mail+

    # Delete or rename Reply_Mail+ file.
    #
    if { $not_final } {
        file delete -force Reply_Mail+
    } else {
	file rename -force Reply_Mail+ Reply_Mail
    }
}

# File Functions
# ---- ---------

# Set the contest_directory global variable to the true
# name of the contest directory, even if that directory
# does not exist yet.  Return the contest_directory
# variable value.
#
# If the variable already exists, do not change it.
# Otherwise if $judging_directory/contest is (linked to)
# a directory, return the truename of that directory.
# Otherwise compute the contest_directory value from
# the judging_directory value.
#
proc set_contest_directory {} {
    global contest_directory judging_directory
    set c_d $judging_directory/contest

    if { [info exists contest_directory] } {
        if { ! [regexp {/} $contest_directory] } {
	    error "contest_directory is not\
	           absolute: $contest_directory"
	}
        if {    ! [catch { file type \
	                        $contest_directory }] \
	     && ! [file isdirectory \
	                $contest_directory] } {
	    error "contest_directory is not\
	           a directory: $contest_directory"
	}
	if {    ! [catch { file type $c_d }] \
	     && (    ! [file isdirectory $c_d] \
	          ||    [truename $c_d] \
		     != $contest_directory ) } {
	    error "contest_directory\
	           ($contest_directory)\n    is not\
		   truename of $c_d$"
	}
        return $contest_directory
    }

    if { [file isdirectory $c_d] } {
        set contest_directory [truename $c_d]
	return $contest_directory
    }

    set j_d [truename $judging_directory]
    set dir [file dirname $j_d]
    set tail [file tail $j_d]
    if { ! [regexp {^judging_(.*)$} $tail \
		   forget tail] } {
	error "$j_d last component does not\
	       begin with `judging_'"
    }
    if { ! [regexp {^(.*)_[^_]+$} $tail \
                   forget tail] } {
	error "$j_d last component does not\
	       end with `_PASSWORD'"
    }
    set contest_directory "$dir/contest_$tail"

    # The following forces checking.
    #
    return [set_contest_directory]
}

# Write entire file to channel.  If number of lines in
# file is greater than third argument, do not write
# more lines than specified by the third argument, but
# in place of the omitted lines write the line:
#
#	... THERE ARE # MORE LINES ...
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
	          [expr { $count - $line_count }]\
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

# Write one-line file.  Actually, the line argument may
# be a string containing many lines, and the whole file
# is simply written all at once.
#
proc write_file { filename line } {
    set file_ch [open $filename w]
    puts $file_ch $line
    close $file_ch
}

# Return entire file as a string.  Each line in the
# string terminates with a line feed, except the line
# feed is omitted from the last line (so a file that
# does not end with a line feed effectively has an
# added ending line feed).
#
proc read_entire_file { filename } {
    set file_ch [open $filename r]
    set result [gets $file_ch]
    while { "yes" } {
	set line [gets $file_ch]
	if { [eof $file_ch] && $line == "" } break
	set result "$result\n$line"
    }
    close $file_ch
    return $result
}

# Write an entire file if and only if it is currently
# different from what would be written.  Contents are
# lines separated by \n but with NO ENDING \n.  A
# non-existant file is always written.
#
proc write_file_if_different { filename contents } {
    if { ! [file exists $filename] } {
        write_file $filename $contents
    } else {
	set f [read_entire_file $filename]
	if { $f != $contents } {
	    write_file $filename $contents
	}
    }
}

# Source a file if it exists.  Before sourcing file, run
# a security check: insist that file not be accessible
# by `others', but only by group and owner.  The file is
# sourced in the caller's level.
#
proc source_file { filename } {
    if { [file exists $filename] } {

	if { ! [file readable $filename] } {
	    error "$filename not readable"
	}

	set __p__ \
	    [file attributes $filename -permissions]
	if { $__p__ & 7 } {
	    error "security violation: $filename is\
		   accessible by `others'\n      \
		   you should execute chmod o-rwx\
		   $filename"
	}

	uplevel "source $filename"
    }
}

# Read a file whose lines are each a filename of the
# form .../PPP/FFF where PPP is a problem or demo name
# and FFF is the last component of the filename.  Return
# a list of all the problem or demo names PPP (with
# duplicates deleted).  Unreadable files and badly
# formatted file lines are detected errors.
#
proc get_problems_from_file { filename } {
    if { ! [file readable $filename] } {
        error "$filename is not readable"
    }
    set file_ch [open $filename r]
    set result {}
    while { "yes" } {
	set f [gets $file_ch]
	if { [eof $file_ch] } break
	set d [file dirname $f]
	if { $d == "." } {
	    error "bad line in $filename: $f"
	}
	set p [file tail $d]
	if { ! [lcontain $result $p] } {
	    lappend result $p
	}
    }
    close $file_ch
    return $result
}

# Flag Functions
# ---- ---------


# Set flag.
#
proc set_flag { flagfilename } {
    global judging_directory
    file mkdir $judging_directory/flag
    set file_ch \
        [open $judging_directory/flag/$flagfilename w]
    close $file_ch
}

# Test flag.
#
proc test_flag { flagfilename } {
    global judging_directory
    return \
     [file exists $judging_directory/flag/$flagfilename]
}

# clear flag.
#
proc clear_flag { flagfilename } {
    global judging_directory
    file delete -force \
         $judging_directory/flag/$flagfilename
}

# Make Functions
# ---- ---------

# Function to execute problem_make_files type instruc-
# tions in the s_d directory.  `name' is the name of the
# instructions: e.g. `problem_make_file value for
# PROBLEM' or `automatically generated make instruc-
# tions'.  It is used only in messages.  Return_first is
# true if and only instead of making anything, the name
# of the first file in s_d that needs to be made is
# returned, or "" is returned if no file needs to be
# made.
#
proc execute_makes \
	{ instructions s_d name \
	  { return_first 0 } } {

    if { [catch { llength $instructions }] } {
        error "$name is not a TCL\
	       list\n:    $instructions"
    }

    foreach instruction $instructions {

	if { [catch {llength $instruction}] } {
	    error "instruction in $name is not a TCL\
		   list:\n    $instruction"
	}

	# If a source file in the instruction does
	# not exist, ignore instruction.
	#
	set sources [lrange $instruction 2 end]
	set non_existant_input_found 0
	foreach source $sources {
	    if { ! [file exists $s_d/$source] } {
		set non_existant_input_found 1
		break
	    }
	}
	if { $non_existant_input_found } continue

	set target [lindex $instruction 0]
	if { [file exists $s_d/$target] } {

	    # If target file exists and no source file
	    # is more recent, skip instruction.
	    #
	    set mtime [file mtime $s_d/$target]
	    set skip 1
	    foreach source $sources {
		if {   [file mtime $s_d/$source] \
		     > $mtime } {
		    set skip 0
		    break
		}
	    }
	    if { $skip } continue
	}

	if { $return_first } {
	    return $target
	}

	if { [file exists $s_d/$target] } {
	    puts "Deleting $s_d/$target"
	    file delete -force -- $s_d/$target
	}
	if { [llength $instruction] == 1 } {
	    set command "make $target"
	} else {
	    set command [lindex $instruction 1]
	}
	puts "In $s_d:"
	puts "    making $target by executing: $command"
	exec_in_directory $s_d $command >&@ stdout
    }
    return ""
}


# Like execute_makes but locks the s_d directory if nec-
# essary.  The s_d_is_locked_name argument names a vari-
# able which is 1 if s_d is locked, 0 if not, and set to
# 1 by this function if it locks the directory.  Makes
# are always executed by this function if necessary
# (there is no return_first option).  To avoid unneces-
# sary locking, this function tests whether there are
# any files to be made before locking s_d.
#
proc execute_locked_makes \
	{ instructions s_d name s_d_is_locked_name } {

    upvar $s_d_is_locked_name s_d_is_locked

    if { ! $s_d_is_locked } {

        set first_target \
	    [execute_makes $instructions $s_d $name 1]

	# If there is nothing to do, exit procedure.
	#
	if { $first_target == "" } return

	# Try repeatedly to lock s_d, but if this fails
	# after a while, just give up and call error.
	#
	set seconds 0
	set limit 120
	set sleep 2
	set step 5
	# puts's are at step * sleep second intervals
	#
	while { "yes" } {
	    if { [dispatch_lock $s_d] } {
	    	break
	    } elseif { $seconds >= $limit } {
		error "CANNOT LOCK $s_d:\
		       giving up on making\
		       $first_target"
	    } elseif { $seconds == 0 } {
	    	# do nothing
	    } elseif { $seconds == $step * $sleep } {
	        puts "Waiting for $s_d lock"
	    } elseif { $seconds \
		       % ( $step * $sleep ) == 0 } {
	        puts "....will wait\
		      [expr $limit - $seconds]\
		      more seconds"
	    }
	    sleep $sleep
	    incr seconds $sleep
	}
	puts "Locked $s_d"
	set s_d_is_locked 1
    }

    execute_makes $instructions $s_d $name

}

# Logical Expression Compilation
# ------- ---------- -----------

# Compile a logical expression by replacing the atoms
# in the expression with `$V(N)', where N is the number
# of the atom in the expression, 0, 1, 2, ..., and V is
# the `values' parameter to this function.  The compiled
# logical expression is returned.  The n'th atom in the
# original expression is stored in `A(n)', where A is
# the value of the `atoms' parameter to this function.
# The `values' and `atoms' parameters are names of
# arrays.  These arrays are output (set by) this func-
# tion.  For each N, V(N) is initialized to 0 by this
# function.
#
# Thus to use this function execute:
#
#	array set abbrev_array { ... }
#	set logical_expression ...
#	set compiled_expression \
#	    [compile_logical_expression \
#		 $logical_expression \
#		 abbreviation_array \
#		 atom_array \
#		 value_array]
#	foreach n [array names atom_array] {
#	    set value_array($n) \
#	        ... some function of $atom_array($n) ...
#	}
#	set result [expr $compiled_expression]
#
# If $logical_expression were "x & ( y | 1 )" then
# "$value_array(0) & ($value_array(1) | 1)" would be
# returned as the compiled_expression and $atom_array(0)
# would be set to "x" while $atom_array(1) would be set
# to "y".
#
# The tokens `0', `1', `(', `)', `|', `&', `^', and `!'
# are non-atoms.  All other tokens are atoms.
#
# Parentheses and other non-atoms must be surrounded by
# whitespace in order to avoid being treated as parts
# of atoms.  E.g., "x & (y | 1)" would treat "(y" and
# "1)" as atoms.
#
# The logical expression is a TCL list whose elements
# are tokens.  Before any token is processed, it is
# looked up in the abbreviations array, and any value
# found replaces the token.  The value may be any TCL
# list.  Thus if `AB(T)' is defined, where AB is the
# `abbreviations' parameter to this function and T is a
# token in the logical expression (an element of the
# logical expression viewed as a list), then the value
# of `AB(T)' replaces T in the logical expression.  This
# rule IS recursive.  T may be any TCL list, e.g., one
# may set AB("OR") to "|" and AB("TRUE") to "1".
#
# Note that the compiled expression must be evaluated
# in an environment in which the `values' array is
# visible, and the `abbreviations', `atoms', and
# `values' arrays must all be accessed in the
# environment of the caller of this function.  The
# `values' array will be initialized to 0's and a test
# evaluation of the logical expression in the caller's
# environment is conducted to find logical expression
# syntax errors.
#
proc unabbreviate_expression \
	{ expression abbreviations } {
    upvar $abbreviations abbrevs
    set result {}
    foreach item $expression {
        if { [info exists abbrevs($item)] } {
	    lappend_lists result \
	         [unabbreviate_expression \
		      $abbrevs($item) abbrevs]
	} else {
	    lappend result $item
	}
    }
    return $result
}
#
proc compile_logical_expression \
	{ expression abbreviations atoms values  } {

    upvar $abbreviations abbrevs
    upvar $atoms atom
    upvar $values value

    catch { unset atom }
    catch { unset value }

    set n 0
    set logical_expression ""

    set depth 0
    foreach token [unabbreviate_expression \
                       $expression abbrevs] {

	switch -- $token {
	    (   {	incr depth }
	    )   {   incr depth -1
	    	    if { $depth < 0 } {
			error "Parentheses mismatch in\
			       `$expression'."
		    }
	    }
	    0   -
	    1   -
	    !   -
	    &   -
	    ^   -
	    |   { }

	    default {
		set atom($n) $token
		set value($n) 0
		set token "\$${values}($n)"
		incr n
	    }
	}

	# Append token to logical expression.
	#
	set logical_expression \
	    "$logical_expression $token"
    }

    if { $depth != 0 } {
	error "Parentheses mismatch in `$expression'."
    }

    if { [catch { uplevel 1 \
                    [list expr $logical_expression] \
		    	}] } {
        error "Bad syntax in logical expression:\n   \
	       $expression"
    }

    return $logical_expression
}

# Parse Functions
# ----- ---------

# The parse_block function takes a block of code con-
# taining `if', `elseif', and `else' constructs, the `#'
# comment statement ( as in {# ... #}), and the `EXIT'
# statement, and outputs a list of instructions with
# these statements processed and removed.
#
# `if' and `elseif' expressions are evaluated by this
# function and used to delete statements that are not
# to be executed.  Statements that are to be executed
# are appended to the command list.  If EXIT is to be
# executed, it is not appended, and this function stops
# and returns `yes'.  Otherwise if there is no EXIT to
# be executed, this function returns `no'.
#
# If a `#' comment statement is to be executed, it is
# NOT appended to the command list, and is thus deleted.
#
# It is the caller's responsibility to initialize the
# command list to the empty list, if the caller intends
# the list to be initially empty.
# 
# Globals is a list of all the global variables that
# can be used in `if' and `elseif' expressions.
#
# Extras is a list of items of the form
#
#	{ variable-name	value-expression }
#
# each causing
#
#	set variable-name [expr value-expression]
#
# to be executed just before an if-statement expression
# is evaluated.  The value-expression may refer to the
# globals in the globals list.
#
# `if' and `elseif' expressions should not change global
# variable values or call functions that do.
#
proc parse_block \
	{ block command_list_name globals \
	  { extras {} } } {

    if { [catch { llength $block }] } {
	error "instructions element is not a TCL\
	       list:\n    $block"
    }

    upvar $command_list_name c

    # In the following:
    #
    #   mode says what occurred immediately before, but
    #	     is often phrased according to what is
    #	     expected next.
    #
    #	     mode value:       what occurred before:
    #
    #	     if_expression     `if' or `elseif'
    #	     if_block	       `if EXP' or `elseif EXP'
    #	     after_if_block    `if EXP BLOCK' or
    #			       `elseif EXP BLOCK'
    #	     else_block	       `else'
    #	     after_else_block  `else BLOCK'
    #	     none	       none of the above
    #
    #	if_done means that some `if' or `elseif' expres-
    #           sion in a `if ... elseif ... elseif ...'
    #	        sequence has evaluated to true, and
    #		therefore subsequent `elseif's and the
    #		`else' should not execute.
    #
    #   if_value is the value of the last `if' or
    #		 `elseif' expression, or is false if
    #		 if_done was true when the expression
    #		 was to be evaluated.

    set mode none
    foreach item $block {
        switch $mode {
	    if_expression {
	        set if_value \
		    [expr { ! $if_done \
		            && \
		            [parse_block_eval_if \
			          $item $globals \
				  $extras] }]
		set mode if_block
	    }
	    if_block {
	        if { $if_value } {
		    if { [parse_block \
		              $item c $globals \
			      $extras] } {
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
		    if { [parse_block \
		              $item c $globals \
			      $extras] } {
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
		else -
		elseif {
		    error "else or elseif not preceded\
		           by `if' in\
			   instructions:\n    $item"
		}
		default {
		    if { ! [regexp {^[ \t]*#} $item] } {
			lappend c $item
		    }
		}
	    }
	}
    }
    return no
}

# Helper function to evaluate if-statement expression
# given a list of globals the expression may access.
# The extra list has items of the form:
#
#	{ variable-name	value-expression }
#
# causing `set variable-name [expr value-expression]'
# to be excuted before the if-statement expression is
# evaluated.
#
proc parse_block_eval_if \
	{ _expression_ _globals_ _extras_ } {
    foreach _g_ $_globals_ {
        global $_g_
    }
    foreach _item_ $_extras_ {
        set [lindex $_item_ 0] \
	    [expr [lindex $_item_ 1]]
    }
    return [expr $_expression_]
}

# Inline Code
# ------ ----


# Catch errors sourcing hpcm_judging.rc.
#
if { [catch {

    if { [info exists judging_directory] } {
        set HPCM_STANDALONE yes
	source_file $lib_directory/hpcm_judging.rc
    } else {

        set HPCM_STANDALONE no

	# Locate the directory containing the judging
	# parameters file.  This should be unique.  If
	# unique, the name is stored in the `judging_
	# directory' variable.
	#
	set judging_directory ""
	foreach __d__ \
	        ". .. ../.. ../../.. ../../../.." {
	    if { [file exists \
		       $__d__/hpcm_judging.rc] } {
		lappend judging_directory $__d__
	    }
	}

	# If directory containing judging parameters
	# file is unique, source the file.  Otherwise
	# call error.  Before sourcing file, run a
	# security check: insist that file not be
	# readable by `others', but only by group and
	# owner.
	#
	if { [llength $judging_directory] == 1 } {
	    source_file \
		$judging_directory/hpcm_judging.rc
	} elseif { [llength $judging_directory] == 0 } {
	    error "hpcm_judging.rc not found"
	} else {
	    set __m__ "Too many hpcm_judging.rc files:"
	    foreach __d__ $judging_directory {
		set __m__ "$__m__\n\
			  \     $__d__/hpcm_judging.rc"
	    }
	    error $__m__
	}
    }

    # Received mail subject field first word to program
    # mapping for autodispatch:
    #
    array set autodispatch_map {
	submit	autojudge
	get		autoinfo
    }

} caught_output] } {
    puts "ERROR: $caught_output"
    exit 2
}
