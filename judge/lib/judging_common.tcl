# Common TCL Code for Judging
#
# File:		judging_common.tcl
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Fri Jan 25 04:42:00 EST 2002
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/01/25 10:23:37 $
#   $RCSfile: judging_common.tcl,v $
#   $Revision: 1.70 $
#

# Table of Contents
#
#	Including this Code
#	Dispatch Locking Functions
#	Date Functions
#	Checked File Functions
#	Error Logging Functions
#	Message Header Functions
#	Reply Functions
#	Scoring Reply Functions
#	File Read/Write Functions
#	Flag Functions
#	Logical Expression Compilation
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
#	... terminates with `exit 0' or `error ...'
#
#	} caught_output
#	caught_error
#
# Put the command:
#
#	set log_globally yes
#
# in front of the `catch {' if you want errors to be
# logged in the log directory instead of the current
# directory.  Put
#
#	set log_mode none
#
# if you do not want any errors written to log files.
# See `log_error' below.
#
# The catch and `caught_error' function catches all
# program errors and causes them to be announced on the
# standard error output and be logged according to the
# log_mode setting.  This can cause errors to be
# recorded in an error log file and to generate email
# notification.

# Default error log directory name.  For use if we
# cannot find judging parameters.
#
set default_log_directory $env(HOME)/HPCM_Error_Log

# Judging parameters file name:
#
set judging_parameters_file hpcm_judging.rc

# Exit cleanup.  Called to do special cleanup before
# exit.  Default does nothing.  This proc may be
# redefined by program.
#
proc exit_cleanup {} {}

# Dispatch Locking Functions
# -------- ------- ---------


# Lock given directory by creating $dispatch_pid_file
# in the directory (defaults to current directory).
# Return `yes' if success, and `no' if failure.
#
proc dispatch_lock { { directory . } } {

    global dispatch_pid_file

    set lock_file $directory/$dispatch_pid_file

    if { [create_file $lock_file] } {

	# Store the current process ID in $dispatch_
	# pid_file.
	#
	write_file $lock_file [current_pid]
	return yes
    } else {
    	return no
    }
}

# Unlock given directory (defaults to current directory)
# by deleting any existing $dispatch_pid_file in the
# directory.
#
proc dispatch_unlock { { directory . } } {

    global dispatch_pid_file

    set lock_file $directory/$dispatch_pid_file

    file delete -force $lock_file
}

# Date Functions
# ---- ---------


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
    if { ! [regexp "^$n-$n-$n-$n:$n:$n\$" $date forget \
    	           year month day \
		   hour minute second] } {
	error "Not a legal filename date:    $date"
    }
    return [clock scan \
	    "$month/$day/$year $hour:$minute:$second"]
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
    log_error $caught_output
    exit_cleanup
    exit 0
}

# Function called to log an error when the program
# may want to continue.  The error is printed on the
# standard error output.  Unless `log_mode' is `none',
# the error is also written to a file.  If `log_mode'
# is `auto' or `auto+manual', the error is also emailed
# to any submitter/requester identified in the $receiv-
# ed_mail file, and if the `log_mode' is `auto', this
# mail is cc'ed to any log manager.
#
# When the error information is written to a file, a
# separate file is created for each error.  If the
# log_globally variable does not exist or does not equal
# `yes' and the current directory is writable, the file
# is written into the current directory.  Otherwise if
# the log_directory variable exists and names a direc-
# tory that exists or can be made and is writable once
# it is made, then the file is written into this direc-
# tory.  Otherwise the file is written into $default_
# log_directory, which is defined above as an emergency
# last resort.  The format of the file name is:
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
# `needs reply' flag.

# Number of calls to log_error in during this program.
#
set log_error_count 0
#
proc log_error { error_output } {

    global argv0 argv errorCode errorInfo \
	   default_log_directory \
	   log_directory log_globally log_mode \
	   log_error_count log_error_maximum \
	   log_manager received_file \
	   message_From_line message_to message_from \
	   message_date message_subject \
	   message_x_hpcm_test_subject \
	   needs_reply_flag_file

    # Increment count of calls to log error and if
    # appropriate change log_mode to `none' or exit
    # from program.
    #
    incr log_error_count
    if { $log_error_count > $log_error_maximum } {
    	set log_mode none
    } elseif { $log_error_count > 1001 } {
        exit 2
    } elseif { $log_error_count > 1000 } {
	exit_cleanup
        exit 2
    }

    # Write error to standard error output.
    #
    puts stderr "------------------------------"
    puts stderr "ERROR during $argv0 $argv"
    puts stderr "-----"
    puts stderr $error_output

    # If `log_mode' is `none', do not write to file, but
    # print errorCode and errorInfo to standard error
    # output and return.
    #
    if { $log_mode == "none" } {
	puts stderr "-----"
	puts stderr "errorCode: $errorCode"
	puts stderr "errorInfo follows:"
	puts stderr "-----"
	puts stderr $errorInfo
	puts stderr "------------------------------"
	return
    }

    # Compute $log_dir, the logging directory to be
    # used.  Make it if necessary.  Be sure it is
    # writable.
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

	if { $count > 10 } {

	    # Desparation move.  Should never happen.
	    #
	    set e LOGGING-FILENAME-GENERATION
	    set e ${e}-unchecked_error
	    set log_file $log_dir/$e
	    break
	}
    }

    # Write error to $log_file file.
    #
    puts stderr "-----"
    puts stderr "Logging to $log_file"

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
	# the $received_file header was read.
	#
	set to		""
	set cc		""
	set received_ch ""

	if { $log_mode == "auto" } {
	    set cc [string trim $log_manager]
	}

        if { $log_dir == "." \
	     && [file exists $received_file] } {

	    set received_ch [open $received_file r]
	    read_header $received_ch
	    close $received_ch

	    set to [compute_message_reply_to]
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

	    # Write reply-to field equal to $received_
	    # mail `To' field, if that exists.
	    #
	    if { [info exists message_to]
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
		puts $mail_ch \
		     "X-HPCM-Test-Subject:\
		     $message_x_hpcm_test_subject"
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
		puts $mail_ch "From: $message_from"
		puts $mail_ch "Date: $message_date"
		puts $mail_ch \
		     "Subject: $message_subject"
		puts $mail_ch ""
	    }

	    # If mail is to going to requester/sub-
	    # mitter, tell them what to do.
	    #
	    if { $to != "" } {
		puts $mail_ch \
		     "System is automatic without\
                      normal human monitoring."
		if { $cc != "" } {
		    puts $mail_ch \
			 "Please correct and resubmit,\
		          or wait for response from"
		    puts $mail_ch "$cc."
		} else {
		    puts $mail_ch \
			 "Please correct and resubmit,\
		          or contact the person"
		    puts $mail_ch \
			 "responsible for this site."
		}
	    }

	    # Close mail file and send mail.
	    #
	    close $mail_ch
	    puts stderr ""
	    puts stderr "Mailing $log_tail"
	    puts stderr "To $to $cc"
	    send_mail $log_file.mail
	}
    }
    puts stderr "------------------------------"

    set_flag $needs_reply_flag_file
}

# Message Header Functions
# ------- ------ ---------


# Read an email message header from the channel. If
# given, the first line of the header is the second
# argument (note that email header lines cannot be
# empty).  Stop reading at the first empty line and
# discard that line.
#
# The results are returned in global variables:
#
#	message_header		All the lines of the
#				header
#	message_From_line	The first line if it
#				begins with `^From\ '.
#	message_from		`From:' field value.
#	message_to		`To:' field value.
#	message_reply_to	`Reply-To:' field value.
#	message_subject		`Subject:' field value.
#	message_date		`Date:' field value.
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
# All the values have the final \n stripped off.  All
# the field values have the `field-name:' stripped off.
# Field values may be multi-line, but if there are two
# copies of a field, only the last is recorded.  If
# there are no copies of a field in the message, the
# message_... global variable for that field is set to
# "".
#
# If a third argument is given, it is a list of field
# names (written with all lower case letters) that
# are to be deleted from the header as it is read, and
# thereby completely ignored.  The typical value is:
#
#	{ x-hpcm-signature-ok }
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
    set message_x_hpcm_date		""
    set message_x_hpcm_reply_to		""
    set message_x_hpcm_signature	""
    set message_x_hpcm_signature_ok	""
    set message_x_hpcm_test_subject	""

    set fields "from to date reply-to subject\
                x-hpcm-reply-to x-hpcm-date\
		x-hpcm-signature x-hpcm-signature-ok \
		x-hpcm-test-subject"

    # Get first line of message.
    #
    set line $first_line
    if { $line == "" } {
        set line [gets $ch]
	if { [eof $ch] } return
    }

    # Set `From ' if found line.
    #
    if { [regexp "^From\ " $line] } {
    	set message_From_line $line
    }

    # Loop until empty line looking for fields listed
    # in the `fields' variable above.
    #
    while { "yes" } {
    	if { $line == "" } break

	set found_field no

	# Loop through field names we are looking for.
	#
	foreach fieldname $fields {
	    if { [regexp -nocase \
	                 "^(${fieldname}):(.*)\$" \
			 $line forget \
			 realname fieldvalue] } {

		# Come here when line is for the
		# field we are looking for.

		# Process any extra lines in a
		# multi-line field, and read first
		# line AFTER field value (or EOF).
		#
		while { "yes" } {
		    set line [gets $ch]
		    if { [eof $ch] \
			 || ! [regexp \
			           "^\[\ \t\]" \
				   $line] } \
			break;
		    set fieldvalue \
			"$fieldvalue\n$line"
		}

		# If field is not to be omitted, set
		# the global variables for it and for
		# the entire message header.
		#
		if { [lsearch -exact \
		              $omit_fields \
			      $fieldname]  < 0 } {
		    set varname message_$fieldname
		    regsub -all -- "-" $varname "_" \
		           varname
		    set $varname $fieldvalue
		    set f "${realname}:${fieldvalue}"
		    if { $message_header == "" } {
		    	set message_header $f
		    } else {
			set message_header \
			    "$message_header\n$f"
		    }
		}

		# Indicate we found a field for the
		# line and got next line after field,
		# and stop looking at field names.
		#
		set found_field yes
		break
	    }
	}

	# If we did not find a field matching line,
	# store line in message header and get next
	# line.
	#
	if { $found_field == "no" } {
	    if { $message_header == "" } {
		set message_header $line
	    } else {
		set message_header \
		    "$message_header\n$line"
	    }
	    set line [gets $ch]
	}

	# If next line is really and EOF, break.
	#
	if { [eof $ch] } break
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

    if { [regexp "\[^\ \t\n\]" \
	         $message_x_hpcm_reply_to] } {
	return $message_x_hpcm_reply_to
    } elseif { [regexp "\[^\ \t\n\]" \
                       $message_reply_to] } {
	return $message_reply_to
    } elseif { [regexp "\[^\ \t\n\]" \
                       $message_from] } {
	return $message_from
    } elseif { [llength $message_From_line] >= 2 } {
	return [lindex $message_From_line 1]
    } else {
    	return "UNKNOWN"
    }
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

    if { [regexp "\[^\ \t\n\]" \
	         $message_x_hpcm_date] } {
	return $message_x_hpcm_date
    } elseif { [regexp "\[^\ \t\n\]" \
                       $message_date] } {
	return $message_date
    } elseif { [llength $message_From_line] >= 3 } {
	return [lreplace $message_From_line 0 1]
    } else {
    	return [clock format [clock seconds]]
    }
}

# Compute whether the header just read by read_header
# is authentic.  Return `yes' if it is, `no' if it is
# not.  If the header does not already have the same
# answer recorded in its last `X-HPCM-Signature-OK'
# field, add that field with the newly computed value
# to the end of the header stored in the message_...
# global variables.
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
    if { [llength $message_x_hpcm_signature] < 2 } {
    	set result no
    } else {
        set keyname \
	    [lindex $message_x_hpcm_signature 0]
        set signature \
	    [lindex $message_x_hpcm_signature 1]
	if { [catch \
	        { set key \
		      $authentication_keys($keyname) \
		      }] } {
	    # If we have no key for the keyname,
	    # result is no.
	    #
	    set result no
	} else {

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

# Reply Functions
# ----- ---------


# Construct a mail reply file and send it to the sender
# of any received mail file.
#
# The reply header is automatically produced using the
# following options.  The -cc option causes the reply to
# be cc'ed to the `reply_manager' if that is not "".
# The -error option changes the reply subject from
# `RE:...' to `Errors In:...'.
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
#	    Subject fields of the $received_file
#	    message.
#
#	RECEIVED-BODY
#	    Includes the body of the $received_file mes-
#	    sage.  This command can appear at most once.
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
# the file `$reply_file+'.  This reply can then be
# manually edited before sending it with `send_reply'.
#
proc compose_reply { args } {

    global received_file reply_file \
           message_From_line message_to \
           message_from message_date \
	   message_reply_to message_subject \
	   message_x_hpcm_test_subject \
	   reply_manager

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

    # Read $received_file header.
    #
    set received_ch [open $received_file r]
    read_header $received_ch

    # Delete any existing $reply_file+ and open that
    # file for writing.
    #
    if { [file exists $reply_file+] } {
    	file delete -force $reply_file+
    }
    set reply_ch    [open $reply_file+ w]

    # Write header.
    #
    puts $reply_ch   "To:[compute_message_reply_to]"
    if { $cc_option && $reply_manager != "" } {
	puts $reply_ch "Cc: $reply_manager"
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
	puts $reply_ch   "X-HPCM-Test-Subject:\
	                  $message_x_hpcm_test_subject"
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
		    incr l 2
		    puts $reply_ch \
		    	 "[string range $bar $l end] $s"
		}
	    }
	    INPUT {
		puts $reply_ch \
		     [read_entire_file \
		          [lindex $command 1]]
	    }
	    RECEIVED-HEADER {

		set nws "\[^\ \t\n\]"
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

# This function copies the `$reply_file+' file into
# into the `$reply_history_file' file and then emails
# the `$reply_file+' file.
#
# Before doing the above, this program deletes any
# $reply_file.
#
# After doing the above, this program renames
# `$reply_file+' to `$reply_file', unless the -notfinal
# option is given, in which case this program simply
# deletes `$reply_file+'.
#
proc send_reply { args } {

    global reply_file reply_history_file

    if { [llength $args] != 0 \
	 && $args != "-notfinal" } {
	error "Bad args to send_reply: $args"
    }

    # Delete any $reply_file.
    #
    if { [file exists $reply_file] } {
	file delete -force $reply_file
    }

    # Copy to the $reply_history_file.
    #
    set history_ch  [open $reply_history_file a]
    puts $history_ch "From [account_name]@[host_name]\
		      [clock format [clock seconds]]"
    put_file $reply_file+ $history_ch

    # An empty line is needed before the next `From'
    # line so the `From' line will be recognized.
    #
    puts $history_ch ""
    close $history_ch

    # Send the $reply_file+.  If there is a bad `To:'
    # address, there may be an error, which will
    # usually be logged by an error file in the
    # current directory.  Otherwise a bad address
    # will cause return mail from the mailer
    # daemon.
    #
    send_mail $reply_file+

    # Delete or rename $reply_file+
    #
    if { $args == "-notfinal" } {
        file delete -force $reply_file+
    } else {
	file rename -force $reply_file+ $reply_file
    }
}

# Read lines from channel.  Return `yes' if eof encoun-
# tered before a non-blank line.  Return `no' if non-
# blank line encountered.
#
proc blank_body { ch } {

    while { "yes" } {
	set line [gets $ch]
	if { [eof $ch] } {
	    return yes
	} elseif { [regexp "^\[\ \t\]*\$" $line] } {
	    # blank lines are ok
	} else {
	    return no
	}
    }
}

# Scoring Reply Functions
# ------- ----- ---------

# Function to process $response_instructions_file value
# and produce a $reply_file+ file by calling compose_
# reply.  Documentation of the $response_instructions_
# file value is found in hpcm_judging.rc with the
# default_response_instructions global variable.  The
# value of this global variable is added to the end of
# the response_instructions argument given to this
# function.
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
# The global variable `problem' must be set to the
# problem name.
#
# Returns a list of commands whose execution merely
# serves to specify options for disposition of
# $reply_file+.  Specifically, any FINAL, NOT-FINAL,
# NO-REPLY, or EDIT commands that are executed are
# merely returned in this list, and have no direct
# effect on the contents of $reply_file+.  This list
# may be passed to send_response.
#
proc compose_response \
    { response_instructions \
      { compose_reply_options "" } } {

    global default_response_instructions

    set commands ""
    parse_block \
        [concat $response_instructions \
		$default_response_instructions] \
	commands
    return [execute_response_commands \
    	        $compose_reply_options $commands]
}

# Function to send a $reply_file+ produced by compose_
# response.  Takes as input the list of commands
# returned by compose_response.  Does the following
#
# If the list contains NO-REPLY: does nothing.
# Else if the list contains FINAL: calls send_reply.
# Else if the list contains NOT-FINAL: calls
#      send_reply -notfinal.
# Else calls error.
#
proc send_response { commands } {
    if { [lcontain $commands NO-REPLY] } {
    	return
    } elseif { [lcontain $commands FINAL] } {
    	send_reply
    } elseif { [lcontain $commands NOT-FINAL] } {
    	send_reply -notfinal
    } else {
        error "response instructions do not contain\
	       NO-REPLY, FINAL, or NOT-FINAL."
    }
}

# Function to execute the if-statements in a block and
# add to the list of commands.  Stop at end of block or
# at EXIT.  Return `yes' if EXIT found, `no' otherwise.
#
proc parse_block { block commands } {

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

    global reponse_instructions_file problem \
    	   auto_score manual_score proposed_score

    set processed_commands ""
    set return_commands ""
    foreach command $commands {
	if { [catch { set length [llength $command] }] \
		    } {
	    response_error $command
	}
        switch -- [lindex $command 0] {
	    FINAL	-
	    NOT-FINAL	-
	    NO-REPLY	-
	    EDIT	{
	    	if { $length != 1 } {
		    response_error $command
		}
	        lappend return_commands $command
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
	        set new_command [lindex $command 0]
		switch $new_command {
		    LINE {
			if { $length != 2 } {
			    response_error $command
		    }
		    BAR {
			if { $length > 2 } {
			    response_error $command
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
		           $string $proposed_score string
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
		       file, unrecognized command:\
		       $command"
	    }
	}
    }

    eval compose_reply $compose_reply_options \
    		       $processed_commands
    return $return_commands
}
proc response_error { command } {
    global reponse_instructions_file
    error "In $response_instructions_file file value,\
           badly formatted command: $command."
}

# File Read/Write Functions
# ---- ---------- ---------


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

# Write entire file to channel.  If number of lines in
# file is greater than third argument, do not write
# more lines than specified by the third argument, but
# in place of the omitted lines write the line:
#
#	... THERE WERE # MORE LINES ...
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
# string terminates with a line feed, except the
# line feed is omitted from the last line (so an
# empty file is indistiguishable from a file with
# a single empty line).
#
proc read_entire_file { filename } {
    set file_ch [open $filename r]
    set result [gets $file_ch]
    while { "yes" } {
	set line [gets $file_ch]
	if { [eof $file_ch] } break
	set result "$result\n$line"
    }
    close $file_ch
    return $result
}

# Flag Functions
# ---- ---------


# Set flag.
#
proc set_flag { flagfilename } {
    global flag_directory
    file mkdir $flag_directory
    set file_ch [open $flag_directory/$flagfilename w]
    close $file_ch
}

# Test flag.
#
proc test_flag { flagfilename } {
    global flag_directory
    return [file exists $flag_directory/$flagfilename]
}

# clear flag.
#
proc clear_flag { flagfilename } {
    global flag_directory
    file delete -force $flag_directory/$flagfilename
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
#
# The logical expression is a list whose elements are
# tokens.  Before any token is processed, it is looked
# up in the abbreviations array, and any value found
# replaces the token.  Note that the value must be a
# single token.  Thus if `AB(T)' is defined, where AB
# is the `abbreviations' parameter to this function and
# T is a token in the logical expression (an element of
# the logical expression viewed as a list), then the
# value of `AB(T)' replaces T in the logical expression.
# This rule is NOT recursive.
#
# The tokens `0', `1', `(', `)', `|', `&', `^', and `!'
# are non-atoms.  All other tokens are atoms.
#
# Note that the compiled expression must be evaluated
# in an environment in which the `values' array is
# visible, and the `abbreviations' and `atoms' arrays
# are accessed in the environment of the caller of this
# function.
#
proc compile_logical_expression \
    { expression abbreviations atoms values } {

    upvar $abbreviations abbreviation
    upvar $atoms atom

    set n 0
    set depth 0
    set logical_expression ""
    foreach token $expression {

	# Replace abbreviation tokens.
	#
	if { [info exists abbreviation($token)] } {
	    set token $abbreviation($token)
	}

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

    return $logical_expression
}

# Inline Code
# ------ ----


# Locate the directory containing the judging para-
# meters file.  This should be unique.  If unique, the
# name is stored in the `judging_directory' variable.
#
set judging_directory ""
foreach __d__ ". .. ../.. ../../.. ../../../.." {
    if { [file exists \
	       $__d__/$judging_parameters_file] } {
	lappend judging_directory $__d__
    }
}

# If directory containing judging parameters file
# is unique, source the file.  Otherwise call error.
# Before sourcing file, run a security check: insist
# that file not be readable by `others', but only by
# group and owner.
#
if { [llength $judging_directory] == 1 } {
    set __d__ $judging_directory
    if { [file readable \
               $__d__/$judging_parameters_file] } {
	set f $__d__/$judging_parameters_file
	set p [file attributes $f -permissions]
	if { $p & 4 } {
	    error "security violation: $f is readable\
	    	   by `others'\n      \
		   you should execute chmod o-r $f"
	}
	source $f
    } else {
	error "$__d__/$judging_parameters_file\
	       not readable"
    }
} elseif { [llength $judging_directory] == 0 } {
    error "$judging_parameters_file not found"
} else {
    set __m__ "Too many $judging_parameters_file files:"
    foreach __d__ $judging_directory {
        set __m__ "$__m__\n\
	          \     $__d__/$judging_parameters_file"
    }
    error $__m__
}

# Set signals to cause errors.
#
make_signals_errors

# Received mail subject field first word to program
# mapping for autodispatch:
#
array set autodispatch_map {
    submit	autojudge
    get		autoinfo
}
