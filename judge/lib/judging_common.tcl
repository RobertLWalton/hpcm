# Common TCL Code for Judging
#
# File:		judging_common.tcl
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Thu Sep 14 07:16:29 EDT 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: acm-cont $
#   $Date: 2000/09/14 14:05:43 $
#   $RCSfile: judging_common.tcl,v $
#   $Revision: 1.35 $
#

# Include this code in TCL program via:
#
#	set lib_directory \
#	    "[file dirname $argv0]/../lib"
#	source $lib_directory/judging_common.tcl
#	catch {
#
#	... your program ...
#	.., (do not change argc, argv0, argv)
#	... terminates with `exit 0' ...
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
# If you do not want any errors written to log files.
# See `log_error' below.

# The catch and `caught_error' function catches all
# program errors and causes them to be announced on the
# standard error output and be logged according to the
# log_mode setting.  This can cause errors to be
# recorded in an error file and to generate email
# notification.

# Default error log directory name.  For use if we
# cannot find judging parameters.
#
set default_log_directory ~/HPCM_Error_Log

# Judging parameters file name:
#
set judging_parameters_file hpcm_judging.rc

# Exit cleanup.  Called to do special cleanup before
# exit.  Default does nothing.  This proc may be
# redefined by program.
#
proc exit_cleanup {} {}

# Lock current directory by creating $dispatch_pid_file.
# Return `yes' is success, and `no' if failure.  Also,
# on success makes the current process a process group
# leader.
#
proc dispatch_lock {} {
    global dispatch_pid_file

    if { [create_file $dispatch_pid_file] } {

	# Make the current process a process group
	# leader, and store the current process ID in
	# $dispatch_pid_file.
	#
	set_current_pgid
	write_file $dispatch_pid_file [current_pid]
	return yes
    } else {
    	return no
    }
}

# Unlock the current directory by deleting any
# existing $dispatch_pid_file.
#
proc dispatch_unlock {} {
    global dispatch_pid_file
    file delete -force $dispatch_pid_file
}

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
# is `auto', the error is also emailed to any
# submitter/requester identified in $received_mail and
# to any log manager.
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
	   message_From_line message_from \
	   message_date message_subject \
	   message_x_hpcm_test_subject

    # Increment count of calls to log error and if
    # appropriate change log_mode to `none' or exit
    # from program.
    #
    incr log_error_count
    if { $log_error_count > $log_error_maximum } {
    	set log_mode none
    } elseif { $log_error_count > 1000 } {
        exit 2
    }

    # Write error to standard error output.
    #
    puts stderr "ERROR during $argv0 $argv"
    puts stderr ""
    puts stderr $error_output

    # If `log_mode' is `none', do not write to file, but
    # print errorCode an errorInfo to standard error
    # output and return.
    #
    if { $log_mode == "none" } {
	puts stderr ""
	puts stderr "errorCode: $errorCode"
	puts stderr "errorInfo:"
	puts stderr $errorInfo
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
    } elseif { [info exists $log_directory] \
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
        set d [clock_to_filename_date [clock seconds]]
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
    puts stderr ""
    puts stderr "Logging to $log_file"

    set log_ch [open $log_file a]
    puts $log_ch "----------------------------------"
    puts $log_ch "$argv0 $argv"
    puts $log_ch ""
    puts $log_ch "date: [clock format [clock seconds]]"
    puts $log_ch "pwd: [pwd]"
    puts $log_ch ""
    puts $log_ch $error_output
    puts $log_ch ""
    puts $log_ch "errorCode: $errorCode"
    puts $log_ch "errorInfo:"
    puts $log_ch $errorInfo
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

	    set dir_tail [file tail $log_dir]
	    set log_tail [file tail $log_file]
	    set log_tail $dir_tail/$log_tail

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

	    # Write `Subject:' field and announce error.
	    #
	    puts $mail_ch "Subject: $log_tail"
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
}

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
# If there are two copies of a field, only the last is
# recorded.  If there are no copies of a field in the
# message, the message_... global variable for that
# field is set to "".
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

    set line $first_line
    if { $line == "" } {
        set line [gets $ch]
	if { [eof $ch] } return
    }

    if { [regexp "^From\ " $line] } {
    	set message_From_line $line
    }

    while { "yes" } {
    	if { $line == "" } break

	set found_field no

	foreach fieldname $fields {
	    if { [regexp -nocase \
	                 "^(${fieldname}):(.*)\$" \
			 $line forget \
			 realname fieldvalue] } {

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

		set found_field yes
		break
	    }
	}

	if { $found_field == "no" } {
	    if { $message_header == "" } {
		set message_header $line
	    } else {
		set message_header \
		    "$message_header\n$line"
	    }
	    set line [gets $ch]
	}

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
           message_x_hpcm_from \
	   message_x_hpcm_signature \
           message_x_hpcm_signature_ok

    if { [llength $message_x_hpcm_signature] < 2 } {
    	set result no
    } else {
        set keyname \
	    [lindex $message_x_hpcm_signature 0]
        set signature \
	    [lindex $message_x_hpcm_signature 1]
	if { [catch
	        { set key \
		      $authentication_keys($keyname) \
		      }] } {
	    set result no
	} else {
	    set d "X-HPCM-Date:$message_x_hpcm_date\n"
	    set r "$message_x_hpcm_reply_to\n"
	    set r "X-HPCM-Reply-To:$r\n"
	    set v "$d$f$key\n"
	    set computed_signature \
	        [compute_signature $v]
	    if { $signature == $computed_signature } {
	    	set result yes
	    } else {
	        set result no
	    }
	}
    }

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

# Construct a mail reply file and send it to the sender
# of any received mail file.  The From, To, Reply-To,
# Subject, and Date fields of the received mail Header
# are copied at the end of the message.  The option
# `-all' causes the body of the received mail file to be
# included.  In any case this information is separated
# from the rest of the message by a blank space and a
# line of the form:
#
#     ---------------------- This message replies to:
#
# Non-option arguments are lines to be copied into the
# body of the reply.  The reply header is automatically
# produced.
#
# When the message is sent it is copied to the reply
# history file.
#
# Any previous reply file is deleted.
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
           message_From_line \
           message_from message_to message_date \
	   message_reply_to message_subject \
	   message_x_hpcm_test_subject

    # If -all is present, set `all_option' to `yes' and
    # shift the arguments left.
    #
    set all_option no
    if { [llength $args] >= 1 \
         && [lindex $args 0] == "-all" } {
        set all_option yes
	set args [lreplace $args 0 0]
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
    puts $reply_ch   "Subject: RE:$message_subject"
    if { $message_x_hpcm_test_subject != "" } {
	puts $reply_ch   "X-HPCM-Test-Subject:\
	                  $message_x_hpcm_test_subject"
    }
    puts $reply_ch   ""

    # Write part of body from arguments.
    #
    foreach line $args {
        puts $reply_ch   $line
    }

    # Append selected parts of $received_file header.
    #
    puts $reply_ch   ""
    puts $reply_ch   "------------------------------\
                      This message replies to:"
    #
    if { ! [regexp "\[^\ \t\n\]" $message_from] \
         || ! [regexp "\[^\ \t\n\]" $message_date]  } {
	puts $reply_ch ">$message_From_line"
    }
    if { [regexp "\[^\ \t\n\]" $message_to]  } {
	puts $reply_ch "To:$message_to"
    }
    if { [regexp "\[^\ \t\n\]" $message_from]  } {
	puts $reply_ch "From:$message_from"
    }
    if { [regexp "\[^\ \t\n\]" $message_date]  } {
	puts $reply_ch "Date:$message_date"
    }
    if { [regexp "\[^\ \t\n\]" $message_reply_to]  } {
	puts $reply_ch "Subject:$message_reply_to"
    }
    if { [regexp "\[^\ \t\n\]" $message_subject]  } {
	puts $reply_ch "Subject:$message_subject"
    }

    # If -all option, append message body from
    # $received_file.
    #
    if { $all_option } {

	puts $reply_ch ""

	while { "yes" } {
	    set line [gets $received_ch]
	    if { [eof $received_ch] } {
		break
	    } else {
		puts $reply_ch "$line"
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
# $reply_file.  After doing the above, this program
# renames `$reply_file+' to `$reply_file'.
#
proc send_reply {} {

    global reply_file reply_history_file

    # Delete any $reply_file.
    #
    if { [file exists $reply_file] } {
	file delete -force $reply_file
    }

    # Copy to the $reply_history_file.
    #
    set history_ch  [open $reply_history_file a]
    puts $history_ch "From [id user]@[info hostname]\
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

    # Rename $reply_file+
    #
    file rename -force $reply_file+ $reply_file
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

# Write one-line file.
#
proc write_file { filename line } {
    set file_ch [open $filename w]
    puts $file_ch $line
    close $file_ch
}

#### END OF FUNCTION DEFINITIONS ####

#### BEGINNING OF INLINE CODE ####

# Set interrupt signal to cause an error.
#
if { [info command signal] == "signal" } {
    signal error SIGINT
}

# Locate the directory containing the judging para-
# meters file.  This should be unique.  If unique, the
# name is stored in $judging_parameters_directory.
#
set judging_parameters_directory ""
foreach __d__ ". .. ../.. ../../.. ../../../.." {
    if { [file exists \
	       $__d__/$judging_parameters_file] } {
	lappend judging_parameters_directory $__d__
    }
}

# If directory containing judging parameters file
# is unique, source the file.  Otherwise call error.
#
if { [llength $judging_parameters_directory] == 1 } {
    set __d__ $judging_parameters_directory
    if { [file readable \
               $__d__/$judging_parameters_file] } {
	source $__d__/$judging_parameters_file
    } else {
	error "$__d__/$judging_parameters_file\
	       not readable"
    }
} elseif { [llength $judging_parameters_directory] \
           == 0 } {
    error "$judging_parameters_file not found"
} else {
    set __m__ "Too many $judging_parameters_file files:"
    foreach __d__ $judging_parameters_directory {
        set __m__ "$__m__\n\
	          \     $__d__/$judging_parameters_file"
    }
    error $__m__
}
