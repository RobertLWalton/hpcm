# Common TCL Code for Judging
#
# File:		judging_common.tcl
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sat Mar 29 14:22:47 EST 2003
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2003/03/29 19:25:06 $
#   $RCSfile: judging_common.tcl,v $
#   $Revision: 1.112 $
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
#	File Functions
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
# log_mode and log_globally settings.  This can cause
# errors to be recorded in an error log file and to
# generate email notification.

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

# Number of calls to log_error during this program.
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
    if { $log_error_count > 1001 } {
        exit 2
    } elseif { $log_error_count > 1000 } {
	exit_cleanup
        exit 2
    } elseif { $log_error_count > $log_error_maximum } {
    	set log_mode none
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

	if { $count > 100 } {

	    # Desperation move.  Should never happen.
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
# All the values have the final \n stripped off.  All
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
# values global variables.  A non-multipart message is
# treated as if its body were the one and only part of
# the message.  It is an error if a multipart message
# has no parts, empty or not.  However, if there is an
# empty part, it is not an error, and the part as read
# by read_part_line is merely empty.
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
# The global message_terminator is used: see hpcm_
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
	    set message__part_error \
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
	return $message_x_hpcm_date
    } elseif { [regexp "\[^\ \t\n\r\f\]" \
                       $message_date] } {
	return $message_date
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
#	RECEIVED-FULL-HEADER
#	    Includes the ENTIRE header of the $received_
#	    file message.
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
    	   message_header \
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
        set mts $message_x_hpcm_test_subject
	puts $reply_ch   "X-HPCM-Test-Subject:$mts"
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
		puts $reply_ch \
		     [read_entire_file \
		          [lindex $command 1]]
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

    set not_final no
    foreach arg $args {
        if { $arg == "-notfinal" } {
	    set not_final yes
	} else {
	    error "Bad arg to send_reply: $arg"
	}
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
    if { $not_final } {
        file delete -force $reply_file+
    } else {
	file rename -force $reply_file+ $reply_file
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

# File Functions
# ---- ---------


# Remove noise from a file name.  Specifically:
#    // => /			/./ => /
#    /foo/../ => /		/.<end> => <end>
#    /foo/..<end> => <end>	/<end> => <end>
#
proc scrub_filename { name } {
    while { [regsub {//} $name {/} name] > 0 } {}
    while { [regsub {/([^/.]|\.[^/.])[^/]*/\.\./} \
		$name {/} name] > 0 } {}
    while { [regsub {/\./} $name {/} name] > 0 } {}
    regsub {/\.$} $name {} name
    regsub {/$} $name {} name
    regsub {/([^/.]|\.[^/.])[^/]*/\.\.$} $name {} name
    return $name
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

# Source a file if it exists.  Before sourcing file, run
# a security check: insist that file not be readable by
# `others', but only by group and owner.  The file is
# sourced in the caller's level.
#
proc source_file { filename } {
    if { [file exists $filename] } {

	if { ! [file readable $filename] } {
	    error "$filename not readable"
	}

	set __p__ \
	    [file attributes $filename -permissions]
	if { $__p__ & 4 } {
	    error "security violation: $filename is\
		   readable by `others'\n      \
		   you should execute chmod o-r\
		   $filename"
	}

	uplevel "source $filename"
    }
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
# visible, and the `abbreviations', `atoms', and
# `values' arrays must all be accessed in the
# environment of the caller of this function.  The
# `values' array will be initialized to 0's and a test
# evaluation of the logical expression in the caller's
# environment is conducted to find logical expression
# syntax errors.
#
proc compile_logical_expression \
    { expression abbreviations atoms values } {

    upvar $abbreviations abbreviation
    upvar $atoms atom
    upvar $values value

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
    source_file \
        $judging_directory/$judging_parameters_file
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
