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
#   $Date: 2000/08/22 16:43:09 $
#   $RCSfile: judging_common.tcl,v $
#   $Revision: 1.12 $
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
# The error information is written as a separate
# file in $log_directory.  If $log_directory is not
# usable, $default_log_directory defined above is used
# instead.  The format of the file name is:
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

    puts "ERROR caught for $argv0 $argv"
    puts $error_output

    if { [info exists log_directory] \
	 && $log_directory != "" } {
        set log_dir "$log_directory"
    } else {
    	set log_dir $default_log_directory
    }
    if { [catch { file mkdir $log_dir } ] \
	 || ! [file writable $log_dir] } {
	set log_dir $default_log_directory
	file mkdir $log_dir
    }

    set count 0
    while { "yes" } {
        set t [clock_to_filename_date [clock seconds]]
        set u [format %06d \
		  [expr { [clock clicks] % 1000000 } ]]
        set p [file tail $argv0]
        set log_file \
	    "$log_dir/${t}-\[<EU>\]-{<${p}>}-${u}"

	if { ! [catch { exec lockfile -0 -r 0 \
	                              $log_file } ] } {
	    chmod u+w $log_file
	    ftruncate $log_file 0
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

    global received_file reply_file Header_line_regexp

    set all_option no
    if { [llength $args] >= 1 \
         && [lindex $args 0] == "-all" } {
        set all_option yes
	set args [lreplace $args 0 0]
    }

    set to [dirname_to_sender [file tail [pwd]]

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
    puts $reply_ch   "------------------------------\
                      This message replies to:"

    set in_header yes
    set From_line_found no
    while { "yes" } {
	set line [gets $received_ch]
	if { [eof $received_ch] } {
	    break
	} elseif { [regexp {^From\ } $line] } {
	    puts $reply_ch ">$line"
	    set From_line_found yes
	} elseif { $in_header == "no" } {
	    puts $reply_ch "$line"
	} elseif { ! [regexp $Header_line_regexp \
	                     $line] } {
	    if { $all_option == "no" } break;
	    set in_header no
	    puts $reply_ch "$line"
	} elseif { [regexp {^(Subject|To):} $line] } {
	    puts $reply_ch "$line"
	} elseif { $From_line_found == "no" \
	           && [regexp {^(From|Date):} $line] } {
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

    global reply_file history_file sendmail_program

    set to [dirname_to_sender [file tail [pwd]]

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
# `Subject:' and any preceding or following whitespace
# are stripped from the result.
#
# If no `Subject:' line is found in the header, or more
# than one `Subject:' lines are found, or if the -nobody
# argument is given an the body contains a non-blank
# line, then error is called with an appropriate error
# message.
#
proc find_subject { args } {

    global Header_line_regexp received_file

    if { $args == "" } {
	set body yes
    } elseif { $args == "-nobody" } {
	set body yes
    } else {
        error "Bad arguments to find_subject: $args"
    }

    set received_ch [open $received_file r]

    # Read header
    #
    set subject_found no
    while { "yes" } {
        set line [gets $received_ch]
	if { [eof $received_ch] } {
	    break
	} elseif { [regexp \
		"^Subject:\[\ \t\]*(\[^\ \t\].*)\$" \
		$line all subject] } {
	    if { $subject_found } {
	        error "More than one `Subject:' line"
	    }
	    set subject_found yes
	} elseif { [regexp $Header_line_regexp $line] } {
	    # Header line is OK
	} else {
	    break
	}
    }

    if { $subject_found == "no" } {
	error "No `Subject:' line found"
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

# Edit a file.
#
proc edit_file { filename } {
    global edit_program
    catch { eval $edit_program $filename }
}

# View a file.
#
proc view_file { filename } {
    global view_program
    catch { eval $view_program $filename }
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
