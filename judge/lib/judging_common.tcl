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
#   $Date: 2000/08/18 12:08:02 $
#   $RCSfile: judging_common.tcl,v $
#   $Revision: 1.2 $
#

# Include this code in TCL program via:
#
#	set lib_directory \
#	    "[file dirname $argv0]/../lib"
#	source "$lib_directory/judging_common.tcl"

# Judging parameters file name:
#
set judging_parameters_file "hpcm_judging.rc"

# Function that prints a fatal error on the standard
# output and exits the program with an error code.  Each
# argument is printed on a separate line.
#
proc fatal_error { args } {
    puts "FATAL ERROR: [lindex $args 0]"
    set args [lreplace $args 0 0]
    foreach m $args {
    	puts "             $m"
    }
    exit 1
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
	lappend judging_parameters_list \
		"$d/$judging_parameters_file"
	set judging_parameters_directory $d
    }
}

# If directory containing judging parameters file
# is unique, source the file, and set juding_parameters_
# directory to the directory name.  Otherwise call
# fatal_error.
#
if { [llength $judging_parameters_file_list] == 1 } {
    if { [file readable \
               $judging_parameters_file_list] } {
	source $judging_parameters_file_list
    } else {
	fatal_error \
	    "$judging_parameters_file_list\
	     not readable"
    }
} elseif { [llength $judging_parameters_file_list] \
           == 0 } {
    fatal_error \
	"$judging_parameters_file not found"
} else {
    fatal_error \
	"Too many $judging_parameters_file files:" \
	$judging_parameters_file_list
}
