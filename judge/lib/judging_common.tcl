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
#   $Date: 2000/08/18 11:26:08 $
#   $RCSfile: judging_common.tcl,v $
#   $Revision: 1.1 $
#

# Include this code in TCL program via:
#
#	set lib_directory \
#	    "[file dirname $argv0]/../lib"
#	source "$lib_directory/judging_common.tcl"

# Judging parameters file name:
#
set judging_parameters_file "hpcm_judging.rc"

# List of directories to search for judging parameters
# file:
#
set judging_parameters_directories \
    ". .. ../.. ../../.. ../../../.."

proc fatal_error { args } {
    puts "ERROR: [lindex $args 0]"
    set args [lreplace $args 0 0]
    foreach m $args {
    	puts "       $m"
    }
    exit 1
}

# Set interrupt signal to cause an error.
#
if { [info command signal] == "signal" } {
    signal error SIGINT
}

# Locate the judging parameters file name.  This
# will be returned as value of the initialize_
# program function.
#
set judging_parameters_list ""
foreach d $judging_parameters_directories {
    if { [file exists \
	       "$d/$judging_parameters_file"] } {
	lappend judging_parameters_list \
		"$d/$judging_parameters_file"
    }
}

if { [llength $judging_parameters_list] == 1 } {
    if { [file readable $judging_parameters_list] } {
	source $judging_parameters_list
    } else {
	fatal_error \
	    "$judging_parameters_list not readable"
    }
} elseif { [llength $judging_parameters_list] == 0 } {
    fatal_error \
	"$judging_parameters_file not found"
} else {
    fatal_error \
	"Too many $judging_parameters_file files:" \
	$judging_parameters_list
}
