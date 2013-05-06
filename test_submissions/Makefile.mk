# Makefile Defining Tests
#
# File:		Makefile.mk
# Author:	Bob Walton (walton@seas.harvard.edu)
# Date:		Sun May  5 16:18:05 EDT 2013
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2013/05/06 08:59:21 $
#   $RCSfile: Makefile.mk,v $
#   $Revision: 1.2 $

CORRECT_TESTS = \
    $S/reverser_correct_cc.mail \
    $S/reverser_correct_c.mail \
    $S/reverser_correct_java.mail \
    $S/reverser_correct_lsp.mail

INCORRECT_TESTS = \
    $S/reverser_formatting_error_cc.mail \
    $S/reverser_incomplete_output_cc.mail \
    $S/reverser_incorrect_output_cc.mail \
    $S/reverser_no_output_crash_cc.mail \
    $S/reverser_segment_fault_crash_cc.mail \
    $S/reverser_too_much_cpu_time_cc.mail \
    $S/reverser_too_much_output_cc.mail

EMAIL_TESTS = \
    $S/bad_multipart_encoding.mail \
    $S/bad_multipart_type.mail \
    $S/multipart_reverser_correct_java.mail

EXTRACTION_TESTS = \
    $S/bad_problem_name1.mail \
    $S/bad_problem_name2.mail \
    $S/bad_problem_name3.mail \
    $S/bad_problem_name4.mail \
    $S/bad_problem_name5.mail \
    $S/bad_problem_name6.mail \
    $S/bad_problem_name7.mail \
    $S/bad_problem_name8.mail \
    $S/bad_problem_name9.mail \
    $S/bad_subject_format.mail
