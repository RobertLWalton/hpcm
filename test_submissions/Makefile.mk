# Makefile Defining Tests
#
# File:		Makefile.mk
# Author:	Bob Walton (walton@seas.harvard.edu)
# Date:		Thu May  2 07:30:09 EDT 2013
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2013/05/02 14:51:58 $
#   $RCSfile: Makefile.mk,v $
#   $Revision: 1.1 $

CORRECT_TESTS = \
    reverser_correct_cc \
    reverser_correct_c \
    reverser_correct_java \
    reverser_correct_lsp

INCORRECT_TESTS = \
    reverser_formatting_error_cc \
    reverser_incomplete_output_cc \
    reverser_incorrect_output_cc \
    reverser_no_output_crash_cc \
    reverser_segment_fault_crash_cc \
    reverser_too_much_cpu_time_cc \
    reverser_too_much_output_cc

EMAIL_TESTS = \
    bad_multipart_encoding \
    bad_multipart_type \
    multipart_reverser_correct_java

EXTRACTION_TESTS = \
    bad_problem_name1 \
    bad_problem_name2 \
    bad_problem_name3 \
    bad_problem_name4 \
    bad_problem_name5 \
    bad_problem_name6 \
    bad_problem_name7 \
    bad_problem_name8 \
    bad_problem_name9 \
    bad_subject_format

%.send:		$S/%.mail
	testsubmit $S/$*.mail

test_basics:	test_correct test_incorrect

test_correct:	${CORRECT_TESTS:%=%.send}

test_incorrect:	${INCORRECT_TESTS:%=%.send}

test_extraction:	${EXTRACTION_TESTS:%=%.send}

test_email:	${EMAIL_TESTS:%=%.send}
