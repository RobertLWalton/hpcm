# Makefile Defining Tests
#
# File:		Makefile.mk
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Thu Feb 12 21:13:15 EST 2004
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2004/02/13 02:48:03 $
#   $RCSfile: Makefile.mk,v $
#   $Revision: 1.23 $

extract_replies:	mbox
	rm -rf replies
	mkdir replies
	cd replies; hpcm_extract_test_replies <../mbox

# In the following, RCSfile and Revision are for
# get_count.
#
diff_replies:
	r=`cd replies; ls *.reply`; for x in $$r; do \
	  echo $$x; email_diff replies/$$x $R/$$x -b \
	    "From " "Received:" "Return-Path:" \
	    "Date:" "From:" "To:" "Reply-To:" \
	    "Message-ID" \
	    "Subject:.*unchecked-error" \
	    ">From " \
	    '#.*\$$Author: hc3 $$' \
	    '#.*\$$Date: 2004/02/13 02:48:03 $$' \
	    '#.*\$$RCSfile: Makefile.mk,v $$' \
	    '#.*\$$Revision: 1.23 $$' \
	    '//.*\$$Author: hc3 $$' \
	    '//.*\$$Date: 2004/02/13 02:48:03 $$' \
	    '//.*\$$RCSfile: Makefile.mk,v $$' \
	    '//.*\$$Revision: 1.23 $$'; \
	done

test_email:	test_count_correct \
		test_javaio_correct \
		test_count_incorrect \
		test_unformatted \
		test_system_error \
		test_get \
		test_query \
		test_problem_name

test_informal:	test_count_correct \
		test_javaio_correct \
		test_count_incorrect \
		test_formatted \
		test_system_error \
		test_get \
		test_query \
		test_problem_name

test_formal:	test_count_correct \
		test_javaio_correct \
		test_count_incorrect \
		test_formatted \
		test_system_error \
		test_query \
		test_problem_name

test_count_correct:	${TEST_PREREQUISITES} \
	$S/count_correct.c.send \
	$S/count_correct.cc.send \
	$S/count_correct.java.send \
	$S/count_correct.p.send \
	$S/count_correct.lsp.send

test_javaio_correct:	${TEST_PREREQUISITES} \
	$S/javaio_correct.send

test_count_incorrect:	${TEST_PREREQUISITES} \
	$S/count_incorrect_output.c.send \
	$S/count_incomplete_output.cc.send \
	$S/count_formatting_error.java.send \
	$S/count_crash.cc.send \
	$S/count_timeout.java.send \
	$S/count_too_much_output.c.send

test_formatted:	${TEST_PREREQUISITES} \
	$S/multipart_formatted.send

test_unformatted:	${TEST_PREREQUISITES} \
	$S/unformatted.send \
	$S/yahoo.send \
	$S/quoted_printable.send \
	$S/base64.send \
	$S/text_plain.send \
	$S/bad_multipart_encoding.send \
	$S/bad_multipart_type.send \
	$S/multipart_7bit.send \
	$S/multipart_base64.send \
	$S/multipart_pine.send \
	$S/multipart_quoted_printable.send \
	$S/multipart_simple_boundary.send

test_system_error:	${TEST_PREREQUISITES} \
	$S/system_error.send

test_get:	${TEST_PREREQUISITES} \
	$S/get_count.send \
	$S/get_bad_body.send \
	$S/get_illegal_filenames.send \
	$S/get_unreadable_files.send \
	$S/get_forbidden_files.send

test_query:	${TEST_PREREQUISITES} \
	$S/query.send \
	$S/bad_subject_format.send

test_problem_name:	${TEST_PREREQUISITES} \
	$S/bad_problem_name1.send \
	$S/bad_problem_name2.send \
	$S/bad_problem_name3.send \
	$S/bad_problem_name4.send \
	$S/bad_problem_name5.send \
	$S/bad_problem_name6.send \
	$S/bad_problem_name7.send \
	$S/bad_problem_name8.send \
	$S/bad_problem_name9.send
