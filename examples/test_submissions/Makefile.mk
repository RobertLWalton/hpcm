# Makefile Defining Tests
#
# File:		Makefile.mk
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Wed Apr  3 10:49:21 EST 2002
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/04/03 15:46:26 $
#   $RCSfile: Makefile.mk,v $
#   $Revision: 1.9 $

# In the following, RCSfile and Revision are for
# get_count.
#
diff_replies:
	for x in *.reply; do \
	  filtered_diff $$x $S/$$x \
	    DATE RECEIVED_BY MESSAGE_ID \
	    TO REPLY_TO FROM ERROR SUBMISSION \
	    'RCSfile: (Makefile|get_count.reply),v' \
	    'Revision: [0-9]+.[0-9]+ \$$'; \
	done

test_email:	test_count_correct \
		test_count_incorrect \
		test_system_error \
		test_get \
		test_query \
		test_problem_name

test_informal:	test_count_correct \
		test_count_incorrect \
		test_system_error \
		test_get \
		test_query \
		test_problem_name

test_formal:	test_count_correct \
		test_count_incorrect \
		test_system_error \
		test_query \
		test_problem_name

test_count_correct:	${TEST_PREREQUISITES} \
	$S/count_correct.c.send \
	$S/count_correct.cc.send \
	$S/count_correct.java.send \
	$S/count_correct.p.send

test_count_incorrect:	${TEST_PREREQUISITES} \
	$S/count_crash.cc.send \
	$S/count_timeout.java.send \
	$S/count_too_much_output.c.send

test_system_error:	${TEST_PREREQUISITES} \
	$S/system_error.send \

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
	$S/bad_problem_name6.send
