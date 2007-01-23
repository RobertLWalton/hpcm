# Makefile Defining Tests
#
# File:		Makefile.mk
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Mon Jan 22 07:12:59 EST 2007
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2007/01/23 02:18:26 $
#   $RCSfile: Makefile.mk,v $
#   $Revision: 1.37 $

extract_replies:	mbox
	rm -rf replies
	mkdir replies
	cd replies; hpcm_extract_test_replies <../mbox

# In the following, RCSfile and Revision are for
# get_count.
#
diff_replies:
	r=`cd replies; ls *.reply`; for x in $$r; do \
	  echo $$x; e=':.*\$$'; \
	     email_diff replies/$$x $R/$$x \
	    "From " "Received:" "Return-Path:" \
	    "Date:" "From:" "To:" "Reply-To:" "Cc:" \
	    "Status:" "Message-ID:" \
	    "Subject:.*unchecked-error" \
	    "X-HPCM-Date:" "X-HPCM-Reply-To:" \
	    "X-HPCM-Signature:" \
	    -body \
	    ">From " \
	    "*Date:" "*From:" "*To:" "*Reply-To:" \
	    "*Cc:" "*Received:" "*Return-Path:" \
	    "*Status:" "*Message-ID:" \
	    "X-HPCM-Date:" "X-HPCM-Reply-To:" \
	    "X-HPCM-Signature:" \
	    '[ \t]*\$$Author'$$e \
	    '[ \t]*\$$Date'$$e \
	    '[ \t]*\$$RCSfile'$$e \
	    '[ \t]*\$$Revision'$$e \
	    '#.*\$$Author'$$e \
	    '#.*\$$Date'$$e \
	    '#.*\$$RCSfile'$$e \
	    '#.*\$$Revision'$$e \
	    '//.*\$$Author'$$e \
	    '//.*\$$Date'$$e \
	    '//.*\$$RCSfile'$$e \
	    '//.*\$$Revision'$$e \
	    'a response from .*@'; \
	done

BASIC_TESTS = \
	test_count_correct.test \
	test_javaio_correct.test \
	test_count_incorrect.test \
	test_system_error.test \
	test_query.test \
	test_problem_name.test

test_email:	${BASIC_TESTS:.test=.email} \
		test_unformatted.email \
		test_get.email

test_local:	${BASIC_TESTS:.test=.local} \
		test_formatted.local

test_informal:	${BASIC_TESTS:.test=.local} \
		test_formatted.local \
		test_get.local

test_forbidden:	$S/get_forbidden_files.local

test_formal:	test_local

%.email:	%.mail ./Contest_Address ./mail
	( echo To: `cat Contest_Address`; cat $*.mail) \
	    | ${SENDMAIL}
	sleep 2

%.local:	%.mail ./mail
	hpcm_sendmail <$*.mail
	sleep 2

COUNT_CORRECT_FILES = \
	$S/count_correct.c.mail \
	$S/count_correct.cc.mail \
	$S/count_correct.java.mail \
	$S/count_correct.lsp.mail

JAVAIO_CORRECT_FILES = \
	$S/javaio_correct.mail

COUNT_INCORRECT_FILES = \
	$S/count_incorrect_output.c.mail \
	$S/count_incomplete_output.cc.mail \
	$S/count_formatting_error.java.mail \
	$S/count_crash.cc.mail \
	$S/count_timeout.java.mail \
	$S/count_timeout.c.mail \
	$S/count_too_much_output.c.mail

FORMATTED_FILES = \
	$S/multipart_formatted.mail

UNFORMATTED_FILES = \
	$S/unformatted.mail \
	$S/yahoo.mail \
	$S/quoted_printable.mail \
	$S/base64.mail \
	$S/text_plain.mail \
	$S/bad_multipart_encoding.mail \
	$S/bad_multipart_type.mail \
	$S/multipart_7bit.mail \
	$S/multipart_base64.mail \
	$S/multipart_pine.mail \
	$S/multipart_quoted_printable.mail \
	$S/multipart_simple_boundary.mail

SYSTEM_ERROR_FILES = \
	$S/system_error.mail

GET_FILES = \
	$S/get_count.mail \
	$S/get_bad_body.mail \
	$S/get_illegal_filenames.mail \
	$S/get_unreadable_files.mail

QUERY_FILES = \
	$S/query.mail \
	$S/bad_subject_format.mail

PROBLEM_NAME_FILES = \
	$S/bad_problem_name1.mail \
	$S/bad_problem_name2.mail \
	$S/bad_problem_name3.mail \
	$S/bad_problem_name4.mail \
	$S/bad_problem_name5.mail \
	$S/bad_problem_name6.mail \
	$S/bad_problem_name7.mail \
	$S/bad_problem_name8.mail \
	$S/bad_problem_name9.mail

test_count_correct.local:	\
	${COUNT_CORRECT_FILES:.mail=.local}

test_count_correct.email:	\
	${COUNT_CORRECT_FILES:.mail=.email}

test_javaio_correct.local:	\
	${JAVAIO_CORRECT_FILES:.mail=.local}

test_javaio_correct.email:	\
	${JAVAIO_CORRECT_FILES:.mail=.email}

test_count_incorrect.local:	\
	${COUNT_INCORRECT_FILES:.mail=.local}

test_count_incorrect.email:	\
	${COUNT_INCORRECT_FILES:.mail=.email}

test_formatted.local:	\
	${FORMATTED_FILES:.mail=.local}

test_formatted.email:	\
	${FORMATTED_FILES:.mail=.email}

test_unformatted.local:	\
	${UNFORMATTED_FILES:.mail=.local}

test_unformatted.email:	\
	${UNFORMATTED_FILES:.mail=.email}

test_system_error.local:	\
	${SYSTEM_ERROR_FILES:.mail=.local}

test_system_error.email:	\
	${SYSTEM_ERROR_FILES:.mail=.email}

test_get.local:	\
	${GET_FILES:.mail=.local}

test_get.email:	\
	${GET_FILES:.mail=.email}

test_query.local:	\
	${QUERY_FILES:.mail=.local}

test_query.email:	\
	${QUERY_FILES:.mail=.email}

test_problem_name.local:	\
	${PROBLEM_NAME_FILES:.mail=.local}

test_problem_name.email:	\
	${PROBLEM_NAME_FILES:.mail=.email}
