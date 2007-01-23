# Makefile Defining Tests
#
# File:		Makefile.mk
# Author:	Bob Walton (walton@deas.harvard.edu)
# Date:		Tue Jan 23 03:50:39 EST 2007
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2007/01/23 09:37:45 $
#   $RCSfile: Makefile.mk,v $
#   $Revision: 1.38 $

BASIC_TESTS = \
	test_count_correct \
	test_javaio_correct \
	test_count_incorrect \
	test_system_error \
	test_query \
	test_problem_name

test_email:	${BASIC_TESTS:%=%.email} \
		test_unformatted.email \
		test_get.email

diff_email:	${BASIC_TESTS:%=%.diff} \
		test_unformatted.diff \
		test_get.diff

test_local:	${BASIC_TESTS:%=%.local} \
		test_formatted.local

diff_local:	${BASIC_TESTS:%=%.diff} \
		test_formatted.diff

test_informal:	${BASIC_TESTS:%=%.local} \
		test_formatted.local \
		test_get.local

diff_informal:	${BASIC_TESTS:%=%.diff} \
		test_formatted.diff \
		test_get.diff

test_forbidden:	$S/get_forbidden_files.local

diff_forbidden:	$S/get_forbidden_files.diff

%.email:	%.mail ./Contest_Address ./mail
	( echo To: `cat Contest_Address`; cat $*.mail) \
	    | ${SENDMAIL}
	sleep 2

%.local:	%.mail ./mail
	hpcm_sendmail <$*.mail
	sleep 2

extract_replies:	mbox
	rm -rf replies
	mkdir replies
	cd replies; hpcm_extract_test_replies <../mbox

# In the following, RCSfile and Revision are for
# get_count.  `e' is defined to avoid replacements by
# cvs/rcs in this text.
#
%.diff:		%.reply ./replies
	@echo $(<F); e=':.*\$$'; \
	 email_diff $< ./replies/$(<F) \
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
	 'a response from .*@'

COUNT_CORRECT_FILES = \
	count_correct.c \
	count_correct.cc \
	count_correct.java \
	count_correct.lsp

JAVAIO_CORRECT_FILES = \
	javaio_correct

COUNT_INCORRECT_FILES = \
	count_incorrect_output.c \
	count_incomplete_output.cc \
	count_formatting_error.java \
	count_crash.cc \
	count_timeout.java \
	count_timeout.c \
	count_too_much_output.c

FORMATTED_FILES = \
	multipart_formatted

UNFORMATTED_FILES = \
	unformatted \
	yahoo \
	quoted_printable \
	base64 \
	text_plain \
	bad_multipart_encoding \
	bad_multipart_type \
	multipart_7bit \
	multipart_base64 \
	multipart_pine \
	multipart_quoted_printable \
	multipart_simple_boundary

SYSTEM_ERROR_FILES = \
	system_error

GET_FILES = \
	get_count \
	get_bad_body \
	get_illegal_filenames \
	get_unreadable_files

QUERY_FILES = \
	query \
	bad_subject_format

PROBLEM_NAME_FILES = \
	bad_problem_name1 \
	bad_problem_name2 \
	bad_problem_name3 \
	bad_problem_name4 \
	bad_problem_name5 \
	bad_problem_name6 \
	bad_problem_name7 \
	bad_problem_name8 \
	bad_problem_name9

test_count_correct.local:	\
	${COUNT_CORRECT_FILES:%=$S/%.local}

test_count_correct.email:	\
	${COUNT_CORRECT_FILES:%=$S/%.email}

test_count_correct.diff:	\
	${COUNT_CORRECT_FILES:%=$S/replies/%.diff}

test_javaio_correct.local:	\
	${JAVAIO_CORRECT_FILES:%=$S/%.local}

test_javaio_correct.email:	\
	${JAVAIO_CORRECT_FILES:%=$S/%.email}

test_javaio_correct.diff:	\
	${JAVAIO_CORRECT_FILES:%=$S/replies/%.diff}

test_count_incorrect.local:	\
	${COUNT_INCORRECT_FILES:%=$S/%.local}

test_count_incorrect.email:	\
	${COUNT_INCORRECT_FILES:%=$S/%.email}

test_count_incorrect.diff:	\
	${COUNT_INCORRECT_FILES:%=$S/replies/%.diff}

test_formatted.local:	\
	${FORMATTED_FILES:%=$S/%.local}

test_formatted.email:	\
	${FORMATTED_FILES:%=$S/%.email}

test_formatted.diff:	\
	${FORMATTED_FILES:%=$S/replies/%.diff}

test_unformatted.local:	\
	${UNFORMATTED_FILES:%=$S/%.local}

test_unformatted.email:	\
	${UNFORMATTED_FILES:%=$S/%.email}

test_unformatted.diff:	\
	${UNFORMATTED_FILES:%=$S/replies/%.diff}

test_system_error.local:	\
	${SYSTEM_ERROR_FILES:%=$S/%.local}

test_system_error.email:	\
	${SYSTEM_ERROR_FILES:%=$S/%.email}

test_system_error.diff:	\
	${SYSTEM_ERROR_FILES:%=$S/replies/%.diff}

test_get.local:	\
	${GET_FILES:%=$S/%.local}

test_get.email:	\
	${GET_FILES:%=$S/%.email}

test_get.diff:	\
	${GET_FILES:%=$S/replies/%.diff}

test_query.local:	\
	${QUERY_FILES:%=$S/%.local}

test_query.email:	\
	${QUERY_FILES:%=$S/%.email}

test_query.diff:	\
	${QUERY_FILES:%=$S/replies/%.diff}

test_problem_name.local:	\
	${PROBLEM_NAME_FILES:%=$S/%.local}

test_problem_name.email:	\
	${PROBLEM_NAME_FILES:%=$S/%.email}

test_problem_name.diff:	\
	${PROBLEM_NAME_FILES:%=$S/replies/%.diff}
