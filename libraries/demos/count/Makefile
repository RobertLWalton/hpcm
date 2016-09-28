# Makefile for the `count' Demonstration Problem
#
# File:		Makefile
# Date:		Wed Sep 28 04:21:26 EDT 2016
#
# Exactly ONE of the four files count.c (C), count.cc
# (C++), count.java (Java), or count.lsp (Commonlisp)
# should exist.
#
# UNIX commands supported by this Makefile:
#
#	make		Same as `make count.out'.
#
#	make count	Makes the binary program file
#			or shell script `count' by a
#			means depending on which of
#			count.c, count.cc, count.java,
#			count.py, or count.lsp exist.
#			E.g., compiled count.c using
#			gcc, or writes a shell script
#			envoking python on count.py.
#			Does nothing if `count' is more
#			up to date than count.c,
#			count.cc, count.java, or
#			count.lsp.
#
#	make count.in	If count.in does not exist,
#			makes it by copying sample.in.
#			Does nothing if count.in already
#			exists.
#
#	make count.test	If count.test does not exist,
#			makes it by copying sample.test.
#			Does nothing if count.test
#			already exists.
#
#	make count.out	Makes `count' and `count.in' as
#			above and then runs `count' with
#			no arguments and with the stand-
#			ard input coming from count.in.
#			Puts the standard output in the
#			file count.out, and then copies
#			that to the screen.  Does noth-
#			ing, however, if count.out is
#			more recent than both count.in
#			and count.
#
#	make test	Makes count.out and count.test
#			as above and prints a difference
#			listing of these two files.
#
#	make count.debug  Ditto but runs `count debug'
#			(with the one argument `debug')
#			instead of `count' and puts the
#			output in count.debug instead
#			of count.out.
#
#	make debug	Same as `make count.debug'.
#			
#	make submit	Makes `count.out' just to be
#			sure that nothing crashes, and
#			then e-mails count.c, count.cc,
#			count.java, or count.lsp to the
#			judges.
#
#	make in-submit	Ditto, but requests that if
#			the score is `Incorrect Output'
#			or `Formatting Error', the
#			judge's input for the failed
#			test case will be returned in
#			e-mail to the contestant.
#
#	make inout-submit
#			Ditto but requests both the
#			judge's input and the judge's
#			output for the test case.
#
#	make solution-submit
#			Like `make submit' but requests
#			that if the score is `Completely
#			Correct' the judge's solution
#			will be returned in e-mail to
#			the contestant.
#	
#	make clean	Removes `count', count.out, and
#			other intermediate files that
#			might exist, such as `core',
#			`count.class', or count.fas.

.SUFFIXES:
.SUFFIXES: .c .cc .java .lsp .py

default:	count.out

.c:
	rm -f $* core core.[0-9]*
	gcc -g -o $* $*.c -lm -lcrypto

.cc:
	rm -f $* core core.[0-9]*
	g++ -g -o $* $*.cc -lm -lcrypto

.java:
	rm -f $* *.class core core.[0-9]*
	javac -g $*.java
	echo >$* '#!/bin/sh'
	echo >>$* "exec `which java` -ea $* \$$*"
	chmod a+r *.class
	chmod a+rx $*

#
.lsp:
	rm -f $* core core.[0-9]*
	echo >$* '#!/bin/sh'
	echo >>$* "exec sbcl --script $*.lsp" '"$$@"'
	chmod a+r $*.lsp
	chmod a+rx $*

.py:
	rm -f $* core core.[0-9]*
	echo >$* '#!/bin/sh'
	echo >>$* "exec python $*.py" '"$$@"'
	chmod a+r $*.py
	chmod a+rx $*

# If count.in does not exist but sample.in does, then
# copy sample.in to count.in and make the copy writable
# by the user.  Ditto for count.test and sample.test.
#
count.in:
	if test -r sample.in; \
	then cp sample.in count.in; \
	chmod u+rw count.in; fi

count.test:
	if test -r sample.test; \
	then cp sample.test count.test; \
	chmod u+rw count.test; fi

#
# hpcm_sandbox below may execute `count' as a special
# unprivileged user named `sandbox', so various files
# must be `a+x' or `a+r'.

count.out:	count count.in
	rm -f count.out core core.[0-9]*
	chmod a+x . count
	hpcm_sandbox -cputime 30 \
	             -datasize 64m \
		     -stacksize 16m \
		     -filesize 64m \
		     -tee count.out \
		     count \
	    <count.in

test:	count.out count.test
	@echo ""
	@echo "DIFFERENCES: count.test ---> count.out"
	@if diff -u count.test count.out; \
	    then echo "NO DIFFERENCES FOUND"; fi

count.debug:	count count.in
	rm -f count.debug core core.[0-9]*
	chmod a+x . count
	hpcm_sandbox -cputime 30 \
	             -datasize 64m \
		     -stacksize 16m \
		     -tee count.debug \
		     count debug \
	    <count.in 

debug:	count.debug

#
submit:		count.out
	hpcm_submit count

in-submit:	count.out
	hpcm_submit -in count

inout-submit:	count.out
	hpcm_submit -inout count

solution-submit:	count.out
	hpcm_submit -solution count

clean:
	rm -f count *.class core core.[0-9]* \
	      *.out *.debug *.fout *.jout *.jfout \
	      make_count_*input
	if cmp -s count.in sample.in; \
	   then rm count.in; fi
	if cmp -s count.test sample.test; \
	   then rm count.test; fi


# Author:	walton@deas.harvard.edu
#
# The authors have placed this file in the public
# domain; they make no warranty and accept no liability
# for this file.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2011/09/21 23:36:12 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.32 $
