# Makefile for the `count' Demonstration Problem
#
# File:		Makefile
# Date:		Tue Apr 11 07:56:42 EDT 2006
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
#			`count' by running gcc on
#			count.c, or g++ on count.cc,
#			or javac on count.java,
#			or hpcm_clisp on count.lsp,
#			depending upon which of count.c,
#			count.cc, count.java, or
#			count.lsp exist.  Also makes a
#			shell script named `count' for
#			count.java and count.lsp files.
#			Does nothing if `count' is more
#			up to date than count.c,
#			count.cc, count.java, or
#			count.lsp.
#
#	make count.out	Makes `count' as above and
#			then runs it with no arguments
#			and with the standard input
#			coming from the file count.in.
#			Puts the standard output in the
#			file count.out, and then copies
#			that to the screen.  Does noth-
#			ing, however, if count.out is
#			more recent than both count.in
#			and count.
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

#
.SUFFIXES:
.SUFFIXES: .c .cc .java .lsp

default:	count.out

.c:
	rm -f $* core
	gcc -g -o $* $*.c -lm

.cc:
	rm -f $* core
	g++ -g -o $* $*.cc -lm

.java:
	rm -f $* *.class core
	javac -g $*.java
	echo >$* '#!/bin/sh'
	echo >>$* "exec `which java` $*"
	chmod a+r *.class
	chmod a+rx $*

.lsp:
	rm -f $* $*.fas $*.lib core
	hpcm_clisp -c $*.lsp
	echo >$* '#!/bin/sh'
	echo >>$* "exec `hpcm_clisp -which` -I $*.fas"
	chmod a+r $*.fas
	chmod a+rx $*
#
# hpcm_sandbox below may execute `count' as a special
# unprivileged user named `sandbox', so various files
# must be `a+x' or `a+r'.  `hpcm_clisp -which' returns
# in the judging account the name of a version of the
# hpcm_clisp program that can be run in the sandbox.

count.out:	count count.in
	rm -f count.out core
	chmod a+x . count
	hpcm_sandbox -cputime 60 \
	             -datasize 4m \
		     -stacksize 4m \
		     -filesize 50k \
		     -watch \
		     count \
	    <count.in >count.out
	@echo ""
	@echo "========== OUTPUT (count.out)" \
	      "=========="
	@cat count.out
	@echo "============== END OUTPUT =============="

count.debug:	count count.in
	rm -f count.debug core
	chmod a+x . count
	hpcm_sandbox -cputime 60 \
	             -datasize 4m \
		     -stacksize 4m \
		     -filesize 50k \
		     -watch \
		     count debug \
	    <count.in >count.debug
	@echo ""
	@echo "====== DEBUG OUTPUT (count.debug)" \
	      "======"
	@cat count.debug
	@echo "=========== END DEBUG OUTPUT ==========="

#
debug:	count.debug

submit:		count.out
	hpcm_submit count

in-submit:	count.out
	hpcm_submit -in count

inout-submit:	count.out
	hpcm_submit -inout count

solution-submit:	count.out
	hpcm_submit -solution count

clean:
	rm -f count *.class core count.out count.debug \
	      count.fas count.lib count.jout


# Author:	walton@deas.harvard.edu
#
# The authors have placed this file in the public
# domain; they make no warranty and accept no liability
# for this file.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2006/04/11 12:07:31 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.27 $
