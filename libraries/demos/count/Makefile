# Makefile for the `count' Demonstration Problem
#
# File:		Makefile
# Date:		Mon Jan 28 03:52:38 EST 2002
#
# Exactly ONE of the four files count.c (C), count.cc
# (C++), count.p (Pascal), or count.java (Java) should
# exist.
#
# UNIX commands supported by this Makefile:
#
#	make		Same as `make count.out'.
#
#	make count	Makes the binary program file
#			`count' by running gcc on
#			count.c, or g++ on count.cc, or
#			p2c on count.p, or javac on
#			count.java, depending upon which
#			of count.c, count.cc, count.p,
#			or count.java exist.  Does
#			nothing if `count' is more up
#			to date than count.c, count.cc,
#			count.p, or count.java.
#
#	make count.out	Makes `count' as above and
#			then runs it with standard
#			input coming from the file
#			count.in.  Puts the standard
#			output in the file count.out,
#			and then copies that to the
#			screen.  Does nothing, however,
#			if count.out is more recent
#			than both count.in and count.
#			
#	make submit	Makes `count.out' just to be
#			sure that nothing crashes, and
#			then e-mails count.c, count.cc,
#			count.java, or count.p to the
#			judges.
#
#	make clean	Removes `count', count.out, and
#			other intermediate files that
#			might exist, such as `core',
#			`count.class', or `count.p2c'.


.SUFFIXES:
.SUFFIXES: .c .cc .java .p

default:	count.out

.c:
	rm -f $* core
	gcc -g -o $* $*.c -lm

.cc:
	rm -f $* core
	g++ -g -o $* $*.cc -lm

.p:
	rm -f $* $*.p2c core
	p2c -o $*.p2c $*.p
	gcc -g -o $* -x c $*.p2c -lp2c -lm

# Do NOT try to use -check option for p2c as it fouls up
# eof detection.

.java:
	rm -f $* $*.class core
	javac -g $*.java
	echo >$* '#!/bin/sh'
	echo >>$* "exec `which java` $*"
	chmod a+r *.class
	chmod a+rx $*
#
# hpcm_sandbox below may execute `count' as a special
# unprivileged user named `sandbox', so various files
# must be `a+x' or `a+r'.

count.out:	count count.in
	rm -f count.out core
	chmod a+x . count
	hpcm_sandbox -cputime 60 \
	             -datasize 8m \
		     -stacksize 2m \
		     -filesize 50k \
		     -watch \
		     count \
	    <count.in >count.out
	@echo ""
	@echo "========== OUTPUT (count.out)" \
	      "=========="
	@cat count.out
	@echo "============== END OUTPUT =============="

submit:		count.out
	hpcm_submit count

clean:
	rm -f count count.class count.p2c core count.out
#
# Author:	walton@deas.harvard.edu
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2002/01/28 08:52:57 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.17 $
