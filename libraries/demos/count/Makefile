# Makefile for the `count' Demonstration Problem
#
# File:		Makefile
# Date:		Mon Sep  4 04:40:06 EDT 2000
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
#			or count.p to the judges.
#
#	make clean	Removes `count' and count.out.
#
.SUFFIXES:
.SUFFIXES: .p .c .cc .java
#
# Put .p before .c so .p takes precedence.

default:	count.out

.c:
	rm -f $* $*.out core
	gcc -g -o $* $*.c -lm

.cc:
	rm -f $* $*.out core
	g++ -g -o $* $*.cc -lm

.p:
	rm -f $* $*.c $*.out core
	p2c $*.p
	gcc -g -o $* $*.c -lp2c -lm
	rm -f $*.c

.java:
	rm -f $* $*.class $*.out core
	javac -g $*.java
	echo >$* '#!/bin/sh'
	echo >>$* "exec `which java` $*"
	chmod a+r $*.class
	chmod a+rx $*
#
count.out:	count count.in
	rm -f count.out core
	chmod a+x count .
	hpcm_sandbox -cputime 60 \
	             -datasize 2m \
		     -stacksize 1m \
		     -filesize 50k \
		     -watch \
		     count \
	    <count.in >count.out
	@echo ""
	@echo -n "========== OUTPUT (count.out)"
	@echo " =========="
	@cat count.out

submit:		count.out
	hpcm_submit count

clean:
	rm -f count count.class core count.out
#
# Author:	walton@deas.harvard.edu
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: acm-cont $
#   $Date: 2000/09/04 09:18:00 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.11 $
