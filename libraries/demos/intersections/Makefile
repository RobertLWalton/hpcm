# Makefile for the Line Segment Intersections Problem
#
# File:		Makefile
# Date:		Sun Sep 12 02:02:56 EDT 2010
#
# See demonstration Makefile for documentation.
#
#   P		problem name
#
#   XTRA_INPUTS	extra input files that must be granted
#		access
#   XTRA_LIBS	extra libraries for C and C++ programs
#   XTRA_LIMITS limits that override those given
#               previously to hpcm_sandbox
#   
P = intersections
XTRA_INPUTS =
XTRA_LIBS =
XTRA_LIMITS =

.SUFFIXES:
.SUFFIXES: .c .cc .java .lsp

default:	$P.out

.c:
	rm -f $* core core.[0-9]*
	gcc -g -o $* $*.c -lm ${XTRA_LIBS}

.cc:
	rm -f $* core core.[0-9]*
	g++ -g -o $* $*.cc -lm ${XTRA_LIBS}

.java:
	rm -f $* *.class core core.[0-9]*
	javac -g $*.java
	echo >$* '#!/bin/sh'
	echo >>$* "exec `which java` $* \$$*"
	chmod a+r *.class
	chmod a+rx $*

#
.lsp:
	rm -f $* $*.fas $*.lib core core.[0-9]*
	hpcm_clisp -c $*.lsp
	echo >$* '#!/bin/sh'
	echo >>$* \
	    "exec `hpcm_clisp -which` -I $*.fas \$$*"
	chmod a+r $*.fas
	chmod a+rx $*

$P.out:	$P $P.in
	rm -f $P.out core core.[0-9]*
	grant_access . $P ${XTRA_INPUTS}
	hpcm_sandbox -cputime 60 \
	             -datasize 4m \
		     -stacksize 4m \
		     -filesize 4m \
		     ${XTRA_LIMITS} \
		     -tee $P.out \
		     $P \
	    <$P.in

$P.debug:	$P $P.in
	rm -f $P.debug core core.[0-9]*
	grant_access . $P ${XTRA_INPUTS}
	hpcm_sandbox -cputime 60 \
	             -datasize 4m \
		     -stacksize 4m \
		     ${XTRA_LIMITS} \
		     -tee $P.debug \
		     $P debug \
	    <$P.in

debug:		$P.debug

submit:		$P.out
	hpcm_submit $P

in-submit:	$P.out
	hpcm_submit -in $P

inout-submit:	$P.out
	hpcm_submit -inout $P

solution-submit:	$P.out
	hpcm_submit -solution $P

clean:
	rm -f $P *.class core core.[0-9]* \
	      *.out *.debug *.fout *.jout *.jfout \
	      $P.fas $P.lib make_$P_*input

#
# Derived From:	Makefile1
# Author:	walton@seas.harvard.edu
#
# The authors have placed this file in the public
# domain; they make no warranty and accept no liability
# for this file.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2010/09/14 00:21:36 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.2 $
