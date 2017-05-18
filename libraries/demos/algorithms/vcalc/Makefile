# Makefile for the Simple Vector Calculator Problem
#
# File:		Makefile
# Date:		Thu May 18 05:22:17 EDT 2017
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
#   XTRA_CLEANS extra files to remove on `clean'
#   
P = vcalc

XTRA_INPUTS =
XTRA_LIBS =
XTRA_LIMITS =
XTRA_CLEANS =

C_FLAGS = -g
CC_FLAGS = -g
JAVA_FLAGS = -g
CL_FLAGS = --script
PY_FLAGS =

.SUFFIXES:
.SUFFIXES: .c .cc .java .lsp .py

default:	$P.out

.c:
	rm -f $* core core.[0-9]*
	gcc -Dinline=static -std=gnu11 ${C_FLAGS} \
	    -o $* $*.c -lm ${XTRA_LIBS}

.cc:
	rm -f $* core core.[0-9]*
	g++ -std=gnu++11 ${CC_FLAGS} \
	    -o $* $*.cc ${XTRA_LIBS}

#
.java:
	rm -f $* *.class core core.[0-9]*
	javac -encoding UTF-8 -sourcepath . -d . \
	      ${JAVA_FLAGS} $*.java
	echo >$* '#!/bin/sh'
	echo >>$* "exec `which java`" -ea $* '"$$@"'
	chmod a+r *.class
	chmod a+rx $*

.lsp:
	rm -f $* core core.[0-9]*
	echo >$* '#!/bin/sh'
	echo >>$* "exec sbcl ${CL_FLAGS}" \
	          "$*.lsp" '"$$@"'
	chmod a+r $*.lsp
	chmod a+rx $*

.py:
	rm -f $* *.pyc core core.[0-9]*
	python2 -m py_compile ${PY_FLAGS} $*.py
	echo >$* '#!/bin/sh'
	echo >>$* "exec python2" "$*.pyc" '"$$@"'
	chmod a+r $*.pyc
	chmod a+rx $*

$P.in:
	if test -r sample.in; \
	then cp sample.in $P.in; chmod u+rw $P.in; fi

$P.test:
	if test -r sample.test; \
	then cp sample.test $P.test; \
	     chmod u+rw $P.test; fi

#
$P.out:	$P $P.in
	rm -f $P.out core core.[0-9]*
	grant_access . $P ${XTRA_INPUTS}
	hpcm_sandbox -cputime 30 \
	             -datasize 64m \
		     -stacksize 16m \
		     -filesize 64m \
		     ${XTRA_LIMITS} \
		     -tee $P.out \
		     $P \
	    <$P.in

test:	$P.out $P.test
	@echo ""
	@echo "DIFFERENCES: $P.test ---> $P.out"
	@if diff -u $P.test $P.out; \
	    then echo "NO DIFFERENCES FOUND"; fi

$P.debug:	$P $P.in
	rm -f $P.debug core core.[0-9]*
	grant_access . $P ${XTRA_INPUTS}
	hpcm_sandbox -cputime 30 \
	             -datasize 64m \
		     -stacksize 16m \
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

#
solution-submit:	$P.out
	hpcm_submit -solution $P

clean:
	rm -f $P *.class *.pyc core core.[0-9]* \
	      *.out *.debug *.fout *.jout *.jfout \
	      make_$P_*input ${XTRA_CLEANS}
	if cmp -s $P.in sample.in; then rm $P.in; fi
	if cmp -s $P.test sample.test; \
	   then rm $P.test; fi

# Derived From:	Makefile1
# Author:	walton@seas.harvard.edu
#
# The authors have placed this file in the public
# domain; they make no warranty and accept no liability
# for this file.
