# Makefile for Demonstration Problem
#
# File:		Makefile
# Author:	walton@deas.harvard.edu
# Date:		Sun Sep  3 05:13:39 EDT 2000
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
#			pc on count.p, or javac on
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
.SUFFIXES: .c .cc .p .java

default:	count.view

.c:
	rm -f $* $*.cerr $*.out $*.err
	-gcc -g -o $* $*.c -lm 2>$*.cerr

.cc:
	rm -f $* $*.cerr $*.out $*.err
	-g++ -g -o $* $*.cc -lm 2>$*.cerr

.p:
	rm -f $* $*.cerr $*.out $*.err
	-pc -g -o $* $*.p -lm 2>$*.cerr

.java:
	rm -f $* $*.class $*.cerr $*.out $*.err
	-javac -g $*.java 2>$*.cerr
	-echo >$* java $* 2>>$*.cerr
	-chmod u+x $* 2>>$*.cerr

count.out:	count count.in
	rm -f count.out count.err core
	-chmod a+x count .
	hpcm_sandbox -cputime 60 \
	             -datasize 1m \
		     -stacksize 1m \
		     -filesize 50k \
		     -watch \
		     count \
	    <count.in >count.out 2>count.err

count.view:	count.out 
	hpcm_view count

submit:		count.out
	hpcm_submit count

clean:
	rm -f count count.class core \
	      count.out count.err count.cerr
