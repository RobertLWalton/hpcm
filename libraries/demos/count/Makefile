# Makefile for Demonstration Problem
#
# File:		Makefile
# Author:	walton@deas.harvard.edu
# Date:		Sun Sep  3 17:25:01 EDT 2000
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
.SUFFIXES: .p .c .cc .java
#
# Put .p before .c so it takes precedence.

default:	count.out

.c:
	rm -f $* $*.out core
	gcc -g -o $* $*.c -lm

.cc:
	rm -f $* $*.out core
	g++ -g -o $* $*.cc -lm

.p:
	rm -f $* $*.out core
	p2c -check $*.p
	echo >>$*.c 'int main () { main_(); }'
	gcc -g -o $* $*.c -lp2c -lm

.java:
	rm -f $* $*.class $*.out core
	javac -g $*.java
	echo >$* java $*
	chmod u+x $*

count.out:	count count.in
	rm -f count.out core
	chmod a+x count .
	hpcm_sandbox -cputime 60 \
	             -datasize 1m \
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
