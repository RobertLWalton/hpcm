# Makefile for Demonstration Problem
#
# File:		Makefile
# Author:	walton@deas.harvard.edu
# Version:	3
#
# Change: Java has been added.
#
# Exactly ONE of the four files count.c (C),
# count.cc (C++), count.p (Pascal), or count.java (Java)
# should exist.
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

default:	count.out

.c:
	gcc -g -o $* $*.c -lm

.cc:
	g++ -g -o $* $*.cc -lm

.p:
	pc -g -o $* $*.p -lm

.java:
	javac -g $*.java
	echo >$* java $*
	chmod u+x $*

count.out:	count count.in
	rm -f count.out count.err
	-csh -c 'limit cputime 30; \
		 limit datasize 2m; \
		 limit filesize 50k; count' \
			<count.in >count.out 2>count.err
	: ========; cat count.err count.out; : =========

#

submit:		count.out
	( if test -r count.c; \
		then program=count.c; \
	  elif test -r count.cc; \
		then program=count.cc; \
	  elif test -r count.p; \
		then program=count.p; \
	  elif test -r count.java; \
		then program=count.java; \
	  fi; \
	  echo '[[..ACM..BEGIN..]]' $$program; \
	  cat $$program | sed -e '/^/s//../'; \
	  echo '[[..ACM..END..]]' ) \
	    | mailx -s '[[..ACM..SUBMIT..]]' \
		acm-cont $$USER

clean:
	rm -f count count.out count.class
