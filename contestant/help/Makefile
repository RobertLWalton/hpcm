# HPCM Contestant Help Directory Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Mon Jul 24 16:47:08 EDT 2017
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.

# To make web help pages, execute `make web', copy
# the `web.tar' file to your web server and unpack
# it to make a directory containing the `index.html'
# file, .pdf files, and files listed in indices.
# web.tar contains the contents of the `web'
# subdirectory.

all:	modes STL_doc

pdf:	acm_contest_help.pdf \
	algorithm_festival_help.pdf \
	timed_practice_help.pdf \
	untimed_practice_help.pdf \
	email_practice_help.pdf \
	c_help.pdf \
	cc_help.pdf \
	java_help.pdf \
	python_help.pdf \
	lsp_help.pdf \
	c_basic_io_demos.pdf \
	c_advanced_io_demos.pdf \
	c_algorithm_demos.pdf \
	cc_basic_io_demos.pdf \
	cc_advanced_io_demos.pdf \
	cc_algorithm_demos.pdf \
	java_basic_io_demos.pdf \
	java_advanced_io_demos.pdf \
	java_algorithm_demos.pdf \
	python_basic_io_demos.pdf \
	lsp_basic_io_demos.pdf \
	lsp_advanced_io_demos.pdf \
	lsp_algorithm_demos.pdf

# Kill all implicit rules and make new rule:
#
.SUFFIXES:
.SUFFIXES: .ps .pdf
.ps.pdf:
	ps2pdf $*.ps $*.pdf

WEB = ""

# If we replace %_help_index by %help_index we get
# circularity with ./help_index.
#
%_help_index:	help_index
	rm -f $*_help_index \
	      $*_help_index.html \
	      $*_help_index.files
	../../judge/bin/makeindex \
	    $*_help_index \
	    help_index ${WEB} ${OPTIONS}

%_help.ps:	%_help_index
	rm -rf $*_help.ps
	../../judge/bin/fprint > $*_help.ps -o \
	    `cat $*_help_index.files`

acm_contest_help_index: \
    OPTIONS = acm_contest_help_index \
              contest/acm_contest \
	      accounts general practice
algorithm_festival_help_index: \
    OPTIONS = algorithm_festival_help_index \
              contest/algorithm_festival accounts general
timed_practice_help_index: \
    OPTIONS = timed_practice_help_index \
              contest/timed_practice accounts general
untimed_practice_help_index: \
    OPTIONS = untimed_practice_help_index \
              contest/untimed_practice accounts general
email_practice_help_index: \
    OPTIONS = email_practice_help_index \
              contest/email_practice general

c_help_index: \
    OPTIONS = c_help_index c
cc_help_index: \
    OPTIONS = cc_help_index c++
java_help_index: \
    OPTIONS = java_help_index java
python_help_index: \
    OPTIONS = python_help_index python
lsp_help_index: \
    OPTIONS = lsp_help_index commonlisp

all_help_index: \
    OPTIONS = all_help_index \
              contest language accounts general practice


# If we replace %_demos_index by %demos_index we get
# circularity with ../../libraries/demos/demos.index.
#
%_demos_index:	../../libraries/demos/demos.index
	rm -f $*_demos_index \
	      $*_demos_index.html \
	      $*_demos_index.files
	../../judge/bin/makeindex \
	    $*_demos_index \
	    ../../libraries/demos/demos.index ${WEB} \
	    $*_help

%_demos.ps:	%_demos_index
	rm -rf $*_demos.ps
	../../judge/bin/fprint > $*_demos.ps -o \
	    `egrep -v '.*\.(in|test).*' \
	           $*_demos_index.files`


# Legacy email stuff

emailsh:	email/sh/hpcm_extract \
		email/sh/hpcm_submit \
		email/sh/hpcm_clisp \
		email/sh/hpcm_clisp.lsp

email/sh/hpcm_extract:
	cd email/sh; \
	    ln -s ../../../bin/hpcm_extract .

email/sh/hpcm_submit:
	cd email/sh; \
	    ln -s ../../../bin/hpcm_submit .

email/sh/hpcm_clisp:
	cd email/sh; \
	    ln -s ../../../bin/hpcm_clisp .

email/sh/hpcm_clisp.lsp:
	cd email/sh; \
	    ln -s ../../../bin/hpcm_clisp.lsp .

# Make STL HTML documentation directory.
#
STL_doc:	private/STL_doc.tar.gz
	rm -rf STL_doc
	tar zxpf private/STL_doc.tar.gz
	touch STL_doc
	find STL_doc -type d -exec chmod a+rx {} \;
	find STL_doc -type f -exec chmod a+r {} \;

# Web_do merely causes `make web' to remake web.tar
# even if it exists.
#
web_do:

web:	web_do
	rm -rf web web.tar
	make clean
	mkdir web
	make WEB=web all_help_index all_demos_index
	make pdf
	cp -p ../../wiki/print/*.pdf web/algorithms
	chmod 444 web/algorithms/*.pdf
	cp index.html web/index.html
	chmod 444 web/index.html
	cp *.pdf web/
	chmod 444 web/*.pdf
	find web -type d -exec chmod 755 {} \;
	cd web; tar cf ../web.tar *

modes:
	chmod a+x . .. ../.. ../../..

clean:
	rm -rf email/sh/hpcm_extract \
	       email/sh/hpcm_submit \
	       email/sh/hpcm_clisp \
	       email/sh/hpcm_clisp.lsp \
	       web web.tar \
	       *_help_index *_demos_index \
	       *.files *.ps *.pdf \
	       *_index.html \
	       STL_doc
