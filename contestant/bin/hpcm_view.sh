#!/bin/sh -f
#
# View output in a problem directory.
#
# File:		hpcm_view
# Author:	Bob Walton <walton@deas.harvard.edu>
# Date:		Fri Sep  1 08:38:46 EDT 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: acm-cont $
#   $Date: 2000/09/03 10:17:09 $
#   $RCSfile: hpcm_view.sh,v $
#   $Revision: 1.2 $

case "$1" in
    "" | -*)
    		echo hi
		exit 1
		;;
esac

program=
extras=""
for e in .c .cc .java .p; do
    if test -r $1$e; then
        if test "$program" = ""; then
	    program=$1$e
	else
	    extras="$extras $1$e"
	fi
    fi
done

if test "$program" = ""; then
    echo "ERROR: no files to submit ($1.*)"
elif test "$extras" != ""; then
    echo "ERROR: too many files to submit:"
    echo "       $program$extras"
fi

if test -s $1.cerr; then
    echo ""
    echo "======== COMPILE ERRORS ($1.cerr) ========="
    cat $1.cerr
    exit 0
elif test ! -f $1; then
    echo -n "ERROR: the program binary ($1)"
    echo " does not exist"
    exit 0
elif test ! -x $1; then
    echo -n "ERROR: the program binary ($1)"
    echo " is not executable"
    exit 0
fi

if test -s $1.err; then
    echo ""
    echo "=========== RUN ERRORS ($1.err) ==========="
    cat $1.err
fi

if test -s $1.out; then
    echo ""
    echo "============= OUTPUT ($1.out) ============="
    cat $1.out
elif test -r $1; then
    echo ""
    echo "ERROR: the output file ($1.out) is empty"
elif test -f $1; then
    echo ""
    echo "ERROR: the output file ($1.out) is unreadable"
else
    echo ""
    echo -n "ERROR: the output file ($1.out)"
    echo " does not exist"
fi

if test -r core; then
    echo ""
    echo "ERROR: a core dump file (core) exists"
fi

exit 0
