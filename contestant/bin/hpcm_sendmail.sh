#!/bin/sh -f
#
# Send email to the judge.
#
# File:		hpcm_sendmail
# Author:	Bob Walton <walton@deas.harvard.edu>
# Date:		Sun Sep 17 21:50:09 EDT 2000
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: hc3 $
#   $Date: 2000/09/18 02:24:18 $
#   $RCSfile: hpcm_sendmail.sh,v $
#   $Revision: 1.3 $

case "$1" in
     "")
    		;;
     *)
    		echo "
cat email_file | hpcm_sendmail

	Sends the email in the email_file to the judge.
	Similar to sendmail -oi -t but adds its own
	To: field, which should not be included in the
	email_file."

		exit 1
		;;
esac

# Locate the To address.
if test ! -r ~/.hpcm_contest/hpcm_sendmail.rc
then
	echo cannot read \
	     ~/.hpcm_contest/hpcm_sendmail.rc
	exit 1
fi
to=`grep '^To:' ~/.hpcm_contest/hpcm_sendmail.rc`

# Submit program file by email.
#
( echo $to; echo Cc: `id -un`; cat - ) \
    | /usr/sbin/sendmail -oi -t

exit 0
