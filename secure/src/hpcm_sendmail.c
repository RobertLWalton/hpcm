/* Programming Contest Sendmail Program 
 *
 * File:	hpcm_sendmail.c
 * Authors:	Bob Walton (walton@deas.harvard.edu)
 * Date:	Sat Oct  7 06:02:06 EDT 2000
 *
 * The authors have placed this program in the public
 * domain; they make no warranty and accept no liability
 * for this program.
 *
 * RCS Info (may not be true date or author):
 *
 *   $Author: hc3 $
 *   $Date: 2000/10/07 10:02:50 $
 *   $RCSfile: hpcm_sendmail.c,v $
 *   $Revision: 1.7 $
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/signal.h>
#include <pwd.h>
#include <sys/utsname.h>
#include <time.h>

#ifndef MD5SUM_LENGTH
#   define MD5SUM_LENGTH 32
#endif

#ifndef MD5SUM
#   define MD5SUM "/usr/bin/md5sum"
#endif

#ifndef MD5SUM_ENV
#   define MD5SUM_ENV "HPCM_MD5SUM"
#endif

#ifndef SENDMAIL
#   define SENDMAIL "/usr/sbin/sendmail", "-oi", "-t"
#endif

#ifndef SENDMAIL_ENV
#   define SENDMAIL_ENV "HPCM_SENDMAIL"
#endif

char documentation [] =
"cat your_mail_file | hpcm_sendmail\n"
"\n"
"    This program performs the same functions as:\n"
"\n"
"            /usr/sbin/sendmail -oi -t\n"
"\n"
"    except that this program supplies the To: field\n"
"    value and authentication field values that will\n"
"    send the mail to the appropriate HPCM judge and\n"
"    will ensure that replies are sent to the sender.\n"
"\n"
"    This program is normally setuid to a judging\n"
"    account so it can read certain authetication\n"
"    information files that are not available to the\n"
"    sender.\n"
"\n"
"    The program will write an error message on the\n"
"    standard error output if any system call is in\n"
"    error.\n" ;

char * md5sum_argv[] = { MD5SUM, NULL };
char * md5sum_env[]  = { MD5SUM_ENV, NULL };
char * sendmail_argv[] = { SENDMAIL, NULL };
char * sendmail_env[]  = { SENDMAIL_ENV, NULL };

void errno_exit ( char * m )
{
    fprintf ( stderr, "hpcm_sendmail: system call error:"
                      " %s: %s\n",
		      m, strerror ( errno ) );
    exit ( errno );
}

void too_big_exit ( char * m )
{
    fprintf ( stderr, "hpcm_sendmail: too big: %s\n",
                      m );
    exit ( 1 );
}

/* Start a child running the program with arguments
   given by argv, where the first argument is the
   name of the program.  The environment is changed
   to env.  Return the pid_t of the child that runs
   this program.  Also return in the files argument
   three FILE's:

   	files[0]	write stdin input to child
   	files[1]	read stdout output from child
   	files[2]	read stderr output from child
*/
pid_t exec_program
    ( FILE * files[3],
      char * const argv[],
      char * const env[] )
{
    int in_pipe[2];
    int out_pipe[2];
    int err_pipe[2];
    pid_t child;

    if ( pipe ( in_pipe ) < 0
         ||
	 pipe ( out_pipe ) < 0
	 ||
	 pipe ( err_pipe ) < 0 )
    	errno_exit ( "pipe" );

    child = fork ();

    if ( child < 0 ) errno_exit ( "fork" );

    if ( child ) {

    	/* Executed by parent. */

	files[0] = fdopen ( in_pipe[1], "w" );

	if ( files[0] == NULL )
	    errno_exit ( "in pipe parent fdopen" );

	if ( close ( in_pipe[0] ) < 0 )
	    errno_exit ( "in pipe parent close" );

	files[1] = fdopen ( out_pipe[0], "r" );

	if ( files[1] == NULL )
	    errno_exit ( "out pipe parent fdopen" );

	if ( close ( out_pipe[1] ) < 0 )
	    errno_exit ( "out pipe parent close" );

	files[2] = fdopen ( err_pipe[0], "r" );

	if ( files[2] == NULL )
	    errno_exit ( "err pipe parent fdopen" );

	if ( close ( err_pipe[1] ) < 0 )
	    errno_exit ( "err pipe parent close" );

	return child;

    } else {

    	/* Executed by child. */

	if ( close ( in_pipe[1] ) < 0 )
	    errno_exit ( "in pipe child close" );

	if ( close ( out_pipe[0] ) < 0 )
	    errno_exit ( "out pipe child close" );

	if ( close ( err_pipe[0] ) < 0 )
	    errno_exit ( "err pipe child close" );

	while ( in_pipe[0] <= 2 )
	    in_pipe[0] = dup ( in_pipe[0] );

	while ( out_pipe[1] <= 2 )
	    out_pipe[1] = dup ( out_pipe[1] );

	while ( err_pipe[1] <= 2 )
	    err_pipe[1] = dup ( err_pipe[1] );

	if ( dup2 ( in_pipe[0], 0 ) < 0 )
	    errno_exit ( "in dup2" );

	if ( dup2 ( out_pipe[1], 1 ) < 0 )
	    errno_exit ( "out dup2" );

	if ( dup2 ( err_pipe[1], 2 ) < 0 )
	    errno_exit ( "err dup2" );

	{
	    int fd;
	    int max_fd = getdtablesize ();

	    if ( max_fd < 0 ) max_fd = 256;

	    for ( fd = 3; fd < max_fd; ++ fd )
	    	close ( fd );
	}

	if ( execve ( argv[0], argv, env ) < 0 )
	{
	    fprintf ( stderr, "Could not execute %s\n",
	    		      argv[0] );
	    errno_exit ( "execev" );
	}
    }
}

/* Copy all characters read from a file to the standard
   output.  Add an `\n' if characters were copied and
   the last was not a `\n'.  Return 1 if there were
   characters copied and 0 if not.
*/
int flush_output ( FILE * file )
{
    int wrote_char = 0;
    int last_char;

    while ( 1 )
    {
    	int c = fgetc ( file );
	if ( c == EOF ) break;
	fputc ( c, stdout );
	wrote_char = 1;
	last_char = c;
    }

    if ( wrote_char && last_char != '\n' )
    {
        fputc ( '\n', stdout );
    }

    return wrote_char;
}

/* Check the completing status of a program started by
   exec_program.  Close the input to this program unless
   the nocloseinput option is true.  Copy any output
   produced by this program to the standard output, and
   close the output file. Ditto for standard error.
   Wait for the program to terminate.  Exit with error
   message if the program terminates with non-zero
   status or the standard error was not empty.
*/
void check_program
    ( FILE * files[3],
      pid_t child,
      char * const argv[],
      int nocloseinput )
{
    int child_exit_status;
    int stderr_nonempty;

    if ( ! nocloseinput && fclose ( files[0] ) == EOF )
    	errno_exit ( "closing input to subprogram" );

    flush_output ( files[1] );
    stderr_nonempty = flush_output ( files[2] );

    if ( waitpid ( child, & child_exit_status, 0 ) < 0 )
	errno_exit ( "wait" );

    if ( WIFSIGNALED ( child_exit_status ) )
    {
	int sig = WTERMSIG ( child_exit_status );

	fprintf ( stderr,
		  "hpcm_sendmail: %s"
		  " terminated with signal:"
		  " %s\n",
		  argv[0],
		  strsignal ( sig ) );

	/* Parent exit when child died by
	   signal.
	*/
	exit ( 128 + sig );
    }
    else if ( WIFEXITED ( child_exit_status ) )
    {
    	int code = WEXITSTATUS ( child_exit_status );

	if ( code != 0 ) {

	    fprintf ( stderr,
		      "hpcm_sendmail: %s"
		      " terminated with error code:"
		      " %s\n",
		      argv[0],
		      strerror ( code ) );

	    /* Parent exit when child returned non-zero
	       status.
	    */
	    exit ( code );
	} else if ( stderr_nonempty ) {
	    fprintf ( stderr,
		      "hpcm_sendmail: %s"
		      " terminated with error output\n",
		      argv[0] );
	    exit ( ECANCELED );
	}
    }
    else
    {
	fprintf ( stderr,
		  "hpcm_sendmail: %s"
		  " terminated for unknown reason\n",
		  argv[0] );

	/* Parent exit when child died by
	   signal.
	*/
	exit ( ECANCELED );
    }
}

/* Main program.
*/
int main ( int argc, char ** argv )
{

    /* Original real and effective user ids. */

    uid_t ruid = getuid ();
    uid_t euid = geteuid ();

#   define MAXLEN 400

    /* Full hpcm_sendmail.rc file name:

    	~/.hpcm_contest/secure/hpcm_sendmail.rc

    */
    char rcfilename	[MAXLEN];

    /* Value for To:, Key:, and Key-Name: fields in
       the hpcm_sendmail.rc file.  These are like mail
       fields without continuation lines.
    */
    char to		[MAXLEN];
    char key		[MAXLEN];
    char key_name	[MAXLEN];

    /* Cc: field value:

    		" `id -un"
    */
    char cc		[MAXLEN];

    /* Reply-To: field value:

    		" `id -un`@`hostname -f`"
    */
    char reply_to	[MAXLEN];

    /* Date: field value:

    		" `date`"
    */
    char date		[MAXLEN];

    /* Signature: field value:

		"key_name md5sum-value"

       where the md5sum-value is the md5sum of:

		"X-HPCM-Date:date\n
	         X-HPCM-Reply-To:reply_to\n
		 key\n"
    */

     char signature	[MAXLEN];


    /* If there are any arguments, print doc. */

    if ( argc > 1 )
    {
        printf ( "%s", documentation );
	exit ( 1 );
    }

    /* Compute rcfilename =
          "~/.hpcm_contest/secure/hpcm_sendmail.rc"
    */
    if ( setreuid (-1, ruid) < 0 )
	errno_exit ( "set ruid" );
    {
	char * p;
	char * home = getenv ( "HOME" );
	int length;
	char contestname [MAXLEN];

	if ( home == NULL ) {
	    fprintf ( stderr,
		      "hpcm_sendmail:"
		      " cannot find `HOME'"
		      " environment variable\n" );
	    exit ( 1 );
	}

	if ( strlen ( home )
	     > sizeof ( contestname ) - 100 )
	    too_big_exit
		( "HOME environment variable" );
	strcpy ( contestname, home );

	p = contestname + strlen ( contestname );
	strcpy ( p, "/.hpcm_contest" );
	length = readlink ( contestname,
			    rcfilename,
		            sizeof ( rcfilename ) );
	if ( length < 0 )
	    errno_exit ( ".hpcm_contest link name" );
	if ( length == 0 ) {
	    fprintf ( stderr,
		      "empty .hpcm_contest link"
		      "name\n" );
	    exit ( 1 );
	}
	if ( length > sizeof ( rcfilename ) - 50 )
	    too_big_exit
		( ".hpcm_contest target name" );
	strcpy ( rcfilename + length,
		 "/secure/hpcm_sendmail.rc" );
	if ( rcfilename[0] != '/' ) {
	    fprintf ( stderr,
		      "non-absolute .hpcm_contest link"
		      "name\n" );
	    exit ( 1 );
	}
    }
    if ( setreuid (-1, euid) < 0 )
	errno_exit ( "set euid" );

    /* Read rc file and save parameters.
    */
    {
	char line [MAXLEN];
    	FILE * rcfile = fopen ( rcfilename, "r" );

	if ( rcfile == NULL ) {
	    fprintf ( stderr,
		      "hpcm_sendmail:"
		      " cannot open %s\n",
		      rcfilename );
	    errno_exit
		( "opening hpcm_judging.rc" );
	}

	to[0]		= 0;
	key[0]		= 0;
	key_name[0]	= 0;

	while ( 1 )
	{
	    if ( fgets ( line, sizeof (line), rcfile )
	    	 == NULL )
	    {
	        if ( feof ( rcfile ) )
		    break;
		else
		    errno_exit
			( "reading hpcm_sendmail.rc" );
	    }

	    /* Check line length and strip trailing
	       whitespace (gets rid of trailing '\n'
	       and trailing whitespace for key).
	    */
	    {
	    	int len = strlen ( line );
	    	char * p = line + len;

		if ( len > sizeof ( line ) - 10 )
		    too_big_exit
			( "line read from"
			  " hpcm_sendmail.rc" );

		while ( p > line && isspace ( p[-1] ) )
		    -- p;
		* p = '\0';
	    }

	    if ( strncasecmp ( line, "#", 1 ) == 0 )
	    	/* Do nothing on comment. */ ;
	    else if ( strncasecmp ( line, "To:", 3 )
	              == 0 )
	        strcpy ( to, line + 3 );
	    else if ( strncasecmp ( line, "Key:", 4 )
	              == 0 )
	        strcpy ( key, line + 4 );
	    else if ( strncasecmp ( line,
	                            "Key-Name:", 9 )
	              == 0 )
	        strcpy ( key_name, line + 9 );
	    else if ( strspn ( line, " \t\n" )
	              == strlen ( line ) )
	    	/* Do nothing on blank line. */ ;
	    else
	    {
		fprintf ( stderr,
		          "hpcm_sendmail:"
			  " bad line in"
			  " hpcm_sendmail.rc\n"
			  "    %s\n",
			  line );
		exit ( 1 );
	    }
	}

	if ( fclose ( rcfile ) == EOF )
	    errno_exit ( "closing hpcm_sendmail.rc" );
    }

    /* Compute cc and reply_to field values:

    	"`id -un`"
    	"`id -un`@`hostname -f`"
    */
    {
    	struct passwd * passwd;
	char * p = reply_to;
	char * endp = reply_to + sizeof ( reply_to );

	endp[-1] = 0;

	errno = 0;
    	passwd = getpwuid ( ruid );
	if ( passwd == NULL )
	{
	    if ( errno != 0 ) errno_exit ( "getpwid" );
	    else
	    {
	    	fprintf ( stderr, "hpcm_sendmail:"
				  " could not find real"
				  " UID %d in"
				  " /etc/passwd\n",
				  ruid );
		exit ( 1 );
	    }
	}

	* p ++ = ' ';
	strncpy ( p, passwd->pw_name,
	          ( endp - p ) - 5 );
	p += strlen ( p );
	if ( p > endp - 10 )
	    too_big_exit ( "Reply-To: field value" );

	strcpy ( cc, reply_to );

	* p ++ = '@';
	if ( gethostname ( p, ( endp - p ) - 5 ) < 0 )
	    errno_exit ( "gethostname" );
	p += strlen ( p );
	if ( p > endp - 10 )
	    too_big_exit ( "Reply-To: field value" );
    }

    /* Compute date. */
    {
	time_t t = time ( NULL );
	date[0] = ' ';
	strftime ( date + 1, sizeof ( date ) - 1,
                   "%a, %d %b %Y %H:%M:%S %z (%Z)",
		   localtime ( &t ) );
    }


    /* Compute signature. */
    {
    	FILE * files[3];
	pid_t child = exec_program ( files,
				     md5sum_argv,
				     md5sum_env );
	char * p = signature;
	char * endp = signature + sizeof ( signature );
	char * md5sump;

	fprintf ( files[0], "X-HPCM-Date:%s\n"
			    "X-HPCM-Reply-To:%s\n"
			    "%s\n",
			    date,
			    reply_to,
			    key + strspn ( key, " \t" )
			    );
	fclose ( files[0] );

	strcpy ( signature, key_name );
	p += strlen ( p );
	if ( p > endp - 10 )
	    too_big_exit ( "Signature: field value" );
	* p ++ = ' ';

	if ( fgets ( p, endp - p - 5, files[1] )
	     == NULL )
	{
	    if ( feof ( files[1] ) ) {
	    	fprintf ( stderr, "hpcm_sendmail:"
				  " premature eof from"
				  " md5sum\n" );
		exit ( 1 );
	    }
	    else
		errno_exit
		    ( "reading md5sum output" );
	}
	md5sump = p;
	p += strlen ( p );
	if ( p > endp - 10 )
	    too_big_exit ( "Signature: field value" );
	while ( p > signature && isspace ( p[-1]) )
	    -- p;
        if ( p <= signature || * -- p != '-' ) {
	    fprintf ( stderr, "hpcm_sendmail:"
			      " badly formatted md5sum"
			      " output\n    %s\n",
			      md5sump );
	    exit ( 1 );
	}
	while ( p > signature && isspace ( p[-1]) )
	    -- p;
	if ( p - md5sump != MD5SUM_LENGTH ) {
	    fprintf ( stderr, "hpcm_sendmail:"
			      " badly formatted md5sum"
			      " output\n    %s\n",
			      md5sump );
	    exit ( 1 );
	}
	* p = '\0';

	check_program ( files, child, md5sum_argv, 1 );
    }
    
    /* Send the mail. */
    {
    	FILE * files[3];
	pid_t child = exec_program ( files,
				     sendmail_argv,
				     sendmail_env );
	char line [MAXLEN];

	/* Output To and CC header fields. */
	fprintf ( files[0], "To:%s\n"
			    "Cc:%s\n",
			    to, cc );

	/* Read and copy any header, stopping at empty
	   line after header.
	*/
	if ( fgets ( line, sizeof (line), stdin )
	     == NULL )
	{
	    if ( feof ( stdin ) ) {
	    	fprintf ( stderr, "hpcm_sendmail:"
				  " empty mail input\n"
				  );
		exit ( 1 );
	    }
	    else
		errno_exit
		    ( "reading mail from input" );
	}
	if ( strlen ( line ) > sizeof ( line ) - 10 )
	    too_big_exit ( "input mail line" );

	/* A header is defined to begin with a line
	   that begins with with:
	   	word{-word}*:
	  where
	  	word::=upper-case-letter letter*
	*/
    	if ( isupper ( line [0] ) ) {
	    char * p = line;
	    int header_found;
	    while ( 1 ) {
	        if ( isupper ( *p ) ) {
		    while ( isalpha ( * ++ p ) );
		} else {
		    header_found = 0;
		    break;
		}

		if ( * p == '-' ) {
		    ++ p;
		    continue;
		} else if ( * p == ':' ) {
		    header_found = 1;
		    break;
		} else {
		    header_found = 0;
		    break;
		}
	    }

	    if ( header_found ) {
		while ( 1 ) {
		    fputs ( line, files[0] );
		    if ( fgets ( line, sizeof (line),
		    		       stdin )
			 == NULL )
		    {
			if ( feof ( stdin ) ) {
			    strcpy ( line, "\n" );
			    break;
			}
			else
			    errno_exit
				( "reading mail from"
				  " input" );
		    }
		    if ( strlen ( line )
		         > sizeof ( line ) - 10 )
			too_big_exit
			    ( "input mail line" );
		    if ( line[0] == '\n' ) break;
		}
	    }
	}

	/* Output our end of header and empty line
	   separating header and body.
	*/
	fprintf ( files[0], "X-HPCM-Date:%s\n"
			    "X-HPCM-Reply-To:%s\n"
			    "X-HPCM-Signature:%s\n"
			    "\n",
			    date,
			    reply_to,
			    signature );

	/* Output body. */
	if ( line[0] != '\n' ) fputs ( line, files[0] );
	while ( 1 ) {
	    if ( fgets ( line, sizeof (line), stdin )
		 == NULL )
	    {
		if ( feof ( stdin ) ) {
		    break;
		}
		else
		    errno_exit
			( "reading mail from input" );
	    }
	    if ( strlen ( line )
		 > sizeof ( line ) - 10 )
		too_big_exit ( "input mail line" );
	    fputs ( line, files[0] );
	}

	check_program
	    ( files, child, sendmail_argv, 0 );
    }

    return 0;
}
