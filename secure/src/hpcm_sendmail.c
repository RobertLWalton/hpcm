/* Programming Contest Sendmail Program 
 *
 * File:	hpcm_sendmail.c
 * Authors:	Bob Walton (walton@deas.harvard.edu)
 * Date:	Mon Sep 11 06:14:00 EDT 2000
 *
 * The authors have placed this program in the public
 * domain; they make no warranty and accept no liability
 * for this program.
 *
 * RCS Info (may not be true date or author):
 *
 *   $Author: hc3 $
 *   $Date: 2000/09/18 02:25:38 $
 *   $RCSfile: hpcm_sendmail.c,v $
 *   $Revision: 1.1 $
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/signal.h>
extern const char * const sys_siglist[];
#include <pwd.h>
#include <sys/utsname.h>
#include <time.h>

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
   characters copies and 0 if not.
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
   exec_program.  Close the input to this program.  Copy
   any output produced by this program to the standard
   output, and close the output files.  Wait for the
   program to terminate.  Exit with error message if the
   program terminates with non-zero status.
*/
void check_program
    ( FILE * files[3],
      pid_t child,
      char * const argv[] )
{
    int child_exit_status;

    if ( fclose ( files[0] ) == EOF )
    	errno_exit ( "closing input to subprogram" );

    flush_output ( files[1] );
    flush_output ( files[2] );

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
		  sys_siglist [ sig ] );

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
	}
    }
    else
    {
	fprintf ( stderr,
		  "hpcm_sendmail: %s"
		  " terminated for unknwon reason\n",
		  argv[0] );

	/* Parent exit when child died by
	   signal.
	*/
	exit ( ECANCELED );
    }
}

/* Copy src string to des string as per strcpy,
   but strip whitespace from beginning and end
   of result.
UNUSED?
*/
void trimcpy ( char * des, char * src )
{
    char c;
    char * end;

    while ( ( c = * src ++ ) && isspace ( c ) );

    end = des;

    while ( c )
    {
    	* des ++ = c;
	if ( ! isspace ( c ) ) end = des;
	c = * src ++;
    }

    * end = 0;
}

/* Main program.
*/
int main ( int argc, char ** argv )
{

    /* Real and effective user and group id's. */

    uid_t ruid = getuid ();
    uid_t euid = geteuid ();
    gid_t rgid = getgid ();
    gid_t egid = getegid ();

#   define MAXLEN 400

    /* Full hpcm_sendmail.rc file name:

    	`dirname $0`/../secure/hpcm_sendmail.rc

    */
    char rcfilename	[MAXLEN];

    /* Value for To:, Key:, and Key-Name: fields in
       the hpcm_sendmail.rc file.  These are like mail
       fields without continuation lines.
    */
    char to		[MAXLEN];
    char key		[MAXLEN];
    char key_name	[MAXLEN];

    /* Reply-To: field value:

    		" `id -un`@`hostname -f`"
    */
    char reply_to	[MAXLEN];

    /* Date: field value:

    		" `date`"
    */
    char date		[MAXLEN];


    /* If there are any arguments, print doc. */

    if ( argc > 1 )
    {
        printf ( "%s", documentation );
	exit ( 1 );
    }

    /* Compute rcfilename =
          "`dirname $0`/../secure/hpcm_sendmail.rc"
    */
    {
	char * p;

	if ( strlen ( argv[0] )
	     > sizeof ( rcfilename ) - 100 )
	    too_big_exit ( "program name (arg0)" );
	strcpy ( rcfilename, argv[0] );

	p = rindex ( rcfilename, '/' );
	if ( p == NULL )
	{
	    p = rcfilename;
	    * p ++ = '.';
	}

	strcpy ( p, "/../secure/hpcm_sendmail.rc" );
    }

    /* Read rc file and save parameters.
    */
    {
	char line [MAXLEN];
    	FILE * rcfile = fopen ( rcfilename, "r" );

	if ( rcfile == NULL )
	    errno_exit
		( "opening hpcm_sendmail.rc" );

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
	       `\n'.
	    */
	    {
	    	int len = strlen ( line );

		if ( len > sizeof ( line ) - 10 )
		    too_big_exit
			( "line read from"
			  " hpcm_sendmail.rc" );

		if ( line[len-1] == '\n' )
			line[len-1] = 0;
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

    /* Compute reply_to field value:

    	"`id -un`@`hostname -f`
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

	* p ++ = '@';
	if ( gethostname ( p, ( endp - p ) - 5 ) < 0 )
	    errno_exit ( "gethostname" );
	p += strlen ( p );
	if ( p > endp - 10 )
	    too_big_exit ( "Reply-To: field value" );

	* p ++ = '.';
	if ( getdomainname ( p, ( endp - p ) - 5 ) < 0 )
	    errno_exit ( "gethostname" );
	p += strlen ( p );
	if ( p > endp - 10 )
	    too_big_exit ( "Reply-To: field value" );

    }

    /* Compute date. */
    {
    	time_t t = time ( NULL );
	sprintf ( date, " %s %s %s %d", ctime ( & t ),
			tzname[0], tzname[1], timezone );
    }


    printf ( "To:%s\n", to );
    printf ( "Key:%s\n", key );
    printf ( "Key-Name:%s\n", key_name );
    printf ( "Reply-To:%s\n", reply_to );
    printf ( "Date:%s\n", date );

    {
    	struct utsname utsname;

	if ( uname ( &utsname ) < 0 )
	    errno_exit ( "utsname" );


	printf ( "Node Name: %s\n", utsname.nodename );
	printf ( "Domain Name: %s\n", utsname.__domainname );
    }
    return 0;
}
