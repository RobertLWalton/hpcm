/* Programming Contest Sendmail Program 
 *
 * File:	hpcm_sendmail.c
 * Authors:	Bob Walton (walton@deas.harvard.edu)
 * Date:	Sat Apr 27 01:39:33 EDT 2013
 *
 * The authors have placed this program in the public
 * domain; they make no warranty and accept no liability
 * for this program.
 *
 * RCS Info (may not be true date or author):
 *
 *   $Author: walton $
 *   $Date: 2013/04/27 06:01:40 $
 *   $RCSfile: hpcm_sendmail.c,v $
 *   $Revision: 1.14 $
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <netdb.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/signal.h>
#include <pwd.h>
#include <sys/utsname.h>
#include <time.h>

#ifndef HPCM_MD5SUM_LENGTH
#   define HPCM_MD5SUM_LENGTH 32
#endif

/* md5sums is list of possible argv values for execve
*/
#ifdef HPCM_MD5SUM
    char * const md5sum_argv_1[]
        = { HPCM_MD5SUM, NULL };
    char * const * md5sums[]
        = { md5sum_argv_1, NULL };
#else
    char * const md5sum_argv_1[]
         = { "/usr/bin/md5sum", NULL };
    char * const md5sum_argv_2[]
         = { "/bin/md5sum", NULL };
    char * const * md5sums[]
         = { md5sum_argv_1, md5sum_argv_2, NULL };
#endif

/* md5sum_env is env value for execve
*/
#ifndef HPCM_MD5SUM_ENV
#   define HPCM_MD5SUM_ENV "HPCM_MD5SUM"
#endif
char * const md5sum_env[]  = { HPCM_MD5SUM_ENV, NULL };

/* sendmails is list of possible argv values for execve
*/
#ifdef HPCM_SENDMAIL
    char * const sendmail_argv_1[]
        = { HPCM_SENDMAIL, NULL };
    char * const * sendmails[]
        = { sendmail_argv_1, NULL };
#else
    char * const sendmail_argv_1[]
        = { "/usr/sbin/sendmail", "-oi", "-t", NULL };
    char * const sendmail_argv_2[]
        = { "/sbin/sendmail", "-oi", "-t", NULL };
    char * const sendmail_argv_3[]
        = { "/usr/bin/sendmail", "-oi", "-t", NULL };
    char * const sendmail_argv_4[]
        = { "/bin/sendmail", "-oi", "-t", NULL };
    char * const sendmail_argv_5[]
        = { "/usr/lib/sendmail", "-oi", "-t", NULL };
    char * const sendmail_argv_6[]
        = { "/lib/sendmail", "-oi", "-t", NULL };
    char * const * sendmails[]
        = { sendmail_argv_1,
            sendmail_argv_2,
            sendmail_argv_3,
            sendmail_argv_4,
            sendmail_argv_5,
            sendmail_argv_6,
	    NULL };
#endif

/* sendmail_env is env value for execve
*/
#ifndef HPCM_SENDMAIL_ENV
#   define HPCM_SENDMAIL_ENV "HPCM_SENDMAIL"
#endif
char * sendmail_env[]  = { HPCM_SENDMAIL_ENV, NULL };

char documentation [] =
"cat your_mail_file | hpcm_sendmail"
	" [-test] [contest-directory]\n"
"\n"
"    This program performs the same functions as:\n"
"\n"
"            /usr/sbin/sendmail -oi -t\n"
"\n"
"    except that this program supplies the `To:'\n"
"    field value and authentication field values that\n"
"    will send the mail to the appropriate HPCM judge\n"
"    and will ensure that replies are sent to the\n"
"    sender.  This program also supplies a `Cc:'\n"
"    field that cc's the mail to the sender.\n"
"\n"
"    This program is normally setuid to a judging\n"
"    account so it can read certain authentication\n"
"    information files that are not available to the\n"
"    sender.\n"
"\n"
"    This program uses `~/.hpcm_contest by default as\n"
"    the name of the contest directory that contains\n"
"    the secure/hpcm_sendmail.rc file this program\n"
"    needs to find the `To:' field and compute au-\n"
"    thentication.\n"
"\n"
"    With the -test option this program pipes to\n"
"    cat(1) the message that it would otherwise pipe\n"
"    to /usr/bin/sendmail.  This is used primarily to\n"
"    build tests in environments where procmailrc has\n"
"    been disabled because of selinux settings.\n"
"\n"
"    The program will write an error message on the\n"
"    standard error output if any system call is in\n"
"    error.\n" ;

void errno_exit ( char * m )
{
    fprintf ( stderr,
              "hpcm_sendmail: system call error:"
	      " %s:\n    %s\n",
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

   Argvs is a list of possible argv values for execev.
   Each is tried in turn until one works.
*/
pid_t exec_program
    ( FILE * files[3],
      char * const * argvs[],
      char * const env[] )
{
    int in_pipe[2];
    int out_pipe[2];
    int err_pipe[2];
    pid_t child;
    char * const ** argvp;

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

	argvp = argvs;
	while ( * argvp )
	{
	    char * const * argv = * argvp ++;
	    execve ( argv[0], argv, env );
	}
	argvp = argvs;
	while ( * argvp )
	{
	    char * const * argv = * argvp ++;
	    fprintf ( stderr, "Could not execute %s\n",
	    		      argv[0] );
	}
	errno_exit ( "execev" );
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
      char * program,
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
		  program,
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
		      program,
		      strerror ( code ) );

	    /* Parent exit when child returned non-zero
	       status.
	    */
	    exit ( code );
	} else if ( stderr_nonempty ) {
	    fprintf ( stderr,
		      "hpcm_sendmail: %s"
		      " terminated with error output\n",
		      program );
	    exit ( ECANCELED );
	}
    }
    else
    {
	fprintf ( stderr,
		  "hpcm_sendmail: %s"
		  " terminated for unknown reason\n",
		  program );

	/* Parent exit when child terminated for
	   unknown reason.
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

    	<contest-directory>/secure/hpcm_sendmail.rc

    where <contest-directory> is the program first
    argument, which defaults to "~/.hpcm_contest".
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

    /* Switch to indicate -test option is present
     */
    int test_option = 0;


    /* If there are too many arguments or the first
       matches -doc*, print documentation.
    */

    if ( argc > 2
         ||
	 ( argc == 2
	   &&
	   strncmp ( "-doc", argv[1], 4 ) == 0 ) )
    {
        printf ( "%s", documentation );
	exit ( 1 );
    }

    if ( argc >= 2
         &&
	 strcmp ( "-test", argv[1] ) == 0 )
    {
        test_option = 1;
	-- argc, ++ argv;
    }

    /* Compute rcfilename =
          "<contest-directory>/secure/hpcm_sendmail.rc"
       where <contest-directory> defaults to 
       ~/.hpcm_contest.  Using the real user ID, the
       contest directory is resolved if it is a symbol-
       lic link, as the unresolved name many not be
       accessible by the effective user.  E.g., only the
       real user can access ~/.hpcm_contest directly.
    */
    if ( setreuid (-1, ruid) < 0 )
	errno_exit ( "set ruid" );
    {
	char * p;
	int length;
	char * contest_directory;
	char default_directory [MAXLEN];

	/* Compute contest directory name.
	*/
	if ( argc == 2 ) {
	    contest_directory = argv[1];
	} else {

	    /* Compute the ~ expansion of
	       ~/.hpcm_contest in default_directory.
	    */
	    char * home = getenv ( "HOME" );
	    if ( home == NULL ) {
		fprintf ( stderr,
			  "hpcm_sendmail:"
			  " cannot find `HOME'"
			  " environment variable\n" );
		exit ( 1 );
	    }

	    if ( strlen ( home )
		 > sizeof ( default_directory ) - 100 )
		too_big_exit
		    ( "HOME environment variable" );
	    strcpy ( default_directory, home );

	    p = default_directory
	      + strlen ( default_directory );
	    strcpy ( p, "/.hpcm_contest" );
	    contest_directory = default_directory;
	}

	/* Resolve symbolic link if any.
	*/
	length = readlink ( contest_directory,
			    rcfilename,
		            sizeof ( rcfilename ) );
	
	if ( errno == EINVAL ) {

	    /* Contest_directory is NOT a symbolic
	       link.
	    */
	    length = strlen ( contest_directory );
	    if ( length > sizeof ( rcfilename ) - 50 )
		too_big_exit
		    ( "contest directory name" );
	    strcpy ( rcfilename, contest_directory );
	} else {

	    /* Contest_directory is a symbolic link.
	    */
	    if ( length < 0 )
		errno_exit
		    ( "contest directory link name" );
	    if ( length == 0 ) {
		fprintf ( stderr,
			  "empty contest directory link"
			  " name\n" );
		exit ( 1 );
	    }
	    if ( length > sizeof ( rcfilename ) - 50 )
		too_big_exit
		    ( "contest directory link target"
		      " name" );
	}

	/* Flush out rcfilename.
	*/
	strcpy ( rcfilename + length,
		 "/secure/hpcm_sendmail.rc" );
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
		      " cannot open %s for reading\n",
		      rcfilename );
	    if ( rcfilename[0] != '/' )
		fprintf ( stderr,
			  "problem may be that the"
			  " contest directory or its"
			  " link resolution is not an"
			  " absolute pathname\n" );

	    errno_exit
		( "opening hpcm_sendmail.rc" );
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
	char hostname[MAXLEN];
	struct hostent * hostentp;

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

	/* We have to use gethostbyname to get the
	   full name of the current host. */

	if ( gethostname ( hostname, MAXLEN - 5 ) < 0 )
	    errno_exit ( "gethostname" );
	if ( strlen ( hostname ) > MAXLEN - 10 )
	    too_big_exit ( "hostname" );
	hostentp = gethostbyname ( hostname );
	if ( hostentp == NULL )
	    errno_exit ( "gethostbyname" );
	if ( p + strlen ( hostentp->h_name )
	         > endp - 10 )
	    too_big_exit ( "Reply-To: field value" );
	strcpy ( p, hostentp->h_name );
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
				     md5sums,
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

	strcpy ( p, key_name );
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
	if ( p - md5sump != HPCM_MD5SUM_LENGTH ) {
	    fprintf ( stderr, "hpcm_sendmail:"
			      " badly formatted md5sum"
			      " output\n    %s\n",
			      md5sump );
	    exit ( 1 );
	}
	* p = '\0';

	check_program ( files, child, "md5sum", 1 );
    }
    
    /* Send the mail. */
    {
    	FILE * files[3];
	char * const testmail_argv[]
        = { "cat", NULL };
	char * const * testmails[]
	    = { testmail_argv, NULL };
	pid_t child = exec_program ( files,
				     test_option ?
					 testmails :
					 sendmails,
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
	    ( files, child, "sendmail", 0 );
    }

    return 0;
}
