/* Programming Contest Sandbox Program 
 *
 * File:	hpcm_sandbox.c
 * Authors:	Bob Walton (walton@deas.harvard.edu)
 * Date:	Sat May  6 01:14:37 EDT 2006
 *
 * The authors have placed this program in the public
 * domain; they make no warranty and accept no liability
 * for this program.
 *
 * RCS Info (may not be true date or author):
 *
 *   $Author: hc3 $
 *   $Date: 2006/05/06 05:36:47 $
 *   $RCSfile: hpcm_sandbox.c,v $
 *   $Revision: 1.19 $
 */

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <sys/param.h>
#include <sys/signal.h>
#include <fcntl.h>
#include <errno.h>
#include <pwd.h>

char documentation [] =
"hpcm_sandbox [options] program argument ...\n"
"\n"
"    This program first checks its arguments for\n"
"    options that set resource limits:\n"
"\n"
"      -cputime N     Cpu Time in Seconds (600)\n"
"      -space N       Virtual Address Space Size,\n"
"                     in Bytes (512m)\n"
"      -datasize N    Data Area Size in Bytes (4m)\n"
"      -stacksize N   Stack Size in Bytes (4m)\n"
"      -filesize N    Output File Size in Bytes (4m)\n"
"      -core N        Core Dump Size in Bytes (4m)\n"
"      -openfiles N   Number of Open Files (30)\n"
"      -processes N   Number of Processes (50)\n"
"\n"
"    Here N is a non-negative decimal integer that\n"
"    can end with `k' to multiply it by 1024 or `m'\n"
"    to multiply it by 1024 * 1024.  The values above\n"
"    in parentheses are the default values.\n"
"\n"
"    There are also two other options.  First, with\n"
"    the\n"
"\n"
"      -watch\n"
"\n"
"    option, this program forks and the parent waits\n"
"    for the child to complete the rest of this pro-\n"
"    gram's action.  If the child terminates with a\n"
"    signal, the parent prints an error message iden-\n"
"    tifying the signal.  It uses strsignal(3) to do\n"
"    this after changing SIGKILL with measured CPU\n"
"    time over the limit to SIGXCPU.  The parent re-\n"
"    turns a 0 exit code if the child does not termi-\n"
"    nate with a signal, and returns 128 + the pos-\n"
"    sibly changed signal number as an exit code if\n"
"    the child does terminate with a signal.\n"
"\f\n"
"    Alternatively, with the\n"
"\n"
"      -tee FILE\n"
"\n"
"    option, the program does the same thing as with\n"
"    the -watch option, but in addition pipes the\n"
"    standard output of the child back through the\n"
"    parent, where it is copied into the FILE and\n"
"    also copied to the standard output, just like\n"
"    the UNIX tee(1) command.  However, unlike\n"
"    tee(1), if the filesize is exceeded for the\n"
"    standard output, a SIGXFSZ signal is sent to\n"
"    the child.\n"
"\n"
"    The rest of this program, which runs as the\n"
"    child with a -watch or -tee option, or as the\n"
"    program proper otherwise, does the following.\n"
"\n"
"    First, if this program's effective user ID is\n"
"    `root', this program eliminates any supplemen-\n"
"    tary groups that the process might have, and\n"
"    changes the effective user and group IDs to\n"
"    those of `sandbox', as looked up in /etc/passwd.\n"
"\n"
"    Then this program sets the real user and group\n"
"    IDs to the effective user and group IDs, sets\n"
"    the resource limits determined by the options\n"
"    and defaults, sets the environment to contain\n"
"    the strings in HPCM_SANDBOX_ENV if that environ-\n"
"    ment variable is defined, or just \"SANDBOX\"\n"
"    otherwise, and executes the program with the\n"
"    given arguments.  If HPCM_SANDBOX_ENV is de-\n"
"    fined, it consists of whitespace separated\n"
"    strings that become the environment, with re-\n"
"    placement of `\\ ', `\\t', `\\n', `\\f', `\\v',\n"
"    and `\\\\' within each environment string by\n"
"    space, tab, new line, form feed, vertical tab,\n"
"    and backslash, respectively.  `\\ ', which de-\n"
"    notes a single space, does not separate strings.\n"
"\f\n"
"    Normally the `sandbox' user is not allowed to\n"
"    log in and owns no useful files or directories.\n"
"\n"
"    The program will write an error message on the\n"
"    standard error output if any system call is in\n"
"    error.\n" ;

void errno_exit ( char * m )
{
    fprintf ( stderr, "hpcm_sandbox: system call error:"
                      " %s:\n    %s\n",
		      m, strerror ( errno ) );
    exit ( 1 );
}

/* Main program.
*/
int main ( int argc, char ** argv )
{
    /* Index of next argv to process. */

    int index = 1;

    /* Options with default values. */

    int cputime = 600;
    int space = 512 * 1024 * 1024;
    int datasize = 4 * 1024 * 1024;
    int stacksize = 4 * 1024 * 1024;
    int filesize = 4 * 1024 * 1024;
    int core = 4 * 1024 * 1024;
    int openfiles = 30;
    int processes = 50;
    int watch = 0;
    int tee = 0;
    const char * file = NULL;

    /* Effective IDs of this process after change
       from `root' to `sandbox' */

    uid_t euid;
    gid_t egid;

    /* Environment for program to be executed. */

    char ** env;

    /* Consume the options. */

    while ( index < argc )
    {
        int * option;

        if ( strcmp ( argv[index], "-watch" )
	     == 0 )
	{
	    watch = 1;
	    ++ index;
	    continue;
	}
        else if ( strcmp ( argv[index], "-tee" )
	     == 0 )
	{
	    tee = 1;
	    ++ index;
	    if ( index >= argc )
	    {
		fprintf ( stderr,
			  "hpcm_sandbox: Too few"
			  " arguments\n" );
		exit (1);
	    }
	    file = argv[index++];
	    continue;
	}
        else if ( strcmp ( argv[index], "-cputime" )
	     == 0 )
	    option = & cputime;
        else if ( strcmp ( argv[index], "-space" )
	     == 0 )
	    option = & space;
        else if ( strcmp ( argv[index], "-datasize" )
	     == 0 )
	    option = & datasize;
        else if ( strcmp ( argv[index], "-stacksize" )
	     == 0 )
	    option = & stacksize;
        else if ( strcmp ( argv[index], "-filesize" )
	     == 0 )
	    option = & filesize;
        else if ( strcmp ( argv[index], "-core" )
	     == 0 )
	    option = & core;
        else if ( strcmp ( argv[index], "-openfiles" )
	     == 0 )
	    option = & openfiles;
        else if ( strcmp ( argv[index], "-processes" )
	     == 0 )
	    option = & processes;
        else break;

	++ index;

	if ( index >= argc )
	{
	    fprintf ( stderr,
	              "hpcm_sandbox: Too few"
		      " arguments\n" );
	    exit (1);
	}

	/* Compute the number. */

	{
	    char * s = argv[index];
	    int n = 0;
	    int c;
	    int digit_found = 0;

	    while ( c = * s ++ )
	    {
	        if ( c < '0' || c > '9' ) break;
		digit_found = 1;

		if ( n > ( INT_MAX / 10 ) )
		{
		    fprintf ( stderr,
			      "hpcm_sandbox: Number"
			      " too large: %s\n",
			      argv[index] );
		    exit (1);
		}
		n = 10 * n + ( c - '0' );
	    }

	    if ( c == 'm' )
	    {
	        c = * s ++;
		if ( n > ( INT_MAX >> 20 ) )
		{
		    fprintf ( stderr,
			      "hpcm_sandbox: Number"
			      " too large: %s\n",
			      argv[index] );
		    exit (1);
		}
		n <<= 20;
	    } else if ( c == 'k' )
	    {
	        c = * s ++;
		if ( n > ( INT_MAX >> 10 ) )
		{
		    fprintf ( stderr,
			      "hpcm_sandbox: Number"
			      " too large: %s\n",
			      argv[index] );
		    exit (1);
		}
		n <<= 10;
	    }

	    if ( c != 0 || ! digit_found )
	    {
		fprintf ( stderr,
			  "hpcm_sandbox: Bad number:"
			  " %s\n",
			  argv[index] );
		exit (1);
	    }

	    * option = n;
        }

	++ index;
    }

    /* If the program name is not left, or if it 
       matches -doc*, print doc. */

    if (    index >= argc
	 || strncmp ( "-doc", argv[index], 4 ) == 0 )
    {
        printf ( "%s", documentation );
	exit ( 1 );
    }

    /* If -watch or -tee, start child and watch it. */

    if ( watch || tee )
    {
	pid_t child;	    /* child ID */
	int out_pipe[2];    /* stdout pipe */
        int tee_fd;	    /* Tee file descriptor. */

        if ( tee )
	{
	    tee_fd =
	        open ( file, O_WRONLY|O_CREAT|O_TRUNC,
			     0666 );
	    if ( tee_fd < 0 )
	        errno_exit ( "opening -tee file" );
	    if ( pipe ( out_pipe ) < 0 )
	        errno_exit ( "pipe" );
	}
	child = fork ();

	if ( child < 0 )
	    errno_exit ( "fork" );

	if ( child == 0 )
	{
	    /* Child executes this. */

	    if ( tee )
	    {
	        if ( close ( out_pipe[0] ) < 0 )
		    errno_exit
		        ( "out pipe child close" );
		if ( dup2 ( out_pipe[1], 1 ) < 0 )
		    errno_exit
		        ( "out child dup2" );
		{
		    int fd;
		    int max_fd = getdtablesize ();

		    if ( max_fd < 0 ) max_fd = 256;

		    for ( fd = 3; fd < max_fd; ++ fd )
			close ( fd );
		}
	    }

	    /* Child now falls through to code that
	       executes with or without -watch or -tee.
	    */
	}
	else
	{
	    /* Parent executes this. */

	    int status;

	    if ( tee )
	    {
		int outsize = 0;
		int sigxfsz_sent = 0;

	        if ( close ( out_pipe[1] ) < 0 )
		    errno_exit
		        ( "out pipe parent close" );

		while ( 1 )
		{
		    char buffer[256];
		    int c = read ( out_pipe[0],
			           buffer, 256 );
		    if ( c < 0 && errno == EINTR )
		        continue;
		    else if ( c <= 0 ) break;

		    outsize += c;
		    if ( outsize > filesize )
		        c -= ( outsize - filesize );
		    if ( c > 0 )
		    {
			if (   write ( 1, buffer, c )
			     < 0 )
			    errno_exit
				( "parent stdout"
				  " write" );
			if (   write ( tee_fd,
			               buffer, c )
			     < 0 )
			    errno_exit
				( "parent tee file"
				  " write" );
		    }
		    if (    outsize > filesize
		         && ! sigxfsz_sent )
		    {
			sigxfsz_sent = 1;
			kill ( child, SIGXFSZ );
		    }
		}
		close ( tee_fd );
	    }

	    if ( wait ( & status ) < 0 )
		errno_exit ( "wait" );

	    if ( WIFSIGNALED ( status ) )
	    {
		int sig = WTERMSIG ( status );

		/* Cpu time exceeded is signalled by
		   SIGKILL, so we check for it and
		   change the sig to SIGXCPU.
		 */

		if ( sig == SIGKILL )
		{
		    struct rusage usage;
		    long sec;

		    if ( getrusage ( RUSAGE_CHILDREN,
		                     & usage )
			 < 0 )
		        errno_exit ( "getrusage" );

		    sec  = usage.ru_utime.tv_sec;
		    sec += usage.ru_stime.tv_sec;
		    if ( ( usage.ru_utime.tv_usec
		           + usage.ru_stime.tv_usec )
			 >= 1000000 )
		        ++ sec;
		    if ( sec >= cputime )
		    	sig = SIGXCPU;
		}

		fprintf ( stderr,
			  "hpcm_sandbox: Child"
			  " terminated with signal:"
			  " %s\n",
			  strsignal ( sig ) );

		/* Parent exit when child died by
		   signal.
		*/
		exit ( 128 + sig );
	    }

	    /* Parent exit when child did NOT die by
	       signal.
	    */
	    exit ( 0 );
	}
    }

    /* Child (or original process if no -watch)
       continues execution here.
    */

    if ( geteuid() == 0 ) {

        /* Execute if effective user is root. */

	gid_t groups [1];

	/* Clear the supplementary groups. */

	if ( setgroups ( 0, groups ) < 0 )
	    errno_exit ( "root setgroups" );

	/* Set the effective user and group ID to
	   that of the `sandbox' user.
	*/
	while ( 1 )
	{
	    struct passwd * p;

            p = getpwent ();

	    if ( p == NULL )
	    {
	        fprintf ( stderr, "hpcm_sandbox: Could"
				  " not find `sandbox'"
		                  " in /etc/passwd\n" );
		exit ( 1 );
	    }

	    if ( strcmp ( p->pw_name, "sandbox" )
	         == 0 )
	    {
		if ( setregid ( -1, p->pw_gid )
		     < 0 )
		     errno_exit ( "root setregid" );
		if ( setreuid ( -1, p->pw_uid )
		     < 0 )
		     errno_exit ( "root setreuid" );

		endpwent ();
		break;
	    }
	}

	/* End root execution. */
    }

    /* Set the real user and group IDs to to effective
       user and group IDs.
    */

    euid = geteuid ();
    egid = getegid ();

    if ( setreuid ( euid, -1 ) < 0 )
        errno_exit ( "setreuid setting real uid" );
    if ( setregid ( egid, -1 ) < 0 )
        errno_exit ( "setregid setting real gid" );

    {
        /* Set the resource limits */

	struct rlimit limit;

	limit.rlim_cur = cputime;
	limit.rlim_max = cputime + 5;
	/* rlim_cur is when the SIGXCPU signal is sent,
	 * and rlim_max is when the SIGKILL signal is
	 * sent.  Cases have been observed in which
	 * the usage.ru_{u,s}time sum as read by this
	 * program does not quite exceed the rlim_{cur,
	 * max} limit when the SIG{XCPU,KILL} signal is
	 * sent.  SIGKILL is turned into SIGXCPU by code
	 * elsewhere in this program if the usage.ru_
	 * {u,s}time sum exceeds cputime.  To make this
	 * work, we must be sure SIGKILL is sent well
	 * after the sum exceeds the limit, so we add
	 * 5 seconds to rlim_max.
	 */
        if ( setrlimit ( RLIMIT_CPU, & limit ) < 0 )
	    errno_exit
	        ( "setrlimit RLIMIT_CPU" );

#	ifdef RLIMIT_AS
	limit.rlim_cur = space;
	limit.rlim_max = space;
        if ( setrlimit ( RLIMIT_AS, & limit ) < 0 )
	    errno_exit
	        ( "setrlimit RLIMIT_AS" );
#	endif

	limit.rlim_cur = datasize;
	limit.rlim_max = datasize;
        if ( setrlimit ( RLIMIT_DATA, & limit ) < 0 )
	    errno_exit
	        ( "setrlimit RLIMIT_DATA" );

	limit.rlim_cur = stacksize;
	limit.rlim_max = stacksize;
        if ( setrlimit ( RLIMIT_STACK, & limit ) < 0 )
	    errno_exit
	        ( "setrlimit RLIMIT_STACK" );

	limit.rlim_cur = filesize;
	limit.rlim_max = filesize;
        if ( setrlimit ( RLIMIT_FSIZE, & limit ) < 0 )
	    errno_exit
	        ( "setrlimit RLIMIT_FSIZE" );

	limit.rlim_cur = core;
	limit.rlim_max = core;
        if ( setrlimit ( RLIMIT_CORE, & limit ) < 0 )
	    errno_exit
	        ( "setrlimit RLIMIT_CORE" );

	limit.rlim_cur = openfiles;
	limit.rlim_max = openfiles;
        if ( setrlimit ( RLIMIT_NOFILE, & limit ) < 0 )
	    errno_exit
	        ( "setrlimit RLIMIT_NOFILE" );

#	ifdef RLIMIT_NPROC
	limit.rlim_cur = processes;
	limit.rlim_max = processes;
        if ( setrlimit ( RLIMIT_NPROC, & limit ) < 0 )
	    errno_exit
	        ( "setrlimit RLIMIT_NPROC" );
#	endif
    }

    {
	/* Compute the environment for the program to be
	   executed.
	*/

	char * hpcm_sandbox_env =
	    getenv ( "HPCM_SANDBOX_ENV" );

	if ( hpcm_sandbox_env == NULL )
	{
	    env = realloc ( NULL, 2 * sizeof (char *) );
	    if ( env == NULL ) errno_exit ( "realloc" );
	    env[0] = "SANDBOX";
	    env[1] = NULL;
	}
	else
	{
	    char * buffer = malloc
		( strlen ( hpcm_sandbox_env ) + 1 );
	    char * ep = hpcm_sandbox_env;
	    char * bp = buffer;
	    int i = 0;		/* Index in env. */
	    int size = 101;	/* Size of env. */

	    if ( buffer == NULL )
		errno_exit
		    ( "malloc ( strlen ("
		      " HPCM_SANDBOX_ENV ) )" );

	    env = realloc
		( NULL, size * sizeof (char *) );
	    if ( env == NULL )
		errno_exit ( "realloc env" );

	    /* Reorganize environment strings in buffer
	       so each ends with a 0 and they have
	       escapes converted to characters.  As
	       each string appears in buffer, record its
	       address in env.
	    */

	    while (1)
	    {
		while ( isspace ( * ep ) ) ++ ep;
		if ( * ep == 0 ) break;

		if ( i >= size - 1 )
		{
		    size *= 2;
		    env = realloc
			( env, size * sizeof (char *) );
		    if ( env == NULL )
			errno_exit ( "realloc env" );
		}

		env[i++] = bp;

		while (1)
		{
		    if ( * ep == '\\' )
		    {
			switch ( * ++ ep )
			{
			case ' ':
			    * bp ++ = ' '; break;
			case 't':
			    * bp ++ = '\t'; break;
			case 'n':
			    * bp ++ = '\n'; break;
			case 'f':
			    * bp ++ = '\f'; break;
			case 'v':
			    * bp ++ = '\v'; break;
			case '\\':
			    * bp ++ = '\\'; break;
			default:
			    fprintf ( stderr,
				      "bad escape in"
				      " HPCM_SANDBOX_"
				      "ENV: \\%c\n",
				      * ep );
			    exit ( 1 );
			}
			++ ep;
		    }
		    else if ( isspace ( * ep ) )
			break;
		    else if ( ! * ep )
			break;
		    else
			* bp ++ = * ep ++;
		}

		* bp ++ = 0;
	    }
	    env[i] = NULL;
	}
    }

    /* Execute program with arguments and computed
       environment.
    */

    execve ( argv[index], argv + index, env );

    /* If execve fails, print error messages. */

    fprintf ( stderr, "hpcm_sandbox: could not:"
    		      " execute %s\n",
		      argv[index] );
    errno_exit ( "execve" );
}
