/* Programming Contest Sandbox Program 
 *
 * File:	hpcm_sandbox.c
 * Authors:	Bob Walton (walton@deas.harvard.edu)
 * Date:	Sun Sep  3 14:56:38 EDT 2000
 *
 * The authors have placed this program in the public
 * domain; they make no warranty and accept no liability
 * for this program.
 *
 * RCS Info (may not be true date or author):
 *
 *   $Author: acm-cont $
 *   $Date: 2000/09/04 02:04:31 $
 *   $RCSfile: hpcm_sandbox.c,v $
 *   $Revision: 1.3 $
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <sys/param.h>
#include <sys/signal.h>
extern const char * const sys_siglist[];
#include <errno.h>
#include <pwd.h>

char documentation [] =
"hpcm_sandbox [options] program argument ...\n"
"\n"
"    This program first checks its arguments for\n"
"    options that set resource limits:\n"
"\n"
"      -cputime N     Cpu Time in Seconds (60)\n"
"      -datasize N    Data Area Size in Bytes (50k)\n"
"      -stacksize N   Stack Size in Bytes (50k)\n"
"      -filesize N    Output File Size in Bytes (50k)\n"
"      -core N        Core Dump Size in Bytes (0)\n"
"      -openfiles N   Number of Open Files (20)\n"
"      -processes N   Number of Processes (1)\n"
"\n"
"    Here N is a positive decimal integer that can\n"
"    end with `k' to multiply it by 1024 or `m' to\n"
"    multiply it by 1024 * 1024.  The values above in\n"
"    parentheses are the default values.\n"
"\n"
"    There is also another possible option:\n"
"\n"
"      -watch\n"
"\n"
"    With this option, this program forks with the\n"
"    parent waiting for the child to complete the\n"
"    the rest of this program's action.  If the\n"
"    child terminates with a signal, the parent\n"
"    prints an error message identifying the signal.\n"
"    In any case the parent returns a 0 exit code.\n"
"\n"
"    Next, if this program's effective user ID is\n"
"    `root', this program eliminates any supplemen-\n"
"    tary groups that the process might have, and\n"
"    changes the effective user and group IDs to\n"
"    those of `sandbox', as looked up in /etc/passwd.\n"
"\n"
"    Then this program sets the real user and group\n"
"    IDs to the effective user and group IDs, sets\n"
"    the resource limits determined by the options\n"
"    and defaults, sets the environment to contain\n"
"    just \"SANDBOX\", and executes the program with\n"
"    the given arguments.\n"
"\n"
"    Normally the `sandbox' user is not allowed to\n"
"    log in and owns no files or directories.\n"
"\n"
"    The program will write an error message on the\n"
"    standard error output if any system call is in\n"
"    error.\n" ;

char * env [] = { "SANDBOX", 0 };

void errno_exit ( char * m )
{
    fprintf ( stderr, "hpcm_sandbox system call error:"
                      " %s: %s\n",
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

    int cputime = 60;
    int datasize = 50 * 1024;
    int stacksize = 50 * 1024;
    int filesize = 50 * 1024;
    int core = 0;
    int openfiles = 20;
    int processes = 1;
    int watch = 0;


    /* Effective IDs of this process after change
       from `root' to `sandbox' */

    uid_t euid;
    gid_t egid;

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
        else if ( strcmp ( argv[index], "-cputime" )
	     == 0 )
	    option = & cputime;
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
	              "Not enough arguments\n" );
	    exit (1);
	}

	/* Compute the number. */

	{
	    char * s = argv[index];
	    int n = 0;
	    int c;

	    while ( c = * s ++ )
	    {
	        if ( c < '0' || c > '9' ) break;
		if ( n > ( 1 << 27 ) )
		{
		    fprintf ( stderr,
			      "Number too large: %s\n",
			      argv[index] );
		    exit (1);
		}
		n = 10 * n + ( c - '0' );
	    }

	    if ( c == 'm' )
	    {
	        c = * s ++;
		if ( n >= ( 1 << 11 ) )
		{
		    fprintf ( stderr,
			      "Number too large: %s\n",
			      argv[index] );
		    exit (1);
		}
		n <<= 20;
	    } else if ( c == 'k' )
	    {
	        c = * s ++;
		if ( n >= ( 1 << 21 ) )
		{
		    fprintf ( stderr,
			      "Number too large: %s\n",
			      argv[index] );
		    exit (1);
		}
		n <<= 10;
	    }

	    if ( c != 0 || n == 0 )
	    {
		fprintf ( stderr,
			  "Bad number: %s\n",
			  argv[index] );
		exit (1);
	    }

	    * option = n;
        }

	++ index;
    }

    /* If the program name is not left, print doc. */

    if ( index >= argc  || argv[index][0] == '-' ) {
        printf ( "%s", documentation );
	exit ( 1 );
    }

    /* If -watch, start parent */

    if ( watch )
    {
	pid_t child = fork ();

	if ( child < 0 )
	    errno_exit ( "fork" );

	if ( child != 0 )
	{
	    int status;

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
			  "Terminated with signal: %s\n",
			  sys_siglist [ sig ] );
	    }

	    exit ( 0 );
	}
    }

    if ( geteuid() == 0 ) {

        /* Execute if effective user is root. */

	gid_t groups [1];

	/* Cleat the supplementary groups. */

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
	        fprintf ( stderr, "Could not find `sandbox'"
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
	limit.rlim_max = cputime;
        if ( setrlimit ( RLIMIT_CPU, & limit ) < 0 )
	    errno_exit
	        ( "setrlimit RLIMIT_CPU" );

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

	limit.rlim_cur = processes;
	limit.rlim_max = processes;
        if ( setrlimit ( RLIMIT_NPROC, & limit ) < 0 )
	    errno_exit
	        ( "setrlimit RLIMIT_NPROC" );
    }

    /* Execute program with argument and `SANDBOX'
       as the only environment.
    */

    execve ( argv[index], argv + index, env );

    /* If execve fails, print error messages. */

    fprintf ( stderr, "ERROR: could not execute %s\n",
		      argv[index] );
    errno_exit ( "execve" );
}
