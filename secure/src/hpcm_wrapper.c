/* Programming Contest Wrapper Program 
 *
 * File:	hpcm_wrapper.c
 * Authors:	Bob Walton (walton@deas.harvard.edu)
 * Date:	Fri Oct 13 01:07:34 EDT 2006
 *
 * The authors have placed this program in the public
 * domain; they make no warranty and accept no liability
 * for this program.
 *
 * RCS Info (may not be true date or author):
 *
 *   $Author: walton $
 *   $Date: 2006/10/13 05:51:56 $
 *   $RCSfile: hpcm_wrapper.c,v $
 *   $Revision: 1.1 $
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

char documentation [] =
"/<directory>/<program>\n"
"\n"
"    This program checks that it has been called\n"
"    no arguments and a program name (argv[0]) begin-\n"
"    ning with `/' and does not end with `/'.  Then\n"
"    the program sets the real UID and GID to the\n"
"    effective UID and GID and execs:\n"
"\n"
"        /<directory>/secure/<program>\n"
"\n"
"    It is intended that this program be set UID,\n"
"    and thus serve as a wrapper for the program it\n"
"    execs, which is a shell script.\n" ;

#define NAME_SIZE 4000

/* Program name is argv[0] and has maximum allowed
 * size equal to NAME_SIZE.  exec_name is name of
 * program to be exec'ed.
 */
const char * program_name;
char exec_name[NAME_SIZE+20];

void errno_exit ( char * m )
{
    fprintf ( stderr, "%s: system call error:"
                      " %s:\n    %s\n",
		      program_name, m,
		      strerror ( errno ) );
    exit ( 1 );
}

/* Main program.
*/
int main ( int argc, char ** argv )
{
    char * p, * q;
    const char * r;
    uid_t euid;
    gid_t egid;

    if ( argc > 1 )
    {
        printf ( "%s", documentation );
	exit ( 1 );
    }
    else if ( argc != 1 )
    {
        fprintf ( stderr, "program name missing\n" );
	exit ( 1 );
    }

    program_name = argv[0];

    if ( strlen ( program_name ) > NAME_SIZE )
    {
        fprintf ( stderr, "%s: program name too long\n",
	                  program_name);
	exit ( 1 );
    }
    else if ( program_name[0] != '/' )
    {
        fprintf ( stderr, "%s: program name does not"
	                  " begin with /\n",
			  program_name );
	exit ( 1 );
    }

    strcpy ( exec_name, program_name );
    p = exec_name;
    q = NULL;
    while ( * p )
    {
        if ( * p ++ == '/' ) q = p;
    }
    if ( * q == 0 )
    {
        fprintf ( stderr, "%s: program name ends with"
	                  " /\n",
			  program_name );
	exit ( 1 );
    }
    strcpy ( q, "secure/" );
    r = program_name + ( q - exec_name );
    q += strlen ( q );
    strcpy ( q, r );


    /* Set the real user and group IDs to to effective
       user and group IDs.
    */

    euid = geteuid ();
    egid = getegid ();

    if ( setreuid ( euid, -1 ) < 0 )
        errno_exit ( "setreuid setting real uid" );
    if ( setregid ( egid, -1 ) < 0 )
        errno_exit ( "setregid setting real gid" );

    execl ( exec_name, exec_name, NULL );

    /* If execl fails, print error messages. */

    fprintf ( stderr, "%s: could not: execute %s\n",
		      program_name, exec_name );
    errno_exit ( "execl" );
}
