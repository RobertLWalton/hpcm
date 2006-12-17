/* Programming Contest Wrapper Program 
 *
 * File:	hpcm_wrapper.c
 * Authors:	Bob Walton (walton@deas.harvard.edu)
 * Date:	Fri Dec 15 12:36:22 EST 2006
 *
 * The authors have placed this program in the public
 * domain; they make no warranty and accept no liability
 * for this program.
 *
 * RCS Info (may not be true date or author):
 *
 *   $Author: walton $
 *   $Date: 2006/12/17 17:37:34 $
 *   $RCSfile: hpcm_wrapper.c,v $
 *   $Revision: 1.3 $
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

char documentation [] =
"/<directory>/<program>\n"
"\n"
"    This program checks that it has been called with\n"
"    no arguments and a program name (argv[0]) that\n"
"    does not end with `/'.  Then the program sets\n"
"    the real UID and GID to the effective UID and\n"
"    GID and execs:\n"
"\n"
"        <directory>/secure/bin/<program>\n"
"\n"
"    where the program name (argv[0]) has the form\n"
"\n"
"        <directory>/<program>\n"
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
char exec_name[NAME_SIZE+40];

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

    strcpy ( exec_name, program_name );
    p = exec_name;
    q = exec_name;
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
    strcpy ( q, "secure/bin/" );
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
