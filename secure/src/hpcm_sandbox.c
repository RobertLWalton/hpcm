// Programming Contest Sandbox Program 
//
// File:	sandbox.c
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Tue Aug 15 21:20:44 EDT 2000
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: acm-cont $
//   $Date: 2000/08/26 13:25:12 $
//   $RCSfile: hpcm_sandbox.c,v $
//   $Revision: 1.1 $

#include <unistd.h>
#include <sys/types.h>
#include <errno.h>

char documentation [] =
"sandbox program argument ...\n"
"\n"
"    Sets the real user and group IDs to the effect-\n"
"    ive user and group IDs, sets the environment to\n"
"    contain just \"SANDBOX\", and executes the prog-\n"
"    gram with the given arguments.  Normally the\n"
"    binary of this `sandbox' program has `sandbox'\n"
"    for both owner and group, and has user and group\n"
"    set ID mode flags, so the program gets run with\n"
"    its real and effective user and group IDs set to\n"
"    `sandbox'.  Normally the `sandbox' user is not\n"
"    allowed to log in and so owns no files or\n"
"    directories.\n"
"\n"
"    If the operating system is such that a non-\n"
"    privileged user cannot set the real user and\n"
"    group IDs to the effective user and group IDs,\n"
"    the real IDs will not be set.  But if in addi-\n"
"    tion the non-priviledged user can set the\n"
"    effective user and group IDs to the real IDs,\n"
"    this `sandbox' program will return an error\n"
"    and not execute the given program.\n" ;

char * env [] = { "SANDBOX", 0 };

int output ( int fd, char * string ) {
    return write ( fd, string, strlen (string) );
}

// Main program.
//
int main ( int argc, char ** argv )
{
    uid_t uid;
    gid_t gid;

    if ( argc == 0 ) {
        output ( 1, documentation );
	exit ( 1 );
    }

    uid = geteuid ();
    gid = getegid ();

    setreuid ( uid, -1 );
    setregid ( gid, -1 );

    setreuid ( -1, getuid() );
    setregid ( -1, getgid() );

    if ( uid != geteuid() || gid != getegid () )
    {
        output ( 2, "ERROR: sandbox failed to securely"
	            " set user and group IDs\n" );
	exit ( 1 );
    }
     
    execve ( argv[1], argv + 1, env );

    output ( 2, "ERROR: could not execute " );
    output ( 2, argv[1] );
    output ( 2, "\n" );

    exit ( errno );
}
