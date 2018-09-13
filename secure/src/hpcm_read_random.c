/* Programming Contest Dev/Random Reader
 *
 * File:	hpcm_read_random.c
 * Authors:	Bob Walton (walton@deas.harvard.edu)
 * Date:	Thu Sep 13 05:17:21 EDT 2018
 *
 * The authors have placed this program in the public
 * domain; they make no warranty and accept no liability
 * for this program.
 */

#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>

char documentation [] =
"hpcm_read_random\n"
"\n"
"    This program reads 32 bits from /dev/random and\n"
"    outputs the result as 2 ASCII short 16-bit\n"
"    unsigned integers.  This program exists because\n"
"    for some reason reading /dev/random directly\n"
"    from tclsh hangs strangely on some systems.\n"
;

void errno_exit ( char * m )
{
    fprintf ( stderr,
              "hpcm_read_random: system call error:\n"
	      "    %s:\n    %s\n",
	      m, strerror ( errno ) );
    exit ( 1 );
}

/* Main program.
*/
int main ( int argc, char ** argv )
{
    int fd = open ( "/dev/random", O_RDONLY );
    if ( fd < 0 )
        errno_exit ( "opening /dev/random" );
    unsigned short buffer[2];
    if ( read ( fd, buffer, 4 ) < 0 )
        errno_exit ( "reading /dev/random" );
    printf ( "%u %u\n", buffer[0], buffer[1] );
    close ( fd );
    return 0;
}
