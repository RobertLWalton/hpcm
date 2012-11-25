/* Solution to the Word Order Reverser Problem
 *
 * File:	reverser.c
 * Authors:	Bob Walton (walton@deas.harvard.edu)
 * Date:	Sun Nov 25 06:46:46 EST 2012
 *
 * The authors have placed this program in the public
 * domain; they make no warranty and accept no liability
 * for this program.
 *
 * RCS Info (may not be true date or author):
 *
 *   $Author: walton $
 *   $Date: 2012/11/25 11:49:28 $
 *   $RCSfile: reverser.c,v $
 *   $Revision: 1.3 $
 */


#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <assert.h>

int debug;
#define dprintf if ( debug ) printf

char line[82];

/* Function to find and print the first word BEFORE q,
 * where q points into `line'.  It is a program error
 * if there is no such word.  Returns pointer to the
 * first character of the word printed.
 */
char * print_word ( char * q )
{
    /* Move q backward to point just after word.
     */
    while ( 1 )
    {
        assert ( q > line );
	if ( isalpha ( q[-1] ) ) break;
	-- q;
    }
    char * p = q;  /* p points just after word. */

    /* Move q to point at 1st character of word.
     */
    -- q;
    while ( q > line && isalpha ( q[-1] ) ) -- q;

    /* Print word.
     */
    char save = * p;
    * p = 0;
    printf ( "%s", q );
    * p = save;
    return q;
}

int main ( int argc, char * argv[] )
{
    debug = ( argc > 1 );

    while ( fgets ( line, sizeof ( line ), stdin ) )
    {
	/* When debugging print the intput as well as
	 * the output.
	 */
	dprintf ( "%s", line );

    	/* Set p to beginning of line and q to end of
	 * line.
	 */
	char * p = line, * q = line;
	while ( * q ) ++ q;

	/* Print line substituting for words.
	 */
	while ( * p )
	{
	    if ( isalpha ( * p ) )
	    {
	        while ( isalpha ( * p ) ) ++ p;
		q = print_word ( q );
	    }
	    else printf ( "%c", * p ++ );
	}
    }

    return 0;
}
