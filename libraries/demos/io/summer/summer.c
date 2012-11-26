/* Solution to the Summation Checking Problem
 *
 * File:	summer.c
 * Authors:	Bob Walton (walton@seas.harvard.edu)
 * Date:	Mon Nov 26 14:25:10 EST 2012
 *
 * The authors have placed this program in the public
 * domain; they make no warranty and accept no liability
 * for this program.
 *
 * RCS Info (may not be true date or author):
 *
 *   $Author: walton $
 *   $Date: 2012/11/26 19:25:42 $
 *   $RCSfile: summer.c,v $
 *   $Revision: 1.3 $
 */


#include <stdlib.h>
#include <stdio.h>
#include <math.h>	/* fabs */
#include <assert.h>

int debug;
#define dprintf if ( debug ) printf

char line[82];

int main ( int argc, char * argv[] )
{
    debug = ( argc > 1 );

    while ( fgets ( line, sizeof ( line ), stdin ) )
    {
	/* Print test case name. */

        printf ( "%s", line );

	/* Read input and compute corrected sum. */

	double corrected_sum = 0, number, sum;

	/* Read number until `='. */

	while ( scanf ( "%lf", & number ) == 1 )
	    corrected_sum += number;

	/* Skip `='.  The last scanf above failed to
	 * read a number, BUT, it did skip over the
	 * whitespace preceding the `=', so we do
	 * not need to skip it here.
	 */

	scanf ( "=" );

	/* Read accountant computer's sum. */

	assert ( scanf ( "%lf", & sum ) == 1 );

	/* Read end of line after sum. */

	fgets ( line, sizeof ( line ), stdin );

	/* corrected_sum and sum are both approximations
	 * to numbers that are exact multiples of 0.01.
	 * So if they differ, they whould differ by at
	 * least 0.01, approximately, and if they are
	 * equal, they should differ by much, much less.
	 */

	dprintf
	    ( "SUM = %.15f, CORRECTED SUM = %.15f\n",
	      sum, corrected_sum );

	if ( fabs ( sum - corrected_sum ) < 0.005 )
	    printf ( "%.2f is correct\n", sum );
	else
	    printf ( "%.2f should be %.2f\n",
	             sum, corrected_sum );
    }

    return 0;
}
