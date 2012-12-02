/* Solution to the Summation Checking Problem
 *
 * File:	summer.c
 * Authors:	Bob Walton (walton@seas.harvard.edu)
 * Date:	Sat Dec  1 22:35:26 EST 2012
 *
 * The authors have placed this program in the public
 * domain; they make no warranty and accept no liability
 * for this program.
 *
 * RCS Info (may not be true date or author):
 *
 *   $Author: walton $
 *   $Date: 2012/12/02 03:43:24 $
 *   $RCSfile: summer.c,v $
 *   $Revision: 1.4 $
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
	/* Print test case name.
	 */
        printf ( "%s", line );

	/* Read input and compute corrected sum.
	 */
	double corrected_sum = 0, number, sum;

	/* Read and sum numbers until `='.
	 */
	while ( scanf ( "%lf", & number ) == 1 )
	    corrected_sum += number;

	/* Skip `='.  The last scanf above failed to
	 * read a number, BUT, it did skip over the
	 * whitespace preceding the `=', so we do
	 * not need to skip whitespace here.
	 */
	scanf ( "=" );

	/* Read accountant computer's sum.
	 */
	assert ( scanf ( "%lf", & sum ) == 1 );

	/* Read end of line after sum.
	 */
	fgets ( line, sizeof ( line ), stdin );
	assert ( line[0] == '\n' );

	/* Corrected_sum and sum are both approximations
	 * to numbers that are exact multiples of 0.01.
	 * So if they differ, they should differ by at
	 * least 0.01, approximately, and certainly by
	 * more than 0.005, and if they are equal, they
	 * should differ by much, much less than 0.01,
	 * and most certainly by less than 0.005.
	 */

	/* If debugging, look at corrected_sum and sum
	 * at maximum precision.
	 */
	dprintf
	    ( "SUM = %.15f, CORRECTED SUM = %.15f\n",
	      sum, corrected_sum );

	/* Print output.
	 */
	if ( fabs ( sum - corrected_sum ) < 0.005 )
	    printf ( "%.2f is correct\n", sum );
	else
	    printf ( "%.2f should be %.2f\n",
	             sum, corrected_sum );
    }

    return 0;
}
