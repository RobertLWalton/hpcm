/* Solution to the Summation Checking Problem
 *
 * File:	summer.c
 * Authors:	Bob Walton (walton@deas.harvard.edu)
 * Date:	Sun Nov 25 08:19:51 EST 2012
 *
 * The authors have placed this program in the public
 * domain; they make no warranty and accept no liability
 * for this program.
 *
 * RCS Info (may not be true date or author):
 *
 *   $Author: walton $
 *   $Date: 2012/11/25 13:32:45 $
 *   $RCSfile: summer.c,v $
 *   $Revision: 1.1 $
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
	scanf ( " =" );  /* Skip `='. */
	assert ( scanf ( "%lf", & sum ) == 1 );
	/* Read end of line after sum. */
	fgets ( line, sizeof ( line ), stdin );

	/* corrected_sum and sum are both approximations
	 * to numbers that are exact multiples of 0.01.
	 * So if they differ, they whould differ by at
	 * least 0.01, approximately, and if they are
	 * equal, they should differ by much, much less.
	 */

	if ( fabs ( sum - corrected_sum ) < 0.005 )
	    printf ( "%.2f is correct\n", sum );
	else
	    printf ( "%.2f should be %.2f\n",
	             sum, corrected_sum );
    }

    return 0;
}
