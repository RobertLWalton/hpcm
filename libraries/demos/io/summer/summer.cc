// Solution to the Summation Checking Problem
//
// File:	summer.c
// Authors:	Bob Walton (walton@seas.harvard.edu)
// Date:	Thu Jan 17 02:45:49 EST 2013
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: walton $
//   $Date: 2013/01/17 07:46:39 $
//   $RCSfile: summer.cc,v $
//   $Revision: 1.3 $

#include <iostream>
#include <iomanip>
#include <cstdio>
using namespace std;
#include <cassert>
#include <cmath>	// fabs

int debug = false;
#define dout    if ( debug ) cout
#define dprintf if ( debug ) printf

char line[81];

int main ( int argc, char * argv[] )
{
    debug = ( argc > 1 );

    while ( cin.getline ( line, sizeof ( line ) ),
            ! cin.eof() )
    {
	// Print test case name.
	//
	cout << line << endl;

	// Read input and compute corrected sum.
	//
	double corrected_sum = 0, number, sum;

	// Read and sum numbers until `='.
	//
	while ( cin >> number, ! cin.fail() )
	    corrected_sum += number;
	cin.clear();
	    // Remember that fail must be cleared to
	    // continue reading.

	// Skip `='.  The last >> above failed to read
	// a number, BUT, it did skip over the white-
	// space preceding the `=', so we do not need
	// to skip whitespace here.
	//
	char c;
	cin >> c;
	assert( c == '=' );

	// Read accountant computer's sum.
	//
	cin >> sum;
	assert ( ! cin.fail() );

	// Read end of line after sum.
	//
	cin.getline ( line, sizeof ( line ) );
	assert ( line[0] == 0 );

	// Corrected_sum and sum are both approximations
	// to numbers that are exact multiples of 0.01.
	// So if they differ, they should differ by at
	// least 0.01, approximately, and certainly by
	// more than 0.005, and if they are equal, they
	// should differ by much, much less than 0.01,
	// and most certainly by less than 0.005.
	//

	// If debugging, look at corrected_sum and sum
	// at maximum precision.
	//
	dprintf
	    ( "SUM = %.16f, CORRECTED SUM = %.16f\n",
	      sum, corrected_sum );

	// Print output.
	//
	if ( fabs ( sum - corrected_sum ) < 0.005 )
	    printf ( "%.2f is correct\n", sum );
	else
	    printf ( "%.2f should be %.2f\n",
	             sum, corrected_sum );
    }

    return 0;
}
