// Solution to the Summation Checking Problem
//
// File:	summer.java
// Authors:	Bob Walton (walton@seas.harvard.edu)
// Date:	Mon Jan  7 13:03:39 EST 2013
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.

import java.util.Scanner;
import java.text.Format;
public class summer {

    static boolean debug = false;

    static void printf
            ( String format, Object ... args )
    {
        System.out.format ( format, args );
    }

    static void dprintf
            ( String format, Object ... args )
    {
        if ( debug )
	    System.out.format ( format, args );
    }

    public static void main ( String[] args )
    {
	debug = ( args.length > 0 );

	Scanner scan = new Scanner ( System.in );

	while ( scan.hasNextLine() )
	{
	    String casename = scan.nextLine();
	    System.out.println ( casename );

	    double corrected_sum = 0;

	    while ( scan.hasNextDouble() )
	        corrected_sum += scan.nextDouble();

	    String equals = scan.next();
	    assert ( equals.equals ( "=" ) );

	    double sum  = scan.nextDouble();

	    scan.nextLine();

	    // If debugging, look at corrected_sum and
	    // sum at maximum precision.
	    //
	    dprintf
		( "SUM = %.16f, CORRECTED SUM = %.16f\n",
		  sum, corrected_sum );

	    /* Print output.
	     */
	    if (   Math.abs ( sum - corrected_sum )
	         < 0.005 )
		printf ( "%.2f is correct\n", sum );
	    else
		printf ( "%.2f should be %.2f\n",
			 sum, corrected_sum );
	    }
    }
}
