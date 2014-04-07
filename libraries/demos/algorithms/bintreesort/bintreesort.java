// Solution to the Binary Tree Sort Problem
//
// File:	bintreesort.java
// Authors:	Bob Walton (walton@seas.harvard.edu)
// Date:	Sun Apr  6 20:52:22 EDT 2014
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.

import java.util.Scanner;
import java.util.treemap;
public class bintreesort {

    static boolean debug = false;

    // printf ( format, ... ) prints output using the
    // given format with ... representing the format
    // controlled arguments.
    //
    static void printf
            ( String format, Object ... args )
    {
        System.out.format ( format, args );
    }

    // Ditto but suppress printout if debug == false.
    //
    static void dprintf
            ( String format, Object ... args )
    {
        if ( debug )
	    System.out.format ( format, args );
    }

    // JAVA Treemap implements a red-black binary tree.
    // Our dataset is such a tree.  The standard
    // comparison function suffices for comparison of
    // keys, which are `doubles'.  The data associated
    // with a new is not relevant, but must have a type
    // that extends Object, so we use `Object()' data.
    //
    static Treemap<Double,Object> dataset;

    // For debugging we keep track of the number of
    // elements in the data set.
    //
    static int setsize = 0;

    // Perform the `A n' operation to add n to the
    // data set.
    //
    void add ( double n )
    {
        if ( dataset.get ( Double ( n ) ) != null )
	    dprintf ( "A %.0f failed%n", n );
	else
	{
	    dprintf ( "A %.0f succeeded," +
	              " data set size = %d%n",
		      n, ++ setsize );
	    dataset.put ( Double ( n ), Object() );
	}
    }

    // Perform the `R n' operation to remove n from the
    // data set.
    //
    void remove ( double n )
    {
        if ( dataset.get ( Double ( n ) ) == null )
	    dprintf ( "R %.0f failed%n", n );
	else
	{
	    dataset.remove ( Double ( n ) );
	    dprintf ( "R %.0f succeeded," +
	              " data set size = %d%n",
		      n, -- setsize );
	}
    }

    // Perform the `P n' operation to print n in the
    // data set.
    //
    void print ( double n )
    {
        if ( dataset.get ( Double ( n ) ) == null )
	    printf ( "(%.0f)", n );
	else
	{
	    Double previous =
	        dataset.lowerkey ( Double ( n ) );
	    if ( previous == null )
	        printf ( "(null)" );
	    else
	        printf ("%.0f",
		        previous.doubleValue() );

	    printf (" < %.0f < ", n );

	    Double next =
	        dataset.higherkey ( Double ( n ) );
	    if ( next == null )
	        printf ( "(null)" );
	    else
	        printf ("%.0f",
		        next.doubleValue() );
	}
	printf ( "%n" );
    }

    // Perform the `E' operation to empty the data set.
    //
    void empty ( void )
    {
        Double first;
	while ( first = datset.firstkey() )
	{
	    dprintf ( "E removes %.0f," +
	              " data set size = %d%n",
		      first.doubleValue(),
		      -- setsize );
	    dataset.remove ( first );
	}
    }

    TBD

    public static void main ( String[] args )
    {
	debug = ( args.length > 0 );

	Scanner scan = new Scanner ( System.in );

	while ( scan.hasNextLine() )
	{
	    String casename = scan.nextLine();
	    System.out.println ( casename );

	    // As long as there are numbers in the
	    // input, read these numbers and compute
	    // their sum.  Note we assume each number
	    // ends just before whitespace.
	    //
	    double corrected_sum = 0;
	    while ( scan.hasNextDouble() )
	        corrected_sum += scan.nextDouble();

	    // Read and check =-sign, which must be
	    // surrounded by whitespace.
	    //
	    assert ( scan.next().equals ( "=" ) );

	    // Read accountant computed sum and the
	    // end of line which follows it.
	    //
	    double sum  = scan.nextDouble();
	    scan.nextLine();

	    // If debugging, look at corrected_sum and
	    // sum at maximum precision.
	    //
	    dprintf
	      ( "SUM = %.16f, CORRECTED SUM = %.16f%n",
		sum, corrected_sum );

	    // Print output.  Errors less than 0.005
	    // must be due to internal rounding and not
	    // to the accountants.
	    //
	    if (   Math.abs ( sum - corrected_sum )
	         < 0.005 )
		printf ( "%.2f is correct%n", sum );
	    else
		printf ( "%.2f should be %.2f%n",
			 sum, corrected_sum );
	    }
    }
}

