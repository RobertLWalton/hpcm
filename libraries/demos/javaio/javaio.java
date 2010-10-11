// JAVA IO Demo
//
// File:     javaio.java
// Author:   Bob Walton <walton@deas.harvard.edu>
// Date:     Mon Oct 11 14:44:21 EDT 2010
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: walton $
//   $Date: 2010/10/11 23:49:58 $
//   $RCSfile: javaio.java,v $
//   $Revision: 1.5 $

// This program is suitable for use as a template for
// ACM programming contest submissions, and provides
// input/output functions that are likely to be useful
// in such contests.
//
// You may copy and modify this code without
// any restriction.
//
// This program is new as of 10/2010, and may well
// contain bugs.  No warranty is made: use at your own
// risk.

import java.io.*;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Locale;

public class javaio {
    // The class name `javaio' must be changed if this
    // is used as a template for a different problem.

    // Set up the StreamTokenizer.
    //
    public static Reader reader
	= new BufferedReader
	      ( new InputStreamReader
		    ( System.in ) );
    public static StreamTokenizer in
	= new StreamTokenizer ( reader );

    // Read the remainder of the line.  Null is returned
    // on an end of file.
    //
    public static String nextLine() throws IOException
    {
        in.resetSyntax();
	in.wordChars ( 0x00, 0xFF );
	in.whitespaceChars ( '\n', '\n' );
	in.eolIsSignificant ( true );
	in.nextToken();
	switch ( in.ttype )
	{
	case StreamTokenizer.TT_EOF:
	    return null;
	case StreamTokenizer.TT_EOL:
	    return "";
	case StreamTokenizer.TT_WORD:
	    {
		String result = in.sval;
		in.nextToken();
		switch ( in.ttype )
		{
		case StreamTokenizer.TT_EOF:
		    return result;
		case StreamTokenizer.TT_EOL:
		    return result;
		}
		throw new IOException
		   ( "readLine line ended with "
		     + in.toString() );
	    }
	}
	throw new IOException
	   ( "readLine encountered "
	     + in.toString() );
    }

    // Read the next number, skipping lines.
    // IOException is thrown if there is no number.
    // This is an EOFException if an EOF is encountered.
    //
    public static double nextDouble() throws IOException
    {
        in.resetSyntax();
	in.whitespaceChars ( 0x00, ' ' );
	in.eolIsSignificant ( false );
	in.parseNumbers();
	in.nextToken();
	switch ( in.ttype )
	{
	case StreamTokenizer.TT_EOF:
	    throw new EOFException
		( "nextNumber encountered EOF" );
	case StreamTokenizer.TT_NUMBER:
	    return in.nval;
	}
	throw new IOException
	    ( "nextNumber found "
	      + in.toString() );
    }

    // Ditto but insist the number be an int.
    //
    public static int nextInt() throws IOException
    {
	double v = nextDouble();
	int i = (int) v;
	if ( v != i )
	    throw new IOException
	        ( "nextInt found " + v );
        return i;
    }

    // Ditto but insist the number be a long.
    //
    public static long nextLong() throws IOException
    {
	double v = nextDouble();
	long i = (long) v;
	if ( v != i )
	    throw new IOException
	        ( "nextInt found " + v );
        return i;
    }

    // Read the next non-whitespace character, skipping
    // lines.  IOException is thrown if there is no such
    // character.  This is an EOFException if an EOF is
    // encountered.
    //
    public static char nextChar() throws IOException
    {
        in.resetSyntax();
	in.whitespaceChars ( 0x00, ' ' );
	in.eolIsSignificant ( false );
	in.nextToken();
	if ( in.ttype == StreamTokenizer.TT_EOF )
	    throw new EOFException
		( "nextChar encountered EOF" );
	return (char) in.ttype;
    }

    // Read the next string of non-whitespace charac-
    // ters, skipping lines.  IOException is thrown if
    // there is no such string.  This is an EOFException
    // if an EOF is encountered.
    //
    public static String nextString() throws IOException
    {
        in.resetSyntax();
	in.whitespaceChars ( 0x00, ' ' );
	in.wordChars ( '!', 0xFF );
	in.eolIsSignificant ( false );
	in.nextToken();
	switch ( in.ttype )
	{
	case StreamTokenizer.TT_EOF:
	    throw new EOFException
		( "nextString encountered EOF" );
	case StreamTokenizer.TT_WORD:
	    return in.sval;
	}
	throw new IOException
	    ( "nextString found "
	      + in.toString() );
    }

    // Execute System.out.print etc.  There functions
    // allow you to `import javaio.*' and avoid
    // 
    //
    public static void print ( String s )
    {
        System.out.print ( s );
    }
    public static void println ( String s )
    {
        System.out.println ( s );
    }
    public static void println()
    {
        System.out.println();
    }
    // Print right adjusted in field.
    //
    public static void printRight
	    ( String s, int width )
    {
        for ( width = width - s.length();
	      width > 0; -- width )
	    System.out.print ( " " );
	System.out.print ( s );
    }
    // Print left adjusted in field.
    //
    public static void printLeft ( String s, int width )
    {
	System.out.print ( s );
        for ( width = width - s.length();
	      width > 0; -- width )
	    System.out.print ( " " );
    }

    // Set up number formatter.  Note that it is
    // important in ACM programming contests to
    // insist on an ENGLISH formatter.
    //
    // Also, do NOT put commas in the output.
    //
    static DecimalFormat formatter = (DecimalFormat)
	NumberFormat.getInstance ( Locale.ENGLISH );

    // Set the decimal number output pattern.  This has
    // the form "###0.000" where if there are I #'s and
    // F after-decimal 0's then there will be from
    // 1 to I+1 integer digits and F fractional digits.
    // 
    public static void applyPattern ( String pattern )
    {
	formatter.applyPattern ( pattern );
    }

    // Print double using the set pattern.
    //
    public static void printDouble ( double v )
    {
        print ( formatter.format ( v ) );
    }

    // Ditto but right adjust.
    //
    public static void printDouble
	    ( double v, int width )
    {
        printRight ( formatter.format ( v ), width );
    }


    public static void main (String[] args)
	    throws IOException {

	// Each test case has the format:
	//
	//	<test-case-name-line>
	//	<test-line>*
	//	.
	//
	// where each <test-line> is free format of the
	// form
	//
	//	<character> <string> <N> <number> ...
	//
	// and there are <N> numbers.
	//
	// The input is pretty printed.

	applyPattern ( "###0.00" );

	for ( String test_case_name = nextLine();
	      test_case_name != null;
	      test_case_name = nextLine() )
	{
	    println ( test_case_name );

	    while ( true )
	    {
	        char c = nextChar();
		if ( c == '.' )
		{
		    println ( c + nextLine() );
		    break;
		}
		String s = nextString();
		int N = nextInt();
		println ( c + " " + s + " " + N + ":" );
		for ( int i = 0; i < N; ++ i )
		{
		    if ( i % 5 == 0 && i != 0 )
		         println ( "" );
		    printDouble ( nextDouble(), 8 );
		}
		if ( N > 0 ) println ( "" );
		nextLine();
	    }
	}
    }
}
