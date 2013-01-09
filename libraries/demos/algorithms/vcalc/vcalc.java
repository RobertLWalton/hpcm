// Solution to the Simple Vector Calculator Problem
//
// File:	vcalc.java
// Authors:	Bob Walton (walton@seas.harvard.edu)
// Date:	Wed Jan  9 03:47:31 EST 2013
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.

import java.util.Scanner;
import java.util.regex.Pattern;
import java.util.HashTable;
public class vcalc {

    static boolean debug = false;

    final static int BOOLEAN  = 1;
    final static int SCALAR   = 2;
    final static int VECTOR   = 3;

    final static Scanner scan =
        new Scanner ( System.in )
	        .useDelimiter ( "[(),: \t\f\n]+" );

    // Returns next token or null if end of file.
    // Implements one-token backup.  Implements
    // line_number.
    //
    static String last_token = new String ( "\n" );
    static boolean backup = false;
    static line_number = 0;
    static Pattern comment =
        Pattern.compile ( "\\G[ \t\f]*//[^\n]*\n" );
    static Pattern separator =
        Pattern.compile ( "\\G[ \t\f]*([(),:\n])" );
    //
    String get_token ( void )
    {
        if ( backup )
	{
	    backup = false;
	    return last_token;
	}
	if ( last_token.equals ( "\n" ) )
	{
	    while ( true )
	    {
		++ line_number;
		if (    scan.findWithinHorizon
		           ( comment, Integer.MAX_VALUE )
		     == null )
		    break;
	    }
	}

        last_token =
	    scan.findWithinHorizon
	       ( separator, Integer.MAX_VALUE );
	if ( last_token != null )
	    last_token = scan.match().group ( 1 );
	else if ( scan.hasNext() )
	    last_token = scan.next();

	return last_token;
    }

    // Print error message and exit.
    //
    static void error ( String message )
    {
        System.out.println
	    ( "ERROR in line " + line_number + ":" );
        System.out.println
	    ( "      " + message );
	System.exit ( 1 );
    }

    static boolean is_variable ( String token )
    {
	return ( token.size() > 0
	         &&
		 Character.isLetter
		     ( token.charAt ( 0 ) ) );
    }

    static void check_variable ( String token )
    {
       if ( ! is_variable ( token ) )
          error ( "`" + token "' is not a variable" );
    }

    public class Value {
        int type;
	boolean b;	// Value if BOOLEAN.
	double s;	// Value if SCALAR.
	double x, y;	// Coordinates if VECTOR.

	public Value ( boolean bvalue )
	{
	    type = BOOLEAN;
	    b = bvalue;
	}
	public Value ( double svalue )
	{
	    type = SCALAR;
	    s = svalue;
	}
	public Value ( double xvalue, double yvalue )
	{
	    type = VECTOR;
	    x = xvalue;
	    y = yvalue;
	}
    }

    static HashTable variable_table = new HashTable;
        // Maps variable name Strings to Values.

    static execute_clear ( void )
    {
        int found = false;
	while ( true )
	{
	    String token = get_token();
	    if ( token.equals ( "\n" ) ) break;
	    check_variable ( token );
	    variable_table.remove ( token );
	    found = true;
	}
	if ( ! found )
	    variable_table.clear();
    }

    static execute_print ( void )
    {
        int first = true;
	while ( true )
	{
	    String token = get_token();
	    if ( token.equals ( "\n" ) ) break;
	    if ( first )
	    {
	        System.out.print ( " " );
		first = false;
	    }
	    Value v = (Value)
	        variable_table.get ( token );

	    if ( v == null )
	        System.out.print ( token );
	    else if ( v.type == BOOLEAN )
	        System.out.print
		   ( v.b ? "true" : "false" );
	    else if ( v.type == SCALAR )
	        System.out.print ( v.s );
	    else if ( v.type == VECTOR )
	        System.out.print
		    ( "(" + v.x + ", " + v.y + ")" );

	}
	System.out.println();
    }


    public static void main ( String[] args )
    {
	debug = ( args.length > 0 );

	// Loop to read and execute a statement.
	//
	while ( true )
	{
	    String token = get_token();
	    if ( token == null ) break;
	    else if ( token.equals ( "print" ) )
	        execute_print();
	}
    }
}

