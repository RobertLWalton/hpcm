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

    static execute_assign ( String variable )
    {
        check_variable ( variable );
        String op = get_token();

	Value v1 = null;

	if ( op.equals ( "-" ) )
	{
	    v1 = get_value();
	    if ( is_scalar ( v1 ) )
	        v1.s = - v1.s;
	    else
	    {
	        v1.x = - v1.x;
		v1.y = - v1.y;
	    }
	}
	else if ( op.equals ( "|" ) )
	{
	    v1 = get_value();
	    require_scalar ( v1 );
	    skip ( "|" );
	    v1.s = Math.abs ( v1.s );
	}
	else if ( op.equals ( "||" ) )
	{
	    v1 = get_value();
	    require_vector ( v1 );
	    skip ( "||" );
	    double dx = v1.x - v2.x;
	    double dy = v1.y - v2.y;
	    v1.s = Math.sqrt ( dx*dx + dy*dy );
	    v1.type = SCALAR;
	}
	else if ( op.equals ( "angle" ) )
	{
	    v1 = get_value();
	    require_vector ( v1 );
	    double dx = v1.x - v2.x;
	    double dy = v1.y - v2.y;
	    v1.s = Math.atan ( dy, dx );
	    v1.s *= 180.0 / Math.PI;
	    v1.type = SCALAR;
	}
	else if ( op.equals ( "!" ) )
	{
	    v1 = get_value();
	    require_boolean ( v1 );
	    v1.b = ! v1.b;
	}
	else
	{
	    backup = true;
	    v1 = get_value();
	    op = get_token();

	    if ( op.equals ( "+" ) )
	    {
		Value v2 = get_value();
		if ( is_scalar ( v1, v2 ) )
		    v1.s += v2.s;
		else
		{
		    v1.x += v2.x;
		    v1.y += v2.y;
		}
	    }
	    else if ( op.equals ( "-" ) )
	    {
		Value v2 = get_value();
		if ( is_scalar ( v1, v2 ) )
		    v1.s -= v2.s;
		else
		{
		    v1.x -= v2.x;
		    v1.y -= v2.y;
		}
	    }
	    else if ( op.equals ( "*" ) )
	    {
		Value v2 = get_value();
		require_scalar ( v1 );
		if ( is_scalar ( v2 ) )
		    v1.s *= v2.s;
		else
		{
		    v1.x = v1.s * v2.x;
		    v1.y = v1.s * v2.y;
		    v1.type = VECTOR;
		}
	    }
	    else if ( op.equals ( "/" ) )
	    {
		Value v2 = get_value();
		require_scalar ( v1, v2 );
		if ( v2.s == 0 )
		    error ( "zero divisor" );
		v1.s /= v2.s;
	    }
	    else if ( op.equals ( "^" ) )
	    {
		Value v2 = get_value();
		require_vector ( v1 );
		require_scalar ( v2 );
		double angle = v2.s * Math.PI / 180;
		double sin = Math.sin ( angle );
		double cos = Math.cos ( angle );
		double x = cos * v1.x - sin * v1.y;
		double y = sin * v1.x + cos * v1.y;
		v1.x = x;
		v1.y = y;
	    }
	    else if ( op.equals ( "&&" ) )
	    {
		Value v2 = get_value();
		require_boolean ( v1, v2 );
		v1.b = v1.b && v2.b;
	    }
	    else if ( op.equals ( "||" ) )
	    {
		Value v2 = get_value();
		require_boolean ( v1, v2 );
		v1.b = v1.b || v2.b;
	    }
	    else if ( op.equals ( "==" ) )
	    {
		Value v2 = get_value();
		require_scalar ( v1, v2 );
		v1.b = ( v1.s == v2.s );
		v1.type = BOOLEAN;
	    }
	    else if ( op.equals ( "!=" ) )
	    {
		Value v2 = get_value();
		require_scalar ( v1, v2 );
		v1.b = ( v1.s != v2.s );
		v1.type = BOOLEAN;
	    }
	    else if ( op.equals ( "<" ) )
	    {
		Value v2 = get_value();
		require_scalar ( v1, v2 );
		v1.b = ( v1.s < v2.s );
		v1.type = BOOLEAN;
	    }
	    else if ( op.equals ( "<=" ) )
	    {
		Value v2 = get_value();
		require_scalar ( v1, v2 );
		v1.b = ( v1.s <= v2.s );
		v1.type = BOOLEAN;
	    }
	    else if ( op.equals ( ">" ) )
	    {
		Value v2 = get_value();
		require_scalar ( v1, v2 );
		v1.b = ( v1.s > v2.s );
		v1.type = BOOLEAN;
	    }
	    else if ( op.equals ( ">=" ) )
	    {
		Value v2 = get_value();
		require_scalar ( v1, v2 );
		v1.b = ( v1.s >= v2.s );
		v1.type = BOOLEAN;
	    }
	    else if ( ! op.equals ( "\n" ) )
	        error ( "`" + op "' unrecognized" );
	}

	skip ( "\n" );

	variable_table.put ( variable, v1 );
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

