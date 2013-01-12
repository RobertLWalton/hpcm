// Solution to the Simple Vector Calculator Problem
//
// File:	vcalc.java
// Authors:	Bob Walton (walton@seas.harvard.edu)
// Date:	Sat Jan 12 02:58:18 EST 2013
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.

import java.util.Scanner;
import java.util.regex.Pattern;
import java.util.Hashtable;
import java.text.DecimalFormat;
public class vcalc {

    static boolean debug = false;

    static void print ( String s )
    {
        System.out.print ( s );
    }
    static void println ( String s )
    {
        System.out.println ( s );
    }
    static void println ( )
    {
        System.out.println();
    }
    static void dprint ( String s )
    {
        if ( debug ) System.out.print ( s );
    }
    static void dprintln ( String s )
    {
        if ( debug ) System.out.println ( s );
    }

    final static Scanner scan =
        new Scanner ( System.in )
	        .useDelimiter ( "[(),: \t\f\n]+" );

    // Returns next token or EOL if end of line or
    // EOF if end of file.  We make EOL and EOF be
    // printable strings as tokens are printed in
    // many error messages.  Implements one-token
    // backup.  Implements line_number.
    //
    // All tokens are printable Strings and none are of
    // zero length.
    //
    final static String EOL =
        new String ( "(END-OF-LINE)" );
    final static String EOF =
        new String ( "(END-OF-FILE)" );
    static boolean backup = false;
        // Set backup = true to backup.
    static int line_number = 0;
        // Current line number; 1, 2, 3, ...
    //
    static String last_token = EOL;
    static Pattern comment =
        Pattern.compile ( "\\G[ \t\f]*(//[^\n]*)?\n" );
    static Pattern separator =
        Pattern.compile ( "\\G[ \t\f]*([(),:\n])" );
    //
    static String get_token ( )
    {
        if ( backup )
	{
	    backup = false;
	    return last_token;
	}
	if ( last_token.equals ( EOL ) )
	{
	    while ( true )
	    {
		++ line_number;
		String c =
		    scan.findWithinHorizon
		        ( comment, 0 );
		if ( c == null ) break;
		dprint ( "COMMENT " + c );
	    }
	}

        last_token =
	    scan.findWithinHorizon ( separator, 0 );
	if ( last_token != null )
	{
	    last_token = scan.match().group ( 1 );
	    if ( last_token.equals ( "\n" ) )
		last_token = EOL;
	}
	else if ( scan.hasNext() )
	    last_token = scan.next();
	else
	    last_token = EOF;

	dprint ( "{" + last_token + "}" );
	return last_token;
    }

    // Print error message and exit.
    //
    static void error ( String message )
    {
        println
	    ( "ERROR in line " + line_number + ":" );
        println
	    ( "      " + message );
	System.exit ( 1 );
    }

    static Double token_to_number ( String token )
    {
        try { return Double.valueOf ( token ); }
	catch ( NumberFormatException e )
	    { return null; }
    }

    static boolean is_scalar ( String token )
    {
        return token_to_number ( token ) != null;
    }

    static double token_to_scalar ( String token )
    {
	Double d = token_to_number ( token );
	if ( d == null )
	    error ( "expected scalar and got `" +
	            token + "'" );

	return d.doubleValue();
    }

    static void check_not_eof ( String token )
    {
       if ( token == EOF )
          error ( "unexpected end of file" );
    }

    static void skip ( String desired )
    {
        String token = get_token();
	if ( ! token.equals ( desired ) )
	{
	    error ( "expected `" + desired +
	            "' but found `" + token + "'" );
	}
    }

    static boolean is_variable ( String token )
    {
	return ( Character.isLetter
		     ( token.charAt ( 0 ) ) );
    }

    static void check_variable ( String token )
    {
       if ( ! is_variable ( token ) )
          error ( "`" + token + "' is not a variable" );
    }

    // This is the only way to get a rough equivalent
    // of the C language %.15g which deletes trailing
    // fraction zeros.
    //
    static DecimalFormat decimal =
        new DecimalFormat ( "0.###############" );

    final static int BOOLEAN  = 1;
    final static int SCALAR   = 2;
    final static int VECTOR   = 3;

    static class Value {
        int type;	// BOOLEAN, SCALAR, or VECTOR.
	boolean b;	// Value if BOOLEAN.
	double s;	// Value if SCALAR.
	double x, y;	// Coordinates if VECTOR.

	void printValue ( )
	{
	    if ( type == BOOLEAN )
	        print ( String.valueOf ( b ) );
	    else if ( type == SCALAR )
	        print
		    ( decimal.format ( s ) );
	    else if ( type == VECTOR )
	        print (   "("  + decimal.format ( x )
		        + ", " + decimal.format ( y )
			+ ")" );
	    else
	        print
		    ( "BAD OBJECT TYPE " + type );
	}
    }

    static Hashtable<String,Value> variable_table =
	    new Hashtable<String,Value>();
        // Maps variable name Strings to Values.

    static void require_boolean ( Value v1 )
    {
        if ( v1.type != BOOLEAN )
	    error ( "operand should be boolean" );
    }
    static void require_boolean ( Value v1, Value v2 )
    {
        if ( v1.type != BOOLEAN )
	    error
	        ( "first operand should be boolean" );
        else if ( v2.type != BOOLEAN )
	    error
	        ( "second operand should be boolean" );
    }

    static void require_scalar ( Value v1 )
    {
        if ( v1.type != SCALAR )
	    error ( "operand should be scalar" );
    }
    static void require_scalar ( Value v1, Value v2 )
    {
        if ( v1.type != SCALAR )
	    error ( "first operand should be scalar" );
        else if ( v2.type != SCALAR )
	    error ( "second operand should be scalar" );
    }

    static void require_vector ( Value v1 )
    {
        if ( v1.type != VECTOR )
	    error ( "operand should be vector" );
    }
    static void require_vector ( Value v1, Value v2 )
    {
        if ( v1.type != VECTOR )
	    error ( "first operand should be vector" );
        else if ( v2.type != VECTOR )
	    error ( "second operand should be vector" );
    }
    static void require_scalar_vector
            ( Value v1, Value v2 )
    {
        if ( v1.type != SCALAR )
	    error ( "first operand should be scalar" );
        else if ( v2.type != VECTOR )
	    error ( "second operand should be vector" );
    }
    static void require_vector_scalar
            ( Value v1, Value v2 )
    {
        if ( v1.type != VECTOR )
	    error ( "first operand should be vector" );
        else if ( v2.type != SCALAR )
	    error ( "second operand should be scalar" );
    }
 
    // Require v1 to be SCALAR or VECTOR and return
    // true if SCALAR, false if VECTOR.
    //
    static boolean is_scalar ( Value v1 )
    {
        if ( v1.type == SCALAR )
	    return true;
        else if ( v1.type == VECTOR )
	    return false;
	else
	    error
	      ( "operand should be scalar or vector" );

	return false; // never executed
    }
 
    // Require v1 and v2 to be BOTH SCALAR or BOTH
    // VECTOR and return true if SCALAR, false if
    // VECTOR.
    //
    static boolean is_scalar ( Value v1, Value v2 )
    {
	if ( v1.type != v2.type )
	    error ( "operands should both be scalar"
	            + " or both be vector" );
        else if ( v1.type == SCALAR )
	    return true;
        else if ( v2.type == VECTOR )
	    return false;
	else
	    error
	      ( "operands should be scalar or vector" );

	return false; // never executed
    }

    // Reads a constant value and returns its Value,
    // or a variable with a value and returns a COPY
    // of its Value.  Always returns a NEW Value.
    //
    static Value get_value ( )
    {
	Value v = new Value();

        String token = get_token();
	if ( token.equals ( "(" ) )
	{
	    v.type = VECTOR;
	    token = get_token();
	    v.x = token_to_scalar ( token );
	    skip ( "," );
	    token = get_token();
	    v.y = token_to_scalar ( token );
	    skip ( ")" );
	}
	else if ( token.equals ( "true" ) )
	{
	    v.type = BOOLEAN;
	    v.b = true;
	}
	else if ( token.equals ( "false" ) )
	{
	    v.type = BOOLEAN;
	    v.b = false;
	}
	else if ( is_variable ( token ) )
	{
	    Value v2 = variable_table.get ( token );
	    if ( v2 == null )
	        error ( "`" + token + "' unassigned" );
	    v.type = v2.type;
	    v.b = v2.b;
	    v.s = v2.s;
	    v.x = v2.x;
	    v.y = v2.y;
	}
	else if ( is_scalar ( token ) )
	{
	    v.type = SCALAR;
	    v.s = token_to_scalar ( token );
	}
	else
	    error ( "expected true, false, " +
	            " scalar constant, " +
	            " vector constant, " +
	            " or variable but got `" +
		    token + "'" );
	return v;
    }

    // Execute `clear ...' statement after `clear' token
    // has been read and skipped.
    //
    static void execute_clear ( )
    {
        boolean found = false;
	while ( true )
	{
	    String token = get_token();
	    if ( token.equals ( EOL ) ) break;
	    check_variable ( token );
	    variable_table.remove ( token );
	    found = true;
	}
	if ( ! found )
	    variable_table.clear();
    }

    // Execute `print{ln} ...' statement after
    // `print{ln}' token has been read and skipped,
    // BUT do not output final space or line end.
    //
    static void execute_print ( )
    {
        boolean first = true;
	while ( true )
	{
	    String token = get_token();
	    if ( token.equals ( EOL ) ) break;
	    if ( first )
		first = false;
	    else
	        print ( " " );
	    Value v = variable_table.get ( token );

	    if ( v == null )
	        print ( token );
	    else 
	        v.printValue();
	}
    }

    // Execute `variable = ...' statement after
    // `variable =' tokens have been read and skipped.
    // Check variable token to be sure its a variable
    // name.
    //
    static void execute_assign ( String variable )
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
	    v1.s = Math.sqrt ( v1.x*v1.x + v1.y*v1.y );
	    v1.type = SCALAR;
	}
	else if ( op.equals ( "angle" ) )
	{
	    v1 = get_value();
	    require_vector ( v1 );
	    if ( v1.x == 0 && v1.y == 0 )
		error ( "angle of zero vector" );
	    else if ( v1.x == 0 )
	        v1.s = ( v1.y > 0 ? +90 : -90 );
	    else if ( v1.y == 0 )
	        v1.s = ( v1.x > 0 ? 0 : +180 );
	    else
	    {
		v1.s = Math.atan2 ( v1.y, v1.x );
		v1.s *= 180.0 / Math.PI;
	    }
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
		if ( is_scalar ( v1 ) )
		{
		    if ( is_scalar ( v2 ) )
			v1.s *= v2.s;
		    else
		    {
			v1.x = v1.s * v2.x;
			v1.y = v1.s * v2.y;
			v1.type = VECTOR;
		    }
		}
		else
		{
		    require_vector ( v1, v2 );
		    v1.s = v1.x * v2.x + v1.y * v2.y;
		    v1.type = SCALAR;
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
		require_vector_scalar ( v1, v2 );
		double angle = v2.s * Math.PI / 180;
		double sin = Math.sin ( angle );
		double cos = Math.cos ( angle );

		// We want perfect precision for
		// certain angles so if v1 has
		// integer coordinates so will
		// the result.
		//
		int k = (int) ( v2.s / 90 );
		if ( v2.s == k * 90 )
		{
		    switch ( k % 4 )
		    {
		    case  0: sin = 0; cos = 1; break;
		    case +1:
		    case -3: sin = 1; cos = 0; break;
		    case +2:
		    case -2: sin = 0; cos = -1; break;
		    case +3:
		    case -1: sin = -1; cos = 0; break;
		    }
		}
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
	    else if ( ! op.equals ( EOL ) )
	        error ( "`" + op + "' unrecognized" );
	    else backup = true;
	}

	skip ( EOL );

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
	    if ( token == EOF ) break;

	    if ( token.equals ( "if" ) )
	    {
	        Value v = get_value();
		require_boolean ( v );
		if ( ! v.b )
		{
		    while ( true )
		    {
		        token = get_token();
			check_not_eof ( token );
			if ( token.equals ( EOL ) )
			    break;
		    }
		    continue;
		}
		skip ( ":" );
		token = get_token();
	    }

	    if ( token.equals ( "clear" ) )
	        execute_clear();
	    else if ( token.equals ( "print" ) )
	    {
	        execute_print();
		print ( " " );
	    }
	    else if ( token.equals ( "println" ) )
	    {
	        execute_print();
		println();
	    }
	    else
	    {
	        String op = get_token();
		if ( op.equals ( "=" ) )
		    execute_assign ( token );
		else
		    error ( "`" + token + " " + op +
		            "' unrecognized" );
	    }
	}
    }
}
