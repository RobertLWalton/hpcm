// Solution to the Simple Vector Calculator Problem
//
// File:	vcalc.cc
// Authors:	Bob Walton (walton@seas.harvard.edu)
// Date:	Tue Jan 22 00:18:29 EST 2013
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: walton $
//   $Date: 2013/01/22 06:28:41 $
//   $RCSfile: vcalc.cc,v $
//   $Revision: 1.2 $

#include <iostream>
#include <iomanip>
#include <cstdio>
#include <string>	// string
#include <map>		// map
using namespace std;
#include <cassert>
#include <cctype>	// isspace, isblank
#include <cmath>	// fabs, atan2, sqrt, M_PI

int debug = false;
#define dout    if ( debug ) cout
#define dprintf if ( debug ) printf

// Convert double to string.
//
string to_string ( double d )
{
    char buffer[100];
    sprintf ( "%.15f", d );
    return string ( buffer );
}

// Skip input until next non-whitespace or end of line
// or end of file if `comment' is false, or until next
// end of line or end of file if `comment' is true.
// Return next non-whitespace character (which is not
// skipped), or '\n' for end of line (the line feed IS
// skipped), or 0 for end of file.
//
char skip_ws ( bool comment = false)
{
    char c;
    while ( cin >> c, cin.good() )
    {
        if ( c == '\n' ) return c;
	else if ( ! comment && ! isblank ( c ) )
	    return c;
    }
    return 0;
}

// Skip non-whitespace until the next separator char-
// acter (`(', `)', `,', `:', `\n') or whitespace,
// and return the characters skipped as a string.
//
string read_symbol ( void )
{
    char buffer[200];
    char * p = buffer;
    string result = "";
    while ( true )
    {
	char c;
        cin >> c;
	if ( ! cin.good() ) break;
	if ( c == '(' || c == ')' ||
	     c == ',' || c == ':' ||
	     isspace ( c ) )
	{
	    cin.unget();
	    break;
	}
	* p ++ = c;
	if ( p - buffer >= sizeof ( buffer ) - 1 )
	{
	    * p = 0;
	    result += buffer;
	    p = buffer;
	}
    }
    * p = 0;
    * result += buffer;
    return result;
}

// Get a token which is either a number or a string or
// a line end or an end of file.  Line end is represent-
// ed by the string "(END-OF-LINE)" and end of file by
// the string "(END-OF-FILE)" so that error messages are
// easy to print. 
//
// Set is_number true if number, false if string.
// If set true, `token' is set to "(NUMBER)" for
// error messages.
//
// Backup may be set true to backup one token.
//
// Note that token is never returned empty.
//
string ELINE = "(END-OF-LINE)";
string EFILE = "(END-OF-FILE)";
string token = ELINE;
	 	// String token gotten or "(NUMBER)".
double number;	// Number token gotten.
bool is_number;	// True if token is number.
bool backup = false;
int line_number = 0;
void get_token ( void )
{
    if ( backup )
    {
        backup = false;
	return;
    }

    char c;
    if ( token == ELINE )
    {
        while ( true )
	{
	    ++ line_number;
	    c = skip_ws();
	    if ( c == '/' )
	    {
		cin >> c;
		if ( ! cin.good() )
		    error ( "unexpected end of file" );
		else if ( c == '/' )
		    skip_ws ( true );
		else
		{
		    cin.unget();
		    break;
		}
	    }
	}
    }
    else
	c = skip_ws();
    
    is_number = false;

    if ( c == 0 )
	token = EFILE;
    else if ( c == '\n' )
	token = ELINE;
    else if ( c == '(' )
	token = "(";
    else if ( c == ')' )
	token = ")";
    else if ( c == ',' )
	token = ",";
    else if ( c == ':' )
	token = ":";
    else if ( cin.unget(),
	      cin >> number, cin.good() )
    {
	is_number = true;
	token = "(NUMBER)";
    }
    else
	cin.clear(), token = read_symbol();
}

// Print error message and exit.
//
static void error ( string message )
{
    cout << "ERROR in line " <<  line_number << ":"
         << endl;
    cout << "      " << message << endl;
    exit ( 1 );
}

static void check_not_eof ( void )
{
   if ( token == EFILE )
      error ( "unexpected end of file" );
}

static void check_number ( void )
{
   if ( ! is_number )
      error ( "expected number but got `" + token
              + "'" );
}

// Read token and check that it matches desired string.
//
static void skip ( string desired )
{
    get_token();
    if ( token != desired )
	error ( "expected `" + desired +
	        "' but found `" + token + "'" );
}

static bool is_variable ( void )
{
    return ! is_number && isalpha ( token[0] );
}

static void check_variable ( String token )
{
    if ( ! is_variable ( token ) )
        error ( "`" + token + "' is not a variable" );
}

struct Vector {
    double x, y;
    Vector ( double xval, double yval )
    {
	x = xval; y = yval;
    }
}
ostream & operator<< ( ostream & s, const Vector & v )
{
    return s << "("  << to_string ( v.x )
             << ", " << to_string ( v.y )
	     << ")";
}
Vector operator- ( Vector v )
{
    return Vector ( - v.x, - v.y );
}
Vector operator+ ( Vector v1, Vector v2 )
{
    return Vector ( v1.x + v2.x, v1.y + v2.y );
}
Vector operator- ( Vector v1, Vector v2 )
{
    return Vector ( v1.x - v2.x, v1.y - v2.y );
}
double operator* ( Vector v1, Vector v2 )
{
    return v1.x * v2.x + v1.y * v2.y;
}
Vector operator* ( double s, Vector v )
{
    return Vector ( s * v.x, s * v.y );
}
double length ( Vector v )
{
    return sqrt ( v.x * v.x + v.y * v.y );
}
double angle ( Vector v )
{
    double result;

    // We take extra care with angles that are
    // multiples of 90 degrees.  This is only
    // necessary if one is using exact equality
    // with integer coordinates instead of
    // approximate equality.
    //
    if ( v.x == 0 && v.y == 0 )
	error ( "angle of zero vector" );
    else if ( v.x == 0 )
	result = ( v.y > 0 ? +90 : -90 );
    else if ( v.y == 0 )
	result = ( v.x > 0 ? 0 : +180 );
    else
    {
	result = atan2 ( v.y, v.x );
	result *= 180.0 / M_PI;
    }
    return result;
}
Vector rotate ( Vector v, double angle ) 
{
    double sin_angle, cos_angle;

    // We take extra care with angles that are
    // multiples of 90 degrees.  This is only
    // necessary if one is using exact equality
    // with integer coordinates instead of
    // approximate equality.
    //
    int k = (int) ( angle / 90 );
    if ( angle == k * 90 )
    {
	switch ( k % 4 )
	{
	case  0: sin_angle = 0; cos_angle = 1; break;
	case +1:
	case -3: sin_angle = 1; cos_angle = 0; break;
	case +2:
	case -2: sin_angle = 0; cos_angle = -1; break;
	case +3:
	case -1: sin_angle = -1; cos_angle = 0; break;
	default: abort();
	}
    }
    else
    {
	angle *= M_PI / 180;
	sin_angle = sin ( angle );
	cos_angle = cos ( angle );
    }
    return Vector
	( cos_angle * v.x - sin_angle * v.y,
	  sin_angle * v.x + cos_angle * v.y );
}

enum {
    BOOLEAN  = 1,
    SCALAR   = 2,
    VECTOR   = 3
};

struct value {
    int type;	// BOOLEAN, SCALAR, or VECTOR.
    bool b;	// Value if BOOLEAN.
    double s;	// Value if SCALAR.
    Vector v;	// Value if VECTOR.
};

ostream & operator<< ( ostream & s, value v )
{
    if ( v.type == BOOLEAN )
        return s << ( v.b ? "true" : "false" );
    else if ( type == SCALAR )
        return s << to_string ( v.s );
    else if ( type == VECTOR )
	return s << v.v;
    else
	return s << "BAD OBJECT TYPE " << type;
}

map<string,Value> variable_table;
    // Maps variable name strings to Values.

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
}

// Require v1 and v2 to be BOTH SCALAR or BOTH
// VECTOR and return true if SCALAR, false if
// VECTOR.
//
static boolean is_scalar ( Value v1, Value v2 )
{
    if ( v1.type != v2.type )
	error ( "operands should both be scalar"
		" or both be vector" );
    else if ( v1.type == SCALAR )
	return true;
    else if ( v2.type == VECTOR )
	return false;
    else
	error
	  ( "operands should be scalar or vector" );
    }

// Reads a constant value and returns its Value,
// or a variable with a value and returns a its
// Value.
//
static Value get_value ( )
{
    Value v;

    get_token();
    if ( token == "(" )
    {
	v.type = VECTOR;
	get_token();
	check_number();
	double x = number;
	skip ( "," );
	token = get_token();
	check_number();
	double y = number;
	skip ( ")" );
	v.v = Vector ( x, y );
    }
    else if ( token == "true" )
    {
	v.type = BOOLEAN;
	v.b = true;
    }
    else if ( token == "false" )
    {
	v.type = BOOLEAN;
	v.b = false;
    }
    else if ( is_variable ( token ) )
    {
        if ( variable_table.count ( token ) == 0 )
	    error ( "`" + token + "' unassigned" );
	Value v = variable_table[token];
    }
    else if ( is_number )
    {
	v.type = SCALAR;
	v.s = number;
    }
    else
	error ( "expected true, false, "
		" scalar constant, "
		" vector constant, "
		" or variable but got `" +
		token + "'" );
    return v;
}

// Execute `clear ...' statement after `clear' token
// has been read and skipped.
//
static void execute_clear ( )
{
    bool found = false;
    while ( true )
    {
	get_token();
	if ( token == ELINE ) break;
	check_variable ( token );
	variable_table.erase ( token );
	found = true;
    }
    if ( ! found )
	variable_table.erase
	    ( variable_table.begin(),
	      variable_table.end() );
}

// Execute `print{ln} ...' statement after
// `print{ln}' token has been read and skipped,
// BUT do not output final space or line end.
//
static void execute_print ( )
{
    bool first = true;
    while ( true )
    {
	get_token();
	if ( token == ELINE ) break;
	if ( first )
	    first = false;
	else
	    cout << " ";

	if ( variable_table.count ( token ) == 0 )
	    cout << token;
	else 
	    cout << variable_table[token];
    }
}

// Execute `variable = ...' statement after
// `variable =' tokens have been read and skipped.
//
static void execute_assign ( string variable )
{
    if ( variable == "true"
	 ||
	 variable == "false"
	 ||
	 variable == "!"
	 ||
	 variable == "-"
	 ||
	 variable == "|"
	 ||
	 variable == "||" )
	error ( "attempt to assign a value to `"
		+ variable + "'" );

    get_token();

    Value v1;

    if ( token == "-" )
    {
	v1 = get_value();
	if ( is_scalar ( v1 ) )
	    v1.s = - v1.s;
	else
	    v1.v = - v1.v;
    }
    else if ( token == "|" )
    {
	v1 = get_value();
	require_scalar ( v1 );
	skip ( "|" );
	v1.s = fabs ( v1.s );
    }
    else if ( token == "||" )
    {
	v1 = get_value();
	require_vector ( v1 );
	skip ( "||" );
	v1.s = length ( v1.v );
	v1.type = SCALAR;
    }
    else if ( token == "angle" )
    {
	v1 = get_value();
	require_vector ( v1 );
	v1.s = angle ( v1.v );
	v1.type = SCALAR;
    }
    else if ( token == "!" )
    {
	v1 = get_value();
	require_boolean ( v1 );
	v1.b = ! v1.b;
    }
    else
    {
	backup = true;
	v1 = get_value();
	get_token();

	if ( token == "+" )
	{
	    Value v2 = get_value();
	    if ( is_scalar ( v1, v2 ) )
		v1.s += v2.s;
	    else
		v1.v = v1.v + v2.v;
	}
	else if ( token == "-" )
	{
	    Value v2 = get_value();
	    if ( is_scalar ( v1, v2 ) )
		v1.s -= v2.s;
	    else
		v1.v = v1.v - v2.v;
	}
	else if ( token == "*" )
	{
	    Value v2 = get_value();
	    if ( is_scalar ( v1 ) )
	    {
		if ( is_scalar ( v2 ) )
		    v1.s *= v2.s;
		else
		{
		    v1.v = v1.s * v2.v;
		    v1.type = VECTOR;
		}
	    }
	    else
	    {
		require_vector ( v1, v2 );
		v1.s = v1.v * v2.v;
		v1.type = SCALAR;
	    }
	}
	else if ( token == "/" )
	{
	    Value v2 = get_value();
	    require_scalar ( v1, v2 );
	    if ( v2.s == 0 )
		error ( "zero divisor" );
	    v1.s /= v2.s;
	}
	else if ( token == "^" )
	{
	    Value v2 = get_value();
	    require_vector_scalar ( v1, v2 );
	    v1.v = rotate ( v1.v, v2.s );
	}
	else if ( token == "&&" )
	{
	    Value v2 = get_value();
	    require_boolean ( v1, v2 );
	    v1.b = v1.b && v2.b;
	}
	else if ( token == "||" )
	{
	    Value v2 = get_value();
	    require_boolean ( v1, v2 );
	    v1.b = v1.b || v2.b;
	}
	else if ( token == "==" )
	{
	    Value v2 = get_value();
	    require_scalar ( v1, v2 );
	    v1.b = ( v1.s == v2.s );
	    v1.type = BOOLEAN;
	}
	else if ( token == "!=" )
	{
	    Value v2 = get_value();
	    require_scalar ( v1, v2 );
	    v1.b = ( v1.s != v2.s );
	    v1.type = BOOLEAN;
	}
	else if ( token == "<" )
	{
	    Value v2 = get_value();
	    require_scalar ( v1, v2 );
	    v1.b = ( v1.s < v2.s );
	    v1.type = BOOLEAN;
	}
	else if ( token == "<=" )
	{
	    Value v2 = get_value();
	    require_scalar ( v1, v2 );
	    v1.b = ( v1.s <= v2.s );
	    v1.type = BOOLEAN;
	}
	else if ( token == ">" )
	{
	    Value v2 = get_value();
	    require_scalar ( v1, v2 );
	    v1.b = ( v1.s > v2.s );
	    v1.type = BOOLEAN;
	}
	else if ( token == ">=" )
	{
	    Value v2 = get_value();
	    require_scalar ( v1, v2 );
	    v1.b = ( v1.s >= v2.s );
	    v1.type = BOOLEAN;
	}
	else if ( ! token == ELINE )
	    error ( "`" + token + "' unrecognized" );
	else backup = true;
    }

    skip ( ELINE );

    variable_table[variable] = v1;
}


int main ( int argc, char * argv[] )
{
    debug = ( argc > 1 );

    // Loop to read and execute a statement.
    //
    while ( true )
    {
	get_token();
	if ( token == EFILE ) break;

	if ( token == "if" )
	{
	    Value v = get_value();
	    require_boolean ( v );
	    skip ( ":" );
	    if ( ! v.b )
	    {
		while ( true )
		{
		    get_token();
		    check_not_eof();
		    if ( token == ELINE )
			break;
		}
		continue;
	    }
	    get_token();
	}

	if ( token == "clear" )
	    execute_clear();
	else if ( token == "print" )
	{
	    execute_print();
	    cout << " ";
	}
	else if ( token == "println" )
	{
	    execute_print();
	    cout << endl;
	}
	else if ( is_variable() )
	{
	    string variable = token;
	    get_token();
	    if ( token == "=" )
		execute_assign ( variable );
	    else
		error ( "`" + variable +
			" " + token +
			"' unrecognized" );
	}
	else
	    error ( "`" + token + "' unrecognized" );
    }

    return 0;
}
