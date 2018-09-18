// Generate 2-D Points in General Position
//
// File:	hpcm_general_position.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Tue Sep 18 07:43:56 EDT 2018
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.

#include <iostream>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <cassert>
using std::cout;
using std::cerr;
using std::endl;
using std::cin;

extern "C" {
# include <unistd.h>
# include <fcntl.h>
}

bool debug = false;
# define dout if ( debug ) cerr

const char * const documentation = "\n"
"hpcm_general_position [-ascii|-binary] [-debug] \\\n"
"                      N xmin xmax ymin ymax [file]\n"
"\n"
"    Write N random points in general position that\n"
"    are within [xmin,xmax] x [ymin,ymax] into the\n"
"    given file or standard input.  For -ascii the\n"
"    output consists of lines of the form:\n"
"\n"
"                  { X, Y },\n"
"\n"
"    For -binary output consists copies of the 8 byte\n"
"    structure:\n"
"\n"
"                  struct { long X, Y; }\n"
"\n"
"    The random number generator is seeded from /dev/\n"
"    random.  Progress is reported on stderr.\n"
;

bool ascii = false;
bool binary = false;

long N, xmin, xmax, ymin, ymax;

long n;
struct point { long x, y; } * points;
struct line { long a, b; long long c; } * lines;
    // So far there are n <= N points and (n-1)*n lines.
    // A new point must not be on any previous line,
    // where a*x+b*y = c is the equation of the line.

long get_int ( const char * s )
{
    char * endp;
    long v = strtol ( s, & endp, 10 );
    if ( * endp != 0 )
    {
	cout << "Not an integer argument: `" << s << "'"
	     << endl
	     << "Use -doc for documentation."
	     << endl;
	exit (1);
    }
    return v;
}

// Main program.
//
int main ( int argc, char ** argv )
{
    while ( argc >= 2 && argv[1][0] == '-' )
    {

	char * name = argv[1] + 1;

        if ( strncmp ( "deb", name, 3 ) == 0 )
	    debug = true;
        else if ( strncmp ( "doc", name, 3 ) == 0 )
	{
	    // Any -doc* option prints documentation
	    // and exits.
	    //
	    FILE * out = popen ( "less -F", "w" );
	    fputs ( documentation, out );
	    pclose ( out );
	    exit ( 0 );
	}
        else if ( strcmp ( "ascii", name ) == 0 )
	    ascii = true;
        else if ( strcmp ( "binary", name ) == 0 )
	    binary = true;
	else
	{
	    cout << "Cannot understand -" << name
	         << endl
	         << "Use -doc for documentation."
	         << endl;
	    exit (1);
	}

	++ argv, -- argc;
    }

    if ( ! ascii && ! binary )
    {
        cout << "Must specify -ascii or -binary"
	     << endl
	     << "Use -doc for documentation." << endl;
	exit ( 1 );
    }

    if ( ascii && binary )
    {
        cout << "Cannot specify BOTH -ascii and -binary"
	     << endl
	     << "Use -doc for documentation." << endl;
	exit ( 1 );
    }

    if ( argc < 6 )
    {
        cout << "Too few arguments." << endl
	     << "Use -doc for documentation." << endl;
	exit ( 1 );
    }

    N = get_int ( argv[1] );
    xmin = get_int ( argv[2] );
    xmax = get_int ( argv[3] );
    ymin = get_int ( argv[4] );
    ymax = get_int ( argv[5] );

    if ( N < 1 )
    {
        cout << "N < 1" << endl
	     << "Use -doc for documentation." << endl;
	exit ( 1 );
    }
    if ( xmax < xmin )
    {
        cout << "xmax < xmin" << endl
	     << "Use -doc for documentation." << endl;
	exit ( 1 );
    }
    if ( ymax < ymin )
    {
        cout << "ymax < ymin" << endl
	     << "Use -doc for documentation." << endl;
	exit ( 1 );
    }

    unsigned seed;
    int rfd = open ( "/dev/random", O_RDONLY );
    if ( rfd < 0 )
    {
        cout << "Cannot open /dev/random for reading"
	     << endl;
	exit ( 1 );
    }
    if (    read ( rfd, & seed, sizeof ( seed ) )
         != sizeof ( seed ) )
    {
        cout << "Error reading from /dev/random"
	     << endl;
	exit ( 1 );
    }
    close ( rfd );
    srandom ( seed );


    return 0;
}
