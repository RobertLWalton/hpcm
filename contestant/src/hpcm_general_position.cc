// Generate 2-D Points in General Position
//
// File:	hpcm_general_position.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Wed Sep 19 02:36:27 EDT 2018
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.

#include <iostream>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <cmath>
#include <cassert>
using std::cout;
using std::cerr;
using std::endl;
using std::cin;
using std::ostream;

extern "C" {
# include <unistd.h>
# include <fcntl.h>
# include <time.h>
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

long n;  // There are n points so far.
struct point { long x, y; } * points;
long m;  // There are m lines so far.
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

    points = new point[N];
    lines = new line[(N-1)*N];

    n = m = 0;
    cerr << endl;
    ostream & out = cout;
    timespec time1;
    clock_gettime ( CLOCK_MONOTONIC_COARSE, & time1 );
    double failed_attempts = 0;
    long long attempts = 0;
    while ( n < N )
    {
    	++ attempts;
        long x = xmin + random() % ( xmax - xmin + 1 );
        long y = ymin + random() % ( ymax - ymin + 1 );

	bool OK = true;

	// Check that (x,y) is not on any line.  Note
	// that if (x,y) equals a previous point, it
	// will be on lines containing that point.
	//
	for ( int i = 0; OK && i < m; ++ i )
	{
	    line & l = lines[i];
	    if (    (long long) l.a * x
	          + (long long) l.b * y
		 == l.c )
	        OK = false;
	}
	if ( ! OK ) continue;

	// For each previous point create a new line
	// passing through it an the new point.
	//
	for ( int i = 0; i < n; ++ i )
	{
	    point & p = points[i];
	    line & l = lines[m++];
	    long dx = p.x - x;
	    long dy = p.y - y;
	    l.a = - dy;
	    l.b = dx;
	    l.c = - (long long) dy * x
	          + (long long) dx * y;
	}

	point & p = points[n++];
	p.x = x;
	p.y = y;

	char buffer[100];
	sprintf ( buffer, "{ %12d, %12d },", x, y );
	out << buffer << endl;

	-- attempts;
	failed_attempts *= 0.99;
	failed_attempts += 0.01 * attempts;
	attempts = 0;

	if ( n >= 300 )
	{
	    timespec time2;
	    clock_gettime
	        ( CLOCK_MONOTONIC_COARSE, & time2 );
	    double dtime =
	          time2.tv_sec - time1.tv_sec
		+ 1e-9
		* ( time2.tv_nsec - time1.tv_nsec );

	    double etime = (double) N / n;
	    etime = etime * etime * etime * dtime;
	    etime -= dtime;

	    char tbuffer[100];
	    sprintf ( tbuffer,
	              "%6d points"
		      "%3.0f:%02.0f:%02.0f"
		      " to finish"
		      "%3.0f:%02.0f:%02.0f"
		      " time"
		      "%5.0f failed attempts per point",
		      n,
		      etime / 3600,
		      fmod ( etime / 60, 60 ),
		      fmod ( etime, 60 ),
		      dtime / 3600,
		      fmod ( dtime / 60, 60 ),
		      fmod ( dtime, 60 ),
		      failed_attempts );

	    cerr << "\r" << tbuffer;
	}
    }
    cerr << endl;

    return 0;
}
