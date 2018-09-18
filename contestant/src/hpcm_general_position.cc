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
#include <cassert>
using std::cout;
using std::cerr;
using std::endl;
using std::cin;

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
"                  struct { int X, Y; }\n"
"\n"
"    The random number generator is seeded from /dev/\n"
"    random.  Progress is reported on stderr.\n"
;


// Main program.
//
int main ( int argc, char ** argv )
{

    return 0;
}
