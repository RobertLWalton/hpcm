// Display a Graph
//
// File:	hpcm_display_graph.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Sun Jul 24 21:21:54 EDT 2011
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: walton $
//   $Date: 2011/07/25 01:30:21 $
//   $RCSfile: hpcm_display_graph.cc,v $
//   $Revision: 1.2 $

#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
using std::cout;

const char * const documentation = "\n"
"hpcm_display_graph [-ps|-X] header \\\n"
"                   input-file output-file\n"
"\n"
"    This program displays undirected graphs defined\n"
"    by points in the input-file and edges in the\n"
"    output-file.\n"
"\n"
"    The format of the input-file is a set of test\n"
"    cases, each beginning with a 1-line test case\n"
"    name that is followed by lines containing the\n"
"    numbers\n"
"\n"
"        N x[1] y[1] x[2] y[2] ... x[N] y[N]\n"
"\n"
"    N is the number of points in the graph, while\n"
"    (x[i],y[i]) is the i'th point (1 <= i <= N).\n"
"    N is a positive integer and the x[i], y[i] are\n"
"    floating point.  Numbers may be separated by any\n"
"    kind and amount of whitespace, including single\n"
"    spaces, tabs, and line feeds.\n"
"\n"
"    The format of the output-file is a set of test\n"
"    cases, each beginning with a 1-line test case\n"
"    name that is followed by `edge lines' EACH with\n"
"    the format:\n"
"\n"
"        i j\n"
"\n"
"    indicating there is an undirected edge between\n"
"    the point (x[i],y[i]) and the point (x[j],y[j]).\n"
"    Here i and j are integers >= 1.\n"
"\n"
"    In the output-file the test case name lines are\n"
"    guarenteed not have a digit as their first non-\n"
"    whitespace character, and thus can be distin-\n"
"    guished from edge lines.\n"
"\n"
"    With the -ps option, postscript is written to the\n"
"    standard output.\n"
"\n"
"    With the -X option, an X-window is opened and the\n"
"    first test case displayed.  Typing a carriage\n"
"    return goes to the next test case, and typing an\n"
"    unsigned integer M followed by a carriage return\n"
"    goes to the M'th test case.\n"
"\n"
"    Points are displayed as dots, edges as lines.  If\n"
"    two edges overlap, the line it thickened.  If\n"
"    there is a line from a point to itself, the point\n"
"    dot is made larger.\n";





// Main program.
//
int main ( int argc, char ** argv )
{

    // Process options.

    while ( argc >= 4 && argv[1][0] == '-' )
    {

	char * name = argv[1] + 1;

        if (    strcmp ( "ps", name ) == 0 )
	{
	}
        else if ( strncmp ( "doc", name, 3 ) == 0 )
	{
	    // Any -doc* option prints documentation
	    // and exits with error status.
	    //
	    cout << documentation;
	    exit (1);
	}

    }

    // Print documentation and exit with error status
    // unless there are exactly three program arguments
    // left.

    if ( argc != 4 )
    {
        cout << documentation;
	exit (1);
    }

    // Open files.

    // TBD


    // Return from main function without error.

    return 0;
}
