// Display a Graph
//
// File:	hpcm_display_graph.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Sun Aug 14 07:51:24 EDT 2011
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: walton $
//   $Date: 2011/08/14 12:49:03 $
//   $RCSfile: hpcm_display_graph.cc,v $
//   $Revision: 1.3 $

#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <sstream>
#include <cctype>
#include <cfloat>
#include <cmath>
#include <cassert>
using std::cout;
using std::endl;
using std::ws;
using std::ifstream;
using std::string;
using std::istringstream;

extern "C" {
#include <cairo-ps.h>
#include <cairo-xlib.h>
}

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
"    The test case name lines in the output file MUST\n"
"    match the corresponding test case name lines in\n"
"    the input file.  Test case name lines are guar-\n"
"    enteed not have a digit as their first non-\n"
"    whitespace character, and thus can be distin-\n"
"    guished from edge lines.\n"
"\n"
"    With the -ps option, postscript is written to\n"
"    the standard output.\n"
"\n"
"    With the -X option, an X-window is opened and\n"
"    the first test case displayed.  Typing a car-\n"
"    riage return goes to the next test case, and\n"
"    typing an unsigned integer M followed by a car-\n"
"    riage return goes to the M'th test case.\n"
"\n"
"    Points are displayed as dots, edges as lines.\n"
"    If two edges overlap, the line is thickened.  If\n"
"    there is a line from a point to itself, the\n"
"    point dot is made larger.\n";


// Current test case data.
//
int N;	// Number of points.
struct point_struct { double x; double y; }
    * point;
//
// Note: internally points are numbered 0 .. N-1 while
// externally they are numbered 1 .. N.
//
// edges(i,j) is the number of edges from point i to
// point j, 0 <= i,j < N.
//
int * edges_vector;
inline int & edges ( int i, int j )
{
    return edges_vector[i*N + j];
}

// For postscript output, units are 1/72".

// You MUST declare the entire paper size, 8.5x11",
// else you get a non-centered printout.

const double page_height = 11*72;	// 11.0"
const double page_width = 8*72 + 72/2;	// 8.5"

const double top_margin = 36;		// 0.5"
const double bottom_margin = 36;	// 0.5"
const double side_margin = 72;		// 1.0"
const double title_height = 72+36;	// 1.5"
const double title_font_size = 16;	// 16/72"

const double big_node_size = 10.8;	// 3/20"
const double small_node_size = 3.6;	// 1/20"
const double narrow_line_width = 1;	// 1/72"
const double wide_line_width = 3;	// 3/72"

const double print_box = page_width - 2 * side_margin;

// cairo_write_func_t to write data to cout.
//
cairo_status_t write_to_cout
    ( void * closure,
      const unsigned char * data, unsigned int length )
{
    cout.write ( (const char *) data, length );
    return CAIRO_STATUS_SUCCESS;
}

// Main program.
//
int main ( int argc, char ** argv )
{

    cairo_surface_t * page = NULL;

    // Process options.

    while ( argc >= 4 && argv[1][0] == '-' )
    {

	char * name = argv[1] + 1;

        if (    strcmp ( "ps", name ) == 0 )
	{
	    page = cairo_ps_surface_create_for_stream
	    		( write_to_cout, NULL,
			  page_width, page_height );
	}
        else if (    strcmp ( "X", name ) == 0 )
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

	++ argv, -- argc;
    }

    if ( page == NULL )
    {
        cout << documentation;
	exit (1);
    }

    // Print documentation and exit with error status
    // unless there are exactly two program arguments
    // left.

    if ( argc != 3 )
    {
        cout << documentation;
	exit (1);
    }

    // Open files.
    //
    const char * in_file = argv[1];
    const char * out_file = argv[2];
    ifstream in ( in_file );
    if ( ! in )
    {
        cout << "Cannot open " << in_file << endl;
	exit ( 1 );
    }
    ifstream out ( out_file );
    if ( ! out )
    {
        cout << "Cannot open " << out_file << endl;
	exit ( 1 );
    }

    // 4 is the max number of allowed cairo contexts.
    // This seems to be undocumented?
    //
    cairo_t * title_c = cairo_create ( page );
    cairo_set_source_rgb ( title_c, 0.0, 0.0, 0.0 );
    cairo_select_font_face ( title_c, "sans-serif",
                             CAIRO_FONT_SLANT_NORMAL,
			     CAIRO_FONT_WEIGHT_BOLD );
    cairo_set_font_size ( title_c, title_font_size );
    assert (    cairo_status ( title_c )
	     == CAIRO_STATUS_SUCCESS );

    cairo_t * graph_c = cairo_create ( page );
    cairo_set_source_rgb ( graph_c, 0.0, 0.0, 0.0 );
    assert (    cairo_status ( title_c )
	     == CAIRO_STATUS_SUCCESS );

    string in_name, out_name;
    getline ( out, out_name );
        // We read-ahead the out file, as the test
	// case name line of the next test case is
	// also the end-of-test-case signal for the
	// previous test case.
    while ( true )
    {
	getline ( in, in_name );
	if ( in.eof() && out.eof() ) break;
	else if ( in.eof() )
	{
	    cout << "Premature end of file on "
	         << in_file << endl;
	    exit ( 1 );
	}
	else if ( out.eof() )
	{
	    cout << "Premature end of file on "
	         << out_file << endl;
	    exit ( 1 );
	}
	else if ( in_name.compare ( out_name ) != 0 )
	{
	    cout << "Test case names are not equal:"
	         << endl
		 << "  " << in_file << ": " << in_name
		 << endl
		 << "  " << out_file << ": " << out_name
		 << endl;
	    exit ( 1 );
	}

	// Read in file data.
	//
	in >> N;
	assert ( in && N > 0 );
	point = new point_struct[N];
	for ( int i = 0; i < N; ++ i )
	    in >> point[i].x >> point[i].y;
	assert ( in );
	while ( in.get() != '\n' )
	    assert ( in );

	// Read out file data.  Note: if there are
	// errors in out file we print a message and
	// exit rather than just using `assert'.
	//
	edges_vector = new int[N*N];
	for ( int i = 0; i < N; ++ i )
	for ( int j = 0; j < N; ++ j )
	    edges(i,j) = 0;
	while ( true )
	{
	    getline ( out, out_name );
	        // Out_name doubles as out file next
		// input line.
	    if ( out.eof() ) break;

	    istringstream outs ( out_name );
	    outs >> ws;
	    if ( ! isdigit ( outs.peek() ) )
	        break;
	    int i, j;
	    outs >> i >> j >> ws;
	    if ( outs.peek() != EOF )
	    {
	        cout << "Badly formatted line in "
		     << out_file << ":" << endl
		     << "  " << out_name << endl;
		exit ( 1 );
	    }
	    if ( i < 1 || j < 1 || i > N || j > N )
	    {
	        cout << "Bad point indices in "
		     << out_file << ":" << endl
		     << "  " << out_name << endl;
		exit ( 1 );
	    }
	    ++ edges(i-1,j-1);
	}

	// Show test case name.
	//
	cairo_text_extents_t te;
	cairo_text_extents
	    ( title_c, in_name.c_str(), & te );
	assert (    cairo_status ( title_c )
		 == CAIRO_STATUS_SUCCESS );
	cairo_move_to
	    ( title_c, page_width/2 - te.width/2,
	      top_margin
	      +
	      title_font_size
	      +
	      ( title_height - title_font_size ) / 2 );
	cairo_show_text ( title_c, in_name.c_str() );
	assert (    cairo_status ( title_c )
	         == CAIRO_STATUS_SUCCESS );

	// Compute boundaries
	//
	double xmin = DBL_MAX, xmax = - DBL_MAX;
	double ymin = DBL_MAX, ymax = - DBL_MAX;
	for ( int i = 0; i < N; ++ i )
	{
	    if ( point[i].x < xmin ) xmin = point[i].x;
	    if ( point[i].x > xmax ) xmax = point[i].x;
	    if ( point[i].y < ymin ) ymin = point[i].y;
	    if ( point[i].y > ymax ) ymax = point[i].y;
	}
	double top = top_margin + title_height;
	double bottom = page_height - bottom_margin;
	double left = side_margin;
	double right = page_width - side_margin;

	// Set up point scaling.
	//
	double dx = xmax - xmin;
	double dy = ymax - ymin;
	if ( dx == 0 ) dx = 1;
	if ( dy == 0 ) dy = 1;
	double xscale = (right - left) / dx;
	double yscale = (bottom - top) / dy;

#	define CONVERT(i) \
	    left + (point[i].x - xmin) * xscale, \
	    bottom - (point[i].y - ymin) * yscale

	// Draw points.
	//
	for ( int i = 0; i < N; ++ i )
	{
	    cairo_new_sub_path ( graph_c );
	    cairo_arc
	        ( graph_c, CONVERT(i),
		  edges(i,i) > 0 ? big_node_size :
				   small_node_size,
		  0.0, 2 * M_PI );
	    cairo_fill ( graph_c );
	    assert (    cairo_status ( graph_c )
		     == CAIRO_STATUS_SUCCESS );
	}

	// Draw edges.
	//
	for ( int i = 0; i < N; ++ i )
	for ( int j = i + 1; j < N; ++ j )
	{
	    int count = edges(i,j) + edges(j,i);
	    if ( count == 0 ) continue;
	    cairo_move_to ( graph_c, CONVERT(i) );
	    cairo_line_to ( graph_c, CONVERT(j) );
	    cairo_set_line_width
		( graph_c,
		  count > 1 ? wide_line_width :
		              narrow_line_width );
	    cairo_stroke ( graph_c );
	}

	cairo_show_page ( title_c );

    }

    cairo_destroy ( title_c );
    cairo_destroy ( graph_c );
    cairo_surface_destroy ( page );

    // Return from main function without error.

    return 0;
}