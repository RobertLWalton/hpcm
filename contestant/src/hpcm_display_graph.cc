// Display a Graph
//
// File:	hpcm_display_graph.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Sun Oct  2 05:29:59 EDT 2011
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: walton $
//   $Date: 2011/10/02 09:31:42 $
//   $RCSfile: hpcm_display_graph.cc,v $
//   $Revision: 1.7 $

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
#define XK_MISCELLANY
#define XK_LATIN1
#include <X11/keysymdef.h>
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
"\f\n"
"    With the -ps option, postscript is written to\n"
"    the standard output.\n"
"\n"
"    With the -X option, an X-window is opened and\n"
"    the first test case displayed.  Typing a car-\n"
"    riage return goes to the next test case, and\n"
"    typing control-C terminates the display.\n"
"\n"
"    Points are displayed as dots, edges as lines.\n"
"    If two edges overlap, the line is thickened.  If\n"
"    there is a line from a point to itself, the\n"
"    point dot is made larger.  The X and Y scales\n"
"    are chosen to be identical so relative distance\n"
"    measurements may be made.\n";


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

const double page_height = 11*72;	    // 11.0"
const double page_width = 8*72 + 72/2;	    // 8.5"

const double top_margin = 36;		    // 0.5"
const double bottom_margin = 36;	    // 0.5"
const double side_margin = 72;		    // 1.0"
const double page_title_height = 72+36;	    // 1.5"
const double title_font_size = 16;	    // 16/72"

const double page_big_node_size = 5;	    // 5/72"
const double page_small_node_size = 3;      // 3/72"
const double page_narrow_edge_size = 1;    // 1/72"
const double page_wide_edge_size = 2;	    // 2/72"

const double print_box = page_width - 2 * side_margin;

// For X-Windows, units are 1 pixel.

const int window_height = 700;
const int window_title_height = 50;
const int window_foot_height = 50;
const int window_width = 600;
const char window_foot[] =
    "Type SPACE for next page, control-C to quit.";

const int window_big_node_size = 5;
const int window_small_node_size = 2;
const int window_narrow_edge_size = 1;
const int window_wide_edge_size = 3;

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
    Display * display = NULL;
    Window window;
    Visual * visual = NULL;

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
	    display = XOpenDisplay ( NULL );
	    if ( display == NULL )
	    {
		cout << "Cannot open X-Display;"
		        " maybe DISPLAY environment"
			" variable is not set"
		     << endl;
		exit (1);
	    }
	    int screen = XDefaultScreen ( display );
	    int black = XBlackPixel ( display, screen );
	    int white = XWhitePixel ( display, screen );
	    window =
	        XCreateSimpleWindow
		    ( display,
	              XDefaultRootWindow ( display ),
		      0, 0,
		      window_width, window_height,
		      0,
		      black, white );
	    Visual * visual =
	        XDefaultVisual ( display, screen );
	    page = cairo_xlib_surface_create
	    		( display, window, visual,
			  window_width, window_height );
	    XSelectInput ( display, window,
	                     StructureNotifyMask
			   | ExposureMask
			   | KeyPressMask
			   | KeyReleaseMask );
	    XMapWindow ( display, window );

	    while ( true )
	    {
	        XEvent e;
		XNextEvent ( display, & e );
		if ( e.type == MapNotify ) break;
	    }
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

    // Keep track of which control keys are pressed.
    //
    bool left_control_pressed = false;
    bool right_control_pressed = false;

    string in_name, out_line;
    getline ( out, out_line );
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
	else if ( in_name.compare ( out_line ) != 0 )
	{
	    cout << "Test case names are not equal:"
	         << endl
		 << "  " << in_file << ": " << in_name
		 << endl
		 << "  " << out_file << ": " << out_line
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
	    getline ( out, out_line );
	    if ( out.eof() ) break;

	    istringstream outs ( out_line );
	    outs >> ws;
	    if ( ! isdigit ( outs.peek() ) )
	        break;
	    int i, j;
	    outs >> i >> j >> ws;
	    if ( outs.peek() != EOF )
	    {
	        cout << "Badly formatted line in "
		     << out_file << ":" << endl
		     << "  " << out_line << endl;
		exit ( 1 );
	    }
	    if ( i < 1 || j < 1 || i > N || j > N )
	    {
	        cout << "Bad point indices in "
		     << out_file << ":" << endl
		     << "  " << out_line << endl;
		exit ( 1 );
	    }
	    ++ edges(i-1,j-1);
	}

	// Compute bonding box for all points.
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

	while ( true )
	{
	    double title_top, title_height, title_width,
	           graph_top, graph_height,
		   graph_bottom,
		   graph_left, graph_width,
		   big_node_size, small_node_size,
		   wide_edge_size, narrow_edge_size;

	    if ( display != NULL )
	    {

		// Find window width and height.
		//
		Window parent;
		int x, y;
		unsigned width, height,
		         border_width, depth;
		XGetGeometry ( display, window,
		               & parent,
			       & x, & y,
			       & width, & height,
			       & border_width,
			       & depth );
		XClearArea ( display, window, 0, 0,
		             width, height, false );

	        title_top = 0;
		title_width = width;
		title_height = window_title_height;

		// Use window_big_node_size as a graph
		// margin to be sure big nodes can be
		// seen.
		//
		graph_top = window_title_height;
		graph_height = height - graph_top
		             - window_big_node_size
			     - window_foot_height;
		graph_bottom = height;
		graph_left = window_big_node_size;
		graph_width = width
		            - 2 * window_big_node_size;

		big_node_size = window_big_node_size;
		small_node_size =
		    window_small_node_size;
		wide_edge_size = window_wide_edge_size;
		narrow_edge_size =
		    window_narrow_edge_size;
	    }
	    else
	    {
	        title_top = top_margin;
		title_width = page_width;
		title_height = page_title_height;

		graph_top = top_margin
		          + page_title_height;
		graph_height =
		    page_height - graph_top
		                - bottom_margin;
		graph_left = side_margin;
		graph_width =
		    page_width - 2 * side_margin;

		big_node_size = page_big_node_size;
		small_node_size = page_small_node_size;
		wide_edge_size = page_wide_edge_size;
		narrow_edge_size =
		    page_narrow_edge_size;
	    }

	    // Set up point scaling.
	    //
	    double dx = xmax - xmin;
	    double dy = ymax - ymin;
	    if ( dx == 0 ) dx = 1;
	    if ( dy == 0 ) dy = 1;
	    double xscale = graph_width / dx;
	    double yscale = graph_height / dy;

	    // Make the scales the same.
	    //
	    if ( xscale > yscale )
	        xscale = yscale;
	    else if ( xscale < yscale )
	        yscale = xscale;

#	    define CONVERT(i) \
		  graph_left \
		+ (point[i].x - xmin) * xscale, \
		  graph_top + graph_height \
		- (point[i].y - ymin) * yscale

	    // Display test case name.
	    //
	    cairo_text_extents_t te;
	    cairo_text_extents
		( title_c, in_name.c_str(), & te );
	    assert (    cairo_status ( title_c )
		     == CAIRO_STATUS_SUCCESS );
	    cairo_move_to
		( title_c, title_width/2 - te.width/2,
		  title_top
		  +
		  title_font_size
		  +
		    ( title_height - title_font_size )
		  / 2 );
	    cairo_show_text
		( title_c, in_name.c_str() );
	    assert (    cairo_status ( title_c )
		     == CAIRO_STATUS_SUCCESS );

	    // Display foot for X-windows.
	    //
	    if ( display != NULL )
	    {
		cairo_text_extents
		    ( title_c, window_foot, & te );
		assert (    cairo_status ( title_c )
			 == CAIRO_STATUS_SUCCESS );
		cairo_move_to
		    ( title_c,
		      title_width/2 - te.width/2,
		        graph_bottom
		      -   (   window_foot_height
		            - title_font_size )
			/ 2 );
		cairo_show_text
		    ( title_c, window_foot );
		assert (    cairo_status ( title_c )
			 == CAIRO_STATUS_SUCCESS );
	    }

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
		      count > 1 ? wide_edge_size :
				  narrow_edge_size );
		cairo_stroke ( graph_c );
	    }

	    cairo_show_page ( title_c );

	    if ( display != NULL ) while ( true )
	    {
	        XEvent e;
		XNextEvent ( display, & e );
		if ( e.type == KeyPress )
		{
		    KeySym key =
		        XLookupKeysym ( & e.xkey, 0 );
		    if ( key == XK_Control_L )
		        left_control_pressed = true;
		    else if ( key == XK_Control_R )
		        right_control_pressed = true;
		    else if ( key == XK_c
		              &&
			      ( left_control_pressed
			        ||
				right_control_pressed )
			    )
		        goto PROGRAM_DONE;
		    else
		       goto PAGE_DONE;
		    // Go to next test case.
		}
		else if ( e.type == KeyRelease )
		{
		    KeySym key =
		        XLookupKeysym ( & e.xkey, 0 );
		    if ( key == XK_Control_L )
		        left_control_pressed = false;
		    else if ( key == XK_Control_R )
		        right_control_pressed = false;
		}
		if ( e.type == Expose
		     &&
		     e.xexpose.count == 0 )
		    break;
		    // Redraw current window.
	    }
	    else goto PAGE_DONE;
	}
	PAGE_DONE:;
    }

    PROGRAM_DONE:

    cairo_destroy ( title_c );
    cairo_destroy ( graph_c );
    cairo_surface_destroy ( page );

    // Return from main function without error.

    return 0;
}
