// Display Segments
//
// File:	hpcm_display_segments.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Mon Aug 13 15:25:38 EDT 2012
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: walton $
//   $Date: 2012/09/09 12:35:32 $
//   $RCSfile: hpcm_display_segments.cc,v $
//   $Revision: 1.1 $

#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <cstdlib>
#include <cstring>
#include <cctype>
#include <cfloat>
#include <cmath>
#include <cassert>
using std::cout;
using std::endl;
using std::ifstream;
using std::string;
using std::vector;

extern "C" {
#include <cairo-ps.h>
#include <cairo-xlib.h>
#define XK_MISCELLANY
#define XK_LATIN1
#include <X11/keysymdef.h>
}

const char * const documentation = "\n"
"hpcm_display_segments [-ps|-X] header file\n"
"\n"
"    This program displays line segment drawings\n"
"    defined in the given file.\n"
"\n"
"    The format of the file is a set of test cases,\n"
"    each beginning with a 1-line test case name that\n"
"    is followed by lines containing the numbers\n"
"\n"
"        x1 y1 x2 y2\n"
"\n"
"    that define line segments.  After these comes a\n"
"    line containing just `.'.\n"
"\n"
"    The test case name lines must not have a digit,\n"
"    sign, or `.' as their first non-whitespace char-\n"
"    acter.\n"
"\n"
"    With the -ps option, postscript is written to\n"
"    the standard output.\n"
"\n"
"    With the -X option, an X-window is opened and\n"
"    the first test case displayed.  Typing a car-\n"
"    riage return goes to the next test case, and\n"
"    typing control-C terminates the display.\n";


// Current test case data.
//
struct point { double x, y; };
struct segment { point begin, end; };
vector<segment> drawing;
double xmin, xmax, ymin, ymax;
    // Bounds on x and y.  Used to set scale.
//

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

    while ( argc >= 2 && argv[1][0] == '-' )
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
	else
	{
	    cout << "Cannot understand -:" << name
	         << endl << endl;
	    cout << documentation;
	    exit (1);
	}

	++ argv, -- argc;
    }

    if ( page == NULL )
    {
	cout << "No -pg or -X" << endl << endl;
        cout << documentation;
	exit (1);
    }

    // Print documentation and exit with error status
    // unless there is exactly one program argument
    // left.

    if ( argc != 2 )
    {
	cout << "Wrong number of arguments."
	     << endl << endl;
        cout << documentation;
	exit (1);
    }

    // Open file.
    //
    const char * file = argv[1];
    ifstream in ( file );
    if ( ! in )
    {
        cout << "Cannot open " << file << endl;
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

    int line_number = 0;
    string name;
    while ( true )
    {
	++ line_number;
	getline ( in, name );
	if ( in.eof() ) break;

	// Read in file data.
	//
	drawing.erase
	    ( drawing.begin(), drawing.end() );
	while ( true )
	{
	    ++ line_number;
	    int c = in.peek();
	    if ( c == '*' )
	    {
	        in.get();
		if ( in.get() != '\n' )
		{
		    cout << "Bad line " << line_number
		         << endl;
		    exit ( 1 );
		}
		break;
	    }
	    else if ( c == EOF )
	    {
	        cout << "Premature end of file" << endl;
		exit ( 1 );
	    }
	    segment s;
	    in >> s.begin.x >> s.begin.y
	        >> s.end.x >> s.end.y;
	    if ( in.get() != '\n' )
	    {
		cout << "Bad line " << line_number
		     << endl;
		exit ( 1 );
	    }
	    drawing.push_back ( s );
	}

	// Compute bonding box for all points.
	//
	double xmin = DBL_MAX, xmax = - DBL_MAX;
	double ymin = DBL_MAX, ymax = - DBL_MAX;
	for ( int i = 0; i < drawing.size(); ++ i )
	{
	    if ( drawing[i].begin.x < xmin ) xmin =
	        drawing[i].begin.x;
	    if ( drawing[i].end.x > xmax ) xmax =
	        drawing[i].end.x;
	    if ( drawing[i].begin.y < ymin ) ymin =
	        drawing[i].begin.y;
	    if ( drawing[i].end.y > ymax ) ymax =
	        drawing[i].end.y;
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

		// big_node_size = window_big_node_size;
		// small_node_size =
		//     window_small_node_size;
		// wide_edge_size =
		//     window_wide_edge_size;
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

		// big_node_size = page_big_node_size;
		// small_node_size =
		//     page_small_node_size;
		// wide_edge_size = page_wide_edge_size;
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

#	    define CONVERT(p) \
		  graph_left \
		+ ((p).x - xmin) * xscale, \
		  graph_top + graph_height \
		- ((p).y - ymin) * yscale

	    // Display test case name.
	    //
	    cairo_text_extents_t te;
	    cairo_text_extents
		( title_c, name.c_str(), & te );
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
	    cairo_show_text ( title_c, name.c_str() );
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

	    // Draw edges.
	    //
	    for ( int i = 0; i < drawing.size(); ++ i )
	    {
		cairo_move_to
		    ( graph_c,
		      CONVERT(drawing[i].begin) );
		cairo_line_to
		    ( graph_c,
		      CONVERT(drawing[i].end) );
		cairo_set_line_width
		    ( graph_c, narrow_edge_size );
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
