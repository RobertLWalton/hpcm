// Display Points, Lines, and Arcs
//
// File:	hpcm_display.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Sat Jul  8 03:25:29 EDT 2017
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.

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
using std::cerr;
using std::cin;
using std::ws;
using std::istream;
using std::ostream;
using std::ifstream;
using std::string;

extern "C" {
#include <cairo-pdf.h>
#include <cairo-xlib.h>
#define XK_MISCELLANY
#define XK_LATIN1
#include <X11/keysymdef.h>
}

bool debug = false;
# define dout if ( debug ) cerr

const char * const documentation = "\n"
"hpcm_display [-pdf|-X] [file]\n"
"\n"
"    This program displays line drawings defined\n"
"    in the given file or standard input.  The file\n"
"    consists of pages each consisting of a name line\n"
"    followed by command lines followed by a line\n"
"    containing just `*'.\n"
"\n"
"    The display commands are chosen from among the\n"
"    following, where * denotes one or more\n"
"    qualifiers, and lower case letters denote para-\n"
"    meters:\n"
"\n"
"        P* x y\n"
"            Display a point at (x,y).  Qualifiers:\n"
"                S = small (default)\n"
"                M = medium\n"
"                L = large\n"
"\n"
"        L* x1 y1 x2 y2\n"
"            Display a line from (x1,y1) to (x2,y2).\n"
"            Qualifiers:\n"
"                S = small width (default)\n"
"                M = medium width\n"
"                L = large width\n"
"                D = put dots on ends\n"
"                A = put arrow head at (x2,y2) end\n"
"\n"
"        A* x y a b r g1 g2\n"
"            Display elliptical arc centered at (x,y)\n"
"            with ellipse major axis a and minor axis\n"
"            b, all rotated by angle r.  The angles\n"
"            g1 and g2 bound the arc ends before rota-\n"
"            tion by r.  Angles are in degrees\n"
"            counter-clockwise.\n"
"\n"
"            For circular arc use a = b, r = 0.\n"
"            For entire ellipse use g1 = 0, g2 = 360.\n"
"            Qualifiers:\n"
"                S = small width (default)\n"
"                M = medium width\n"
"                L = large width\n"
"                D = put dots on ends\n"
"                A = put arrow head at g2 end\n"
"\n"
"    With the -pdf option, pdf is written to\n"
"    the standard output.\n"
"\n"
"    With the -X option, an X-window is opened and\n"
"    the first test case displayed.  Typing a car-\n"
"    riage return goes to the next test case, and\n"
"    typing control-C terminates the display.\n";

// Vectors:
//
struct vector { double x, y; };

vector operator + ( vector v1, vector v2 )
{
    vector r = { v1.x + v2.x, v1.y + v2.y };
    return r;
}

vector operator - ( vector v1, vector v2 )
{
    vector r = { v1.x - v2.x, v1.y - v2.y };
    return r;
}

vector operator - ( vector v )
{
    vector r = { - v.x, - v.y };
    return r;
}

vector operator * ( double s, vector v )
{
    vector r = { s * v.x, s * v.y };
    return r;
}

// Rotate v by angle.
//
vector operator ^ ( vector v, double angle )
{
    double s = sin ( M_PI * angle / 180 );
    double c = cos ( M_PI * angle / 180 );
    vector r = { c * v.x - s * v.y,
                 s * v.x + c * v.y };
    return r;
}

// Current test case data.
//
string testname;

struct command { command * next; char command; };
enum width { SMALL = 1, MEDIUM = 2, LARGE = 3 };
struct point : public command
{
    vector p; // At p.
    width w;
};
struct line : public command
{
    vector p1, p2;  // From p1 to p2.
    width w;
    bool dotted;
    bool arrow;
};
struct arc : public command
{
    vector c;  // Center.
    vector m;  // (major axis, minor axis)
    double r;
    vector g;  // (g1,g2)
    width w;
    bool dotted;
    bool arrow;
};

// List of all commands:
//
command * commands;

double xmin, xmax, ymin, ymax;
    // Bounds on x and y over all commands.
    // Used to set scale.  Does NOT account
    // for width of lines or points.  Bounds
    // entire ellipse and not just the arc.

void compute_bounds ( void )
{
    xmin = ymin = DBL_MAX;
    xmax = ymax = DBL_MIN;
#   define BOUND(v) \
	 dout << "BOUND " << (v).x << " " << (v).y \
	      << endl; \
         if ( (v).x < xmin ) xmin = (v).x; \
         if ( (v).x > xmax ) xmax = (v).x; \
         if ( (v).y < ymin ) ymin = (v).y; \
         if ( (v).y > ymax ) ymax = (v).y;

    for ( command * c = commands; c != NULL;
                                  c = c->next )
    {
        switch ( c->command )
	{
	case 'P':
	{
	    point & P = * (point *) c;
	    BOUND ( P.p );
	    break;
	}
	case 'L':
	{
	    line & L = * (line *) c;
	    BOUND ( L.p1 );
	    BOUND ( L.p2 );
	    break;
	}
	case 'A':
	{
	    // Compute bounding rectangle of ellipse,
	    // rotate it by A.r, translate it by A.c,
	    // and bound the corners.
	    //
	    arc & A = * (arc *) c;
	    vector d1 = 0.5 * A.m;
	    vector d2 = { d1.x, - d1.y };
	    vector ll = - d1;
	    vector lr =   d2;
	    vector ur =   d1;
	    vector ul = - d2;
	    BOUND ( A.c + ll^A.r );
	    BOUND ( A.c + lr^A.r );
	    BOUND ( A.c + ur^A.r );
	    BOUND ( A.c + ul^A.r );
	    break;
	}
	default:
	    assert ( ! "bounding bad command" );
	}
    }
#   undef BOUND

    dout <<  "XMIN " << xmin << " XMAX " << xmax
         << " YMIN " << ymin << " YMAX " << ymax
         << endl; 
}

// Delete first command.
//
void delete_command ( void )
{
    if ( commands == NULL ) return;
    command * next = commands->next;

    switch ( commands->command )
    {
    case 'P':
	delete (point *) commands;
	break;
    case 'L':
	delete (line *) commands;
	break;
    case 'A':
	delete (arc *) commands;
	break;
    default:
	assert ( ! "deleting bad command" );
    }
    commands = next;
}

// Print command for debugging.
//
ostream & print_command_and_options
	( ostream & s, char command, width w,
	  bool dotted = false, bool arrow = false )
{
    s << command
      << ( w == SMALL ? "S" :
           w == MEDIUM ? "M" :
           w == LARGE ? "L" :
                        "W?" )
      << ( dotted ? "D" : "" )
      << ( arrow ? "A" : "" );
    return s;
}
//
ostream & operator << ( ostream & s, const command & c )
{
    switch ( c.command )
    {
    case 'P':
    {
        point & P = * (point *) & c;
	return print_command_and_options
	       ( s, P.command, P.w )
	    << " " << P.p.x << " " << P.p.y;
    }
    case 'L':
    {
        line & L = * (line *) & c;
	return print_command_and_options
	       ( s, L.command, L.w, L.dotted, L.arrow )
	    << " " << L.p1.x << " " << L.p1.y
	    << " " << L.p2.x << " " << L.p2.y;
    }
    case 'A':
    {
        arc & A = * (arc *) & c;
	return print_command_and_options
	       ( s, A.command, A.w, A.dotted, A.arrow )
	    << " " << A.c.x << " " << A.c.y
	    << " " << A.m.x << " " << A.m.y
	    << " " << A.r
	    << " " << A.g.x << " " << A.g.y;
    }
    default:
        return s << "BAD COMMAND " << c.command;
    }
}

// Read test case.  Return true if read and false if
// end of file.
//
int line_number = 0;
void bad_modifier ( int c )
{
    cerr << "ERROR in line " << line_number
         << ": bad modifier `" << (char) c
	 << "' - ignored" << endl;
}
void skip ( istream & in )
{
    int c;
    while ( c = in.peek(),
            isspace ( c ) && c != '\n' )
        in.get();
}

bool read_testcase ( istream & in )
{
    ++ line_number;
    getline ( in, testname );
    if ( in.eof() ) return false;

    while ( commands != NULL )
        delete_command();
        
    bool done = false;
    while ( ! done )
    {
        if ( in.eof() )
	{
	    cerr << "ERROR after line " << line_number
	         << "; unexpected end of file"
		    " - `*' inserted" << endl;
	    break;
	}
	   
	++ line_number;
	int errors = 0;
#	define ERROR(s) { \
	    cerr << "ERROR in line " << line_number \
	         << ": " << s << " - line ignored" \
		 << endl; \
	    ++ errors; \
	    }

	switch ( in.peek() )
	{
	case 'P':
	{
	    point & P = * new point();
	    P.next = commands;
	    commands = & P;
	    in >> P.command;
	    assert ( P.command == 'P' );
	    P.w = SMALL;
	    while ( ! isspace ( in.peek() ) )
	    {
		int c = in.get();
	        switch ( c )
		{
		case 'S': P.w = SMALL; break;
		case 'M': P.w = MEDIUM; break;
		case 'L': P.w = LARGE; break;
		default:  bad_modifier ( c );
		}
	    }
	    in >> P.p.x >> P.p.y;
	    break;
	}
	case 'L':
	{
	    line & L = * new line();
	    L.next = commands;
	    commands = & L;
	    in >> L.command;
	    assert ( L.command == 'L' );
	    L.w = SMALL;
	    L.arrow = false;
	    while ( ! isspace ( in.peek() ) )
	    {
		int c = in.get();
	        switch ( c )
		{
		case 'S': L.w = SMALL; break;
		case 'M': L.w = MEDIUM; break;
		case 'L': L.w = LARGE; break;
		case 'D': L.dotted = true; break;
		case 'A': L.arrow = true; break;
		default:  bad_modifier ( c );
		}
	    }
	    in >> L.p1.x >> L.p1.y >> L.p2.x >> L.p2.y;
	    break;
	}
	case 'A':
	{
	    arc & A = * new arc();
	    A.next = commands;
	    commands = & A;
	    in >> A.command;
	    assert ( A.command == 'A' );
	    A.w = SMALL;
	    A.arrow = false;
	    while ( ! isspace ( in.peek() ) )
	    {
		int c = in.get();
	        switch ( c )
		{
		case 'S': A.w = SMALL; break;
		case 'M': A.w = MEDIUM; break;
		case 'L': A.w = LARGE; break;
		case 'D': A.dotted = true; break;
		case 'A': A.arrow = true; break;
		default:  bad_modifier ( c );
		}
	    }
	    in >> A.c.x >> A.c.y >> A.m.x >> A.m.y
	       >> A.r >> A.g.x >> A.g.y;
	    if ( in.good() )
	    {
	        if ( A.m.x <= 0 )
		    ERROR ( "minor axis < 0" )
		if ( A.m.x < A.m.y )
		    ERROR ( "major axis < minor axis" )
	    }
	    break;
	}
	case '*':
	{
	    assert ( in.get() == '*' );
	    done = true;
	    break;
	}
	default:
	{
	    cerr << "ERROR in line " << line_number
	         << "; cannot understand command `"
		 << (char) in.peek()
		 << "' line ignored" << endl;
	    string line;
	    getline ( in, line );
	    continue;
	}
	}
#       undef ERROR

	if ( ! in.good() )
	{
	    cerr << "ERROR in line " << line_number
		 << ": line - ignored"
		 << endl;
	    delete_command();

	    in.clear();
	    string extra;
	    getline ( in, extra );
	}
	else if ( errors > 0 )
	{
	    delete_command();

	    in.clear();
	    string extra;
	    getline ( in, extra );
	}
	else if ( skip ( in ), in.get() != '\n' )
	{
	    cerr << "ERROR in line " << line_number
		 << ": extra stuff at end of line - ignored"
		 << endl;
	    string extra;
	    getline ( in, extra );
	    cerr << "STUFF: " << extra << endl;
	}
    }
    return true;
}

// For pdf output, units are 1/72".

// You MUST declare the entire paper size, 8.5x11",
// else you get a non-centered printout.

const double page_height = 11*72;	    // 11.0"
const double page_width = 8*72 + 72/2;	    // 8.5"

const double top_margin = 36;		    // 0.5"
const double bottom_margin = 36;	    // 0.5"
const double side_margin = 72;		    // 1.0"
const double page_title_height = 72+36;	    // 1.5"
const double title_font_size = 16;	    // 16/72"

const double page_line_size = 1;    	    // 1/72"
const double page_dot_size = 2;    	    // 2/72"

const double print_box = page_width - 2 * side_margin;

// For X-Windows, units are 1 pixel.

const int window_height = 700;
const int window_title_height = 50;
const int window_foot_height = 50;
const int window_width = 600;
const char window_foot[] =
    "Type SPACE for next page, control-C to quit.";

const int window_line_size = 2;
const int window_dot_size = 3;

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

        if (    strcmp ( "pdf", name ) == 0 )
	{
	    if ( page != NULL )
	    {
		cout << "At most one -pdf or -X option"
			" allowed"
		     << endl;
		exit (1);
	    }
	    page = cairo_pdf_surface_create_for_stream
	    		( write_to_cout, NULL,
			  page_width, page_height );
	}
        else if (    strcmp ( "X", name ) == 0 )
	{
	    if ( page != NULL )
	    {
		cout << "At most one -pdf or -X option"
			" allowed"
		     << endl;
		exit (1);
	    }
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
        else if ( strncmp ( "deb", name, 3 ) == 0 )
	    debug = true;
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
	    cout << "Cannot understand -" << name
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

    if ( argc > 2 )
    {
	cout << "Wrong number of arguments."
	     << endl << endl;
        cout << documentation;
	exit (1);
    }

    // Open file.
    //
    ifstream in;
    const char * file = NULL;
    if ( argc == 2 )
    {
        file = argv[1];
	in.open ( file );
	if ( ! in )
	{
	    cout << "Cannot open " << file << endl;
	    exit ( 1 );
	}
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
    assert (    cairo_status ( graph_c )
	     == CAIRO_STATUS_SUCCESS );

    // Keep track of which control keys are pressed.
    //
    bool left_control_pressed = false;
    bool right_control_pressed = false;

    while ( read_testcase
                ( file != NULL ? * (istream *) & in :
		               cin ) )
    {
        compute_bounds();

	while ( true )
	{
	    double title_top, title_height, title_width,
	           graph_top, graph_height,
		   graph_left, graph_width,
		   line_size, dot_size, foot_top;

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

		foot_top = height
		         - window_foot_height;

		graph_top = window_title_height;
		graph_height = height
			     - window_title_height
			     - window_foot_height;
		graph_left  = 0;
		graph_width = width;

		line_size = window_line_size;
		dot_size = window_dot_size;
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

		line_size = page_line_size;
		dot_size = page_dot_size;
	    }

	    // Set up point scaling.  Insist on a margin
	    // of 4 * line_size to allow lines to be
	    // inside graph box.
	    //
	    double dx = xmax - xmin;
	    double dy = ymax - ymin;
	    if ( dx == 0 ) dx = 1;
	    if ( dy == 0 ) dy = 1;
	    double xscale =
	        ( graph_width - 4 * line_size ) / dx;
	    double yscale =
	        ( graph_height - 4 * line_size ) / dy;

	    // Make the scales the same.
	    //
	    if ( xscale > yscale )
	        xscale = yscale;
	    else if ( xscale < yscale )
	        yscale = xscale;

	    // Compute left and bottom of graph so as
	    // to center graph.
	    //
	    double left = graph_left
	        + 0.5 * (   graph_width
		          - ( xmax - xmin ) * xscale );
	    double bottom = graph_top + graph_height
	        - 0.5 * (   graph_height
		          - ( ymax - ymin ) * yscale );

	    dout << "LEFT " << left
	         << " XSCALE " << xscale
	         << " BOTTOM " << bottom
	         << " YSCALE " << yscale
		 << endl;

#	    define CONVERT(p) \
		  left + ((p).x - xmin) * xscale, \
		  bottom - ((p).y - ymin) * yscale

	    // Display test case name.
	    //
	    cairo_text_extents_t te;
	    cairo_text_extents
		( title_c, testname.c_str(), & te );
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
	        ( title_c, testname.c_str() );
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
		        foot_top
		      +
		      title_font_size
		      +   (   window_foot_height
		            - title_font_size )
			/ 2 );
		cairo_show_text
		    ( title_c, window_foot );
		assert (    cairo_status ( title_c )
			 == CAIRO_STATUS_SUCCESS );
	    }

	    // Execute drawing commands.
	    //
	    for ( command * c = commands; c != NULL;
	                                  c = c->next )
	    {
		dout << "EXECUTE " << * c << endl;
	        switch ( c->command )
		{
		case 'P':
		{
		    point & P = * (point *) c;
		    cairo_arc
		        ( graph_c,
			  CONVERT(P.p),
			  P.w * dot_size,
			  0, 2*M_PI);
		    cairo_fill ( graph_c );
		    break;
		}
		case 'L':
		{
		    line & L = * (line *) c;
		    cairo_move_to
			( graph_c,
			  CONVERT(L.p1) );
		    cairo_line_to
			( graph_c,
			  CONVERT(L.p2) );
		    cairo_set_line_width
			( graph_c,
			  L.w * line_size );
		    cairo_stroke ( graph_c );

		    if ( L.dotted )
		    {
			cairo_arc
			    ( graph_c,
			      CONVERT(L.p1),
			      L.w * dot_size,
			      0, 2*M_PI);
			cairo_fill ( graph_c );
			cairo_arc
			    ( graph_c,
			      CONVERT(L.p2),
			      L.w * dot_size,
			      0, 2*M_PI);
			cairo_fill ( graph_c );
		    }
		    break;
		}
		case 'A':
		{
		    arc & A = * (arc *) c;

		    double g1 = M_PI * A.g.x / 180;
		    double g2 = M_PI * A.g.y / 180;
		    double r  = M_PI * A.r   / 180;

		    cairo_matrix_t saved_matrix;
		    cairo_get_matrix
		        ( graph_c, & saved_matrix );
		    cairo_translate
		        ( graph_c, CONVERT(A.c) );
		    cairo_rotate
		        ( graph_c, - r );
		    cairo_scale
		        ( graph_c,
			  0.5 * A.m.x * xscale,
			  - 0.5 * A.m.y * yscale );

		    cairo_new_path ( graph_c );
		    cairo_arc
			( graph_c,
			  0, 0,
			  1, g1, g2 );

		    cairo_set_matrix
		        ( graph_c, & saved_matrix );
		    cairo_set_line_width
			( graph_c,
			  A.w * line_size );
		    cairo_stroke ( graph_c );

		    if ( A.dotted )
		    {
			double s1 = sin ( g1 );
			double c1 = cos ( g1 );
			double s2 = sin ( g2 );
			double c2 = cos ( g2 );
			vector p1 =
			    { 0.5 * c1 * A.m.x,
			      0.5 * s1 * A.m.y };
			vector p2 =
			    { 0.5 * c2 * A.m.x,
			      0.5 * s2 * A.m.y };
			p1 = A.c + p1 ^ r;
			p2 = A.c + p2 ^ r;

			cairo_arc
			    ( graph_c,
			      CONVERT(p1),
			      A.w * dot_size,
			      0, 2*M_PI);
			cairo_fill ( graph_c );
			cairo_arc
			    ( graph_c,
			      CONVERT(p2),
			      A.w * dot_size,
			      0, 2*M_PI);
			cairo_fill ( graph_c );
		    }
		}
		}
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
