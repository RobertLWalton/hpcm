// Display Points, Lines, and Arcs
//
// File:	hpcm_display.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Mon Jul  9 15:14:54 EDT 2018
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.

#include <iostream>
#include <iomanip>
#include <fstream>
#include <algorithm>
#include <string>
#include <sstream>
#include <cstdlib>
#include <cstdio>
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
using std::min;
using std::max;
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
"hpcm_display [-pdf|-RxC|-X] [-debug] [file]\n"
"\n"
"    This program displays line drawings defined\n"
"    in the given file or standard input.  The file\n"
"    consists of pages each consisting of a name line\n"
"    followed by command lines followed by a line\n"
"    containing just `*'.\n"
"\n"
"    The display commands are chosen from among the\n"
"    following, where + denotes one or more\n"
"    qualifiers, and lower case letters denote para-\n"
"    meters:\n"
"\n"
"        P+ x y\n"
"            Display a point at (x,y).  Qualifiers:\n"
"              S = small (default)\n"
"              M = medium\n"
"              L = large\n"
"              G|GG|GGG = color is light,medium,dark\n"
"                         gray (default is black)\n"
"\n"
"        L+ x1 y1 x2 y2\n"
"            Display a line from (x1,y1) to (x2,y2).\n"
"            Qualifiers:\n"
"              S = small width (default)\n"
"              M = medium width\n"
"              L = large width\n"
"              D = put dot on end(s)\n"
"              F = put forward arrow head at end(s)\n"
"              R = put rearward arrow head at end(s)\n"
"              B = put last dot or arrow head only at\n"
"                  beginning of oriented line\n"
"              E = put last dot or arrow head only at\n"
"                  end of oriented line\n"
"              G|GG|GGG = color is light,medium,dark\n"
"                         gray (default is black)\n"
"\n"
"        A+ x y xa ya r g1 g2\n"
"            Display elliptical arc centered at"
                                          " (x,y).\n"
"            The ellipse is first drawn with axes\n"
"            parallel to the x- and y-axes; with x\n"
"            semi-axis xa and y semi-axis ya.  Then\n"
"            the ellipse is rotated by angle r.  The\n"
"            angles g1 and g2 bound the arc ends\n"
"            before rotation by r.  Angles are in\n"
"            degrees. If g1 < g2 the arc goes\n"
"            counter-clock-wise; if g2 < g1 the arc\n"
"            goes clockwise. Any integer multiple of\n"
"            360 added to BOTH g1 and g2 does not\n"
"            affect the result.\n"
"\n"
"            For circular arc use a = b, r = 0.\n"
"            For entire ellipse use g1 = 0, g2 = 360.\n"
"            Qualifiers:\n"
"              S = small width (default)\n"
"              M = medium width\n"
"              L = large width\n"
"              D = put dot on end(s)\n"
"              F = put forward arrow head at end(s)\n"
"              R = put rearward arrow head at end(s)\n"
"              B = put last dot or arrow head only at\n"
"                  beginning of oriented arc\n"
"              E = put last dot or arrow head only at\n"
"                  end of oriented arc\n"
"              G|GG|GGG = color is light,medium,dark\n"
"                         gray (default is black)\n"
"\n"
"    With the -pdf option, pdf is written to\n"
"    the standard output.\n"
"\n"
"    With the -RxC option, pdf is written to the\n"
"    standard output as per the -pdf option, but with\n"
"    multiple testcase pages per physical paper page.\n"
"    Here R and C are very small integers, and there\n"
"    are R rows of C columns each of test cases on\n"
"    each physical page.\n"
"\n"
"    With the -X option, an X-window is opened and\n"
"    the first test case displayed.  Typing a car-\n"
"    riage return goes to the next test case, and\n"
"    typing control-C terminates the display.\n"
;

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

double operator * ( vector v1, vector v2 )
{
    return v1.x * v2.x + v1.y * v2.y;
}

// Rotate v by angle.
//
// WARING: ^ has lower precedence than + or -.
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
enum head { NEITHER = 0, BEGIN = 1, END = 2, BOTH = 3 };
enum color { BLACK = 0, DARK_GRAY = 1,
             MEDIUM_GRAY = 2, LIGHT_GRAY = 3 };
struct qualifiers
{
    width w;
    head dot;
    head forward;
    head rearward;
    color c;
};
struct point : public command
{
    vector p; // At p.
    qualifiers q;
};
struct line : public command
{
    vector p1, p2;  // From p1 to p2.
    qualifiers q;
};
struct arc : public command
{
    vector c;  // Center.
    vector a;  // (x axis, y axis )
    double r;
    vector g;  // (g1,g2)
    qualifiers q;
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
	    vector d1 = A.a;
	    vector d2 = { d1.x, - d1.y };
	    vector ll = - d1;
	    vector lr =   d2;
	    vector ur =   d1;
	    vector ul = - d2;
	    BOUND ( A.c + ( ll^A.r ) );
	    BOUND ( A.c + ( lr^A.r ) );
	    BOUND ( A.c + ( ur^A.r ) );
	    BOUND ( A.c + ( ul^A.r ) );
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
ostream & print_command_and_qualifiers
	( ostream & s, char command,
	  const qualifiers & q )
{
    s << command
      << ( q.w == SMALL ?  "S" :
           q.w == MEDIUM ? "M" :
           q.w == LARGE ?  "L" :
                           "W?" )
      << ( q.dot == NEITHER ? "" :
           q.dot == BEGIN ?   "DB" :
           q.dot == END ?     "DE" :
           q.dot == BOTH ?    "D" :
	                      "D?" )
      << ( q.forward == NEITHER ? "" :
           q.forward == BEGIN ?   "FB" :
           q.forward == END ?     "FE" :
           q.forward == BOTH ?    "F" :
	                          "F?" )
      << ( q.rearward == NEITHER ? "" :
           q.rearward == BEGIN ?   "RB" :
           q.rearward == END ?     "RE" :
           q.rearward == BOTH ?    "R" :
	                           "R?" )
      << ( q.c == BLACK ? "" :
           q.c == DARK_GRAY ?   "GGG" :
           q.c == MEDIUM_GRAY ? "GG" :
           q.c == LIGHT_GRAY ?  "G" :
	                        "G?" );
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
	return print_command_and_qualifiers
	       ( s, P.command, P.q )
	    << " " << P.p.x << " " << P.p.y;
    }
    case 'L':
    {
        line & L = * (line *) & c;
	return print_command_and_qualifiers
	       ( s, L.command, L.q )
	    << " " << L.p1.x << " " << L.p1.y
	    << " " << L.p2.x << " " << L.p2.y;
    }
    case 'A':
    {
        arc & A = * (arc *) & c;
	return print_command_and_qualifiers
	       ( s, A.command, A.q )
	    << " " << A.c.x << " " << A.c.y
	    << " " << A.a.x << " " << A.a.y
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

void read_qualifiers
    ( istream & in,
      qualifiers & q, bool heads_allowed = true )
{
    head * last_head = NULL;
    q.w = SMALL;
    q.dot = NEITHER;
    q.forward = NEITHER;
    q.rearward = NEITHER;
    q.c = BLACK;
    while ( ! isspace ( in.peek() ) )
    {
	int c = in.get();
	bool found = true;
	switch ( c )
	{
	case 'S': q.w = SMALL;
	          break;
	case 'M': q.w = MEDIUM;
	          break;
	case 'L': q.w = LARGE;
	          break;
	case 'G': q.c = (color) ( ( q.c + 3 ) % 4 );
	          break;
	default:  found = false;
	}
	if ( ! found && heads_allowed )
	{
	    found = true;
	    switch ( c )
	    {
	    case 'D': q.dot = BOTH;
		      last_head = & q.dot;
		      break;
	    case 'F': q.forward = BOTH;
		      last_head = & q.forward;
		      break;
	    case 'R': q.rearward = BOTH;
		      last_head = & q.rearward;
		      break;
	    case 'B':
	    case 'E':
		      if ( last_head == NULL )
			  cerr << "ERROR in line "
			       << line_number
			       << ": no preceeding"
			          " D, F, or R - `"
			       << (char) c
			       << "' ignored" << endl;
		      else if ( * last_head != BOTH )
			  cerr << "ERROR in line "
			       << line_number
			       << ": B and E conflict"
			          " - `"
			       << (char) c
			       << "' ignored" << endl;
		      else
			  * last_head =
			      ( c == 'B' ? BEGIN
			                 : END );
		      break;
	    default:  found = false;  
	    }
	}

	if ( ! found )
	    cerr << "ERROR in line " << line_number
		 << ": unknown qualifer `" << (char) c
		 << "' - ignored" << endl;
    }
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
	    read_qualifiers ( in, P.q, false );
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
	    read_qualifiers ( in, L.q );
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
	    read_qualifiers ( in, A.q );
	    in >> A.c.x >> A.c.y >> A.a.x >> A.a.y
	       >> A.r >> A.g.x >> A.g.y;
	    if ( in.good() )
	    {
	        if ( A.a.x <= 0 )
		    ERROR ( "x semi-axis < 0" )
	        if ( A.a.y <= 0 )
		    ERROR ( "y semi-axis < 0" )
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
		 << ": extra stuff at end of line"
		    " - ignored"
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
const double side_margin = 54;		    // 0.75"
const double separation = 8;		    // 8/72"
const double title_large_font_size = 16;    // 16/72"
const double title_small_font_size = 10;    // 10/72"

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

// PDF Options
//
int R = 1, C = 1;

// Parse -LBRxC and return true on success and false
// on failure.
//
bool pdfoptions ( const char * name )
{
    long R = 1, C = 1;
    while ( * name )
    {
	if ( '0' <= * name && * name <= '9' )
	{
	    char * endp;
	    R = strtol ( name, & endp, 10 );
	    if ( endp == name ) return false;
	    if ( R < 1 || R > 30 ) return false;
	    name = endp;
	    if ( * name ++ != 'x' ) return false;
	    C = strtol ( name, & endp, 10 );
	    if ( endp == name ) return false;
	    if ( C < 1 || C > 30 ) return false;
	    name = endp;
	}
	else return false;
    }
    ::R = R;
    ::C = C;
    return true;
}

// cairo_write_func_t to write data to cout.
//
cairo_status_t write_to_cout
    ( void * closure,
      const unsigned char * data, unsigned int length )
{
    cout.write ( (const char *) data, length );
    return CAIRO_STATUS_SUCCESS;
}

// Drawing data.
//
cairo_t * title_c;
double title_font_size,
       title_left, title_top, title_height, title_width,
       graph_top, graph_height,
       graph_left, graph_width;
cairo_t * graph_c;
double xscale, yscale, left, bottom;
double dot_size, line_size;

# define CONVERT(p) \
    left + ((p).x - xmin) * xscale, \
    bottom - ((p).y - ymin) * yscale

// Set color of graph_c.
//
void set_color ( color c )
{
    double rgb = 0.25 * c;
    cairo_set_source_rgb ( graph_c, rgb, rgb, rgb );
}

// Draw dot at position p with width w.
//
void draw_dot ( vector p, width w )
{
    cairo_arc
        ( graph_c, CONVERT(p), w * dot_size, 0, 2*M_PI);
    cairo_fill ( graph_c );
}

// Draw dot at position p with direction d and width w.
//
void draw_arrow ( vector p, vector d, width w )
{
    d = ( 1 / sqrt ( d * d ) ) * d;
    d = ( min ( xmax - xmin, ymax - ymin ) / 25 ) * d;
    vector d1 = d^45;
    vector d2 = d^(-45);
    cairo_move_to ( graph_c, CONVERT(p-d1) );
    cairo_line_to ( graph_c, CONVERT(p) );
    cairo_line_to ( graph_c, CONVERT(p-d2) );
    cairo_set_line_width ( graph_c, w * line_size );
    cairo_stroke ( graph_c );
}

void draw_test_case ( void )
{
    // Set up point scaling.  Insist on a margin
    // of 4 * line_size to allow lines to be
    // inside graph box.
    //
    double dx = xmax - xmin;
    double dy = ymax - ymin;
    if ( dx == 0 ) dx = 1;
    if ( dy == 0 ) dy = 1;
    xscale =
	( graph_width - 4 * line_size ) / dx;
    yscale =
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
    left = graph_left
	+ 0.5 * (   graph_width
		  - ( xmax - xmin ) * xscale );
    bottom = graph_top + graph_height
	- 0.5 * (   graph_height
		  - ( ymax - ymin ) * yscale );

    dout << "LEFT " << left
	 << " XSCALE " << xscale
	 << " BOTTOM " << bottom
	 << " YSCALE " << yscale
	 << endl;

    // Display test case name.
    //
    cairo_text_extents_t te;
    cairo_text_extents
	( title_c, testname.c_str(), & te );
    assert (    cairo_status ( title_c )
	     == CAIRO_STATUS_SUCCESS );
    cairo_move_to
	( title_c, 
	  title_left + title_width/2 - te.width/2,
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
	    set_color ( P.q.c );
	    draw_dot ( P.p, P.q.w );
	    break;
	}
	case 'L':
	{
	    line & L = * (line *) c;
	    set_color ( L.q.c );
	    cairo_move_to
		( graph_c,
		  CONVERT(L.p1) );
	    cairo_line_to
		( graph_c,
		  CONVERT(L.p2) );
	    cairo_set_line_width
		( graph_c,
		  L.q.w * line_size );
	    cairo_stroke ( graph_c );

	    if ( L.q.dot & BEGIN )
		draw_dot ( L.p1, L.q.w );
	    if ( L.q.dot & END )
		draw_dot ( L.p2, L.q.w );
	    if ( L.q.forward & BEGIN )
		draw_arrow
		    ( L.p1, L.p2 - L.p1,
		      L.q.w );
	    if ( L.q.forward & END )
		draw_arrow
		    ( L.p2, L.p2 - L.p1,
		      L.q.w );
	    if ( L.q.rearward & BEGIN )
		draw_arrow
		    ( L.p1, L.p1 - L.p2,
		      L.q.w );
	    if ( L.q.rearward & END )
		draw_arrow
		    ( L.p2, L.p1 - L.p2,
		      L.q.w );
	    break;
	}
	case 'A':
	{
	    arc & A = * (arc *) c;
	    set_color ( A.q.c );

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
		  A.a.x * xscale,
		  - A.a.y * yscale );

	    cairo_new_path ( graph_c );
	    cairo_arc
		( graph_c,
		  0, 0, 1,
		  min ( g1, g2 ),
		  max ( g1, g2 ) );

	    cairo_set_matrix
		( graph_c, & saved_matrix );
	    cairo_set_line_width
		( graph_c,
		  A.q.w * line_size );
	    cairo_stroke ( graph_c );

	    double s1 = sin ( g1 );
	    double c1 = cos ( g1 );
	    double s2 = sin ( g2 );
	    double c2 = cos ( g2 );
	    vector p1 =
		{ c1 * A.a.x,
		  s1 * A.a.y };
	    vector d1 =
		{ - s1 * A.a.x,
		    c1 * A.a.y };
	    vector p2 =
		{ c2 * A.a.x,
		  s2 * A.a.y };
	    vector d2 =
		{ - s2 * A.a.x,
		    c2 * A.a.y };
	    p1 = A.c + ( p1 ^ A.r );
	    p2 = A.c + ( p2 ^ A.r );
	    d1 = d1 ^ A.r;
	    d2 = d2 ^ A.r;

	    if ( A.q.dot & BEGIN )
		draw_dot ( p1, A.q.w );
	    if ( A.q.dot & END )
		draw_dot ( p2, A.q.w );
	    if ( A.q.forward & BEGIN )
		draw_arrow
		    ( p1, d1, A.q.w );
	    if ( A.q.forward & END )
		draw_arrow
		    ( p2, d2, A.q.w );
	    if ( A.q.rearward & BEGIN )
		draw_arrow
		    ( p1, - d1, A.q.w );
	    if ( A.q.rearward & END )
		draw_arrow
		    ( p2, - d2, A.q.w );
	}
	}
    }
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

        if (    strcmp ( "X", name ) == 0 )
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
	    // and exits.
	    //
	    FILE * out = popen ( "less -F", "w" );
	    fputs ( documentation, out );
	    pclose ( out );
	    exit ( 0 );
	}
        else if (    strcmp ( "pdf", name ) == 0
	             ||
		     pdfoptions ( name ) )
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
    title_c = cairo_create ( page );
    cairo_set_source_rgb ( title_c, 0.0, 0.0, 0.0 );
    cairo_select_font_face ( title_c, "sans-serif",
                             CAIRO_FONT_SLANT_NORMAL,
			     CAIRO_FONT_WEIGHT_BOLD );
    title_font_size =
        ( C == 1 ? title_large_font_size :
	           title_small_font_size );
    cairo_set_font_size ( title_c, title_font_size );
    assert (    cairo_status ( title_c )
	     == CAIRO_STATUS_SUCCESS );

    graph_c = cairo_create ( page );
    cairo_set_source_rgb ( graph_c, 0.0, 0.0, 0.0 );
    assert (    cairo_status ( graph_c )
	     == CAIRO_STATUS_SUCCESS );

    // Keep track of which control keys are pressed.
    //
    bool left_control_pressed = false;
    bool right_control_pressed = false;

    if ( display == NULL )
    {
	double case_width = page_width
	                  - 2 * side_margin
			  - ( C - 1 ) * separation;
	case_width /= C;
	double case_height =
	    page_height - top_margin
			- bottom_margin
			- ( R - 1 ) * separation; 
	case_height /= R;

	title_width = case_width;
	title_height = 2 * title_font_size;

	graph_height = case_height
	             - 2 * title_font_size;
	graph_width = case_width;

	line_size = page_line_size;
	dot_size = page_dot_size;

	int curR = 1, curC = 1;
	while ( read_testcase
		    ( file != NULL ?
		      * (istream *) & in :
		      cin ) )
	{
	    title_top = top_margin
	              + case_height * ( curR - 1 )
	              + separation * ( curR - 1 );
	    title_left = side_margin
	               + case_width * ( curC - 1 )
	               + separation * ( curC - 1 );

	    graph_top = top_margin
		      + case_height * ( curR - 1 )
	              + separation * ( curR - 1 )
		      + 2 * title_font_size;
	    graph_left = side_margin
	    	       + case_width * ( curC - 1 )
	               + separation * ( curC - 1 );

	    compute_bounds();
	    draw_test_case();
	    if ( ++ curC > C )
	    {
		curC = 1;
		if ( ++ curR > R )
		{
		    curR = 1;
		    cairo_show_page ( title_c );
		}
	    }
	}
	if ( curR != 1 || curC != 1 )
	    cairo_show_page ( title_c );
    }
    else // if ( display != NULL )
    {
	while ( read_testcase
		    ( file != NULL ?
		      * (istream *) & in :
		      cin ) )
	{
	    compute_bounds();

	    while ( true )
	    {
		double foot_top;

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
		title_left = 0;

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

		draw_test_case();

		// Display foot for X-windows.
		//
		cairo_text_extents_t te;
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

		cairo_show_page ( title_c );

		while ( true )
		{
		    XEvent e;
		    XNextEvent ( display, & e );
		    if ( e.type == KeyPress )
		    {
			KeySym key =
			    XLookupKeysym
			        ( & e.xkey, 0 );
			if ( key == XK_Control_L )
			    left_control_pressed =
			        true;
			else if ( key == XK_Control_R )
			    right_control_pressed =
			        true;
			else
			if ( key == XK_c
			     &&
			     ( left_control_pressed
			       ||
			       right_control_pressed ) )
			    goto PROGRAM_DONE;
			else
			    goto PAGE_DONE;
			// Go to next test case.
		    }
		    else if ( e.type == KeyRelease )
		    {
			KeySym key =
			    XLookupKeysym
			        ( & e.xkey, 0 );
			if ( key == XK_Control_L )
			    left_control_pressed =
			        false;
			else if ( key == XK_Control_R )
			    right_control_pressed =
			        false;
		    }
		    if ( e.type == Expose
			 &&
			 e.xexpose.count == 0 )
			break;
			// Redraw current window.
		}
	    }
	PAGE_DONE:;
	}
    }

    PROGRAM_DONE:

    cairo_destroy ( title_c );
    cairo_destroy ( graph_c );
    cairo_surface_destroy ( page );

    // Return from main function without error.

    return 0;
}
