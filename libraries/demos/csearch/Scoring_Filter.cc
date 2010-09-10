// Scoring Filter for the Constrained Search (Graph
// Coloring) Problem
//
// File:	Scoring_Filter.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Thu Sep  9 23:55:12 EDT 2010
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: walton $
//   $Date: 2010/09/10 03:57:46 $
//   $RCSfile: Scoring_Filter.cc,v $
//   $Revision: 1.2 $

// Scoring_Filter csearch.in \
//		  < csearch.out \
//                > csearch.fout
//
// The .fout file copies lines from the .out file, but
// replaces each set of correct solutions for a test
// case by `SUCCESS'.  For an incorrect solution error
// messages are output identifying the error and
// and the offending solution line is also printed.
//
// Any non-test-case-name line not containing just
// color names is simply copied to the output (it could
// be a `No solution' line).  If the first non-white-
// space character is `-' it is treated as a test case
// name line.  Any bad test case name causes all
// subsequent lines to be copied to the output.

#define SCORING_FILTER
#include "csearch.cc"
#include <fstream>
using std::ifstream;
using std::cerr;

// Test case name from csearch.in
//
char test_case_name[MAX_LINE+2];

// Next line from cin (csearch.out)
//
char out_line[MAX_LINE+2];

// Number of successful and failed solutions output
// for the current test case, and number of non-solu-
// tion non-test-case-name lines.
//
int successes;
int failures;
int non_solution_lines;

// Given an line in outline, compute color[i] for
// each node.  If the line does not contain n color
// names, return false.  Otherwise return true.
//
bool compute_colors ( void )
{
    if ( strlen ( out_line ) != n ) return false;

    FOR(i,n)
    {
	int cfound = -1;
        FOR(c,m)
	{
	    if ( color_name[c] == out_line[i] )
	    {
	        cfound = c;
		break;
	    }
	}
	if ( cfound == -1 ) return false;
	color[i] = cfound;
    }
    return true;
}


int main ( int argc, char ** argv )
{
    debug = ( argc > 2 );

    ifstream in ( argv[1] );
    if ( ! in )
    {
        cerr << "Cannot open " << argv[1] << endl;
	exit ( 1 );
    }

    // We read one .out line ahead.
    //
    cin.getline ( out_line, MAX_LINE+2 );

    while ( true )
    {
        // Read test case input.

	in.getline ( test_case_name, MAX_LINE+2 );
	if ( in.eof() ) break;
	assert ( in.gcount() <= MAX_LINE + 1 );

	read_data ( in );

	// Read .out lines until we get a test
	// case name line.
	//
	char * p;
	while ( ! cin.eof() )
	{
	    p = out_line;
	    while ( isspace ( * p ) ) ++ p;
	    if ( * p == '-' ) break;
	    cout << out_line << endl;
	    cin.getline ( out_line, MAX_LINE+2 );
	}

	if (    strncmp ( test_case_name, p,
	                  strlen ( test_case_name ) )
	     != 0 )
	    break; 
	cout << out_line << endl;
	cin.getline ( out_line, MAX_LINE+2 );

	successes = failures =
	            non_solution_lines = 0;

	// Loop to handle multiple solutions.
	//
	while ( ! cin.eof() )
	{
	    if ( compute_colors() )
	    {
	        if ( check_legality() )
		    ++ successes;
		else
		{
		    ++ failures;
		    cout << "SOLUTION THAT FAILED AS"
		            " PER ABOVE:" << endl
			 << out_line << endl;
		}
	    }
	    else
	    {
	        // Check for name line.
		char * p = out_line;
		while ( isspace ( * p ) ) ++ p;
		if ( * p == '-' ) break;
		cout << out_line << endl;
	    }
	    cin.getline ( out_line, MAX_LINE+2 );
	}

	if ( failures == 0 && successes > 0 )
	    cout << "SUCCESS" << endl;
	else if ( failures > 0 )
	    cout << successes << " SUCCESSES AND "
		 << failures << " FAILURES AND "
		 << non_solution_lines
		 << " NON-SOLUTION LINES" << endl;
    }

    // Copy any remaining .out file lines.
    //
    while ( ! cin.eof() )
    {
	if ( cin.eof() ) break;
	cout << out_line << endl;
	cin.getline ( out_line, MAX_LINE+2 );
    }

    return 0;
}
