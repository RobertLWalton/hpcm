// Solution to the Constrained Search Problem
//
// File:	constrainedsearch.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Sun Sep  5 04:16:00 EDT 2010
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: walton $
//   $Date: 2010/09/06 01:50:06 $
//   $RCSfile: csearch.cc,v $
//   $Revision: 1.1 $

#include <iostream>
#include <cstdlib>
#include <cassert>
// You could use `using namespace std;' instead of the
// following.
using std::cin;
using std::cout;
using std::endl;

const int MAX_N = 80;
const int MAX_M = 26;
const int MAX_LINE = 80;

bool debug = false;
#define dout if ( debug ) cout
#define FOR(i,n) for ( int i = 0; i < n; ++ i )

// Input data.
//
//   n	        	Number of nodes.
//			Node indices i, j are 0 .. n-1.
//   m			Number of colors.
//			Color indices c are 0  .. m-1.
//   color_name[c]	1-character naming color c.
//   color[i]		Color index of node i or UNKNOWN
//			if the color is unknown.
//   connected[i][j]	True if and only if node i is
//			connected to node j.
int n;
int m;
char color_name[MAX_M];
int color[MAX_N];
const int UNKNOWN = -1;
bool connected[MAX_N][MAX_N];

// We use constrained search.

int number_of_solutions;

// allowed[i] is the number of colors allowed for
// node i, if color[i] == UNKNOWN.  The search picks
// a node with minimum allowed[i] to set the color of
// next.  allowed[i] is recomputed by propagation; if
// it is 0 propagation fails, and if it is 1 propagation
// sets the color of node i.
//
int allowed[MAX_N];

// An action is setting color[i] to c, or setting
// allowed[i] to a new value.  The previous value of
// color[i] is always UNKNOWN.
//
enum action_type { SET_COLOR, SET_ALLOWED };
//
// The actions are organized as a stack.  Actionsp
// points just after the last action in the stack.
// We assert ( actionsp <= actions_endp ).
//
// There can be at most n set color actions and at most
// n**2 SET_ALLOWED actions.
//
struct action
{
    action_type type;
    int i;
    int old_value;
} actions[MAX_N + MAX_N * MAX_N],
  * actionsp,
  * actions_endp = actions + MAX_N;

// Set color[i]= c.  Old_value == UNKNOWN always.
//
void set_color ( int i, int c )
{
    dout << "SET COLOR[" << i << "] = " << c << endl;
    assert ( actionsp < actions_endp );
    actionsp->type = SET_COLOR;
    actionsp->i = i;
    color[i] = c;
}

// Set allowed[i] = value.
//
void set_allowed ( int i, int value )
{
    dout << "SET ALLOWED[" << i << "] = " << value
         << endl;
    assert ( actionsp < actions_endp );
    actionsp->type = SET_ALLOWED;
    actionsp->i = i;
    actionsp->old_value = allowed[i];
    allowed[i] = value;
}

// Undo actions until actionsp == ap.
//
void undo ( action * ap )
{
    assert ( ap >= actions );
    while ( actionsp > ap )
    {
        -- actionsp;
	int i = actionsp->i;
	switch ( actionsp->type )
	{
	case SET_COLOR:
	    color[i] = UNKNOWN;
	    break;
	case SET_ALLOWED:
	    allowed[i] = actionsp->old_value;
	}
    }
}

// Given a node j compute used[c] == true if and only
// if node j has a neighbor with color c, and compute
// new_allowed == m - # c with used[c] == true.
//
void compute_allowed
	( int j, bool used[MAX_M], int & new_allowed )
{

    FOR(c,m) used[c] = false;
    new_allowed = m;
    FOR(k,n)
    {
	if ( ! connected[k][j] ) continue;
	int c = color[k];
	if ( c != UNKNOWN && ! used[c] )
	{
	    used[c] = true;
	    -- new_allowed;
	}
    }
}

// Propagate the effects of actions from ap to
// actionsp.  Return true if no conflicts discovered
// and false if a conflict is discovered.  In the
// later case the propagation is left unfinished.
//
// For each SET_COLOR action propagate recomputes
// allowed[j] for all neighbors j of the node whose
// color was set.  If a new allowed[j] == 0 a conflict
// is signalled.  If a new allowed[j] == 1 the color
// of j is set (it is forced).
//
bool propagate ( action * ap )
{
    for ( ; ap < actionsp; ++ ap )
    {
	if ( ap->type != SET_COLOR ) continue;

	FOR(j,n)
	{
	    if ( ! connected[ap->i][j] ) continue;
	    if ( color[j] != UNKNOWN ) continue;

	    // Set used[c] true if a neighbor of node j
	    // has color c.  Set new_allowed to new
	    // value of allowed[j].
	    //
	    bool used[MAX_M];
	    int new_allowed;
	    compute_allowed ( j, used, new_allowed );

	    if ( new_allowed == allowed[j] ) continue;

	    if ( new_allowed == 0 ) return false;

	    if ( new_allowed > 1 )
	    {
		set_allowed ( j, new_allowed );
		continue;
	    }

	    int c;
	    for ( c = 0; c < m; ++ c )
	    {
	        if ( ! used[c] ) break;
	    }
	    assert ( c < m );

	    set_color ( j, c );
	}
    }
}

// Continue Search.  Find a node i with minimum
// allowed[i] and try all its allowed colors.
//
void search ( int depth )
{

    // Find node i with color[i] == UNKNOWN and
    // minimum allowed[i].
    //
    int i = -1;
    FOR(j,n)
    {
        if ( color[j] != UNKNOWN ) continue;
	if ( i == -1 ) i = j;
	else if ( allowed[i] > allowed[j] ) i = j;
    }

    if ( i == -1 )
    {
        // We are done successfully.
	//
	// print result.
	//
	FOR(i,n) cout << color_name[color[i]];
	cout << endl;

	++ number_of_solutions;
	return;
    }

    bool used[MAX_M];
    int new_allowed;
    compute_allowed ( i, used, new_allowed );
    assert ( new_allowed == allowed[i] );

    action * saved_actionsp = actionsp;

    FOR(c,m)
    {
        if ( used[c] ) continue;
	set_color ( i, c );
	if ( propagate ( saved_actionsp ) )
	    search ( depth + 1 );
	undo ( saved_actionsp );
    }
}

char line [MAX_LINE + 2];

// Judge's function to get line checking for line
// too long.  Return false on eof, true otherwise.
//
bool get_line ( void )
{
    cin.getline ( line, MAX_LINE+2 );
    if ( cin.eof() ) return false;
    assert ( cin.gcount() <= MAX_LINE );
    return true;
}

int main ( int argc, char * argv[] )
{
    debug = ( argc > 1 );

    while ( true )
    {
	// Read and output test case name line.
	//
        if ( ! get_line() ) break;
	cout << line << endl;

	// Read colors names.
	//
	assert ( get_line() );
	m = strlen ( line );
	assert ( 1 <= m && m <= MAX_M );
	strncpy ( color_name, line, m );

	// Read initial colors.
	//
	actionsp = actions;
	//
	assert ( get_line() );
	n = strlen ( line );
	assert ( 1 <= n && n <= MAX_N );
	FOR(i,n)
	{
	    if ( line[i] == '?' )
	        color[i] = UNKNOWN;
	    else FOR(c,m)
	    {
	        if ( line[i] == color_name[c] )
		{
		    set_color ( i, c );
		    break;
		}
	    }
	}

	// Read connected[i][j].
	//
	FOR(i,n)
	{
	    assert ( get_line() );
	    assert ( strlen ( line ) == n );
	    FOR(j,n)
	    {
	        if ( line[j] == '.' )
		    connected[i][j] = false;
		else
		{
		    assert ( line[j] == '*' );
		    connected[i][j] = true;
		}
	    }
	}

	// Compute allowed[i].
	//
	bool used[MAX_M];
	FOR(i,n)
	    compute_allowed ( i, used, allowed[i] );

	// Check legality and propagate initial color
	// settings.
	//
	assert ( propagate ( actions ) );

	// Search
	//
	number_of_solutions = 0;
	search ( 0 );

	if ( number_of_solutions = 0 )
	    cout << "no solutions" << endl;
    }

    return 0;
}

