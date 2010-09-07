// Generate Constrained Search (Graph Coloring) Problem
// Data
//
// File:     make_constrainedsearch_input.cc
// Author:   Bob Walton <walton@seas.harvard.edu>
// Date:     Mon Sep  6 16:12:59 EDT 2010
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: walton $
//   $Date: 2010/09/07 09:38:31 $
//   $RCSfile: make_csearch_input.cc,v $
//   $Revision: 1.1 $

// Input:
//
//	For each case, a line with the test case name,
//	followed by one line with the numbers:
//
//		n m i d v seed
//
//      A test case with the given values of n and m
//	is generated in which i nodes are given colors
//      initially, the average degree is d, and there
//      is a solution.  d >= m is required.  Then v
//	extra edges connecting nodes of the same color
//	are added.  If v == 0 there is a solution; if
//	v > 0 there may not be a solution.  Seed is the
//      random number generator seed for generating the
//	test case.

# define MAKE_CONSTRAINEDSEARCH_INPUT
# include "constrainedsearch.cc"
# include <iomanip>
using std::setw;

// Input data
//
// int n, m;
int i, d, v;
unsigned seed;

// Output data.
//
char initial_node_colors[MAX_N];
    // Initial node color assignment line (without
    // ending NUL or \n).

// degree[i] is the number of edges connecting to node
// i.  Edges is the total number of edges (and should
// end up == d * n / 2).
//
int degree[MAX_N];
int edges;

// Assign color names.
//
void assign_color_names ( void )
{
    bool used[26];
    FOR(i,26) used[i] = false;
    FOR(c,m)
    {
        int i = rand() % 26;
	while ( used[i] ) i = (i+1) % 26;
	color_name[c] = 'A' + i;
	used[i] = true;
    }
}

// Assign node colors.
//
void assign_node_colors ( void )
{
    FOR(i,n)
    {
        int c = rand() % m;
	color[i] = c;
    }
}

// Assign initial node colors.
//
void assign_initial_node_colors ( void )
{
    FOR(j,n) initial_node_colors[j] = '?';

    FOR(k,i)
    {
        int j = rand() % n;
	while ( initial_node_colors[j] != '?' )
	    j = ( j + 1 ) % n;
	initial_node_colors[j] =
	    color_name [color[j]];
    }
}

// Assign connections so the degree of each node is m.
// Return true on success and false on failure.  The
// later means there were too many nodes of some color,
// and too few of different colors, which is unlikely
// but possible.
//
bool assign_m_edges ( void )
{
    edges = 0;
    FOR(i,n) degree[i] = 0;
    FOR(i,n)FOR(j,n) connected[i][j] = 0;

    FOR(i,n)
    {
        while ( degree[i] < m )
	{
	    int count = n;
	    for ( int j = rand() % n; ;
	          j = ( j + 1 ) % n )
	    {
		if ( count == 0 ) return false;
		-- count;

	        if ( j == i ) continue;
		if ( degree[j] > m ) continue;
		if ( color[i] == color[j] ) continue;
		if ( connected[i][j] ) continue;

		connected[i][j] = connected[j][i]
		                = true;
		++ degree[i];
		++ degree[j];
		++ edges;
		break;
	    }
	}
    }
    return true;
}

// Assign edges connecting different color nodes at
// random until edges = ( d * n ) / 2.
//
void assign_d_edges ( void )
{
    int dcount = 100 * n * n;
    while ( edges < d * n / 2 )
    {
	// Fail if d to large.
	//
        assert ( dcount -- );

        int i = rand() % n;
        int j = rand() % n;
	if ( i == j ) continue;
	if ( connected[i][j] ) continue;
	if ( color[i] == color[j] ) continue;
	connected[i][j] = connected[j][i] = true;
	++ degree[i];
	++ degree[j];
	++ edges;
    }
}

// Assign v edges connecting same color nodes.
//
void assign_v_edges ( void )
{
    int desired = edges + v;
    int vcount = 1000 * v;
    while ( edges < desired )
    {
	// Fail if v to large.
	//
        assert ( vcount -- );

        int i = rand() % n;
        int j = rand() % n;
	if ( i == j ) continue;
	if ( connected[i][j] ) continue;
	if ( color[i] != color[j] ) continue;
	connected[i][j] = connected[j][i] = true;
	++ degree[i];
	++ degree[j];
	++ edges;
    }
}


int main ( int argc )
{
    debug = ( argc > 1 );

    while ( true )
    {
        // Input.
	//
	cin.getline ( line, MAX_LINE+2 );
	if ( cin.eof() ) break;
	assert ( cin.gcount() <= MAX_LINE + 1 );

	assert ( line[0] == '-' );
	cout << line << endl;

        cin >> n >> m >> i >> d >> v >> seed;
	while ( cin.get() != '\n' );

	assert ( 1 <= n && n <= MAX_N );
	assert ( 1 <= m && m <= MAX_M );
	assert ( 0 <= i && i <= n );
	assert ( m <= d && d <= n-1 );
	assert ( 0 <= v );

	srand ( seed );

	assign_color_names();
	while ( true )
	{
	    assign_node_colors();
	    if ( assign_m_edges() ) break;
	}
	assign_initial_node_colors();
	assign_d_edges();
	assign_v_edges();

	cout << setw ( m ) << color_name << endl;
	cout << setw ( n ) << initial_node_colors
	     << endl;
	FOR(i,n)
	{
	    FOR(j,n)
	        cout << ( connected[i][j] ? '*'
		                          : '.' );
	    cout << endl;
	}
    }

    return 0;
}
