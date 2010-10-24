// Solution to the Line Segment Intersections Problem
//
// File:	intersections.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Sun Oct 24 11:33:21 EDT 2010
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: walton $
//   $Date: 2010/10/24 20:11:04 $
//   $RCSfile: intersections.cc,v $
//   $Revision: 1.4 $

#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <cmath>
#include <cassert>
// You could use `using namespace std;' instead of the
// following.
using std::cin;
using std::cout;
using std::endl;
using std::ostream;
using std::setprecision;
using std::ios;

// The number of decimal places of point coordinates in
// the input and output.
//
int const DP = 3;
double const D = 1000;   // 10**DP.

// The denominator of any sum of products of coordinates
// of points is D**2 so any number in the range
// [-ACCURACY,+ACCURACY] is an approximation of true
// zero.
//
double ACCURACY = 0.5 / ( D * D );

bool debug = false;
#define dout if ( debug ) cout

// The big difficulty is finding a geometrical solution
// which can be done with precise rational arithmetic
// so we can make exact judgements as to whether or
// not there is a near miss.  The input coordinates are
// all rational numbers of the form X/D where X is
// an integer and D is as defined above, and we must
// compute using rational numbers.  To make things easy
// we use floating point numbers to hold rational
// numbers, and keep track of the denominator (e.g.,
// D or D**2) in the comments.

// Our method uses the fact that two line segments do
// NOT intersect if and only if
//
//	both endpoints of one segment are on the same
//	     side of the infinite line extending the
//	     other segment
//   or
//	both endpoints of one segment are rearward of
//	     the other segment viewed as a directed
//	     finite line
//   or
//	both endpoints of one segment are foreward of
//	     the other segment viewed as a directed
//	     finite line
//
// The directed infinite line that extends a directed
// line segment divides 2d space into a left side, a
// right side, and an on-line line.
//
// Given a finite line segment AB whose direction is A
// to B, consider the two lines perpendicular to AB
// going through A and B respectively.  The halfplane
// bounded by the perpendicular though A is `rearward'.
// The halfplane bounded by the perpendicular through
// B is `forward'.  The two perpendiculars themselves
// and the space between them is `sideward'.

struct point { double x, y; };

// For debugging only.  Temporarily set precision to
// 2DP.
//
ostream & operator << ( ostream & s, point p )
{
    return s << setprecision ( 2*DP ) << "(" << p.x
             << "," << p.y << ")"
	     << setprecision ( DP );
}

// Return X - Y.
//
point operator - ( point X, point Y )
{
    X.x -= Y.x;
    X.y -= Y.y;
    return X;
}

// Distance between X and Y.
//
double dist ( point X, point Y )
{
    point XY = X - Y;
    return sqrt ( XY.x * XY.x + XY.y * XY.y );
}

// Cross product of X and Y
//   == dot ( X, Rotate-clockwise-90-degrees Y )
//   == dot ( Y, Rotate-counterclockwise-90-degrees X )
//
double cross ( point X, point Y )
{
    return X.x * Y.y - X.y * Y.x;
}

// Dot product of X and Y.
//
double dot ( point X, point Y )
{
    return X.x * Y.x + X.y * Y.y;
}

// Given points X, Y, Z with compute:
//
//   side =
//
//	-1	X is to the left of the directed
//		     infinite line through YZ
//	 0	X is on the directed
//		     infinite line through YZ
//	+1	X is to the right of the directed
//		     infinite line through YZ
//
//   ward =
//
//	-1	X is rearward of YZ
//
//	 0	X is sideward of YZ
//
//	+1	X is forward of YZ
//
// The computation assumes sums of products of point
// coordinates that are in the range [-ACC,+ACC] are
// actually 0.  This will be true if point coordinates
// are fractions with demominator D and ACC =
// 1/(2*D*D) = ACCURACY.
//
void where
	( int & side, int & ward,
	  point X, point Y, point Z,
	  double ACC = ACCURACY )
{
    // Change coordinates so Y is (0,0).  This does not
    // change coordinate denominator.
    //
    double cp = cross ( X - Y, Z - Y );
    side = cp < - ACC ? -1 :
           cp > + ACC ? +1 :
	   0;

    double dotX = dot ( X, Z - Y );
    double dotY = dot ( Y, Z - Y );
    double dotZ = dot ( Z, Z - Y );
    assert ( dotY < dotZ );
    ward = dotX < dotY - ACC ? -1 :
           dotX > dotZ + ACC ? +1 :
	   0;

    dout << "WHERE " << X
         << " " << Y << "-" << Z
         << " SIDE " << side
         << " WARD " << ward
	 << endl;
}

int main ( int argc, char * argv[] )
{
    debug = ( argc > 1 );

    cout << setiosflags ( ios::showpoint | ios::fixed );
    cout << setprecision ( DP );

    while ( true )
    {
	point A, B, C, D;
	cin >> A.x >> A.y
	    >> B.x >> B.y
	    >> C.x >> C.y
	    >> D.x >> D.y;
	if ( cin.eof() ) break;

	int A_side, B_side, C_side, D_side;
	int A_ward, B_ward, C_ward, D_ward;

	where ( A_side, A_ward, A, C, D );
	where ( B_side, B_ward, B, C, D );
	where ( C_side, C_ward, C, A, B );
	where ( D_side, D_ward, D, A, B );

	if ( A_side * B_side == 1 )
	    cout << "NO INTERSECTION" << endl;
	else if ( C_side * D_side == 1 )
	    cout << "NO INTERSECTION" << endl;
	if ( A_ward * B_ward == 1 )
	    cout << "NO INTERSECTION" << endl;
	else if ( C_ward * D_ward == 1 )
	    cout << "NO INTERSECTION" << endl;
	else if (    A_side != 0 || B_side != 0
	          || C_side != 0 || D_side != 0 )
	{
	    // Lines are not parallel.
	    //
	    // Intersection must be a single point
	    // that is the intersections of the
	    // infinite lines extending the
	    // segments.

	    cout << "INTERSECTION ";
	    if ( A_side == 0 ) cout << "A";
	    if ( B_side == 0 ) cout << "B";
	    if ( C_side == 0 ) cout << "C";
	    if ( D_side == 0 ) cout << "D";
	    cout << " 0.000" << endl;
	}
	else
	{
	    // Lines are parallel.
	    //
	    // Infinite lines extending the segments
	    // are identical.
	    //
	    // Any endpoint X with X_ward == 0 is an
	    // intersection.
	    // 
	    cout << "INTERSECTION ";
	    if ( A_ward == 0 ) cout << "A";
	    if ( B_ward == 0 ) cout << "B";
	    if ( C_ward == 0 ) cout << "C";
	    if ( D_ward == 0 ) cout << "D";
	    cout << " ";

	    if ( A_ward == 0 && B_ward == 0 )
	        cout << dist ( A, B );
	    else if ( A_ward == 0 && C_ward == 0 )
	        cout << dist ( A, C );
	    else if ( A_ward == 0 && D_ward == 0 )
	        cout << dist ( A, D );
	    else if ( B_ward == 0 && C_ward == 0 )
	        cout << dist ( B, C );
	    else if ( B_ward == 0 && D_ward == 0 )
	        cout << dist ( B, D );
	    else if ( C_ward == 0 && D_ward == 0 )
	        cout << dist ( C, D );
	    else abort();  // Should not happen.

	    cout << endl;
	}
    }

    return 0;
}
