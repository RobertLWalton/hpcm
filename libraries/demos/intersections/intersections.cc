// Solution to the Line Segment Intersections Problem
//
// File:	intersections.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Sat Sep 18 07:12:31 EDT 2010
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: walton $
//   $Date: 2010/09/18 11:20:44 $
//   $RCSfile: intersections.cc,v $
//   $Revision: 1.3 $

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

// The number of decimal places in the input and output.
//
int const D = 3;
double const DENOMINATOR = 1000;   // 10**D.

bool debug = false;
#define dout if ( debug ) cout

// The big difficulty is finding a geometrical solution
// which can be done with precise rational arithmetic
// so we can make exact judgements as to whether or
// not there is a near miss.  The input coordinates are
// all rational numbers of the form X/(10*D) where X is
// an integer and D is as defined above, and we must
// compute using rational numbers.  To make things easy
// we use floating point numbers to hold rational
// numbers, and keep track of the denominator (e.g.,
// 10**D or 10**(2D)) in the comments.

// Our method uses the fact that AB and CD intersect if
// and only if AB intersects the infinite line through
// CD AND CD intersects the infinite line through AB.
// These tests can be done easily using rational numbers
// with denominators 10**(2D).  Also, determining
// whether the points A or B are on the infinite line
// through CD, etc., gives us information about which
// end points are in the intersection.

struct point { double x; double y; };

// For debugging only.  Temporarily set precision to
// 2D.
//
ostream & operator << ( ostream & s, point p )
{
    return s << setprecision ( 2*D ) << "(" << p.x
             << "," << p.y << ")" << setprecision ( D );
}

// Return distance between X and Y.
//
double dist ( point X, point Y )
{
    point XY;
    XY.x = Y.x - X.x;
    XY.y = Y.y - X.y;
    return sqrt ( XY.x * XY.x + XY.y * XY.y );
}

// Given points X, Y, Z with coordinates that have
// denominator 10**D, determine whether X is on the
// infinite line YZ, and if not, which side of the line
// it is on.  Set which_side as follows:
//
//	-1	X is to the left of the directed
//		     infinite line through YZ
//	 0	X is on the directed
//		     infinite line through YZ
//	+1	X is to the right of the directed
//		     infinite line through YZ
//
// In the case where which_side is 0 an additional
// value, which_end is returned, as follows:
//
//	-1	X is before Y on the directed infinite
//		     line through YZ
//	 0	X is in the segment YZ
//	+1	X is after Z on the directed infinite
//		     line through YZ
//
void where
	( int & which_side, int & which_end,
	  point X, point Y, point Z )
{
    // Change coordinates so Y is (0,0).  This does not
    // change coordinate denominator.
    //
    X.x -= Y.x;
    X.y -= Y.y;
    Z.x -= Y.x;
    Z.y -= Y.y;

    // Change coordinates so YZ is on the x-axis.  This
    // is done by taking the scalar product of each
    // point with YZ to get a new x coordinate and
    // with YZ rotated right 90 degrees to get a new
    // y coordinate.  We just do this transform on X
    // as Y stays at the origin under this transform
    // and Z ends up on the x-axis and we do not need
    // to know more about Z (for now).
    //
    // Note that as Y=(0,0) then YZ.{x,y} = Z.{x,y}.
    //
    point tX;  // Transformed X.
    tX.x = X.x * Z.x + X.y * Z.y;
    tX.y = X.x * Z.y - X.y * Z.x;

    // Now tX has coordinate denominator 10**(2D).
    //
    // The infinite line through transformed YZ is the
    // X axis.  tX.y determines which side of this X is
    // on.  > 0 means right, < 0 means left, 0 means on.
    // tX.y represents and is very close to a rational
    // number with denominator 10**(2D).  If tX.y is
    // within 10**(2D)/2 of 0 the rational number must
    // be exactly zero.  Otherwise it must be exactly
    // some non-zero rational number whose sign is the
    // sign of tX.y.
    //
    if ( tX.y < -0.5/(DENOMINATOR*DENOMINATOR) )
        which_side = -1;
    else if ( tX.y > 0.5/(DENOMINATOR*DENOMINATOR) )
        which_side = +1;
    else
    {
        which_side = 0;

        // To compute which_end we need to transform Z.
	//
	point tZ;  // Transformed Z.
	tZ.x = Z.x * Z.x + Z.y * Z.y;
	tZ.y = Z.x * Z.y - Z.y * Z.x; // == 0 of course.

	// The transformed x coordinates represent and
	// are very close to exact rational numbers with
	// denominator 10**(2D).  So, for example, X.x
	// and Z.x represent the same rational number
	// if and only if their difference is much less
	// than 10**(2D)/2 in absolute value.

	if ( X.x < -0.5/(DENOMINATOR*DENOMINATOR) )
	     which_end = -1;
	else if ( X.x
	          >
		  Z.x + 0.5/(DENOMINATOR*DENOMINATOR) )
	     which_end = +1;
	else
	     which_end = 0;
    }
}



int main ( int argc, char * argv[] )
{
    debug = ( argc > 1 );

    while ( true )
    {
	point A, B, C, D;
	cin >> A.x >> A.y
	    >> B.x >> B.y
	    >> C.x >> C.y
	    >> D.x >> D.y;
	if ( cin.eof() ) break;

	int A_side, B_side, C_side, D_side;
	int A_end, B_end, C_end, D_end;

	where ( A_side, A_end, A, C, D );
	where ( B_side, B_end, B, C, D );
	where ( C_side, C_end, C, A, B );
	where ( D_side, D_end, D, A, B );

	if ( A_side * B_side == 1 )
	    cout << "NO INTERSECTION" << endl;
	else if ( C_side * D_side == 1 )
	    cout << "NO INTERSECTION" << endl;
	else if ( A_side != 0 || B_side != 0 )
	{
	    // Lines are not parallel.
	    // Intersection must be a single point.

	    cout << "INTERSECTION ";
	    if ( A_side == 0 ) cout << "A";
	    if ( B_side == 0 ) cout << "B";
	    if ( C_side == 0 ) cout << "C";
	    if ( D_side == 0 ) cout << "D";
	    cout << " 0.000" << endl;
	}
	else if ( A_end * B_end == 1 )
	    cout << "NO INTERSECTION" << endl;
	else
	{
	    cout << "INTERSECTION ";
	    if ( A_end == 0 ) cout << "A";
	    if ( B_end == 0 ) cout << "B";
	    if ( C_end == 0 ) cout << "C";
	    if ( D_end == 0 ) cout << "D";
	    cout << " ";

	    if ( A_end == 0 && B_end == 0 )
	        cout << dist ( A, B );
	    else if ( A_end == 0 && C_end == 0 )
	        cout << dist ( A, C );
	    else if ( A_end == 0 && D_end == 0 )
	        cout << dist ( A, D );
	    else if ( B_end == 0 && C_end == 0 )
	        cout << dist ( B, C );
	    else if ( B_end == 0 && D_end == 0 )
	        cout << dist ( B, D );
	    else if ( C_end == 0 && D_end == 0 )
	        cout << dist ( C, D );
	    else abort();  // Should not happen.

	    cout << endl;
	}
    }

    return 0;
}
