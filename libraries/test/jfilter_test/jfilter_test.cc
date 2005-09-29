// Solution to Jfilter Test Problem
//
// File:	jfilter_test.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Thu Sep 29 04:40:59 EDT 2005
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: hc3 $
//   $Date: 2005/09/29 08:43:27 $
//   $RCSfile: jfilter_test.cc,v $
//   $Revision: 1.1 $

#include <iostream>
using std::cout;
using std::cin;
using std::endl;

const unsigned MAX_LINE = 300;

int main ( int argc )
{
    char line [MAX_LINE+1];

    while ( true )
    {
        cin.getline ( line, MAX_LINE );
	if ( cin.eof() ) break;
	cout << line << endl;
    }

    return 0;
}
