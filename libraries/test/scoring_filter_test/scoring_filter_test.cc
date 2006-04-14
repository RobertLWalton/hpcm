// Solution to Scoring Filter Test Problem
//
// File:	scoring_filter_test.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Fri Apr 14 06:46:57 EDT 2006
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: hc3 $
//   $Date: 2006/04/14 10:49:46 $
//   $RCSfile: scoring_filter_test.cc,v $
//   $Revision: 1.2 $

// Compile with -DSCORING_FILTER to obtain Scoring_
// Filter program that takes a .out file as stdin,
// writes a .fout file on stdout.

#include <iostream>
using std::cout;
using std::cin;
using std::endl;

const unsigned MAX_LINE = 300;


#ifndef Scoring_Filter

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

#endif
