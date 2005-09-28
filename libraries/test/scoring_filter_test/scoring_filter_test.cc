// Solution to Scoring Filter Test Problem
//
// File:	scoring_filter_test.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Wed Sep 28 13:13:12 EDT 2005
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: hc3 $
//   $Date: 2005/09/28 18:24:15 $
//   $RCSfile: scoring_filter_test.cc,v $
//   $Revision: 1.1 $

// Compile with -DSCORING_FILTER to obtain Scoring_
// Filter program that takes a .out file as stdin,
// writes a .fout file on stdout.

#include <iostream>
using std::cout;
using std::cin;
using std::endl;

const unsigned MAX_LINE = 300;


#ifndef SCORING_FILTER

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

#ifdef SCORING_FILTER

int main ( int argc )
{
    char line [MAX_LINE+1];

    while ( true )
    {
        cin.getline ( line, MAX_LINE );
	if ( cin.eof() ) break;
	cout << "FILTERED: " << line << endl;
    }

    return 0;
}

#endif

