// Scoring Filter for the Scoring Filter Test Problem
//
// File:	scoring_filter.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Fri Apr 14 06:45:28 EDT 2006
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: hc3 $
//   $Date: 2006/04/14 10:49:54 $
//   $RCSfile: Scoring_Filter.cc,v $
//   $Revision: 1.1 $


#define Scoring_Filter
#include "scoring_filter_test.cc"

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
