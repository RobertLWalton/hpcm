// Programming Contest File Difference Tester
//
// File:	scorediff.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Sat Aug 12 09:41:03 EDT 2000
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: acm-cont $
//   $Date: 2000/08/14 01:20:40 $
//   $RCSfile: old_scorediff.cc,v $
//   $Revision: 1.5 $

#include <stdlib.h>
#include <iostream.h>
#include <fstream.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <assert.h>

char documentation [] =
"scorediff output_file test_file\n"
"\n"
"    Returns on a single line a list of the types of\n"
"    differences between the two files.  The types of\n"
"    differences are:\n"
"\n"
"    none	There were no differences.\n"
"    spacebreak	One file had whitespace in a line\n"
"		where there the other file did not.\n"
"    linebreak	One file had a line break where the\n"
"		other file did not.\n"
"    whitespace	Both files had whitespace in the same\n"
"		place, but there these whitespaces\n"
"		did not exactly match.\n"
"    eof1	The first file ended and the second\n"
"		file had remaining non-space\n"
"		characters.\n"
"    eof2	The second file ended and the first\n"
"		file had remaining non-space\n"
"		characters.\n"
"    number D	Two files had a number in the same\n"
"		place, but the numbers were not\n"
"		represented by the same character\n"
"		string, and the maximum absolute\n"
"		value of the difference of all such\n"
"		number pairs was D.\n"
"    nonblank	There were different non-whitespace\n"
"		characters at some place in the file\n"
"		than those in numbers.\n"
"\n"
"    The files are parsed into whitespace, numbers,\n"
"    and other characters.  A number is an optional\n"
"    sign followed by digits containing an optional\n"
"    decimal point followed by an optional exponent.\n"
"    An exponent is an `e' or `E' followed by an\n"
"    an optional sign followed by digits.  A number\n"
"    is scanned by the strtod(3) function.\n" ;

struct file
{
    ifstream stream;	// Input stream.

    // Set by scanspace:

    int linebreaks;	// Number of `\n's scanned.

    // Set by Scannumber:

    bool isnumber;	// True iff a number is found.
    double number;	// The value any found number.
    char buffer [ 4000 ];
    			// The character string of
    			// any found number.
    char * end;		// Points to the `\0' at the
    			// end of any string in the
			// buffer.

    // Set to backup in input string:

    char * back;	// If pointing at `\0', there
    			// are no backed up characters
			// to deliver.  Otherwise
			// use `* back ++' to input
			// next character.
    char backup [ 5 ];  // Buffer of backed up char-
    			// acters into which `back'
			// points.
};

int open ( file & f, char * filename )
{
    f.stream.open ( filename );

    if ( ! f.stream ) {
        cout << "ERROR: not readable: "
	     << filename << endl;
    	exit ( 1 );
    }

    f.back = f.backup;
    * f.back = 0;
}

inline int getc ( file & f )
{
    int c = * f.back;

    if ( c != 0 )
    {
        ++ f.back;
        return c;
    }
    else
        return f.stream.get();
}

int scanspace ( file & f, int c )
{
    f.linebreaks = 0;
    while ( isspace ( c ) )
    {
        if ( c == '\n' ) ++ f.linebreaks;
	c = getc ( f );
    }
    return c;
}

int scannumber ( file & f, int c1, int c2 = 0 )
{
    char * limit = f.buffer + sizeof ( f.buffer ) - 1;

    f.end = f.buffer;
    * f.end ++ = c1;

    bool found_point = ( c1 == '.' );
    bool found_digit = isdigit ( c1 );

    char c = ( c2 == 0 ? getc ( f ) : c2 );

    while (1) {
	if ( c == '.' )
	{
	    if ( found_point ) 
		break;
	    else found_point = true;
	}
	else if ( isdigit ( c ) )
	    found_digit = true;
	else break;

	if ( f.end >= limit ) break;

	* f.end ++ = c;
        c = getc ( f );
    }

    if ( ! found_digit )
    {
    	* f.end = 0;
	f.isnumber = false;

	assert ( * f.back == 0 );
	strcpy ( f.backup, f.buffer );
	f.back = f.backup;

	return c;
    }

    if ( ( c == 'e' || c == 'E' )
         &&
	 f.end < limit - 20 )
    {
    	char * endsave = f.end;

	* f.end ++ = c;
	c = getc ( f );

	if ( c == '+' || c == '-' )
	{
	    * f.end ++ = c;
	    c = getc ( f );
	}

	if ( isdigit ( c ) )
	{
	    while ( isdigit ( c ) && f.end < limit )
	    {
		* f.end ++ = c;
		c = getc ( f );
	    }
	}
	else
	{
	    assert ( * f.back == 0 );

	    * f.end = 0;
	    strcpy ( f.backup, endsave );
	    f.end = endsave;
	    * endsave = 0;
	    f.back = f.backup;
	}
    }

    * f.end = 0;
    f.isnumber = true;

    char * e;
    f.number = strtod ( f.buffer, & e );
    assert ( e == f.end || ! finite ( f.number ) );

    return c;
}

inline void diffnumber
	( file & file1, file & file2,
	  bool & nonblank, bool & number,
	  double & number_diff )
{
    if ( ! finite ( file1.number )
	 ||
	 ! finite ( file2.number ) )
    {
	nonblank = true;
	return;
    }

    double diffn
	( file1.number - file2.number );
    if ( ! finite ( diffn ) )
    {
	nonblank = true;
	return;
    }
    if ( diffn < 0 ) diffn = - diffn;

    number = true;
    if ( diffn > number_diff )
	number_diff = diffn;
}

int main ( int argc, char ** argv )
{
    int c1, c2;

    file file1;
    file file2;

    if ( argc != 3 )
    {
        cout << documentation;
	exit (1);
    }

    open ( file1, argv[1] );
    open ( file2, argv[2] );

    bool spacebreak	= false;
    bool linebreak	= false;
    bool whitespace	= false;
    bool eof1		= false;
    bool eof2		= false;
    bool number		= false;
    bool nonblank	= false;

    double number_diff	= 0.0;

    c1 = getc ( file1 );
    c2 = getc ( file2 );

    while (1)
    {
        if ( c1 == EOF )
	{
	    if ( c2 == EOF ) break;

	    if ( isspace ( c2 ) )
	    {
		spacebreak = true;
		c2 = scanspace ( file2, c2 );
		if ( file2.linebreaks > 0 )
		    linebreak = true;

		if ( c2 == EOF ) break;
	    }

	    eof1 = true;

	    break;
     	}
        else if ( isspace ( c1 ) )
	{
	    if ( c2 == c1 )
	    {
	    	do {
		    c1 = getc ( file1 );
		    c2 = getc ( file2 );
		} while ( c1 == c2 && isspace ( c1 ) );

		if ( isspace ( c1 ) )
		{
		    if ( ! isspace ( c2 ) )
		    {
			whitespace = true;
			c1 = scanspace ( file1, c1 );
			if ( file1.linebreaks > 0 )
			    linebreak = true;
		    }
		}
		else if ( isspace ( c2 ) )
		{
		    whitespace = true;
		    c2 = scanspace ( file2, c2 );
		    if ( file2.linebreaks > 0 )
			linebreak = true;
		}
	    }
	    else if ( isspace ( c2 ) )
	    {
	    	whitespace = true;

	        c1 = scanspace ( file1, c1 );
	        c2 = scanspace ( file2, c2 );

		if ( file1.linebreaks
			!= file2.linebreaks )
		    linebreak = true;
	    }
	    else
	    {
	        spacebreak = true;
		c1 = scanspace ( file1, c1 );
		if ( file1.linebreaks > 0 )
		    linebreak = true;
	    }
     	}
	else if  (c1 == c2 )
	{
	    if ( c1 == '.' )
	    {
	    	c1 = getc ( file1 );
		c2 = getc ( file2 );

		if ( isdigit ( c1 )  && isdigit ( c2 ) )
		{
		    c1 = scannumber ( file1, '.', c1 );
		    c2 = scannumber ( file2, '.', c2 );
		    if ( strcmp ( file1.buffer,
		                  file2.buffer )
			 != 0 )
		    {
		        assert ( file1.isnumber );
		        assert ( file2.isnumber );

			diffnumber ( file1, file2,
			             nonblank, number,
				     number_diff );
		        if ( nonblank ) break;
		    }
		}
	    }
	    else if ( isdigit ( c1 ) )
	    {
		c1 = scannumber ( file1, c1 );
		c2 = scannumber ( file2, c2 );
		if ( strcmp ( file1.buffer,
			      file2.buffer )
		     != 0 )
		{
		    assert ( file1.isnumber );
		    assert ( file2.isnumber );

		    diffnumber ( file1, file2,
				 nonblank, number,
				 number_diff );
		    if ( nonblank ) break;
		}
	    }
	    else
	    {
	        // Matching signs beginning numbers
		// are ignored, as only absolute
		// values of numbers are diffed.

	    	c1 = getc ( file1 );
		c2 = getc ( file2 );
	    }
	}
        else if ( isspace ( c2 ) )
	{
	    spacebreak = true;
	    c2 = scanspace ( file2, c2 );
	    if ( file2.linebreaks > 0 )
		linebreak = true;
     	}
	else if ( c2 == EOF )
	{
	    eof2 = true;
	    break;
	}
	else if ( ( c1 = scannumber ( file1, c1 ),
		    c2 = scannumber ( file2, c2 ),
		    file1.isnumber && file2.isnumber ) )
	{
	    diffnumber ( file1, file2,
			 nonblank, number,
			 number_diff );
	    if ( nonblank ) break;
	}
        else
	{
	    nonblank = true;
	    break;
     	}
    }

    bool any = false;

    if ( spacebreak ) {
    	cout << (any ? " spacebreak" : "spacebreak");
	any = true;
    }
    if ( linebreak ) {
    	cout << (any ? " linebreak" : "linebreak");
	any = true;
    }
    if ( whitespace ) {
    	cout << (any ? " whitespace" : "whitespace");
	any = true;
    }
    if ( eof1 ) {
    	cout << (any ? " eof1" : "eof1");
	any = true;
    }
    if ( eof2 ) {
    	cout << (any ? " eof2" : "eof2");
	any = true;
    }
    if ( number ) {
    	cout << (any ? " number " : "number ")
	     << number_diff;
	any = true;
    }
    if ( nonblank ) {
    	cout << (any ? " nonblank" : "nonblank");
	any = true;
    }
    cout << (any ? "" : "none") << endl;

    return 0;
}
