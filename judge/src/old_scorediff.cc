// Programming Contest File Difference Tester
//
// File:	scorediff.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Tue Aug 15 21:20:44 EDT 2000
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: acm-cont $
//   $Date: 2000/08/16 02:19:57 $
//   $RCSfile: old_scorediff.cc,v $
//   $Revision: 1.6 $

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
"		place, but these whitespaces did not\n"
"		exactly match.\n"
"    eof1	The first file ended and the second\n"
"		file had remaining non-whitespace\n"
"		characters.\n"
"    eof2	The second file ended and the first\n"
"		file had remaining non-whitespace\n"
"		characters.\n"
"    number D	Two files had a number in the same\n"
"		place, but the numbers were not\n"
"		represented by the same character\n"
"		string, and the maximum absolute\n"
"		value of the difference of all such\n"
"		number pairs was D.\n"
"    nonblank	There were different non-whitespace\n"
"		characters at some place in the file\n"
"		other than those in matching numbers.\n"
"\n"
"    The files are parsed into whitespace, numbers,\n"
"    and other characters.  A number is an optional\n"
"    sign followed by digits containing an optional\n"
"    decimal point followed by an optional exponent.\n"
"    An exponent is an `e' or `E' followed by an\n"
"    an optional sign followed by digits.  A number\n"
"    is scanned by the strtod(3) function.  Numbers\n"
"    longer than about 4000 characters are arbitrar-\n"
"    ily truncated.\n" ;

struct file
{
    ifstream stream;	// Input stream.

    // Set by scanspace:

    int linebreaks;	// Number of `\n's scanned.

    // Set by Scannumber:

    bool isnumber;	// True iff a number is found.
    double number;	// Value of any found number.
    char buffer [ 4000 ];
    			// The character string of
    			// any found number.
    char * end;		// Points to the `\0' at the
    			// end of any string in the
			// buffer.

    // Set to backup in input string when scannumber
    // determines it has not been passed a number or
    // exponent.  Possible values are:
    //
    //		+    -    +.   -.
    //		e    e+   e-   E    E+   E-
    //
    // Must be such that the backup is empty when
    // scannumber decides to put something in the
    // backup.

    char * back;	// If pointing at `\0', there
    			// are no backed up characters
			// to deliver.  Otherwise
			// use `* back ++' to input
			// next character.
    char backup [ 5 ];  // Buffer of backed up char-
    			// acters into which `back'
			// points.
};

// Open file for reading.
//
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

// Get next character from file, respecting any backup.
// Returns next character in file or EOF if end of file.
//
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

// Skips over whitespace in file, returning first non-
// whitespace character returned by getc of file.
// Returns the count of '\n's seen in the files line-
// breaks member.  Takes a character argument that is
// the first character tested for whitespace, before
// getc is called.
//
inline int scanspace ( file & f, int c )
{
    f.linebreaks = 0;
    while ( isspace ( c ) )
    {
        if ( c == '\n' ) ++ f.linebreaks;
	c = getc ( f );
    }
    return c;
}

// Put string s followed by character c in backup of
// file f.  Then return the getc of the file.  c == EOF
// is allowed.
//
inline int backup ( file & f, char * s, int c )
{
    assert ( * f.back == 0 );

    char * end   = f.backup;
    char * limit = f.backup + sizeof ( f.backup ) - 2;

    int c2;
    while ( c2 = * s ++ )
    {
        assert ( end < limit );
	* end ++ = c2;
    }
    if ( c != EOF ) * end ++ = c;
    * end = 0;

    f.back = f.backup;

    return getc ( f );
}

// Scans a file for a number.  Takes one or two char-
// acters which are the first characters of the number.
// Returns the first character not in the number.
//
// Sets the `isnumber' member of the file to true iff
// a number is found.  If a number is found, sets
// the `buffer' member of the file to the character
// string of the number and the `number' member of
// of the file to the value of the number.  Note that
// this value may be an infinity if the number is too
// large.
//
int scannumber ( file & f, int c1, int c2 = 0 )
{
    if ( c1 != '+' && c1 != '-' && c1 != '.'
                   && ! isdigit ( c1 ) )
    {
    	f.isnumber = false;
	return c1;
    }

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
	f.isnumber = false;

    	* f.end = 0;
	return backup ( f, f.buffer, c );
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
	    * f.end = 0;
	    c = backup ( f, endsave, c );
	    f.end = endsave;
	}
    }

    * f.end = 0;
    f.isnumber = true;

    char * e;
    f.number = strtod ( f.buffer, & e );
    assert ( e == f.end || ! finite ( f.number ) );
    	//
    	// If number is too large then f.number is
	// set to an infinity and e is not set to
	// the end of the number; which is probably
	// a bug in strtod.

    return c;
}

// Tests two numbers just scanned for two files to
// see if there is a computable difference.  If so,
// sets the `number' flag argument and updates the
// `number_diff' argument by writing the difference
// just found into it if this new difference is
// larger than the previous value of `number_diff'.
//
// If there is no computable difference, sets the
// `nonblank' flag argument instead.  This happens
// if one of the numbers is not `finite' or their
// difference is not `finite'.
// 
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

// Main program.
//
int main ( int argc, char ** argv )
{
    int c1, c2;

    file file1;
    file file2;

    // Print documentation and exit with error status
    // unless there are exactly two program arguments.

    if ( argc != 3 )
    {
        cout << documentation;
	exit (1);
    }

    // Open files.

    open ( file1, argv[1] );
    open ( file2, argv[2] );

    // Loop that reads the two files and compares
    // their characters (in c1 and c2), setting flags
    // describing any differences found.

    bool spacebreak	= false;	// Difference
    bool linebreak	= false;	// flags.
    bool whitespace	= false;
    bool eof1		= false;
    bool eof2		= false;
    bool number		= false;
    bool nonblank	= false;

    double number_diff	= 0.0;		// Numeric
    					// difference.

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
	else if ( c1 == c2 )
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

    // Loop done and difference flags are now computed.
    // Produce output line according to difference
    // flags.

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

    // Return from main function without error.

    return 0;
}
