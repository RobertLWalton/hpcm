// Programming Contest File Difference Tester
//
// File:	scorediff.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Mon Jan 15 09:08:08 EST 2001
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: hc3 $
//   $Date: 2001/01/15 15:46:51 $
//   $RCSfile: old_scorediff.cc,v $
//   $Revision: 1.11 $

#include <stdlib.h>
#include <iostream.h>
#include <fstream.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#ifdef sun
#   include <ieeefp.h>
#endif
int finite (double);	// Not always in math.h
#include <assert.h>

#ifdef getc
#   undef getc
#endif

char documentation [] =
"scorediff output_file test_file\n"
"\n"
"    Returns on a single line a list of the types of\n"
"    differences between the two files.  The types of\n"
"    differences are:\n"
"\n"
"    none	There were no differences.\n"
"\n"
"    spacebreak	One file had whitespace in a line\n"
"		where there the other file did not.\n"
"\n"
"    linebreak	One file had a line break where the\n"
"		other file did not.\n"
"\n"
"    whitespace	Both files had whitespace in the same\n"
"		place, but these whitespaces did not\n"
"		exactly match.\n"
"\n"
"    eof1	The first file ended and the second\n"
"		file had remaining non-whitespace\n"
"		characters.\n"
"\n"
"    eof2	The second file ended and the first\n"
"		file had remaining non-whitespace\n"
"		characters.\n"
"\n"
"    number A R	Two files had a number in the same\n"
"		place, but the numbers were not\n"
"		represented by the same character\n"
"		string, and the maximum absolute\n"
"		value of the difference of all such\n"
"		number pairs was A and the maximum\n"
"		relative value of the difference of\n"
"		of all such number pairs was R.\n"
"\n"
"    decimal	Two numbers that were compared had\n"
"		different numbers of decimal places,\n"
"		or one had a decimal point and the\n"
"		other did not.\n"
"\f\n"
"    exponent	Two numbers were compared and one had\n"
"		an exponent but the other did not.\n"
"\n"
"    case	There were different non-whitespace\n"
"		characters at some place in the file\n"
"		other than those in matching numbers,\n"
"		and both were the same letter, but of\n"
"		different case.\n"
"\n"
"    column	Two non-whitespace characters were\n"
"		compared that were outside of numbers\n"
"		and these were in different columns,\n"
"		or two numbers were compared that did\n"
"		not end in the same column.\n"
"\n"
"    nonblank	There were different non-whitespace\n"
"		characters at some place in the file\n"
"		other than those in matching numbers\n"
"		or in letters that match but for\n"
"		case.  When this is discovered, the\n"
"		search for other kinds of differences\n"
"		terminates.\n"
"\n"
"    The files are parsed into whitespace, numbers,\n"
"    and other characters.  A number is an optional\n"
"    sign followed by digits containing an optional\n"
"    decimal point followed by an optional exponent.\n"
"    An exponent is an `e' or `E' followed by an\n"
"    an optional sign followed by digits.  A number\n"
"    is scanned by the strtod(3) function.  Numbers\n"
"    longer than about 4000 characters are arbitrar-\n"
"    ily truncated.\n"
"\f\n"
"    The relative value of the difference between two\n"
"    numbers x and y is:\n"
"\n"
"                        | x - y |\n"
"                     ----------------\n"
"                     max ( |x|, |y| )\n"
"\n"
"    and is never larger than 2.\n"
"\n"
"    If numbers are too large to compare (either they\n"
"    or their difference is infinity) then they are\n"
"    treated as non-numbers whose character string\n"
"    representations must match exactly.\n"
"\n"
"    For the purpose of computing the column of a\n"
"    character, tabs are set every 8 columns.\n" ;

struct file
{
    ifstream stream;	// Input stream.
    int column;		// Column within the line of
    			// the last character returned.
			// If this character is a \n,
			// this column is the length
			// of the line.  The first
			// column is 0.
    int nextcolumn;	// Next value for column.

    // Set by scanspace:

    int linebreaks;	// Number of `\n's scanned.

    // Set by Scannumber:

    bool isnumber;	// True iff a number is found.
    double number;	// Value of any found number.
    int decimals;	// Number of digits after the
    			// decimal point in the number,
			// or -1 if there is no decimal
			// point.
    bool hasexponent;	// True iff number has an exp-
    			// onent.
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

// Open file for reading.  Put a phantom `\n' at the
// beginning of each file so `spacebreak' does not
// falsely detect at the beginning of a file.
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
    f.backup [0] = '\n';
    f.backup [1] = 0;

    f.nextcolumn = 0;
}

// Get next character from file, respecting any backup.
// Returns next character in file or EOF if end of file.
//
inline int getc ( file & f )
{
    int c = * f.back;

    if ( c != 0 )	++ f.back;
    else		c = f.stream.get();

    f.column = f.nextcolumn;

    if ( c == '\n' )
	f.nextcolumn = 0;
    else if ( c == '\t' )
	f.nextcolumn += 8 - ( f.nextcolumn % 8 );
    else if ( c != EOF )
	++ f.nextcolumn;

    return c;
}

// Skips over whitespace in file, returning first non-
// whitespace character returned by getc of file.
// Returns the count of '\n's seen in the file's line-
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
// file f.  Then return the getc of the file.  c must
// be the last character gotten by getc of the file.
// All the string s characters must be non-whitespace,
// but c can be anything, including EOF.
//
inline int backup ( file & f, char * s, int c )
{
    assert ( * f.back == 0 );

    char * end   = f.backup;
    char * limit = f.backup + sizeof ( f.backup ) - 2;

    f.nextcolumn = f.column;

    int c2;
    while ( c2 = * s ++ )
    {
        assert ( end < limit );
	* end ++ = c2;

	assert ( ! isspace ( c2 ) );
	-- f.nextcolumn;
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
// Also sets the `decimals' and `hasexponent' members
// of the file if `isnumber' is set true.
//
// If the second input character is supplied, the first
// character MUST be a legal first character of a
// number.
//
int scannumber ( file & f, int c1, int c2 = 0 )
{
    if ( c1 != '+' && c1 != '-' && c1 != '.'
                   && ! isdigit ( c1 ) )
    {
        assert ( c2 == 0 );
    	f.isnumber = false;
	return c1;
    }

    char * limit = f.buffer + sizeof ( f.buffer ) - 1;

    f.end = f.buffer;
    * f.end ++ = c1;

    bool found_point = ( c1 == '.' );
    bool found_digit = isdigit ( c1 );

    f.decimals = found_point ? 0 : -1;
    f.hasexponent = false;

    char c = ( c2 == 0 ? getc ( f ) : c2 );

    while (1) {

	if ( f.end >= limit )
	    break;
	else if ( c == '.' )
	{
	    if ( found_point ) 
		break;
	    else
	    {
		found_point = true;
		f.decimals = 0;
	    }
	}
	else if ( isdigit ( c ) )
	{
	    found_digit = true;
	    if ( found_point )
	    	++ f.decimals;
	}
	else break;

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
	    f.hasexponent = true;
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
// `number_absdiff' and `number_reldiff' arguments
// by writing the differences just found into them
// iff these new differences are larger than the
// previous value of `number_absdiff' or `number_
// reldiff' respectively.  Also sets the decimal
// and exponent flags if the two number `decimals'
// or `hasexponent' members are unequal.
//
// If there is no computable difference, sets the
// `nonblank' flag argument instead.  This happens
// if one of the numbers is not `finite' or their
// difference is not `finite'.
// 
inline void diffnumber
	( file & file1, file & file2,
	  bool & nonblank, bool & number,
	  bool & decimal, bool & exponent,
	  double & number_absdiff,
	  double & number_reldiff )
{
    if ( ! finite ( file1.number )
	 ||
	 ! finite ( file2.number ) )
    {
	nonblank = true;
	return;
    }

    double absdiff =
	( file1.number - file2.number );
    if ( ! finite ( absdiff ) )
    {
	nonblank = true;
	return;
    }
    if ( absdiff < 0 ) absdiff = - absdiff;

    double abs1 = file1.number;
    if ( abs1 < 0 ) abs1 = - abs1;

    double abs2 = file2.number;
    if ( abs2 < 0 ) abs2 = - abs2;

    double max = abs1;
    if ( max < abs2 ) max = abs2;

    double reldiff = absdiff == 0.0 ?
                     0.0 :
		     absdiff / max;

    if ( ! finite ( reldiff ) )
    {
        // Actually this should never happen.

	nonblank = true;
	return;
    }

    number = true;

    if ( absdiff > number_absdiff )
	number_absdiff = absdiff;

    if ( reldiff > number_reldiff )
	number_reldiff = reldiff;

    if ( file1.decimals != file2.decimals )
    	decimal = true;

    if ( file1.hasexponent != file2.hasexponent )
    	exponent = true;
}

// Main program.
//
int main ( int argc, char ** argv )
{
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
    bool decimal	= false;
    bool exponent	= false;
    bool caseflag	= false;
    bool column		= false;
    bool nonblank	= false;

    double number_absdiff	= 0.0;	// Numeric
    double number_reldiff	= 0.0;	// differences.

    // Current characters from the two files.
    //
    int c1 = getc ( file1 );
    int c2 = getc ( file2 );

    // Flags that are true iff the current characters
    // follow whitespace.
    //
    bool follows_whitespace1 = false;
    bool follows_whitespace2 = false;

    while (1)
    {
        // At this point the last thing in each file
	// before c1 or c2 is one of:
	//
	//	the beginning of file
	//	a scanned number
	//	a scanned stretch of whitespace
	//	a character not in a number or
	//		whitespace
	//
	// Furthermore, one of these things preceeds c1
	// in file1 iff the same kind of thing preceeds
	// c2 in file2, except that one character can
	// follow whitespace and the other may not.
	//
	// Scanned numbers and whitespace are as long as
	// possible, so a character c1 or c2 could not
	// be part of a preceeding number or whitespace.
	// Exceptions to this rule are made if:
	//
	//	(1) c1 and c2 are both preceeded by
	//	    equal sign characters, + or -,
	//	    that may start a number
	//      (2) c1 and c2 are both whitespace
	//          characters and are preceeded by
	//          identical whitespace

        if ( c1 == EOF )
	{
	    if ( c2 == EOF ) break;

	    if ( isspace ( c2 ) )
	    {
		if ( ! follows_whitespace1 )
		    spacebreak = true;

		c2 = scanspace ( file2, c2 );
		follows_whitespace2 = true;

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

		follows_whitespace1 = true;
		follows_whitespace2 = true;

		if ( isspace ( c1 ) )
		{
		    if ( ! isspace ( c2 ) )
		    {
			whitespace = true;
			c1 = scanspace ( file1, c1 );
			if ( file1.linebreaks > 0 )
			    linebreak = true;
		    }

		    // If c1 and c2 are both != white-
		    // space characters, we leave them
		    // for later processig.
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

		follows_whitespace1 = true;
		follows_whitespace2 = true;

		if ( file1.linebreaks
			!= file2.linebreaks )
		    linebreak = true;
	    }
	    else
	    {
	    	if ( ! follows_whitespace2 )
		    spacebreak = true;

		c1 = scanspace ( file1, c1 );
		follows_whitespace1 = true;

		if ( file1.linebreaks > 0 )
		    linebreak = true;
	    }
     	}
	else if ( c1 == c2 )
	{
	    follows_whitespace1 = false;
	    follows_whitespace2 = false;

	    if ( c1 == '.' )
	    {
	    	c1 = getc ( file1 );
		c2 = getc ( file2 );

		if ( isdigit ( c1 )  && isdigit ( c2 ) )
		{
		    c1 = scannumber ( file1, '.', c1 );
		    c2 = scannumber ( file2, '.', c2 );

		    assert ( file1.isnumber );
		    assert ( file2.isnumber );

		    if ( file1.column != file2.column )
		    	column = true;

		    if ( strcmp ( file1.buffer,
		                  file2.buffer )
			 != 0 )
		    {
			diffnumber ( file1, file2,
			             nonblank, number,
				     decimal, exponent,
				     number_absdiff,
				     number_reldiff );
		        if ( nonblank ) break;
		    }
		}
		else
		{
		    // As previous character in both
		    // files was `.', its OK that we
		    // make this comparison one
		    // character later than we should.
		    //
		    if ( file1.column != file2.column )
		    	column = true;
		}
	    }
	    else if ( isdigit ( c1 ) )
	    {
		c1 = scannumber ( file1, c1 );
		c2 = scannumber ( file2, c2 );

		assert ( file1.isnumber );
		assert ( file2.isnumber );

		if ( file1.column != file2.column )
		    column = true;

		if ( strcmp ( file1.buffer,
			      file2.buffer )
		     != 0 )
		{
		    diffnumber ( file1, file2,
				 nonblank, number,
				 decimal, exponent,
				 number_absdiff,
				 number_reldiff );
		    if ( nonblank ) break;
		}
	    }
	    else
	    {
		if ( file1.column != file2.column )
		    column = true;

	        // If c1 and c2 are matching signs we
		// do not include them in numbers, as
		// in this case only absolute values of
		// the numbers need to be diffed.

	    	c1 = getc ( file1 );
		c2 = getc ( file2 );
	    }
	}
        else if ( isspace ( c2 ) )
	{
	    if ( ! follows_whitespace1 )
		spacebreak = true;

	    c2 = scanspace ( file2, c2 );
	    follows_whitespace2 = true;

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
		    file1.isnumber || file2.isnumber ) )
	{
	    follows_whitespace1 = false;
	    follows_whitespace2 = false;

	    if ( ( ! file1.isnumber )
	         ||
		 ( ! file2.isnumber ) )
	    {
		nonblank = true;
		break;
	    }

	    if ( file1.column != file2.column )
		column = true;

	    diffnumber ( file1, file2,
			 nonblank, number,
			 decimal, exponent,
			 number_absdiff,
			 number_reldiff );
	    if ( nonblank ) break;
	}
	else if ( toupper ( c1 ) == toupper ( c2 ) )
	{
	    follows_whitespace1 = false;
	    follows_whitespace2 = false;

	    caseflag = true;

	    if ( file1.column != file2.column )
		column = true;

	    c1 = getc ( file1 );
	    c2 = getc ( file2 );
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
	     << number_absdiff << " "
	     << number_reldiff;
	any = true;
    }
    if ( decimal ) {
    	cout << (any ? " decimal" : "decimal");
	any = true;
    }
    if ( exponent ) {
    	cout << (any ? " exponent" : "exponent");
	any = true;
    }
    if ( caseflag ) {
    	cout << (any ? " case" : "case");
	any = true;
    }
    if ( column ) {
    	cout << (any ? " column" : "column");
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
