// Programming Contest File Difference Tester
//
// File:	scorediff.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Sat Oct 28 22:24:12 EDT 2000
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: hc3 $
//   $Date: 2000/10/30 04:37:08 $
//   $RCSfile: scorediff.cc,v $
//   $Revision: 1.14 $

// This is version 2, a major revision of the first
// scorediff program.  This version is more explicitly
// token oriented.

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

// Maximum size of a token, or of whitespace preceding
// a token.
//
#define MAX_SIZE 10000

char documentation [] =
"scorediff output_file test_file\n"
"\n"
"    Returns on a single line a list of the types of\n"
"    differences between the two files, followed by\n"
"    lines that are `proofs' of these differences.\n"
"\n"
"    To find differences, the files are parsed into\n"
"    non-blank `tokens', and successive tokens of the\n"
"    two files are matched, along with the whitespace\n"
"    that precedes the matched tokens.  A token is by\n"
"    definition either a number, or a non-whitespace\n"
"    character that is not part of a number, or an\n"
"    end-of-file.\n"
"\n"
"    The types of differences are:\n"
"\n"
"    linebreak	For two matching tokens the preceding\n"
"		whitespace of one had a different num-\n"
"		ber of new lines than the preceding\n"
"		whitespace of the other.\n"
"\n"
"    whitespace	For two matching tokens the preceding\n"
"		whitespaces both had no new lines,\n"
"		and both were non-empty, but did not\n"
"		match exactly.\n"
"\n"
"    spacebreak	For two matching tokens, one was pre-\n"
"		ceded by whitespace containing no\n"
"		new lines, and the other had no pre-\n"
"		ceding whitespace at all."
"\n"
"    endspace	For two matching tokens the preceding\n"
"		whitespaces had the same number N of\n"
"		new lines and N >= 1 and the white-\n"
"		space characters preceding the first\n"
"		new line for one token did not exact-\n"
"		ly match the whitespace characters\n"
"		preceding the first new line for the\n"
"		other token.\n"
"\n"
"    linespace	For two matching tokens the preceding\n"
"		whitespaces had the same number N of\n"
"		new lines and N >= 1 and the white-\n"
"		space characters between some pair of\n"
"		matching new lines of the two token\n"
"	        preceding whitespaces do not exactly\n"
"		agree.\n"
"\n"
"    beginspace	For two matching tokens the preceding\n"
"		whitespaces had the same number N of\n"
"		new lines and N >= 1 and the white-\n"
"		space characters following the last\n"
"		new line for one token did not exact-\n"
"		ly match the whitespace characters\n"
"		following the last new line for the\n"
"		other token.\n"
"\n"
"    eof1	The first file ended and the second\n"
"		file had a remaining token.\n"
"\n"
"    eof2	The second file ended and the first\n"
"		file had a remaining token.\n"
"\n"
"    number A R	For two matching number tokens, the\n"
"		token character strings did not match\n"
"		exactly, the absolute difference of\n"
"		the numbers is A, and the relative\n"
"		difference of the numbers is R.\n"
"\n"
"		In the line returned reporting dif-\n"
"		ferences in the two files, A and R\n"
"		are the maximum observed A and R\n"
"		values for all matching number\n"
"		tokens.\n"
"\n"
"    decimal	Two matching numbers had different\n"
"		numbers of decimal places, or one had\n"
"		a decimal point and the other did\n"
"		not.\n"
"\n"
"    exponent	For two matching numbers one had an\n"
"		exponent but the other did not.\n"
"\n"
"    case	Two matching non-number tokens were\n"
"		the same letter but different case.\n"
"\n"
"    column	Two matching tokens ended in differ-\n"
"		ent columns.\n"
"\n"
"    nonblank	Two matching tokens were not both\n"
"		number tokens and were not the same\n"
"		letter except for case."
"\n"
"    none	If there are no differences, this is\n"
"		returned as the sole contents of the\n"
"		output line that lists differences.\n"
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
"\n"
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
"    character, tabs are set every 8 columns.\n"
"\n"
"    Note that if the two matching numbers have expo-\n"
"    nents and the letter case of the `e' or `E' in\n"
"    the two exponents does not match, then the dif-\n"
"    erence will always be reported as a `number'\n"
"    difference and not a `case' difference.\n"
"\n"
"    To avoid whitespace comparison anomalies, a new\n"
"    line is added in front of each file before the\n"
"    file is parsed.\n"
"\n"
"    When an end-of-file (eof) token is matched with\n"
"    a non-eof token, the whitespace preceding the\n"
"    non-eof token may be truncated before being com-\n"
"    pared with the whitespace preceding the eof\n"
"    token, to allow for the possibility that the eof\n"
"    occurred in the middle of whitespace, and not\n"
"    just before a non-eof-token.  First, if the\n"
"    whitespace before the eof token has N > 0 new\n"
"    lines, any N+1'st new line of the whitespace be-\n"
"    fore the non-eof token is deleted, as are all-\n"
"    characters following it.  Then, if now the\n"
"    whitespaces being compared have the same number\n"
"    of new lines, including possibly both having no\n"
"    new lines, we consider in each whitespace being\n"
"    compared the string of characters after the last\n"
"    new line in the whitespace, or the string of all\n"
"    the characters in the whitespace if the white-\n"
"    space has no new lines.  If the considered\n"
"    string for the eof exactly matches a prefix for\n"
"    the considered string of the non-eof, then all\n"
"    the characters after this prefix in the white-\n"
"    space preceding the non-eof token are deleted.\n"
"\n"
"    For each kind of difference found in the two\n"
"    files, the scorediff program constructs a\n"
"    `proof' of the difference.  A proof is a line\n"
"    with the syntax:\n"
"\n"
"          proof ::= difference-description\n"
"                    output-line-number\n"
"                    test-line-number\n"
"                    token-locator token-locator*\n"
"\n"
"          difference-description ::=\n"
"                    `nonblank' | `case' | `column' |\n"
"                    `decimal' | `exponent' |\n"
"                    `number' absolute-difference\n"
"                             relative-difference\n"
"\n"
"          absolute-difference ::=\n"
"                    floating-point-number\n"
"\n"
"          relative-difference ::=\n"
"                    floating-point-number\n"
"\n"
"          token-locator ::=\n"
"                    output-token-start-column\n"
"                    output-token-start-length\n"
"                    test-token-start-column\n"
"                    test-token-start-length\n"
"\n"
"    where the column numbers in a line start with 0.\n"
"    Only one token-locator is required for a proof,\n"
"    but proofs with the same difference description\n"
"    and line numbers are merged into proofs with\n"
"    than one token-locator.\n"
\n"
;

struct file
{
    ifstream stream;	// Input stream.
    char * filename;	// File name.

    // Token description.
    //
    char token [ MAX_SIZE + 1 ]; // The current token.
    int line;		// Line number of current token.
    			// First line is 1.
    int column;		// Column within the line of
    			// the last character of the
			// current token.  The first
			// column is 0.
    bool is_eof;	// True iff the current token
    			// is an end-of-file.
    bool is_number;	// True iff the current token
    			// is a number.
    double number;	// For a number token, the
    			// value of the number.
    int decimals;	// For a number token, the
    			// number of digits after the
    			// decimal point, or -1 if there
			// is no decimal point.
    bool has_exponent;	// For a number token, true iff
    			// the token has an exponent.

    // Whitespace description.
    //
    char token [ MAX_SIZE + 1 ]; // Whitespace preced-
    			// ing the current token.
    int newlines;	// Number of new line characters
    			// in this whitespace.

    // Set to backup in input string when scantoken
    // determines it has not been passed a number or
    // exponent.  Possible values are:
    //
    //		+    -    +.   -.
    //		e    e+   e-   E    E+   E-
    //
    // Also set to the single character following a
    // number, whatever that may be.
    //
    // Must be such that the backup is empty when
    // scantoken decides to put something in the
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
    f.filename = new char [strlen ( filename ) + 1 ];
    strcpy ( f.filename, filename );

    if ( ! f.stream ) {
        cout << "ERROR: not readable: "
	     << filename << endl;
    	exit ( 1 );
    }

    f.backup[0]		= '\n';
    f.backup[1]		= '\0';
    f.back		= f.backup;

    f.token[0]		= 0;
    f.column		= -1;
    f.line		= 0;
    f.is_number		= false;
    f.is_eof		= false;
    f.whitespace[0]	= 0;
}

// Get next character from file, respecting any backup.
// Returns next character in file or EOF if end of file.
//
inline int get_character ( file & f )
{
    int c = * f.back;

    if ( c != 0 )	++ f.back;
    else		c = f.stream.get();

    return c;
}

void whitespace_too_long ( file & f ) {
    cerr << "Whitespace too long at line "
         << f.line
	 << " of "
         << f.filename
	 << endl;
    exit 1
}

void token_too_long ( file & f ) {
    cerr << "Token too long at line "
         << f.line
	 << " of "
         << f.filename
	 << endl;
    exit 1
}


// Scan next token.
//
void scantoken ( file & f )
{
    if ( f.is_eof ) return;

    int c = get_character ( f );
    int column = f.column + 1;
    
    // Scan whitespace.
    //
    char * wp = file.whitespace;
    char * endwp = wp + MAX_SIZE;

    f.linebreaks = 0;

    while ( isspace ( c ) ) {
        if ( c == '\n' ) {
	    column = -1;
	    ++ f.linebreaks;
	    ++ f.line;
	}
	else if ( c == '\t' )
	    column += 7 - ( column % 8 );
	else if ( c == '\f' ) -- column;
	else if ( c == '\v' ) -- column;

	if ( wp < endwp ) * wp ++ = c;
	else whitespace_too_long ();

	c = get_character ( f );
	++ column;
    }

    * wp = 0;

    if ( c == EOF ) {
    	f.is_eof = true;
	f.column = column;
	f.is_number = false;
	f.token[0] = 0;
	return;
    }

    char * tp = f.token;
    char * endtp = tp + MAX_SIZE;
    int decimals = -1;

    switch ( c ) {

    case '+':
    case '-':
	* tp ++ = c;

	c = get_character ( f );
	++ column;

	switch ( c ) {
	case '.':
	    ++ decimals;

	    * tp ++ = c;

	    c = get_character ( f );
	    ++ column;

	    if ( ! isdigit ( c ) ) goto backout;
	    break;

	default:
	    if ( ! isdigit ( c ) ) goto backout;
	}
	break;

    case '.':
	* tp ++ = c;
	c = get_character ( f );
	++ column;
	++ decimals;
	if ( ! isdigit ( c ) ) goto backout;
	break;

    default:
        if ( ! isdigit ( c ) ) {
	    * tp ++ = c;
	    * tp ++ = '\0';
	    f.is_number = false;
	    f.column = column;
	    return;
	}
	break;
    }

    // Come here when c is the first digit of a number.

    * tp ++ = c;
    c = get_character ( f );
    ++ column;

    f.is_number = true;

    // Get rest of mantissa.
    //
    while ( 1 ) {
        if ( isdigit ( c ) ) {
	    if ( decimals >= 0 ) ++ decimals;
	} else if ( c == '.' ) {
	    if ( decimals < 0 ) ++ decimals;
	    else break;
	} else break;

	if ( tp < endtp ) * tp ++ = c;
	else token_too_long ();
	c = get_character ( f );
	++ column;
    }

    // Get exponent if present.
    //
    f.has_exponent = false;

    if ( c == 'e' || c == 'E' ) {

        char * expp = tp;
	int expcolumn = column;

	if ( tp < endtp ) * tp ++ = c;
	else token_too_long ();

	c = get_character ( f );
	++ column;

	if ( c == '+' || c == '-' ) {
	    if ( tp < endtp ) * tp ++ = c;
	    else token_too_long ();

	    c = get_character ( f );
	    ++ column;
	}

	if ( ! isdigit ( c ) ) {
	    assert ( * f.back == 0 );
	    * tp = 0;
	    strcp ( backup, expp );
	    f.back = f.backup;
	    tp = expp;
	    column = expcolumn;
	} else {
	    do {
		if ( tp < endtp ) * tp ++ = c;
		else token_too_long ();
		c = get_character ( f );
		++ column;
	    } while ( isdigit ( c ) );
	    f.has_exponent = true;
	}
    }

    * tp = 0;

    f.column = -- column;
    f.decimals = decimals;

    assert ( * f.back == 0 );

    if ( c != EOF ) {
	f.back = f.backup;
	f.backup[0] = c;
	f.backup[1] = 0;
    }

    char * e;
    f.number = strtod ( f.token, & e );
    assert ( e == f.end || ! finite ( f.number ) );
    	//
    	// If number is too large then f.number is
	// set to an infinity and e is not set to
	// the end of the number; which is probably
	// a bug in strtod.

    return;

// Come here if partial number scanned and found
// not to be a number.
//
backout:
    column -= tp - f.token;
    * tp ++ = c;
    * tp ++ = 0;
    strcpy ( f.backup, f.token + 1 );
    f.back = f.backup;
    f.token[1] = 0;
    f.column = column;
    f.is_number = false;
    return;
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
// or `has_exponent' members are unequal.
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

    if ( file1.has_exponent != file2.has_exponent )
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

		    assert ( file1.is_number );
		    assert ( file2.is_number );

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

		assert ( file1.is_number );
		assert ( file2.is_number );

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
		    file1.is_number || file2.is_number ) )
	{
	    follows_whitespace1 = false;
	    follows_whitespace2 = false;

	    if ( ( ! file1.is_number )
	         ||
		 ( ! file2.is_number ) )
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
