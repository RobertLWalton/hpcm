// Programming Contest File Difference Tester
//
// File:	scorediff.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Mon Oct 30 06:10:10 EST 2000
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: hc3 $
//   $Date: 2001/06/06 05:21:26 $
//   $RCSfile: scorediff.cc,v $
//   $Revision: 1.16 $

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
#define MAX_SIZE 10200

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
"    end-of-file (eof).\n"
"\n"
"    The types of differences are:\n"
"\n"
"    linebreak	For two matching non-eof tokens, the\n"
"		preceding whitespace of one has a\n"
"		different number of new lines than\n"
"		the preceding whitespace of the\n"
"		other.\n"
"\n"
"    whitespace	For two matching non-eof tokens, the\n"
"		preceding whitespaces both have no\n"
"		new lines, and both are non-empty,\n"
"		but these whitespaces do not match\n"
"		exactly.\n"
"\n"
"    spacebreak	For two matching non-eof tokens, one\n"
"		is preceded by whitespace containing\n"
"		no new lines, and the other has no\n"
"		preceding whitespace at all."
"\n"
"    endspace	For two matching non-eof tokens, the\n"
"		preceding whitespaces have the same\n"
"		number N >= 1 of new lines, and the\n"
"		whitespace characters preceding the\n"
"		first new line for one token do not\n"
"		exactly match the whitespace charac-\n"
"		ters preceding the first new line for\n"
"		the other token.\n"
"\n"
"    linespace	For two matching non-eof tokens, the\n"
"		preceding whitespaces have the same\n"
"		number N >= 1 of new lines, and the\n"
"		whitespace characters between some\n"
"		pair of matching new lines of the two\n"
"		token preceding whitespaces do not\n"
"		exactly agree.\n"
"\n"
"    beginspace	For two matching non-eof tokens, the\n"
"		preceding whitespaces have the same\n"
"		number N >= 1 of new lines, and the\n"
"		whitespace characters following the\n"
"		last new line for one token do not\n"
"		exactly match the whitespace charac-\n"
"		ters following the last new line for\n"
"		the other token.\n"
"\n"
"    eof1	When the first file ends, the second\n"
"		file has a remaining token.  In this\n"
"		case preceding whitespaces are NOT\n"
"		compared."
"\n"
"    eof2	When second file ends, the first\n"
"		file has a remaining token.  In this\n"
"		case preceding whitespaces are NOT\n"
"		compared."
"\n"
"    number A R	For two matching number tokens, the\n"
"		token character strings do not match\n"
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
"    decimal	Two matching numbers have different\n"
"		numbers of decimal places, or one has\n"
"		a decimal point and the other does\n"
"		not.\n"
"\n"
"    exponent	For two matching numbers one has an\n"
"		exponent but the other does not.\n"
"\n"
"    case	Two matching non-number tokens are\n"
"		the same letter but different case.\n"
"\n"
"    column	Two matching tokens end in differ-\n"
"		ent columns.\n"
"\n"
"    nonblank	Two matching tokens are not both\n"
"		number tokens and are not the same\n"
"		letter ignoring case."
"\n"
"    none	There are no differences in the files\n"
"		at all.  This may be returned as the\n"
"		sole contents of an output line that\n"
"		lists differences.\n"
"\n"
"    The files are parsed into whitespace, numbers,\n"
"    and other characters.  A number is an optional\n"
"    sign followed by digits containing an optional\n"
"    decimal point followed by an optional exponent.\n"
"    An exponent is an `e' or `E' followed by an\n"
"    an optional sign followed by digits.  A number\n"
"    is scanned by the strtod(3) function.  Numbers\n"
"    longer than 10,100 characters are arbitrarily\n"
"    truncated.\n"
"\n"
"    The relative value of the difference between two\n"
"    numbers x and y is:\n"
"\n"
"                        | x - y |\n"
"                     ----------------\n"
"                     max ( |x|, |y| )\n"
"\n"
"    and is never larger than 2.  If x == y == 0 this\n"
"    relative difference is taken to be zero.\n"
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
"    a non-eof token, no check is made of the white-\n"
"    space preceding the tokens."
"\n"
"    For each kind of difference found in the two\n"
"    files, the scorediff program outputs a `proof'\n"
"    of the difference.  A proof is a line with the \n"
"    syntax:\n"
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
"    Here non-floating-point numbers are unsigned\n"
"    integers.  Only one token-locator is required\n"
"    for a proof, but proofs with the same difference\n"
"    description and line numbers are merged to form\n"
"    proofs with more than one token-locator.\n"
"\n"
"    The first `nonblank' difference found terminates\n"
"    this program and the search for more differ-\n"
"    ences.\n"
"\n"
;

// A token is either a number token, an end of file
// (eof) token, or character token.  This last means a
// any non-whitespace character not in a number.
//
enum token_type { NUMBER, END_OF_FILE, CHARACTER };

struct file
{
    ifstream stream;	// Input stream.
    char * filename;	// File name.

    // Token description.
    //
    token_type type;	// Type of tokens.
    char tokens [ MAX_SIZE + 1 ]; // The current tokens,
    			// if not an end-of-file (eof).
    			// There is either just one nu-
			// meric token, or there
			// are one or more consecutive
			// character tokens that were
			// scanned together for effici-
			// ency.
    int count;		// Number of tokens.  1 for
    			// number or eof token.
    int line;		// Line number of current
    			// tokens.  The first line is 1.
    int column;		// Column within the line of
    			// the last character of the
			// last of the current tokens.
			// The first column is 0.

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
    char whitespace [ MAX_SIZE + 1 ]; // Whitespace pre-
    			// ceding the current tokens.
    int wslength;	// Number of characters of
    			// whitespace in above.
    int newlines;	// Number of new line characters
    			// in this whitespace.

    // Backup description.
    //
    // Set to characters backed over in the input string
    // when scan_token determines it has not been passed
    // an exponent.  Possible values are:
    //
    //		e    e+   e-   E    E+   E-
    //
    // Also set to the single character following a
    // number or sequence of non-number tokens.

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
    f.filename = new char [strlen ( filename ) + 1 ];
    strcpy ( f.filename, filename );

    f.stream.open ( filename );
    if ( ! f.stream ) {
        cout << "ERROR: not readable: "
	     << filename << endl;
    	exit ( 1 );
    }

    f.backup[0]		= '\n';
    f.backup[1]		= '\0';
    f.back		= f.backup;

    f.tokens[0]		= 0;
    f.column		= -1;
    f.line		= 0;
    f.type		= CHARACTER;
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
    exit (1);
}

void token_too_long ( file & f ) {
    cerr << "Token too long at line "
         << f.line
	 << " of "
         << f.filename
	 << endl;
    exit (1);
}


// Scan next token.
//
void scan_token ( file & f )
{
    if ( f.type == END_OF_FILE ) return;

    int c = get_character ( f );
    int column = f.column + 1;
    
    // Scan whitespace.
    //
    char * wp = f.whitespace;
    char * endwp = wp + MAX_SIZE;

    f.newlines = 0;

    while ( isspace ( c ) ) {
        if ( c == '\n' ) {
	    column = -1;
	    ++ f.newlines;
	    ++ f.line;
	}
	else if ( c == '\t' )
	    column += 7 - ( column % 8 );
	else if ( c == '\f' ) -- column;
	else if ( c == '\v' ) -- column;
	else if ( c == '\r' ) column = -1;


	if ( wp < endwp ) * wp ++ = c;
	else whitespace_too_long ( f );

	c = get_character ( f );
	++ column;
    }

    * wp = 0;

    if ( c == EOF ) {
    	f.type		= END_OF_FILE;
	f.count		= 1;
	f.column	= column;
	f.tokens[0]	= 0;
	return;
    }

    char * tp = f.tokens;
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

	    if ( ! isdigit ( c ) ) goto non_number;
	    break;

	default:
	    if ( ! isdigit ( c ) ) goto non_number;
	}
	break;

    case '.':
	* tp ++ = c;
	c = get_character ( f );
	++ column;
	++ decimals;
	if ( ! isdigit ( c ) ) goto non_number;
	break;

    default:
        if ( ! isdigit ( c ) ) goto non_number;
	break;
    }

    // Come here when c is the first digit of a number.

    * tp ++ = c;
    c = get_character ( f );
    ++ column;

    f.type = NUMBER;

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
	else token_too_long ( f );
	c = get_character ( f );
	++ column;
    }

    // Get exponent if present.
    //
    f.has_exponent = false;

    if ( c == 'e' || c == 'E' ) {

        char * ep = tp;
	int ecolumn = column;

	if ( tp < endtp ) * tp ++ = c;
	else token_too_long ( f );

	c = get_character ( f );
	++ column;

	if ( c == '+' || c == '-' ) {
	    if ( tp < endtp ) * tp ++ = c;
	    else token_too_long ( f );

	    c = get_character ( f );
	    ++ column;
	}

	if ( ! isdigit ( c ) ) {
	    assert ( * f.back == 0 );
	    * tp = 0;
	    strcpy ( f.backup, ep );
	    f.back = f.backup;
	    tp = ep;
	    column = ecolumn;
	} else {
	    do {
		if ( tp < endtp ) * tp ++ = c;
		else token_too_long ( f );
		c = get_character ( f );
		++ column;
	    } while ( isdigit ( c ) );
	    f.has_exponent = true;
	}
    }

    * tp = 0;

    f.column	= -- column;
    f.decimals	= decimals;

    if ( c != EOF ) {
	char * p = f.back;
	if ( * p == 0 ) p = f.backup;
	else p = f.back + strlen ( f.back );
	* p ++ = c;
	* p = 0;
	f.back = f.backup;
    }

    char * e;
    f.number = strtod ( f.tokens, & e );
    assert ( e == tp || ! finite ( f.number ) );
    	//
    	// If number is too large then f.number is
	// set to an infinity and e is not set to
	// the end of the number; which is probably
	// a bug in strtod.

    return;

// Come here if we have concluded that the characters
// scanned into f.tokens so far are not part of a
// number or eof token.
//
non_number:

    while ( 1 ) {
        if ( isspace ( c )
	     || isdigit ( c )
	     || c == '+'
	     || c == '-'
	     || c == '.'
	     || c == EOF ) break;

	if ( tp >= endtp ) break;

	* tp ++ = c;
	c = get_character ( f );
	++ column;
    }
    f.column	= -- column;
    f.count	= tp - f.tokens;
    f.type	= CHARACTER;

    * tp = 0;

    if ( c != EOF ) {
	assert ( * f.back == 0 );
	f.back = f.backup;
	f.backup[0] = c;
	f.backup[1] = 0;
    }

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
        // Actually, this should never happen.

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

    // Loop that reads the two files and compares their
    // tokens, setting flags describing any differences
    // found.

    bool linebreak	= false;	// Difference
    bool spacebreak	= false;	// flags.
    bool whitespace	= false;
    bool beginspace	= false;
    bool linespace	= false;
    bool endspace	= false;
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

    // Scan the first tokens.
    //
    scan_token ( file1 );
    scan_token ( file2 );

    bool done		= false;

    while ( ! done )
    {
        // Terminate loop if just one file has an END_OF_FILE.

	if ( file1.type == END_OF_FILE
	     && file2.type != END_OF_FILE )
	{
	    done = eof1 = true;
	    break;
	}
	else
	if ( file2.type == END_OF_FILE
	     && file1.type != END_OF_FILE )
	{
	    done = eof2 = true;
	    break;
	}

        // Compare whitespace preceding tokens.

	if ( file1.newlines != file1.newlines )
	    linebreak = true;
	else
	{
	    char * wp1		= file1.whitespace;
	    char * wp2		= file2.whitespace;
	    int newlines	= 0;

	    while (1) {
		while ( * wp1 == * wp2 ) {
		    if ( * wp1 == 0 ) break;
		    if ( * wp1 == '\n' ) ++ newlines;
		    ++ wp1, ++ wp2;
		}

		if ( * wp1 == * wp2 ) break;

		while ( * wp1 && * wp1 != '\n' ) ++ wp1;
		while ( * wp2 && * wp2 != '\n' ) ++ wp2;

		if ( newlines == 0 ) {
		    if ( file1.newlines == 0 )
			whitespace = true;
		    else
			beginspace = true;
		}
		else if ( newlines == file1.newlines )
		    endspace = true;
		else
		    linespace = true;
	    }
	}
        
	// Compare tokens.
	//
        switch ( file1.type ) {
	case NUMBER:
	case CHARACTER:
	    switch ( file2.type ) {
	    case END_OF_FILE:
	    	done = eof2 = true;
		break;
	    case NUMBER:
	    case CHARACTER:

	        if ( file1.type != file2.type )
		{
		    done = nonblank = true;
		    break;
		}

	    	char * tp1 = file1.tokens;
	    	char * tp2 = file2.tokens;
		bool token_case = false;

		while ( * tp1 )
		{
		    if ( * tp1 != * tp2 )
		    {
			if ( toupper ( * tp1 )
			     == toupper ( * tp2 ) )
			    token_case = true;
			else
			    break;
		    }
		    ++ tp1, ++ tp2;
		}

	        if ( * tp1 == * tp2 )
		{
		    if ( file1.type != NUMBER
		         && token_case )
		        caseflag = true;
		    break;
		}

		else if ( file1.type != NUMBER )
		{
		    done = nonblank = true;
		    break;
		}

		diffnumber ( file1, file2,
			     nonblank, number,
			     decimal, exponent,
			     number_absdiff,
			     number_reldiff );

		if ( nonblank ) done = true;
		break;
	    }
	    break;

	case END_OF_FILE:
            assert ( file2.type == END_OF_FILE );
	    done = true;
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
