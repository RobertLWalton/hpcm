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
//   $Date: 2001/06/06 15:14:54 $
//   $RCSfile: scorediff.cc,v $
//   $Revision: 1.17 $

// This is version 2, a major revision of the first
// scorediff program.  This version is more explicitly
// token oriented.

#include <stdlib.h>
#include <limits.h>
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
"scorediff [options] output_file test_file\n"
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
"\f\n"
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
"\f\n"
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
"\f\n"
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
"    representations must match exactly, except that\n"
"    letter case (`e' versus `E') is ignored.\n"
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
"\f\n"
"    When an end-of-file (eof) token is matched with\n"
"    a non-eof token, no check is made of the white-\n"
"    space preceding the tokens."
"\n"
"    For each kind of difference found in the two\n"
"    files, the scorediff program outputs a `proof'\n"
"    of the difference.  Proofs are output on proof-\n"
"    lines that have the following syntax:\n"
"\n"
"          proof-line ::=\n"
"                    output-line-number\n"
"                    test-line-number\n"
"                    proof proof*\n"
"\n"
"          proof ::= difference-description\n"
"                    token-locator\n"
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
"                    output-token-end-column\n"
"                    test-token-end-column\n"
"\n"
"    where the column numbers in a line start with 0\n"
"    and the line numbers in a file start with 1.\n"
"    Here non-floating-point numbers are unsigned\n"
"    integers.  All the proofs with the same output\n"
"    test file line numbers are put on the same\n"
"    proof-line.\n"
"\f\n"
"    Program options may be used to surpress proofs\n"
"    the output of this program.  An option consist-\n"
"    ing of a `-' followed by difference name follow-\n"
"    ed by an unsigned integer N suppresses all but\n"
"    the first N proofs with that difference name.\n"
"    Thus `-case 5' suppresses all but the first 5\n"
"    `case' proofs.  If N is omitted, it is assumed\n"
"    to be 0 (and next program argument must NOT\n"
"    begin with a digit).  Thus `-case' with no\n"
"    following number suppresses all `case' proofs.\n"
"\n"
"    The `-number' option differs in that it has the\n"
"    form:\n"
"\n"
"        -number absolute-diff relative-diff N\n"
"\n"
"    and the program outputs only the first N\n"
"    `number' proofs that have an absolute or\n"
"    relative difference larger than the values given\n"
"    in the option.\n"
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
enum token_type {
    NUMBER_TOKEN, EOF_TOKEN, CHARACTER_TOKEN };

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

// The two files.
//
file output;
file test;

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
    f.type		= CHARACTER_TOKEN;
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
    if ( f.type == EOF_TOKEN ) return;

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
    	f.type		= EOF_TOKEN;
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

    f.type = NUMBER_TOKEN;

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
    f.type	= CHARACTER_TOKEN;

    * tp = 0;

    if ( c != EOF ) {
	assert ( * f.back == 0 );
	f.back = f.backup;
	f.backup[0] = c;
	f.backup[1] = 0;
    }

    return;
}

// Possible difference types.
//
enum difference_type {
    LINEBREAK,
    SPACEBREAK,
    WHITESPACE,
    BEGINSPACE,
    LINESPACE,
    ENDSPACE,
    EOF1,
    EOF2,
    NUMBER,
    DECIMAL,
    EXPONENT,
    CASE,
    COLUMN,
    NONBLANK,
    MAX_DIFFERENCE
};

// Difference data.
//
struct difference
{
    char *	name;
    bool	flag;
        // True if difference has been found.
    unsigned	proof_limit;
       // Decremented whenever a proof is output.
       // If zero, suppresses output of proofs.
};

difference differences[] = {
    { "linebreak", false, UINT_MAX },
    { "spacebreak", false, UINT_MAX },
    { "whitespace", false, UINT_MAX },
    { "beginspace", false, UINT_MAX },
    { "linespace", false, UINT_MAX },
    { "endspace", false, UINT_MAX },
    { "eof1", false, UINT_MAX },
    { "eof2", false, UINT_MAX },
    { "number", false, UINT_MAX },
    { "decimal", false, UINT_MAX },
    { "exponent", false, UINT_MAX },
    { "case", false, UINT_MAX },
    { "column", false, UINT_MAX },
    { "nonblank", false, UINT_MAX }
};

// Maximum numeric differences found so far.
//
double absdiff_maximum	= 0.0;
double reldiff_maximum	= 0.0;

// Numeric differences equal to or below these are
// NOT output as proofs.
//
double absdiff_limit	= 0.0;
double reldiff_limit	= 0.0;

struct proof
{
    difference_type	type;
    unsigned		output_token_end_column;
    unsigned		test_token_end_column;
    double		absdiff;
    double		reldiff;
    proof *		next;
};

struct proof_line
{
    unsigned		output_line_number;
    unsigned		test_line_number;
    proof *		proofs;
    proof_line *	next;
};

proof_line *	first_proof_line	= NULL;
proof_line *	last_proof_line		= NULL;
proof *		last_proof		= NULL;

// Output a new proof.
//
inline void output_proof
	( difference_type type,
	  double absdiff = 0.0,
	  double reldiff = 0.0 )
{

    if ( last_proof_line == NULL
	 ||
         last_proof_line->output_line_number
	 != output.line
	 ||
	 last_proof_line->test_line_number
	 != test.line )
    {
        proof_line * pline		= new
	                                  proof_line;
	pline->output_line_number	= output.line;
	pline->test_line_number		= test.line;
	pline->proofs			= NULL;

	last_proof			= NULL;

	if ( last_proof_line )
	{
	    last_proof_line->next	= pline;
	    last_proof_line		= pline;
	}
	else
	{
	    first_proof_line		= pline;
	    last_proof_line		= pline;
	}
    }

    proof * p	= new proof;

    p->type			= type;
    p->output_token_end_column	= output.column;
    p->test_token_end_column	= test.column;
    p->absdiff			= absdiff;
    p->reldiff			= reldiff;
    p->next			= NULL;

    if ( last_proof == NULL )
    {
        last_proof		= p;
	last_proof_line->proofs	= p;
    }
    else
    {
        last_proof->next	= p;
    }

    -- differences[type].proof_limit;
}

// Record a found difference.
//
inline void found_difference
	( difference_type type,
	  double absdiff = 0.0,
	  double reldiff = 0.0 )
{
    differences[type].flag = true;
    if ( differences[type].proof_limit > 0
         && ( type != NUMBER
	      || absdiff > absdiff_limit
	      || reldiff > reldiff_limit ) )
        output_proof ( type, absdiff, reldiff );
}

// Tests two numbers just scanned for the output and
// test files to see if there is a computable differ-
// ence.  If so, calls found_difference (NUMBER), and
// updates `absdiff_maximum' and `reldiff_maximum' by
// writing the differences just found into them iff
// these new differences are larger than the previous
// value of `absdiff_maximum' or `reldiff_maximum',
// respectively.  Also calls found_difference for
// DECIMAL or EXPONENT if the two number `decimals'
// or `has_exponent' file members are unequal.
//
// If there is no computable difference, calls found_
// difference(NONBLANK) instead.  This happens if one of
// the numbers is not `finite' or their difference is
// not `finite'.
// 
void diffnumber ()
{
    if ( ! finite ( output.number )
	 ||
	 ! finite ( test.number ) )
    {
	found_difference ( NONBLANK );
	return;
    }

    double absdiff =
	( output.number - test.number );
    if ( ! finite ( absdiff ) )
    {
	found_difference ( NONBLANK );
	return;
    }
    if ( absdiff < 0 ) absdiff = - absdiff;

    double abs1 = output.number;
    if ( abs1 < 0 ) abs1 = - abs1;

    double abs2 = test.number;
    if ( abs2 < 0 ) abs2 = - abs2;

    double max = abs1;
    if ( max < abs2 ) max = abs2;

    double reldiff = absdiff == 0.0 ?
                     0.0 :
		     absdiff / max;

    if ( ! finite ( reldiff ) )
    {
        // Actually, this should never happen.

	found_difference ( NONBLANK );
	return;
    }

    found_difference ( NUMBER, absdiff, reldiff );

    if ( absdiff > absdiff_maximum )
	absdiff_maximum = absdiff;

    if ( reldiff > reldiff_maximum )
	reldiff_maximum = reldiff;

    if ( output.decimals != test.decimals )
	found_difference ( DECIMAL );

    if ( output.has_exponent != test.has_exponent )
	found_difference ( EXPONENT );
}

// Main program.
//
int main ( int argc, char ** argv )
{

    // Process options.

    while ( argc >= 3 && argv[1][0] == '-' )
    {

	char * name = argv[1] + 1;

        if ( strcmp ( "number", name ) == 0 )
	{
	    // special case.

	    if ( argc < 5 ) break;

	    absdiff_limit = atof ( argv[2] );
	    absdiff_limit = atof ( argv[3] );
	    ++ argv, -- argc;
	    ++ argv, -- argc;
	}
        else if ( strcmp ( "doc", name ) == 0 )
	    break;

	int i; for ( i = 0; i < MAX_DIFFERENCE; ++ i )
	{
	    if ( strcmp ( differences[i].name, name )
	         == 0 ) break;
	}

    	if ( i < MAX_DIFFERENCE )
	    differences[i].proof_limit
	    	= (unsigned) atol ( argv[2] );
	else
	{
	    cerr << "Unrecognized option -"
		 << name
		 << endl;
	    exit (1);
	}

        ++ argv, -- argc;
	if ( isdigit ( argv[1][0] ) )
		++ argv, -- argc;
    }

    // Print documentation and exit with error status
    // unless there are exactly two program arguments.

    if ( argc != 3 )
    {
        cout << documentation;
	exit (1);
    }

    // Open files.

    open ( output, argv[1] );
    open ( test, argv[2] );

    // Loop that reads the two files and compares their
    // tokens, setting flags describing any differences
    // found.

    // Scan the first tokens.
    //
    scan_token ( output );
    scan_token ( test );

    bool done		= false;

    while ( ! done )
    {
        // Terminate loop if just one file has an
	// EOF_TOKEN.

	if ( output.type == EOF_TOKEN
	     && test.type != EOF_TOKEN )
	{
	    found_difference ( EOF1 );
	    done = true;
	    break;
	}
	else
	if ( test.type == EOF_TOKEN
	     && output.type != EOF_TOKEN )
	{
	    found_difference ( EOF2 );
	    done = true;
	    break;
	}

        // Compare whitespace preceding tokens.

	if ( output.newlines != test.newlines )
	    found_difference ( LINEBREAK );
	else
	{
	    char * wp1		= output.whitespace;
	    char * wp2		= test.whitespace;
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
		    if ( output.newlines == 0 )
			found_difference ( WHITESPACE );
		    else
			found_difference ( BEGINSPACE );
		}
		else if ( newlines == output.newlines )
		    found_difference ( ENDSPACE );
		else
		    found_difference ( LINESPACE );
	    }
	}
        
	// Compare tokens.
	//
        switch ( output.type ) {
	case NUMBER_TOKEN:
	case CHARACTER_TOKEN:
	    switch ( test.type ) {
	    case EOF_TOKEN:
		found_difference ( EOF2 );
		break;
	    case NUMBER_TOKEN:
	    case CHARACTER_TOKEN:

	        if ( output.type != test.type )
		{
		    found_difference ( NONBLANK );
		    break;
		}

	    	char * tp1 = output.tokens;
	    	char * tp2 = test.tokens;
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
		    if ( output.type != NUMBER_TOKEN
		         && token_case )
		        found_difference ( CASE );
		    break;
		}

		else if ( output.type != NUMBER_TOKEN )
		{
		    found_difference ( NONBLANK );
		    done = true;
		    break;
		}

		diffnumber ();

		if ( differences[NONBLANK].flag )
		    done = true;
		break;
	    }
	    break;

	case EOF_TOKEN:
            assert ( test.type == EOF_TOKEN );
	    done = true;
	    break;
     	}
    }

    // Loop done and difference flags are now computed.
    // Produce output line according to difference
    // flags.

    bool any = false;

    for ( int i = 0; i < MAX_DIFFERENCE; ++ i )
    {
        if ( differences[i].flag )
	{
	    if ( any ) cout << " ";
	    cout << differences[i].name;
	    if ( i == NUMBER )
		cout << " " << absdiff_maximum
		     << " " << reldiff_maximum;
	    any = true;
	}
    }

    cout << (any ? "" : "none") << endl;

    for ( proof_line * pline = first_proof_line;
          pline != NULL;
	  pline = pline->next )
    {
        cout << pline->output_line_number << " "
             << pline->test_line_number;

	for ( proof * p = pline->proofs;
	      p != NULL;
	      p = p->next )
	{
	    cout << " " << differences[p->type].name;
	    if ( p->type == NUMBER )
	    {
		cout << " " << p->absdiff;
		cout << " " << p->reldiff;
	    }
	    cout << " " << p->output_token_end_column;
	    cout << " " << p->test_token_end_column;
	}
	cout << endl;
    }

    // Return from main function without error.

    return 0;
}
