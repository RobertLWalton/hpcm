// Programming Contest File Difference Tester
//
// File:	scorediff.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Sun Jul  1 03:14:51 EDT 2001
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: hc3 $
//   $Date: 2001/07/01 07:06:05 $
//   $RCSfile: scorediff.cc,v $
//   $Revision: 1.24 $

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
"    definition either an end-of-file (eof) or a\n"
"    number or a word.   A word is just a string of\n"
"    non-whitespace characters that does not contain\n"
"    a number.\n"
"\n"
"    The types of differences are:\n"
"\n"
"    linebreak	For two matching tokens, the preced-\n"
"		ing whitespace of one has a different\n"
"		number of new lines than the preced-\n"
"		ing whitespace of the other.\n"
"\n"
"    whitespace	For two matching tokens, the preced-\n"
"		ing whitespaces both have no new\n"
"		lines, and both are non-empty, but\n"
"		these whitespaces do not match\n"
"		exactly.\n"
"\n"
"    spacebreak	For two matching tokens, one is pre-\n"
"		ceded by whitespace containing no new\n"
"		lines, and the other has no preceding\n"
"		whitespace at all.\n"
"\f\n"
"    endspace	For two matching tokens, the preced-\n"
"		ing whitespaces have the same number\n"
"		N >= 1 of new lines, and the white-\n"
"		space characters preceding the first\n"
"		new line for one token do not exactly\n"
"		match the whitespace characters pre-\n"
"		ceding the first new line for the\n"
"		other token.\n"
"\n"
"    linespace	For two matching tokens, the preced-\n"
"		ing whitespaces have the same number\n"
"		N >= 1 of new lines, and the white-\n"
"		space characters between some pair of\n"
"		matching new lines of the two token\n"
"		preceding whitespaces do not exactly\n"
"		agree.\n"
"\n"
"    beginspace	For two matching tokens, the preced-\n"
"		ing whitespaces have the same number\n"
"		N >= 1 of new lines, and the white-\n"
"		space characters following the last\n"
"		new line for one token do not exactly\n"
"		match the whitespace characters\n"
"		following the last new line for the\n"
"		other token.\n"
"\n"
"    eof1	When the first file ends, the second\n"
"		file has a remaining non-eof token.\n"
"		In this case, preceding whitespaces\n"
"		are NOT compared.\n"
"\n"
"    eof2	When the second file ends, the first\n"
"		file has a remaining non-eof token.\n"
"		In this case, preceding whitespaces\n"
"		are NOT compared.\n"
"\f\n"
"    float A R	For two matching number tokens,\n"
"		at least ONE of which contains a\n"
"		a decimal point or exponent, the\n"
"		token character strings do not match\n"
"		exactly, the absolute difference of\n"
"		the numbers is A, and the relative\n"
"		difference of the numbers is R.\n"
"\n"
"		In the line returned reporting diff-\n"
"		erences in the two files, A and R\n"
"		are the maximum observed A and R\n"
"		values for all observed `float' diff-\n"
"		erences.\n"
"\n"
"    integer A R    Same as `float A R', but for two\n"
"		matching number tokens, NEITHER of\n"
"		which contains a a decimal point or\n"
"		exponent.\n"
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
"		NOT identical, but would be identical\n"
"		if letter case differences were\n"
"		ignored.\n"
"\n"
"    column	Two matching tokens end in differ-\n"
"		ent columns.\n"
"\f\n"
"    nonblank	At least one of two matching tokens\n"
"		is a word token, and it is not true\n"
"		that both are word tokens of the same\n"
"		length (see word splitting below)\n"
"		that are identical except for letter\n"
"		case.\n"
"\n"
"		Or two matching number tokens cannot\n"
"		be compared because they are NOT\n"
"		identical except for letter case and\n"
"		one or both or their difference is\n"
"		too large to be represented as a\n"
"		finite double precision floating\n"
"		point number.\n"
"\n"
"    none	There are no differences in the files\n"
"		at all.  This is returned as the sole\n"
"		contents of an output line that lists\n"
"		differences.\n"
"\n"
"    The files are parsed into whitespace, numbers,\n"
"    words, and end-of-files (eofs).  A number is an\n"
"    optional sign followed by digits containing an\n"
"    optional decimal point followed by an optional\n"
"    exponent.  An exponent is an `e' or `E' followed\n"
"    by an an optional sign followed by digits.  A\n"
"    number is scanned by the strtod(3) function.\n"
"\n"
"    A word is a string of non-whitespace characers\n"
"    that does not contain a number.  If two words S\n"
"    and L are being matched, and S is shorter than\n"
"    L, then L is split into a first word of the same\n"
"    length as S and a second word to be matched to\n"
"    the token that follows S.  Thus any two words\n"
"    being matched are forced to have the same\n"
"    length.\n"
"\f\n"
"    It is an error if a token longer than 10,100\n"
"    characters is found, or if a sequence of con-\n"
"    secutive whitespace characters longer than\n"
"    10,100 characters is found.\n"
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
"    Numbers that match exactly, character by charac-\n"
"    ter, do not produce any difference indication.\n"
"    Numbers that match exactly except for the case\n"
"    of an exponent indicator (`e' versus `E') always\n"
"    produce a `float 0 0' difference indication.\n"
"    Other than these cases, numbers too large to\n"
"    compare (either they or their difference are\n"
"    infinity when represented as double precision\n"
"    floating point numbers) cause a `nonblank' diff-\n"
"    erence.\n"
"\n"
"    For the purpose of computing the column of a\n"
"    character, tabs are set every 8 columns.\n"
"\n"
"    Note that if the two matching numbers have expo-\n"
"    nents and the letter case of the `e' or `E' in\n"
"    the two exponents does not match, then the dif-\n"
"    erence will always be reported as a `float'\n"
"    difference and not a `case' difference.\n"
"\f\n"
"    To avoid whitespace comparison anomalies, a new\n"
"    line is added in front of each file before the\n"
"    file is parsed.  Thus differences in whitespace\n"
"    beginning the first line of a file are reported\n"
"    as `beginspace' differences.\n"
"\n"
"    When an end-of-file (eof) token is matched with\n"
"    a non-eof token, no check is made of the white-\n"
"    space preceding the tokens.\n"
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
"                    `integer' absolute-difference\n"
"                              relative-difference |\n"
"                    `integer' absolute-difference\n"
"                              relative-difference\n"
"\n"
"          absolute-difference ::=\n"
"                    floating-point-number\n"
"\n"
"          relative-difference ::=\n"
"                    floating-point-number\n"
"\f\n"
"          token-locator ::=\n"
"                    output-token-end-column\n"
"                    test-token-end-column\n"
"\n"
"    where the column numbers in a line start with 0\n"
"    and the line numbers in a file start with 1.\n"
"    Here non-floating-point numbers output as part\n"
"    of proofs are unsigned integers.  All the proofs\n"
"    with the same output and test file line numbers\n"
"    are put on the same proof-line.\n"
"\n"
"    Program options may be used to suppress the out-\n"
"    put of proofs by this program.  An option con-\n"
"    sisting of a `-' followed by difference name\n"
"    followed by an unsigned integer N suppresses all\n"
"    but the first N proofs with that difference\n"
"    name.  Thus `-case 5' suppresses all but the\n"
"    first 5 `case' proofs.  If N is omitted, it is\n"
"    assumed to be 0 (and next program argument must\n"
"    NOT begin with a digit).  Thus `-case' with no\n"
"    following number suppresses all `case' proofs.\n"
"\n"
"    The `-float' and `-integer' program options\n"
"    differ in that they have the forms:\n"
"\n"
"        -float absolute-diff relative-diff N\n"
"        -integer absolute-diff relative-diff N\n"
"\n"
"    and the program outputs only the first N `float'\n"
"    or `integer' proofs that have an absolute or\n"
"    relative difference larger than the values given\n"
"    in the program option.  In these options the\n"
"    differences may be omitted if they are zero and\n"
"    the program argument following them does NOT\n"
"    begin with a digit or decimal point.\n"
"\n"
"    The first `nonblank' difference found terminates\n"
"    this program and the search for more differ-\n"
"    ences.\n"
;

// A token is either a number token, an end of file
// (eof) token, or a word token.
//
enum token_type {
    NUMBER_TOKEN, EOF_TOKEN, WORD_TOKEN };

struct file
{
    ifstream stream;	// Input stream.
    char * filename;	// File name.

    // Token description.
    //
    token_type type;	// Type of token.
    char token [ MAX_SIZE + 1 ]; // The current token,
    			// if not an end-of-file (eof).
    int length;		// Length of token in charac-
    			// ters.  0 for eof.
    int line;		// Line number of current
    			// token.  The first line is 1.
    int column;		// Column within the line of
    			// the last character of the
			// the current token.  The first
			// column is 0.

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
    			// ceding the current token.
    int newlines;	// Number of new line characters
    			// in this whitespace.

    // Token split description.
    //
    // If a word token is split, this information is
    // set so the next call to scan a token will return
    // the remainder of the split token.
    //
    int remainder_length;  // Length of remainder.
    			// 0 if no remainder.
    char remainder_c;	// First character of remainder.

    // Backup description.
    //
    // Set to characters backed over in the input string
    // when scan_token determines that some characters
    // encountered scanning a word begin a number or that
    // some characters encountered scanning a number do
    // not begin an exponent.  Possible values are:
    //
    //		+#   -#   .#   +.#  -.#
    //		e    e+   e-   E    E+   E-
    //
    // where # denotes any digit.  Can also be set to a
    // single character following a token.

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

    f.token[0]		= 0;
    f.column		= -1;
    f.line		= 0;
    f.type		= WORD_TOKEN;
    f.whitespace[0]	= 0;

    f.remainder_length	= 0;
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


// Scan next token in a file.
//
void scan_token ( file & f )
{
    if ( f.type == EOF_TOKEN ) return;

    if ( f.remainder_length != 0 )
    {
        assert ( f.type == WORD_TOKEN );

	char * p = f.token + f.length;
	char * q = f.token;

	* p = f.remainder_c;
	while ( * q ++ = * p ++ );

	f.length		= f.remainder_length;
	f.column		+= f.remainder_length;
	f.remainder_length	= 0;

	f.whitespace[0]		= 0;
	f.newlines		= 0;

	return;
    }

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

    // Come here then c is the first character of the
    // token.

    if ( c == EOF ) {
    	f.type		= EOF_TOKEN;
	f.length	= 0;
	f.column	= column;
	f.token[0]	= 0;
	return;
    }

    // Come here when c is the first character of a
    // word or number token.

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

	    if ( ! isdigit ( c ) ) goto word;
	    break;

	default:
	    if ( ! isdigit ( c ) ) goto word;
	}
	break;

    case '.':
	* tp ++ = c;
	c = get_character ( f );
	++ column;
	++ decimals;
	if ( ! isdigit ( c ) ) goto word;
	break;

    default:
        if ( ! isdigit ( c ) ) goto word;
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
	if ( * p == 0 ) p = f.back = f.backup;
	else p = f.back + strlen ( f.back );
	* p ++ = c;
	* p = 0;
    }

    char * e;
    f.number = strtod ( f.token, & e );
    assert ( e == tp || ! finite ( f.number ) );
    	//
    	// If number is too large then f.number is
	// set to an infinity and e is not set to
	// the end of the number; which is probably
	// a bug in strtod.

    return;

// Come here if we have concluded that the characters
// scanned into f.token so far are part of a word,
// and c is the next character of the word or is a
// whitespace character.
//
word:

    while ( 1 ) {
        if (    isspace ( c )
	     || isdigit ( c )
	     || c == EOF ) break;

	switch ( c ) {

	case '+':
	case '-':
	case '.':

	    char * np = tp;
	    int ncolumn = column;
	    int oldc = c;

	    if ( tp < endtp ) * tp ++ = c;
	    else token_too_long ( f );

	    c = get_character ( f );
	    ++ column;

	    if ( c == '.' && oldc != '.' ) {
		if ( tp < endtp ) * tp ++ = c;
		else token_too_long ( f );

		c = get_character ( f );
		++ column;
	    }

	    if ( isdigit ( c ) ) {
		assert ( * f.back == 0 );
		* tp = 0;
		strcpy ( f.backup, np );
		f.back = f.backup;
		tp = np;
		column = ncolumn;
		goto end_word;
	    } else continue;
	}

	if ( tp < endtp ) * tp ++ = c;
	else token_too_long ( f );

	c = get_character ( f );
	++ column;
    }

end_word:

    f.column	= -- column;
    f.length	= tp - f.token;
    f.type	= WORD_TOKEN;

    * tp = 0;

    if ( c != EOF ) {
	char * p = f.back;
	if ( * p == 0 ) p = f.back = f.backup;
	else p = f.back + strlen ( f.back );
	* p ++ = c;
	* p = 0;
    }

    return;
}

// Split word token so first part has n characters.
//
void split_word ( file & f, int n )
{
    assert ( f.type == WORD_TOKEN );
    assert ( n < f.length );

    cout << "SPLIT " << f.token << " " << n << endl;

    f.remainder_length = f.length - n;
    char * p = f.token + n;
    f.remainder_c = * p;
    * p = 0;
    f.length = n;
    f.column -= f.remainder_length;
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
    FLOAT,
    INTEGER,
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
    { "float", false, UINT_MAX },
    { "integer", false, UINT_MAX },
    { "decimal", false, UINT_MAX },
    { "exponent", false, UINT_MAX },
    { "case", false, UINT_MAX },
    { "column", false, UINT_MAX },
    { "nonblank", false, UINT_MAX }
};

// Maximum numeric differences found so far.
//
double float_absdiff_maximum	= 0.0;
double float_reldiff_maximum	= 0.0;
double integer_absdiff_maximum	= 0.0;
double integer_reldiff_maximum	= 0.0;

// Numeric differences equal to or below these are
// NOT output as proofs.
//
double float_absdiff_limit	= 0.0;
double float_reldiff_limit	= 0.0;
double integer_absdiff_limit	= 0.0;
double integer_reldiff_limit	= 0.0;

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
	pline->next			= NULL;

	last_proof			= NULL;

	if ( last_proof_line == NULL )
	    first_proof_line		= pline;
	else
	    last_proof_line->next	= pline;

	last_proof_line		= pline;
    }

    proof * p	= new proof;

    p->type			= type;
    p->output_token_end_column	= output.column;
    p->test_token_end_column	= test.column;
    p->absdiff			= absdiff;
    p->reldiff			= reldiff;
    p->next			= NULL;

    if ( last_proof == NULL )
	last_proof_line->proofs	= p;
    else
        last_proof->next	= p;

    last_proof		= p;

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
         && (    type != FLOAT
	      || absdiff > float_absdiff_limit
	      || reldiff > float_reldiff_limit )
         && (    type != INTEGER
	      || absdiff > integer_absdiff_limit
	      || reldiff > integer_reldiff_limit ) )
        output_proof ( type, absdiff, reldiff );
}

// Tests two numbers just scanned for the output and
// test files to see if there is a computable differ-
// ence.  If so, calls found_difference (FLOAT) (or
// found_difference (INTEGER)), and updates `float_
// absdiff_maximum' and `float_reldiff_maximum' (or
// integer_absdiff_maximum' and `integer_reldiff_
// maximum') by writing the differences just found
// into these variables iff the new differences are
// larger than the previous values of these variables.
//
// Also calls found_difference for DECIMAL or EXPONENT
// if the two number `decimals' or `has_exponent' file
// members are unequal.
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

    if (    output.decimals >= 0
         || test.decimals >= 0
	 || output.has_exponent
	 || test.has_exponent )
    {
	found_difference ( FLOAT, absdiff, reldiff );

	if ( absdiff > float_absdiff_maximum )
	    float_absdiff_maximum = absdiff;

	if ( reldiff > float_reldiff_maximum )
	    float_reldiff_maximum = reldiff;
    }
    else
    {
	found_difference ( INTEGER, absdiff, reldiff );

	if ( absdiff > integer_absdiff_maximum )
	    integer_absdiff_maximum = absdiff;

	if ( reldiff > integer_reldiff_maximum )
	    integer_reldiff_maximum = reldiff;
    }

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

        if (    strcmp ( "float", name ) == 0
	     || strcmp ( "integer", name ) == 0 )
	{
	    // special case.

	    double absdiff_limit =
		   isdigit ( argv[2][0] )
		|| argv[2][0] == '.' ?
	    	atof ( argv[2] ) : 0.0;
	    if (    isdigit ( argv[2][0] )
	         || argv[2][0] == '.' )
		    ++ argv, -- argc;

	    if ( argc < 3 ) break;

	    double reldiff_limit =
		   isdigit ( argv[2][0] )
		|| argv[2][0] == '.' ?
	    	atof ( argv[2] ) : 0.0;
	    if (    isdigit ( argv[2][0] )
	         || argv[2][0] == '.' )
		    ++ argv, -- argc;

	    if ( name[0] == 'f' )
	    {
	    	float_absdiff_limit = absdiff_limit;
	    	float_reldiff_limit = reldiff_limit;
	    }
	    else
	    {
	    	integer_absdiff_limit = absdiff_limit;
	    	integer_reldiff_limit = reldiff_limit;
	    }

	    if ( argc < 3 ) break;
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
	    	= isdigit ( argv[2][0] ) ?
		  (unsigned) atol ( argv[2] ) :
		  0;
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

    bool done		= false;

    while ( ! done )
    {

	// Scan next tokens.
	//
	scan_token ( output );
	scan_token ( test );

        // Terminate loop if just one file has an
	// EOF_TOKEN.

	if (    output.type == EOF_TOKEN
	     && test.type != EOF_TOKEN )
	{
	    found_difference ( EOF1 );
	    done = true;
	    break;
	}
	else
	if (    test.type == EOF_TOKEN
	     && output.type != EOF_TOKEN )
	{
	    found_difference ( EOF2 );
	    done = true;
	    break;
	}

        // Compare whitespace preceding tokens.

	if ( output.newlines != test.newlines )
	    found_difference ( LINEBREAK );
	else if ( (    output.whitespace[0] != 0
	            && test.whitespace[0]   == 0 )
		  ||
		  (    output.whitespace[0] == 0
		    && test.whitespace[0]   != 0 ) )
	    found_difference ( SPACEBREAK );
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

		if ( output.newlines == 0 )
		    found_difference ( WHITESPACE );
		else if ( newlines == 0 )
			found_difference ( ENDSPACE );
		else if ( newlines == output.newlines )
		    found_difference ( BEGINSPACE );
		else
		    found_difference ( LINESPACE );
	    }
	}
        
	// Compare tokens.
	//
        switch ( output.type ) {

	case EOF_TOKEN:
            assert ( test.type == EOF_TOKEN );
	    done = true;
	    break;

	case NUMBER_TOKEN:
	case WORD_TOKEN:

	    assert ( test.type != EOF_TOKEN );

	    if ( output.type != test.type )
	    {
		found_difference ( NONBLANK );
		done = true;
		break;
	    }

	    if ( output.type == WORD_TOKEN )
	    {
	        assert ( test.type == WORD_TOKEN );

		if ( output.length < test.length )
		    split_word ( test, output.length );
		else if ( test.length < output.length )
		    split_word ( output, test.length );
	    }

	    if ( output.column != test.column )
	        found_difference ( COLUMN );

	    char * tp1 = output.token;
	    char * tp2 = test.token;
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
	        assert ( * tp1 == 0 );

		if ( token_case )
		    found_difference
		        ( output.type != NUMBER_TOKEN ?
			  CASE :
			  output.decimals >= 0 ?
			  FLOAT :
			  test.decimals >= 0 ?
			  FLOAT :
			  output.has_exponent ?
			  FLOAT :
			  test.has_exponent ?
			  FLOAT :
			  INTEGER );
	    }

	    else if ( output.type == NUMBER_TOKEN )
	    {
	        assert ( test.type == NUMBER_TOKEN );
		diffnumber ();

		done = differences[NONBLANK].flag;
	    }

	    else
	    {
		found_difference ( NONBLANK );
		done = true;
	    }

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
	    if ( i == FLOAT )
		cout << " " << float_absdiff_maximum
		     << " " << float_reldiff_maximum;
	    else if ( i == INTEGER )
		cout << " " << integer_absdiff_maximum
		     << " " << integer_reldiff_maximum;
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
	    if (    p->type == FLOAT
	         || p->type == INTEGER )
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
