// Programming Contest File Difference Tester
//
// File:	scorediff.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Sun Aug  5 09:01:36 EDT 2001
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: hc3 $
//   $Date: 2001/08/05 12:53:33 $
//   $RCSfile: scorediff.cc,v $
//   $Revision: 1.32 $

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
unsigned const MAX_SIZE = 10200;

// Default maximum number of proof lines containing any
// one type of difference.
//
unsigned const MAX_PROOF_LINES = 10;

char documentation [] =
"scorediff [options] output_file test_file\n"
"\n"
"    Returns on a single line a list of the types of\n"
"    differences between the two files, followed by\n"
"    lines that contain `proofs' of these differ-\n"
"    ences.\n"
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
"    spacebreak	For two matching tokens, one is pre-\n"
"		ceded by whitespace (possibly con-\n"
"		taining a newline), and the other has\n"
"		no preceding whitespace at all.\n"
"\n"
"    linebreak	For two matching tokens, the preced-\n"
"		ing whitespaces of both are non-\n"
"		empty, and one preceding whitespace\n"
"		has a different number of new lines\n"
"		than the other.\n"
"\n"
"    whitespace	For two matching tokens, the preced-\n"
"		ing whitespaces both have no new\n"
"		lines, and both are non-empty, but\n"
"		these whitespaces do not match\n"
"		exactly.\n"
"\n"
"		However, this difference is NOT re-\n"
"		cognized if either following matching\n"
"		token is a number with a decimal\n"
"		point or exponent.\n"
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
"		However, this difference is NOT re-\n"
"		cognized if either following matching\n"
"		token is a number with a decimal\n"
"		point or exponent.\n"
"\n"
"    eof1	When the first file ends, the second\n"
"		file has a remaining non-eof token.\n"
"		In this case, preceding whitespaces\n"
"		are NOT compared.\n"
"\f\n"
"    eof2	When the second file ends, the first\n"
"		file has a remaining non-eof token.\n"
"		In this case, preceding whitespaces\n"
"		are NOT compared.\n"
"\n"
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
"		which contains a decimal point or\n"
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
"    sign	For two matching numbers, only one\n"
"		begins with a sign, or the two num-\n"
"		bers begin with different signs.\n"
"\f\n"
"    case	Two matching non-number tokens are\n"
"		NOT identical, but would be identical\n"
"		if letter case differences were\n"
"		ignored.\n"
"\n"
"    column	Two matching tokens end in differ-\n"
"		ent columns.\n"
"\n"
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
"\f\n"
"    A word is a string of non-whitespace characters\n"
"    that does not contain a number.  If two words S\n"
"    and L are being matched, and S is shorter than\n"
"    L, then L is split into a first word of the same\n"
"    length as S and a second word to be matched to\n"
"    the token that follows S, provided that the\n"
"    first part of L and the whole of S are equal\n"
"    except perhaps for case.  If L is so split, and\n"
"    the remainder of L does not equal the token\n"
"    following S except perhaps for case, this dif-\n"
"    ference is reported and then the token after S\n"
"    is matched to the token after L.  Thus failure\n"
"    to separate words by space, provided the words\n"
"    are otherwise correct except perhaps for case,\n"
"    will be reported as a formatting error, while\n"
"    failure to have words that are equal except\n"
"    perhaps for case will be reported as a non-\n"
"    blank difference with the effects of word split-\n"
"    ting undone so that subsequent tokens will be\n"
"    more likely to match correctly.\n"
"\n"
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
"\f\n"
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
"    the two exponents does not match, then the diff-\n"
"    erence will always be reported as a `float'\n"
"    difference and not a `case' difference.\n"
"\n"
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
"    of the difference.  Proofs are output on lines\n"
"    that have the following syntax:\n"
"\n"
"          line-proof ::=\n"
"                    output-line-number\n"
"                    test-line-number\n"
"                    token-proof token-proof*\n"
"\f\n"
"          token-proof ::=\n"
"                    output-token-begin-column\n"
"                    output-token-end-column\n"
"                    test-token-begin-column\n"
"                    test-token-end-column\n"
"                    proof proof*\n"
"\n"
"          proof ::= `nonblank' | `case' | `column' |\n"
"                    `decimal' | `exponent' |\n"
"                    `integer' absolute-difference\n"
"                              relative-difference |\n"
"                    `float'   absolute-difference\n"
"                              relative-difference\n"
"\n"
"          absolute-difference ::=\n"
"                    floating-point-number\n"
"\n"
"          relative-difference ::=\n"
"                    floating-point-number\n"
"\n"
"    where the column numbers in a line start with 0\n"
"    and the line numbers in a file start with 1.\n"
"    Here non-floating-point numbers output as part\n"
"    of proofs are unsigned integers.  All the proofs\n"
"    concerning the same pair of matching tokens are\n"
"    grouped together into a token-proof that begins\n"
"    with the beginning and ending column numbers of\n"
"    the matching tokens.  All the token-proofs whose\n"
"    tokens are in the same lines within their re-\n"
"    spective files are grouped together into one\n"
"    line-proof that begins with the line numbers of\n"
"    the respective lines.  Each line-proof is output\n"
"    on a line by itself.\n"
"\f\n"
"    There is a limit for each difference type to the\n"
"    number of proofs of that type that will be out-\n"
"    put.  Specifically, if the limit is N for diff-\n"
"    erence type T, then after N line-proofs each\n"
"    containing at least one proof of type T have\n"
"    output, no more proofs of type T will be output.\n"
"\n"
"    These limits default to 10 for each difference\n"
"    type, but the limits can be changed by program\n"
"    options.  An option consisting of a `-' followed\n"
"    by a difference name followed by an unsigned\n"
"    integer N sets the limit to N for the named\n"
"    difference.  Thus `-case 5' suppresses all\n"
"    `case' proofs after 5 line-proofs containing\n"
"    `case' proofs have been output.  If N is omit-\n"
"    ted, it is assumed to be 0 (and next program\n"
"    argument must NOT begin with a digit).  Thus\n"
"    `-case' with no following number suppresses all\n"
"    `case' proofs.\n"
"\n"
"    The `-float' and `-integer' program options\n"
"    differ in that they have the forms:\n"
"\n"
"        -float absolute-diff relative-diff N\n"
"        -integer absolute-diff relative-diff N\n"
"\n"
"    and the program outputs only `float' or\n"
"   `integer' proofs that have an absolute or rela-\n"
"    tive difference larger than the values given in\n"
"    the program option.  In these options the diff-\n"
"    erences may be omitted if they are zero and the\n"
"    program argument following them does NOT begin\n"
"    with a digit or decimal point.\n"
"\f\n"
"    If N is the limit on the number of line-proofs\n"
"    containing a `nonblank' proof, then after the\n"
"    last of these N line-proofs is finished, this\n"
"    program terminates without continuing its search\n"
"    for more differences.  Here if N == 0, then for\n"
"    the purposes of applying this rule, N is treated\n"
"    as if it were 1.\n"
;

// A token is either a number token, an end of file
// (eof) token, or a word token.
//
enum token_type {
    NUMBER_TOKEN, EOF_TOKEN, WORD_TOKEN };

struct file
    // Information about one of the input files (output
    // or test file).
{
    ifstream stream;	// Input stream.
    char * filename;	// File name.

    // Token description.
    //
    token_type type;	// Type of token.
    char token [ MAX_SIZE + 1 ]; // The current token,
    			// if not an end-of-file (eof).
			// Terminated by `\0'.
    int length;		// Length of token in charac-
    			// ters (not including `\0').
			// 0 for eof.
    int line;		// Line number of current
    			// token.  The first line is 1.
    int column;		// Column within the line of
    			// the last character of the
			// the current token.  The first
			// column is 0.
    bool remainder;	// True iff token is a remainder
    			// from a split token.

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
			// Terminated by `\0'.
    int newlines;	// Number of new line characters
    			// in this whitespace.

    // Token split description.
    //
    // If a word token is split, this information is
    // set so the next call to scan a token will return
    // the remainder of the split token.  To split a
    // word token a `\0' is inserted at the end of the
    // first result of the split, after recording the
    // character being replaced in `remainder_c' below.
    // Thus the reminder, or second result of the split,
    // follows the first part of the split in the
    // token[] member, except the remainder is missing
    // its first character, which is saved in
    // `remainder_c'.
    //
    int remainder_length;  // Length of remainder.
    			// 0 if no remainder.
    char remainder_c;	// First character of remainder.

    // Backup description.
    //
    // Set to characters backed over in the input string
    // when scan_token determines that some characters
    // encountered scanning a word begin a number or
    // that some characters encountered scanning a
    // number do not begin an exponent.  Possible
    // values are:
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
			// points.  Terminated by `\0'.
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

    f.column		= -1;
    f.line		= 0;
    f.type		= WORD_TOKEN;

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
	f.remainder		= true;

	return;
    }

    f.remainder = false;

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

	// f.token now holds a sign.

	switch ( c ) {
	case '.':
	    ++ decimals;

	    * tp ++ = c;

	    c = get_character ( f );
	    ++ column;

	    // f.token now holds a sign followed by a
	    // decimal point.

	    if ( ! isdigit ( c ) ) goto word;
	    break;

	default:
	    // Here f.token holds just a sign.

	    if ( ! isdigit ( c ) ) goto word;
	}
	break;

    case '.':
	* tp ++ = c;
	c = get_character ( f );
	++ column;
	++ decimals;

	// f.token now holds just a decimal point.

	if ( ! isdigit ( c ) ) goto word;
	break;

    default:
	// Here f.token is empty.

        if ( ! isdigit ( c ) ) goto word;
	break;
    }

    // Come here when c is the first digit of a number.

    * tp ++ = c;
    c = get_character ( f );
    ++ column;

    // Get rest of mantissa.
    //
    while ( true ) {
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

	// Save tp and column in case we want to back
	// up to this point.

        char * ep = tp;
	int ecolumn = column;

	if ( tp < endtp ) * tp ++ = c;
	else token_too_long ( f );

	c = get_character ( f );
	++ column;

	// f.token now holds a number followed by an
	// `e' or `E'.

	if ( c == '+' || c == '-' ) {
	    if ( tp < endtp ) * tp ++ = c;
	    else token_too_long ( f );

	    c = get_character ( f );
	    ++ column;
	}

	// f.token now holds a number followed by an
	// `e' or `E' and possibly then followed by
	// a sign.

	if ( ! isdigit ( c ) ) {
	    // No digit next: backup.

	    assert ( * f.back == 0 );
	    * tp = 0;
	    strcpy ( f.backup, ep );
	    f.back = f.backup;
	    tp = ep;
	    column = ecolumn;
	} else {
	    // Exponent first digit next: scan rest of
	    // exponent.

	    do {
		if ( tp < endtp ) * tp ++ = c;
		else token_too_long ( f );
		c = get_character ( f );
		++ column;
	    } while ( isdigit ( c ) );
	    f.has_exponent = true;
	}
    }

    // End of number token.  c is first character beyond
    // number token.

    * tp = 0;

    f.type	= NUMBER_TOKEN;
    f.length	= tp - f.token;
    f.column	= -- column;
    f.decimals	= decimals;

    // Put c into backup.

    if ( c != EOF ) {
	char * p = f.back;
	if ( * p == 0 ) p = f.back = f.backup;
	else p = f.back + strlen ( f.back );
	* p ++ = c;
	* p = 0;
    }

    // Convert number token to floating point using
    // strtod.

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
// whitespace character or is the beginning of a number
// token.  In the cases where c is not the next char-
// acter of the word, f.token is not empty at this
// point.
//
word:

    while ( true ) {
        if (    isspace ( c )
	     || isdigit ( c )
	     || c == EOF ) break;

	switch ( c ) {

	case '+':
	case '-':
	case '.':

	    // Possible first character of number.

	    // Save tp and column in case we want to
	    // backup to this point.

	    char * np = tp;
	    int ncolumn = column;
	    int oldc = c;

	    if ( tp < endtp ) * tp ++ = c;
	    else token_too_long ( f );

	    c = get_character ( f );
	    ++ column;

	    // f.token now holds a word followed by a
	    // sign or decimal point.

	    if ( c == '.' && oldc != '.' ) {
		if ( tp < endtp ) * tp ++ = c;
		else token_too_long ( f );

		c = get_character ( f );
		++ column;
	    }

	    // f.token now holds a word followed by a
	    // sign and then possibly a decimal point,
	    // or followed by just a decimal point.

	    if ( isdigit ( c ) ) {
	        // Found digit and hence number: backup.

		assert ( * f.back == 0 );
		* tp = 0;
		strcpy ( f.backup, np );
		f.back = f.backup;
		tp = np;
		column = ncolumn;
		goto end_word;
	    } else continue;
	        // No digit; we are still in word.  Go
		// check c for possible number beginning
		// character (it might be `.' or sign).
	}

	if ( tp < endtp ) * tp ++ = c;
	else token_too_long ( f );

	c = get_character ( f );
	++ column;
    }

end_word:

    // End of word.  c is first character beyond word.

    f.type	= WORD_TOKEN;
    f.length	= tp - f.token;
    f.column	= -- column;

    assert ( f.length  > 0 );

    * tp = 0;

    // Put c into backup.

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

    f.remainder_length = f.length - n;
    char * p = f.token + n;
    f.remainder_c = * p;
    * p = 0;
    f.length = n;
    f.column -= f.remainder_length;
}

// Undo a token split.  Does nothing if file
// token is not split.
//
void undo_split ( file & f )
{
    if ( f.remainder_length != 0 )
    {
    	f.token[f.length] = f.remainder_c;
	f.length += f.remainder_length;
	f.column += f.remainder_length;
	f.remainder_length = 0;
    }
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
    SIGN,
    CASE,
    COLUMN,
    NONBLANK,
    MAX_DIFFERENCE
};

// Difference data.
//
struct difference
    // Information about one type of difference.
{
    char *	name;
    	// Name of difference type.

    bool	found;
        // True if difference of this type has been
	// found.

    unsigned	last_output_line;
    unsigned	last_test_line;
       // Line numbers of last proof output that
       // contains this type of difference.  Zero
       // if no proof containing this difference
       // has been output.

    unsigned	proof_limit;
       // If zero, suppresses output of proofs of this
       // difference type.  Decremented conceptually at
       // the end of a line in either file if a proof
       // for a difference of this type has been output
       // for that line.  In actual practice, the
       // decrementing is not done till the next
       // difference of this type is found.
};

// Information on the various differences found.
//
difference differences[] = {
    { "linebreak",	false, 0, 0, MAX_PROOF_LINES },
    { "spacebreak",	false, 0, 0, MAX_PROOF_LINES },
    { "whitespace",	false, 0, 0, MAX_PROOF_LINES },
    { "beginspace",	false, 0, 0, MAX_PROOF_LINES },
    { "linespace",	false, 0, 0, MAX_PROOF_LINES },
    { "endspace",	false, 0, 0, MAX_PROOF_LINES },
    { "eof1",		false, 0, 0, MAX_PROOF_LINES },
    { "eof2",		false, 0, 0, MAX_PROOF_LINES },
    { "float",		false, 0, 0, MAX_PROOF_LINES },
    { "integer",	false, 0, 0, MAX_PROOF_LINES },
    { "decimal",	false, 0, 0, MAX_PROOF_LINES },
    { "exponent",	false, 0, 0, MAX_PROOF_LINES },
    { "sign",		false, 0, 0, MAX_PROOF_LINES },
    { "case",		false, 0, 0, MAX_PROOF_LINES },
    { "column",		false, 0, 0, MAX_PROOF_LINES },
    { "nonblank",	false, 0, 0, MAX_PROOF_LINES }
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
    // A description of one single proof to be output.
{
    difference_type	type;
        // Difference type.

    unsigned		output_token_end_column;
    unsigned		test_token_end_column;
        // Column numbers.

    double		absdiff;
    double		reldiff;
    	// Numeric differences for numeric difference
	// types.

    proof *		next;
        // Next proof in list of proofs on one proof
	// line.
};

struct proof_line
    // A description of one single line of proofs that
    // is to be output.
{
    unsigned		output_line;
    unsigned		test_line;
        // Line numbers.

    proof *		proofs;
        // First proof on this line.

    proof_line *	next;
        // Next proof line to be output.
};

proof_line *	first_proof_line	= NULL;
proof_line *	last_proof_line		= NULL;
    // First and last proof lines being output.

proof *		last_proof		= NULL;
    // Last proof being output on last proof
    // line being output.

// Output a new proof.  Use current line and
// column numbers.
//
inline void output_proof
	( difference_type type,
	  double absdiff = 0.0,
	  double reldiff = 0.0 )
{

    if ( last_proof_line == NULL
	 ||
         last_proof_line->output_line
	 != output.line
	 ||
	 last_proof_line->test_line
	 != test.line )
    {
        proof_line * pline	= new proof_line;

	pline->output_line	= output.line;
	pline->test_line	= test.line;
	pline->proofs		= NULL;
	pline->next		= NULL;

	last_proof		= NULL;

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

    difference & d = differences[type];

    d.last_output_line = output.line;
    d.last_test_line   = test.line;
}

// Record a found difference.
//
inline void found_difference
	( difference_type type,
	  double absdiff = 0.0,
	  double reldiff = 0.0 )
{
    difference & d = differences[type];

    d.found = true;

    if ( d.proof_limit > 0
         && (    type != FLOAT
	      || absdiff > float_absdiff_limit
	      || reldiff > float_reldiff_limit )
         && (    type != INTEGER
	      || absdiff > integer_absdiff_limit
	      || reldiff > integer_reldiff_limit ) )
    {
        // Conceptually, d.proof_limit is decremented
	// at the end of a proof line containing an
	// output of a proof of the given `type'.  But
	// in practice, to reduce coding complexity,
	// the decrementing is deferred until the next
	// proof of this type is discovered, and then
	// the decrementing is done here.
	//
	if ( d.last_output_line != 0
	     && ( d.last_output_line != output.line
	         ||
	         d.last_test_line != test.line ) )
	    -- d.proof_limit;

	if ( d.proof_limit > 0 )
	    output_proof ( type, absdiff, reldiff );
    }
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
// Also calls found_difference for DECIMAL, EXPONENT,
// or SIGN if the two number `decimals' or `has_expo-
// nent' file members are unequal, or the number signs
// are unequal (where a sign is the first character of a
// token if that is `+' or `-' and is `\0' otherwise).
//
// If there is no computable difference, calls found_
// difference(NONBLANK) instead.  This happens if one of
// the numbers is not `finite' or their difference is
// not `finite'.
//
// Returns true if found_difference(NONBLANK) was called
// and false otherwise.
// 
bool diffnumber ()
{
    if ( ! finite ( output.number )
	 ||
	 ! finite ( test.number ) )
    {
	found_difference ( NONBLANK );
	return true;
    }

    double absdiff =
	( output.number - test.number );
    if ( ! finite ( absdiff ) )
    {
	found_difference ( NONBLANK );
	return true;
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
	return true;
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

    char oc = output.token[0];
    char tc = test.token[0];

    if ( oc != '-' && oc != '+' ) oc = 0;
    if ( tc != '-' && tc != '+' ) tc = 0;

    if ( oc != tc )
    	found_difference ( SIGN );

    return false;
}

// Main program.
//
int main ( int argc, char ** argv )
{

    // Process options.

    while ( argc >= 4 && argv[1][0] == '-' )
    {

	char * name = argv[1] + 1;

        if (    strcmp ( "float", name ) == 0
	     || strcmp ( "integer", name ) == 0 )
	{
	    // special case.

	    double absdiff_limit = 0.0;
	    double reldiff_limit = 0.0;

	    if (    isdigit ( argv[2][0] )
	         || argv[2][0] == '.' )
	    {
		absdiff_limit = atof ( argv[2] );
		++ argv, -- argc;
		if ( argc < 3 ) break;
	    }

	    if (    isdigit ( argv[2][0] )
	         || argv[2][0] == '.' )
	    {
		reldiff_limit = atof ( argv[2] );
		++ argv, -- argc;
		if ( argc < 3 ) break;
	    }

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
	}
        else if ( strcmp ( "doc", name ) == 0 )
	    break;

	int i; for ( i = 0; i < MAX_DIFFERENCE; ++ i )
	{
	    if ( strcmp ( differences[i].name, name )
	         == 0 ) break;
	}

	assert ( argc >= 3 );

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
    // unless there are exactly two program arguments
    // left.

    if ( argc != 3 )
    {
        cout << documentation;
	exit (1);
    }

    // Open files.

    open ( output, argv[1] );
    open ( test, argv[2] );

    // Loop that reads the two files and compares their
    // tokens, recording any differences found.

    bool done		= false;
    difference & nb	= differences[NONBLANK];

    bool last_match_was_nonblank_diff	= false;

    while ( ! done )
    {

	// Scan next tokens.
	//
	if ( last_match_was_nonblank_diff
	     && ( output.remainder || test.remainder ) )
	{
	    // If the last two tokens had a nonblank
	    // diff and one was a remainder, undo any
	    // splits and discard only the remainder,
	    // leaving the other token for the next
	    // match.

	    undo_split ( output );
	    undo_split ( test );

	    assert (    ! output.remainder
	    	     || ! test.remainder );

	    if ( output.remainder )
		scan_token ( output );
	    else
		scan_token ( test );
	}
	else
	{
	    scan_token ( output );
	    scan_token ( test );
	}

	// Terminate loop if we have output the last
	// nonblank containing proof line.
	//
	// Conceptually we decrement nb.proof_limit at
	// the end of a line in either file if a `non-
	// blank' proof has been output for that line,
	// but in practice the decrement is deferred
	// until the next `nonblank' difference is
	// discovered, which has not happened yet.  So
	// we must compensate.
	//
	// Note that nb.proof_limit == 0 is treated as
	// np.proof_limit == 1 for this test.

	if ( nb.last_output_line != 0
	     &&
	     nb.proof_limit <= 1
	     &&
	     ( nb.last_output_line != output.line
	       ||
	       nb.last_test_line != test.line ) )
	    break;

        // Terminate loop if just one file has an
	// EOF_TOKEN.

	if (    output.type == EOF_TOKEN
	     && test.type   != EOF_TOKEN )
	{
	    found_difference ( EOF1 );
	    done = true;
	    break;
	}
	else
	if (    output.type != EOF_TOKEN
	     && test.type   == EOF_TOKEN )
	{
	    found_difference ( EOF2 );
	    done = true;
	    break;
	}
        
	// Compare tokens.
	//
	last_match_was_nonblank_diff = false;
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
		last_match_was_nonblank_diff = true;
		break;
	    }

	    // If both tokens are words and one is
	    // longer than the other, split the longer
	    // word.  If we get a non-blank diff, we
	    // will undo the split.

	    if ( output.type == WORD_TOKEN )
	    {
		if ( output.length < test.length )
		    split_word ( test, output.length );
		else if ( test.length < output.length )
		    split_word ( output, test.length );
	    }

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
			  CASE : FLOAT );
	    }

	    else if ( output.type == NUMBER_TOKEN )
	    {
	        assert ( test.type == NUMBER_TOKEN );
		last_match_was_nonblank_diff
		    = diffnumber ();
	    }

	    else
	    {
	    	undo_split ( test );
	    	undo_split ( output );

		found_difference ( NONBLANK );
		last_match_was_nonblank_diff = true;
	    }

	    break;
     	}

	// Compare column numbers.  This is done after
	// token comparison so that the results of word
	// splitting can be taken into account in token
	// ending column numbers.

	if (    output.type != EOF_TOKEN
	     && output.column != test.column )
	    found_difference ( COLUMN );

        // Compare whitespace preceding tokens.  This is
	// done after token comparison so that the
	// results of word splitting can be taken into
	// account in token ending column numbers.

	if ( (    output.whitespace[0] != 0
	       && test.whitespace[0]   == 0 )
	     ||
	     (    output.whitespace[0] == 0
	       && test.whitespace[0]   != 0 ) )
	    found_difference ( SPACEBREAK );
	else if ( output.newlines != test.newlines )
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

		// Come here if a difference in white-
		// space has been detected.  `newlines'
		// is the number of newlines scanned so
		// far.

		// Skip to just before next newline or
		// string end in each whitespace.

		while ( * wp1 && * wp1 != '\n' ) ++ wp1;
		while ( * wp2 && * wp2 != '\n' ) ++ wp2;

		assert ( output.newlines
		         == test.newlines );

		if ( newlines == output.newlines )
		{
		    bool output_is_float =
		         output.type == NUMBER_TOKEN
		         && ( output.decimals >= 0 
			      || output.has_exponent );
		    bool test_is_float =
		         test.type == NUMBER_TOKEN
		         && ( test.decimals >= 0 
			      || test.has_exponent );
		    if (    ! output_is_float
		         && ! test_is_float )
			found_difference
			    ( newlines == 0 ?
			      WHITESPACE :
			      BEGINSPACE );
		}
		else if ( newlines == 0 )
			found_difference ( ENDSPACE );
		else
		    found_difference ( LINESPACE );
	    }
	}
    }

    // The file reading loop is done, and differences
    // are now recorded in memory, ready for outputting.

    // Produce first output line listing all found
    // differences, regardless of the proofs to be
    // output.

    bool any = false;

    for ( int i = 0; i < MAX_DIFFERENCE; ++ i )
    {
        if ( differences[i].found )
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

    // Output proof lines.

    for ( proof_line * pline = first_proof_line;
          pline != NULL;
	  pline = pline->next )
    {
        cout << pline->output_line << " "
             << pline->test_line;

	// Output proofs within a proof line.

	int last_output_column	= -1;
	int last_test_column	= -1;

	for ( proof * p = pline->proofs;
	      p != NULL;
	      p = p->next )
	{
	    if ( last_output_column
	             != p->output_token_end_column
	         ||
		 last_test_column
	             != p->test_token_end_column )
	    {
		last_output_column =
		    p->output_token_end_column;
		last_test_column =
		    p->test_token_end_column;
		cout << " " << last_output_column;
		cout << " " << last_test_column;
	    }

	    cout << " " << differences[p->type].name;
	    if (    p->type == FLOAT
	         || p->type == INTEGER )
	    {
		cout << " " << p->absdiff;
		cout << " " << p->reldiff;
	    }
	}

	cout << endl;
    }

    // Return from main function without error.

    return 0;
}
