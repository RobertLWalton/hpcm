// Programming Contest File Difference Tester
//
// File:	scorediff.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Thu Sep 22 07:02:58 EDT 2005
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: hc3 $
//   $Date: 2005/09/22 13:04:11 $
//   $RCSfile: scorediff.cc,v $
//   $Revision: 1.56 $

// This is version 2, a major revision of the first
// scorediff program.  This version is more explicitly
// token oriented.

#include <cstdlib>
#include <climits>
#include <iostream>
#include <fstream>
#include <cctype>
#include <cstring>
#include <cmath>
#ifdef sun
#   include <ieeefp.h>
#endif
int finite (double);	// Not always in math.h
#include <cassert>
using namespace std;

// Name defined in include file that is used below and
// needs to be changed in its usage below.
//
#ifdef INFINITY
#    undef INFINITY
#    endif
#define INFINITY Infinity

// Maximum size of a token, or of whitespace preceding
// a token.
//
unsigned const MAX_SIZE = 10100;

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
"    any part of a number.  Thus words cannot contain\n"
"    digits, nor can they contain signs, decimal\n"
"    points, or exponents that are parts of numbers.\n"
"    A number with a decimal point or exponent is a\n"
"    floating point number, while other numbers are\n"
"    integers.\n"
"\n"
"    The option -filtered specifies that both the\n"
"    output_file and the test_file are filtered\n"
"    output from the jfilter program, and have marker\n"
"    characters at the beginning of each line to\n"
"    delimit cases and groups.\n"
"\n"
"    The option -nonumber specifies that all number\n"
"    characters should be treated as word characters,\n"
"    so there are no number tokens.  The option\n"
"    -nosign specifies that sign characters preceding\n"
"    numbers should be treated as word characters, so\n"
"    numbers can only include signs in their exponent\n"
"    parts.\n"
"\n"
"    The types of differences are:\n"
"\f\n"
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
"		token is a floating point number.\n"
"\n"
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
"\f\n"
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
"		token is a floating point number.\n"
"\n"
"    word-eof1		When one file ends, the other\n"
"    integer-eof1	file has a remaining word,\n"
"    float-eof1		integer number, or floating\n"
"    word-eof2		point number.  After the\n"
"    integer-eof2	shorter file ends, ALL tokens\n"
"    float-eof2		in the longer file are read\n"
"		to compute these values, so, for ex-\n"
"		ample, word-eof1 and float-eof1 can\n"
"		both be reported.  Whitespace after\n"
"		the last matching non-eof tokens in\n"
"		the files is NOT compared.  Eof1\n"
"		means the first file is shorter,\n"
"		while eof2 means the second file is\n"
"		shorter. Thus up to three of these\n"
"		differences can occur in the same\n"
"		file comparison.\n"
"\n"
"    float A R	For two matching number tokens, at\n"
"		least ONE of which is floating point,\n"
"		the token character strings do not\n"
"		match exactly, the absolute differ-\n"
"		ence of the numbers is A, and the re-\n"
"		lative difference of the numbers is\n"
"		R.\n"
"\f\n"
"		In the line returned reporting diff-\n"
"		erences in the two files, A and R\n"
"		are the maximum observed A and R\n"
"		values for all observed `float' diff-\n"
"		erences.\n"
"\n"
"    integer A R    Same as `float A R', but for two\n"
"		matching integer number tokens.\n"
"\n"
"    decimal	Two matching numbers have different\n"
"		numbers of decimal places, or one has\n"
"		a decimal point and the other does\n"
"		not.\n"
"\n"
"    exponent	For two matching numbers one has an\n"
"		exponent but the other does not.\n"
"\n"
"    sign	For two matching INTEGER number\n"
"		tokens, only one begins with a sign,\n"
"		or the two integers begin with dif-\n"
"		ferent signs.\n"
"\n"
"    case	Two matching word tokens are NOT\n"
"		identical, but would be identical if\n"
"		letter case differences were ignored.\n"
"\n"
"    column	Two matching tokens end in differ-\n"
"		ent columns.\n"
"\n"
"    word	At least one of two matching tokens\n"
"		is a word token, and it is not true\n"
"		that both are word tokens of the same\n"
"		length (see word splitting below)\n"
"		that are identical except for letter\n"
"		case.\n"
"\f\n"
"    infinity	Two matching number tokens cannot be\n"
"		compared because they are NOT identi-\n"
"		cal except for letter case and one or\n"
"		both or their difference is too large\n"
"		to be represented as a finite double\n"
"		precision floating point number.\n"
"\n"
"    none	There are no differences in the files\n"
"		at all.  This is returned as the sole\n"
"		contents of the output line that\n"
"		lists differences.\n"
"\n"
"    The files are parsed into whitespace, numbers,\n"
"    words, and end-of-files (eofs).  A number is an\n"
"    optional sign followed by digits containing an\n"
"    optional decimal point followed by an optional\n"
"    exponent.  An exponent is an `e' or `E' followed\n"
"    by an an optional sign followed by digits.  A\n"
"    number is scanned by the strtod(3) function.\n"
"    A number with a decimal point or exponent is a\n"
"    floating point number, while other numbers are\n"
"    integers.\n"
"\n"
"    A word is a string of non-whitespace characters\n"
"    that does not contain any part of a number.  If\n"
"    two words S and L are being matched, and S is\n"
"    shorter than L, then L is split into a first\n"
"    word of the same length as S and a second word\n"
"    to be matched to the token that follows S, pro-\n"
"    vided that the first part of L and the whole of\n"
"    S are equal except perhaps for case.  But if S\n"
"    and the first part of L are NOT equal except for\n"
"    case, then L is NOT split.\n"
"\f\n"
"    Note that failure to separate correctly spelled\n"
"    words by space will be reported as a spacebreak,\n"
"    due to word splitting, and in other cases where\n"
"    word splitting applies, a word difference will\n"
"    be reported, as one would expect.  However, when\n"
"    word splitting applies and a word difference is\n"
"    reported, the tokens reported to be different in\n"
"    the word difference token-proof (see below) may\n"
"    not be exactly those one would expect.\n"
"\n"
"    Normally when two tokens do not match, both are\n"
"    skipped over to get to the next tokens which are\n"
"    then matched with each other.  But there are\n"
"    exceptions to this rule.\n"
"\n"
"    The first exceptions occur when the output_file\n"
"    and test_file are filtered, and the -filtered\n"
"    option is used.  In this case, if one token is\n"
"    followed by a group beginning and the other is\n"
"    not, the other token only is skipped.  If this\n"
"    rule does not apply, and one token is followed\n"
"    by a case beginning but the other token is not,\n"
"    the other token only is skipped.\n"
"\n"
"    If none of the above exceptions apply, one of\n"
"    tokens is a word, and there is a `word' differ-\n"
"    ence between the tokens (they are not identical\n"
"    with case ignored), then the following rules\n"
"    apply.  If one token is a remainder of a split\n"
"    and the other is not (it cannot be, actually),\n"
"    then only the remainder is skipped.  If one of\n"
"    the tokens is followed by white space containing\n"
"    a new line or ending with an end of file, and\n"
"    the other token is not so followed, only this\n"
"    other token is skipped.\n"
"\n"
"    If none of the above rules apply, but one of the\n"
"    tokens is a number and one is a word, only the\n"
"    word is skipped over.\n"
"\f\n"
"    It is an error if a token longer than 10,100\n"
"    characters is found, or if a sequence of con-\n"
"    secutive whitespace characters longer than\n"
"    10,100 characters is found.\n"
"\n"
"    The relative difference between two numbers x\n"
"    and y is:\n"
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
"    floating point numbers) cause an `infinity'\n"
"    difference.\n"
"\n"
"    For the purpose of computing the column of a\n"
"    character, tabs are set every 8 columns.\n"
"\n"
"    Note that if the two matching numbers have expo-\n"
"    nents and the letter case of the `e' or `E' in\n"
"    the two exponents does not match, then the diff-\n"
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
"    space preceding the tokens, or of any whitespace\n"
"    that is after the non-eof token in its file.\n"
"\n"
"    For each kind of difference found in the two\n"
"    files, the scorediff program outputs a `proof'\n"
"    of the difference.  Proofs are output on lines\n"
"    that have the following syntax:\n"
"\n"
"          line-proof ::=\n"
"                    group-number\n"
"                    case-number\n"
"                    output-line-number\n"
"                    test-line-number\n"
"                    token-proof token-proof*\n"
"\n"
"          token-proof ::=\n"
"                    output-token-begin-column\n"
"                    output-token-end-column\n"
"                    test-token-begin-column\n"
"                    test-token-end-column\n"
"                    proof proof*\n"
"\f\n"
"          proof ::= `word' | `case' | `column' |\n"
"                    `decimal' | `exponent' |\n"
"                    `sign' | `infinity' |\n"
"                    `integer' absolute-difference\n"
"                              relative-difference |\n"
"                    `float'   absolute-difference\n"
"                              relative-difference |\n"
"                    `word-eof1' | `integer-eof1' |\n"
"                    `float-eof1' | `word-eof2' |\n"
"                    `integer-eof2' | `float-eof2' |\n"
"                    `linebreak' | `spacebreak' |\n"
"                    `whitespace' | `beginspace' |\n"
"                    `linespace' | `endspace'\n"
"\n"
"          absolute-difference ::=\n"
"                    floating-point-number\n"
"\n"
"          relative-difference ::=\n"
"                    floating-point-number\n"
"\n"
"    Here the column numbers in a line start with 0\n"
"    and the group, case, and line numbers in a file\n"
"    start with 1.  An proof before the first group\n"
"    beginning has the group number 0, and a proof\n"
"    before the first case beginning gets has case\n"
"    number 0.  If the -filtered option is not given,\n"
"    the group and case numbers are all 0.\n"
"\n"
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
"    put for each group.  Specifically, if the limit\n"
"    is N for difference type T, then after N line-\n"
"    proofs each containing at least one proof of\n"
"    type T have output with a particular group\n"
"    number, no more proofs of type T will be output\n"
"    with that group number.  Group number 0 is\n"
"    treated here as a separate group, which is\n"
"    present if the -filtered option is absent.\n"
"\n"
"    These limits default to 10 for each difference\n"
"    type, but the limits can be changed by program\n"
"    options.  An option consisting of a `-' followed\n"
"    by a difference name followed by an unsigned\n"
"    integer N sets the limit to N for the named\n"
"    difference.  Thus `-column 5' suppresses all\n"
"    `column' proofs after 5 line-proofs containing\n"
"    `column' proofs have been output.  If N is omit-\n"
"    ted, it is assumed to be 0 (and next program\n"
"    argument must NOT begin with a digit).  Thus\n"
"    `-column' with no following number suppresses\n"
"    all `column' proofs.\n"
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
"    If no `-float' option is given, then any differ-\n"
"    ence in the representation of equal numbers, one\n"
"    of which is floating point, will output a\n"
"    `float 0 0' proof.  Similarly if no `-integer'\n"
"    option is given, then any difference in repre-\n"
"    sentation of equal integers will output an\n"
"    `integer 0 0' proof.\n"
"\n"
"    The special option `-all N' sets the limits for\n"
"    all the types of differences, where N is taken\n"
"    to be 0 if it is omitted (the next argument must\n"
"    NOT begin with a digit).\n"
"\n"
"    If more than one limit setting option affects\n"
"    the limit of a difference type, the last such\n"
"    option is the effective option for that type.\n"
;

// A token is either a number token, a word token, a
// beginning of test group (bog) token, a beginning of
// test case (boc) token, or an end of file (eof) token.
//
enum token_type {
    NUMBER_TOKEN, WORD_TOKEN,
    BOG_TOKEN, BOC_TOKEN, EOF_TOKEN };

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
    int test_group;	// Group number of current
    			// token.  The first group is 1.
			// The group number is 0 before
			// the first group begins, or
			// if there is no -filtered
			// option.
    int test_case;	// Ditto but case number.  Note
    			// cases are numbered from the
			// beginning of the file, and
			// NOT from the beginning of
			// a group.
    int line;		// Line number of current
    			// token.  The first line is 1.
    int column;		// Column within the line of
    			// the last character of the
			// the current token.  The first
			// column is 0.
    bool boc_next;	// The next token is a BOC_
    			// TOKEN.  Set only when a fil-
			// tered line begins with `+',
			// signifying a BOG_TOKEN/BOC_
			// TOKEN pair.
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
    bool is_float;	// For a number token, true iff
    			// decimals >= 0 or has_exponent
			// is true.

    // One of the following two switches is set by scan-
    // ning whitespace into `back' by the before_nl
    // function.
    //
    bool before_nl;	// True if whitespace following
    			// token has a new line or end
			// of file.
    bool not_before_nl;	// True if whitespace following
    			// token does not contain a new
			// line or end of file.

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
    // number do not begin an exponent.
    //
    // An important feature of the backup is that if it
    // is not empty at the beginning of a token scan, it
    // will be empty at the end of the token scan, and
    // therefore it is easier to put characters after
    // the token into the backup, because that is empty.
    // As a consequence strcpy can be used to set the
    // backup.
    //
    // Possible values are:
    //
    //		+D   -D   .D   +.D  -.D
    //		eX   e+Y  e-Y  EX   E+Y   E-Y
    //
    // where D denotes any digit, X denotes any charac-
    // ter that is neither a digit nor a sign, and Y
    // denotes any character that is not a digit.
    //
    // Can also be set to a single character following a
    // token.
    //
    // Can also be set to the whitespace following a
    // token (by before_nl), possibly followed by a
    // single non-whitespace character.

    char * back;	// If pointing at `\0', there
    			// are no backed up characters
			// to deliver.  Otherwise
			// use `* back ++' to input
			// next character.
    char backup [ 5 + MAX_SIZE + 1 ]; 
    			// Buffer of backed up char-
    			// acters into which `back'
			// points.  Terminated by `\0'.
};

// The two files.
//
file output;
file test;

// Scanning options.
//
bool nonumber = false;
bool nosign = false;
bool filtered = false;

// Open file for reading.
//
void open ( file & f, char * filename )
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
    f.test_group	= 0;
    f.test_case		= 0;
    f.line		= 0;
    f.type		= WORD_TOKEN;
    f.boc_next		= false;

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

// Routines to announce errors and exit program.
//
void whitespace_too_long ( file & f ) {
    cerr << "Whitespace too long in line "
         << f.line
	 << " of "
         << f.filename
	 << endl;
    exit (1);
}
void token_too_long ( file & f ) {
    cerr << "Token too long in line "
         << f.line
	 << " of "
         << f.filename
	 << endl;
    exit (1);
}
void bad_filtered_mark ( file & f ) {
    cerr << "Bad filtered file line mark"
            " beginning line "
         << f.line
	 << " of "
         << f.filename
	 << endl;
    exit (1);
}


// Scan next token in a file.  EOF_TOKEN is
// returned repeatedly at end of file.
//
void scan_token ( file & f )
{
    if ( f.type == EOF_TOKEN ) return;
    if ( f.boc_next )
    {
	// Current token is part of BOG/BOC pair.
	//
	assert ( f.type == BOG_TOKEN );
        f.boc_next	= false;
	f.type		= BOC_TOKEN;
	return;
    }

    f.before_nl		= false;
    f.not_before_nl	= false;

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

    // Set in case BOG/BOC/EOF_TOKEN found.
    //
    f.length	= 0;
    f.token[0]	= 0;

    while ( isspace ( c ) ) {
        if ( c == '\n' ) {
	    column = -1;
	    ++ f.newlines;
	    ++ f.line;
	    if ( filtered ) {
	        switch ( get_character ( f ) ) {
		case '+':	++ f.test_group;
				f.type = BOG_TOKEN;
				f.column = column;
				f.boc_next = true;
				return;
		case '-':	++ f.test_case;
				f.type = BOC_TOKEN;
				f.column = column;
				return;
		case '|':	++ f.test_group;
				f.type = BOG_TOKEN;
				f.column = column;
				return;
		case '.':	break;
		case EOF:	break;
		default:	bad_filtered_mark ( f );
		}
	    }
	}
	else if ( c == '\t' )
	    column += 7 - ( column % 8 );
	else if ( c == '\f' ) -- column;
	else if ( c == '\v' ) -- column;
	else if ( c == '\r' ) column = -1;
	else if ( c == '\b' && column >= 1 )
	    column -= 2;

	// Note: terminals, unlike printers, generally
	// do not treat \f as going back to the first
	// column, so we do not here.


	if ( wp < endwp ) * wp ++ = c;
	else whitespace_too_long ( f );

	c = get_character ( f );
	++ column;
    }

    * wp = 0;

    // Come here then c is the first character of the
    // token.

    if ( c == EOF ) {
    	f.type = EOF_TOKEN;
	f.column = column;
	return;
    }

    // Come here when c is the first character of a
    // word or number token.

    char * tp = f.token;
    char * endtp = tp + MAX_SIZE;
    int decimals = -1;

    if ( nonumber ) goto word;

    switch ( c ) {

    case '+':
    case '-':
	if ( nosign ) goto word;

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

	// f.token now holds a mantissa followed by an
	// `e' or `E'.

	if ( c == '+' || c == '-' ) {
	    if ( tp < endtp ) * tp ++ = c;
	    else token_too_long ( f );

	    c = get_character ( f );
	    ++ column;
	}

	// f.token now holds a mantissa followed by an
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
    // number token, and if the backup string is not
    // empty, it was set just above and c should be
    // APPENDED to it.

    * tp = 0;

    f.type	= NUMBER_TOKEN;
    f.length	= tp - f.token;
    f.column	= -- column;
    f.decimals	= decimals;
    f.is_float	= ( f.decimals >= 0 ) || f.has_exponent;

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
// token or is an EOF.  In the cases where c is not the
// next character of the word, f.token is not empty at
// this point.
//
word:

    while ( true ) {
        if (    isspace ( c )
	     || ( isdigit ( c ) && ! nonumber )
	     || c == EOF ) break;

	switch ( c ) {

	case '+':
	case '-':
	    if ( nosign ) break;
	case '.':
	    if ( nonumber ) break;

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

    // End of word.  c is first character beyond the
    // word, and if the backup string is not empty,
    // it was set just above and c should be APPENDED
    // to it.

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

    f.before_nl		= false;
    f.not_before_nl	= true;
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
	f.before_nl	= false;
	f.not_before_nl	= false;
    }
}

// Sets f.before_nl and f.not_before_nl according to
// what comes next in the input stream.  Sets backup to
// some portion of the whitespace that comes next, plus
// possibly a following non-whitespace character.
//
// If f.before_nl or f.not_before_nl is already set,
// nothing needs to be done.
//
// If f.type == EOF_TOKEN assumes EOF is next thing
// in input string.
//
// Returns f.before_nl.
//
bool before_nl ( file & f )
{
    if ( f.before_nl || f.not_before_nl )
        return f.before_nl;

    if ( f.type == EOF_TOKEN ) 
    {
        f.before_nl = true;
	return true;
    }

    // Scan characters to answer question.  Start by
    // scanning characters in backup, and then add to
    // backup until we scan the first non-whitespace
    // character or '\n' or EOF.  Set before_nl if we
    // find an `\n' or EOF, and not_before_nl other-
    // wise.
    //
    char * p = f.back;
    int c;
    while ( true )
    {
	// Get next character.
	//
        c = * p;
	if ( c != 0 )	++ p;
        else if ( p >= f.backup + sizeof ( f.backup ) ) {
	    whitespace_too_long ( f );
	}
	else
	{
	    c = f.stream.get();
	    if ( c == EOF ) {
		f.before_nl = true;
		return true;
	    }
	    * p ++ = c;
	    * p = 0;
	}

    	if ( ! isspace ( c ) )
	{
	    f.not_before_nl = true;
	    return false;
	}
	else if ( c == '\n' )
	{
	    f.before_nl = true;
	    return true;
        }
    }
}

// Possible difference types.
//
enum difference_type {
    LINEBREAK = 0,
    SPACEBREAK,
    WHITESPACE,
    BEGINSPACE,
    LINESPACE,
    ENDSPACE,
    WORD_EOF1,
    INTEGER_EOF1,
    FLOAT_EOF1,
    WORD_EOF2,
    INTEGER_EOF2,
    FLOAT_EOF2,
    FLOAT,
    INTEGER,
    DECIMAL,
    EXPONENT,
    SIGN,
    INFINITY,
    CASE,
    COLUMN,
    WORD,
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

    int		last_output_line;
    int		last_test_line;
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
    { "word-eof1",	false, 0, 0, MAX_PROOF_LINES },
    { "integer-eof1",	false, 0, 0, MAX_PROOF_LINES },
    { "float-eof1",	false, 0, 0, MAX_PROOF_LINES },
    { "word-eof2",	false, 0, 0, MAX_PROOF_LINES },
    { "integer-eof2",	false, 0, 0, MAX_PROOF_LINES },
    { "float-eof2",	false, 0, 0, MAX_PROOF_LINES },
    { "float",		false, 0, 0, MAX_PROOF_LINES },
    { "integer",	false, 0, 0, MAX_PROOF_LINES },
    { "decimal",	false, 0, 0, MAX_PROOF_LINES },
    { "exponent",	false, 0, 0, MAX_PROOF_LINES },
    { "sign",		false, 0, 0, MAX_PROOF_LINES },
    { "infinity",	false, 0, 0, MAX_PROOF_LINES },
    { "case",		false, 0, 0, MAX_PROOF_LINES },
    { "column",		false, 0, 0, MAX_PROOF_LINES },
    { "word",		false, 0, 0, MAX_PROOF_LINES }
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
double float_absdiff_limit	= -1.0;
double float_reldiff_limit	= -1.0;
double integer_absdiff_limit	= -1.0;
double integer_reldiff_limit	= -1.0;

struct proof
    // A description of one single proof to be output.
{
    difference_type	type;
        // Difference type.

    int			output_token_begin_column;
    int			output_token_end_column;
    int			test_token_begin_column;
    int			test_token_end_column;
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
    int			output_line;
    int			test_line;
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
    p->output_token_begin_column
    				= output.column -
				  output.length + 1;
    p->output_token_end_column	= output.column;
    p->test_token_begin_column	= test.column -
				  test.length + 1;
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
// `integer_absdiff_maximum' and `integer_reldiff_
// maximum') by writing the differences just found
// into these variables iff the new differences are
// larger than the previous values of these variables.
//
// Also calls found_difference for DECIMAL, EXPONENT,
// or SIGN if the two number `decimals' or `has_expo-
// nent' file members are unequal, or the number signs
// are unequal (where a sign is the first character of a
// token if that is `+' or `-' and is `\0' otherwise)
// and the numbers are both integers.
//
// If there is no computable difference, calls found_
// difference(INFINITY) instead.  This happens if one of
// the numbers is not `finite' or their difference is
// not `finite'.
// 
void diffnumber ()
{
    if ( ! finite ( output.number )
	 ||
	 ! finite ( test.number ) )
    {
	found_difference ( INFINITY );
	return;
    }

    double absdiff =
	( output.number - test.number );
    if ( ! finite ( absdiff ) )
    {
	found_difference ( INFINITY );
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

	found_difference ( INFINITY );
	return;
    }

    if ( output.is_float || test.is_float )
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

	char oc = output.token[0];
	char tc = test.token[0];

	if ( oc != '-' && oc != '+' ) oc = 0;
	if ( tc != '-' && tc != '+' ) tc = 0;

	if ( oc != tc )
	    found_difference ( SIGN );
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
        else if ( strncmp ( "doc", name, 3 ) == 0 )
	{
	    // Any -doc* option prints documentation
	    // and exits with error status.
	    //
	    cout << documentation;
	    exit (1);
	}

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
    	else if ( strcmp ( "all", name ) == 0 )
	{
	    int limit = isdigit ( argv[2][0] ) ?
		        (unsigned) atol ( argv[2] ) :
		        0;
	    for ( int j = 0; j < MAX_DIFFERENCE; ++ j )
		differences[j].proof_limit = limit;
	}
        else if ( strcmp ( "nosign", name ) == 0 )
	    nosign = true;
        else if ( strcmp ( "nonumber", name ) == 0 )
	    nonumber = true;
        else if ( strcmp ( "filtered", name ) == 0 )
	    filtered = true;
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

    bool last_match_was_word_diff	= false;
    bool skip_whitespace_comparison	= false;

    while ( ! done )
    {

	// Scan next tokens.
	//
	if ( last_match_was_word_diff
	     && ( output.remainder != test.remainder ||
	          output.type != test.type ||
		  before_nl ( output )
		      != before_nl ( test ) ) )
	{
	    assert (    ! output.remainder
	    	     || ! test.remainder );
	    assert (    ( output.type == WORD_TOKEN )
		     || ( test.type == WORD_TOKEN ) );

	    // If the last two tokens had a word diff-
	    // erence and one is a remainder or a
	    // number, or one is followed by a new line
	    // and the other is not, discard the
	    // remainder, the one not followed by a new
	    // line, or the word (non-number), leaving
	    // the other token for the next match.

	    if ( output.remainder )
		scan_token ( output );
	    else if ( test.remainder )
		scan_token ( test );
	    else if (      before_nl ( test )
	              && ! before_nl ( output ) )
		scan_token ( output );
	    else if (    ! before_nl ( test )
	              &&   before_nl ( output ) )
		scan_token ( test );
	    else if ( output.type == WORD_TOKEN )
		scan_token ( output );
	    else if ( test.type == WORD_TOKEN )
		scan_token ( test );
	}
	else
	{
	    scan_token ( output );
	    scan_token ( test );
	}

	// Compare tokens.
	//
	last_match_was_word_diff = false;
        switch ( output.type ) {

	case EOF_TOKEN:
	    switch ( test.type ) {
	    case EOF_TOKEN:
		done = true;
		break;
	    case WORD_TOKEN:
	        found_difference ( WORD_EOF1 );
		skip_whitespace_comparison = true;
		break;
	    case NUMBER_TOKEN:
		found_difference ( test.is_float ?
				   FLOAT_EOF1 :
				   INTEGER_EOF1 );
		skip_whitespace_comparison = true;
		break;
	    }

	    break;

	case NUMBER_TOKEN:
	case WORD_TOKEN:

	    if ( test.type == EOF_TOKEN )
	    {
		found_difference
		    ( output.type == WORD_TOKEN ?
		      WORD_EOF2 :
		      output.is_float ?
		      FLOAT_EOF2 :
		      INTEGER_EOF2 );
		skip_whitespace_comparison = true;
		break;
	    }
	    else if ( output.type != test.type )
	    {
		found_difference ( WORD );
		last_match_was_word_diff = true;
		break;
	    }

	    // If both tokens are words and one is
	    // longer than the other, split the longer
	    // word.  If we get a word diff, we will
	    // undo the split.

	    if ( output.type == WORD_TOKEN )
	    {
		if ( output.length < test.length )
		    split_word ( test, output.length );
		else if ( test.length < output.length )
		    split_word ( output, test.length );
	    }

	    // Compare tokens for match that is either
	    // exact or exact but for case.

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

		// Tokens are equal except perhaps for
		// case.

		if ( token_case )
		    found_difference
		        ( output.type != NUMBER_TOKEN ?
			      CASE :
			  output.is_float ?
			      FLOAT :
			      INTEGER );
	    }

	    else if ( output.type == NUMBER_TOKEN )
	    {
	        // Tokens are not equal with case
		// ignored, but both are numbers.

	        assert ( test.type == NUMBER_TOKEN );
		diffnumber ();
	    }

	    else
	    {
	        // Tokens are not equal with case
		// ignored, and both are words.

		assert ( test.type == WORD_TOKEN );

	    	undo_split ( test );
	    	undo_split ( output );

		found_difference ( WORD );
		last_match_was_word_diff = true;
	    }

	    break;
     	}

	// The rest of the loop compares columns and
	// whitespace.  If we are skipping whitespace
	// comparisons because one file is longer than
	// the other, continue loop here.

	if ( skip_whitespace_comparison ) continue;

	// Compare column numbers.  This is done after
	// token comparison so that the results of word
	// splitting can be taken into account in token
	// ending column numbers.  It is not done if
	// both files have EOF_TOKENs.

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
			&& output.is_float;
		    bool test_is_float =
		        test.type == NUMBER_TOKEN
			&& test.is_float;

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
		cout << " "
		     << p->output_token_begin_column
		     << " "
		     << p->output_token_end_column;
		cout << " "
		     << p->test_token_begin_column
		     << " "
		     << p->test_token_end_column;

		last_output_column =
		    p->output_token_end_column;
		last_test_column =
		    p->test_token_end_column;
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
