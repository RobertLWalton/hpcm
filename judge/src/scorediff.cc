// Programming Contest File Difference Tester
//
// File:	scorediff.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Fri Sep 23 05:46:41 EDT 2005
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: hc3 $
//   $Date: 2005/09/23 10:00:07 $
//   $RCSfile: scorediff.cc,v $
//   $Revision: 1.67 $

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
unsigned const PROOF_LIMIT = 10;

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
"    definition either a word, a number, a beginning\n"
"    of test case (boc), a beginning of test group\n"
"    (bog), or an end-of-file (eof).\n"
"\n"
"    A word is just a string of non-whitespace char-\n"
"    acters that does not contain any part of a num-\n"
"    ber.  Thus words cannot contain digits, nor can\n"
"    they contain signs, decimal points, or exponents\n"
"    that are parts of numbers.  A number with a dec-\n"
"    imal point or exponent is a floating point num-\n"
"    ber, while other numbers are integers.\n"
"\n"
"    The -filtered option specifies that both the\n"
"    output_file and the test_file are filtered\n"
"    output from the jfilter program, and have marker\n"
"    characters at the beginning of each line to\n"
"    delimit test cases and test groups.  The legal\n"
"    marks are:\n"
"\n"
"        |  begin a test group\n"
"        -  begin a test case\n"
"        +  begin a test group and a test case\n"
"        .  neither begin a test group\n"
"           nor begin a test case\n"
"\n"
"    These marks are translated into beginning of\n"
"    test group (bog) or beginning of test case (boc)\n"
"    tokens.  + translates to a bog followed by a\n"
"    boc.\n"
"\f\n"
"    The -nonumber option specifies that all number\n"
"    characters should be treated as word characters,\n"
"    so there are no number tokens.  The -nosign\n"
"    option specifies that sign characters preceding\n"
"    numbers should be treated as word characters, so\n"
"    numbers can only include signs in their exponent\n"
"    parts.\n"
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
"\f\n"
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
"		token is a floating point number.\n"
"\f\n"
"    word-number    Here TYPE1-TYPE2 means a token of\n"
"    number-word    type TYPE1 in the first file was\n"
"    word-boc       matched to a token of type TYPE2\n"
"    boc-word       in the second file.\n"
"    word-bog\n"
"    bog-word\n"
"    word-eof\n"
"    eof-word\n"
"    number-boc\n"
"    boc-number\n"
"    number-bog\n"
"    bog-number\n"
"    number-eof\n"
"    eof-number\n"
"    boc-bog\n"
"    bog-boc\n"
"    boc-eof\n"
"    eof-boc\n"
"    bog-eof\n"
"    eof-bog\n"
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
"    word	Both of the two matching tokens are\n"
"		word tokens, and it is not true that\n"
"		they are of the same length (see\n"
"		word splitting below) and are iden-\n"
"		tical except for letter case.\n"
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
"		lists differences so that line will\n"
"		never be blank.\n"
"\n"
"    The files are parsed into whitespace, words,\n"
"    numbers, beginning-of-cases (bocs), beginning-\n"
"    of-groups (bogs), and end-of-files (eofs).  A\n"
"    number is an optional sign followed by digits\n"
"    containing an optional decimal point followed by\n"
"    an optional exponent.  An exponent is an `e' or\n"
"    `E' followed by an an optional sign followed by\n"
"    digits.  Two numbers are first checked to see if\n"
"    they are identical as character strings, except\n"
"    for exponent letter case.  Otherwise numbers\n"
"    are scanned by the strtod(3) function before\n"
"    they are compared.  A number with a decimal\n"
"    point or exponent is a floating point number,\n"
"    while other numbers are integers.\n"
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
"    The main exception occurs when two tokens that\n"
"    are being matched have different types.  Token\n"
"    types are ordered, and when tokens of types\n"
"    T1 < T2 are matched, only the token of type T1\n"
"    is skipped.  When skipping a stream of tokens of\n"
"    types < T2 in this way, only the first token of\n"
"    each skipped type T1 is reported as a T1-T2\n"
"    difference.  This if a word is matched to an\n"
"    eof, and the word is followed by a number,\n"
"    another word, another number, and an eof, a\n"
"    word-eof difference will be reported for the\n"
"    first word but not the second, and a number-eof\n"
"    difference will be reported for the first num-\n"
"    ber but not the second.  In addition, compari-\n"
"    sons of whitespace preceding or following a\n"
"    token skipped in this way are suppressed.\n"
"\n"
"    The order of token types is word, number, boc,\n"
"    bog, eof.  Thus scorediff tries to match num-\n"
"    bers at the cost of matching words, tries to\n"
"    match boc's at the cost of matching words or\n"
"    numbers, etc.\n"
"\f\n"
"    The other exception occurs following two words\n"
"    that do not match even if case is ignored.\n"
"    If one word is a remainder of a split and the\n"
"    other is not (it cannot be, actually), then\n"
"    only the remainder is skipped.  If one of the\n"
"    words is followed by white space containing a\n"
"    new line or ending with an end of file, and the\n"
"    other token is not so followed, only this other\n"
"    token is skipped.\n"
"\n"
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
"\f\n"
"    Note that if the two matching numbers have expo-\n"
"    nents and the letter case of the `e' or `E' in\n"
"    the two exponents does not match, then the diff-\n"
"    erence will always be reported as a `float'\n"
"    difference and not a `case' difference.\n"
"\n"
"    For the purpose of computing the column of a\n"
"    character, tabs are set every 8 columns.\n"
"\n"
"    To avoid whitespace comparison anomalies, a new\n"
"    line is added in front of each file before the\n"
"    file is parsed.  Thus differences in whitespace\n"
"    beginning the first line of a file are reported\n"
"    as `beginspace' differences.\n"
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
"                    `TYPE1-TYPE2' |\n"
"                    `linebreak' | `spacebreak' |\n"
"                    `whitespace' | `beginspace' |\n"
"                    `linespace' | `endspace'\n"
"\n"
"             where TYPE1 and TYPE2 are not the same.\n"
"\n"
"          TYPE ::= `word' | `number' |\n"
"                   `boc' | `bog' | `eof'\n"
"\n"
"          absolute-difference ::=\n"
"                    floating-point-number\n"
"\n"
"          relative-difference ::=\n"
"                    floating-point-number\n"
"\n"
"    Here the column numbers in a line start with 0\n"
"    and line numbers in a file start with 1.  The\n"
"    test groups in a file are numbered 1, 2, 3, ...,\n"
"    and consist of disjoint sets of lines.  Lines\n"
"    before the first test group in the file (first\n"
"    bog) are treated as being in group 0.  The test\n"
"    cases within a test group are numbered 1, 2, 3,\n"
"    ..., with lines before the first test case in a\n"
"    group being treated as being in test case 0.\n"
"    If the -filtered option is not given, the test\n"
"    group and test case numbers are all 0.\n"
"\f\n"
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
"\n"
"    There is a limit for each difference type to the\n"
"    number of proofs of that type that will be out-\n"
"    put for each test group.  Specifically, if the\n"
"    limit is N for difference type T, then after N\n"
"    line-proofs each containing at least one proof\n"
"    of type T have output with a particular group\n"
"    number, no more proofs of type T will be output\n"
"    with that group number.  Group number 0 is\n"
"    treated here as a separate group, which is the\n"
"    only group if the -filtered option is absent.\n"
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
"\f\n"
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
"\n"
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

// A token is either a word token, a number token, a
// beginning of test group (bog) token, a beginning of
// test case (boc) token, or an end of file (eof) token.
//
// These are given in a specific order so that if there
// is a token type mismatch between tokens, only the
// token with smaller type is skipped.
//
enum token_type {
    WORD_TOKEN = 0, NUMBER_TOKEN,
    BOC_TOKEN, BOG_TOKEN, EOF_TOKEN, MAX_TOKEN };

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
    			// if not a beginning-of-case
			// (boc), beginning-of-group
			// (bog), or end-of-file (eof).
			// Not terminated by `\0', as
			// that is a legal token
			// character.
    int length;		// Length of token in charac-
    			// ters.  0 for boc, bog, eof.
    int test_group;	// Test group number of current
    			// token.  The first group is 1.
			// The group number is 0 before
			// the first test group begins,
			// or if there is no -filtered
			// option.
    int test_case;	// Ditto but test case number.  
    			// Test cases are numbered from
			// the beginning of their test
			// group.  Lines before the
			// first test case in a group
			// get the test case number 0.
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
    bool remainder;	// True iff this token is a
    			// remainder from a split token.
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
    // word token, reset the token length to the length
    // of the first part and the remainder length to
    // the length of the second part.
    //
    int remainder_length;  // Length of remainder.
    			   // 0 if no remainder.

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
    // Can also be set to some of the whitespace follow-
    // ing a token (by before_nl), possibly followed by
    // a single non-whitespace character.

    char * backp;	// If >= endbackp, there are no
    			// backed up characters to de-
			// liver.  Otherwise use
			// `* backp ++' to input next
			// character.
    char * endbackp;	// Points just after last backup
    			// character.
    char backup [ 5 + MAX_SIZE + 1 ]; 
    			// Buffer of backed up char-
    			// acters into which `back'
			// points.  NOT terminated by
			// `\0', as that is a legitimate
			// input character.
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
    f.backp		= f.backup;
    f.endbackp		= f.backup + 1;

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
    if ( f.backp < f.endbackp ) return * f.backp ++;
    else return f.stream.get();
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
void zero_proof_lines ( void );
void scan_token ( file & f )
{
    if ( f.type == EOF_TOKEN ) return;
    if ( f.boc_next )
    {
	// Current token is part of BOG/BOC pair.
	//
	assert ( f.type == BOG_TOKEN );
        f.boc_next	= false;
	++ f.test_case;
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
	char * endq = q + f.remainder_length;
	while ( q < endq ) * q ++ = * p ++;
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

    while ( isspace ( c ) ) {
        if ( c == '\n' ) {
	    column = -1;
	    ++ f.newlines;
	    ++ f.line;
	    if ( filtered ) {
	        switch ( get_character ( f ) ) {
		case '+':	++ f.test_group;
				f.test_case = 0;
				f.type = BOG_TOKEN;
				zero_proof_lines();
				f.column = column;
				f.boc_next = true;
				return;
		case '-':	++ f.test_case;
				f.type = BOC_TOKEN;
				f.column = column;
				return;
		case '|':	++ f.test_group;
				f.test_case = 0;
				f.type = BOG_TOKEN;
				zero_proof_lines();
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

	    assert ( f.backp == f.endbackp );
	    int len = tp - ep;
	    memcpy ( f.backup, ep, len );
	    f.backp = f.backup;
	    f.endbackp = f.backup + len;
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

    f.type	= NUMBER_TOKEN;
    f.length	= tp - f.token;
    f.column	= -- column;
    f.decimals	= decimals;
    f.is_float	= ( f.decimals >= 0 ) || f.has_exponent;

    // Put c into backup.

    if ( c != EOF ) {
	if ( f.backp == f.endbackp )
	    f.backp = f.endbackp = f.backup;
	* f.endbackp ++ = c;
    }

    // Convert number token to floating point using
    // strtod.

    char * e;
    f.token[f.length] = 0;
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

		assert ( f.backp == f.endbackp );
		int len = tp - np;
		memcpy ( f.backup, np, len );
		f.backp = f.backup;
		f.endbackp = f.backup + len;
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

    // Put c into backup.

    if ( c != EOF ) {
	if ( f.backp == f.endbackp )
	    f.backp = f.endbackp = f.backup;
	* f.endbackp ++ = c;
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
    char * p = f.backp;
    int c;
    while ( true )
    {
	// Get next character.
	//
	if ( p < f.endbackp ) c = * p ++;
        else if ( p >= f.backup + sizeof ( f.backup ) )
	    whitespace_too_long ( f );
	else
	{
	    c = f.stream.get();

	    if ( c == EOF ) {
		f.before_nl = true;
		return true;
	    }

	    * f.endbackp ++ = c;
	    p = f.endbackp;
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

// Possible difference types.  The first group
// have indices computed by the function that
// followed the enum definition.
//
enum difference_type {
    LINEBREAK = MAX_TOKEN * MAX_TOKEN,
    SPACEBREAK,
    WHITESPACE,
    BEGINSPACE,
    LINESPACE,
    ENDSPACE,
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
//
inline difference_type type_mismatch 
	( token_type TYPE1, token_type TYPE2 )
{
    return difference_type
    		( TYPE1 * MAX_TOKEN + TYPE2 );
}

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

    unsigned	proof_lines;
       // Number of proof lines containing a proof of
       // this difference type.  Incremented conceptual-
       // ly at the end of a line in either file if a
       // proof for a difference of this type has been
       // output for that line.  In actual practice, the
       // incrementing is not done till the next differ-
       // ence of this type is found.

    unsigned	proof_limit;
       // If not greater than proof_count, suppresses
       // further output of proofs of this difference
       // type.
};

// Information on the various differences found.
//
difference differences[] = {
    { NULL,		false, 0, 0, 0, 0 },
    { "word-number",	false, 0, 0, 0, PROOF_LIMIT },
    { "word-boc",	false, 0, 0, 0, PROOF_LIMIT },
    { "word-bog",	false, 0, 0, 0, PROOF_LIMIT },
    { "word-eof",	false, 0, 0, 0, PROOF_LIMIT },
    { "number-word",	false, 0, 0, 0, PROOF_LIMIT },
    { NULL,		false, 0, 0, 0, 0 },
    { "number-boc",	false, 0, 0, 0, PROOF_LIMIT },
    { "number-bog",	false, 0, 0, 0, PROOF_LIMIT },
    { "number-eof",	false, 0, 0, 0, PROOF_LIMIT },
    { "boc-word",	false, 0, 0, 0, PROOF_LIMIT },
    { "boc-number",	false, 0, 0, 0, PROOF_LIMIT },
    { NULL,		false, 0, 0, 0, 0 },
    { "boc-bog",	false, 0, 0, 0, PROOF_LIMIT },
    { "boc-eof",	false, 0, 0, 0, PROOF_LIMIT },
    { "bog-word",	false, 0, 0, 0, PROOF_LIMIT },
    { "bog-number",	false, 0, 0, 0, PROOF_LIMIT },
    { "bog-boc",	false, 0, 0, 0, PROOF_LIMIT },
    { NULL,		false, 0, 0, 0, 0 },
    { "bog-eof",	false, 0, 0, 0, PROOF_LIMIT },
    { "eof-word",	false, 0, 0, 0, PROOF_LIMIT },
    { "eof-number",	false, 0, 0, 0, PROOF_LIMIT },
    { "eof-boc",	false, 0, 0, 0, PROOF_LIMIT },
    { "eof-bog",	false, 0, 0, 0, PROOF_LIMIT },
    { NULL,		false, 0, 0, 0, 0 },
    { "linebreak",	false, 0, 0, 0, PROOF_LIMIT },
    { "spacebreak",	false, 0, 0, 0, PROOF_LIMIT },
    { "whitespace",	false, 0, 0, 0, PROOF_LIMIT },
    { "beginspace",	false, 0, 0, 0, PROOF_LIMIT },
    { "linespace",	false, 0, 0, 0, PROOF_LIMIT },
    { "endspace",	false, 0, 0, 0, PROOF_LIMIT },
    { "float",		false, 0, 0, 0, PROOF_LIMIT },
    { "integer",	false, 0, 0, 0, PROOF_LIMIT },
    { "decimal",	false, 0, 0, 0, PROOF_LIMIT },
    { "exponent",	false, 0, 0, 0, PROOF_LIMIT },
    { "sign",		false, 0, 0, 0, PROOF_LIMIT },
    { "infinity",	false, 0, 0, 0, PROOF_LIMIT },
    { "case",		false, 0, 0, 0, PROOF_LIMIT },
    { "column",		false, 0, 0, 0, PROOF_LIMIT },
    { "word",		false, 0, 0, 0, PROOF_LIMIT }
};

// Function to zero difference proof_lines counts.
//
void zero_proof_lines ( void )
{
    int i; for ( i = 0; i < MAX_DIFFERENCE; ++ i )
	differences[i].proof_lines == 0;
}

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

    if (    (    type != FLOAT
	      || absdiff > float_absdiff_limit
	      || reldiff > float_reldiff_limit )
         && (    type != INTEGER
	      || absdiff > integer_absdiff_limit
	      || reldiff > integer_reldiff_limit ) )
    {
        // Conceptually, d.proof_lines is incremented
	// at the end of a proof line containing an
	// output of a proof of the given `type'.  But
	// in practice, to reduce coding complexity,
	// the incrementing is deferred until the next
	// proof of this type is discovered, and then
	// the incrementing is done here.
	//
	if ( d.last_output_line != 0
	     && ( d.last_output_line != output.line
	         ||
	         d.last_test_line != test.line ) )
	    ++ d.proof_lines;

	if ( d.proof_lines < d.proof_limit )
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
	    if ( differences[i].name == NULL )
	        continue;
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

    while ( ! done )
    {
	bool skip_whitespace_comparison	= false;

	// Scan next tokens.
	//
	if ( output.type != test.type )
	{
	    // Type differences for current tokens have
	    // not yet been announced by calling found_
	    // difference.

	    bool announced[MAX_TOKEN];
	    for ( int i = 0; i < MAX_TOKEN; ++ i )
	        announced[i] = false;
	    if ( output.type < test.type )
	    {
	        while ( output.type < test.type )
		{
		     if ( ! announced[output.type] )
		     {
			found_difference
			    ( type_mismatch
				( output.type,
				  test.type ) );
		        announced[output.type] = true;
		     }
		     scan_token ( output );
		}
	    }
	    else
	    {
	        while ( test.type < output.type )
		{
		     if ( ! announced[test.type] )
		     {
			found_difference
			    ( type_mismatch
				( output.type,
				  test.type ) );
		        announced[test.type] = true;
		     }
		     scan_token ( test );
		}
	    }
	    skip_whitespace_comparison = true;
	}
	else if ( last_match_was_word_diff
		  && (    output.remainder
		          != test.remainder
		       ||
		       before_nl ( output )
			   != before_nl ( test ) ) )
	{
	    assert (    ! output.remainder
	    	     || ! test.remainder );

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
	}
	else
	{
	    scan_token ( output );
	    scan_token ( test );
	}

	// Compare tokens.  Type mismatch is handled
	// at beginning of containing loop.
	//
	if ( output.type != test.type ) continue;

	last_match_was_word_diff = false;
        switch ( output.type ) {

	case EOF_TOKEN:
		done = true;
	case BOG_TOKEN:
	case BOC_TOKEN:
		break;

	case NUMBER_TOKEN:
	case WORD_TOKEN:

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
	    char * endtp2 = tp2 + test.length;
	    bool token_match_but_for_case =
	        ( output.length == test.length );
	    bool token_match = token_match_but_for_case;

	    while ( tp2 < endtp2
	            && token_match_but_for_case )
	    {
		if ( * tp1 != * tp2 )
		{
		    token_match = false;
		    token_match_but_for_case =
			( toupper ( * tp1 )
			  == toupper ( * tp2 ) );
		}
		++ tp1, ++ tp2;
	    }

	    if ( token_match_but_for_case )
	    {
		if ( ! token_match )
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

	assert ( output.type == test.type );

	// Compare column numbers.  This is done after
	// token comparison so that the results of word
	// splitting can be taken into account in token
	// ending column numbers.  It is not done if
	// both files have BOC_TOKENs, BOG_TOKENS, or
	// EOF_TOKENs.

	if (    output.type != EOF_TOKEN
	     && output.type != BOG_TOKEN
	     && output.type != BOC_TOKEN
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
