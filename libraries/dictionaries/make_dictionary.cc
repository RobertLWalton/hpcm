// Program to Make Dictionaries
//
// File:     make_dictionary.cc
// Authors:  Bob Walton <walton@seas.harvard.edu>
// Date:     Thu Aug 31 04:35:48 EDT 2017
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.

#include <iostream>
#include <fstream>
#include <string>
#include <unordered_map>
#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <cctype>
#include <cassert>
using std::cout;
using std::endl;
using std::cerr;
using std::cin;
using std::ifstream;
using std::string;
using std::unordered_map;
using std::sort;

const int MAX_WORDS = 1e6;
    // Maximum number of distinct words that can be
    // processed.

const int MAX_CODE = 13;
const char * abbreviation[MAX_CODE+1] = {
    "ignored",	// 0
    "n",	// 1
    "v",	// 2
    "adj",	// 3
    "adv",	// 4
    "pron",	// 5
    "rel",	// 6
    "modal",	// 7
    "inf",	// 8
    "det",	// 9
    "cconj",	// 10
    "sconj",	// 11
    "prep",	// 12
    "inter"	// 13
};

enum {
    IGNORED = 1 << 0,
    NOUN    = 1 << 1,
    VERB    = 1 << 2,
    ADJ     = 1 << 3,
    ADV     = 1 << 4
};

const int MAX_SENSET_TYPE = 5;
const int senset_type[MAX_SENSET_TYPE+1] =
	{ 0, 1, 2, 3, 4, 3 };
    // index.sense line format is:
    //
    //     word%T...
    //
    // where T is the senset type digit and
    //
    //     senset_type[T-'0']
    //
    // is the corresponding part of speech code.

const char * senset_file =
    "index.sense";
const char * frequency_file =
    "words-by-frequency.txt";
const char * spell_checker_file =
    "spell-checker-file.txt";
const char * part_of_speech_file =
    "part-of-speech.txt";
const char * ignored_words_file =
    "ignored-words.txt";
const char * approvals_file =
    "word-approvals.txt";

const int NUMBER_PART_FILES = 8;
struct part_file
{
    const char * file_name;
    int part;
} part_files[NUMBER_PART_FILES] = {
    { "index.noun", NOUN },
    { "noun.exc", NOUN },
    { "index.verb", VERB },
    { "verb.exc", VERB },
    { "index.adj", ADJ },
    { "adj.exc", ADJ },
    { "index.adv", ADV },
    { "adv.exc", ADV } };

const int NUMBER_MORPH_RULES = 18;
struct morph_rule
{
    int suffix_length;
    const char * suffix;
    const char * replacement_suffix;
    int parts;
} morph_rules[NUMBER_MORPH_RULES] = {
    { 1, "s", "", NOUN | VERB },
    { 2, "es", "", VERB },
    { 2, "es", "s", VERB },
    { 2, "ed", "e", VERB },
    { 2, "ed", "", VERB },
    { 2, "er", "e", ADJ },
    { 2, "er", "", ADJ },
    { 3, "ses", "s", NOUN },
    { 3, "xes", "x", NOUN },
    { 3, "zes", "z", NOUN },
    { 3, "men", "man", NOUN },
    { 3, "ies", "y", NOUN | VERB },
    { 3, "ing", "e", VERB },
    { 3, "ing", "", VERB },
    { 3, "est", "e", ADJ },
    { 3, "est", "", ADJ },
    { 4, "ches", "ch", NOUN },
    { 4, "shes", "sh", NOUN } };

const char * const documentation = "\n"
"make_dictionary [-[acdn]] [size approvals]\n"
"\n"
"    Given input listing words in order of importance\n"
"    (e.g., in frequency order), output these words\n"
"    with their parts of speech.  Input has one word\n"
"    per line.  Output lines have the form:\n"
"\n"
"        word code abbreviation\n"
"\n"
"    The possible parts of speech are:\n"
"\n"
"      Code   Abbreviation    Part of Speech\n"
"\n"
"        1    n               Noun\n"
"        2    v               Verb\n"
"        3    adj             Adjective\n"
"        4    adv             Adverb\n"
"        5    pron            Pronoun\n"
"        6    rel             Relative Pronoun\n"
"        7    modal           Modal Verb\n"
"        8    inf             Infinitive Marker\n"
"        9    det             Determiner\n"
"        10   cconj           Coordinating\n"
"                             Conjunction\n"
"        11   sconj           Subordinating\n"
"                             Conjunction\n"
"        12   prep            Preposition\n"
"        13   inter           Interjection\n"
"\n"
"    The code and/or abbreviation can be omitted\n"
"    from the output.  The -a option omits the\n"
"    abbreviation and the -c option omits the code.\n"
"\n"
"    The `size' argument gives the number of words to\n"
"    be output.  The `approvals' argument gives the\n"
"    number of approvals a word must have to appear\n"
"    in the output.  A word gets one approval for\n"
"    each spell checker file it appears in.  The\n"
"    `size' and `approvals' arguments must not be\n"
"    given if -d is used.\n"
"\n"
"    The words are sorted after they are all input\n"
"    and then output in alphabetical order, unless\n"
"    the -d option is given, in which case the words\n"
"    are output as soon as they are input, for\n"
"    debugging purposes.  If -d is given the\n"
"    approvals A of the word are also output by\n"
"    appending `/A' to the word.\n"
"\n"
"    Normally input words whose part of speech\n"
"    cannot be determined are ignored.  If the -n\n"
"    option is given, each such word not listed in\n"
"    the ignored-words.txt file is output on a line\n"
"    by itself, and the normal output is suppressed.\n"
"\n"
"    A separate line is output for each part of\n"
"    speech that a word has, if the word has more\n"
"    than one.\n"
"\n"
"    The input files are:\n"
"\n"
"        index.*\n"
"        *.exc\n"
"            Word Net (Princeton) files.\n"
"            Determines which words are nouns, verbs,\n"
"            adjectives, or adverbs.\n"
"\n"
"        part-of-speech.txt\n"
"            Part of speech information not in Word\n"
"            Net files.  File lines have the formats:\n"
"\n"
"                word part-of-speech-abbreviation\n"
"                word !part-of-speech-abbreviation\n"
"\n"
"            where the first form adds the part of\n"
"            speech to the word and the second form\n"
"            removes the part of speech from the\n"
"            word.\n"
"\n"
"            The file may also have comment lines\n"
"            beginning with #.\n"
"\n"
"        spell-checker.txt\n"
"            Legal word list.  Parts of speech of\n"
"            these words are determined by applying\n"
"            morpheme formation rules to them and\n"
"            looking up the base words using parts\n"
"            determined by the above files.\n"
"\n"
"            A word appearing in this file gets one\n"
"            approval.\n"
"\n"
"            Currently there is only one spell\n"
"            checker file.\n"
"\n"
"        ignored-words.txt\n"
"            Illegal word list.  These words will\n"
"            NOT be output.\n"
"\n"
"            The file may also have comment lines\n"
"            beginning with #.\n"
"\n"
"        word-approvals.txt\n"
"            This file sets the number of approvals\n"
"            of particular words.  The file lines\n"
"            have the format:\n"
"\n"
"                word number-of-approvals\n"
"\n"
"            and override approval counts from spell\n"
"            checker files.\n"
"\n"
"            The file may also have comment lines\n"
"            beginning with #.\n"
"\n"
;

struct entry  // dictionary entry
{
    string word;
    int parts;
        // Bit 1 << c is set is parts if the word has
	// the part of speech with code c.
    short approvals;
        // Number of spell checker files that contain
	// the word, thereby indicating it is a
	// real word (and not just `m' or `sh').
    bool include;
        // Include in output; i.e., word is one of
	// the `size' most frequent.
} dictionary[MAX_WORDS];
int D;    // Number of entries in dictionary.

// Sort included entries before not included entries
// and otherwise sort by word lexical order.
//
bool operator < ( const entry & e1, const entry & e2 )
{
    if ( ! e1.include && e2.include ) return false;
    else if ( e1.include && ! e2.include ) return true;
    else return e1.word < e2.word;
}

unordered_map<string, entry *> hashtable;
typedef unordered_map<string, entry *>::iterator
    hashp;

bool output_abbreviations = true;
bool output_codes = true;
bool output_no_parts_words = false;
bool debug = false;
long size;
long required_approvals;

// Add a word to the dictionary with the given parts
// of speech.
//
void add_word ( const char * word, int parts )
{
    string word_str = word;
    hashp hp = hashtable.find ( word_str );
    if ( hp == hashtable.end() )
    {
	entry & e = dictionary[D++];
	e.word = word_str;
	e.parts = parts;
	e.approvals = 0;
	e.include = false;
	hashtable.emplace ( word_str, & e );
    }
    else
    {
	entry & e = * hp->second;
	e.parts |= parts;
    }
}

// Given a word, check if one of its morphs is in the
// dictionary, and if so, add the word with the parts
// found designated for the morph rule that are found
// for the morph in the dictionary.
//
void add_morphs ( const char * word )
{
    unsigned wsize = strlen ( word );
    for ( int i = 0; i < NUMBER_MORPH_RULES; ++ i )
    {
        morph_rule & mr = morph_rules[i];
	if (    wsize <= mr.suffix_length
	     || strcmp
	          ( word + wsize - mr.suffix_length,
		    mr.suffix ) != 0 )
	    continue;

	string sword =
	      string ( word, wsize - mr.suffix_length )
	    + mr.replacement_suffix;
	hashp hp = hashtable.find ( sword );
	if ( hp == hashtable.end() ) continue;
	entry & e = * hp->second;
	int parts = e.parts & mr.parts;
	if ( parts == 0 ) continue;
	add_word ( word, parts );
    }
}

// Remove parts of speech from a word IF the word
// is in the dictionary.
//
void remove_parts ( const char * word, int parts )
{
    string sword = word;
    hashp hp = hashtable.find ( sword );
    if ( hp == hashtable.end() ) return;
    entry & e = * hp->second;
    e.parts &= ~ parts;
}

// Increment the approval count of a word IF the word
// is in the dictionary.
//
void approve_word ( const char * word )
{
    string sword = word;
    hashp hp = hashtable.find ( sword );
    if ( hp == hashtable.end() ) return;
    entry & e = * hp->second;
    ++ e.approvals;
}

// Set the approval count of a word IF the word
// is in the dictionary.
//
void set_approvals ( const char * word, int approvals )
{
    string sword = word;
    hashp hp = hashtable.find ( sword );
    if ( hp == hashtable.end() ) return;
    entry & e = * hp->second;
    e.approvals = approvals;
}

// Output entry, one line for each part of speech
// of entry.
//
void output ( entry & e )
{
    string word = e.word;
    unsigned wsize = word.size();
    if ( word == "i" ) word = "I";

    for ( int c = 0; c <= MAX_CODE; ++ c )
    {
	if ( ( 1 << c ) & e.parts )
	{
	    cout << word;
	    if ( debug ) cout << "/" << e.approvals;
	    if ( output_codes )
	        cout << " " << c;
	    if ( output_abbreviations )
		cout << " " << abbreviation[c];
	    cout << endl;
	}
    }
}

// Main program.
//
int main ( int argc, char ** argv )
{

    // Process options.

    if ( argc >= 2 && argv[1][0] == '-' )
    {
        const char * p = argv[1] + 1;
	while ( * p )
	{
	    switch ( * p ) {
	    case 'd': debug = true;
	              break;
	    case 'a': output_abbreviations = false;
	              break;
	    case 'c': output_codes = false;
	              break;
	    case 'n': output_no_parts_words = true;
	              break;
	    default:
	        cerr << "Unrecognized option -" << *p
		     << endl;
		exit ( 1 );
	    }
	    ++ p;
	}
	-- argc, ++ argv;
    }

    if ( debug || output_no_parts_words ?
         argc != 1 :
         argc != 3 || argv[1][0] == '-' )
    {

	// Any unrecognized -* option prints documenta-
	// tion and exits with error status.
	//
	cout << documentation;
	exit (1);
    }

    if ( debug || output_no_parts_words )
        size = 1e9, required_approvals = 0;
    else
    {
	char * p;
	size = strtol ( argv[1], & p, 10 );
	if ( size < 0 || * p )
	{
	    cerr << "ERROR: bad size argument."
		 << endl;
	    exit ( 1 );
	}
	required_approvals =
	    strtol ( argv[2], & p, 10 );
	if ( required_approvals < 0 || * p )
	{
	    cerr << "ERROR: bad approvals argument."
		 << endl;
	    exit ( 1 );
	}
    }

    ifstream in;
    char line[5000];

    // Process part files.
    //
    for ( int i= 0; i < NUMBER_PART_FILES; ++ i )
    {
        part_file & pf = part_files[i];
	in.open ( pf.file_name );
	if ( ! in )
	{
	    cout << "Cannot open " << pf.file_name
	         << endl;
	    exit ( 1 );
	}

	int Dstart = D;
	while ( in.getline ( line, sizeof ( line ) ),
		in.good() )
	{
	    assert
	      ( strlen ( line ) < sizeof ( line ) - 1 );

	    if ( line[0] == ' ' ) continue;

	    char * p = line;
	    while ( * p && ! isspace ( * p ) ) ++ p;
	    * p = 0;

	    add_word ( line, pf.part );
	}
	in.close();
	cerr << "File " << pf.file_name
	     << " added " << D - Dstart
	     << " dictionary entries." << endl;
    }
    int Dparts = D;
    cerr << "Part files added " << Dparts
         << " dictionary entries." << endl;

    // Process senset file.
    //
    in.open ( senset_file );
    if ( ! in )
    {
	cout << "Cannot open " << senset_file << endl;
	exit ( 1 );
    }
    while ( in.getline ( line, sizeof ( line ) ),
            in.good() )
    {
        assert
	    ( strlen ( line ) < sizeof ( line ) - 1 );
	char * p = line;
	while ( * p && * p != '%' ) ++ p;
	assert ( * p == '%' );
	* p ++ = 0;
	int i = * p - '0';
	assert ( 1 <= i && i <= MAX_SENSET_TYPE );
	int c = senset_type[i];

	add_word ( line, 1 << c );
    }
    in.close();
    int Dsenset = D - Dparts;
    cerr << "Senset file added " << Dsenset
         << " dictionary entries." << endl;

    // Process part-of-speech file.
    //
    in.open ( part_of_speech_file );
    if ( ! in )
    {
	cout << "Cannot open " << part_of_speech_file
	     << endl;
	exit ( 1 );
    }
    while ( in.getline ( line, sizeof ( line ) ),
            in.good() )
    {
        assert
	    ( strlen ( line ) < sizeof ( line ) - 1 );
	if ( line[0] == '#' ) continue;
	if ( line[0] == 0 ) continue;

	char * p = line;
	while ( * p && ! isspace ( * p ) ) ++ p;
	* p ++ = 0;
	bool remove = ( * p == '!' );
	if ( remove ) ++ p;
	int c = 0;
	for ( int i = 1; i <= MAX_CODE; ++ i )
	{
	    if ( strcmp ( abbreviation[i], p ) == 0 )
	    {
	        c = i;
		break;
	    }
	}
	if ( c == 0 )
	{
	    cerr << "ERROR: Could not recognize `"
	         << p
		 << "' as part of speech for word `"
		 << line << "'." << endl;
	    continue;
	}

	if ( remove )
	    remove_parts ( line, 1 << c );
	else
	    add_word ( line, 1 << c );
    }
    in.close();
    int Dpart = D - Dsenset - Dparts;
    cerr << "Part of speech file added " << Dpart
         << " dictionary entries." << endl;

    // Process spell checker file.
    //
    in.open ( spell_checker_file );
    if ( ! in )
    {
	cout << "Cannot open " << spell_checker_file
	     << endl;
	exit ( 1 );
    }
    while ( in.getline ( line, sizeof ( line ) ),
            in.good() )
    {
        assert
	    ( strlen ( line ) < sizeof ( line ) - 1 );

	add_morphs ( line );
	approve_word ( line );
    }
    in.close();
    int Dchecker = D - Dsenset - Dparts - Dpart;
    cerr << "Spell checker file added " << Dchecker
         << " dictionary entries." << endl;

    // Process ignored words file.
    //
    int ignored_count = 0;
    in.open ( ignored_words_file );
    if ( ! in )
    {
	cout << "Cannot open " << ignored_words_file
	     << endl;
	exit ( 1 );
    }
    while ( in.getline ( line, sizeof ( line ) ),
            in.good() )
    {
        assert
	    ( strlen ( line ) < sizeof ( line ) - 1 );
	if ( line[0] == '#' ) continue;
	if ( line[0] == 0 ) continue;

	add_word ( line, IGNORED );
    }
    in.close();
    int Dignored =
        D - Dsenset - Dparts - Dpart - Dchecker;
    cerr << "Ignored words file added " << Dignored
         << " dictionary entries." << endl;

    // Process word approvals file.
    //
    int approvals_count = 0;
    in.open ( approvals_file );
    if ( ! in )
    {
	cout << "Cannot open " << approvals_file
	     << endl;
	exit ( 1 );
    }
    while ( in.getline ( line, sizeof ( line ) ),
            in.good() )
    {
        assert
	    ( strlen ( line ) < sizeof ( line ) - 1 );
	if ( line[0] == '#' ) continue;
	if ( line[0] == 0 ) continue;

	char * p = line;
	while ( * p && ! isspace ( * p ) ) ++ p;
	* p ++ = 0;
	long approvals = strtol ( p, & p, 10 );
	assert ( * p == 0 );
	set_approvals ( line, approvals );
	++ approvals_count;
    }
    in.close();
    cerr << "Approvals set for " << approvals_count
         << " dictionary entries." << endl;

    // Process input.
    //
    int position = 0;
    int included_count = 0;
    ignored_count = 0;
    int unapproved_count = 0;
    int no_parts_count = 0;
    while ( cin.getline ( line, sizeof ( line ) ),
            cin.good() && included_count < size )
    {
        assert
	    ( strlen ( line ) < sizeof ( line ) - 1 );

	string word = line;
	++ position;

	hashp hp = hashtable.find ( word );
	if ( hp == hashtable.end() )
	{
	    if ( output_no_parts_words )
	        cout << word << endl;
	    ++ no_parts_count;
	    continue;
	}
	entry & e = * hp->second;
	if ( e.include )
	    cerr << "ERROR: `" << word
		 << "' input more than once."
		 << endl;
	bool approved =
	    ( e.approvals >= required_approvals );

	if ( e.parts & IGNORED )
	    ++ ignored_count;
	else if ( approved )
	{
	    ++ included_count;
	    e.include = true;
	}
	else
	    ++ unapproved_count;

	if ( debug ) output ( e );

    }
    in.close();
    cerr << "Included " << included_count << " out of "
         << position << " input words."
	 << endl;
    cerr << "Excluded " << ignored_count
         << " words listed in ignored-words.txt file."
	 << endl;
    cerr << "Excluded " << no_parts_count
         << " additional words"
            " whose part of speech could not be"
	    " determined." << endl;
    cerr << "Excluded " << unapproved_count
         << " additional words with too low approval"
	    " count."
	 << endl;

    if ( output_no_parts_words || debug )
        return 0;

    sort ( dictionary, dictionary + D );

    int count = 0;
    for ( int i = 0; i < D; ++ i )
    {
        entry & e = dictionary[i];
	if ( ! e.include ) break;

	output ( e );
	++ count;
    }
    cerr << "Output " << count << " words." << endl;

    // Return from main function without error.

    return 0;
}
