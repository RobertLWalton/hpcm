// Program to Make Dictionaries
//
// File:     make_dictionary.cc
// Authors:  Bob Walton <walton@seas.harvard.edu>
// Date:     Tue Aug 29 17:13:48 EDT 2017
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
const char * senset_file = "index.sense";
const char * frequency_file = "words-by-frequency.txt";
const char * part_file = "part-of-speech.txt";

const char * const documentation = "\n"
"make_dictionary [-a] size\n"
"\n"
"    Output a dictionary whose entries are words and\n"
"    their parts of speech.  The most frequently used\n"
"    `size' words are included.  The lines have the\n"
"    format:\n"
"\n"
"        word part-of-speech\n"
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
"    The -a option outputs the `abbreviation' for the\n"
"    part of speech; otherwise the `code' is output.\n"
"\n"
"    The words are output in alphabetical order.\n"
"\n"
"    A separate line is output for each part of\n"
"    speech that a word has, if the word has more\n"
"    than one.  Word frequency is computed without\n"
"    regard to part of speech.\n"
"\n"
"    The input files are:\n"
"\n"
"        index.sense\n"
"            Word Net (Princeton) index.sense file.\n"
"            Determines which words are nouns, verbs,\n"
"            adjectives, or adverbs.\n"
"\n"
"        words-by-frequency.txt\n"
"            Word frequency list derived by from the\n"
"            Google Web Trillion Word Corpus by Peter\n"
"            Norvig and filtered to remove word\n"
"            frequency counts and words beyond the\n"
"            first several tens of thousands.\n"
"\n"
"        part-of-speech.txt\n"
"            Part of speech information not in Word\n"
"            Net files.  In format:\n"
"\n"
"                word part-of-speech-abbreviation\n"
"\n"
"            with comment lines beginning with #.\n"
"\n"
;

const int MAX_CODE = 13;
const char * abbreviation[MAX_CODE+1] = {
    NULL,	// 0
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
    NOUN = 1 << 1,
    VERB = 1 << 2
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

struct entry  // dictionary entry
{
    string word;
    int parts;
        // Bit 1 << c is set is parts if the word has
	// the part of speech with code c.
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

bool output_abbreviations = false;
long size;

// Include a word if it is in the dictionary and
// has one of the parts of speech indicated.  Return
// true if successful and false if word not found or
// did not have one of the parts of speech indicated.
// Increment included_count if true returned.
//
int included_count = 0;
bool include ( string word, int parts = -1 )
{
    hashp hp = hashtable.find ( word );
    if ( hp == hashtable.end() ) return false;
    entry & e = * hp->second;
    if ( ( e.parts & parts ) == 0 ) return false;
    e.include = true;
    ++ included_count;
    return true;
}

// Main program.
//
int main ( int argc, char ** argv )
{

    // Process options.

    if (    argc >= 2
         && strcmp( argv[1], "-a" ) == 0 )
    {
        output_abbreviations = true;
	-- argc, ++ argv;
    }

    if ( argc != 2 || argv[1][0] == '-' )
    {

	// Any unrecognized -* option prints documenta-
	// tion and exits with error status.
	//
	cout << documentation;
	exit (1);
    }

    char * p;
    size = strtol ( argv[1], & p, 10 );
    if ( size < 0 || * p )
    {
        cerr << "ERROR: bad size argument."
	     << endl;
	exit ( 1 );
    }

    ifstream in;

    // Process senset file.
    //
    in.open ( senset_file );
    if ( ! in )
    {
	cout << "Cannot open " << senset_file << endl;
	exit ( 1 );
    }
    char line[5000];
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

	string word = line;
	hashp hp = hashtable.find ( word );
	if ( hp == hashtable.end() )
	{
	    entry & e = dictionary[D++];
	    e.word = word;
	    e.parts = 1 << c;
	    e.include = false;
	    hashtable.emplace ( word, & e );
	}
	else
	{
	    entry & e = * hp->second;
	    e.parts |= 1 << c;
	}
    }
    in.close();
    int Dsenset = D;
    cerr << "Senset file added " << Dsenset
         << " dictionary entries." << endl;

    // Process part-of-speech file.
    //
    in.open ( part_file );
    if ( ! in )
    {
	cout << "Cannot open " << part_file << endl;
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
	int i = * p - '0';
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
		 << line << "'" << endl;
	    continue;
	}

	string word = line;
	hashp hp = hashtable.find ( word );
	if ( hp == hashtable.end() )
	{
	    entry & e = dictionary[D++];
	    e.word = word;
	    e.parts = 1 << c;
	    e.include = false;
	    hashtable.emplace ( word, & e );
	}
	else
	{
	    entry & e = * hp->second;
	    e.parts |= 1 << c;
	}
    }
    in.close();
    int Dpart = D - Dsenset;
    cerr << "Part of speech file added " << Dpart
         << " dictionary entries." << endl;

    // Process word frequency file.
    //
    in.open ( frequency_file );
    if ( ! in )
    {
	cout << "Cannot open " << frequency_file << endl;
	exit ( 1 );
    }
    int position = 0;
    included_count = 0;
    while ( in.getline ( line, sizeof ( line ) ),
            in.good() && position < size )
    {
        assert
	    ( strlen ( line ) < sizeof ( line ) - 1 );

	string word = line;
	++ position;

	if ( include ( word ) ) continue;
	int wsize = word.size();
	if ( wsize >= 4 && word[wsize-1] == 's' )
	{
	    string word2 = word.substr ( 0, wsize - 1 );
	    if ( include ( word2, NOUN | VERB ) )
	        continue;
	}
	if (    wsize >= 5
	     && word.substr ( wsize - 2, 2 ) == "ed" )
	{
	    string word2 =
	        word.substr ( 0, wsize - 2 );
	    if ( include ( word2, VERB ) )
	        continue;
	    word2 += "e";
	    if ( include ( word2, VERB ) )
	        continue;
	}
	if (    wsize >= 5
	     && word.substr ( wsize - 2, 2 ) == "es" )
	{
	    string word2 =
	        word.substr ( 0, wsize - 2 );
	    if ( include ( word2, NOUN ) )
	        continue;
	}
	if (    wsize >= 5
	     && word.substr ( wsize - 3, 3 ) == "ies" )
	{
	    string word2 =
	        word.substr ( 0, wsize - 3 ) + "y";
	    if ( include ( word2, NOUN ) )
	        continue;
	}
	if (    wsize >= 5
	     && word.substr ( wsize - 3, 3 ) == "ied" )
	{
	    string word2 =
	        word.substr ( 0, wsize - 3 ) + "y";
	    if ( include ( word2, VERB ) )
	        continue;
	}
	if (    wsize >= 5
	     && word.substr ( wsize - 3, 3 ) == "ing" )
	{
	    string word2 = word.substr ( 0, wsize - 3 );
	    if ( include ( word2, VERB ) )
	        continue;
	    word2 += "e";
	    if ( include ( word2, VERB ) )
	        continue;
	}
	cerr << "ERROR: could find no parts of"
		" speech for `" << word
	     << "' of position " << position
	     << endl;
    }
    in.close();
    cerr << "Found " << included_count << " out of "
         << position << " frequency file words."
	 << endl;

    sort ( dictionary, dictionary + D );

    int count = 0;
    for ( int i = 0; i < D; ++ i )
    {
        entry & e = dictionary[i];
	if ( ! e.include ) break;

	string word = e.word;
	unsigned wsize = word.size();
	if ( wsize == 1 )
	{
	    if ( word == "i" ) word = "I";
	    else if ( word != "a" ) continue;
	}

	++ count;
	for ( int c = 1; c <= MAX_CODE; ++ c )
	{
	    if ( ( 1 << c ) & e.parts )
	        cout << word << " "
		     << abbreviation[c]
		     << endl;
	}
    }
    cerr << "Output " << count << " words." << endl;

    // Return from main function without error.

    return 0;
}
