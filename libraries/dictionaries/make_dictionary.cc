// Program to Make Dictionaries
//
// File:     make_dictionary.cc
// Authors:  Bob Walton <walton@seas.harvard.edu>
// Date:     Tue Aug 29 05:11:11 EDT 2017
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.

#include <iostream>
#include <fstream>
#include <algorithm>
#include <unordered_map>
#include <cstdlib>
#include <cstring>
#include <cctype>
#include <cassert>
using std::cout;
using std::endl;
using std::cerr;
using std::cin;
using std::ifstream;
using std::unordered_map;

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
"        7    mod             Modal Verb\n"
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
    "n",	// 1
    "v",	// 2
    "adj",	// 3
    "adv",	// 4
    "pron",	// 5
    "rel",	// 6
    "mod",	// 7
    "inf",	// 8
    "det",	// 9
    "cconj",	// 10
    "sconj",	// 11
    "prep",	// 12
    "inter"	// 13
};

const int senset_type[6] = { 0, 1, 2, 3, 4, 3 };
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
    const char * word;
    int parts;
        // Bit 1 << c is set is parts if the word has
	// the part of speech with code c.
    bool include;
        // Include in output; i.e., word is one of
	// the `size' most frequent.
} dictionary[MAX_WORDS];

// Sort included entries before not included entries
// and otherwise sort by word lexical order.
//
bool operator < ( const entry & e1, const entry & e2 )
{
    if ( ! e1.include && e2.include ) return false;
    else if ( e1.include && ! e2.include ) return true;
    else return strcmp ( e1.word, e2.word ) < 0;
}

unordered_map<const char *, entry *> hashtable;
typedef unordered_map<const char *, entry *>::iterator
    hashp;

bool output_abbreviations = false;

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


    // Process senset file.
    //
    ifstream in;
    in.open ( senset_file );
    if ( ! in )
    {
	cout << "Cannot open " << senset_file << endl;
	exit ( 1 );
    }

    // Return from main function without error.

    return 0;
}
