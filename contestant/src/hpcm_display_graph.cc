// Display a Graph
//
// File:	hpcm_display_graph.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Thu Jul 21 06:59:26 EDT 2011
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: walton $
//   $Date: 2011/07/21 11:56:16 $
//   $RCSfile: hpcm_display_graph.cc,v $
//   $Revision: 1.1 $

#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>

const char * const documentation = "
hpcm_display_graph [-ps|-X] header \\
                   input-file output-file

    This program displays undirected graphs defined by
    points in the input-file and edges in the output-
    file.

    The format of the input-file is a set of test cases,
    each beginning with a 1-line test case name that is
    followed by lines containing the numbers

    	N x[1] y[1] x[2] y[2] ... x[N] y[N]

    N is the number of points in the graph, while
    (x[i],y[i]) is the i'th point (1 <= i <= N).  N is
    a positive integer and the x[i], y[i] are floating
    point.  Numbers may be separated by any kind and
    amount of whitespace, including single spaces, tabs,
    and line feeds.

    The format of the output-file is a set of test cases,
    each beginning with a 1-line test case name that is
    followed by `edge lines' EACH with the format

    	i j

    indicating there is an undirected edge between the
    point (x[i],y[i]) and the point (x[j],y[j]).  Here
    i and j are integers >= 1.

    In the output-file the test case name lines are
    guarenteed not have a digit as their first non-
    whitespace character, and thus can be distinguished
    from edge lines.

    With the -ps option, postscript is written to the
    standard output.

    With the -X option, an X-window is opened and the
    first test case displayed.  Typing a carriage
    return goes to the next test case, and typing an
    unsigned integer M followed by a carriage return
    goes to the M'th test case.

    Points are displayed as dots, edges as lines.  If
    two edges overlap, the line it thickened.  If there
    is a line from a point to itself, the point dot is
    made larger.
";





// Main program.
//
int main ( int argc, char ** argv )
{

    // Process options.

    while ( argc >= 4 && argv[1][0] == '-' )
    {

	char * name = argv[1] + 1;

        if (    strcmp ( "ps", name ) == 0 )
	{
	}
        else if ( strncmp ( "doc", name, 3 ) == 0 )
	{
	    // Any -doc* option prints documentation
	    // and exits with error status.
	    //
	    cout << documentation;
	    exit (1);
	}

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

    open ( output, argv[1], "out" );
    open ( test, argv[2], "test" );

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
	    // exact or exact but for letter case.

	    char * tp1 = output.token;
	    char * tp2 = test.token;
	    char * endtp2 = tp2 + test.length;
	    bool token_match_but_for_letter_case =
	        ( output.length == test.length );
	    bool token_match =
	        token_match_but_for_letter_case;

	    while ( tp2 < endtp2
	            && token_match_but_for_letter_case )
	    {
		if ( * tp1 != * tp2 )
		{
		    token_match = false;
		    token_match_but_for_letter_case =
			( toupper ( * tp1 )
			  == toupper ( * tp2 ) );
		}
		++ tp1, ++ tp2;
	    }

	    if ( token_match_but_for_letter_case )
	    {
		if ( ! token_match )
		    found_difference
		        ( output.type != NUMBER_TOKEN ?
			      LETTER_CASE :
			  output.is_float ?
			      FLOAT :
			      INTEGER );
	    }

	    else if ( output.type == NUMBER_TOKEN )
	    {
	        // Tokens are not equal with letter case
		// ignored, but both are numbers.

	        assert ( test.type == NUMBER_TOKEN );
		diffnumber ();
	    }

	    else
	    {
	        // Tokens are not equal with letter case
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

    // The file reading loop is done because we have
    // found matched EOF_TOKENS.  Output fake eof-eof
    // difference.
    //
    assert ( output.type == EOF_TOKEN );
    assert ( test.type == EOF_TOKEN );
    found_difference
	( type_mismatch ( EOF_TOKEN, EOF_TOKEN ) );

    // Differences are now recorded in memory, ready for
    // outputting.

    // Produce first output line listing all found
    // differences, regardless of the proofs to be
    // output.  As these are output, their found flags
    // are cleared.  Differences with smaller
    // OGN:OCN-TGN:TCN markers are printed first.

    bool any = false;	// True if anything printed.
    while ( true )
    {
	// Find the lowest OGN,OCN,TGN,TCN 4-tuple among
	// all differences that that remain to be
	// printed.
	//
	int output_group;
	int output_case;
	int test_group;
	int test_case;
	bool found = false;
	for ( int i = 0; i < MAX_DIFFERENCE; ++ i )
	{
	    if ( ! differences[i].found ) continue;

	    if ( ! found )
	    {
		found = true;
		/* Drop through */
	    }
	    else if (   differences[i].output_group
		      > output_group )
		continue;
	    else if (   differences[i].output_group
		      < output_group )
		/* Drop through */;
	    else if (   differences[i].output_case
		      > output_case )
		continue;
	    else if (   differences[i].output_case
		      < output_case )
		/* Drop through */;
	    else if (   differences[i].test_group
		      > test_group )
		continue;
	    else if (   differences[i].test_group
		      < test_group )
		/* Drop through */;
	    else if (   differences[i].test_case
		      > test_case )
		continue;
	    else if (   differences[i].test_case
		      < test_case )
		/* Drop through */;
	    else continue;

	    output_group =
		differences[i].output_group;
	    output_case  =
		differences[i].output_case;
	    test_group   =
		differences[i].test_group;
	    test_case    =
		differences[i].test_case;
	}
	if ( ! found ) break;

	// Print out the OGN:OCN-TGN:TCN marker, and
	// then all the differences for this marker,
	// clearing the found flags of differences
	// printed.
	//
	if ( any ) cout << " ";
	any = true;
	cout << output_group << ":" << output_case
	     << "-"
	     << test_group << ":" << test_case;
	for ( int i = 0; i < MAX_DIFFERENCE; ++ i )
	{
	    if ( ! differences[i].found ) continue;

	    if (    differences[i].output_group
	         != output_group ) continue;
	    if (    differences[i].output_case
	         != output_case ) continue;
	    if (    differences[i].test_group
	         != test_group ) continue;
	    if (    differences[i].test_case
	         != test_case ) continue;

	    differences[i].found = false;

	    cout << " " << differences[i].name;
	    if ( i == FLOAT )
		cout << " " << float_absdiff_maximum
		     << " " << float_reldiff_maximum;
	    else if ( i == INTEGER )
		cout << " " << integer_absdiff_maximum
		     << " " << integer_reldiff_maximum;
	}
    }

    cout << endl;

    // Output proof lines.

    for ( proof_line * pline = first_proof_line;
          pline != NULL;
	  pline = pline->next )
    {
        cout << pline->output_line << " "
             << pline->test_line;

	// Output proofs within a proof line.

	int last_output_column	= -2;
	int last_test_column	= -2;

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
