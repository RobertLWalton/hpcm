import java.io.*;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Locale;

// This program reads input, parses it into tokens,
// prints info about the tokens, and prints a summary
// at the end.  The program illustrates use of the
// StreamTokenizer and DecimalFormat classes.

public class javaio {

    public static void main (String[] args)
	    throws IOException {

	// Set up the StreamTokenizer.
	//
	Reader reader
	    = new BufferedReader
		  ( new InputStreamReader
		        ( System.in ) );
	StreamTokenizer tokenizer
	    = new StreamTokenizer ( reader );

	// Set to read any string of non-whitespace
	// characters as a word.
	//
	tokenizer.resetSyntax();
	tokenizer.wordChars ( ' ', '\u00FF' );
	tokenizer.whitespaceChars ( '\u0000', ' ' );
	//
	// The above code is a bit tricky in that is
	// first sets ' ' to a wordChar and then to
	// a whitespaceChar.

	// Set to read end of line as a token.
	// If this function is not called, end of
	// line is treated as a simple space character.
	//
	tokenizer.eolIsSignificant ( true );

	// Read numbers as tokens.  If not called,
	// numbers are not handled specially.
	//
	// WARNING: This makes isolated '.'s input as
	// the the number 0, while `-'s may input as
	// a separator.
	//
	tokenizer.parseNumbers();

	// Parse certain characters as 1-character
	// tokens.
	//
	tokenizer.ordinaryChar ( ',' );
	tokenizer.ordinaryChar ( '(' );
	tokenizer.ordinaryChar ( ')' );

	// Set up number formatter.  Not that it is
	// important in ACM programming contests to
	// insist on an ENGLISH formatter.
	//
	DecimalFormat formatter = (DecimalFormat)
	    NumberFormat.getInstance ( Locale.ENGLISH );
	formatter.applyPattern ( "#0.00" );


	// In ACM programming contests, you do NOT
	// (usually) want commas in the thousands
	// place.
	//
	// formatter.setGroupingUsed ( false);

	// Set digit limits.  Some of these are not
	// used in this program.
	//
	// formatter.setMinimumFractionDigits ( 2 );
	// formatter.setMinimumIntegerDigits ( ... );
	// formatter.setMaximumIntegerDigits ( ... );

	// Process a paragraph.  Paragraphs are
	// separated by blank lines.
	//
	int paragraph = 1;
	boolean eof_found = false;
	while ( ! eof_found )
	{
	    int numbers = 0;
	    int words = 0;
	    int separators = 0;
	    int lines = 0;

	    boolean eop_found = false;
	    boolean line_is_blank = true;

	    while ( ! eop_found && ! eof_found )
	    {
	        tokenizer.nextToken();
	        switch ( tokenizer.ttype )
		{

		case StreamTokenizer.TT_EOF:

		    if ( line_is_blank )
		    {
			eof_found = true;
			break;
		    } else
		        throw new RuntimeException
			    ( "EOF in bad place" );

		case StreamTokenizer.TT_EOL:

		    if ( ! line_is_blank )
		        ++ lines;
		    else if ( lines != 0 )
		        eop_found = true;
		    line_is_blank = true;
		    break;

		case StreamTokenizer.TT_NUMBER:

		    System.out.print ( "NUMBER ");
		    System.out.print ( tokenizer.nval );
		    System.out.println();
		    line_is_blank = false;
		    ++ numbers;
		    break;

		case StreamTokenizer.TT_WORD:
		    System.out.print ( "WORD ");
		    System.out.print ( tokenizer.sval );
		    System.out.println();
		    line_is_blank = false;
		    ++ words;
		    break;

		case '(':
		case ')':
		case ',':
		case '-':
		    System.out.print ( "SEPARATOR ");
		    System.out.print
		        ( (char) tokenizer.ttype );
		    System.out.println();
		    line_is_blank = false;
		    ++ separators;
		    break;

		default:
		    throw new RuntimeException
			( "Bad token type "
			  + tokenizer.ttype );
		}
	    }

	    if ( lines > 0  )
	    {
		System.out.println
		    ( "Paragraph " + paragraph + ":" );

		System.out.println
		    ( "    " + lines + " lines, "
		             + words + " words, "
			     + numbers + " numbers, "
		             + separators
			     + " separators." );

		double m =
		    ( (double) 100.0 )
		    / ( words + numbers + separators );

		System.out.println
		    ( "    "
		      + formatter.format
		            ( m * words )
		      + "% words, "
		      + formatter.format
		            ( m * numbers )
		      + "% numbers, "
		      + formatter.format
		            ( m * separators )
		      + "% separators." );

		++ paragraph;
	    }
	}
    }
}
