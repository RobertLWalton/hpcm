import java.io.*;
import java.util.StringTokenizer;

public class count {

    public static void main (String[] args)
	    throws IOException
    {

	BufferedReader reader
	   = new BufferedReader
	         ( new InputStreamReader
		       ( System.in ) );

	// Loop through paragraphs.
	//
	int paragraph = 1;
	boolean eof_seen = false;
	while ( ! eof_seen )
	{
	    int characters = 0;
	    int words = 0;
	    int lines = 0;

	    while ( true )
	    {
		String line = reader.readLine();
		if ( line == null )
		{
		    // readLine returns null on EOF.
		    //
		    eof_seen = true;
		    break;
		}

		StringTokenizer tokenizer
		    = new StringTokenizer ( line );

		// Break on blank line.
		//
		if ( ! tokenizer.hasMoreTokens() )
		    break;

		++ lines;

		// Count words in line.
		//
		while ( tokenizer.hasMoreTokens() )
		{
		    ++ words;
		    tokenizer.nextToken();
		}

		// Count characters in line.
		//
		characters += line.length();
	    }

	    // Ignore blank `paragraphs'.
	    //
	    if ( lines > 0  )
	    {
		// Print paragraph output.
		//
		System.out.println
		    (   "Paragraph " + paragraph + ": "
		      + lines + " lines, "
		      + words + " words, "
		      + characters + " characters."
		    );

		++ paragraph;
	    }
	}
    }
}
