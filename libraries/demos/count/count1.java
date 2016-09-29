// Count Demo Program: JAVA Version
//
// File:	count.java [After renaming]
// Actual-File:	count1.java [Before renaming]
// Author:	Bob Walton <walton@deas.harvard.edu>
// Date:	Thu May  4 10:07:11 EDT 2006
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.

import java.io.*;
import java.util.StringTokenizer;

public class count {

    public static boolean debug;

    public static void dprintln ( String s )
    {
        if ( debug ) System.out.println ( s );
    }

    public static void main (String[] args)
	    throws IOException
    {

        debug = ( args.length > 0 );

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

		dprintln ( "+ " + line );
		dprintln ( ". " + characters +
		           " " + words +
			   " " + lines );
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
