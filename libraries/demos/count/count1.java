// Count Demo Program: JAVA Version
//
// File:     count1.java [Rename to count.java]
// Author:   Bob Walton <walton@deas.harvard.edu>
// Date:     Fri Nov  1 06:37:28 EST 2002
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.
//
// RCS Info (may not be true date or author):
//
//   $Author: hc3 $
//   $Date: 2006/05/04 13:53:54 $
//   $RCSfile: count1.java,v $
//   $Revision: 1.6 $

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
