// Count Demo Program: JAVA Version
//
// File:     count1.java
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
//   $Date: 2002/11/01 11:38:21 $
//   $RCSfile: count1.java,v $
//   $Revision: 1.4 $

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
