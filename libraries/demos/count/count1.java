import java.io.*;

public class count {

    public static void main (String[] args) throws IOException {

	int paragraph = 1;
	boolean eof_seen = false;

	while ( ! eof_seen )
	{
	    int characters = 0;
	    int words = 0;
	    int lines = 0;

	    while ( true )
	    {
		int buffer [] = new int [ 100 ];
		int len = 0;

		while ( true )
		{
		    int c = System.in.read ();
		    if ( c == '\n' ) break;
		    else if ( c == -1 )
		    {
			eof_seen = true;
			break;
		    }
		    else
			buffer [len ++] = c;
		}

		int cp = 0;
		while ( cp < len && buffer [cp] == ' ' ) ++ cp;

		if ( cp == len ) break;

		++ lines;

		do
		{
		    ++ words;
		    while ( cp < len && buffer [cp] != ' ' ) ++ cp;
		    while ( cp < len && buffer [cp] == ' ' ) ++ cp;
		} while ( cp != len );

		characters += cp;
	    }

	    if ( lines > 0  )
	    {
		System.out.print ( "Paragraph " );
		System.out.print ( paragraph );
		System.out.print ( ": " );
		System.out.print ( lines );
		System.out.print ( " lines, " );
		System.out.print ( words );
		System.out.print ( " words, " );
		System.out.print ( characters );
		System.out.print ( " characters." );
		System.out.println ();

		++ paragraph;
	    }
	}
    }
}
