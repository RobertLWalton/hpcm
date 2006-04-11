#include <iostream>
using namespace std;

#define dout if ( debug ) cout
bool debug;

main( int argc )
{
    debug = ( argc > 1 );

    int paragraph = 1;

    while ( ! cin.eof() )
    {
	int characters = 0;
	int words = 0;
	int lines = 0;

	char buffer [101];

	while
	  ( cin.getline ( buffer, sizeof ( buffer ) ),
	    ! cin.eof() )
	{
	    char * cp = buffer;
	    while ( * cp == ' ' ) ++ cp;

	    if ( * cp == 0 ) break;

	    ++ lines;

	    do
	    {
		++ words;
		while ( * cp != ' ' && * cp ) ++ cp;
		while ( * cp == ' ' ) ++ cp;
	    } while ( * cp );

	    characters += ( cp - buffer );
	    dout << "+ " << buffer << endl;
	    dout << ". " << characters
	         << " " << words
		 << " " << lines << endl;
	}

	if ( lines > 0  )
	{
	    cout << "Paragraph " << paragraph << ": "
		 << lines << " lines, "
		 << words << " words, "
		 << characters << " characters."
		 << endl;

	    ++ paragraph;
	}
    }

    return 1;   // This line can be omitted.
		// It is a test that make count.out
		// works even if count returns an
		// error code.
}
