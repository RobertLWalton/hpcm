#include <stdio.h>

#define dprintf if ( debug ) printf
int debug;

main ( int argc )
{
    debug = ( argc > 1 );

    int paragraph = 1;

    while ( 1 )
    {
	int characters = 0;
	int words = 0;
	int lines = 0;

	char buffer [102];

	int at_end_of_file = 1;

	while ( fgets ( buffer, sizeof ( buffer),
	                stdin ) )
	{
	    char * cp = buffer;

	    at_end_of_file = 0;

	    while ( * cp == ' ' ) ++ cp;

	    if ( * cp == 0 || * cp == '\n' ) break;

	    ++ lines;

	    do
	    {
		++ words;
		while ( * cp != ' ' &&
		        * cp != '\n' &&
			* cp != 0 ) ++ cp;
		while ( * cp == ' ' ) ++ cp;
	    } while ( * cp != 0 && * cp != '\n' );

	    characters += ( cp - buffer );
	    dprintf ( "+ %s", buffer );
	    dprintf ( ". %d %d %d\n",
	              characters, words, lines );
	}

	if ( at_end_of_file ) break;

	if ( lines > 0  )
	{
	    printf ( "Paragraph %d: %d lines, %d words,"
	             " %d characters.\n", paragraph,
		     lines, words, characters );

	    ++ paragraph;
	}
    }

    return 1;   /* This line can be omitted.
		 * It is a test that make count.out
		 * works even if count returns an
		 * error code.
		 */
}
