#include <iostream.h>

char documentation [] =
"scorediff output_file test_file\n"
"\n"
"    Returns on a single line a list of the types of\n"
"    differences between the two files.  The types of\n"
"    differences are:\n"
"\n"
"    none	There were no differences.\n"
"    spacebreak	One file had whitespace in a line\n"
"		where there the other file did not.\n"
"    linebreak	One file had a line break where the\n"
"		other file did not.\n"
"    whitespace	Both files had whitespace in the same\n"
"		place, but there was differing amounts\n"
"		of whitespace.\n"
"    number D	Two files had a number in the same\n"
"		place, but the numbers were not repre-\n"
"		sented by the same character string,\n"
"		and the maximum absolute value of the\n"
"		difference of all such number pairs\n"
"		was D.\n"
"    nonblank	There were different characters at\n"
"		some place in the file other than\n"
"		those in whitespace or numbers.\n"
"\n"
"    The files are parsed into whitespace, numbers,\n"
"    and other characters.  A number begins with an\n"
"    optional sign followed by an optional decimal\n"
"    point followed by a digit.  A number is scanned\n"
"    by the strtod(3) function.\n" ;

struct file
{
    FILE stream;
    int linebreaks;

    char buffer [ 4000 ];
    char * end;

    char backup [ 5 ];
    char * back;

    double number;
    bool isnumber;
};

int open ( & file f, char * filename )
{
    f.stream = fopen ( filename, "r" );

    if ( f.steam == 0 ) {
        cout << "ERROR: " << strerror ( errno ) << eol;
	cout << "       on opening " << filename;
    	exit ( 1 );
    }

    back = backup;
    * back = 0;
}

inline int getc ( file & f )
{
    int c = * back;

    if ( c != 0 )
    {
        ++ back;
        return c;
    }
    else
        return fgetc ( f.stream );
}

int scanspace ( file & f, int c )
{
    f.linebreaks = 0;
    while ( isspace ( c ) )
    {
        if ( c == '\n' ) ++ f.linebreaks;
	c = getc ( f );
    }
    return c;
}

int scannumber ( file & f, int c )
{
    f.end = f.buffer;
    * f.end ++ = c;

    bool found_point = ( c == '.' );
    bool found_digit = isdigit ( c );

    while (1) {
        c = getc ( f );
	if ( c == '.' )
	{
	    if ( found_point ) 
		break;
	    else found_point = true;
	}
	else if ( isdigit ( c ) )
	    found_digit = true;
	else break;

	* f.end ++ = c;
    }

    if ( ! found_digit )
    {
    	* f.end = 0;
	f.isnumber = false;

	assert ( * back == 0 );
	strcpy ( backup, buffer );
	back = backup;

	return c;
    }

    if ( c == 'e' || c == 'E' )
    {
    	char * endsave = end;

	* f.end ++ = c;
	c = getc ( f );
	if ( c == '+' || c == '-' )
	{
	    * f.end ++ = c;
	    c = getc ( f );
	}
	if ( isdigit ( c ) )
	{
	    while ( isdigit ( c ) )
	    {
		* f.end ++ = c;
		c = getc ( f );
	    }
	}
	else
	{
	    assert ( * back == 0 );

	    * f.end = 0;
	    strcpy ( backup, endsave );
	    f.end = endsave;
	    * endsave = 0;
	    back = backup;
	}
    }

    f.isnumber = true;

    char * e;
    f.number = strtod ( f.buffer, & e )

    assert ( e == f.end );

    return c;
}

int main ( int argc, char ** argv )
{
    int c1, c2;

    file file1;
    file file2;

    if ( argc != 3 )
    {
        cout << document;
	exit (1);
    }

    file1.open ( argv[1] );
    file2.open ( argv[2] );

    bool spacebreak	= false;
    bool linebreak	= false;
    bool whitespace	= false;
    bool number		= false;
    bool nonblank	= false;
    double number_diff	= 0.0;

    while (1)
    {
        if ( c1 == EOF )
	{
	    if ( c2 == EOF ) break;

	    if ( isspace ( c2 ) )
	    {
		spacebreak = true;
		c2 = scanspace ( file2, c2 );
		if ( break2 > 0 ) linebreak = true;

		if ( c2 == EOF ) break;
	    }

	    nonblank = true;

	    break;
     	}
        else if ( isspace ( c1 ) )
	{
	    if ( c2 == c1 )
	    {
	    	do {
		    c1 = getc ( file1 );
		    c2 = getc ( file2 );
		} while ( c1 == c2 && isspace ( c1 ) );

		if ( isspace ( c1 ) )
		{
		    if ( isspace ( c2 ) ) continue;

		    whitespace = true;
		    c1 = scanspace ( file1, c1 );
		    if ( break1 > 0 ) linebreak = true;

		} else if ( isspace ( c2 ) )
		{
		    whitespace = true;
		    c2 = scanspace ( file2, c2 );
		    if ( break2 > 0 ) linebreak = true;
		}
	    }
	    else if ( isspace ( c2 ) )
	    {
	    	whitespace = true;

	        c1 = scanspace ( file1, c1 );
	        c2 = scanspace ( file2, c2 );

		if ( break1 != break2 )
		    linebreak = true;
	    }
	    else
	    {
	        spacebreak = true;
		c1 = scanspace ( file1, c1 );
		if ( break1 > 0 ) linebreak = true;
	    }
     	}
	else if  (c1 == c2 )
	{
	    if ( c1 == '.' )
	    {
	    	c1 = gets ( file1.stream );
		c2 = gets ( file2.stream );

		if ( isdigit ( c1 )  && isdigit ( c2 ) )
		{
		    c1 = scannumber ( file1, c1 );
		    c2 = scannumber ( file2, c2 );
		    if ( strcmp ( file1.buffer,
		                  file2.buffer )
			 != 0 )
		    {
			number = true;
			double diffn = abs
			    ( file1.number
			      - file2.number );
			if ( diffn > number_diff )
			    number_diff = diffn;
		    }
		}
	    }
	    else if ( isdigit ( c1 ) )
	    {
		c1 = scannumber ( file1, c1 );
		c2 = scannumber ( file2, c1 );
		if ( strcmp ( file1.buffer,
			      file2.buffer )
		     != 0 )
		{
		    number = true;
		    double diffn = abs
			( file1.number - file2.number );
		    if ( diffn > number_diff )
			number_diff = diffn;
		}
	    }
	    else
	    {
	    	c1 = getc ( file1 );
		c2 = getc ( file2 );
	    }
	}
        else if ( isspace ( c2 ) )
	{
	    spacebreak = true;
	    c2 = scanspace ( file2, c2 );
	    if ( break2 > 0 ) linebreak = true;
     	}
	else if ( ( c1 = scannumber ( file1, c1 ),
		    c2 = scannumber ( file2, c2 ),
		    file1.isnumber && file2.isnumber ) )
	{
	    number = true;
	    double diffn = abs
		( file1.number - file2.number );
	    if ( diffn > number_diff )
		number_diff = diffn;
	}
        else
	{
	    nonblank = true;
	    break;
     	}
    }
}
