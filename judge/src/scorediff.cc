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

int main ( int argc, char ** argv )
{
    char * not_eof1, * not_eof2;

    char * p1, * p2;

    char c1, c2;

    not_eof1 = fgets ( file1, buffer1, BUFFER_SIZE );
    p1 = buffer1;

    not_eof2 = fgets ( file2, buffer2, BUFFER_SIZE );

    while (1)
    {
        if ( c1 == EOF )
	{
	    if ( c2 == EOF ) break;

	    if ( isspace ( c2 ) )
	    {
		spacebreak = true;
		c2 = scanspace ( file2, break2 );
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
		    c1 = fgetc ( file1 );
		    c2 = fgetc ( file2 );
		} while ( c1 == c2 && isspace ( c1 ) );

		if ( isspace ( c1 ) )
		{
		    if ( isspace ( c2 ) ) continue;

		    whitespace = true;
		    c1 = scanspace ( file1, break1 );
		    if ( break1 > 0 ) linebreak = true;

		} else if ( isspace ( c2 ) )
		{
		    whitespace = true;
		    c2 = scanspace ( file2, break2 );
		    if ( break2 > 0 ) linebreak = true;
		}
	    }
	    else if ( isspace ( c2 ) )
	    {
	    	whitespace = true;

	        c1 = scanspace ( file1, break1 );
	        c2 = scanspace ( file2, break2 );

		if ( break1 != break2 )
		    linebreak = true;
	    }
	    else
	    {
	        spacebreak = true;
		c1 = scanspace ( file1, break1 );
		if ( break1 > 0 ) linebreak = true;
	    }
     	}
	else if  (c1 == c2 )
	{
	}
        else if ( c2 == EOF )
	{
	    if ( isspace ( c1 ) )
	    {
		spacebreak = true;
		c1 = scanspace ( file1, break1 );
		if ( break1 > 0 ) linebreak = true;

		if ( c1 == EOF ) break;
	    }

	    nonblank = true;

	    break;
     	}
        else if ( isspace ( c2 ) )
	{
	    spacebreak = true;
	    c2 = scanspace ( file2, break2 );
	    if ( break2 > 0 ) linebreak = true;
     	}
    }
}
