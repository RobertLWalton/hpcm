/* Program to check file for overflowing max line
** and page length.
**
** Author:	Bob Walton (walton@deas.harvard.edu)
** File:	chkpage.c
** Version:	1
**
** The authors have placed this program in the public
** domain; they make no warranty and accept no liability
** for this program.
*/

#include <stdio.h>

char documentation [] =
"chkpage [-cCC] [-lLL] [filename ...]\n"
"\n"
"    Checks that files contain lines with no more than"
		" CC\n"
"    columns and pages with no more than LL lines.\n"
"\n"
"    If no filenames are given, the standard input"
		" is\n"
"    checked.\n"
"\n"
"    CC defaults to 80 colums and LL to 58 lines.\n"
"\n"
"    Too long lines and lines overflowing a page are\n"
"    output.  Nothing is output if the files are all\n"
"    OK.\n";

void checkfile
    ( FILE * in, char * name,
      int maxcolumn, int maxline )
{
    int line = 1;
    int line_in_page = 0;

    char buffer [257];

    int column = 0;
    while ( fgets ( buffer, 257, in ) )
    {
	char * cp = buffer;
	char c = * cp ++;

	while ( c != 0 && c != '\n' )
	{
	    if ( c == '\t' )
		column += 8 - ( column % 8 );
	    else if ( c == '\f' )
		line_in_page = 0;
	    else ++ column;
	    c = * cp ++;
	}

	if ( column > maxcolumn )
	{
	    printf ( "LINE TOO LONG: " );
	    if ( name ) printf ( "%s: ", name );
	    printf ( "%d: %s", line, buffer );
	    if ( c != '\n' ) printf ( "\n" );
	}

	if ( c == '\n' )
	{
	    ++ line_in_page;

	    if ( line_in_page > maxline )
	    {
		printf ( "PAGE TOO LONG: " );
		if ( name ) printf ( "%s: ", name );
		printf ( "%d: %s", line, buffer );
	    }

	    ++ line;
	    column = 0;
	}
    }
}



int main ( int argc, char ** argv )
{
    int maxcolumn = 80;
    int maxline = 58;

    while ( argc >= 2 && argv[1][0] == '-' )
    {
	if ( argv[1][1] == 'c' )
	{
	    maxcolumn = atoi (argv[1] + 2);
	    if ( maxcolumn == 0 )
	    {
		printf ( "chkpage: bad argument %s\n",
			 argv[1] );
		exit (1);
	    }
	}
	else if ( argv[1][1] == 'l' )
	{
	    maxline = atoi (argv[1] + 2);
	    if ( maxline == 0 )
	    {
		printf ( "chkpage: bad argument %s\n",
			 argv[1] );
		exit (1);
	    }
	}
	else
	{
	    printf ( documentation );
	    exit (1);
	}

	++ argv; -- argc;
    }

    if ( argc <= 1 )
	checkfile ( stdin, NULL, maxcolumn, maxline );
    else while ( argc >= 2 )
    {
	FILE * in = fopen ( argv[1], "r" );
	if ( in != NULL )
	    checkfile
		( in, argv[1], maxcolumn, maxline );
	else printf ( "%s: cannot open\n", argv [1] );
	-- argc; ++ argv;
    }
    exit (0);
}
