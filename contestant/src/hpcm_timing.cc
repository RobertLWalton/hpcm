// Compute Primitive Operation Timing Information 
//
// File:	hpcm_timing.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Thu Aug 17 11:07:37 EDT 2017
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.

#include <iostream>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include <cassert>
using std::cout;
using std::endl;
using std::cin;

bool debug = false;
# define dout if ( debug ) cerr

const char * const documentation = "\n"
"hpcm_timing repetitions action\n"
"\n"
"    Execute the given number of repetitions of the\n"
"    given action in a tight loop.  The time taken\n"
"    by this, measured independently by time(1),\n"
"    can then be divided by the repetitions to\n"
"    estimate the time of the action.\n"
"\n"
"    The possible actions are:\n"
"\n"
"        iter            10 inline instructions\n"
"\n"
"        cache           1 cache memory reference\n"
"\n"
"        miss            1 cache miss\n"
"\n"
"        trig            1 call to a trig function\n"
"\n"
"   A meaningless number is printed which is the\n"
"   result of a computation performed by the\n"
"   repeated actions and is necessary to prevent the\n"
"   optimizer from optimizing away the action code.\n";

unsigned buffer[1<<28];
    // 1 gigabyte buffer

unsigned PRIME_2M = 1999993;
    // Largest prime < 2 million.
unsigned PRIME_1G = 999999937;
    // Largest prime < 1 billion.

// Main program.
//
int main ( int argc, char ** argv )
{
    assert ( sizeof ( buffer ) == ( 1 << 30 ) );

    if ( argc < 3 )
    {
	if (    argc == 2
	     && strncmp ( argv[1], "-doc", 4 ) == 0 )
	    cout << documentation;
	else
	    cout << "ERROR: wrong number of argumments"
	         << endl;
	return 1;
    }

    char * p;
    long repetitions = strtol ( argv[1], & p, 10 );
    if ( * p != 0 || repetitions <= 0 )
    {
        cout << "ERROR: first argument not integer > 0"
	     << endl;
	return 1;
    }

    if ( strcmp ( argv[2], "iter" ) == 0 )
    {
        unsigned count = 453849;
	for ( int r = 0; r < repetitions; ++ r )
	{
	    count += 242632;
	    count ^= 8593672;
	    count -= 58367;
	    count ^= 9605145;
	    count += 325342;
	    count ^= 537489;
	    count -= 249484;
	    count ^= 352625467;
	    count += 463784987;
	    count ^= 0xFFFF0000;
	}
	cout << count << endl;
    }
    else if ( strcmp ( argv[2], "cache" ) == 0 )
    {
        unsigned count = 648576;
	for ( int i = 0; i < ( 1 << 19 ); ++ i )
	{
	    buffer[i] = count;
	    count >>= 1;
	    count *= 16807;
	}
	count = 547281909;
	for ( int r = 0; r < repetitions/10; ++ r )
	{
	    count += buffer[count&0x7FFFF] << 20;
	    count ^= buffer[count&0x7FFFF] >> 10;
	    count -= buffer[count&0x7FFFF] <<  8;
	    count += buffer[count&0x7FFFF] << 23;
	    count ^= buffer[count&0x7FFFF] >> 15;
	    count -= buffer[count&0x7FFFF] <<  9;
	    count += buffer[count&0x7FFFF] >>  2;
	    count ^= buffer[count&0x7FFFF] << 16;
	    count -= buffer[count&0x7FFFF] << 21;
	    count += buffer[count&0x7FFFF] >> 13;
	}
	cout << count << endl;
    }
    else if ( strcmp ( argv[2], "miss" ) == 0 )
    {
        unsigned count = 648576;
	for ( int i = 0; i < ( 1 << 26 ); ++ i )
	{
	    buffer[i] = count;
	    count >>= 1;
	    count *= 16807;
	}
	count = 547281909;
	for ( int r = 0; r < repetitions/10; ++ r )
	{
	    count += buffer[count&0xFFFFFFF] << 20;
	    count ^= buffer[count&0xFFFFFFF] >> 10;
	    count -= buffer[count&0xFFFFFFF] <<  8;
	    count += buffer[count&0xFFFFFFF] << 23;
	    count ^= buffer[count&0xFFFFFFF] >> 15;
	    count -= buffer[count&0xFFFFFFF] <<  9;
	    count += buffer[count&0xFFFFFFF] >>  2;
	    count ^= buffer[count&0xFFFFFFF] << 16;
	    count -= buffer[count&0xFFFFFFF] << 21;
	    count += buffer[count&0xFFFFFFF] >> 13;
	}
	cout << count << endl;
    }
    else if ( strcmp ( argv[2], "trig" ) == 0 )
    {
	double count = 0.78239240;
	for ( int r = 0; r < repetitions/10; ++ r )
	{
	    count = sin ( count ) + 0.43543897;
	    count = cos ( count ) + 0.75837361;
	    count = sin ( count ) + 0.38293725;
	    count = cos ( count ) + 0.65746534;
	    count = sin ( count ) + 0.20784673;
	    count = cos ( count ) + 0.39746278;
	    count = sin ( count ) + 0.20178907;
	    count = cos ( count ) + 0.29251089;
	    count = sin ( count ) + 0.29503876;
	    count = cos ( count ) + 0.19267389;
	}
	cout << count << endl;
    }
    else
	cout << "ERROR: cannot understand: `"
	     << argv[2] << "'" << endl;



    // Return from main function without error.

    return 0;
}

