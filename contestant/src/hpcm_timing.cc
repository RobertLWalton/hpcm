// Compute Primitive Operation Timing Information 
//
// File:	hpcm_timing.cc
// Authors:	Bob Walton (walton@deas.harvard.edu)
// Date:	Thu Aug 17 16:23:21 EDT 2017
//
// The authors have placed this program in the public
// domain; they make no warranty and accept no liability
// for this program.

#include <iostream>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <cmath>
#include <cassert>
using std::cout;
using std::endl;
using std::cin;

extern "C" {
# include <unistd.h>
# include <sys/times.h>
}

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

// We embed the random number generator so it cannot
// change on us.  This is the same as lrand48 in 2016.
// Note that for this random number generator the
// low order k bits produced depend only on the low
// order k bits of the seed.  Thus the generator is
// only good at generating floating point numbers in
// the range 0 .. 1.  To get small random numbers, use
// random ( n ) below.  To get large random numbers
// use lrandom ( n ) below.
//
# include <cmath>
# define random RANDOM
# define srandom SRANDOM
    // To avoid conflict with libraries.
unsigned long long last_random_number;
const unsigned long long MAX_RANDOM_NUMBER =
	( 1ull << 32 ) - 1;
void srandom ( unsigned long long seed )
{
    seed &= MAX_RANDOM_NUMBER;
    last_random_number = ( seed << 16 ) + 0x330E;
}
// Return floating point random number in range [0 .. 1)
//
inline double drandom ( void )
{
    last_random_number =
        0x5DEECE66Dull * last_random_number + 0xB;
    unsigned long long v =
          ( last_random_number >> 16 )
	& MAX_RANDOM_NUMBER;
    return (double) v / (MAX_RANDOM_NUMBER + 1 );
}
// Return a random number in the range 0 .. n - 1.
//
inline unsigned long random ( unsigned long n )
{
    return (unsigned long) floor ( drandom() * n );
}
// Return a random number in the range [first,last].
//
inline long random ( long first, long last )
{
    assert ( first <= last );
    return first + random ( last - first + 1 );
}

double seconds_per_tick = -1;

double user_time ( void )
{
    if ( seconds_per_tick < 0 )
    {
        seconds_per_tick = 1.0 / sysconf(_SC_CLK_TCK);
    }

    struct tms t;
    times ( & t );
    return t.tms_utime * seconds_per_tick;
}

const char header[] =
    "  Action   Repetitions  mil/sec  iters";
void print_time
	( const char * name, int repetitions,
	  double time, double iter_time )
{
    double iters = time / iter_time;
    double mps = repetitions / ( time * 1e6 );
    printf ( "%7s%14d%8.1f%9.2f\n",
              name, repetitions, mps, iters );
}

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
    unsigned repetitions = strtol ( argv[1], & p, 10 );
    if ( * p != 0 || repetitions <= 0 )
    {
        cout << "ERROR: first argument not integer > 0"
	     << endl;
	return 1;
    }
    unsigned long long SEED =
	strtoul ( argv[2], & p, 10 );
    if ( * p != 0 || SEED < 1e8 || SEED >= 1e9 )
    {
        cout << "ERROR: SEED not in range" << endl;
	return 1;
    }

    srandom ( SEED );
    for ( int i = 0; i < ( 1 << 28 ); ++ i )
        buffer[i] = random ( 1 << 31 );


    double iter_time;
    {
        unsigned count = 453849;
	double start = user_time();
	for ( int r = 0; r < repetitions; ++ r )
	{
	    count += 647242632;
	    count ^= 859367289;
	    count -= 583676478;
	    count ^= 960514578;
	    count += 325342201;
	    count ^= 537489278;
	    count -= 249484387;
	    count ^= 352625467;
	    count += 463784987;
	    count ^= 356281765;
	}
	iter_time = user_time() - start;
	cout << "ITER " << count << " "
	     << iter_time << " seconds" << endl;
    }
    double cache_time;
    {
	unsigned count = 547281909;
	double start = user_time();
	for ( int r = 0; r < repetitions; ++ r )
	{
	    count += 647242632;
	    count ^= 859367289;
	    count -= 583676478;
	    count ^= 960514578;
	    count += buffer[count&0x7FFFF];
	    count ^= 537489278;
	    count -= 249484387;
	    count ^= 352625467;
	    count += buffer[count&0x7FFFF];
	    count ^= 356281765;
	}
	cache_time = user_time() - start - iter_time;
	cout << "CACHE " << count << " "
	     << cache_time << " + "
	     << iter_time << " seconds" << endl;
	cache_time *= 5;
    }
    double miss_time;
    {
	unsigned count = 547281909;
	double start = user_time();
	for ( int r = 0; r < repetitions; ++ r )
	{
	    count += 647242632;
	    count ^= 859367289;
	    count -= 583676478;
	    count ^= 960514578;
	    count += buffer[count&0xFFFFFFF];
	    count ^= 537489278;
	    count -= 249484387;
	    count ^= 352625467;
	    count += buffer[count&0xFFFFFFF];
	    count ^= 356281765;
	}
	miss_time = user_time() - start - iter_time;
	cout << "MISS " << count << " "
	     << miss_time << " + "
	     << iter_time << " seconds" << endl;
	miss_time *= 5;
    }
    double trig_time;
    {
	double count = 0.78239240;
	double start = user_time();
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
	trig_time = user_time() - start;
	cout << "TRIG " << count << " "
	     << trig_time << " seconds" << endl;
	trig_time *= 10;
    }

    cout << header << endl;
    print_time ( "ITER", repetitions,
                 iter_time, iter_time );
    print_time ( "CACHE", repetitions,
                 cache_time, iter_time );
    print_time ( "MISS", repetitions,
                 miss_time, iter_time );
    print_time ( "TRIG", repetitions,
                 trig_time, iter_time );

    // Return from main function without error.

    return 0;
}

