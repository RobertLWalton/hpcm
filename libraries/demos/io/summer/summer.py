# Solution to the Summation Checking Problem
#
# File:		summer.py
# Authors:	Bob Walton (walton@seas.harvard.edu)
# Date:		Fri Oct 11 07:23:43 EDT 2019
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.

import sys

debug = ( len ( sys.argv ) > 1 )

def dprint (*args):
    global debug
    if debug:
        for arg in args:
            print ( arg, end='' )
        print ()

def read():
    try:
        result = input().rstrip ( '\n' )
    except EOFError:
        result = None
    finally:
        return result

while True:

    line = read()
    if line == None:
        break

    print ( line )

    line = read()
    items = line.split()

    actual = 0
    assert ( items[-2] == '=' )
    for item in items[:-2]:
        actual += float ( item )
    desired = float ( items[-1] )

    if desired == actual:
        print ( '{:.2f} is correct'.format ( actual ) )
    else:
        print ( '{:.2f} should be {:.2f}' \
                .format ( desired, actual ) )
