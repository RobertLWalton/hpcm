/* Solution to the Binary Tree Sort Problem
 *
 * File:	bintreesort.c
 * Authors:	Bob Walton (walton@seas.harvard.edu)
 * Date:	Sun Mar 30 09:08:19 EDT 2014
 *
 * The authors have placed this program in the public
 * domain; they make no warranty and accept no liability
 * for this program.
 *
 * RCS Info (may not be true date or author):
 *
 *   $Author: walton $
 *   $Date: 2014/03/30 13:57:28 $
 *   $RCSfile: bintreesort.c,v $
 *   $Revision: 1.1 $
 */

#include <stdio.h>	/* sprintf */
#include <stdlib.h>	/* exit, abort */
#include <search.h>	/* tsearch, tfind, tdelete */
#include <assert.h>

int debug = 0;
#define dprintf if ( debug ) printf

/* Numbers must be stored so they may be remembered and
 * compared.  A 64-bit IEEE floating point format
 * suffices for 15 digit integers.
 */
typedef double number;

/* search.h implements a tree of nodes, which are
 * structures, with the first thing in the node being
 * a pointer to a user defined item, which contains
 * the key.  The tree proper is a variable pointing
 * at the root of the tree, or equal to NULL for an
 * empty tree.  Our dataset is such a tree.
 */
typedef void node;
const node * dataset;

/* The item type contains the number, which acts as the
 * key, and pointers for a double linked list that
 * contains all items in number sorted order, with a
 * special item, the `head', as both the beginning and
 * end of the list (the head is not part of the
 * dataset).
 */
typedef struct {
    number key;
    item * previous;
    item * next;
} item;
item head;

/* Get the item pointed at by a node.
 */
inline item * item_of ( const node * n )
{
    return * (item **) n;
}

/* Compare two items.
 */
int compare ( const void * xp, const void * yp )
{
    number x = (const item *) xp -> key;
    number y = (const item *) yp -> key;
    return x < y ? -1 : x == y ? 0 : +1;
}


/* Perform the `A n' operation to add n to the database.
 */
void add ( number n )
{
    node * parent = dataset;

    item * new_item =
        (item *) malloc ( sizeof ( item ) );
    new_item->key = n;
    node * found_node =
        tsearch ( new_item, & dataset, compare );
    if ( item_of ( found_node ) != new_item )
    {
        free ( new_item );
	return;
    }

    if ( parent == NULL )
    {
        /* Item is first in dataset.  Add it to list
	 * and return.
	 */

	new_item->previous = new_item->next = & head;
	head.previous = head.next = new_item;
	return;
    }
    else if ( item_of ( head ) != new_item )
    {
        /* New item is not now at root of tree.  Find
	 * its parent by removing item and then
	 * reinserting it.
	 */
	parent = (node *)
	    tdelete ( new_item, & dataset, compare );
	tsearch ( new_item, & dataset, compare );
    }
    else
    {
        /* New item is root of tree and not only
	 * thing in the tree.  Pretend old tree
	 * root is parent.
	 */
    }

    TBD

}

/* Perform the `E' operation to empty the database.
 */
void empty ( void )
{
}


int main ( int argc, char ** argv )
{
    head.previous = & head;
    head.next = & head;
    dataset = NULL;
}
