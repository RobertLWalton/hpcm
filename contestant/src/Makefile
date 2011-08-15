# HPCM Contestant Source Code Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Mon Aug 15 06:29:59 EDT 2011
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2011/08/15 10:39:33 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.2 $

# Kill all implicit rules
#
.SUFFIXES:
%.out:	%

all:	hpcm_display_graph

test_ps:	hpcm_display_graph_test.ps

hpcm_display_graph:	hpcm_display_graph.cc
	g++ -I /usr/include/cairo \
	    -o hpcm_display_graph \
	    hpcm_display_graph.cc -lcairo

hpcm_display_graph_test.ps:	\
	hpcm_display_graph \
	hpcm_display_graph_test.in \
	hpcm_display_graph_test.out
	rm -f hpcm_display_graph_test.ps
	hpcm_display_graph -ps \
	    hpcm_display_graph_test.in \
	    hpcm_display_graph_test.out \
	    > hpcm_display_graph_test.ps

test_X:		hpcm_display_graph
	hpcm_display_graph -X \
	    hpcm_display_graph_test.in \
	    hpcm_display_graph_test.out

clean:
	rm -f hpcm_display_graph \
	      hpcm_display_graph_test.ps
