# HPCM Contestant Source Code Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Thu Mar 10 03:41:30 EST 2016
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2012/10/26 08:54:52 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.4 $

# Kill all implicit rules
#
.SUFFIXES:
%.out:	%

all:	hpcm_display_graph \
        hpcm_display_segments

test_ps:	hpcm_display_graph_test.ps

hpcm_display_graph:	hpcm_display_graph.cc
	g++ -I /usr/include/cairo \
	    -o hpcm_display_graph \
	    hpcm_display_graph.cc -lcairo -lX11

hpcm_display_graph_test.ps:	\
	hpcm_display_graph \
	hpcm_display_graph_test.in \
	hpcm_display_graph_test.out
	rm -f hpcm_display_graph_test.ps
	hpcm_display_graph -ps \
	    hpcm_display_graph_test.in \
	    hpcm_display_graph_test.out \
	    > hpcm_display_graph_test.ps

hpcm_display_segments:	hpcm_display_segments.cc
	g++ -I /usr/include/cairo \
	    -o hpcm_display_segments \
	    hpcm_display_segments.cc -lcairo -lX11

test_X:		hpcm_display_graph
	hpcm_display_graph -X \
	    hpcm_display_graph_test.in \
	    hpcm_display_graph_test.out

clean:
	rm -f hpcm_display_graph \
	      hpcm_display_graph_test.ps \
	      hpcm_display_segments
