# HPCM Contestant Source Code Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Fri Jul  7 16:43:08 EDT 2017
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

all:	hpcm_display \
	hpcm_display_graph \
        hpcm_display_segments \
        hpcm_display_polygon

test_ps:	hpcm_display_graph_test.ps


hpcm_display:	hpcm_display.cc
	g++ -I /usr/include/cairo \
	    -o hpcm_display \
	    hpcm_display.cc -lcairo -lX11

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

hpcm_display_polygon:	hpcm_display_polygon.cc
	g++ -I /usr/include/cairo \
	    -o hpcm_display_polygon \
	    hpcm_display_polygon.cc -lcairo -lX11

test_X:		hpcm_display_graph
	hpcm_display_graph -X \
	    hpcm_display_graph_test.in \
	    hpcm_display_graph_test.out

clean:
	rm -f hpcm_display_graph \
	      hpcm_display_graph_test.ps \
	      hpcm_display_segments
