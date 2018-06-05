# HPCM Contestant Source Code Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Tue Jun  5 06:47:59 EDT 2018
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.

# Kill all implicit rules
#
.SUFFIXES:
%.out:	%

all:	hpcm_display \
	hpcm_display_graph \
        hpcm_display_segments \
        hpcm_display_polygon \
	hpcm_timing_g \
	hpcm_timing_O2

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

hpcm_display_graph:	hpcm_display_graph.cc
	g++ -I /usr/include/cairo \
	    -o hpcm_display_graph \
	    hpcm_display_graph.cc -lcairo -lX11

hpcm_display_segments:	hpcm_display_segments.cc
	g++ -I /usr/include/cairo \
	    -o hpcm_display_segments \
	    hpcm_display_segments.cc -lcairo -lX11

hpcm_display_polygon:	hpcm_display_polygon.cc
	g++ -I /usr/include/cairo \
	    -o hpcm_display_polygon \
	    hpcm_display_polygon.cc -lcairo -lX11

hpcm_timing_g:	hpcm_timing.cc
	g++ -g -o hpcm_timing_g hpcm_timing.cc

hpcm_timing_O2:	hpcm_timing.cc
	g++ -O2 -o hpcm_timing_O2 hpcm_timing.cc

test_X:		hpcm_display_graph
	hpcm_display -X \
	    hpcm_display_test.disp

clean:
	rm -f hpcm_display_graph \
	      hpcm_display_graph_test.ps \
	      hpcm_display_segments
