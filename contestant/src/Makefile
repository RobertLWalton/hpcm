# HPCM Contestant Source Code Makefile
#
# File:		Makefile
# Authors:	Bob Walton (walton@deas.harvard.edu)
# Date:		Sun Aug 14 08:40:06 EDT 2011
#
# The authors have placed this program in the public
# domain; they make no warranty and accept no liability
# for this program.
#
# RCS Info (may not be true date or author):
#
#   $Author: walton $
#   $Date: 2011/08/14 12:49:03 $
#   $RCSfile: Makefile,v $
#   $Revision: 1.1 $

# Kill all implicit rules
#
.SUFFIXES:
%.out:	%

all:	hpcm_display_graph

test:	hpcm_display_graph_test.ps

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
	    > hpcm_display_graph_test.ps \

clean:
	rm -f hpcm_display_graph \
	      hpcm_display_graph_test.ps
