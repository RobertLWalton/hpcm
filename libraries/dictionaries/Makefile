# Makefile for Making Dictionaries
#
# File:		Makefile
# Date:		Fri Sep  1 07:03:23 EDT 2017
#
# The authors have placed this file in the public
# domain; they make no warranty and accept no liability
# for this file.

BUILDING_FILES = \
	Makefile \
	make_dictionary \
	adj.exc \
	adv.exc \
	index.adj \
	index.adv \
	index.noun \
	index.sense \
	index.verb \
	noun.exc \
	part-of-speech.txt \
	spell-checker-file.txt \
	verb.exc \
	ignored-words.txt \
	word-approvals.txt \
	words-by-frequency.txt

all:	${BUILDING_FILES} \
	most-common-20000-with-part-code.dict

WordNet=WordNet-3.0
sc_file=yawl-0.3.2.03/word.list
wbf_file=ngram/most-common-50000-words-by-frequency.txt

adj.exc:
	ln -s ${WordNet}/adj.exc .
adv.exc:
	ln -s ${WordNet}/adv.exc .
index.adj:
	ln -s ${WordNet}/index.adj .
index.adv:
	ln -s ${WordNet}/index.adv .
index.noun:
	ln -s ${WordNet}/index.noun .
index.sense:
	ln -s ${WordNet}/index.sense .
index.verb:
	ln -s ${WordNet}/index.verb .
noun.exc:
	ln -s ${WordNet}/noun.exc .
spell-checker-file.txt:
	ln -s ${sc_file} spell-checker-file.txt
verb.exc:
	ln -s ${WordNet}/verb.exc .
words-by-frequency.txt:
	ln -s ${wbf_file} words-by-frequency.txt

make_dictionary:	make_dictionary.cc
	g++ -g -std=c++11 -o make_dictionary \
	    make_dictionary.cc

most-common-20000-with-part-code.dict:	\
		${BUILDING_FILES}
	-rm most-common-20000-with-part-code.dict
	make_dictionary -a 20000 1 \
	    < words-by-frequency.txt \
	    > most-common-20000-with-part-code.dict

find_abbreviations:	find_abbreviations.cc
	g++ -g -std=c++11 -o find_abbreviations \
	    find_abbreviations.cc

ignored-words.txt:	find_abbreviations \
                        /usr/share/dict/words
	sort /usr/share/dict/words \
	    | find_abbreviations > ignored-words.txt

clean:
	rm -f make_dictionary
