import sys

debug = ( len ( sys.argv ) > 1 )

def dprint (*args):
    global debug
    if debug:
        for arg in args:
	    print arg,
	print

def read():
    try:
        result = raw_input().rstrip ( '\n' )
    except EOFError:
        result = None
    finally:
        return result

paragraph = 1
characters = 0
words = 0
lines = 0

while True:

    line = read()
    if line == None or line.strip() == '':
	if lines > 0:
	    print "Paragraph", str(paragraph) + ":", \
		 lines, "lines,", \
		 words, "words,", \
		 characters, "characters."

	    characters = 0
	    words = 0
	    lines = 0
	    paragraph += 1

	if line == None:
	    break
	else:
	    continue

    lines += 1
    characters += len ( line )
    dprint ( "+", line )

    line = line.strip()
    line = line.split()
    words += len ( line )

    dprint ( ".", characters, words, lines )

exit ( 1 )  # This line can be omitted.
	    # It is a test that make count.out
	    # works even if count returns an
	    # error code.
