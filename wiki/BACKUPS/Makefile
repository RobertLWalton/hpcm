wiki-hpcm-2%.tgz:	never-there
	ssh wiki-hpcm@problems1.seas.harvard.edu \
	    'cd BACKUPS; make $@'
	rm -f $@
	scp wiki-hpcm@problems1.seas.harvard.edu:BACKUPS/$@ $@

never-there:	
