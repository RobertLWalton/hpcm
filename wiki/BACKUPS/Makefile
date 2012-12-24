# Makefile for hpcm wiki backups.
#
#	File:	Makefile
#	Author:	walton@seas.harvard.edu
#	Date:	Mon Dec 24 11:36:45 EST 2012

HOST = problems1.seas.harvard.edu

# wiki-hpcm-2%.tgz contains the public backups.
#
# wiki-hpcm-private-2%.tgz contains the private backups
# with keys and passwords.

wiki-hpcm-2%.tgz:	never-there
	rm -f $@
	@if test "${HOST}" == "`hostname -f`"; \
	then \
	  rm -f wiki-hpcm-2$*.mysql; \
	  mysqldump -p${MYSQLPASSWORD} wiki_hpcm \
		  > wiki-hpcm-2$*.mysql; \
	  make -C ../images; \
	  tar zcf $@ --exclude=CVS \
                     wiki-hpcm-2$*.mysql \
		  -C ~/wiki images \
                  -C ~ hpcm/wiki/images \
                       hpcm/wiki/BACKUPS/Makefile; \
	  chmod 444 $@ wiki-hpcm-2$*.mysql; \
	  ls -l $@ wiki-hpcm-2$*.mysql; \
          chmod a+x . .. ../.. ../../..; \
	  chmod a+r .; \
	  chmod a+r Makefile; \
	else \
	  ssh wiki-hpcm@${HOST} \
	      'cd hpcm/wiki/BACKUPS; make $@'; \
	  scp wiki-hpcm@${HOST}:hpcm/wiki/BACKUPS/$@ $@; \
	  ls -l $@; \
	fi

wiki-hpcm-private-2%.tgz:	never-there
	rm -f $@
	@if test "${HOST}" == "`hostname -f`"; \
	then \
	  tar zcf $@ -C ~/wiki \
	          LocalSettings.php RCS; \
	  chmod 400 $@; \
	  ls -l $@; \
	else \
	  ssh wiki-hpcm@${HOST} \
	      'cd hpcm/wiki/BACKUPS; make $@'; \
	  scp wiki-hpcm@${HOST}:hpcm/wiki/BACKUPS/$@ $@; \
	  ls -l $@; \
	fi
     
never-there:	
