# Makefile for hpcm wiki backups.
#
#	File:	Makefile
#	Author:	walton@seas.harvard.edu
#	Date:	Sat Dec 22 04:35:56 EST 2012

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
	  tar zcf $@ wiki-hpcm-2$*.mysql \
		  -C ~/wiki \
		  images; \
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
