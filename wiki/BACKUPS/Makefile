# Makefile for hpcm wiki backups.
#
#	File:	Makefile
#	Author:	walton@seas.harvard.edu
#	Date:	Wed Jan  2 11:10:56 EST 2013

HOST = problems1.seas

CHANGED = LocalSettings.php RCS \
          skins/common/commonPrint.css \
          skins/common/RCS

# wiki-hpcm-2%.tgz contains the public backups.
#
# wiki-hpcm-private-2%.tgz contains the private backups
# with keys and passwords.

wiki-hpcm-2%.tgz:	never-there
	rm -f $@
	@H=`hostname`; \
            if test `expr "$$H" : "${HOST}"` -gt 0; \
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
	  scp wiki-hpcm@${HOST}:hpcm/wiki/BACKUPS/$@ \
              $@; \
	  ls -l $@; \
	fi

wiki-hpcm-private-2%.tgz:	never-there
	rm -f $@
	@H=`hostname`; \
            if test `expr "$$H" : "${HOST}"` -gt 0; \
	then \
	  tar zcf $@ -C ~/wiki ${CHANGED}; \
	  chmod 400 $@; \
	  ls -l $@; \
	else \
	  ssh wiki-hpcm@${HOST} \
	      'cd hpcm/wiki/BACKUPS; make $@'; \
	  scp wiki-hpcm@${HOST}:hpcm/wiki/BACKUPS/$@ \
              $@; \
	  ls -l $@; \
	fi
     
never-there:	
