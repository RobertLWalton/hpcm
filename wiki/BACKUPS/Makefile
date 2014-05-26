# Makefile for hpcm wiki backups.
#
#	File:	Makefile
#	Author:	walton@seas.harvard.edu
#	Mon May 26 01:24:00 EDT 2014

FULLHOST = problems1.seas.harvard.edu
SHORTHOST = problems1

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
            if test `expr "$$H" : "${SHORTHOST}"` \
	            -gt 0; \
	then \
	  rm -f wiki-hpcm-2$*.mysql; \
	  mysqldump -p${MYSQLPASSWORD} wiki_hpcm \
		  > wiki-hpcm-2$*.mysql; \
	  make -C ../images; \
	  make -C ../print; \
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
	  ssh wiki-hpcm@${FULLHOST} \
	      'cd hpcm/wiki/BACKUPS; make $@'; \
	  scp \
	    wiki-hpcm@${FULLHOST}:hpcm/wiki/BACKUPS/$@ \
            $@; \
	  ls -l $@; \
	fi

wiki-hpcm-2%.restore:	never-there
	@echo drop database if exists wiki_hpcm ';'
	@echo create database wiki_hpcm ';'
	@echo drop database if exists wiki_hpcm ';' \
             create database wiki_hpcm ';' \
	     | mysql -p${MYSQLPASSWORD}
	@echo mysql -p... wiki_hpcm '<' \
              wiki-hpcm-2$*.mysql
	@mysql -p${MYSQLPASSWORD} wiki_hpcm \
            < wiki-hpcm-2$*.mysql

wiki-hpcm-private-2%.tgz:	never-there
	rm -f $@
	@H=`hostname`; \
            if test `expr "$$H" : "${SHORTHOST}"` \
	             -gt 0; \
	then \
	  tar zcf $@ -C ~/wiki ${CHANGED}; \
	  chmod 400 $@; \
	  ls -l $@; \
	else \
	  ssh wiki-hpcm@${FULLHOST} \
	      'cd hpcm/wiki/BACKUPS; make $@'; \
	  scp \
	    wiki-hpcm@${FULLHOST}:hpcm/wiki/BACKUPS/$@ \
            $@; \
	  ls -l $@; \
	fi
     
never-there:	
