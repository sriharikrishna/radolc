#-------------------------------------------------------------------------------
# ADOLC Makefile
#-------------------------------------------------------------------------------

.PHONY : default all get library purge clean distclean ccode

default: all

# Compile the C-callable libraries and the Demo programs.

get:
	echo 'download.file(url="https://gitlab.com/adol-c/adol-c/repository/swig/archive.tar.gz", destfile="archive.tar.gz", method="curl", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))'|$(R_HOME)/bin/R --vanilla --slave
	tar zxvf archive.tar.gz
	mv adol-c-swig* adol-c-swig
 
ifeq (, $(shell which autoreconf))
	echo 'download.file(url="https://ftp.gnu.org/gnu/m4/m4-latest.tar.gz", destfile="m4-latest.tar.gz", method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE, extra = getOption("download.file.extra"))    '|$(R_HOME)/bin/R --vanilla 
	gzip -d m4-latest.tar.gz
	tar -xvf m4-latest.tar
	( cd  ${PWD}/m4-1.4.17  ; ./configure --prefix=${PWD}/m4-latest-inst; $(MAKE); $(MAKE) install )

	echo 'download.file(url="https://ftp.gnu.org/gnu/autoconf/autoconf-latest.tar.gz", destfile="autoconf-latest.tar.gz", method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE, extra = getOption("download.file.extra"))    '|$(R_HOME)/bin/R --vanilla 
	gzip -d autoconf-latest.tar.gz
	tar -xvf autoconf-latest.tar
	( cd  ${PWD}/autoconf-2.69  ; ./configure --prefix=${PWD}/autoconf-latest-inst; $(MAKE); $(MAKE) install )

	echo 'download.file(url="https://ftp.gnu.org/gnu/automake/automake-1.16.tar.gz", destfile="automake-1.16.tar.gz", method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE, extra = getOption("download.file.extra"))    '|$(R_HOME)/bin/R --vanilla 
	gzip -d automake-1.16.tar.gz
	tar -xvf automake-1.16.tar
	( cd  ${PWD}/automake-1.16  ; ./configure --prefix=${PWD}/automake-latest-inst; $(MAKE); $(MAKE) install )

	echo 'download.file(url="http://ftpmirror.gnu.org/libtool/libtool-2.4.6.tar.gz", destfile="libtool-2.4.6.tar.gz", method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE, extra = getOption("download.file.extra"))    '|$(R_HOME)/bin/R --vanilla 
	gzip -d libtool-2.4.6.tar.gz
	tar -xvf libtool-2.4.6.tar
	( cd  ${PWD}/libtool-2.4.6  ; ./configure --prefix=${PWD}/libtool-latest-inst; $(MAKE); $(MAKE) install )
endif

all: get
	( cd ${PWD}/adol-c-swig ; libtoolize; autoreconf -fi ; ./configure --enable-sparse --with-colpack=${PWD}/../Colpack/Colpack-master-inst/ --prefix=${PWD}/adol-c-swig-inst; $(MAKE); $(MAKE) install )
 
# Compile the C-callable libraries only.
library: all

# Remove all files not in the original distribution
purge:
	( rm -Rf adol-c-swig* archive* *.tar.gz *.tar.bz2 )

# Remove all files not in the original distribution 
clean: purge

distclean: purge

