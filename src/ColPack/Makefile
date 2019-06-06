#-------------------------------------------------------------------------------
# COLPACK Makefile
#-------------------------------------------------------------------------------

.PHONY : default all get library purge clean distclean ccode

default: all

# Compile the C-callable libraries and the Demo programs.


get:
	echo 'download.file(url="https://github.com/CSCsw/ColPack/archive/ec52afcd93b96f35c0f3edb562c090f67485e5d2.zip", destfile="archive.zip", method="wget", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))'|R --vanilla -slave
	unzip archive.zip
	mv ColPack-* ColPack-master

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
	( rm -Rf ${PWD}/ColPack-master/src/PartialD2SMPGC; rm -Rf ${PWD}/ColPack-master/src/SMPGC; cd ${PWD}/ColPack-master/build/automake ; sed  '/pkginclude_HEADERS[[:space:]]+/d' Makefile.am >temp;  sed  '/SMPGC/d' temp >Makefile.am;  sed  '/example/d' Makefile.am >temp1; sed '/Example/d' temp1 >temp2 ; sed  '/noinst_PROGRAMS/d' temp2 > temp1; sed '/ColPack_SOURCES/d' temp1 > Makefile.am; libtoolize; autoreconf -vif ; mkdir mywork; cd mywork; ${PWD}/ColPack-master/build/automake/configure --disable-openmp --disable-examples --prefix=${PWD}//ColPack-master-inst; $(MAKE) LDFLAGS=-no-undefined; $(MAKE) install )

# Compile the C-callable libraries only.
library: all

# Remove all files not in the original distribution
purge:
	( rm -Rf ColPack-master* archive* )
	( rm -Rf ColPack-master)

# Remove all files not in the original distribution, expcept keep the
# compiled libraries
clean:
	( rm -Rf ColPack-master ColPack-master* archive* master*)

distclean: purge

ccode: all
