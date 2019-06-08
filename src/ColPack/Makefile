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

all: get
	( rm -Rf ${PWD}/ColPack-master/src/PartialD2SMPGC; rm -Rf ${PWD}/ColPack-master/src/SMPGC; cd ${PWD}/ColPack-master/build/automake ; sed  '/pkginclude_HEADERS[[:space:]]+/d' Makefile.am >temp;  sed  '/SMPGC/d' temp >Makefile.am;  sed  '/example/d' Makefile.am >temp1; sed '/Example/d' temp1 >temp2 ; sed  '/noinst_PROGRAMS/d' temp2 > temp1; sed '/ColPack_SOURCES/d' temp1 > Makefile.am; libtoolize; autoreconf -vif ; mkdir mywork; cd mywork; ${PWD}/ColPack-master/build/automake/configure --disable-openmp --disable-examples --prefix=${PWD}//ColPack-master-inst; $(MAKE) LDFLAGS=-no-undefined; $(MAKE) install )

# Compile the C-callable libraries only.
library: all

# Remove all files not in the original distribution
purge:
	( rm -Rf ColPack* ColPack-master* archive* )
	( rm -Rf ColPack-master master*)
	( rm -Rf  *.tar *.zip *.tar.gz *inst)

# Remove all files not in the original distribution, expcept keep the
# compiled libraries
clean: purge

distclean: purge

ccode: all
