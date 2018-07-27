#-------------------------------------------------------------------------------
# ADOLC Makefile
#-------------------------------------------------------------------------------

.PHONY : default all get library purge clean distclean ccode

default: all

# Compile the C-callable libraries and the Demo programs.

get:
	echo 'download.file(url="https://gitlab.com/adol-c/adol-c/repository/swig/archive.tar.gz", destfile="archive.tar.gz", method="curl", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))'|R --vanilla --slave
	tar zxvf archive.tar.gz
	mv adol-c-swig* adol-c-swig
 
all: get
	( cd ${PWD}/adol-c-swig ; autoreconf -fi ; ./configure --enable-sparse --prefix=${PWD}/adol-c-swig-inst; $(MAKE); $(MAKE) install )

# Compile the C-callable libraries only.
library: all

# Remove all files not in the original distribution
purge:
	( rm -Rf adol-c-swig* archive* *.tar.gz *.tar.bz2 )

# Remove all files not in the original distribution 
clean: purge

distclean: purge

