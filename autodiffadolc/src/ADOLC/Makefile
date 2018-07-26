#-------------------------------------------------------------------------------
# CHOLMOD Makefile
#-------------------------------------------------------------------------------

.PHONY : default all get library purge clean distclean ccode

default: all

# Compile the C-callable libraries and the Demo programs.

get:
	echo 'download.file(url="https://math.uni-paderborn.de/fileadmin/mathematik/AG-Mathematik-u-i-Anwendungen/Software/adolc-swig/adolc-swig-branch.tar.bz2", destfile="adolc-swig-branch.tar.bz2", method="curl", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))'|R --vanilla --slave
	tar zxvf adolc-swig-branch.tar.bz2
	mv adolc-swig-branch adol-c-swig
 
all: get
	( cd ${PWD}/adol-c-swig ; ./configure --enable-sparse --prefix=${PWD}/adol-c-swig-inst; $(MAKE); $(MAKE) install )

# Compile the C-callable libraries only.
library: all

# Remove all files not in the original distribution
purge:
	( rm -Rf adol-c-swig* archive* *.tar.gz *.tar.bz2 )

# Remove all files not in the original distribution, except keep the 
# compiled libraries.
clean:
#	( cd ${PWD}/adol-c-swig ; $(MAKE) clean )

distclean: purge

ccode: all

