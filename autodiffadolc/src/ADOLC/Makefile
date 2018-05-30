#-------------------------------------------------------------------------------
# CHOLMOD Makefile
#-------------------------------------------------------------------------------

.PHONY : default all get library purge clean distclean ccode

default: all

# Compile the C-callable libraries and the Demo programs.

get:
	curl https://gitlab.com/adol-c/adol-c/repository/swig/archive.tar.gz > archive.tar.gz
	tar zxvf archive.tar.gz
	mv adol-c-swig-* adol-c-swig
 
all: get
	( cd ${PWD}/adol-c-swig ; autoreconf -fi; ./configure CXXFLAGS=-std=c++11 --enable-sparse --enable-static --prefix=${PWD}/adol-c-swig-inst; $(MAKE); $(MAKE) install )

# Compile the C-callable libraries only.
library: all

# Remove all files not in the original distribution
purge:
	( rm -Rf adol-c-swig* archive* )

# Remove all files not in the original distribution, except keep the 
# compiled libraries.
clean:
#	( cd ${PWD}/adol-c-swig ; $(MAKE) clean )

distclean: purge

ccode: all

