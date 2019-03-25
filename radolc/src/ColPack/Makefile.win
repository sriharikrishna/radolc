#-------------------------------------------------------------------------------
# COLPACK Makefile
#-------------------------------------------------------------------------------

.PHONY : default all get library purge clean distclean ccode

default: all

# Compile the C-callable libraries and the Demo programs.


get:
	#mkdir $(shell pwd)/ColPack-master
	#xcopy "C:\Users\celsloaner\Downloads\ColPack-master" "C:\Users\celsloaner\Documents\GitHub\radolc\autodiffadolc\src\ColPack" /s /i
	#wget --no-check-certificate --content-disposition https://github.com/CSCsw/ColPack/archive_colpack.tar.gz > archive_colpack.tar.gz
	#echo 'download.file(url="https://github.com/CSCsw/ColPack/archive/master.zip", destfile="ColPack-master.zip", method="wget", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))'|${R_HOME}/bin/R --vanilla -slave
	#echo 'download.file(url="https://github.com/sriharikrishna/ColPack/archive/master.zip", destfile="master.zip", method="wget", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))'|'C:/Program Files/R/R-3.5.0/bin/x64/R' --vanilla
	echo 'git2r::clone(url = "https://github.com/CSCsw/ColPack.git", local_path = "Colpack-master", bare = FALSE, branch = NULL, checkout = TRUE, credentials = NULL, progress = TRUE)'|'C:/Program Files/R/R-3.5.0/bin/x64/R' --vanilla -slave
	#tar zxvf archive_colpack.tar.gz
	#cp C:\\Users\\celsloaner\\Desktop\\ColPack-master.zip $(CURDIR)/
	#wget http://mcs.anl.gov/~snarayan/master.zip
	#bsdtar -xf ColPack-master.zip
	#unzip C:\Users\celsloaner\Downloads\ColPack-master.zip -d $(shell pwd)
	#unzip C:\Users\celsloaner\Downloads\ColPack-master.zip
	#gzip -k C:/Users/celsloaner/Downloads/ColPack-master.zip
	#"C:\Program Files (x86)\7-Zip\7z" x ColPack-master.zip -aoa
	#mv master.zip ColPack-master.zip
	#7z x ColPack-master.zip -aoa

#get: C:\Users\celsloaner\Desktop\ColPack-master

all: get
	( cd $(CURDIR)\\ColPack-master ; sed -i.bak 's/^\(AM_PROG_AR\).*/\m4_ifdef([AM_PROG_AR], [AM_PROG_AR])/' configure.ac ; sh autoreconf -fi ; sh configure --prefix=$(CURDIR)/ColPack-master-inst --disable-openmp; $(MAKE) clean ; $(MAKE) LDFLAGS=-no-undefined; $(MAKE) install )

# Compile the C-callable libraries only.
library: all

# Remove all files not in the original distribution
purge:
	#( rm -Rf ColPack-master* archive* )
	#( rm -Rf ColPack-master)
# Remove all files not in the original distribution, expcept keep the
# compiled libraries
clean:
	#(rm -Rf ColPack-master-inst ; cd ${CURDIR}\\ColPack-master ; $(MAKE) clean )
	#( rm -Rf ColPack-master* archive* master*)
	( rm -Rf ColPack-master ColPack-master* archive* master*)
	#(rm -Rf ColPack-master-inst )
distclean: purge

ccode: all
