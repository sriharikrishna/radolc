# Interface between R and ADOL-C 
This repository contains the supporting files to use the automatic differentiation tool ADOL-C from R. 

## Build Instructions
**NOTE**: Any text in *italics* should be replaced by a directory location in your file system. 

1. Obtain the code in this repository
> mkdir RProject
> cd RPRoject
 
2. Install ColPack
>git clone https://github.com/CSCsw/ColPack.git

>cd ColPack

>autoreconf -fi

>./configure --prefix=*COLPACK_INST_DIR* --exec-prefix=*COLPACK_INST_DIR* CC=/usr/bin/clang CXX=/usr/bin/clang++ --enable-examples

>make

>make install

3. Install ADOL-C
>git clone https://gitlab.com/adol-c/adol-c.git 

> cd adol-c/

>git checkout swig

>autoreconf -fi

>./configure CC=/usr/bin/clang CXX=/usr/bin/clang++ --prefix=*ADOLC_INST_DIR* --with-colpack=*COLPACK_INST_DIR*

>make

>make install

4. Install SWIG from the specific repository
>git clone git@gitlab.com:sriharikrishna/swig.git

> cd swig

>./autogen.sh

>./configure --prefix=*SWIG_INST_DIR*

>make

>make install

>export PATH=*SWIG_INST_DIR*/bin/:$PATH

5. Use Swig to create the interface
>cd adol-c/ADOL-C/swig

>python swigprocess.py --r

## Usage 
1. Ensure that ADOL-C libraries are available (on linux)

>export LD\_LIBRARY\_PATH=*ADOLC_INST_DIR*/lib64/:$LD\_LIBRARY\_PATH

OR on OSX

>export DYLD\_LIBRARY\_PATH=*ADOLC_INST_DIR*/lib64/:$DYLD\_LIBRARY\_PATH

2. Go into radolc and run an example
>cd radolc

>R

>> source("rosenbrock\_univariate\_optim.r")

## Known shortcomings
1. Matrices are not handled. 

2. External functions such as *solve* are not handled

## How to cite
K. Kulshreshtha, S.H.K. Narayanan, J. Bessac, and K. MacIntyre, Efficient computation of derivatives for solving optimization problems in R and Python using SWIG-generated interfaces to ADOL-C, *in submission Optimization Methods and Software*, [available as preprint](http://www.mcs.anl.gov/papers/P6096-0117.pdf).
