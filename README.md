# Interface between R and ADOL-C 
This repository contains the R package autodiffadolc which is an interface to the automatic differentiation tool ADOL-C from R.

## Usage
~~~~
#Load the library
library('autodiffadolc')

#Define the Rosenbrock Banana function

fr <- function(x) {   ## Rosenbrock Banana function
    x1 <- x[[1]]
    x2 <- x[[2]]
    y <- 100 * (x2 - x1 * x1)* (x2 - x1 * x1) + (1 - x1)*(1 - x1)
    y }


#Start the trace of the computation
trace_on(1)
x <- c(adouble(1.0),adouble(2.0))
#Declare the independent (input) variable
badouble_declareIndependent(x)
#Perform the computation
y <- fr(x)
#Declare the dependent (input) variable
badouble_declareDependent(y)
#Stop the trace of the computation
trace_off()

#Define a function to compute the Gradient of 'fr'
grrADOLC <- function(x) {
   xx <- x
	 yy <- c(0.0,0.0)
#This is the call to the ADOL-C driver to compute the derivatives
	 gradient(1,2,xx,yy);
   yy }

#Call the optimization routine. It will use grrADOLC to get the derivatives from ADOL-C's gradient()
res6 <- optim(c(1.2,1), fr, grrADOLC, method = "L-BFGS-B", control = list(type = 3, trace = 2))
~~~~

## Known shortcomings
1. External functions such as *solve* are not handled

## How to cite
[K. Kulshreshtha, S. Narayanan, J. Bessac, and K. MacIntyre, Efficient computation of derivatives for solving optimization problems in R and Python using SWIG-generated interfaces to ADOL-C, Optimization Methods and Software 33 (2018), pp. 1173--1191.][https://doi.org/10.1080/10556788.2018.1425861]
