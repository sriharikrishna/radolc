
rm(list=ls())

source('init_adolc.R')

library('numDeriv')

grr <- function(x) { ## Gradient of 'fr'
         x1 <- x[1]
         x2 <- x[2]
         c(-400 * x1 * (x2 - x1 * x1) - 2 * (1 - x1), 200 * (x2 - x1 * x1))     }

#------------------------------------------------------------- testing ADOLC in the bi-variate case

fr <- function(x) {   ## Rosenbrock Banana function
    x1 <- x[[1]]
    x2 <- x[[2]]
    y <- 100 * (x2 - x1 * x1)* (x2 - x1 * x1) + (1 - x1)*(1 - x1)
    y }

#---- Problems start here ....

trace_on(1)
x <- c(adouble(1.0),adouble(2.0))
badouble_declareIndependent(x)
y <- fr(x)
badouble_declareDependent(y)
trace_off()

grrADOLC <- function(x) { ## Gradient of 'fr'
         xx <- x
	 yy <- c(0.0,0.0)
	 gradient(1,2,xx,yy);
   	 yy     }

grrADOLC(x=c(1,2))

res6 <- optim(c(1.2,1), fr, grrADOLC, method = "L-BFGS-B", control = list(type = 3, trace = 2))

