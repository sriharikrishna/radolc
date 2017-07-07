
rm(list=ls())

source('init_adolc.R')

library('numDeriv')


fr <- function(x) {   ## Rosenbrock Banana function
    y <- 100 * (1 - x * x)* (1 - x * x) + (1 - x)*(1 - x)
    y }

grr <- function(x) { ## Gradient of 'fr'
    g <-  0-400 * x * (1 - x * x) - 2 * (1 - x)
    g }

grrNumDeriv <- function(x) { ## Gradient of 'fr'
    g = grad(func=fr,x=x)
    g     }

#---- testing gradient
grr(3)
grrNumDeriv(3)

#---- building the gradient function with ADOLC
trace_on(1)
x <- adouble(1.0)
badouble_declareIndependent(x)
y <- fr(x)
badouble_declareDependent(y)
trace_off()

grrADOLC <- function(x) { ## Gradient of 'fr'
    y <- c(0.0)
    gradient(1,1,x,y);
    y    }

#---- ADOLC gradient
grrADOLC(3)

grr(3)

#---- optim with the gradients 
res0 <- optim(c(-1), fr, method = "L-BFGS-B", control = list(type = 3, trace = 2))
res1 <- optim(c(-1), fr, grr, method = "L-BFGS-B", control = list(type = 3, trace = 2))
res2 <- optim(c(-1), fr, grrADOLC, method = "L-BFGS-B", control = list(type = 3, trace = 2))

print(res0)

print(res1)

print(res2)

