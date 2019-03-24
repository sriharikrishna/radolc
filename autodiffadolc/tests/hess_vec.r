rm(list=ls())

library('autodiffadolc')

#--- testing ADOLC's hessian vector product
fr <- function(x) {   ## Rosenbrock Banana function
  x1 <- x[1]
  x2 <- x[2]
  y <- 100 * (x2 - x1 * x1)* (x2 - x1 * x1) + (1 - x1)*(1 - x1)
  y }

trace_on(1)
x <- adolc_createList(2,2.0)
badouble_declareIndependent(x)
y <- fr(x)
badouble_declareDependent(y)
trace_off()

xx <- c(1.0,2.0)
vv <- c(3.0,5.5)
yy <- c(0.0,0.0)
hess_vec(1,2,xx,vv,yy) # result z = \nambla^2F(x)v
#hess_vec(tag,n,x,v,z)
# tag: integer, tape identification
# n  : integer, number of independents n and m = 1
# x[n]: independent vector x
# v[n]: vector
# z[n]: resulting Hessian vector product

print(yy)     

#Always detach the package
detach(package:autodiffadolc, unload=TRUE) 
