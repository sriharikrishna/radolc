
rm(list=ls())

library('radolc')

#--- testing ADOLC's hessian
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
yy <- matrix(rep(0.0,4), nrow = 2, ncol = 2)
hessian(1,2,xx,yy);
#hessian(tag,n,x,H)
# tag: integer, tape identification
# n  : integer, number of independents n and m = 1
# x[n]: independent vector x
# double H[n][n]: resulting Hessian matrix \nabla^2F(x)
print(yy)     

#Always detach the package
detach(package:radolc, unload=TRUE) 
