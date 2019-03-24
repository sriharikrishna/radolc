
rm(list=ls())

library('autodiffadolc')

#--- testing ADOLC's jacobian

fr <- function(x) {
  x1 <- x[[1]]
  x2 <- x[[2]]
  #Use create list to create adouble objects
  y<-adolc_createList(2,2.0)
  #y <- c(0.0,0.0)
  y[[1]] <- 100 * (x2 - x1 * x1)* (x2 - x1 * x1) + (1 - x1)*(1 - x1)
  y[[2]] <- 100 * (x2 - x1 * x1)+ (x2 - x1 * x1) * (1 - x1)*(1 - x1)
  y[[3]] <- 100 * (x2 + x1 * x1)+ (x2 + x1 * x1) * (1 + x1)*(1 - x1)
  y}

trace_on(1)
x <- adolc_createList(2,2.0)
badouble_declareIndependent(x)
y <- fr(x)
badouble_declareDependent(y)
trace_off()

xx <- c(1.0,2.0)
vv <- c(3.0,5.5,1,1)
yy <- c(0.0,0.0)
vec_jac(1,3,2,0,xx,vv,yy);
# vec_jac(tag,m,n,repeat,x,u,z) #result z = u^TF′(x)
# tag: integer, tape identification
# m  : integer, number of dependents m
# n  : integer, number of independents n
#repeat: ID of tape to reuse. Leave at 0 for no repetition
# x[n]: independent vector x
# u[m]: vector
# z[n]: resulting Jacobian vector product

print(yy)     

#Always detach the package
detach(package:autodiffadolc, unload=TRUE) 
