
rm(list=ls())

library('radolc')

#--- testing ADOLC's jacobian

fr <- function(x) {
  x1 <- x[[1]]
  x2 <- x[[2]]
  #Use create list to create adouble objects
  y<-adolc_createList(2,2.0)
  #y <- c(0.0,0.0)
  y[[1]] <- 100 * (x2 - x1 * x1)* (x2 - x1 * x1) + (1 - x1)*(1 - x1)
  y[[2]] <- 100 * (x2 - x1 * x1)+ (x2 - x1 * x1) * (1 - x1)*(1 - x1)
  y}

trace_on(1)
x <- adolc_createList(2,2.0)
badouble_declareIndependent(x)
y <- fr(x)
badouble_declareDependent(y)
trace_off()

xx <- c(1.0,2.0)
yy <- matrix(rep(0.0,4), nrow = 2, ncol = 2)
jacobian(1,2,2,xx,yy);
#jacobian(tag,m,n,x,J)
# tag: integer, tape identification
# m  : integer, number of dependent variables
# n  : integer, number of independent variables n
# x[n]: independent vector x
#J[m][n]; // resulting Jacobian F(x)
print(yy)     

#Always detach the package
detach(package:radolc, unload=TRUE) 
