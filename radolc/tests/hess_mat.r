rm(list=ls())

library('radolc')

#--- testing ADOLC's Hessian matrix product
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
V <- matrix(c(2.2, 1.4, 3.4, 7.4), nrow = 2, ncol = 2)
W <- matrix(rep(0.0,4), nrow = 2, ncol = 2)
hess_mat(1,2,2,xx,V,W) 
# hess_mat(tag, n, q, x[n], V[n][q], W[n][q]) # result z = \nambla^2F(x)v
# tag: integer, tape identification
# n  : integer, number of independents n and m = 1
# q  : dimension of matrix (n*q)
# x[n]: independent vector x
# V[n][q]: matrix
# W[n][q]: resulting Hessian matrix product

print(W)     

#Always detach the package
detach(package:radolc, unload=TRUE) 
