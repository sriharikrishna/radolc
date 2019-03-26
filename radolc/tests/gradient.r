library(radolc)

fr <- function(x) {   ## Rosenbrock Banana function
    x1 <- x[[1]]
    x2 <- x[[2]]
    y <- 100 * (x2 - x1 * x1)* (x2 - x1 * x1) + (1 - x1)*(1 - x1)
    y }

tag <- 1
trace_on(tag)
x <- c(adouble(1.0),adouble(2.0))
badouble_declareIndependent(x)
y <- fr(x)

badouble_declareDependent(y)
trace_off()
#gradient(tag,n,x,g)
# tag: integer, tape identification
# n  : integer, number of independents n and m = 1
# x[n]: independent vector x
# g[n] :resulting gradient \gradF(x)

x=c(1,2)
g <- c(0.0,0.0)
gradient(1,2,x,g)

#Always detach the package
detach(package:radolc, unload=TRUE)
