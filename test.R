#dyn.load(paste("adolc", .Platform$dynlib.ext, sep=""))




trace_on(1)

a <- adouble(2.0)
b <- adouble(1.0)
badouble_declareIndependent(a)
badouble_declareIndependent(b)
x <- a*a + b*b + 2 *a *b
badouble_declareDependent(x)
trace_off()

c1 <- c(1.0,2.0)
c2 <- c(0.0)
zos_forward(1,1,2,1,c1,c2)

c4 <- c(1.0, 1.0)
c5 <- c(0.0)
fos_forward(1,1,2,1,c1,c4,c2,c5)

# gradient(tag, n, x[n], g[n])
# int gradient(short,int,const double*,double*);
c6 <- c(1.0, 2.0)
c7 <- c(0.0, 0.0)
gradient(1,2, c6, c7);


#fos_reverse(tag, m, n, u[m], z[n])
#int fos_reverse(short,int,int,double*,double*);
c8<- c(1.0)
c9 <- c(0.0,0.0)
fos_reverse(1,1,2,c8,c9)

#jacobian(tag, m, n, x[n], J[m][n])
#jacobian(short,int,int,const double*,double**);
c10 <- c(1.0, 2.0)
c11 <- matrix(0.0, ncol = 2, nrow = 1)
jacobian(1,1,2,c10,c11);

c12 <- c(1.0, 2.0)
c13 <- matrix(0.0, ncol = 2, nrow = 2)
hessian(1,2,c12,c13);

c1;
c2;
c4;
c5;
c6;
c7;
c8
c9
c10
c11