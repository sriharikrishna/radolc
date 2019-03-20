\name{badouble_declareIndependent}
\alias{badouble_declareIndependent}
\title{
 Identify independent variables
}
\description{
	\code{badouble_declareIndependent} identifies the independent variables in the computation.
}
\usage{
badouble_declareIndependent(arguments)
}
\details{
  \code{badouble_declareIndependent} can take a single or multiple arguments. It is used to identify those input variables that are independents in computation being differentiated. Derivatives will be computed for identified output variables (using \code{badouble_declareDependent}) with respect to the identified input variables.
}
\seealso{
  \code{\link{trace_on}}, \code{\link{trace_off}}, \code{\link{badouble_declareDependent}}
}
\keyword{autodiffadolc}
