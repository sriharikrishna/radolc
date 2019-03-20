\name{badouble_declareDependent}
\alias{badouble_declareDependent}
\title{
 Identify dependent variables
}
\description{
	\code{badouble_declareDependent} identifies the dependent variables in the computation.
}
\usage{
badouble_declareDependent(arguments)
}
\details{
  \code{badouble_declareDependent} can take a single or multiple arguments. It is used to identify those output variables that are dependents in computation being differentiated. Derivatives will be computed for identified output variables with respect to the identified input variables (using \code{badouble_declareIndependent}).
}
\seealso{
  \code{\link{trace_on}}, \code{\link{trace_off}}, \code{\link{badouble_declareIndependent}}
}
\keyword{autodiffadolc}
