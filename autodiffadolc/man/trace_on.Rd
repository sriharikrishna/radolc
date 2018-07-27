\name{trace \textunderscore on}
\alias{trace_on}
\title{
	Reports which variables are active in the differentiation routine
}
\description{
	\code{trace_on} returns a numeric that provides the number 
	of active variables
}
\usage{
trace_on()
}
\details{
	The \code{trace_on} inside the library \code{autodiffadolc} returns
	a numeric value of the number of active variables in an optimization 
	function that is provided by user.
}
\seealso{
  \code{\link{autodiffadolc}}
}
\keyword{ADOL-C}