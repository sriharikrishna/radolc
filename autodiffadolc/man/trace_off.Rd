\name{trace_off}
\alias{trace_off}
\title{
 Stop Tracing
}
\description{
	\code{trace_off} ends the \code{autodiffadolc} tracing process.
}
\usage{
trace_on()
}
\details{
  Using \code{autodiffadolc} consists of two phases (1) Tracing (also called taping), generally performed once and (2) Computation of derivatives, which can be performed. \code{trace_off} marks where the taping ends.  Cleans up, and turns off trace_flag.
}
\seealso{
  \code{\link{trace_on}}, \code{\link{badouble_declareIndependent}}, \code{\link{badouble_declareDependent}},
}
\keyword{autodiffadolc}
