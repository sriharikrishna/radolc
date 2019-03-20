\name{trace_on}
\alias{trace_on}
\title{
 Initialization for the taping process
}
\description{
	\code{trace_on} starts the \code{autodiffadolc} tracing process.
}
\usage{
trace_on()
}
\details{
  Using \code{autodiffadolc} consists of two phases (1) Tracing (also called taping), generally performed once and (2) Computation of derivatives, which can be performed. \code{trace_on} is the initialization for the taping process. Creates buffers for this tape, sets files names, and calls appropriate setup routines.
}
\seealso{
  \code{\link{trace_off}}, \code{\link{badouble_declareIndependent}}, \code{\link{badouble_declareDependent}}
}
\keyword{autodiffadolc}
