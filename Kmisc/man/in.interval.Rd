\name{in.interval}
\alias{in.interval}
\title{Determine if Value Lies within Interval}
\usage{
  in.interval(x, lo, hi, include.lower = TRUE,
    include.upper = FALSE)
}
\arguments{
  \item{x}{numeric. vector of numbers.}

  \item{lo}{numeric, length 1. lower boundary.}

  \item{hi}{numeric, length 1. upper boundary.}

  \item{include.lower}{boolean. include the lower
  endpoint?}

  \item{include.upper}{boolean. include the upper
  endpoint?}
}
\description{
  This function determines whether elements of a numeric
  vector \code{x} lie within boundaries \code{[lo, hi)}.
  Marginally slower than the R equivalent code \code{x >=
  lo & x < hi} for small vectors; much faster for very
  large vectors.
}
\note{
  See
  \url{http://stackoverflow.com/questions/13970333/fast-vectorized-function-to-check-if-a-value-is-in-an-interval/13970618#comment19278585_13970618}
  for some discussion.
}
\examples{
x <- runif(100); lo <- 0.5; hi <- 1
f <- function(x, lo, hi) {
  return( x >= lo & x < hi )
  }
stopifnot( all( in.interval( x, lo, hi ) == f(x, lo, hi) ) )
}

