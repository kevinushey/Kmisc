\name{in_interval}
\alias{in_interval}
\title{Determine if Value Lies within Interval}
\usage{
  in_interval(x, lo, hi, include.lower = TRUE,
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
  Marginally slower than the \R equivalent code \code{x >=
  lo & x < hi} for small vectors; much faster for very
  large vectors.
}
\examples{
x <- runif(100); lo <- 0.5; hi <- 1
f <- function(x, lo, hi) {
  return( x >= lo & x < hi )
  }
stopifnot( all( in_interval( x, lo, hi ) == f(x, lo, hi) ) )
}

