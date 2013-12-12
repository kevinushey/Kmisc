\name{lu}
\alias{lu}
\title{Number of non-NA unique elements in a vector}
\usage{
lu(x, ...)
}
\arguments{
  \item{x}{a vector}

  \item{...}{passed to \code{\link{unique}}}
}
\description{
Returns the number of non-NA unique elements in a vector. A
wrapper to \code{length( unique( x[!is.na(x)], ... ) )}.
Primarily intended for interactive, not programmatic, use.
}

