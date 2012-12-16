\name{kReplace}
\alias{kReplace}
\title{Replace Elements in a Vector}
\usage{
  kReplace(vec, from, to = names(from))
}
\arguments{
  \item{vec}{the vector of items you will be translating}

  \item{from}{the items you will be mapping 'from'}

  \item{to}{the items you will be mapping 'to'. must be
  same length and order as \code{from}.}
}
\description{
  This function replaces elements in a vector. See examples
  for usage.
}
\examples{
x <- c(1, 2, 2, 3)
from <- c(1, 2)
to <- c(10, 20)
kReplace( x, from, to )

## alternatively, we can submit a named character vector
## we translate from value to name
names(from) <- to
kReplace( x, from )

## NAs are handled sensibly. Types are coerced as needed.
x <- c(1, NA, 2, 2, 3)
kReplace(x, c(1, 2), c("a", "b") )
}

