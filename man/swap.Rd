\name{swap}
\alias{swap}
\title{Swap Elements in a Vector}
\usage{
swap(vec, from, to = names(from))
}
\arguments{
  \item{vec}{the vector of items whose elements you will be
  replacing.}

  \item{from}{the items you will be mapping 'from'.}

  \item{to}{the items you will be mapping 'to'. must be
  same length and order as \code{from}.}
}
\description{
  This function swaps elements in a vector. See examples
  for usage.
}
\details{
  If \code{to} is of different type than \code{from}, it
  will be coerced to be of the same type.
}
\examples{
x <- c(1, 2, 2, 3)
from <- c(1, 2)
to <- c(10, 20)
swap( x, from, to )

## alternatively, we can submit a named character vector
## we translate from value to name. note that this forces
## a conversion to character
names(from) <- to
swap( x, from )

## NAs are handled sensibly. Types are coerced as needed.
x <- c(1, NA, 2, 2, 3)
swap(x, c(1, 2), c("a", "b") )
}
\seealso{
  \code{\link{swap_}}
}

