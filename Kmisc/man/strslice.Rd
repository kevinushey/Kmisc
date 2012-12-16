\name{strslice}
\alias{strslice}
\title{Slice a String at Consecutive Indices}
\usage{
  strslice(x, n)
}
\arguments{
  \item{x}{a character vector of length one; ie, a string.}

  \item{n}{the index at which to cut}
}
\value{
  A character vector, 'sliced' at consecutive indices
  \code{n}.
}
\description{
  Slice a string at consecutive indices, such that
  substrings of equal length are generated from a string.
}
\examples{
x <- "ABCDEF"
stopifnot( strslice( x, 2 ) == c("AB", "CD", "EF") )

## applying strslice to a vector of 'strings'
y <- c("ABCD", "EFGH", "IJK") ## note unequal length
sapply( y, function(x) { strslice(x, 2) } ) ## each element sliced; warning generated for "IJK"
}

