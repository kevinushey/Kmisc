\name{transpose_list}
\alias{transpose_list}
\title{Transpose a List}
\usage{
  transpose_list(x)
}
\arguments{
  \item{x}{A matrix-like list; ie, a list with each element
  of the same length. Types are coerced as necessary.}
}
\description{
  Transpose a List
}
\examples{
l <- replicate(100, rnorm(100), simplify=FALSE)
stopifnot( identical(
  transpose_list( transpose_list(l) ), l
) )
}

