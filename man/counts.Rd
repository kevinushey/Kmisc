\name{counts}
\alias{counts}
\title{Generate Counts of Values in a Vector}
\usage{
counts(x)
}
\arguments{
  \item{x}{A numeric, integer, character or logical vector,
  or a (potentially nested) list of such vectors. If
  \code{x} is a list, we recursively apply \code{counts}
  throughout elements in the list.}
}
\description{
This function uses Rcpp sugar to implement a fast
\code{table}, for unique counts of a single vector. This
implementation seeks to produce identical output to
\code{table(x, useNA="ifany")}.
}
\details{
For numeric vectors, we do not distinguish between `NA` and
`NaN`. The order is not guaranteed to be identical with
that of \code{table}, when any \code{NA}s or \code{NaN}s
are present.
}
\examples{
x <- round( rnorm(1E2), 1 )
x_int <- as.integer(x)
x_char <- as.character(x)
stopifnot( identical( counts(x), c(table(x)) ) )
stopifnot( identical( counts(x_int), c(table(x_int)) ) )
stopifnot( identical( counts(x_char), c(table(x_char)) ) )
}

