\name{counts}
\alias{counts}
\title{Generate Counts of Values in a Vector}
\usage{
  counts(x)
}
\arguments{
  \item{x}{A numeric, integer, character or logical vector,
  or a (potentially nested) list of such vectors. If
  \code{x} is a list, we recursively apply counts
  throughout elements in the list.}
}
\description{
  This function uses Rcpp sugar to implement a fast
  \code{table}, for unique counts of a single vector.
}

