\name{is.sorted}
\alias{is.sorted}
\title{Test if an Object is Sorted}
\usage{
  is.sorted(x, na.rm = FALSE, strictly = FALSE)
}
\arguments{
  \item{x}{an \R object with a class or a numeric, complex,
  character or logical vector.}

  \item{na.rm}{logical. Should missing values be removed
  before checking?}

  \item{strictly}{logical indicating if the check should be
  for strictly increasing values.}
}
\description{
  Test if an object is sorted, without the cost of sorting
  it. Wrapper to \code{\link{is.unsorted}}.
}
\examples{
stopifnot( is.sorted(1, 2, 4) )
}
\seealso{
  \code{\link{is.unsorted}}
}

