\name{str_slice2}
\alias{str_slice2}
\title{Slice a Vector at Consecutive Indices}
\usage{
str_slice2(x, n = 1, USE.NAMES = TRUE)
}
\arguments{
  \item{x}{a character vector.}

  \item{n}{integer (or numeric coercible to integer); index
  at which to slice.}

  \item{USE.NAMES}{logical. if names attribute already
  exists on \code{x}, pass this through to the result?}
}
\value{
  A list of length equal to the length of \code{x}, with
  each element made up of the substrings generated from
  \code{x[i]}.
}
\description{
  This function 'slices' the strings of a character vector
  \code{x} at consecutive indices \code{n}, thereby
  generating consecutive substrings of length \code{n} and
  returning the result as a list. This function will safely
  'slice' a UTF-8 encoded vector.
}
\note{
  Safe for use with UTF-8 characters, but slower than
  \code{str_slice}.
}
\seealso{
  \code{\link{str_slice}}, for slicing an ASCII vector.
}

