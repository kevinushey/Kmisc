\name{str_slice}
\alias{str_slice}
\title{Slice a Vector at Consecutive Indices}
\usage{
  str_slice(x, n = 1)
}
\arguments{
  \item{x}{a character vector.}

  \item{n}{integer (or numeric coercible to integer); index
  at which to slice.}
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
  returning the result as a list.
}
\note{
  Underlying code is written in C for fast execution.
}

