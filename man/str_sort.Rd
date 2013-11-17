\name{str_sort}
\alias{str_sort}
\title{Sort a Vector of Strings}
\usage{
str_sort(x, increasing = TRUE, ignore.case = FALSE, USE.NAMES = FALSE)
}
\arguments{
  \item{x}{a character vector (a vector of 'strings' to
  sort)}

  \item{increasing}{boolean. sort the string in increasing
  lexical order?}

  \item{ignore.case}{boolean. ignore case (so that, eg,
  \code{a < A < b})}

  \item{USE.NAMES}{logical. if names attribute already
  exists on \code{x}, pass this through to the result?}
}
\description{
  Sorts a vector of strings lexically, as based on their
  UTF-8 ordering scheme. Lower-case letters are, by
  default, 'larger' than upper-case letters. This function
  will safely sort a UTF-8 vector.
}
\examples{
stopifnot( all( str_sort(c("cba", "fed")) == c("abc", "def") ) )
}

