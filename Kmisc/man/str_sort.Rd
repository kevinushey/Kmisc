\name{str_sort}
\alias{str_sort}
\title{Sort a Vector of Strings}
\usage{
  str_sort(x, increasing = TRUE)
}
\arguments{
  \item{x}{a character vector (a vector of 'strings' to
  sort)}

  \item{increasing}{boolean. sort the string in increasing
  lexical order?}
}
\description{
  Sorts a vector of strings lexically, as based on their
  UTF-8 ordering scheme. Lower-case letters are, by
  default, 'larger' than upper-case letters.
}
\details{
  The sorting is done in C++ with \code{std::sort}.
}
\examples{
stopifnot( all( str_sort(c("cba", "fed")) == c("abc", "def") ) )
}

