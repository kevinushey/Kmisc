\name{char_to_factor}
\alias{char2factor}
\alias{char_to_factor}
\title{Converts Characters to Factors in an Object}
\usage{
char_to_factor(X, inplace = FALSE, ...)

char2factor(X, inplace = FALSE, ...)
}
\arguments{
  \item{X}{an object.}

  \item{inplace}{boolean; if \code{TRUE} the object is
  modified in place.  Useful if you're modifying a list and
  don't want to force a copy, but be wary of other symbols
  pointing as the same data.}

  \item{...}{Ignored.}
}
\description{
Converts characters to factors in an object. Leaves
non-factor elements untouched.
}

