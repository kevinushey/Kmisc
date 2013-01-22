\name{char_to_factor}
\alias{char_to_factor}
\title{Converts Characters to Factors in an Object}
\usage{
  char_to_factor(x, ...)
}
\arguments{
  \item{x}{an object.}

  \item{...}{optional arguments passed to
  \code{\link{factor}}.}
}
\description{
  Converts characters to factors in an object. Leaves
  non-character elements untouched. Objects that can be
  subscripted with \code{[[} are supported.
}

