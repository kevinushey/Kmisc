\name{char_to_factor}
\alias{char_to_factor}
\title{Converts Factors to Characters in an Object}
\usage{
  char_to_factor(X, ...)
}
\arguments{
  \item{X}{an object.}

  \item{...}{optional arguments passed to \code{factor}.}
}
\description{
  Converts factors to characters in an object. Leaves
  non-factor elements untouched. Objects that can be
  subscripted with \code{[[} are supported.
}

