\name{without.re}
\alias{without.re}
\title{Remove Variables from a List / Data Frame, with Regular Expressions}
\usage{
  without.re(dat, pattern, value = TRUE, perl = TRUE, ...)
}
\arguments{
  \item{dat}{\code{list} or \code{data.frame} object, or
  other similar object with a \code{names} attribute}

  \item{pattern}{a regular expression pattern to match
  against \code{names(dat)}}

  \item{perl}{boolean. use perl-compatible regular
  expressions?}

  \item{...}{optional arguments passed to \code{grep}.}
}
\description{
  Remove variables from a \code{list} / \code{data.frame},
  whereby the variables not matching \code{pattern} are
  returned.
}
\seealso{
  \code{\link{grep}} \code{\link{regex}}
}

