\name{extract.re}
\alias{extract.re}
\title{Extract Variable from a Data Frame / List, with Regular Expressions}
\usage{
  extract.re(dat, pattern, value = TRUE, perl = TRUE, ...)
}
\arguments{
  \item{dat}{\code{list} or \code{data.frame} object, or
  other similar object with a \code{names} attribute}

  \item{pattern}{a regular expression pattern to match
  against \code{names(dat)}}

  \item{value}{boolean. passed to \code{grep}}

  \item{perl}{boolean. use perl-compatible regular
  expressions?}

  \item{...}{optional arguments passed to grep}
}
\description{
  Extract variables from a \code{list} / \code{data.frame},
  whereby the variables matching \code{pattern} are
  returned.
}
\seealso{
  \code{\link{grep}}, \code{\link{regex}}
}

