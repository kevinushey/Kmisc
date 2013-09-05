\name{extract.re}
\alias{extract.re}
\alias{re_extract}
\title{Extract Elements from a Named Object with Regular Expressions}
\usage{
  extract.re(x, pattern, perl = TRUE, ...)

  re_extract(x, pattern, perl = TRUE, ...)
}
\arguments{
  \item{x}{An \R object with a \code{names} attribute.}

  \item{pattern}{a regular expression pattern to match
  against \code{names(x)}.}

  \item{perl}{boolean. use perl-compatible regular
  expressions?}

  \item{...}{optional arguments passed to \code{grep}.}
}
\description{
  Extract elements from an \R object, whereby elements in
  the object whose names attribute matches the regular
  expression supplied, are returned.
}
\seealso{
  \code{\link{grep}}, \code{\link{regex}}
}

