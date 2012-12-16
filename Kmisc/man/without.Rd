\name{without}
\alias{without}
\title{Remove Variables from a List / Data Frame}
\usage{
  without(dat, ...)
}
\arguments{
  \item{dat}{\code{list} or \code{data.frame} object, or
  other similar object with a \code{names} attribute}

  \item{...}{an optional number of 'names' to match in
  \code{dat}}
}
\description{
  Removes variables from a \code{list} / \code{data.frame}
  in a 'lazy' way. The first argument is the data frame,
  while the second is passed and parsed from \code{...}. We
  return the \code{list} / \code{data.frame}, sans the
  names matched from \code{...}.
}
\details{
  We can be 'lazy' with how we name the variables. The
  \code{\link{name}}s passed to \code{...} are not
  evaluated directly; rather, their character
  representation is taken and used for extraction.
  Furthermore, for a given item submitted, all text before
  a \code{$} is removed.

  First, symbols are parsed as characters, and named of
  \code{dat} are checked to see if they match any of
  \code{names(dat)}. If not, we try to find the variable in
  the local search path, and match that against the names.
  If none of these are successful, we throw an error.
}
\examples{
dat <- data.frame( x = c(1, 2, 3), y = c("a", "b", "c"), z=c(4, 5, 6) )
## all of these return identical output
dat[ !( names(dat) \%in\% c("x","z") ) ]
without( dat, x, z)
without( dat, dat$x, dat$z )
}
\seealso{
  \code{\link{extract}}
}

