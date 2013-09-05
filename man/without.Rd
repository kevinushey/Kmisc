\name{without}
\alias{without}
\title{Remove Elements from a Named Object}
\usage{
  without(x, ...)
}
\arguments{
  \item{x}{An \R object with a \code{names} attribute.}

  \item{...}{an optional number of 'names' to match in
  \code{x}.}
}
\description{
  Removes elements from an \R object with the names
  attribute set in a 'lazy' way. The first argument is the
  object, while the second is a set of names parsed from
  \code{...}. We return the object, including only the
  elements with names matched from \code{...}.
}
\details{
  We can be 'lazy' with how we pass names. The
  \code{\link{name}}s passed to \code{...} are not
  evaluated directly; rather, their character
  representation is taken and used for extraction.
  Furthermore, for a given item submitted, all text before
  a \code{$} is removed.

  First, symbols are parsed as characters, and entries in
  \code{...} are checked to see if they match any of
  \code{names(dat)}. If not, we try to find the variable in
  the local search path, and match that against the names.
  If none of these are successful, we display a warning.
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

