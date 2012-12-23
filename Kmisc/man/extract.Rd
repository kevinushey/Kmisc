\name{extract}
\alias{extract}
\title{Extract Variables from a List / Data Frame}
\usage{
  extract(dat, ...)
}
\arguments{
  \item{dat}{\code{list} or \code{data.frame} object, or
  other similar object with a \code{names} attribute}

  \item{...}{an optional number of 'names' to match in
  \code{dat}}
}
\description{
  Extract variables from a \code{list} / \code{data.frame}.
  The first argument is the \code{list} /
  \code{data.frame}, while the second is passed to and
  parsed from \code{...}. \code{\link{name}}s passed to
  \code{...} are first searched for in the global
  environment; if not found, we fall back to a character
  representation of the \code{name}. We return the
  \code{list} / \code{data.frame}, including only those
  whose names were found in \code{...}.
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
dat[ names(dat) \%in\% c("x","z") ]
extract( dat, x, z)
extract( dat, dat$x, dat$z )

## we can even have a variable that includes names
a <- "z"
extract( dat, dat$x, a)
}
\seealso{
  \code{\link{without}}, \code{\link{extract.re}}
}

