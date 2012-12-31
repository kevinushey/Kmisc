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
  Extracts variables from a \code{list} / \code{data.frame}
  / other R object with the names attribute set in a 'lazy'
  way. The first argument is the R object, while the second
  is passed and parsed from \code{...}. We return the R
  object, sans the elements with names matched from
  \code{...}.
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
  If none of these are successful, we display a warning.
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

