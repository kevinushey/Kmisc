\name{fgrep}
\alias{fgrep}
\title{Fixed/Fast Grep}
\usage{
fgrep(pattern, x, ignore.case = FALSE, value = FALSE, useBytes = FALSE,
  invert = FALSE)
}
\arguments{
  \item{pattern}{character string containing a character
  string to be matched in the given character vector;
  coerced by \code{\link{as.character}} if possible.}

  \item{x}{a character vector where matches are sought, or
  an object coercable by \code{as.character}.}

  \item{ignore.case}{boolean; if \code{TRUE} we perform
  case-insensitive matching.}

  \item{value}{boolean; if \code{TRUE} we return the actual
  matches; if \code{FALSE} we return the indices
  corresponding to the matches.}

  \item{useBytes}{boolean; if \code{TRUE} we perform
  matching byte-by-byte rather than character by
  character.}

  \item{invert}{boolean; if \code{TRUE} we return indices
  or values for elements that do not match.}
}
\description{
  This is equivalent to calling \code{grep} with
  \code{fixed=TRUE}.
}
\examples{
fgrep( "abc", c("abc", "babcd", "abcdef", "apple"), value=TRUE )
}
\seealso{
  \code{\link{grep}}
}

